#include "Event.h"
#include "getClass.h"
#include "PHIODataNode.h"
#include "RunHeader.h"

#include "TofwRaw.h"
#include "TofwHit.h"
#include "TofwGeometry.h"
#include "TfwPISAHit.h"     // pisa2000/src/root

#include "TofwSimEvent.h"
#include "tfwghitWrapper.h"

#include <iostream>
#include <gsl/gsl_randist.h>

using namespace std;

typedef PHDataNode<Event>       EventNode_t;
typedef PHIODataNode<TofwRaw>   TofwRawNode_t;
typedef PHIODataNode<TofwHit>   TofwHitNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

//________________________________________________________________
TofwSimEvent::TofwSimEvent() : debug(0)
{
  d_raw=0;
  d_hit=0;

  z_strip_center[0] = -4.665;
  z_strip_center[1] = -1.555;
  z_strip_center[2] =  1.555;
  z_strip_center[3] =  4.665;
  
  //0.1ns
  sigmaGauss = 0.1;
  
  // initialize gsl random generator
  gsl_rng_env_setup();
  const gsl_rng_type *T = gsl_rng_default;
  _rng = gsl_rng_alloc(T);

}

//________________________________________________________________
TofwSimEvent::~TofwSimEvent()
{
  // free gsl random generator
  gsl_rng_free( _rng ); 
}

//________________________________________________________________
PHBoolean TofwSimEvent::findNodes(PHCompositeNode* top)
{
  d_raw = 0;
  d_hit = 0;
  // Search out the nodes from the node tree
  // RAW
  PHTypedNodeIterator<TofwRaw> iRAW(top);
  TofwRawNode_t* TofwRawNode = iRAW.find("TofwRaw");
  if(TofwRawNode)
    {
      d_raw = TofwRawNode->getData();
    }
  else
    {
      std::cout << PHWHERE << " raw data not in Node Tree" << std::endl;
    }

  // HIT
  PHTypedNodeIterator<TofwHit> iHIT(top);
  TofwHitNode_t* TofwHitNode = iHIT.find("TofwHit");
  if(TofwHitNode) 
    {
      d_hit = TofwHitNode->getData();
    }
  else 
    {
      std::cout << PHWHERE << " hit data not in Node Tree" << std::endl;
    }

  return True;
}

//________________________________________________________________
int TofwSimEvent::Reset(PHCompositeNode *top)
{
  //  This routine should perform resets.
  
  if (debug) std::cout << "TofwSimEvent::Reset() : Resetting the MRPC " <<std::endl;
  if (d_raw) d_raw->Reset();
  if (d_hit) d_hit->Reset();
  
  return 0;
}

//________________________________________________________________
int TofwSimEvent::process_event(PHCompositeNode *top,TofwGeometry *geom) 
{

  static bool iFirst = true;
  if(iFirst && debug) {
    iFirst = false;
    std::cout << "\n\n  TofwSimEvent::process_event(), version December 8, 2007 with gsl random number generator\n" << std::endl;
  }

  // Find Nodes
  if(findNodes(top)){
    // Fills the Raw objects
    if (debug) std::cout << "TofwSimEvent::process_event(), fill Sim Raw data... " << std::endl;
    bool pisatoraw = TofwSimEvent::PisaToRaw(top);
    if (debug) std::cout << "TofwSimEvent::process_event(), fill Sim Hit data... " << std::endl;
    if(pisatoraw) TofwSimEvent::RawToHit(top,geom);
  }
  return 0;
}

//________________________________________________________________
PHBoolean TofwSimEvent::PisaToRaw(PHCompositeNode* top)
{
  tfwghitWrapper *tfwghitWrap = findNode::getClass<tfwghitWrapper>(top,"tfwghit");
  if (!tfwghitWrap)
    {
      cout << PHWHERE << "tfwghit Node missing, exiting" << endl;
      exit(1);
    }

  TFWGHIT_ST *tfwghit = tfwghitWrap->TableData();

  short nghit_total = tfwghitWrap->RowCount();
  if(debug>0)
    {
      cout<<"Number of rows in TFWGHIT = "<<nghit_total<<endl;
    }

  //Initialize the hits count to zero in all channels before the event
  int nhits[128];
  float tof[128];
  float elos[128];
  float local_y[128];
  float local_z[128];
  float global_z[128];
  for(int ichamber=0; ichamber<128; ichamber++) 
    {
      nhits[ichamber]=0;
      tof[ichamber]  = 0;
      elos[ichamber] = 0;
      local_y[ichamber] = 0; 
      local_z[ichamber] = 0;
      global_z[ichamber] = 0;
    }

  // --- Start PISA Hit Loop ---
  for(short ighit=0; ighit<nghit_total; ighit++ )
    {
      float gy    = tfwghit[ighit].xyzinloc[1];
      float gz    = tfwghit[ighit].xyzinloc[2];

      float glx   = tfwghit[ighit].xyzinglo[0];
      float gly   = tfwghit[ighit].xyzinglo[1];
      float glz   = tfwghit[ighit].xyzinglo[2];

      float g_tof = tfwghit[ighit].tof;
      float g_dele= tfwghit[ighit].dele;

      //calculate the chamber from the global y z
      int ichamber = CalChamber(glx,gly, glz);
      //if(ighit==0) cout<<ichamber<<" "<<glx<<" "<<gly<<" "<<glz<<endl;
      nhits[ichamber]++;
      tof[ichamber]  += g_tof;
      elos[ichamber] += g_dele;
      local_y[ichamber] += gy;
      local_z[ichamber] += gz;
      global_z[ichamber] += glz;
    }

  int nraw=0;
  for(int ichamber=0; ichamber<128; ichamber++) 
    {
      if(nhits[ichamber]>0)
	{
	  d_raw->AddRaw(nraw);
      
	  int ibox = ichamber/32;
	  d_raw->set_boxid(nraw,ibox);
	  d_raw->set_chamberid(nraw,ichamber);
      
	  local_z[ichamber] = local_z[ichamber]/nhits[ichamber];
	  global_z[ichamber] = global_z[ichamber]/nhits[ichamber];

	  int istrip = CalStrip(local_z[ichamber],global_z[ichamber]);
	  d_raw->set_stripid(nraw,istrip);
      
	  //distribute the energy loss
	  float e_strip = CalElossDis(elos[ichamber],local_z[ichamber]);
	  d_raw->set_qvc(nraw, 0, e_strip);
	  d_raw->set_qvc(nraw, 1, e_strip);
      
	  //Smear the timing by propogation and gaussian pcb length is 37.00cm
	  tof[ichamber]  = tof[ichamber]/nhits[ichamber];
	  local_y[ichamber] = local_y[ichamber]/nhits[ichamber];
      
	  float t_up= tof[ichamber]+(37.0/2.0- local_y[ichamber])/Tofw_Velocity;
	  float t_dw= tof[ichamber]+(37.0/2.0+ local_y[ichamber])/Tofw_Velocity;
      
	  d_raw->set_tvc(nraw, 1, t_up);  	  
	  d_raw->set_tvc(nraw, 0, t_dw);
      
	  nraw++;
	}      
    }
  //end loop of chambers
  d_raw->set_nraw(nraw);
  return True;
}

//________________________________________________________________
PHBoolean TofwSimEvent::RawToHit(PHCompositeNode* top, TofwGeometry *geom)
{
  if(!d_raw) 
    {
      cout<<"No raw hit"<<endl;
      return false;
    }
  
  for(int iraw=0; iraw<d_raw->get_nraw(); iraw++)
    {
      d_hit->AddHit(iraw); 
    
      int ibox     = d_raw->get_boxid(iraw);
      int ichamber = d_raw->get_chamberid(iraw);
      ichamber = ichamber%32;
      int istrip   = d_raw->get_stripid(iraw);
    
      float T0 = d_raw->get_tvc(iraw,0);
      float T1 = d_raw->get_tvc(iraw,1);
    
      float Tdiff = 0.5*(T0-T1) + gsl_ran_gaussian(_rng,0.06);
    
      //add electronic smear 60ps
    
      T0 = T0 +  gsl_ran_gaussian(_rng, sigmaGauss );
      T1 = T1 +  gsl_ran_gaussian(_rng, sigmaGauss );
    
      float A0 = d_raw->get_qvc(iraw,0);
      float A1 = d_raw->get_qvc(iraw,1);
    
      float Traw  = (T0 + T1)/2.0;
    
      float charge =sqrt(A0*A1);
    
      // Hit Position ____________________________________________
      float pos[3]   = {0,0,0};
      float ypos = Tdiff * Tofw_Velocity; 
    
      PHPoint hitpoint(0,0,0);    // TOFW hit position by PHPoint
      
      hitpoint = geom->getSimStripCenter(ibox,ichamber,istrip);
      
      if(ibox==0||ibox==1) {
        pos[0] = (float)hitpoint.getX();
        pos[1] = (float)hitpoint.getY() + ypos;
      }else {
        pos[0] = (float)hitpoint.getX() - ypos*sin(box_angle);
        pos[1] = (float)hitpoint.getY() + ypos*cos(box_angle);
      }
      pos[2] = (float)hitpoint.getZ(); 
      
      // Fill hit info... 
      d_hit->set_stripid(iraw, 0, istrip);
      d_hit->set_time(iraw, 0, Traw);
      d_hit->set_charge(iraw, 0, charge);
      
      d_hit->set_xyz(iraw, 0, 0, pos[0]);
      d_hit->set_xyz(iraw, 0, 1, pos[1]);
      d_hit->set_xyz(iraw, 0, 2, pos[2]);
      d_hit->set_rawadc(iraw, 0, 0, A0);
      d_hit->set_rawadc(iraw, 0, 1, A1);
      d_hit->set_rawtdc(iraw, 0, 0, T0);
      d_hit->set_rawtdc(iraw, 0, 1, T1);
      
      d_hit->set_boxid(iraw, ibox);
      d_hit->set_chamberid(iraw, ichamber);	
      d_hit->set_nstrip(iraw,1);
      d_hit->set_max(iraw,0);
    }//end of iraw loop
    
  d_hit->set_nhit(d_raw->get_nraw());
    
  return True;
}
  
//________________________________________________________________
int TofwSimEvent::CalChamber(float glx, float gly, float glz)
{
  float par0=-2.41421;
  float par1= 1248.04;
    
  float gap = 22.4638;
  float edge0 = -179.413;
  float edge1 = -190.660;
    
  int ichamber = -9999;
    
  if(gly<100)//bottom box
    {
      if(gly<-12)//bottom
	{
	  if(glx<477.5&&glz<0)//east face
	    {
	      ichamber = 2*int((glz-edge1)/gap);	      
	    }
	  else if(glx>477.5&&glz<0)//west face
	    {
	      ichamber = 2*int((glz-edge0)/gap)+1;
	    }
	  else if(glx>477.5&&glz>0)//west
	    {
	      ichamber = 47 - 2*int((-1.0*glz-edge1)/gap); //convert to west face
	    } 
	  else{//east
	    ichamber = 46 -2*int((-1.0*glz-edge0)/gap);
	  }
	}
      else
	{
	  if(glx<477.5&&glz<0)//east face
	    {
	      ichamber = 16+2*int((glz-edge0)/gap)+1;
	    }
	  else if(glx>477.5&&glz<0)//west face
	    {
	      ichamber = 16+2*int((glz-edge1)/gap);
	    }
	  else if(glx>477.5&&glz>0)//west face
	    {
	      ichamber = 62-2*int((-1.0*glz-edge0)/gap);
	    }
	  else //east face
	    {
	      ichamber = 63-2*int((-1.0*glz-edge1)/gap);
	    }
	}
    }
  else if(gly>100)//top box
    {
      if(gly<228)//bottom
	{
	  if(par0*glx+par1<gly&&glz<0)//east face
	    {
	      ichamber = 64 + 2*int((glz-edge0)/gap)+1;
	    }
	  else if(par0*glx+par1>gly&&glz<0)//west face
	    {
	      ichamber = 64 + 2*int((glz-edge1)/gap);
	    }
	  else if(par0*glx+par1>gly&&glz>0)//west face
	    {
	      ichamber = 110 - 2*int((-1.0*glz-edge0)/gap);
	    }
	  else
	    {
	      ichamber = 111 - 2*int((-1.0*glz-edge1)/gap);
	    }
	}
      else
	{
	  if(par0*glx+par1<gly&&glz<0)//east face
	    {
	      ichamber = 80 + 2*int((glz-edge1)/gap);
	    }
	  else if(par0*glx+par1>gly&&glz<0)//west face
	    {
	      ichamber = 80 + 2*int((glz-edge0)/gap)+1;
	    }
	  else if(par0*glx+par1>gly&&glz>0)//west face
	    {
	      ichamber = 127 - 2*int((-1.0*glz-edge1)/gap);
	    }
	  else
	    {
	      ichamber = 126 - 2*int((-1.0*glz-edge0)/gap);
	    }
	}
    }
    
  return ichamber;
}

//________________________________________________________________
int TofwSimEvent::CalStrip(float localz, float globalz)
{
  float min_distance = 9999;
  int strip_id(-1);
  
  for(int istrip=0; istrip<4; istrip++)
    {
      if(fabs(localz-z_strip_center[istrip])<min_distance)
	{ 
	  min_distance=fabs(localz-z_strip_center[istrip]);
	  strip_id = istrip;
	}
    }
  
  if(globalz>0) strip_id = 3 - strip_id;
  
  return strip_id;
}

//________________________________________________________________  
float TofwSimEvent::CalElossDis(float eloss, float localz)
{
  float gap = fabs(localz-z_strip_center[1]);
  
  float min_distance = 9999;
  for(int istrip=0;istrip<4;istrip++)
    {
      if(fabs(localz-z_strip_center[istrip])<min_distance)
	{ 
	  min_distance=fabs(localz-z_strip_center[istrip]);
	}
    }
  
  float e_weight=0;
  float total_square = 4.0*gap/3.0;
  
  if(min_distance<=gap){
    float wgap = gap - min_distance;
    if(wgap<=gap/3.0) e_weight = 0.5+wgap/total_square; 
    if(wgap>gap/3.0) e_weight = 0.75+(wgap-gap/3.0)/2.0/total_square;
  }
  
  return eloss*e_weight;
}
