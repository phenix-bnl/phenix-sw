#include "Event.h"
#include "PHIODataNode.h"
#include "RunHeader.h"
#include "TofwRaw.h"
#include "TofwHit.h"
#include "TofwGeometry.h"
#include "TofwCalib.h"

#include "TofwEvent.h"
#include <iostream>


using namespace std;

typedef PHDataNode<Event>       EventNode_t;
typedef PHIODataNode<TofwRaw>   TofwRawNode_t;
typedef PHIODataNode<TofwHit>   TofwHitNode_t;
typedef PHIODataNode<RunHeader> RunHeaderNode_t;

//________________________________________________________________
TofwEvent::TofwEvent() : debug(0)
{
  d_raw=0;
  d_hit=0;
}

//________________________________________________________________
PHBoolean TofwEvent::findNodes(PHCompositeNode* top)
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
int TofwEvent::Reset(PHCompositeNode *top)
{
  //  This routine should perform resets.
  
  if (debug) std::cout << "TofwEvent::Reset() : Resetting the MRPC " <<std::endl;
  if (d_raw) d_raw->Reset();
  if (d_hit) d_hit->Reset();
  
  return 0;
}

//________________________________________________________________
int TofwEvent::process_event(PHCompositeNode *top, 
			     TofwGeometry *geom,
			     TofwCalib   *calib) 
{
  // This routine should perform analysis of the event.

  // Find Nodes
  TofwEvent::findNodes(top);

  // Fills the Raw objects
  if (debug) std::cout << "TofwEvent::process_event(), fill Raw data... " << std::endl;
  bool dcmtoraw = TofwEvent::DcmToRaw(top, calib);
  if (debug) std::cout << "TofwEvent::process_event(), fill Hit data... " << std::endl;
  if(dcmtoraw) TofwEvent::RawToHit(top, geom, calib);

  return 0;
}

//________________________________________________________________
PHBoolean TofwEvent::DcmToRaw(PHCompositeNode* top,
			      TofwCalib* calib)
{
  PHNodeIterator iter(top);
  EventNode_t*   evtNode;
  
  // Find the node with the Event object.
  evtNode = dynamic_cast<EventNode_t*>(iter.findFirst("PHDataNode","PRDF"));
  
  if(!evtNode) return False;

  // Extract the Event object from the node.
  Event* event = evtNode->getData();
  
  if(!event) return False;
  
  int t3[512][2];
  int t4[512][2];
  float q[512][2];
  float t[512][2];

  for(int icrate=0; icrate<TOFW_NCRATE; icrate++)
    {
      Packet* pTOFW = event->getPacket(TOFW_PACKET_ID[icrate]);
      
      if(pTOFW)
	{
	  int HitFormat = pTOFW->getHitFormat();
	  
	  if(HitFormat == TOFW_HITF_NON_ZEROSUP || 
	     HitFormat == TOFW_HITF_ZEROSUP)
	    {
	      for(int iboard=0;iboard<16;iboard++)
		{ 
		  
		  for(int ich=0;ich<16;ich++) 
		    {
		      int ichannel = iboard*16+ich;
		      int t_3 = pTOFW->iValue(ichannel, "TC3");
		      int t_4 = pTOFW->iValue(ichannel, "TC4");
		      //cout<<pTOFW->iValue(ichannel, "QC1")<<" "<<pTOFW->iValue(ichannel, "QC3")<<endl;
		      int qvc = pTOFW->iValue(ichannel, "QC1") - pTOFW->iValue(ichannel, "QC3");
		      
		      int istrip  =  calib->Channel2Strip(ichannel+256*icrate);
		      int iend    =  calib->Channel2End(ichannel+256*icrate);
		      float Tconv = calib->getTvcConv(icrate,ichannel);
		      t3[istrip][iend] = t_3;
		      t4[istrip][iend] = t_4;
		      q[istrip][iend] = qvc*1.0;
		      t[istrip][iend] = t_3*Tconv;
		    }//end of chn loop
		}// end of iboard loop		
	    } // end of Hit Format if
	  delete pTOFW;
	}// end of packet if
      else
	{
	  
	  for(int jstrip=0; jstrip<128; jstrip++)
	    {
	      int istrip = jstrip + 128* icrate;
	      for(int iend=0; iend<2; iend++){
		t3[istrip][iend] = -9999;//protect events without Tofw packet
		t4[istrip][iend] = -9999;
		q[istrip][iend]  = -9999;
		t[istrip][iend]  = -9999;
	      }
	    }
	}
      
    }//end of packets loop

  for(int istrip=0; istrip<512; istrip++){
    // Add a raw data and set raw count
    d_raw->AddRaw(istrip); 
    
    int ibox = istrip/128;
    d_raw->set_boxid(istrip,ibox);
    
    int ichamber = istrip%128/8;
    d_raw->set_chamberid(istrip,ichamber);
    
    d_raw->set_stripid(istrip,istrip);
    
    d_raw->set_t3 (istrip, 0, t3[istrip][0]);
    d_raw->set_t4 (istrip, 0, t4[istrip][0]);
    d_raw->set_qvc(istrip, 0, q[istrip][0]);
    d_raw->set_tvc(istrip, 0, t[istrip][0]);

    d_raw->set_t3 (istrip, 1, t3[istrip][1]);
    d_raw->set_t4 (istrip, 1, t4[istrip][1]);
    d_raw->set_qvc(istrip, 1, q[istrip][1]);
    d_raw->set_tvc(istrip, 1, t[istrip][1]);
  }
  return True;
}

//________________________________________________________________
PHBoolean TofwEvent::RawToHit(PHCompositeNode* top,
			      TofwGeometry* geom,
			      TofwCalib* calib)
{
  int ihit = 0;
  int iraw = 0;

  float ftvc[2];
  float fqvc[2];
  float ft3[2];

  //cout<<"check event ******************************"<<endl;
  for(int ibox=0; ibox<TOFW_NBOX; ibox++)
    {
      for(int ichamber=0; ichamber<TOFW_NCHAMBER_BOX; ichamber++)
	{ 
	  d_hit->AddHit(ihit); 
	  
	  int nstripgood = 0;
	  int max_strip = -9999;
	  float max_adc   = -9999;
	  for(int istrip=0;istrip < TOFW_NSTRIP_CHAMBER; istrip++)
	    {
	      iraw = ibox*32*4+ichamber*4+istrip;//the sequence of iraw is followed by the strip 

	      // ==============
	      // Get raw info
	      // ==============
	      
	      if (calib->getGoodStrip(iraw))
		{
		  for (int iend=0; iend < 2; iend++)
		    { 
		      if(d_raw)
			{
			  // readout type (0: bottom, 1: top)
			  ft3[iend] =  d_raw->get_t3(iraw,iend);
			  ftvc[iend] = d_raw->get_tvc(iraw,iend);    // TVC
			  fqvc[iend] = d_raw->get_qvc(iraw,iend);    // QVC
			}
		      else{
			ft3[iend] =  -9999.0;
			ftvc[iend] = -9999.0;
			fqvc[iend] = -9999.0;
		      }
		    }
		  
		  //cout<<fqvc[0]<<" "<< ft3[0]<<" "<< ft3[0] <<endl;
		  if(fqvc[0]>10.0 && ft3[0]>15.0 && ft3[0]<3800.0 && 
		     fqvc[1]>10.0 && ft3[1]>15.0 && ft3[1]<3800.0)
		    {		  
		      float T0 = ftvc[0];
		      float T1 = ftvc[1] - calib->get_toffset(iraw);
		      
		      float A0 = fqvc[0];
		      float A1 = fqvc[1];
		      
		      if((A0+A1)>max_adc) {
			max_strip = nstripgood;
			max_adc = A0+A1;		    
		      }
		      
		      float Traw  = (T0 + T1)/2.0;
		      float Tdiff = (T0 - T1)/2.0;
		      
		      float charge =sqrt(A0*A1);
		      
		      // Hit Position ____________________________________________
		      float pos[3]   = {0,0,0};
		      float ypos = Tdiff * Tofw_Velocity; 
		      
		      PHPoint hitpoint(0,0,0);    // TOFW hit position by PHPoint
		      
		      hitpoint = geom->getStripCenter(ibox,ichamber,istrip);
		      
		      if(ibox==0||ibox==1) {
			pos[0] = (float)hitpoint.getX();
			pos[1] = (float)hitpoint.getY() + ypos;
		      }else {
			pos[0] = (float)hitpoint.getX() - ypos*sin(box_angle);
			pos[1] = (float)hitpoint.getY() + ypos*cos(box_angle);
		      }
		      pos[2] = (float)hitpoint.getZ(); 
		      
		      // Fill hit info... 
		      d_hit->set_stripid(ihit, nstripgood, istrip);
		      d_hit->set_time(ihit, nstripgood, Traw);
		      d_hit->set_charge(ihit, nstripgood, charge);
		      
		      d_hit->set_xyz(ihit, nstripgood, 0, pos[0]);
		      d_hit->set_xyz(ihit, nstripgood, 1, pos[1]);
		      d_hit->set_xyz(ihit, nstripgood, 2, pos[2]);
		      d_hit->set_rawadc(ihit, nstripgood, 0, A0);
		      d_hit->set_rawadc(ihit, nstripgood, 1, A1);
		      d_hit->set_rawtdc(ihit, nstripgood, 0, T0);
		      d_hit->set_rawtdc(ihit, nstripgood, 1, T1);
		      
		      nstripgood ++;
		    }//end of selection
		}//end of goodstrip
	    }//end of istrip
	  if(nstripgood==0) d_hit->RemoveHit(ihit);
	  else {
	    d_hit->set_boxid(ihit, ibox);
	    d_hit->set_chamberid(ihit, ichamber);	
	    d_hit->set_nstrip(ihit,nstripgood);
	    d_hit->set_max(ihit, max_strip);
	    d_hit->set_nhit(ihit+1);
	    ihit++;
	  }
	  // Calculate Hit info and fill in DST
	}//end of ichamber
    }//end of ibox
  return True;
}
