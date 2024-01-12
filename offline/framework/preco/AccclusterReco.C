#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "phool.h"

#include "AccclusterReco.h"
#include "AccRaw.h"
#include "AccHit.h"
#include "AccCluster.h"
#include "AccSnglRaw.h"
#include "AccSnglHit.h"
#include "AccSnglCluster.h"
#include "AccGeometry.h"
#include "AccClusterv1.h"
#include "AccProj.h"

#include "Fun4AllServer.h"
#include "CglTrack.h"
#include "PHTrackOut.h"
#include "getClass.h"

typedef PHIODataNode<AccCluster> AccClusterNode_t;

using namespace std;

AccclusterReco::AccclusterReco(const string &name, const int ver)
 : SubsysReco(name)
{
  version = ver; 
  d_accproj = 0;
  return;
}

AccclusterReco::~AccclusterReco()
{
  delete d_accproj;
  return;
}

int AccclusterReco::Init(PHCompositeNode* topNode)
{
  // Make your projection object...
  d_accproj = new AccProj();

  // Find the DST node so we can put objects there
  PHNodeIterator iter(topNode);
  PHCompositeNode* dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode","DST"));

  // make the data objects
  AccCluster *d_acccluster = 0;
  switch ( version ){
    case 1:
      d_acccluster = new AccClusterv1();
      break;

    default:
      cout << PHWHERE << Name() << "... Unknown version requested: " << version << endl;
      cout << PHWHERE << Name() << "... You will receive version 1 instead. " << endl;

      d_acccluster = new AccClusterv1();
      break;
  }

  // Make node and put in tree...
  AccClusterNode_t* acccluster = new PHIODataNode<AccCluster>(d_acccluster,"AccCluster","PHObject");
  dstNode->addNode(acccluster);

  return 0;
}


int AccclusterReco::ResetEvent(PHCompositeNode *topNode)
{
  //  Use Fun4AllServer as the easy way to get the data...
  AccCluster* d_acccls = findNode::getClass<AccCluster> (topNode, "AccCluster");
  
  // We reset all the data containing Nodes...
  d_acccls->Reset();
  
  return 0;
}

int AccclusterReco::process_event(PHCompositeNode* topNode)
{
  static const unsigned int NBOXES=4;
  
  CglTrack*   d_cgl    = findNode::getClass<CglTrack>   (topNode, "CglTrack");
  CglTrack*   d_scgl   = findNode::getClass<CglTrack>   (topNode, "CglTrackBack");
  PHTrackOut* d_proj   = findNode::getClass<PHTrackOut> (topNode, "PHTrackOut");
  PHTrackOut* d_sproj  = findNode::getClass<PHTrackOut> (topNode, "PHTrackOutBack");
  
  AccRaw*     d_accraw = findNode::getClass<AccRaw>     (topNode, "AccRaw");
  AccCluster* d_acccls = findNode::getClass<AccCluster> (topNode, "AccCluster");
  
  //  OK...Do the analysis...
  if (d_cgl && d_scgl && d_proj && d_sproj && d_accraw && d_acccls)
    {
      
      int iacc=0;
      
      for (unsigned int icgl=0; icgl < d_cgl->get_CglNTrack() ; icgl++)
	{	  
	  //  Set up all the pointers to other structures...
	  int dchid = d_cgl->get_dctracksid(icgl);
	  
	  //  OK, get all the 4-box projections...including swapped...
	  float pc2x = d_proj->get_projectionPc2(dchid,0); 
	  float pc2y = d_proj->get_projectionPc2(dchid,1); 
	  float pc2z = d_proj->get_projectionPc2(dchid,2); 
	  float pc3x = d_proj->get_projectionPc3(dchid,0); 
	  float pc3y = d_proj->get_projectionPc3(dchid,1); 
	  float pc3z = d_proj->get_projectionPc3(dchid,2); 
	  
	  float spc2x = d_sproj->get_projectionPc2(dchid,0); 
	  float spc2y = d_sproj->get_projectionPc2(dchid,1); 
	  float spc2z = d_sproj->get_projectionPc2(dchid,2); 
	  float spc3x = d_sproj->get_projectionPc3(dchid,0); 
	  float spc3y = d_sproj->get_projectionPc3(dchid,1); 
	  float spc3z = d_sproj->get_projectionPc3(dchid,2); 
	  
	  //  OK, now that we know the PC projections...time to get ACC:
	  int  boxid[NBOXES];
	  int sboxid[NBOXES];
	  PHBoolean     struck = False;
	  PHBoolean swapStruck = False;
	  for (unsigned int i=0; i<NBOXES; i++)
	    {
	      boxid[i]  = d_accproj->getBoxIDfromXYZ( pc2x,  pc2y,  pc2z,  pc3x,  pc3y,  pc3z, i);
	      sboxid[i] = d_accproj->getBoxIDfromXYZ(spc2x, spc2y, spc2z, spc3x, spc3y, spc3z, i);
	      
	      if (-1< boxid[i] &&  boxid[i]<ACC::ACC_NBOX)     struck = True;
	      if (-1<sboxid[i] && sboxid[i]<ACC::ACC_NBOX) swapStruck = True;
	    }
	  int hitid      = d_accproj->getHitIDfromXYZ(pc2x, pc2y, pc2z, pc3x, pc3y, pc3z);
	  int hitconfig  = d_accproj->getHitConfig(boxid[0], boxid[1], boxid[2], boxid[3], hitid);	  
	  int shitid     = d_accproj->getHitIDfromXYZ(spc2x, spc2y, spc2z, spc3x, spc3y, spc3z);
	  int shitconfig = d_accproj->getHitConfig(sboxid[0], sboxid[1], sboxid[2], sboxid[3], shitid);
	  
	  // OK, if you hit ANY of the boxes...make another quad-box
	  // and store the index to the quad-box in cgl...
	  if( struck ) 
	    {
	      
	      d_acccls->AddCluster(iacc);
	      d_acccls->set_ncluster(iacc+1);
	      
	      // HitID
	      if (-1<hitid && hitid<ACC::ACC_NBOX) 
		{
		  d_acccls->get_cluster(iacc)->set_aerhitid    (hitid);
		  d_acccls->get_cluster(iacc)->set_aerhitconfig(hitconfig);
		}
	      
	      for (unsigned int i=0; i<NBOXES; i++)
		{
		  if (-1< boxid[i] &&  boxid[i]<ACC::ACC_NBOX)
		    {
		      d_acccls->get_cluster(iacc)->set_aerph1(i, d_accraw->get_adc(boxid[i], 0));
		      d_acccls->get_cluster(iacc)->set_aerph2(i, d_accraw->get_adc(boxid[i], 1));
		      d_acccls->get_cluster(iacc)->set_aert1 (i, d_accraw->get_tdc(boxid[i], 0));
		      d_acccls->get_cluster(iacc)->set_aert2 (i, d_accraw->get_tdc(boxid[i], 1));
		      if(d_accraw->get_adcpost(boxid[i], 0)==1023) d_acccls->get_cluster(iacc)->set_aerph1(i, +9999);
		      if(d_accraw->get_adcpost(boxid[i], 1)==1023) d_acccls->get_cluster(iacc)->set_aerph2(i, +9999);
		    }
		  else
		    {
		      d_acccls->get_cluster(iacc)->set_aerph1(i, -9999);
		      d_acccls->get_cluster(iacc)->set_aerph2(i, -9999);
		      d_acccls->get_cluster(iacc)->set_aert1 (i, -9999);
		      d_acccls->get_cluster(iacc)->set_aert2 (i, -9999);
		    }
		}
	      
	      d_cgl->set_accrecid(icgl, (short)iacc);
	      iacc++;
	    }
	  else
	    {
	      d_cgl->set_accrecid(icgl, -9999);
	    }
	  
	  
	  // OK, if you hit ANY of the "swapped" boxes...make another quad-box
	  // and store the index to the quad-box in scgl...
	  if( swapStruck ) 
	    {
	      
	      d_acccls->AddCluster(iacc);
	      d_acccls->set_ncluster(iacc+1);
	      
	      // sHitID
	      if (-1<shitid && shitid<ACC::ACC_NBOX) {
		d_acccls->get_cluster(iacc)->set_aerhitid    (shitid);
		d_acccls->get_cluster(iacc)->set_aerhitconfig(shitconfig);
	      }
	      
	      for (unsigned int i=0; i<NBOXES; i++)
		{
		  if (-1<sboxid[i] && sboxid[i]<ACC::ACC_NBOX)
		    {
		      d_acccls->get_cluster(iacc)->set_aerph1(i, d_accraw->get_adc(sboxid[i], 0));
		      d_acccls->get_cluster(iacc)->set_aerph2(i, d_accraw->get_adc(sboxid[i], 1));
		      d_acccls->get_cluster(iacc)->set_aert1 (i, d_accraw->get_tdc(sboxid[i], 0));
		      d_acccls->get_cluster(iacc)->set_aert2 (i, d_accraw->get_tdc(sboxid[i], 1));
		      if(d_accraw->get_adcpost(sboxid[i], 0)==1023) d_acccls->get_cluster(iacc)->set_aerph1(i, +9999);
		      if(d_accraw->get_adcpost(sboxid[i], 1)==1023) d_acccls->get_cluster(iacc)->set_aerph2(i, +9999);
		    }
		  else
		    {
		      d_acccls->get_cluster(iacc)->set_aerph1(i, -9999);
		      d_acccls->get_cluster(iacc)->set_aerph2(i, -9999);
		      d_acccls->get_cluster(iacc)->set_aert1 (i, -9999);
		      d_acccls->get_cluster(iacc)->set_aert2 (i, -9999);
		    }
		}
	      d_scgl->set_accrecid(icgl, (short)iacc);
	      iacc++;
	    }  
	  else
	    {
	      d_scgl->set_accrecid(icgl, -9999);
	    }
	  
	}
    }
  
  return 0;
}
