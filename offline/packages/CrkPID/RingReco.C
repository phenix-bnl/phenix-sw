#include "RingReco.h"
#include "CrkProjv1.h"
#include "CrkRingMicrov4.h"
#include "CrkPID.hh"

#include <CglTrack.h>
#include <dCrkHitWrapper.h>
#include <DchTrack.h>
#include <dEmcClusterLocalExtWrapper.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <dPadClusterWrapper.h>
#include <PHTrackOut.h>
#include <T0Out.h>
#include <T0Outv1.h>

#include <PHPoint.h>
#include <PHLine.h>


#include <recoConsts.h>

#include <getClass.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeReset.h>


#include <gsl/gsl_math.h>

#include <iostream>

typedef PHIODataNode<PHObject> PHObjectNode_t;

using namespace std;

RingReco::RingReco(const string &name): SubsysReco(name)
{
  crkpid = NULL;

  return ;
}

RingReco::~RingReco()
{
  delete crkpid;
  return;
}

int 
RingReco::InitRun(PHCompositeNode *topNode)
{

  recoConsts *rc = recoConsts::instance();
  CreateNodeTree(topNode);

  int runnumber = rc->get_IntFlag("RUNNUMBER");
  
  if (rc->FlagExist("SIMULATIONFLAG"))
    {
      if (rc->get_IntFlag("SIMULATIONFLAG") >= 1)
	{
	  runnumber = -1;
	}
    }

  if( rc->get_IntFlag("LOOKATSIMFROMEMBED",1) == 0)
    {
      runnumber = rc->get_IntFlag("RUNNUMBER");
    }

  
  cout << "RingReco using Run number " << runnumber << endl;
  crkpid = new CrkPID(runnumber);

  return 0;
}

int 
RingReco::process_event(PHCompositeNode *topNode)
{
  // CrkRing microdst
  CrkRing *crkringmicro = findNode::getClass<CrkRing>(topNode,"CrkRing");
  if (!crkringmicro)
    {
      cout << "UDST Node CrkRing not found" << endl;
    }

  // CrkRing microdst
  CrkRing *crkringmicroBG = findNode::getClass<CrkRing>(topNode,"CrkRingBack");
  if (!crkringmicroBG)
    {
      cout << "UDST Node CrkRingBack not found" << endl;
    }

  // CrkProj 
  CrkProj *crkprojmicro = findNode::getClass<CrkProj>(topNode,"CrkProj");
  if (!crkprojmicro)
    {
      cout << PHWHERE << "UDST Node CrkProj not found" << endl;
    }

  // CrkProjBG 
  CrkProj *crkprojBGmicro = findNode::getClass<CrkProj>(topNode,"CrkProjBG");
  if (!crkprojBGmicro)
    {
      cout << PHWHERE << "UDST Node CrkProjBG not found" << endl;
    }


  float EMC_MIN_E = 0.1;
  float PC2_DPHI_MAX = 0.015;
  float PC2_DZ_MAX = 5.0;
  float PC3_DPHI_MAX = 0.02;
  float PC3_DZ_MAX = 5.0;
  int nrings = 0;
  int nsrings = 0;

  PHTrackOut *PHTrack = findNode::getClass<PHTrackOut>(topNode,"PHTrackOut");
  if (!PHTrack)
    {
      cout << PHWHERE << "ERROR: Node PHTrackOut not found." << endl;
      return -1;
    }

  DchTrack *dchtrack = findNode::getClass<DchTrack>(topNode,"DchTrack");
  if (!dchtrack)
    {
      cout << PHWHERE << "ERROR: Node DchTrack not found." << endl;
      return -1;
    }

  //FM get the cgl node from udst
  CglTrack *cgltrack = findNode::getClass<CglTrack>(topNode,"CglTrack");
  if (!cgltrack)
    {
      cout << PHWHERE << "Node CglTrack not found" << endl;
      return -1;
    }

  CglTrack *cgltrackBG = findNode::getClass<CglTrack>(topNode,"CglTrackBack");
  if (!cgltrackBG)
    {
      cout << PHWHERE << "Node CglTrackBack not found" << endl;
      return -1;
    }

  dPadClusterWrapper *dPc1Cluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc1Cluster");
  if (!dPc1Cluster)
    {
      cout << PHWHERE << "ERROR: Node dPc1Cluster not found." << endl;
      return -1;
    }

  dPadClusterWrapper *dPc2Cluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc2Cluster");
  if (!dPc2Cluster)
    {
      cout << PHWHERE << "ERROR: Node dPc2Cluster not found." << endl;
      return -1;
    }

  dPadClusterWrapper *dPc3Cluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc3Cluster");
  if (!dPc3Cluster)
    {
      cout << PHWHERE << "ERROR: Node dPc3Cluster not found." << endl;
      return -1;
    }

  dEmcClusterLocalExtWrapper *dEmcClusterLocalExt = NULL;
  emcClusterContainer* emcClusters = findNode::getClass<emcClusterContainer>(topNode,"emcClusterContainer");

  if (! emcClusters)
    {
      dEmcClusterLocalExt = findNode::getClass<dEmcClusterLocalExtWrapper>(topNode,"dEmcClusterLocalExt");
    }
  if (!dEmcClusterLocalExt && !emcClusters)
    {
      cout << PHWHERE << "ERROR: none of emcClusterContainer and "
	   << "dEmcClusterLocalExt found."
	   << endl;
      return -1;
    }

  dCrkHitWrapper* dCrkHit = findNode::getClass<dCrkHitWrapper>(topNode,"dCrkHit");
  if (!dCrkHit)
    {
      cout << PHWHERE << "ERROR: Node dCrkHit not found." << endl;
      return -1;
    }


  T0Out *d_t0 = findNode::getClass<T0Out>(topNode,"T0Out");

  float TimeZero = 0.;
  if (d_t0 && d_t0->isValid())
    {
      TimeZero = d_t0->get_T0();
    }
  else
    {
      cout << PHWHERE << "No Time Zero available" << endl;
    }

  crkpid->SetBbcT0(TimeZero);

  crkpid->SetCrkHitFromTop(topNode);

  // Loop over all charged tracks and make the damned rings!!!
  for (unsigned int i = 0; i < PHTrack->get_PHNTrack(); i++)
    {

      // Now re-calculate the track direction based on pc1-pc2 or
      // pc1-pc3 or pc1-EMC
      // Start point   : PC1 used by the DC-PC1 reconstruction.
      // End point: (From lowest priority to the highest priority)
      //   PC2 PROJECTION (CALCULATED)
      //   good EMC HIT position
      //   good PC3 HIT position
      //   good PC2 HIT position
      unsigned int dchindex = PHTrack->get_trackIndex(i);
      int pc1ptr = dchtrack->get_pc1hit(dchindex);
      if (pc1ptr > -1 && PHTrack->get_ifIntersectPc2(i))
        {

          // Pc1 cluster used by Dch tracking
          float pc1x = dPc1Cluster->get_xyz(0, pc1ptr);
          float pc1y = dPc1Cluster->get_xyz(1, pc1ptr);
          float pc1z = dPc1Cluster->get_xyz(2, pc1ptr);
          PHPoint pstart(pc1x, pc1y, pc1z);

          // They are for temporary
	  float pc2x = PHTrack->get_projectionPc2(i, 0);
	  float pc2y = PHTrack->get_projectionPc2(i, 1);
	  float pc2z = PHTrack->get_projectionPc2(i, 2);
	  PHPoint pend = PHPoint(pc2x, pc2y, pc2z);

          // Pointer for each clusters
          int emcptr = cgltrack->get_emcclusid(i);
          if (emcptr > -1)
            {
              if ( emcClusters )
                {
                  emcClusterContent* clus = emcClusters->getCluster(emcptr);
                  if ( clus->ecore() >= EMC_MIN_E )
                    {
                      pend = PHPoint(clus->x(), clus->y(), clus->z());
                    }
                }
              else if ( dEmcClusterLocalExt )
                {
                  if (dEmcClusterLocalExt->get_ecore(emcptr) >= EMC_MIN_E)
                    {
                      float emcx = dEmcClusterLocalExt->get_xyz(0, emcptr);
                      float emcy = dEmcClusterLocalExt->get_xyz(1, emcptr);
                      float emcz = dEmcClusterLocalExt->get_xyz(2, emcptr);
                      pend = PHPoint(emcx, emcy, emcz);
                    }
                }
            }
	  // If there is a good PC3 cluster, use it.
          int pc3ptr = cgltrack->get_pc3clusid(i);
          if (pc3ptr > -1 && dPc3Cluster->get_xyz(0, pc3ptr) != 0. &&
              PHTrack->get_projectionPc3(i, 1) != 0.)
            {
              float pc3phi = atan(dPc3Cluster->get_xyz(1, pc3ptr) /
                                  dPc3Cluster->get_xyz(0, pc3ptr));
              float projpc3phi = atan(PHTrack->get_projectionPc3(i, 1) /
                                      PHTrack->get_projectionPc3(i, 0));
              float dphi = pc3phi - projpc3phi;
              float dz = dPc3Cluster->get_xyz(2, pc3ptr) - PHTrack->get_projectionPc3(i, 2);
              if (fabs(dphi) < PC3_DPHI_MAX && fabs(dz) < PC3_DZ_MAX)
                {
                  float pc3x = dPc3Cluster->get_xyz(0, pc3ptr);
                  float pc3y = dPc3Cluster->get_xyz(1, pc3ptr);
                  float pc3z = dPc3Cluster->get_xyz(2, pc3ptr);
                  pend = PHPoint(pc3x, pc3y, pc3z);

                }
            }

	  // If there is a good PC2 cluster, use it.
          int pc2ptr = cgltrack->get_pc2clusid(i);
          if (pc2ptr > -1 && dPc2Cluster->get_xyz(0, pc2ptr) != 0. &&
              PHTrack->get_projectionPc2(i, 1) != 0.)
            {
              float pc2phi = atan(dPc2Cluster->get_xyz(1, pc2ptr) /
                                  dPc2Cluster->get_xyz(0, pc2ptr));
              float projpc2phi = atan(PHTrack->get_projectionPc2(i, 1) /
                                      PHTrack->get_projectionPc2(i, 0));
              float dphi = pc2phi - projpc2phi;
              float dz = dPc2Cluster->get_xyz(2, pc2ptr) - PHTrack->get_projectionPc2(i, 2);
              if (fabs(dphi) < PC2_DPHI_MAX && fabs(dz) < PC2_DZ_MAX)
                {
                  float pc2x = dPc2Cluster->get_xyz(0, pc2ptr);
                  float pc2y = dPc2Cluster->get_xyz(1, pc2ptr);
                  float pc2z = dPc2Cluster->get_xyz(2, pc2ptr);
                  pend = PHPoint(pc2x, pc2y, pc2z);

                }
            }

          PHLine rich_proj(pstart, pend);
          rich_proj.normalize();
          PHLine srich_proj = ReflectInZ(rich_proj);

          CrkPIDout rich;
          CrkPIDout srich;
          crkpid->AssociateTrack(rich_proj, &rich);
          crkpid->AssociateTrack(srich_proj, &srich);

          if (rich.accepted && rich.npmt1 > 1 && crkringmicro)
            {
              crkringmicro->set_CrkNRing(nrings + 1); //index starts at 0
              crkringmicro->set_TClonesArraySize(nrings + 1);
              crkringmicro->AddCrkRing(nrings);
              crkringmicro->set_accepted(nrings, rich.accepted);
              crkringmicro->set_panel(nrings, rich.panel);
              crkringmicro->set_npmt0(nrings, rich.npmt0);
              crkringmicro->set_npmt1(nrings, rich.npmt1);
              crkringmicro->set_npmt2(nrings, rich.npmt2);
              crkringmicro->set_npmt3(nrings, rich.npmt3);
              crkringmicro->set_npe0(nrings, rich.npe0);
              crkringmicro->set_npe1(nrings, rich.npe1);
              crkringmicro->set_npe2(nrings, rich.npe2);
              crkringmicro->set_npe3(nrings, rich.npe3);
              crkringmicro->set_chi2(nrings, rich.chi2);
              crkringmicro->set_disp(nrings, rich.disp);
              crkringmicro->set_tcrk(nrings, rich.time);
              crkringmicro->set_cross_phi(nrings, rich.cross_phi);
              crkringmicro->set_cross_z(nrings, rich.cross_z);
              cgltrack->set_richringid(i, nrings);   // FM update cgl
              crkringmicro->set_cgltrackid(nrings, i); //FM

              if (rich.center[0] != 0)
                {
		  if(atan2(rich.center[1], rich.center[0]) < -0.5*M_PI){
		    crkringmicro->set_center_phi(nrings, atan2(rich.center[1], rich.center[0])+2*M_PI);
		  }else{
		    crkringmicro->set_center_phi(nrings, atan2(rich.center[1], rich.center[0]));
		  }
		}
	      crkringmicro->set_center_z(nrings, rich.center[2]);


	      if(crkprojmicro)
		{
		  // Add the track projections to CrkProj for later use in the compact CNT storage scheme
		  crkprojmicro->set_CrkNProj(nrings+1);
		  crkprojmicro->set_TClonesArraySize(nrings+1);
		  crkprojmicro->AddCrkProj(nrings);
		  crkprojmicro->set_pstartx(nrings, (float) pstart.getX());
		  crkprojmicro->set_pstarty(nrings, (float) pstart.getY());
		  crkprojmicro->set_pstartz(nrings, (float) pstart.getZ());
		  crkprojmicro->set_pendx(nrings, (float) pend.getX());
		  crkprojmicro->set_pendy(nrings, (float) pend.getY());
		  crkprojmicro->set_pendz(nrings, (float) pend.getZ());		  
		  crkprojmicro->set_ringid(nrings, nrings);
		  crkprojmicro->set_cgltrackid(nrings, i);		

		}

	      nrings++;		
	    }
          if (srich.accepted && srich.npmt1 > 1 && crkringmicroBG)
            {
              crkringmicroBG->set_CrkNRing(nsrings + 1); // index starts at 0
              crkringmicroBG->set_TClonesArraySize(nsrings + 1);
              crkringmicroBG->AddCrkRing(nsrings);
              crkringmicroBG->set_accepted(nsrings, srich.accepted);
              crkringmicroBG->set_panel(nsrings, srich.panel);
              crkringmicroBG->set_npmt0(nsrings, srich.npmt0);
              crkringmicroBG->set_npmt1(nsrings, srich.npmt1);
              crkringmicroBG->set_npmt2(nsrings, srich.npmt2);
              crkringmicroBG->set_npmt3(nsrings, srich.npmt3);
              crkringmicroBG->set_npe0(nsrings, srich.npe0);
              crkringmicroBG->set_npe1(nsrings, srich.npe1);
              crkringmicroBG->set_npe2(nsrings, srich.npe2);
              crkringmicroBG->set_npe3(nsrings, srich.npe3);
              crkringmicroBG->set_chi2(nsrings, srich.chi2);
              crkringmicroBG->set_disp(nsrings, srich.disp);
              crkringmicroBG->set_tcrk(nsrings, srich.time);
              crkringmicroBG->set_cross_phi(nsrings, srich.cross_phi);
              crkringmicroBG->set_cross_z(nsrings, srich.cross_z);
              cgltrackBG->set_richringid(i, nsrings);  
              crkringmicroBG->set_cgltrackid(nsrings, i);  

              if (srich.center[0] != 0)
                {
		  if(atan2(srich.center[1], srich.center[0])<-0.5*M_PI){
		    crkringmicroBG->set_center_phi(nsrings, atan2(srich.center[1],srich.center[0])+2*M_PI);
		  }else{
		    crkringmicroBG->set_center_phi(nsrings, atan2(srich.center[1],srich.center[0]));
		  }             
                }
              crkringmicroBG->set_center_z(nsrings, srich.center[2]);


	      if(crkprojBGmicro)
		{
		  // Add the track projections to CrkProj for later use in the compact CNT storage scheme

		  // Note: this is the track projection before reflection!

		  crkprojBGmicro->set_CrkNProj(nsrings+1);
		  crkprojBGmicro->set_TClonesArraySize(nsrings+1);
		  crkprojBGmicro->AddCrkProj(nsrings);
		  crkprojBGmicro->set_pstartx(nsrings, pstart.getX());
		  crkprojBGmicro->set_pstarty(nsrings, pstart.getY());
		  crkprojBGmicro->set_pstartz(nsrings, pstart.getZ());
		  crkprojBGmicro->set_pendx(nsrings, pend.getX());
		  crkprojBGmicro->set_pendy(nsrings, pend.getY());
		  crkprojBGmicro->set_pendz(nsrings, pend.getZ());
		  crkprojBGmicro->set_ringid(nsrings, nsrings);
		  crkprojBGmicro->set_cgltrackid(nsrings, i);
		  
		}
	      else
		{
		  std::cout << PHWHERE << "CrkProjBG node not found when needed! Quit!" << endl;
		  exit(1);
		}

              nsrings++;      
            }

        } // Pc1 is used by Dch and there is a projection to Pc2

    } // end loop over charged tracks

  return 0;
}

int
RingReco::End(PHCompositeNode *topNode)
{
  // dumpfile.close();
  return 0;
}

int 
RingReco::CreateNodeTree(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);
  PHCompositeNode *dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (! dstNode)
    {
      cout << PHWHERE << " dstNode is missing doing nothing" << endl;
      return -1;
    }
  CrkRing *crkring = findNode::getClass<CrkRing>(dstNode,"CrkRing");
  if (!crkring)
    {
      crkring = new CrkRingMicrov4();
      PHObjectNode_t *CrkRingNode =
        new PHIODataNode<PHObject>(crkring, "CrkRing", "PHObject");
      dstNode->addNode(CrkRingNode);
    }

  crkring = findNode::getClass<CrkRing>(dstNode,"CrkRingBack");
  if (!crkring)
    {
      crkring = new CrkRingMicrov4();
      PHObjectNode_t *CrkRingBGNode =
        new PHIODataNode<PHObject>(crkring, "CrkRingBack", "PHObject");
      dstNode->addNode(CrkRingBGNode);
    }
PHCompositeNode* crkpidNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "CRKPID"));
  if (!crkpidNode)
    {
      crkpidNode = new PHCompositeNode("CRKPID");
      topNode->addNode(crkpidNode);
    }

  // This stores the projected Track lines for the RICH 
  CrkProj *crkproj = findNode::getClass<CrkProj>(topNode,"CrkProj");
  if (!crkproj)
    {
      crkproj = new CrkProjv1();
      PHDataNode<PHObject> *CrkProjNode =
        new PHDataNode<PHObject>(crkproj, "CrkProj", "PHObject");
      crkpidNode->addNode(CrkProjNode);
    }

  // This stores the projected swapped track lines for the RICH 
  crkproj = findNode::getClass<CrkProj>(topNode,"CrkProjBG");
  if (!crkproj)
    {
      crkproj = new CrkProjv1();
      PHDataNode<PHObject> *CrkProjBGNode =
        new PHDataNode<PHObject>(crkproj, "CrkProjBG", "PHObject");
      crkpidNode->addNode(CrkProjBGNode);
    }

  return 0;
}


PHLine 
RingReco::ReflectInZ(const PHLine &trk)
{
  PHPoint base_zref = trk.getBasepoint();
  PHVector dir_zref = trk.getDirection();
  base_zref.setZ( -1.0*base_zref.getZ());
  dir_zref.setZ( -1.0*dir_zref.getZ());
  return PHLine(base_zref, dir_zref);
}

int 
RingReco::ResetEvent(PHCompositeNode *topNode)
{
  PHNodeIterator mainIter(topNode);
  PHNodeReset reset;
  if (mainIter.cd("CRKPID"))
    {
      mainIter.forEach(reset);
    }
  return 0;
}
