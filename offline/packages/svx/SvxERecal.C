#include <SvxERecal.h>

#include <iostream>
#include <iomanip>
#include "gsl/gsl_rng.h"



#include "phool.h"
#include <getClass.h>

#include "PHNodeIterator.h"
#include "PHTypedNodeIterator.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "SvxClusterContainer.h"
#include "SvxClusterInfo.h"

#include "Fun4AllReturnCodes.h"
#include "Fun4AllServer.h"
#include "recoConsts.h"
#include "PHGlobal.h"
#include "PHCentralTrack.h"
#include <VtxOrdercodes.h>


#include "PHPoint.h"
#include "PHCentralTrack.h"
#include "PHTrackOut.h"
#include "TriggerHelper.h"
#include "VtxOut.h"
#include "ErtOut.h"

 //--------get pythia information---------                                                                       
                                                                
#include "PHPythiaContainer.h"   
#include <McEvalSingleList.h>  
#include <TMCParticle.h> 
 //---------------------------------------                                                     

#include "SvxGhitList.h"
#include "SvxGhit.h"
#include "SvxRawhitList.h"
#include "SvxRawhit.h"
#include "SvxClusterList.h"
#include "SvxClusterInfo.h"
#include "SvxSegmentList.h"
#include "SvxGhitClusterList.h"
#include "SvxGhitRawhitList.h"
#include "SvxRawhitClusterList.h"
#include "SvxRawhitCluster.h"
#include "SvxClusterv1.h"
#include "SvxSegmentv1.h"
#include "SvxCentralTrackList.h"
#include "SvxCentralTrack.h"
#include "fkinWrapper.h"

#include "TFile.h"
#include "TNtuple.h"
#include "TF1.h"
#include "TLorentzVector.h"
#include "TVector3.h"
#include "TRandom2.h"



#include "DchTrack.h"
#include "dDchTracksWrapper.h"
#include "dDchHitWrapper.h"
#include "dDchGhitHitsWrapper.h"
#include "dcghitWrapper.h"


using namespace std;
SvxClusterContainer* d_container = NULL;



SvxERecal::SvxERecal(const std::string &name): SubsysReco(name)
{
  d_redotracking = false;
  // histos_out = filename;
  d_container = new SvxClusterContainer();

  return;
}
SvxERecal::~SvxERecal()
{
}

int SvxERecal::Init(PHCompositeNode *topNode)
{

  return EVENT_OK;
}

int SvxERecal::InitRun(PHCompositeNode *topNode)
{
  if (verbosity>0)
    {
      topNode->print();
    }

  return EVENT_OK;
}


int SvxERecal::End(PHCompositeNode *topNode)
{

  return EVENT_OK;
}


int SvxERecal::process_event(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  SvxCentralTrackList* svxcentraltracklist = findNode::getClass<SvxCentralTrackList>( topNode, "SvxCentralTrackList");
  if (!svxcentraltracklist) { cerr << PHWHERE << " No SvxCentralTrackList object found." << endl; return DISCARDEVENT; }

  PHCentralTrack* phcntrltrk = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if(!phcntrltrk) {cerr << PHWHERE << "ERROR: No PHCentralTrack object" << endl; return DISCARDEVENT;}

  SvxSegmentList* svxsegmentlist = findNode::getClass<SvxSegmentList>(topNode,"SvxSegmentList");
  if(!svxsegmentlist) {cerr << PHWHERE << "ERROR: No SvxSegmentLIst object" << endl; return DISCARDEVENT;}


  VtxOut *vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if(!vtxout) {cerr << PHWHERE << "ERROR: No VtxOut object" << endl; return DISCARDEVENT;}

  SvxClusterList* svxclusterlist = findNode::getClass<SvxClusterList>(topNode,"SvxClusterList");
  if(!svxclusterlist) {cerr << PHWHERE << "ERROR: No SvxClusterList object" << endl; return DISCARDEVENT;}
  


  PHPoint seedvtx   = vtxout->get_Vertex("SVX");
 
  if (d_redotracking)
    {

      PHCompositeNode *svxsubNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SVXSUB"));
      if (!svxsubNode) {
	svxsubNode = new PHCompositeNode("SVXSUB");
	topNode->addNode(svxsubNode);
      }
      // SvxClusterContainer does not exist on the node tree, need to create it to run SvxCentralTrackReco
      PHDataNode<SvxClusterContainer>* SvxClusterContainerNode = NULL;
      SvxClusterContainerNode = (PHIODataNode<SvxClusterContainer>*)iter.findFirst("PHDataNode", "SvxClusterContainer");
      if (!SvxClusterContainerNode)
	{
	  SvxClusterContainerNode =
	    new PHDataNode<SvxClusterContainer>(d_container, "SvxClusterContainer");
	  svxsubNode->addNode(SvxClusterContainerNode);
	}
    

      // extract the beam x and y coordinates from the seed vertex to put in SvxClusterContainer
      double beam_x =  seedvtx.getX();
      double beam_y = seedvtx.getY();
      d_container->set_beam_center(beam_x, beam_y);
      /// fill SvxClusters in SvxClusterContainer
      /// clear the container before loading clusters.
      d_container->clear();
      /// load clusters
      d_container->load_clusters(svxclusterlist);
    }

  int nsvxseg = svxsegmentlist->get_nSegments();
  int nsvxcnttrk = svxcentraltracklist->get_nCentralTracks();
 
  int matchedsegments[nsvxcnttrk];
  for (int q = 0; q<nsvxcnttrk;q++)
    {
      matchedsegments[q]=-9999;
    }
  for (int i = 0;i<nsvxcnttrk;i++)
    {
      SvxCentralTrack* cntrltrk = svxcentraltracklist->getCentralTrack(i);
      int dchindex = cntrltrk->getDchIndex();
      float emce = phcntrltrk->get_emce(dchindex);
      float mom = phcntrltrk->get_mom(dchindex);
      float n0 = phcntrltrk->get_n0(dchindex);
      if (emce/mom > 0.65 && n0>0) // identify electron candidate tracks, could be modified to select any sort of track
      	{
	
	  SvxClusterInfo* clusterinfol0 = cntrltrk->getClusterInfo(3);
	  SvxClusterInfo* clusterinfol1 = cntrltrk->getClusterInfo(2);
	  SvxClusterInfo* clusterinfol2 = cntrltrk->getClusterInfo(1);
	  SvxClusterInfo* clusterinfol3 = cntrltrk->getClusterInfo(0);
	  int clust[4];
	  int nsvxcentclusters =0;
	  for (int q=0;q<4;q++)
	    {
	      clust[q]=-9999;
	    }
	  if (clusterinfol0)
	    {
	      clust[0]=  clusterinfol0->getClusterId();
	      nsvxcentclusters = nsvxcentclusters + 1;
	    }
	  if (clusterinfol1)
	    {
	      clust[1] =  clusterinfol1->getClusterId();
	      nsvxcentclusters = nsvxcentclusters + 1;
	    }
	  if (clusterinfol2)
	    {
	      clust[2] = clusterinfol2->getClusterId();
	      nsvxcentclusters = nsvxcentclusters + 1;
	    }
	  if (clusterinfol3) 
	    {
	      clust[3] = clusterinfol3->getClusterId();
	      nsvxcentclusters = nsvxcentclusters + 1;
	    }
	  //------------------------------------
	
	  int segmentmatching[nsvxseg];
	  for (int q = 0;q<nsvxseg;q++)
	    {
	      segmentmatching[q]=0;
	    }
	  int matchingparam = 0;
	  int matchedsegment =-9999;
	  //loop over svxsegments to identify maximum contributor
	  for (int j=0;j<nsvxseg;j++)
	    {
	      SvxSegment* svxsegment = svxsegmentlist->get_segment(j);
	      if (!svxsegment){}
	      int svxcluster[4];
	      for (int q = 0; q<4;q++)
		{
		  svxcluster[q]=-9998;
		}
	      svxcluster[0]= svxsegment->getClusterID(0);
	      svxcluster[1] = svxsegment->getClusterID(1);
	      svxcluster[2] = svxsegment->getClusterID(2);
	      svxcluster[3] = svxsegment->getClusterID(3);
	      for (int c = 0;c<4;c++)
		{
		  for (int sc = 0;sc<4;sc++)
		    {
		      if (svxcluster[sc]==clust[c])
			{
			  segmentmatching[j] ++;
			}
		    }
		}
	      if (verbosity>0)
		{
		  cout << "segment matching for " << j << " is " << segmentmatching[j] << endl;
		  cout << clust[0] << "  |   " << clust[1] << "   |   " << clust[2] << "  |  " << clust[3] << "   |  " << endl;
		  cout << svxcluster[0] << "  |   " << svxcluster[1] << "   |   " << svxcluster[2] << "  |  " << svxcluster[3] << "   |  " << endl;
		}

	      
	      if (segmentmatching[j] > matchingparam)
		{
		  matchingparam = segmentmatching[j];
		  matchedsegment=j;
		}
	      else 
		{
		  if (verbosity>0)
		    {
		      cout << "no good match" <<endl;
		    }
		}
	    }
	  
	  if (matchingparam>0)
	    {

	      matchedsegments[i]=matchedsegment;
	    }

	}
    }


  //extract relevant vertex information  prior to erasing the VtxOut object so the information can be re-saved
  float seed[3];
  seed[0] = seedvtx.getX();
  seed[1] = seedvtx.getY();
  seed[2] = seedvtx.getZ();



  PHPoint verpoint_seederror = vtxout->get_VertexError("SVX");
  float seederr[3];
  seederr[0] = verpoint_seederror.getX();
  seederr[1] = verpoint_seederror.getY();
  seederr[2] = verpoint_seederror.getZ();


  PHPoint bbcvtx = vtxout->get_Vertex("BBC");
  float bbc[3];
  bbc[0] = bbcvtx.getX();
  bbc[1] = bbcvtx.getY();
  bbc[2] = bbcvtx.getZ();

  PHPoint verpoint_bbcerror = vtxout->get_VertexError("BBC");
  float bbcerr[3];
  bbcerr[0] = verpoint_bbcerror.getX();
  bbcerr[1] = verpoint_bbcerror.getY();
  bbcerr[2] = verpoint_bbcerror.getZ();


  vtxout->Reset();

  vtxout->AddVtx("SVX", seed, seederr, VTX::SVXORDER);
  vtxout->AddVtx("BBC", bbc, bbcerr, VTX::BBCORDER);


  if (d_redotracking)
    {
      svxcentraltracklist->Reset();   //erase SvxCentralTracks so that SvxCentralTrackReco can write new tracks
    }
      
      // This is the current way desired tracks are removed from the precise vertex determination, electron tracks have a score of -13
  int deltasegmentsv =0;
  for (int i=0;i<nsvxcnttrk;i++)
    { 
      if (matchedsegments[i]>-1)
	{
	  deltasegmentsv = deltasegmentsv+1;
	  if (svxsegmentlist->get_segment(matchedsegments[i]))
	    {
	      SvxSegment* svxsegment = svxsegmentlist->get_segment(matchedsegments[i]);
	      svxsegment-> setSegmentScore(-13);
	    }
	  
	}
    }
  
  
  return EVENT_OK;

   
   
}


