#include "SvxCompactToDST.h"
#include "SvxHitMap.h"
#include "SvxHitMapEntry.h"
#include "SvxTrackMap.h"
#include "SvxTrackMapEntry.h"
#include "SvxCentralTrackMap.h"
#include "SvxCentralTrackMapEntry.h"

#include <phool.h>
#include <PHTypedNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <recoConsts.h>
#include <RunToTime.hh>

#include <SvxClusterList.h>
#include <SvxClusterListv5.h>
#include <SvxCluster.h>
#include <SvxClusterv5.h>

#include <SvxSegmentList.h>
#include <SvxSegmentListv6.h>
#include <SvxSegment.h>
#include <SvxSegmentv6.h>

#include <SvxCentralTrackList.h>
#include <SvxCentralTrackListv9.h>
#include <SvxCentralTrack.h>
#include <SvxCentralTrackv9.h>

#include <svxDetectorGeo.hh>
#include <SvxSensor.h>
#include <SvxComponentGeom.h>

#include <TMath.h>

#include <vararray/VariableArray.h>

#include <limits>

using namespace std;
using namespace findNode;


float calc_chisq_fromquality(float quality, float score);


SvxCompactToDST::SvxCompactToDST(const string& name) :
  SubsysReco(name),
  fNewGeo(false),
  fGeo(NULL),
  m_event(0),
  m_skipCluster(false),
  m_test(false),
  m_testSelClus(false),
  m_print(false),
  have_svx_data(1)
//  m_nBadCluster(0),

{
}

SvxCompactToDST::~SvxCompactToDST()
{
  if (fNewGeo) // Wipe out only if newed in this class
    delete fGeo;
}

int SvxCompactToDST::Init(PHCompositeNode *topNode)
{
  return 0;
}

int SvxCompactToDST::InitRun(PHCompositeNode *topNode)
{
  VariableArray *svxmap = findNode::getClass<VariableArray>(topNode,"SvxHit_VarArray");
  if (!svxmap)
    {
      have_svx_data = 0;
      return 0;
    }
  int ret = CreateNodeTree(topNode);
  if (ret)
    {
      cerr << PHWHERE << "CreateNodeTree() returned with code " << ret << endl;
    }

  fGeo = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if (!fGeo)
    {
      cout << PHWHERE << "svxDetectorGeo node not found. Making new instance." << endl;
      fGeo = new svxDetectorGeo();
      fNewGeo = true;
      recoConsts *rc = recoConsts::instance();
      int runnumber = rc->get_IntFlag("RUNNUMBER");
      RunToTime *rt = RunToTime::instance();
      PHTimeStamp *Tsearch = rt->getBeginTime(runnumber);
      fGeo->Fetch_coordinateOffset(runnumber);
      fGeo->Fetch_svxPISApar(Tsearch);
      delete Tsearch;
    }

  if(verbosity > 0) topNode->print();

  cout << "SvxCompactToDST::InitRun" << endl;
  cout << "  testMode " << (m_test ? "on" : "off") << endl;
  if(m_test)
    {
      cout << "   ForSelectedCluster : " << (m_testSelClus ? "Yes" : "No") << endl;
    }

  m_event = 0;
//  m_nBadCluster = 0;
  return 0;
}

int SvxCompactToDST::process_event(PHCompositeNode *topNode)
{
  if (!  have_svx_data)
    {
      return EVENT_OK;
    }
  if(verbosity > 0 || m_test)
    cout << "SvxCompatToDST::" << __FUNCTION__ << " event : " << m_event << endl;

  FillSvxCentralTrack(topNode);
  FillSvxCentralTrack(topNode, 1); // for backlist

  FillSvxTrack(topNode);
  if(!m_skipCluster) FillSvxCluster(topNode);

  if(m_test)
    {
      CompareSvxTrackList(topNode);
      if(!m_skipCluster) CompareSvxClusterList(topNode);
      CompareSvxCentralTrackList(topNode);

      CompareSvxCentralTrackList(topNode, 1);
    }

  m_event++;
  return 0;
}

int SvxCompactToDST::End(PHCompositeNode *topNode)
{
  //if(m_nBadCluster>0){
  //  cout<<"SvxCompactToDST NBadCluster="<<m_nBadCluster<<"."<<endl;
  //  cout<<"SvxCompactToDST Check Svx geometry parameter. The geometry must be wrong"<<endl;
  //}


  return 0;
}

int SvxCompactToDST::CreateNodeTree(PHCompositeNode* topNode)
{
  PHNodeIterator iter(topNode);

  // Find DST node.
  PHCompositeNode *dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode)
    {
      cerr << PHWHERE << "DST Node missing, doing nothing." << endl;
      return 1;
    }

  // Find or create the SVX node.
  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SVX"));
  if (!svxNode)
    {
      cout << "SvxCompactToDST::CreateNodeTree(): SVX node not found. Adding to node tree." << endl;
      svxNode = new PHCompositeNode("SVX");
      dstNode->addNode(svxNode);
    }

  // Find or create the SvxCentralTrackList node.
  PHIODataNode<PHObject>* SvxCentralTrackListNode = 0;
  PHString svxcntrklst = (m_test) ? "SvxCentralTrackListTest" : "SvxCentralTrackList";
  /*
  // TODO add flag to handle BG if needed
  if (m_rndmassocflag != 0) {
  svxcntrklst += "Back";
  }
  svxcntrklst += "List";
  */
  SvxCentralTrackListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", svxcntrklst);
  if (!SvxCentralTrackListNode)
    {
      cout << "SvxCompactToDST::CreateNodeTree(): SvxCentralTrackListNode not found. Adding to node tree." << endl;
      SvxCentralTrackList* svxcentraltracks = new SvxCentralTrackListv9();
      SvxCentralTrackListNode = new PHIODataNode<PHObject>(svxcentraltracks, svxcntrklst, "PHObject");
      svxNode->addNode(SvxCentralTrackListNode);
    }

  PHIODataNode<PHObject>* SvxCentralTrackBackListNode = 0;
  PHString svxcntrkbklst = (m_test) ? "SvxCentralTrackBackListTest" : "SvxCentralTrackBackList";
  SvxCentralTrackBackListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", svxcntrkbklst);
  if (!SvxCentralTrackBackListNode)
    {
      cout << "SvxCompactToDST::CreateNodeTree(): SvxCentralTrackBackListNode not found. Adding to node tree." << endl;
      SvxCentralTrackList* svxcentralbgtracks = new SvxCentralTrackListv9();
      SvxCentralTrackBackListNode = new PHIODataNode<PHObject>(svxcentralbgtracks, svxcntrkbklst, "PHObject");
      svxNode->addNode(SvxCentralTrackBackListNode);
    }

  // Find or create the SvxSegmentList node.
  PHIODataNode<PHObject>* SvxSegmentListNode = NULL;
  PHString svxseglst = (m_test) ? "SvxSegmentListTest" : "SvxSegmentList";
  SvxSegmentListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", svxseglst);
  if(!SvxSegmentListNode)
    {
      cout << "SvxCompactToDST::CreateNodeTree(): SvxSegmentListNode not found. Adding to node tree." << endl;
      SvxSegmentList* svxsegments = new SvxSegmentListv6();
      SvxSegmentListNode = new PHIODataNode<PHObject>(svxsegments, svxseglst, "PHObject");
      svxNode->addNode(SvxSegmentListNode);
    }

  // Find or create the SvxClusterList node.
  PHIODataNode<PHObject>* SvxClusterListNode = NULL;
  PHString svxclslst = (m_test) ? "SvxClusterListTest" : "SvxClusterList";
  SvxClusterListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", svxclslst);
  if (!SvxClusterListNode)
    {
      cout << "SvxCompactToDST::CreateNodeTree(): SvxClusterListNode not found. Adding to node tree." << endl;
      SvxClusterList* svxclusters = new SvxClusterListv5();
      SvxClusterListNode = new PHIODataNode<PHObject>(svxclusters, svxclslst, "PHObject");
      svxNode->addNode(SvxClusterListNode);
    }

  return 0;
}

int SvxCompactToDST::FillSvxCentralTrack(PHCompositeNode* topNode, int flag)
{
  // Pointers to compact input objects
  SvxCentralTrackMap   *svxmap = getClass<SvxCentralTrackMap> (topNode, (flag==0 ? "SvxCentralTrack_comp" : "SvxCentralTrackBG_comp") );
  SvxHitMap         *svxhitmap = getClass<SvxHitMap>          (topNode, "SvxHit_comp"         );

  // Pointer to DST output object
  PHString    svxcntrklst = (m_test) ? "SvxCentralTrackListTest" : "SvxCentralTrackList";
  if(flag==1) svxcntrklst = (m_test) ? "SvxCentralTrackBackListTest" : "SvxCentralTrackBackList";

  SvxCentralTrackList *cntlist = getClass<SvxCentralTrackList>(topNode, svxcntrklst.getString());

  if (!svxmap)
    {
      cout << "SvxCompactToDST::FillSvxCentralTrack(): "<<(flag==0?"SvxCentralTrackMap":"SvxCentralTrackBGMap")<<" not found" << endl;
      return 1;
    }
  if (!svxhitmap)
    {
      cout << "SvxCompactToDST::FillSvxCentralTrack(): SvxHitMap not found" << endl;
      return 1;
    }
  if (!cntlist)
    {
      cout << "SvxCompactToDST::FillSvxCentralTrack(): "<<(flag==0?"SvxCentralTrackList":"<<SvxCentralTrackBackList")<<" not found" << endl;
      return 1;
    }

  if(verbosity > 0) cout << "SvxCompactToDST::" << __FUNCTION__ << " NCntTrack : " << svxmap->GetNentries() << endl;

  for (int itrk = 0; itrk < svxmap->GetNentries(); itrk++)
    {
      SvxCentralTrack* scnt = new SvxCentralTrackv9();
      const SvxCentralTrackMapEntry *svxmapent = svxmap->GetHit(itrk);

      // For scaling factors, check FillSvxHits.C
      scnt->setDchIndex(       svxmapent->get_DchIndex());
      scnt->setUnique(         svxmapent->get_Unique());
      scnt->setDCA2D(          svxmapent->get_DCA2D() * 1e-4);
      scnt->setDCAZ(           svxmapent->get_DCAZ() * 1e-4);
      scnt->setDCA2Dprimary(   svxmapent->get_DCA2Dprimary() * 1e-4);
      scnt->setDCAZprimary(    svxmapent->get_DCAZprimary() * 1e-4);
      scnt->setClosestApproach(svxmapent->get_ClosestApproach(0) * 1e-4,
			       svxmapent->get_ClosestApproach(1) * 1e-4,
			       svxmapent->get_ClosestApproach(2) * 1e-3);
      scnt->setChiSquareDPHI(  svxmapent->get_ChisquarePhi() * 1e-2);
      scnt->setChiSquareDZ(    svxmapent->get_ChisquareZ() * 1e-2);
      scnt->setChiSquare(      svxmapent->get_Chisquare() * 1e-2);
      scnt->setChiSquare2(     svxmapent->get_Chisquare2() * 1e-2);

      for (int l = 0; l < 4; l++)
	{
	  scnt->setLivePercentage(l,
				  svxmapent->get_LivePercent(l) * 1e-2);
	}
      scnt->setLinkQuality(    svxmapent->get_LinkQuality() * 1e-4);
      scnt->setLinkScore(      svxmapent->get_LinkScore() * 1e-2);

      scnt->set3MomentumAtPrimaryVertex(
					svxmapent->get_MomentumX() * 1e-3,
					svxmapent->get_MomentumY() * 1e-3,
					svxmapent->get_MomentumZ() * 1e-3
					);

      // Fill cluster info
      int nhit = svxmapent->get_NClusters();
      scnt->setNDF(2*(nhit-2)+2);

      for (int icls = 0; icls < nhit; icls++)
	{
	  SvxClusterInfov5 info;
	  int id = svxmapent->get_ClusterID(icls);
	  info.setClusterId(id);
	  float dphi = svxmapent->get_ClusterDPhi(icls);
	  info.setdproj(dphi * 1e-4);
	  info.setbend(0.0);
	  info.setzproj(svxmapent->get_ClusterDZ(icls) * 1e-4);

	  // Find this cluster in the SvxHitMap array.
	  // Assign mci as matching cluster index in SvxHitMap.
	  int mci = -1;
	  for (int iclus = 0; iclus < svxhitmap->GetNentries(); iclus++)
	    {
	      const SvxHitMapEntry *hitmapent = svxhitmap->GetHit(iclus);
	      if (id == hitmapent->get_id())
		{
		  mci = iclus;
		  break;
		}
	    }

	  // If a matching cluster is available in SvxHitMap,
	  // copy that data into SvxClusterInfo object.
	  if (mci >= 0)
	    {
	      const SvxHitMapEntry *hitmapent = svxhitmap->GetHit(mci);
	      int size_or_adc = hitmapent->get_adcandsize();
	      float x = hitmapent->get_x() * 1e-3;
	      float y = hitmapent->get_y() * 1e-3;
	      float z = hitmapent->get_z() * 1e-3;
	      int layer = fGeo->get_nearestLayer(sqrt(x * x + y * y));
	      SvxSensor* sensor = fGeo->get_nearestSensor(x, y, z, layer);
              if(sensor==NULL){ 
                if(verbosity > 0) {
                  cerr<<"SvxCompactToDST: no sensor found. Probably wrong geometry is used. Skip this cluster. ";
                  cerr<<"(x,y,z)=("<<x<<" "<<y<<" "<<z<<endl;
                }
                continue;
              }

	      if (layer < 2)
		{
		  info.setSize(size_or_adc);   // Layer 0 or 1: cluster size.
		}
	      else
		{
		  int x_adc = size_or_adc & 0xFF;
		  int z_adc = (size_or_adc >> 8) & 0xFF;
		  info.setAdc(x_adc, z_adc);   // Layer 2 or 3: x and u ADC information.
		}

	      info.setzproj(info.getzproj() + z); // this update using "z" info
	      info.setPosition(x, y, z);
	      info.setLayer(layer);
	      info.setLadder(sensor->get_ladder());
	      info.setSensor(sensor->get_sensor());
	      info.setNcold(hitmapent->get_ncold());
	      info.setNhot(hitmapent->get_nhot());

	    }

	  scnt->addClusterInfo(&info);
	}

      int ncentral = cntlist->get_nCentralTracks();
      cntlist->addCentralTrack(ncentral, *scnt);
      delete scnt;
    }

  return 0;
}

int SvxCompactToDST::FillSvxTrack(PHCompositeNode* topNode)
{

  // Pointer to compact input
  SvxTrackMap     *svxmap = getClass<SvxTrackMap>(topNode, "SvxTrack_comp");

  // Pointer to DST output object
  PHString svxseglst = (m_test) ? "SvxSegmentListTest" : "SvxSegmentList";
  SvxSegmentList *seglist = getClass<SvxSegmentList>(topNode, svxseglst.getString());

  if (!svxmap)
    {
      cout << "SvxCompactToDST::FillSvxCentralTrack(): SvxTrack_comp not found" << endl;
      return 1;
    }
  if (!seglist)
    {
      cout << "SvxCompactToDST::FillSvxCentralTrack(): SvxSegmentList not found" << endl;
      return 1;
    }

  for (int iseg = 0; iseg < svxmap->GetNentries(); iseg++)
    {
      SvxSegment *seg   = new SvxSegmentv6();
      const SvxTrackMapEntry *svxmapent = svxmap->GetHit(iseg);

      seg->setIsPositive(              svxmapent->get_charge() == 1);
      seg->setQuality(                 svxmapent->get_quality() * 1e-2);
      seg->setDCA2D(                   svxmapent->get_dca2d() * 1e-4);
      seg->setDCA(                     svxmapent->get_dca3d() * 1e-4);
      seg->setClosestApproach(         svxmapent->get_x() * 1e-4,
				       svxmapent->get_y() * 1e-4,
				       svxmapent->get_z() * 1e-3);
      seg->set3MomentumAtPrimaryVertex(svxmapent->get_px() * 1e-3,
				       svxmapent->get_py() * 1e-3,
				       svxmapent->get_pz() * 1e-3);
      seg->setLivePercentage(0,        svxmapent->get_livePercentage(0) * 1e-2);
      seg->setLivePercentage(1,        svxmapent->get_livePercentage(1) * 1e-2);
      seg->setLivePercentage(2,        svxmapent->get_livePercentage(2) * 1e-2);
      seg->setLivePercentage(3,        svxmapent->get_livePercentage(3) * 1e-2);
      seg->setSegmentQuality(          svxmapent->get_segmentQuality() * 1e-4);
      seg->setSegmentScore(            svxmapent->get_segmentScore() * 1e-2);
      seg->set_dEdX1(                  svxmapent->get_dEdX1() * 1e-1 );
      seg->set_dEdX2(                  svxmapent->get_dEdX2() * 1e-1 );

      // NOTE: This is bad, but better than not setting the info.
      //       This is used to reconstruct PrimVertex during recalibrator.
      seg->set3Momentum(svxmapent->get_px() * 1e-3,
			svxmapent->get_py() * 1e-3,
			svxmapent->get_pz() * 1e-3);

      // NOTE: Setting total hits --> hits/layer for all layers
      //       This is bad, but better than not setting the info.
      //       SvxSegment should be updated include a set/get method
      //       for total hits.
      for (int ilayer = 0; ilayer < 4; ilayer++)
	{
	  //seg->setNhits(ilayer,            svxmapent->get_nhits());
	  if(ilayer == 0) seg->setNhits(ilayer, svxmapent->get_nhits());
	  else          seg->setNhits(ilayer, 0);
	}

      // This variable is not yet available in svxmapent.
      //    seg->setClusterGoodFraction(ilayer, const int hit, const float frac);
      for(int ilay = 0; ilay < 4; ilay++)
	{
	  for(int ihit = 0; ihit < 2; ihit++)
	    {
	      seg->setClusterGoodFraction(ilay, ihit, svxmapent->get_clusterGoodFraction(ilay, ihit) * 1e-4 );
	    }
	}

      // Set the chisq & ndf. Must be calculated from quality and nhits
      // See SvxTracker::TrackFit() for definitions
      int ndf = (svxmapent->get_nhits() - 2)*2 - 1;

      
      float chisq  = calc_chisq_fromquality(seg->getSegmentQuality(),seg->getSegmentScore());//this returns the chisq/ndf


      seg->setChiSq(chisq*ndf);
      seg->setNDF(ndf);

      unsigned int nseg = seglist->get_nSegments();
      seglist->AddSegment(nseg, *seg);
      delete seg;
    }

  return 0;
}

int SvxCompactToDST::FillSvxCluster(PHCompositeNode* topNode)
{

  // Pointer to compact input
  SvxHitMap *svxmap  = getClass<SvxHitMap>(topNode, "SvxHit_comp");
  if (!svxmap)
    {
      cout << "SvxCompactToDST::FillSvxCluster(): no SvxHitMap" << endl;
      return 1;
    }

  // Pointer to DST output object
  PHString svxclslst = (m_test) ? "SvxClusterListTest" : "SvxClusterList";
  SvxClusterList *clslist = getClass<SvxClusterList>(topNode, svxclslst.getString());
  if (!clslist)
    {
      cout << "SvxCompactToDST::FillSvxCluster(): no SvxSelectedClusterList" << endl;
      return 1;
    }

  if(verbosity > 0) cout << "Ncluster at begining : " << clslist->get_nClusters() << endl;

  static const int NMAXERROR = 50;
  static       int nerror    = 0;

  int nclus = svxmap->GetNentries();
  for (int iclus = 0; iclus < nclus; iclus++)
    {
      const SvxHitMapEntry *svxmapent = svxmap->GetHit(iclus);

      float x = svxmapent->get_x() * 1e-3;
      float y = svxmapent->get_y() * 1e-3;
      float z = svxmapent->get_z() * 1e-3;
      int layer = fGeo->get_nearestLayer(sqrt(x * x + y * y));
      SvxSensor* sensor = fGeo->get_nearestSensor(x, y, z, layer);
      if(sensor==NULL){ 
        if(nerror<NMAXERROR){
          cerr<<"SvxCompactToDST: no sensor found. Probably wrong geometry is used. Skip this cluster. ";
          cerr<<"(x,y,z)=("<<x<<" "<<y<<" "<<z<<")"<<endl;
          //m_nBadCluster++;
        }
        nerror++;

        continue;
      }

      // look for the sensor where the cluster is placed
      double pos_global[3] = {x, y, z};
      double pos_local[3] = { -9999.};
      sensor->position_global2local(pos_global, pos_local);

      //--cout<<"cls : "<<x<<" "<<y<<" "<<z<<" : "<<pos_local[0]<<" "<<pos_local[1]<<" "<<pos_local[2]
      //--    <<" : "<<layer<<" "<<sensor->get_ladder()<<" "<<sensor->get_sensor()<<endl;

      SvxCluster* cls = clslist->addCluster();
      if (layer < 2)
	{
	  cls->set_size(svxmapent->get_adcandsize());
	}
      else
	{
	  int x_adc = svxmapent->get_adcandsize() & 0xFF;
	  int z_adc = (svxmapent->get_adcandsize() >> 8) & 0xFF;
	  cls->set_adc(0, x_adc);
	  cls->set_adc(1, z_adc);
	}
      cls->set_layer(sensor->get_layer());
      cls->set_ladder(sensor->get_ladder());
      cls->set_sensor(sensor->get_sensor());
      cls->set_xyz_global(0, x);
      cls->set_xyz_global(1, y);
      cls->set_xyz_global(2, z);
      cls->set_xyz_local(0, pos_local[0]);
      cls->set_xyz_local(1, pos_local[1]);
      cls->set_xyz_local(2, pos_local[2]);
      cls->set_hitID(svxmapent->get_id());
      cls->set_Nhot(svxmapent->get_nhot()) ;
      cls->set_Ncold(svxmapent->get_ncold()) ;

    }

  if(verbosity > 0) cout << "Ncluster at end : " << clslist->get_nClusters() << endl;

  return 0;
}

bool SvxCompactToDST::CompareSvxTrackList(PHCompositeNode *topNode)
{
  SvxSegmentList *segorglist = getClass<SvxSegmentList>(topNode, "SvxSegmentList");
  SvxSegmentList *segnewlist = getClass<SvxSegmentList>(topNode, "SvxSegmentListTest");
  if(segorglist == NULL)
    {
      cerr << PHWHERE << " Error no org SvxSegmentList" << endl;
      return false;
    }
  if(segnewlist == NULL)
    {
      cerr << PHWHERE << " Error no new SvxSegmentList" << endl;
      return false;
    }

  int n_org = segorglist->get_nSegments();
  int n_new = segnewlist->get_nSegments();
  bool result = (n_org == n_new);
  if(!result)
    {
      cout << "Inconsistent SvxCheckCompactCNT::compareSegmentList Nsegment : " << n_org << "!=" << n_new << endl;
    }
  else
    {

      for(int iseg = 0; iseg < n_org; iseg++)
	{
	  SvxSegment      *seg_org   = segorglist->get_segment(iseg);
	  SvxSegment      *seg_new   = segnewlist->get_segment(iseg);

	  result &= CompareSvxTrack(seg_org, seg_new);
	  if(!result)
	    {
	      cerr << PHWHERE << " inconsistent SegmentID : " << iseg << endl;
	      break;
	    }
	}
    }

  cout << "Checked SvxCompactToDST::" << __FUNCTION__ << " Nsegment : " << n_org << " " << flush;
  cout << (result ? "OK" : "FAILED") << endl;
  cout << endl;

  return result;
}

bool SvxCompactToDST::CompareSvxTrack(SvxSegment* sorg, SvxSegment* snew)
{
  if(sorg == NULL || snew == NULL)
    {
      cerr << "SvxCompactToDST::compareSvxTrack" << endl;
      return false;
    }

  bool result = true;

  result &= compareInt(sorg->IsPositive(),   snew->IsPositive(),        "IsPositive");
  //--result &= compareInt(sorg->getNhits(0),    snew->getNhits(0),         "NHits0");
  //--result &= compareInt(sorg->getNhits(1),    snew->getNhits(1),         "NHits1");
  //--result &= compareInt(sorg->getNhits(2),    snew->getNhits(2),         "NHits2");
  //--result &= compareInt(sorg->getNhits(3),    snew->getNhits(3),         "NHits3");

  result &= compareFloat(sorg->getQuality(), snew->getQuality(), 0.01,   "Quality");
  result &= compareFloat(sorg->getDCA2D(),   snew->getDCA2D(),  0.0001, "DCA2D");
  result &= compareFloat(sorg->getDCA(),     snew->getDCA(),    0.0001, "DCA");

  for(int i = 0; i < 3; i++)
    {
      float limit = (i < 2) ? 0.0001 : 0.001;
      result &= compareFloat(sorg->getClosestApproach(i), snew->getClosestApproach(i),
			     limit, TString::Format("ClosestApproach %d", i));
    }
  for(int i = 0; i < 3; i++)
    {
      result &= compareFloat(sorg->get3MomentumAtPrimaryVertex(i), snew->get3MomentumAtPrimaryVertex(i),
			     0.001, TString::Format("MomentumAtPrimaryVertex %d", i));
    }

  for(int i = 0; i < 3; i++)
    {
      if(sorg->getLivePercentage(i) < -9900) // uninit value
	{
	  result &= compareFloat(snew->getLivePercentage(i), -320.,
				 0.01, TString::Format("LivePercentage %d", i));
	}
      else
	{
	  result &= compareFloat(sorg->getLivePercentage(i), snew->getLivePercentage(i),
				 0.01, TString::Format("LivePercentage %d", i));
	}
    }
  result &= compareFloat(sorg->getSegmentQuality(), snew->getSegmentQuality(), 0.01, "SegmentQuality");
  result &= compareFloat(sorg->getSegmentScore(),   snew->getSegmentScore(),   0.01, "SegmentScore");

  result &= compareFloat(sorg->get_dEdX1(),   snew->get_dEdX1(),   0.1, "dEdX1");
  result &= compareFloat(sorg->get_dEdX2(),   snew->get_dEdX2(),   0.1, "dEdX2");
  for(int ilay = 0; ilay < 4; ilay++)
    {
      for(int ihit = 0; ihit < 2; ihit++)
	{
	  //if(sorg->getClusterGoodFraction(ilay, ihit)<-9900){ // uninit value
	  //  result &= compareFloat(snew->getClusterGoodFraction(ilay, ihit), -3.2,
	  //                         0.0001, TString::Format("ClusterGoodFrac %d %d", ilay, ihit));
	  //}
	  //else {
	  result &= compareFloat(sorg->getClusterGoodFraction(ilay, ihit), snew->getClusterGoodFraction(ilay, ihit),
				 0.0001, TString::Format("ClusterGoodFrac %d %d", ilay, ihit));
	  //}
	}
    }

  return result;
}

bool SvxCompactToDST::CompareSvxClusterList(PHCompositeNode *topNode)
{
  PHString svxclslst = (m_testSelClus) ? "SvxSelectedClusterList" : "SvxClusterList";
  SvxClusterList *clsorglist = getClass<SvxClusterList>(topNode, svxclslst.getString());
  SvxClusterList *clsnewlist = getClass<SvxClusterList>(topNode, "SvxClusterListTest");
  if(clsorglist == NULL)
    {
      cerr << PHWHERE << " Error no org " << svxclslst.getString() << endl;
      return false;
    }
  if(clsnewlist == NULL)
    {
      cerr << PHWHERE << " Error no new SvxClusterListTest" <<     endl;
      return false;
    }

  int n_org = clsorglist->get_nClusters();
  int n_new = clsnewlist->get_nClusters();

  bool result = (n_org == n_new);
  if(!result)
    {
      cerr << PHWHERE << " Inconsistent Nclusters : " << n_org << "!=" << n_new << endl;
    }
  else
    {

      for(int iclus = 0; iclus < n_org; iclus++)
	{
	  SvxCluster      *cls_org   = clsorglist->get_Cluster(iclus);
	  SvxCluster      *cls_new   = clsnewlist->get_Cluster(iclus);

	  result &= CompareSvxCluster(cls_org, cls_new);
	  if(!result)
	    {
	      cerr << PHWHERE << " Inconsistent ClusterID : " << iclus << endl;
	      break;
	    }
	}
    }

  cout << "Checked SvxCompactToDST::CompareClusterList Ncluster : " << n_org << " " << flush;
  cout << (result ? "OK" : "FAILED") << endl;
  cout << endl;

  return result;
}

bool SvxCompactToDST::CompareSvxCluster(SvxCluster* corg, SvxCluster* cnew)
{
  if(corg == NULL || cnew == NULL)
    {
      cerr << PHWHERE << endl;
      return false;
    }

  bool result = true;

  result &= compareInt(corg->get_hitID(), cnew->get_hitID(), "hitID");
  //  result &= compareInt(corg->get_svxSection(), cnew->get_svxSection(), "svxSection");
  result &= compareInt(corg->get_layer()     , cnew->get_layer()     , "layer");
  result &= compareInt(corg->get_ladder()    , cnew->get_ladder()    , "ladder");
  result &= compareInt(corg->get_sensor()    , cnew->get_sensor()    , "sensor");

  //  result &= compareInt(corg->get_sensorType(), cnew->get_sensorType(), "sensorType");
  //  result &= compareInt(corg->get_edgeflag  (), cnew->get_edgeflag  (), "edgeFlag");

  //  result &= compareInt(corg->get_ambiguous ()          , cnew->get_ambiguous(), "ambiguous");
  //  result &= compareInt(corg->get_AssociatedCGL()       , cnew->get_AssociatedCGL(), "AssociatedCLG");
  //  result &= compareInt(corg->get_AssociatedStandalone(), cnew->get_AssociatedStandalone(), "AssociatedCLG");
  //  result &= compareInt(corg->get_circumference()       , cnew->get_circumference(), "circumference");


  if(corg->get_layer() < 2) // size check for layer 0,1
    {
      result &= compareInt(corg->get_size()                , cnew->get_size(),      "size");
    }
  else   // adc check for layer 2, 3
    {
      for(int i = 0; i < 2; i++)
	{
	  float adc_org = corg->get_adc(i) < 256 ? corg->get_adc(i) : 255;
	  result &= compareInt(adc_org, cnew->get_adc     (i), "adc");
	  //  result &= compareInt(corg->get_xz_size (i), cnew->get_xz_size (i), "xz_size");
	}
    }
  for(int i = 0; i < 3; i++)
    {
      //    result &= compareFloat(corg->get_xyz_local (i), cnew->get_xyz_local (i), "XYZ_Local");
      result &= compareFloat(corg->get_xyz_global(i), cnew->get_xyz_global(i), 0.001, "XYZ_Global");
    }

  if(!result){
    cout<<"   failed info: "<<endl;
    cout<<"   -- org --"<<endl;
    cout<<"         "<<corg->get_layer()<<" "<<corg->get_ladder()<<" "<<corg->get_sensor()<<" : ";
    cout<<"("<<corg->get_xyz_global(0)<<" "<<corg->get_xyz_global(1)<<" "<<corg->get_xyz_global(2)<<"), ";
    cout<<"("<<corg->get_xyz_local(0)<<" "<<corg->get_xyz_local(1)<<" "<<corg->get_xyz_local(2)<<") ";
    cout<<" "<<corg->get_svxSection();
    cout<<endl;
    cout<<"   -- new --"<<endl;
    cout<<"         "<<cnew->get_layer()<<" "<<cnew->get_ladder()<<" "<<cnew->get_sensor()<<" : ";
    cout<<"("<<cnew->get_xyz_global(0)<<" "<<cnew->get_xyz_global(1)<<" "<<cnew->get_xyz_global(2)<<"), ";
    cout<<"("<<cnew->get_xyz_local(0)<<" "<<cnew->get_xyz_local(1)<<" "<<cnew->get_xyz_local(2)<<") ";
    cout<<endl;
  }

  return result;
}

bool SvxCompactToDST::CompareSvxCentralTrackList(PHCompositeNode *topNode, int flag)
{
  string snode_org = (flag==0 ? "SvxCentralTrackList" : "SvxCentralTrackBackList");
  SvxCentralTrackList *cntorglist = getClass<SvxCentralTrackList>(topNode, snode_org.c_str());
  if(cntorglist == NULL)
    {
      cerr << PHWHERE << " Error no org SvxCentralTrackList" << endl;
      return false;
    }

  string snode_new = (flag==0 ? "SvxCentralTrackListTest" : "SvxCentralTrackBackListTest");
  SvxCentralTrackList *cntnewlist = getClass<SvxCentralTrackList>(topNode, snode_new.c_str());
  if(cntnewlist == NULL)
    {
      cerr << PHWHERE << " Error no new SvxCentralTrackList" << endl;
      return false;
    }

  int n_org = cntorglist->get_nCentralTracks();
  int n_new = cntnewlist->get_nCentralTracks();
  bool result = (n_org == n_new);
  if(!result)
      {
          cout << PHWHERE << " " << __FUNCTION__ << " Inconsistent Ntrack : " << n_org << "!=" << n_new << endl;
      }
  else
      {

          for(int itrk = 0; itrk < n_org; itrk++)
              {
                  SvxCentralTrack *scnt_org = cntorglist->getCentralTrack(itrk);
                  SvxCentralTrack *scnt_new = cntnewlist->getCentralTrack(itrk);

                  result &= CompareSvxCentralTrack(scnt_org, scnt_new);
                  if(!result)
                      {
                          cout << PHWHERE << " " << __FUNCTION__ << " Inconsistent CentralTrackID : " << itrk << endl;
                          break;
                      }
              }
      }

  cout << PHWHERE << " " << __FUNCTION__ << (flag==0?"":"Back") << " Checked Ntrack : " << n_org << " " << flush;
  cout << (result ? "OK" : "FAILED") << endl;
  cout << endl;

  return result;
}

bool SvxCompactToDST::CompareSvxCentralTrack(SvxCentralTrack* strk_org, SvxCentralTrack* strk_new)
{
    if(strk_org == NULL || strk_new == NULL)
        {
            cerr << PHWHERE << endl;
            return false;
        }

    bool result = true;

    result &= compareInt(  strk_org->getDchIndex(),      strk_new->getDchIndex(),            "DchIndex");
    result &= compareInt(  strk_org->getUnique(),        strk_new->getUnique(),              "Unique");
    result &= compareFloat(strk_org->getDCA2D(),         strk_new->getDCA2D(),         1e-4, "DCA2D");
    result &= compareFloat(strk_org->getDCAZ(),          strk_new->getDCAZ(),          1e-4, "DCAZ");
    result &= compareFloat(strk_org->getDCA2Dprimary(),  strk_new->getDCA2Dprimary(),  1e-4, "DCA2Dprimary");
    result &= compareFloat(strk_org->getDCAZprimary(),   strk_new->getDCAZprimary(),   1e-4, "DCAZprimary");
    result &= compareFloat(strk_org->getChiSquareDPHI(), strk_new->getChiSquareDPHI(), 1e-2, "ChiSquareDPHI");
    result &= compareFloat(strk_org->getChiSquareDZ(),   strk_new->getChiSquareDZ(),   1e-2, "ChiSquareDZ");
    result &= compareFloat(strk_org->getChiSquare(),     strk_new->getChiSquare(),     1e-2, "ChiSquare");
    result &= compareFloat(strk_org->getChiSquare2(),    strk_new->getChiSquare2(),    1e-2, "ChiSquare2");

    for(int i = 0; i < 3; i++)
        {
            float limit = (i < 2) ? 0.0001 : 0.001;
            result &= compareFloat(strk_org->getClosestApproach(i), strk_new->getClosestApproach(i),
                                   limit, TString::Format("ClosestApproach %d", i));
        }

    for(int i = 0; i < 3; i++)
        {
            result &= compareFloat(strk_org->getLivePercentage(i), strk_new->getLivePercentage(i),
                                   1e-2, TString::Format("LivePercentage %d", i));
        }

    result &= compareFloat(strk_org->getLinkQuality(), strk_new->getLinkQuality(), 1e-4, "LinkQuality");
    result &= compareFloat(strk_org->getLinkScore(),   strk_new->getLinkScore(),   1e-2, "LinkScore");

    for(int i = 0; i < 3; i++)
        {
            result &= compareFloat(strk_org->get3MomentumAtPrimaryVertex(i),
                                   strk_new->get3MomentumAtPrimaryVertex(i),
                                   1e-3, TString::Format("Momentum %d", i));
        }

    // For scaling factors, check FillSvxHits.C

    int nhit_org = strk_org->getNhits();
    int nhit_new = strk_new->getNhits();
    result &= compareInt(nhit_org, nhit_new, "Nhits");

    for(int icls = 0; icls < nhit_org; icls++)
        {
            SvxClusterInfo *cls_org = strk_org->getClusterInfo(icls);
            SvxClusterInfo *cls_new = strk_new->getClusterInfo(icls);

            result &= compareInt(cls_org->getClusterId(), cls_new->getClusterId(),
                                 TString::Format("ClusterId %d", icls));

            result &= compareInt(cls_org->get_sublayer(), cls_new->get_sublayer(),
                                 TString::Format("sublayer %d", icls));

            result &= compareFloat(cls_org->getdphi(), cls_new->getdphi(),
                                   1e-4, TString::Format("dphi %d", icls));
            result &= compareFloat(cls_org->getdz(), cls_new->getdz(),
                                   1e-4, TString::Format("dz %d", icls));
        }

    return result;
}

bool SvxCompactToDST::compareInt(int orgval, int newval, const char* err)
{
    bool result = (orgval == newval);
    if(!result)
        {
            cout << "Failed : " << err << " " << orgval << "!=" << newval << endl;
        }
    if(m_print)
        {
            cout << err << "  " << orgval << "==" << newval << "  " << (result ? "OK" : "FAIL") << endl;
        }
    return result;
}

bool SvxCompactToDST::compareFloat(float orgval, float newval, float judge, const char* err)
{
    // NAN check
    if(!isfinite(orgval)) {
       cout<<"org is no-value : "<<orgval<<endl;
    }
    if(!isfinite(newval)) {
       cout<<"new is no-value : "<<newval<<endl;
    }

    /// check if value is consistent
    bool result = (fabs(orgval - newval) < judge);
    if(m_print)
        {
            cout << err << "  " << orgval << "==" << newval << " " << judge << "  " << (result ? "OK" : "FAIL") << endl;
        }

    if(result) return result;
    else   // if fail
        {
            // check overflow
            bool isoverflow = fabs(orgval) > 32000 * judge;
            if(isoverflow)
                {
                    bool of_result = ( fabs( fabs(newval) - (32000 * judge) ) < judge );
                    if(!of_result)
                        {
                            cout << "Failed overflow : " << err << " " << orgval << "!=" << newval << " " << 32000 * judge << endl;
                        }
                    if(m_print)
                        {
                            cout << "overflow " << err << "  " << orgval << "==" << newval << " " << judge << "  " << (of_result ? "OK" : "FAIL") << endl;
                        }
                    return of_result;
                }

            // this is fail & not overflow -> really fail
            cout << "Failed : " << err << " " << orgval << "!=" << newval << endl;
        }
    return false;
}

float calc_chisq_fromquality(float quality, float score)
{
  static int nerr_chisq = 0;

  float chisq = quality - score/100.;
  chisq = 1/chisq-2.0;
  if (chisq < 0)
    {
      if(nerr_chisq<100){
        cout << "WARNING!! calc_chisq_fromquality(" << quality << "," << score <<") gives chisq/ndf=" << chisq << endl;
      } else if(nerr_chisq==100){
        cout << "WARNING!! calc_chisq_fromquality error too much. Stop showing this error" << endl;
      }
      nerr_chisq++;

      return -1;
    }
  return chisq;
}
