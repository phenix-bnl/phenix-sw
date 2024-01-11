#include <SvxStandAloneRecoLowMult.h>
#include <svxDetectorGeo.hh>
#include <SvxCluster.h>
#include <SvxClusterContainer.h>
#include <PHPoint.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <SvxSegmentv6.h>
#include <SvxSegmentListv6.h>
#include <BbcOut.h>
#include <Bbc.hh>
#include <VtxOut.h>
#include <RunHeader.h>
#include <PHCompositeNode.h>
#include <PHNodeIterator.h>
#include <PHTypedNodeIterator.h>
#include <PHIODataNode.h>
#include <getClass.h>
#include <SvxComponentGeom.h>
#include <SvxPixelHotDeadMap.h>
#include <SvxPixelHotDeadMapv2.h>
#include <SvxDeadMap.h>
#include <svxAddress.hh>
#include <recoConsts.h>


#include <iostream>
#include <algorithm>
#include <stdlib.h>


using namespace std;

int SvxStandAloneRecoLowMult::segmentID = 0;

SvxStandAloneRecoLowMult::SvxStandAloneRecoLowMult(const string& name):
  SubsysReco(name),
  d_cluster(0),
  d_segment(0),
  d_vertexrecoflag(2),
  d_svxgeometry(0),
  d_vtxclusters(0),
  _timer(PHTimeServer::get()->insert_new(name)),
  d_Bpolarity(true),
  d_use_SvxVtxOut(false),
  d_zerofieldflag(false),
  d_svxCompGeom(0),
  d_pixelMap(0),
  d_stripMap(0),
  d_address(0),
  d_minhit(4),
  d_recomode(true)
  /*
    SvxClusterList *d_cluster;
    SvxSegmentList *d_segment;
    float d_vertex[3];    //primary vertex
    int d_vertexrecoflag;
    float d_windowscale;
    int d_reco_method; //select reconstruction method
    double Rsub[svxDetectorGeo::SVXNSUBLAYER];
    svxDetectorGeo *d_svxgeometry;
    SvxClusterContainer *d_vtxclusters;
    SvxTracker d_tracker;
    PHTimeServer::timer _timer;   ///< Timer
    bool d_Bpolarity;
    bool d_use_SvxVtxOut;
    bool d_use_vtx_projection;
    bool d_swap[svxDetectorGeo::SVXNSUBLAYER];
    bool d_swapmode;
    bool d_ppflag;
    bool d_requireB1hit;
    bool d_use_vtx_projection_default;
    bool d_requireB1hit_default;
    int d_bbcqcut;
    bool d_zerofieldflag;
    SvxComponentGeom* d_svxCompGeom;
    SvxPixelHotDeadMap* d_pixelMap;
    SvxDeadMap* d_stripMap;
    svxAddress* d_address;
    float DPHI0;  //phi acceptance for second hit
    float DPHI1;  //phi acceptance for third hit
    float DPHI2;  //phi acceptance for after fourth hit
    float DZ0;    //Z acceptance for second hit
    float DZ1;    //Z acceptance fot third hit
    float DZ2;    //Z acceptance for after fourth hit
    int d_minhit; //minimum number of hits associated to link
    bool d_recomode;
   */
{

  for ( int i = 0; i < svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    Rsub[i] = 0.0;
  }


  d_vertex[0] = 0.0;
  d_vertex[1] = 0.0;
  d_vertex[2] = 0.0;

  srand( time(NULL) );
}

SvxStandAloneRecoLowMult::~SvxStandAloneRecoLowMult()
{}

int SvxStandAloneRecoLowMult::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  /// Search SvxVtxOut node.
  /// If you can find it, use it. Otherwise, use VtxOut node.
  PHIODataNode<PHObject>* VtxOutNode = NULL;
  VtxOutNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxVtxOut");
  if (VtxOutNode) {
    d_use_SvxVtxOut = true;
  } else {
    d_use_SvxVtxOut = false;
  }

  d_svxgeometry = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if ( d_svxgeometry == NULL) {
    if (verbosity > 0) { cout << PHWHERE << "Can't find svxDetector. " << endl; }
    return EVENT_OK;
  }

  // check magnet current
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader == NULL) {
    cout << PHWHERE << "Can't find runheader. " << endl;
    return EVENT_OK;
  }
  if (runheader->get_currentCentral() > 0) {
    d_Bpolarity = true;
  } else {
    d_Bpolarity = false;
  }

  d_tracker.set_DetectorGeo(d_svxgeometry);
  for ( int isub = 0; isub < svxDetectorGeo::SVXNSUBLAYER; isub++ ) {
    Rsub[isub] = d_svxgeometry->get_Rsublayer(isub);
  }

  if (d_zerofieldflag) {
    d_tracker.set_ZeroFieldFlag(true);
  }


  // Build model of active volumes for track pointing / deadmap integration
  if (!d_svxCompGeom)
    d_svxCompGeom = new SvxComponentGeom(d_svxgeometry);

  d_svxCompGeom->SetVerbosity(0);

  // Get deadmap information from SvxPixelHotDeadMap and SvxDeadMap if they
  // exist on the node tree
  bool foundPixelMap = false;
  d_pixelMap = findNode::getClass<SvxPixelHotDeadMapv2>(topNode, "SvxPixelHotDeadMapv2");
  if (!d_pixelMap) {
    cout << PHWHERE << "No SvxPixelHotDeadMapv2 found on the node tree. "
         << "SvxComponentGeom will get pixel deadmap information from the database based on the runnumber."
         << endl;
  }
  else {
    foundPixelMap = true;
  }

  bool foundStripMap = false;
  d_stripMap = findNode::getClass<SvxDeadMap>(topNode, "SvxStripHotDeadMap");
  if (!d_stripMap) {
    cout << PHWHERE << "No SvxDeadMap found on the node tree. "
         << "SvxComponentGeom will get strip deadmap information from the database based on the runnumber."
         << endl;
  }
  else {
    foundStripMap = true;
  }


  // Read deadmap from database using run number from DST if it exists.
  recoConsts *rc =  recoConsts::instance();


  if (rc->FlagExist("RUNNUMBER")) {
    int runnumber = rc->get_IntFlag("RUNNUMBER");
    d_address = findNode::getClass<svxAddress>(topNode, "svxAddress");
    if (!d_address) {
      cout << PHWHERE
           << "No svxAddress object found on node tree. "
           << "New instance will be created in SvxComponentGeom::AssignMap()."
           << endl;
    }
    if (foundPixelMap && foundStripMap && d_address) {
      cout << PHWHERE << "Found pixel & strip deadmaps on the node tree, initializing SvxComponentGeom" << endl;
      d_svxCompGeom->AssignMap(d_pixelMap, d_stripMap, d_address);
    }
    else {
      d_svxCompGeom->AssignMap(runnumber, d_address);
    }
  }
  else {
    cout << PHWHERE << "Can't find RUNNUMBER flag in recoConsts" << endl;
  }


  return 0;
}


int SvxStandAloneRecoLowMult::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  d_cluster = 0;
  segmentID = 0;

  //
  // get PHOOL nodes
  // d_cluser: SVX hit clusters (input. 3D hit points)
  // d_segement: recontructed SVX track segement
  // vtxout : event vertex
  //

  if ( verbosity > 0 ) {
    std::cout << "SvxStandAloneRecoLowMult::process_event() is called";
    std::cout << " d_vertexrecoflag = " << d_vertexrecoflag;
  }

  d_vtxclusters = findNode::getClass<SvxClusterContainer>(topNode, "SvxClusterContainer");
  if ( d_vtxclusters == NULL) {
    if (verbosity > 0) { cout << PHWHERE << "Can't find SvxClusterContainer. " << endl; }
    return EVENT_OK;
  }

  d_segment = 0;
  PHTypedNodeIterator<SvxSegmentList> segmentiter(topNode);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  if (!SvxSegmentListNode) {
    if (verbosity > 0) { cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl; }
    return EVENT_OK;
  } else {
    d_segment = (SvxSegmentList*)SvxSegmentListNode->getData();
  }

  VtxOut *vtxout = 0;
  PHTypedNodeIterator<VtxOut> vtxoutiter(topNode);
  PHIODataNode<VtxOut> *VtxOutNode;
  if (!d_use_SvxVtxOut) {
    /// get primary vertex information from VtxOut.
    VtxOutNode = vtxoutiter.find("VtxOut");
  } else {
    /// get primary vertex information from SvxVtxOut.
    VtxOutNode = vtxoutiter.find("SvxVtxOut");
  }
  if (!VtxOutNode) {
    cerr << PHWHERE << " ERROR: Can't find VtxOut." << endl;
    if ( d_vertexrecoflag == 0 || d_vertexrecoflag == 2 ) { return EVENT_OK; }
  } else {
    vtxout = (VtxOut*)VtxOutNode->getData();
  }

  // d_vertexrecoflag
  // 0: use beam center
  // 1:
  // 2: use latest vertex

  if (d_vertexrecoflag == 0) { /// force to use beam center
    PHPoint verpoint = vtxout->get_Vertex("SVX");
    d_vertex[0] = verpoint.getX();
    d_vertex[1] = verpoint.getY();
    d_vertex[2] = verpoint.getZ();
  } else if (d_vertexrecoflag == 2) {
    PHPoint verpoint = vtxout->get_Vertex();
    d_vertex[0] = verpoint.getX();
    d_vertex[1] = verpoint.getY();
    d_vertex[2] = verpoint.getZ();
  } else {
    /// get beam center position from cluster container
    float beam_x;
    float beam_y;
    d_vtxclusters->get_beam_center(beam_x, beam_y);

    d_vertex[0] = beam_x;
    d_vertex[1] = beam_y;
    d_vertex[2] = 0.;
  }

  if ( verbosity > 0 ) {
    std::cout << " d_vertex[]=(" << d_vertex[0] << "," << d_vertex[1] << "," << d_vertex[2] << ")" << std::endl;
  }


  // run the pattern recognition
  run_patternRecognition();

  // run the track fitting
  run_linkFitting();


  _timer.get()->stop();

  return 0;
}


/******************************************************
* From here, code for link method
********************************************************/

// static void print(SvxLinkLM * link) {
//   int n = link->get_size();
//   for (int i = 0; i < n; i++) {
//     cout << i << ": " << link->nodelist[i]->sublayer
//          << "| " <<
//          (link->nodelist[i]->cluster == NULL ? -1
//           : link->nodelist[i]->cluster->get_hitID())
//          << " "
//          << "| (" << link->nodelist[i]->x
//          << ", " << link->nodelist[i]->y
//          << ", " << link->nodelist[i]->z
//          << ") " << endl;
//   }
//   cout << "++++++++++++++++++++++++++" << endl;
// }

/********************************
 * Helper class for link method
 *********************************/

struct TRACK_FITTER_LM {
  //
  // Interface to SvxTracker and d_segment
  //
  TRACK_FITTER_LM():
    d_segmentlist(0),
    d_cut(-0.375),
    dv_vertex(3, 0.0),
    d_use_vtx_projection(false),
    d_Bpolarity(false),
    d_recomode(false),
    d_zerofieldflag(false),
    d_svxCompGeom(0)
  {}
  void operator()(SvxLinkLM *link);

  SvxTracker      d_tracker;     // object for track fitting
  SvxSegmentList *d_segmentlist; // pointer to PHOOL node for SvxSegmentList
  float           d_cut;         // track quality cut parameter
  vector<float>   dv_vertex;     // vertex position (vx,vy,vz)
  bool            d_use_vtx_projection;
  bool            d_Bpolarity;   // polarity of magnetic field
  bool            d_recomode;
  bool            d_zerofieldflag;
  SvxComponentGeom* d_svxCompGeom;

  //  void set_DetectorGeo(svxDetectorGeo *svxgeo) { d_tracker.set_DetectorGeo(svxgeo); }
  void set_tracker(const SvxTracker &tracker) { d_tracker = tracker; }
  void set_ProjectionFlag(bool flag) { d_use_vtx_projection = flag; }
  void set_Bpolarity(bool polarity) { d_Bpolarity = polarity; }
  void set_recomode(bool mode) { d_recomode = mode; }
  void set_zerofieldflag(bool mode) { d_zerofieldflag = mode; }
  void set_compGeom(SvxComponentGeom* geom) { d_svxCompGeom = geom; }

  float getLinkScore(ScgTrack &geoTrack, int* Nhits_layer);
  float getLinkQuality(float linkScore, float chisq, float ndf);

};


void TRACK_FITTER_LM::operator()(SvxLinkLM * link) {
  //
  // Perform track fitting
  //
  // input:
  //   link   link of clusters found in VTX.
  //   Note that number of hits in a link is 3 or 4 or 5.
  //
  int n = link->get_size();
  vector<SvxCluster*> vcluster;
  for ( int i = 0; i < n; i++ ) {
    SvxCluster *tmp_cls = link->nodelist[i]->cluster;
    vcluster.push_back(tmp_cls);
  }

  // rough guess of pt
  double pt = link->Rrot * 0.003 * 0.9;
  // if (pt < 0.02 && (!d_zerofieldflag)) return; // pt<20MeV should curl up in the VTX. stop processing



  bool helicity;   /// true : left-handed; false : right-handed
  if ( (link->cy - link->posy[1]) * (link->posx[n - 1] - link->posx[1])
       - (link->cx - link->posx[1]) * (link->posy[n - 1] - link->posy[1]) > 0. ) {
    helicity = true;
  } else {
    helicity = false;
  }
  // track fitting. Note that track fitting can handle
  // 3, 4, and 5 hit tracks.
  // fill these variables too
  float chisq = 0.0;
  int ndf = 0;
  float tprob = d_tracker.TrackFit(vcluster, helicity, pt, chisq, ndf);

  SvxSegment *segment = new SvxSegmentv6();
  segment->Reset();

  // 3-momentum (px,py,pz) of the track
  float px = d_tracker.get3Mom(0);
  float py = d_tracker.get3Mom(1);
  float pz = d_tracker.get3Mom(2);

  // position (x0,y0,z0) at layer 0
  float x0;
  float y0;
  float z0;
  d_tracker.getExpectedPosition(0, x0, y0, z0, 0);

  //
  // calculate DCA
  // Note: dv_vertex contains (x,y,z) of the primary vertex
  //
  vector<float> pos_primary(3);
  vector<float> mom_primary(3);
  float dca2d;
  d_tracker.calc_InfoAtClosestApproach(helicity,
                                       x0, y0, z0,
                                       px, py, pz,
                                       dv_vertex,
                                       pos_primary, mom_primary, dca2d);
  float px0 = mom_primary[0];
  float py0 = mom_primary[1];
  float pz0 = mom_primary[2];
  float dca3d = sqrt( (dv_vertex[0] - pos_primary[0]) * (dv_vertex[0] - pos_primary[0])
                      + (dv_vertex[1] - pos_primary[1]) * (dv_vertex[1] - pos_primary[1])
                      + (dv_vertex[2] - pos_primary[2]) * (dv_vertex[2] - pos_primary[2]) );
  if ( dca2d < 0 ) { dca3d *= -1.; } /// if dca2d is negative, dca3d is also negative.

  /// 0.05 is a temporary value, so it should be modified.
  bool is_primary = false;
  if ( dca3d < 0.05 ) is_primary = true;

  //****************************************
  // Now store the results in d_segmentlist
  //****************************************
  //
  // set Nhits, ClusterID, Scatter and ProjectedPosition.
  //
  // Note that pos3d[] contains the projected track position
  // after tracking while cluster->get_xyz_global() gives
  // center of the clusters. They can be different. we
  // store the projected hit position in segment.
  //

  int Nhits_layer[4] = {0, 0, 0, 0};
  float dEdx1 = 0;
  float dEdx2 = 0;

  for (int i = 0; i < n; i++) {
    SvxCluster *cluster = link->nodelist[i]->cluster;
    int layer = cluster->get_layer();
    int clusterID = cluster->get_hitID();
    if (layer >= 0 && layer < 4) {
      segment->setClusterID(layer, Nhits_layer[layer], clusterID);

      float clusGoodFrac = 1;
      int nbad = 0;
      nbad += (cluster->get_Ncold() > 0) ? cluster->get_Ncold() : 0;
      nbad += (cluster->get_Nhot() > 0) ? cluster->get_Nhot() : 0;
      int size = cluster->get_size();
      clusGoodFrac = (float)(size - nbad) / (float)size;
      segment->setClusterGoodFraction(layer, Nhits_layer[layer], clusGoodFrac);

      if (layer == 0) {
        segment->setScatter(layer, 0.0);
      } else if (layer < 3) {
        segment->setScatter(layer, d_tracker.getScatter(layer - 1));
      }
      if (layer == 2) dEdx1 += (cluster->get_adc(0) + cluster->get_adc(1));
      if (layer == 3) dEdx2 += (cluster->get_adc(0) + cluster->get_adc(1));

      /// set expected hit position
      if ( Nhits_layer[layer] < 2 ) {
        float pos[3];
        d_tracker.getExpectedPosition(layer, pos[0], pos[1], pos[2], Nhits_layer[layer]);
        segment->setProjectedPosition(layer, pos[0], pos[1], pos[2], Nhits_layer[layer]);
        Nhits_layer[layer]++;
      }
    }
  }
  for (int layer = 0; layer < 4; layer++) {
    segment->setNhits(layer, Nhits_layer[layer]);
  }

  segment->setSegmentID(SvxStandAloneRecoLowMult::segmentID);
  SvxStandAloneRecoLowMult::segmentID++;

  segment->set3MomentumAtPrimaryVertex(px0, py0, pz0);
  segment->set3Momentum(px, py, pz);
  segment->setIsPositive(1 ^ (helicity ^ d_Bpolarity));
  segment->setPrimary(is_primary);
  segment->setQuality(tprob);
  segment->setChiSq(chisq);
  segment->setNDF(ndf);
  segment->setDCA(dca3d);
  segment->setDCA2D(dca2d);
  segment->setInnerMostProjectedPosition(0, x0);
  segment->setInnerMostProjectedPosition(1, y0);
  segment->setInnerMostProjectedPosition(2, z0);
  segment->setClosestApproach(pos_primary[0],
                              pos_primary[1],
                              pos_primary[2]);
  segment->set_dEdX1(dEdx1);
  segment->set_dEdX2(dEdx2);
  segment->set_recomode(d_recomode);
  //    segment->setDchIndex(int ind);


  //include information from the SvxComponentGeom
  float linkScore = -9999.;
  float linkQuality = -9999.;
  float livePerc[4];
  for (int i = 0; i < 4; i++)
    livePerc[i] = -9999.;


  if (d_svxCompGeom)
  {
    float mom = sqrt(px0 * px0 + py0 * py0 + pz0 * pz0);
    float pt = sqrt(px0 * px0 + py0 * py0);
    float phi0 = atan2(py0, px0);
    float the0 = acos(pz0 / pt);
    float charge = 1;
    if (helicity != d_Bpolarity)
      charge = -1;
    float B = 0.90; //field strength
    float fieldScale = 1.;
    if (!d_Bpolarity)
      fieldScale = -1;
    ScgTrack geoTrack = d_svxCompGeom->FindHitsFromVertex(dv_vertex.at(0), dv_vertex.at(1), dv_vertex.at(2),
                        mom, phi0, the0,
                        charge, fieldScale * B);

    //get the link score
    linkScore = getLinkScore(geoTrack, Nhits_layer);

    //get the link quality
    linkQuality = getLinkQuality(linkScore, chisq, ndf);


    //get the live fraction in each layer
    //if we find a cluster, it's based off
    //the cluster position, else it's based
    //off the geoTrack
    for (int i = 0; i < 4; i++)
    {
      if (Nhits_layer[i] > 0)
      {
        //get the live fraction for the found cluster.
        //in the strip layers, find the live fraction of the
        //sublayer with the largest live fraction
        for (int iclus = 0; iclus < link->get_size(); iclus++)
        {
          SvxCluster *cluster = link->nodelist[iclus]->cluster;
          if (cluster->get_layer() == i)
          {
            //get the layer, ladder, sensor, comp indeces
            int layer = cluster->get_layer();
            int ladder = cluster->get_ladder();
            int sensor = cluster->get_sensor();

            //get the local coordinates
            float lx = cluster->get_xyz_local(0);
            float ly = cluster->get_xyz_local(1);
            float lz = cluster->get_xyz_local(2);

            //get the component (chip or half-module) index
            int comp = d_svxCompGeom->GetComponent(layer, ladder, sensor, lx, ly, lz);

            //get the tile index
            int tile = d_svxCompGeom->GetTile(cluster->get_layer(), cluster->get_ladder(), cluster->get_sensor(), lx, ly, lz);

            //get the tile live fraction
            float liveperc = d_svxCompGeom->GetTileGoodFrac(layer, ladder, sensor, comp, tile) * 100.;

            //get the highest live fraction
            if (liveperc > livePerc[i])
              livePerc[i] = liveperc;

          }
        }
      }
      else
      {
        for (int ihit = 0; ihit < geoTrack.nhits; ihit++)
        {
          ScgHit hit = geoTrack.GetHit(ihit);

          if (hit.layer == i && hit.livefrac * 100. > livePerc[i])
            livePerc[i] = hit.livefrac * 100.;
        }
      }
    }
  }

  segment->setSegmentScore(linkScore);
  segment->setSegmentQuality(linkQuality);
  for (int i = 0; i < 4; i++)
    segment->setLivePercentage(i, livePerc[i]);

  ((SvxSegmentListv6*)d_segmentlist)->AddSegment((unsigned int)(d_segmentlist->get_nSegments()), *segment);

  delete segment;

}

float TRACK_FITTER_LM::getLinkScore(ScgTrack & geoTrack, int* Nhits_layer)
{
  // Accounting variables
  int nclus = 0, npixl = 0;

  //the fraction of bad pixels in the tile for a
  vector<float> badFrac_missingLayers;
  vector<int> missingLayers;

  //find the number of clusters in this link
  for (int i = 0; i < 4; ++i) {
    if (Nhits_layer[i] > 0) { //cluster found in this layer
      nclus++;
      if (i < 2)
        npixl++;
    }
    else { //no cluster, check for geohit
      missingLayers.push_back(i); //record which layer is missing

      bool foundHit = false;
      for (int ihit = 0; ihit < geoTrack.nhits; ihit++) {
        ScgHit geoHit = geoTrack.GetHit(ihit);
        if (geoHit.layer == i) {
          badFrac_missingLayers.push_back(1 - geoHit.livefrac);
          foundHit = true;
        }
      }
      if (!foundHit) { //did not find a geohit
        badFrac_missingLayers.push_back(1);
      }
    }

  }


  float linkScore = -9999.;

  //calculate the link score
  switch (nclus) {

  case 4:
    // Best possible score.
    linkScore = 100.;

    break;

  case 3:
    // Favor links including both pixel layers
    linkScore = (npixl == 2) ? 70. : 60.;

    // Favor links with projected hits in a bad area or missed hit
    //linkScore += (linkval & 1 << (missingLayers.at(0)+8)) ? 20 : 0;
    linkScore += badFrac_missingLayers.at(0) * 20.;

    // Favor outermost missing layers
    linkScore += missingLayers.at(0);

    break;

  case 2:
    // Favor links including both pixel layers
    //    linkScore = (npixl==2) ? 20 : 15;
    linkScore = 10.*npixl;

    // Favor links with projected hits in bad areas
    linkScore += badFrac_missingLayers.at(0) * 5.;
    linkScore += badFrac_missingLayers.at(1) * 5.;

    // Favor outermost missing layers
    linkScore += missingLayers.at(0);
    linkScore += missingLayers.at(1);

    break;

  default:
    cout << PHWHERE << nclus
         << " clusters found. Should be 2,3,4 only." << endl;
  }

  return linkScore;

}
float TRACK_FITTER_LM::getLinkQuality(float linkScore, float chisq, float ndf)
{
  //check ndf for nan and < 0
  if (ndf < 0 || ndf != ndf)
    return -9999.;
  else
    return 1.0 / (chisq / ndf + 2) + linkScore / 100.;

}

/**
 * Simple pattern recognition:
 *   Make all combinations of 4 clusters in each arm
 *
 * returns the number of compinations ("links")
 *
 */
int SvxStandAloneRecoLowMult::run_patternRecognition()
{

  // clear the data from previous event
  d_linklist.clear();
  d_goodlinklist.clear();

  // Select all clusters in B0
  int sublayer = 0;
  vector<SvxCluster*> vclust;
  int nclust = d_vtxclusters->find_clusters(vclust, sublayer);


  // Make all combinations between the 4 layers
  for (int i = 0; i < nclust; i++)
  {
    float x0 = vclust.at(i)->get_xyz_global(0);
    float y0 = vclust.at(i)->get_xyz_global(1);
    float z0 = vclust.at(i)->get_xyz_global(2);

    SvxLinkNodeLM *node = new SvxLinkNodeLM(vclust.at(i), x0, y0, z0, 0);
    SvxLinkLM *link = new SvxLinkLM(node);
    d_linklist.push_back(link);

    // Run recurssive pattern recognition
    LinkClusters(sublayer + 1, link, x0, 0, -1);
  }// i (lyr = 0)

  // vector<SvxCluster*> vclust_B0;
  // vector<SvxCluster*> vclust_B1;
  // vector<SvxCluster*> vclust_B2;
  // vector<SvxCluster*> vclust_B3;
  // vector<int> nclust_sub;

  // int nclust_B0 = d_vtxclusters->find_clusters_layer(vclust_B0, nclust_sub,
  //                 0,
  //                 0, 2 * M_PI,
  //                 0, 20.0);
  // int nclust_B1 = d_vtxclusters->find_clusters_layer(vclust_B1, nclust_sub,
  //                 1,
  //                 0, 2 * M_PI,
  //                 0, 20.0);
  // int nclust_B2 = d_vtxclusters->find_clusters_layer(vclust_B2, nclust_sub,
  //                 2,
  //                 0, 2 * M_PI,
  //                 0, 20.0);
  // int nclust_B3 = d_vtxclusters->find_clusters_layer(vclust_B3, nclust_sub,
  //                 3,
  //                 0, 2 * M_PI,
  //                 0, 20.0);

  // if (verbosity > 0)
  // {
  //   cout << "************* Clusters found **********" << endl;
  //   cout << "    B0: " << nclust_B0 << endl;
  //   cout << "    B1: " << nclust_B1 << endl;
  //   cout << "    B2: " << nclust_B2 << endl;
  //   cout << "    B3: " << nclust_B3 << endl;
  //   cout << "***************************************" << endl;
  // }

  // for (int i = 0; i < nclust_B0; i++)
  // {
  //   float x0 = vclust_B0.at(i)->get_xyz_global(0);
  //   float y0 = vclust_B0.at(i)->get_xyz_global(1);
  //   float z0 = vclust_B0.at(i)->get_xyz_global(2);

  //   SvxLinkNodeLM *node = new SvxLinkNodeLM(vclust_B0.at(i), x0, y0, z0, 0);

  //   for (int j = 0; j < nclust_B1; j++)
  //   {
  //     float x1 = vclust_B1.at(j)->get_xyz_global(0);
  //     float y1 = vclust_B1.at(j)->get_xyz_global(1);
  //     float z1 = vclust_B1.at(j)->get_xyz_global(2);

  //     if (x1 * x0 < 0)
  //       continue;

  //     SvxLinkNodeLM *node_B1 = new SvxLinkNodeLM(vclust_B1.at(j),
  //         x1, y1, z1, 1);

  //     for (int k = 0; k < nclust_B2; k++)
  //     {
  //       float x2 = vclust_B2.at(k)->get_xyz_global(0);
  //       float y2 = vclust_B2.at(k)->get_xyz_global(1);
  //       float z2 = vclust_B2.at(k)->get_xyz_global(2);

  //       if (x2 * x0 < 0)
  //         continue;

  //       SvxLinkNodeLM *node_B2 = new SvxLinkNodeLM(vclust_B2.at(k),
  //           x2, y2, z2, 2);

  //       for (int m = 0; m < nclust_B3; m++)
  //       {
  //         float x3 = vclust_B3.at(m)->get_xyz_global(0);
  //         float y3 = vclust_B3.at(m)->get_xyz_global(1);
  //         float z3 = vclust_B3.at(m)->get_xyz_global(2);

  //         if (x3 * x0 < 0)
  //           continue;

  //         SvxLinkNodeLM *node_B3 = new SvxLinkNodeLM(vclust_B3.at(m),
  //             x3, y3, z3, 3);

  //         SvxLinkLM *link = new SvxLinkLM(node);
  //         link->add_node(node_B1);
  //         link->add_node(node_B2);
  //         link->add_node(node_B3);
  //         d_goodlinklist.push_back(link);

  //       } //k (B2)
  //     } //k (B2)
  //   } //j (B1)

  // } //i (B0)

  if (verbosity > 0) {
    // print the links generated
    cout << "************* LINKS found *********** " << d_goodlinklist.size() << endl;
    // for_each(d_goodlinklist.begin(), d_goodlinklist.end(), print);
  }

  return d_goodlinklist.size();
}

/**
 * Recursive algorithm to link all clusters in a given arm
 */
void SvxStandAloneRecoLowMult::LinkClusters(int sublayer, SvxLinkLM *link,
    float x0, float z0, float dz)
{
  // find clusters in layer
  vector<SvxCluster*> vclust;
  // give extreme values for dphi and dz to get all clusters
  int nclust = 0;
  if (dz < 0)
  {
    nclust = d_vtxclusters->find_clusters(vclust, sublayer);
  }
  else
  {
    nclust = d_vtxclusters->find_clusters(vclust, sublayer,
                                          0, 2 * M_PI,
                                          z0, dz);
  }

  //for use in calc_projection
  float zproj, new_dz;

  for (int i = 0; i < nclust; i++) {
    float x = vclust.at(i)->get_xyz_global(0);
    float y = vclust.at(i)->get_xyz_global(1);
    float z = vclust.at(i)->get_xyz_global(2);

    // check that hits are in the same arm
    if (x0 * x >= 0)
    {
      //
      // acceptable hit is found.
      // Make a new branch of tree to continue this link direction
      // Note that you must make new_link here. Otherwise, you have a problem
      // if there is more than one acceptable hit
      //
      SvxLinkNodeLM *new_node = new SvxLinkNodeLM(vclust[i], x, y, z, sublayer);
      SvxLinkLM *new_link = new SvxLinkLM(link);
      d_linklist.push_back(new_link);
      new_link->add_node(new_node);

      // If reached the outer layer, stop the search.
      if (sublayer >= SVXMAXSUBLAYER) {
        if (new_link->get_size() >= d_minhit) {
          quickCircleCalculator(new_link);
          d_goodlinklist.push_back(new_link);
        }
      } else {
        // Otherwise search the next layer
        calc_projection(new_link, Rsub[sublayer + 1], zproj, new_dz);
        LinkClusters(sublayer + 1, new_link, x0, zproj, new_dz);
      }
    }
  } // i

  // If reached the outer layer, stop the search.
  if (sublayer >= SVXMAXSUBLAYER) {
    if (link->get_size() >= d_minhit) {
      quickCircleCalculator(link);
      d_goodlinklist.push_back(link);
    }
    return;
  }


  //Otherwise, search the next sublayer
  calc_projection(link, Rsub[sublayer + 1], zproj, new_dz);
  LinkClusters(sublayer + 1, link, x0, zproj, new_dz);

}

/**
 * Calculate the projected position
 */
bool SvxStandAloneRecoLowMult::calc_projection(SvxLinkLM *link, float R,
    float &z_projection, float &dz)
{

  int Nhit = link->get_size();

  if (Nhit <= 1)
  {
    // If there is only 1 hit so far (B0), then we want to search the
    // entire next layer, so return a negative value for dz
    z_projection = 0;
    dz = -1;
    return true;
  }


  // Using the inner two hits (should be pixel)
  // form a straight line in 3D and find
  // the z intersection point with a cylinder
  // at the nominal radius of B2 & B3
  // Compare to the actual cluster positions
  //
  // r0 = (x0, y0, z0) <- B0 cluster
  // r1 = (x1, y1, z1) <- B1 cluster
  // R <- nominal radius of B2/B3
  //
  // line in 3D:
  //   r = r0 + vt
  //   v = r1 - r0 = (a, b, c)
  //
  // parametric form:
  //   x = x0 + lt
  //   y = y0 + mt
  //   z = z0 + nt
  //
  // clylinder: x^2 + y^2 = R^2
  //
  // R^2 = (x0 + lt)^2 + (y0 + mt)^2
  // 0 = (x0^2 + y0^2 - R^2) + (2x0l + 2y0m)t + (l^2 + m^2)t^2
  // 0 = c + bt + at^2
  // t = [-b + sqrt(b^2 - 4ac)] / 2a
  // zproj = z0 + nt

  float x0 = link->posx[0];
  float y0 = link->posy[0];
  float z0 = link->posz[0];

  float x1 = link->posx[1];
  float y1 = link->posy[1];
  float z1 = link->posz[1];

  float l = x1 - x0;
  float m = y1 - y0;
  float n = z1 - z0;

  float a = l * l + m * m;
  float b = 2 * x0 * l + 2 * y0 * m;
  float c = x0 * x0 + y0 * y0 - R * R;

  float t = (-1 * b + sqrt(b * b - 4 * a * c)) / (2 * a);

  z_projection = z0 + n * t;

  dz = 1; // +/- cm


  return true;

}

/**
 * Fit all the links found with run_patternRecognition
 *
 * This fills the segment list, so delete all links when done.
 */
void SvxStandAloneRecoLowMult::run_linkFitting()
{

  static TRACK_FITTER_LM fit_track;
  //  fit_track.set_DetectorGeo(d_svxgeometry);
  fit_track.set_tracker(d_tracker);
  fit_track.set_ProjectionFlag(false);
  fit_track.set_Bpolarity(d_Bpolarity);
  fit_track.set_recomode(d_recomode);
  fit_track.set_zerofieldflag(d_zerofieldflag);
  fit_track.set_compGeom(d_svxCompGeom);


  // initialize fit_track object
  fit_track.d_segmentlist = d_segment;
  // fit_track.d_cut = cut;
  fit_track.dv_vertex[0] = d_vertex[0];
  fit_track.dv_vertex[1] = d_vertex[1];
  fit_track.dv_vertex[2] = d_vertex[2];


  if (d_goodlinklist.size() > 0) {

    if (verbosity > 0) {
      cout << "************* Good LINKS found *********** " << d_goodlinklist.size() << endl;
    }
    // track fitting
    for_each(d_goodlinklist.begin(), d_goodlinklist.end(), fit_track);

  }

  if (verbosity > 0) {
    cout << "************* N SvxSegments *********** " << d_segment->get_nSegments() << endl;
  }

  // clear links and linknodes
  int nlink = d_linklist.size();
  for ( int ilink = 0; ilink < nlink; ilink++ ) {
    delete d_linklist[ilink];
  }

  d_linklist.clear();
  d_goodlinklist.clear();

}

const double m_dphi[2] = {0.005 / sqrt(12.), 0.008 / sqrt(12.)};
/// 0.005  : pixel size of pixel detector in phi-direction
/// 0.008  : pixel size of stripixel detector in phi-direction
const double m_dz[2] = {0.0425 / sqrt(12.), 0.1 / sqrt(12.)};
/// 0.0425 : pixel size of pixel detector in z-direction
/// 0.1    : pixel size of stripixel detector in z-direction

// better uncertainties from simulation -MPM
//const double m_dphi[2] = {1.16e-3, 1.14e-2};
//const double m_dz[2] = {1.72e-3, 2.49e-2};

void SvxStandAloneRecoLowMult::quickCircleCalculator(SvxLinkLM *link)
///
/// This function is intended to fit hits quickly as circle in XY-plane
/// and calculate rotation diameter and rotation center.
///
/// Input:
///  posx      x position of i-th hit
///  posy      y position of i-th hit
///  sublayerlist
///            sublayer ID of i-th hit
///
/// Output:
///  Rrot      rotation diameter
///  (cx, cy)  rotation center
///
/// This function requires at least 3 hits.
///
{
  int nhit = link->get_size();
  if ( nhit < 3 )
  {
    /// This function requires at least 3 hits.
    link->Rrot = -999.;
    link->cx = -999.;
    link->cy = -999.;
    return;
  }

  float sum_1  = 0.;
  float sum_x  = 0.;
  float sum_y  = 0.;
  float sum_z  = 0.;
  float sum_xx = 0.;
  float sum_xy = 0.;
  float sum_xz = 0.;
  float sum_yz = 0.;
  float sum_zz = 0.;

  for ( int ihit = 0; ihit < nhit; ihit++ )
  {
    float r2 = link->posx[ihit] * link->posx[ihit] + link->posy[ihit] * link->posy[ihit];
    float x = link->posx[ihit] / (r2 + 1.);
    float y = link->posy[ihit] / (r2 + 1);
    float z = r2 / (r2 + 1.);
    float sigma;
    if ( link->sublayerlist[ihit] < 0 ) {
      /// collision vertex (beam center)
      sigma = 0.01 / (r2 + 1.);
    } else if ( link->sublayerlist[ihit] < 2 ) {
      /// pixel
      sigma = m_dphi[0] / (r2 + 1.);
    } else {
      /// stripixel
      sigma = m_dphi[1] / (r2 + 1.);
    }
    sum_1  += 1. / sigma / sigma;
    sum_x  += x / sigma / sigma;
    sum_y  += y / sigma / sigma;
    sum_z  += z / sigma / sigma;
    sum_xx += x * x / sigma / sigma;
    sum_xy += x * y / sigma / sigma;
    sum_xz += x * z / sigma / sigma;
    sum_yz += y * z / sigma / sigma;
    sum_zz += z * z / sigma / sigma;
  }

  float XX = sum_xx - sum_x * sum_x / sum_1;
  float XY = sum_xy - sum_x * sum_y / sum_1;
  float XZ = sum_xz - sum_x * sum_z / sum_1;
  float YZ = sum_yz - sum_y * sum_z / sum_1;
  float ZZ = sum_zz - sum_z * sum_z / sum_1;

  /// fit with n1*x+y+n3*z+n4=0 plane
  float n1 = (YZ * XZ - XY * ZZ) / (XX * ZZ - XZ * XZ);
  float n3 = (XY * XZ - XX * YZ) / (XX * ZZ - XZ * XZ);
  float n4 = (-sum_x * n1 - sum_y - sum_z * n3) / sum_1;

  /// back to circle
  link->Rrot = sqrt( (n1 * n1 + 1. - 4.*n4 * (n3 + n4)) / (4.*(n3 + n4) * (n3 + n4)) );
  link->cx   = -0.5 * n1 / (n3 + n4);
  link->cy   = -0.5 / (n3 + n4);
}


