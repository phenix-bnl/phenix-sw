#include <SvxStandAloneReco.h>
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

int SvxStandAloneReco::segmentID = 0;

SvxStandAloneReco::SvxStandAloneReco(const string& name):
  SubsysReco(name),
  d_cluster(0),
  d_segment(0),
  d_vertexrecoflag(2),
  d_windowscale(1.0),
  d_reco_method(1),
  d_svxgeometry(0),
  d_vtxclusters(0),
  _timer(PHTimeServer::get()->insert_new(name)),
  d_Bpolarity(true),
  d_use_SvxVtxOut(false),
  d_use_vtx_projection(true),
  d_swapmode(false),
  d_ppflag(false),
  d_requireB1hit(true),
  d_use_vtx_projection_default(true),
  d_requireB1hit_default(true),
  d_bbcqcut(200),
  d_maxnclustcut(1000000),
  d_zerofieldflag(false),
  d_svxCompGeom(0),
  d_pixelMap(0),
  d_stripMap(0),
  d_address(0),
  DPHI0(0.),
  DPHI1(0.),
  DPHI2(0.),
  DZ0(0.),
  DZ1(0.),
  DZ2(0.),
  /// default size
  DPHI0_default(0.02),
  DPHI1_default(0.03),
  DPHI2_default(0.012),
  DZ0_default(0.05),  
  DZ1_default(0.02),  
  DZ2_default(0.03),  
  /// low multiplicity
  DPHI0_LowMult(0.04),
  DPHI1_LowMult(0.03),
  DPHI2_LowMult(0.024),
  DZ0_LowMult(-1),  
  DZ1_LowMult(0.03),  
  DZ2_LowMult(0.04),  
  /// pp
  DPHI0_PP(0.04),
  DPHI1_PP(0.03),
  DPHI2_PP(0.024),
  DZ0_PP(-1),  
  DZ1_PP(0.03),  
  DZ2_PP(0.04),  

  d_minhit(0),
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

  for ( int i=0; i<svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    d_swap[i] = false;  
  }
  for ( int i=0; i<svxDetectorGeo::SVXNSUBLAYER; i++ ) {
    Rsub[i] = 0.0;  
  }
  d_vertex[0] = 0.0;
  d_vertex[1] = 0.0;
  d_vertex[2] = 0.0;

  srand( time(NULL) );
}

SvxStandAloneReco::~SvxStandAloneReco()
{}

int SvxStandAloneReco::InitRun(PHCompositeNode *topNode)
{
  PHNodeIterator iter(topNode);

  if(d_swapmode) {
    // Set up for SvxFakeSegmentList node
    PHCompositeNode *dstNode;
    dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
    if(!dstNode) {
      cerr << PHWHERE << "DST Node missing, doing nothing." << endl;
      return EVENT_OK;
    }
    PHCompositeNode *svxNode;
    svxNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "SVX"));
    if(!svxNode) {
      cerr << PHWHERE << "SVX node missing, doing nothing." << endl;
      return EVENT_OK;
    }
    // add SvxFakeSegmentList node
    PHIODataNode<PHObject>* SvxSegmentListNode = NULL;
    SvxSegmentListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxFakeSegmentList");
    if(!SvxSegmentListNode) {
      SvxSegmentList* svxsegments = new SvxSegmentListv6();
      SvxSegmentListNode = new PHIODataNode<PHObject>(svxsegments, "SvxFakeSegmentList", "PHObject");
      svxNode->addNode(SvxSegmentListNode);
    }
  }

  /// Search SvxVtxOut node.
  /// If you can find it, use it. Otherwise, use VtxOut node.
  PHIODataNode<PHObject>* VtxOutNode = NULL;
  VtxOutNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxVtxOut");
  if(VtxOutNode) {
    d_use_SvxVtxOut = true;
  } else {
    d_use_SvxVtxOut = false;
  }

  d_svxgeometry = findNode::getClass<svxDetectorGeo>(topNode, "svxDetectorGeo");
  if ( d_svxgeometry == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find svxDetector. " << endl; }
    return EVENT_OK;
  }

  // check magnet current 
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if(runheader==NULL) {
    cout << PHWHERE<< "Can't find runheader. " << endl;
    return EVENT_OK;
  }
  if(runheader->get_currentCentral()>0){
   d_Bpolarity = true;
  } else {
   d_Bpolarity = false;
  }
      
  d_tracker.set_DetectorGeo(d_svxgeometry);
  for ( int isub=0; isub<svxDetectorGeo::SVXNSUBLAYER; isub++ ) {
    Rsub[isub] = d_svxgeometry->get_Rsublayer(isub);
  }
  
  if(d_zerofieldflag){
    d_tracker.set_ZeroFieldFlag(true);
  }

  /// for p+p events
  if(d_ppflag) { 
    //d_use_vtx_projection_default = false;
    d_requireB1hit_default = false;
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
     
  // Maximum limit of nclust in SvxStandAloneReco
  cout << PHWHERE << "Maximum limit of nclust in SvxStandAloneReco set to " << d_maxnclustcut << endl;

  return 0;
}


int SvxStandAloneReco::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();

  d_cluster=0;          
  segmentID=0;
  d_recomode = true;

  //
  // get PHOOL nodes
  // d_cluser: SVX hit clusters (input. 3D hit points)
  // d_segement: recontructed SVX track segement
  // vtxout : event vertex
  //

  if ( verbosity>0 ) {
    std::cout<<"SvxStandAloneReco::process_event() is called";
    std::cout<<" d_vertexrecoflag = "<<d_vertexrecoflag;
  }

  d_vtxclusters = findNode::getClass<SvxClusterContainer>(topNode, "SvxClusterContainer");
  if ( d_vtxclusters == NULL) {
    if(verbosity>0) { cout << PHWHERE<< "Can't find SvxClusterContainer. " << endl; }
    return EVENT_OK;
  }

  int nclust0 = d_vtxclusters->get_ncluster(0);
  int nclust1 = d_vtxclusters->get_ncluster(1);
  int nclust2 = d_vtxclusters->get_ncluster(2);
  int nclust3 = d_vtxclusters->get_ncluster(3);
  int nclust = nclust0 + nclust1 + nclust2 + nclust3;
  if(nclust > d_maxnclustcut) {
    cout << PHWHERE << "Too many clusters in SVX. Total = " << nclust << ", layer0 = " << nclust0 << ", layer1 = " << nclust1 << ", layer2 = " << nclust2 << ", layer3 = " << nclust3 << ": ABORTEVENT" << endl;
    return ABORTEVENT;
  }

  d_segment=0;
  PHTypedNodeIterator<SvxSegmentList> segmentiter(topNode);
  PHIODataNode<SvxSegmentList> *SvxSegmentListNode;
  if(d_swapmode) {
    SvxSegmentListNode = segmentiter.find("SvxFakeSegmentList");
  } else {
    SvxSegmentListNode = segmentiter.find("SvxSegmentList");
  }
  if (!SvxSegmentListNode) {
    if(verbosity>0) { cerr << PHWHERE << " ERROR: Can't find SvxSegmentList." << endl; }
    return EVENT_OK;
  } else {
    d_segment = (SvxSegmentList*)SvxSegmentListNode->getData();
  }
  
  VtxOut *vtxout=0;
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
    if ( d_vertexrecoflag==0 || d_vertexrecoflag==2 ) { return EVENT_OK; }
  } else {
    vtxout = (VtxOut*)VtxOutNode->getData();
  }

  // d_vertexrecoflag
  // 0: use beam center
  // 1: 
  // 2: use latest vertex
  
  if(d_vertexrecoflag==0) {  /// force to use beam center
    PHPoint verpoint = vtxout->get_Vertex("SVX");
    d_vertex[0] = verpoint.getX();
    d_vertex[1] = verpoint.getY();
    d_vertex[2] = verpoint.getZ();
  } else if(d_vertexrecoflag==2) {
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

  /// parameters reset to default values
  DPHI0 = DPHI0_default*d_windowscale;
  DPHI1 = DPHI1_default*d_windowscale;
  DPHI2 = DPHI2_default*d_windowscale;
  DZ0   = DZ0_default*d_windowscale;
  DZ1   = DZ1_default*d_windowscale;
  DZ2   = DZ2_default*d_windowscale;
  d_use_vtx_projection = d_use_vtx_projection_default;
  d_requireB1hit       = d_requireB1hit_default;
  d_minhit             = (d_use_vtx_projection) ? 4 : 3;

  BbcOut* bbcout = findNode::getClass<BbcOut>(topNode, "BbcOut");
  if(bbcout!=NULL){
    float bbcq = bbcout->get_ChargeSum(Bbc::North) + bbcout->get_ChargeSum(Bbc::South);
   
    if (d_ppflag)
      {

	DPHI0 = DPHI0_PP*d_windowscale;
	DPHI1 = DPHI1_PP*d_windowscale;
	DPHI2 = DPHI2_PP*d_windowscale;
	DZ0 = DZ0_PP; // no need to scale because -1 means no cut
	DZ1 = DZ1_PP*d_windowscale; 
	DZ2 = DZ2_PP*d_windowscale; 

      }
    else if (bbcq<d_bbcqcut)
      {
	/// use wider cut for low multiplicity ///
	DPHI0 = DPHI0_LowMult*d_windowscale;
	DPHI1 = DPHI1_LowMult*d_windowscale;
	DPHI2 = DPHI2_LowMult*d_windowscale;
	DZ0 = DZ0_LowMult; // no need to scale because -1 means no cut
	DZ1 = DZ1_LowMult*d_windowscale;
	DZ2 = DZ2_LowMult*d_windowscale;
	//d_use_vtx_projection = false;
	//d_minhit = 3;
	d_requireB1hit = false;	
      }
  } else {
    cout<<"No BbcOut found"<<endl;
    return EVENT_OK;
  }

  if ( verbosity>0 ) {
    std::cout<<" d_vertex[]=("<<d_vertex[0]<<","<<d_vertex[1]<<","<<d_vertex[2]<<")"<<std::endl;
  }

  //  float cut=-0.375;
  //  float cut=-5.;
  float cut=-50.;
  
  if( d_reco_method == 1) {
    // Use new method.
    run_link(cut);
  }

  _timer.get()->stop();

  return 0;
}


/******************************************************
* From here, code for link method
********************************************************/

static void print(SvxLink *link) {
  int n = link->get_size();
  for(int i=0;i<n;i++) {
    cout <<i<<": "<<link->nodelist[i]->sublayer
	 <<"| "<<
         (link->nodelist[i]->cluster==NULL ? -1 
                                           : link->nodelist[i]->cluster->get_hitID())
         <<" "
	 <<"| ("<<link->nodelist[i]->x
	 <<", "<<link->nodelist[i]->y
	 <<", "<<link->nodelist[i]->z
	 <<") "<<endl;
  }
  cout <<"++++++++++++++++++++++++++"<<endl;
}

/********************************
 * Helper class for link method
 *********************************/

struct TRACK_FITTER {
  //
  // Interface to SvxTracker and d_segment
  //
  TRACK_FITTER():
    d_segmentlist(0),
    d_cut(-0.375),
    dv_vertex(3,0.0),
    d_use_vtx_projection(false),
    d_Bpolarity(false),
    d_recomode(false),
    d_zerofieldflag(false),
    d_svxCompGeom(0)
  {}
  void operator()(SvxLink *link);

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


void TRACK_FITTER::operator()(SvxLink *link) {
  //
  // Perform track fitting
  // 
  // input:
  //   link   link of clusters found in VTX.
  //   Note that number of hits in a link is 3 or 4 or 5.
  //  
  int n = link->get_size();
  vector<SvxCluster*> vcluster;
  if ( d_use_vtx_projection ) {
    for ( int i=1; i<n; i++ ) {
      SvxCluster *tmp_cls = link->nodelist[i]->cluster;
      vcluster.push_back(tmp_cls);
    }
  } else {
    for ( int i=0; i<n; i++ ) {
      SvxCluster *tmp_cls = link->nodelist[i]->cluster;
      vcluster.push_back(tmp_cls);
    }
  }
  // rough guess of pt
  double pt = link->Rrot*0.003*0.9;
  if(pt<0.02 && (!d_zerofieldflag)) return; // pt<20MeV should curl up in the VTX. stop processing



  bool helicity;   /// true : left-handed; false : right-handed
  if ( (link->cy-link->posy[1])*(link->posx[n-1]-link->posx[1])
       - (link->cx-link->posx[1])*(link->posy[n-1]-link->posy[1])>0. ) {
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

  if(tprob > d_cut) { // good track. Accept it.
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
    d_tracker.getExpectedPosition(0,x0,y0,z0,0);

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
    float dca3d = sqrt( (dv_vertex[0]-pos_primary[0])*(dv_vertex[0]-pos_primary[0])
			+ (dv_vertex[1]-pos_primary[1])*(dv_vertex[1]-pos_primary[1])
			+ (dv_vertex[2]-pos_primary[2])*(dv_vertex[2]-pos_primary[2]) );
    if ( dca2d<0 ) { dca3d *= -1.; }   /// if dca2d is negative, dca3d is also negative.

    /// 0.05 is a temporary value, so it should be modified.
    bool is_primary = false;
    if ( dca3d<0.05 ) is_primary = true;

    // check this track is consistent with a primary track.
    // if it is, re-calcualted the momentum with additional constraint
    // that the track is from the vertex.
    // (basically the same function of findPrimary() in the original
    
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
    
    int Nhits_layer[4]={0,0,0,0};
    float dEdx1=0;
    float dEdx2=0;

    int start=(d_use_vtx_projection) ? 1 : 0;
    for(int i=start;i<n;i++) {
      SvxCluster *cluster=link->nodelist[i]->cluster;
      int layer = cluster->get_layer();
      int clusterID = cluster->get_hitID();
      if(layer>=0&& layer<4) {
	segment->setClusterID(layer,Nhits_layer[layer],clusterID);

	float clusGoodFrac = 1;
	int nbad = 0;
	nbad += (cluster->get_Ncold()>0) ? cluster->get_Ncold() : 0;
	nbad += (cluster->get_Nhot()>0) ? cluster->get_Nhot() : 0;
	int size = cluster->get_size();
	clusGoodFrac = (float)(size-nbad)/(float)size;
	segment->setClusterGoodFraction(layer,Nhits_layer[layer],clusGoodFrac);

	if(layer==0) {
	  segment->setScatter(layer,0.0);
	} else if(layer<3) {
	  segment->setScatter(layer,d_tracker.getScatter(layer-1));
	}
	if(layer == 2) dEdx1 += (cluster->get_adc(0)+cluster->get_adc(1));
	if(layer == 3) dEdx2 += (cluster->get_adc(0)+cluster->get_adc(1));
	
	/// set expected hit position	
	if ( Nhits_layer[layer]<2 ) {
	  float pos[3];
	  d_tracker.getExpectedPosition(layer,pos[0],pos[1],pos[2],Nhits_layer[layer]);
	  segment->setProjectedPosition(layer,pos[0],pos[1],pos[2],Nhits_layer[layer]);
	  Nhits_layer[layer]++;
	}
      }
    }
    for(int layer=0;layer<4;layer++) {
      segment->setNhits(layer,Nhits_layer[layer]);
    }
    
    segment->setSegmentID(SvxStandAloneReco::segmentID);
    SvxStandAloneReco::segmentID++;

    segment->set3MomentumAtPrimaryVertex(px0,py0,pz0);
    segment->set3Momentum(px,py,pz);
    segment->setIsPositive(1^(helicity^d_Bpolarity));
    segment->setPrimary(is_primary);
    segment->setQuality(tprob);
    segment->setChiSq(chisq);
    segment->setNDF(ndf);
    segment->setDCA(dca3d);
    segment->setDCA2D(dca2d);
    segment->setInnerMostProjectedPosition(0,x0);
    segment->setInnerMostProjectedPosition(1,y0);
    segment->setInnerMostProjectedPosition(2,z0);
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
	float mom = sqrt(px0*px0 + py0*py0 + pz0*pz0);
	float pt = sqrt(px0*px0 + py0*py0);
	float phi0 = atan2(py0,px0);
	float the0 = acos(pz0/pt);
	float charge = 1;
	if (helicity != d_Bpolarity)
	  charge = -1;
	float B = 0.90; //field strength
	float fieldScale = 1.;
	if (!d_Bpolarity)
	  fieldScale = -1;
	ScgTrack geoTrack = d_svxCompGeom->FindHitsFromVertex(dv_vertex.at(0), dv_vertex.at(1), dv_vertex.at(2),
								 mom, phi0, the0, 
								 charge, fieldScale*B);
	
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
		for(int iclus=start;iclus<link->get_size();iclus++) 
		  {
		    SvxCluster *cluster=link->nodelist[iclus]->cluster;
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
			float liveperc = d_svxCompGeom->GetTileGoodFrac(layer, ladder, sensor, comp, tile)*100.;
			
			//get the highest live fraction 
			if (liveperc > livePerc[i])
			  livePerc[i] = liveperc;
			
		      }
		  }
	      }
	    else
	      {
		for (int ihit=0; ihit<geoTrack.nhits; ihit++) 
		  {
		    ScgHit hit = geoTrack.GetHit(ihit);
		    
		    if (hit.layer == i && hit.livefrac*100. > livePerc[i])
		      livePerc[i] = hit.livefrac*100.;
		  }
	      }
	  }
      }

    segment->setSegmentScore(linkScore);
    segment->setSegmentQuality(linkQuality);
    for (int i = 0; i < 4; i++)
      segment->setLivePercentage(i,livePerc[i]);
    
    // quick hack to write out a small portion of randomly selected tracks
    // to make the output size reasonable for debugging central events -MPM
    //if(((float)rand()/(float)RAND_MAX) < 0.01)
    //{
    //((SvxSegmentListv6*)d_segmentlist)->AddSegment((unsigned int)(d_segmentlist->get_nSegments()), *segment);
    //delete segment;
    //}

    ((SvxSegmentListv6*)d_segmentlist)->AddSegment((unsigned int)(d_segmentlist->get_nSegments()), *segment);

    delete segment;
  }
}

float TRACK_FITTER::getLinkScore(ScgTrack &geoTrack, int* Nhits_layer)
{
  // Accounting variables
  int nclus = 0, npixl = 0;

  //the fraction of bad pixels in the tile for a 
  vector<float> badFrac_missingLayers; 
  vector<int> missingLayers;

  //find the number of clusters in this link
  for (int i=0; i<4; ++i) {
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
	  badFrac_missingLayers.push_back(1-geoHit.livefrac);
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
    linkScore = (npixl==2) ? 70. : 60.;

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
float TRACK_FITTER::getLinkQuality(float linkScore, float chisq, float ndf)
{
  //check ndf for nan and < 0
  if (ndf < 0 || ndf !=ndf)
    return -9999.;
  else
    return 1.0/(chisq/ndf + 2) + linkScore/100.;

}




void SvxStandAloneReco::run_link(float cut) {
  // Tracking by "link" method.
  // this only does the patter recongnition. VTX hit clusters are formed
  // into a link, and the link is the fitted by SvxTracker code.
  // 
  // input:
  //   quality cut parameter for tracks.
  //   those track with tprob < 2*cut is accepted.
  // output:
  //   d_segment
  //
  static TRACK_FITTER fit_track;
  //  fit_track.set_DetectorGeo(d_svxgeometry);
  fit_track.set_tracker(d_tracker);
  fit_track.set_ProjectionFlag(d_use_vtx_projection);
  fit_track.set_Bpolarity(d_Bpolarity);
  fit_track.set_recomode(d_recomode);
  fit_track.set_zerofieldflag(d_zerofieldflag);
  fit_track.set_compGeom(d_svxCompGeom);

  // clear the data from previous event
  d_linklist.clear();
  d_goodlinklist.clear();
  d_linknodelist.clear();

  // initialize fit_track object
  fit_track.d_segmentlist = d_segment;
  fit_track.d_cut = cut;
  fit_track.dv_vertex[0] = d_vertex[0];
  fit_track.dv_vertex[1] = d_vertex[1];
  fit_track.dv_vertex[2] = d_vertex[2];

  //  d_vtxclusters->print();

  // Select all clusters in layer 0
  int sublayer = 0;
  vector<SvxCluster*> vclust;
  int nclust = d_vtxclusters->find_clusters(vclust,sublayer);



  // Start link search starting from all clusters in layer 0
  // LinkCluster find a link of clusters recursively. The result
  // is stored in d_goodlinklist.

  // link node for the primary vertex
  // the node has no corresponding cluster object and sublayer is -1.
  SvxLinkNode *privtxnode = new SvxLinkNode(NULL,d_vertex[0],d_vertex[1],d_vertex[2],-1);
  d_linknodelist.push_back(privtxnode);

  for(int i=0;i<nclust;i++) {
    float x = vclust[i]->get_xyz_global(0);
    float y = vclust[i]->get_xyz_global(1);
    float z = vclust[i]->get_xyz_global(2);
    float phi = d_vtxclusters->calc_phi(x,y);

    SvxLinkNode *node = new SvxLinkNode(vclust[i],x,y,z,0);
    d_linknodelist.push_back(node);
    SvxLink *link;
    if(d_use_vtx_projection) {
      link = new SvxLink(privtxnode);
      link->add_node(node);
      d_linklist.push_back(link);
    } else {
      link = new SvxLink(node);
      d_linklist.push_back(link);
    }
    if(!d_use_vtx_projection || d_vtxclusters->get_ncluster(0) < 30 || d_ppflag) {
      // low multiplicity event like p+p or not use primary vertex in projection.
      // -1 in the last argument (dz) means that there is no cut in dz.
      LinkClusters(sublayer+1,link,phi,DPHI0,0.0,-1);
    } else {
      // high multiplicity event. Here Zvetex should be
      // reliably calculated.
      float phi_proj;
      float z_proj;
      float dphi;
      float dz;
      if(!calc_projection(link,Rsub[1],phi_proj,z_proj,dphi,dz)) continue;
      if(fabs(z_proj)< SvxClusterContainer::Zmax1 ) {
	LinkClusters(sublayer+1,link,phi_proj,dphi,z_proj,dz);
      }
    }
  }
  
  if(verbosity>0) {
    // print the links generated
    cout <<"************* LINKS found *********** "<<d_goodlinklist.size()<<endl;
    for_each(d_goodlinklist.begin(),d_goodlinklist.end(),print);
  }

  vector<SvxLink*> good_links;
  SelectLinks(d_goodlinklist, good_links);
    
  if(good_links.size()>0 || !d_recomode) {
    // track fitting
    for_each(good_links.begin(),good_links.end(),fit_track);  

    if(verbosity>0) {
      cout <<"************* Good LINKS found *********** "<<good_links.size()<<endl;
    }
  }
  
  // clear links and linknodes
  int nlink = d_linklist.size();
  for ( int ilink=0; ilink<nlink; ilink++ ) {
    delete d_linklist[ilink];
  }
  int nnode = d_linknodelist.size();
  for ( int inode=0; inode<nnode; inode++ ) {
    delete d_linknodelist[inode];
  }
  
  d_linklist.clear();
  d_goodlinklist.clear();
  d_linknodelist.clear();
}


void SvxStandAloneReco::LinkClusters(int sublayer, SvxLink *link,
				     float phi0, float dphi0,
				     float z0, float dz0)
{
  // find clusters in sublayer
  vector<SvxCluster*> vclust;
  int nclust;
  if(d_swap[sublayer]) {
    // find swapped clusters
    if(dz0<0) {
      // select entire dphi range. No dz cut.
      nclust = d_vtxclusters->find_fake_clusters(vclust,sublayer,phi0,dphi0);
    } else {
      nclust = d_vtxclusters->find_fake_clusters(vclust,sublayer,phi0,dphi0,z0,dz0);
    }
  } else {
    // find real clusters
    if(dz0<0) {
      // select entire dphi range. No dz cut.
      nclust = d_vtxclusters->find_clusters(vclust,sublayer,phi0,dphi0);
    } else {
      nclust = d_vtxclusters->find_clusters(vclust,sublayer,phi0,dphi0,z0,dz0);
    }
  }

  float phi_proj;
  float dphi;
  float z_proj;
  float dz;
  for(int i=0;i<nclust;i++) {
    float x = vclust[i]->get_xyz_global(0);
    float y = vclust[i]->get_xyz_global(1);
    float z = vclust[i]->get_xyz_global(2);

    double org_x=0., org_y=0., org_z=0.;
    getOriginOffset(((x>0) ? 0 : 1), org_x, org_y, org_z);
    float r = sqrt( ((x-org_x)*(x-org_x)) + ((y-org_y)*(y-org_y))); // the offset must be subtracted

    float phi = d_vtxclusters->calc_phi(x,y);    
    if(!calc_projection(link,r,phi_proj,z_proj,dphi,dz)) continue;
    if (dz0<0) {   /// no cut in z direction
      z_proj = 0;
      dz = 100;
    }
    if(fabs(phi-phi_proj)<dphi && fabs(z-z_proj)<dz) {
      //
      // acceptable hit is found.
      // Make a new branch of tree to continue this link direction
      // Note that you must make new_link here. Otherwise, you have a problem
      // if there is more than one acceptable hit
      //
      SvxLinkNode *new_node = new SvxLinkNode(vclust[i],x,y,z,sublayer);
      d_linknodelist.push_back(new_node);
      SvxLink *new_link = new SvxLink(link);
      d_linklist.push_back(new_link);
      new_link->add_node(new_node);
      if(d_swap[sublayer]) new_link->is_real = false;
      if(new_link->get_size()>=3) quickCircleCalculator(new_link);
      
      // If reached SVXMAXSUBLAYER, stop the search.
      if(sublayer >= SVXMAXSUBLAYER) {
	if(new_link->get_size()>=d_minhit) {
	  if(d_swapmode) {
	    // Only links with fake cluster are stored.
	    if(!new_link->is_real) d_goodlinklist.push_back(new_link);
	  } else {
	    d_goodlinklist.push_back(new_link);
	  }
	}
      } else {
	// Otherwise search the next sublayer
	// calculate phi projected position            
	if(!calc_projection(new_link,Rsub[sublayer+1],phi_proj,z_proj,dphi,dz)) continue;
	LinkClusters(sublayer+1,new_link,phi_proj,dphi,z_proj,dz);
      }
    }
  }
  //If d_requireB1hit=true and this is the second pixel layer, stop the search
  //i.e. we require layer0 and layer1
  if(d_requireB1hit && sublayer==1) return;
  
  //If reached SVXMAXSUBLAYER, stop the search
  if(sublayer==SVXMAXSUBLAYER) {
    if(link->get_size()>=d_minhit) {
      if(d_swapmode) {
	// Only links with fake cluster are stored.
	if(!link->is_real) d_goodlinklist.push_back(link);
      } else {
	d_goodlinklist.push_back(link);
      }
    }
    return;
  }

  //Otherwise, search the next sublayer
  if(!calc_projection(link,Rsub[sublayer+1],phi_proj,z_proj,dphi,dz)) return;
  if ( dz0<0 ) {
    z_proj = 0;
    dz = dz0;
  }
  LinkClusters(sublayer+1,link,phi_proj,dphi,z_proj,dz);
}


const double m_dphi[2] = {0.005/sqrt(12.), 0.008/sqrt(12.)};
/// 0.005  : pixel size of pixel detector in phi-direction
/// 0.008  : pixel size of stripixel detector in phi-direction
const double m_dz[2] = {0.0425/sqrt(12.), 0.1/sqrt(12.)};
/// 0.0425 : pixel size of pixel detector in z-direction
/// 0.1    : pixel size of stripixel detector in z-direction

// better uncertainties from simulation -MPM
//const double m_dphi[2] = {1.16e-3, 1.14e-2};
//const double m_dz[2] = {1.72e-3, 2.49e-2};

void SvxStandAloneReco::quickCircleCalculator(SvxLink *link)
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
  if ( nhit<3 )
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

  for ( int ihit=0; ihit<nhit; ihit++ )
    {      
      float r2 = link->posx[ihit]*link->posx[ihit]+link->posy[ihit]*link->posy[ihit];
      float x = link->posx[ihit]/(r2+1.);
      float y = link->posy[ihit]/(r2+1);
      float z = r2/(r2+1.);
      float sigma;
      if ( link->sublayerlist[ihit]<0 ) {
	/// collision vertex (beam center)
	sigma = 0.01/(r2+1.);
      } else if ( link->sublayerlist[ihit]<2 ) {
	/// pixel
	sigma = m_dphi[0]/(r2+1.);
      } else {
	/// stripixel
	sigma = m_dphi[1]/(r2+1.);
      }
      sum_1  += 1./sigma/sigma;
      sum_x  += x/sigma/sigma;
      sum_y  += y/sigma/sigma;
      sum_z  += z/sigma/sigma;
      sum_xx += x*x/sigma/sigma;
      sum_xy += x*y/sigma/sigma;
      sum_xz += x*z/sigma/sigma;
      sum_yz += y*z/sigma/sigma;
      sum_zz += z*z/sigma/sigma;
    }

  float XX = sum_xx - sum_x*sum_x/sum_1;
  float XY = sum_xy - sum_x*sum_y/sum_1;
  float XZ = sum_xz - sum_x*sum_z/sum_1;
  float YZ = sum_yz - sum_y*sum_z/sum_1;
  float ZZ = sum_zz - sum_z*sum_z/sum_1;
  
  /// fit with n1*x+y+n3*z+n4=0 plane
  float n1 = (YZ*XZ - XY*ZZ) / (XX*ZZ - XZ*XZ);
  float n3 = (XY*XZ - XX*YZ) / (XX*ZZ - XZ*XZ);
  float n4 = (-sum_x*n1 - sum_y - sum_z*n3) / sum_1;

  /// back to circle
  link->Rrot = sqrt( (n1*n1+1.-4.*n4*(n3+n4)) / (4.*(n3+n4)*(n3+n4)) );
  link->cx   = -0.5*n1 / (n3+n4);
  link->cy   = -0.5 / (n3+n4);
}



bool SvxStandAloneReco::calc_projection(SvxLink *link, float r0,
					float &phi_projection, float &z_projection,
					float &dphi, float &dz)
{
// // description by T. Hachiya 2014.11.4
// Why does the offset needs to be subtracted?
//
// Projection position is calculated in the VTX local coordinate system.
// This assumes that layers are aligned in cylindrical symmetry with a certain radius 
// from the local (x,y)=(0,0).
//
// In clustering, the coordinate origin is shifted with the coordinate offset 
// in order to match the difference of the system between VTX and DCH.
// Due to this shift, the projection can be calculated to the wrong position.
// 
// In order to avoid this, the cluster position and vertex position is shifted back to 
// the position in the VTX local system.
//
// the function "getOriginOffset" provides the (x,y,z) offset (VtxToCNT for west and VtxToCNT + EastToWest for east).
// These offsets is subtracted from the cluster position and vertex position
//
//
  int n = link->get_size();

  if ( n<1 ) {
    cout << "ERROR : Projection needs at least one hit" << endl; 
    phi_projection = -100.;
    z_projection = -100.;
    dphi = -1.;
    dz = -1.;
    return false;
  } else {
    ////////////////
    double org_x=0., org_y=0., org_z=0.;
    getOriginOffset(((link->posx[n-1]>0) ? 0 : 1), org_x, org_y, org_z);

    float xproj;   /// projection position in X
    float yproj;   /// projection position in Y
    float xprev1;
    float xprev2;
    float yprev1;
    float yprev2;
    float zprev1;
    float zprev2;
    if ( n==1 ) {
      // origin offset is subtracted
      xprev1 = link->posx[0] - org_x;
      xprev2 = d_vertex[0]   - org_x;
      yprev1 = link->posy[0] - org_y;
      yprev2 = d_vertex[1]   - org_y;
      zprev1 = link->posz[0] - org_z;
      zprev2 = d_vertex[2]   - org_z;
      if ( d_use_vtx_projection || (xprev1==xprev2 && yprev1==yprev2) ) {
	cout << "ERROR : Projection needs at least one hit other than vertex" << endl;
	phi_projection = -100.;
	z_projection = -100.;
	dphi = -1.;
	dz = -1.;
	return false;
      }
    } else {
      // origin offset is subtracted
      xprev1 = link->posx[n-1] - org_x;
      xprev2 = link->posx[n-2] - org_x;
      yprev1 = link->posy[n-1] - org_y;
      yprev2 = link->posy[n-2] - org_y;
      zprev1 = link->posz[n-1] - org_z;
      zprev2 = link->posz[n-2] - org_z;
    }


    if ( n<3 ) {
      ///
      /// If n<3, projection in XY-plane is line projection
      ///
      phi_projection = d_vtxclusters->calc_phi(xprev1 + org_x, yprev1 + org_y); // origin offset is subtracted
      
      /// calculate x & y projection
      float cos_phi = cos(phi_projection);
      float sin_phi = sin(phi_projection);
      float l0 = (d_vertex[0]-org_x)*sin_phi-(d_vertex[1]-org_y)*cos_phi; // origin offset is subtracted
      float vx = l0*sin_phi;
      float vy = l0*(-cos_phi);
      if ( r0*r0-l0*l0<0 ) {
	cout << "ERROR : Primary vertex is too far from (0,0)" << endl;
	return false;
      }
      float dl = sqrt(r0*r0-l0*l0);
      /// (xproj, yproj) should be the same side from (vx, vy)
      /// as (xprev1, yprev1).
      if ( xprev1*cos_phi+yprev1*sin_phi>0 ) {
	xproj = vx + dl*cos_phi;
	yproj = vy + dl*sin_phi;
      } else {
	xproj = vx - dl*cos_phi;
	yproj = vy - dl*sin_phi;
      }
      float dr1 = sqrt((xproj -xprev1)*(xproj -xprev1)+(yproj -yprev1)*(yproj -yprev1));
      float dr2 = sqrt((xprev1-xprev2)*(xprev1-xprev2)+(yprev1-yprev2)*(yprev1-yprev2));
      dphi = DPHI0*dr1/dr2;

      //cout<<"vx : vy = "<<xproj<<" "<<yproj<<" "<<dr1<<" "<<dr2<<" "<<l0<<endl;
    } else {
      ///
      /// If n>=3, projection in XY-plane is circle projection
      ///
      if ( link->Rrot<0. ) {
	cout << "ERROR at rotation diameter and rotation center calculation" << endl;
	return false;
      }
      /// check helicity
      int helicity;
      float phi0 = atan2(link->posy[0]-link->cy, link->posx[0]-link->cx);
      float phi1 = atan2(link->posy[1]-link->cy, link->posx[1]-link->cx);
      if ( phi1-phi0> M_PI ) phi1 -= M_PI*2.;
      if ( phi1-phi0<-M_PI ) phi1 += M_PI*2.;
      if ( phi1>phi0 ) {
	helicity = 1;
      } else {
	helicity = -1;
      }
      
      /// calculate x & y projection
      float rprev1 = sqrt(xprev1*xprev1+yprev1*yprev1);
      float phi    = atan2(yprev1-(link->cy-org_y), xprev1-(link->cx-org_x))+M_PI*0.5*helicity; // origin offset is subtracted

      /// phi : momentum vector direction in XY-plane
      /// px=pT*cos(phi), py=pT*sin(phi)
      if ( (xprev1*cos(phi)+yprev1*sin(phi)) < 0 ) return false;
      float Rdiff = (r0-rprev1) / (xprev1/rprev1*cos(phi)+yprev1/rprev1*sin(phi));
      /// Rdiff : distance between (xprev1, yprev1) and projected hit. projection is
      ///         done by extention of momentum vector.
      float L;
      if ( link->Rrot>Rdiff ) {
	L = 2.*link->Rrot*sqrt(0.5*(1.-sqrt(1.-Rdiff*Rdiff/link->Rrot/link->Rrot)));
      } else {
	L = Rdiff*sqrt(2.);
      }
      /// L : distance between (xprev1, yprev1) and projected hit. bending in magnetic
      ///     field is taken into account in the projection.
      if ( 0.5*L/link->Rrot>1. ) return false;
      float phi_bend = asin(0.5*L/link->Rrot)*helicity;

      //ADDED By Theo Koblesky theodore.koblesky@colorado.edu
      // on June 15, 2014
      //if were are reconstructing for a zerofield run
      // set the phi ben angle to zero
      if(d_zerofieldflag) phi_bend = 0;
      /// phi_bend : bending angle in XY-plane
      xproj = L*cos(phi+phi_bend)+xprev1;
      yproj = L*sin(phi+phi_bend)+yprev1;

      /// calculate phi projection
      phi_projection = d_vtxclusters->calc_phi(xproj + org_x, yproj + org_y); // origin offset is subtracted
      if ( link->get_size()==3 ) {
	dphi = DPHI1*L/sqrt(xproj*xproj+yproj*yproj);
      } else {
	dphi = DPHI2*L/sqrt(xproj*xproj+yproj*yproj);
      }
    }
    //cout<<"phi_proj : "<<n<<" "<<phi_projection<<" "<<dphi<<" "<<r0<<endl;

    /// calculate z projection
    float dr1 = sqrt((xproj -xprev1)*(xproj -xprev1)+(yproj -yprev1)*(yproj -yprev1));
    float dr2 = sqrt((xprev1-xprev2)*(xprev1-xprev2)+(yprev1-yprev2)*(yprev1-yprev2));
    if ( n<3 ) {
      z_projection = zprev1 + (zprev1-zprev2)*dr1/dr2;
      dz = DZ0*sqrt(dr1*dr1+(z_projection-zprev1)*(z_projection-zprev1));
    } else {
      float l1 = 2.*link->Rrot*asin(0.5*dr1/link->Rrot);
      float l2 = 2.*link->Rrot*asin(0.5*dr2/link->Rrot);
      z_projection = zprev1 + (zprev1-zprev2)*l1/l2;
      if ( n==3 ) {
	dz = DZ1*sqrt(l1*l1+(z_projection-zprev1)*(z_projection-zprev1));
      } else {
	dz = DZ2*sqrt(l1*l1+(z_projection-zprev1)*(z_projection-zprev1));
      }
    }
    //cout<<"z_proj : "<<n<<" "<<z_projection<<" "<<dz<<" "<<endl;
  }
  return true;
}

void SvxStandAloneReco::getOriginOffset(int weflag, double& xoff, double& yoff, double& zoff){
  if(d_svxgeometry==NULL){
    cerr<<"SvxStandAloneReco::getOriginOffset() : svxDetectorGeo object is NULL"<<endl;
    return;
  }


  if(weflag==0) { // west VTXtoCNT
    double vtoc_off[3];
    d_svxgeometry->getOffsetVtxToCnt(vtoc_off[0], vtoc_off[1], vtoc_off[2]);

    xoff = vtoc_off[0];
    yoff = vtoc_off[1];
    zoff = vtoc_off[2];
  }
  else { // east VTXtoCNT + EastToWest 
    double vtoc_off[3], etow_off[3]={0,0,0};
    d_svxgeometry->getOffsetVtxToCnt(  vtoc_off[0], vtoc_off[1], vtoc_off[2]);
    d_svxgeometry->getOffsetEastToWest(etow_off[0], etow_off[1], etow_off[2]);

    xoff = vtoc_off[0] + etow_off[0];
    yoff = vtoc_off[1] + etow_off[1];
    zoff = vtoc_off[2] + etow_off[2];
  }
  
}



void SvxStandAloneReco::SelectLinks(const vector<SvxLink*> &linklist,
				    vector<SvxLink*> &outlist)
{
  /// clear output vector
  outlist.clear();

  map<int, int> map_clusterID_linklocation;
  /// correspondence between ID of cluster on Barrel-0 and location in outlist
 
 int prelink_id = -1;
  int prelink_location = 0;
  /// location in outlist of the previous link.
  int id_max = -1;
  for ( unsigned int ilink=0; ilink<linklist.size(); ilink++ ) {
    /// get ID of cluster on Barrel-0
    int B0_location = ( d_use_vtx_projection ) ? 1 : 0;
    /// location of B0 hit at nodelist.
    int id = linklist[ilink]->nodelist[B0_location]->cluster->get_hitID();
    if ( id_max<id ) {
      outlist.push_back(linklist[ilink]);
      id_max = id;
      prelink_id = id;
      prelink_location = outlist.size()-1;     
      /// update map_clusterID_linklocation
      map_clusterID_linklocation.insert(make_pair(id, outlist.size()-1));
    } else if ( prelink_id==id ) {
      outlist[prelink_location] = compare_links(outlist[prelink_location], linklist[ilink]);
    } else {
      map<int, int>::iterator itr = map_clusterID_linklocation.find(id);
      if ( itr!=map_clusterID_linklocation.end() ) {
	/// succeed to find
	outlist[itr->second] = compare_links(outlist[itr->second], linklist[ilink]);
	prelink_id = id;
	prelink_location = itr->second;
      } else {
	/// fail to find
	outlist.push_back(linklist[ilink]);
	prelink_id = id;
	prelink_location = outlist.size()-1;
      }
    }
  }
}


SvxLink* SvxStandAloneReco::compare_links(SvxLink *link1, SvxLink *link2)
{
  ///
  /// at first, compare with # of hits
  ///
  int n1 = link1->get_size();
  int n2 = link2->get_size();
  /// n1, n2 = # of hits + 1 (<-primary vertex) if d_use_vtx_projection=true.
  if ( n1>=d_minhit+1 && n2<d_minhit+1 ) {
    return link1;
  } else if ( n1<d_minhit+1 && n2>=d_minhit+1 ) {
    return link2;
  }

  ///
  /// then, compare with track quality
  ///
  //  SvxTracker tracker;
  //  tracker.set_DetectorGeo(d_svxgeometry);

  /// if quality has not been calculated yet, calculate it.
  if ( link1->quality<-100 ) {
    vector<SvxCluster*> vcluster;
    if ( d_use_vtx_projection ) {
      for ( int i=1; i<n1; i++ ) {
	SvxCluster *tmp_cls = link1->nodelist[i]->cluster;
	vcluster.push_back(tmp_cls);
      }
    } else {
      for ( int i=0; i<n1; i++ ) {
	SvxCluster *tmp_cls = link1->nodelist[i]->cluster;
	vcluster.push_back(tmp_cls);
      }
    }
    double pt = link1->Rrot*0.003*0.9;
    bool helicity;   /// true : counter-clockwise; false : clockwise
    if ( (link1->cy-link1->posy[1])*(link1->posx[n1-1]-link1->posx[1])
	 - (link1->cx-link1->posx[1])*(link1->posy[n1-1]-link1->posy[1])>0. ) {
      helicity = true;
    } else {
      helicity = false;
    }
    // fill these variables too
    float chisq = 0.0;
    int ndf = 0;
    link1->quality = d_tracker.TrackFit(vcluster, helicity, pt, chisq, ndf);
  }

  /// if quality has not been calculated yet, calculate it.
  if ( link2->quality<-100 ) {
    vector<SvxCluster*> vcluster;
    if ( d_use_vtx_projection ) {
      for ( int i=1; i<n2; i++ ) {
	SvxCluster *tmp_cls = link2->nodelist[i]->cluster;
	vcluster.push_back(tmp_cls);
      }
    } else {
      for ( int i=0; i<n2; i++ ) {
	SvxCluster *tmp_cls = link2->nodelist[i]->cluster;
	vcluster.push_back(tmp_cls);
      }
    }
    double pt = link2->Rrot*0.003*0.9;
    bool helicity;   /// true : counter-clockwise; false : clockwise
    if ( (link2->cy-link2->posy[1])*(link2->posx[n2-1]-link2->posx[1])
	 - (link2->cx-link2->posx[1])*(link2->posy[n2-1]-link2->posy[1])>0. ) {
      helicity = true;
    } else {
      helicity = false;
    }
    // fill these variables too
    float chisq = 0.0;
    int ndf = 0;
    link2->quality = d_tracker.TrackFit(vcluster, helicity, pt, chisq, ndf);
  }

  if ( link1->quality>=link2->quality ) {
    return link1;
  } else {
    return link2;
  }
}
