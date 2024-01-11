#include <cstdlib>
#include <iostream>
#include <fstream>
#include <algorithm>

#include <SvxCentralTrack.h>
#include <SvxCentralTrackList.h>
#include <SvxSelectClusters.h>
#include <SvxClusterContainer.h>
#include <SvxCluster.h>
#include <SvxClusterList.h>
#include <SvxClusterListv5.h>
#include <SvxSegment.h>
#include <SvxSegmentList.h>

#include <RunHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <Bbc.hh>
#include <BbcOut.h>
#include <VtxOut.h>
#include <PHPoint.h>
#include <Fun4AllReturnCodes.h>
#include <PHNodeIterator.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>

using namespace std;

//----------------------------------------------------------------------------------------------------

SvxSelectClusters::SvxSelectClusters(const string &name) : SubsysReco(name) { 
  ZCut[0]= 2.0; ZCut[1]=3.0; ZCut[2]=4.0; // i=0,1,2 => 200<=bbcq, 50<=bbcq<200, 0<=bbcq<50
  PhiCut = 1.0; // cm
  MomCut = 0.5; // GeV/c
  Z_reduction   = 2.0;
  Phi_reduction = 2.0;
  m_vtxflag  = 0;
  m_select_e = 1;
  m_select_e_n0cut = 2;
  m_select_e_eopcut = 0.3;
  m_fieldScale = 0;
  EventNumber = 0;
  m_selectmode = 0; // initial value is 0(default)
}

//------------------------------------------------------------------------------------------------------

SvxSelectClusters::~SvxSelectClusters() { }

//------------------------------------------------------------------------------------------------------

int SvxSelectClusters::Init(PHCompositeNode *topNode)
{
  EventNumber = 0;
  return EVENT_OK;
}

//----------------------------------------------------------------------------------------------------

int SvxSelectClusters::InitRun(PHCompositeNode *topNode)
{

  if(verbosity>0) cout<<"SvxSelectClusters::InitRun started... " << endl;
  PHNodeIterator iter(topNode);

  PHCompositeNode *dstNode;
  dstNode = static_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));
  if (!dstNode) { cerr << PHWHERE << " DST node missing, doing nothing." << endl; return EVENT_OK; }

  PHCompositeNode* svxNode = dynamic_cast<PHCompositeNode*> (iter.findFirst("PHCompositeNode", "SVX"));
  if (! svxNode) { cerr << PHWHERE << " SVX node missing, doing nothing." << endl; return EVENT_OK; }

  if(verbosity>0) cout<<"SvxSelectClusters::InitRun dreating SvxSelectedClusterList node..." << endl;
  PHIODataNode<PHObject>* SvxSelClusterListNode = NULL;
  SvxSelClusterListNode = (PHIODataNode<PHObject>*)iter.findFirst("PHIODataNode", "SvxSelectedClusterList");
  if (!SvxSelClusterListNode)
    {
      SvxClusterList* svxselclust = new SvxClusterListv5();
      SvxSelClusterListNode = new PHIODataNode<PHObject>(svxselclust, "SvxSelectedClusterList", "PHObject");
      svxNode->addNode(SvxSelClusterListNode);
      if(verbosity>0) {cout << PHWHERE << " SvxSelectedClusterList node created." << endl;} 
    }

  // check magnet current 
  RunHeader* runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader==NULL) { cerr << PHWHERE<< "Can't find runheader. " << endl; return ABORTRUN; }
  if(verbosity>0) cout<<"SvxSelectClusters::InitRun RunHeader node found." << endl;
  if(runheader->get_currentCentral()>0){
   m_fieldScale = 1.0;
  } else {
   m_fieldScale = -1.0;
  }
  if(verbosity>0) cout<<"SvxSelectClusters::InitRun  fieldScale = "<< m_fieldScale << endl;


  cout<<"SvxSelectCluster::"<<__FUNCTION__<<" Window Size"<<endl;
  cout<<"   dZ  =("<<ZCut[0]<<", "<<ZCut[1]<<", "<<ZCut[2]<<")"<<endl;
  cout<<"   dPhi=("<<PhiCut<<")"<<endl;
  cout<<"   select_e ="<<m_select_e<<endl;
  if(m_select_e>0)
    {
      cout<<"   Reduction Z, Phi="<<Z_reduction<<", "<<Phi_reduction<<endl; 
      cout<<"   MomCut ="<<MomCut<<endl;
      cout<<"   n0cut  ="<<m_select_e_n0cut<<endl;
      cout<<"   Epcut  ="<<m_select_e_eopcut<<endl;
    }
  cout<<"   mode = "<<m_selectmode<<endl;
 
  if(verbosity>0) cout<<"SvxSelectClusters::InitRun inished. " << endl;
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

void SvxSelectClusters::calculate_dphidz(
                   float the0, float pt, float ux, float uy, int charge, // track info
                   float svxx, float svxy, float svxz,        // cluster position
                   float xvtx, float yvtx, float zvtx,        // prim. vertex position;
                   float bDir,                                // direction of B-field
                   float* dproj, float* magbend, float* zproj // "output"
                                        )
{

  float dx = svxx - xvtx;
  float dy = svxy - yvtx;
  float rhit_2 = dx*dx+dy*dy;
  float rhit = sqrt(rhit_2);

  // vector product (ux,uy) x (dx, dy)
  // this is signed difference between straight line projection
  // from beam spot (xoffset1,yoffset1) and a svx hit position.
  float u_cross_dx = ux*dy - uy*dx;
  float mag_bend   = -bDir*(0.00135*charge*rhit_2/pt); // 2012.03.16 dir of mag.bend depends on the field direction.

  *dproj   = u_cross_dx;
  *magbend = mag_bend;
  *zproj   = (zvtx + rhit/tan(the0));
}

//---------------------------------------------------------------------------------------------

int SvxSelectClusters::process_event(PHCompositeNode *topNode)
{
  if(verbosity>0) cout << "========== SvxSelectClusters::process_event() started..." <<endl;

  if(m_selectmode == 1) 
  {
    BbcOut *bbcout = findNode::getClass<BbcOut>(topNode,"BbcOut");
    if (bbcout==NULL) { cout << "No BbcOut in the NODE tree" << endl; return EVENT_OK;}

    float bbcqn = bbcout->get_ChargeSum(0);
    float bbcqs = bbcout->get_ChargeSum(1);
    float bbcq  = (isnan(bbcqn) || isnan(bbcqs)) ? -9999 : bbcqn + bbcqs;

    if(bbcq>=200){ // save selected clusters for central events
      if(verbosity>1) cout<<"fillSelectedCluster : bbcq="<<bbcq<<endl;
      fillSelectedClusters(topNode);
    } 
    else {         // save all clusters for all events
      if(verbosity>1) cout<<"fillAllCluster : bbcq="<<bbcq<<endl;
      fillAllClusters(topNode);
    }
  }
  else // 0(default)
  { 
    fillSelectedClusters(topNode);
  }

  if(verbosity>0) cout << "SvxSelectClusters::process_event() finished. " << endl;
  EventNumber++;
  return EVENT_OK;
}

int SvxSelectClusters::fillSelectedClusters(PHCompositeNode *topNode)
{
  SvxClusterList* d_svx = findNode::getClass<SvxClusterList> (topNode, "SvxClusterList");
  if(d_svx==NULL){ cout<<PHWHERE<<"No SvxClusterList in the NODE tree"<<endl; return EVENT_OK; }
  int nclusters = d_svx->get_nClusters();

  SvxClusterList* d_svxsel = findNode::getClass<SvxClusterList> (topNode, "SvxSelectedClusterList");
  if(d_svxsel==NULL){ cout<<PHWHERE<<"No SvxSelectedClusterList in the NODE tree"<<endl; return EVENT_OK; }
  //int nselclusters = d_svxsel->get_nClusters();

  SvxSegmentList* d_svxtrk = findNode::getClass<SvxSegmentList> (topNode, "SvxSegmentList");
  if(d_svxtrk==NULL){ cout<<PHWHERE<<"No SvxSegmentList in the NODE tree"<<endl; return EVENT_OK; }
  int ntracks = d_svxtrk->get_nSegments();

  SvxCentralTrackList* d_svxcentraltrk = findNode::getClass<SvxCentralTrackList> (topNode, "SvxCentralTrackList");
  if(d_svxcentraltrk==NULL){ cout<<PHWHERE<<"No SvxCentralTrackList in the NODE tree"<<endl; return EVENT_OK; }
  int nsvxcentraltracks = d_svxcentraltrk->get_nCentralTracks();

  SvxCentralTrackList* svxcentraltrkbg = findNode::getClass<SvxCentralTrackList> (topNode, "SvxCentralTrackBackList");
  int nsvxcentraltrackbgs = (svxcentraltrkbg!=NULL) ? svxcentraltrkbg->get_nCentralTracks() : 0;

  SvxClusterContainer *clscont = findNode::getClass<SvxClusterContainer>(topNode,"SvxClusterContainer");
  if(clscont==NULL){ cout<<"No ClusterContainer in the NODE tree"<<endl; return EVENT_OK; }
  int nclscont = clscont->get_ncluster();

  PHCentralTrack *trk = findNode::getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if(trk==NULL){ cout<<"No PHCentralTrack in the NODE tree"<<endl; return EVENT_OK; }
  int nphtrk = trk->get_npart();

  VtxOut *vtxout = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if(vtxout==NULL){ cout<<PHWHERE<<"No VtxOut in the NODE tree"<<endl; return EVENT_OK; }

  BbcOut *bbcout = findNode::getClass<BbcOut>(topNode,"BbcOut");
  if (bbcout==NULL) { cout << "No BbcOut in the NODE tree" << endl; return EVENT_OK;}

  float bbcqn = bbcout->get_ChargeSum(0);
  float bbcqs = bbcout->get_ChargeSum(1);
  float bbcq  = (isnan(bbcqn) || isnan(bbcqs)) ? -9999 : bbcqn + bbcqs;

  int icent=0;
  if     (50<=bbcq&&bbcq<200){ icent=1; }
  else if( 0<=bbcq&&bbcq< 50){ icent=2; }


  float xvtx = 0.;
  float yvtx = 0.;
  float zvtx = -9999.;
  if(m_vtxflag==1){ // simulation
    zvtx =  (vtxout->get_Vertex("SIM")).getZ();
  }
  else if(m_vtxflag==2){ // svxprc
    zvtx =  (vtxout->get_Vertex("SVX_PRECISE")).getZ();
  }
  else if(m_vtxflag==3){
    zvtx =  (vtxout->get_Vertex("SVX")).getZ();
  }
  else if(m_vtxflag==4){
    zvtx =  (vtxout->get_Vertex("BBC")).getZ();
  }
  else {
    xvtx =  (vtxout->get_Vertex()).getX();
    yvtx =  (vtxout->get_Vertex()).getY();
    zvtx =  (vtxout->get_Vertex()).getZ();
    string s_vtx = vtxout->which_Vtx();
    if(verbosity>0) cout << "Using " << s_vtx << " Vertex." << endl;
  }
  if(verbosity>0) cout << "Event Vertex = " << xvtx << " " << yvtx << " " << zvtx << " bbcq="<<bbcq<<endl;
  if(verbosity>0) cout << "Using window: " << ZCut[icent] << " " << PhiCut << endl;

  if(verbosity>0) {
    std::cout << "Number of SVX clusters = " << nclusters << std::endl;
    std::cout << "Number of clusters in ClusterContainer = " << nclscont << std::endl;
    std::cout << "Number of SVX standalone tracks = " << ntracks << std::endl;
    std::cout << "Number of SvxCentral tracks = " << nsvxcentraltracks << std::endl;
    std::cout << "Number of PHCentralTracks tracks = " << nphtrk << std::endl;
  }

  // 
  vector<SvxCentralTrack*> vSvxCntTrk(nphtrk, (SvxCentralTrack*)NULL);

  // make a list of unique clusters associated with SvxCentral tracks 
  vector<int> assocclus;
  vector<int> assocclusdup;
  for(int i=0; i<nsvxcentraltracks; i++) {
    SvxCentralTrack* tmp = d_svxcentraltrk->getCentralTrack(i);    
    int svxnhits = tmp->getNhits();
    for(int ih=0; ih<svxnhits; ih++) {
      SvxClusterInfo* clusinfo = tmp->getClusterInfo(ih);
      int clusid = clusinfo->getClusterId();
      assocclusdup.push_back(clusid);
      bool isadded=false;
      for(unsigned int itmp=0; itmp<assocclus.size(); itmp++) {
        if(assocclus[itmp]==clusid) {isadded=true; break;}
      }
      if(!isadded) {assocclus.push_back(clusid); }
    }
    // associate to CNT track 
    int dchid = tmp->getDchIndex();
    if(0<=dchid&&dchid<nphtrk){
      vSvxCntTrk[dchid] = tmp;
    }
  }
  if(verbosity>0) { std::cout << "Number of associated clusters: " << assocclusdup.size() << " " << assocclus.size() << endl; }

  ///////////////////////////////
  // select clustest close to dch tracks
  vector<int> selectedclus;
  vector<int> selectedclusdup;

  for(int itrk=0; itrk<nphtrk; itrk++) {
    PHSnglCentralTrack *track = trk->get_track(itrk);
    if(track==NULL) continue;

    SvxCentralTrack *svxtrk = vSvxCntTrk[itrk];

    float phi0   = track->get_phi0();
    float the0   = track->get_the0();
    find_nearbyhit(track, svxtrk, clscont, phi0, the0, xvtx, yvtx, zvtx, icent, selectedclus, selectedclusdup);
  } // end loop ove CA tracks
  if(verbosity>0) {
    cout<< "    SELECTED " << selectedclus.size() << " " << selectedclusdup.size() << " clusters out of " << nclusters << endl;
  }

  // check that all associated clusters are in selcted cluster list
  int nbad = check_associated_in_selected(d_svx, assocclus, selectedclus);
  if(verbosity>1) {cout << "NBAD = " << nbad << endl;}


  /////////////////////////////////////////
  // add SvxCentralTrackBack (BGTrack)
  // store clusters for only saved tracks
  if(svxcentraltrkbg!=NULL){
    // 
    vector<SvxCentralTrack*> vSvxCntTrkBG(nphtrk, (SvxCentralTrack*)NULL);

    // associated cluster
    vector<int> assocclusbg;
    vector<int> assocclusbgdup;

    for(int i=0; i<nsvxcentraltrackbgs; i++) {
      SvxCentralTrack* tmp = svxcentraltrkbg->getCentralTrack(i);    
      int svxnhits = tmp->getNhits();
      for(int ih=0; ih<svxnhits; ih++) {
        SvxClusterInfo* clusinfo = tmp->getClusterInfo(ih);
        int clusid = clusinfo->getClusterId();
        assocclusbgdup.push_back(clusid);
        bool isadded=false;
        for(unsigned int itmp=0; itmp<assocclusbg.size(); itmp++) {
          if(assocclusbg[itmp]==clusid) {isadded=true; break;}
        }
        if(!isadded) {assocclusbg.push_back(clusid); }
      }
      // associate to CNT track 
      int dchid = tmp->getDchIndex();
      if(0<=dchid&&dchid<nphtrk){
        vSvxCntTrkBG[dchid] = tmp;
      }
    }
    if(verbosity>0) { 
      std::cout << "Number of associated clusters (BG): " 
                << assocclusbgdup.size() << " " << assocclusbg.size() << endl; 
    }

    /////////////////////////////////////
    // search for nearby hit for BG track
    vector<int> selectedclusbg;
    vector<int> selectedclusbgdup;

    for(int itrk=0; itrk<nsvxcentraltrackbgs; itrk++) {
      SvxCentralTrack* tmp = svxcentraltrkbg->getCentralTrack(itrk);    
      int trkid = tmp->getDchIndex();
      PHSnglCentralTrack *track = trk->get_track(trkid);
      if(track==NULL) continue;

      SvxCentralTrack *svxtrk = vSvxCntTrkBG[itrk];

      float phi_new = tmp->getRotatedAngle(0);
      float the_new = tmp->getRotatedAngle(1);

      find_nearbyhit(track, svxtrk, clscont, phi_new, the_new, xvtx, yvtx, zvtx, icent,  
                     selectedclusbg, selectedclusbgdup);
    } // end loop ove CA tracks
    if(verbosity>0) {
     cout << "    SELECTED " << selectedclusbg.size() << " " 
          << selectedclusbgdup.size() << " clusters out of " << nclusters << endl;
    }


    // check that all associated clusters are in selcted cluster list
    int nbadbg = check_associated_in_selected(d_svx, assocclusbg, selectedclusbg);
    if(verbosity>1) {cout << "NBAD_BG = " << nbadbg << endl;}



    // add selectedBG cluster (BG) into selected(FG) cluster list
    int nalready=0;
    for(unsigned int i=0; i<selectedclusbg.size(); i++) {
      int id1 = selectedclusbg[i];
      bool included=false;
      for(unsigned int j=0; j<selectedclus.size(); j++) {
        int id2 = selectedclus[j];
        if(id1==id2) {included=true; break;}
      }
      if(!included) {
        selectedclus.push_back(id1);
      } else {
        nalready++;
      }
    }
    if(verbosity>0) {
     cout << "   TOTAL SELECTED (after BG)" << selectedclus.size() << " " << nalready<<endl;
    }
  }

  ////////////////////////
  // sort with ascending order
  sort(selectedclus.begin(), selectedclus.end());
  //cout<<"After sorting : "<<endl;
  //for(unsigned int i=0; i<selectedclus.size(); i++){
  //  cout<<"after sort : "<<i<<" "<<selectedclus[i]<<endl;
  //}

  ////////////////////////
  // save the associated clusters and all the cluster around the Track
  for(unsigned int i=0; i<selectedclus.size(); i++) {
     int index = selectedclus[i];
     SvxCluster *orig    = d_svx->get_Cluster(index);
     SvxCluster *cluster = d_svxsel->addCluster();
     cluster->set_hitID(     orig->get_hitID());
     cluster->set_svxSection(orig->get_svxSection());
     cluster->set_layer(     orig->get_layer());
     cluster->set_ladder(    orig->get_ladder());
     cluster->set_sensor(    orig->get_sensor());
     cluster->set_sensorType(orig->get_sensorType());
     cluster->set_adc(    0, orig->get_adc(0));
     cluster->set_adc(    1, orig->get_adc(1));
     cluster->set_size(      orig->get_size());
     cluster->set_xz_size(0, orig->get_xz_size(0));
     cluster->set_xz_size(1, orig->get_xz_size(1));
     cluster->set_circumference(orig->get_circumference());
     cluster->set_edgeflag(  orig->get_edgeflag());
     cluster->set_Nhot(      orig->get_Nhot());
     cluster->set_Ncold(     orig->get_Ncold());
     for ( int j=0; j<3; j++ ) {
       cluster->set_xyz_local (j, orig->get_xyz_local(j));
       cluster->set_xyz_global(j, orig->get_xyz_global(j));
     }
     cluster->set_AssociatedStandalone(orig->get_AssociatedStandalone());
     cluster->set_AssociatedCGL(orig->get_AssociatedCGL());

  }

  return EVENT_OK;
}

int SvxSelectClusters::fillAllClusters(PHCompositeNode *topNode)
{
  SvxClusterList* d_svx = findNode::getClass<SvxClusterList> (topNode, "SvxClusterList");
  if(d_svx==NULL){ cout<<PHWHERE<<"No SvxClusterList in the NODE tree"<<endl; return EVENT_OK; }
  int nclusters = d_svx->get_nClusters();

  SvxClusterList* d_svxsel = findNode::getClass<SvxClusterList> (topNode, "SvxSelectedClusterList");
  if(d_svxsel==NULL){ cout<<PHWHERE<<"No SvxSelectedClusterList in the NODE tree"<<endl; return EVENT_OK; }
  //int nselclusters = d_svxsel->get_nClusters();

  if(verbosity>0) {
    std::cout << "Number of SVX clusters = " << nclusters << std::endl;
  }

  ////////////////////////
  // save the associated clusters and all the cluster around the Track
  for(int i=0; i<d_svx->get_nClusters(); i++) {
     SvxCluster *orig    = d_svx->get_Cluster(i);
     SvxCluster *cluster = d_svxsel->addCluster();
     cluster->set_hitID(     orig->get_hitID());
     cluster->set_svxSection(orig->get_svxSection());
     cluster->set_layer(     orig->get_layer());
     cluster->set_ladder(    orig->get_ladder());
     cluster->set_sensor(    orig->get_sensor());
     cluster->set_sensorType(orig->get_sensorType());
     cluster->set_adc(    0, orig->get_adc(0));
     cluster->set_adc(    1, orig->get_adc(1));
     cluster->set_size(      orig->get_size());
     cluster->set_xz_size(0, orig->get_xz_size(0));
     cluster->set_xz_size(1, orig->get_xz_size(1));
     cluster->set_circumference(orig->get_circumference());
     cluster->set_edgeflag(  orig->get_edgeflag());
     cluster->set_Nhot(      orig->get_Nhot());
     cluster->set_Ncold(     orig->get_Ncold());
     for ( int j=0; j<3; j++ ) {
       cluster->set_xyz_local (j, orig->get_xyz_local(j));
       cluster->set_xyz_global(j, orig->get_xyz_global(j));
     }
     cluster->set_AssociatedStandalone(orig->get_AssociatedStandalone());
     cluster->set_AssociatedCGL(orig->get_AssociatedCGL());
  }

  return EVENT_OK;
}

void SvxSelectClusters::find_nearbyhit(
      PHSnglCentralTrack *track, SvxCentralTrack* svxtrk, SvxClusterContainer *clscont,
      float phi, float the, 
      float xvtx, float yvtx, float zvtx,
      int icent,
      std::vector<int>& selectedclus, std::vector<int>& selectedclusdup)
{
  static const float Rsub[8] = {
    2.63,   // radius of sublayer 0
    5.13,   // radius of sublayer 1
    10.365, // radius of sublayer 2
    11.765, // radius of sublayer 3
    12.845, // radius of sublayer 4
    15.475, // radius of sublayer 5
    16.687, // radius of sublayer 6
    17.905  // radius of sublayer 7
  };

  // this variables are used for rough cut
  static float dphi = 0.2; // radians
  static float dz   = 4.0; // cm


  float mom    = track->get_mom();
  int   charge = track->get_charge();
  float phi0   = phi;
  float the0   = the;
  int   n0     = track->get_n0();
  float emce   = track->get_emce();

  // window size
  float useZCut   = ZCut[icent];
  float usePhiCut = PhiCut;
  bool  isReduced = false;
  if(m_select_e>0) // if requested, reduce search window for non-electrons 
    { 
      if(mom<MomCut || n0<m_select_e_n0cut || emce/mom<m_select_e_eopcut ) 
        { 
          useZCut   = ZCut[icent]/Z_reduction; 
          usePhiCut = PhiCut     /Phi_reduction; 
          isReduced = true;
        }
    }
  if(verbosity>2) { 
    cout<<"Window Z="<<useZCut <<", Phi="<<usePhiCut<<", icent:"<<icent<<" ";
    cout<<( isReduced ? "Reduced" : "Normal" )<<endl; 
  }

  float pt = mom*sin(the0);
  float ux = cos(phi0);
  float uy = sin(phi0);

  //cout << " ########## track: " << itrk << " " << mom << " " << charge << " ";
  //cout << phi0 << " " << the0 << " " << Rsub[0]/tan(the0)+zvtx << " ";
  //cout << Rsub[7]/tan(the0)+zvtx << endl;
  //
  // associated cluster
  SvxClusterInfo *cls_associated[8] = {NULL, NULL, NULL, NULL, NULL, NULL, NULL, NULL};
  if(svxtrk!=NULL){
    for(int ihit=0; ihit<svxtrk->getNhits(); ihit++){
      SvxClusterInfo *info = svxtrk->getClusterInfo(ihit);
      int sublayer = info->get_sublayer();
      if(sublayer>0){
        cls_associated[sublayer] = info;
      }
    }
  }

  for(int isl=0; isl<8; isl++) {  // eight sublayers
    float phi0_trk = phi0;
    float z0_trk   = Rsub[isl]/tan(the0)+zvtx;

    // get info from associated cluster
    float dproj_ref=0., magbend_ref=0., zproj_ref=0., svxz_ref=0.;
    if(cls_associated[isl]!=NULL){
      dproj_ref    = cls_associated[isl]->getdproj();
      magbend_ref  = cls_associated[isl]->getbend();
      zproj_ref    = cls_associated[isl]->getzproj();
      svxz_ref     = cls_associated[isl]->getPosition(2);
      float svxx_ref = cls_associated[isl]->getPosition(0);
      float svxy_ref = cls_associated[isl]->getPosition(1);

      phi0_trk  = atan2(svxy_ref-yvtx, svxx_ref-xvtx);
      z0_trk    = svxz_ref;
    }
    float dphi_ref = (dproj_ref + magbend_ref);
    float dz_ref   = (zproj_ref - svxz_ref);
    if(verbosity>1) { 
      cout<<"isub="<<isl<<" "<<(cls_associated[isl]!=NULL?"Exist":"NULL");
      cout<<" dphi, dz="<<dphi_ref<<" "<<dz_ref<<endl;
    }


    vector<SvxCluster*> vcluster;
    clscont->find_clusters(vcluster, isl, phi0_trk, dphi, z0_trk, dz); // make a very loose selection

    for(unsigned int k=0; k<vcluster.size(); k++) {
      int   clid = (vcluster[k])->get_hitID();
      float svxx = (vcluster[k])->get_xyz_global(0);
      float svxy = (vcluster[k])->get_xyz_global(1);
      float svxz = (vcluster[k])->get_xyz_global(2);

      float dproj,magbend,zproj;
      calculate_dphidz(the0, pt, ux, uy, charge, // track info
                 svxx, svxy, svxz,        // cluster position
                 xvtx, yvtx, zvtx,        // prim. vertex position;
                 m_fieldScale,                   // direction of B-field
                 &dproj, &magbend, &zproj);

      float dphi_cls = (dproj + magbend);
      float dz_cls   = (zproj - svxz);
      //cout << "      Matching: " <<isl << " " << z0_trk << " " << phi0_trk << " :   " << zproj << " "
      //       << dproj+magbend << " " << svxx << " " << svxy << " " << svxz << " " << zvtx << " " << clid << endl;

      if(verbosity>1) cout<<" dphi, dz="<<(dphi_cls-dphi_ref)<<" "<<dz_cls-dz_ref;

      if(fabs(dphi_cls-dphi_ref)<usePhiCut && fabs(dz_cls-dz_ref)<useZCut) {
        if(verbosity>1) cout<<" OK";

        selectedclusdup.push_back(clid);
        bool isadded=false;
        for(unsigned int itmp=0; itmp<selectedclus.size(); itmp++) {
          if(selectedclus[itmp]==clid) {isadded=true; break;}
        }
        if(!isadded) { selectedclus.push_back(clid); if(verbosity>1) cout<<" Added"; }

      }
      if(verbosity>1) cout<<endl;
    }
  }
  //cout << " Total: " << selectedclus.size() << " " << selectedclusdup.size() << endl;

}

int SvxSelectClusters::check_associated_in_selected(
    SvxClusterList* d_svx,
    std::vector<int>& assocclus, std::vector<int>& selectedclus)
{
  int nbad=0;
  for(unsigned int i=0; i<assocclus.size(); i++) {
    int id1 = assocclus[i];
    SvxCluster *tmp1 =  d_svx->get_Cluster(id1);
    int layer = tmp1->get_layer();
    bool included=false;
    for(unsigned int j=0; j<selectedclus.size(); j++) {
      int id2 = selectedclus[j];
      if(id1==id2) {included=true; break;}
    }
    if(!included) {
      nbad++; 
      if(verbosity>2) {cout << "BAD: cluster " << id1 << " not included in selected list. " << layer << endl;}
      selectedclus.push_back(id1);
    }
  }
  if(verbosity>0) {
   cout << "   TOTAL SELECTED " << selectedclus.size() << endl;
  }

  return nbad;
}


//---------------------------------------------------------------------------------------------

int SvxSelectClusters::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

