#include "SvxEfficiencyQA.h"
#include "SvxQADefs.h"

#include <phool.h>
#include <PHCompositeNode.h>
#include <Fun4AllServer.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>
#include <TrigLvl1.h>
#include <TriggerHelper.h>

#include <EventHeader.h>
#include <RunHeader.h>
#include <PreviousEvent.h>
#include <SvxRawhitList.h>
#include <SvxRawhit.h>
#include <SvxClusterList.h>
#include <SvxCluster.h>
#include <svxAddress.hh>
#include <SvxRawhitClusterList.h>
#include <SvxRawhitCluster.h>
#include <svxDetectorGeo.hh>
#include <SvxSensor.h>
#include <PHCentralTrack.h>
#include <PHGlobal.h>
#include <VtxOut.h>
#include <PHPoint.h>
#include <SvxComponentGeom.h>

#include <TFile.h>
#include <TH1.h>
#include <TH2.h>
#include <TBenchmark.h>
#include <TAxis.h>
#include <TMath.h>

#include <iostream>
#include <cmath>
#include <gsl/gsl_rng.h>
#include <vector>

using namespace std;
using namespace findNode;

SvxProjection::SvxProjection()
{
  for(int i=0;i<4;i++){
    ladder[i]=-1;
    sensor[i]=-1;
    chipSS[i]=-1;
    tile[i]=-1;
  }
}



SvxEfficiencyQA::SvxEfficiencyQA():
    SubsysReco("SVXEFFICIENCYQA"),
    d_eventhead(NULL),
    d_vtxout(NULL),
    d_peve(NULL),
    d_cnt(NULL),
    d_global(NULL),
    d_svxadr(NULL),
    d_svxraw(NULL),
    d_svxcls(NULL),
    d_trg(NULL),
    d_svxrawcls(NULL),
    d_svxgeo(NULL),
    m_svxCompGeom(0),
    m_bbczcut(10), //bbcz cut
    m_bbcqcut(800),//bbc charge cut
    timer(PHTimeServer::get()->insert_new("SvxEfficiencyQA"))
{
  is_usemodeler = true;
  is_useinternalproj = false;
  is_tickcut = true;
  m_pc3dphi_emcdphioffset = -0.0003;
  m_pc3dphi_emcdphicut = 0.02;
  m_pc3dz_emcdzoffset = 0.2;
  m_pc3dz_emcdzcut = 5.0;
  m_dproj_cut = 0.4;
  m_zproj_cut = 5.0;
}


int SvxEfficiencyQA::Init(PHCompositeNode *topNode)
{
  m_EventNumber = 0;
  m_EventSeqNumber = 0;


  Init_Hists();

  return EVENT_OK;
}

//==============================================================
int SvxEfficiencyQA::InitRun(PHCompositeNode *topNode)
{

  RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader == NULL) {
    cout << PHWHERE << "Can't find RunHeader. " << endl;
    return ABORTRUN;
  }

  if (runheader->get_currentCentral() > 0) {
    m_fieldScale = 1.0;
  } else {
    m_fieldScale = -1.0;
  }
  d_svxgeo = findNode::getClass<svxDetectorGeo>(topNode,"svxDetectorGeo");
  if (d_svxgeo == NULL) {
    cerr << PHWHERE << "Can't find svxDetectorGeo " << endl;
    cerr << PHWHERE << "No point in continuing "  << endl;
    return ABORTRUN;
  }

  Init_Hists();
  Create_chipGeometry();
  Create_tileGeometry();
//  Print_chipGeometry();
   
  if(is_usemodeler){
    // Build model of active volumes for track pointing / deadmap integration
    if (!m_svxCompGeom)
    m_svxCompGeom = new SvxComponentGeom(d_svxgeo);
    m_svxCompGeom->SetVerbosity(0);
  }

  return EVENT_OK;
}
//==============================================================
int SvxEfficiencyQA::process_event(PHCompositeNode *topNode)
{

  timer.get()->restart();
  GetNodes(topNode);

  if (m_EventNumber % 1000 == 0 && verbosity > 0) {
    cout << "SvxEfficiencyQA::process_event() event no: " << m_EventNumber << endl;
  }
  m_EventNumber++;

  float zbbc = d_vtxout->get_ZVertex("BBC");
//  m_hzbbc->Fill(zbbc);
//  m_hzbbc_vs_evtseq->Fill(m_EventSeqNumber, zbbc);
  float bbcq = d_global->getBbcChargeN() + d_global->getBbcChargeS();
  if (!EventSelection(topNode, zbbc, bbcq)) return EVENT_OK;

  PHPoint verpointsvxprim = d_vtxout->get_Vertex("SVX_PRECISE");
  float svxprim[3] = { -9999};
  svxprim[0] = verpointsvxprim.getX();
  svxprim[1] = verpointsvxprim.getY();
  svxprim[2] = verpointsvxprim.getZ();

  m_EventSeqNumber = d_eventhead->get_EvtSequence();

//  m_hzbbc_vs_evtseq_selection->Fill(m_EventSeqNumber, zbbc);

  //////////////////////
  int nclus    = (d_svxcls != NULL)    ? d_svxcls->get_nClusters() : 0;
 
  int gl_ntrk = 0;
  (d_cnt != NULL) ? gl_ntrk = d_cnt->get_npart() : 0;

  //fill the all clusters
  int nclus_counter[4][24][6][4];
  memset(nclus_counter,0,sizeof(nclus_counter));

  for (int ihit = 0; ihit < nclus; ihit++) {

 //   int size, pos;
    SvxCluster *cls = d_svxcls->get_Cluster(ihit);

    if (cls->get_svxSection() != 0) continue; // require pixel and strip
    
    //require the cluster is associated to CNT
    int cnt_index = Get_AssociatedID(ihit);
    int cls_layer = cls->get_layer();
    int cls_ladder = cls->get_ladder();
    int cls_sensor = cls->get_sensor();
    float cls_lsvxx = cls->get_xyz_local(0);
    float cls_lsvxz = cls->get_xyz_local(2);
    int sensor_chip = Get_chipSS(cls_layer,cls_lsvxz);
    //tempolary 
    nclus_counter[cls_layer][cls_ladder][cls_sensor][sensor_chip]++;
    
    float chipSSlocalz = Get_chipSSlocalz(cls_layer,sensor_chip,cls_lsvxz);

    if (cls_layer == 0) {
    //  pixel_map[cls_layer][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz,cls_lsvxx);
      if (cnt_index>-1) pixel_map_cnt[cls_layer][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz,cls_lsvxx);
      if (  (verbosity > 100)) {
        cout << "cnt index" << cnt_index << endl;
        cout << "eventnum" << m_EventNumber << endl;
        cout << "layer " << cls_layer << endl;
        cout << "ladder" << cls_ladder << endl;
        cout << "sensor" << cls_sensor << endl;
        cout << "chip  " << sensor_chip << endl;
      }
    } else if (cls_layer == 1) {
   //   pixel_map[cls_layer][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz, cls_lsvxx);
      if (cnt_index>-1) pixel_map_cnt[cls_layer][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz, cls_lsvxx);

    } else { // strip
      if (cls_layer == 2) {
    //    strip_map[cls_layer-2][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz,cls_lsvxx);
        if(cnt_index>-1) strip_map_cnt[cls_layer-2][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz,cls_lsvxx);
      } else if (cls_layer == 3) {
    //    strip_map[cls_layer-2][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz,cls_lsvxx);
        if(cnt_index>-1) strip_map_cnt[cls_layer-2][cls_ladder][cls_sensor][sensor_chip]->Fill(chipSSlocalz,cls_lsvxx);
      }
    }

  }//nclus

  //check bbc vs ncluster
  const int nladder[4]={10,20,16,24};
  const int nsensor[4]={4,4,5,6};
  const int nchipSS[4]={4,4,2,2};
  for(int ilr=0;ilr<4;ilr++){
    for(int ild=0;ild<nladder[ilr];ild++){
      for(int isn=0;isn<nsensor[ilr];isn++){
        for(int icpSS=0;icpSS<nchipSS[ilr];icpSS++){
          if(ilr<2) pixel_bbcq_vs_nhit[ilr][ild][isn][icpSS]->Fill(bbcq,nclus_counter[ilr][ild][isn][icpSS]);
          else      strip_bbcq_vs_nhit[ilr-2][ild][isn][icpSS]->Fill(bbcq,nclus_counter[ilr][ild][isn][icpSS]); 
        }
      }
    }
  }
  
  if(is_useinternalproj){
    for(int itrk = 0; itrk < gl_ntrk; itrk++) {
      if(CNTTrackSelection(d_cnt,itrk)){ 
      float mom    = d_cnt->get_mom(itrk);
      int   charge = d_cnt->get_charge(itrk);
      float the0   = d_cnt->get_the0(itrk);
      float phi0   = d_cnt->get_phi0(itrk); 
      if(phi0<-M_PI/2.0) phi0+=2.0*M_PI;
      float pt     = fabs(mom*sin(the0));
           
      Search_CNTProjectedChip(svxprim[0],svxprim[1],svxprim[2],pt,phi0,the0,charge,m_fieldScale);
      }
    }
  }

  if(is_usemodeler){
    vector<ScgTrack> geoTracks;
    for(int itrk=0;itrk<gl_ntrk;itrk++){
      if(!CNTTrackSelection(d_cnt,itrk)) continue;
      float mom    = d_cnt->get_mom(itrk);
      int   charge = d_cnt->get_charge(itrk);
      float the0   = d_cnt->get_the0(itrk);
      float phi0   = d_cnt->get_phi0(itrk); 

      ScgTrack geoTrack = 
        m_svxCompGeom->FindHitsFromVertex(svxprim[0], svxprim[1], svxprim[2],
            mom, phi0, the0, 
            charge, m_fieldScale*0.90);//0.90 is field strength (tesla)

      geoTracks.push_back(geoTrack);
      for (int ihit=0; ihit<geoTrack.nhits; ihit++) {
        ScgHit hit = geoTrack.GetHit(ihit);
        //hit.component= m_svxCompGeom->GetNearestSensorSegment(*hit.x,*hit.y,*hit.z);

        if (0)
          Printf("   xyz: (%#6.2f, %#6.2f, %#6.2f) "
              "address: %d %d %d %d %d \tstatus %d",
              hit.x, hit.y, hit.z,
              hit.layer, hit.ladder, hit.sensor, hit.component,hit.tile,
              hit.status);

        //component = ROC (Pixel)
        //          = readout 0 or 1 (Strip)
        /*
        if(hit.layer==0 && hit.ladder==0 && hit.sensor==1) h2_check_modeler_B0L0S1->Fill(hit.component,hit.z);
        if(hit.layer==0 && hit.ladder==5 && hit.sensor==1) h2_check_modeler_B0L5S1->Fill(hit.component,hit.z);
        if(hit.layer==2 && hit.ladder==3 && hit.sensor==2) h2_check_modeler_B2L3S2->Fill(hit.component,hit.z);
        if(hit.layer==2 && hit.ladder==10 && hit.sensor==2) h2_check_modeler_B2L10S2->Fill(hit.component,hit.z);
        if(hit.layer==3 && hit.ladder==3 && hit.sensor==2) h2_check_modeler_B3L3S2->Fill(hit.component,hit.z);
        if(hit.layer==3 && hit.ladder==20 && hit.sensor==2) h2_check_modeler_B3L20S2->Fill(hit.component,hit.z);
        */
        if(hit.layer>-1 && hit.ladder>-1 && hit.sensor>-1 && hit.component>-1 ){             
          int chipSS=-1;
          if(hit.layer<2) chipSS = ComponentToPixelROC(hit.layer,hit.ladder,hit.sensor,hit.component);
          else            chipSS = ComponentToStripSS(hit.layer,hit.ladder,hit.sensor,hit.component);

          float project_phi = atan2(hit.y,hit.x);  
          if(project_phi<-M_PI/2.0) project_phi+=2.0*M_PI;
          h2_projected_zphi[hit.layer]->Fill(hit.z,project_phi);
          float cx =  m_chip[hit.layer][hit.ladder][hit.sensor][chipSS].transvector_corr[0];
          float cy =  m_chip[hit.layer][hit.ladder][hit.sensor][chipSS].transvector_corr[1];
          float cz =  m_chip[hit.layer][hit.ladder][hit.sensor][chipSS].transvector_corr[2];
          float cphi = atan2(cy,cx);
          if(cphi<-M_PI/2.0) cphi += 2.0*M_PI;
          float dphi_angle = cphi - project_phi;
          float dphi = dphi_angle*sqrt(cx*cx+cy*cy);
          float dz   = cz - hit.z;
          if(hit.layer <2 ){
            h1_dz_pixelchip_pro_modeler[hit.layer][hit.ladder][hit.sensor][chipSS]->Fill(dz);
            h1_dphi_pixelchip_pro_modeler[hit.layer][hit.ladder][hit.sensor][chipSS]->Fill(dphi);
          }else{
            h1_dz_stripss_pro_modeler[hit.layer-2][hit.ladder][hit.sensor][chipSS]->Fill(dz);
            h1_dphi_stripss_pro_modeler[hit.layer-2][hit.ladder][hit.sensor][chipSS]->Fill(dphi);
          }
        }//
      }//inhits
    }//itrk
  }
  timer.get()->stop();

  return EVENT_OK;
}
//==============================================================

//----Classes needed----------------------------------------

int SvxEfficiencyQA::GetNodes(PHCompositeNode *topNode)
{
  //-------------------EventHeader----------------------------------
  d_eventhead = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (d_eventhead == NULL) {
    cerr << PHWHERE << " EventHeader node not found." << endl;
    return DISCARDEVENT;
  }
  //------------------------------------------------------------------

  //------------------PreviousEvent--------------------------------
  d_peve    = getClass<PreviousEvent>(topNode, "PreviousEvent"); // for Tick cut
  if (d_peve == NULL) {
    cerr << PHWHERE << " No PreviousEvent object !" << endl;
    return DISCARDEVENT;
  }
  //-------------------------------------------------------------------

  //------------------Vertex------------------------------
  d_vtxout = findNode::getClass<VtxOut>(topNode, "VtxOut");
  if (d_vtxout == NULL) {
    cerr << "VtxOut node not found." << endl;
    return DISCARDEVENT;
  }
  //-----------------------------------------------------

  //----------------SvxRawhitList-------------------------
  d_svxraw = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");
  if (d_svxraw == NULL) {
    cerr << "SvxRawhit node not found." << endl;
    return DISCARDEVENT;
  }
  //------------------------------------------------------

  //----------------SvxClusterList-------------------------
  d_svxcls = findNode::getClass<SvxClusterList>(topNode, "SvxClusterList");
  if (d_svxcls == NULL) {
    cerr << "SvxClusterList node not found.Register SvxReco module" << endl;
    return DISCARDEVENT;
  }
  //------------------------------------------------------

  //----------------SvxClusterList-------------------------
  d_svxrawcls = findNode::getClass<SvxRawhitClusterList>(topNode, "SvxRawhitClusterList");
  if (d_svxrawcls == NULL) {
    cerr << "SvxRawhitClusterList node not found.Register SvxReco module" << endl;
    return DISCARDEVENT;
  }
  //------------------------------------------------------

  //----------------svxAddress------------------------------
  d_svxadr = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if (d_svxadr == NULL) {
    cerr << "svxAddress node not found." << endl;
    return DISCARDEVENT;
  }

  //----------------PHCentralTrack------------------------------
  d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if (d_cnt == NULL) {
    cerr << "PHCentralTrack node not found." << endl;
    return DISCARDEVENT;
  }

  d_global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (d_global == NULL) {
    cerr << "PHGlobal node not found."  << endl;
    return DISCARDEVENT;
  }


  return 0;
}

//==============================================================

int SvxEfficiencyQA::End(PHCompositeNode *topNode)
{

  TFile *file = new TFile(outname.c_str(), "RECREATE");
  
  const unsigned int nladder[4] = {10, 20, 16, 24};
  const unsigned int nsensor[4] = {4, 4, 5 ,6};
  for (unsigned int ilr = 0; ilr < 2; ilr++) {
    for (unsigned int ild = 0; ild < nladder[ilr]; ild++) {
      for (unsigned int isn = 0; isn < 4; isn++) {
     //   h1_dz_pixelsensor_pro[ilr][ild][isn]->Write();
     //   h1_dphi_pixelsensor_pro[ilr][ild][isn]->Write();
        for (int ichp = 0; ichp < 4; ichp++) {
    //      pixel_map[ilr][ild][isn][ichp]->Write();
          pixel_map_cnt[ilr][ild][isn][ichp]->Write();
          h1_dz_pixelchip[ilr][ild][isn][ichp]->Write();
          h1_dphi_pixelchip[ilr][ild][isn][ichp]->Write();
          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]->Write();
          if(is_useinternalproj){
            h1_dz_pixelchip_pro[ilr][ild][isn][ichp]->Write();
            h1_dphi_pixelchip_pro[ilr][ild][isn][ichp]->Write();
          }
          if(is_usemodeler){
            h1_dz_pixelchip_pro_modeler[ilr][ild][isn][ichp]->Write();
            h1_dphi_pixelchip_pro_modeler[ilr][ild][isn][ichp]->Write();
          }
        }
      }
    }
  }
  for (unsigned int istriplr = 0; istriplr < 2; istriplr++) {
    for (unsigned int ild = 0; ild < nladder[istriplr+2]; ild++) {
      for (unsigned int isn = 0; isn < nsensor[istriplr+2]; isn++) {
        for (unsigned int iSS = 0; iSS < 2; iSS++) {
      //    strip_map[istriplr][ild][isn][iSS]->Write();
          strip_map_cnt[istriplr][ild][isn][iSS]->Write();
          h1_dz_stripss[istriplr][ild][isn][iSS]->Write();
          h1_dphi_stripss[istriplr][ild][isn][iSS]->Write();
          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]->Write();
          if(is_useinternalproj){
            h1_dz_stripss_pro[istriplr][ild][isn][iSS]->Write();
            h1_dphi_stripss_pro[istriplr][ild][isn][iSS]->Write();
          }
          if(is_usemodeler){
            h1_dz_stripss_pro_modeler[istriplr][ild][isn][iSS]->Write();
            h1_dphi_stripss_pro_modeler[istriplr][ild][isn][iSS]->Write();
          }
        }
      }
    }
  }

  for (unsigned int ilr = 0; ilr < 4; ilr++) {
    h2_cntcluster_zphi[ilr]->Write(); 
    h2_projected_zphi[ilr]->Write(); 
  }

  h1_projected_pixel->Write();
  h1_projected_strip->Write();
  h2_cntzedphi0->Write();
  h2_cntzedphi->Write();
  h1_noprojection_layer->Write();

  h2_check_modeler_B0L0S1->Write();
  h2_check_modeler_B0L5S1->Write();
  h2_check_modeler_B2L3S2->Write();
  h2_check_modeler_B2L10S2->Write();
  h2_check_modeler_B3L3S2->Write();
  h2_check_modeler_B3L20S2->Write();
  
  file->Close();
  return 0;
}

bool SvxEfficiencyQA::TickCut()
{
  return ((50 < m_pticks[0] && m_pticks[0] < 120) ||
          (700 < m_pticks[1] && m_pticks[1] < 780));
}


//event selection
bool SvxEfficiencyQA::EventSelection(PHCompositeNode *topNode, float zbbc, float bbcq)
{

  //pixel tick cut
  if (is_tickcut) {
    for (int i = 0; i < 3; i++) m_pticks[i] = (d_peve != NULL) ? d_peve->get_clockticks(i) : -999;
    if (TickCut() != EVENT_OK) return false;
  }
  //bbcz vertex cut
  if (fabs(zbbc) > m_bbczcut) return false;
  if (bbcq > m_bbcqcut) return false;

  if(!d_vtxout->isVtx("SVX_PRECISE")) return false;
  PHPoint verpointsvxprim = d_vtxout->get_Vertex("SVX_PRECISE");
  float vx = verpointsvxprim.getX();
  float vy = verpointsvxprim.getY();
  float vz = verpointsvxprim.getZ();

  if (TMath::IsNaN(vx) || TMath::IsNaN(vy) || TMath::IsNaN(vz)) {
    cout << PHWHERE << "NaN value is provided to primary vertex. " << endl;
    cout << "vtx name: " << d_vtxout->which_Vtx() << endl;
    return false;
  }

  //trigger selection
  TriggerHelper d_trghelp(topNode);
  bool isL1     = d_trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes)");   //BBCL1
  bool isnarrow = d_trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx");   //BBCL1 narrow
  bool isnarrow_copyA = d_trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx Copy A");   // BBCL1 narrow A
  bool isnarrow_copyB = d_trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) narrowvtx Copy B");   // BBCL1 narrow B
  bool isnovtx  = d_trghelp.didLevel1TriggerGetScaled("BBCLL1(>1 tubes) novertex");   // BBCL1 novertex

  if (isL1 || isnarrow || isnarrow_copyA || isnarrow_copyB || isnovtx) {
    //OK. This is MB event
    return true;
  } else {
    return false;
  }

}

void SvxEfficiencyQA::Init_Hists()
{
  const int nladder[4] = {10, 20, 16, 24};
  const int nsensor[4] = {4, 4, 5, 6};
  for (int ilr = 0; ilr < 4; ilr++) {
    h2_cntcluster_zphi[ilr] = new TH2F(Form("h2_cntcluster_zphi_%d",ilr),Form("CNT associated cluster position at layer %d",ilr),400,-20,20,700,-2,5);
    h2_cntcluster_zphi[ilr]->SetXTitle("z (cm)");
    h2_cntcluster_zphi[ilr]->SetYTitle("phi (cm)");
    h2_projected_zphi[ilr] = new TH2F(Form("h2_projected_zphi_%d",ilr),Form("CNT projected point layer %d",ilr),400,-20,20,700,-2,5);
    h2_projected_zphi[ilr] -> SetXTitle("z (cm)");
    h2_projected_zphi[ilr] -> SetYTitle("phi (cm)");
  }
  h2_cntzedphi0 = new TH2F("h2_cntzedphi0","h2_cntzedphi0",400,-100,100,700,-2,5);
  h2_cntzedphi0->SetXTitle("zed(cm)");
  h2_cntzedphi0->SetYTitle("phi0(cm)");
  h2_cntzedphi = new TH2F("h2_cntzedphi","h2_cntzedphi",400,-100,100,700,-2,5);
  h2_cntzedphi->SetXTitle("zed(cm)");
  h2_cntzedphi->SetYTitle("phi(cm)");

  for (int ilr = 0; ilr < 2; ilr++) {
    for (int ild = 0; ild < nladder[ilr]; ild++) {
      for (int isn = 0; isn < 4; isn++) {
      //  h1_dz_pixelsensor_pro[ilr][ild][isn]
      //    = new TH1F(Form("h1_dz_pixelsensor_pro_B%dL%dS%d",ilr,ild,isn),Form("Pixel sensor-dz Layer %d Ladder%d Sensor%d", ilr ,ild ,isn),200,-8.0,8.0);
    //    h1_dz_pixelsensor_pro[ilr][ild][isn]->SetXTitle("dz(cm)");
     //   h1_dphi_pixelsensor_pro[ilr][ild][isn]
     //     = new TH1F(Form("h1_dphi_pixelsensor_pro_B%dL%dS%d",ilr,ild,isn),Form("Pixel sensor-dphi Layer %d Ladder%d Sensor%d", ilr ,ild ,isn),1400,-1.4,1.4);
     //   h1_dphi_pixelsensor_pro[ilr][ild][isn]->SetXTitle("dphi(cm)");
        for (int ichp = 0; ichp < 4; ichp++) {
          int ROCno = ichp + (isn % 2) * 4;//ROCno 0-8
        //  pixel_map[ilr][ild][isn][ichp]
        //  = new TH2F(Form("pixel_chipmap_B%dL%dS%dC%d",ilr,ild,isn,ichp), Form("Pixel Map Chip Layer B%d Ladder%d Sensor%d chip%d", ilr, ild, isn, ROCno), 64, 0.0, 1.44, 512, -0.64,0.64);
          pixel_map_cnt[ilr][ild][isn][ichp]
          = new TH2F(Form("pixel_chipmap_cnt_B%dL%dS%dC%d",ilr,ild,isn,ichp), Form("Pixel Map Chip Layer B%d Ladder%d Sensor%d chip%d", ilr, ild, isn, ROCno), 64, 0.0, 1.44, 512, -0.64, 0.64);
          pixel_map_cnt[ilr][ild][isn][ichp]->SetXTitle("local z(cm)");
          pixel_map_cnt[ilr][ild][isn][ichp]->SetYTitle("local x(cm)");

          h1_dz_pixelchip[ilr][ild][isn][ichp]
          = new TH1F(Form("h1_dz_pixelchip_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("Pixel dz Layer %d Ladder%d Sensor%d chip%d", ilr ,ild ,isn,ROCno),200,-1.0,1.0);
          h1_dz_pixelchip[ilr][ild][isn][ichp]->SetXTitle("dz(cm)");
         
          h1_dphi_pixelchip[ilr][ild][isn][ichp]
          = new TH1F(Form("h1_dphi_pixelchip_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("Pixel dphi Layer %d Ladder%d Sensor%d chip%d", ilr ,ild ,isn,ROCno),1400,-0.7,0.7);
          h1_dphi_pixelchip[ilr][ild][isn][ichp]->SetXTitle("dphi(cm)");
         
          if(is_useinternalproj){
            h1_dz_pixelchip_pro[ilr][ild][isn][ichp]
            = new TH1F(Form("h1_dz_pixelchip_pro_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("Pixel Chip-dz Layer %d Ladder%d Sensor%d chip%d", ilr ,ild ,isn,ROCno),200,-2.0,2.0);
            h1_dz_pixelchip_pro[ilr][ild][isn][ichp]->SetXTitle("dz(cm)");
          
            h1_dphi_pixelchip_pro[ilr][ild][isn][ichp]
            = new TH1F(Form("h1_dphi_pixelchip_pro_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("Pixel Chip-dphi Layer %d Ladder%d Sensor%d chip%d", ilr ,ild ,isn,ROCno),1400,-1.4,1.4);
            h1_dphi_pixelchip_pro[ilr][ild][isn][ichp]->SetXTitle("dphi(cm)");
          }
          
          if(is_usemodeler){
            h1_dz_pixelchip_pro_modeler[ilr][ild][isn][ichp]
            = new TH1F(Form("h1_dz_pixelchip_pro_modeler_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("Pixel Chip-dz Layer %d Ladder%d Sensor%d chip%d", ilr ,ild ,isn,ROCno),200,-2.0,2.0);
            h1_dz_pixelchip_pro_modeler[ilr][ild][isn][ichp]->SetXTitle("dz(cm)");
           
            h1_dphi_pixelchip_pro_modeler[ilr][ild][isn][ichp]
            = new TH1F(Form("h1_dphi_pixelchip_pro_modeler_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("Pixel Chip-dphi Layer %d Ladder%d Sensor%d chip%d", ilr ,ild ,isn,ROCno),1400,-1.4,1.4);
            h1_dphi_pixelchip_pro_modeler[ilr][ild][isn][ichp]->SetXTitle("dphi(cm)");
          }

          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]
          = new TH2I(Form("h2_bbcq_vs_nhit_B%dL%dS%dC%d",ilr,ild,isn,ichp),Form("pixel bbccharge vs nhit B%dL%dS%dC%d",ilr,ild,isn,ichp),50,0,1000,100,0,100);
          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]->SetXTitle("bbc charge");
          pixel_bbcq_vs_nhit[ilr][ild][isn][ichp]->SetYTitle("ncluster");
        }
      }
    }
  }

  for (int istriplr = 0; istriplr < 2; istriplr++) {
    for (int ild = 0; ild < nladder[istriplr+2]; ild++) {
      for (int isn = 0; isn < nsensor[istriplr+2]; isn++) {
        for (int iSS = 0; iSS < 2; iSS++) {
          int index_section = istriplr * nladder[istriplr+1] * nsensor[istriplr+1] * 2
                              + ild * nsensor[istriplr+2] * 2
                              + isn * 2
                              + iSS;
          if(index_section > 5*16*5*2+6*24*6*2) cout << PHWHERE << "something is wrong " << endl;
       //   strip_map[istriplr][ild][isn][iSS]
       //     = new TH2F(Form("strip_SSmap_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS), Form("Stripixel Map B%d Ladder%d Sensor%d Section%d", istriplr + 2, ild, isn, iSS), 300, 0.0, 3.00, 300, -1.536, 1.536);
          strip_map_cnt[istriplr][ild][isn][iSS]
          = new TH2F(Form("strip_SSmap_cnt_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS), Form("Stripixel Map B%d Ladder%d Sensor%d Section%d", istriplr + 2, ild, isn, iSS), 300, 0.0, 3.00, 300, -1.536, 1.536);
          strip_map_cnt[istriplr][ild][isn][iSS]->SetXTitle("local z(cm)");
          strip_map_cnt[istriplr][ild][isn][iSS]->SetYTitle("local x(cm)");
          
          h1_dz_stripss[istriplr][ild][isn][iSS]
          = new TH1F(Form("h1_dz_stripss_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS),Form("Stripixel dz B%d Ladder%d Sensor%d Section%d", istriplr+2,ild,isn,iSS),300,-1.5,1.5);
          h1_dz_stripss[istriplr][ild][isn][iSS]->SetXTitle("dz(cm)");
         
          h1_dphi_stripss[istriplr][ild][isn][iSS]
          = new TH1F(Form("h1_dphi_stripss_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS),Form("Stripixel dphi B%d Ladder%d Sensor%d Section%d", istriplr+2, ild, isn, iSS),2800,-1.4,1.4);
          h1_dphi_stripss[istriplr][ild][isn][iSS]->SetXTitle("dphi(cm)");

          if(is_useinternalproj){
            h1_dz_stripss_pro[istriplr][ild][isn][iSS]
            = new TH1F(Form("h1_dz_stripss_pro_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS),Form("Stripixel SS-dz B%d Ladder%d Sensor%d Section%d", istriplr+2,ild,isn,iSS),300,-3.0,3.0);
            h1_dz_stripss_pro[istriplr][ild][isn][iSS]->SetXTitle("dz(cm)");
           
            h1_dphi_stripss_pro[istriplr][ild][isn][iSS]
            = new TH1F(Form("h1_dphi_stripss_pro_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS),Form("Stripixel SS-dphi B%d Ladder%d Sensor%d Section%d", istriplr+2, ild, isn, iSS),1500,-3.0,3.0);
            h1_dphi_stripss_pro[istriplr][ild][isn][iSS]->SetXTitle("dphi(cm)");
          }

          if(is_usemodeler){
            h1_dz_stripss_pro_modeler[istriplr][ild][isn][iSS]
            = new TH1F(Form("h1_dz_stripss_pro_modeler_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS),Form("Stripixel SS-dz B%d Ladder%d Sensor%d Section%d", istriplr+2,ild,isn,iSS),300,-3.0,3.0);
            h1_dz_stripss_pro_modeler[istriplr][ild][isn][iSS]->SetXTitle("dz(cm)");
       
            h1_dphi_stripss_pro_modeler[istriplr][ild][isn][iSS]
            = new TH1F(Form("h1_dphi_stripss_pro_modeler_B%dL%dS%dSS%d",istriplr+2,ild,isn,iSS),Form("Stripixel SS-dphi B%d Ladder%d Sensor%d Section%d", istriplr+2, ild, isn, iSS),1500,-3.0,3.0);
            h1_dphi_stripss_pro_modeler[istriplr][ild][isn][iSS]->SetXTitle("dphi(cm)");
          }

          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]
          = new TH2I(Form("h2_bbcq_vs_nhit_B%dL%dS%dC%d",istriplr+2,ild,isn,iSS),Form("strip bbccharge vs nhit B%dL%dS%dC%d",istriplr+2,ild,isn,iSS),50,0,1000,100,0,100);
          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]->SetXTitle("bbc charge");
          strip_bbcq_vs_nhit[istriplr][ild][isn][iSS]->SetYTitle("ncluster");
        }//iSS
      }//isn
    }//ild
  }//istrip
  
  h1_projected_pixel = new TH1F("h1_projected_pixel","h1_projected_pixel",480,0,480);
  h1_projected_strip = new TH1F("h1_projected_strip","h1_projected_strip",448,0,448);
  
  h1_noprojection_layer =new TH1I("h1_noprojection_layer","No projection layer",4,0,4);
  

  h2_check_modeler_B0L0S1 = new TH2F("h2_check_modeler_B0L0S1","component vs z on B0L0S1",8,0,8,300,-15,15);
  h2_check_modeler_B0L5S1 = new TH2F("h2_check_modeler_B0L5S1","component vs z on B0L5S1",8,0,8,300,-15,15); 
  h2_check_modeler_B2L3S2 = new TH2F("h2_check_modeler_B2L3S2","component vs z on B2L3S2",8,0,8,300,-30,30); 
  h2_check_modeler_B2L10S2 = new TH2F("h2_check_modeler_B2L10S2","component vs z on B2L10S2",8,0,8,300,-30,30); 
  h2_check_modeler_B3L3S2 = new TH2F("h2_check_modeler_B3L3S2","component vs z on B3L3S2",8,0,8,300,-30,30); 
  h2_check_modeler_B3L20S2 = new TH2F("h2_check_modeler_B3L20S2","component vs z on B3L20S2",8,0,8,300,-30,30); 
 
  return;
}



int SvxEfficiencyQA::Get_AssociatedID(int index_svxcls)
{

  int gl_ntrk = 0;
  if(d_cnt != NULL){
    gl_ntrk = d_cnt->get_npart();
  }else{
    return -1;
  }
  
  if(!d_vtxout->isVtx("SVX_PRECISE")){ 
    cout << PHWHERE << "something is wrong"   << endl;  
  }
  PHPoint verpointsvxprim = d_vtxout->get_Vertex("SVX_PRECISE");
  float svxprim[3] = {-9999.0};
  svxprim[0] = verpointsvxprim.getX();
  svxprim[1] = verpointsvxprim.getY();
  svxprim[2] = verpointsvxprim.getZ();

  float evt_bbcz = (d_vtxout->get_Vertex("BBC")).getZ();

  for (int itrk = 0; itrk < gl_ntrk; itrk++) {
    float gl_mom = d_cnt->get_mom(itrk);
    //range [-PI/2,3/2PI]
    float gl_phi0 = d_cnt->get_phi0(itrk);
    if (gl_phi0 < -M_PI / 2.) gl_phi0 = gl_phi0 + 2.*M_PI;
    float gl_phi = d_cnt->get_phi(itrk);
    if (gl_phi < -M_PI / 2.) gl_phi = gl_phi + 2.*M_PI;
    float gl_the0 = d_cnt->get_the0(itrk); //theta at the vertex
    float gl_pt = fabs(gl_mom*sin(gl_the0));//pt 
    int   gl_charge = d_cnt->get_charge(itrk);//charge
    float gl_zed= d_cnt->get_zed(itrk);// Z coordinate at which the track crosses PC1

    if (CNTTrackSelection(d_cnt,itrk)) {
      h2_cntzedphi0->Fill(gl_zed,gl_phi0);
      h2_cntzedphi->Fill(gl_zed,gl_phi);

      SvxCluster *svxcls = d_svxcls->get_Cluster(index_svxcls);
      int gc_layer = svxcls->get_layer();
      int gc_ladder = svxcls->get_ladder();
      int gc_sensor = svxcls->get_sensor();
      float gc_svxx = svxcls->get_xyz_global(0);
      float gc_svxy = svxcls->get_xyz_global(1);
      float gc_svxz = svxcls->get_xyz_global(2);
 //   float  gc_lsvxx = svxcls->get_xyz_local(0);
 //   float gc_lsvxy = svxcls->get_xyz_local(1);
      float gc_lsvxz = svxcls->get_xyz_local(2);
      
      //get chip id 
      int gc_chipSS = Get_chipSS(gc_layer,gc_lsvxz);
     

      float gc_phiv = atan2(gc_svxy, gc_svxx);
      if(gc_phiv < -M_PI/2.) gc_phiv = gc_phiv + 2.*M_PI;

      float dproj_cut[4] = {0.05, 0.07, 0.15, 0.2};
      float zproj_cut[4] = {0.05, 0.07, 0.15, 0.2};
      float dproj_loosecut[4] = {0.4,0.4,1.0,1.4};
      float zproj_loosecut[4] = {5.0,5.0,5.0,5.0};
      
      const float clustercut_angle = 0.25;
      if (fabs(fabs(gc_phiv) - fabs(gl_phi0)) < clustercut_angle) {
        float gc_dproj;
        float gc_mag_bend;
        float gc_zproj;
        calc_dphidz(gl_phi0,gl_the0,gl_pt,gl_charge,gc_svxx,gc_svxy,gc_svxz,svxprim[0],svxprim[1],svxprim[2],m_fieldScale,&gc_dproj,&gc_mag_bend,&gc_zproj);
        if (fabs(gc_zproj + evt_bbcz - gc_svxz) < zproj_loosecut[gc_layer]){
          if (fabs(gc_dproj + gc_mag_bend) < dproj_loosecut[gc_layer]){
            bool fill_flag_dphi = false;
            bool fill_flag_dz = false;
            if (fabs(gc_zproj + svxprim[2] - gc_svxz) < 3.*zproj_cut[gc_layer]) {
              if(gc_layer<2){
                h1_dphi_pixelchip[gc_layer][gc_ladder][gc_sensor][gc_chipSS]->Fill(gc_dproj+gc_mag_bend); 
              }else{
                h1_dphi_stripss[gc_layer-2][gc_ladder][gc_sensor][gc_chipSS]->Fill(gc_dproj+gc_mag_bend);
              }
              fill_flag_dz = true;
            }
            if (fabs(gc_dproj + gc_mag_bend) < 3.*dproj_cut[gc_layer]) {
              if(gc_layer<2){
                h1_dz_pixelchip[gc_layer][gc_ladder][gc_sensor][gc_chipSS]->Fill(gc_zproj+svxprim[2]-gc_svxz);
              }else{
                h1_dz_stripss[gc_layer-2][gc_ladder][gc_sensor][gc_chipSS]->Fill(gc_zproj+svxprim[2]-gc_svxz);
              }
              fill_flag_dphi = true;
            }
            if( (fill_flag_dphi == true) && (fill_flag_dz == true)){
              h2_cntcluster_zphi[gc_layer]->Fill(gc_svxz,gc_phiv);
              return itrk;//ok this cluster has an associating cnt
            }
          }//dproj_loosecut
        }//zproj_loosecut
      }//angle cut
    }//cut
  }//itrk

  //can not find cnt associated cluster
  return -1;
}

SvxProjection  
SvxEfficiencyQA::Search_CNTProjectedChip(float primx,float primy,float primz,float pt,float phi0,float the0, int charge,float bDir)                       
{

  SvxProjection projection;

  const unsigned int nchipSS[4]={4,4,2,2};
  const float pixelladdersize_z = 22.24+0.04; //5.56*4 + gap 
  const float stripladdersize_zB2 = 31.8; //5.56*4 + gap 
  const float stripladdersize_zB3 = 38.2; //5.56*4 + gap 
  const float pixelsensorsize_x = 1.28;
  const float pixelsensorsize_z = 5.56;
  const float stripsensorsize_x = 3.072;
  const float stripsensorsize_z = 6.000;

  const float pixelsensorsize_phi = pixelsensorsize_x*cos(13.0/180.0*M_PI);
  const float pixelchipsize_phi = pixelsensorsize_phi;
  const float pixelchipsize_z = pixelsensorsize_z/4.0;
  const float stripSSsize_x   = stripsensorsize_x;
  const float stripSSsize_z   = stripsensorsize_z/2.0;

  const float phi_edge[4] = {0.05,0.07,0.15,0.2};
  const float z_edge[4] = {0.05,0.07,0.15,0.2};
  
  if( (m_EventNumber==0) && (verbosity>0) ){
    cout << PHWHERE << endl;
    cout << "pixelchipsize_phi: "<<  pixelchipsize_phi << endl;
    cout << "pixelchipsize_z: "<<  pixelchipsize_z << endl;
    cout << "stripSSsize_x: "<<  stripSSsize_x << endl;
    cout << "stripSSsize_z: "<<  stripSSsize_z << endl;
  }

  int nearest_ladder[4]={-1};
  int nearest_sensor[4]={-1};
  int nearest_chipSS[4]={-1};

  //find nearest ladder at first
  for(unsigned int ilr=0;ilr<4;ilr++){
    unsigned int nLadders = d_svxgeo->get_nBarLadder(ilr);  
    float smallest_ladder_dphi=-1000;
    //--float smallest_ladder_dz=-1000;
    for(unsigned int ild=0;ild<nLadders;ild++){
      //use sensor 0 
      int usedsensor0=-1;
      int usedsensor1=-1;
      if(ilr<2){ //pixel 
        usedsensor0 = 1;
        usedsensor1 = 2;
      }else if(ilr==2){ //B2
        usedsensor0 = 2;
        usedsensor1 = 2;
      }else{//B3
        usedsensor0 = 2;
        usedsensor1 = 3;
      }
      SvxSensor *sens0 = d_svxgeo->GetSensorPtr(ilr,ild,usedsensor0);
      float sx0 = sens0->get_correctedTransVector(0);
      float sy0 = sens0->get_correctedTransVector(1);
      float sz0 = sens0->get_correctedTransVector(2);
      SvxSensor *sens1 = d_svxgeo->GetSensorPtr(ilr,ild,usedsensor1);
//      float sx1 = sens1->get_correctedTransVector(0);
//      float sy1 = sens1->get_correctedTransVector(1);
      float sz1 = sens1->get_correctedTransVector(2);
      float lx = sx0;
      float ly = sy0;
      float lz = (sz0+sz1)/2.0;
      float dproj_ladder=-100;
      float magbend_ladder=-100;
      float zproj_ladder=-100;
      float ladder_angle = atan2(ly,lx);
      if(ladder_angle<-M_PI/2.) ladder_angle += 2.0*M_PI;
      const float cluster_anglecut=0.25;
      if(fabs(fabs(phi0) - fabs(ladder_angle))< cluster_anglecut){
        calc_dphidz(phi0,the0,pt,charge,lx,ly,lz,primx,primy,primz,bDir,&dproj_ladder,&magbend_ladder,&zproj_ladder);
        if(ilr<2){
          if( (fabs(dproj_ladder+magbend_ladder)<pixelsensorsize_phi/2.0+phi_edge[ilr]) && (fabs(zproj_ladder+primz-lz)<pixelladdersize_z/2.0+z_edge[ilr])){
            if( fabs(smallest_ladder_dphi) > fabs(dproj_ladder+magbend_ladder)){
              //--smallest_ladder_dz = zproj_ladder+primz-lz;
              smallest_ladder_dphi = dproj_ladder+magbend_ladder; 
              nearest_ladder[ilr] = ild;
            }
          }
        }else if(ilr==2){
          if( (fabs(dproj_ladder+magbend_ladder)<stripsensorsize_x/2.0+phi_edge[ilr]) && (fabs(zproj_ladder+primz-lz)<stripladdersize_zB2/2.0+z_edge[ilr])){
            if( fabs(smallest_ladder_dphi) > fabs(dproj_ladder+magbend_ladder)){
              //--smallest_ladder_dz = zproj_ladder+primz-lz;
              smallest_ladder_dphi = dproj_ladder+magbend_ladder; 
              nearest_ladder[ilr] = ild;
            }
          }
        }else{
          if( (fabs(dproj_ladder+magbend_ladder)<stripsensorsize_x/2.0+phi_edge[ilr]) && (fabs(zproj_ladder+primz-lz)<stripladdersize_zB3/2.0+z_edge[ilr])){
            if( fabs(smallest_ladder_dphi) > fabs(dproj_ladder+magbend_ladder)){
              //--smallest_ladder_dz = zproj_ladder+primz-lz;
              smallest_ladder_dphi = dproj_ladder+magbend_ladder; 
              nearest_ladder[ilr] = ild;
            }
          }
        }
      }//cluster_angle
    }//ild

      
    unsigned int nSensors = d_svxgeo->get_nBarSensor(ilr);
    //--float smallest_dphi_sensor=-1000;
    float smallest_dz_sensor=-1000;
    if(nearest_ladder[ilr]>-1){
      for(unsigned int isn=0;isn<nSensors;isn++){
        SvxSensor *sens = d_svxgeo->GetSensorPtr(ilr,nearest_ladder[ilr],isn);
        float sx = sens->get_correctedTransVector(0);
        float sy = sens->get_correctedTransVector(1);
        float sz = sens->get_correctedTransVector(2);
        float sen_angle = atan2(sy,sx);
        if(sen_angle<-M_PI/2.) sen_angle += 2.0*M_PI;
        const float cluster_anglecut=0.25;
        if(fabs(fabs(phi0) - fabs(sen_angle))< cluster_anglecut){
          float dproj_sens=-100;
          float magbend_sens=-100;
          float zproj_sens=-100;
          calc_dphidz(phi0,the0,pt,charge,sx,sy,sz,primx,primy,primz,bDir,&dproj_sens,&magbend_sens,&zproj_sens);
          if(ilr<2){
            if( (fabs(dproj_sens+magbend_sens)<pixelsensorsize_phi/2.0+phi_edge[ilr]) && (fabs(zproj_sens+primz-sz)<pixelsensorsize_z/2.0+z_edge[ilr])){
              if(fabs(smallest_dz_sensor) > fabs(zproj_sens+primz-sz)){
                smallest_dz_sensor = zproj_sens+primz-sz;
                //--smallest_dphi_sensor = dproj_sens+magbend_sens;
                nearest_sensor[ilr] = isn;
              }
            }
          }else{
            if( (fabs(dproj_sens+magbend_sens)<stripsensorsize_x/2.0+phi_edge[ilr]) && fabs((zproj_sens+primz-sz)<stripsensorsize_z/2.0+z_edge[ilr]) ){
              if((fabs(smallest_dz_sensor) > fabs(zproj_sens+primz-sz))){
                smallest_dz_sensor = zproj_sens+primz-sz;
                //--smallest_dphi_sensor = dproj_sens+magbend_sens;
                nearest_sensor[ilr] = isn;
              }
            }
          }
        }//cluster angle
      }//for isn
    }
    projection.ladder[ilr] = nearest_ladder[ilr];
    projection.sensor[ilr] = nearest_sensor[ilr];

    //search chip
    float smallest_chipdphi=-100;
    float smallest_chipdz=-100;
    if( (ilr<2) && (nearest_ladder[ilr]>-1) && (nearest_sensor[ilr]>-1) ){
      for(unsigned int icp=0;icp<nchipSS[ilr];icp++){
        float cx = m_chip[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][icp].transvector_corr[0];
        float cy = m_chip[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][icp].transvector_corr[1];
        float cz = m_chip[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][icp].transvector_corr[2];
        float dproj_chip=-100;
        float magbend_chip=-100;
        float zproj_chip=-100;
        calc_dphidz(phi0,the0,pt,charge,cx,cy,cz,primx,primy,primz,bDir,&dproj_chip,&magbend_chip,&zproj_chip);
        if( (fabs(dproj_chip+magbend_chip)<pixelchipsize_phi/2.0+phi_edge[ilr]) && (fabs(zproj_chip+primz-cz)<pixelchipsize_z/2.0+z_edge[ilr])){
          if(fabs(smallest_chipdz) > fabs(zproj_chip+primz-cz)){
            smallest_chipdphi = dproj_chip+magbend_chip;
            smallest_chipdz = zproj_chip+primz-cz;
            nearest_chipSS[ilr] = icp;
          }
        }
      }
    }else if ( (ilr>1) && (nearest_ladder[ilr]>-1) && (nearest_sensor[ilr]>-1)){ 
      for(unsigned int iss=0;iss<nchipSS[ilr];iss++){
        float ssx = m_chip[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][iss].transvector_corr[0];
        float ssy = m_chip[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][iss].transvector_corr[1];
        float ssz = m_chip[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][iss].transvector_corr[2];
        float dproj_ss=-100;
        float magbend_ss=-100;
        float zproj_ss=-100;
        calc_dphidz(phi0,the0,pt,charge,ssx,ssy,ssz,primx,primy,primz,bDir,&dproj_ss,&magbend_ss,&zproj_ss);
        if( (fabs(dproj_ss+magbend_ss)<stripSSsize_x/2.0+phi_edge[ilr]) && (fabs(zproj_ss+primz-ssz)<stripSSsize_z/2.0+z_edge[ilr])){
          if(fabs(smallest_chipdz) > fabs(zproj_ss+primz-ssz)){  
            smallest_chipdphi = dproj_ss+magbend_ss;
            smallest_chipdz = zproj_ss+primz-ssz;
            nearest_chipSS[ilr] = iss;
          }
        }
      }//for
    }     

    //if found nearest chip
    projection.chipSS[ilr] = nearest_chipSS[ilr];
    if( (nearest_chipSS[ilr]>-1) && (nearest_sensor[ilr]>-1) && (nearest_ladder[ilr]>-1) ){
      if(ilr<2){
        h1_dphi_pixelchip_pro[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][nearest_chipSS[ilr]]->Fill(smallest_chipdphi);
        h1_dz_pixelchip_pro[ilr][nearest_ladder[ilr]][nearest_sensor[ilr]][nearest_chipSS[ilr]]->Fill(smallest_chipdz);
      }else{
        h1_dphi_stripss_pro[ilr-2][nearest_ladder[ilr]][nearest_sensor[ilr]][nearest_chipSS[ilr]]->Fill(smallest_chipdphi);    
        h1_dz_stripss_pro[ilr-2][nearest_ladder[ilr]][nearest_sensor[ilr]][nearest_chipSS[ilr]]->Fill(smallest_chipdz);    
      }
    }else{
      h1_noprojection_layer->Fill(ilr);
    }
  }//ilr


  return projection;
}

void SvxEfficiencyQA::Create_chipGeometry()
{
//  const float pixelchip_halfwidth_x = 0.640;
  const float pixelchip_halfwidth_z = 5.56/8.0;//sensor size / 4 chip /2
//  const float stripSS_halfwidth_x = 1.536;
  const float stripSS_halfwidth_z = 6.0/4.0; // sensor size / 2 sensor section /2
   
  const unsigned int nchipSS[4]={4,4,2,2};
  const unsigned int nladder_half[4] = {5,10,8,12};
  const float nhalf_pixel[4]={-3,-1,1,3};
  const float  nhalf_strip[2]={-1,1};
  for(unsigned int ilr=0;ilr<4;ilr++){
    unsigned int nLadders = d_svxgeo->get_nBarLadder(ilr);  
    unsigned int nSensors = d_svxgeo->get_nBarSensor(ilr);
    for(unsigned int ild=0;ild<nLadders;ild++){
      for(unsigned int isn=0;isn<nSensors;isn++){
        SvxSensor *sens = d_svxgeo->GetSensorPtr(ilr,ild,isn);
        float sx = sens->get_correctedTransVector(0);
        float sy = sens->get_correctedTransVector(1);
        float sz = sens->get_correctedTransVector(2);
        
        float tmp_matrix[3][3];
        for(unsigned int i=0;i<3;i++){
          for(unsigned int j=0;j<3;j++){
            tmp_matrix[i][j]=sens->get_rotMatrix(i,j);
          }
        }
        float iwe = 0;
        if(ild<nladder_half[ilr]) iwe=-1;//west 
        else                       iwe=1;//east
        // pixel
        // west 
        //
        // chip | 3 | 2 | 1 | 0 |
        //  ---------------------> global z
        // east 
        //
        // chip | 0 | 1 | 2 | 3 |
        //  ---------------------> global z
        
        // strip L2
        // west 
        // R L R L R L R L R L 
        // east 
        // L R L R L R L R L R
        // strip L3
        // west 
        // L R L R L R L R L R
        // east
        // R L R L R L R L R L
        //
        // L = 0
        // R = 1
        

        for(unsigned int ichp = 0;ichp<nchipSS[ilr];ichp++){
          m_chip[ilr][ild][isn][ichp].transvector_corr[0] = sx;
          m_chip[ilr][ild][isn][ichp].transvector_corr[1] = sy;
          if(ilr<2) m_chip[ilr][ild][isn][ichp].transvector_corr[2] = sz+iwe*nhalf_pixel[ichp]*pixelchip_halfwidth_z;
          else if (ilr==2)     m_chip[ilr][ild][isn][ichp].transvector_corr[2] = sz+iwe*nhalf_strip[ichp]*stripSS_halfwidth_z; 
          else                 m_chip[ilr][ild][isn][ichp].transvector_corr[2] = sz-iwe*nhalf_strip[ichp]*stripSS_halfwidth_z; 
          for(unsigned int i=0;i<3;i++){
            for(unsigned int j=0;j<3;j++){
              m_chip[ilr][ild][isn][ichp].rotMatrix[i][j]=tmp_matrix[i][j];
            }
          }
        }//ichp
      }//isen
    }//ild
  }//ilr
}

void SvxEfficiencyQA::Print_chipGeometry()
{
  
  const unsigned int nchipSS[4]={4,4,2,2};
  cout << PHWHERE << "SvxEfficiencyQA: creating chip based geometry.... " << endl;
  cout << endl;
  for(unsigned int ilr=0;ilr<4;ilr++){
    unsigned int nLadders = d_svxgeo->get_nBarLadder(ilr);  
    unsigned int nSensors = d_svxgeo->get_nBarSensor(ilr);
    for(unsigned int ild=0;ild<nLadders;ild++){
      for(unsigned int isn=0;isn<nSensors;isn++){
        for(unsigned int ichp=0;ichp<nchipSS[ilr];ichp++){
          cout << ilr <<'\t' << ild << '\t' << isn << '\t' << ichp << '\t';
          cout << m_chip[ilr][ild][isn][ichp].transvector_corr[0] << '\t' ;
          cout << m_chip[ilr][ild][isn][ichp].transvector_corr[1] << '\t' ;
          cout << m_chip[ilr][ild][isn][ichp].transvector_corr[2] << '\t' << endl;;
        }
      }
    }
  }
  
  return;
}

void SvxEfficiencyQA::Create_tileGeometry()
{
  



  return;
}


void SvxEfficiencyQA::calc_dphidz(float phi0,float the0, float pt, int charge, // track info
                   float x, float y, float z,        // cluster position or center of chip
                   float xvtx, float yvtx, float zvtx,        // prim. vertex position;
                   float bDir,                                // direction of B-field
                   float* dproj, float* magbend, float* zproj) // "output"
{

  float ux = cos(phi0); //unit vector
  float uy = sin(phi0); //unit vector

  float dx = x - xvtx;
  float dy = y - yvtx;

  float svx_xy2 = dx*dx + dy*dy;
  float svx_xy = sqrt(svx_xy2);
  const float bendconst = 0.00135;

  *dproj = ux * dy - uy * dx;
  *magbend = -1.0 * bDir * bendconst * charge * svx_xy2 / pt;

  *zproj = svx_xy/tan(the0);

  return;
}


bool SvxEfficiencyQA::CNTTrackSelection(PHCentralTrack *d_cnt,unsigned int itrk)
{
    float gl_mom = d_cnt->get_mom(itrk);
    //range [-PI/2,3/2PI]
    //--float gl_phi0 = d_cnt->get_phi0(itrk);
    //--if (gl_phi0 < -M_PI / 2.) gl_phi0 = gl_phi0 + 2.*M_PI;
    //--float gl_phi = d_cnt->get_phi(itrk);
    //--if (gl_phi < -M_PI / 2.) gl_phi = gl_phi + 2.*M_PI;
    float gl_the0 = d_cnt->get_the0(itrk); //theta at the vertex
    float gl_pt = fabs(gl_mom*sin(gl_the0));//pt 
    float gl_emcdz = d_cnt->get_emcdz(itrk);//emcz
    float gl_emcdphi = d_cnt->get_emcdphi(itrk);//emcdphi
    float gl_pc3dz = d_cnt->get_pc3dz(itrk);//pc3dz
    float gl_pc3dphi = d_cnt->get_pc3dphi(itrk);//pc3dphi
    float gl_zed= d_cnt->get_zed(itrk);// Z coordinate at which the track crosses PC1
    int gl_trk_quality = d_cnt->get_quality(itrk);

    const float pt_cut_min = 0.5;
    const float pt_cut_max = 10.0;
    if ((gl_pt > pt_cut_min)
        && (gl_pt < pt_cut_max)
        && (fabs(gl_pc3dphi - gl_emcdphi + m_pc3dphi_emcdphioffset) < m_pc3dphi_emcdphicut)
        && (fabs(gl_pc3dz - gl_emcdz + m_pc3dz_emcdzoffset) < m_pc3dz_emcdzcut)
        && (gl_pc3dphi > -2000)
        && (gl_emcdphi > -2000)
        && (fabs(gl_zed) < 75)
        && ((gl_trk_quality == 31) || (gl_trk_quality == 63))
       ) {
         return true;
    }

  return false;
}


int SvxEfficiencyQA::Get_chipSS(int ilr,float local_z)
{
  if(ilr>3){
    cerr << PHWHERE << "out of range of layer " << ilr << endl;
    return -1;
  }
  
  int chipSS =-1;
  if(ilr<2){
    if(1.400<=local_z) chipSS=0; 
    else if( 0<=local_z && local_z<1.400) chipSS=1;     
    else if( -1.400<=local_z && local_z<0) chipSS=2;     
    else if( local_z<-1.400) chipSS=3;     
    else cout << "something is wrong!! " << endl;
  }else{
    if(local_z<0) chipSS=0;//left SS
    else          chipSS=1;//right SS
  }


  return chipSS;
}



int SvxEfficiencyQA::Get_tileID(int ilr,float local_x,float chiplocal_z)
{
/*
  const float pixelchip_width_z = 1.39;
  const float pixelchip_width_phi = 1.28;
  const float stripSS_width_z = 3.0;
  const float stripSS_width_phi = 3.072;
 
  
    |----+----+----+----|
    |  0 |  1 |  2 |  3 |
    |----+----+----+----|
    |  4 |  5 |  6 |  7 |
    |----+----+----+----|
    |  8 |  9 | 10 | 11 |
    |----+----+----+----|
    | 12 | 13 | 14 | 15 |
    |----+----+----+----|
  

  //pixel
  if(ilr<2){
    if(local_x <  ){



    }




*/


  return 0;

}


float SvxEfficiencyQA::Get_chipSSlocalz(int ilr,int chipSS,float local_z)
{
  const float pixelchip_halfwidth_z = 0.695;
  const float stripSS_halfwidth_z = 1.5;
  float chip_localz=-9999;
  if(ilr<2){
    if(chipSS==0) chip_localz = local_z - 2.0*pixelchip_halfwidth_z;
    else if(chipSS==1) chip_localz = local_z;
    else if(chipSS==2) chip_localz = local_z + 2.0*pixelchip_halfwidth_z;
    else if(chipSS==3) chip_localz = local_z + 4.0*pixelchip_halfwidth_z;
  }else{
    if(chipSS==0)chip_localz = local_z + 2.0*stripSS_halfwidth_z; //left
    else         chip_localz = local_z;
  }

  return chip_localz;
}


int SvxEfficiencyQA::ComponentToPixelROC(int ilr,int ild,int isn,int component)
{
  if(ilr>1){
    cout << "This function works only in pixel layer !!" << endl;
    return -1;
  }
  
  int pixelROC=-1;
  if(isn%2==0){
    if(component == 0 ) pixelROC=3;
    else if(component == 1 ) pixelROC=2;
    else if(component == 2 ) pixelROC=1;
    else if(component == 3 ) pixelROC=0;
  }else{
    if(component == 0 ) pixelROC=3;
    else if(component == 1 ) pixelROC=2;
    else if(component == 2 ) pixelROC=1;
    else if(component == 3 ) pixelROC=0;
  }


  return pixelROC;
}


int SvxEfficiencyQA::ComponentToStripSS(int ilr,int ild,int isn,int component)
{
  if(ilr<2){
    cout << "This function works only in strip layer !! " << endl;
    return -1;
  }

  int stripSS=-1;
  if(0 <= component && component <=1){
    stripSS = component;
  }


  return stripSS;


}
