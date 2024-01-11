#include "RPCalibrator.h"

#include <Fun4AllHistoManager.h>
#include <Fun4AllReturnCodes.h>
#include <RunNumberRanges.h>
#include <getClass.h>
#include <recoConsts.h>

#include <PHGlobal.h>
#include <PreviousEvent.h>
#include <EventHeader.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>

#include "RpSumXYObject.h"
#include "RpSnglSumXY.h"
#include "ReactionPlaneObject.h"
#include "ReactionPlaneSngl.h"

#include "ReactionPlaneCalibv1.h"

//#include <compactCNT/SvxCentralTrackMap.h>
//#include <compactCNT/SvxCentralTrackMapEntry.h>

#include <TFile.h>
#include <TProfile.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>

#include <vector>
#include <iostream>
#include <fstream>
#include <gsl/gsl_math.h>


using namespace std;

//==============================================================
RPCalibrator::RPCalibrator(const char* output) : 
   SubsysReco("RPCALIBRATOR")
  ,m_RecalFlag(FLAG_NOCALIB)
//--  ,m_MasterRecalFlag(1)
//--  ,m_VnFlag(0)
//--  ,m_EvSkipFlag(0)
  ,OutputFileName(output)
  ,m_RunNumber  (0)
  ,m_EventNumber(0)
  ,rpcalibv1(NULL)
  ,HistoManager(new Fun4AllHistoManager("RPCalibHisto"))
{
 
  // histo NULL
  m_centzv = NULL;
  m_centzv_svx = NULL;
  
  for(unsigned int idet=0; idet<NDET; idet++)
   for(int icent=0; icent<RP::NMUL3; icent++)
     for(int iz=0; iz<RP::NZPS2; iz++)
       for(int ihar=0; ihar<RP::NHAR4; ihar++) {
          m_rawq[icent][iz][ihar][idet] = NULL;
        }

  for(unsigned int idet=0; idet<NDET; idet++)
   for(int icent=0; icent<RP::NMUL3; icent++)
     for(unsigned int ihar=0; ihar<NHAR2D; ihar++){
        m_rawqxy[icent][ihar][idet] = NULL;
      }

  for(unsigned int idet=0; idet<NDET; idet++)
   for(int icent=0; icent<RP::NMUL3; icent++)
     for(int iz=0; iz<RP::NZPS2; iz++)
       for(int ihar=0; ihar<RP::NHAR4; ihar++) {
          m_flatpar[icent][iz][ihar][idet] = NULL;
          m_rawrp[  icent][iz][ihar][idet] = NULL;
        }

}
//==============================================================
RPCalibrator::~RPCalibrator()
{
  for(unsigned int idet=0; idet<NDET; idet++)
   for(int icent=0; icent<RP::NMUL3; icent++)
     for(int iz=0; iz<RP::NZPS2; iz++)
       for(int ihar=0; ihar<RP::NHAR4; ihar++) {
         if(m_rawq[icent][iz][ihar][idet]!=NULL) delete [] m_rawq[icent][iz][ihar][idet];
       }

  for(unsigned int idet=0; idet<NDET; idet++)
   for(int icent=0; icent<RP::NMUL3; icent++)
     for(int unsigned ihar=0; ihar<NHAR2D; ihar++) {
        if(m_rawqxy[icent][ihar][idet] != NULL) delete [] m_rawqxy[icent][ihar][idet];
      }

  for(unsigned int idet=0; idet<NDET; idet++)
   for(int icent=0; icent<RP::NMUL3; icent++)
     for(int iz=0; iz<RP::NZPS2; iz++)
       for(int ihar=0; ihar<RP::NHAR4; ihar++) {
         if(m_flatpar[icent][iz][ihar][idet]!=NULL) delete [] m_flatpar[icent][iz][ihar][idet];
         if(m_rawrp[  icent][iz][ihar][idet]!=NULL) delete [] m_rawrp[  icent][iz][ihar][idet];
       }

  delete HistoManager;
}
//==============================================================
int RPCalibrator::Init(PHCompositeNode *topNode)
{
  cout << "RPCalibrator::Init started..." << endl;
  cout << "m_RecalFlag = " << m_RecalFlag << endl;
  cout << "RPCalibrator::Init ended." << endl;

  return 0;
}
//==============================================================
int RPCalibrator::InitRun(PHCompositeNode *topNode)
{
  cout << "RPCalibrator::InitRun started..." << endl;
  
  // Get Run Number
  recoConsts *rc = recoConsts::instance();
  m_RunNumber = rc->get_IntFlag("RUNNUMBER");
  cout << "RPCalibrator::InitRun   run number      = " << m_RunNumber << endl;
  cout << "                        M_RECAL_FLAG IS = " << m_RecalFlag << endl;

  // set parameter
  rpcalibv1 = findNode::getClass<ReactionPlaneCalibv1>(topNode, "ReactionPlaneCalibv1");
  if(rpcalibv1==NULL){
    cerr<<"RPCalibrator::"<<__FUNCTION__<<" no ReactionPlaneCalibv1 !!!!  SHOULD STOP !!!!"<<endl;
    return EVENT_OK;
  }

  // histogram Initialization
  //   this must be after getting rpcalib
  initHisto();


/*
  //this should be set in the different place
  int icalibversion = 4002; // after Run5 and Field_ON
  
  cout << "RPCalibrator::InitRun - calibration version:  " << icalibversion << endl;
  rc->set_IntFlag("BBCCALIBVERSION", icalibversion);
*/


  cout << "RPCalibrator::InitRun ended." << endl;

  return 0;    
}
//==============================================================
int RPCalibrator::process_event(PHCompositeNode *topNode)
{
  //cout << " process " << m_EventNumber << endl;
  if(m_EventNumber%10000==0) cout << m_EventNumber<< "//------------------------------------" << endl;
  m_EventNumber++;

  if(rpcalibv1==NULL) return EVENT_OK;

  //return 0;
  ////////////////
  // Event Skip //
  //if( EventSkip() == -1 ) return ABORTEVENT;
  //else if( EventSkip() == -2 ) return ABORTRUN;
  
  
  PHGlobal      *global = findNode::getClass<PHGlobal>(     topNode, "PHGlobal");
  RpSumXYObject *sumxy  = findNode::getClass<RpSumXYObject>(topNode, "RpSumXYObject");
  PreviousEvent *peve   = findNode::getClass<PreviousEvent>(topNode, "PreviousEvent"); // Tick
  if(!global){ cerr << PHWHERE << " No PHGloval object !"      << endl; return EVENT_OK; }
  if(!peve)  { cerr << PHWHERE << " No PreviousEvent object !" << endl; return EVENT_OK; }
  if(!sumxy) { cerr << PHWHERE << " No RpSumXYObject object !" << endl; return EVENT_OK; }

  ReactionPlaneObject *rp = findNode::getClass<ReactionPlaneObject>(topNode, "ReactionPlaneObject"); 
  if(m_RecalFlag==FLAG_NOCALIB){
    if(!rp) { cerr << PHWHERE << " No ReactionPlaneObject !" << endl; return EVENT_OK; }
  }
  else { rp = NULL; }



  //////////////////
  // Z_Vertex Cut //
  float vertex=-9999;
  int iz    = rpcalibv1->GetZVertexBin(topNode, vertex);
  int izsvx = rpcalibv1->GetZVertexBinSvx(vertex);
  if(iz<0) {
    if(verbosity>0) cerr <<"Z Vertex is out of range for RP calibration:"<<vertex<<" , skip the event" << endl;
    return EVENT_OK;
  }
  if(izsvx<0) {
    if(verbosity>0) cerr <<"Z Vertex is out of range for SVX RP calibration, skip the event" << endl;
  }
  
  ////////////////
  // Centrlity //
  float centrality = global->getCentrality();
  int   icent = rpcalibv1->GetCentralityBin(centrality);
  if( icent < 0 ) {
    float bbcq = global->getBbcChargeN()+global->getBbcChargeS();

    cerr<<" Unknown cenrality : "<<centrality<<" (bbcq:"<<bbcq<<") ";

    EventHeader *evthdr = findNode::getClass<EventHeader>(topNode, "EventHeader"); 
    if(!evthdr){ cerr << PHWHERE << " No EventHeader object !" << endl;   return EVENT_OK; }
    int evtseq = evthdr->get_EvtSequence();
    cerr<<evtseq<<endl;

    return EVENT_OK;
  }

  //////////////////////
  // event histo
  m_centzv->Fill(centrality, vertex);
  m_centzv_svx->Fill(centrality, vertex);


  //////////////////////
  // Q-Vector Section //
  
  ////////////////////////////////////
  // Get Q-Vector from RpSumXYObject 
  if (sumxy) 
  {
    if( m_RecalFlag == FLAG_RECENT || m_RecalFlag == FLAG_FLAT ) {
      if(!isBadTick(peve)) fillAllRawQvector(RP::ID_SVX, icent, izsvx, sumxy);
      if(!isBadTick(peve)) fillAllRawQvector(RP::ID_SEG, icent, izsvx, sumxy);

      fillAllRawQvector(RP::ID_MPC, icent, iz, sumxy);
      fillAllRawQvector(RP::ID_BBC, icent, iz, sumxy);
      fillAllRawQvector(RP::ID_SMD, icent, iz, sumxy);
      fillAllRawQvector(RP::ID_CNT, icent, iz, sumxy);
      fillAllRawQvector(RP::ID_FVT, icent, iz, sumxy);
    }

    if( m_RecalFlag == FLAG_FLAT || m_RecalFlag == FLAG_NOCALIB ) {
      if(!isBadTick(peve)) fillAllFlatpar(RP::ID_SVX, icent, izsvx, sumxy, rp);
      if(!isBadTick(peve)) fillAllFlatpar(RP::ID_SEG, icent, izsvx, sumxy, rp);

      fillAllFlatpar(RP::ID_MPC, icent, iz, sumxy, rp);
      fillAllFlatpar(RP::ID_BBC, icent, iz, sumxy, rp);
      fillAllFlatpar(RP::ID_SMD, icent, iz, sumxy, rp);
      fillAllFlatpar(RP::ID_CNT, icent, iz, sumxy, rp);
      fillAllFlatpar(RP::ID_FVT, icent, iz, sumxy, rp);
    }

  } // if(sumxy)
  
//--  if( m_MasterRecalFlag == 0 ){ // when you read from recalibrated DST files
//--
//--     ////////////////////////////////////////////////////////
//--     // Additional Code to check correlation from new DSTs //
//--     ReactionPlaneObject *rpobject = findNode::getClass<ReactionPlaneObject>(topNode,"ReactionPlaneObject");
//--     if(rpobject == NULL){
//--       if(verbosity > 0) cout << PHWHERE << "Cannot find rpobject." << endl;
//--       return 0;
//--     }
//--     if(!rpobject) cout << " No rp object " << endl;
//--     float RP[ndet3];
//--     // Get calibrated reaction plane from ReactionPlaneObject
//--     for(int idet = 0; idet < ndet3; idet++) RP[idet] = -9999.0;
//-- 
//--     for(int idet = 0; idet < NDet; idet++){
//--       int idcode;
//--       if     (idet < (mmdet+25)   )  idcode=RP::calcIdCode(RP::ID_SVX,idet,1); // ihar = 1 , 0-24
//--       else if(idet < (mmdet+25+3) )  idcode=RP::calcIdCode(RP::ID_MPC,idet-(mmdet+25),1); // ihar = 1 25-27
//--       else if(idet < (mmdet+25+6) )  idcode=RP::calcIdCode(RP::ID_BBC,idet-(mmdet+25+3),1); // ihar = 1, 28-30
//--       else if(idet < (mmdet+25+9) )  idcode=RP::calcIdCode(RP::ID_SMD,idet-(mmdet+25+6),0); // ihar = 0  31-33
//--       else if(idet < (mmdet+25+14))  idcode=RP::calcIdCode(RP::ID_CNT,idet-(mmdet+25+9),1); // ihar = 1  34-38
//--       else                           idcode=RP::calcIdCode(RP::ID_FVT,idet-(mmdet+25+14),1); // ihar = 1 39-68
//--       
//--       RP[idet] = rpobject->getReactionPlane(idcode)->GetPsi(); // ihar = 1 or 0
//--     }
//--     
//--     for(int idet = 0; idet < NDet; idet++){
//--       if(RP[idet]>-9000. ){
//--         FL_Psi1D[icent][iz][idet] -> Fill(RP[idet]);
//--         for(int isn=0;isn<3;isn++){
//--           float dpsi = RP[idet] - RP[28+isn];//subsys vs BBC
//--           if(idet==31 || idet==32 || idet==33) dpsi = RP[idet] - RP[28+isn]; // ihar = 1
//--           float cor_cos = cos((2.)*(dpsi));
//--           float cor_sin = sin((2.)*(dpsi));
//--           
//--           if(RP[idet] > -900. && RP[28+isn] > -900.){
//--             Vtx_Bbc_Reso[idet][isn][0]->Fill(icent, cor_cos);
//--             Vtx_Bbc_Reso[idet][isn][1]->Fill(icent, cor_sin);
//--           }
//--         }
//--       }
//--     }
//-- 
//--     for(int idet = 0; idet < NDet; idet++){
//--       float dpsi = RP[idet] - RP[38];
//--       if(idet==31 || idet==82 || idet==33) dpsi = RP[idet] - RP[38]; // ihar = 1
//--       float cor_cos = cos((2.)*(dpsi));
//--       float cor_sin = sin((2.)*(dpsi));
//--       if(RP[idet] > -900. && RP[38] > -900.){
//--         Cnt_Reso[idet][0]->Fill(icent, cor_cos);
//--         Cnt_Reso[idet][1]->Fill(icent, cor_sin);
//--       }
//--     }
//-- 
//--     ////          | MPC | BBC | SMD    
//--     ////          |     |     |
//--     int Type1[3] = { 25,  28,  31}; // idet2
//--     int Type2[3] = { 26,  29,  32}; // idet2
//--     for(int type = 0; type < 3; type++){
//--       float dpsi;
//--       if(type<2){
//--         if(  RP[Type1[type]] > -900. &&  RP[Type1[type]] > -900. ){
//--           dpsi = RP[Type1[type]] - RP[Type2[type]]; // ihar = 1
//--           Cor_S_N[type][0]->Fill(icent, cos(2.0*dpsi));
//--           Cor_S_N[type][1]->Fill(icent, sin(2.0*dpsi));
//--         }
//--       }
//--       else{
//--         if(  RP[Type1[type]] > -900. &&  RP[Type1[type]] > -900. ){
//--           dpsi =  RP[Type1[type]] - RP[Type2[type]]; // ihar = 0
//--           Cor_S_N[type][0]->Fill(icent, cos(1.0*dpsi));
//--           Cor_S_N[type][1]->Fill(icent, sin(1.0*dpsi));
//--         }
//--       }
//--     }
//-- 
//--     / *
//--     ///FVTX N S Correlation
//--     ////////////////////////
//--     for(int i=0;i<2;i++){
//--       for(int idet=0;idet<5;idet++){
//--         int ifvtx;
//--         float dpsi;
//-- 
//--         ifvtx = idet + 25 + 14 + 15*i;
//--         dpsi= RP[ifvtx] - RP[ifvtx+5];//76,77,78,79,80  81,82,83,84,85
//--         if(  RP[ifvtx] > -900. &&  RP[ifvtx+5] > -900. ){
//--           Fvtx_S_N[idet+i*5][0]->Fill(icent, cos(2*dpsi));
//--           Fvtx_S_N[idet+i*5][1]->Fill(icent, sin(2*dpsi));
//--         }
//--       }
//--     }
//--     * /
//-- 
//--     //Flow measurement
//-- / *
//--     PHCentralTrack* central = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
//-- 
//--     if(!central) cout << " CNT Event skip " << endl;
//--     else{
//--       int ntrk=central->get_npart();
//--       for(int itrk=0;itrk<ntrk;itrk++){
//--         short quality  = central->get_quality(itrk);
//--         float mom      = central->get_mom(itrk);
//--         float the0     = central->get_the0(itrk);
//--         float zed      = central->get_zed(itrk);
//--         float phi0     = central->get_phi0(itrk);
//--         float pc3dphi  = central->get_pc3dphi(itrk);
//--         float pc3dz    = central->get_pc3dz(itrk);
//--         float pc3sdphi = pc3dphi/0.001; // around 1 sigma for dphi  ?????????
//--         float pc3sdz   = pc3dz/0.7; // around 1 sigma for dz  ?????????
//--         //float eta0 = -log(tan(the0/2.0)); 
//--         // Good track selection
//--         if ((quality == 31 || quality == 63) 
//--             && sqrt(pc3sdphi*pc3sdphi+pc3sdz*pc3sdz)<3.0 
//--             && the0 > -100 
//--             && fabs(zed) < 100
//--             && phi0 > -100 
//--             && mom != 0.0 && mom < 20.0)
//--           {
//--             float phi = atan2(sin(phi0), cos(phi0)); 
//--             float pt  = mom * sin(the0);
//--             float vn_cnt_cos=-9999.;
//--             float vn_cnt_sin=-9999.;
//--             int Imul;
//--             Imul = (int)icent/2;
//--             //cntdphidz[Imul][iz]->Fill(pc3dphi,pc3dz);
//--             //cnttrack[Imul][iz]->Fill(eta0,phi);
//--             if(BEGIN_OF_RUN11 <= m_RunNumber && m_RunNumber < BEGIN_OF_RUN12){
//--               for(int irpl=0;irpl<NDet-5;irpl++){
//--                 if(RP[irpl]>-9000.  && pt<5.0){
//--                   vn_cnt_cos = cos((2.0)*(phi-RP[irpl] ));
//--                   vn_cnt_sin = sin((2.0)*(phi-RP[irpl] ));
//--                   Cntvn_Ob[Imul][irpl][0]->Fill(pt,vn_cnt_cos);
//--                   Cntvn_Ob[Imul][irpl][1]->Fill(pt,vn_cnt_sin);
//--                 }         
//--               }
//--             }
//--             else if(m_RunNumber >= BEGIN_OF_RUN12){
//--               for(int irpl=0;irpl<15;irpl++){
//--                 int Irpl;
//--                 if(irpl<9) Irpl = irpl;
//--                 else       Irpl = 13 + 5*(irpl-8); 
//--                 if(RP[Irpl+25]>-9000.  && pt<5.0){
//--                   vn_cnt_cos = cos((2.0)*(phi-RP[Irpl+25] ));
//--                   vn_cnt_sin = sin((2.0)*(phi-RP[Irpl+25] ));
//--                   Cntvn_Ob[Imul][irpl][0]->Fill(pt,vn_cnt_cos);
//--                   Cntvn_Ob[Imul][irpl][1]->Fill(pt,vn_cnt_sin);
//--                 }         
//--               } 
//--             }//v2         
//--           }//v2                                                         
//--       }//idet                                                                         
//--     }//quarity                                                
//-- * /
//--     ////              |      all eta         | e gap 1 | e gap 2 |
//--     ////               ALL   S   N    S   N    S/N S/N  MPC  BBC
//--     //int Type1[9] = {12,  19, 20,   22, 23,   19, 22,  25, 28}; // idet2
//--     //int Type2[9] = {30,  30, 30,   30, 30,   20, 23,  26, 29}; // idet2
//--     //for(int type = 0; type < 9; type++){
//--     //  if(RP[Type1[type]] > -9000 && RP[Type2[type]] > -9000){
//--     //    float dpsi = RP[Type1[type]] - RP[Type2[type]];
//--     //    Cor_S_N[type][0]->Fill(icent, cos(2.0*dpsi)); // ihar = 1
//--     //    Cor_S_N[type][1]->Fill(icent, sin(2.0*dpsi)); // ihar = 1
//--     //  }
//--     //}
//--     
//--  }// m_MasterRecalFlag == 0
  //cout<< " process end " << m_EventNumber << endl;

  return 0;
}
//==============================================================
int RPCalibrator::End(PHCompositeNode *topNode)
{
  cout << "RPCalibrator::End" << endl;
  
    
  HistoManager->dumpHistos(OutputFileName);
  return 0; 
}
//==============================================================
void RPCalibrator::initHisto()
{
  char name[200];
  
  //Hist Event check
  unsigned int nzbin = rpcalibv1->GetDetNz(RP::ID_BBC);
  sprintf(name, "centzv");
  HistoManager->registerHisto(new TH2F(name, name, RP::NMUL3, 0.,100., nzbin, -30.,30.) ); 
  m_centzv = static_cast<TH2*>(HistoManager->getHisto(name));

  nzbin = rpcalibv1->GetDetNz(RP::ID_SVX);
  sprintf(name, "centzv_svx");
  HistoManager->registerHisto(new TH2F(name, name, RP::NMUL3, 0.,100., nzbin, -10.,10.) ); 
  m_centzv_svx = static_cast<TH2*>(HistoManager->getHisto(name));
    
//-  if( m_MasterRecalFlag == 1 )
  {
    //------------------------------------------------------------------------------------
    // Hist to Generate Re-centering Const.
    //------------------------------------------------------------------------------------
    if( m_RecalFlag == FLAG_RECENT || m_RecalFlag == FLAG_FLAT ){
      //-----Re_centering-----
      cout << "--- RP_CALIB_RECENT_M::Registering Histogram ---" << endl;

      const vector<int> &vDetid = rpcalibv1->GetEnableDetArray();
      for(unsigned int idet=0; idet<vDetid.size(); idet++){
        int          detid   = vDetid[idet];

        if(!rpcalibv1->GetEnableDet(detid)) continue;

        string       detname = rpcalibv1->GetDetName(detid);
        unsigned int nzbin   = rpcalibv1->GetDetNz(detid);
        unsigned int nkind   = rpcalibv1->GetDetNkind(detid);

        for(unsigned int icent=0; icent<rpcalibv1->GetDetNcent(detid); icent++){
          for(unsigned int ihar=0; ihar<NHAR2D; ihar++){
            m_rawqxy[icent][ihar][idet] = new TH3*[nkind];

            for(unsigned int ikind=0; ikind<nkind; ikind++){
              if(rpcalibv1->isUsedQvectorForAnalysis(detid, ikind)){
                //--sprintf(name, "rawqxy_%s_c%d_h%d_k%d", detname.c_str(), icent, ihar, ikind); 
                sprintf(name, "rawqxy_%s_c%u_h%u_k%u", detname.c_str(), icent, ihar, ikind); 
                //if(icent==0&&ihar==0)
                //  cout<<name<<" "<<icent<<" "<<ihar<<" "<<idet<<" "<<ikind<<endl;

                //if(idet==19 || idet==20 || idet==21 ) HistoManager->registerHisto(new TH2F(name, name, 100., -10, 10, 100,-10 ,10 ) ); 
                //else                                  HistoManager->registerHisto(new TH2F(name, name, 100., -1, 1, 100,-1 ,1 ) ); 
                //
                float range = (m_RecalFlag==FLAG_RECENT) ? 1.0 : 3.0;
                HistoManager->registerHisto(new TH3F(name, name, 100,-range,range, 100,-range,range, nzbin,-0.5,nzbin-0.5) ); 
                m_rawqxy[icent][ihar][idet][ikind] = static_cast<TH3*>(HistoManager->getHisto(name));
              }
              else {
                m_rawqxy[icent][ihar][idet][ikind] = NULL;
              }
            }
          }
        }
      }

      // raw Q
      for(unsigned int idet=0; idet<vDetid.size(); idet++){
        int          detid   = vDetid[idet];

        if(!rpcalibv1->GetEnableDet(detid)) continue;

        string       detname = rpcalibv1->GetDetName(detid);
        unsigned int nkind   = rpcalibv1->GetDetNkind(detid);

        for(unsigned int icent=0; icent<rpcalibv1->GetDetNcent(detid); icent++) {
          for(unsigned int iz=0; iz<rpcalibv1->GetDetNz(detid); iz++) {
            for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++) {
              m_rawq[icent][iz][ihar][idet] = new TProfile*[nkind];

              for(unsigned int ikind=0; ikind<nkind; ikind++){
                if(rpcalibv1->isUsedQvectorForAnalysis(detid, ikind)){
                  sprintf(name, "rawq_%s_c%u_z%u_h%u_k%u", detname.c_str(), icent, iz, ihar, ikind); 
                  //if(icent==0&&iz==0&&ihar==0)
                  //  cout<<name<<" "<<icent<<" "<<iz<<" "<<ihar<<" "<<idet<<" "<<ikind<<endl;

                  HistoManager->registerHisto(new TProfile(name, name, 3, -0.5, 2.5, -50.0, 9000.0, "s"));//"S" option (error bar means RMS )
                  m_rawq[icent][iz][ihar][idet][ikind] = static_cast<TProfile*>(HistoManager->getHisto(name)); // average of Q and RMS
                }
                else {
                  m_rawq[icent][iz][ihar][idet][ikind] = NULL;
                }
              }
            }
          }
        }
      }
    }

    //------------------------------------------------------------------------------------
    // Histo to Generate Flattening Const.
    //------------------------------------------------------------------------------------
    if( m_RecalFlag == FLAG_FLAT || m_RecalFlag == FLAG_NOCALIB){
      //------Flattening-------  
      cout << "--- RP_CALIB_FLAT::Registering Histogram ---" << endl;
      const vector<int> &vDetid = rpcalibv1->GetEnableDetArray();
      for(unsigned int idet=0; idet<vDetid.size(); idet++){
        int          detid   = vDetid[idet];
        if(!rpcalibv1->GetEnableDet(detid)) continue;

        string       detname = rpcalibv1->GetDetName(detid);
        unsigned int nkind   = rpcalibv1->GetDetNkind(detid);
        unsigned int norder  = rpcalibv1->GetDetNorder(detid);

        for(unsigned int icent=0; icent<rpcalibv1->GetDetNcent(detid); icent++) {
          for(unsigned int iz=0; iz<rpcalibv1->GetDetNz(detid); iz++) {
            for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++) {
              m_flatpar[icent][iz][ihar][idet] = new TProfile*[nkind];
              m_rawrp[  icent][iz][ihar][idet] = new TH1*[nkind];

              for(unsigned int ikind=0; ikind<nkind; ikind++){
                if(rpcalibv1->isUsedQvectorForAnalysis(detid, ikind)){
                  sprintf(name,"flatpar_%s_c%u_z%u_h%u_k%u", detname.c_str(), icent, iz, ihar, ikind);
                  HistoManager->registerHisto( new TProfile(name,name, 2*norder, -0.5, 2*norder-0.5, -10.0, 10.0) ); // 2n:cos, 2n+1:sin, n=order
                  m_flatpar[icent][iz][ihar][idet][ikind] = static_cast<TProfile*>(HistoManager->getHisto(name));

                  sprintf(name,"rawrp_%s_c%u_z%u_h%u_k%u", detname.c_str(), icent, iz, ihar, ikind);
                  HistoManager->registerHisto( new TH1F(name,name, 100, -M_PI, M_PI) ); // 2n:cos, 2n+1:sin, n=order
                  m_rawrp[icent][iz][ihar][idet][ikind] = static_cast<TH1*>(HistoManager->getHisto(name));
                }
                else {
                  m_flatpar[icent][iz][ihar][idet][ikind] = NULL;
                  m_rawrp[  icent][iz][ihar][idet][ikind] = NULL;
                }
              }
            }
          }
        }
      }
    }
  }

//--  else if( m_MasterRecalFlag == 0 ){
//--     //Re-Centered Psi vs Flatten Psi 
//--  
//--     for(int icent=0;icent<RP::NMUL3;icent++){
//--       for(int iz=0;iz<RP::NZPS2;iz++){
//--         for(int idet=0; idet<NDet;idet++){
//--           sprintf(name, "FL_Psi1D_%d_%d_%d_%d", icent, iz, idet, 2); 
//--           if(idet==31 || idet==32 || idet==33 ) HistoManager->registerHisto(new TH1F(name, name, 100., -PI,PI)); 
//--           else HistoManager->registerHisto(new TH1F(name, name, 100., -PI/2.,PI/2.)); 
//--           FL_Psi1D[icent][iz][idet] =  static_cast<TH1F*>(HistoManager->getHisto(name));
//--         }
//--       }
//--     }
//--     
//--     //------Resolution-----
//--     // VTX - BBC (v2 - full ev class) //
//--     for(int idet=0;idet<NDet;idet++){
//--       for(int isn=0;isn<3;isn++){
//--         for(int ico=0;ico<2;ico++){
//--           sprintf(name,"Bbcsn%dVSVtxID%dH%d_%d",isn,idet,2,ico);
//--           HistoManager->registerHisto(new TProfile(name,name,RP::NMUL3,-0.5,RP::NMUL3-0.5,-2.0,2.0));   
//--           Vtx_Bbc_Reso[idet][isn][ico] = static_cast<TProfile*>(HistoManager->getHisto(name));
//--         }
//--       }
//--     }  
//--     
//--     for(int idet=0;idet<NDet;idet++){
//--       for(int ico=0;ico<2;ico++){
//--         sprintf(name,"CntVSVtxID%dH%d_%d",idet,2,ico);
//--         HistoManager->registerHisto(new TProfile(name,name,RP::NMUL3,-0.5,RP::NMUL3-0.5,-2.0,2.0));   
//--         Cnt_Reso[idet][ico] = static_cast<TProfile*>(HistoManager->getHisto(name));
//--       }
//--     }
//--     
//--     
//--     for(int ico=0;ico<2;ico++){
//--       for(int type=0;type<3;type++){
//--         sprintf(name,"Corre_Type%d_%d",type,ico);
//--         HistoManager->registerHisto(new TProfile(name,name,RP::NMUL3,-0.5,RP::NMUL3-0.5,-2.0,2.0));   
//--         Cor_S_N[type][ico] = static_cast<TProfile*>(HistoManager->getHisto(name));
//--       }
//--     }
//--     
//--     //for(int idet=0;idet<10;idet++){
//--     //  for(int ico=0;ico<2;ico++){
//--     //    sprintf(name,"Fvtx_cor%dH%d_%d",idet,2,ico);
//--     //    HistoManager->registerHisto(new TProfile(name,name,RP::NMUL3,-0.5,RP::NMUL3-0.5,-2.0,2.0));   
//--     //    Fvtx_S_N[idet][ico] = static_cast<TProfile*>(HistoManager->getHisto(name));
//--     //  }
//--     //}
//--     
//-- 
//--     for(int Imul=0;Imul<10;Imul++){
//--       for(int iz=0;iz<RP::NZPS2;iz++){
//--         //sprintf(name,"C%dZ%dCntdphidz",Imul,iz);
//--         //HistoManager->registerHisto(new TH2F(name,name,100,-0.002,0.002,100,-3,3));
//--         //cntdphidz[Imul][iz] = static_cast<TH2F*>(HistoManager->getHisto(name));
//--         //
//--         //sprintf(name,"C%dZ%dCnttrack",Imul,iz);
//--         //HistoManager->registerHisto(new TH2F(name,name,100,-0.35,0.35,100,-PI,PI));
//--         //cnttrack[Imul][iz] = static_cast<TH2F*>(HistoManager->getHisto(name));
//--       }
//--       
//-- /*
//--       for(int irpl=0;irpl<NDet;irpl++){
//--         for(int ico=0;ico<2;ico++){
//--           sprintf(name,"C%dRP%dCnt_Obv%d_%d",Imul,irpl,2,ico);
//--           HistoManager->registerHisto(new TProfile(name,name,20,0.0,5.0,-2.0,2.0));
//--           Cntvn_Ob[Imul][irpl][ico] = static_cast<TProfile*>(HistoManager->getHisto(name));
//--         }
//--       }
//-- */
//--     }
//--  }
  
  return;
}

//==============================================================
bool RPCalibrator::isBadTick(PreviousEvent *peve)
{
  if(peve==NULL){ return false; }

  int pticks[3]= {peve->get_clockticks(0), 
                    peve->get_clockticks(1),
                    peve->get_clockticks(2)};

  return ( ( 50<pticks[0]&&pticks[0]<120)||
           (700<pticks[1]&&pticks[1]<780) );
}
//==============================================================
//--int RPCalibrator::EventSkip()
//--{
//--  if( m_EventNumber <= 36000*(m_EvSkipFlag-1) )  return -1; // Skip
//--  else if ( m_EventNumber > 36000*m_EvSkipFlag ) return -2; // EndRun
//--  else return EVENT_OK;
//--}

bool RPCalibrator::fillAllRawQvector(const int detid, const int icent, const int iz, RpSumXYObject *sumxy)
{
  if(!rpcalibv1->GetEnableDet(detid)) return false;

  if(iz<0) return false;

  bool status = true;
  for(unsigned int ikind=0; ikind<rpcalibv1->GetDetNkind(detid); ikind++){ 
    if( !rpcalibv1->isUsedQvectorForAnalysis(detid, ikind) ) continue;

    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++){
      // fill Qvector
      status &= fillRawQvector(detid, ikind, ihar, icent, iz, sumxy);
    }
  }

  return status;
}

bool RPCalibrator::fillRawQvector(
  const int detid, const unsigned int ikind, const unsigned int ihar,
  const int icent, const int iz, RpSumXYObject* sumxy)
{
  int idcode = RP::calcIdCode(detid, ikind, ihar);
  RpSnglSumXY* qvect = sumxy->getRpSumXY(idcode);
  if(qvect==NULL)
    {
      //cerr<<"No Qvect : detid:ikind:ihar = "<<detid<<" "<<ikind<<" "<<ihar<<endl;
      return false;
    }

  int idx = rpcalibv1->getDetIdx(detid);
  if(idx<0)
    {
      cerr<<"Unknown DetIdx "<<idx<<endl;
      return false;
    }

  
  float qx = qvect->QVector(0);
  float qy = qvect->QVector(1);
  float qw = qvect->Weight();
  if(qw<=0.0) return false;

  ///////////////////////////////
  // raw q vector
  float qxw = (m_RecalFlag==FLAG_RECENT) ? qx/qw : qx;
  float qyw = (m_RecalFlag==FLAG_RECENT) ? qy/qw : qy;
  
  bool status=true;
  if(m_rawq[icent][iz][ihar][idx][ikind]!=NULL){
    m_rawq[icent][iz][ihar][idx][ikind]->Fill(0., qxw); // Fill pQvectors for Re_centering, 
    m_rawq[icent][iz][ihar][idx][ikind]->Fill(1., qyw); // Fill pQvectors for Re_centering, 
    m_rawq[icent][iz][ihar][idx][ikind]->Fill(2., qw);  // Fill pQvectors for Re_centering, 
  }
  else {
    cerr<<"histo rawq is null : "<<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<endl;
    status=false;
  }

  if( ihar<NHAR2D ) {
    if(m_rawqxy[icent][ihar][idx][ikind]!=NULL){
      m_rawqxy[icent][ihar][idx][ikind]->Fill(qxw, qyw, iz);
    }
    else {
      cerr<<"histo rawqxy is null : "<<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<endl;
      status=false;
    }
  }

  return status;
}

bool RPCalibrator::fillAllFlatpar(const int detid, const int icent, const int iz, RpSumXYObject *sumxy, ReactionPlaneObject *rp)
{
  if(!rpcalibv1->GetEnableDet(detid)) return false;

  if(iz<0){ return false; }

  bool status=true;
  for(unsigned int ikind=0; ikind<rpcalibv1->GetDetNkind(detid); ikind++){ 
    if( !rpcalibv1->isUsedQvectorForAnalysis(detid, ikind) ) continue;  

    for(unsigned int ihar=0; ihar<rpcalibv1->GetDetNhar(detid); ihar++){
      status &= fillFlatpar(detid, ikind, ihar, icent, iz, sumxy, rp);
    } //ihar
  }

  return status;
}

bool RPCalibrator::fillFlatpar(
  const int detid, const unsigned int ikind, const unsigned int ihar, 
  const int icent, const int iz, RpSumXYObject *sumxy, ReactionPlaneObject *rp)
{

  int idcode = RP::calcIdCode(detid, ikind, ihar);
  RpSnglSumXY* qvect = sumxy->getRpSumXY(idcode);
  if(qvect==NULL){ return false; }

  int idx = rpcalibv1->getDetIdx(detid);
  if(idx<0) { cerr<<"Unknown DetIdx "<<idx<<endl; return false; }

  // this is recentered Q vector
  float qx = qvect->QVector(0);
  float qy = qvect->QVector(1);
  float qw = qvect->Weight();

  if(qw<=0.0) return false;

  ///////////////////////////
  // flattening calibration
  //float psi = atan2(qy, qx)/(ihar+1.0);
      //float fCos = cos((ihar+1.0)*(ordPsi));
      //float fSin = sin((ihar+1.0)*(ordPsi));

  float psi = atan2(qy, qx);

  bool status=true;
  if(m_flatpar[icent][iz][ihar][idx][ikind]!=NULL){
    for(unsigned int iord=0; iord<rpcalibv1->GetDetNorder(detid); iord++){
      float ordPsi = (iord+1.0)*psi;
      float fCos   = cos(ordPsi);
      float fSin   = sin(ordPsi);

      m_flatpar[icent][iz][ihar][idx][ikind]->Fill(2*iord+0, fCos);
      m_flatpar[icent][iz][ihar][idx][ikind]->Fill(2*iord+1, fSin);
    }
  }
  else {
    cerr<<"histo flatpar is null : "<<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<endl;
    status=false;
  }

  if(m_RecalFlag==FLAG_NOCALIB){
    ReactionPlaneSngl* rpSngl = rp->getReactionPlane(idcode);
    if(rpSngl==NULL){ return false; }

    psi = rpSngl->GetPsi();
  }

  if(m_rawrp[icent][iz][ihar][idx][ikind]!=NULL){
    m_rawrp[icent][iz][ihar][idx][ikind]->Fill(psi);
  }
  else {
    cerr<<"histo rawrp is null : "<<detid<<" "<<ikind<<" "<<ihar<<" "<<icent<<" "<<iz<<endl;
    status=false;
  }

  return status;
}

