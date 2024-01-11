#include <vector>
#include <cstdlib>
#include "gsl/gsl_rng.h"
#include "Fun4AllReturnCodes.h"
#include "RPCalib_priv.h"
#include <recoConsts.h>
#include "BbcOut.h"
#include "BbcEvent.hh"
#include "SvxClusterList.h"
#include "SvxCluster.h"
#include "SvxSegmentList.h"
#include "SvxSegment.h"
#include "PHGlobal.h"
#include "PreviousEvent.h"
#include "getClass.h"
#include "Fun4AllHistoManager.h"
#include "TProfile.h"
#include "TH1.h"
#include "TH2.h"
#include "RpConst.h"
#include "RpSnglSumXY.h"
#include "RpSumXYObject.h"
#include "VtxOut.h"

#include "TFile.h"
#include "TTree.h"

#include "ReactionPlaneObject.h"
#include "ReactionPlaneSngl.h"
#include "ReactionPlaneCalibv1.h"

#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"
#include "EventHeader.h"
#include "compactCNT/SvxCentralTrackMap.h"
#include "compactCNT/SvxCentralTrackMapEntry.h"

using namespace RP;
using namespace std;
using namespace findNode;

//==============================================================
RPCalib_priv::RPCalib_priv(const char* output, int agsegnum) : OutputFileName(output)
{
  ThisName = "RPCalib_priv";
  m_EventNumber = 0;
  m_EvtSequence = 0;
  m_ievtsq = 0;
	rpcalibv1 = 0;
	m_agsegnum = agsegnum;
  m_RecalFlag = nocalib_flag;
  HistoManager = new Fun4AllHistoManager(OutputFileName.c_str());
}
//==============================================================
RPCalib_priv::~RPCalib_priv()
{
	delete rpcalibv1;
}
//==============================================================
int RPCalib_priv::Init(PHCompositeNode *topNode)
{
  cout << "RPCalib_priv::Init started..." << endl;
  cout << "m_RecalFlag = " << m_RecalFlag << endl;
  cout << "RPCalib_priv::Init ended." << endl;

  return 0;
}
//==============================================================
int RPCalib_priv::InitRun(PHCompositeNode *topNode)
{
  cout << "RPCalib_priv::InitRun started..." << endl;
  
  calFlag=2;
  int icalibversion = 4002; // after Run5 and Field_ON
  char rpcalib[100], centcalib[100];

  // Get Run Number //
  recoConsts *rc = recoConsts::instance();
  m_runnumber = rc->get_IntFlag("RUNNUMBER");

  cout << "M_RECAL_FLAG IS " << m_RecalFlag << endl;

  // Open Root File //
  if( m_RecalFlag == nocalib_flag ){
  //if( m_RecalFlag == recent_flag ){
  //if( m_RecalFlag == flat_flag ){
    char output[100];
    //sprintf(output, "RPCalib_NEWSUMXY/RUN347128/W_MASK/owncalibpar/RPCalib_%d-%d.root", m_runnumber, m_agsegnum);
    sprintf(output, "RPCalib_NEWSUMXY/RUN349679/W_MASK/othercalibpar/WO_MASK/RPCalib_%d-%d.root", m_runnumber, m_agsegnum);
    OutputNtupleFile = new TFile(output, "RECREATE");
    cout << "RPCalib_priv::InitRun: output file " << output << " opened." << endl;
  }

  // Register Histogram //
//   initHisto();
  // Initialization of TTree //
//   initEvtTree();
  initClstTree();

  // Read Calibration Data //
  if( m_RecalFlag != recent_flag ){
    //sprintf(rpcalib, "Calib_para_%d-%d.dat", m_runnumber, m_agsegnum);
    sprintf(rpcalib, "Calib_para_%d-0.dat", m_runnumber);
    initRpCalibData(rpcalib);
  }
  sprintf(centcalib, "copyof_CentCalib_para/centcalib_%d-%d.dat", m_runnumber, m_agsegnum);
  initCentCalibData(centcalib);

  
  cout << "RPCalib_priv::InitRun - run number: " << m_runnumber << endl;
  cout << "RPCalib_priv::InitRun - calibration version:  " << icalibversion << endl;
  rc->set_IntFlag("BBCCALIBVERSION", icalibversion);
  cout << "RPCalib_priv::InitRun ended." << endl;

  return 0;    
}
//==============================================================
int RPCalib_priv::process_event(PHCompositeNode *topNode)
{
  m_EventNumber++;

  ////////////////
  // Event Skip //
	//if( EventSkip() == -1 ) return ABORTEVENT;
	//else if( EventSkip() == -2 ) return ABORTRUN;

  if(m_EventNumber%10000==0) cout << m_EventNumber<< "//------------------------------------" << endl;
  
  PHGlobal       *global  = getClass<PHGlobal>(topNode, "PHGlobal");
  BbcOut         *bbc_out = getClass<BbcOut>(topNode, "BbcOut");
  VtxOut         *vtxout  = getClass<VtxOut>(topNode, "VtxOut");
  RpSumXYObject  *sumxy   = getClass<RpSumXYObject>(topNode, "RpSumXYObject");
  PreviousEvent  *peve    = getClass<PreviousEvent>(topNode, "PreviousEvent"); // Tick
  EventHeader    *evhead  = getClass<EventHeader>(topNode, "EventHeader");
  SvxClusterList *cluster = getClass<SvxClusterList>(topNode, "SvxClusterList");

  if(!global){
      cerr << PHWHERE << " No PHGlobal object !" << endl;
      return ABORTEVENT;
  }
  if(!vtxout){
      cerr << PHWHERE << " No VtxOut object !" << endl;
      return ABORTEVENT;
  }
  if(!peve){
      cerr << PHWHERE << " No PreviousEvent object !" << endl;
      return ABORTEVENT;
  }
  if(!evhead){
      cerr << PHWHERE << " No EventHeader object !" << endl;
      return ABORTEVENT;
  }
  if(!sumxy){
      cerr << PHWHERE << " No RpSumXYObject object !" << endl;
      return ABORTEVENT;
  }
  if(!cluster){
      cerr << PHWHERE << " No SvxClusterList object !" << endl;
      return ABORTEVENT;
  }

  ////////////////
  // Tick Cut //
  for( int i = 0; i < 3; i++ )
    m_pticks[i] = peve->get_clockticks(i);
  if( TickCut() != EVENT_OK ) return ABORTEVENT;

  ////////////////////////
  // Get Event Sequence //
  m_EvtSequence = evhead->get_EvtSequence();
  if( m_EvtSequence%500 )
    m_ievtsq++;

  ////////////////
  // Centrality //
  float bbccha_s = -9999;
  float bbccha_n = -9999;
  float bbccha_t = -9999;
  if(global){
    bbccha_s = global->getBbcChargeS();
    bbccha_n = global->getBbcChargeN();
  }
  else if(bbc_out){
		bbccha_s = bbc_out->get_ChargeSum(0);
    bbccha_n = bbc_out->get_ChargeSum(1);
  }
  if(bbccha_s>-9000 && bbccha_n>-9000)
    bbccha_t = bbccha_s + bbccha_n;    
  else bbccha_t=-9999;
  m_bbcq = bbccha_t;

  if(bbccha_t < -9000){
    cout << "no BBCcharge available, skip the event" << endl;
    return 0;
  }
  
//   // Temporaly Definition of Centrality //
//   int icent = ncent-1;
// 	for( int jcent = 0; jcent < ncent-1; jcent++ ){
// 		if( bbccha_t > 2500.0*m_bbccut[jcent]/5000.0 ) icent = ncent-jcent-2;
// 	}
//   //cout << icent << endl;
  const int icenttmp = (int)global->getCentrality();
  m_icent = icenttmp;
  int icent = rpcalibv1->GetCentralityBin(icenttmp);
  if( icent < 0 ) return EVENT_OK;

  //////////////////
  // Z_Vertex Cut //
  float vertex=-9999;
  float bbcz=-9999;
  float vtxz=-9999;

  if(vtxout&&vtxout->isVtx("SVX")) vtxz = vtxout->get_ZVertex("SVX");
	m_zvtx = vtxz;

  if(global)
    bbcz=global->getBbcZVertex();
  else if(bbc_out)
    bbcz=bbc_out->get_VertexPoint();
  m_bbcz = bbcz;

  if(vtxz>-9000) vertex=vtxz;
  else if(bbcz>-9000) vertex=bbcz;
  else vertex=-9999;
  
  float zlim = 10.0; // +-10cm window
  int izv = (int)(nzv*(vertex + zlim)/(2.0*zlim));
  if (vertex < -zlim || vertex > zlim) return EVENT_OK; 
  if(izv<0 || izv>=nzv ) { return EVENT_OK;} 
  
  if(vertex < -9000){
    cout <<"no Z Vertex available ,skip the event" << endl;
    return 0;
  }

  //////////////////////
  // Q-Vector Section //
  float Q[ndet2][nhar][3], Mclus[4];  
  float ave_co_si;
  float f_co_si;

  // Initialization
  for(int ilayer = 0; ilayer < 4; ilayer++){
    Mclus[ilayer]=0;
    m_nclst_egap1[ilayer] = 0;
    m_nclst_egap2[ilayer] = 0;
  }
  m_nclstsum_bbc = 0;

  //cout << "Initialization of Q-vector" << endl;
  for(int idet2 = 0; idet2 < ndet2; idet2++) // ndet1 = 76 ndet2 = 88
    for(int ihar = 0; ihar < nhar; ihar++)  // nhar = 6
      for(int i = 0; i < 3; i++) // i = 0 ; x, i = 1 : y, i = 2 : weight
        Q[idet2][ihar][i] = 0.0;

  if(sumxy == NULL){
    if(verbosity > 0)
      cout << PHWHERE << "Cannot find sumxy." << endl;
    return ABORTRUN;
  }

  // Mask Sensor //
  if(m_VnFlag == 1 && MaskSensor(cluster) != EVENT_OK) return(ABORTEVENT);

  return 0;

  // Get Q-Vector from RpSumXYObject //
  if (sumxy) {
    //VTX
    for(int idet1=0;idet1<mdet;idet1++){ // mdet = 62->74 : SVX
      for(int ihar=0;ihar<6;ihar++){
        int id=RP::ID_SVX;
        int idcode = RP::calcIdCode(id, idet1, ihar);
        float qx = sumxy->getRpSumXY(idcode)->QVector(0);
        float qy = sumxy->getRpSumXY(idcode)->QVector(1);
        float qw = sumxy->getRpSumXY(idcode)->Weight();
        Q[idet1][ihar][0]=qx;
        Q[idet1][ihar][1]=qy;
        Q[idet1][ihar][2]=qw;
        //if( idet1 == 52 ) cout << "######### " << m_EventNumber << " " << ihar << " "  << qx << " " << qy << endl;
      }
      if( 0<=idet1 && idet1<12) Mclus[0]+=Q[idet1][0][2]; // barrel 0
      if(12<=idet1 && idet1<22) Mclus[1]+=Q[idet1][0][2]; // barrel 1
      if(22<=idet1 && idet1<30) Mclus[2]+=Q[idet1][0][2]; // barrel 2
      if(30<=idet1 && idet1<36) Mclus[3]+=Q[idet1][0][2]; // barrel 3
      if(( 0<=idet1 && idet1<= 4) || ( 7<=idet1 && idet1<=11)) m_nclst_egap1[0]+=Q[idet1][0][2]; // barrel 0
      if((12<=idet1 && idet1<=15) || (18<=idet1 && idet1<=21)) m_nclst_egap1[1]+=Q[idet1][0][2]; // barrel 1
      if((22<=idet1 && idet1<=24) || (27<=idet1 && idet1<=29)) m_nclst_egap1[2]+=Q[idet1][0][2]; // barrel 2
      if((30<=idet1 && idet1<=31) || (34<=idet1 && idet1<=35)) m_nclst_egap1[3]+=Q[idet1][0][2]; // barrel 3
      if(( 0<=idet1 && idet1<= 3) || ( 8<=idet1 && idet1<=11)) m_nclst_egap2[0]+=Q[idet1][0][2]; // barrel 0
      if((12<=idet1 && idet1<=14) || (19<=idet1 && idet1<=21)) m_nclst_egap2[1]+=Q[idet1][0][2]; // barrel 1
      if((22<=idet1 && idet1<=23) || (28<=idet1 && idet1<=29)) m_nclst_egap2[2]+=Q[idet1][0][2]; // barrel 2
      if((30<=idet1 && idet1<=30) || (35<=idet1 && idet1<=35)) m_nclst_egap2[3]+=Q[idet1][0][2]; // barrel 3
    }
    //MPC
    for(int idet1=0;idet1<3;idet1++){
      for(int ihar=0;ihar<nhar;ihar++){
        int id=RP::ID_MPC;
        int idcode=RP::calcIdCode(id,idet1,ihar);
        float qx=sumxy->getRpSumXY(idcode)->QVector(0);
        float qy=sumxy->getRpSumXY(idcode)->QVector(1);
        float qw=sumxy->getRpSumXY(idcode)->Weight();
        Q[idet1+mdet][ihar][0]=qx;
        Q[idet1+mdet][ihar][1]=qy;
        Q[idet1+mdet][ihar][2]=qw;
      }
    }
    //BBC
    for(int idet1=0;idet1<3;idet1++){
      for(int ihar=0;ihar<nhar;ihar++){
        int id=RP::ID_BBC;
        int idcode=RP::calcIdCode(id,idet1,ihar);
        float qx=sumxy->getRpSumXY(idcode)->QVector(0);
        float qy=sumxy->getRpSumXY(idcode)->QVector(1);
        float qw=sumxy->getRpSumXY(idcode)->Weight();
        Q[idet1+mdet+3][ihar][0]=qx;
        Q[idet1+mdet+3][ihar][1]=qy;
        Q[idet1+mdet+3][ihar][2]=qw;
      }
    }  
    m_nclstsum_bbc+=Q[79][0][2];
    ///SMD///
    for(int idet1=0;idet1<3;idet1++){
      for(int ihar=0;ihar<1;ihar++){
        int id=RP::ID_SMD;
        int idcode = RP::calcIdCode(id, idet1, ihar);
        float qx = sumxy->getRpSumXY(idcode)->QVector(0);
        float qy = sumxy->getRpSumXY(idcode)->QVector(1);
        float qw = sumxy->getRpSumXY(idcode)->Weight();
        Q[idet1+mdet+6][ihar][0]=qx;
        Q[idet1+mdet+6][ihar][1]=qy;
        Q[idet1+mdet+6][ihar][2]=qw;
      }
    }
    ///CNT///
    for(int idet1=0;idet1<5;idet1++){
      for(int ihar=0;ihar<nhar;ihar++){
        int id=RP::ID_CNT;
        int idcode = RP::calcIdCode(id, idet1, ihar);
        float qx = sumxy->getRpSumXY(idcode)->QVector(0);
        float qy = sumxy->getRpSumXY(idcode)->QVector(1);
        float qw = sumxy->getRpSumXY(idcode)->Weight();
        Q[idet1+mdet+9][ihar][0]=qx;
        Q[idet1+mdet+9][ihar][1]=qy;
        Q[idet1+mdet+9][ihar][2]=qw;
      }
    }

    // Get Number of Clusters //
    for( int i = 0; i < 4; i++ )
      m_nclst[i] = (int)Mclus[i];

    /////////////////////////////////
    // Re-centering and Flattening //
    float Re_Q_tmp[ndet2][nhar][2];
    float Re_Q[ndet2][nhar][3];     // Q-vector after re-centering
    float Re_Psi[ndet2][nhar];      // Psi after re-centering
    float F_Q[ndet2][nhar][2][nfn];   // Q-vector after flattening
    float dPsi[ndet2][nhar][nfn];
    float FPsi[ndet2][nhar][nfn];
    float F_Psi[ndet2][nhar][nfn];    // Psi after flattening

    // Initialization //
    for(int idet2 = 0; idet2 < ndet2; idet2++){ // ndet2 = 88
      for(int ihar = 0; ihar < nhar; ihar++){
        for(int ico = 0; ico < 2; ico++){
          Re_Q_tmp[idet2][ihar][ico] = 0.0;
          Re_Q[idet2][ihar][ico] = 0.0;
        }
        //Q_tmp[idet2][ihar][2] = 0.0;
        Re_Q[idet2][ihar][2] = 0.0;
        for( int inf = 0; inf < nfn; inf++ ){
          F_Psi[idet2][ihar][inf] = -999.0;
        }
      }
    }
		for( int idet3 = 0; idet3 < ndet3+13; idet3++ )
			for( int ico = 0; ico < 3; ico++ )
				m_Q[idet3][ico] = -999.0;
		for( int idet3 = 0; idet3 < ndet3; idet3++ ){
			m_Re_Psi2[idet3] = -999.0;
			for( int ico = 0; ico < 2; ico++ )
				m_Re_Q[idet3][ico] = -999.0;
			for( int inf = 0; inf < nfn; inf++ ){
				m_F_Psi2[idet3][inf] = -999.0;
				for( int ihar = 0; ihar < nhar; ihar++ ){
					m_F_Psi[idet3][ihar][inf] = -999.0;
					m_F_Psi7[idet3][ihar] = -999.0;
				}
			}
		}
    for( int ihar = 0; ihar < nhar; ihar++ ){
      m_Cosnpsi_BBC[ihar] = -999.0;
      m_Cosnpsi_Egap1[ihar] = -999.0;
      m_Cosnpsi_Egap2[ihar] = -999.0;
    }

    // Re-Centering and Flattening //
    for(int ihar = 0; ihar < nhar; ihar++){
      for(int idet2 = 0; idet2 < ndet2; idet2++){ // ndet2 = 88
        if( DetSkip(idet2) != EVENT_OK) continue;
        int noise2 = 3;
        if((Q[idet2][ihar][0] == 0.0 && Q[idet2][ihar][1] == 0.0) || Q[idet2][ihar][2] == 0.0){
          noise2 = 0;
        }
        else noise2 = 1;
        if( noise2 == 1 ){
          //////////////////
          // Re-centering //
          for(int ico = 0; ico < 2; ico++){
            ave_co_si = Q[idet2][ihar][ico]/Q[idet2][ihar][2]; // normarized w.r.t. number of events
            //ave_co_si = Q[idet2][ihar][ico];

            if( m_RecalFlag == recent_flag ){
              q[icent][izv][idet2][ihar]->Fill(ico, ave_co_si); // Fill parameters for Re_centering
            }
						if( RMS[icent][izv][idet2][ihar][ico]!=0 ){
              Re_Q_tmp[idet2][ihar][ico] = (ave_co_si - ave[icent][izv][idet2][ihar][ico])/RMS[icent][izv][idet2][ihar][ico]; //Calibrated Q-vectors
              // Q-Vectors defined by SVX Clusters & Segments, MBC, BBC, SMD, CNT//
              Re_Q[idet2][ihar][ico] = Re_Q_tmp[idet2][ihar][ico]*Q[idet2][ihar][2];
              //Re_Q[idet2][ihar][ico] = Re_Q_tmp[idet2][ihar][ico];
            }
          } // ico
          Re_Q[idet2][ihar][2] = Q[idet2][ihar][2];
        } // noise2
      } // idet2

      ////////////////
      // Flattening //
      if( m_RecalFlag != recent_flag ){
        for(int idet2 = 0; idet2 < ndet2; idet2++){ // ndet2 = 88
          if( DetSkip(idet2) != EVENT_OK) continue;
          int noise2 = 3;
          if( (Re_Q[idet2][ihar][0] == 0.0 && Re_Q[idet2][ihar][1] == 0.0) || Re_Q[idet2][ihar][2] == 0.0 )
            noise2 = 0;
          else noise2 = 1;
          if(noise2 == 1){
            float DPsi = 0.0; 
            Re_Psi[idet2][ihar] = atan2(Re_Q[idet2][ihar][1], Re_Q[idet2][ihar][0])/(ihar+1.0);

            for(int inf = 0; inf < nfn; inf++){
              F_Q[idet2][ihar][0][inf] = cos((inf+1.0)*(ihar+1.0)*Re_Psi[idet2][ihar]);
              F_Q[idet2][ihar][1][inf] = sin((inf+1.0)*(ihar+1.0)*Re_Psi[idet2][ihar]);

              dPsi[idet2][ihar][inf] = (-fla[icent][izv][idet2][ihar][1][inf]*F_Q[idet2][ihar][0][inf]
                                        +fla[icent][izv][idet2][ihar][0][inf]*F_Q[idet2][ihar][1][inf])
                                       *2.0/((inf+1.0)*(ihar+1.0));

              DPsi += dPsi[idet2][ihar][inf];

              FPsi[idet2][ihar][inf] = Re_Psi[idet2][ihar] + DPsi;
              F_Psi[idet2][ihar][inf] = atan(sin(FPsi[idet2][ihar][inf])/cos(FPsi[idet2][ihar][inf]));
            } // inf
            // Fill flattening parameters //
            if( m_RecalFlag == flat_flag ){
              for(int ico = 0; ico < 2; ico++){
                for(int inf = 0; inf < nfn; inf++){ // nfn = 8
                  f_co_si = F_Q[idet2][ihar][ico][inf];
                  f_q[icent][izv][idet2][ihar][ico]->Fill(inf, f_co_si); // parameter of Flatterning
                }
              }
            }
          } // noise
        } // idet2
      } // m_RecalFlag != recent_flag
    } // ihar

    if ( m_RecalFlag == nocalib_flag ){
      ////////////
      // Ntuple //
			int idet3 = 0;
      for( int idet2 = 0; idet2 < ndet2; idet2++ ){ // ndet2 = 88
        if( DetSkip(idet2) != EVENT_OK) continue;
        m_type = idet2;
        if( m_type < ndet2 ){ // ndet2 = 88
          if( !((Q[idet2][1][0] == 0.0 && Q[idet2][1][1] == 0.0) || Q[idet2][1][2] == 0.0) ){
            m_Q[idet3][0] = Q[m_type][1][0]; // SMD : ihar < 1
            m_Q[idet3][1] = Q[m_type][1][1];
            m_Q[idet3][2] = Q[m_type][1][2];
          }
        }
        if( !((Re_Q[idet2][1][0] == 0.0 && Re_Q[idet2][1][1] == 0.0) || Re_Q[idet2][1][2] == 0.0) ) {
          m_Re_Q[idet3][0] = Re_Q[m_type][1][0];
          m_Re_Q[idet3][1] = Re_Q[m_type][1][1];
          m_Re_Psi2[idet3] = Re_Psi[m_type][1];
          for( int inf = 0; inf < nfn; inf++ ){
            m_F_Psi2[idet3][inf] = F_Psi[m_type][1][inf];
            for( int ihar = 0; ihar < nhar; ihar++ ){
              m_F_Psi[idet3][ihar][inf] = F_Psi[m_type][ihar][inf];
							if( inf == 7 ) m_F_Psi7[idet3][ihar] = F_Psi[m_type][ihar][inf];
            }
          } // inf
        }
				idet3++;
      } // idet2
      m_cent = icent;
      m_nfn = nfn;

      // EACH LAYER //
      for( int i = 48; i < 61; i++ )
        if( !((Q[i][1][0] == 0.0 && Q[i][1][1] == 0.0) || Q[i][1][2] == 0.0) )
          for( int j = 0; j < 3; j++ )
            m_Q[i-9][j] = Q[i][1][j];

//       if( CalcVnReso(topNode, icent) == ABORTEVENT ) return ABORTEVENT;
      if( m_VnFlag == 1 ){
        if( GetVn(topNode, icent) == ABORTEVENT ) return ABORTEVENT;
        GetQAHistos(icent, izv);
        ntp->Fill();
      }
      //ntp->Fill();

      ////////////////
      // Resolution //
      for(int idet = 0; idet < 6; idet++){
        // Psi for SVX
        if(F_Psi[idet+mdet][1][7] > -900.)
          psi_hist[icent][idet]->Fill(F_Psi[idet+mdet][1][7]);
      }
      for(int idet = 0; idet < mdet+12; idet++){
        float dpsi = F_Psi[idet][1][7] - F_Psi[79][1][7]; // ihar = 1
        float cor_cos = cos(2.0*(dpsi));
        float cor_sin = sin(2.0*(dpsi));
        //if(F_Psi[idet][1][7] > -900.){
        if(F_Psi[idet][1][7] > -900. && F_Psi[79][1][7] > -900.){
          vtx_bbc_reso[idet][0]->Fill(icent, cor_cos);
          vtx_bbc_reso[idet][1]->Fill(icent, cor_sin);
        }
      }

      //              |      all eta         | e gap 1 | e gap 2 |
      //               ALL   S   N    S   N    S/N S/N  MPC  BBC
      int Type1[9] = {52,  68, 69,   71, 72,   68, 71,  74, 77}; // idet2
      int Type2[9] = {79,  79, 79,   79, 79,   69, 72,  75, 78}; // idet2
      for(int type = 0; type < 9; type++){
        float dpsi = F_Psi[Type1[type]][1][7] - F_Psi[Type2[type]][1][7]; // ihar = 1
        if(  F_Psi[Type1[type]][1][7] > -900. &&  F_Psi[Type1[type]][1][7] > -900. ){
          cor_S_N[type][0]->Fill(icent, cos(2.0*dpsi));
          cor_S_N[type][1]->Fill(icent, sin(2.0*dpsi));
        }
      }
    } // m_RecoFlag = nocalib_flag
  } // sumxy
 
  if( m_MasterRecalFlag == 0 ){ // when you read from recalibrated DST files
    ////////////////////////////////////////////////////////
    // Additional Code to check correlation from new DSTs //
    ReactionPlaneObject *rpobject = getClass<ReactionPlaneObject>(topNode,"ReactionPlaneObject");
    if(rpobject == NULL){
      if(verbosity > 0) cout << PHWHERE << "Cannot find rpobject." << endl;
      return 0;
    }
    float RP[ndet3];
    // Get calibrated reaction plane from ReactionPlaneObject
    for(int idet = 0; idet < ndet3; idet++) // ndet3 = 39
      RP[idet] = -9999.0;
    for(int idet = 0; idet < ndet3; idet++){
      int idcode;
      if(     idet < (mmdet+25))   idcode=RP::calcIdCode(RP::ID_SVX,idet,1); // ihar = 1
      else if(idet < (mmdet+25+3)) idcode=RP::calcIdCode(RP::ID_MPC,idet-(mmdet+25),1); // ihar = 1
      else if(idet < (mmdet+25+6)) idcode=RP::calcIdCode(RP::ID_BBC,idet-(mmdet+25+3),1); // ihar = 1
      else if(idet < (mmdet+25+9)) idcode=RP::calcIdCode(RP::ID_SMD,idet-(mmdet+25+6),0); // ihar = 0
      else idcode=RP::calcIdCode(RP::ID_CNT,idet-(mmdet+25+9),1); // ihar = 1
      RP[idet] = rpobject->getReactionPlane(idcode)->GetPsi(); // ihar = 1 or 0
    }
    
    //vxt each eta region vs BBC correlation
    for(int idet=0;idet<mmdet+12;idet++){
      if(RP[idet]>-9000 && RP[30] >-9000){
        float dpsi = RP[idet] - RP[30];
        float cor_cos = cos(2.0*(dpsi)); // ihar = 1
        float cor_sin = sin(2.0*(dpsi)); // ihar = 1
        Vtx_bbc_reso[idet][0]->Fill(icent, cor_cos);
        Vtx_bbc_reso[idet][1]->Fill(icent, cor_sin);
      }
    }
    //              |      all eta         | e gap 1 | e gap 2 |
    //               ALL   S   N    S   N    S/N S/N  MPC  BBC
    int Type1[9] = {12,  19, 20,   22, 23,   19, 22,  25, 28}; // idet2
    int Type2[9] = {30,  30, 30,   30, 30,   20, 23,  26, 29}; // idet2
    for(int type = 0; type < 9; type++){
      if(RP[Type1[type]] > -9000 && RP[Type2[type]] > -9000){
        float dpsi = RP[Type1[type]] - RP[Type2[type]];
        Cor_S_N[type][0]->Fill(icent, cos(2.0*dpsi)); // ihar = 1
        Cor_S_N[type][1]->Fill(icent, sin(2.0*dpsi)); // ihar = 1
      }
    }
  } // m_MasterRecalFlag == 0
  
  return 0;
}
//==============================================================
int RPCalib_priv::End(PHCompositeNode *topNode)
{
  cout << "RPCalib_priv::End" << endl;
  
  EndCalibData();

  return 0; 
}
//==============================================================
void RPCalib_priv::initHisto()
{
  char name[200];

  int laddermax;
  for( int ilayer = 0; ilayer < 2; ilayer++ ){
    if(ilayer == 0) laddermax = 10;
    else laddermax = 20;
    for( int iladder = 0; iladder < laddermax; iladder++ ){
      for( int isensor = 0; isensor < 4; isensor++ ){
        for( int ichip = 0; ichip < 4; ichip++ ){
          sprintf(name, "clstB%dL%dS%dC%d", ilayer, iladder, isensor, ichip); 
          HistoManager->registerHisto(new TProfile(name, name, 100., -0.5, 99.5, 0., 1000., ""));//"S" option (error bar means RMS )
          clusternum[ilayer][iladder][isensor][ichip] = static_cast<TProfile*>(HistoManager->getHisto(name)); // average of Q and RMS
        }
      }
    }
  }

  return;

  for(int idet = 0; idet < 6; idet++){
    for(int icent = 0; icent < ncent; icent++){
      sprintf(name,"psi1D%dC%d",idet+mdet,icent);
      HistoManager->registerHisto(new TH1F(name, name, 200, -3.14, 3.14));   
      psi_hist[icent][idet] = static_cast<TH1F*>(HistoManager->getHisto(name));
    }
  }
	if( m_MasterRecalFlag == 1 ){
		for(int ico=0;ico<2;ico++){
			for(int idet=0;idet<mmdet+12;idet++){
				sprintf(name,"BBCSNvsVTXID%d_%d",idet,ico);
				HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
				Vtx_bbc_reso[idet][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
			}
			for(int type=0;type<9;type++){
				sprintf(name,"CORRE_Type%d_%d",type,ico);
				HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
				Cor_S_N[type][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
			}
		}
	}
  for(int ico=0;ico<2;ico++){
    for(int type=0;type<9;type++){
      sprintf(name,"corre_Type%d_%d",type,ico);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
      cor_S_N[type][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
    }
  }

  HistoManager->registerHisto(BBC_charge = new TH1F("bbc","bbc",500,0,2500));
  HistoManager->registerHisto(z_vertex   = new TH1F("vertex","vertex",400,-20,20));

  if( m_RecalFlag == recent_flag ){
  //-----Re_centering-----
    cout << endl;
    cout << "--- RP_CALIB_RECENT_M::Registering Histogram ---" << endl;
    for(int icent = 0; icent < ncent; icent++)        
      for(int izv = 0; izv < nzv; izv++)
        for(int idet12 = 0; idet12 < ndet2; idet12++)
          for(int ihar = 0; ihar < nhar; ihar++){
            if( DetSkip(idet12) != EVENT_OK) continue;
            sprintf(name, "C%dZ%dD%dRe_Q%d", icent, izv, idet12, ihar+1); 
            HistoManager->registerHisto(new TProfile(name, name, 3.0, -0.5, 2.5, -2.0, 2.0, "s"));//"S" option (error bar means RMS )
            //HistoManager->registerHisto(new TProfile(name, name, 3.0, -0.5, 2.5, -500.0, 2000.0, "s"));//"S" option (error bar means RMS )
            q[icent][izv][idet12][ihar] = static_cast<TProfile*>(HistoManager->getHisto(name)); // average of Q and RMS
          }
  }

  if( m_RecalFlag == flat_flag ){
    //------Flattening-------  
    cout << endl;
    cout << "--- RP_CALIB_FLAT::Registering Histogram ---" << endl;
    for(int icent = 0; icent < ncent; icent++)
      for(int izv = 0; izv < nzv; izv++)
        for(int idet12 = 0; idet12 < ndet2; idet12++){
          if( DetSkip(idet12) != EVENT_OK) continue;
          for(int ihar = 0; ihar < nhar; ihar++)
            for(int ico=0;ico<2;++ico){
              sprintf(name,"C%dZ%dD%dF_Q%d_%d",icent,izv,idet12,ihar+1,ico);
              HistoManager->registerHisto(new TProfile(name,name, 10, -0.5, 9.5, -10.0, 10.0));
              f_q[icent][izv][idet12][ihar][ico] = static_cast<TProfile*>(HistoManager->getHisto(name));
            }
        }
  }
  if( m_RecalFlag == nocalib_flag ){
  //------Resolution-----
    // VTX - BBC (v2 - full ev class) //
    for(int idet=0;idet<mdet+12;idet++){
      for(int ico=0;ico<2;ico++){
        sprintf(name,"bbcsnVSvtxID%d_%d",idet,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        vtx_bbc_reso[idet][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
      }
    }
    for(int ihar=0;ihar<nhar;ihar++){
      for(int ico=0;ico<2;ico++){
        // VTXS - BBCSN //
        sprintf(name,"CentVTXsBBCsnEgap1_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxs_egap1_bbcsn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"CentVTXsBBCsnEgap2_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxs_egap2_bbcsn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        // VTXN - BBCSN //
        sprintf(name,"CentVTXnBBCsnEgap1_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxn_egap1_bbcsn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"CentVTXnBBCsnEgap2_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxn_egap2_bbcsn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        // VTXSN - BBCS //
        sprintf(name,"CentVTXsnBBCsEgap1_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxsn_egap1_bbcs[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"CentVTXsnBBCsEgap2_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxsn_egap2_bbcs[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        // VTXSN - BBCN //
        sprintf(name,"CentVTXsnBBCnEgap1_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxsn_egap1_bbcn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"CentVTXsnBBCnEgap2_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxsn_egap2_bbcn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        // VTXSN - BBCSN //
        sprintf(name,"CentVTXsnBBCsnEgap1_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxsn_egap1_bbcsn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"CentVTXsnBBCsnEgap2_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_vtxsn_egap2_bbcsn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        // MPCS - MPCN //
        sprintf(name,"CentMPCsMPCn_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_mpcs_mpcn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
        // BBCS - BBCN //
        sprintf(name,"CentBBCsBBCn_v%d_%d",ihar+1,ico);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.0,2.0));   
        corr_cent_bbcs_bbcn[ihar][ico]= static_cast<TProfile*>(HistoManager->getHisto(name));
      }
    }
  }
  if( m_VnFlag == 1 ){
    // Vn //
    for(int ihar=0;ihar<nhar;ihar++){
      for(int ipt=0;ipt<npt;ipt++){
        sprintf(name,"pt%dBbcSNCos_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_bbcsn_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dBbcSNSin_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_bbcsn_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1Cos_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap1_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1Sin_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap1_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2Cos_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap2_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2Sin_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap2_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1eCos_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap1_e_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1eSin_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap1_e_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2eCos_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap2_e_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2eSin_v%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        vn_cent_vtxsn_egap2_e_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
      }
      for(int icent=0;icent<ncent;icent++){
        sprintf(name,"C%dBbcSNCos_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_bbcsn_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dBbcSNSin_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_bbcsn_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap1Cos_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap1_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap1Sin_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap1_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap2Cos_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap2_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap2Sin_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap2_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap1eCos_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap1_e_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap1eSin_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap1_e_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap2eCos_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap2_e_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"C%dVtxEgap2eSin_v%d",icent,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,4*npt,-0.5,npt-0.5,-5.0,5.0));
        vn_pt_vtxsn_egap2_e_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
      }
    }
    // Raw Vn //
    for(int ihar = 0; ihar < nhar; ihar++){
      for( int ipt = 0; ipt < npt; ipt++ ){
        sprintf(name,"pt%dBbcSNCos_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_bbcsn_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dBbcSNSin_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_bbcsn_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1Cos_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap1_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1Sin_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap1_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2Cos_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap2_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2Sin_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap2_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1eCos_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap1_e_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap1eSin_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap1_e_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2eCos_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap2_e_cos[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name,"pt%dVtxEgap2eSin_rawv%d",ipt,ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-5.0,5.0));
        rawvn_cent_vtxsn_egap2_e_sin[ipt][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
      }
      for(int icent = 0; icent < ncent; icent++){
	sprintf(name,"C%dBbcSNCos_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_bbcsn_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dBbcSNSin_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_bbcsn_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap1Cos_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap1_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap1Sin_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap1_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap2Cos_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap2_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap2Sin_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap2_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap1eCos_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap1_e_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap1eSin_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap1_e_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap2eCos_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap2_e_cos[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
	sprintf(name,"C%dVtxEgap2eSin_rawv%d",icent,ihar+1);
	HistoManager->registerHisto(new TProfile(name,name,npt*4,-0.5,npt-0.5,-5.0,5.0));
	rawvn_pt_vtxsn_egap2_e_sin[icent][ihar]= static_cast<TProfile*>(HistoManager->getHisto(name));
      }
    }
    // QA Re-centering //
    sprintf(name, "QMpcSN");
    HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
    scat_q_mpcsn=static_cast<TH2F*>(HistoManager->getHisto(name));
    sprintf(name, "QBbcSN");
    HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
    scat_q_bbcsn=static_cast<TH2F*>(HistoManager->getHisto(name));
    sprintf(name, "QVtxSN");
    HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
    scat_q_vtxsn=static_cast<TH2F*>(HistoManager->getHisto(name));
    for( int ihar = 0; ihar < nhar; ihar++ ){
      // QA Flattening //
      sprintf(name, "FPsiMpcSN_v%d", ihar+1);
      HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
      scat_psi_mpcsn[ihar]=static_cast<TH2F*>(HistoManager->getHisto(name));
      sprintf(name, "FPsiBbcSN_v%d", ihar+1);
      HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
      scat_psi_bbcsn[ihar]=static_cast<TH2F*>(HistoManager->getHisto(name));
      sprintf(name, "FPsiVtxSNEgap1_v%d", ihar+1);
      HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
      scat_psi_vtxsn_egap1[ihar]=static_cast<TH2F*>(HistoManager->getHisto(name));
      sprintf(name, "FPsiVtxSNEgap2_v%d", ihar+1);
      HistoManager->registerHisto(new TH2F(name,name,40,-2.,2.,40,-2.,2.));
      scat_psi_vtxsn_egap2[ihar]=static_cast<TH2F*>(HistoManager->getHisto(name));
    }
		// QA calib para //
    for( int ihar = 0; ihar < nhar; ihar++ ){
      sprintf(name, "BbcSNCosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,-2.,2.));
      bbcsn_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "BbcSNSinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,-2.,2.));
      bbcsn_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "VtxSNEgap1Cosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,-2.,2.));
      vtxsn_egap1_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "VtxSNEgap1Sinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,-2.,2.));
      vtxsn_egap1_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "VtxSNEgap2Cosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,-2.,2.));
      vtxsn_egap2_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "VtxSNEgap2Sinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,-2.,2.));
      vtxsn_egap2_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));

    }
    for( int ihar = 0; ihar < nhar; ihar++ ){
      sprintf(name, "CNTBbcSNCosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.,2.));
      cent_bbcsn_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTBbcSNSinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.,2.));
      cent_bbcsn_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTVtxSNEgap1Cosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.,2.));
      cent_vtxsn_egap1_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTVtxSNEgap1Sinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.,2.));
      cent_vtxsn_egap1_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTVtxSNEgap2Cosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.,2.));
      cent_vtxsn_egap2_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTVtxSNEgap2Sinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-2.,2.));
      cent_vtxsn_egap2_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXBbcSNCosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,-2.,2.));
      zvtx_bbcsn_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXBbcSNSinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,-2.,2.));
      zvtx_bbcsn_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXVtxSNEgap1Cosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,-2.,2.));
      zvtx_vtxsn_egap1_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXVtxSNEgap1Sinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,-2.,2.));
      zvtx_vtxsn_egap1_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXVtxSNEgap2Cosnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,-2.,2.));
      zvtx_vtxsn_egap2_cos[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXVtxSNEgap2Sinnpsi_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,-2.,2.));
      zvtx_vtxsn_egap2_sin[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
		}
    for( int ihar = 0; ihar < nhar; ihar++ ){
      for( int ilayer = 0; ilayer < 4; ilayer++ ){
        sprintf(name, "NclustEgap1_l%d_v%d", ilayer+1, ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,0.,3000.));
        nclust_egap1[ihar][ilayer]=static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name, "NclustEgap2_l%d_v%d", ilayer+1, ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,0.,3000.));
        nclust_egap2[ihar][ilayer]=static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name, "CNTNclustEgap1_l%d_v%d", ilayer+1, ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,nzv-0.5,0.,3000.));
        cent_nclust_egap1[ihar][ilayer]=static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name, "CNTNclustEgap2_l%d_v%d", ilayer+1, ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,nzv-0.5,0.,3000.));
        cent_nclust_egap2[ihar][ilayer]=static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name, "ZVTXNclustEgap1_l%d_v%d", ilayer+1, ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,0.,3000.));
        zvtx_nclust_egap1[ihar][ilayer]=static_cast<TProfile*>(HistoManager->getHisto(name));
        sprintf(name, "ZVTXNclustEgap2_l%d_v%d", ilayer+1, ihar+1);
        HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,0.,3000.));
        zvtx_nclust_egap2[ihar][ilayer]=static_cast<TProfile*>(HistoManager->getHisto(name));
      }
      sprintf(name, "NclustSumBbc_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,0.,10000.));
      nclustsum_bbc[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "NclustSumEgap1_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,0.,10000.));
      nclustsum_egap1[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "NclustSumEgap2_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,1,0.,1.,0.,10000.));
      nclustsum_egap2[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));

      sprintf(name, "CNTNclustSumBbc_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,0.,10000.));
      cent_nclustsum_bbc[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXNclustSumBbc_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,0.,10000.));
      zvtx_nclustsum_bbc[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTNclustSumEgap1_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,0.,10000.));
      cent_nclustsum_egap1[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXNclustSumEgap1_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,0.,10000.));
      zvtx_nclustsum_egap1[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTNclustSumEgap2_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,0.,10000.));
      cent_nclustsum_egap2[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "ZVTXNclustSumEgap2_v%d", ihar+1);
      HistoManager->registerHisto(new TProfile(name,name,nzv,-0.5,nzv-0.5,0.,10000.));
      zvtx_nclustsum_egap2[ihar]=static_cast<TProfile*>(HistoManager->getHisto(name));
    }
    // QA calib //
    for( int ico = 0; ico < 2; ico++ ){
      sprintf(name, "CNTQBbc_%d", ico);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-10.,10.));
      cent_q_bbc[ico]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTQVtx_%d", ico);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-10.,10.));
      cent_q_vtx[ico]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "EVTSQQBbc_%d", ico);
      HistoManager->registerHisto(new TProfile(name,name,500,0.,5000000.,-10.,10.));
      evtsq_q_bbc[ico]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "EVTSQQVtx_%d", ico);
      HistoManager->registerHisto(new TProfile(name,name,500,0.,5000000,-10.,10.));
      evtsq_q_vtx[ico]=static_cast<TProfile*>(HistoManager->getHisto(name));
    }
    for( int inf = 0; inf < nfn; inf++ ){
      sprintf(name, "CNTFpsiv2Bbc_%d", inf);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-10.,10.));
      cent_fpsiv2_bbc[inf]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTFpsiv2Egap1_%d", inf);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-10.,10.));
      cent_fpsiv2_egap1[inf]=static_cast<TProfile*>(HistoManager->getHisto(name));
      sprintf(name, "CNTFpsiv2Egap2_%d", inf);
      HistoManager->registerHisto(new TProfile(name,name,ncent,-0.5,ncent-0.5,-10.,10.));
      cent_fpsiv2_egap2[inf]=static_cast<TProfile*>(HistoManager->getHisto(name));
    }

    // Hit Map //
    int maxladder;
    float colmin, colmax;
    float rowmin, rowmax;
    colmin =  -64.5*0.0425; colmax =  63.5*0.0425;
    rowmin = -128.5*0.0050; rowmax = 127.5*0.0050;
    for( int layer = 0; layer < 2; layer++ ){
      if( layer == 0 ) maxladder = 10;
      else if( layer == 1 ) maxladder = 20;
      for( int ladder = 0; ladder < maxladder; ladder++ ){
        for( int sensor = 0; sensor < 4; sensor++ ){
          sprintf(name, "pixel_ly%d_ld%d_s%d", layer, ladder, sensor);
          HistoManager->registerHisto(new TH2F(name, name, 128, colmin, colmax, 256, rowmin, rowmax));
          pixelhmp[layer][ladder][sensor]=static_cast<TH2F*>(HistoManager->getHisto(name));
        }
      }
    }
//     for( int layer = 2; layer < 4; layer++ ){
//       if( layer == 2 ){ maxladder = 16; maxsensor = 5; }
//       else if( layer == 3 ){ maxladder = 24; maxsensor = 6; }
//       for( int ladder = 0; ladder < maxladder; ladder++ ){
//         for( int sensor = 0; sensor < maxsensor; sensor++ ){
//           sprintf(name, "Strip_ly%d_ld%d_s%d", layer, ladder, sensor);
//           HistoManager->registerHisto(new TH2F(name, name, 128, -0.5, 127.5, 256, -0.5, 255.5));
//           striphp_[layer][ladder][sensor]=static_cast<TH2F*>(HistoManager->getHisto(name));
//         }
//       }
//     }

//     // Number of Clusters //
//     for( int layer = 0; layer < 4; layer++ ){
//       if( layer == 0 ){ m_maxladder = 10; m_maxsensor = 4; }
//       else if( layer == 1 ){ m_maxladder = 20; m_maxsensor = 4; }
//       else if( layer == 2 ){ m_maxladder = 16; m_maxsensor = 5; }
//       else if( layer == 3 ){ m_maxladder = 24; m_maxsensor = 6; }
//       for( int ladder = 0; ladder < m_maxladder; ladder++ ){
// 	for( int sensor = 0; sensor < m_maxsensor; sensor++ ){
// 	  sprintf(name, "clst_ly%d_ld%d_s%d", layer, ladder, sensor);
// 	  HistoManager->registerHisto(new TH2F(name, name, 220, ))
// 	}
//       }
//     }
  }

  return;
}
//==============================================================
void RPCalib_priv::initEvtTree()
{
  ntp = new TTree("ntp", "Event Info Tree");
  ntp->Branch("evtsq",  &m_EvtSequence, "evtsq/I");
  ntp->Branch("event",  &m_EventNumber, "event/I");
//   ntp->Branch("ptick",   m_pticks,      "ptick[3]/I");
  ntp->Branch("bbcq",   &m_bbcq,        "bbcq/I");
  ntp->Branch("bbcz",   &m_bbcz,        "bbcz/F");
  ntp->Branch("ncls",    m_nclst,       "ncls[4]/I");
  ntp->Branch("nclsbbc",&m_nclstsum_bbc, "nclsbbc/I");
  ntp->Branch("icent",  &m_icent,        "icent/I");
  ntp->Branch("cent",   &m_cent,        "cent/I");
  ntp->Branch("zvtx",   &m_zvtx,        "zvtx/F");
  ntp->Branch("Q",       m_Q,           "Q[52][3]/F");   // 2 : x and y
  ntp->Branch("ReQ",     m_Re_Q,        "ReQ[39][2]/F"); // 2 : x and y
  ntp->Branch("repsiv2", m_Re_Psi2,     "repsiv2[39]/F");  // Reaction Plane for v2 after Re-centering
  ntp->Branch("fpsiv2",  m_F_Psi2,      "fpsiv2[39][8]/F"); // Reaction Plane for v2 after Flattening
  ntp->Branch("fpsivn",  m_F_Psi7,      "fpsivn[39][6]/F"); // Reaction Plane for vn after Flattening
  ntp->Branch("cnpbbc",  m_Cosnpsi_BBC, "cnpbbc[6]/F");
  ntp->Branch("cnpegap1", m_Cosnpsi_Egap1, "cnpegap1[6]/F");
  ntp->Branch("cnpegap2", m_Cosnpsi_Egap2, "cnpegap2[6]/F");
  //ntp->Branch("clsn",     m_clsn,       "clsn[4][24][6]");

  return;
}
//==============================================================
void RPCalib_priv::initClstTree()
{
  cls = new TTree("cls", "Cluster Info Tree");
  cls->Branch("evtsq",  &m_EvtSequence, "evtsq/I");
//   cls->Branch("event",  &m_EventNumber, "event/I");
//   cls->Branch("ncls",   &m_nclstsum,    "ncls/I");
//   cls->Branch("layer",  &m_layer,       "layer/I");
//   cls->Branch("ladder", &m_ladder,      "ladder/I");
//   cls->Branch("sensor", &m_sensor,      "sensor/I");
//   cls->Branch("clsxyz", m_clsxyz,       "clsxyz[3]/F");
//    cls->Branch("clsn",   m_clsns,         "clsn[2][20][4]");
   cls->Branch("clsnc",   m_clsnc,         "clsnc[2][20][4][4]");

  return;
}
//==============================================================
void RPCalib_priv::initRpCalibData( char* calib )
{
  ifstream ifs_1, ifs_2;
  float re_c1, re_c2;
  char reso[100];

  cout << "Open Calibration file " << calib << endl;
  for(int idet2 = 0; idet2 < ndet2; idet2++) // without additional event plane
    for(int ihar = 0; ihar < nhar; ihar++)
      for(int icent = 0; icent< ncent; icent++)
        for(int izv = 0; izv < nzv; izv++)
          for(int ico = 0; ico < 2; ico++){
            ave[icent][izv][idet2][ihar][ico]=-999.0; // average of normalized Q vector w.r.t. number of events
            RMS[icent][izv][idet2][ihar][ico]=-999.0; // RMS of normalized Q vector w.r.t. number of events
          }
  for(int ihar=0;ihar<nhar;ihar++)
    for(int icent=0;icent<ncent;icent++)
      for(int ico=0;ico<2;ico++){
        m_reso_bbcsn[icent][ihar][ico] = -999.0;
        m_reso_vtxsn_egap1[icent][ihar][ico] = -999.0;
        m_reso_vtxsn_egap2[icent][ihar][ico] = -999.0;
      }
  for(int idet2=0;idet2<ndet2;idet2++) // include additional event plane
    for(int ihar=0;ihar<nhar;ihar++)
      for(int icent=0;icent<ncent;icent++)
        for(int izv=0;izv<nzv;izv++)
          for(int ico=0;ico<2;ico++)
            for(int ifn=0;ifn<nfn;ifn++)
              fla[icent][izv][idet2][ihar][ico][ifn] = -999.0;

  if( m_RecalFlag != recent_flag ){
    ifs_1.open(calib);
    if(!ifs_1) cout << "No RPCalib file." << calib << endl;
    for(int idet2 = 0; idet2 < ndet2; idet2++){ // without additional event plane
      if( DetSkip(idet2) != EVENT_OK) continue;
      for(int ihar = 0; ihar < nhar; ihar++)
        for(int icent = 0; icent < ncent; icent++)
          for(int izv = 0; izv < nzv; izv++)
            for(int ico = 0; ico < 2; ico++){
              ifs_1 >> re_c1 >> re_c2;
              ave[icent][izv][idet2][ihar][ico] = re_c1; // average of normalized Q vector w.r.t. number of events
              RMS[icent][izv][idet2][ihar][ico] = re_c2; // RMS of normalized Q vector w.r.t. number of events
              if(idet2==ndet2-1 && ihar==nhar-1 && icent==ncent-1 && izv==nzv-1 && ico==1){
                cout <<  "ave, RMS" << re_c1 << " " << re_c2 << endl;
              }
            }
    }
  }

  if( m_RecalFlag == flat_flag  ) ifs_1.close();
  else if( m_RecalFlag == nocalib_flag ){
    for(int idet2=0;idet2<ndet2;idet2++){ // include additional event plane
      if( DetSkip(idet2) != EVENT_OK) continue;
      for(int ihar=0;ihar<nhar;ihar++)
        for(int icent=0;icent<ncent;icent++)
          for(int izv=0;izv<nzv;izv++)
            for(int ifn=0;ifn<nfn;ifn++){
              ifs_1 >> fla[icent][izv][idet2][ihar][0][ifn] >>  fla[icent][izv][idet2][ihar][1][ifn] ;

              if(idet2==52 && icent==0 && izv==0 && ihar==0){
                cout <<" cos"<<fla[icent][izv][idet2][ihar][0][ifn] <<" sin" << fla[icent][izv][idet2][ihar][1][ifn] << endl;
              }
            }
    }
    ifs_1.close();
  }
  if( m_VnFlag == 1 ){
    sprintf(reso, "Reso_VTX_SN_%d-%d.dat", m_runnumber, m_agsegnum);
    ifs_2.open(reso);
    for( int ihar = 0; ihar < nhar; ihar++ ){
      for( int icent = 0; icent < ncent; icent++ ){
        for( int ico = 0; ico < 2; ico++ ){
          ifs_2 >> m_reso_mpcsn[icent][ihar][ico] >> m_reso_bbcsn[icent][ihar][ico] 
                >> m_reso_vtxs_egap1[icent][ihar][ico] >> m_reso_vtxs_egap2[icent][ihar][ico] 
                >> m_reso_vtxn_egap1[icent][ihar][ico] >> m_reso_vtxn_egap2[icent][ihar][ico] 
                >> m_reso_vtxsn_egap1[icent][ihar][ico] >> m_reso_vtxsn_egap2[icent][ihar][ico];
        }
      }
    }
    ifs_2.close();
  }

  return;
}
//==============================================================
void RPCalib_priv::initCentCalibData(char* calib)
{
	ifstream ifs;
	ifs.open(calib);

	for( int icent = 0; icent < ncent-1; icent++ )
		ifs >> m_bbccut[icent];
	ifs.close();

	return;
}
//==============================================================
void RPCalib_priv::EndCalibData()
{
  ofstream ofs_1,ofs_2,ofs_5,ofs_3,ofs_4,ofs_6,ofs_7;
  char calib[100], calibtmp[100], reso[100];
  if ( m_RecalFlag != nocalib_flag ){
    sprintf(calibtmp, "Calib_para_tmp_%d-%d.dat", m_runnumber, m_agsegnum);
    sprintf(calib, "Calib_para_%d-%d.dat", m_runnumber, m_agsegnum);
    ofs_1.open(calib);
    cout << "RPCalib_priv::End:  Writing out... " << calib << "..." << endl;
  }

  //////////////////
  // Re-centering //
  if ( m_RecalFlag != nocalib_flag ){
    cout << "Writing Re-centering Parameters" << endl;
    for(int idet2 = 0; idet2 < ndet2; ++idet2){   // ndet2 = 88 
      if( DetSkip(idet2) != EVENT_OK) continue;
      for(int ihar = 0; ihar < nhar; ihar++){     // nhar = 6
        for(int icent = 0; icent < ncent; icent++){ // ncent = 10
          for(int izv = 0; izv < nzv; izv++){ // nzv = 5
            for(int ico = 0; ico < 2; ++ico){
              if ( m_RecalFlag == recent_flag ){
                ofs_1 <<  q[icent][izv][idet2][ihar] ->GetBinContent(ico+1) << " ";
                ofs_1 <<  q[icent][izv][idet2][ihar] ->GetBinError(ico+1) << " ";
              }
              if ( m_RecalFlag == flat_flag ){
                ofs_1 <<  ave[icent][izv][idet2][ihar][ico] << " ";
                ofs_1 <<  RMS[icent][izv][idet2][ihar][ico] << " ";
              }
            }
          }
          if ( m_RecalFlag == recent_flag || m_RecalFlag == flat_flag ){
            ofs_1 << endl;
          }
        }
      }
    }
    if ( m_RecalFlag == recent_flag ){
      ofs_1.close();
    }
    if ( m_RecalFlag == flat_flag ){
      ////////////////
      // Flattening //
      cout << "Writing Flattening Parameters" << endl;
      for(int idet2=0;idet2<ndet2;idet2++){ // ndet2 = 88
        if( DetSkip(idet2) != EVENT_OK) continue;
        for(int ihar=0;ihar<nhar;ihar++){   // nhar = 6
          for(int icent=0;icent<ncent;icent++){ // ncent = 10
            for(int izv=0;izv<nzv;izv++){ // nzv = 5
              for(int inf=0;inf<nfn;inf++){ // nfn = 8
                ofs_1 << f_q[icent][izv][idet2][ihar][0]->GetBinContent(inf+1) << " "
                      << f_q[icent][izv][idet2][ihar][1]->GetBinContent(inf+1) << " " ;
              }
              ofs_1 << endl;
            }
          }
        }
      }
      ofs_1.close();
    }
  } // if ( m_RecalFlag != nocalib_flag ){
  else if(m_VnFlag == 0){
    ////////////////
    // Resolution //
    cout << endl;
    cout << "Writing Resolution..." << endl;
    sprintf(reso, "Reso_VTX_SN_%d-%d.dat", m_runnumber, m_agsegnum);
    ofs_3.open(reso);
    float corr_vtxs_bbcsn_egap1[2];
    float corr_vtxs_bbcsn_egap2[2];
    float corr_vtxn_bbcsn_egap1[2];
    float corr_vtxn_bbcsn_egap2[2];
    float corr_vtxsn_bbcsn_egap1[2];
    float corr_vtxsn_bbcsn_egap2[2];
    float corr_mpcsn[2];
    float corr_bbcsn[2];

    float reso_mpcsn[2];
    float reso_bbcsn[2];
    float reso_vtxs_egap1[2], reso_vtxs_egap2[2];
    float reso_vtxn_egap1[2], reso_vtxn_egap2[2];
    float reso_vtxsn_egap1[2], reso_vtxsn_egap2[2];
    for(int ihar = 0; ihar < nhar; ihar++){   // nhar = 6
      if( ihar != 0 ) ofs_3 << endl;
      for(int icent = 0; icent < ncent; icent++){ // ncent = 10
        for(int ico = 0; ico < 2; ico++){
          corr_vtxs_bbcsn_egap1[ico] = abs(corr_cent_vtxs_egap1_bbcsn[ihar][ico]->GetBinContent(icent+1));
          corr_vtxs_bbcsn_egap2[ico] = abs(corr_cent_vtxs_egap2_bbcsn[ihar][ico]->GetBinContent(icent+1));
          corr_vtxn_bbcsn_egap1[ico] = abs(corr_cent_vtxn_egap1_bbcsn[ihar][ico]->GetBinContent(icent+1));
          corr_vtxn_bbcsn_egap2[ico] = abs(corr_cent_vtxn_egap2_bbcsn[ihar][ico]->GetBinContent(icent+1));
          corr_vtxsn_bbcsn_egap1[ico] = abs(corr_cent_vtxsn_egap1_bbcsn[ihar][ico]->GetBinContent(icent+1));
          corr_vtxsn_bbcsn_egap2[ico] = abs(corr_cent_vtxsn_egap2_bbcsn[ihar][ico]->GetBinContent(icent+1));
          corr_mpcsn[ico] = abs(corr_cent_mpcs_mpcn[ihar][ico]->GetBinContent(icent+1));
          corr_bbcsn[ico] = abs(corr_cent_bbcs_bbcn[ihar][ico]->GetBinContent(icent+1));

          reso_mpcsn[ico] = sqrt(2*corr_mpcsn[ico]);
          reso_bbcsn[ico] = sqrt(2*corr_bbcsn[ico]);
          reso_vtxs_egap1[ico] = corr_vtxs_bbcsn_egap1[ico]/sqrt(2*corr_bbcsn[ico]);
          reso_vtxs_egap2[ico] = corr_vtxs_bbcsn_egap2[ico]/sqrt(2*corr_bbcsn[ico]);
          reso_vtxn_egap1[ico] = corr_vtxn_bbcsn_egap1[ico]/sqrt(2*corr_bbcsn[ico]);
          reso_vtxn_egap2[ico] = corr_vtxn_bbcsn_egap2[ico]/sqrt(2*corr_bbcsn[ico]);
          reso_vtxsn_egap1[ico] = corr_vtxsn_bbcsn_egap1[ico]/sqrt(2*corr_bbcsn[ico]);
          reso_vtxsn_egap2[ico] = corr_vtxsn_bbcsn_egap2[ico]/sqrt(2*corr_bbcsn[ico]);
          //cout << icent << " " << "#########" << reso_vtxsn_egap1[ico] <<  " " << reso_vtxsn_egap2[ico] << " " << sqrt(2*abs(corr_bbcsn[ico])) <<  endl;
          if(icent == 0 && ico == 0)
            ofs_3 << reso_mpcsn[ico] << " " << reso_bbcsn[ico] << " "
                  << reso_vtxs_egap1[ico] << " " << reso_vtxs_egap2[ico] << " "
                  << reso_vtxn_egap1[ico] << " " << reso_vtxn_egap2[ico] << " "
                  << reso_vtxsn_egap1[ico] << " " << reso_vtxsn_egap2[ico];
          else
            ofs_3 << " " << reso_mpcsn[ico] << " " << reso_bbcsn[ico] << " "
                  << reso_vtxs_egap1[ico] << " " << reso_vtxs_egap2[ico] << " "
                  << reso_vtxn_egap1[ico] << " " << reso_vtxn_egap2[ico] << " "
                  << reso_vtxsn_egap1[ico] << " " << reso_vtxsn_egap2[ico];
        }
      }
    }
    cout << "Closing " << reso << endl;
    ofs_3.close();
	}
	if( m_VnFlag == 1 ){
  //if( m_RecalFlag == recent_flag ){
  //if ( m_RecalFlag == flat_flag ){
		cout << "RPCalib_priv::End:  Writing out..." << endl;
		OutputNtupleFile->Write();
		cout << "RPCalib_priv::End:  Closing output file..." << endl;
		OutputNtupleFile->Close();
		delete OutputNtupleFile;
		OutputNtupleFile = 0;
  }

  return;
}
//==============================================================
int RPCalib_priv::TickCut()
{
  if( ( 50<m_pticks[0]&&m_pticks[0]<120)||
      (700<m_pticks[1]&&m_pticks[1]<780) )
    return ABORTEVENT;
  else
    return EVENT_OK;
}
//==============================================================
int RPCalib_priv::DetSkip( int idet )
{
  if( idet < 36 || (47 < idet && idet < 52) || (52 < idet && idet < 62) )
    return ABORTEVENT;
  else return EVENT_OK;
}
//==============================================================
int RPCalib_priv::EventSkip()
{
	if( m_EventNumber <= 36000*(m_EvSkipFlag-1) ) return -1;  // Skip
	else if ( m_EventNumber > 36000*m_EvSkipFlag ) return -2; // EndRun
	else return EVENT_OK;
}
//==============================================================
int RPCalib_priv::CalcVnReso(PHCompositeNode *topNode, int icent)
{
  PHCentralTrack *central = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if(!central){
      cerr << PHWHERE << " No PHCentralTrack object !" << endl;
      return ABORTEVENT;
	}

  float corr[2];
  float dpsi;
  // VTXS - BBCSN //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[19][ihar][7] > -900 && m_F_Psi[30][ihar][7] > -900 ){
      dpsi = m_F_Psi[19][ihar][7] - m_F_Psi[30][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        //if(icent == 11 ) cout << icent << " " << corr[ico] << endl;
        corr_cent_vtxs_egap1_bbcsn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
    // eta gap 2 //
    if( m_F_Psi[22][ihar][7] > -900 && m_F_Psi[30][ihar][7] > -900 ){
      dpsi = m_F_Psi[22][ihar][7] - m_F_Psi[30][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        corr_cent_vtxs_egap2_bbcsn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
  }
  // VTXN - BBCSN //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[20][ihar][7] > -900 && m_F_Psi[30][ihar][7] > -900 ){
      dpsi = m_F_Psi[20][ihar][7] - m_F_Psi[30][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        //if(icent == 11 ) cout << icent << " " << corr[ico] << endl;
        corr_cent_vtxn_egap1_bbcsn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
    // eta gap 2 //
    if( m_F_Psi[23][ihar][7] > -900 && m_F_Psi[30][ihar][7] > -900 ){
      dpsi = m_F_Psi[23][ihar][7] - m_F_Psi[30][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        corr_cent_vtxn_egap2_bbcsn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
  }
  // VTXSN - BBCSN //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[21][ihar][7] > -900 && m_F_Psi[30][ihar][7] > -900 ){
      dpsi = m_F_Psi[21][ihar][7] - m_F_Psi[30][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        //if(icent == 11 ) cout << icent << " " << corr[ico] << endl;
        corr_cent_vtxsn_egap1_bbcsn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
    // eta gap 2 //
    if( m_F_Psi[24][ihar][7] > -900 && m_F_Psi[30][ihar][7] > -900 ){
      dpsi = m_F_Psi[24][ihar][7] - m_F_Psi[30][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        corr_cent_vtxsn_egap2_bbcsn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
  }
  // VTXSN - BBCS //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[21][ihar][7] > -900 && m_F_Psi[28][ihar][7] > -900 ){
      dpsi = m_F_Psi[21][ihar][7] - m_F_Psi[28][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        //if(icent == 11 ) cout << icent << " " << corr[ico] << endl;
        corr_cent_vtxsn_egap1_bbcs[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
    // eta gap 2 //
    if( m_F_Psi[24][ihar][7] > -900 && m_F_Psi[28][ihar][7] > -900 ){
      dpsi = m_F_Psi[24][ihar][7] - m_F_Psi[28][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        corr_cent_vtxsn_egap2_bbcs[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
  }
  // VTXSN - BBCN //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[21][ihar][7] > -900 && m_F_Psi[29][ihar][7] > -900 ){
      dpsi = m_F_Psi[21][ihar][7] - m_F_Psi[29][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        //if(icent == 11 ) cout << icent << " " << corr[ico] << endl;
        corr_cent_vtxsn_egap1_bbcn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
    // eta gap 2 //
    if( m_F_Psi[24][ihar][7] > -900 && m_F_Psi[29][ihar][7] > -900 ){
      dpsi = m_F_Psi[24][ihar][7] - m_F_Psi[29][ihar][7];
      corr[0] = cos((ihar+1)*dpsi);
      corr[1] = sin((ihar+1)*dpsi);
      for( int ico = 0; ico < 2; ico++ ){
        corr_cent_vtxsn_egap2_bbcn[ihar][ico]->Fill(icent, corr[ico]);
      }
    }
  }
  // MPCS - MPCN //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[25][ihar][7] < -900 ||
        m_F_Psi[26][ihar][7] < -900 ) continue;
    dpsi = m_F_Psi[25][ihar][7] - m_F_Psi[26][ihar][7];
    corr[0] = cos((ihar+1)*dpsi);
    corr[1] = sin((ihar+1)*dpsi);
    for( int ico = 0; ico < 2; ico++ ){
      corr_cent_mpcs_mpcn[ihar][ico]->Fill(icent, corr[ico]);
    }
  }
  // BBCS - BBCN //
  for( int ihar = 0; ihar < nhar; ihar++ ){
    // eta gap 1 //
    if( m_F_Psi[28][ihar][7] < -900 ||
        m_F_Psi[29][ihar][7] < -900 ) continue;
    dpsi = m_F_Psi[28][ihar][7] - m_F_Psi[29][ihar][7];
    corr[0] = cos((ihar+1)*dpsi);
    corr[1] = sin((ihar+1)*dpsi);
    for( int ico = 0; ico < 2; ico++ ){
      corr_cent_bbcs_bbcn[ihar][ico]->Fill(icent, corr[ico]);
    }
  }
  
  return EVENT_OK;
}
//==============================================================
bool RPCalib_priv::goodDCAAbs(SvxCentralTrackMapEntry* svxcnt, float dcacut)
{
	if(svxcnt==NULL) return false;

	int nhit = svxcnt->get_NClusters();
	int ndf = (nhit-1)*2;

	bool result = (
								 nhit>2 &&
								 (svxcnt->get_HitPattern()&0x3)==0x3 &&
								 (svxcnt->get_Chisquare()*1e-2)/ndf < 5 &&
								 fabs(svxcnt->get_DCA2D()*1e-4)<dcacut &&
								 fabs(svxcnt->get_DCAZ()*1e-4)<1);

	return result;
}
//==============================================================
bool RPCalib_priv::isElectron(PHSnglCentralTrack* cnt)
{
	bool isElectron =
		( cnt->get_n0()>2 &&
			(cnt->get_chi2()/cnt->get_npe0())<10 &&
			cnt->get_disp()<5 &&
			(cnt->get_ecore()/cnt->get_mom())>0.7);

	return isElectron;
}
//==============================================================
int RPCalib_priv::GetVn(PHCompositeNode *topNode, int icent)
{
	//cout << m_EventNumber << endl;
  PHCentralTrack *central = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if(!central){
      cerr << PHWHERE << " No PHCentralTrack object !" << endl;
      return ABORTEVENT;
	}
	int ntrk = central->get_npart();

// 	SvxCentralTrackMap* svxcntmap = findNode::getClass<SvxCentralTrackMap>(topNode, "SvxCentralTrack_comp");
// 	if(svxcntmap==NULL){
// 		cout << "Error SvxVnAna::Process_event no SvxCentralTrackMap" << endl;
// 		return -1;
// 	}
// 	int nsvxcnt = svxcntmap->GetNentries();

// 	SvxCentralTrackMapEntry** svxCntAry = new SvxCentralTrackMapEntry*[ntrk];
// 	for(int itrk=0; itrk<ntrk; itrk++){ svxCntAry[itrk] = NULL; }

// 	for( int itrk=0; itrk<nsvxcnt; itrk++ ){
// 		const SvxCentralTrackMapEntry* svxmapent = svxcntmap->GetHit(itrk);
// 		if(svxmapent==NULL){
// 			continue;
// 		}
// 		int cntidx = svxmapent->get_DchIndex();
// 		if(cntidx<0||ntrk<=cntidx){ cerdr<<" DchIndex is out of range"<<endl; }
// 		svxCntAry[cntidx] = (SvxCentralTrackMapEntry*)svxmapent;
// 	}

  // vn in this code //
  for(int itrk = 0; itrk < ntrk; itrk++){
		PHSnglCentralTrack* cnt = central->get_track(itrk);

    short quality = cnt->get_quality();
    float mom = cnt->get_mom();
    float the0 = cnt->get_the0();
    float zed = cnt->get_zed();
    float phi0 = cnt->get_phi0();
    // Good track selection
    if ((quality == 31 || quality == 63)
        //&& sqrt(pc3sdphi*pc3sdphi+pc3sdz*pc3sdz)<3.0
        && the0 > -100
        && fabs(zed) < 100
        && phi0 > -100
        && mom != 0.0 && mom < 20.0){
      float phi = atan2(sin(phi0), cos(phi0));
      float pt = mom * sin(the0);
      float vn_bbcsn_cos[nhar], vn_bbcsn_sin[nhar];
      float vn_vtxsn_egap1_cos[nhar], vn_vtxsn_egap1_sin[nhar];
      float vn_vtxsn_egap2_cos[nhar], vn_vtxsn_egap2_sin[nhar];
      float rawvn_bbcsn_cos[nhar], rawvn_bbcsn_sin[nhar];
      float rawvn_vtxsn_egap1_cos[nhar], rawvn_vtxsn_egap1_sin[nhar];
      float rawvn_vtxsn_egap2_cos[nhar], rawvn_vtxsn_egap2_sin[nhar];
      int ipt = (int)(pt)%npt;
      // BBC Vn //
      for( int ihar = 0; ihar < nhar; ihar++ ){
        if( m_F_Psi[30][ihar][7] > -900. && pt < 5.0){
//           if( m_RecalFlag == nocalib_flag &&
//               m_reso_bbcsn[icent][ihar][0] != 0 && m_reso_bbcsn[icent][ihar][1] != 0){
          if( m_RecalFlag == nocalib_flag ){
            // Raw Vn //
            rawvn_bbcsn_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[30][ihar][7]));
            rawvn_bbcsn_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[30][ihar][7]));
//             if( ihar == 1 )
//               cout << rawvn_bbcsn_cos[ihar] << " " <<  m_reso_bbcsn[icent][ihar][0] << " --> " << rawvn_bbcsn_cos[ihar]/m_reso_bbcsn[icent][ihar][0] << endl;
            rawvn_pt_bbcsn_cos[icent][ihar]->Fill(pt,rawvn_bbcsn_cos[ihar]);
            rawvn_pt_bbcsn_sin[icent][ihar]->Fill(pt,rawvn_bbcsn_sin[ihar]);
            rawvn_cent_bbcsn_cos[ipt][ihar]->Fill(icent,rawvn_bbcsn_cos[ihar]);
            rawvn_cent_bbcsn_sin[ipt][ihar]->Fill(icent,rawvn_bbcsn_sin[ihar]);
            // Corrected Vn //
            vn_bbcsn_cos[ihar] = rawvn_bbcsn_cos[ihar]/m_reso_bbcsn[icent][ihar][0];
            vn_bbcsn_sin[ihar] = rawvn_bbcsn_sin[ihar]/m_reso_bbcsn[icent][ihar][1];
            vn_pt_bbcsn_cos[icent][ihar]->Fill(pt,vn_bbcsn_cos[ihar]);
            vn_pt_bbcsn_sin[icent][ihar]->Fill(pt,vn_bbcsn_sin[ihar]);
            vn_cent_bbcsn_cos[ipt][ihar]->Fill(icent,vn_bbcsn_cos[ihar]);
            vn_cent_bbcsn_sin[ipt][ihar]->Fill(icent,vn_bbcsn_sin[ihar]);
          }
        }
      }
      // BBC Raw Vn //
      for( int ihar = 0; ihar < nhar; ihar++ ){
        if( m_F_Psi[30][ihar][7] > -900. && pt < 5.0){
          if( m_RecalFlag == nocalib_flag ){
          }
        }
      }
      // VTX Vn for Egap1 and Egap2 //
      for( int ihar = 0; ihar < nhar; ihar++ ){
        if( m_F_Psi[21][ihar][7] > -900. && m_F_Psi[24][ihar][7] > -900.  && pt < 5.0){
//           if( m_RecalFlag == nocalib_flag &&
//               m_reso_vtxsn_egap1[icent][ihar][0] != 0 && m_reso_vtxsn_egap2[icent][ihar][0] != 0 &&
//               m_reso_vtxsn_egap1[icent][ihar][1] != 0 && m_reso_vtxsn_egap2[icent][ihar][1] != 0){
          if( m_RecalFlag == nocalib_flag ){
            // Raw Vn //
            rawvn_vtxsn_egap1_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[21][ihar][7]));
            rawvn_vtxsn_egap2_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[24][ihar][7]));
            rawvn_vtxsn_egap1_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[21][ihar][7]));
            rawvn_vtxsn_egap2_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[24][ihar][7]));
            rawvn_pt_vtxsn_egap1_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_cos[ihar]);
            rawvn_pt_vtxsn_egap2_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_cos[ihar]);
            rawvn_pt_vtxsn_egap1_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_sin[ihar]);
            rawvn_pt_vtxsn_egap2_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_sin[ihar]);
            rawvn_cent_vtxsn_egap1_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_cos[ihar]);
            rawvn_cent_vtxsn_egap2_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_cos[ihar]);
            rawvn_cent_vtxsn_egap1_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_sin[ihar]);
            rawvn_cent_vtxsn_egap2_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_sin[ihar]);
						if( isElectron(cnt) ){
							rawvn_pt_vtxsn_egap1_e_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_cos[ihar]);
							rawvn_pt_vtxsn_egap2_e_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_cos[ihar]);
							rawvn_pt_vtxsn_egap1_e_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_sin[ihar]);
							rawvn_pt_vtxsn_egap2_e_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_sin[ihar]);
							rawvn_cent_vtxsn_egap1_e_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_cos[ihar]);
							rawvn_cent_vtxsn_egap2_e_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_cos[ihar]);
							rawvn_cent_vtxsn_egap1_e_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_sin[ihar]);
							rawvn_cent_vtxsn_egap2_e_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_sin[ihar]);
						}
            // Corrected Vn //
            vn_vtxsn_egap1_cos[ihar] = rawvn_vtxsn_egap1_cos[ihar]/m_reso_vtxsn_egap1[icent][ihar][0];
            vn_vtxsn_egap1_sin[ihar] = rawvn_vtxsn_egap1_sin[ihar]/m_reso_vtxsn_egap1[icent][ihar][1];
            vn_vtxsn_egap2_cos[ihar] = rawvn_vtxsn_egap2_cos[ihar]/m_reso_vtxsn_egap2[icent][ihar][0];
            vn_vtxsn_egap2_sin[ihar] = rawvn_vtxsn_egap2_sin[ihar]/m_reso_vtxsn_egap2[icent][ihar][1];
            vn_pt_vtxsn_egap1_cos[icent][ihar]->Fill(pt,vn_vtxsn_egap1_cos[ihar]);
            vn_pt_vtxsn_egap1_sin[icent][ihar]->Fill(pt,vn_vtxsn_egap1_sin[ihar]);
            vn_pt_vtxsn_egap2_cos[icent][ihar]->Fill(pt,vn_vtxsn_egap2_cos[ihar]);
            vn_pt_vtxsn_egap2_sin[icent][ihar]->Fill(pt,vn_vtxsn_egap2_sin[ihar]);
            vn_cent_vtxsn_egap1_cos[ipt][ihar]->Fill(icent,vn_vtxsn_egap1_cos[ihar]);
            vn_cent_vtxsn_egap1_sin[ipt][ihar]->Fill(icent,vn_vtxsn_egap1_sin[ihar]);
            vn_cent_vtxsn_egap2_cos[ipt][ihar]->Fill(icent,vn_vtxsn_egap2_cos[ihar]);
            vn_cent_vtxsn_egap2_sin[ipt][ihar]->Fill(icent,vn_vtxsn_egap2_sin[ihar]);
						if( isElectron(cnt) ){
							vn_pt_vtxsn_egap1_e_cos[icent][ihar]->Fill(pt,vn_vtxsn_egap1_cos[ihar]);
							vn_pt_vtxsn_egap1_e_sin[icent][ihar]->Fill(pt,vn_vtxsn_egap1_sin[ihar]);
							vn_pt_vtxsn_egap2_e_cos[icent][ihar]->Fill(pt,vn_vtxsn_egap2_cos[ihar]);
							vn_pt_vtxsn_egap2_e_sin[icent][ihar]->Fill(pt,vn_vtxsn_egap2_sin[ihar]);
							vn_cent_vtxsn_egap1_e_cos[ipt][ihar]->Fill(icent,vn_vtxsn_egap1_cos[ihar]);
							vn_cent_vtxsn_egap1_e_sin[ipt][ihar]->Fill(icent,vn_vtxsn_egap1_sin[ihar]);
							vn_cent_vtxsn_egap2_e_cos[ipt][ihar]->Fill(icent,vn_vtxsn_egap2_cos[ihar]);
							vn_cent_vtxsn_egap2_e_sin[ipt][ihar]->Fill(icent,vn_vtxsn_egap2_sin[ihar]);
						}
          }

        }
      }
      // VTX Raw Vn for Egap1 and Egap2 //
      for( int ihar = 0; ihar < nhar; ihar++ ){
        if( m_F_Psi[21][ihar][7] > -900. && m_F_Psi[24][ihar][7] > -900. && pt <= 5.0){
          if( m_RecalFlag == nocalib_flag ){
//             rawvn_vtxsn_egap1_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[21][ihar][7]));
//             rawvn_vtxsn_egap2_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[24][ihar][7]));
//             rawvn_vtxsn_egap1_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[21][ihar][7]));
//             rawvn_vtxsn_egap2_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[24][ihar][7]));
//             rawvn_pt_vtxsn_egap1_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_cos[ihar]);
//             rawvn_pt_vtxsn_egap2_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_cos[ihar]);
//             rawvn_pt_vtxsn_egap1_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_sin[ihar]);
//             rawvn_pt_vtxsn_egap2_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_sin[ihar]);
//             rawvn_cent_vtxsn_egap1_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_cos[ihar]);
//             rawvn_cent_vtxsn_egap2_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_cos[ihar]);
//             rawvn_cent_vtxsn_egap1_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_sin[ihar]);
//             rawvn_cent_vtxsn_egap2_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_sin[ihar]);
// 						if( isElectron(cnt) ){
// 							rawvn_vtxsn_egap1_e_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[21][ihar][7]));
// 							rawvn_vtxsn_egap2_e_cos[ihar] = cos((ihar+1.)*(phi-m_F_Psi[24][ihar][7]));
// 							rawvn_vtxsn_egap1_e_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[21][ihar][7]));
// 							rawvn_vtxsn_egap2_e_sin[ihar] = sin((ihar+1.)*(phi-m_F_Psi[24][ihar][7]));
// 							rawvn_pt_vtxsn_egap1_e_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_cos[ihar]);
// 							rawvn_pt_vtxsn_egap2_e_cos[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_cos[ihar]);
// 							rawvn_pt_vtxsn_egap1_e_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap1_sin[ihar]);
// 							rawvn_pt_vtxsn_egap2_e_sin[icent][ihar]->Fill(pt, rawvn_vtxsn_egap2_sin[ihar]);
// 							rawvn_cent_vtxsn_egap1_e_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_cos[ihar]);
// 							rawvn_cent_vtxsn_egap2_e_cos[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_cos[ihar]);
// 							rawvn_cent_vtxsn_egap1_e_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap1_sin[ihar]);
// 							rawvn_cent_vtxsn_egap2_e_sin[ipt][ihar]->Fill(icent, rawvn_vtxsn_egap2_sin[ihar]);
// 						}
          }
        }
      }
    }
  }

  return EVENT_OK;
}
//==============================================================
void RPCalib_priv::GetQAHistos( int icent, int izv )
{
  if( m_Re_Q[27][0] > -900 && m_Re_Q[27][1] > -900 )
    scat_q_mpcsn->Fill(m_Re_Q[27][1],m_Re_Q[27][0]);
  if( m_Re_Q[30][0] > -900 && m_Re_Q[30][1] > -900 )
    scat_q_bbcsn->Fill(m_Re_Q[30][1],m_Re_Q[30][0]);
  if( m_Re_Q[21][0] > -900 && m_Re_Q[21][1] > -900 )
    scat_q_vtxsn->Fill(m_Re_Q[12][1],m_Re_Q[12][0]);
  for( int ihar = 0; ihar < nhar; ihar++ ){
    if( m_F_Psi[25][ihar][7] > -900 && m_F_Psi[26][ihar][7] > -900 )
      scat_psi_mpcsn[ihar]->Fill(m_F_Psi[25][ihar][7],m_F_Psi[26][ihar][7]);
    if( m_F_Psi[28][ihar][7] > -900 && m_F_Psi[29][ihar][7] > -900 )
      scat_psi_bbcsn[ihar]->Fill(m_F_Psi[28][ihar][7],m_F_Psi[29][ihar][7]);
    if( m_F_Psi[19][ihar][7] > -900 && m_F_Psi[20][ihar][7] > -900 )
      scat_psi_vtxsn_egap1[ihar]->Fill(m_F_Psi[19][ihar][7],m_F_Psi[20][ihar][7]);
    if( m_F_Psi[22][ihar][7] > -900 && m_F_Psi[23][ihar][7] > -900 )
      scat_psi_vtxsn_egap2[ihar]->Fill(m_F_Psi[22][ihar][7],m_F_Psi[23][ihar][7]);
    if( m_F_Psi[30][ihar][7] > -900 ){
      bbcsn_cos[ihar]->Fill(0., cos((ihar+1)*m_F_Psi[30][ihar][7]));
      bbcsn_sin[ihar]->Fill(0., sin((ihar+1)*m_F_Psi[30][ihar][7]));
      m_Cosnpsi_BBC[ihar] = cos((ihar+1)*m_F_Psi[30][ihar][7]);
    }
    if( m_F_Psi[21][ihar][7] > -900 ){
      vtxsn_egap1_cos[ihar]->Fill(0., cos((ihar+1)*m_F_Psi[21][ihar][7]));
      vtxsn_egap1_sin[ihar]->Fill(0., sin((ihar+1)*m_F_Psi[21][ihar][7]));
      m_Cosnpsi_Egap1[ihar] = cos((ihar+1)*m_F_Psi[21][ihar][7]);
    }
    if( m_F_Psi[24][ihar][7] > -900 ){
      vtxsn_egap2_cos[ihar]->Fill(0., cos((ihar+1)*m_F_Psi[24][ihar][7]));
      vtxsn_egap2_sin[ihar]->Fill(0., sin((ihar+1)*m_F_Psi[24][ihar][7]));
      m_Cosnpsi_Egap2[ihar] = cos((ihar+1)*m_F_Psi[24][ihar][7]);
    }
  }
  for( int ihar = 0; ihar < nhar; ihar++ ){
    if( m_F_Psi[30][ihar][7] > -900 ){
      nclustsum_bbc[ihar]->Fill(0., m_nclstsum_bbc);
    }
    if( m_F_Psi[21][ihar][7] > -900 ){
      for( int ilayer = 0; ilayer < 4; ilayer++ ){
        nclust_egap1[ihar][ilayer]->Fill(0., m_nclst_egap1[ilayer]);
        cent_nclust_egap1[ihar][ilayer]->Fill(icent, m_nclst_egap1[ilayer]);
        zvtx_nclust_egap1[ihar][ilayer]->Fill(izv, m_nclst_egap1[ilayer]);
      }
      nclustsum_egap1[ihar]->Fill(0., m_nclst_egap1[0]+m_nclst_egap1[1]+m_nclst_egap1[2]+m_nclst_egap1[3]);
    }
    if( m_F_Psi[24][ihar][7] > -900 ){
      for( int ilayer = 0; ilayer < 4; ilayer++ ){
        nclust_egap2[ihar][ilayer]->Fill(0., m_nclst_egap2[ilayer]);
        cent_nclust_egap2[ihar][ilayer]->Fill(icent, m_nclst_egap2[ilayer]);
        zvtx_nclust_egap2[ihar][ilayer]->Fill(izv, m_nclst_egap2[ilayer]);
      }
      nclustsum_egap2[ihar]->Fill(0., m_nclst_egap2[0]+m_nclst_egap2[1]+m_nclst_egap2[2]+m_nclst_egap2[3]);
    }

    if( m_F_Psi[30][ihar][7] > -900 ){
      cent_bbcsn_cos[ihar]->Fill(icent, cos((ihar+1)*m_F_Psi[30][ihar][7]));
      cent_bbcsn_sin[ihar]->Fill(icent, sin((ihar+1)*m_F_Psi[30][ihar][7]));
      zvtx_bbcsn_cos[ihar]->Fill(izv, cos((ihar+1)*m_F_Psi[30][ihar][7]));
      zvtx_bbcsn_sin[ihar]->Fill(izv, sin((ihar+1)*m_F_Psi[30][ihar][7]));
    }
    if( m_F_Psi[21][ihar][7] > -900 ){
      cent_vtxsn_egap1_cos[ihar]->Fill(icent, cos((ihar+1)*m_F_Psi[21][ihar][7]));
      cent_vtxsn_egap1_sin[ihar]->Fill(icent, sin((ihar+1)*m_F_Psi[21][ihar][7]));
      zvtx_vtxsn_egap1_cos[ihar]->Fill(izv, cos((ihar+1)*m_F_Psi[21][ihar][7]));
      zvtx_vtxsn_egap1_sin[ihar]->Fill(izv, sin((ihar+1)*m_F_Psi[21][ihar][7]));
    }
    if( m_F_Psi[24][ihar][7] > -900 ){
      cent_vtxsn_egap2_cos[ihar]->Fill(icent, cos((ihar+1)*m_F_Psi[24][ihar][7]));
      cent_vtxsn_egap2_sin[ihar]->Fill(icent, sin((ihar+1)*m_F_Psi[24][ihar][7]));
      zvtx_vtxsn_egap2_cos[ihar]->Fill(izv, cos((ihar+1)*m_F_Psi[24][ihar][7]));
      zvtx_vtxsn_egap2_sin[ihar]->Fill(izv, sin((ihar+1)*m_F_Psi[24][ihar][7]));
    }
    if( m_F_Psi[30][ihar][7] > -900 ){
      cent_nclustsum_bbc[ihar]->Fill(icent, m_nclstsum_bbc);
      zvtx_nclustsum_bbc[ihar]->Fill(izv, m_nclstsum_bbc);
    }
    if( m_F_Psi[21][ihar][7] > -900 ){
      cent_nclustsum_egap1[ihar]->Fill(icent, m_nclst_egap1[0]+m_nclst_egap1[1]+m_nclst_egap1[2]+m_nclst_egap1[3]);
      zvtx_nclustsum_egap1[ihar]->Fill(izv, m_nclst_egap1[0]+m_nclst_egap1[1]+m_nclst_egap1[2]+m_nclst_egap1[3]);
    }
    if( m_F_Psi[24][ihar][7] > -900 ){
      cent_nclustsum_egap2[ihar]->Fill(icent, m_nclst_egap2[0]+m_nclst_egap2[1]+m_nclst_egap2[2]+m_nclst_egap2[3]);
      zvtx_nclustsum_egap2[ihar]->Fill(izv, m_nclst_egap2[0]+m_nclst_egap2[1]+m_nclst_egap2[2]+m_nclst_egap2[3]);
    }
  }
  for( int ico = 0; ico < 2; ico++ ){
    if( m_Q[18][ico] > -900 ){
      cent_q_bbc[ico]->Fill(icent, m_Q[18][ico]/m_nclstsum_bbc);
      evtsq_q_bbc[ico]->Fill(m_EvtSequence, m_Q[18][ico]/m_nclstsum_bbc);
    }
    if( m_Q[12][ico] > -900 ){
      cent_q_vtx[ico]->Fill(icent, m_Q[12][ico]/(m_nclst[0]+m_nclst[1]+m_nclst[2]+m_nclst[3]));
      evtsq_q_vtx[ico]->Fill(m_EvtSequence, m_Q[12][ico]/(m_nclst[0]+m_nclst[1]+m_nclst[2]+m_nclst[3]));
    }
  }
  for( int inf = 0; inf < 8; inf++ ){
    if( m_F_Psi[30][1][inf] > -900 )
      cent_fpsiv2_bbc[inf]->Fill(icent, m_F_Psi[30][1][inf]);
    if( m_F_Psi[21][1][inf] > -900 )
      cent_fpsiv2_egap1[inf]->Fill(icent, m_F_Psi[21][1][inf]);
    if( m_F_Psi[24][1][inf] > -900 )
      cent_fpsiv2_egap2[inf]->Fill(icent, m_F_Psi[24][1][inf]);
  }

  return;
}
//==============================================================
int RPCalib_priv::MaskSensor( SvxClusterList* clusterlist )
{
  int n = m_nclstsum = clusterlist->get_nClusters();

  for( int ilayer = 0; ilayer < 2; ilayer++ ){
    for( int iladder = 0; iladder < 20; iladder++ ){
      for( int isensor = 0; isensor < 4; isensor++ ){
        m_clsns[ilayer][iladder][isensor] = 0;
        for( int ichip = 0; ichip < 4; ichip++ ){
          m_clsnc[ilayer][iladder][isensor][ichip] = 0;
        }
      }
    }
  }
  for( int i = 0; i < n; i++ ){
    SvxCluster* cluster;
    cluster = clusterlist->get_Cluster(i);
    int layer = cluster->get_layer();
    int ladder = cluster->get_ladder();
    int sensor = cluster->get_sensor();
    int chip = 0;
    //cout << layer << " " << ladder << " " << sensor << endl;
    //layer = 0; ladder = 0; sensor =0;
//     if(layer == 0 && ladder == 0 && (sensor == 2 || sensor == 3)) return(ABORTEVENT);
//     else if(layer == 1 && (ladder == 0 || ladder == 1) && (sensor == 2 || sensor == 3)) return(ABORTEVENT);
    //if(layer > 1) continue; // Skip Strip
    for( int j = 0; j < 3; j++ ){
      m_clsxyz[j] = cluster->get_xyz_local(j);
    }
    // CHIP 0
    if( 1.3980 < m_clsxyz[2] && m_clsxyz[2] < 2.7770 ) chip = 0;
    else if( 0 < m_clsxyz[2] && m_clsxyz[2] <= 1.3980 ) chip = 1;
    else if( -1.3980 < m_clsxyz[2] && m_clsxyz[2] <= 0 ) chip = 2;
    else if( -2.770 < m_clsxyz[2] && m_clsxyz[2] <= -1.3980 ) chip = 3;
    else continue;
    m_layer  = layer;
    m_ladder = ladder;
    m_sensor = sensor;
    if(layer < 2){
      m_clsns[layer][ladder][sensor]++;
      m_clsnc[layer][ladder][sensor][chip]++;
      //pixelhmp[layer][ladder][sensor]->Fill(m_clsxyz[2], m_clsxyz[0]);
    }
  }

//   int laddermax;
//   for( int ilayer = 0; ilayer < 2; ilayer++ ){
//     if(ilayer == 0) laddermax = 5;
//     else laddermax = 10;
//     for( int iladder = 0; iladder < laddermax; iladder++ ){
//       for( int isensor = 0; isensor < 4; isensor++ ){
//         for( int ichip = 0; ichip < 4; ichip++ ){
//           float m_clusternum = (float)m_clsn[ilayer][iladder][isensor][ichip];
//           if( m_clusternum > 0 ){
//             int ievsqbin = m_EvtSequence/100000;
//             clusternum[ilayer][iladder][isensor][ichip]->Fill(ievsqbin, m_clusternum);
//           }
//         }
//       }
//     }
//   }
  cls->Fill();

  return(EVENT_OK);
}
