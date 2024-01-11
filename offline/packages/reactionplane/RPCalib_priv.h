#ifndef __RPCALIB_PRIV_H__
#define __RPCALIB_PRIV_H__

#include "SubsysReco.h"
#include <string>
#include "SvxClusterList.h"

class Fun4AllHistoManager;
class TProfile;
class TH1;
class TH2;
class TFile;
class TTree;

class PHSnglCentralTrack;
class SvxCentralTrackMapEntry;
class ReactionPlaneCalibv1;

class RPCalib_priv: public SubsysReco 
{

public:

  RPCalib_priv(const char* output = "RPCalib_priv", int agsegnum = 888);
  virtual ~RPCalib_priv();

  virtual int  Init(PHCompositeNode *topNode);
  virtual int  process_event(PHCompositeNode *topNode);
  virtual int  End(PHCompositeNode *topNode);

  virtual int  Reset(PHCompositeNode *topNode) { return 0; }
  virtual int  ResetEvent(PHCompositeNode *topNode) { return 0; }
  virtual int  InitRun(PHCompositeNode *topNode);
//  virtual void Print(const std::string& what) const { return; }

  virtual void setRecalFlag(int flag) { m_RecalFlag = flag; }
  virtual void setMasterRecalFlag(int flag) { m_MasterRecalFlag = flag; }
  virtual void setVnFlag(int flag) { m_VnFlag = flag; }
  virtual void setEvSkipFlag(int flag) { m_EvSkipFlag = flag; }

protected:

  int CreateNodeTree(PHCompositeNode *topNode) { return 0; }
  TFile* OutputNtupleFile;
  TTree* ntp;
  TTree* cls;

  int m_RecalFlag;       // 0 : use caliblation parameters
                         // 1 : generate re-centering parameters
                         // 2 : generate flattening parameters
  int m_MasterRecalFlag; // 0 : master recalibrator module doesn't run
                         // 1 : run master recalibrator module
	int m_VnFlag;          // 0 : Resolution data for Vn is not created
	                       // 1 : Resolution data is generated.
	int m_EvSkipFlag;

private:
  const std::string OutputFileName;
  int m_EventNumber;
  int calFlag;
  int m_type, m_cent, m_icent, m_nfn;
  int m_runnumber, m_agsegnum;

  const static int ncent = 20; //ncent=5;
  const static int nzv = 10;   //nzv=20;
  const static int nhar = 6;  
  const static int ndet1 = 76; 
  const static int ndet2 = 88; 
  const static int ndet3 = 39;
  const static int mdet = 74; 
  const static int mmdet = 0; 
  const static int nfn = 8;
  const static int npt = 5;
  const static int nocalib_flag = 2;
  const static int recent_flag = 0;
  const static int flat_flag = 1;
  
  Fun4AllHistoManager *HistoManager;
  ReactionPlaneCalibv1* rpcalibv1;

  void initHisto();
  void initEvtTree();
  void initClstTree();
  void initRpCalibData(char* calib);
  void initCentCalibData(char* calib);
  void EndCalibData();
  int  TickCut();
  int  DetSkip(int idet);
  int  EventSkip();
  int  CalcVnReso(PHCompositeNode *topNode, int icent);
  int  GetVn(PHCompositeNode *topNode, int icent);
  bool isElectron(PHSnglCentralTrack* cnt);
  bool goodDCAAbs(SvxCentralTrackMapEntry* svxcnt, float dcacut);
  void GetQAHistos( int icent, int izv );
  int  MaskSensor(SvxClusterList* clusterlist);

  float ave[ncent][nzv][ndet2][nhar][2];
  float RMS[ncent][nzv][ndet2][nhar][2];
  float fla[ncent][nzv][ndet2][nhar][2][nfn];
  float m_reso_mpcsn[ncent][nhar][2];
  float m_reso_bbcsn[ncent][nhar][2];
  float m_reso_vtxs_egap1[ncent][nhar][2];
  float m_reso_vtxs_egap2[ncent][nhar][2];
  float m_reso_vtxn_egap1[ncent][nhar][2];
  float m_reso_vtxn_egap2[ncent][nhar][2];
  float m_reso_vtxsn_egap1[ncent][nhar][2];
  float m_reso_vtxsn_egap2[ncent][nhar][2];
  float m_Re_Psi2[ndet3], m_F_Psi[ndet3][nhar][nfn], m_F_Psi2[ndet3][nfn], m_F_Psi7[ndet3][nhar];
  float m_Q[52][3], m_Re_Q[ndet3][2];
  float /*m_pt, m_vn[38][ncent][nhar],*/ m_zvtx;
  float m_bbcz;
  int   m_nclst[4], m_bbcq, m_pticks[3];
  int   m_nclstsum_bbc;
  int   m_nclst_egap1[4], m_nclst_egap2[4];
  int   m_bbccut[ncent-1];
  int   m_EvtSequence, m_ievtsq;
  int   m_layer, m_ladder, m_sensor;
  float m_clsxyz[3];
  float m_clsns[2][20][4];
  float m_clsnc[2][20][4][4];
  int   m_nclstsum;
//  int   m_maxladder, m_maxsensor;
  
  float m_Cosnpsi_BBC[nhar];
  float m_Cosnpsi_Egap1[nhar];
  float m_Cosnpsi_Egap2[nhar];
  TProfile* q[ncent][nzv][ndet2][nhar];
  TProfile* clusternum[2][20][4][4];
  TProfile* f_q[ncent][nzv][ndet2][nhar][2];
  TProfile* Vtx_bbc_reso[mdet+13][2]; // 13 : additional plane
  TProfile* Cor_S_N[13][2]; // 13 : type
  TProfile* vtx_bbc_reso[mdet+13][2]; // 13 : additional plane
  TProfile* cor_S_N[13][2]; // 13 : type

  TProfile* corr_cent_vtxs_egap1_bbcsn[nhar][2];
  TProfile* corr_cent_vtxs_egap2_bbcsn[nhar][2];
  TProfile* corr_cent_vtxn_egap1_bbcsn[nhar][2];
  TProfile* corr_cent_vtxn_egap2_bbcsn[nhar][2];
  TProfile* corr_cent_vtxsn_egap1_bbcsn[nhar][2];
  TProfile* corr_cent_vtxsn_egap2_bbcsn[nhar][2];
  TProfile* corr_cent_vtxsn_egap1_bbcs[nhar][2];
  TProfile* corr_cent_vtxsn_egap2_bbcs[nhar][2];
  TProfile* corr_cent_vtxsn_egap1_bbcn[nhar][2];
  TProfile* corr_cent_vtxsn_egap2_bbcn[nhar][2];
  TProfile* corr_cent_mpcs_mpcn[nhar][2];
  TProfile* corr_cent_bbcs_bbcn[nhar][2];

  TProfile* vn_pt_bbcsn_cos[ncent][nhar];
	TProfile* vn_pt_bbcsn_sin[ncent][nhar];
  TProfile* vn_pt_vtxsn_egap1_cos[ncent][nhar];
	TProfile* vn_pt_vtxsn_egap1_sin[ncent][nhar];
  TProfile* vn_pt_vtxsn_egap2_cos[ncent][nhar];
	TProfile* vn_pt_vtxsn_egap2_sin[ncent][nhar];
  TProfile* vn_pt_vtxsn_egap1_e_cos[ncent][nhar];
	TProfile* vn_pt_vtxsn_egap1_e_sin[ncent][nhar];
  TProfile* vn_pt_vtxsn_egap2_e_cos[ncent][nhar];
	TProfile* vn_pt_vtxsn_egap2_e_sin[ncent][nhar];
  TProfile* vn_cent_bbcsn_cos[npt][nhar];
	TProfile* vn_cent_bbcsn_sin[npt][nhar];
  TProfile* vn_cent_vtxsn_egap1_cos[npt][nhar];
	TProfile* vn_cent_vtxsn_egap1_sin[npt][nhar];
  TProfile* vn_cent_vtxsn_egap2_cos[npt][nhar];
	TProfile* vn_cent_vtxsn_egap2_sin[npt][nhar];
  TProfile* vn_cent_vtxsn_egap1_e_cos[npt][nhar];
	TProfile* vn_cent_vtxsn_egap1_e_sin[npt][nhar];
  TProfile* vn_cent_vtxsn_egap2_e_cos[npt][nhar];
	TProfile* vn_cent_vtxsn_egap2_e_sin[npt][nhar];
	TProfile* rawvn_pt_bbcsn_cos[ncent][nhar];
	TProfile* rawvn_pt_bbcsn_sin[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap1_cos[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap1_sin[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap2_cos[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap2_sin[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap1_e_cos[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap1_e_sin[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap2_e_cos[ncent][nhar];
	TProfile* rawvn_pt_vtxsn_egap2_e_sin[ncent][nhar];
	TProfile* rawvn_cent_bbcsn_cos[npt][nhar];
	TProfile* rawvn_cent_bbcsn_sin[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap1_cos[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap1_sin[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap2_cos[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap2_sin[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap1_e_cos[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap1_e_sin[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap2_e_cos[npt][nhar];
	TProfile* rawvn_cent_vtxsn_egap2_e_sin[npt][nhar];

	TProfile* bbcsn_cos[nhar];
	TProfile* bbcsn_sin[nhar];
	TProfile* vtxsn_egap1_cos[nhar];
	TProfile* vtxsn_egap1_sin[nhar];
	TProfile* vtxsn_egap2_cos[nhar];
	TProfile* vtxsn_egap2_sin[nhar];

	TProfile* cent_bbcsn_cos[nhar];
	TProfile* cent_bbcsn_sin[nhar];
	TProfile* cent_vtxsn_egap1_cos[nhar];
	TProfile* cent_vtxsn_egap1_sin[nhar];
	TProfile* cent_vtxsn_egap2_cos[nhar];
	TProfile* cent_vtxsn_egap2_sin[nhar];
	TProfile* zvtx_bbcsn_cos[nhar];
	TProfile* zvtx_bbcsn_sin[nhar];
	TProfile* zvtx_vtxsn_egap1_cos[nhar];
	TProfile* zvtx_vtxsn_egap1_sin[nhar];
	TProfile* zvtx_vtxsn_egap2_cos[nhar];
	TProfile* zvtx_vtxsn_egap2_sin[nhar];

  TProfile* nclust_egap1[nhar][4];
  TProfile* nclust_egap2[nhar][4];
  TProfile* nclustsum_bbc[nhar];
  TProfile* nclustsum_egap1[nhar];
  TProfile* nclustsum_egap2[nhar];
  TProfile* cent_nclust_egap1[nhar][4];
  TProfile* cent_nclust_egap2[nhar][4];
  TProfile* cent_nclustsum_bbc[nhar];
  TProfile* cent_nclustsum_egap1[nhar];
  TProfile* cent_nclustsum_egap2[nhar];
  TProfile* zvtx_nclust_egap1[nhar][4];
  TProfile* zvtx_nclust_egap2[nhar][4];
  TProfile* zvtx_nclustsum_bbc[nhar];
  TProfile* zvtx_nclustsum_egap1[nhar];
  TProfile* zvtx_nclustsum_egap2[nhar];

  TProfile* cent_q_bbc[2];
  TProfile* cent_q_vtx[2];
  TProfile* evtsq_q_bbc[2];
  TProfile* evtsq_q_vtx[2];
  TProfile* cent_fpsiv2_bbc[8];
  TProfile* cent_fpsiv2_egap1[8];
  TProfile* cent_fpsiv2_egap2[8];

  TH1* BBC_charge;
//  TH1* psi_hist0;
  TH1* psi_hist[ncent][12];
  TH1* z_vertex;
//  TH1* recal_psi_hist[ncent][12];
//  TH2* vtx_bbc[4];
//  TH2* SUMXY_MPC[3];
//  TH2* QA_Psi_z[nzv][ndet2][nhar];
//  TH2* QA_Psi_c[ncent][ndet2][nhar];
  TH2* scat_q_mpcsn;
  TH2* scat_q_bbcsn;
  TH2* scat_q_vtxsn;
  TH2* scat_psi_mpcsn[nhar];
  TH2* scat_psi_bbcsn[nhar];
  TH2* scat_psi_vtxsn_egap1[nhar];
  TH2* scat_psi_vtxsn_egap2[nhar];

  TH2* pixelhmp[2][20][4];
};



#endif

