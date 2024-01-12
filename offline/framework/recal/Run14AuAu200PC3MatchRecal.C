#include "Run14AuAu200PC3MatchRecal.h"

#include "Fun4AllReturnCodes.h"
#include "PHCompositeNode.h"
#include "recoConsts.h"
#include "getClass.h"

#include "RunHeader.h"
#include "PHGlobal.h"
#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"

#include "TOAD.h"

#include <TDirectory.h>
#include <TFile.h>
#include <TH1.h>
#include <TF1.h>
#include <TGraphErrors.h>

#include <iostream>
#include <cmath>
#include <algorithm>

using namespace std;
using namespace findNode;

Run14AuAu200PC3MatchRecal::Run14AuAu200PC3MatchRecal(const char* name): 
  Recalibrator(name),
  m_runNumber(0),
  m_cnt(NULL),
  m_global(NULL)
{
  baseclasses.insert("PHCentralTrack");

  for(int ic=0; ic<2; ic++){
    for(int iarm=0; iarm<2; iarm++){
      for(int ized=0; ized<10; ized++){
        m_fn_dphi_mean[ ic][iarm][ized] = NULL;
        m_fn_dphi_sigma[ic][iarm][ized] = NULL;
        m_fn_dz_mean[   ic][iarm][ized] = NULL; 
        m_fn_dz_sigma[  ic][iarm][ized] = NULL;
        m_g_dphi_mean[ ic][iarm][ized]  = NULL;
        m_g_dphi_sigma[ic][iarm][ized]  = NULL;
        m_g_dz_mean[   ic][iarm][ized]  = NULL; 
        m_g_dz_sigma[  ic][iarm][ized]  = NULL;
      } 
    } 
  } 
}

Run14AuAu200PC3MatchRecal::~Run14AuAu200PC3MatchRecal()
{
  for(int ic=0; ic<2; ic++){
    for(int iarm=0; iarm<2; iarm++){
      for(int ized=0; ized<10; ized++){
        if(m_fn_dphi_mean[ ic][iarm][ized]!=NULL) delete m_fn_dphi_mean[ ic][iarm][ized];
        if(m_fn_dphi_sigma[ic][iarm][ized]!=NULL) delete m_fn_dphi_sigma[ic][iarm][ized];
        if(m_fn_dz_mean[   ic][iarm][ized]!=NULL) delete m_fn_dz_mean[   ic][iarm][ized]; 
        if(m_fn_dz_sigma[  ic][iarm][ized]!=NULL) delete m_fn_dz_sigma[  ic][iarm][ized];
        if(m_g_dphi_mean[ ic][iarm][ized]!=NULL)  delete m_g_dphi_mean[ ic][iarm][ized];
        if(m_g_dphi_sigma[ic][iarm][ized]!=NULL)  delete m_g_dphi_sigma[ic][iarm][ized];
        if(m_g_dz_mean[   ic][iarm][ized]!=NULL)  delete m_g_dz_mean[   ic][iarm][ized]; 
        if(m_g_dz_sigma[  ic][iarm][ized]!=NULL)  delete m_g_dz_sigma[  ic][iarm][ized];
      } 
    } 
  } 

}

void Run14AuAu200PC3MatchRecal::help()
{
    cout << "==================================================================="                              << endl;
    cout << "Run14AuAu200PC3MatchREcal::help method output"                                              << endl;
    cout << "Author:  Takashi Hachiya (hachiya@rcf.rhic.bnl.gov). " << endl;
    cout << "         This recalibrator updates the pc3sdphi and pc3sdz values in PHCentralTrack   "             << endl;
    cout << "             modeling method is used to decide "                                             << endl;
    cout << "             the offset and sigma value as a function of pT"                                      << endl;
    cout                                                                                                       << endl;
    cout << "==================================================================="                              << endl;
}


int Run14AuAu200PC3MatchRecal::Init(PHCompositeNode *topNode)
{
  return 0;
}


int Run14AuAu200PC3MatchRecal::InitRun(PHCompositeNode *topNode)
{
  RunHeader* run = getClass<RunHeader>(topNode, "RunHeader");
  if(!run){
    cout << PHWHERE << " RunHeader not found" << endl;
    return 0;
  }

  m_runNumber = run->get_RunNumber();

  InitParameters();

  return 0;
}

int Run14AuAu200PC3MatchRecal::isValidRun(const int runno) const
{

  // Run14AuAu 200 GeV
  // to check the run range, see https://www.phenix.bnl.gov/WWW/run/14/history.html and run control log
  if (405639 <= runno && runno <=414988)
  {
    return 1;
  }

  return 0;
}

int Run14AuAu200PC3MatchRecal::process_event(PHCompositeNode *topNode)
{
  //cout<<"PC3MatchRecal::process_event"<<" :"<<inputnodename.c_str()<<":"<<endl;
  //m_cnt    = getClass<PHCentralTrack>(topNode, inputnodename.c_str());
  m_cnt    = getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  m_global = getClass<PHGlobal>(      topNode, "PHGlobal");

  if (!m_cnt || !m_global) {
    if(!m_cnt)    cout<<"No PHCentralTrack"<<endl;
    if(!m_global) cout<<"No PHGlobal"<<endl;
    return 0;
  }

  Calibrate();

  return EVENT_OK;
}

int Run14AuAu200PC3MatchRecal::Calibrate()
{
  //cout<<"PC3MatchRecal::Calibrate "<<m_cnt->get_npart()<<endl;
  for (unsigned int itrk = 0; itrk < m_cnt->get_npart(); itrk++)
    {

      PHSnglCentralTrack *sngltrk = m_cnt->get_track(itrk);
      sngltrk->ShutUp();
      if (
        sngltrk->isImplemented(sngltrk->get_the0()) &&
        sngltrk->isImplemented(sngltrk->get_mom()) &&
        sngltrk->isImplemented(sngltrk->get_charge()) &&
        sngltrk->isImplemented(sngltrk->get_zed()) &&
        sngltrk->isImplemented(sngltrk->get_phi()) &&
        sngltrk->isImplemented(sngltrk->get_pc3dphi()) &&
        sngltrk->isImplemented(sngltrk->get_pc3dz()) &&
        sngltrk->isImplemented(sngltrk->get_dcarm())
          )
        {
          if (verbosity > 0) cout << PHWHERE << " " << Name() << "Workable" << endl;
        }
      else
        {
          sngltrk->ShutUp(1);
          if (verbosity > 0) cout << PHWHERE << " " << Name() << "Not workable" << endl;
          return 0;
        }
      
      ///////////////////////////
      sngltrk->ShutUp(1); // enable virtual warnings again

      float phi  = sngltrk->get_phi();
      float the0 = sngltrk->get_the0();
      float mom  = sngltrk->get_mom();

      float pt   = mom;
      if (the0 > -999)
        {
          pt = mom * sin(the0);
        }

      int   arm    = (phi<1.5 ? 0 : 1); //(int)sngltrk->get_dcarm();
      short charge = sngltrk->get_charge();
      float zed    = sngltrk->get_zed();

      // Matching variables
      double pc3dphi  = sngltrk->get_pc3dphi();
      double pc3dz    = sngltrk->get_pc3dz();
      double spc3dphi = sngltrk->get_spc3dphi();
      double spc3dz   = sngltrk->get_spc3dz();

      //Calculate the new variables
      // ------------------
      static const int NZED=10;

      int icharge = (charge == 1) ? 0 : 1;
      int iarm    = arm;
      int ized    = (int)(NZED * (zed + 75.0) / 150.0);

      float pc3sdz   = -999., pc3sdphi  = -999.;
      float spc3sdz  = -999., spc3sdphi = -999.;

      bool isChgOK, isArmOK, isZedOK;
      if((isChgOK=IsChargeRangeOK(icharge)) &&
         (isArmOK=IsArmRangeOK(iarm))       &&
         (isZedOK=IsZedRangeOK(ized))       )      
      {
        if(pc3dphi>-998 && pc3dz>-998) {
          pc3sdz   = Calc(pc3dz,   pt, icharge, iarm, ized, 0);
          pc3sdphi = Calc(pc3dphi, pt, icharge, iarm, ized, 1);
        }
        if(spc3dphi>-998 && spc3dz>-998) {
          spc3sdz   = Calc(spc3dz,   pt, icharge, iarm, ized, 0);
          spc3sdphi = Calc(spc3dphi, pt, icharge, iarm, ized, 1);
        }
      }
      else {
        if(verbosity>3){
          if(!isChgOK) cerr<<"PC3 matching Calc : out of range : icharge = "<<icharge<<" charge="<<charge<<endl;
          if(!isArmOK) cerr<<"PC3 matching Calc : out of range : iarm = "   <<iarm   <<" phi="   <<phi<<endl;
          if(!isZedOK) cerr<<"PC3 matching Calc : out of range : ized = "   <<ized   <<" zed="   <<zed<<endl;
        }
      }
    

      //Set the new variables
      sngltrk->set_pc3sdphi(pc3sdphi);
      sngltrk->set_pc3sdz(  pc3sdz);
      sngltrk->set_spc3sdphi(spc3sdphi);
      sngltrk->set_spc3sdz(  spc3sdz);
      //cout<<"calibrate : "<<pc3dphi<<" "<<pc3dz<<" : "<<pc3sdphi<<" "<<pc3sdz<<" "<<pt<<endl;
    }

  return EVENT_OK;
}


double Run14AuAu200PC3MatchRecal::CalcMeanDZ(double pt, int icharge, int iarm, int ized)
{
  TGraphErrors *g = m_g_dz_mean[icharge][iarm][ized];
  if(NULL==g) return -19999.;

  double mean=0;

  if(0.5<=pt&&pt<7.5){
    mean = g->Eval(pt);
  }
  else if(7.5<=pt){ // use fit function
    TF1* func = m_fn_dz_mean[icharge][iarm][ized];
    if(NULL==func) return -29999.;

    mean = func->Eval(pt);
  }
  
  return mean;
}

double Run14AuAu200PC3MatchRecal::CalcSigmaDZ(double pt, int icharge, int iarm, int ized)
{
  //if(NULL==g) return -19998.;

  TF1* func = m_fn_dz_sigma[icharge][iarm][ized];
  if(NULL==func) return -29998.;

  double sigma= func->Eval(pt);
  
  return sigma;
}

double Run14AuAu200PC3MatchRecal::CalcMeanDPHI(double pt, int icharge, int iarm, int ized)
{
  double mean=0;

  if(0.5<=pt&&pt<7.5){
    TGraphErrors *g = m_g_dphi_mean[icharge][iarm][ized];
    if(NULL==g) return -19997.;

    mean = g->Eval(pt);
  }
  else if(7.5<=pt){ // use fit function
    TF1* func = m_fn_dphi_mean[icharge][iarm][ized];
    if(NULL==func) return -29997;

    mean = func->Eval(pt);
  }
  
  return mean;
}

double Run14AuAu200PC3MatchRecal::CalcSigmaDPHI(double pt, int icharge, int iarm, int ized)
{
//  if(NULL==g) return -19996;

  TF1* func = m_fn_dphi_sigma[icharge][iarm][ized];
  if(NULL==func) return -29996;

  double sigma= func->Eval(pt);
  
  return sigma;
}

double Run14AuAu200PC3MatchRecal::Calc(double raw, double pt, int icharge, int iarm, int ized, int DPHI_DZ)
{
  if(isinf(raw)||isnan(raw)) { return raw;}

  double mean  = (DPHI_DZ==0) ? CalcMeanDZ(  pt, icharge, iarm, ized)
                              : CalcMeanDPHI(pt, icharge, iarm, ized);
  double sigma = (DPHI_DZ==0) ? CalcSigmaDZ(  pt, icharge, iarm, ized)
                              : CalcSigmaDPHI(pt, icharge, iarm, ized);

  if(mean<-9998 || sigma<-9998) { /*cout<<"mm "<<mean<<" "<<sigma<<endl;*/ return -9999.; }
  //if(sigma==0) { cout<<"sigma==0"<<endl; return -9999.; }

  if( isinf(sigma) ) cout<<"sigma : "<<sigma<<endl;
  if( isinf(mean)  ) cout<<"mean : " <<mean <<endl;

  double sigmalized = (raw - mean) / sigma;

  if( isinf(sigmalized) ) cout<<"PC3 "<<(DPHI_DZ==0?"DZ":"DPHI")<<"val : "<<sigmalized<<" "<<raw<<" "<<mean<<" "<<sigma<<endl;

  return sigmalized;
}


void Run14AuAu200PC3MatchRecal::readParameters()
{
/*
  static const TString sf[4] = {"pc3dz_secondcut_mean_h3.root",
                                "pc3dz_secondcut_sigma_h3.root",
                                "pc3dphi_secondcut_mean_h3.root",
                                "pc3dphi_secondcut_sigma_h3.root"};

  TOAD toad_loader("HighpTHadronVn_RP");

  TDirectory *gDir = gDirectory;

  for(int ifile=0; ifile<4; ifile++){
    TString s_fname = toad_loader.location(Form("par/%s", sf[ifile].Data()));

    TFile *f = TFile::Open(s_fname.Data());
    if(f==NULL){
      cout<<"failed to open : "<<s_fname<<endl;
      return;
    }

    gDirectory = gDir;

    for(int ic=0; ic<2; ic++){
      for(int iarm=0; iarm<2; iarm++){
        for(int iz=0; iz<10; iz++){
          if(ifile==0){
            m_gdz_mean_pt[ic][iarm][iz]    = (TGraphErrors*)f->Get(Form("pc3dz_mean_c%dd%dz%d", ic,iarm,iz));
          } else if(ifile==1){
            m_gdz_sigma_pt[ic][iarm][iz]   = (TGraphErrors*)f->Get(Form("pc3dz_sigma_c%dd%dz%d",ic,iarm,iz));
          } else if(ifile==2){
            m_gdphi_mean_pt[ic][iarm][iz]  = (TGraphErrors*)f->Get(Form("pc3dphi_mean_c%dd%dz%d", ic,iarm,iz));
          } else {
            m_gdphi_sigma_pt[ic][iarm][iz] = (TGraphErrors*)f->Get(Form("pc3dphi_sigma_c%dd%dz%d",ic,iarm,iz));
          }
        }
      }
    }
    f->Close();
    delete f;
  }
*/
}

void Run14AuAu200PC3MatchRecal::InitParameters()
{
  initParMatchFunc();
  initParMatchGraph();
}


void Run14AuAu200PC3MatchRecal::initParMatchFunc()
{
  TString sfname[4][2][2][10];
  TString sfunc [4][2][2][10];
  int     nfpar[ 4][2][2][10];
  double  fpar[  4][2][2][10][5];

  //pc3dphi_mean_c0d0z0
  sfname[0][0][0][0] = "m_gpc3dphi_mean_c0d0z0";
  sfunc[0][0][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][0] = 3;
  fpar[0][0][0][ 0][0] =  -5.185677e-04; fpar[0][0][0][ 0][1] =   6.143843e-04; fpar[0][0][0][ 0][2] =  -1.162669e-03; 

  //pc3dphi_mean_c0d0z1
  sfname[0][0][0][1] = "m_gpc3dphi_mean_c0d0z1";
  sfunc[0][0][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][1] = 3;
  fpar[0][0][0][ 1][0] =  -4.905121e-04; fpar[0][0][0][ 1][1] =   6.171264e-05; fpar[0][0][0][ 1][2] =  -4.277677e-04; 

  //pc3dphi_mean_c0d0z2
  sfname[0][0][0][2] = "m_gpc3dphi_mean_c0d0z2";
  sfunc[0][0][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][2] = 3;
  fpar[0][0][0][ 2][0] =  -5.338308e-04; fpar[0][0][0][ 2][1] =  -2.115497e-05; fpar[0][0][0][ 2][2] =  -1.511844e-04; 

  //pc3dphi_mean_c0d0z3
  sfname[0][0][0][3] = "m_gpc3dphi_mean_c0d0z3";
  sfunc[0][0][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][3] = 3;
  fpar[0][0][0][ 3][0] =  -6.791336e-04; fpar[0][0][0][ 3][1] =   6.988888e-05; fpar[0][0][0][ 3][2] =  -6.202392e-06; 

  //pc3dphi_mean_c0d0z4
  sfname[0][0][0][4] = "m_gpc3dphi_mean_c0d0z4";
  sfunc[0][0][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][4] = 3;
  fpar[0][0][0][ 4][0] =  -7.203738e-04; fpar[0][0][0][ 4][1] =   1.466130e-04; fpar[0][0][0][ 4][2] =   6.084890e-05; 

  //pc3dphi_mean_c0d0z5
  sfname[0][0][0][5] = "m_gpc3dphi_mean_c0d0z5";
  sfunc[0][0][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][5] = 3;
  fpar[0][0][0][ 5][0] =  -2.638421e-04; fpar[0][0][0][ 5][1] =  -2.599321e-04; fpar[0][0][0][ 5][2] =   3.080086e-04; 

  //pc3dphi_mean_c0d0z6
  sfname[0][0][0][6] = "m_gpc3dphi_mean_c0d0z6";
  sfunc[0][0][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][6] = 3;
  fpar[0][0][0][ 6][0] =  -3.860889e-04; fpar[0][0][0][ 6][1] =  -2.186988e-04; fpar[0][0][0][ 6][2] =   1.607850e-04; 

  //pc3dphi_mean_c0d0z7
  sfname[0][0][0][7] = "m_gpc3dphi_mean_c0d0z7";
  sfunc[0][0][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][7] = 3;
  fpar[0][0][0][ 7][0] =  -5.588475e-04; fpar[0][0][0][ 7][1] =  -3.101783e-04; fpar[0][0][0][ 7][2] =   5.046895e-05; 

  //pc3dphi_mean_c0d0z8
  sfname[0][0][0][8] = "m_gpc3dphi_mean_c0d0z8";
  sfunc[0][0][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][8] = 3;
  fpar[0][0][0][ 8][0] =  -7.774862e-04; fpar[0][0][0][ 8][1] =  -2.472952e-04; fpar[0][0][0][ 8][2] =  -3.511277e-04; 

  //pc3dphi_mean_c0d0z9
  sfname[0][0][0][9] = "m_gpc3dphi_mean_c0d0z9";
  sfunc[0][0][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][9] = 3;
  fpar[0][0][0][ 9][0] =  -1.151112e-03; fpar[0][0][0][ 9][1] =   4.010102e-04; fpar[0][0][0][ 9][2] =  -1.096113e-03; 

  //pc3dphi_mean_c0d1z0
  sfname[0][0][1][0] = "m_gpc3dphi_mean_c0d1z0";
  sfunc[0][0][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][0] = 3;
  fpar[0][0][1][ 0][0] =   9.522602e-04; fpar[0][0][1][ 0][1] =   7.431840e-04; fpar[0][0][1][ 0][2] =  -9.812984e-04; 

  //pc3dphi_mean_c0d1z1
  sfname[0][0][1][1] = "m_gpc3dphi_mean_c0d1z1";
  sfunc[0][0][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][1] = 3;
  fpar[0][0][1][ 1][0] =   1.075268e-03; fpar[0][0][1][ 1][1] =   4.570173e-04; fpar[0][0][1][ 1][2] =  -5.401064e-04; 

  //pc3dphi_mean_c0d1z2
  sfname[0][0][1][2] = "m_gpc3dphi_mean_c0d1z2";
  sfunc[0][0][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][2] = 3;
  fpar[0][0][1][ 2][0] =   1.142591e-03; fpar[0][0][1][ 2][1] =   2.574044e-04; fpar[0][0][1][ 2][2] =  -1.539499e-04; 

  //pc3dphi_mean_c0d1z3
  sfname[0][0][1][3] = "m_gpc3dphi_mean_c0d1z3";
  sfunc[0][0][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][3] = 3;
  fpar[0][0][1][ 3][0] =   1.296102e-03; fpar[0][0][1][ 3][1] =   6.216082e-05; fpar[0][0][1][ 3][2] =   7.982839e-05; 

  //pc3dphi_mean_c0d1z4
  sfname[0][0][1][4] = "m_gpc3dphi_mean_c0d1z4";
  sfunc[0][0][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][4] = 3;
  fpar[0][0][1][ 4][0] =   1.073783e-03; fpar[0][0][1][ 4][1] =   5.650238e-04; fpar[0][0][1][ 4][2] =  -6.309602e-05; 

  //pc3dphi_mean_c0d1z5
  sfname[0][0][1][5] = "m_gpc3dphi_mean_c0d1z5";
  sfunc[0][0][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][5] = 3;
  fpar[0][0][1][ 5][0] =   9.963506e-04; fpar[0][0][1][ 5][1] =   5.189663e-05; fpar[0][0][1][ 5][2] =   2.073125e-04; 

  //pc3dphi_mean_c0d1z6
  sfname[0][0][1][6] = "m_gpc3dphi_mean_c0d1z6";
  sfunc[0][0][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][6] = 3;
  fpar[0][0][1][ 6][0] =   1.087399e-03; fpar[0][0][1][ 6][1] =  -1.104626e-04; fpar[0][0][1][ 6][2] =   1.721953e-04; 

  //pc3dphi_mean_c0d1z7
  sfname[0][0][1][7] = "m_gpc3dphi_mean_c0d1z7";
  sfunc[0][0][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][7] = 3;
  fpar[0][0][1][ 7][0] =   8.893198e-04; fpar[0][0][1][ 7][1] =   3.713614e-04; fpar[0][0][1][ 7][2] =  -3.307255e-04; 

  //pc3dphi_mean_c0d1z8
  sfname[0][0][1][8] = "m_gpc3dphi_mean_c0d1z8";
  sfunc[0][0][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][8] = 3;
  fpar[0][0][1][ 8][0] =   1.013127e-03; fpar[0][0][1][ 8][1] =   1.849325e-05; fpar[0][0][1][ 8][2] =  -4.769867e-04; 

  //pc3dphi_mean_c0d1z9
  sfname[0][0][1][9] = "m_gpc3dphi_mean_c0d1z9";
  sfunc[0][0][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][9] = 3;
  fpar[0][0][1][ 9][0] =   1.082275e-03; fpar[0][0][1][ 9][1] =  -5.492558e-05; fpar[0][0][1][ 9][2] =  -7.902249e-04; 

  //pc3dphi_mean_c1d0z0
  sfname[0][1][0][0] = "m_gpc3dphi_mean_c1d0z0";
  sfunc[0][1][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][0] = 3;
  fpar[0][1][0][ 0][0] =  -1.206052e-03; fpar[0][1][0][ 0][1] =   8.111162e-04; fpar[0][1][0][ 0][2] =   5.204844e-04; 

  //pc3dphi_mean_c1d0z1
  sfname[0][1][0][1] = "m_gpc3dphi_mean_c1d0z1";
  sfunc[0][1][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][1] = 3;
  fpar[0][1][0][ 1][0] =  -1.349993e-03; fpar[0][1][0][ 1][1] =   7.093315e-04; fpar[0][1][0][ 1][2] =   1.890697e-04; 

  //pc3dphi_mean_c1d0z2
  sfname[0][1][0][2] = "m_gpc3dphi_mean_c1d0z2";
  sfunc[0][1][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][2] = 3;
  fpar[0][1][0][ 2][0] =  -1.319450e-03; fpar[0][1][0][ 2][1] =   3.680308e-04; fpar[0][1][0][ 2][2] =   8.265174e-06; 

  //pc3dphi_mean_c1d0z3
  sfname[0][1][0][3] = "m_gpc3dphi_mean_c1d0z3";
  sfunc[0][1][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][3] = 3;
  fpar[0][1][0][ 3][0] =  -1.451682e-03; fpar[0][1][0][ 3][1] =   3.703709e-04; fpar[0][1][0][ 3][2] =  -1.308588e-04; 

  //pc3dphi_mean_c1d0z4
  sfname[0][1][0][4] = "m_gpc3dphi_mean_c1d0z4";
  sfunc[0][1][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][4] = 3;
  fpar[0][1][0][ 4][0] =  -1.667313e-03; fpar[0][1][0][ 4][1] =   6.849624e-04; fpar[0][1][0][ 4][2] =  -3.966184e-04; 

  //pc3dphi_mean_c1d0z5
  sfname[0][1][0][5] = "m_gpc3dphi_mean_c1d0z5";
  sfunc[0][1][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][5] = 3;
  fpar[0][1][0][ 5][0] =  -9.944137e-04; fpar[0][1][0][ 5][1] =  -2.339432e-06; fpar[0][1][0][ 5][2] =   1.528442e-05; 

  //pc3dphi_mean_c1d0z6
  sfname[0][1][0][6] = "m_gpc3dphi_mean_c1d0z6";
  sfunc[0][1][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][6] = 3;
  fpar[0][1][0][ 6][0] =  -1.043153e-03; fpar[0][1][0][ 6][1] =   9.927043e-05; fpar[0][1][0][ 6][2] =  -2.058494e-05; 

  //pc3dphi_mean_c1d0z7
  sfname[0][1][0][7] = "m_gpc3dphi_mean_c1d0z7";
  sfunc[0][1][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][7] = 3;
  fpar[0][1][0][ 7][0] =  -1.273021e-03; fpar[0][1][0][ 7][1] =   2.002604e-04; fpar[0][1][0][ 7][2] =   1.427574e-04; 

  //pc3dphi_mean_c1d0z8
  sfname[0][1][0][8] = "m_gpc3dphi_mean_c1d0z8";
  sfunc[0][1][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][8] = 3;
  fpar[0][1][0][ 8][0] =  -1.544287e-03; fpar[0][1][0][ 8][1] =   2.147360e-04; fpar[0][1][0][ 8][2] =   4.481998e-04; 

  //pc3dphi_mean_c1d0z9
  sfname[0][1][0][9] = "m_gpc3dphi_mean_c1d0z9";
  sfunc[0][1][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][9] = 3;
  fpar[0][1][0][ 9][0] =  -1.707669e-03; fpar[0][1][0][ 9][1] =   2.693383e-04; fpar[0][1][0][ 9][2] =   7.704302e-04; 

  //pc3dphi_mean_c1d1z0
  sfname[0][1][1][0] = "m_gpc3dphi_mean_c1d1z0";
  sfunc[0][1][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][0] = 3;
  fpar[0][1][1][ 0][0] =   7.023887e-04; fpar[0][1][1][ 0][1] =   3.208995e-04; fpar[0][1][1][ 0][2] =   5.184648e-04; 

  //pc3dphi_mean_c1d1z1
  sfname[0][1][1][1] = "m_gpc3dphi_mean_c1d1z1";
  sfunc[0][1][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][1] = 3;
  fpar[0][1][1][ 1][0] =   8.779616e-04; fpar[0][1][1][ 1][1] =  -8.297764e-05; fpar[0][1][1][ 1][2] =   4.126055e-04; 

  //pc3dphi_mean_c1d1z2
  sfname[0][1][1][2] = "m_gpc3dphi_mean_c1d1z2";
  sfunc[0][1][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][2] = 3;
  fpar[0][1][1][ 2][0] =   7.473705e-04; fpar[0][1][1][ 2][1] =   2.096158e-04; fpar[0][1][1][ 2][2] =   6.404315e-06; 

  //pc3dphi_mean_c1d1z3
  sfname[0][1][1][3] = "m_gpc3dphi_mean_c1d1z3";
  sfunc[0][1][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][3] = 3;
  fpar[0][1][1][ 3][0] =   6.179365e-04; fpar[0][1][1][ 3][1] =   5.876401e-04; fpar[0][1][1][ 3][2] =  -3.726936e-04; 

  //pc3dphi_mean_c1d1z4
  sfname[0][1][1][4] = "m_gpc3dphi_mean_c1d1z4";
  sfunc[0][1][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][4] = 3;
  fpar[0][1][1][ 4][0] =   5.112285e-04; fpar[0][1][1][ 4][1] =   7.706180e-04; fpar[0][1][1][ 4][2] =  -5.361888e-04; 

  //pc3dphi_mean_c1d1z5
  sfname[0][1][1][5] = "m_gpc3dphi_mean_c1d1z5";
  sfunc[0][1][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][5] = 3;
  fpar[0][1][1][ 5][0] =   5.645942e-04; fpar[0][1][1][ 5][1] =  -1.626669e-04; fpar[0][1][1][ 5][2] =  -5.291120e-05; 

  //pc3dphi_mean_c1d1z6
  sfname[0][1][1][6] = "m_gpc3dphi_mean_c1d1z6";
  sfunc[0][1][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][6] = 3;
  fpar[0][1][1][ 6][0] =   4.996272e-04; fpar[0][1][1][ 6][1] =   2.171823e-04; fpar[0][1][1][ 6][2] =  -1.000834e-04; 

  //pc3dphi_mean_c1d1z7
  sfname[0][1][1][7] = "m_gpc3dphi_mean_c1d1z7";
  sfunc[0][1][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][7] = 3;
  fpar[0][1][1][ 7][0] =   6.347728e-04; fpar[0][1][1][ 7][1] =   5.990056e-05; fpar[0][1][1][ 7][2] =   2.021764e-04; 

  //pc3dphi_mean_c1d1z8
  sfname[0][1][1][8] = "m_gpc3dphi_mean_c1d1z8";
  sfunc[0][1][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][8] = 3;
  fpar[0][1][1][ 8][0] =   8.142115e-04; fpar[0][1][1][ 8][1] =  -2.784729e-04; fpar[0][1][1][ 8][2] =   6.693009e-04; 

  //pc3dphi_mean_c1d1z9
  sfname[0][1][1][9] = "m_gpc3dphi_mean_c1d1z9";
  sfunc[0][1][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][9] = 3;
  fpar[0][1][1][ 9][0] =   7.499006e-04; fpar[0][1][1][ 9][1] =   1.084694e-04; fpar[0][1][1][ 9][2] =   7.797760e-04; 

  //pc3dphi_sigma_c0d0z0
  sfname[1][0][0][0] = "m_gpc3dphi_sigma_c0d0z0";
  sfunc[1][0][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][0] = 3;
  fpar[1][0][0][ 0][0] =   1.471800e-03; fpar[1][0][0][ 0][1] =   9.903523e-04; fpar[1][0][0][ 0][2] =   1.065730e-04; 

  //pc3dphi_sigma_c0d0z1
  sfname[1][0][0][1] = "m_gpc3dphi_sigma_c0d0z1";
  sfunc[1][0][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][1] = 3;
  fpar[1][0][0][ 1][0] =   1.384782e-03; fpar[1][0][0][ 1][1] =   1.053726e-03; fpar[1][0][0][ 1][2] =   9.266901e-05; 

  //pc3dphi_sigma_c0d0z2
  sfname[1][0][0][2] = "m_gpc3dphi_sigma_c0d0z2";
  sfunc[1][0][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][2] = 3;
  fpar[1][0][0][ 2][0] =   1.415166e-03; fpar[1][0][0][ 2][1] =   9.774218e-04; fpar[1][0][0][ 2][2] =   3.671720e-05; 

  //pc3dphi_sigma_c0d0z3
  sfname[1][0][0][3] = "m_gpc3dphi_sigma_c0d0z3";
  sfunc[1][0][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][3] = 3;
  fpar[1][0][0][ 3][0] =   1.453795e-03; fpar[1][0][0][ 3][1] =   7.935798e-04; fpar[1][0][0][ 3][2] =   1.210140e-04; 

  //pc3dphi_sigma_c0d0z4
  sfname[1][0][0][4] = "m_gpc3dphi_sigma_c0d0z4";
  sfunc[1][0][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][4] = 3;
  fpar[1][0][0][ 4][0] =   1.524803e-03; fpar[1][0][0][ 4][1] =   5.711319e-04; fpar[1][0][0][ 4][2] =   1.936502e-04; 

  //pc3dphi_sigma_c0d0z5
  sfname[1][0][0][5] = "m_gpc3dphi_sigma_c0d0z5";
  sfunc[1][0][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][5] = 3;
  fpar[1][0][0][ 5][0] =   1.539630e-03; fpar[1][0][0][ 5][1] =   7.747740e-04; fpar[1][0][0][ 5][2] =   8.928578e-05; 

  //pc3dphi_sigma_c0d0z6
  sfname[1][0][0][6] = "m_gpc3dphi_sigma_c0d0z6";
  sfunc[1][0][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][6] = 3;
  fpar[1][0][0][ 6][0] =   1.505029e-03; fpar[1][0][0][ 6][1] =   7.784160e-04; fpar[1][0][0][ 6][2] =   1.110967e-04; 

  //pc3dphi_sigma_c0d0z7
  sfname[1][0][0][7] = "m_gpc3dphi_sigma_c0d0z7";
  sfunc[1][0][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][7] = 3;
  fpar[1][0][0][ 7][0] =   1.358988e-03; fpar[1][0][0][ 7][1] =   1.034053e-03; fpar[1][0][0][ 7][2] =   6.803088e-05; 

  //pc3dphi_sigma_c0d0z8
  sfname[1][0][0][8] = "m_gpc3dphi_sigma_c0d0z8";
  sfunc[1][0][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][8] = 3;
  fpar[1][0][0][ 8][0] =   1.480934e-03; fpar[1][0][0][ 8][1] =   8.116870e-04; fpar[1][0][0][ 8][2] =   8.622217e-05; 

  //pc3dphi_sigma_c0d0z9
  sfname[1][0][0][9] = "m_gpc3dphi_sigma_c0d0z9";
  sfunc[1][0][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][9] = 3;
  fpar[1][0][0][ 9][0] =   1.401917e-03; fpar[1][0][0][ 9][1] =   1.037277e-03; fpar[1][0][0][ 9][2] =  -9.076355e-05; 

  //pc3dphi_sigma_c0d1z0
  sfname[1][0][1][0] = "m_gpc3dphi_sigma_c0d1z0";
  sfunc[1][0][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][0] = 3;
  fpar[1][0][1][ 0][0] =   1.439547e-03; fpar[1][0][1][ 0][1] =  -1.327239e-04; fpar[1][0][1][ 0][2] =   6.065585e-04; 

  //pc3dphi_sigma_c0d1z1
  sfname[1][0][1][1] = "m_gpc3dphi_sigma_c0d1z1";
  sfunc[1][0][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][1] = 3;
  fpar[1][0][1][ 1][0] =   1.441178e-03; fpar[1][0][1][ 1][1] =  -1.131015e-04; fpar[1][0][1][ 1][2] =   5.785073e-04; 

  //pc3dphi_sigma_c0d1z2
  sfname[1][0][1][2] = "m_gpc3dphi_sigma_c0d1z2";
  sfunc[1][0][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][2] = 3;
  fpar[1][0][1][ 2][0] =   1.180706e-03; fpar[1][0][1][ 2][1] =   6.683673e-04; fpar[1][0][1][ 2][2] =   3.428986e-05; 

  //pc3dphi_sigma_c0d1z3
  sfname[1][0][1][3] = "m_gpc3dphi_sigma_c0d1z3";
  sfunc[1][0][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][3] = 3;
  fpar[1][0][1][ 3][0] =   1.252753e-03; fpar[1][0][1][ 3][1] =   5.237084e-04; fpar[1][0][1][ 3][2] =   1.916577e-04; 

  //pc3dphi_sigma_c0d1z4
  sfname[1][0][1][4] = "m_gpc3dphi_sigma_c0d1z4";
  sfunc[1][0][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][4] = 3;
  fpar[1][0][1][ 4][0] =   1.080258e-03; fpar[1][0][1][ 4][1] =   9.230270e-04; fpar[1][0][1][ 4][2] =  -1.627159e-06; 

  //pc3dphi_sigma_c0d1z5
  sfname[1][0][1][5] = "m_gpc3dphi_sigma_c0d1z5";
  sfunc[1][0][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][5] = 3;
  fpar[1][0][1][ 5][0] =   1.376309e-03; fpar[1][0][1][ 5][1] =   1.164213e-04; fpar[1][0][1][ 5][2] =   4.037298e-04; 

  //pc3dphi_sigma_c0d1z6
  sfname[1][0][1][6] = "m_gpc3dphi_sigma_c0d1z6";
  sfunc[1][0][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][6] = 3;
  fpar[1][0][1][ 6][0] =   1.368622e-03; fpar[1][0][1][ 6][1] =   1.938544e-04; fpar[1][0][1][ 6][2] =   3.609839e-04; 

  //pc3dphi_sigma_c0d1z7
  sfname[1][0][1][7] = "m_gpc3dphi_sigma_c0d1z7";
  sfunc[1][0][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][7] = 3;
  fpar[1][0][1][ 7][0] =   1.453501e-03; fpar[1][0][1][ 7][1] =  -1.190199e-04; fpar[1][0][1][ 7][2] =   5.888508e-04; 

  //pc3dphi_sigma_c0d1z8
  sfname[1][0][1][8] = "m_gpc3dphi_sigma_c0d1z8";
  sfunc[1][0][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][8] = 3;
  fpar[1][0][1][ 8][0] =   1.122799e-03; fpar[1][0][1][ 8][1] =   6.857702e-04; fpar[1][0][1][ 8][2] =   2.029837e-04; 

  //pc3dphi_sigma_c0d1z9
  sfname[1][0][1][9] = "m_gpc3dphi_sigma_c0d1z9";
  sfunc[1][0][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][9] = 3;
  fpar[1][0][1][ 9][0] =   1.276205e-03; fpar[1][0][1][ 9][1] =   3.412190e-04; fpar[1][0][1][ 9][2] =   5.130398e-04; 

  //pc3dphi_sigma_c1d0z0
  sfname[1][1][0][0] = "m_gpc3dphi_sigma_c1d0z0";
  sfunc[1][1][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][0] = 3;
  fpar[1][1][0][ 0][0] =   1.307396e-03; fpar[1][1][0][ 0][1] =   1.674680e-03; fpar[1][1][0][ 0][2] =  -4.945966e-04; 

  //pc3dphi_sigma_c1d0z1
  sfname[1][1][0][1] = "m_gpc3dphi_sigma_c1d0z1";
  sfunc[1][1][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][1] = 3;
  fpar[1][1][0][ 1][0] =   1.550714e-03; fpar[1][1][0][ 1][1] =   8.877351e-04; fpar[1][1][0][ 1][2] =   7.091126e-06; 

  //pc3dphi_sigma_c1d0z2
  sfname[1][1][0][2] = "m_gpc3dphi_sigma_c1d0z2";
  sfunc[1][1][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][2] = 3;
  fpar[1][1][0][ 2][0] =   1.551658e-03; fpar[1][1][0][ 2][1] =   9.000672e-04; fpar[1][1][0][ 2][2] =  -2.581160e-06; 

  //pc3dphi_sigma_c1d0z3
  sfname[1][1][0][3] = "m_gpc3dphi_sigma_c1d0z3";
  sfunc[1][1][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][3] = 3;
  fpar[1][1][0][ 3][0] =   1.459957e-03; fpar[1][1][0][ 3][1] =   8.539656e-04; fpar[1][1][0][ 3][2] =   1.869482e-04; 

  //pc3dphi_sigma_c1d0z4
  sfname[1][1][0][4] = "m_gpc3dphi_sigma_c1d0z4";
  sfunc[1][1][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][4] = 3;
  fpar[1][1][0][ 4][0] =   1.262354e-03; fpar[1][1][0][ 4][1] =   1.218504e-03; fpar[1][1][0][ 4][2] =  -5.032316e-05; 

  //pc3dphi_sigma_c1d0z5
  sfname[1][1][0][5] = "m_gpc3dphi_sigma_c1d0z5";
  sfunc[1][1][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][5] = 3;
  fpar[1][1][0][ 5][0] =   1.658882e-03; fpar[1][1][0][ 5][1] =   5.028239e-04; fpar[1][1][0][ 5][2] =   3.550643e-04; 

  //pc3dphi_sigma_c1d0z6
  sfname[1][1][0][6] = "m_gpc3dphi_sigma_c1d0z6";
  sfunc[1][1][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][6] = 3;
  fpar[1][1][0][ 6][0] =   1.312535e-03; fpar[1][1][0][ 6][1] =   1.280398e-03; fpar[1][1][0][ 6][2] =  -1.117553e-04; 

  //pc3dphi_sigma_c1d0z7
  sfname[1][1][0][7] = "m_gpc3dphi_sigma_c1d0z7";
  sfunc[1][1][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][7] = 3;
  fpar[1][1][0][ 7][0] =   1.347665e-03; fpar[1][1][0][ 7][1] =   1.182963e-03; fpar[1][1][0][ 7][2] =  -9.063655e-05; 

  //pc3dphi_sigma_c1d0z8
  sfname[1][1][0][8] = "m_gpc3dphi_sigma_c1d0z8";
  sfunc[1][1][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][8] = 3;
  fpar[1][1][0][ 8][0] =   1.261938e-03; fpar[1][1][0][ 8][1] =   1.312175e-03; fpar[1][1][0][ 8][2] =  -1.754911e-04; 

  //pc3dphi_sigma_c1d0z9
  sfname[1][1][0][9] = "m_gpc3dphi_sigma_c1d0z9";
  sfunc[1][1][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][9] = 3;
  fpar[1][1][0][ 9][0] =   1.402893e-03; fpar[1][1][0][ 9][1] =   9.622606e-04; fpar[1][1][0][ 9][2] =   7.306254e-06; 

  //pc3dphi_sigma_c1d1z0
  sfname[1][1][1][0] = "m_gpc3dphi_sigma_c1d1z0";
  sfunc[1][1][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][0] = 3;
  fpar[1][1][1][ 0][0] =   1.222240e-03; fpar[1][1][1][ 0][1] =   4.741286e-04; fpar[1][1][1][ 0][2] =   2.023840e-04; 

  //pc3dphi_sigma_c1d1z1
  sfname[1][1][1][1] = "m_gpc3dphi_sigma_c1d1z1";
  sfunc[1][1][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][1] = 3;
  fpar[1][1][1][ 1][0] =   1.384644e-03; fpar[1][1][1][ 1][1] =   8.532255e-05; fpar[1][1][1][ 1][2] =   3.663634e-04; 

  //pc3dphi_sigma_c1d1z2
  sfname[1][1][1][2] = "m_gpc3dphi_sigma_c1d1z2";
  sfunc[1][1][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][2] = 3;
  fpar[1][1][1][ 2][0] =   1.208498e-03; fpar[1][1][1][ 2][1] =   5.767023e-04; fpar[1][1][1][ 2][2] =   3.381307e-05; 

  //pc3dphi_sigma_c1d1z3
  sfname[1][1][1][3] = "m_gpc3dphi_sigma_c1d1z3";
  sfunc[1][1][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][3] = 3;
  fpar[1][1][1][ 3][0] =   1.549497e-03; fpar[1][1][1][ 3][1] =  -4.109877e-04; fpar[1][1][1][ 3][2] =   6.919928e-04; 

  //pc3dphi_sigma_c1d1z4
  sfname[1][1][1][4] = "m_gpc3dphi_sigma_c1d1z4";
  sfunc[1][1][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][4] = 3;
  fpar[1][1][1][ 4][0] =   1.494729e-03; fpar[1][1][1][ 4][1] =  -3.474702e-04; fpar[1][1][1][ 4][2] =   6.545877e-04; 

  //pc3dphi_sigma_c1d1z5
  sfname[1][1][1][5] = "m_gpc3dphi_sigma_c1d1z5";
  sfunc[1][1][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][5] = 3;
  fpar[1][1][1][ 5][0] =   1.153183e-03; fpar[1][1][1][ 5][1] =   7.433988e-04; fpar[1][1][1][ 5][2] =   1.253992e-04; 

  //pc3dphi_sigma_c1d1z6
  sfname[1][1][1][6] = "m_gpc3dphi_sigma_c1d1z6";
  sfunc[1][1][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][6] = 3;
  fpar[1][1][1][ 6][0] =   1.506644e-03; fpar[1][1][1][ 6][1] =  -1.194938e-04; fpar[1][1][1][ 6][2] =   5.434840e-04; 

  //pc3dphi_sigma_c1d1z7
  sfname[1][1][1][7] = "m_gpc3dphi_sigma_c1d1z7";
  sfunc[1][1][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][7] = 3;
  fpar[1][1][1][ 7][0] =   1.260208e-03; fpar[1][1][1][ 7][1] =   5.201368e-04; fpar[1][1][1][ 7][2] =   6.689513e-05; 

  //pc3dphi_sigma_c1d1z8
  sfname[1][1][1][8] = "m_gpc3dphi_sigma_c1d1z8";
  sfunc[1][1][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][8] = 3;
  fpar[1][1][1][ 8][0] =   1.434935e-03; fpar[1][1][1][ 8][1] =   3.879703e-05; fpar[1][1][1][ 8][2] =   3.828560e-04; 

  //pc3dphi_sigma_c1d1z9
  sfname[1][1][1][9] = "m_gpc3dphi_sigma_c1d1z9";
  sfunc[1][1][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][9] = 3;
  fpar[1][1][1][ 9][0] =   1.404340e-03; fpar[1][1][1][ 9][1] =   7.281420e-05; fpar[1][1][1][ 9][2] =   4.483375e-04; 

  //pc3dz_mean_c0d0z0
  sfname[2][0][0][0] = "m_gpc3dz_mean_c0d0z0";
  sfunc[2][0][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][0] = 3;
  fpar[2][0][0][ 0][0] =   1.536361e+00; fpar[2][0][0][ 0][1] =   1.261257e-02; fpar[2][0][0][ 0][2] =   1.977008e+00; 

  //pc3dz_mean_c0d0z1
  sfname[2][0][0][1] = "m_gpc3dz_mean_c0d0z1";
  sfunc[2][0][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][1] = 3;
  fpar[2][0][0][ 1][0] =   1.645834e+00; fpar[2][0][0][ 1][1] =  -4.243938e-02; fpar[2][0][0][ 1][2] =   1.519658e+00; 

  //pc3dz_mean_c0d0z2
  sfname[2][0][0][2] = "m_gpc3dz_mean_c0d0z2";
  sfunc[2][0][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][2] = 3;
  fpar[2][0][0][ 2][0] =   1.751066e+00; fpar[2][0][0][ 2][1] =  -3.391612e-01; fpar[2][0][0][ 2][2] =   1.337264e+00; 

  //pc3dz_mean_c0d0z3
  sfname[2][0][0][3] = "m_gpc3dz_mean_c0d0z3";
  sfunc[2][0][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][3] = 3;
  fpar[2][0][0][ 3][0] =   1.703539e+00; fpar[2][0][0][ 3][1] =  -9.859229e-02; fpar[2][0][0][ 3][2] =   6.047513e-01; 

  //pc3dz_mean_c0d0z4
  sfname[2][0][0][4] = "m_gpc3dz_mean_c0d0z4";
  sfunc[2][0][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][4] = 3;
  fpar[2][0][0][ 4][0] =   1.680668e+00; fpar[2][0][0][ 4][1] =  -2.857381e-02; fpar[2][0][0][ 4][2] =   1.681871e-01; 

  //pc3dz_mean_c0d0z5
  sfname[2][0][0][5] = "m_gpc3dz_mean_c0d0z5";
  sfunc[2][0][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][5] = 3;
  fpar[2][0][0][ 5][0] =   1.808558e+00; fpar[2][0][0][ 5][1] =   3.059792e-01; fpar[2][0][0][ 5][2] =  -4.759935e-01; 

  //pc3dz_mean_c0d0z6
  sfname[2][0][0][6] = "m_gpc3dz_mean_c0d0z6";
  sfunc[2][0][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][6] = 3;
  fpar[2][0][0][ 6][0] =   1.848408e+00; fpar[2][0][0][ 6][1] =   2.301580e-01; fpar[2][0][0][ 6][2] =  -7.385834e-01; 

  //pc3dz_mean_c0d0z7
  sfname[2][0][0][7] = "m_gpc3dz_mean_c0d0z7";
  sfunc[2][0][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][7] = 3;
  fpar[2][0][0][ 7][0] =   1.888906e+00; fpar[2][0][0][ 7][1] =   3.114355e-01; fpar[2][0][0][ 7][2] =  -1.230611e+00; 

  //pc3dz_mean_c0d0z8
  sfname[2][0][0][8] = "m_gpc3dz_mean_c0d0z8";
  sfunc[2][0][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][8] = 3;
  fpar[2][0][0][ 8][0] =   1.954718e+00; fpar[2][0][0][ 8][1] =   6.476031e-01; fpar[2][0][0][ 8][2] =  -2.132724e+00; 

  //pc3dz_mean_c0d0z9
  sfname[2][0][0][9] = "m_gpc3dz_mean_c0d0z9";
  sfunc[2][0][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][0][9] = 3;
  fpar[2][0][0][ 9][0] =   2.188100e+00; fpar[2][0][0][ 9][1] =   6.438890e-01; fpar[2][0][0][ 9][2] =  -2.787618e+00; 

  //pc3dz_mean_c0d1z0
  sfname[2][0][1][0] = "m_gpc3dz_mean_c0d1z0";
  sfunc[2][0][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][0] = 3;
  fpar[2][0][1][ 0][0] =  -3.019715e-01; fpar[2][0][1][ 0][1] =  -8.965993e-01; fpar[2][0][1][ 0][2] =   2.897781e+00; 

  //pc3dz_mean_c0d1z1
  sfname[2][0][1][1] = "m_gpc3dz_mean_c0d1z1";
  sfunc[2][0][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][1] = 3;
  fpar[2][0][1][ 1][0] =  -2.252512e-01; fpar[2][0][1][ 1][1] =  -1.999659e-01; fpar[2][0][1][ 1][2] =   1.572760e+00; 

  //pc3dz_mean_c0d1z2
  sfname[2][0][1][2] = "m_gpc3dz_mean_c0d1z2";
  sfunc[2][0][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][2] = 3;
  fpar[2][0][1][ 2][0] =  -1.761965e-02; fpar[2][0][1][ 2][1] =  -4.514022e-01; fpar[2][0][1][ 2][2] =   1.334506e+00; 

  //pc3dz_mean_c0d1z3
  sfname[2][0][1][3] = "m_gpc3dz_mean_c0d1z3";
  sfunc[2][0][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][3] = 3;
  fpar[2][0][1][ 3][0] =   1.750498e-01; fpar[2][0][1][ 3][1] =  -5.523535e-01; fpar[2][0][1][ 3][2] =   1.039120e+00; 

  //pc3dz_mean_c0d1z4
  sfname[2][0][1][4] = "m_gpc3dz_mean_c0d1z4";
  sfunc[2][0][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][4] = 3;
  fpar[2][0][1][ 4][0] =   1.551957e-01; fpar[2][0][1][ 4][1] =  -3.024098e-01; fpar[2][0][1][ 4][2] =   5.256183e-01; 

  //pc3dz_mean_c0d1z5
  sfname[2][0][1][5] = "m_gpc3dz_mean_c0d1z5";
  sfunc[2][0][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][5] = 3;
  fpar[2][0][1][ 5][0] =   4.203644e-01; fpar[2][0][1][ 5][1] =  -6.962889e-01; fpar[2][0][1][ 5][2] =   4.537193e-01; 

  //pc3dz_mean_c0d1z6
  sfname[2][0][1][6] = "m_gpc3dz_mean_c0d1z6";
  sfunc[2][0][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][6] = 3;
  fpar[2][0][1][ 6][0] =   4.575442e-01; fpar[2][0][1][ 6][1] =  -3.223584e-01; fpar[2][0][1][ 6][2] =  -1.972063e-01; 

  //pc3dz_mean_c0d1z7
  sfname[2][0][1][7] = "m_gpc3dz_mean_c0d1z7";
  sfunc[2][0][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][7] = 3;
  fpar[2][0][1][ 7][0] =   6.030482e-01; fpar[2][0][1][ 7][1] =  -3.247610e-01; fpar[2][0][1][ 7][2] =  -5.155489e-01; 

  //pc3dz_mean_c0d1z8
  sfname[2][0][1][8] = "m_gpc3dz_mean_c0d1z8";
  sfunc[2][0][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][8] = 3;
  fpar[2][0][1][ 8][0] =   8.085745e-01; fpar[2][0][1][ 8][1] =  -5.602463e-03; fpar[2][0][1][ 8][2] =  -1.152032e+00; 

  //pc3dz_mean_c0d1z9
  sfname[2][0][1][9] = "m_gpc3dz_mean_c0d1z9";
  sfunc[2][0][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][0][1][9] = 3;
  fpar[2][0][1][ 9][0] =   1.126036e+00; fpar[2][0][1][ 9][1] =   4.761246e-02; fpar[2][0][1][ 9][2] =  -1.739961e+00; 

  //pc3dz_mean_c1d0z0
  sfname[2][1][0][0] = "m_gpc3dz_mean_c1d0z0";
  sfunc[2][1][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][0] = 3;
  fpar[2][1][0][ 0][0] =   1.638661e+00; fpar[2][1][0][ 0][1] =  -3.549748e-01; fpar[2][1][0][ 0][2] =   2.076624e+00; 

  //pc3dz_mean_c1d0z1
  sfname[2][1][0][1] = "m_gpc3dz_mean_c1d0z1";
  sfunc[2][1][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][1] = 3;
  fpar[2][1][0][ 1][0] =   1.710103e+00; fpar[2][1][0][ 1][1] =  -2.563868e-01; fpar[2][1][0][ 1][2] =   1.497246e+00; 

  //pc3dz_mean_c1d0z2
  sfname[2][1][0][2] = "m_gpc3dz_mean_c1d0z2";
  sfunc[2][1][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][2] = 3;
  fpar[2][1][0][ 2][0] =   1.733495e+00; fpar[2][1][0][ 2][1] =  -1.049875e-01; fpar[2][1][0][ 2][2] =   8.813167e-01; 

  //pc3dz_mean_c1d0z3
  sfname[2][1][0][3] = "m_gpc3dz_mean_c1d0z3";
  sfunc[2][1][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][3] = 3;
  fpar[2][1][0][ 3][0] =   1.649684e+00; fpar[2][1][0][ 3][1] =   1.948644e-01; fpar[2][1][0][ 3][2] =   2.582858e-01; 

  //pc3dz_mean_c1d0z4
  sfname[2][1][0][4] = "m_gpc3dz_mean_c1d0z4";
  sfunc[2][1][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][4] = 3;
  fpar[2][1][0][ 4][0] =   1.644492e+00; fpar[2][1][0][ 4][1] =   2.548949e-01; fpar[2][1][0][ 4][2] =  -9.830683e-02; 

  //pc3dz_mean_c1d0z5
  sfname[2][1][0][5] = "m_gpc3dz_mean_c1d0z5";
  sfunc[2][1][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][5] = 3;
  fpar[2][1][0][ 5][0] =   1.969413e+00; fpar[2][1][0][ 5][1] =  -6.679407e-02; fpar[2][1][0][ 5][2] =  -1.467796e-01; 

  //pc3dz_mean_c1d0z6
  sfname[2][1][0][6] = "m_gpc3dz_mean_c1d0z6";
  sfunc[2][1][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][6] = 3;
  fpar[2][1][0][ 6][0] =   1.912291e+00; fpar[2][1][0][ 6][1] =   1.816012e-01; fpar[2][1][0][ 6][2] =  -6.191235e-01; 

  //pc3dz_mean_c1d0z7
  sfname[2][1][0][7] = "m_gpc3dz_mean_c1d0z7";
  sfunc[2][1][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][7] = 3;
  fpar[2][1][0][ 7][0] =   1.955806e+00; fpar[2][1][0][ 7][1] =   2.605005e-01; fpar[2][1][0][ 7][2] =  -1.053168e+00; 

  //pc3dz_mean_c1d0z8
  sfname[2][1][0][8] = "m_gpc3dz_mean_c1d0z8";
  sfunc[2][1][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][8] = 3;
  fpar[2][1][0][ 8][0] =   2.025085e+00; fpar[2][1][0][ 8][1] =   4.904340e-01; fpar[2][1][0][ 8][2] =  -1.701716e+00; 

  //pc3dz_mean_c1d0z9
  sfname[2][1][0][9] = "m_gpc3dz_mean_c1d0z9";
  sfunc[2][1][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][0][9] = 3;
  fpar[2][1][0][ 9][0] =   2.231917e+00; fpar[2][1][0][ 9][1] =   5.096523e-01; fpar[2][1][0][ 9][2] =  -2.282954e+00; 

  //pc3dz_mean_c1d1z0
  sfname[2][1][1][0] = "m_gpc3dz_mean_c1d1z0";
  sfunc[2][1][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][0] = 3;
  fpar[2][1][1][ 0][0] =  -4.618559e-01; fpar[2][1][1][ 0][1] =   3.611607e-01; fpar[2][1][1][ 0][2] =   1.555862e+00; 

  //pc3dz_mean_c1d1z1
  sfname[2][1][1][1] = "m_gpc3dz_mean_c1d1z1";
  sfunc[2][1][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][1] = 3;
  fpar[2][1][1][ 1][0] =  -2.114559e-01; fpar[2][1][1][ 1][1] =   9.743182e-02; fpar[2][1][1][ 1][2] =   1.296032e+00; 

  //pc3dz_mean_c1d1z2
  sfname[2][1][1][2] = "m_gpc3dz_mean_c1d1z2";
  sfunc[2][1][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][2] = 3;
  fpar[2][1][1][ 2][0] =  -5.116515e-02; fpar[2][1][1][ 2][1] =   1.975361e-03; fpar[2][1][1][ 2][2] =   9.018400e-01; 

  //pc3dz_mean_c1d1z3
  sfname[2][1][1][3] = "m_gpc3dz_mean_c1d1z3";
  sfunc[2][1][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][3] = 3;
  fpar[2][1][1][ 3][0] =   4.202968e-02; fpar[2][1][1][ 3][1] =   1.277278e-01; fpar[2][1][1][ 3][2] =   3.907085e-01; 

  //pc3dz_mean_c1d1z4
  sfname[2][1][1][4] = "m_gpc3dz_mean_c1d1z4";
  sfunc[2][1][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][4] = 3;
  fpar[2][1][1][ 4][0] =   1.237600e-01; fpar[2][1][1][ 4][1] =   9.402962e-02; fpar[2][1][1][ 4][2] =   5.308142e-02; 

  //pc3dz_mean_c1d1z5
  sfname[2][1][1][5] = "m_gpc3dz_mean_c1d1z5";
  sfunc[2][1][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][5] = 3;
  fpar[2][1][1][ 5][0] =   1.827191e-01; fpar[2][1][1][ 5][1] =   2.498495e-01; fpar[2][1][1][ 5][2] =  -4.182492e-01; 

  //pc3dz_mean_c1d1z6
  sfname[2][1][1][6] = "m_gpc3dz_mean_c1d1z6";
  sfunc[2][1][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][6] = 3;
  fpar[2][1][1][ 6][0] =   3.086501e-01; fpar[2][1][1][ 6][1] =   1.714979e-01; fpar[2][1][1][ 6][2] =  -6.706523e-01; 

  //pc3dz_mean_c1d1z7
  sfname[2][1][1][7] = "m_gpc3dz_mean_c1d1z7";
  sfunc[2][1][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][7] = 3;
  fpar[2][1][1][ 7][0] =   4.693748e-01; fpar[2][1][1][ 7][1] =   2.104393e-01; fpar[2][1][1][ 7][2] =  -1.120263e+00; 

  //pc3dz_mean_c1d1z8
  sfname[2][1][1][8] = "m_gpc3dz_mean_c1d1z8";
  sfunc[2][1][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][8] = 3;
  fpar[2][1][1][ 8][0] =   6.982841e-01; fpar[2][1][1][ 8][1] =   5.715164e-01; fpar[2][1][1][ 8][2] =  -2.084496e+00; 

  //pc3dz_mean_c1d1z9
  sfname[2][1][1][9] = "m_gpc3dz_mean_c1d1z9";
  sfunc[2][1][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[2][1][1][9] = 3;
  fpar[2][1][1][ 9][0] =   1.056130e+00; fpar[2][1][1][ 9][1] =   2.718997e-01; fpar[2][1][1][ 9][2] =  -2.359368e+00; 

  //pc3dz_sigma_c0d0z0
  sfname[3][0][0][0] = "m_gpc3dz_sigma_c0d0z0";
  sfunc[3][0][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][0] = 3;
  fpar[3][0][0][ 0][0] =   5.991291e-01; fpar[3][0][0][ 0][1] =   3.299604e-01; fpar[3][0][0][ 0][2] =   1.536016e-01; 

  //pc3dz_sigma_c0d0z1
  sfname[3][0][0][1] = "m_gpc3dz_sigma_c0d0z1";
  sfunc[3][0][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][1] = 3;
  fpar[3][0][0][ 1][0] =   5.615610e-01; fpar[3][0][0][ 1][1] =   4.189983e-01; fpar[3][0][0][ 1][2] =   9.490511e-02; 

  //pc3dz_sigma_c0d0z2
  sfname[3][0][0][2] = "m_gpc3dz_sigma_c0d0z2";
  sfunc[3][0][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][2] = 3;
  fpar[3][0][0][ 2][0] =   5.857181e-01; fpar[3][0][0][ 2][1] =   3.556411e-01; fpar[3][0][0][ 2][2] =   1.216528e-01; 

  //pc3dz_sigma_c0d0z3
  sfname[3][0][0][3] = "m_gpc3dz_sigma_c0d0z3";
  sfunc[3][0][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][3] = 3;
  fpar[3][0][0][ 3][0] =   5.848781e-01; fpar[3][0][0][ 3][1] =   3.387039e-01; fpar[3][0][0][ 3][2] =   1.389414e-01; 

  //pc3dz_sigma_c0d0z4
  sfname[3][0][0][4] = "m_gpc3dz_sigma_c0d0z4";
  sfunc[3][0][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][4] = 3;
  fpar[3][0][0][ 4][0] =   5.891063e-01; fpar[3][0][0][ 4][1] =   3.095033e-01; fpar[3][0][0][ 4][2] =   1.396267e-01; 

  //pc3dz_sigma_c0d0z5
  sfname[3][0][0][5] = "m_gpc3dz_sigma_c0d0z5";
  sfunc[3][0][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][5] = 3;
  fpar[3][0][0][ 5][0] =   5.884810e-01; fpar[3][0][0][ 5][1] =   3.974158e-01; fpar[3][0][0][ 5][2] =   9.984204e-02; 

  //pc3dz_sigma_c0d0z6
  sfname[3][0][0][6] = "m_gpc3dz_sigma_c0d0z6";
  sfunc[3][0][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][6] = 3;
  fpar[3][0][0][ 6][0] =   6.075620e-01; fpar[3][0][0][ 6][1] =   3.319659e-01; fpar[3][0][0][ 6][2] =   1.371732e-01; 

  //pc3dz_sigma_c0d0z7
  sfname[3][0][0][7] = "m_gpc3dz_sigma_c0d0z7";
  sfunc[3][0][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][7] = 3;
  fpar[3][0][0][ 7][0] =   6.091278e-01; fpar[3][0][0][ 7][1] =   3.073428e-01; fpar[3][0][0][ 7][2] =   1.501023e-01; 

  //pc3dz_sigma_c0d0z8
  sfname[3][0][0][8] = "m_gpc3dz_sigma_c0d0z8";
  sfunc[3][0][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][8] = 3;
  fpar[3][0][0][ 8][0] =   6.226662e-01; fpar[3][0][0][ 8][1] =   2.890627e-01; fpar[3][0][0][ 8][2] =   1.481485e-01; 

  //pc3dz_sigma_c0d0z9
  sfname[3][0][0][9] = "m_gpc3dz_sigma_c0d0z9";
  sfunc[3][0][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][0][9] = 3;
  fpar[3][0][0][ 9][0] =   6.373230e-01; fpar[3][0][0][ 9][1] =   2.511064e-01; fpar[3][0][0][ 9][2] =   1.733484e-01; 

  //pc3dz_sigma_c0d1z0
  sfname[3][0][1][0] = "m_gpc3dz_sigma_c0d1z0";
  sfunc[3][0][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][0] = 3;
  fpar[3][0][1][ 0][0] =   6.276338e-01; fpar[3][0][1][ 0][1] =   3.467915e-01; fpar[3][0][1][ 0][2] =   9.345572e-02; 

  //pc3dz_sigma_c0d1z1
  sfname[3][0][1][1] = "m_gpc3dz_sigma_c0d1z1";
  sfunc[3][0][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][1] = 3;
  fpar[3][0][1][ 1][0] =   6.015915e-01; fpar[3][0][1][ 1][1] =   3.826112e-01; fpar[3][0][1][ 1][2] =   7.230502e-02; 

  //pc3dz_sigma_c0d1z2
  sfname[3][0][1][2] = "m_gpc3dz_sigma_c0d1z2";
  sfunc[3][0][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][2] = 3;
  fpar[3][0][1][ 2][0] =   5.683396e-01; fpar[3][0][1][ 2][1] =   4.876677e-01; fpar[3][0][1][ 2][2] =   1.805741e-02; 

  //pc3dz_sigma_c0d1z3
  sfname[3][0][1][3] = "m_gpc3dz_sigma_c0d1z3";
  sfunc[3][0][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][3] = 3;
  fpar[3][0][1][ 3][0] =   6.260458e-01; fpar[3][0][1][ 3][1] =   3.758281e-01; fpar[3][0][1][ 3][2] =   8.015855e-02; 

  //pc3dz_sigma_c0d1z4
  sfname[3][0][1][4] = "m_gpc3dz_sigma_c0d1z4";
  sfunc[3][0][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][4] = 3;
  fpar[3][0][1][ 4][0] =   6.594245e-01; fpar[3][0][1][ 4][1] =   2.694130e-01; fpar[3][0][1][ 4][2] =   1.230788e-01; 

  //pc3dz_sigma_c0d1z5
  sfname[3][0][1][5] = "m_gpc3dz_sigma_c0d1z5";
  sfunc[3][0][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][5] = 3;
  fpar[3][0][1][ 5][0] =   6.397162e-01; fpar[3][0][1][ 5][1] =   3.199756e-01; fpar[3][0][1][ 5][2] =   1.019065e-01; 

  //pc3dz_sigma_c0d1z6
  sfname[3][0][1][6] = "m_gpc3dz_sigma_c0d1z6";
  sfunc[3][0][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][6] = 3;
  fpar[3][0][1][ 6][0] =   6.598385e-01; fpar[3][0][1][ 6][1] =   3.394885e-01; fpar[3][0][1][ 6][2] =   8.841365e-02; 

  //pc3dz_sigma_c0d1z7
  sfname[3][0][1][7] = "m_gpc3dz_sigma_c0d1z7";
  sfunc[3][0][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][7] = 3;
  fpar[3][0][1][ 7][0] =   6.453840e-01; fpar[3][0][1][ 7][1] =   3.973683e-01; fpar[3][0][1][ 7][2] =   5.694648e-02; 

  //pc3dz_sigma_c0d1z8
  sfname[3][0][1][8] = "m_gpc3dz_sigma_c0d1z8";
  sfunc[3][0][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][8] = 3;
  fpar[3][0][1][ 8][0] =   7.439388e-01; fpar[3][0][1][ 8][1] =   2.675922e-01; fpar[3][0][1][ 8][2] =   9.552336e-02; 

  //pc3dz_sigma_c0d1z9
  sfname[3][0][1][9] = "m_gpc3dz_sigma_c0d1z9";
  sfunc[3][0][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][0][1][9] = 3;
  fpar[3][0][1][ 9][0] =   7.543745e-01; fpar[3][0][1][ 9][1] =   2.547033e-01; fpar[3][0][1][ 9][2] =   1.220354e-01; 

  //pc3dz_sigma_c1d0z0
  sfname[3][1][0][0] = "m_gpc3dz_sigma_c1d0z0";
  sfunc[3][1][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][0] = 3;
  fpar[3][1][0][ 0][0] =   5.785750e-01; fpar[3][1][0][ 0][1] =   3.381118e-01; fpar[3][1][0][ 0][2] =   1.304461e-01; 

  //pc3dz_sigma_c1d0z1
  sfname[3][1][0][1] = "m_gpc3dz_sigma_c1d0z1";
  sfunc[3][1][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][1] = 3;
  fpar[3][1][0][ 1][0] =   5.905150e-01; fpar[3][1][0][ 1][1] =   3.129675e-01; fpar[3][1][0][ 1][2] =   1.308840e-01; 

  //pc3dz_sigma_c1d0z2
  sfname[3][1][0][2] = "m_gpc3dz_sigma_c1d0z2";
  sfunc[3][1][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][2] = 3;
  fpar[3][1][0][ 2][0] =   5.915497e-01; fpar[3][1][0][ 2][1] =   3.532932e-01; fpar[3][1][0][ 2][2] =   1.027222e-01; 

  //pc3dz_sigma_c1d0z3
  sfname[3][1][0][3] = "m_gpc3dz_sigma_c1d0z3";
  sfunc[3][1][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][3] = 3;
  fpar[3][1][0][ 3][0] =   5.979479e-01; fpar[3][1][0][ 3][1] =   3.064933e-01; fpar[3][1][0][ 3][2] =   1.438639e-01; 

  //pc3dz_sigma_c1d0z4
  sfname[3][1][0][4] = "m_gpc3dz_sigma_c1d0z4";
  sfunc[3][1][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][4] = 3;
  fpar[3][1][0][ 4][0] =   5.929770e-01; fpar[3][1][0][ 4][1] =   2.963953e-01; fpar[3][1][0][ 4][2] =   1.356565e-01; 

  //pc3dz_sigma_c1d0z5
  sfname[3][1][0][5] = "m_gpc3dz_sigma_c1d0z5";
  sfunc[3][1][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][5] = 3;
  fpar[3][1][0][ 5][0] =   6.288523e-01; fpar[3][1][0][ 5][1] =   3.210830e-01; fpar[3][1][0][ 5][2] =   1.070073e-01; 

  //pc3dz_sigma_c1d0z6
  sfname[3][1][0][6] = "m_gpc3dz_sigma_c1d0z6";
  sfunc[3][1][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][6] = 3;
  fpar[3][1][0][ 6][0] =   6.234148e-01; fpar[3][1][0][ 6][1] =   3.237623e-01; fpar[3][1][0][ 6][2] =   1.227255e-01; 

  //pc3dz_sigma_c1d0z7
  sfname[3][1][0][7] = "m_gpc3dz_sigma_c1d0z7";
  sfunc[3][1][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][7] = 3;
  fpar[3][1][0][ 7][0] =   6.119233e-01; fpar[3][1][0][ 7][1] =   3.216337e-01; fpar[3][1][0][ 7][2] =   1.247687e-01; 

  //pc3dz_sigma_c1d0z8
  sfname[3][1][0][8] = "m_gpc3dz_sigma_c1d0z8";
  sfunc[3][1][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][8] = 3;
  fpar[3][1][0][ 8][0] =   6.216128e-01; fpar[3][1][0][ 8][1] =   3.289080e-01; fpar[3][1][0][ 8][2] =   1.124327e-01; 

  //pc3dz_sigma_c1d0z9
  sfname[3][1][0][9] = "m_gpc3dz_sigma_c1d0z9";
  sfunc[3][1][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][0][9] = 3;
  fpar[3][1][0][ 9][0] =   6.565275e-01; fpar[3][1][0][ 9][1] =   2.633906e-01; fpar[3][1][0][ 9][2] =   1.555070e-01; 

  //pc3dz_sigma_c1d1z0
  sfname[3][1][1][0] = "m_gpc3dz_sigma_c1d1z0";
  sfunc[3][1][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][0] = 3;
  fpar[3][1][1][ 0][0] =   5.732112e-01; fpar[3][1][1][ 0][1] =   4.159643e-01; fpar[3][1][1][ 0][2] =   6.304146e-02; 

  //pc3dz_sigma_c1d1z1
  sfname[3][1][1][1] = "m_gpc3dz_sigma_c1d1z1";
  sfunc[3][1][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][1] = 3;
  fpar[3][1][1][ 1][0] =   5.662897e-01; fpar[3][1][1][ 1][1] =   4.163083e-01; fpar[3][1][1][ 1][2] =   6.110459e-02; 

  //pc3dz_sigma_c1d1z2
  sfname[3][1][1][2] = "m_gpc3dz_sigma_c1d1z2";
  sfunc[3][1][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][2] = 3;
  fpar[3][1][1][ 2][0] =   5.739545e-01; fpar[3][1][1][ 2][1] =   4.314354e-01; fpar[3][1][1][ 2][2] =   4.784995e-02; 

  //pc3dz_sigma_c1d1z3
  sfname[3][1][1][3] = "m_gpc3dz_sigma_c1d1z3";
  sfunc[3][1][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][3] = 3;
  fpar[3][1][1][ 3][0] =   6.372626e-01; fpar[3][1][1][ 3][1] =   2.797766e-01; fpar[3][1][1][ 3][2] =   1.321696e-01; 

  //pc3dz_sigma_c1d1z4
  sfname[3][1][1][4] = "m_gpc3dz_sigma_c1d1z4";
  sfunc[3][1][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][4] = 3;
  fpar[3][1][1][ 4][0] =   6.293427e-01; fpar[3][1][1][ 4][1] =   3.170332e-01; fpar[3][1][1][ 4][2] =   1.023539e-01; 

  //pc3dz_sigma_c1d1z5
  sfname[3][1][1][5] = "m_gpc3dz_sigma_c1d1z5";
  sfunc[3][1][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][5] = 3;
  fpar[3][1][1][ 5][0] =   5.972050e-01; fpar[3][1][1][ 5][1] =   3.572536e-01; fpar[3][1][1][ 5][2] =   7.464595e-02; 

  //pc3dz_sigma_c1d1z6
  sfname[3][1][1][6] = "m_gpc3dz_sigma_c1d1z6";
  sfunc[3][1][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][6] = 3;
  fpar[3][1][1][ 6][0] =   6.068336e-01; fpar[3][1][1][ 6][1] =   3.992181e-01; fpar[3][1][1][ 6][2] =   6.435898e-02; 

  //pc3dz_sigma_c1d1z7
  sfname[3][1][1][7] = "m_gpc3dz_sigma_c1d1z7";
  sfunc[3][1][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][7] = 3;
  fpar[3][1][1][ 7][0] =   6.617792e-01; fpar[3][1][1][ 7][1] =   2.509375e-01; fpar[3][1][1][ 7][2] =   1.244324e-01; 

  //pc3dz_sigma_c1d1z8
  sfname[3][1][1][8] = "m_gpc3dz_sigma_c1d1z8";
  sfunc[3][1][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][8] = 3;
  fpar[3][1][1][ 8][0] =   7.236583e-01; fpar[3][1][1][ 8][1] =   1.866250e-01; fpar[3][1][1][ 8][2] =   1.428800e-01; 

  //pc3dz_sigma_c1d1z9
  sfname[3][1][1][9] = "m_gpc3dz_sigma_c1d1z9";
  sfunc[3][1][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[3][1][1][9] = 3;
  fpar[3][1][1][ 9][0] =   6.584798e-01; fpar[3][1][1][ 9][1] =   3.232682e-01; fpar[3][1][1][ 9][2] =   9.001396e-02; 

  ////////////////////////////////
  for(int ipar=0; ipar<4; ipar++){
    for(int ic=0; ic<2; ic++){
      for(int iarm=0; iarm<2; iarm++){
        for(int ized=0; ized<10; ized++){
          TF1* f1 = new TF1(sfname[ipar][ic][iarm][ized].Data(), sfunc[ipar][ic][iarm][ized].Data(), nfpar[ipar][ic][iarm][ized]);
          for(int i=0; i<nfpar[ipar][ic][iarm][ized]; i++) f1->SetParameter(i, fpar[ipar][ic][iarm][ized][i]);

          if(ipar==0)      m_fn_dphi_mean[ic][iarm][ized]  = f1;
          else if(ipar==1) m_fn_dphi_sigma[ic][iarm][ized] = f1;
          else if(ipar==2) m_fn_dz_mean[ic][iarm][ized]    = f1;
          else             m_fn_dz_sigma[ic][iarm][ized]   = f1;

        }
      }
    }
  }

}

void Run14AuAu200PC3MatchRecal::initParMatchGraph()
{
  double n_val[4][2][2][10];
  double val[  4][2][2][10][30];
  double eval[ 4][2][2][10][30];
  double pt  [30];
  double zero[30] = {0.0};

  pt[ 0] =   1.250000e-01; 
  pt[ 1] =   3.750000e-01; 
  pt[ 2] =   6.250000e-01; 
  pt[ 3] =   8.750000e-01; 
  pt[ 4] =   1.125000e+00; 
  pt[ 5] =   1.375000e+00; 
  pt[ 6] =   1.625000e+00; 
  pt[ 7] =   1.875000e+00; 
  pt[ 8] =   2.125000e+00; 
  pt[ 9] =   2.375000e+00; 
  pt[10] =   2.625000e+00; 
  pt[11] =   2.875000e+00; 
  pt[12] =   3.125000e+00; 
  pt[13] =   3.375000e+00; 
  pt[14] =   3.625000e+00; 
  pt[15] =   3.875000e+00; 
  pt[16] =   4.125000e+00; 
  pt[17] =   4.375000e+00; 
  pt[18] =   4.625000e+00; 
  pt[19] =   4.875000e+00; 
  pt[20] =   5.250000e+00; 
  pt[21] =   5.750000e+00; 
  pt[22] =   6.250000e+00; 
  pt[23] =   6.750000e+00; 
  pt[24] =   7.250000e+00; 
  pt[25] =   7.750000e+00; 
  pt[26] =   8.250000e+00; 
  pt[27] =   8.750000e+00; 
  pt[28] =   9.250000e+00; 
  pt[29] =   9.750000e+00; 
  //pc3dphi_mean_c0d0z0
  n_val[0][0][0][0] = 30;
  val[0][0][0][ 0][ 0] =   0.000000e+00; eval[0][0][0][ 0][ 0] =   0.000000e+00;
  val[0][0][0][ 0][ 1] =   0.000000e+00; eval[0][0][0][ 0][ 1] =   0.000000e+00;
  val[0][0][0][ 0][ 2] =  -2.560813e-03; eval[0][0][0][ 0][ 2] =   1.243308e-06;
  val[0][0][0][ 0][ 3] =  -1.239980e-03; eval[0][0][0][ 0][ 3] =   1.059731e-06;
  val[0][0][0][ 0][ 4] =  -7.057152e-04; eval[0][0][0][ 0][ 4] =   1.698644e-06;
  val[0][0][0][ 0][ 5] =  -7.984177e-04; eval[0][0][0][ 0][ 5] =   8.802144e-07;
  val[0][0][0][ 0][ 6] =  -6.330277e-04; eval[0][0][0][ 0][ 6] =   1.291393e-06;
  val[0][0][0][ 0][ 7] =  -5.332962e-04; eval[0][0][0][ 0][ 7] =   1.509379e-06;
  val[0][0][0][ 0][ 8] =  -4.409471e-04; eval[0][0][0][ 0][ 8] =   1.866557e-06;
  val[0][0][0][ 0][ 9] =  -3.890234e-04; eval[0][0][0][ 0][ 9] =   2.425545e-06;
  val[0][0][0][ 0][10] =  -3.484069e-04; eval[0][0][0][ 0][10] =   3.133408e-06;
  val[0][0][0][ 0][11] =  -3.234401e-04; eval[0][0][0][ 0][11] =   4.061483e-06;
  val[0][0][0][ 0][12] =  -2.819521e-04; eval[0][0][0][ 0][12] =   5.243363e-06;
  val[0][0][0][ 0][13] =  -2.766140e-04; eval[0][0][0][ 0][13] =   6.767248e-06;
  val[0][0][0][ 0][14] =  -2.222117e-04; eval[0][0][0][ 0][14] =   8.574400e-06;
  val[0][0][0][ 0][15] =  -2.004290e-04; eval[0][0][0][ 0][15] =   1.097813e-05;
  val[0][0][0][ 0][16] =  -2.232023e-04; eval[0][0][0][ 0][16] =   1.334772e-05;
  val[0][0][0][ 0][17] =  -1.395978e-04; eval[0][0][0][ 0][17] =   1.622653e-05;
  val[0][0][0][ 0][18] =  -1.314697e-04; eval[0][0][0][ 0][18] =   2.060949e-05;
  val[0][0][0][ 0][19] =  -1.557981e-04; eval[0][0][0][ 0][19] =   2.445915e-05;
  val[0][0][0][ 0][20] =  -1.703148e-04; eval[0][0][0][ 0][20] =   2.214529e-05;
  val[0][0][0][ 0][21] =  -1.378366e-04; eval[0][0][0][ 0][21] =   3.203594e-05;
  val[0][0][0][ 0][22] =   4.058731e-05; eval[0][0][0][ 0][22] =   4.366076e-05;
  val[0][0][0][ 0][23] =  -1.366748e-04; eval[0][0][0][ 0][23] =   5.109613e-05;
  val[0][0][0][ 0][24] =  -1.895152e-04; eval[0][0][0][ 0][24] =   6.753860e-05;
  val[0][0][0][ 0][25] =  -6.422951e-05; eval[0][0][0][ 0][25] =   9.560672e-05;
  val[0][0][0][ 0][26] =  -8.995037e-05; eval[0][0][0][ 0][26] =   1.106975e-04;
  val[0][0][0][ 0][27] =  -1.285588e-04; eval[0][0][0][ 0][27] =   1.272359e-04;
  val[0][0][0][ 0][28] =   6.931292e-05; eval[0][0][0][ 0][28] =   1.597097e-04;
  val[0][0][0][ 0][29] =   4.455375e-05; eval[0][0][0][ 0][29] =   2.204285e-04;
  //pc3dphi_mean_c0d0z1
  n_val[0][0][0][1] = 30;
  val[0][0][0][ 1][ 0] =   0.000000e+00; eval[0][0][0][ 1][ 0] =   0.000000e+00;
  val[0][0][0][ 1][ 1] =   0.000000e+00; eval[0][0][0][ 1][ 1] =   0.000000e+00;
  val[0][0][0][ 1][ 2] =  -1.524710e-03; eval[0][0][0][ 1][ 2] =   1.266996e-06;
  val[0][0][0][ 1][ 3] =  -8.449501e-04; eval[0][0][0][ 1][ 3] =   1.564904e-06;
  val[0][0][0][ 1][ 4] =  -5.536895e-04; eval[0][0][0][ 1][ 4] =   1.706604e-06;
  val[0][0][0][ 1][ 5] =  -7.611555e-04; eval[0][0][0][ 1][ 5] =   8.710522e-07;
  val[0][0][0][ 1][ 6] =  -6.736374e-04; eval[0][0][0][ 1][ 6] =   1.247145e-06;
  val[0][0][0][ 1][ 7] =  -6.021083e-04; eval[0][0][0][ 1][ 7] =   1.481034e-06;
  val[0][0][0][ 1][ 8] =  -5.156063e-04; eval[0][0][0][ 1][ 8] =   1.820373e-06;
  val[0][0][0][ 1][ 9] =  -4.608162e-04; eval[0][0][0][ 1][ 9] =   2.337598e-06;
  val[0][0][0][ 1][10] =  -4.272577e-04; eval[0][0][0][ 1][10] =   3.033989e-06;
  val[0][0][0][ 1][11] =  -4.082506e-04; eval[0][0][0][ 1][11] =   3.906587e-06;
  val[0][0][0][ 1][12] =  -3.904140e-04; eval[0][0][0][ 1][12] =   5.165296e-06;
  val[0][0][0][ 1][13] =  -3.792457e-04; eval[0][0][0][ 1][13] =   6.487427e-06;
  val[0][0][0][ 1][14] =  -3.392771e-04; eval[0][0][0][ 1][14] =   8.460889e-06;
  val[0][0][0][ 1][15] =  -3.504634e-04; eval[0][0][0][ 1][15] =   1.055230e-05;
  val[0][0][0][ 1][16] =  -3.594956e-04; eval[0][0][0][ 1][16] =   1.323879e-05;
  val[0][0][0][ 1][17] =  -3.137571e-04; eval[0][0][0][ 1][17] =   1.685013e-05;
  val[0][0][0][ 1][18] =  -3.345468e-04; eval[0][0][0][ 1][18] =   2.050487e-05;
  val[0][0][0][ 1][19] =  -3.513952e-04; eval[0][0][0][ 1][19] =   2.412515e-05;
  val[0][0][0][ 1][20] =  -3.011161e-04; eval[0][0][0][ 1][20] =   2.315644e-05;
  val[0][0][0][ 1][21] =  -2.783896e-04; eval[0][0][0][ 1][21] =   3.262512e-05;
  val[0][0][0][ 1][22] =  -3.522108e-04; eval[0][0][0][ 1][22] =   4.285114e-05;
  val[0][0][0][ 1][23] =  -2.635905e-04; eval[0][0][0][ 1][23] =   5.344551e-05;
  val[0][0][0][ 1][24] =  -2.983863e-04; eval[0][0][0][ 1][24] =   7.314429e-05;
  val[0][0][0][ 1][25] =  -4.640694e-04; eval[0][0][0][ 1][25] =   8.460626e-05;
  val[0][0][0][ 1][26] =  -4.179584e-04; eval[0][0][0][ 1][26] =   1.097704e-04;
  val[0][0][0][ 1][27] =  -3.990330e-04; eval[0][0][0][ 1][27] =   1.411307e-04;
  val[0][0][0][ 1][28] =  -3.545887e-04; eval[0][0][0][ 1][28] =   1.745729e-04;
  val[0][0][0][ 1][29] =  -3.111504e-04; eval[0][0][0][ 1][29] =   2.137671e-04;
  //pc3dphi_mean_c0d0z2
  n_val[0][0][0][2] = 30;
  val[0][0][0][ 2][ 0] =   0.000000e+00; eval[0][0][0][ 2][ 0] =   0.000000e+00;
  val[0][0][0][ 2][ 1] =   0.000000e+00; eval[0][0][0][ 2][ 1] =   0.000000e+00;
  val[0][0][0][ 2][ 2] =  -9.658944e-04; eval[0][0][0][ 2][ 2] =   8.177506e-07;
  val[0][0][0][ 2][ 3] =  -7.278355e-04; eval[0][0][0][ 2][ 3] =   9.170314e-07;
  val[0][0][0][ 2][ 4] =  -4.418989e-04; eval[0][0][0][ 2][ 4] =   1.820116e-06;
  val[0][0][0][ 2][ 5] =  -6.907000e-04; eval[0][0][0][ 2][ 5] =   8.658394e-07;
  val[0][0][0][ 2][ 6] =  -6.674200e-04; eval[0][0][0][ 2][ 6] =   1.241057e-06;
  val[0][0][0][ 2][ 7] =  -6.147786e-04; eval[0][0][0][ 2][ 7] =   1.482043e-06;
  val[0][0][0][ 2][ 8] =  -5.470227e-04; eval[0][0][0][ 2][ 8] =   1.821723e-06;
  val[0][0][0][ 2][ 9] =  -4.998579e-04; eval[0][0][0][ 2][ 9] =   2.329385e-06;
  val[0][0][0][ 2][10] =  -4.809533e-04; eval[0][0][0][ 2][10] =   2.981946e-06;
  val[0][0][0][ 2][11] =  -4.622409e-04; eval[0][0][0][ 2][11] =   3.817675e-06;
  val[0][0][0][ 2][12] =  -4.588228e-04; eval[0][0][0][ 2][12] =   5.023565e-06;
  val[0][0][0][ 2][13] =  -4.453601e-04; eval[0][0][0][ 2][13] =   6.387578e-06;
  val[0][0][0][ 2][14] =  -4.087354e-04; eval[0][0][0][ 2][14] =   8.334788e-06;
  val[0][0][0][ 2][15] =  -4.447155e-04; eval[0][0][0][ 2][15] =   1.030392e-05;
  val[0][0][0][ 2][16] =  -4.224730e-04; eval[0][0][0][ 2][16] =   1.281171e-05;
  val[0][0][0][ 2][17] =  -4.167866e-04; eval[0][0][0][ 2][17] =   1.654544e-05;
  val[0][0][0][ 2][18] =  -4.339548e-04; eval[0][0][0][ 2][18] =   1.984670e-05;
  val[0][0][0][ 2][19] =  -4.307825e-04; eval[0][0][0][ 2][19] =   2.347555e-05;
  val[0][0][0][ 2][20] =  -4.276603e-04; eval[0][0][0][ 2][20] =   2.201037e-05;
  val[0][0][0][ 2][21] =  -4.033485e-04; eval[0][0][0][ 2][21] =   3.133968e-05;
  val[0][0][0][ 2][22] =  -4.522611e-04; eval[0][0][0][ 2][22] =   4.300935e-05;
  val[0][0][0][ 2][23] =  -4.236517e-04; eval[0][0][0][ 2][23] =   5.316629e-05;
  val[0][0][0][ 2][24] =  -4.052233e-04; eval[0][0][0][ 2][24] =   7.396672e-05;
  val[0][0][0][ 2][25] =  -4.380959e-04; eval[0][0][0][ 2][25] =   8.434359e-05;
  val[0][0][0][ 2][26] =  -5.698347e-04; eval[0][0][0][ 2][26] =   1.022490e-04;
  val[0][0][0][ 2][27] =  -4.042017e-04; eval[0][0][0][ 2][27] =   1.371964e-04;
  val[0][0][0][ 2][28] =  -4.725835e-04; eval[0][0][0][ 2][28] =   1.731573e-04;
  val[0][0][0][ 2][29] =  -4.954597e-04; eval[0][0][0][ 2][29] =   2.003675e-04;
  //pc3dphi_mean_c0d0z3
  n_val[0][0][0][3] = 30;
  val[0][0][0][ 3][ 0] =   0.000000e+00; eval[0][0][0][ 3][ 0] =   0.000000e+00;
  val[0][0][0][ 3][ 1] =   0.000000e+00; eval[0][0][0][ 3][ 1] =   0.000000e+00;
  val[0][0][0][ 3][ 2] =  -5.993344e-04; eval[0][0][0][ 3][ 2] =   1.188900e-06;
  val[0][0][0][ 3][ 3] =  -5.945046e-04; eval[0][0][0][ 3][ 3] =   8.435999e-07;
  val[0][0][0][ 3][ 4] =  -4.760236e-04; eval[0][0][0][ 3][ 4] =   1.575826e-06;
  val[0][0][0][ 3][ 5] =  -6.876878e-04; eval[0][0][0][ 3][ 5] =   1.003801e-06;
  val[0][0][0][ 3][ 6] =  -6.820737e-04; eval[0][0][0][ 3][ 6] =   1.144120e-06;
  val[0][0][0][ 3][ 7] =  -6.739225e-04; eval[0][0][0][ 3][ 7] =   1.399044e-06;
  val[0][0][0][ 3][ 8] =  -6.272797e-04; eval[0][0][0][ 3][ 8] =   1.760902e-06;
  val[0][0][0][ 3][ 9] =  -6.007839e-04; eval[0][0][0][ 3][ 9] =   2.266471e-06;
  val[0][0][0][ 3][10] =  -5.892662e-04; eval[0][0][0][ 3][10] =   2.942800e-06;
  val[0][0][0][ 3][11] =  -4.935745e-04; eval[0][0][0][ 3][11] =   5.617844e-06;
  val[0][0][0][ 3][12] =  -4.751270e-04; eval[0][0][0][ 3][12] =   7.487512e-06;
  val[0][0][0][ 3][13] =  -4.612060e-04; eval[0][0][0][ 3][13] =   9.323256e-06;
  val[0][0][0][ 3][14] =  -4.424865e-04; eval[0][0][0][ 3][14] =   1.233148e-05;
  val[0][0][0][ 3][15] =  -5.553831e-04; eval[0][0][0][ 3][15] =   9.886938e-06;
  val[0][0][0][ 3][16] =  -4.336161e-04; eval[0][0][0][ 3][16] =   1.937863e-05;
  val[0][0][0][ 3][17] =  -3.800874e-04; eval[0][0][0][ 3][17] =   2.469855e-05;
  val[0][0][0][ 3][18] =  -4.900929e-04; eval[0][0][0][ 3][18] =   2.665747e-05;
  val[0][0][0][ 3][19] =  -5.383553e-04; eval[0][0][0][ 3][19] =   2.269307e-05;
  val[0][0][0][ 3][20] =  -5.143107e-04; eval[0][0][0][ 3][20] =   2.146011e-05;
  val[0][0][0][ 3][21] =  -4.058492e-04; eval[0][0][0][ 3][21] =   4.427998e-05;
  val[0][0][0][ 3][22] =  -4.941015e-04; eval[0][0][0][ 3][22] =   4.000241e-05;
  val[0][0][0][ 3][23] =  -5.366431e-04; eval[0][0][0][ 3][23] =   4.983315e-05;
  val[0][0][0][ 3][24] =  -3.764812e-04; eval[0][0][0][ 3][24] =   6.740291e-05;
  val[0][0][0][ 3][25] =  -4.649504e-04; eval[0][0][0][ 3][25] =   8.346029e-05;
  val[0][0][0][ 3][26] =  -5.510847e-04; eval[0][0][0][ 3][26] =   9.712121e-05;
  val[0][0][0][ 3][27] =  -3.589815e-04; eval[0][0][0][ 3][27] =   1.164702e-04;
  val[0][0][0][ 3][28] =  -3.936200e-04; eval[0][0][0][ 3][28] =   1.517740e-04;
  val[0][0][0][ 3][29] =  -3.090366e-04; eval[0][0][0][ 3][29] =   1.842422e-04;
  //pc3dphi_mean_c0d0z4
  n_val[0][0][0][4] = 30;
  val[0][0][0][ 4][ 0] =   0.000000e+00; eval[0][0][0][ 4][ 0] =   0.000000e+00;
  val[0][0][0][ 4][ 1] =   0.000000e+00; eval[0][0][0][ 4][ 1] =   0.000000e+00;
  val[0][0][0][ 4][ 2] =  -3.372227e-04; eval[0][0][0][ 4][ 2] =   1.226893e-06;
  val[0][0][0][ 4][ 3] =  -4.735365e-04; eval[0][0][0][ 4][ 3] =   9.089223e-07;
  val[0][0][0][ 4][ 4] =  -3.589721e-04; eval[0][0][0][ 4][ 4] =   1.872926e-06;
  val[0][0][0][ 4][ 5] =  -6.294214e-04; eval[0][0][0][ 4][ 5] =   1.100532e-06;
  val[0][0][0][ 4][ 6] =  -6.563592e-04; eval[0][0][0][ 4][ 6] =   1.272808e-06;
  val[0][0][0][ 4][ 7] =  -6.038931e-04; eval[0][0][0][ 4][ 7] =   2.156386e-06;
  val[0][0][0][ 4][ 8] =  -6.348985e-04; eval[0][0][0][ 4][ 8] =   2.018395e-06;
  val[0][0][0][ 4][ 9] =  -5.683149e-04; eval[0][0][0][ 4][ 9] =   3.614683e-06;
  val[0][0][0][ 4][10] =  -5.664698e-04; eval[0][0][0][ 4][10] =   4.684910e-06;
  val[0][0][0][ 4][11] =  -6.251555e-04; eval[0][0][0][ 4][11] =   4.384818e-06;
  val[0][0][0][ 4][12] =  -6.300281e-04; eval[0][0][0][ 4][12] =   5.731765e-06;
  val[0][0][0][ 4][13] =  -6.068974e-04; eval[0][0][0][ 4][13] =   7.329535e-06;
  val[0][0][0][ 4][14] =  -5.910490e-04; eval[0][0][0][ 4][14] =   9.470922e-06;
  val[0][0][0][ 4][15] =  -5.970425e-04; eval[0][0][0][ 4][15] =   1.156963e-05;
  val[0][0][0][ 4][16] =  -5.900239e-04; eval[0][0][0][ 4][16] =   1.433315e-05;
  val[0][0][0][ 4][17] =  -5.760156e-04; eval[0][0][0][ 4][17] =   1.807599e-05;
  val[0][0][0][ 4][18] =  -5.327914e-04; eval[0][0][0][ 4][18] =   2.813102e-05;
  val[0][0][0][ 4][19] =  -5.545795e-04; eval[0][0][0][ 4][19] =   3.370513e-05;
  val[0][0][0][ 4][20] =  -5.161377e-04; eval[0][0][0][ 4][20] =   2.482860e-05;
  val[0][0][0][ 4][21] =  -5.185382e-04; eval[0][0][0][ 4][21] =   4.508308e-05;
  val[0][0][0][ 4][22] =  -5.437861e-04; eval[0][0][0][ 4][22] =   4.396189e-05;
  val[0][0][0][ 4][23] =  -5.775630e-04; eval[0][0][0][ 4][23] =   5.698541e-05;
  val[0][0][0][ 4][24] =  -6.545859e-04; eval[0][0][0][ 4][24] =   7.627386e-05;
  val[0][0][0][ 4][25] =  -4.719967e-04; eval[0][0][0][ 4][25] =   1.036478e-04;
  val[0][0][0][ 4][26] =  -4.302531e-04; eval[0][0][0][ 4][26] =   1.222275e-04;
  val[0][0][0][ 4][27] =  -5.089490e-04; eval[0][0][0][ 4][27] =   1.571818e-04;
  val[0][0][0][ 4][28] =  -4.463791e-04; eval[0][0][0][ 4][28] =   2.045542e-04;
  val[0][0][0][ 4][29] =  -5.217630e-04; eval[0][0][0][ 4][29] =   1.588692e-04;
  //pc3dphi_mean_c0d0z5
  n_val[0][0][0][5] = 30;
  val[0][0][0][ 5][ 0] =   0.000000e+00; eval[0][0][0][ 5][ 0] =   0.000000e+00;
  val[0][0][0][ 5][ 1] =   0.000000e+00; eval[0][0][0][ 5][ 1] =   0.000000e+00;
  val[0][0][0][ 5][ 2] =   1.009579e-04; eval[0][0][0][ 5][ 2] =   1.303330e-06;
  val[0][0][0][ 5][ 3] =  -1.407787e-04; eval[0][0][0][ 5][ 3] =   1.032970e-06;
  val[0][0][0][ 5][ 4] =  -2.658855e-04; eval[0][0][0][ 5][ 4] =   1.113067e-06;
  val[0][0][0][ 5][ 5] =  -2.738597e-04; eval[0][0][0][ 5][ 5] =   1.315319e-06;
  val[0][0][0][ 5][ 6] =  -3.432584e-04; eval[0][0][0][ 5][ 6] =   1.588427e-06;
  val[0][0][0][ 5][ 7] =  -3.591590e-04; eval[0][0][0][ 5][ 7] =   1.944930e-06;
  val[0][0][0][ 5][ 8] =  -2.769601e-04; eval[0][0][0][ 5][ 8] =   2.485949e-06;
  val[0][0][0][ 5][ 9] =  -2.699207e-04; eval[0][0][0][ 5][ 9] =   3.268996e-06;
  val[0][0][0][ 5][10] =  -2.775591e-04; eval[0][0][0][ 5][10] =   4.218819e-06;
  val[0][0][0][ 5][11] =  -2.539764e-04; eval[0][0][0][ 5][11] =   5.566158e-06;
  val[0][0][0][ 5][12] =  -2.999201e-04; eval[0][0][0][ 5][12] =   7.272218e-06;
  val[0][0][0][ 5][13] =  -2.701895e-04; eval[0][0][0][ 5][13] =   9.305638e-06;
  val[0][0][0][ 5][14] =  -3.116702e-04; eval[0][0][0][ 5][14] =   1.213447e-05;
  val[0][0][0][ 5][15] =  -3.204662e-04; eval[0][0][0][ 5][15] =   1.461413e-05;
  val[0][0][0][ 5][16] =  -2.864040e-04; eval[0][0][0][ 5][16] =   1.895291e-05;
  val[0][0][0][ 5][17] =  -2.878142e-04; eval[0][0][0][ 5][17] =   2.386566e-05;
  val[0][0][0][ 5][18] =  -3.603700e-04; eval[0][0][0][ 5][18] =   2.674265e-05;
  val[0][0][0][ 5][19] =  -3.753023e-04; eval[0][0][0][ 5][19] =   3.265013e-05;
  val[0][0][0][ 5][20] =  -3.154508e-04; eval[0][0][0][ 5][20] =   3.105727e-05;
  val[0][0][0][ 5][21] =  -3.640074e-04; eval[0][0][0][ 5][21] =   4.606633e-05;
  val[0][0][0][ 5][22] =  -3.096350e-04; eval[0][0][0][ 5][22] =   5.434680e-05;
  val[0][0][0][ 5][23] =  -2.392814e-04; eval[0][0][0][ 5][23] =   7.034136e-05;
  val[0][0][0][ 5][24] =   5.592517e-05; eval[0][0][0][ 5][24] =   1.002929e-04;
  val[0][0][0][ 5][25] =  -4.910178e-05; eval[0][0][0][ 5][25] =   1.148967e-04;
  val[0][0][0][ 5][26] =  -4.013180e-04; eval[0][0][0][ 5][26] =   1.518027e-04;
  val[0][0][0][ 5][27] =  -9.846267e-05; eval[0][0][0][ 5][27] =   1.475768e-04;
  val[0][0][0][ 5][28] =  -2.851720e-04; eval[0][0][0][ 5][28] =   1.924719e-04;
  val[0][0][0][ 5][29] =   2.724842e-05; eval[0][0][0][ 5][29] =   2.412481e-04;
  //pc3dphi_mean_c0d0z6
  n_val[0][0][0][6] = 30;
  val[0][0][0][ 6][ 0] =   0.000000e+00; eval[0][0][0][ 6][ 0] =   0.000000e+00;
  val[0][0][0][ 6][ 1] =   0.000000e+00; eval[0][0][0][ 6][ 1] =   0.000000e+00;
  val[0][0][0][ 6][ 2] =  -3.408626e-04; eval[0][0][0][ 6][ 2] =   1.181943e-06;
  val[0][0][0][ 6][ 3] =  -3.981557e-04; eval[0][0][0][ 6][ 3] =   8.777219e-07;
  val[0][0][0][ 6][ 4] =  -4.579380e-04; eval[0][0][0][ 6][ 4] =   9.412676e-07;
  val[0][0][0][ 6][ 5] =  -4.583063e-04; eval[0][0][0][ 6][ 5] =   1.115083e-06;
  val[0][0][0][ 6][ 6] =  -5.192096e-04; eval[0][0][0][ 6][ 6] =   1.319331e-06;
  val[0][0][0][ 6][ 7] =  -5.283350e-04; eval[0][0][0][ 6][ 7] =   1.611170e-06;
  val[0][0][0][ 6][ 8] =  -4.016357e-04; eval[0][0][0][ 6][ 8] =   1.997024e-06;
  val[0][0][0][ 6][ 9] =  -3.730697e-04; eval[0][0][0][ 6][ 9] =   2.588262e-06;
  val[0][0][0][ 6][10] =  -3.681785e-04; eval[0][0][0][ 6][10] =   3.340815e-06;
  val[0][0][0][ 6][11] =  -3.503018e-04; eval[0][0][0][ 6][11] =   4.344844e-06;
  val[0][0][0][ 6][12] =  -3.803327e-04; eval[0][0][0][ 6][12] =   5.694134e-06;
  val[0][0][0][ 6][13] =  -3.490924e-04; eval[0][0][0][ 6][13] =   7.377612e-06;
  val[0][0][0][ 6][14] =  -3.964689e-04; eval[0][0][0][ 6][14] =   9.700568e-06;
  val[0][0][0][ 6][15] =  -3.932538e-04; eval[0][0][0][ 6][15] =   1.151799e-05;
  val[0][0][0][ 6][16] =  -3.661281e-04; eval[0][0][0][ 6][16] =   1.490501e-05;
  val[0][0][0][ 6][17] =  -3.823488e-04; eval[0][0][0][ 6][17] =   1.880772e-05;
  val[0][0][0][ 6][18] =  -4.312384e-04; eval[0][0][0][ 6][18] =   2.198838e-05;
  val[0][0][0][ 6][19] =  -2.662485e-04; eval[0][0][0][ 6][19] =   4.839854e-05;
  val[0][0][0][ 6][20] =  -4.227131e-04; eval[0][0][0][ 6][20] =   2.519743e-05;
  val[0][0][0][ 6][21] =  -4.224535e-04; eval[0][0][0][ 6][21] =   3.473096e-05;
  val[0][0][0][ 6][22] =  -4.638191e-04; eval[0][0][0][ 6][22] =   4.169308e-05;
  val[0][0][0][ 6][23] =  -4.105457e-04; eval[0][0][0][ 6][23] =   5.664739e-05;
  val[0][0][0][ 6][24] =  -5.720921e-04; eval[0][0][0][ 6][24] =   7.261925e-05;
  val[0][0][0][ 6][25] =  -4.727487e-04; eval[0][0][0][ 6][25] =   9.315266e-05;
  val[0][0][0][ 6][26] =  -2.271921e-04; eval[0][0][0][ 6][26] =   1.013486e-04;
  val[0][0][0][ 6][27] =  -4.895321e-04; eval[0][0][0][ 6][27] =   1.354827e-04;
  val[0][0][0][ 6][28] =  -6.517796e-04; eval[0][0][0][ 6][28] =   1.878219e-04;
  val[0][0][0][ 6][29] =  -2.070306e-04; eval[0][0][0][ 6][29] =   2.167164e-04;
  //pc3dphi_mean_c0d0z7
  n_val[0][0][0][7] = 30;
  val[0][0][0][ 7][ 0] =   0.000000e+00; eval[0][0][0][ 7][ 0] =   0.000000e+00;
  val[0][0][0][ 7][ 1] =   0.000000e+00; eval[0][0][0][ 7][ 1] =   0.000000e+00;
  val[0][0][0][ 7][ 2] =  -9.482180e-04; eval[0][0][0][ 7][ 2] =   1.637993e-06;
  val[0][0][0][ 7][ 3] =  -8.047715e-04; eval[0][0][0][ 7][ 3] =   1.563613e-06;
  val[0][0][0][ 7][ 4] =  -7.395016e-04; eval[0][0][0][ 7][ 4] =   1.539884e-06;
  val[0][0][0][ 7][ 5] =  -7.731500e-04; eval[0][0][0][ 7][ 5] =   9.348869e-07;
  val[0][0][0][ 7][ 6] =  -7.746320e-04; eval[0][0][0][ 7][ 6] =   1.414261e-06;
  val[0][0][0][ 7][ 7] =  -7.870215e-04; eval[0][0][0][ 7][ 7] =   1.724470e-06;
  val[0][0][0][ 7][ 8] =  -6.623337e-04; eval[0][0][0][ 7][ 8] =   2.087936e-06;
  val[0][0][0][ 7][ 9] =  -6.218620e-04; eval[0][0][0][ 7][ 9] =   2.657307e-06;
  val[0][0][0][ 7][10] =  -5.910390e-04; eval[0][0][0][ 7][10] =   3.392143e-06;
  val[0][0][0][ 7][11] =  -4.684053e-04; eval[0][0][0][ 7][11] =   6.580308e-06;
  val[0][0][0][ 7][12] =  -4.652472e-04; eval[0][0][0][ 7][12] =   8.763630e-06;
  val[0][0][0][ 7][13] =  -4.581768e-04; eval[0][0][0][ 7][13] =   1.094899e-05;
  val[0][0][0][ 7][14] =  -4.518789e-04; eval[0][0][0][ 7][14] =   1.386014e-05;
  val[0][0][0][ 7][15] =  -5.867030e-04; eval[0][0][0][ 7][15] =   1.148548e-05;
  val[0][0][0][ 7][16] =  -5.558994e-04; eval[0][0][0][ 7][16] =   1.416960e-05;
  val[0][0][0][ 7][17] =  -4.308624e-04; eval[0][0][0][ 7][17] =   2.889761e-05;
  val[0][0][0][ 7][18] =  -5.559616e-04; eval[0][0][0][ 7][18] =   2.219027e-05;
  val[0][0][0][ 7][19] =  -6.024950e-04; eval[0][0][0][ 7][19] =   2.557814e-05;
  val[0][0][0][ 7][20] =  -5.218406e-04; eval[0][0][0][ 7][20] =   2.358954e-05;
  val[0][0][0][ 7][21] =  -5.075282e-04; eval[0][0][0][ 7][21] =   4.345148e-05;
  val[0][0][0][ 7][22] =  -5.696654e-04; eval[0][0][0][ 7][22] =   4.118169e-05;
  val[0][0][0][ 7][23] =  -5.570945e-04; eval[0][0][0][ 7][23] =   5.672971e-05;
  val[0][0][0][ 7][24] =  -5.267177e-04; eval[0][0][0][ 7][24] =   6.852013e-05;
  val[0][0][0][ 7][25] =  -6.281054e-04; eval[0][0][0][ 7][25] =   8.904243e-05;
  val[0][0][0][ 7][26] =  -7.714937e-04; eval[0][0][0][ 7][26] =   1.173902e-04;
  val[0][0][0][ 7][27] =  -3.653470e-04; eval[0][0][0][ 7][27] =   1.157350e-04;
  val[0][0][0][ 7][28] =  -6.503827e-04; eval[0][0][0][ 7][28] =   2.044924e-04;
  val[0][0][0][ 7][29] =  -5.199325e-04; eval[0][0][0][ 7][29] =   1.840203e-04;
  //pc3dphi_mean_c0d0z8
  n_val[0][0][0][8] = 30;
  val[0][0][0][ 8][ 0] =   0.000000e+00; eval[0][0][0][ 8][ 0] =   0.000000e+00;
  val[0][0][0][ 8][ 1] =   0.000000e+00; eval[0][0][0][ 8][ 1] =   0.000000e+00;
  val[0][0][0][ 8][ 2] =  -2.088811e-03; eval[0][0][0][ 8][ 2] =   1.073379e-06;
  val[0][0][0][ 8][ 3] =  -1.472770e-03; eval[0][0][0][ 8][ 3] =   9.017338e-07;
  val[0][0][0][ 8][ 4] =  -1.378765e-03; eval[0][0][0][ 8][ 4] =   1.658951e-06;
  val[0][0][0][ 8][ 5] =  -1.150281e-03; eval[0][0][0][ 8][ 5] =   9.409059e-07;
  val[0][0][0][ 8][ 6] =  -1.070523e-03; eval[0][0][0][ 8][ 6] =   1.478892e-06;
  val[0][0][0][ 8][ 7] =  -1.056705e-03; eval[0][0][0][ 8][ 7] =   1.772280e-06;
  val[0][0][0][ 8][ 8] =  -9.570203e-04; eval[0][0][0][ 8][ 8] =   2.214201e-06;
  val[0][0][0][ 8][ 9] =  -9.009946e-04; eval[0][0][0][ 8][ 9] =   2.763757e-06;
  val[0][0][0][ 8][10] =  -8.576404e-04; eval[0][0][0][ 8][10] =   3.483900e-06;
  val[0][0][0][ 8][11] =  -8.149864e-04; eval[0][0][0][ 8][11] =   5.265121e-06;
  val[0][0][0][ 8][12] =  -8.021570e-04; eval[0][0][0][ 8][12] =   6.823931e-06;
  val[0][0][0][ 8][13] =  -7.989645e-04; eval[0][0][0][ 8][13] =   8.429408e-06;
  val[0][0][0][ 8][14] =  -7.558953e-04; eval[0][0][0][ 8][14] =   1.122705e-05;
  val[0][0][0][ 8][15] =  -7.944333e-04; eval[0][0][0][ 8][15] =   1.357688e-05;
  val[0][0][0][ 8][16] =  -7.372784e-04; eval[0][0][0][ 8][16] =   1.704534e-05;
  val[0][0][0][ 8][17] =  -7.119718e-04; eval[0][0][0][ 8][17] =   2.239604e-05;
  val[0][0][0][ 8][18] =  -8.159109e-04; eval[0][0][0][ 8][18] =   2.675845e-05;
  val[0][0][0][ 8][19] =  -7.949569e-04; eval[0][0][0][ 8][19] =   2.958653e-05;
  val[0][0][0][ 8][20] =  -7.328847e-04; eval[0][0][0][ 8][20] =   2.678194e-05;
  val[0][0][0][ 8][21] =  -6.867448e-04; eval[0][0][0][ 8][21] =   3.976771e-05;
  val[0][0][0][ 8][22] =  -7.587073e-04; eval[0][0][0][ 8][22] =   4.269472e-05;
  val[0][0][0][ 8][23] =  -8.270884e-04; eval[0][0][0][ 8][23] =   5.503786e-05;
  val[0][0][0][ 8][24] =  -7.163673e-04; eval[0][0][0][ 8][24] =   6.803735e-05;
  val[0][0][0][ 8][25] =  -7.654733e-04; eval[0][0][0][ 8][25] =   8.784602e-05;
  val[0][0][0][ 8][26] =  -6.597189e-04; eval[0][0][0][ 8][26] =   1.216311e-04;
  val[0][0][0][ 8][27] =  -8.117634e-04; eval[0][0][0][ 8][27] =   1.282139e-04;
  val[0][0][0][ 8][28] =  -4.619621e-04; eval[0][0][0][ 8][28] =   1.519849e-04;
  val[0][0][0][ 8][29] =  -5.600580e-04; eval[0][0][0][ 8][29] =   1.657557e-04;
  //pc3dphi_mean_c0d0z9
  n_val[0][0][0][9] = 30;
  val[0][0][0][ 9][ 0] =   0.000000e+00; eval[0][0][0][ 9][ 0] =   0.000000e+00;
  val[0][0][0][ 9][ 1] =   0.000000e+00; eval[0][0][0][ 9][ 1] =   0.000000e+00;
  val[0][0][0][ 9][ 2] =  -3.339191e-03; eval[0][0][0][ 9][ 2] =   1.119452e-06;
  val[0][0][0][ 9][ 3] =  -2.082980e-03; eval[0][0][0][ 9][ 3] =   8.371610e-07;
  val[0][0][0][ 9][ 4] =  -1.660295e-03; eval[0][0][0][ 9][ 4] =   8.935067e-07;
  val[0][0][0][ 9][ 5] =  -1.489396e-03; eval[0][0][0][ 9][ 5] =   1.122742e-06;
  val[0][0][0][ 9][ 6] =  -1.364828e-03; eval[0][0][0][ 9][ 6] =   1.340330e-06;
  val[0][0][0][ 9][ 7] =  -1.314077e-03; eval[0][0][0][ 9][ 7] =   1.619513e-06;
  val[0][0][0][ 9][ 8] =  -1.119859e-03; eval[0][0][0][ 9][ 8] =   2.134679e-06;
  val[0][0][0][ 9][ 9] =  -1.075583e-03; eval[0][0][0][ 9][ 9] =   3.202787e-06;
  val[0][0][0][ 9][10] =  -1.024322e-03; eval[0][0][0][ 9][10] =   4.096024e-06;
  val[0][0][0][ 9][11] =  -1.017437e-03; eval[0][0][0][ 9][11] =   5.305500e-06;
  val[0][0][0][ 9][12] =  -9.966670e-04; eval[0][0][0][ 9][12] =   6.953860e-06;
  val[0][0][0][ 9][13] =  -1.014125e-03; eval[0][0][0][ 9][13] =   8.654852e-06;
  val[0][0][0][ 9][14] =  -9.836905e-04; eval[0][0][0][ 9][14] =   1.095497e-05;
  val[0][0][0][ 9][15] =  -1.035139e-03; eval[0][0][0][ 9][15] =   1.360042e-05;
  val[0][0][0][ 9][16] =  -9.629032e-04; eval[0][0][0][ 9][16] =   1.653756e-05;
  val[0][0][0][ 9][17] =  -9.488228e-04; eval[0][0][0][ 9][17] =   2.073117e-05;
  val[0][0][0][ 9][18] =  -9.930347e-04; eval[0][0][0][ 9][18] =   2.522160e-05;
  val[0][0][0][ 9][19] =  -9.826525e-04; eval[0][0][0][ 9][19] =   2.940119e-05;
  val[0][0][0][ 9][20] =  -8.698307e-04; eval[0][0][0][ 9][20] =   2.745226e-05;
  val[0][0][0][ 9][21] =  -8.786794e-04; eval[0][0][0][ 9][21] =   3.772168e-05;
  val[0][0][0][ 9][22] =  -9.410833e-04; eval[0][0][0][ 9][22] =   4.021455e-05;
  val[0][0][0][ 9][23] =  -9.172916e-04; eval[0][0][0][ 9][23] =   5.194120e-05;
  val[0][0][0][ 9][24] =  -8.523153e-04; eval[0][0][0][ 9][24] =   6.476113e-05;
  val[0][0][0][ 9][25] =  -7.696010e-04; eval[0][0][0][ 9][25] =   8.758492e-05;
  val[0][0][0][ 9][26] =  -7.402334e-04; eval[0][0][0][ 9][26] =   1.130755e-04;
  val[0][0][0][ 9][27] =  -9.310204e-04; eval[0][0][0][ 9][27] =   1.243212e-04;
  val[0][0][0][ 9][28] =  -8.777987e-04; eval[0][0][0][ 9][28] =   1.536907e-04;
  val[0][0][0][ 9][29] =  -1.086189e-03; eval[0][0][0][ 9][29] =   1.791675e-04;
  //pc3dphi_mean_c0d1z0
  n_val[0][0][1][0] = 30;
  val[0][0][1][ 0][ 0] =   0.000000e+00; eval[0][0][1][ 0][ 0] =   0.000000e+00;
  val[0][0][1][ 0][ 1] =   0.000000e+00; eval[0][0][1][ 0][ 1] =   0.000000e+00;
  val[0][0][1][ 0][ 2] =  -3.967311e-04; eval[0][0][1][ 0][ 2] =   1.016730e-06;
  val[0][0][1][ 0][ 3] =   5.956598e-04; eval[0][0][1][ 0][ 3] =   9.142510e-07;
  val[0][0][1][ 0][ 4] =   8.108495e-04; eval[0][0][1][ 0][ 4] =   8.194281e-07;
  val[0][0][1][ 0][ 5] =   9.466577e-04; eval[0][0][1][ 0][ 5] =   8.863699e-07;
  val[0][0][1][ 0][ 6] =   1.019413e-03; eval[0][0][1][ 0][ 6] =   1.066462e-06;
  val[0][0][1][ 0][ 7] =   1.054184e-03; eval[0][0][1][ 0][ 7] =   1.306974e-06;
  val[0][0][1][ 0][ 8] =   1.090397e-03; eval[0][0][1][ 0][ 8] =   1.573416e-06;
  val[0][0][1][ 0][ 9] =   1.118300e-03; eval[0][0][1][ 0][ 9] =   2.041841e-06;
  val[0][0][1][ 0][10] =   1.138878e-03; eval[0][0][1][ 0][10] =   2.686585e-06;
  val[0][0][1][ 0][11] =   1.154816e-03; eval[0][0][1][ 0][11] =   3.500227e-06;
  val[0][0][1][ 0][12] =   1.174701e-03; eval[0][0][1][ 0][12] =   4.605266e-06;
  val[0][0][1][ 0][13] =   1.191491e-03; eval[0][0][1][ 0][13] =   6.038196e-06;
  val[0][0][1][ 0][14] =   1.202380e-03; eval[0][0][1][ 0][14] =   7.703358e-06;
  val[0][0][1][ 0][15] =   1.214393e-03; eval[0][0][1][ 0][15] =   9.660951e-06;
  val[0][0][1][ 0][16] =   1.234811e-03; eval[0][0][1][ 0][16] =   1.220861e-05;
  val[0][0][1][ 0][17] =   1.211606e-03; eval[0][0][1][ 0][17] =   1.517815e-05;
  val[0][0][1][ 0][18] =   1.238656e-03; eval[0][0][1][ 0][18] =   1.859996e-05;
  val[0][0][1][ 0][19] =   1.265156e-03; eval[0][0][1][ 0][19] =   2.354407e-05;
  val[0][0][1][ 0][20] =   1.270310e-03; eval[0][0][1][ 0][20] =   2.272855e-05;
  val[0][0][1][ 0][21] =   1.253452e-03; eval[0][0][1][ 0][21] =   3.159818e-05;
  val[0][0][1][ 0][22] =   1.233318e-03; eval[0][0][1][ 0][22] =   4.336133e-05;
  val[0][0][1][ 0][23] =   1.246489e-03; eval[0][0][1][ 0][23] =   6.350479e-05;
  val[0][0][1][ 0][24] =   1.467906e-03; eval[0][0][1][ 0][24] =   7.730280e-05;
  val[0][0][1][ 0][25] =   1.443377e-03; eval[0][0][1][ 0][25] =   1.101344e-04;
  val[0][0][1][ 0][26] =   1.336108e-03; eval[0][0][1][ 0][26] =   1.473805e-04;
  val[0][0][1][ 0][27] =   6.973274e-04; eval[0][0][1][ 0][27] =   9.880049e-03;
  val[0][0][1][ 0][28] =   1.319775e-03; eval[0][0][1][ 0][28] =   1.363491e-04;
  val[0][0][1][ 0][29] =   1.396580e-03; eval[0][0][1][ 0][29] =   2.118328e-04;
  //pc3dphi_mean_c0d1z1
  n_val[0][0][1][1] = 30;
  val[0][0][1][ 1][ 0] =   0.000000e+00; eval[0][0][1][ 1][ 0] =   0.000000e+00;
  val[0][0][1][ 1][ 1] =   0.000000e+00; eval[0][0][1][ 1][ 1] =   0.000000e+00;
  val[0][0][1][ 1][ 2] =   4.078777e-04; eval[0][0][1][ 1][ 2] =   8.776321e-07;
  val[0][0][1][ 1][ 3] =   9.357658e-04; eval[0][0][1][ 1][ 3] =   7.430228e-07;
  val[0][0][1][ 1][ 4] =   1.027013e-03; eval[0][0][1][ 1][ 4] =   7.409240e-07;
  val[0][0][1][ 1][ 5] =   1.115553e-03; eval[0][0][1][ 1][ 5] =   8.429297e-07;
  val[0][0][1][ 1][ 6] =   1.135297e-03; eval[0][0][1][ 1][ 6] =   9.783692e-07;
  val[0][0][1][ 1][ 7] =   1.151300e-03; eval[0][0][1][ 1][ 7] =   1.221085e-06;
  val[0][0][1][ 1][ 8] =   1.179060e-03; eval[0][0][1][ 1][ 8] =   1.521958e-06;
  val[0][0][1][ 1][ 9] =   1.195317e-03; eval[0][0][1][ 1][ 9] =   1.988366e-06;
  val[0][0][1][ 1][10] =   1.201456e-03; eval[0][0][1][ 1][10] =   2.593929e-06;
  val[0][0][1][ 1][11] =   1.213837e-03; eval[0][0][1][ 1][11] =   3.393601e-06;
  val[0][0][1][ 1][12] =   1.221989e-03; eval[0][0][1][ 1][12] =   4.425149e-06;
  val[0][0][1][ 1][13] =   1.224701e-03; eval[0][0][1][ 1][13] =   5.721809e-06;
  val[0][0][1][ 1][14] =   1.231961e-03; eval[0][0][1][ 1][14] =   7.251333e-06;
  val[0][0][1][ 1][15] =   1.254912e-03; eval[0][0][1][ 1][15] =   9.227084e-06;
  val[0][0][1][ 1][16] =   1.283584e-03; eval[0][0][1][ 1][16] =   1.163727e-05;
  val[0][0][1][ 1][17] =   1.270549e-03; eval[0][0][1][ 1][17] =   1.463234e-05;
  val[0][0][1][ 1][18] =   1.286038e-03; eval[0][0][1][ 1][18] =   1.802685e-05;
  val[0][0][1][ 1][19] =   1.281768e-03; eval[0][0][1][ 1][19] =   2.208615e-05;
  val[0][0][1][ 1][20] =   1.286894e-03; eval[0][0][1][ 1][20] =   1.995573e-05;
  val[0][0][1][ 1][21] =   1.235459e-03; eval[0][0][1][ 1][21] =   2.839488e-05;
  val[0][0][1][ 1][22] =   1.302769e-03; eval[0][0][1][ 1][22] =   4.117193e-05;
  val[0][0][1][ 1][23] =   1.424946e-03; eval[0][0][1][ 1][23] =   4.935422e-05;
  val[0][0][1][ 1][24] =   1.391730e-03; eval[0][0][1][ 1][24] =   7.563187e-05;
  val[0][0][1][ 1][25] =   1.316046e-03; eval[0][0][1][ 1][25] =   8.514456e-05;
  val[0][0][1][ 1][26] =   1.153407e-03; eval[0][0][1][ 1][26] =   1.047893e-04;
  val[0][0][1][ 1][27] =   1.427099e-03; eval[0][0][1][ 1][27] =   1.323407e-04;
  val[0][0][1][ 1][28] =   9.379754e-03; eval[0][0][1][ 1][28] =   2.476635e-02;
  val[0][0][1][ 1][29] =   1.000000e-03; eval[0][0][1][ 1][29] =   1.887043e-01;
  //pc3dphi_mean_c0d1z2
  n_val[0][0][1][2] = 30;
  val[0][0][1][ 2][ 0] =   0.000000e+00; eval[0][0][1][ 2][ 0] =   0.000000e+00;
  val[0][0][1][ 2][ 1] =   0.000000e+00; eval[0][0][1][ 2][ 1] =   0.000000e+00;
  val[0][0][1][ 2][ 2] =   1.154592e-03; eval[0][0][1][ 2][ 2] =   8.616640e-07;
  val[0][0][1][ 2][ 3] =   1.251387e-03; eval[0][0][1][ 2][ 3] =   7.600831e-07;
  val[0][0][1][ 2][ 4] =   1.240459e-03; eval[0][0][1][ 2][ 4] =   7.553566e-07;
  val[0][0][1][ 2][ 5] =   1.252046e-03; eval[0][0][1][ 2][ 5] =   8.385331e-07;
  val[0][0][1][ 2][ 6] =   1.233978e-03; eval[0][0][1][ 2][ 6] =   9.830637e-07;
  val[0][0][1][ 2][ 7] =   1.217698e-03; eval[0][0][1][ 2][ 7] =   1.205578e-06;
  val[0][0][1][ 2][ 8] =   1.225936e-03; eval[0][0][1][ 2][ 8] =   1.476072e-06;
  val[0][0][1][ 2][ 9] =   1.233401e-03; eval[0][0][1][ 2][ 9] =   1.931991e-06;
  val[0][0][1][ 2][10] =   1.241284e-03; eval[0][0][1][ 2][10] =   2.524878e-06;
  val[0][0][1][ 2][11] =   1.238358e-03; eval[0][0][1][ 2][11] =   3.310721e-06;
  val[0][0][1][ 2][12] =   1.252174e-03; eval[0][0][1][ 2][12] =   4.408495e-06;
  val[0][0][1][ 2][13] =   1.245150e-03; eval[0][0][1][ 2][13] =   5.694876e-06;
  val[0][0][1][ 2][14] =   1.244732e-03; eval[0][0][1][ 2][14] =   7.281519e-06;
  val[0][0][1][ 2][15] =   1.273877e-03; eval[0][0][1][ 2][15] =   9.263393e-06;
  val[0][0][1][ 2][16] =   1.242551e-03; eval[0][0][1][ 2][16] =   1.145343e-05;
  val[0][0][1][ 2][17] =   1.263542e-03; eval[0][0][1][ 2][17] =   1.468657e-05;
  val[0][0][1][ 2][18] =   1.284344e-03; eval[0][0][1][ 2][18] =   1.784760e-05;
  val[0][0][1][ 2][19] =   1.212743e-03; eval[0][0][1][ 2][19] =   2.070628e-05;
  val[0][0][1][ 2][20] =   1.289328e-03; eval[0][0][1][ 2][20] =   2.095380e-05;
  val[0][0][1][ 2][21] =   1.192650e-03; eval[0][0][1][ 2][21] =   2.930647e-05;
  val[0][0][1][ 2][22] =   1.309688e-03; eval[0][0][1][ 2][22] =   4.339437e-05;
  val[0][0][1][ 2][23] =   1.258362e-03; eval[0][0][1][ 2][23] =   4.986870e-05;
  val[0][0][1][ 2][24] =   1.242597e-03; eval[0][0][1][ 2][24] =   7.009098e-05;
  val[0][0][1][ 2][25] =   1.257503e-03; eval[0][0][1][ 2][25] =   8.397173e-05;
  val[0][0][1][ 2][26] =   1.332509e-03; eval[0][0][1][ 2][26] =   1.039632e-04;
  val[0][0][1][ 2][27] =   1.289708e-03; eval[0][0][1][ 2][27] =   1.358492e-04;
  val[0][0][1][ 2][28] =   1.291905e-03; eval[0][0][1][ 2][28] =   1.643232e-04;
  val[0][0][1][ 2][29] =   1.437827e-03; eval[0][0][1][ 2][29] =   1.984696e-04;
  //pc3dphi_mean_c0d1z3
  n_val[0][0][1][3] = 30;
  val[0][0][1][ 3][ 0] =   0.000000e+00; eval[0][0][1][ 3][ 0] =   0.000000e+00;
  val[0][0][1][ 3][ 1] =   0.000000e+00; eval[0][0][1][ 3][ 1] =   0.000000e+00;
  val[0][0][1][ 3][ 2] =   1.605629e-03; eval[0][0][1][ 3][ 2] =   7.764248e-07;
  val[0][0][1][ 3][ 3] =   1.447456e-03; eval[0][0][1][ 3][ 3] =   8.563366e-07;
  val[0][0][1][ 3][ 4] =   1.405475e-03; eval[0][0][1][ 3][ 4] =   8.070442e-07;
  val[0][0][1][ 3][ 5] =   1.437718e-03; eval[0][0][1][ 3][ 5] =   7.964348e-07;
  val[0][0][1][ 3][ 6] =   1.337332e-03; eval[0][0][1][ 3][ 6] =   1.023802e-06;
  val[0][0][1][ 3][ 7] =   1.329111e-03; eval[0][0][1][ 3][ 7] =   1.256871e-06;
  val[0][0][1][ 3][ 8] =   1.323092e-03; eval[0][0][1][ 3][ 8] =   1.556577e-06;
  val[0][0][1][ 3][ 9] =   1.331542e-03; eval[0][0][1][ 3][ 9] =   2.047530e-06;
  val[0][0][1][ 3][10] =   1.331864e-03; eval[0][0][1][ 3][10] =   2.686459e-06;
  val[0][0][1][ 3][11] =   1.324178e-03; eval[0][0][1][ 3][11] =   3.507431e-06;
  val[0][0][1][ 3][12] =   1.333767e-03; eval[0][0][1][ 3][12] =   4.590200e-06;
  val[0][0][1][ 3][13] =   1.320025e-03; eval[0][0][1][ 3][13] =   6.037336e-06;
  val[0][0][1][ 3][14] =   1.333699e-03; eval[0][0][1][ 3][14] =   7.754990e-06;
  val[0][0][1][ 3][15] =   1.353386e-03; eval[0][0][1][ 3][15] =   9.609987e-06;
  val[0][0][1][ 3][16] =   1.369650e-03; eval[0][0][1][ 3][16] =   1.241654e-05;
  val[0][0][1][ 3][17] =   1.312115e-03; eval[0][0][1][ 3][17] =   1.464024e-05;
  val[0][0][1][ 3][18] =   1.319243e-03; eval[0][0][1][ 3][18] =   1.808664e-05;
  val[0][0][1][ 3][19] =   1.344285e-03; eval[0][0][1][ 3][19] =   2.288831e-05;
  val[0][0][1][ 3][20] =   1.339105e-03; eval[0][0][1][ 3][20] =   2.163979e-05;
  val[0][0][1][ 3][21] =   1.302288e-03; eval[0][0][1][ 3][21] =   2.877518e-05;
  val[0][0][1][ 3][22] =   1.343431e-03; eval[0][0][1][ 3][22] =   4.081783e-05;
  val[0][0][1][ 3][23] =   1.419022e-03; eval[0][0][1][ 3][23] =   5.240619e-05;
  val[0][0][1][ 3][24] =   1.409982e-03; eval[0][0][1][ 3][24] =   7.400534e-05;
  val[0][0][1][ 3][25] =   1.441286e-03; eval[0][0][1][ 3][25] =   9.485792e-05;
  val[0][0][1][ 3][26] =   1.290601e-03; eval[0][0][1][ 3][26] =   8.998111e-05;
  val[0][0][1][ 3][27] =   1.291583e-03; eval[0][0][1][ 3][27] =   1.585398e-04;
  val[0][0][1][ 3][28] =   1.405003e-03; eval[0][0][1][ 3][28] =   1.579976e-04;
  val[0][0][1][ 3][29] =   1.331556e-03; eval[0][0][1][ 3][29] =   3.083903e-04;
  //pc3dphi_mean_c0d1z4
  n_val[0][0][1][4] = 30;
  val[0][0][1][ 4][ 0] =   0.000000e+00; eval[0][0][1][ 4][ 0] =   0.000000e+00;
  val[0][0][1][ 4][ 1] =   0.000000e+00; eval[0][0][1][ 4][ 1] =   0.000000e+00;
  val[0][0][1][ 4][ 2] =   1.804462e-03; eval[0][0][1][ 4][ 2] =   9.098016e-07;
  val[0][0][1][ 4][ 3] =   1.672592e-03; eval[0][0][1][ 4][ 3] =   7.821419e-07;
  val[0][0][1][ 4][ 4] =   1.473916e-03; eval[0][0][1][ 4][ 4] =   9.938596e-07;
  val[0][0][1][ 4][ 5] =   1.471068e-03; eval[0][0][1][ 4][ 5] =   9.137532e-07;
  val[0][0][1][ 4][ 6] =   1.357924e-03; eval[0][0][1][ 4][ 6] =   1.171951e-06;
  val[0][0][1][ 4][ 7] =   1.344597e-03; eval[0][0][1][ 4][ 7] =   1.435773e-06;
  val[0][0][1][ 4][ 8] =   1.335945e-03; eval[0][0][1][ 4][ 8] =   1.809467e-06;
  val[0][0][1][ 4][ 9] =   1.325870e-03; eval[0][0][1][ 4][ 9] =   2.403319e-06;
  val[0][0][1][ 4][10] =   1.311379e-03; eval[0][0][1][ 4][10] =   3.111151e-06;
  val[0][0][1][ 4][11] =   1.307809e-03; eval[0][0][1][ 4][11] =   4.106588e-06;
  val[0][0][1][ 4][12] =   1.314864e-03; eval[0][0][1][ 4][12] =   5.398554e-06;
  val[0][0][1][ 4][13] =   1.294153e-03; eval[0][0][1][ 4][13] =   6.921546e-06;
  val[0][0][1][ 4][14] =   1.304299e-03; eval[0][0][1][ 4][14] =   8.798555e-06;
  val[0][0][1][ 4][15] =   1.316613e-03; eval[0][0][1][ 4][15] =   1.081755e-05;
  val[0][0][1][ 4][16] =   1.328866e-03; eval[0][0][1][ 4][16] =   1.443141e-05;
  val[0][0][1][ 4][17] =   1.295748e-03; eval[0][0][1][ 4][17] =   1.756579e-05;
  val[0][0][1][ 4][18] =   1.314984e-03; eval[0][0][1][ 4][18] =   2.201818e-05;
  val[0][0][1][ 4][19] =   1.313069e-03; eval[0][0][1][ 4][19] =   2.653608e-05;
  val[0][0][1][ 4][20] =   1.315059e-03; eval[0][0][1][ 4][20] =   2.555741e-05;
  val[0][0][1][ 4][21] =   1.256548e-03; eval[0][0][1][ 4][21] =   3.469886e-05;
  val[0][0][1][ 4][22] =   1.257477e-03; eval[0][0][1][ 4][22] =   4.586955e-05;
  val[0][0][1][ 4][23] =   1.419831e-03; eval[0][0][1][ 4][23] =   6.678364e-05;
  val[0][0][1][ 4][24] =   1.174767e-03; eval[0][0][1][ 4][24] =   8.139861e-05;
  val[0][0][1][ 4][25] =   1.584659e-03; eval[0][0][1][ 4][25] =   1.156015e-04;
  val[0][0][1][ 4][26] =   1.516009e-03; eval[0][0][1][ 4][26] =   1.308675e-04;
  val[0][0][1][ 4][27] =   1.490104e-03; eval[0][0][1][ 4][27] =   1.544252e-04;
  val[0][0][1][ 4][28] =   1.539416e-03; eval[0][0][1][ 4][28] =   2.804217e-04;
  val[0][0][1][ 4][29] =   1.401290e-03; eval[0][0][1][ 4][29] =   3.587116e-04;
  //pc3dphi_mean_c0d1z5
  n_val[0][0][1][5] = 30;
  val[0][0][1][ 5][ 0] =   0.000000e+00; eval[0][0][1][ 5][ 0] =   0.000000e+00;
  val[0][0][1][ 5][ 1] =   0.000000e+00; eval[0][0][1][ 5][ 1] =   0.000000e+00;
  val[0][0][1][ 5][ 2] =   1.614513e-03; eval[0][0][1][ 5][ 2] =   1.025403e-06;
  val[0][0][1][ 5][ 3] =   1.299842e-03; eval[0][0][1][ 5][ 3] =   9.616981e-07;
  val[0][0][1][ 5][ 4] =   1.241699e-03; eval[0][0][1][ 5][ 4] =   9.148154e-07;
  val[0][0][1][ 5][ 5] =   1.148659e-03; eval[0][0][1][ 5][ 5] =   1.003179e-06;
  val[0][0][1][ 5][ 6] =   1.086557e-03; eval[0][0][1][ 5][ 6] =   1.167546e-06;
  val[0][0][1][ 5][ 7] =   1.059777e-03; eval[0][0][1][ 5][ 7] =   1.468533e-06;
  val[0][0][1][ 5][ 8] =   1.045173e-03; eval[0][0][1][ 5][ 8] =   1.852921e-06;
  val[0][0][1][ 5][ 9] =   1.052050e-03; eval[0][0][1][ 5][ 9] =   2.404696e-06;
  val[0][0][1][ 5][10] =   1.064440e-03; eval[0][0][1][ 5][10] =   3.195463e-06;
  val[0][0][1][ 5][11] =   1.075449e-03; eval[0][0][1][ 5][11] =   4.205215e-06;
  val[0][0][1][ 5][12] =   1.085671e-03; eval[0][0][1][ 5][12] =   5.537963e-06;
  val[0][0][1][ 5][13] =   1.092787e-03; eval[0][0][1][ 5][13] =   7.219850e-06;
  val[0][0][1][ 5][14] =   1.103294e-03; eval[0][0][1][ 5][14] =   9.438515e-06;
  val[0][0][1][ 5][15] =   1.142372e-03; eval[0][0][1][ 5][15] =   1.161851e-05;
  val[0][0][1][ 5][16] =   1.109952e-03; eval[0][0][1][ 5][16] =   1.470023e-05;
  val[0][0][1][ 5][17] =   1.088709e-03; eval[0][0][1][ 5][17] =   1.872017e-05;
  val[0][0][1][ 5][18] =   1.107955e-03; eval[0][0][1][ 5][18] =   2.184821e-05;
  val[0][0][1][ 5][19] =   1.143775e-03; eval[0][0][1][ 5][19] =   2.706854e-05;
  val[0][0][1][ 5][20] =   1.163337e-03; eval[0][0][1][ 5][20] =   2.504528e-05;
  val[0][0][1][ 5][21] =   1.027203e-03; eval[0][0][1][ 5][21] =   3.769871e-05;
  val[0][0][1][ 5][22] =   1.114571e-03; eval[0][0][1][ 5][22] =   4.595619e-05;
  val[0][0][1][ 5][23] =   1.116978e-03; eval[0][0][1][ 5][23] =   6.617128e-05;
  val[0][0][1][ 5][24] =   1.037785e-03; eval[0][0][1][ 5][24] =   8.841679e-05;
  val[0][0][1][ 5][25] =   8.487642e-04; eval[0][0][1][ 5][25] =   1.185916e-04;
  val[0][0][1][ 5][26] =   1.305175e-03; eval[0][0][1][ 5][26] =   1.263877e-04;
  val[0][0][1][ 5][27] =   1.304351e-03; eval[0][0][1][ 5][27] =   1.859394e-04;
  val[0][0][1][ 5][28] =   1.193316e-03; eval[0][0][1][ 5][28] =   1.491330e-04;
  val[0][0][1][ 5][29] =   1.335238e-03; eval[0][0][1][ 5][29] =   3.514967e-04;
  //pc3dphi_mean_c0d1z6
  n_val[0][0][1][6] = 30;
  val[0][0][1][ 6][ 0] =   0.000000e+00; eval[0][0][1][ 6][ 0] =   0.000000e+00;
  val[0][0][1][ 6][ 1] =   0.000000e+00; eval[0][0][1][ 6][ 1] =   0.000000e+00;
  val[0][0][1][ 6][ 2] =   1.358996e-03; eval[0][0][1][ 6][ 2] =   9.427214e-07;
  val[0][0][1][ 6][ 3] =   1.159524e-03; eval[0][0][1][ 6][ 3] =   7.688837e-07;
  val[0][0][1][ 6][ 4] =   1.155122e-03; eval[0][0][1][ 6][ 4] =   7.492352e-07;
  val[0][0][1][ 6][ 5] =   1.102097e-03; eval[0][0][1][ 6][ 5] =   8.416040e-07;
  val[0][0][1][ 6][ 6] =   1.071790e-03; eval[0][0][1][ 6][ 6] =   9.902006e-07;
  val[0][0][1][ 6][ 7] =   1.064986e-03; eval[0][0][1][ 6][ 7] =   1.234833e-06;
  val[0][0][1][ 6][ 8] =   1.065415e-03; eval[0][0][1][ 6][ 8] =   1.545974e-06;
  val[0][0][1][ 6][ 9] =   1.061920e-03; eval[0][0][1][ 6][ 9] =   2.023937e-06;
  val[0][0][1][ 6][10] =   1.078550e-03; eval[0][0][1][ 6][10] =   2.671855e-06;
  val[0][0][1][ 6][11] =   1.070027e-03; eval[0][0][1][ 6][11] =   3.506995e-06;
  val[0][0][1][ 6][12] =   1.097037e-03; eval[0][0][1][ 6][12] =   4.602471e-06;
  val[0][0][1][ 6][13] =   1.094242e-03; eval[0][0][1][ 6][13] =   6.037474e-06;
  val[0][0][1][ 6][14] =   1.095771e-03; eval[0][0][1][ 6][14] =   7.738321e-06;
  val[0][0][1][ 6][15] =   1.120788e-03; eval[0][0][1][ 6][15] =   9.952595e-06;
  val[0][0][1][ 6][16] =   1.086816e-03; eval[0][0][1][ 6][16] =   1.237278e-05;
  val[0][0][1][ 6][17] =   1.089021e-03; eval[0][0][1][ 6][17] =   1.561295e-05;
  val[0][0][1][ 6][18] =   1.104950e-03; eval[0][0][1][ 6][18] =   1.913022e-05;
  val[0][0][1][ 6][19] =   1.177532e-03; eval[0][0][1][ 6][19] =   2.285211e-05;
  val[0][0][1][ 6][20] =   1.129131e-03; eval[0][0][1][ 6][20] =   2.195611e-05;
  val[0][0][1][ 6][21] =   1.065161e-03; eval[0][0][1][ 6][21] =   3.200817e-05;
  val[0][0][1][ 6][22] =   1.123942e-03; eval[0][0][1][ 6][22] =   3.974233e-05;
  val[0][0][1][ 6][23] =   1.000000e-03; eval[0][0][1][ 6][23] =   5.702235e-05;
  val[0][0][1][ 6][24] =   1.023056e-03; eval[0][0][1][ 6][24] =   7.509909e-05;
  val[0][0][1][ 6][25] =   1.170292e-03; eval[0][0][1][ 6][25] =   9.006985e-05;
  val[0][0][1][ 6][26] =   1.015636e-03; eval[0][0][1][ 6][26] =   1.259612e-04;
  val[0][0][1][ 6][27] =  -6.668907e-03; eval[0][0][1][ 6][27] =   4.730534e+00;
  val[0][0][1][ 6][28] =   1.095148e-03; eval[0][0][1][ 6][28] =   2.191376e-04;
  val[0][0][1][ 6][29] =   1.290506e-03; eval[0][0][1][ 6][29] =   2.762555e-04;
  //pc3dphi_mean_c0d1z7
  n_val[0][0][1][7] = 30;
  val[0][0][1][ 7][ 0] =   0.000000e+00; eval[0][0][1][ 7][ 0] =   0.000000e+00;
  val[0][0][1][ 7][ 1] =   0.000000e+00; eval[0][0][1][ 7][ 1] =   0.000000e+00;
  val[0][0][1][ 7][ 2] =   6.256917e-04; eval[0][0][1][ 7][ 2] =   1.024737e-06;
  val[0][0][1][ 7][ 3] =   9.005362e-04; eval[0][0][1][ 7][ 3] =   7.929657e-07;
  val[0][0][1][ 7][ 4] =   9.607916e-04; eval[0][0][1][ 7][ 4] =   7.557544e-07;
  val[0][0][1][ 7][ 5] =   9.793058e-04; eval[0][0][1][ 7][ 5] =   8.652918e-07;
  val[0][0][1][ 7][ 6] =   9.587733e-04; eval[0][0][1][ 7][ 6] =   1.014500e-06;
  val[0][0][1][ 7][ 7] =   9.763675e-04; eval[0][0][1][ 7][ 7] =   1.248983e-06;
  val[0][0][1][ 7][ 8] =   9.951694e-04; eval[0][0][1][ 7][ 8] =   1.561856e-06;
  val[0][0][1][ 7][ 9] =   1.017129e-03; eval[0][0][1][ 7][ 9] =   2.057433e-06;
  val[0][0][1][ 7][10] =   1.030950e-03; eval[0][0][1][ 7][10] =   2.706128e-06;
  val[0][0][1][ 7][11] =   1.027381e-03; eval[0][0][1][ 7][11] =   3.585959e-06;
  val[0][0][1][ 7][12] =   1.031566e-03; eval[0][0][1][ 7][12] =   4.757915e-06;
  val[0][0][1][ 7][13] =   1.025721e-03; eval[0][0][1][ 7][13] =   6.167577e-06;
  val[0][0][1][ 7][14] =   1.019895e-03; eval[0][0][1][ 7][14] =   7.919473e-06;
  val[0][0][1][ 7][15] =   1.044761e-03; eval[0][0][1][ 7][15] =   9.911358e-06;
  val[0][0][1][ 7][16] =   1.044790e-03; eval[0][0][1][ 7][16] =   1.238744e-05;
  val[0][0][1][ 7][17] =   1.023484e-03; eval[0][0][1][ 7][17] =   1.532796e-05;
  val[0][0][1][ 7][18] =   1.058712e-03; eval[0][0][1][ 7][18] =   1.926350e-05;
  val[0][0][1][ 7][19] =   1.054778e-03; eval[0][0][1][ 7][19] =   2.219491e-05;
  val[0][0][1][ 7][20] =   1.054757e-03; eval[0][0][1][ 7][20] =   2.116484e-05;
  val[0][0][1][ 7][21] =   1.018438e-03; eval[0][0][1][ 7][21] =   3.246647e-05;
  val[0][0][1][ 7][22] =   1.101173e-03; eval[0][0][1][ 7][22] =   3.859131e-05;
  val[0][0][1][ 7][23] =   9.818693e-04; eval[0][0][1][ 7][23] =   5.195346e-05;
  val[0][0][1][ 7][24] =   1.170323e-03; eval[0][0][1][ 7][24] =   7.053066e-05;
  val[0][0][1][ 7][25] =   1.103572e-03; eval[0][0][1][ 7][25] =   1.028756e-04;
  val[0][0][1][ 7][26] =   1.241606e-03; eval[0][0][1][ 7][26] =   1.266002e-04;
  val[0][0][1][ 7][27] =   1.106355e-03; eval[0][0][1][ 7][27] =   1.520535e-04;
  val[0][0][1][ 7][28] =   1.365341e-03; eval[0][0][1][ 7][28] =   1.552282e-04;
  val[0][0][1][ 7][29] =   1.402250e-03; eval[0][0][1][ 7][29] =   3.846414e-04;
  //pc3dphi_mean_c0d1z8
  n_val[0][0][1][8] = 30;
  val[0][0][1][ 8][ 0] =   0.000000e+00; eval[0][0][1][ 8][ 0] =   0.000000e+00;
  val[0][0][1][ 8][ 1] =   0.000000e+00; eval[0][0][1][ 8][ 1] =   0.000000e+00;
  val[0][0][1][ 8][ 2] =  -1.659458e-04; eval[0][0][1][ 8][ 2] =   8.730962e-07;
  val[0][0][1][ 8][ 3] =   3.644179e-04; eval[0][0][1][ 8][ 3] =   7.268975e-07;
  val[0][0][1][ 8][ 4] =   7.235169e-04; eval[0][0][1][ 8][ 4] =   7.865903e-07;
  val[0][0][1][ 8][ 5] =   7.701031e-04; eval[0][0][1][ 8][ 5] =   8.527226e-07;
  val[0][0][1][ 8][ 6] =   8.217111e-04; eval[0][0][1][ 8][ 6] =   9.939161e-07;
  val[0][0][1][ 8][ 7] =   8.625657e-04; eval[0][0][1][ 8][ 7] =   1.244174e-06;
  val[0][0][1][ 8][ 8] =   9.027389e-04; eval[0][0][1][ 8][ 8] =   1.574511e-06;
  val[0][0][1][ 8][ 9] =   9.434001e-04; eval[0][0][1][ 8][ 9] =   2.058244e-06;
  val[0][0][1][ 8][10] =   9.620337e-04; eval[0][0][1][ 8][10] =   2.724343e-06;
  val[0][0][1][ 8][11] =   9.789920e-04; eval[0][0][1][ 8][11] =   3.589238e-06;
  val[0][0][1][ 8][12] =   9.871982e-04; eval[0][0][1][ 8][12] =   4.731278e-06;
  val[0][0][1][ 8][13] =   1.002807e-03; eval[0][0][1][ 8][13] =   6.172191e-06;
  val[0][0][1][ 8][14] =   1.005832e-03; eval[0][0][1][ 8][14] =   8.059000e-06;
  val[0][0][1][ 8][15] =   1.024701e-03; eval[0][0][1][ 8][15] =   1.023340e-05;
  val[0][0][1][ 8][16] =   1.040101e-03; eval[0][0][1][ 8][16] =   1.298706e-05;
  val[0][0][1][ 8][17] =   1.032431e-03; eval[0][0][1][ 8][17] =   1.606510e-05;
  val[0][0][1][ 8][18] =   1.040955e-03; eval[0][0][1][ 8][18] =   1.947678e-05;
  val[0][0][1][ 8][19] =   1.016797e-03; eval[0][0][1][ 8][19] =   2.310714e-05;
  val[0][0][1][ 8][20] =   1.034158e-03; eval[0][0][1][ 8][20] =   2.193237e-05;
  val[0][0][1][ 8][21] =   1.084952e-03; eval[0][0][1][ 8][21] =   3.347058e-05;
  val[0][0][1][ 8][22] =   1.059048e-03; eval[0][0][1][ 8][22] =   4.130813e-05;
  val[0][0][1][ 8][23] =   1.081018e-03; eval[0][0][1][ 8][23] =   6.341007e-05;
  val[0][0][1][ 8][24] =   1.016875e-03; eval[0][0][1][ 8][24] =   7.349702e-05;
  val[0][0][1][ 8][25] =   1.058074e-03; eval[0][0][1][ 8][25] =   1.033935e-04;
  val[0][0][1][ 8][26] =   1.091125e-03; eval[0][0][1][ 8][26] =   1.307388e-04;
  val[0][0][1][ 8][27] =   1.320309e-03; eval[0][0][1][ 8][27] =   1.606736e-04;
  val[0][0][1][ 8][28] =   1.194775e-03; eval[0][0][1][ 8][28] =   1.855786e-04;
  val[0][0][1][ 8][29] =   1.450211e-03; eval[0][0][1][ 8][29] =   2.091604e-04;
  //pc3dphi_mean_c0d1z9
  n_val[0][0][1][9] = 30;
  val[0][0][1][ 9][ 0] =   0.000000e+00; eval[0][0][1][ 9][ 0] =   0.000000e+00;
  val[0][0][1][ 9][ 1] =   0.000000e+00; eval[0][0][1][ 9][ 1] =   0.000000e+00;
  val[0][0][1][ 9][ 2] =  -1.005681e-03; eval[0][0][1][ 9][ 2] =   1.865438e-06;
  val[0][0][1][ 9][ 3] =  -5.463669e-05; eval[0][0][1][ 9][ 3] =   9.171665e-07;
  val[0][0][1][ 9][ 4] =   5.206363e-04; eval[0][0][1][ 9][ 4] =   1.016875e-06;
  val[0][0][1][ 9][ 5] =   5.522206e-04; eval[0][0][1][ 9][ 5] =   9.997579e-07;
  val[0][0][1][ 9][ 6] =   7.492216e-04; eval[0][0][1][ 9][ 6] =   1.211390e-06;
  val[0][0][1][ 9][ 7] =   8.289009e-04; eval[0][0][1][ 9][ 7] =   1.467303e-06;
  val[0][0][1][ 9][ 8] =   8.891692e-04; eval[0][0][1][ 9][ 8] =   1.868149e-06;
  val[0][0][1][ 9][ 9] =   9.271392e-04; eval[0][0][1][ 9][ 9] =   2.435319e-06;
  val[0][0][1][ 9][10] =   9.641598e-04; eval[0][0][1][ 9][10] =   3.220150e-06;
  val[0][0][1][ 9][11] =   9.919998e-04; eval[0][0][1][ 9][11] =   4.207776e-06;
  val[0][0][1][ 9][12] =   1.002927e-03; eval[0][0][1][ 9][12] =   5.580318e-06;
  val[0][0][1][ 9][13] =   1.012340e-03; eval[0][0][1][ 9][13] =   7.259917e-06;
  val[0][0][1][ 9][14] =   9.975480e-04; eval[0][0][1][ 9][14] =   9.586055e-06;
  val[0][0][1][ 9][15] =   1.019068e-03; eval[0][0][1][ 9][15] =   1.181627e-05;
  val[0][0][1][ 9][16] =   1.000832e-03; eval[0][0][1][ 9][16] =   1.473013e-05;
  val[0][0][1][ 9][17] =   9.785275e-04; eval[0][0][1][ 9][17] =   1.808095e-05;
  val[0][0][1][ 9][18] =   1.022718e-03; eval[0][0][1][ 9][18] =   2.247278e-05;
  val[0][0][1][ 9][19] =   1.058484e-03; eval[0][0][1][ 9][19] =   2.660947e-05;
  val[0][0][1][ 9][20] =   1.061504e-03; eval[0][0][1][ 9][20] =   2.395964e-05;
  val[0][0][1][ 9][21] =   1.050198e-03; eval[0][0][1][ 9][21] =   3.689993e-05;
  val[0][0][1][ 9][22] =   1.138165e-03; eval[0][0][1][ 9][22] =   5.280693e-05;
  val[0][0][1][ 9][23] =   1.118998e-03; eval[0][0][1][ 9][23] =   6.401668e-05;
  val[0][0][1][ 9][24] =   1.057264e-03; eval[0][0][1][ 9][24] =   8.986659e-05;
  val[0][0][1][ 9][25] =   1.028006e-03; eval[0][0][1][ 9][25] =   1.000655e-04;
  val[0][0][1][ 9][26] =   1.154288e-03; eval[0][0][1][ 9][26] =   1.772461e-04;
  val[0][0][1][ 9][27] =   1.480916e-03; eval[0][0][1][ 9][27] =   1.876059e-04;
  val[0][0][1][ 9][28] =   1.069180e-03; eval[0][0][1][ 9][28] =   1.693111e-04;
  val[0][0][1][ 9][29] =   1.355231e-03; eval[0][0][1][ 9][29] =   2.997194e-04;
  //pc3dphi_mean_c1d0z0
  n_val[0][1][0][0] = 30;
  val[0][1][0][ 0][ 0] =   0.000000e+00; eval[0][1][0][ 0][ 0] =   0.000000e+00;
  val[0][1][0][ 0][ 1] =   0.000000e+00; eval[0][1][0][ 0][ 1] =   0.000000e+00;
  val[0][1][0][ 0][ 2] =   1.413821e-03; eval[0][1][0][ 0][ 2] =   9.248283e-07;
  val[0][1][0][ 0][ 3] =   4.258998e-04; eval[0][1][0][ 0][ 3] =   8.691735e-07;
  val[0][1][0][ 0][ 4] =  -4.590894e-05; eval[0][1][0][ 0][ 4] =   9.771834e-07;
  val[0][1][0][ 0][ 5] =  -4.190192e-04; eval[0][1][0][ 0][ 5] =   1.052772e-06;
  val[0][1][0][ 0][ 6] =  -5.355664e-04; eval[0][1][0][ 0][ 6] =   1.451573e-06;
  val[0][1][0][ 0][ 7] =  -6.004385e-04; eval[0][1][0][ 0][ 7] =   1.820769e-06;
  val[0][1][0][ 0][ 8] =  -6.690282e-04; eval[0][1][0][ 0][ 8] =   2.363568e-06;
  val[0][1][0][ 0][ 9] =  -6.992173e-04; eval[0][1][0][ 0][ 9] =   3.070531e-06;
  val[0][1][0][ 0][10] =  -7.429147e-04; eval[0][1][0][ 0][10] =   4.100403e-06;
  val[0][1][0][ 0][11] =  -7.917246e-04; eval[0][1][0][ 0][11] =   5.432009e-06;
  val[0][1][0][ 0][12] =  -6.987727e-04; eval[0][1][0][ 0][12] =   1.032542e-05;
  val[0][1][0][ 0][13] =  -7.665533e-04; eval[0][1][0][ 0][13] =   1.238268e-05;
  val[0][1][0][ 0][14] =  -7.977308e-04; eval[0][1][0][ 0][14] =   1.671512e-05;
  val[0][1][0][ 0][15] =  -9.816045e-04; eval[0][1][0][ 0][15] =   1.250928e-05;
  val[0][1][0][ 0][16] =  -1.010087e-03; eval[0][1][0][ 0][16] =   1.619040e-05;
  val[0][1][0][ 0][17] =  -1.028099e-03; eval[0][1][0][ 0][17] =   2.005510e-05;
  val[0][1][0][ 0][18] =  -1.024863e-03; eval[0][1][0][ 0][18] =   2.465618e-05;
  val[0][1][0][ 0][19] =  -1.026374e-03; eval[0][1][0][ 0][19] =   2.961816e-05;
  val[0][1][0][ 0][20] =  -1.070783e-03; eval[0][1][0][ 0][20] =   2.749898e-05;
  val[0][1][0][ 0][21] =  -1.084075e-03; eval[0][1][0][ 0][21] =   3.958714e-05;
  val[0][1][0][ 0][22] =  -1.096998e-03; eval[0][1][0][ 0][22] =   5.368099e-05;
  val[0][1][0][ 0][23] =  -1.108750e-03; eval[0][1][0][ 0][23] =   6.915360e-05;
  val[0][1][0][ 0][24] =  -1.172228e-03; eval[0][1][0][ 0][24] =   8.878523e-05;
  val[0][1][0][ 0][25] =  -1.406220e-03; eval[0][1][0][ 0][25] =   1.184642e-04;
  val[0][1][0][ 0][26] =  -1.237045e-03; eval[0][1][0][ 0][26] =   1.409277e-04;
  val[0][1][0][ 0][27] =  -1.588145e-03; eval[0][1][0][ 0][27] =   1.667513e-04;
  val[0][1][0][ 0][28] =  -1.114070e-03; eval[0][1][0][ 0][28] =   4.429928e-04;
  val[0][1][0][ 0][29] =   1.663057e-01; eval[0][1][0][ 0][29] =   4.650010e+00;
  //pc3dphi_mean_c1d0z1
  n_val[0][1][0][1] = 30;
  val[0][1][0][ 1][ 0] =   0.000000e+00; eval[0][1][0][ 1][ 0] =   0.000000e+00;
  val[0][1][0][ 1][ 1] =   0.000000e+00; eval[0][1][0][ 1][ 1] =   0.000000e+00;
  val[0][1][0][ 1][ 2] =   2.573775e-04; eval[0][1][0][ 1][ 2] =   9.901852e-07;
  val[0][1][0][ 1][ 3] =  -2.701288e-04; eval[0][1][0][ 1][ 3] =   8.809820e-07;
  val[0][1][0][ 1][ 4] =  -2.575014e-04; eval[0][1][0][ 1][ 4] =   2.732997e-06;
  val[0][1][0][ 1][ 5] =  -8.067113e-04; eval[0][1][0][ 1][ 5] =   9.853209e-07;
  val[0][1][0][ 1][ 6] =  -8.759306e-04; eval[0][1][0][ 1][ 6] =   1.174933e-06;
  val[0][1][0][ 1][ 7] =  -8.738241e-04; eval[0][1][0][ 1][ 7] =   1.867856e-06;
  val[0][1][0][ 1][ 8] =  -8.739165e-04; eval[0][1][0][ 1][ 8] =   2.996487e-06;
  val[0][1][0][ 1][ 9] =  -8.765470e-04; eval[0][1][0][ 1][ 9] =   3.910762e-06;
  val[0][1][0][ 1][10] =  -9.915947e-04; eval[0][1][0][ 1][10] =   4.213511e-06;
  val[0][1][0][ 1][11] =  -1.026604e-03; eval[0][1][0][ 1][11] =   5.569540e-06;
  val[0][1][0][ 1][12] =  -1.079726e-03; eval[0][1][0][ 1][12] =   5.605621e-06;
  val[0][1][0][ 1][13] =  -1.106007e-03; eval[0][1][0][ 1][13] =   7.421659e-06;
  val[0][1][0][ 1][14] =  -1.052854e-03; eval[0][1][0][ 1][14] =   1.232173e-05;
  val[0][1][0][ 1][15] =  -1.108393e-03; eval[0][1][0][ 1][15] =   1.483298e-05;
  val[0][1][0][ 1][16] =  -1.073710e-03; eval[0][1][0][ 1][16] =   1.985667e-05;
  val[0][1][0][ 1][17] =  -1.055878e-03; eval[0][1][0][ 1][17] =   2.464766e-05;
  val[0][1][0][ 1][18] =  -1.148255e-03; eval[0][1][0][ 1][18] =   2.920350e-05;
  val[0][1][0][ 1][19] =  -1.164170e-03; eval[0][1][0][ 1][19] =   3.501297e-05;
  val[0][1][0][ 1][20] =  -1.118201e-03; eval[0][1][0][ 1][20] =   3.488176e-05;
  val[0][1][0][ 1][21] =  -1.127965e-03; eval[0][1][0][ 1][21] =   4.740659e-05;
  val[0][1][0][ 1][22] =  -1.194184e-03; eval[0][1][0][ 1][22] =   5.131885e-05;
  val[0][1][0][ 1][23] =  -1.093354e-03; eval[0][1][0][ 1][23] =   7.239368e-05;
  val[0][1][0][ 1][24] =  -1.270433e-03; eval[0][1][0][ 1][24] =   8.482381e-05;
  val[0][1][0][ 1][25] =  -1.234318e-03; eval[0][1][0][ 1][25] =   1.177784e-04;
  val[0][1][0][ 1][26] =  -1.353725e-03; eval[0][1][0][ 1][26] =   1.494700e-04;
  val[0][1][0][ 1][27] =  -1.214275e-03; eval[0][1][0][ 1][27] =   1.814421e-04;
  val[0][1][0][ 1][28] =  -8.874983e-04; eval[0][1][0][ 1][28] =   2.191662e-04;
  val[0][1][0][ 1][29] =  -9.319205e-04; eval[0][1][0][ 1][29] =   3.279291e-04;
  //pc3dphi_mean_c1d0z2
  n_val[0][1][0][2] = 30;
  val[0][1][0][ 2][ 0] =   0.000000e+00; eval[0][1][0][ 2][ 0] =   0.000000e+00;
  val[0][1][0][ 2][ 1] =   0.000000e+00; eval[0][1][0][ 2][ 1] =   0.000000e+00;
  val[0][1][0][ 2][ 2] =  -7.204510e-04; eval[0][1][0][ 2][ 2] =   1.147969e-06;
  val[0][1][0][ 2][ 3] =  -8.596843e-04; eval[0][1][0][ 2][ 3] =   1.075912e-06;
  val[0][1][0][ 2][ 4] =  -9.294082e-04; eval[0][1][0][ 2][ 4] =   1.782020e-06;
  val[0][1][0][ 2][ 5] =  -1.098894e-03; eval[0][1][0][ 2][ 5] =   9.536527e-07;
  val[0][1][0][ 2][ 6] =  -1.081062e-03; eval[0][1][0][ 2][ 6] =   1.564996e-06;
  val[0][1][0][ 2][ 7] =  -1.095555e-03; eval[0][1][0][ 2][ 7] =   1.909319e-06;
  val[0][1][0][ 2][ 8] =  -1.131143e-03; eval[0][1][0][ 2][ 8] =   2.397944e-06;
  val[0][1][0][ 2][ 9] =  -1.117510e-03; eval[0][1][0][ 2][ 9] =   3.181838e-06;
  val[0][1][0][ 2][10] =  -1.134152e-03; eval[0][1][0][ 2][10] =   4.135407e-06;
  val[0][1][0][ 2][11] =  -1.145285e-03; eval[0][1][0][ 2][11] =   5.426083e-06;
  val[0][1][0][ 2][12] =  -1.182465e-03; eval[0][1][0][ 2][12] =   8.263994e-06;
  val[0][1][0][ 2][13] =  -1.186105e-03; eval[0][1][0][ 2][13] =   9.224229e-06;
  val[0][1][0][ 2][14] =  -1.173301e-03; eval[0][1][0][ 2][14] =   1.161139e-05;
  val[0][1][0][ 2][15] =  -1.196237e-03; eval[0][1][0][ 2][15] =   1.422401e-05;
  val[0][1][0][ 2][16] =  -1.148966e-03; eval[0][1][0][ 2][16] =   2.005450e-05;
  val[0][1][0][ 2][17] =  -1.234026e-03; eval[0][1][0][ 2][17] =   2.775407e-05;
  val[0][1][0][ 2][18] =  -1.223114e-03; eval[0][1][0][ 2][18] =   2.767395e-05;
  val[0][1][0][ 2][19] =  -1.230122e-03; eval[0][1][0][ 2][19] =   3.852024e-05;
  val[0][1][0][ 2][20] =  -1.199216e-03; eval[0][1][0][ 2][20] =   3.211324e-05;
  val[0][1][0][ 2][21] =  -1.298426e-03; eval[0][1][0][ 2][21] =   4.301154e-05;
  val[0][1][0][ 2][22] =  -1.257132e-03; eval[0][1][0][ 2][22] =   5.132671e-05;
  val[0][1][0][ 2][23] =  -1.202927e-03; eval[0][1][0][ 2][23] =   6.916609e-05;
  val[0][1][0][ 2][24] =  -1.190385e-03; eval[0][1][0][ 2][24] =   8.488203e-05;
  val[0][1][0][ 2][25] =  -1.379308e-03; eval[0][1][0][ 2][25] =   1.210561e-04;
  val[0][1][0][ 2][26] =  -1.148499e-03; eval[0][1][0][ 2][26] =   1.516300e-04;
  val[0][1][0][ 2][27] =  -1.468376e-03; eval[0][1][0][ 2][27] =   1.983098e-04;
  val[0][1][0][ 2][28] =  -1.422964e-03; eval[0][1][0][ 2][28] =   1.954878e-04;
  val[0][1][0][ 2][29] =  -1.186404e-03; eval[0][1][0][ 2][29] =   2.190002e-04;
  //pc3dphi_mean_c1d0z3
  n_val[0][1][0][3] = 30;
  val[0][1][0][ 3][ 0] =   0.000000e+00; eval[0][1][0][ 3][ 0] =   0.000000e+00;
  val[0][1][0][ 3][ 1] =   0.000000e+00; eval[0][1][0][ 3][ 1] =   0.000000e+00;
  val[0][1][0][ 3][ 2] =  -1.218702e-03; eval[0][1][0][ 3][ 2] =   1.383593e-06;
  val[0][1][0][ 3][ 3] =  -1.122939e-03; eval[0][1][0][ 3][ 3] =   1.206564e-06;
  val[0][1][0][ 3][ 4] =  -1.341176e-03; eval[0][1][0][ 3][ 4] =   1.913850e-06;
  val[0][1][0][ 3][ 5] =  -1.277527e-03; eval[0][1][0][ 3][ 5] =   9.566606e-07;
  val[0][1][0][ 3][ 6] =  -1.273298e-03; eval[0][1][0][ 3][ 6] =   1.433040e-06;
  val[0][1][0][ 3][ 7] =  -1.271334e-03; eval[0][1][0][ 3][ 7] =   1.793938e-06;
  val[0][1][0][ 3][ 8] =  -1.285682e-03; eval[0][1][0][ 3][ 8] =   2.285380e-06;
  val[0][1][0][ 3][ 9] =  -1.267638e-03; eval[0][1][0][ 3][ 9] =   3.033019e-06;
  val[0][1][0][ 3][10] =  -1.277945e-03; eval[0][1][0][ 3][10] =   3.942889e-06;
  val[0][1][0][ 3][11] =  -1.295947e-03; eval[0][1][0][ 3][11] =   5.125090e-06;
  val[0][1][0][ 3][12] =  -1.368101e-03; eval[0][1][0][ 3][12] =   8.487765e-06;
  val[0][1][0][ 3][13] =  -1.327177e-03; eval[0][1][0][ 3][13] =   8.549966e-06;
  val[0][1][0][ 3][14] =  -1.415512e-03; eval[0][1][0][ 3][14] =   1.442354e-05;
  val[0][1][0][ 3][15] =  -1.443327e-03; eval[0][1][0][ 3][15] =   1.943276e-05;
  val[0][1][0][ 3][16] =  -1.342682e-03; eval[0][1][0][ 3][16] =   1.771771e-05;
  val[0][1][0][ 3][17] =  -1.349928e-03; eval[0][1][0][ 3][17] =   2.069724e-05;
  val[0][1][0][ 3][18] =  -1.496607e-03; eval[0][1][0][ 3][18] =   3.691140e-05;
  val[0][1][0][ 3][19] =  -1.414215e-03; eval[0][1][0][ 3][19] =   3.003565e-05;
  val[0][1][0][ 3][20] =  -1.411730e-03; eval[0][1][0][ 3][20] =   3.974700e-05;
  val[0][1][0][ 3][21] =  -1.434419e-03; eval[0][1][0][ 3][21] =   4.067839e-05;
  val[0][1][0][ 3][22] =  -1.467444e-03; eval[0][1][0][ 3][22] =   5.548473e-05;
  val[0][1][0][ 3][23] =  -1.322461e-03; eval[0][1][0][ 3][23] =   6.579326e-05;
  val[0][1][0][ 3][24] =  -1.445422e-03; eval[0][1][0][ 3][24] =   9.391803e-05;
  val[0][1][0][ 3][25] =  -1.531012e-03; eval[0][1][0][ 3][25] =   1.325613e-04;
  val[0][1][0][ 3][26] =  -1.607944e-03; eval[0][1][0][ 3][26] =   1.688016e-04;
  val[0][1][0][ 3][27] =  -1.327993e-03; eval[0][1][0][ 3][27] =   1.808731e-04;
  val[0][1][0][ 3][28] =  -1.981664e-03; eval[0][1][0][ 3][28] =   2.748974e-04;
  val[0][1][0][ 3][29] =  -1.547223e-03; eval[0][1][0][ 3][29] =   2.288216e-04;
  //pc3dphi_mean_c1d0z4
  n_val[0][1][0][4] = 30;
  val[0][1][0][ 4][ 0] =   0.000000e+00; eval[0][1][0][ 4][ 0] =   0.000000e+00;
  val[0][1][0][ 4][ 1] =   0.000000e+00; eval[0][1][0][ 4][ 1] =   0.000000e+00;
  val[0][1][0][ 4][ 2] =  -1.601146e-03; eval[0][1][0][ 4][ 2] =   1.180957e-06;
  val[0][1][0][ 4][ 3] =  -1.348513e-03; eval[0][1][0][ 4][ 3] =   1.107461e-06;
  val[0][1][0][ 4][ 4] =  -1.561713e-03; eval[0][1][0][ 4][ 4] =   2.319305e-06;
  val[0][1][0][ 4][ 5] =  -1.393248e-03; eval[0][1][0][ 4][ 5] =   1.280307e-06;
  val[0][1][0][ 4][ 6] =  -1.401755e-03; eval[0][1][0][ 4][ 6] =   1.477792e-06;
  val[0][1][0][ 4][ 7] =  -1.407068e-03; eval[0][1][0][ 4][ 7] =   1.848941e-06;
  val[0][1][0][ 4][ 8] =  -1.417122e-03; eval[0][1][0][ 4][ 8] =   2.336303e-06;
  val[0][1][0][ 4][ 9] =  -1.397550e-03; eval[0][1][0][ 4][ 9] =   3.105416e-06;
  val[0][1][0][ 4][10] =  -1.448657e-03; eval[0][1][0][ 4][10] =   5.468070e-06;
  val[0][1][0][ 4][11] =  -1.457696e-03; eval[0][1][0][ 4][11] =   7.200881e-06;
  val[0][1][0][ 4][12] =  -1.465331e-03; eval[0][1][0][ 4][12] =   9.105821e-06;
  val[0][1][0][ 4][13] =  -1.482434e-03; eval[0][1][0][ 4][13] =   1.280554e-05;
  val[0][1][0][ 4][14] =  -1.412837e-03; eval[0][1][0][ 4][14] =   1.122455e-05;
  val[0][1][0][ 4][15] =  -1.521411e-03; eval[0][1][0][ 4][15] =   2.048041e-05;
  val[0][1][0][ 4][16] =  -1.472085e-03; eval[0][1][0][ 4][16] =   2.537481e-05;
  val[0][1][0][ 4][17] =  -1.424027e-03; eval[0][1][0][ 4][17] =   2.831311e-05;
  val[0][1][0][ 4][18] =  -1.482822e-03; eval[0][1][0][ 4][18] =   2.691765e-05;
  val[0][1][0][ 4][19] =  -1.495233e-03; eval[0][1][0][ 4][19] =   4.766021e-05;
  val[0][1][0][ 4][20] =  -1.391950e-03; eval[0][1][0][ 4][20] =   3.804823e-05;
  val[0][1][0][ 4][21] =  -1.475701e-03; eval[0][1][0][ 4][21] =   5.845257e-05;
  val[0][1][0][ 4][22] =  -1.493259e-03; eval[0][1][0][ 4][22] =   5.887896e-05;
  val[0][1][0][ 4][23] =  -1.673933e-03; eval[0][1][0][ 4][23] =   7.469453e-05;
  val[0][1][0][ 4][24] =  -1.374241e-03; eval[0][1][0][ 4][24] =   1.023187e-04;
  val[0][1][0][ 4][25] =  -1.418385e-03; eval[0][1][0][ 4][25] =   1.419723e-04;
  val[0][1][0][ 4][26] =  -1.669895e-03; eval[0][1][0][ 4][26] =   1.480592e-04;
  val[0][1][0][ 4][27] =  -1.896773e-03; eval[0][1][0][ 4][27] =   4.460173e-04;
  val[0][1][0][ 4][28] =  -1.783539e-03; eval[0][1][0][ 4][28] =   2.267403e-04;
  val[0][1][0][ 4][29] =  -1.678731e-03; eval[0][1][0][ 4][29] =   2.859412e-04;
  //pc3dphi_mean_c1d0z5
  n_val[0][1][0][5] = 30;
  val[0][1][0][ 5][ 0] =   0.000000e+00; eval[0][1][0][ 5][ 0] =   0.000000e+00;
  val[0][1][0][ 5][ 1] =   0.000000e+00; eval[0][1][0][ 5][ 1] =   0.000000e+00;
  val[0][1][0][ 5][ 2] =  -9.381429e-04; eval[0][1][0][ 5][ 2] =   1.948143e-06;
  val[0][1][0][ 5][ 3] =  -9.939284e-04; eval[0][1][0][ 5][ 3] =   9.316679e-07;
  val[0][1][0][ 5][ 4] =  -9.236591e-04; eval[0][1][0][ 5][ 4] =   2.480858e-06;
  val[0][1][0][ 5][ 5] =  -9.846339e-04; eval[0][1][0][ 5][ 5] =   1.272877e-06;
  val[0][1][0][ 5][ 6] =  -9.806797e-04; eval[0][1][0][ 5][ 6] =   1.546437e-06;
  val[0][1][0][ 5][ 7] =  -9.732533e-04; eval[0][1][0][ 5][ 7] =   2.576358e-06;
  val[0][1][0][ 5][ 8] =  -9.263227e-04; eval[0][1][0][ 5][ 8] =   4.038074e-06;
  val[0][1][0][ 5][ 9] =  -9.275851e-04; eval[0][1][0][ 5][ 9] =   5.078232e-06;
  val[0][1][0][ 5][10] =  -1.074758e-03; eval[0][1][0][ 5][10] =   4.276363e-06;
  val[0][1][0][ 5][11] =  -1.097865e-03; eval[0][1][0][ 5][11] =   5.622632e-06;
  val[0][1][0][ 5][12] =  -1.015910e-03; eval[0][1][0][ 5][12] =   9.890797e-06;
  val[0][1][0][ 5][13] =  -1.146751e-03; eval[0][1][0][ 5][13] =   9.699419e-06;
  val[0][1][0][ 5][14] =  -1.101594e-03; eval[0][1][0][ 5][14] =   1.604031e-05;
  val[0][1][0][ 5][15] =  -1.182581e-03; eval[0][1][0][ 5][15] =   1.969618e-05;
  val[0][1][0][ 5][16] =  -1.120856e-03; eval[0][1][0][ 5][16] =   2.769756e-05;
  val[0][1][0][ 5][17] =  -1.073883e-03; eval[0][1][0][ 5][17] =   3.279594e-05;
  val[0][1][0][ 5][18] =  -1.161547e-03; eval[0][1][0][ 5][18] =   3.567965e-05;
  val[0][1][0][ 5][19] =  -1.251074e-03; eval[0][1][0][ 5][19] =   4.168590e-05;
  val[0][1][0][ 5][20] =  -1.119318e-03; eval[0][1][0][ 5][20] =   4.637951e-05;
  val[0][1][0][ 5][21] =  -1.283784e-03; eval[0][1][0][ 5][21] =   5.971756e-05;
  val[0][1][0][ 5][22] =  -1.270042e-03; eval[0][1][0][ 5][22] =   6.747151e-05;
  val[0][1][0][ 5][23] =  -1.420622e-03; eval[0][1][0][ 5][23] =   7.870041e-05;
  val[0][1][0][ 5][24] =  -1.124580e-03; eval[0][1][0][ 5][24] =   1.263409e-04;
  val[0][1][0][ 5][25] =  -1.274766e-03; eval[0][1][0][ 5][25] =   1.486416e-04;
  val[0][1][0][ 5][26] =  -1.308354e-03; eval[0][1][0][ 5][26] =   2.038678e-04;
  val[0][1][0][ 5][27] =  -1.602962e-03; eval[0][1][0][ 5][27] =   2.365819e-04;
  val[0][1][0][ 5][28] =  -1.719335e-03; eval[0][1][0][ 5][28] =   5.212446e-04;
  val[0][1][0][ 5][29] =  -1.043449e-03; eval[0][1][0][ 5][29] =   4.825995e-04;
  //pc3dphi_mean_c1d0z6
  n_val[0][1][0][6] = 30;
  val[0][1][0][ 6][ 0] =   0.000000e+00; eval[0][1][0][ 6][ 0] =   0.000000e+00;
  val[0][1][0][ 6][ 1] =   0.000000e+00; eval[0][1][0][ 6][ 1] =   0.000000e+00;
  val[0][1][0][ 6][ 2] =  -9.354992e-04; eval[0][1][0][ 6][ 2] =   7.787824e-07;
  val[0][1][0][ 6][ 3] =  -9.627238e-04; eval[0][1][0][ 6][ 3] =   1.195438e-06;
  val[0][1][0][ 6][ 4] =  -1.021545e-03; eval[0][1][0][ 6][ 4] =   2.027449e-06;
  val[0][1][0][ 6][ 5] =  -9.669543e-04; eval[0][1][0][ 6][ 5] =   1.041386e-06;
  val[0][1][0][ 6][ 6] =  -9.797691e-04; eval[0][1][0][ 6][ 6] =   1.229049e-06;
  val[0][1][0][ 6][ 7] =  -1.000682e-03; eval[0][1][0][ 6][ 7] =   2.021853e-06;
  val[0][1][0][ 6][ 8] =  -1.001182e-03; eval[0][1][0][ 6][ 8] =   2.542119e-06;
  val[0][1][0][ 6][ 9] =  -1.019794e-03; eval[0][1][0][ 6][ 9] =   2.585624e-06;
  val[0][1][0][ 6][10] =  -9.926381e-04; eval[0][1][0][ 6][10] =   4.447086e-06;
  val[0][1][0][ 6][11] =  -1.042477e-03; eval[0][1][0][ 6][11] =   6.257044e-06;
  val[0][1][0][ 6][12] =  -1.041714e-03; eval[0][1][0][ 6][12] =   7.968325e-06;
  val[0][1][0][ 6][13] =  -1.070846e-03; eval[0][1][0][ 6][13] =   9.660683e-06;
  val[0][1][0][ 6][14] =  -1.082493e-03; eval[0][1][0][ 6][14] =   1.314572e-05;
  val[0][1][0][ 6][15] =  -1.151318e-03; eval[0][1][0][ 6][15] =   1.505505e-05;
  val[0][1][0][ 6][16] =  -1.120648e-03; eval[0][1][0][ 6][16] =   2.045037e-05;
  val[0][1][0][ 6][17] =  -1.123930e-03; eval[0][1][0][ 6][17] =   2.706947e-05;
  val[0][1][0][ 6][18] =  -1.176464e-03; eval[0][1][0][ 6][18] =   2.914230e-05;
  val[0][1][0][ 6][19] =  -1.169581e-03; eval[0][1][0][ 6][19] =   3.616128e-05;
  val[0][1][0][ 6][20] =  -1.140072e-03; eval[0][1][0][ 6][20] =   3.828551e-05;
  val[0][1][0][ 6][21] =  -1.216209e-03; eval[0][1][0][ 6][21] =   4.967780e-05;
  val[0][1][0][ 6][22] =  -1.335386e-03; eval[0][1][0][ 6][22] =   5.302176e-05;
  val[0][1][0][ 6][23] =  -1.261998e-03; eval[0][1][0][ 6][23] =   6.733119e-05;
  val[0][1][0][ 6][24] =  -1.171538e-03; eval[0][1][0][ 6][24] =   9.067649e-05;
  val[0][1][0][ 6][25] =  -1.511038e-03; eval[0][1][0][ 6][25] =   1.211455e-04;
  val[0][1][0][ 6][26] =  -1.565427e-03; eval[0][1][0][ 6][26] =   1.366038e-04;
  val[0][1][0][ 6][27] =  -1.063416e-03; eval[0][1][0][ 6][27] =   1.923384e-04;
  val[0][1][0][ 6][28] =  -1.239613e-03; eval[0][1][0][ 6][28] =   2.524680e-04;
  val[0][1][0][ 6][29] =  -1.358434e-03; eval[0][1][0][ 6][29] =   2.761709e-04;
  //pc3dphi_mean_c1d0z7
  n_val[0][1][0][7] = 30;
  val[0][1][0][ 7][ 0] =   0.000000e+00; eval[0][1][0][ 7][ 0] =   0.000000e+00;
  val[0][1][0][ 7][ 1] =   0.000000e+00; eval[0][1][0][ 7][ 1] =   0.000000e+00;
  val[0][1][0][ 7][ 2] =  -5.876799e-04; eval[0][1][0][ 7][ 2] =   1.026981e-06;
  val[0][1][0][ 7][ 3] =  -8.516103e-04; eval[0][1][0][ 7][ 3] =   1.058363e-06;
  val[0][1][0][ 7][ 4] =  -1.015139e-03; eval[0][1][0][ 7][ 4] =   1.848645e-06;
  val[0][1][0][ 7][ 5] =  -1.052811e-03; eval[0][1][0][ 7][ 5] =   1.010266e-06;
  val[0][1][0][ 7][ 6] =  -1.080061e-03; eval[0][1][0][ 7][ 6] =   1.568850e-06;
  val[0][1][0][ 7][ 7] =  -1.161456e-03; eval[0][1][0][ 7][ 7] =   2.230974e-06;
  val[0][1][0][ 7][ 8] =  -1.107622e-03; eval[0][1][0][ 7][ 8] =   2.342336e-06;
  val[0][1][0][ 7][ 9] =  -1.122177e-03; eval[0][1][0][ 7][ 9] =   2.990155e-06;
  val[0][1][0][ 7][10] =  -1.170498e-03; eval[0][1][0][ 7][10] =   3.810472e-06;
  val[0][1][0][ 7][11] =  -1.230625e-03; eval[0][1][0][ 7][11] =   4.851417e-06;
  val[0][1][0][ 7][12] =  -1.247555e-03; eval[0][1][0][ 7][12] =   7.542211e-06;
  val[0][1][0][ 7][13] =  -1.257753e-03; eval[0][1][0][ 7][13] =   8.237777e-06;
  val[0][1][0][ 7][14] =  -1.292395e-03; eval[0][1][0][ 7][14] =   1.251092e-05;
  val[0][1][0][ 7][15] =  -1.316449e-03; eval[0][1][0][ 7][15] =   1.292069e-05;
  val[0][1][0][ 7][16] =  -1.382896e-03; eval[0][1][0][ 7][16] =   2.276769e-05;
  val[0][1][0][ 7][17] =  -1.284491e-03; eval[0][1][0][ 7][17] =   2.093442e-05;
  val[0][1][0][ 7][18] =  -1.341866e-03; eval[0][1][0][ 7][18] =   2.494668e-05;
  val[0][1][0][ 7][19] =  -1.286999e-03; eval[0][1][0][ 7][19] =   3.180895e-05;
  val[0][1][0][ 7][20] =  -1.321054e-03; eval[0][1][0][ 7][20] =   3.510749e-05;
  val[0][1][0][ 7][21] =  -1.355243e-03; eval[0][1][0][ 7][21] =   5.082438e-05;
  val[0][1][0][ 7][22] =  -1.342601e-03; eval[0][1][0][ 7][22] =   4.821335e-05;
  val[0][1][0][ 7][23] =  -1.252930e-03; eval[0][1][0][ 7][23] =   6.965068e-05;
  val[0][1][0][ 7][24] =  -1.315679e-03; eval[0][1][0][ 7][24] =   1.008494e-04;
  val[0][1][0][ 7][25] =  -1.305005e-03; eval[0][1][0][ 7][25] =   1.171681e-04;
  val[0][1][0][ 7][26] =  -1.648574e-03; eval[0][1][0][ 7][26] =   1.308282e-04;
  val[0][1][0][ 7][27] =  -1.372042e-03; eval[0][1][0][ 7][27] =   1.843909e-04;
  val[0][1][0][ 7][28] =  -1.676149e-03; eval[0][1][0][ 7][28] =   1.802645e-04;
  val[0][1][0][ 7][29] =  -1.126030e-03; eval[0][1][0][ 7][29] =   2.836436e-04;
  //pc3dphi_mean_c1d0z8
  n_val[0][1][0][8] = 30;
  val[0][1][0][ 8][ 0] =   0.000000e+00; eval[0][1][0][ 8][ 0] =   0.000000e+00;
  val[0][1][0][ 8][ 1] =   0.000000e+00; eval[0][1][0][ 8][ 1] =   0.000000e+00;
  val[0][1][0][ 8][ 2] =  -5.066153e-05; eval[0][1][0][ 8][ 2] =   8.354006e-07;
  val[0][1][0][ 8][ 3] =  -7.224491e-04; eval[0][1][0][ 8][ 3] =   9.242312e-07;
  val[0][1][0][ 8][ 4] =  -1.016111e-03; eval[0][1][0][ 8][ 4] =   1.631329e-06;
  val[0][1][0][ 8][ 5] =  -1.136134e-03; eval[0][1][0][ 8][ 5] =   9.814535e-07;
  val[0][1][0][ 8][ 6] =  -1.234270e-03; eval[0][1][0][ 8][ 6] =   1.478788e-06;
  val[0][1][0][ 8][ 7] =  -1.292795e-03; eval[0][1][0][ 8][ 7] =   1.728350e-06;
  val[0][1][0][ 8][ 8] =  -1.372762e-03; eval[0][1][0][ 8][ 8] =   2.852040e-06;
  val[0][1][0][ 8][ 9] =  -1.391449e-03; eval[0][1][0][ 8][ 9] =   3.716543e-06;
  val[0][1][0][ 8][10] =  -1.442800e-03; eval[0][1][0][ 8][10] =   4.828659e-06;
  val[0][1][0][ 8][11] =  -1.485959e-03; eval[0][1][0][ 8][11] =   6.290959e-06;
  val[0][1][0][ 8][12] =  -1.390090e-03; eval[0][1][0][ 8][12] =   5.766665e-06;
  val[0][1][0][ 8][13] =  -1.535776e-03; eval[0][1][0][ 8][13] =   1.068140e-05;
  val[0][1][0][ 8][14] =  -1.463145e-03; eval[0][1][0][ 8][14] =   9.359166e-06;
  val[0][1][0][ 8][15] =  -1.514889e-03; eval[0][1][0][ 8][15] =   1.163932e-05;
  val[0][1][0][ 8][16] =  -1.585084e-03; eval[0][1][0][ 8][16] =   2.225778e-05;
  val[0][1][0][ 8][17] =  -1.510541e-03; eval[0][1][0][ 8][17] =   1.823175e-05;
  val[0][1][0][ 8][18] =  -1.536476e-03; eval[0][1][0][ 8][18] =   2.164595e-05;
  val[0][1][0][ 8][19] =  -1.576575e-03; eval[0][1][0][ 8][19] =   2.769835e-05;
  val[0][1][0][ 8][20] =  -1.488213e-03; eval[0][1][0][ 8][20] =   2.819368e-05;
  val[0][1][0][ 8][21] =  -1.480573e-03; eval[0][1][0][ 8][21] =   4.759458e-05;
  val[0][1][0][ 8][22] =  -1.561099e-03; eval[0][1][0][ 8][22] =   5.116687e-05;
  val[0][1][0][ 8][23] =  -1.638521e-03; eval[0][1][0][ 8][23] =   6.344735e-05;
  val[0][1][0][ 8][24] =  -1.461308e-03; eval[0][1][0][ 8][24] =   8.772442e-05;
  val[0][1][0][ 8][25] =  -1.598255e-03; eval[0][1][0][ 8][25] =   1.232487e-04;
  val[0][1][0][ 8][26] =  -1.404065e-03; eval[0][1][0][ 8][26] =   1.480262e-04;
  val[0][1][0][ 8][27] =  -1.796471e-03; eval[0][1][0][ 8][27] =   1.541265e-04;
  val[0][1][0][ 8][28] =  -1.511544e-03; eval[0][1][0][ 8][28] =   2.433475e-04;
  val[0][1][0][ 8][29] =  -1.605045e-03; eval[0][1][0][ 8][29] =   2.353036e-04;
  //pc3dphi_mean_c1d0z9
  n_val[0][1][0][9] = 30;
  val[0][1][0][ 9][ 0] =   0.000000e+00; eval[0][1][0][ 9][ 0] =   0.000000e+00;
  val[0][1][0][ 9][ 1] =   0.000000e+00; eval[0][1][0][ 9][ 1] =   0.000000e+00;
  val[0][1][0][ 9][ 2] =   6.955977e-04; eval[0][1][0][ 9][ 2] =   1.029298e-06;
  val[0][1][0][ 9][ 3] =  -3.947325e-04; eval[0][1][0][ 9][ 3] =   8.206760e-07;
  val[0][1][0][ 9][ 4] =  -8.520927e-04; eval[0][1][0][ 9][ 4] =   1.562515e-06;
  val[0][1][0][ 9][ 5] =  -1.098254e-03; eval[0][1][0][ 9][ 5] =   9.890244e-07;
  val[0][1][0][ 9][ 6] =  -1.263424e-03; eval[0][1][0][ 9][ 6] =   1.503920e-06;
  val[0][1][0][ 9][ 7] =  -1.381482e-03; eval[0][1][0][ 9][ 7] =   1.733277e-06;
  val[0][1][0][ 9][ 8] =  -1.381451e-03; eval[0][1][0][ 9][ 8] =   2.147839e-06;
  val[0][1][0][ 9][ 9] =  -1.428195e-03; eval[0][1][0][ 9][ 9] =   2.744264e-06;
  val[0][1][0][ 9][10] =  -1.479738e-03; eval[0][1][0][ 9][10] =   3.545507e-06;
  val[0][1][0][ 9][11] =  -1.521452e-03; eval[0][1][0][ 9][11] =   4.517293e-06;
  val[0][1][0][ 9][12] =  -1.520489e-03; eval[0][1][0][ 9][12] =   5.893911e-06;
  val[0][1][0][ 9][13] =  -1.586356e-03; eval[0][1][0][ 9][13] =   7.461680e-06;
  val[0][1][0][ 9][14] =  -1.649092e-03; eval[0][1][0][ 9][14] =   1.500887e-05;
  val[0][1][0][ 9][15] =  -1.671335e-03; eval[0][1][0][ 9][15] =   1.810629e-05;
  val[0][1][0][ 9][16] =  -1.641314e-03; eval[0][1][0][ 9][16] =   1.526278e-05;
  val[0][1][0][ 9][17] =  -1.573643e-03; eval[0][1][0][ 9][17] =   1.939069e-05;
  val[0][1][0][ 9][18] =  -1.707770e-03; eval[0][1][0][ 9][18] =   3.754304e-05;
  val[0][1][0][ 9][19] =  -1.629548e-03; eval[0][1][0][ 9][19] =   2.690273e-05;
  val[0][1][0][ 9][20] =  -1.693909e-03; eval[0][1][0][ 9][20] =   2.739821e-05;
  val[0][1][0][ 9][21] =  -1.623617e-03; eval[0][1][0][ 9][21] =   5.880965e-05;
  val[0][1][0][ 9][22] =  -1.591480e-03; eval[0][1][0][ 9][22] =   4.907465e-05;
  val[0][1][0][ 9][23] =  -1.742143e-03; eval[0][1][0][ 9][23] =   7.185122e-05;
  val[0][1][0][ 9][24] =  -1.693759e-03; eval[0][1][0][ 9][24] =   9.025563e-05;
  val[0][1][0][ 9][25] =  -1.542956e-03; eval[0][1][0][ 9][25] =   9.918580e-05;
  val[0][1][0][ 9][26] =  -1.551871e-03; eval[0][1][0][ 9][26] =   1.589119e-04;
  val[0][1][0][ 9][27] =  -1.458211e-03; eval[0][1][0][ 9][27] =   1.914167e-04;
  val[0][1][0][ 9][28] =  -1.493955e-03; eval[0][1][0][ 9][28] =   2.089267e-04;
  val[0][1][0][ 9][29] =  -1.667129e-03; eval[0][1][0][ 9][29] =   3.192706e-04;
  //pc3dphi_mean_c1d1z0
  n_val[0][1][1][0] = 30;
  val[0][1][1][ 0][ 0] =   0.000000e+00; eval[0][1][1][ 0][ 0] =   0.000000e+00;
  val[0][1][1][ 0][ 1] =   0.000000e+00; eval[0][1][1][ 0][ 1] =   0.000000e+00;
  val[0][1][1][ 0][ 2] =   2.535544e-03; eval[0][1][1][ 0][ 2] =   7.342830e-07;
  val[0][1][1][ 0][ 3] =   1.766721e-03; eval[0][1][1][ 0][ 3] =   5.636909e-07;
  val[0][1][1][ 0][ 4] =   1.364208e-03; eval[0][1][1][ 0][ 4] =   7.110522e-07;
  val[0][1][1][ 0][ 5] =   1.206546e-03; eval[0][1][1][ 0][ 5] =   8.238943e-07;
  val[0][1][1][ 0][ 6] =   1.115338e-03; eval[0][1][1][ 0][ 6] =   1.033077e-06;
  val[0][1][1][ 0][ 7] =   1.020558e-03; eval[0][1][1][ 0][ 7] =   1.331971e-06;
  val[0][1][1][ 0][ 8] =   9.562548e-04; eval[0][1][1][ 0][ 8] =   1.751507e-06;
  val[0][1][1][ 0][ 9] =   9.246023e-04; eval[0][1][1][ 0][ 9] =   2.312178e-06;
  val[0][1][1][ 0][10] =   8.998694e-04; eval[0][1][1][ 0][10] =   3.023296e-06;
  val[0][1][1][ 0][11] =   8.967422e-04; eval[0][1][1][ 0][11] =   3.894207e-06;
  val[0][1][1][ 0][12] =   9.024607e-04; eval[0][1][1][ 0][12] =   5.089353e-06;
  val[0][1][1][ 0][13] =   9.021476e-04; eval[0][1][1][ 0][13] =   6.508806e-06;
  val[0][1][1][ 0][14] =   9.129521e-04; eval[0][1][1][ 0][14] =   8.237982e-06;
  val[0][1][1][ 0][15] =   8.861117e-04; eval[0][1][1][ 0][15] =   1.035892e-05;
  val[0][1][1][ 0][16] =   8.861338e-04; eval[0][1][1][ 0][16] =   1.301674e-05;
  val[0][1][1][ 0][17] =   8.647802e-04; eval[0][1][1][ 0][17] =   1.600660e-05;
  val[0][1][1][ 0][18] =   8.576694e-04; eval[0][1][1][ 0][18] =   1.975294e-05;
  val[0][1][1][ 0][19] =   8.422728e-04; eval[0][1][1][ 0][19] =   2.328629e-05;
  val[0][1][1][ 0][20] =   8.474915e-04; eval[0][1][1][ 0][20] =   2.230630e-05;
  val[0][1][1][ 0][21] =   8.246775e-04; eval[0][1][1][ 0][21] =   2.960405e-05;
  val[0][1][1][ 0][22] =   7.319113e-04; eval[0][1][1][ 0][22] =   3.992817e-05;
  val[0][1][1][ 0][23] =   8.705864e-04; eval[0][1][1][ 0][23] =   5.339286e-05;
  val[0][1][1][ 0][24] =   8.720336e-04; eval[0][1][1][ 0][24] =   7.896090e-05;
  val[0][1][1][ 0][25] =   7.296619e-04; eval[0][1][1][ 0][25] =   1.019253e-04;
  val[0][1][1][ 0][26] =   8.086979e-04; eval[0][1][1][ 0][26] =   1.103875e-04;
  val[0][1][1][ 0][27] =   3.750657e-04; eval[0][1][1][ 0][27] =   1.783306e-04;
  val[0][1][1][ 0][28] =   8.966916e-04; eval[0][1][1][ 0][28] =   1.504317e-04;
  val[0][1][1][ 0][29] =  -2.020294e-02; eval[0][1][1][ 0][29] =   2.387476e-01;
  //pc3dphi_mean_c1d1z1
  n_val[0][1][1][1] = 30;
  val[0][1][1][ 1][ 0] =   0.000000e+00; eval[0][1][1][ 1][ 0] =   0.000000e+00;
  val[0][1][1][ 1][ 1] =   0.000000e+00; eval[0][1][1][ 1][ 1] =   0.000000e+00;
  val[0][1][1][ 1][ 2] =   1.804150e-03; eval[0][1][1][ 1][ 2] =   6.213660e-07;
  val[0][1][1][ 1][ 3] =   1.313674e-03; eval[0][1][1][ 1][ 3] =   6.199002e-07;
  val[0][1][1][ 1][ 4] =   1.129533e-03; eval[0][1][1][ 1][ 4] =   6.303189e-07;
  val[0][1][1][ 1][ 5] =   1.045791e-03; eval[0][1][1][ 1][ 5] =   7.665979e-07;
  val[0][1][1][ 1][ 6] =   9.966095e-04; eval[0][1][1][ 1][ 6] =   9.552564e-07;
  val[0][1][1][ 1][ 7] =   9.511693e-04; eval[0][1][1][ 1][ 7] =   1.233249e-06;
  val[0][1][1][ 1][ 8] =   9.175551e-04; eval[0][1][1][ 1][ 8] =   1.601793e-06;
  val[0][1][1][ 1][ 9] =   8.989857e-04; eval[0][1][1][ 1][ 9] =   2.111634e-06;
  val[0][1][1][ 1][10] =   8.814714e-04; eval[0][1][1][ 1][10] =   2.766075e-06;
  val[0][1][1][ 1][11] =   8.816207e-04; eval[0][1][1][ 1][11] =   3.597009e-06;
  val[0][1][1][ 1][12] =   8.850540e-04; eval[0][1][1][ 1][12] =   4.661063e-06;
  val[0][1][1][ 1][13] =   8.891569e-04; eval[0][1][1][ 1][13] =   6.072280e-06;
  val[0][1][1][ 1][14] =   8.666084e-04; eval[0][1][1][ 1][14] =   7.743698e-06;
  val[0][1][1][ 1][15] =   8.494054e-04; eval[0][1][1][ 1][15] =   9.840111e-06;
  val[0][1][1][ 1][16] =   8.788171e-04; eval[0][1][1][ 1][16] =   1.237901e-05;
  val[0][1][1][ 1][17] =   8.481664e-04; eval[0][1][1][ 1][17] =   1.547598e-05;
  val[0][1][1][ 1][18] =   7.792280e-04; eval[0][1][1][ 1][18] =   1.889339e-05;
  val[0][1][1][ 1][19] =   8.561466e-04; eval[0][1][1][ 1][19] =   2.296494e-05;
  val[0][1][1][ 1][20] =   8.513238e-04; eval[0][1][1][ 1][20] =   2.055094e-05;
  val[0][1][1][ 1][21] =   8.041989e-04; eval[0][1][1][ 1][21] =   3.020377e-05;
  val[0][1][1][ 1][22] =   7.812375e-04; eval[0][1][1][ 1][22] =   3.997508e-05;
  val[0][1][1][ 1][23] =   8.462203e-04; eval[0][1][1][ 1][23] =   5.073935e-05;
  val[0][1][1][ 1][24] =   7.561483e-04; eval[0][1][1][ 1][24] =   7.028283e-05;
  val[0][1][1][ 1][25] =   6.853161e-04; eval[0][1][1][ 1][25] =   7.947037e-05;
  val[0][1][1][ 1][26] =   7.882576e-04; eval[0][1][1][ 1][26] =   9.785834e-05;
  val[0][1][1][ 1][27] =   9.218957e-04; eval[0][1][1][ 1][27] =   1.279327e-04;
  val[0][1][1][ 1][28] =   7.816362e-04; eval[0][1][1][ 1][28] =   1.776352e-04;
  val[0][1][1][ 1][29] =   9.001964e-04; eval[0][1][1][ 1][29] =   1.362796e-04;
  //pc3dphi_mean_c1d1z2
  n_val[0][1][1][2] = 30;
  val[0][1][1][ 2][ 0] =   0.000000e+00; eval[0][1][1][ 2][ 0] =   0.000000e+00;
  val[0][1][1][ 2][ 1] =   0.000000e+00; eval[0][1][1][ 2][ 1] =   0.000000e+00;
  val[0][1][1][ 2][ 2] =   1.095368e-03; eval[0][1][1][ 2][ 2] =   6.682074e-07;
  val[0][1][1][ 2][ 3] =   1.005048e-03; eval[0][1][1][ 2][ 3] =   5.363054e-07;
  val[0][1][1][ 2][ 4] =   9.291328e-04; eval[0][1][1][ 2][ 4] =   6.340880e-07;
  val[0][1][1][ 2][ 5] =   8.967847e-04; eval[0][1][1][ 2][ 5] =   7.654445e-07;
  val[0][1][1][ 2][ 6] =   8.863123e-04; eval[0][1][1][ 2][ 6] =   9.632603e-07;
  val[0][1][1][ 2][ 7] =   8.564508e-04; eval[0][1][1][ 2][ 7] =   1.257177e-06;
  val[0][1][1][ 2][ 8] =   8.353236e-04; eval[0][1][1][ 2][ 8] =   1.635615e-06;
  val[0][1][1][ 2][ 9] =   8.344680e-04; eval[0][1][1][ 2][ 9] =   2.174919e-06;
  val[0][1][1][ 2][10] =   8.385730e-04; eval[0][1][1][ 2][10] =   2.830669e-06;
  val[0][1][1][ 2][11] =   8.493693e-04; eval[0][1][1][ 2][11] =   3.651160e-06;
  val[0][1][1][ 2][12] =   8.685756e-04; eval[0][1][1][ 2][12] =   4.731039e-06;
  val[0][1][1][ 2][13] =   8.588066e-04; eval[0][1][1][ 2][13] =   6.049340e-06;
  val[0][1][1][ 2][14] =   8.612336e-04; eval[0][1][1][ 2][14] =   7.674602e-06;
  val[0][1][1][ 2][15] =   8.417826e-04; eval[0][1][1][ 2][15] =   9.582258e-06;
  val[0][1][1][ 2][16] =   8.284218e-04; eval[0][1][1][ 2][16] =   1.190856e-05;
  val[0][1][1][ 2][17] =   8.529167e-04; eval[0][1][1][ 2][17] =   1.475006e-05;
  val[0][1][1][ 2][18] =   8.179755e-04; eval[0][1][1][ 2][18] =   1.912935e-05;
  val[0][1][1][ 2][19] =   8.574596e-04; eval[0][1][1][ 2][19] =   2.216807e-05;
  val[0][1][1][ 2][20] =   8.140781e-04; eval[0][1][1][ 2][20] =   2.002162e-05;
  val[0][1][1][ 2][21] =   7.516139e-04; eval[0][1][1][ 2][21] =   2.899741e-05;
  val[0][1][1][ 2][22] =   8.014841e-04; eval[0][1][1][ 2][22] =   4.303235e-05;
  val[0][1][1][ 2][23] =   7.599586e-04; eval[0][1][1][ 2][23] =   5.370057e-05;
  val[0][1][1][ 2][24] =   8.165218e-04; eval[0][1][1][ 2][24] =   6.373924e-05;
  val[0][1][1][ 2][25] =   7.035365e-04; eval[0][1][1][ 2][25] =   9.247889e-05;
  val[0][1][1][ 2][26] =   8.578082e-04; eval[0][1][1][ 2][26] =   1.124651e-04;
  val[0][1][1][ 2][27] =   5.314769e-04; eval[0][1][1][ 2][27] =   1.542839e-04;
  val[0][1][1][ 2][28] =   5.823610e-04; eval[0][1][1][ 2][28] =   1.751936e-04;
  val[0][1][1][ 2][29] =   4.921860e-04; eval[0][1][1][ 2][29] =   2.320573e-04;
  //pc3dphi_mean_c1d1z3
  n_val[0][1][1][3] = 30;
  val[0][1][1][ 3][ 0] =   0.000000e+00; eval[0][1][1][ 3][ 0] =   0.000000e+00;
  val[0][1][1][ 3][ 1] =   0.000000e+00; eval[0][1][1][ 3][ 1] =   0.000000e+00;
  val[0][1][1][ 3][ 2] =   5.938371e-04; eval[0][1][1][ 3][ 2] =   7.579229e-07;
  val[0][1][1][ 3][ 3] =   8.276385e-04; eval[0][1][1][ 3][ 3] =   6.078367e-07;
  val[0][1][1][ 3][ 4] =   8.293992e-04; eval[0][1][1][ 3][ 4] =   6.462355e-07;
  val[0][1][1][ 3][ 5] =   8.387813e-04; eval[0][1][1][ 3][ 5] =   7.662241e-07;
  val[0][1][1][ 3][ 6] =   8.286912e-04; eval[0][1][1][ 3][ 6] =   9.600527e-07;
  val[0][1][1][ 3][ 7] =   8.168716e-04; eval[0][1][1][ 3][ 7] =   1.247929e-06;
  val[0][1][1][ 3][ 8] =   8.123831e-04; eval[0][1][1][ 3][ 8] =   1.623602e-06;
  val[0][1][1][ 3][ 9] =   8.178328e-04; eval[0][1][1][ 3][ 9] =   2.176363e-06;
  val[0][1][1][ 3][10] =   8.243645e-04; eval[0][1][1][ 3][10] =   2.864266e-06;
  val[0][1][1][ 3][11] =   8.379149e-04; eval[0][1][1][ 3][11] =   3.775922e-06;
  val[0][1][1][ 3][12] =   8.419025e-04; eval[0][1][1][ 3][12] =   4.970929e-06;
  val[0][1][1][ 3][13] =   8.432051e-04; eval[0][1][1][ 3][13] =   6.420722e-06;
  val[0][1][1][ 3][14] =   8.498617e-04; eval[0][1][1][ 3][14] =   8.157010e-06;
  val[0][1][1][ 3][15] =   8.368948e-04; eval[0][1][1][ 3][15] =   1.020247e-05;
  val[0][1][1][ 3][16] =   8.460978e-04; eval[0][1][1][ 3][16] =   1.299188e-05;
  val[0][1][1][ 3][17] =   8.149992e-04; eval[0][1][1][ 3][17] =   1.620000e-05;
  val[0][1][1][ 3][18] =   7.812977e-04; eval[0][1][1][ 3][18] =   2.044752e-05;
  val[0][1][1][ 3][19] =   8.313634e-04; eval[0][1][1][ 3][19] =   2.303302e-05;
  val[0][1][1][ 3][20] =   7.845679e-04; eval[0][1][1][ 3][20] =   2.224527e-05;
  val[0][1][1][ 3][21] =   7.546134e-04; eval[0][1][1][ 3][21] =   3.086739e-05;
  val[0][1][1][ 3][22] =   7.656597e-04; eval[0][1][1][ 3][22] =   4.171396e-05;
  val[0][1][1][ 3][23] =   7.411346e-04; eval[0][1][1][ 3][23] =   5.521968e-05;
  val[0][1][1][ 3][24] =   8.091214e-04; eval[0][1][1][ 3][24] =   6.978452e-05;
  val[0][1][1][ 3][25] =   6.512749e-04; eval[0][1][1][ 3][25] =   9.737796e-05;
  val[0][1][1][ 3][26] =   6.876573e-04; eval[0][1][1][ 3][26] =   9.706551e-05;
  val[0][1][1][ 3][27] =   7.812324e-04; eval[0][1][1][ 3][27] =   1.003925e-04;
  val[0][1][1][ 3][28] =   7.149434e-04; eval[0][1][1][ 3][28] =   1.360198e-04;
  val[0][1][1][ 3][29] =   4.462172e-04; eval[0][1][1][ 3][29] =   2.053442e-04;
  //pc3dphi_mean_c1d1z4
  n_val[0][1][1][4] = 30;
  val[0][1][1][ 4][ 0] =   0.000000e+00; eval[0][1][1][ 4][ 0] =   0.000000e+00;
  val[0][1][1][ 4][ 1] =   0.000000e+00; eval[0][1][1][ 4][ 1] =   0.000000e+00;
  val[0][1][1][ 4][ 2] =   3.601142e-04; eval[0][1][1][ 4][ 2] =   7.800495e-07;
  val[0][1][1][ 4][ 3] =   7.248192e-04; eval[0][1][1][ 4][ 3] =   7.033703e-07;
  val[0][1][1][ 4][ 4] =   7.570544e-04; eval[0][1][1][ 4][ 4] =   7.581649e-07;
  val[0][1][1][ 4][ 5] =   7.681741e-04; eval[0][1][1][ 4][ 5] =   8.988144e-07;
  val[0][1][1][ 4][ 6] =   7.555788e-04; eval[0][1][1][ 4][ 6] =   1.149901e-06;
  val[0][1][1][ 4][ 7] =   7.615140e-04; eval[0][1][1][ 4][ 7] =   1.458779e-06;
  val[0][1][1][ 4][ 8] =   7.654651e-04; eval[0][1][1][ 4][ 8] =   1.871081e-06;
  val[0][1][1][ 4][ 9] =   7.743101e-04; eval[0][1][1][ 4][ 9] =   2.516083e-06;
  val[0][1][1][ 4][10] =   7.802711e-04; eval[0][1][1][ 4][10] =   3.372079e-06;
  val[0][1][1][ 4][11] =   8.041803e-04; eval[0][1][1][ 4][11] =   4.447726e-06;
  val[0][1][1][ 4][12] =   8.264339e-04; eval[0][1][1][ 4][12] =   5.705054e-06;
  val[0][1][1][ 4][13] =   8.258552e-04; eval[0][1][1][ 4][13] =   7.480977e-06;
  val[0][1][1][ 4][14] =   8.080550e-04; eval[0][1][1][ 4][14] =   9.468414e-06;
  val[0][1][1][ 4][15] =   7.878301e-04; eval[0][1][1][ 4][15] =   1.237743e-05;
  val[0][1][1][ 4][16] =   7.800691e-04; eval[0][1][1][ 4][16] =   1.534011e-05;
  val[0][1][1][ 4][17] =   8.122039e-04; eval[0][1][1][ 4][17] =   1.832679e-05;
  val[0][1][1][ 4][18] =   7.906091e-04; eval[0][1][1][ 4][18] =   2.247800e-05;
  val[0][1][1][ 4][19] =   7.619586e-04; eval[0][1][1][ 4][19] =   2.739885e-05;
  val[0][1][1][ 4][20] =   7.361772e-04; eval[0][1][1][ 4][20] =   2.516829e-05;
  val[0][1][1][ 4][21] =   6.791845e-04; eval[0][1][1][ 4][21] =   3.613436e-05;
  val[0][1][1][ 4][22] =   7.796822e-04; eval[0][1][1][ 4][22] =   4.919905e-05;
  val[0][1][1][ 4][23] =   7.478280e-04; eval[0][1][1][ 4][23] =   7.163046e-05;
  val[0][1][1][ 4][24] =   8.614144e-04; eval[0][1][1][ 4][24] =   7.825542e-05;
  val[0][1][1][ 4][25] =   4.817140e-04; eval[0][1][1][ 4][25] =   1.364102e-04;
  val[0][1][1][ 4][26] =   6.225954e-04; eval[0][1][1][ 4][26] =   1.246331e-04;
  val[0][1][1][ 4][27] =   5.338141e-04; eval[0][1][1][ 4][27] =   1.529369e-04;
  val[0][1][1][ 4][28] =   6.487574e-04; eval[0][1][1][ 4][28] =   2.161182e-04;
  val[0][1][1][ 4][29] =   1.182020e-03; eval[0][1][1][ 4][29] =   2.170925e-04;
  //pc3dphi_mean_c1d1z5
  n_val[0][1][1][5] = 30;
  val[0][1][1][ 5][ 0] =   0.000000e+00; eval[0][1][1][ 5][ 0] =   0.000000e+00;
  val[0][1][1][ 5][ 1] =   0.000000e+00; eval[0][1][1][ 5][ 1] =   0.000000e+00;
  val[0][1][1][ 5][ 2] =   1.695464e-04; eval[0][1][1][ 5][ 2] =   7.838127e-07;
  val[0][1][1][ 5][ 3] =   2.924847e-04; eval[0][1][1][ 5][ 3] =   6.779267e-07;
  val[0][1][1][ 5][ 4] =   4.638979e-04; eval[0][1][1][ 5][ 4] =   8.666745e-07;
  val[0][1][1][ 5][ 5] =   3.811211e-04; eval[0][1][1][ 5][ 5] =   8.326373e-07;
  val[0][1][1][ 5][ 6] =   3.772246e-04; eval[0][1][1][ 5][ 6] =   1.045109e-06;
  val[0][1][1][ 5][ 7] =   4.848917e-04; eval[0][1][1][ 5][ 7] =   1.538471e-06;
  val[0][1][1][ 5][ 8] =   5.137764e-04; eval[0][1][1][ 5][ 8] =   1.962332e-06;
  val[0][1][1][ 5][ 9] =   5.311558e-04; eval[0][1][1][ 5][ 9] =   2.637266e-06;
  val[0][1][1][ 5][10] =   5.501145e-04; eval[0][1][1][ 5][10] =   3.570941e-06;
  val[0][1][1][ 5][11] =   5.631857e-04; eval[0][1][1][ 5][11] =   4.681458e-06;
  val[0][1][1][ 5][12] =   5.778040e-04; eval[0][1][1][ 5][12] =   6.097561e-06;
  val[0][1][1][ 5][13] =   5.872143e-04; eval[0][1][1][ 5][13] =   7.786845e-06;
  val[0][1][1][ 5][14] =   5.762083e-04; eval[0][1][1][ 5][14] =   9.964075e-06;
  val[0][1][1][ 5][15] =   5.730727e-04; eval[0][1][1][ 5][15] =   1.259732e-05;
  val[0][1][1][ 5][16] =   5.917887e-04; eval[0][1][1][ 5][16] =   1.533596e-05;
  val[0][1][1][ 5][17] =   5.880428e-04; eval[0][1][1][ 5][17] =   1.846902e-05;
  val[0][1][1][ 5][18] =   5.515164e-04; eval[0][1][1][ 5][18] =   2.394366e-05;
  val[0][1][1][ 5][19] =   5.034892e-04; eval[0][1][1][ 5][19] =   2.989237e-05;
  val[0][1][1][ 5][20] =   5.397868e-04; eval[0][1][1][ 5][20] =   2.514907e-05;
  val[0][1][1][ 5][21] =   5.677934e-04; eval[0][1][1][ 5][21] =   3.478357e-05;
  val[0][1][1][ 5][22] =   5.007905e-04; eval[0][1][1][ 5][22] =   4.985290e-05;
  val[0][1][1][ 5][23] =   4.898053e-04; eval[0][1][1][ 5][23] =   5.924539e-05;
  val[0][1][1][ 5][24] =   4.902078e-04; eval[0][1][1][ 5][24] =   8.144124e-05;
  val[0][1][1][ 5][25] =   4.115772e-04; eval[0][1][1][ 5][25] =   1.172823e-04;
  val[0][1][1][ 5][26] =   4.356529e-04; eval[0][1][1][ 5][26] =   1.339440e-04;
  val[0][1][1][ 5][27] =   2.941983e-04; eval[0][1][1][ 5][27] =   1.621064e-04;
  val[0][1][1][ 5][28] =   5.024433e-04; eval[0][1][1][ 5][28] =   1.877870e-04;
  val[0][1][1][ 5][29] =  -1.508354e-04; eval[0][1][1][ 5][29] =   2.612585e-04;
  //pc3dphi_mean_c1d1z6
  n_val[0][1][1][6] = 30;
  val[0][1][1][ 6][ 0] =   0.000000e+00; eval[0][1][1][ 6][ 0] =   0.000000e+00;
  val[0][1][1][ 6][ 1] =   0.000000e+00; eval[0][1][1][ 6][ 1] =   0.000000e+00;
  val[0][1][1][ 6][ 2] =   5.729989e-04; eval[0][1][1][ 6][ 2] =   7.604295e-07;
  val[0][1][1][ 6][ 3] =   6.626957e-04; eval[0][1][1][ 6][ 3] =   6.556025e-07;
  val[0][1][1][ 6][ 4] =   6.119944e-04; eval[0][1][1][ 6][ 4] =   7.274103e-07;
  val[0][1][1][ 6][ 5] =   5.292764e-04; eval[0][1][1][ 6][ 5] =   7.704679e-07;
  val[0][1][1][ 6][ 6] =   5.964683e-04; eval[0][1][1][ 6][ 6] =   1.048729e-06;
  val[0][1][1][ 6][ 7] =   6.046016e-04; eval[0][1][1][ 6][ 7] =   1.336194e-06;
  val[0][1][1][ 6][ 8] =   6.148926e-04; eval[0][1][1][ 6][ 8] =   1.721121e-06;
  val[0][1][1][ 6][ 9] =   6.368124e-04; eval[0][1][1][ 6][ 9] =   2.315029e-06;
  val[0][1][1][ 6][10] =   6.411452e-04; eval[0][1][1][ 6][10] =   3.143871e-06;
  val[0][1][1][ 6][11] =   6.645057e-04; eval[0][1][1][ 6][11] =   4.116793e-06;
  val[0][1][1][ 6][12] =   6.383258e-04; eval[0][1][1][ 6][12] =   5.484262e-06;
  val[0][1][1][ 6][13] =   6.536813e-04; eval[0][1][1][ 6][13] =   7.064867e-06;
  val[0][1][1][ 6][14] =   6.360229e-04; eval[0][1][1][ 6][14] =   9.080144e-06;
  val[0][1][1][ 6][15] =   6.064820e-04; eval[0][1][1][ 6][15] =   1.119156e-05;
  val[0][1][1][ 6][16] =   6.133856e-04; eval[0][1][1][ 6][16] =   1.441780e-05;
  val[0][1][1][ 6][17] =   6.219636e-04; eval[0][1][1][ 6][17] =   1.761343e-05;
  val[0][1][1][ 6][18] =   5.812692e-04; eval[0][1][1][ 6][18] =   2.161500e-05;
  val[0][1][1][ 6][19] =   5.934772e-04; eval[0][1][1][ 6][19] =   2.543960e-05;
  val[0][1][1][ 6][20] =   5.671551e-04; eval[0][1][1][ 6][20] =   2.444700e-05;
  val[0][1][1][ 6][21] =   5.336591e-04; eval[0][1][1][ 6][21] =   3.924759e-05;
  val[0][1][1][ 6][22] =   5.964646e-04; eval[0][1][1][ 6][22] =   4.554057e-05;
  val[0][1][1][ 6][23] =   5.247702e-04; eval[0][1][1][ 6][23] =   5.721193e-05;
  val[0][1][1][ 6][24] =   4.638790e-04; eval[0][1][1][ 6][24] =   7.509567e-05;
  val[0][1][1][ 6][25] =   5.579161e-04; eval[0][1][1][ 6][25] =   8.795683e-05;
  val[0][1][1][ 6][26] =   5.449225e-04; eval[0][1][1][ 6][26] =   1.153083e-04;
  val[0][1][1][ 6][27] =   2.922420e-04; eval[0][1][1][ 6][27] =   1.597719e-04;
  val[0][1][1][ 6][28] =   7.005681e-04; eval[0][1][1][ 6][28] =   1.419389e-04;
  val[0][1][1][ 6][29] =  -3.276480e-05; eval[0][1][1][ 6][29] =   2.368638e-04;
  //pc3dphi_mean_c1d1z7
  n_val[0][1][1][7] = 30;
  val[0][1][1][ 7][ 0] =   0.000000e+00; eval[0][1][1][ 7][ 0] =   0.000000e+00;
  val[0][1][1][ 7][ 1] =   0.000000e+00; eval[0][1][1][ 7][ 1] =   0.000000e+00;
  val[0][1][1][ 7][ 2] =   1.248279e-03; eval[0][1][1][ 7][ 2] =   6.984205e-07;
  val[0][1][1][ 7][ 3] =   9.667036e-04; eval[0][1][1][ 7][ 3] =   5.701024e-07;
  val[0][1][1][ 7][ 4] =   8.464610e-04; eval[0][1][1][ 7][ 4] =   6.382040e-07;
  val[0][1][1][ 7][ 5] =   8.019453e-04; eval[0][1][1][ 7][ 5] =   7.659550e-07;
  val[0][1][1][ 7][ 6] =   7.204565e-04; eval[0][1][1][ 7][ 6] =   9.835427e-07;
  val[0][1][1][ 7][ 7] =   7.073148e-04; eval[0][1][1][ 7][ 7] =   1.269276e-06;
  val[0][1][1][ 7][ 8] =   7.251860e-04; eval[0][1][1][ 7][ 8] =   1.617703e-06;
  val[0][1][1][ 7][ 9] =   7.324395e-04; eval[0][1][1][ 7][ 9] =   2.173339e-06;
  val[0][1][1][ 7][10] =   7.125244e-04; eval[0][1][1][ 7][10] =   2.915460e-06;
  val[0][1][1][ 7][11] =   7.006313e-04; eval[0][1][1][ 7][11] =   3.912954e-06;
  val[0][1][1][ 7][12] =   6.637759e-04; eval[0][1][1][ 7][12] =   5.242716e-06;
  val[0][1][1][ 7][13] =   6.497258e-04; eval[0][1][1][ 7][13] =   6.887030e-06;
  val[0][1][1][ 7][14] =   6.131087e-04; eval[0][1][1][ 7][14] =   8.893602e-06;
  val[0][1][1][ 7][15] =   6.029212e-04; eval[0][1][1][ 7][15] =   1.124056e-05;
  val[0][1][1][ 7][16] =   6.022546e-04; eval[0][1][1][ 7][16] =   1.372162e-05;
  val[0][1][1][ 7][17] =   6.092992e-04; eval[0][1][1][ 7][17] =   1.708808e-05;
  val[0][1][1][ 7][18] =   5.744101e-04; eval[0][1][1][ 7][18] =   2.149796e-05;
  val[0][1][1][ 7][19] =   5.676207e-04; eval[0][1][1][ 7][19] =   2.622473e-05;
  val[0][1][1][ 7][20] =   5.922785e-04; eval[0][1][1][ 7][20] =   2.379864e-05;
  val[0][1][1][ 7][21] =   5.028926e-04; eval[0][1][1][ 7][21] =   3.583492e-05;
  val[0][1][1][ 7][22] =   4.587360e-04; eval[0][1][1][ 7][22] =   4.836272e-05;
  val[0][1][1][ 7][23] =   2.878628e-04; eval[0][1][1][ 7][23] =   5.991912e-05;
  val[0][1][1][ 7][24] =   3.045066e-04; eval[0][1][1][ 7][24] =   8.117259e-05;
  val[0][1][1][ 7][25] =   3.846919e-04; eval[0][1][1][ 7][25] =   1.112805e-04;
  val[0][1][1][ 7][26] =  -1.335918e-04; eval[0][1][1][ 7][26] =   1.451805e-04;
  val[0][1][1][ 7][27] =   1.944876e-05; eval[0][1][1][ 7][27] =   1.753684e-04;
  val[0][1][1][ 7][28] =   2.526461e-04; eval[0][1][1][ 7][28] =   2.629609e-04;
  val[0][1][1][ 7][29] =   4.385186e-04; eval[0][1][1][ 7][29] =   1.909909e-04;
  //pc3dphi_mean_c1d1z8
  n_val[0][1][1][8] = 30;
  val[0][1][1][ 8][ 0] =   0.000000e+00; eval[0][1][1][ 8][ 0] =   0.000000e+00;
  val[0][1][1][ 8][ 1] =   0.000000e+00; eval[0][1][1][ 8][ 1] =   0.000000e+00;
  val[0][1][1][ 8][ 2] =   2.088883e-03; eval[0][1][1][ 8][ 2] =   5.565499e-07;
  val[0][1][1][ 8][ 3] =   1.339155e-03; eval[0][1][1][ 8][ 3] =   6.127841e-07;
  val[0][1][1][ 8][ 4] =   1.107147e-03; eval[0][1][1][ 8][ 4] =   6.230775e-07;
  val[0][1][1][ 8][ 5] =   1.006820e-03; eval[0][1][1][ 8][ 5] =   7.641663e-07;
  val[0][1][1][ 8][ 6] =   8.860731e-04; eval[0][1][1][ 8][ 6] =   9.910483e-07;
  val[0][1][1][ 8][ 7] =   8.371706e-04; eval[0][1][1][ 8][ 7] =   1.287153e-06;
  val[0][1][1][ 8][ 8] =   8.224893e-04; eval[0][1][1][ 8][ 8] =   1.629239e-06;
  val[0][1][1][ 8][ 9] =   8.097135e-04; eval[0][1][1][ 8][ 9] =   2.186220e-06;
  val[0][1][1][ 8][10] =   7.794255e-04; eval[0][1][1][ 8][10] =   2.967473e-06;
  val[0][1][1][ 8][11] =   7.634153e-04; eval[0][1][1][ 8][11] =   3.930550e-06;
  val[0][1][1][ 8][12] =   7.320186e-04; eval[0][1][1][ 8][12] =   5.278843e-06;
  val[0][1][1][ 8][13] =   7.152403e-04; eval[0][1][1][ 8][13] =   6.809765e-06;
  val[0][1][1][ 8][14] =   6.916762e-04; eval[0][1][1][ 8][14] =   8.876380e-06;
  val[0][1][1][ 8][15] =   6.467922e-04; eval[0][1][1][ 8][15] =   1.147913e-05;
  val[0][1][1][ 8][16] =   6.446490e-04; eval[0][1][1][ 8][16] =   1.461071e-05;
  val[0][1][1][ 8][17] =   6.568398e-04; eval[0][1][1][ 8][17] =   1.753441e-05;
  val[0][1][1][ 8][18] =   6.792140e-04; eval[0][1][1][ 8][18] =   2.215639e-05;
  val[0][1][1][ 8][19] =   6.246176e-04; eval[0][1][1][ 8][19] =   2.590565e-05;
  val[0][1][1][ 8][20] =   6.209935e-04; eval[0][1][1][ 8][20] =   2.442170e-05;
  val[0][1][1][ 8][21] =   6.182474e-04; eval[0][1][1][ 8][21] =   3.429124e-05;
  val[0][1][1][ 8][22] =   4.838238e-04; eval[0][1][1][ 8][22] =   5.140375e-05;
  val[0][1][1][ 8][23] =   5.515704e-04; eval[0][1][1][ 8][23] =   5.484430e-05;
  val[0][1][1][ 8][24] =   3.071894e-04; eval[0][1][1][ 8][24] =   8.572512e-05;
  val[0][1][1][ 8][25] =   5.281336e-04; eval[0][1][1][ 8][25] =   8.730348e-05;
  val[0][1][1][ 8][26] =   7.149561e-04; eval[0][1][1][ 8][26] =   1.059114e-04;
  val[0][1][1][ 8][27] =   3.220656e-04; eval[0][1][1][ 8][27] =   2.494075e-04;
  val[0][1][1][ 8][28] =   5.722314e-04; eval[0][1][1][ 8][28] =   1.837093e-04;
  val[0][1][1][ 8][29] =  -3.321334e-03; eval[0][1][1][ 8][29] =   1.217815e-03;
  //pc3dphi_mean_c1d1z9
  n_val[0][1][1][9] = 30;
  val[0][1][1][ 9][ 0] =   0.000000e+00; eval[0][1][1][ 9][ 0] =   0.000000e+00;
  val[0][1][1][ 9][ 1] =   0.000000e+00; eval[0][1][1][ 9][ 1] =   0.000000e+00;
  val[0][1][1][ 9][ 2] =   2.917259e-03; eval[0][1][1][ 9][ 2] =   8.821377e-07;
  val[0][1][1][ 9][ 3] =   1.898063e-03; eval[0][1][1][ 9][ 3] =   5.381278e-07;
  val[0][1][1][ 9][ 4] =   1.439102e-03; eval[0][1][1][ 9][ 4] =   7.589784e-07;
  val[0][1][1][ 9][ 5] =   1.271574e-03; eval[0][1][1][ 9][ 5] =   8.759556e-07;
  val[0][1][1][ 9][ 6] =   1.091223e-03; eval[0][1][1][ 9][ 6] =   1.104371e-06;
  val[0][1][1][ 9][ 7] =   1.012447e-03; eval[0][1][1][ 9][ 7] =   1.444840e-06;
  val[0][1][1][ 9][ 8] =   9.887052e-04; eval[0][1][1][ 9][ 8] =   1.826733e-06;
  val[0][1][1][ 9][ 9] =   9.604640e-04; eval[0][1][1][ 9][ 9] =   2.463611e-06;
  val[0][1][1][ 9][10] =   9.268773e-04; eval[0][1][1][ 9][10] =   3.367597e-06;
  val[0][1][1][ 9][11] =   9.012006e-04; eval[0][1][1][ 9][11] =   4.407129e-06;
  val[0][1][1][ 9][12] =   8.654384e-04; eval[0][1][1][ 9][12] =   5.818149e-06;
  val[0][1][1][ 9][13] =   8.242826e-04; eval[0][1][1][ 9][13] =   7.470868e-06;
  val[0][1][1][ 9][14] =   8.154698e-04; eval[0][1][1][ 9][14] =   9.316525e-06;
  val[0][1][1][ 9][15] =   7.508055e-04; eval[0][1][1][ 9][15] =   1.184614e-05;
  val[0][1][1][ 9][16] =   7.507597e-04; eval[0][1][1][ 9][16] =   1.512800e-05;
  val[0][1][1][ 9][17] =   7.635836e-04; eval[0][1][1][ 9][17] =   1.825741e-05;
  val[0][1][1][ 9][18] =   7.492954e-04; eval[0][1][1][ 9][18] =   2.230776e-05;
  val[0][1][1][ 9][19] =   6.998770e-04; eval[0][1][1][ 9][19] =   2.827690e-05;
  val[0][1][1][ 9][20] =   7.041888e-04; eval[0][1][1][ 9][20] =   2.569231e-05;
  val[0][1][1][ 9][21] =   6.954855e-04; eval[0][1][1][ 9][21] =   3.725021e-05;
  val[0][1][1][ 9][22] =   5.553716e-04; eval[0][1][1][ 9][22] =   5.053799e-05;
  val[0][1][1][ 9][23] =   5.205018e-04; eval[0][1][1][ 9][23] =   7.076943e-05;
  val[0][1][1][ 9][24] =   7.116229e-04; eval[0][1][1][ 9][24] =   7.463464e-05;
  val[0][1][1][ 9][25] =   6.906060e-04; eval[0][1][1][ 9][25] =   9.292228e-05;
  val[0][1][1][ 9][26] =   3.920281e-04; eval[0][1][1][ 9][26] =   1.792229e-04;
  val[0][1][1][ 9][27] =   2.760145e-04; eval[0][1][1][ 9][27] =   1.676613e-04;
  val[0][1][1][ 9][28] =   5.375601e-04; eval[0][1][1][ 9][28] =   1.836582e-04;
  val[0][1][1][ 9][29] =   5.481366e-04; eval[0][1][1][ 9][29] =   2.111401e-04;
  //pc3dphi_sigma_c0d0z0
  n_val[1][0][0][0] = 30;
  val[1][0][0][ 0][ 0] =   0.000000e+00; eval[1][0][0][ 0][ 0] =   0.000000e+00;
  val[1][0][0][ 0][ 1] =   0.000000e+00; eval[1][0][0][ 0][ 1] =   0.000000e+00;
  val[1][0][0][ 0][ 2] =   3.298392e-03; eval[1][0][0][ 0][ 2] =   2.001956e-06;
  val[1][0][0][ 0][ 3] =   2.758450e-03; eval[1][0][0][ 0][ 3] =   1.257176e-06;
  val[1][0][0][ 0][ 4] =   2.867585e-03; eval[1][0][0][ 0][ 4] =   3.590269e-06;
  val[1][0][0][ 0][ 5] =   2.245599e-03; eval[1][0][0][ 0][ 5] =   7.697010e-07;
  val[1][0][0][ 0][ 6] =   2.069888e-03; eval[1][0][0][ 0][ 6] =   1.201733e-06;
  val[1][0][0][ 0][ 7] =   1.982955e-03; eval[1][0][0][ 0][ 7] =   1.365077e-06;
  val[1][0][0][ 0][ 8] =   1.961870e-03; eval[1][0][0][ 0][ 8] =   1.699014e-06;
  val[1][0][0][ 0][ 9] =   1.940034e-03; eval[1][0][0][ 0][ 9] =   2.204296e-06;
  val[1][0][0][ 0][10] =   1.907101e-03; eval[1][0][0][ 0][10] =   2.812076e-06;
  val[1][0][0][ 0][11] =   1.903597e-03; eval[1][0][0][ 0][11] =   3.674411e-06;
  val[1][0][0][ 0][12] =   1.879801e-03; eval[1][0][0][ 0][12] =   4.684579e-06;
  val[1][0][0][ 0][13] =   1.868461e-03; eval[1][0][0][ 0][13] =   6.012010e-06;
  val[1][0][0][ 0][14] =   1.853147e-03; eval[1][0][0][ 0][14] =   7.606461e-06;
  val[1][0][0][ 0][15] =   1.869646e-03; eval[1][0][0][ 0][15] =   9.889128e-06;
  val[1][0][0][ 0][16] =   1.852211e-03; eval[1][0][0][ 0][16] =   1.191501e-05;
  val[1][0][0][ 0][17] =   1.852715e-03; eval[1][0][0][ 0][17] =   1.475546e-05;
  val[1][0][0][ 0][18] =   1.883275e-03; eval[1][0][0][ 0][18] =   1.903145e-05;
  val[1][0][0][ 0][19] =   1.846436e-03; eval[1][0][0][ 0][19] =   2.206013e-05;
  val[1][0][0][ 0][20] =   1.796567e-03; eval[1][0][0][ 0][20] =   1.915360e-05;
  val[1][0][0][ 0][21] =   1.844408e-03; eval[1][0][0][ 0][21] =   2.882280e-05;
  val[1][0][0][ 0][22] =   1.858908e-03; eval[1][0][0][ 0][22] =   4.099430e-05;
  val[1][0][0][ 0][23] =   1.695152e-03; eval[1][0][0][ 0][23] =   4.245612e-05;
  val[1][0][0][ 0][24] =   1.766727e-03; eval[1][0][0][ 0][24] =   5.912746e-05;
  val[1][0][0][ 0][25] =   1.911749e-03; eval[1][0][0][ 0][25] =   9.084715e-05;
  val[1][0][0][ 0][26] =   1.734503e-03; eval[1][0][0][ 0][26] =   9.417455e-05;
  val[1][0][0][ 0][27] =   1.781256e-03; eval[1][0][0][ 0][27] =   1.115591e-04;
  val[1][0][0][ 0][28] =   2.031017e-03; eval[1][0][0][ 0][28] =   1.127833e-04;
  val[1][0][0][ 0][29] =   2.113760e-03; eval[1][0][0][ 0][29] =   1.594322e-04;
  //pc3dphi_sigma_c0d0z1
  n_val[1][0][0][1] = 30;
  val[1][0][0][ 1][ 0] =   0.000000e+00; eval[1][0][0][ 1][ 0] =   0.000000e+00;
  val[1][0][0][ 1][ 1] =   0.000000e+00; eval[1][0][0][ 1][ 1] =   0.000000e+00;
  val[1][0][0][ 1][ 2] =   3.259581e-03; eval[1][0][0][ 1][ 2] =   1.958153e-06;
  val[1][0][0][ 1][ 3] =   3.100287e-03; eval[1][0][0][ 1][ 3] =   3.828258e-06;
  val[1][0][0][ 1][ 4] =   2.701074e-03; eval[1][0][0][ 1][ 4] =   3.094869e-06;
  val[1][0][0][ 1][ 5] =   2.187724e-03; eval[1][0][0][ 1][ 5] =   7.511364e-07;
  val[1][0][0][ 1][ 6] =   2.021366e-03; eval[1][0][0][ 1][ 6] =   1.133325e-06;
  val[1][0][0][ 1][ 7] =   1.933253e-03; eval[1][0][0][ 1][ 7] =   1.312758e-06;
  val[1][0][0][ 1][ 8] =   1.904970e-03; eval[1][0][0][ 1][ 8] =   1.613984e-06;
  val[1][0][0][ 1][ 9] =   1.873749e-03; eval[1][0][0][ 1][ 9] =   2.057169e-06;
  val[1][0][0][ 1][10] =   1.843426e-03; eval[1][0][0][ 1][10] =   2.625538e-06;
  val[1][0][0][ 1][11] =   1.824326e-03; eval[1][0][0][ 1][11] =   3.367452e-06;
  val[1][0][0][ 1][12] =   1.824945e-03; eval[1][0][0][ 1][12] =   4.425133e-06;
  val[1][0][0][ 1][13] =   1.790703e-03; eval[1][0][0][ 1][13] =   5.503764e-06;
  val[1][0][0][ 1][14] =   1.806180e-03; eval[1][0][0][ 1][14] =   7.253525e-06;
  val[1][0][0][ 1][15] =   1.779964e-03; eval[1][0][0][ 1][15] =   8.906471e-06;
  val[1][0][0][ 1][16] =   1.795747e-03; eval[1][0][0][ 1][16] =   1.122626e-05;
  val[1][0][0][ 1][17] =   1.840578e-03; eval[1][0][0][ 1][17] =   1.484998e-05;
  val[1][0][0][ 1][18] =   1.805294e-03; eval[1][0][0][ 1][18] =   1.772851e-05;
  val[1][0][0][ 1][19] =   1.761986e-03; eval[1][0][0][ 1][19] =   2.015340e-05;
  val[1][0][0][ 1][20] =   1.792304e-03; eval[1][0][0][ 1][20] =   1.944943e-05;
  val[1][0][0][ 1][21] =   1.811029e-03; eval[1][0][0][ 1][21] =   2.832141e-05;
  val[1][0][0][ 1][22] =   1.739858e-03; eval[1][0][0][ 1][22] =   3.513421e-05;
  val[1][0][0][ 1][23] =   1.725054e-03; eval[1][0][0][ 1][23] =   4.456108e-05;
  val[1][0][0][ 1][24] =   1.917074e-03; eval[1][0][0][ 1][24] =   5.876341e-05;
  val[1][0][0][ 1][25] =   1.783314e-03; eval[1][0][0][ 1][25] =   7.278294e-05;
  val[1][0][0][ 1][26] =   1.704341e-03; eval[1][0][0][ 1][26] =   9.138164e-05;
  val[1][0][0][ 1][27] =   1.814592e-03; eval[1][0][0][ 1][27] =   1.222789e-04;
  val[1][0][0][ 1][28] =   1.776068e-03; eval[1][0][0][ 1][28] =   1.491384e-04;
  val[1][0][0][ 1][29] =   2.059877e-03; eval[1][0][0][ 1][29] =   1.362127e-04;
  //pc3dphi_sigma_c0d0z2
  n_val[1][0][0][2] = 30;
  val[1][0][0][ 2][ 0] =   0.000000e+00; eval[1][0][0][ 2][ 0] =   0.000000e+00;
  val[1][0][0][ 2][ 1] =   0.000000e+00; eval[1][0][0][ 2][ 1] =   0.000000e+00;
  val[1][0][0][ 2][ 2] =   3.074250e-03; eval[1][0][0][ 2][ 2] =   9.715060e-07;
  val[1][0][0][ 2][ 3] =   2.557775e-03; eval[1][0][0][ 2][ 3] =   1.022121e-06;
  val[1][0][0][ 2][ 4] =   2.674185e-03; eval[1][0][0][ 2][ 4] =   3.033697e-06;
  val[1][0][0][ 2][ 5] =   2.156350e-03; eval[1][0][0][ 2][ 5] =   7.375218e-07;
  val[1][0][0][ 2][ 6] =   1.996087e-03; eval[1][0][0][ 2][ 6] =   1.127835e-06;
  val[1][0][0][ 2][ 7] =   1.901866e-03; eval[1][0][0][ 2][ 7] =   1.298253e-06;
  val[1][0][0][ 2][ 8] =   1.870061e-03; eval[1][0][0][ 2][ 8] =   1.587147e-06;
  val[1][0][0][ 2][ 9] =   1.843745e-03; eval[1][0][0][ 2][ 9] =   2.015814e-06;
  val[1][0][0][ 2][10] =   1.811095e-03; eval[1][0][0][ 2][10] =   2.540111e-06;
  val[1][0][0][ 2][11] =   1.792260e-03; eval[1][0][0][ 2][11] =   3.244953e-06;
  val[1][0][0][ 2][12] =   1.788890e-03; eval[1][0][0][ 2][12] =   4.219825e-06;
  val[1][0][0][ 2][13] =   1.768489e-03; eval[1][0][0][ 2][13] =   5.352377e-06;
  val[1][0][0][ 2][14] =   1.785520e-03; eval[1][0][0][ 2][14] =   7.075955e-06;
  val[1][0][0][ 2][15] =   1.753099e-03; eval[1][0][0][ 2][15] =   8.592938e-06;
  val[1][0][0][ 2][16] =   1.732593e-03; eval[1][0][0][ 2][16] =   1.044550e-05;
  val[1][0][0][ 2][17] =   1.784373e-03; eval[1][0][0][ 2][17] =   1.402065e-05;
  val[1][0][0][ 2][18] =   1.758206e-03; eval[1][0][0][ 2][18] =   1.676974e-05;
  val[1][0][0][ 2][19] =   1.729260e-03; eval[1][0][0][ 2][19] =   1.954722e-05;
  val[1][0][0][ 2][20] =   1.718523e-03; eval[1][0][0][ 2][20] =   1.796358e-05;
  val[1][0][0][ 2][21] =   1.756694e-03; eval[1][0][0][ 2][21] =   2.645601e-05;
  val[1][0][0][ 2][22] =   1.718368e-03; eval[1][0][0][ 2][22] =   3.444180e-05;
  val[1][0][0][ 2][23] =   1.664701e-03; eval[1][0][0][ 2][23] =   4.253160e-05;
  val[1][0][0][ 2][24] =   1.721042e-03; eval[1][0][0][ 2][24] =   5.949934e-05;
  val[1][0][0][ 2][25] =   1.758513e-03; eval[1][0][0][ 2][25] =   7.381503e-05;
  val[1][0][0][ 2][26] =   1.727635e-03; eval[1][0][0][ 2][26] =   6.961394e-05;
  val[1][0][0][ 2][27] =   1.726583e-03; eval[1][0][0][ 2][27] =   1.161820e-04;
  val[1][0][0][ 2][28] =   1.615668e-03; eval[1][0][0][ 2][28] =   1.239034e-04;
  val[1][0][0][ 2][29] =   1.907071e-03; eval[1][0][0][ 2][29] =   1.735816e-04;
  //pc3dphi_sigma_c0d0z3
  n_val[1][0][0][3] = 30;
  val[1][0][0][ 3][ 0] =   0.000000e+00; eval[1][0][0][ 3][ 0] =   0.000000e+00;
  val[1][0][0][ 3][ 1] =   0.000000e+00; eval[1][0][0][ 3][ 1] =   0.000000e+00;
  val[1][0][0][ 3][ 2] =   3.028107e-03; eval[1][0][0][ 3][ 2] =   1.627179e-06;
  val[1][0][0][ 3][ 3] =   2.508940e-03; eval[1][0][0][ 3][ 3] =   9.638484e-07;
  val[1][0][0][ 3][ 4] =   2.552113e-03; eval[1][0][0][ 3][ 4] =   2.565851e-06;
  val[1][0][0][ 3][ 5] =   2.091139e-03; eval[1][0][0][ 3][ 5] =   9.504458e-07;
  val[1][0][0][ 3][ 6] =   1.956415e-03; eval[1][0][0][ 3][ 6] =   1.029016e-06;
  val[1][0][0][ 3][ 7] =   1.876947e-03; eval[1][0][0][ 3][ 7] =   1.214922e-06;
  val[1][0][0][ 3][ 8] =   1.859744e-03; eval[1][0][0][ 3][ 8] =   1.521577e-06;
  val[1][0][0][ 3][ 9] =   1.830622e-03; eval[1][0][0][ 3][ 9] =   1.938917e-06;
  val[1][0][0][ 3][10] =   1.807680e-03; eval[1][0][0][ 3][10] =   2.477603e-06;
  val[1][0][0][ 3][11] =   1.928262e-03; eval[1][0][0][ 3][11] =   6.991376e-06;
  val[1][0][0][ 3][12] =   1.937606e-03; eval[1][0][0][ 3][12] =   9.282215e-06;
  val[1][0][0][ 3][13] =   1.887163e-03; eval[1][0][0][ 3][13] =   1.119691e-05;
  val[1][0][0][ 3][14] =   1.900791e-03; eval[1][0][0][ 3][14] =   1.478209e-05;
  val[1][0][0][ 3][15] =   1.713222e-03; eval[1][0][0][ 3][15] =   7.997377e-06;
  val[1][0][0][ 3][16] =   1.887549e-03; eval[1][0][0][ 3][16] =   2.298406e-05;
  val[1][0][0][ 3][17] =   1.880683e-03; eval[1][0][0][ 3][17] =   2.846642e-05;
  val[1][0][0][ 3][18] =   1.814872e-03; eval[1][0][0][ 3][18] =   3.122277e-05;
  val[1][0][0][ 3][19] =   1.682567e-03; eval[1][0][0][ 3][19] =   1.794813e-05;
  val[1][0][0][ 3][20] =   1.708971e-03; eval[1][0][0][ 3][20] =   1.718109e-05;
  val[1][0][0][ 3][21] =   1.804033e-03; eval[1][0][0][ 3][21] =   4.980000e-05;
  val[1][0][0][ 3][22] =   1.716758e-03; eval[1][0][0][ 3][22] =   3.387462e-05;
  val[1][0][0][ 3][23] =   1.608275e-03; eval[1][0][0][ 3][23] =   3.747483e-05;
  val[1][0][0][ 3][24] =   1.674359e-03; eval[1][0][0][ 3][24] =   5.363454e-05;
  val[1][0][0][ 3][25] =   1.712955e-03; eval[1][0][0][ 3][25] =   8.022240e-05;
  val[1][0][0][ 3][26] =   1.766781e-03; eval[1][0][0][ 3][26] =   6.732083e-05;
  val[1][0][0][ 3][27] =   1.656064e-03; eval[1][0][0][ 3][27] =   9.749466e-05;
  val[1][0][0][ 3][28] =   1.750993e-03; eval[1][0][0][ 3][28] =   1.326662e-04;
  val[1][0][0][ 3][29] =   1.753508e-03; eval[1][0][0][ 3][29] =   1.695445e-04;
  //pc3dphi_sigma_c0d0z4
  n_val[1][0][0][4] = 30;
  val[1][0][0][ 4][ 0] =   0.000000e+00; eval[1][0][0][ 4][ 0] =   0.000000e+00;
  val[1][0][0][ 4][ 1] =   0.000000e+00; eval[1][0][0][ 4][ 1] =   0.000000e+00;
  val[1][0][0][ 4][ 2] =   2.929778e-03; eval[1][0][0][ 4][ 2] =   1.758992e-06;
  val[1][0][0][ 4][ 3] =   2.422216e-03; eval[1][0][0][ 4][ 3] =   1.033526e-06;
  val[1][0][0][ 4][ 4] =   2.473974e-03; eval[1][0][0][ 4][ 4] =   2.737695e-06;
  val[1][0][0][ 4][ 5] =   2.029213e-03; eval[1][0][0][ 4][ 5] =   1.024939e-06;
  val[1][0][0][ 4][ 6] =   1.904918e-03; eval[1][0][0][ 4][ 6] =   1.120874e-06;
  val[1][0][0][ 4][ 7] =   1.951750e-03; eval[1][0][0][ 4][ 7] =   2.847311e-06;
  val[1][0][0][ 4][ 8] =   1.830024e-03; eval[1][0][0][ 4][ 8] =   1.730984e-06;
  val[1][0][0][ 4][ 9] =   1.909734e-03; eval[1][0][0][ 4][ 9] =   4.601356e-06;
  val[1][0][0][ 4][10] =   1.892433e-03; eval[1][0][0][ 4][10] =   5.902551e-06;
  val[1][0][0][ 4][11] =   1.756636e-03; eval[1][0][0][ 4][11] =   3.633663e-06;
  val[1][0][0][ 4][12] =   1.745607e-03; eval[1][0][0][ 4][12] =   4.670423e-06;
  val[1][0][0][ 4][13] =   1.734455e-03; eval[1][0][0][ 4][13] =   5.993961e-06;
  val[1][0][0][ 4][14] =   1.740760e-03; eval[1][0][0][ 4][14] =   7.797800e-06;
  val[1][0][0][ 4][15] =   1.706172e-03; eval[1][0][0][ 4][15] =   9.268943e-06;
  val[1][0][0][ 4][16] =   1.709964e-03; eval[1][0][0][ 4][16] =   1.171482e-05;
  val[1][0][0][ 4][17] =   1.728457e-03; eval[1][0][0][ 4][17] =   1.491490e-05;
  val[1][0][0][ 4][18] =   1.751388e-03; eval[1][0][0][ 4][18] =   3.231766e-05;
  val[1][0][0][ 4][19] =   1.761245e-03; eval[1][0][0][ 4][19] =   3.923037e-05;
  val[1][0][0][ 4][20] =   1.699779e-03; eval[1][0][0][ 4][20] =   1.987807e-05;
  val[1][0][0][ 4][21] =   1.754095e-03; eval[1][0][0][ 4][21] =   5.161858e-05;
  val[1][0][0][ 4][22] =   1.705867e-03; eval[1][0][0][ 4][22] =   3.230994e-05;
  val[1][0][0][ 4][23] =   1.603212e-03; eval[1][0][0][ 4][23] =   4.215705e-05;
  val[1][0][0][ 4][24] =   1.768959e-03; eval[1][0][0][ 4][24] =   5.717003e-05;
  val[1][0][0][ 4][25] =   1.739845e-03; eval[1][0][0][ 4][25] =   8.436092e-05;
  val[1][0][0][ 4][26] =   1.848942e-03; eval[1][0][0][ 4][26] =   9.455631e-05;
  val[1][0][0][ 4][27] =   1.828969e-03; eval[1][0][0][ 4][27] =   1.421896e-04;
  val[1][0][0][ 4][28] =   1.777793e-03; eval[1][0][0][ 4][28] =   1.596350e-04;
  val[1][0][0][ 4][29] =   1.501917e-03; eval[1][0][0][ 4][29] =   1.189578e-04;
  //pc3dphi_sigma_c0d0z5
  n_val[1][0][0][5] = 30;
  val[1][0][0][ 5][ 0] =   0.000000e+00; eval[1][0][0][ 5][ 0] =   0.000000e+00;
  val[1][0][0][ 5][ 1] =   0.000000e+00; eval[1][0][0][ 5][ 1] =   0.000000e+00;
  val[1][0][0][ 5][ 2] =   2.998134e-03; eval[1][0][0][ 5][ 2] =   2.045252e-06;
  val[1][0][0][ 5][ 3] =   2.540408e-03; eval[1][0][0][ 5][ 3] =   1.316595e-06;
  val[1][0][0][ 5][ 4] =   2.333923e-03; eval[1][0][0][ 5][ 4] =   1.266387e-06;
  val[1][0][0][ 5][ 5] =   2.137185e-03; eval[1][0][0][ 5][ 5] =   1.359336e-06;
  val[1][0][0][ 5][ 6] =   2.020155e-03; eval[1][0][0][ 5][ 6] =   1.532019e-06;
  val[1][0][0][ 5][ 7] =   1.938366e-03; eval[1][0][0][ 5][ 7] =   1.793348e-06;
  val[1][0][0][ 5][ 8] =   1.933530e-03; eval[1][0][0][ 5][ 8] =   2.311102e-06;
  val[1][0][0][ 5][ 9] =   1.907614e-03; eval[1][0][0][ 5][ 9] =   2.991120e-06;
  val[1][0][0][ 5][10] =   1.867563e-03; eval[1][0][0][ 5][10] =   3.759849e-06;
  val[1][0][0][ 5][11] =   1.870730e-03; eval[1][0][0][ 5][11] =   4.973456e-06;
  val[1][0][0][ 5][12] =   1.867858e-03; eval[1][0][0][ 5][12] =   6.455063e-06;
  val[1][0][0][ 5][13] =   1.835184e-03; eval[1][0][0][ 5][13] =   8.077381e-06;
  val[1][0][0][ 5][14] =   1.874331e-03; eval[1][0][0][ 5][14] =   1.085616e-05;
  val[1][0][0][ 5][15] =   1.819884e-03; eval[1][0][0][ 5][15] =   1.257434e-05;
  val[1][0][0][ 5][16] =   1.871838e-03; eval[1][0][0][ 5][16] =   1.688586e-05;
  val[1][0][0][ 5][17] =   1.897702e-03; eval[1][0][0][ 5][17] =   2.175665e-05;
  val[1][0][0][ 5][18] =   1.769396e-03; eval[1][0][0][ 5][18] =   2.254254e-05;
  val[1][0][0][ 5][19] =   1.741191e-03; eval[1][0][0][ 5][19] =   2.645992e-05;
  val[1][0][0][ 5][20] =   1.821353e-03; eval[1][0][0][ 5][20] =   2.678822e-05;
  val[1][0][0][ 5][21] =   1.839388e-03; eval[1][0][0][ 5][21] =   3.924450e-05;
  val[1][0][0][ 5][22] =   1.687378e-03; eval[1][0][0][ 5][22] =   4.378646e-05;
  val[1][0][0][ 5][23] =   1.746553e-03; eval[1][0][0][ 5][23] =   5.982138e-05;
  val[1][0][0][ 5][24] =   1.896624e-03; eval[1][0][0][ 5][24] =   9.528871e-05;
  val[1][0][0][ 5][25] =   1.790881e-03; eval[1][0][0][ 5][25] =   1.027602e-04;
  val[1][0][0][ 5][26] =   1.741083e-03; eval[1][0][0][ 5][26] =   1.239357e-04;
  val[1][0][0][ 5][27] =   1.597030e-03; eval[1][0][0][ 5][27] =   1.181295e-04;
  val[1][0][0][ 5][28] =   1.515487e-03; eval[1][0][0][ 5][28] =   1.343748e-04;
  val[1][0][0][ 5][29] =   1.107415e-03; eval[1][0][0][ 5][29] =   5.408517e-03;
  //pc3dphi_sigma_c0d0z6
  n_val[1][0][0][6] = 30;
  val[1][0][0][ 6][ 0] =   0.000000e+00; eval[1][0][0][ 6][ 0] =   0.000000e+00;
  val[1][0][0][ 6][ 1] =   0.000000e+00; eval[1][0][0][ 6][ 1] =   0.000000e+00;
  val[1][0][0][ 6][ 2] =   3.032798e-03; eval[1][0][0][ 6][ 2] =   1.753858e-06;
  val[1][0][0][ 6][ 3] =   2.529428e-03; eval[1][0][0][ 6][ 3] =   1.058845e-06;
  val[1][0][0][ 6][ 4] =   2.316239e-03; eval[1][0][0][ 6][ 4] =   1.023655e-06;
  val[1][0][0][ 6][ 5] =   2.128047e-03; eval[1][0][0][ 6][ 5] =   1.113595e-06;
  val[1][0][0][ 6][ 6] =   2.000896e-03; eval[1][0][0][ 6][ 6] =   1.224906e-06;
  val[1][0][0][ 6][ 7] =   1.916647e-03; eval[1][0][0][ 6][ 7] =   1.438019e-06;
  val[1][0][0][ 6][ 8] =   1.903861e-03; eval[1][0][0][ 6][ 8] =   1.799651e-06;
  val[1][0][0][ 6][ 9] =   1.869224e-03; eval[1][0][0][ 6][ 9] =   2.293915e-06;
  val[1][0][0][ 6][10] =   1.831388e-03; eval[1][0][0][ 6][10] =   2.887258e-06;
  val[1][0][0][ 6][11] =   1.811416e-03; eval[1][0][0][ 6][11] =   3.726920e-06;
  val[1][0][0][ 6][12] =   1.800138e-03; eval[1][0][0][ 6][12] =   4.808067e-06;
  val[1][0][0][ 6][13] =   1.796993e-03; eval[1][0][0][ 6][13] =   6.264986e-06;
  val[1][0][0][ 6][14] =   1.821323e-03; eval[1][0][0][ 6][14] =   8.294561e-06;
  val[1][0][0][ 6][15] =   1.759931e-03; eval[1][0][0][ 6][15] =   9.632842e-06;
  val[1][0][0][ 6][16] =   1.792824e-03; eval[1][0][0][ 6][16] =   1.254622e-05;
  val[1][0][0][ 6][17] =   1.803796e-03; eval[1][0][0][ 6][17] =   1.594739e-05;
  val[1][0][0][ 6][18] =   1.716308e-03; eval[1][0][0][ 6][18] =   1.779809e-05;
  val[1][0][0][ 6][19] =   1.947236e-03; eval[1][0][0][ 6][19] =   5.415245e-05;
  val[1][0][0][ 6][20] =   1.771717e-03; eval[1][0][0][ 6][20] =   2.143548e-05;
  val[1][0][0][ 6][21] =   1.744316e-03; eval[1][0][0][ 6][21] =   2.853666e-05;
  val[1][0][0][ 6][22] =   1.609436e-03; eval[1][0][0][ 6][22] =   3.320163e-05;
  val[1][0][0][ 6][23] =   1.622705e-03; eval[1][0][0][ 6][23] =   4.281742e-05;
  val[1][0][0][ 6][24] =   1.741491e-03; eval[1][0][0][ 6][24] =   5.090890e-05;
  val[1][0][0][ 6][25] =   1.704794e-03; eval[1][0][0][ 6][25] =   7.588551e-05;
  val[1][0][0][ 6][26] =   1.556983e-03; eval[1][0][0][ 6][26] =   7.824460e-05;
  val[1][0][0][ 6][27] =   1.665073e-03; eval[1][0][0][ 6][27] =   1.110250e-04;
  val[1][0][0][ 6][28] =   1.768393e-03; eval[1][0][0][ 6][28] =   1.425756e-04;
  val[1][0][0][ 6][29] =   1.830407e-03; eval[1][0][0][ 6][29] =   1.968046e-04;
  //pc3dphi_sigma_c0d0z7
  n_val[1][0][0][7] = 30;
  val[1][0][0][ 7][ 0] =   0.000000e+00; eval[1][0][0][ 7][ 0] =   0.000000e+00;
  val[1][0][0][ 7][ 1] =   0.000000e+00; eval[1][0][0][ 7][ 1] =   0.000000e+00;
  val[1][0][0][ 7][ 2] =   3.144338e-03; eval[1][0][0][ 7][ 2] =   1.937716e-06;
  val[1][0][0][ 7][ 3] =   2.981297e-03; eval[1][0][0][ 7][ 3] =   3.609680e-06;
  val[1][0][0][ 7][ 4] =   2.628877e-03; eval[1][0][0][ 7][ 4] =   3.007561e-06;
  val[1][0][0][ 7][ 5] =   2.118391e-03; eval[1][0][0][ 7][ 5] =   7.719235e-07;
  val[1][0][0][ 7][ 6] =   1.986294e-03; eval[1][0][0][ 7][ 6] =   1.260243e-06;
  val[1][0][0][ 7][ 7] =   1.908911e-03; eval[1][0][0][ 7][ 7] =   1.490145e-06;
  val[1][0][0][ 7][ 8] =   1.884434e-03; eval[1][0][0][ 7][ 8] =   1.806973e-06;
  val[1][0][0][ 7][ 9] =   1.846619e-03; eval[1][0][0][ 7][ 9] =   2.276510e-06;
  val[1][0][0][ 7][10] =   1.805147e-03; eval[1][0][0][ 7][10] =   2.837462e-06;
  val[1][0][0][ 7][11] =   1.932278e-03; eval[1][0][0][ 7][11] =   8.110121e-06;
  val[1][0][0][ 7][12] =   1.947925e-03; eval[1][0][0][ 7][12] =   1.086870e-05;
  val[1][0][0][ 7][13] =   1.904829e-03; eval[1][0][0][ 7][13] =   1.324768e-05;
  val[1][0][0][ 7][14] =   1.887684e-03; eval[1][0][0][ 7][14] =   1.657924e-05;
  val[1][0][0][ 7][15] =   1.719732e-03; eval[1][0][0][ 7][15] =   9.183704e-06;
  val[1][0][0][ 7][16] =   1.714683e-03; eval[1][0][0][ 7][16] =   1.133521e-05;
  val[1][0][0][ 7][17] =   1.925912e-03; eval[1][0][0][ 7][17] =   3.487179e-05;
  val[1][0][0][ 7][18] =   1.711384e-03; eval[1][0][0][ 7][18] =   1.748681e-05;
  val[1][0][0][ 7][19] =   1.669414e-03; eval[1][0][0][ 7][19] =   1.999773e-05;
  val[1][0][0][ 7][20] =   1.675795e-03; eval[1][0][0][ 7][20] =   1.854364e-05;
  val[1][0][0][ 7][21] =   1.743289e-03; eval[1][0][0][ 7][21] =   4.926022e-05;
  val[1][0][0][ 7][22] =   1.617926e-03; eval[1][0][0][ 7][22] =   3.245305e-05;
  val[1][0][0][ 7][23] =   1.621878e-03; eval[1][0][0][ 7][23] =   4.346014e-05;
  val[1][0][0][ 7][24] =   1.701305e-03; eval[1][0][0][ 7][24] =   4.712047e-05;
  val[1][0][0][ 7][25] =   1.602035e-03; eval[1][0][0][ 7][25] =   7.226201e-05;
  val[1][0][0][ 7][26] =   1.646041e-03; eval[1][0][0][ 7][26] =   9.369321e-05;
  val[1][0][0][ 7][27] =   1.541084e-03; eval[1][0][0][ 7][27] =   9.619061e-05;
  val[1][0][0][ 7][28] =   1.995931e-03; eval[1][0][0][ 7][28] =   1.731381e-04;
  val[1][0][0][ 7][29] =   1.499023e-03; eval[1][0][0][ 7][29] =   1.263548e-04;
  //pc3dphi_sigma_c0d0z8
  n_val[1][0][0][8] = 30;
  val[1][0][0][ 8][ 0] =   0.000000e+00; eval[1][0][0][ 8][ 0] =   0.000000e+00;
  val[1][0][0][ 8][ 1] =   0.000000e+00; eval[1][0][0][ 8][ 1] =   0.000000e+00;
  val[1][0][0][ 8][ 2] =   3.007264e-03; eval[1][0][0][ 8][ 2] =   1.687407e-06;
  val[1][0][0][ 8][ 3] =   2.498683e-03; eval[1][0][0][ 8][ 3] =   1.040986e-06;
  val[1][0][0][ 8][ 4] =   2.631768e-03; eval[1][0][0][ 8][ 4] =   3.048153e-06;
  val[1][0][0][ 8][ 5] =   2.113627e-03; eval[1][0][0][ 8][ 5] =   7.718893e-07;
  val[1][0][0][ 8][ 6] =   1.992054e-03; eval[1][0][0][ 8][ 6] =   1.287906e-06;
  val[1][0][0][ 8][ 7] =   1.908757e-03; eval[1][0][0][ 8][ 7] =   1.504666e-06;
  val[1][0][0][ 8][ 8] =   1.894998e-03; eval[1][0][0][ 8][ 8] =   1.864603e-06;
  val[1][0][0][ 8][ 9] =   1.845985e-03; eval[1][0][0][ 8][ 9] =   2.315413e-06;
  val[1][0][0][ 8][10] =   1.802097e-03; eval[1][0][0][ 8][10] =   2.864611e-06;
  val[1][0][0][ 8][11] =   1.867653e-03; eval[1][0][0][ 8][11] =   7.043416e-06;
  val[1][0][0][ 8][12] =   1.851576e-03; eval[1][0][0][ 8][12] =   9.008299e-06;
  val[1][0][0][ 8][13] =   1.799598e-03; eval[1][0][0][ 8][13] =   1.073233e-05;
  val[1][0][0][ 8][14] =   1.831502e-03; eval[1][0][0][ 8][14] =   1.447830e-05;
  val[1][0][0][ 8][15] =   1.795908e-03; eval[1][0][0][ 8][15] =   1.722735e-05;
  val[1][0][0][ 8][16] =   1.790953e-03; eval[1][0][0][ 8][16] =   2.130044e-05;
  val[1][0][0][ 8][17] =   1.831582e-03; eval[1][0][0][ 8][17] =   2.856797e-05;
  val[1][0][0][ 8][18] =   1.821600e-03; eval[1][0][0][ 8][18] =   3.469827e-05;
  val[1][0][0][ 8][19] =   1.736290e-03; eval[1][0][0][ 8][19] =   3.598513e-05;
  val[1][0][0][ 8][20] =   1.701821e-03; eval[1][0][0][ 8][20] =   3.142603e-05;
  val[1][0][0][ 8][21] =   1.744818e-03; eval[1][0][0][ 8][21] =   4.759922e-05;
  val[1][0][0][ 8][22] =   1.672186e-03; eval[1][0][0][ 8][22] =   3.532119e-05;
  val[1][0][0][ 8][23] =   1.637227e-03; eval[1][0][0][ 8][23] =   4.823808e-05;
  val[1][0][0][ 8][24] =   1.479135e-03; eval[1][0][0][ 8][24] =   6.708184e-05;
  val[1][0][0][ 8][25] =   1.644215e-03; eval[1][0][0][ 8][25] =   5.693378e-05;
  val[1][0][0][ 8][26] =   1.842772e-03; eval[1][0][0][ 8][26] =   9.353828e-05;
  val[1][0][0][ 8][27] =   1.610879e-03; eval[1][0][0][ 8][27] =   1.094201e-04;
  val[1][0][0][ 8][28] =   1.440919e-03; eval[1][0][0][ 8][28] =   1.186100e-04;
  val[1][0][0][ 8][29] =   1.376891e-03; eval[1][0][0][ 8][29] =   9.793569e-05;
  //pc3dphi_sigma_c0d0z9
  n_val[1][0][0][9] = 30;
  val[1][0][0][ 9][ 0] =   0.000000e+00; eval[1][0][0][ 9][ 0] =   0.000000e+00;
  val[1][0][0][ 9][ 1] =   0.000000e+00; eval[1][0][0][ 9][ 1] =   0.000000e+00;
  val[1][0][0][ 9][ 2] =   2.819438e-03; eval[1][0][0][ 9][ 2] =   1.491524e-06;
  val[1][0][0][ 9][ 3] =   2.477460e-03; eval[1][0][0][ 9][ 3] =   1.062810e-06;
  val[1][0][0][ 9][ 4] =   2.263032e-03; eval[1][0][0][ 9][ 4] =   9.626678e-07;
  val[1][0][0][ 9][ 5] =   2.114987e-03; eval[1][0][0][ 9][ 5] =   1.108832e-06;
  val[1][0][0][ 9][ 6] =   1.974325e-03; eval[1][0][0][ 9][ 6] =   1.220146e-06;
  val[1][0][0][ 9][ 7] =   1.883361e-03; eval[1][0][0][ 9][ 7] =   1.412705e-06;
  val[1][0][0][ 9][ 8] =   1.890052e-03; eval[1][0][0][ 9][ 8] =   1.815649e-06;
  val[1][0][0][ 9][ 9] =   1.947491e-03; eval[1][0][0][ 9][ 9] =   4.436596e-06;
  val[1][0][0][ 9][10] =   1.911251e-03; eval[1][0][0][ 9][10] =   5.746529e-06;
  val[1][0][0][ 9][11] =   1.889235e-03; eval[1][0][0][ 9][11] =   7.333670e-06;
  val[1][0][0][ 9][12] =   1.881423e-03; eval[1][0][0][ 9][12] =   9.562087e-06;
  val[1][0][0][ 9][13] =   1.836322e-03; eval[1][0][0][ 9][13] =   1.152879e-05;
  val[1][0][0][ 9][14] =   1.819427e-03; eval[1][0][0][ 9][14] =   1.441618e-05;
  val[1][0][0][ 9][15] =   1.802444e-03; eval[1][0][0][ 9][15] =   1.766964e-05;
  val[1][0][0][ 9][16] =   1.775061e-03; eval[1][0][0][ 9][16] =   2.105410e-05;
  val[1][0][0][ 9][17] =   1.774526e-03; eval[1][0][0][ 9][17] =   2.636784e-05;
  val[1][0][0][ 9][18] =   1.762311e-03; eval[1][0][0][ 9][18] =   3.182245e-05;
  val[1][0][0][ 9][19] =   1.729087e-03; eval[1][0][0][ 9][19] =   3.616225e-05;
  val[1][0][0][ 9][20] =   1.737042e-03; eval[1][0][0][ 9][20] =   3.374447e-05;
  val[1][0][0][ 9][21] =   1.713904e-03; eval[1][0][0][ 9][21] =   4.559948e-05;
  val[1][0][0][ 9][22] =   1.675971e-03; eval[1][0][0][ 9][22] =   2.632104e-05;
  val[1][0][0][ 9][23] =   1.678131e-03; eval[1][0][0][ 9][23] =   3.637900e-05;
  val[1][0][0][ 9][24] =   1.629844e-03; eval[1][0][0][ 9][24] =   4.587401e-05;
  val[1][0][0][ 9][25] =   1.638680e-03; eval[1][0][0][ 9][25] =   5.805043e-05;
  val[1][0][0][ 9][26] =   1.732597e-03; eval[1][0][0][ 9][26] =   7.664984e-05;
  val[1][0][0][ 9][27] =   1.658614e-03; eval[1][0][0][ 9][27] =   8.402819e-05;
  val[1][0][0][ 9][28] =   1.731317e-03; eval[1][0][0][ 9][28] =   1.137393e-04;
  val[1][0][0][ 9][29] =   1.703419e-03; eval[1][0][0][ 9][29] =   1.305304e-04;
  //pc3dphi_sigma_c0d1z0
  n_val[1][0][1][0] = 30;
  val[1][0][1][ 0][ 0] =   0.000000e+00; eval[1][0][1][ 0][ 0] =   0.000000e+00;
  val[1][0][1][ 0][ 1] =   0.000000e+00; eval[1][0][1][ 0][ 1] =   0.000000e+00;
  val[1][0][1][ 0][ 2] =   2.796178e-03; eval[1][0][1][ 0][ 2] =   1.371786e-06;
  val[1][0][1][ 0][ 3] =   2.036417e-03; eval[1][0][1][ 0][ 3] =   1.260104e-06;
  val[1][0][1][ 0][ 4] =   1.804784e-03; eval[1][0][1][ 0][ 4] =   1.049225e-06;
  val[1][0][1][ 0][ 5] =   1.677700e-03; eval[1][0][1][ 0][ 5] =   1.045944e-06;
  val[1][0][1][ 0][ 6] =   1.604759e-03; eval[1][0][1][ 0][ 6] =   1.185564e-06;
  val[1][0][1][ 0][ 7] =   1.554109e-03; eval[1][0][1][ 0][ 7] =   1.389436e-06;
  val[1][0][1][ 0][ 8] =   1.506195e-03; eval[1][0][1][ 0][ 8] =   1.600603e-06;
  val[1][0][1][ 0][ 9] =   1.478919e-03; eval[1][0][1][ 0][ 9] =   2.024265e-06;
  val[1][0][1][ 0][10] =   1.456107e-03; eval[1][0][1][ 0][10] =   2.606387e-06;
  val[1][0][1][ 0][11] =   1.438228e-03; eval[1][0][1][ 0][11] =   3.338453e-06;
  val[1][0][1][ 0][12] =   1.430906e-03; eval[1][0][1][ 0][12] =   4.361793e-06;
  val[1][0][1][ 0][13] =   1.424093e-03; eval[1][0][1][ 0][13] =   5.682403e-06;
  val[1][0][1][ 0][14] =   1.409942e-03; eval[1][0][1][ 0][14] =   7.154223e-06;
  val[1][0][1][ 0][15] =   1.380260e-03; eval[1][0][1][ 0][15] =   8.725648e-06;
  val[1][0][1][ 0][16] =   1.373285e-03; eval[1][0][1][ 0][16] =   1.096209e-05;
  val[1][0][1][ 0][17] =   1.376194e-03; eval[1][0][1][ 0][17] =   1.365419e-05;
  val[1][0][1][ 0][18] =   1.353925e-03; eval[1][0][1][ 0][18] =   1.639999e-05;
  val[1][0][1][ 0][19] =   1.378949e-03; eval[1][0][1][ 0][19] =   2.127723e-05;
  val[1][0][1][ 0][20] =   1.401770e-03; eval[1][0][1][ 0][20] =   2.097046e-05;
  val[1][0][1][ 0][21] =   1.394893e-03; eval[1][0][1][ 0][21] =   2.896371e-05;
  val[1][0][1][ 0][22] =   1.377845e-03; eval[1][0][1][ 0][22] =   3.909952e-05;
  val[1][0][1][ 0][23] =   1.448363e-03; eval[1][0][1][ 0][23] =   6.106568e-05;
  val[1][0][1][ 0][24] =   1.506523e-03; eval[1][0][1][ 0][24] =   6.128999e-05;
  val[1][0][1][ 0][25] =   1.672194e-03; eval[1][0][1][ 0][25] =   1.043627e-04;
  val[1][0][1][ 0][26] =   1.719017e-03; eval[1][0][1][ 0][26] =   1.498803e-04;
  val[1][0][1][ 0][27] =   1.442216e-02; eval[1][0][1][ 0][27] =   1.621629e-02;
  val[1][0][1][ 0][28] =   1.181509e-03; eval[1][0][1][ 0][28] =   1.031736e-04;
  val[1][0][1][ 0][29] =   1.385377e-03; eval[1][0][1][ 0][29] =   1.933563e-04;
  //pc3dphi_sigma_c0d1z1
  n_val[1][0][1][1] = 30;
  val[1][0][1][ 1][ 0] =   0.000000e+00; eval[1][0][1][ 1][ 0] =   0.000000e+00;
  val[1][0][1][ 1][ 1] =   0.000000e+00; eval[1][0][1][ 1][ 1] =   0.000000e+00;
  val[1][0][1][ 1][ 2] =   2.755676e-03; eval[1][0][1][ 1][ 2] =   1.224460e-06;
  val[1][0][1][ 1][ 3] =   2.027255e-03; eval[1][0][1][ 1][ 3] =   1.121436e-06;
  val[1][0][1][ 1][ 4] =   1.803727e-03; eval[1][0][1][ 1][ 4] =   9.637394e-07;
  val[1][0][1][ 1][ 5] =   1.681457e-03; eval[1][0][1][ 1][ 5] =   9.940618e-07;
  val[1][0][1][ 1][ 6] =   1.602749e-03; eval[1][0][1][ 1][ 6] =   1.080860e-06;
  val[1][0][1][ 1][ 7] =   1.551554e-03; eval[1][0][1][ 1][ 7] =   1.290395e-06;
  val[1][0][1][ 1][ 8] =   1.514143e-03; eval[1][0][1][ 1][ 8] =   1.554565e-06;
  val[1][0][1][ 1][ 9] =   1.485305e-03; eval[1][0][1][ 1][ 9] =   1.978546e-06;
  val[1][0][1][ 1][10] =   1.462630e-03; eval[1][0][1][ 1][10] =   2.528789e-06;
  val[1][0][1][ 1][11] =   1.449273e-03; eval[1][0][1][ 1][11] =   3.267902e-06;
  val[1][0][1][ 1][12] =   1.436934e-03; eval[1][0][1][ 1][12] =   4.213479e-06;
  val[1][0][1][ 1][13] =   1.422174e-03; eval[1][0][1][ 1][13] =   5.375299e-06;
  val[1][0][1][ 1][14] =   1.403616e-03; eval[1][0][1][ 1][14] =   6.697362e-06;
  val[1][0][1][ 1][15] =   1.399194e-03; eval[1][0][1][ 1][15] =   8.491173e-06;
  val[1][0][1][ 1][16] =   1.396304e-03; eval[1][0][1][ 1][16] =   1.068798e-05;
  val[1][0][1][ 1][17] =   1.396489e-03; eval[1][0][1][ 1][17] =   1.343699e-05;
  val[1][0][1][ 1][18] =   1.391095e-03; eval[1][0][1][ 1][18] =   1.648137e-05;
  val[1][0][1][ 1][19] =   1.383813e-03; eval[1][0][1][ 1][19] =   2.005977e-05;
  val[1][0][1][ 1][20] =   1.358564e-03; eval[1][0][1][ 1][20] =   1.772259e-05;
  val[1][0][1][ 1][21] =   1.369859e-03; eval[1][0][1][ 1][21] =   2.541411e-05;
  val[1][0][1][ 1][22] =   1.391615e-03; eval[1][0][1][ 1][22] =   3.767694e-05;
  val[1][0][1][ 1][23] =   1.288095e-03; eval[1][0][1][ 1][23] =   4.200684e-05;
  val[1][0][1][ 1][24] =   1.429068e-03; eval[1][0][1][ 1][24] =   7.137210e-05;
  val[1][0][1][ 1][25] =   1.393969e-03; eval[1][0][1][ 1][25] =   7.810024e-05;
  val[1][0][1][ 1][26] =   1.359310e-03; eval[1][0][1][ 1][26] =   9.250432e-05;
  val[1][0][1][ 1][27] =   1.313415e-03; eval[1][0][1][ 1][27] =   1.147931e-04;
  val[1][0][1][ 1][28] =   1.483137e-02; eval[1][0][1][ 1][28] =   1.926342e-02;
  val[1][0][1][ 1][29] =   3.264660e-02; eval[1][0][1][ 1][29] =   3.265099e-02;
  //pc3dphi_sigma_c0d1z2
  n_val[1][0][1][2] = 30;
  val[1][0][1][ 2][ 0] =   0.000000e+00; eval[1][0][1][ 2][ 0] =   0.000000e+00;
  val[1][0][1][ 2][ 1] =   0.000000e+00; eval[1][0][1][ 2][ 1] =   0.000000e+00;
  val[1][0][1][ 2][ 2] =   2.324969e-03; eval[1][0][1][ 2][ 2] =   1.515031e-06;
  val[1][0][1][ 2][ 3] =   2.012185e-03; eval[1][0][1][ 2][ 3] =   1.095044e-06;
  val[1][0][1][ 2][ 4] =   1.795443e-03; eval[1][0][1][ 2][ 4] =   9.514507e-07;
  val[1][0][1][ 2][ 5] =   1.679855e-03; eval[1][0][1][ 2][ 5] =   9.729176e-07;
  val[1][0][1][ 2][ 6] =   1.601823e-03; eval[1][0][1][ 2][ 6] =   1.075370e-06;
  val[1][0][1][ 2][ 7] =   1.539340e-03; eval[1][0][1][ 2][ 7] =   1.255353e-06;
  val[1][0][1][ 2][ 8] =   1.497105e-03; eval[1][0][1][ 2][ 8] =   1.482057e-06;
  val[1][0][1][ 2][ 9] =   1.470204e-03; eval[1][0][1][ 2][ 9] =   1.894437e-06;
  val[1][0][1][ 2][10] =   1.449766e-03; eval[1][0][1][ 2][10] =   2.431186e-06;
  val[1][0][1][ 2][11] =   1.433890e-03; eval[1][0][1][ 2][11] =   3.143273e-06;
  val[1][0][1][ 2][12] =   1.432143e-03; eval[1][0][1][ 2][12] =   4.178546e-06;
  val[1][0][1][ 2][13] =   1.415065e-03; eval[1][0][1][ 2][13] =   5.315905e-06;
  val[1][0][1][ 2][14] =   1.399847e-03; eval[1][0][1][ 2][14] =   6.703446e-06;
  val[1][0][1][ 2][15] =   1.387250e-03; eval[1][0][1][ 2][15] =   8.437188e-06;
  val[1][0][1][ 2][16] =   1.371844e-03; eval[1][0][1][ 2][16] =   1.027338e-05;
  val[1][0][1][ 2][17] =   1.385855e-03; eval[1][0][1][ 2][17] =   1.335548e-05;
  val[1][0][1][ 2][18] =   1.363174e-03; eval[1][0][1][ 2][18] =   1.591416e-05;
  val[1][0][1][ 2][19] =   1.327471e-03; eval[1][0][1][ 2][19] =   1.776116e-05;
  val[1][0][1][ 2][20] =   1.377891e-03; eval[1][0][1][ 2][20] =   1.893660e-05;
  val[1][0][1][ 2][21] =   1.363747e-03; eval[1][0][1][ 2][21] =   2.602801e-05;
  val[1][0][1][ 2][22] =   1.431609e-03; eval[1][0][1][ 2][22] =   4.108814e-05;
  val[1][0][1][ 2][23] =   1.313830e-03; eval[1][0][1][ 2][23] =   4.237556e-05;
  val[1][0][1][ 2][24] =   1.366742e-03; eval[1][0][1][ 2][24] =   6.256844e-05;
  val[1][0][1][ 2][25] =   1.332885e-03; eval[1][0][1][ 2][25] =   7.266109e-05;
  val[1][0][1][ 2][26] =   1.308777e-03; eval[1][0][1][ 2][26] =   8.868802e-05;
  val[1][0][1][ 2][27] =   1.404109e-03; eval[1][0][1][ 2][27] =   1.256280e-04;
  val[1][0][1][ 2][28] =   1.343834e-03; eval[1][0][1][ 2][28] =   1.440233e-04;
  val[1][0][1][ 2][29] =   1.361812e-03; eval[1][0][1][ 2][29] =   1.784247e-04;
  //pc3dphi_sigma_c0d1z3
  n_val[1][0][1][3] = 30;
  val[1][0][1][ 3][ 0] =   0.000000e+00; eval[1][0][1][ 3][ 0] =   0.000000e+00;
  val[1][0][1][ 3][ 1] =   0.000000e+00; eval[1][0][1][ 3][ 1] =   0.000000e+00;
  val[1][0][1][ 3][ 2] =   2.598371e-03; eval[1][0][1][ 3][ 2] =   1.013911e-06;
  val[1][0][1][ 3][ 3] =   2.037669e-03; eval[1][0][1][ 3][ 3] =   1.157644e-06;
  val[1][0][1][ 3][ 4] =   1.819744e-03; eval[1][0][1][ 3][ 4] =   9.931018e-07;
  val[1][0][1][ 3][ 5] =   1.823108e-03; eval[1][0][1][ 3][ 5] =   7.622848e-07;
  val[1][0][1][ 3][ 6] =   1.614124e-03; eval[1][0][1][ 3][ 6] =   1.113504e-06;
  val[1][0][1][ 3][ 7] =   1.552570e-03; eval[1][0][1][ 3][ 7] =   1.308187e-06;
  val[1][0][1][ 3][ 8] =   1.512147e-03; eval[1][0][1][ 3][ 8] =   1.571755e-06;
  val[1][0][1][ 3][ 9] =   1.480381e-03; eval[1][0][1][ 3][ 9] =   2.015368e-06;
  val[1][0][1][ 3][10] =   1.461439e-03; eval[1][0][1][ 3][10] =   2.604787e-06;
  val[1][0][1][ 3][11] =   1.442713e-03; eval[1][0][1][ 3][11] =   3.350710e-06;
  val[1][0][1][ 3][12] =   1.430990e-03; eval[1][0][1][ 3][12] =   4.342617e-06;
  val[1][0][1][ 3][13] =   1.430030e-03; eval[1][0][1][ 3][13] =   5.708261e-06;
  val[1][0][1][ 3][14] =   1.417862e-03; eval[1][0][1][ 3][14] =   7.258556e-06;
  val[1][0][1][ 3][15] =   1.393618e-03; eval[1][0][1][ 3][15] =   8.820876e-06;
  val[1][0][1][ 3][16] =   1.401558e-03; eval[1][0][1][ 3][16] =   1.147324e-05;
  val[1][0][1][ 3][17] =   1.364022e-03; eval[1][0][1][ 3][17] =   1.308484e-05;
  val[1][0][1][ 3][18] =   1.367634e-03; eval[1][0][1][ 3][18] =   1.622239e-05;
  val[1][0][1][ 3][19] =   1.383730e-03; eval[1][0][1][ 3][19] =   2.083394e-05;
  val[1][0][1][ 3][20] =   1.396160e-03; eval[1][0][1][ 3][20] =   1.989748e-05;
  val[1][0][1][ 3][21] =   1.330802e-03; eval[1][0][1][ 3][21] =   2.495123e-05;
  val[1][0][1][ 3][22] =   1.332930e-03; eval[1][0][1][ 3][22] =   3.560249e-05;
  val[1][0][1][ 3][23] =   1.287990e-03; eval[1][0][1][ 3][23] =   4.455229e-05;
  val[1][0][1][ 3][24] =   1.358607e-03; eval[1][0][1][ 3][24] =   6.626965e-05;
  val[1][0][1][ 3][25] =   1.332228e-03; eval[1][0][1][ 3][25] =   8.355655e-05;
  val[1][0][1][ 3][26] =   1.223497e-03; eval[1][0][1][ 3][26] =   7.037109e-05;
  val[1][0][1][ 3][27] =   1.464759e-03; eval[1][0][1][ 3][27] =   1.543772e-04;
  val[1][0][1][ 3][28] =   1.329719e-03; eval[1][0][1][ 3][28] =   1.383370e-04;
  val[1][0][1][ 3][29] =   2.114334e-03; eval[1][0][1][ 3][29] =   3.159942e-04;
  //pc3dphi_sigma_c0d1z4
  n_val[1][0][1][4] = 30;
  val[1][0][1][ 4][ 0] =   0.000000e+00; eval[1][0][1][ 4][ 0] =   0.000000e+00;
  val[1][0][1][ 4][ 1] =   0.000000e+00; eval[1][0][1][ 4][ 1] =   0.000000e+00;
  val[1][0][1][ 4][ 2] =   2.544086e-03; eval[1][0][1][ 4][ 2] =   1.183931e-06;
  val[1][0][1][ 4][ 3] =   2.157226e-03; eval[1][0][1][ 4][ 3] =   8.536938e-07;
  val[1][0][1][ 4][ 4] =   1.809022e-03; eval[1][0][1][ 4][ 4] =   1.177207e-06;
  val[1][0][1][ 4][ 5] =   1.805939e-03; eval[1][0][1][ 4][ 5] =   8.659176e-07;
  val[1][0][1][ 4][ 6] =   1.601474e-03; eval[1][0][1][ 4][ 6] =   1.258664e-06;
  val[1][0][1][ 4][ 7] =   1.538973e-03; eval[1][0][1][ 4][ 7] =   1.476448e-06;
  val[1][0][1][ 4][ 8] =   1.505082e-03; eval[1][0][1][ 4][ 8] =   1.815085e-06;
  val[1][0][1][ 4][ 9] =   1.478653e-03; eval[1][0][1][ 4][ 9] =   2.363157e-06;
  val[1][0][1][ 4][10] =   1.450341e-03; eval[1][0][1][ 4][10] =   2.991853e-06;
  val[1][0][1][ 4][11] =   1.436333e-03; eval[1][0][1][ 4][11] =   3.903833e-06;
  val[1][0][1][ 4][12] =   1.421805e-03; eval[1][0][1][ 4][12] =   5.069696e-06;
  val[1][0][1][ 4][13] =   1.401125e-03; eval[1][0][1][ 4][13] =   6.385222e-06;
  val[1][0][1][ 4][14] =   1.382342e-03; eval[1][0][1][ 4][14] =   7.987434e-06;
  val[1][0][1][ 4][15] =   1.350938e-03; eval[1][0][1][ 4][15] =   9.561112e-06;
  val[1][0][1][ 4][16] =   1.385156e-03; eval[1][0][1][ 4][16] =   1.314441e-05;
  val[1][0][1][ 4][17] =   1.359967e-03; eval[1][0][1][ 4][17] =   1.562821e-05;
  val[1][0][1][ 4][18] =   1.372552e-03; eval[1][0][1][ 4][18] =   1.982843e-05;
  val[1][0][1][ 4][19] =   1.344118e-03; eval[1][0][1][ 4][19] =   2.330736e-05;
  val[1][0][1][ 4][20] =   1.373918e-03; eval[1][0][1][ 4][20] =   2.304297e-05;
  val[1][0][1][ 4][21] =   1.328090e-03; eval[1][0][1][ 4][21] =   2.988724e-05;
  val[1][0][1][ 4][22] =   1.278252e-03; eval[1][0][1][ 4][22] =   3.764136e-05;
  val[1][0][1][ 4][23] =   1.370165e-03; eval[1][0][1][ 4][23] =   6.035410e-05;
  val[1][0][1][ 4][24] =   1.288295e-03; eval[1][0][1][ 4][24] =   6.684720e-05;
  val[1][0][1][ 4][25] =   1.491960e-03; eval[1][0][1][ 4][25] =   1.002956e-04;
  val[1][0][1][ 4][26] =   1.545978e-03; eval[1][0][1][ 4][26] =   1.115495e-04;
  val[1][0][1][ 4][27] =   1.289690e-03; eval[1][0][1][ 4][27] =   1.331931e-04;
  val[1][0][1][ 4][28] =   1.846237e-03; eval[1][0][1][ 4][28] =   2.296638e-04;
  val[1][0][1][ 4][29] =   1.466723e-03; eval[1][0][1][ 4][29] =   4.960916e-04;
  //pc3dphi_sigma_c0d1z5
  n_val[1][0][1][5] = 30;
  val[1][0][1][ 5][ 0] =   0.000000e+00; eval[1][0][1][ 5][ 0] =   0.000000e+00;
  val[1][0][1][ 5][ 1] =   0.000000e+00; eval[1][0][1][ 5][ 1] =   0.000000e+00;
  val[1][0][1][ 5][ 2] =   2.596878e-03; eval[1][0][1][ 5][ 2] =   1.334737e-06;
  val[1][0][1][ 5][ 3] =   2.037901e-03; eval[1][0][1][ 5][ 3] =   1.384175e-06;
  val[1][0][1][ 5][ 4] =   1.789055e-03; eval[1][0][1][ 5][ 4] =   1.146978e-06;
  val[1][0][1][ 5][ 5] =   1.682861e-03; eval[1][0][1][ 5][ 5] =   1.180776e-06;
  val[1][0][1][ 5][ 6] =   1.608259e-03; eval[1][0][1][ 5][ 6] =   1.299415e-06;
  val[1][0][1][ 5][ 7] =   1.549821e-03; eval[1][0][1][ 5][ 7] =   1.555063e-06;
  val[1][0][1][ 5][ 8] =   1.518548e-03; eval[1][0][1][ 5][ 8] =   1.907925e-06;
  val[1][0][1][ 5][ 9] =   1.485715e-03; eval[1][0][1][ 5][ 9] =   2.401524e-06;
  val[1][0][1][ 5][10] =   1.479053e-03; eval[1][0][1][ 5][10] =   3.170870e-06;
  val[1][0][1][ 5][11] =   1.461566e-03; eval[1][0][1][ 5][11] =   4.103622e-06;
  val[1][0][1][ 5][12] =   1.452231e-03; eval[1][0][1][ 5][12] =   5.355594e-06;
  val[1][0][1][ 5][13] =   1.453069e-03; eval[1][0][1][ 5][13] =   6.987375e-06;
  val[1][0][1][ 5][14] =   1.462126e-03; eval[1][0][1][ 5][14] =   9.212956e-06;
  val[1][0][1][ 5][15] =   1.421687e-03; eval[1][0][1][ 5][15] =   1.090788e-05;
  val[1][0][1][ 5][16] =   1.420580e-03; eval[1][0][1][ 5][16] =   1.378499e-05;
  val[1][0][1][ 5][17] =   1.452407e-03; eval[1][0][1][ 5][17] =   1.810646e-05;
  val[1][0][1][ 5][18] =   1.398110e-03; eval[1][0][1][ 5][18] =   2.003665e-05;
  val[1][0][1][ 5][19] =   1.390096e-03; eval[1][0][1][ 5][19] =   2.463962e-05;
  val[1][0][1][ 5][20] =   1.395321e-03; eval[1][0][1][ 5][20] =   2.292352e-05;
  val[1][0][1][ 5][21] =   1.445295e-03; eval[1][0][1][ 5][21] =   3.622069e-05;
  val[1][0][1][ 5][22] =   1.341693e-03; eval[1][0][1][ 5][22] =   3.978405e-05;
  val[1][0][1][ 5][23] =   1.417712e-03; eval[1][0][1][ 5][23] =   6.187816e-05;
  val[1][0][1][ 5][24] =   1.438270e-03; eval[1][0][1][ 5][24] =   8.436104e-05;
  val[1][0][1][ 5][25] =   1.484681e-03; eval[1][0][1][ 5][25] =   1.180969e-04;
  val[1][0][1][ 5][26] =   1.353433e-03; eval[1][0][1][ 5][26] =   1.118490e-04;
  val[1][0][1][ 5][27] =   1.623609e-03; eval[1][0][1][ 5][27] =   1.753316e-04;
  val[1][0][1][ 5][28] =   1.175488e-03; eval[1][0][1][ 5][28] =   1.084408e-04;
  val[1][0][1][ 5][29] =   1.934201e-03; eval[1][0][1][ 5][29] =   3.232670e-04;
  //pc3dphi_sigma_c0d1z6
  n_val[1][0][1][6] = 30;
  val[1][0][1][ 6][ 0] =   0.000000e+00; eval[1][0][1][ 6][ 0] =   0.000000e+00;
  val[1][0][1][ 6][ 1] =   0.000000e+00; eval[1][0][1][ 6][ 1] =   0.000000e+00;
  val[1][0][1][ 6][ 2] =   2.605265e-03; eval[1][0][1][ 6][ 2] =   1.176607e-06;
  val[1][0][1][ 6][ 3] =   2.057850e-03; eval[1][0][1][ 6][ 3] =   1.166282e-06;
  val[1][0][1][ 6][ 4] =   1.817037e-03; eval[1][0][1][ 6][ 4] =   9.770812e-07;
  val[1][0][1][ 6][ 5] =   1.711122e-03; eval[1][0][1][ 6][ 5] =   1.016093e-06;
  val[1][0][1][ 6][ 6] =   1.634799e-03; eval[1][0][1][ 6][ 6] =   1.127478e-06;
  val[1][0][1][ 6][ 7] =   1.568928e-03; eval[1][0][1][ 6][ 7] =   1.329664e-06;
  val[1][0][1][ 6][ 8] =   1.535774e-03; eval[1][0][1][ 6][ 8] =   1.616415e-06;
  val[1][0][1][ 6][ 9] =   1.509652e-03; eval[1][0][1][ 6][ 9] =   2.066568e-06;
  val[1][0][1][ 6][10] =   1.495365e-03; eval[1][0][1][ 6][10] =   2.691606e-06;
  val[1][0][1][ 6][11] =   1.475643e-03; eval[1][0][1][ 6][11] =   3.468599e-06;
  val[1][0][1][ 6][12] =   1.455413e-03; eval[1][0][1][ 6][12] =   4.464126e-06;
  val[1][0][1][ 6][13] =   1.452246e-03; eval[1][0][1][ 6][13] =   5.838393e-06;
  val[1][0][1][ 6][14] =   1.436522e-03; eval[1][0][1][ 6][14] =   7.370337e-06;
  val[1][0][1][ 6][15] =   1.437384e-03; eval[1][0][1][ 6][15] =   9.486493e-06;
  val[1][0][1][ 6][16] =   1.424215e-03; eval[1][0][1][ 6][16] =   1.164342e-05;
  val[1][0][1][ 6][17] =   1.421138e-03; eval[1][0][1][ 6][17] =   1.464814e-05;
  val[1][0][1][ 6][18] =   1.409232e-03; eval[1][0][1][ 6][18] =   1.773906e-05;
  val[1][0][1][ 6][19] =   1.389659e-03; eval[1][0][1][ 6][19] =   2.080685e-05;
  val[1][0][1][ 6][20] =   1.404738e-03; eval[1][0][1][ 6][20] =   2.027327e-05;
  val[1][0][1][ 6][21] =   1.417404e-03; eval[1][0][1][ 6][21] =   2.991742e-05;
  val[1][0][1][ 6][22] =   1.323756e-03; eval[1][0][1][ 6][22] =   3.377342e-05;
  val[1][0][1][ 6][23] =   1.386326e-03; eval[1][0][1][ 6][23] =   5.163279e-05;
  val[1][0][1][ 6][24] =   1.399270e-03; eval[1][0][1][ 6][24] =   6.891134e-05;
  val[1][0][1][ 6][25] =   1.373715e-03; eval[1][0][1][ 6][25] =   8.071230e-05;
  val[1][0][1][ 6][26] =   1.425078e-03; eval[1][0][1][ 6][26] =   1.186191e-04;
  val[1][0][1][ 6][27] =   4.027555e-04; eval[1][0][1][ 6][27] =   6.927775e-06;
  val[1][0][1][ 6][28] =   1.893124e-03; eval[1][0][1][ 6][28] =   2.311913e-04;
  val[1][0][1][ 6][29] =   1.993122e-03; eval[1][0][1][ 6][29] =   2.773911e-04;
  //pc3dphi_sigma_c0d1z7
  n_val[1][0][1][7] = 30;
  val[1][0][1][ 7][ 0] =   0.000000e+00; eval[1][0][1][ 7][ 0] =   0.000000e+00;
  val[1][0][1][ 7][ 1] =   0.000000e+00; eval[1][0][1][ 7][ 1] =   0.000000e+00;
  val[1][0][1][ 7][ 2] =   2.779591e-03; eval[1][0][1][ 7][ 2] =   1.394361e-06;
  val[1][0][1][ 7][ 3] =   2.064431e-03; eval[1][0][1][ 7][ 3] =   1.219752e-06;
  val[1][0][1][ 7][ 4] =   1.811960e-03; eval[1][0][1][ 7][ 4] =   9.869196e-07;
  val[1][0][1][ 7][ 5] =   1.693712e-03; eval[1][0][1][ 7][ 5] =   1.035099e-06;
  val[1][0][1][ 7][ 6] =   1.613143e-03; eval[1][0][1][ 7][ 6] =   1.135479e-06;
  val[1][0][1][ 7][ 7] =   1.550595e-03; eval[1][0][1][ 7][ 7] =   1.324221e-06;
  val[1][0][1][ 7][ 8] =   1.522420e-03; eval[1][0][1][ 7][ 8] =   1.614394e-06;
  val[1][0][1][ 7][ 9] =   1.502744e-03; eval[1][0][1][ 7][ 9] =   2.088263e-06;
  val[1][0][1][ 7][10] =   1.485677e-03; eval[1][0][1][ 7][10] =   2.702882e-06;
  val[1][0][1][ 7][11] =   1.477445e-03; eval[1][0][1][ 7][11] =   3.553887e-06;
  val[1][0][1][ 7][12] =   1.473676e-03; eval[1][0][1][ 7][12] =   4.698375e-06;
  val[1][0][1][ 7][13] =   1.459049e-03; eval[1][0][1][ 7][13] =   6.005562e-06;
  val[1][0][1][ 7][14] =   1.457448e-03; eval[1][0][1][ 7][14] =   7.699607e-06;
  val[1][0][1][ 7][15] =   1.429532e-03; eval[1][0][1][ 7][15] =   9.376002e-06;
  val[1][0][1][ 7][16] =   1.421120e-03; eval[1][0][1][ 7][16] =   1.162082e-05;
  val[1][0][1][ 7][17] =   1.414377e-03; eval[1][0][1][ 7][17] =   1.428216e-05;
  val[1][0][1][ 7][18] =   1.429937e-03; eval[1][0][1][ 7][18] =   1.823030e-05;
  val[1][0][1][ 7][19] =   1.367028e-03; eval[1][0][1][ 7][19] =   1.970422e-05;
  val[1][0][1][ 7][20] =   1.406202e-03; eval[1][0][1][ 7][20] =   1.956067e-05;
  val[1][0][1][ 7][21] =   1.473704e-03; eval[1][0][1][ 7][21] =   3.206273e-05;
  val[1][0][1][ 7][22] =   1.349130e-03; eval[1][0][1][ 7][22] =   3.365602e-05;
  val[1][0][1][ 7][23] =   1.336968e-03; eval[1][0][1][ 7][23] =   4.465864e-05;
  val[1][0][1][ 7][24] =   1.401471e-03; eval[1][0][1][ 7][24] =   6.494903e-05;
  val[1][0][1][ 7][25] =   1.494830e-03; eval[1][0][1][ 7][25] =   1.035298e-04;
  val[1][0][1][ 7][26] =   1.641900e-03; eval[1][0][1][ 7][26] =   8.543379e-05;
  val[1][0][1][ 7][27] =   1.470351e-03; eval[1][0][1][ 7][27] =   1.495479e-04;
  val[1][0][1][ 7][28] =   1.334234e-03; eval[1][0][1][ 7][28] =   1.358212e-04;
  val[1][0][1][ 7][29] =   1.703814e-03; eval[1][0][1][ 7][29] =   6.084736e-04;
  //pc3dphi_sigma_c0d1z8
  n_val[1][0][1][8] = 30;
  val[1][0][1][ 8][ 0] =   0.000000e+00; eval[1][0][1][ 8][ 0] =   0.000000e+00;
  val[1][0][1][ 8][ 1] =   0.000000e+00; eval[1][0][1][ 8][ 1] =   0.000000e+00;
  val[1][0][1][ 8][ 2] =   2.675312e-03; eval[1][0][1][ 8][ 2] =   1.177422e-06;
  val[1][0][1][ 8][ 3] =   2.305365e-03; eval[1][0][1][ 8][ 3] =   8.574701e-07;
  val[1][0][1][ 8][ 4] =   1.788096e-03; eval[1][0][1][ 8][ 4] =   9.778496e-07;
  val[1][0][1][ 8][ 5] =   1.667139e-03; eval[1][0][1][ 8][ 5] =   9.831528e-07;
  val[1][0][1][ 8][ 6] =   1.600738e-03; eval[1][0][1][ 8][ 6] =   1.092519e-06;
  val[1][0][1][ 8][ 7] =   1.547223e-03; eval[1][0][1][ 8][ 7] =   1.310825e-06;
  val[1][0][1][ 8][ 8] =   1.519045e-03; eval[1][0][1][ 8][ 8] =   1.620308e-06;
  val[1][0][1][ 8][ 9] =   1.500543e-03; eval[1][0][1][ 8][ 9] =   2.084126e-06;
  val[1][0][1][ 8][10] =   1.489766e-03; eval[1][0][1][ 8][10] =   2.731462e-06;
  val[1][0][1][ 8][11] =   1.476887e-03; eval[1][0][1][ 8][11] =   3.555332e-06;
  val[1][0][1][ 8][12] =   1.468803e-03; eval[1][0][1][ 8][12] =   4.650606e-06;
  val[1][0][1][ 8][13] =   1.454883e-03; eval[1][0][1][ 8][13] =   5.986021e-06;
  val[1][0][1][ 8][14] =   1.462198e-03; eval[1][0][1][ 8][14] =   7.871464e-06;
  val[1][0][1][ 8][15] =   1.443993e-03; eval[1][0][1][ 8][15] =   9.819672e-06;
  val[1][0][1][ 8][16] =   1.440890e-03; eval[1][0][1][ 8][16] =   1.242390e-05;
  val[1][0][1][ 8][17] =   1.438060e-03; eval[1][0][1][ 8][17] =   1.532595e-05;
  val[1][0][1][ 8][18] =   1.407420e-03; eval[1][0][1][ 8][18] =   1.802178e-05;
  val[1][0][1][ 8][19] =   1.381530e-03; eval[1][0][1][ 8][19] =   2.082024e-05;
  val[1][0][1][ 8][20] =   1.403795e-03; eval[1][0][1][ 8][20] =   2.021919e-05;
  val[1][0][1][ 8][21] =   1.459018e-03; eval[1][0][1][ 8][21] =   3.258009e-05;
  val[1][0][1][ 8][22] =   1.380221e-03; eval[1][0][1][ 8][22] =   3.717995e-05;
  val[1][0][1][ 8][23] =   1.475173e-03; eval[1][0][1][ 8][23] =   6.267968e-05;
  val[1][0][1][ 8][24] =   1.386359e-03; eval[1][0][1][ 8][24] =   6.655353e-05;
  val[1][0][1][ 8][25] =   1.484870e-03; eval[1][0][1][ 8][25] =   1.031637e-04;
  val[1][0][1][ 8][26] =   1.497167e-03; eval[1][0][1][ 8][26] =   1.318782e-04;
  val[1][0][1][ 8][27] =   1.634318e-03; eval[1][0][1][ 8][27] =   1.568662e-04;
  val[1][0][1][ 8][28] =   1.565616e-03; eval[1][0][1][ 8][28] =   1.406850e-04;
  val[1][0][1][ 8][29] =   1.602458e-03; eval[1][0][1][ 8][29] =   1.778270e-04;
  //pc3dphi_sigma_c0d1z9
  n_val[1][0][1][9] = 30;
  val[1][0][1][ 9][ 0] =   0.000000e+00; eval[1][0][1][ 9][ 0] =   0.000000e+00;
  val[1][0][1][ 9][ 1] =   0.000000e+00; eval[1][0][1][ 9][ 1] =   0.000000e+00;
  val[1][0][1][ 9][ 2] =   3.090389e-03; eval[1][0][1][ 9][ 2] =   2.276830e-06;
  val[1][0][1][ 9][ 3] =   2.396355e-03; eval[1][0][1][ 9][ 3] =   1.109875e-06;
  val[1][0][1][ 9][ 4] =   1.845759e-03; eval[1][0][1][ 9][ 4] =   1.225927e-06;
  val[1][0][1][ 9][ 5] =   1.883943e-03; eval[1][0][1][ 9][ 5] =   1.006958e-06;
  val[1][0][1][ 9][ 6] =   1.624196e-03; eval[1][0][1][ 9][ 6] =   1.347143e-06;
  val[1][0][1][ 9][ 7] =   1.573566e-03; eval[1][0][1][ 9][ 7] =   1.577882e-06;
  val[1][0][1][ 9][ 8] =   1.548933e-03; eval[1][0][1][ 9][ 8] =   1.973533e-06;
  val[1][0][1][ 9][ 9] =   1.518415e-03; eval[1][0][1][ 9][ 9] =   2.506170e-06;
  val[1][0][1][ 9][10] =   1.501264e-03; eval[1][0][1][ 9][10] =   3.263532e-06;
  val[1][0][1][ 9][11] =   1.477118e-03; eval[1][0][1][ 9][11] =   4.169066e-06;
  val[1][0][1][ 9][12] =   1.480444e-03; eval[1][0][1][ 9][12] =   5.546531e-06;
  val[1][0][1][ 9][13] =   1.472890e-03; eval[1][0][1][ 9][13] =   7.164124e-06;
  val[1][0][1][ 9][14] =   1.499483e-03; eval[1][0][1][ 9][14] =   9.700487e-06;
  val[1][0][1][ 9][15] =   1.462060e-03; eval[1][0][1][ 9][15] =   1.153958e-05;
  val[1][0][1][ 9][16] =   1.444611e-03; eval[1][0][1][ 9][16] =   1.414339e-05;
  val[1][0][1][ 9][17] =   1.449290e-03; eval[1][0][1][ 9][17] =   1.744012e-05;
  val[1][0][1][ 9][18] =   1.445320e-03; eval[1][0][1][ 9][18] =   2.159236e-05;
  val[1][0][1][ 9][19] =   1.433548e-03; eval[1][0][1][ 9][19] =   2.527209e-05;
  val[1][0][1][ 9][20] =   1.412224e-03; eval[1][0][1][ 9][20] =   2.227873e-05;
  val[1][0][1][ 9][21] =   1.472098e-03; eval[1][0][1][ 9][21] =   3.637966e-05;
  val[1][0][1][ 9][22] =   1.521174e-03; eval[1][0][1][ 9][22] =   5.437206e-05;
  val[1][0][1][ 9][23] =   1.432981e-03; eval[1][0][1][ 9][23] =   6.075996e-05;
  val[1][0][1][ 9][24] =   1.695970e-03; eval[1][0][1][ 9][24] =   7.157629e-05;
  val[1][0][1][ 9][25] =   1.464729e-03; eval[1][0][1][ 9][25] =   9.796303e-05;
  val[1][0][1][ 9][26] =   2.026158e-03; eval[1][0][1][ 9][26] =   2.553670e-04;
  val[1][0][1][ 9][27] =   1.674352e-03; eval[1][0][1][ 9][27] =   2.189472e-04;
  val[1][0][1][ 9][28] =   1.286999e-03; eval[1][0][1][ 9][28] =   1.378851e-04;
  val[1][0][1][ 9][29] =   1.664859e-03; eval[1][0][1][ 9][29] =   2.054627e-04;
  //pc3dphi_sigma_c1d0z0
  n_val[1][1][0][0] = 30;
  val[1][1][0][ 0][ 0] =   0.000000e+00; eval[1][1][0][ 0][ 0] =   0.000000e+00;
  val[1][1][0][ 0][ 1] =   0.000000e+00; eval[1][1][0][ 0][ 1] =   0.000000e+00;
  val[1][1][0][ 0][ 2] =   2.708318e-03; eval[1][1][0][ 0][ 2] =   1.200828e-06;
  val[1][1][0][ 0][ 3] =   2.607122e-03; eval[1][1][0][ 0][ 3] =   1.148102e-06;
  val[1][1][0][ 0][ 4] =   2.391222e-03; eval[1][1][0][ 0][ 4] =   1.173089e-06;
  val[1][1][0][ 0][ 5] =   2.290886e-03; eval[1][1][0][ 0][ 5] =   9.602465e-07;
  val[1][1][0][ 0][ 6] =   2.086509e-03; eval[1][1][0][ 0][ 6] =   1.384065e-06;
  val[1][1][0][ 0][ 7] =   2.000166e-03; eval[1][1][0][ 0][ 7] =   1.645539e-06;
  val[1][1][0][ 0][ 8] =   1.977763e-03; eval[1][1][0][ 0][ 8] =   2.081619e-06;
  val[1][1][0][ 0][ 9] =   1.941501e-03; eval[1][1][0][ 0][ 9] =   2.630561e-06;
  val[1][1][0][ 0][10] =   1.924285e-03; eval[1][1][0][ 0][10] =   3.425366e-06;
  val[1][1][0][ 0][11] =   1.921101e-03; eval[1][1][0][ 0][11] =   4.546022e-06;
  val[1][1][0][ 0][12] =   2.199649e-03; eval[1][1][0][ 0][12] =   1.617081e-05;
  val[1][1][0][ 0][13] =   2.129785e-03; eval[1][1][0][ 0][13] =   1.919197e-05;
  val[1][1][0][ 0][14] =   2.250772e-03; eval[1][1][0][ 0][14] =   2.780140e-05;
  val[1][1][0][ 0][15] =   1.930528e-03; eval[1][1][0][ 0][15] =   9.475296e-06;
  val[1][1][0][ 0][16] =   1.939703e-03; eval[1][1][0][ 0][16] =   1.221786e-05;
  val[1][1][0][ 0][17] =   1.935273e-03; eval[1][1][0][ 0][17] =   1.502864e-05;
  val[1][1][0][ 0][18] =   1.933023e-03; eval[1][1][0][ 0][18] =   1.841397e-05;
  val[1][1][0][ 0][19] =   1.908858e-03; eval[1][1][0][ 0][19] =   2.203043e-05;
  val[1][1][0][ 0][20] =   1.899639e-03; eval[1][1][0][ 0][20] =   2.028538e-05;
  val[1][1][0][ 0][21] =   1.902200e-03; eval[1][1][0][ 0][21] =   2.946757e-05;
  val[1][1][0][ 0][22] =   1.925627e-03; eval[1][1][0][ 0][22] =   4.090622e-05;
  val[1][1][0][ 0][23] =   1.914845e-03; eval[1][1][0][ 0][23] =   5.148801e-05;
  val[1][1][0][ 0][24] =   1.818591e-03; eval[1][1][0][ 0][24] =   6.001663e-05;
  val[1][1][0][ 0][25] =   1.788996e-03; eval[1][1][0][ 0][25] =   7.324988e-05;
  val[1][1][0][ 0][26] =   1.770718e-03; eval[1][1][0][ 0][26] =   9.331170e-05;
  val[1][1][0][ 0][27] =   1.776759e-03; eval[1][1][0][ 0][27] =   9.829258e-05;
  val[1][1][0][ 0][28] =   2.281968e-03; eval[1][1][0][ 0][28] =   3.695796e-04;
  val[1][1][0][ 0][29] =   2.442923e-01; eval[1][1][0][ 0][29] =   1.747613e+00;
  //pc3dphi_sigma_c1d0z1
  n_val[1][1][0][1] = 30;
  val[1][1][0][ 1][ 0] =   0.000000e+00; eval[1][1][0][ 1][ 0] =   0.000000e+00;
  val[1][1][0][ 1][ 1] =   0.000000e+00; eval[1][1][0][ 1][ 1] =   0.000000e+00;
  val[1][1][0][ 1][ 2] =   2.983381e-03; eval[1][1][0][ 1][ 2] =   1.536734e-06;
  val[1][1][0][ 1][ 3] =   2.569580e-03; eval[1][1][0][ 1][ 3] =   1.106977e-06;
  val[1][1][0][ 1][ 4] =   2.863994e-03; eval[1][1][0][ 1][ 4] =   4.269922e-06;
  val[1][1][0][ 1][ 5] =   2.203283e-03; eval[1][1][0][ 1][ 5] =   8.429477e-07;
  val[1][1][0][ 1][ 6] =   2.075110e-03; eval[1][1][0][ 1][ 6] =   9.540459e-07;
  val[1][1][0][ 1][ 7] =   1.967240e-03; eval[1][1][0][ 1][ 7] =   1.605113e-06;
  val[1][1][0][ 1][ 8] =   2.138251e-03; eval[1][1][0][ 1][ 8] =   4.793831e-06;
  val[1][1][0][ 1][ 9] =   2.112314e-03; eval[1][1][0][ 1][ 9] =   6.166304e-06;
  val[1][1][0][ 1][10] =   1.902115e-03; eval[1][1][0][ 1][10] =   3.454880e-06;
  val[1][1][0][ 1][11] =   1.889296e-03; eval[1][1][0][ 1][11] =   4.515001e-06;
  val[1][1][0][ 1][12] =   1.880751e-03; eval[1][1][0][ 1][12] =   4.199581e-06;
  val[1][1][0][ 1][13] =   1.890801e-03; eval[1][1][0][ 1][13] =   5.497585e-06;
  val[1][1][0][ 1][14] =   1.939855e-03; eval[1][1][0][ 1][14] =   1.093779e-05;
  val[1][1][0][ 1][15] =   1.894022e-03; eval[1][1][0][ 1][15] =   1.301781e-05;
  val[1][1][0][ 1][16] =   1.937822e-03; eval[1][1][0][ 1][16] =   1.739069e-05;
  val[1][1][0][ 1][17] =   1.939283e-03; eval[1][1][0][ 1][17] =   2.192799e-05;
  val[1][1][0][ 1][18] =   1.878191e-03; eval[1][1][0][ 1][18] =   2.450406e-05;
  val[1][1][0][ 1][19] =   1.860184e-03; eval[1][1][0][ 1][19] =   2.941329e-05;
  val[1][1][0][ 1][20] =   1.941729e-03; eval[1][1][0][ 1][20] =   3.101868e-05;
  val[1][1][0][ 1][21] =   1.901569e-03; eval[1][1][0][ 1][21] =   4.171694e-05;
  val[1][1][0][ 1][22] =   1.855189e-03; eval[1][1][0][ 1][22] =   3.902031e-05;
  val[1][1][0][ 1][23] =   1.886392e-03; eval[1][1][0][ 1][23] =   5.357706e-05;
  val[1][1][0][ 1][24] =   1.818816e-03; eval[1][1][0][ 1][24] =   6.984873e-05;
  val[1][1][0][ 1][25] =   1.716917e-03; eval[1][1][0][ 1][25] =   7.395345e-05;
  val[1][1][0][ 1][26] =   1.901809e-03; eval[1][1][0][ 1][26] =   1.166870e-04;
  val[1][1][0][ 1][27] =   1.885573e-03; eval[1][1][0][ 1][27] =   1.276492e-04;
  val[1][1][0][ 1][28] =   1.830477e-03; eval[1][1][0][ 1][28] =   1.698690e-04;
  val[1][1][0][ 1][29] =   1.880875e-03; eval[1][1][0][ 1][29] =   2.324907e-04;
  //pc3dphi_sigma_c1d0z2
  n_val[1][1][0][2] = 30;
  val[1][1][0][ 2][ 0] =   0.000000e+00; eval[1][1][0][ 2][ 0] =   0.000000e+00;
  val[1][1][0][ 2][ 1] =   0.000000e+00; eval[1][1][0][ 2][ 1] =   0.000000e+00;
  val[1][1][0][ 2][ 2] =   2.973081e-03; eval[1][1][0][ 2][ 2] =   1.470949e-06;
  val[1][1][0][ 2][ 3] =   2.589341e-03; eval[1][1][0][ 2][ 3] =   1.147428e-06;
  val[1][1][0][ 2][ 4] =   2.827971e-03; eval[1][1][0][ 2][ 4] =   4.001502e-06;
  val[1][1][0][ 2][ 5] =   2.179867e-03; eval[1][1][0][ 2][ 5] =   8.000536e-07;
  val[1][1][0][ 2][ 6] =   2.103754e-03; eval[1][1][0][ 2][ 6] =   1.436985e-06;
  val[1][1][0][ 2][ 7] =   1.999675e-03; eval[1][1][0][ 2][ 7] =   1.703214e-06;
  val[1][1][0][ 2][ 8] =   1.980883e-03; eval[1][1][0][ 2][ 8] =   2.143206e-06;
  val[1][1][0][ 2][ 9] =   1.965594e-03; eval[1][1][0][ 2][ 9] =   2.834278e-06;
  val[1][1][0][ 2][10] =   1.939363e-03; eval[1][1][0][ 2][10] =   3.663616e-06;
  val[1][1][0][ 2][11] =   1.928790e-03; eval[1][1][0][ 2][11] =   4.801714e-06;
  val[1][1][0][ 2][12] =   1.971266e-03; eval[1][1][0][ 2][12] =   1.182430e-05;
  val[1][1][0][ 2][13] =   1.913203e-03; eval[1][1][0][ 2][13] =   8.092247e-06;
  val[1][1][0][ 2][14] =   1.884553e-03; eval[1][1][0][ 2][14] =   1.023221e-05;
  val[1][1][0][ 2][15] =   1.836326e-03; eval[1][1][0][ 2][15] =   1.228819e-05;
  val[1][1][0][ 2][16] =   1.931856e-03; eval[1][1][0][ 2][16] =   1.741232e-05;
  val[1][1][0][ 2][17] =   1.921019e-03; eval[1][1][0][ 2][17] =   3.801991e-05;
  val[1][1][0][ 2][18] =   1.804533e-03; eval[1][1][0][ 2][18] =   2.289604e-05;
  val[1][1][0][ 2][19] =   1.831408e-03; eval[1][1][0][ 2][19] =   4.982916e-05;
  val[1][1][0][ 2][20] =   1.833270e-03; eval[1][1][0][ 2][20] =   2.774460e-05;
  val[1][1][0][ 2][21] =   1.774927e-03; eval[1][1][0][ 2][21] =   3.608548e-05;
  val[1][1][0][ 2][22] =   1.757924e-03; eval[1][1][0][ 2][22] =   3.731451e-05;
  val[1][1][0][ 2][23] =   1.776069e-03; eval[1][1][0][ 2][23] =   4.936263e-05;
  val[1][1][0][ 2][24] =   1.695306e-03; eval[1][1][0][ 2][24] =   6.397213e-05;
  val[1][1][0][ 2][25] =   1.776006e-03; eval[1][1][0][ 2][25] =   8.983054e-05;
  val[1][1][0][ 2][26] =   1.817547e-03; eval[1][1][0][ 2][26] =   1.070350e-04;
  val[1][1][0][ 2][27] =   1.849333e-03; eval[1][1][0][ 2][27] =   1.533759e-04;
  val[1][1][0][ 2][28] =   1.745810e-03; eval[1][1][0][ 2][28] =   1.221397e-04;
  val[1][1][0][ 2][29] =   1.424011e-03; eval[1][1][0][ 2][29] =   2.060086e-04;
  //pc3dphi_sigma_c1d0z3
  n_val[1][1][0][3] = 30;
  val[1][1][0][ 3][ 0] =   0.000000e+00; eval[1][1][0][ 3][ 0] =   0.000000e+00;
  val[1][1][0][ 3][ 1] =   0.000000e+00; eval[1][1][0][ 3][ 1] =   0.000000e+00;
  val[1][1][0][ 3][ 2] =   3.254184e-03; eval[1][1][0][ 3][ 2] =   1.906783e-06;
  val[1][1][0][ 3][ 3] =   2.751257e-03; eval[1][1][0][ 3][ 3] =   1.372447e-06;
  val[1][1][0][ 3][ 4] =   2.731935e-03; eval[1][1][0][ 3][ 4] =   3.743076e-06;
  val[1][1][0][ 3][ 5] =   2.142235e-03; eval[1][1][0][ 3][ 5] =   7.907409e-07;
  val[1][1][0][ 3][ 6] =   2.036888e-03; eval[1][1][0][ 3][ 6] =   1.325393e-06;
  val[1][1][0][ 3][ 7] =   1.941709e-03; eval[1][1][0][ 3][ 7] =   1.597412e-06;
  val[1][1][0][ 3][ 8] =   1.927192e-03; eval[1][1][0][ 3][ 8] =   2.038432e-06;
  val[1][1][0][ 3][ 9] =   1.913148e-03; eval[1][1][0][ 3][ 9] =   2.684225e-06;
  val[1][1][0][ 3][10] =   1.892711e-03; eval[1][1][0][ 3][10] =   3.475109e-06;
  val[1][1][0][ 3][11] =   1.876021e-03; eval[1][1][0][ 3][11] =   4.468843e-06;
  val[1][1][0][ 3][12] =   1.921719e-03; eval[1][1][0][ 3][12] =   1.114323e-05;
  val[1][1][0][ 3][13] =   1.847199e-03; eval[1][1][0][ 3][13] =   7.408236e-06;
  val[1][1][0][ 3][14] =   1.920634e-03; eval[1][1][0][ 3][14] =   1.859315e-05;
  val[1][1][0][ 3][15] =   1.948889e-03; eval[1][1][0][ 3][15] =   2.514487e-05;
  val[1][1][0][ 3][16] =   1.845198e-03; eval[1][1][0][ 3][16] =   1.520519e-05;
  val[1][1][0][ 3][17] =   1.779554e-03; eval[1][1][0][ 3][17] =   1.751304e-05;
  val[1][1][0][ 3][18] =   1.905602e-03; eval[1][1][0][ 3][18] =   4.560393e-05;
  val[1][1][0][ 3][19] =   1.725951e-03; eval[1][1][0][ 3][19] =   2.379459e-05;
  val[1][1][0][ 3][20] =   1.878823e-03; eval[1][1][0][ 3][20] =   5.012081e-05;
  val[1][1][0][ 3][21] =   1.791283e-03; eval[1][1][0][ 3][21] =   3.522848e-05;
  val[1][1][0][ 3][22] =   1.779499e-03; eval[1][1][0][ 3][22] =   4.826735e-05;
  val[1][1][0][ 3][23] =   1.737675e-03; eval[1][1][0][ 3][23] =   4.878599e-05;
  val[1][1][0][ 3][24] =   1.865151e-03; eval[1][1][0][ 3][24] =   7.275643e-05;
  val[1][1][0][ 3][25] =   1.926793e-03; eval[1][1][0][ 3][25] =   1.043416e-04;
  val[1][1][0][ 3][26] =   1.948979e-03; eval[1][1][0][ 3][26] =   1.253683e-04;
  val[1][1][0][ 3][27] =   1.679346e-03; eval[1][1][0][ 3][27] =   1.651724e-04;
  val[1][1][0][ 3][28] =   1.958465e-03; eval[1][1][0][ 3][28] =   2.673569e-04;
  val[1][1][0][ 3][29] =   1.560156e-03; eval[1][1][0][ 3][29] =   1.686257e-04;
  //pc3dphi_sigma_c1d0z4
  n_val[1][1][0][4] = 30;
  val[1][1][0][ 4][ 0] =   0.000000e+00; eval[1][1][0][ 4][ 0] =   0.000000e+00;
  val[1][1][0][ 4][ 1] =   0.000000e+00; eval[1][1][0][ 4][ 1] =   0.000000e+00;
  val[1][1][0][ 4][ 2] =   3.058528e-03; eval[1][1][0][ 4][ 2] =   1.750531e-06;
  val[1][1][0][ 4][ 3] =   2.620624e-03; eval[1][1][0][ 4][ 3] =   1.328107e-06;
  val[1][1][0][ 4][ 4] =   2.626747e-03; eval[1][1][0][ 4][ 4] =   3.789521e-06;
  val[1][1][0][ 4][ 5] =   2.087793e-03; eval[1][1][0][ 4][ 5] =   1.224970e-06;
  val[1][1][0][ 4][ 6] =   1.950964e-03; eval[1][1][0][ 4][ 6] =   1.341775e-06;
  val[1][1][0][ 4][ 7] =   1.864400e-03; eval[1][1][0][ 4][ 7] =   1.616968e-06;
  val[1][1][0][ 4][ 8] =   1.847643e-03; eval[1][1][0][ 4][ 8] =   2.037640e-06;
  val[1][1][0][ 4][ 9] =   1.835124e-03; eval[1][1][0][ 4][ 9] =   2.687856e-06;
  val[1][1][0][ 4][10] =   1.881788e-03; eval[1][1][0][ 4][10] =   6.807279e-06;
  val[1][1][0][ 4][11] =   1.871819e-03; eval[1][1][0][ 4][11] =   8.882997e-06;
  val[1][1][0][ 4][12] =   1.825842e-03; eval[1][1][0][ 4][12] =   1.092195e-05;
  val[1][1][0][ 4][13] =   1.886454e-03; eval[1][1][0][ 4][13] =   1.575950e-05;
  val[1][1][0][ 4][14] =   1.733441e-03; eval[1][1][0][ 4][14] =   9.287085e-06;
  val[1][1][0][ 4][15] =   1.836827e-03; eval[1][1][0][ 4][15] =   2.415275e-05;
  val[1][1][0][ 4][16] =   1.836346e-03; eval[1][1][0][ 4][16] =   3.052292e-05;
  val[1][1][0][ 4][17] =   1.748035e-03; eval[1][1][0][ 4][17] =   3.292379e-05;
  val[1][1][0][ 4][18] =   1.666561e-03; eval[1][1][0][ 4][18] =   2.127413e-05;
  val[1][1][0][ 4][19] =   1.810520e-03; eval[1][1][0][ 4][19] =   5.601542e-05;
  val[1][1][0][ 4][20] =   1.694484e-03; eval[1][1][0][ 4][20] =   4.318710e-05;
  val[1][1][0][ 4][21] =   1.743230e-03; eval[1][1][0][ 4][21] =   6.664527e-05;
  val[1][1][0][ 4][22] =   1.676818e-03; eval[1][1][0][ 4][22] =   4.632055e-05;
  val[1][1][0][ 4][23] =   1.623827e-03; eval[1][1][0][ 4][23] =   5.979544e-05;
  val[1][1][0][ 4][24] =   1.632397e-03; eval[1][1][0][ 4][24] =   8.851792e-05;
  val[1][1][0][ 4][25] =   1.800463e-03; eval[1][1][0][ 4][25] =   1.036377e-04;
  val[1][1][0][ 4][26] =   1.648784e-03; eval[1][1][0][ 4][26] =   1.384611e-04;
  val[1][1][0][ 4][27] =   1.091587e-03; eval[1][1][0][ 4][27] =   2.062427e-03;
  val[1][1][0][ 4][28] =   1.615631e-03; eval[1][1][0][ 4][28] =   1.899221e-04;
  val[1][1][0][ 4][29] =   1.724346e-03; eval[1][1][0][ 4][29] =   2.446001e-04;
  //pc3dphi_sigma_c1d0z5
  n_val[1][1][0][5] = 30;
  val[1][1][0][ 5][ 0] =   0.000000e+00; eval[1][1][0][ 5][ 0] =   0.000000e+00;
  val[1][1][0][ 5][ 1] =   0.000000e+00; eval[1][1][0][ 5][ 1] =   0.000000e+00;
  val[1][1][0][ 5][ 2] =   3.333580e-03; eval[1][1][0][ 5][ 2] =   2.426479e-06;
  val[1][1][0][ 5][ 3] =   2.710920e-03; eval[1][1][0][ 5][ 3] =   9.654866e-07;
  val[1][1][0][ 5][ 4] =   2.944515e-03; eval[1][1][0][ 5][ 4] =   5.839454e-06;
  val[1][1][0][ 5][ 5] =   2.197451e-03; eval[1][1][0][ 5][ 5] =   1.078354e-06;
  val[1][1][0][ 5][ 6] =   2.067334e-03; eval[1][1][0][ 5][ 6] =   1.248042e-06;
  val[1][1][0][ 5][ 7] =   1.980013e-03; eval[1][1][0][ 5][ 7] =   2.189825e-06;
  val[1][1][0][ 5][ 8] =   2.185908e-03; eval[1][1][0][ 5][ 8] =   6.688720e-06;
  val[1][1][0][ 5][ 9] =   2.127415e-03; eval[1][1][0][ 5][ 9] =   8.134966e-06;
  val[1][1][0][ 5][10] =   1.947496e-03; eval[1][1][0][ 5][10] =   3.260314e-06;
  val[1][1][0][ 5][11] =   1.941713e-03; eval[1][1][0][ 5][11] =   4.267742e-06;
  val[1][1][0][ 5][12] =   1.995476e-03; eval[1][1][0][ 5][12] =   8.816822e-06;
  val[1][1][0][ 5][13] =   1.933394e-03; eval[1][1][0][ 5][13] =   7.188219e-06;
  val[1][1][0][ 5][14] =   1.968780e-03; eval[1][1][0][ 5][14] =   1.424984e-05;
  val[1][1][0][ 5][15] =   1.946036e-03; eval[1][1][0][ 5][15] =   1.728750e-05;
  val[1][1][0][ 5][16] =   2.041487e-03; eval[1][1][0][ 5][16] =   2.516939e-05;
  val[1][1][0][ 5][17] =   1.969442e-03; eval[1][1][0][ 5][17] =   2.886432e-05;
  val[1][1][0][ 5][18] =   1.879454e-03; eval[1][1][0][ 5][18] =   3.119749e-05;
  val[1][1][0][ 5][19] =   1.822085e-03; eval[1][1][0][ 5][19] =   3.546050e-05;
  val[1][1][0][ 5][20] =   1.975743e-03; eval[1][1][0][ 5][20] =   4.109676e-05;
  val[1][1][0][ 5][21] =   1.923693e-03; eval[1][1][0][ 5][21] =   5.237654e-05;
  val[1][1][0][ 5][22] =   1.900561e-03; eval[1][1][0][ 5][22] =   5.235419e-05;
  val[1][1][0][ 5][23] =   1.641138e-03; eval[1][1][0][ 5][23] =   4.601832e-05;
  val[1][1][0][ 5][24] =   1.806620e-03; eval[1][1][0][ 5][24] =   9.706310e-05;
  val[1][1][0][ 5][25] =   1.919472e-03; eval[1][1][0][ 5][25] =   1.151968e-04;
  val[1][1][0][ 5][26] =   1.877133e-03; eval[1][1][0][ 5][26] =   1.706249e-04;
  val[1][1][0][ 5][27] =   1.823175e-03; eval[1][1][0][ 5][27] =   1.649157e-04;
  val[1][1][0][ 5][28] =   2.572699e-03; eval[1][1][0][ 5][28] =   4.735175e-04;
  val[1][1][0][ 5][29] =   1.990371e-03; eval[1][1][0][ 5][29] =   3.685541e-04;
  //pc3dphi_sigma_c1d0z6
  n_val[1][1][0][6] = 30;
  val[1][1][0][ 6][ 0] =   0.000000e+00; eval[1][1][0][ 6][ 0] =   0.000000e+00;
  val[1][1][0][ 6][ 1] =   0.000000e+00; eval[1][1][0][ 6][ 1] =   0.000000e+00;
  val[1][1][0][ 6][ 2] =   3.062711e-03; eval[1][1][0][ 6][ 2] =   9.221930e-07;
  val[1][1][0][ 6][ 3] =   2.690215e-03; eval[1][1][0][ 6][ 3] =   1.271766e-06;
  val[1][1][0][ 6][ 4] =   2.928736e-03; eval[1][1][0][ 6][ 4] =   4.765678e-06;
  val[1][1][0][ 6][ 5] =   2.159501e-03; eval[1][1][0][ 6][ 5] =   8.695595e-07;
  val[1][1][0][ 6][ 6] =   2.015469e-03; eval[1][1][0][ 6][ 6] =   9.746029e-07;
  val[1][1][0][ 6][ 7] =   1.934457e-03; eval[1][1][0][ 6][ 7] =   1.712352e-06;
  val[1][1][0][ 6][ 8] =   1.919223e-03; eval[1][1][0][ 6][ 8] =   2.140579e-06;
  val[1][1][0][ 6][ 9] =   1.901394e-03; eval[1][1][0][ 6][ 9] =   1.966043e-06;
  val[1][1][0][ 6][10] =   1.984925e-03; eval[1][1][0][ 6][10] =   7.083630e-06;
  val[1][1][0][ 6][11] =   1.964279e-03; eval[1][1][0][ 6][11] =   8.827670e-06;
  val[1][1][0][ 6][12] =   1.923346e-03; eval[1][1][0][ 6][12] =   1.126313e-05;
  val[1][1][0][ 6][13] =   1.906323e-03; eval[1][1][0][ 6][13] =   8.420459e-06;
  val[1][1][0][ 6][14] =   1.891350e-03; eval[1][1][0][ 6][14] =   1.813224e-05;
  val[1][1][0][ 6][15] =   1.847669e-03; eval[1][1][0][ 6][15] =   1.254425e-05;
  val[1][1][0][ 6][16] =   1.927881e-03; eval[1][1][0][ 6][16] =   1.793501e-05;
  val[1][1][0][ 6][17] =   1.914503e-03; eval[1][1][0][ 6][17] =   3.774232e-05;
  val[1][1][0][ 6][18] =   1.807031e-03; eval[1][1][0][ 6][18] =   2.315756e-05;
  val[1][1][0][ 6][19] =   1.843615e-03; eval[1][1][0][ 6][19] =   2.934661e-05;
  val[1][1][0][ 6][20] =   1.901488e-03; eval[1][1][0][ 6][20] =   5.279514e-05;
  val[1][1][0][ 6][21] =   1.781328e-03; eval[1][1][0][ 6][21] =   6.227148e-05;
  val[1][1][0][ 6][22] =   1.786771e-03; eval[1][1][0][ 6][22] =   3.934556e-05;
  val[1][1][0][ 6][23] =   1.742572e-03; eval[1][1][0][ 6][23] =   5.107643e-05;
  val[1][1][0][ 6][24] =   1.725018e-03; eval[1][1][0][ 6][24] =   6.601078e-05;
  val[1][1][0][ 6][25] =   1.874572e-03; eval[1][1][0][ 6][25] =   9.303326e-05;
  val[1][1][0][ 6][26] =   1.593990e-03; eval[1][1][0][ 6][26] =   1.073240e-04;
  val[1][1][0][ 6][27] =   1.766362e-03; eval[1][1][0][ 6][27] =   1.628144e-04;
  val[1][1][0][ 6][28] =   1.765892e-03; eval[1][1][0][ 6][28] =   1.737257e-04;
  val[1][1][0][ 6][29] =   1.929107e-03; eval[1][1][0][ 6][29] =   2.090679e-04;
  //pc3dphi_sigma_c1d0z7
  n_val[1][1][0][7] = 30;
  val[1][1][0][ 7][ 0] =   0.000000e+00; eval[1][1][0][ 7][ 0] =   0.000000e+00;
  val[1][1][0][ 7][ 1] =   0.000000e+00; eval[1][1][0][ 7][ 1] =   0.000000e+00;
  val[1][1][0][ 7][ 2] =   2.984518e-03; eval[1][1][0][ 7][ 2] =   1.384439e-06;
  val[1][1][0][ 7][ 3] =   2.622808e-03; eval[1][1][0][ 7][ 3] =   1.147984e-06;
  val[1][1][0][ 7][ 4] =   2.868797e-03; eval[1][1][0][ 7][ 4] =   4.245795e-06;
  val[1][1][0][ 7][ 5] =   2.123956e-03; eval[1][1][0][ 7][ 5] =   8.287658e-07;
  val[1][1][0][ 7][ 6] =   1.989053e-03; eval[1][1][0][ 7][ 6] =   1.370478e-06;
  val[1][1][0][ 7][ 7] =   2.015763e-03; eval[1][1][0][ 7][ 7] =   3.298631e-06;
  val[1][1][0][ 7][ 8] =   1.900518e-03; eval[1][1][0][ 7][ 8] =   2.016598e-06;
  val[1][1][0][ 7][ 9] =   1.871496e-03; eval[1][1][0][ 7][ 9] =   2.578341e-06;
  val[1][1][0][ 7][10] =   1.834488e-03; eval[1][1][0][ 7][10] =   3.254131e-06;
  val[1][1][0][ 7][11] =   1.809661e-03; eval[1][1][0][ 7][11] =   4.115077e-06;
  val[1][1][0][ 7][12] =   1.858625e-03; eval[1][1][0][ 7][12] =   9.893477e-06;
  val[1][1][0][ 7][13] =   1.794284e-03; eval[1][1][0][ 7][13] =   6.936137e-06;
  val[1][1][0][ 7][14] =   1.817914e-03; eval[1][1][0][ 7][14] =   1.579955e-05;
  val[1][1][0][ 7][15] =   1.745473e-03; eval[1][1][0][ 7][15] =   1.060437e-05;
  val[1][1][0][ 7][16] =   1.904961e-03; eval[1][1][0][ 7][16] =   2.953473e-05;
  val[1][1][0][ 7][17] =   1.773818e-03; eval[1][1][0][ 7][17] =   1.754140e-05;
  val[1][1][0][ 7][18] =   1.714604e-03; eval[1][1][0][ 7][18] =   1.980500e-05;
  val[1][1][0][ 7][19] =   1.762754e-03; eval[1][1][0][ 7][19] =   2.575722e-05;
  val[1][1][0][ 7][20] =   1.792193e-03; eval[1][1][0][ 7][20] =   4.326664e-05;
  val[1][1][0][ 7][21] =   1.795897e-03; eval[1][1][0][ 7][21] =   6.217149e-05;
  val[1][1][0][ 7][22] =   1.709725e-03; eval[1][1][0][ 7][22] =   3.363638e-05;
  val[1][1][0][ 7][23] =   1.797389e-03; eval[1][1][0][ 7][23] =   5.546908e-05;
  val[1][1][0][ 7][24] =   1.796810e-03; eval[1][1][0][ 7][24] =   1.020467e-04;
  val[1][1][0][ 7][25] =   1.883520e-03; eval[1][1][0][ 7][25] =   1.021532e-04;
  val[1][1][0][ 7][26] =   1.643202e-03; eval[1][1][0][ 7][26] =   1.106118e-04;
  val[1][1][0][ 7][27] =   1.570721e-03; eval[1][1][0][ 7][27] =   1.145316e-04;
  val[1][1][0][ 7][28] =   1.516821e-03; eval[1][1][0][ 7][28] =   1.482726e-04;
  val[1][1][0][ 7][29] =   1.794560e-03; eval[1][1][0][ 7][29] =   3.048464e-04;
  //pc3dphi_sigma_c1d0z8
  n_val[1][1][0][8] = 30;
  val[1][1][0][ 8][ 0] =   0.000000e+00; eval[1][1][0][ 8][ 0] =   0.000000e+00;
  val[1][1][0][ 8][ 1] =   0.000000e+00; eval[1][1][0][ 8][ 1] =   0.000000e+00;
  val[1][1][0][ 8][ 2] =   2.899613e-03; eval[1][1][0][ 8][ 2] =   1.249932e-06;
  val[1][1][0][ 8][ 3] =   2.546265e-03; eval[1][1][0][ 8][ 3] =   1.024703e-06;
  val[1][1][0][ 8][ 4] =   2.732554e-03; eval[1][1][0][ 8][ 4] =   3.543329e-06;
  val[1][1][0][ 8][ 5] =   2.105789e-03; eval[1][1][0][ 8][ 5] =   8.019997e-07;
  val[1][1][0][ 8][ 6] =   1.963736e-03; eval[1][1][0][ 8][ 6] =   1.306416e-06;
  val[1][1][0][ 8][ 7] =   1.863995e-03; eval[1][1][0][ 8][ 7] =   1.484037e-06;
  val[1][1][0][ 8][ 8] =   1.969002e-03; eval[1][1][0][ 8][ 8] =   3.859606e-06;
  val[1][1][0][ 8][ 9] =   1.940119e-03; eval[1][1][0][ 8][ 9] =   4.890043e-06;
  val[1][1][0][ 8][10] =   1.891473e-03; eval[1][1][0][ 8][10] =   6.057746e-06;
  val[1][1][0][ 8][11] =   1.865437e-03; eval[1][1][0][ 8][11] =   7.644482e-06;
  val[1][1][0][ 8][12] =   1.738411e-03; eval[1][1][0][ 8][12] =   4.754005e-06;
  val[1][1][0][ 8][13] =   1.833367e-03; eval[1][1][0][ 8][13] =   1.249829e-05;
  val[1][1][0][ 8][14] =   1.707886e-03; eval[1][1][0][ 8][14] =   7.705763e-06;
  val[1][1][0][ 8][15] =   1.685398e-03; eval[1][1][0][ 8][15] =   9.549917e-06;
  val[1][1][0][ 8][16] =   1.806060e-03; eval[1][1][0][ 8][16] =   2.518118e-05;
  val[1][1][0][ 8][17] =   1.670864e-03; eval[1][1][0][ 8][17] =   1.494634e-05;
  val[1][1][0][ 8][18] =   1.606488e-03; eval[1][1][0][ 8][18] =   1.685519e-05;
  val[1][1][0][ 8][19] =   1.663886e-03; eval[1][1][0][ 8][19] =   2.213858e-05;
  val[1][1][0][ 8][20] =   1.761603e-03; eval[1][1][0][ 8][20] =   2.409226e-05;
  val[1][1][0][ 8][21] =   1.702969e-03; eval[1][1][0][ 8][21] =   5.294009e-05;
  val[1][1][0][ 8][22] =   1.571757e-03; eval[1][1][0][ 8][22] =   3.622762e-05;
  val[1][1][0][ 8][23] =   1.604420e-03; eval[1][1][0][ 8][23] =   4.898114e-05;
  val[1][1][0][ 8][24] =   1.789196e-03; eval[1][1][0][ 8][24] =   6.318709e-05;
  val[1][1][0][ 8][25] =   1.833550e-03; eval[1][1][0][ 8][25] =   1.142830e-04;
  val[1][1][0][ 8][26] =   1.881573e-03; eval[1][1][0][ 8][26] =   1.181873e-04;
  val[1][1][0][ 8][27] =   1.582493e-03; eval[1][1][0][ 8][27] =   1.221014e-04;
  val[1][1][0][ 8][28] =   1.885990e-03; eval[1][1][0][ 8][28] =   1.585481e-04;
  val[1][1][0][ 8][29] =   1.558380e-03; eval[1][1][0][ 8][29] =   1.675831e-04;
  //pc3dphi_sigma_c1d0z9
  n_val[1][1][0][9] = 30;
  val[1][1][0][ 9][ 0] =   0.000000e+00; eval[1][1][0][ 9][ 0] =   0.000000e+00;
  val[1][1][0][ 9][ 1] =   0.000000e+00; eval[1][1][0][ 9][ 1] =   0.000000e+00;
  val[1][1][0][ 9][ 2] =   2.964204e-03; eval[1][1][0][ 9][ 2] =   1.436236e-06;
  val[1][1][0][ 9][ 3] =   2.495287e-03; eval[1][1][0][ 9][ 3] =   9.785431e-07;
  val[1][1][0][ 9][ 4] =   2.623439e-03; eval[1][1][0][ 9][ 4] =   3.171557e-06;
  val[1][1][0][ 9][ 5] =   2.105411e-03; eval[1][1][0][ 9][ 5] =   8.074414e-07;
  val[1][1][0][ 9][ 6] =   1.980625e-03; eval[1][1][0][ 9][ 6] =   1.350975e-06;
  val[1][1][0][ 9][ 7] =   1.871254e-03; eval[1][1][0][ 9][ 7] =   1.512339e-06;
  val[1][1][0][ 9][ 8] =   1.854966e-03; eval[1][1][0][ 9][ 8] =   1.860774e-06;
  val[1][1][0][ 9][ 9] =   1.826800e-03; eval[1][1][0][ 9][ 9] =   2.362537e-06;
  val[1][1][0][ 9][10] =   1.796432e-03; eval[1][1][0][ 9][10] =   3.032277e-06;
  val[1][1][0][ 9][11] =   1.762348e-03; eval[1][1][0][ 9][11] =   3.827418e-06;
  val[1][1][0][ 9][12] =   1.757736e-03; eval[1][1][0][ 9][12] =   4.999517e-06;
  val[1][1][0][ 9][13] =   1.730378e-03; eval[1][1][0][ 9][13] =   6.239295e-06;
  val[1][1][0][ 9][14] =   1.832454e-03; eval[1][1][0][ 9][14] =   1.668888e-05;
  val[1][1][0][ 9][15] =   1.776143e-03; eval[1][1][0][ 9][15] =   1.946304e-05;
  val[1][1][0][ 9][16] =   1.716286e-03; eval[1][1][0][ 9][16] =   1.280117e-05;
  val[1][1][0][ 9][17] =   1.722525e-03; eval[1][1][0][ 9][17] =   1.597210e-05;
  val[1][1][0][ 9][18] =   1.812678e-03; eval[1][1][0][ 9][18] =   4.026441e-05;
  val[1][1][0][ 9][19] =   1.615981e-03; eval[1][1][0][ 9][19] =   2.081828e-05;
  val[1][1][0][ 9][20] =   1.734335e-03; eval[1][1][0][ 9][20] =   2.290982e-05;
  val[1][1][0][ 9][21] =   1.808948e-03; eval[1][1][0][ 9][21] =   6.544582e-05;
  val[1][1][0][ 9][22] =   1.619716e-03; eval[1][1][0][ 9][22] =   3.948984e-05;
  val[1][1][0][ 9][23] =   1.674780e-03; eval[1][1][0][ 9][23] =   5.665317e-05;
  val[1][1][0][ 9][24] =   1.756210e-03; eval[1][1][0][ 9][24] =   7.672995e-05;
  val[1][1][0][ 9][25] =   1.583335e-03; eval[1][1][0][ 9][25] =   8.655138e-05;
  val[1][1][0][ 9][26] =   1.685206e-03; eval[1][1][0][ 9][26] =   1.175990e-04;
  val[1][1][0][ 9][27] =   1.758855e-03; eval[1][1][0][ 9][27] =   1.608172e-04;
  val[1][1][0][ 9][28] =   1.739847e-03; eval[1][1][0][ 9][28] =   1.800469e-04;
  val[1][1][0][ 9][29] =   2.208799e-03; eval[1][1][0][ 9][29] =   2.585262e-04;
  //pc3dphi_sigma_c1d1z0
  n_val[1][1][1][0] = 30;
  val[1][1][1][ 0][ 0] =   0.000000e+00; eval[1][1][1][ 0][ 0] =   0.000000e+00;
  val[1][1][1][ 0][ 1] =   0.000000e+00; eval[1][1][1][ 0][ 1] =   0.000000e+00;
  val[1][1][1][ 0][ 2] =   2.480201e-03; eval[1][1][1][ 0][ 2] =   8.573179e-07;
  val[1][1][1][ 0][ 3] =   2.064171e-03; eval[1][1][1][ 0][ 3] =   5.870114e-07;
  val[1][1][1][ 0][ 4] =   1.753640e-03; eval[1][1][1][ 0][ 4] =   8.448640e-07;
  val[1][1][1][ 0][ 5] =   1.645359e-03; eval[1][1][1][ 0][ 5] =   9.368058e-07;
  val[1][1][1][ 0][ 6] =   1.576927e-03; eval[1][1][1][ 0][ 6] =   1.117973e-06;
  val[1][1][1][ 0][ 7] =   1.542432e-03; eval[1][1][1][ 0][ 7] =   1.401940e-06;
  val[1][1][1][ 0][ 8] =   1.531953e-03; eval[1][1][1][ 0][ 8] =   1.825701e-06;
  val[1][1][1][ 0][ 9] =   1.498051e-03; eval[1][1][1][ 0][ 9] =   2.335203e-06;
  val[1][1][1][ 0][10] =   1.461308e-03; eval[1][1][1][ 0][10] =   2.948856e-06;
  val[1][1][1][ 0][11] =   1.438005e-03; eval[1][1][1][ 0][11] =   3.714278e-06;
  val[1][1][1][ 0][12] =   1.436499e-03; eval[1][1][1][ 0][12] =   4.847202e-06;
  val[1][1][1][ 0][13] =   1.421190e-03; eval[1][1][1][ 0][13] =   6.107016e-06;
  val[1][1][1][ 0][14] =   1.408426e-03; eval[1][1][1][ 0][14] =   7.631880e-06;
  val[1][1][1][ 0][15] =   1.401183e-03; eval[1][1][1][ 0][15] =   9.529776e-06;
  val[1][1][1][ 0][16] =   1.408473e-03; eval[1][1][1][ 0][16] =   1.206184e-05;
  val[1][1][1][ 0][17] =   1.393697e-03; eval[1][1][1][ 0][17] =   1.462027e-05;
  val[1][1][1][ 0][18] =   1.397660e-03; eval[1][1][1][ 0][18] =   1.811499e-05;
  val[1][1][1][ 0][19] =   1.376840e-03; eval[1][1][1][ 0][19] =   2.092602e-05;
  val[1][1][1][ 0][20] =   1.417705e-03; eval[1][1][1][ 0][20] =   2.086246e-05;
  val[1][1][1][ 0][21] =   1.378304e-03; eval[1][1][1][ 0][21] =   2.665528e-05;
  val[1][1][1][ 0][22] =   1.367710e-03; eval[1][1][1][ 0][22] =   3.571944e-05;
  val[1][1][1][ 0][23] =   1.372669e-03; eval[1][1][1][ 0][23] =   4.774469e-05;
  val[1][1][1][ 0][24] =   1.521605e-03; eval[1][1][1][ 0][24] =   8.136259e-05;
  val[1][1][1][ 0][25] =   1.648280e-03; eval[1][1][1][ 0][25] =   9.665351e-05;
  val[1][1][1][ 0][26] =   1.373870e-03; eval[1][1][1][ 0][26] =   9.899918e-05;
  val[1][1][1][ 0][27] =   1.866925e-03; eval[1][1][1][ 0][27] =   1.587939e-04;
  val[1][1][1][ 0][28] =   1.322174e-03; eval[1][1][1][ 0][28] =   1.274941e-04;
  val[1][1][1][ 0][29] =   4.146275e-02; eval[1][1][1][ 0][29] =   4.349981e-02;
  //pc3dphi_sigma_c1d1z1
  n_val[1][1][1][1] = 30;
  val[1][1][1][ 1][ 0] =   0.000000e+00; eval[1][1][1][ 1][ 0] =   0.000000e+00;
  val[1][1][1][ 1][ 1] =   0.000000e+00; eval[1][1][1][ 1][ 1] =   0.000000e+00;
  val[1][1][1][ 1][ 2] =   2.462883e-03; eval[1][1][1][ 1][ 2] =   7.773249e-07;
  val[1][1][1][ 1][ 3] =   1.944068e-03; eval[1][1][1][ 1][ 3] =   8.405770e-07;
  val[1][1][1][ 1][ 4] =   1.756638e-03; eval[1][1][1][ 1][ 4] =   7.863713e-07;
  val[1][1][1][ 1][ 5] =   1.650565e-03; eval[1][1][1][ 1][ 5] =   8.851146e-07;
  val[1][1][1][ 1][ 6] =   1.574056e-03; eval[1][1][1][ 1][ 6] =   1.034172e-06;
  val[1][1][1][ 1][ 7] =   1.533392e-03; eval[1][1][1][ 1][ 7] =   1.287068e-06;
  val[1][1][1][ 1][ 8] =   1.516375e-03; eval[1][1][1][ 1][ 8] =   1.644995e-06;
  val[1][1][1][ 1][ 9] =   1.482655e-03; eval[1][1][1][ 1][ 9] =   2.101418e-06;
  val[1][1][1][ 1][10] =   1.455858e-03; eval[1][1][1][ 1][10] =   2.683517e-06;
  val[1][1][1][ 1][11] =   1.439789e-03; eval[1][1][1][ 1][11] =   3.436522e-06;
  val[1][1][1][ 1][12] =   1.418612e-03; eval[1][1][1][ 1][12] =   4.362491e-06;
  val[1][1][1][ 1][13] =   1.414187e-03; eval[1][1][1][ 1][13] =   5.658539e-06;
  val[1][1][1][ 1][14] =   1.406611e-03; eval[1][1][1][ 1][14] =   7.163642e-06;
  val[1][1][1][ 1][15] =   1.405146e-03; eval[1][1][1][ 1][15] =   9.091595e-06;
  val[1][1][1][ 1][16] =   1.405890e-03; eval[1][1][1][ 1][16] =   1.144232e-05;
  val[1][1][1][ 1][17] =   1.413229e-03; eval[1][1][1][ 1][17] =   1.441172e-05;
  val[1][1][1][ 1][18] =   1.394101e-03; eval[1][1][1][ 1][18] =   1.729246e-05;
  val[1][1][1][ 1][19] =   1.398962e-03; eval[1][1][1][ 1][19] =   2.108815e-05;
  val[1][1][1][ 1][20] =   1.380549e-03; eval[1][1][1][ 1][20] =   1.853199e-05;
  val[1][1][1][ 1][21] =   1.432488e-03; eval[1][1][1][ 1][21] =   2.864724e-05;
  val[1][1][1][ 1][22] =   1.382356e-03; eval[1][1][1][ 1][22] =   3.618229e-05;
  val[1][1][1][ 1][23] =   1.364331e-03; eval[1][1][1][ 1][23] =   4.502310e-05;
  val[1][1][1][ 1][24] =   1.444923e-03; eval[1][1][1][ 1][24] =   6.738146e-05;
  val[1][1][1][ 1][25] =   1.313994e-03; eval[1][1][1][ 1][25] =   6.795995e-05;
  val[1][1][1][ 1][26] =   1.337095e-03; eval[1][1][1][ 1][26] =   8.473458e-05;
  val[1][1][1][ 1][27] =   1.445458e-03; eval[1][1][1][ 1][27] =   1.229010e-04;
  val[1][1][1][ 1][28] =   1.697506e-03; eval[1][1][1][ 1][28] =   1.638257e-04;
  val[1][1][1][ 1][29] =   1.219887e-03; eval[1][1][1][ 1][29] =   1.029657e-04;
  //pc3dphi_sigma_c1d1z2
  n_val[1][1][1][2] = 30;
  val[1][1][1][ 2][ 0] =   0.000000e+00; eval[1][1][1][ 2][ 0] =   0.000000e+00;
  val[1][1][1][ 2][ 1] =   0.000000e+00; eval[1][1][1][ 2][ 1] =   0.000000e+00;
  val[1][1][1][ 2][ 2] =   2.209450e-03; eval[1][1][1][ 2][ 2] =   1.118153e-06;
  val[1][1][1][ 2][ 3] =   1.925989e-03; eval[1][1][1][ 2][ 3] =   8.222513e-07;
  val[1][1][1][ 2][ 4] =   1.746962e-03; eval[1][1][1][ 2][ 4] =   7.868677e-07;
  val[1][1][1][ 2][ 5] =   1.638168e-03; eval[1][1][1][ 2][ 5] =   8.725719e-07;
  val[1][1][1][ 2][ 6] =   1.561635e-03; eval[1][1][1][ 2][ 6] =   1.028831e-06;
  val[1][1][1][ 2][ 7] =   1.518588e-03; eval[1][1][1][ 2][ 7] =   1.291195e-06;
  val[1][1][1][ 2][ 8] =   1.501254e-03; eval[1][1][1][ 2][ 8] =   1.652736e-06;
  val[1][1][1][ 2][ 9] =   1.471847e-03; eval[1][1][1][ 2][ 9] =   2.139983e-06;
  val[1][1][1][ 2][10] =   1.448146e-03; eval[1][1][1][ 2][10] =   2.725003e-06;
  val[1][1][1][ 2][11] =   1.429041e-03; eval[1][1][1][ 2][11] =   3.452161e-06;
  val[1][1][1][ 2][12] =   1.416081e-03; eval[1][1][1][ 2][12] =   4.417358e-06;
  val[1][1][1][ 2][13] =   1.398790e-03; eval[1][1][1][ 2][13] =   5.553790e-06;
  val[1][1][1][ 2][14] =   1.393318e-03; eval[1][1][1][ 2][14] =   7.007656e-06;
  val[1][1][1][ 2][15] =   1.376052e-03; eval[1][1][1][ 2][15] =   8.604348e-06;
  val[1][1][1][ 2][16] =   1.377340e-03; eval[1][1][1][ 2][16] =   1.071100e-05;
  val[1][1][1][ 2][17] =   1.377447e-03; eval[1][1][1][ 2][17] =   1.325929e-05;
  val[1][1][1][ 2][18] =   1.415274e-03; eval[1][1][1][ 2][18] =   1.785252e-05;
  val[1][1][1][ 2][19] =   1.367630e-03; eval[1][1][1][ 2][19] =   1.972925e-05;
  val[1][1][1][ 2][20] =   1.374266e-03; eval[1][1][1][ 2][20] =   1.796190e-05;
  val[1][1][1][ 2][21] =   1.394623e-03; eval[1][1][1][ 2][21] =   2.657007e-05;
  val[1][1][1][ 2][22] =   1.474634e-03; eval[1][1][1][ 2][22] =   4.240976e-05;
  val[1][1][1][ 2][23] =   1.404629e-03; eval[1][1][1][ 2][23] =   4.965038e-05;
  val[1][1][1][ 2][24] =   1.293406e-03; eval[1][1][1][ 2][24] =   5.267013e-05;
  val[1][1][1][ 2][25] =   1.466376e-03; eval[1][1][1][ 2][25] =   9.016395e-05;
  val[1][1][1][ 2][26] =   1.511664e-03; eval[1][1][1][ 2][26] =   1.147944e-04;
  val[1][1][1][ 2][27] =   1.616655e-03; eval[1][1][1][ 2][27] =   1.706969e-04;
  val[1][1][1][ 2][28] =   1.676310e-03; eval[1][1][1][ 2][28] =   1.628839e-04;
  val[1][1][1][ 2][29] =   1.745696e-03; eval[1][1][1][ 2][29] =   2.735040e-04;
  //pc3dphi_sigma_c1d1z3
  n_val[1][1][1][3] = 30;
  val[1][1][1][ 3][ 0] =   0.000000e+00; eval[1][1][1][ 3][ 0] =   0.000000e+00;
  val[1][1][1][ 3][ 1] =   0.000000e+00; eval[1][1][1][ 3][ 1] =   0.000000e+00;
  val[1][1][1][ 3][ 2] =   2.682882e-03; eval[1][1][1][ 3][ 2] =   1.010091e-06;
  val[1][1][1][ 3][ 3] =   1.931088e-03; eval[1][1][1][ 3][ 3] =   8.495653e-07;
  val[1][1][1][ 3][ 4] =   1.757167e-03; eval[1][1][1][ 3][ 4] =   8.012432e-07;
  val[1][1][1][ 3][ 5] =   1.635289e-03; eval[1][1][1][ 3][ 5] =   8.675140e-07;
  val[1][1][1][ 3][ 6] =   1.567515e-03; eval[1][1][1][ 3][ 6] =   1.027147e-06;
  val[1][1][1][ 3][ 7] =   1.529531e-03; eval[1][1][1][ 3][ 7] =   1.291572e-06;
  val[1][1][1][ 3][ 8] =   1.507784e-03; eval[1][1][1][ 3][ 8] =   1.648493e-06;
  val[1][1][1][ 3][ 9] =   1.475725e-03; eval[1][1][1][ 3][ 9] =   2.148026e-06;
  val[1][1][1][ 3][10] =   1.456715e-03; eval[1][1][1][ 3][10] =   2.778801e-06;
  val[1][1][1][ 3][11] =   1.441794e-03; eval[1][1][1][ 3][11] =   3.613360e-06;
  val[1][1][1][ 3][12] =   1.437224e-03; eval[1][1][1][ 3][12] =   4.736595e-06;
  val[1][1][1][ 3][13] =   1.429162e-03; eval[1][1][1][ 3][13] =   6.071438e-06;
  val[1][1][1][ 3][14] =   1.416243e-03; eval[1][1][1][ 3][14] =   7.618144e-06;
  val[1][1][1][ 3][15] =   1.404694e-03; eval[1][1][1][ 3][15] =   9.423626e-06;
  val[1][1][1][ 3][16] =   1.417456e-03; eval[1][1][1][ 3][16] =   1.214808e-05;
  val[1][1][1][ 3][17] =   1.420952e-03; eval[1][1][1][ 3][17] =   1.520035e-05;
  val[1][1][1][ 3][18] =   1.443141e-03; eval[1][1][1][ 3][18] =   1.957998e-05;
  val[1][1][1][ 3][19] =   1.382996e-03; eval[1][1][1][ 3][19] =   2.083090e-05;
  val[1][1][1][ 3][20] =   1.416130e-03; eval[1][1][1][ 3][20] =   2.078148e-05;
  val[1][1][1][ 3][21] =   1.396692e-03; eval[1][1][1][ 3][21] =   2.833531e-05;
  val[1][1][1][ 3][22] =   1.396145e-03; eval[1][1][1][ 3][22] =   3.826365e-05;
  val[1][1][1][ 3][23] =   1.419509e-03; eval[1][1][1][ 3][23] =   5.175396e-05;
  val[1][1][1][ 3][24] =   1.400867e-03; eval[1][1][1][ 3][24] =   6.424458e-05;
  val[1][1][1][ 3][25] =   1.418604e-03; eval[1][1][1][ 3][25] =   9.119244e-05;
  val[1][1][1][ 3][26] =   1.308703e-03; eval[1][1][1][ 3][26] =   8.258674e-05;
  val[1][1][1][ 3][27] =   1.176208e-03; eval[1][1][1][ 3][27] =   7.347172e-05;
  val[1][1][1][ 3][28] =   1.243339e-03; eval[1][1][1][ 3][28] =   1.083356e-04;
  val[1][1][1][ 3][29] =   1.652779e-03; eval[1][1][1][ 3][29] =   1.918625e-04;
  //pc3dphi_sigma_c1d1z4
  n_val[1][1][1][4] = 30;
  val[1][1][1][ 4][ 0] =   0.000000e+00; eval[1][1][1][ 4][ 0] =   0.000000e+00;
  val[1][1][1][ 4][ 1] =   0.000000e+00; eval[1][1][1][ 4][ 1] =   0.000000e+00;
  val[1][1][1][ 4][ 2] =   2.632801e-03; eval[1][1][1][ 4][ 2] =   1.056550e-06;
  val[1][1][1][ 4][ 3] =   1.897993e-03; eval[1][1][1][ 4][ 3] =   9.374474e-07;
  val[1][1][1][ 4][ 4] =   1.731017e-03; eval[1][1][1][ 4][ 4] =   9.125463e-07;
  val[1][1][1][ 4][ 5] =   1.609852e-03; eval[1][1][1][ 4][ 5] =   9.903413e-07;
  val[1][1][1][ 4][ 6] =   1.543460e-03; eval[1][1][1][ 4][ 6] =   1.198904e-06;
  val[1][1][1][ 4][ 7] =   1.493452e-03; eval[1][1][1][ 4][ 7] =   1.459172e-06;
  val[1][1][1][ 4][ 8] =   1.470443e-03; eval[1][1][1][ 4][ 8] =   1.835024e-06;
  val[1][1][1][ 4][ 9] =   1.438154e-03; eval[1][1][1][ 4][ 9] =   2.398293e-06;
  val[1][1][1][ 4][10] =   1.428599e-03; eval[1][1][1][ 4][10] =   3.186552e-06;
  val[1][1][1][ 4][11] =   1.407367e-03; eval[1][1][1][ 4][11] =   4.120341e-06;
  val[1][1][1][ 4][12] =   1.378816e-03; eval[1][1][1][ 4][12] =   5.139087e-06;
  val[1][1][1][ 4][13] =   1.382055e-03; eval[1][1][1][ 4][13] =   6.760428e-06;
  val[1][1][1][ 4][14] =   1.368952e-03; eval[1][1][1][ 4][14] =   8.452012e-06;
  val[1][1][1][ 4][15] =   1.402010e-03; eval[1][1][1][ 4][15] =   1.141120e-05;
  val[1][1][1][ 4][16] =   1.381522e-03; eval[1][1][1][ 4][16] =   1.387416e-05;
  val[1][1][1][ 4][17] =   1.347967e-03; eval[1][1][1][ 4][17] =   1.602014e-05;
  val[1][1][1][ 4][18] =   1.349348e-03; eval[1][1][1][ 4][18] =   1.969875e-05;
  val[1][1][1][ 4][19] =   1.350643e-03; eval[1][1][1][ 4][19] =   2.408164e-05;
  val[1][1][1][ 4][20] =   1.349560e-03; eval[1][1][1][ 4][20] =   2.213488e-05;
  val[1][1][1][ 4][21] =   1.347774e-03; eval[1][1][1][ 4][21] =   3.185946e-05;
  val[1][1][1][ 4][22] =   1.364233e-03; eval[1][1][1][ 4][22] =   4.376883e-05;
  val[1][1][1][ 4][23] =   1.382081e-03; eval[1][1][1][ 4][23] =   6.488828e-05;
  val[1][1][1][ 4][24] =   1.326447e-03; eval[1][1][1][ 4][24] =   6.673813e-05;
  val[1][1][1][ 4][25] =   1.666442e-03; eval[1][1][1][ 4][25] =   1.542046e-04;
  val[1][1][1][ 4][26] =   1.479369e-03; eval[1][1][1][ 4][26] =   9.650700e-05;
  val[1][1][1][ 4][27] =   1.279295e-03; eval[1][1][1][ 4][27] =   1.304030e-04;
  val[1][1][1][ 4][28] =   1.545585e-03; eval[1][1][1][ 4][28] =   1.838623e-04;
  val[1][1][1][ 4][29] =   1.244633e-03; eval[1][1][1][ 4][29] =   1.701920e-04;
  //pc3dphi_sigma_c1d1z5
  n_val[1][1][1][5] = 30;
  val[1][1][1][ 5][ 0] =   0.000000e+00; eval[1][1][1][ 5][ 0] =   0.000000e+00;
  val[1][1][1][ 5][ 1] =   0.000000e+00; eval[1][1][1][ 5][ 1] =   0.000000e+00;
  val[1][1][1][ 5][ 2] =   2.649992e-03; eval[1][1][1][ 5][ 2] =   1.075190e-06;
  val[1][1][1][ 5][ 3] =   2.210008e-03; eval[1][1][1][ 5][ 3] =   7.686336e-07;
  val[1][1][1][ 5][ 4] =   1.783572e-03; eval[1][1][1][ 5][ 4] =   9.888296e-07;
  val[1][1][1][ 5][ 5] =   1.799324e-03; eval[1][1][1][ 5][ 5] =   7.882512e-07;
  val[1][1][1][ 5][ 6] =   1.691568e-03; eval[1][1][1][ 5][ 6] =   9.478355e-07;
  val[1][1][1][ 5][ 7] =   1.530026e-03; eval[1][1][1][ 5][ 7] =   1.534055e-06;
  val[1][1][1][ 5][ 8] =   1.498440e-03; eval[1][1][1][ 5][ 8] =   1.928322e-06;
  val[1][1][1][ 5][ 9] =   1.469391e-03; eval[1][1][1][ 5][ 9] =   2.549220e-06;
  val[1][1][1][ 5][10] =   1.464690e-03; eval[1][1][1][ 5][10] =   3.446388e-06;
  val[1][1][1][ 5][11] =   1.447989e-03; eval[1][1][1][ 5][11] =   4.471037e-06;
  val[1][1][1][ 5][12] =   1.440251e-03; eval[1][1][1][ 5][12] =   5.795992e-06;
  val[1][1][1][ 5][13] =   1.419865e-03; eval[1][1][1][ 5][13] =   7.296097e-06;
  val[1][1][1][ 5][14] =   1.421540e-03; eval[1][1][1][ 5][14] =   9.345881e-06;
  val[1][1][1][ 5][15] =   1.411944e-03; eval[1][1][1][ 5][15] =   1.173499e-05;
  val[1][1][1][ 5][16] =   1.390904e-03; eval[1][1][1][ 5][16] =   1.406674e-05;
  val[1][1][1][ 5][17] =   1.364547e-03; eval[1][1][1][ 5][17] =   1.661445e-05;
  val[1][1][1][ 5][18] =   1.400868e-03; eval[1][1][1][ 5][18] =   2.213360e-05;
  val[1][1][1][ 5][19] =   1.420322e-03; eval[1][1][1][ 5][19] =   2.796969e-05;
  val[1][1][1][ 5][20] =   1.355466e-03; eval[1][1][1][ 5][20] =   2.255206e-05;
  val[1][1][1][ 5][21] =   1.346371e-03; eval[1][1][1][ 5][21] =   3.092388e-05;
  val[1][1][1][ 5][22] =   1.378865e-03; eval[1][1][1][ 5][22] =   4.547655e-05;
  val[1][1][1][ 5][23] =   1.304816e-03; eval[1][1][1][ 5][23] =   5.175671e-05;
  val[1][1][1][ 5][24] =   1.367617e-03; eval[1][1][1][ 5][24] =   7.382111e-05;
  val[1][1][1][ 5][25] =   1.659098e-03; eval[1][1][1][ 5][25] =   1.108815e-04;
  val[1][1][1][ 5][26] =   1.365308e-03; eval[1][1][1][ 5][26] =   1.215571e-04;
  val[1][1][1][ 5][27] =   1.500353e-03; eval[1][1][1][ 5][27] =   1.393969e-04;
  val[1][1][1][ 5][28] =   1.314037e-03; eval[1][1][1][ 5][28] =   1.645950e-04;
  val[1][1][1][ 5][29] =   1.717419e-03; eval[1][1][1][ 5][29] =   2.179761e-04;
  //pc3dphi_sigma_c1d1z6
  n_val[1][1][1][6] = 30;
  val[1][1][1][ 6][ 0] =   0.000000e+00; eval[1][1][1][ 6][ 0] =   0.000000e+00;
  val[1][1][1][ 6][ 1] =   0.000000e+00; eval[1][1][1][ 6][ 1] =   0.000000e+00;
  val[1][1][1][ 6][ 2] =   2.748928e-03; eval[1][1][1][ 6][ 2] =   1.043844e-06;
  val[1][1][1][ 6][ 3] =   1.976717e-03; eval[1][1][1][ 6][ 3] =   8.990498e-07;
  val[1][1][1][ 6][ 4] =   1.805841e-03; eval[1][1][1][ 6][ 4] =   8.579210e-07;
  val[1][1][1][ 6][ 5] =   1.839133e-03; eval[1][1][1][ 6][ 5] =   7.529616e-07;
  val[1][1][1][ 6][ 6] =   1.595197e-03; eval[1][1][1][ 6][ 6] =   1.112135e-06;
  val[1][1][1][ 6][ 7] =   1.543549e-03; eval[1][1][1][ 6][ 7] =   1.369517e-06;
  val[1][1][1][ 6][ 8] =   1.517645e-03; eval[1][1][1][ 6][ 8] =   1.734189e-06;
  val[1][1][1][ 6][ 9] =   1.489377e-03; eval[1][1][1][ 6][ 9] =   2.289377e-06;
  val[1][1][1][ 6][10] =   1.483216e-03; eval[1][1][1][ 6][10] =   3.095583e-06;
  val[1][1][1][ 6][11] =   1.463701e-03; eval[1][1][1][ 6][11] =   3.998147e-06;
  val[1][1][1][ 6][12] =   1.460004e-03; eval[1][1][1][ 6][12] =   5.304104e-06;
  val[1][1][1][ 6][13] =   1.447200e-03; eval[1][1][1][ 6][13] =   6.769194e-06;
  val[1][1][1][ 6][14] =   1.441992e-03; eval[1][1][1][ 6][14] =   8.659871e-06;
  val[1][1][1][ 6][15] =   1.414275e-03; eval[1][1][1][ 6][15] =   1.044454e-05;
  val[1][1][1][ 6][16] =   1.436978e-03; eval[1][1][1][ 6][16] =   1.368871e-05;
  val[1][1][1][ 6][17] =   1.426438e-03; eval[1][1][1][ 6][17] =   1.659269e-05;
  val[1][1][1][ 6][18] =   1.412518e-03; eval[1][1][1][ 6][18] =   2.014439e-05;
  val[1][1][1][ 6][19] =   1.405983e-03; eval[1][1][1][ 6][19] =   2.359564e-05;
  val[1][1][1][ 6][20] =   1.422402e-03; eval[1][1][1][ 6][20] =   2.294059e-05;
  val[1][1][1][ 6][21] =   1.502905e-03; eval[1][1][1][ 6][21] =   3.877255e-05;
  val[1][1][1][ 6][22] =   1.372207e-03; eval[1][1][1][ 6][22] =   4.118396e-05;
  val[1][1][1][ 6][23] =   1.363399e-03; eval[1][1][1][ 6][23] =   5.162564e-05;
  val[1][1][1][ 6][24] =   1.550220e-03; eval[1][1][1][ 6][24] =   6.716446e-05;
  val[1][1][1][ 6][25] =   1.305729e-03; eval[1][1][1][ 6][25] =   7.604633e-05;
  val[1][1][1][ 6][26] =   1.344686e-03; eval[1][1][1][ 6][26] =   1.025935e-04;
  val[1][1][1][ 6][27] =   1.587548e-03; eval[1][1][1][ 6][27] =   1.441301e-04;
  val[1][1][1][ 6][28] =   1.161835e-03; eval[1][1][1][ 6][28] =   1.046504e-04;
  val[1][1][1][ 6][29] =   1.845343e-03; eval[1][1][1][ 6][29] =   2.181995e-04;
  //pc3dphi_sigma_c1d1z7
  n_val[1][1][1][7] = 30;
  val[1][1][1][ 7][ 0] =   0.000000e+00; eval[1][1][1][ 7][ 0] =   0.000000e+00;
  val[1][1][1][ 7][ 1] =   0.000000e+00; eval[1][1][1][ 7][ 1] =   0.000000e+00;
  val[1][1][1][ 7][ 2] =   2.257674e-03; eval[1][1][1][ 7][ 2] =   1.152345e-06;
  val[1][1][1][ 7][ 3] =   1.949571e-03; eval[1][1][1][ 7][ 3] =   8.200721e-07;
  val[1][1][1][ 7][ 4] =   1.778685e-03; eval[1][1][1][ 7][ 4] =   8.066260e-07;
  val[1][1][1][ 7][ 5] =   1.672803e-03; eval[1][1][1][ 7][ 5] =   8.921366e-07;
  val[1][1][1][ 7][ 6] =   1.587472e-03; eval[1][1][1][ 7][ 6] =   1.057962e-06;
  val[1][1][1][ 7][ 7] =   1.546995e-03; eval[1][1][1][ 7][ 7] =   1.320983e-06;
  val[1][1][1][ 7][ 8] =   1.522879e-03; eval[1][1][1][ 7][ 8] =   1.654315e-06;
  val[1][1][1][ 7][ 9] =   1.497440e-03; eval[1][1][1][ 7][ 9] =   2.177689e-06;
  val[1][1][1][ 7][10] =   1.479109e-03; eval[1][1][1][ 7][10] =   2.873936e-06;
  val[1][1][1][ 7][11] =   1.478086e-03; eval[1][1][1][ 7][11] =   3.851574e-06;
  val[1][1][1][ 7][12] =   1.483587e-03; eval[1][1][1][ 7][12] =   5.171789e-06;
  val[1][1][1][ 7][13] =   1.473918e-03; eval[1][1][1][ 7][13] =   6.736891e-06;
  val[1][1][1][ 7][14] =   1.468456e-03; eval[1][1][1][ 7][14] =   8.644067e-06;
  val[1][1][1][ 7][15] =   1.460547e-03; eval[1][1][1][ 7][15] =   1.085517e-05;
  val[1][1][1][ 7][16] =   1.431923e-03; eval[1][1][1][ 7][16] =   1.297479e-05;
  val[1][1][1][ 7][17] =   1.432408e-03; eval[1][1][1][ 7][17] =   1.616668e-05;
  val[1][1][1][ 7][18] =   1.444742e-03; eval[1][1][1][ 7][18] =   2.049681e-05;
  val[1][1][1][ 7][19] =   1.445540e-03; eval[1][1][1][ 7][19] =   2.500945e-05;
  val[1][1][1][ 7][20] =   1.438457e-03; eval[1][1][1][ 7][20] =   2.260482e-05;
  val[1][1][1][ 7][21] =   1.462862e-03; eval[1][1][1][ 7][21] =   3.441325e-05;
  val[1][1][1][ 7][22] =   1.690711e-03; eval[1][1][1][ 7][22] =   4.726995e-05;
  val[1][1][1][ 7][23] =   1.617689e-03; eval[1][1][1][ 7][23] =   5.362507e-05;
  val[1][1][1][ 7][24] =   1.662892e-03; eval[1][1][1][ 7][24] =   7.070464e-05;
  val[1][1][1][ 7][25] =   1.840353e-03; eval[1][1][1][ 7][25] =   1.117969e-04;
  val[1][1][1][ 7][26] =   1.729387e-03; eval[1][1][1][ 7][26] =   1.204035e-04;
  val[1][1][1][ 7][27] =   1.764712e-03; eval[1][1][1][ 7][27] =   1.559848e-04;
  val[1][1][1][ 7][28] =   1.877522e-03; eval[1][1][1][ 7][28] =   1.999731e-04;
  val[1][1][1][ 7][29] =   1.297058e-03; eval[1][1][1][ 7][29] =   1.675837e-04;
  //pc3dphi_sigma_c1d1z8
  n_val[1][1][1][8] = 30;
  val[1][1][1][ 8][ 0] =   0.000000e+00; eval[1][1][1][ 8][ 0] =   0.000000e+00;
  val[1][1][1][ 8][ 1] =   0.000000e+00; eval[1][1][1][ 8][ 1] =   0.000000e+00;
  val[1][1][1][ 8][ 2] =   2.479096e-03; eval[1][1][1][ 8][ 2] =   6.996070e-07;
  val[1][1][1][ 8][ 3] =   1.969463e-03; eval[1][1][1][ 8][ 3] =   8.362888e-07;
  val[1][1][1][ 8][ 4] =   1.772656e-03; eval[1][1][1][ 8][ 4] =   7.882562e-07;
  val[1][1][1][ 8][ 5] =   1.678357e-03; eval[1][1][1][ 8][ 5] =   9.030846e-07;
  val[1][1][1][ 8][ 6] =   1.605911e-03; eval[1][1][1][ 8][ 6] =   1.099251e-06;
  val[1][1][1][ 8][ 7] =   1.561128e-03; eval[1][1][1][ 8][ 7] =   1.370463e-06;
  val[1][1][1][ 8][ 8] =   1.529718e-03; eval[1][1][1][ 8][ 8] =   1.687024e-06;
  val[1][1][1][ 8][ 9] =   1.508269e-03; eval[1][1][1][ 8][ 9] =   2.220396e-06;
  val[1][1][1][ 8][10] =   1.499382e-03; eval[1][1][1][ 8][10] =   2.986174e-06;
  val[1][1][1][ 8][11] =   1.486708e-03; eval[1][1][1][ 8][11] =   3.909272e-06;
  val[1][1][1][ 8][12] =   1.488289e-03; eval[1][1][1][ 8][12] =   5.249027e-06;
  val[1][1][1][ 8][13] =   1.469073e-03; eval[1][1][1][ 8][13] =   6.657821e-06;
  val[1][1][1][ 8][14] =   1.470590e-03; eval[1][1][1][ 8][14] =   8.679868e-06;
  val[1][1][1][ 8][15] =   1.466113e-03; eval[1][1][1][ 8][15] =   1.115938e-05;
  val[1][1][1][ 8][16] =   1.478335e-03; eval[1][1][1][ 8][16] =   1.433627e-05;
  val[1][1][1][ 8][17] =   1.441909e-03; eval[1][1][1][ 8][17] =   1.673166e-05;
  val[1][1][1][ 8][18] =   1.479285e-03; eval[1][1][1][ 8][18] =   2.180357e-05;
  val[1][1][1][ 8][19] =   1.414201e-03; eval[1][1][1][ 8][19] =   2.417598e-05;
  val[1][1][1][ 8][20] =   1.440549e-03; eval[1][1][1][ 8][20] =   2.325476e-05;
  val[1][1][1][ 8][21] =   1.443793e-03; eval[1][1][1][ 8][21] =   3.272989e-05;
  val[1][1][1][ 8][22] =   1.690763e-03; eval[1][1][1][ 8][22] =   5.210542e-05;
  val[1][1][1][ 8][23] =   1.354288e-03; eval[1][1][1][ 8][23] =   4.909741e-05;
  val[1][1][1][ 8][24] =   1.669573e-03; eval[1][1][1][ 8][24] =   7.971984e-05;
  val[1][1][1][ 8][25] =   1.317732e-03; eval[1][1][1][ 8][25] =   7.643686e-05;
  val[1][1][1][ 8][26] =   1.357230e-03; eval[1][1][1][ 8][26] =   9.392568e-05;
  val[1][1][1][ 8][27] =   2.088091e-03; eval[1][1][1][ 8][27] =   1.873690e-04;
  val[1][1][1][ 8][28] =   1.383597e-03; eval[1][1][1][ 8][28] =   1.676709e-04;
  val[1][1][1][ 8][29] =   2.524880e-04; eval[1][1][1][ 8][29] =   8.645513e-04;
  //pc3dphi_sigma_c1d1z9
  n_val[1][1][1][9] = 30;
  val[1][1][1][ 9][ 0] =   0.000000e+00; eval[1][1][1][ 9][ 0] =   0.000000e+00;
  val[1][1][1][ 9][ 1] =   0.000000e+00; eval[1][1][1][ 9][ 1] =   0.000000e+00;
  val[1][1][1][ 9][ 2] =   2.628045e-03; eval[1][1][1][ 9][ 2] =   1.819482e-06;
  val[1][1][1][ 9][ 3] =   2.090719e-03; eval[1][1][1][ 9][ 3] =   5.655875e-07;
  val[1][1][1][ 9][ 4] =   1.782691e-03; eval[1][1][1][ 9][ 4] =   8.843090e-07;
  val[1][1][1][ 9][ 5] =   1.692034e-03; eval[1][1][1][ 9][ 5] =   1.022718e-06;
  val[1][1][1][ 9][ 6] =   1.613538e-03; eval[1][1][1][ 9][ 6] =   1.234303e-06;
  val[1][1][1][ 9][ 7] =   1.584037e-03; eval[1][1][1][ 9][ 7] =   1.577845e-06;
  val[1][1][1][ 9][ 8] =   1.549757e-03; eval[1][1][1][ 9][ 8] =   1.935477e-06;
  val[1][1][1][ 9][ 9] =   1.531103e-03; eval[1][1][1][ 9][ 9] =   2.566127e-06;
  val[1][1][1][ 9][10] =   1.522637e-03; eval[1][1][1][ 9][10] =   3.478894e-06;
  val[1][1][1][ 9][11] =   1.497470e-03; eval[1][1][1][ 9][11] =   4.446653e-06;
  val[1][1][1][ 9][12] =   1.498439e-03; eval[1][1][1][ 9][12] =   5.870164e-06;
  val[1][1][1][ 9][13] =   1.482939e-03; eval[1][1][1][ 9][13] =   7.423162e-06;
  val[1][1][1][ 9][14] =   1.456197e-03; eval[1][1][1][ 9][14] =   9.032819e-06;
  val[1][1][1][ 9][15] =   1.446352e-03; eval[1][1][1][ 9][15] =   1.137051e-05;
  val[1][1][1][ 9][16] =   1.466476e-03; eval[1][1][1][ 9][16] =   1.477788e-05;
  val[1][1][1][ 9][17] =   1.438596e-03; eval[1][1][1][ 9][17] =   1.740759e-05;
  val[1][1][1][ 9][18] =   1.441533e-03; eval[1][1][1][ 9][18] =   2.132090e-05;
  val[1][1][1][ 9][19] =   1.455865e-03; eval[1][1][1][ 9][19] =   2.732707e-05;
  val[1][1][1][ 9][20] =   1.449268e-03; eval[1][1][1][ 9][20] =   2.469623e-05;
  val[1][1][1][ 9][21] =   1.470221e-03; eval[1][1][1][ 9][21] =   3.642103e-05;
  val[1][1][1][ 9][22] =   1.630466e-03; eval[1][1][1][ 9][22] =   4.899855e-05;
  val[1][1][1][ 9][23] =   1.634483e-03; eval[1][1][1][ 9][23] =   7.569236e-05;
  val[1][1][1][ 9][24] =   1.320011e-03; eval[1][1][1][ 9][24] =   6.398899e-05;
  val[1][1][1][ 9][25] =   1.321091e-03; eval[1][1][1][ 9][25] =   7.992370e-05;
  val[1][1][1][ 9][26] =   1.628479e-03; eval[1][1][1][ 9][26] =   1.912865e-04;
  val[1][1][1][ 9][27] =   1.659334e-03; eval[1][1][1][ 9][27] =   1.335528e-04;
  val[1][1][1][ 9][28] =   1.559974e-03; eval[1][1][1][ 9][28] =   1.681175e-04;
  val[1][1][1][ 9][29] =   1.357571e-03; eval[1][1][1][ 9][29] =   1.894285e-04;
  //pc3dz_mean_c0d0z0
  n_val[2][0][0][0] = 30;
  val[2][0][0][ 0][ 0] =   0.000000e+00; eval[2][0][0][ 0][ 0] =   0.000000e+00;
  val[2][0][0][ 0][ 1] =   0.000000e+00; eval[2][0][0][ 0][ 1] =   0.000000e+00;
  val[2][0][0][ 0][ 2] =   2.501985e+00; eval[2][0][0][ 0][ 2] =   3.964993e-04;
  val[2][0][0][ 0][ 3] =   2.427973e+00; eval[2][0][0][ 0][ 3] =   3.939180e-04;
  val[2][0][0][ 0][ 4] =   2.415163e+00; eval[2][0][0][ 0][ 4] =   4.155953e-04;
  val[2][0][0][ 0][ 5] =   2.401264e+00; eval[2][0][0][ 0][ 5] =   4.700047e-04;
  val[2][0][0][ 0][ 6] =   2.289903e+00; eval[2][0][0][ 0][ 6] =   6.202771e-04;
  val[2][0][0][ 0][ 7] =   2.113847e+00; eval[2][0][0][ 0][ 7] =   7.509466e-04;
  val[2][0][0][ 0][ 8] =   1.983296e+00; eval[2][0][0][ 0][ 8] =   1.027695e-03;
  val[2][0][0][ 0][ 9] =   1.880188e+00; eval[2][0][0][ 0][ 9] =   1.153368e-03;
  val[2][0][0][ 0][10] =   1.818452e+00; eval[2][0][0][ 0][10] =   1.440427e-03;
  val[2][0][0][ 0][11] =   1.780610e+00; eval[2][0][0][ 0][11] =   1.822609e-03;
  val[2][0][0][ 0][12] =   1.744821e+00; eval[2][0][0][ 0][12] =   2.344189e-03;
  val[2][0][0][ 0][13] =   1.717832e+00; eval[2][0][0][ 0][13] =   3.047618e-03;
  val[2][0][0][ 0][14] =   1.693400e+00; eval[2][0][0][ 0][14] =   3.997486e-03;
  val[2][0][0][ 0][15] =   1.684401e+00; eval[2][0][0][ 0][15] =   4.920264e-03;
  val[2][0][0][ 0][16] =   1.673600e+00; eval[2][0][0][ 0][16] =   6.397568e-03;
  val[2][0][0][ 0][17] =   1.679520e+00; eval[2][0][0][ 0][17] =   1.127465e-02;
  val[2][0][0][ 0][18] =   1.650745e+00; eval[2][0][0][ 0][18] =   9.682250e-03;
  val[2][0][0][ 0][19] =   1.648540e+00; eval[2][0][0][ 0][19] =   1.442353e-02;
  val[2][0][0][ 0][20] =   1.662681e+00; eval[2][0][0][ 0][20] =   1.473963e-02;
  val[2][0][0][ 0][21] =   1.644171e+00; eval[2][0][0][ 0][21] =   2.046934e-02;
  val[2][0][0][ 0][22] =   1.592870e+00; eval[2][0][0][ 0][22] =   2.231686e-02;
  val[2][0][0][ 0][23] =   1.616399e+00; eval[2][0][0][ 0][23] =   2.567308e-02;
  val[2][0][0][ 0][24] =   1.625533e+00; eval[2][0][0][ 0][24] =   3.153023e-02;
  val[2][0][0][ 0][25] =   1.601703e+00; eval[2][0][0][ 0][25] =   4.147509e-02;
  val[2][0][0][ 0][26] =   1.575918e+00; eval[2][0][0][ 0][26] =   4.958228e-02;
  val[2][0][0][ 0][27] =   1.522343e+00; eval[2][0][0][ 0][27] =   4.561725e-02;
  val[2][0][0][ 0][28] =   1.527912e+00; eval[2][0][0][ 0][28] =   8.012321e-02;
  val[2][0][0][ 0][29] =   1.679346e+00; eval[2][0][0][ 0][29] =   8.490365e-02;
  //pc3dz_mean_c0d0z1
  n_val[2][0][0][1] = 30;
  val[2][0][0][ 1][ 0] =   0.000000e+00; eval[2][0][0][ 1][ 0] =   0.000000e+00;
  val[2][0][0][ 1][ 1] =   0.000000e+00; eval[2][0][0][ 1][ 1] =   0.000000e+00;
  val[2][0][0][ 1][ 2] =   2.379681e+00; eval[2][0][0][ 1][ 2] =   4.325897e-04;
  val[2][0][0][ 1][ 3] =   2.274107e+00; eval[2][0][0][ 1][ 3] =   3.741521e-04;
  val[2][0][0][ 1][ 4] =   2.254352e+00; eval[2][0][0][ 1][ 4] =   3.769730e-04;
  val[2][0][0][ 1][ 5] =   2.251125e+00; eval[2][0][0][ 1][ 5] =   5.340525e-04;
  val[2][0][0][ 1][ 6] =   2.192308e+00; eval[2][0][0][ 1][ 6] =   6.083274e-04;
  val[2][0][0][ 1][ 7] =   2.064796e+00; eval[2][0][0][ 1][ 7] =   7.817822e-04;
  val[2][0][0][ 1][ 8] =   1.965717e+00; eval[2][0][0][ 1][ 8] =   1.040188e-03;
  val[2][0][0][ 1][ 9] =   1.885179e+00; eval[2][0][0][ 1][ 9] =   1.166815e-03;
  val[2][0][0][ 1][10] =   1.842985e+00; eval[2][0][0][ 1][10] =   1.473313e-03;
  val[2][0][0][ 1][11] =   1.808526e+00; eval[2][0][0][ 1][11] =   1.851517e-03;
  val[2][0][0][ 1][12] =   1.790542e+00; eval[2][0][0][ 1][12] =   2.383272e-03;
  val[2][0][0][ 1][13] =   1.771529e+00; eval[2][0][0][ 1][13] =   3.071815e-03;
  val[2][0][0][ 1][14] =   1.759716e+00; eval[2][0][0][ 1][14] =   3.989282e-03;
  val[2][0][0][ 1][15] =   1.744700e+00; eval[2][0][0][ 1][15] =   4.976676e-03;
  val[2][0][0][ 1][16] =   1.738438e+00; eval[2][0][0][ 1][16] =   5.978001e-03;
  val[2][0][0][ 1][17] =   1.738907e+00; eval[2][0][0][ 1][17] =   7.507178e-03;
  val[2][0][0][ 1][18] =   1.731704e+00; eval[2][0][0][ 1][18] =   9.366824e-03;
  val[2][0][0][ 1][19] =   1.724769e+00; eval[2][0][0][ 1][19] =   1.096591e-02;
  val[2][0][0][ 1][20] =   1.706828e+00; eval[2][0][0][ 1][20] =   1.391665e-02;
  val[2][0][0][ 1][21] =   1.713781e+00; eval[2][0][0][ 1][21] =   1.459925e-02;
  val[2][0][0][ 1][22] =   1.718818e+00; eval[2][0][0][ 1][22] =   2.067377e-02;
  val[2][0][0][ 1][23] =   1.708728e+00; eval[2][0][0][ 1][23] =   2.709206e-02;
  val[2][0][0][ 1][24] =   1.730802e+00; eval[2][0][0][ 1][24] =   2.997681e-02;
  val[2][0][0][ 1][25] =   1.760905e+00; eval[2][0][0][ 1][25] =   4.059409e-02;
  val[2][0][0][ 1][26] =   1.825219e+00; eval[2][0][0][ 1][26] =   5.078410e-02;
  val[2][0][0][ 1][27] =   1.856176e+00; eval[2][0][0][ 1][27] =   6.458760e-02;
  val[2][0][0][ 1][28] =   1.690226e+00; eval[2][0][0][ 1][28] =   8.094250e-02;
  val[2][0][0][ 1][29] =   1.769625e+00; eval[2][0][0][ 1][29] =   1.171205e-01;
  //pc3dz_mean_c0d0z2
  n_val[2][0][0][2] = 30;
  val[2][0][0][ 2][ 0] =   0.000000e+00; eval[2][0][0][ 2][ 0] =   0.000000e+00;
  val[2][0][0][ 2][ 1] =   0.000000e+00; eval[2][0][0][ 2][ 1] =   0.000000e+00;
  val[2][0][0][ 2][ 2] =   2.192931e+00; eval[2][0][0][ 2][ 2] =   4.197663e-04;
  val[2][0][0][ 2][ 3] =   2.104937e+00; eval[2][0][0][ 2][ 3] =   3.779117e-04;
  val[2][0][0][ 2][ 4] =   2.072555e+00; eval[2][0][0][ 2][ 4] =   4.211947e-04;
  val[2][0][0][ 2][ 5] =   2.061727e+00; eval[2][0][0][ 2][ 5] =   4.712946e-04;
  val[2][0][0][ 2][ 6] =   2.048643e+00; eval[2][0][0][ 2][ 6] =   6.826235e-04;
  val[2][0][0][ 2][ 7] =   1.950660e+00; eval[2][0][0][ 2][ 7] =   8.213241e-04;
  val[2][0][0][ 2][ 8] =   1.888480e+00; eval[2][0][0][ 2][ 8] =   9.276005e-04;
  val[2][0][0][ 2][ 9] =   1.844330e+00; eval[2][0][0][ 2][ 9] =   1.142269e-03;
  val[2][0][0][ 2][10] =   1.815567e+00; eval[2][0][0][ 2][10] =   1.441525e-03;
  val[2][0][0][ 2][11] =   1.794639e+00; eval[2][0][0][ 2][11] =   1.845230e-03;
  val[2][0][0][ 2][12] =   1.780649e+00; eval[2][0][0][ 2][12] =   2.367863e-03;
  val[2][0][0][ 2][13] =   1.765072e+00; eval[2][0][0][ 2][13] =   3.002362e-03;
  val[2][0][0][ 2][14] =   1.760252e+00; eval[2][0][0][ 2][14] =   3.879953e-03;
  val[2][0][0][ 2][15] =   1.754820e+00; eval[2][0][0][ 2][15] =   5.014839e-03;
  val[2][0][0][ 2][16] =   1.751204e+00; eval[2][0][0][ 2][16] =   6.083013e-03;
  val[2][0][0][ 2][17] =   1.735661e+00; eval[2][0][0][ 2][17] =   7.574558e-03;
  val[2][0][0][ 2][18] =   1.727582e+00; eval[2][0][0][ 2][18] =   9.620005e-03;
  val[2][0][0][ 2][19] =   1.757644e+00; eval[2][0][0][ 2][19] =   1.139976e-02;
  val[2][0][0][ 2][20] =   1.734414e+00; eval[2][0][0][ 2][20] =   1.058183e-02;
  val[2][0][0][ 2][21] =   1.754221e+00; eval[2][0][0][ 2][21] =   1.461439e-02;
  val[2][0][0][ 2][22] =   1.724849e+00; eval[2][0][0][ 2][22] =   2.034642e-02;
  val[2][0][0][ 2][23] =   1.734025e+00; eval[2][0][0][ 2][23] =   2.476347e-02;
  val[2][0][0][ 2][24] =   1.743165e+00; eval[2][0][0][ 2][24] =   3.788481e-02;
  val[2][0][0][ 2][25] =   1.678284e+00; eval[2][0][0][ 2][25] =   4.070788e-02;
  val[2][0][0][ 2][26] =   1.747595e+00; eval[2][0][0][ 2][26] =   4.443004e-02;
  val[2][0][0][ 2][27] =   1.771485e+00; eval[2][0][0][ 2][27] =   6.065900e-02;
  val[2][0][0][ 2][28] =   1.861185e+00; eval[2][0][0][ 2][28] =   6.965525e-02;
  val[2][0][0][ 2][29] =   1.773786e+00; eval[2][0][0][ 2][29] =   8.124870e-02;
  //pc3dz_mean_c0d0z3
  n_val[2][0][0][3] = 30;
  val[2][0][0][ 3][ 0] =   0.000000e+00; eval[2][0][0][ 3][ 0] =   0.000000e+00;
  val[2][0][0][ 3][ 1] =   0.000000e+00; eval[2][0][0][ 3][ 1] =   0.000000e+00;
  val[2][0][0][ 3][ 2] =   1.947766e+00; eval[2][0][0][ 3][ 2] =   4.557193e-04;
  val[2][0][0][ 3][ 3] =   1.891944e+00; eval[2][0][0][ 3][ 3] =   3.798521e-04;
  val[2][0][0][ 3][ 4] =   1.888867e+00; eval[2][0][0][ 3][ 4] =   4.154342e-04;
  val[2][0][0][ 3][ 5] =   1.887658e+00; eval[2][0][0][ 3][ 5] =   4.603882e-04;
  val[2][0][0][ 3][ 6] =   1.871051e+00; eval[2][0][0][ 3][ 6] =   6.149388e-04;
  val[2][0][0][ 3][ 7] =   1.825656e+00; eval[2][0][0][ 3][ 7] =   7.136854e-04;
  val[2][0][0][ 3][ 8] =   1.790102e+00; eval[2][0][0][ 3][ 8] =   8.517772e-04;
  val[2][0][0][ 3][ 9] =   1.767368e+00; eval[2][0][0][ 3][ 9] =   1.078345e-03;
  val[2][0][0][ 3][10] =   1.751317e+00; eval[2][0][0][ 3][10] =   1.387142e-03;
  val[2][0][0][ 3][11] =   1.743028e+00; eval[2][0][0][ 3][11] =   1.793732e-03;
  val[2][0][0][ 3][12] =   1.736519e+00; eval[2][0][0][ 3][12] =   2.267068e-03;
  val[2][0][0][ 3][13] =   1.728968e+00; eval[2][0][0][ 3][13] =   2.967677e-03;
  val[2][0][0][ 3][14] =   1.719549e+00; eval[2][0][0][ 3][14] =   3.829756e-03;
  val[2][0][0][ 3][15] =   1.723573e+00; eval[2][0][0][ 3][15] =   4.840036e-03;
  val[2][0][0][ 3][16] =   1.711280e+00; eval[2][0][0][ 3][16] =   6.167143e-03;
  val[2][0][0][ 3][17] =   1.724721e+00; eval[2][0][0][ 3][17] =   7.536599e-03;
  val[2][0][0][ 3][18] =   1.716565e+00; eval[2][0][0][ 3][18] =   8.773249e-03;
  val[2][0][0][ 3][19] =   1.700490e+00; eval[2][0][0][ 3][19] =   1.513401e-02;
  val[2][0][0][ 3][20] =   1.715577e+00; eval[2][0][0][ 3][20] =   1.415565e-02;
  val[2][0][0][ 3][21] =   1.698447e+00; eval[2][0][0][ 3][21] =   1.965087e-02;
  val[2][0][0][ 3][22] =   1.718488e+00; eval[2][0][0][ 3][22] =   1.981805e-02;
  val[2][0][0][ 3][23] =   1.713545e+00; eval[2][0][0][ 3][23] =   2.643694e-02;
  val[2][0][0][ 3][24] =   1.700640e+00; eval[2][0][0][ 3][24] =   3.195350e-02;
  val[2][0][0][ 3][25] =   1.746923e+00; eval[2][0][0][ 3][25] =   4.423541e-02;
  val[2][0][0][ 3][26] =   1.605060e+00; eval[2][0][0][ 3][26] =   5.606714e-02;
  val[2][0][0][ 3][27] =   1.726279e+00; eval[2][0][0][ 3][27] =   8.490616e-02;
  val[2][0][0][ 3][28] =   1.769585e+00; eval[2][0][0][ 3][28] =   4.078354e-02;
  val[2][0][0][ 3][29] =   1.685302e+00; eval[2][0][0][ 3][29] =   8.186871e-02;
  //pc3dz_mean_c0d0z4
  n_val[2][0][0][4] = 30;
  val[2][0][0][ 4][ 0] =   0.000000e+00; eval[2][0][0][ 4][ 0] =   0.000000e+00;
  val[2][0][0][ 4][ 1] =   0.000000e+00; eval[2][0][0][ 4][ 1] =   0.000000e+00;
  val[2][0][0][ 4][ 2] =   1.701062e+00; eval[2][0][0][ 4][ 2] =   5.076227e-04;
  val[2][0][0][ 4][ 3] =   1.703937e+00; eval[2][0][0][ 4][ 3] =   4.222023e-04;
  val[2][0][0][ 4][ 4] =   1.704524e+00; eval[2][0][0][ 4][ 4] =   4.985532e-04;
  val[2][0][0][ 4][ 5] =   1.726153e+00; eval[2][0][0][ 4][ 5] =   5.822231e-04;
  val[2][0][0][ 4][ 6] =   1.726889e+00; eval[2][0][0][ 4][ 6] =   6.670054e-04;
  val[2][0][0][ 4][ 7] =   1.712850e+00; eval[2][0][0][ 4][ 7] =   8.152564e-04;
  val[2][0][0][ 4][ 8] =   1.704712e+00; eval[2][0][0][ 4][ 8] =   9.999389e-04;
  val[2][0][0][ 4][ 9] =   1.698251e+00; eval[2][0][0][ 4][ 9] =   1.274738e-03;
  val[2][0][0][ 4][10] =   1.696002e+00; eval[2][0][0][ 4][10] =   1.660828e-03;
  val[2][0][0][ 4][11] =   1.690320e+00; eval[2][0][0][ 4][11] =   2.139085e-03;
  val[2][0][0][ 4][12] =   1.688113e+00; eval[2][0][0][ 4][12] =   2.809367e-03;
  val[2][0][0][ 4][13] =   1.683729e+00; eval[2][0][0][ 4][13] =   3.573759e-03;
  val[2][0][0][ 4][14] =   1.686266e+00; eval[2][0][0][ 4][14] =   4.517418e-03;
  val[2][0][0][ 4][15] =   1.683672e+00; eval[2][0][0][ 4][15] =   8.021002e-03;
  val[2][0][0][ 4][16] =   1.680423e+00; eval[2][0][0][ 4][16] =   9.984330e-03;
  val[2][0][0][ 4][17] =   1.694929e+00; eval[2][0][0][ 4][17] =   8.731860e-03;
  val[2][0][0][ 4][18] =   1.680848e+00; eval[2][0][0][ 4][18] =   1.154585e-02;
  val[2][0][0][ 4][19] =   1.685170e+00; eval[2][0][0][ 4][19] =   1.845776e-02;
  val[2][0][0][ 4][20] =   1.673528e+00; eval[2][0][0][ 4][20] =   1.642431e-02;
  val[2][0][0][ 4][21] =   1.685087e+00; eval[2][0][0][ 4][21] =   1.714653e-02;
  val[2][0][0][ 4][22] =   1.673137e+00; eval[2][0][0][ 4][22] =   2.635140e-02;
  val[2][0][0][ 4][23] =   1.655117e+00; eval[2][0][0][ 4][23] =   3.407260e-02;
  val[2][0][0][ 4][24] =   1.684111e+00; eval[2][0][0][ 4][24] =   4.145734e-02;
  val[2][0][0][ 4][25] =   1.562218e+00; eval[2][0][0][ 4][25] =   5.045738e-02;
  val[2][0][0][ 4][26] =   1.679088e+00; eval[2][0][0][ 4][26] =   6.346834e-02;
  val[2][0][0][ 4][27] =   1.722430e+00; eval[2][0][0][ 4][27] =   6.171078e-02;
  val[2][0][0][ 4][28] =   1.922788e+00; eval[2][0][0][ 4][28] =   8.772535e-02;
  val[2][0][0][ 4][29] =   1.620619e+00; eval[2][0][0][ 4][29] =   9.838158e-02;
  //pc3dz_mean_c0d0z5
  n_val[2][0][0][5] = 30;
  val[2][0][0][ 5][ 0] =   0.000000e+00; eval[2][0][0][ 5][ 0] =   0.000000e+00;
  val[2][0][0][ 5][ 1] =   0.000000e+00; eval[2][0][0][ 5][ 1] =   0.000000e+00;
  val[2][0][0][ 5][ 2] =   1.778857e+00; eval[2][0][0][ 5][ 2] =   5.753111e-04;
  val[2][0][0][ 5][ 3] =   1.758610e+00; eval[2][0][0][ 5][ 3] =   4.979196e-04;
  val[2][0][0][ 5][ 4] =   1.765654e+00; eval[2][0][0][ 5][ 4] =   4.973140e-04;
  val[2][0][0][ 5][ 5] =   1.785938e+00; eval[2][0][0][ 5][ 5] =   7.399579e-04;
  val[2][0][0][ 5][ 6] =   1.815013e+00; eval[2][0][0][ 5][ 6] =   8.633558e-04;
  val[2][0][0][ 5][ 7] =   1.841359e+00; eval[2][0][0][ 5][ 7] =   1.042415e-03;
  val[2][0][0][ 5][ 8] =   1.846967e+00; eval[2][0][0][ 5][ 8] =   1.284437e-03;
  val[2][0][0][ 5][ 9] =   1.846352e+00; eval[2][0][0][ 5][ 9] =   1.627634e-03;
  val[2][0][0][ 5][10] =   1.854369e+00; eval[2][0][0][ 5][10] =   2.119902e-03;
  val[2][0][0][ 5][11] =   1.856210e+00; eval[2][0][0][ 5][11] =   2.718771e-03;
  val[2][0][0][ 5][12] =   1.855835e+00; eval[2][0][0][ 5][12] =   3.502490e-03;
  val[2][0][0][ 5][13] =   1.857940e+00; eval[2][0][0][ 5][13] =   4.544671e-03;
  val[2][0][0][ 5][14] =   1.862395e+00; eval[2][0][0][ 5][14] =   6.031915e-03;
  val[2][0][0][ 5][15] =   1.878697e+00; eval[2][0][0][ 5][15] =   8.361429e-03;
  val[2][0][0][ 5][16] =   1.868139e+00; eval[2][0][0][ 5][16] =   1.099146e-02;
  val[2][0][0][ 5][17] =   1.862388e+00; eval[2][0][0][ 5][17] =   1.126257e-02;
  val[2][0][0][ 5][18] =   1.858015e+00; eval[2][0][0][ 5][18] =   1.439400e-02;
  val[2][0][0][ 5][19] =   1.871625e+00; eval[2][0][0][ 5][19] =   1.988164e-02;
  val[2][0][0][ 5][20] =   1.852779e+00; eval[2][0][0][ 5][20] =   1.544216e-02;
  val[2][0][0][ 5][21] =   1.862794e+00; eval[2][0][0][ 5][21] =   2.747315e-02;
  val[2][0][0][ 5][22] =   1.851001e+00; eval[2][0][0][ 5][22] =   2.479509e-02;
  val[2][0][0][ 5][23] =   1.833919e+00; eval[2][0][0][ 5][23] =   3.810217e-02;
  val[2][0][0][ 5][24] =   1.874120e+00; eval[2][0][0][ 5][24] =   4.471512e-02;
  val[2][0][0][ 5][25] =   1.915096e+00; eval[2][0][0][ 5][25] =   5.353137e-02;
  val[2][0][0][ 5][26] =   1.767368e+00; eval[2][0][0][ 5][26] =   5.897709e-02;
  val[2][0][0][ 5][27] =   1.900232e+00; eval[2][0][0][ 5][27] =   6.964197e-02;
  val[2][0][0][ 5][28] =   1.970005e+00; eval[2][0][0][ 5][28] =   1.346245e-01;
  val[2][0][0][ 5][29] =   1.785610e+00; eval[2][0][0][ 5][29] =   1.478590e-01;
  //pc3dz_mean_c0d0z6
  n_val[2][0][0][6] = 30;
  val[2][0][0][ 6][ 0] =   0.000000e+00; eval[2][0][0][ 6][ 0] =   0.000000e+00;
  val[2][0][0][ 6][ 1] =   0.000000e+00; eval[2][0][0][ 6][ 1] =   0.000000e+00;
  val[2][0][0][ 6][ 2] =   1.604358e+00; eval[2][0][0][ 6][ 2] =   4.932322e-04;
  val[2][0][0][ 6][ 3] =   1.612825e+00; eval[2][0][0][ 6][ 3] =   4.160222e-04;
  val[2][0][0][ 6][ 4] =   1.622228e+00; eval[2][0][0][ 6][ 4] =   4.668475e-04;
  val[2][0][0][ 6][ 5] =   1.654601e+00; eval[2][0][0][ 6][ 5] =   5.553719e-04;
  val[2][0][0][ 6][ 6] =   1.709118e+00; eval[2][0][0][ 6][ 6] =   6.944933e-04;
  val[2][0][0][ 6][ 7] =   1.765102e+00; eval[2][0][0][ 6][ 7] =   8.337258e-04;
  val[2][0][0][ 6][ 8] =   1.792195e+00; eval[2][0][0][ 6][ 8] =   1.003816e-03;
  val[2][0][0][ 6][ 9] =   1.810698e+00; eval[2][0][0][ 6][ 9] =   1.288297e-03;
  val[2][0][0][ 6][10] =   1.826848e+00; eval[2][0][0][ 6][10] =   1.661915e-03;
  val[2][0][0][ 6][11] =   1.835839e+00; eval[2][0][0][ 6][11] =   2.197078e-03;
  val[2][0][0][ 6][12] =   1.849666e+00; eval[2][0][0][ 6][12] =   2.823804e-03;
  val[2][0][0][ 6][13] =   1.860898e+00; eval[2][0][0][ 6][13] =   4.510610e-03;
  val[2][0][0][ 6][14] =   1.864702e+00; eval[2][0][0][ 6][14] =   5.780872e-03;
  val[2][0][0][ 6][15] =   1.857510e+00; eval[2][0][0][ 6][15] =   5.984855e-03;
  val[2][0][0][ 6][16] =   1.851464e+00; eval[2][0][0][ 6][16] =   7.344228e-03;
  val[2][0][0][ 6][17] =   1.880106e+00; eval[2][0][0][ 6][17] =   1.047211e-02;
  val[2][0][0][ 6][18] =   1.875555e+00; eval[2][0][0][ 6][18] =   1.349563e-02;
  val[2][0][0][ 6][19] =   1.878163e+00; eval[2][0][0][ 6][19] =   1.448782e-02;
  val[2][0][0][ 6][20] =   1.860366e+00; eval[2][0][0][ 6][20] =   1.609589e-02;
  val[2][0][0][ 6][21] =   1.900511e+00; eval[2][0][0][ 6][21] =   2.036936e-02;
  val[2][0][0][ 6][22] =   1.904705e+00; eval[2][0][0][ 6][22] =   2.539126e-02;
  val[2][0][0][ 6][23] =   1.891157e+00; eval[2][0][0][ 6][23] =   3.186551e-02;
  val[2][0][0][ 6][24] =   1.826306e+00; eval[2][0][0][ 6][24] =   3.879494e-02;
  val[2][0][0][ 6][25] =   1.873736e+00; eval[2][0][0][ 6][25] =   4.221060e-02;
  val[2][0][0][ 6][26] =   1.940568e+00; eval[2][0][0][ 6][26] =   6.154349e-02;
  val[2][0][0][ 6][27] =   1.785221e+00; eval[2][0][0][ 6][27] =   5.744541e-02;
  val[2][0][0][ 6][28] =   1.901643e+00; eval[2][0][0][ 6][28] =   7.829324e-02;
  val[2][0][0][ 6][29] =   1.768592e+00; eval[2][0][0][ 6][29] =   9.237496e-02;
  //pc3dz_mean_c0d0z7
  n_val[2][0][0][7] = 30;
  val[2][0][0][ 7][ 0] =   0.000000e+00; eval[2][0][0][ 7][ 0] =   0.000000e+00;
  val[2][0][0][ 7][ 1] =   0.000000e+00; eval[2][0][0][ 7][ 1] =   0.000000e+00;
  val[2][0][0][ 7][ 2] =   1.427024e+00; eval[2][0][0][ 7][ 2] =   4.827855e-04;
  val[2][0][0][ 7][ 3] =   1.463223e+00; eval[2][0][0][ 7][ 3] =   4.213012e-04;
  val[2][0][0][ 7][ 4] =   1.508019e+00; eval[2][0][0][ 7][ 4] =   4.399108e-04;
  val[2][0][0][ 7][ 5] =   1.544040e+00; eval[2][0][0][ 7][ 5] =   5.296806e-04;
  val[2][0][0][ 7][ 6] =   1.614303e+00; eval[2][0][0][ 7][ 6] =   7.437191e-04;
  val[2][0][0][ 7][ 7] =   1.705305e+00; eval[2][0][0][ 7][ 7] =   8.514253e-04;
  val[2][0][0][ 7][ 8] =   1.763819e+00; eval[2][0][0][ 7][ 8] =   1.008241e-03;
  val[2][0][0][ 7][ 9] =   1.800969e+00; eval[2][0][0][ 7][ 9] =   1.299523e-03;
  val[2][0][0][ 7][10] =   1.828420e+00; eval[2][0][0][ 7][10] =   1.676558e-03;
  val[2][0][0][ 7][11] =   1.845590e+00; eval[2][0][0][ 7][11] =   2.185996e-03;
  val[2][0][0][ 7][12] =   1.859122e+00; eval[2][0][0][ 7][12] =   2.841415e-03;
  val[2][0][0][ 7][13] =   1.883629e+00; eval[2][0][0][ 7][13] =   4.265457e-03;
  val[2][0][0][ 7][14] =   1.878999e+00; eval[2][0][0][ 7][14] =   5.788411e-03;
  val[2][0][0][ 7][15] =   1.894164e+00; eval[2][0][0][ 7][15] =   7.116267e-03;
  val[2][0][0][ 7][16] =   1.902724e+00; eval[2][0][0][ 7][16] =   8.228317e-03;
  val[2][0][0][ 7][17] =   1.900361e+00; eval[2][0][0][ 7][17] =   1.058107e-02;
  val[2][0][0][ 7][18] =   1.897303e+00; eval[2][0][0][ 7][18] =   1.394483e-02;
  val[2][0][0][ 7][19] =   1.895948e+00; eval[2][0][0][ 7][19] =   1.581653e-02;
  val[2][0][0][ 7][20] =   1.905339e+00; eval[2][0][0][ 7][20] =   1.434826e-02;
  val[2][0][0][ 7][21] =   1.905534e+00; eval[2][0][0][ 7][21] =   1.938291e-02;
  val[2][0][0][ 7][22] =   1.891163e+00; eval[2][0][0][ 7][22] =   2.568491e-02;
  val[2][0][0][ 7][23] =   1.897670e+00; eval[2][0][0][ 7][23] =   2.873550e-02;
  val[2][0][0][ 7][24] =   1.920755e+00; eval[2][0][0][ 7][24] =   3.556577e-02;
  val[2][0][0][ 7][25] =   1.889179e+00; eval[2][0][0][ 7][25] =   5.097636e-02;
  val[2][0][0][ 7][26] =   1.885740e+00; eval[2][0][0][ 7][26] =   5.607516e-02;
  val[2][0][0][ 7][27] =   1.984041e+00; eval[2][0][0][ 7][27] =   6.285893e-02;
  val[2][0][0][ 7][28] =   1.718504e+00; eval[2][0][0][ 7][28] =   8.500017e-02;
  val[2][0][0][ 7][29] =   1.869206e+00; eval[2][0][0][ 7][29] =   8.587180e-02;
  //pc3dz_mean_c0d0z8
  n_val[2][0][0][8] = 30;
  val[2][0][0][ 8][ 0] =   0.000000e+00; eval[2][0][0][ 8][ 0] =   0.000000e+00;
  val[2][0][0][ 8][ 1] =   0.000000e+00; eval[2][0][0][ 8][ 1] =   0.000000e+00;
  val[2][0][0][ 8][ 2] =   1.303255e+00; eval[2][0][0][ 8][ 2] =   4.644653e-04;
  val[2][0][0][ 8][ 3] =   1.345235e+00; eval[2][0][0][ 8][ 3] =   4.087610e-04;
  val[2][0][0][ 8][ 4] =   1.403688e+00; eval[2][0][0][ 8][ 4] =   4.463510e-04;
  val[2][0][0][ 8][ 5] =   1.447526e+00; eval[2][0][0][ 8][ 5] =   5.338735e-04;
  val[2][0][0][ 8][ 6] =   1.544936e+00; eval[2][0][0][ 8][ 6] =   6.242213e-04;
  val[2][0][0][ 8][ 7] =   1.696144e+00; eval[2][0][0][ 8][ 7] =   8.426684e-04;
  val[2][0][0][ 8][ 8] =   1.786745e+00; eval[2][0][0][ 8][ 8] =   1.000524e-03;
  val[2][0][0][ 8][ 9] =   1.846599e+00; eval[2][0][0][ 8][ 9] =   1.314914e-03;
  val[2][0][0][ 8][10] =   1.887268e+00; eval[2][0][0][ 8][10] =   1.734883e-03;
  val[2][0][0][ 8][11] =   1.929254e+00; eval[2][0][0][ 8][11] =   2.581492e-03;
  val[2][0][0][ 8][12] =   1.945210e+00; eval[2][0][0][ 8][12] =   3.272346e-03;
  val[2][0][0][ 8][13] =   1.957423e+00; eval[2][0][0][ 8][13] =   4.118963e-03;
  val[2][0][0][ 8][14] =   1.975829e+00; eval[2][0][0][ 8][14] =   5.319643e-03;
  val[2][0][0][ 8][15] =   1.973348e+00; eval[2][0][0][ 8][15] =   6.626499e-03;
  val[2][0][0][ 8][16] =   1.991362e+00; eval[2][0][0][ 8][16] =   7.968743e-03;
  val[2][0][0][ 8][17] =   1.997389e+00; eval[2][0][0][ 8][17] =   9.672786e-03;
  val[2][0][0][ 8][18] =   1.989865e+00; eval[2][0][0][ 8][18] =   1.214957e-02;
  val[2][0][0][ 8][19] =   2.006476e+00; eval[2][0][0][ 8][19] =   1.461060e-02;
  val[2][0][0][ 8][20] =   1.986221e+00; eval[2][0][0][ 8][20] =   1.341076e-02;
  val[2][0][0][ 8][21] =   2.059551e+00; eval[2][0][0][ 8][21] =   2.020991e-02;
  val[2][0][0][ 8][22] =   2.035653e+00; eval[2][0][0][ 8][22] =   2.544008e-02;
  val[2][0][0][ 8][23] =   1.992170e+00; eval[2][0][0][ 8][23] =   2.840809e-02;
  val[2][0][0][ 8][24] =   2.031075e+00; eval[2][0][0][ 8][24] =   3.602334e-02;
  val[2][0][0][ 8][25] =   2.055101e+00; eval[2][0][0][ 8][25] =   4.931543e-02;
  val[2][0][0][ 8][26] =   2.029715e+00; eval[2][0][0][ 8][26] =   4.856552e-02;
  val[2][0][0][ 8][27] =   1.764466e+00; eval[2][0][0][ 8][27] =   7.929542e-02;
  val[2][0][0][ 8][28] =   1.860004e+00; eval[2][0][0][ 8][28] =   7.159809e-02;
  val[2][0][0][ 8][29] =   2.015823e+00; eval[2][0][0][ 8][29] =   7.543023e-02;
  //pc3dz_mean_c0d0z9
  n_val[2][0][0][9] = 30;
  val[2][0][0][ 9][ 0] =   0.000000e+00; eval[2][0][0][ 9][ 0] =   0.000000e+00;
  val[2][0][0][ 9][ 1] =   0.000000e+00; eval[2][0][0][ 9][ 1] =   0.000000e+00;
  val[2][0][0][ 9][ 2] =   1.266742e+00; eval[2][0][0][ 9][ 2] =   4.897184e-04;
  val[2][0][0][ 9][ 3] =   1.287559e+00; eval[2][0][0][ 9][ 3] =   4.221790e-04;
  val[2][0][0][ 9][ 4] =   1.341675e+00; eval[2][0][0][ 9][ 4] =   4.651315e-04;
  val[2][0][0][ 9][ 5] =   1.381583e+00; eval[2][0][0][ 9][ 5] =   5.361901e-04;
  val[2][0][0][ 9][ 6] =   1.528816e+00; eval[2][0][0][ 9][ 6] =   6.239166e-04;
  val[2][0][0][ 9][ 7] =   1.739251e+00; eval[2][0][0][ 9][ 7] =   8.368280e-04;
  val[2][0][0][ 9][ 8] =   1.869041e+00; eval[2][0][0][ 9][ 8] =   1.034774e-03;
  val[2][0][0][ 9][ 9] =   1.970294e+00; eval[2][0][0][ 9][ 9] =   1.540069e-03;
  val[2][0][0][ 9][10] =   2.034041e+00; eval[2][0][0][ 9][10] =   1.950776e-03;
  val[2][0][0][ 9][11] =   2.077644e+00; eval[2][0][0][ 9][11] =   2.616525e-03;
  val[2][0][0][ 9][12] =   2.104076e+00; eval[2][0][0][ 9][12] =   3.496313e-03;
  val[2][0][0][ 9][13] =   2.133106e+00; eval[2][0][0][ 9][13] =   4.697093e-03;
  val[2][0][0][ 9][14] =   2.150314e+00; eval[2][0][0][ 9][14] =   4.689451e-03;
  val[2][0][0][ 9][15] =   2.165248e+00; eval[2][0][0][ 9][15] =   7.617946e-03;
  val[2][0][0][ 9][16] =   2.179760e+00; eval[2][0][0][ 9][16] =   7.076648e-03;
  val[2][0][0][ 9][17] =   2.178184e+00; eval[2][0][0][ 9][17] =   8.848747e-03;
  val[2][0][0][ 9][18] =   2.180733e+00; eval[2][0][0][ 9][18] =   1.159818e-02;
  val[2][0][0][ 9][19] =   2.195299e+00; eval[2][0][0][ 9][19] =   1.285819e-02;
  val[2][0][0][ 9][20] =   2.214792e+00; eval[2][0][0][ 9][20] =   1.171829e-02;
  val[2][0][0][ 9][21] =   2.231207e+00; eval[2][0][0][ 9][21] =   1.718414e-02;
  val[2][0][0][ 9][22] =   2.209854e+00; eval[2][0][0][ 9][22] =   2.211747e-02;
  val[2][0][0][ 9][23] =   2.211589e+00; eval[2][0][0][ 9][23] =   2.984630e-02;
  val[2][0][0][ 9][24] =   2.225228e+00; eval[2][0][0][ 9][24] =   3.998462e-02;
  val[2][0][0][ 9][25] =   2.178777e+00; eval[2][0][0][ 9][25] =   4.905177e-02;
  val[2][0][0][ 9][26] =   2.256563e+00; eval[2][0][0][ 9][26] =   5.731256e-02;
  val[2][0][0][ 9][27] =   2.225494e+00; eval[2][0][0][ 9][27] =   5.818075e-02;
  val[2][0][0][ 9][28] =   2.210032e+00; eval[2][0][0][ 9][28] =   7.080357e-02;
  val[2][0][0][ 9][29] =   2.572389e+00; eval[2][0][0][ 9][29] =   1.242127e-01;
  //pc3dz_mean_c0d1z0
  n_val[2][0][1][0] = 30;
  val[2][0][1][ 0][ 0] =   0.000000e+00; eval[2][0][1][ 0][ 0] =   0.000000e+00;
  val[2][0][1][ 0][ 1] =   0.000000e+00; eval[2][0][1][ 0][ 1] =   0.000000e+00;
  val[2][0][1][ 0][ 2] =   5.063518e-01; eval[2][0][1][ 0][ 2] =   4.973252e-04;
  val[2][0][1][ 0][ 3] =   4.006145e-01; eval[2][0][1][ 0][ 3] =   4.694847e-04;
  val[2][0][1][ 0][ 4] =   3.698441e-01; eval[2][0][1][ 0][ 4] =   5.147168e-04;
  val[2][0][1][ 0][ 5] =   3.586403e-01; eval[2][0][1][ 0][ 5] =   5.640368e-04;
  val[2][0][1][ 0][ 6] =   2.415257e-01; eval[2][0][1][ 0][ 6] =   7.496913e-04;
  val[2][0][1][ 0][ 7] =   5.373458e-02; eval[2][0][1][ 0][ 7] =   9.950504e-04;
  val[2][0][1][ 0][ 8] =  -8.851330e-02; eval[2][0][1][ 0][ 8] =   1.176753e-03;
  val[2][0][1][ 0][ 9] =  -1.679747e-01; eval[2][0][1][ 0][ 9] =   1.419197e-03;
  val[2][0][1][ 0][10] =  -2.265022e-01; eval[2][0][1][ 0][10] =   1.831976e-03;
  val[2][0][1][ 0][11] =  -2.661050e-01; eval[2][0][1][ 0][11] =   2.400755e-03;
  val[2][0][1][ 0][12] =  -2.896802e-01; eval[2][0][1][ 0][12] =   3.120437e-03;
  val[2][0][1][ 0][13] =  -3.078322e-01; eval[2][0][1][ 0][13] =   4.049086e-03;
  val[2][0][1][ 0][14] =  -3.258037e-01; eval[2][0][1][ 0][14] =   5.341854e-03;
  val[2][0][1][ 0][15] =  -3.430202e-01; eval[2][0][1][ 0][15] =   6.784146e-03;
  val[2][0][1][ 0][16] =  -3.309325e-01; eval[2][0][1][ 0][16] =   8.741536e-03;
  val[2][0][1][ 0][17] =  -3.378772e-01; eval[2][0][1][ 0][17] =   1.090247e-02;
  val[2][0][1][ 0][18] =  -3.473094e-01; eval[2][0][1][ 0][18] =   1.757612e-02;
  val[2][0][1][ 0][19] =  -3.453242e-01; eval[2][0][1][ 0][19] =   1.647209e-02;
  val[2][0][1][ 0][20] =  -3.533385e-01; eval[2][0][1][ 0][20] =   1.472264e-02;
  val[2][0][1][ 0][21] =  -3.658253e-01; eval[2][0][1][ 0][21] =   2.399714e-02;
  val[2][0][1][ 0][22] =  -2.972343e-01; eval[2][0][1][ 0][22] =   3.020954e-02;
  val[2][0][1][ 0][23] =  -3.710544e-01; eval[2][0][1][ 0][23] =   3.694185e-02;
  val[2][0][1][ 0][24] =  -3.626155e-01; eval[2][0][1][ 0][24] =   4.306845e-02;
  val[2][0][1][ 0][25] =  -4.100633e-01; eval[2][0][1][ 0][25] =   6.371775e-02;
  val[2][0][1][ 0][26] =  -5.345400e-01; eval[2][0][1][ 0][26] =   7.498170e-02;
  val[2][0][1][ 0][27] =  -2.580843e-01; eval[2][0][1][ 0][27] =   9.814940e-02;
  val[2][0][1][ 0][28] =  -4.668820e-01; eval[2][0][1][ 0][28] =   8.823126e-02;
  val[2][0][1][ 0][29] =  -4.212152e-01; eval[2][0][1][ 0][29] =   1.523226e-01;
  //pc3dz_mean_c0d1z1
  n_val[2][0][1][1] = 30;
  val[2][0][1][ 1][ 0] =   0.000000e+00; eval[2][0][1][ 1][ 0] =   0.000000e+00;
  val[2][0][1][ 1][ 1] =   0.000000e+00; eval[2][0][1][ 1][ 1] =   0.000000e+00;
  val[2][0][1][ 1][ 2] =   4.946471e-01; eval[2][0][1][ 1][ 2] =   4.398573e-04;
  val[2][0][1][ 1][ 3] =   3.760562e-01; eval[2][0][1][ 1][ 3] =   4.129737e-04;
  val[2][0][1][ 1][ 4] =   3.374053e-01; eval[2][0][1][ 1][ 4] =   4.765004e-04;
  val[2][0][1][ 1][ 5] =   3.298933e-01; eval[2][0][1][ 1][ 5] =   5.950995e-04;
  val[2][0][1][ 1][ 6] =   2.467715e-01; eval[2][0][1][ 1][ 6] =   6.434334e-04;
  val[2][0][1][ 1][ 7] =   1.160319e-01; eval[2][0][1][ 1][ 7] =   8.541762e-04;
  val[2][0][1][ 1][ 8] =   3.234405e-02; eval[2][0][1][ 1][ 8] =   1.127691e-03;
  val[2][0][1][ 1][ 9] =  -2.788236e-02; eval[2][0][1][ 1][ 9] =   1.563627e-03;
  val[2][0][1][ 1][10] =  -8.255219e-02; eval[2][0][1][ 1][10] =   1.838349e-03;
  val[2][0][1][ 1][11] =  -1.104498e-01; eval[2][0][1][ 1][11] =   2.337567e-03;
  val[2][0][1][ 1][12] =  -1.325270e-01; eval[2][0][1][ 1][12] =   2.993239e-03;
  val[2][0][1][ 1][13] =  -1.446116e-01; eval[2][0][1][ 1][13] =   3.844871e-03;
  val[2][0][1][ 1][14] =  -1.592849e-01; eval[2][0][1][ 1][14] =   4.891980e-03;
  val[2][0][1][ 1][15] =  -1.749858e-01; eval[2][0][1][ 1][15] =   5.975164e-03;
  val[2][0][1][ 1][16] =  -1.745702e-01; eval[2][0][1][ 1][16] =   7.911784e-03;
  val[2][0][1][ 1][17] =  -1.698103e-01; eval[2][0][1][ 1][17] =   9.685423e-03;
  val[2][0][1][ 1][18] =  -1.774176e-01; eval[2][0][1][ 1][18] =   1.159968e-02;
  val[2][0][1][ 1][19] =  -1.741336e-01; eval[2][0][1][ 1][19] =   1.533732e-02;
  val[2][0][1][ 1][20] =  -1.775955e-01; eval[2][0][1][ 1][20] =   1.414297e-02;
  val[2][0][1][ 1][21] =  -1.928532e-01; eval[2][0][1][ 1][21] =   1.926893e-02;
  val[2][0][1][ 1][22] =  -1.564870e-01; eval[2][0][1][ 1][22] =   2.730105e-02;
  val[2][0][1][ 1][23] =  -1.572009e-01; eval[2][0][1][ 1][23] =   3.707296e-02;
  val[2][0][1][ 1][24] =  -1.402911e-01; eval[2][0][1][ 1][24] =   3.999912e-02;
  val[2][0][1][ 1][25] =  -2.814402e-01; eval[2][0][1][ 1][25] =   6.267423e-02;
  val[2][0][1][ 1][26] =  -1.963455e-01; eval[2][0][1][ 1][26] =   5.935870e-02;
  val[2][0][1][ 1][27] =   1.665014e-02; eval[2][0][1][ 1][27] =   7.935529e-02;
  val[2][0][1][ 1][28] =  -2.483815e-01; eval[2][0][1][ 1][28] =   1.177305e-01;
  val[2][0][1][ 1][29] =  -5.385600e-01; eval[2][0][1][ 1][29] =   1.156244e-01;
  //pc3dz_mean_c0d1z2
  n_val[2][0][1][2] = 30;
  val[2][0][1][ 2][ 0] =   0.000000e+00; eval[2][0][1][ 2][ 0] =   0.000000e+00;
  val[2][0][1][ 2][ 1] =   0.000000e+00; eval[2][0][1][ 2][ 1] =   0.000000e+00;
  val[2][0][1][ 2][ 2] =   4.438168e-01; eval[2][0][1][ 2][ 2] =   4.395915e-04;
  val[2][0][1][ 2][ 3] =   3.417371e-01; eval[2][0][1][ 2][ 3] =   4.078357e-04;
  val[2][0][1][ 2][ 4] =   2.785393e-01; eval[2][0][1][ 2][ 4] =   4.167458e-04;
  val[2][0][1][ 2][ 5] =   2.646912e-01; eval[2][0][1][ 2][ 5] =   5.799623e-04;
  val[2][0][1][ 2][ 6] =   2.097987e-01; eval[2][0][1][ 2][ 6] =   6.721481e-04;
  val[2][0][1][ 2][ 7] =   1.218505e-01; eval[2][0][1][ 2][ 7] =   8.637141e-04;
  val[2][0][1][ 2][ 8] =   6.547520e-02; eval[2][0][1][ 2][ 8] =   1.099853e-03;
  val[2][0][1][ 2][ 9] =   2.778871e-02; eval[2][0][1][ 2][ 9] =   1.591227e-03;
  val[2][0][1][ 2][10] =   3.515180e-03; eval[2][0][1][ 2][10] =   2.035860e-03;
  val[2][0][1][ 2][11] =  -1.274434e-02; eval[2][0][1][ 2][11] =   2.695623e-03;
  val[2][0][1][ 2][12] =  -2.835450e-02; eval[2][0][1][ 2][12] =   3.557260e-03;
  val[2][0][1][ 2][13] =  -3.243761e-02; eval[2][0][1][ 2][13] =   4.635361e-03;
  val[2][0][1][ 2][14] =  -4.428892e-02; eval[2][0][1][ 2][14] =   6.000498e-03;
  val[2][0][1][ 2][15] =  -3.666919e-02; eval[2][0][1][ 2][15] =   7.358266e-03;
  val[2][0][1][ 2][16] =  -3.299790e-02; eval[2][0][1][ 2][16] =   9.703213e-03;
  val[2][0][1][ 2][17] =  -3.354687e-02; eval[2][0][1][ 2][17] =   1.190099e-02;
  val[2][0][1][ 2][18] =  -5.803909e-02; eval[2][0][1][ 2][18] =   1.465182e-02;
  val[2][0][1][ 2][19] =  -6.170829e-02; eval[2][0][1][ 2][19] =   1.770166e-02;
  val[2][0][1][ 2][20] =  -6.714809e-02; eval[2][0][1][ 2][20] =   1.653626e-02;
  val[2][0][1][ 2][21] =  -6.682155e-02; eval[2][0][1][ 2][21] =   2.247906e-02;
  val[2][0][1][ 2][22] =  -1.218730e-02; eval[2][0][1][ 2][22] =   3.209087e-02;
  val[2][0][1][ 2][23] =  -7.121584e-02; eval[2][0][1][ 2][23] =   3.046946e-02;
  val[2][0][1][ 2][24] =  -1.459805e-01; eval[2][0][1][ 2][24] =   4.979874e-02;
  val[2][0][1][ 2][25] =  -9.746370e-02; eval[2][0][1][ 2][25] =   5.576101e-02;
  val[2][0][1][ 2][26] =  -4.955806e-02; eval[2][0][1][ 2][26] =   7.853788e-02;
  val[2][0][1][ 2][27] =  -8.344830e-02; eval[2][0][1][ 2][27] =   8.796160e-02;
  val[2][0][1][ 2][28] =  -8.180913e-02; eval[2][0][1][ 2][28] =   1.025581e-01;
  val[2][0][1][ 2][29] =  -3.314009e-02; eval[2][0][1][ 2][29] =   1.069881e-01;
  //pc3dz_mean_c0d1z3
  n_val[2][0][1][3] = 30;
  val[2][0][1][ 3][ 0] =   0.000000e+00; eval[2][0][1][ 3][ 0] =   0.000000e+00;
  val[2][0][1][ 3][ 1] =   0.000000e+00; eval[2][0][1][ 3][ 1] =   0.000000e+00;
  val[2][0][1][ 3][ 2] =   3.965955e-01; eval[2][0][1][ 3][ 2] =   4.111708e-04;
  val[2][0][1][ 3][ 3] =   3.213577e-01; eval[2][0][1][ 3][ 3] =   4.090225e-04;
  val[2][0][1][ 3][ 4] =   2.734032e-01; eval[2][0][1][ 3][ 4] =   4.184260e-04;
  val[2][0][1][ 3][ 5] =   2.666511e-01; eval[2][0][1][ 3][ 5] =   6.040903e-04;
  val[2][0][1][ 3][ 6] =   2.282711e-01; eval[2][0][1][ 3][ 6] =   6.866497e-04;
  val[2][0][1][ 3][ 7] =   1.770305e-01; eval[2][0][1][ 3][ 7] =   8.545850e-04;
  val[2][0][1][ 3][ 8] =   1.469365e-01; eval[2][0][1][ 3][ 8] =   1.063257e-03;
  val[2][0][1][ 3][ 9] =   1.222489e-01; eval[2][0][1][ 3][ 9] =   1.414768e-03;
  val[2][0][1][ 3][10] =   1.131384e-01; eval[2][0][1][ 3][10] =   1.851133e-03;
  val[2][0][1][ 3][11] =   1.085732e-01; eval[2][0][1][ 3][11] =   2.422123e-03;
  val[2][0][1][ 3][12] =   1.075493e-01; eval[2][0][1][ 3][12] =   3.161829e-03;
  val[2][0][1][ 3][13] =   1.087996e-01; eval[2][0][1][ 3][13] =   4.070714e-03;
  val[2][0][1][ 3][14] =   1.041948e-01; eval[2][0][1][ 3][14] =   5.389785e-03;
  val[2][0][1][ 3][15] =   1.091838e-01; eval[2][0][1][ 3][15] =   6.950148e-03;
  val[2][0][1][ 3][16] =   9.698735e-02; eval[2][0][1][ 3][16] =   8.605469e-03;
  val[2][0][1][ 3][17] =   1.235763e-01; eval[2][0][1][ 3][17] =   1.055416e-02;
  val[2][0][1][ 3][18] =   9.895998e-02; eval[2][0][1][ 3][18] =   1.450883e-02;
  val[2][0][1][ 3][19] =   9.520219e-02; eval[2][0][1][ 3][19] =   1.723077e-02;
  val[2][0][1][ 3][20] =   9.505694e-02; eval[2][0][1][ 3][20] =   1.569935e-02;
  val[2][0][1][ 3][21] =   7.381866e-02; eval[2][0][1][ 3][21] =   2.343917e-02;
  val[2][0][1][ 3][22] =   1.025761e-01; eval[2][0][1][ 3][22] =   2.756337e-02;
  val[2][0][1][ 3][23] =   1.101954e-01; eval[2][0][1][ 3][23] =   4.174581e-02;
  val[2][0][1][ 3][24] =   1.745534e-01; eval[2][0][1][ 3][24] =   4.446003e-02;
  val[2][0][1][ 3][25] =   9.218488e-02; eval[2][0][1][ 3][25] =   1.069441e-01;
  val[2][0][1][ 3][26] =   8.770598e-02; eval[2][0][1][ 3][26] =   5.276487e-02;
  val[2][0][1][ 3][27] =   3.026057e-01; eval[2][0][1][ 3][27] =   7.244244e-02;
  val[2][0][1][ 3][28] =   2.616191e-01; eval[2][0][1][ 3][28] =   9.677193e-02;
  val[2][0][1][ 3][29] =  -6.626415e-02; eval[2][0][1][ 3][29] =   1.473319e-01;
  //pc3dz_mean_c0d1z4
  n_val[2][0][1][4] = 30;
  val[2][0][1][ 4][ 0] =   0.000000e+00; eval[2][0][1][ 4][ 0] =   0.000000e+00;
  val[2][0][1][ 4][ 1] =   0.000000e+00; eval[2][0][1][ 4][ 1] =   0.000000e+00;
  val[2][0][1][ 4][ 2] =   2.849415e-01; eval[2][0][1][ 4][ 2] =   4.914332e-04;
  val[2][0][1][ 4][ 3] =   2.607870e-01; eval[2][0][1][ 4][ 3] =   4.721026e-04;
  val[2][0][1][ 4][ 4] =   2.065735e-01; eval[2][0][1][ 4][ 4] =   5.810942e-04;
  val[2][0][1][ 4][ 5] =   2.055994e-01; eval[2][0][1][ 4][ 5] =   6.808606e-04;
  val[2][0][1][ 4][ 6] =   1.697402e-01; eval[2][0][1][ 4][ 6] =   7.878822e-04;
  val[2][0][1][ 4][ 7] =   1.371052e-01; eval[2][0][1][ 4][ 7] =   9.897308e-04;
  val[2][0][1][ 4][ 8] =   1.327787e-01; eval[2][0][1][ 4][ 8] =   1.237851e-03;
  val[2][0][1][ 4][ 9] =   1.266573e-01; eval[2][0][1][ 4][ 9] =   1.647101e-03;
  val[2][0][1][ 4][10] =   1.186448e-01; eval[2][0][1][ 4][10] =   2.161355e-03;
  val[2][0][1][ 4][11] =   1.112889e-01; eval[2][0][1][ 4][11] =   2.889903e-03;
  val[2][0][1][ 4][12] =   1.078411e-01; eval[2][0][1][ 4][12] =   3.877652e-03;
  val[2][0][1][ 4][13] =   1.134854e-01; eval[2][0][1][ 4][13] =   4.882641e-03;
  val[2][0][1][ 4][14] =   1.073797e-01; eval[2][0][1][ 4][14] =   6.420969e-03;
  val[2][0][1][ 4][15] =   1.049862e-01; eval[2][0][1][ 4][15] =   7.904723e-03;
  val[2][0][1][ 4][16] =   1.056542e-01; eval[2][0][1][ 4][16] =   1.037035e-02;
  val[2][0][1][ 4][17] =   1.126297e-01; eval[2][0][1][ 4][17] =   1.334088e-02;
  val[2][0][1][ 4][18] =   1.230938e-01; eval[2][0][1][ 4][18] =   1.615504e-02;
  val[2][0][1][ 4][19] =   7.572588e-02; eval[2][0][1][ 4][19] =   2.184414e-02;
  val[2][0][1][ 4][20] =   9.559098e-02; eval[2][0][1][ 4][20] =   1.859768e-02;
  val[2][0][1][ 4][21] =   9.132605e-02; eval[2][0][1][ 4][21] =   2.938565e-02;
  val[2][0][1][ 4][22] =   1.521964e-01; eval[2][0][1][ 4][22] =   3.243265e-02;
  val[2][0][1][ 4][23] =   4.476638e-02; eval[2][0][1][ 4][23] =   3.641264e-02;
  val[2][0][1][ 4][24] =   7.905071e-02; eval[2][0][1][ 4][24] =   5.479918e-02;
  val[2][0][1][ 4][25] =   6.512886e-04; eval[2][0][1][ 4][25] =   7.815810e-02;
  val[2][0][1][ 4][26] =  -3.575923e-04; eval[2][0][1][ 4][26] =   6.916612e-02;
  val[2][0][1][ 4][27] =   4.668556e-02; eval[2][0][1][ 4][27] =   2.493307e-01;
  val[2][0][1][ 4][28] =  -2.923125e-01; eval[2][0][1][ 4][28] =   9.600396e-02;
  val[2][0][1][ 4][29] =   2.503729e-01; eval[2][0][1][ 4][29] =   1.558661e-01;
  //pc3dz_mean_c0d1z5
  n_val[2][0][1][5] = 30;
  val[2][0][1][ 5][ 0] =   0.000000e+00; eval[2][0][1][ 5][ 0] =   0.000000e+00;
  val[2][0][1][ 5][ 1] =   0.000000e+00; eval[2][0][1][ 5][ 1] =   0.000000e+00;
  val[2][0][1][ 5][ 2] =   3.343919e-01; eval[2][0][1][ 5][ 2] =   5.206712e-04;
  val[2][0][1][ 5][ 3] =   2.342736e-01; eval[2][0][1][ 5][ 3] =   4.752799e-04;
  val[2][0][1][ 5][ 4] =   1.817448e-01; eval[2][0][1][ 5][ 4] =   5.923282e-04;
  val[2][0][1][ 5][ 5] =   1.643512e-01; eval[2][0][1][ 5][ 5] =   6.421722e-04;
  val[2][0][1][ 5][ 6] =   1.646258e-01; eval[2][0][1][ 5][ 6] =   8.210933e-04;
  val[2][0][1][ 5][ 7] =   1.753558e-01; eval[2][0][1][ 5][ 7] =   1.014112e-03;
  val[2][0][1][ 5][ 8] =   1.938355e-01; eval[2][0][1][ 5][ 8] =   1.238515e-03;
  val[2][0][1][ 5][ 9] =   2.067266e-01; eval[2][0][1][ 5][ 9] =   1.600704e-03;
  val[2][0][1][ 5][10] =   2.236072e-01; eval[2][0][1][ 5][10] =   2.073601e-03;
  val[2][0][1][ 5][11] =   2.386089e-01; eval[2][0][1][ 5][11] =   2.706432e-03;
  val[2][0][1][ 5][12] =   2.494934e-01; eval[2][0][1][ 5][12] =   3.507126e-03;
  val[2][0][1][ 5][13] =   2.544854e-01; eval[2][0][1][ 5][13] =   4.630487e-03;
  val[2][0][1][ 5][14] =   2.586009e-01; eval[2][0][1][ 5][14] =   5.934279e-03;
  val[2][0][1][ 5][15] =   2.665315e-01; eval[2][0][1][ 5][15] =   7.748838e-03;
  val[2][0][1][ 5][16] =   2.794193e-01; eval[2][0][1][ 5][16] =   9.905440e-03;
  val[2][0][1][ 5][17] =   2.883712e-01; eval[2][0][1][ 5][17] =   1.200527e-02;
  val[2][0][1][ 5][18] =   2.747455e-01; eval[2][0][1][ 5][18] =   1.405225e-02;
  val[2][0][1][ 5][19] =   2.664014e-01; eval[2][0][1][ 5][19] =   1.737670e-02;
  val[2][0][1][ 5][20] =   2.559123e-01; eval[2][0][1][ 5][20] =   1.610646e-02;
  val[2][0][1][ 5][21] =   2.483365e-01; eval[2][0][1][ 5][21] =   2.573758e-02;
  val[2][0][1][ 5][22] =   2.950295e-01; eval[2][0][1][ 5][22] =   3.048081e-02;
  val[2][0][1][ 5][23] =   3.345209e-01; eval[2][0][1][ 5][23] =   4.665438e-02;
  val[2][0][1][ 5][24] =   3.117099e-01; eval[2][0][1][ 5][24] =   4.582200e-02;
  val[2][0][1][ 5][25] =   2.354690e-01; eval[2][0][1][ 5][25] =   7.752419e-02;
  val[2][0][1][ 5][26] =   9.471050e-02; eval[2][0][1][ 5][26] =   7.140485e-02;
  val[2][0][1][ 5][27] =   2.407388e-01; eval[2][0][1][ 5][27] =   8.299712e-02;
  val[2][0][1][ 5][28] =   2.591884e-01; eval[2][0][1][ 5][28] =   9.640710e-02;
  val[2][0][1][ 5][29] =   3.012370e-01; eval[2][0][1][ 5][29] =   2.803049e-01;
  //pc3dz_mean_c0d1z6
  n_val[2][0][1][6] = 30;
  val[2][0][1][ 6][ 0] =   0.000000e+00; eval[2][0][1][ 6][ 0] =   0.000000e+00;
  val[2][0][1][ 6][ 1] =   0.000000e+00; eval[2][0][1][ 6][ 1] =   0.000000e+00;
  val[2][0][1][ 6][ 2] =   2.937302e-01; eval[2][0][1][ 6][ 2] =   4.578667e-04;
  val[2][0][1][ 6][ 3] =   2.262340e-01; eval[2][0][1][ 6][ 3] =   4.073186e-04;
  val[2][0][1][ 6][ 4] =   1.669879e-01; eval[2][0][1][ 6][ 4] =   5.069068e-04;
  val[2][0][1][ 6][ 5] =   1.664988e-01; eval[2][0][1][ 6][ 5] =   5.589093e-04;
  val[2][0][1][ 6][ 6] =   1.856743e-01; eval[2][0][1][ 6][ 6] =   7.130743e-04;
  val[2][0][1][ 6][ 7] =   2.261970e-01; eval[2][0][1][ 6][ 7] =   8.715421e-04;
  val[2][0][1][ 6][ 8] =   2.621055e-01; eval[2][0][1][ 6][ 8] =   1.072275e-03;
  val[2][0][1][ 6][ 9] =   2.877028e-01; eval[2][0][1][ 6][ 9] =   1.399172e-03;
  val[2][0][1][ 6][10] =   3.098606e-01; eval[2][0][1][ 6][10] =   1.830709e-03;
  val[2][0][1][ 6][11] =   3.279872e-01; eval[2][0][1][ 6][11] =   2.435280e-03;
  val[2][0][1][ 6][12] =   3.415854e-01; eval[2][0][1][ 6][12] =   3.238602e-03;
  val[2][0][1][ 6][13] =   3.435739e-01; eval[2][0][1][ 6][13] =   4.205286e-03;
  val[2][0][1][ 6][14] =   3.508409e-01; eval[2][0][1][ 6][14] =   5.549207e-03;
  val[2][0][1][ 6][15] =   3.505806e-01; eval[2][0][1][ 6][15] =   7.051850e-03;
  val[2][0][1][ 6][16] =   3.484067e-01; eval[2][0][1][ 6][16] =   8.619502e-03;
  val[2][0][1][ 6][17] =   3.528250e-01; eval[2][0][1][ 6][17] =   1.078270e-02;
  val[2][0][1][ 6][18] =   3.694154e-01; eval[2][0][1][ 6][18] =   1.332641e-02;
  val[2][0][1][ 6][19] =   3.574251e-01; eval[2][0][1][ 6][19] =   1.493275e-02;
  val[2][0][1][ 6][20] =   3.693626e-01; eval[2][0][1][ 6][20] =   1.540530e-02;
  val[2][0][1][ 6][21] =   3.708358e-01; eval[2][0][1][ 6][21] =   2.272306e-02;
  val[2][0][1][ 6][22] =   3.563028e-01; eval[2][0][1][ 6][22] =   3.261981e-02;
  val[2][0][1][ 6][23] =   3.250178e-01; eval[2][0][1][ 6][23] =   4.021789e-02;
  val[2][0][1][ 6][24] =   3.367640e-01; eval[2][0][1][ 6][24] =   4.769854e-02;
  val[2][0][1][ 6][25] =   2.735857e-01; eval[2][0][1][ 6][25] =   6.375282e-02;
  val[2][0][1][ 6][26] =   2.874258e-01; eval[2][0][1][ 6][26] =   8.858143e-02;
  val[2][0][1][ 6][27] =   4.364563e-01; eval[2][0][1][ 6][27] =   9.871217e-02;
  val[2][0][1][ 6][28] =   3.363557e-01; eval[2][0][1][ 6][28] =   1.365787e-01;
  val[2][0][1][ 6][29] =   2.870737e-01; eval[2][0][1][ 6][29] =   1.375656e-01;
  //pc3dz_mean_c0d1z7
  n_val[2][0][1][7] = 30;
  val[2][0][1][ 7][ 0] =   0.000000e+00; eval[2][0][1][ 7][ 0] =   0.000000e+00;
  val[2][0][1][ 7][ 1] =   0.000000e+00; eval[2][0][1][ 7][ 1] =   0.000000e+00;
  val[2][0][1][ 7][ 2] =   3.092635e-01; eval[2][0][1][ 7][ 2] =   4.563370e-04;
  val[2][0][1][ 7][ 3] =   2.407609e-01; eval[2][0][1][ 7][ 3] =   4.216831e-04;
  val[2][0][1][ 7][ 4] =   2.142431e-01; eval[2][0][1][ 7][ 4] =   4.446908e-04;
  val[2][0][1][ 7][ 5] =   1.742497e-01; eval[2][0][1][ 7][ 5] =   5.942189e-04;
  val[2][0][1][ 7][ 6] =   2.077229e-01; eval[2][0][1][ 7][ 6] =   7.581101e-04;
  val[2][0][1][ 7][ 7] =   2.832838e-01; eval[2][0][1][ 7][ 7] =   9.211883e-04;
  val[2][0][1][ 7][ 8] =   3.390546e-01; eval[2][0][1][ 7][ 8] =   1.140985e-03;
  val[2][0][1][ 7][ 9] =   3.704481e-01; eval[2][0][1][ 7][ 9] =   1.485820e-03;
  val[2][0][1][ 7][10] =   4.027804e-01; eval[2][0][1][ 7][10] =   1.993133e-03;
  val[2][0][1][ 7][11] =   4.280807e-01; eval[2][0][1][ 7][11] =   2.661534e-03;
  val[2][0][1][ 7][12] =   4.440815e-01; eval[2][0][1][ 7][12] =   3.538110e-03;
  val[2][0][1][ 7][13] =   4.695283e-01; eval[2][0][1][ 7][13] =   4.965231e-03;
  val[2][0][1][ 7][14] =   4.836692e-01; eval[2][0][1][ 7][14] =   6.127448e-03;
  val[2][0][1][ 7][15] =   4.861290e-01; eval[2][0][1][ 7][15] =   8.212675e-03;
  val[2][0][1][ 7][16] =   4.888944e-01; eval[2][0][1][ 7][16] =   1.032802e-02;
  val[2][0][1][ 7][17] =   5.177424e-01; eval[2][0][1][ 7][17] =   1.204398e-02;
  val[2][0][1][ 7][18] =   4.946208e-01; eval[2][0][1][ 7][18] =   1.345997e-02;
  val[2][0][1][ 7][19] =   5.260463e-01; eval[2][0][1][ 7][19] =   1.762747e-02;
  val[2][0][1][ 7][20] =   5.135914e-01; eval[2][0][1][ 7][20] =   1.728631e-02;
  val[2][0][1][ 7][21] =   5.316896e-01; eval[2][0][1][ 7][21] =   2.873290e-02;
  val[2][0][1][ 7][22] =   4.863494e-01; eval[2][0][1][ 7][22] =   2.640099e-02;
  val[2][0][1][ 7][23] =   6.028595e-01; eval[2][0][1][ 7][23] =   3.725497e-02;
  val[2][0][1][ 7][24] =   6.051036e-01; eval[2][0][1][ 7][24] =   4.375331e-02;
  val[2][0][1][ 7][25] =   5.361925e-01; eval[2][0][1][ 7][25] =   6.338837e-02;
  val[2][0][1][ 7][26] =   6.163776e-01; eval[2][0][1][ 7][26] =   5.373073e-02;
  val[2][0][1][ 7][27] =   5.288971e-01; eval[2][0][1][ 7][27] =   1.337026e-01;
  val[2][0][1][ 7][28] =   1.928610e-01; eval[2][0][1][ 7][28] =   7.742270e-02;
  val[2][0][1][ 7][29] =   7.500000e-01; eval[2][0][1][ 7][29] =   1.153273e-01;
  //pc3dz_mean_c0d1z8
  n_val[2][0][1][8] = 30;
  val[2][0][1][ 8][ 0] =   0.000000e+00; eval[2][0][1][ 8][ 0] =   0.000000e+00;
  val[2][0][1][ 8][ 1] =   0.000000e+00; eval[2][0][1][ 8][ 1] =   0.000000e+00;
  val[2][0][1][ 8][ 2] =   3.593776e-01; eval[2][0][1][ 8][ 2] =   4.377698e-04;
  val[2][0][1][ 8][ 3] =   3.227613e-01; eval[2][0][1][ 8][ 3] =   4.237474e-04;
  val[2][0][1][ 8][ 4] =   2.984693e-01; eval[2][0][1][ 8][ 4] =   4.594719e-04;
  val[2][0][1][ 8][ 5] =   3.174533e-01; eval[2][0][1][ 8][ 5] =   6.217990e-04;
  val[2][0][1][ 8][ 6] =   3.691095e-01; eval[2][0][1][ 8][ 6] =   7.020015e-04;
  val[2][0][1][ 8][ 7] =   4.784001e-01; eval[2][0][1][ 8][ 7] =   8.581789e-04;
  val[2][0][1][ 8][ 8] =   5.463718e-01; eval[2][0][1][ 8][ 8] =   1.072729e-03;
  val[2][0][1][ 8][ 9] =   6.070042e-01; eval[2][0][1][ 8][ 9] =   1.678358e-03;
  val[2][0][1][ 8][10] =   6.408058e-01; eval[2][0][1][ 8][10] =   2.104197e-03;
  val[2][0][1][ 8][11] =   6.719776e-01; eval[2][0][1][ 8][11] =   2.694945e-03;
  val[2][0][1][ 8][12] =   6.917695e-01; eval[2][0][1][ 8][12] =   3.533001e-03;
  val[2][0][1][ 8][13] =   7.101468e-01; eval[2][0][1][ 8][13] =   4.598385e-03;
  val[2][0][1][ 8][14] =   7.157521e-01; eval[2][0][1][ 8][14] =   5.922568e-03;
  val[2][0][1][ 8][15] =   7.389303e-01; eval[2][0][1][ 8][15] =   7.636170e-03;
  val[2][0][1][ 8][16] =   7.316324e-01; eval[2][0][1][ 8][16] =   9.146325e-03;
  val[2][0][1][ 8][17] =   7.354496e-01; eval[2][0][1][ 8][17] =   1.174504e-02;
  val[2][0][1][ 8][18] =   7.300593e-01; eval[2][0][1][ 8][18] =   1.412711e-02;
  val[2][0][1][ 8][19] =   7.402314e-01; eval[2][0][1][ 8][19] =   1.655097e-02;
  val[2][0][1][ 8][20] =   7.394107e-01; eval[2][0][1][ 8][20] =   1.763701e-02;
  val[2][0][1][ 8][21] =   7.502575e-01; eval[2][0][1][ 8][21] =   2.368565e-02;
  val[2][0][1][ 8][22] =   7.187185e-01; eval[2][0][1][ 8][22] =   3.028314e-02;
  val[2][0][1][ 8][23] =   7.407253e-01; eval[2][0][1][ 8][23] =   4.064920e-02;
  val[2][0][1][ 8][24] =   6.902077e-01; eval[2][0][1][ 8][24] =   5.424021e-02;
  val[2][0][1][ 8][25] =   6.329365e-01; eval[2][0][1][ 8][25] =   7.969761e-02;
  val[2][0][1][ 8][26] =   6.829193e-01; eval[2][0][1][ 8][26] =   7.599295e-02;
  val[2][0][1][ 8][27] =   6.421153e-01; eval[2][0][1][ 8][27] =   7.460527e-02;
  val[2][0][1][ 8][28] =   8.697958e-01; eval[2][0][1][ 8][28] =   1.906971e-01;
  val[2][0][1][ 8][29] =   8.300914e-01; eval[2][0][1][ 8][29] =   1.211421e-01;
  //pc3dz_mean_c0d1z9
  n_val[2][0][1][9] = 30;
  val[2][0][1][ 9][ 0] =   0.000000e+00; eval[2][0][1][ 9][ 0] =   0.000000e+00;
  val[2][0][1][ 9][ 1] =   0.000000e+00; eval[2][0][1][ 9][ 1] =   0.000000e+00;
  val[2][0][1][ 9][ 2] =   5.634591e-01; eval[2][0][1][ 9][ 2] =   5.935642e-04;
  val[2][0][1][ 9][ 3] =   4.736985e-01; eval[2][0][1][ 9][ 3] =   4.816454e-04;
  val[2][0][1][ 9][ 4] =   4.502178e-01; eval[2][0][1][ 9][ 4] =   5.880967e-04;
  val[2][0][1][ 9][ 5] =   4.089123e-01; eval[2][0][1][ 9][ 5] =   7.026206e-04;
  val[2][0][1][ 9][ 6] =   4.960264e-01; eval[2][0][1][ 9][ 6] =   8.191420e-04;
  val[2][0][1][ 9][ 7] =   6.562421e-01; eval[2][0][1][ 9][ 7] =   1.056350e-03;
  val[2][0][1][ 9][ 8] =   7.691132e-01; eval[2][0][1][ 9][ 8] =   1.487753e-03;
  val[2][0][1][ 9][ 9] =   8.375442e-01; eval[2][0][1][ 9][ 9] =   1.987390e-03;
  val[2][0][1][ 9][10] =   8.788762e-01; eval[2][0][1][ 9][10] =   2.665053e-03;
  val[2][0][1][ 9][11] =   9.230717e-01; eval[2][0][1][ 9][11] =   3.660305e-03;
  val[2][0][1][ 9][12] =   9.663895e-01; eval[2][0][1][ 9][12] =   3.846576e-03;
  val[2][0][1][ 9][13] =   9.857105e-01; eval[2][0][1][ 9][13] =   4.982818e-03;
  val[2][0][1][ 9][14] =   1.008232e+00; eval[2][0][1][ 9][14] =   6.399366e-03;
  val[2][0][1][ 9][15] =   1.020053e+00; eval[2][0][1][ 9][15] =   8.015270e-03;
  val[2][0][1][ 9][16] =   1.037421e+00; eval[2][0][1][ 9][16] =   1.039293e-02;
  val[2][0][1][ 9][17] =   1.069169e+00; eval[2][0][1][ 9][17] =   1.474892e-02;
  val[2][0][1][ 9][18] =   1.061201e+00; eval[2][0][1][ 9][18] =   1.788549e-02;
  val[2][0][1][ 9][19] =   1.054808e+00; eval[2][0][1][ 9][19] =   2.248195e-02;
  val[2][0][1][ 9][20] =   1.142878e+00; eval[2][0][1][ 9][20] =   2.052726e-02;
  val[2][0][1][ 9][21] =   1.138748e+00; eval[2][0][1][ 9][21] =   2.725836e-02;
  val[2][0][1][ 9][22] =   1.144686e+00; eval[2][0][1][ 9][22] =   3.729555e-02;
  val[2][0][1][ 9][23] =   1.108897e+00; eval[2][0][1][ 9][23] =   5.037409e-02;
  val[2][0][1][ 9][24] =   1.196055e+00; eval[2][0][1][ 9][24] =   5.688319e-02;
  val[2][0][1][ 9][25] =   1.095769e+00; eval[2][0][1][ 9][25] =   6.370961e-02;
  val[2][0][1][ 9][26] =   1.266302e+00; eval[2][0][1][ 9][26] =   7.249825e-02;
  val[2][0][1][ 9][27] =   1.310583e+00; eval[2][0][1][ 9][27] =   1.889706e-01;
  val[2][0][1][ 9][28] =   1.831885e+01; eval[2][0][1][ 9][28] =   9.933533e+00;
  val[2][0][1][ 9][29] =   1.225524e+00; eval[2][0][1][ 9][29] =   2.065708e-01;
  //pc3dz_mean_c1d0z0
  n_val[2][1][0][0] = 30;
  val[2][1][0][ 0][ 0] =   0.000000e+00; eval[2][1][0][ 0][ 0] =   0.000000e+00;
  val[2][1][0][ 0][ 1] =   0.000000e+00; eval[2][1][0][ 0][ 1] =   0.000000e+00;
  val[2][1][0][ 0][ 2] =   2.234736e+00; eval[2][1][0][ 0][ 2] =   4.233642e-04;
  val[2][1][0][ 0][ 3] =   2.261766e+00; eval[2][1][0][ 0][ 3] =   3.935556e-04;
  val[2][1][0][ 0][ 4] =   2.288625e+00; eval[2][1][0][ 0][ 4] =   4.805787e-04;
  val[2][1][0][ 0][ 5] =   2.298654e+00; eval[2][1][0][ 0][ 5] =   5.786522e-04;
  val[2][1][0][ 0][ 6] =   2.206748e+00; eval[2][1][0][ 0][ 6] =   6.640830e-04;
  val[2][1][0][ 0][ 7] =   2.037963e+00; eval[2][1][0][ 0][ 7] =   9.884638e-04;
  val[2][1][0][ 0][ 8] =   1.937940e+00; eval[2][1][0][ 0][ 8] =   1.211035e-03;
  val[2][1][0][ 0][ 9] =   1.853650e+00; eval[2][1][0][ 0][ 9] =   1.312541e-03;
  val[2][1][0][ 0][10] =   1.802104e+00; eval[2][1][0][ 0][10] =   1.661695e-03;
  val[2][1][0][ 0][11] =   1.766556e+00; eval[2][1][0][ 0][11] =   2.158999e-03;
  val[2][1][0][ 0][12] =   1.734935e+00; eval[2][1][0][ 0][12] =   2.766909e-03;
  val[2][1][0][ 0][13] =   1.723932e+00; eval[2][1][0][ 0][13] =   3.639916e-03;
  val[2][1][0][ 0][14] =   1.706417e+00; eval[2][1][0][ 0][14] =   4.734610e-03;
  val[2][1][0][ 0][15] =   1.686168e+00; eval[2][1][0][ 0][15] =   5.997477e-03;
  val[2][1][0][ 0][16] =   1.685800e+00; eval[2][1][0][ 0][16] =   1.106523e-02;
  val[2][1][0][ 0][17] =   1.659840e+00; eval[2][1][0][ 0][17] =   1.137475e-02;
  val[2][1][0][ 0][18] =   1.659916e+00; eval[2][1][0][ 0][18] =   1.208166e-02;
  val[2][1][0][ 0][19] =   1.638175e+00; eval[2][1][0][ 0][19] =   1.796875e-02;
  val[2][1][0][ 0][20] =   1.649998e+00; eval[2][1][0][ 0][20] =   1.406853e-02;
  val[2][1][0][ 0][21] =   1.632097e+00; eval[2][1][0][ 0][21] =   2.375000e-02;
  val[2][1][0][ 0][22] =   1.613703e+00; eval[2][1][0][ 0][22] =   2.665570e-02;
  val[2][1][0][ 0][23] =   1.596342e+00; eval[2][1][0][ 0][23] =   3.655251e-02;
  val[2][1][0][ 0][24] =   1.568994e+00; eval[2][1][0][ 0][24] =   4.984861e-02;
  val[2][1][0][ 0][25] =   1.646357e+00; eval[2][1][0][ 0][25] =   5.834430e-02;
  val[2][1][0][ 0][26] =   1.597572e+00; eval[2][1][0][ 0][26] =   6.273564e-02;
  val[2][1][0][ 0][27] =   1.600539e+00; eval[2][1][0][ 0][27] =   7.291133e-02;
  val[2][1][0][ 0][28] =   1.455968e+00; eval[2][1][0][ 0][28] =   1.470218e-01;
  val[2][1][0][ 0][29] =   1.826715e+00; eval[2][1][0][ 0][29] =   1.264310e-01;
  //pc3dz_mean_c1d0z1
  n_val[2][1][0][1] = 30;
  val[2][1][0][ 1][ 0] =   0.000000e+00; eval[2][1][0][ 1][ 0] =   0.000000e+00;
  val[2][1][0][ 1][ 1] =   0.000000e+00; eval[2][1][0][ 1][ 1] =   0.000000e+00;
  val[2][1][0][ 1][ 2] =   2.139848e+00; eval[2][1][0][ 1][ 2] =   4.071113e-04;
  val[2][1][0][ 1][ 3] =   2.135837e+00; eval[2][1][0][ 1][ 3] =   3.829742e-04;
  val[2][1][0][ 1][ 4] =   2.144832e+00; eval[2][1][0][ 1][ 4] =   4.705965e-04;
  val[2][1][0][ 1][ 5] =   2.174736e+00; eval[2][1][0][ 1][ 5] =   5.694779e-04;
  val[2][1][0][ 1][ 6] =   2.119209e+00; eval[2][1][0][ 1][ 6] =   6.814284e-04;
  val[2][1][0][ 1][ 7] =   2.001698e+00; eval[2][1][0][ 1][ 7] =   9.643563e-04;
  val[2][1][0][ 1][ 8] =   1.915660e+00; eval[2][1][0][ 1][ 8] =   1.065973e-03;
  val[2][1][0][ 1][ 9] =   1.869620e+00; eval[2][1][0][ 1][ 9] =   1.320465e-03;
  val[2][1][0][ 1][10] =   1.831812e+00; eval[2][1][0][ 1][10] =   1.682116e-03;
  val[2][1][0][ 1][11] =   1.806702e+00; eval[2][1][0][ 1][11] =   2.152034e-03;
  val[2][1][0][ 1][12] =   1.778636e+00; eval[2][1][0][ 1][12] =   2.777587e-03;
  val[2][1][0][ 1][13] =   1.766617e+00; eval[2][1][0][ 1][13] =   3.622600e-03;
  val[2][1][0][ 1][14] =   1.752779e+00; eval[2][1][0][ 1][14] =   4.701504e-03;
  val[2][1][0][ 1][15] =   1.740145e+00; eval[2][1][0][ 1][15] =   5.888508e-03;
  val[2][1][0][ 1][16] =   1.743323e+00; eval[2][1][0][ 1][16] =   7.420249e-03;
  val[2][1][0][ 1][17] =   1.732793e+00; eval[2][1][0][ 1][17] =   9.073551e-03;
  val[2][1][0][ 1][18] =   1.708193e+00; eval[2][1][0][ 1][18] =   1.179603e-02;
  val[2][1][0][ 1][19] =   1.704810e+00; eval[2][1][0][ 1][19] =   1.452114e-02;
  val[2][1][0][ 1][20] =   1.710751e+00; eval[2][1][0][ 1][20] =   1.305205e-02;
  val[2][1][0][ 1][21] =   1.720093e+00; eval[2][1][0][ 1][21] =   2.501500e-02;
  val[2][1][0][ 1][22] =   1.693966e+00; eval[2][1][0][ 1][22] =   2.671345e-02;
  val[2][1][0][ 1][23] =   1.641198e+00; eval[2][1][0][ 1][23] =   3.506917e-02;
  val[2][1][0][ 1][24] =   1.624045e+00; eval[2][1][0][ 1][24] =   3.915054e-02;
  val[2][1][0][ 1][25] =   1.813369e+00; eval[2][1][0][ 1][25] =   4.533677e-02;
  val[2][1][0][ 1][26] =   1.557499e+00; eval[2][1][0][ 1][26] =   9.522358e-02;
  val[2][1][0][ 1][27] =   1.447991e+00; eval[2][1][0][ 1][27] =   1.002690e-01;
  val[2][1][0][ 1][28] =   1.804681e+00; eval[2][1][0][ 1][28] =   1.383437e-01;
  val[2][1][0][ 1][29] =   1.729878e+00; eval[2][1][0][ 1][29] =   6.669436e-02;
  //pc3dz_mean_c1d0z2
  n_val[2][1][0][2] = 30;
  val[2][1][0][ 2][ 0] =   0.000000e+00; eval[2][1][0][ 2][ 0] =   0.000000e+00;
  val[2][1][0][ 2][ 1] =   0.000000e+00; eval[2][1][0][ 2][ 1] =   0.000000e+00;
  val[2][1][0][ 2][ 2] =   2.018647e+00; eval[2][1][0][ 2][ 2] =   4.333022e-04;
  val[2][1][0][ 2][ 3] =   2.026322e+00; eval[2][1][0][ 2][ 3] =   4.114895e-04;
  val[2][1][0][ 2][ 4] =   2.026573e+00; eval[2][1][0][ 2][ 4] =   4.510850e-04;
  val[2][1][0][ 2][ 5] =   2.036329e+00; eval[2][1][0][ 2][ 5] =   4.972377e-04;
  val[2][1][0][ 2][ 6] =   2.003313e+00; eval[2][1][0][ 2][ 6] =   5.823142e-04;
  val[2][1][0][ 2][ 7] =   1.925546e+00; eval[2][1][0][ 2][ 7] =   8.996223e-04;
  val[2][1][0][ 2][ 8] =   1.877059e+00; eval[2][1][0][ 2][ 8] =   1.050264e-03;
  val[2][1][0][ 2][ 9] =   1.850542e+00; eval[2][1][0][ 2][ 9] =   1.337347e-03;
  val[2][1][0][ 2][10] =   1.822487e+00; eval[2][1][0][ 2][10] =   1.715348e-03;
  val[2][1][0][ 2][11] =   1.806796e+00; eval[2][1][0][ 2][11] =   2.223702e-03;
  val[2][1][0][ 2][12] =   1.792810e+00; eval[2][1][0][ 2][12] =   2.891621e-03;
  val[2][1][0][ 2][13] =   1.780946e+00; eval[2][1][0][ 2][13] =   3.753900e-03;
  val[2][1][0][ 2][14] =   1.766444e+00; eval[2][1][0][ 2][14] =   4.828009e-03;
  val[2][1][0][ 2][15] =   1.763284e+00; eval[2][1][0][ 2][15] =   6.233048e-03;
  val[2][1][0][ 2][16] =   1.747071e+00; eval[2][1][0][ 2][16] =   7.895451e-03;
  val[2][1][0][ 2][17] =   1.748191e+00; eval[2][1][0][ 2][17] =   9.219405e-03;
  val[2][1][0][ 2][18] =   1.744982e+00; eval[2][1][0][ 2][18] =   1.267267e-02;
  val[2][1][0][ 2][19] =   1.741750e+00; eval[2][1][0][ 2][19] =   1.572977e-02;
  val[2][1][0][ 2][20] =   1.718483e+00; eval[2][1][0][ 2][20] =   1.366757e-02;
  val[2][1][0][ 2][21] =   1.717475e+00; eval[2][1][0][ 2][21] =   1.902710e-02;
  val[2][1][0][ 2][22] =   1.682730e+00; eval[2][1][0][ 2][22] =   2.442701e-02;
  val[2][1][0][ 2][23] =   1.731305e+00; eval[2][1][0][ 2][23] =   3.553145e-02;
  val[2][1][0][ 2][24] =   1.733931e+00; eval[2][1][0][ 2][24] =   4.865746e-02;
  val[2][1][0][ 2][25] =   1.813076e+00; eval[2][1][0][ 2][25] =   5.426656e-02;
  val[2][1][0][ 2][26] =   1.750774e+00; eval[2][1][0][ 2][26] =   6.617421e-02;
  val[2][1][0][ 2][27] =   1.750000e+00; eval[2][1][0][ 2][27] =   8.592197e-02;
  val[2][1][0][ 2][28] =   1.963437e+00; eval[2][1][0][ 2][28] =   1.446830e-01;
  val[2][1][0][ 2][29] =   1.719811e+00; eval[2][1][0][ 2][29] =   1.359043e-01;
  //pc3dz_mean_c1d0z3
  n_val[2][1][0][3] = 30;
  val[2][1][0][ 3][ 0] =   0.000000e+00; eval[2][1][0][ 3][ 0] =   0.000000e+00;
  val[2][1][0][ 3][ 1] =   0.000000e+00; eval[2][1][0][ 3][ 1] =   0.000000e+00;
  val[2][1][0][ 3][ 2] =   1.873570e+00; eval[2][1][0][ 3][ 2] =   4.101399e-04;
  val[2][1][0][ 3][ 3] =   1.860263e+00; eval[2][1][0][ 3][ 3] =   4.008727e-04;
  val[2][1][0][ 3][ 4] =   1.893837e+00; eval[2][1][0][ 3][ 4] =   4.764504e-04;
  val[2][1][0][ 3][ 5] =   1.896161e+00; eval[2][1][0][ 3][ 5] =   5.138838e-04;
  val[2][1][0][ 3][ 6] =   1.868124e+00; eval[2][1][0][ 3][ 6] =   7.034419e-04;
  val[2][1][0][ 3][ 7] =   1.824796e+00; eval[2][1][0][ 3][ 7] =   8.578840e-04;
  val[2][1][0][ 3][ 8] =   1.798242e+00; eval[2][1][0][ 3][ 8] =   1.046563e-03;
  val[2][1][0][ 3][ 9] =   1.781700e+00; eval[2][1][0][ 3][ 9] =   1.348414e-03;
  val[2][1][0][ 3][10] =   1.761566e+00; eval[2][1][0][ 3][10] =   1.754902e-03;
  val[2][1][0][ 3][11] =   1.752410e+00; eval[2][1][0][ 3][11] =   2.255891e-03;
  val[2][1][0][ 3][12] =   1.735010e+00; eval[2][1][0][ 3][12] =   2.938852e-03;
  val[2][1][0][ 3][13] =   1.726615e+00; eval[2][1][0][ 3][13] =   3.754416e-03;
  val[2][1][0][ 3][14] =   1.715807e+00; eval[2][1][0][ 3][14] =   4.778495e-03;
  val[2][1][0][ 3][15] =   1.714566e+00; eval[2][1][0][ 3][15] =   5.984211e-03;
  val[2][1][0][ 3][16] =   1.713250e+00; eval[2][1][0][ 3][16] =   7.584728e-03;
  val[2][1][0][ 3][17] =   1.680996e+00; eval[2][1][0][ 3][17] =   1.269594e-02;
  val[2][1][0][ 3][18] =   1.712835e+00; eval[2][1][0][ 3][18] =   1.105644e-02;
  val[2][1][0][ 3][19] =   1.687636e+00; eval[2][1][0][ 3][19] =   1.421703e-02;
  val[2][1][0][ 3][20] =   1.719947e+00; eval[2][1][0][ 3][20] =   1.402997e-02;
  val[2][1][0][ 3][21] =   1.674415e+00; eval[2][1][0][ 3][21] =   2.577817e-02;
  val[2][1][0][ 3][22] =   1.650550e+00; eval[2][1][0][ 3][22] =   2.836830e-02;
  val[2][1][0][ 3][23] =   1.752075e+00; eval[2][1][0][ 3][23] =   3.251973e-02;
  val[2][1][0][ 3][24] =   1.611893e+00; eval[2][1][0][ 3][24] =   4.415566e-02;
  val[2][1][0][ 3][25] =   1.727477e+00; eval[2][1][0][ 3][25] =   5.249021e-02;
  val[2][1][0][ 3][26] =   1.783677e+00; eval[2][1][0][ 3][26] =   7.048021e-02;
  val[2][1][0][ 3][27] =   1.579579e+00; eval[2][1][0][ 3][27] =   9.768396e-02;
  val[2][1][0][ 3][28] =   1.797786e+00; eval[2][1][0][ 3][28] =   8.832542e-02;
  val[2][1][0][ 3][29] =   1.785348e+00; eval[2][1][0][ 3][29] =   1.037094e-01;
  //pc3dz_mean_c1d0z4
  n_val[2][1][0][4] = 30;
  val[2][1][0][ 4][ 0] =   0.000000e+00; eval[2][1][0][ 4][ 0] =   0.000000e+00;
  val[2][1][0][ 4][ 1] =   0.000000e+00; eval[2][1][0][ 4][ 1] =   0.000000e+00;
  val[2][1][0][ 4][ 2] =   1.717184e+00; eval[2][1][0][ 4][ 2] =   4.291452e-04;
  val[2][1][0][ 4][ 3] =   1.739089e+00; eval[2][1][0][ 4][ 3] =   4.302829e-04;
  val[2][1][0][ 4][ 4] =   1.771192e+00; eval[2][1][0][ 4][ 4] =   6.022616e-04;
  val[2][1][0][ 4][ 5] =   1.777207e+00; eval[2][1][0][ 4][ 5] =   6.363988e-04;
  val[2][1][0][ 4][ 6] =   1.764961e+00; eval[2][1][0][ 4][ 6] =   7.491102e-04;
  val[2][1][0][ 4][ 7] =   1.749743e+00; eval[2][1][0][ 4][ 7] =   9.438964e-04;
  val[2][1][0][ 4][ 8] =   1.742410e+00; eval[2][1][0][ 4][ 8] =   1.155503e-03;
  val[2][1][0][ 4][ 9] =   1.738148e+00; eval[2][1][0][ 4][ 9] =   1.496166e-03;
  val[2][1][0][ 4][10] =   1.727765e+00; eval[2][1][0][ 4][10] =   1.941298e-03;
  val[2][1][0][ 4][11] =   1.724796e+00; eval[2][1][0][ 4][11] =   2.521003e-03;
  val[2][1][0][ 4][12] =   1.716927e+00; eval[2][1][0][ 4][12] =   3.307443e-03;
  val[2][1][0][ 4][13] =   1.709723e+00; eval[2][1][0][ 4][13] =   4.268001e-03;
  val[2][1][0][ 4][14] =   1.703697e+00; eval[2][1][0][ 4][14] =   5.440280e-03;
  val[2][1][0][ 4][15] =   1.693296e+00; eval[2][1][0][ 4][15] =   6.971415e-03;
  val[2][1][0][ 4][16] =   1.699344e+00; eval[2][1][0][ 4][16] =   8.713467e-03;
  val[2][1][0][ 4][17] =   1.686610e+00; eval[2][1][0][ 4][17] =   1.150063e-02;
  val[2][1][0][ 4][18] =   1.697093e+00; eval[2][1][0][ 4][18] =   1.374641e-02;
  val[2][1][0][ 4][19] =   1.655968e+00; eval[2][1][0][ 4][19] =   2.117624e-02;
  val[2][1][0][ 4][20] =   1.683399e+00; eval[2][1][0][ 4][20] =   1.608108e-02;
  val[2][1][0][ 4][21] =   1.673878e+00; eval[2][1][0][ 4][21] =   2.992160e-02;
  val[2][1][0][ 4][22] =   1.652093e+00; eval[2][1][0][ 4][22] =   3.046786e-02;
  val[2][1][0][ 4][23] =   1.699615e+00; eval[2][1][0][ 4][23] =   4.050723e-02;
  val[2][1][0][ 4][24] =   1.614087e+00; eval[2][1][0][ 4][24] =   6.143633e-02;
  val[2][1][0][ 4][25] =   1.750000e+00; eval[2][1][0][ 4][25] =   8.616938e-02;
  val[2][1][0][ 4][26] =   1.761322e+00; eval[2][1][0][ 4][26] =   5.931396e-02;
  val[2][1][0][ 4][27] =   1.517060e+00; eval[2][1][0][ 4][27] =   1.327676e-01;
  val[2][1][0][ 4][28] =   1.617044e+00; eval[2][1][0][ 4][28] =   8.941665e-02;
  val[2][1][0][ 4][29] =   1.958305e+00; eval[2][1][0][ 4][29] =   1.445705e-01;
  //pc3dz_mean_c1d0z5
  n_val[2][1][0][5] = 30;
  val[2][1][0][ 5][ 0] =   0.000000e+00; eval[2][1][0][ 5][ 0] =   0.000000e+00;
  val[2][1][0][ 5][ 1] =   0.000000e+00; eval[2][1][0][ 5][ 1] =   0.000000e+00;
  val[2][1][0][ 5][ 2] =   1.860374e+00; eval[2][1][0][ 5][ 2] =   4.579089e-04;
  val[2][1][0][ 5][ 3] =   1.879086e+00; eval[2][1][0][ 5][ 3] =   4.786202e-04;
  val[2][1][0][ 5][ 4] =   1.889949e+00; eval[2][1][0][ 5][ 4] =   6.009302e-04;
  val[2][1][0][ 5][ 5] =   1.889495e+00; eval[2][1][0][ 5][ 5] =   6.984802e-04;
  val[2][1][0][ 5][ 6] =   1.874245e+00; eval[2][1][0][ 5][ 6] =   9.816516e-04;
  val[2][1][0][ 5][ 7] =   1.886675e+00; eval[2][1][0][ 5][ 7] =   1.221537e-03;
  val[2][1][0][ 5][ 8] =   1.908283e+00; eval[2][1][0][ 5][ 8] =   1.507039e-03;
  val[2][1][0][ 5][ 9] =   1.916598e+00; eval[2][1][0][ 5][ 9] =   1.918233e-03;
  val[2][1][0][ 5][10] =   1.922717e+00; eval[2][1][0][ 5][10] =   2.496459e-03;
  val[2][1][0][ 5][11] =   1.938224e+00; eval[2][1][0][ 5][11] =   3.569651e-03;
  val[2][1][0][ 5][12] =   1.937441e+00; eval[2][1][0][ 5][12] =   4.500358e-03;
  val[2][1][0][ 5][13] =   1.936100e+00; eval[2][1][0][ 5][13] =   6.406680e-03;
  val[2][1][0][ 5][14] =   1.934501e+00; eval[2][1][0][ 5][14] =   7.911205e-03;
  val[2][1][0][ 5][15] =   1.935163e+00; eval[2][1][0][ 5][15] =   1.052564e-02;
  val[2][1][0][ 5][16] =   1.937551e+00; eval[2][1][0][ 5][16] =   1.224413e-02;
  val[2][1][0][ 5][17] =   1.932082e+00; eval[2][1][0][ 5][17] =   1.553545e-02;
  val[2][1][0][ 5][18] =   1.893165e+00; eval[2][1][0][ 5][18] =   2.076528e-02;
  val[2][1][0][ 5][19] =   1.942992e+00; eval[2][1][0][ 5][19] =   2.102314e-02;
  val[2][1][0][ 5][20] =   1.919937e+00; eval[2][1][0][ 5][20] =   2.185908e-02;
  val[2][1][0][ 5][21] =   1.909844e+00; eval[2][1][0][ 5][21] =   3.047699e-02;
  val[2][1][0][ 5][22] =   1.982771e+00; eval[2][1][0][ 5][22] =   4.075459e-02;
  val[2][1][0][ 5][23] =   1.808074e+00; eval[2][1][0][ 5][23] =   4.969051e-02;
  val[2][1][0][ 5][24] =   1.931483e+00; eval[2][1][0][ 5][24] =   6.603579e-02;
  val[2][1][0][ 5][25] =   1.899453e+00; eval[2][1][0][ 5][25] =   6.886230e-02;
  val[2][1][0][ 5][26] =   1.735703e+00; eval[2][1][0][ 5][26] =   8.239454e-02;
  val[2][1][0][ 5][27] =   1.936052e+00; eval[2][1][0][ 5][27] =   9.527841e-02;
  val[2][1][0][ 5][28] =   2.328837e+00; eval[2][1][0][ 5][28] =   9.119255e-01;
  val[2][1][0][ 5][29] =   2.365206e+00; eval[2][1][0][ 5][29] =   1.064662e-01;
  //pc3dz_mean_c1d0z6
  n_val[2][1][0][6] = 30;
  val[2][1][0][ 6][ 0] =   0.000000e+00; eval[2][1][0][ 6][ 0] =   0.000000e+00;
  val[2][1][0][ 6][ 1] =   0.000000e+00; eval[2][1][0][ 6][ 1] =   0.000000e+00;
  val[2][1][0][ 6][ 2] =   1.784908e+00; eval[2][1][0][ 6][ 2] =   3.957815e-04;
  val[2][1][0][ 6][ 3] =   1.812614e+00; eval[2][1][0][ 6][ 3] =   3.970895e-04;
  val[2][1][0][ 6][ 4] =   1.806850e+00; eval[2][1][0][ 6][ 4] =   5.421479e-04;
  val[2][1][0][ 6][ 5] =   1.792007e+00; eval[2][1][0][ 6][ 5] =   6.750317e-04;
  val[2][1][0][ 6][ 6] =   1.791027e+00; eval[2][1][0][ 6][ 6] =   7.788976e-04;
  val[2][1][0][ 6][ 7] =   1.828071e+00; eval[2][1][0][ 6][ 7] =   9.627196e-04;
  val[2][1][0][ 6][ 8] =   1.861945e+00; eval[2][1][0][ 6][ 8] =   1.192744e-03;
  val[2][1][0][ 6][ 9] =   1.883786e+00; eval[2][1][0][ 6][ 9] =   1.540235e-03;
  val[2][1][0][ 6][10] =   1.894024e+00; eval[2][1][0][ 6][10] =   2.020548e-03;
  val[2][1][0][ 6][11] =   1.901419e+00; eval[2][1][0][ 6][11] =   2.619528e-03;
  val[2][1][0][ 6][12] =   1.907778e+00; eval[2][1][0][ 6][12] =   3.446172e-03;
  val[2][1][0][ 6][13] =   1.904743e+00; eval[2][1][0][ 6][13] =   4.479406e-03;
  val[2][1][0][ 6][14] =   1.915104e+00; eval[2][1][0][ 6][14] =   6.424735e-03;
  val[2][1][0][ 6][15] =   1.917714e+00; eval[2][1][0][ 6][15] =   8.069421e-03;
  val[2][1][0][ 6][16] =   1.925569e+00; eval[2][1][0][ 6][16] =   9.902256e-03;
  val[2][1][0][ 6][17] =   1.923053e+00; eval[2][1][0][ 6][17] =   1.277576e-02;
  val[2][1][0][ 6][18] =   1.864923e+00; eval[2][1][0][ 6][18] =   1.766127e-02;
  val[2][1][0][ 6][19] =   1.901961e+00; eval[2][1][0][ 6][19] =   1.775857e-02;
  val[2][1][0][ 6][20] =   1.909602e+00; eval[2][1][0][ 6][20] =   1.754027e-02;
  val[2][1][0][ 6][21] =   1.872146e+00; eval[2][1][0][ 6][21] =   2.919916e-02;
  val[2][1][0][ 6][22] =   1.912274e+00; eval[2][1][0][ 6][22] =   2.934340e-02;
  val[2][1][0][ 6][23] =   1.864760e+00; eval[2][1][0][ 6][23] =   3.653341e-02;
  val[2][1][0][ 6][24] =   1.877847e+00; eval[2][1][0][ 6][24] =   4.860858e-02;
  val[2][1][0][ 6][25] =   1.984594e+00; eval[2][1][0][ 6][25] =   8.315130e-02;
  val[2][1][0][ 6][26] =   1.967040e+00; eval[2][1][0][ 6][26] =   8.771441e-02;
  val[2][1][0][ 6][27] =   1.787137e+00; eval[2][1][0][ 6][27] =   7.216143e-02;
  val[2][1][0][ 6][28] =  -6.750000e+00; eval[2][1][0][ 6][28] =   8.725037e-01;
  val[2][1][0][ 6][29] =   1.943896e+00; eval[2][1][0][ 6][29] =   3.752746e-01;
  //pc3dz_mean_c1d0z7
  n_val[2][1][0][7] = 30;
  val[2][1][0][ 7][ 0] =   0.000000e+00; eval[2][1][0][ 7][ 0] =   0.000000e+00;
  val[2][1][0][ 7][ 1] =   0.000000e+00; eval[2][1][0][ 7][ 1] =   0.000000e+00;
  val[2][1][0][ 7][ 2] =   1.693964e+00; eval[2][1][0][ 7][ 2] =   3.795663e-04;
  val[2][1][0][ 7][ 3] =   1.715016e+00; eval[2][1][0][ 7][ 3] =   3.795642e-04;
  val[2][1][0][ 7][ 4] =   1.695991e+00; eval[2][1][0][ 7][ 4] =   5.046461e-04;
  val[2][1][0][ 7][ 5] =   1.700552e+00; eval[2][1][0][ 7][ 5] =   6.587480e-04;
  val[2][1][0][ 7][ 6] =   1.718911e+00; eval[2][1][0][ 7][ 6] =   7.668951e-04;
  val[2][1][0][ 7][ 7] =   1.790098e+00; eval[2][1][0][ 7][ 7] =   9.196767e-04;
  val[2][1][0][ 7][ 8] =   1.846733e+00; eval[2][1][0][ 7][ 8] =   1.119274e-03;
  val[2][1][0][ 7][ 9] =   1.881394e+00; eval[2][1][0][ 7][ 9] =   1.454452e-03;
  val[2][1][0][ 7][10] =   1.902768e+00; eval[2][1][0][ 7][10] =   1.896656e-03;
  val[2][1][0][ 7][11] =   1.930155e+00; eval[2][1][0][ 7][11] =   2.831027e-03;
  val[2][1][0][ 7][12] =   1.934671e+00; eval[2][1][0][ 7][12] =   3.728274e-03;
  val[2][1][0][ 7][13] =   1.938332e+00; eval[2][1][0][ 7][13] =   4.841584e-03;
  val[2][1][0][ 7][14] =   1.945992e+00; eval[2][1][0][ 7][14] =   5.883577e-03;
  val[2][1][0][ 7][15] =   1.938106e+00; eval[2][1][0][ 7][15] =   7.548951e-03;
  val[2][1][0][ 7][16] =   1.965325e+00; eval[2][1][0][ 7][16] =   9.156810e-03;
  val[2][1][0][ 7][17] =   1.931357e+00; eval[2][1][0][ 7][17] =   1.209314e-02;
  val[2][1][0][ 7][18] =   1.923806e+00; eval[2][1][0][ 7][18] =   1.539502e-02;
  val[2][1][0][ 7][19] =   1.948638e+00; eval[2][1][0][ 7][19] =   1.834792e-02;
  val[2][1][0][ 7][20] =   1.910841e+00; eval[2][1][0][ 7][20] =   1.636999e-02;
  val[2][1][0][ 7][21] =   1.950765e+00; eval[2][1][0][ 7][21] =   2.063345e-02;
  val[2][1][0][ 7][22] =   1.914528e+00; eval[2][1][0][ 7][22] =   2.832397e-02;
  val[2][1][0][ 7][23] =   1.971807e+00; eval[2][1][0][ 7][23] =   3.155213e-02;
  val[2][1][0][ 7][24] =   1.951565e+00; eval[2][1][0][ 7][24] =   4.035965e-02;
  val[2][1][0][ 7][25] =   1.871071e+00; eval[2][1][0][ 7][25] =   6.676421e-02;
  val[2][1][0][ 7][26] =   1.788633e+00; eval[2][1][0][ 7][26] =   7.320437e-02;
  val[2][1][0][ 7][27] =   1.963667e+00; eval[2][1][0][ 7][27] =   6.758552e-02;
  val[2][1][0][ 7][28] =   2.019983e+00; eval[2][1][0][ 7][28] =   1.425848e-01;
  val[2][1][0][ 7][29] =   1.792635e+00; eval[2][1][0][ 7][29] =   1.225558e-01;
  //pc3dz_mean_c1d0z8
  n_val[2][1][0][8] = 30;
  val[2][1][0][ 8][ 0] =   0.000000e+00; eval[2][1][0][ 8][ 0] =   0.000000e+00;
  val[2][1][0][ 8][ 1] =   0.000000e+00; eval[2][1][0][ 8][ 1] =   0.000000e+00;
  val[2][1][0][ 8][ 2] =   1.641928e+00; eval[2][1][0][ 8][ 2] =   3.729951e-04;
  val[2][1][0][ 8][ 3] =   1.664293e+00; eval[2][1][0][ 8][ 3] =   3.753943e-04;
  val[2][1][0][ 8][ 4] =   1.640563e+00; eval[2][1][0][ 8][ 4] =   4.773352e-04;
  val[2][1][0][ 8][ 5] =   1.628762e+00; eval[2][1][0][ 8][ 5] =   5.824553e-04;
  val[2][1][0][ 8][ 6] =   1.684011e+00; eval[2][1][0][ 8][ 6] =   7.920622e-04;
  val[2][1][0][ 8][ 7] =   1.797778e+00; eval[2][1][0][ 8][ 7] =   9.319071e-04;
  val[2][1][0][ 8][ 8] =   1.881282e+00; eval[2][1][0][ 8][ 8] =   1.160337e-03;
  val[2][1][0][ 8][ 9] =   1.930175e+00; eval[2][1][0][ 8][ 9] =   1.521350e-03;
  val[2][1][0][ 8][10] =   1.970709e+00; eval[2][1][0][ 8][10] =   2.181699e-03;
  val[2][1][0][ 8][11] =   1.996552e+00; eval[2][1][0][ 8][11] =   2.740013e-03;
  val[2][1][0][ 8][12] =   2.011185e+00; eval[2][1][0][ 8][12] =   3.614071e-03;
  val[2][1][0][ 8][13] =   2.022213e+00; eval[2][1][0][ 8][13] =   4.611744e-03;
  val[2][1][0][ 8][14] =   2.028045e+00; eval[2][1][0][ 8][14] =   5.915937e-03;
  val[2][1][0][ 8][15] =   2.009268e+00; eval[2][1][0][ 8][15] =   7.432949e-03;
  val[2][1][0][ 8][16] =   2.031730e+00; eval[2][1][0][ 8][16] =   9.671325e-03;
  val[2][1][0][ 8][17] =   2.029584e+00; eval[2][1][0][ 8][17] =   1.081242e-02;
  val[2][1][0][ 8][18] =   2.017182e+00; eval[2][1][0][ 8][18] =   1.457327e-02;
  val[2][1][0][ 8][19] =   2.073180e+00; eval[2][1][0][ 8][19] =   1.805980e-02;
  val[2][1][0][ 8][20] =   2.027182e+00; eval[2][1][0][ 8][20] =   1.609586e-02;
  val[2][1][0][ 8][21] =   2.048618e+00; eval[2][1][0][ 8][21] =   2.264176e-02;
  val[2][1][0][ 8][22] =   2.060927e+00; eval[2][1][0][ 8][22] =   3.155781e-02;
  val[2][1][0][ 8][23] =   2.065240e+00; eval[2][1][0][ 8][23] =   3.865613e-02;
  val[2][1][0][ 8][24] =   2.030744e+00; eval[2][1][0][ 8][24] =   4.319997e-02;
  val[2][1][0][ 8][25] =   2.091062e+00; eval[2][1][0][ 8][25] =   7.421269e-02;
  val[2][1][0][ 8][26] =   2.156307e+00; eval[2][1][0][ 8][26] =   5.925486e-02;
  val[2][1][0][ 8][27] =   1.998290e+00; eval[2][1][0][ 8][27] =   9.005915e-02;
  val[2][1][0][ 8][28] =   1.892282e+00; eval[2][1][0][ 8][28] =   1.413697e-01;
  val[2][1][0][ 8][29] =   1.874530e+00; eval[2][1][0][ 8][29] =   1.740187e-01;
  //pc3dz_mean_c1d0z9
  n_val[2][1][0][9] = 30;
  val[2][1][0][ 9][ 0] =   0.000000e+00; eval[2][1][0][ 9][ 0] =   0.000000e+00;
  val[2][1][0][ 9][ 1] =   0.000000e+00; eval[2][1][0][ 9][ 1] =   0.000000e+00;
  val[2][1][0][ 9][ 2] =   1.654978e+00; eval[2][1][0][ 9][ 2] =   4.101154e-04;
  val[2][1][0][ 9][ 3] =   1.649164e+00; eval[2][1][0][ 9][ 3] =   3.969148e-04;
  val[2][1][0][ 9][ 4] =   1.618733e+00; eval[2][1][0][ 9][ 4] =   4.919007e-04;
  val[2][1][0][ 9][ 5] =   1.597446e+00; eval[2][1][0][ 9][ 5] =   5.908807e-04;
  val[2][1][0][ 9][ 6] =   1.683638e+00; eval[2][1][0][ 9][ 6] =   8.179095e-04;
  val[2][1][0][ 9][ 7] =   1.846386e+00; eval[2][1][0][ 9][ 7] =   9.931285e-04;
  val[2][1][0][ 9][ 8] =   1.965403e+00; eval[2][1][0][ 9][ 8] =   1.307411e-03;
  val[2][1][0][ 9][ 9] =   2.052608e+00; eval[2][1][0][ 9][ 9] =   1.603385e-03;
  val[2][1][0][ 9][10] =   2.098936e+00; eval[2][1][0][ 9][10] =   2.015914e-03;
  val[2][1][0][ 9][11] =   2.137067e+00; eval[2][1][0][ 9][11] =   2.537115e-03;
  val[2][1][0][ 9][12] =   2.161183e+00; eval[2][1][0][ 9][12] =   3.263941e-03;
  val[2][1][0][ 9][13] =   2.182767e+00; eval[2][1][0][ 9][13] =   4.075114e-03;
  val[2][1][0][ 9][14] =   2.194103e+00; eval[2][1][0][ 9][14] =   5.143982e-03;
  val[2][1][0][ 9][15] =   2.203248e+00; eval[2][1][0][ 9][15] =   6.838179e-03;
  val[2][1][0][ 9][16] =   2.206556e+00; eval[2][1][0][ 9][16] =   8.262079e-03;
  val[2][1][0][ 9][17] =   2.212437e+00; eval[2][1][0][ 9][17] =   9.726988e-03;
  val[2][1][0][ 9][18] =   2.204384e+00; eval[2][1][0][ 9][18] =   1.275685e-02;
  val[2][1][0][ 9][19] =   2.216348e+00; eval[2][1][0][ 9][19] =   1.574370e-02;
  val[2][1][0][ 9][20] =   2.241487e+00; eval[2][1][0][ 9][20] =   1.382196e-02;
  val[2][1][0][ 9][21] =   2.201062e+00; eval[2][1][0][ 9][21] =   2.067937e-02;
  val[2][1][0][ 9][22] =   2.212471e+00; eval[2][1][0][ 9][22] =   2.567600e-02;
  val[2][1][0][ 9][23] =   2.200650e+00; eval[2][1][0][ 9][23] =   3.698343e-02;
  val[2][1][0][ 9][24] =   2.238967e+00; eval[2][1][0][ 9][24] =   4.173137e-02;
  val[2][1][0][ 9][25] =   2.207218e+00; eval[2][1][0][ 9][25] =   5.697649e-02;
  val[2][1][0][ 9][26] =   2.262854e+00; eval[2][1][0][ 9][26] =   6.020383e-02;
  val[2][1][0][ 9][27] =   2.208836e+00; eval[2][1][0][ 9][27] =   7.681007e-02;
  val[2][1][0][ 9][28] =   6.421409e+01; eval[2][1][0][ 9][28] =   7.948628e+01;
  val[2][1][0][ 9][29] =   2.035372e+00; eval[2][1][0][ 9][29] =   1.642294e-01;
  //pc3dz_mean_c1d1z0
  n_val[2][1][1][0] = 30;
  val[2][1][1][ 0][ 0] =   0.000000e+00; eval[2][1][1][ 0][ 0] =   0.000000e+00;
  val[2][1][1][ 0][ 1] =   0.000000e+00; eval[2][1][1][ 0][ 1] =   0.000000e+00;
  val[2][1][1][ 0][ 2] =   5.700687e-01; eval[2][1][1][ 0][ 2] =   4.261865e-04;
  val[2][1][1][ 0][ 3] =   4.772754e-01; eval[2][1][1][ 0][ 3] =   4.080525e-04;
  val[2][1][1][ 0][ 4] =   4.629952e-01; eval[2][1][1][ 0][ 4] =   4.270508e-04;
  val[2][1][1][ 0][ 5] =   4.630878e-01; eval[2][1][1][ 0][ 5] =   4.978596e-04;
  val[2][1][1][ 0][ 6] =   3.491348e-01; eval[2][1][1][ 0][ 6] =   7.278039e-04;
  val[2][1][1][ 0][ 7] =   1.734862e-01; eval[2][1][1][ 0][ 7] =   8.862332e-04;
  val[2][1][1][ 0][ 8] =   5.702671e-02; eval[2][1][1][ 0][ 8] =   1.202492e-03;
  val[2][1][1][ 0][ 9] =  -3.459821e-02; eval[2][1][1][ 0][ 9] =   1.735740e-03;
  val[2][1][1][ 0][10] =  -1.064805e-01; eval[2][1][1][ 0][10] =   2.060106e-03;
  val[2][1][1][ 0][11] =  -1.551042e-01; eval[2][1][1][ 0][11] =   2.566028e-03;
  val[2][1][1][ 0][12] =  -1.881727e-01; eval[2][1][1][ 0][12] =   3.242679e-03;
  val[2][1][1][ 0][13] =  -2.149277e-01; eval[2][1][1][ 0][13] =   4.254319e-03;
  val[2][1][1][ 0][14] =  -2.401090e-01; eval[2][1][1][ 0][14] =   5.194222e-03;
  val[2][1][1][ 0][15] =  -2.508137e-01; eval[2][1][1][ 0][15] =   6.958954e-03;
  val[2][1][1][ 0][16] =  -2.880099e-01; eval[2][1][1][ 0][16] =   8.326127e-03;
  val[2][1][1][ 0][17] =  -2.998738e-01; eval[2][1][1][ 0][17] =   1.067148e-02;
  val[2][1][1][ 0][18] =  -3.022753e-01; eval[2][1][1][ 0][18] =   1.250518e-02;
  val[2][1][1][ 0][19] =  -2.926959e-01; eval[2][1][1][ 0][19] =   1.513814e-02;
  val[2][1][1][ 0][20] =  -3.350975e-01; eval[2][1][1][ 0][20] =   1.495789e-02;
  val[2][1][1][ 0][21] =  -2.942998e-01; eval[2][1][1][ 0][21] =   1.943981e-02;
  val[2][1][1][ 0][22] =  -3.309204e-01; eval[2][1][1][ 0][22] =   2.972751e-02;
  val[2][1][1][ 0][23] =  -3.265120e-01; eval[2][1][1][ 0][23] =   3.513477e-02;
  val[2][1][1][ 0][24] =  -4.226853e-01; eval[2][1][1][ 0][24] =   5.409061e-02;
  val[2][1][1][ 0][25] =  -2.501298e-01; eval[2][1][1][ 0][25] =   5.292192e-02;
  val[2][1][1][ 0][26] =  -2.905972e-01; eval[2][1][1][ 0][26] =   7.506890e-02;
  val[2][1][1][ 0][27] =  -2.919521e-01; eval[2][1][1][ 0][27] =   1.059557e-01;
  val[2][1][1][ 0][28] =  -2.673805e-01; eval[2][1][1][ 0][28] =   8.824891e-02;
  val[2][1][1][ 0][29] =  -1.846747e-01; eval[2][1][1][ 0][29] =   1.085339e-01;
  //pc3dz_mean_c1d1z1
  n_val[2][1][1][1] = 30;
  val[2][1][1][ 1][ 0] =   0.000000e+00; eval[2][1][1][ 1][ 0] =   0.000000e+00;
  val[2][1][1][ 1][ 1] =   0.000000e+00; eval[2][1][1][ 1][ 1] =   0.000000e+00;
  val[2][1][1][ 1][ 2] =   5.205691e-01; eval[2][1][1][ 1][ 2] =   3.866318e-04;
  val[2][1][1][ 1][ 3] =   4.122721e-01; eval[2][1][1][ 1][ 3] =   3.472962e-04;
  val[2][1][1][ 1][ 4] =   4.132665e-01; eval[2][1][1][ 1][ 4] =   3.933271e-04;
  val[2][1][1][ 1][ 5] =   4.151950e-01; eval[2][1][1][ 1][ 5] =   4.650276e-04;
  val[2][1][1][ 1][ 6] =   3.393851e-01; eval[2][1][1][ 1][ 6] =   6.640280e-04;
  val[2][1][1][ 1][ 7] =   2.083944e-01; eval[2][1][1][ 1][ 7] =   8.166952e-04;
  val[2][1][1][ 1][ 8] =   1.241454e-01; eval[2][1][1][ 1][ 8] =   1.056787e-03;
  val[2][1][1][ 1][ 9] =   5.471734e-02; eval[2][1][1][ 1][ 9] =   1.636046e-03;
  val[2][1][1][ 1][10] =   1.257393e-02; eval[2][1][1][ 1][10] =   2.102277e-03;
  val[2][1][1][ 1][11] =  -1.932834e-02; eval[2][1][1][ 1][11] =   2.733380e-03;
  val[2][1][1][ 1][12] =  -4.578849e-02; eval[2][1][1][ 1][12] =   3.664124e-03;
  val[2][1][1][ 1][13] =  -6.168868e-02; eval[2][1][1][ 1][13] =   4.738767e-03;
  val[2][1][1][ 1][14] =  -7.679850e-02; eval[2][1][1][ 1][14] =   5.694569e-03;
  val[2][1][1][ 1][15] =  -1.145926e-01; eval[2][1][1][ 1][15] =   8.165377e-03;
  val[2][1][1][ 1][16] =  -1.280532e-01; eval[2][1][1][ 1][16] =   9.906899e-03;
  val[2][1][1][ 1][17] =  -1.175050e-01; eval[2][1][1][ 1][17] =   1.052561e-02;
  val[2][1][1][ 1][18] =  -1.333928e-01; eval[2][1][1][ 1][18] =   1.274405e-02;
  val[2][1][1][ 1][19] =  -1.498976e-01; eval[2][1][1][ 1][19] =   1.495741e-02;
  val[2][1][1][ 1][20] =  -1.575132e-01; eval[2][1][1][ 1][20] =   1.374252e-02;
  val[2][1][1][ 1][21] =  -1.615868e-01; eval[2][1][1][ 1][21] =   2.063307e-02;
  val[2][1][1][ 1][22] =  -1.414163e-01; eval[2][1][1][ 1][22] =   2.449356e-02;
  val[2][1][1][ 1][23] =  -1.783372e-01; eval[2][1][1][ 1][23] =   3.784759e-02;
  val[2][1][1][ 1][24] =  -7.334718e-02; eval[2][1][1][ 1][24] =   4.176844e-02;
  val[2][1][1][ 1][25] =  -1.460681e-01; eval[2][1][1][ 1][25] =   4.872156e-02;
  val[2][1][1][ 1][26] =  -1.624084e-01; eval[2][1][1][ 1][26] =   6.762072e-02;
  val[2][1][1][ 1][27] =   9.175067e-03; eval[2][1][1][ 1][27] =   9.520704e-02;
  val[2][1][1][ 1][28] =  -7.447280e-02; eval[2][1][1][ 1][28] =   1.008369e-01;
  val[2][1][1][ 1][29] =  -2.282850e-01; eval[2][1][1][ 1][29] =   1.392820e-01;
  //pc3dz_mean_c1d1z2
  n_val[2][1][1][2] = 30;
  val[2][1][1][ 2][ 0] =   0.000000e+00; eval[2][1][1][ 2][ 0] =   0.000000e+00;
  val[2][1][1][ 2][ 1] =   0.000000e+00; eval[2][1][1][ 2][ 1] =   0.000000e+00;
  val[2][1][1][ 2][ 2] =   4.042214e-01; eval[2][1][1][ 2][ 2] =   3.772287e-04;
  val[2][1][1][ 2][ 3] =   3.190777e-01; eval[2][1][1][ 2][ 3] =   3.470094e-04;
  val[2][1][1][ 2][ 4] =   3.349623e-01; eval[2][1][1][ 2][ 4] =   4.229763e-04;
  val[2][1][1][ 2][ 5] =   3.269366e-01; eval[2][1][1][ 2][ 5] =   5.580551e-04;
  val[2][1][1][ 2][ 6] =   2.916102e-01; eval[2][1][1][ 2][ 6] =   6.767160e-04;
  val[2][1][1][ 2][ 7] =   2.061435e-01; eval[2][1][1][ 2][ 7] =   8.536172e-04;
  val[2][1][1][ 2][ 8] =   1.507841e-01; eval[2][1][1][ 2][ 8] =   1.096504e-03;
  val[2][1][1][ 2][ 9] =   1.059024e-01; eval[2][1][1][ 2][ 9] =   1.498178e-03;
  val[2][1][1][ 2][10] =   8.453370e-02; eval[2][1][1][ 2][10] =   1.964934e-03;
  val[2][1][1][ 2][11] =   5.413833e-02; eval[2][1][1][ 2][11] =   2.892803e-03;
  val[2][1][1][ 2][12] =   4.329185e-02; eval[2][1][1][ 2][12] =   3.842295e-03;
  val[2][1][1][ 2][13] =   3.928367e-02; eval[2][1][1][ 2][13] =   4.718570e-03;
  val[2][1][1][ 2][14] =   1.259152e-02; eval[2][1][1][ 2][14] =   6.062236e-03;
  val[2][1][1][ 2][15] =   4.074444e-03; eval[2][1][1][ 2][15] =   7.515051e-03;
  val[2][1][1][ 2][16] =   1.514556e-02; eval[2][1][1][ 2][16] =   1.011126e-02;
  val[2][1][1][ 2][17] =  -2.705942e-03; eval[2][1][1][ 2][17] =   1.172316e-02;
  val[2][1][1][ 2][18] =  -1.215957e-02; eval[2][1][1][ 2][18] =   1.307834e-02;
  val[2][1][1][ 2][19] =  -3.168443e-02; eval[2][1][1][ 2][19] =   1.919830e-02;
  val[2][1][1][ 2][20] =  -3.975276e-02; eval[2][1][1][ 2][20] =   1.704816e-02;
  val[2][1][1][ 2][21] =  -5.852535e-02; eval[2][1][1][ 2][21] =   2.247021e-02;
  val[2][1][1][ 2][22] =  -1.540723e-02; eval[2][1][1][ 2][22] =   2.479532e-02;
  val[2][1][1][ 2][23] =  -1.997164e-02; eval[2][1][1][ 2][23] =   3.438827e-02;
  val[2][1][1][ 2][24] =   5.833560e-03; eval[2][1][1][ 2][24] =   3.505005e-02;
  val[2][1][1][ 2][25] =   5.998921e-03; eval[2][1][1][ 2][25] =   4.405320e-02;
  val[2][1][1][ 2][26] =   1.933271e-02; eval[2][1][1][ 2][26] =   7.883519e-02;
  val[2][1][1][ 2][27] =   8.154846e-02; eval[2][1][1][ 2][27] =   8.231021e-02;
  val[2][1][1][ 2][28] =   1.988246e-01; eval[2][1][1][ 2][28] =   1.086283e-01;
  val[2][1][1][ 2][29] =  -1.125056e-01; eval[2][1][1][ 2][29] =   1.325631e-01;
  //pc3dz_mean_c1d1z3
  n_val[2][1][1][3] = 30;
  val[2][1][1][ 3][ 0] =   0.000000e+00; eval[2][1][1][ 3][ 0] =   0.000000e+00;
  val[2][1][1][ 3][ 1] =   0.000000e+00; eval[2][1][1][ 3][ 1] =   0.000000e+00;
  val[2][1][1][ 3][ 2] =   3.044462e-01; eval[2][1][1][ 3][ 2] =   3.586820e-04;
  val[2][1][1][ 3][ 3] =   2.626012e-01; eval[2][1][1][ 3][ 3] =   3.482349e-04;
  val[2][1][1][ 3][ 4] =   2.861636e-01; eval[2][1][1][ 3][ 4] =   4.370424e-04;
  val[2][1][1][ 3][ 5] =   2.818685e-01; eval[2][1][1][ 3][ 5] =   5.502368e-04;
  val[2][1][1][ 3][ 6] =   2.678121e-01; eval[2][1][1][ 3][ 6] =   6.655679e-04;
  val[2][1][1][ 3][ 7] =   2.237239e-01; eval[2][1][1][ 3][ 7] =   8.363033e-04;
  val[2][1][1][ 3][ 8] =   1.889646e-01; eval[2][1][1][ 3][ 8] =   1.067212e-03;
  val[2][1][1][ 3][ 9] =   1.616816e-01; eval[2][1][1][ 3][ 9] =   1.460301e-03;
  val[2][1][1][ 3][10] =   1.453501e-01; eval[2][1][1][ 3][10] =   1.927999e-03;
  val[2][1][1][ 3][11] =   1.320979e-01; eval[2][1][1][ 3][11] =   2.563243e-03;
  val[2][1][1][ 3][12] =   1.208500e-01; eval[2][1][1][ 3][12] =   3.459662e-03;
  val[2][1][1][ 3][13] =   1.125706e-01; eval[2][1][1][ 3][13] =   4.495978e-03;
  val[2][1][1][ 3][14] =   1.093192e-01; eval[2][1][1][ 3][14] =   5.574820e-03;
  val[2][1][1][ 3][15] =   1.098034e-01; eval[2][1][1][ 3][15] =   7.240788e-03;
  val[2][1][1][ 3][16] =   9.236617e-02; eval[2][1][1][ 3][16] =   1.040957e-02;
  val[2][1][1][ 3][17] =   1.128219e-01; eval[2][1][1][ 3][17] =   1.109087e-02;
  val[2][1][1][ 3][18] =   8.831745e-02; eval[2][1][1][ 3][18] =   1.669535e-02;
  val[2][1][1][ 3][19] =   1.109711e-01; eval[2][1][1][ 3][19] =   1.664063e-02;
  val[2][1][1][ 3][20] =   8.658688e-02; eval[2][1][1][ 3][20] =   1.976711e-02;
  val[2][1][1][ 3][21] =   1.068765e-01; eval[2][1][1][ 3][21] =   2.235441e-02;
  val[2][1][1][ 3][22] =   1.244115e-01; eval[2][1][1][ 3][22] =   3.060064e-02;
  val[2][1][1][ 3][23] =   1.011698e-01; eval[2][1][1][ 3][23] =   3.756966e-02;
  val[2][1][1][ 3][24] =   9.708337e-02; eval[2][1][1][ 3][24] =   4.453086e-02;
  val[2][1][1][ 3][25] =   9.297422e-02; eval[2][1][1][ 3][25] =   5.162411e-02;
  val[2][1][1][ 3][26] =   5.516594e-02; eval[2][1][1][ 3][26] =   4.847802e-02;
  val[2][1][1][ 3][27] =   2.206371e-01; eval[2][1][1][ 3][27] =   7.816382e-02;
  val[2][1][1][ 3][28] =   3.391838e-02; eval[2][1][1][ 3][28] =   1.595872e-01;
  val[2][1][1][ 3][29] =   1.490066e-01; eval[2][1][1][ 3][29] =   1.319616e-01;
  //pc3dz_mean_c1d1z4
  n_val[2][1][1][4] = 30;
  val[2][1][1][ 4][ 0] =   0.000000e+00; eval[2][1][1][ 4][ 0] =   0.000000e+00;
  val[2][1][1][ 4][ 1] =   0.000000e+00; eval[2][1][1][ 4][ 1] =   0.000000e+00;
  val[2][1][1][ 4][ 2] =   1.461289e-01; eval[2][1][1][ 4][ 2] =   3.907175e-04;
  val[2][1][1][ 4][ 3] =   1.390159e-01; eval[2][1][1][ 4][ 3] =   4.040027e-04;
  val[2][1][1][ 4][ 4] =   1.687954e-01; eval[2][1][1][ 4][ 4] =   5.106170e-04;
  val[2][1][1][ 4][ 5] =   1.989153e-01; eval[2][1][1][ 4][ 5] =   6.628722e-04;
  val[2][1][1][ 4][ 6] =   2.029749e-01; eval[2][1][1][ 4][ 6] =   8.132703e-04;
  val[2][1][1][ 4][ 7] =   1.850376e-01; eval[2][1][1][ 4][ 7] =   1.025218e-03;
  val[2][1][1][ 4][ 8] =   1.788850e-01; eval[2][1][1][ 4][ 8] =   1.296311e-03;
  val[2][1][1][ 4][ 9] =   1.782864e-01; eval[2][1][1][ 4][ 9] =   1.734261e-03;
  val[2][1][1][ 4][10] =   1.734450e-01; eval[2][1][1][ 4][10] =   2.344703e-03;
  val[2][1][1][ 4][11] =   1.682816e-01; eval[2][1][1][ 4][11] =   3.109873e-03;
  val[2][1][1][ 4][12] =   1.584038e-01; eval[2][1][1][ 4][12] =   4.065729e-03;
  val[2][1][1][ 4][13] =   1.463208e-01; eval[2][1][1][ 4][13] =   5.289713e-03;
  val[2][1][1][ 4][14] =   1.408446e-01; eval[2][1][1][ 4][14] =   6.729116e-03;
  val[2][1][1][ 4][15] =   1.393582e-01; eval[2][1][1][ 4][15] =   8.303181e-03;
  val[2][1][1][ 4][16] =   1.451259e-01; eval[2][1][1][ 4][16] =   1.056265e-02;
  val[2][1][1][ 4][17] =   1.458351e-01; eval[2][1][1][ 4][17] =   1.247895e-02;
  val[2][1][1][ 4][18] =   1.455616e-01; eval[2][1][1][ 4][18] =   1.612299e-02;
  val[2][1][1][ 4][19] =   1.085377e-01; eval[2][1][1][ 4][19] =   2.116398e-02;
  val[2][1][1][ 4][20] =   1.107517e-01; eval[2][1][1][ 4][20] =   1.903393e-02;
  val[2][1][1][ 4][21] =   1.164501e-01; eval[2][1][1][ 4][21] =   2.562926e-02;
  val[2][1][1][ 4][22] =   1.737552e-01; eval[2][1][1][ 4][22] =   3.050333e-02;
  val[2][1][1][ 4][23] =   1.598633e-01; eval[2][1][1][ 4][23] =   4.395371e-02;
  val[2][1][1][ 4][24] =   1.129581e-01; eval[2][1][1][ 4][24] =   5.376398e-02;
  val[2][1][1][ 4][25] =   1.619023e-01; eval[2][1][1][ 4][25] =   6.378376e-02;
  val[2][1][1][ 4][26] =   2.010679e-01; eval[2][1][1][ 4][26] =   6.932602e-02;
  val[2][1][1][ 4][27] =  -6.958090e-02; eval[2][1][1][ 4][27] =   9.730108e-02;
  val[2][1][1][ 4][28] =  -7.074972e+00; eval[2][1][1][ 4][28] =   3.422044e+00;
  val[2][1][1][ 4][29] =   1.910142e-02; eval[2][1][1][ 4][29] =   1.862145e-01;
  //pc3dz_mean_c1d1z5
  n_val[2][1][1][5] = 30;
  val[2][1][1][ 5][ 0] =   0.000000e+00; eval[2][1][1][ 5][ 0] =   0.000000e+00;
  val[2][1][1][ 5][ 1] =   0.000000e+00; eval[2][1][1][ 5][ 1] =   0.000000e+00;
  val[2][1][1][ 5][ 2] =   1.148144e-01; eval[2][1][1][ 5][ 2] =   4.204742e-04;
  val[2][1][1][ 5][ 3] =   1.271002e-01; eval[2][1][1][ 5][ 3] =   3.931439e-04;
  val[2][1][1][ 5][ 4] =   1.231506e-01; eval[2][1][1][ 5][ 4] =   4.573072e-04;
  val[2][1][1][ 5][ 5] =   1.554218e-01; eval[2][1][1][ 5][ 5] =   6.019643e-04;
  val[2][1][1][ 5][ 6] =   1.775576e-01; eval[2][1][1][ 5][ 6] =   7.353088e-04;
  val[2][1][1][ 5][ 7] =   1.987615e-01; eval[2][1][1][ 5][ 7] =   9.126993e-04;
  val[2][1][1][ 5][ 8] =   2.074242e-01; eval[2][1][1][ 5][ 8] =   1.163928e-03;
  val[2][1][1][ 5][ 9] =   2.100708e-01; eval[2][1][1][ 5][ 9] =   1.596430e-03;
  val[2][1][1][ 5][10] =   2.212622e-01; eval[2][1][1][ 5][10] =   2.148509e-03;
  val[2][1][1][ 5][11] =   2.163870e-01; eval[2][1][1][ 5][11] =   2.878120e-03;
  val[2][1][1][ 5][12] =   2.201353e-01; eval[2][1][1][ 5][12] =   3.660405e-03;
  val[2][1][1][ 5][13] =   2.059749e-01; eval[2][1][1][ 5][13] =   4.834726e-03;
  val[2][1][1][ 5][14] =   2.194327e-01; eval[2][1][1][ 5][14] =   6.108484e-03;
  val[2][1][1][ 5][15] =   2.296247e-01; eval[2][1][1][ 5][15] =   7.676260e-03;
  val[2][1][1][ 5][16] =   2.205261e-01; eval[2][1][1][ 5][16] =   9.927225e-03;
  val[2][1][1][ 5][17] =   2.409421e-01; eval[2][1][1][ 5][17] =   1.162406e-02;
  val[2][1][1][ 5][18] =   2.108854e-01; eval[2][1][1][ 5][18] =   1.613181e-02;
  val[2][1][1][ 5][19] =   2.391516e-01; eval[2][1][1][ 5][19] =   1.851916e-02;
  val[2][1][1][ 5][20] =   2.347212e-01; eval[2][1][1][ 5][20] =   1.656423e-02;
  val[2][1][1][ 5][21] =   2.415717e-01; eval[2][1][1][ 5][21] =   2.243091e-02;
  val[2][1][1][ 5][22] =   2.007684e-01; eval[2][1][1][ 5][22] =   2.958834e-02;
  val[2][1][1][ 5][23] =   2.349377e-01; eval[2][1][1][ 5][23] =   4.656898e-02;
  val[2][1][1][ 5][24] =   1.918440e-01; eval[2][1][1][ 5][24] =   4.709773e-02;
  val[2][1][1][ 5][25] =   3.116155e-01; eval[2][1][1][ 5][25] =   5.576019e-02;
  val[2][1][1][ 5][26] =   1.606574e-01; eval[2][1][1][ 5][26] =   6.908443e-02;
  val[2][1][1][ 5][27] =   5.667318e-01; eval[2][1][1][ 5][27] =   9.960604e-02;
  val[2][1][1][ 5][28] =   4.594212e-01; eval[2][1][1][ 5][28] =   1.331240e-01;
  val[2][1][1][ 5][29] =   2.500433e-01; eval[2][1][1][ 5][29] =   1.054512e-01;
  //pc3dz_mean_c1d1z6
  n_val[2][1][1][6] = 30;
  val[2][1][1][ 6][ 0] =   0.000000e+00; eval[2][1][1][ 6][ 0] =   0.000000e+00;
  val[2][1][1][ 6][ 1] =   0.000000e+00; eval[2][1][1][ 6][ 1] =   0.000000e+00;
  val[2][1][1][ 6][ 2] =  -9.718517e-03; eval[2][1][1][ 6][ 2] =   3.830891e-04;
  val[2][1][1][ 6][ 3] =   4.308956e-02; eval[2][1][1][ 6][ 3] =   3.638688e-04;
  val[2][1][1][ 6][ 4] =   7.475052e-02; eval[2][1][1][ 6][ 4] =   4.226947e-04;
  val[2][1][1][ 6][ 5] =   1.078737e-01; eval[2][1][1][ 6][ 5] =   4.956917e-04;
  val[2][1][1][ 6][ 6] =   1.598562e-01; eval[2][1][1][ 6][ 6] =   6.963429e-04;
  val[2][1][1][ 6][ 7] =   2.102051e-01; eval[2][1][1][ 6][ 7] =   8.657305e-04;
  val[2][1][1][ 6][ 8] =   2.416838e-01; eval[2][1][1][ 6][ 8] =   1.091771e-03;
  val[2][1][1][ 6][ 9] =   2.605711e-01; eval[2][1][1][ 6][ 9] =   1.476467e-03;
  val[2][1][1][ 6][10] =   2.751404e-01; eval[2][1][1][ 6][10] =   1.958966e-03;
  val[2][1][1][ 6][11] =   2.857168e-01; eval[2][1][1][ 6][11] =   2.607818e-03;
  val[2][1][1][ 6][12] =   2.930381e-01; eval[2][1][1][ 6][12] =   3.448988e-03;
  val[2][1][1][ 6][13] =   2.970196e-01; eval[2][1][1][ 6][13] =   4.429919e-03;
  val[2][1][1][ 6][14] =   3.060444e-01; eval[2][1][1][ 6][14] =   5.766569e-03;
  val[2][1][1][ 6][15] =   3.169954e-01; eval[2][1][1][ 6][15] =   7.083334e-03;
  val[2][1][1][ 6][16] =   3.143107e-01; eval[2][1][1][ 6][16] =   9.003972e-03;
  val[2][1][1][ 6][17] =   3.224246e-01; eval[2][1][1][ 6][17] =   1.051290e-02;
  val[2][1][1][ 6][18] =   3.107295e-01; eval[2][1][1][ 6][18] =   1.397676e-02;
  val[2][1][1][ 6][19] =   2.936977e-01; eval[2][1][1][ 6][19] =   1.722660e-02;
  val[2][1][1][ 6][20] =   3.591948e-01; eval[2][1][1][ 6][20] =   1.525956e-02;
  val[2][1][1][ 6][21] =   3.347858e-01; eval[2][1][1][ 6][21] =   2.288976e-02;
  val[2][1][1][ 6][22] =   3.235557e-01; eval[2][1][1][ 6][22] =   2.721563e-02;
  val[2][1][1][ 6][23] =   3.241801e-01; eval[2][1][1][ 6][23] =   3.317328e-02;
  val[2][1][1][ 6][24] =   3.047764e-01; eval[2][1][1][ 6][24] =   7.311468e-02;
  val[2][1][1][ 6][25] =   2.941597e-01; eval[2][1][1][ 6][25] =   5.716357e-02;
  val[2][1][1][ 6][26] =   2.404782e-01; eval[2][1][1][ 6][26] =   7.332383e-02;
  val[2][1][1][ 6][27] =   2.349175e-01; eval[2][1][1][ 6][27] =   7.222021e-02;
  val[2][1][1][ 6][28] =   5.101680e-01; eval[2][1][1][ 6][28] =   1.483397e-01;
  val[2][1][1][ 6][29] =   4.493050e-01; eval[2][1][1][ 6][29] =   9.537320e-02;
  //pc3dz_mean_c1d1z7
  n_val[2][1][1][7] = 30;
  val[2][1][1][ 7][ 0] =   0.000000e+00; eval[2][1][1][ 7][ 0] =   0.000000e+00;
  val[2][1][1][ 7][ 1] =   0.000000e+00; eval[2][1][1][ 7][ 1] =   0.000000e+00;
  val[2][1][1][ 7][ 2] =  -6.498612e-02; eval[2][1][1][ 7][ 2] =   3.606742e-04;
  val[2][1][1][ 7][ 3] =   3.363391e-03; eval[2][1][1][ 7][ 3] =   3.620574e-04;
  val[2][1][1][ 7][ 4] =   5.671912e-02; eval[2][1][1][ 7][ 4] =   3.924085e-04;
  val[2][1][1][ 7][ 5] =   1.091231e-01; eval[2][1][1][ 7][ 5] =   4.644717e-04;
  val[2][1][1][ 7][ 6] =   1.740183e-01; eval[2][1][1][ 7][ 6] =   6.658080e-04;
  val[2][1][1][ 7][ 7] =   2.639013e-01; eval[2][1][1][ 7][ 7] =   8.201997e-04;
  val[2][1][1][ 7][ 8] =   3.247766e-01; eval[2][1][1][ 7][ 8] =   1.041977e-03;
  val[2][1][1][ 7][ 9] =   3.529152e-01; eval[2][1][1][ 7][ 9] =   1.403771e-03;
  val[2][1][1][ 7][10] =   3.816686e-01; eval[2][1][1][ 7][10] =   1.911547e-03;
  val[2][1][1][ 7][11] =   3.994842e-01; eval[2][1][1][ 7][11] =   2.569519e-03;
  val[2][1][1][ 7][12] =   4.323987e-01; eval[2][1][1][ 7][12] =   3.867185e-03;
  val[2][1][1][ 7][13] =   4.406108e-01; eval[2][1][1][ 7][13] =   5.045740e-03;
  val[2][1][1][ 7][14] =   4.586184e-01; eval[2][1][1][ 7][14] =   6.581898e-03;
  val[2][1][1][ 7][15] =   4.618716e-01; eval[2][1][1][ 7][15] =   8.248449e-03;
  val[2][1][1][ 7][16] =   4.617918e-01; eval[2][1][1][ 7][16] =   9.647978e-03;
  val[2][1][1][ 7][17] =   4.648572e-01; eval[2][1][1][ 7][17] =   1.314567e-02;
  val[2][1][1][ 7][18] =   4.762812e-01; eval[2][1][1][ 7][18] =   1.567718e-02;
  val[2][1][1][ 7][19] =   4.427589e-01; eval[2][1][1][ 7][19] =   1.945221e-02;
  val[2][1][1][ 7][20] =   4.914171e-01; eval[2][1][1][ 7][20] =   1.686184e-02;
  val[2][1][1][ 7][21] =   4.929454e-01; eval[2][1][1][ 7][21] =   2.570264e-02;
  val[2][1][1][ 7][22] =   4.542224e-01; eval[2][1][1][ 7][22] =   2.586518e-02;
  val[2][1][1][ 7][23] =   4.617747e-01; eval[2][1][1][ 7][23] =   3.499994e-02;
  val[2][1][1][ 7][24] =   5.317814e-01; eval[2][1][1][ 7][24] =   4.587005e-02;
  val[2][1][1][ 7][25] =   4.168595e-01; eval[2][1][1][ 7][25] =   4.824741e-02;
  val[2][1][1][ 7][26] =   4.878523e-01; eval[2][1][1][ 7][26] =   6.914211e-02;
  val[2][1][1][ 7][27] =   5.009435e-01; eval[2][1][1][ 7][27] =   8.132535e-02;
  val[2][1][1][ 7][28] =   5.879930e-01; eval[2][1][1][ 7][28] =   1.039381e-01;
  val[2][1][1][ 7][29] =  -9.500000e+00; eval[2][1][1][ 7][29] =   2.181672e-01;
  //pc3dz_mean_c1d1z8
  n_val[2][1][1][8] = 30;
  val[2][1][1][ 8][ 0] =   0.000000e+00; eval[2][1][1][ 8][ 0] =   0.000000e+00;
  val[2][1][1][ 8][ 1] =   0.000000e+00; eval[2][1][1][ 8][ 1] =   0.000000e+00;
  val[2][1][1][ 8][ 2] =  -7.807424e-02; eval[2][1][1][ 8][ 2] =   3.456403e-04;
  val[2][1][1][ 8][ 3] =   1.147399e-02; eval[2][1][1][ 8][ 3] =   3.650168e-04;
  val[2][1][1][ 8][ 4] =   9.621314e-02; eval[2][1][1][ 8][ 4] =   4.080108e-04;
  val[2][1][1][ 8][ 5] =   1.605799e-01; eval[2][1][1][ 8][ 5] =   5.019097e-04;
  val[2][1][1][ 8][ 6] =   2.612831e-01; eval[2][1][1][ 8][ 6] =   7.047125e-04;
  val[2][1][1][ 8][ 7] =   4.055664e-01; eval[2][1][1][ 8][ 7] =   9.632358e-04;
  val[2][1][1][ 8][ 8] =   5.104415e-01; eval[2][1][1][ 8][ 8] =   9.868520e-04;
  val[2][1][1][ 8][ 9] =   5.667721e-01; eval[2][1][1][ 8][ 9] =   1.648232e-03;
  val[2][1][1][ 8][10] =   6.135235e-01; eval[2][1][1][ 8][10] =   2.069653e-03;
  val[2][1][1][ 8][11] =   6.462683e-01; eval[2][1][1][ 8][11] =   2.666757e-03;
  val[2][1][1][ 8][12] =   6.586328e-01; eval[2][1][1][ 8][12] =   3.549060e-03;
  val[2][1][1][ 8][13] =   6.864469e-01; eval[2][1][1][ 8][13] =   4.661687e-03;
  val[2][1][1][ 8][14] =   6.976408e-01; eval[2][1][1][ 8][14] =   5.844989e-03;
  val[2][1][1][ 8][15] =   6.999972e-01; eval[2][1][1][ 8][15] =   7.665890e-03;
  val[2][1][1][ 8][16] =   7.206121e-01; eval[2][1][1][ 8][16] =   9.410656e-03;
  val[2][1][1][ 8][17] =   7.197095e-01; eval[2][1][1][ 8][17] =   1.119766e-02;
  val[2][1][1][ 8][18] =   7.160242e-01; eval[2][1][1][ 8][18] =   1.488925e-02;
  val[2][1][1][ 8][19] =   7.385282e-01; eval[2][1][1][ 8][19] =   1.754727e-02;
  val[2][1][1][ 8][20] =   7.315478e-01; eval[2][1][1][ 8][20] =   1.624060e-02;
  val[2][1][1][ 8][21] =   7.281986e-01; eval[2][1][1][ 8][21] =   2.234922e-02;
  val[2][1][1][ 8][22] =   7.645025e-01; eval[2][1][1][ 8][22] =   2.650381e-02;
  val[2][1][1][ 8][23] =   7.458288e-01; eval[2][1][1][ 8][23] =   3.404300e-02;
  val[2][1][1][ 8][24] =   7.675764e-01; eval[2][1][1][ 8][24] =   4.881645e-02;
  val[2][1][1][ 8][25] =   6.903697e-01; eval[2][1][1][ 8][25] =   6.441471e-02;
  val[2][1][1][ 8][26] =   8.212272e-01; eval[2][1][1][ 8][26] =   6.613322e-02;
  val[2][1][1][ 8][27] =   4.687747e-01; eval[2][1][1][ 8][27] =   1.066900e-01;
  val[2][1][1][ 8][28] =   9.640751e-01; eval[2][1][1][ 8][28] =   1.055882e-01;
  val[2][1][1][ 8][29] =   7.946214e-01; eval[2][1][1][ 8][29] =   1.088655e-01;
  //pc3dz_mean_c1d1z9
  n_val[2][1][1][9] = 30;
  val[2][1][1][ 9][ 0] =   0.000000e+00; eval[2][1][1][ 9][ 0] =   0.000000e+00;
  val[2][1][1][ 9][ 1] =   0.000000e+00; eval[2][1][1][ 9][ 1] =   0.000000e+00;
  val[2][1][1][ 9][ 2] =  -2.700682e-02; eval[2][1][1][ 9][ 2] =   3.684911e-04;
  val[2][1][1][ 9][ 3] =   6.652203e-02; eval[2][1][1][ 9][ 3] =   3.758329e-04;
  val[2][1][1][ 9][ 4] =   1.238306e-01; eval[2][1][1][ 9][ 4] =   4.610590e-04;
  val[2][1][1][ 9][ 5] =   2.063960e-01; eval[2][1][1][ 9][ 5] =   6.188815e-04;
  val[2][1][1][ 9][ 6] =   3.289021e-01; eval[2][1][1][ 9][ 6] =   8.050559e-04;
  val[2][1][1][ 9][ 7] =   5.309228e-01; eval[2][1][1][ 9][ 7] =   8.763535e-04;
  val[2][1][1][ 9][ 8] =   6.687505e-01; eval[2][1][1][ 9][ 8] =   1.253282e-03;
  val[2][1][1][ 9][ 9] =   7.487105e-01; eval[2][1][1][ 9][ 9] =   1.627826e-03;
  val[2][1][1][ 9][10] =   8.060945e-01; eval[2][1][1][ 9][10] =   2.184666e-03;
  val[2][1][1][ 9][11] =   8.495097e-01; eval[2][1][1][ 9][11] =   2.941581e-03;
  val[2][1][1][ 9][12] =   9.006215e-01; eval[2][1][1][ 9][12] =   4.077235e-03;
  val[2][1][1][ 9][13] =   9.309758e-01; eval[2][1][1][ 9][13] =   5.326418e-03;
  val[2][1][1][ 9][14] =   9.754269e-01; eval[2][1][1][ 9][14] =   7.157306e-03;
  val[2][1][1][ 9][15] =   9.981563e-01; eval[2][1][1][ 9][15] =   8.561401e-03;
  val[2][1][1][ 9][16] =   9.981285e-01; eval[2][1][1][ 9][16] =   1.234179e-02;
  val[2][1][1][ 9][17] =   1.015836e+00; eval[2][1][1][ 9][17] =   1.430916e-02;
  val[2][1][1][ 9][18] =   1.022924e+00; eval[2][1][1][ 9][18] =   1.626135e-02;
  val[2][1][1][ 9][19] =   1.034237e+00; eval[2][1][1][ 9][19] =   2.124775e-02;
  val[2][1][1][ 9][20] =   1.077388e+00; eval[2][1][1][ 9][20] =   1.772614e-02;
  val[2][1][1][ 9][21] =   1.076001e+00; eval[2][1][1][ 9][21] =   2.923131e-02;
  val[2][1][1][ 9][22] =   1.062052e+00; eval[2][1][1][ 9][22] =   3.087247e-02;
  val[2][1][1][ 9][23] =   1.104992e+00; eval[2][1][1][ 9][23] =   4.174087e-02;
  val[2][1][1][ 9][24] =   1.064012e+00; eval[2][1][1][ 9][24] =   5.010830e-02;
  val[2][1][1][ 9][25] =   1.025433e+00; eval[2][1][1][ 9][25] =   7.316897e-02;
  val[2][1][1][ 9][26] =   1.236707e+00; eval[2][1][1][ 9][26] =   8.513206e-02;
  val[2][1][1][ 9][27] =   9.948603e-01; eval[2][1][1][ 9][27] =   8.464833e-02;
  val[2][1][1][ 9][28] =   1.046017e+00; eval[2][1][1][ 9][28] =   1.888541e-01;
  val[2][1][1][ 9][29] =   1.021921e+00; eval[2][1][1][ 9][29] =   1.366903e-01;
  //pc3dz_sigma_c0d0z0
  n_val[3][0][0][0] = 30;
  val[3][0][0][ 0][ 0] =   0.000000e+00; eval[3][0][0][ 0][ 0] =   0.000000e+00;
  val[3][0][0][ 0][ 1] =   0.000000e+00; eval[3][0][0][ 0][ 1] =   0.000000e+00;
  val[3][0][0][ 0][ 2] =   1.520151e+00; eval[3][0][0][ 0][ 2] =   4.708820e-04;
  val[3][0][0][ 0][ 3] =   1.175859e+00; eval[3][0][0][ 0][ 3] =   5.036404e-04;
  val[3][0][0][ 0][ 4] =   1.015988e+00; eval[3][0][0][ 0][ 4] =   5.497099e-04;
  val[3][0][0][ 0][ 5] =   9.260812e-01; eval[3][0][0][ 0][ 5] =   5.660331e-04;
  val[3][0][0][ 0][ 6] =   8.526397e-01; eval[3][0][0][ 0][ 6] =   8.440482e-04;
  val[3][0][0][ 0][ 7] =   8.079228e-01; eval[3][0][0][ 0][ 7] =   9.335548e-04;
  val[3][0][0][ 0][ 8] =   7.615518e-01; eval[3][0][0][ 0][ 8] =   1.628814e-03;
  val[3][0][0][ 0][ 9] =   7.685965e-01; eval[3][0][0][ 0][ 9] =   1.380335e-03;
  val[3][0][0][ 0][10] =   7.524999e-01; eval[3][0][0][ 0][10] =   1.714734e-03;
  val[3][0][0][ 0][11] =   7.396759e-01; eval[3][0][0][ 0][11] =   2.149086e-03;
  val[3][0][0][ 0][12] =   7.317210e-01; eval[3][0][0][ 0][12] =   2.730291e-03;
  val[3][0][0][ 0][13] =   7.288105e-01; eval[3][0][0][ 0][13] =   3.529567e-03;
  val[3][0][0][ 0][14] =   7.357465e-01; eval[3][0][0][ 0][14] =   4.647547e-03;
  val[3][0][0][ 0][15] =   7.218790e-01; eval[3][0][0][ 0][15] =   5.651640e-03;
  val[3][0][0][ 0][16] =   7.396972e-01; eval[3][0][0][ 0][16] =   7.415590e-03;
  val[3][0][0][ 0][17] =   7.469004e-01; eval[3][0][0][ 0][17] =   1.534338e-02;
  val[3][0][0][ 0][18] =   7.340287e-01; eval[3][0][0][ 0][18] =   1.109052e-02;
  val[3][0][0][ 0][19] =   6.949584e-01; eval[3][0][0][ 0][19] =   1.878577e-02;
  val[3][0][0][ 0][20] =   7.396592e-01; eval[3][0][0][ 0][20] =   2.047164e-02;
  val[3][0][0][ 0][21] =   7.361842e-01; eval[3][0][0][ 0][21] =   2.854581e-02;
  val[3][0][0][ 0][22] =   7.458938e-01; eval[3][0][0][ 0][22] =   2.530348e-02;
  val[3][0][0][ 0][23] =   6.939595e-01; eval[3][0][0][ 0][23] =   2.681767e-02;
  val[3][0][0][ 0][24] =   6.849361e-01; eval[3][0][0][ 0][24] =   3.469198e-02;
  val[3][0][0][ 0][25] =   8.209363e-01; eval[3][0][0][ 0][25] =   3.963475e-02;
  val[3][0][0][ 0][26] =   6.103215e-01; eval[3][0][0][ 0][26] =   6.374586e-02;
  val[3][0][0][ 0][27] =   5.230684e-01; eval[3][0][0][ 0][27] =   4.873296e-02;
  val[3][0][0][ 0][28] =   6.693494e-01; eval[3][0][0][ 0][28] =   1.087386e-01;
  val[3][0][0][ 0][29] =   4.809277e-01; eval[3][0][0][ 0][29] =   1.147781e-01;
  //pc3dz_sigma_c0d0z1
  n_val[3][0][0][1] = 30;
  val[3][0][0][ 1][ 0] =   0.000000e+00; eval[3][0][0][ 1][ 0] =   0.000000e+00;
  val[3][0][0][ 1][ 1] =   0.000000e+00; eval[3][0][0][ 1][ 1] =   0.000000e+00;
  val[3][0][0][ 1][ 2] =   1.475369e+00; eval[3][0][0][ 1][ 2] =   5.505949e-04;
  val[3][0][0][ 1][ 3] =   1.156134e+00; eval[3][0][0][ 1][ 3] =   4.854699e-04;
  val[3][0][0][ 1][ 4] =   1.026288e+00; eval[3][0][0][ 1][ 4] =   4.333648e-04;
  val[3][0][0][ 1][ 5] =   8.969012e-01; eval[3][0][0][ 1][ 5] =   7.690074e-04;
  val[3][0][0][ 1][ 6] =   8.378437e-01; eval[3][0][0][ 1][ 6] =   8.086129e-04;
  val[3][0][0][ 1][ 7] =   7.993759e-01; eval[3][0][0][ 1][ 7] =   9.320848e-04;
  val[3][0][0][ 1][ 8] =   7.555532e-01; eval[3][0][0][ 1][ 8] =   1.626572e-03;
  val[3][0][0][ 1][ 9] =   7.637160e-01; eval[3][0][0][ 1][ 9] =   1.384844e-03;
  val[3][0][0][ 1][10] =   7.500875e-01; eval[3][0][0][ 1][10] =   1.743337e-03;
  val[3][0][0][ 1][11] =   7.357157e-01; eval[3][0][0][ 1][11] =   2.170432e-03;
  val[3][0][0][ 1][12] =   7.279401e-01; eval[3][0][0][ 1][12] =   2.768418e-03;
  val[3][0][0][ 1][13] =   7.257308e-01; eval[3][0][0][ 1][13] =   3.553281e-03;
  val[3][0][0][ 1][14] =   7.277175e-01; eval[3][0][0][ 1][14] =   4.624416e-03;
  val[3][0][0][ 1][15] =   7.198421e-01; eval[3][0][0][ 1][15] =   5.710291e-03;
  val[3][0][0][ 1][16] =   7.080800e-01; eval[3][0][0][ 1][16] =   6.749457e-03;
  val[3][0][0][ 1][17] =   7.094307e-01; eval[3][0][0][ 1][17] =   8.502701e-03;
  val[3][0][0][ 1][18] =   7.082633e-01; eval[3][0][0][ 1][18] =   1.052974e-02;
  val[3][0][0][ 1][19] =   6.919144e-01; eval[3][0][0][ 1][19] =   1.211555e-02;
  val[3][0][0][ 1][20] =   6.783988e-01; eval[3][0][0][ 1][20] =   1.690896e-02;
  val[3][0][0][ 1][21] =   7.018947e-01; eval[3][0][0][ 1][21] =   1.601115e-02;
  val[3][0][0][ 1][22] =   7.156244e-01; eval[3][0][0][ 1][22] =   2.401011e-02;
  val[3][0][0][ 1][23] =   7.227675e-01; eval[3][0][0][ 1][23] =   3.099843e-02;
  val[3][0][0][ 1][24] =   6.510985e-01; eval[3][0][0][ 1][24] =   3.180249e-02;
  val[3][0][0][ 1][25] =   7.266300e-01; eval[3][0][0][ 1][25] =   4.663678e-02;
  val[3][0][0][ 1][26] =   7.753237e-01; eval[3][0][0][ 1][26] =   4.831571e-02;
  val[3][0][0][ 1][27] =   7.016908e-01; eval[3][0][0][ 1][27] =   7.096871e-02;
  val[3][0][0][ 1][28] =   6.961852e-01; eval[3][0][0][ 1][28] =   9.434891e-02;
  val[3][0][0][ 1][29] =   9.052464e-01; eval[3][0][0][ 1][29] =   1.147537e-01;
  //pc3dz_sigma_c0d0z2
  n_val[3][0][0][2] = 30;
  val[3][0][0][ 2][ 0] =   0.000000e+00; eval[3][0][0][ 2][ 0] =   0.000000e+00;
  val[3][0][0][ 2][ 1] =   0.000000e+00; eval[3][0][0][ 2][ 1] =   0.000000e+00;
  val[3][0][0][ 2][ 2] =   1.465544e+00; eval[3][0][0][ 2][ 2] =   5.346323e-04;
  val[3][0][0][ 2][ 3] =   1.151143e+00; eval[3][0][0][ 2][ 3] =   4.762237e-04;
  val[3][0][0][ 2][ 4] =   1.001399e+00; eval[3][0][0][ 2][ 4] =   5.520744e-04;
  val[3][0][0][ 2][ 5] =   9.123619e-01; eval[3][0][0][ 2][ 5] =   5.655685e-04;
  val[3][0][0][ 2][ 6] =   8.357066e-01; eval[3][0][0][ 2][ 6] =   8.345029e-04;
  val[3][0][0][ 2][ 7] =   8.051270e-01; eval[3][0][0][ 2][ 7] =   9.818713e-04;
  val[3][0][0][ 2][ 8] =   7.739985e-01; eval[3][0][0][ 2][ 8] =   1.112536e-03;
  val[3][0][0][ 2][ 9] =   7.556405e-01; eval[3][0][0][ 2][ 9] =   1.360062e-03;
  val[3][0][0][ 2][10] =   7.417765e-01; eval[3][0][0][ 2][10] =   1.699662e-03;
  val[3][0][0][ 2][11] =   7.344081e-01; eval[3][0][0][ 2][11] =   2.161795e-03;
  val[3][0][0][ 2][12] =   7.260512e-01; eval[3][0][0][ 2][12] =   2.741819e-03;
  val[3][0][0][ 2][13] =   7.162392e-01; eval[3][0][0][ 2][13] =   3.425475e-03;
  val[3][0][0][ 2][14] =   7.162240e-01; eval[3][0][0][ 2][14] =   4.427224e-03;
  val[3][0][0][ 2][15] =   7.240016e-01; eval[3][0][0][ 2][15] =   5.809710e-03;
  val[3][0][0][ 2][16] =   7.108697e-01; eval[3][0][0][ 2][16] =   6.932399e-03;
  val[3][0][0][ 2][17] =   7.086266e-01; eval[3][0][0][ 2][17] =   8.618112e-03;
  val[3][0][0][ 2][18] =   7.184605e-01; eval[3][0][0][ 2][18] =   1.090924e-02;
  val[3][0][0][ 2][19] =   7.123197e-01; eval[3][0][0][ 2][19] =   1.274753e-02;
  val[3][0][0][ 2][20] =   7.058006e-01; eval[3][0][0][ 2][20] =   1.185225e-02;
  val[3][0][0][ 2][21] =   7.018254e-01; eval[3][0][0][ 2][21] =   1.619534e-02;
  val[3][0][0][ 2][22] =   7.002845e-01; eval[3][0][0][ 2][22] =   2.248758e-02;
  val[3][0][0][ 2][23] =   6.754396e-01; eval[3][0][0][ 2][23] =   2.649450e-02;
  val[3][0][0][ 2][24] =   7.457219e-01; eval[3][0][0][ 2][24] =   3.463377e-02;
  val[3][0][0][ 2][25] =   5.945971e-01; eval[3][0][0][ 2][25] =   4.575370e-02;
  val[3][0][0][ 2][26] =   6.298538e-01; eval[3][0][0][ 2][26] =   4.379276e-02;
  val[3][0][0][ 2][27] =   5.041546e-01; eval[3][0][0][ 2][27] =   9.068728e-02;
  val[3][0][0][ 2][28] =   5.449432e-01; eval[3][0][0][ 2][28] =   7.328988e-02;
  val[3][0][0][ 2][29] =   6.510462e-01; eval[3][0][0][ 2][29] =   7.497813e-02;
  //pc3dz_sigma_c0d0z3
  n_val[3][0][0][3] = 30;
  val[3][0][0][ 3][ 0] =   0.000000e+00; eval[3][0][0][ 3][ 0] =   0.000000e+00;
  val[3][0][0][ 3][ 1] =   0.000000e+00; eval[3][0][0][ 3][ 1] =   0.000000e+00;
  val[3][0][0][ 3][ 2] =   1.481532e+00; eval[3][0][0][ 3][ 2] =   5.751951e-04;
  val[3][0][0][ 3][ 3] =   1.154783e+00; eval[3][0][0][ 3][ 3] =   4.821488e-04;
  val[3][0][0][ 3][ 4] =   9.960204e-01; eval[3][0][0][ 3][ 4] =   5.348204e-04;
  val[3][0][0][ 3][ 5] =   9.082243e-01; eval[3][0][0][ 3][ 5] =   5.429949e-04;
  val[3][0][0][ 3][ 6] =   8.396183e-01; eval[3][0][0][ 3][ 6] =   8.020494e-04;
  val[3][0][0][ 3][ 7] =   7.964521e-01; eval[3][0][0][ 3][ 7] =   8.995828e-04;
  val[3][0][0][ 3][ 8] =   7.683631e-01; eval[3][0][0][ 3][ 8] =   1.042859e-03;
  val[3][0][0][ 3][ 9] =   7.497691e-01; eval[3][0][0][ 3][ 9] =   1.290482e-03;
  val[3][0][0][ 3][10] =   7.381571e-01; eval[3][0][0][ 3][10] =   1.631881e-03;
  val[3][0][0][ 3][11] =   7.311346e-01; eval[3][0][0][ 3][11] =   2.087152e-03;
  val[3][0][0][ 3][12] =   7.156182e-01; eval[3][0][0][ 3][12] =   2.581825e-03;
  val[3][0][0][ 3][13] =   7.173044e-01; eval[3][0][0][ 3][13] =   3.384524e-03;
  val[3][0][0][ 3][14] =   7.174831e-01; eval[3][0][0][ 3][14] =   4.384773e-03;
  val[3][0][0][ 3][15] =   7.163284e-01; eval[3][0][0][ 3][15] =   5.519582e-03;
  val[3][0][0][ 3][16] =   7.235044e-01; eval[3][0][0][ 3][16] =   7.081884e-03;
  val[3][0][0][ 3][17] =   7.164968e-01; eval[3][0][0][ 3][17] =   8.590338e-03;
  val[3][0][0][ 3][18] =   6.902895e-01; eval[3][0][0][ 3][18] =   9.639414e-03;
  val[3][0][0][ 3][19] =   6.828709e-01; eval[3][0][0][ 3][19] =   1.843833e-02;
  val[3][0][0][ 3][20] =   6.792456e-01; eval[3][0][0][ 3][20] =   1.688994e-02;
  val[3][0][0][ 3][21] =   6.808560e-01; eval[3][0][0][ 3][21] =   2.395246e-02;
  val[3][0][0][ 3][22] =   7.086427e-01; eval[3][0][0][ 3][22] =   2.211271e-02;
  val[3][0][0][ 3][23] =   7.180004e-01; eval[3][0][0][ 3][23] =   3.006006e-02;
  val[3][0][0][ 3][24] =   6.811810e-01; eval[3][0][0][ 3][24] =   3.492114e-02;
  val[3][0][0][ 3][25] =   7.703629e-01; eval[3][0][0][ 3][25] =   4.431159e-02;
  val[3][0][0][ 3][26] =   7.294376e-01; eval[3][0][0][ 3][26] =   6.233322e-02;
  val[3][0][0][ 3][27] =   8.592146e-01; eval[3][0][0][ 3][27] =   1.213290e-01;
  val[3][0][0][ 3][28] =   3.564417e-01; eval[3][0][0][ 3][28] =   3.843030e-02;
  val[3][0][0][ 3][29] =   4.877480e-01; eval[3][0][0][ 3][29] =   1.133368e-01;
  //pc3dz_sigma_c0d0z4
  n_val[3][0][0][4] = 30;
  val[3][0][0][ 4][ 0] =   0.000000e+00; eval[3][0][0][ 4][ 0] =   0.000000e+00;
  val[3][0][0][ 4][ 1] =   0.000000e+00; eval[3][0][0][ 4][ 1] =   0.000000e+00;
  val[3][0][0][ 4][ 2] =   1.440818e+00; eval[3][0][0][ 4][ 2] =   6.364010e-04;
  val[3][0][0][ 4][ 3] =   1.123392e+00; eval[3][0][0][ 4][ 3] =   5.319954e-04;
  val[3][0][0][ 4][ 4] =   9.906757e-01; eval[3][0][0][ 4][ 4] =   6.140994e-04;
  val[3][0][0][ 4][ 5] =   8.715096e-01; eval[3][0][0][ 4][ 5] =   8.119736e-04;
  val[3][0][0][ 4][ 6] =   8.210209e-01; eval[3][0][0][ 4][ 6] =   8.738337e-04;
  val[3][0][0][ 4][ 7] =   7.871395e-01; eval[3][0][0][ 4][ 7] =   1.020858e-03;
  val[3][0][0][ 4][ 8] =   7.643646e-01; eval[3][0][0][ 4][ 8] =   1.215284e-03;
  val[3][0][0][ 4][ 9] =   7.447758e-01; eval[3][0][0][ 4][ 9] =   1.505949e-03;
  val[3][0][0][ 4][10] =   7.397945e-01; eval[3][0][0][ 4][10] =   1.944000e-03;
  val[3][0][0][ 4][11] =   7.290011e-01; eval[3][0][0][ 4][11] =   2.470107e-03;
  val[3][0][0][ 4][12] =   7.300955e-01; eval[3][0][0][ 4][12] =   3.250345e-03;
  val[3][0][0][ 4][13] =   7.198183e-01; eval[3][0][0][ 4][13] =   4.061126e-03;
  val[3][0][0][ 4][14] =   7.122958e-01; eval[3][0][0][ 4][14] =   5.078899e-03;
  val[3][0][0][ 4][15] =   7.131588e-01; eval[3][0][0][ 4][15] =   1.036249e-02;
  val[3][0][0][ 4][16] =   7.134273e-01; eval[3][0][0][ 4][16] =   1.289306e-02;
  val[3][0][0][ 4][17] =   7.094724e-01; eval[3][0][0][ 4][17] =   9.837684e-03;
  val[3][0][0][ 4][18] =   7.429378e-01; eval[3][0][0][ 4][18] =   1.340254e-02;
  val[3][0][0][ 4][19] =   7.222009e-01; eval[3][0][0][ 4][19] =   2.482750e-02;
  val[3][0][0][ 4][20] =   6.990222e-01; eval[3][0][0][ 4][20] =   2.138012e-02;
  val[3][0][0][ 4][21] =   7.080477e-01; eval[3][0][0][ 4][21] =   1.927605e-02;
  val[3][0][0][ 4][22] =   6.427083e-01; eval[3][0][0][ 4][22] =   3.156537e-02;
  val[3][0][0][ 4][23] =   7.586053e-01; eval[3][0][0][ 4][23] =   4.219638e-02;
  val[3][0][0][ 4][24] =   7.343798e-01; eval[3][0][0][ 4][24] =   4.793918e-02;
  val[3][0][0][ 4][25] =   6.372308e-01; eval[3][0][0][ 4][25] =   6.628793e-02;
  val[3][0][0][ 4][26] =   7.650857e-01; eval[3][0][0][ 4][26] =   5.910553e-02;
  val[3][0][0][ 4][27] =   6.407742e-01; eval[3][0][0][ 4][27] =   6.406858e-02;
  val[3][0][0][ 4][28] =   6.983102e-01; eval[3][0][0][ 4][28] =   7.704218e-02;
  val[3][0][0][ 4][29] =   7.061502e-01; eval[3][0][0][ 4][29] =   1.036182e-01;
  //pc3dz_sigma_c0d0z5
  n_val[3][0][0][5] = 30;
  val[3][0][0][ 5][ 0] =   0.000000e+00; eval[3][0][0][ 5][ 0] =   0.000000e+00;
  val[3][0][0][ 5][ 1] =   0.000000e+00; eval[3][0][0][ 5][ 1] =   0.000000e+00;
  val[3][0][0][ 5][ 2] =   1.481318e+00; eval[3][0][0][ 5][ 2] =   7.415994e-04;
  val[3][0][0][ 5][ 3] =   1.162989e+00; eval[3][0][0][ 5][ 3] =   6.512551e-04;
  val[3][0][0][ 5][ 4] =   1.037484e+00; eval[3][0][0][ 5][ 4] =   5.805277e-04;
  val[3][0][0][ 5][ 5] =   9.100530e-01; eval[3][0][0][ 5][ 5] =   1.078006e-03;
  val[3][0][0][ 5][ 6] =   8.549727e-01; eval[3][0][0][ 5][ 6] =   1.171541e-03;
  val[3][0][0][ 5][ 7] =   8.173842e-01; eval[3][0][0][ 5][ 7] =   1.339362e-03;
  val[3][0][0][ 5][ 8] =   7.959982e-01; eval[3][0][0][ 5][ 8] =   1.606665e-03;
  val[3][0][0][ 5][ 9] =   7.721568e-01; eval[3][0][0][ 5][ 9] =   1.976533e-03;
  val[3][0][0][ 5][10] =   7.647915e-01; eval[3][0][0][ 5][10] =   2.551275e-03;
  val[3][0][0][ 5][11] =   7.526575e-01; eval[3][0][0][ 5][11] =   3.211838e-03;
  val[3][0][0][ 5][12] =   7.479835e-01; eval[3][0][0][ 5][12] =   4.125461e-03;
  val[3][0][0][ 5][13] =   7.473852e-01; eval[3][0][0][ 5][13] =   5.341942e-03;
  val[3][0][0][ 5][14] =   7.604608e-01; eval[3][0][0][ 5][14] =   7.223437e-03;
  val[3][0][0][ 5][15] =   6.953648e-01; eval[3][0][0][ 5][15] =   1.122416e-02;
  val[3][0][0][ 5][16] =   7.151812e-01; eval[3][0][0][ 5][16] =   1.531233e-02;
  val[3][0][0][ 5][17] =   7.454966e-01; eval[3][0][0][ 5][17] =   1.324567e-02;
  val[3][0][0][ 5][18] =   7.586339e-01; eval[3][0][0][ 5][18] =   1.717890e-02;
  val[3][0][0][ 5][19] =   7.085714e-01; eval[3][0][0][ 5][19] =   2.740376e-02;
  val[3][0][0][ 5][20] =   7.451630e-01; eval[3][0][0][ 5][20] =   1.804010e-02;
  val[3][0][0][ 5][21] =   7.202288e-01; eval[3][0][0][ 5][21] =   3.819026e-02;
  val[3][0][0][ 5][22] =   7.269847e-01; eval[3][0][0][ 5][22] =   2.576850e-02;
  val[3][0][0][ 5][23] =   7.403309e-01; eval[3][0][0][ 5][23] =   4.490366e-02;
  val[3][0][0][ 5][24] =   6.912864e-01; eval[3][0][0][ 5][24] =   4.791896e-02;
  val[3][0][0][ 5][25] =   7.567370e-01; eval[3][0][0][ 5][25] =   5.121552e-02;
  val[3][0][0][ 5][26] =   7.030212e-01; eval[3][0][0][ 5][26] =   4.580093e-02;
  val[3][0][0][ 5][27] =   5.505087e-01; eval[3][0][0][ 5][27] =   7.234129e-02;
  val[3][0][0][ 5][28] =   9.251784e-01; eval[3][0][0][ 5][28] =   1.745378e-01;
  val[3][0][0][ 5][29] =   9.120060e-01; eval[3][0][0][ 5][29] =   1.843121e-01;
  //pc3dz_sigma_c0d0z6
  n_val[3][0][0][6] = 30;
  val[3][0][0][ 6][ 0] =   0.000000e+00; eval[3][0][0][ 6][ 0] =   0.000000e+00;
  val[3][0][0][ 6][ 1] =   0.000000e+00; eval[3][0][0][ 6][ 1] =   0.000000e+00;
  val[3][0][0][ 6][ 2] =   1.491056e+00; eval[3][0][0][ 6][ 2] =   6.320365e-04;
  val[3][0][0][ 6][ 3] =   1.161698e+00; eval[3][0][0][ 6][ 3] =   5.326399e-04;
  val[3][0][0][ 6][ 4] =   1.014466e+00; eval[3][0][0][ 6][ 4] =   6.108878e-04;
  val[3][0][0][ 6][ 5] =   9.341881e-01; eval[3][0][0][ 6][ 5] =   6.670052e-04;
  val[3][0][0][ 6][ 6] =   8.480172e-01; eval[3][0][0][ 6][ 6] =   9.383423e-04;
  val[3][0][0][ 6][ 7] =   8.142326e-01; eval[3][0][0][ 6][ 7] =   1.083565e-03;
  val[3][0][0][ 6][ 8] =   7.875994e-01; eval[3][0][0][ 6][ 8] =   1.257976e-03;
  val[3][0][0][ 6][ 9] =   7.691876e-01; eval[3][0][0][ 6][ 9] =   1.573878e-03;
  val[3][0][0][ 6][10] =   7.562295e-01; eval[3][0][0][ 6][10] =   1.990184e-03;
  val[3][0][0][ 6][11] =   7.547791e-01; eval[3][0][0][ 6][11] =   2.615453e-03;
  val[3][0][0][ 6][12] =   7.442745e-01; eval[3][0][0][ 6][12] =   3.305529e-03;
  val[3][0][0][ 6][13] =   7.090011e-01; eval[3][0][0][ 6][13] =   6.073043e-03;
  val[3][0][0][ 6][14] =   7.091148e-01; eval[3][0][0][ 6][14] =   7.792223e-03;
  val[3][0][0][ 6][15] =   7.458299e-01; eval[3][0][0][ 6][15] =   7.047929e-03;
  val[3][0][0][ 6][16] =   7.352176e-01; eval[3][0][0][ 6][16] =   8.443225e-03;
  val[3][0][0][ 6][17] =   6.876323e-01; eval[3][0][0][ 6][17] =   1.400780e-02;
  val[3][0][0][ 6][18] =   6.952940e-01; eval[3][0][0][ 6][18] =   1.796466e-02;
  val[3][0][0][ 6][19] =   7.543807e-01; eval[3][0][0][ 6][19] =   1.720544e-02;
  val[3][0][0][ 6][20] =   7.166731e-01; eval[3][0][0][ 6][20] =   2.201053e-02;
  val[3][0][0][ 6][21] =   6.979556e-01; eval[3][0][0][ 6][21] =   2.799847e-02;
  val[3][0][0][ 6][22] =   7.493808e-01; eval[3][0][0][ 6][22] =   2.931657e-02;
  val[3][0][0][ 6][23] =   7.321294e-01; eval[3][0][0][ 6][23] =   3.612227e-02;
  val[3][0][0][ 6][24] =   7.151324e-01; eval[3][0][0][ 6][24] =   4.248162e-02;
  val[3][0][0][ 6][25] =   6.576144e-01; eval[3][0][0][ 6][25] =   4.473977e-02;
  val[3][0][0][ 6][26] =   6.446570e-01; eval[3][0][0][ 6][26] =   7.818601e-02;
  val[3][0][0][ 6][27] =   4.780006e-01; eval[3][0][0][ 6][27] =   7.971120e-02;
  val[3][0][0][ 6][28] =   8.250099e-01; eval[3][0][0][ 6][28] =   6.824489e-02;
  val[3][0][0][ 6][29] =   7.976816e-01; eval[3][0][0][ 6][29] =   8.462924e-02;
  //pc3dz_sigma_c0d0z7
  n_val[3][0][0][7] = 30;
  val[3][0][0][ 7][ 0] =   0.000000e+00; eval[3][0][0][ 7][ 0] =   0.000000e+00;
  val[3][0][0][ 7][ 1] =   0.000000e+00; eval[3][0][0][ 7][ 1] =   0.000000e+00;
  val[3][0][0][ 7][ 2] =   1.484635e+00; eval[3][0][0][ 7][ 2] =   6.138146e-04;
  val[3][0][0][ 7][ 3] =   1.157954e+00; eval[3][0][0][ 7][ 3] =   5.260592e-04;
  val[3][0][0][ 7][ 4] =   9.972261e-01; eval[3][0][0][ 7][ 4] =   5.781043e-04;
  val[3][0][0][ 7][ 5] =   9.200088e-01; eval[3][0][0][ 7][ 5] =   6.412554e-04;
  val[3][0][0][ 7][ 6] =   8.458554e-01; eval[3][0][0][ 7][ 6] =   9.646465e-04;
  val[3][0][0][ 7][ 7] =   8.088987e-01; eval[3][0][0][ 7][ 7] =   1.095086e-03;
  val[3][0][0][ 7][ 8] =   7.818440e-01; eval[3][0][0][ 7][ 8] =   1.257065e-03;
  val[3][0][0][ 7][ 9] =   7.675710e-01; eval[3][0][0][ 7][ 9] =   1.583994e-03;
  val[3][0][0][ 7][10] =   7.542316e-01; eval[3][0][0][ 7][10] =   1.999553e-03;
  val[3][0][0][ 7][11] =   7.475401e-01; eval[3][0][0][ 7][11] =   2.571769e-03;
  val[3][0][0][ 7][12] =   7.416421e-01; eval[3][0][0][ 7][12] =   3.308987e-03;
  val[3][0][0][ 7][13] =   6.981859e-01; eval[3][0][0][ 7][13] =   5.804083e-03;
  val[3][0][0][ 7][14] =   7.191262e-01; eval[3][0][0][ 7][14] =   8.114345e-03;
  val[3][0][0][ 7][15] =   7.143738e-01; eval[3][0][0][ 7][15] =   9.968696e-03;
  val[3][0][0][ 7][16] =   6.900087e-01; eval[3][0][0][ 7][16] =   1.127667e-02;
  val[3][0][0][ 7][17] =   6.971340e-01; eval[3][0][0][ 7][17] =   1.462442e-02;
  val[3][0][0][ 7][18] =   7.231184e-01; eval[3][0][0][ 7][18] =   2.001974e-02;
  val[3][0][0][ 7][19] =   6.961071e-01; eval[3][0][0][ 7][19] =   2.155646e-02;
  val[3][0][0][ 7][20] =   6.935240e-01; eval[3][0][0][ 7][20] =   1.964142e-02;
  val[3][0][0][ 7][21] =   6.821448e-01; eval[3][0][0][ 7][21] =   2.618659e-02;
  val[3][0][0][ 7][22] =   7.614471e-01; eval[3][0][0][ 7][22] =   3.034814e-02;
  val[3][0][0][ 7][23] =   6.722575e-01; eval[3][0][0][ 7][23] =   2.922020e-02;
  val[3][0][0][ 7][24] =   4.557417e-01; eval[3][0][0][ 7][24] =   3.875564e-02;
  val[3][0][0][ 7][25] =   7.227109e-01; eval[3][0][0][ 7][25] =   5.685649e-02;
  val[3][0][0][ 7][26] =   6.577371e-01; eval[3][0][0][ 7][26] =   5.546540e-02;
  val[3][0][0][ 7][27] =   7.403713e-01; eval[3][0][0][ 7][27] =   5.927345e-02;
  val[3][0][0][ 7][28] =   5.544590e-01; eval[3][0][0][ 7][28] =   8.569536e-02;
  val[3][0][0][ 7][29] =   5.975091e-01; eval[3][0][0][ 7][29] =   7.007839e-02;
  //pc3dz_sigma_c0d0z8
  n_val[3][0][0][8] = 30;
  val[3][0][0][ 8][ 0] =   0.000000e+00; eval[3][0][0][ 8][ 0] =   0.000000e+00;
  val[3][0][0][ 8][ 1] =   0.000000e+00; eval[3][0][0][ 8][ 1] =   0.000000e+00;
  val[3][0][0][ 8][ 2] =   1.464425e+00; eval[3][0][0][ 8][ 2] =   5.917625e-04;
  val[3][0][0][ 8][ 3] =   1.147947e+00; eval[3][0][0][ 8][ 3] =   5.230753e-04;
  val[3][0][0][ 8][ 4] =   9.898645e-01; eval[3][0][0][ 8][ 4] =   5.736554e-04;
  val[3][0][0][ 8][ 5] =   9.173993e-01; eval[3][0][0][ 8][ 5] =   6.429742e-04;
  val[3][0][0][ 8][ 6] =   8.639939e-01; eval[3][0][0][ 8][ 6] =   7.117405e-04;
  val[3][0][0][ 8][ 7] =   8.071553e-01; eval[3][0][0][ 8][ 7] =   1.079783e-03;
  val[3][0][0][ 8][ 8] =   7.838648e-01; eval[3][0][0][ 8][ 8] =   1.247185e-03;
  val[3][0][0][ 8][ 9] =   7.713400e-01; eval[3][0][0][ 8][ 9] =   1.595291e-03;
  val[3][0][0][ 8][10] =   7.616016e-01; eval[3][0][0][ 8][10] =   2.041963e-03;
  val[3][0][0][ 8][11] =   7.306506e-01; eval[3][0][0][ 8][11] =   3.820437e-03;
  val[3][0][0][ 8][12] =   7.208585e-01; eval[3][0][0][ 8][12] =   4.809010e-03;
  val[3][0][0][ 8][13] =   7.119894e-01; eval[3][0][0][ 8][13] =   6.018049e-03;
  val[3][0][0][ 8][14] =   7.202234e-01; eval[3][0][0][ 8][14] =   7.911257e-03;
  val[3][0][0][ 8][15] =   7.123741e-01; eval[3][0][0][ 8][15] =   9.718124e-03;
  val[3][0][0][ 8][16] =   7.028058e-01; eval[3][0][0][ 8][16] =   1.155771e-02;
  val[3][0][0][ 8][17] =   6.886953e-01; eval[3][0][0][ 8][17] =   1.373484e-02;
  val[3][0][0][ 8][18] =   6.954606e-01; eval[3][0][0][ 8][18] =   1.743119e-02;
  val[3][0][0][ 8][19] =   6.966766e-01; eval[3][0][0][ 8][19] =   2.101086e-02;
  val[3][0][0][ 8][20] =   6.940273e-01; eval[3][0][0][ 8][20] =   1.918224e-02;
  val[3][0][0][ 8][21] =   7.137723e-01; eval[3][0][0][ 8][21] =   2.913775e-02;
  val[3][0][0][ 8][22] =   7.444902e-01; eval[3][0][0][ 8][22] =   2.744496e-02;
  val[3][0][0][ 8][23] =   7.671042e-01; eval[3][0][0][ 8][23] =   2.847283e-02;
  val[3][0][0][ 8][24] =   7.654513e-01; eval[3][0][0][ 8][24] =   3.703658e-02;
  val[3][0][0][ 8][25] =   6.521387e-01; eval[3][0][0][ 8][25] =   6.591093e-02;
  val[3][0][0][ 8][26] =   5.503878e-01; eval[3][0][0][ 8][26] =   5.264168e-02;
  val[3][0][0][ 8][27] =   7.591785e-01; eval[3][0][0][ 8][27] =   9.497270e-02;
  val[3][0][0][ 8][28] =   8.245349e-01; eval[3][0][0][ 8][28] =   6.480660e-02;
  val[3][0][0][ 8][29] =   6.802729e-01; eval[3][0][0][ 8][29] =   6.490806e-02;
  //pc3dz_sigma_c0d0z9
  n_val[3][0][0][9] = 30;
  val[3][0][0][ 9][ 0] =   0.000000e+00; eval[3][0][0][ 9][ 0] =   0.000000e+00;
  val[3][0][0][ 9][ 1] =   0.000000e+00; eval[3][0][0][ 9][ 1] =   0.000000e+00;
  val[3][0][0][ 9][ 2] =   1.483835e+00; eval[3][0][0][ 9][ 2] =   6.317992e-04;
  val[3][0][0][ 9][ 3] =   1.149309e+00; eval[3][0][0][ 9][ 3] =   5.444278e-04;
  val[3][0][0][ 9][ 4] =   9.932901e-01; eval[3][0][0][ 9][ 4] =   5.858362e-04;
  val[3][0][0][ 9][ 5] =   9.163319e-01; eval[3][0][0][ 9][ 5] =   6.357838e-04;
  val[3][0][0][ 9][ 6] =   8.678505e-01; eval[3][0][0][ 9][ 6] =   7.142192e-04;
  val[3][0][0][ 9][ 7] =   8.095591e-01; eval[3][0][0][ 9][ 7] =   1.082447e-03;
  val[3][0][0][ 9][ 8] =   7.909221e-01; eval[3][0][0][ 9][ 8] =   1.274370e-03;
  val[3][0][0][ 9][ 9] =   7.528222e-01; eval[3][0][0][ 9][ 9] =   2.401026e-03;
  val[3][0][0][ 9][10] =   7.351075e-01; eval[3][0][0][ 9][10] =   2.961793e-03;
  val[3][0][0][ 9][11] =   7.309790e-01; eval[3][0][0][ 9][11] =   3.859291e-03;
  val[3][0][0][ 9][12] =   7.259530e-01; eval[3][0][0][ 9][12] =   5.005689e-03;
  val[3][0][0][ 9][13] =   7.279267e-01; eval[3][0][0][ 9][13] =   6.568798e-03;
  val[3][0][0][ 9][14] =   7.347022e-01; eval[3][0][0][ 9][14] =   5.406778e-03;
  val[3][0][0][ 9][15] =   7.090735e-01; eval[3][0][0][ 9][15] =   9.975706e-03;
  val[3][0][0][ 9][16] =   7.188610e-01; eval[3][0][0][ 9][16] =   8.043708e-03;
  val[3][0][0][ 9][17] =   7.229275e-01; eval[3][0][0][ 9][17] =   1.020783e-02;
  val[3][0][0][ 9][18] =   7.452876e-01; eval[3][0][0][ 9][18] =   1.372772e-02;
  val[3][0][0][ 9][19] =   7.113832e-01; eval[3][0][0][ 9][19] =   1.436371e-02;
  val[3][0][0][ 9][20] =   7.086549e-01; eval[3][0][0][ 9][20] =   1.292292e-02;
  val[3][0][0][ 9][21] =   7.307172e-01; eval[3][0][0][ 9][21] =   1.998043e-02;
  val[3][0][0][ 9][22] =   7.095838e-01; eval[3][0][0][ 9][22] =   2.490093e-02;
  val[3][0][0][ 9][23] =   7.300354e-01; eval[3][0][0][ 9][23] =   3.502721e-02;
  val[3][0][0][ 9][24] =   7.480096e-01; eval[3][0][0][ 9][24] =   4.954733e-02;
  val[3][0][0][ 9][25] =   7.238558e-01; eval[3][0][0][ 9][25] =   5.381316e-02;
  val[3][0][0][ 9][26] =   6.870831e-01; eval[3][0][0][ 9][26] =   6.456163e-02;
  val[3][0][0][ 9][27] =   7.893981e-01; eval[3][0][0][ 9][27] =   3.901910e-02;
  val[3][0][0][ 9][28] =   6.496406e-01; eval[3][0][0][ 9][28] =   7.377966e-02;
  val[3][0][0][ 9][29] =   5.138737e-01; eval[3][0][0][ 9][29] =   1.126181e-01;
  //pc3dz_sigma_c0d1z0
  n_val[3][0][1][0] = 30;
  val[3][0][1][ 0][ 0] =   0.000000e+00; eval[3][0][1][ 0][ 0] =   0.000000e+00;
  val[3][0][1][ 0][ 1] =   0.000000e+00; eval[3][0][1][ 0][ 1] =   0.000000e+00;
  val[3][0][1][ 0][ 2] =   1.415868e+00; eval[3][0][1][ 0][ 2] =   6.877298e-04;
  val[3][0][1][ 0][ 3] =   1.162702e+00; eval[3][0][1][ 0][ 3] =   5.979901e-04;
  val[3][0][1][ 0][ 4] =   9.988995e-01; eval[3][0][1][ 0][ 4] =   6.567021e-04;
  val[3][0][1][ 0][ 5] =   9.233272e-01; eval[3][0][1][ 0][ 5] =   6.676387e-04;
  val[3][0][1][ 0][ 6] =   8.663364e-01; eval[3][0][1][ 0][ 6] =   1.039336e-03;
  val[3][0][1][ 0][ 7] =   8.322690e-01; eval[3][0][1][ 0][ 7] =   1.214862e-03;
  val[3][0][1][ 0][ 8] =   8.224898e-01; eval[3][0][1][ 0][ 8] =   1.471445e-03;
  val[3][0][1][ 0][ 9] =   7.988998e-01; eval[3][0][1][ 0][ 9] =   1.793186e-03;
  val[3][0][1][ 0][10] =   7.872910e-01; eval[3][0][1][ 0][10] =   2.301767e-03;
  val[3][0][1][ 0][11] =   7.819791e-01; eval[3][0][1][ 0][11] =   2.993313e-03;
  val[3][0][1][ 0][12] =   7.719570e-01; eval[3][0][1][ 0][12] =   3.830560e-03;
  val[3][0][1][ 0][13] =   7.632170e-01; eval[3][0][1][ 0][13] =   4.875572e-03;
  val[3][0][1][ 0][14] =   7.705605e-01; eval[3][0][1][ 0][14] =   6.503982e-03;
  val[3][0][1][ 0][15] =   7.593966e-01; eval[3][0][1][ 0][15] =   8.102508e-03;
  val[3][0][1][ 0][16] =   7.641640e-01; eval[3][0][1][ 0][16] =   1.050133e-02;
  val[3][0][1][ 0][17] =   7.665010e-01; eval[3][0][1][ 0][17] =   1.326706e-02;
  val[3][0][1][ 0][18] =   7.454173e-01; eval[3][0][1][ 0][18] =   2.453178e-02;
  val[3][0][1][ 0][19] =   7.519558e-01; eval[3][0][1][ 0][19] =   1.884780e-02;
  val[3][0][1][ 0][20] =   7.373497e-01; eval[3][0][1][ 0][20] =   1.719965e-02;
  val[3][0][1][ 0][21] =   7.934480e-01; eval[3][0][1][ 0][21] =   2.904782e-02;
  val[3][0][1][ 0][22] =   7.674224e-01; eval[3][0][1][ 0][22] =   3.748469e-02;
  val[3][0][1][ 0][23] =   6.975642e-01; eval[3][0][1][ 0][23] =   3.836820e-02;
  val[3][0][1][ 0][24] =   6.498028e-01; eval[3][0][1][ 0][24] =   4.125602e-02;
  val[3][0][1][ 0][25] =   7.290640e-01; eval[3][0][1][ 0][25] =   7.073756e-02;
  val[3][0][1][ 0][26] =   6.334075e-01; eval[3][0][1][ 0][26] =   9.927821e-02;
  val[3][0][1][ 0][27] =   7.705107e-01; eval[3][0][1][ 0][27] =   1.199247e-01;
  val[3][0][1][ 0][28] =   7.704398e-01; eval[3][0][1][ 0][28] =   8.024484e-02;
  val[3][0][1][ 0][29] =   8.314838e-01; eval[3][0][1][ 0][29] =   1.501073e-01;
  //pc3dz_sigma_c0d1z1
  n_val[3][0][1][1] = 30;
  val[3][0][1][ 1][ 0] =   0.000000e+00; eval[3][0][1][ 1][ 0] =   0.000000e+00;
  val[3][0][1][ 1][ 1] =   0.000000e+00; eval[3][0][1][ 1][ 1] =   0.000000e+00;
  val[3][0][1][ 1][ 2] =   1.393775e+00; eval[3][0][1][ 1][ 2] =   5.887855e-04;
  val[3][0][1][ 1][ 3] =   1.147048e+00; eval[3][0][1][ 1][ 3] =   5.247373e-04;
  val[3][0][1][ 1][ 4] =   9.938051e-01; eval[3][0][1][ 1][ 4] =   5.993466e-04;
  val[3][0][1][ 1][ 5] =   9.018860e-01; eval[3][0][1][ 1][ 5] =   8.477102e-04;
  val[3][0][1][ 1][ 6] =   8.518875e-01; eval[3][0][1][ 1][ 6] =   8.701956e-04;
  val[3][0][1][ 1][ 7] =   8.226792e-01; eval[3][0][1][ 1][ 7] =   1.076171e-03;
  val[3][0][1][ 1][ 8] =   8.030298e-01; eval[3][0][1][ 1][ 8] =   1.314891e-03;
  val[3][0][1][ 1][ 9] =   7.678364e-01; eval[3][0][1][ 1][ 9] =   2.494186e-03;
  val[3][0][1][ 1][10] =   7.854254e-01; eval[3][0][1][ 1][10] =   2.201111e-03;
  val[3][0][1][ 1][11] =   7.760545e-01; eval[3][0][1][ 1][11] =   2.814297e-03;
  val[3][0][1][ 1][12] =   7.679422e-01; eval[3][0][1][ 1][12] =   3.580618e-03;
  val[3][0][1][ 1][13] =   7.634054e-01; eval[3][0][1][ 1][13] =   4.638625e-03;
  val[3][0][1][ 1][14] =   7.588123e-01; eval[3][0][1][ 1][14] =   5.838418e-03;
  val[3][0][1][ 1][15] =   7.415732e-01; eval[3][0][1][ 1][15] =   7.037622e-03;
  val[3][0][1][ 1][16] =   7.621037e-01; eval[3][0][1][ 1][16] =   9.550354e-03;
  val[3][0][1][ 1][17] =   7.510392e-01; eval[3][0][1][ 1][17] =   1.159124e-02;
  val[3][0][1][ 1][18] =   7.379521e-01; eval[3][0][1][ 1][18] =   1.358318e-02;
  val[3][0][1][ 1][19] =   7.648385e-01; eval[3][0][1][ 1][19] =   1.853311e-02;
  val[3][0][1][ 1][20] =   7.627778e-01; eval[3][0][1][ 1][20] =   1.718799e-02;
  val[3][0][1][ 1][21] =   7.499678e-01; eval[3][0][1][ 1][21] =   2.283136e-02;
  val[3][0][1][ 1][22] =   7.482367e-01; eval[3][0][1][ 1][22] =   3.216683e-02;
  val[3][0][1][ 1][23] =   7.444831e-01; eval[3][0][1][ 1][23] =   4.325718e-02;
  val[3][0][1][ 1][24] =   7.827009e-01; eval[3][0][1][ 1][24] =   3.333541e-02;
  val[3][0][1][ 1][25] =   8.317829e-01; eval[3][0][1][ 1][25] =   6.802920e-02;
  val[3][0][1][ 1][26] =   4.971077e-01; eval[3][0][1][ 1][26] =   8.514783e-02;
  val[3][0][1][ 1][27] =   6.090662e-01; eval[3][0][1][ 1][27] =   9.731884e-02;
  val[3][0][1][ 1][28] =   9.173659e-01; eval[3][0][1][ 1][28] =   1.238670e-01;
  val[3][0][1][ 1][29] =   6.560177e-01; eval[3][0][1][ 1][29] =   9.131519e-02;
  //pc3dz_sigma_c0d1z2
  n_val[3][0][1][2] = 30;
  val[3][0][1][ 2][ 0] =   0.000000e+00; eval[3][0][1][ 2][ 0] =   0.000000e+00;
  val[3][0][1][ 2][ 1] =   0.000000e+00; eval[3][0][1][ 2][ 1] =   0.000000e+00;
  val[3][0][1][ 2][ 2] =   1.394237e+00; eval[3][0][1][ 2][ 2] =   5.976145e-04;
  val[3][0][1][ 2][ 3] =   1.147521e+00; eval[3][0][1][ 2][ 3] =   5.217296e-04;
  val[3][0][1][ 2][ 4] =   1.025675e+00; eval[3][0][1][ 2][ 4] =   4.820801e-04;
  val[3][0][1][ 2][ 5] =   9.099126e-01; eval[3][0][1][ 2][ 5] =   8.479926e-04;
  val[3][0][1][ 2][ 6] =   8.634875e-01; eval[3][0][1][ 2][ 6] =   9.252731e-04;
  val[3][0][1][ 2][ 7] =   8.330791e-01; eval[3][0][1][ 2][ 7] =   1.106957e-03;
  val[3][0][1][ 2][ 8] =   8.123656e-01; eval[3][0][1][ 2][ 8] =   1.328153e-03;
  val[3][0][1][ 2][ 9] =   7.817126e-01; eval[3][0][1][ 2][ 9] =   2.589464e-03;
  val[3][0][1][ 2][10] =   7.668544e-01; eval[3][0][1][ 2][10] =   3.257022e-03;
  val[3][0][1][ 2][11] =   7.635465e-01; eval[3][0][1][ 2][11] =   4.285779e-03;
  val[3][0][1][ 2][12] =   7.594063e-01; eval[3][0][1][ 2][12] =   5.604591e-03;
  val[3][0][1][ 2][13] =   7.539906e-01; eval[3][0][1][ 2][13] =   7.238692e-03;
  val[3][0][1][ 2][14] =   7.515302e-01; eval[3][0][1][ 2][14] =   9.292984e-03;
  val[3][0][1][ 2][15] =   7.333107e-01; eval[3][0][1][ 2][15] =   1.112663e-02;
  val[3][0][1][ 2][16] =   7.506816e-01; eval[3][0][1][ 2][16] =   1.505892e-02;
  val[3][0][1][ 2][17] =   7.381873e-01; eval[3][0][1][ 2][17] =   1.811851e-02;
  val[3][0][1][ 2][18] =   7.352715e-01; eval[3][0][1][ 2][18] =   2.197189e-02;
  val[3][0][1][ 2][19] =   7.278231e-01; eval[3][0][1][ 2][19] =   2.604990e-02;
  val[3][0][1][ 2][20] =   7.223603e-01; eval[3][0][1][ 2][20] =   2.426843e-02;
  val[3][0][1][ 2][21] =   7.000919e-01; eval[3][0][1][ 2][21] =   3.176115e-02;
  val[3][0][1][ 2][22] =   7.252293e-01; eval[3][0][1][ 2][22] =   4.821150e-02;
  val[3][0][1][ 2][23] =   6.048184e-01; eval[3][0][1][ 2][23] =   3.728710e-02;
  val[3][0][1][ 2][24] =   8.397643e-01; eval[3][0][1][ 2][24] =   5.495175e-02;
  val[3][0][1][ 2][25] =   7.019902e-01; eval[3][0][1][ 2][25] =   6.184725e-02;
  val[3][0][1][ 2][26] =   8.395347e-01; eval[3][0][1][ 2][26] =   8.279994e-02;
  val[3][0][1][ 2][27] =   8.915547e-01; eval[3][0][1][ 2][27] =   8.377318e-02;
  val[3][0][1][ 2][28] =   8.636101e-01; eval[3][0][1][ 2][28] =   8.451565e-02;
  val[3][0][1][ 2][29] =   5.859167e-01; eval[3][0][1][ 2][29] =   1.266350e-01;
  //pc3dz_sigma_c0d1z3
  n_val[3][0][1][3] = 30;
  val[3][0][1][ 3][ 0] =   0.000000e+00; eval[3][0][1][ 3][ 0] =   0.000000e+00;
  val[3][0][1][ 3][ 1] =   0.000000e+00; eval[3][0][1][ 3][ 1] =   0.000000e+00;
  val[3][0][1][ 3][ 2] =   1.433539e+00; eval[3][0][1][ 3][ 2] =   5.090599e-04;
  val[3][0][1][ 3][ 3] =   1.152257e+00; eval[3][0][1][ 3][ 3] =   5.272054e-04;
  val[3][0][1][ 3][ 4] =   1.036153e+00; eval[3][0][1][ 3][ 4] =   4.886443e-04;
  val[3][0][1][ 3][ 5] =   9.301180e-01; eval[3][0][1][ 3][ 5] =   9.028760e-04;
  val[3][0][1][ 3][ 6] =   8.755526e-01; eval[3][0][1][ 3][ 6] =   9.630020e-04;
  val[3][0][1][ 3][ 7] =   8.418789e-01; eval[3][0][1][ 3][ 7] =   1.135998e-03;
  val[3][0][1][ 3][ 8] =   8.162151e-01; eval[3][0][1][ 3][ 8] =   1.351692e-03;
  val[3][0][1][ 3][ 9] =   8.001702e-01; eval[3][0][1][ 3][ 9] =   1.746952e-03;
  val[3][0][1][ 3][10] =   7.867734e-01; eval[3][0][1][ 3][10] =   2.232994e-03;
  val[3][0][1][ 3][11] =   7.769651e-01; eval[3][0][1][ 3][11] =   2.889277e-03;
  val[3][0][1][ 3][12] =   7.720954e-01; eval[3][0][1][ 3][12] =   3.751952e-03;
  val[3][0][1][ 3][13] =   7.596828e-01; eval[3][0][1][ 3][13] =   4.714922e-03;
  val[3][0][1][ 3][14] =   7.648161e-01; eval[3][0][1][ 3][14] =   6.227565e-03;
  val[3][0][1][ 3][15] =   7.744298e-01; eval[3][0][1][ 3][15] =   8.152884e-03;
  val[3][0][1][ 3][16] =   7.569295e-01; eval[3][0][1][ 3][16] =   9.916260e-03;
  val[3][0][1][ 3][17] =   7.606784e-01; eval[3][0][1][ 3][17] =   1.243264e-02;
  val[3][0][1][ 3][18] =   7.200473e-01; eval[3][0][1][ 3][18] =   2.113208e-02;
  val[3][0][1][ 3][19] =   6.984871e-01; eval[3][0][1][ 3][19] =   2.378657e-02;
  val[3][0][1][ 3][20] =   7.761484e-01; eval[3][0][1][ 3][20] =   1.851308e-02;
  val[3][0][1][ 3][21] =   7.794574e-01; eval[3][0][1][ 3][21] =   2.700247e-02;
  val[3][0][1][ 3][22] =   7.240528e-01; eval[3][0][1][ 3][22] =   3.122803e-02;
  val[3][0][1][ 3][23] =   7.710888e-01; eval[3][0][1][ 3][23] =   5.047213e-02;
  val[3][0][1][ 3][24] =   6.975000e-01; eval[3][0][1][ 3][24] =   4.918480e-02;
  val[3][0][1][ 3][25] =   9.390652e-01; eval[3][0][1][ 3][25] =   1.479510e-01;
  val[3][0][1][ 3][26] =   6.787680e-01; eval[3][0][1][ 3][26] =   4.679353e-02;
  val[3][0][1][ 3][27] =   4.855159e-01; eval[3][0][1][ 3][27] =   1.010472e-01;
  val[3][0][1][ 3][28] =   6.713886e-01; eval[3][0][1][ 3][28] =   9.349558e-02;
  val[3][0][1][ 3][29] =   6.571757e-01; eval[3][0][1][ 3][29] =   1.851004e-01;
  //pc3dz_sigma_c0d1z4
  n_val[3][0][1][4] = 30;
  val[3][0][1][ 4][ 0] =   0.000000e+00; eval[3][0][1][ 4][ 0] =   0.000000e+00;
  val[3][0][1][ 4][ 1] =   0.000000e+00; eval[3][0][1][ 4][ 1] =   0.000000e+00;
  val[3][0][1][ 4][ 2] =   1.405629e+00; eval[3][0][1][ 4][ 2] =   6.031622e-04;
  val[3][0][1][ 4][ 3] =   1.125249e+00; eval[3][0][1][ 4][ 3] =   5.975632e-04;
  val[3][0][1][ 4][ 4] =   1.008467e+00; eval[3][0][1][ 4][ 4] =   7.296915e-04;
  val[3][0][1][ 4][ 5] =   9.104430e-01; eval[3][0][1][ 4][ 5] =   9.902913e-04;
  val[3][0][1][ 4][ 6] =   8.644121e-01; eval[3][0][1][ 4][ 6] =   1.072733e-03;
  val[3][0][1][ 4][ 7] =   8.331771e-01; eval[3][0][1][ 4][ 7] =   1.278168e-03;
  val[3][0][1][ 4][ 8] =   8.114884e-01; eval[3][0][1][ 4][ 8] =   1.555578e-03;
  val[3][0][1][ 4][ 9] =   7.959188e-01; eval[3][0][1][ 4][ 9] =   2.021057e-03;
  val[3][0][1][ 4][10] =   7.848723e-01; eval[3][0][1][ 4][10] =   2.612253e-03;
  val[3][0][1][ 4][11] =   7.807664e-01; eval[3][0][1][ 4][11] =   3.461414e-03;
  val[3][0][1][ 4][12] =   7.818548e-01; eval[3][0][1][ 4][12] =   4.619286e-03;
  val[3][0][1][ 4][13] =   7.663344e-01; eval[3][0][1][ 4][13] =   5.764595e-03;
  val[3][0][1][ 4][14] =   7.684630e-01; eval[3][0][1][ 4][14] =   7.567866e-03;
  val[3][0][1][ 4][15] =   7.586148e-01; eval[3][0][1][ 4][15] =   9.261991e-03;
  val[3][0][1][ 4][16] =   7.664131e-01; eval[3][0][1][ 4][16] =   1.218899e-02;
  val[3][0][1][ 4][17] =   7.785986e-01; eval[3][0][1][ 4][17] =   1.602498e-02;
  val[3][0][1][ 4][18] =   7.694385e-01; eval[3][0][1][ 4][18] =   1.902835e-02;
  val[3][0][1][ 4][19] =   7.226998e-01; eval[3][0][1][ 4][19] =   3.206907e-02;
  val[3][0][1][ 4][20] =   7.641236e-01; eval[3][0][1][ 4][20] =   2.175626e-02;
  val[3][0][1][ 4][21] =   7.146109e-01; eval[3][0][1][ 4][21] =   4.143485e-02;
  val[3][0][1][ 4][22] =   7.177182e-01; eval[3][0][1][ 4][22] =   3.695831e-02;
  val[3][0][1][ 4][23] =   7.181855e-01; eval[3][0][1][ 4][23] =   3.433214e-02;
  val[3][0][1][ 4][24] =   8.658393e-01; eval[3][0][1][ 4][24] =   5.406777e-02;
  val[3][0][1][ 4][25] =   8.152664e-01; eval[3][0][1][ 4][25] =   8.457914e-02;
  val[3][0][1][ 4][26] =   7.093321e-01; eval[3][0][1][ 4][26] =   6.012017e-02;
  val[3][0][1][ 4][27] =   2.665643e-01; eval[3][0][1][ 4][27] =   6.076422e-01;
  val[3][0][1][ 4][28] =   4.591700e-01; eval[3][0][1][ 4][28] =   1.258887e-01;
  val[3][0][1][ 4][29] =   7.256916e-01; eval[3][0][1][ 4][29] =   2.135421e-01;
  //pc3dz_sigma_c0d1z5
  n_val[3][0][1][5] = 30;
  val[3][0][1][ 5][ 0] =   0.000000e+00; eval[3][0][1][ 5][ 0] =   0.000000e+00;
  val[3][0][1][ 5][ 1] =   0.000000e+00; eval[3][0][1][ 5][ 1] =   0.000000e+00;
  val[3][0][1][ 5][ 2] =   1.413194e+00; eval[3][0][1][ 5][ 2] =   6.407490e-04;
  val[3][0][1][ 5][ 3] =   1.135690e+00; eval[3][0][1][ 5][ 3] =   6.072417e-04;
  val[3][0][1][ 5][ 4] =   1.006001e+00; eval[3][0][1][ 5][ 4] =   7.486465e-04;
  val[3][0][1][ 5][ 5] =   9.363085e-01; eval[3][0][1][ 5][ 5] =   7.682246e-04;
  val[3][0][1][ 5][ 6] =   8.654182e-01; eval[3][0][1][ 5][ 6] =   1.117564e-03;
  val[3][0][1][ 5][ 7] =   8.313583e-01; eval[3][0][1][ 5][ 7] =   1.328344e-03;
  val[3][0][1][ 5][ 8] =   8.052152e-01; eval[3][0][1][ 5][ 8] =   1.581448e-03;
  val[3][0][1][ 5][ 9] =   7.899149e-01; eval[3][0][1][ 5][ 9] =   2.010344e-03;
  val[3][0][1][ 5][10] =   7.768870e-01; eval[3][0][1][ 5][10] =   2.569376e-03;
  val[3][0][1][ 5][11] =   7.661437e-01; eval[3][0][1][ 5][11] =   3.308332e-03;
  val[3][0][1][ 5][12] =   7.557736e-01; eval[3][0][1][ 5][12] =   4.216473e-03;
  val[3][0][1][ 5][13] =   7.617138e-01; eval[3][0][1][ 5][13] =   5.645903e-03;
  val[3][0][1][ 5][14] =   7.563714e-01; eval[3][0][1][ 5][14] =   7.139756e-03;
  val[3][0][1][ 5][15] =   7.677735e-01; eval[3][0][1][ 5][15] =   9.437225e-03;
  val[3][0][1][ 5][16] =   7.685725e-01; eval[3][0][1][ 5][16] =   1.216656e-02;
  val[3][0][1][ 5][17] =   7.590916e-01; eval[3][0][1][ 5][17] =   1.458238e-02;
  val[3][0][1][ 5][18] =   7.374958e-01; eval[3][0][1][ 5][18] =   1.657605e-02;
  val[3][0][1][ 5][19] =   7.354329e-01; eval[3][0][1][ 5][19] =   2.042519e-02;
  val[3][0][1][ 5][20] =   7.384188e-01; eval[3][0][1][ 5][20] =   1.874906e-02;
  val[3][0][1][ 5][21] =   7.899852e-01; eval[3][0][1][ 5][21] =   3.222377e-02;
  val[3][0][1][ 5][22] =   7.653382e-01; eval[3][0][1][ 5][22] =   2.958157e-02;
  val[3][0][1][ 5][23] =   7.859503e-01; eval[3][0][1][ 5][23] =   5.981826e-02;
  val[3][0][1][ 5][24] =   6.542741e-01; eval[3][0][1][ 5][24] =   4.842268e-02;
  val[3][0][1][ 5][25] =   7.864285e-01; eval[3][0][1][ 5][25] =   9.490738e-02;
  val[3][0][1][ 5][26] =   5.809863e-01; eval[3][0][1][ 5][26] =   8.379303e-02;
  val[3][0][1][ 5][27] =   6.612749e-01; eval[3][0][1][ 5][27] =   6.188374e-02;
  val[3][0][1][ 5][28] =   7.343022e-01; eval[3][0][1][ 5][28] =   1.131722e-01;
  val[3][0][1][ 5][29] =   1.497382e+00; eval[3][0][1][ 5][29] =   2.906008e-01;
  //pc3dz_sigma_c0d1z6
  n_val[3][0][1][6] = 30;
  val[3][0][1][ 6][ 0] =   0.000000e+00; eval[3][0][1][ 6][ 0] =   0.000000e+00;
  val[3][0][1][ 6][ 1] =   0.000000e+00; eval[3][0][1][ 6][ 1] =   0.000000e+00;
  val[3][0][1][ 6][ 2] =   1.430174e+00; eval[3][0][1][ 6][ 2] =   5.707536e-04;
  val[3][0][1][ 6][ 3] =   1.159772e+00; eval[3][0][1][ 6][ 3] =   5.293558e-04;
  val[3][0][1][ 6][ 4] =   1.033952e+00; eval[3][0][1][ 6][ 4] =   6.621075e-04;
  val[3][0][1][ 6][ 5] =   9.652859e-01; eval[3][0][1][ 6][ 5] =   6.876321e-04;
  val[3][0][1][ 6][ 6] =   8.900534e-01; eval[3][0][1][ 6][ 6] =   1.004843e-03;
  val[3][0][1][ 6][ 7] =   8.534590e-01; eval[3][0][1][ 6][ 7] =   1.187410e-03;
  val[3][0][1][ 6][ 8] =   8.313840e-01; eval[3][0][1][ 6][ 8] =   1.426894e-03;
  val[3][0][1][ 6][ 9] =   8.165529e-01; eval[3][0][1][ 6][ 9] =   1.820661e-03;
  val[3][0][1][ 6][10] =   8.042427e-01; eval[3][0][1][ 6][10] =   2.339845e-03;
  val[3][0][1][ 6][11] =   7.986248e-01; eval[3][0][1][ 6][11] =   3.072397e-03;
  val[3][0][1][ 6][12] =   7.924605e-01; eval[3][0][1][ 6][12] =   4.045355e-03;
  val[3][0][1][ 6][13] =   7.867638e-01; eval[3][0][1][ 6][13] =   5.187679e-03;
  val[3][0][1][ 6][14] =   7.893442e-01; eval[3][0][1][ 6][14] =   6.821542e-03;
  val[3][0][1][ 6][15] =   7.847748e-01; eval[3][0][1][ 6][15] =   8.664464e-03;
  val[3][0][1][ 6][16] =   7.722844e-01; eval[3][0][1][ 6][16] =   1.046469e-02;
  val[3][0][1][ 6][17] =   7.669601e-01; eval[3][0][1][ 6][17] =   1.287894e-02;
  val[3][0][1][ 6][18] =   7.635078e-01; eval[3][0][1][ 6][18] =   1.587314e-02;
  val[3][0][1][ 6][19] =   7.281413e-01; eval[3][0][1][ 6][19] =   1.716824e-02;
  val[3][0][1][ 6][20] =   7.601129e-01; eval[3][0][1][ 6][20] =   1.818786e-02;
  val[3][0][1][ 6][21] =   7.683505e-01; eval[3][0][1][ 6][21] =   2.696992e-02;
  val[3][0][1][ 6][22] =   7.979461e-01; eval[3][0][1][ 6][22] =   4.095219e-02;
  val[3][0][1][ 6][23] =   8.282739e-01; eval[3][0][1][ 6][23] =   4.360005e-02;
  val[3][0][1][ 6][24] =   8.385991e-01; eval[3][0][1][ 6][24] =   5.085041e-02;
  val[3][0][1][ 6][25] =   8.637063e-01; eval[3][0][1][ 6][25] =   5.826493e-02;
  val[3][0][1][ 6][26] =   7.892413e-01; eval[3][0][1][ 6][26] =   1.066946e-01;
  val[3][0][1][ 6][27] =   8.385263e-01; eval[3][0][1][ 6][27] =   1.130332e-01;
  val[3][0][1][ 6][28] =   1.078686e+00; eval[3][0][1][ 6][28] =   1.199774e-01;
  val[3][0][1][ 6][29] =   8.267639e-01; eval[3][0][1][ 6][29] =   1.351842e-01;
  //pc3dz_sigma_c0d1z7
  n_val[3][0][1][7] = 30;
  val[3][0][1][ 7][ 0] =   0.000000e+00; eval[3][0][1][ 7][ 0] =   0.000000e+00;
  val[3][0][1][ 7][ 1] =   0.000000e+00; eval[3][0][1][ 7][ 1] =   0.000000e+00;
  val[3][0][1][ 7][ 2] =   1.429444e+00; eval[3][0][1][ 7][ 2] =   5.670568e-04;
  val[3][0][1][ 7][ 3] =   1.162160e+00; eval[3][0][1][ 7][ 3] =   5.498277e-04;
  val[3][0][1][ 7][ 4] =   1.055500e+00; eval[3][0][1][ 7][ 4] =   5.281151e-04;
  val[3][0][1][ 7][ 5] =   9.675660e-01; eval[3][0][1][ 7][ 5] =   7.262521e-04;
  val[3][0][1][ 7][ 6] =   9.002600e-01; eval[3][0][1][ 7][ 6] =   1.090954e-03;
  val[3][0][1][ 7][ 7] =   8.641767e-01; eval[3][0][1][ 7][ 7] =   1.271869e-03;
  val[3][0][1][ 7][ 8] =   8.380849e-01; eval[3][0][1][ 7][ 8] =   1.500845e-03;
  val[3][0][1][ 7][ 9] =   8.167828e-01; eval[3][0][1][ 7][ 9] =   1.878535e-03;
  val[3][0][1][ 7][10] =   8.092171e-01; eval[3][0][1][ 7][10] =   2.459978e-03;
  val[3][0][1][ 7][11] =   8.004062e-01; eval[3][0][1][ 7][11] =   3.211214e-03;
  val[3][0][1][ 7][12] =   7.951934e-01; eval[3][0][1][ 7][12] =   4.212689e-03;
  val[3][0][1][ 7][13] =   7.606453e-01; eval[3][0][1][ 7][13] =   7.832939e-03;
  val[3][0][1][ 7][14] =   7.447927e-01; eval[3][0][1][ 7][14] =   9.494045e-03;
  val[3][0][1][ 7][15] =   7.635644e-01; eval[3][0][1][ 7][15] =   1.300446e-02;
  val[3][0][1][ 7][16] =   7.634986e-01; eval[3][0][1][ 7][16] =   1.637205e-02;
  val[3][0][1][ 7][17] =   7.350124e-01; eval[3][0][1][ 7][17] =   1.836588e-02;
  val[3][0][1][ 7][18] =   6.981830e-01; eval[3][0][1][ 7][18] =   1.940702e-02;
  val[3][0][1][ 7][19] =   7.217570e-01; eval[3][0][1][ 7][19] =   2.627336e-02;
  val[3][0][1][ 7][20] =   7.485763e-01; eval[3][0][1][ 7][20] =   2.693785e-02;
  val[3][0][1][ 7][21] =   8.095360e-01; eval[3][0][1][ 7][21] =   4.833888e-02;
  val[3][0][1][ 7][22] =   8.075474e-01; eval[3][0][1][ 7][22] =   2.789880e-02;
  val[3][0][1][ 7][23] =   7.380773e-01; eval[3][0][1][ 7][23] =   4.143313e-02;
  val[3][0][1][ 7][24] =   7.157335e-01; eval[3][0][1][ 7][24] =   4.965224e-02;
  val[3][0][1][ 7][25] =   8.318114e-01; eval[3][0][1][ 7][25] =   6.895227e-02;
  val[3][0][1][ 7][26] =   7.094669e-01; eval[3][0][1][ 7][26] =   4.677177e-02;
  val[3][0][1][ 7][27] =   1.132143e+00; eval[3][0][1][ 7][27] =   1.610565e-01;
  val[3][0][1][ 7][28] =   4.456502e-01; eval[3][0][1][ 7][28] =   9.679890e-02;
  val[3][0][1][ 7][29] =   4.945998e-01; eval[3][0][1][ 7][29] =   1.686932e-01;
  //pc3dz_sigma_c0d1z8
  n_val[3][0][1][8] = 30;
  val[3][0][1][ 8][ 0] =   0.000000e+00; eval[3][0][1][ 8][ 0] =   0.000000e+00;
  val[3][0][1][ 8][ 1] =   0.000000e+00; eval[3][0][1][ 8][ 1] =   0.000000e+00;
  val[3][0][1][ 8][ 2] =   1.418611e+00; eval[3][0][1][ 8][ 2] =   5.379335e-04;
  val[3][0][1][ 8][ 3] =   1.165908e+00; eval[3][0][1][ 8][ 3] =   5.513059e-04;
  val[3][0][1][ 8][ 4] =   1.064124e+00; eval[3][0][1][ 8][ 4] =   5.484517e-04;
  val[3][0][1][ 8][ 5] =   9.898836e-01; eval[3][0][1][ 8][ 5] =   7.717361e-04;
  val[3][0][1][ 8][ 6] =   9.473999e-01; eval[3][0][1][ 8][ 6] =   8.550607e-04;
  val[3][0][1][ 8][ 7] =   9.160282e-01; eval[3][0][1][ 8][ 7] =   1.034054e-03;
  val[3][0][1][ 8][ 8] =   8.905310e-01; eval[3][0][1][ 8][ 8] =   1.254784e-03;
  val[3][0][1][ 8][ 9] =   8.608314e-01; eval[3][0][1][ 8][ 9] =   2.205920e-03;
  val[3][0][1][ 8][10] =   8.410767e-01; eval[3][0][1][ 8][10] =   2.754583e-03;
  val[3][0][1][ 8][11] =   8.294054e-01; eval[3][0][1][ 8][11] =   3.537116e-03;
  val[3][0][1][ 8][12] =   8.265729e-01; eval[3][0][1][ 8][12] =   4.638400e-03;
  val[3][0][1][ 8][13] =   8.208742e-01; eval[3][0][1][ 8][13] =   6.017076e-03;
  val[3][0][1][ 8][14] =   8.175848e-01; eval[3][0][1][ 8][14] =   7.685621e-03;
  val[3][0][1][ 8][15] =   8.172526e-01; eval[3][0][1][ 8][15] =   1.000237e-02;
  val[3][0][1][ 8][16] =   7.886631e-01; eval[3][0][1][ 8][16] =   1.158186e-02;
  val[3][0][1][ 8][17] =   8.044994e-01; eval[3][0][1][ 8][17] =   1.515435e-02;
  val[3][0][1][ 8][18] =   7.853435e-01; eval[3][0][1][ 8][18] =   1.772919e-02;
  val[3][0][1][ 8][19] =   7.659591e-01; eval[3][0][1][ 8][19] =   2.036536e-02;
  val[3][0][1][ 8][20] =   8.331639e-01; eval[3][0][1][ 8][20] =   2.391289e-02;
  val[3][0][1][ 8][21] =   7.986321e-01; eval[3][0][1][ 8][21] =   3.000961e-02;
  val[3][0][1][ 8][22] =   7.707829e-01; eval[3][0][1][ 8][22] =   3.795259e-02;
  val[3][0][1][ 8][23] =   7.603711e-01; eval[3][0][1][ 8][23] =   4.951200e-02;
  val[3][0][1][ 8][24] =   7.806811e-01; eval[3][0][1][ 8][24] =   7.024587e-02;
  val[3][0][1][ 8][25] =   8.031223e-01; eval[3][0][1][ 8][25] =   9.535550e-02;
  val[3][0][1][ 8][26] =   7.612957e-01; eval[3][0][1][ 8][26] =   7.362546e-02;
  val[3][0][1][ 8][27] =   4.595538e-01; eval[3][0][1][ 8][27] =   9.115981e-02;
  val[3][0][1][ 8][28] =   9.665982e-01; eval[3][0][1][ 8][28] =   2.890460e-01;
  val[3][0][1][ 8][29] =   6.650191e-01; eval[3][0][1][ 8][29] =   1.097897e-01;
  //pc3dz_sigma_c0d1z9
  n_val[3][0][1][9] = 30;
  val[3][0][1][ 9][ 0] =   0.000000e+00; eval[3][0][1][ 9][ 0] =   0.000000e+00;
  val[3][0][1][ 9][ 1] =   0.000000e+00; eval[3][0][1][ 9][ 1] =   0.000000e+00;
  val[3][0][1][ 9][ 2] =   1.469579e+00; eval[3][0][1][ 9][ 2] =   7.422262e-04;
  val[3][0][1][ 9][ 3] =   1.216866e+00; eval[3][0][1][ 9][ 3] =   5.761623e-04;
  val[3][0][1][ 9][ 4] =   1.059059e+00; eval[3][0][1][ 9][ 4] =   8.172453e-04;
  val[3][0][1][ 9][ 5] =   9.940541e-01; eval[3][0][1][ 9][ 5] =   9.068091e-04;
  val[3][0][1][ 9][ 6] =   9.583224e-01; eval[3][0][1][ 9][ 6] =   1.031621e-03;
  val[3][0][1][ 9][ 7] =   9.362774e-01; eval[3][0][1][ 9][ 7] =   1.265843e-03;
  val[3][0][1][ 9][ 8] =   9.001501e-01; eval[3][0][1][ 9][ 8] =   2.149249e-03;
  val[3][0][1][ 9][ 9] =   8.851249e-01; eval[3][0][1][ 9][ 9] =   2.769146e-03;
  val[3][0][1][ 9][10] =   8.695973e-01; eval[3][0][1][ 9][10] =   3.557792e-03;
  val[3][0][1][ 9][11] =   8.662647e-01; eval[3][0][1][ 9][11] =   4.742592e-03;
  val[3][0][1][ 9][12] =   8.732003e-01; eval[3][0][1][ 9][12] =   4.423632e-03;
  val[3][0][1][ 9][13] =   8.672165e-01; eval[3][0][1][ 9][13] =   5.707954e-03;
  val[3][0][1][ 9][14] =   8.674226e-01; eval[3][0][1][ 9][14] =   7.356340e-03;
  val[3][0][1][ 9][15] =   8.563392e-01; eval[3][0][1][ 9][15] =   9.085403e-03;
  val[3][0][1][ 9][16] =   8.672747e-01; eval[3][0][1][ 9][16] =   1.203888e-02;
  val[3][0][1][ 9][17] =   8.191271e-01; eval[3][0][1][ 9][17] =   1.796444e-02;
  val[3][0][1][ 9][18] =   8.056541e-01; eval[3][0][1][ 9][18] =   2.164063e-02;
  val[3][0][1][ 9][19] =   8.245192e-01; eval[3][0][1][ 9][19] =   2.712638e-02;
  val[3][0][1][ 9][20] =   8.478698e-01; eval[3][0][1][ 9][20] =   2.748141e-02;
  val[3][0][1][ 9][21] =   7.976222e-01; eval[3][0][1][ 9][21] =   3.301685e-02;
  val[3][0][1][ 9][22] =   8.133312e-01; eval[3][0][1][ 9][22] =   4.734388e-02;
  val[3][0][1][ 9][23] =   9.178958e-01; eval[3][0][1][ 9][23] =   6.172317e-02;
  val[3][0][1][ 9][24] =   7.600045e-01; eval[3][0][1][ 9][24] =   7.156195e-02;
  val[3][0][1][ 9][25] =   9.175457e-01; eval[3][0][1][ 9][25] =   5.686872e-02;
  val[3][0][1][ 9][26] =   6.767864e-01; eval[3][0][1][ 9][26] =   8.267546e-02;
  val[3][0][1][ 9][27] =   9.966099e-01; eval[3][0][1][ 9][27] =   1.921415e-01;
  val[3][0][1][ 9][28] =   4.812431e+00; eval[3][0][1][ 9][28] =   4.060860e+00;
  val[3][0][1][ 9][29] =   1.112382e+00; eval[3][0][1][ 9][29] =   1.832704e-01;
  //pc3dz_sigma_c1d0z0
  n_val[3][1][0][0] = 30;
  val[3][1][0][ 0][ 0] =   0.000000e+00; eval[3][1][0][ 0][ 0] =   0.000000e+00;
  val[3][1][0][ 0][ 1] =   0.000000e+00; eval[3][1][0][ 0][ 1] =   0.000000e+00;
  val[3][1][0][ 0][ 2] =   1.450099e+00; eval[3][1][0][ 0][ 2] =   5.337308e-04;
  val[3][1][0][ 0][ 3] =   1.144808e+00; eval[3][1][0][ 0][ 3] =   5.058173e-04;
  val[3][1][0][ 0][ 4] =   9.819673e-01; eval[3][1][0][ 0][ 4] =   5.837824e-04;
  val[3][1][0][ 0][ 5] =   8.784430e-01; eval[3][1][0][ 0][ 5] =   8.112865e-04;
  val[3][1][0][ 0][ 6] =   8.262702e-01; eval[3][1][0][ 0][ 6] =   8.735159e-04;
  val[3][1][0][ 0][ 7] =   7.772431e-01; eval[3][1][0][ 0][ 7] =   1.592976e-03;
  val[3][1][0][ 0][ 8] =   7.500541e-01; eval[3][1][0][ 0][ 8] =   1.853525e-03;
  val[3][1][0][ 0][ 9] =   7.586450e-01; eval[3][1][0][ 0][ 9] =   1.568838e-03;
  val[3][1][0][ 0][10] =   7.442285e-01; eval[3][1][0][ 0][10] =   1.969122e-03;
  val[3][1][0][ 0][11] =   7.387463e-01; eval[3][1][0][ 0][11] =   2.540531e-03;
  val[3][1][0][ 0][12] =   7.281131e-01; eval[3][1][0][ 0][12] =   3.217016e-03;
  val[3][1][0][ 0][13] =   7.292383e-01; eval[3][1][0][ 0][13] =   4.234107e-03;
  val[3][1][0][ 0][14] =   7.273334e-01; eval[3][1][0][ 0][14] =   5.485555e-03;
  val[3][1][0][ 0][15] =   7.219830e-01; eval[3][1][0][ 0][15] =   6.869082e-03;
  val[3][1][0][ 0][16] =   7.278489e-01; eval[3][1][0][ 0][16] =   1.437649e-02;
  val[3][1][0][ 0][17] =   6.747794e-01; eval[3][1][0][ 0][17] =   1.452148e-02;
  val[3][1][0][ 0][18] =   7.285937e-01; eval[3][1][0][ 0][18] =   1.394078e-02;
  val[3][1][0][ 0][19] =   6.949150e-01; eval[3][1][0][ 0][19] =   2.378581e-02;
  val[3][1][0][ 0][20] =   7.409348e-01; eval[3][1][0][ 0][20] =   1.619938e-02;
  val[3][1][0][ 0][21] =   6.999633e-01; eval[3][1][0][ 0][21] =   3.217119e-02;
  val[3][1][0][ 0][22] =   7.248569e-01; eval[3][1][0][ 0][22] =   3.058255e-02;
  val[3][1][0][ 0][23] =   7.492370e-01; eval[3][1][0][ 0][23] =   4.410097e-02;
  val[3][1][0][ 0][24] =   8.319904e-01; eval[3][1][0][ 0][24] =   5.623091e-02;
  val[3][1][0][ 0][25] =   8.443124e-01; eval[3][1][0][ 0][25] =   5.066720e-02;
  val[3][1][0][ 0][26] =   5.679755e-01; eval[3][1][0][ 0][26] =   7.026109e-02;
  val[3][1][0][ 0][27] =   7.129383e-01; eval[3][1][0][ 0][27] =   6.827864e-02;
  val[3][1][0][ 0][28] =   9.050730e-01; eval[3][1][0][ 0][28] =   1.734894e-01;
  val[3][1][0][ 0][29] =   6.207225e-01; eval[3][1][0][ 0][29] =   1.505691e-01;
  //pc3dz_sigma_c1d0z1
  n_val[3][1][0][1] = 30;
  val[3][1][0][ 1][ 0] =   0.000000e+00; eval[3][1][0][ 1][ 0] =   0.000000e+00;
  val[3][1][0][ 1][ 1] =   0.000000e+00; eval[3][1][0][ 1][ 1] =   0.000000e+00;
  val[3][1][0][ 1][ 2] =   1.424074e+00; eval[3][1][0][ 1][ 2] =   5.016321e-04;
  val[3][1][0][ 1][ 3] =   1.123737e+00; eval[3][1][0][ 1][ 3] =   4.767709e-04;
  val[3][1][0][ 1][ 4] =   9.814341e-01; eval[3][1][0][ 1][ 4] =   5.943417e-04;
  val[3][1][0][ 1][ 5] =   8.660475e-01; eval[3][1][0][ 1][ 5] =   7.784910e-04;
  val[3][1][0][ 1][ 6] =   8.198205e-01; eval[3][1][0][ 1][ 6] =   8.590873e-04;
  val[3][1][0][ 1][ 7] =   7.714283e-01; eval[3][1][0][ 1][ 7] =   1.553170e-03;
  val[3][1][0][ 1][ 8] =   7.770912e-01; eval[3][1][0][ 1][ 8] =   1.268573e-03;
  val[3][1][0][ 1][ 9] =   7.565423e-01; eval[3][1][0][ 1][ 9] =   1.567096e-03;
  val[3][1][0][ 1][10] =   7.429160e-01; eval[3][1][0][ 1][10] =   1.974808e-03;
  val[3][1][0][ 1][11] =   7.312447e-01; eval[3][1][0][ 1][11] =   2.504900e-03;
  val[3][1][0][ 1][12] =   7.244492e-01; eval[3][1][0][ 1][12] =   3.208237e-03;
  val[3][1][0][ 1][13] =   7.206971e-01; eval[3][1][0][ 1][13] =   4.159601e-03;
  val[3][1][0][ 1][14] =   7.226594e-01; eval[3][1][0][ 1][14] =   5.428352e-03;
  val[3][1][0][ 1][15] =   7.160245e-01; eval[3][1][0][ 1][15] =   6.713785e-03;
  val[3][1][0][ 1][16] =   7.137974e-01; eval[3][1][0][ 1][16] =   8.405649e-03;
  val[3][1][0][ 1][17] =   7.026245e-01; eval[3][1][0][ 1][17] =   1.033436e-02;
  val[3][1][0][ 1][18] =   7.188043e-01; eval[3][1][0][ 1][18] =   1.354065e-02;
  val[3][1][0][ 1][19] =   7.199565e-01; eval[3][1][0][ 1][19] =   1.693753e-02;
  val[3][1][0][ 1][20] =   7.079979e-01; eval[3][1][0][ 1][20] =   1.505644e-02;
  val[3][1][0][ 1][21] =   6.227438e-01; eval[3][1][0][ 1][21] =   4.800364e-02;
  val[3][1][0][ 1][22] =   7.264778e-01; eval[3][1][0][ 1][22] =   3.089307e-02;
  val[3][1][0][ 1][23] =   7.931445e-01; eval[3][1][0][ 1][23] =   3.080999e-02;
  val[3][1][0][ 1][24] =   8.058789e-01; eval[3][1][0][ 1][24] =   3.488315e-02;
  val[3][1][0][ 1][25] =   4.595315e-01; eval[3][1][0][ 1][25] =   5.856609e-02;
  val[3][1][0][ 1][26] =   7.150041e-01; eval[3][1][0][ 1][26] =   7.598139e-02;
  val[3][1][0][ 1][27] =   8.269959e-01; eval[3][1][0][ 1][27] =   1.070078e-01;
  val[3][1][0][ 1][28] =   9.791346e-01; eval[3][1][0][ 1][28] =   1.347206e-01;
  val[3][1][0][ 1][29] =   3.322083e-01; eval[3][1][0][ 1][29] =   5.686632e-02;
  //pc3dz_sigma_c1d0z2
  n_val[3][1][0][2] = 30;
  val[3][1][0][ 2][ 0] =   0.000000e+00; eval[3][1][0][ 2][ 0] =   0.000000e+00;
  val[3][1][0][ 2][ 1] =   0.000000e+00; eval[3][1][0][ 2][ 1] =   0.000000e+00;
  val[3][1][0][ 2][ 2] =   1.416286e+00; eval[3][1][0][ 2][ 2] =   5.999158e-04;
  val[3][1][0][ 2][ 3] =   1.138593e+00; eval[3][1][0][ 2][ 3] =   5.014863e-04;
  val[3][1][0][ 2][ 4] =   9.801142e-01; eval[3][1][0][ 2][ 4] =   5.822749e-04;
  val[3][1][0][ 2][ 5] =   8.985703e-01; eval[3][1][0][ 2][ 5] =   5.898555e-04;
  val[3][1][0][ 2][ 6] =   8.472958e-01; eval[3][1][0][ 2][ 6] =   6.529194e-04;
  val[3][1][0][ 2][ 7] =   8.058522e-01; eval[3][1][0][ 2][ 7] =   1.095691e-03;
  val[3][1][0][ 2][ 8] =   7.780108e-01; eval[3][1][0][ 2][ 8] =   1.273139e-03;
  val[3][1][0][ 2][ 9] =   7.620162e-01; eval[3][1][0][ 2][ 9] =   1.605928e-03;
  val[3][1][0][ 2][10] =   7.479548e-01; eval[3][1][0][ 2][10] =   2.036077e-03;
  val[3][1][0][ 2][11] =   7.391444e-01; eval[3][1][0][ 2][11] =   2.614180e-03;
  val[3][1][0][ 2][12] =   7.317562e-01; eval[3][1][0][ 2][12] =   3.368353e-03;
  val[3][1][0][ 2][13] =   7.293402e-01; eval[3][1][0][ 2][13] =   4.372694e-03;
  val[3][1][0][ 2][14] =   7.248723e-01; eval[3][1][0][ 2][14] =   5.576501e-03;
  val[3][1][0][ 2][15] =   7.285181e-01; eval[3][1][0][ 2][15] =   7.211405e-03;
  val[3][1][0][ 2][16] =   7.278066e-01; eval[3][1][0][ 2][16] =   9.211379e-03;
  val[3][1][0][ 2][17] =   6.944661e-01; eval[3][1][0][ 2][17] =   1.011973e-02;
  val[3][1][0][ 2][18] =   7.380718e-01; eval[3][1][0][ 2][18] =   1.469718e-02;
  val[3][1][0][ 2][19] =   7.449956e-01; eval[3][1][0][ 2][19] =   1.884868e-02;
  val[3][1][0][ 2][20] =   7.141366e-01; eval[3][1][0][ 2][20] =   1.569934e-02;
  val[3][1][0][ 2][21] =   6.966985e-01; eval[3][1][0][ 2][21] =   2.074012e-02;
  val[3][1][0][ 2][22] =   7.604702e-01; eval[3][1][0][ 2][22] =   2.342188e-02;
  val[3][1][0][ 2][23] =   6.992336e-01; eval[3][1][0][ 2][23] =   3.863549e-02;
  val[3][1][0][ 2][24] =   7.408444e-01; eval[3][1][0][ 2][24] =   5.899650e-02;
  val[3][1][0][ 2][25] =   5.574561e-01; eval[3][1][0][ 2][25] =   5.703521e-02;
  val[3][1][0][ 2][26] =   7.967915e-01; eval[3][1][0][ 2][26] =   5.098944e-02;
  val[3][1][0][ 2][27] =   5.005024e-01; eval[3][1][0][ 2][27] =   1.277776e-01;
  val[3][1][0][ 2][28] =   9.433506e-01; eval[3][1][0][ 2][28] =   1.951565e-01;
  val[3][1][0][ 2][29] =   7.532592e-01; eval[3][1][0][ 2][29] =   1.518701e-01;
  //pc3dz_sigma_c1d0z3
  n_val[3][1][0][3] = 30;
  val[3][1][0][ 3][ 0] =   0.000000e+00; eval[3][1][0][ 3][ 0] =   0.000000e+00;
  val[3][1][0][ 3][ 1] =   0.000000e+00; eval[3][1][0][ 3][ 1] =   0.000000e+00;
  val[3][1][0][ 3][ 2] =   1.454007e+00; eval[3][1][0][ 3][ 2] =   5.158526e-04;
  val[3][1][0][ 3][ 3] =   1.145479e+00; eval[3][1][0][ 3][ 3] =   5.112593e-04;
  val[3][1][0][ 3][ 4] =   9.775921e-01; eval[3][1][0][ 3][ 4] =   6.026932e-04;
  val[3][1][0][ 3][ 5] =   8.936883e-01; eval[3][1][0][ 3][ 5] =   5.981009e-04;
  val[3][1][0][ 3][ 6] =   8.350119e-01; eval[3][1][0][ 3][ 6] =   9.145810e-04;
  val[3][1][0][ 3][ 7] =   7.964524e-01; eval[3][1][0][ 3][ 7] =   1.081320e-03;
  val[3][1][0][ 3][ 8] =   7.735753e-01; eval[3][1][0][ 3][ 8] =   1.289287e-03;
  val[3][1][0][ 3][ 9] =   7.580740e-01; eval[3][1][0][ 3][ 9] =   1.629275e-03;
  val[3][1][0][ 3][10] =   7.504545e-01; eval[3][1][0][ 3][10] =   2.104946e-03;
  val[3][1][0][ 3][11] =   7.385024e-01; eval[3][1][0][ 3][11] =   2.651603e-03;
  val[3][1][0][ 3][12] =   7.338634e-01; eval[3][1][0][ 3][12] =   3.426948e-03;
  val[3][1][0][ 3][13] =   7.230569e-01; eval[3][1][0][ 3][13] =   4.315809e-03;
  val[3][1][0][ 3][14] =   7.197700e-01; eval[3][1][0][ 3][14] =   5.459789e-03;
  val[3][1][0][ 3][15] =   7.082918e-01; eval[3][1][0][ 3][15] =   6.757320e-03;
  val[3][1][0][ 3][16] =   7.113168e-01; eval[3][1][0][ 3][16] =   8.571619e-03;
  val[3][1][0][ 3][17] =   6.922010e-01; eval[3][1][0][ 3][17] =   1.602999e-02;
  val[3][1][0][ 3][18] =   6.927376e-01; eval[3][1][0][ 3][18] =   1.228103e-02;
  val[3][1][0][ 3][19] =   7.107689e-01; eval[3][1][0][ 3][19] =   1.609057e-02;
  val[3][1][0][ 3][20] =   7.341344e-01; eval[3][1][0][ 3][20] =   1.682274e-02;
  val[3][1][0][ 3][21] =   6.080256e-01; eval[3][1][0][ 3][21] =   4.514664e-02;
  val[3][1][0][ 3][22] =   7.435355e-01; eval[3][1][0][ 3][22] =   3.267833e-02;
  val[3][1][0][ 3][23] =   6.859148e-01; eval[3][1][0][ 3][23] =   3.461564e-02;
  val[3][1][0][ 3][24] =   7.675845e-01; eval[3][1][0][ 3][24] =   4.728712e-02;
  val[3][1][0][ 3][25] =   4.919253e-01; eval[3][1][0][ 3][25] =   7.605746e-02;
  val[3][1][0][ 3][26] =   5.614471e-01; eval[3][1][0][ 3][26] =   7.231588e-02;
  val[3][1][0][ 3][27] =   7.973965e-01; eval[3][1][0][ 3][27] =   9.869778e-02;
  val[3][1][0][ 3][28] =   6.371251e-01; eval[3][1][0][ 3][28] =   8.913828e-02;
  val[3][1][0][ 3][29] =   7.003885e-01; eval[3][1][0][ 3][29] =   1.174134e-01;
  //pc3dz_sigma_c1d0z4
  n_val[3][1][0][4] = 30;
  val[3][1][0][ 4][ 0] =   0.000000e+00; eval[3][1][0][ 4][ 0] =   0.000000e+00;
  val[3][1][0][ 4][ 1] =   0.000000e+00; eval[3][1][0][ 4][ 1] =   0.000000e+00;
  val[3][1][0][ 4][ 2] =   1.410898e+00; eval[3][1][0][ 4][ 2] =   5.274788e-04;
  val[3][1][0][ 4][ 3] =   1.122750e+00; eval[3][1][0][ 4][ 3] =   5.428079e-04;
  val[3][1][0][ 4][ 4] =   9.472087e-01; eval[3][1][0][ 4][ 4] =   9.181108e-04;
  val[3][1][0][ 4][ 5] =   8.631313e-01; eval[3][1][0][ 4][ 5] =   8.802933e-04;
  val[3][1][0][ 4][ 6] =   8.180478e-01; eval[3][1][0][ 4][ 6] =   9.797347e-04;
  val[3][1][0][ 4][ 7] =   7.885392e-01; eval[3][1][0][ 4][ 7] =   1.187006e-03;
  val[3][1][0][ 4][ 8] =   7.677386e-01; eval[3][1][0][ 4][ 8] =   1.415430e-03;
  val[3][1][0][ 4][ 9] =   7.532634e-01; eval[3][1][0][ 4][ 9] =   1.796145e-03;
  val[3][1][0][ 4][10] =   7.417778e-01; eval[3][1][0][ 4][10] =   2.290540e-03;
  val[3][1][0][ 4][11] =   7.338886e-01; eval[3][1][0][ 4][11] =   2.946158e-03;
  val[3][1][0][ 4][12] =   7.275492e-01; eval[3][1][0][ 4][12] =   3.832785e-03;
  val[3][1][0][ 4][13] =   7.225754e-01; eval[3][1][0][ 4][13] =   4.863581e-03;
  val[3][1][0][ 4][14] =   7.153674e-01; eval[3][1][0][ 4][14] =   6.130224e-03;
  val[3][1][0][ 4][15] =   7.160818e-01; eval[3][1][0][ 4][15] =   7.877452e-03;
  val[3][1][0][ 4][16] =   7.095368e-01; eval[3][1][0][ 4][16] =   9.741076e-03;
  val[3][1][0][ 4][17] =   7.306870e-01; eval[3][1][0][ 4][17] =   1.340943e-02;
  val[3][1][0][ 4][18] =   7.122748e-01; eval[3][1][0][ 4][18] =   1.581009e-02;
  val[3][1][0][ 4][19] =   6.873594e-01; eval[3][1][0][ 4][19] =   2.762639e-02;
  val[3][1][0][ 4][20] =   7.186797e-01; eval[3][1][0][ 4][20] =   1.834349e-02;
  val[3][1][0][ 4][21] =   7.012783e-01; eval[3][1][0][ 4][21] =   3.901637e-02;
  val[3][1][0][ 4][22] =   6.207395e-01; eval[3][1][0][ 4][22] =   3.662410e-02;
  val[3][1][0][ 4][23] =   5.256018e-01; eval[3][1][0][ 4][23] =   6.237783e-02;
  val[3][1][0][ 4][24] =   8.808274e-01; eval[3][1][0][ 4][24] =   6.028268e-02;
  val[3][1][0][ 4][25] =   8.169570e-01; eval[3][1][0][ 4][25] =   1.131564e-01;
  val[3][1][0][ 4][26] =   6.564755e-01; eval[3][1][0][ 4][26] =   5.763309e-02;
  val[3][1][0][ 4][27] =   9.149461e-01; eval[3][1][0][ 4][27] =   1.624875e-01;
  val[3][1][0][ 4][28] =   6.254956e-01; eval[3][1][0][ 4][28] =   9.184667e-02;
  val[3][1][0][ 4][29] =   8.905892e-01; eval[3][1][0][ 4][29] =   1.608456e-01;
  //pc3dz_sigma_c1d0z5
  n_val[3][1][0][5] = 30;
  val[3][1][0][ 5][ 0] =   0.000000e+00; eval[3][1][0][ 5][ 0] =   0.000000e+00;
  val[3][1][0][ 5][ 1] =   0.000000e+00; eval[3][1][0][ 5][ 1] =   0.000000e+00;
  val[3][1][0][ 5][ 2] =   1.415435e+00; eval[3][1][0][ 5][ 2] =   5.628461e-04;
  val[3][1][0][ 5][ 3] =   1.141186e+00; eval[3][1][0][ 5][ 3] =   6.062500e-04;
  val[3][1][0][ 5][ 4] =   9.896084e-01; eval[3][1][0][ 5][ 4] =   7.701003e-04;
  val[3][1][0][ 5][ 5] =   9.214395e-01; eval[3][1][0][ 5][ 5] =   8.366822e-04;
  val[3][1][0][ 5][ 6] =   8.614686e-01; eval[3][1][0][ 5][ 6] =   1.310171e-03;
  val[3][1][0][ 5][ 7] =   8.271740e-01; eval[3][1][0][ 5][ 7] =   1.555863e-03;
  val[3][1][0][ 5][ 8] =   8.092499e-01; eval[3][1][0][ 5][ 8] =   1.859959e-03;
  val[3][1][0][ 5][ 9] =   7.906014e-01; eval[3][1][0][ 5][ 9] =   2.303783e-03;
  val[3][1][0][ 5][10] =   7.827939e-01; eval[3][1][0][ 5][10] =   2.973402e-03;
  val[3][1][0][ 5][11] =   7.429188e-01; eval[3][1][0][ 5][11] =   5.406991e-03;
  val[3][1][0][ 5][12] =   7.243308e-01; eval[3][1][0][ 5][12] =   6.634088e-03;
  val[3][1][0][ 5][13] =   7.579830e-01; eval[3][1][0][ 5][13] =   9.903693e-03;
  val[3][1][0][ 5][14] =   7.364795e-01; eval[3][1][0][ 5][14] =   1.181892e-02;
  val[3][1][0][ 5][15] =   7.561592e-01; eval[3][1][0][ 5][15] =   1.618623e-02;
  val[3][1][0][ 5][16] =   7.207161e-01; eval[3][1][0][ 5][16] =   1.787355e-02;
  val[3][1][0][ 5][17] =   7.240166e-01; eval[3][1][0][ 5][17] =   2.273394e-02;
  val[3][1][0][ 5][18] =   7.528934e-01; eval[3][1][0][ 5][18] =   3.097928e-02;
  val[3][1][0][ 5][19] =   6.823885e-01; eval[3][1][0][ 5][19] =   2.937734e-02;
  val[3][1][0][ 5][20] =   7.209187e-01; eval[3][1][0][ 5][20] =   3.190911e-02;
  val[3][1][0][ 5][21] =   7.133314e-01; eval[3][1][0][ 5][21] =   4.440222e-02;
  val[3][1][0][ 5][22] =   7.241390e-01; eval[3][1][0][ 5][22] =   6.078218e-02;
  val[3][1][0][ 5][23] =   7.641998e-01; eval[3][1][0][ 5][23] =   6.001140e-02;
  val[3][1][0][ 5][24] =   6.558942e-01; eval[3][1][0][ 5][24] =   8.555888e-02;
  val[3][1][0][ 5][25] =   7.254581e-01; eval[3][1][0][ 5][25] =   6.030799e-02;
  val[3][1][0][ 5][26] =   4.859148e-01; eval[3][1][0][ 5][26] =   1.177669e-01;
  val[3][1][0][ 5][27] =   5.616815e-01; eval[3][1][0][ 5][27] =   1.082418e-01;
  val[3][1][0][ 5][28] =   1.881286e+00; eval[3][1][0][ 5][28] =   1.890346e+00;
  val[3][1][0][ 5][29] =   4.076942e-01; eval[3][1][0][ 5][29] =   1.138914e-01;
  //pc3dz_sigma_c1d0z6
  n_val[3][1][0][6] = 30;
  val[3][1][0][ 6][ 0] =   0.000000e+00; eval[3][1][0][ 6][ 0] =   0.000000e+00;
  val[3][1][0][ 6][ 1] =   0.000000e+00; eval[3][1][0][ 6][ 1] =   0.000000e+00;
  val[3][1][0][ 6][ 2] =   1.454187e+00; eval[3][1][0][ 6][ 2] =   5.007741e-04;
  val[3][1][0][ 6][ 3] =   1.158231e+00; eval[3][1][0][ 6][ 3] =   5.147162e-04;
  val[3][1][0][ 6][ 4] =   1.009292e+00; eval[3][1][0][ 6][ 4] =   6.823315e-04;
  val[3][1][0][ 6][ 5] =   9.111296e-01; eval[3][1][0][ 6][ 5] =   9.838571e-04;
  val[3][1][0][ 6][ 6] =   8.603379e-01; eval[3][1][0][ 6][ 6] =   1.069342e-03;
  val[3][1][0][ 6][ 7] =   8.237180e-01; eval[3][1][0][ 6][ 7] =   1.251571e-03;
  val[3][1][0][ 6][ 8] =   8.051513e-01; eval[3][1][0][ 6][ 8] =   1.494532e-03;
  val[3][1][0][ 6][ 9] =   7.877696e-01; eval[3][1][0][ 6][ 9] =   1.870716e-03;
  val[3][1][0][ 6][10] =   7.808733e-01; eval[3][1][0][ 6][10] =   2.431416e-03;
  val[3][1][0][ 6][11] =   7.736374e-01; eval[3][1][0][ 6][11] =   3.117672e-03;
  val[3][1][0][ 6][12] =   7.727415e-01; eval[3][1][0][ 6][12] =   4.095154e-03;
  val[3][1][0][ 6][13] =   7.694346e-01; eval[3][1][0][ 6][13] =   5.280919e-03;
  val[3][1][0][ 6][14] =   7.293323e-01; eval[3][1][0][ 6][14] =   9.407203e-03;
  val[3][1][0][ 6][15] =   7.244623e-01; eval[3][1][0][ 6][15] =   1.173531e-02;
  val[3][1][0][ 6][16] =   7.131715e-01; eval[3][1][0][ 6][16] =   1.428669e-02;
  val[3][1][0][ 6][17] =   7.261230e-01; eval[3][1][0][ 6][17] =   1.856121e-02;
  val[3][1][0][ 6][18] =   7.428610e-01; eval[3][1][0][ 6][18] =   2.521153e-02;
  val[3][1][0][ 6][19] =   6.879227e-01; eval[3][1][0][ 6][19] =   2.454552e-02;
  val[3][1][0][ 6][20] =   7.039825e-01; eval[3][1][0][ 6][20] =   2.437857e-02;
  val[3][1][0][ 6][21] =   7.661554e-01; eval[3][1][0][ 6][21] =   4.424784e-02;
  val[3][1][0][ 6][22] =   6.505650e-01; eval[3][1][0][ 6][22] =   3.831538e-02;
  val[3][1][0][ 6][23] =   7.285424e-01; eval[3][1][0][ 6][23] =   4.330116e-02;
  val[3][1][0][ 6][24] =   7.468733e-01; eval[3][1][0][ 6][24] =   4.383093e-02;
  val[3][1][0][ 6][25] =   7.768482e-01; eval[3][1][0][ 6][25] =   1.005319e-01;
  val[3][1][0][ 6][26] =   7.388908e-01; eval[3][1][0][ 6][26] =   9.884989e-02;
  val[3][1][0][ 6][27] =   5.596669e-01; eval[3][1][0][ 6][27] =   5.985556e-02;
  val[3][1][0][ 6][28] =   1.111457e+00; eval[3][1][0][ 6][28] =   8.156834e+00;
  val[3][1][0][ 6][29] =   2.630273e-01; eval[3][1][0][ 6][29] =   1.286535e+00;
  //pc3dz_sigma_c1d0z7
  n_val[3][1][0][7] = 30;
  val[3][1][0][ 7][ 0] =   0.000000e+00; eval[3][1][0][ 7][ 0] =   0.000000e+00;
  val[3][1][0][ 7][ 1] =   0.000000e+00; eval[3][1][0][ 7][ 1] =   0.000000e+00;
  val[3][1][0][ 7][ 2] =   1.445609e+00; eval[3][1][0][ 7][ 2] =   4.764819e-04;
  val[3][1][0][ 7][ 3] =   1.142724e+00; eval[3][1][0][ 7][ 3] =   4.863324e-04;
  val[3][1][0][ 7][ 4] =   1.001339e+00; eval[3][1][0][ 7][ 4] =   6.299291e-04;
  val[3][1][0][ 7][ 5] =   9.017171e-01; eval[3][1][0][ 7][ 5] =   9.487598e-04;
  val[3][1][0][ 7][ 6] =   8.520849e-01; eval[3][1][0][ 7][ 6] =   1.044710e-03;
  val[3][1][0][ 7][ 7] =   8.149128e-01; eval[3][1][0][ 7][ 7] =   1.194687e-03;
  val[3][1][0][ 7][ 8] =   7.927015e-01; eval[3][1][0][ 7][ 8] =   1.394032e-03;
  val[3][1][0][ 7][ 9] =   7.772726e-01; eval[3][1][0][ 7][ 9] =   1.749744e-03;
  val[3][1][0][ 7][10] =   7.635927e-01; eval[3][1][0][ 7][10] =   2.238130e-03;
  val[3][1][0][ 7][11] =   7.358932e-01; eval[3][1][0][ 7][11] =   4.229002e-03;
  val[3][1][0][ 7][12] =   7.382471e-01; eval[3][1][0][ 7][12] =   5.598218e-03;
  val[3][1][0][ 7][13] =   7.342781e-01; eval[3][1][0][ 7][13] =   7.233505e-03;
  val[3][1][0][ 7][14] =   7.109969e-01; eval[3][1][0][ 7][14] =   8.527688e-03;
  val[3][1][0][ 7][15] =   7.113054e-01; eval[3][1][0][ 7][15] =   1.085567e-02;
  val[3][1][0][ 7][16] =   7.013493e-01; eval[3][1][0][ 7][16] =   1.322308e-02;
  val[3][1][0][ 7][17] =   7.185137e-01; eval[3][1][0][ 7][17] =   1.761624e-02;
  val[3][1][0][ 7][18] =   7.259234e-01; eval[3][1][0][ 7][18] =   2.274402e-02;
  val[3][1][0][ 7][19] =   7.232390e-01; eval[3][1][0][ 7][19] =   2.727533e-02;
  val[3][1][0][ 7][20] =   6.876596e-01; eval[3][1][0][ 7][20] =   2.212046e-02;
  val[3][1][0][ 7][21] =   6.563375e-01; eval[3][1][0][ 7][21] =   2.737779e-02;
  val[3][1][0][ 7][22] =   7.303327e-01; eval[3][1][0][ 7][22] =   3.142335e-02;
  val[3][1][0][ 7][23] =   7.178429e-01; eval[3][1][0][ 7][23] =   2.965431e-02;
  val[3][1][0][ 7][24] =   6.901810e-01; eval[3][1][0][ 7][24] =   3.829248e-02;
  val[3][1][0][ 7][25] =   7.687235e-01; eval[3][1][0][ 7][25] =   8.193802e-02;
  val[3][1][0][ 7][26] =   7.275145e-01; eval[3][1][0][ 7][26] =   8.500070e-02;
  val[3][1][0][ 7][27] =   5.520426e-01; eval[3][1][0][ 7][27] =   7.386364e-02;
  val[3][1][0][ 7][28] =   1.007008e+00; eval[3][1][0][ 7][28] =   1.366492e-01;
  val[3][1][0][ 7][29] =   8.102843e-01; eval[3][1][0][ 7][29] =   1.261190e-01;
  //pc3dz_sigma_c1d0z8
  n_val[3][1][0][8] = 30;
  val[3][1][0][ 8][ 0] =   0.000000e+00; eval[3][1][0][ 8][ 0] =   0.000000e+00;
  val[3][1][0][ 8][ 1] =   0.000000e+00; eval[3][1][0][ 8][ 1] =   0.000000e+00;
  val[3][1][0][ 8][ 2] =   1.436020e+00; eval[3][1][0][ 8][ 2] =   4.629227e-04;
  val[3][1][0][ 8][ 3] =   1.143211e+00; eval[3][1][0][ 8][ 3] =   4.777977e-04;
  val[3][1][0][ 8][ 4] =   1.001395e+00; eval[3][1][0][ 8][ 4] =   6.139341e-04;
  val[3][1][0][ 8][ 5] =   9.284128e-01; eval[3][1][0][ 8][ 5] =   7.008735e-04;
  val[3][1][0][ 8][ 6] =   8.613298e-01; eval[3][1][0][ 8][ 6] =   1.081818e-03;
  val[3][1][0][ 8][ 7] =   8.210319e-01; eval[3][1][0][ 8][ 7] =   1.217529e-03;
  val[3][1][0][ 8][ 8] =   8.013312e-01; eval[3][1][0][ 8][ 8] =   1.436834e-03;
  val[3][1][0][ 8][ 9] =   7.855527e-01; eval[3][1][0][ 8][ 9] =   1.802218e-03;
  val[3][1][0][ 8][10] =   7.526609e-01; eval[3][1][0][ 8][10] =   3.403476e-03;
  val[3][1][0][ 8][11] =   7.378273e-01; eval[3][1][0][ 8][11] =   4.198367e-03;
  val[3][1][0][ 8][12] =   7.404856e-01; eval[3][1][0][ 8][12] =   5.545305e-03;
  val[3][1][0][ 8][13] =   7.301566e-01; eval[3][1][0][ 8][13] =   6.969028e-03;
  val[3][1][0][ 8][14] =   7.277426e-01; eval[3][1][0][ 8][14] =   8.902719e-03;
  val[3][1][0][ 8][15] =   7.216142e-01; eval[3][1][0][ 8][15] =   1.108918e-02;
  val[3][1][0][ 8][16] =   7.296020e-01; eval[3][1][0][ 8][16] =   1.454530e-02;
  val[3][1][0][ 8][17] =   6.879378e-01; eval[3][1][0][ 8][17] =   1.528941e-02;
  val[3][1][0][ 8][18] =   7.212114e-01; eval[3][1][0][ 8][18] =   2.173903e-02;
  val[3][1][0][ 8][19] =   7.175105e-01; eval[3][1][0][ 8][19] =   2.615522e-02;
  val[3][1][0][ 8][20] =   7.015887e-01; eval[3][1][0][ 8][20] =   2.326026e-02;
  val[3][1][0][ 8][21] =   7.001858e-01; eval[3][1][0][ 8][21] =   3.240823e-02;
  val[3][1][0][ 8][22] =   6.976848e-01; eval[3][1][0][ 8][22] =   4.539523e-02;
  val[3][1][0][ 8][23] =   6.826093e-01; eval[3][1][0][ 8][23] =   5.367016e-02;
  val[3][1][0][ 8][24] =   7.465100e-01; eval[3][1][0][ 8][24] =   4.171886e-02;
  val[3][1][0][ 8][25] =   8.737741e-01; eval[3][1][0][ 8][25] =   8.311382e-02;
  val[3][1][0][ 8][26] =   7.855254e-01; eval[3][1][0][ 8][26] =   4.862459e-02;
  val[3][1][0][ 8][27] =   6.039968e-01; eval[3][1][0][ 8][27] =   1.100322e-01;
  val[3][1][0][ 8][28] =   6.785903e-01; eval[3][1][0][ 8][28] =   1.176637e-01;
  val[3][1][0][ 8][29] =   8.643727e-01; eval[3][1][0][ 8][29] =   1.417432e-01;
  //pc3dz_sigma_c1d0z9
  n_val[3][1][0][9] = 30;
  val[3][1][0][ 9][ 0] =   0.000000e+00; eval[3][1][0][ 9][ 0] =   0.000000e+00;
  val[3][1][0][ 9][ 1] =   0.000000e+00; eval[3][1][0][ 9][ 1] =   0.000000e+00;
  val[3][1][0][ 9][ 2] =   1.475914e+00; eval[3][1][0][ 9][ 2] =   5.225686e-04;
  val[3][1][0][ 9][ 3] =   1.161144e+00; eval[3][1][0][ 9][ 3] =   5.109463e-04;
  val[3][1][0][ 9][ 4] =   1.011191e+00; eval[3][1][0][ 9][ 4] =   6.426569e-04;
  val[3][1][0][ 9][ 5] =   9.368134e-01; eval[3][1][0][ 9][ 5] =   7.205880e-04;
  val[3][1][0][ 9][ 6] =   8.700586e-01; eval[3][1][0][ 9][ 6] =   1.127663e-03;
  val[3][1][0][ 9][ 7] =   8.339818e-01; eval[3][1][0][ 9][ 7] =   1.298868e-03;
  val[3][1][0][ 9][ 8] =   8.163400e-01; eval[3][1][0][ 9][ 8] =   1.557806e-03;
  val[3][1][0][ 9][ 9] =   7.905692e-01; eval[3][1][0][ 9][ 9] =   1.871727e-03;
  val[3][1][0][ 9][10] =   7.812980e-01; eval[3][1][0][ 9][10] =   2.402953e-03;
  val[3][1][0][ 9][11] =   7.704195e-01; eval[3][1][0][ 9][11] =   3.036883e-03;
  val[3][1][0][ 9][12] =   7.666611e-01; eval[3][1][0][ 9][12] =   3.918230e-03;
  val[3][1][0][ 9][13] =   7.509403e-01; eval[3][1][0][ 9][13] =   4.820531e-03;
  val[3][1][0][ 9][14] =   7.453580e-01; eval[3][1][0][ 9][14] =   6.106419e-03;
  val[3][1][0][ 9][15] =   7.649510e-01; eval[3][1][0][ 9][15] =   8.264780e-03;
  val[3][1][0][ 9][16] =   7.441322e-01; eval[3][1][0][ 9][16] =   9.766422e-03;
  val[3][1][0][ 9][17] =   7.194076e-01; eval[3][1][0][ 9][17] =   1.103241e-02;
  val[3][1][0][ 9][18] =   7.446300e-01; eval[3][1][0][ 9][18] =   1.471263e-02;
  val[3][1][0][ 9][19] =   7.528376e-01; eval[3][1][0][ 9][19] =   1.875871e-02;
  val[3][1][0][ 9][20] =   7.262899e-01; eval[3][1][0][ 9][20] =   1.592154e-02;
  val[3][1][0][ 9][21] =   7.362834e-01; eval[3][1][0][ 9][21] =   2.380144e-02;
  val[3][1][0][ 9][22] =   6.901703e-01; eval[3][1][0][ 9][22] =   2.736681e-02;
  val[3][1][0][ 9][23] =   8.367363e-01; eval[3][1][0][ 9][23] =   3.578528e-02;
  val[3][1][0][ 9][24] =   8.835463e-01; eval[3][1][0][ 9][24] =   4.472579e-02;
  val[3][1][0][ 9][25] =   7.146202e-01; eval[3][1][0][ 9][25] =   6.626358e-02;
  val[3][1][0][ 9][26] =   6.199132e-01; eval[3][1][0][ 9][26] =   6.147531e-02;
  val[3][1][0][ 9][27] =   5.367114e-01; eval[3][1][0][ 9][27] =   7.288418e-02;
  val[3][1][0][ 9][28] =   2.737870e+01; eval[3][1][0][ 9][28] =   2.498396e+01;
  val[3][1][0][ 9][29] =   8.793243e-01; eval[3][1][0][ 9][29] =   1.905564e-01;
  //pc3dz_sigma_c1d1z0
  n_val[3][1][1][0] = 30;
  val[3][1][1][ 0][ 0] =   0.000000e+00; eval[3][1][1][ 0][ 0] =   0.000000e+00;
  val[3][1][1][ 0][ 1] =   0.000000e+00; eval[3][1][1][ 0][ 1] =   0.000000e+00;
  val[3][1][1][ 0][ 2] =   1.394431e+00; eval[3][1][1][ 0][ 2] =   5.786384e-04;
  val[3][1][1][ 0][ 3] =   1.147545e+00; eval[3][1][1][ 0][ 3] =   5.022924e-04;
  val[3][1][1][ 0][ 4] =   9.799946e-01; eval[3][1][1][ 0][ 4] =   5.494324e-04;
  val[3][1][1][ 0][ 5] =   9.042171e-01; eval[3][1][1][ 0][ 5] =   5.922272e-04;
  val[3][1][1][ 0][ 6] =   8.477441e-01; eval[3][1][1][ 0][ 6] =   9.669578e-04;
  val[3][1][1][ 0][ 7] =   8.072430e-01; eval[3][1][1][ 0][ 7] =   1.128461e-03;
  val[3][1][1][ 0][ 8] =   7.845653e-01; eval[3][1][1][ 0][ 8] =   1.398708e-03;
  val[3][1][1][ 0][ 9] =   7.512752e-01; eval[3][1][1][ 0][ 9] =   2.697196e-03;
  val[3][1][1][ 0][10] =   7.734265e-01; eval[3][1][1][ 0][10] =   2.471225e-03;
  val[3][1][1][ 0][11] =   7.595095e-01; eval[3][1][1][ 0][11] =   3.074951e-03;
  val[3][1][1][ 0][12] =   7.482767e-01; eval[3][1][1][ 0][12] =   3.860573e-03;
  val[3][1][1][ 0][13] =   7.550643e-01; eval[3][1][1][ 0][13] =   5.120970e-03;
  val[3][1][1][ 0][14] =   7.346078e-01; eval[3][1][1][ 0][14] =   6.063243e-03;
  val[3][1][1][ 0][15] =   7.602517e-01; eval[3][1][1][ 0][15] =   8.466267e-03;
  val[3][1][1][ 0][16] =   7.401233e-01; eval[3][1][1][ 0][16] =   9.818092e-03;
  val[3][1][1][ 0][17] =   7.543403e-01; eval[3][1][1][ 0][17] =   1.282300e-02;
  val[3][1][1][ 0][18] =   7.321706e-01; eval[3][1][1][ 0][18] =   1.469270e-02;
  val[3][1][1][ 0][19] =   7.306499e-01; eval[3][1][1][ 0][19] =   1.749956e-02;
  val[3][1][1][ 0][20] =   7.578526e-01; eval[3][1][1][ 0][20] =   1.791261e-02;
  val[3][1][1][ 0][21] =   7.388377e-01; eval[3][1][1][ 0][21] =   2.278747e-02;
  val[3][1][1][ 0][22] =   7.909011e-01; eval[3][1][1][ 0][22] =   3.652901e-02;
  val[3][1][1][ 0][23] =   7.258257e-01; eval[3][1][1][ 0][23] =   4.043705e-02;
  val[3][1][1][ 0][24] =   7.248779e-01; eval[3][1][1][ 0][24] =   8.098469e-02;
  val[3][1][1][ 0][25] =   8.737104e-01; eval[3][1][1][ 0][25] =   4.410306e-02;
  val[3][1][1][ 0][26] =   7.534803e-01; eval[3][1][1][ 0][26] =   8.819127e-02;
  val[3][1][1][ 0][27] =   8.536502e-01; eval[3][1][1][ 0][27] =   1.533770e-01;
  val[3][1][1][ 0][28] =   6.763027e-01; eval[3][1][1][ 0][28] =   9.276570e-02;
  val[3][1][1][ 0][29] =   6.831598e-01; eval[3][1][1][ 0][29] =   1.126373e-01;
  //pc3dz_sigma_c1d1z1
  n_val[3][1][1][1] = 30;
  val[3][1][1][ 1][ 0] =   0.000000e+00; eval[3][1][1][ 1][ 0] =   0.000000e+00;
  val[3][1][1][ 1][ 1] =   0.000000e+00; eval[3][1][1][ 1][ 1] =   0.000000e+00;
  val[3][1][1][ 1][ 2] =   1.384965e+00; eval[3][1][1][ 1][ 2] =   5.233634e-04;
  val[3][1][1][ 1][ 3] =   1.132853e+00; eval[3][1][1][ 1][ 3] =   4.323773e-04;
  val[3][1][1][ 1][ 4] =   9.728993e-01; eval[3][1][1][ 1][ 4] =   4.981466e-04;
  val[3][1][1][ 1][ 5] =   8.989708e-01; eval[3][1][1][ 1][ 5] =   5.461527e-04;
  val[3][1][1][ 1][ 6] =   8.422431e-01; eval[3][1][1][ 1][ 6] =   8.803883e-04;
  val[3][1][1][ 1][ 7] =   8.081404e-01; eval[3][1][1][ 1][ 7] =   1.049773e-03;
  val[3][1][1][ 1][ 8] =   7.848473e-01; eval[3][1][1][ 1][ 8] =   1.280775e-03;
  val[3][1][1][ 1][ 9] =   7.556195e-01; eval[3][1][1][ 1][ 9] =   2.537713e-03;
  val[3][1][1][ 1][10] =   7.439466e-01; eval[3][1][1][ 1][10] =   3.250324e-03;
  val[3][1][1][ 1][11] =   7.366172e-01; eval[3][1][1][ 1][11] =   4.174049e-03;
  val[3][1][1][ 1][12] =   7.385658e-01; eval[3][1][1][ 1][12] =   5.558647e-03;
  val[3][1][1][ 1][13] =   7.290109e-01; eval[3][1][1][ 1][13] =   7.029556e-03;
  val[3][1][1][ 1][14] =   7.000775e-01; eval[3][1][1][ 1][14] =   8.013092e-03;
  val[3][1][1][ 1][15] =   7.330385e-01; eval[3][1][1][ 1][15] =   1.166767e-02;
  val[3][1][1][ 1][16] =   7.109639e-01; eval[3][1][1][ 1][16] =   1.347578e-02;
  val[3][1][1][ 1][17] =   7.587490e-01; eval[3][1][1][ 1][17] =   1.246553e-02;
  val[3][1][1][ 1][18] =   7.449245e-01; eval[3][1][1][ 1][18] =   1.472462e-02;
  val[3][1][1][ 1][19] =   7.344813e-01; eval[3][1][1][ 1][19] =   1.697239e-02;
  val[3][1][1][ 1][20] =   7.421974e-01; eval[3][1][1][ 1][20] =   1.592256e-02;
  val[3][1][1][ 1][21] =   7.825724e-01; eval[3][1][1][ 1][21] =   2.542091e-02;
  val[3][1][1][ 1][22] =   7.584791e-01; eval[3][1][1][ 1][22] =   2.508266e-02;
  val[3][1][1][ 1][23] =   7.884412e-01; eval[3][1][1][ 1][23] =   4.881127e-02;
  val[3][1][1][ 1][24] =   6.504496e-01; eval[3][1][1][ 1][24] =   5.478944e-02;
  val[3][1][1][ 1][25] =   6.923108e-01; eval[3][1][1][ 1][25] =   5.476458e-02;
  val[3][1][1][ 1][26] =   8.597127e-01; eval[3][1][1][ 1][26] =   7.881846e-02;
  val[3][1][1][ 1][27] =   8.874187e-01; eval[3][1][1][ 1][27] =   1.107281e-01;
  val[3][1][1][ 1][28] =   7.724206e-01; eval[3][1][1][ 1][28] =   7.501258e-02;
  val[3][1][1][ 1][29] =   7.477189e-01; eval[3][1][1][ 1][29] =   1.142635e-01;
  //pc3dz_sigma_c1d1z2
  n_val[3][1][1][2] = 30;
  val[3][1][1][ 2][ 0] =   0.000000e+00; eval[3][1][1][ 2][ 0] =   0.000000e+00;
  val[3][1][1][ 2][ 1] =   0.000000e+00; eval[3][1][1][ 2][ 1] =   0.000000e+00;
  val[3][1][1][ 2][ 2] =   1.383986e+00; eval[3][1][1][ 2][ 2] =   5.064283e-04;
  val[3][1][1][ 2][ 3] =   1.137180e+00; eval[3][1][1][ 2][ 3] =   4.419066e-04;
  val[3][1][1][ 2][ 4] =   9.910022e-01; eval[3][1][1][ 2][ 4] =   5.295851e-04;
  val[3][1][1][ 2][ 5] =   8.999574e-01; eval[3][1][1][ 2][ 5] =   7.949857e-04;
  val[3][1][1][ 2][ 6] =   8.513065e-01; eval[3][1][1][ 2][ 6] =   9.201076e-04;
  val[3][1][1][ 2][ 7] =   8.150427e-01; eval[3][1][1][ 2][ 7] =   1.106452e-03;
  val[3][1][1][ 2][ 8] =   7.946208e-01; eval[3][1][1][ 2][ 8] =   1.362737e-03;
  val[3][1][1][ 2][ 9] =   7.809697e-01; eval[3][1][1][ 2][ 9] =   1.791680e-03;
  val[3][1][1][ 2][10] =   7.668568e-01; eval[3][1][1][ 2][10] =   2.284418e-03;
  val[3][1][1][ 2][11] =   7.435684e-01; eval[3][1][1][ 2][11] =   4.409103e-03;
  val[3][1][1][ 2][12] =   7.495971e-01; eval[3][1][1][ 2][12] =   5.940862e-03;
  val[3][1][1][ 2][13] =   7.268453e-01; eval[3][1][1][ 2][13] =   7.056772e-03;
  val[3][1][1][ 2][14] =   7.335244e-01; eval[3][1][1][ 2][14] =   9.232164e-03;
  val[3][1][1][ 2][15] =   7.233515e-01; eval[3][1][1][ 2][15] =   1.124968e-02;
  val[3][1][1][ 2][16] =   7.568028e-01; eval[3][1][1][ 2][16] =   1.593210e-02;
  val[3][1][1][ 2][17] =   7.287193e-01; eval[3][1][1][ 2][17] =   1.772563e-02;
  val[3][1][1][ 2][18] =   6.858940e-01; eval[3][1][1][ 2][18] =   1.848046e-02;
  val[3][1][1][ 2][19] =   7.589198e-01; eval[3][1][1][ 2][19] =   3.005513e-02;
  val[3][1][1][ 2][20] =   7.550340e-01; eval[3][1][1][ 2][20] =   2.666581e-02;
  val[3][1][1][ 2][21] =   7.253115e-01; eval[3][1][1][ 2][21] =   3.339043e-02;
  val[3][1][1][ 2][22] =   7.841281e-01; eval[3][1][1][ 2][22] =   2.492126e-02;
  val[3][1][1][ 2][23] =   6.635451e-01; eval[3][1][1][ 2][23] =   4.663916e-02;
  val[3][1][1][ 2][24] =   5.620066e-01; eval[3][1][1][ 2][24] =   3.976775e-02;
  val[3][1][1][ 2][25] =   7.681959e-01; eval[3][1][1][ 2][25] =   3.409747e-02;
  val[3][1][1][ 2][26] =   9.024858e-01; eval[3][1][1][ 2][26] =   9.336163e-02;
  val[3][1][1][ 2][27] =   7.334190e-01; eval[3][1][1][ 2][27] =   7.641482e-02;
  val[3][1][1][ 2][28] =   7.714946e-01; eval[3][1][1][ 2][28] =   1.370985e-01;
  val[3][1][1][ 2][29] =   8.864879e-01; eval[3][1][1][ 2][29] =   1.573775e-01;
  //pc3dz_sigma_c1d1z3
  n_val[3][1][1][3] = 30;
  val[3][1][1][ 3][ 0] =   0.000000e+00; eval[3][1][1][ 3][ 0] =   0.000000e+00;
  val[3][1][1][ 3][ 1] =   0.000000e+00; eval[3][1][1][ 3][ 1] =   0.000000e+00;
  val[3][1][1][ 3][ 2] =   1.421674e+00; eval[3][1][1][ 3][ 2] =   4.437516e-04;
  val[3][1][1][ 3][ 3] =   1.134945e+00; eval[3][1][1][ 3][ 3] =   4.449863e-04;
  val[3][1][1][ 3][ 4] =   9.886974e-01; eval[3][1][1][ 3][ 4] =   5.337696e-04;
  val[3][1][1][ 3][ 5] =   9.019877e-01; eval[3][1][1][ 3][ 5] =   7.954229e-04;
  val[3][1][1][ 3][ 6] =   8.533776e-01; eval[3][1][1][ 3][ 6] =   9.093798e-04;
  val[3][1][1][ 3][ 7] =   8.194348e-01; eval[3][1][1][ 3][ 7] =   1.092644e-03;
  val[3][1][1][ 3][ 8] =   7.975655e-01; eval[3][1][1][ 3][ 8] =   1.344940e-03;
  val[3][1][1][ 3][ 9] =   7.868293e-01; eval[3][1][1][ 3][ 9] =   1.804577e-03;
  val[3][1][1][ 3][10] =   7.752053e-01; eval[3][1][1][ 3][10] =   2.326835e-03;
  val[3][1][1][ 3][11] =   7.696400e-01; eval[3][1][1][ 3][11] =   3.065069e-03;
  val[3][1][1][ 3][12] =   7.741447e-01; eval[3][1][1][ 3][12] =   4.135563e-03;
  val[3][1][1][ 3][13] =   7.727959e-01; eval[3][1][1][ 3][13] =   5.330642e-03;
  val[3][1][1][ 3][14] =   7.516987e-01; eval[3][1][1][ 3][14] =   6.436596e-03;
  val[3][1][1][ 3][15] =   7.668547e-01; eval[3][1][1][ 3][15] =   8.528849e-03;
  val[3][1][1][ 3][16] =   7.319187e-01; eval[3][1][1][ 3][16] =   1.507602e-02;
  val[3][1][1][ 3][17] =   7.554021e-01; eval[3][1][1][ 3][17] =   1.275698e-02;
  val[3][1][1][ 3][18] =   7.507228e-01; eval[3][1][1][ 3][18] =   2.499484e-02;
  val[3][1][1][ 3][19] =   7.623493e-01; eval[3][1][1][ 3][19] =   1.931651e-02;
  val[3][1][1][ 3][20] =   7.768623e-01; eval[3][1][1][ 3][20] =   3.060720e-02;
  val[3][1][1][ 3][21] =   6.883020e-01; eval[3][1][1][ 3][21] =   3.013653e-02;
  val[3][1][1][ 3][22] =   7.821185e-01; eval[3][1][1][ 3][22] =   3.682134e-02;
  val[3][1][1][ 3][23] =   7.661802e-01; eval[3][1][1][ 3][23] =   4.445274e-02;
  val[3][1][1][ 3][24] =   8.044037e-01; eval[3][1][1][ 3][24] =   4.656266e-02;
  val[3][1][1][ 3][25] =   6.184634e-01; eval[3][1][1][ 3][25] =   4.575650e-02;
  val[3][1][1][ 3][26] =   5.421266e-01; eval[3][1][1][ 3][26] =   5.267129e-02;
  val[3][1][1][ 3][27] =   7.362123e-01; eval[3][1][1][ 3][27] =   9.447360e-02;
  val[3][1][1][ 3][28] =   8.584904e-01; eval[3][1][1][ 3][28] =   2.925272e-01;
  val[3][1][1][ 3][29] =   8.889269e-01; eval[3][1][1][ 3][29] =   1.279635e-01;
  //pc3dz_sigma_c1d1z4
  n_val[3][1][1][4] = 30;
  val[3][1][1][ 4][ 0] =   0.000000e+00; eval[3][1][1][ 4][ 0] =   0.000000e+00;
  val[3][1][1][ 4][ 1] =   0.000000e+00; eval[3][1][1][ 4][ 1] =   0.000000e+00;
  val[3][1][1][ 4][ 2] =   1.398152e+00; eval[3][1][1][ 4][ 2] =   4.733706e-04;
  val[3][1][1][ 4][ 3] =   1.125515e+00; eval[3][1][1][ 4][ 3] =   5.047723e-04;
  val[3][1][1][ 4][ 4] =   9.998131e-01; eval[3][1][1][ 4][ 4] =   6.472048e-04;
  val[3][1][1][ 4][ 5] =   9.026416e-01; eval[3][1][1][ 4][ 5] =   9.545017e-04;
  val[3][1][1][ 4][ 6] =   8.518552e-01; eval[3][1][1][ 4][ 6] =   1.103157e-03;
  val[3][1][1][ 4][ 7] =   8.208829e-01; eval[3][1][1][ 4][ 7] =   1.331730e-03;
  val[3][1][1][ 4][ 8] =   8.031020e-01; eval[3][1][1][ 4][ 8] =   1.644797e-03;
  val[3][1][1][ 4][ 9] =   7.848392e-01; eval[3][1][1][ 4][ 9] =   2.145227e-03;
  val[3][1][1][ 4][10] =   7.827903e-01; eval[3][1][1][ 4][10] =   2.879704e-03;
  val[3][1][1][ 4][11] =   7.741157e-01; eval[3][1][1][ 4][11] =   3.787719e-03;
  val[3][1][1][ 4][12] =   7.650527e-01; eval[3][1][1][ 4][12] =   4.869097e-03;
  val[3][1][1][ 4][13] =   7.593330e-01; eval[3][1][1][ 4][13] =   6.258512e-03;
  val[3][1][1][ 4][14] =   7.569962e-01; eval[3][1][1][ 4][14] =   7.957996e-03;
  val[3][1][1][ 4][15] =   7.457255e-01; eval[3][1][1][ 4][15] =   9.573821e-03;
  val[3][1][1][ 4][16] =   7.548810e-01; eval[3][1][1][ 4][16] =   1.269227e-02;
  val[3][1][1][ 4][17] =   7.313753e-01; eval[3][1][1][ 4][17] =   1.413281e-02;
  val[3][1][1][ 4][18] =   7.523099e-01; eval[3][1][1][ 4][18] =   1.879201e-02;
  val[3][1][1][ 4][19] =   7.779143e-01; eval[3][1][1][ 4][19] =   2.548673e-02;
  val[3][1][1][ 4][20] =   6.823581e-01; eval[3][1][1][ 4][20] =   2.575521e-02;
  val[3][1][1][ 4][21] =   6.522504e-01; eval[3][1][1][ 4][21] =   3.241289e-02;
  val[3][1][1][ 4][22] =   7.071612e-01; eval[3][1][1][ 4][22] =   3.386587e-02;
  val[3][1][1][ 4][23] =   7.961195e-01; eval[3][1][1][ 4][23] =   4.389725e-02;
  val[3][1][1][ 4][24] =   8.167325e-01; eval[3][1][1][ 4][24] =   6.091614e-02;
  val[3][1][1][ 4][25] =   6.845940e-01; eval[3][1][1][ 4][25] =   6.483919e-02;
  val[3][1][1][ 4][26] =   7.951834e-01; eval[3][1][1][ 4][26] =   5.919466e-02;
  val[3][1][1][ 4][27] =   5.872346e-01; eval[3][1][1][ 4][27] =   1.105886e-01;
  val[3][1][1][ 4][28] =   2.457924e-01; eval[3][1][1][ 4][28] =   1.270049e+00;
  val[3][1][1][ 4][29] =   6.660695e-01; eval[3][1][1][ 4][29] =   2.499653e-01;
  //pc3dz_sigma_c1d1z5
  n_val[3][1][1][5] = 30;
  val[3][1][1][ 5][ 0] =   0.000000e+00; eval[3][1][1][ 5][ 0] =   0.000000e+00;
  val[3][1][1][ 5][ 1] =   0.000000e+00; eval[3][1][1][ 5][ 1] =   0.000000e+00;
  val[3][1][1][ 5][ 2] =   1.357345e+00; eval[3][1][1][ 5][ 2] =   5.540767e-04;
  val[3][1][1][ 5][ 3] =   1.108479e+00; eval[3][1][1][ 5][ 3] =   4.832860e-04;
  val[3][1][1][ 5][ 4] =   9.766133e-01; eval[3][1][1][ 5][ 4] =   5.767762e-04;
  val[3][1][1][ 5][ 5] =   8.801352e-01; eval[3][1][1][ 5][ 5] =   8.307121e-04;
  val[3][1][1][ 5][ 6] =   8.349739e-01; eval[3][1][1][ 5][ 6] =   9.694416e-04;
  val[3][1][1][ 5][ 7] =   8.024296e-01; eval[3][1][1][ 5][ 7] =   1.163010e-03;
  val[3][1][1][ 5][ 8] =   7.820871e-01; eval[3][1][1][ 5][ 8] =   1.446174e-03;
  val[3][1][1][ 5][ 9] =   7.739969e-01; eval[3][1][1][ 5][ 9] =   1.967308e-03;
  val[3][1][1][ 5][10] =   7.679169e-01; eval[3][1][1][ 5][10] =   2.624048e-03;
  val[3][1][1][ 5][11] =   7.670618e-01; eval[3][1][1][ 5][11] =   3.523193e-03;
  val[3][1][1][ 5][12] =   7.489355e-01; eval[3][1][1][ 5][12] =   4.389293e-03;
  val[3][1][1][ 5][13] =   7.521880e-01; eval[3][1][1][ 5][13] =   5.762162e-03;
  val[3][1][1][ 5][14] =   7.482206e-01; eval[3][1][1][ 5][14] =   7.250785e-03;
  val[3][1][1][ 5][15] =   7.413043e-01; eval[3][1][1][ 5][15] =   9.139025e-03;
  val[3][1][1][ 5][16] =   7.570246e-01; eval[3][1][1][ 5][16] =   1.193178e-02;
  val[3][1][1][ 5][17] =   7.310655e-01; eval[3][1][1][ 5][17] =   1.355818e-02;
  val[3][1][1][ 5][18] =   7.827968e-01; eval[3][1][1][ 5][18] =   2.008031e-02;
  val[3][1][1][ 5][19] =   7.623630e-01; eval[3][1][1][ 5][19] =   2.261839e-02;
  val[3][1][1][ 5][20] =   7.468322e-01; eval[3][1][1][ 5][20] =   1.960022e-02;
  val[3][1][1][ 5][21] =   7.281704e-01; eval[3][1][1][ 5][21] =   2.592434e-02;
  val[3][1][1][ 5][22] =   7.215943e-01; eval[3][1][1][ 5][22] =   3.307785e-02;
  val[3][1][1][ 5][23] =   8.111191e-01; eval[3][1][1][ 5][23] =   6.249418e-02;
  val[3][1][1][ 5][24] =   7.051615e-01; eval[3][1][1][ 5][24] =   5.317580e-02;
  val[3][1][1][ 5][25] =   6.654813e-01; eval[3][1][1][ 5][25] =   5.900200e-02;
  val[3][1][1][ 5][26] =   4.789412e-01; eval[3][1][1][ 5][26] =   9.072694e-02;
  val[3][1][1][ 5][27] =   8.643196e-01; eval[3][1][1][ 5][27] =   1.084479e-01;
  val[3][1][1][ 5][28] =   8.454657e-01; eval[3][1][1][ 5][28] =   1.488779e-01;
  val[3][1][1][ 5][29] =   5.959408e-01; eval[3][1][1][ 5][29] =   9.961421e-02;
  //pc3dz_sigma_c1d1z6
  n_val[3][1][1][6] = 30;
  val[3][1][1][ 6][ 0] =   0.000000e+00; eval[3][1][1][ 6][ 0] =   0.000000e+00;
  val[3][1][1][ 6][ 1] =   0.000000e+00; eval[3][1][1][ 6][ 1] =   0.000000e+00;
  val[3][1][1][ 6][ 2] =   1.408912e+00; eval[3][1][1][ 6][ 2] =   5.277206e-04;
  val[3][1][1][ 6][ 3] =   1.150217e+00; eval[3][1][1][ 6][ 3] =   4.481705e-04;
  val[3][1][1][ 6][ 4] =   1.011152e+00; eval[3][1][1][ 6][ 4] =   5.587214e-04;
  val[3][1][1][ 6][ 5] =   9.341265e-01; eval[3][1][1][ 6][ 5] =   6.034625e-04;
  val[3][1][1][ 6][ 6] =   8.607258e-01; eval[3][1][1][ 6][ 6] =   9.403222e-04;
  val[3][1][1][ 6][ 7] =   8.298968e-01; eval[3][1][1][ 6][ 7] =   1.145169e-03;
  val[3][1][1][ 6][ 8] =   8.078690e-01; eval[3][1][1][ 6][ 8] =   1.409704e-03;
  val[3][1][1][ 6][ 9] =   7.931453e-01; eval[3][1][1][ 6][ 9] =   1.867963e-03;
  val[3][1][1][ 6][10] =   7.785374e-01; eval[3][1][1][ 6][10] =   2.431047e-03;
  val[3][1][1][ 6][11] =   7.737913e-01; eval[3][1][1][ 6][11] =   3.209201e-03;
  val[3][1][1][ 6][12] =   7.693815e-01; eval[3][1][1][ 6][12] =   4.232026e-03;
  val[3][1][1][ 6][13] =   7.603779e-01; eval[3][1][1][ 6][13] =   5.373120e-03;
  val[3][1][1][ 6][14] =   7.649070e-01; eval[3][1][1][ 6][14] =   7.047553e-03;
  val[3][1][1][ 6][15] =   7.511727e-01; eval[3][1][1][ 6][15] =   8.441306e-03;
  val[3][1][1][ 6][16] =   7.577598e-01; eval[3][1][1][ 6][16] =   1.078198e-02;
  val[3][1][1][ 6][17] =   7.286638e-01; eval[3][1][1][ 6][17] =   1.219175e-02;
  val[3][1][1][ 6][18] =   7.643768e-01; eval[3][1][1][ 6][18] =   1.702184e-02;
  val[3][1][1][ 6][19] =   7.790027e-01; eval[3][1][1][ 6][19] =   2.123584e-02;
  val[3][1][1][ 6][20] =   7.488374e-01; eval[3][1][1][ 6][20] =   1.831678e-02;
  val[3][1][1][ 6][21] =   7.705460e-01; eval[3][1][1][ 6][21] =   2.772561e-02;
  val[3][1][1][ 6][22] =   7.615596e-01; eval[3][1][1][ 6][22] =   2.818311e-02;
  val[3][1][1][ 6][23] =   8.314942e-01; eval[3][1][1][ 6][23] =   2.894058e-02;
  val[3][1][1][ 6][24] =   9.430437e-01; eval[3][1][1][ 6][24] =   1.098636e-01;
  val[3][1][1][ 6][25] =   7.150904e-01; eval[3][1][1][ 6][25] =   4.602414e-02;
  val[3][1][1][ 6][26] =   5.303619e-01; eval[3][1][1][ 6][26] =   1.172279e-01;
  val[3][1][1][ 6][27] =   7.117009e-01; eval[3][1][1][ 6][27] =   5.960369e-02;
  val[3][1][1][ 6][28] =   9.237934e-01; eval[3][1][1][ 6][28] =   1.487316e-01;
  val[3][1][1][ 6][29] =   5.380132e-01; eval[3][1][1][ 6][29] =   1.119338e-01;
  //pc3dz_sigma_c1d1z7
  n_val[3][1][1][7] = 30;
  val[3][1][1][ 7][ 0] =   0.000000e+00; eval[3][1][1][ 7][ 0] =   0.000000e+00;
  val[3][1][1][ 7][ 1] =   0.000000e+00; eval[3][1][1][ 7][ 1] =   0.000000e+00;
  val[3][1][1][ 7][ 2] =   1.383614e+00; eval[3][1][1][ 7][ 2] =   4.864632e-04;
  val[3][1][1][ 7][ 3] =   1.101676e+00; eval[3][1][1][ 7][ 3] =   5.261720e-04;
  val[3][1][1][ 7][ 4] =   9.895297e-01; eval[3][1][1][ 7][ 4] =   5.093777e-04;
  val[3][1][1][ 7][ 5] =   9.177821e-01; eval[3][1][1][ 7][ 5] =   5.537657e-04;
  val[3][1][1][ 7][ 6] =   8.540163e-01; eval[3][1][1][ 7][ 6] =   8.983177e-04;
  val[3][1][1][ 7][ 7] =   8.214227e-01; eval[3][1][1][ 7][ 7] =   1.077161e-03;
  val[3][1][1][ 7][ 8] =   8.010673e-01; eval[3][1][1][ 7][ 8] =   1.318714e-03;
  val[3][1][1][ 7][ 9] =   7.846933e-01; eval[3][1][1][ 7][ 9] =   1.725636e-03;
  val[3][1][1][ 7][10] =   7.776175e-01; eval[3][1][1][ 7][10] =   2.299179e-03;
  val[3][1][1][ 7][11] =   7.754233e-01; eval[3][1][1][ 7][11] =   3.063745e-03;
  val[3][1][1][ 7][12] =   7.458668e-01; eval[3][1][1][ 7][12] =   5.854227e-03;
  val[3][1][1][ 7][13] =   7.444261e-01; eval[3][1][1][ 7][13] =   7.677944e-03;
  val[3][1][1][ 7][14] =   7.521834e-01; eval[3][1][1][ 7][14] =   1.021813e-02;
  val[3][1][1][ 7][15] =   7.475808e-01; eval[3][1][1][ 7][15] =   1.272006e-02;
  val[3][1][1][ 7][16] =   7.182475e-01; eval[3][1][1][ 7][16] =   1.428661e-02;
  val[3][1][1][ 7][17] =   7.556524e-01; eval[3][1][1][ 7][17] =   2.055437e-02;
  val[3][1][1][ 7][18] =   7.448239e-01; eval[3][1][1][ 7][18] =   2.425184e-02;
  val[3][1][1][ 7][19] =   7.444181e-01; eval[3][1][1][ 7][19] =   2.963181e-02;
  val[3][1][1][ 7][20] =   7.266738e-01; eval[3][1][1][ 7][20] =   2.530888e-02;
  val[3][1][1][ 7][21] =   7.495489e-01; eval[3][1][1][ 7][21] =   4.011096e-02;
  val[3][1][1][ 7][22] =   7.879924e-01; eval[3][1][1][ 7][22] =   2.694567e-02;
  val[3][1][1][ 7][23] =   8.073396e-01; eval[3][1][1][ 7][23] =   3.696434e-02;
  val[3][1][1][ 7][24] =   7.844657e-01; eval[3][1][1][ 7][24] =   4.817731e-02;
  val[3][1][1][ 7][25] =   6.087061e-01; eval[3][1][1][ 7][25] =   6.019478e-02;
  val[3][1][1][ 7][26] =   7.547536e-01; eval[3][1][1][ 7][26] =   5.634970e-02;
  val[3][1][1][ 7][27] =   7.551698e-01; eval[3][1][1][ 7][27] =   7.988945e-02;
  val[3][1][1][ 7][28] =   6.056973e-01; eval[3][1][1][ 7][28] =   1.224476e-01;
  val[3][1][1][ 7][29] =   2.778650e-01; eval[3][1][1][ 7][29] =   2.039139e+00;
  //pc3dz_sigma_c1d1z8
  n_val[3][1][1][8] = 30;
  val[3][1][1][ 8][ 0] =   0.000000e+00; eval[3][1][1][ 8][ 0] =   0.000000e+00;
  val[3][1][1][ 8][ 1] =   0.000000e+00; eval[3][1][1][ 8][ 1] =   0.000000e+00;
  val[3][1][1][ 8][ 2] =   1.388998e+00; eval[3][1][1][ 8][ 2] =   4.671929e-04;
  val[3][1][1][ 8][ 3] =   1.117212e+00; eval[3][1][1][ 8][ 3] =   5.383162e-04;
  val[3][1][1][ 8][ 4] =   1.008030e+00; eval[3][1][1][ 8][ 4] =   5.342096e-04;
  val[3][1][1][ 8][ 5] =   9.408533e-01; eval[3][1][1][ 8][ 5] =   6.028659e-04;
  val[3][1][1][ 8][ 6] =   8.790044e-01; eval[3][1][1][ 8][ 6] =   9.936459e-04;
  val[3][1][1][ 8][ 7] =   8.529257e-01; eval[3][1][1][ 8][ 7] =   1.243625e-03;
  val[3][1][1][ 8][ 8] =   8.467615e-01; eval[3][1][1][ 8][ 8] =   1.104992e-03;
  val[3][1][1][ 8][ 9] =   8.266504e-01; eval[3][1][1][ 8][ 9] =   2.039233e-03;
  val[3][1][1][ 8][10] =   8.061208e-01; eval[3][1][1][ 8][10] =   2.570199e-03;
  val[3][1][1][ 8][11] =   8.013744e-01; eval[3][1][1][ 8][11] =   3.340231e-03;
  val[3][1][1][ 8][12] =   7.991546e-01; eval[3][1][1][ 8][12] =   4.447702e-03;
  val[3][1][1][ 8][13] =   8.066322e-01; eval[3][1][1][ 8][13] =   5.978482e-03;
  val[3][1][1][ 8][14] =   7.918374e-01; eval[3][1][1][ 8][14] =   7.357677e-03;
  val[3][1][1][ 8][15] =   8.005733e-01; eval[3][1][1][ 8][15] =   9.678365e-03;
  val[3][1][1][ 8][16] =   7.908008e-01; eval[3][1][1][ 8][16] =   1.180133e-02;
  val[3][1][1][ 8][17] =   7.717383e-01; eval[3][1][1][ 8][17] =   1.379769e-02;
  val[3][1][1][ 8][18] =   8.109226e-01; eval[3][1][1][ 8][18] =   1.896033e-02;
  val[3][1][1][ 8][19] =   7.845965e-01; eval[3][1][1][ 8][19] =   2.222249e-02;
  val[3][1][1][ 8][20] =   7.896549e-01; eval[3][1][1][ 8][20] =   2.055451e-02;
  val[3][1][1][ 8][21] =   8.270098e-01; eval[3][1][1][ 8][21] =   2.360077e-02;
  val[3][1][1][ 8][22] =   8.878403e-01; eval[3][1][1][ 8][22] =   2.602867e-02;
  val[3][1][1][ 8][23] =   8.585747e-01; eval[3][1][1][ 8][23] =   3.105145e-02;
  val[3][1][1][ 8][24] =   9.081360e-01; eval[3][1][1][ 8][24] =   4.715859e-02;
  val[3][1][1][ 8][25] =   9.048312e-01; eval[3][1][1][ 8][25] =   6.668288e-02;
  val[3][1][1][ 8][26] =   8.826753e-01; eval[3][1][1][ 8][26] =   5.607793e-02;
  val[3][1][1][ 8][27] =   9.477885e-01; eval[3][1][1][ 8][27] =   8.806289e-02;
  val[3][1][1][ 8][28] =   6.037382e-01; eval[3][1][1][ 8][28] =   1.254534e-01;
  val[3][1][1][ 8][29] =   6.257980e-01; eval[3][1][1][ 8][29] =   1.093400e-01;
  //pc3dz_sigma_c1d1z9
  n_val[3][1][1][9] = 30;
  val[3][1][1][ 9][ 0] =   0.000000e+00; eval[3][1][1][ 9][ 0] =   0.000000e+00;
  val[3][1][1][ 9][ 1] =   0.000000e+00; eval[3][1][1][ 9][ 1] =   0.000000e+00;
  val[3][1][1][ 9][ 2] =   1.404896e+00; eval[3][1][1][ 9][ 2] =   5.055454e-04;
  val[3][1][1][ 9][ 3] =   1.149313e+00; eval[3][1][1][ 9][ 3] =   4.691123e-04;
  val[3][1][1][ 9][ 4] =   1.015846e+00; eval[3][1][1][ 9][ 4] =   6.031870e-04;
  val[3][1][1][ 9][ 5] =   9.272164e-01; eval[3][1][1][ 9][ 5] =   9.179320e-04;
  val[3][1][1][ 9][ 6] =   8.843119e-01; eval[3][1][1][ 9][ 6] =   1.123613e-03;
  val[3][1][1][ 9][ 7] =   8.680449e-01; eval[3][1][1][ 9][ 7] =   1.005123e-03;
  val[3][1][1][ 9][ 8] =   8.268560e-01; eval[3][1][1][ 9][ 8] =   1.634471e-03;
  val[3][1][1][ 9][ 9] =   8.098318e-01; eval[3][1][1][ 9][ 9] =   2.106058e-03;
  val[3][1][1][ 9][10] =   7.949744e-01; eval[3][1][1][ 9][10] =   2.754387e-03;
  val[3][1][1][ 9][11] =   7.898945e-01; eval[3][1][1][ 9][11] =   3.645351e-03;
  val[3][1][1][ 9][12] =   8.001504e-01; eval[3][1][1][ 9][12] =   4.987991e-03;
  val[3][1][1][ 9][13] =   7.917872e-01; eval[3][1][1][ 9][13] =   6.324472e-03;
  val[3][1][1][ 9][14] =   7.469093e-01; eval[3][1][1][ 9][14] =   1.107788e-02;
  val[3][1][1][ 9][15] =   7.405487e-01; eval[3][1][1][ 9][15] =   1.294350e-02;
  val[3][1][1][ 9][16] =   7.883269e-01; eval[3][1][1][ 9][16] =   1.944458e-02;
  val[3][1][1][ 9][17] =   7.554519e-01; eval[3][1][1][ 9][17] =   2.249188e-02;
  val[3][1][1][ 9][18] =   7.252470e-01; eval[3][1][1][ 9][18] =   2.443208e-02;
  val[3][1][1][ 9][19] =   8.047326e-01; eval[3][1][1][ 9][19] =   2.540016e-02;
  val[3][1][1][ 9][20] =   7.768347e-01; eval[3][1][1][ 9][20] =   2.097776e-02;
  val[3][1][1][ 9][21] =   7.505910e-01; eval[3][1][1][ 9][21] =   4.374987e-02;
  val[3][1][1][ 9][22] =   6.708326e-01; eval[3][1][1][ 9][22] =   4.198924e-02;
  val[3][1][1][ 9][23] =   8.479696e-01; eval[3][1][1][ 9][23] =   4.829039e-02;
  val[3][1][1][ 9][24] =   8.974705e-01; eval[3][1][1][ 9][24] =   4.516017e-02;
  val[3][1][1][ 9][25] =   8.796172e-01; eval[3][1][1][ 9][25] =   8.427374e-02;
  val[3][1][1][ 9][26] =   7.188633e-01; eval[3][1][1][ 9][26] =   9.995564e-02;
  val[3][1][1][ 9][27] =   6.055840e-01; eval[3][1][1][ 9][27] =   1.036880e-01;
  val[3][1][1][ 9][28] =   1.075611e+00; eval[3][1][1][ 9][28] =   3.001827e-01;
  val[3][1][1][ 9][29] =   6.283182e-01; eval[3][1][1][ 9][29] =   1.789091e-01;

  ////////////////////////////////
  for(int ipar=0; ipar<4; ipar++){
    for(int ic=0; ic<2; ic++){
      for(int iarm=0; iarm<2; iarm++){
        for(int ized=0; ized<10; ized++){
          TGraphErrors* g = new TGraphErrors(n_val[ipar][ic][iarm][ized], pt, val[ipar][ic][iarm][ized], zero, eval[ipar][ic][iarm][ized]);

          if(ipar==0)      m_g_dphi_mean[ic][iarm][ized]  = g;
          else if(ipar==1) m_g_dphi_sigma[ic][iarm][ized] = g;
          else if(ipar==2) m_g_dz_mean[ic][iarm][ized]    = g;
          else             m_g_dz_sigma[ic][iarm][ized]   = g;

        }
      }
    }
  }

}
