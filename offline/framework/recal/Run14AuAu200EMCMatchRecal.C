#include "Run14AuAu200EMCMatchRecal.h"

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

Run14AuAu200EMCMatchRecal::Run14AuAu200EMCMatchRecal(const char* name): 
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

Run14AuAu200EMCMatchRecal::~Run14AuAu200EMCMatchRecal()
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

int Run14AuAu200EMCMatchRecal::Init(PHCompositeNode *topNode)
{
  return 0;
}


int Run14AuAu200EMCMatchRecal::InitRun(PHCompositeNode *topNode)
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

int Run14AuAu200EMCMatchRecal::isValidRun(const int runno) const
{

  // Run14AuAu 200 GeV
  // to check the run range, see https://www.phenix.bnl.gov/WWW/run/14/history.html and run control log
  if (405639 <= runno && runno <=414988)
  {
    return 1;
  }

  return 0;
}

int Run14AuAu200EMCMatchRecal::process_event(PHCompositeNode *topNode)
{
  //cout<<"EMCMatchRecal::process_event"<<" :"<<inputnodename.c_str()<<":"<<endl;
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

int Run14AuAu200EMCMatchRecal::Calibrate()
{
  //cout<<"EMCMatchRecal::Calibrate "<<m_cnt->get_npart()<<endl;
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
        sngltrk->isImplemented(sngltrk->get_emcdphi()) &&
        sngltrk->isImplemented(sngltrk->get_emcdz()) &&
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

      float ecore  = sngltrk->get_ecore();
      short emcsect = sngltrk->get_sect();

      // Matching variables
      double emcdphi  = sngltrk->get_emcdphi();
      double emcdz    = sngltrk->get_emcdz();
      double semcdphi = sngltrk->get_semcdphi();
      double semcdz   = sngltrk->get_semcdz();

      //Calculate the new variables
      // ------------------
      static const int NZED=10;

      int icharge = (charge == 1) ? 0 : 1;
      int iarm    = arm;
      int ized    = (int)(NZED * (zed + 75.0) / 150.0);

      float emcsdz   = -999., emcsdphi  = -999.;
      float semcsdz  = -999., semcsdphi = -999.;

      bool isChgOK, isArmOK, isZedOK;
      if((isChgOK=IsChargeRangeOK(icharge)) &&
         (isArmOK=IsArmRangeOK(iarm))       &&
         (isZedOK=IsZedRangeOK(ized))       )      
      {
        if(emcdphi>-998 && emcdz>-998) {
          emcdz = correctEmcdzByZed(emcdz, icharge, iarm, emcsect, ecore, zed);

          emcsdz   = Calc(emcdz,   pt, icharge, iarm, ized, 0);
          emcsdphi = Calc(emcdphi, pt, icharge, iarm, ized, 1);
        }
        if(semcdphi>-998 && semcdz>-998) {
          semcdz = correctEmcdzByZed(semcdz, icharge, iarm, emcsect, ecore, zed);

          semcsdz   = Calc(semcdz,   pt, icharge, iarm, ized, 0);
          semcsdphi = Calc(semcdphi, pt, icharge, iarm, ized, 1);
        }
      }
      else {
        if(verbosity>3){
          if(!isChgOK) cerr<<"EMC matching Calc : out of range : icharge = "<<icharge<<", charge="<<charge<<endl;
          if(!isArmOK) cerr<<"EMC matching Calc : out of range : iarm = "   <<iarm   <<", phi="   <<phi<<endl;
          if(!isZedOK) cerr<<"EMC matching Calc : out of range : ized = "   <<ized   <<", zed="   <<zed<<endl;
        }
      }
    

      //Set the new variables
      sngltrk->set_emcsdphi(emcsdphi);
      sngltrk->set_emcsdz(  emcsdz);
      sngltrk->set_semcsdphi(semcsdphi);
      sngltrk->set_semcsdz(  semcsdz);
      //--cout<<"EMC calibrate : "<<emcdphi<<" "<<emcdz<<" : "<<emcsdphi<<" "<<emcsdz<<" "<<pt<<endl;
    }

  return EVENT_OK;
}


double Run14AuAu200EMCMatchRecal::CalcMeanDZ(double pt, int icharge, int iarm, int ized)
{
  TGraphErrors *g = m_g_dz_mean[icharge][iarm][ized];
  if(NULL==g) return -19999.;

  double mean=0;

  if(0.65<=pt&&pt<0.8){
    mean = g->Eval(pt);
  }
  else if(0.8<=pt){ // use fit function
    TF1* func = m_fn_dz_mean[icharge][iarm][ized];
    if(NULL==func) return -29999.;

    mean = func->Eval(pt);
  }
  else { // pt<0.65
    TF1* func = m_fn_dz_mean[icharge][iarm][ized];
    if(NULL==func) return -29999.;

    pt = 0.5;
    mean = func->Eval(pt);
  }
  
  return mean;
}

double Run14AuAu200EMCMatchRecal::CalcSigmaDZ(double pt, int icharge, int iarm, int ized)
{
  double sigma=0;

  if(0.65<=pt&&pt<0.8){
    TGraphErrors *g = m_g_dz_sigma[icharge][iarm][ized];
    if(NULL==g) return -19997.;

    sigma = g->Eval(pt);
  }
  else if(0.8<=pt){ // use fit function
    TF1* func = m_fn_dz_sigma[icharge][iarm][ized];
    if(NULL==func) return -29997;

    sigma = func->Eval(pt);
  }
  else { // pt<0.65 use fixed pt value
    TF1* func = m_fn_dz_sigma[icharge][iarm][ized];
    if(NULL==func) return -29997;

    pt = 0.5; // use fixed pt value
    sigma = func->Eval(pt);
  }
  
  return sigma;
}

double Run14AuAu200EMCMatchRecal::CalcMeanDPHI(double pt, int icharge, int iarm, int ized)
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

double Run14AuAu200EMCMatchRecal::CalcSigmaDPHI(double pt, int icharge, int iarm, int ized)
{
//  if(NULL==g) return -19996;

  TF1* func = m_fn_dphi_sigma[icharge][iarm][ized];
  if(NULL==func) return -29996;

  double sigma= func->Eval(pt);
  
  return sigma;
}

double Run14AuAu200EMCMatchRecal::Calc(double raw, double pt, int icharge, int iarm, int ized, int DPHI_DZ)
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

  if( isinf(sigmalized) ) {
    cout<<"EMC "<<(DPHI_DZ==0?"DZ":"DPHI")<<"val : "<<sigmalized<<" "<<raw<<" "<<mean<<" "<<sigma<<" : "<<pt<<endl;
  }

  return sigmalized;
}


void Run14AuAu200EMCMatchRecal::readParameters()
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

double Run14AuAu200EMCMatchRecal::correctEmcdzByZed(
         float emcdz, int icharge, int iarm, 
         short emcsect, float ecore, float zed)
{
  static const float par[2][2][4][2] = // [c][arm][sec][par]
   {{{{ 0.0589499, 0.0120912 },{ 0.0554228, 0.0120941 },{ 0.156365,  0.00896124},{ 0.131519,  0.0129233}},
     {{-0.163611, -0.0357714}, {-0.23478,  -0.0362698}, {-0.0443738, 0.0146547}, {-0.0447539, 0.0132018}}},
    {{{ 0.119707,  0.0189475}, { 0.071192,  0.017593 }, { 0.135736,  0.0193715}, { 0.0889046, 0.0189582}},
     {{-0.120793, -0.0370052}, {-0.125249, -0.0360596}, { 0.0374725, 0.0187303}, {-0.0104017, 0.0197221}} }};

  int chrgId = icharge;
  int armId  = iarm;

  bool isPbGl = (armId==1)&&(emcsect<2);

  bool isMIP = ( (isPbGl&&(ecore<0.2)) || ((!isPbGl)&&fabs(ecore-0.3)<0.1) );

  double updated_emcdz = emcdz;
  if(isMIP) {
    double par0 = par[chrgId][armId][emcsect][0];
    double par1 = par[chrgId][armId][emcsect][1];
    updated_emcdz = (emcdz - (par0 + par1 * zed));
  }

  return updated_emcdz;
}

void Run14AuAu200EMCMatchRecal::InitParameters()
{
  initParMatchFunc();
  initParMatchGraph();
}




void Run14AuAu200EMCMatchRecal::initParMatchFunc()
{
  TString sfname[4][2][2][10];
  TString sfunc [4][2][2][10];
  int     nfpar[ 4][2][2][10];
  double  fpar[  4][2][2][10][5];

  //emcdphi_mean_c0d0z0
  sfname[0][0][0][0] = "m_fn_dphi_mean_c0d0z0";
  sfunc[0][0][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][0] = 3;
  fpar[0][0][0][ 0][0] =  -1.449746e-03; fpar[0][0][0][ 0][1] =   3.710252e-03; fpar[0][0][0][ 0][2] =  -1.570581e-03; 

  //emcdphi_mean_c0d0z1
  sfname[0][0][0][1] = "m_fn_dphi_mean_c0d0z1";
  sfunc[0][0][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][1] = 3;
  fpar[0][0][0][ 1][0] =  -1.617187e-03; fpar[0][0][0][ 1][1] =   4.214717e-03; fpar[0][0][0][ 1][2] =  -1.378920e-03; 

  //emcdphi_mean_c0d0z2
  sfname[0][0][0][2] = "m_fn_dphi_mean_c0d0z2";
  sfunc[0][0][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][2] = 3;
  fpar[0][0][0][ 2][0] =  -1.170426e-03; fpar[0][0][0][ 2][1] =   3.773764e-03; fpar[0][0][0][ 2][2] =  -9.284485e-04; 

  //emcdphi_mean_c0d0z3
  sfname[0][0][0][3] = "m_fn_dphi_mean_c0d0z3";
  sfunc[0][0][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][3] = 3;
  fpar[0][0][0][ 3][0] =  -1.241655e-03; fpar[0][0][0][ 3][1] =   4.061133e-03; fpar[0][0][0][ 3][2] =  -9.465192e-04; 

  //emcdphi_mean_c0d0z4
  sfname[0][0][0][4] = "m_fn_dphi_mean_c0d0z4";
  sfunc[0][0][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][4] = 3;
  fpar[0][0][0][ 4][0] =  -1.378782e-03; fpar[0][0][0][ 4][1] =   4.481714e-03; fpar[0][0][0][ 4][2] =  -1.042614e-03; 

  //emcdphi_mean_c0d0z5
  sfname[0][0][0][5] = "m_fn_dphi_mean_c0d0z5";
  sfunc[0][0][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][5] = 3;
  fpar[0][0][0][ 5][0] =  -1.137334e-03; fpar[0][0][0][ 5][1] =   2.884089e-03; fpar[0][0][0][ 5][2] =  -2.687692e-04; 

  //emcdphi_mean_c0d0z6
  sfname[0][0][0][6] = "m_fn_dphi_mean_c0d0z6";
  sfunc[0][0][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][6] = 3;
  fpar[0][0][0][ 6][0] =  -1.020385e-03; fpar[0][0][0][ 6][1] =   2.831419e-03; fpar[0][0][0][ 6][2] =  -4.325501e-04; 

  //emcdphi_mean_c0d0z7
  sfname[0][0][0][7] = "m_fn_dphi_mean_c0d0z7";
  sfunc[0][0][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][7] = 3;
  fpar[0][0][0][ 7][0] =  -8.398040e-04; fpar[0][0][0][ 7][1] =   1.793922e-03; fpar[0][0][0][ 7][2] =  -1.106168e-04; 

  //emcdphi_mean_c0d0z8
  sfname[0][0][0][8] = "m_fn_dphi_mean_c0d0z8";
  sfunc[0][0][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][8] = 3;
  fpar[0][0][0][ 8][0] =  -8.984889e-04; fpar[0][0][0][ 8][1] =   9.269409e-04; fpar[0][0][0][ 8][2] =   5.603348e-05; 

  //emcdphi_mean_c0d0z9
  sfname[0][0][0][9] = "m_fn_dphi_mean_c0d0z9";
  sfunc[0][0][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][0][9] = 3;
  fpar[0][0][0][ 9][0] =  -1.132838e-03; fpar[0][0][0][ 9][1] =   1.034210e-03; fpar[0][0][0][ 9][2] =  -3.693023e-04; 

  //emcdphi_mean_c0d1z0
  sfname[0][0][1][0] = "m_fn_dphi_mean_c0d1z0";
  sfunc[0][0][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][0] = 3;
  fpar[0][0][1][ 0][0] =   8.685324e-04; fpar[0][0][1][ 0][1] =   5.384601e-03; fpar[0][0][1][ 0][2] =  -1.229845e-03; 

  //emcdphi_mean_c0d1z1
  sfname[0][0][1][1] = "m_fn_dphi_mean_c0d1z1";
  sfunc[0][0][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][1] = 3;
  fpar[0][0][1][ 1][0] =   7.547099e-04; fpar[0][0][1][ 1][1] =   5.764537e-03; fpar[0][0][1][ 1][2] =  -1.126623e-03; 

  //emcdphi_mean_c0d1z2
  sfname[0][0][1][2] = "m_fn_dphi_mean_c0d1z2";
  sfunc[0][0][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][2] = 3;
  fpar[0][0][1][ 2][0] =   6.254489e-04; fpar[0][0][1][ 2][1] =   6.104811e-03; fpar[0][0][1][ 2][2] =  -9.634239e-04; 

  //emcdphi_mean_c0d1z3
  sfname[0][0][1][3] = "m_fn_dphi_mean_c0d1z3";
  sfunc[0][0][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][3] = 3;
  fpar[0][0][1][ 3][0] =   8.341112e-04; fpar[0][0][1][ 3][1] =   5.984952e-03; fpar[0][0][1][ 3][2] =  -8.116600e-04; 

  //emcdphi_mean_c0d1z4
  sfname[0][0][1][4] = "m_fn_dphi_mean_c0d1z4";
  sfunc[0][0][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][4] = 3;
  fpar[0][0][1][ 4][0] =   3.838241e-04; fpar[0][0][1][ 4][1] =   7.002858e-03; fpar[0][0][1][ 4][2] =  -1.168582e-03; 

  //emcdphi_mean_c0d1z5
  sfname[0][0][1][5] = "m_fn_dphi_mean_c0d1z5";
  sfunc[0][0][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][5] = 3;
  fpar[0][0][1][ 5][0] =   4.908018e-04; fpar[0][0][1][ 5][1] =   7.518301e-03; fpar[0][0][1][ 5][2] =  -1.511280e-03; 

  //emcdphi_mean_c0d1z6
  sfname[0][0][1][6] = "m_fn_dphi_mean_c0d1z6";
  sfunc[0][0][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][6] = 3;
  fpar[0][0][1][ 6][0] =   9.416633e-04; fpar[0][0][1][ 6][1] =   6.468227e-03; fpar[0][0][1][ 6][2] =  -1.207684e-03; 

  //emcdphi_mean_c0d1z7
  sfname[0][0][1][7] = "m_fn_dphi_mean_c0d1z7";
  sfunc[0][0][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][7] = 3;
  fpar[0][0][1][ 7][0] =   9.559618e-04; fpar[0][0][1][ 7][1] =   5.715375e-03; fpar[0][0][1][ 7][2] =  -9.736021e-04; 

  //emcdphi_mean_c0d1z8
  sfname[0][0][1][8] = "m_fn_dphi_mean_c0d1z8";
  sfunc[0][0][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][8] = 3;
  fpar[0][0][1][ 8][0] =   2.267817e-04; fpar[0][0][1][ 8][1] =   6.403737e-03; fpar[0][0][1][ 8][2] =  -1.513589e-03; 

  //emcdphi_mean_c0d1z9
  sfname[0][0][1][9] = "m_fn_dphi_mean_c0d1z9";
  sfunc[0][0][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][0][1][9] = 3;
  fpar[0][0][1][ 9][0] =   2.028354e-04; fpar[0][0][1][ 9][1] =   6.687843e-03; fpar[0][0][1][ 9][2] =  -2.150851e-03; 

  //emcdphi_mean_c1d0z0
  sfname[0][1][0][0] = "m_fn_dphi_mean_c1d0z0";
  sfunc[0][1][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][0] = 3;
  fpar[0][1][0][ 0][0] =  -3.333577e-03; fpar[0][1][0][ 0][1] =  -4.024257e-04; fpar[0][1][0][ 0][2] =  -2.167601e-04; 

  //emcdphi_mean_c1d0z1
  sfname[0][1][0][1] = "m_fn_dphi_mean_c1d0z1";
  sfunc[0][1][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][1] = 3;
  fpar[0][1][0][ 1][0] =  -3.196899e-03; fpar[0][1][0][ 1][1] =  -1.083639e-03; fpar[0][1][0][ 1][2] =  -1.895459e-04; 

  //emcdphi_mean_c1d0z2
  sfname[0][1][0][2] = "m_fn_dphi_mean_c1d0z2";
  sfunc[0][1][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][2] = 3;
  fpar[0][1][0][ 2][0] =  -3.236989e-03; fpar[0][1][0][ 2][1] =  -9.142353e-04; fpar[0][1][0][ 2][2] =  -6.218389e-04; 

  //emcdphi_mean_c1d0z3
  sfname[0][1][0][3] = "m_fn_dphi_mean_c1d0z3";
  sfunc[0][1][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][3] = 3;
  fpar[0][1][0][ 3][0] =  -3.230449e-03; fpar[0][1][0][ 3][1] =  -1.214466e-03; fpar[0][1][0][ 3][2] =  -5.975600e-04; 

  //emcdphi_mean_c1d0z4
  sfname[0][1][0][4] = "m_fn_dphi_mean_c1d0z4";
  sfunc[0][1][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][4] = 3;
  fpar[0][1][0][ 4][0] =  -3.299521e-03; fpar[0][1][0][ 4][1] =  -1.006166e-03; fpar[0][1][0][ 4][2] =  -7.638179e-04; 

  //emcdphi_mean_c1d0z5
  sfname[0][1][0][5] = "m_fn_dphi_mean_c1d0z5";
  sfunc[0][1][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][5] = 3;
  fpar[0][1][0][ 5][0] =  -3.649787e-03; fpar[0][1][0][ 5][1] =  -7.333561e-04; fpar[0][1][0][ 5][2] =  -8.820833e-04; 

  //emcdphi_mean_c1d0z6
  sfname[0][1][0][6] = "m_fn_dphi_mean_c1d0z6";
  sfunc[0][1][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][6] = 3;
  fpar[0][1][0][ 6][0] =  -3.605562e-03; fpar[0][1][0][ 6][1] =  -4.032601e-04; fpar[0][1][0][ 6][2] =  -9.267242e-04; 

  //emcdphi_mean_c1d0z7
  sfname[0][1][0][7] = "m_fn_dphi_mean_c1d0z7";
  sfunc[0][1][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][7] = 3;
  fpar[0][1][0][ 7][0] =  -3.945465e-03; fpar[0][1][0][ 7][1] =   3.512789e-04; fpar[0][1][0][ 7][2] =  -1.100438e-03; 

  //emcdphi_mean_c1d0z8
  sfname[0][1][0][8] = "m_fn_dphi_mean_c1d0z8";
  sfunc[0][1][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][8] = 3;
  fpar[0][1][0][ 8][0] =  -3.807267e-03; fpar[0][1][0][ 8][1] =  -1.751015e-04; fpar[0][1][0][ 8][2] =  -5.156214e-04; 

  //emcdphi_mean_c1d0z9
  sfname[0][1][0][9] = "m_fn_dphi_mean_c1d0z9";
  sfunc[0][1][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][0][9] = 3;
  fpar[0][1][0][ 9][0] =  -3.692991e-03; fpar[0][1][0][ 9][1] =  -5.162348e-04; fpar[0][1][0][ 9][2] =  -3.868719e-06; 

  //emcdphi_mean_c1d1z0
  sfname[0][1][1][0] = "m_fn_dphi_mean_c1d1z0";
  sfunc[0][1][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][0] = 3;
  fpar[0][1][1][ 0][0] =   1.010866e-03; fpar[0][1][1][ 0][1] =  -3.224154e-03; fpar[0][1][1][ 0][2] =   7.266619e-04; 

  //emcdphi_mean_c1d1z1
  sfname[0][1][1][1] = "m_fn_dphi_mean_c1d1z1";
  sfunc[0][1][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][1] = 3;
  fpar[0][1][1][ 1][0] =   1.418155e-03; fpar[0][1][1][ 1][1] =  -4.484433e-03; fpar[0][1][1][ 1][2] =   9.473548e-04; 

  //emcdphi_mean_c1d1z2
  sfname[0][1][1][2] = "m_fn_dphi_mean_c1d1z2";
  sfunc[0][1][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][2] = 3;
  fpar[0][1][1][ 2][0] =   1.256604e-03; fpar[0][1][1][ 2][1] =  -4.018089e-03; fpar[0][1][1][ 2][2] =   3.678737e-04; 

  //emcdphi_mean_c1d1z3
  sfname[0][1][1][3] = "m_fn_dphi_mean_c1d1z3";
  sfunc[0][1][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][3] = 3;
  fpar[0][1][1][ 3][0] =   1.365406e-03; fpar[0][1][1][ 3][1] =  -4.281464e-03; fpar[0][1][1][ 3][2] =   2.750030e-04; 

  //emcdphi_mean_c1d1z4
  sfname[0][1][1][4] = "m_fn_dphi_mean_c1d1z4";
  sfunc[0][1][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][4] = 3;
  fpar[0][1][1][ 4][0] =   1.158366e-03; fpar[0][1][1][ 4][1] =  -3.972434e-03; fpar[0][1][1][ 4][2] =   9.215942e-05; 

  //emcdphi_mean_c1d1z5
  sfname[0][1][1][5] = "m_fn_dphi_mean_c1d1z5";
  sfunc[0][1][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][5] = 3;
  fpar[0][1][1][ 5][0] =   1.773041e-03; fpar[0][1][1][ 5][1] =  -4.650157e-03; fpar[0][1][1][ 5][2] =   5.682526e-04; 

  //emcdphi_mean_c1d1z6
  sfname[0][1][1][6] = "m_fn_dphi_mean_c1d1z6";
  sfunc[0][1][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][6] = 3;
  fpar[0][1][1][ 6][0] =   1.632775e-03; fpar[0][1][1][ 6][1] =  -4.375128e-03; fpar[0][1][1][ 6][2] =   6.291154e-04; 

  //emcdphi_mean_c1d1z7
  sfname[0][1][1][7] = "m_fn_dphi_mean_c1d1z7";
  sfunc[0][1][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][7] = 3;
  fpar[0][1][1][ 7][0] =   1.674066e-03; fpar[0][1][1][ 7][1] =  -4.082383e-03; fpar[0][1][1][ 7][2] =   8.863003e-04; 

  //emcdphi_mean_c1d1z8
  sfname[0][1][1][8] = "m_fn_dphi_mean_c1d1z8";
  sfunc[0][1][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][8] = 3;
  fpar[0][1][1][ 8][0] =   1.350146e-03; fpar[0][1][1][ 8][1] =  -3.804123e-03; fpar[0][1][1][ 8][2] =   1.140988e-03; 

  //emcdphi_mean_c1d1z9
  sfname[0][1][1][9] = "m_fn_dphi_mean_c1d1z9";
  sfunc[0][1][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[0][1][1][9] = 3;
  fpar[0][1][1][ 9][0] =   1.361803e-03; fpar[0][1][1][ 9][1] =  -3.274408e-03; fpar[0][1][1][ 9][2] =   1.263976e-03; 

  //emcdphi_sigma_c0d0z0
  sfname[1][0][0][0] = "m_fn_dphi_sigma_c0d0z0";
  sfunc[1][0][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][0] = 3;
  fpar[1][0][0][ 0][0] =   4.708457e-03; fpar[1][0][0][ 0][1] =   1.124483e-04; fpar[1][0][0][ 0][2] =   4.494603e-04; 

  //emcdphi_sigma_c0d0z1
  sfname[1][0][0][1] = "m_fn_dphi_sigma_c0d0z1";
  sfunc[1][0][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][1] = 3;
  fpar[1][0][0][ 1][0] =   4.511592e-03; fpar[1][0][0][ 1][1] =   1.159909e-04; fpar[1][0][0][ 1][2] =   5.092691e-04; 

  //emcdphi_sigma_c0d0z2
  sfname[1][0][0][2] = "m_fn_dphi_sigma_c0d0z2";
  sfunc[1][0][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][2] = 3;
  fpar[1][0][0][ 2][0] =   4.226382e-03; fpar[1][0][0][ 2][1] =   6.910777e-04; fpar[1][0][0][ 2][2] =   2.400380e-04; 

  //emcdphi_sigma_c0d0z3
  sfname[1][0][0][3] = "m_fn_dphi_sigma_c0d0z3";
  sfunc[1][0][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][3] = 3;
  fpar[1][0][0][ 3][0] =   4.453821e-03; fpar[1][0][0][ 3][1] =   2.240260e-04; fpar[1][0][0][ 3][2] =   4.606404e-04; 

  //emcdphi_sigma_c0d0z4
  sfname[1][0][0][4] = "m_fn_dphi_sigma_c0d0z4";
  sfunc[1][0][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][4] = 3;
  fpar[1][0][0][ 4][0] =   4.451799e-03; fpar[1][0][0][ 4][1] =   6.706674e-05; fpar[1][0][0][ 4][2] =   5.265411e-04; 

  //emcdphi_sigma_c0d0z5
  sfname[1][0][0][5] = "m_fn_dphi_sigma_c0d0z5";
  sfunc[1][0][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][5] = 3;
  fpar[1][0][0][ 5][0] =   4.175737e-03; fpar[1][0][0][ 5][1] =   9.575498e-04; fpar[1][0][0][ 5][2] =   1.287978e-04; 

  //emcdphi_sigma_c0d0z6
  sfname[1][0][0][6] = "m_fn_dphi_sigma_c0d0z6";
  sfunc[1][0][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][6] = 3;
  fpar[1][0][0][ 6][0] =   4.348417e-03; fpar[1][0][0][ 6][1] =   6.808592e-04; fpar[1][0][0][ 6][2] =   2.176592e-04; 

  //emcdphi_sigma_c0d0z7
  sfname[1][0][0][7] = "m_fn_dphi_sigma_c0d0z7";
  sfunc[1][0][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][7] = 3;
  fpar[1][0][0][ 7][0] =   4.259804e-03; fpar[1][0][0][ 7][1] =   7.598023e-04; fpar[1][0][0][ 7][2] =   1.962455e-04; 

  //emcdphi_sigma_c0d0z8
  sfname[1][0][0][8] = "m_fn_dphi_sigma_c0d0z8";
  sfunc[1][0][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][8] = 3;
  fpar[1][0][0][ 8][0] =   4.182110e-03; fpar[1][0][0][ 8][1] =   7.318054e-04; fpar[1][0][0][ 8][2] =   2.021785e-04; 

  //emcdphi_sigma_c0d0z9
  sfname[1][0][0][9] = "m_fn_dphi_sigma_c0d0z9";
  sfunc[1][0][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][0][9] = 3;
  fpar[1][0][0][ 9][0] =   3.983006e-03; fpar[1][0][0][ 9][1] =   9.613211e-04; fpar[1][0][0][ 9][2] =   1.268021e-04; 

  //emcdphi_sigma_c0d1z0
  sfname[1][0][1][0] = "m_fn_dphi_sigma_c0d1z0";
  sfunc[1][0][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][0] = 3;
  fpar[1][0][1][ 0][0] =   3.529608e-03; fpar[1][0][1][ 0][1] =  -1.366175e-04; fpar[1][0][1][ 0][2] =   5.438585e-04; 

  //emcdphi_sigma_c0d1z1
  sfname[1][0][1][1] = "m_fn_dphi_sigma_c0d1z1";
  sfunc[1][0][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][1] = 3;
  fpar[1][0][1][ 1][0] =   3.758141e-03; fpar[1][0][1][ 1][1] =  -6.628101e-04; fpar[1][0][1][ 1][2] =   8.139204e-04; 

  //emcdphi_sigma_c0d1z2
  sfname[1][0][1][2] = "m_fn_dphi_sigma_c0d1z2";
  sfunc[1][0][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][2] = 3;
  fpar[1][0][1][ 2][0] =   3.839029e-03; fpar[1][0][1][ 2][1] =  -7.546685e-04; fpar[1][0][1][ 2][2] =   8.897531e-04; 

  //emcdphi_sigma_c0d1z3
  sfname[1][0][1][3] = "m_fn_dphi_sigma_c0d1z3";
  sfunc[1][0][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][3] = 3;
  fpar[1][0][1][ 3][0] =   3.598183e-03; fpar[1][0][1][ 3][1] =  -1.710611e-04; fpar[1][0][1][ 3][2] =   6.574625e-04; 

  //emcdphi_sigma_c0d1z4
  sfname[1][0][1][4] = "m_fn_dphi_sigma_c0d1z4";
  sfunc[1][0][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][4] = 3;
  fpar[1][0][1][ 4][0] =   3.749396e-03; fpar[1][0][1][ 4][1] =  -5.359462e-04; fpar[1][0][1][ 4][2] =   8.198116e-04; 

  //emcdphi_sigma_c0d1z5
  sfname[1][0][1][5] = "m_fn_dphi_sigma_c0d1z5";
  sfunc[1][0][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][5] = 3;
  fpar[1][0][1][ 5][0] =   3.616113e-03; fpar[1][0][1][ 5][1] =  -1.550407e-04; fpar[1][0][1][ 5][2] =   6.846079e-04; 

  //emcdphi_sigma_c0d1z6
  sfname[1][0][1][6] = "m_fn_dphi_sigma_c0d1z6";
  sfunc[1][0][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][6] = 3;
  fpar[1][0][1][ 6][0] =   3.513839e-03; fpar[1][0][1][ 6][1] =   4.867245e-05; fpar[1][0][1][ 6][2] =   5.868365e-04; 

  //emcdphi_sigma_c0d1z7
  sfname[1][0][1][7] = "m_fn_dphi_sigma_c0d1z7";
  sfunc[1][0][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][7] = 3;
  fpar[1][0][1][ 7][0] =   3.531195e-03; fpar[1][0][1][ 7][1] =   8.247672e-05; fpar[1][0][1][ 7][2] =   5.126639e-04; 

  //emcdphi_sigma_c0d1z8
  sfname[1][0][1][8] = "m_fn_dphi_sigma_c0d1z8";
  sfunc[1][0][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][8] = 3;
  fpar[1][0][1][ 8][0] =   3.548316e-03; fpar[1][0][1][ 8][1] =   2.200583e-04; fpar[1][0][1][ 8][2] =   3.369328e-04; 

  //emcdphi_sigma_c0d1z9
  sfname[1][0][1][9] = "m_fn_dphi_sigma_c0d1z9";
  sfunc[1][0][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][0][1][9] = 3;
  fpar[1][0][1][ 9][0] =   3.371473e-03; fpar[1][0][1][ 9][1] =   6.385804e-04; fpar[1][0][1][ 9][2] =   1.348527e-04; 

  //emcdphi_sigma_c1d0z0
  sfname[1][1][0][0] = "m_fn_dphi_sigma_c1d0z0";
  sfunc[1][1][0][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][0] = 3;
  fpar[1][1][0][ 0][0] =   4.736820e-03; fpar[1][1][0][ 0][1] =   3.611904e-04; fpar[1][1][0][ 0][2] =   2.317149e-04; 

  //emcdphi_sigma_c1d0z1
  sfname[1][1][0][1] = "m_fn_dphi_sigma_c1d0z1";
  sfunc[1][1][0][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][1] = 3;
  fpar[1][1][0][ 1][0] =   4.568989e-03; fpar[1][1][0][ 1][1] =   4.851303e-04; fpar[1][1][0][ 1][2] =   2.486472e-04; 

  //emcdphi_sigma_c1d0z2
  sfname[1][1][0][2] = "m_fn_dphi_sigma_c1d0z2";
  sfunc[1][1][0][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][2] = 3;
  fpar[1][1][0][ 2][0] =   4.555624e-03; fpar[1][1][0][ 2][1] =   5.705633e-04; fpar[1][1][0][ 2][2] =   1.990408e-04; 

  //emcdphi_sigma_c1d0z3
  sfname[1][1][0][3] = "m_fn_dphi_sigma_c1d0z3";
  sfunc[1][1][0][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][3] = 3;
  fpar[1][1][0][ 3][0] =   4.517575e-03; fpar[1][1][0][ 3][1] =   3.112172e-04; fpar[1][1][0][ 3][2] =   3.710186e-04; 

  //emcdphi_sigma_c1d0z4
  sfname[1][1][0][4] = "m_fn_dphi_sigma_c1d0z4";
  sfunc[1][1][0][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][4] = 3;
  fpar[1][1][0][ 4][0] =   4.734400e-03; fpar[1][1][0][ 4][1] =  -1.184319e-04; fpar[1][1][0][ 4][2] =   5.271670e-04; 

  //emcdphi_sigma_c1d0z5
  sfname[1][1][0][5] = "m_fn_dphi_sigma_c1d0z5";
  sfunc[1][1][0][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][5] = 3;
  fpar[1][1][0][ 5][0] =   4.792528e-03; fpar[1][1][0][ 5][1] =  -2.144390e-04; fpar[1][1][0][ 5][2] =   5.567070e-04; 

  //emcdphi_sigma_c1d0z6
  sfname[1][1][0][6] = "m_fn_dphi_sigma_c1d0z6";
  sfunc[1][1][0][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][6] = 3;
  fpar[1][1][0][ 6][0] =   4.778016e-03; fpar[1][1][0][ 6][1] =  -1.275734e-05; fpar[1][1][0][ 6][2] =   4.581590e-04; 

  //emcdphi_sigma_c1d0z7
  sfname[1][1][0][7] = "m_fn_dphi_sigma_c1d0z7";
  sfunc[1][1][0][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][7] = 3;
  fpar[1][1][0][ 7][0] =   4.718769e-03; fpar[1][1][0][ 7][1] =   4.640689e-05; fpar[1][1][0][ 7][2] =   4.263440e-04; 

  //emcdphi_sigma_c1d0z8
  sfname[1][1][0][8] = "m_fn_dphi_sigma_c1d0z8";
  sfunc[1][1][0][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][8] = 3;
  fpar[1][1][0][ 8][0] =   4.767106e-03; fpar[1][1][0][ 8][1] =  -1.799610e-04; fpar[1][1][0][ 8][2] =   5.434430e-04; 

  //emcdphi_sigma_c1d0z9
  sfname[1][1][0][9] = "m_fn_dphi_sigma_c1d0z9";
  sfunc[1][1][0][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][0][9] = 3;
  fpar[1][1][0][ 9][0] =   4.920651e-03; fpar[1][1][0][ 9][1] =  -6.239510e-04; fpar[1][1][0][ 9][2] =   7.478899e-04; 

  //emcdphi_sigma_c1d1z0
  sfname[1][1][1][0] = "m_fn_dphi_sigma_c1d1z0";
  sfunc[1][1][1][0] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][0] = 3;
  fpar[1][1][1][ 0][0] =   3.850979e-03; fpar[1][1][1][ 0][1] =   2.472632e-04; fpar[1][1][1][ 0][2] =   5.235427e-04; 

  //emcdphi_sigma_c1d1z1
  sfname[1][1][1][1] = "m_fn_dphi_sigma_c1d1z1";
  sfunc[1][1][1][1] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][1] = 3;
  fpar[1][1][1][ 1][0] =   4.319787e-03; fpar[1][1][1][ 1][1] =  -8.961215e-04; fpar[1][1][1][ 1][2] =   1.054023e-03; 

  //emcdphi_sigma_c1d1z2
  sfname[1][1][1][2] = "m_fn_dphi_sigma_c1d1z2";
  sfunc[1][1][1][2] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][2] = 3;
  fpar[1][1][1][ 2][0] =   4.176397e-03; fpar[1][1][1][ 2][1] =  -3.593926e-04; fpar[1][1][1][ 2][2] =   7.612214e-04; 

  //emcdphi_sigma_c1d1z3
  sfname[1][1][1][3] = "m_fn_dphi_sigma_c1d1z3";
  sfunc[1][1][1][3] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][3] = 3;
  fpar[1][1][1][ 3][0] =   3.884990e-03; fpar[1][1][1][ 3][1] =   1.725952e-04; fpar[1][1][1][ 3][2] =   5.663203e-04; 

  //emcdphi_sigma_c1d1z4
  sfname[1][1][1][4] = "m_fn_dphi_sigma_c1d1z4";
  sfunc[1][1][1][4] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][4] = 3;
  fpar[1][1][1][ 4][0] =   4.132371e-03; fpar[1][1][1][ 4][1] =  -1.857635e-04; fpar[1][1][1][ 4][2] =   6.985887e-04; 

  //emcdphi_sigma_c1d1z5
  sfname[1][1][1][5] = "m_fn_dphi_sigma_c1d1z5";
  sfunc[1][1][1][5] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][5] = 3;
  fpar[1][1][1][ 5][0] =   4.086527e-03; fpar[1][1][1][ 5][1] =   1.123941e-04; fpar[1][1][1][ 5][2] =   5.652637e-04; 

  //emcdphi_sigma_c1d1z6
  sfname[1][1][1][6] = "m_fn_dphi_sigma_c1d1z6";
  sfunc[1][1][1][6] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][6] = 3;
  fpar[1][1][1][ 6][0] =   3.640117e-03; fpar[1][1][1][ 6][1] =   8.275482e-04; fpar[1][1][1][ 6][2] =   4.005050e-04; 

  //emcdphi_sigma_c1d1z7
  sfname[1][1][1][7] = "m_fn_dphi_sigma_c1d1z7";
  sfunc[1][1][1][7] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][7] = 3;
  fpar[1][1][1][ 7][0] =   3.943065e-03; fpar[1][1][1][ 7][1] =   4.621867e-04; fpar[1][1][1][ 7][2] =   5.116533e-04; 

  //emcdphi_sigma_c1d1z8
  sfname[1][1][1][8] = "m_fn_dphi_sigma_c1d1z8";
  sfunc[1][1][1][8] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][8] = 3;
  fpar[1][1][1][ 8][0] =   4.421796e-03; fpar[1][1][1][ 8][1] =  -1.989366e-04; fpar[1][1][1][ 8][2] =   8.216876e-04; 

  //emcdphi_sigma_c1d1z9
  sfname[1][1][1][9] = "m_fn_dphi_sigma_c1d1z9";
  sfunc[1][1][1][9] = "[0]+[1]/x+[2]/(x*x)";
  nfpar[1][1][1][9] = 3;
  fpar[1][1][1][ 9][0] =   4.057611e-03; fpar[1][1][1][ 9][1] =   9.329989e-04; fpar[1][1][1][ 9][2] =   2.862326e-04; 

  //emcdz_mean_c0d0z0
  sfname[2][0][0][0] = "m_fn_dz_mean_c0d0z0";
  sfunc[2][0][0][0] = "[0]+[1]/x";
  nfpar[2][0][0][0] = 2;
  fpar[2][0][0][ 0][0] =  -1.723034e+00; fpar[2][0][0][ 0][1] =   2.235738e+00; 

  //emcdz_mean_c0d0z1
  sfname[2][0][0][1] = "m_fn_dz_mean_c0d0z1";
  sfunc[2][0][0][1] = "[0]+[1]/x";
  nfpar[2][0][0][1] = 2;
  fpar[2][0][0][ 1][0] =  -8.477498e-01; fpar[2][0][0][ 1][1] =   1.713973e+00; 

  //emcdz_mean_c0d0z2
  sfname[2][0][0][2] = "m_fn_dz_mean_c0d0z2";
  sfunc[2][0][0][2] = "[0]+[1]/x";
  nfpar[2][0][0][2] = 2;
  fpar[2][0][0][ 2][0] =   2.230070e-01; fpar[2][0][0][ 2][1] =   1.162112e+00; 

  //emcdz_mean_c0d0z3
  sfname[2][0][0][3] = "m_fn_dz_mean_c0d0z3";
  sfunc[2][0][0][3] = "[0]+[1]/x";
  nfpar[2][0][0][3] = 2;
  fpar[2][0][0][ 3][0] =   1.067091e+00; fpar[2][0][0][ 3][1] =   6.390403e-01; 

  //emcdz_mean_c0d0z4
  sfname[2][0][0][4] = "m_fn_dz_mean_c0d0z4";
  sfunc[2][0][0][4] = "[0]+[1]/x";
  nfpar[2][0][0][4] = 2;
  fpar[2][0][0][ 4][0] =   1.813466e+00; fpar[2][0][0][ 4][1] =   1.715355e-01; 

  //emcdz_mean_c0d0z5
  sfname[2][0][0][5] = "m_fn_dz_mean_c0d0z5";
  sfunc[2][0][0][5] = "[0]+[1]/x";
  nfpar[2][0][0][5] = 2;
  fpar[2][0][0][ 5][0] =   2.713570e+00; fpar[2][0][0][ 5][1] =  -3.470753e-01; 

  //emcdz_mean_c0d0z6
  sfname[2][0][0][6] = "m_fn_dz_mean_c0d0z6";
  sfunc[2][0][0][6] = "[0]+[1]/x";
  nfpar[2][0][0][6] = 2;
  fpar[2][0][0][ 6][0] =   3.500030e+00; fpar[2][0][0][ 6][1] =  -7.983365e-01; 

  //emcdz_mean_c0d0z7
  sfname[2][0][0][7] = "m_fn_dz_mean_c0d0z7";
  sfunc[2][0][0][7] = "[0]+[1]/x";
  nfpar[2][0][0][7] = 2;
  fpar[2][0][0][ 7][0] =   4.275154e+00; fpar[2][0][0][ 7][1] =  -1.231694e+00; 

  //emcdz_mean_c0d0z8
  sfname[2][0][0][8] = "m_fn_dz_mean_c0d0z8";
  sfunc[2][0][0][8] = "[0]+[1]/x";
  nfpar[2][0][0][8] = 2;
  fpar[2][0][0][ 8][0] =   5.192655e+00; fpar[2][0][0][ 8][1] =  -1.738562e+00; 

  //emcdz_mean_c0d0z9
  sfname[2][0][0][9] = "m_fn_dz_mean_c0d0z9";
  sfunc[2][0][0][9] = "[0]+[1]/x";
  nfpar[2][0][0][9] = 2;
  fpar[2][0][0][ 9][0] =   5.908275e+00; fpar[2][0][0][ 9][1] =  -2.233221e+00; 

  //emcdz_mean_c0d1z0
  sfname[2][0][1][0] = "m_fn_dz_mean_c0d1z0";
  sfunc[2][0][1][0] = "[0]+[1]/x";
  nfpar[2][0][1][0] = 2;
  fpar[2][0][1][ 0][0] =  -3.261851e+00; fpar[2][0][1][ 0][1] =   1.844923e+00; 

  //emcdz_mean_c0d1z1
  sfname[2][0][1][1] = "m_fn_dz_mean_c0d1z1";
  sfunc[2][0][1][1] = "[0]+[1]/x";
  nfpar[2][0][1][1] = 2;
  fpar[2][0][1][ 1][0] =  -2.744641e+00; fpar[2][0][1][ 1][1] =   1.570014e+00; 

  //emcdz_mean_c0d1z2
  sfname[2][0][1][2] = "m_fn_dz_mean_c0d1z2";
  sfunc[2][0][1][2] = "[0]+[1]/x";
  nfpar[2][0][1][2] = 2;
  fpar[2][0][1][ 2][0] =  -2.049686e+00; fpar[2][0][1][ 2][1] =   1.160918e+00; 

  //emcdz_mean_c0d1z3
  sfname[2][0][1][3] = "m_fn_dz_mean_c0d1z3";
  sfunc[2][0][1][3] = "[0]+[1]/x";
  nfpar[2][0][1][3] = 2;
  fpar[2][0][1][ 3][0] =  -1.276428e+00; fpar[2][0][1][ 3][1] =   7.003071e-01; 

  //emcdz_mean_c0d1z4
  sfname[2][0][1][4] = "m_fn_dz_mean_c0d1z4";
  sfunc[2][0][1][4] = "[0]+[1]/x";
  nfpar[2][0][1][4] = 2;
  fpar[2][0][1][ 4][0] =  -7.003486e-01; fpar[2][0][1][ 4][1] =   3.351881e-01; 

  //emcdz_mean_c0d1z5
  sfname[2][0][1][5] = "m_fn_dz_mean_c0d1z5";
  sfunc[2][0][1][5] = "[0]+[1]/x";
  nfpar[2][0][1][5] = 2;
  fpar[2][0][1][ 5][0] =   1.200100e-01; fpar[2][0][1][ 5][1] =  -1.333412e-01; 

  //emcdz_mean_c0d1z6
  sfname[2][0][1][6] = "m_fn_dz_mean_c0d1z6";
  sfunc[2][0][1][6] = "[0]+[1]/x";
  nfpar[2][0][1][6] = 2;
  fpar[2][0][1][ 6][0] =   7.452339e-01; fpar[2][0][1][ 6][1] =  -5.973183e-01; 

  //emcdz_mean_c0d1z7
  sfname[2][0][1][7] = "m_fn_dz_mean_c0d1z7";
  sfunc[2][0][1][7] = "[0]+[1]/x";
  nfpar[2][0][1][7] = 2;
  fpar[2][0][1][ 7][0] =   1.482554e+00; fpar[2][0][1][ 7][1] =  -9.684934e-01; 

  //emcdz_mean_c0d1z8
  sfname[2][0][1][8] = "m_fn_dz_mean_c0d1z8";
  sfunc[2][0][1][8] = "[0]+[1]/x";
  nfpar[2][0][1][8] = 2;
  fpar[2][0][1][ 8][0] =   2.301582e+00; fpar[2][0][1][ 8][1] =  -1.404324e+00; 

  //emcdz_mean_c0d1z9
  sfname[2][0][1][9] = "m_fn_dz_mean_c0d1z9";
  sfunc[2][0][1][9] = "[0]+[1]/x";
  nfpar[2][0][1][9] = 2;
  fpar[2][0][1][ 9][0] =   2.892153e+00; fpar[2][0][1][ 9][1] =  -1.640934e+00; 

  //emcdz_mean_c1d0z0
  sfname[2][1][0][0] = "m_fn_dz_mean_c1d0z0";
  sfunc[2][1][0][0] = "[0]+[1]/x";
  nfpar[2][1][0][0] = 2;
  fpar[2][1][0][ 0][0] =  -1.288642e+00; fpar[2][1][0][ 0][1] =   1.913443e+00; 

  //emcdz_mean_c1d0z1
  sfname[2][1][0][1] = "m_fn_dz_mean_c1d0z1";
  sfunc[2][1][0][1] = "[0]+[1]/x";
  nfpar[2][1][0][1] = 2;
  fpar[2][1][0][ 1][0] =  -5.374637e-01; fpar[2][1][0][ 1][1] =   1.472976e+00; 

  //emcdz_mean_c1d0z2
  sfname[2][1][0][2] = "m_fn_dz_mean_c1d0z2";
  sfunc[2][1][0][2] = "[0]+[1]/x";
  nfpar[2][1][0][2] = 2;
  fpar[2][1][0][ 2][0] =   4.231547e-01; fpar[2][1][0][ 2][1] =   9.987028e-01; 

  //emcdz_mean_c1d0z3
  sfname[2][1][0][3] = "m_fn_dz_mean_c1d0z3";
  sfunc[2][1][0][3] = "[0]+[1]/x";
  nfpar[2][1][0][3] = 2;
  fpar[2][1][0][ 3][0] =   1.191300e+00; fpar[2][1][0][ 3][1] =   5.534880e-01; 

  //emcdz_mean_c1d0z4
  sfname[2][1][0][4] = "m_fn_dz_mean_c1d0z4";
  sfunc[2][1][0][4] = "[0]+[1]/x";
  nfpar[2][1][0][4] = 2;
  fpar[2][1][0][ 4][0] =   1.843712e+00; fpar[2][1][0][ 4][1] =   1.443071e-01; 

  //emcdz_mean_c1d0z5
  sfname[2][1][0][5] = "m_fn_dz_mean_c1d0z5";
  sfunc[2][1][0][5] = "[0]+[1]/x";
  nfpar[2][1][0][5] = 2;
  fpar[2][1][0][ 5][0] =   2.683997e+00; fpar[2][1][0][ 5][1] =  -3.787632e-01; 

  //emcdz_mean_c1d0z6
  sfname[2][1][0][6] = "m_fn_dz_mean_c1d0z6";
  sfunc[2][1][0][6] = "[0]+[1]/x";
  nfpar[2][1][0][6] = 2;
  fpar[2][1][0][ 6][0] =   3.385690e+00; fpar[2][1][0][ 6][1] =  -7.272420e-01; 

  //emcdz_mean_c1d0z7
  sfname[2][1][0][7] = "m_fn_dz_mean_c1d0z7";
  sfunc[2][1][0][7] = "[0]+[1]/x";
  nfpar[2][1][0][7] = 2;
  fpar[2][1][0][ 7][0] =   4.123617e+00; fpar[2][1][0][ 7][1] =  -1.175400e+00; 

  //emcdz_mean_c1d0z8
  sfname[2][1][0][8] = "m_fn_dz_mean_c1d0z8";
  sfunc[2][1][0][8] = "[0]+[1]/x";
  nfpar[2][1][0][8] = 2;
  fpar[2][1][0][ 8][0] =   4.951027e+00; fpar[2][1][0][ 8][1] =  -1.556401e+00; 

  //emcdz_mean_c1d0z9
  sfname[2][1][0][9] = "m_fn_dz_mean_c1d0z9";
  sfunc[2][1][0][9] = "[0]+[1]/x";
  nfpar[2][1][0][9] = 2;
  fpar[2][1][0][ 9][0] =   5.514531e+00; fpar[2][1][0][ 9][1] =  -1.867448e+00; 

  //emcdz_mean_c1d1z0
  sfname[2][1][1][0] = "m_fn_dz_mean_c1d1z0";
  sfunc[2][1][1][0] = "[0]+[1]/x";
  nfpar[2][1][1][0] = 2;
  fpar[2][1][1][ 0][0] =  -2.805287e+00; fpar[2][1][1][ 0][1] =   1.344277e+00; 

  //emcdz_mean_c1d1z1
  sfname[2][1][1][1] = "m_fn_dz_mean_c1d1z1";
  sfunc[2][1][1][1] = "[0]+[1]/x";
  nfpar[2][1][1][1] = 2;
  fpar[2][1][1][ 1][0] =  -2.480912e+00; fpar[2][1][1][ 1][1] =   1.280315e+00; 

  //emcdz_mean_c1d1z2
  sfname[2][1][1][2] = "m_fn_dz_mean_c1d1z2";
  sfunc[2][1][1][2] = "[0]+[1]/x";
  nfpar[2][1][1][2] = 2;
  fpar[2][1][1][ 2][0] =  -1.838291e+00; fpar[2][1][1][ 2][1] =   9.474090e-01; 

  //emcdz_mean_c1d1z3
  sfname[2][1][1][3] = "m_fn_dz_mean_c1d1z3";
  sfunc[2][1][1][3] = "[0]+[1]/x";
  nfpar[2][1][1][3] = 2;
  fpar[2][1][1][ 3][0] =  -1.159681e+00; fpar[2][1][1][ 3][1] =   5.204265e-01; 

  //emcdz_mean_c1d1z4
  sfname[2][1][1][4] = "m_fn_dz_mean_c1d1z4";
  sfunc[2][1][1][4] = "[0]+[1]/x";
  nfpar[2][1][1][4] = 2;
  fpar[2][1][1][ 4][0] =  -6.124702e-01; fpar[2][1][1][ 4][1] =   1.939762e-01; 

  //emcdz_mean_c1d1z5
  sfname[2][1][1][5] = "m_fn_dz_mean_c1d1z5";
  sfunc[2][1][1][5] = "[0]+[1]/x";
  nfpar[2][1][1][5] = 2;
  fpar[2][1][1][ 5][0] =   1.203859e-01; fpar[2][1][1][ 5][1] =  -1.788030e-01; 

  //emcdz_mean_c1d1z6
  sfname[2][1][1][6] = "m_fn_dz_mean_c1d1z6";
  sfunc[2][1][1][6] = "[0]+[1]/x";
  nfpar[2][1][1][6] = 2;
  fpar[2][1][1][ 6][0] =   6.963893e-01; fpar[2][1][1][ 6][1] =  -5.779237e-01; 

  //emcdz_mean_c1d1z7
  sfname[2][1][1][7] = "m_fn_dz_mean_c1d1z7";
  sfunc[2][1][1][7] = "[0]+[1]/x";
  nfpar[2][1][1][7] = 2;
  fpar[2][1][1][ 7][0] =   1.388100e+00; fpar[2][1][1][ 7][1] =  -9.563912e-01; 

  //emcdz_mean_c1d1z8
  sfname[2][1][1][8] = "m_fn_dz_mean_c1d1z8";
  sfunc[2][1][1][8] = "[0]+[1]/x";
  nfpar[2][1][1][8] = 2;
  fpar[2][1][1][ 8][0] =   2.169724e+00; fpar[2][1][1][ 8][1] =  -1.347230e+00; 

  //emcdz_mean_c1d1z9
  sfname[2][1][1][9] = "m_fn_dz_mean_c1d1z9";
  sfunc[2][1][1][9] = "[0]+[1]/x";
  nfpar[2][1][1][9] = 2;
  fpar[2][1][1][ 9][0] =   2.661611e+00; fpar[2][1][1][ 9][1] =  -1.568438e+00; 

  //emcdz_sigma_c0d0z0
  sfname[3][0][0][0] = "m_fn_dz_sigma_c0d0z0";
  sfunc[3][0][0][0] = "[0]+[1]/(x)";
  nfpar[3][0][0][0] = 2;
  fpar[3][0][0][ 0][0] =   2.163270e+00; fpar[3][0][0][ 0][1] =   7.500070e-01; 

  //emcdz_sigma_c0d0z1
  sfname[3][0][0][1] = "m_fn_dz_sigma_c0d0z1";
  sfunc[3][0][0][1] = "[0]+[1]/(x)";
  nfpar[3][0][0][1] = 2;
  fpar[3][0][0][ 1][0] =   1.979831e+00; fpar[3][0][0][ 1][1] =   6.789607e-01; 

  //emcdz_sigma_c0d0z2
  sfname[3][0][0][2] = "m_fn_dz_sigma_c0d0z2";
  sfunc[3][0][0][2] = "[0]+[1]/(x)";
  nfpar[3][0][0][2] = 2;
  fpar[3][0][0][ 2][0] =   1.759227e+00; fpar[3][0][0][ 2][1] =   7.890850e-01; 

  //emcdz_sigma_c0d0z3
  sfname[3][0][0][3] = "m_fn_dz_sigma_c0d0z3";
  sfunc[3][0][0][3] = "[0]+[1]/(x)";
  nfpar[3][0][0][3] = 2;
  fpar[3][0][0][ 3][0] =   1.577248e+00; fpar[3][0][0][ 3][1] =   8.797687e-01; 

  //emcdz_sigma_c0d0z4
  sfname[3][0][0][4] = "m_fn_dz_sigma_c0d0z4";
  sfunc[3][0][0][4] = "[0]+[1]/(x)";
  nfpar[3][0][0][4] = 2;
  fpar[3][0][0][ 4][0] =   1.706601e+00; fpar[3][0][0][ 4][1] =   7.094097e-01; 

  //emcdz_sigma_c0d0z5
  sfname[3][0][0][5] = "m_fn_dz_sigma_c0d0z5";
  sfunc[3][0][0][5] = "[0]+[1]/(x)";
  nfpar[3][0][0][5] = 2;
  fpar[3][0][0][ 5][0] =   1.722272e+00; fpar[3][0][0][ 5][1] =   7.259425e-01; 

  //emcdz_sigma_c0d0z6
  sfname[3][0][0][6] = "m_fn_dz_sigma_c0d0z6";
  sfunc[3][0][0][6] = "[0]+[1]/(x)";
  nfpar[3][0][0][6] = 2;
  fpar[3][0][0][ 6][0] =   1.740255e+00; fpar[3][0][0][ 6][1] =   7.179321e-01; 

  //emcdz_sigma_c0d0z7
  sfname[3][0][0][7] = "m_fn_dz_sigma_c0d0z7";
  sfunc[3][0][0][7] = "[0]+[1]/(x)";
  nfpar[3][0][0][7] = 2;
  fpar[3][0][0][ 7][0] =   1.811447e+00; fpar[3][0][0][ 7][1] =   8.203752e-01; 

  //emcdz_sigma_c0d0z8
  sfname[3][0][0][8] = "m_fn_dz_sigma_c0d0z8";
  sfunc[3][0][0][8] = "[0]+[1]/(x)";
  nfpar[3][0][0][8] = 2;
  fpar[3][0][0][ 8][0] =   2.013951e+00; fpar[3][0][0][ 8][1] =   7.785804e-01; 

  //emcdz_sigma_c0d0z9
  sfname[3][0][0][9] = "m_fn_dz_sigma_c0d0z9";
  sfunc[3][0][0][9] = "[0]+[1]/(x)";
  nfpar[3][0][0][9] = 2;
  fpar[3][0][0][ 9][0] =   2.505945e+00; fpar[3][0][0][ 9][1] =   4.167749e-01; 

  //emcdz_sigma_c0d1z0
  sfname[3][0][1][0] = "m_fn_dz_sigma_c0d1z0";
  sfunc[3][0][1][0] = "[0]+[1]/(x)";
  nfpar[3][0][1][0] = 2;
  fpar[3][0][1][ 0][0] =   2.579050e+00; fpar[3][0][1][ 0][1] =   8.781759e-02; 

  //emcdz_sigma_c0d1z1
  sfname[3][0][1][1] = "m_fn_dz_sigma_c0d1z1";
  sfunc[3][0][1][1] = "[0]+[1]/(x)";
  nfpar[3][0][1][1] = 2;
  fpar[3][0][1][ 1][0] =   2.133273e+00; fpar[3][0][1][ 1][1] =   3.894516e-01; 

  //emcdz_sigma_c0d1z2
  sfname[3][0][1][2] = "m_fn_dz_sigma_c0d1z2";
  sfunc[3][0][1][2] = "[0]+[1]/(x)";
  nfpar[3][0][1][2] = 2;
  fpar[3][0][1][ 2][0] =   1.871687e+00; fpar[3][0][1][ 2][1] =   5.267156e-01; 

  //emcdz_sigma_c0d1z3
  sfname[3][0][1][3] = "m_fn_dz_sigma_c0d1z3";
  sfunc[3][0][1][3] = "[0]+[1]/(x)";
  nfpar[3][0][1][3] = 2;
  fpar[3][0][1][ 3][0] =   1.675110e+00; fpar[3][0][1][ 3][1] =   6.585466e-01; 

  //emcdz_sigma_c0d1z4
  sfname[3][0][1][4] = "m_fn_dz_sigma_c0d1z4";
  sfunc[3][0][1][4] = "[0]+[1]/(x)";
  nfpar[3][0][1][4] = 2;
  fpar[3][0][1][ 4][0] =   1.654626e+00; fpar[3][0][1][ 4][1] =   6.028415e-01; 

  //emcdz_sigma_c0d1z5
  sfname[3][0][1][5] = "m_fn_dz_sigma_c0d1z5";
  sfunc[3][0][1][5] = "[0]+[1]/(x)";
  nfpar[3][0][1][5] = 2;
  fpar[3][0][1][ 5][0] =   1.620439e+00; fpar[3][0][1][ 5][1] =   6.264903e-01; 

  //emcdz_sigma_c0d1z6
  sfname[3][0][1][6] = "m_fn_dz_sigma_c0d1z6";
  sfunc[3][0][1][6] = "[0]+[1]/(x)";
  nfpar[3][0][1][6] = 2;
  fpar[3][0][1][ 6][0] =   1.575115e+00; fpar[3][0][1][ 6][1] =   6.983308e-01; 

  //emcdz_sigma_c0d1z7
  sfname[3][0][1][7] = "m_fn_dz_sigma_c0d1z7";
  sfunc[3][0][1][7] = "[0]+[1]/(x)";
  nfpar[3][0][1][7] = 2;
  fpar[3][0][1][ 7][0] =   1.713360e+00; fpar[3][0][1][ 7][1] =   6.408888e-01; 

  //emcdz_sigma_c0d1z8
  sfname[3][0][1][8] = "m_fn_dz_sigma_c0d1z8";
  sfunc[3][0][1][8] = "[0]+[1]/(x)";
  nfpar[3][0][1][8] = 2;
  fpar[3][0][1][ 8][0] =   2.028481e+00; fpar[3][0][1][ 8][1] =   3.442515e-01; 

  //emcdz_sigma_c0d1z9
  sfname[3][0][1][9] = "m_fn_dz_sigma_c0d1z9";
  sfunc[3][0][1][9] = "[0]+[1]/(x)";
  nfpar[3][0][1][9] = 2;
  fpar[3][0][1][ 9][0] =   2.409660e+00; fpar[3][0][1][ 9][1] =   1.598829e-01; 

  //emcdz_sigma_c1d0z0
  sfname[3][1][0][0] = "m_fn_dz_sigma_c1d0z0";
  sfunc[3][1][0][0] = "[0]+[1]/(x)";
  nfpar[3][1][0][0] = 2;
  fpar[3][1][0][ 0][0] =   2.475012e+00; fpar[3][1][0][ 0][1] =   5.364366e-01; 

  //emcdz_sigma_c1d0z1
  sfname[3][1][0][1] = "m_fn_dz_sigma_c1d0z1";
  sfunc[3][1][0][1] = "[0]+[1]/(x)";
  nfpar[3][1][0][1] = 2;
  fpar[3][1][0][ 1][0] =   2.137135e+00; fpar[3][1][0][ 1][1] =   7.125981e-01; 

  //emcdz_sigma_c1d0z2
  sfname[3][1][0][2] = "m_fn_dz_sigma_c1d0z2";
  sfunc[3][1][0][2] = "[0]+[1]/(x)";
  nfpar[3][1][0][2] = 2;
  fpar[3][1][0][ 2][0] =   1.953922e+00; fpar[3][1][0][ 2][1] =   6.479535e-01; 

  //emcdz_sigma_c1d0z3
  sfname[3][1][0][3] = "m_fn_dz_sigma_c1d0z3";
  sfunc[3][1][0][3] = "[0]+[1]/(x)";
  nfpar[3][1][0][3] = 2;
  fpar[3][1][0][ 3][0] =   1.664580e+00; fpar[3][1][0][ 3][1] =   8.803152e-01; 

  //emcdz_sigma_c1d0z4
  sfname[3][1][0][4] = "m_fn_dz_sigma_c1d0z4";
  sfunc[3][1][0][4] = "[0]+[1]/(x)";
  nfpar[3][1][0][4] = 2;
  fpar[3][1][0][ 4][0] =   1.751704e+00; fpar[3][1][0][ 4][1] =   7.584490e-01; 

  //emcdz_sigma_c1d0z5
  sfname[3][1][0][5] = "m_fn_dz_sigma_c1d0z5";
  sfunc[3][1][0][5] = "[0]+[1]/(x)";
  nfpar[3][1][0][5] = 2;
  fpar[3][1][0][ 5][0] =   1.707610e+00; fpar[3][1][0][ 5][1] =   9.020618e-01; 

  //emcdz_sigma_c1d0z6
  sfname[3][1][0][6] = "m_fn_dz_sigma_c1d0z6";
  sfunc[3][1][0][6] = "[0]+[1]/(x)";
  nfpar[3][1][0][6] = 2;
  fpar[3][1][0][ 6][0] =   1.755187e+00; fpar[3][1][0][ 6][1] =   8.795176e-01; 

  //emcdz_sigma_c1d0z7
  sfname[3][1][0][7] = "m_fn_dz_sigma_c1d0z7";
  sfunc[3][1][0][7] = "[0]+[1]/(x)";
  nfpar[3][1][0][7] = 2;
  fpar[3][1][0][ 7][0] =   1.951562e+00; fpar[3][1][0][ 7][1] =   7.711023e-01; 

  //emcdz_sigma_c1d0z8
  sfname[3][1][0][8] = "m_fn_dz_sigma_c1d0z8";
  sfunc[3][1][0][8] = "[0]+[1]/(x)";
  nfpar[3][1][0][8] = 2;
  fpar[3][1][0][ 8][0] =   2.276145e+00; fpar[3][1][0][ 8][1] =   5.841598e-01; 

  //emcdz_sigma_c1d0z9
  sfname[3][1][0][9] = "m_fn_dz_sigma_c1d0z9";
  sfunc[3][1][0][9] = "[0]+[1]/(x)";
  nfpar[3][1][0][9] = 2;
  fpar[3][1][0][ 9][0] =   2.716391e+00; fpar[3][1][0][ 9][1] =   3.667362e-01; 

  //emcdz_sigma_c1d1z0
  sfname[3][1][1][0] = "m_fn_dz_sigma_c1d1z0";
  sfunc[3][1][1][0] = "[0]+[1]/(x)";
  nfpar[3][1][1][0] = 2;
  fpar[3][1][1][ 0][0] =   2.644078e+00; fpar[3][1][1][ 0][1] =   2.887200e-01; 

  //emcdz_sigma_c1d1z1
  sfname[3][1][1][1] = "m_fn_dz_sigma_c1d1z1";
  sfunc[3][1][1][1] = "[0]+[1]/(x)";
  nfpar[3][1][1][1] = 2;
  fpar[3][1][1][ 1][0] =   2.342665e+00; fpar[3][1][1][ 1][1] =   3.528333e-01; 

  //emcdz_sigma_c1d1z2
  sfname[3][1][1][2] = "m_fn_dz_sigma_c1d1z2";
  sfunc[3][1][1][2] = "[0]+[1]/(x)";
  nfpar[3][1][1][2] = 2;
  fpar[3][1][1][ 2][0] =   2.057607e+00; fpar[3][1][1][ 2][1] =   4.394796e-01; 

  //emcdz_sigma_c1d1z3
  sfname[3][1][1][3] = "m_fn_dz_sigma_c1d1z3";
  sfunc[3][1][1][3] = "[0]+[1]/(x)";
  nfpar[3][1][1][3] = 2;
  fpar[3][1][1][ 3][0] =   1.802460e+00; fpar[3][1][1][ 3][1] =   6.434957e-01; 

  //emcdz_sigma_c1d1z4
  sfname[3][1][1][4] = "m_fn_dz_sigma_c1d1z4";
  sfunc[3][1][1][4] = "[0]+[1]/(x)";
  nfpar[3][1][1][4] = 2;
  fpar[3][1][1][ 4][0] =   1.778948e+00; fpar[3][1][1][ 4][1] =   6.567101e-01; 

  //emcdz_sigma_c1d1z5
  sfname[3][1][1][5] = "m_fn_dz_sigma_c1d1z5";
  sfunc[3][1][1][5] = "[0]+[1]/(x)";
  nfpar[3][1][1][5] = 2;
  fpar[3][1][1][ 5][0] =   1.725479e+00; fpar[3][1][1][ 5][1] =   6.818855e-01; 

  //emcdz_sigma_c1d1z6
  sfname[3][1][1][6] = "m_fn_dz_sigma_c1d1z6";
  sfunc[3][1][1][6] = "[0]+[1]/(x)";
  nfpar[3][1][1][6] = 2;
  fpar[3][1][1][ 6][0] =   1.705111e+00; fpar[3][1][1][ 6][1] =   7.352794e-01; 

  //emcdz_sigma_c1d1z7
  sfname[3][1][1][7] = "m_fn_dz_sigma_c1d1z7";
  sfunc[3][1][1][7] = "[0]+[1]/(x)";
  nfpar[3][1][1][7] = 2;
  fpar[3][1][1][ 7][0] =   1.953692e+00; fpar[3][1][1][ 7][1] =   5.172750e-01; 

  //emcdz_sigma_c1d1z8
  sfname[3][1][1][8] = "m_fn_dz_sigma_c1d1z8";
  sfunc[3][1][1][8] = "[0]+[1]/(x)";
  nfpar[3][1][1][8] = 2;
  fpar[3][1][1][ 8][0] =   2.268995e+00; fpar[3][1][1][ 8][1] =   4.096332e-01; 

  //emcdz_sigma_c1d1z9
  sfname[3][1][1][9] = "m_fn_dz_sigma_c1d1z9";
  sfunc[3][1][1][9] = "[0]+[1]/(x)";
  nfpar[3][1][1][9] = 2;
  fpar[3][1][1][ 9][0] =   2.664466e+00; fpar[3][1][1][ 9][1] =   2.605500e-01; 

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

void Run14AuAu200EMCMatchRecal::initParMatchGraph()
{
  double n_val[5][2][2][10];
  double val[  5][2][2][10][30];
  double eval[ 5][2][2][10][30];
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
  //emcdphi_mean_c0d0z0
  n_val[0][0][0][0] = 30;
  val[0][0][0][ 0][ 0] =   0.000000e+00; eval[0][0][0][ 0][ 0] =   0.000000e+00;
  val[0][0][0][ 0][ 1] =   0.000000e+00; eval[0][0][0][ 0][ 1] =   0.000000e+00;
  val[0][0][0][ 0][ 2] =   4.578321e-04; eval[0][0][0][ 0][ 2] =   1.647035e-06;
  val[0][0][0][ 0][ 3] =   7.565564e-04; eval[0][0][0][ 0][ 3] =   1.688229e-06;
  val[0][0][0][ 0][ 4] =   6.560223e-04; eval[0][0][0][ 0][ 4] =   1.999662e-06;
  val[0][0][0][ 0][ 5] =   3.301389e-04; eval[0][0][0][ 0][ 5] =   2.448538e-06;
  val[0][0][0][ 0][ 6] =   1.558001e-04; eval[0][0][0][ 0][ 6] =   3.038429e-06;
  val[0][0][0][ 0][ 7] =   7.040617e-05; eval[0][0][0][ 0][ 7] =   3.837316e-06;
  val[0][0][0][ 0][ 8] =   6.094950e-06; eval[0][0][0][ 0][ 8] =   4.887057e-06;
  val[0][0][0][ 0][ 9] =  -8.281784e-05; eval[0][0][0][ 0][ 9] =   6.611444e-06;
  val[0][0][0][ 0][10] =  -7.559818e-05; eval[0][0][0][ 0][10] =   8.674328e-06;
  val[0][0][0][ 0][11] =  -2.136399e-04; eval[0][0][0][ 0][11] =   1.267431e-05;
  val[0][0][0][ 0][12] =  -2.757212e-04; eval[0][0][0][ 0][12] =   1.648782e-05;
  val[0][0][0][ 0][13] =  -3.765494e-04; eval[0][0][0][ 0][13] =   2.142422e-05;
  val[0][0][0][ 0][14] =  -3.868102e-04; eval[0][0][0][ 0][14] =   2.838347e-05;
  val[0][0][0][ 0][15] =  -4.180264e-04; eval[0][0][0][ 0][15] =   3.512122e-05;
  val[0][0][0][ 0][16] =  -4.157443e-04; eval[0][0][0][ 0][16] =   4.329026e-05;
  val[0][0][0][ 0][17] =  -4.992932e-04; eval[0][0][0][ 0][17] =   5.343633e-05;
  val[0][0][0][ 0][18] =  -4.211519e-04; eval[0][0][0][ 0][18] =   6.695818e-05;
  val[0][0][0][ 0][19] =  -5.414510e-04; eval[0][0][0][ 0][19] =   8.324476e-05;
  val[0][0][0][ 0][20] =  -6.034028e-04; eval[0][0][0][ 0][20] =   5.617543e-05;
  val[0][0][0][ 0][21] =  -6.101561e-04; eval[0][0][0][ 0][21] =   7.691581e-05;
  val[0][0][0][ 0][22] =  -3.644720e-04; eval[0][0][0][ 0][22] =   1.168955e-04;
  val[0][0][0][ 0][23] =  -6.806381e-04; eval[0][0][0][ 0][23] =   1.421648e-04;
  val[0][0][0][ 0][24] =  -9.278861e-04; eval[0][0][0][ 0][24] =   1.621720e-04;
  val[0][0][0][ 0][25] =  -1.033879e-03; eval[0][0][0][ 0][25] =   1.899486e-04;
  val[0][0][0][ 0][26] =  -8.081807e-04; eval[0][0][0][ 0][26] =   3.771932e-04;
  val[0][0][0][ 0][27] =  -6.703689e-04; eval[0][0][0][ 0][27] =   2.759272e-04;
  val[0][0][0][ 0][28] =  -1.015190e-03; eval[0][0][0][ 0][28] =   3.823181e-04;
  val[0][0][0][ 0][29] =  -2.130556e-04; eval[0][0][0][ 0][29] =   5.787184e-04;
  //emcdphi_mean_c0d0z1
  n_val[0][0][0][1] = 30;
  val[0][0][0][ 1][ 0] =   0.000000e+00; eval[0][0][0][ 1][ 0] =   0.000000e+00;
  val[0][0][0][ 1][ 1] =   0.000000e+00; eval[0][0][0][ 1][ 1] =   0.000000e+00;
  val[0][0][0][ 1][ 2] =   1.583200e-03; eval[0][0][0][ 1][ 2] =   1.667337e-06;
  val[0][0][0][ 1][ 3] =   1.433143e-03; eval[0][0][0][ 1][ 3] =   1.643761e-06;
  val[0][0][0][ 1][ 4] =   1.073499e-03; eval[0][0][0][ 1][ 4] =   2.150999e-06;
  val[0][0][0][ 1][ 5] =   6.226842e-04; eval[0][0][0][ 1][ 5] =   2.623660e-06;
  val[0][0][0][ 1][ 6] =   3.331863e-04; eval[0][0][0][ 1][ 6] =   3.328343e-06;
  val[0][0][0][ 1][ 7] =   1.987700e-04; eval[0][0][0][ 1][ 7] =   4.346556e-06;
  val[0][0][0][ 1][ 8] =   8.457008e-05; eval[0][0][0][ 1][ 8] =   4.793331e-06;
  val[0][0][0][ 1][ 9] =   5.386677e-05; eval[0][0][0][ 1][ 9] =   6.413395e-06;
  val[0][0][0][ 1][10] =   6.900345e-05; eval[0][0][0][ 1][10] =   8.454293e-06;
  val[0][0][0][ 1][11] =  -8.308400e-05; eval[0][0][0][ 1][11] =   1.303660e-05;
  val[0][0][0][ 1][12] =  -1.525303e-04; eval[0][0][0][ 1][12] =   1.714466e-05;
  val[0][0][0][ 1][13] =  -3.461093e-04; eval[0][0][0][ 1][13] =   2.047078e-05;
  val[0][0][0][ 1][14] =  -4.033806e-04; eval[0][0][0][ 1][14] =   2.666999e-05;
  val[0][0][0][ 1][15] =  -4.244114e-04; eval[0][0][0][ 1][15] =   3.483111e-05;
  val[0][0][0][ 1][16] =  -5.299224e-04; eval[0][0][0][ 1][16] =   4.156780e-05;
  val[0][0][0][ 1][17] =  -6.207232e-04; eval[0][0][0][ 1][17] =   4.846232e-05;
  val[0][0][0][ 1][18] =  -7.464494e-04; eval[0][0][0][ 1][18] =   6.370546e-05;
  val[0][0][0][ 1][19] =  -5.090418e-04; eval[0][0][0][ 1][19] =   8.063005e-05;
  val[0][0][0][ 1][20] =  -6.583957e-04; eval[0][0][0][ 1][20] =   5.568979e-05;
  val[0][0][0][ 1][21] =  -8.581888e-04; eval[0][0][0][ 1][21] =   7.958727e-05;
  val[0][0][0][ 1][22] =  -8.998648e-04; eval[0][0][0][ 1][22] =   1.251478e-04;
  val[0][0][0][ 1][23] =  -1.169911e-03; eval[0][0][0][ 1][23] =   1.760034e-04;
  val[0][0][0][ 1][24] =  -9.767204e-04; eval[0][0][0][ 1][24] =   2.068215e-04;
  val[0][0][0][ 1][25] =  -1.494119e-03; eval[0][0][0][ 1][25] =   2.489203e-04;
  val[0][0][0][ 1][26] =  -1.283240e-03; eval[0][0][0][ 1][26] =   2.878866e-04;
  val[0][0][0][ 1][27] =  -1.109724e-03; eval[0][0][0][ 1][27] =   3.628972e-04;
  val[0][0][0][ 1][28] =  -1.071205e-03; eval[0][0][0][ 1][28] =   3.756954e-04;
  val[0][0][0][ 1][29] =  -1.349583e-03; eval[0][0][0][ 1][29] =   4.572488e-04;
  //emcdphi_mean_c0d0z2
  n_val[0][0][0][2] = 30;
  val[0][0][0][ 2][ 0] =   0.000000e+00; eval[0][0][0][ 2][ 0] =   0.000000e+00;
  val[0][0][0][ 2][ 1] =   0.000000e+00; eval[0][0][0][ 2][ 1] =   0.000000e+00;
  val[0][0][0][ 2][ 2] =   2.490437e-03; eval[0][0][0][ 2][ 2] =   1.625704e-06;
  val[0][0][0][ 2][ 3] =   1.917624e-03; eval[0][0][0][ 2][ 3] =   1.597611e-06;
  val[0][0][0][ 2][ 4] =   1.509754e-03; eval[0][0][0][ 2][ 4] =   1.933024e-06;
  val[0][0][0][ 2][ 5] =   1.055564e-03; eval[0][0][0][ 2][ 5] =   2.613378e-06;
  val[0][0][0][ 2][ 6] =   6.826060e-04; eval[0][0][0][ 2][ 6] =   3.289138e-06;
  val[0][0][0][ 2][ 7] =   5.205604e-04; eval[0][0][0][ 2][ 7] =   4.206897e-06;
  val[0][0][0][ 2][ 8] =   4.251993e-04; eval[0][0][0][ 2][ 8] =   5.325639e-06;
  val[0][0][0][ 2][ 9] =   3.978586e-04; eval[0][0][0][ 2][ 9] =   7.116161e-06;
  val[0][0][0][ 2][10] =   3.471207e-04; eval[0][0][0][ 2][10] =   9.324969e-06;
  val[0][0][0][ 2][11] =   1.226030e-04; eval[0][0][0][ 2][11] =   1.258142e-05;
  val[0][0][0][ 2][12] =   3.910997e-05; eval[0][0][0][ 2][12] =   1.689679e-05;
  val[0][0][0][ 2][13] =  -1.188495e-04; eval[0][0][0][ 2][13] =   2.226543e-05;
  val[0][0][0][ 2][14] =  -1.522555e-04; eval[0][0][0][ 2][14] =   2.787001e-05;
  val[0][0][0][ 2][15] =  -1.517729e-04; eval[0][0][0][ 2][15] =   3.593711e-05;
  val[0][0][0][ 2][16] =  -2.917326e-04; eval[0][0][0][ 2][16] =   4.146082e-05;
  val[0][0][0][ 2][17] =  -4.148102e-04; eval[0][0][0][ 2][17] =   5.050636e-05;
  val[0][0][0][ 2][18] =  -5.635572e-04; eval[0][0][0][ 2][18] =   6.227731e-05;
  val[0][0][0][ 2][19] =  -5.288059e-04; eval[0][0][0][ 2][19] =   7.593645e-05;
  val[0][0][0][ 2][20] =  -6.745773e-04; eval[0][0][0][ 2][20] =   5.527089e-05;
  val[0][0][0][ 2][21] =  -6.811610e-04; eval[0][0][0][ 2][21] =   7.730864e-05;
  val[0][0][0][ 2][22] =  -6.150621e-04; eval[0][0][0][ 2][22] =   1.119365e-04;
  val[0][0][0][ 2][23] =  -9.009315e-04; eval[0][0][0][ 2][23] =   1.671747e-04;
  val[0][0][0][ 2][24] =  -1.009788e-03; eval[0][0][0][ 2][24] =   2.059504e-04;
  val[0][0][0][ 2][25] =  -1.287552e-03; eval[0][0][0][ 2][25] =   2.059866e-04;
  val[0][0][0][ 2][26] =  -9.169933e-04; eval[0][0][0][ 2][26] =   2.468269e-04;
  val[0][0][0][ 2][27] =  -1.242579e-03; eval[0][0][0][ 2][27] =   3.145419e-04;
  val[0][0][0][ 2][28] =  -1.191052e-03; eval[0][0][0][ 2][28] =   4.066666e-04;
  val[0][0][0][ 2][29] =  -7.686733e-05; eval[0][0][0][ 2][29] =   4.217481e-04;
  //emcdphi_mean_c0d0z3
  n_val[0][0][0][3] = 30;
  val[0][0][0][ 3][ 0] =   0.000000e+00; eval[0][0][0][ 3][ 0] =   0.000000e+00;
  val[0][0][0][ 3][ 1] =   0.000000e+00; eval[0][0][0][ 3][ 1] =   0.000000e+00;
  val[0][0][0][ 3][ 2] =   2.832393e-03; eval[0][0][0][ 3][ 2] =   1.697576e-06;
  val[0][0][0][ 3][ 3] =   2.151766e-03; eval[0][0][0][ 3][ 3] =   1.597015e-06;
  val[0][0][0][ 3][ 4] =   1.686196e-03; eval[0][0][0][ 3][ 4] =   1.909552e-06;
  val[0][0][0][ 3][ 5] =   1.142122e-03; eval[0][0][0][ 3][ 5] =   2.519103e-06;
  val[0][0][0][ 3][ 6] =   8.430573e-04; eval[0][0][0][ 3][ 6] =   3.115419e-06;
  val[0][0][0][ 3][ 7] =   6.134355e-04; eval[0][0][0][ 3][ 7] =   4.132978e-06;
  val[0][0][0][ 3][ 8] =   4.818483e-04; eval[0][0][0][ 3][ 8] =   5.449229e-06;
  val[0][0][0][ 3][ 9] =   4.101462e-04; eval[0][0][0][ 3][ 9] =   7.424014e-06;
  val[0][0][0][ 3][10] =   3.162284e-04; eval[0][0][0][ 3][10] =   9.441827e-06;
  val[0][0][0][ 3][11] =   2.090684e-04; eval[0][0][0][ 3][11] =   1.219048e-05;
  val[0][0][0][ 3][12] =   3.889666e-05; eval[0][0][0][ 3][12] =   1.760156e-05;
  val[0][0][0][ 3][13] =  -8.606576e-05; eval[0][0][0][ 3][13] =   2.345762e-05;
  val[0][0][0][ 3][14] =  -1.417796e-04; eval[0][0][0][ 3][14] =   2.791069e-05;
  val[0][0][0][ 3][15] =  -2.295402e-04; eval[0][0][0][ 3][15] =   3.391340e-05;
  val[0][0][0][ 3][16] =  -2.782479e-04; eval[0][0][0][ 3][16] =   4.458707e-05;
  val[0][0][0][ 3][17] =  -3.355713e-04; eval[0][0][0][ 3][17] =   5.430063e-05;
  val[0][0][0][ 3][18] =  -5.162196e-04; eval[0][0][0][ 3][18] =   6.470120e-05;
  val[0][0][0][ 3][19] =  -3.971872e-04; eval[0][0][0][ 3][19] =   7.792618e-05;
  val[0][0][0][ 3][20] =  -5.026629e-04; eval[0][0][0][ 3][20] =   5.891698e-05;
  val[0][0][0][ 3][21] =  -4.472493e-04; eval[0][0][0][ 3][21] =   6.925403e-05;
  val[0][0][0][ 3][22] =  -6.714058e-04; eval[0][0][0][ 3][22] =   1.085160e-04;
  val[0][0][0][ 3][23] =  -6.031995e-04; eval[0][0][0][ 3][23] =   1.241140e-04;
  val[0][0][0][ 3][24] =  -8.492487e-04; eval[0][0][0][ 3][24] =   1.682748e-04;
  val[0][0][0][ 3][25] =  -8.039402e-04; eval[0][0][0][ 3][25] =   2.146369e-04;
  val[0][0][0][ 3][26] =  -6.693928e-04; eval[0][0][0][ 3][26] =   2.310499e-04;
  val[0][0][0][ 3][27] =  -5.720041e-04; eval[0][0][0][ 3][27] =   2.744565e-04;
  val[0][0][0][ 3][28] =   1.944202e-05; eval[0][0][0][ 3][28] =   1.102091e-03;
  val[0][0][0][ 3][29] =  -1.589001e-04; eval[0][0][0][ 3][29] =   4.496227e-04;
  //emcdphi_mean_c0d0z4
  n_val[0][0][0][4] = 30;
  val[0][0][0][ 4][ 0] =   0.000000e+00; eval[0][0][0][ 4][ 0] =   0.000000e+00;
  val[0][0][0][ 4][ 1] =   0.000000e+00; eval[0][0][0][ 4][ 1] =   0.000000e+00;
  val[0][0][0][ 4][ 2] =   3.126618e-03; eval[0][0][0][ 4][ 2] =   1.966376e-06;
  val[0][0][0][ 4][ 3] =   2.352618e-03; eval[0][0][0][ 4][ 3] =   1.845022e-06;
  val[0][0][0][ 4][ 4] =   1.864694e-03; eval[0][0][0][ 4][ 4] =   2.136572e-06;
  val[0][0][0][ 4][ 5] =   1.258496e-03; eval[0][0][0][ 4][ 5] =   2.851250e-06;
  val[0][0][0][ 4][ 6] =   9.430166e-04; eval[0][0][0][ 4][ 6] =   3.524893e-06;
  val[0][0][0][ 4][ 7] =   6.783244e-04; eval[0][0][0][ 4][ 7] =   4.746874e-06;
  val[0][0][0][ 4][ 8] =   5.136726e-04; eval[0][0][0][ 4][ 8] =   6.358109e-06;
  val[0][0][0][ 4][ 9] =   3.836669e-04; eval[0][0][0][ 4][ 9] =   8.563746e-06;
  val[0][0][0][ 4][10] =   2.613872e-04; eval[0][0][0][ 4][10] =   1.091467e-05;
  val[0][0][0][ 4][11] =   1.776137e-04; eval[0][0][0][ 4][11] =   1.494353e-05;
  val[0][0][0][ 4][12] =   5.469152e-06; eval[0][0][0][ 4][12] =   2.002918e-05;
  val[0][0][0][ 4][13] =  -9.936461e-05; eval[0][0][0][ 4][13] =   2.576393e-05;
  val[0][0][0][ 4][14] =  -1.878707e-04; eval[0][0][0][ 4][14] =   3.469616e-05;
  val[0][0][0][ 4][15] =  -1.943237e-04; eval[0][0][0][ 4][15] =   4.462496e-05;
  val[0][0][0][ 4][16] =  -3.034715e-04; eval[0][0][0][ 4][16] =   4.864179e-05;
  val[0][0][0][ 4][17] =  -2.746724e-04; eval[0][0][0][ 4][17] =   6.607272e-05;
  val[0][0][0][ 4][18] =  -2.317887e-04; eval[0][0][0][ 4][18] =   7.898123e-05;
  val[0][0][0][ 4][19] =  -5.496836e-04; eval[0][0][0][ 4][19] =   7.318799e-05;
  val[0][0][0][ 4][20] =  -4.398912e-04; eval[0][0][0][ 4][20] =   5.961748e-05;
  val[0][0][0][ 4][21] =  -4.749530e-04; eval[0][0][0][ 4][21] =   9.311228e-05;
  val[0][0][0][ 4][22] =  -4.171167e-04; eval[0][0][0][ 4][22] =   1.116342e-04;
  val[0][0][0][ 4][23] =  -7.314246e-04; eval[0][0][0][ 4][23] =   1.344875e-04;
  val[0][0][0][ 4][24] =  -8.987031e-04; eval[0][0][0][ 4][24] =   1.764393e-04;
  val[0][0][0][ 4][25] =  -1.076664e-03; eval[0][0][0][ 4][25] =   2.420943e-04;
  val[0][0][0][ 4][26] =  -4.940959e-04; eval[0][0][0][ 4][26] =   4.170292e-04;
  val[0][0][0][ 4][27] =  -1.100318e-04; eval[0][0][0][ 4][27] =   4.549819e-04;
  val[0][0][0][ 4][28] =  -4.366306e-04; eval[0][0][0][ 4][28] =   4.190047e-04;
  val[0][0][0][ 4][29] =  -1.138414e-03; eval[0][0][0][ 4][29] =   5.366790e-04;
  //emcdphi_mean_c0d0z5
  n_val[0][0][0][5] = 30;
  val[0][0][0][ 5][ 0] =   0.000000e+00; eval[0][0][0][ 5][ 0] =   0.000000e+00;
  val[0][0][0][ 5][ 1] =   0.000000e+00; eval[0][0][0][ 5][ 1] =   0.000000e+00;
  val[0][0][0][ 5][ 2] =   2.795463e-03; eval[0][0][0][ 5][ 2] =   2.185842e-06;
  val[0][0][0][ 5][ 3] =   1.789932e-03; eval[0][0][0][ 5][ 3] =   2.180555e-06;
  val[0][0][0][ 5][ 4] =   1.194363e-03; eval[0][0][0][ 5][ 4] =   2.637063e-06;
  val[0][0][0][ 5][ 5] =   9.130732e-04; eval[0][0][0][ 5][ 5] =   3.469134e-06;
  val[0][0][0][ 5][ 6] =   5.618112e-04; eval[0][0][0][ 5][ 6] =   4.341894e-06;
  val[0][0][0][ 5][ 7] =   2.203819e-04; eval[0][0][0][ 5][ 7] =   5.896460e-06;
  val[0][0][0][ 5][ 8] =   1.425202e-04; eval[0][0][0][ 5][ 8] =   7.895787e-06;
  val[0][0][0][ 5][ 9] =   2.069596e-05; eval[0][0][0][ 5][ 9] =   1.081972e-05;
  val[0][0][0][ 5][10] =  -6.039226e-05; eval[0][0][0][ 5][10] =   1.511584e-05;
  val[0][0][0][ 5][11] =  -1.974963e-04; eval[0][0][0][ 5][11] =   1.978725e-05;
  val[0][0][0][ 5][12] =  -2.203677e-04; eval[0][0][0][ 5][12] =   2.221505e-05;
  val[0][0][0][ 5][13] =  -3.834860e-04; eval[0][0][0][ 5][13] =   2.860154e-05;
  val[0][0][0][ 5][14] =  -5.440553e-04; eval[0][0][0][ 5][14] =   3.676286e-05;
  val[0][0][0][ 5][15] =  -6.715992e-04; eval[0][0][0][ 5][15] =   4.416728e-05;
  val[0][0][0][ 5][16] =  -8.686936e-04; eval[0][0][0][ 5][16] =   5.461188e-05;
  val[0][0][0][ 5][17] =  -7.912100e-04; eval[0][0][0][ 5][17] =   6.830679e-05;
  val[0][0][0][ 5][18] =  -8.659368e-04; eval[0][0][0][ 5][18] =   7.864368e-05;
  val[0][0][0][ 5][19] =  -9.386735e-04; eval[0][0][0][ 5][19] =   8.740365e-05;
  val[0][0][0][ 5][20] =  -8.785467e-04; eval[0][0][0][ 5][20] =   6.689701e-05;
  val[0][0][0][ 5][21] =  -9.454310e-04; eval[0][0][0][ 5][21] =   9.719434e-05;
  val[0][0][0][ 5][22] =  -1.264048e-03; eval[0][0][0][ 5][22] =   1.209669e-04;
  val[0][0][0][ 5][23] =  -9.825340e-04; eval[0][0][0][ 5][23] =   1.623772e-04;
  val[0][0][0][ 5][24] =  -9.246117e-04; eval[0][0][0][ 5][24] =   1.938874e-04;
  val[0][0][0][ 5][25] =  -1.016624e-03; eval[0][0][0][ 5][25] =   3.045551e-04;
  val[0][0][0][ 5][26] =  -1.270496e-03; eval[0][0][0][ 5][26] =   3.263490e-04;
  val[0][0][0][ 5][27] =  -1.285359e-03; eval[0][0][0][ 5][27] =   3.758866e-04;
  val[0][0][0][ 5][28] =  -1.172739e-03; eval[0][0][0][ 5][28] =   4.573418e-04;
  val[0][0][0][ 5][29] =   7.442046e-02; eval[0][0][0][ 5][29] =   7.381278e-02;
  //emcdphi_mean_c0d0z6
  n_val[0][0][0][6] = 30;
  val[0][0][0][ 6][ 0] =   0.000000e+00; eval[0][0][0][ 6][ 0] =   0.000000e+00;
  val[0][0][0][ 6][ 1] =   0.000000e+00; eval[0][0][0][ 6][ 1] =   0.000000e+00;
  val[0][0][0][ 6][ 2] =   2.396659e-03; eval[0][0][0][ 6][ 2] =   1.827295e-06;
  val[0][0][0][ 6][ 3] =   1.667923e-03; eval[0][0][0][ 6][ 3] =   1.776903e-06;
  val[0][0][0][ 6][ 4] =   1.146664e-03; eval[0][0][0][ 6][ 4] =   2.204943e-06;
  val[0][0][0][ 6][ 5] =   8.309073e-04; eval[0][0][0][ 6][ 5] =   2.880206e-06;
  val[0][0][0][ 6][ 6] =   4.872181e-04; eval[0][0][0][ 6][ 6] =   3.549421e-06;
  val[0][0][0][ 6][ 7] =   2.202316e-04; eval[0][0][0][ 6][ 7] =   4.849639e-06;
  val[0][0][0][ 6][ 8] =   2.715924e-04; eval[0][0][0][ 6][ 8] =   6.525735e-06;
  val[0][0][0][ 6][ 9] =   2.851380e-04; eval[0][0][0][ 6][ 9] =   9.015809e-06;
  val[0][0][0][ 6][10] =   2.376315e-04; eval[0][0][0][ 6][10] =   9.784371e-06;
  val[0][0][0][ 6][11] =   5.827468e-05; eval[0][0][0][ 6][11] =   1.274585e-05;
  val[0][0][0][ 6][12] =  -7.328186e-05; eval[0][0][0][ 6][12] =   1.666919e-05;
  val[0][0][0][ 6][13] =  -1.409111e-04; eval[0][0][0][ 6][13] =   2.159796e-05;
  val[0][0][0][ 6][14] =  -2.127678e-04; eval[0][0][0][ 6][14] =   3.346152e-05;
  val[0][0][0][ 6][15] =  -3.697524e-04; eval[0][0][0][ 6][15] =   4.097805e-05;
  val[0][0][0][ 6][16] =  -4.814974e-04; eval[0][0][0][ 6][16] =   4.942126e-05;
  val[0][0][0][ 6][17] =  -5.988794e-04; eval[0][0][0][ 6][17] =   6.087729e-05;
  val[0][0][0][ 6][18] =  -6.738704e-04; eval[0][0][0][ 6][18] =   7.732645e-05;
  val[0][0][0][ 6][19] =  -8.042519e-04; eval[0][0][0][ 6][19] =   6.797432e-05;
  val[0][0][0][ 6][20] =  -7.807116e-04; eval[0][0][0][ 6][20] =   5.769573e-05;
  val[0][0][0][ 6][21] =  -7.996777e-04; eval[0][0][0][ 6][21] =   7.692670e-05;
  val[0][0][0][ 6][22] =  -8.346892e-04; eval[0][0][0][ 6][22] =   1.061347e-04;
  val[0][0][0][ 6][23] =  -7.530763e-04; eval[0][0][0][ 6][23] =   1.421967e-04;
  val[0][0][0][ 6][24] =  -1.034387e-03; eval[0][0][0][ 6][24] =   1.886762e-04;
  val[0][0][0][ 6][25] =  -3.912134e-04; eval[0][0][0][ 6][25] =   2.097480e-04;
  val[0][0][0][ 6][26] =  -1.017285e-03; eval[0][0][0][ 6][26] =   2.601410e-04;
  val[0][0][0][ 6][27] =  -1.616907e-03; eval[0][0][0][ 6][27] =   3.405945e-04;
  val[0][0][0][ 6][28] =  -1.790539e-03; eval[0][0][0][ 6][28] =   4.048473e-04;
  val[0][0][0][ 6][29] =  -4.506351e-04; eval[0][0][0][ 6][29] =   4.409687e-04;
  //emcdphi_mean_c0d0z7
  n_val[0][0][0][7] = 30;
  val[0][0][0][ 7][ 0] =   0.000000e+00; eval[0][0][0][ 7][ 0] =   0.000000e+00;
  val[0][0][0][ 7][ 1] =   0.000000e+00; eval[0][0][0][ 7][ 1] =   0.000000e+00;
  val[0][0][0][ 7][ 2] =   1.743850e-03; eval[0][0][0][ 7][ 2] =   1.791519e-06;
  val[0][0][0][ 7][ 3] =   1.083295e-03; eval[0][0][0][ 7][ 3] =   1.828603e-06;
  val[0][0][0][ 7][ 4] =   6.386233e-04; eval[0][0][0][ 7][ 4] =   2.106004e-06;
  val[0][0][0][ 7][ 5] =   4.364728e-04; eval[0][0][0][ 7][ 5] =   3.007484e-06;
  val[0][0][0][ 7][ 6] =   2.111442e-04; eval[0][0][0][ 7][ 6] =   3.669878e-06;
  val[0][0][0][ 7][ 7] =  -3.622441e-05; eval[0][0][0][ 7][ 7] =   5.297725e-06;
  val[0][0][0][ 7][ 8] =   2.140992e-05; eval[0][0][0][ 7][ 8] =   5.309558e-06;
  val[0][0][0][ 7][ 9] =   7.659689e-06; eval[0][0][0][ 7][ 9] =   7.118082e-06;
  val[0][0][0][ 7][10] =   6.834271e-06; eval[0][0][0][ 7][10] =   1.188670e-05;
  val[0][0][0][ 7][11] =  -2.056604e-04; eval[0][0][0][ 7][11] =   1.478282e-05;
  val[0][0][0][ 7][12] =  -2.803869e-04; eval[0][0][0][ 7][12] =   1.922627e-05;
  val[0][0][0][ 7][13] =  -3.626143e-04; eval[0][0][0][ 7][13] =   2.429127e-05;
  val[0][0][0][ 7][14] =  -4.449598e-04; eval[0][0][0][ 7][14] =   3.078349e-05;
  val[0][0][0][ 7][15] =  -5.548768e-04; eval[0][0][0][ 7][15] =   3.657707e-05;
  val[0][0][0][ 7][16] =  -6.215817e-04; eval[0][0][0][ 7][16] =   4.552606e-05;
  val[0][0][0][ 7][17] =  -7.421530e-04; eval[0][0][0][ 7][17] =   5.638972e-05;
  val[0][0][0][ 7][18] =  -8.298502e-04; eval[0][0][0][ 7][18] =   6.912937e-05;
  val[0][0][0][ 7][19] =  -8.999608e-04; eval[0][0][0][ 7][19] =   6.550063e-05;
  val[0][0][0][ 7][20] =  -1.036993e-03; eval[0][0][0][ 7][20] =   5.478117e-05;
  val[0][0][0][ 7][21] =  -1.033182e-03; eval[0][0][0][ 7][21] =   7.724439e-05;
  val[0][0][0][ 7][22] =  -1.065113e-03; eval[0][0][0][ 7][22] =   1.000077e-04;
  val[0][0][0][ 7][23] =  -1.056901e-03; eval[0][0][0][ 7][23] =   1.413493e-04;
  val[0][0][0][ 7][24] =  -1.287017e-03; eval[0][0][0][ 7][24] =   1.645448e-04;
  val[0][0][0][ 7][25] =  -1.136218e-03; eval[0][0][0][ 7][25] =   2.413824e-04;
  val[0][0][0][ 7][26] =  -1.490983e-03; eval[0][0][0][ 7][26] =   2.457535e-04;
  val[0][0][0][ 7][27] =  -8.978435e-04; eval[0][0][0][ 7][27] =   3.201802e-04;
  val[0][0][0][ 7][28] =  -2.082572e-03; eval[0][0][0][ 7][28] =   4.488421e-04;
  val[0][0][0][ 7][29] =  -1.109882e-03; eval[0][0][0][ 7][29] =   4.871335e-04;
  //emcdphi_mean_c0d0z8
  n_val[0][0][0][8] = 30;
  val[0][0][0][ 8][ 0] =   0.000000e+00; eval[0][0][0][ 8][ 0] =   0.000000e+00;
  val[0][0][0][ 8][ 1] =   0.000000e+00; eval[0][0][0][ 8][ 1] =   0.000000e+00;
  val[0][0][0][ 8][ 2] =   7.305087e-04; eval[0][0][0][ 8][ 2] =   1.714236e-06;
  val[0][0][0][ 8][ 3] =   2.291942e-04; eval[0][0][0][ 8][ 3] =   1.706245e-06;
  val[0][0][0][ 8][ 4] =  -4.571001e-05; eval[0][0][0][ 8][ 4] =   2.020229e-06;
  val[0][0][0][ 8][ 5] =  -1.566560e-04; eval[0][0][0][ 8][ 5] =   2.594739e-06;
  val[0][0][0][ 8][ 6] =  -2.677419e-04; eval[0][0][0][ 8][ 6] =   3.590150e-06;
  val[0][0][0][ 8][ 7] =  -4.913904e-04; eval[0][0][0][ 8][ 7] =   4.429089e-06;
  val[0][0][0][ 8][ 8] =  -4.190796e-04; eval[0][0][0][ 8][ 8] =   5.801981e-06;
  val[0][0][0][ 8][ 9] =  -4.196583e-04; eval[0][0][0][ 8][ 9] =   7.779460e-06;
  val[0][0][0][ 8][10] =  -4.644281e-04; eval[0][0][0][ 8][10] =   1.012238e-05;
  val[0][0][0][ 8][11] =  -6.241581e-04; eval[0][0][0][ 8][11] =   1.290028e-05;
  val[0][0][0][ 8][12] =  -6.797516e-04; eval[0][0][0][ 8][12] =   1.669034e-05;
  val[0][0][0][ 8][13] =  -6.768815e-04; eval[0][0][0][ 8][13] =   2.166750e-05;
  val[0][0][0][ 8][14] =  -7.690469e-04; eval[0][0][0][ 8][14] =   2.713435e-05;
  val[0][0][0][ 8][15] =  -9.208712e-04; eval[0][0][0][ 8][15] =   3.369770e-05;
  val[0][0][0][ 8][16] =  -9.373628e-04; eval[0][0][0][ 8][16] =   4.162479e-05;
  val[0][0][0][ 8][17] =  -9.475990e-04; eval[0][0][0][ 8][17] =   5.073814e-05;
  val[0][0][0][ 8][18] =  -1.067974e-03; eval[0][0][0][ 8][18] =   6.357599e-05;
  val[0][0][0][ 8][19] =  -1.140700e-03; eval[0][0][0][ 8][19] =   7.469120e-05;
  val[0][0][0][ 8][20] =  -1.157266e-03; eval[0][0][0][ 8][20] =   5.663180e-05;
  val[0][0][0][ 8][21] =  -1.159559e-03; eval[0][0][0][ 8][21] =   7.476682e-05;
  val[0][0][0][ 8][22] =  -9.287554e-04; eval[0][0][0][ 8][22] =   1.062020e-04;
  val[0][0][0][ 8][23] =  -1.201396e-03; eval[0][0][0][ 8][23] =   1.237057e-04;
  val[0][0][0][ 8][24] =  -1.738776e-03; eval[0][0][0][ 8][24] =   1.925740e-04;
  val[0][0][0][ 8][25] =  -1.742138e-03; eval[0][0][0][ 8][25] =   2.056494e-04;
  val[0][0][0][ 8][26] =  -1.331944e-03; eval[0][0][0][ 8][26] =   2.533677e-04;
  val[0][0][0][ 8][27] =  -2.022684e-03; eval[0][0][0][ 8][27] =   3.154002e-04;
  val[0][0][0][ 8][28] =  -1.965606e-03; eval[0][0][0][ 8][28] =   3.478400e-04;
  val[0][0][0][ 8][29] =  -1.303526e-03; eval[0][0][0][ 8][29] =   4.138284e-04;
  //emcdphi_mean_c0d0z9
  n_val[0][0][0][9] = 30;
  val[0][0][0][ 9][ 0] =   0.000000e+00; eval[0][0][0][ 9][ 0] =   0.000000e+00;
  val[0][0][0][ 9][ 1] =   0.000000e+00; eval[0][0][0][ 9][ 1] =   0.000000e+00;
  val[0][0][0][ 9][ 2] =  -4.279534e-04; eval[0][0][0][ 9][ 2] =   1.779543e-06;
  val[0][0][0][ 9][ 3] =  -4.156876e-04; eval[0][0][0][ 9][ 3] =   1.772319e-06;
  val[0][0][0][ 9][ 4] =  -5.211537e-04; eval[0][0][0][ 9][ 4] =   2.033440e-06;
  val[0][0][0][ 9][ 5] =  -5.799404e-04; eval[0][0][0][ 9][ 5] =   2.879947e-06;
  val[0][0][0][ 9][ 6] =  -6.336784e-04; eval[0][0][0][ 9][ 6] =   3.374121e-06;
  val[0][0][0][ 9][ 7] =  -7.886997e-04; eval[0][0][0][ 9][ 7] =   4.243059e-06;
  val[0][0][0][ 9][ 8] =  -6.707893e-04; eval[0][0][0][ 9][ 8] =   5.441530e-06;
  val[0][0][0][ 9][ 9] =  -6.295368e-04; eval[0][0][0][ 9][ 9] =   7.250625e-06;
  val[0][0][0][ 9][10] =  -6.730225e-04; eval[0][0][0][ 9][10] =   9.536429e-06;
  val[0][0][0][ 9][11] =  -7.275000e-04; eval[0][0][0][ 9][11] =   1.252093e-05;
  val[0][0][0][ 9][12] =  -8.092085e-04; eval[0][0][0][ 9][12] =   1.661409e-05;
  val[0][0][0][ 9][13] =  -8.941151e-04; eval[0][0][0][ 9][13] =   2.123608e-05;
  val[0][0][0][ 9][14] =  -9.648218e-04; eval[0][0][0][ 9][14] =   2.632303e-05;
  val[0][0][0][ 9][15] =  -1.098263e-03; eval[0][0][0][ 9][15] =   3.376927e-05;
  val[0][0][0][ 9][16] =  -1.181645e-03; eval[0][0][0][ 9][16] =   4.202377e-05;
  val[0][0][0][ 9][17] =  -1.252587e-03; eval[0][0][0][ 9][17] =   5.177828e-05;
  val[0][0][0][ 9][18] =  -1.211015e-03; eval[0][0][0][ 9][18] =   8.400507e-05;
  val[0][0][0][ 9][19] =  -1.282034e-03; eval[0][0][0][ 9][19] =   7.474884e-05;
  val[0][0][0][ 9][20] =  -1.328295e-03; eval[0][0][0][ 9][20] =   5.689711e-05;
  val[0][0][0][ 9][21] =  -1.216873e-03; eval[0][0][0][ 9][21] =   7.749311e-05;
  val[0][0][0][ 9][22] =  -1.471626e-03; eval[0][0][0][ 9][22] =   1.094538e-04;
  val[0][0][0][ 9][23] =  -1.434779e-03; eval[0][0][0][ 9][23] =   1.450724e-04;
  val[0][0][0][ 9][24] =  -1.532069e-03; eval[0][0][0][ 9][24] =   1.865636e-04;
  val[0][0][0][ 9][25] =  -1.740072e-03; eval[0][0][0][ 9][25] =   2.206383e-04;
  val[0][0][0][ 9][26] =  -1.484433e-03; eval[0][0][0][ 9][26] =   2.508918e-04;
  val[0][0][0][ 9][27] =  -1.446330e-03; eval[0][0][0][ 9][27] =   3.095681e-04;
  val[0][0][0][ 9][28] =  -1.200992e-03; eval[0][0][0][ 9][28] =   3.537526e-04;
  val[0][0][0][ 9][29] =  -1.617752e-03; eval[0][0][0][ 9][29] =   4.499574e-04;
  //emcdphi_mean_c0d1z0
  n_val[0][0][1][0] = 30;
  val[0][0][1][ 0][ 0] =   0.000000e+00; eval[0][0][1][ 0][ 0] =   0.000000e+00;
  val[0][0][1][ 0][ 1] =   0.000000e+00; eval[0][0][1][ 0][ 1] =   0.000000e+00;
  val[0][0][1][ 0][ 2] =   6.313664e-03; eval[0][0][1][ 0][ 2] =   1.611571e-06;
  val[0][0][1][ 0][ 3] =   5.509555e-03; eval[0][0][1][ 0][ 3] =   1.639100e-06;
  val[0][0][1][ 0][ 4] =   4.595083e-03; eval[0][0][1][ 0][ 4] =   1.875122e-06;
  val[0][0][1][ 0][ 5] =   4.086076e-03; eval[0][0][1][ 0][ 5] =   2.139106e-06;
  val[0][0][1][ 0][ 6] =   3.699386e-03; eval[0][0][1][ 0][ 6] =   2.631999e-06;
  val[0][0][1][ 0][ 7] =   3.451835e-03; eval[0][0][1][ 0][ 7] =   4.111987e-06;
  val[0][0][1][ 0][ 8] =   3.205338e-03; eval[0][0][1][ 0][ 8] =   5.077224e-06;
  val[0][0][1][ 0][ 9] =   2.980321e-03; eval[0][0][1][ 0][ 9] =   6.748886e-06;
  val[0][0][1][ 0][10] =   2.849645e-03; eval[0][0][1][ 0][10] =   9.262110e-06;
  val[0][0][1][ 0][11] =   2.704112e-03; eval[0][0][1][ 0][11] =   1.252983e-05;
  val[0][0][1][ 0][12] =   2.548243e-03; eval[0][0][1][ 0][12] =   1.742784e-05;
  val[0][0][1][ 0][13] =   2.537436e-03; eval[0][0][1][ 0][13] =   2.198350e-05;
  val[0][0][1][ 0][14] =   2.459277e-03; eval[0][0][1][ 0][14] =   2.909038e-05;
  val[0][0][1][ 0][15] =   2.424065e-03; eval[0][0][1][ 0][15] =   3.108672e-05;
  val[0][0][1][ 0][16] =   2.341474e-03; eval[0][0][1][ 0][16] =   3.857668e-05;
  val[0][0][1][ 0][17] =   2.264072e-03; eval[0][0][1][ 0][17] =   4.774855e-05;
  val[0][0][1][ 0][18] =   2.265681e-03; eval[0][0][1][ 0][18] =   5.867640e-05;
  val[0][0][1][ 0][19] =   2.318178e-03; eval[0][0][1][ 0][19] =   8.282213e-05;
  val[0][0][1][ 0][20] =   2.336814e-03; eval[0][0][1][ 0][20] =   5.744023e-05;
  val[0][0][1][ 0][21] =   2.070081e-03; eval[0][0][1][ 0][21] =   8.249170e-05;
  val[0][0][1][ 0][22] =   1.921580e-03; eval[0][0][1][ 0][22] =   1.167834e-04;
  val[0][0][1][ 0][23] =   2.013560e-03; eval[0][0][1][ 0][23] =   1.373525e-04;
  val[0][0][1][ 0][24] =   2.316496e-03; eval[0][0][1][ 0][24] =   1.844255e-04;
  val[0][0][1][ 0][25] =   2.372940e-03; eval[0][0][1][ 0][25] =   2.225398e-04;
  val[0][0][1][ 0][26] =   2.095012e-03; eval[0][0][1][ 0][26] =   3.376593e-04;
  val[0][0][1][ 0][27] =  -4.251001e-03; eval[0][0][1][ 0][27] =   1.606796e-02;
  val[0][0][1][ 0][28] =   1.470604e-03; eval[0][0][1][ 0][28] =   4.900952e-04;
  val[0][0][1][ 0][29] =   9.859871e-04; eval[0][0][1][ 0][29] =   7.528754e-04;
  //emcdphi_mean_c0d1z1
  n_val[0][0][1][1] = 30;
  val[0][0][1][ 1][ 0] =   0.000000e+00; eval[0][0][1][ 1][ 0] =   0.000000e+00;
  val[0][0][1][ 1][ 1] =   0.000000e+00; eval[0][0][1][ 1][ 1] =   0.000000e+00;
  val[0][0][1][ 1][ 2] =   7.076542e-03; eval[0][0][1][ 1][ 2] =   1.466975e-06;
  val[0][0][1][ 1][ 3] =   5.933156e-03; eval[0][0][1][ 1][ 3] =   1.475460e-06;
  val[0][0][1][ 1][ 4] =   4.964827e-03; eval[0][0][1][ 1][ 4] =   1.872672e-06;
  val[0][0][1][ 1][ 5] =   4.259781e-03; eval[0][0][1][ 1][ 5] =   2.043182e-06;
  val[0][0][1][ 1][ 6] =   3.836289e-03; eval[0][0][1][ 1][ 6] =   2.412312e-06;
  val[0][0][1][ 1][ 7] =   3.573702e-03; eval[0][0][1][ 1][ 7] =   4.000773e-06;
  val[0][0][1][ 1][ 8] =   3.303421e-03; eval[0][0][1][ 1][ 8] =   4.944461e-06;
  val[0][0][1][ 1][ 9] =   3.084717e-03; eval[0][0][1][ 1][ 9] =   6.599011e-06;
  val[0][0][1][ 1][10] =   2.919170e-03; eval[0][0][1][ 1][10] =   8.933402e-06;
  val[0][0][1][ 1][11] =   2.758293e-03; eval[0][0][1][ 1][11] =   1.194983e-05;
  val[0][0][1][ 1][12] =   2.645234e-03; eval[0][0][1][ 1][12] =   1.587558e-05;
  val[0][0][1][ 1][13] =   2.552554e-03; eval[0][0][1][ 1][13] =   2.147722e-05;
  val[0][0][1][ 1][14] =   2.534809e-03; eval[0][0][1][ 1][14] =   2.774935e-05;
  val[0][0][1][ 1][15] =   2.386324e-03; eval[0][0][1][ 1][15] =   3.763890e-05;
  val[0][0][1][ 1][16] =   2.479270e-03; eval[0][0][1][ 1][16] =   3.600353e-05;
  val[0][0][1][ 1][17] =   2.411834e-03; eval[0][0][1][ 1][17] =   4.485926e-05;
  val[0][0][1][ 1][18] =   2.378878e-03; eval[0][0][1][ 1][18] =   6.466503e-05;
  val[0][0][1][ 1][19] =   2.416455e-03; eval[0][0][1][ 1][19] =   6.645644e-05;
  val[0][0][1][ 1][20] =   2.298712e-03; eval[0][0][1][ 1][20] =   5.025379e-05;
  val[0][0][1][ 1][21] =   2.331882e-03; eval[0][0][1][ 1][21] =   7.344341e-05;
  val[0][0][1][ 1][22] =   2.227383e-03; eval[0][0][1][ 1][22] =   1.011165e-04;
  val[0][0][1][ 1][23] =   1.912123e-03; eval[0][0][1][ 1][23] =   1.317928e-04;
  val[0][0][1][ 1][24] =   2.253637e-03; eval[0][0][1][ 1][24] =   2.152583e-04;
  val[0][0][1][ 1][25] =   1.522466e-03; eval[0][0][1][ 1][25] =   2.052378e-04;
  val[0][0][1][ 1][26] =   1.841630e-03; eval[0][0][1][ 1][26] =   2.942845e-04;
  val[0][0][1][ 1][27] =   1.842119e-03; eval[0][0][1][ 1][27] =   3.339826e-04;
  val[0][0][1][ 1][28] =   2.050193e-03; eval[0][0][1][ 1][28] =   1.882508e-02;
  val[0][0][1][ 1][29] =  -2.031640e-03; eval[0][0][1][ 1][29] =   6.640543e-02;
  //emcdphi_mean_c0d1z2
  n_val[0][0][1][2] = 30;
  val[0][0][1][ 2][ 0] =   0.000000e+00; eval[0][0][1][ 2][ 0] =   0.000000e+00;
  val[0][0][1][ 2][ 1] =   0.000000e+00; eval[0][0][1][ 2][ 1] =   0.000000e+00;
  val[0][0][1][ 2][ 2] =   7.917529e-03; eval[0][0][1][ 2][ 2] =   1.350477e-06;
  val[0][0][1][ 2][ 3] =   6.381820e-03; eval[0][0][1][ 2][ 3] =   1.518532e-06;
  val[0][0][1][ 2][ 4] =   5.296279e-03; eval[0][0][1][ 2][ 4] =   1.738577e-06;
  val[0][0][1][ 2][ 5] =   4.474697e-03; eval[0][0][1][ 2][ 5] =   2.053488e-06;
  val[0][0][1][ 2][ 6] =   3.989472e-03; eval[0][0][1][ 2][ 6] =   2.441085e-06;
  val[0][0][1][ 2][ 7] =   3.639395e-03; eval[0][0][1][ 2][ 7] =   3.220706e-06;
  val[0][0][1][ 2][ 8] =   3.336011e-03; eval[0][0][1][ 2][ 8] =   5.030485e-06;
  val[0][0][1][ 2][ 9] =   3.103823e-03; eval[0][0][1][ 2][ 9] =   6.796507e-06;
  val[0][0][1][ 2][10] =   2.942216e-03; eval[0][0][1][ 2][10] =   9.040239e-06;
  val[0][0][1][ 2][11] =   2.740467e-03; eval[0][0][1][ 2][11] =   1.228548e-05;
  val[0][0][1][ 2][12] =   2.601489e-03; eval[0][0][1][ 2][12] =   1.614503e-05;
  val[0][0][1][ 2][13] =   2.474945e-03; eval[0][0][1][ 2][13] =   2.234223e-05;
  val[0][0][1][ 2][14] =   2.412858e-03; eval[0][0][1][ 2][14] =   3.005623e-05;
  val[0][0][1][ 2][15] =   2.389212e-03; eval[0][0][1][ 2][15] =   3.723910e-05;
  val[0][0][1][ 2][16] =   2.301090e-03; eval[0][0][1][ 2][16] =   4.566500e-05;
  val[0][0][1][ 2][17] =   2.282876e-03; eval[0][0][1][ 2][17] =   5.928363e-05;
  val[0][0][1][ 2][18] =   2.323489e-03; eval[0][0][1][ 2][18] =   5.651881e-05;
  val[0][0][1][ 2][19] =   2.154517e-03; eval[0][0][1][ 2][19] =   6.706143e-05;
  val[0][0][1][ 2][20] =   2.078337e-03; eval[0][0][1][ 2][20] =   5.409583e-05;
  val[0][0][1][ 2][21] =   2.109810e-03; eval[0][0][1][ 2][21] =   7.460440e-05;
  val[0][0][1][ 2][22] =   2.055003e-03; eval[0][0][1][ 2][22] =   9.846450e-05;
  val[0][0][1][ 2][23] =   1.777117e-03; eval[0][0][1][ 2][23] =   1.395125e-04;
  val[0][0][1][ 2][24] =   1.676112e-03; eval[0][0][1][ 2][24] =   1.768015e-04;
  val[0][0][1][ 2][25] =   2.227434e-03; eval[0][0][1][ 2][25] =   1.981937e-04;
  val[0][0][1][ 2][26] =   1.789801e-03; eval[0][0][1][ 2][26] =   2.821724e-04;
  val[0][0][1][ 2][27] =   2.179554e-03; eval[0][0][1][ 2][27] =   4.706356e-04;
  val[0][0][1][ 2][28] =   2.079872e-03; eval[0][0][1][ 2][28] =   5.373204e-04;
  val[0][0][1][ 2][29] =   2.679661e-03; eval[0][0][1][ 2][29] =   4.276213e-04;
  //emcdphi_mean_c0d1z3
  n_val[0][0][1][3] = 30;
  val[0][0][1][ 3][ 0] =   0.000000e+00; eval[0][0][1][ 3][ 0] =   0.000000e+00;
  val[0][0][1][ 3][ 1] =   0.000000e+00; eval[0][0][1][ 3][ 1] =   0.000000e+00;
  val[0][0][1][ 3][ 2] =   8.319788e-03; eval[0][0][1][ 3][ 2] =   1.386496e-06;
  val[0][0][1][ 3][ 3] =   6.653545e-03; eval[0][0][1][ 3][ 3] =   1.395935e-06;
  val[0][0][1][ 3][ 4] =   5.522003e-03; eval[0][0][1][ 3][ 4] =   1.678990e-06;
  val[0][0][1][ 3][ 5] =   4.651669e-03; eval[0][0][1][ 3][ 5] =   2.131760e-06;
  val[0][0][1][ 3][ 6] =   4.168347e-03; eval[0][0][1][ 3][ 6] =   2.479235e-06;
  val[0][0][1][ 3][ 7] =   3.813152e-03; eval[0][0][1][ 3][ 7] =   3.239469e-06;
  val[0][0][1][ 3][ 8] =   3.523556e-03; eval[0][0][1][ 3][ 8] =   4.285813e-06;
  val[0][0][1][ 3][ 9] =   3.353116e-03; eval[0][0][1][ 3][ 9] =   7.202333e-06;
  val[0][0][1][ 3][10] =   3.198720e-03; eval[0][0][1][ 3][10] =   9.534537e-06;
  val[0][0][1][ 3][11] =   2.981927e-03; eval[0][0][1][ 3][11] =   1.259889e-05;
  val[0][0][1][ 3][12] =   2.841666e-03; eval[0][0][1][ 3][12] =   1.631732e-05;
  val[0][0][1][ 3][13] =   2.722931e-03; eval[0][0][1][ 3][13] =   2.178994e-05;
  val[0][0][1][ 3][14] =   2.661262e-03; eval[0][0][1][ 3][14] =   2.719648e-05;
  val[0][0][1][ 3][15] =   2.572782e-03; eval[0][0][1][ 3][15] =   3.546930e-05;
  val[0][0][1][ 3][16] =   2.675697e-03; eval[0][0][1][ 3][16] =   4.230307e-05;
  val[0][0][1][ 3][17] =   2.420287e-03; eval[0][0][1][ 3][17] =   4.608300e-05;
  val[0][0][1][ 3][18] =   2.448088e-03; eval[0][0][1][ 3][18] =   6.935113e-05;
  val[0][0][1][ 3][19] =   2.512538e-03; eval[0][0][1][ 3][19] =   6.935984e-05;
  val[0][0][1][ 3][20] =   2.375447e-03; eval[0][0][1][ 3][20] =   5.272620e-05;
  val[0][0][1][ 3][21] =   2.307735e-03; eval[0][0][1][ 3][21] =   7.389312e-05;
  val[0][0][1][ 3][22] =   2.219314e-03; eval[0][0][1][ 3][22] =   1.044049e-04;
  val[0][0][1][ 3][23] =   2.147511e-03; eval[0][0][1][ 3][23] =   1.298485e-04;
  val[0][0][1][ 3][24] =   2.698572e-03; eval[0][0][1][ 3][24] =   2.267797e-04;
  val[0][0][1][ 3][25] =   1.660486e-03; eval[0][0][1][ 3][25] =   2.169504e-04;
  val[0][0][1][ 3][26] =   2.321565e-03; eval[0][0][1][ 3][26] =   2.561666e-04;
  val[0][0][1][ 3][27] =   2.555703e-03; eval[0][0][1][ 3][27] =   3.377852e-04;
  val[0][0][1][ 3][28] =   1.677271e-03; eval[0][0][1][ 3][28] =   4.326327e-04;
  val[0][0][1][ 3][29] =   1.544010e-03; eval[0][0][1][ 3][29] =   4.405967e-04;
  //emcdphi_mean_c0d1z4
  n_val[0][0][1][4] = 30;
  val[0][0][1][ 4][ 0] =   0.000000e+00; eval[0][0][1][ 4][ 0] =   0.000000e+00;
  val[0][0][1][ 4][ 1] =   0.000000e+00; eval[0][0][1][ 4][ 1] =   0.000000e+00;
  val[0][0][1][ 4][ 2] =   8.589084e-03; eval[0][0][1][ 4][ 2] =   1.720367e-06;
  val[0][0][1][ 4][ 3] =   6.880628e-03; eval[0][0][1][ 4][ 3] =   1.627854e-06;
  val[0][0][1][ 4][ 4] =   5.694921e-03; eval[0][0][1][ 4][ 4] =   1.932060e-06;
  val[0][0][1][ 4][ 5] =   4.807450e-03; eval[0][0][1][ 4][ 5] =   2.760395e-06;
  val[0][0][1][ 4][ 6] =   4.200672e-03; eval[0][0][1][ 4][ 6] =   2.848198e-06;
  val[0][0][1][ 4][ 7] =   3.774794e-03; eval[0][0][1][ 4][ 7] =   3.767368e-06;
  val[0][0][1][ 4][ 8] =   3.468723e-03; eval[0][0][1][ 4][ 8] =   5.083509e-06;
  val[0][0][1][ 4][ 9] =   3.231666e-03; eval[0][0][1][ 4][ 9] =   8.431282e-06;
  val[0][0][1][ 4][10] =   3.016025e-03; eval[0][0][1][ 4][10] =   1.120087e-05;
  val[0][0][1][ 4][11] =   2.797208e-03; eval[0][0][1][ 4][11] =   1.523581e-05;
  val[0][0][1][ 4][12] =   2.631645e-03; eval[0][0][1][ 4][12] =   1.978239e-05;
  val[0][0][1][ 4][13] =   2.507397e-03; eval[0][0][1][ 4][13] =   2.706927e-05;
  val[0][0][1][ 4][14] =   2.419463e-03; eval[0][0][1][ 4][14] =   2.695386e-05;
  val[0][0][1][ 4][15] =   2.363924e-03; eval[0][0][1][ 4][15] =   4.379824e-05;
  val[0][0][1][ 4][16] =   2.242389e-03; eval[0][0][1][ 4][16] =   5.905477e-05;
  val[0][0][1][ 4][17] =   2.200400e-03; eval[0][0][1][ 4][17] =   5.210414e-05;
  val[0][0][1][ 4][18] =   2.139916e-03; eval[0][0][1][ 4][18] =   7.331779e-05;
  val[0][0][1][ 4][19] =   2.231313e-03; eval[0][0][1][ 4][19] =   8.298032e-05;
  val[0][0][1][ 4][20] =   2.030986e-03; eval[0][0][1][ 4][20] =   6.412714e-05;
  val[0][0][1][ 4][21] =   2.047632e-03; eval[0][0][1][ 4][21] =   9.167474e-05;
  val[0][0][1][ 4][22] =   1.806532e-03; eval[0][0][1][ 4][22] =   1.380893e-04;
  val[0][0][1][ 4][23] =   2.011627e-03; eval[0][0][1][ 4][23] =   1.625254e-04;
  val[0][0][1][ 4][24] =   1.975779e-03; eval[0][0][1][ 4][24] =   2.000578e-04;
  val[0][0][1][ 4][25] =   1.571897e-03; eval[0][0][1][ 4][25] =   3.404149e-04;
  val[0][0][1][ 4][26] =   2.127672e-03; eval[0][0][1][ 4][26] =   2.969303e-04;
  val[0][0][1][ 4][27] =   1.845075e-03; eval[0][0][1][ 4][27] =   4.009723e-04;
  val[0][0][1][ 4][28] =   1.428505e-03; eval[0][0][1][ 4][28] =   8.823235e-04;
  val[0][0][1][ 4][29] =   1.842989e-03; eval[0][0][1][ 4][29] =   7.028021e-04;
  //emcdphi_mean_c0d1z5
  n_val[0][0][1][5] = 30;
  val[0][0][1][ 5][ 0] =   0.000000e+00; eval[0][0][1][ 5][ 0] =   0.000000e+00;
  val[0][0][1][ 5][ 1] =   0.000000e+00; eval[0][0][1][ 5][ 1] =   0.000000e+00;
  val[0][0][1][ 5][ 2] =   8.638607e-03; eval[0][0][1][ 5][ 2] =   1.843935e-06;
  val[0][0][1][ 5][ 3] =   7.130999e-03; eval[0][0][1][ 5][ 3] =   1.705825e-06;
  val[0][0][1][ 5][ 4] =   6.025925e-03; eval[0][0][1][ 5][ 4] =   2.018788e-06;
  val[0][0][1][ 5][ 5] =   5.082143e-03; eval[0][0][1][ 5][ 5] =   2.939125e-06;
  val[0][0][1][ 5][ 6] =   4.436232e-03; eval[0][0][1][ 5][ 6] =   3.067478e-06;
  val[0][0][1][ 5][ 7] =   3.997550e-03; eval[0][0][1][ 5][ 7] =   3.998997e-06;
  val[0][0][1][ 5][ 8] =   3.672038e-03; eval[0][0][1][ 5][ 8] =   5.221875e-06;
  val[0][0][1][ 5][ 9] =   3.496108e-03; eval[0][0][1][ 5][ 9] =   7.149151e-06;
  val[0][0][1][ 5][10] =   3.443788e-03; eval[0][0][1][ 5][10] =   9.864032e-06;
  val[0][0][1][ 5][11] =   3.303925e-03; eval[0][0][1][ 5][11] =   1.523502e-05;
  val[0][0][1][ 5][12] =   3.133584e-03; eval[0][0][1][ 5][12] =   1.957753e-05;
  val[0][0][1][ 5][13] =   3.096050e-03; eval[0][0][1][ 5][13] =   2.512154e-05;
  val[0][0][1][ 5][14] =   3.014792e-03; eval[0][0][1][ 5][14] =   3.003986e-05;
  val[0][0][1][ 5][15] =   2.929801e-03; eval[0][0][1][ 5][15] =   3.918408e-05;
  val[0][0][1][ 5][16] =   2.862878e-03; eval[0][0][1][ 5][16] =   4.758105e-05;
  val[0][0][1][ 5][17] =   2.791118e-03; eval[0][0][1][ 5][17] =   5.905589e-05;
  val[0][0][1][ 5][18] =   2.830534e-03; eval[0][0][1][ 5][18] =   6.404391e-05;
  val[0][0][1][ 5][19] =   2.864304e-03; eval[0][0][1][ 5][19] =   8.236467e-05;
  val[0][0][1][ 5][20] =   2.625461e-03; eval[0][0][1][ 5][20] =   6.144280e-05;
  val[0][0][1][ 5][21] =   2.619045e-03; eval[0][0][1][ 5][21] =   9.183031e-05;
  val[0][0][1][ 5][22] =   2.666014e-03; eval[0][0][1][ 5][22] =   1.188641e-04;
  val[0][0][1][ 5][23] =   2.438738e-03; eval[0][0][1][ 5][23] =   1.599427e-04;
  val[0][0][1][ 5][24] =   2.409709e-03; eval[0][0][1][ 5][24] =   1.732006e-04;
  val[0][0][1][ 5][25] =   2.174005e-03; eval[0][0][1][ 5][25] =   2.291557e-04;
  val[0][0][1][ 5][26] =   2.322751e-03; eval[0][0][1][ 5][26] =   3.373686e-04;
  val[0][0][1][ 5][27] =   2.741255e-03; eval[0][0][1][ 5][27] =   2.862927e-04;
  val[0][0][1][ 5][28] =   3.000946e-03; eval[0][0][1][ 5][28] =   5.666994e-04;
  val[0][0][1][ 5][29] =   2.669318e-03; eval[0][0][1][ 5][29] =   5.067667e-04;
  //emcdphi_mean_c0d1z6
  n_val[0][0][1][6] = 30;
  val[0][0][1][ 6][ 0] =   0.000000e+00; eval[0][0][1][ 6][ 0] =   0.000000e+00;
  val[0][0][1][ 6][ 1] =   0.000000e+00; eval[0][0][1][ 6][ 1] =   0.000000e+00;
  val[0][0][1][ 6][ 2] =   8.201803e-03; eval[0][0][1][ 6][ 2] =   1.532762e-06;
  val[0][0][1][ 6][ 3] =   6.729213e-03; eval[0][0][1][ 6][ 3] =   1.410042e-06;
  val[0][0][1][ 6][ 4] =   5.817963e-03; eval[0][0][1][ 6][ 4] =   1.663779e-06;
  val[0][0][1][ 6][ 5] =   4.986230e-03; eval[0][0][1][ 6][ 5] =   2.367535e-06;
  val[0][0][1][ 6][ 6] =   4.385049e-03; eval[0][0][1][ 6][ 6] =   2.505951e-06;
  val[0][0][1][ 6][ 7] =   3.988102e-03; eval[0][0][1][ 6][ 7] =   3.283106e-06;
  val[0][0][1][ 6][ 8] =   3.669977e-03; eval[0][0][1][ 6][ 8] =   4.265197e-06;
  val[0][0][1][ 6][ 9] =   3.469580e-03; eval[0][0][1][ 6][ 9] =   5.856118e-06;
  val[0][0][1][ 6][10] =   3.428332e-03; eval[0][0][1][ 6][10] =   9.524659e-06;
  val[0][0][1][ 6][11] =   3.271395e-03; eval[0][0][1][ 6][11] =   1.219648e-05;
  val[0][0][1][ 6][12] =   3.126642e-03; eval[0][0][1][ 6][12] =   1.566822e-05;
  val[0][0][1][ 6][13] =   3.061245e-03; eval[0][0][1][ 6][13] =   2.000877e-05;
  val[0][0][1][ 6][14] =   3.000165e-03; eval[0][0][1][ 6][14] =   2.495336e-05;
  val[0][0][1][ 6][15] =   2.927676e-03; eval[0][0][1][ 6][15] =   2.908290e-05;
  val[0][0][1][ 6][16] =   2.933254e-03; eval[0][0][1][ 6][16] =   3.861908e-05;
  val[0][0][1][ 6][17] =   2.757197e-03; eval[0][0][1][ 6][17] =   4.684894e-05;
  val[0][0][1][ 6][18] =   2.749039e-03; eval[0][0][1][ 6][18] =   6.074968e-05;
  val[0][0][1][ 6][19] =   2.769996e-03; eval[0][0][1][ 6][19] =   6.925310e-05;
  val[0][0][1][ 6][20] =   2.615283e-03; eval[0][0][1][ 6][20] =   5.504860e-05;
  val[0][0][1][ 6][21] =   2.637158e-03; eval[0][0][1][ 6][21] =   7.410342e-05;
  val[0][0][1][ 6][22] =   2.497554e-03; eval[0][0][1][ 6][22] =   1.129827e-04;
  val[0][0][1][ 6][23] =   2.422723e-03; eval[0][0][1][ 6][23] =   1.480406e-04;
  val[0][0][1][ 6][24] =   2.657085e-03; eval[0][0][1][ 6][24] =   1.883246e-04;
  val[0][0][1][ 6][25] =   2.571682e-03; eval[0][0][1][ 6][25] =   2.092585e-04;
  val[0][0][1][ 6][26] =   3.109643e-03; eval[0][0][1][ 6][26] =   2.728776e-04;
  val[0][0][1][ 6][27] =   2.272993e-03; eval[0][0][1][ 6][27] =   3.127706e-04;
  val[0][0][1][ 6][28] =   2.946753e-03; eval[0][0][1][ 6][28] =   3.635995e-04;
  val[0][0][1][ 6][29] =   3.424764e-03; eval[0][0][1][ 6][29] =   3.647088e-04;
  //emcdphi_mean_c0d1z7
  n_val[0][0][1][7] = 30;
  val[0][0][1][ 7][ 0] =   0.000000e+00; eval[0][0][1][ 7][ 0] =   0.000000e+00;
  val[0][0][1][ 7][ 1] =   0.000000e+00; eval[0][0][1][ 7][ 1] =   0.000000e+00;
  val[0][0][1][ 7][ 2] =   7.604698e-03; eval[0][0][1][ 7][ 2] =   1.481357e-06;
  val[0][0][1][ 7][ 3] =   6.212012e-03; eval[0][0][1][ 7][ 3] =   1.640825e-06;
  val[0][0][1][ 7][ 4] =   5.334875e-03; eval[0][0][1][ 7][ 4] =   1.795367e-06;
  val[0][0][1][ 7][ 5] =   4.534169e-03; eval[0][0][1][ 7][ 5] =   2.191177e-06;
  val[0][0][1][ 7][ 6] =   4.031185e-03; eval[0][0][1][ 7][ 6] =   2.585447e-06;
  val[0][0][1][ 7][ 7] =   3.702542e-03; eval[0][0][1][ 7][ 7] =   3.376258e-06;
  val[0][0][1][ 7][ 8] =   3.448441e-03; eval[0][0][1][ 7][ 8] =   5.358098e-06;
  val[0][0][1][ 7][ 9] =   3.287186e-03; eval[0][0][1][ 7][ 9] =   7.074504e-06;
  val[0][0][1][ 7][10] =   3.205637e-03; eval[0][0][1][ 7][10] =   9.265666e-06;
  val[0][0][1][ 7][11] =   2.989563e-03; eval[0][0][1][ 7][11] =   1.180066e-05;
  val[0][0][1][ 7][12] =   2.817156e-03; eval[0][0][1][ 7][12] =   1.554261e-05;
  val[0][0][1][ 7][13] =   2.773575e-03; eval[0][0][1][ 7][13] =   1.971749e-05;
  val[0][0][1][ 7][14] =   2.688781e-03; eval[0][0][1][ 7][14] =   2.486326e-05;
  val[0][0][1][ 7][15] =   2.585724e-03; eval[0][0][1][ 7][15] =   3.417247e-05;
  val[0][0][1][ 7][16] =   2.594873e-03; eval[0][0][1][ 7][16] =   4.080807e-05;
  val[0][0][1][ 7][17] =   2.386342e-03; eval[0][0][1][ 7][17] =   4.862534e-05;
  val[0][0][1][ 7][18] =   2.472803e-03; eval[0][0][1][ 7][18] =   5.923423e-05;
  val[0][0][1][ 7][19] =   2.273378e-03; eval[0][0][1][ 7][19] =   6.532223e-05;
  val[0][0][1][ 7][20] =   2.294946e-03; eval[0][0][1][ 7][20] =   5.105702e-05;
  val[0][0][1][ 7][21] =   2.360284e-03; eval[0][0][1][ 7][21] =   7.280568e-05;
  val[0][0][1][ 7][22] =   2.474586e-03; eval[0][0][1][ 7][22] =   1.086115e-04;
  val[0][0][1][ 7][23] =   2.230133e-03; eval[0][0][1][ 7][23] =   1.331924e-04;
  val[0][0][1][ 7][24] =   2.082276e-03; eval[0][0][1][ 7][24] =   1.554184e-04;
  val[0][0][1][ 7][25] =   2.326052e-03; eval[0][0][1][ 7][25] =   2.141120e-04;
  val[0][0][1][ 7][26] =   2.335160e-03; eval[0][0][1][ 7][26] =   2.535058e-04;
  val[0][0][1][ 7][27] =   2.275685e-03; eval[0][0][1][ 7][27] =   3.237370e-04;
  val[0][0][1][ 7][28] =   2.215281e-03; eval[0][0][1][ 7][28] =   2.995750e-04;
  val[0][0][1][ 7][29] =   1.521558e-03; eval[0][0][1][ 7][29] =   6.133570e-04;
  //emcdphi_mean_c0d1z8
  n_val[0][0][1][8] = 30;
  val[0][0][1][ 8][ 0] =   0.000000e+00; eval[0][0][1][ 8][ 0] =   0.000000e+00;
  val[0][0][1][ 8][ 1] =   0.000000e+00; eval[0][0][1][ 8][ 1] =   0.000000e+00;
  val[0][0][1][ 8][ 2] =   6.581505e-03; eval[0][0][1][ 8][ 2] =   1.517324e-06;
  val[0][0][1][ 8][ 3] =   5.619283e-03; eval[0][0][1][ 8][ 3] =   1.588480e-06;
  val[0][0][1][ 8][ 4] =   4.758364e-03; eval[0][0][1][ 8][ 4] =   1.882961e-06;
  val[0][0][1][ 8][ 5] =   3.962018e-03; eval[0][0][1][ 8][ 5] =   2.103994e-06;
  val[0][0][1][ 8][ 6] =   3.540413e-03; eval[0][0][1][ 8][ 6] =   2.627965e-06;
  val[0][0][1][ 8][ 7] =   3.219077e-03; eval[0][0][1][ 8][ 7] =   4.077241e-06;
  val[0][0][1][ 8][ 8] =   2.920638e-03; eval[0][0][1][ 8][ 8] =   5.350606e-06;
  val[0][0][1][ 8][ 9] =   2.762992e-03; eval[0][0][1][ 8][ 9] =   7.354472e-06;
  val[0][0][1][ 8][10] =   2.666728e-03; eval[0][0][1][ 8][10] =   1.031715e-05;
  val[0][0][1][ 8][11] =   2.557004e-03; eval[0][0][1][ 8][11] =   1.085934e-05;
  val[0][0][1][ 8][12] =   2.421008e-03; eval[0][0][1][ 8][12] =   1.393904e-05;
  val[0][0][1][ 8][13] =   2.311701e-03; eval[0][0][1][ 8][13] =   1.832077e-05;
  val[0][0][1][ 8][14] =   2.297778e-03; eval[0][0][1][ 8][14] =   2.393802e-05;
  val[0][0][1][ 8][15] =   2.187292e-03; eval[0][0][1][ 8][15] =   2.947037e-05;
  val[0][0][1][ 8][16] =   2.163221e-03; eval[0][0][1][ 8][16] =   3.699891e-05;
  val[0][0][1][ 8][17] =   2.141429e-03; eval[0][0][1][ 8][17] =   4.606973e-05;
  val[0][0][1][ 8][18] =   2.112544e-03; eval[0][0][1][ 8][18] =   5.758589e-05;
  val[0][0][1][ 8][19] =   2.060295e-03; eval[0][0][1][ 8][19] =   6.685106e-05;
  val[0][0][1][ 8][20] =   2.106273e-03; eval[0][0][1][ 8][20] =   6.077106e-05;
  val[0][0][1][ 8][21] =   2.107566e-03; eval[0][0][1][ 8][21] =   8.366359e-05;
  val[0][0][1][ 8][22] =   2.026826e-03; eval[0][0][1][ 8][22] =   1.018594e-04;
  val[0][0][1][ 8][23] =   2.093715e-03; eval[0][0][1][ 8][23] =   1.520204e-04;
  val[0][0][1][ 8][24] =   2.331427e-03; eval[0][0][1][ 8][24] =   2.305887e-04;
  val[0][0][1][ 8][25] =   1.968649e-03; eval[0][0][1][ 8][25] =   2.174773e-04;
  val[0][0][1][ 8][26] =   2.773237e-03; eval[0][0][1][ 8][26] =   2.695905e-04;
  val[0][0][1][ 8][27] =   2.089823e-03; eval[0][0][1][ 8][27] =   3.616618e-04;
  val[0][0][1][ 8][28] =   2.552154e-03; eval[0][0][1][ 8][28] =   3.931578e-04;
  val[0][0][1][ 8][29] =   3.080040e-03; eval[0][0][1][ 8][29] =   5.631811e-04;
  //emcdphi_mean_c0d1z9
  n_val[0][0][1][9] = 30;
  val[0][0][1][ 9][ 0] =   0.000000e+00; eval[0][0][1][ 9][ 0] =   0.000000e+00;
  val[0][0][1][ 9][ 1] =   0.000000e+00; eval[0][0][1][ 9][ 1] =   0.000000e+00;
  val[0][0][1][ 9][ 2] =   5.382910e-03; eval[0][0][1][ 9][ 2] =   1.774416e-06;
  val[0][0][1][ 9][ 3] =   5.069520e-03; eval[0][0][1][ 9][ 3] =   1.602341e-06;
  val[0][0][1][ 9][ 4] =   4.470906e-03; eval[0][0][1][ 9][ 4] =   1.989137e-06;
  val[0][0][1][ 9][ 5] =   3.839790e-03; eval[0][0][1][ 9][ 5] =   2.428584e-06;
  val[0][0][1][ 9][ 6] =   3.441602e-03; eval[0][0][1][ 9][ 6] =   3.071196e-06;
  val[0][0][1][ 9][ 7] =   3.161161e-03; eval[0][0][1][ 9][ 7] =   4.525304e-06;
  val[0][0][1][ 9][ 8] =   2.876820e-03; eval[0][0][1][ 9][ 8] =   5.879544e-06;
  val[0][0][1][ 9][ 9] =   2.696972e-03; eval[0][0][1][ 9][ 9] =   8.215738e-06;
  val[0][0][1][ 9][10] =   2.633805e-03; eval[0][0][1][ 9][10] =   9.598857e-06;
  val[0][0][1][ 9][11] =   2.518243e-03; eval[0][0][1][ 9][11] =   1.259724e-05;
  val[0][0][1][ 9][12] =   2.328234e-03; eval[0][0][1][ 9][12] =   1.591592e-05;
  val[0][0][1][ 9][13] =   2.244135e-03; eval[0][0][1][ 9][13] =   2.030048e-05;
  val[0][0][1][ 9][14] =   2.134606e-03; eval[0][0][1][ 9][14] =   2.622910e-05;
  val[0][0][1][ 9][15] =   2.130935e-03; eval[0][0][1][ 9][15] =   3.208929e-05;
  val[0][0][1][ 9][16] =   2.202729e-03; eval[0][0][1][ 9][16] =   4.213722e-05;
  val[0][0][1][ 9][17] =   1.979183e-03; eval[0][0][1][ 9][17] =   5.006097e-05;
  val[0][0][1][ 9][18] =   2.048974e-03; eval[0][0][1][ 9][18] =   6.046369e-05;
  val[0][0][1][ 9][19] =   2.127059e-03; eval[0][0][1][ 9][19] =   7.468313e-05;
  val[0][0][1][ 9][20] =   1.986584e-03; eval[0][0][1][ 9][20] =   5.782742e-05;
  val[0][0][1][ 9][21] =   1.927278e-03; eval[0][0][1][ 9][21] =   8.365426e-05;
  val[0][0][1][ 9][22] =   2.028589e-03; eval[0][0][1][ 9][22] =   1.123526e-04;
  val[0][0][1][ 9][23] =   2.204045e-03; eval[0][0][1][ 9][23] =   1.514681e-04;
  val[0][0][1][ 9][24] =   2.432899e-03; eval[0][0][1][ 9][24] =   2.691683e-04;
  val[0][0][1][ 9][25] =   2.091882e-03; eval[0][0][1][ 9][25] =   2.485804e-04;
  val[0][0][1][ 9][26] =   2.169458e-03; eval[0][0][1][ 9][26] =   2.877376e-04;
  val[0][0][1][ 9][27] =   1.393248e-03; eval[0][0][1][ 9][27] =   3.952153e-04;
  val[0][0][1][ 9][28] =   4.830312e-02; eval[0][0][1][ 9][28] =   2.267528e-02;
  val[0][0][1][ 9][29] =   9.849770e-04; eval[0][0][1][ 9][29] =   4.654476e-04;
  //emcdphi_mean_c1d0z0
  n_val[0][1][0][0] = 30;
  val[0][1][0][ 0][ 0] =   0.000000e+00; eval[0][1][0][ 0][ 0] =   0.000000e+00;
  val[0][1][0][ 0][ 1] =   0.000000e+00; eval[0][1][0][ 0][ 1] =   0.000000e+00;
  val[0][1][0][ 0][ 2] =  -4.537163e-03; eval[0][1][0][ 0][ 2] =   1.682082e-06;
  val[0][1][0][ 0][ 3] =  -4.061249e-03; eval[0][1][0][ 0][ 3] =   1.839159e-06;
  val[0][1][0][ 0][ 4] =  -3.836794e-03; eval[0][1][0][ 0][ 4] =   2.254640e-06;
  val[0][1][0][ 0][ 5] =  -3.840395e-03; eval[0][1][0][ 0][ 5] =   2.886847e-06;
  val[0][1][0][ 0][ 6] =  -3.676845e-03; eval[0][1][0][ 0][ 6] =   3.657875e-06;
  val[0][1][0][ 0][ 7] =  -3.545456e-03; eval[0][1][0][ 0][ 7] =   5.442737e-06;
  val[0][1][0][ 0][ 8] =  -3.516937e-03; eval[0][1][0][ 0][ 8] =   7.006279e-06;
  val[0][1][0][ 0][ 9] =  -3.493197e-03; eval[0][1][0][ 0][ 9] =   9.266170e-06;
  val[0][1][0][ 0][10] =  -3.430183e-03; eval[0][1][0][ 0][10] =   1.225273e-05;
  val[0][1][0][ 0][11] =  -3.452394e-03; eval[0][1][0][ 0][11] =   1.604587e-05;
  val[0][1][0][ 0][12] =  -3.401089e-03; eval[0][1][0][ 0][12] =   2.067733e-05;
  val[0][1][0][ 0][13] =  -3.389661e-03; eval[0][1][0][ 0][13] =   2.633282e-05;
  val[0][1][0][ 0][14] =  -3.434538e-03; eval[0][1][0][ 0][14] =   3.517396e-05;
  val[0][1][0][ 0][15] =  -3.491569e-03; eval[0][1][0][ 0][15] =   4.519444e-05;
  val[0][1][0][ 0][16] =  -3.313426e-03; eval[0][1][0][ 0][16] =   4.942519e-05;
  val[0][1][0][ 0][17] =  -3.392672e-03; eval[0][1][0][ 0][17] =   7.052003e-05;
  val[0][1][0][ 0][18] =  -3.434670e-03; eval[0][1][0][ 0][18] =   8.178712e-05;
  val[0][1][0][ 0][19] =  -3.310838e-03; eval[0][1][0][ 0][19] =   1.008425e-04;
  val[0][1][0][ 0][20] =  -3.217298e-03; eval[0][1][0][ 0][20] =   6.939054e-05;
  val[0][1][0][ 0][21] =  -3.165546e-03; eval[0][1][0][ 0][21] =   1.004680e-04;
  val[0][1][0][ 0][22] =  -2.919366e-03; eval[0][1][0][ 0][22] =   1.293538e-04;
  val[0][1][0][ 0][23] =  -2.978805e-03; eval[0][1][0][ 0][23] =   1.976541e-04;
  val[0][1][0][ 0][24] =  -3.145549e-03; eval[0][1][0][ 0][24] =   2.204585e-04;
  val[0][1][0][ 0][25] =  -3.309462e-03; eval[0][1][0][ 0][25] =   3.243558e-04;
  val[0][1][0][ 0][26] =  -3.292521e-03; eval[0][1][0][ 0][26] =   3.414735e-04;
  val[0][1][0][ 0][27] =  -2.825269e-03; eval[0][1][0][ 0][27] =   4.438994e-04;
  val[0][1][0][ 0][28] =  -1.043154e-03; eval[0][1][0][ 0][28] =   6.654038e-04;
  val[0][1][0][ 0][29] =   1.675727e-01; eval[0][1][0][ 0][29] =   4.627225e+00;
  //emcdphi_mean_c1d0z1
  n_val[0][1][0][1] = 30;
  val[0][1][0][ 1][ 0] =   0.000000e+00; eval[0][1][0][ 1][ 0] =   0.000000e+00;
  val[0][1][0][ 1][ 1] =   0.000000e+00; eval[0][1][0][ 1][ 1] =   0.000000e+00;
  val[0][1][0][ 1][ 2] =  -5.427583e-03; eval[0][1][0][ 1][ 2] =   1.687381e-06;
  val[0][1][0][ 1][ 3] =  -4.635518e-03; eval[0][1][0][ 1][ 3] =   1.868203e-06;
  val[0][1][0][ 1][ 4] =  -4.298861e-03; eval[0][1][0][ 1][ 4] =   2.241813e-06;
  val[0][1][0][ 1][ 5] =  -4.244126e-03; eval[0][1][0][ 1][ 5] =   2.779265e-06;
  val[0][1][0][ 1][ 6] =  -3.942396e-03; eval[0][1][0][ 1][ 6] =   3.524880e-06;
  val[0][1][0][ 1][ 7] =  -3.720273e-03; eval[0][1][0][ 1][ 7] =   5.294908e-06;
  val[0][1][0][ 1][ 8] =  -3.657251e-03; eval[0][1][0][ 1][ 8] =   6.791248e-06;
  val[0][1][0][ 1][ 9] =  -3.549640e-03; eval[0][1][0][ 1][ 9] =   9.017996e-06;
  val[0][1][0][ 1][10] =  -3.525855e-03; eval[0][1][0][ 1][10] =   1.197960e-05;
  val[0][1][0][ 1][11] =  -3.476677e-03; eval[0][1][0][ 1][11] =   1.519963e-05;
  val[0][1][0][ 1][12] =  -3.444603e-03; eval[0][1][0][ 1][12] =   1.964956e-05;
  val[0][1][0][ 1][13] =  -3.431032e-03; eval[0][1][0][ 1][13] =   2.576856e-05;
  val[0][1][0][ 1][14] =  -3.471181e-03; eval[0][1][0][ 1][14] =   3.366695e-05;
  val[0][1][0][ 1][15] =  -3.478158e-03; eval[0][1][0][ 1][15] =   4.285298e-05;
  val[0][1][0][ 1][16] =  -3.451891e-03; eval[0][1][0][ 1][16] =   5.345319e-05;
  val[0][1][0][ 1][17] =  -3.440901e-03; eval[0][1][0][ 1][17] =   6.041653e-05;
  val[0][1][0][ 1][18] =  -3.233186e-03; eval[0][1][0][ 1][18] =   7.627916e-05;
  val[0][1][0][ 1][19] =  -3.214978e-03; eval[0][1][0][ 1][19] =   9.425759e-05;
  val[0][1][0][ 1][20] =  -3.316943e-03; eval[0][1][0][ 1][20] =   7.552979e-05;
  val[0][1][0][ 1][21] =  -3.448937e-03; eval[0][1][0][ 1][21] =   9.576888e-05;
  val[0][1][0][ 1][22] =  -3.405036e-03; eval[0][1][0][ 1][22] =   1.421288e-04;
  val[0][1][0][ 1][23] =  -2.955416e-03; eval[0][1][0][ 1][23] =   1.773560e-04;
  val[0][1][0][ 1][24] =  -3.141293e-03; eval[0][1][0][ 1][24] =   1.958131e-04;
  val[0][1][0][ 1][25] =  -3.252644e-03; eval[0][1][0][ 1][25] =   2.956059e-04;
  val[0][1][0][ 1][26] =  -3.644988e-03; eval[0][1][0][ 1][26] =   3.225634e-04;
  val[0][1][0][ 1][27] =  -2.859913e-03; eval[0][1][0][ 1][27] =   3.759998e-04;
  val[0][1][0][ 1][28] =  -3.036168e-03; eval[0][1][0][ 1][28] =   5.320787e-04;
  val[0][1][0][ 1][29] =  -2.679922e-03; eval[0][1][0][ 1][29] =   6.542135e-04;
  //emcdphi_mean_c1d0z2
  n_val[0][1][0][2] = 30;
  val[0][1][0][ 2][ 0] =   0.000000e+00; eval[0][1][0][ 2][ 0] =   0.000000e+00;
  val[0][1][0][ 2][ 1] =   0.000000e+00; eval[0][1][0][ 2][ 1] =   0.000000e+00;
  val[0][1][0][ 2][ 2] =  -6.306484e-03; eval[0][1][0][ 2][ 2] =   1.654897e-06;
  val[0][1][0][ 2][ 3] =  -5.041151e-03; eval[0][1][0][ 2][ 3] =   1.639087e-06;
  val[0][1][0][ 2][ 4] =  -4.572651e-03; eval[0][1][0][ 2][ 4] =   2.284998e-06;
  val[0][1][0][ 2][ 5] =  -4.335647e-03; eval[0][1][0][ 2][ 5] =   2.708972e-06;
  val[0][1][0][ 2][ 6] =  -4.082414e-03; eval[0][1][0][ 2][ 6] =   3.470430e-06;
  val[0][1][0][ 2][ 7] =  -3.839848e-03; eval[0][1][0][ 2][ 7] =   4.627319e-06;
  val[0][1][0][ 2][ 8] =  -3.709664e-03; eval[0][1][0][ 2][ 8] =   5.878187e-06;
  val[0][1][0][ 2][ 9] =  -3.658459e-03; eval[0][1][0][ 2][ 9] =   9.563197e-06;
  val[0][1][0][ 2][10] =  -3.525224e-03; eval[0][1][0][ 2][10] =   1.061602e-05;
  val[0][1][0][ 2][11] =  -3.440592e-03; eval[0][1][0][ 2][11] =   1.589896e-05;
  val[0][1][0][ 2][12] =  -3.353441e-03; eval[0][1][0][ 2][12] =   2.026741e-05;
  val[0][1][0][ 2][13] =  -3.342871e-03; eval[0][1][0][ 2][13] =   2.665741e-05;
  val[0][1][0][ 2][14] =  -3.338808e-03; eval[0][1][0][ 2][14] =   3.389118e-05;
  val[0][1][0][ 2][15] =  -3.341810e-03; eval[0][1][0][ 2][15] =   4.382246e-05;
  val[0][1][0][ 2][16] =  -3.169102e-03; eval[0][1][0][ 2][16] =   5.280821e-05;
  val[0][1][0][ 2][17] =  -3.189194e-03; eval[0][1][0][ 2][17] =   6.473503e-05;
  val[0][1][0][ 2][18] =  -3.152450e-03; eval[0][1][0][ 2][18] =   7.925301e-05;
  val[0][1][0][ 2][19] =  -3.140602e-03; eval[0][1][0][ 2][19] =   9.437393e-05;
  val[0][1][0][ 2][20] =  -3.091276e-03; eval[0][1][0][ 2][20] =   7.359577e-05;
  val[0][1][0][ 2][21] =  -3.075998e-03; eval[0][1][0][ 2][21] =   1.010129e-04;
  val[0][1][0][ 2][22] =  -3.346354e-03; eval[0][1][0][ 2][22] =   1.324433e-04;
  val[0][1][0][ 2][23] =  -3.077743e-03; eval[0][1][0][ 2][23] =   1.776987e-04;
  val[0][1][0][ 2][24] =  -3.350333e-03; eval[0][1][0][ 2][24] =   2.410630e-04;
  val[0][1][0][ 2][25] =  -3.032250e-03; eval[0][1][0][ 2][25] =   3.194097e-04;
  val[0][1][0][ 2][26] =  -3.066565e-03; eval[0][1][0][ 2][26] =   3.690668e-04;
  val[0][1][0][ 2][27] =  -3.432588e-03; eval[0][1][0][ 2][27] =   6.222406e-04;
  val[0][1][0][ 2][28] =  -2.497564e-03; eval[0][1][0][ 2][28] =   6.992751e-04;
  val[0][1][0][ 2][29] =  -3.087619e-03; eval[0][1][0][ 2][29] =   4.604520e-04;
  //emcdphi_mean_c1d0z3
  n_val[0][1][0][3] = 30;
  val[0][1][0][ 3][ 0] =   0.000000e+00; eval[0][1][0][ 3][ 0] =   0.000000e+00;
  val[0][1][0][ 3][ 1] =   0.000000e+00; eval[0][1][0][ 3][ 1] =   0.000000e+00;
  val[0][1][0][ 3][ 2] =  -6.714007e-03; eval[0][1][0][ 3][ 2] =   1.625877e-06;
  val[0][1][0][ 3][ 3] =  -5.342976e-03; eval[0][1][0][ 3][ 3] =   1.850717e-06;
  val[0][1][0][ 3][ 4] =  -4.837584e-03; eval[0][1][0][ 3][ 4] =   2.558740e-06;
  val[0][1][0][ 3][ 5] =  -4.550090e-03; eval[0][1][0][ 3][ 5] =   3.045508e-06;
  val[0][1][0][ 3][ 6] =  -4.173599e-03; eval[0][1][0][ 3][ 6] =   3.431918e-06;
  val[0][1][0][ 3][ 7] =  -3.959174e-03; eval[0][1][0][ 3][ 7] =   4.493729e-06;
  val[0][1][0][ 3][ 8] =  -3.910459e-03; eval[0][1][0][ 3][ 8] =   6.873863e-06;
  val[0][1][0][ 3][ 9] =  -3.806573e-03; eval[0][1][0][ 3][ 9] =   9.254383e-06;
  val[0][1][0][ 3][10] =  -3.768257e-03; eval[0][1][0][ 3][10] =   1.215706e-05;
  val[0][1][0][ 3][11] =  -3.612973e-03; eval[0][1][0][ 3][11] =   1.540036e-05;
  val[0][1][0][ 3][12] =  -3.537642e-03; eval[0][1][0][ 3][12] =   2.002900e-05;
  val[0][1][0][ 3][13] =  -3.543824e-03; eval[0][1][0][ 3][13] =   2.587689e-05;
  val[0][1][0][ 3][14] =  -3.658531e-03; eval[0][1][0][ 3][14] =   3.313217e-05;
  val[0][1][0][ 3][15] =  -3.561939e-03; eval[0][1][0][ 3][15] =   4.189648e-05;
  val[0][1][0][ 3][16] =  -3.488789e-03; eval[0][1][0][ 3][16] =   5.290548e-05;
  val[0][1][0][ 3][17] =  -3.390285e-03; eval[0][1][0][ 3][17] =   6.403524e-05;
  val[0][1][0][ 3][18] =  -3.429225e-03; eval[0][1][0][ 3][18] =   7.899913e-05;
  val[0][1][0][ 3][19] =  -3.259860e-03; eval[0][1][0][ 3][19] =   8.420769e-05;
  val[0][1][0][ 3][20] =  -3.294566e-03; eval[0][1][0][ 3][20] =   7.353590e-05;
  val[0][1][0][ 3][21] =  -3.320913e-03; eval[0][1][0][ 3][21] =   9.265126e-05;
  val[0][1][0][ 3][22] =  -3.248647e-03; eval[0][1][0][ 3][22] =   1.355809e-04;
  val[0][1][0][ 3][23] =  -3.200597e-03; eval[0][1][0][ 3][23] =   1.806388e-04;
  val[0][1][0][ 3][24] =  -3.372427e-03; eval[0][1][0][ 3][24] =   2.049976e-04;
  val[0][1][0][ 3][25] =  -4.014866e-03; eval[0][1][0][ 3][25] =   3.459928e-04;
  val[0][1][0][ 3][26] =  -3.073058e-03; eval[0][1][0][ 3][26] =   4.524126e-04;
  val[0][1][0][ 3][27] =  -2.405331e-03; eval[0][1][0][ 3][27] =   4.613387e-04;
  val[0][1][0][ 3][28] =  -3.460038e-03; eval[0][1][0][ 3][28] =   4.331956e-04;
  val[0][1][0][ 3][29] =  -3.149966e-03; eval[0][1][0][ 3][29] =   7.260049e-04;
  //emcdphi_mean_c1d0z4
  n_val[0][1][0][4] = 30;
  val[0][1][0][ 4][ 0] =   0.000000e+00; eval[0][1][0][ 4][ 0] =   0.000000e+00;
  val[0][1][0][ 4][ 1] =   0.000000e+00; eval[0][1][0][ 4][ 1] =   0.000000e+00;
  val[0][1][0][ 4][ 2] =  -6.879361e-03; eval[0][1][0][ 4][ 2] =   1.771645e-06;
  val[0][1][0][ 4][ 3] =  -5.366537e-03; eval[0][1][0][ 4][ 3] =   2.021950e-06;
  val[0][1][0][ 4][ 4] =  -4.914863e-03; eval[0][1][0][ 4][ 4] =   2.845911e-06;
  val[0][1][0][ 4][ 5] =  -4.538453e-03; eval[0][1][0][ 4][ 5] =   3.466091e-06;
  val[0][1][0][ 4][ 6] =  -4.156193e-03; eval[0][1][0][ 4][ 6] =   3.943894e-06;
  val[0][1][0][ 4][ 7] =  -3.985573e-03; eval[0][1][0][ 4][ 7] =   5.184031e-06;
  val[0][1][0][ 4][ 8] =  -3.977968e-03; eval[0][1][0][ 4][ 8] =   8.090866e-06;
  val[0][1][0][ 4][ 9] =  -3.840878e-03; eval[0][1][0][ 4][ 9] =   1.076809e-05;
  val[0][1][0][ 4][10] =  -3.645316e-03; eval[0][1][0][ 4][10] =   1.156357e-05;
  val[0][1][0][ 4][11] =  -3.584609e-03; eval[0][1][0][ 4][11] =   1.770976e-05;
  val[0][1][0][ 4][12] =  -3.497080e-03; eval[0][1][0][ 4][12] =   2.314692e-05;
  val[0][1][0][ 4][13] =  -3.521378e-03; eval[0][1][0][ 4][13] =   3.044893e-05;
  val[0][1][0][ 4][14] =  -3.451196e-03; eval[0][1][0][ 4][14] =   3.906553e-05;
  val[0][1][0][ 4][15] =  -3.514599e-03; eval[0][1][0][ 4][15] =   4.976758e-05;
  val[0][1][0][ 4][16] =  -3.372452e-03; eval[0][1][0][ 4][16] =   6.404389e-05;
  val[0][1][0][ 4][17] =  -3.208694e-03; eval[0][1][0][ 4][17] =   7.583455e-05;
  val[0][1][0][ 4][18] =  -3.311608e-03; eval[0][1][0][ 4][18] =   8.128784e-05;
  val[0][1][0][ 4][19] =  -3.202371e-03; eval[0][1][0][ 4][19] =   1.060144e-04;
  val[0][1][0][ 4][20] =  -2.918593e-03; eval[0][1][0][ 4][20] =   7.638117e-05;
  val[0][1][0][ 4][21] =  -3.072823e-03; eval[0][1][0][ 4][21] =   1.093237e-04;
  val[0][1][0][ 4][22] =  -3.230796e-03; eval[0][1][0][ 4][22] =   1.565890e-04;
  val[0][1][0][ 4][23] =  -3.011930e-03; eval[0][1][0][ 4][23] =   1.887687e-04;
  val[0][1][0][ 4][24] =  -2.467763e-03; eval[0][1][0][ 4][24] =   3.152368e-04;
  val[0][1][0][ 4][25] =  -3.199261e-03; eval[0][1][0][ 4][25] =   3.203236e-04;
  val[0][1][0][ 4][26] =  -3.128016e-03; eval[0][1][0][ 4][26] =   3.645368e-04;
  val[0][1][0][ 4][27] =  -2.293996e-03; eval[0][1][0][ 4][27] =   4.915856e-04;
  val[0][1][0][ 4][28] =  -2.917120e-03; eval[0][1][0][ 4][28] =   8.162662e-04;
  val[0][1][0][ 4][29] =  -5.849889e-02; eval[0][1][0][ 4][29] =   7.463025e-02;
  //emcdphi_mean_c1d0z5
  n_val[0][1][0][5] = 30;
  val[0][1][0][ 5][ 0] =   0.000000e+00; eval[0][1][0][ 5][ 0] =   0.000000e+00;
  val[0][1][0][ 5][ 1] =   0.000000e+00; eval[0][1][0][ 5][ 1] =   0.000000e+00;
  val[0][1][0][ 5][ 2] =  -7.088666e-03; eval[0][1][0][ 5][ 2] =   1.862856e-06;
  val[0][1][0][ 5][ 3] =  -5.603797e-03; eval[0][1][0][ 5][ 3] =   2.095221e-06;
  val[0][1][0][ 5][ 4] =  -5.035131e-03; eval[0][1][0][ 5][ 4] =   3.050765e-06;
  val[0][1][0][ 5][ 5] =  -4.729952e-03; eval[0][1][0][ 5][ 5] =   3.973183e-06;
  val[0][1][0][ 5][ 6] =  -4.447001e-03; eval[0][1][0][ 5][ 6] =   4.651509e-06;
  val[0][1][0][ 5][ 7] =  -4.288684e-03; eval[0][1][0][ 5][ 7] =   5.904518e-06;
  val[0][1][0][ 5][ 8] =  -4.092919e-03; eval[0][1][0][ 5][ 8] =   7.257150e-06;
  val[0][1][0][ 5][ 9] =  -4.025262e-03; eval[0][1][0][ 5][ 9] =   9.613970e-06;
  val[0][1][0][ 5][10] =  -4.040096e-03; eval[0][1][0][ 5][10] =   1.256766e-05;
  val[0][1][0][ 5][11] =  -4.040227e-03; eval[0][1][0][ 5][11] =   2.135482e-05;
  val[0][1][0][ 5][12] =  -3.830705e-03; eval[0][1][0][ 5][12] =   2.146115e-05;
  val[0][1][0][ 5][13] =  -3.781128e-03; eval[0][1][0][ 5][13] =   2.777327e-05;
  val[0][1][0][ 5][14] =  -3.935128e-03; eval[0][1][0][ 5][14] =   4.283248e-05;
  val[0][1][0][ 5][15] =  -4.042197e-03; eval[0][1][0][ 5][15] =   5.391391e-05;
  val[0][1][0][ 5][16] =  -4.046710e-03; eval[0][1][0][ 5][16] =   6.679311e-05;
  val[0][1][0][ 5][17] =  -3.766510e-03; eval[0][1][0][ 5][17] =   7.910852e-05;
  val[0][1][0][ 5][18] =  -3.857903e-03; eval[0][1][0][ 5][18] =   1.066352e-04;
  val[0][1][0][ 5][19] =  -3.745389e-03; eval[0][1][0][ 5][19] =   1.247436e-04;
  val[0][1][0][ 5][20] =  -3.602508e-03; eval[0][1][0][ 5][20] =   8.810227e-05;
  val[0][1][0][ 5][21] =  -3.487263e-03; eval[0][1][0][ 5][21] =   1.241342e-04;
  val[0][1][0][ 5][22] =  -3.311720e-03; eval[0][1][0][ 5][22] =   1.446196e-04;
  val[0][1][0][ 5][23] =  -3.639791e-03; eval[0][1][0][ 5][23] =   2.254439e-04;
  val[0][1][0][ 5][24] =  -3.593189e-03; eval[0][1][0][ 5][24] =   3.010612e-04;
  val[0][1][0][ 5][25] =  -3.488988e-03; eval[0][1][0][ 5][25] =   3.342241e-04;
  val[0][1][0][ 5][26] =  -2.995481e-03; eval[0][1][0][ 5][26] =   6.244255e-04;
  val[0][1][0][ 5][27] =  -4.427656e-03; eval[0][1][0][ 5][27] =   4.360813e-04;
  val[0][1][0][ 5][28] =  -1.033483e-03; eval[0][1][0][ 5][28] =   1.057508e-03;
  val[0][1][0][ 5][29] =  -6.134558e-03; eval[0][1][0][ 5][29] =   1.078587e-03;
  //emcdphi_mean_c1d0z6
  n_val[0][1][0][6] = 30;
  val[0][1][0][ 6][ 0] =   0.000000e+00; eval[0][1][0][ 6][ 0] =   0.000000e+00;
  val[0][1][0][ 6][ 1] =   0.000000e+00; eval[0][1][0][ 6][ 1] =   0.000000e+00;
  val[0][1][0][ 6][ 2] =  -6.631525e-03; eval[0][1][0][ 6][ 2] =   1.581695e-06;
  val[0][1][0][ 6][ 3] =  -5.234866e-03; eval[0][1][0][ 6][ 3] =   1.796211e-06;
  val[0][1][0][ 6][ 4] =  -4.739353e-03; eval[0][1][0][ 6][ 4] =   2.424133e-06;
  val[0][1][0][ 6][ 5] =  -4.441040e-03; eval[0][1][0][ 6][ 5] =   3.010903e-06;
  val[0][1][0][ 6][ 6] =  -4.220478e-03; eval[0][1][0][ 6][ 6] =   3.808311e-06;
  val[0][1][0][ 6][ 7] =  -4.063354e-03; eval[0][1][0][ 6][ 7] =   4.847767e-06;
  val[0][1][0][ 6][ 8] =  -3.969573e-03; eval[0][1][0][ 6][ 8] =   7.379114e-06;
  val[0][1][0][ 6][ 9] =  -3.874319e-03; eval[0][1][0][ 6][ 9] =   9.712782e-06;
  val[0][1][0][ 6][10] =  -3.770947e-03; eval[0][1][0][ 6][10] =   1.024088e-05;
  val[0][1][0][ 6][11] =  -3.783564e-03; eval[0][1][0][ 6][11] =   1.546595e-05;
  val[0][1][0][ 6][12] =  -3.687329e-03; eval[0][1][0][ 6][12] =   2.049742e-05;
  val[0][1][0][ 6][13] =  -3.715239e-03; eval[0][1][0][ 6][13] =   2.763290e-05;
  val[0][1][0][ 6][14] =  -3.653749e-03; eval[0][1][0][ 6][14] =   3.601942e-05;
  val[0][1][0][ 6][15] =  -3.669975e-03; eval[0][1][0][ 6][15] =   4.639576e-05;
  val[0][1][0][ 6][16] =  -3.525607e-03; eval[0][1][0][ 6][16] =   5.432351e-05;
  val[0][1][0][ 6][17] =  -3.596978e-03; eval[0][1][0][ 6][17] =   6.506245e-05;
  val[0][1][0][ 6][18] =  -3.544932e-03; eval[0][1][0][ 6][18] =   8.026505e-05;
  val[0][1][0][ 6][19] =  -3.633035e-03; eval[0][1][0][ 6][19] =   9.364687e-05;
  val[0][1][0][ 6][20] =  -3.285136e-03; eval[0][1][0][ 6][20] =   6.877644e-05;
  val[0][1][0][ 6][21] =  -3.351908e-03; eval[0][1][0][ 6][21] =   1.119105e-04;
  val[0][1][0][ 6][22] =  -3.190532e-03; eval[0][1][0][ 6][22] =   1.362838e-04;
  val[0][1][0][ 6][23] =  -3.135520e-03; eval[0][1][0][ 6][23] =   1.770729e-04;
  val[0][1][0][ 6][24] =  -2.688533e-03; eval[0][1][0][ 6][24] =   1.994838e-04;
  val[0][1][0][ 6][25] =  -3.568526e-03; eval[0][1][0][ 6][25] =   2.853115e-04;
  val[0][1][0][ 6][26] =  -3.807471e-03; eval[0][1][0][ 6][26] =   3.894645e-04;
  val[0][1][0][ 6][27] =  -4.122514e-03; eval[0][1][0][ 6][27] =   4.715411e-04;
  val[0][1][0][ 6][28] =  -1.010683e-01; eval[0][1][0][ 6][28] =   1.676883e-02;
  val[0][1][0][ 6][29] =  -3.594412e-03; eval[0][1][0][ 6][29] =   4.883050e-04;
  //emcdphi_mean_c1d0z7
  n_val[0][1][0][7] = 30;
  val[0][1][0][ 7][ 0] =   0.000000e+00; eval[0][1][0][ 7][ 0] =   0.000000e+00;
  val[0][1][0][ 7][ 1] =   0.000000e+00; eval[0][1][0][ 7][ 1] =   0.000000e+00;
  val[0][1][0][ 7][ 2] =  -6.210032e-03; eval[0][1][0][ 7][ 2] =   1.532080e-06;
  val[0][1][0][ 7][ 3] =  -4.937164e-03; eval[0][1][0][ 7][ 3] =   1.747146e-06;
  val[0][1][0][ 7][ 4] =  -4.526662e-03; eval[0][1][0][ 7][ 4] =   2.204266e-06;
  val[0][1][0][ 7][ 5] =  -4.342743e-03; eval[0][1][0][ 7][ 5] =   2.954527e-06;
  val[0][1][0][ 7][ 6] =  -4.196615e-03; eval[0][1][0][ 7][ 6] =   3.804354e-06;
  val[0][1][0][ 7][ 7] =  -4.051395e-03; eval[0][1][0][ 7][ 7] =   4.737348e-06;
  val[0][1][0][ 7][ 8] =  -3.985565e-03; eval[0][1][0][ 7][ 8] =   7.253071e-06;
  val[0][1][0][ 7][ 9] =  -3.929081e-03; eval[0][1][0][ 7][ 9] =   9.569144e-06;
  val[0][1][0][ 7][10] =  -3.800981e-03; eval[0][1][0][ 7][10] =   9.946828e-06;
  val[0][1][0][ 7][11] =  -3.842475e-03; eval[0][1][0][ 7][11] =   1.549667e-05;
  val[0][1][0][ 7][12] =  -3.737873e-03; eval[0][1][0][ 7][12] =   2.015969e-05;
  val[0][1][0][ 7][13] =  -3.729191e-03; eval[0][1][0][ 7][13] =   2.725772e-05;
  val[0][1][0][ 7][14] =  -3.695168e-03; eval[0][1][0][ 7][14] =   3.452234e-05;
  val[0][1][0][ 7][15] =  -3.645637e-03; eval[0][1][0][ 7][15] =   4.264549e-05;
  val[0][1][0][ 7][16] =  -3.633468e-03; eval[0][1][0][ 7][16] =   5.309749e-05;
  val[0][1][0][ 7][17] =  -3.542147e-03; eval[0][1][0][ 7][17] =   6.497382e-05;
  val[0][1][0][ 7][18] =  -3.514272e-03; eval[0][1][0][ 7][18] =   8.028547e-05;
  val[0][1][0][ 7][19] =  -3.452055e-03; eval[0][1][0][ 7][19] =   8.351005e-05;
  val[0][1][0][ 7][20] =  -3.272680e-03; eval[0][1][0][ 7][20] =   6.805401e-05;
  val[0][1][0][ 7][21] =  -3.377445e-03; eval[0][1][0][ 7][21] =   9.162436e-05;
  val[0][1][0][ 7][22] =  -3.298076e-03; eval[0][1][0][ 7][22] =   1.290428e-04;
  val[0][1][0][ 7][23] =  -3.254445e-03; eval[0][1][0][ 7][23] =   1.822011e-04;
  val[0][1][0][ 7][24] =  -3.199085e-03; eval[0][1][0][ 7][24] =   2.410478e-04;
  val[0][1][0][ 7][25] =  -3.084250e-03; eval[0][1][0][ 7][25] =   3.638084e-04;
  val[0][1][0][ 7][26] =  -3.884773e-03; eval[0][1][0][ 7][26] =   3.214306e-04;
  val[0][1][0][ 7][27] =  -3.717154e-03; eval[0][1][0][ 7][27] =   4.519170e-04;
  val[0][1][0][ 7][28] =  -2.847504e-03; eval[0][1][0][ 7][28] =   5.072745e-04;
  val[0][1][0][ 7][29] =  -3.437656e-03; eval[0][1][0][ 7][29] =   5.592800e-04;
  //emcdphi_mean_c1d0z8
  n_val[0][1][0][8] = 30;
  val[0][1][0][ 8][ 0] =   0.000000e+00; eval[0][1][0][ 8][ 0] =   0.000000e+00;
  val[0][1][0][ 8][ 1] =   0.000000e+00; eval[0][1][0][ 8][ 1] =   0.000000e+00;
  val[0][1][0][ 8][ 2] =  -5.410688e-03; eval[0][1][0][ 8][ 2] =   1.475371e-06;
  val[0][1][0][ 8][ 3] =  -4.669844e-03; eval[0][1][0][ 8][ 3] =   1.660320e-06;
  val[0][1][0][ 8][ 4] =  -4.360161e-03; eval[0][1][0][ 8][ 4] =   2.109967e-06;
  val[0][1][0][ 8][ 5] =  -4.238913e-03; eval[0][1][0][ 8][ 5] =   2.859671e-06;
  val[0][1][0][ 8][ 6] =  -4.168561e-03; eval[0][1][0][ 8][ 6] =   3.774216e-06;
  val[0][1][0][ 8][ 7] =  -4.065298e-03; eval[0][1][0][ 8][ 7] =   4.724396e-06;
  val[0][1][0][ 8][ 8] =  -3.931842e-03; eval[0][1][0][ 8][ 8] =   5.832820e-06;
  val[0][1][0][ 8][ 9] =  -3.977890e-03; eval[0][1][0][ 8][ 9] =   9.266464e-06;
  val[0][1][0][ 8][10] =  -3.911401e-03; eval[0][1][0][ 8][10] =   1.206702e-05;
  val[0][1][0][ 8][11] =  -3.829065e-03; eval[0][1][0][ 8][11] =   1.491774e-05;
  val[0][1][0][ 8][12] =  -3.763434e-03; eval[0][1][0][ 8][12] =   1.948053e-05;
  val[0][1][0][ 8][13] =  -3.738254e-03; eval[0][1][0][ 8][13] =   2.504119e-05;
  val[0][1][0][ 8][14] =  -3.654220e-03; eval[0][1][0][ 8][14] =   3.121870e-05;
  val[0][1][0][ 8][15] =  -3.853003e-03; eval[0][1][0][ 8][15] =   4.185371e-05;
  val[0][1][0][ 8][16] =  -3.690516e-03; eval[0][1][0][ 8][16] =   5.121354e-05;
  val[0][1][0][ 8][17] =  -3.668468e-03; eval[0][1][0][ 8][17] =   6.319026e-05;
  val[0][1][0][ 8][18] =  -3.628933e-03; eval[0][1][0][ 8][18] =   8.713811e-05;
  val[0][1][0][ 8][19] =  -3.717194e-03; eval[0][1][0][ 8][19] =   9.504322e-05;
  val[0][1][0][ 8][20] =  -3.324740e-03; eval[0][1][0][ 8][20] =   6.826999e-05;
  val[0][1][0][ 8][21] =  -3.384879e-03; eval[0][1][0][ 8][21] =   9.181581e-05;
  val[0][1][0][ 8][22] =  -3.184463e-03; eval[0][1][0][ 8][22] =   1.172074e-04;
  val[0][1][0][ 8][23] =  -3.532167e-03; eval[0][1][0][ 8][23] =   1.641002e-04;
  val[0][1][0][ 8][24] =  -3.865649e-03; eval[0][1][0][ 8][24] =   2.294188e-04;
  val[0][1][0][ 8][25] =  -3.042183e-03; eval[0][1][0][ 8][25] =   2.845900e-04;
  val[0][1][0][ 8][26] =  -3.233249e-03; eval[0][1][0][ 8][26] =   3.288197e-04;
  val[0][1][0][ 8][27] =  -2.495003e-03; eval[0][1][0][ 8][27] =   5.443373e-04;
  val[0][1][0][ 8][28] =  -4.306469e-03; eval[0][1][0][ 8][28] =   3.994688e-04;
  val[0][1][0][ 8][29] =  -2.250093e-03; eval[0][1][0][ 8][29] =   6.120118e-04;
  //emcdphi_mean_c1d0z9
  n_val[0][1][0][9] = 30;
  val[0][1][0][ 9][ 0] =   0.000000e+00; eval[0][1][0][ 9][ 0] =   0.000000e+00;
  val[0][1][0][ 9][ 1] =   0.000000e+00; eval[0][1][0][ 9][ 1] =   0.000000e+00;
  val[0][1][0][ 9][ 2] =  -4.527344e-03; eval[0][1][0][ 9][ 2] =   1.535764e-06;
  val[0][1][0][ 9][ 3] =  -4.298493e-03; eval[0][1][0][ 9][ 3] =   1.625527e-06;
  val[0][1][0][ 9][ 4] =  -4.134282e-03; eval[0][1][0][ 9][ 4] =   2.087592e-06;
  val[0][1][0][ 9][ 5] =  -4.049743e-03; eval[0][1][0][ 9][ 5] =   2.817665e-06;
  val[0][1][0][ 9][ 6] =  -4.053003e-03; eval[0][1][0][ 9][ 6] =   3.776395e-06;
  val[0][1][0][ 9][ 7] =  -4.029490e-03; eval[0][1][0][ 9][ 7] =   4.832092e-06;
  val[0][1][0][ 9][ 8] =  -3.898563e-03; eval[0][1][0][ 9][ 8] =   5.949168e-06;
  val[0][1][0][ 9][ 9] =  -3.958260e-03; eval[0][1][0][ 9][ 9] =   9.684621e-06;
  val[0][1][0][ 9][10] =  -3.905069e-03; eval[0][1][0][ 9][10] =   1.257823e-05;
  val[0][1][0][ 9][11] =  -3.852722e-03; eval[0][1][0][ 9][11] =   1.564269e-05;
  val[0][1][0][ 9][12] =  -3.772166e-03; eval[0][1][0][ 9][12] =   2.002107e-05;
  val[0][1][0][ 9][13] =  -3.744583e-03; eval[0][1][0][ 9][13] =   2.574855e-05;
  val[0][1][0][ 9][14] =  -3.688087e-03; eval[0][1][0][ 9][14] =   3.199204e-05;
  val[0][1][0][ 9][15] =  -3.656446e-03; eval[0][1][0][ 9][15] =   4.221793e-05;
  val[0][1][0][ 9][16] =  -3.706899e-03; eval[0][1][0][ 9][16] =   5.351395e-05;
  val[0][1][0][ 9][17] =  -3.634291e-03; eval[0][1][0][ 9][17] =   6.318364e-05;
  val[0][1][0][ 9][18] =  -3.637289e-03; eval[0][1][0][ 9][18] =   9.165824e-05;
  val[0][1][0][ 9][19] =  -3.388692e-03; eval[0][1][0][ 9][19] =   8.127119e-05;
  val[0][1][0][ 9][20] =  -3.518721e-03; eval[0][1][0][ 9][20] =   6.521601e-05;
  val[0][1][0][ 9][21] =  -3.237764e-03; eval[0][1][0][ 9][21] =   9.421299e-05;
  val[0][1][0][ 9][22] =  -3.319032e-03; eval[0][1][0][ 9][22] =   1.338876e-04;
  val[0][1][0][ 9][23] =  -3.165875e-03; eval[0][1][0][ 9][23] =   1.743404e-04;
  val[0][1][0][ 9][24] =  -3.477426e-03; eval[0][1][0][ 9][24] =   2.601766e-04;
  val[0][1][0][ 9][25] =  -2.886032e-03; eval[0][1][0][ 9][25] =   2.560954e-04;
  val[0][1][0][ 9][26] =  -3.356170e-03; eval[0][1][0][ 9][26] =   3.592468e-04;
  val[0][1][0][ 9][27] =  -2.573346e-03; eval[0][1][0][ 9][27] =   4.067444e-04;
  val[0][1][0][ 9][28] =  -4.215236e-03; eval[0][1][0][ 9][28] =   5.130741e-04;
  val[0][1][0][ 9][29] =  -3.890746e-03; eval[0][1][0][ 9][29] =   1.139454e-03;
  //emcdphi_mean_c1d1z0
  n_val[0][1][1][0] = 30;
  val[0][1][1][ 0][ 0] =   0.000000e+00; eval[0][1][1][ 0][ 0] =   0.000000e+00;
  val[0][1][1][ 0][ 1] =   0.000000e+00; eval[0][1][1][ 0][ 1] =   0.000000e+00;
  val[0][1][1][ 0][ 2] =  -2.276748e-03; eval[0][1][1][ 0][ 2] =   1.590080e-06;
  val[0][1][1][ 0][ 3] =  -1.771786e-03; eval[0][1][1][ 0][ 3] =   1.663656e-06;
  val[0][1][1][ 0][ 4] =  -1.248332e-03; eval[0][1][1][ 0][ 4] =   1.791530e-06;
  val[0][1][1][ 0][ 5] =  -9.086740e-04; eval[0][1][1][ 0][ 5] =   2.296885e-06;
  val[0][1][1][ 0][ 6] =  -6.934294e-04; eval[0][1][1][ 0][ 6] =   3.056609e-06;
  val[0][1][1][ 0][ 7] =  -5.258264e-04; eval[0][1][1][ 0][ 7] =   4.679368e-06;
  val[0][1][1][ 0][ 8] =  -3.918883e-04; eval[0][1][1][ 0][ 8] =   5.851060e-06;
  val[0][1][1][ 0][ 9] =  -2.747775e-04; eval[0][1][1][ 0][ 9] =   7.577477e-06;
  val[0][1][1][ 0][10] =  -1.815040e-04; eval[0][1][1][ 0][10] =   1.009310e-05;
  val[0][1][1][ 0][11] =  -1.390806e-04; eval[0][1][1][ 0][11] =   1.331150e-05;
  val[0][1][1][ 0][12] =  -2.309135e-05; eval[0][1][1][ 0][12] =   1.694808e-05;
  val[0][1][1][ 0][13] =   4.594666e-05; eval[0][1][1][ 0][13] =   2.211917e-05;
  val[0][1][1][ 0][14] =   4.587922e-05; eval[0][1][1][ 0][14] =   2.849123e-05;
  val[0][1][1][ 0][15] =   9.052966e-05; eval[0][1][1][ 0][15] =   3.618594e-05;
  val[0][1][1][ 0][16] =   1.157959e-04; eval[0][1][1][ 0][16] =   4.397306e-05;
  val[0][1][1][ 0][17] =   2.159629e-04; eval[0][1][1][ 0][17] =   5.664991e-05;
  val[0][1][1][ 0][18] =   2.597789e-04; eval[0][1][1][ 0][18] =   7.396737e-05;
  val[0][1][1][ 0][19] =   7.755142e-05; eval[0][1][1][ 0][19] =   7.779296e-05;
  val[0][1][1][ 0][20] =   2.878236e-04; eval[0][1][1][ 0][20] =   5.672218e-05;
  val[0][1][1][ 0][21] =   2.625320e-04; eval[0][1][1][ 0][21] =   7.567889e-05;
  val[0][1][1][ 0][22] =   2.725691e-04; eval[0][1][1][ 0][22] =   1.062963e-04;
  val[0][1][1][ 0][23] =   3.613376e-04; eval[0][1][1][ 0][23] =   1.393921e-04;
  val[0][1][1][ 0][24] =   2.747327e-04; eval[0][1][1][ 0][24] =   1.931266e-04;
  val[0][1][1][ 0][25] =   1.641081e-04; eval[0][1][1][ 0][25] =   2.236837e-04;
  val[0][1][1][ 0][26] =   4.403970e-04; eval[0][1][1][ 0][26] =   2.926081e-04;
  val[0][1][1][ 0][27] =   1.508590e-04; eval[0][1][1][ 0][27] =   2.731565e-04;
  val[0][1][1][ 0][28] =   5.957173e-04; eval[0][1][1][ 0][28] =   4.804586e-04;
  val[0][1][1][ 0][29] =   1.342873e-03; eval[0][1][1][ 0][29] =   1.187099e-02;
  //emcdphi_mean_c1d1z1
  n_val[0][1][1][1] = 30;
  val[0][1][1][ 1][ 0] =   0.000000e+00; eval[0][1][1][ 1][ 0] =   0.000000e+00;
  val[0][1][1][ 1][ 1] =   0.000000e+00; eval[0][1][1][ 1][ 1] =   0.000000e+00;
  val[0][1][1][ 1][ 2] =  -3.320137e-03; eval[0][1][1][ 1][ 2] =   1.327565e-06;
  val[0][1][1][ 1][ 3] =  -2.512120e-03; eval[0][1][1][ 1][ 3] =   1.385869e-06;
  val[0][1][1][ 1][ 4] =  -1.812693e-03; eval[0][1][1][ 1][ 4] =   1.855875e-06;
  val[0][1][1][ 1][ 5] =  -1.248126e-03; eval[0][1][1][ 1][ 5] =   2.139147e-06;
  val[0][1][1][ 1][ 6] =  -9.570340e-04; eval[0][1][1][ 1][ 6] =   2.782804e-06;
  val[0][1][1][ 1][ 7] =  -6.975083e-04; eval[0][1][1][ 1][ 7] =   4.398384e-06;
  val[0][1][1][ 1][ 8] =  -5.657881e-04; eval[0][1][1][ 1][ 8] =   5.443918e-06;
  val[0][1][1][ 1][ 9] =  -4.171347e-04; eval[0][1][1][ 1][ 9] =   7.129371e-06;
  val[0][1][1][ 1][10] =  -3.175442e-04; eval[0][1][1][ 1][10] =   9.371157e-06;
  val[0][1][1][ 1][11] =  -2.315385e-04; eval[0][1][1][ 1][11] =   1.239498e-05;
  val[0][1][1][ 1][12] =  -1.040853e-04; eval[0][1][1][ 1][12] =   1.589445e-05;
  val[0][1][1][ 1][13] =  -1.125265e-05; eval[0][1][1][ 1][13] =   2.069888e-05;
  val[0][1][1][ 1][14] =  -2.418968e-05; eval[0][1][1][ 1][14] =   2.582848e-05;
  val[0][1][1][ 1][15] =   6.606398e-05; eval[0][1][1][ 1][15] =   3.365996e-05;
  val[0][1][1][ 1][16] =   1.634733e-04; eval[0][1][1][ 1][16] =   4.109492e-05;
  val[0][1][1][ 1][17] =   2.174338e-04; eval[0][1][1][ 1][17] =   5.034631e-05;
  val[0][1][1][ 1][18] =   1.803976e-04; eval[0][1][1][ 1][18] =   6.084726e-05;
  val[0][1][1][ 1][19] =   1.913387e-04; eval[0][1][1][ 1][19] =   8.257062e-05;
  val[0][1][1][ 1][20] =   1.725877e-04; eval[0][1][1][ 1][20] =   5.552497e-05;
  val[0][1][1][ 1][21] =   2.122663e-04; eval[0][1][1][ 1][21] =   7.230638e-05;
  val[0][1][1][ 1][22] =   3.209412e-04; eval[0][1][1][ 1][22] =   1.015670e-04;
  val[0][1][1][ 1][23] =   4.604145e-04; eval[0][1][1][ 1][23] =   1.367889e-04;
  val[0][1][1][ 1][24] =   4.529429e-04; eval[0][1][1][ 1][24] =   1.618214e-04;
  val[0][1][1][ 1][25] =   3.398014e-04; eval[0][1][1][ 1][25] =   2.184367e-04;
  val[0][1][1][ 1][26] =   8.707499e-04; eval[0][1][1][ 1][26] =   2.773677e-04;
  val[0][1][1][ 1][27] =   5.654707e-04; eval[0][1][1][ 1][27] =   4.719631e-04;
  val[0][1][1][ 1][28] =   6.200272e-04; eval[0][1][1][ 1][28] =   3.412916e-04;
  val[0][1][1][ 1][29] =   2.486108e-04; eval[0][1][1][ 1][29] =   3.398898e-04;
  //emcdphi_mean_c1d1z2
  n_val[0][1][1][2] = 30;
  val[0][1][1][ 2][ 0] =   0.000000e+00; eval[0][1][1][ 2][ 0] =   0.000000e+00;
  val[0][1][1][ 2][ 1] =   0.000000e+00; eval[0][1][1][ 2][ 1] =   0.000000e+00;
  val[0][1][1][ 2][ 2] =  -4.221225e-03; eval[0][1][1][ 2][ 2] =   1.421244e-06;
  val[0][1][1][ 2][ 3] =  -2.887654e-03; eval[0][1][1][ 2][ 3] =   1.426521e-06;
  val[0][1][1][ 2][ 4] =  -2.009115e-03; eval[0][1][1][ 2][ 4] =   1.932976e-06;
  val[0][1][1][ 2][ 5] =  -1.420749e-03; eval[0][1][1][ 2][ 5] =   2.202431e-06;
  val[0][1][1][ 2][ 6] =  -1.059988e-03; eval[0][1][1][ 2][ 6] =   2.883481e-06;
  val[0][1][1][ 2][ 7] =  -7.438609e-04; eval[0][1][1][ 2][ 7] =   3.831704e-06;
  val[0][1][1][ 2][ 8] =  -6.045716e-04; eval[0][1][1][ 2][ 8] =   5.898930e-06;
  val[0][1][1][ 2][ 9] =  -4.691650e-04; eval[0][1][1][ 2][ 9] =   7.691554e-06;
  val[0][1][1][ 2][10] =  -3.611226e-04; eval[0][1][1][ 2][10] =   1.003970e-05;
  val[0][1][1][ 2][11] =  -2.628638e-04; eval[0][1][1][ 2][11] =   1.294684e-05;
  val[0][1][1][ 2][12] =  -1.913752e-04; eval[0][1][1][ 2][12] =   1.704058e-05;
  val[0][1][1][ 2][13] =  -8.712306e-05; eval[0][1][1][ 2][13] =   2.156823e-05;
  val[0][1][1][ 2][14] =  -4.441707e-05; eval[0][1][1][ 2][14] =   2.713725e-05;
  val[0][1][1][ 2][15] =  -2.313620e-05; eval[0][1][1][ 2][15] =   3.512766e-05;
  val[0][1][1][ 2][16] =  -3.920087e-05; eval[0][1][1][ 2][16] =   4.252872e-05;
  val[0][1][1][ 2][17] =   7.641578e-05; eval[0][1][1][ 2][17] =   5.543011e-05;
  val[0][1][1][ 2][18] =   3.036567e-05; eval[0][1][1][ 2][18] =   6.910864e-05;
  val[0][1][1][ 2][19] =   1.777740e-04; eval[0][1][1][ 2][19] =   8.380924e-05;
  val[0][1][1][ 2][20] =   2.174490e-04; eval[0][1][1][ 2][20] =   5.617255e-05;
  val[0][1][1][ 2][21] =   1.184076e-04; eval[0][1][1][ 2][21] =   7.388825e-05;
  val[0][1][1][ 2][22] =   1.425465e-04; eval[0][1][1][ 2][22] =   1.044578e-04;
  val[0][1][1][ 2][23] =   1.667829e-04; eval[0][1][1][ 2][23] =   1.416011e-04;
  val[0][1][1][ 2][24] =   3.841184e-04; eval[0][1][1][ 2][24] =   1.794233e-04;
  val[0][1][1][ 2][25] =   1.032492e-04; eval[0][1][1][ 2][25] =   2.137511e-04;
  val[0][1][1][ 2][26] =   5.134052e-04; eval[0][1][1][ 2][26] =   3.682454e-04;
  val[0][1][1][ 2][27] =   4.194276e-04; eval[0][1][1][ 2][27] =   4.117761e-04;
  val[0][1][1][ 2][28] =  -5.467640e-04; eval[0][1][1][ 2][28] =   4.270073e-04;
  val[0][1][1][ 2][29] =   8.014118e-04; eval[0][1][1][ 2][29] =   4.939629e-04;
  //emcdphi_mean_c1d1z3
  n_val[0][1][1][3] = 30;
  val[0][1][1][ 3][ 0] =   0.000000e+00; eval[0][1][1][ 3][ 0] =   0.000000e+00;
  val[0][1][1][ 3][ 1] =   0.000000e+00; eval[0][1][1][ 3][ 1] =   0.000000e+00;
  val[0][1][1][ 3][ 2] =  -4.765212e-03; eval[0][1][1][ 3][ 2] =   1.323523e-06;
  val[0][1][1][ 3][ 3] =  -3.240147e-03; eval[0][1][1][ 3][ 3] =   1.461113e-06;
  val[0][1][1][ 3][ 4] =  -2.170723e-03; eval[0][1][1][ 3][ 4] =   1.964014e-06;
  val[0][1][1][ 3][ 5] =  -1.497164e-03; eval[0][1][1][ 3][ 5] =   2.199828e-06;
  val[0][1][1][ 3][ 6] =  -1.144939e-03; eval[0][1][1][ 3][ 6] =   2.787469e-06;
  val[0][1][1][ 3][ 7] =  -8.737668e-04; eval[0][1][1][ 3][ 7] =   3.620538e-06;
  val[0][1][1][ 3][ 8] =  -6.724906e-04; eval[0][1][1][ 3][ 8] =   5.536126e-06;
  val[0][1][1][ 3][ 9] =  -5.237568e-04; eval[0][1][1][ 3][ 9] =   7.262546e-06;
  val[0][1][1][ 3][10] =  -3.879728e-04; eval[0][1][1][ 3][10] =   9.413698e-06;
  val[0][1][1][ 3][11] =  -2.875300e-04; eval[0][1][1][ 3][11] =   1.270927e-05;
  val[0][1][1][ 3][12] =  -1.735920e-04; eval[0][1][1][ 3][12] =   1.642407e-05;
  val[0][1][1][ 3][13] =  -1.418833e-04; eval[0][1][1][ 3][13] =   2.120655e-05;
  val[0][1][1][ 3][14] =  -1.942639e-05; eval[0][1][1][ 3][14] =   2.738487e-05;
  val[0][1][1][ 3][15] =   2.191881e-07; eval[0][1][1][ 3][15] =   3.476158e-05;
  val[0][1][1][ 3][16] =  -1.248021e-05; eval[0][1][1][ 3][16] =   4.416065e-05;
  val[0][1][1][ 3][17] =   5.071399e-05; eval[0][1][1][ 3][17] =   5.353678e-05;
  val[0][1][1][ 3][18] =   9.696306e-05; eval[0][1][1][ 3][18] =   6.730730e-05;
  val[0][1][1][ 3][19] =  -2.663728e-05; eval[0][1][1][ 3][19] =   6.056895e-05;
  val[0][1][1][ 3][20] =   1.456280e-04; eval[0][1][1][ 3][20] =   5.652762e-05;
  val[0][1][1][ 3][21] =   1.356399e-04; eval[0][1][1][ 3][21] =   7.903747e-05;
  val[0][1][1][ 3][22] =   1.005916e-05; eval[0][1][1][ 3][22] =   1.033466e-04;
  val[0][1][1][ 3][23] =   4.905750e-04; eval[0][1][1][ 3][23] =   1.583161e-04;
  val[0][1][1][ 3][24] =   5.629426e-04; eval[0][1][1][ 3][24] =   1.859698e-04;
  val[0][1][1][ 3][25] =  -2.095774e-04; eval[0][1][1][ 3][25] =   2.488135e-04;
  val[0][1][1][ 3][26] =   6.300880e-04; eval[0][1][1][ 3][26] =   2.337789e-04;
  val[0][1][1][ 3][27] =  -2.340677e-04; eval[0][1][1][ 3][27] =   3.017218e-04;
  val[0][1][1][ 3][28] =  -2.357839e-04; eval[0][1][1][ 3][28] =   4.574517e-04;
  val[0][1][1][ 3][29] =   3.594213e-04; eval[0][1][1][ 3][29] =   4.410464e-04;
  //emcdphi_mean_c1d1z4
  n_val[0][1][1][4] = 30;
  val[0][1][1][ 4][ 0] =   0.000000e+00; eval[0][1][1][ 4][ 0] =   0.000000e+00;
  val[0][1][1][ 4][ 1] =   0.000000e+00; eval[0][1][1][ 4][ 1] =   0.000000e+00;
  val[0][1][1][ 4][ 2] =  -4.951808e-03; eval[0][1][1][ 4][ 2] =   1.484140e-06;
  val[0][1][1][ 4][ 3] =  -3.314774e-03; eval[0][1][1][ 4][ 3] =   1.707052e-06;
  val[0][1][1][ 4][ 4] =  -2.242379e-03; eval[0][1][1][ 4][ 4] =   2.086237e-06;
  val[0][1][1][ 4][ 5] =  -1.639120e-03; eval[0][1][1][ 4][ 5] =   2.710836e-06;
  val[0][1][1][ 4][ 6] =  -1.249605e-03; eval[0][1][1][ 4][ 6] =   3.481893e-06;
  val[0][1][1][ 4][ 7] =  -9.526252e-04; eval[0][1][1][ 4][ 7] =   4.468515e-06;
  val[0][1][1][ 4][ 8] =  -7.495857e-04; eval[0][1][1][ 4][ 8] =   7.115274e-06;
  val[0][1][1][ 4][ 9] =  -5.872028e-04; eval[0][1][1][ 4][ 9] =   9.143708e-06;
  val[0][1][1][ 4][10] =  -4.479021e-04; eval[0][1][1][ 4][10] =   1.220496e-05;
  val[0][1][1][ 4][11] =  -3.182199e-04; eval[0][1][1][ 4][11] =   1.602053e-05;
  val[0][1][1][ 4][12] =  -1.761809e-04; eval[0][1][1][ 4][12] =   2.033057e-05;
  val[0][1][1][ 4][13] =  -1.470629e-04; eval[0][1][1][ 4][13] =   2.657647e-05;
  val[0][1][1][ 4][14] =  -1.094142e-04; eval[0][1][1][ 4][14] =   3.392016e-05;
  val[0][1][1][ 4][15] =  -7.533773e-05; eval[0][1][1][ 4][15] =   4.462952e-05;
  val[0][1][1][ 4][16] =  -6.611608e-05; eval[0][1][1][ 4][16] =   5.546673e-05;
  val[0][1][1][ 4][17] =  -1.038318e-04; eval[0][1][1][ 4][17] =   6.605939e-05;
  val[0][1][1][ 4][18] =  -2.169343e-05; eval[0][1][1][ 4][18] =   8.159060e-05;
  val[0][1][1][ 4][19] =   9.904116e-05; eval[0][1][1][ 4][19] =   1.015081e-04;
  val[0][1][1][ 4][20] =   6.867081e-05; eval[0][1][1][ 4][20] =   7.075464e-05;
  val[0][1][1][ 4][21] =  -1.302253e-05; eval[0][1][1][ 4][21] =   1.044739e-04;
  val[0][1][1][ 4][22] =   8.091546e-05; eval[0][1][1][ 4][22] =   1.437731e-04;
  val[0][1][1][ 4][23] =  -3.425519e-05; eval[0][1][1][ 4][23] =   2.155904e-04;
  val[0][1][1][ 4][24] =   1.287079e-04; eval[0][1][1][ 4][24] =   1.882003e-04;
  val[0][1][1][ 4][25] =  -2.183522e-04; eval[0][1][1][ 4][25] =   2.588217e-04;
  val[0][1][1][ 4][26] =   4.877436e-04; eval[0][1][1][ 4][26] =   3.039278e-04;
  val[0][1][1][ 4][27] =  -1.796144e-04; eval[0][1][1][ 4][27] =   3.940427e-04;
  val[0][1][1][ 4][28] =  -1.979425e-04; eval[0][1][1][ 4][28] =   5.745023e-04;
  val[0][1][1][ 4][29] =  -1.524110e-03; eval[0][1][1][ 4][29] =   9.863949e-04;
  //emcdphi_mean_c1d1z5
  n_val[0][1][1][5] = 30;
  val[0][1][1][ 5][ 0] =   0.000000e+00; eval[0][1][1][ 5][ 0] =   0.000000e+00;
  val[0][1][1][ 5][ 1] =   0.000000e+00; eval[0][1][1][ 5][ 1] =   0.000000e+00;
  val[0][1][1][ 5][ 2] =  -4.204628e-03; eval[0][1][1][ 5][ 2] =   1.731378e-06;
  val[0][1][1][ 5][ 3] =  -2.841625e-03; eval[0][1][1][ 5][ 3] =   1.771111e-06;
  val[0][1][1][ 5][ 4] =  -1.837037e-03; eval[0][1][1][ 5][ 4] =   2.199129e-06;
  val[0][1][1][ 5][ 5] =  -1.283147e-03; eval[0][1][1][ 5][ 5] =   2.592736e-06;
  val[0][1][1][ 5][ 6] =  -9.571941e-04; eval[0][1][1][ 5][ 6] =   3.266235e-06;
  val[0][1][1][ 5][ 7] =  -5.781733e-04; eval[0][1][1][ 5][ 7] =   5.208207e-06;
  val[0][1][1][ 5][ 8] =  -2.912291e-04; eval[0][1][1][ 5][ 8] =   6.583854e-06;
  val[0][1][1][ 5][ 9] =  -6.758617e-05; eval[0][1][1][ 5][ 9] =   9.027262e-06;
  val[0][1][1][ 5][10] =   1.319469e-04; eval[0][1][1][ 5][10] =   1.241930e-05;
  val[0][1][1][ 5][11] =   3.344684e-04; eval[0][1][1][ 5][11] =   1.642491e-05;
  val[0][1][1][ 5][12] =   4.602140e-04; eval[0][1][1][ 5][12] =   2.110074e-05;
  val[0][1][1][ 5][13] =   4.097230e-04; eval[0][1][1][ 5][13] =   2.670158e-05;
  val[0][1][1][ 5][14] =   5.143084e-04; eval[0][1][1][ 5][14] =   3.458756e-05;
  val[0][1][1][ 5][15] =   4.545112e-04; eval[0][1][1][ 5][15] =   4.546169e-05;
  val[0][1][1][ 5][16] =   5.609458e-04; eval[0][1][1][ 5][16] =   5.744446e-05;
  val[0][1][1][ 5][17] =   6.350125e-04; eval[0][1][1][ 5][17] =   7.663217e-05;
  val[0][1][1][ 5][18] =   6.490344e-04; eval[0][1][1][ 5][18] =   9.134098e-05;
  val[0][1][1][ 5][19] =   5.397611e-04; eval[0][1][1][ 5][19] =   9.178663e-05;
  val[0][1][1][ 5][20] =   5.513507e-04; eval[0][1][1][ 5][20] =   7.401134e-05;
  val[0][1][1][ 5][21] =   4.177041e-04; eval[0][1][1][ 5][21] =   9.351992e-05;
  val[0][1][1][ 5][22] =   5.275060e-04; eval[0][1][1][ 5][22] =   1.263358e-04;
  val[0][1][1][ 5][23] =   5.993132e-04; eval[0][1][1][ 5][23] =   2.510969e-04;
  val[0][1][1][ 5][24] =   1.974910e-04; eval[0][1][1][ 5][24] =   2.909435e-04;
  val[0][1][1][ 5][25] =   5.275299e-04; eval[0][1][1][ 5][25] =   3.121068e-04;
  val[0][1][1][ 5][26] =   1.280894e-03; eval[0][1][1][ 5][26] =   3.551438e-04;
  val[0][1][1][ 5][27] =   6.396624e-04; eval[0][1][1][ 5][27] =   5.018201e-04;
  val[0][1][1][ 5][28] =   6.986286e-04; eval[0][1][1][ 5][28] =   6.910763e-04;
  val[0][1][1][ 5][29] =  -4.690607e-04; eval[0][1][1][ 5][29] =   4.708254e-04;
  //emcdphi_mean_c1d1z6
  n_val[0][1][1][6] = 30;
  val[0][1][1][ 6][ 0] =   0.000000e+00; eval[0][1][1][ 6][ 0] =   0.000000e+00;
  val[0][1][1][ 6][ 1] =   0.000000e+00; eval[0][1][1][ 6][ 1] =   0.000000e+00;
  val[0][1][1][ 6][ 2] =  -3.753171e-03; eval[0][1][1][ 6][ 2] =   1.431991e-06;
  val[0][1][1][ 6][ 3] =  -2.568845e-03; eval[0][1][1][ 6][ 3] =   1.393556e-06;
  val[0][1][1][ 6][ 4] =  -1.694827e-03; eval[0][1][1][ 6][ 4] =   1.884327e-06;
  val[0][1][1][ 6][ 5] =  -1.203000e-03; eval[0][1][1][ 6][ 5] =   2.229207e-06;
  val[0][1][1][ 6][ 6] =  -9.258890e-04; eval[0][1][1][ 6][ 6] =   2.768665e-06;
  val[0][1][1][ 6][ 7] =  -5.416059e-04; eval[0][1][1][ 6][ 7] =   4.394189e-06;
  val[0][1][1][ 6][ 8] =  -2.600637e-04; eval[0][1][1][ 6][ 8] =   5.570353e-06;
  val[0][1][1][ 6][ 9] =  -3.601167e-05; eval[0][1][1][ 6][ 9] =   7.497216e-06;
  val[0][1][1][ 6][10] =   1.433087e-04; eval[0][1][1][ 6][10] =   9.970403e-06;
  val[0][1][1][ 6][11] =   3.361190e-04; eval[0][1][1][ 6][11] =   1.332999e-05;
  val[0][1][1][ 6][12] =   4.324026e-04; eval[0][1][1][ 6][12] =   1.722991e-05;
  val[0][1][1][ 6][13] =   4.486083e-04; eval[0][1][1][ 6][13] =   2.147234e-05;
  val[0][1][1][ 6][14] =   4.722770e-04; eval[0][1][1][ 6][14] =   2.721657e-05;
  val[0][1][1][ 6][15] =   4.727651e-04; eval[0][1][1][ 6][15] =   3.459190e-05;
  val[0][1][1][ 6][16] =   5.819808e-04; eval[0][1][1][ 6][16] =   4.428026e-05;
  val[0][1][1][ 6][17] =   5.200938e-04; eval[0][1][1][ 6][17] =   5.360113e-05;
  val[0][1][1][ 6][18] =   5.714833e-04; eval[0][1][1][ 6][18] =   7.045677e-05;
  val[0][1][1][ 6][19] =   6.004413e-04; eval[0][1][1][ 6][19] =   6.868785e-05;
  val[0][1][1][ 6][20] =   5.201514e-04; eval[0][1][1][ 6][20] =   6.277785e-05;
  val[0][1][1][ 6][21] =   7.391149e-04; eval[0][1][1][ 6][21] =   9.650498e-05;
  val[0][1][1][ 6][22] =   5.239467e-04; eval[0][1][1][ 6][22] =   1.164670e-04;
  val[0][1][1][ 6][23] =   4.256545e-04; eval[0][1][1][ 6][23] =   1.305247e-04;
  val[0][1][1][ 6][24] =   6.633470e-04; eval[0][1][1][ 6][24] =   1.787734e-04;
  val[0][1][1][ 6][25] =   5.624132e-04; eval[0][1][1][ 6][25] =   2.393808e-04;
  val[0][1][1][ 6][26] =   6.710372e-04; eval[0][1][1][ 6][26] =   3.343402e-04;
  val[0][1][1][ 6][27] =   9.556676e-04; eval[0][1][1][ 6][27] =   3.509261e-04;
  val[0][1][1][ 6][28] =   9.832838e-04; eval[0][1][1][ 6][28] =   3.854064e-04;
  val[0][1][1][ 6][29] =   1.265349e-03; eval[0][1][1][ 6][29] =   6.008531e-04;
  //emcdphi_mean_c1d1z7
  n_val[0][1][1][7] = 30;
  val[0][1][1][ 7][ 0] =   0.000000e+00; eval[0][1][1][ 7][ 0] =   0.000000e+00;
  val[0][1][1][ 7][ 1] =   0.000000e+00; eval[0][1][1][ 7][ 1] =   0.000000e+00;
  val[0][1][1][ 7][ 2] =  -2.576595e-03; eval[0][1][1][ 7][ 2] =   1.393232e-06;
  val[0][1][1][ 7][ 3] =  -1.888910e-03; eval[0][1][1][ 7][ 3] =   1.385141e-06;
  val[0][1][1][ 7][ 4] =  -1.174177e-03; eval[0][1][1][ 7][ 4] =   1.820788e-06;
  val[0][1][1][ 7][ 5] =  -7.508732e-04; eval[0][1][1][ 7][ 5] =   2.279641e-06;
  val[0][1][1][ 7][ 6] =  -5.898295e-04; eval[0][1][1][ 7][ 6] =   2.881689e-06;
  val[0][1][1][ 7][ 7] =  -3.290910e-04; eval[0][1][1][ 7][ 7] =   4.518302e-06;
  val[0][1][1][ 7][ 8] =  -1.148923e-04; eval[0][1][1][ 7][ 8] =   5.824804e-06;
  val[0][1][1][ 7][ 9] =   1.091590e-04; eval[0][1][1][ 7][ 9] =   7.898832e-06;
  val[0][1][1][ 7][10] =   2.559868e-04; eval[0][1][1][ 7][10] =   1.073010e-05;
  val[0][1][1][ 7][11] =   4.331213e-04; eval[0][1][1][ 7][11] =   1.443952e-05;
  val[0][1][1][ 7][12] =   5.615097e-04; eval[0][1][1][ 7][12] =   1.823953e-05;
  val[0][1][1][ 7][13] =   5.184267e-04; eval[0][1][1][ 7][13] =   2.360740e-05;
  val[0][1][1][ 7][14] =   4.384028e-04; eval[0][1][1][ 7][14] =   2.953171e-05;
  val[0][1][1][ 7][15] =   4.816262e-04; eval[0][1][1][ 7][15] =   4.059093e-05;
  val[0][1][1][ 7][16] =   4.823546e-04; eval[0][1][1][ 7][16] =   5.024784e-05;
  val[0][1][1][ 7][17] =   4.361852e-04; eval[0][1][1][ 7][17] =   6.093280e-05;
  val[0][1][1][ 7][18] =   4.700433e-04; eval[0][1][1][ 7][18] =   7.462706e-05;
  val[0][1][1][ 7][19] =   5.334935e-04; eval[0][1][1][ 7][19] =   1.000875e-04;
  val[0][1][1][ 7][20] =   4.398754e-04; eval[0][1][1][ 7][20] =   6.621934e-05;
  val[0][1][1][ 7][21] =   4.930137e-04; eval[0][1][1][ 7][21] =   8.242732e-05;
  val[0][1][1][ 7][22] =   5.169432e-04; eval[0][1][1][ 7][22] =   1.204976e-04;
  val[0][1][1][ 7][23] =   5.229091e-04; eval[0][1][1][ 7][23] =   1.442892e-04;
  val[0][1][1][ 7][24] =   2.314121e-04; eval[0][1][1][ 7][24] =   2.415685e-04;
  val[0][1][1][ 7][25] =   2.773159e-04; eval[0][1][1][ 7][25] =   2.586274e-04;
  val[0][1][1][ 7][26] =   2.623294e-05; eval[0][1][1][ 7][26] =   2.965208e-04;
  val[0][1][1][ 7][27] =   1.651270e-03; eval[0][1][1][ 7][27] =   4.135485e-04;
  val[0][1][1][ 7][28] =   1.693592e-04; eval[0][1][1][ 7][28] =   6.836664e-04;
  val[0][1][1][ 7][29] =  -3.439510e-05; eval[0][1][1][ 7][29] =   6.828939e-04;
  //emcdphi_mean_c1d1z8
  n_val[0][1][1][8] = 30;
  val[0][1][1][ 8][ 0] =   0.000000e+00; eval[0][1][1][ 8][ 0] =   0.000000e+00;
  val[0][1][1][ 8][ 1] =   0.000000e+00; eval[0][1][1][ 8][ 1] =   0.000000e+00;
  val[0][1][1][ 8][ 2] =  -1.805840e-03; eval[0][1][1][ 8][ 2] =   1.286545e-06;
  val[0][1][1][ 8][ 3] =  -1.563984e-03; eval[0][1][1][ 8][ 3] =   1.440873e-06;
  val[0][1][1][ 8][ 4] =  -1.032147e-03; eval[0][1][1][ 8][ 4] =   1.907480e-06;
  val[0][1][1][ 8][ 5] =  -7.706058e-04; eval[0][1][1][ 8][ 5] =   2.456836e-06;
  val[0][1][1][ 8][ 6] =  -6.562390e-04; eval[0][1][1][ 8][ 6] =   3.200891e-06;
  val[0][1][1][ 8][ 7] =  -3.747980e-04; eval[0][1][1][ 8][ 7] =   4.436290e-06;
  val[0][1][1][ 8][ 8] =  -2.085292e-04; eval[0][1][1][ 8][ 8] =   5.873881e-06;
  val[0][1][1][ 8][ 9] =  -5.939049e-05; eval[0][1][1][ 8][ 9] =   8.960078e-06;
  val[0][1][1][ 8][10] =   3.874174e-05; eval[0][1][1][ 8][10] =   1.195105e-05;
  val[0][1][1][ 8][11] =   1.821927e-04; eval[0][1][1][ 8][11] =   1.543003e-05;
  val[0][1][1][ 8][12] =   2.423332e-04; eval[0][1][1][ 8][12] =   1.974837e-05;
  val[0][1][1][ 8][13] =   2.098769e-04; eval[0][1][1][ 8][13] =   2.480665e-05;
  val[0][1][1][ 8][14] =   2.380339e-04; eval[0][1][1][ 8][14] =   3.059370e-05;
  val[0][1][1][ 8][15] =   2.312743e-04; eval[0][1][1][ 8][15] =   3.980422e-05;
  val[0][1][1][ 8][16] =   2.092205e-04; eval[0][1][1][ 8][16] =   5.154426e-05;
  val[0][1][1][ 8][17] =   1.578100e-04; eval[0][1][1][ 8][17] =   6.195441e-05;
  val[0][1][1][ 8][18] =   1.928502e-04; eval[0][1][1][ 8][18] =   7.621896e-05;
  val[0][1][1][ 8][19] =   2.627869e-04; eval[0][1][1][ 8][19] =   7.281268e-05;
  val[0][1][1][ 8][20] =   3.517398e-04; eval[0][1][1][ 8][20] =   6.754767e-05;
  val[0][1][1][ 8][21] =   3.492620e-04; eval[0][1][1][ 8][21] =   9.331063e-05;
  val[0][1][1][ 8][22] =   7.938532e-05; eval[0][1][1][ 8][22] =   1.226198e-04;
  val[0][1][1][ 8][23] =   2.624223e-04; eval[0][1][1][ 8][23] =   1.574127e-04;
  val[0][1][1][ 8][24] =  -6.353172e-05; eval[0][1][1][ 8][24] =   3.075939e-04;
  val[0][1][1][ 8][25] =   3.260466e-04; eval[0][1][1][ 8][25] =   2.598126e-04;
  val[0][1][1][ 8][26] =   8.593698e-04; eval[0][1][1][ 8][26] =   5.517333e-04;
  val[0][1][1][ 8][27] =   1.749895e-03; eval[0][1][1][ 8][27] =   4.553340e-04;
  val[0][1][1][ 8][28] =  -1.535314e-04; eval[0][1][1][ 8][28] =   5.256651e-04;
  val[0][1][1][ 8][29] =  -1.719363e-03; eval[0][1][1][ 8][29] =   1.317893e-03;
  //emcdphi_mean_c1d1z9
  n_val[0][1][1][9] = 30;
  val[0][1][1][ 9][ 0] =   0.000000e+00; eval[0][1][1][ 9][ 0] =   0.000000e+00;
  val[0][1][1][ 9][ 1] =   0.000000e+00; eval[0][1][1][ 9][ 1] =   0.000000e+00;
  val[0][1][1][ 9][ 2] =  -6.231503e-04; eval[0][1][1][ 9][ 2] =   1.522402e-06;
  val[0][1][1][ 9][ 3] =  -8.350093e-04; eval[0][1][1][ 9][ 3] =   1.708713e-06;
  val[0][1][1][ 9][ 4] =  -4.224072e-04; eval[0][1][1][ 9][ 4] =   1.982943e-06;
  val[0][1][1][ 9][ 5] =  -2.694748e-04; eval[0][1][1][ 9][ 5] =   2.459076e-06;
  val[0][1][1][ 9][ 6] =  -3.378412e-04; eval[0][1][1][ 9][ 6] =   3.743537e-06;
  val[0][1][1][ 9][ 7] =  -1.268407e-04; eval[0][1][1][ 9][ 7] =   5.192845e-06;
  val[0][1][1][ 9][ 8] =   4.318109e-05; eval[0][1][1][ 9][ 8] =   7.078599e-06;
  val[0][1][1][ 9][ 9] =   2.031220e-04; eval[0][1][1][ 9][ 9] =   9.178746e-06;
  val[0][1][1][ 9][10] =   3.210261e-04; eval[0][1][1][ 9][10] =   1.213068e-05;
  val[0][1][1][ 9][11] =   3.850808e-04; eval[0][1][1][ 9][11] =   1.589804e-05;
  val[0][1][1][ 9][12] =   3.920802e-04; eval[0][1][1][ 9][12] =   2.011329e-05;
  val[0][1][1][ 9][13] =   3.253533e-04; eval[0][1][1][ 9][13] =   2.805020e-05;
  val[0][1][1][ 9][14] =   2.151351e-04; eval[0][1][1][ 9][14] =   3.541280e-05;
  val[0][1][1][ 9][15] =   2.421833e-04; eval[0][1][1][ 9][15] =   4.493307e-05;
  val[0][1][1][ 9][16] =   2.280391e-04; eval[0][1][1][ 9][16] =   5.304160e-05;
  val[0][1][1][ 9][17] =   2.071929e-04; eval[0][1][1][ 9][17] =   6.582738e-05;
  val[0][1][1][ 9][18] =   2.451591e-04; eval[0][1][1][ 9][18] =   7.820576e-05;
  val[0][1][1][ 9][19] =   2.887455e-04; eval[0][1][1][ 9][19] =   1.075419e-04;
  val[0][1][1][ 9][20] =   2.300438e-04; eval[0][1][1][ 9][20] =   7.095551e-05;
  val[0][1][1][ 9][21] =   3.103118e-04; eval[0][1][1][ 9][21] =   1.016433e-04;
  val[0][1][1][ 9][22] =   9.692927e-05; eval[0][1][1][ 9][22] =   1.381350e-04;
  val[0][1][1][ 9][23] =   1.201441e-04; eval[0][1][1][ 9][23] =   1.750785e-04;
  val[0][1][1][ 9][24] =   3.355228e-04; eval[0][1][1][ 9][24] =   2.094091e-04;
  val[0][1][1][ 9][25] =  -6.477184e-05; eval[0][1][1][ 9][25] =   3.377637e-04;
  val[0][1][1][ 9][26] =   6.335920e-04; eval[0][1][1][ 9][26] =   5.246750e-04;
  val[0][1][1][ 9][27] =  -4.053679e-04; eval[0][1][1][ 9][27] =   3.293600e-04;
  val[0][1][1][ 9][28] =  -8.827379e-04; eval[0][1][1][ 9][28] =   2.889270e-04;
  val[0][1][1][ 9][29] =   8.619516e-04; eval[0][1][1][ 9][29] =   6.550019e-04;
  //emcdphi_sigma_c0d0z0
  n_val[1][0][0][0] = 30;
  val[1][0][0][ 0][ 0] =   0.000000e+00; eval[1][0][0][ 0][ 0] =   0.000000e+00;
  val[1][0][0][ 0][ 1] =   0.000000e+00; eval[1][0][0][ 0][ 1] =   0.000000e+00;
  val[1][0][0][ 0][ 2] =   6.053518e-03; eval[1][0][0][ 0][ 2] =   2.130164e-06;
  val[1][0][0][ 0][ 3] =   5.354401e-03; eval[1][0][0][ 0][ 3] =   2.143965e-06;
  val[1][0][0][ 0][ 4] =   5.259712e-03; eval[1][0][0][ 0][ 4] =   2.491382e-06;
  val[1][0][0][ 0][ 5] =   5.048266e-03; eval[1][0][0][ 0][ 5] =   3.005062e-06;
  val[1][0][0][ 0][ 6] =   4.905546e-03; eval[1][0][0][ 0][ 6] =   3.631868e-06;
  val[1][0][0][ 0][ 7] =   4.853843e-03; eval[1][0][0][ 0][ 7] =   4.542280e-06;
  val[1][0][0][ 0][ 8] =   4.845994e-03; eval[1][0][0][ 0][ 8] =   5.786081e-06;
  val[1][0][0][ 0][ 9] =   4.882584e-03; eval[1][0][0][ 0][ 9] =   7.914097e-06;
  val[1][0][0][ 0][10] =   4.840180e-03; eval[1][0][0][ 0][10] =   1.030718e-05;
  val[1][0][0][ 0][11] =   4.667029e-03; eval[1][0][0][ 0][11] =   1.611543e-05;
  val[1][0][0][ 0][12] =   4.659533e-03; eval[1][0][0][ 0][12] =   2.091246e-05;
  val[1][0][0][ 0][13] =   4.684535e-03; eval[1][0][0][ 0][13] =   2.763275e-05;
  val[1][0][0][ 0][14] =   4.763176e-03; eval[1][0][0][ 0][14] =   3.656510e-05;
  val[1][0][0][ 0][15] =   4.701901e-03; eval[1][0][0][ 0][15] =   4.505142e-05;
  val[1][0][0][ 0][16] =   4.701544e-03; eval[1][0][0][ 0][16] =   5.545435e-05;
  val[1][0][0][ 0][17] =   4.743021e-03; eval[1][0][0][ 0][17] =   7.107331e-05;
  val[1][0][0][ 0][18] =   4.735412e-03; eval[1][0][0][ 0][18] =   8.733206e-05;
  val[1][0][0][ 0][19] =   4.864768e-03; eval[1][0][0][ 0][19] =   1.142842e-04;
  val[1][0][0][ 0][20] =   4.563695e-03; eval[1][0][0][ 0][20] =   5.552297e-05;
  val[1][0][0][ 0][21] =   4.465822e-03; eval[1][0][0][ 0][21] =   7.524974e-05;
  val[1][0][0][ 0][22] =   4.576609e-03; eval[1][0][0][ 0][22] =   1.288809e-04;
  val[1][0][0][ 0][23] =   4.202165e-03; eval[1][0][0][ 0][23] =   1.407208e-04;
  val[1][0][0][ 0][24] =   4.491878e-03; eval[1][0][0][ 0][24] =   1.247279e-04;
  val[1][0][0][ 0][25] =   4.031067e-03; eval[1][0][0][ 0][25] =   1.444638e-04;
  val[1][0][0][ 0][26] =   4.760565e-03; eval[1][0][0][ 0][26] =   5.377260e-04;
  val[1][0][0][ 0][27] =   4.273319e-03; eval[1][0][0][ 0][27] =   2.024960e-04;
  val[1][0][0][ 0][28] =   4.866765e-03; eval[1][0][0][ 0][28] =   3.444672e-04;
  val[1][0][0][ 0][29] =   4.839786e-03; eval[1][0][0][ 0][29] =   6.277779e-04;
  //emcdphi_sigma_c0d0z1
  n_val[1][0][0][1] = 30;
  val[1][0][0][ 1][ 0] =   0.000000e+00; eval[1][0][0][ 1][ 0] =   0.000000e+00;
  val[1][0][0][ 1][ 1] =   0.000000e+00; eval[1][0][0][ 1][ 1] =   0.000000e+00;
  val[1][0][0][ 1][ 2] =   6.001523e-03; eval[1][0][0][ 1][ 2] =   2.134435e-06;
  val[1][0][0][ 1][ 3] =   5.297521e-03; eval[1][0][0][ 1][ 3] =   2.094299e-06;
  val[1][0][0][ 1][ 4] =   5.090826e-03; eval[1][0][0][ 1][ 4] =   3.065499e-06;
  val[1][0][0][ 1][ 5] =   4.821394e-03; eval[1][0][0][ 1][ 5] =   3.522398e-06;
  val[1][0][0][ 1][ 6] =   4.705549e-03; eval[1][0][0][ 1][ 6] =   4.248312e-06;
  val[1][0][0][ 1][ 7] =   4.666701e-03; eval[1][0][0][ 1][ 7] =   5.427796e-06;
  val[1][0][0][ 1][ 8] =   4.742031e-03; eval[1][0][0][ 1][ 8] =   5.586767e-06;
  val[1][0][0][ 1][ 9] =   4.754342e-03; eval[1][0][0][ 1][ 9] =   7.547994e-06;
  val[1][0][0][ 1][10] =   4.723122e-03; eval[1][0][0][ 1][10] =   9.894335e-06;
  val[1][0][0][ 1][11] =   4.438287e-03; eval[1][0][0][ 1][11] =   1.928963e-05;
  val[1][0][0][ 1][12] =   4.443891e-03; eval[1][0][0][ 1][12] =   2.536044e-05;
  val[1][0][0][ 1][13] =   4.512275e-03; eval[1][0][0][ 1][13] =   2.524642e-05;
  val[1][0][0][ 1][14] =   4.540567e-03; eval[1][0][0][ 1][14] =   3.308231e-05;
  val[1][0][0][ 1][15] =   4.632839e-03; eval[1][0][0][ 1][15] =   4.416438e-05;
  val[1][0][0][ 1][16] =   4.571847e-03; eval[1][0][0][ 1][16] =   5.267546e-05;
  val[1][0][0][ 1][17] =   4.444723e-03; eval[1][0][0][ 1][17] =   5.969688e-05;
  val[1][0][0][ 1][18] =   4.580466e-03; eval[1][0][0][ 1][18] =   8.282704e-05;
  val[1][0][0][ 1][19] =   4.679566e-03; eval[1][0][0][ 1][19] =   1.055410e-04;
  val[1][0][0][ 1][20] =   4.447327e-03; eval[1][0][0][ 1][20] =   5.477905e-05;
  val[1][0][0][ 1][21] =   4.468663e-03; eval[1][0][0][ 1][21] =   7.784242e-05;
  val[1][0][0][ 1][22] =   4.332145e-03; eval[1][0][0][ 1][22] =   1.563141e-04;
  val[1][0][0][ 1][23] =   4.588490e-03; eval[1][0][0][ 1][23] =   2.406174e-04;
  val[1][0][0][ 1][24] =   4.334148e-03; eval[1][0][0][ 1][24] =   2.669820e-04;
  val[1][0][0][ 1][25] =   4.341847e-03; eval[1][0][0][ 1][25] =   3.200928e-04;
  val[1][0][0][ 1][26] =   4.305669e-03; eval[1][0][0][ 1][26] =   2.199036e-04;
  val[1][0][0][ 1][27] =   5.384378e-03; eval[1][0][0][ 1][27] =   3.156126e-04;
  val[1][0][0][ 1][28] =   4.230085e-03; eval[1][0][0][ 1][28] =   2.806981e-04;
  val[1][0][0][ 1][29] =   3.984295e-03; eval[1][0][0][ 1][29] =   4.248053e-04;
  //emcdphi_sigma_c0d0z2
  n_val[1][0][0][2] = 30;
  val[1][0][0][ 2][ 0] =   0.000000e+00; eval[1][0][0][ 2][ 0] =   0.000000e+00;
  val[1][0][0][ 2][ 1] =   0.000000e+00; eval[1][0][0][ 2][ 1] =   0.000000e+00;
  val[1][0][0][ 2][ 2] =   5.953626e-03; eval[1][0][0][ 2][ 2] =   2.069743e-06;
  val[1][0][0][ 2][ 3] =   5.291216e-03; eval[1][0][0][ 2][ 3] =   2.062827e-06;
  val[1][0][0][ 2][ 4] =   5.099852e-03; eval[1][0][0][ 2][ 4] =   2.370839e-06;
  val[1][0][0][ 2][ 5] =   4.829236e-03; eval[1][0][0][ 2][ 5] =   3.540838e-06;
  val[1][0][0][ 2][ 6] =   4.713667e-03; eval[1][0][0][ 2][ 6] =   4.294940e-06;
  val[1][0][0][ 2][ 7] =   4.619021e-03; eval[1][0][0][ 2][ 7] =   5.336767e-06;
  val[1][0][0][ 2][ 8] =   4.579261e-03; eval[1][0][0][ 2][ 8] =   6.683296e-06;
  val[1][0][0][ 2][ 9] =   4.611476e-03; eval[1][0][0][ 2][ 9] =   9.004440e-06;
  val[1][0][0][ 2][10] =   4.581530e-03; eval[1][0][0][ 2][10] =   1.183836e-05;
  val[1][0][0][ 2][11] =   4.346351e-03; eval[1][0][0][ 2][11] =   1.832544e-05;
  val[1][0][0][ 2][12] =   4.419093e-03; eval[1][0][0][ 2][12] =   2.485182e-05;
  val[1][0][0][ 2][13] =   4.448264e-03; eval[1][0][0][ 2][13] =   3.284805e-05;
  val[1][0][0][ 2][14] =   4.598896e-03; eval[1][0][0][ 2][14] =   3.376670e-05;
  val[1][0][0][ 2][15] =   4.662987e-03; eval[1][0][0][ 2][15] =   4.437459e-05;
  val[1][0][0][ 2][16] =   4.503857e-03; eval[1][0][0][ 2][16] =   5.061014e-05;
  val[1][0][0][ 2][17] =   4.476696e-03; eval[1][0][0][ 2][17] =   6.267839e-05;
  val[1][0][0][ 2][18] =   4.487866e-03; eval[1][0][0][ 2][18] =   7.727573e-05;
  val[1][0][0][ 2][19] =   4.517276e-03; eval[1][0][0][ 2][19] =   9.490465e-05;
  val[1][0][0][ 2][20] =   4.346222e-03; eval[1][0][0][ 2][20] =   5.134828e-05;
  val[1][0][0][ 2][21] =   4.309094e-03; eval[1][0][0][ 2][21] =   7.178817e-05;
  val[1][0][0][ 2][22] =   4.525022e-03; eval[1][0][0][ 2][22] =   1.098438e-04;
  val[1][0][0][ 2][23] =   4.383707e-03; eval[1][0][0][ 2][23] =   2.117698e-04;
  val[1][0][0][ 2][24] =   4.221267e-03; eval[1][0][0][ 2][24] =   2.548003e-04;
  val[1][0][0][ 2][25] =   4.538604e-03; eval[1][0][0][ 2][25] =   1.945817e-04;
  val[1][0][0][ 2][26] =   4.344501e-03; eval[1][0][0][ 2][26] =   1.809009e-04;
  val[1][0][0][ 2][27] =   4.182133e-03; eval[1][0][0][ 2][27] =   2.396219e-04;
  val[1][0][0][ 2][28] =   4.459853e-03; eval[1][0][0][ 2][28] =   3.360851e-04;
  val[1][0][0][ 2][29] =   3.764094e-03; eval[1][0][0][ 2][29] =   3.726779e-04;
  //emcdphi_sigma_c0d0z3
  n_val[1][0][0][3] = 30;
  val[1][0][0][ 3][ 0] =   0.000000e+00; eval[1][0][0][ 3][ 0] =   0.000000e+00;
  val[1][0][0][ 3][ 1] =   0.000000e+00; eval[1][0][0][ 3][ 1] =   0.000000e+00;
  val[1][0][0][ 3][ 2] =   5.998866e-03; eval[1][0][0][ 3][ 2] =   2.197075e-06;
  val[1][0][0][ 3][ 3] =   5.267493e-03; eval[1][0][0][ 3][ 3] =   2.052178e-06;
  val[1][0][0][ 3][ 4] =   5.121404e-03; eval[1][0][0][ 3][ 4] =   2.364502e-06;
  val[1][0][0][ 3][ 5] =   4.792544e-03; eval[1][0][0][ 3][ 5] =   3.379464e-06;
  val[1][0][0][ 3][ 6] =   4.679024e-03; eval[1][0][0][ 3][ 6] =   4.069272e-06;
  val[1][0][0][ 3][ 7] =   4.663414e-03; eval[1][0][0][ 3][ 7] =   5.309756e-06;
  val[1][0][0][ 3][ 8] =   4.707712e-03; eval[1][0][0][ 3][ 8] =   7.021110e-06;
  val[1][0][0][ 3][ 9] =   4.767534e-03; eval[1][0][0][ 3][ 9] =   9.657754e-06;
  val[1][0][0][ 3][10] =   4.658256e-03; eval[1][0][0][ 3][10] =   1.200881e-05;
  val[1][0][0][ 3][11] =   4.560145e-03; eval[1][0][0][ 3][11] =   1.498374e-05;
  val[1][0][0][ 3][12] =   4.563026e-03; eval[1][0][0][ 3][12] =   2.661879e-05;
  val[1][0][0][ 3][13] =   4.618114e-03; eval[1][0][0][ 3][13] =   3.587679e-05;
  val[1][0][0][ 3][14] =   4.643821e-03; eval[1][0][0][ 3][14] =   3.394474e-05;
  val[1][0][0][ 3][15] =   4.593824e-03; eval[1][0][0][ 3][15] =   4.157680e-05;
  val[1][0][0][ 3][16] =   4.730755e-03; eval[1][0][0][ 3][16] =   5.624617e-05;
  val[1][0][0][ 3][17] =   4.706905e-03; eval[1][0][0][ 3][17] =   6.839238e-05;
  val[1][0][0][ 3][18] =   4.661287e-03; eval[1][0][0][ 3][18] =   8.294813e-05;
  val[1][0][0][ 3][19] =   4.576358e-03; eval[1][0][0][ 3][19] =   9.489043e-05;
  val[1][0][0][ 3][20] =   4.362817e-03; eval[1][0][0][ 3][20] =   6.191211e-05;
  val[1][0][0][ 3][21] =   4.383736e-03; eval[1][0][0][ 3][21] =   5.326164e-05;
  val[1][0][0][ 3][22] =   4.510004e-03; eval[1][0][0][ 3][22] =   1.038874e-04;
  val[1][0][0][ 3][23] =   4.262214e-03; eval[1][0][0][ 3][23] =   1.023498e-04;
  val[1][0][0][ 3][24] =   4.263852e-03; eval[1][0][0][ 3][24] =   1.524289e-04;
  val[1][0][0][ 3][25] =   4.544929e-03; eval[1][0][0][ 3][25] =   1.840502e-04;
  val[1][0][0][ 3][26] =   4.177244e-03; eval[1][0][0][ 3][26] =   1.606337e-04;
  val[1][0][0][ 3][27] =   4.130869e-03; eval[1][0][0][ 3][27] =   2.067916e-04;
  val[1][0][0][ 3][28] =   6.398755e-03; eval[1][0][0][ 3][28] =   1.888630e-03;
  val[1][0][0][ 3][29] =   4.587060e-03; eval[1][0][0][ 3][29] =   4.026834e-04;
  //emcdphi_sigma_c0d0z4
  n_val[1][0][0][4] = 30;
  val[1][0][0][ 4][ 0] =   0.000000e+00; eval[1][0][0][ 4][ 0] =   0.000000e+00;
  val[1][0][0][ 4][ 1] =   0.000000e+00; eval[1][0][0][ 4][ 1] =   0.000000e+00;
  val[1][0][0][ 4][ 2] =   5.912475e-03; eval[1][0][0][ 4][ 2] =   2.514394e-06;
  val[1][0][0][ 4][ 3] =   5.181923e-03; eval[1][0][0][ 4][ 3] =   2.322604e-06;
  val[1][0][0][ 4][ 4] =   5.014284e-03; eval[1][0][0][ 4][ 4] =   2.610213e-06;
  val[1][0][0][ 4][ 5] =   4.701939e-03; eval[1][0][0][ 4][ 5] =   3.744629e-06;
  val[1][0][0][ 4][ 6] =   4.607390e-03; eval[1][0][0][ 4][ 6] =   4.532586e-06;
  val[1][0][0][ 4][ 7] =   4.634147e-03; eval[1][0][0][ 4][ 7] =   6.102205e-06;
  val[1][0][0][ 4][ 8] =   4.706817e-03; eval[1][0][0][ 4][ 8] =   8.231821e-06;
  val[1][0][0][ 4][ 9] =   4.704844e-03; eval[1][0][0][ 4][ 9] =   1.098320e-05;
  val[1][0][0][ 4][10] =   4.586434e-03; eval[1][0][0][ 4][10] =   1.359214e-05;
  val[1][0][0][ 4][11] =   4.412857e-03; eval[1][0][0][ 4][11] =   2.184914e-05;
  val[1][0][0][ 4][12] =   4.483723e-03; eval[1][0][0][ 4][12] =   2.966771e-05;
  val[1][0][0][ 4][13] =   4.455999e-03; eval[1][0][0][ 4][13] =   3.783014e-05;
  val[1][0][0][ 4][14] =   4.556081e-03; eval[1][0][0][ 4][14] =   5.144048e-05;
  val[1][0][0][ 4][15] =   4.624914e-03; eval[1][0][0][ 4][15] =   6.765799e-05;
  val[1][0][0][ 4][16] =   4.570549e-03; eval[1][0][0][ 4][16] =   5.863109e-05;
  val[1][0][0][ 4][17] =   4.503926e-03; eval[1][0][0][ 4][17] =   9.678303e-05;
  val[1][0][0][ 4][18] =   4.453631e-03; eval[1][0][0][ 4][18] =   1.142072e-04;
  val[1][0][0][ 4][19] =   4.371435e-03; eval[1][0][0][ 4][19] =   7.463677e-05;
  val[1][0][0][ 4][20] =   4.346387e-03; eval[1][0][0][ 4][20] =   5.011433e-05;
  val[1][0][0][ 4][21] =   4.315566e-03; eval[1][0][0][ 4][21] =   9.520493e-05;
  val[1][0][0][ 4][22] =   4.405467e-03; eval[1][0][0][ 4][22] =   9.072943e-05;
  val[1][0][0][ 4][23] =   4.187121e-03; eval[1][0][0][ 4][23] =   1.071311e-04;
  val[1][0][0][ 4][24] =   4.150529e-03; eval[1][0][0][ 4][24] =   1.338102e-04;
  val[1][0][0][ 4][25] =   3.868684e-03; eval[1][0][0][ 4][25] =   2.181767e-04;
  val[1][0][0][ 4][26] =   4.461381e-03; eval[1][0][0][ 4][26] =   5.048396e-04;
  val[1][0][0][ 4][27] =   4.723315e-03; eval[1][0][0][ 4][27] =   5.278278e-04;
  val[1][0][0][ 4][28] =   4.428506e-03; eval[1][0][0][ 4][28] =   3.177309e-04;
  val[1][0][0][ 4][29] =   4.060353e-03; eval[1][0][0][ 4][29] =   4.650547e-04;
  //emcdphi_sigma_c0d0z5
  n_val[1][0][0][5] = 30;
  val[1][0][0][ 5][ 0] =   0.000000e+00; eval[1][0][0][ 5][ 0] =   0.000000e+00;
  val[1][0][0][ 5][ 1] =   0.000000e+00; eval[1][0][0][ 5][ 1] =   0.000000e+00;
  val[1][0][0][ 5][ 2] =   6.037485e-03; eval[1][0][0][ 5][ 2] =   2.849765e-06;
  val[1][0][0][ 5][ 3] =   5.416447e-03; eval[1][0][0][ 5][ 3] =   2.870564e-06;
  val[1][0][0][ 5][ 4] =   5.218266e-03; eval[1][0][0][ 5][ 4] =   3.212037e-06;
  val[1][0][0][ 5][ 5] =   4.860774e-03; eval[1][0][0][ 5][ 5] =   4.682199e-06;
  val[1][0][0][ 5][ 6] =   4.676153e-03; eval[1][0][0][ 5][ 6] =   5.524903e-06;
  val[1][0][0][ 5][ 7] =   4.670088e-03; eval[1][0][0][ 5][ 7] =   7.246264e-06;
  val[1][0][0][ 5][ 8] =   4.755652e-03; eval[1][0][0][ 5][ 8] =   9.785593e-06;
  val[1][0][0][ 5][ 9] =   4.772548e-03; eval[1][0][0][ 5][ 9] =   1.317416e-05;
  val[1][0][0][ 5][10] =   4.804353e-03; eval[1][0][0][ 5][10] =   2.322170e-05;
  val[1][0][0][ 5][11] =   4.700467e-03; eval[1][0][0][ 5][11] =   3.026914e-05;
  val[1][0][0][ 5][12] =   4.576131e-03; eval[1][0][0][ 5][12] =   2.645612e-05;
  val[1][0][0][ 5][13] =   4.585645e-03; eval[1][0][0][ 5][13] =   3.450927e-05;
  val[1][0][0][ 5][14] =   4.634905e-03; eval[1][0][0][ 5][14] =   4.517750e-05;
  val[1][0][0][ 5][15] =   4.578652e-03; eval[1][0][0][ 5][15] =   5.408762e-05;
  val[1][0][0][ 5][16] =   4.577013e-03; eval[1][0][0][ 5][16] =   6.785041e-05;
  val[1][0][0][ 5][17] =   4.592078e-03; eval[1][0][0][ 5][17] =   8.395310e-05;
  val[1][0][0][ 5][18] =   4.446598e-03; eval[1][0][0][ 5][18] =   9.609473e-05;
  val[1][0][0][ 5][19] =   4.338078e-03; eval[1][0][0][ 5][19] =   8.514074e-05;
  val[1][0][0][ 5][20] =   4.416515e-03; eval[1][0][0][ 5][20] =   5.462304e-05;
  val[1][0][0][ 5][21] =   4.438594e-03; eval[1][0][0][ 5][21] =   8.114803e-05;
  val[1][0][0][ 5][22] =   3.926110e-03; eval[1][0][0][ 5][22] =   1.001306e-04;
  val[1][0][0][ 5][23] =   4.113701e-03; eval[1][0][0][ 5][23] =   1.536706e-04;
  val[1][0][0][ 5][24] =   4.055799e-03; eval[1][0][0][ 5][24] =   1.454511e-04;
  val[1][0][0][ 5][25] =   4.040408e-03; eval[1][0][0][ 5][25] =   2.684686e-04;
  val[1][0][0][ 5][26] =   4.237825e-03; eval[1][0][0][ 5][26] =   2.691793e-04;
  val[1][0][0][ 5][27] =   4.030566e-03; eval[1][0][0][ 5][27] =   3.221996e-04;
  val[1][0][0][ 5][28] =   3.848248e-03; eval[1][0][0][ 5][28] =   3.643169e-04;
  val[1][0][0][ 5][29] =   2.530779e-02; eval[1][0][0][ 5][29] =   2.532514e-02;
  //emcdphi_sigma_c0d0z6
  n_val[1][0][0][6] = 30;
  val[1][0][0][ 6][ 0] =   0.000000e+00; eval[1][0][0][ 6][ 0] =   0.000000e+00;
  val[1][0][0][ 6][ 1] =   0.000000e+00; eval[1][0][0][ 6][ 1] =   0.000000e+00;
  val[1][0][0][ 6][ 2] =   5.991625e-03; eval[1][0][0][ 6][ 2] =   2.336930e-06;
  val[1][0][0][ 6][ 3] =   5.393715e-03; eval[1][0][0][ 6][ 3] =   2.321046e-06;
  val[1][0][0][ 6][ 4] =   5.242912e-03; eval[1][0][0][ 6][ 4] =   2.703342e-06;
  val[1][0][0][ 6][ 5] =   4.860944e-03; eval[1][0][0][ 6][ 5] =   3.882397e-06;
  val[1][0][0][ 6][ 6] =   4.644270e-03; eval[1][0][0][ 6][ 6] =   4.471310e-06;
  val[1][0][0][ 6][ 7] =   4.676725e-03; eval[1][0][0][ 6][ 7] =   6.018372e-06;
  val[1][0][0][ 6][ 8] =   4.827937e-03; eval[1][0][0][ 6][ 8] =   8.355304e-06;
  val[1][0][0][ 6][ 9] =   4.924378e-03; eval[1][0][0][ 6][ 9] =   1.156062e-05;
  val[1][0][0][ 6][10] =   4.837043e-03; eval[1][0][0][ 6][10] =   1.112303e-05;
  val[1][0][0][ 6][11] =   4.793918e-03; eval[1][0][0][ 6][11] =   1.445340e-05;
  val[1][0][0][ 6][12] =   4.790750e-03; eval[1][0][0][ 6][12] =   1.883118e-05;
  val[1][0][0][ 6][13] =   4.766641e-03; eval[1][0][0][ 6][13] =   2.420749e-05;
  val[1][0][0][ 6][14] =   4.787645e-03; eval[1][0][0][ 6][14] =   4.072654e-05;
  val[1][0][0][ 6][15] =   4.785155e-03; eval[1][0][0][ 6][15] =   5.062193e-05;
  val[1][0][0][ 6][16] =   4.739262e-03; eval[1][0][0][ 6][16] =   6.094489e-05;
  val[1][0][0][ 6][17] =   4.741435e-03; eval[1][0][0][ 6][17] =   7.587771e-05;
  val[1][0][0][ 6][18] =   4.766656e-03; eval[1][0][0][ 6][18] =   9.618718e-05;
  val[1][0][0][ 6][19] =   4.454769e-03; eval[1][0][0][ 6][19] =   6.227213e-05;
  val[1][0][0][ 6][20] =   4.460311e-03; eval[1][0][0][ 6][20] =   4.688948e-05;
  val[1][0][0][ 6][21] =   4.452088e-03; eval[1][0][0][ 6][21] =   5.808970e-05;
  val[1][0][0][ 6][22] =   4.414748e-03; eval[1][0][0][ 6][22] =   8.069575e-05;
  val[1][0][0][ 6][23] =   4.156000e-03; eval[1][0][0][ 6][23] =   1.189001e-04;
  val[1][0][0][ 6][24] =   4.264946e-03; eval[1][0][0][ 6][24] =   1.505203e-04;
  val[1][0][0][ 6][25] =   4.337297e-03; eval[1][0][0][ 6][25] =   1.601392e-04;
  val[1][0][0][ 6][26] =   4.248177e-03; eval[1][0][0][ 6][26] =   1.951166e-04;
  val[1][0][0][ 6][27] =   4.012810e-03; eval[1][0][0][ 6][27] =   3.845548e-04;
  val[1][0][0][ 6][28] =   4.388135e-03; eval[1][0][0][ 6][28] =   3.767257e-04;
  val[1][0][0][ 6][29] =   3.595422e-03; eval[1][0][0][ 6][29] =   3.431417e-04;
  //emcdphi_sigma_c0d0z7
  n_val[1][0][0][7] = 30;
  val[1][0][0][ 7][ 0] =   0.000000e+00; eval[1][0][0][ 7][ 0] =   0.000000e+00;
  val[1][0][0][ 7][ 1] =   0.000000e+00; eval[1][0][0][ 7][ 1] =   0.000000e+00;
  val[1][0][0][ 7][ 2] =   5.978020e-03; eval[1][0][0][ 7][ 2] =   2.271570e-06;
  val[1][0][0][ 7][ 3] =   5.361199e-03; eval[1][0][0][ 7][ 3] =   2.285853e-06;
  val[1][0][0][ 7][ 4] =   5.177687e-03; eval[1][0][0][ 7][ 4] =   2.570663e-06;
  val[1][0][0][ 7][ 5] =   4.877366e-03; eval[1][0][0][ 7][ 5] =   3.956376e-06;
  val[1][0][0][ 7][ 6] =   4.602194e-03; eval[1][0][0][ 7][ 6] =   4.453030e-06;
  val[1][0][0][ 7][ 7] =   4.621700e-03; eval[1][0][0][ 7][ 7] =   8.022509e-06;
  val[1][0][0][ 7][ 8] =   4.698805e-03; eval[1][0][0][ 7][ 8] =   5.968517e-06;
  val[1][0][0][ 7][ 9] =   4.725184e-03; eval[1][0][0][ 7][ 9] =   7.926014e-06;
  val[1][0][0][ 7][10] =   4.812574e-03; eval[1][0][0][ 7][10] =   1.436894e-05;
  val[1][0][0][ 7][11] =   4.724890e-03; eval[1][0][0][ 7][11] =   1.802234e-05;
  val[1][0][0][ 7][12] =   4.734815e-03; eval[1][0][0][ 7][12] =   2.356620e-05;
  val[1][0][0][ 7][13] =   4.673607e-03; eval[1][0][0][ 7][13] =   2.946860e-05;
  val[1][0][0][ 7][14] =   4.679627e-03; eval[1][0][0][ 7][14] =   3.750922e-05;
  val[1][0][0][ 7][15] =   4.543510e-03; eval[1][0][0][ 7][15] =   4.338685e-05;
  val[1][0][0][ 7][16] =   4.573105e-03; eval[1][0][0][ 7][16] =   5.463327e-05;
  val[1][0][0][ 7][17] =   4.538299e-03; eval[1][0][0][ 7][17] =   6.803769e-05;
  val[1][0][0][ 7][18] =   4.533137e-03; eval[1][0][0][ 7][18] =   8.315614e-05;
  val[1][0][0][ 7][19] =   4.371829e-03; eval[1][0][0][ 7][19] =   5.908924e-05;
  val[1][0][0][ 7][20] =   4.313122e-03; eval[1][0][0][ 7][20] =   4.343097e-05;
  val[1][0][0][ 7][21] =   4.351020e-03; eval[1][0][0][ 7][21] =   6.279987e-05;
  val[1][0][0][ 7][22] =   4.366029e-03; eval[1][0][0][ 7][22] =   7.751265e-05;
  val[1][0][0][ 7][23] =   4.113722e-03; eval[1][0][0][ 7][23] =   1.307075e-04;
  val[1][0][0][ 7][24] =   4.272164e-03; eval[1][0][0][ 7][24] =   1.240921e-04;
  val[1][0][0][ 7][25] =   4.390707e-03; eval[1][0][0][ 7][25] =   1.800614e-04;
  val[1][0][0][ 7][26] =   3.837929e-03; eval[1][0][0][ 7][26] =   1.811390e-04;
  val[1][0][0][ 7][27] =   4.147917e-03; eval[1][0][0][ 7][27] =   2.881865e-04;
  val[1][0][0][ 7][28] =   4.737320e-03; eval[1][0][0][ 7][28] =   4.034677e-04;
  val[1][0][0][ 7][29] =   4.158989e-03; eval[1][0][0][ 7][29] =   4.097187e-04;
  //emcdphi_sigma_c0d0z8
  n_val[1][0][0][8] = 30;
  val[1][0][0][ 8][ 0] =   0.000000e+00; eval[1][0][0][ 8][ 0] =   0.000000e+00;
  val[1][0][0][ 8][ 1] =   0.000000e+00; eval[1][0][0][ 8][ 1] =   0.000000e+00;
  val[1][0][0][ 8][ 2] =   5.880855e-03; eval[1][0][0][ 8][ 2] =   2.180132e-06;
  val[1][0][0][ 8][ 3] =   5.226942e-03; eval[1][0][0][ 8][ 3] =   2.159009e-06;
  val[1][0][0][ 8][ 4] =   5.081239e-03; eval[1][0][0][ 8][ 4] =   2.448203e-06;
  val[1][0][0][ 8][ 5] =   4.844111e-03; eval[1][0][0][ 8][ 5] =   3.033936e-06;
  val[1][0][0][ 8][ 6] =   4.590936e-03; eval[1][0][0][ 8][ 6] =   4.406289e-06;
  val[1][0][0][ 8][ 7] =   4.520414e-03; eval[1][0][0][ 8][ 7] =   5.450644e-06;
  val[1][0][0][ 8][ 8] =   4.586974e-03; eval[1][0][0][ 8][ 8] =   7.159196e-06;
  val[1][0][0][ 8][ 9] =   4.609302e-03; eval[1][0][0][ 8][ 9] =   9.598678e-06;
  val[1][0][0][ 8][10] =   4.578035e-03; eval[1][0][0][ 8][10] =   1.245987e-05;
  val[1][0][0][ 8][11] =   4.514289e-03; eval[1][0][0][ 8][11] =   1.572327e-05;
  val[1][0][0][ 8][12] =   4.482577e-03; eval[1][0][0][ 8][12] =   2.012306e-05;
  val[1][0][0][ 8][13] =   4.493228e-03; eval[1][0][0][ 8][13] =   2.633675e-05;
  val[1][0][0][ 8][14] =   4.444726e-03; eval[1][0][0][ 8][14] =   3.241957e-05;
  val[1][0][0][ 8][15] =   4.410078e-03; eval[1][0][0][ 8][15] =   3.984116e-05;
  val[1][0][0][ 8][16] =   4.381752e-03; eval[1][0][0][ 8][16] =   4.879456e-05;
  val[1][0][0][ 8][17] =   4.326813e-03; eval[1][0][0][ 8][17] =   5.822323e-05;
  val[1][0][0][ 8][18] =   4.355802e-03; eval[1][0][0][ 8][18] =   7.376562e-05;
  val[1][0][0][ 8][19] =   4.274896e-03; eval[1][0][0][ 8][19] =   8.464400e-05;
  val[1][0][0][ 8][20] =   4.222576e-03; eval[1][0][0][ 8][20] =   5.053317e-05;
  val[1][0][0][ 8][21] =   4.037974e-03; eval[1][0][0][ 8][21] =   6.369959e-05;
  val[1][0][0][ 8][22] =   4.223570e-03; eval[1][0][0][ 8][22] =   9.407349e-05;
  val[1][0][0][ 8][23] =   4.033965e-03; eval[1][0][0][ 8][23] =   9.939120e-05;
  val[1][0][0][ 8][24] =   4.190771e-03; eval[1][0][0][ 8][24] =   1.962002e-04;
  val[1][0][0][ 8][25] =   4.125902e-03; eval[1][0][0][ 8][25] =   1.611143e-04;
  val[1][0][0][ 8][26] =   3.916856e-03; eval[1][0][0][ 8][26] =   2.378591e-04;
  val[1][0][0][ 8][27] =   3.470692e-03; eval[1][0][0][ 8][27] =   3.140682e-04;
  val[1][0][0][ 8][28] =   3.469368e-03; eval[1][0][0][ 8][28] =   2.864303e-04;
  val[1][0][0][ 8][29] =   4.073096e-03; eval[1][0][0][ 8][29] =   4.258273e-04;
  //emcdphi_sigma_c0d0z9
  n_val[1][0][0][9] = 30;
  val[1][0][0][ 9][ 0] =   0.000000e+00; eval[1][0][0][ 9][ 0] =   0.000000e+00;
  val[1][0][0][ 9][ 1] =   0.000000e+00; eval[1][0][0][ 9][ 1] =   0.000000e+00;
  val[1][0][0][ 9][ 2] =   5.854961e-03; eval[1][0][0][ 9][ 2] =   2.231880e-06;
  val[1][0][0][ 9][ 3] =   5.194035e-03; eval[1][0][0][ 9][ 3] =   2.221886e-06;
  val[1][0][0][ 9][ 4] =   5.020775e-03; eval[1][0][0][ 9][ 4] =   2.442587e-06;
  val[1][0][0][ 9][ 5] =   4.793116e-03; eval[1][0][0][ 9][ 5] =   3.790898e-06;
  val[1][0][0][ 9][ 6] =   4.503569e-03; eval[1][0][0][ 9][ 6] =   4.181064e-06;
  val[1][0][0][ 9][ 7] =   4.431697e-03; eval[1][0][0][ 9][ 7] =   5.208735e-06;
  val[1][0][0][ 9][ 8] =   4.475341e-03; eval[1][0][0][ 9][ 8] =   6.688166e-06;
  val[1][0][0][ 9][ 9] =   4.466929e-03; eval[1][0][0][ 9][ 9] =   8.811274e-06;
  val[1][0][0][ 9][10] =   4.429577e-03; eval[1][0][0][ 9][10] =   1.154017e-05;
  val[1][0][0][ 9][11] =   4.409875e-03; eval[1][0][0][ 9][11] =   1.511640e-05;
  val[1][0][0][ 9][12] =   4.431048e-03; eval[1][0][0][ 9][12] =   2.012029e-05;
  val[1][0][0][ 9][13] =   4.388714e-03; eval[1][0][0][ 9][13] =   2.532542e-05;
  val[1][0][0][ 9][14] =   4.296677e-03; eval[1][0][0][ 9][14] =   3.071751e-05;
  val[1][0][0][ 9][15] =   4.332914e-03; eval[1][0][0][ 9][15] =   3.940566e-05;
  val[1][0][0][ 9][16] =   4.316774e-03; eval[1][0][0][ 9][16] =   4.868785e-05;
  val[1][0][0][ 9][17] =   4.257012e-03; eval[1][0][0][ 9][17] =   5.784516e-05;
  val[1][0][0][ 9][18] =   4.444478e-03; eval[1][0][0][ 9][18] =   1.117328e-04;
  val[1][0][0][ 9][19] =   4.199998e-03; eval[1][0][0][ 9][19] =   8.535200e-05;
  val[1][0][0][ 9][20] =   4.148448e-03; eval[1][0][0][ 9][20] =   5.078991e-05;
  val[1][0][0][ 9][21] =   4.056713e-03; eval[1][0][0][ 9][21] =   7.090590e-05;
  val[1][0][0][ 9][22] =   4.202950e-03; eval[1][0][0][ 9][22] =   1.011951e-04;
  val[1][0][0][ 9][23] =   4.242094e-03; eval[1][0][0][ 9][23] =   1.367638e-04;
  val[1][0][0][ 9][24] =   4.266427e-03; eval[1][0][0][ 9][24] =   1.735852e-04;
  val[1][0][0][ 9][25] =   4.319491e-03; eval[1][0][0][ 9][25] =   2.130167e-04;
  val[1][0][0][ 9][26] =   3.907079e-03; eval[1][0][0][ 9][26] =   1.750176e-04;
  val[1][0][0][ 9][27] =   3.960920e-03; eval[1][0][0][ 9][27] =   2.845911e-04;
  val[1][0][0][ 9][28] =   4.130804e-03; eval[1][0][0][ 9][28] =   2.741817e-04;
  val[1][0][0][ 9][29] =   4.063874e-03; eval[1][0][0][ 9][29] =   3.646600e-04;
  //emcdphi_sigma_c0d1z0
  n_val[1][0][1][0] = 30;
  val[1][0][1][ 0][ 0] =   0.000000e+00; eval[1][0][1][ 0][ 0] =   0.000000e+00;
  val[1][0][1][ 0][ 1] =   0.000000e+00; eval[1][0][1][ 0][ 1] =   0.000000e+00;
  val[1][0][1][ 0][ 2] =   4.704082e-03; eval[1][0][1][ 0][ 2] =   2.055443e-06;
  val[1][0][1][ 0][ 3] =   4.069143e-03; eval[1][0][1][ 0][ 3] =   2.152229e-06;
  val[1][0][1][ 0][ 4] =   3.879100e-03; eval[1][0][1][ 0][ 4] =   2.354411e-06;
  val[1][0][1][ 0][ 5] =   3.717698e-03; eval[1][0][1][ 0][ 5] =   2.614519e-06;
  val[1][0][1][ 0][ 6] =   3.592349e-03; eval[1][0][1][ 0][ 6] =   3.070193e-06;
  val[1][0][1][ 0][ 7] =   3.590121e-03; eval[1][0][1][ 0][ 7] =   5.696987e-06;
  val[1][0][1][ 0][ 8] =   3.600172e-03; eval[1][0][1][ 0][ 8] =   7.234739e-06;
  val[1][0][1][ 0][ 9] =   3.642763e-03; eval[1][0][1][ 0][ 9] =   9.544445e-06;
  val[1][0][1][ 0][10] =   3.642094e-03; eval[1][0][1][ 0][10] =   1.337516e-05;
  val[1][0][1][ 0][11] =   3.658310e-03; eval[1][0][1][ 0][11] =   1.791881e-05;
  val[1][0][1][ 0][12] =   3.709976e-03; eval[1][0][1][ 0][12] =   2.475980e-05;
  val[1][0][1][ 0][13] =   3.604110e-03; eval[1][0][1][ 0][13] =   3.007382e-05;
  val[1][0][1][ 0][14] =   3.621740e-03; eval[1][0][1][ 0][14] =   3.941756e-05;
  val[1][0][1][ 0][15] =   3.684643e-03; eval[1][0][1][ 0][15] =   3.648128e-05;
  val[1][0][1][ 0][16] =   3.643877e-03; eval[1][0][1][ 0][16] =   4.482465e-05;
  val[1][0][1][ 0][17] =   3.644927e-03; eval[1][0][1][ 0][17] =   5.567378e-05;
  val[1][0][1][ 0][18] =   3.615743e-03; eval[1][0][1][ 0][18] =   6.757300e-05;
  val[1][0][1][ 0][19] =   3.380132e-03; eval[1][0][1][ 0][19] =   1.020481e-04;
  val[1][0][1][ 0][20] =   3.444435e-03; eval[1][0][1][ 0][20] =   5.532541e-05;
  val[1][0][1][ 0][21] =   3.449325e-03; eval[1][0][1][ 0][21] =   7.868682e-05;
  val[1][0][1][ 0][22] =   3.369755e-03; eval[1][0][1][ 0][22] =   1.312110e-04;
  val[1][0][1][ 0][23] =   3.391832e-03; eval[1][0][1][ 0][23] =   1.193746e-04;
  val[1][0][1][ 0][24] =   3.730347e-03; eval[1][0][1][ 0][24] =   1.479519e-04;
  val[1][0][1][ 0][25] =   3.603186e-03; eval[1][0][1][ 0][25] =   1.801949e-04;
  val[1][0][1][ 0][26] =   3.948038e-03; eval[1][0][1][ 0][26] =   3.549693e-04;
  val[1][0][1][ 0][27] =   2.740379e-02; eval[1][0][1][ 0][27] =   2.075177e-02;
  val[1][0][1][ 0][28] =   3.897844e-03; eval[1][0][1][ 0][28] =   4.610245e-04;
  val[1][0][1][ 0][29] =   4.562171e-03; eval[1][0][1][ 0][29] =   7.290241e-04;
  //emcdphi_sigma_c0d1z1
  n_val[1][0][1][1] = 30;
  val[1][0][1][ 1][ 0] =   0.000000e+00; eval[1][0][1][ 1][ 0] =   0.000000e+00;
  val[1][0][1][ 1][ 1] =   0.000000e+00; eval[1][0][1][ 1][ 1] =   0.000000e+00;
  val[1][0][1][ 1][ 2] =   4.775563e-03; eval[1][0][1][ 1][ 2] =   1.964055e-06;
  val[1][0][1][ 1][ 3] =   4.093465e-03; eval[1][0][1][ 1][ 3] =   1.995668e-06;
  val[1][0][1][ 1][ 4] =   3.728453e-03; eval[1][0][1][ 1][ 4] =   2.793063e-06;
  val[1][0][1][ 1][ 5] =   3.751464e-03; eval[1][0][1][ 1][ 5] =   2.507346e-06;
  val[1][0][1][ 1][ 6] =   3.624343e-03; eval[1][0][1][ 1][ 6] =   2.849878e-06;
  val[1][0][1][ 1][ 7] =   3.642911e-03; eval[1][0][1][ 1][ 7] =   5.509141e-06;
  val[1][0][1][ 1][ 8] =   3.645853e-03; eval[1][0][1][ 1][ 8] =   7.050472e-06;
  val[1][0][1][ 1][ 9] =   3.666120e-03; eval[1][0][1][ 1][ 9] =   9.620434e-06;
  val[1][0][1][ 1][10] =   3.689680e-03; eval[1][0][1][ 1][10] =   1.307901e-05;
  val[1][0][1][ 1][11] =   3.685338e-03; eval[1][0][1][ 1][11] =   1.725960e-05;
  val[1][0][1][ 1][12] =   3.668796e-03; eval[1][0][1][ 1][12] =   2.253238e-05;
  val[1][0][1][ 1][13] =   3.697710e-03; eval[1][0][1][ 1][13] =   3.030621e-05;
  val[1][0][1][ 1][14] =   3.685423e-03; eval[1][0][1][ 1][14] =   3.897638e-05;
  val[1][0][1][ 1][15] =   3.743636e-03; eval[1][0][1][ 1][15] =   5.180629e-05;
  val[1][0][1][ 1][16] =   3.656576e-03; eval[1][0][1][ 1][16] =   4.157401e-05;
  val[1][0][1][ 1][17] =   3.642355e-03; eval[1][0][1][ 1][17] =   5.137167e-05;
  val[1][0][1][ 1][18] =   3.519676e-03; eval[1][0][1][ 1][18] =   8.588542e-05;
  val[1][0][1][ 1][19] =   3.580113e-03; eval[1][0][1][ 1][19] =   7.558501e-05;
  val[1][0][1][ 1][20] =   3.565072e-03; eval[1][0][1][ 1][20] =   4.500817e-05;
  val[1][0][1][ 1][21] =   3.443786e-03; eval[1][0][1][ 1][21] =   7.061857e-05;
  val[1][0][1][ 1][22] =   3.627005e-03; eval[1][0][1][ 1][22] =   9.246003e-05;
  val[1][0][1][ 1][23] =   3.631825e-03; eval[1][0][1][ 1][23] =   1.252554e-04;
  val[1][0][1][ 1][24] =   3.636607e-03; eval[1][0][1][ 1][24] =   2.740622e-04;
  val[1][0][1][ 1][25] =   3.563849e-03; eval[1][0][1][ 1][25] =   1.843116e-04;
  val[1][0][1][ 1][26] =   3.366391e-03; eval[1][0][1][ 1][26] =   2.218087e-04;
  val[1][0][1][ 1][27] =   3.733246e-03; eval[1][0][1][ 1][27] =   2.640202e-04;
  val[1][0][1][ 1][28] =   2.450099e-02; eval[1][0][1][ 1][28] =   2.928733e-02;
  val[1][0][1][ 1][29] =   4.940129e-02; eval[1][0][1][ 1][29] =   8.020574e-02;
  //emcdphi_sigma_c0d1z2
  n_val[1][0][1][2] = 30;
  val[1][0][1][ 2][ 0] =   0.000000e+00; eval[1][0][1][ 2][ 0] =   0.000000e+00;
  val[1][0][1][ 2][ 1] =   0.000000e+00; eval[1][0][1][ 2][ 1] =   0.000000e+00;
  val[1][0][1][ 2][ 2] =   4.908988e-03; eval[1][0][1][ 2][ 2] =   1.628537e-06;
  val[1][0][1][ 2][ 3] =   4.140316e-03; eval[1][0][1][ 2][ 3] =   2.048933e-06;
  val[1][0][1][ 2][ 4] =   3.865296e-03; eval[1][0][1][ 2][ 4] =   2.128209e-06;
  val[1][0][1][ 2][ 5] =   3.787339e-03; eval[1][0][1][ 2][ 5] =   2.513669e-06;
  val[1][0][1][ 2][ 6] =   3.687005e-03; eval[1][0][1][ 2][ 6] =   2.937271e-06;
  val[1][0][1][ 2][ 7] =   3.658445e-03; eval[1][0][1][ 2][ 7] =   3.784637e-06;
  val[1][0][1][ 2][ 8] =   3.707375e-03; eval[1][0][1][ 2][ 8] =   7.281553e-06;
  val[1][0][1][ 2][ 9] =   3.754480e-03; eval[1][0][1][ 2][ 9] =   1.014150e-05;
  val[1][0][1][ 2][10] =   3.747871e-03; eval[1][0][1][ 2][10] =   1.343307e-05;
  val[1][0][1][ 2][11] =   3.756680e-03; eval[1][0][1][ 2][11] =   1.810662e-05;
  val[1][0][1][ 2][12] =   3.697103e-03; eval[1][0][1][ 2][12] =   2.301702e-05;
  val[1][0][1][ 2][13] =   3.744095e-03; eval[1][0][1][ 2][13] =   3.143107e-05;
  val[1][0][1][ 2][14] =   3.783213e-03; eval[1][0][1][ 2][14] =   4.202515e-05;
  val[1][0][1][ 2][15] =   3.703976e-03; eval[1][0][1][ 2][15] =   5.108465e-05;
  val[1][0][1][ 2][16] =   3.603837e-03; eval[1][0][1][ 2][16] =   5.937836e-05;
  val[1][0][1][ 2][17] =   3.675697e-03; eval[1][0][1][ 2][17] =   7.844237e-05;
  val[1][0][1][ 2][18] =   3.678009e-03; eval[1][0][1][ 2][18] =   6.580000e-05;
  val[1][0][1][ 2][19] =   3.620500e-03; eval[1][0][1][ 2][19] =   7.751638e-05;
  val[1][0][1][ 2][20] =   3.701983e-03; eval[1][0][1][ 2][20] =   5.019190e-05;
  val[1][0][1][ 2][21] =   3.608689e-03; eval[1][0][1][ 2][21] =   6.732463e-05;
  val[1][0][1][ 2][22] =   3.516508e-03; eval[1][0][1][ 2][22] =   8.776317e-05;
  val[1][0][1][ 2][23] =   3.424075e-03; eval[1][0][1][ 2][23] =   1.368731e-04;
  val[1][0][1][ 2][24] =   3.383573e-03; eval[1][0][1][ 2][24] =   1.699647e-04;
  val[1][0][1][ 2][25] =   3.350589e-03; eval[1][0][1][ 2][25] =   1.719177e-04;
  val[1][0][1][ 2][26] =   3.699863e-03; eval[1][0][1][ 2][26] =   2.115983e-04;
  val[1][0][1][ 2][27] =   3.952749e-03; eval[1][0][1][ 2][27] =   4.578471e-04;
  val[1][0][1][ 2][28] =   2.903811e-03; eval[1][0][1][ 2][28] =   4.899343e-04;
  val[1][0][1][ 2][29] =   3.541079e-03; eval[1][0][1][ 2][29] =   4.332990e-04;
  //emcdphi_sigma_c0d1z3
  n_val[1][0][1][3] = 30;
  val[1][0][1][ 3][ 0] =   0.000000e+00; eval[1][0][1][ 3][ 0] =   0.000000e+00;
  val[1][0][1][ 3][ 1] =   0.000000e+00; eval[1][0][1][ 3][ 1] =   0.000000e+00;
  val[1][0][1][ 3][ 2] =   4.997098e-03; eval[1][0][1][ 3][ 2] =   1.699526e-06;
  val[1][0][1][ 3][ 3] =   4.309625e-03; eval[1][0][1][ 3][ 3] =   1.689304e-06;
  val[1][0][1][ 3][ 4] =   3.884208e-03; eval[1][0][1][ 3][ 4] =   2.108629e-06;
  val[1][0][1][ 3][ 5] =   3.843603e-03; eval[1][0][1][ 3][ 5] =   2.604517e-06;
  val[1][0][1][ 3][ 6] =   3.728923e-03; eval[1][0][1][ 3][ 6] =   3.007902e-06;
  val[1][0][1][ 3][ 7] =   3.690577e-03; eval[1][0][1][ 3][ 7] =   3.868326e-06;
  val[1][0][1][ 3][ 8] =   3.682430e-03; eval[1][0][1][ 3][ 8] =   4.989572e-06;
  val[1][0][1][ 3][ 9] =   3.784567e-03; eval[1][0][1][ 3][ 9] =   1.066687e-05;
  val[1][0][1][ 3][10] =   3.790251e-03; eval[1][0][1][ 3][10] =   1.434932e-05;
  val[1][0][1][ 3][11] =   3.772952e-03; eval[1][0][1][ 3][11] =   1.887122e-05;
  val[1][0][1][ 3][12] =   3.712906e-03; eval[1][0][1][ 3][12] =   2.397184e-05;
  val[1][0][1][ 3][13] =   3.708965e-03; eval[1][0][1][ 3][13] =   3.142656e-05;
  val[1][0][1][ 3][14] =   3.594430e-03; eval[1][0][1][ 3][14] =   3.747005e-05;
  val[1][0][1][ 3][15] =   3.616424e-03; eval[1][0][1][ 3][15] =   4.821190e-05;
  val[1][0][1][ 3][16] =   3.524609e-03; eval[1][0][1][ 3][16] =   5.735272e-05;
  val[1][0][1][ 3][17] =   3.657589e-03; eval[1][0][1][ 3][17] =   5.343514e-05;
  val[1][0][1][ 3][18] =   3.596025e-03; eval[1][0][1][ 3][18] =   9.105976e-05;
  val[1][0][1][ 3][19] =   3.610945e-03; eval[1][0][1][ 3][19] =   7.929657e-05;
  val[1][0][1][ 3][20] =   3.398124e-03; eval[1][0][1][ 3][20] =   4.972809e-05;
  val[1][0][1][ 3][21] =   3.557501e-03; eval[1][0][1][ 3][21] =   6.774835e-05;
  val[1][0][1][ 3][22] =   3.578038e-03; eval[1][0][1][ 3][22] =   9.817329e-05;
  val[1][0][1][ 3][23] =   3.380074e-03; eval[1][0][1][ 3][23] =   1.102065e-04;
  val[1][0][1][ 3][24] =   3.590183e-03; eval[1][0][1][ 3][24] =   2.631622e-04;
  val[1][0][1][ 3][25] =   3.312090e-03; eval[1][0][1][ 3][25] =   1.806474e-04;
  val[1][0][1][ 3][26] =   3.170006e-03; eval[1][0][1][ 3][26] =   2.623274e-04;
  val[1][0][1][ 3][27] =   3.518793e-03; eval[1][0][1][ 3][27] =   2.595424e-04;
  val[1][0][1][ 3][28] =   3.930214e-03; eval[1][0][1][ 3][28] =   3.889867e-04;
  val[1][0][1][ 3][29] =   3.095437e-03; eval[1][0][1][ 3][29] =   4.284675e-04;
  //emcdphi_sigma_c0d1z4
  n_val[1][0][1][4] = 30;
  val[1][0][1][ 4][ 0] =   0.000000e+00; eval[1][0][1][ 4][ 0] =   0.000000e+00;
  val[1][0][1][ 4][ 1] =   0.000000e+00; eval[1][0][1][ 4][ 1] =   0.000000e+00;
  val[1][0][1][ 4][ 2] =   4.978118e-03; eval[1][0][1][ 4][ 2] =   2.090840e-06;
  val[1][0][1][ 4][ 3] =   4.255711e-03; eval[1][0][1][ 4][ 3] =   1.956902e-06;
  val[1][0][1][ 4][ 4] =   3.855752e-03; eval[1][0][1][ 4][ 4] =   2.444432e-06;
  val[1][0][1][ 4][ 5] =   3.761995e-03; eval[1][0][1][ 4][ 5] =   4.134598e-06;
  val[1][0][1][ 4][ 6] =   3.720014e-03; eval[1][0][1][ 4][ 6] =   3.424145e-06;
  val[1][0][1][ 4][ 7] =   3.715299e-03; eval[1][0][1][ 4][ 7] =   4.515796e-06;
  val[1][0][1][ 4][ 8] =   3.724251e-03; eval[1][0][1][ 4][ 8] =   5.959526e-06;
  val[1][0][1][ 4][ 9] =   3.802925e-03; eval[1][0][1][ 4][ 9] =   1.258567e-05;
  val[1][0][1][ 4][10] =   3.805534e-03; eval[1][0][1][ 4][10] =   1.686342e-05;
  val[1][0][1][ 4][11] =   3.815163e-03; eval[1][0][1][ 4][11] =   2.277664e-05;
  val[1][0][1][ 4][12] =   3.714958e-03; eval[1][0][1][ 4][12] =   2.831901e-05;
  val[1][0][1][ 4][13] =   3.744397e-03; eval[1][0][1][ 4][13] =   3.809058e-05;
  val[1][0][1][ 4][14] =   3.631565e-03; eval[1][0][1][ 4][14] =   3.118037e-05;
  val[1][0][1][ 4][15] =   3.615804e-03; eval[1][0][1][ 4][15] =   5.711216e-05;
  val[1][0][1][ 4][16] =   3.686718e-03; eval[1][0][1][ 4][16] =   7.742415e-05;
  val[1][0][1][ 4][17] =   3.547619e-03; eval[1][0][1][ 4][17] =   5.928997e-05;
  val[1][0][1][ 4][18] =   3.830052e-03; eval[1][0][1][ 4][18] =   9.112828e-05;
  val[1][0][1][ 4][19] =   3.632010e-03; eval[1][0][1][ 4][19] =   9.811599e-05;
  val[1][0][1][ 4][20] =   3.625796e-03; eval[1][0][1][ 4][20] =   5.640714e-05;
  val[1][0][1][ 4][21] =   3.727999e-03; eval[1][0][1][ 4][21] =   7.541725e-05;
  val[1][0][1][ 4][22] =   3.383753e-03; eval[1][0][1][ 4][22] =   1.564940e-04;
  val[1][0][1][ 4][23] =   3.214576e-03; eval[1][0][1][ 4][23] =   1.674508e-04;
  val[1][0][1][ 4][24] =   3.520646e-03; eval[1][0][1][ 4][24] =   1.667733e-04;
  val[1][0][1][ 4][25] =   4.078740e-03; eval[1][0][1][ 4][25] =   3.214921e-04;
  val[1][0][1][ 4][26] =   3.393560e-03; eval[1][0][1][ 4][26] =   2.306763e-04;
  val[1][0][1][ 4][27] =   3.326269e-03; eval[1][0][1][ 4][27] =   3.020249e-04;
  val[1][0][1][ 4][28] =   4.389164e-03; eval[1][0][1][ 4][28] =   8.434730e-04;
  val[1][0][1][ 4][29] =   4.188256e-03; eval[1][0][1][ 4][29] =   7.077825e-04;
  //emcdphi_sigma_c0d1z5
  n_val[1][0][1][5] = 30;
  val[1][0][1][ 5][ 0] =   0.000000e+00; eval[1][0][1][ 5][ 0] =   0.000000e+00;
  val[1][0][1][ 5][ 1] =   0.000000e+00; eval[1][0][1][ 5][ 1] =   0.000000e+00;
  val[1][0][1][ 5][ 2] =   5.104264e-03; eval[1][0][1][ 5][ 2] =   2.284229e-06;
  val[1][0][1][ 5][ 3] =   4.398737e-03; eval[1][0][1][ 5][ 3] =   2.114950e-06;
  val[1][0][1][ 5][ 4] =   3.918175e-03; eval[1][0][1][ 5][ 4] =   2.617648e-06;
  val[1][0][1][ 5][ 5] =   3.845601e-03; eval[1][0][1][ 5][ 5] =   4.552611e-06;
  val[1][0][1][ 5][ 6] =   3.788964e-03; eval[1][0][1][ 5][ 6] =   3.742098e-06;
  val[1][0][1][ 5][ 7] =   3.760752e-03; eval[1][0][1][ 5][ 7] =   4.887075e-06;
  val[1][0][1][ 5][ 8] =   3.721308e-03; eval[1][0][1][ 5][ 8] =   6.186216e-06;
  val[1][0][1][ 5][ 9] =   3.714880e-03; eval[1][0][1][ 5][ 9] =   8.297854e-06;
  val[1][0][1][ 5][10] =   3.754581e-03; eval[1][0][1][ 5][10] =   1.155520e-05;
  val[1][0][1][ 5][11] =   3.771669e-03; eval[1][0][1][ 5][11] =   2.281987e-05;
  val[1][0][1][ 5][12] =   3.727422e-03; eval[1][0][1][ 5][12] =   2.935104e-05;
  val[1][0][1][ 5][13] =   3.701972e-03; eval[1][0][1][ 5][13] =   3.705266e-05;
  val[1][0][1][ 5][14] =   3.526504e-03; eval[1][0][1][ 5][14] =   4.206006e-05;
  val[1][0][1][ 5][15] =   3.562709e-03; eval[1][0][1][ 5][15] =   5.538564e-05;
  val[1][0][1][ 5][16] =   3.470864e-03; eval[1][0][1][ 5][16] =   6.523764e-05;
  val[1][0][1][ 5][17] =   3.475310e-03; eval[1][0][1][ 5][17] =   8.041197e-05;
  val[1][0][1][ 5][18] =   3.239792e-03; eval[1][0][1][ 5][18] =   8.061143e-05;
  val[1][0][1][ 5][19] =   3.300460e-03; eval[1][0][1][ 5][19] =   1.074014e-04;
  val[1][0][1][ 5][20] =   3.371569e-03; eval[1][0][1][ 5][20] =   5.923501e-05;
  val[1][0][1][ 5][21] =   3.534250e-03; eval[1][0][1][ 5][21] =   9.456434e-05;
  val[1][0][1][ 5][22] =   3.457163e-03; eval[1][0][1][ 5][22] =   1.133094e-04;
  val[1][0][1][ 5][23] =   3.379449e-03; eval[1][0][1][ 5][23] =   1.699475e-04;
  val[1][0][1][ 5][24] =   3.293528e-03; eval[1][0][1][ 5][24] =   1.470510e-04;
  val[1][0][1][ 5][25] =   3.294404e-03; eval[1][0][1][ 5][25] =   2.135873e-04;
  val[1][0][1][ 5][26] =   3.578180e-03; eval[1][0][1][ 5][26] =   3.776708e-04;
  val[1][0][1][ 5][27] =   1.723688e-03; eval[1][0][1][ 5][27] =   3.414705e-04;
  val[1][0][1][ 5][28] =   4.189192e-03; eval[1][0][1][ 5][28] =   5.702753e-04;
  val[1][0][1][ 5][29] =   2.302662e-03; eval[1][0][1][ 5][29] =   5.663490e-04;
  //emcdphi_sigma_c0d1z6
  n_val[1][0][1][6] = 30;
  val[1][0][1][ 6][ 0] =   0.000000e+00; eval[1][0][1][ 6][ 0] =   0.000000e+00;
  val[1][0][1][ 6][ 1] =   0.000000e+00; eval[1][0][1][ 6][ 1] =   0.000000e+00;
  val[1][0][1][ 6][ 2] =   5.075765e-03; eval[1][0][1][ 6][ 2] =   1.910485e-06;
  val[1][0][1][ 6][ 3] =   4.404862e-03; eval[1][0][1][ 6][ 3] =   1.742941e-06;
  val[1][0][1][ 6][ 4] =   3.924598e-03; eval[1][0][1][ 6][ 4] =   2.148305e-06;
  val[1][0][1][ 6][ 5] =   3.811074e-03; eval[1][0][1][ 6][ 5] =   3.631236e-06;
  val[1][0][1][ 6][ 6] =   3.768539e-03; eval[1][0][1][ 6][ 6] =   3.050117e-06;
  val[1][0][1][ 6][ 7] =   3.744024e-03; eval[1][0][1][ 6][ 7] =   4.005055e-06;
  val[1][0][1][ 6][ 8] =   3.715439e-03; eval[1][0][1][ 6][ 8] =   5.067457e-06;
  val[1][0][1][ 6][ 9] =   3.698283e-03; eval[1][0][1][ 6][ 9] =   6.791737e-06;
  val[1][0][1][ 6][10] =   3.789717e-03; eval[1][0][1][ 6][10] =   1.407188e-05;
  val[1][0][1][ 6][11] =   3.733821e-03; eval[1][0][1][ 6][11] =   1.812636e-05;
  val[1][0][1][ 6][12] =   3.666134e-03; eval[1][0][1][ 6][12] =   2.304057e-05;
  val[1][0][1][ 6][13] =   3.605525e-03; eval[1][0][1][ 6][13] =   2.880003e-05;
  val[1][0][1][ 6][14] =   3.516681e-03; eval[1][0][1][ 6][14] =   3.511106e-05;
  val[1][0][1][ 6][15] =   3.323378e-03; eval[1][0][1][ 6][15] =   3.795636e-05;
  val[1][0][1][ 6][16] =   3.430655e-03; eval[1][0][1][ 6][16] =   5.251650e-05;
  val[1][0][1][ 6][17] =   3.315429e-03; eval[1][0][1][ 6][17] =   6.027104e-05;
  val[1][0][1][ 6][18] =   3.416078e-03; eval[1][0][1][ 6][18] =   8.062225e-05;
  val[1][0][1][ 6][19] =   3.295030e-03; eval[1][0][1][ 6][19] =   8.882503e-05;
  val[1][0][1][ 6][20] =   3.472519e-03; eval[1][0][1][ 6][20] =   5.366751e-05;
  val[1][0][1][ 6][21] =   3.360624e-03; eval[1][0][1][ 6][21] =   6.960254e-05;
  val[1][0][1][ 6][22] =   3.357512e-03; eval[1][0][1][ 6][22] =   1.225355e-04;
  val[1][0][1][ 6][23] =   3.358557e-03; eval[1][0][1][ 6][23] =   1.683935e-04;
  val[1][0][1][ 6][24] =   3.468514e-03; eval[1][0][1][ 6][24] =   2.010307e-04;
  val[1][0][1][ 6][25] =   3.148219e-03; eval[1][0][1][ 6][25] =   1.790218e-04;
  val[1][0][1][ 6][26] =   3.477247e-03; eval[1][0][1][ 6][26] =   1.761983e-04;
  val[1][0][1][ 6][27] =   3.552519e-03; eval[1][0][1][ 6][27] =   2.481365e-04;
  val[1][0][1][ 6][28] =   3.448867e-03; eval[1][0][1][ 6][28] =   2.839961e-04;
  val[1][0][1][ 6][29] =   2.562435e-03; eval[1][0][1][ 6][29] =   2.974101e-04;
  //emcdphi_sigma_c0d1z7
  n_val[1][0][1][7] = 30;
  val[1][0][1][ 7][ 0] =   0.000000e+00; eval[1][0][1][ 7][ 0] =   0.000000e+00;
  val[1][0][1][ 7][ 1] =   0.000000e+00; eval[1][0][1][ 7][ 1] =   0.000000e+00;
  val[1][0][1][ 7][ 2] =   4.969129e-03; eval[1][0][1][ 7][ 2] =   1.794028e-06;
  val[1][0][1][ 7][ 3] =   4.351923e-03; eval[1][0][1][ 7][ 3] =   2.352808e-06;
  val[1][0][1][ 7][ 4] =   3.943132e-03; eval[1][0][1][ 7][ 4] =   2.237039e-06;
  val[1][0][1][ 7][ 5] =   3.893744e-03; eval[1][0][1][ 7][ 5] =   2.741858e-06;
  val[1][0][1][ 7][ 6] =   3.763251e-03; eval[1][0][1][ 7][ 6] =   3.166383e-06;
  val[1][0][1][ 7][ 7] =   3.725098e-03; eval[1][0][1][ 7][ 7] =   4.056082e-06;
  val[1][0][1][ 7][ 8] =   3.742625e-03; eval[1][0][1][ 7][ 8] =   7.731493e-06;
  val[1][0][1][ 7][ 9] =   3.752129e-03; eval[1][0][1][ 7][ 9] =   1.048265e-05;
  val[1][0][1][ 7][10] =   3.719692e-03; eval[1][0][1][ 7][10] =   1.366791e-05;
  val[1][0][1][ 7][11] =   3.628814e-03; eval[1][0][1][ 7][11] =   1.709507e-05;
  val[1][0][1][ 7][12] =   3.593426e-03; eval[1][0][1][ 7][12] =   2.212732e-05;
  val[1][0][1][ 7][13] =   3.513631e-03; eval[1][0][1][ 7][13] =   2.727610e-05;
  val[1][0][1][ 7][14] =   3.446560e-03; eval[1][0][1][ 7][14] =   3.314273e-05;
  val[1][0][1][ 7][15] =   3.549538e-03; eval[1][0][1][ 7][15] =   4.607452e-05;
  val[1][0][1][ 7][16] =   3.476042e-03; eval[1][0][1][ 7][16] =   5.509146e-05;
  val[1][0][1][ 7][17] =   3.323576e-03; eval[1][0][1][ 7][17] =   6.052656e-05;
  val[1][0][1][ 7][18] =   3.316370e-03; eval[1][0][1][ 7][18] =   7.341867e-05;
  val[1][0][1][ 7][19] =   3.514416e-03; eval[1][0][1][ 7][19] =   7.298231e-05;
  val[1][0][1][ 7][20] =   3.574065e-03; eval[1][0][1][ 7][20] =   4.580615e-05;
  val[1][0][1][ 7][21] =   3.337220e-03; eval[1][0][1][ 7][21] =   6.737126e-05;
  val[1][0][1][ 7][22] =   3.371767e-03; eval[1][0][1][ 7][22] =   1.222692e-04;
  val[1][0][1][ 7][23] =   3.556901e-03; eval[1][0][1][ 7][23] =   1.242559e-04;
  val[1][0][1][ 7][24] =   3.390319e-03; eval[1][0][1][ 7][24] =   1.291986e-04;
  val[1][0][1][ 7][25] =   3.589064e-03; eval[1][0][1][ 7][25] =   1.789873e-04;
  val[1][0][1][ 7][26] =   3.583525e-03; eval[1][0][1][ 7][26] =   1.945986e-04;
  val[1][0][1][ 7][27] =   3.421061e-03; eval[1][0][1][ 7][27] =   2.599608e-04;
  val[1][0][1][ 7][28] =   2.888047e-03; eval[1][0][1][ 7][28] =   2.813741e-04;
  val[1][0][1][ 7][29] =   4.510493e-03; eval[1][0][1][ 7][29] =   5.432983e-04;
  //emcdphi_sigma_c0d1z8
  n_val[1][0][1][8] = 30;
  val[1][0][1][ 8][ 0] =   0.000000e+00; eval[1][0][1][ 8][ 0] =   0.000000e+00;
  val[1][0][1][ 8][ 1] =   0.000000e+00; eval[1][0][1][ 8][ 1] =   0.000000e+00;
  val[1][0][1][ 8][ 2] =   4.756398e-03; eval[1][0][1][ 8][ 2] =   1.999643e-06;
  val[1][0][1][ 8][ 3] =   4.266971e-03; eval[1][0][1][ 8][ 3] =   2.209052e-06;
  val[1][0][1][ 8][ 4] =   4.006011e-03; eval[1][0][1][ 8][ 4] =   2.353088e-06;
  val[1][0][1][ 8][ 5] =   3.859449e-03; eval[1][0][1][ 8][ 5] =   2.644176e-06;
  val[1][0][1][ 8][ 6] =   3.787367e-03; eval[1][0][1][ 8][ 6] =   3.164752e-06;
  val[1][0][1][ 8][ 7] =   3.753077e-03; eval[1][0][1][ 8][ 7] =   6.029565e-06;
  val[1][0][1][ 8][ 8] =   3.784304e-03; eval[1][0][1][ 8][ 8] =   8.069722e-06;
  val[1][0][1][ 8][ 9] =   3.823810e-03; eval[1][0][1][ 8][ 9] =   1.112416e-05;
  val[1][0][1][ 8][10] =   3.889668e-03; eval[1][0][1][ 8][10] =   1.567378e-05;
  val[1][0][1][ 8][11] =   3.720393e-03; eval[1][0][1][ 8][11] =   1.269658e-05;
  val[1][0][1][ 8][12] =   3.680281e-03; eval[1][0][1][ 8][12] =   1.630553e-05;
  val[1][0][1][ 8][13] =   3.687664e-03; eval[1][0][1][ 8][13] =   2.151856e-05;
  val[1][0][1][ 8][14] =   3.715540e-03; eval[1][0][1][ 8][14] =   2.836059e-05;
  val[1][0][1][ 8][15] =   3.630007e-03; eval[1][0][1][ 8][15] =   3.434481e-05;
  val[1][0][1][ 8][16] =   3.623105e-03; eval[1][0][1][ 8][16] =   4.355785e-05;
  val[1][0][1][ 8][17] =   3.629962e-03; eval[1][0][1][ 8][17] =   5.379232e-05;
  val[1][0][1][ 8][18] =   3.632258e-03; eval[1][0][1][ 8][18] =   6.839885e-05;
  val[1][0][1][ 8][19] =   3.510346e-03; eval[1][0][1][ 8][19] =   7.508521e-05;
  val[1][0][1][ 8][20] =   3.481897e-03; eval[1][0][1][ 8][20] =   6.924021e-05;
  val[1][0][1][ 8][21] =   3.396577e-03; eval[1][0][1][ 8][21] =   9.281987e-05;
  val[1][0][1][ 8][22] =   3.591118e-03; eval[1][0][1][ 8][22] =   8.925765e-05;
  val[1][0][1][ 8][23] =   3.286064e-03; eval[1][0][1][ 8][23] =   1.681007e-04;
  val[1][0][1][ 8][24] =   3.696369e-03; eval[1][0][1][ 8][24] =   2.884251e-04;
  val[1][0][1][ 8][25] =   3.438727e-03; eval[1][0][1][ 8][25] =   1.810050e-04;
  val[1][0][1][ 8][26] =   2.900706e-03; eval[1][0][1][ 8][26] =   3.262407e-04;
  val[1][0][1][ 8][27] =   3.261850e-03; eval[1][0][1][ 8][27] =   4.168808e-04;
  val[1][0][1][ 8][28] =   3.552733e-03; eval[1][0][1][ 8][28] =   3.109164e-04;
  val[1][0][1][ 8][29] =   4.395144e-03; eval[1][0][1][ 8][29] =   5.128540e-04;
  //emcdphi_sigma_c0d1z9
  n_val[1][0][1][9] = 30;
  val[1][0][1][ 9][ 0] =   0.000000e+00; eval[1][0][1][ 9][ 0] =   0.000000e+00;
  val[1][0][1][ 9][ 1] =   0.000000e+00; eval[1][0][1][ 9][ 1] =   0.000000e+00;
  val[1][0][1][ 9][ 2] =   4.722885e-03; eval[1][0][1][ 9][ 2] =   2.329010e-06;
  val[1][0][1][ 9][ 3] =   4.320076e-03; eval[1][0][1][ 9][ 3] =   1.950182e-06;
  val[1][0][1][ 9][ 4] =   3.999534e-03; eval[1][0][1][ 9][ 4] =   2.565830e-06;
  val[1][0][1][ 9][ 5] =   3.877724e-03; eval[1][0][1][ 9][ 5] =   3.061228e-06;
  val[1][0][1][ 9][ 6] =   3.796741e-03; eval[1][0][1][ 9][ 6] =   3.709809e-06;
  val[1][0][1][ 9][ 7] =   3.737527e-03; eval[1][0][1][ 9][ 7] =   6.716226e-06;
  val[1][0][1][ 9][ 8] =   3.723703e-03; eval[1][0][1][ 9][ 8] =   8.735047e-06;
  val[1][0][1][ 9][ 9] =   3.763155e-03; eval[1][0][1][ 9][ 9] =   1.216694e-05;
  val[1][0][1][ 9][10] =   3.769029e-03; eval[1][0][1][ 9][10] =   1.131305e-05;
  val[1][0][1][ 9][11] =   3.759475e-03; eval[1][0][1][ 9][11] =   1.505606e-05;
  val[1][0][1][ 9][12] =   3.702871e-03; eval[1][0][1][ 9][12] =   1.889423e-05;
  val[1][0][1][ 9][13] =   3.661731e-03; eval[1][0][1][ 9][13] =   2.400329e-05;
  val[1][0][1][ 9][14] =   3.690102e-03; eval[1][0][1][ 9][14] =   3.105665e-05;
  val[1][0][1][ 9][15] =   3.595444e-03; eval[1][0][1][ 9][15] =   3.739345e-05;
  val[1][0][1][ 9][16] =   3.665279e-03; eval[1][0][1][ 9][16] =   4.993481e-05;
  val[1][0][1][ 9][17] =   3.613661e-03; eval[1][0][1][ 9][17] =   5.893771e-05;
  val[1][0][1][ 9][18] =   3.549568e-03; eval[1][0][1][ 9][18] =   6.931228e-05;
  val[1][0][1][ 9][19] =   3.582736e-03; eval[1][0][1][ 9][19] =   8.612926e-05;
  val[1][0][1][ 9][20] =   3.613264e-03; eval[1][0][1][ 9][20] =   5.223449e-05;
  val[1][0][1][ 9][21] =   3.634673e-03; eval[1][0][1][ 9][21] =   7.642997e-05;
  val[1][0][1][ 9][22] =   3.246683e-03; eval[1][0][1][ 9][22] =   1.203535e-04;
  val[1][0][1][ 9][23] =   4.033343e-03; eval[1][0][1][ 9][23] =   1.188139e-04;
  val[1][0][1][ 9][24] =   3.911376e-03; eval[1][0][1][ 9][24] =   3.737790e-04;
  val[1][0][1][ 9][25] =   3.379368e-03; eval[1][0][1][ 9][25] =   2.287245e-04;
  val[1][0][1][ 9][26] =   3.215207e-03; eval[1][0][1][ 9][26] =   2.298222e-04;
  val[1][0][1][ 9][27] =   4.100301e-03; eval[1][0][1][ 9][27] =   3.170575e-04;
  val[1][0][1][ 9][28] =   1.613041e-02; eval[1][0][1][ 9][28] =   8.140027e-03;
  val[1][0][1][ 9][29] =   2.341246e-03; eval[1][0][1][ 9][29] =   3.902009e-04;
  //emcdphi_sigma_c1d0z0
  n_val[1][1][0][0] = 30;
  val[1][1][0][ 0][ 0] =   0.000000e+00; eval[1][1][0][ 0][ 0] =   0.000000e+00;
  val[1][1][0][ 0][ 1] =   0.000000e+00; eval[1][1][0][ 0][ 1] =   0.000000e+00;
  val[1][1][0][ 0][ 2] =   5.907672e-03; eval[1][1][0][ 0][ 2] =   2.137759e-06;
  val[1][1][0][ 0][ 3] =   5.451550e-03; eval[1][1][0][ 0][ 3] =   2.433445e-06;
  val[1][1][0][ 0][ 4] =   5.249783e-03; eval[1][1][0][ 0][ 4] =   2.860733e-06;
  val[1][1][0][ 0][ 5] =   5.112331e-03; eval[1][1][0][ 0][ 5] =   3.554070e-06;
  val[1][1][0][ 0][ 6] =   5.036263e-03; eval[1][1][0][ 0][ 6] =   4.417581e-06;
  val[1][1][0][ 0][ 7] =   4.980445e-03; eval[1][1][0][ 0][ 7] =   7.367594e-06;
  val[1][1][0][ 0][ 8] =   4.974986e-03; eval[1][1][0][ 0][ 8] =   9.525834e-06;
  val[1][1][0][ 0][ 9] =   4.974285e-03; eval[1][1][0][ 0][ 9] =   1.260084e-05;
  val[1][1][0][ 0][10] =   4.966875e-03; eval[1][1][0][ 0][10] =   1.667916e-05;
  val[1][1][0][ 0][11] =   4.940419e-03; eval[1][1][0][ 0][11] =   2.167607e-05;
  val[1][1][0][ 0][12] =   4.903580e-03; eval[1][1][0][ 0][12] =   2.774756e-05;
  val[1][1][0][ 0][13] =   4.817705e-03; eval[1][1][0][ 0][13] =   3.486687e-05;
  val[1][1][0][ 0][14] =   4.890086e-03; eval[1][1][0][ 0][14] =   4.715679e-05;
  val[1][1][0][ 0][15] =   4.893452e-03; eval[1][1][0][ 0][15] =   6.003661e-05;
  val[1][1][0][ 0][16] =   4.708495e-03; eval[1][1][0][ 0][16] =   5.282490e-05;
  val[1][1][0][ 0][17] =   4.884998e-03; eval[1][1][0][ 0][17] =   9.531890e-05;
  val[1][1][0][ 0][18] =   4.684681e-03; eval[1][1][0][ 0][18] =   1.023866e-04;
  val[1][1][0][ 0][19] =   4.733760e-03; eval[1][1][0][ 0][19] =   1.347994e-04;
  val[1][1][0][ 0][20] =   4.544905e-03; eval[1][1][0][ 0][20] =   6.239599e-05;
  val[1][1][0][ 0][21] =   4.570807e-03; eval[1][1][0][ 0][21] =   9.109141e-05;
  val[1][1][0][ 0][22] =   4.678213e-03; eval[1][1][0][ 0][22] =   1.083897e-04;
  val[1][1][0][ 0][23] =   4.432366e-03; eval[1][1][0][ 0][23] =   2.200467e-04;
  val[1][1][0][ 0][24] =   4.635857e-03; eval[1][1][0][ 0][24] =   1.948965e-04;
  val[1][1][0][ 0][25] =   4.832908e-03; eval[1][1][0][ 0][25] =   3.147144e-04;
  val[1][1][0][ 0][26] =   4.311457e-03; eval[1][1][0][ 0][26] =   3.095888e-04;
  val[1][1][0][ 0][27] =   5.066911e-03; eval[1][1][0][ 0][27] =   3.741653e-04;
  val[1][1][0][ 0][28] =   3.513316e-03; eval[1][1][0][ 0][28] =   7.610204e-04;
  val[1][1][0][ 0][29] =   2.500418e-01; eval[1][1][0][ 0][29] =   1.644487e+00;
  //emcdphi_sigma_c1d0z1
  n_val[1][1][0][1] = 30;
  val[1][1][0][ 1][ 0] =   0.000000e+00; eval[1][1][0][ 1][ 0] =   0.000000e+00;
  val[1][1][0][ 1][ 1] =   0.000000e+00; eval[1][1][0][ 1][ 1] =   0.000000e+00;
  val[1][1][0][ 1][ 2] =   5.977971e-03; eval[1][1][0][ 1][ 2] =   2.167978e-06;
  val[1][1][0][ 1][ 3] =   5.468339e-03; eval[1][1][0][ 1][ 3] =   2.431874e-06;
  val[1][1][0][ 1][ 4] =   5.178018e-03; eval[1][1][0][ 1][ 4] =   2.809682e-06;
  val[1][1][0][ 1][ 5] =   5.033663e-03; eval[1][1][0][ 1][ 5] =   3.395351e-06;
  val[1][1][0][ 1][ 6] =   4.970533e-03; eval[1][1][0][ 1][ 6] =   4.238294e-06;
  val[1][1][0][ 1][ 7] =   4.895678e-03; eval[1][1][0][ 1][ 7] =   6.994148e-06;
  val[1][1][0][ 1][ 8] =   4.879980e-03; eval[1][1][0][ 1][ 8] =   9.011740e-06;
  val[1][1][0][ 1][ 9] =   4.879913e-03; eval[1][1][0][ 1][ 9] =   1.201206e-05;
  val[1][1][0][ 1][10] =   4.874942e-03; eval[1][1][0][ 1][10] =   1.593479e-05;
  val[1][1][0][ 1][11] =   4.776101e-03; eval[1][1][0][ 1][11] =   1.991818e-05;
  val[1][1][0][ 1][12] =   4.731924e-03; eval[1][1][0][ 1][12] =   2.558317e-05;
  val[1][1][0][ 1][13] =   4.730747e-03; eval[1][1][0][ 1][13] =   3.375715e-05;
  val[1][1][0][ 1][14] =   4.768424e-03; eval[1][1][0][ 1][14] =   4.425185e-05;
  val[1][1][0][ 1][15] =   4.758208e-03; eval[1][1][0][ 1][15] =   5.559689e-05;
  val[1][1][0][ 1][16] =   4.735618e-03; eval[1][1][0][ 1][16] =   6.857309e-05;
  val[1][1][0][ 1][17] =   4.483387e-03; eval[1][1][0][ 1][17] =   7.522084e-05;
  val[1][1][0][ 1][18] =   4.519319e-03; eval[1][1][0][ 1][18] =   9.719238e-05;
  val[1][1][0][ 1][19] =   4.543227e-03; eval[1][1][0][ 1][19] =   1.160479e-04;
  val[1][1][0][ 1][20] =   4.333193e-03; eval[1][1][0][ 1][20] =   7.895498e-05;
  val[1][1][0][ 1][21] =   4.063898e-03; eval[1][1][0][ 1][21] =   9.451049e-05;
  val[1][1][0][ 1][22] =   4.288898e-03; eval[1][1][0][ 1][22] =   1.477862e-04;
  val[1][1][0][ 1][23] =   4.420535e-03; eval[1][1][0][ 1][23] =   1.523823e-04;
  val[1][1][0][ 1][24] =   3.665189e-03; eval[1][1][0][ 1][24] =   1.975026e-04;
  val[1][1][0][ 1][25] =   4.318735e-03; eval[1][1][0][ 1][25] =   2.450456e-04;
  val[1][1][0][ 1][26] =   4.171953e-03; eval[1][1][0][ 1][26] =   2.767897e-04;
  val[1][1][0][ 1][27] =   3.981487e-03; eval[1][1][0][ 1][27] =   3.527761e-04;
  val[1][1][0][ 1][28] =   4.074148e-03; eval[1][1][0][ 1][28] =   4.816173e-04;
  val[1][1][0][ 1][29] =   3.358132e-03; eval[1][1][0][ 1][29] =   5.420183e-04;
  //emcdphi_sigma_c1d0z2
  n_val[1][1][0][2] = 30;
  val[1][1][0][ 2][ 0] =   0.000000e+00; eval[1][1][0][ 2][ 0] =   0.000000e+00;
  val[1][1][0][ 2][ 1] =   0.000000e+00; eval[1][1][0][ 2][ 1] =   0.000000e+00;
  val[1][1][0][ 2][ 2] =   5.974659e-03; eval[1][1][0][ 2][ 2] =   2.094484e-06;
  val[1][1][0][ 2][ 3] =   5.474715e-03; eval[1][1][0][ 2][ 3] =   1.937109e-06;
  val[1][1][0][ 2][ 4] =   5.244353e-03; eval[1][1][0][ 2][ 4] =   2.868193e-06;
  val[1][1][0][ 2][ 5] =   5.014719e-03; eval[1][1][0][ 2][ 5] =   3.285197e-06;
  val[1][1][0][ 2][ 6] =   4.973227e-03; eval[1][1][0][ 2][ 6] =   4.165746e-06;
  val[1][1][0][ 2][ 7] =   4.929931e-03; eval[1][1][0][ 2][ 7] =   5.479646e-06;
  val[1][1][0][ 2][ 8] =   4.865761e-03; eval[1][1][0][ 2][ 8] =   6.823594e-06;
  val[1][1][0][ 2][ 9] =   5.006490e-03; eval[1][1][0][ 2][ 9] =   1.288313e-05;
  val[1][1][0][ 2][10] =   4.854262e-03; eval[1][1][0][ 2][10] =   1.213133e-05;
  val[1][1][0][ 2][11] =   4.888626e-03; eval[1][1][0][ 2][11] =   2.121006e-05;
  val[1][1][0][ 2][12] =   4.799192e-03; eval[1][1][0][ 2][12] =   2.682132e-05;
  val[1][1][0][ 2][13] =   4.816387e-03; eval[1][1][0][ 2][13] =   3.549098e-05;
  val[1][1][0][ 2][14] =   4.752274e-03; eval[1][1][0][ 2][14] =   4.462618e-05;
  val[1][1][0][ 2][15] =   4.797325e-03; eval[1][1][0][ 2][15] =   5.753015e-05;
  val[1][1][0][ 2][16] =   4.668641e-03; eval[1][1][0][ 2][16] =   6.729482e-05;
  val[1][1][0][ 2][17] =   4.605634e-03; eval[1][1][0][ 2][17] =   8.214276e-05;
  val[1][1][0][ 2][18] =   4.556287e-03; eval[1][1][0][ 2][18] =   1.005873e-04;
  val[1][1][0][ 2][19] =   4.479881e-03; eval[1][1][0][ 2][19] =   1.202448e-04;
  val[1][1][0][ 2][20] =   4.503603e-03; eval[1][1][0][ 2][20] =   7.265729e-05;
  val[1][1][0][ 2][21] =   4.368822e-03; eval[1][1][0][ 2][21] =   9.852849e-05;
  val[1][1][0][ 2][22] =   4.262977e-03; eval[1][1][0][ 2][22] =   1.237273e-04;
  val[1][1][0][ 2][23] =   3.856210e-03; eval[1][1][0][ 2][23] =   1.966398e-04;
  val[1][1][0][ 2][24] =   4.576938e-03; eval[1][1][0][ 2][24] =   2.201325e-04;
  val[1][1][0][ 2][25] =   4.385925e-03; eval[1][1][0][ 2][25] =   2.860021e-04;
  val[1][1][0][ 2][26] =   3.758294e-03; eval[1][1][0][ 2][26] =   3.767505e-04;
  val[1][1][0][ 2][27] =   3.860707e-03; eval[1][1][0][ 2][27] =   6.246069e-04;
  val[1][1][0][ 2][28] =   4.241355e-03; eval[1][1][0][ 2][28] =   9.456486e-04;
  val[1][1][0][ 2][29] =   3.485984e-03; eval[1][1][0][ 2][29] =   3.692848e-04;
  //emcdphi_sigma_c1d0z3
  n_val[1][1][0][3] = 30;
  val[1][1][0][ 3][ 0] =   0.000000e+00; eval[1][1][0][ 3][ 0] =   0.000000e+00;
  val[1][1][0][ 3][ 1] =   0.000000e+00; eval[1][1][0][ 3][ 1] =   0.000000e+00;
  val[1][1][0][ 3][ 2] =   5.962061e-03; eval[1][1][0][ 3][ 2] =   2.091674e-06;
  val[1][1][0][ 3][ 3] =   5.369866e-03; eval[1][1][0][ 3][ 3] =   2.353011e-06;
  val[1][1][0][ 3][ 4] =   5.110894e-03; eval[1][1][0][ 3][ 4] =   3.627233e-06;
  val[1][1][0][ 3][ 5] =   4.849895e-03; eval[1][1][0][ 3][ 5] =   4.012710e-06;
  val[1][1][0][ 3][ 6] =   4.872196e-03; eval[1][1][0][ 3][ 6] =   4.046343e-06;
  val[1][1][0][ 3][ 7] =   4.767626e-03; eval[1][1][0][ 3][ 7] =   5.166769e-06;
  val[1][1][0][ 3][ 8] =   4.798349e-03; eval[1][1][0][ 3][ 8] =   8.750063e-06;
  val[1][1][0][ 3][ 9] =   4.797796e-03; eval[1][1][0][ 3][ 9] =   1.184078e-05;
  val[1][1][0][ 3][10] =   4.782636e-03; eval[1][1][0][ 3][10] =   1.552988e-05;
  val[1][1][0][ 3][11] =   4.719554e-03; eval[1][1][0][ 3][11] =   1.969927e-05;
  val[1][1][0][ 3][12] =   4.703018e-03; eval[1][1][0][ 3][12] =   2.558996e-05;
  val[1][1][0][ 3][13] =   4.672843e-03; eval[1][1][0][ 3][13] =   3.296936e-05;
  val[1][1][0][ 3][14] =   4.656799e-03; eval[1][1][0][ 3][14] =   4.188761e-05;
  val[1][1][0][ 3][15] =   4.638325e-03; eval[1][1][0][ 3][15] =   5.293360e-05;
  val[1][1][0][ 3][16] =   4.657354e-03; eval[1][1][0][ 3][16] =   6.687118e-05;
  val[1][1][0][ 3][17] =   4.601964e-03; eval[1][1][0][ 3][17] =   7.987958e-05;
  val[1][1][0][ 3][18] =   4.593592e-03; eval[1][1][0][ 3][18] =   9.847887e-05;
  val[1][1][0][ 3][19] =   4.396943e-03; eval[1][1][0][ 3][19] =   8.594821e-05;
  val[1][1][0][ 3][20] =   4.195167e-03; eval[1][1][0][ 3][20] =   7.494933e-05;
  val[1][1][0][ 3][21] =   4.292463e-03; eval[1][1][0][ 3][21] =   8.379261e-05;
  val[1][1][0][ 3][22] =   4.368856e-03; eval[1][1][0][ 3][22] =   1.292771e-04;
  val[1][1][0][ 3][23] =   4.405998e-03; eval[1][1][0][ 3][23] =   1.743538e-04;
  val[1][1][0][ 3][24] =   3.992917e-03; eval[1][1][0][ 3][24] =   1.590950e-04;
  val[1][1][0][ 3][25] =   3.922375e-03; eval[1][1][0][ 3][25] =   4.676350e-04;
  val[1][1][0][ 3][26] =   4.561504e-03; eval[1][1][0][ 3][26] =   3.707344e-04;
  val[1][1][0][ 3][27] =   4.312034e-03; eval[1][1][0][ 3][27] =   3.385278e-04;
  val[1][1][0][ 3][28] =   3.616875e-03; eval[1][1][0][ 3][28] =   4.380907e-04;
  val[1][1][0][ 3][29] =   4.907418e-03; eval[1][1][0][ 3][29] =   7.776078e-04;
  //emcdphi_sigma_c1d0z4
  n_val[1][1][0][4] = 30;
  val[1][1][0][ 4][ 0] =   0.000000e+00; eval[1][1][0][ 4][ 0] =   0.000000e+00;
  val[1][1][0][ 4][ 1] =   0.000000e+00; eval[1][1][0][ 4][ 1] =   0.000000e+00;
  val[1][1][0][ 4][ 2] =   5.892778e-03; eval[1][1][0][ 4][ 2] =   2.260146e-06;
  val[1][1][0][ 4][ 3] =   5.295047e-03; eval[1][1][0][ 4][ 3] =   2.536532e-06;
  val[1][1][0][ 4][ 4] =   5.055197e-03; eval[1][1][0][ 4][ 4] =   3.990476e-06;
  val[1][1][0][ 4][ 5] =   4.855708e-03; eval[1][1][0][ 4][ 5] =   4.559627e-06;
  val[1][1][0][ 4][ 6] =   4.890309e-03; eval[1][1][0][ 4][ 6] =   4.634911e-06;
  val[1][1][0][ 4][ 7] =   4.814438e-03; eval[1][1][0][ 4][ 7] =   5.981006e-06;
  val[1][1][0][ 4][ 8] =   4.880656e-03; eval[1][1][0][ 4][ 8] =   1.036433e-05;
  val[1][1][0][ 4][ 9] =   4.890379e-03; eval[1][1][0][ 4][ 9] =   1.391528e-05;
  val[1][1][0][ 4][10] =   4.684719e-03; eval[1][1][0][ 4][10] =   1.273697e-05;
  val[1][1][0][ 4][11] =   4.782716e-03; eval[1][1][0][ 4][11] =   2.288519e-05;
  val[1][1][0][ 4][12] =   4.762442e-03; eval[1][1][0][ 4][12] =   3.002272e-05;
  val[1][1][0][ 4][13] =   4.785377e-03; eval[1][1][0][ 4][13] =   3.975962e-05;
  val[1][1][0][ 4][14] =   4.764640e-03; eval[1][1][0][ 4][14] =   5.088711e-05;
  val[1][1][0][ 4][15] =   4.760580e-03; eval[1][1][0][ 4][15] =   6.418343e-05;
  val[1][1][0][ 4][16] =   4.826031e-03; eval[1][1][0][ 4][16] =   8.576551e-05;
  val[1][1][0][ 4][17] =   4.677446e-03; eval[1][1][0][ 4][17] =   9.655563e-05;
  val[1][1][0][ 4][18] =   4.268396e-03; eval[1][1][0][ 4][18] =   9.763486e-05;
  val[1][1][0][ 4][19] =   4.527391e-03; eval[1][1][0][ 4][19] =   1.054213e-04;
  val[1][1][0][ 4][20] =   4.425735e-03; eval[1][1][0][ 4][20] =   6.256188e-05;
  val[1][1][0][ 4][21] =   4.252157e-03; eval[1][1][0][ 4][21] =   1.002664e-04;
  val[1][1][0][ 4][22] =   4.070352e-03; eval[1][1][0][ 4][22] =   1.585866e-04;
  val[1][1][0][ 4][23] =   4.047168e-03; eval[1][1][0][ 4][23] =   1.660489e-04;
  val[1][1][0][ 4][24] =   4.003268e-03; eval[1][1][0][ 4][24] =   3.223225e-04;
  val[1][1][0][ 4][25] =   4.180195e-03; eval[1][1][0][ 4][25] =   2.374871e-04;
  val[1][1][0][ 4][26] =   4.166797e-03; eval[1][1][0][ 4][26] =   3.313384e-04;
  val[1][1][0][ 4][27] =   4.459147e-03; eval[1][1][0][ 4][27] =   4.426054e-04;
  val[1][1][0][ 4][28] =   4.392618e-03; eval[1][1][0][ 4][28] =   9.893207e-04;
  val[1][1][0][ 4][29] =   1.658286e-02; eval[1][1][0][ 4][29] =   1.257405e-02;
  //emcdphi_sigma_c1d0z5
  n_val[1][1][0][5] = 30;
  val[1][1][0][ 5][ 0] =   0.000000e+00; eval[1][1][0][ 5][ 0] =   0.000000e+00;
  val[1][1][0][ 5][ 1] =   0.000000e+00; eval[1][1][0][ 5][ 1] =   0.000000e+00;
  val[1][1][0][ 5][ 2] =   5.880205e-03; eval[1][1][0][ 5][ 2] =   2.381822e-06;
  val[1][1][0][ 5][ 3] =   5.254663e-03; eval[1][1][0][ 5][ 3] =   2.650051e-06;
  val[1][1][0][ 5][ 4] =   5.035276e-03; eval[1][1][0][ 5][ 4] =   4.270568e-06;
  val[1][1][0][ 5][ 5] =   4.947914e-03; eval[1][1][0][ 5][ 5] =   5.388988e-06;
  val[1][1][0][ 5][ 6] =   4.983929e-03; eval[1][1][0][ 5][ 6] =   5.497286e-06;
  val[1][1][0][ 5][ 7] =   4.859665e-03; eval[1][1][0][ 5][ 7] =   6.855550e-06;
  val[1][1][0][ 5][ 8] =   4.757736e-03; eval[1][1][0][ 5][ 8] =   8.284131e-06;
  val[1][1][0][ 5][ 9] =   4.745651e-03; eval[1][1][0][ 5][ 9] =   1.084300e-05;
  val[1][1][0][ 5][10] =   4.711867e-03; eval[1][1][0][ 5][10] =   1.410833e-05;
  val[1][1][0][ 5][11] =   4.671272e-03; eval[1][1][0][ 5][11] =   3.257951e-05;
  val[1][1][0][ 5][12] =   4.616153e-03; eval[1][1][0][ 5][12] =   2.361768e-05;
  val[1][1][0][ 5][13] =   4.583199e-03; eval[1][1][0][ 5][13] =   3.019023e-05;
  val[1][1][0][ 5][14] =   4.707667e-03; eval[1][1][0][ 5][14] =   5.476786e-05;
  val[1][1][0][ 5][15] =   4.383561e-03; eval[1][1][0][ 5][15] =   7.724281e-05;
  val[1][1][0][ 5][16] =   4.311291e-03; eval[1][1][0][ 5][16] =   9.368514e-05;
  val[1][1][0][ 5][17] =   4.530965e-03; eval[1][1][0][ 5][17] =   9.818244e-05;
  val[1][1][0][ 5][18] =   4.405183e-03; eval[1][1][0][ 5][18] =   1.553972e-04;
  val[1][1][0][ 5][19] =   4.246229e-03; eval[1][1][0][ 5][19] =   1.681696e-04;
  val[1][1][0][ 5][20] =   4.142067e-03; eval[1][1][0][ 5][20] =   9.248203e-05;
  val[1][1][0][ 5][21] =   4.456519e-03; eval[1][1][0][ 5][21] =   1.060150e-04;
  val[1][1][0][ 5][22] =   4.194575e-03; eval[1][1][0][ 5][22] =   1.110799e-04;
  val[1][1][0][ 5][23] =   4.579676e-03; eval[1][1][0][ 5][23] =   2.065201e-04;
  val[1][1][0][ 5][24] =   4.633808e-03; eval[1][1][0][ 5][24] =   2.826360e-04;
  val[1][1][0][ 5][25] =   3.933332e-03; eval[1][1][0][ 5][25] =   3.363598e-04;
  val[1][1][0][ 5][26] =   4.507867e-03; eval[1][1][0][ 5][26] =   6.726758e-04;
  val[1][1][0][ 5][27] =   3.423556e-03; eval[1][1][0][ 5][27] =   3.906194e-04;
  val[1][1][0][ 5][28] =   6.771094e-03; eval[1][1][0][ 5][28] =   1.038258e-03;
  val[1][1][0][ 5][29] =   5.584716e-03; eval[1][1][0][ 5][29] =   1.299095e-03;
  //emcdphi_sigma_c1d0z6
  n_val[1][1][0][6] = 30;
  val[1][1][0][ 6][ 0] =   0.000000e+00; eval[1][1][0][ 6][ 0] =   0.000000e+00;
  val[1][1][0][ 6][ 1] =   0.000000e+00; eval[1][1][0][ 6][ 1] =   0.000000e+00;
  val[1][1][0][ 6][ 2] =   5.941855e-03; eval[1][1][0][ 6][ 2] =   2.023317e-06;
  val[1][1][0][ 6][ 3] =   5.306482e-03; eval[1][1][0][ 6][ 3] =   2.230388e-06;
  val[1][1][0][ 6][ 4] =   5.195642e-03; eval[1][1][0][ 6][ 4] =   2.979534e-06;
  val[1][1][0][ 6][ 5] =   5.047070e-03; eval[1][1][0][ 6][ 5] =   3.631052e-06;
  val[1][1][0][ 6][ 6] =   4.983457e-03; eval[1][1][0][ 6][ 6] =   4.514032e-06;
  val[1][1][0][ 6][ 7] =   4.853154e-03; eval[1][1][0][ 6][ 7] =   5.612794e-06;
  val[1][1][0][ 6][ 8] =   4.887012e-03; eval[1][1][0][ 6][ 8] =   9.189547e-06;
  val[1][1][0][ 6][ 9] =   4.862090e-03; eval[1][1][0][ 6][ 9] =   1.231371e-05;
  val[1][1][0][ 6][10] =   4.633547e-03; eval[1][1][0][ 6][10] =   1.115370e-05;
  val[1][1][0][ 6][11] =   4.669696e-03; eval[1][1][0][ 6][11] =   1.916057e-05;
  val[1][1][0][ 6][12] =   4.726496e-03; eval[1][1][0][ 6][12] =   2.612776e-05;
  val[1][1][0][ 6][13] =   4.787748e-03; eval[1][1][0][ 6][13] =   3.554305e-05;
  val[1][1][0][ 6][14] =   4.830759e-03; eval[1][1][0][ 6][14] =   4.734328e-05;
  val[1][1][0][ 6][15] =   4.861945e-03; eval[1][1][0][ 6][15] =   6.093901e-05;
  val[1][1][0][ 6][16] =   4.668589e-03; eval[1][1][0][ 6][16] =   6.917189e-05;
  val[1][1][0][ 6][17] =   4.561915e-03; eval[1][1][0][ 6][17] =   8.124833e-05;
  val[1][1][0][ 6][18] =   4.508437e-03; eval[1][1][0][ 6][18] =   9.832241e-05;
  val[1][1][0][ 6][19] =   3.936292e-03; eval[1][1][0][ 6][19] =   9.157596e-05;
  val[1][1][0][ 6][20] =   4.241932e-03; eval[1][1][0][ 6][20] =   6.509168e-05;
  val[1][1][0][ 6][21] =   4.290069e-03; eval[1][1][0][ 6][21] =   1.193051e-04;
  val[1][1][0][ 6][22] =   4.228084e-03; eval[1][1][0][ 6][22] =   1.136191e-04;
  val[1][1][0][ 6][23] =   3.984131e-03; eval[1][1][0][ 6][23] =   1.683593e-04;
  val[1][1][0][ 6][24] =   3.988096e-03; eval[1][1][0][ 6][24] =   1.472072e-04;
  val[1][1][0][ 6][25] =   4.511970e-03; eval[1][1][0][ 6][25] =   2.557173e-04;
  val[1][1][0][ 6][26] =   4.849083e-03; eval[1][1][0][ 6][26] =   3.207148e-04;
  val[1][1][0][ 6][27] =   4.467442e-03; eval[1][1][0][ 6][27] =   4.011274e-04;
  val[1][1][0][ 6][28] =   2.928116e-02; eval[1][1][0][ 6][28] =   2.886361e-02;
  val[1][1][0][ 6][29] =   3.548775e-03; eval[1][1][0][ 6][29] =   4.073303e-04;
  //emcdphi_sigma_c1d0z7
  n_val[1][1][0][7] = 30;
  val[1][1][0][ 7][ 0] =   0.000000e+00; eval[1][1][0][ 7][ 0] =   0.000000e+00;
  val[1][1][0][ 7][ 1] =   0.000000e+00; eval[1][1][0][ 7][ 1] =   0.000000e+00;
  val[1][1][0][ 7][ 2] =   5.888168e-03; eval[1][1][0][ 7][ 2] =   1.899033e-06;
  val[1][1][0][ 7][ 3] =   5.314629e-03; eval[1][1][0][ 7][ 3] =   2.165705e-06;
  val[1][1][0][ 7][ 4] =   5.091165e-03; eval[1][1][0][ 7][ 4] =   2.686125e-06;
  val[1][1][0][ 7][ 5] =   5.006890e-03; eval[1][1][0][ 7][ 5] =   3.539816e-06;
  val[1][1][0][ 7][ 6] =   4.972137e-03; eval[1][1][0][ 7][ 6] =   4.498048e-06;
  val[1][1][0][ 7][ 7] =   4.827249e-03; eval[1][1][0][ 7][ 7] =   5.451032e-06;
  val[1][1][0][ 7][ 8] =   4.855374e-03; eval[1][1][0][ 7][ 8] =   9.059807e-06;
  val[1][1][0][ 7][ 9] =   4.864264e-03; eval[1][1][0][ 7][ 9] =   1.198926e-05;
  val[1][1][0][ 7][10] =   4.626399e-03; eval[1][1][0][ 7][10] =   1.080554e-05;
  val[1][1][0][ 7][11] =   4.707956e-03; eval[1][1][0][ 7][11] =   1.905622e-05;
  val[1][1][0][ 7][12] =   4.715549e-03; eval[1][1][0][ 7][12] =   2.518462e-05;
  val[1][1][0][ 7][13] =   4.801793e-03; eval[1][1][0][ 7][13] =   3.483491e-05;
  val[1][1][0][ 7][14] =   4.743018e-03; eval[1][1][0][ 7][14] =   4.409761e-05;
  val[1][1][0][ 7][15] =   4.693859e-03; eval[1][1][0][ 7][15] =   5.414596e-05;
  val[1][1][0][ 7][16] =   4.667174e-03; eval[1][1][0][ 7][16] =   6.702027e-05;
  val[1][1][0][ 7][17] =   4.614066e-03; eval[1][1][0][ 7][17] =   8.168774e-05;
  val[1][1][0][ 7][18] =   4.606491e-03; eval[1][1][0][ 7][18] =   9.977592e-05;
  val[1][1][0][ 7][19] =   4.336988e-03; eval[1][1][0][ 7][19] =   8.366458e-05;
  val[1][1][0][ 7][20] =   4.389342e-03; eval[1][1][0][ 7][20] =   5.747368e-05;
  val[1][1][0][ 7][21] =   4.171039e-03; eval[1][1][0][ 7][21] =   7.588439e-05;
  val[1][1][0][ 7][22] =   4.317657e-03; eval[1][1][0][ 7][22] =   1.114307e-04;
  val[1][1][0][ 7][23] =   4.368043e-03; eval[1][1][0][ 7][23] =   1.725133e-04;
  val[1][1][0][ 7][24] =   3.959649e-03; eval[1][1][0][ 7][24] =   2.398802e-04;
  val[1][1][0][ 7][25] =   4.483758e-03; eval[1][1][0][ 7][25] =   3.962012e-04;
  val[1][1][0][ 7][26] =   3.863781e-03; eval[1][1][0][ 7][26] =   3.430513e-04;
  val[1][1][0][ 7][27] =   4.251182e-03; eval[1][1][0][ 7][27] =   4.863318e-04;
  val[1][1][0][ 7][28] =   4.655624e-03; eval[1][1][0][ 7][28] =   5.523793e-04;
  val[1][1][0][ 7][29] =   4.038959e-03; eval[1][1][0][ 7][29] =   5.222734e-04;
  //emcdphi_sigma_c1d0z8
  n_val[1][1][0][8] = 30;
  val[1][1][0][ 8][ 0] =   0.000000e+00; eval[1][1][0][ 8][ 0] =   0.000000e+00;
  val[1][1][0][ 8][ 1] =   0.000000e+00; eval[1][1][0][ 8][ 1] =   0.000000e+00;
  val[1][1][0][ 8][ 2] =   5.876072e-03; eval[1][1][0][ 8][ 2] =   1.872240e-06;
  val[1][1][0][ 8][ 3] =   5.247375e-03; eval[1][1][0][ 8][ 3] =   2.077434e-06;
  val[1][1][0][ 8][ 4] =   5.046781e-03; eval[1][1][0][ 8][ 4] =   2.570961e-06;
  val[1][1][0][ 8][ 5] =   4.947418e-03; eval[1][1][0][ 8][ 5] =   3.402231e-06;
  val[1][1][0][ 8][ 6] =   4.928902e-03; eval[1][1][0][ 8][ 6] =   4.432771e-06;
  val[1][1][0][ 8][ 7] =   4.812785e-03; eval[1][1][0][ 8][ 7] =   5.413978e-06;
  val[1][1][0][ 8][ 8] =   4.741236e-03; eval[1][1][0][ 8][ 8] =   6.599765e-06;
  val[1][1][0][ 8][ 9] =   4.832399e-03; eval[1][1][0][ 8][ 9] =   1.174601e-05;
  val[1][1][0][ 8][10] =   4.744581e-03; eval[1][1][0][ 8][10] =   1.478879e-05;
  val[1][1][0][ 8][11] =   4.618317e-03; eval[1][1][0][ 8][11] =   1.799032e-05;
  val[1][1][0][ 8][12] =   4.633632e-03; eval[1][1][0][ 8][12] =   2.387778e-05;
  val[1][1][0][ 8][13] =   4.606870e-03; eval[1][1][0][ 8][13] =   3.063946e-05;
  val[1][1][0][ 8][14] =   4.534010e-03; eval[1][1][0][ 8][14] =   3.792022e-05;
  val[1][1][0][ 8][15] =   4.297587e-03; eval[1][1][0][ 8][15] =   5.819301e-05;
  val[1][1][0][ 8][16] =   4.575356e-03; eval[1][1][0][ 8][16] =   6.233248e-05;
  val[1][1][0][ 8][17] =   4.535138e-03; eval[1][1][0][ 8][17] =   7.652371e-05;
  val[1][1][0][ 8][18] =   4.395907e-03; eval[1][1][0][ 8][18] =   1.204778e-04;
  val[1][1][0][ 8][19] =   4.112151e-03; eval[1][1][0][ 8][19] =   1.242715e-04;
  val[1][1][0][ 8][20] =   4.296176e-03; eval[1][1][0][ 8][20] =   6.533154e-05;
  val[1][1][0][ 8][21] =   4.205188e-03; eval[1][1][0][ 8][21] =   8.031937e-05;
  val[1][1][0][ 8][22] =   3.966178e-03; eval[1][1][0][ 8][22] =   9.204970e-05;
  val[1][1][0][ 8][23] =   3.833873e-03; eval[1][1][0][ 8][23] =   1.497666e-04;
  val[1][1][0][ 8][24] =   4.054559e-03; eval[1][1][0][ 8][24] =   2.257496e-04;
  val[1][1][0][ 8][25] =   4.249478e-03; eval[1][1][0][ 8][25] =   2.537261e-04;
  val[1][1][0][ 8][26] =   3.130150e-03; eval[1][1][0][ 8][26] =   4.236846e-04;
  val[1][1][0][ 8][27] =   4.176034e-03; eval[1][1][0][ 8][27] =   5.952645e-04;
  val[1][1][0][ 8][28] =   3.095020e-03; eval[1][1][0][ 8][28] =   3.391447e-04;
  val[1][1][0][ 8][29] =   4.444200e-03; eval[1][1][0][ 8][29] =   8.385079e-04;
  //emcdphi_sigma_c1d0z9
  n_val[1][1][0][9] = 30;
  val[1][1][0][ 9][ 0] =   0.000000e+00; eval[1][1][0][ 9][ 0] =   0.000000e+00;
  val[1][1][0][ 9][ 1] =   0.000000e+00; eval[1][1][0][ 9][ 1] =   0.000000e+00;
  val[1][1][0][ 9][ 2] =   5.843126e-03; eval[1][1][0][ 9][ 2] =   1.928331e-06;
  val[1][1][0][ 9][ 3] =   5.160795e-03; eval[1][1][0][ 9][ 3] =   2.035095e-06;
  val[1][1][0][ 9][ 4] =   4.970718e-03; eval[1][1][0][ 9][ 4] =   2.515627e-06;
  val[1][1][0][ 9][ 5] =   4.872868e-03; eval[1][1][0][ 9][ 5] =   3.293948e-06;
  val[1][1][0][ 9][ 6] =   4.879356e-03; eval[1][1][0][ 9][ 6] =   4.394255e-06;
  val[1][1][0][ 9][ 7] =   4.819109e-03; eval[1][1][0][ 9][ 7] =   5.542880e-06;
  val[1][1][0][ 9][ 8] =   4.740378e-03; eval[1][1][0][ 9][ 8] =   6.721884e-06;
  val[1][1][0][ 9][ 9] =   4.818095e-03; eval[1][1][0][ 9][ 9] =   1.192047e-05;
  val[1][1][0][ 9][10] =   4.765164e-03; eval[1][1][0][ 9][10] =   1.546928e-05;
  val[1][1][0][ 9][11] =   4.631092e-03; eval[1][1][0][ 9][11] =   1.876813e-05;
  val[1][1][0][ 9][12] =   4.598210e-03; eval[1][1][0][ 9][12] =   2.416175e-05;
  val[1][1][0][ 9][13] =   4.584418e-03; eval[1][1][0][ 9][13] =   3.093850e-05;
  val[1][1][0][ 9][14] =   4.507500e-03; eval[1][1][0][ 9][14] =   3.846690e-05;
  val[1][1][0][ 9][15] =   4.610462e-03; eval[1][1][0][ 9][15] =   5.173144e-05;
  val[1][1][0][ 9][16] =   4.582584e-03; eval[1][1][0][ 9][16] =   6.466182e-05;
  val[1][1][0][ 9][17] =   4.481474e-03; eval[1][1][0][ 9][17] =   7.520524e-05;
  val[1][1][0][ 9][18] =   4.476290e-03; eval[1][1][0][ 9][18] =   1.283751e-04;
  val[1][1][0][ 9][19] =   4.244285e-03; eval[1][1][0][ 9][19] =   7.914158e-05;
  val[1][1][0][ 9][20] =   4.237882e-03; eval[1][1][0][ 9][20] =   5.794657e-05;
  val[1][1][0][ 9][21] =   4.150559e-03; eval[1][1][0][ 9][21] =   8.506265e-05;
  val[1][1][0][ 9][22] =   4.294552e-03; eval[1][1][0][ 9][22] =   1.261329e-04;
  val[1][1][0][ 9][23] =   4.476400e-03; eval[1][1][0][ 9][23] =   1.296268e-04;
  val[1][1][0][ 9][24] =   4.352359e-03; eval[1][1][0][ 9][24] =   2.728945e-04;
  val[1][1][0][ 9][25] =   4.049416e-03; eval[1][1][0][ 9][25] =   2.175210e-04;
  val[1][1][0][ 9][26] =   4.214494e-03; eval[1][1][0][ 9][26] =   3.192832e-04;
  val[1][1][0][ 9][27] =   4.178041e-03; eval[1][1][0][ 9][27] =   3.194560e-04;
  val[1][1][0][ 9][28] =   4.018269e-03; eval[1][1][0][ 9][28] =   6.194448e-04;
  val[1][1][0][ 9][29] =   3.544269e-03; eval[1][1][0][ 9][29] =   1.149412e-03;
  //emcdphi_sigma_c1d1z0
  n_val[1][1][1][0] = 30;
  val[1][1][1][ 0][ 0] =   0.000000e+00; eval[1][1][1][ 0][ 0] =   0.000000e+00;
  val[1][1][1][ 0][ 1] =   0.000000e+00; eval[1][1][1][ 0][ 1] =   0.000000e+00;
  val[1][1][1][ 0][ 2] =   5.586340e-03; eval[1][1][1][ 0][ 2] =   2.178614e-06;
  val[1][1][1][ 0][ 3] =   4.828370e-03; eval[1][1][1][ 0][ 3] =   2.199465e-06;
  val[1][1][1][ 0][ 4] =   4.454475e-03; eval[1][1][1][ 0][ 4] =   2.236065e-06;
  val[1][1][1][ 0][ 5] =   4.320591e-03; eval[1][1][1][ 0][ 5] =   2.754396e-06;
  val[1][1][1][ 0][ 6] =   4.242188e-03; eval[1][1][1][ 0][ 6] =   3.565371e-06;
  val[1][1][1][ 0][ 7] =   4.163796e-03; eval[1][1][1][ 0][ 7] =   6.164549e-06;
  val[1][1][1][ 0][ 8] =   4.064896e-03; eval[1][1][1][ 0][ 8] =   7.588484e-06;
  val[1][1][1][ 0][ 9] =   3.979734e-03; eval[1][1][1][ 0][ 9] =   9.749295e-06;
  val[1][1][1][ 0][10] =   3.955259e-03; eval[1][1][1][ 0][10] =   1.289340e-05;
  val[1][1][1][ 0][11] =   3.961229e-03; eval[1][1][1][ 0][11] =   1.714430e-05;
  val[1][1][1][ 0][12] =   3.902539e-03; eval[1][1][1][ 0][12] =   2.144065e-05;
  val[1][1][1][ 0][13] =   3.914705e-03; eval[1][1][1][ 0][13] =   2.802033e-05;
  val[1][1][1][ 0][14] =   3.917496e-03; eval[1][1][1][ 0][14] =   3.611378e-05;
  val[1][1][1][ 0][15] =   3.928816e-03; eval[1][1][1][ 0][15] =   4.565730e-05;
  val[1][1][1][ 0][16] =   3.867667e-03; eval[1][1][1][ 0][16] =   5.449998e-05;
  val[1][1][1][ 0][17] =   3.944825e-03; eval[1][1][1][ 0][17] =   7.204087e-05;
  val[1][1][1][ 0][18] =   4.099063e-03; eval[1][1][1][ 0][18] =   9.673514e-05;
  val[1][1][1][ 0][19] =   3.765594e-03; eval[1][1][1][ 0][19] =   9.516865e-05;
  val[1][1][1][ 0][20] =   3.822313e-03; eval[1][1][1][ 0][20] =   4.848298e-05;
  val[1][1][1][ 0][21] =   3.756776e-03; eval[1][1][1][ 0][21] =   6.498585e-05;
  val[1][1][1][ 0][22] =   3.713031e-03; eval[1][1][1][ 0][22] =   9.335339e-05;
  val[1][1][1][ 0][23] =   3.602926e-03; eval[1][1][1][ 0][23] =   1.262463e-04;
  val[1][1][1][ 0][24] =   4.017498e-03; eval[1][1][1][ 0][24] =   1.667958e-04;
  val[1][1][1][ 0][25] =   3.674987e-03; eval[1][1][1][ 0][25] =   1.919447e-04;
  val[1][1][1][ 0][26] =   3.247614e-03; eval[1][1][1][ 0][26] =   3.204746e-04;
  val[1][1][1][ 0][27] =   3.303242e-03; eval[1][1][1][ 0][27] =   2.442519e-04;
  val[1][1][1][ 0][28] =   4.299635e-03; eval[1][1][1][ 0][28] =   4.453087e-04;
  val[1][1][1][ 0][29] =   1.915720e-02; eval[1][1][1][ 0][29] =   2.072013e-02;
  //emcdphi_sigma_c1d1z1
  n_val[1][1][1][1] = 30;
  val[1][1][1][ 1][ 0] =   0.000000e+00; eval[1][1][1][ 1][ 0] =   0.000000e+00;
  val[1][1][1][ 1][ 1] =   0.000000e+00; eval[1][1][1][ 1][ 1] =   0.000000e+00;
  val[1][1][1][ 1][ 2] =   5.594987e-03; eval[1][1][1][ 1][ 2] =   1.626208e-06;
  val[1][1][1][ 1][ 3] =   4.631999e-03; eval[1][1][1][ 1][ 3] =   1.770742e-06;
  val[1][1][1][ 1][ 4] =   4.348785e-03; eval[1][1][1][ 1][ 4] =   2.646219e-06;
  val[1][1][1][ 1][ 5] =   4.309763e-03; eval[1][1][1][ 1][ 5] =   2.560426e-06;
  val[1][1][1][ 1][ 6] =   4.211189e-03; eval[1][1][1][ 1][ 6] =   3.248151e-06;
  val[1][1][1][ 1][ 7] =   4.135239e-03; eval[1][1][1][ 1][ 7] =   5.646286e-06;
  val[1][1][1][ 1][ 8] =   4.055706e-03; eval[1][1][1][ 1][ 8] =   6.947974e-06;
  val[1][1][1][ 1][ 9] =   3.991830e-03; eval[1][1][1][ 1][ 9] =   9.101291e-06;
  val[1][1][1][ 1][10] =   3.960613e-03; eval[1][1][1][ 1][10] =   1.193711e-05;
  val[1][1][1][ 1][11] =   3.973663e-03; eval[1][1][1][ 1][11] =   1.588223e-05;
  val[1][1][1][ 1][12] =   3.913761e-03; eval[1][1][1][ 1][12] =   2.017581e-05;
  val[1][1][1][ 1][13] =   3.900148e-03; eval[1][1][1][ 1][13] =   2.607555e-05;
  val[1][1][1][ 1][14] =   3.842652e-03; eval[1][1][1][ 1][14] =   3.219759e-05;
  val[1][1][1][ 1][15] =   3.890640e-03; eval[1][1][1][ 1][15] =   4.173944e-05;
  val[1][1][1][ 1][16] =   3.826638e-03; eval[1][1][1][ 1][16] =   4.992472e-05;
  val[1][1][1][ 1][17] =   3.789809e-03; eval[1][1][1][ 1][17] =   5.992179e-05;
  val[1][1][1][ 1][18] =   3.751743e-03; eval[1][1][1][ 1][18] =   7.294758e-05;
  val[1][1][1][ 1][19] =   4.011952e-03; eval[1][1][1][ 1][19] =   1.087521e-04;
  val[1][1][1][ 1][20] =   3.778246e-03; eval[1][1][1][ 1][20] =   5.227860e-05;
  val[1][1][1][ 1][21] =   3.572473e-03; eval[1][1][1][ 1][21] =   6.228685e-05;
  val[1][1][1][ 1][22] =   3.739191e-03; eval[1][1][1][ 1][22] =   8.197882e-05;
  val[1][1][1][ 1][23] =   3.724275e-03; eval[1][1][1][ 1][23] =   1.214098e-04;
  val[1][1][1][ 1][24] =   3.594825e-03; eval[1][1][1][ 1][24] =   1.404161e-04;
  val[1][1][1][ 1][25] =   3.676161e-03; eval[1][1][1][ 1][25] =   1.864491e-04;
  val[1][1][1][ 1][26] =   3.609362e-03; eval[1][1][1][ 1][26] =   2.804140e-04;
  val[1][1][1][ 1][27] =   3.862185e-03; eval[1][1][1][ 1][27] =   5.625997e-04;
  val[1][1][1][ 1][28] =   3.717489e-03; eval[1][1][1][ 1][28] =   2.325429e-04;
  val[1][1][1][ 1][29] =   2.876747e-03; eval[1][1][1][ 1][29] =   3.268704e-04;
  //emcdphi_sigma_c1d1z2
  n_val[1][1][1][2] = 30;
  val[1][1][1][ 2][ 0] =   0.000000e+00; eval[1][1][1][ 2][ 0] =   0.000000e+00;
  val[1][1][1][ 2][ 1] =   0.000000e+00; eval[1][1][1][ 2][ 1] =   0.000000e+00;
  val[1][1][1][ 2][ 2] =   5.556147e-03; eval[1][1][1][ 2][ 2] =   1.935360e-06;
  val[1][1][1][ 2][ 3] =   4.743685e-03; eval[1][1][1][ 2][ 3] =   1.902647e-06;
  val[1][1][1][ 2][ 4] =   4.437686e-03; eval[1][1][1][ 2][ 4] =   2.830721e-06;
  val[1][1][1][ 2][ 5] =   4.361059e-03; eval[1][1][1][ 2][ 5] =   2.641023e-06;
  val[1][1][1][ 2][ 6] =   4.274726e-03; eval[1][1][1][ 2][ 6] =   3.409865e-06;
  val[1][1][1][ 2][ 7] =   4.191892e-03; eval[1][1][1][ 2][ 7] =   4.418435e-06;
  val[1][1][1][ 2][ 8] =   4.160772e-03; eval[1][1][1][ 2][ 8] =   7.676750e-06;
  val[1][1][1][ 2][ 9] =   4.073545e-03; eval[1][1][1][ 2][ 9] =   9.946304e-06;
  val[1][1][1][ 2][10] =   4.055926e-03; eval[1][1][1][ 2][10] =   1.303373e-05;
  val[1][1][1][ 2][11] =   4.020565e-03; eval[1][1][1][ 2][11] =   1.679588e-05;
  val[1][1][1][ 2][12] =   4.035706e-03; eval[1][1][1][ 2][12] =   2.220276e-05;
  val[1][1][1][ 2][13] =   3.974249e-03; eval[1][1][1][ 2][13] =   2.748698e-05;
  val[1][1][1][ 2][14] =   3.942125e-03; eval[1][1][1][ 2][14] =   3.418457e-05;
  val[1][1][1][ 2][15] =   3.990241e-03; eval[1][1][1][ 2][15] =   4.468847e-05;
  val[1][1][1][ 2][16] =   3.944978e-03; eval[1][1][1][ 2][16] =   5.366521e-05;
  val[1][1][1][ 2][17] =   4.057920e-03; eval[1][1][1][ 2][17] =   7.133317e-05;
  val[1][1][1][ 2][18] =   4.055340e-03; eval[1][1][1][ 2][18] =   9.046494e-05;
  val[1][1][1][ 2][19] =   4.039329e-03; eval[1][1][1][ 2][19] =   1.080258e-04;
  val[1][1][1][ 2][20] =   3.863930e-03; eval[1][1][1][ 2][20] =   5.270164e-05;
  val[1][1][1][ 2][21] =   4.099970e-03; eval[1][1][1][ 2][21] =   5.842262e-05;
  val[1][1][1][ 2][22] =   4.032782e-03; eval[1][1][1][ 2][22] =   8.784965e-05;
  val[1][1][1][ 2][23] =   3.894184e-03; eval[1][1][1][ 2][23] =   1.229644e-04;
  val[1][1][1][ 2][24] =   4.136742e-03; eval[1][1][1][ 2][24] =   1.263966e-04;
  val[1][1][1][ 2][25] =   3.567154e-03; eval[1][1][1][ 2][25] =   1.913543e-04;
  val[1][1][1][ 2][26] =   3.927137e-03; eval[1][1][1][ 2][26] =   4.679392e-04;
  val[1][1][1][ 2][27] =   4.398159e-03; eval[1][1][1][ 2][27] =   3.865992e-04;
  val[1][1][1][ 2][28] =   2.751554e-03; eval[1][1][1][ 2][28] =   6.208462e-04;
  val[1][1][1][ 2][29] =   3.121331e-03; eval[1][1][1][ 2][29] =   6.843763e-04;
  //emcdphi_sigma_c1d1z3
  n_val[1][1][1][3] = 30;
  val[1][1][1][ 3][ 0] =   0.000000e+00; eval[1][1][1][ 3][ 0] =   0.000000e+00;
  val[1][1][1][ 3][ 1] =   0.000000e+00; eval[1][1][1][ 3][ 1] =   0.000000e+00;
  val[1][1][1][ 3][ 2] =   5.616819e-03; eval[1][1][1][ 3][ 2] =   1.613662e-06;
  val[1][1][1][ 3][ 3] =   4.792404e-03; eval[1][1][1][ 3][ 3] =   1.968129e-06;
  val[1][1][1][ 3][ 4] =   4.487800e-03; eval[1][1][1][ 3][ 4] =   2.899182e-06;
  val[1][1][1][ 3][ 5] =   4.361731e-03; eval[1][1][1][ 3][ 5] =   2.624381e-06;
  val[1][1][1][ 3][ 6] =   4.231213e-03; eval[1][1][1][ 3][ 6] =   3.274385e-06;
  val[1][1][1][ 3][ 7] =   4.126334e-03; eval[1][1][1][ 3][ 7] =   4.141382e-06;
  val[1][1][1][ 3][ 8] =   4.028701e-03; eval[1][1][1][ 3][ 8] =   6.933658e-06;
  val[1][1][1][ 3][ 9] =   3.957488e-03; eval[1][1][1][ 3][ 9] =   9.108792e-06;
  val[1][1][1][ 3][10] =   3.911751e-03; eval[1][1][1][ 3][10] =   1.176989e-05;
  val[1][1][1][ 3][11] =   3.951267e-03; eval[1][1][1][ 3][11] =   1.620820e-05;
  val[1][1][1][ 3][12] =   3.907904e-03; eval[1][1][1][ 3][12] =   2.080206e-05;
  val[1][1][1][ 3][13] =   3.888833e-03; eval[1][1][1][ 3][13] =   2.658139e-05;
  val[1][1][1][ 3][14] =   3.900970e-03; eval[1][1][1][ 3][14] =   3.425223e-05;
  val[1][1][1][ 3][15] =   3.920563e-03; eval[1][1][1][ 3][15] =   4.374188e-05;
  val[1][1][1][ 3][16] =   3.949964e-03; eval[1][1][1][ 3][16] =   5.562569e-05;
  val[1][1][1][ 3][17] =   3.893184e-03; eval[1][1][1][ 3][17] =   6.646252e-05;
  val[1][1][1][ 3][18] =   3.945960e-03; eval[1][1][1][ 3][18] =   8.554611e-05;
  val[1][1][1][ 3][19] =   4.053482e-03; eval[1][1][1][ 3][19] =   4.977617e-05;
  val[1][1][1][ 3][20] =   3.861490e-03; eval[1][1][1][ 3][20] =   4.959948e-05;
  val[1][1][1][ 3][21] =   3.836190e-03; eval[1][1][1][ 3][21] =   6.657382e-05;
  val[1][1][1][ 3][22] =   3.664857e-03; eval[1][1][1][ 3][22] =   8.994498e-05;
  val[1][1][1][ 3][23] =   3.779178e-03; eval[1][1][1][ 3][23] =   1.682009e-04;
  val[1][1][1][ 3][24] =   3.623118e-03; eval[1][1][1][ 3][24] =   1.881415e-04;
  val[1][1][1][ 3][25] =   3.361762e-03; eval[1][1][1][ 3][25] =   2.769401e-04;
  val[1][1][1][ 3][26] =   3.481402e-03; eval[1][1][1][ 3][26] =   1.967021e-04;
  val[1][1][1][ 3][27] =   3.207923e-03; eval[1][1][1][ 3][27] =   2.431596e-04;
  val[1][1][1][ 3][28] =   3.521169e-03; eval[1][1][1][ 3][28] =   5.471387e-04;
  val[1][1][1][ 3][29] =   3.404945e-03; eval[1][1][1][ 3][29] =   4.167655e-04;
  //emcdphi_sigma_c1d1z4
  n_val[1][1][1][4] = 30;
  val[1][1][1][ 4][ 0] =   0.000000e+00; eval[1][1][1][ 4][ 0] =   0.000000e+00;
  val[1][1][1][ 4][ 1] =   0.000000e+00; eval[1][1][1][ 4][ 1] =   0.000000e+00;
  val[1][1][1][ 4][ 2] =   5.627193e-03; eval[1][1][1][ 4][ 2] =   1.800055e-06;
  val[1][1][1][ 4][ 3] =   4.819759e-03; eval[1][1][1][ 4][ 3] =   2.307925e-06;
  val[1][1][1][ 4][ 4] =   4.489638e-03; eval[1][1][1][ 4][ 4] =   2.509679e-06;
  val[1][1][1][ 4][ 5] =   4.439247e-03; eval[1][1][1][ 4][ 5] =   3.263776e-06;
  val[1][1][1][ 4][ 6] =   4.327465e-03; eval[1][1][1][ 4][ 6] =   4.148993e-06;
  val[1][1][1][ 4][ 7] =   4.205153e-03; eval[1][1][1][ 4][ 7] =   5.175180e-06;
  val[1][1][1][ 4][ 8] =   4.175898e-03; eval[1][1][1][ 4][ 8] =   9.065457e-06;
  val[1][1][1][ 4][ 9] =   4.071960e-03; eval[1][1][1][ 4][ 9] =   1.167207e-05;
  val[1][1][1][ 4][10] =   4.065892e-03; eval[1][1][1][ 4][10] =   1.576691e-05;
  val[1][1][1][ 4][11] =   4.035815e-03; eval[1][1][1][ 4][11] =   2.083873e-05;
  val[1][1][1][ 4][12] =   3.947830e-03; eval[1][1][1][ 4][12] =   2.587105e-05;
  val[1][1][1][ 4][13] =   3.950976e-03; eval[1][1][1][ 4][13] =   3.365213e-05;
  val[1][1][1][ 4][14] =   3.946140e-03; eval[1][1][1][ 4][14] =   4.287544e-05;
  val[1][1][1][ 4][15] =   4.044177e-03; eval[1][1][1][ 4][15] =   5.843126e-05;
  val[1][1][1][ 4][16] =   4.010781e-03; eval[1][1][1][ 4][16] =   7.218421e-05;
  val[1][1][1][ 4][17] =   3.923900e-03; eval[1][1][1][ 4][17] =   8.110943e-05;
  val[1][1][1][ 4][18] =   3.958286e-03; eval[1][1][1][ 4][18] =   1.030004e-04;
  val[1][1][1][ 4][19] =   4.005480e-03; eval[1][1][1][ 4][19] =   1.312498e-04;
  val[1][1][1][ 4][20] =   3.929865e-03; eval[1][1][1][ 4][20] =   5.910581e-05;
  val[1][1][1][ 4][21] =   3.935052e-03; eval[1][1][1][ 4][21] =   1.017330e-04;
  val[1][1][1][ 4][22] =   4.011113e-03; eval[1][1][1][ 4][22] =   1.424309e-04;
  val[1][1][1][ 4][23] =   4.106678e-03; eval[1][1][1][ 4][23] =   2.131622e-04;
  val[1][1][1][ 4][24] =   3.385004e-03; eval[1][1][1][ 4][24] =   1.639597e-04;
  val[1][1][1][ 4][25] =   3.779652e-03; eval[1][1][1][ 4][25] =   2.178889e-04;
  val[1][1][1][ 4][26] =   3.397667e-03; eval[1][1][1][ 4][26] =   2.829788e-04;
  val[1][1][1][ 4][27] =   3.587471e-03; eval[1][1][1][ 4][27] =   4.137376e-04;
  val[1][1][1][ 4][28] =   4.141273e-03; eval[1][1][1][ 4][28] =   6.758205e-04;
  val[1][1][1][ 4][29] =   5.119539e-03; eval[1][1][1][ 4][29] =   9.322621e-04;
  //emcdphi_sigma_c1d1z5
  n_val[1][1][1][5] = 30;
  val[1][1][1][ 5][ 0] =   0.000000e+00; eval[1][1][1][ 5][ 0] =   0.000000e+00;
  val[1][1][1][ 5][ 1] =   0.000000e+00; eval[1][1][1][ 5][ 1] =   0.000000e+00;
  val[1][1][1][ 5][ 2] =   5.728902e-03; eval[1][1][1][ 5][ 2] =   2.426969e-06;
  val[1][1][1][ 5][ 3] =   4.881836e-03; eval[1][1][1][ 5][ 3] =   2.414522e-06;
  val[1][1][1][ 5][ 4] =   4.725295e-03; eval[1][1][1][ 5][ 4] =   2.795559e-06;
  val[1][1][1][ 5][ 5] =   4.504241e-03; eval[1][1][1][ 5][ 5] =   3.230373e-06;
  val[1][1][1][ 5][ 6] =   4.301343e-03; eval[1][1][1][ 5][ 6] =   3.898188e-06;
  val[1][1][1][ 5][ 7] =   4.297425e-03; eval[1][1][1][ 5][ 7] =   7.068990e-06;
  val[1][1][1][ 5][ 8] =   4.234411e-03; eval[1][1][1][ 5][ 8] =   9.036269e-06;
  val[1][1][1][ 5][ 9] =   4.204003e-03; eval[1][1][1][ 5][ 9] =   1.232104e-05;
  val[1][1][1][ 5][10] =   4.227667e-03; eval[1][1][1][ 5][10] =   1.706034e-05;
  val[1][1][1][ 5][11] =   4.156101e-03; eval[1][1][1][ 5][11] =   2.196087e-05;
  val[1][1][1][ 5][12] =   4.058296e-03; eval[1][1][1][ 5][12] =   2.711688e-05;
  val[1][1][1][ 5][13] =   4.004567e-03; eval[1][1][1][ 5][13] =   3.407701e-05;
  val[1][1][1][ 5][14] =   4.014095e-03; eval[1][1][1][ 5][14] =   4.337431e-05;
  val[1][1][1][ 5][15] =   4.112253e-03; eval[1][1][1][ 5][15] =   5.937261e-05;
  val[1][1][1][ 5][16] =   4.056707e-03; eval[1][1][1][ 5][16] =   7.211292e-05;
  val[1][1][1][ 5][17] =   4.219335e-03; eval[1][1][1][ 5][17] =   9.797425e-05;
  val[1][1][1][ 5][18] =   4.120947e-03; eval[1][1][1][ 5][18] =   1.136541e-04;
  val[1][1][1][ 5][19] =   4.128574e-03; eval[1][1][1][ 5][19] =   1.029984e-04;
  val[1][1][1][ 5][20] =   4.024031e-03; eval[1][1][1][ 5][20] =   7.144822e-05;
  val[1][1][1][ 5][21] =   3.924667e-03; eval[1][1][1][ 5][21] =   8.442038e-05;
  val[1][1][1][ 5][22] =   3.895915e-03; eval[1][1][1][ 5][22] =   1.121345e-04;
  val[1][1][1][ 5][23] =   4.216747e-03; eval[1][1][1][ 5][23] =   3.596983e-04;
  val[1][1][1][ 5][24] =   4.252907e-03; eval[1][1][1][ 5][24] =   3.298123e-04;
  val[1][1][1][ 5][25] =   3.573845e-03; eval[1][1][1][ 5][25] =   3.578690e-04;
  val[1][1][1][ 5][26] =   3.592959e-03; eval[1][1][1][ 5][26] =   3.402702e-04;
  val[1][1][1][ 5][27] =   4.019946e-03; eval[1][1][1][ 5][27] =   4.395188e-04;
  val[1][1][1][ 5][28] =   4.085514e-03; eval[1][1][1][ 5][28] =   9.263841e-04;
  val[1][1][1][ 5][29] =   2.702849e-03; eval[1][1][1][ 5][29] =   5.089595e-04;
  //emcdphi_sigma_c1d1z6
  n_val[1][1][1][6] = 30;
  val[1][1][1][ 6][ 0] =   0.000000e+00; eval[1][1][1][ 6][ 0] =   0.000000e+00;
  val[1][1][1][ 6][ 1] =   0.000000e+00; eval[1][1][1][ 6][ 1] =   0.000000e+00;
  val[1][1][1][ 6][ 2] =   5.995400e-03; eval[1][1][1][ 6][ 2] =   1.828673e-06;
  val[1][1][1][ 6][ 3] =   5.082881e-03; eval[1][1][1][ 6][ 3] =   1.722226e-06;
  val[1][1][1][ 6][ 4] =   4.749735e-03; eval[1][1][1][ 6][ 4] =   2.442958e-06;
  val[1][1][1][ 6][ 5] =   4.475582e-03; eval[1][1][1][ 6][ 5] =   2.770087e-06;
  val[1][1][1][ 6][ 6] =   4.232542e-03; eval[1][1][1][ 6][ 6] =   3.228120e-06;
  val[1][1][1][ 6][ 7] =   4.216690e-03; eval[1][1][1][ 6][ 7] =   5.901022e-06;
  val[1][1][1][ 6][ 8] =   4.155863e-03; eval[1][1][1][ 6][ 8] =   7.514913e-06;
  val[1][1][1][ 6][ 9] =   4.086390e-03; eval[1][1][1][ 6][ 9] =   9.978383e-06;
  val[1][1][1][ 6][10] =   4.023521e-03; eval[1][1][1][ 6][10] =   1.306605e-05;
  val[1][1][1][ 6][11] =   3.984730e-03; eval[1][1][1][ 6][11] =   1.710742e-05;
  val[1][1][1][ 6][12] =   3.905345e-03; eval[1][1][1][ 6][12] =   2.147991e-05;
  val[1][1][1][ 6][13] =   3.798827e-03; eval[1][1][1][ 6][13] =   2.605528e-05;
  val[1][1][1][ 6][14] =   3.757829e-03; eval[1][1][1][ 6][14] =   3.267324e-05;
  val[1][1][1][ 6][15] =   3.769447e-03; eval[1][1][1][ 6][15] =   4.176820e-05;
  val[1][1][1][ 6][16] =   3.772728e-03; eval[1][1][1][ 6][16] =   5.230100e-05;
  val[1][1][1][ 6][17] =   3.756959e-03; eval[1][1][1][ 6][17] =   6.429198e-05;
  val[1][1][1][ 6][18] =   3.875514e-03; eval[1][1][1][ 6][18] =   8.528400e-05;
  val[1][1][1][ 6][19] =   3.750870e-03; eval[1][1][1][ 6][19] =   6.897758e-05;
  val[1][1][1][ 6][20] =   3.887110e-03; eval[1][1][1][ 6][20] =   5.941608e-05;
  val[1][1][1][ 6][21] =   3.874697e-03; eval[1][1][1][ 6][21] =   1.072012e-04;
  val[1][1][1][ 6][22] =   3.780626e-03; eval[1][1][1][ 6][22] =   1.135735e-04;
  val[1][1][1][ 6][23] =   3.595995e-03; eval[1][1][1][ 6][23] =   1.101071e-04;
  val[1][1][1][ 6][24] =   3.960412e-03; eval[1][1][1][ 6][24] =   1.467427e-04;
  val[1][1][1][ 6][25] =   3.550790e-03; eval[1][1][1][ 6][25] =   2.359316e-04;
  val[1][1][1][ 6][26] =   3.748586e-03; eval[1][1][1][ 6][26] =   3.304040e-04;
  val[1][1][1][ 6][27] =   2.874208e-03; eval[1][1][1][ 6][27] =   4.108616e-04;
  val[1][1][1][ 6][28] =   3.243082e-03; eval[1][1][1][ 6][28] =   3.195004e-04;
  val[1][1][1][ 6][29] =   4.423197e-03; eval[1][1][1][ 6][29] =   5.315643e-04;
  //emcdphi_sigma_c1d1z7
  n_val[1][1][1][7] = 30;
  val[1][1][1][ 7][ 0] =   0.000000e+00; eval[1][1][1][ 7][ 0] =   0.000000e+00;
  val[1][1][1][ 7][ 1] =   0.000000e+00; eval[1][1][1][ 7][ 1] =   0.000000e+00;
  val[1][1][1][ 7][ 2] =   5.992412e-03; eval[1][1][1][ 7][ 2] =   1.779311e-06;
  val[1][1][1][ 7][ 3] =   5.135190e-03; eval[1][1][1][ 7][ 3] =   1.717710e-06;
  val[1][1][1][ 7][ 4] =   4.781217e-03; eval[1][1][1][ 7][ 4] =   2.422887e-06;
  val[1][1][1][ 7][ 5] =   4.565428e-03; eval[1][1][1][ 7][ 5] =   2.857247e-06;
  val[1][1][1][ 7][ 6] =   4.331031e-03; eval[1][1][1][ 7][ 6] =   3.407679e-06;
  val[1][1][1][ 7][ 7] =   4.378543e-03; eval[1][1][1][ 7][ 7] =   6.388700e-06;
  val[1][1][1][ 7][ 8] =   4.357927e-03; eval[1][1][1][ 7][ 8] =   8.280431e-06;
  val[1][1][1][ 7][ 9] =   4.300364e-03; eval[1][1][1][ 7][ 9] =   1.103961e-05;
  val[1][1][1][ 7][10] =   4.277439e-03; eval[1][1][1][ 7][10] =   1.480241e-05;
  val[1][1][1][ 7][11] =   4.243174e-03; eval[1][1][1][ 7][11] =   1.936074e-05;
  val[1][1][1][ 7][12] =   4.106350e-03; eval[1][1][1][ 7][12] =   2.323037e-05;
  val[1][1][1][ 7][13] =   4.088263e-03; eval[1][1][1][ 7][13] =   3.012379e-05;
  val[1][1][1][ 7][14] =   4.029304e-03; eval[1][1][1][ 7][14] =   3.752454e-05;
  val[1][1][1][ 7][15] =   4.216422e-03; eval[1][1][1][ 7][15] =   5.331780e-05;
  val[1][1][1][ 7][16] =   4.160053e-03; eval[1][1][1][ 7][16] =   6.464902e-05;
  val[1][1][1][ 7][17] =   4.112539e-03; eval[1][1][1][ 7][17] =   7.802350e-05;
  val[1][1][1][ 7][18] =   4.089252e-03; eval[1][1][1][ 7][18] =   9.678970e-05;
  val[1][1][1][ 7][19] =   4.270984e-03; eval[1][1][1][ 7][19] =   1.320870e-04;
  val[1][1][1][ 7][20] =   4.140474e-03; eval[1][1][1][ 7][20] =   6.484971e-05;
  val[1][1][1][ 7][21] =   4.072483e-03; eval[1][1][1][ 7][21] =   7.032028e-05;
  val[1][1][1][ 7][22] =   3.996645e-03; eval[1][1][1][ 7][22] =   1.107797e-04;
  val[1][1][1][ 7][23] =   4.060170e-03; eval[1][1][1][ 7][23] =   1.203336e-04;
  val[1][1][1][ 7][24] =   3.808958e-03; eval[1][1][1][ 7][24] =   3.122581e-04;
  val[1][1][1][ 7][25] =   3.566542e-03; eval[1][1][1][ 7][25] =   3.081520e-04;
  val[1][1][1][ 7][26] =   4.050014e-03; eval[1][1][1][ 7][26] =   2.214401e-04;
  val[1][1][1][ 7][27] =   4.334543e-03; eval[1][1][1][ 7][27] =   3.217929e-04;
  val[1][1][1][ 7][28] =   4.269355e-03; eval[1][1][1][ 7][28] =   1.095404e-03;
  val[1][1][1][ 7][29] =   5.014574e-03; eval[1][1][1][ 7][29] =   6.298424e-04;
  //emcdphi_sigma_c1d1z8
  n_val[1][1][1][8] = 30;
  val[1][1][1][ 8][ 0] =   0.000000e+00; eval[1][1][1][ 8][ 0] =   0.000000e+00;
  val[1][1][1][ 8][ 1] =   0.000000e+00; eval[1][1][1][ 8][ 1] =   0.000000e+00;
  val[1][1][1][ 8][ 2] =   6.209598e-03; eval[1][1][1][ 8][ 2] =   1.541580e-06;
  val[1][1][1][ 8][ 3] =   5.252091e-03; eval[1][1][1][ 8][ 3] =   1.820964e-06;
  val[1][1][1][ 8][ 4] =   4.919664e-03; eval[1][1][1][ 8][ 4] =   2.627022e-06;
  val[1][1][1][ 8][ 5] =   4.742562e-03; eval[1][1][1][ 8][ 5] =   3.219508e-06;
  val[1][1][1][ 8][ 6] =   4.555783e-03; eval[1][1][1][ 8][ 6] =   4.008228e-06;
  val[1][1][1][ 8][ 7] =   4.548534e-03; eval[1][1][1][ 8][ 7] =   5.397826e-06;
  val[1][1][1][ 8][ 8] =   4.529525e-03; eval[1][1][1][ 8][ 8] =   6.988274e-06;
  val[1][1][1][ 8][ 9] =   4.593846e-03; eval[1][1][1][ 8][ 9] =   1.354162e-05;
  val[1][1][1][ 8][10] =   4.541452e-03; eval[1][1][1][ 8][10] =   1.771649e-05;
  val[1][1][1][ 8][11] =   4.438903e-03; eval[1][1][1][ 8][11] =   2.234994e-05;
  val[1][1][1][ 8][12] =   4.328001e-03; eval[1][1][1][ 8][12] =   2.770032e-05;
  val[1][1][1][ 8][13] =   4.242785e-03; eval[1][1][1][ 8][13] =   3.402058e-05;
  val[1][1][1][ 8][14] =   4.125741e-03; eval[1][1][1][ 8][14] =   4.068335e-05;
  val[1][1][1][ 8][15] =   4.143707e-03; eval[1][1][1][ 8][15] =   5.306574e-05;
  val[1][1][1][ 8][16] =   4.219330e-03; eval[1][1][1][ 8][16] =   7.052833e-05;
  val[1][1][1][ 8][17] =   4.139783e-03; eval[1][1][1][ 8][17] =   8.367926e-05;
  val[1][1][1][ 8][18] =   4.165907e-03; eval[1][1][1][ 8][18] =   1.023813e-04;
  val[1][1][1][ 8][19] =   4.106119e-03; eval[1][1][1][ 8][19] =   7.464044e-05;
  val[1][1][1][ 8][20] =   4.110781e-03; eval[1][1][1][ 8][20] =   6.562708e-05;
  val[1][1][1][ 8][21] =   4.129555e-03; eval[1][1][1][ 8][21] =   8.369578e-05;
  val[1][1][1][ 8][22] =   4.108997e-03; eval[1][1][1][ 8][22] =   1.052951e-04;
  val[1][1][1][ 8][23] =   4.215876e-03; eval[1][1][1][ 8][23] =   1.457762e-04;
  val[1][1][1][ 8][24] =   4.389280e-03; eval[1][1][1][ 8][24] =   4.798935e-04;
  val[1][1][1][ 8][25] =   4.326562e-03; eval[1][1][1][ 8][25] =   1.744922e-04;
  val[1][1][1][ 8][26] =   4.245507e-03; eval[1][1][1][ 8][26] =   4.723195e-04;
  val[1][1][1][ 8][27] =   2.930265e-03; eval[1][1][1][ 8][27] =   4.787739e-04;
  val[1][1][1][ 8][28] =   3.602737e-03; eval[1][1][1][ 8][28] =   5.418705e-04;
  val[1][1][1][ 8][29] =   5.540242e-03; eval[1][1][1][ 8][29] =   2.004148e-03;
  //emcdphi_sigma_c1d1z9
  n_val[1][1][1][9] = 30;
  val[1][1][1][ 9][ 0] =   0.000000e+00; eval[1][1][1][ 9][ 0] =   0.000000e+00;
  val[1][1][1][ 9][ 1] =   0.000000e+00; eval[1][1][1][ 9][ 1] =   0.000000e+00;
  val[1][1][1][ 9][ 2] =   6.277644e-03; eval[1][1][1][ 9][ 2] =   2.035790e-06;
  val[1][1][1][ 9][ 3] =   5.517261e-03; eval[1][1][1][ 9][ 3] =   2.244580e-06;
  val[1][1][1][ 9][ 4] =   5.118853e-03; eval[1][1][1][ 9][ 4] =   2.468040e-06;
  val[1][1][1][ 9][ 5] =   4.875459e-03; eval[1][1][1][ 9][ 5] =   2.909243e-06;
  val[1][1][1][ 9][ 6] =   4.612736e-03; eval[1][1][1][ 9][ 6] =   4.597097e-06;
  val[1][1][1][ 9][ 7] =   4.585222e-03; eval[1][1][1][ 9][ 7] =   6.172708e-06;
  val[1][1][1][ 9][ 8] =   4.780675e-03; eval[1][1][1][ 9][ 8] =   8.887052e-06;
  val[1][1][1][ 9][ 9] =   4.684469e-03; eval[1][1][1][ 9][ 9] =   1.150639e-05;
  val[1][1][1][ 9][10] =   4.624359e-03; eval[1][1][1][ 9][10] =   1.521216e-05;
  val[1][1][1][ 9][11] =   4.596625e-03; eval[1][1][1][ 9][11] =   2.000467e-05;
  val[1][1][1][ 9][12] =   4.490343e-03; eval[1][1][1][ 9][12] =   2.480088e-05;
  val[1][1][1][ 9][13] =   4.280605e-03; eval[1][1][1][ 9][13] =   3.839518e-05;
  val[1][1][1][ 9][14] =   4.264498e-03; eval[1][1][1][ 9][14] =   4.934067e-05;
  val[1][1][1][ 9][15] =   4.252470e-03; eval[1][1][1][ 9][15] =   6.197978e-05;
  val[1][1][1][ 9][16] =   4.116899e-03; eval[1][1][1][ 9][16] =   7.154799e-05;
  val[1][1][1][ 9][17] =   4.117499e-03; eval[1][1][1][ 9][17] =   8.756856e-05;
  val[1][1][1][ 9][18] =   4.044308e-03; eval[1][1][1][ 9][18] =   1.003923e-04;
  val[1][1][1][ 9][19] =   4.301155e-03; eval[1][1][1][ 9][19] =   1.509553e-04;
  val[1][1][1][ 9][20] =   4.094144e-03; eval[1][1][1][ 9][20] =   7.150100e-05;
  val[1][1][1][ 9][21] =   4.069745e-03; eval[1][1][1][ 9][21] =   1.004564e-04;
  val[1][1][1][ 9][22] =   4.118085e-03; eval[1][1][1][ 9][22] =   1.263049e-04;
  val[1][1][1][ 9][23] =   4.051920e-03; eval[1][1][1][ 9][23] =   1.704923e-04;
  val[1][1][1][ 9][24] =   4.319260e-03; eval[1][1][1][ 9][24] =   1.707666e-04;
  val[1][1][1][ 9][25] =   3.838187e-03; eval[1][1][1][ 9][25] =   3.136113e-04;
  val[1][1][1][ 9][26] =   3.195407e-03; eval[1][1][1][ 9][26] =   4.075250e-04;
  val[1][1][1][ 9][27] =   3.186885e-03; eval[1][1][1][ 9][27] =   3.079883e-04;
  val[1][1][1][ 9][28] =   2.019335e-03; eval[1][1][1][ 9][28] =   2.357848e-04;
  val[1][1][1][ 9][29] =   3.996993e-03; eval[1][1][1][ 9][29] =   5.411950e-04;
  //emcdz_mean_c0d0z0
  n_val[2][0][0][0] = 28;
  val[2][0][0][ 0][ 0] =   1.064888e+00; eval[2][0][0][ 0][ 0] =   8.807418e-02;
  val[2][0][0][ 0][ 1] =   5.909116e-01; eval[2][0][0][ 0][ 1] =   1.728382e-02;
  val[2][0][0][ 0][ 2] =   2.515809e-01; eval[2][0][0][ 0][ 2] =   1.054316e-02;
  val[2][0][0][ 0][ 3] =   1.003297e-01; eval[2][0][0][ 0][ 3] =   1.312004e-02;
  val[2][0][0][ 0][ 4] =  -1.296334e-01; eval[2][0][0][ 0][ 4] =   1.680767e-02;
  val[2][0][0][ 0][ 5] =  -4.268133e-01; eval[2][0][0][ 0][ 5] =   2.116636e-02;
  val[2][0][0][ 0][ 6] =  -5.929183e-01; eval[2][0][0][ 0][ 6] =   1.283969e-02;
  val[2][0][0][ 0][ 7] =  -7.902720e-01; eval[2][0][0][ 0][ 7] =   1.694990e-02;
  val[2][0][0][ 0][ 8] =  -8.997487e-01; eval[2][0][0][ 0][ 8] =   2.215184e-02;
  val[2][0][0][ 0][ 9] =  -9.772829e-01; eval[2][0][0][ 0][ 9] =   2.889737e-02;
  val[2][0][0][ 0][10] =  -1.032599e+00; eval[2][0][0][ 0][10] =   7.440078e-03;
  val[2][0][0][ 0][11] =  -1.102594e+00; eval[2][0][0][ 0][11] =   9.692496e-03;
  val[2][0][0][ 0][12] =  -1.130137e+00; eval[2][0][0][ 0][12] =   1.232115e-02;
  val[2][0][0][ 0][13] =  -1.170990e+00; eval[2][0][0][ 0][13] =   1.552521e-02;
  val[2][0][0][ 0][14] =  -1.236009e+00; eval[2][0][0][ 0][14] =   1.844827e-02;
  val[2][0][0][ 0][15] =  -1.289549e+00; eval[2][0][0][ 0][15] =   2.421413e-02;
  val[2][0][0][ 0][16] =  -1.249152e+00; eval[2][0][0][ 0][16] =   3.015929e-02;
  val[2][0][0][ 0][17] =  -1.300055e+00; eval[2][0][0][ 0][17] =   3.520670e-02;
  val[2][0][0][ 0][18] =  -1.267485e+00; eval[2][0][0][ 0][18] =   3.133805e-02;
  val[2][0][0][ 0][19] =  -1.242741e+00; eval[2][0][0][ 0][19] =   4.629364e-02;
  val[2][0][0][ 0][20] =  -1.436431e+00; eval[2][0][0][ 0][20] =   6.483142e-02;
  val[2][0][0][ 0][21] =  -1.425586e+00; eval[2][0][0][ 0][21] =   7.154286e-02;
  val[2][0][0][ 0][22] =  -1.617058e+00; eval[2][0][0][ 0][22] =   1.061162e-01;
  val[2][0][0][ 0][23] =  -1.640309e+00; eval[2][0][0][ 0][23] =   1.347650e-01;
  val[2][0][0][ 0][24] =  -1.436791e+00; eval[2][0][0][ 0][24] =   1.634958e-01;
  val[2][0][0][ 0][25] =  -1.727326e+00; eval[2][0][0][ 0][25] =   1.942907e-01;
  val[2][0][0][ 0][26] =  -1.510521e+00; eval[2][0][0][ 0][26] =   2.374594e-01;
  val[2][0][0][ 0][27] =  -1.326049e+00; eval[2][0][0][ 0][27] =   3.249861e-01;
  //emcdz_mean_c0d0z1
  n_val[2][0][0][1] = 28;
  val[2][0][0][ 1][ 0] =   1.352970e+00; eval[2][0][0][ 1][ 0] =   8.197383e-02;
  val[2][0][0][ 1][ 1] =   9.306290e-01; eval[2][0][0][ 1][ 1] =   1.596571e-02;
  val[2][0][0][ 1][ 2] =   6.553572e-01; eval[2][0][0][ 1][ 2] =   9.429417e-03;
  val[2][0][0][ 1][ 3] =   5.153748e-01; eval[2][0][0][ 1][ 3] =   1.192320e-02;
  val[2][0][0][ 1][ 4] =   3.961789e-01; eval[2][0][0][ 1][ 4] =   1.461291e-02;
  val[2][0][0][ 1][ 5] =   1.725671e-01; eval[2][0][0][ 1][ 5] =   1.858637e-02;
  val[2][0][0][ 1][ 6] =   4.394705e-02; eval[2][0][0][ 1][ 6] =   1.124591e-02;
  val[2][0][0][ 1][ 7] =  -5.953563e-02; eval[2][0][0][ 1][ 7] =   1.476064e-02;
  val[2][0][0][ 1][ 8] =  -2.309323e-01; eval[2][0][0][ 1][ 8] =   1.980536e-02;
  val[2][0][0][ 1][ 9] =  -2.496471e-01; eval[2][0][0][ 1][ 9] =   2.544674e-02;
  val[2][0][0][ 1][10] =  -3.468568e-01; eval[2][0][0][ 1][10] =   6.620102e-03;
  val[2][0][0][ 1][11] =  -3.822554e-01; eval[2][0][0][ 1][11] =   8.592963e-03;
  val[2][0][0][ 1][12] =  -3.765698e-01; eval[2][0][0][ 1][12] =   1.095869e-02;
  val[2][0][0][ 1][13] =  -4.239059e-01; eval[2][0][0][ 1][13] =   1.381471e-02;
  val[2][0][0][ 1][14] =  -4.557518e-01; eval[2][0][0][ 1][14] =   1.707475e-02;
  val[2][0][0][ 1][15] =  -4.658298e-01; eval[2][0][0][ 1][15] =   2.119570e-02;
  val[2][0][0][ 1][16] =  -4.895819e-01; eval[2][0][0][ 1][16] =   2.640848e-02;
  val[2][0][0][ 1][17] =  -4.996405e-01; eval[2][0][0][ 1][17] =   3.193902e-02;
  val[2][0][0][ 1][18] =  -5.481941e-01; eval[2][0][0][ 1][18] =   2.939403e-02;
  val[2][0][0][ 1][19] =  -5.298537e-01; eval[2][0][0][ 1][19] =   4.001067e-02;
  val[2][0][0][ 1][20] =  -5.414840e-01; eval[2][0][0][ 1][20] =   5.060927e-02;
  val[2][0][0][ 1][21] =  -4.999990e-01; eval[2][0][0][ 1][21] =   7.137356e-02;
  val[2][0][0][ 1][22] =  -6.454249e-01; eval[2][0][0][ 1][22] =   1.024202e-01;
  val[2][0][0][ 1][23] =  -6.284224e-01; eval[2][0][0][ 1][23] =   1.102489e-01;
  val[2][0][0][ 1][24] =  -5.075387e-01; eval[2][0][0][ 1][24] =   1.497855e-01;
  val[2][0][0][ 1][25] =  -9.388014e-01; eval[2][0][0][ 1][25] =   1.515148e-01;
  val[2][0][0][ 1][26] =  -1.076231e+00; eval[2][0][0][ 1][26] =   1.708539e-01;
  val[2][0][0][ 1][27] =  -1.853286e-01; eval[2][0][0][ 1][27] =   3.164925e-01;
  //emcdz_mean_c0d0z2
  n_val[2][0][0][2] = 28;
  val[2][0][0][ 2][ 0] =   1.726760e+00; eval[2][0][0][ 2][ 0] =   7.667986e-02;
  val[2][0][0][ 2][ 1] =   1.433698e+00; eval[2][0][0][ 2][ 1] =   1.455071e-02;
  val[2][0][0][ 2][ 2] =   1.243054e+00; eval[2][0][0][ 2][ 2] =   8.699757e-03;
  val[2][0][0][ 2][ 3] =   1.150495e+00; eval[2][0][0][ 2][ 3] =   1.069653e-02;
  val[2][0][0][ 2][ 4] =   1.071491e+00; eval[2][0][0][ 2][ 4] =   1.329057e-02;
  val[2][0][0][ 2][ 5] =   8.918614e-01; eval[2][0][0][ 2][ 5] =   1.698631e-02;
  val[2][0][0][ 2][ 6] =   8.009332e-01; eval[2][0][0][ 2][ 6] =   1.024183e-02;
  val[2][0][0][ 2][ 7] =   7.306684e-01; eval[2][0][0][ 2][ 7] =   1.353428e-02;
  val[2][0][0][ 2][ 8] =   6.709588e-01; eval[2][0][0][ 2][ 8] =   1.767611e-02;
  val[2][0][0][ 2][ 9] =   6.220978e-01; eval[2][0][0][ 2][ 9] =   2.296341e-02;
  val[2][0][0][ 2][10] =   5.803184e-01; eval[2][0][0][ 2][10] =   5.917139e-03;
  val[2][0][0][ 2][11] =   5.250452e-01; eval[2][0][0][ 2][11] =   7.614691e-03;
  val[2][0][0][ 2][12] =   5.447479e-01; eval[2][0][0][ 2][12] =   9.745962e-03;
  val[2][0][0][ 2][13] =   5.108234e-01; eval[2][0][0][ 2][13] =   1.238499e-02;
  val[2][0][0][ 2][14] =   5.041430e-01; eval[2][0][0][ 2][14] =   1.507108e-02;
  val[2][0][0][ 2][15] =   4.727943e-01; eval[2][0][0][ 2][15] =   1.909420e-02;
  val[2][0][0][ 2][16] =   4.525401e-01; eval[2][0][0][ 2][16] =   2.380812e-02;
  val[2][0][0][ 2][17] =   4.640053e-01; eval[2][0][0][ 2][17] =   2.736980e-02;
  val[2][0][0][ 2][18] =   4.410034e-01; eval[2][0][0][ 2][18] =   2.595244e-02;
  val[2][0][0][ 2][19] =   4.042789e-01; eval[2][0][0][ 2][19] =   3.815211e-02;
  val[2][0][0][ 2][20] =   4.508307e-01; eval[2][0][0][ 2][20] =   4.942960e-02;
  val[2][0][0][ 2][21] =   4.718747e-01; eval[2][0][0][ 2][21] =   6.912142e-02;
  val[2][0][0][ 2][22] =   4.896821e-01; eval[2][0][0][ 2][22] =   9.109178e-02;
  val[2][0][0][ 2][23] =   4.126028e-01; eval[2][0][0][ 2][23] =   9.247417e-02;
  val[2][0][0][ 2][24] =   5.628974e-01; eval[2][0][0][ 2][24] =   1.193078e-01;
  val[2][0][0][ 2][25] =   7.390733e-01; eval[2][0][0][ 2][25] =   1.644953e-01;
  val[2][0][0][ 2][26] =   6.235475e-01; eval[2][0][0][ 2][26] =   2.915026e-01;
  val[2][0][0][ 2][27] =   1.004648e+00; eval[2][0][0][ 2][27] =   2.907842e-01;
  //emcdz_mean_c0d0z3
  n_val[2][0][0][3] = 28;
  val[2][0][0][ 3][ 0] =   1.871470e+00; eval[2][0][0][ 3][ 0] =   7.718187e-02;
  val[2][0][0][ 3][ 1] =   1.751690e+00; eval[2][0][0][ 3][ 1] =   1.370382e-02;
  val[2][0][0][ 3][ 2] =   1.619754e+00; eval[2][0][0][ 3][ 2] =   8.142431e-03;
  val[2][0][0][ 3][ 3] =   1.571490e+00; eval[2][0][0][ 3][ 3] =   9.833315e-03;
  val[2][0][0][ 3][ 4] =   1.519442e+00; eval[2][0][0][ 3][ 4] =   1.228739e-02;
  val[2][0][0][ 3][ 5] =   1.443680e+00; eval[2][0][0][ 3][ 5] =   1.548602e-02;
  val[2][0][0][ 3][ 6] =   1.388069e+00; eval[2][0][0][ 3][ 6] =   9.312865e-03;
  val[2][0][0][ 3][ 7] =   1.344341e+00; eval[2][0][0][ 3][ 7] =   1.206285e-02;
  val[2][0][0][ 3][ 8] =   1.308972e+00; eval[2][0][0][ 3][ 8] =   1.566979e-02;
  val[2][0][0][ 3][ 9] =   1.278459e+00; eval[2][0][0][ 3][ 9] =   2.065755e-02;
  val[2][0][0][ 3][10] =   1.264838e+00; eval[2][0][0][ 3][10] =   5.340070e-03;
  val[2][0][0][ 3][11] =   1.246507e+00; eval[2][0][0][ 3][11] =   6.828198e-03;
  val[2][0][0][ 3][12] =   1.231624e+00; eval[2][0][0][ 3][12] =   8.736633e-03;
  val[2][0][0][ 3][13] =   1.238333e+00; eval[2][0][0][ 3][13] =   1.088073e-02;
  val[2][0][0][ 3][14] =   1.217798e+00; eval[2][0][0][ 3][14] =   1.371326e-02;
  val[2][0][0][ 3][15] =   1.211696e+00; eval[2][0][0][ 3][15] =   1.653050e-02;
  val[2][0][0][ 3][16] =   1.195565e+00; eval[2][0][0][ 3][16] =   2.025889e-02;
  val[2][0][0][ 3][17] =   1.161346e+00; eval[2][0][0][ 3][17] =   2.442817e-02;
  val[2][0][0][ 3][18] =   1.187714e+00; eval[2][0][0][ 3][18] =   2.331075e-02;
  val[2][0][0][ 3][19] =   1.166629e+00; eval[2][0][0][ 3][19] =   3.218986e-02;
  val[2][0][0][ 3][20] =   1.099629e+00; eval[2][0][0][ 3][20] =   4.320953e-02;
  val[2][0][0][ 3][21] =   1.168506e+00; eval[2][0][0][ 3][21] =   5.379236e-02;
  val[2][0][0][ 3][22] =   1.229058e+00; eval[2][0][0][ 3][22] =   7.437693e-02;
  val[2][0][0][ 3][23] =   1.165452e+00; eval[2][0][0][ 3][23] =   9.469525e-02;
  val[2][0][0][ 3][24] =   1.120363e+00; eval[2][0][0][ 3][24] =   9.319956e-02;
  val[2][0][0][ 3][25] =   1.106952e+00; eval[2][0][0][ 3][25] =   1.100863e-01;
  val[2][0][0][ 3][26] =   1.187550e+00; eval[2][0][0][ 3][26] =   1.542205e-01;
  val[2][0][0][ 3][27] =   1.559276e+00; eval[2][0][0][ 3][27] =   1.692677e-01;
  //emcdz_mean_c0d0z4
  n_val[2][0][0][4] = 28;
  val[2][0][0][ 4][ 0] =   2.000578e+00; eval[2][0][0][ 4][ 0] =   9.076441e-02;
  val[2][0][0][ 4][ 1] =   1.979937e+00; eval[2][0][0][ 4][ 1] =   1.583845e-02;
  val[2][0][0][ 4][ 2] =   1.965385e+00; eval[2][0][0][ 4][ 2] =   9.252211e-03;
  val[2][0][0][ 4][ 3] =   1.953107e+00; eval[2][0][0][ 4][ 3] =   1.142113e-02;
  val[2][0][0][ 4][ 4] =   1.943726e+00; eval[2][0][0][ 4][ 4] =   1.454446e-02;
  val[2][0][0][ 4][ 5] =   1.917895e+00; eval[2][0][0][ 4][ 5] =   1.852139e-02;
  val[2][0][0][ 4][ 6] =   1.901787e+00; eval[2][0][0][ 4][ 6] =   1.124881e-02;
  val[2][0][0][ 4][ 7] =   1.886716e+00; eval[2][0][0][ 4][ 7] =   1.473848e-02;
  val[2][0][0][ 4][ 8] =   1.884433e+00; eval[2][0][0][ 4][ 8] =   1.920637e-02;
  val[2][0][0][ 4][ 9] =   1.870816e+00; eval[2][0][0][ 4][ 9] =   2.479126e-02;
  val[2][0][0][ 4][10] =   1.870308e+00; eval[2][0][0][ 4][10] =   6.652154e-03;
  val[2][0][0][ 4][11] =   1.865957e+00; eval[2][0][0][ 4][11] =   8.580736e-03;
  val[2][0][0][ 4][12] =   1.830653e+00; eval[2][0][0][ 4][12] =   1.155303e-02;
  val[2][0][0][ 4][13] =   1.852938e+00; eval[2][0][0][ 4][13] =   1.362631e-02;
  val[2][0][0][ 4][14] =   1.855860e+00; eval[2][0][0][ 4][14] =   1.690679e-02;
  val[2][0][0][ 4][15] =   1.885884e+00; eval[2][0][0][ 4][15] =   2.092451e-02;
  val[2][0][0][ 4][16] =   1.805591e+00; eval[2][0][0][ 4][16] =   2.474846e-02;
  val[2][0][0][ 4][17] =   1.830210e+00; eval[2][0][0][ 4][17] =   2.870927e-02;
  val[2][0][0][ 4][18] =   1.833860e+00; eval[2][0][0][ 4][18] =   2.938416e-02;
  val[2][0][0][ 4][19] =   1.820009e+00; eval[2][0][0][ 4][19] =   4.328632e-02;
  val[2][0][0][ 4][20] =   1.900640e+00; eval[2][0][0][ 4][20] =   5.126977e-02;
  val[2][0][0][ 4][21] =   1.842972e+00; eval[2][0][0][ 4][21] =   8.220655e-02;
  val[2][0][0][ 4][22] =   1.899373e+00; eval[2][0][0][ 4][22] =   8.977549e-02;
  val[2][0][0][ 4][23] =   1.719186e+00; eval[2][0][0][ 4][23] =   9.756868e-02;
  val[2][0][0][ 4][24] =   1.510066e+00; eval[2][0][0][ 4][24] =   1.554732e-01;
  val[2][0][0][ 4][25] =   1.887494e+00; eval[2][0][0][ 4][25] =   1.502150e-01;
  val[2][0][0][ 4][26] =   2.179411e+00; eval[2][0][0][ 4][26] =   1.719487e-01;
  val[2][0][0][ 4][27] =   2.062115e+00; eval[2][0][0][ 4][27] =   2.340999e-01;
  //emcdz_mean_c0d0z5
  n_val[2][0][0][5] = 28;
  val[2][0][0][ 5][ 0] =   2.147612e+00; eval[2][0][0][ 5][ 0] =   9.771244e-02;
  val[2][0][0][ 5][ 1] =   2.332206e+00; eval[2][0][0][ 5][ 1] =   1.817843e-02;
  val[2][0][0][ 5][ 2] =   2.425093e+00; eval[2][0][0][ 5][ 2] =   1.063759e-02;
  val[2][0][0][ 5][ 3] =   2.448691e+00; eval[2][0][0][ 5][ 3] =   1.377167e-02;
  val[2][0][0][ 5][ 4] =   2.440916e+00; eval[2][0][0][ 5][ 4] =   1.710478e-02;
  val[2][0][0][ 5][ 5] =   2.478703e+00; eval[2][0][0][ 5][ 5] =   2.157670e-02;
  val[2][0][0][ 5][ 6] =   2.517186e+00; eval[2][0][0][ 5][ 6] =   1.311905e-02;
  val[2][0][0][ 5][ 7] =   2.554454e+00; eval[2][0][0][ 5][ 7] =   1.749841e-02;
  val[2][0][0][ 5][ 8] =   2.570866e+00; eval[2][0][0][ 5][ 8] =   2.285997e-02;
  val[2][0][0][ 5][ 9] =   2.575406e+00; eval[2][0][0][ 5][ 9] =   2.976970e-02;
  val[2][0][0][ 5][10] =   2.633650e+00; eval[2][0][0][ 5][10] =   7.841722e-03;
  val[2][0][0][ 5][11] =   2.601639e+00; eval[2][0][0][ 5][11] =   9.780372e-03;
  val[2][0][0][ 5][12] =   2.630318e+00; eval[2][0][0][ 5][12] =   1.293719e-02;
  val[2][0][0][ 5][13] =   2.635003e+00; eval[2][0][0][ 5][13] =   1.549079e-02;
  val[2][0][0][ 5][14] =   2.600765e+00; eval[2][0][0][ 5][14] =   1.887414e-02;
  val[2][0][0][ 5][15] =   2.685314e+00; eval[2][0][0][ 5][15] =   2.468464e-02;
  val[2][0][0][ 5][16] =   2.549244e+00; eval[2][0][0][ 5][16] =   2.887108e-02;
  val[2][0][0][ 5][17] =   2.637868e+00; eval[2][0][0][ 5][17] =   3.780967e-02;
  val[2][0][0][ 5][18] =   2.652703e+00; eval[2][0][0][ 5][18] =   3.197956e-02;
  val[2][0][0][ 5][19] =   2.700752e+00; eval[2][0][0][ 5][19] =   4.407127e-02;
  val[2][0][0][ 5][20] =   2.600597e+00; eval[2][0][0][ 5][20] =   6.294091e-02;
  val[2][0][0][ 5][21] =   2.656817e+00; eval[2][0][0][ 5][21] =   8.011551e-02;
  val[2][0][0][ 5][22] =   2.621918e+00; eval[2][0][0][ 5][22] =   9.718445e-02;
  val[2][0][0][ 5][23] =   2.447478e+00; eval[2][0][0][ 5][23] =   1.649109e-01;
  val[2][0][0][ 5][24] =   2.557420e+00; eval[2][0][0][ 5][24] =   1.337607e-01;
  val[2][0][0][ 5][25] =   2.453095e+00; eval[2][0][0][ 5][25] =   2.194261e-01;
  val[2][0][0][ 5][26] =   2.757221e+00; eval[2][0][0][ 5][26] =   1.355292e-01;
  val[2][0][0][ 5][27] =   2.716099e+00; eval[2][0][0][ 5][27] =   5.648092e-01;
  //emcdz_mean_c0d0z6
  n_val[2][0][0][6] = 28;
  val[2][0][0][ 6][ 0] =   2.368609e+00; eval[2][0][0][ 6][ 0] =   8.652844e-02;
  val[2][0][0][ 6][ 1] =   2.623122e+00; eval[2][0][0][ 6][ 1] =   1.544651e-02;
  val[2][0][0][ 6][ 2] =   2.803892e+00; eval[2][0][0][ 6][ 2] =   8.959080e-03;
  val[2][0][0][ 6][ 3] =   2.892416e+00; eval[2][0][0][ 6][ 3] =   1.170454e-02;
  val[2][0][0][ 6][ 4] =   2.944309e+00; eval[2][0][0][ 6][ 4] =   1.448598e-02;
  val[2][0][0][ 6][ 5] =   3.023884e+00; eval[2][0][0][ 6][ 5] =   1.819367e-02;
  val[2][0][0][ 6][ 6] =   3.092096e+00; eval[2][0][0][ 6][ 6] =   1.084952e-02;
  val[2][0][0][ 6][ 7] =   3.142605e+00; eval[2][0][0][ 6][ 7] =   1.424503e-02;
  val[2][0][0][ 6][ 8] =   3.184996e+00; eval[2][0][0][ 6][ 8] =   1.850375e-02;
  val[2][0][0][ 6][ 9] =   3.220538e+00; eval[2][0][0][ 6][ 9] =   2.449937e-02;
  val[2][0][0][ 6][10] =   3.263611e+00; eval[2][0][0][ 6][10] =   6.318463e-03;
  val[2][0][0][ 6][11] =   3.300081e+00; eval[2][0][0][ 6][11] =   8.250391e-03;
  val[2][0][0][ 6][12] =   3.279048e+00; eval[2][0][0][ 6][12] =   1.032689e-02;
  val[2][0][0][ 6][13] =   3.289843e+00; eval[2][0][0][ 6][13] =   1.286470e-02;
  val[2][0][0][ 6][14] =   3.283061e+00; eval[2][0][0][ 6][14] =   1.599673e-02;
  val[2][0][0][ 6][15] =   3.287268e+00; eval[2][0][0][ 6][15] =   1.973784e-02;
  val[2][0][0][ 6][16] =   3.331100e+00; eval[2][0][0][ 6][16] =   2.395482e-02;
  val[2][0][0][ 6][17] =   3.338744e+00; eval[2][0][0][ 6][17] =   2.990464e-02;
  val[2][0][0][ 6][18] =   3.299162e+00; eval[2][0][0][ 6][18] =   2.589869e-02;
  val[2][0][0][ 6][19] =   3.351739e+00; eval[2][0][0][ 6][19] =   3.559653e-02;
  val[2][0][0][ 6][20] =   3.374746e+00; eval[2][0][0][ 6][20] =   5.207735e-02;
  val[2][0][0][ 6][21] =   3.416109e+00; eval[2][0][0][ 6][21] =   7.137284e-02;
  val[2][0][0][ 6][22] =   3.224312e+00; eval[2][0][0][ 6][22] =   7.728851e-02;
  val[2][0][0][ 6][23] =   3.484058e+00; eval[2][0][0][ 6][23] =   8.649331e-02;
  val[2][0][0][ 6][24] =   3.326734e+00; eval[2][0][0][ 6][24] =   1.696293e-01;
  val[2][0][0][ 6][25] =   5.630175e+00; eval[2][0][0][ 6][25] =   1.705717e+00;
  val[2][0][0][ 6][26] =   2.867710e+00; eval[2][0][0][ 6][26] =   1.414200e+00;
  val[2][0][0][ 6][27] =   3.116370e+00; eval[2][0][0][ 6][27] =   2.056230e-01;
  //emcdz_mean_c0d0z7
  n_val[2][0][0][7] = 28;
  val[2][0][0][ 7][ 0] =   2.521290e+00; eval[2][0][0][ 7][ 0] =   8.838369e-02;
  val[2][0][0][ 7][ 1] =   2.930604e+00; eval[2][0][0][ 7][ 1] =   1.607302e-02;
  val[2][0][0][ 7][ 2] =   3.179632e+00; eval[2][0][0][ 7][ 2] =   9.561158e-03;
  val[2][0][0][ 7][ 3] =   3.332627e+00; eval[2][0][0][ 7][ 3] =   1.261505e-02;
  val[2][0][0][ 7][ 4] =   3.428289e+00; eval[2][0][0][ 7][ 4] =   1.572806e-02;
  val[2][0][0][ 7][ 5] =   3.585580e+00; eval[2][0][0][ 7][ 5] =   1.993513e-02;
  val[2][0][0][ 7][ 6] =   3.686478e+00; eval[2][0][0][ 7][ 6] =   1.187811e-02;
  val[2][0][0][ 7][ 7] =   3.757643e+00; eval[2][0][0][ 7][ 7] =   1.559072e-02;
  val[2][0][0][ 7][ 8] =   3.813264e+00; eval[2][0][0][ 7][ 8] =   2.029337e-02;
  val[2][0][0][ 7][ 9] =   3.881589e+00; eval[2][0][0][ 7][ 9] =   2.629863e-02;
  val[2][0][0][ 7][10] =   3.910533e+00; eval[2][0][0][ 7][10] =   6.786295e-03;
  val[2][0][0][ 7][11] =   3.906668e+00; eval[2][0][0][ 7][11] =   8.786770e-03;
  val[2][0][0][ 7][12] =   3.946410e+00; eval[2][0][0][ 7][12] =   1.120957e-02;
  val[2][0][0][ 7][13] =   3.936881e+00; eval[2][0][0][ 7][13] =   1.411427e-02;
  val[2][0][0][ 7][14] =   3.950254e+00; eval[2][0][0][ 7][14] =   1.725772e-02;
  val[2][0][0][ 7][15] =   3.996265e+00; eval[2][0][0][ 7][15] =   2.130003e-02;
  val[2][0][0][ 7][16] =   4.059657e+00; eval[2][0][0][ 7][16] =   2.669081e-02;
  val[2][0][0][ 7][17] =   4.013475e+00; eval[2][0][0][ 7][17] =   3.208611e-02;
  val[2][0][0][ 7][18] =   4.009250e+00; eval[2][0][0][ 7][18] =   2.930066e-02;
  val[2][0][0][ 7][19] =   3.986391e+00; eval[2][0][0][ 7][19] =   4.036285e-02;
  val[2][0][0][ 7][20] =   4.084083e+00; eval[2][0][0][ 7][20] =   5.513620e-02;
  val[2][0][0][ 7][21] =   4.035337e+00; eval[2][0][0][ 7][21] =   7.137882e-02;
  val[2][0][0][ 7][22] =   4.086910e+00; eval[2][0][0][ 7][22] =   9.176389e-02;
  val[2][0][0][ 7][23] =   4.050539e+00; eval[2][0][0][ 7][23] =   1.193230e-01;
  val[2][0][0][ 7][24] =   3.913340e+00; eval[2][0][0][ 7][24] =   1.612193e-01;
  val[2][0][0][ 7][25] =   4.311101e+00; eval[2][0][0][ 7][25] =   1.821072e-01;
  val[2][0][0][ 7][26] =   3.912609e+00; eval[2][0][0][ 7][26] =   2.006395e-01;
  val[2][0][0][ 7][27] =   4.533188e+00; eval[2][0][0][ 7][27] =   2.818014e-01;
  //emcdz_mean_c0d0z8
  n_val[2][0][0][8] = 28;
  val[2][0][0][ 8][ 0] =   2.856790e+00; eval[2][0][0][ 8][ 0] =   9.529766e-02;
  val[2][0][0][ 8][ 1] =   3.323498e+00; eval[2][0][0][ 8][ 1] =   1.818324e-02;
  val[2][0][0][ 8][ 2] =   3.652891e+00; eval[2][0][0][ 8][ 2] =   1.073305e-02;
  val[2][0][0][ 8][ 3] =   3.798421e+00; eval[2][0][0][ 8][ 3] =   1.422930e-02;
  val[2][0][0][ 8][ 4] =   4.053773e+00; eval[2][0][0][ 8][ 4] =   1.811461e-02;
  val[2][0][0][ 8][ 5] =   4.223498e+00; eval[2][0][0][ 8][ 5] =   2.275088e-02;
  val[2][0][0][ 8][ 6] =   4.346825e+00; eval[2][0][0][ 8][ 6] =   1.353267e-02;
  val[2][0][0][ 8][ 7] =   4.424390e+00; eval[2][0][0][ 8][ 7] =   1.789632e-02;
  val[2][0][0][ 8][ 8] =   4.536004e+00; eval[2][0][0][ 8][ 8] =   2.390211e-02;
  val[2][0][0][ 8][ 9] =   4.602496e+00; eval[2][0][0][ 8][ 9] =   3.048018e-02;
  val[2][0][0][ 8][10] =   4.668644e+00; eval[2][0][0][ 8][10] =   7.827379e-03;
  val[2][0][0][ 8][11] =   4.663589e+00; eval[2][0][0][ 8][11] =   9.987793e-03;
  val[2][0][0][ 8][12] =   4.752581e+00; eval[2][0][0][ 8][12] =   1.292413e-02;
  val[2][0][0][ 8][13] =   4.760623e+00; eval[2][0][0][ 8][13] =   1.569924e-02;
  val[2][0][0][ 8][14] =   4.773767e+00; eval[2][0][0][ 8][14] =   1.991115e-02;
  val[2][0][0][ 8][15] =   4.802658e+00; eval[2][0][0][ 8][15] =   2.471109e-02;
  val[2][0][0][ 8][16] =   4.860434e+00; eval[2][0][0][ 8][16] =   2.937601e-02;
  val[2][0][0][ 8][17] =   4.921159e+00; eval[2][0][0][ 8][17] =   3.502875e-02;
  val[2][0][0][ 8][18] =   4.775330e+00; eval[2][0][0][ 8][18] =   3.424063e-02;
  val[2][0][0][ 8][19] =   4.866712e+00; eval[2][0][0][ 8][19] =   4.458446e-02;
  val[2][0][0][ 8][20] =   4.918241e+00; eval[2][0][0][ 8][20] =   6.062753e-02;
  val[2][0][0][ 8][21] =   4.677622e+00; eval[2][0][0][ 8][21] =   8.662575e-02;
  val[2][0][0][ 8][22] =   5.050271e+00; eval[2][0][0][ 8][22] =   9.629428e-02;
  val[2][0][0][ 8][23] =   4.883703e+00; eval[2][0][0][ 8][23] =   1.241858e-01;
  val[2][0][0][ 8][24] =   4.922809e+00; eval[2][0][0][ 8][24] =   1.837917e-01;
  val[2][0][0][ 8][25] =   5.051816e+00; eval[2][0][0][ 8][25] =   1.555289e-01;
  val[2][0][0][ 8][26] =   5.009015e+00; eval[2][0][0][ 8][26] =   2.092332e-01;
  val[2][0][0][ 8][27] =   4.427413e+00; eval[2][0][0][ 8][27] =   3.518456e-01;
  //emcdz_mean_c0d0z9
  n_val[2][0][0][9] = 28;
  val[2][0][0][ 9][ 0] =   2.999981e+00; eval[2][0][0][ 9][ 0] =   1.050965e-01;
  val[2][0][0][ 9][ 1] =   3.568891e+00; eval[2][0][0][ 9][ 1] =   2.144561e-02;
  val[2][0][0][ 9][ 2] =   3.916788e+00; eval[2][0][0][ 9][ 2] =   1.244013e-02;
  val[2][0][0][ 9][ 3] =   4.124477e+00; eval[2][0][0][ 9][ 3] =   1.654678e-02;
  val[2][0][0][ 9][ 4] =   4.406892e+00; eval[2][0][0][ 9][ 4] =   2.111970e-02;
  val[2][0][0][ 9][ 5] =   4.627194e+00; eval[2][0][0][ 9][ 5] =   2.707248e-02;
  val[2][0][0][ 9][ 6] =   4.787469e+00; eval[2][0][0][ 9][ 6] =   1.614694e-02;
  val[2][0][0][ 9][ 7] =   4.972534e+00; eval[2][0][0][ 9][ 7] =   2.162933e-02;
  val[2][0][0][ 9][ 8] =   5.006853e+00; eval[2][0][0][ 9][ 8] =   2.848653e-02;
  val[2][0][0][ 9][ 9] =   5.103408e+00; eval[2][0][0][ 9][ 9] =   3.739616e-02;
  val[2][0][0][ 9][10] =   5.184473e+00; eval[2][0][0][ 9][10] =   9.689948e-03;
  val[2][0][0][ 9][11] =   5.286301e+00; eval[2][0][0][ 9][11] =   1.227958e-02;
  val[2][0][0][ 9][12] =   5.396997e+00; eval[2][0][0][ 9][12] =   1.557799e-02;
  val[2][0][0][ 9][13] =   5.364327e+00; eval[2][0][0][ 9][13] =   1.953999e-02;
  val[2][0][0][ 9][14] =   5.436913e+00; eval[2][0][0][ 9][14] =   2.420360e-02;
  val[2][0][0][ 9][15] =   5.408877e+00; eval[2][0][0][ 9][15] =   3.011864e-02;
  val[2][0][0][ 9][16] =   5.478332e+00; eval[2][0][0][ 9][16] =   3.707939e-02;
  val[2][0][0][ 9][17] =   5.446738e+00; eval[2][0][0][ 9][17] =   4.372740e-02;
  val[2][0][0][ 9][18] =   5.576154e+00; eval[2][0][0][ 9][18] =   4.241711e-02;
  val[2][0][0][ 9][19] =   5.477681e+00; eval[2][0][0][ 9][19] =   5.900588e-02;
  val[2][0][0][ 9][20] =   5.433424e+00; eval[2][0][0][ 9][20] =   8.519375e-02;
  val[2][0][0][ 9][21] =   5.430338e+00; eval[2][0][0][ 9][21] =   1.007872e-01;
  val[2][0][0][ 9][22] =   5.384705e+00; eval[2][0][0][ 9][22] =   1.576536e-01;
  val[2][0][0][ 9][23] =   3.396398e+00; eval[2][0][0][ 9][23] =   1.411815e+00;
  val[2][0][0][ 9][24] =   6.054507e+00; eval[2][0][0][ 9][24] =   1.683866e-01;
  val[2][0][0][ 9][25] =   5.698393e+00; eval[2][0][0][ 9][25] =   3.162339e-01;
  val[2][0][0][ 9][26] =  -9.145928e+00; eval[2][0][0][ 9][26] =   1.414214e+00;
  val[2][0][0][ 9][27] =   5.225680e+00; eval[2][0][0][ 9][27] =   4.677387e-01;
  //emcdz_mean_c0d1z0
  n_val[2][0][1][0] = 28;
  val[2][0][1][ 0][ 0] =  -1.074627e+00; eval[2][0][1][ 0][ 0] =   8.855081e-02;
  val[2][0][1][ 0][ 1] =  -1.391378e+00; eval[2][0][1][ 0][ 1] =   1.887595e-02;
  val[2][0][1][ 0][ 2] =  -1.635204e+00; eval[2][0][1][ 0][ 2] =   1.176333e-02;
  val[2][0][1][ 0][ 3] =  -1.695310e+00; eval[2][0][1][ 0][ 3] =   1.447182e-02;
  val[2][0][1][ 0][ 4] =  -1.912358e+00; eval[2][0][1][ 0][ 4] =   1.887352e-02;
  val[2][0][1][ 0][ 5] =  -2.162111e+00; eval[2][0][1][ 0][ 5] =   2.433096e-02;
  val[2][0][1][ 0][ 6] =  -2.355030e+00; eval[2][0][1][ 0][ 6] =   1.509170e-02;
  val[2][0][1][ 0][ 7] =  -2.465297e+00; eval[2][0][1][ 0][ 7] =   2.012840e-02;
  val[2][0][1][ 0][ 8] =  -2.588264e+00; eval[2][0][1][ 0][ 8] =   2.815905e-02;
  val[2][0][1][ 0][ 9] =  -2.652419e+00; eval[2][0][1][ 0][ 9] =   3.695039e-02;
  val[2][0][1][ 0][10] =  -2.705735e+00; eval[2][0][1][ 0][10] =   9.885452e-03;
  val[2][0][1][ 0][11] =  -2.743261e+00; eval[2][0][1][ 0][11] =   1.284197e-02;
  val[2][0][1][ 0][12] =  -2.831037e+00; eval[2][0][1][ 0][12] =   1.751653e-02;
  val[2][0][1][ 0][13] =  -2.837413e+00; eval[2][0][1][ 0][13] =   2.150751e-02;
  val[2][0][1][ 0][14] =  -2.924036e+00; eval[2][0][1][ 0][14] =   2.979661e-02;
  val[2][0][1][ 0][15] =  -2.972999e+00; eval[2][0][1][ 0][15] =   3.503973e-02;
  val[2][0][1][ 0][16] =  -2.943283e+00; eval[2][0][1][ 0][16] =   4.256599e-02;
  val[2][0][1][ 0][17] =  -3.051307e+00; eval[2][0][1][ 0][17] =   5.103323e-02;
  val[2][0][1][ 0][18] =  -2.798843e+00; eval[2][0][1][ 0][18] =   5.019277e-02;
  val[2][0][1][ 0][19] =  -2.925092e+00; eval[2][0][1][ 0][19] =   6.697363e-02;
  val[2][0][1][ 0][20] =  -2.971897e+00; eval[2][0][1][ 0][20] =   1.079315e-01;
  val[2][0][1][ 0][21] =  -2.804033e+00; eval[2][0][1][ 0][21] =   1.247678e-01;
  val[2][0][1][ 0][22] =  -2.835372e+00; eval[2][0][1][ 0][22] =   2.352990e-01;
  val[2][0][1][ 0][23] =  -3.080663e+00; eval[2][0][1][ 0][23] =   2.032597e-01;
  val[2][0][1][ 0][24] =  -2.817991e+00; eval[2][0][1][ 0][24] =   3.055161e-01;
  val[2][0][1][ 0][25] =  -3.297898e+00; eval[2][0][1][ 0][25] =   2.792341e-01;
  val[2][0][1][ 0][26] =  -1.898600e+00; eval[2][0][1][ 0][26] =   1.311402e+00;
  val[2][0][1][ 0][27] =  -2.988867e+00; eval[2][0][1][ 0][27] =   1.170937e+00;
  //emcdz_mean_c0d1z1
  n_val[2][0][1][1] = 28;
  val[2][0][1][ 1][ 0] =  -8.153460e-01; eval[2][0][1][ 1][ 0] =   7.694223e-02;
  val[2][0][1][ 1][ 1] =  -1.146605e+00; eval[2][0][1][ 1][ 1] =   1.580842e-02;
  val[2][0][1][ 1][ 2] =  -1.352969e+00; eval[2][0][1][ 1][ 2] =   9.619196e-03;
  val[2][0][1][ 1][ 3] =  -1.449931e+00; eval[2][0][1][ 1][ 3] =   1.192400e-02;
  val[2][0][1][ 1][ 4] =  -1.607201e+00; eval[2][0][1][ 1][ 4] =   1.518199e-02;
  val[2][0][1][ 1][ 5] =  -1.830573e+00; eval[2][0][1][ 1][ 5] =   2.003659e-02;
  val[2][0][1][ 1][ 6] =  -1.958980e+00; eval[2][0][1][ 1][ 6] =   1.232644e-02;
  val[2][0][1][ 1][ 7] =  -2.042819e+00; eval[2][0][1][ 1][ 7] =   1.651336e-02;
  val[2][0][1][ 1][ 8] =  -2.161327e+00; eval[2][0][1][ 1][ 8] =   2.261578e-02;
  val[2][0][1][ 1][ 9] =  -2.220044e+00; eval[2][0][1][ 1][ 9] =   2.984126e-02;
  val[2][0][1][ 1][10] =  -2.280025e+00; eval[2][0][1][ 1][10] =   7.852034e-03;
  val[2][0][1][ 1][11] =  -2.322287e+00; eval[2][0][1][ 1][11] =   1.018395e-02;
  val[2][0][1][ 1][12] =  -2.349810e+00; eval[2][0][1][ 1][12] =   1.331472e-02;
  val[2][0][1][ 1][13] =  -2.372024e+00; eval[2][0][1][ 1][13] =   1.691885e-02;
  val[2][0][1][ 1][14] =  -2.338043e+00; eval[2][0][1][ 1][14] =   2.145359e-02;
  val[2][0][1][ 1][15] =  -2.479837e+00; eval[2][0][1][ 1][15] =   2.608096e-02;
  val[2][0][1][ 1][16] =  -2.452571e+00; eval[2][0][1][ 1][16] =   3.372477e-02;
  val[2][0][1][ 1][17] =  -2.496124e+00; eval[2][0][1][ 1][17] =   4.117476e-02;
  val[2][0][1][ 1][18] =  -2.417187e+00; eval[2][0][1][ 1][18] =   4.031359e-02;
  val[2][0][1][ 1][19] =  -2.478313e+00; eval[2][0][1][ 1][19] =   5.459771e-02;
  val[2][0][1][ 1][20] =  -2.461673e+00; eval[2][0][1][ 1][20] =   7.994368e-02;
  val[2][0][1][ 1][21] =  -2.360187e+00; eval[2][0][1][ 1][21] =   9.156033e-02;
  val[2][0][1][ 1][22] =  -2.483812e+00; eval[2][0][1][ 1][22] =   1.325602e-01;
  val[2][0][1][ 1][23] =  -2.669019e+00; eval[2][0][1][ 1][23] =   1.378896e-01;
  val[2][0][1][ 1][24] =  -2.676879e+00; eval[2][0][1][ 1][24] =   2.280144e-01;
  val[2][0][1][ 1][25] =  -2.494081e+00; eval[2][0][1][ 1][25] =   3.423061e-01;
  val[2][0][1][ 1][26] =  -2.027727e+00; eval[2][0][1][ 1][26] =   3.018180e-01;
  val[2][0][1][ 1][27] =   4.749062e+02; eval[2][0][1][ 1][27] =   1.414214e+00;
  //emcdz_mean_c0d1z2
  n_val[2][0][1][2] = 28;
  val[2][0][1][ 2][ 0] =  -6.176983e-01; eval[2][0][1][ 2][ 0] =   7.418835e-02;
  val[2][0][1][ 2][ 1] =  -8.390135e-01; eval[2][0][1][ 2][ 1] =   1.508452e-02;
  val[2][0][1][ 2][ 2] =  -1.025385e+00; eval[2][0][1][ 2][ 2] =   8.712371e-03;
  val[2][0][1][ 2][ 3] =  -1.106710e+00; eval[2][0][1][ 2][ 3] =   1.082182e-02;
  val[2][0][1][ 2][ 4] =  -1.222319e+00; eval[2][0][1][ 2][ 4] =   1.341246e-02;
  val[2][0][1][ 2][ 5] =  -1.396281e+00; eval[2][0][1][ 2][ 5] =   1.747642e-02;
  val[2][0][1][ 2][ 6] =  -1.503438e+00; eval[2][0][1][ 2][ 6] =   1.085976e-02;
  val[2][0][1][ 2][ 7] =  -1.572649e+00; eval[2][0][1][ 2][ 7] =   1.447970e-02;
  val[2][0][1][ 2][ 8] =  -1.615357e+00; eval[2][0][1][ 2][ 8] =   1.922670e-02;
  val[2][0][1][ 2][ 9] =  -1.646257e+00; eval[2][0][1][ 2][ 9] =   2.547777e-02;
  val[2][0][1][ 2][10] =  -1.680623e+00; eval[2][0][1][ 2][10] =   6.738756e-03;
  val[2][0][1][ 2][11] =  -1.721837e+00; eval[2][0][1][ 2][11] =   8.756741e-03;
  val[2][0][1][ 2][12] =  -1.793963e+00; eval[2][0][1][ 2][12] =   1.149059e-02;
  val[2][0][1][ 2][13] =  -1.754692e+00; eval[2][0][1][ 2][13] =   1.475856e-02;
  val[2][0][1][ 2][14] =  -1.763644e+00; eval[2][0][1][ 2][14] =   1.795421e-02;
  val[2][0][1][ 2][15] =  -1.819784e+00; eval[2][0][1][ 2][15] =   2.294715e-02;
  val[2][0][1][ 2][16] =  -1.805858e+00; eval[2][0][1][ 2][16] =   2.884874e-02;
  val[2][0][1][ 2][17] =  -1.784646e+00; eval[2][0][1][ 2][17] =   3.520501e-02;
  val[2][0][1][ 2][18] =  -1.856301e+00; eval[2][0][1][ 2][18] =   3.307684e-02;
  val[2][0][1][ 2][19] =  -1.837977e+00; eval[2][0][1][ 2][19] =   4.702677e-02;
  val[2][0][1][ 2][20] =  -1.883291e+00; eval[2][0][1][ 2][20] =   6.606045e-02;
  val[2][0][1][ 2][21] =  -1.836411e+00; eval[2][0][1][ 2][21] =   7.518981e-02;
  val[2][0][1][ 2][22] =  -1.779185e+00; eval[2][0][1][ 2][22] =   1.067791e-01;
  val[2][0][1][ 2][23] =  -1.802428e+00; eval[2][0][1][ 2][23] =   1.260565e-01;
  val[2][0][1][ 2][24] =  -1.946551e+00; eval[2][0][1][ 2][24] =   1.848017e-01;
  val[2][0][1][ 2][25] =  -1.844370e+00; eval[2][0][1][ 2][25] =   1.903824e-01;
  val[2][0][1][ 2][26] =  -2.064271e+00; eval[2][0][1][ 2][26] =   3.260834e-01;
  val[2][0][1][ 2][27] =  -1.671485e+00; eval[2][0][1][ 2][27] =   4.218131e-01;
  //emcdz_mean_c0d1z3
  n_val[2][0][1][3] = 28;
  val[2][0][1][ 3][ 0] =  -3.987915e-01; eval[2][0][1][ 3][ 0] =   7.292903e-02;
  val[2][0][1][ 3][ 1] =  -5.726234e-01; eval[2][0][1][ 3][ 1] =   1.456898e-02;
  val[2][0][1][ 3][ 2] =  -6.592900e-01; eval[2][0][1][ 3][ 2] =   8.336063e-03;
  val[2][0][1][ 3][ 3] =  -6.979553e-01; eval[2][0][1][ 3][ 3] =   1.006384e-02;
  val[2][0][1][ 3][ 4] =  -7.644121e-01; eval[2][0][1][ 3][ 4] =   1.231575e-02;
  val[2][0][1][ 3][ 5] =  -8.852992e-01; eval[2][0][1][ 3][ 5] =   1.640920e-02;
  val[2][0][1][ 3][ 6] =  -9.409351e-01; eval[2][0][1][ 3][ 6] =   1.000083e-02;
  val[2][0][1][ 3][ 7] =  -9.884955e-01; eval[2][0][1][ 3][ 7] =   1.344838e-02;
  val[2][0][1][ 3][ 8] =  -1.019674e+00; eval[2][0][1][ 3][ 8] =   1.773247e-02;
  val[2][0][1][ 3][ 9] =  -1.029475e+00; eval[2][0][1][ 3][ 9] =   2.353659e-02;
  val[2][0][1][ 3][10] =  -1.063470e+00; eval[2][0][1][ 3][10] =   6.201037e-03;
  val[2][0][1][ 3][11] =  -1.077331e+00; eval[2][0][1][ 3][11] =   8.072290e-03;
  val[2][0][1][ 3][12] =  -1.095448e+00; eval[2][0][1][ 3][12] =   1.049543e-02;
  val[2][0][1][ 3][13] =  -1.091950e+00; eval[2][0][1][ 3][13] =   1.289862e-02;
  val[2][0][1][ 3][14] =  -1.165465e+00; eval[2][0][1][ 3][14] =   1.687357e-02;
  val[2][0][1][ 3][15] =  -1.117043e+00; eval[2][0][1][ 3][15] =   2.066097e-02;
  val[2][0][1][ 3][16] =  -1.108230e+00; eval[2][0][1][ 3][16] =   2.458496e-02;
  val[2][0][1][ 3][17] =  -1.035279e+00; eval[2][0][1][ 3][17] =   3.172740e-02;
  val[2][0][1][ 3][18] =  -1.182403e+00; eval[2][0][1][ 3][18] =   2.815753e-02;
  val[2][0][1][ 3][19] =  -1.273829e+00; eval[2][0][1][ 3][19] =   4.190887e-02;
  val[2][0][1][ 3][20] =  -1.215895e+00; eval[2][0][1][ 3][20] =   5.766743e-02;
  val[2][0][1][ 3][21] =  -1.107819e+00; eval[2][0][1][ 3][21] =   7.813037e-02;
  val[2][0][1][ 3][22] =  -1.111070e+00; eval[2][0][1][ 3][22] =   9.148258e-02;
  val[2][0][1][ 3][23] =  -1.108515e+00; eval[2][0][1][ 3][23] =   1.332754e-01;
  val[2][0][1][ 3][24] =  -7.081642e-01; eval[2][0][1][ 3][24] =   1.396171e-01;
  val[2][0][1][ 3][25] =  -2.913701e-01; eval[2][0][1][ 3][25] =   2.067863e-01;
  val[2][0][1][ 3][26] =  -1.038930e+00; eval[2][0][1][ 3][26] =   3.961329e-01;
  val[2][0][1][ 3][27] =  -1.663223e+00; eval[2][0][1][ 3][27] =   2.814481e-01;
  //emcdz_mean_c0d1z4
  n_val[2][0][1][4] = 28;
  val[2][0][1][ 4][ 0] =  -2.882696e-01; eval[2][0][1][ 4][ 0] =   8.813945e-02;
  val[2][0][1][ 4][ 1] =  -3.341828e-01; eval[2][0][1][ 4][ 1] =   1.686326e-02;
  val[2][0][1][ 4][ 2] =  -4.062086e-01; eval[2][0][1][ 4][ 2] =   9.595876e-03;
  val[2][0][1][ 4][ 3] =  -4.387376e-01; eval[2][0][1][ 4][ 3] =   1.134916e-02;
  val[2][0][1][ 4][ 4] =  -4.868062e-01; eval[2][0][1][ 4][ 4] =   1.432804e-02;
  val[2][0][1][ 4][ 5] =  -5.190642e-01; eval[2][0][1][ 4][ 5] =   1.822284e-02;
  val[2][0][1][ 4][ 6] =  -5.278226e-01; eval[2][0][1][ 4][ 6] =   1.124869e-02;
  val[2][0][1][ 4][ 7] =  -5.506308e-01; eval[2][0][1][ 4][ 7] =   1.524228e-02;
  val[2][0][1][ 4][ 8] =  -5.742898e-01; eval[2][0][1][ 4][ 8] =   2.031879e-02;
  val[2][0][1][ 4][ 9] =  -5.868672e-01; eval[2][0][1][ 4][ 9] =   2.710167e-02;
  val[2][0][1][ 4][10] =  -5.955374e-01; eval[2][0][1][ 4][10] =   7.224700e-03;
  val[2][0][1][ 4][11] =  -5.943755e-01; eval[2][0][1][ 4][11] =   9.212302e-03;
  val[2][0][1][ 4][12] =  -6.364821e-01; eval[2][0][1][ 4][12] =   1.234073e-02;
  val[2][0][1][ 4][13] =  -6.209308e-01; eval[2][0][1][ 4][13] =   1.551321e-02;
  val[2][0][1][ 4][14] =  -6.199499e-01; eval[2][0][1][ 4][14] =   1.983015e-02;
  val[2][0][1][ 4][15] =  -6.945554e-01; eval[2][0][1][ 4][15] =   2.529534e-02;
  val[2][0][1][ 4][16] =  -6.418140e-01; eval[2][0][1][ 4][16] =   3.019539e-02;
  val[2][0][1][ 4][17] =  -6.394943e-01; eval[2][0][1][ 4][17] =   3.761052e-02;
  val[2][0][1][ 4][18] =  -6.418754e-01; eval[2][0][1][ 4][18] =   3.468888e-02;
  val[2][0][1][ 4][19] =  -5.523222e-01; eval[2][0][1][ 4][19] =   4.824345e-02;
  val[2][0][1][ 4][20] =  -3.534372e-01; eval[2][0][1][ 4][20] =   7.079084e-02;
  val[2][0][1][ 4][21] =  -6.257170e-01; eval[2][0][1][ 4][21] =   8.114834e-02;
  val[2][0][1][ 4][22] =  -4.304623e-01; eval[2][0][1][ 4][22] =   1.176261e-01;
  val[2][0][1][ 4][23] =  -7.318275e-01; eval[2][0][1][ 4][23] =   1.452374e-01;
  val[2][0][1][ 4][24] =  -5.964415e-01; eval[2][0][1][ 4][24] =   1.621700e-01;
  val[2][0][1][ 4][25] =  -5.920087e-01; eval[2][0][1][ 4][25] =   2.636372e-01;
  val[2][0][1][ 4][26] =  -1.054037e+00; eval[2][0][1][ 4][26] =   4.254716e-01;
  val[2][0][1][ 4][27] =  -4.353682e-01; eval[2][0][1][ 4][27] =   4.427401e-01;
  //emcdz_mean_c0d1z5
  n_val[2][0][1][5] = 28;
  val[2][0][1][ 5][ 0] =  -1.307271e-01; eval[2][0][1][ 5][ 0] =   8.939025e-02;
  val[2][0][1][ 5][ 1] =  -3.325474e-03; eval[2][0][1][ 5][ 1] =   1.656311e-02;
  val[2][0][1][ 5][ 2] =   2.658896e-02; eval[2][0][1][ 5][ 2] =   9.730740e-03;
  val[2][0][1][ 5][ 3] =  -1.016280e-03; eval[2][0][1][ 5][ 3] =   1.152581e-02;
  val[2][0][1][ 5][ 4] =  -1.432655e-03; eval[2][0][1][ 5][ 4] =   1.439682e-02;
  val[2][0][1][ 5][ 5] =   8.805424e-03; eval[2][0][1][ 5][ 5] =   1.869140e-02;
  val[2][0][1][ 5][ 6] =   2.590117e-02; eval[2][0][1][ 5][ 6] =   1.156526e-02;
  val[2][0][1][ 5][ 7] =   3.957224e-02; eval[2][0][1][ 5][ 7] =   1.532780e-02;
  val[2][0][1][ 5][ 8] =   5.817567e-02; eval[2][0][1][ 5][ 8] =   2.033973e-02;
  val[2][0][1][ 5][ 9] =   7.758750e-02; eval[2][0][1][ 5][ 9] =   2.680749e-02;
  val[2][0][1][ 5][10] =   7.123330e-02; eval[2][0][1][ 5][10] =   7.129194e-03;
  val[2][0][1][ 5][11] =   9.981927e-02; eval[2][0][1][ 5][11] =   9.348661e-03;
  val[2][0][1][ 5][12] =   1.040340e-01; eval[2][0][1][ 5][12] =   1.186310e-02;
  val[2][0][1][ 5][13] =   1.105572e-01; eval[2][0][1][ 5][13] =   1.495896e-02;
  val[2][0][1][ 5][14] =   1.090443e-01; eval[2][0][1][ 5][14] =   1.904934e-02;
  val[2][0][1][ 5][15] =   9.507368e-02; eval[2][0][1][ 5][15] =   2.324176e-02;
  val[2][0][1][ 5][16] =   1.183005e-01; eval[2][0][1][ 5][16] =   2.866683e-02;
  val[2][0][1][ 5][17] =   1.201698e-01; eval[2][0][1][ 5][17] =   3.415001e-02;
  val[2][0][1][ 5][18] =   7.424933e-02; eval[2][0][1][ 5][18] =   3.302081e-02;
  val[2][0][1][ 5][19] =   9.415282e-02; eval[2][0][1][ 5][19] =   4.743197e-02;
  val[2][0][1][ 5][20] =   1.657426e-02; eval[2][0][1][ 5][20] =   5.498043e-02;
  val[2][0][1][ 5][21] =   2.835806e-01; eval[2][0][1][ 5][21] =   9.212587e-02;
  val[2][0][1][ 5][22] =   2.122513e-01; eval[2][0][1][ 5][22] =   9.792718e-02;
  val[2][0][1][ 5][23] =   2.484093e-01; eval[2][0][1][ 5][23] =   1.506315e-01;
  val[2][0][1][ 5][24] =  -1.590830e-02; eval[2][0][1][ 5][24] =   1.495766e-01;
  val[2][0][1][ 5][25] =   5.281087e-02; eval[2][0][1][ 5][25] =   2.878204e-01;
  val[2][0][1][ 5][26] =   9.609357e-01; eval[2][0][1][ 5][26] =   1.265403e+00;
  val[2][0][1][ 5][27] =  -7.995597e-01; eval[2][0][1][ 5][27] =   8.464718e-02;
  //emcdz_mean_c0d1z6
  n_val[2][0][1][6] = 28;
  val[2][0][1][ 6][ 0] =  -3.308047e-02; eval[2][0][1][ 6][ 0] =   7.695600e-02;
  val[2][0][1][ 6][ 1] =   1.477651e-01; eval[2][0][1][ 6][ 1] =   1.375850e-02;
  val[2][0][1][ 6][ 2] =   2.337980e-01; eval[2][0][1][ 6][ 2] =   8.044372e-03;
  val[2][0][1][ 6][ 3] =   2.426496e-01; eval[2][0][1][ 6][ 3] =   9.478492e-03;
  val[2][0][1][ 6][ 4] =   2.942588e-01; eval[2][0][1][ 6][ 4] =   1.201075e-02;
  val[2][0][1][ 6][ 5] =   3.973414e-01; eval[2][0][1][ 6][ 5] =   1.549629e-02;
  val[2][0][1][ 6][ 6] =   4.506455e-01; eval[2][0][1][ 6][ 6] =   9.476299e-03;
  val[2][0][1][ 6][ 7] =   4.922005e-01; eval[2][0][1][ 6][ 7] =   1.257455e-02;
  val[2][0][1][ 6][ 8] =   5.224870e-01; eval[2][0][1][ 6][ 8] =   1.664455e-02;
  val[2][0][1][ 6][ 9] =   5.497372e-01; eval[2][0][1][ 6][ 9] =   2.197883e-02;
  val[2][0][1][ 6][10] =   5.594322e-01; eval[2][0][1][ 6][10] =   5.841856e-03;
  val[2][0][1][ 6][11] =   5.646491e-01; eval[2][0][1][ 6][11] =   7.628511e-03;
  val[2][0][1][ 6][12] =   6.087480e-01; eval[2][0][1][ 6][12] =   1.003640e-02;
  val[2][0][1][ 6][13] =   6.038336e-01; eval[2][0][1][ 6][13] =   1.250488e-02;
  val[2][0][1][ 6][14] =   6.301616e-01; eval[2][0][1][ 6][14] =   1.579628e-02;
  val[2][0][1][ 6][15] =   5.997634e-01; eval[2][0][1][ 6][15] =   1.963727e-02;
  val[2][0][1][ 6][16] =   6.808139e-01; eval[2][0][1][ 6][16] =   2.393761e-02;
  val[2][0][1][ 6][17] =   6.313677e-01; eval[2][0][1][ 6][17] =   3.031676e-02;
  val[2][0][1][ 6][18] =   7.062699e-01; eval[2][0][1][ 6][18] =   2.927388e-02;
  val[2][0][1][ 6][19] =   6.457852e-01; eval[2][0][1][ 6][19] =   3.940677e-02;
  val[2][0][1][ 6][20] =   7.126920e-01; eval[2][0][1][ 6][20] =   5.411044e-02;
  val[2][0][1][ 6][21] =   6.887574e-01; eval[2][0][1][ 6][21] =   7.033128e-02;
  val[2][0][1][ 6][22] =   6.306972e-01; eval[2][0][1][ 6][22] =   9.091097e-02;
  val[2][0][1][ 6][23] =   7.292934e-01; eval[2][0][1][ 6][23] =   1.086951e-01;
  val[2][0][1][ 6][24] =   7.253103e-01; eval[2][0][1][ 6][24] =   1.458476e-01;
  val[2][0][1][ 6][25] =   6.670217e-01; eval[2][0][1][ 6][25] =   1.535555e-01;
  val[2][0][1][ 6][26] =   4.601557e-01; eval[2][0][1][ 6][26] =   2.625529e-01;
  val[2][0][1][ 6][27] =   1.487530e+00; eval[2][0][1][ 6][27] =   1.184872e+00;
  //emcdz_mean_c0d1z7
  n_val[2][0][1][7] = 28;
  val[2][0][1][ 7][ 0] =   1.911505e-01; eval[2][0][1][ 7][ 0] =   7.653864e-02;
  val[2][0][1][ 7][ 1] =   4.908544e-01; eval[2][0][1][ 7][ 1] =   1.469833e-02;
  val[2][0][1][ 7][ 2] =   6.450245e-01; eval[2][0][1][ 7][ 2] =   8.614527e-03;
  val[2][0][1][ 7][ 3] =   6.916167e-01; eval[2][0][1][ 7][ 3] =   1.049428e-02;
  val[2][0][1][ 7][ 4] =   7.601112e-01; eval[2][0][1][ 7][ 4] =   1.300523e-02;
  val[2][0][1][ 7][ 5] =   9.237646e-01; eval[2][0][1][ 7][ 5] =   1.712939e-02;
  val[2][0][1][ 7][ 6] =   1.004336e+00; eval[2][0][1][ 7][ 6] =   1.049090e-02;
  val[2][0][1][ 7][ 7] =   1.069625e+00; eval[2][0][1][ 7][ 7] =   1.395130e-02;
  val[2][0][1][ 7][ 8] =   1.112864e+00; eval[2][0][1][ 7][ 8] =   1.854888e-02;
  val[2][0][1][ 7][ 9] =   1.157331e+00; eval[2][0][1][ 7][ 9] =   2.482860e-02;
  val[2][0][1][ 7][10] =   1.173753e+00; eval[2][0][1][ 7][10] =   6.564331e-03;
  val[2][0][1][ 7][11] =   1.206597e+00; eval[2][0][1][ 7][11] =   8.461210e-03;
  val[2][0][1][ 7][12] =   1.261497e+00; eval[2][0][1][ 7][12] =   1.123578e-02;
  val[2][0][1][ 7][13] =   1.249090e+00; eval[2][0][1][ 7][13] =   1.400450e-02;
  val[2][0][1][ 7][14] =   1.266685e+00; eval[2][0][1][ 7][14] =   1.790344e-02;
  val[2][0][1][ 7][15] =   1.274417e+00; eval[2][0][1][ 7][15] =   2.227662e-02;
  val[2][0][1][ 7][16] =   1.311886e+00; eval[2][0][1][ 7][16] =   2.681902e-02;
  val[2][0][1][ 7][17] =   1.351773e+00; eval[2][0][1][ 7][17] =   3.495354e-02;
  val[2][0][1][ 7][18] =   1.399792e+00; eval[2][0][1][ 7][18] =   2.938322e-02;
  val[2][0][1][ 7][19] =   1.332054e+00; eval[2][0][1][ 7][19] =   4.569483e-02;
  val[2][0][1][ 7][20] =   1.312856e+00; eval[2][0][1][ 7][20] =   5.555712e-02;
  val[2][0][1][ 7][21] =   1.570782e+00; eval[2][0][1][ 7][21] =   8.252367e-02;
  val[2][0][1][ 7][22] =   1.482984e+00; eval[2][0][1][ 7][22] =   1.028309e-01;
  val[2][0][1][ 7][23] =   1.534714e+00; eval[2][0][1][ 7][23] =   1.541521e-01;
  val[2][0][1][ 7][24] =   1.584817e+00; eval[2][0][1][ 7][24] =   1.438185e-01;
  val[2][0][1][ 7][25] =   1.765287e+00; eval[2][0][1][ 7][25] =   2.530605e-01;
  val[2][0][1][ 7][26] =   1.288292e+00; eval[2][0][1][ 7][26] =   3.488439e-01;
  val[2][0][1][ 7][27] =   1.522924e+00; eval[2][0][1][ 7][27] =   3.724567e-01;
  //emcdz_mean_c0d1z8
  n_val[2][0][1][8] = 28;
  val[2][0][1][ 8][ 0] =   4.934980e-01; eval[2][0][1][ 8][ 0] =   7.652377e-02;
  val[2][0][1][ 8][ 1] =   8.984239e-01; eval[2][0][1][ 8][ 1] =   1.544013e-02;
  val[2][0][1][ 8][ 2] =   1.060495e+00; eval[2][0][1][ 8][ 2] =   9.244778e-03;
  val[2][0][1][ 8][ 3] =   1.154033e+00; eval[2][0][1][ 8][ 3] =   1.135137e-02;
  val[2][0][1][ 8][ 4] =   1.294004e+00; eval[2][0][1][ 8][ 4] =   1.420205e-02;
  val[2][0][1][ 8][ 5] =   1.455922e+00; eval[2][0][1][ 8][ 5] =   1.878273e-02;
  val[2][0][1][ 8][ 6] =   1.574824e+00; eval[2][0][1][ 8][ 6] =   1.177430e-02;
  val[2][0][1][ 8][ 7] =   1.692576e+00; eval[2][0][1][ 8][ 7] =   1.590651e-02;
  val[2][0][1][ 8][ 8] =   1.754141e+00; eval[2][0][1][ 8][ 8] =   2.138193e-02;
  val[2][0][1][ 8][ 9] =   1.823122e+00; eval[2][0][1][ 8][ 9] =   2.844364e-02;
  val[2][0][1][ 8][10] =   1.855890e+00; eval[2][0][1][ 8][10] =   7.497686e-03;
  val[2][0][1][ 8][11] =   1.903520e+00; eval[2][0][1][ 8][11] =   1.000392e-02;
  val[2][0][1][ 8][12] =   1.990263e+00; eval[2][0][1][ 8][12] =   1.309372e-02;
  val[2][0][1][ 8][13] =   2.003647e+00; eval[2][0][1][ 8][13] =   1.646575e-02;
  val[2][0][1][ 8][14] =   2.087926e+00; eval[2][0][1][ 8][14] =   2.130755e-02;
  val[2][0][1][ 8][15] =   2.094658e+00; eval[2][0][1][ 8][15] =   2.640122e-02;
  val[2][0][1][ 8][16] =   2.060888e+00; eval[2][0][1][ 8][16] =   3.310243e-02;
  val[2][0][1][ 8][17] =   2.052634e+00; eval[2][0][1][ 8][17] =   4.021007e-02;
  val[2][0][1][ 8][18] =   2.072929e+00; eval[2][0][1][ 8][18] =   3.665815e-02;
  val[2][0][1][ 8][19] =   2.156378e+00; eval[2][0][1][ 8][19] =   4.710649e-02;
  val[2][0][1][ 8][20] =   2.090340e+00; eval[2][0][1][ 8][20] =   6.851450e-02;
  val[2][0][1][ 8][21] =   2.052715e+00; eval[2][0][1][ 8][21] =   9.675440e-02;
  val[2][0][1][ 8][22] =   2.143006e+00; eval[2][0][1][ 8][22] =   1.330503e-01;
  val[2][0][1][ 8][23] =   1.910150e+00; eval[2][0][1][ 8][23] =   1.747813e-01;
  val[2][0][1][ 8][24] =   1.182896e+00; eval[2][0][1][ 8][24] =   1.613434e+00;
  val[2][0][1][ 8][25] =   9.641761e-01; eval[2][0][1][ 8][25] =   1.414208e+00;
  val[2][0][1][ 8][26] =   1.988316e+00; eval[2][0][1][ 8][26] =   3.216749e-01;
  val[2][0][1][ 8][27] =   1.256608e+00; eval[2][0][1][ 8][27] =   4.511834e-01;
  //emcdz_mean_c0d1z9
  n_val[2][0][1][9] = 28;
  val[2][0][1][ 9][ 0] =   8.104166e-01; eval[2][0][1][ 9][ 0] =   9.537874e-02;
  val[2][0][1][ 9][ 1] =   1.235828e+00; eval[2][0][1][ 9][ 1] =   1.914346e-02;
  val[2][0][1][ 9][ 2] =   1.444014e+00; eval[2][0][1][ 9][ 2] =   1.146680e-02;
  val[2][0][1][ 9][ 3] =   1.512466e+00; eval[2][0][1][ 9][ 3] =   1.466983e-02;
  val[2][0][1][ 9][ 4] =   1.697012e+00; eval[2][0][1][ 9][ 4] =   1.888947e-02;
  val[2][0][1][ 9][ 5] =   1.904054e+00; eval[2][0][1][ 9][ 5] =   2.463814e-02;
  val[2][0][1][ 9][ 6] =   2.095552e+00; eval[2][0][1][ 9][ 6] =   1.588386e-02;
  val[2][0][1][ 9][ 7] =   2.189984e+00; eval[2][0][1][ 9][ 7] =   2.152104e-02;
  val[2][0][1][ 9][ 8] =   2.251294e+00; eval[2][0][1][ 9][ 8] =   2.953406e-02;
  val[2][0][1][ 9][ 9] =   2.306349e+00; eval[2][0][1][ 9][ 9] =   3.983286e-02;
  val[2][0][1][ 9][10] =   2.376589e+00; eval[2][0][1][ 9][10] =   1.042792e-02;
  val[2][0][1][ 9][11] =   2.446740e+00; eval[2][0][1][ 9][11] =   1.368508e-02;
  val[2][0][1][ 9][12] =   2.561843e+00; eval[2][0][1][ 9][12] =   1.847124e-02;
  val[2][0][1][ 9][13] =   2.490420e+00; eval[2][0][1][ 9][13] =   2.235334e-02;
  val[2][0][1][ 9][14] =   2.634433e+00; eval[2][0][1][ 9][14] =   2.778137e-02;
  val[2][0][1][ 9][15] =   2.533447e+00; eval[2][0][1][ 9][15] =   3.325407e-02;
  val[2][0][1][ 9][16] =   2.540653e+00; eval[2][0][1][ 9][16] =   4.465357e-02;
  val[2][0][1][ 9][17] =   2.642190e+00; eval[2][0][1][ 9][17] =   5.312228e-02;
  val[2][0][1][ 9][18] =   2.671964e+00; eval[2][0][1][ 9][18] =   4.911255e-02;
  val[2][0][1][ 9][19] =   2.608010e+00; eval[2][0][1][ 9][19] =   7.644451e-02;
  val[2][0][1][ 9][20] =   2.535195e+00; eval[2][0][1][ 9][20] =   1.002581e-01;
  val[2][0][1][ 9][21] =   2.623374e+00; eval[2][0][1][ 9][21] =   1.324371e-01;
  val[2][0][1][ 9][22] =   2.799892e+00; eval[2][0][1][ 9][22] =   1.662667e-01;
  val[2][0][1][ 9][23] =   3.119695e+00; eval[2][0][1][ 9][23] =   1.786609e-01;
  val[2][0][1][ 9][24] =   3.306949e+00; eval[2][0][1][ 9][24] =   1.743912e-01;
  val[2][0][1][ 9][25] =   2.829079e+00; eval[2][0][1][ 9][25] =   3.647187e-01;
  val[2][0][1][ 9][26] =   3.088456e+00; eval[2][0][1][ 9][26] =   6.173001e-01;
  val[2][0][1][ 9][27] =   2.882370e+00; eval[2][0][1][ 9][27] =   1.325451e+00;
  //emcdz_mean_c1d0z0
  n_val[2][1][0][0] = 28;
  val[2][1][0][ 0][ 0] =   1.169333e+00; eval[2][1][0][ 0][ 0] =   8.901284e-02;
  val[2][1][0][ 0][ 1] =   6.538132e-01; eval[2][1][0][ 0][ 1] =   1.956688e-02;
  val[2][1][0][ 0][ 2] =   4.081647e-01; eval[2][1][0][ 0][ 2] =   1.256532e-02;
  val[2][1][0][ 0][ 3] =   2.907921e-01; eval[2][1][0][ 0][ 3] =   1.654577e-02;
  val[2][1][0][ 0][ 4] =   1.279895e-01; eval[2][1][0][ 0][ 4] =   2.054736e-02;
  val[2][1][0][ 0][ 5] =  -1.038219e-01; eval[2][1][0][ 0][ 5] =   2.632790e-02;
  val[2][1][0][ 0][ 6] =  -2.931259e-01; eval[2][1][0][ 0][ 6] =   1.614134e-02;
  val[2][1][0][ 0][ 7] =  -4.146819e-01; eval[2][1][0][ 0][ 7] =   2.098845e-02;
  val[2][1][0][ 0][ 8] =  -5.398716e-01; eval[2][1][0][ 0][ 8] =   2.794229e-02;
  val[2][1][0][ 0][ 9] =  -6.363668e-01; eval[2][1][0][ 0][ 9] =   3.612870e-02;
  val[2][1][0][ 0][10] =  -6.944495e-01; eval[2][1][0][ 0][10] =   9.341417e-03;
  val[2][1][0][ 0][11] =  -7.390192e-01; eval[2][1][0][ 0][11] =   1.213106e-02;
  val[2][1][0][ 0][12] =  -8.709387e-01; eval[2][1][0][ 0][12] =   1.580761e-02;
  val[2][1][0][ 0][13] =  -8.833614e-01; eval[2][1][0][ 0][13] =   1.985950e-02;
  val[2][1][0][ 0][14] =  -8.953692e-01; eval[2][1][0][ 0][14] =   2.474872e-02;
  val[2][1][0][ 0][15] =  -8.970769e-01; eval[2][1][0][ 0][15] =   3.063977e-02;
  val[2][1][0][ 0][16] =  -9.094997e-01; eval[2][1][0][ 0][16] =   3.746852e-02;
  val[2][1][0][ 0][17] =  -8.924685e-01; eval[2][1][0][ 0][17] =   4.825679e-02;
  val[2][1][0][ 0][18] =  -1.003181e+00; eval[2][1][0][ 0][18] =   4.194907e-02;
  val[2][1][0][ 0][19] =  -1.042310e+00; eval[2][1][0][ 0][19] =   5.863335e-02;
  val[2][1][0][ 0][20] =  -1.047676e+00; eval[2][1][0][ 0][20] =   8.237447e-02;
  val[2][1][0][ 0][21] =  -1.135175e+00; eval[2][1][0][ 0][21] =   1.161782e-01;
  val[2][1][0][ 0][22] =  -1.185503e+00; eval[2][1][0][ 0][22] =   1.188804e-01;
  val[2][1][0][ 0][23] =  -1.285556e+00; eval[2][1][0][ 0][23] =   1.906479e-01;
  val[2][1][0][ 0][24] =  -7.801707e-01; eval[2][1][0][ 0][24] =   2.825296e-01;
  val[2][1][0][ 0][25] =  -8.254032e-01; eval[2][1][0][ 0][25] =   4.253451e-01;
  val[2][1][0][ 0][26] =  -9.589887e-01; eval[2][1][0][ 0][26] =   4.734892e-01;
  val[2][1][0][ 0][27] =  -6.995713e-01; eval[2][1][0][ 0][27] =   3.900067e-01;
  //emcdz_mean_c1d0z1
  n_val[2][1][0][1] = 28;
  val[2][1][0][ 1][ 0] =   1.359553e+00; eval[2][1][0][ 1][ 0] =   8.210477e-02;
  val[2][1][0][ 1][ 1] =   9.562037e-01; eval[2][1][0][ 1][ 1] =   1.711308e-02;
  val[2][1][0][ 1][ 2] =   7.589150e-01; eval[2][1][0][ 1][ 2] =   1.155365e-02;
  val[2][1][0][ 1][ 3] =   6.870464e-01; eval[2][1][0][ 1][ 3] =   1.407279e-02;
  val[2][1][0][ 1][ 4] =   5.502162e-01; eval[2][1][0][ 1][ 4] =   1.746474e-02;
  val[2][1][0][ 1][ 5] =   3.451186e-01; eval[2][1][0][ 1][ 5] =   2.303575e-02;
  val[2][1][0][ 1][ 6] =   2.458055e-01; eval[2][1][0][ 1][ 6] =   1.406569e-02;
  val[2][1][0][ 1][ 7] =   1.089808e-01; eval[2][1][0][ 1][ 7] =   1.855605e-02;
  val[2][1][0][ 1][ 8] =   4.361662e-02; eval[2][1][0][ 1][ 8] =   2.441767e-02;
  val[2][1][0][ 1][ 9] =  -1.569807e-02; eval[2][1][0][ 1][ 9] =   3.166180e-02;
  val[2][1][0][ 1][10] =  -8.244685e-02; eval[2][1][0][ 1][10] =   8.181358e-03;
  val[2][1][0][ 1][11] =  -1.312717e-01; eval[2][1][0][ 1][11] =   1.063329e-02;
  val[2][1][0][ 1][12] =  -1.953637e-01; eval[2][1][0][ 1][12] =   1.364283e-02;
  val[2][1][0][ 1][13] =  -1.831025e-01; eval[2][1][0][ 1][13] =   1.774975e-02;
  val[2][1][0][ 1][14] =  -2.329810e-01; eval[2][1][0][ 1][14] =   2.146954e-02;
  val[2][1][0][ 1][15] =  -1.737821e-01; eval[2][1][0][ 1][15] =   2.786677e-02;
  val[2][1][0][ 1][16] =  -3.191866e-01; eval[2][1][0][ 1][16] =   3.225029e-02;
  val[2][1][0][ 1][17] =  -3.668343e-01; eval[2][1][0][ 1][17] =   3.998787e-02;
  val[2][1][0][ 1][18] =  -3.603240e-01; eval[2][1][0][ 1][18] =   3.711237e-02;
  val[2][1][0][ 1][19] =  -3.764766e-01; eval[2][1][0][ 1][19] =   4.846484e-02;
  val[2][1][0][ 1][20] =  -3.140468e-01; eval[2][1][0][ 1][20] =   7.442814e-02;
  val[2][1][0][ 1][21] =  -4.423582e-01; eval[2][1][0][ 1][21] =   8.777192e-02;
  val[2][1][0][ 1][22] =  -1.467334e-01; eval[2][1][0][ 1][22] =   1.000342e-01;
  val[2][1][0][ 1][23] =  -3.635786e-01; eval[2][1][0][ 1][23] =   1.345139e-01;
  val[2][1][0][ 1][24] =  -1.130464e-01; eval[2][1][0][ 1][24] =   2.115595e-01;
  val[2][1][0][ 1][25] =  -1.979395e-01; eval[2][1][0][ 1][25] =   2.171044e-01;
  val[2][1][0][ 1][26] =  -5.292475e-01; eval[2][1][0][ 1][26] =   3.446934e-01;
  val[2][1][0][ 1][27] =   2.777794e-01; eval[2][1][0][ 1][27] =   1.243091e+00;
  //emcdz_mean_c1d0z2
  n_val[2][1][0][2] = 28;
  val[2][1][0][ 2][ 0] =   1.708961e+00; eval[2][1][0][ 2][ 0] =   7.484099e-02;
  val[2][1][0][ 2][ 1] =   1.417012e+00; eval[2][1][0][ 2][ 1] =   1.617981e-02;
  val[2][1][0][ 2][ 2] =   1.309602e+00; eval[2][1][0][ 2][ 2] =   1.005155e-02;
  val[2][1][0][ 2][ 3] =   1.242858e+00; eval[2][1][0][ 2][ 3] =   1.233024e-02;
  val[2][1][0][ 2][ 4] =   1.147910e+00; eval[2][1][0][ 2][ 4] =   1.540788e-02;
  val[2][1][0][ 2][ 5] =   1.034626e+00; eval[2][1][0][ 2][ 5] =   2.042588e-02;
  val[2][1][0][ 2][ 6] =   9.298424e-01; eval[2][1][0][ 2][ 6] =   1.232770e-02;
  val[2][1][0][ 2][ 7] =   8.754321e-01; eval[2][1][0][ 2][ 7] =   1.615054e-02;
  val[2][1][0][ 2][ 8] =   8.150584e-01; eval[2][1][0][ 2][ 8] =   2.158066e-02;
  val[2][1][0][ 2][ 9] =   7.670934e-01; eval[2][1][0][ 2][ 9] =   2.801914e-02;
  val[2][1][0][ 2][10] =   7.355058e-01; eval[2][1][0][ 2][10] =   7.236332e-03;
  val[2][1][0][ 2][11] =   6.981058e-01; eval[2][1][0][ 2][11] =   9.334010e-03;
  val[2][1][0][ 2][12] =   6.774821e-01; eval[2][1][0][ 2][12] =   1.202836e-02;
  val[2][1][0][ 2][13] =   6.690014e-01; eval[2][1][0][ 2][13] =   1.539555e-02;
  val[2][1][0][ 2][14] =   6.562016e-01; eval[2][1][0][ 2][14] =   1.872377e-02;
  val[2][1][0][ 2][15] =   5.716714e-01; eval[2][1][0][ 2][15] =   2.365478e-02;
  val[2][1][0][ 2][16] =   5.853375e-01; eval[2][1][0][ 2][16] =   2.892028e-02;
  val[2][1][0][ 2][17] =   5.432455e-01; eval[2][1][0][ 2][17] =   3.520503e-02;
  val[2][1][0][ 2][18] =   5.174056e-01; eval[2][1][0][ 2][18] =   3.316185e-02;
  val[2][1][0][ 2][19] =   5.481114e-01; eval[2][1][0][ 2][19] =   4.631049e-02;
  val[2][1][0][ 2][20] =   4.674763e-01; eval[2][1][0][ 2][20] =   6.927036e-02;
  val[2][1][0][ 2][21] =   4.967873e-01; eval[2][1][0][ 2][21] =   7.022972e-02;
  val[2][1][0][ 2][22] =   6.563224e-01; eval[2][1][0][ 2][22] =   1.081244e-01;
  val[2][1][0][ 2][23] =   5.294332e-01; eval[2][1][0][ 2][23] =   1.238162e-01;
  val[2][1][0][ 2][24] =   4.957717e-01; eval[2][1][0][ 2][24] =   2.043137e-01;
  val[2][1][0][ 2][25] =   7.349340e-01; eval[2][1][0][ 2][25] =   2.420938e-01;
  val[2][1][0][ 2][26] =  -7.805618e-02; eval[2][1][0][ 2][26] =   1.414180e+00;
  val[2][1][0][ 2][27] =   6.994722e-01; eval[2][1][0][ 2][27] =   5.255619e-01;
  //emcdz_mean_c1d0z3
  n_val[2][1][0][3] = 28;
  val[2][1][0][ 3][ 0] =   1.851443e+00; eval[2][1][0][ 3][ 0] =   7.288200e-02;
  val[2][1][0][ 3][ 1] =   1.722201e+00; eval[2][1][0][ 3][ 1] =   1.557721e-02;
  val[2][1][0][ 3][ 2] =   1.687478e+00; eval[2][1][0][ 3][ 2] =   9.770822e-03;
  val[2][1][0][ 3][ 3] =   1.661639e+00; eval[2][1][0][ 3][ 3] =   1.179933e-02;
  val[2][1][0][ 3][ 4] =   1.576375e+00; eval[2][1][0][ 3][ 4] =   1.472726e-02;
  val[2][1][0][ 3][ 5] =   1.515232e+00; eval[2][1][0][ 3][ 5] =   1.957592e-02;
  val[2][1][0][ 3][ 6] =   1.481119e+00; eval[2][1][0][ 3][ 6] =   1.181107e-02;
  val[2][1][0][ 3][ 7] =   1.448058e+00; eval[2][1][0][ 3][ 7] =   1.543015e-02;
  val[2][1][0][ 3][ 8] =   1.421853e+00; eval[2][1][0][ 3][ 8] =   1.997287e-02;
  val[2][1][0][ 3][ 9] =   1.385663e+00; eval[2][1][0][ 3][ 9] =   2.567328e-02;
  val[2][1][0][ 3][10] =   1.372882e+00; eval[2][1][0][ 3][10] =   6.632193e-03;
  val[2][1][0][ 3][11] =   1.331385e+00; eval[2][1][0][ 3][11] =   8.772066e-03;
  val[2][1][0][ 3][12] =   1.342244e+00; eval[2][1][0][ 3][12] =   1.100971e-02;
  val[2][1][0][ 3][13] =   1.320240e+00; eval[2][1][0][ 3][13] =   1.405673e-02;
  val[2][1][0][ 3][14] =   1.288485e+00; eval[2][1][0][ 3][14] =   1.711607e-02;
  val[2][1][0][ 3][15] =   1.248650e+00; eval[2][1][0][ 3][15] =   2.131263e-02;
  val[2][1][0][ 3][16] =   1.272961e+00; eval[2][1][0][ 3][16] =   2.585020e-02;
  val[2][1][0][ 3][17] =   1.282046e+00; eval[2][1][0][ 3][17] =   3.247266e-02;
  val[2][1][0][ 3][18] =   1.278604e+00; eval[2][1][0][ 3][18] =   2.813173e-02;
  val[2][1][0][ 3][19] =   1.296101e+00; eval[2][1][0][ 3][19] =   3.912701e-02;
  val[2][1][0][ 3][20] =   1.211644e+00; eval[2][1][0][ 3][20] =   5.785559e-02;
  val[2][1][0][ 3][21] =   1.357128e+00; eval[2][1][0][ 3][21] =   9.396557e-02;
  val[2][1][0][ 3][22] =   1.183532e+00; eval[2][1][0][ 3][22] =   9.105489e-02;
  val[2][1][0][ 3][23] =   1.020209e+00; eval[2][1][0][ 3][23] =   1.041232e-01;
  val[2][1][0][ 3][24] =   1.249696e+00; eval[2][1][0][ 3][24] =   1.480328e-01;
  val[2][1][0][ 3][25] =   1.268655e+00; eval[2][1][0][ 3][25] =   1.931654e-01;
  val[2][1][0][ 3][26] =   1.702724e+00; eval[2][1][0][ 3][26] =   2.280880e-01;
  val[2][1][0][ 3][27] =   2.034033e+00; eval[2][1][0][ 3][27] =   4.866699e-01;
  //emcdz_mean_c1d0z4
  n_val[2][1][0][4] = 28;
  val[2][1][0][ 4][ 0] =   1.953685e+00; eval[2][1][0][ 4][ 0] =   8.014057e-02;
  val[2][1][0][ 4][ 1] =   1.947727e+00; eval[2][1][0][ 4][ 1] =   1.708651e-02;
  val[2][1][0][ 4][ 2] =   1.981323e+00; eval[2][1][0][ 4][ 2] =   1.128460e-02;
  val[2][1][0][ 4][ 3] =   1.977689e+00; eval[2][1][0][ 4][ 3] =   1.340219e-02;
  val[2][1][0][ 4][ 4] =   1.957377e+00; eval[2][1][0][ 4][ 4] =   1.677275e-02;
  val[2][1][0][ 4][ 5] =   1.946723e+00; eval[2][1][0][ 4][ 5] =   2.253591e-02;
  val[2][1][0][ 4][ 6] =   1.941564e+00; eval[2][1][0][ 4][ 6] =   1.352576e-02;
  val[2][1][0][ 4][ 7] =   1.924477e+00; eval[2][1][0][ 4][ 7] =   1.768379e-02;
  val[2][1][0][ 4][ 8] =   1.917588e+00; eval[2][1][0][ 4][ 8] =   2.286653e-02;
  val[2][1][0][ 4][ 9] =   1.910160e+00; eval[2][1][0][ 4][ 9] =   3.002912e-02;
  val[2][1][0][ 4][10] =   1.877394e+00; eval[2][1][0][ 4][10] =   7.793462e-03;
  val[2][1][0][ 4][11] =   1.885649e+00; eval[2][1][0][ 4][11] =   9.976283e-03;
  val[2][1][0][ 4][12] =   1.868067e+00; eval[2][1][0][ 4][12] =   1.327020e-02;
  val[2][1][0][ 4][13] =   1.872373e+00; eval[2][1][0][ 4][13] =   1.636003e-02;
  val[2][1][0][ 4][14] =   1.890309e+00; eval[2][1][0][ 4][14] =   2.129069e-02;
  val[2][1][0][ 4][15] =   1.833837e+00; eval[2][1][0][ 4][15] =   2.837301e-02;
  val[2][1][0][ 4][16] =   1.829619e+00; eval[2][1][0][ 4][16] =   3.139999e-02;
  val[2][1][0][ 4][17] =   1.860308e+00; eval[2][1][0][ 4][17] =   3.802483e-02;
  val[2][1][0][ 4][18] =   1.881650e+00; eval[2][1][0][ 4][18] =   3.904069e-02;
  val[2][1][0][ 4][19] =   1.895606e+00; eval[2][1][0][ 4][19] =   5.014893e-02;
  val[2][1][0][ 4][20] =   1.809496e+00; eval[2][1][0][ 4][20] =   6.553984e-02;
  val[2][1][0][ 4][21] =   1.802230e+00; eval[2][1][0][ 4][21] =   8.360169e-02;
  val[2][1][0][ 4][22] =   1.730687e+00; eval[2][1][0][ 4][22] =   1.237430e-01;
  val[2][1][0][ 4][23] =   1.882989e+00; eval[2][1][0][ 4][23] =   1.502916e-01;
  val[2][1][0][ 4][24] =   2.008258e+00; eval[2][1][0][ 4][24] =   2.293777e-01;
  val[2][1][0][ 4][25] =   1.452095e+00; eval[2][1][0][ 4][25] =   2.234188e-01;
  val[2][1][0][ 4][26] =   1.205615e+00; eval[2][1][0][ 4][26] =   1.221808e+00;
  val[2][1][0][ 4][27] =   2.497416e+00; eval[2][1][0][ 4][27] =   4.681863e-01;
  //emcdz_mean_c1d0z5
  n_val[2][1][0][5] = 28;
  val[2][1][0][ 5][ 0] =   2.087589e+00; eval[2][1][0][ 5][ 0] =   8.410689e-02;
  val[2][1][0][ 5][ 1] =   2.254538e+00; eval[2][1][0][ 5][ 1] =   1.901491e-02;
  val[2][1][0][ 5][ 2] =   2.354121e+00; eval[2][1][0][ 5][ 2] =   1.232783e-02;
  val[2][1][0][ 5][ 3] =   2.387973e+00; eval[2][1][0][ 5][ 3] =   1.535730e-02;
  val[2][1][0][ 5][ 4] =   2.431812e+00; eval[2][1][0][ 5][ 4] =   1.975636e-02;
  val[2][1][0][ 5][ 5] =   2.469165e+00; eval[2][1][0][ 5][ 5] =   2.506804e-02;
  val[2][1][0][ 5][ 6] =   2.508482e+00; eval[2][1][0][ 5][ 6] =   1.482718e-02;
  val[2][1][0][ 5][ 7] =   2.550336e+00; eval[2][1][0][ 5][ 7] =   1.946037e-02;
  val[2][1][0][ 5][ 8] =   2.551647e+00; eval[2][1][0][ 5][ 8] =   2.540081e-02;
  val[2][1][0][ 5][ 9] =   2.552263e+00; eval[2][1][0][ 5][ 9] =   3.307388e-02;
  val[2][1][0][ 5][10] =   2.570575e+00; eval[2][1][0][ 5][10] =   8.573377e-03;
  val[2][1][0][ 5][11] =   2.562584e+00; eval[2][1][0][ 5][11] =   1.109211e-02;
  val[2][1][0][ 5][12] =   2.585995e+00; eval[2][1][0][ 5][12] =   1.395871e-02;
  val[2][1][0][ 5][13] =   2.579748e+00; eval[2][1][0][ 5][13] =   1.760783e-02;
  val[2][1][0][ 5][14] =   2.615465e+00; eval[2][1][0][ 5][14] =   2.190928e-02;
  val[2][1][0][ 5][15] =   2.594104e+00; eval[2][1][0][ 5][15] =   2.736233e-02;
  val[2][1][0][ 5][16] =   2.586673e+00; eval[2][1][0][ 5][16] =   3.296490e-02;
  val[2][1][0][ 5][17] =   2.571148e+00; eval[2][1][0][ 5][17] =   4.100156e-02;
  val[2][1][0][ 5][18] =   2.572190e+00; eval[2][1][0][ 5][18] =   3.765038e-02;
  val[2][1][0][ 5][19] =   2.644720e+00; eval[2][1][0][ 5][19] =   5.122233e-02;
  val[2][1][0][ 5][20] =   2.450124e+00; eval[2][1][0][ 5][20] =   7.326198e-02;
  val[2][1][0][ 5][21] =   2.565371e+00; eval[2][1][0][ 5][21] =   9.939076e-02;
  val[2][1][0][ 5][22] =   2.794629e+00; eval[2][1][0][ 5][22] =   1.714802e-01;
  val[2][1][0][ 5][23] =   2.641791e+00; eval[2][1][0][ 5][23] =   1.566850e-01;
  val[2][1][0][ 5][24] =   2.366073e+00; eval[2][1][0][ 5][24] =   2.683853e-01;
  val[2][1][0][ 5][25] =   2.571243e+00; eval[2][1][0][ 5][25] =   3.393660e-01;
  val[2][1][0][ 5][26] =   2.639726e+00; eval[2][1][0][ 5][26] =   9.759254e-01;
  val[2][1][0][ 5][27] =   2.490736e+00; eval[2][1][0][ 5][27] =   2.854965e-01;
  //emcdz_mean_c1d0z6
  n_val[2][1][0][6] = 28;
  val[2][1][0][ 6][ 0] =   2.290047e+00; eval[2][1][0][ 6][ 0] =   7.395602e-02;
  val[2][1][0][ 6][ 1] =   2.555291e+00; eval[2][1][0][ 6][ 1] =   1.560321e-02;
  val[2][1][0][ 6][ 2] =   2.766526e+00; eval[2][1][0][ 6][ 2] =   1.068881e-02;
  val[2][1][0][ 6][ 3] =   2.826370e+00; eval[2][1][0][ 6][ 3] =   1.329376e-02;
  val[2][1][0][ 6][ 4] =   2.876469e+00; eval[2][1][0][ 6][ 4] =   1.648734e-02;
  val[2][1][0][ 6][ 5] =   2.978854e+00; eval[2][1][0][ 6][ 5] =   2.114673e-02;
  val[2][1][0][ 6][ 6] =   3.039923e+00; eval[2][1][0][ 6][ 6] =   1.243239e-02;
  val[2][1][0][ 6][ 7] =   3.078394e+00; eval[2][1][0][ 6][ 7] =   1.607643e-02;
  val[2][1][0][ 6][ 8] =   3.108766e+00; eval[2][1][0][ 6][ 8] =   2.087868e-02;
  val[2][1][0][ 6][ 9] =   3.119620e+00; eval[2][1][0][ 6][ 9] =   2.706753e-02;
  val[2][1][0][ 6][10] =   3.155346e+00; eval[2][1][0][ 6][10] =   7.011639e-03;
  val[2][1][0][ 6][11] =   3.181654e+00; eval[2][1][0][ 6][11] =   9.175403e-03;
  val[2][1][0][ 6][12] =   3.188791e+00; eval[2][1][0][ 6][12] =   1.179997e-02;
  val[2][1][0][ 6][13] =   3.200619e+00; eval[2][1][0][ 6][13] =   1.454784e-02;
  val[2][1][0][ 6][14] =   3.204401e+00; eval[2][1][0][ 6][14] =   1.812521e-02;
  val[2][1][0][ 6][15] =   3.251247e+00; eval[2][1][0][ 6][15] =   2.304413e-02;
  val[2][1][0][ 6][16] =   3.241731e+00; eval[2][1][0][ 6][16] =   2.761543e-02;
  val[2][1][0][ 6][17] =   3.202613e+00; eval[2][1][0][ 6][17] =   3.532022e-02;
  val[2][1][0][ 6][18] =   3.258982e+00; eval[2][1][0][ 6][18] =   3.092409e-02;
  val[2][1][0][ 6][19] =   3.207497e+00; eval[2][1][0][ 6][19] =   4.284339e-02;
  val[2][1][0][ 6][20] =   3.301047e+00; eval[2][1][0][ 6][20] =   6.271377e-02;
  val[2][1][0][ 6][21] =   3.238556e+00; eval[2][1][0][ 6][21] =   7.417551e-02;
  val[2][1][0][ 6][22] =   3.160909e+00; eval[2][1][0][ 6][22] =   1.137703e-01;
  val[2][1][0][ 6][23] =   3.271886e+00; eval[2][1][0][ 6][23] =   1.146752e-01;
  val[2][1][0][ 6][24] =   3.384439e+00; eval[2][1][0][ 6][24] =   1.729244e-01;
  val[2][1][0][ 6][25] =   3.351564e+00; eval[2][1][0][ 6][25] =   3.843853e-01;
  val[2][1][0][ 6][26] =   3.428606e+00; eval[2][1][0][ 6][26] =   2.920327e-01;
  val[2][1][0][ 6][27] =   3.234965e+00; eval[2][1][0][ 6][27] =   5.125573e-01;
  //emcdz_mean_c1d0z7
  n_val[2][1][0][7] = 28;
  val[2][1][0][ 7][ 0] =   2.443384e+00; eval[2][1][0][ 7][ 0] =   7.334643e-02;
  val[2][1][0][ 7][ 1] =   2.865063e+00; eval[2][1][0][ 7][ 1] =   1.607916e-02;
  val[2][1][0][ 7][ 2] =   3.077677e+00; eval[2][1][0][ 7][ 2] =   1.063086e-02;
  val[2][1][0][ 7][ 3] =   3.193158e+00; eval[2][1][0][ 7][ 3] =   1.408704e-02;
  val[2][1][0][ 7][ 4] =   3.288065e+00; eval[2][1][0][ 7][ 4] =   1.769339e-02;
  val[2][1][0][ 7][ 5] =   3.451652e+00; eval[2][1][0][ 7][ 5] =   2.228906e-02;
  val[2][1][0][ 7][ 6] =   3.546690e+00; eval[2][1][0][ 7][ 6] =   1.313497e-02;
  val[2][1][0][ 7][ 7] =   3.633856e+00; eval[2][1][0][ 7][ 7] =   1.721740e-02;
  val[2][1][0][ 7][ 8] =   3.678400e+00; eval[2][1][0][ 7][ 8] =   2.245255e-02;
  val[2][1][0][ 7][ 9] =   3.723049e+00; eval[2][1][0][ 7][ 9] =   2.914834e-02;
  val[2][1][0][ 7][10] =   3.767629e+00; eval[2][1][0][ 7][10] =   7.512797e-03;
  val[2][1][0][ 7][11] =   3.790249e+00; eval[2][1][0][ 7][11] =   9.759482e-03;
  val[2][1][0][ 7][12] =   3.804744e+00; eval[2][1][0][ 7][12] =   1.256374e-02;
  val[2][1][0][ 7][13] =   3.809389e+00; eval[2][1][0][ 7][13] =   1.573319e-02;
  val[2][1][0][ 7][14] =   3.836625e+00; eval[2][1][0][ 7][14] =   1.968676e-02;
  val[2][1][0][ 7][15] =   3.845527e+00; eval[2][1][0][ 7][15] =   2.497742e-02;
  val[2][1][0][ 7][16] =   3.852107e+00; eval[2][1][0][ 7][16] =   2.858059e-02;
  val[2][1][0][ 7][17] =   3.916091e+00; eval[2][1][0][ 7][17] =   3.657251e-02;
  val[2][1][0][ 7][18] =   3.994911e+00; eval[2][1][0][ 7][18] =   3.335897e-02;
  val[2][1][0][ 7][19] =   4.027202e+00; eval[2][1][0][ 7][19] =   4.672196e-02;
  val[2][1][0][ 7][20] =   3.840932e+00; eval[2][1][0][ 7][20] =   6.161388e-02;
  val[2][1][0][ 7][21] =   3.954554e+00; eval[2][1][0][ 7][21] =   8.097017e-02;
  val[2][1][0][ 7][22] =   3.763797e+00; eval[2][1][0][ 7][22] =   1.036858e-01;
  val[2][1][0][ 7][23] =   4.005341e+00; eval[2][1][0][ 7][23] =   1.339998e-01;
  val[2][1][0][ 7][24] =   4.249518e+00; eval[2][1][0][ 7][24] =   1.528157e-01;
  val[2][1][0][ 7][25] =   4.020087e+00; eval[2][1][0][ 7][25] =   2.089920e-01;
  val[2][1][0][ 7][26] =   3.396407e+00; eval[2][1][0][ 7][26] =   4.493503e-01;
  val[2][1][0][ 7][27] =   3.831130e+00; eval[2][1][0][ 7][27] =   4.505028e-01;
  //emcdz_mean_c1d0z8
  n_val[2][1][0][8] = 28;
  val[2][1][0][ 8][ 0] =   2.806692e+00; eval[2][1][0][ 8][ 0] =   7.674119e-02;
  val[2][1][0][ 8][ 1] =   3.305244e+00; eval[2][1][0][ 8][ 1] =   1.730385e-02;
  val[2][1][0][ 8][ 2] =   3.578172e+00; eval[2][1][0][ 8][ 2] =   1.170707e-02;
  val[2][1][0][ 8][ 3] =   3.685070e+00; eval[2][1][0][ 8][ 3] =   1.560834e-02;
  val[2][1][0][ 8][ 4] =   3.811142e+00; eval[2][1][0][ 8][ 4] =   2.007966e-02;
  val[2][1][0][ 8][ 5] =   3.995619e+00; eval[2][1][0][ 8][ 5] =   2.528415e-02;
  val[2][1][0][ 8][ 6] =   4.172395e+00; eval[2][1][0][ 8][ 6] =   1.494070e-02;
  val[2][1][0][ 8][ 7] =   4.265639e+00; eval[2][1][0][ 8][ 7] =   1.929324e-02;
  val[2][1][0][ 8][ 8] =   4.337400e+00; eval[2][1][0][ 8][ 8] =   2.539793e-02;
  val[2][1][0][ 8][ 9] =   4.430279e+00; eval[2][1][0][ 8][ 9] =   3.312268e-02;
  val[2][1][0][ 8][10] =   4.467640e+00; eval[2][1][0][ 8][10] =   8.589098e-03;
  val[2][1][0][ 8][11] =   4.523320e+00; eval[2][1][0][ 8][11] =   1.091392e-02;
  val[2][1][0][ 8][12] =   4.547270e+00; eval[2][1][0][ 8][12] =   1.411200e-02;
  val[2][1][0][ 8][13] =   4.567578e+00; eval[2][1][0][ 8][13] =   1.792370e-02;
  val[2][1][0][ 8][14] =   4.611885e+00; eval[2][1][0][ 8][14] =   2.239935e-02;
  val[2][1][0][ 8][15] =   4.619841e+00; eval[2][1][0][ 8][15] =   2.771981e-02;
  val[2][1][0][ 8][16] =   4.639957e+00; eval[2][1][0][ 8][16] =   3.408868e-02;
  val[2][1][0][ 8][17] =   4.615727e+00; eval[2][1][0][ 8][17] =   4.030300e-02;
  val[2][1][0][ 8][18] =   4.777508e+00; eval[2][1][0][ 8][18] =   3.893053e-02;
  val[2][1][0][ 8][19] =   4.739121e+00; eval[2][1][0][ 8][19] =   5.039482e-02;
  val[2][1][0][ 8][20] =   4.783224e+00; eval[2][1][0][ 8][20] =   7.508356e-02;
  val[2][1][0][ 8][21] =   4.874903e+00; eval[2][1][0][ 8][21] =   9.412047e-02;
  val[2][1][0][ 8][22] =   4.758272e+00; eval[2][1][0][ 8][22] =   1.190602e-01;
  val[2][1][0][ 8][23] =   4.634526e+00; eval[2][1][0][ 8][23] =   1.764282e-01;
  val[2][1][0][ 8][24] =   4.836708e+00; eval[2][1][0][ 8][24] =   1.950021e-01;
  val[2][1][0][ 8][25] =   4.566886e+00; eval[2][1][0][ 8][25] =   3.148935e-01;
  val[2][1][0][ 8][26] =   4.608799e+00; eval[2][1][0][ 8][26] =   2.188180e-01;
  val[2][1][0][ 8][27] =   4.069176e+00; eval[2][1][0][ 8][27] =   6.876039e-01;
  //emcdz_mean_c1d0z9
  n_val[2][1][0][9] = 28;
  val[2][1][0][ 9][ 0] =   2.973746e+00; eval[2][1][0][ 9][ 0] =   8.430933e-02;
  val[2][1][0][ 9][ 1] =   3.570515e+00; eval[2][1][0][ 9][ 1] =   1.955768e-02;
  val[2][1][0][ 9][ 2] =   3.832295e+00; eval[2][1][0][ 9][ 2] =   1.324594e-02;
  val[2][1][0][ 9][ 3] =   3.989137e+00; eval[2][1][0][ 9][ 3] =   1.806609e-02;
  val[2][1][0][ 9][ 4] =   4.144588e+00; eval[2][1][0][ 9][ 4] =   2.355814e-02;
  val[2][1][0][ 9][ 5] =   4.417732e+00; eval[2][1][0][ 9][ 5] =   3.035697e-02;
  val[2][1][0][ 9][ 6] =   4.599770e+00; eval[2][1][0][ 9][ 6] =   1.789375e-02;
  val[2][1][0][ 9][ 7] =   4.703589e+00; eval[2][1][0][ 9][ 7] =   2.337799e-02;
  val[2][1][0][ 9][ 8] =   4.798913e+00; eval[2][1][0][ 9][ 8] =   3.085679e-02;
  val[2][1][0][ 9][ 9] =   4.873392e+00; eval[2][1][0][ 9][ 9] =   4.027958e-02;
  val[2][1][0][ 9][10] =   4.928708e+00; eval[2][1][0][ 9][10] =   1.055970e-02;
  val[2][1][0][ 9][11] =   4.969356e+00; eval[2][1][0][ 9][11] =   1.348678e-02;
  val[2][1][0][ 9][12] =   5.063370e+00; eval[2][1][0][ 9][12] =   1.722692e-02;
  val[2][1][0][ 9][13] =   5.041008e+00; eval[2][1][0][ 9][13] =   2.124596e-02;
  val[2][1][0][ 9][14] =   5.178305e+00; eval[2][1][0][ 9][14] =   2.696583e-02;
  val[2][1][0][ 9][15] =   5.185216e+00; eval[2][1][0][ 9][15] =   3.330055e-02;
  val[2][1][0][ 9][16] =   5.145487e+00; eval[2][1][0][ 9][16] =   4.217491e-02;
  val[2][1][0][ 9][17] =   5.208040e+00; eval[2][1][0][ 9][17] =   4.803112e-02;
  val[2][1][0][ 9][18] =   5.211056e+00; eval[2][1][0][ 9][18] =   4.778953e-02;
  val[2][1][0][ 9][19] =   5.235060e+00; eval[2][1][0][ 9][19] =   6.454760e-02;
  val[2][1][0][ 9][20] =   5.438453e+00; eval[2][1][0][ 9][20] =   8.231426e-02;
  val[2][1][0][ 9][21] =   5.101138e+00; eval[2][1][0][ 9][21] =   1.269785e-01;
  val[2][1][0][ 9][22] =   5.188804e+00; eval[2][1][0][ 9][22] =   1.649152e-01;
  val[2][1][0][ 9][23] =   9.423851e+00; eval[2][1][0][ 9][23] =   1.414042e+00;
  val[2][1][0][ 9][24] =   5.573749e+00; eval[2][1][0][ 9][24] =   2.394468e-01;
  val[2][1][0][ 9][25] =   5.247499e+00; eval[2][1][0][ 9][25] =   3.176931e-01;
  val[2][1][0][ 9][26] =   4.575660e+00; eval[2][1][0][ 9][26] =   1.249287e+00;
  val[2][1][0][ 9][27] =   5.208586e+00; eval[2][1][0][ 9][27] =   1.675331e+00;
  //emcdz_mean_c1d1z0
  n_val[2][1][1][0] = 28;
  val[2][1][1][ 0][ 0] =  -9.437291e-01; eval[2][1][1][ 0][ 0] =   8.010454e-02;
  val[2][1][1][ 0][ 1] =  -1.422988e+00; eval[2][1][1][ 0][ 1] =   1.779152e-02;
  val[2][1][1][ 0][ 2] =  -1.622715e+00; eval[2][1][1][ 0][ 2] =   1.141392e-02;
  val[2][1][1][ 0][ 3] =  -1.685422e+00; eval[2][1][1][ 0][ 3] =   1.502884e-02;
  val[2][1][1][ 0][ 4] =  -1.817502e+00; eval[2][1][1][ 0][ 4] =   2.103809e-02;
  val[2][1][1][ 0][ 5] =  -1.996624e+00; eval[2][1][1][ 0][ 5] =   2.904964e-02;
  val[2][1][1][ 0][ 6] =  -2.072733e+00; eval[2][1][1][ 0][ 6] =   1.864143e-02;
  val[2][1][1][ 0][ 7] =  -2.140041e+00; eval[2][1][1][ 0][ 7] =   2.545951e-02;
  val[2][1][1][ 0][ 8] =  -2.280823e+00; eval[2][1][1][ 0][ 8] =   3.591206e-02;
  val[2][1][1][ 0][ 9] =  -2.281059e+00; eval[2][1][1][ 0][ 9] =   4.565051e-02;
  val[2][1][1][ 0][10] =  -2.399361e+00; eval[2][1][1][ 0][10] =   1.220759e-02;
  val[2][1][1][ 0][11] =  -2.400561e+00; eval[2][1][1][ 0][11] =   1.511191e-02;
  val[2][1][1][ 0][12] =  -2.517247e+00; eval[2][1][1][ 0][12] =   1.949021e-02;
  val[2][1][1][ 0][13] =  -2.544869e+00; eval[2][1][1][ 0][13] =   2.470743e-02;
  val[2][1][1][ 0][14] =  -2.599462e+00; eval[2][1][1][ 0][14] =   3.070946e-02;
  val[2][1][1][ 0][15] =  -2.681740e+00; eval[2][1][1][ 0][15] =   4.004098e-02;
  val[2][1][1][ 0][16] =  -2.725401e+00; eval[2][1][1][ 0][16] =   4.751042e-02;
  val[2][1][1][ 0][17] =  -2.603145e+00; eval[2][1][1][ 0][17] =   5.607784e-02;
  val[2][1][1][ 0][18] =  -2.632162e+00; eval[2][1][1][ 0][18] =   5.111777e-02;
  val[2][1][1][ 0][19] =  -2.701977e+00; eval[2][1][1][ 0][19] =   7.163931e-02;
  val[2][1][1][ 0][20] =  -3.028016e+00; eval[2][1][1][ 0][20] =   1.111752e-01;
  val[2][1][1][ 0][21] =  -2.705495e+00; eval[2][1][1][ 0][21] =   1.476407e-01;
  val[2][1][1][ 0][22] =  -2.766405e+00; eval[2][1][1][ 0][22] =   1.781388e-01;
  val[2][1][1][ 0][23] =  -2.550437e+00; eval[2][1][1][ 0][23] =   1.799806e-01;
  val[2][1][1][ 0][24] =  -2.507314e+00; eval[2][1][1][ 0][24] =   3.169778e-01;
  val[2][1][1][ 0][25] =  -3.138271e+00; eval[2][1][1][ 0][25] =   4.807151e-01;
  val[2][1][1][ 0][26] =  -6.058048e+00; eval[2][1][1][ 0][26] =   4.367068e+00;
  val[2][1][1][ 0][27] =  -1.885302e+00; eval[2][1][1][ 0][27] =   9.264935e-01;
  //emcdz_mean_c1d1z1
  n_val[2][1][1][1] = 28;
  val[2][1][1][ 1][ 0] =  -7.180666e-01; eval[2][1][1][ 1][ 0] =   6.826084e-02;
  val[2][1][1][ 1][ 1] =  -1.182453e+00; eval[2][1][1][ 1][ 1] =   1.421918e-02;
  val[2][1][1][ 1][ 2] =  -1.356346e+00; eval[2][1][1][ 1][ 2] =   9.385108e-03;
  val[2][1][1][ 1][ 3] =  -1.411591e+00; eval[2][1][1][ 1][ 3] =   1.237614e-02;
  val[2][1][1][ 1][ 4] =  -1.504413e+00; eval[2][1][1][ 1][ 4] =   1.648775e-02;
  val[2][1][1][ 1][ 5] =  -1.679193e+00; eval[2][1][1][ 1][ 5] =   2.250297e-02;
  val[2][1][1][ 1][ 6] =  -1.758423e+00; eval[2][1][1][ 1][ 6] =   1.429009e-02;
  val[2][1][1][ 1][ 7] =  -1.867534e+00; eval[2][1][1][ 1][ 7] =   2.002420e-02;
  val[2][1][1][ 1][ 8] =  -1.985128e+00; eval[2][1][1][ 1][ 8] =   2.781319e-02;
  val[2][1][1][ 1][ 9] =  -2.030042e+00; eval[2][1][1][ 1][ 9] =   3.667263e-02;
  val[2][1][1][ 1][10] =  -2.086477e+00; eval[2][1][1][ 1][10] =   9.579308e-03;
  val[2][1][1][ 1][11] =  -2.144447e+00; eval[2][1][1][ 1][11] =   1.237104e-02;
  val[2][1][1][ 1][12] =  -2.187104e+00; eval[2][1][1][ 1][12] =   1.547595e-02;
  val[2][1][1][ 1][13] =  -2.230241e+00; eval[2][1][1][ 1][13] =   1.965271e-02;
  val[2][1][1][ 1][14] =  -2.272943e+00; eval[2][1][1][ 1][14] =   2.393188e-02;
  val[2][1][1][ 1][15] =  -2.303484e+00; eval[2][1][1][ 1][15] =   2.924882e-02;
  val[2][1][1][ 1][16] =  -2.259186e+00; eval[2][1][1][ 1][16] =   3.769612e-02;
  val[2][1][1][ 1][17] =  -2.445226e+00; eval[2][1][1][ 1][17] =   4.574372e-02;
  val[2][1][1][ 1][18] =  -2.437562e+00; eval[2][1][1][ 1][18] =   4.122774e-02;
  val[2][1][1][ 1][19] =  -2.387937e+00; eval[2][1][1][ 1][19] =   5.774908e-02;
  val[2][1][1][ 1][20] =  -2.552125e+00; eval[2][1][1][ 1][20] =   7.620064e-02;
  val[2][1][1][ 1][21] =  -2.369601e+00; eval[2][1][1][ 1][21] =   9.170800e-02;
  val[2][1][1][ 1][22] =  -2.314747e+00; eval[2][1][1][ 1][22] =   1.200911e-01;
  val[2][1][1][ 1][23] =  -2.610657e+00; eval[2][1][1][ 1][23] =   1.529160e-01;
  val[2][1][1][ 1][24] =  -2.676782e+00; eval[2][1][1][ 1][24] =   2.194376e-01;
  val[2][1][1][ 1][25] =  -2.583800e+00; eval[2][1][1][ 1][25] =   1.875595e-01;
  val[2][1][1][ 1][26] =  -2.473408e+00; eval[2][1][1][ 1][26] =   2.672423e-01;
  val[2][1][1][ 1][27] =  -2.301072e+00; eval[2][1][1][ 1][27] =   3.639456e-01;
  //emcdz_mean_c1d1z2
  n_val[2][1][1][2] = 28;
  val[2][1][1][ 2][ 0] =  -5.314897e-01; eval[2][1][1][ 2][ 0] =   6.384742e-02;
  val[2][1][1][ 2][ 1] =  -8.765923e-01; eval[2][1][1][ 2][ 1] =   1.385324e-02;
  val[2][1][1][ 2][ 2] =  -1.001242e+00; eval[2][1][1][ 2][ 2] =   8.690579e-03;
  val[2][1][1][ 2][ 3] =  -1.083762e+00; eval[2][1][1][ 2][ 3] =   1.125790e-02;
  val[2][1][1][ 2][ 4] =  -1.148665e+00; eval[2][1][1][ 2][ 4] =   1.490838e-02;
  val[2][1][1][ 2][ 5] =  -1.231616e+00; eval[2][1][1][ 2][ 5] =   2.009667e-02;
  val[2][1][1][ 2][ 6] =  -1.311031e+00; eval[2][1][1][ 2][ 6] =   1.293213e-02;
  val[2][1][1][ 2][ 7] =  -1.370269e+00; eval[2][1][1][ 2][ 7] =   1.760956e-02;
  val[2][1][1][ 2][ 8] =  -1.419816e+00; eval[2][1][1][ 2][ 8] =   2.308419e-02;
  val[2][1][1][ 2][ 9] =  -1.471846e+00; eval[2][1][1][ 2][ 9] =   3.012486e-02;
  val[2][1][1][ 2][10] =  -1.525504e+00; eval[2][1][1][ 2][10] =   7.814483e-03;
  val[2][1][1][ 2][11] =  -1.586628e+00; eval[2][1][1][ 2][11] =   1.037150e-02;
  val[2][1][1][ 2][12] =  -1.608984e+00; eval[2][1][1][ 2][12] =   1.300525e-02;
  val[2][1][1][ 2][13] =  -1.662103e+00; eval[2][1][1][ 2][13] =   1.595375e-02;
  val[2][1][1][ 2][14] =  -1.702426e+00; eval[2][1][1][ 2][14] =   2.026040e-02;
  val[2][1][1][ 2][15] =  -1.730302e+00; eval[2][1][1][ 2][15] =   2.405613e-02;
  val[2][1][1][ 2][16] =  -1.757378e+00; eval[2][1][1][ 2][16] =   2.989545e-02;
  val[2][1][1][ 2][17] =  -1.706841e+00; eval[2][1][1][ 2][17] =   3.671709e-02;
  val[2][1][1][ 2][18] =  -1.723911e+00; eval[2][1][1][ 2][18] =   3.238685e-02;
  val[2][1][1][ 2][19] =  -1.806314e+00; eval[2][1][1][ 2][19] =   4.558401e-02;
  val[2][1][1][ 2][20] =  -1.736655e+00; eval[2][1][1][ 2][20] =   5.614661e-02;
  val[2][1][1][ 2][21] =  -1.757933e+00; eval[2][1][1][ 2][21] =   9.667821e-02;
  val[2][1][1][ 2][22] =  -1.922050e+00; eval[2][1][1][ 2][22] =   1.060832e-01;
  val[2][1][1][ 2][23] =  -1.970217e+00; eval[2][1][1][ 2][23] =   1.245521e-01;
  val[2][1][1][ 2][24] =  -1.727031e+00; eval[2][1][1][ 2][24] =   1.349506e-01;
  val[2][1][1][ 2][25] =  -1.535358e+00; eval[2][1][1][ 2][25] =   2.243658e-01;
  val[2][1][1][ 2][26] =  -1.794560e+00; eval[2][1][1][ 2][26] =   2.289839e-01;
  val[2][1][1][ 2][27] =  -1.622901e+00; eval[2][1][1][ 2][27] =   2.076864e-01;
  //emcdz_mean_c1d1z3
  n_val[2][1][1][3] = 28;
  val[2][1][1][ 3][ 0] =  -4.025199e-01; eval[2][1][1][ 3][ 0] =   6.306415e-02;
  val[2][1][1][ 3][ 1] =  -6.405369e-01; eval[2][1][1][ 3][ 1] =   1.315079e-02;
  val[2][1][1][ 3][ 2] =  -6.999982e-01; eval[2][1][1][ 3][ 2] =   7.934940e-03;
  val[2][1][1][ 3][ 3] =  -7.431164e-01; eval[2][1][1][ 3][ 3] =   1.030437e-02;
  val[2][1][1][ 3][ 4] =  -7.677285e-01; eval[2][1][1][ 3][ 4] =   1.340546e-02;
  val[2][1][1][ 3][ 5] =  -8.391696e-01; eval[2][1][1][ 3][ 5] =   1.782226e-02;
  val[2][1][1][ 3][ 6] =  -8.724783e-01; eval[2][1][1][ 3][ 6] =   1.127961e-02;
  val[2][1][1][ 3][ 7] =  -9.238478e-01; eval[2][1][1][ 3][ 7] =   1.579587e-02;
  val[2][1][1][ 3][ 8] =  -9.472190e-01; eval[2][1][1][ 3][ 8] =   2.068882e-02;
  val[2][1][1][ 3][ 9] =  -9.685068e-01; eval[2][1][1][ 3][ 9] =   2.786608e-02;
  val[2][1][1][ 3][10] =  -9.874652e-01; eval[2][1][1][ 3][10] =   7.193182e-03;
  val[2][1][1][ 3][11] =  -1.027934e+00; eval[2][1][1][ 3][11] =   9.121382e-03;
  val[2][1][1][ 3][12] =  -1.045265e+00; eval[2][1][1][ 3][12] =   1.174457e-02;
  val[2][1][1][ 3][13] =  -1.056286e+00; eval[2][1][1][ 3][13] =   1.488846e-02;
  val[2][1][1][ 3][14] =  -1.086581e+00; eval[2][1][1][ 3][14] =   1.842578e-02;
  val[2][1][1][ 3][15] =  -9.950974e-01; eval[2][1][1][ 3][15] =   2.320769e-02;
  val[2][1][1][ 3][16] =  -1.124807e+00; eval[2][1][1][ 3][16] =   2.789822e-02;
  val[2][1][1][ 3][17] =  -1.160651e+00; eval[2][1][1][ 3][17] =   3.155150e-02;
  val[2][1][1][ 3][18] =  -1.053792e+00; eval[2][1][1][ 3][18] =   3.129052e-02;
  val[2][1][1][ 3][19] =  -1.109552e+00; eval[2][1][1][ 3][19] =   3.947464e-02;
  val[2][1][1][ 3][20] =  -1.129136e+00; eval[2][1][1][ 3][20] =   5.345394e-02;
  val[2][1][1][ 3][21] =  -1.168733e+00; eval[2][1][1][ 3][21] =   6.538966e-02;
  val[2][1][1][ 3][22] =  -9.649315e-01; eval[2][1][1][ 3][22] =   1.014412e-01;
  val[2][1][1][ 3][23] =  -1.087001e+00; eval[2][1][1][ 3][23] =   1.210955e-01;
  val[2][1][1][ 3][24] =  -1.090080e+00; eval[2][1][1][ 3][24] =   1.460012e-01;
  val[2][1][1][ 3][25] =  -1.039171e+00; eval[2][1][1][ 3][25] =   1.352390e-01;
  val[2][1][1][ 3][26] =  -1.551012e+00; eval[2][1][1][ 3][26] =   1.714825e-01;
  val[2][1][1][ 3][27] =  -1.443124e+00; eval[2][1][1][ 3][27] =   3.905232e-01;
  //emcdz_mean_c1d1z4
  n_val[2][1][1][4] = 28;
  val[2][1][1][ 4][ 0] =  -3.048677e-01; eval[2][1][1][ 4][ 0] =   6.979332e-02;
  val[2][1][1][ 4][ 1] =  -4.317458e-01; eval[2][1][1][ 4][ 1] =   1.508128e-02;
  val[2][1][1][ 4][ 2] =  -4.372718e-01; eval[2][1][1][ 4][ 2] =   9.238385e-03;
  val[2][1][1][ 4][ 3] =  -4.606954e-01; eval[2][1][1][ 4][ 3] =   1.223753e-02;
  val[2][1][1][ 4][ 4] =  -4.580521e-01; eval[2][1][1][ 4][ 4] =   1.599910e-02;
  val[2][1][1][ 4][ 5] =  -4.834552e-01; eval[2][1][1][ 4][ 5] =   2.079082e-02;
  val[2][1][1][ 4][ 6] =  -4.977665e-01; eval[2][1][1][ 4][ 6] =   1.301108e-02;
  val[2][1][1][ 4][ 7] =  -5.180704e-01; eval[2][1][1][ 4][ 7] =   1.814259e-02;
  val[2][1][1][ 4][ 8] =  -5.288542e-01; eval[2][1][1][ 4][ 8] =   2.431115e-02;
  val[2][1][1][ 4][ 9] =  -5.391798e-01; eval[2][1][1][ 4][ 9] =   3.241707e-02;
  val[2][1][1][ 4][10] =  -5.532373e-01; eval[2][1][1][ 4][10] =   8.393010e-03;
  val[2][1][1][ 4][11] =  -5.596829e-01; eval[2][1][1][ 4][11] =   1.092480e-02;
  val[2][1][1][ 4][12] =  -5.622057e-01; eval[2][1][1][ 4][12] =   1.391893e-02;
  val[2][1][1][ 4][13] =  -5.669566e-01; eval[2][1][1][ 4][13] =   1.716028e-02;
  val[2][1][1][ 4][14] =  -6.051612e-01; eval[2][1][1][ 4][14] =   2.145469e-02;
  val[2][1][1][ 4][15] =  -6.137116e-01; eval[2][1][1][ 4][15] =   2.652276e-02;
  val[2][1][1][ 4][16] =  -5.471974e-01; eval[2][1][1][ 4][16] =   3.383933e-02;
  val[2][1][1][ 4][17] =  -6.281235e-01; eval[2][1][1][ 4][17] =   4.062739e-02;
  val[2][1][1][ 4][18] =  -6.285434e-01; eval[2][1][1][ 4][18] =   3.476798e-02;
  val[2][1][1][ 4][19] =  -5.693479e-01; eval[2][1][1][ 4][19] =   5.132213e-02;
  val[2][1][1][ 4][20] =  -6.339715e-01; eval[2][1][1][ 4][20] =   6.460552e-02;
  val[2][1][1][ 4][21] =  -4.597120e-01; eval[2][1][1][ 4][21] =   8.929540e-02;
  val[2][1][1][ 4][22] =  -6.783780e-01; eval[2][1][1][ 4][22] =   1.054406e-01;
  val[2][1][1][ 4][23] =  -4.900086e-01; eval[2][1][1][ 4][23] =   1.701329e-01;
  val[2][1][1][ 4][24] =  -2.815653e-01; eval[2][1][1][ 4][24] =   2.907722e-01;
  val[2][1][1][ 4][25] =  -8.855222e-01; eval[2][1][1][ 4][25] =   1.974260e-01;
  val[2][1][1][ 4][26] =  -5.628224e-01; eval[2][1][1][ 4][26] =   4.172949e-01;
  val[2][1][1][ 4][27] =  -7.162159e-01; eval[2][1][1][ 4][27] =   4.790106e-01;
  //emcdz_mean_c1d1z5
  n_val[2][1][1][5] = 28;
  val[2][1][1][ 5][ 0] =  -1.334346e-01; eval[2][1][1][ 5][ 0] =   6.953069e-02;
  val[2][1][1][ 5][ 1] =  -8.217052e-02; eval[2][1][1][ 5][ 1] =   1.480886e-02;
  val[2][1][1][ 5][ 2] =  -3.389686e-02; eval[2][1][1][ 5][ 2] =   8.981408e-03;
  val[2][1][1][ 5][ 3] =  -2.365161e-02; eval[2][1][1][ 5][ 3] =   1.128711e-02;
  val[2][1][1][ 5][ 4] =  -7.817190e-03; eval[2][1][1][ 5][ 4] =   1.475162e-02;
  val[2][1][1][ 5][ 5] =   3.395229e-02; eval[2][1][1][ 5][ 5] =   1.967970e-02;
  val[2][1][1][ 5][ 6] =   5.384497e-02; eval[2][1][1][ 5][ 6] =   1.251888e-02;
  val[2][1][1][ 5][ 7] =   5.203775e-02; eval[2][1][1][ 5][ 7] =   1.727525e-02;
  val[2][1][1][ 5][ 8] =   6.083469e-02; eval[2][1][1][ 5][ 8] =   2.306349e-02;
  val[2][1][1][ 5][ 9] =   5.687848e-02; eval[2][1][1][ 5][ 9] =   3.078406e-02;
  val[2][1][1][ 5][10] =   6.262954e-02; eval[2][1][1][ 5][10] =   8.058052e-03;
  val[2][1][1][ 5][11] =   5.472568e-02; eval[2][1][1][ 5][11] =   1.033748e-02;
  val[2][1][1][ 5][12] =   8.632637e-02; eval[2][1][1][ 5][12] =   1.301614e-02;
  val[2][1][1][ 5][13] =   6.985654e-02; eval[2][1][1][ 5][13] =   1.647040e-02;
  val[2][1][1][ 5][14] =   3.914675e-02; eval[2][1][1][ 5][14] =   2.088106e-02;
  val[2][1][1][ 5][15] =   1.535929e-01; eval[2][1][1][ 5][15] =   2.593832e-02;
  val[2][1][1][ 5][16] =   1.514544e-03; eval[2][1][1][ 5][16] =   3.316768e-02;
  val[2][1][1][ 5][17] =   7.221579e-02; eval[2][1][1][ 5][17] =   3.704812e-02;
  val[2][1][1][ 5][18] =   1.302711e-01; eval[2][1][1][ 5][18] =   3.476598e-02;
  val[2][1][1][ 5][19] =   4.651705e-02; eval[2][1][1][ 5][19] =   4.630745e-02;
  val[2][1][1][ 5][20] =   1.841846e-01; eval[2][1][1][ 5][20] =   6.221156e-02;
  val[2][1][1][ 5][21] =   2.129606e-01; eval[2][1][1][ 5][21] =   8.118338e-02;
  val[2][1][1][ 5][22] =   4.812450e-02; eval[2][1][1][ 5][22] =   1.037701e-01;
  val[2][1][1][ 5][23] =   1.685358e-01; eval[2][1][1][ 5][23] =   1.224983e-01;
  val[2][1][1][ 5][24] =   3.354703e-01; eval[2][1][1][ 5][24] =   1.451091e-01;
  val[2][1][1][ 5][25] =   4.578859e-01; eval[2][1][1][ 5][25] =   2.829336e-01;
  val[2][1][1][ 5][26] =   3.025367e-01; eval[2][1][1][ 5][26] =   3.755014e-01;
  val[2][1][1][ 5][27] =   5.911225e+02; eval[2][1][1][ 5][27] =   7.248435e+03;
  //emcdz_mean_c1d1z6
  n_val[2][1][1][6] = 28;
  val[2][1][1][ 6][ 0] =  -5.883382e-02; eval[2][1][1][ 6][ 0] =   6.021843e-02;
  val[2][1][1][ 6][ 1] =   9.058557e-02; eval[2][1][1][ 6][ 1] =   1.253239e-02;
  val[2][1][1][ 6][ 2] =   1.896406e-01; eval[2][1][1][ 6][ 2] =   7.692632e-03;
  val[2][1][1][ 6][ 3] =   2.156625e-01; eval[2][1][1][ 6][ 3] =   9.899762e-03;
  val[2][1][1][ 6][ 4] =   2.593406e-01; eval[2][1][1][ 6][ 4] =   1.291733e-02;
  val[2][1][1][ 6][ 5] =   3.672894e-01; eval[2][1][1][ 6][ 5] =   1.726654e-02;
  val[2][1][1][ 6][ 6] =   4.417003e-01; eval[2][1][1][ 6][ 6] =   1.102099e-02;
  val[2][1][1][ 6][ 7] =   4.776773e-01; eval[2][1][1][ 6][ 7] =   1.514440e-02;
  val[2][1][1][ 6][ 8] =   4.911266e-01; eval[2][1][1][ 6][ 8] =   2.026606e-02;
  val[2][1][1][ 6][ 9] =   5.043730e-01; eval[2][1][1][ 6][ 9] =   2.670633e-02;
  val[2][1][1][ 6][10] =   5.109701e-01; eval[2][1][1][ 6][10] =   7.033051e-03;
  val[2][1][1][ 6][11] =   5.181984e-01; eval[2][1][1][ 6][11] =   9.078812e-03;
  val[2][1][1][ 6][12] =   5.418813e-01; eval[2][1][1][ 6][12] =   1.165272e-02;
  val[2][1][1][ 6][13] =   5.628287e-01; eval[2][1][1][ 6][13] =   1.451385e-02;
  val[2][1][1][ 6][14] =   6.068866e-01; eval[2][1][1][ 6][14] =   1.738528e-02;
  val[2][1][1][ 6][15] =   6.144070e-01; eval[2][1][1][ 6][15] =   2.195067e-02;
  val[2][1][1][ 6][16] =   5.856491e-01; eval[2][1][1][ 6][16] =   2.792145e-02;
  val[2][1][1][ 6][17] =   5.881723e-01; eval[2][1][1][ 6][17] =   3.316693e-02;
  val[2][1][1][ 6][18] =   5.571618e-01; eval[2][1][1][ 6][18] =   3.018833e-02;
  val[2][1][1][ 6][19] =   6.765089e-01; eval[2][1][1][ 6][19] =   4.238995e-02;
  val[2][1][1][ 6][20] =   6.677981e-01; eval[2][1][1][ 6][20] =   5.470261e-02;
  val[2][1][1][ 6][21] =   6.564346e-01; eval[2][1][1][ 6][21] =   6.709189e-02;
  val[2][1][1][ 6][22] =   6.527455e-01; eval[2][1][1][ 6][22] =   8.629361e-02;
  val[2][1][1][ 6][23] =   6.041582e-01; eval[2][1][1][ 6][23] =   1.364102e-01;
  val[2][1][1][ 6][24] =   6.215689e-01; eval[2][1][1][ 6][24] =   1.667429e-01;
  val[2][1][1][ 6][25] =   5.806794e-01; eval[2][1][1][ 6][25] =   2.481594e-01;
  val[2][1][1][ 6][26] =   3.529934e-01; eval[2][1][1][ 6][26] =   3.308765e-01;
  val[2][1][1][ 6][27] =   8.714412e-01; eval[2][1][1][ 6][27] =   4.630951e-01;
  //emcdz_mean_c1d1z7
  n_val[2][1][1][7] = 28;
  val[2][1][1][ 7][ 0] =   9.951919e-02; eval[2][1][1][ 7][ 0] =   6.201475e-02;
  val[2][1][1][ 7][ 1] =   3.739332e-01; eval[2][1][1][ 7][ 1] =   1.286153e-02;
  val[2][1][1][ 7][ 2] =   5.538670e-01; eval[2][1][1][ 7][ 2] =   8.005977e-03;
  val[2][1][1][ 7][ 3] =   6.228822e-01; eval[2][1][1][ 7][ 3] =   1.038054e-02;
  val[2][1][1][ 7][ 4] =   6.721425e-01; eval[2][1][1][ 7][ 4] =   1.380068e-02;
  val[2][1][1][ 7][ 5] =   7.913450e-01; eval[2][1][1][ 7][ 5] =   1.820708e-02;
  val[2][1][1][ 7][ 6] =   9.134557e-01; eval[2][1][1][ 7][ 6] =   1.143604e-02;
  val[2][1][1][ 7][ 7] =   9.764635e-01; eval[2][1][1][ 7][ 7] =   1.565022e-02;
  val[2][1][1][ 7][ 8] =   1.017399e+00; eval[2][1][1][ 7][ 8] =   2.105988e-02;
  val[2][1][1][ 7][ 9] =   1.080341e+00; eval[2][1][1][ 7][ 9] =   2.844834e-02;
  val[2][1][1][ 7][10] =   1.094903e+00; eval[2][1][1][ 7][10] =   7.450940e-03;
  val[2][1][1][ 7][11] =   1.121415e+00; eval[2][1][1][ 7][11] =   9.726760e-03;
  val[2][1][1][ 7][12] =   1.137145e+00; eval[2][1][1][ 7][12] =   1.235913e-02;
  val[2][1][1][ 7][13] =   1.144144e+00; eval[2][1][1][ 7][13] =   1.542349e-02;
  val[2][1][1][ 7][14] =   1.177165e+00; eval[2][1][1][ 7][14] =   1.970932e-02;
  val[2][1][1][ 7][15] =   1.227821e+00; eval[2][1][1][ 7][15] =   2.400065e-02;
  val[2][1][1][ 7][16] =   1.246278e+00; eval[2][1][1][ 7][16] =   2.996191e-02;
  val[2][1][1][ 7][17] =   1.320071e+00; eval[2][1][1][ 7][17] =   3.598729e-02;
  val[2][1][1][ 7][18] =   1.287079e+00; eval[2][1][1][ 7][18] =   3.272413e-02;
  val[2][1][1][ 7][19] =   1.348638e+00; eval[2][1][1][ 7][19] =   4.421193e-02;
  val[2][1][1][ 7][20] =   1.281422e+00; eval[2][1][1][ 7][20] =   6.063858e-02;
  val[2][1][1][ 7][21] =   1.131132e+00; eval[2][1][1][ 7][21] =   8.216771e-02;
  val[2][1][1][ 7][22] =   1.415044e+00; eval[2][1][1][ 7][22] =   9.748235e-02;
  val[2][1][1][ 7][23] =   1.530475e+00; eval[2][1][1][ 7][23] =   1.379922e-01;
  val[2][1][1][ 7][24] =   1.456521e+00; eval[2][1][1][ 7][24] =   1.853394e-01;
  val[2][1][1][ 7][25] =   1.490018e+00; eval[2][1][1][ 7][25] =   1.840713e-01;
  val[2][1][1][ 7][26] =   1.419476e+00; eval[2][1][1][ 7][26] =   2.327235e-01;
  val[2][1][1][ 7][27] =   1.348659e+00; eval[2][1][1][ 7][27] =   2.804464e-01;
  //emcdz_mean_c1d1z8
  n_val[2][1][1][8] = 28;
  val[2][1][1][ 8][ 0] =   3.823389e-01; eval[2][1][1][ 8][ 0] =   6.151377e-02;
  val[2][1][1][ 8][ 1] =   7.603271e-01; eval[2][1][1][ 8][ 1] =   1.343449e-02;
  val[2][1][1][ 8][ 2] =   9.901914e-01; eval[2][1][1][ 8][ 2] =   8.758456e-03;
  val[2][1][1][ 8][ 3] =   1.052492e+00; eval[2][1][1][ 8][ 3] =   1.157967e-02;
  val[2][1][1][ 8][ 4] =   1.113368e+00; eval[2][1][1][ 8][ 4] =   1.588891e-02;
  val[2][1][1][ 8][ 5] =   1.343557e+00; eval[2][1][1][ 8][ 5] =   2.142214e-02;
  val[2][1][1][ 8][ 6] =   1.471875e+00; eval[2][1][1][ 8][ 6] =   1.336971e-02;
  val[2][1][1][ 8][ 7] =   1.568286e+00; eval[2][1][1][ 8][ 7] =   1.823674e-02;
  val[2][1][1][ 8][ 8] =   1.669380e+00; eval[2][1][1][ 8][ 8] =   2.501223e-02;
  val[2][1][1][ 8][ 9] =   1.762737e+00; eval[2][1][1][ 8][ 9] =   3.328064e-02;
  val[2][1][1][ 8][10] =   1.778377e+00; eval[2][1][1][ 8][10] =   8.857166e-03;
  val[2][1][1][ 8][11] =   1.827684e+00; eval[2][1][1][ 8][11] =   1.129461e-02;
  val[2][1][1][ 8][12] =   1.843083e+00; eval[2][1][1][ 8][12] =   1.420695e-02;
  val[2][1][1][ 8][13] =   1.797657e+00; eval[2][1][1][ 8][13] =   1.828461e-02;
  val[2][1][1][ 8][14] =   1.901380e+00; eval[2][1][1][ 8][14] =   2.242664e-02;
  val[2][1][1][ 8][15] =   1.914021e+00; eval[2][1][1][ 8][15] =   2.789003e-02;
  val[2][1][1][ 8][16] =   1.980324e+00; eval[2][1][1][ 8][16] =   3.554575e-02;
  val[2][1][1][ 8][17] =   1.972075e+00; eval[2][1][1][ 8][17] =   4.284023e-02;
  val[2][1][1][ 8][18] =   1.894899e+00; eval[2][1][1][ 8][18] =   4.016214e-02;
  val[2][1][1][ 8][19] =   1.998483e+00; eval[2][1][1][ 8][19] =   5.162591e-02;
  val[2][1][1][ 8][20] =   2.128410e+00; eval[2][1][1][ 8][20] =   7.443646e-02;
  val[2][1][1][ 8][21] =   2.196136e+00; eval[2][1][1][ 8][21] =   8.770629e-02;
  val[2][1][1][ 8][22] =   2.108178e+00; eval[2][1][1][ 8][22] =   1.266137e-01;
  val[2][1][1][ 8][23] =   1.849080e+00; eval[2][1][1][ 8][23] =   2.337485e-01;
  val[2][1][1][ 8][24] =   2.002958e+00; eval[2][1][1][ 8][24] =   2.468180e-01;
  val[2][1][1][ 8][25] =   2.307648e+00; eval[2][1][1][ 8][25] =   2.715946e-01;
  val[2][1][1][ 8][26] =   2.999306e+00; eval[2][1][1][ 8][26] =   3.795580e-01;
  val[2][1][1][ 8][27] =   2.032887e+00; eval[2][1][1][ 8][27] =   6.251515e-01;
  //emcdz_mean_c1d1z9
  n_val[2][1][1][9] = 28;
  val[2][1][1][ 9][ 0] =   6.614140e-01; eval[2][1][1][ 9][ 0] =   6.940275e-02;
  val[2][1][1][ 9][ 1] =   1.048185e+00; eval[2][1][1][ 9][ 1] =   1.648120e-02;
  val[2][1][1][ 9][ 2] =   1.255042e+00; eval[2][1][1][ 9][ 2] =   1.089169e-02;
  val[2][1][1][ 9][ 3] =   1.347470e+00; eval[2][1][1][ 9][ 3] =   1.422390e-02;
  val[2][1][1][ 9][ 4] =   1.418148e+00; eval[2][1][1][ 9][ 4] =   2.045293e-02;
  val[2][1][1][ 9][ 5] =   1.678855e+00; eval[2][1][1][ 9][ 5] =   2.776749e-02;
  val[2][1][1][ 9][ 6] =   1.890730e+00; eval[2][1][1][ 9][ 6] =   1.793131e-02;
  val[2][1][1][ 9][ 7] =   2.065960e+00; eval[2][1][1][ 9][ 7] =   2.488803e-02;
  val[2][1][1][ 9][ 8] =   2.138372e+00; eval[2][1][1][ 9][ 8] =   3.366882e-02;
  val[2][1][1][ 9][ 9] =   2.181422e+00; eval[2][1][1][ 9][ 9] =   4.501747e-02;
  val[2][1][1][ 9][10] =   2.211218e+00; eval[2][1][1][ 9][10] =   1.173794e-02;
  val[2][1][1][ 9][11] =   2.251126e+00; eval[2][1][1][ 9][11] =   1.525102e-02;
  val[2][1][1][ 9][12] =   2.285982e+00; eval[2][1][1][ 9][12] =   1.929482e-02;
  val[2][1][1][ 9][13] =   2.257573e+00; eval[2][1][1][ 9][13] =   2.481808e-02;
  val[2][1][1][ 9][14] =   2.352708e+00; eval[2][1][1][ 9][14] =   2.971082e-02;
  val[2][1][1][ 9][15] =   2.310933e+00; eval[2][1][1][ 9][15] =   3.871737e-02;
  val[2][1][1][ 9][16] =   2.340531e+00; eval[2][1][1][ 9][16] =   4.543086e-02;
  val[2][1][1][ 9][17] =   2.466303e+00; eval[2][1][1][ 9][17] =   5.060383e-02;
  val[2][1][1][ 9][18] =   2.427431e+00; eval[2][1][1][ 9][18] =   5.038859e-02;
  val[2][1][1][ 9][19] =   2.397117e+00; eval[2][1][1][ 9][19] =   6.841135e-02;
  val[2][1][1][ 9][20] =   2.552969e+00; eval[2][1][1][ 9][20] =   9.748798e-02;
  val[2][1][1][ 9][21] =   2.420706e+00; eval[2][1][1][ 9][21] =   1.305535e-01;
  val[2][1][1][ 9][22] =   2.621953e+00; eval[2][1][1][ 9][22] =   1.615064e-01;
  val[2][1][1][ 9][23] =   2.377328e+00; eval[2][1][1][ 9][23] =   1.811143e-01;
  val[2][1][1][ 9][24] =   2.125101e+00; eval[2][1][1][ 9][24] =   3.614179e-01;
  val[2][1][1][ 9][25] =   2.755192e+00; eval[2][1][1][ 9][25] =   3.258846e-01;
  val[2][1][1][ 9][26] =   2.803599e+00; eval[2][1][1][ 9][26] =   4.269758e-01;
  val[2][1][1][ 9][27] =   1.812638e+00; eval[2][1][1][ 9][27] =   3.334178e+00;
  //emcdz_sigma_c0d0z0
  n_val[3][0][0][0] = 28;
  val[3][0][0][ 0][ 0] =   3.444336e+00; eval[3][0][0][ 0][ 0] =   1.897715e-02;
  val[3][0][0][ 0][ 1] =   3.016328e+00; eval[3][0][0][ 0][ 1] =   1.780435e-02;
  val[3][0][0][ 0][ 2] =   2.819053e+00; eval[3][0][0][ 0][ 2] =   2.187507e-02;
  val[3][0][0][ 0][ 3] =   2.712200e+00; eval[3][0][0][ 0][ 3] =   1.354536e-03;
  val[3][0][0][ 0][ 4] =   2.627660e+00; eval[3][0][0][ 0][ 4] =   1.693999e-03;
  val[3][0][0][ 0][ 5] =   2.528753e+00; eval[3][0][0][ 0][ 5] =   2.280071e-03;
  val[3][0][0][ 0][ 6] =   2.529255e+00; eval[3][0][0][ 0][ 6] =   2.814753e-03;
  val[3][0][0][ 0][ 7] =   2.466078e+00; eval[3][0][0][ 0][ 7] =   3.604467e-03;
  val[3][0][0][ 0][ 8] =   2.453575e+00; eval[3][0][0][ 0][ 8] =   4.761107e-03;
  val[3][0][0][ 0][ 9] =   2.453523e+00; eval[3][0][0][ 0][ 9] =   6.307077e-03;
  val[3][0][0][ 0][10] =   2.432653e+00; eval[3][0][0][ 0][10] =   8.100722e-03;
  val[3][0][0][ 0][11] =   2.444908e+00; eval[3][0][0][ 0][11] =   1.065889e-02;
  val[3][0][0][ 0][12] =   2.433422e+00; eval[3][0][0][ 0][12] =   1.365075e-02;
  val[3][0][0][ 0][13] =   2.421112e+00; eval[3][0][0][ 0][13] =   1.723857e-02;
  val[3][0][0][ 0][14] =   2.360761e+00; eval[3][0][0][ 0][14] =   2.023178e-02;
  val[3][0][0][ 0][15] =   2.399341e+00; eval[3][0][0][ 0][15] =   2.594591e-02;
  val[3][0][0][ 0][16] =   2.470461e+00; eval[3][0][0][ 0][16] =   3.376370e-02;
  val[3][0][0][ 0][17] =   2.417695e+00; eval[3][0][0][ 0][17] =   4.033109e-02;
  val[3][0][0][ 0][18] =   2.372254e+00; eval[3][0][0][ 0][18] =   3.547710e-02;
  val[3][0][0][ 0][19] =   2.416478e+00; eval[3][0][0][ 0][19] =   5.112745e-02;
  val[3][0][0][ 0][20] =   2.440637e+00; eval[3][0][0][ 0][20] =   6.990879e-02;
  val[3][0][0][ 0][21] =   2.192427e+00; eval[3][0][0][ 0][21] =   7.630069e-02;
  val[3][0][0][ 0][22] =   2.411841e+00; eval[3][0][0][ 0][22] =   1.233706e-01;
  val[3][0][0][ 0][23] =   2.324652e+00; eval[3][0][0][ 0][23] =   1.754804e-01;
  val[3][0][0][ 0][24] =   2.313400e+00; eval[3][0][0][ 0][24] =   1.978344e-01;
  val[3][0][0][ 0][25] =   2.428880e+00; eval[3][0][0][ 0][25] =   2.165474e-01;
  val[3][0][0][ 0][26] =   2.414831e+00; eval[3][0][0][ 0][26] =   2.462878e-01;
  val[3][0][0][ 0][27] =   2.304841e+00; eval[3][0][0][ 0][27] =   3.925003e-01;
  //emcdz_sigma_c0d0z1
  n_val[3][0][0][1] = 28;
  val[3][0][0][ 1][ 0] =   3.273000e+00; eval[3][0][0][ 1][ 0] =   1.712450e-02;
  val[3][0][0][ 1][ 1] =   2.785197e+00; eval[3][0][0][ 1][ 1] =   1.677292e-02;
  val[3][0][0][ 1][ 2] =   2.642222e+00; eval[3][0][0][ 1][ 2] =   1.901580e-02;
  val[3][0][0][ 1][ 3] =   2.462001e+00; eval[3][0][0][ 1][ 3] =   1.240322e-03;
  val[3][0][0][ 1][ 4] =   2.406725e+00; eval[3][0][0][ 1][ 4] =   1.522239e-03;
  val[3][0][0][ 1][ 5] =   2.340085e+00; eval[3][0][0][ 1][ 5] =   1.896717e-03;
  val[3][0][0][ 1][ 6] =   2.323093e+00; eval[3][0][0][ 1][ 6] =   2.298991e-03;
  val[3][0][0][ 1][ 7] =   2.311594e+00; eval[3][0][0][ 1][ 7] =   3.045449e-03;
  val[3][0][0][ 1][ 8] =   2.170018e+00; eval[3][0][0][ 1][ 8] =   4.116623e-03;
  val[3][0][0][ 1][ 9] =   2.201445e+00; eval[3][0][0][ 1][ 9] =   5.549287e-03;
  val[3][0][0][ 1][10] =   2.141834e+00; eval[3][0][0][ 1][10] =   6.943215e-03;
  val[3][0][0][ 1][11] =   2.146964e+00; eval[3][0][0][ 1][11] =   9.085233e-03;
  val[3][0][0][ 1][12] =   2.180484e+00; eval[3][0][0][ 1][12] =   1.196376e-02;
  val[3][0][0][ 1][13] =   2.127346e+00; eval[3][0][0][ 1][13] =   1.455833e-02;
  val[3][0][0][ 1][14] =   2.128477e+00; eval[3][0][0][ 1][14] =   1.801965e-02;
  val[3][0][0][ 1][15] =   2.124790e+00; eval[3][0][0][ 1][15] =   2.240098e-02;
  val[3][0][0][ 1][16] =   2.143716e+00; eval[3][0][0][ 1][16] =   2.807009e-02;
  val[3][0][0][ 1][17] =   2.131366e+00; eval[3][0][0][ 1][17] =   3.376996e-02;
  val[3][0][0][ 1][18] =   2.053196e+00; eval[3][0][0][ 1][18] =   2.994869e-02;
  val[3][0][0][ 1][19] =   2.070982e+00; eval[3][0][0][ 1][19] =   4.349791e-02;
  val[3][0][0][ 1][20] =   1.960338e+00; eval[3][0][0][ 1][20] =   5.163473e-02;
  val[3][0][0][ 1][21] =   2.095758e+00; eval[3][0][0][ 1][21] =   7.127641e-02;
  val[3][0][0][ 1][22] =   2.246918e+00; eval[3][0][0][ 1][22] =   1.186669e-01;
  val[3][0][0][ 1][23] =   2.036001e+00; eval[3][0][0][ 1][23] =   1.051100e-01;
  val[3][0][0][ 1][24] =   2.055348e+00; eval[3][0][0][ 1][24] =   1.556795e-01;
  val[3][0][0][ 1][25] =   1.893952e+00; eval[3][0][0][ 1][25] =   1.341026e-01;
  val[3][0][0][ 1][26] =   1.569207e+00; eval[3][0][0][ 1][26] =   1.787476e-01;
  val[3][0][0][ 1][27] =   2.291716e+00; eval[3][0][0][ 1][27] =   2.920415e-01;
  //emcdz_sigma_c0d0z2
  n_val[3][0][0][2] = 28;
  val[3][0][0][ 2][ 0] =   3.147942e+00; eval[3][0][0][ 2][ 0] =   1.566767e-02;
  val[3][0][0][ 2][ 1] =   2.649409e+00; eval[3][0][0][ 2][ 1] =   1.491870e-02;
  val[3][0][0][ 2][ 2] =   2.452551e+00; eval[3][0][0][ 2][ 2] =   1.827082e-02;
  val[3][0][0][ 2][ 3] =   2.331630e+00; eval[3][0][0][ 2][ 3] =   1.095125e-03;
  val[3][0][0][ 2][ 4] =   2.267901e+00; eval[3][0][0][ 2][ 4] =   1.349484e-03;
  val[3][0][0][ 2][ 5] =   2.132884e+00; eval[3][0][0][ 2][ 5] =   1.767570e-03;
  val[3][0][0][ 2][ 6] =   2.105653e+00; eval[3][0][0][ 2][ 6] =   2.123914e-03;
  val[3][0][0][ 2][ 7] =   2.103624e+00; eval[3][0][0][ 2][ 7] =   2.821945e-03;
  val[3][0][0][ 2][ 8] =   2.089270e+00; eval[3][0][0][ 2][ 8] =   3.684773e-03;
  val[3][0][0][ 2][ 9] =   2.076542e+00; eval[3][0][0][ 2][ 9] =   4.784630e-03;
  val[3][0][0][ 2][10] =   2.051618e+00; eval[3][0][0][ 2][10] =   6.153220e-03;
  val[3][0][0][ 2][11] =   2.027452e+00; eval[3][0][0][ 2][11] =   7.742634e-03;
  val[3][0][0][ 2][12] =   2.027882e+00; eval[3][0][0][ 2][12] =   9.968477e-03;
  val[3][0][0][ 2][13] =   2.029234e+00; eval[3][0][0][ 2][13] =   1.291654e-02;
  val[3][0][0][ 2][14] =   1.987128e+00; eval[3][0][0][ 2][14] =   1.568339e-02;
  val[3][0][0][ 2][15] =   1.998873e+00; eval[3][0][0][ 2][15] =   1.922745e-02;
  val[3][0][0][ 2][16] =   2.013659e+00; eval[3][0][0][ 2][16] =   2.388461e-02;
  val[3][0][0][ 2][17] =   1.935069e+00; eval[3][0][0][ 2][17] =   2.628400e-02;
  val[3][0][0][ 2][18] =   1.967074e+00; eval[3][0][0][ 2][18] =   2.636145e-02;
  val[3][0][0][ 2][19] =   1.925350e+00; eval[3][0][0][ 2][19] =   3.960559e-02;
  val[3][0][0][ 2][20] =   1.946794e+00; eval[3][0][0][ 2][20] =   4.806304e-02;
  val[3][0][0][ 2][21] =   2.021346e+00; eval[3][0][0][ 2][21] =   6.933243e-02;
  val[3][0][0][ 2][22] =   2.007802e+00; eval[3][0][0][ 2][22] =   1.007074e-01;
  val[3][0][0][ 2][23] =   1.782337e+00; eval[3][0][0][ 2][23] =   9.649370e-02;
  val[3][0][0][ 2][24] =   1.799461e+00; eval[3][0][0][ 2][24] =   1.053609e-01;
  val[3][0][0][ 2][25] =   1.953796e+00; eval[3][0][0][ 2][25] =   1.534709e-01;
  val[3][0][0][ 2][26] =   2.478800e+00; eval[3][0][0][ 2][26] =   2.889580e-01;
  val[3][0][0][ 2][27] =   1.927605e+00; eval[3][0][0][ 2][27] =   3.574616e-01;
  //emcdz_sigma_c0d0z3
  n_val[3][0][0][3] = 28;
  val[3][0][0][ 3][ 0] =   3.076765e+00; eval[3][0][0][ 3][ 0] =   1.564624e-02;
  val[3][0][0][ 3][ 1] =   2.543608e+00; eval[3][0][0][ 3][ 1] =   1.376130e-02;
  val[3][0][0][ 3][ 2] =   2.345315e+00; eval[3][0][0][ 3][ 2] =   1.679870e-02;
  val[3][0][0][ 3][ 3] =   2.225964e+00; eval[3][0][0][ 3][ 3] =   9.951933e-04;
  val[3][0][0][ 3][ 4] =   2.093602e+00; eval[3][0][0][ 3][ 4] =   1.288187e-03;
  val[3][0][0][ 3][ 5] =   2.039717e+00; eval[3][0][0][ 3][ 5] =   1.611175e-03;
  val[3][0][0][ 3][ 6] =   2.002936e+00; eval[3][0][0][ 3][ 6] =   1.918568e-03;
  val[3][0][0][ 3][ 7] =   1.971684e+00; eval[3][0][0][ 3][ 7] =   2.469020e-03;
  val[3][0][0][ 3][ 8] =   1.948697e+00; eval[3][0][0][ 3][ 8] =   3.195769e-03;
  val[3][0][0][ 3][ 9] =   1.859852e+00; eval[3][0][0][ 3][ 9] =   4.399549e-03;
  val[3][0][0][ 3][10] =   1.845483e+00; eval[3][0][0][ 3][10] =   5.674041e-03;
  val[3][0][0][ 3][11] =   1.823955e+00; eval[3][0][0][ 3][11] =   7.127951e-03;
  val[3][0][0][ 3][12] =   1.814767e+00; eval[3][0][0][ 3][12] =   9.083796e-03;
  val[3][0][0][ 3][13] =   1.793698e+00; eval[3][0][0][ 3][13] =   1.134482e-02;
  val[3][0][0][ 3][14] =   1.801603e+00; eval[3][0][0][ 3][14] =   1.431054e-02;
  val[3][0][0][ 3][15] =   1.766171e+00; eval[3][0][0][ 3][15] =   1.700762e-02;
  val[3][0][0][ 3][16] =   1.757111e+00; eval[3][0][0][ 3][16] =   2.022954e-02;
  val[3][0][0][ 3][17] =   1.741392e+00; eval[3][0][0][ 3][17] =   2.508411e-02;
  val[3][0][0][ 3][18] =   1.747398e+00; eval[3][0][0][ 3][18] =   2.411324e-02;
  val[3][0][0][ 3][19] =   1.731710e+00; eval[3][0][0][ 3][19] =   3.347663e-02;
  val[3][0][0][ 3][20] =   1.711748e+00; eval[3][0][0][ 3][20] =   4.317637e-02;
  val[3][0][0][ 3][21] =   1.671361e+00; eval[3][0][0][ 3][21] =   5.195035e-02;
  val[3][0][0][ 3][22] =   1.765038e+00; eval[3][0][0][ 3][22] =   7.457399e-02;
  val[3][0][0][ 3][23] =   1.810397e+00; eval[3][0][0][ 3][23] =   9.594104e-02;
  val[3][0][0][ 3][24] =   1.505648e+00; eval[3][0][0][ 3][24] =   8.627272e-02;
  val[3][0][0][ 3][25] =   1.425405e+00; eval[3][0][0][ 3][25] =   9.483119e-02;
  val[3][0][0][ 3][26] =   1.675269e+00; eval[3][0][0][ 3][26] =   1.560887e-01;
  val[3][0][0][ 3][27] =   1.506848e+00; eval[3][0][0][ 3][27] =   1.486174e-01;
  //emcdz_sigma_c0d0z4
  n_val[3][0][0][4] = 28;
  val[3][0][0][ 4][ 0] =   2.924164e+00; eval[3][0][0][ 4][ 0] =   1.851147e-02;
  val[3][0][0][ 4][ 1] =   2.495882e+00; eval[3][0][0][ 4][ 1] =   1.556530e-02;
  val[3][0][0][ 4][ 2] =   2.321820e+00; eval[3][0][0][ 4][ 2] =   1.859431e-02;
  val[3][0][0][ 4][ 3] =   2.229737e+00; eval[3][0][0][ 4][ 3] =   1.126572e-03;
  val[3][0][0][ 4][ 4] =   2.122825e+00; eval[3][0][0][ 4][ 4] =   1.493137e-03;
  val[3][0][0][ 4][ 5] =   2.078995e+00; eval[3][0][0][ 4][ 5] =   1.880922e-03;
  val[3][0][0][ 4][ 6] =   2.044908e+00; eval[3][0][0][ 4][ 6] =   2.272468e-03;
  val[3][0][0][ 4][ 7] =   2.023186e+00; eval[3][0][0][ 4][ 7] =   2.946992e-03;
  val[3][0][0][ 4][ 8] =   2.006259e+00; eval[3][0][0][ 4][ 8] =   3.833522e-03;
  val[3][0][0][ 4][ 9] =   1.979524e+00; eval[3][0][0][ 4][ 9] =   4.868835e-03;
  val[3][0][0][ 4][10] =   1.911776e+00; eval[3][0][0][ 4][10] =   6.904339e-03;
  val[3][0][0][ 4][11] =   1.902649e+00; eval[3][0][0][ 4][11] =   8.891738e-03;
  val[3][0][0][ 4][12] =   1.893203e+00; eval[3][0][0][ 4][12] =   1.187673e-02;
  val[3][0][0][ 4][13] =   1.875792e+00; eval[3][0][0][ 4][13] =   1.389292e-02;
  val[3][0][0][ 4][14] =   1.873936e+00; eval[3][0][0][ 4][14] =   1.734468e-02;
  val[3][0][0][ 4][15] =   1.864304e+00; eval[3][0][0][ 4][15] =   2.178866e-02;
  val[3][0][0][ 4][16] =   1.826602e+00; eval[3][0][0][ 4][16] =   2.505613e-02;
  val[3][0][0][ 4][17] =   1.773793e+00; eval[3][0][0][ 4][17] =   2.908155e-02;
  val[3][0][0][ 4][18] =   1.851033e+00; eval[3][0][0][ 4][18] =   2.976343e-02;
  val[3][0][0][ 4][19] =   1.859263e+00; eval[3][0][0][ 4][19] =   4.386597e-02;
  val[3][0][0][ 4][20] =   1.722882e+00; eval[3][0][0][ 4][20] =   4.904721e-02;
  val[3][0][0][ 4][21] =   1.789658e+00; eval[3][0][0][ 4][21] =   8.697282e-02;
  val[3][0][0][ 4][22] =   1.843009e+00; eval[3][0][0][ 4][22] =   7.860593e-02;
  val[3][0][0][ 4][23] =   1.557049e+00; eval[3][0][0][ 4][23] =   6.908256e-02;
  val[3][0][0][ 4][24] =   1.908219e+00; eval[3][0][0][ 4][24] =   1.822881e-01;
  val[3][0][0][ 4][25] =   1.552887e+00; eval[3][0][0][ 4][25] =   1.426826e-01;
  val[3][0][0][ 4][26] =   1.460329e+00; eval[3][0][0][ 4][26] =   1.878057e-01;
  val[3][0][0][ 4][27] =   1.795869e+00; eval[3][0][0][ 4][27] =   2.312303e-01;
  //emcdz_sigma_c0d0z5
  n_val[3][0][0][5] = 28;
  val[3][0][0][ 5][ 0] =   3.050708e+00; eval[3][0][0][ 5][ 0] =   1.956038e-02;
  val[3][0][0][ 5][ 1] =   2.532015e+00; eval[3][0][0][ 5][ 1] =   1.786279e-02;
  val[3][0][0][ 5][ 2] =   2.339047e+00; eval[3][0][0][ 5][ 2] =   2.132215e-02;
  val[3][0][0][ 5][ 3] =   2.239718e+00; eval[3][0][0][ 5][ 3] =   1.343952e-03;
  val[3][0][0][ 5][ 4] =   2.174574e+00; eval[3][0][0][ 5][ 4] =   1.671703e-03;
  val[3][0][0][ 5][ 5] =   2.127067e+00; eval[3][0][0][ 5][ 5] =   2.094323e-03;
  val[3][0][0][ 5][ 6] =   2.092442e+00; eval[3][0][0][ 5][ 6] =   2.545061e-03;
  val[3][0][0][ 5][ 7] =   1.999830e+00; eval[3][0][0][ 5][ 7] =   3.485200e-03;
  val[3][0][0][ 5][ 8] =   1.980356e+00; eval[3][0][0][ 5][ 8] =   4.589869e-03;
  val[3][0][0][ 5][ 9] =   1.967418e+00; eval[3][0][0][ 5][ 9] =   5.931097e-03;
  val[3][0][0][ 5][10] =   1.926144e+00; eval[3][0][0][ 5][10] =   7.707110e-03;
  val[3][0][0][ 5][11] =   1.920972e+00; eval[3][0][0][ 5][11] =   9.566954e-03;
  val[3][0][0][ 5][12] =   1.909147e+00; eval[3][0][0][ 5][12] =   1.263075e-02;
  val[3][0][0][ 5][13] =   1.858586e+00; eval[3][0][0][ 5][13] =   1.510017e-02;
  val[3][0][0][ 5][14] =   1.870455e+00; eval[3][0][0][ 5][14] =   1.844135e-02;
  val[3][0][0][ 5][15] =   1.769356e+00; eval[3][0][0][ 5][15] =   2.427185e-02;
  val[3][0][0][ 5][16] =   1.861016e+00; eval[3][0][0][ 5][16] =   2.762886e-02;
  val[3][0][0][ 5][17] =   1.810858e+00; eval[3][0][0][ 5][17] =   4.116040e-02;
  val[3][0][0][ 5][18] =   1.783058e+00; eval[3][0][0][ 5][18] =   3.253011e-02;
  val[3][0][0][ 5][19] =   1.686453e+00; eval[3][0][0][ 5][19] =   4.156245e-02;
  val[3][0][0][ 5][20] =   1.781652e+00; eval[3][0][0][ 5][20] =   6.462704e-02;
  val[3][0][0][ 5][21] =   1.822314e+00; eval[3][0][0][ 5][21] =   7.796215e-02;
  val[3][0][0][ 5][22] =   1.695212e+00; eval[3][0][0][ 5][22] =   8.662291e-02;
  val[3][0][0][ 5][23] =   1.742125e+00; eval[3][0][0][ 5][23] =   1.509158e-01;
  val[3][0][0][ 5][24] =   1.294055e+00; eval[3][0][0][ 5][24] =   1.539938e-01;
  val[3][0][0][ 5][25] =   1.944860e+00; eval[3][0][0][ 5][25] =   2.052020e-01;
  val[3][0][0][ 5][26] =   1.011999e+00; eval[3][0][0][ 5][26] =   1.689493e-01;
  val[3][0][0][ 5][27] =   2.073425e+00; eval[3][0][0][ 5][27] =   5.605484e-01;
  //emcdz_sigma_c0d0z6
  n_val[3][0][0][6] = 28;
  val[3][0][0][ 6][ 0] =   3.116029e+00; eval[3][0][0][ 6][ 0] =   1.751267e-02;
  val[3][0][0][ 6][ 1] =   2.586145e+00; eval[3][0][0][ 6][ 1] =   1.561894e-02;
  val[3][0][0][ 6][ 2] =   2.421719e+00; eval[3][0][0][ 6][ 2] =   1.743951e-02;
  val[3][0][0][ 6][ 3] =   2.253475e+00; eval[3][0][0][ 6][ 3] =   1.169136e-03;
  val[3][0][0][ 6][ 4] =   2.183417e+00; eval[3][0][0][ 6][ 4] =   1.438976e-03;
  val[3][0][0][ 6][ 5] =   2.131979e+00; eval[3][0][0][ 6][ 5] =   1.806357e-03;
  val[3][0][0][ 6][ 6] =   2.090183e+00; eval[3][0][0][ 6][ 6] =   2.141814e-03;
  val[3][0][0][ 6][ 7] =   2.058055e+00; eval[3][0][0][ 6][ 7] =   2.797129e-03;
  val[3][0][0][ 6][ 8] =   2.032040e+00; eval[3][0][0][ 6][ 8] =   3.639104e-03;
  val[3][0][0][ 6][ 9] =   1.959780e+00; eval[3][0][0][ 6][ 9] =   5.003223e-03;
  val[3][0][0][ 6][10] =   1.940088e+00; eval[3][0][0][ 6][10] =   6.401730e-03;
  val[3][0][0][ 6][11] =   1.907743e+00; eval[3][0][0][ 6][11] =   8.171325e-03;
  val[3][0][0][ 6][12] =   1.902293e+00; eval[3][0][0][ 6][12] =   1.051315e-02;
  val[3][0][0][ 6][13] =   1.887581e+00; eval[3][0][0][ 6][13] =   1.292522e-02;
  val[3][0][0][ 6][14] =   1.888347e+00; eval[3][0][0][ 6][14] =   1.590826e-02;
  val[3][0][0][ 6][15] =   1.863946e+00; eval[3][0][0][ 6][15] =   2.002857e-02;
  val[3][0][0][ 6][16] =   1.818976e+00; eval[3][0][0][ 6][16] =   2.480516e-02;
  val[3][0][0][ 6][17] =   1.865269e+00; eval[3][0][0][ 6][17] =   2.868792e-02;
  val[3][0][0][ 6][18] =   1.776623e+00; eval[3][0][0][ 6][18] =   2.606533e-02;
  val[3][0][0][ 6][19] =   1.759294e+00; eval[3][0][0][ 6][19] =   3.337298e-02;
  val[3][0][0][ 6][20] =   1.865223e+00; eval[3][0][0][ 6][20] =   4.894534e-02;
  val[3][0][0][ 6][21] =   1.977760e+00; eval[3][0][0][ 6][21] =   7.099074e-02;
  val[3][0][0][ 6][22] =   1.645356e+00; eval[3][0][0][ 6][22] =   7.060201e-02;
  val[3][0][0][ 6][23] =   1.571115e+00; eval[3][0][0][ 6][23] =   7.065648e-02;
  val[3][0][0][ 6][24] =   1.867480e+00; eval[3][0][0][ 6][24] =   1.483898e-01;
  val[3][0][0][ 6][25] =   1.093295e+01; eval[3][0][0][ 6][25] =   6.007692e+03;
  val[3][0][0][ 6][26] =   7.314770e+03; eval[3][0][0][ 6][26] =   6.211948e+03;
  val[3][0][0][ 6][27] =   1.598306e+00; eval[3][0][0][ 6][27] =   2.606327e-01;
  //emcdz_sigma_c0d0z7
  n_val[3][0][0][7] = 28;
  val[3][0][0][ 7][ 0] =   3.205305e+00; eval[3][0][0][ 7][ 0] =   1.815181e-02;
  val[3][0][0][ 7][ 1] =   2.717794e+00; eval[3][0][0][ 7][ 1] =   1.556221e-02;
  val[3][0][0][ 7][ 2] =   2.543114e+00; eval[3][0][0][ 7][ 2] =   1.909498e-02;
  val[3][0][0][ 7][ 3] =   2.407457e+00; eval[3][0][0][ 7][ 3] =   1.209165e-03;
  val[3][0][0][ 7][ 4] =   2.329111e+00; eval[3][0][0][ 7][ 4] =   1.501906e-03;
  val[3][0][0][ 7][ 5] =   2.218104e+00; eval[3][0][0][ 7][ 5] =   1.973544e-03;
  val[3][0][0][ 7][ 6] =   2.193532e+00; eval[3][0][0][ 7][ 6] =   2.362786e-03;
  val[3][0][0][ 7][ 7] =   2.164807e+00; eval[3][0][0][ 7][ 7] =   3.101967e-03;
  val[3][0][0][ 7][ 8] =   2.141522e+00; eval[3][0][0][ 7][ 8] =   4.048415e-03;
  val[3][0][0][ 7][ 9] =   2.097538e+00; eval[3][0][0][ 7][ 9] =   5.094324e-03;
  val[3][0][0][ 7][10] =   2.075999e+00; eval[3][0][0][ 7][10] =   6.544504e-03;
  val[3][0][0][ 7][11] =   2.091660e+00; eval[3][0][0][ 7][11] =   8.808776e-03;
  val[3][0][0][ 7][12] =   2.063104e+00; eval[3][0][0][ 7][12] =   1.088953e-02;
  val[3][0][0][ 7][13] =   2.083374e+00; eval[3][0][0][ 7][13] =   1.394151e-02;
  val[3][0][0][ 7][14] =   2.049327e+00; eval[3][0][0][ 7][14] =   1.705217e-02;
  val[3][0][0][ 7][15] =   2.015695e+00; eval[3][0][0][ 7][15] =   2.012288e-02;
  val[3][0][0][ 7][16] =   2.041923e+00; eval[3][0][0][ 7][16] =   2.641033e-02;
  val[3][0][0][ 7][17] =   1.989836e+00; eval[3][0][0][ 7][17] =   3.279282e-02;
  val[3][0][0][ 7][18] =   1.948182e+00; eval[3][0][0][ 7][18] =   2.824660e-02;
  val[3][0][0][ 7][19] =   1.976298e+00; eval[3][0][0][ 7][19] =   4.151640e-02;
  val[3][0][0][ 7][20] =   1.978577e+00; eval[3][0][0][ 7][20] =   4.712124e-02;
  val[3][0][0][ 7][21] =   1.963684e+00; eval[3][0][0][ 7][21] =   6.792907e-02;
  val[3][0][0][ 7][22] =   2.021367e+00; eval[3][0][0][ 7][22] =   8.771378e-02;
  val[3][0][0][ 7][23] =   1.938899e+00; eval[3][0][0][ 7][23] =   9.520100e-02;
  val[3][0][0][ 7][24] =   2.102698e+00; eval[3][0][0][ 7][24] =   1.378498e-01;
  val[3][0][0][ 7][25] =   2.011827e+00; eval[3][0][0][ 7][25] =   1.622275e-01;
  val[3][0][0][ 7][26] =   1.600040e+00; eval[3][0][0][ 7][26] =   2.776336e-01;
  val[3][0][0][ 7][27] =   2.150246e+00; eval[3][0][0][ 7][27] =   2.755435e-01;
  //emcdz_sigma_c0d0z8
  n_val[3][0][0][8] = 28;
  val[3][0][0][ 8][ 0] =   3.298127e+00; eval[3][0][0][ 8][ 0] =   1.969243e-02;
  val[3][0][0][ 8][ 1] =   2.850318e+00; eval[3][0][0][ 8][ 1] =   1.786608e-02;
  val[3][0][0][ 8][ 2] =   2.697464e+00; eval[3][0][0][ 8][ 2] =   2.027087e-02;
  val[3][0][0][ 8][ 3] =   2.605507e+00; eval[3][0][0][ 8][ 3] =   1.339762e-03;
  val[3][0][0][ 8][ 4] =   2.436694e+00; eval[3][0][0][ 8][ 4] =   1.731975e-03;
  val[3][0][0][ 8][ 5] =   2.410336e+00; eval[3][0][0][ 8][ 5] =   2.195567e-03;
  val[3][0][0][ 8][ 6] =   2.395985e+00; eval[3][0][0][ 8][ 6] =   2.630374e-03;
  val[3][0][0][ 8][ 7] =   2.382611e+00; eval[3][0][0][ 8][ 7] =   3.493647e-03;
  val[3][0][0][ 8][ 8] =   2.330967e+00; eval[3][0][0][ 8][ 8] =   4.537159e-03;
  val[3][0][0][ 8][ 9] =   2.292505e+00; eval[3][0][0][ 8][ 9] =   5.777670e-03;
  val[3][0][0][ 8][10] =   2.284748e+00; eval[3][0][0][ 8][10] =   7.598834e-03;
  val[3][0][0][ 8][11] =   2.305765e+00; eval[3][0][0][ 8][11] =   9.925472e-03;
  val[3][0][0][ 8][12] =   2.282829e+00; eval[3][0][0][ 8][12] =   1.233112e-02;
  val[3][0][0][ 8][13] =   2.234684e+00; eval[3][0][0][ 8][13] =   1.532942e-02;
  val[3][0][0][ 8][14] =   2.246140e+00; eval[3][0][0][ 8][14] =   1.894359e-02;
  val[3][0][0][ 8][15] =   2.257576e+00; eval[3][0][0][ 8][15] =   2.332676e-02;
  val[3][0][0][ 8][16] =   2.212809e+00; eval[3][0][0][ 8][16] =   2.826254e-02;
  val[3][0][0][ 8][17] =   2.192620e+00; eval[3][0][0][ 8][17] =   3.394375e-02;
  val[3][0][0][ 8][18] =   2.266242e+00; eval[3][0][0][ 8][18] =   3.400501e-02;
  val[3][0][0][ 8][19] =   2.160832e+00; eval[3][0][0][ 8][19] =   4.154509e-02;
  val[3][0][0][ 8][20] =   2.185520e+00; eval[3][0][0][ 8][20] =   5.691232e-02;
  val[3][0][0][ 8][21] =   2.314478e+00; eval[3][0][0][ 8][21] =   9.322974e-02;
  val[3][0][0][ 8][22] =   2.105643e+00; eval[3][0][0][ 8][22] =   9.047387e-02;
  val[3][0][0][ 8][23] =   2.119971e+00; eval[3][0][0][ 8][23] =   1.022513e-01;
  val[3][0][0][ 8][24] =   2.422413e+00; eval[3][0][0][ 8][24] =   1.815532e-01;
  val[3][0][0][ 8][25] =   1.753197e+00; eval[3][0][0][ 8][25] =   1.605015e-01;
  val[3][0][0][ 8][26] =   2.036036e+00; eval[3][0][0][ 8][26] =   2.276495e-01;
  val[3][0][0][ 8][27] =   2.107135e+00; eval[3][0][0][ 8][27] =   4.006720e-01;
  //emcdz_sigma_c0d0z9
  n_val[3][0][0][9] = 28;
  val[3][0][0][ 9][ 0] =   3.545575e+00; eval[3][0][0][ 9][ 0] =   2.170010e-02;
  val[3][0][0][ 9][ 1] =   3.103844e+00; eval[3][0][0][ 9][ 1] =   2.060460e-02;
  val[3][0][0][ 9][ 2] =   2.964867e+00; eval[3][0][0][ 9][ 2] =   2.316535e-02;
  val[3][0][0][ 9][ 3] =   2.817196e+00; eval[3][0][0][ 9][ 3] =   1.596281e-03;
  val[3][0][0][ 9][ 4] =   2.720217e+00; eval[3][0][0][ 9][ 4] =   2.010908e-03;
  val[3][0][0][ 9][ 5] =   2.722272e+00; eval[3][0][0][ 9][ 5] =   2.617417e-03;
  val[3][0][0][ 9][ 6] =   2.733109e+00; eval[3][0][0][ 9][ 6] =   3.159017e-03;
  val[3][0][0][ 9][ 7] =   2.667808e+00; eval[3][0][0][ 9][ 7] =   4.150721e-03;
  val[3][0][0][ 9][ 8] =   2.743106e+00; eval[3][0][0][ 9][ 8] =   5.371099e-03;
  val[3][0][0][ 9][ 9] =   2.731661e+00; eval[3][0][0][ 9][ 9] =   7.053460e-03;
  val[3][0][0][ 9][10] =   2.707184e+00; eval[3][0][0][ 9][10] =   9.160344e-03;
  val[3][0][0][ 9][11] =   2.613046e+00; eval[3][0][0][ 9][11] =   1.212204e-02;
  val[3][0][0][ 9][12] =   2.539622e+00; eval[3][0][0][ 9][12] =   1.512565e-02;
  val[3][0][0][ 9][13] =   2.583441e+00; eval[3][0][0][ 9][13] =   1.940980e-02;
  val[3][0][0][ 9][14] =   2.584423e+00; eval[3][0][0][ 9][14] =   2.474986e-02;
  val[3][0][0][ 9][15] =   2.587665e+00; eval[3][0][0][ 9][15] =   3.034811e-02;
  val[3][0][0][ 9][16] =   2.532208e+00; eval[3][0][0][ 9][16] =   3.617437e-02;
  val[3][0][0][ 9][17] =   2.575421e+00; eval[3][0][0][ 9][17] =   4.311964e-02;
  val[3][0][0][ 9][18] =   2.584254e+00; eval[3][0][0][ 9][18] =   4.160069e-02;
  val[3][0][0][ 9][19] =   2.662829e+00; eval[3][0][0][ 9][19] =   6.154056e-02;
  val[3][0][0][ 9][20] =   2.731503e+00; eval[3][0][0][ 9][20] =   7.859004e-02;
  val[3][0][0][ 9][21] =   2.560358e+00; eval[3][0][0][ 9][21] =   9.449630e-02;
  val[3][0][0][ 9][22] =   2.740204e+00; eval[3][0][0][ 9][22] =   1.479836e-01;
  val[3][0][0][ 9][23] =   3.245593e+03; eval[3][0][0][ 9][23] =   8.052817e+03;
  val[3][0][0][ 9][24] =   2.222576e+00; eval[3][0][0][ 9][24] =   2.153577e-01;
  val[3][0][0][ 9][25] =   3.175530e+00; eval[3][0][0][ 9][25] =   3.576164e-01;
  val[3][0][0][ 9][26] =   3.754646e-01; eval[3][0][0][ 9][26] =   7.140245e+03;
  val[3][0][0][ 9][27] =   2.741213e+00; eval[3][0][0][ 9][27] =   6.588025e-01;
  //emcdz_sigma_c0d1z0
  n_val[3][0][1][0] = 28;
  val[3][0][1][ 0][ 0] =   3.054812e+00; eval[3][0][1][ 0][ 0] =   1.809151e-02;
  val[3][0][1][ 0][ 1] =   2.773329e+00; eval[3][0][1][ 0][ 1] =   1.900522e-02;
  val[3][0][1][ 0][ 2] =   2.684508e+00; eval[3][0][1][ 0][ 2] =   2.419850e-02;
  val[3][0][1][ 0][ 3] =   2.637702e+00; eval[3][0][1][ 0][ 3] =   1.470070e-03;
  val[3][0][1][ 0][ 4] =   2.619072e+00; eval[3][0][1][ 0][ 4] =   1.886949e-03;
  val[3][0][1][ 0][ 5] =   2.634549e+00; eval[3][0][1][ 0][ 5] =   2.462134e-03;
  val[3][0][1][ 0][ 6] =   2.627146e+00; eval[3][0][1][ 0][ 6] =   3.000681e-03;
  val[3][0][1][ 0][ 7] =   2.633466e+00; eval[3][0][1][ 0][ 7] =   4.021894e-03;
  val[3][0][1][ 0][ 8] =   2.635001e+00; eval[3][0][1][ 0][ 8] =   5.548808e-03;
  val[3][0][1][ 0][ 9] =   2.631732e+00; eval[3][0][1][ 0][ 9] =   7.288849e-03;
  val[3][0][1][ 0][10] =   2.653870e+00; eval[3][0][1][ 0][10] =   9.851082e-03;
  val[3][0][1][ 0][11] =   2.637791e+00; eval[3][0][1][ 0][11] =   1.268839e-02;
  val[3][0][1][ 0][12] =   2.611465e+00; eval[3][0][1][ 0][12] =   1.671794e-02;
  val[3][0][1][ 0][13] =   2.647975e+00; eval[3][0][1][ 0][13] =   2.156887e-02;
  val[3][0][1][ 0][14] =   2.555012e+00; eval[3][0][1][ 0][14] =   3.006957e-02;
  val[3][0][1][ 0][15] =   2.586199e+00; eval[3][0][1][ 0][15] =   3.227466e-02;
  val[3][0][1][ 0][16] =   2.580140e+00; eval[3][0][1][ 0][16] =   4.580860e-02;
  val[3][0][1][ 0][17] =   2.397630e+00; eval[3][0][1][ 0][17] =   4.989658e-02;
  val[3][0][1][ 0][18] =   2.634294e+00; eval[3][0][1][ 0][18] =   5.508538e-02;
  val[3][0][1][ 0][19] =   2.451319e+00; eval[3][0][1][ 0][19] =   6.874841e-02;
  val[3][0][1][ 0][20] =   2.603380e+00; eval[3][0][1][ 0][20] =   1.049577e-01;
  val[3][0][1][ 0][21] =   2.285190e+00; eval[3][0][1][ 0][21] =   9.651778e-02;
  val[3][0][1][ 0][22] =   2.569784e+00; eval[3][0][1][ 0][22] =   1.828412e-01;
  val[3][0][1][ 0][23] =   2.634543e+00; eval[3][0][1][ 0][23] =   2.006677e-01;
  val[3][0][1][ 0][24] =   2.425582e+00; eval[3][0][1][ 0][24] =   2.680818e-01;
  val[3][0][1][ 0][25] =   1.976940e+00; eval[3][0][1][ 0][25] =   2.826370e-01;
  val[3][0][1][ 0][26] =   4.895336e+00; eval[3][0][1][ 0][26] =   1.805871e+00;
  val[3][0][1][ 0][27] =   4.005673e+00; eval[3][0][1][ 0][27] =   1.416071e+00;
  //emcdz_sigma_c0d1z1
  n_val[3][0][1][1] = 28;
  val[3][0][1][ 1][ 0] =   2.967133e+00; eval[3][0][1][ 1][ 0] =   1.545264e-02;
  val[3][0][1][ 1][ 1] =   2.628525e+00; eval[3][0][1][ 1][ 1] =   1.537886e-02;
  val[3][0][1][ 1][ 2] =   2.487745e+00; eval[3][0][1][ 1][ 2] =   1.893717e-02;
  val[3][0][1][ 1][ 3] =   2.421295e+00; eval[3][0][1][ 1][ 3] =   1.149635e-03;
  val[3][0][1][ 1][ 4] =   2.354172e+00; eval[3][0][1][ 1][ 4] =   1.524873e-03;
  val[3][0][1][ 1][ 5] =   2.325208e+00; eval[3][0][1][ 1][ 5] =   1.979130e-03;
  val[3][0][1][ 1][ 6] =   2.324192e+00; eval[3][0][1][ 1][ 6] =   2.440092e-03;
  val[3][0][1][ 1][ 7] =   2.320513e+00; eval[3][0][1][ 1][ 7] =   3.288016e-03;
  val[3][0][1][ 1][ 8] =   2.302545e+00; eval[3][0][1][ 1][ 8] =   4.413528e-03;
  val[3][0][1][ 1][ 9] =   2.298354e+00; eval[3][0][1][ 1][ 9] =   5.803350e-03;
  val[3][0][1][ 1][10] =   2.300515e+00; eval[3][0][1][ 1][10] =   7.661980e-03;
  val[3][0][1][ 1][11] =   2.300964e+00; eval[3][0][1][ 1][11] =   1.003889e-02;
  val[3][0][1][ 1][12] =   2.305785e+00; eval[3][0][1][ 1][12] =   1.325524e-02;
  val[3][0][1][ 1][13] =   2.309198e+00; eval[3][0][1][ 1][13] =   1.649871e-02;
  val[3][0][1][ 1][14] =   2.301390e+00; eval[3][0][1][ 1][14] =   2.129106e-02;
  val[3][0][1][ 1][15] =   2.176538e+00; eval[3][0][1][ 1][15] =   2.751604e-02;
  val[3][0][1][ 1][16] =   2.204880e+00; eval[3][0][1][ 1][16] =   3.465472e-02;
  val[3][0][1][ 1][17] =   2.123274e+00; eval[3][0][1][ 1][17] =   3.976099e-02;
  val[3][0][1][ 1][18] =   2.339671e+00; eval[3][0][1][ 1][18] =   4.460624e-02;
  val[3][0][1][ 1][19] =   2.180918e+00; eval[3][0][1][ 1][19] =   5.736339e-02;
  val[3][0][1][ 1][20] =   2.190807e+00; eval[3][0][1][ 1][20] =   7.956933e-02;
  val[3][0][1][ 1][21] =   2.117684e+00; eval[3][0][1][ 1][21] =   8.421323e-02;
  val[3][0][1][ 1][22] =   2.261104e+00; eval[3][0][1][ 1][22] =   1.427607e-01;
  val[3][0][1][ 1][23] =   2.047066e+00; eval[3][0][1][ 1][23] =   1.139408e-01;
  val[3][0][1][ 1][24] =   2.188757e+00; eval[3][0][1][ 1][24] =   2.782445e-01;
  val[3][0][1][ 1][25] =   2.666330e+00; eval[3][0][1][ 1][25] =   3.824133e-01;
  val[3][0][1][ 1][26] =   2.082233e+00; eval[3][0][1][ 1][26] =   2.929527e-01;
  val[3][0][1][ 1][27] =   1.250157e-01; eval[3][0][1][ 1][27] =   7.092626e+03;
  //emcdz_sigma_c0d1z2
  n_val[3][0][1][2] = 28;
  val[3][0][1][ 2][ 0] =   2.913540e+00; eval[3][0][1][ 2][ 0] =   1.473750e-02;
  val[3][0][1][ 2][ 1] =   2.486735e+00; eval[3][0][1][ 2][ 1] =   1.495542e-02;
  val[3][0][1][ 2][ 2] =   2.357861e+00; eval[3][0][1][ 2][ 2] =   1.674235e-02;
  val[3][0][1][ 2][ 3] =   2.244772e+00; eval[3][0][1][ 2][ 3] =   1.066727e-03;
  val[3][0][1][ 2][ 4] =   2.206822e+00; eval[3][0][1][ 2][ 4] =   1.314749e-03;
  val[3][0][1][ 2][ 5] =   2.170227e+00; eval[3][0][1][ 2][ 5] =   1.687116e-03;
  val[3][0][1][ 2][ 6] =   2.094276e+00; eval[3][0][1][ 2][ 6] =   2.190113e-03;
  val[3][0][1][ 2][ 7] =   2.083301e+00; eval[3][0][1][ 2][ 7] =   2.922949e-03;
  val[3][0][1][ 2][ 8] =   2.079592e+00; eval[3][0][1][ 2][ 8] =   3.885550e-03;
  val[3][0][1][ 2][ 9] =   2.065971e+00; eval[3][0][1][ 2][ 9] =   5.117573e-03;
  val[3][0][1][ 2][10] =   2.060066e+00; eval[3][0][1][ 2][10] =   6.790200e-03;
  val[3][0][1][ 2][11] =   2.042041e+00; eval[3][0][1][ 2][11] =   8.793462e-03;
  val[3][0][1][ 2][12] =   2.015265e+00; eval[3][0][1][ 2][12] =   1.130969e-02;
  val[3][0][1][ 2][13] =   2.046841e+00; eval[3][0][1][ 2][13] =   1.501246e-02;
  val[3][0][1][ 2][14] =   1.961790e+00; eval[3][0][1][ 2][14] =   1.742333e-02;
  val[3][0][1][ 2][15] =   1.987530e+00; eval[3][0][1][ 2][15] =   2.212133e-02;
  val[3][0][1][ 2][16] =   1.989200e+00; eval[3][0][1][ 2][16] =   2.775106e-02;
  val[3][0][1][ 2][17] =   2.033939e+00; eval[3][0][1][ 2][17] =   3.518093e-02;
  val[3][0][1][ 2][18] =   2.004689e+00; eval[3][0][1][ 2][18] =   3.327369e-02;
  val[3][0][1][ 2][19] =   2.033799e+00; eval[3][0][1][ 2][19] =   4.804108e-02;
  val[3][0][1][ 2][20] =   2.048364e+00; eval[3][0][1][ 2][20] =   7.166682e-02;
  val[3][0][1][ 2][21] =   1.833122e+00; eval[3][0][1][ 2][21] =   7.143895e-02;
  val[3][0][1][ 2][22] =   1.845182e+00; eval[3][0][1][ 2][22] =   9.894121e-02;
  val[3][0][1][ 2][23] =   1.867538e+00; eval[3][0][1][ 2][23] =   1.203190e-01;
  val[3][0][1][ 2][24] =   1.902578e+00; eval[3][0][1][ 2][24] =   2.045553e-01;
  val[3][0][1][ 2][25] =   1.813555e+00; eval[3][0][1][ 2][25] =   1.760147e-01;
  val[3][0][1][ 2][26] =   2.009307e+00; eval[3][0][1][ 2][26] =   3.435236e-01;
  val[3][0][1][ 2][27] =   1.994334e+00; eval[3][0][1][ 2][27] =   4.563176e-01;
  //emcdz_sigma_c0d1z3
  n_val[3][0][1][3] = 28;
  val[3][0][1][ 3][ 0] =   2.905174e+00; eval[3][0][1][ 3][ 0] =   1.451421e-02;
  val[3][0][1][ 3][ 1] =   2.437517e+00; eval[3][0][1][ 3][ 1] =   1.429761e-02;
  val[3][0][1][ 3][ 2] =   2.238873e+00; eval[3][0][1][ 3][ 2] =   1.650268e-02;
  val[3][0][1][ 3][ 3] =   2.150385e+00; eval[3][0][1][ 3][ 3] =   9.823392e-04;
  val[3][0][1][ 3][ 4] =   2.092170e+00; eval[3][0][1][ 3][ 4] =   1.193314e-03;
  val[3][0][1][ 3][ 5] =   2.003683e+00; eval[3][0][1][ 3][ 5] =   1.645844e-03;
  val[3][0][1][ 3][ 6] =   1.975292e+00; eval[3][0][1][ 3][ 6] =   1.996537e-03;
  val[3][0][1][ 3][ 7] =   1.960528e+00; eval[3][0][1][ 3][ 7] =   2.667346e-03;
  val[3][0][1][ 3][ 8] =   1.939025e+00; eval[3][0][1][ 3][ 8] =   3.508795e-03;
  val[3][0][1][ 3][ 9] =   1.929316e+00; eval[3][0][1][ 3][ 9] =   4.653035e-03;
  val[3][0][1][ 3][10] =   1.922770e+00; eval[3][0][1][ 3][10] =   6.059276e-03;
  val[3][0][1][ 3][11] =   1.909230e+00; eval[3][0][1][ 3][11] =   7.955153e-03;
  val[3][0][1][ 3][12] =   1.912135e+00; eval[3][0][1][ 3][12] =   1.036033e-02;
  val[3][0][1][ 3][13] =   1.867463e+00; eval[3][0][1][ 3][13] =   1.279852e-02;
  val[3][0][1][ 3][14] =   1.868017e+00; eval[3][0][1][ 3][14] =   1.673547e-02;
  val[3][0][1][ 3][15] =   1.797856e+00; eval[3][0][1][ 3][15] =   2.051282e-02;
  val[3][0][1][ 3][16] =   1.761212e+00; eval[3][0][1][ 3][16] =   2.492586e-02;
  val[3][0][1][ 3][17] =   1.811284e+00; eval[3][0][1][ 3][17] =   3.273328e-02;
  val[3][0][1][ 3][18] =   1.751186e+00; eval[3][0][1][ 3][18] =   2.879045e-02;
  val[3][0][1][ 3][19] =   1.737734e+00; eval[3][0][1][ 3][19] =   4.122455e-02;
  val[3][0][1][ 3][20] =   1.847979e+00; eval[3][0][1][ 3][20] =   5.852868e-02;
  val[3][0][1][ 3][21] =   1.746791e+00; eval[3][0][1][ 3][21] =   7.098712e-02;
  val[3][0][1][ 3][22] =   1.659115e+00; eval[3][0][1][ 3][22] =   8.920997e-02;
  val[3][0][1][ 3][23] =   1.695395e+00; eval[3][0][1][ 3][23] =   1.495008e-01;
  val[3][0][1][ 3][24] =   1.725675e+00; eval[3][0][1][ 3][24] =   1.511840e-01;
  val[3][0][1][ 3][25] =   1.778631e+00; eval[3][0][1][ 3][25] =   2.379796e-01;
  val[3][0][1][ 3][26] =   1.966975e+00; eval[3][0][1][ 3][26] =   4.691804e-01;
  val[3][0][1][ 3][27] =   1.564449e+00; eval[3][0][1][ 3][27] =   2.933511e-01;
  //emcdz_sigma_c0d1z4
  n_val[3][0][1][4] = 28;
  val[3][0][1][ 4][ 0] =   2.867545e+00; eval[3][0][1][ 4][ 0] =   1.749732e-02;
  val[3][0][1][ 4][ 1] =   2.384035e+00; eval[3][0][1][ 4][ 1] =   1.627657e-02;
  val[3][0][1][ 4][ 2] =   2.186420e+00; eval[3][0][1][ 4][ 2] =   1.872794e-02;
  val[3][0][1][ 4][ 3] =   2.099653e+00; eval[3][0][1][ 4][ 3] =   1.093447e-03;
  val[3][0][1][ 4][ 4] =   2.004276e+00; eval[3][0][1][ 4][ 4] =   1.408983e-03;
  val[3][0][1][ 4][ 5] =   1.967829e+00; eval[3][0][1][ 4][ 5] =   1.794237e-03;
  val[3][0][1][ 4][ 6] =   1.938828e+00; eval[3][0][1][ 4][ 6] =   2.211845e-03;
  val[3][0][1][ 4][ 7] =   1.921872e+00; eval[3][0][1][ 4][ 7] =   2.990062e-03;
  val[3][0][1][ 4][ 8] =   1.915855e+00; eval[3][0][1][ 4][ 8] =   3.989628e-03;
  val[3][0][1][ 4][ 9] =   1.908655e+00; eval[3][0][1][ 4][ 9] =   5.295556e-03;
  val[3][0][1][ 4][10] =   1.907114e+00; eval[3][0][1][ 4][10] =   7.060829e-03;
  val[3][0][1][ 4][11] =   1.868481e+00; eval[3][0][1][ 4][11] =   8.954527e-03;
  val[3][0][1][ 4][12] =   1.821693e+00; eval[3][0][1][ 4][12] =   1.267600e-02;
  val[3][0][1][ 4][13] =   1.808440e+00; eval[3][0][1][ 4][13] =   1.568238e-02;
  val[3][0][1][ 4][14] =   1.813687e+00; eval[3][0][1][ 4][14] =   2.043011e-02;
  val[3][0][1][ 4][15] =   1.833118e+00; eval[3][0][1][ 4][15] =   2.636955e-02;
  val[3][0][1][ 4][16] =   1.788208e+00; eval[3][0][1][ 4][16] =   3.107869e-02;
  val[3][0][1][ 4][17] =   1.785134e+00; eval[3][0][1][ 4][17] =   3.818606e-02;
  val[3][0][1][ 4][18] =   1.722679e+00; eval[3][0][1][ 4][18] =   3.475841e-02;
  val[3][0][1][ 4][19] =   1.741251e+00; eval[3][0][1][ 4][19] =   4.873619e-02;
  val[3][0][1][ 4][20] =   1.849162e+00; eval[3][0][1][ 4][20] =   7.069468e-02;
  val[3][0][1][ 4][21] =   1.654067e+00; eval[3][0][1][ 4][21] =   7.763083e-02;
  val[3][0][1][ 4][22] =   1.792327e+00; eval[3][0][1][ 4][22] =   1.059954e-01;
  val[3][0][1][ 4][23] =   1.629403e+00; eval[3][0][1][ 4][23] =   1.150605e-01;
  val[3][0][1][ 4][24] =   1.645092e+00; eval[3][0][1][ 4][24] =   1.365950e-01;
  val[3][0][1][ 4][25] =   1.799293e+00; eval[3][0][1][ 4][25] =   2.307220e-01;
  val[3][0][1][ 4][26] =   2.279427e+00; eval[3][0][1][ 4][26] =   5.758440e-01;
  val[3][0][1][ 4][27] =   2.137781e+00; eval[3][0][1][ 4][27] =   6.255049e-01;
  //emcdz_sigma_c0d1z5
  n_val[3][0][1][5] = 28;
  val[3][0][1][ 5][ 0] =   2.851460e+00; eval[3][0][1][ 5][ 0] =   1.762639e-02;
  val[3][0][1][ 5][ 1] =   2.379094e+00; eval[3][0][1][ 5][ 1] =   1.606122e-02;
  val[3][0][1][ 5][ 2] =   2.177424e+00; eval[3][0][1][ 5][ 2] =   1.895319e-02;
  val[3][0][1][ 5][ 3] =   2.083232e+00; eval[3][0][1][ 5][ 3] =   1.106525e-03;
  val[3][0][1][ 5][ 4] =   1.980646e+00; eval[3][0][1][ 5][ 4] =   1.422673e-03;
  val[3][0][1][ 5][ 5] =   1.946322e+00; eval[3][0][1][ 5][ 5] =   1.836283e-03;
  val[3][0][1][ 5][ 6] =   1.925067e+00; eval[3][0][1][ 5][ 6] =   2.263353e-03;
  val[3][0][1][ 5][ 7] =   1.906036e+00; eval[3][0][1][ 5][ 7] =   2.995612e-03;
  val[3][0][1][ 5][ 8] =   1.893842e+00; eval[3][0][1][ 5][ 8] =   3.959467e-03;
  val[3][0][1][ 5][ 9] =   1.873782e+00; eval[3][0][1][ 5][ 9] =   5.224150e-03;
  val[3][0][1][ 5][10] =   1.872827e+00; eval[3][0][1][ 5][10] =   6.948510e-03;
  val[3][0][1][ 5][11] =   1.792620e+00; eval[3][0][1][ 5][11] =   9.409238e-03;
  val[3][0][1][ 5][12] =   1.770971e+00; eval[3][0][1][ 5][12] =   1.200062e-02;
  val[3][0][1][ 5][13] =   1.749216e+00; eval[3][0][1][ 5][13] =   1.487609e-02;
  val[3][0][1][ 5][14] =   1.755683e+00; eval[3][0][1][ 5][14] =   1.886987e-02;
  val[3][0][1][ 5][15] =   1.737287e+00; eval[3][0][1][ 5][15] =   2.281272e-02;
  val[3][0][1][ 5][16] =   1.732254e+00; eval[3][0][1][ 5][16] =   2.792909e-02;
  val[3][0][1][ 5][17] =   1.674180e+00; eval[3][0][1][ 5][17] =   3.350803e-02;
  val[3][0][1][ 5][18] =   1.743772e+00; eval[3][0][1][ 5][18] =   3.284276e-02;
  val[3][0][1][ 5][19] =   1.743662e+00; eval[3][0][1][ 5][19] =   4.783713e-02;
  val[3][0][1][ 5][20] =   1.562420e+00; eval[3][0][1][ 5][20] =   4.779742e-02;
  val[3][0][1][ 5][21] =   1.702781e+00; eval[3][0][1][ 5][21] =   9.210657e-02;
  val[3][0][1][ 5][22] =   1.522605e+00; eval[3][0][1][ 5][22] =   9.962252e-02;
  val[3][0][1][ 5][23] =   1.718523e+00; eval[3][0][1][ 5][23] =   1.733660e-01;
  val[3][0][1][ 5][24] =   1.409260e+00; eval[3][0][1][ 5][24] =   1.388259e-01;
  val[3][0][1][ 5][25] =   1.979194e+00; eval[3][0][1][ 5][25] =   3.539074e-01;
  val[3][0][1][ 5][26] =   1.910214e+00; eval[3][0][1][ 5][26] =   8.865138e-01;
  val[3][0][1][ 5][27] =   2.692516e-01; eval[3][0][1][ 5][27] =   5.577220e-02;
  //emcdz_sigma_c0d1z6
  n_val[3][0][1][6] = 28;
  val[3][0][1][ 6][ 0] =   2.874449e+00; eval[3][0][1][ 6][ 0] =   1.526732e-02;
  val[3][0][1][ 6][ 1] =   2.407422e+00; eval[3][0][1][ 6][ 1] =   1.345648e-02;
  val[3][0][1][ 6][ 2] =   2.200400e+00; eval[3][0][1][ 6][ 2] =   1.592646e-02;
  val[3][0][1][ 6][ 3] =   2.092470e+00; eval[3][0][1][ 6][ 3] =   9.242630e-04;
  val[3][0][1][ 6][ 4] =   1.980399e+00; eval[3][0][1][ 6][ 4] =   1.196635e-03;
  val[3][0][1][ 6][ 5] =   1.933164e+00; eval[3][0][1][ 6][ 5] =   1.544406e-03;
  val[3][0][1][ 6][ 6] =   1.906162e+00; eval[3][0][1][ 6][ 6] =   1.881753e-03;
  val[3][0][1][ 6][ 7] =   1.887987e+00; eval[3][0][1][ 6][ 7] =   2.493042e-03;
  val[3][0][1][ 6][ 8] =   1.874718e+00; eval[3][0][1][ 6][ 8] =   3.295448e-03;
  val[3][0][1][ 6][ 9] =   1.856173e+00; eval[3][0][1][ 6][ 9] =   4.316230e-03;
  val[3][0][1][ 6][10] =   1.844408e+00; eval[3][0][1][ 6][10] =   5.746373e-03;
  val[3][0][1][ 6][11] =   1.838134e+00; eval[3][0][1][ 6][11] =   7.556108e-03;
  val[3][0][1][ 6][12] =   1.772627e+00; eval[3][0][1][ 6][12] =   1.015453e-02;
  val[3][0][1][ 6][13] =   1.734788e+00; eval[3][0][1][ 6][13] =   1.242814e-02;
  val[3][0][1][ 6][14] =   1.744935e+00; eval[3][0][1][ 6][14] =   1.590668e-02;
  val[3][0][1][ 6][15] =   1.708916e+00; eval[3][0][1][ 6][15] =   1.950775e-02;
  val[3][0][1][ 6][16] =   1.685630e+00; eval[3][0][1][ 6][16] =   2.339805e-02;
  val[3][0][1][ 6][17] =   1.739693e+00; eval[3][0][1][ 6][17] =   3.023075e-02;
  val[3][0][1][ 6][18] =   1.729630e+00; eval[3][0][1][ 6][18] =   3.062203e-02;
  val[3][0][1][ 6][19] =   1.697595e+00; eval[3][0][1][ 6][19] =   3.763181e-02;
  val[3][0][1][ 6][20] =   1.697672e+00; eval[3][0][1][ 6][20] =   5.239346e-02;
  val[3][0][1][ 6][21] =   1.641126e+00; eval[3][0][1][ 6][21] =   6.318401e-02;
  val[3][0][1][ 6][22] =   1.634953e+00; eval[3][0][1][ 6][22] =   8.538040e-02;
  val[3][0][1][ 6][23] =   1.610968e+00; eval[3][0][1][ 6][23] =   1.031838e-01;
  val[3][0][1][ 6][24] =   1.318996e+00; eval[3][0][1][ 6][24] =   1.421598e-01;
  val[3][0][1][ 6][25] =   1.421193e+00; eval[3][0][1][ 6][25] =   1.510271e-01;
  val[3][0][1][ 6][26] =   1.735095e+00; eval[3][0][1][ 6][26] =   2.844158e-01;
  val[3][0][1][ 6][27] =   3.005055e+00; eval[3][0][1][ 6][27] =   1.484543e+00;
  //emcdz_sigma_c0d1z7
  n_val[3][0][1][7] = 28;
  val[3][0][1][ 7][ 0] =   2.874399e+00; eval[3][0][1][ 7][ 0] =   1.511224e-02;
  val[3][0][1][ 7][ 1] =   2.460537e+00; eval[3][0][1][ 7][ 1] =   1.447414e-02;
  val[3][0][1][ 7][ 2] =   2.268044e+00; eval[3][0][1][ 7][ 2] =   1.721551e-02;
  val[3][0][1][ 7][ 3] =   2.176663e+00; eval[3][0][1][ 7][ 3] =   1.032486e-03;
  val[3][0][1][ 7][ 4] =   2.126516e+00; eval[3][0][1][ 7][ 4] =   1.272916e-03;
  val[3][0][1][ 7][ 5] =   2.016954e+00; eval[3][0][1][ 7][ 5] =   1.704649e-03;
  val[3][0][1][ 7][ 6] =   2.002119e+00; eval[3][0][1][ 7][ 6] =   2.087082e-03;
  val[3][0][1][ 7][ 7] =   1.987014e+00; eval[3][0][1][ 7][ 7] =   2.759837e-03;
  val[3][0][1][ 7][ 8] =   1.976901e+00; eval[3][0][1][ 7][ 8] =   3.675695e-03;
  val[3][0][1][ 7][ 9] =   1.980462e+00; eval[3][0][1][ 7][ 9] =   4.934460e-03;
  val[3][0][1][ 7][10] =   1.967645e+00; eval[3][0][1][ 7][10] =   6.515653e-03;
  val[3][0][1][ 7][11] =   1.940875e+00; eval[3][0][1][ 7][11] =   8.381306e-03;
  val[3][0][1][ 7][12] =   1.939162e+00; eval[3][0][1][ 7][12] =   1.099889e-02;
  val[3][0][1][ 7][13] =   1.933891e+00; eval[3][0][1][ 7][13] =   1.393821e-02;
  val[3][0][1][ 7][14] =   1.967572e+00; eval[3][0][1][ 7][14] =   1.800775e-02;
  val[3][0][1][ 7][15] =   1.942071e+00; eval[3][0][1][ 7][15] =   2.226742e-02;
  val[3][0][1][ 7][16] =   1.826282e+00; eval[3][0][1][ 7][16] =   2.760673e-02;
  val[3][0][1][ 7][17] =   1.831516e+00; eval[3][0][1][ 7][17] =   3.390667e-02;
  val[3][0][1][ 7][18] =   1.764530e+00; eval[3][0][1][ 7][18] =   2.862360e-02;
  val[3][0][1][ 7][19] =   1.908478e+00; eval[3][0][1][ 7][19] =   4.729632e-02;
  val[3][0][1][ 7][20] =   1.757545e+00; eval[3][0][1][ 7][20] =   5.832700e-02;
  val[3][0][1][ 7][21] =   1.945169e+00; eval[3][0][1][ 7][21] =   7.912581e-02;
  val[3][0][1][ 7][22] =   1.905633e+00; eval[3][0][1][ 7][22] =   1.057925e-01;
  val[3][0][1][ 7][23] =   1.946303e+00; eval[3][0][1][ 7][23] =   1.753271e-01;
  val[3][0][1][ 7][24] =   1.742738e+00; eval[3][0][1][ 7][24] =   1.230895e-01;
  val[3][0][1][ 7][25] =   2.089177e+00; eval[3][0][1][ 7][25] =   2.674366e-01;
  val[3][0][1][ 7][26] =   2.100748e+00; eval[3][0][1][ 7][26] =   4.266438e-01;
  val[3][0][1][ 7][27] =   1.925158e+00; eval[3][0][1][ 7][27] =   4.146343e-01;
  //emcdz_sigma_c0d1z8
  n_val[3][0][1][8] = 28;
  val[3][0][1][ 8][ 0] =   2.892786e+00; eval[3][0][1][ 8][ 0] =   1.518368e-02;
  val[3][0][1][ 8][ 1] =   2.520080e+00; eval[3][0][1][ 8][ 1] =   1.537184e-02;
  val[3][0][1][ 8][ 2] =   2.418437e+00; eval[3][0][1][ 8][ 2] =   1.799954e-02;
  val[3][0][1][ 8][ 3] =   2.279606e+00; eval[3][0][1][ 8][ 3] =   1.135544e-03;
  val[3][0][1][ 8][ 4] =   2.218732e+00; eval[3][0][1][ 8][ 4] =   1.385217e-03;
  val[3][0][1][ 8][ 5] =   2.214130e+00; eval[3][0][1][ 8][ 5] =   1.846494e-03;
  val[3][0][1][ 8][ 6] =   2.209812e+00; eval[3][0][1][ 8][ 6] =   2.326035e-03;
  val[3][0][1][ 8][ 7] =   2.179519e+00; eval[3][0][1][ 8][ 7] =   3.112941e-03;
  val[3][0][1][ 8][ 8] =   2.183511e+00; eval[3][0][1][ 8][ 8] =   4.173052e-03;
  val[3][0][1][ 8][ 9] =   2.185305e+00; eval[3][0][1][ 8][ 9] =   5.560704e-03;
  val[3][0][1][ 8][10] =   2.177203e+00; eval[3][0][1][ 8][10] =   7.387606e-03;
  val[3][0][1][ 8][11] =   2.190682e+00; eval[3][0][1][ 8][11] =   9.890233e-03;
  val[3][0][1][ 8][12] =   2.142874e+00; eval[3][0][1][ 8][12] =   1.279775e-02;
  val[3][0][1][ 8][13] =   2.164080e+00; eval[3][0][1][ 8][13] =   1.629704e-02;
  val[3][0][1][ 8][14] =   2.045717e+00; eval[3][0][1][ 8][14] =   2.098216e-02;
  val[3][0][1][ 8][15] =   2.055549e+00; eval[3][0][1][ 8][15] =   2.561307e-02;
  val[3][0][1][ 8][16] =   2.115628e+00; eval[3][0][1][ 8][16] =   3.393251e-02;
  val[3][0][1][ 8][17] =   2.078586e+00; eval[3][0][1][ 8][17] =   4.034594e-02;
  val[3][0][1][ 8][18] =   2.059403e+00; eval[3][0][1][ 8][18] =   3.673093e-02;
  val[3][0][1][ 8][19] =   1.940930e+00; eval[3][0][1][ 8][19] =   5.024069e-02;
  val[3][0][1][ 8][20] =   2.036396e+00; eval[3][0][1][ 8][20] =   6.731152e-02;
  val[3][0][1][ 8][21] =   1.819913e+00; eval[3][0][1][ 8][21] =   8.925823e-02;
  val[3][0][1][ 8][22] =   2.142499e+00; eval[3][0][1][ 8][22] =   1.399606e-01;
  val[3][0][1][ 8][23] =   1.961024e+00; eval[3][0][1][ 8][23] =   1.557212e-01;
  val[3][0][1][ 8][24] =   6.537340e+00; eval[3][0][1][ 8][24] =   6.255282e+03;
  val[3][0][1][ 8][25] =   8.507160e+03; eval[3][0][1][ 8][25] =   5.279922e+03;
  val[3][0][1][ 8][26] =   2.262649e+00; eval[3][0][1][ 8][26] =   3.331497e-01;
  val[3][0][1][ 8][27] =   2.343194e+00; eval[3][0][1][ 8][27] =   5.373477e-01;
  //emcdz_sigma_c0d1z9
  n_val[3][0][1][9] = 28;
  val[3][0][1][ 9][ 0] =   3.007571e+00; eval[3][0][1][ 9][ 0] =   1.932539e-02;
  val[3][0][1][ 9][ 1] =   2.747180e+00; eval[3][0][1][ 9][ 1] =   1.950472e-02;
  val[3][0][1][ 9][ 2] =   2.591022e+00; eval[3][0][1][ 9][ 2] =   2.314626e-02;
  val[3][0][1][ 9][ 3] =   2.524761e+00; eval[3][0][1][ 9][ 3] =   1.465445e-03;
  val[3][0][1][ 9][ 4] =   2.487512e+00; eval[3][0][1][ 9][ 4] =   1.854437e-03;
  val[3][0][1][ 9][ 5] =   2.499741e+00; eval[3][0][1][ 9][ 5] =   2.453134e-03;
  val[3][0][1][ 9][ 6] =   2.484901e+00; eval[3][0][1][ 9][ 6] =   3.117359e-03;
  val[3][0][1][ 9][ 7] =   2.497855e+00; eval[3][0][1][ 9][ 7] =   4.239440e-03;
  val[3][0][1][ 9][ 8] =   2.526247e+00; eval[3][0][1][ 9][ 8] =   5.864919e-03;
  val[3][0][1][ 9][ 9] =   2.538701e+00; eval[3][0][1][ 9][ 9] =   7.904766e-03;
  val[3][0][1][ 9][10] =   2.534679e+00; eval[3][0][1][ 9][10] =   1.039847e-02;
  val[3][0][1][ 9][11] =   2.442349e+00; eval[3][0][1][ 9][11] =   1.407296e-02;
  val[3][0][1][ 9][12] =   2.440480e+00; eval[3][0][1][ 9][12] =   1.890669e-02;
  val[3][0][1][ 9][13] =   2.444183e+00; eval[3][0][1][ 9][13] =   2.351429e-02;
  val[3][0][1][ 9][14] =   2.329988e+00; eval[3][0][1][ 9][14] =   2.783867e-02;
  val[3][0][1][ 9][15] =   2.381926e+00; eval[3][0][1][ 9][15] =   3.379069e-02;
  val[3][0][1][ 9][16] =   2.464361e+00; eval[3][0][1][ 9][16] =   4.683415e-02;
  val[3][0][1][ 9][17] =   2.493301e+00; eval[3][0][1][ 9][17] =   5.819709e-02;
  val[3][0][1][ 9][18] =   2.415158e+00; eval[3][0][1][ 9][18] =   5.114930e-02;
  val[3][0][1][ 9][19] =   2.477105e+00; eval[3][0][1][ 9][19] =   7.651767e-02;
  val[3][0][1][ 9][20] =   2.557867e+00; eval[3][0][1][ 9][20] =   1.094844e-01;
  val[3][0][1][ 9][21] =   2.376721e+00; eval[3][0][1][ 9][21] =   1.206622e-01;
  val[3][0][1][ 9][22] =   2.410031e+00; eval[3][0][1][ 9][22] =   1.632384e-01;
  val[3][0][1][ 9][23] =   2.227874e+00; eval[3][0][1][ 9][23] =   1.655635e-01;
  val[3][0][1][ 9][24] =   1.738604e+00; eval[3][0][1][ 9][24] =   1.731521e-01;
  val[3][0][1][ 9][25] =   2.101920e+00; eval[3][0][1][ 9][25] =   4.722565e-01;
  val[3][0][1][ 9][26] =   2.720164e+00; eval[3][0][1][ 9][26] =   1.085537e+00;
  val[3][0][1][ 9][27] =   3.658813e+00; eval[3][0][1][ 9][27] =   1.968175e+00;
  //emcdz_sigma_c1d0z0
  n_val[3][1][0][0] = 28;
  val[3][1][0][ 0][ 0] =   3.335740e+00; eval[3][1][0][ 0][ 0] =   1.893195e-02;
  val[3][1][0][ 0][ 1] =   3.067753e+00; eval[3][1][0][ 0][ 1] =   2.071932e-02;
  val[3][1][0][ 0][ 2] =   2.980617e+00; eval[3][1][0][ 0][ 2] =   2.605403e-02;
  val[3][1][0][ 0][ 3] =   2.848760e+00; eval[3][1][0][ 0][ 3] =   1.797584e-03;
  val[3][1][0][ 0][ 4] =   2.821056e+00; eval[3][1][0][ 0][ 4] =   2.254655e-03;
  val[3][1][0][ 0][ 5] =   2.800544e+00; eval[3][1][0][ 0][ 5] =   2.919555e-03;
  val[3][1][0][ 0][ 6] =   2.735925e+00; eval[3][1][0][ 0][ 6] =   3.495381e-03;
  val[3][1][0][ 0][ 7] =   2.721480e+00; eval[3][1][0][ 0][ 7] =   4.573567e-03;
  val[3][1][0][ 0][ 8] =   2.614603e+00; eval[3][1][0][ 0][ 8] =   6.469184e-03;
  val[3][1][0][ 0][ 9] =   2.593553e+00; eval[3][1][0][ 0][ 9] =   8.363708e-03;
  val[3][1][0][ 0][10] =   2.576805e+00; eval[3][1][0][ 0][10] =   1.084348e-02;
  val[3][1][0][ 0][11] =   2.562819e+00; eval[3][1][0][ 0][11] =   1.409098e-02;
  val[3][1][0][ 0][12] =   2.530462e+00; eval[3][1][0][ 0][12] =   1.792250e-02;
  val[3][1][0][ 0][13] =   2.514928e+00; eval[3][1][0][ 0][13] =   2.229419e-02;
  val[3][1][0][ 0][14] =   2.499508e+00; eval[3][1][0][ 0][14] =   2.751767e-02;
  val[3][1][0][ 0][15] =   2.511421e+00; eval[3][1][0][ 0][15] =   3.527403e-02;
  val[3][1][0][ 0][16] =   2.477912e+00; eval[3][1][0][ 0][16] =   4.111048e-02;
  val[3][1][0][ 0][17] =   2.510718e+00; eval[3][1][0][ 0][17] =   5.750705e-02;
  val[3][1][0][ 0][18] =   2.465670e+00; eval[3][1][0][ 0][18] =   4.834502e-02;
  val[3][1][0][ 0][19] =   2.421071e+00; eval[3][1][0][ 0][19] =   6.883927e-02;
  val[3][1][0][ 0][20] =   2.470668e+00; eval[3][1][0][ 0][20] =   9.291138e-02;
  val[3][1][0][ 0][21] =   2.528269e+00; eval[3][1][0][ 0][21] =   1.345072e-01;
  val[3][1][0][ 0][22] =   2.275809e+00; eval[3][1][0][ 0][22] =   1.138000e-01;
  val[3][1][0][ 0][23] =   2.566085e+00; eval[3][1][0][ 0][23] =   2.210484e-01;
  val[3][1][0][ 0][24] =   2.975119e+00; eval[3][1][0][ 0][24] =   3.005641e-01;
  val[3][1][0][ 0][25] =   3.287066e+00; eval[3][1][0][ 0][25] =   5.498791e-01;
  val[3][1][0][ 0][26] =   3.168088e+00; eval[3][1][0][ 0][26] =   5.683989e-01;
  val[3][1][0][ 0][27] =   2.553904e+00; eval[3][1][0][ 0][27] =   4.594684e-01;
  //emcdz_sigma_c1d0z1
  n_val[3][1][0][1] = 28;
  val[3][1][0][ 1][ 0] =   3.224851e+00; eval[3][1][0][ 1][ 0] =   1.704321e-02;
  val[3][1][0][ 1][ 1] =   2.897836e+00; eval[3][1][0][ 1][ 1] =   1.754828e-02;
  val[3][1][0][ 1][ 2] =   2.742493e+00; eval[3][1][0][ 1][ 2] =   2.423384e-02;
  val[3][1][0][ 1][ 3] =   2.648715e+00; eval[3][1][0][ 1][ 3] =   1.460569e-03;
  val[3][1][0][ 1][ 4] =   2.598911e+00; eval[3][1][0][ 1][ 4] =   1.813790e-03;
  val[3][1][0][ 1][ 5] =   2.499700e+00; eval[3][1][0][ 1][ 5] =   2.517148e-03;
  val[3][1][0][ 1][ 6] =   2.480626e+00; eval[3][1][0][ 1][ 6] =   3.082061e-03;
  val[3][1][0][ 1][ 7] =   2.426643e+00; eval[3][1][0][ 1][ 7] =   3.961758e-03;
  val[3][1][0][ 1][ 8] =   2.413167e+00; eval[3][1][0][ 1][ 8] =   5.237578e-03;
  val[3][1][0][ 1][ 9] =   2.388442e+00; eval[3][1][0][ 1][ 9] =   6.785986e-03;
  val[3][1][0][ 1][10] =   2.364299e+00; eval[3][1][0][ 1][10] =   8.758759e-03;
  val[3][1][0][ 1][11] =   2.352225e+00; eval[3][1][0][ 1][11] =   1.135991e-02;
  val[3][1][0][ 1][12] =   2.236785e+00; eval[3][1][0][ 1][12] =   1.505896e-02;
  val[3][1][0][ 1][13] =   2.273142e+00; eval[3][1][0][ 1][13] =   2.009201e-02;
  val[3][1][0][ 1][14] =   2.208669e+00; eval[3][1][0][ 1][14] =   2.344004e-02;
  val[3][1][0][ 1][15] =   2.258773e+00; eval[3][1][0][ 1][15] =   3.214545e-02;
  val[3][1][0][ 1][16] =   2.123166e+00; eval[3][1][0][ 1][16] =   3.521560e-02;
  val[3][1][0][ 1][17] =   2.147581e+00; eval[3][1][0][ 1][17] =   4.207701e-02;
  val[3][1][0][ 1][18] =   2.155769e+00; eval[3][1][0][ 1][18] =   3.882601e-02;
  val[3][1][0][ 1][19] =   2.042260e+00; eval[3][1][0][ 1][19] =   5.172320e-02;
  val[3][1][0][ 1][20] =   2.175333e+00; eval[3][1][0][ 1][20] =   7.682735e-02;
  val[3][1][0][ 1][21] =   2.097823e+00; eval[3][1][0][ 1][21] =   9.427043e-02;
  val[3][1][0][ 1][22] =   1.871591e+00; eval[3][1][0][ 1][22] =   9.391398e-02;
  val[3][1][0][ 1][23] =   1.859852e+00; eval[3][1][0][ 1][23] =   1.702736e-01;
  val[3][1][0][ 1][24] =   2.395465e+00; eval[3][1][0][ 1][24] =   2.418257e-01;
  val[3][1][0][ 1][25] =   1.980742e+00; eval[3][1][0][ 1][25] =   2.307521e-01;
  val[3][1][0][ 1][26] =   2.265582e+00; eval[3][1][0][ 1][26] =   5.613427e-01;
  val[3][1][0][ 1][27] =   3.440162e+00; eval[3][1][0][ 1][27] =   1.855949e+00;
  //emcdz_sigma_c1d0z2
  n_val[3][1][0][2] = 28;
  val[3][1][0][ 2][ 0] =   3.109744e+00; eval[3][1][0][ 2][ 0] =   1.522956e-02;
  val[3][1][0][ 2][ 1] =   2.707402e+00; eval[3][1][0][ 2][ 1] =   1.698958e-02;
  val[3][1][0][ 2][ 2] =   2.585453e+00; eval[3][1][0][ 2][ 2] =   2.064539e-02;
  val[3][1][0][ 2][ 3] =   2.410821e+00; eval[3][1][0][ 2][ 3] =   1.297914e-03;
  val[3][1][0][ 2][ 4] =   2.355547e+00; eval[3][1][0][ 2][ 4] =   1.607974e-03;
  val[3][1][0][ 2][ 5] =   2.331339e+00; eval[3][1][0][ 2][ 5] =   2.126951e-03;
  val[3][1][0][ 2][ 6] =   2.285738e+00; eval[3][1][0][ 2][ 6] =   2.537778e-03;
  val[3][1][0][ 2][ 7] =   2.253088e+00; eval[3][1][0][ 2][ 7] =   3.305862e-03;
  val[3][1][0][ 2][ 8] =   2.158882e+00; eval[3][1][0][ 2][ 8] =   4.594298e-03;
  val[3][1][0][ 2][ 9] =   2.129410e+00; eval[3][1][0][ 2][ 9] =   5.957276e-03;
  val[3][1][0][ 2][10] =   2.094471e+00; eval[3][1][0][ 2][10] =   7.542302e-03;
  val[3][1][0][ 2][11] =   2.072274e+00; eval[3][1][0][ 2][11] =   9.699256e-03;
  val[3][1][0][ 2][12] =   2.066927e+00; eval[3][1][0][ 2][12] =   1.245413e-02;
  val[3][1][0][ 2][13] =   2.067426e+00; eval[3][1][0][ 2][13] =   1.597453e-02;
  val[3][1][0][ 2][14] =   2.006952e+00; eval[3][1][0][ 2][14] =   1.917099e-02;
  val[3][1][0][ 2][15] =   2.005636e+00; eval[3][1][0][ 2][15] =   2.416081e-02;
  val[3][1][0][ 2][16] =   1.994690e+00; eval[3][1][0][ 2][16] =   2.962866e-02;
  val[3][1][0][ 2][17] =   1.984962e+00; eval[3][1][0][ 2][17] =   3.613562e-02;
  val[3][1][0][ 2][18] =   1.984114e+00; eval[3][1][0][ 2][18] =   3.330524e-02;
  val[3][1][0][ 2][19] =   1.958836e+00; eval[3][1][0][ 2][19] =   4.689302e-02;
  val[3][1][0][ 2][20] =   2.044420e+00; eval[3][1][0][ 2][20] =   6.633161e-02;
  val[3][1][0][ 2][21] =   1.677334e+00; eval[3][1][0][ 2][21] =   6.606223e-02;
  val[3][1][0][ 2][22] =   1.594340e+00; eval[3][1][0][ 2][22] =   1.032835e-01;
  val[3][1][0][ 2][23] =   1.665327e+00; eval[3][1][0][ 2][23] =   1.066314e-01;
  val[3][1][0][ 2][24] =   2.092468e+00; eval[3][1][0][ 2][24] =   2.192346e-01;
  val[3][1][0][ 2][25] =   1.871015e+00; eval[3][1][0][ 2][25] =   3.523310e-01;
  val[3][1][0][ 2][26] =   5.038407e+03; eval[3][1][0][ 2][26] =   7.453133e+03;
  val[3][1][0][ 2][27] =   2.380297e+00; eval[3][1][0][ 2][27] =   8.018444e-01;
  //emcdz_sigma_c1d0z3
  n_val[3][1][0][3] = 28;
  val[3][1][0][ 3][ 0] =   3.046152e+00; eval[3][1][0][ 3][ 0] =   1.473785e-02;
  val[3][1][0][ 3][ 1] =   2.608454e+00; eval[3][1][0][ 3][ 1] =   1.604314e-02;
  val[3][1][0][ 3][ 2] =   2.464087e+00; eval[3][1][0][ 3][ 2] =   1.970962e-02;
  val[3][1][0][ 3][ 3] =   2.295417e+00; eval[3][1][0][ 3][ 3] =   1.219346e-03;
  val[3][1][0][ 3][ 4] =   2.235796e+00; eval[3][1][0][ 3][ 4] =   1.518205e-03;
  val[3][1][0][ 3][ 5] =   2.114138e+00; eval[3][1][0][ 3][ 5] =   2.082549e-03;
  val[3][1][0][ 3][ 6] =   2.073235e+00; eval[3][1][0][ 3][ 6] =   2.486746e-03;
  val[3][1][0][ 3][ 7] =   2.042176e+00; eval[3][1][0][ 3][ 7] =   3.229141e-03;
  val[3][1][0][ 3][ 8] =   2.010248e+00; eval[3][1][0][ 3][ 8] =   4.144334e-03;
  val[3][1][0][ 3][ 9] =   1.974141e+00; eval[3][1][0][ 3][ 9] =   5.269593e-03;
  val[3][1][0][ 3][10] =   1.949601e+00; eval[3][1][0][ 3][10] =   6.751327e-03;
  val[3][1][0][ 3][11] =   1.872521e+00; eval[3][1][0][ 3][11] =   9.373558e-03;
  val[3][1][0][ 3][12] =   1.846444e+00; eval[3][1][0][ 3][12] =   1.169701e-02;
  val[3][1][0][ 3][13] =   1.838683e+00; eval[3][1][0][ 3][13] =   1.495093e-02;
  val[3][1][0][ 3][14] =   1.798926e+00; eval[3][1][0][ 3][14] =   1.768652e-02;
  val[3][1][0][ 3][15] =   1.790764e+00; eval[3][1][0][ 3][15] =   2.236004e-02;
  val[3][1][0][ 3][16] =   1.775076e+00; eval[3][1][0][ 3][16] =   2.779569e-02;
  val[3][1][0][ 3][17] =   1.811924e+00; eval[3][1][0][ 3][17] =   3.444813e-02;
  val[3][1][0][ 3][18] =   1.715784e+00; eval[3][1][0][ 3][18] =   2.787144e-02;
  val[3][1][0][ 3][19] =   1.699460e+00; eval[3][1][0][ 3][19] =   3.868850e-02;
  val[3][1][0][ 3][20] =   1.762161e+00; eval[3][1][0][ 3][20] =   5.738250e-02;
  val[3][1][0][ 3][21] =   1.936096e+00; eval[3][1][0][ 3][21] =   1.020211e-01;
  val[3][1][0][ 3][22] =   1.697122e+00; eval[3][1][0][ 3][22] =   8.679064e-02;
  val[3][1][0][ 3][23] =   1.460900e+00; eval[3][1][0][ 3][23] =   8.291048e-02;
  val[3][1][0][ 3][24] =   1.730352e+00; eval[3][1][0][ 3][24] =   1.264374e-01;
  val[3][1][0][ 3][25] =   1.686158e+00; eval[3][1][0][ 3][25] =   1.667002e-01;
  val[3][1][0][ 3][26] =   1.541875e+00; eval[3][1][0][ 3][26] =   2.952225e-01;
  val[3][1][0][ 3][27] =   2.382154e+00; eval[3][1][0][ 3][27] =   9.886607e-01;
  //emcdz_sigma_c1d0z4
  n_val[3][1][0][4] = 28;
  val[3][1][0][ 4][ 0] =   2.917531e+00; eval[3][1][0][ 4][ 0] =   1.657718e-02;
  val[3][1][0][ 4][ 1] =   2.572850e+00; eval[3][1][0][ 4][ 1] =   1.731714e-02;
  val[3][1][0][ 4][ 2] =   2.393013e+00; eval[3][1][0][ 4][ 2] =   2.344774e-02;
  val[3][1][0][ 4][ 3] =   2.296682e+00; eval[3][1][0][ 4][ 3] =   1.365218e-03;
  val[3][1][0][ 4][ 4] =   2.241495e+00; eval[3][1][0][ 4][ 4] =   1.687760e-03;
  val[3][1][0][ 4][ 5] =   2.142141e+00; eval[3][1][0][ 4][ 5] =   2.353022e-03;
  val[3][1][0][ 4][ 6] =   2.103182e+00; eval[3][1][0][ 4][ 6] =   2.791531e-03;
  val[3][1][0][ 4][ 7] =   2.073183e+00; eval[3][1][0][ 4][ 7] =   3.614097e-03;
  val[3][1][0][ 4][ 8] =   2.034138e+00; eval[3][1][0][ 4][ 8] =   4.613103e-03;
  val[3][1][0][ 4][ 9] =   2.024487e+00; eval[3][1][0][ 4][ 9] =   6.052314e-03;
  val[3][1][0][ 4][10] =   1.996577e+00; eval[3][1][0][ 4][10] =   7.767253e-03;
  val[3][1][0][ 4][11] =   1.971757e+00; eval[3][1][0][ 4][11] =   9.831762e-03;
  val[3][1][0][ 4][12] =   1.906696e+00; eval[3][1][0][ 4][12] =   1.357983e-02;
  val[3][1][0][ 4][13] =   1.859714e+00; eval[3][1][0][ 4][13] =   1.682371e-02;
  val[3][1][0][ 4][14] =   1.891907e+00; eval[3][1][0][ 4][14] =   2.168675e-02;
  val[3][1][0][ 4][15] =   1.915782e+00; eval[3][1][0][ 4][15] =   3.042388e-02;
  val[3][1][0][ 4][16] =   1.824119e+00; eval[3][1][0][ 4][16] =   3.156578e-02;
  val[3][1][0][ 4][17] =   1.819661e+00; eval[3][1][0][ 4][17] =   3.838506e-02;
  val[3][1][0][ 4][18] =   1.842277e+00; eval[3][1][0][ 4][18] =   3.978452e-02;
  val[3][1][0][ 4][19] =   1.723768e+00; eval[3][1][0][ 4][19] =   5.308496e-02;
  val[3][1][0][ 4][20] =   1.736615e+00; eval[3][1][0][ 4][20] =   6.809796e-02;
  val[3][1][0][ 4][21] =   1.795291e+00; eval[3][1][0][ 4][21] =   7.566922e-02;
  val[3][1][0][ 4][22] =   1.807863e+00; eval[3][1][0][ 4][22] =   1.339952e-01;
  val[3][1][0][ 4][23] =   1.666381e+00; eval[3][1][0][ 4][23] =   1.185966e-01;
  val[3][1][0][ 4][24] =   2.246967e+00; eval[3][1][0][ 4][24] =   2.919275e-01;
  val[3][1][0][ 4][25] =   1.769235e+00; eval[3][1][0][ 4][25] =   2.480460e-01;
  val[3][1][0][ 4][26] =   2.466355e+00; eval[3][1][0][ 4][26] =   1.377313e+00;
  val[3][1][0][ 4][27] =   2.233997e+00; eval[3][1][0][ 4][27] =   5.519363e-01;
  //emcdz_sigma_c1d0z5
  n_val[3][1][0][5] = 28;
  val[3][1][0][ 5][ 0] =   2.991608e+00; eval[3][1][0][ 5][ 0] =   1.666799e-02;
  val[3][1][0][ 5][ 1] =   2.586740e+00; eval[3][1][0][ 5][ 1] =   1.906449e-02;
  val[3][1][0][ 5][ 2] =   2.466997e+00; eval[3][1][0][ 5][ 2] =   2.433059e-02;
  val[3][1][0][ 5][ 3] =   2.371342e+00; eval[3][1][0][ 5][ 3] =   1.496475e-03;
  val[3][1][0][ 5][ 4] =   2.254248e+00; eval[3][1][0][ 5][ 4] =   1.987777e-03;
  val[3][1][0][ 5][ 5] =   2.195048e+00; eval[3][1][0][ 5][ 5] =   2.486802e-03;
  val[3][1][0][ 5][ 6] =   2.154268e+00; eval[3][1][0][ 5][ 6] =   2.927450e-03;
  val[3][1][0][ 5][ 7] =   2.051183e+00; eval[3][1][0][ 5][ 7] =   3.967128e-03;
  val[3][1][0][ 5][ 8] =   2.030384e+00; eval[3][1][0][ 5][ 8] =   5.128277e-03;
  val[3][1][0][ 5][ 9] =   2.013713e+00; eval[3][1][0][ 5][ 9] =   6.664103e-03;
  val[3][1][0][ 5][10] =   1.987032e+00; eval[3][1][0][ 5][10] =   8.584420e-03;
  val[3][1][0][ 5][11] =   1.970836e+00; eval[3][1][0][ 5][11] =   1.096820e-02;
  val[3][1][0][ 5][12] =   1.932014e+00; eval[3][1][0][ 5][12] =   1.378533e-02;
  val[3][1][0][ 5][13] =   1.919354e+00; eval[3][1][0][ 5][13] =   1.740564e-02;
  val[3][1][0][ 5][14] =   1.889101e+00; eval[3][1][0][ 5][14] =   2.145093e-02;
  val[3][1][0][ 5][15] =   1.888762e+00; eval[3][1][0][ 5][15] =   2.761563e-02;
  val[3][1][0][ 5][16] =   1.870142e+00; eval[3][1][0][ 5][16] =   3.201527e-02;
  val[3][1][0][ 5][17] =   1.856658e+00; eval[3][1][0][ 5][17] =   4.099074e-02;
  val[3][1][0][ 5][18] =   1.862053e+00; eval[3][1][0][ 5][18] =   3.653685e-02;
  val[3][1][0][ 5][19] =   1.791107e+00; eval[3][1][0][ 5][19] =   5.240886e-02;
  val[3][1][0][ 5][20] =   1.832865e+00; eval[3][1][0][ 5][20] =   6.700315e-02;
  val[3][1][0][ 5][21] =   1.786914e+00; eval[3][1][0][ 5][21] =   9.657934e-02;
  val[3][1][0][ 5][22] =   2.198865e+00; eval[3][1][0][ 5][22] =   1.995974e-01;
  val[3][1][0][ 5][23] =   1.729795e+00; eval[3][1][0][ 5][23] =   1.996315e-01;
  val[3][1][0][ 5][24] =   2.089801e+00; eval[3][1][0][ 5][24] =   2.196400e-01;
  val[3][1][0][ 5][25] =   2.003439e+00; eval[3][1][0][ 5][25] =   4.275507e-01;
  val[3][1][0][ 5][26] =   3.771214e+00; eval[3][1][0][ 5][26] =   1.401883e+00;
  val[3][1][0][ 5][27] =   1.267941e+00; eval[3][1][0][ 5][27] =   3.392991e-01;
  //emcdz_sigma_c1d0z6
  n_val[3][1][0][6] = 28;
  val[3][1][0][ 6][ 0] =   3.075648e+00; eval[3][1][0][ 6][ 0] =   1.484641e-02;
  val[3][1][0][ 6][ 1] =   2.699365e+00; eval[3][1][0][ 6][ 1] =   1.541823e-02;
  val[3][1][0][ 6][ 2] =   2.496050e+00; eval[3][1][0][ 6][ 2] =   2.125823e-02;
  val[3][1][0][ 6][ 3] =   2.389566e+00; eval[3][1][0][ 6][ 3] =   1.309188e-03;
  val[3][1][0][ 6][ 4] =   2.323190e+00; eval[3][1][0][ 6][ 4] =   1.606849e-03;
  val[3][1][0][ 6][ 5] =   2.204888e+00; eval[3][1][0][ 6][ 5] =   2.121290e-03;
  val[3][1][0][ 6][ 6] =   2.159862e+00; eval[3][1][0][ 6][ 6] =   2.481061e-03;
  val[3][1][0][ 6][ 7] =   2.123413e+00; eval[3][1][0][ 6][ 7] =   3.184853e-03;
  val[3][1][0][ 6][ 8] =   2.094811e+00; eval[3][1][0][ 6][ 8] =   4.129388e-03;
  val[3][1][0][ 6][ 9] =   2.073962e+00; eval[3][1][0][ 6][ 9] =   5.335174e-03;
  val[3][1][0][ 6][10] =   2.044908e+00; eval[3][1][0][ 6][10] =   6.897236e-03;
  val[3][1][0][ 6][11] =   1.979816e+00; eval[3][1][0][ 6][11] =   9.310609e-03;
  val[3][1][0][ 6][12] =   1.971159e+00; eval[3][1][0][ 6][12] =   1.212892e-02;
  val[3][1][0][ 6][13] =   1.918766e+00; eval[3][1][0][ 6][13] =   1.473696e-02;
  val[3][1][0][ 6][14] =   1.896528e+00; eval[3][1][0][ 6][14] =   1.845979e-02;
  val[3][1][0][ 6][15] =   1.929157e+00; eval[3][1][0][ 6][15] =   2.321888e-02;
  val[3][1][0][ 6][16] =   1.856920e+00; eval[3][1][0][ 6][16] =   2.790848e-02;
  val[3][1][0][ 6][17] =   1.922215e+00; eval[3][1][0][ 6][17] =   3.581891e-02;
  val[3][1][0][ 6][18] =   1.851359e+00; eval[3][1][0][ 6][18] =   3.192854e-02;
  val[3][1][0][ 6][19] =   1.799492e+00; eval[3][1][0][ 6][19] =   4.227367e-02;
  val[3][1][0][ 6][20] =   1.926554e+00; eval[3][1][0][ 6][20] =   6.562039e-02;
  val[3][1][0][ 6][21] =   1.829980e+00; eval[3][1][0][ 6][21] =   5.444907e-02;
  val[3][1][0][ 6][22] =   1.867258e+00; eval[3][1][0][ 6][22] =   1.154512e-01;
  val[3][1][0][ 6][23] =   1.646830e+00; eval[3][1][0][ 6][23] =   1.146317e-01;
  val[3][1][0][ 6][24] =   1.991535e+00; eval[3][1][0][ 6][24] =   1.653577e-01;
  val[3][1][0][ 6][25] =   2.753669e+00; eval[3][1][0][ 6][25] =   4.968927e-01;
  val[3][1][0][ 6][26] =   1.875675e+00; eval[3][1][0][ 6][26] =   4.723584e-01;
  val[3][1][0][ 6][27] =   2.459398e+00; eval[3][1][0][ 6][27] =   8.424663e-01;
  //emcdz_sigma_c1d0z7
  n_val[3][1][0][7] = 28;
  val[3][1][0][ 7][ 0] =   3.135893e+00; eval[3][1][0][ 7][ 0] =   1.483777e-02;
  val[3][1][0][ 7][ 1] =   2.762129e+00; eval[3][1][0][ 7][ 1] =   1.580749e-02;
  val[3][1][0][ 7][ 2] =   2.662677e+00; eval[3][1][0][ 7][ 2] =   2.079897e-02;
  val[3][1][0][ 7][ 3] =   2.504299e+00; eval[3][1][0][ 7][ 3] =   1.415330e-03;
  val[3][1][0][ 7][ 4] =   2.445279e+00; eval[3][1][0][ 7][ 4] =   1.772018e-03;
  val[3][1][0][ 7][ 5] =   2.367069e+00; eval[3][1][0][ 7][ 5] =   2.173345e-03;
  val[3][1][0][ 7][ 6] =   2.330772e+00; eval[3][1][0][ 7][ 6] =   2.551704e-03;
  val[3][1][0][ 7][ 7] =   2.247107e+00; eval[3][1][0][ 7][ 7] =   3.482523e-03;
  val[3][1][0][ 7][ 8] =   2.222827e+00; eval[3][1][0][ 7][ 8] =   4.551746e-03;
  val[3][1][0][ 7][ 9] =   2.207491e+00; eval[3][1][0][ 7][ 9] =   5.863253e-03;
  val[3][1][0][ 7][10] =   2.176614e+00; eval[3][1][0][ 7][10] =   7.539797e-03;
  val[3][1][0][ 7][11] =   2.159094e+00; eval[3][1][0][ 7][11] =   9.832287e-03;
  val[3][1][0][ 7][12] =   2.152884e+00; eval[3][1][0][ 7][12] =   1.260176e-02;
  val[3][1][0][ 7][13] =   2.127364e+00; eval[3][1][0][ 7][13] =   1.573098e-02;
  val[3][1][0][ 7][14] =   2.116178e+00; eval[3][1][0][ 7][14] =   1.976220e-02;
  val[3][1][0][ 7][15] =   2.130161e+00; eval[3][1][0][ 7][15] =   2.434211e-02;
  val[3][1][0][ 7][16] =   2.015589e+00; eval[3][1][0][ 7][16] =   2.710790e-02;
  val[3][1][0][ 7][17] =   2.074881e+00; eval[3][1][0][ 7][17] =   3.765090e-02;
  val[3][1][0][ 7][18] =   2.023935e+00; eval[3][1][0][ 7][18] =   3.260796e-02;
  val[3][1][0][ 7][19] =   1.979959e+00; eval[3][1][0][ 7][19] =   4.284095e-02;
  val[3][1][0][ 7][20] =   1.982597e+00; eval[3][1][0][ 7][20] =   5.786067e-02;
  val[3][1][0][ 7][21] =   1.965105e+00; eval[3][1][0][ 7][21] =   8.018932e-02;
  val[3][1][0][ 7][22] =   1.764945e+00; eval[3][1][0][ 7][22] =   8.242807e-02;
  val[3][1][0][ 7][23] =   1.909607e+00; eval[3][1][0][ 7][23] =   1.333567e-01;
  val[3][1][0][ 7][24] =   1.399855e+00; eval[3][1][0][ 7][24] =   1.593789e-01;
  val[3][1][0][ 7][25] =   1.885102e+00; eval[3][1][0][ 7][25] =   1.919272e-01;
  val[3][1][0][ 7][26] =   2.717198e+00; eval[3][1][0][ 7][26] =   4.863388e-01;
  val[3][1][0][ 7][27] =   2.400299e+00; eval[3][1][0][ 7][27] =   6.261786e-01;
  //emcdz_sigma_c1d0z8
  n_val[3][1][0][8] = 28;
  val[3][1][0][ 8][ 0] =   3.196123e+00; eval[3][1][0][ 8][ 0] =   1.554814e-02;
  val[3][1][0][ 8][ 1] =   2.876033e+00; eval[3][1][0][ 8][ 1] =   1.723751e-02;
  val[3][1][0][ 8][ 2] =   2.773303e+00; eval[3][1][0][ 8][ 2] =   2.278598e-02;
  val[3][1][0][ 8][ 3] =   2.691782e+00; eval[3][1][0][ 8][ 3] =   1.510365e-03;
  val[3][1][0][ 8][ 4] =   2.648614e+00; eval[3][1][0][ 8][ 4] =   1.952533e-03;
  val[3][1][0][ 8][ 5] =   2.610599e+00; eval[3][1][0][ 8][ 5] =   2.471015e-03;
  val[3][1][0][ 8][ 6] =   2.551155e+00; eval[3][1][0][ 8][ 6] =   2.871949e-03;
  val[3][1][0][ 8][ 7] =   2.524197e+00; eval[3][1][0][ 8][ 7] =   3.714705e-03;
  val[3][1][0][ 8][ 8] =   2.507806e+00; eval[3][1][0][ 8][ 8] =   4.910175e-03;
  val[3][1][0][ 8][ 9] =   2.434076e+00; eval[3][1][0][ 8][ 9] =   6.646838e-03;
  val[3][1][0][ 8][10] =   2.421383e+00; eval[3][1][0][ 8][10] =   8.682860e-03;
  val[3][1][0][ 8][11] =   2.378380e+00; eval[3][1][0][ 8][11] =   1.085006e-02;
  val[3][1][0][ 8][12] =   2.381445e+00; eval[3][1][0][ 8][12] =   1.442137e-02;
  val[3][1][0][ 8][13] =   2.374646e+00; eval[3][1][0][ 8][13] =   1.784652e-02;
  val[3][1][0][ 8][14] =   2.321165e+00; eval[3][1][0][ 8][14] =   2.216661e-02;
  val[3][1][0][ 8][15] =   2.331268e+00; eval[3][1][0][ 8][15] =   2.734585e-02;
  val[3][1][0][ 8][16] =   2.295325e+00; eval[3][1][0][ 8][16] =   3.392581e-02;
  val[3][1][0][ 8][17] =   2.217794e+00; eval[3][1][0][ 8][17] =   3.886366e-02;
  val[3][1][0][ 8][18] =   2.293491e+00; eval[3][1][0][ 8][18] =   3.722036e-02;
  val[3][1][0][ 8][19] =   2.165143e+00; eval[3][1][0][ 8][19] =   5.003836e-02;
  val[3][1][0][ 8][20] =   2.244210e+00; eval[3][1][0][ 8][20] =   6.838521e-02;
  val[3][1][0][ 8][21] =   2.236622e+00; eval[3][1][0][ 8][21] =   1.072064e-01;
  val[3][1][0][ 8][22] =   2.214506e+00; eval[3][1][0][ 8][22] =   1.131865e-01;
  val[3][1][0][ 8][23] =   2.127631e+00; eval[3][1][0][ 8][23] =   1.774951e-01;
  val[3][1][0][ 8][24] =   2.232516e+00; eval[3][1][0][ 8][24] =   1.826527e-01;
  val[3][1][0][ 8][25] =   2.678551e+00; eval[3][1][0][ 8][25] =   3.448362e-01;
  val[3][1][0][ 8][26] =   1.426770e+00; eval[3][1][0][ 8][26] =   2.406226e-01;
  val[3][1][0][ 8][27] =   3.410169e+00; eval[3][1][0][ 8][27] =   9.176333e-01;
  //emcdz_sigma_c1d0z9
  n_val[3][1][0][9] = 28;
  val[3][1][0][ 9][ 0] =   3.406881e+00; eval[3][1][0][ 9][ 0] =   1.708530e-02;
  val[3][1][0][ 9][ 1] =   3.096441e+00; eval[3][1][0][ 9][ 1] =   1.927602e-02;
  val[3][1][0][ 9][ 2] =   3.077489e+00; eval[3][1][0][ 9][ 2] =   2.634037e-02;
  val[3][1][0][ 9][ 3] =   2.975415e+00; eval[3][1][0][ 9][ 3] =   1.751329e-03;
  val[3][1][0][ 9][ 4] =   2.957020e+00; eval[3][1][0][ 9][ 4] =   2.312980e-03;
  val[3][1][0][ 9][ 5] =   2.896832e+00; eval[3][1][0][ 9][ 5] =   2.950281e-03;
  val[3][1][0][ 9][ 6] =   2.890677e+00; eval[3][1][0][ 9][ 6] =   3.513437e-03;
  val[3][1][0][ 9][ 7] =   2.883485e+00; eval[3][1][0][ 9][ 7] =   4.631465e-03;
  val[3][1][0][ 9][ 8] =   2.875410e+00; eval[3][1][0][ 9][ 8] =   6.164923e-03;
  val[3][1][0][ 9][ 9] =   2.861875e+00; eval[3][1][0][ 9][ 9] =   8.072527e-03;
  val[3][1][0][ 9][10] =   2.857025e+00; eval[3][1][0][ 9][10] =   1.062911e-02;
  val[3][1][0][ 9][11] =   2.833313e+00; eval[3][1][0][ 9][11] =   1.369441e-02;
  val[3][1][0][ 9][12] =   2.787871e+00; eval[3][1][0][ 9][12] =   1.720001e-02;
  val[3][1][0][ 9][13] =   2.768508e+00; eval[3][1][0][ 9][13] =   2.157537e-02;
  val[3][1][0][ 9][14] =   2.714011e+00; eval[3][1][0][ 9][14] =   2.651375e-02;
  val[3][1][0][ 9][15] =   2.650518e+00; eval[3][1][0][ 9][15] =   3.248857e-02;
  val[3][1][0][ 9][16] =   2.792660e+00; eval[3][1][0][ 9][16] =   4.195141e-02;
  val[3][1][0][ 9][17] =   2.666605e+00; eval[3][1][0][ 9][17] =   4.912964e-02;
  val[3][1][0][ 9][18] =   2.748274e+00; eval[3][1][0][ 9][18] =   4.625578e-02;
  val[3][1][0][ 9][19] =   2.646668e+00; eval[3][1][0][ 9][19] =   6.313051e-02;
  val[3][1][0][ 9][20] =   2.501054e+00; eval[3][1][0][ 9][20] =   8.249327e-02;
  val[3][1][0][ 9][21] =   2.672074e+00; eval[3][1][0][ 9][21] =   1.284373e-01;
  val[3][1][0][ 9][22] =   2.920790e+00; eval[3][1][0][ 9][22] =   1.493153e-01;
  val[3][1][0][ 9][23] =   5.923342e+03; eval[3][1][0][ 9][23] =   7.037267e+03;
  val[3][1][0][ 9][24] =   2.593948e+00; eval[3][1][0][ 9][24] =   2.672811e-01;
  val[3][1][0][ 9][25] =   2.480492e+00; eval[3][1][0][ 9][25] =   3.912812e-01;
  val[3][1][0][ 9][26] =   4.907027e+00; eval[3][1][0][ 9][26] =   1.672248e+00;
  val[3][1][0][ 9][27] =   5.667610e+00; eval[3][1][0][ 9][27] =   2.761847e+00;
  //emcdz_sigma_c1d1z0
  n_val[3][1][1][0] = 28;
  val[3][1][1][ 0][ 0] =   3.191652e+00; eval[3][1][1][ 0][ 0] =   1.651647e-02;
  val[3][1][1][ 0][ 1] =   2.958374e+00; eval[3][1][1][ 0][ 1] =   1.835937e-02;
  val[3][1][1][ 0][ 2] =   2.895977e+00; eval[3][1][1][ 0][ 2] =   2.297171e-02;
  val[3][1][1][ 0][ 3] =   2.860136e+00; eval[3][1][1][ 0][ 3] =   1.504010e-03;
  val[3][1][1][ 0][ 4] =   2.803909e+00; eval[3][1][1][ 0][ 4] =   2.202723e-03;
  val[3][1][1][ 0][ 5] =   2.773508e+00; eval[3][1][1][ 0][ 5] =   2.955731e-03;
  val[3][1][1][ 0][ 6] =   2.786515e+00; eval[3][1][1][ 0][ 6] =   3.780993e-03;
  val[3][1][1][ 0][ 7] =   2.798194e+00; eval[3][1][1][ 0][ 7] =   5.165653e-03;
  val[3][1][1][ 0][ 8] =   2.770104e+00; eval[3][1][1][ 0][ 8] =   7.043254e-03;
  val[3][1][1][ 0][ 9] =   2.830177e+00; eval[3][1][1][ 0][ 9] =   9.444343e-03;
  val[3][1][1][ 0][10] =   2.768848e+00; eval[3][1][1][ 0][10] =   1.209197e-02;
  val[3][1][1][ 0][11] =   2.809629e+00; eval[3][1][1][ 0][11] =   1.575846e-02;
  val[3][1][1][ 0][12] =   2.731597e+00; eval[3][1][1][ 0][12] =   1.927964e-02;
  val[3][1][1][ 0][13] =   2.733269e+00; eval[3][1][1][ 0][13] =   2.462244e-02;
  val[3][1][1][ 0][14] =   2.728278e+00; eval[3][1][1][ 0][14] =   3.075354e-02;
  val[3][1][1][ 0][15] =   2.726168e+00; eval[3][1][1][ 0][15] =   3.899295e-02;
  val[3][1][1][ 0][16] =   2.677882e+00; eval[3][1][1][ 0][16] =   4.820674e-02;
  val[3][1][1][ 0][17] =   2.727643e+00; eval[3][1][1][ 0][17] =   5.439995e-02;
  val[3][1][1][ 0][18] =   2.728155e+00; eval[3][1][1][ 0][18] =   5.335524e-02;
  val[3][1][1][ 0][19] =   2.695957e+00; eval[3][1][1][ 0][19] =   6.756305e-02;
  val[3][1][1][ 0][20] =   2.368539e+00; eval[3][1][1][ 0][20] =   9.990743e-02;
  val[3][1][1][ 0][21] =   2.713491e+00; eval[3][1][1][ 0][21] =   1.404049e-01;
  val[3][1][1][ 0][22] =   2.702147e+00; eval[3][1][1][ 0][22] =   1.778362e-01;
  val[3][1][1][ 0][23] =   2.314837e+00; eval[3][1][1][ 0][23] =   1.738627e-01;
  val[3][1][1][ 0][24] =   2.941390e+00; eval[3][1][1][ 0][24] =   3.242384e-01;
  val[3][1][1][ 0][25] =   3.128050e+00; eval[3][1][1][ 0][25] =   4.977860e-01;
  val[3][1][1][ 0][26] =   5.173347e+00; eval[3][1][1][ 0][26] =   2.479288e+00;
  val[3][1][1][ 0][27] =   3.570817e+00; eval[3][1][1][ 0][27] =   1.207653e+00;
  //emcdz_sigma_c1d1z1
  n_val[3][1][1][1] = 28;
  val[3][1][1][ 1][ 0] =   3.072271e+00; eval[3][1][1][ 1][ 0] =   1.376270e-02;
  val[3][1][1][ 1][ 1] =   2.777823e+00; eval[3][1][1][ 1][ 1] =   1.416928e-02;
  val[3][1][1][ 1][ 2] =   2.637592e+00; eval[3][1][1][ 1][ 2] =   1.899420e-02;
  val[3][1][1][ 1][ 3] =   2.586405e+00; eval[3][1][1][ 1][ 3] =   1.239977e-03;
  val[3][1][1][ 1][ 4] =   2.569802e+00; eval[3][1][1][ 1][ 4] =   1.647908e-03;
  val[3][1][1][ 1][ 5] =   2.543414e+00; eval[3][1][1][ 1][ 5] =   2.204288e-03;
  val[3][1][1][ 1][ 6] =   2.541727e+00; eval[3][1][1][ 1][ 6] =   2.795136e-03;
  val[3][1][1][ 1][ 7] =   2.479156e+00; eval[3][1][1][ 1][ 7] =   4.103943e-03;
  val[3][1][1][ 1][ 8] =   2.444685e+00; eval[3][1][1][ 1][ 8] =   5.506276e-03;
  val[3][1][1][ 1][ 9] =   2.445884e+00; eval[3][1][1][ 1][ 9] =   7.269828e-03;
  val[3][1][1][ 1][10] =   2.439007e+00; eval[3][1][1][ 1][10] =   9.519888e-03;
  val[3][1][1][ 1][11] =   2.437027e+00; eval[3][1][1][ 1][11] =   1.234305e-02;
  val[3][1][1][ 1][12] =   2.388107e+00; eval[3][1][1][ 1][12] =   1.514875e-02;
  val[3][1][1][ 1][13] =   2.404630e+00; eval[3][1][1][ 1][13] =   1.938255e-02;
  val[3][1][1][ 1][14] =   2.378517e+00; eval[3][1][1][ 1][14] =   2.377986e-02;
  val[3][1][1][ 1][15] =   2.355767e+00; eval[3][1][1][ 1][15] =   2.967204e-02;
  val[3][1][1][ 1][16] =   2.357879e+00; eval[3][1][1][ 1][16] =   3.618598e-02;
  val[3][1][1][ 1][17] =   2.300437e+00; eval[3][1][1][ 1][17] =   4.524387e-02;
  val[3][1][1][ 1][18] =   2.318942e+00; eval[3][1][1][ 1][18] =   3.802991e-02;
  val[3][1][1][ 1][19] =   2.327791e+00; eval[3][1][1][ 1][19] =   6.325756e-02;
  val[3][1][1][ 1][20] =   2.282219e+00; eval[3][1][1][ 1][20] =   7.189350e-02;
  val[3][1][1][ 1][21] =   2.254915e+00; eval[3][1][1][ 1][21] =   8.819915e-02;
  val[3][1][1][ 1][22] =   2.230289e+00; eval[3][1][1][ 1][22] =   1.243183e-01;
  val[3][1][1][ 1][23] =   2.307738e+00; eval[3][1][1][ 1][23] =   1.522512e-01;
  val[3][1][1][ 1][24] =   2.479736e+00; eval[3][1][1][ 1][24] =   2.286102e-01;
  val[3][1][1][ 1][25] =   2.013809e+00; eval[3][1][1][ 1][25] =   1.803929e-01;
  val[3][1][1][ 1][26] =   2.153539e+00; eval[3][1][1][ 1][26] =   3.531714e-01;
  val[3][1][1][ 1][27] =   2.649615e+00; eval[3][1][1][ 1][27] =   4.556022e-01;
  //emcdz_sigma_c1d1z2
  n_val[3][1][1][2] = 28;
  val[3][1][1][ 2][ 0] =   2.988139e+00; eval[3][1][1][ 2][ 0] =   1.265570e-02;
  val[3][1][1][ 2][ 1] =   2.632407e+00; eval[3][1][1][ 2][ 1] =   1.420740e-02;
  val[3][1][1][ 2][ 2] =   2.504489e+00; eval[3][1][1][ 2][ 2] =   1.711455e-02;
  val[3][1][1][ 2][ 3] =   2.366752e+00; eval[3][1][1][ 2][ 3] =   1.149396e-03;
  val[3][1][1][ 2][ 4] =   2.324187e+00; eval[3][1][1][ 2][ 4] =   1.512077e-03;
  val[3][1][1][ 2][ 5] =   2.309019e+00; eval[3][1][1][ 2][ 5] =   2.037341e-03;
  val[3][1][1][ 2][ 6] =   2.274592e+00; eval[3][1][1][ 2][ 6] =   2.562226e-03;
  val[3][1][1][ 2][ 7] =   2.266183e+00; eval[3][1][1][ 2][ 7] =   3.481700e-03;
  val[3][1][1][ 2][ 8] =   2.242230e+00; eval[3][1][1][ 2][ 8] =   4.550049e-03;
  val[3][1][1][ 2][ 9] =   2.233581e+00; eval[3][1][1][ 2][ 9] =   5.895833e-03;
  val[3][1][1][ 2][10] =   2.215261e+00; eval[3][1][1][ 2][10] =   7.714798e-03;
  val[3][1][1][ 2][11] =   2.149766e+00; eval[3][1][1][ 2][11] =   1.072158e-02;
  val[3][1][1][ 2][12] =   2.132041e+00; eval[3][1][1][ 2][12] =   1.335652e-02;
  val[3][1][1][ 2][13] =   2.085162e+00; eval[3][1][1][ 2][13] =   1.630047e-02;
  val[3][1][1][ 2][14] =   2.117028e+00; eval[3][1][1][ 2][14] =   2.073203e-02;
  val[3][1][1][ 2][15] =   2.057182e+00; eval[3][1][1][ 2][15] =   2.424373e-02;
  val[3][1][1][ 2][16] =   2.026251e+00; eval[3][1][1][ 2][16] =   2.832163e-02;
  val[3][1][1][ 2][17] =   2.065102e+00; eval[3][1][1][ 2][17] =   3.720082e-02;
  val[3][1][1][ 2][18] =   2.045610e+00; eval[3][1][1][ 2][18] =   3.299012e-02;
  val[3][1][1][ 2][19] =   1.995827e+00; eval[3][1][1][ 2][19] =   4.357920e-02;
  val[3][1][1][ 2][20] =   1.877736e+00; eval[3][1][1][ 2][20] =   5.808538e-02;
  val[3][1][1][ 2][21] =   2.038132e+00; eval[3][1][1][ 2][21] =   8.393148e-02;
  val[3][1][1][ 2][22] =   1.798551e+00; eval[3][1][1][ 2][22] =   8.452368e-02;
  val[3][1][1][ 2][23] =   1.897565e+00; eval[3][1][1][ 2][23] =   1.173024e-01;
  val[3][1][1][ 2][24] =   1.737749e+00; eval[3][1][1][ 2][24] =   1.388722e-01;
  val[3][1][1][ 2][25] =   1.900955e+00; eval[3][1][1][ 2][25] =   2.223616e-01;
  val[3][1][1][ 2][26] =   1.780042e+00; eval[3][1][1][ 2][26] =   2.048759e-01;
  val[3][1][1][ 2][27] =   1.543702e+00; eval[3][1][1][ 2][27] =   2.443809e-01;
  //emcdz_sigma_c1d1z3
  n_val[3][1][1][3] = 28;
  val[3][1][1][ 3][ 0] =   2.955102e+00; eval[3][1][1][ 3][ 0] =   1.251171e-02;
  val[3][1][1][ 3][ 1] =   2.534805e+00; eval[3][1][1][ 3][ 1] =   1.325366e-02;
  val[3][1][1][ 3][ 2] =   2.405277e+00; eval[3][1][1][ 3][ 2] =   1.567603e-02;
  val[3][1][1][ 3][ 3] =   2.259808e+00; eval[3][1][1][ 3][ 3] =   1.043815e-03;
  val[3][1][1][ 3][ 4] =   2.207950e+00; eval[3][1][1][ 3][ 4] =   1.345411e-03;
  val[3][1][1][ 3][ 5] =   2.159398e+00; eval[3][1][1][ 3][ 5] =   1.768822e-03;
  val[3][1][1][ 3][ 6] =   2.129093e+00; eval[3][1][1][ 3][ 6] =   2.227261e-03;
  val[3][1][1][ 3][ 7] =   2.043521e+00; eval[3][1][1][ 3][ 7] =   3.179951e-03;
  val[3][1][1][ 3][ 8] =   2.022990e+00; eval[3][1][1][ 3][ 8] =   4.202300e-03;
  val[3][1][1][ 3][ 9] =   2.025591e+00; eval[3][1][1][ 3][ 9] =   5.632960e-03;
  val[3][1][1][ 3][10] =   1.998586e+00; eval[3][1][1][ 3][10] =   7.194854e-03;
  val[3][1][1][ 3][11] =   1.966502e+00; eval[3][1][1][ 3][11] =   9.103521e-03;
  val[3][1][1][ 3][12] =   1.967247e+00; eval[3][1][1][ 3][12] =   1.183468e-02;
  val[3][1][1][ 3][13] =   1.969152e+00; eval[3][1][1][ 3][13] =   1.487472e-02;
  val[3][1][1][ 3][14] =   1.950950e+00; eval[3][1][1][ 3][14] =   1.812997e-02;
  val[3][1][1][ 3][15] =   1.985237e+00; eval[3][1][1][ 3][15] =   2.351993e-02;
  val[3][1][1][ 3][16] =   1.858028e+00; eval[3][1][1][ 3][16] =   2.982140e-02;
  val[3][1][1][ 3][17] =   1.780841e+00; eval[3][1][1][ 3][17] =   3.131540e-02;
  val[3][1][1][ 3][18] =   1.840322e+00; eval[3][1][1][ 3][18] =   3.164077e-02;
  val[3][1][1][ 3][19] =   1.731483e+00; eval[3][1][1][ 3][19] =   3.857069e-02;
  val[3][1][1][ 3][20] =   1.695547e+00; eval[3][1][1][ 3][20] =   5.120732e-02;
  val[3][1][1][ 3][21] =   1.657390e+00; eval[3][1][1][ 3][21] =   6.268279e-02;
  val[3][1][1][ 3][22] =   1.850058e+00; eval[3][1][1][ 3][22] =   9.146212e-02;
  val[3][1][1][ 3][23] =   1.608276e+00; eval[3][1][1][ 3][23] =   1.064311e-01;
  val[3][1][1][ 3][24] =   1.858733e+00; eval[3][1][1][ 3][24] =   1.482460e-01;
  val[3][1][1][ 3][25] =   1.520665e+00; eval[3][1][1][ 3][25] =   1.111591e-01;
  val[3][1][1][ 3][26] =   1.351687e+00; eval[3][1][1][ 3][26] =   1.793442e-01;
  val[3][1][1][ 3][27] =   2.409421e+00; eval[3][1][1][ 3][27] =   5.083712e-01;
  //emcdz_sigma_c1d1z4
  n_val[3][1][1][4] = 28;
  val[3][1][1][ 4][ 0] =   2.838203e+00; eval[3][1][1][ 4][ 0] =   1.409896e-02;
  val[3][1][1][ 4][ 1] =   2.501791e+00; eval[3][1][1][ 4][ 1] =   1.500447e-02;
  val[3][1][1][ 4][ 2] =   2.382932e+00; eval[3][1][1][ 4][ 2] =   1.805260e-02;
  val[3][1][1][ 4][ 3] =   2.249554e+00; eval[3][1][1][ 4][ 3] =   1.219379e-03;
  val[3][1][1][ 4][ 4] =   2.191578e+00; eval[3][1][1][ 4][ 4] =   1.588127e-03;
  val[3][1][1][ 4][ 5] =   2.143441e+00; eval[3][1][1][ 4][ 5] =   2.040249e-03;
  val[3][1][1][ 4][ 6] =   2.106625e+00; eval[3][1][1][ 4][ 6] =   2.546792e-03;
  val[3][1][1][ 4][ 7] =   2.023592e+00; eval[3][1][1][ 4][ 7] =   3.675337e-03;
  val[3][1][1][ 4][ 8] =   2.008065e+00; eval[3][1][1][ 4][ 8] =   4.884670e-03;
  val[3][1][1][ 4][ 9] =   1.988226e+00; eval[3][1][1][ 4][ 9] =   6.491629e-03;
  val[3][1][1][ 4][10] =   1.960298e+00; eval[3][1][1][ 4][10] =   8.435210e-03;
  val[3][1][1][ 4][11] =   1.957941e+00; eval[3][1][1][ 4][11] =   1.081780e-02;
  val[3][1][1][ 4][12] =   1.950835e+00; eval[3][1][1][ 4][12] =   1.377895e-02;
  val[3][1][1][ 4][13] =   1.918257e+00; eval[3][1][1][ 4][13] =   1.709883e-02;
  val[3][1][1][ 4][14] =   1.898946e+00; eval[3][1][1][ 4][14] =   2.096449e-02;
  val[3][1][1][ 4][15] =   1.825028e+00; eval[3][1][1][ 4][15] =   2.625876e-02;
  val[3][1][1][ 4][16] =   1.866226e+00; eval[3][1][1][ 4][16] =   3.387126e-02;
  val[3][1][1][ 4][17] =   1.853215e+00; eval[3][1][1][ 4][17] =   4.141596e-02;
  val[3][1][1][ 4][18] =   1.769405e+00; eval[3][1][1][ 4][18] =   3.492370e-02;
  val[3][1][1][ 4][19] =   1.802366e+00; eval[3][1][1][ 4][19] =   5.054140e-02;
  val[3][1][1][ 4][20] =   1.672336e+00; eval[3][1][1][ 4][20] =   6.694738e-02;
  val[3][1][1][ 4][21] =   1.781201e+00; eval[3][1][1][ 4][21] =   9.399815e-02;
  val[3][1][1][ 4][22] =   1.685330e+00; eval[3][1][1][ 4][22] =   1.044427e-01;
  val[3][1][1][ 4][23] =   1.663303e+00; eval[3][1][1][ 4][23] =   1.429697e-01;
  val[3][1][1][ 4][24] =   2.332005e+00; eval[3][1][1][ 4][24] =   4.254072e-01;
  val[3][1][1][ 4][25] =   1.466647e+00; eval[3][1][1][ 4][25] =   2.331332e-01;
  val[3][1][1][ 4][26] =   2.037798e+00; eval[3][1][1][ 4][26] =   6.071902e-01;
  val[3][1][1][ 4][27] =   1.935653e+00; eval[3][1][1][ 4][27] =   6.283331e-01;
  //emcdz_sigma_c1d1z5
  n_val[3][1][1][5] = 28;
  val[3][1][1][ 5][ 0] =   2.809870e+00; eval[3][1][1][ 5][ 0] =   1.395798e-02;
  val[3][1][1][ 5][ 1] =   2.465369e+00; eval[3][1][1][ 5][ 1] =   1.448924e-02;
  val[3][1][1][ 5][ 2] =   2.305839e+00; eval[3][1][1][ 5][ 2] =   1.815439e-02;
  val[3][1][1][ 5][ 3] =   2.217518e+00; eval[3][1][1][ 5][ 3] =   1.121867e-03;
  val[3][1][1][ 5][ 4] =   2.164638e+00; eval[3][1][1][ 5][ 4] =   1.454934e-03;
  val[3][1][1][ 5][ 5] =   2.066139e+00; eval[3][1][1][ 5][ 5] =   2.004607e-03;
  val[3][1][1][ 5][ 6] =   2.038647e+00; eval[3][1][1][ 5][ 6] =   2.538988e-03;
  val[3][1][1][ 5][ 7] =   2.020889e+00; eval[3][1][1][ 5][ 7] =   3.491171e-03;
  val[3][1][1][ 5][ 8] =   1.990494e+00; eval[3][1][1][ 5][ 8] =   4.609412e-03;
  val[3][1][1][ 5][ 9] =   1.981172e+00; eval[3][1][1][ 5][ 9] =   6.144488e-03;
  val[3][1][1][ 5][10] =   1.965827e+00; eval[3][1][1][ 5][10] =   7.998335e-03;
  val[3][1][1][ 5][11] =   1.941371e+00; eval[3][1][1][ 5][11] =   1.028104e-02;
  val[3][1][1][ 5][12] =   1.917596e+00; eval[3][1][1][ 5][12] =   1.279197e-02;
  val[3][1][1][ 5][13] =   1.815916e+00; eval[3][1][1][ 5][13] =   1.659341e-02;
  val[3][1][1][ 5][14] =   1.831562e+00; eval[3][1][1][ 5][14] =   2.135216e-02;
  val[3][1][1][ 5][15] =   1.781972e+00; eval[3][1][1][ 5][15] =   2.593285e-02;
  val[3][1][1][ 5][16] =   1.886044e+00; eval[3][1][1][ 5][16] =   3.521043e-02;
  val[3][1][1][ 5][17] =   1.754922e+00; eval[3][1][1][ 5][17] =   3.564028e-02;
  val[3][1][1][ 5][18] =   1.805292e+00; eval[3][1][1][ 5][18] =   3.613415e-02;
  val[3][1][1][ 5][19] =   1.725738e+00; eval[3][1][1][ 5][19] =   4.778085e-02;
  val[3][1][1][ 5][20] =   1.631640e+00; eval[3][1][1][ 5][20] =   6.759243e-02;
  val[3][1][1][ 5][21] =   1.661551e+00; eval[3][1][1][ 5][21] =   8.227520e-02;
  val[3][1][1][ 5][22] =   1.744601e+00; eval[3][1][1][ 5][22] =   1.120634e-01;
  val[3][1][1][ 5][23] =   1.662333e+00; eval[3][1][1][ 5][23] =   9.860997e-02;
  val[3][1][1][ 5][24] =   1.523917e+00; eval[3][1][1][ 5][24] =   1.396906e-01;
  val[3][1][1][ 5][25] =   2.021555e+00; eval[3][1][1][ 5][25] =   2.635638e-01;
  val[3][1][1][ 5][26] =   2.139548e+00; eval[3][1][1][ 5][26] =   4.155804e-01;
  val[3][1][1][ 5][27] =   1.565724e+03; eval[3][1][1][ 5][27] =   9.199230e+03;
  //emcdz_sigma_c1d1z6
  n_val[3][1][1][6] = 28;
  val[3][1][1][ 6][ 0] =   2.954506e+00; eval[3][1][1][ 6][ 0] =   1.196335e-02;
  val[3][1][1][ 6][ 1] =   2.524261e+00; eval[3][1][1][ 6][ 1] =   1.262712e-02;
  val[3][1][1][ 6][ 2] =   2.389668e+00; eval[3][1][1][ 6][ 2] =   1.523383e-02;
  val[3][1][1][ 6][ 3] =   2.229427e+00; eval[3][1][1][ 6][ 3] =   1.000915e-03;
  val[3][1][1][ 6][ 4] =   2.170608e+00; eval[3][1][1][ 6][ 4] =   1.299272e-03;
  val[3][1][1][ 6][ 5] =   2.124814e+00; eval[3][1][1][ 6][ 5] =   1.723030e-03;
  val[3][1][1][ 6][ 6] =   2.028813e+00; eval[3][1][1][ 6][ 6] =   2.256881e-03;
  val[3][1][1][ 6][ 7] =   2.007891e+00; eval[3][1][1][ 6][ 7] =   3.091300e-03;
  val[3][1][1][ 6][ 8] =   1.980073e+00; eval[3][1][1][ 6][ 8] =   4.127383e-03;
  val[3][1][1][ 6][ 9] =   1.952057e+00; eval[3][1][1][ 6][ 9] =   5.365198e-03;
  val[3][1][1][ 6][10] =   1.937222e+00; eval[3][1][1][ 6][10] =   7.050623e-03;
  val[3][1][1][ 6][11] =   1.909853e+00; eval[3][1][1][ 6][11] =   9.150249e-03;
  val[3][1][1][ 6][12] =   1.903679e+00; eval[3][1][1][ 6][12] =   1.184096e-02;
  val[3][1][1][ 6][13] =   1.885545e+00; eval[3][1][1][ 6][13] =   1.438277e-02;
  val[3][1][1][ 6][14] =   1.754738e+00; eval[3][1][1][ 6][14] =   1.750329e-02;
  val[3][1][1][ 6][15] =   1.760139e+00; eval[3][1][1][ 6][15] =   2.240190e-02;
  val[3][1][1][ 6][16] =   1.789232e+00; eval[3][1][1][ 6][16] =   2.832839e-02;
  val[3][1][1][ 6][17] =   1.802100e+00; eval[3][1][1][ 6][17] =   3.400048e-02;
  val[3][1][1][ 6][18] =   1.752880e+00; eval[3][1][1][ 6][18] =   2.965211e-02;
  val[3][1][1][ 6][19] =   1.735987e+00; eval[3][1][1][ 6][19] =   4.426085e-02;
  val[3][1][1][ 6][20] =   1.636348e+00; eval[3][1][1][ 6][20] =   4.838558e-02;
  val[3][1][1][ 6][21] =   1.605820e+00; eval[3][1][1][ 6][21] =   6.586138e-02;
  val[3][1][1][ 6][22] =   1.640124e+00; eval[3][1][1][ 6][22] =   7.495019e-02;
  val[3][1][1][ 6][23] =   1.859964e+00; eval[3][1][1][ 6][23] =   1.299695e-01;
  val[3][1][1][ 6][24] =   1.897996e+00; eval[3][1][1][ 6][24] =   2.028796e-01;
  val[3][1][1][ 6][25] =   2.128487e+00; eval[3][1][1][ 6][25] =   2.835393e-01;
  val[3][1][1][ 6][26] =   1.891162e+00; eval[3][1][1][ 6][26] =   4.460642e-01;
  val[3][1][1][ 6][27] =   2.497319e+00; eval[3][1][1][ 6][27] =   6.522863e-01;
  //emcdz_sigma_c1d1z7
  n_val[3][1][1][7] = 28;
  val[3][1][1][ 7][ 0] =   2.942472e+00; eval[3][1][1][ 7][ 0] =   1.286240e-02;
  val[3][1][1][ 7][ 1] =   2.595783e+00; eval[3][1][1][ 7][ 1] =   1.316165e-02;
  val[3][1][1][ 7][ 2] =   2.460990e+00; eval[3][1][1][ 7][ 2] =   1.584341e-02;
  val[3][1][1][ 7][ 3] =   2.320807e+00; eval[3][1][1][ 7][ 3] =   1.060846e-03;
  val[3][1][1][ 7][ 4] =   2.271155e+00; eval[3][1][1][ 7][ 4] =   1.398701e-03;
  val[3][1][1][ 7][ 5] =   2.248846e+00; eval[3][1][1][ 7][ 5] =   1.844158e-03;
  val[3][1][1][ 7][ 6] =   2.205339e+00; eval[3][1][1][ 7][ 6] =   2.272248e-03;
  val[3][1][1][ 7][ 7] =   2.190991e+00; eval[3][1][1][ 7][ 7] =   3.102997e-03;
  val[3][1][1][ 7][ 8] =   2.170845e+00; eval[3][1][1][ 7][ 8] =   4.144453e-03;
  val[3][1][1][ 7][ 9] =   2.100115e+00; eval[3][1][1][ 7][ 9] =   5.855931e-03;
  val[3][1][1][ 7][10] =   2.077352e+00; eval[3][1][1][ 7][10] =   7.617624e-03;
  val[3][1][1][ 7][11] =   2.068406e+00; eval[3][1][1][ 7][11] =   9.969166e-03;
  val[3][1][1][ 7][12] =   2.046435e+00; eval[3][1][1][ 7][12] =   1.247995e-02;
  val[3][1][1][ 7][13] =   2.020887e+00; eval[3][1][1][ 7][13] =   1.549965e-02;
  val[3][1][1][ 7][14] =   2.038941e+00; eval[3][1][1][ 7][14] =   2.060347e-02;
  val[3][1][1][ 7][15] =   1.965587e+00; eval[3][1][1][ 7][15] =   2.404371e-02;
  val[3][1][1][ 7][16] =   1.982932e+00; eval[3][1][1][ 7][16] =   2.911108e-02;
  val[3][1][1][ 7][17] =   1.971266e+00; eval[3][1][1][ 7][17] =   3.351539e-02;
  val[3][1][1][ 7][18] =   1.978758e+00; eval[3][1][1][ 7][18] =   3.324939e-02;
  val[3][1][1][ 7][19] =   1.828155e+00; eval[3][1][1][ 7][19] =   4.563677e-02;
  val[3][1][1][ 7][20] =   1.869829e+00; eval[3][1][1][ 7][20] =   6.159755e-02;
  val[3][1][1][ 7][21] =   1.920111e+00; eval[3][1][1][ 7][21] =   7.841273e-02;
  val[3][1][1][ 7][22] =   1.825291e+00; eval[3][1][1][ 7][22] =   9.118104e-02;
  val[3][1][1][ 7][23] =   1.883266e+00; eval[3][1][1][ 7][23] =   1.348177e-01;
  val[3][1][1][ 7][24] =   1.746144e+00; eval[3][1][1][ 7][24] =   1.608959e-01;
  val[3][1][1][ 7][25] =   1.706380e+00; eval[3][1][1][ 7][25] =   1.720593e-01;
  val[3][1][1][ 7][26] =   1.527624e+00; eval[3][1][1][ 7][26] =   3.171174e-01;
  val[3][1][1][ 7][27] =   1.750672e+00; eval[3][1][1][ 7][27] =   3.016371e-01;
  //emcdz_sigma_c1d1z8
  n_val[3][1][1][8] = 28;
  val[3][1][1][ 8][ 0] =   3.098274e+00; eval[3][1][1][ 8][ 0] =   1.253974e-02;
  val[3][1][1][ 8][ 1] =   2.772382e+00; eval[3][1][1][ 8][ 1] =   1.349449e-02;
  val[3][1][1][ 8][ 2] =   2.604917e+00; eval[3][1][1][ 8][ 2] =   1.785400e-02;
  val[3][1][1][ 8][ 3] =   2.552767e+00; eval[3][1][1][ 8][ 3] =   1.175995e-03;
  val[3][1][1][ 8][ 4] =   2.532068e+00; eval[3][1][1][ 8][ 4] =   1.612179e-03;
  val[3][1][1][ 8][ 5] =   2.496931e+00; eval[3][1][1][ 8][ 5] =   2.137409e-03;
  val[3][1][1][ 8][ 6] =   2.486125e+00; eval[3][1][1][ 8][ 6] =   2.663756e-03;
  val[3][1][1][ 8][ 7] =   2.481910e+00; eval[3][1][1][ 8][ 7] =   3.653482e-03;
  val[3][1][1][ 8][ 8] =   2.441195e+00; eval[3][1][1][ 8][ 8] =   4.893376e-03;
  val[3][1][1][ 8][ 9] =   2.338762e+00; eval[3][1][1][ 8][ 9] =   6.727005e-03;
  val[3][1][1][ 8][10] =   2.342466e+00; eval[3][1][1][ 8][10] =   8.910259e-03;
  val[3][1][1][ 8][11] =   2.308653e+00; eval[3][1][1][ 8][11] =   1.140503e-02;
  val[3][1][1][ 8][12] =   2.273502e+00; eval[3][1][1][ 8][12] =   1.445004e-02;
  val[3][1][1][ 8][13] =   2.310499e+00; eval[3][1][1][ 8][13] =   1.897530e-02;
  val[3][1][1][ 8][14] =   2.253152e+00; eval[3][1][1][ 8][14] =   2.217742e-02;
  val[3][1][1][ 8][15] =   2.239788e+00; eval[3][1][1][ 8][15] =   2.757606e-02;
  val[3][1][1][ 8][16] =   2.229004e+00; eval[3][1][1][ 8][16] =   3.482233e-02;
  val[3][1][1][ 8][17] =   2.250629e+00; eval[3][1][1][ 8][17] =   4.228386e-02;
  val[3][1][1][ 8][18] =   2.278533e+00; eval[3][1][1][ 8][18] =   4.057382e-02;
  val[3][1][1][ 8][19] =   2.160146e+00; eval[3][1][1][ 8][19] =   5.004767e-02;
  val[3][1][1][ 8][20] =   2.126527e+00; eval[3][1][1][ 8][20] =   7.543521e-02;
  val[3][1][1][ 8][21] =   2.003085e+00; eval[3][1][1][ 8][21] =   8.424271e-02;
  val[3][1][1][ 8][22] =   2.216840e+00; eval[3][1][1][ 8][22] =   1.222226e-01;
  val[3][1][1][ 8][23] =   2.310452e+00; eval[3][1][1][ 8][23] =   1.913906e-01;
  val[3][1][1][ 8][24] =   2.617168e+00; eval[3][1][1][ 8][24] =   2.720978e-01;
  val[3][1][1][ 8][25] =   2.303173e+00; eval[3][1][1][ 8][25] =   3.740717e-01;
  val[3][1][1][ 8][26] =   2.491391e+00; eval[3][1][1][ 8][26] =   4.452157e-01;
  val[3][1][1][ 8][27] =   2.943086e+00; eval[3][1][1][ 8][27] =   9.194128e-01;
  //emcdz_sigma_c1d1z9
  n_val[3][1][1][9] = 28;
  val[3][1][1][ 9][ 0] =   3.221886e+00; eval[3][1][1][ 9][ 0] =   1.440993e-02;
  val[3][1][1][ 9][ 1] =   2.967582e+00; eval[3][1][1][ 9][ 1] =   1.724161e-02;
  val[3][1][1][ 9][ 2] =   2.888081e+00; eval[3][1][1][ 9][ 2] =   2.226749e-02;
  val[3][1][1][ 9][ 3] =   2.830293e+00; eval[3][1][1][ 9][ 3] =   1.442497e-03;
  val[3][1][1][ 9][ 4] =   2.869501e+00; eval[3][1][1][ 9][ 4] =   2.101175e-03;
  val[3][1][1][ 9][ 5] =   2.842076e+00; eval[3][1][1][ 9][ 5] =   2.817973e-03;
  val[3][1][1][ 9][ 6] =   2.779520e+00; eval[3][1][1][ 9][ 6] =   3.817567e-03;
  val[3][1][1][ 9][ 7] =   2.731061e+00; eval[3][1][1][ 9][ 7] =   5.168247e-03;
  val[3][1][1][ 9][ 8] =   2.716343e+00; eval[3][1][1][ 9][ 8] =   6.969352e-03;
  val[3][1][1][ 9][ 9] =   2.722055e+00; eval[3][1][1][ 9][ 9] =   9.401635e-03;
  val[3][1][1][ 9][10] =   2.706356e+00; eval[3][1][1][ 9][10] =   1.217149e-02;
  val[3][1][1][ 9][11] =   2.705043e+00; eval[3][1][1][ 9][11] =   1.584235e-02;
  val[3][1][1][ 9][12] =   2.695059e+00; eval[3][1][1][ 9][12] =   2.064867e-02;
  val[3][1][1][ 9][13] =   2.714703e+00; eval[3][1][1][ 9][13] =   2.599464e-02;
  val[3][1][1][ 9][14] =   2.656128e+00; eval[3][1][1][ 9][14] =   3.133085e-02;
  val[3][1][1][ 9][15] =   2.715741e+00; eval[3][1][1][ 9][15] =   4.021141e-02;
  val[3][1][1][ 9][16] =   2.748891e+00; eval[3][1][1][ 9][16] =   4.856458e-02;
  val[3][1][1][ 9][17] =   2.508248e+00; eval[3][1][1][ 9][17] =   5.211108e-02;
  val[3][1][1][ 9][18] =   2.620463e+00; eval[3][1][1][ 9][18] =   5.078902e-02;
  val[3][1][1][ 9][19] =   2.622446e+00; eval[3][1][1][ 9][19] =   7.005490e-02;
  val[3][1][1][ 9][20] =   2.391595e+00; eval[3][1][1][ 9][20] =   9.279736e-02;
  val[3][1][1][ 9][21] =   2.365719e+00; eval[3][1][1][ 9][21] =   1.236123e-01;
  val[3][1][1][ 9][22] =   2.568522e+00; eval[3][1][1][ 9][22] =   1.665779e-01;
  val[3][1][1][ 9][23] =   2.423520e+00; eval[3][1][1][ 9][23] =   2.044267e-01;
  val[3][1][1][ 9][24] =   2.555277e+00; eval[3][1][1][ 9][24] =   4.097125e-01;
  val[3][1][1][ 9][25] =   2.368492e+00; eval[3][1][1][ 9][25] =   3.645033e-01;
  val[3][1][1][ 9][26] =   2.395363e+00; eval[3][1][1][ 9][26] =   4.325457e-01;
  val[3][1][1][ 9][27] =   5.422314e+00; eval[3][1][1][ 9][27] =   4.944769e+00;

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
