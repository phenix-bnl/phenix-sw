#include "EmcPidrecal_dAu_Reco.h"

#include "PHCentralTrack.h"
#include "PHSnglCentralTrack.h"

#include "recoConsts.h"
#include "getClass.h"
#include "PHCompositeNode.h"
#include "Fun4AllServer.h"

#include "TH2.h"

#include <iostream>

  // SET IN SetCalibConsts
  // large array values -- for each of the 271 runs and then each sector and sign
  float RUN[271];
  float w0mp[271],w1mp[271],w2mp[271],w3mp[271],e2mp[271],e3mp[271];
  float w0mn[271],w1mn[271],w2mn[271],w3mn[271],e2mn[271],e3mn[271];

  // hot or dead tower flags -- one flag for each ysect, zsect for each sector
  int flagw0[72][36],flagw1[72][36],flagw2[72][36];
  int flagw3[72][36],flage2[72][36],flage3[72][36];

  // tower corrections -- one value for each ysect, zsect for each sector and sign
  float tw0p[72][36],tw1p[72][36],tw2p[72][36];
  float tw3p[72][36],te2p[72][36],te3p[72][36];
  float tw0n[72][36],tw1n[72][36],tw2n[72][36];
  float tw3n[72][36],te2n[72][36],te3n[72][36];

  // slew correction parameters
  float w0p0,w0p1,w0p2;
  float w1p0,w1p1,w1p2;
  float w2p0,w2p1,w2p2;
  float w3p0,w3p1,w3p2;
  float w0n0,w0n1,w0n2;
  float w1n0,w1n1,w1n2;
  float w2n0,w2n1,w2n2;
  float w3n0,w3n1,w3n2;
  float e2p0,e2p1,e2p2;
  float e3p0,e3p1,e3p2;
  float e2n0,e2n1,e2n2;
  float e3n0,e3n1,e3n2;

  // mom corrections for temc parameters
  float w0p0m,w0p1m,w0p2m;
  float w1p0m,w1p1m,w1p2m;
  float w2p0m,w2p1m,w2p2m;
  float w3p0m,w3p1m,w3p2m;
  float w0n0m,w0n1m,w0n2m;
  float w1n0m,w1n1m,w1n2m;
  float w2n0m,w2n1m,w2n2m;
  float w3n0m,w3n1m,w3n2m;
  float e2p0m,e2p1m,e2p2m;
  float e3p0m,e3p1m,e3p2m;
  float e2n0m,e2n1m,e2n2m;
  float e3n0m,e3n1m,e3n2m;
  
  // M2 fit parameters
  float w0mp0_pi,w1mp0_pi,w2mp0_pi,w3mp0_pi,e2mp0_pi,e3mp0_pi;
  float w0mp1_pi,w1mp1_pi,w2mp1_pi,w3mp1_pi,e2mp1_pi,e3mp1_pi;
  float w0mp2_pi,w1mp2_pi,w2mp2_pi,w3mp2_pi,e2mp2_pi,e3mp2_pi;
  float w0mp3_pi,w1mp3_pi,w2mp3_pi,w3mp3_pi,e2mp3_pi,e3mp3_pi;
  float w0mn0_pi,w1mn0_pi,w2mn0_pi,w3mn0_pi,e2mn0_pi,e3mn0_pi;
  float w0mn1_pi,w1mn1_pi,w2mn1_pi,w3mn1_pi,e2mn1_pi,e3mn1_pi;
  float w0mn2_pi,w1mn2_pi,w2mn2_pi,w3mn2_pi,e2mn2_pi,e3mn2_pi;
  float w0mn3_pi,w1mn3_pi,w2mn3_pi,w3mn3_pi,e2mn3_pi,e3mn3_pi;
  float w0sp0_pi,w1sp0_pi,w2sp0_pi,w3sp0_pi,e2sp0_pi,e3sp0_pi;
  float w0sp1_pi,w1sp1_pi,w2sp1_pi,w3sp1_pi,e2sp1_pi,e3sp1_pi;
  float w0sp2_pi,w1sp2_pi,w2sp2_pi,w3sp2_pi,e2sp2_pi,e3sp2_pi;
  float w0sn0_pi,w1sn0_pi,w2sn0_pi,w3sn0_pi,e2sn0_pi,e3sn0_pi;
  float w0sn1_pi,w1sn1_pi,w2sn1_pi,w3sn1_pi,e2sn1_pi,e3sn1_pi;
  float w0sn2_pi,w1sn2_pi,w2sn2_pi,w3sn2_pi,e2sn2_pi,e3sn2_pi;
  float w0mp0_K,w1mp0_K,w2mp0_K,w3mp0_K,e2mp0_K,e3mp0_K;
  float w0mp1_K,w1mp1_K,w2mp1_K,w3mp1_K,e2mp1_K,e3mp1_K;
  float w0mp2_K,w1mp2_K,w2mp2_K,w3mp2_K,e2mp2_K,e3mp2_K;
  float w0mn0_K,w1mn0_K,w2mn0_K,w3mn0_K,e2mn0_K,e3mn0_K;
  float w0mn1_K,w1mn1_K,w2mn1_K,w3mn1_K,e2mn1_K,e3mn1_K;
  float w0mn2_K,w1mn2_K,w2mn2_K,w3mn2_K,e2mn2_K,e3mn2_K;
  float w0sp0_K,w1sp0_K,w2sp0_K,w3sp0_K,e2sp0_K,e3sp0_K;
  float w0sp1_K,w1sp1_K,w2sp1_K,w3sp1_K,e2sp1_K,e3sp1_K;
  float w0sp2_K,w1sp2_K,w2sp2_K,w3sp2_K,e2sp2_K,e3sp2_K;
  float w0sn0_K,w1sn0_K,w2sn0_K,w3sn0_K,e2sn0_K,e3sn0_K;
  float w0sn1_K,w1sn1_K,w2sn1_K,w3sn1_K,e2sn1_K,e3sn1_K;
  float w0sn2_K,w1sn2_K,w2sn2_K,w3sn2_K,e2sn2_K,e3sn2_K;
  float w0mp0_p,w1mp0_p,w2mp0_p,w3mp0_p,e2mp0_p,e3mp0_p;
  float w0mp1_p,w1mp1_p,w2mp1_p,w3mp1_p,e2mp1_p,e3mp1_p;
  float w0mp2_p,w1mp2_p,w2mp2_p,w3mp2_p,e2mp2_p,e3mp2_p;
  float w0mn0_p,w1mn0_p,w2mn0_p,w3mn0_p,e2mn0_p,e3mn0_p;
  float w0mn1_p,w1mn1_p,w2mn1_p,w3mn1_p,e2mn1_p,e3mn1_p;
  float w0mn2_p,w1mn2_p,w2mn2_p,w3mn2_p,e2mn2_p,e3mn2_p;
  float w0sp0_p,w1sp0_p,w2sp0_p,w3sp0_p,e2sp0_p,e3sp0_p;
  float w0sp1_p,w1sp1_p,w2sp1_p,w3sp1_p,e2sp1_p,e3sp1_p;
  float w0sp2_p,w1sp2_p,w2sp2_p,w3sp2_p,e2sp2_p,e3sp2_p;
  float w0sn0_p,w1sn0_p,w2sn0_p,w3sn0_p,e2sn0_p,e3sn0_p;
  float w0sn1_p,w1sn1_p,w2sn1_p,w3sn1_p,e2sn1_p,e3sn1_p;
  float w0sn2_p,w1sn2_p,w2sn2_p,w3sn2_p,e2sn2_p,e3sn2_p;

using namespace std;

EmcPidrecal_dAu_Reco::EmcPidrecal_dAu_Reco(const string &name): Recalibrator(name)
{
  baseclasses.insert("PHCentralTrack");
  SetCalibConsts();
}

int
EmcPidrecal_dAu_Reco::isValidRun(const int runno) const
  {
    if (runno < 67282 || runno > 80312) // see an466 for run list and reasons why
    {
      return 0;
    }
    
    bool matching_Run =false;
    for (int i=0; i<271; i++)
      {
	if (RUN[i]==runno) { matching_Run=true; }
      }
    
    if (matching_Run)
      {
	return 1; //1 means good keep going
      }
    else 
      {
	return 0;
      }
}

int
EmcPidrecal_dAu_Reco::Init(PHCompositeNode *topNode)
{
  Fun4AllServer *se = Fun4AllServer::instance();
  string Histoname = Name();
  Histoname += "_dAuEMCmomtof";
  dAuEMCmomtof = new TH2F(Histoname.c_str(),"1./momentum vs tof",100,15,40,100,-10,10);
  se->registerHisto(dAuEMCmomtof);

  Histoname = Name();
  Histoname += "_dAuEMCmomtofP";
  dAuEMCmomtofP = new TH2F(Histoname.c_str(),"1./momentum vs tof Proton",100,15,40,100,-10,10);
  se->registerHisto(dAuEMCmomtofP);

  Histoname = Name();
  Histoname += "_dAuEMCmomtofK";
  dAuEMCmomtofK = new TH2F(Histoname.c_str(),"1./momentum vs tof Kaon",100,15,40,100,-10,10);
  se->registerHisto(dAuEMCmomtofK);

  Histoname = Name();
  Histoname += "_dAuEMCmomtofPi";
  dAuEMCmomtofPi = new TH2F(Histoname.c_str(),"1./momentum vs tof Pion",100,15,40,100,-10,10);
  se->registerHisto(dAuEMCmomtofPi);

  return 0;
}

int
EmcPidrecal_dAu_Reco::InitRun(PHCompositeNode *topNode)
{
  recoConsts *rc = recoConsts::instance();
  int runnumber =  rc->get_IntFlag("RUNNUMBER");

  for (int i=0; i<271; i++)
    {
      if (RUN[i]==runnumber)
	{
	  Mw0p=w0mp[i];
	  Mw1p=w1mp[i];
	  Mw2p=w2mp[i];
	  Mw3p=w3mp[i];
	  Me2p=e2mp[i];
	  Me3p=e3mp[i];
	  Mw0n=w0mn[i];
	  Mw1n=w1mn[i];
	  Mw2n=w2mn[i];
	  Mw3n=w3mn[i];
	  Me2n=e2mn[i];
	  Me3n=e3mn[i]; 
	}
    }
  return 0;
}

int
EmcPidrecal_dAu_Reco::process_event(PHCompositeNode *topNode)
{
  PHCentralTrack *d_cnt = findNode::getClass<PHCentralTrack>(topNode,inputnodename.c_str());

  //don't calibrate without CNT
  if (!d_cnt)
    {
      if (verbosity > 0) cout << PHWHERE << "No PHCentralTrack" << endl;
      return 0;
    }
  
  for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
    {
      PHSnglCentralTrack *sngltrk = d_cnt->get_track(i);
      sngltrk->ShutUp();

      if (!(sngltrk->isValid(sngltrk->get_dcarm())) &&
	   (sngltrk->isValid(sngltrk->get_sect ())) &&
	   (sngltrk->isValid(sngltrk->get_ysect())) &&
	   (sngltrk->isValid(sngltrk->get_zsect())) )
	{
	  if (verbosity > 0) cout << PHWHERE << "missing valid values" << endl;
	  continue;
	}

      int dcarm = sngltrk->get_dcarm();
      int sect  = sngltrk->get_sect();
      int ysect = sngltrk->get_ysect();
      int zsect = sngltrk->get_zsect();

      //don't recalibrate the hot towers ?!?
      if(dcarm==1&&sect==0&&(flagw0[zsect][ysect]==1))continue;
      if(dcarm==1&&sect==1&&(flagw1[zsect][ysect]==1))continue;
      if(dcarm==1&&sect==2&&(flagw2[zsect][ysect]==1))continue;
      if(dcarm==1&&sect==3&&(flagw3[zsect][ysect]==1))continue;
      if(dcarm==0&&sect==2&&(flage2[zsect][ysect]==1))continue;
      if(dcarm==0&&sect==3&&(flage3[zsect][ysect]==1))continue;

      float temc   = sngltrk->get_temc();
      float mom    = sngltrk->get_mom();
      float ecent  = sngltrk->get_ecent();
      float alpha  = sngltrk->get_alpha();
      float charge = sngltrk->get_charge();
      if ( (dcarm == 0 && (sect == 2 || sect == 3)) || (dcarm == 1) ) // do this only for pbsc
	{
	  //correct and reset temc, m2emc
	  //-- this subsection of code is taken verbatim out of EMCAnalyzer.C
	  float corr_temc = Correct_temc(temc, mom, ecent, alpha, dcarm, sect, ysect, zsect);
	  float plemc=sngltrk->get_plemc();  
	  float m2emc=mom*mom*(900*corr_temc*corr_temc/plemc/plemc-1);
	  sngltrk->set_temc(corr_temc);
	  sngltrk->set_m2emc(m2emc);

	  //calculate and set isPi,isK,isP 
	  //-- these subsections of code are taken verbatim out of EMCAnalyzer.C
	  float nPi = IsPion  (m2emc, mom, alpha, dcarm, sect);
	  float nK  = IsKaon  (m2emc, mom, alpha, dcarm, sect);
	  float nP  = IsProton(m2emc, mom, alpha, dcarm, sect);
	  sngltrk->set_isPi(nPi);
	  sngltrk->set_isK(nK);
	  sngltrk->set_isP(nP);
	  sngltrk->ShutUp(0);

	  // fill histos
	  dAuEMCmomtof->Fill(sngltrk->get_temc(),1./mom*charge);
	  if (fabs(nPi)<1)
	    {
	      dAuEMCmomtofPi->Fill(sngltrk->get_temc(),1./mom*charge);
	    }
	  if (fabs(nP)<1)
	    {
	      dAuEMCmomtofP->Fill(sngltrk->get_temc(),1./mom*charge);
	    }
	  if (fabs(nK)<1)
	    {
	      dAuEMCmomtofK->Fill(sngltrk->get_temc(),1./mom*charge);
	    }
	}
    }

  return 0;
}

float 	  //-- this subsection of code is taken verbatim out of EMCAnalyzer.C
EmcPidrecal_dAu_Reco::Correct_temc(float temc, const float mom, const float ecent, const float alpha, const int dcarm, const int sect, const int ysect, const int zsect)
{
  if(alpha<0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      temc=temc-Mw0p;
	      temc=temc-tw0p[zsect][ysect];
	      temc=temc-(w0p0+w0p1*(TMath::Log(ecent)+w0p2)*(TMath::Log(ecent)+w0p2));
	      temc=temc-(w0p0m+w0p1m/mom+w0p2m/mom/mom);
	    }
	  if(sect==1)
	    {
	      temc=temc-Mw1p;
	      temc=temc-tw1p[zsect][ysect];
	      temc=temc-(w1p0+w1p1*(TMath::Log(ecent)+w1p2)*(TMath::Log(ecent)+w1p2));
	      temc=temc-(w1p0m+w1p1m/mom+w1p2m/mom/mom);
	    }
	  if(sect==2)
	    {
	      temc=temc-Mw2p;
	      temc=temc-tw2p[zsect][ysect];
	      temc=temc-(w2p0+w2p1*(TMath::Log(ecent)+w2p2)*(TMath::Log(ecent)+w2p2));
	      temc=temc-(w2p0m+w2p1m/mom+w2p2m/mom/mom);
	    }
	  if(sect==3)
	    {
	      temc=temc-Mw3p;
	      temc=temc-tw3p[zsect][ysect];
	      temc=temc-(w3p0+w3p1*(TMath::Log(ecent)+w3p2)*(TMath::Log(ecent)+w3p2));
	      temc=temc-(w3p0m+w3p1m/mom+w3p2m/mom/mom);
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      temc=temc-Me2p;
	      temc=temc-te2p[zsect][ysect];
	      temc=temc-(e2p0+e2p1*(TMath::Log(ecent)+e2p2)*(TMath::Log(ecent)+e2p2));
	      temc=temc-(e2p0m+e2p1m/mom+e2p2m/mom/mom);
	    }
	  if(sect==3)
	    {
	      temc=temc-Me3p;
	      temc=temc-te3p[zsect][ysect];
	      temc=temc-(e3p0+e3p1*(TMath::Log(ecent)+e3p2)*(TMath::Log(ecent)+e3p2));
	      temc=temc-(e3p0m+e3p1m/mom+e3p2m/mom/mom);
	    }
	}
    }
  
  if(alpha>0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      temc=temc-Mw0n;
	      temc=temc-tw0n[zsect][ysect];
	      temc=temc-(w0n0+w0n1*(TMath::Log(ecent)+w0n2)*(TMath::Log(ecent)+w0n2));
	      temc=temc-(w0n0m+w0n1m/mom+w0n2m/mom/mom);
	    }
	  if(sect==1)
	    {
	      temc=temc-Mw1n;
	      temc=temc-tw1n[zsect][ysect];
	      temc=temc-(w1n0+w1n1*(TMath::Log(ecent)+w1n2)*(TMath::Log(ecent)+w1n2));
	      temc=temc-(w1n0m+w1n1m/mom+w1n2m/mom/mom);
	    }
	  if(sect==2)
	    {
	      temc=temc-Mw2n;
	      temc=temc-tw2n[zsect][ysect];
	      temc=temc-(w2n0+w2n1*(TMath::Log(ecent)+w2n2)*(TMath::Log(ecent)+w2n2));
	      temc=temc-(w2n0m+w2n1m/mom+w2n2m/mom/mom);
	    }
	  if(sect==3)
	    {
	      temc=temc-Mw3n;
	      temc=temc-tw3n[zsect][ysect];
	      temc=temc-(w3n0+w3n1*(TMath::Log(ecent)+w3n2)*(TMath::Log(ecent)+w3n2));	
	      temc=temc-(w3n0m+w3n1m/mom+w3n2m/mom/mom);
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      temc=temc-Me2n;
	      temc=temc-te2n[zsect][ysect];
	      temc=temc-(e2n0+e2n1*(TMath::Log(ecent)+e2n2)*(TMath::Log(ecent)+e2n2));
	      temc=temc-(e2n0m+e2n1m/mom+e2n2m/mom/mom);
	    }
	  if(sect==3)
	    {
	      temc=temc-Me3n;
	      temc=temc-te3n[zsect][ysect];
	      temc=temc-(e3n0+e3n1*(TMath::Log(ecent)+e3n2)*(TMath::Log(ecent)+e3n2));
	      temc=temc-(e3n0m+e3n1m/mom+e3n2m/mom/mom);
	    }
	}
    }
  return temc;
}

float	  //-- this subsection of code is taken verbatim out of EMCAnalyzer.C
EmcPidrecal_dAu_Reco::IsPion(const float m2emc, const float mom, const float alpha, const int dcarm, const int sect)
{
  if (mom < 0.2 || mom > 2.4)
    {
      return -9999.9;
    }

  float Is_emc_pi = -9999.9;

  if(alpha<0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      float meanM2w0p_pi=w0mp0_pi+w0mp1_pi*mom+w0mp2_pi*mom*mom+w0mp3_pi*mom*mom*mom;
	      float sigmaM2w0p_pi=w0sp0_pi+w0sp1_pi*mom+w0sp2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w0p_pi)/sigmaM2w0p_pi;
	    }
	  if(sect==1)
	    {
	      float meanM2w1p_pi=w1mp0_pi+w1mp1_pi*mom+w1mp2_pi*mom*mom+w1mp3_pi*mom*mom*mom;
	      float sigmaM2w1p_pi=w1sp0_pi+w1sp1_pi*mom+w1sp2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w1p_pi)/sigmaM2w1p_pi;
	    }
	  if(sect==2)
	    {
	      float meanM2w2p_pi=w2mp0_pi+w2mp1_pi*mom+w2mp2_pi*mom*mom+w2mp3_pi*mom*mom*mom;
	      float sigmaM2w2p_pi=w2sp0_pi+w2sp1_pi*mom+w2sp2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w2p_pi)/sigmaM2w2p_pi;
	    }
	  if(sect==3)
	    {
	      float meanM2w3p_pi=w3mp0_pi+w3mp1_pi*mom+w3mp2_pi*mom*mom+w3mp3_pi*mom*mom*mom;
	      float sigmaM2w3p_pi=w3sp0_pi+w3sp1_pi*mom+w3sp2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w3p_pi)/sigmaM2w3p_pi;
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      float meanM2e2p_pi=e2mp0_pi+e2mp1_pi*mom+e2mp2_pi*mom*mom+e2mp3_pi*mom*mom*mom;
	      float sigmaM2e2p_pi=e2sp0_pi+e2sp1_pi*mom+e2sp2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2e2p_pi)/sigmaM2e2p_pi;
	    }
	  if(sect==3)
	    {
	      float meanM2e3p_pi=e3mp0_pi+e3mp1_pi*mom+e3mp2_pi*mom*mom+e3mp3_pi*mom*mom*mom;
	      float sigmaM2e3p_pi=e3sp0_pi+e3sp1_pi*mom+e3sp2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2e3p_pi)/sigmaM2e3p_pi;
	    }
	}
    }
  
  if(alpha>0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      float meanM2w0n_pi=w0mn0_pi+w0mn1_pi*mom+w0mn2_pi*mom*mom+w0mn3_pi*mom*mom*mom;
	      float sigmaM2w0n_pi=w0sn0_pi+w0sn1_pi*mom+w0sn2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w0n_pi)/sigmaM2w0n_pi;
	    }
	  if(sect==1)
	    {
	      float meanM2w1n_pi=w1mn0_pi+w1mn1_pi*mom+w1mn2_pi*mom*mom+w1mn3_pi*mom*mom*mom;
	      float sigmaM2w1n_pi=w1sn0_pi+w1sn1_pi*mom+w1sn2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w1n_pi)/sigmaM2w1n_pi;
	    }
	  if(sect==2)
	    {
	      float meanM2w2n_pi=w2mn0_pi+w2mn1_pi*mom+w2mn2_pi*mom*mom+w2mn3_pi*mom*mom*mom;
	      float sigmaM2w2n_pi=w2sn0_pi+w2sn1_pi*mom+w2sn2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w2n_pi)/sigmaM2w2n_pi;
	    }
	  if(sect==3)
	    {
	      float meanM2w3n_pi=w3mn0_pi+w3mn1_pi*mom+w3mn2_pi*mom*mom+w3mn3_pi*mom*mom*mom;
	      float sigmaM2w3n_pi=w3sn0_pi+w3sn1_pi*mom+w3sn2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2w3n_pi)/sigmaM2w3n_pi;
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      float meanM2e2n_pi=e2mn0_pi+e2mn1_pi*mom+e2mn2_pi*mom*mom+e2mn3_pi*mom*mom*mom;
	      float sigmaM2e2n_pi=e2sn0_pi+e2sn1_pi*mom+e2sn2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2e2n_pi)/sigmaM2e2n_pi;
	    }
	  if(sect==3)
	    {
	      float meanM2e3n_pi=e3mn0_pi+e3mn1_pi*mom+e3mn2_pi*mom*mom+e3mn3_pi*mom*mom*mom;
	      float sigmaM2e3n_pi=e3sn0_pi+e3sn1_pi*mom+e3sn2_pi*mom*mom;  
	      Is_emc_pi=(m2emc-meanM2e3n_pi)/sigmaM2e3n_pi;
	    }
	}
    }
  return Is_emc_pi;
}

float	  //-- this subsection of code is taken verbatim out of EMCAnalyzer.C
EmcPidrecal_dAu_Reco::IsKaon(const float m2emc, const float mom, const float alpha, const int dcarm, const int sect)
{
  if (mom < 0.4 || mom > 1.0 )
    {
      return -9999.9;
    }

  float Is_emc_K = -9999.9;

  if(alpha<0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      float meanM2w0p_K=w0mp0_K+w0mp1_K*mom+w0mp2_K*mom*mom;
	      float sigmaM2w0p_K=w0sp0_K+w0sp1_K*mom+w0sp2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w0p_K)/sigmaM2w0p_K;
	    }
	  if(sect==1)
	    {
	      float meanM2w1p_K=w1mp0_K+w1mp1_K*mom+w1mp2_K*mom*mom;
	      float sigmaM2w1p_K=w1sp0_K+w1sp1_K*mom+w1sp2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w1p_K)/sigmaM2w1p_K;
	    }
	  if(sect==2)
	    {
	      float meanM2w2p_K=w2mp0_K+w2mp1_K*mom+w2mp2_K*mom*mom;
	      float sigmaM2w2p_K=w2sp0_K+w2sp1_K*mom+w2sp2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w2p_K)/sigmaM2w2p_K;
	    }
	  if(sect==3)
	    {
	      float meanM2w3p_K=w3mp0_K+w3mp1_K*mom+w3mp2_K*mom*mom;
	      float sigmaM2w3p_K=w3sp0_K+w3sp1_K*mom+w3sp2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w3p_K)/sigmaM2w3p_K;
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      float meanM2e2p_K=e2mp0_K+e2mp1_K*mom+e2mp2_K*mom*mom;
	      float sigmaM2e2p_K=e2sp0_K+e2sp1_K*mom+e2sp2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2e2p_K)/sigmaM2e2p_K;
	    }
	  if(sect==3)
	    {
	      float meanM2e3p_K=e3mp0_K+e3mp1_K*mom+e3mp2_K*mom*mom;
	      float sigmaM2e3p_K=e3sp0_K+e3sp1_K*mom+e3sp2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2e3p_K)/sigmaM2e3p_K;
	    }
	}
    }
  
  if(alpha>0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      float meanM2w0n_K=w0mn0_K+w0mn1_K*mom+w0mn2_K*mom*mom;
	      float sigmaM2w0n_K=w0sn0_K+w0sn1_K*mom+w0sn2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w0n_K)/sigmaM2w0n_K;
	    }
	  if(sect==1)
	    {
	      float meanM2w1n_K=w1mn0_K+w1mn1_K*mom+w1mn2_K*mom*mom;
	      float sigmaM2w1n_K=w1sn0_K+w1sn1_K*mom+w1sn2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w1n_K)/sigmaM2w1n_K;
	    }
	  if(sect==2)
	    {
	      float meanM2w2n_K=w2mn0_K+w2mn1_K*mom+w2mn2_K*mom*mom;
	      float sigmaM2w2n_K=w2sn0_K+w2sn1_K*mom+w2sn2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w2n_K)/sigmaM2w2n_K;
	    }
	  if(sect==3)
	    {
	      float meanM2w3n_K=w3mn0_K+w3mn1_K*mom+w3mn2_K*mom*mom;
	      float sigmaM2w3n_K=w3sn0_K+w3sn1_K*mom+w3sn2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2w3n_K)/sigmaM2w3n_K;
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      float meanM2e2n_K=e2mn0_K+e2mn1_K*mom+e2mn2_K*mom*mom;
	      float sigmaM2e2n_K=e2sn0_K+e2sn1_K*mom+e2sn2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2e2n_K)/sigmaM2e2n_K;
	    }
	  if(sect==3)
	    {
	      float meanM2e3n_K=e3mn0_K+e3mn1_K*mom+e3mn2_K*mom*mom;
	      float sigmaM2e3n_K=e3sn0_K+e3sn1_K*mom+e3sn2_K*mom*mom;  
	      Is_emc_K=(m2emc-meanM2e3n_K)/sigmaM2e3n_K;
	    }
	}
    }
  
  return Is_emc_K;
}

float	  //-- this subsection of code is taken verbatim out of EMCAnalyzer.C
EmcPidrecal_dAu_Reco::IsProton(const float m2emc, const float mom, const float alpha, const int dcarm, const int sect)
{
  if (mom < 0.5 || mom > 2.1)
    {
      return -9999.9;
    }
  float Is_emc_p = 9999.9;

  if(alpha<0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      float meanM2w0p_p=w0mp0_p+w0mp1_p*mom+w0mp2_p*mom*mom;
	      float sigmaM2w0p_p=w0sp0_p+w0sp1_p*mom+w0sp2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w0p_p)/sigmaM2w0p_p;
	    }
	  if(sect==1)
	    {
	      float meanM2w1p_p=w1mp0_p+w1mp1_p*mom+w1mp2_p*mom*mom;
	      float sigmaM2w1p_p=w1sp0_p+w1sp1_p*mom+w1sp2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w1p_p)/sigmaM2w1p_p;
	    }
	  if(sect==2)
	    {
	      float meanM2w2p_p=w2mp0_p+w2mp1_p*mom+w2mp2_p*mom*mom;
	      float sigmaM2w2p_p=w2sp0_p+w2sp1_p*mom+w2sp2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w2p_p)/sigmaM2w2p_p;
	    }
	  if(sect==3)
	    {
	      float meanM2w3p_p=w3mp0_p+w3mp1_p*mom+w3mp2_p*mom*mom;
	      float sigmaM2w3p_p=w3sp0_p+w3sp1_p*mom+w3sp2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w3p_p)/sigmaM2w3p_p;
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      float meanM2e2p_p=e2mp0_p+e2mp1_p*mom+e2mp2_p*mom*mom;
	      float sigmaM2e2p_p=e2sp0_p+e2sp1_p*mom+e2sp2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2e2p_p)/sigmaM2e2p_p;
	    }
	  if(sect==3)
	    {
	      float meanM2e3p_p=e3mp0_p+e3mp1_p*mom+e3mp2_p*mom*mom;
	      float sigmaM2e3p_p=e3sp0_p+e3sp1_p*mom+e3sp2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2e3p_p)/sigmaM2e3p_p;
	    }
	}
    }
  
  if(alpha>0)
    {
      if(dcarm==1)
	{
	  if(sect==0)
	    {
	      float meanM2w0n_p=w0mn0_p+w0mn1_p*mom+w0mn2_p*mom*mom;
	      float sigmaM2w0n_p=w0sn0_p+w0sn1_p*mom+w0sn2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w0n_p)/sigmaM2w0n_p;
	    }
	  if(sect==1)
	    {
	      float meanM2w1n_p=w1mn0_p+w1mn1_p*mom+w1mn2_p*mom*mom;
	      float sigmaM2w1n_p=w1sn0_p+w1sn1_p*mom+w1sn2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w1n_p)/sigmaM2w1n_p;
	    }
	  if(sect==2)
	    {
	      float meanM2w2n_p=w2mn0_p+w2mn1_p*mom+w2mn2_p*mom*mom;
	      float sigmaM2w2n_p=w2sn0_p+w2sn1_p*mom+w2sn2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w2n_p)/sigmaM2w2n_p;
	    }
	  if(sect==3)
	    {
	      float meanM2w3n_p=w3mn0_p+w3mn1_p*mom+w3mn2_p*mom*mom;
	      float sigmaM2w3n_p=w3sn0_p+w3sn1_p*mom+w3sn2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2w3n_p)/sigmaM2w3n_p;
	    }
	}
      if(dcarm==0)
	{
	  if(sect==2)
	    {
	      float meanM2e2n_p=e2mn0_p+e2mn1_p*mom+e2mn2_p*mom*mom;
	      float sigmaM2e2n_p=e2sn0_p+e2sn1_p*mom+e2sn2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2e2n_p)/sigmaM2e2n_p;
	    }
	  if(sect==3)
	    {
	      float meanM2e3n_p=e3mn0_p+e3mn1_p*mom+e3mn2_p*mom*mom;
	      float sigmaM2e3n_p=e3sn0_p+e3sn1_p*mom+e3sn2_p*mom*mom;  
	      Is_emc_p=(m2emc-meanM2e3n_p)/sigmaM2e3n_p;
	    }
	}
    }

  return Is_emc_p;
}

void
EmcPidrecal_dAu_Reco::SetCalibConsts()
{
  // set values in arrays

  // --bad or dead tower flags
  // from /offline/analysis/EMC_PbSc_tof_run3dAu/code/mask_w0_run3dAu.txt etc for w1, w2, w3, e2, e3
  // also at /phenix/workarea/kotchet/EMC_PbSc_tof_run3dAu/code
  for (int m=1; m<=72; m++)
    {
      for (int mm=1; mm<=36; mm++)
	{
	  flagw0[m-1][mm-1]=0;
	  flagw1[m-1][mm-1]=0;
	  flagw2[m-1][mm-1]=0;
	  flagw3[m-1][mm-1]=0;
	  flage2[m-1][mm-1]=0;
	  flage3[m-1][mm-1]=0;	  
	  if (m==18 && mm==25) { flagw0[m-1][mm-1]=1; }
	  if (m==9  && mm==26) { flagw1[m-1][mm-1]=1; }
	  if (m==23 && mm==2 ) { flagw1[m-1][mm-1]=1; }
	  if (m==25 && mm==14) { flagw1[m-1][mm-1]=1; }
	  if (m==45 && mm==2 ) { flagw1[m-1][mm-1]=1; }
	  if (m==15 && mm==15) { flagw2[m-1][mm-1]=1; }
	  if (m==15 && mm==16) { flagw2[m-1][mm-1]=1; }
	  if (m==15 && mm==17) { flagw2[m-1][mm-1]=1; }
	  if (m==15 && mm==18) { flagw2[m-1][mm-1]=1; }
	  if (m==16 && mm==15) { flagw2[m-1][mm-1]=1; }
	  if (m==16 && mm==16) { flagw2[m-1][mm-1]=1; }
	  if (m==16 && mm==17) { flagw2[m-1][mm-1]=1; }
	  if (m==16 && mm==18) { flagw2[m-1][mm-1]=1; }
	  if (m==63 && mm==32) { flagw2[m-1][mm-1]=1; }
	  if (m==10 && mm==13) { flagw3[m-1][mm-1]=1; }
	  if (m==47 && mm==32) { flagw3[m-1][mm-1]=1; }
	  if (m==7  && mm==1 ) { flage2[m-1][mm-1]=1; }
	  if (m==7  && mm==2 ) { flage2[m-1][mm-1]=1; }
	  if (m==7  && mm==3 ) { flage2[m-1][mm-1]=1; }
	  if (m==7  && mm==4 ) { flage2[m-1][mm-1]=1; }
	  if (m==7  && mm==5 ) { flage2[m-1][mm-1]=1; }
	  if (m==7  && mm==6 ) { flage2[m-1][mm-1]=1; }
	  if (m==8  && mm==1 ) { flage2[m-1][mm-1]=1; }
	  if (m==8  && mm==2 ) { flage2[m-1][mm-1]=1; }
	  if (m==8  && mm==3 ) { flage2[m-1][mm-1]=1; }
	  if (m==8  && mm==4 ) { flage2[m-1][mm-1]=1; }
	  if (m==8  && mm==5 ) { flage2[m-1][mm-1]=1; }
	  if (m==8  && mm==6 ) { flage2[m-1][mm-1]=1; }
	  if (m==53 && mm==25) { flage2[m-1][mm-1]=1; }
	  if (m==53 && mm==27) { flage2[m-1][mm-1]=1; }
	  if (m==61 && mm==19) { flage2[m-1][mm-1]=1; }
	}
    }

  // tower corrections from tower_corr_run3dAu.txt --same directories  
  for (int m=0; m<72; m++)
    {
      for (int mm=0; mm<36; mm++)
	{
	  tw0p[m][mm]=0;
	  tw1p[m][mm]=0;
	  tw2p[m][mm]=0;
	  tw3p[m][mm]=0;
	  te2p[m][mm]=0;
	  te3p[m][mm]=0;
	  tw0n[m][mm]=0;
	  tw1n[m][mm]=0;
	  tw2n[m][mm]=0;
	  tw3n[m][mm]=0;
	  te2n[m][mm]=0;
	  te3n[m][mm]=0;

	  if(m==0 && mm == 14) {     tw0p[m][mm]= 0.482809; } 
	  if(m==0 && mm == 21) {     tw0p[m][mm]= 0.395404; } 
	  if(m==0 && mm == 34) {     tw0p[m][mm]= 0.733972; } 
	  if(m==0 && mm == 2) {     tw1p[m][mm]= 1.132137; } 
	  if(m==0 && mm == 3) {     tw1p[m][mm]= 1.051034; } 
	  if(m==0 && mm == 5) {     tw1p[m][mm]= 0.344736; } 
	  if(m==0 && mm == 30) {     tw1p[m][mm]= 0.286289; } 
	  if(m==0 && mm == 9) {     tw3p[m][mm]= 0.351444; } 
	  if(m==0 && mm == 5) {     te2p[m][mm]= 1.606171; } 
	  if(m==0 && mm == 25) {     te2p[m][mm]= 1.613243; } 
	  if(m==0 && mm == 21) {     tw0n[m][mm]= 0.319908; } 
	  if(m==0 && mm == 34) {     tw1n[m][mm]= 0.213229; } 
	  if(m==0 && mm == 1) {     tw2n[m][mm]= 0.039880; } 
	  if(m==0 && mm == 1) {     tw3n[m][mm]= 0.849563; } 
	  if(m==0 && mm == 1) {     te3n[m][mm]= 0.593073; } 
	  if(m==1 && mm == 9) {     tw0p[m][mm]= 0.718667; } 
	  if(m==1 && mm == 10) {     tw0p[m][mm]= 0.615089; } 
	  if(m==1 && mm == 11) {     tw0p[m][mm]= 0.853700; } 
	  if(m==1 && mm == 12) {     tw0p[m][mm]= 0.235932; } 
	  if(m==1 && mm == 13) {     tw0p[m][mm]= 0.613926; } 
	  if(m==1 && mm == 15) {     tw0p[m][mm]= 0.613816; } 
	  if(m==1 && mm == 16) {     tw0p[m][mm]= 0.883554; } 
	  if(m==1 && mm == 17) {     tw0p[m][mm]= 0.537409; } 
	  if(m==1 && mm == 18) {     tw0p[m][mm]= 1.204986; } 
	  if(m==1 && mm == 19) {     tw0p[m][mm]= 0.980687; } 
	  if(m==1 && mm == 20) {     tw0p[m][mm]= 0.729175; } 
	  if(m==1 && mm == 21) {     tw0p[m][mm]= 0.366857; } 
	  if(m==1 && mm == 22) {     tw0p[m][mm]= 0.281474; } 
	  if(m==1 && mm == 23) {     tw0p[m][mm]= 0.651902; } 
	  if(m==1 && mm == 28) {     tw0p[m][mm]= 0.515931; } 
	  if(m==1 && mm == 29) {     tw0p[m][mm]= 0.456580; } 
	  if(m==1 && mm == 30) {     tw0p[m][mm]= 0.406457; } 
	  if(m==1 && mm == 32) {     tw0p[m][mm]= 0.775679; } 
	  if(m==1 && mm == 33) {     tw0p[m][mm]= 0.859812; } 
	  if(m==1 && mm == 34) {     tw0p[m][mm]= 0.761708; } 
	  if(m==1 && mm == 0) {     tw1p[m][mm]= 1.327626; } 
	  if(m==1 && mm == 2) {     tw1p[m][mm]= 0.946203; } 
	  if(m==1 && mm == 3) {     tw1p[m][mm]= 0.493173; } 
	  if(m==1 && mm == 28) {     tw1p[m][mm]= 1.031616; } 
	  if(m==1 && mm == 29) {     tw1p[m][mm]= 2.261679; } 
	  if(m==1 && mm == 32) {     tw1p[m][mm]= 0.268416; } 
	  if(m==1 && mm == 33) {     tw1p[m][mm]= 0.585866; } 
	  if(m==1 && mm == 35) {     tw1p[m][mm]= 0.448144; } 
	  if(m==1 && mm == 9) {     tw2p[m][mm]= 0.280942; } 
	  if(m==1 && mm == 10) {     tw2p[m][mm]= 0.065150; } 
	  if(m==1 && mm == 11) {     tw2p[m][mm]= 0.217144; } 
	  if(m==1 && mm == 12) {     tw2p[m][mm]= 0.576106; } 
	  if(m==1 && mm == 13) {     tw2p[m][mm]= 0.696244; } 
	  if(m==1 && mm == 14) {     tw2p[m][mm]= 1.275372; } 
	  if(m==1 && mm == 17) {     tw2p[m][mm]= 1.239219; } 
	  if(m==1 && mm == 18) {     tw2p[m][mm]= 0.274888; } 
	  if(m==1 && mm == 1) {     tw3p[m][mm]= 1.489714; } 
	  if(m==1 && mm == 3) {     tw3p[m][mm]= 1.270472; } 
	  if(m==1 && mm == 5) {     tw3p[m][mm]= 1.500037; } 
	  if(m==1 && mm == 10) {     tw3p[m][mm]= 1.229211; } 
	  if(m==1 && mm == 11) {     tw3p[m][mm]= 0.765751; } 
	  if(m==1 && mm == 15) {     tw3p[m][mm]= 2.288463; } 
	  if(m==1 && mm == 18) {     tw3p[m][mm]= 0.068823; } 
	  if(m==1 && mm == 20) {     tw3p[m][mm]= 1.577475; } 
	  if(m==1 && mm == 21) {     tw3p[m][mm]= 2.166632; } 
	  if(m==1 && mm == 22) {     tw3p[m][mm]= 1.028179; } 
	  if(m==1 && mm == 23) {     tw3p[m][mm]= 1.166752; } 
	  if(m==1 && mm == 25) {     tw3p[m][mm]= 0.338426; } 
	  if(m==1 && mm == 1) {     te2p[m][mm]= 0.668202; } 
	  if(m==1 && mm == 2) {     te2p[m][mm]= 0.638834; } 
	  if(m==1 && mm == 3) {     te2p[m][mm]= 0.632745; } 
	  if(m==1 && mm == 4) {     te2p[m][mm]= 2.291938; } 
	  if(m==1 && mm == 10) {     te2p[m][mm]= 0.107892; } 
	  if(m==1 && mm == 11) {     te2p[m][mm]= 1.140893; } 
	  if(m==1 && mm == 15) {     te2p[m][mm]= 1.401139; } 
	  if(m==1 && mm == 18) {     te2p[m][mm]= 1.542144; } 
	  if(m==1 && mm == 26) {     te2p[m][mm]= 1.207230; } 
	  if(m==1 && mm == 6) {     te3p[m][mm]= 0.529003; } 
	  if(m==1 && mm == 7) {     te3p[m][mm]= 0.354669; } 
	  if(m==1 && mm == 8) {     te3p[m][mm]= 0.450714; } 
	  if(m==1 && mm == 9) {     te3p[m][mm]= 1.586355; } 
	  if(m==1 && mm == 10) {     te3p[m][mm]= 0.375732; } 
	  if(m==1 && mm == 11) {     te3p[m][mm]= 0.223199; } 
	  if(m==1 && mm == 13) {     te3p[m][mm]= 1.001024; } 
	  if(m==1 && mm == 17) {     te3p[m][mm]= 1.914601; } 
	  if(m==1 && mm == 20) {     te3p[m][mm]= 0.332637; } 
	  if(m==1 && mm == 21) {     te3p[m][mm]= 0.917912; } 
	  if(m==1 && mm == 2) {     tw0n[m][mm]= 0.035448; } 
	  if(m==1 && mm == 8) {     tw0n[m][mm]= 0.032582; } 
	  if(m==1 && mm == 9) {     tw0n[m][mm]= 0.378073; } 
	  if(m==1 && mm == 15) {     tw0n[m][mm]= 0.477344; } 
	  if(m==1 && mm == 17) {     tw0n[m][mm]= 0.448431; } 
	  if(m==1 && mm == 19) {     tw0n[m][mm]= 0.640844; } 
	  if(m==1 && mm == 20) {     tw0n[m][mm]= 0.334382; } 
	  if(m==1 && mm == 21) {     tw0n[m][mm]= 0.421087; } 
	  if(m==1 && mm == 22) {     tw0n[m][mm]= 0.185278; } 
	  if(m==1 && mm == 23) {     tw0n[m][mm]= 0.653858; } 
	  if(m==1 && mm == 25) {     tw0n[m][mm]= 0.328346; } 
	  if(m==1 && mm == 26) {     tw0n[m][mm]= 0.671936; } 
	  if(m==1 && mm == 10) {     tw1n[m][mm]= 0.364614; } 
	  if(m==1 && mm == 12) {     tw1n[m][mm]= 0.927503; } 
	  if(m==1 && mm == 13) {     tw1n[m][mm]= 0.643507; } 
	  if(m==1 && mm == 15) {     tw1n[m][mm]= 0.284213; } 
	  if(m==1 && mm == 18) {     tw1n[m][mm]= 0.417038; } 
	  if(m==1 && mm == 19) {     tw1n[m][mm]= 0.182473; } 
	  if(m==1 && mm == 21) {     tw1n[m][mm]= 0.784954; } 
	  if(m==1 && mm == 32) {     tw1n[m][mm]= 0.396470; } 
	  if(m==1 && mm == 33) {     tw1n[m][mm]= 0.100603; } 
	  if(m==1 && mm == 0) {     tw2n[m][mm]= 0.211948; } 
	  if(m==1 && mm == 1) {     tw2n[m][mm]= 0.155969; } 
	  if(m==1 && mm == 3) {     tw2n[m][mm]= 0.215660; } 
	  if(m==1 && mm == 4) {     tw2n[m][mm]= 0.363422; } 
	  if(m==1 && mm == 15) {     tw2n[m][mm]= 1.365353; } 
	  if(m==1 && mm == 18) {     tw2n[m][mm]= 0.373892; } 
	  if(m==1 && mm == 20) {     tw2n[m][mm]= 1.273493; } 
	  if(m==1 && mm == 21) {     tw2n[m][mm]= 0.372000; } 
	  if(m==1 && mm == 23) {     tw2n[m][mm]= 0.478117; } 
	  if(m==1 && mm == 5) {     tw3n[m][mm]= 1.406342; } 
	  if(m==1 && mm == 11) {     tw3n[m][mm]= 0.058005; } 
	  if(m==1 && mm == 15) {     tw3n[m][mm]= 1.512184; } 
	  if(m==1 && mm == 18) {     tw3n[m][mm]= 0.005760; } 
	  if(m==1 && mm == 20) {     tw3n[m][mm]= 3.019031; } 
	  if(m==1 && mm == 22) {     tw3n[m][mm]= 1.360677; } 
	  if(m==1 && mm == 23) {     tw3n[m][mm]= 0.826428; } 
	  if(m==1 && mm == 9) {     te2n[m][mm]= 3.259295; } 
	  if(m==1 && mm == 10) {     te2n[m][mm]= 0.160545; } 
	  if(m==1 && mm == 11) {     te2n[m][mm]= 0.110336; } 
	  if(m==1 && mm == 12) {     te2n[m][mm]= 0.923907; } 
	  if(m==1 && mm == 15) {     te2n[m][mm]= 1.424928; } 
	  if(m==1 && mm == 17) {     te2n[m][mm]= 1.757379; } 
	  if(m==1 && mm == 18) {     te2n[m][mm]= 1.757647; } 
	  if(m==1 && mm == 19) {     te2n[m][mm]= 0.930862; } 
	  if(m==1 && mm == 26) {     te2n[m][mm]= 0.095744; } 
	  if(m==1 && mm == 29) {     te2n[m][mm]= 0.810980; } 
	  if(m==1 && mm == 30) {     te2n[m][mm]= 0.967790; } 
	  if(m==1 && mm == 32) {     te2n[m][mm]= 0.476760; } 
	  if(m==1 && mm == 1) {     te3n[m][mm]= 0.086252; } 
	  if(m==1 && mm == 3) {     te3n[m][mm]= 0.509616; } 
	  if(m==1 && mm == 20) {     te3n[m][mm]= 0.357789; } 
	  if(m==1 && mm == 21) {     te3n[m][mm]= 1.346138; } 
	  if(m==1 && mm == 24) {     te3n[m][mm]= 0.490290; } 
	  if(m==1 && mm == 25) {     te3n[m][mm]= 1.025144; } 
	  if(m==1 && mm == 27) {     te3n[m][mm]= 0.611858; } 
	  if(m==1 && mm == 28) {     te3n[m][mm]= 0.761546; } 
	  if(m==1 && mm == 29) {     te3n[m][mm]= 2.637857; } 
	  if(m==1 && mm == 30) {     te3n[m][mm]= 0.815941; } 
	  if(m==1 && mm == 32) {     te3n[m][mm]= 0.244988; } 
	  if(m==2 && mm == 12) {     tw0p[m][mm]= 0.139994; } 
	  if(m==2 && mm == 13) {     tw0p[m][mm]= 1.230425; } 
	  if(m==2 && mm == 14) {     tw0p[m][mm]= 0.257154; } 
	  if(m==2 && mm == 17) {     tw0p[m][mm]= 0.134688; } 
	  if(m==2 && mm == 18) {     tw0p[m][mm]= 0.070763; } 
	  if(m==2 && mm == 19) {     tw0p[m][mm]= 0.796585; } 
	  if(m==2 && mm == 20) {     tw0p[m][mm]= 0.370723; } 
	  if(m==2 && mm == 21) {     tw0p[m][mm]= 0.446196; } 
	  if(m==2 && mm == 22) {     tw0p[m][mm]= 0.477124; } 
	  if(m==2 && mm == 23) {     tw0p[m][mm]= 0.176475; } 
	  if(m==2 && mm == 27) {     tw0p[m][mm]= 0.539994; } 
	  if(m==2 && mm == 30) {     tw0p[m][mm]= 0.183601; } 
	  if(m==2 && mm == 31) {     tw0p[m][mm]= 0.653793; } 
	  if(m==2 && mm == 32) {     tw0p[m][mm]= 0.243665; } 
	  if(m==2 && mm == 33) {     tw0p[m][mm]= 0.483944; } 
	  if(m==2 && mm == 34) {     tw0p[m][mm]= 0.476583; } 
	  if(m==2 && mm == 0) {     tw1p[m][mm]= 0.195829; } 
	  if(m==2 && mm == 1) {     tw1p[m][mm]= 2.062199; } 
	  if(m==2 && mm == 3) {     tw1p[m][mm]= 0.461715; } 
	  if(m==2 && mm == 5) {     tw1p[m][mm]= 0.513209; } 
	  if(m==2 && mm == 22) {     tw1p[m][mm]= 2.968923; } 
	  if(m==2 && mm == 23) {     tw1p[m][mm]= 1.130483; } 
	  if(m==2 && mm == 28) {     tw1p[m][mm]= 0.774303; } 
	  if(m==2 && mm == 29) {     tw1p[m][mm]= 0.515990; } 
	  if(m==2 && mm == 31) {     tw1p[m][mm]= 0.125295; } 
	  if(m==2 && mm == 32) {     tw1p[m][mm]= 0.159607; } 
	  if(m==2 && mm == 33) {     tw1p[m][mm]= 0.342323; } 
	  if(m==2 && mm == 35) {     tw1p[m][mm]= 0.226139; } 
	  if(m==2 && mm == 10) {     tw2p[m][mm]= 0.111109; } 
	  if(m==2 && mm == 12) {     tw2p[m][mm]= 0.864952; } 
	  if(m==2 && mm == 13) {     tw2p[m][mm]= 0.708233; } 
	  if(m==2 && mm == 15) {     tw2p[m][mm]= 0.472807; } 
	  if(m==2 && mm == 16) {     tw2p[m][mm]= 1.066411; } 
	  if(m==2 && mm == 17) {     tw2p[m][mm]= 0.250923; } 
	  if(m==2 && mm == 19) {     tw2p[m][mm]= 1.760787; } 
	  if(m==2 && mm == 20) {     tw2p[m][mm]= 0.208888; } 
	  if(m==2 && mm == 29) {     tw2p[m][mm]= 1.673936; } 
	  if(m==2 && mm == 30) {     tw2p[m][mm]= 0.248940; } 
	  if(m==2 && mm == 31) {     tw2p[m][mm]= 0.406685; } 
	  if(m==2 && mm == 32) {     tw2p[m][mm]= 1.772856; } 
	  if(m==2 && mm == 33) {     tw2p[m][mm]= 0.803222; } 
	  if(m==2 && mm == 34) {     tw2p[m][mm]= 1.347709; } 
	  if(m==2 && mm == 35) {     tw2p[m][mm]= 2.491830; } 
	  if(m==2 && mm == 0) {     tw3p[m][mm]= 0.535712; } 
	  if(m==2 && mm == 1) {     tw3p[m][mm]= 0.878899; } 
	  if(m==2 && mm == 2) {     tw3p[m][mm]= 0.965210; } 
	  if(m==2 && mm == 3) {     tw3p[m][mm]= 0.973482; } 
	  if(m==2 && mm == 4) {     tw3p[m][mm]= 0.545009; } 
	  if(m==2 && mm == 5) {     tw3p[m][mm]= 0.842182; } 
	  if(m==2 && mm == 9) {     tw3p[m][mm]= 1.009903; } 
	  if(m==2 && mm == 10) {     tw3p[m][mm]= 1.782325; } 
	  if(m==2 && mm == 11) {     tw3p[m][mm]= 1.556978; } 
	  if(m==2 && mm == 12) {     tw3p[m][mm]= 0.612962; } 
	  if(m==2 && mm == 17) {     tw3p[m][mm]= 0.143166; } 
	  if(m==2 && mm == 18) {     tw3p[m][mm]= 0.633935; } 
	  if(m==2 && mm == 21) {     tw3p[m][mm]= 2.022054; } 
	  if(m==2 && mm == 27) {     tw3p[m][mm]= 0.976218; } 
	  if(m==2 && mm == 28) {     tw3p[m][mm]= 0.942890; } 
	  if(m==2 && mm == 33) {     tw3p[m][mm]= 0.287987; } 
	  if(m==2 && mm == 0) {     te2p[m][mm]= 0.233019; } 
	  if(m==2 && mm == 1) {     te2p[m][mm]= 0.283356; } 
	  if(m==2 && mm == 2) {     te2p[m][mm]= 1.433760; } 
	  if(m==2 && mm == 3) {     te2p[m][mm]= 0.786943; } 
	  if(m==2 && mm == 4) {     te2p[m][mm]= 0.158346; } 
	  if(m==2 && mm == 5) {     te2p[m][mm]= 0.211659; } 
	  if(m==2 && mm == 8) {     te2p[m][mm]= 1.994225; } 
	  if(m==2 && mm == 9) {     te2p[m][mm]= 1.052122; } 
	  if(m==2 && mm == 11) {     te2p[m][mm]= 2.453866; } 
	  if(m==2 && mm == 13) {     te2p[m][mm]= 0.411133; } 
	  if(m==2 && mm == 15) {     te2p[m][mm]= 0.490647; } 
	  if(m==2 && mm == 17) {     te2p[m][mm]= 0.126387; } 
	  if(m==2 && mm == 18) {     te2p[m][mm]= 0.535551; } 
	  if(m==2 && mm == 19) {     te2p[m][mm]= 2.034041; } 
	  if(m==2 && mm == 20) {     te2p[m][mm]= 0.249556; } 
	  if(m==2 && mm == 21) {     te2p[m][mm]= 0.360983; } 
	  if(m==2 && mm == 22) {     te2p[m][mm]= 0.292864; } 
	  if(m==2 && mm == 23) {     te2p[m][mm]= 1.211411; } 
	  if(m==2 && mm == 24) {     te2p[m][mm]= 0.089562; } 
	  if(m==2 && mm == 25) {     te2p[m][mm]= 0.348511; } 
	  if(m==2 && mm == 26) {     te2p[m][mm]= 1.336338; } 
	  if(m==2 && mm == 28) {     te2p[m][mm]= 0.551131; } 
	  if(m==2 && mm == 7) {     te3p[m][mm]= 1.160804; } 
	  if(m==2 && mm == 8) {     te3p[m][mm]= 0.434736; } 
	  if(m==2 && mm == 9) {     te3p[m][mm]= 0.672529; } 
	  if(m==2 && mm == 10) {     te3p[m][mm]= 0.473564; } 
	  if(m==2 && mm == 12) {     te3p[m][mm]= 1.816320; } 
	  if(m==2 && mm == 13) {     te3p[m][mm]= 0.777395; } 
	  if(m==2 && mm == 14) {     te3p[m][mm]= 1.130558; } 
	  if(m==2 && mm == 15) {     te3p[m][mm]= 3.081740; } 
	  if(m==2 && mm == 17) {     te3p[m][mm]= 0.345992; } 
	  if(m==2 && mm == 18) {     te3p[m][mm]= 2.689839; } 
	  if(m==2 && mm == 19) {     te3p[m][mm]= 2.005443; } 
	  if(m==2 && mm == 20) {     te3p[m][mm]= 1.145504; } 
	  if(m==2 && mm == 21) {     te3p[m][mm]= 0.089403; } 
	  if(m==2 && mm == 1) {     tw0n[m][mm]= 0.942294; } 
	  if(m==2 && mm == 4) {     tw0n[m][mm]= 0.171757; } 
	  if(m==2 && mm == 15) {     tw0n[m][mm]= 0.447098; } 
	  if(m==2 && mm == 17) {     tw0n[m][mm]= 0.455491; } 
	  if(m==2 && mm == 20) {     tw0n[m][mm]= 0.238147; } 
	  if(m==2 && mm == 22) {     tw0n[m][mm]= 0.558042; } 
	  if(m==2 && mm == 27) {     tw0n[m][mm]= 0.238606; } 
	  if(m==2 && mm == 10) {     tw1n[m][mm]= 0.022421; } 
	  if(m==2 && mm == 11) {     tw1n[m][mm]= 0.593706; } 
	  if(m==2 && mm == 15) {     tw1n[m][mm]= 1.027192; } 
	  if(m==2 && mm == 17) {     tw1n[m][mm]= 0.580898; } 
	  if(m==2 && mm == 18) {     tw1n[m][mm]= 0.596533; } 
	  if(m==2 && mm == 19) {     tw1n[m][mm]= 0.624391; } 
	  if(m==2 && mm == 20) {     tw1n[m][mm]= 1.564640; } 
	  if(m==2 && mm == 33) {     tw1n[m][mm]= 0.625452; } 
	  if(m==2 && mm == 34) {     tw1n[m][mm]= 0.939773; } 
	  if(m==2 && mm == 0) {     tw2n[m][mm]= 0.726904; } 
	  if(m==2 && mm == 1) {     tw2n[m][mm]= 0.065868; } 
	  if(m==2 && mm == 2) {     tw2n[m][mm]= 0.495782; } 
	  if(m==2 && mm == 17) {     tw2n[m][mm]= 0.441980; } 
	  if(m==2 && mm == 19) {     tw2n[m][mm]= 1.370105; } 
	  if(m==2 && mm == 20) {     tw2n[m][mm]= 0.122250; } 
	  if(m==2 && mm == 21) {     tw2n[m][mm]= 0.019068; } 
	  if(m==2 && mm == 22) {     tw2n[m][mm]= 1.601271; } 
	  if(m==2 && mm == 24) {     tw2n[m][mm]= 0.099026; } 
	  if(m==2 && mm == 25) {     tw2n[m][mm]= 0.732960; } 
	  if(m==2 && mm == 29) {     tw2n[m][mm]= 1.390914; } 
	  if(m==2 && mm == 31) {     tw2n[m][mm]= 0.022756; } 
	  if(m==2 && mm == 32) {     tw2n[m][mm]= 1.021309; } 
	  if(m==2 && mm == 33) {     tw2n[m][mm]= 1.007303; } 
	  if(m==2 && mm == 34) {     tw2n[m][mm]= 0.867751; } 
	  if(m==2 && mm == 0) {     tw3n[m][mm]= 0.471109; } 
	  if(m==2 && mm == 1) {     tw3n[m][mm]= 1.429802; } 
	  if(m==2 && mm == 2) {     tw3n[m][mm]= 1.893544; } 
	  if(m==2 && mm == 3) {     tw3n[m][mm]= 0.416515; } 
	  if(m==2 && mm == 5) {     tw3n[m][mm]= 0.968050; } 
	  if(m==2 && mm == 7) {     tw3n[m][mm]= 1.426782; } 
	  if(m==2 && mm == 9) {     tw3n[m][mm]= 0.400046; } 
	  if(m==2 && mm == 11) {     tw3n[m][mm]= 1.907678; } 
	  if(m==2 && mm == 12) {     tw3n[m][mm]= 0.636610; } 
	  if(m==2 && mm == 16) {     tw3n[m][mm]= 1.372115; } 
	  if(m==2 && mm == 18) {     tw3n[m][mm]= 0.071180; } 
	  if(m==2 && mm == 20) {     tw3n[m][mm]= 0.394566; } 
	  if(m==2 && mm == 22) {     tw3n[m][mm]= 1.310370; } 
	  if(m==2 && mm == 26) {     tw3n[m][mm]= 1.789402; } 
	  if(m==2 && mm == 27) {     tw3n[m][mm]= 0.296164; } 
	  if(m==2 && mm == 8) {     te2n[m][mm]= 1.165828; } 
	  if(m==2 && mm == 11) {     te2n[m][mm]= 1.579716; } 
	  if(m==2 && mm == 13) {     te2n[m][mm]= 0.401600; } 
	  if(m==2 && mm == 14) {     te2n[m][mm]= 1.022858; } 
	  if(m==2 && mm == 17) {     te2n[m][mm]= 0.114848; } 
	  if(m==2 && mm == 18) {     te2n[m][mm]= 0.578940; } 
	  if(m==2 && mm == 19) {     te2n[m][mm]= 1.415803; } 
	  if(m==2 && mm == 20) {     te2n[m][mm]= 0.100966; } 
	  if(m==2 && mm == 21) {     te2n[m][mm]= 0.059835; } 
	  if(m==2 && mm == 22) {     te2n[m][mm]= 0.830490; } 
	  if(m==2 && mm == 23) {     te2n[m][mm]= 1.027902; } 
	  if(m==2 && mm == 24) {     te2n[m][mm]= 0.125798; } 
	  if(m==2 && mm == 25) {     te2n[m][mm]= 1.043855; } 
	  if(m==2 && mm == 32) {     te2n[m][mm]= 0.254395; } 
	  if(m==2 && mm == 0) {     te3n[m][mm]= 0.966775; } 
	  if(m==2 && mm == 1) {     te3n[m][mm]= 1.170639; } 
	  if(m==2 && mm == 2) {     te3n[m][mm]= 0.407182; } 
	  if(m==2 && mm == 3) {     te3n[m][mm]= 0.378694; } 
	  if(m==2 && mm == 4) {     te3n[m][mm]= 0.509656; } 
	  if(m==2 && mm == 20) {     te3n[m][mm]= 1.370860; } 
	  if(m==2 && mm == 21) {     te3n[m][mm]= 0.008416; } 
	  if(m==2 && mm == 24) {     te3n[m][mm]= 0.045266; } 
	  if(m==2 && mm == 26) {     te3n[m][mm]= 0.006391; } 
	  if(m==2 && mm == 28) {     te3n[m][mm]= 0.843020; } 
	  if(m==2 && mm == 29) {     te3n[m][mm]= 0.623465; } 
	  if(m==2 && mm == 30) {     te3n[m][mm]= 1.331607; } 
	  if(m==2 && mm == 31) {     te3n[m][mm]= 1.110550; } 
	  if(m==2 && mm == 32) {     te3n[m][mm]= 0.401904; } 
	  if(m==2 && mm == 33) {     te3n[m][mm]= 0.692037; } 
	  if(m==2 && mm == 34) {     te3n[m][mm]= 1.646772; } 
	  if(m==3 && mm == 9) {     tw0p[m][mm]= 0.234619; } 
	  if(m==3 && mm == 13) {     tw0p[m][mm]= 0.320267; } 
	  if(m==3 && mm == 15) {     tw0p[m][mm]= 0.301927; } 
	  if(m==3 && mm == 16) {     tw0p[m][mm]= 0.141239; } 
	  if(m==3 && mm == 17) {     tw0p[m][mm]= 0.364881; } 
	  if(m==3 && mm == 19) {     tw0p[m][mm]= 0.412839; } 
	  if(m==3 && mm == 20) {     tw0p[m][mm]= 0.584902; } 
	  if(m==3 && mm == 22) {     tw0p[m][mm]= 1.121122; } 
	  if(m==3 && mm == 23) {     tw0p[m][mm]= 0.064639; } 
	  if(m==3 && mm == 24) {     tw0p[m][mm]= 0.585731; } 
	  if(m==3 && mm == 25) {     tw0p[m][mm]= 0.028221; } 
	  if(m==3 && mm == 26) {     tw0p[m][mm]= 0.389678; } 
	  if(m==3 && mm == 28) {     tw0p[m][mm]= 0.886406; } 
	  if(m==3 && mm == 31) {     tw0p[m][mm]= 0.199215; } 
	  if(m==3 && mm == 32) {     tw0p[m][mm]= 0.512674; } 
	  if(m==3 && mm == 33) {     tw0p[m][mm]= 0.258183; } 
	  if(m==3 && mm == 34) {     tw0p[m][mm]= 0.388617; } 
	  if(m==3 && mm == 35) {     tw0p[m][mm]= 0.500002; } 
	  if(m==3 && mm == 0) {     tw1p[m][mm]= 0.512995; } 
	  if(m==3 && mm == 5) {     tw1p[m][mm]= 0.149820; } 
	  if(m==3 && mm == 6) {     tw1p[m][mm]= 0.753694; } 
	  if(m==3 && mm == 7) {     tw1p[m][mm]= 0.311604; } 
	  if(m==3 && mm == 8) {     tw1p[m][mm]= 0.592830; } 
	  if(m==3 && mm == 20) {     tw1p[m][mm]= 0.708695; } 
	  if(m==3 && mm == 22) {     tw1p[m][mm]= 0.923377; } 
	  if(m==3 && mm == 23) {     tw1p[m][mm]= 0.771616; } 
	  if(m==3 && mm == 28) {     tw1p[m][mm]= 0.389507; } 
	  if(m==3 && mm == 30) {     tw1p[m][mm]= 0.723578; } 
	  if(m==3 && mm == 31) {     tw1p[m][mm]= 0.497227; } 
	  if(m==3 && mm == 32) {     tw1p[m][mm]= 0.718371; } 
	  if(m==3 && mm == 33) {     tw1p[m][mm]= 0.104700; } 
	  if(m==3 && mm == 34) {     tw1p[m][mm]= 0.599966; } 
	  if(m==3 && mm == 35) {     tw1p[m][mm]= 0.722863; } 
	  if(m==3 && mm == 9) {     tw2p[m][mm]= 0.172812; } 
	  if(m==3 && mm == 11) {     tw2p[m][mm]= 0.139136; } 
	  if(m==3 && mm == 12) {     tw2p[m][mm]= 0.609458; } 
	  if(m==3 && mm == 13) {     tw2p[m][mm]= 0.678580; } 
	  if(m==3 && mm == 14) {     tw2p[m][mm]= 0.010489; } 
	  if(m==3 && mm == 15) {     tw2p[m][mm]= 0.799313; } 
	  if(m==3 && mm == 16) {     tw2p[m][mm]= 0.713003; } 
	  if(m==3 && mm == 17) {     tw2p[m][mm]= 0.505325; } 
	  if(m==3 && mm == 18) {     tw2p[m][mm]= 1.025229; } 
	  if(m==3 && mm == 19) {     tw2p[m][mm]= 0.316296; } 
	  if(m==3 && mm == 20) {     tw2p[m][mm]= 0.522084; } 
	  if(m==3 && mm == 31) {     tw2p[m][mm]= 0.974438; } 
	  if(m==3 && mm == 32) {     tw2p[m][mm]= 0.534520; } 
	  if(m==3 && mm == 33) {     tw2p[m][mm]= 0.666975; } 
	  if(m==3 && mm == 34) {     tw2p[m][mm]= 1.281988; } 
	  if(m==3 && mm == 35) {     tw2p[m][mm]= 0.591363; } 
	  if(m==3 && mm == 0) {     tw3p[m][mm]= 1.031360; } 
	  if(m==3 && mm == 1) {     tw3p[m][mm]= 0.652584; } 
	  if(m==3 && mm == 3) {     tw3p[m][mm]= 0.367490; } 
	  if(m==3 && mm == 4) {     tw3p[m][mm]= 1.032989; } 
	  if(m==3 && mm == 5) {     tw3p[m][mm]= 0.352460; } 
	  if(m==3 && mm == 7) {     tw3p[m][mm]= 0.521756; } 
	  if(m==3 && mm == 10) {     tw3p[m][mm]= 0.822290; } 
	  if(m==3 && mm == 11) {     tw3p[m][mm]= 2.843064; } 
	  if(m==3 && mm == 13) {     tw3p[m][mm]= 0.940248; } 
	  if(m==3 && mm == 21) {     tw3p[m][mm]= 1.442649; } 
	  if(m==3 && mm == 22) {     tw3p[m][mm]= 1.371879; } 
	  if(m==3 && mm == 23) {     tw3p[m][mm]= 0.504776; } 
	  if(m==3 && mm == 26) {     tw3p[m][mm]= 0.302254; } 
	  if(m==3 && mm == 27) {     tw3p[m][mm]= 0.568132; } 
	  if(m==3 && mm == 28) {     tw3p[m][mm]= 0.471281; } 
	  if(m==3 && mm == 31) {     tw3p[m][mm]= 0.025523; } 
	  if(m==3 && mm == 33) {     tw3p[m][mm]= 0.171753; } 
	  if(m==3 && mm == 34) {     tw3p[m][mm]= 0.104482; } 
	  if(m==3 && mm == 0) {     te2p[m][mm]= 0.220722; } 
	  if(m==3 && mm == 1) {     te2p[m][mm]= 0.044914; } 
	  if(m==3 && mm == 2) {     te2p[m][mm]= 0.333076; } 
	  if(m==3 && mm == 4) {     te2p[m][mm]= 0.380721; } 
	  if(m==3 && mm == 5) {     te2p[m][mm]= 0.979135; } 
	  if(m==3 && mm == 6) {     te2p[m][mm]= 1.122357; } 
	  if(m==3 && mm == 7) {     te2p[m][mm]= 2.490519; } 
	  if(m==3 && mm == 8) {     te2p[m][mm]= 0.087452; } 
	  if(m==3 && mm == 9) {     te2p[m][mm]= 1.492775; } 
	  if(m==3 && mm == 11) {     te2p[m][mm]= 1.602993; } 
	  if(m==3 && mm == 12) {     te2p[m][mm]= 0.185976; } 
	  if(m==3 && mm == 13) {     te2p[m][mm]= 0.143059; } 
	  if(m==3 && mm == 14) {     te2p[m][mm]= 0.254829; } 
	  if(m==3 && mm == 15) {     te2p[m][mm]= 0.623425; } 
	  if(m==3 && mm == 17) {     te2p[m][mm]= 0.531540; } 
	  if(m==3 && mm == 18) {     te2p[m][mm]= 0.662652; } 
	  if(m==3 && mm == 19) {     te2p[m][mm]= 0.300242; } 
	  if(m==3 && mm == 20) {     te2p[m][mm]= 0.155915; } 
	  if(m==3 && mm == 21) {     te2p[m][mm]= 0.268787; } 
	  if(m==3 && mm == 22) {     te2p[m][mm]= 0.450901; } 
	  if(m==3 && mm == 24) {     te2p[m][mm]= 0.386702; } 
	  if(m==3 && mm == 25) {     te2p[m][mm]= 0.320418; } 
	  if(m==3 && mm == 26) {     te2p[m][mm]= 0.694585; } 
	  if(m==3 && mm == 27) {     te2p[m][mm]= 0.239579; } 
	  if(m==3 && mm == 28) {     te2p[m][mm]= 0.264820; } 
	  if(m==3 && mm == 29) {     te2p[m][mm]= 0.623999; } 
	  if(m==3 && mm == 7) {     te3p[m][mm]= 0.941370; } 
	  if(m==3 && mm == 8) {     te3p[m][mm]= 0.908047; } 
	  if(m==3 && mm == 9) {     te3p[m][mm]= 1.630634; } 
	  if(m==3 && mm == 10) {     te3p[m][mm]= 1.047710; } 
	  if(m==3 && mm == 11) {     te3p[m][mm]= 0.411176; } 
	  if(m==3 && mm == 12) {     te3p[m][mm]= 1.319405; } 
	  if(m==3 && mm == 13) {     te3p[m][mm]= 0.740107; } 
	  if(m==3 && mm == 14) {     te3p[m][mm]= 0.207389; } 
	  if(m==3 && mm == 16) {     te3p[m][mm]= 0.471324; } 
	  if(m==3 && mm == 17) {     te3p[m][mm]= 0.454061; } 
	  if(m==3 && mm == 18) {     te3p[m][mm]= 0.901249; } 
	  if(m==3 && mm == 19) {     te3p[m][mm]= 1.474186; } 
	  if(m==3 && mm == 20) {     te3p[m][mm]= 0.103056; } 
	  if(m==3 && mm == 21) {     te3p[m][mm]= 1.828287; } 
	  if(m==3 && mm == 5) {     tw0n[m][mm]= 0.225547; } 
	  if(m==3 && mm == 6) {     tw0n[m][mm]= 0.089591; } 
	  if(m==3 && mm == 8) {     tw0n[m][mm]= 0.655946; } 
	  if(m==3 && mm == 9) {     tw0n[m][mm]= 0.210784; } 
	  if(m==3 && mm == 15) {     tw0n[m][mm]= 0.305413; } 
	  if(m==3 && mm == 16) {     tw0n[m][mm]= 0.131372; } 
	  if(m==3 && mm == 17) {     tw0n[m][mm]= 0.630036; } 
	  if(m==3 && mm == 18) {     tw0n[m][mm]= 0.841444; } 
	  if(m==3 && mm == 19) {     tw0n[m][mm]= 0.131321; } 
	  if(m==3 && mm == 20) {     tw0n[m][mm]= 0.670424; } 
	  if(m==3 && mm == 22) {     tw0n[m][mm]= 1.183892; } 
	  if(m==3 && mm == 23) {     tw0n[m][mm]= 0.259480; } 
	  if(m==3 && mm == 24) {     tw0n[m][mm]= 0.709154; } 
	  if(m==3 && mm == 25) {     tw0n[m][mm]= 0.005372; } 
	  if(m==3 && mm == 26) {     tw0n[m][mm]= 0.332301; } 
	  if(m==3 && mm == 27) {     tw0n[m][mm]= 0.027168; } 
	  if(m==3 && mm == 7) {     tw1n[m][mm]= 0.160479; } 
	  if(m==3 && mm == 8) {     tw1n[m][mm]= 0.649932; } 
	  if(m==3 && mm == 10) {     tw1n[m][mm]= 0.993162; } 
	  if(m==3 && mm == 11) {     tw1n[m][mm]= 0.325576; } 
	  if(m==3 && mm == 12) {     tw1n[m][mm]= 0.451655; } 
	  if(m==3 && mm == 15) {     tw1n[m][mm]= 0.359171; } 
	  if(m==3 && mm == 16) {     tw1n[m][mm]= 0.520357; } 
	  if(m==3 && mm == 17) {     tw1n[m][mm]= 1.121458; } 
	  if(m==3 && mm == 18) {     tw1n[m][mm]= 1.290283; } 
	  if(m==3 && mm == 19) {     tw1n[m][mm]= 1.119448; } 
	  if(m==3 && mm == 20) {     tw1n[m][mm]= 0.668047; } 
	  if(m==3 && mm == 21) {     tw1n[m][mm]= 1.980570; } 
	  if(m==3 && mm == 22) {     tw1n[m][mm]= 1.707855; } 
	  if(m==3 && mm == 31) {     tw1n[m][mm]= 0.099581; } 
	  if(m==3 && mm == 32) {     tw1n[m][mm]= 0.322583; } 
	  if(m==3 && mm == 34) {     tw1n[m][mm]= 0.296476; } 
	  if(m==3 && mm == 35) {     tw1n[m][mm]= 0.784397; } 
	  if(m==3 && mm == 0) {     tw2n[m][mm]= 0.989113; } 
	  if(m==3 && mm == 1) {     tw2n[m][mm]= 0.692428; } 
	  if(m==3 && mm == 2) {     tw2n[m][mm]= 0.271674; } 
	  if(m==3 && mm == 3) {     tw2n[m][mm]= 0.444495; } 
	  if(m==3 && mm == 4) {     tw2n[m][mm]= 0.054101; } 
	  if(m==3 && mm == 5) {     tw2n[m][mm]= 0.277463; } 
	  if(m==3 && mm == 16) {     tw2n[m][mm]= 0.745612; } 
	  if(m==3 && mm == 17) {     tw2n[m][mm]= 0.135873; } 
	  if(m==3 && mm == 19) {     tw2n[m][mm]= 0.246708; } 
	  if(m==3 && mm == 20) {     tw2n[m][mm]= 0.688343; } 
	  if(m==3 && mm == 21) {     tw2n[m][mm]= 0.735838; } 
	  if(m==3 && mm == 22) {     tw2n[m][mm]= 0.737565; } 
	  if(m==3 && mm == 25) {     tw2n[m][mm]= 0.089807; } 
	  if(m==3 && mm == 26) {     tw2n[m][mm]= 0.586322; } 
	  if(m==3 && mm == 27) {     tw2n[m][mm]= 0.347182; } 
	  if(m==3 && mm == 28) {     tw2n[m][mm]= 0.829175; } 
	  if(m==3 && mm == 30) {     tw2n[m][mm]= 0.943318; } 
	  if(m==3 && mm == 31) {     tw2n[m][mm]= 0.337892; } 
	  if(m==3 && mm == 32) {     tw2n[m][mm]= 0.376144; } 
	  if(m==3 && mm == 33) {     tw2n[m][mm]= 0.320230; } 
	  if(m==3 && mm == 34) {     tw2n[m][mm]= 1.101697; } 
	  if(m==3 && mm == 35) {     tw2n[m][mm]= 1.475953; } 
	  if(m==3 && mm == 0) {     tw3n[m][mm]= 0.928187; } 
	  if(m==3 && mm == 1) {     tw3n[m][mm]= 0.605121; } 
	  if(m==3 && mm == 3) {     tw3n[m][mm]= 0.367945; } 
	  if(m==3 && mm == 4) {     tw3n[m][mm]= 0.928800; } 
	  if(m==3 && mm == 5) {     tw3n[m][mm]= 0.349487; } 
	  if(m==3 && mm == 7) {     tw3n[m][mm]= 0.653747; } 
	  if(m==3 && mm == 10) {     tw3n[m][mm]= 1.145084; } 
	  if(m==3 && mm == 22) {     tw3n[m][mm]= 0.800218; } 
	  if(m==3 && mm == 23) {     tw3n[m][mm]= 0.400192; } 
	  if(m==3 && mm == 26) {     tw3n[m][mm]= 0.104661; } 
	  if(m==3 && mm == 5) {     te2n[m][mm]= 1.115022; } 
	  if(m==3 && mm == 6) {     te2n[m][mm]= 0.859334; } 
	  if(m==3 && mm == 7) {     te2n[m][mm]= 0.666251; } 
	  if(m==3 && mm == 9) {     te2n[m][mm]= 1.507413; } 
	  if(m==3 && mm == 10) {     te2n[m][mm]= 1.893666; } 
	  if(m==3 && mm == 11) {     te2n[m][mm]= 0.293733; } 
	  if(m==3 && mm == 12) {     te2n[m][mm]= 0.144869; } 
	  if(m==3 && mm == 14) {     te2n[m][mm]= 0.371594; } 
	  if(m==3 && mm == 17) {     te2n[m][mm]= 0.593273; } 
	  if(m==3 && mm == 18) {     te2n[m][mm]= 0.782652; } 
	  if(m==3 && mm == 19) {     te2n[m][mm]= 0.025533; } 
	  if(m==3 && mm == 20) {     te2n[m][mm]= 0.077617; } 
	  if(m==3 && mm == 21) {     te2n[m][mm]= 0.151796; } 
	  if(m==3 && mm == 22) {     te2n[m][mm]= 0.459940; } 
	  if(m==3 && mm == 23) {     te2n[m][mm]= 1.861786; } 
	  if(m==3 && mm == 24) {     te2n[m][mm]= 0.061682; } 
	  if(m==3 && mm == 25) {     te2n[m][mm]= 0.391356; } 
	  if(m==3 && mm == 27) {     te2n[m][mm]= 0.548498; } 
	  if(m==3 && mm == 28) {     te2n[m][mm]= 0.348486; } 
	  if(m==3 && mm == 29) {     te2n[m][mm]= 0.750049; } 
	  if(m==3 && mm == 32) {     te2n[m][mm]= 0.349819; } 
	  if(m==3 && mm == 33) {     te2n[m][mm]= 0.486762; } 
	  if(m==3 && mm == 34) {     te2n[m][mm]= 0.954490; } 
	  if(m==3 && mm == 35) {     te2n[m][mm]= 0.575683; } 
	  if(m==3 && mm == 0) {     te3n[m][mm]= 3.239801; } 
	  if(m==3 && mm == 1) {     te3n[m][mm]= 0.972593; } 
	  if(m==3 && mm == 2) {     te3n[m][mm]= 1.087590; } 
	  if(m==3 && mm == 3) {     te3n[m][mm]= 0.564422; } 
	  if(m==3 && mm == 5) {     te3n[m][mm]= 0.407456; } 
	  if(m==3 && mm == 20) {     te3n[m][mm]= 0.046608; } 
	  if(m==3 && mm == 23) {     te3n[m][mm]= 1.296254; } 
	  if(m==3 && mm == 24) {     te3n[m][mm]= 0.369262; } 
	  if(m==3 && mm == 25) {     te3n[m][mm]= 0.454051; } 
	  if(m==3 && mm == 27) {     te3n[m][mm]= 1.037006; } 
	  if(m==3 && mm == 28) {     te3n[m][mm]= 0.154777; } 
	  if(m==3 && mm == 29) {     te3n[m][mm]= 0.566838; } 
	  if(m==3 && mm == 30) {     te3n[m][mm]= 0.617105; } 
	  if(m==3 && mm == 31) {     te3n[m][mm]= 0.100564; } 
	  if(m==3 && mm == 32) {     te3n[m][mm]= 0.296814; } 
	  if(m==3 && mm == 33) {     te3n[m][mm]= 0.599151; } 
	  if(m==3 && mm == 34) {     te3n[m][mm]= 0.885616; } 
	  if(m==3 && mm == 35) {     te3n[m][mm]= 0.691508; } 
	  if(m==4 && mm == 7) {     tw0p[m][mm]= 0.398959; } 
	  if(m==4 && mm == 10) {     tw0p[m][mm]= 0.298287; } 
	  if(m==4 && mm == 12) {     tw0p[m][mm]= 0.725653; } 
	  if(m==4 && mm == 13) {     tw0p[m][mm]= 0.658076; } 
	  if(m==4 && mm == 14) {     tw0p[m][mm]= 0.946992; } 
	  if(m==4 && mm == 15) {     tw0p[m][mm]= 1.343521; } 
	  if(m==4 && mm == 16) {     tw0p[m][mm]= 0.216894; } 
	  if(m==4 && mm == 18) {     tw0p[m][mm]= 0.467205; } 
	  if(m==4 && mm == 19) {     tw0p[m][mm]= 0.831035; } 
	  if(m==4 && mm == 20) {     tw0p[m][mm]= 1.067567; } 
	  if(m==4 && mm == 21) {     tw0p[m][mm]= 0.114439; } 
	  if(m==4 && mm == 22) {     tw0p[m][mm]= 0.684304; } 
	  if(m==4 && mm == 23) {     tw0p[m][mm]= 0.437209; } 
	  if(m==4 && mm == 24) {     tw0p[m][mm]= 0.017171; } 
	  if(m==4 && mm == 27) {     tw0p[m][mm]= 0.379028; } 
	  if(m==4 && mm == 29) {     tw0p[m][mm]= 0.520150; } 
	  if(m==4 && mm == 30) {     tw0p[m][mm]= 0.487365; } 
	  if(m==4 && mm == 31) {     tw0p[m][mm]= 1.294872; } 
	  if(m==4 && mm == 32) {     tw0p[m][mm]= 0.519871; } 
	  if(m==4 && mm == 33) {     tw0p[m][mm]= 0.781837; } 
	  if(m==4 && mm == 34) {     tw0p[m][mm]= 1.134721; } 
	  if(m==4 && mm == 35) {     tw0p[m][mm]= 0.106128; } 
	  if(m==4 && mm == 0) {     tw1p[m][mm]= 0.028889; } 
	  if(m==4 && mm == 2) {     tw1p[m][mm]= 0.030433; } 
	  if(m==4 && mm == 4) {     tw1p[m][mm]= 0.696879; } 
	  if(m==4 && mm == 5) {     tw1p[m][mm]= 0.151584; } 
	  if(m==4 && mm == 20) {     tw1p[m][mm]= 0.355305; } 
	  if(m==4 && mm == 21) {     tw1p[m][mm]= 0.829912; } 
	  if(m==4 && mm == 28) {     tw1p[m][mm]= 0.520228; } 
	  if(m==4 && mm == 29) {     tw1p[m][mm]= 1.024901; } 
	  if(m==4 && mm == 30) {     tw1p[m][mm]= 0.269997; } 
	  if(m==4 && mm == 31) {     tw1p[m][mm]= 0.007938; } 
	  if(m==4 && mm == 33) {     tw1p[m][mm]= 0.164668; } 
	  if(m==4 && mm == 34) {     tw1p[m][mm]= 0.626352; } 
	  if(m==4 && mm == 6) {     tw2p[m][mm]= 0.890001; } 
	  if(m==4 && mm == 7) {     tw2p[m][mm]= 0.486472; } 
	  if(m==4 && mm == 9) {     tw2p[m][mm]= 0.656347; } 
	  if(m==4 && mm == 10) {     tw2p[m][mm]= 0.449240; } 
	  if(m==4 && mm == 11) {     tw2p[m][mm]= 0.026072; } 
	  if(m==4 && mm == 12) {     tw2p[m][mm]= 0.657513; } 
	  if(m==4 && mm == 13) {     tw2p[m][mm]= 0.235833; } 
	  if(m==4 && mm == 14) {     tw2p[m][mm]= 0.960106; } 
	  if(m==4 && mm == 15) {     tw2p[m][mm]= 1.465638; } 
	  if(m==4 && mm == 16) {     tw2p[m][mm]= 1.264172; } 
	  if(m==4 && mm == 17) {     tw2p[m][mm]= 0.491602; } 
	  if(m==4 && mm == 18) {     tw2p[m][mm]= 0.217311; } 
	  if(m==4 && mm == 31) {     tw2p[m][mm]= 0.459846; } 
	  if(m==4 && mm == 33) {     tw2p[m][mm]= 0.197024; } 
	  if(m==4 && mm == 34) {     tw2p[m][mm]= 0.852537; } 
	  if(m==4 && mm == 1) {     tw3p[m][mm]= 0.336522; } 
	  if(m==4 && mm == 13) {     tw3p[m][mm]= 0.803965; } 
	  if(m==4 && mm == 16) {     tw3p[m][mm]= 0.494582; } 
	  if(m==4 && mm == 17) {     tw3p[m][mm]= 1.632690; } 
	  if(m==4 && mm == 19) {     tw3p[m][mm]= 0.466959; } 
	  if(m==4 && mm == 20) {     tw3p[m][mm]= 0.631849; } 
	  if(m==4 && mm == 21) {     tw3p[m][mm]= 0.919590; } 
	  if(m==4 && mm == 23) {     tw3p[m][mm]= 0.986020; } 
	  if(m==4 && mm == 26) {     tw3p[m][mm]= 0.873465; } 
	  if(m==4 && mm == 27) {     tw3p[m][mm]= 0.462745; } 
	  if(m==4 && mm == 31) {     tw3p[m][mm]= 0.091717; } 
	  if(m==4 && mm == 33) {     tw3p[m][mm]= 0.257870; } 
	  if(m==4 && mm == 34) {     tw3p[m][mm]= 0.127918; } 
	  if(m==4 && mm == 1) {     te2p[m][mm]= 0.366384; } 
	  if(m==4 && mm == 3) {     te2p[m][mm]= 0.201310; } 
	  if(m==4 && mm == 4) {     te2p[m][mm]= 0.046639; } 
	  if(m==4 && mm == 5) {     te2p[m][mm]= 3.714250; } 
	  if(m==4 && mm == 6) {     te2p[m][mm]= 0.304484; } 
	  if(m==4 && mm == 7) {     te2p[m][mm]= 0.316381; } 
	  if(m==4 && mm == 8) {     te2p[m][mm]= 0.089047; } 
	  if(m==4 && mm == 9) {     te2p[m][mm]= 0.465537; } 
	  if(m==4 && mm == 10) {     te2p[m][mm]= 0.885153; } 
	  if(m==4 && mm == 11) {     te2p[m][mm]= 0.905282; } 
	  if(m==4 && mm == 12) {     te2p[m][mm]= 0.258272; } 
	  if(m==4 && mm == 13) {     te2p[m][mm]= 0.216393; } 
	  if(m==4 && mm == 14) {     te2p[m][mm]= 0.317433; } 
	  if(m==4 && mm == 15) {     te2p[m][mm]= 0.186780; } 
	  if(m==4 && mm == 16) {     te2p[m][mm]= 0.688796; } 
	  if(m==4 && mm == 19) {     te2p[m][mm]= 0.275242; } 
	  if(m==4 && mm == 21) {     te2p[m][mm]= 0.272418; } 
	  if(m==4 && mm == 22) {     te2p[m][mm]= 0.253883; } 
	  if(m==4 && mm == 23) {     te2p[m][mm]= 0.546748; } 
	  if(m==4 && mm == 26) {     te2p[m][mm]= 0.142969; } 
	  if(m==4 && mm == 27) {     te2p[m][mm]= 0.099716; } 
	  if(m==4 && mm == 28) {     te2p[m][mm]= 0.332689; } 
	  if(m==4 && mm == 5) {     te3p[m][mm]= 0.156164; } 
	  if(m==4 && mm == 7) {     te3p[m][mm]= 0.701462; } 
	  if(m==4 && mm == 8) {     te3p[m][mm]= 0.064121; } 
	  if(m==4 && mm == 9) {     te3p[m][mm]= 0.557657; } 
	  if(m==4 && mm == 12) {     te3p[m][mm]= 0.576186; } 
	  if(m==4 && mm == 13) {     te3p[m][mm]= 0.239337; } 
	  if(m==4 && mm == 15) {     te3p[m][mm]= 0.096339; } 
	  if(m==4 && mm == 17) {     te3p[m][mm]= 0.157265; } 
	  if(m==4 && mm == 18) {     te3p[m][mm]= 0.039882; } 
	  if(m==4 && mm == 19) {     te3p[m][mm]= 0.568567; } 
	  if(m==4 && mm == 0) {     tw0n[m][mm]= 0.777650; } 
	  if(m==4 && mm == 2) {     tw0n[m][mm]= 0.116265; } 
	  if(m==4 && mm == 3) {     tw0n[m][mm]= 0.165220; } 
	  if(m==4 && mm == 6) {     tw0n[m][mm]= 0.097530; } 
	  if(m==4 && mm == 10) {     tw0n[m][mm]= 0.573207; } 
	  if(m==4 && mm == 11) {     tw0n[m][mm]= 0.207985; } 
	  if(m==4 && mm == 12) {     tw0n[m][mm]= 1.034304; } 
	  if(m==4 && mm == 13) {     tw0n[m][mm]= 0.944731; } 
	  if(m==4 && mm == 14) {     tw0n[m][mm]= 1.138897; } 
	  if(m==4 && mm == 15) {     tw0n[m][mm]= 1.509806; } 
	  if(m==4 && mm == 16) {     tw0n[m][mm]= 0.336856; } 
	  if(m==4 && mm == 19) {     tw0n[m][mm]= 0.831205; } 
	  if(m==4 && mm == 20) {     tw0n[m][mm]= 1.434283; } 
	  if(m==4 && mm == 21) {     tw0n[m][mm]= 0.370859; } 
	  if(m==4 && mm == 22) {     tw0n[m][mm]= 0.809213; } 
	  if(m==4 && mm == 23) {     tw0n[m][mm]= 0.123847; } 
	  if(m==4 && mm == 26) {     tw0n[m][mm]= 0.545092; } 
	  if(m==4 && mm == 27) {     tw0n[m][mm]= 0.273578; } 
	  if(m==4 && mm == 29) {     tw0n[m][mm]= 0.501969; } 
	  if(m==4 && mm == 9) {     tw1n[m][mm]= 0.124097; } 
	  if(m==4 && mm == 10) {     tw1n[m][mm]= 0.025140; } 
	  if(m==4 && mm == 11) {     tw1n[m][mm]= 0.731036; } 
	  if(m==4 && mm == 14) {     tw1n[m][mm]= 0.581462; } 
	  if(m==4 && mm == 17) {     tw1n[m][mm]= 0.538833; } 
	  if(m==4 && mm == 19) {     tw1n[m][mm]= 0.364609; } 
	  if(m==4 && mm == 20) {     tw1n[m][mm]= 0.136245; } 
	  if(m==4 && mm == 21) {     tw1n[m][mm]= 0.100453; } 
	  if(m==4 && mm == 23) {     tw1n[m][mm]= 1.196410; } 
	  if(m==4 && mm == 30) {     tw1n[m][mm]= 0.183262; } 
	  if(m==4 && mm == 31) {     tw1n[m][mm]= 0.117695; } 
	  if(m==4 && mm == 34) {     tw1n[m][mm]= 0.528351; } 
	  if(m==4 && mm == 35) {     tw1n[m][mm]= 2.537618; } 
	  if(m==4 && mm == 1) {     tw2n[m][mm]= 0.306498; } 
	  if(m==4 && mm == 15) {     tw2n[m][mm]= 1.342817; } 
	  if(m==4 && mm == 16) {     tw2n[m][mm]= 1.357192; } 
	  if(m==4 && mm == 17) {     tw2n[m][mm]= 0.202123; } 
	  if(m==4 && mm == 18) {     tw2n[m][mm]= 0.270373; } 
	  if(m==4 && mm == 23) {     tw2n[m][mm]= 0.456032; } 
	  if(m==4 && mm == 24) {     tw2n[m][mm]= 0.569860; } 
	  if(m==4 && mm == 25) {     tw2n[m][mm]= 0.275220; } 
	  if(m==4 && mm == 27) {     tw2n[m][mm]= 0.505603; } 
	  if(m==4 && mm == 28) {     tw2n[m][mm]= 0.035249; } 
	  if(m==4 && mm == 29) {     tw2n[m][mm]= 0.140218; } 
	  if(m==4 && mm == 34) {     tw2n[m][mm]= 0.612933; } 
	  if(m==4 && mm == 35) {     tw2n[m][mm]= 0.113452; } 
	  if(m==4 && mm == 1) {     tw3n[m][mm]= 0.581507; } 
	  if(m==4 && mm == 13) {     tw3n[m][mm]= 0.754243; } 
	  if(m==4 && mm == 16) {     tw3n[m][mm]= 1.068355; } 
	  if(m==4 && mm == 17) {     tw3n[m][mm]= 1.662156; } 
	  if(m==4 && mm == 19) {     tw3n[m][mm]= 0.305231; } 
	  if(m==4 && mm == 20) {     tw3n[m][mm]= 0.303251; } 
	  if(m==4 && mm == 21) {     tw3n[m][mm]= 0.756351; } 
	  if(m==4 && mm == 22) {     tw3n[m][mm]= 0.667824; } 
	  if(m==4 && mm == 23) {     tw3n[m][mm]= 1.097370; } 
	  if(m==4 && mm == 26) {     tw3n[m][mm]= 1.243135; } 
	  if(m==4 && mm == 27) {     tw3n[m][mm]= 0.219672; } 
	  if(m==4 && mm == 6) {     te2n[m][mm]= 0.258502; } 
	  if(m==4 && mm == 7) {     te2n[m][mm]= 0.124044; } 
	  if(m==4 && mm == 9) {     te2n[m][mm]= 0.740465; } 
	  if(m==4 && mm == 10) {     te2n[m][mm]= 0.696776; } 
	  if(m==4 && mm == 11) {     te2n[m][mm]= 0.618279; } 
	  if(m==4 && mm == 12) {     te2n[m][mm]= 0.042214; } 
	  if(m==4 && mm == 13) {     te2n[m][mm]= 0.469288; } 
	  if(m==4 && mm == 14) {     te2n[m][mm]= 0.515641; } 
	  if(m==4 && mm == 15) {     te2n[m][mm]= 0.174864; } 
	  if(m==4 && mm == 16) {     te2n[m][mm]= 0.994934; } 
	  if(m==4 && mm == 19) {     te2n[m][mm]= 0.082821; } 
	  if(m==4 && mm == 20) {     te2n[m][mm]= 0.196839; } 
	  if(m==4 && mm == 21) {     te2n[m][mm]= 0.405873; } 
	  if(m==4 && mm == 22) {     te2n[m][mm]= 0.447946; } 
	  if(m==4 && mm == 24) {     te2n[m][mm]= 0.729970; } 
	  if(m==4 && mm == 26) {     te2n[m][mm]= 0.350055; } 
	  if(m==4 && mm == 27) {     te2n[m][mm]= 0.393324; } 
	  if(m==4 && mm == 28) {     te2n[m][mm]= 0.487200; } 
	  if(m==4 && mm == 31) {     te2n[m][mm]= 0.356253; } 
	  if(m==4 && mm == 32) {     te2n[m][mm]= 0.382804; } 
	  if(m==4 && mm == 35) {     te2n[m][mm]= 0.105183; } 
	  if(m==4 && mm == 0) {     te3n[m][mm]= 0.178922; } 
	  if(m==4 && mm == 1) {     te3n[m][mm]= 0.238414; } 
	  if(m==4 && mm == 2) {     te3n[m][mm]= 0.344566; } 
	  if(m==4 && mm == 3) {     te3n[m][mm]= 1.209121; } 
	  if(m==4 && mm == 5) {     te3n[m][mm]= 0.091766; } 
	  if(m==4 && mm == 19) {     te3n[m][mm]= 0.410960; } 
	  if(m==4 && mm == 21) {     te3n[m][mm]= 1.031994; } 
	  if(m==4 && mm == 22) {     te3n[m][mm]= 1.510650; } 
	  if(m==4 && mm == 23) {     te3n[m][mm]= 0.679442; } 
	  if(m==4 && mm == 24) {     te3n[m][mm]= 0.505618; } 
	  if(m==4 && mm == 25) {     te3n[m][mm]= 0.003979; } 
	  if(m==4 && mm == 27) {     te3n[m][mm]= 1.143939; } 
	  if(m==4 && mm == 28) {     te3n[m][mm]= 0.690480; } 
	  if(m==4 && mm == 29) {     te3n[m][mm]= 0.986019; } 
	  if(m==4 && mm == 30) {     te3n[m][mm]= 0.323353; } 
	  if(m==4 && mm == 32) {     te3n[m][mm]= 0.184433; } 
	  if(m==4 && mm == 33) {     te3n[m][mm]= 0.196765; } 
	  if(m==4 && mm == 34) {     te3n[m][mm]= 0.787394; } 
	  if(m==5 && mm == 7) {     tw0p[m][mm]= 0.305904; } 
	  if(m==5 && mm == 9) {     tw0p[m][mm]= 0.227676; } 
	  if(m==5 && mm == 10) {     tw0p[m][mm]= 0.081738; } 
	  if(m==5 && mm == 11) {     tw0p[m][mm]= 0.011602; } 
	  if(m==5 && mm == 12) {     tw0p[m][mm]= 1.177873; } 
	  if(m==5 && mm == 13) {     tw0p[m][mm]= 0.138969; } 
	  if(m==5 && mm == 14) {     tw0p[m][mm]= 1.026837; } 
	  if(m==5 && mm == 15) {     tw0p[m][mm]= 1.750262; } 
	  if(m==5 && mm == 16) {     tw0p[m][mm]= 0.400173; } 
	  if(m==5 && mm == 17) {     tw0p[m][mm]= 0.763928; } 
	  if(m==5 && mm == 18) {     tw0p[m][mm]= 0.229743; } 
	  if(m==5 && mm == 19) {     tw0p[m][mm]= 0.508909; } 
	  if(m==5 && mm == 20) {     tw0p[m][mm]= 0.771699; } 
	  if(m==5 && mm == 21) {     tw0p[m][mm]= 0.009690; } 
	  if(m==5 && mm == 22) {     tw0p[m][mm]= 0.167804; } 
	  if(m==5 && mm == 23) {     tw0p[m][mm]= 0.700259; } 
	  if(m==5 && mm == 24) {     tw0p[m][mm]= 0.492231; } 
	  if(m==5 && mm == 25) {     tw0p[m][mm]= 0.362315; } 
	  if(m==5 && mm == 26) {     tw0p[m][mm]= 0.613861; } 
	  if(m==5 && mm == 27) {     tw0p[m][mm]= 0.036516; } 
	  if(m==5 && mm == 29) {     tw0p[m][mm]= 0.646268; } 
	  if(m==5 && mm == 30) {     tw0p[m][mm]= 0.495715; } 
	  if(m==5 && mm == 31) {     tw0p[m][mm]= 0.643557; } 
	  if(m==5 && mm == 32) {     tw0p[m][mm]= 0.872276; } 
	  if(m==5 && mm == 33) {     tw0p[m][mm]= 0.064887; } 
	  if(m==5 && mm == 35) {     tw0p[m][mm]= 0.439374; } 
	  if(m==5 && mm == 0) {     tw1p[m][mm]= 0.304010; } 
	  if(m==5 && mm == 1) {     tw1p[m][mm]= 0.695880; } 
	  if(m==5 && mm == 2) {     tw1p[m][mm]= 0.196318; } 
	  if(m==5 && mm == 3) {     tw1p[m][mm]= 0.060244; } 
	  if(m==5 && mm == 5) {     tw1p[m][mm]= 0.015694; } 
	  if(m==5 && mm == 7) {     tw1p[m][mm]= 0.234627; } 
	  if(m==5 && mm == 19) {     tw1p[m][mm]= 0.360315; } 
	  if(m==5 && mm == 20) {     tw1p[m][mm]= 1.910562; } 
	  if(m==5 && mm == 21) {     tw1p[m][mm]= 1.222509; } 
	  if(m==5 && mm == 22) {     tw1p[m][mm]= 0.331270; } 
	  if(m==5 && mm == 23) {     tw1p[m][mm]= 1.472491; } 
	  if(m==5 && mm == 28) {     tw1p[m][mm]= 1.938477; } 
	  if(m==5 && mm == 29) {     tw1p[m][mm]= 0.042822; } 
	  if(m==5 && mm == 30) {     tw1p[m][mm]= 0.944190; } 
	  if(m==5 && mm == 31) {     tw1p[m][mm]= 0.886662; } 
	  if(m==5 && mm == 32) {     tw1p[m][mm]= 0.439455; } 
	  if(m==5 && mm == 33) {     tw1p[m][mm]= 0.031224; } 
	  if(m==5 && mm == 34) {     tw1p[m][mm]= 0.489322; } 
	  if(m==5 && mm == 35) {     tw1p[m][mm]= 0.528510; } 
	  if(m==5 && mm == 7) {     tw2p[m][mm]= 0.537339; } 
	  if(m==5 && mm == 9) {     tw2p[m][mm]= 0.571578; } 
	  if(m==5 && mm == 10) {     tw2p[m][mm]= 0.484523; } 
	  if(m==5 && mm == 11) {     tw2p[m][mm]= 0.583830; } 
	  if(m==5 && mm == 12) {     tw2p[m][mm]= 0.828156; } 
	  if(m==5 && mm == 13) {     tw2p[m][mm]= 1.115410; } 
	  if(m==5 && mm == 14) {     tw2p[m][mm]= 0.208354; } 
	  if(m==5 && mm == 15) {     tw2p[m][mm]= 0.559399; } 
	  if(m==5 && mm == 17) {     tw2p[m][mm]= 1.779934; } 
	  if(m==5 && mm == 18) {     tw2p[m][mm]= 0.892422; } 
	  if(m==5 && mm == 19) {     tw2p[m][mm]= 0.708755; } 
	  if(m==5 && mm == 20) {     tw2p[m][mm]= 0.551884; } 
	  if(m==5 && mm == 21) {     tw2p[m][mm]= 0.634418; } 
	  if(m==5 && mm == 29) {     tw2p[m][mm]= 0.588689; } 
	  if(m==5 && mm == 30) {     tw2p[m][mm]= 0.129128; } 
	  if(m==5 && mm == 31) {     tw2p[m][mm]= 0.302522; } 
	  if(m==5 && mm == 33) {     tw2p[m][mm]= 0.719057; } 
	  if(m==5 && mm == 35) {     tw2p[m][mm]= 1.035608; } 
	  if(m==5 && mm == 1) {     tw3p[m][mm]= 0.685178; } 
	  if(m==5 && mm == 16) {     tw3p[m][mm]= 0.954046; } 
	  if(m==5 && mm == 17) {     tw3p[m][mm]= 0.247299; } 
	  if(m==5 && mm == 18) {     tw3p[m][mm]= 0.434528; } 
	  if(m==5 && mm == 20) {     tw3p[m][mm]= 1.317241; } 
	  if(m==5 && mm == 21) {     tw3p[m][mm]= 1.348472; } 
	  if(m==5 && mm == 22) {     tw3p[m][mm]= 0.509935; } 
	  if(m==5 && mm == 23) {     tw3p[m][mm]= 0.338503; } 
	  if(m==5 && mm == 25) {     tw3p[m][mm]= 0.314588; } 
	  if(m==5 && mm == 26) {     tw3p[m][mm]= 0.440486; } 
	  if(m==5 && mm == 27) {     tw3p[m][mm]= 1.383259; } 
	  if(m==5 && mm == 30) {     tw3p[m][mm]= 0.418580; } 
	  if(m==5 && mm == 32) {     tw3p[m][mm]= 0.309870; } 
	  if(m==5 && mm == 35) {     tw3p[m][mm]= 0.040952; } 
	  if(m==5 && mm == 0) {     te2p[m][mm]= 0.398276; } 
	  if(m==5 && mm == 1) {     te2p[m][mm]= 0.541213; } 
	  if(m==5 && mm == 2) {     te2p[m][mm]= 0.276449; } 
	  if(m==5 && mm == 3) {     te2p[m][mm]= 0.148996; } 
	  if(m==5 && mm == 4) {     te2p[m][mm]= 0.668070; } 
	  if(m==5 && mm == 6) {     te2p[m][mm]= 0.546498; } 
	  if(m==5 && mm == 7) {     te2p[m][mm]= 2.138673; } 
	  if(m==5 && mm == 8) {     te2p[m][mm]= 0.527836; } 
	  if(m==5 && mm == 9) {     te2p[m][mm]= 1.115520; } 
	  if(m==5 && mm == 10) {     te2p[m][mm]= 1.400694; } 
	  if(m==5 && mm == 11) {     te2p[m][mm]= 0.163858; } 
	  if(m==5 && mm == 12) {     te2p[m][mm]= 0.127006; } 
	  if(m==5 && mm == 13) {     te2p[m][mm]= 0.820290; } 
	  if(m==5 && mm == 14) {     te2p[m][mm]= 0.028926; } 
	  if(m==5 && mm == 15) {     te2p[m][mm]= 0.199010; } 
	  if(m==5 && mm == 16) {     te2p[m][mm]= 0.779819; } 
	  if(m==5 && mm == 17) {     te2p[m][mm]= 0.128235; } 
	  if(m==5 && mm == 18) {     te2p[m][mm]= 0.754082; } 
	  if(m==5 && mm == 19) {     te2p[m][mm]= 0.691011; } 
	  if(m==5 && mm == 20) {     te2p[m][mm]= 1.232200; } 
	  if(m==5 && mm == 21) {     te2p[m][mm]= 0.532946; } 
	  if(m==5 && mm == 22) {     te2p[m][mm]= 0.308650; } 
	  if(m==5 && mm == 23) {     te2p[m][mm]= 0.426486; } 
	  if(m==5 && mm == 26) {     te2p[m][mm]= 0.224409; } 
	  if(m==5 && mm == 28) {     te2p[m][mm]= 0.667584; } 
	  if(m==5 && mm == 29) {     te2p[m][mm]= 0.584071; } 
	  if(m==5 && mm == 4) {     te3p[m][mm]= 1.815549; } 
	  if(m==5 && mm == 5) {     te3p[m][mm]= 0.527834; } 
	  if(m==5 && mm == 7) {     te3p[m][mm]= 0.213345; } 
	  if(m==5 && mm == 8) {     te3p[m][mm]= 0.527024; } 
	  if(m==5 && mm == 9) {     te3p[m][mm]= 0.568039; } 
	  if(m==5 && mm == 10) {     te3p[m][mm]= 0.216830; } 
	  if(m==5 && mm == 11) {     te3p[m][mm]= 0.064371; } 
	  if(m==5 && mm == 12) {     te3p[m][mm]= 0.119830; } 
	  if(m==5 && mm == 13) {     te3p[m][mm]= 0.427969; } 
	  if(m==5 && mm == 14) {     te3p[m][mm]= 0.044980; } 
	  if(m==5 && mm == 15) {     te3p[m][mm]= 0.530094; } 
	  if(m==5 && mm == 16) {     te3p[m][mm]= 0.493963; } 
	  if(m==5 && mm == 17) {     te3p[m][mm]= 0.003666; } 
	  if(m==5 && mm == 18) {     te3p[m][mm]= 0.901719; } 
	  if(m==5 && mm == 19) {     te3p[m][mm]= 0.128750; } 
	  if(m==5 && mm == 20) {     te3p[m][mm]= 0.873159; } 
	  if(m==5 && mm == 21) {     te3p[m][mm]= 1.565912; } 
	  if(m==5 && mm == 22) {     te3p[m][mm]= 0.669825; } 
	  if(m==5 && mm == 23) {     te3p[m][mm]= 0.683696; } 
	  if(m==5 && mm == 0) {     tw0n[m][mm]= 0.228758; } 
	  if(m==5 && mm == 4) {     tw0n[m][mm]= 0.005954; } 
	  if(m==5 && mm == 5) {     tw0n[m][mm]= 0.098936; } 
	  if(m==5 && mm == 6) {     tw0n[m][mm]= 0.184999; } 
	  if(m==5 && mm == 7) {     tw0n[m][mm]= 0.398936; } 
	  if(m==5 && mm == 9) {     tw0n[m][mm]= 0.103694; } 
	  if(m==5 && mm == 10) {     tw0n[m][mm]= 0.320975; } 
	  if(m==5 && mm == 11) {     tw0n[m][mm]= 0.417631; } 
	  if(m==5 && mm == 12) {     tw0n[m][mm]= 1.811926; } 
	  if(m==5 && mm == 13) {     tw0n[m][mm]= 0.318982; } 
	  if(m==5 && mm == 14) {     tw0n[m][mm]= 1.224358; } 
	  if(m==5 && mm == 16) {     tw0n[m][mm]= 0.313221; } 
	  if(m==5 && mm == 18) {     tw0n[m][mm]= 0.140636; } 
	  if(m==5 && mm == 19) {     tw0n[m][mm]= 0.497789; } 
	  if(m==5 && mm == 20) {     tw0n[m][mm]= 0.849546; } 
	  if(m==5 && mm == 21) {     tw0n[m][mm]= 0.053917; } 
	  if(m==5 && mm == 22) {     tw0n[m][mm]= 0.152941; } 
	  if(m==5 && mm == 23) {     tw0n[m][mm]= 0.618965; } 
	  if(m==5 && mm == 24) {     tw0n[m][mm]= 0.345015; } 
	  if(m==5 && mm == 25) {     tw0n[m][mm]= 0.169436; } 
	  if(m==5 && mm == 26) {     tw0n[m][mm]= 0.766622; } 
	  if(m==5 && mm == 29) {     tw0n[m][mm]= 1.071808; } 
	  if(m==5 && mm == 7) {     tw1n[m][mm]= 0.354955; } 
	  if(m==5 && mm == 8) {     tw1n[m][mm]= 0.028649; } 
	  if(m==5 && mm == 9) {     tw1n[m][mm]= 0.599768; } 
	  if(m==5 && mm == 11) {     tw1n[m][mm]= 0.582877; } 
	  if(m==5 && mm == 12) {     tw1n[m][mm]= 0.200490; } 
	  if(m==5 && mm == 13) {     tw1n[m][mm]= 0.154621; } 
	  if(m==5 && mm == 16) {     tw1n[m][mm]= 0.557858; } 
	  if(m==5 && mm == 17) {     tw1n[m][mm]= 0.281092; } 
	  if(m==5 && mm == 18) {     tw1n[m][mm]= 0.412008; } 
	  if(m==5 && mm == 19) {     tw1n[m][mm]= 0.150815; } 
	  if(m==5 && mm == 20) {     tw1n[m][mm]= 1.237392; } 
	  if(m==5 && mm == 21) {     tw1n[m][mm]= 1.248533; } 
	  if(m==5 && mm == 22) {     tw1n[m][mm]= 0.275090; } 
	  if(m==5 && mm == 23) {     tw1n[m][mm]= 0.612531; } 
	  if(m==5 && mm == 30) {     tw1n[m][mm]= 0.714449; } 
	  if(m==5 && mm == 31) {     tw1n[m][mm]= 0.664230; } 
	  if(m==5 && mm == 32) {     tw1n[m][mm]= 0.186587; } 
	  if(m==5 && mm == 34) {     tw1n[m][mm]= 0.215850; } 
	  if(m==5 && mm == 35) {     tw1n[m][mm]= 0.136709; } 
	  if(m==5 && mm == 0) {     tw2n[m][mm]= 0.156642; } 
	  if(m==5 && mm == 2) {     tw2n[m][mm]= 0.315597; } 
	  if(m==5 && mm == 3) {     tw2n[m][mm]= 0.131056; } 
	  if(m==5 && mm == 6) {     tw2n[m][mm]= 0.869093; } 
	  if(m==5 && mm == 15) {     tw2n[m][mm]= 0.596226; } 
	  if(m==5 && mm == 16) {     tw2n[m][mm]= 1.426666; } 
	  if(m==5 && mm == 17) {     tw2n[m][mm]= 1.062626; } 
	  if(m==5 && mm == 18) {     tw2n[m][mm]= 0.794399; } 
	  if(m==5 && mm == 19) {     tw2n[m][mm]= 0.706578; } 
	  if(m==5 && mm == 20) {     tw2n[m][mm]= 0.572156; } 
	  if(m==5 && mm == 21) {     tw2n[m][mm]= 0.504869; } 
	  if(m==5 && mm == 22) {     tw2n[m][mm]= 0.715397; } 
	  if(m==5 && mm == 23) {     tw2n[m][mm]= 0.448689; } 
	  if(m==5 && mm == 25) {     tw2n[m][mm]= 0.236684; } 
	  if(m==5 && mm == 28) {     tw2n[m][mm]= 0.890080; } 
	  if(m==5 && mm == 29) {     tw2n[m][mm]= 0.339898; } 
	  if(m==5 && mm == 30) {     tw2n[m][mm]= 0.049762; } 
	  if(m==5 && mm == 31) {     tw2n[m][mm]= 0.661609; } 
	  if(m==5 && mm == 33) {     tw2n[m][mm]= 0.315046; } 
	  if(m==5 && mm == 35) {     tw2n[m][mm]= 0.908280; } 
	  if(m==5 && mm == 1) {     tw3n[m][mm]= 0.853011; } 
	  if(m==5 && mm == 16) {     tw3n[m][mm]= 0.645502; } 
	  if(m==5 && mm == 17) {     tw3n[m][mm]= 0.560118; } 
	  if(m==5 && mm == 18) {     tw3n[m][mm]= 0.357571; } 
	  if(m==5 && mm == 20) {     tw3n[m][mm]= 0.961890; } 
	  if(m==5 && mm == 21) {     tw3n[m][mm]= 1.323141; } 
	  if(m==5 && mm == 22) {     tw3n[m][mm]= 0.900734; } 
	  if(m==5 && mm == 23) {     tw3n[m][mm]= 0.288404; } 
	  if(m==5 && mm == 26) {     tw3n[m][mm]= 0.135968; } 
	  if(m==5 && mm == 27) {     tw3n[m][mm]= 0.481751; } 
	  if(m==5 && mm == 7) {     te2n[m][mm]= 1.580001; } 
	  if(m==5 && mm == 8) {     te2n[m][mm]= 0.370491; } 
	  if(m==5 && mm == 9) {     te2n[m][mm]= 1.102557; } 
	  if(m==5 && mm == 10) {     te2n[m][mm]= 1.593330; } 
	  if(m==5 && mm == 11) {     te2n[m][mm]= 0.300268; } 
	  if(m==5 && mm == 12) {     te2n[m][mm]= 0.479459; } 
	  if(m==5 && mm == 13) {     te2n[m][mm]= 0.788175; } 
	  if(m==5 && mm == 15) {     te2n[m][mm]= 0.252972; } 
	  if(m==5 && mm == 16) {     te2n[m][mm]= 0.771387; } 
	  if(m==5 && mm == 17) {     te2n[m][mm]= 0.097968; } 
	  if(m==5 && mm == 18) {     te2n[m][mm]= 0.646105; } 
	  if(m==5 && mm == 19) {     te2n[m][mm]= 0.192744; } 
	  if(m==5 && mm == 20) {     te2n[m][mm]= 1.310935; } 
	  if(m==5 && mm == 21) {     te2n[m][mm]= 0.580144; } 
	  if(m==5 && mm == 22) {     te2n[m][mm]= 0.441996; } 
	  if(m==5 && mm == 23) {     te2n[m][mm]= 0.303685; } 
	  if(m==5 && mm == 28) {     te2n[m][mm]= 0.890535; } 
	  if(m==5 && mm == 29) {     te2n[m][mm]= 0.302442; } 
	  if(m==5 && mm == 32) {     te2n[m][mm]= 0.027228; } 
	  if(m==5 && mm == 0) {     te3n[m][mm]= 0.142263; } 
	  if(m==5 && mm == 1) {     te3n[m][mm]= 0.103663; } 
	  if(m==5 && mm == 2) {     te3n[m][mm]= 0.994344; } 
	  if(m==5 && mm == 3) {     te3n[m][mm]= 1.967088; } 
	  if(m==5 && mm == 4) {     te3n[m][mm]= 1.068184; } 
	  if(m==5 && mm == 5) {     te3n[m][mm]= 0.414340; } 
	  if(m==5 && mm == 6) {     te3n[m][mm]= 0.978156; } 
	  if(m==5 && mm == 16) {     te3n[m][mm]= 0.386743; } 
	  if(m==5 && mm == 17) {     te3n[m][mm]= 0.109450; } 
	  if(m==5 && mm == 18) {     te3n[m][mm]= 1.418322; } 
	  if(m==5 && mm == 19) {     te3n[m][mm]= 0.231707; } 
	  if(m==5 && mm == 20) {     te3n[m][mm]= 1.068068; } 
	  if(m==5 && mm == 21) {     te3n[m][mm]= 1.395636; } 
	  if(m==5 && mm == 22) {     te3n[m][mm]= 0.452516; } 
	  if(m==5 && mm == 23) {     te3n[m][mm]= 0.602712; } 
	  if(m==5 && mm == 24) {     te3n[m][mm]= 0.294696; } 
	  if(m==5 && mm == 25) {     te3n[m][mm]= 0.265443; } 
	  if(m==5 && mm == 26) {     te3n[m][mm]= 0.517715; } 
	  if(m==5 && mm == 28) {     te3n[m][mm]= 0.412911; } 
	  if(m==5 && mm == 29) {     te3n[m][mm]= 1.039072; } 
	  if(m==5 && mm == 30) {     te3n[m][mm]= 1.297692; } 
	  if(m==5 && mm == 31) {     te3n[m][mm]= 0.133061; } 
	  if(m==5 && mm == 32) {     te3n[m][mm]= 0.480643; } 
	  if(m==5 && mm == 33) {     te3n[m][mm]= 0.424156; } 
	  if(m==5 && mm == 34) {     te3n[m][mm]= 0.195133; } 
	  if(m==5 && mm == 35) {     te3n[m][mm]= 0.008372; } 
	  if(m==6 && mm == 12) {     tw0p[m][mm]= 2.094676; } 
	  if(m==6 && mm == 13) {     tw0p[m][mm]= 0.808929; } 
	  if(m==6 && mm == 15) {     tw0p[m][mm]= 0.261551; } 
	  if(m==6 && mm == 16) {     tw0p[m][mm]= 0.158516; } 
	  if(m==6 && mm == 17) {     tw0p[m][mm]= 0.818368; } 
	  if(m==6 && mm == 18) {     tw0p[m][mm]= 0.022670; } 
	  if(m==6 && mm == 19) {     tw0p[m][mm]= 1.065197; } 
	  if(m==6 && mm == 20) {     tw0p[m][mm]= 2.004001; } 
	  if(m==6 && mm == 21) {     tw0p[m][mm]= 0.183222; } 
	  if(m==6 && mm == 23) {     tw0p[m][mm]= 0.876939; } 
	  if(m==6 && mm == 24) {     tw0p[m][mm]= 0.217753; } 
	  if(m==6 && mm == 25) {     tw0p[m][mm]= 0.438963; } 
	  if(m==6 && mm == 26) {     tw0p[m][mm]= 0.801502; } 
	  if(m==6 && mm == 27) {     tw0p[m][mm]= 0.127828; } 
	  if(m==6 && mm == 28) {     tw0p[m][mm]= 0.660601; } 
	  if(m==6 && mm == 29) {     tw0p[m][mm]= 0.800524; } 
	  if(m==6 && mm == 30) {     tw0p[m][mm]= 0.462497; } 
	  if(m==6 && mm == 31) {     tw0p[m][mm]= 1.280504; } 
	  if(m==6 && mm == 32) {     tw0p[m][mm]= 0.025648; } 
	  if(m==6 && mm == 33) {     tw0p[m][mm]= 0.323617; } 
	  if(m==6 && mm == 34) {     tw0p[m][mm]= 1.046508; } 
	  if(m==6 && mm == 35) {     tw0p[m][mm]= 0.458566; } 
	  if(m==6 && mm == 1) {     tw1p[m][mm]= 0.693136; } 
	  if(m==6 && mm == 2) {     tw1p[m][mm]= 0.122077; } 
	  if(m==6 && mm == 4) {     tw1p[m][mm]= 0.456777; } 
	  if(m==6 && mm == 5) {     tw1p[m][mm]= 0.405630; } 
	  if(m==6 && mm == 6) {     tw1p[m][mm]= 0.011063; } 
	  if(m==6 && mm == 7) {     tw1p[m][mm]= 0.332400; } 
	  if(m==6 && mm == 8) {     tw1p[m][mm]= 0.317946; } 
	  if(m==6 && mm == 20) {     tw1p[m][mm]= 0.365687; } 
	  if(m==6 && mm == 21) {     tw1p[m][mm]= 0.304944; } 
	  if(m==6 && mm == 22) {     tw1p[m][mm]= 0.398336; } 
	  if(m==6 && mm == 23) {     tw1p[m][mm]= 0.531535; } 
	  if(m==6 && mm == 28) {     tw1p[m][mm]= 0.204020; } 
	  if(m==6 && mm == 30) {     tw1p[m][mm]= 0.581022; } 
	  if(m==6 && mm == 31) {     tw1p[m][mm]= 0.121868; } 
	  if(m==6 && mm == 6) {     tw2p[m][mm]= 0.707508; } 
	  if(m==6 && mm == 8) {     tw2p[m][mm]= 0.953757; } 
	  if(m==6 && mm == 9) {     tw2p[m][mm]= 0.081220; } 
	  if(m==6 && mm == 10) {     tw2p[m][mm]= 0.115886; } 
	  if(m==6 && mm == 11) {     tw2p[m][mm]= 0.143236; } 
	  if(m==6 && mm == 12) {     tw2p[m][mm]= 1.096544; } 
	  if(m==6 && mm == 15) {     tw2p[m][mm]= 1.326122; } 
	  if(m==6 && mm == 16) {     tw2p[m][mm]= 0.553681; } 
	  if(m==6 && mm == 17) {     tw2p[m][mm]= 0.602866; } 
	  if(m==6 && mm == 19) {     tw2p[m][mm]= 0.649440; } 
	  if(m==6 && mm == 20) {     tw2p[m][mm]= 0.578586; } 
	  if(m==6 && mm == 10) {     tw3p[m][mm]= 0.499877; } 
	  if(m==6 && mm == 11) {     tw3p[m][mm]= 0.064020; } 
	  if(m==6 && mm == 26) {     tw3p[m][mm]= 0.267510; } 
	  if(m==6 && mm == 6) {     te2p[m][mm]= 0.814706; } 
	  if(m==6 && mm == 7) {     te2p[m][mm]= 1.361584; } 
	  if(m==6 && mm == 8) {     te2p[m][mm]= 1.788419; } 
	  if(m==6 && mm == 10) {     te2p[m][mm]= 1.537613; } 
	  if(m==6 && mm == 14) {     te2p[m][mm]= 0.770645; } 
	  if(m==6 && mm == 15) {     te2p[m][mm]= 0.665169; } 
	  if(m==6 && mm == 17) {     te2p[m][mm]= 0.557904; } 
	  if(m==6 && mm == 18) {     te2p[m][mm]= 0.428015; } 
	  if(m==6 && mm == 19) {     te2p[m][mm]= 0.197090; } 
	  if(m==6 && mm == 20) {     te2p[m][mm]= 0.331234; } 
	  if(m==6 && mm == 21) {     te2p[m][mm]= 0.912402; } 
	  if(m==6 && mm == 22) {     te2p[m][mm]= 0.377438; } 
	  if(m==6 && mm == 27) {     te2p[m][mm]= 0.350984; } 
	  if(m==6 && mm == 28) {     te2p[m][mm]= 0.359815; } 
	  if(m==6 && mm == 29) {     te2p[m][mm]= 0.917312; } 
	  if(m==6 && mm == 30) {     te2p[m][mm]= 0.598196; } 
	  if(m==6 && mm == 31) {     te2p[m][mm]= 0.698038; } 
	  if(m==6 && mm == 5) {     te3p[m][mm]= 0.460388; } 
	  if(m==6 && mm == 7) {     te3p[m][mm]= 0.437655; } 
	  if(m==6 && mm == 8) {     te3p[m][mm]= 0.126248; } 
	  if(m==6 && mm == 9) {     te3p[m][mm]= 0.061241; } 
	  if(m==6 && mm == 10) {     te3p[m][mm]= 0.372659; } 
	  if(m==6 && mm == 11) {     te3p[m][mm]= 0.035999; } 
	  if(m==6 && mm == 12) {     te3p[m][mm]= 0.896012; } 
	  if(m==6 && mm == 13) {     te3p[m][mm]= 0.359772; } 
	  if(m==6 && mm == 14) {     te3p[m][mm]= 0.216902; } 
	  if(m==6 && mm == 15) {     te3p[m][mm]= 0.918083; } 
	  if(m==6 && mm == 16) {     te3p[m][mm]= 1.433206; } 
	  if(m==6 && mm == 17) {     te3p[m][mm]= 1.365707; } 
	  if(m==6 && mm == 18) {     te3p[m][mm]= 0.113915; } 
	  if(m==6 && mm == 20) {     te3p[m][mm]= 1.293124; } 
	  if(m==6 && mm == 22) {     te3p[m][mm]= 0.366103; } 
	  if(m==6 && mm == 2) {     tw0n[m][mm]= 0.434844; } 
	  if(m==6 && mm == 3) {     tw0n[m][mm]= 0.193598; } 
	  if(m==6 && mm == 4) {     tw0n[m][mm]= 0.532020; } 
	  if(m==6 && mm == 5) {     tw0n[m][mm]= 0.352543; } 
	  if(m==6 && mm == 12) {     tw0n[m][mm]= 2.086629; } 
	  if(m==6 && mm == 13) {     tw0n[m][mm]= 0.942123; } 
	  if(m==6 && mm == 14) {     tw0n[m][mm]= 0.076933; } 
	  if(m==6 && mm == 15) {     tw0n[m][mm]= 0.258225; } 
	  if(m==6 && mm == 16) {     tw0n[m][mm]= 0.208348; } 
	  if(m==6 && mm == 17) {     tw0n[m][mm]= 0.914285; } 
	  if(m==6 && mm == 19) {     tw0n[m][mm]= 1.269902; } 
	  if(m==6 && mm == 20) {     tw0n[m][mm]= 1.944340; } 
	  if(m==6 && mm == 23) {     tw0n[m][mm]= 0.239627; } 
	  if(m==6 && mm == 25) {     tw0n[m][mm]= 0.196157; } 
	  if(m==6 && mm == 26) {     tw0n[m][mm]= 0.536080; } 
	  if(m==6 && mm == 27) {     tw0n[m][mm]= 0.478886; } 
	  if(m==6 && mm == 28) {     tw0n[m][mm]= 0.512069; } 
	  if(m==6 && mm == 29) {     tw0n[m][mm]= 0.805523; } 
	  if(m==6 && mm == 30) {     tw0n[m][mm]= 0.518736; } 
	  if(m==6 && mm == 7) {     tw1n[m][mm]= 0.369675; } 
	  if(m==6 && mm == 8) {     tw1n[m][mm]= 0.359606; } 
	  if(m==6 && mm == 9) {     tw1n[m][mm]= 0.033576; } 
	  if(m==6 && mm == 10) {     tw1n[m][mm]= 0.080412; } 
	  if(m==6 && mm == 11) {     tw1n[m][mm]= 0.095347; } 
	  if(m==6 && mm == 12) {     tw1n[m][mm]= 0.867808; } 
	  if(m==6 && mm == 14) {     tw1n[m][mm]= 0.210962; } 
	  if(m==6 && mm == 15) {     tw1n[m][mm]= 0.625621; } 
	  if(m==6 && mm == 16) {     tw1n[m][mm]= 0.574807; } 
	  if(m==6 && mm == 17) {     tw1n[m][mm]= 0.026695; } 
	  if(m==6 && mm == 18) {     tw1n[m][mm]= 0.619531; } 
	  if(m==6 && mm == 19) {     tw1n[m][mm]= 0.577923; } 
	  if(m==6 && mm == 20) {     tw1n[m][mm]= 0.262598; } 
	  if(m==6 && mm == 21) {     tw1n[m][mm]= 0.169301; } 
	  if(m==6 && mm == 22) {     tw1n[m][mm]= 0.169729; } 
	  if(m==6 && mm == 23) {     tw1n[m][mm]= 0.279800; } 
	  if(m==6 && mm == 30) {     tw1n[m][mm]= 0.560671; } 
	  if(m==6 && mm == 0) {     tw2n[m][mm]= 0.525594; } 
	  if(m==6 && mm == 1) {     tw2n[m][mm]= 0.136287; } 
	  if(m==6 && mm == 2) {     tw2n[m][mm]= 0.237737; } 
	  if(m==6 && mm == 4) {     tw2n[m][mm]= 0.652576; } 
	  if(m==6 && mm == 6) {     tw2n[m][mm]= 0.400354; } 
	  if(m==6 && mm == 7) {     tw2n[m][mm]= 0.006961; } 
	  if(m==6 && mm == 16) {     tw2n[m][mm]= 0.549939; } 
	  if(m==6 && mm == 17) {     tw2n[m][mm]= 0.752933; } 
	  if(m==6 && mm == 18) {     tw2n[m][mm]= 0.140683; } 
	  if(m==6 && mm == 19) {     tw2n[m][mm]= 0.783061; } 
	  if(m==6 && mm == 20) {     tw2n[m][mm]= 0.617562; } 
	  if(m==6 && mm == 21) {     tw2n[m][mm]= 0.869247; } 
	  if(m==6 && mm == 22) {     tw2n[m][mm]= 0.492570; } 
	  if(m==6 && mm == 23) {     tw2n[m][mm]= 0.357291; } 
	  if(m==6 && mm == 10) {     tw3n[m][mm]= 0.405771; } 
	  if(m==6 && mm == 11) {     tw3n[m][mm]= 0.018532; } 
	  if(m==6 && mm == 26) {     tw3n[m][mm]= 0.042637; } 
	  if(m==6 && mm == 6) {     te2n[m][mm]= 0.637203; } 
	  if(m==6 && mm == 7) {     te2n[m][mm]= 1.299567; } 
	  if(m==6 && mm == 8) {     te2n[m][mm]= 1.774836; } 
	  if(m==6 && mm == 9) {     te2n[m][mm]= 2.275608; } 
	  if(m==6 && mm == 10) {     te2n[m][mm]= 1.533321; } 
	  if(m==6 && mm == 14) {     te2n[m][mm]= 0.734459; } 
	  if(m==6 && mm == 15) {     te2n[m][mm]= 0.236879; } 
	  if(m==6 && mm == 17) {     te2n[m][mm]= 0.943776; } 
	  if(m==6 && mm == 19) {     te2n[m][mm]= 0.330151; } 
	  if(m==6 && mm == 20) {     te2n[m][mm]= 0.540324; } 
	  if(m==6 && mm == 21) {     te2n[m][mm]= 1.227062; } 
	  if(m==6 && mm == 22) {     te2n[m][mm]= 0.295072; } 
	  if(m==6 && mm == 27) {     te2n[m][mm]= 0.633185; } 
	  if(m==6 && mm == 28) {     te2n[m][mm]= 0.582643; } 
	  if(m==6 && mm == 29) {     te2n[m][mm]= 1.278343; } 
	  if(m==6 && mm == 30) {     te2n[m][mm]= 0.247717; } 
	  if(m==6 && mm == 31) {     te2n[m][mm]= 0.795711; } 
	  if(m==6 && mm == 33) {     te2n[m][mm]= 0.456240; } 
	  if(m==6 && mm == 0) {     te3n[m][mm]= 0.147690; } 
	  if(m==6 && mm == 1) {     te3n[m][mm]= 0.377176; } 
	  if(m==6 && mm == 2) {     te3n[m][mm]= 0.093743; } 
	  if(m==6 && mm == 4) {     te3n[m][mm]= 0.372799; } 
	  if(m==6 && mm == 5) {     te3n[m][mm]= 0.536116; } 
	  if(m==6 && mm == 16) {     te3n[m][mm]= 2.346087; } 
	  if(m==6 && mm == 17) {     te3n[m][mm]= 1.215276; } 
	  if(m==6 && mm == 18) {     te3n[m][mm]= 0.156675; } 
	  if(m==6 && mm == 20) {     te3n[m][mm]= 3.213994; } 
	  if(m==6 && mm == 21) {     te3n[m][mm]= 0.988496; } 
	  if(m==6 && mm == 22) {     te3n[m][mm]= 0.470710; } 
	  if(m==6 && mm == 23) {     te3n[m][mm]= 0.011924; } 
	  if(m==6 && mm == 24) {     te3n[m][mm]= 0.110316; } 
	  if(m==6 && mm == 25) {     te3n[m][mm]= 0.399688; } 
	  if(m==6 && mm == 26) {     te3n[m][mm]= 0.924842; } 
	  if(m==6 && mm == 27) {     te3n[m][mm]= 0.942601; } 
	  if(m==6 && mm == 28) {     te3n[m][mm]= 1.534280; } 
	  if(m==6 && mm == 29) {     te3n[m][mm]= 0.457553; } 
	  if(m==6 && mm == 30) {     te3n[m][mm]= 0.420561; } 
	  if(m==6 && mm == 31) {     te3n[m][mm]= 0.384549; } 
	  if(m==6 && mm == 32) {     te3n[m][mm]= 0.315693; } 
	  if(m==6 && mm == 33) {     te3n[m][mm]= 0.453972; } 
	  if(m==6 && mm == 34) {     te3n[m][mm]= 0.685835; } 
	  if(m==7 && mm == 13) {     tw0p[m][mm]= 1.046883; } 
	  if(m==7 && mm == 14) {     tw0p[m][mm]= 0.255127; } 
	  if(m==7 && mm == 15) {     tw0p[m][mm]= 0.299925; } 
	  if(m==7 && mm == 16) {     tw0p[m][mm]= 0.084605; } 
	  if(m==7 && mm == 17) {     tw0p[m][mm]= 0.688433; } 
	  if(m==7 && mm == 18) {     tw0p[m][mm]= 0.353533; } 
	  if(m==7 && mm == 19) {     tw0p[m][mm]= 0.426888; } 
	  if(m==7 && mm == 20) {     tw0p[m][mm]= 0.499689; } 
	  if(m==7 && mm == 21) {     tw0p[m][mm]= 0.334152; } 
	  if(m==7 && mm == 22) {     tw0p[m][mm]= 0.513303; } 
	  if(m==7 && mm == 23) {     tw0p[m][mm]= 0.232435; } 
	  if(m==7 && mm == 24) {     tw0p[m][mm]= 0.340849; } 
	  if(m==7 && mm == 25) {     tw0p[m][mm]= 0.574184; } 
	  if(m==7 && mm == 26) {     tw0p[m][mm]= 0.519965; } 
	  if(m==7 && mm == 27) {     tw0p[m][mm]= 0.947783; } 
	  if(m==7 && mm == 28) {     tw0p[m][mm]= 0.511062; } 
	  if(m==7 && mm == 29) {     tw0p[m][mm]= 0.287141; } 
	  if(m==7 && mm == 30) {     tw0p[m][mm]= 0.732307; } 
	  if(m==7 && mm == 31) {     tw0p[m][mm]= 1.024166; } 
	  if(m==7 && mm == 32) {     tw0p[m][mm]= 0.158252; } 
	  if(m==7 && mm == 33) {     tw0p[m][mm]= 0.036085; } 
	  if(m==7 && mm == 34) {     tw0p[m][mm]= 0.522593; } 
	  if(m==7 && mm == 35) {     tw0p[m][mm]= 0.435875; } 
	  if(m==7 && mm == 0) {     tw1p[m][mm]= 0.577354; } 
	  if(m==7 && mm == 1) {     tw1p[m][mm]= 0.339017; } 
	  if(m==7 && mm == 2) {     tw1p[m][mm]= 0.412694; } 
	  if(m==7 && mm == 3) {     tw1p[m][mm]= 0.425393; } 
	  if(m==7 && mm == 4) {     tw1p[m][mm]= 0.132993; } 
	  if(m==7 && mm == 5) {     tw1p[m][mm]= 0.293033; } 
	  if(m==7 && mm == 6) {     tw1p[m][mm]= 0.451007; } 
	  if(m==7 && mm == 7) {     tw1p[m][mm]= 0.264564; } 
	  if(m==7 && mm == 8) {     tw1p[m][mm]= 0.377611; } 
	  if(m==7 && mm == 20) {     tw1p[m][mm]= 0.391966; } 
	  if(m==7 && mm == 21) {     tw1p[m][mm]= 1.037475; } 
	  if(m==7 && mm == 22) {     tw1p[m][mm]= 0.988340; } 
	  if(m==7 && mm == 28) {     tw1p[m][mm]= 1.288428; } 
	  if(m==7 && mm == 29) {     tw1p[m][mm]= 0.307982; } 
	  if(m==7 && mm == 30) {     tw1p[m][mm]= 1.885375; } 
	  if(m==7 && mm == 33) {     tw1p[m][mm]= 0.438564; } 
	  if(m==7 && mm == 34) {     tw1p[m][mm]= 0.593732; } 
	  if(m==7 && mm == 35) {     tw1p[m][mm]= 0.720021; } 
	  if(m==7 && mm == 6) {     tw2p[m][mm]= 0.906790; } 
	  if(m==7 && mm == 7) {     tw2p[m][mm]= 0.110021; } 
	  if(m==7 && mm == 9) {     tw2p[m][mm]= 0.578950; } 
	  if(m==7 && mm == 10) {     tw2p[m][mm]= 0.025084; } 
	  if(m==7 && mm == 11) {     tw2p[m][mm]= 0.399026; } 
	  if(m==7 && mm == 14) {     tw2p[m][mm]= 0.311292; } 
	  if(m==7 && mm == 15) {     tw2p[m][mm]= 0.387840; } 
	  if(m==7 && mm == 16) {     tw2p[m][mm]= 0.367586; } 
	  if(m==7 && mm == 17) {     tw2p[m][mm]= 0.382220; } 
	  if(m==7 && mm == 18) {     tw2p[m][mm]= 0.598458; } 
	  if(m==7 && mm == 19) {     tw2p[m][mm]= 0.739017; } 
	  if(m==7 && mm == 20) {     tw2p[m][mm]= 0.745768; } 
	  if(m==7 && mm == 22) {     tw2p[m][mm]= 1.311525; } 
	  if(m==7 && mm == 1) {     tw3p[m][mm]= 0.966891; } 
	  if(m==7 && mm == 4) {     tw3p[m][mm]= 0.051156; } 
	  if(m==7 && mm == 6) {     tw3p[m][mm]= 0.548000; } 
	  if(m==7 && mm == 9) {     tw3p[m][mm]= 0.274163; } 
	  if(m==7 && mm == 12) {     tw3p[m][mm]= 0.717330; } 
	  if(m==7 && mm == 24) {     tw3p[m][mm]= 0.162116; } 
	  if(m==7 && mm == 25) {     tw3p[m][mm]= 0.063995; } 
	  if(m==7 && mm == 34) {     tw3p[m][mm]= 0.237994; } 
	  if(m==7 && mm == 6) {     te2p[m][mm]= 1.509279; } 
	  if(m==7 && mm == 7) {     te2p[m][mm]= 0.541556; } 
	  if(m==7 && mm == 8) {     te2p[m][mm]= 1.364444; } 
	  if(m==7 && mm == 9) {     te2p[m][mm]= 2.038008; } 
	  if(m==7 && mm == 10) {     te2p[m][mm]= 2.532215; } 
	  if(m==7 && mm == 11) {     te2p[m][mm]= 1.014179; } 
	  if(m==7 && mm == 14) {     te2p[m][mm]= 0.012693; } 
	  if(m==7 && mm == 15) {     te2p[m][mm]= 0.487401; } 
	  if(m==7 && mm == 16) {     te2p[m][mm]= 0.367247; } 
	  if(m==7 && mm == 19) {     te2p[m][mm]= 1.110133; } 
	  if(m==7 && mm == 20) {     te2p[m][mm]= 0.721915; } 
	  if(m==7 && mm == 21) {     te2p[m][mm]= 0.215813; } 
	  if(m==7 && mm == 22) {     te2p[m][mm]= 0.606310; } 
	  if(m==7 && mm == 23) {     te2p[m][mm]= 0.221935; } 
	  if(m==7 && mm == 24) {     te2p[m][mm]= 0.133700; } 
	  if(m==7 && mm == 26) {     te2p[m][mm]= 0.696400; } 
	  if(m==7 && mm == 28) {     te2p[m][mm]= 0.031687; } 
	  if(m==7 && mm == 29) {     te2p[m][mm]= 0.508708; } 
	  if(m==7 && mm == 30) {     te2p[m][mm]= 0.744025; } 
	  if(m==7 && mm == 5) {     te3p[m][mm]= 0.339540; } 
	  if(m==7 && mm == 6) {     te3p[m][mm]= 0.406438; } 
	  if(m==7 && mm == 7) {     te3p[m][mm]= 0.077164; } 
	  if(m==7 && mm == 8) {     te3p[m][mm]= 0.187696; } 
	  if(m==7 && mm == 9) {     te3p[m][mm]= 0.294604; } 
	  if(m==7 && mm == 11) {     te3p[m][mm]= 0.212490; } 
	  if(m==7 && mm == 12) {     te3p[m][mm]= 0.397827; } 
	  if(m==7 && mm == 13) {     te3p[m][mm]= 0.391024; } 
	  if(m==7 && mm == 14) {     te3p[m][mm]= 0.638039; } 
	  if(m==7 && mm == 15) {     te3p[m][mm]= 0.537467; } 
	  if(m==7 && mm == 16) {     te3p[m][mm]= 0.575684; } 
	  if(m==7 && mm == 17) {     te3p[m][mm]= 0.272160; } 
	  if(m==7 && mm == 18) {     te3p[m][mm]= 0.048702; } 
	  if(m==7 && mm == 19) {     te3p[m][mm]= 1.607892; } 
	  if(m==7 && mm == 20) {     te3p[m][mm]= 0.746457; } 
	  if(m==7 && mm == 21) {     te3p[m][mm]= 0.730423; } 
	  if(m==7 && mm == 22) {     te3p[m][mm]= 0.166937; } 
	  if(m==7 && mm == 23) {     te3p[m][mm]= 1.851016; } 
	  if(m==7 && mm == 0) {     tw0n[m][mm]= 0.121454; } 
	  if(m==7 && mm == 1) {     tw0n[m][mm]= 0.177116; } 
	  if(m==7 && mm == 3) {     tw0n[m][mm]= 0.191832; } 
	  if(m==7 && mm == 4) {     tw0n[m][mm]= 0.070166; } 
	  if(m==7 && mm == 5) {     tw0n[m][mm]= 0.035439; } 
	  if(m==7 && mm == 12) {     tw0n[m][mm]= 0.159454; } 
	  if(m==7 && mm == 13) {     tw0n[m][mm]= 1.290361; } 
	  if(m==7 && mm == 14) {     tw0n[m][mm]= 0.201733; } 
	  if(m==7 && mm == 15) {     tw0n[m][mm]= 0.200835; } 
	  if(m==7 && mm == 16) {     tw0n[m][mm]= 0.524740; } 
	  if(m==7 && mm == 17) {     tw0n[m][mm]= 0.277430; } 
	  if(m==7 && mm == 18) {     tw0n[m][mm]= 0.395620; } 
	  if(m==7 && mm == 19) {     tw0n[m][mm]= 0.465519; } 
	  if(m==7 && mm == 20) {     tw0n[m][mm]= 0.636168; } 
	  if(m==7 && mm == 21) {     tw0n[m][mm]= 0.377555; } 
	  if(m==7 && mm == 22) {     tw0n[m][mm]= 0.365992; } 
	  if(m==7 && mm == 23) {     tw0n[m][mm]= 0.224541; } 
	  if(m==7 && mm == 24) {     tw0n[m][mm]= 0.169123; } 
	  if(m==7 && mm == 25) {     tw0n[m][mm]= 0.522053; } 
	  if(m==7 && mm == 26) {     tw0n[m][mm]= 0.363900; } 
	  if(m==7 && mm == 27) {     tw0n[m][mm]= 0.699876; } 
	  if(m==7 && mm == 28) {     tw0n[m][mm]= 0.345260; } 
	  if(m==7 && mm == 29) {     tw0n[m][mm]= 0.357167; } 
	  if(m==7 && mm == 7) {     tw1n[m][mm]= 0.399795; } 
	  if(m==7 && mm == 8) {     tw1n[m][mm]= 0.459477; } 
	  if(m==7 && mm == 9) {     tw1n[m][mm]= 0.423784; } 
	  if(m==7 && mm == 10) {     tw1n[m][mm]= 0.265065; } 
	  if(m==7 && mm == 11) {     tw1n[m][mm]= 0.367716; } 
	  if(m==7 && mm == 12) {     tw1n[m][mm]= 0.162998; } 
	  if(m==7 && mm == 13) {     tw1n[m][mm]= 0.603980; } 
	  if(m==7 && mm == 14) {     tw1n[m][mm]= 0.180945; } 
	  if(m==7 && mm == 15) {     tw1n[m][mm]= 0.861545; } 
	  if(m==7 && mm == 16) {     tw1n[m][mm]= 0.446460; } 
	  if(m==7 && mm == 17) {     tw1n[m][mm]= 0.275447; } 
	  if(m==7 && mm == 18) {     tw1n[m][mm]= 1.092042; } 
	  if(m==7 && mm == 19) {     tw1n[m][mm]= 1.126693; } 
	  if(m==7 && mm == 20) {     tw1n[m][mm]= 0.387961; } 
	  if(m==7 && mm == 21) {     tw1n[m][mm]= 1.027471; } 
	  if(m==7 && mm == 22) {     tw1n[m][mm]= 0.945403; } 
	  if(m==7 && mm == 23) {     tw1n[m][mm]= 1.779464; } 
	  if(m==7 && mm == 30) {     tw1n[m][mm]= 1.075691; } 
	  if(m==7 && mm == 33) {     tw1n[m][mm]= 0.063807; } 
	  if(m==7 && mm == 34) {     tw1n[m][mm]= 0.161650; } 
	  if(m==7 && mm == 35) {     tw1n[m][mm]= 0.536697; } 
	  if(m==7 && mm == 0) {     tw2n[m][mm]= 0.481193; } 
	  if(m==7 && mm == 1) {     tw2n[m][mm]= 0.157357; } 
	  if(m==7 && mm == 2) {     tw2n[m][mm]= 0.545965; } 
	  if(m==7 && mm == 3) {     tw2n[m][mm]= 0.022587; } 
	  if(m==7 && mm == 4) {     tw2n[m][mm]= 0.224902; } 
	  if(m==7 && mm == 5) {     tw2n[m][mm]= 0.229835; } 
	  if(m==7 && mm == 6) {     tw2n[m][mm]= 1.089770; } 
	  if(m==7 && mm == 14) {     tw2n[m][mm]= 0.307085; } 
	  if(m==7 && mm == 15) {     tw2n[m][mm]= 0.234498; } 
	  if(m==7 && mm == 16) {     tw2n[m][mm]= 0.307727; } 
	  if(m==7 && mm == 17) {     tw2n[m][mm]= 0.426765; } 
	  if(m==7 && mm == 18) {     tw2n[m][mm]= 0.603921; } 
	  if(m==7 && mm == 19) {     tw2n[m][mm]= 0.584535; } 
	  if(m==7 && mm == 20) {     tw2n[m][mm]= 1.011595; } 
	  if(m==7 && mm == 21) {     tw2n[m][mm]= 0.778633; } 
	  if(m==7 && mm == 22) {     tw2n[m][mm]= 1.278514; } 
	  if(m==7 && mm == 23) {     tw2n[m][mm]= 0.211737; } 
	  if(m==7 && mm == 1) {     tw3n[m][mm]= 0.778631; } 
	  if(m==7 && mm == 6) {     tw3n[m][mm]= 0.472279; } 
	  if(m==7 && mm == 9) {     tw3n[m][mm]= 0.020863; } 
	  if(m==7 && mm == 12) {     tw3n[m][mm]= 0.541772; } 
	  if(m==7 && mm == 6) {     te2n[m][mm]= 0.628394; } 
	  if(m==7 && mm == 7) {     te2n[m][mm]= 0.371930; } 
	  if(m==7 && mm == 8) {     te2n[m][mm]= 1.325637; } 
	  if(m==7 && mm == 9) {     te2n[m][mm]= 2.063841; } 
	  if(m==7 && mm == 10) {     te2n[m][mm]= 2.365383; } 
	  if(m==7 && mm == 11) {     te2n[m][mm]= 1.107384; } 
	  if(m==7 && mm == 13) {     te2n[m][mm]= 0.256253; } 
	  if(m==7 && mm == 14) {     te2n[m][mm]= 0.037452; } 
	  if(m==7 && mm == 15) {     te2n[m][mm]= 0.431232; } 
	  if(m==7 && mm == 16) {     te2n[m][mm]= 0.637963; } 
	  if(m==7 && mm == 19) {     te2n[m][mm]= 0.789812; } 
	  if(m==7 && mm == 21) {     te2n[m][mm]= 0.113437; } 
	  if(m==7 && mm == 22) {     te2n[m][mm]= 0.159982; } 
	  if(m==7 && mm == 23) {     te2n[m][mm]= 0.287869; } 
	  if(m==7 && mm == 25) {     te2n[m][mm]= 0.037321; } 
	  if(m==7 && mm == 26) {     te2n[m][mm]= 0.718529; } 
	  if(m==7 && mm == 28) {     te2n[m][mm]= 0.143509; } 
	  if(m==7 && mm == 29) {     te2n[m][mm]= 0.534384; } 
	  if(m==7 && mm == 30) {     te2n[m][mm]= 0.618016; } 
	  if(m==7 && mm == 32) {     te2n[m][mm]= 0.356523; } 
	  if(m==7 && mm == 35) {     te2n[m][mm]= 1.262435; } 
	  if(m==7 && mm == 0) {     te3n[m][mm]= 0.161833; } 
	  if(m==7 && mm == 1) {     te3n[m][mm]= 0.173583; } 
	  if(m==7 && mm == 5) {     te3n[m][mm]= 0.235342; } 
	  if(m==7 && mm == 6) {     te3n[m][mm]= 0.248587; } 
	  if(m==7 && mm == 14) {     te3n[m][mm]= 1.374356; } 
	  if(m==7 && mm == 15) {     te3n[m][mm]= 0.372088; } 
	  if(m==7 && mm == 16) {     te3n[m][mm]= 0.604534; } 
	  if(m==7 && mm == 17) {     te3n[m][mm]= 0.099420; } 
	  if(m==7 && mm == 18) {     te3n[m][mm]= 0.042381; } 
	  if(m==7 && mm == 20) {     te3n[m][mm]= 0.771734; } 
	  if(m==7 && mm == 22) {     te3n[m][mm]= 1.133642; } 
	  if(m==7 && mm == 23) {     te3n[m][mm]= 1.644219; } 
	  if(m==7 && mm == 24) {     te3n[m][mm]= 0.042691; } 
	  if(m==7 && mm == 25) {     te3n[m][mm]= 0.186833; } 
	  if(m==7 && mm == 26) {     te3n[m][mm]= 0.579168; } 
	  if(m==7 && mm == 28) {     te3n[m][mm]= 1.808058; } 
	  if(m==7 && mm == 29) {     te3n[m][mm]= 0.597138; } 
	  if(m==7 && mm == 30) {     te3n[m][mm]= 0.500195; } 
	  if(m==7 && mm == 31) {     te3n[m][mm]= 0.170493; } 
	  if(m==7 && mm == 32) {     te3n[m][mm]= 0.448817; } 
	  if(m==7 && mm == 33) {     te3n[m][mm]= 0.311709; } 
	  if(m==7 && mm == 34) {     te3n[m][mm]= 0.716370; } 
	  if(m==7 && mm == 35) {     te3n[m][mm]= 0.331251; } 
	  if(m==8 && mm == 8) {     tw0p[m][mm]= 0.025936; } 
	  if(m==8 && mm == 12) {     tw0p[m][mm]= 0.461067; } 
	  if(m==8 && mm == 13) {     tw0p[m][mm]= 0.425531; } 
	  if(m==8 && mm == 14) {     tw0p[m][mm]= 0.298927; } 
	  if(m==8 && mm == 16) {     tw0p[m][mm]= 0.135642; } 
	  if(m==8 && mm == 17) {     tw0p[m][mm]= 0.547980; } 
	  if(m==8 && mm == 18) {     tw0p[m][mm]= 1.149746; } 
	  if(m==8 && mm == 19) {     tw0p[m][mm]= 0.437100; } 
	  if(m==8 && mm == 20) {     tw0p[m][mm]= 0.272951; } 
	  if(m==8 && mm == 22) {     tw0p[m][mm]= 0.193721; } 
	  if(m==8 && mm == 23) {     tw0p[m][mm]= 0.192850; } 
	  if(m==8 && mm == 25) {     tw0p[m][mm]= 0.390182; } 
	  if(m==8 && mm == 26) {     tw0p[m][mm]= 0.128584; } 
	  if(m==8 && mm == 27) {     tw0p[m][mm]= 0.315858; } 
	  if(m==8 && mm == 30) {     tw0p[m][mm]= 0.514166; } 
	  if(m==8 && mm == 31) {     tw0p[m][mm]= 0.631280; } 
	  if(m==8 && mm == 32) {     tw0p[m][mm]= 0.576657; } 
	  if(m==8 && mm == 33) {     tw0p[m][mm]= 0.286576; } 
	  if(m==8 && mm == 34) {     tw0p[m][mm]= 0.267390; } 
	  if(m==8 && mm == 35) {     tw0p[m][mm]= 0.385402; } 
	  if(m==8 && mm == 0) {     tw1p[m][mm]= 0.838907; } 
	  if(m==8 && mm == 1) {     tw1p[m][mm]= 0.217748; } 
	  if(m==8 && mm == 2) {     tw1p[m][mm]= 0.319574; } 
	  if(m==8 && mm == 4) {     tw1p[m][mm]= 0.231891; } 
	  if(m==8 && mm == 6) {     tw1p[m][mm]= 0.194055; } 
	  if(m==8 && mm == 7) {     tw1p[m][mm]= 0.056499; } 
	  if(m==8 && mm == 20) {     tw1p[m][mm]= 0.124539; } 
	  if(m==8 && mm == 21) {     tw1p[m][mm]= 0.360199; } 
	  if(m==8 && mm == 22) {     tw1p[m][mm]= 0.704201; } 
	  if(m==8 && mm == 23) {     tw1p[m][mm]= 0.054316; } 
	  if(m==8 && mm == 28) {     tw1p[m][mm]= 0.552004; } 
	  if(m==8 && mm == 29) {     tw1p[m][mm]= 0.398778; } 
	  if(m==8 && mm == 30) {     tw1p[m][mm]= 0.427762; } 
	  if(m==8 && mm == 32) {     tw1p[m][mm]= 0.581361; } 
	  if(m==8 && mm == 34) {     tw1p[m][mm]= 0.180264; } 
	  if(m==8 && mm == 5) {     tw2p[m][mm]= 0.552470; } 
	  if(m==8 && mm == 8) {     tw2p[m][mm]= 0.289545; } 
	  if(m==8 && mm == 9) {     tw2p[m][mm]= 0.034778; } 
	  if(m==8 && mm == 10) {     tw2p[m][mm]= 0.115234; } 
	  if(m==8 && mm == 12) {     tw2p[m][mm]= 0.460043; } 
	  if(m==8 && mm == 13) {     tw2p[m][mm]= 0.219144; } 
	  if(m==8 && mm == 14) {     tw2p[m][mm]= 0.623108; } 
	  if(m==8 && mm == 15) {     tw2p[m][mm]= 0.055131; } 
	  if(m==8 && mm == 16) {     tw2p[m][mm]= 0.745623; } 
	  if(m==8 && mm == 17) {     tw2p[m][mm]= 0.727310; } 
	  if(m==8 && mm == 18) {     tw2p[m][mm]= 0.165402; } 
	  if(m==8 && mm == 19) {     tw2p[m][mm]= 0.425332; } 
	  if(m==8 && mm == 20) {     tw2p[m][mm]= 0.242358; } 
	  if(m==8 && mm == 21) {     tw2p[m][mm]= 0.290339; } 
	  if(m==8 && mm == 28) {     tw2p[m][mm]= 0.394759; } 
	  if(m==8 && mm == 29) {     tw2p[m][mm]= 0.296502; } 
	  if(m==8 && mm == 30) {     tw2p[m][mm]= 0.979147; } 
	  if(m==8 && mm == 31) {     tw2p[m][mm]= 0.157649; } 
	  if(m==8 && mm == 34) {     tw2p[m][mm]= 0.203596; } 
	  if(m==8 && mm == 35) {     tw2p[m][mm]= 0.379474; } 
	  if(m==8 && mm == 6) {     tw3p[m][mm]= 0.624730; } 
	  if(m==8 && mm == 7) {     tw3p[m][mm]= 0.851736; } 
	  if(m==8 && mm == 8) {     tw3p[m][mm]= 0.172190; } 
	  if(m==8 && mm == 9) {     tw3p[m][mm]= 1.027003; } 
	  if(m==8 && mm == 10) {     tw3p[m][mm]= 0.572411; } 
	  if(m==8 && mm == 11) {     tw3p[m][mm]= 0.909536; } 
	  if(m==8 && mm == 13) {     tw3p[m][mm]= 0.162044; } 
	  if(m==8 && mm == 14) {     tw3p[m][mm]= 0.825900; } 
	  if(m==8 && mm == 15) {     tw3p[m][mm]= 0.665198; } 
	  if(m==8 && mm == 16) {     tw3p[m][mm]= 2.785617; } 
	  if(m==8 && mm == 17) {     tw3p[m][mm]= 0.720370; } 
	  if(m==8 && mm == 18) {     tw3p[m][mm]= 1.392265; } 
	  if(m==8 && mm == 19) {     tw3p[m][mm]= 0.256292; } 
	  if(m==8 && mm == 20) {     tw3p[m][mm]= 0.280046; } 
	  if(m==8 && mm == 21) {     tw3p[m][mm]= 2.005246; } 
	  if(m==8 && mm == 23) {     tw3p[m][mm]= 0.516278; } 
	  if(m==8 && mm == 33) {     tw3p[m][mm]= 2.928577; } 
	  if(m==8 && mm == 1) {     te2p[m][mm]= 0.173229; } 
	  if(m==8 && mm == 2) {     te2p[m][mm]= 0.558138; } 
	  if(m==8 && mm == 3) {     te2p[m][mm]= 0.092384; } 
	  if(m==8 && mm == 4) {     te2p[m][mm]= 0.959758; } 
	  if(m==8 && mm == 5) {     te2p[m][mm]= 0.568401; } 
	  if(m==8 && mm == 6) {     te2p[m][mm]= 1.075830; } 
	  if(m==8 && mm == 7) {     te2p[m][mm]= 0.819692; } 
	  if(m==8 && mm == 8) {     te2p[m][mm]= 0.054592; } 
	  if(m==8 && mm == 9) {     te2p[m][mm]= 1.003804; } 
	  if(m==8 && mm == 10) {     te2p[m][mm]= 0.616318; } 
	  if(m==8 && mm == 11) {     te2p[m][mm]= 0.743237; } 
	  if(m==8 && mm == 12) {     te2p[m][mm]= 0.168765; } 
	  if(m==8 && mm == 14) {     te2p[m][mm]= 0.073389; } 
	  if(m==8 && mm == 15) {     te2p[m][mm]= 0.232418; } 
	  if(m==8 && mm == 18) {     te2p[m][mm]= 0.429354; } 
	  if(m==8 && mm == 19) {     te2p[m][mm]= 0.310418; } 
	  if(m==8 && mm == 21) {     te2p[m][mm]= 0.421390; } 
	  if(m==8 && mm == 22) {     te2p[m][mm]= 0.119816; } 
	  if(m==8 && mm == 23) {     te2p[m][mm]= 0.521928; } 
	  if(m==8 && mm == 24) {     te2p[m][mm]= 0.125569; } 
	  if(m==8 && mm == 25) {     te2p[m][mm]= 0.058737; } 
	  if(m==8 && mm == 28) {     te2p[m][mm]= 0.229914; } 
	  if(m==8 && mm == 29) {     te2p[m][mm]= 0.750600; } 
	  if(m==8 && mm == 12) {     te3p[m][mm]= 0.105087; } 
	  if(m==8 && mm == 13) {     te3p[m][mm]= 0.659949; } 
	  if(m==8 && mm == 14) {     te3p[m][mm]= 0.501579; } 
	  if(m==8 && mm == 15) {     te3p[m][mm]= 0.363329; } 
	  if(m==8 && mm == 16) {     te3p[m][mm]= 0.385269; } 
	  if(m==8 && mm == 17) {     te3p[m][mm]= 0.258182; } 
	  if(m==8 && mm == 18) {     te3p[m][mm]= 0.589653; } 
	  if(m==8 && mm == 19) {     te3p[m][mm]= 0.288504; } 
	  if(m==8 && mm == 21) {     te3p[m][mm]= 0.261546; } 
	  if(m==8 && mm == 22) {     te3p[m][mm]= 0.153092; } 
	  if(m==8 && mm == 0) {     tw0n[m][mm]= 0.660169; } 
	  if(m==8 && mm == 4) {     tw0n[m][mm]= 0.090050; } 
	  if(m==8 && mm == 5) {     tw0n[m][mm]= 0.328031; } 
	  if(m==8 && mm == 12) {     tw0n[m][mm]= 0.815061; } 
	  if(m==8 && mm == 13) {     tw0n[m][mm]= 0.234704; } 
	  if(m==8 && mm == 14) {     tw0n[m][mm]= 0.304508; } 
	  if(m==8 && mm == 16) {     tw0n[m][mm]= 0.138814; } 
	  if(m==8 && mm == 17) {     tw0n[m][mm]= 0.451234; } 
	  if(m==8 && mm == 18) {     tw0n[m][mm]= 1.383431; } 
	  if(m==8 && mm == 19) {     tw0n[m][mm]= 0.421279; } 
	  if(m==8 && mm == 20) {     tw0n[m][mm]= 0.167124; } 
	  if(m==8 && mm == 22) {     tw0n[m][mm]= 0.266615; } 
	  if(m==8 && mm == 23) {     tw0n[m][mm]= 0.116321; } 
	  if(m==8 && mm == 24) {     tw0n[m][mm]= 0.420806; } 
	  if(m==8 && mm == 25) {     tw0n[m][mm]= 1.148396; } 
	  if(m==8 && mm == 26) {     tw0n[m][mm]= 0.107015; } 
	  if(m==8 && mm == 27) {     tw0n[m][mm]= 0.137869; } 
	  if(m==8 && mm == 29) {     tw0n[m][mm]= 0.050225; } 
	  if(m==8 && mm == 30) {     tw0n[m][mm]= 0.704629; } 
	  if(m==8 && mm == 6) {     tw1n[m][mm]= 0.301732; } 
	  if(m==8 && mm == 7) {     tw1n[m][mm]= 0.339825; } 
	  if(m==8 && mm == 9) {     tw1n[m][mm]= 0.129843; } 
	  if(m==8 && mm == 12) {     tw1n[m][mm]= 0.339716; } 
	  if(m==8 && mm == 13) {     tw1n[m][mm]= 0.025360; } 
	  if(m==8 && mm == 15) {     tw1n[m][mm]= 0.111348; } 
	  if(m==8 && mm == 16) {     tw1n[m][mm]= 0.285074; } 
	  if(m==8 && mm == 18) {     tw1n[m][mm]= 0.153145; } 
	  if(m==8 && mm == 19) {     tw1n[m][mm]= 0.065934; } 
	  if(m==8 && mm == 21) {     tw1n[m][mm]= 0.129200; } 
	  if(m==8 && mm == 22) {     tw1n[m][mm]= 0.503430; } 
	  if(m==8 && mm == 23) {     tw1n[m][mm]= 0.069655; } 
	  if(m==8 && mm == 32) {     tw1n[m][mm]= 0.056651; } 
	  if(m==8 && mm == 34) {     tw1n[m][mm]= 0.071173; } 
	  if(m==8 && mm == 0) {     tw2n[m][mm]= 0.410051; } 
	  if(m==8 && mm == 2) {     tw2n[m][mm]= 0.182025; } 
	  if(m==8 && mm == 3) {     tw2n[m][mm]= 0.969054; } 
	  if(m==8 && mm == 4) {     tw2n[m][mm]= 0.287175; } 
	  if(m==8 && mm == 5) {     tw2n[m][mm]= 0.496274; } 
	  if(m==8 && mm == 6) {     tw2n[m][mm]= 0.671988; } 
	  if(m==8 && mm == 14) {     tw2n[m][mm]= 0.840655; } 
	  if(m==8 && mm == 15) {     tw2n[m][mm]= 0.216379; } 
	  if(m==8 && mm == 16) {     tw2n[m][mm]= 0.319105; } 
	  if(m==8 && mm == 17) {     tw2n[m][mm]= 0.692673; } 
	  if(m==8 && mm == 18) {     tw2n[m][mm]= 0.217695; } 
	  if(m==8 && mm == 19) {     tw2n[m][mm]= 0.395567; } 
	  if(m==8 && mm == 20) {     tw2n[m][mm]= 0.123767; } 
	  if(m==8 && mm == 21) {     tw2n[m][mm]= 0.436393; } 
	  if(m==8 && mm == 22) {     tw2n[m][mm]= 0.621444; } 
	  if(m==8 && mm == 23) {     tw2n[m][mm]= 0.547059; } 
	  if(m==8 && mm == 24) {     tw2n[m][mm]= 0.187659; } 
	  if(m==8 && mm == 25) {     tw2n[m][mm]= 0.037619; } 
	  if(m==8 && mm == 26) {     tw2n[m][mm]= 0.091983; } 
	  if(m==8 && mm == 27) {     tw2n[m][mm]= 0.222873; } 
	  if(m==8 && mm == 29) {     tw2n[m][mm]= 0.049851; } 
	  if(m==8 && mm == 30) {     tw2n[m][mm]= 0.505601; } 
	  if(m==8 && mm == 31) {     tw2n[m][mm]= 0.013584; } 
	  if(m==8 && mm == 32) {     tw2n[m][mm]= 0.081938; } 
	  if(m==8 && mm == 35) {     tw2n[m][mm]= 0.176888; } 
	  if(m==8 && mm == 6) {     tw3n[m][mm]= 0.502109; } 
	  if(m==8 && mm == 7) {     tw3n[m][mm]= 0.982505; } 
	  if(m==8 && mm == 8) {     tw3n[m][mm]= 0.164509; } 
	  if(m==8 && mm == 9) {     tw3n[m][mm]= 0.667634; } 
	  if(m==8 && mm == 10) {     tw3n[m][mm]= 0.502747; } 
	  if(m==8 && mm == 11) {     tw3n[m][mm]= 0.847977; } 
	  if(m==8 && mm == 13) {     tw3n[m][mm]= 0.151449; } 
	  if(m==8 && mm == 14) {     tw3n[m][mm]= 0.939237; } 
	  if(m==8 && mm == 15) {     tw3n[m][mm]= 0.375399; } 
	  if(m==8 && mm == 17) {     tw3n[m][mm]= 0.736538; } 
	  if(m==8 && mm == 18) {     tw3n[m][mm]= 1.288854; } 
	  if(m==8 && mm == 19) {     tw3n[m][mm]= 0.150441; } 
	  if(m==8 && mm == 20) {     tw3n[m][mm]= 0.159658; } 
	  if(m==8 && mm == 21) {     tw3n[m][mm]= 2.808962; } 
	  if(m==8 && mm == 23) {     tw3n[m][mm]= 0.338669; } 
	  if(m==8 && mm == 6) {     te2n[m][mm]= 1.505266; } 
	  if(m==8 && mm == 7) {     te2n[m][mm]= 1.098228; } 
	  if(m==8 && mm == 9) {     te2n[m][mm]= 0.458310; } 
	  if(m==8 && mm == 10) {     te2n[m][mm]= 0.402592; } 
	  if(m==8 && mm == 11) {     te2n[m][mm]= 0.482047; } 
	  if(m==8 && mm == 12) {     te2n[m][mm]= 0.031948; } 
	  if(m==8 && mm == 14) {     te2n[m][mm]= 0.127341; } 
	  if(m==8 && mm == 15) {     te2n[m][mm]= 0.410963; } 
	  if(m==8 && mm == 18) {     te2n[m][mm]= 0.403544; } 
	  if(m==8 && mm == 19) {     te2n[m][mm]= 0.106135; } 
	  if(m==8 && mm == 20) {     te2n[m][mm]= 0.109393; } 
	  if(m==8 && mm == 21) {     te2n[m][mm]= 0.374109; } 
	  if(m==8 && mm == 22) {     te2n[m][mm]= 0.178839; } 
	  if(m==8 && mm == 23) {     te2n[m][mm]= 0.619652; } 
	  if(m==8 && mm == 25) {     te2n[m][mm]= 0.121792; } 
	  if(m==8 && mm == 28) {     te2n[m][mm]= 0.526877; } 
	  if(m==8 && mm == 29) {     te2n[m][mm]= 1.193631; } 
	  if(m==8 && mm == 30) {     te2n[m][mm]= 0.118220; } 
	  if(m==8 && mm == 31) {     te2n[m][mm]= 0.822749; } 
	  if(m==8 && mm == 32) {     te2n[m][mm]= 0.567825; } 
	  if(m==8 && mm == 34) {     te2n[m][mm]= 0.242858; } 
	  if(m==8 && mm == 35) {     te2n[m][mm]= 0.064497; } 
	  if(m==8 && mm == 0) {     te3n[m][mm]= 3.390056; } 
	  if(m==8 && mm == 1) {     te3n[m][mm]= 0.682217; } 
	  if(m==8 && mm == 2) {     te3n[m][mm]= 1.033975; } 
	  if(m==8 && mm == 3) {     te3n[m][mm]= 0.492170; } 
	  if(m==8 && mm == 4) {     te3n[m][mm]= 0.361375; } 
	  if(m==8 && mm == 16) {     te3n[m][mm]= 0.612703; } 
	  if(m==8 && mm == 17) {     te3n[m][mm]= 0.204966; } 
	  if(m==8 && mm == 19) {     te3n[m][mm]= 0.294357; } 
	  if(m==8 && mm == 21) {     te3n[m][mm]= 0.133059; } 
	  if(m==8 && mm == 22) {     te3n[m][mm]= 0.501181; } 
	  if(m==8 && mm == 25) {     te3n[m][mm]= 0.471428; } 
	  if(m==8 && mm == 26) {     te3n[m][mm]= 0.124226; } 
	  if(m==8 && mm == 28) {     te3n[m][mm]= 0.492637; } 
	  if(m==8 && mm == 29) {     te3n[m][mm]= 1.498056; } 
	  if(m==8 && mm == 31) {     te3n[m][mm]= 0.548110; } 
	  if(m==8 && mm == 32) {     te3n[m][mm]= 0.195465; } 
	  if(m==8 && mm == 33) {     te3n[m][mm]= 0.558962; } 
	  if(m==8 && mm == 34) {     te3n[m][mm]= 0.019931; } 
	  if(m==8 && mm == 35) {     te3n[m][mm]= 0.036730; } 
	  if(m==9 && mm == 11) {     tw0p[m][mm]= 0.010198; } 
	  if(m==9 && mm == 12) {     tw0p[m][mm]= 0.317474; } 
	  if(m==9 && mm == 13) {     tw0p[m][mm]= 1.368137; } 
	  if(m==9 && mm == 14) {     tw0p[m][mm]= 0.243878; } 
	  if(m==9 && mm == 15) {     tw0p[m][mm]= 0.362571; } 
	  if(m==9 && mm == 16) {     tw0p[m][mm]= 0.467783; } 
	  if(m==9 && mm == 17) {     tw0p[m][mm]= 0.378360; } 
	  if(m==9 && mm == 18) {     tw0p[m][mm]= 0.188627; } 
	  if(m==9 && mm == 19) {     tw0p[m][mm]= 1.074698; } 
	  if(m==9 && mm == 20) {     tw0p[m][mm]= 0.321078; } 
	  if(m==9 && mm == 21) {     tw0p[m][mm]= 0.007122; } 
	  if(m==9 && mm == 22) {     tw0p[m][mm]= 0.388545; } 
	  if(m==9 && mm == 23) {     tw0p[m][mm]= 0.798607; } 
	  if(m==9 && mm == 24) {     tw0p[m][mm]= 0.000202; } 
	  if(m==9 && mm == 27) {     tw0p[m][mm]= 0.153555; } 
	  if(m==9 && mm == 28) {     tw0p[m][mm]= 0.955611; } 
	  if(m==9 && mm == 29) {     tw0p[m][mm]= 0.031498; } 
	  if(m==9 && mm == 30) {     tw0p[m][mm]= 0.652036; } 
	  if(m==9 && mm == 31) {     tw0p[m][mm]= 0.448310; } 
	  if(m==9 && mm == 32) {     tw0p[m][mm]= 0.274182; } 
	  if(m==9 && mm == 33) {     tw0p[m][mm]= 0.858100; } 
	  if(m==9 && mm == 34) {     tw0p[m][mm]= 0.650416; } 
	  if(m==9 && mm == 35) {     tw0p[m][mm]= 0.991533; } 
	  if(m==9 && mm == 3) {     tw1p[m][mm]= 0.217191; } 
	  if(m==9 && mm == 4) {     tw1p[m][mm]= 0.019675; } 
	  if(m==9 && mm == 5) {     tw1p[m][mm]= 0.412576; } 
	  if(m==9 && mm == 6) {     tw1p[m][mm]= 0.026541; } 
	  if(m==9 && mm == 8) {     tw1p[m][mm]= 0.052490; } 
	  if(m==9 && mm == 9) {     tw1p[m][mm]= 0.039019; } 
	  if(m==9 && mm == 19) {     tw1p[m][mm]= 0.004970; } 
	  if(m==9 && mm == 20) {     tw1p[m][mm]= 0.095394; } 
	  if(m==9 && mm == 21) {     tw1p[m][mm]= 0.409033; } 
	  if(m==9 && mm == 22) {     tw1p[m][mm]= 1.976432; } 
	  if(m==9 && mm == 23) {     tw1p[m][mm]= 0.681710; } 
	  if(m==9 && mm == 28) {     tw1p[m][mm]= 0.072780; } 
	  if(m==9 && mm == 29) {     tw1p[m][mm]= 0.868540; } 
	  if(m==9 && mm == 30) {     tw1p[m][mm]= 0.577614; } 
	  if(m==9 && mm == 31) {     tw1p[m][mm]= 0.410206; } 
	  if(m==9 && mm == 32) {     tw1p[m][mm]= 0.554466; } 
	  if(m==9 && mm == 33) {     tw1p[m][mm]= 0.185531; } 
	  if(m==9 && mm == 34) {     tw1p[m][mm]= 0.548803; } 
	  if(m==9 && mm == 35) {     tw1p[m][mm]= 0.265780; } 
	  if(m==9 && mm == 5) {     tw2p[m][mm]= 0.297412; } 
	  if(m==9 && mm == 7) {     tw2p[m][mm]= 0.132557; } 
	  if(m==9 && mm == 8) {     tw2p[m][mm]= 0.252299; } 
	  if(m==9 && mm == 9) {     tw2p[m][mm]= 0.302235; } 
	  if(m==9 && mm == 10) {     tw2p[m][mm]= 0.530740; } 
	  if(m==9 && mm == 11) {     tw2p[m][mm]= 0.720476; } 
	  if(m==9 && mm == 12) {     tw2p[m][mm]= 0.157190; } 
	  if(m==9 && mm == 13) {     tw2p[m][mm]= 0.885304; } 
	  if(m==9 && mm == 14) {     tw2p[m][mm]= 0.484989; } 
	  if(m==9 && mm == 15) {     tw2p[m][mm]= 0.635691; } 
	  if(m==9 && mm == 17) {     tw2p[m][mm]= 0.683260; } 
	  if(m==9 && mm == 18) {     tw2p[m][mm]= 0.528710; } 
	  if(m==9 && mm == 19) {     tw2p[m][mm]= 0.709718; } 
	  if(m==9 && mm == 20) {     tw2p[m][mm]= 0.447074; } 
	  if(m==9 && mm == 21) {     tw2p[m][mm]= 0.928599; } 
	  if(m==9 && mm == 22) {     tw2p[m][mm]= 0.306346; } 
	  if(m==9 && mm == 28) {     tw2p[m][mm]= 0.388677; } 
	  if(m==9 && mm == 29) {     tw2p[m][mm]= 0.298932; } 
	  if(m==9 && mm == 30) {     tw2p[m][mm]= 0.089588; } 
	  if(m==9 && mm == 31) {     tw2p[m][mm]= 0.280283; } 
	  if(m==9 && mm == 32) {     tw2p[m][mm]= 0.564345; } 
	  if(m==9 && mm == 33) {     tw2p[m][mm]= 0.261932; } 
	  if(m==9 && mm == 34) {     tw2p[m][mm]= 0.554055; } 
	  if(m==9 && mm == 35) {     tw2p[m][mm]= 0.898171; } 
	  if(m==9 && mm == 6) {     tw3p[m][mm]= 0.778261; } 
	  if(m==9 && mm == 7) {     tw3p[m][mm]= 0.988857; } 
	  if(m==9 && mm == 8) {     tw3p[m][mm]= 0.426084; } 
	  if(m==9 && mm == 9) {     tw3p[m][mm]= 0.531882; } 
	  if(m==9 && mm == 10) {     tw3p[m][mm]= 1.121952; } 
	  if(m==9 && mm == 11) {     tw3p[m][mm]= 0.690674; } 
	  if(m==9 && mm == 13) {     tw3p[m][mm]= 0.713091; } 
	  if(m==9 && mm == 14) {     tw3p[m][mm]= 0.774887; } 
	  if(m==9 && mm == 15) {     tw3p[m][mm]= 0.015478; } 
	  if(m==9 && mm == 16) {     tw3p[m][mm]= 1.698850; } 
	  if(m==9 && mm == 17) {     tw3p[m][mm]= 0.794178; } 
	  if(m==9 && mm == 18) {     tw3p[m][mm]= 0.670842; } 
	  if(m==9 && mm == 19) {     tw3p[m][mm]= 1.100549; } 
	  if(m==9 && mm == 20) {     tw3p[m][mm]= 0.694111; } 
	  if(m==9 && mm == 21) {     tw3p[m][mm]= 1.699603; } 
	  if(m==9 && mm == 22) {     tw3p[m][mm]= 0.524154; } 
	  if(m==9 && mm == 0) {     te2p[m][mm]= 0.860798; } 
	  if(m==9 && mm == 1) {     te2p[m][mm]= 0.467465; } 
	  if(m==9 && mm == 3) {     te2p[m][mm]= 0.686669; } 
	  if(m==9 && mm == 4) {     te2p[m][mm]= 0.109927; } 
	  if(m==9 && mm == 5) {     te2p[m][mm]= 0.751326; } 
	  if(m==9 && mm == 6) {     te2p[m][mm]= 3.105088; } 
	  if(m==9 && mm == 8) {     te2p[m][mm]= 0.809145; } 
	  if(m==9 && mm == 9) {     te2p[m][mm]= 0.447671; } 
	  if(m==9 && mm == 10) {     te2p[m][mm]= 0.531989; } 
	  if(m==9 && mm == 11) {     te2p[m][mm]= 0.079583; } 
	  if(m==9 && mm == 14) {     te2p[m][mm]= 0.677929; } 
	  if(m==9 && mm == 16) {     te2p[m][mm]= 0.190155; } 
	  if(m==9 && mm == 18) {     te2p[m][mm]= 0.152598; } 
	  if(m==9 && mm == 19) {     te2p[m][mm]= 0.091451; } 
	  if(m==9 && mm == 20) {     te2p[m][mm]= 0.405121; } 
	  if(m==9 && mm == 21) {     te2p[m][mm]= 0.083359; } 
	  if(m==9 && mm == 22) {     te2p[m][mm]= 0.645161; } 
	  if(m==9 && mm == 23) {     te2p[m][mm]= 0.076518; } 
	  if(m==9 && mm == 24) {     te2p[m][mm]= 0.221635; } 
	  if(m==9 && mm == 25) {     te2p[m][mm]= 0.028019; } 
	  if(m==9 && mm == 26) {     te2p[m][mm]= 0.817018; } 
	  if(m==9 && mm == 27) {     te2p[m][mm]= 0.218823; } 
	  if(m==9 && mm == 28) {     te2p[m][mm]= 0.527342; } 
	  if(m==9 && mm == 29) {     te2p[m][mm]= 0.319675; } 
	  if(m==9 && mm == 30) {     te2p[m][mm]= 0.154158; } 
	  if(m==9 && mm == 31) {     te2p[m][mm]= 0.853254; } 
	  if(m==9 && mm == 5) {     te3p[m][mm]= 0.273873; } 
	  if(m==9 && mm == 14) {     te3p[m][mm]= 0.531910; } 
	  if(m==9 && mm == 15) {     te3p[m][mm]= 0.324832; } 
	  if(m==9 && mm == 16) {     te3p[m][mm]= 0.472338; } 
	  if(m==9 && mm == 17) {     te3p[m][mm]= 0.269395; } 
	  if(m==9 && mm == 18) {     te3p[m][mm]= 0.088994; } 
	  if(m==9 && mm == 20) {     te3p[m][mm]= 0.453017; } 
	  if(m==9 && mm == 21) {     te3p[m][mm]= 0.427283; } 
	  if(m==9 && mm == 22) {     te3p[m][mm]= 0.620215; } 
	  if(m==9 && mm == 23) {     te3p[m][mm]= 0.475841; } 
	  if(m==9 && mm == 1) {     tw0n[m][mm]= 0.149400; } 
	  if(m==9 && mm == 2) {     tw0n[m][mm]= 0.105679; } 
	  if(m==9 && mm == 5) {     tw0n[m][mm]= 0.006489; } 
	  if(m==9 && mm == 6) {     tw0n[m][mm]= 0.108887; } 
	  if(m==9 && mm == 11) {     tw0n[m][mm]= 0.565335; } 
	  if(m==9 && mm == 12) {     tw0n[m][mm]= 0.516611; } 
	  if(m==9 && mm == 13) {     tw0n[m][mm]= 1.068044; } 
	  if(m==9 && mm == 14) {     tw0n[m][mm]= 0.392046; } 
	  if(m==9 && mm == 15) {     tw0n[m][mm]= 0.410214; } 
	  if(m==9 && mm == 16) {     tw0n[m][mm]= 0.573749; } 
	  if(m==9 && mm == 17) {     tw0n[m][mm]= 0.376756; } 
	  if(m==9 && mm == 18) {     tw0n[m][mm]= 0.100831; } 
	  if(m==9 && mm == 19) {     tw0n[m][mm]= 0.983852; } 
	  if(m==9 && mm == 20) {     tw0n[m][mm]= 0.389950; } 
	  if(m==9 && mm == 22) {     tw0n[m][mm]= 0.305747; } 
	  if(m==9 && mm == 23) {     tw0n[m][mm]= 0.745034; } 
	  if(m==9 && mm == 24) {     tw0n[m][mm]= 0.046172; } 
	  if(m==9 && mm == 25) {     tw0n[m][mm]= 0.526392; } 
	  if(m==9 && mm == 27) {     tw0n[m][mm]= 0.152446; } 
	  if(m==9 && mm == 28) {     tw0n[m][mm]= 0.402008; } 
	  if(m==9 && mm == 6) {     tw1n[m][mm]= 0.289231; } 
	  if(m==9 && mm == 8) {     tw1n[m][mm]= 0.252891; } 
	  if(m==9 && mm == 9) {     tw1n[m][mm]= 0.117579; } 
	  if(m==9 && mm == 10) {     tw1n[m][mm]= 0.220745; } 
	  if(m==9 && mm == 12) {     tw1n[m][mm]= 0.639932; } 
	  if(m==9 && mm == 13) {     tw1n[m][mm]= 0.532602; } 
	  if(m==9 && mm == 15) {     tw1n[m][mm]= 0.263289; } 
	  if(m==9 && mm == 16) {     tw1n[m][mm]= 0.258908; } 
	  if(m==9 && mm == 17) {     tw1n[m][mm]= 0.241693; } 
	  if(m==9 && mm == 18) {     tw1n[m][mm]= 0.228767; } 
	  if(m==9 && mm == 19) {     tw1n[m][mm]= 0.328191; } 
	  if(m==9 && mm == 20) {     tw1n[m][mm]= 0.029840; } 
	  if(m==9 && mm == 21) {     tw1n[m][mm]= 0.282585; } 
	  if(m==9 && mm == 22) {     tw1n[m][mm]= 2.175873; } 
	  if(m==9 && mm == 23) {     tw1n[m][mm]= 0.515088; } 
	  if(m==9 && mm == 29) {     tw1n[m][mm]= 0.717583; } 
	  if(m==9 && mm == 30) {     tw1n[m][mm]= 0.276308; } 
	  if(m==9 && mm == 31) {     tw1n[m][mm]= 0.195739; } 
	  if(m==9 && mm == 32) {     tw1n[m][mm]= 0.423133; } 
	  if(m==9 && mm == 34) {     tw1n[m][mm]= 0.369577; } 
	  if(m==9 && mm == 35) {     tw1n[m][mm]= 0.207810; } 
	  if(m==9 && mm == 1) {     tw2n[m][mm]= 0.064889; } 
	  if(m==9 && mm == 3) {     tw2n[m][mm]= 0.051582; } 
	  if(m==9 && mm == 4) {     tw2n[m][mm]= 0.781246; } 
	  if(m==9 && mm == 5) {     tw2n[m][mm]= 0.280248; } 
	  if(m==9 && mm == 14) {     tw2n[m][mm]= 0.663346; } 
	  if(m==9 && mm == 15) {     tw2n[m][mm]= 0.468694; } 
	  if(m==9 && mm == 16) {     tw2n[m][mm]= 0.789441; } 
	  if(m==9 && mm == 17) {     tw2n[m][mm]= 0.439099; } 
	  if(m==9 && mm == 18) {     tw2n[m][mm]= 0.454508; } 
	  if(m==9 && mm == 19) {     tw2n[m][mm]= 0.596405; } 
	  if(m==9 && mm == 20) {     tw2n[m][mm]= 0.277720; } 
	  if(m==9 && mm == 21) {     tw2n[m][mm]= 0.557328; } 
	  if(m==9 && mm == 22) {     tw2n[m][mm]= 0.236190; } 
	  if(m==9 && mm == 23) {     tw2n[m][mm]= 0.112698; } 
	  if(m==9 && mm == 24) {     tw2n[m][mm]= 0.206972; } 
	  if(m==9 && mm == 28) {     tw2n[m][mm]= 0.116705; } 
	  if(m==9 && mm == 31) {     tw2n[m][mm]= 0.010209; } 
	  if(m==9 && mm == 32) {     tw2n[m][mm]= 0.535571; } 
	  if(m==9 && mm == 34) {     tw2n[m][mm]= 0.487745; } 
	  if(m==9 && mm == 35) {     tw2n[m][mm]= 0.438771; } 
	  if(m==9 && mm == 6) {     tw3n[m][mm]= 0.818868; } 
	  if(m==9 && mm == 7) {     tw3n[m][mm]= 0.879030; } 
	  if(m==9 && mm == 8) {     tw3n[m][mm]= 0.597545; } 
	  if(m==9 && mm == 9) {     tw3n[m][mm]= 0.518215; } 
	  if(m==9 && mm == 10) {     tw3n[m][mm]= 1.054231; } 
	  if(m==9 && mm == 11) {     tw3n[m][mm]= 0.648584; } 
	  if(m==9 && mm == 13) {     tw3n[m][mm]= 0.481906; } 
	  if(m==9 && mm == 14) {     tw3n[m][mm]= 0.462387; } 
	  if(m==9 && mm == 16) {     tw3n[m][mm]= 1.338720; } 
	  if(m==9 && mm == 17) {     tw3n[m][mm]= 0.712716; } 
	  if(m==9 && mm == 18) {     tw3n[m][mm]= 0.748069; } 
	  if(m==9 && mm == 19) {     tw3n[m][mm]= 1.108199; } 
	  if(m==9 && mm == 20) {     tw3n[m][mm]= 0.568865; } 
	  if(m==9 && mm == 21) {     tw3n[m][mm]= 1.430450; } 
	  if(m==9 && mm == 22) {     tw3n[m][mm]= 0.504345; } 
	  if(m==9 && mm == 27) {     tw3n[m][mm]= 4.052835; } 
	  if(m==9 && mm == 1) {     te2n[m][mm]= 0.457735; } 
	  if(m==9 && mm == 7) {     te2n[m][mm]= 1.374494; } 
	  if(m==9 && mm == 8) {     te2n[m][mm]= 0.987168; } 
	  if(m==9 && mm == 9) {     te2n[m][mm]= 0.335535; } 
	  if(m==9 && mm == 10) {     te2n[m][mm]= 0.395891; } 
	  if(m==9 && mm == 14) {     te2n[m][mm]= 0.388435; } 
	  if(m==9 && mm == 16) {     te2n[m][mm]= 0.146847; } 
	  if(m==9 && mm == 19) {     te2n[m][mm]= 0.399705; } 
	  if(m==9 && mm == 20) {     te2n[m][mm]= 0.384022; } 
	  if(m==9 && mm == 21) {     te2n[m][mm]= 0.135196; } 
	  if(m==9 && mm == 22) {     te2n[m][mm]= 0.629479; } 
	  if(m==9 && mm == 23) {     te2n[m][mm]= 0.197645; } 
	  if(m==9 && mm == 24) {     te2n[m][mm]= 0.258311; } 
	  if(m==9 && mm == 25) {     te2n[m][mm]= 0.087260; } 
	  if(m==9 && mm == 26) {     te2n[m][mm]= 0.892730; } 
	  if(m==9 && mm == 27) {     te2n[m][mm]= 0.447849; } 
	  if(m==9 && mm == 28) {     te2n[m][mm]= 0.684992; } 
	  if(m==9 && mm == 29) {     te2n[m][mm]= 0.497039; } 
	  if(m==9 && mm == 30) {     te2n[m][mm]= 0.172869; } 
	  if(m==9 && mm == 31) {     te2n[m][mm]= 0.505677; } 
	  if(m==9 && mm == 32) {     te2n[m][mm]= 1.234040; } 
	  if(m==9 && mm == 34) {     te2n[m][mm]= 0.445290; } 
	  if(m==9 && mm == 35) {     te2n[m][mm]= 0.529182; } 
	  if(m==9 && mm == 0) {     te3n[m][mm]= 0.092927; } 
	  if(m==9 && mm == 2) {     te3n[m][mm]= 0.122033; } 
	  if(m==9 && mm == 3) {     te3n[m][mm]= 0.234228; } 
	  if(m==9 && mm == 5) {     te3n[m][mm]= 0.013993; } 
	  if(m==9 && mm == 15) {     te3n[m][mm]= 0.496002; } 
	  if(m==9 && mm == 17) {     te3n[m][mm]= 0.289541; } 
	  if(m==9 && mm == 18) {     te3n[m][mm]= 0.132483; } 
	  if(m==9 && mm == 21) {     te3n[m][mm]= 0.254670; } 
	  if(m==9 && mm == 23) {     te3n[m][mm]= 0.345204; } 
	  if(m==9 && mm == 24) {     te3n[m][mm]= 0.103444; } 
	  if(m==9 && mm == 25) {     te3n[m][mm]= 0.040669; } 
	  if(m==9 && mm == 26) {     te3n[m][mm]= 0.057931; } 
	  if(m==9 && mm == 27) {     te3n[m][mm]= 0.115940; } 
	  if(m==9 && mm == 28) {     te3n[m][mm]= 0.297726; } 
	  if(m==9 && mm == 29) {     te3n[m][mm]= 0.411177; } 
	  if(m==9 && mm == 30) {     te3n[m][mm]= 0.274209; } 
	  if(m==9 && mm == 33) {     te3n[m][mm]= 0.311417; } 
	  if(m==9 && mm == 34) {     te3n[m][mm]= 0.109891; } 
	  if(m==9 && mm == 35) {     te3n[m][mm]= 0.405248; } 
	  if(m==10 && mm == 13) {     tw0p[m][mm]= 0.460370; } 
	  if(m==10 && mm == 14) {     tw0p[m][mm]= 0.174221; } 
	  if(m==10 && mm == 15) {     tw0p[m][mm]= 0.045643; } 
	  if(m==10 && mm == 17) {     tw0p[m][mm]= 0.465652; } 
	  if(m==10 && mm == 18) {     tw0p[m][mm]= 0.480613; } 
	  if(m==10 && mm == 19) {     tw0p[m][mm]= 0.336414; } 
	  if(m==10 && mm == 20) {     tw0p[m][mm]= 1.182350; } 
	  if(m==10 && mm == 21) {     tw0p[m][mm]= 0.212923; } 
	  if(m==10 && mm == 22) {     tw0p[m][mm]= 0.291484; } 
	  if(m==10 && mm == 23) {     tw0p[m][mm]= 0.009155; } 
	  if(m==10 && mm == 24) {     tw0p[m][mm]= 0.291416; } 
	  if(m==10 && mm == 25) {     tw0p[m][mm]= 0.069071; } 
	  if(m==10 && mm == 26) {     tw0p[m][mm]= 0.346032; } 
	  if(m==10 && mm == 27) {     tw0p[m][mm]= 0.342008; } 
	  if(m==10 && mm == 29) {     tw0p[m][mm]= 0.954382; } 
	  if(m==10 && mm == 30) {     tw0p[m][mm]= 0.769036; } 
	  if(m==10 && mm == 31) {     tw0p[m][mm]= 0.378929; } 
	  if(m==10 && mm == 32) {     tw0p[m][mm]= 1.836730; } 
	  if(m==10 && mm == 33) {     tw0p[m][mm]= 0.024820; } 
	  if(m==10 && mm == 35) {     tw0p[m][mm]= 0.212853; } 
	  if(m==10 && mm == 0) {     tw1p[m][mm]= 0.316092; } 
	  if(m==10 && mm == 1) {     tw1p[m][mm]= 0.530972; } 
	  if(m==10 && mm == 2) {     tw1p[m][mm]= 0.043642; } 
	  if(m==10 && mm == 3) {     tw1p[m][mm]= 0.041951; } 
	  if(m==10 && mm == 4) {     tw1p[m][mm]= 0.419070; } 
	  if(m==10 && mm == 6) {     tw1p[m][mm]= 0.408781; } 
	  if(m==10 && mm == 7) {     tw1p[m][mm]= 0.442185; } 
	  if(m==10 && mm == 19) {     tw1p[m][mm]= 0.111018; } 
	  if(m==10 && mm == 20) {     tw1p[m][mm]= 0.065420; } 
	  if(m==10 && mm == 21) {     tw1p[m][mm]= 0.647278; } 
	  if(m==10 && mm == 22) {     tw1p[m][mm]= 0.572225; } 
	  if(m==10 && mm == 23) {     tw1p[m][mm]= 0.227857; } 
	  if(m==10 && mm == 28) {     tw1p[m][mm]= 0.039257; } 
	  if(m==10 && mm == 29) {     tw1p[m][mm]= 1.078884; } 
	  if(m==10 && mm == 30) {     tw1p[m][mm]= 0.294450; } 
	  if(m==10 && mm == 32) {     tw1p[m][mm]= 0.350394; } 
	  if(m==10 && mm == 33) {     tw1p[m][mm]= 0.780693; } 
	  if(m==10 && mm == 8) {     tw2p[m][mm]= 1.212754; } 
	  if(m==10 && mm == 9) {     tw2p[m][mm]= 0.401582; } 
	  if(m==10 && mm == 11) {     tw2p[m][mm]= 0.122495; } 
	  if(m==10 && mm == 12) {     tw2p[m][mm]= 0.029435; } 
	  if(m==10 && mm == 13) {     tw2p[m][mm]= 0.127998; } 
	  if(m==10 && mm == 14) {     tw2p[m][mm]= 0.379065; } 
	  if(m==10 && mm == 15) {     tw2p[m][mm]= 1.341315; } 
	  if(m==10 && mm == 16) {     tw2p[m][mm]= 0.605384; } 
	  if(m==10 && mm == 17) {     tw2p[m][mm]= 0.797383; } 
	  if(m==10 && mm == 18) {     tw2p[m][mm]= 0.309874; } 
	  if(m==10 && mm == 19) {     tw2p[m][mm]= 0.412974; } 
	  if(m==10 && mm == 20) {     tw2p[m][mm]= 0.264475; } 
	  if(m==10 && mm == 21) {     tw2p[m][mm]= 0.920103; } 
	  if(m==10 && mm == 22) {     tw2p[m][mm]= 0.275336; } 
	  if(m==10 && mm == 29) {     tw2p[m][mm]= 0.048091; } 
	  if(m==10 && mm == 31) {     tw2p[m][mm]= 0.256532; } 
	  if(m==10 && mm == 32) {     tw2p[m][mm]= 0.177985; } 
	  if(m==10 && mm == 33) {     tw2p[m][mm]= 0.346018; } 
	  if(m==10 && mm == 34) {     tw2p[m][mm]= 0.842273; } 
	  if(m==10 && mm == 35) {     tw2p[m][mm]= 0.238802; } 
	  if(m==10 && mm == 12) {     tw3p[m][mm]= 0.625476; } 
	  if(m==10 && mm == 13) {     tw3p[m][mm]= 0.448227; } 
	  if(m==10 && mm == 23) {     tw3p[m][mm]= 0.093401; } 
	  if(m==10 && mm == 31) {     tw3p[m][mm]= 0.103880; } 
	  if(m==10 && mm == 32) {     tw3p[m][mm]= 0.126483; } 
	  if(m==10 && mm == 1) {     te2p[m][mm]= 0.581686; } 
	  if(m==10 && mm == 2) {     te2p[m][mm]= 0.664832; } 
	  if(m==10 && mm == 3) {     te2p[m][mm]= 0.272766; } 
	  if(m==10 && mm == 4) {     te2p[m][mm]= 0.391597; } 
	  if(m==10 && mm == 5) {     te2p[m][mm]= 1.145656; } 
	  if(m==10 && mm == 7) {     te2p[m][mm]= 1.530199; } 
	  if(m==10 && mm == 8) {     te2p[m][mm]= 0.690420; } 
	  if(m==10 && mm == 9) {     te2p[m][mm]= 0.251171; } 
	  if(m==10 && mm == 10) {     te2p[m][mm]= 0.929412; } 
	  if(m==10 && mm == 15) {     te2p[m][mm]= 0.194704; } 
	  if(m==10 && mm == 18) {     te2p[m][mm]= 0.468425; } 
	  if(m==10 && mm == 19) {     te2p[m][mm]= 0.092609; } 
	  if(m==10 && mm == 21) {     te2p[m][mm]= 0.195743; } 
	  if(m==10 && mm == 22) {     te2p[m][mm]= 0.165378; } 
	  if(m==10 && mm == 27) {     te2p[m][mm]= 0.090468; } 
	  if(m==10 && mm == 30) {     te2p[m][mm]= 0.089416; } 
	  if(m==10 && mm == 31) {     te2p[m][mm]= 0.752401; } 
	  if(m==10 && mm == 5) {     te3p[m][mm]= 0.017198; } 
	  if(m==10 && mm == 7) {     te3p[m][mm]= 1.261517; } 
	  if(m==10 && mm == 8) {     te3p[m][mm]= 0.002714; } 
	  if(m==10 && mm == 9) {     te3p[m][mm]= 0.159470; } 
	  if(m==10 && mm == 10) {     te3p[m][mm]= 0.336328; } 
	  if(m==10 && mm == 13) {     te3p[m][mm]= 0.795963; } 
	  if(m==10 && mm == 14) {     te3p[m][mm]= 0.558711; } 
	  if(m==10 && mm == 15) {     te3p[m][mm]= 0.948972; } 
	  if(m==10 && mm == 16) {     te3p[m][mm]= 0.162998; } 
	  if(m==10 && mm == 17) {     te3p[m][mm]= 0.596821; } 
	  if(m==10 && mm == 18) {     te3p[m][mm]= 0.093213; } 
	  if(m==10 && mm == 19) {     te3p[m][mm]= 0.447309; } 
	  if(m==10 && mm == 20) {     te3p[m][mm]= 0.073454; } 
	  if(m==10 && mm == 21) {     te3p[m][mm]= 1.790969; } 
	  if(m==10 && mm == 0) {     tw0n[m][mm]= 0.044833; } 
	  if(m==10 && mm == 1) {     tw0n[m][mm]= 0.154323; } 
	  if(m==10 && mm == 2) {     tw0n[m][mm]= 0.324541; } 
	  if(m==10 && mm == 7) {     tw0n[m][mm]= 0.003545; } 
	  if(m==10 && mm == 12) {     tw0n[m][mm]= 0.060321; } 
	  if(m==10 && mm == 13) {     tw0n[m][mm]= 0.492104; } 
	  if(m==10 && mm == 14) {     tw0n[m][mm]= 0.254736; } 
	  if(m==10 && mm == 15) {     tw0n[m][mm]= 0.089905; } 
	  if(m==10 && mm == 16) {     tw0n[m][mm]= 0.198251; } 
	  if(m==10 && mm == 17) {     tw0n[m][mm]= 0.596269; } 
	  if(m==10 && mm == 18) {     tw0n[m][mm]= 0.582897; } 
	  if(m==10 && mm == 19) {     tw0n[m][mm]= 0.226760; } 
	  if(m==10 && mm == 20) {     tw0n[m][mm]= 0.879516; } 
	  if(m==10 && mm == 21) {     tw0n[m][mm]= 0.170237; } 
	  if(m==10 && mm == 22) {     tw0n[m][mm]= 0.224141; } 
	  if(m==10 && mm == 24) {     tw0n[m][mm]= 0.337088; } 
	  if(m==10 && mm == 25) {     tw0n[m][mm]= 0.087365; } 
	  if(m==10 && mm == 26) {     tw0n[m][mm]= 0.474766; } 
	  if(m==10 && mm == 27) {     tw0n[m][mm]= 0.124846; } 
	  if(m==10 && mm == 29) {     tw0n[m][mm]= 0.358661; } 
	  if(m==10 && mm == 30) {     tw0n[m][mm]= 0.478487; } 
	  if(m==10 && mm == 6) {     tw1n[m][mm]= 0.991367; } 
	  if(m==10 && mm == 7) {     tw1n[m][mm]= 0.328976; } 
	  if(m==10 && mm == 8) {     tw1n[m][mm]= 0.862580; } 
	  if(m==10 && mm == 10) {     tw1n[m][mm]= 0.161247; } 
	  if(m==10 && mm == 12) {     tw1n[m][mm]= 0.380707; } 
	  if(m==10 && mm == 13) {     tw1n[m][mm]= 0.116539; } 
	  if(m==10 && mm == 14) {     tw1n[m][mm]= 0.054976; } 
	  if(m==10 && mm == 17) {     tw1n[m][mm]= 0.042921; } 
	  if(m==10 && mm == 18) {     tw1n[m][mm]= 0.687405; } 
	  if(m==10 && mm == 19) {     tw1n[m][mm]= 0.167524; } 
	  if(m==10 && mm == 21) {     tw1n[m][mm]= 0.756657; } 
	  if(m==10 && mm == 22) {     tw1n[m][mm]= 0.400662; } 
	  if(m==10 && mm == 23) {     tw1n[m][mm]= 0.172123; } 
	  if(m==10 && mm == 30) {     tw1n[m][mm]= 0.374965; } 
	  if(m==10 && mm == 32) {     tw1n[m][mm]= 0.180682; } 
	  if(m==10 && mm == 33) {     tw1n[m][mm]= 0.297526; } 
	  if(m==10 && mm == 0) {     tw2n[m][mm]= 0.301225; } 
	  if(m==10 && mm == 1) {     tw2n[m][mm]= 0.506075; } 
	  if(m==10 && mm == 2) {     tw2n[m][mm]= 0.240647; } 
	  if(m==10 && mm == 3) {     tw2n[m][mm]= 0.157952; } 
	  if(m==10 && mm == 4) {     tw2n[m][mm]= 0.019464; } 
	  if(m==10 && mm == 14) {     tw2n[m][mm]= 0.261315; } 
	  if(m==10 && mm == 16) {     tw2n[m][mm]= 0.009452; } 
	  if(m==10 && mm == 17) {     tw2n[m][mm]= 0.293894; } 
	  if(m==10 && mm == 18) {     tw2n[m][mm]= 0.299214; } 
	  if(m==10 && mm == 19) {     tw2n[m][mm]= 0.239176; } 
	  if(m==10 && mm == 20) {     tw2n[m][mm]= 0.102370; } 
	  if(m==10 && mm == 21) {     tw2n[m][mm]= 0.614133; } 
	  if(m==10 && mm == 23) {     tw2n[m][mm]= 0.195129; } 
	  if(m==10 && mm == 25) {     tw2n[m][mm]= 0.378706; } 
	  if(m==10 && mm == 26) {     tw2n[m][mm]= 0.232029; } 
	  if(m==10 && mm == 27) {     tw2n[m][mm]= 0.203735; } 
	  if(m==10 && mm == 30) {     tw2n[m][mm]= 0.185476; } 
	  if(m==10 && mm == 31) {     tw2n[m][mm]= 0.533372; } 
	  if(m==10 && mm == 33) {     tw2n[m][mm]= 0.037522; } 
	  if(m==10 && mm == 34) {     tw2n[m][mm]= 0.525901; } 
	  if(m==10 && mm == 35) {     tw2n[m][mm]= 0.155326; } 
	  if(m==10 && mm == 12) {     tw3n[m][mm]= 0.590983; } 
	  if(m==10 && mm == 13) {     tw3n[m][mm]= 0.381682; } 
	  if(m==10 && mm == 5) {     te2n[m][mm]= 1.726723; } 
	  if(m==10 && mm == 6) {     te2n[m][mm]= 0.845594; } 
	  if(m==10 && mm == 7) {     te2n[m][mm]= 0.830758; } 
	  if(m==10 && mm == 8) {     te2n[m][mm]= 0.281808; } 
	  if(m==10 && mm == 9) {     te2n[m][mm]= 0.225923; } 
	  if(m==10 && mm == 10) {     te2n[m][mm]= 0.732335; } 
	  if(m==10 && mm == 11) {     te2n[m][mm]= 1.389071; } 
	  if(m==10 && mm == 15) {     te2n[m][mm]= 0.131470; } 
	  if(m==10 && mm == 17) {     te2n[m][mm]= 0.166679; } 
	  if(m==10 && mm == 18) {     te2n[m][mm]= 0.326597; } 
	  if(m==10 && mm == 19) {     te2n[m][mm]= 0.203790; } 
	  if(m==10 && mm == 20) {     te2n[m][mm]= 0.004203; } 
	  if(m==10 && mm == 21) {     te2n[m][mm]= 0.497180; } 
	  if(m==10 && mm == 22) {     te2n[m][mm]= 0.309975; } 
	  if(m==10 && mm == 27) {     te2n[m][mm]= 0.087100; } 
	  if(m==10 && mm == 30) {     te2n[m][mm]= 0.239126; } 
	  if(m==10 && mm == 31) {     te2n[m][mm]= 0.510963; } 
	  if(m==10 && mm == 32) {     te2n[m][mm]= 0.609979; } 
	  if(m==10 && mm == 33) {     te2n[m][mm]= 0.311177; } 
	  if(m==10 && mm == 34) {     te2n[m][mm]= 0.124050; } 
	  if(m==10 && mm == 35) {     te2n[m][mm]= 0.029200; } 
	  if(m==10 && mm == 0) {     te3n[m][mm]= 0.071413; } 
	  if(m==10 && mm == 2) {     te3n[m][mm]= 0.112549; } 
	  if(m==10 && mm == 3) {     te3n[m][mm]= 0.153202; } 
	  if(m==10 && mm == 4) {     te3n[m][mm]= 0.277129; } 
	  if(m==10 && mm == 5) {     te3n[m][mm]= 0.226233; } 
	  if(m==10 && mm == 17) {     te3n[m][mm]= 0.687774; } 
	  if(m==10 && mm == 18) {     te3n[m][mm]= 0.161241; } 
	  if(m==10 && mm == 19) {     te3n[m][mm]= 0.315994; } 
	  if(m==10 && mm == 21) {     te3n[m][mm]= 2.590948; } 
	  if(m==10 && mm == 22) {     te3n[m][mm]= 0.843801; } 
	  if(m==10 && mm == 25) {     te3n[m][mm]= 1.087302; } 
	  if(m==10 && mm == 28) {     te3n[m][mm]= 0.270950; } 
	  if(m==10 && mm == 31) {     te3n[m][mm]= 0.354288; } 
	  if(m==10 && mm == 34) {     te3n[m][mm]= 0.219610; } 
	  if(m==11 && mm == 12) {     tw0p[m][mm]= 0.111567; } 
	  if(m==11 && mm == 13) {     tw0p[m][mm]= 0.136596; } 
	  if(m==11 && mm == 15) {     tw0p[m][mm]= 0.181880; } 
	  if(m==11 && mm == 16) {     tw0p[m][mm]= 0.293635; } 
	  if(m==11 && mm == 17) {     tw0p[m][mm]= 0.506887; } 
	  if(m==11 && mm == 18) {     tw0p[m][mm]= 0.425045; } 
	  if(m==11 && mm == 19) {     tw0p[m][mm]= 0.038268; } 
	  if(m==11 && mm == 20) {     tw0p[m][mm]= 0.604642; } 
	  if(m==11 && mm == 21) {     tw0p[m][mm]= 0.760599; } 
	  if(m==11 && mm == 22) {     tw0p[m][mm]= 0.227063; } 
	  if(m==11 && mm == 23) {     tw0p[m][mm]= 0.393364; } 
	  if(m==11 && mm == 24) {     tw0p[m][mm]= 0.479936; } 
	  if(m==11 && mm == 25) {     tw0p[m][mm]= 0.175058; } 
	  if(m==11 && mm == 27) {     tw0p[m][mm]= 0.924893; } 
	  if(m==11 && mm == 28) {     tw0p[m][mm]= 0.076248; } 
	  if(m==11 && mm == 29) {     tw0p[m][mm]= 0.284004; } 
	  if(m==11 && mm == 30) {     tw0p[m][mm]= 0.897811; } 
	  if(m==11 && mm == 31) {     tw0p[m][mm]= 0.509393; } 
	  if(m==11 && mm == 32) {     tw0p[m][mm]= 0.179486; } 
	  if(m==11 && mm == 33) {     tw0p[m][mm]= 0.555785; } 
	  if(m==11 && mm == 34) {     tw0p[m][mm]= 0.436185; } 
	  if(m==11 && mm == 35) {     tw0p[m][mm]= 0.510622; } 
	  if(m==11 && mm == 2) {     tw1p[m][mm]= 0.302464; } 
	  if(m==11 && mm == 3) {     tw1p[m][mm]= 0.054341; } 
	  if(m==11 && mm == 4) {     tw1p[m][mm]= 0.190433; } 
	  if(m==11 && mm == 5) {     tw1p[m][mm]= 0.077936; } 
	  if(m==11 && mm == 8) {     tw1p[m][mm]= 0.237596; } 
	  if(m==11 && mm == 19) {     tw1p[m][mm]= 0.399123; } 
	  if(m==11 && mm == 20) {     tw1p[m][mm]= 0.837463; } 
	  if(m==11 && mm == 21) {     tw1p[m][mm]= 0.706210; } 
	  if(m==11 && mm == 22) {     tw1p[m][mm]= 0.593270; } 
	  if(m==11 && mm == 23) {     tw1p[m][mm]= 0.623836; } 
	  if(m==11 && mm == 29) {     tw1p[m][mm]= 0.473670; } 
	  if(m==11 && mm == 30) {     tw1p[m][mm]= 0.666398; } 
	  if(m==11 && mm == 31) {     tw1p[m][mm]= 0.156253; } 
	  if(m==11 && mm == 32) {     tw1p[m][mm]= 0.176343; } 
	  if(m==11 && mm == 33) {     tw1p[m][mm]= 0.227444; } 
	  if(m==11 && mm == 34) {     tw1p[m][mm]= 0.159568; } 
	  if(m==11 && mm == 35) {     tw1p[m][mm]= 0.294098; } 
	  if(m==11 && mm == 5) {     tw2p[m][mm]= 0.679809; } 
	  if(m==11 && mm == 6) {     tw2p[m][mm]= 0.161208; } 
	  if(m==11 && mm == 7) {     tw2p[m][mm]= 0.483005; } 
	  if(m==11 && mm == 8) {     tw2p[m][mm]= 0.534083; } 
	  if(m==11 && mm == 9) {     tw2p[m][mm]= 0.453562; } 
	  if(m==11 && mm == 10) {     tw2p[m][mm]= 0.297610; } 
	  if(m==11 && mm == 11) {     tw2p[m][mm]= 0.132567; } 
	  if(m==11 && mm == 12) {     tw2p[m][mm]= 0.075488; } 
	  if(m==11 && mm == 13) {     tw2p[m][mm]= 0.158321; } 
	  if(m==11 && mm == 14) {     tw2p[m][mm]= 0.184470; } 
	  if(m==11 && mm == 15) {     tw2p[m][mm]= 0.035863; } 
	  if(m==11 && mm == 16) {     tw2p[m][mm]= 0.641007; } 
	  if(m==11 && mm == 17) {     tw2p[m][mm]= 0.164196; } 
	  if(m==11 && mm == 18) {     tw2p[m][mm]= 1.289371; } 
	  if(m==11 && mm == 19) {     tw2p[m][mm]= 0.603290; } 
	  if(m==11 && mm == 20) {     tw2p[m][mm]= 0.929814; } 
	  if(m==11 && mm == 21) {     tw2p[m][mm]= 0.624538; } 
	  if(m==11 && mm == 28) {     tw2p[m][mm]= 0.285850; } 
	  if(m==11 && mm == 30) {     tw2p[m][mm]= 0.039526; } 
	  if(m==11 && mm == 31) {     tw2p[m][mm]= 0.161434; } 
	  if(m==11 && mm == 32) {     tw2p[m][mm]= 0.222309; } 
	  if(m==11 && mm == 33) {     tw2p[m][mm]= 0.207932; } 
	  if(m==11 && mm == 34) {     tw2p[m][mm]= 0.979274; } 
	  if(m==11 && mm == 35) {     tw2p[m][mm]= 0.635971; } 
	  if(m==11 && mm == 12) {     tw3p[m][mm]= 0.278281; } 
	  if(m==11 && mm == 13) {     tw3p[m][mm]= 0.741511; } 
	  if(m==11 && mm == 22) {     tw3p[m][mm]= 0.756218; } 
	  if(m==11 && mm == 25) {     tw3p[m][mm]= 0.537866; } 
	  if(m==11 && mm == 27) {     tw3p[m][mm]= 0.423087; } 
	  if(m==11 && mm == 30) {     tw3p[m][mm]= 1.016774; } 
	  if(m==11 && mm == 33) {     tw3p[m][mm]= 0.242221; } 
	  if(m==11 && mm == 34) {     tw3p[m][mm]= 0.240673; } 
	  if(m==11 && mm == 0) {     te2p[m][mm]= 0.133573; } 
	  if(m==11 && mm == 1) {     te2p[m][mm]= 0.746490; } 
	  if(m==11 && mm == 2) {     te2p[m][mm]= 0.498168; } 
	  if(m==11 && mm == 3) {     te2p[m][mm]= 1.379444; } 
	  if(m==11 && mm == 5) {     te2p[m][mm]= 0.689785; } 
	  if(m==11 && mm == 6) {     te2p[m][mm]= 1.645636; } 
	  if(m==11 && mm == 7) {     te2p[m][mm]= 1.745609; } 
	  if(m==11 && mm == 8) {     te2p[m][mm]= 0.393222; } 
	  if(m==11 && mm == 9) {     te2p[m][mm]= 0.817297; } 
	  if(m==11 && mm == 10) {     te2p[m][mm]= 1.630000; } 
	  if(m==11 && mm == 11) {     te2p[m][mm]= 0.538305; } 
	  if(m==11 && mm == 12) {     te2p[m][mm]= 0.432366; } 
	  if(m==11 && mm == 15) {     te2p[m][mm]= 0.192482; } 
	  if(m==11 && mm == 18) {     te2p[m][mm]= 0.140610; } 
	  if(m==11 && mm == 20) {     te2p[m][mm]= 0.019577; } 
	  if(m==11 && mm == 21) {     te2p[m][mm]= 0.134772; } 
	  if(m==11 && mm == 22) {     te2p[m][mm]= 0.014584; } 
	  if(m==11 && mm == 23) {     te2p[m][mm]= 0.010805; } 
	  if(m==11 && mm == 27) {     te2p[m][mm]= 0.573515; } 
	  if(m==11 && mm == 30) {     te2p[m][mm]= 0.330887; } 
	  if(m==11 && mm == 5) {     te3p[m][mm]= 0.347329; } 
	  if(m==11 && mm == 7) {     te3p[m][mm]= 0.133866; } 
	  if(m==11 && mm == 8) {     te3p[m][mm]= 0.086704; } 
	  if(m==11 && mm == 9) {     te3p[m][mm]= 0.710258; } 
	  if(m==11 && mm == 11) {     te3p[m][mm]= 0.044493; } 
	  if(m==11 && mm == 12) {     te3p[m][mm]= 0.576680; } 
	  if(m==11 && mm == 14) {     te3p[m][mm]= 0.519351; } 
	  if(m==11 && mm == 15) {     te3p[m][mm]= 0.284760; } 
	  if(m==11 && mm == 16) {     te3p[m][mm]= 0.256449; } 
	  if(m==11 && mm == 17) {     te3p[m][mm]= 0.025830; } 
	  if(m==11 && mm == 18) {     te3p[m][mm]= 0.702561; } 
	  if(m==11 && mm == 19) {     te3p[m][mm]= 0.126313; } 
	  if(m==11 && mm == 20) {     te3p[m][mm]= 0.502668; } 
	  if(m==11 && mm == 21) {     te3p[m][mm]= 1.387507; } 
	  if(m==11 && mm == 22) {     te3p[m][mm]= 0.680591; } 
	  if(m==11 && mm == 23) {     te3p[m][mm]= 0.551579; } 
	  if(m==11 && mm == 0) {     tw0n[m][mm]= 0.063219; } 
	  if(m==11 && mm == 1) {     tw0n[m][mm]= 0.093955; } 
	  if(m==11 && mm == 2) {     tw0n[m][mm]= 0.088489; } 
	  if(m==11 && mm == 5) {     tw0n[m][mm]= 0.149262; } 
	  if(m==11 && mm == 6) {     tw0n[m][mm]= 0.083693; } 
	  if(m==11 && mm == 12) {     tw0n[m][mm]= 0.190825; } 
	  if(m==11 && mm == 13) {     tw0n[m][mm]= 0.021438; } 
	  if(m==11 && mm == 14) {     tw0n[m][mm]= 0.009305; } 
	  if(m==11 && mm == 15) {     tw0n[m][mm]= 0.324840; } 
	  if(m==11 && mm == 16) {     tw0n[m][mm]= 0.475950; } 
	  if(m==11 && mm == 17) {     tw0n[m][mm]= 1.270000; } 
	  if(m==11 && mm == 18) {     tw0n[m][mm]= 0.570530; } 
	  if(m==11 && mm == 20) {     tw0n[m][mm]= 0.905479; } 
	  if(m==11 && mm == 21) {     tw0n[m][mm]= 0.608252; } 
	  if(m==11 && mm == 22) {     tw0n[m][mm]= 0.086239; } 
	  if(m==11 && mm == 23) {     tw0n[m][mm]= 0.466102; } 
	  if(m==11 && mm == 24) {     tw0n[m][mm]= 0.405806; } 
	  if(m==11 && mm == 25) {     tw0n[m][mm]= 0.135223; } 
	  if(m==11 && mm == 26) {     tw0n[m][mm]= 0.450367; } 
	  if(m==11 && mm == 27) {     tw0n[m][mm]= 0.456008; } 
	  if(m==11 && mm == 28) {     tw0n[m][mm]= 0.300088; } 
	  if(m==11 && mm == 29) {     tw0n[m][mm]= 0.084507; } 
	  if(m==11 && mm == 30) {     tw0n[m][mm]= 0.654854; } 
	  if(m==11 && mm == 6) {     tw1n[m][mm]= 0.269152; } 
	  if(m==11 && mm == 8) {     tw1n[m][mm]= 0.181716; } 
	  if(m==11 && mm == 9) {     tw1n[m][mm]= 0.397219; } 
	  if(m==11 && mm == 11) {     tw1n[m][mm]= 0.215648; } 
	  if(m==11 && mm == 14) {     tw1n[m][mm]= 0.066839; } 
	  if(m==11 && mm == 15) {     tw1n[m][mm]= 0.199814; } 
	  if(m==11 && mm == 17) {     tw1n[m][mm]= 1.384099; } 
	  if(m==11 && mm == 18) {     tw1n[m][mm]= 0.262430; } 
	  if(m==11 && mm == 19) {     tw1n[m][mm]= 0.234509; } 
	  if(m==11 && mm == 20) {     tw1n[m][mm]= 0.698191; } 
	  if(m==11 && mm == 21) {     tw1n[m][mm]= 1.655326; } 
	  if(m==11 && mm == 22) {     tw1n[m][mm]= 0.174372; } 
	  if(m==11 && mm == 23) {     tw1n[m][mm]= 0.285729; } 
	  if(m==11 && mm == 30) {     tw1n[m][mm]= 0.764104; } 
	  if(m==11 && mm == 33) {     tw1n[m][mm]= 0.110053; } 
	  if(m==11 && mm == 35) {     tw1n[m][mm]= 0.043517; } 
	  if(m==11 && mm == 0) {     tw2n[m][mm]= 0.448975; } 
	  if(m==11 && mm == 1) {     tw2n[m][mm]= 0.176712; } 
	  if(m==11 && mm == 2) {     tw2n[m][mm]= 0.161289; } 
	  if(m==11 && mm == 3) {     tw2n[m][mm]= 0.315113; } 
	  if(m==11 && mm == 4) {     tw2n[m][mm]= 0.433695; } 
	  if(m==11 && mm == 5) {     tw2n[m][mm]= 0.143521; } 
	  if(m==11 && mm == 6) {     tw2n[m][mm]= 0.157997; } 
	  if(m==11 && mm == 7) {     tw2n[m][mm]= 0.700831; } 
	  if(m==11 && mm == 15) {     tw2n[m][mm]= 0.085947; } 
	  if(m==11 && mm == 16) {     tw2n[m][mm]= 0.839304; } 
	  if(m==11 && mm == 17) {     tw2n[m][mm]= 0.123688; } 
	  if(m==11 && mm == 18) {     tw2n[m][mm]= 1.265502; } 
	  if(m==11 && mm == 19) {     tw2n[m][mm]= 0.515012; } 
	  if(m==11 && mm == 20) {     tw2n[m][mm]= 0.803084; } 
	  if(m==11 && mm == 21) {     tw2n[m][mm]= 0.402213; } 
	  if(m==11 && mm == 22) {     tw2n[m][mm]= 0.437288; } 
	  if(m==11 && mm == 23) {     tw2n[m][mm]= 0.482505; } 
	  if(m==11 && mm == 25) {     tw2n[m][mm]= 0.044460; } 
	  if(m==11 && mm == 26) {     tw2n[m][mm]= 0.117954; } 
	  if(m==11 && mm == 27) {     tw2n[m][mm]= 0.444543; } 
	  if(m==11 && mm == 28) {     tw2n[m][mm]= 0.245314; } 
	  if(m==11 && mm == 32) {     tw2n[m][mm]= 0.051037; } 
	  if(m==11 && mm == 33) {     tw2n[m][mm]= 0.075518; } 
	  if(m==11 && mm == 34) {     tw2n[m][mm]= 0.680193; } 
	  if(m==11 && mm == 35) {     tw2n[m][mm]= 0.379665; } 
	  if(m==11 && mm == 12) {     tw3n[m][mm]= 0.248595; } 
	  if(m==11 && mm == 13) {     tw3n[m][mm]= 0.789320; } 
	  if(m==11 && mm == 22) {     tw3n[m][mm]= 0.094354; } 
	  if(m==11 && mm == 25) {     tw3n[m][mm]= 0.463802; } 
	  if(m==11 && mm == 5) {     te2n[m][mm]= 0.821482; } 
	  if(m==11 && mm == 6) {     te2n[m][mm]= 0.580988; } 
	  if(m==11 && mm == 7) {     te2n[m][mm]= 1.659585; } 
	  if(m==11 && mm == 8) {     te2n[m][mm]= 0.121309; } 
	  if(m==11 && mm == 9) {     te2n[m][mm]= 0.522454; } 
	  if(m==11 && mm == 10) {     te2n[m][mm]= 2.206458; } 
	  if(m==11 && mm == 11) {     te2n[m][mm]= 0.441057; } 
	  if(m==11 && mm == 12) {     te2n[m][mm]= 0.254993; } 
	  if(m==11 && mm == 15) {     te2n[m][mm]= 0.187986; } 
	  if(m==11 && mm == 18) {     te2n[m][mm]= 0.009280; } 
	  if(m==11 && mm == 19) {     te2n[m][mm]= 0.144309; } 
	  if(m==11 && mm == 21) {     te2n[m][mm]= 0.175973; } 
	  if(m==11 && mm == 22) {     te2n[m][mm]= 0.109187; } 
	  if(m==11 && mm == 23) {     te2n[m][mm]= 0.157265; } 
	  if(m==11 && mm == 27) {     te2n[m][mm]= 0.372508; } 
	  if(m==11 && mm == 30) {     te2n[m][mm]= 0.528498; } 
	  if(m==11 && mm == 31) {     te2n[m][mm]= 0.475787; } 
	  if(m==11 && mm == 32) {     te2n[m][mm]= 0.270365; } 
	  if(m==11 && mm == 33) {     te2n[m][mm]= 0.723408; } 
	  if(m==11 && mm == 34) {     te2n[m][mm]= 1.288761; } 
	  if(m==11 && mm == 35) {     te2n[m][mm]= 0.495476; } 
	  if(m==11 && mm == 0) {     te3n[m][mm]= 0.036463; } 
	  if(m==11 && mm == 2) {     te3n[m][mm]= 0.218708; } 
	  if(m==11 && mm == 5) {     te3n[m][mm]= 0.383995; } 
	  if(m==11 && mm == 15) {     te3n[m][mm]= 0.431815; } 
	  if(m==11 && mm == 16) {     te3n[m][mm]= 0.127429; } 
	  if(m==11 && mm == 18) {     te3n[m][mm]= 0.985333; } 
	  if(m==11 && mm == 19) {     te3n[m][mm]= 0.124765; } 
	  if(m==11 && mm == 20) {     te3n[m][mm]= 0.303380; } 
	  if(m==11 && mm == 21) {     te3n[m][mm]= 1.762381; } 
	  if(m==11 && mm == 22) {     te3n[m][mm]= 0.585584; } 
	  if(m==11 && mm == 23) {     te3n[m][mm]= 0.358536; } 
	  if(m==11 && mm == 24) {     te3n[m][mm]= 0.013267; } 
	  if(m==11 && mm == 25) {     te3n[m][mm]= 0.263791; } 
	  if(m==11 && mm == 26) {     te3n[m][mm]= 0.055846; } 
	  if(m==11 && mm == 27) {     te3n[m][mm]= 0.605819; } 
	  if(m==11 && mm == 28) {     te3n[m][mm]= 0.528538; } 
	  if(m==11 && mm == 29) {     te3n[m][mm]= 0.245218; } 
	  if(m==11 && mm == 30) {     te3n[m][mm]= 0.772825; } 
	  if(m==11 && mm == 31) {     te3n[m][mm]= 0.226910; } 
	  if(m==11 && mm == 32) {     te3n[m][mm]= 0.713833; } 
	  if(m==11 && mm == 33) {     te3n[m][mm]= 0.474660; } 
	  if(m==11 && mm == 34) {     te3n[m][mm]= 0.480846; } 
	  if(m==11 && mm == 35) {     te3n[m][mm]= 0.134373; } 
	  if(m==12 && mm == 8) {     tw0p[m][mm]= 0.145134; } 
	  if(m==12 && mm == 9) {     tw0p[m][mm]= 0.277854; } 
	  if(m==12 && mm == 12) {     tw0p[m][mm]= 0.299992; } 
	  if(m==12 && mm == 13) {     tw0p[m][mm]= 0.185811; } 
	  if(m==12 && mm == 14) {     tw0p[m][mm]= 0.297915; } 
	  if(m==12 && mm == 15) {     tw0p[m][mm]= 0.428164; } 
	  if(m==12 && mm == 16) {     tw0p[m][mm]= 0.081875; } 
	  if(m==12 && mm == 17) {     tw0p[m][mm]= 0.338748; } 
	  if(m==12 && mm == 18) {     tw0p[m][mm]= 0.473241; } 
	  if(m==12 && mm == 19) {     tw0p[m][mm]= 0.269745; } 
	  if(m==12 && mm == 20) {     tw0p[m][mm]= 0.347258; } 
	  if(m==12 && mm == 21) {     tw0p[m][mm]= 0.318113; } 
	  if(m==12 && mm == 22) {     tw0p[m][mm]= 0.530147; } 
	  if(m==12 && mm == 23) {     tw0p[m][mm]= 0.417542; } 
	  if(m==12 && mm == 24) {     tw0p[m][mm]= 0.561805; } 
	  if(m==12 && mm == 25) {     tw0p[m][mm]= 0.438435; } 
	  if(m==12 && mm == 26) {     tw0p[m][mm]= 0.965844; } 
	  if(m==12 && mm == 27) {     tw0p[m][mm]= 0.476283; } 
	  if(m==12 && mm == 28) {     tw0p[m][mm]= 0.220633; } 
	  if(m==12 && mm == 30) {     tw0p[m][mm]= 0.168804; } 
	  if(m==12 && mm == 31) {     tw0p[m][mm]= 0.793758; } 
	  if(m==12 && mm == 32) {     tw0p[m][mm]= 0.030077; } 
	  if(m==12 && mm == 35) {     tw0p[m][mm]= 0.511066; } 
	  if(m==12 && mm == 6) {     tw1p[m][mm]= 0.380707; } 
	  if(m==12 && mm == 19) {     tw1p[m][mm]= 1.848603; } 
	  if(m==12 && mm == 20) {     tw1p[m][mm]= 0.763823; } 
	  if(m==12 && mm == 21) {     tw1p[m][mm]= 0.710769; } 
	  if(m==12 && mm == 22) {     tw1p[m][mm]= 0.761072; } 
	  if(m==12 && mm == 23) {     tw1p[m][mm]= 0.170118; } 
	  if(m==12 && mm == 25) {     tw1p[m][mm]= 0.502944; } 
	  if(m==12 && mm == 30) {     tw1p[m][mm]= 0.380702; } 
	  if(m==12 && mm == 31) {     tw1p[m][mm]= 0.519957; } 
	  if(m==12 && mm == 32) {     tw1p[m][mm]= 0.562979; } 
	  if(m==12 && mm == 33) {     tw1p[m][mm]= 0.315394; } 
	  if(m==12 && mm == 34) {     tw1p[m][mm]= 0.504687; } 
	  if(m==12 && mm == 35) {     tw1p[m][mm]= 0.353338; } 
	  if(m==12 && mm == 6) {     tw2p[m][mm]= 0.295798; } 
	  if(m==12 && mm == 7) {     tw2p[m][mm]= 0.346911; } 
	  if(m==12 && mm == 8) {     tw2p[m][mm]= 0.088479; } 
	  if(m==12 && mm == 9) {     tw2p[m][mm]= 0.296021; } 
	  if(m==12 && mm == 10) {     tw2p[m][mm]= 0.230443; } 
	  if(m==12 && mm == 11) {     tw2p[m][mm]= 0.063085; } 
	  if(m==12 && mm == 22) {     tw2p[m][mm]= 0.144736; } 
	  if(m==12 && mm == 29) {     tw2p[m][mm]= 0.169264; } 
	  if(m==12 && mm == 30) {     tw2p[m][mm]= 0.414788; } 
	  if(m==12 && mm == 31) {     tw2p[m][mm]= 0.516191; } 
	  if(m==12 && mm == 32) {     tw2p[m][mm]= 0.448684; } 
	  if(m==12 && mm == 33) {     tw2p[m][mm]= 0.616474; } 
	  if(m==12 && mm == 34) {     tw2p[m][mm]= 0.373059; } 
	  if(m==12 && mm == 2) {     tw3p[m][mm]= 0.507434; } 
	  if(m==12 && mm == 4) {     tw3p[m][mm]= 0.337250; } 
	  if(m==12 && mm == 5) {     tw3p[m][mm]= 0.461442; } 
	  if(m==12 && mm == 6) {     tw3p[m][mm]= 0.622453; } 
	  if(m==12 && mm == 7) {     tw3p[m][mm]= 0.556556; } 
	  if(m==12 && mm == 8) {     tw3p[m][mm]= 0.841916; } 
	  if(m==12 && mm == 9) {     tw3p[m][mm]= 0.943682; } 
	  if(m==12 && mm == 11) {     tw3p[m][mm]= 0.867485; } 
	  if(m==12 && mm == 12) {     tw3p[m][mm]= 0.013050; } 
	  if(m==12 && mm == 15) {     tw3p[m][mm]= 0.125201; } 
	  if(m==12 && mm == 23) {     tw3p[m][mm]= 0.443622; } 
	  if(m==12 && mm == 27) {     tw3p[m][mm]= 0.157671; } 
	  if(m==12 && mm == 29) {     tw3p[m][mm]= 0.187590; } 
	  if(m==12 && mm == 34) {     tw3p[m][mm]= 0.378761; } 
	  if(m==12 && mm == 0) {     te2p[m][mm]= 0.555868; } 
	  if(m==12 && mm == 1) {     te2p[m][mm]= 0.355915; } 
	  if(m==12 && mm == 3) {     te2p[m][mm]= 0.550186; } 
	  if(m==12 && mm == 4) {     te2p[m][mm]= 0.300323; } 
	  if(m==12 && mm == 5) {     te2p[m][mm]= 0.072895; } 
	  if(m==12 && mm == 7) {     te2p[m][mm]= 0.040928; } 
	  if(m==12 && mm == 8) {     te2p[m][mm]= 0.050055; } 
	  if(m==12 && mm == 9) {     te2p[m][mm]= 0.015310; } 
	  if(m==12 && mm == 10) {     te2p[m][mm]= 0.058072; } 
	  if(m==12 && mm == 12) {     te2p[m][mm]= 1.375606; } 
	  if(m==12 && mm == 13) {     te2p[m][mm]= 2.614078; } 
	  if(m==12 && mm == 15) {     te2p[m][mm]= 1.195064; } 
	  if(m==12 && mm == 17) {     te2p[m][mm]= 0.573545; } 
	  if(m==12 && mm == 18) {     te2p[m][mm]= 0.643349; } 
	  if(m==12 && mm == 19) {     te2p[m][mm]= 1.418200; } 
	  if(m==12 && mm == 20) {     te2p[m][mm]= 1.164817; } 
	  if(m==12 && mm == 21) {     te2p[m][mm]= 0.178674; } 
	  if(m==12 && mm == 22) {     te2p[m][mm]= 0.421058; } 
	  if(m==12 && mm == 23) {     te2p[m][mm]= 0.605354; } 
	  if(m==12 && mm == 25) {     te2p[m][mm]= 0.138342; } 
	  if(m==12 && mm == 26) {     te2p[m][mm]= 0.268757; } 
	  if(m==12 && mm == 27) {     te2p[m][mm]= 0.489716; } 
	  if(m==12 && mm == 29) {     te2p[m][mm]= 0.733132; } 
	  if(m==12 && mm == 31) {     te2p[m][mm]= 0.115781; } 
	  if(m==12 && mm == 4) {     te3p[m][mm]= 0.280913; } 
	  if(m==12 && mm == 5) {     te3p[m][mm]= 0.205225; } 
	  if(m==12 && mm == 7) {     te3p[m][mm]= 0.103142; } 
	  if(m==12 && mm == 8) {     te3p[m][mm]= 0.260463; } 
	  if(m==12 && mm == 9) {     te3p[m][mm]= 2.602278; } 
	  if(m==12 && mm == 10) {     te3p[m][mm]= 0.798437; } 
	  if(m==12 && mm == 11) {     te3p[m][mm]= 0.731463; } 
	  if(m==12 && mm == 12) {     te3p[m][mm]= 0.295143; } 
	  if(m==12 && mm == 14) {     te3p[m][mm]= 0.329018; } 
	  if(m==12 && mm == 15) {     te3p[m][mm]= 0.413548; } 
	  if(m==12 && mm == 16) {     te3p[m][mm]= 0.361360; } 
	  if(m==12 && mm == 18) {     te3p[m][mm]= 0.334380; } 
	  if(m==12 && mm == 19) {     te3p[m][mm]= 0.384230; } 
	  if(m==12 && mm == 20) {     te3p[m][mm]= 0.259695; } 
	  if(m==12 && mm == 21) {     te3p[m][mm]= 0.331077; } 
	  if(m==12 && mm == 22) {     te3p[m][mm]= 0.444887; } 
	  if(m==12 && mm == 0) {     tw0n[m][mm]= 0.510686; } 
	  if(m==12 && mm == 1) {     tw0n[m][mm]= 0.206842; } 
	  if(m==12 && mm == 4) {     tw0n[m][mm]= 0.255011; } 
	  if(m==12 && mm == 5) {     tw0n[m][mm]= 0.179549; } 
	  if(m==12 && mm == 8) {     tw0n[m][mm]= 0.298464; } 
	  if(m==12 && mm == 9) {     tw0n[m][mm]= 0.230455; } 
	  if(m==12 && mm == 12) {     tw0n[m][mm]= 0.358678; } 
	  if(m==12 && mm == 13) {     tw0n[m][mm]= 0.397031; } 
	  if(m==12 && mm == 14) {     tw0n[m][mm]= 0.344256; } 
	  if(m==12 && mm == 15) {     tw0n[m][mm]= 0.433035; } 
	  if(m==12 && mm == 16) {     tw0n[m][mm]= 0.105032; } 
	  if(m==12 && mm == 17) {     tw0n[m][mm]= 0.242898; } 
	  if(m==12 && mm == 18) {     tw0n[m][mm]= 0.193593; } 
	  if(m==12 && mm == 19) {     tw0n[m][mm]= 0.287967; } 
	  if(m==12 && mm == 21) {     tw0n[m][mm]= 0.013082; } 
	  if(m==12 && mm == 22) {     tw0n[m][mm]= 0.279563; } 
	  if(m==12 && mm == 23) {     tw0n[m][mm]= 0.413579; } 
	  if(m==12 && mm == 24) {     tw0n[m][mm]= 0.238501; } 
	  if(m==12 && mm == 25) {     tw0n[m][mm]= 0.001785; } 
	  if(m==12 && mm == 27) {     tw0n[m][mm]= 0.374980; } 
	  if(m==12 && mm == 28) {     tw0n[m][mm]= 0.069663; } 
	  if(m==12 && mm == 7) {     tw1n[m][mm]= 0.178716; } 
	  if(m==12 && mm == 11) {     tw1n[m][mm]= 0.015845; } 
	  if(m==12 && mm == 12) {     tw1n[m][mm]= 0.140018; } 
	  if(m==12 && mm == 13) {     tw1n[m][mm]= 0.426399; } 
	  if(m==12 && mm == 14) {     tw1n[m][mm]= 1.105617; } 
	  if(m==12 && mm == 15) {     tw1n[m][mm]= 0.408646; } 
	  if(m==12 && mm == 16) {     tw1n[m][mm]= 0.074949; } 
	  if(m==12 && mm == 17) {     tw1n[m][mm]= 0.113182; } 
	  if(m==12 && mm == 18) {     tw1n[m][mm]= 0.191489; } 
	  if(m==12 && mm == 20) {     tw1n[m][mm]= 0.130470; } 
	  if(m==12 && mm == 21) {     tw1n[m][mm]= 0.371769; } 
	  if(m==12 && mm == 23) {     tw1n[m][mm]= 0.202683; } 
	  if(m==12 && mm == 24) {     tw1n[m][mm]= 0.684348; } 
	  if(m==12 && mm == 30) {     tw1n[m][mm]= 0.297275; } 
	  if(m==12 && mm == 31) {     tw1n[m][mm]= 0.256884; } 
	  if(m==12 && mm == 32) {     tw1n[m][mm]= 0.288625; } 
	  if(m==12 && mm == 33) {     tw1n[m][mm]= 0.077333; } 
	  if(m==12 && mm == 34) {     tw1n[m][mm]= 0.314117; } 
	  if(m==12 && mm == 35) {     tw1n[m][mm]= 0.183928; } 
	  if(m==12 && mm == 2) {     tw2n[m][mm]= 0.082572; } 
	  if(m==12 && mm == 3) {     tw2n[m][mm]= 0.439073; } 
	  if(m==12 && mm == 4) {     tw2n[m][mm]= 0.227569; } 
	  if(m==12 && mm == 5) {     tw2n[m][mm]= 0.608185; } 
	  if(m==12 && mm == 23) {     tw2n[m][mm]= 0.140554; } 
	  if(m==12 && mm == 24) {     tw2n[m][mm]= 0.438747; } 
	  if(m==12 && mm == 25) {     tw2n[m][mm]= 0.699156; } 
	  if(m==12 && mm == 26) {     tw2n[m][mm]= 0.198508; } 
	  if(m==12 && mm == 27) {     tw2n[m][mm]= 0.339500; } 
	  if(m==12 && mm == 28) {     tw2n[m][mm]= 0.024051; } 
	  if(m==12 && mm == 29) {     tw2n[m][mm]= 0.037372; } 
	  if(m==12 && mm == 30) {     tw2n[m][mm]= 0.268611; } 
	  if(m==12 && mm == 31) {     tw2n[m][mm]= 0.225969; } 
	  if(m==12 && mm == 32) {     tw2n[m][mm]= 0.190315; } 
	  if(m==12 && mm == 33) {     tw2n[m][mm]= 0.352230; } 
	  if(m==12 && mm == 34) {     tw2n[m][mm]= 0.263683; } 
	  if(m==12 && mm == 2) {     tw3n[m][mm]= 0.755372; } 
	  if(m==12 && mm == 3) {     tw3n[m][mm]= 0.053941; } 
	  if(m==12 && mm == 5) {     tw3n[m][mm]= 0.439888; } 
	  if(m==12 && mm == 7) {     tw3n[m][mm]= 0.732516; } 
	  if(m==12 && mm == 8) {     tw3n[m][mm]= 0.177442; } 
	  if(m==12 && mm == 9) {     tw3n[m][mm]= 0.903070; } 
	  if(m==12 && mm == 11) {     tw3n[m][mm]= 0.627820; } 
	  if(m==12 && mm == 15) {     tw3n[m][mm]= 0.012571; } 
	  if(m==12 && mm == 23) {     tw3n[m][mm]= 0.344342; } 
	  if(m==12 && mm == 29) {     tw3n[m][mm]= 0.041983; } 
	  if(m==12 && mm == 9) {     te2n[m][mm]= 0.004729; } 
	  if(m==12 && mm == 12) {     te2n[m][mm]= 1.135107; } 
	  if(m==12 && mm == 13) {     te2n[m][mm]= 1.833283; } 
	  if(m==12 && mm == 15) {     te2n[m][mm]= 0.366123; } 
	  if(m==12 && mm == 16) {     te2n[m][mm]= 0.428461; } 
	  if(m==12 && mm == 17) {     te2n[m][mm]= 0.698437; } 
	  if(m==12 && mm == 18) {     te2n[m][mm]= 0.210896; } 
	  if(m==12 && mm == 19) {     te2n[m][mm]= 0.902682; } 
	  if(m==12 && mm == 20) {     te2n[m][mm]= 0.760511; } 
	  if(m==12 && mm == 22) {     te2n[m][mm]= 0.570026; } 
	  if(m==12 && mm == 23) {     te2n[m][mm]= 1.020314; } 
	  if(m==12 && mm == 24) {     te2n[m][mm]= 0.039611; } 
	  if(m==12 && mm == 25) {     te2n[m][mm]= 0.354753; } 
	  if(m==12 && mm == 26) {     te2n[m][mm]= 0.480461; } 
	  if(m==12 && mm == 27) {     te2n[m][mm]= 0.511484; } 
	  if(m==12 && mm == 29) {     te2n[m][mm]= 1.516277; } 
	  if(m==12 && mm == 32) {     te2n[m][mm]= 0.154111; } 
	  if(m==12 && mm == 33) {     te2n[m][mm]= 0.628037; } 
	  if(m==12 && mm == 2) {     te3n[m][mm]= 0.483102; } 
	  if(m==12 && mm == 4) {     te3n[m][mm]= 0.426416; } 
	  if(m==12 && mm == 17) {     te3n[m][mm]= 0.078219; } 
	  if(m==12 && mm == 18) {     te3n[m][mm]= 0.289180; } 
	  if(m==12 && mm == 19) {     te3n[m][mm]= 0.418875; } 
	  if(m==12 && mm == 20) {     te3n[m][mm]= 0.513179; } 
	  if(m==12 && mm == 21) {     te3n[m][mm]= 0.272612; } 
	  if(m==12 && mm == 22) {     te3n[m][mm]= 0.383995; } 
	  if(m==12 && mm == 23) {     te3n[m][mm]= 0.147444; } 
	  if(m==12 && mm == 24) {     te3n[m][mm]= 0.229206; } 
	  if(m==12 && mm == 25) {     te3n[m][mm]= 0.214901; } 
	  if(m==12 && mm == 26) {     te3n[m][mm]= 0.288783; } 
	  if(m==12 && mm == 28) {     te3n[m][mm]= 0.071201; } 
	  if(m==12 && mm == 31) {     te3n[m][mm]= 0.392158; } 
	  if(m==12 && mm == 33) {     te3n[m][mm]= 0.735171; } 
	  if(m==12 && mm == 35) {     te3n[m][mm]= 0.138241; } 
	  if(m==13 && mm == 7) {     tw0p[m][mm]= 0.159146; } 
	  if(m==13 && mm == 11) {     tw0p[m][mm]= 0.293574; } 
	  if(m==13 && mm == 12) {     tw0p[m][mm]= 0.193471; } 
	  if(m==13 && mm == 13) {     tw0p[m][mm]= 0.474975; } 
	  if(m==13 && mm == 14) {     tw0p[m][mm]= 0.156348; } 
	  if(m==13 && mm == 15) {     tw0p[m][mm]= 0.356510; } 
	  if(m==13 && mm == 16) {     tw0p[m][mm]= 0.519180; } 
	  if(m==13 && mm == 17) {     tw0p[m][mm]= 0.440058; } 
	  if(m==13 && mm == 18) {     tw0p[m][mm]= 0.299935; } 
	  if(m==13 && mm == 19) {     tw0p[m][mm]= 0.050219; } 
	  if(m==13 && mm == 21) {     tw0p[m][mm]= 0.590386; } 
	  if(m==13 && mm == 22) {     tw0p[m][mm]= 0.447865; } 
	  if(m==13 && mm == 23) {     tw0p[m][mm]= 0.242968; } 
	  if(m==13 && mm == 24) {     tw0p[m][mm]= 0.009463; } 
	  if(m==13 && mm == 27) {     tw0p[m][mm]= 0.290582; } 
	  if(m==13 && mm == 28) {     tw0p[m][mm]= 0.196642; } 
	  if(m==13 && mm == 29) {     tw0p[m][mm]= 0.473927; } 
	  if(m==13 && mm == 30) {     tw0p[m][mm]= 0.393354; } 
	  if(m==13 && mm == 31) {     tw0p[m][mm]= 0.600071; } 
	  if(m==13 && mm == 33) {     tw0p[m][mm]= 0.441467; } 
	  if(m==13 && mm == 34) {     tw0p[m][mm]= 0.141562; } 
	  if(m==13 && mm == 35) {     tw0p[m][mm]= 0.112334; } 
	  if(m==13 && mm == 0) {     tw1p[m][mm]= 0.156117; } 
	  if(m==13 && mm == 4) {     tw1p[m][mm]= 0.223073; } 
	  if(m==13 && mm == 5) {     tw1p[m][mm]= 0.211348; } 
	  if(m==13 && mm == 20) {     tw1p[m][mm]= 1.124612; } 
	  if(m==13 && mm == 21) {     tw1p[m][mm]= 1.224945; } 
	  if(m==13 && mm == 22) {     tw1p[m][mm]= 0.203913; } 
	  if(m==13 && mm == 23) {     tw1p[m][mm]= 0.460382; } 
	  if(m==13 && mm == 24) {     tw1p[m][mm]= 0.694913; } 
	  if(m==13 && mm == 25) {     tw1p[m][mm]= 3.138885; } 
	  if(m==13 && mm == 26) {     tw1p[m][mm]= 0.895243; } 
	  if(m==13 && mm == 27) {     tw1p[m][mm]= 0.924385; } 
	  if(m==13 && mm == 28) {     tw1p[m][mm]= 0.117390; } 
	  if(m==13 && mm == 29) {     tw1p[m][mm]= 0.120716; } 
	  if(m==13 && mm == 30) {     tw1p[m][mm]= 0.438018; } 
	  if(m==13 && mm == 31) {     tw1p[m][mm]= 0.183548; } 
	  if(m==13 && mm == 32) {     tw1p[m][mm]= 0.537447; } 
	  if(m==13 && mm == 33) {     tw1p[m][mm]= 0.405523; } 
	  if(m==13 && mm == 34) {     tw1p[m][mm]= 0.766747; } 
	  if(m==13 && mm == 35) {     tw1p[m][mm]= 0.471981; } 
	  if(m==13 && mm == 5) {     tw2p[m][mm]= 0.448975; } 
	  if(m==13 && mm == 6) {     tw2p[m][mm]= 0.608111; } 
	  if(m==13 && mm == 7) {     tw2p[m][mm]= 0.091468; } 
	  if(m==13 && mm == 8) {     tw2p[m][mm]= 0.917330; } 
	  if(m==13 && mm == 9) {     tw2p[m][mm]= 0.593445; } 
	  if(m==13 && mm == 10) {     tw2p[m][mm]= 0.629855; } 
	  if(m==13 && mm == 11) {     tw2p[m][mm]= 0.553427; } 
	  if(m==13 && mm == 13) {     tw2p[m][mm]= 0.368897; } 
	  if(m==13 && mm == 15) {     tw2p[m][mm]= 0.367518; } 
	  if(m==13 && mm == 17) {     tw2p[m][mm]= 0.455681; } 
	  if(m==13 && mm == 30) {     tw2p[m][mm]= 0.702633; } 
	  if(m==13 && mm == 31) {     tw2p[m][mm]= 0.226956; } 
	  if(m==13 && mm == 32) {     tw2p[m][mm]= 0.134645; } 
	  if(m==13 && mm == 33) {     tw2p[m][mm]= 0.146667; } 
	  if(m==13 && mm == 34) {     tw2p[m][mm]= 0.973718; } 
	  if(m==13 && mm == 35) {     tw2p[m][mm]= 0.252297; } 
	  if(m==13 && mm == 2) {     tw3p[m][mm]= 0.820763; } 
	  if(m==13 && mm == 3) {     tw3p[m][mm]= 0.544669; } 
	  if(m==13 && mm == 4) {     tw3p[m][mm]= 0.262077; } 
	  if(m==13 && mm == 5) {     tw3p[m][mm]= 1.162486; } 
	  if(m==13 && mm == 6) {     tw3p[m][mm]= 0.628920; } 
	  if(m==13 && mm == 7) {     tw3p[m][mm]= 0.517497; } 
	  if(m==13 && mm == 8) {     tw3p[m][mm]= 0.860061; } 
	  if(m==13 && mm == 9) {     tw3p[m][mm]= 0.562963; } 
	  if(m==13 && mm == 11) {     tw3p[m][mm]= 0.471151; } 
	  if(m==13 && mm == 17) {     tw3p[m][mm]= 0.221467; } 
	  if(m==13 && mm == 20) {     tw3p[m][mm]= 0.454436; } 
	  if(m==13 && mm == 21) {     tw3p[m][mm]= 0.249023; } 
	  if(m==13 && mm == 22) {     tw3p[m][mm]= 0.102341; } 
	  if(m==13 && mm == 23) {     tw3p[m][mm]= 0.636055; } 
	  if(m==13 && mm == 24) {     tw3p[m][mm]= 0.213875; } 
	  if(m==13 && mm == 25) {     tw3p[m][mm]= 0.801283; } 
	  if(m==13 && mm == 27) {     tw3p[m][mm]= 0.477337; } 
	  if(m==13 && mm == 30) {     tw3p[m][mm]= 0.183174; } 
	  if(m==13 && mm == 31) {     tw3p[m][mm]= 1.254906; } 
	  if(m==13 && mm == 0) {     te2p[m][mm]= 0.319165; } 
	  if(m==13 && mm == 1) {     te2p[m][mm]= 0.217941; } 
	  if(m==13 && mm == 2) {     te2p[m][mm]= 0.003678; } 
	  if(m==13 && mm == 3) {     te2p[m][mm]= 0.532102; } 
	  if(m==13 && mm == 4) {     te2p[m][mm]= 0.467982; } 
	  if(m==13 && mm == 5) {     te2p[m][mm]= 0.125646; } 
	  if(m==13 && mm == 7) {     te2p[m][mm]= 0.765919; } 
	  if(m==13 && mm == 8) {     te2p[m][mm]= 0.564673; } 
	  if(m==13 && mm == 9) {     te2p[m][mm]= 0.277656; } 
	  if(m==13 && mm == 10) {     te2p[m][mm]= 0.310641; } 
	  if(m==13 && mm == 11) {     te2p[m][mm]= 0.233155; } 
	  if(m==13 && mm == 12) {     te2p[m][mm]= 0.376469; } 
	  if(m==13 && mm == 13) {     te2p[m][mm]= 0.978617; } 
	  if(m==13 && mm == 14) {     te2p[m][mm]= 0.310575; } 
	  if(m==13 && mm == 15) {     te2p[m][mm]= 0.336334; } 
	  if(m==13 && mm == 16) {     te2p[m][mm]= 0.978658; } 
	  if(m==13 && mm == 17) {     te2p[m][mm]= 0.179473; } 
	  if(m==13 && mm == 19) {     te2p[m][mm]= 0.345585; } 
	  if(m==13 && mm == 20) {     te2p[m][mm]= 0.455359; } 
	  if(m==13 && mm == 21) {     te2p[m][mm]= 1.056399; } 
	  if(m==13 && mm == 22) {     te2p[m][mm]= 0.522040; } 
	  if(m==13 && mm == 23) {     te2p[m][mm]= 0.404271; } 
	  if(m==13 && mm == 24) {     te2p[m][mm]= 0.039688; } 
	  if(m==13 && mm == 25) {     te2p[m][mm]= 0.146292; } 
	  if(m==13 && mm == 27) {     te2p[m][mm]= 0.076713; } 
	  if(m==13 && mm == 29) {     te2p[m][mm]= 0.264432; } 
	  if(m==13 && mm == 30) {     te2p[m][mm]= 0.377952; } 
	  if(m==13 && mm == 6) {     te3p[m][mm]= 1.176370; } 
	  if(m==13 && mm == 7) {     te3p[m][mm]= 0.200993; } 
	  if(m==13 && mm == 8) {     te3p[m][mm]= 0.480376; } 
	  if(m==13 && mm == 9) {     te3p[m][mm]= 0.406390; } 
	  if(m==13 && mm == 10) {     te3p[m][mm]= 0.710422; } 
	  if(m==13 && mm == 11) {     te3p[m][mm]= 0.569492; } 
	  if(m==13 && mm == 13) {     te3p[m][mm]= 0.409854; } 
	  if(m==13 && mm == 14) {     te3p[m][mm]= 0.142601; } 
	  if(m==13 && mm == 17) {     te3p[m][mm]= 0.495787; } 
	  if(m==13 && mm == 18) {     te3p[m][mm]= 0.577390; } 
	  if(m==13 && mm == 19) {     te3p[m][mm]= 0.121462; } 
	  if(m==13 && mm == 20) {     te3p[m][mm]= 0.865981; } 
	  if(m==13 && mm == 21) {     te3p[m][mm]= 0.180682; } 
	  if(m==13 && mm == 22) {     te3p[m][mm]= 0.014012; } 
	  if(m==13 && mm == 23) {     te3p[m][mm]= 0.016351; } 
	  if(m==13 && mm == 4) {     tw0n[m][mm]= 0.134505; } 
	  if(m==13 && mm == 5) {     tw0n[m][mm]= 0.166060; } 
	  if(m==13 && mm == 7) {     tw0n[m][mm]= 0.094534; } 
	  if(m==13 && mm == 8) {     tw0n[m][mm]= 0.098986; } 
	  if(m==13 && mm == 11) {     tw0n[m][mm]= 0.340625; } 
	  if(m==13 && mm == 12) {     tw0n[m][mm]= 0.297896; } 
	  if(m==13 && mm == 13) {     tw0n[m][mm]= 0.717268; } 
	  if(m==13 && mm == 14) {     tw0n[m][mm]= 0.323830; } 
	  if(m==13 && mm == 16) {     tw0n[m][mm]= 0.582899; } 
	  if(m==13 && mm == 17) {     tw0n[m][mm]= 0.482712; } 
	  if(m==13 && mm == 18) {     tw0n[m][mm]= 0.146120; } 
	  if(m==13 && mm == 20) {     tw0n[m][mm]= 0.079254; } 
	  if(m==13 && mm == 21) {     tw0n[m][mm]= 0.493709; } 
	  if(m==13 && mm == 22) {     tw0n[m][mm]= 0.433432; } 
	  if(m==13 && mm == 23) {     tw0n[m][mm]= 0.112159; } 
	  if(m==13 && mm == 25) {     tw0n[m][mm]= 0.993213; } 
	  if(m==13 && mm == 26) {     tw0n[m][mm]= 0.507498; } 
	  if(m==13 && mm == 27) {     tw0n[m][mm]= 0.037173; } 
	  if(m==13 && mm == 28) {     tw0n[m][mm]= 0.185490; } 
	  if(m==13 && mm == 29) {     tw0n[m][mm]= 0.078950; } 
	  if(m==13 && mm == 30) {     tw0n[m][mm]= 0.657305; } 
	  if(m==13 && mm == 6) {     tw1n[m][mm]= 0.260485; } 
	  if(m==13 && mm == 7) {     tw1n[m][mm]= 0.259176; } 
	  if(m==13 && mm == 11) {     tw1n[m][mm]= 0.094104; } 
	  if(m==13 && mm == 12) {     tw1n[m][mm]= 0.184063; } 
	  if(m==13 && mm == 13) {     tw1n[m][mm]= 0.561871; } 
	  if(m==13 && mm == 14) {     tw1n[m][mm]= 0.506138; } 
	  if(m==13 && mm == 16) {     tw1n[m][mm]= 0.199678; } 
	  if(m==13 && mm == 17) {     tw1n[m][mm]= 0.095889; } 
	  if(m==13 && mm == 18) {     tw1n[m][mm]= 0.320174; } 
	  if(m==13 && mm == 19) {     tw1n[m][mm]= 0.141017; } 
	  if(m==13 && mm == 20) {     tw1n[m][mm]= 1.102302; } 
	  if(m==13 && mm == 21) {     tw1n[m][mm]= 0.998388; } 
	  if(m==13 && mm == 23) {     tw1n[m][mm]= 0.462610; } 
	  if(m==13 && mm == 24) {     tw1n[m][mm]= 0.813381; } 
	  if(m==13 && mm == 29) {     tw1n[m][mm]= 0.080260; } 
	  if(m==13 && mm == 30) {     tw1n[m][mm]= 0.321687; } 
	  if(m==13 && mm == 32) {     tw1n[m][mm]= 0.398963; } 
	  if(m==13 && mm == 33) {     tw1n[m][mm]= 0.023322; } 
	  if(m==13 && mm == 34) {     tw1n[m][mm]= 0.379881; } 
	  if(m==13 && mm == 35) {     tw1n[m][mm]= 0.187306; } 
	  if(m==13 && mm == 0) {     tw2n[m][mm]= 0.203534; } 
	  if(m==13 && mm == 1) {     tw2n[m][mm]= 0.151385; } 
	  if(m==13 && mm == 2) {     tw2n[m][mm]= 0.454004; } 
	  if(m==13 && mm == 4) {     tw2n[m][mm]= 0.404645; } 
	  if(m==13 && mm == 6) {     tw2n[m][mm]= 0.651960; } 
	  if(m==13 && mm == 15) {     tw2n[m][mm]= 0.252080; } 
	  if(m==13 && mm == 17) {     tw2n[m][mm]= 0.655976; } 
	  if(m==13 && mm == 23) {     tw2n[m][mm]= 0.390733; } 
	  if(m==13 && mm == 24) {     tw2n[m][mm]= 0.330164; } 
	  if(m==13 && mm == 30) {     tw2n[m][mm]= 0.480606; } 
	  if(m==13 && mm == 31) {     tw2n[m][mm]= 0.067150; } 
	  if(m==13 && mm == 34) {     tw2n[m][mm]= 0.699176; } 
	  if(m==13 && mm == 35) {     tw2n[m][mm]= 0.053545; } 
	  if(m==13 && mm == 2) {     tw3n[m][mm]= 1.061967; } 
	  if(m==13 && mm == 3) {     tw3n[m][mm]= 0.719826; } 
	  if(m==13 && mm == 4) {     tw3n[m][mm]= 0.291246; } 
	  if(m==13 && mm == 5) {     tw3n[m][mm]= 1.121235; } 
	  if(m==13 && mm == 6) {     tw3n[m][mm]= 0.753592; } 
	  if(m==13 && mm == 7) {     tw3n[m][mm]= 0.466508; } 
	  if(m==13 && mm == 8) {     tw3n[m][mm]= 0.869804; } 
	  if(m==13 && mm == 9) {     tw3n[m][mm]= 0.533748; } 
	  if(m==13 && mm == 11) {     tw3n[m][mm]= 0.472823; } 
	  if(m==13 && mm == 17) {     tw3n[m][mm]= 0.035219; } 
	  if(m==13 && mm == 20) {     tw3n[m][mm]= 0.301307; } 
	  if(m==13 && mm == 21) {     tw3n[m][mm]= 0.217184; } 
	  if(m==13 && mm == 23) {     tw3n[m][mm]= 0.512945; } 
	  if(m==13 && mm == 25) {     tw3n[m][mm]= 0.229744; } 
	  if(m==13 && mm == 4) {     te2n[m][mm]= 0.465309; } 
	  if(m==13 && mm == 8) {     te2n[m][mm]= 0.377730; } 
	  if(m==13 && mm == 9) {     te2n[m][mm]= 0.379395; } 
	  if(m==13 && mm == 11) {     te2n[m][mm]= 0.124138; } 
	  if(m==13 && mm == 12) {     te2n[m][mm]= 0.137763; } 
	  if(m==13 && mm == 13) {     te2n[m][mm]= 0.997311; } 
	  if(m==13 && mm == 14) {     te2n[m][mm]= 0.100593; } 
	  if(m==13 && mm == 15) {     te2n[m][mm]= 0.206609; } 
	  if(m==13 && mm == 16) {     te2n[m][mm]= 0.559311; } 
	  if(m==13 && mm == 19) {     te2n[m][mm]= 0.422829; } 
	  if(m==13 && mm == 20) {     te2n[m][mm]= 0.259735; } 
	  if(m==13 && mm == 21) {     te2n[m][mm]= 1.043870; } 
	  if(m==13 && mm == 22) {     te2n[m][mm]= 0.544681; } 
	  if(m==13 && mm == 23) {     te2n[m][mm]= 0.318906; } 
	  if(m==13 && mm == 24) {     te2n[m][mm]= 0.209250; } 
	  if(m==13 && mm == 25) {     te2n[m][mm]= 0.024401; } 
	  if(m==13 && mm == 27) {     te2n[m][mm]= 0.502825; } 
	  if(m==13 && mm == 29) {     te2n[m][mm]= 0.230201; } 
	  if(m==13 && mm == 32) {     te2n[m][mm]= 0.124747; } 
	  if(m==13 && mm == 33) {     te2n[m][mm]= 0.505491; } 
	  if(m==13 && mm == 34) {     te2n[m][mm]= 1.019137; } 
	  if(m==13 && mm == 35) {     te2n[m][mm]= 0.098706; } 
	  if(m==13 && mm == 1) {     te3n[m][mm]= 0.428109; } 
	  if(m==13 && mm == 2) {     te3n[m][mm]= 0.876987; } 
	  if(m==13 && mm == 3) {     te3n[m][mm]= 0.334859; } 
	  if(m==13 && mm == 4) {     te3n[m][mm]= 0.080271; } 
	  if(m==13 && mm == 5) {     te3n[m][mm]= 0.867406; } 
	  if(m==13 && mm == 17) {     te3n[m][mm]= 0.539893; } 
	  if(m==13 && mm == 18) {     te3n[m][mm]= 0.476918; } 
	  if(m==13 && mm == 19) {     te3n[m][mm]= 0.187379; } 
	  if(m==13 && mm == 20) {     te3n[m][mm]= 0.460735; } 
	  if(m==13 && mm == 21) {     te3n[m][mm]= 0.045205; } 
	  if(m==13 && mm == 22) {     te3n[m][mm]= 0.064646; } 
	  if(m==13 && mm == 23) {     te3n[m][mm]= 0.202051; } 
	  if(m==13 && mm == 24) {     te3n[m][mm]= 0.192202; } 
	  if(m==13 && mm == 25) {     te3n[m][mm]= 0.350548; } 
	  if(m==13 && mm == 26) {     te3n[m][mm]= 0.059222; } 
	  if(m==13 && mm == 30) {     te3n[m][mm]= 0.197650; } 
	  if(m==13 && mm == 32) {     te3n[m][mm]= 0.615887; } 
	  if(m==13 && mm == 33) {     te3n[m][mm]= 0.190110; } 
	  if(m==13 && mm == 34) {     te3n[m][mm]= 0.185333; } 
	  if(m==13 && mm == 35) {     te3n[m][mm]= 0.778096; } 
	  if(m==14 && mm == 9) {     tw0p[m][mm]= 0.123230; } 
	  if(m==14 && mm == 12) {     tw0p[m][mm]= 0.145927; } 
	  if(m==14 && mm == 13) {     tw0p[m][mm]= 0.205248; } 
	  if(m==14 && mm == 15) {     tw0p[m][mm]= 0.050097; } 
	  if(m==14 && mm == 16) {     tw0p[m][mm]= 0.180656; } 
	  if(m==14 && mm == 17) {     tw0p[m][mm]= 0.055794; } 
	  if(m==14 && mm == 18) {     tw0p[m][mm]= 0.168628; } 
	  if(m==14 && mm == 19) {     tw0p[m][mm]= 0.019918; } 
	  if(m==14 && mm == 22) {     tw0p[m][mm]= 0.455898; } 
	  if(m==14 && mm == 23) {     tw0p[m][mm]= 0.487880; } 
	  if(m==14 && mm == 25) {     tw0p[m][mm]= 0.203608; } 
	  if(m==14 && mm == 26) {     tw0p[m][mm]= 0.133172; } 
	  if(m==14 && mm == 27) {     tw0p[m][mm]= 0.397217; } 
	  if(m==14 && mm == 30) {     tw0p[m][mm]= 0.213808; } 
	  if(m==14 && mm == 31) {     tw0p[m][mm]= 0.291292; } 
	  if(m==14 && mm == 32) {     tw0p[m][mm]= 0.269262; } 
	  if(m==14 && mm == 33) {     tw0p[m][mm]= 0.209977; } 
	  if(m==14 && mm == 34) {     tw0p[m][mm]= 0.528395; } 
	  if(m==14 && mm == 35) {     tw0p[m][mm]= 0.241330; } 
	  if(m==14 && mm == 7) {     tw1p[m][mm]= 0.246052; } 
	  if(m==14 && mm == 19) {     tw1p[m][mm]= 0.366433; } 
	  if(m==14 && mm == 20) {     tw1p[m][mm]= 0.190000; } 
	  if(m==14 && mm == 21) {     tw1p[m][mm]= 0.018171; } 
	  if(m==14 && mm == 23) {     tw1p[m][mm]= 0.243705; } 
	  if(m==14 && mm == 24) {     tw1p[m][mm]= 1.280815; } 
	  if(m==14 && mm == 26) {     tw1p[m][mm]= 0.396407; } 
	  if(m==14 && mm == 27) {     tw1p[m][mm]= 0.568399; } 
	  if(m==14 && mm == 30) {     tw1p[m][mm]= 0.545251; } 
	  if(m==14 && mm == 31) {     tw1p[m][mm]= 0.751621; } 
	  if(m==14 && mm == 32) {     tw1p[m][mm]= 1.816260; } 
	  if(m==14 && mm == 33) {     tw1p[m][mm]= 0.668451; } 
	  if(m==14 && mm == 34) {     tw1p[m][mm]= 0.204930; } 
	  if(m==14 && mm == 35) {     tw1p[m][mm]= 0.700217; } 
	  if(m==14 && mm == 5) {     tw2p[m][mm]= 0.084070; } 
	  if(m==14 && mm == 6) {     tw2p[m][mm]= 0.241743; } 
	  if(m==14 && mm == 7) {     tw2p[m][mm]= 0.282179; } 
	  if(m==14 && mm == 9) {     tw2p[m][mm]= 0.372674; } 
	  if(m==14 && mm == 10) {     tw2p[m][mm]= 0.413690; } 
	  if(m==14 && mm == 11) {     tw2p[m][mm]= 0.278596; } 
	  if(m==14 && mm == 18) {     tw2p[m][mm]= 0.157621; } 
	  if(m==14 && mm == 19) {     tw2p[m][mm]= 0.348295; } 
	  if(m==14 && mm == 28) {     tw2p[m][mm]= 0.022916; } 
	  if(m==14 && mm == 29) {     tw2p[m][mm]= 0.325933; } 
	  if(m==14 && mm == 30) {     tw2p[m][mm]= 0.188925; } 
	  if(m==14 && mm == 31) {     tw2p[m][mm]= 0.173291; } 
	  if(m==14 && mm == 32) {     tw2p[m][mm]= 1.129452; } 
	  if(m==14 && mm == 33) {     tw2p[m][mm]= 0.068175; } 
	  if(m==14 && mm == 34) {     tw2p[m][mm]= 0.252526; } 
	  if(m==14 && mm == 35) {     tw2p[m][mm]= 0.176881; } 
	  if(m==14 && mm == 6) {     tw3p[m][mm]= 0.284286; } 
	  if(m==14 && mm == 7) {     tw3p[m][mm]= 0.073310; } 
	  if(m==14 && mm == 8) {     tw3p[m][mm]= 0.349692; } 
	  if(m==14 && mm == 9) {     tw3p[m][mm]= 0.879545; } 
	  if(m==14 && mm == 10) {     tw3p[m][mm]= 0.515246; } 
	  if(m==14 && mm == 11) {     tw3p[m][mm]= 1.086574; } 
	  if(m==14 && mm == 20) {     tw3p[m][mm]= 0.003654; } 
	  if(m==14 && mm == 22) {     tw3p[m][mm]= 0.049931; } 
	  if(m==14 && mm == 25) {     tw3p[m][mm]= 0.332365; } 
	  if(m==14 && mm == 26) {     tw3p[m][mm]= 1.038339; } 
	  if(m==14 && mm == 27) {     tw3p[m][mm]= 1.068212; } 
	  if(m==14 && mm == 28) {     tw3p[m][mm]= 0.179991; } 
	  if(m==14 && mm == 30) {     tw3p[m][mm]= 0.343831; } 
	  if(m==14 && mm == 6) {     te2p[m][mm]= 0.365629; } 
	  if(m==14 && mm == 7) {     te2p[m][mm]= 0.139409; } 
	  if(m==14 && mm == 8) {     te2p[m][mm]= 0.118741; } 
	  if(m==14 && mm == 10) {     te2p[m][mm]= 0.036331; } 
	  if(m==14 && mm == 12) {     te2p[m][mm]= 0.390850; } 
	  if(m==14 && mm == 13) {     te2p[m][mm]= 0.056555; } 
	  if(m==14 && mm == 14) {     te2p[m][mm]= 2.722423; } 
	  if(m==14 && mm == 15) {     te2p[m][mm]= 0.175303; } 
	  if(m==14 && mm == 16) {     te2p[m][mm]= 0.662614; } 
	  if(m==14 && mm == 17) {     te2p[m][mm]= 1.184670; } 
	  if(m==14 && mm == 18) {     te2p[m][mm]= 0.241550; } 
	  if(m==14 && mm == 19) {     te2p[m][mm]= 0.209446; } 
	  if(m==14 && mm == 20) {     te2p[m][mm]= 0.106854; } 
	  if(m==14 && mm == 21) {     te2p[m][mm]= 0.254703; } 
	  if(m==14 && mm == 22) {     te2p[m][mm]= 0.679522; } 
	  if(m==14 && mm == 23) {     te2p[m][mm]= 0.350147; } 
	  if(m==14 && mm == 25) {     te2p[m][mm]= 0.222902; } 
	  if(m==14 && mm == 26) {     te2p[m][mm]= 0.306901; } 
	  if(m==14 && mm == 29) {     te2p[m][mm]= 0.605132; } 
	  if(m==14 && mm == 30) {     te2p[m][mm]= 0.010057; } 
	  if(m==14 && mm == 9) {     te3p[m][mm]= 0.757509; } 
	  if(m==14 && mm == 10) {     te3p[m][mm]= 0.279955; } 
	  if(m==14 && mm == 11) {     te3p[m][mm]= 0.396393; } 
	  if(m==14 && mm == 12) {     te3p[m][mm]= 0.273236; } 
	  if(m==14 && mm == 13) {     te3p[m][mm]= 0.139309; } 
	  if(m==14 && mm == 14) {     te3p[m][mm]= 0.438907; } 
	  if(m==14 && mm == 15) {     te3p[m][mm]= 0.625146; } 
	  if(m==14 && mm == 17) {     te3p[m][mm]= 0.093139; } 
	  if(m==14 && mm == 18) {     te3p[m][mm]= 0.209044; } 
	  if(m==14 && mm == 20) {     te3p[m][mm]= 0.003687; } 
	  if(m==14 && mm == 21) {     te3p[m][mm]= 0.637234; } 
	  if(m==14 && mm == 22) {     te3p[m][mm]= 0.620933; } 
	  if(m==14 && mm == 23) {     te3p[m][mm]= 1.092994; } 
	  if(m==14 && mm == 0) {     tw0n[m][mm]= 0.139291; } 
	  if(m==14 && mm == 2) {     tw0n[m][mm]= 0.341058; } 
	  if(m==14 && mm == 3) {     tw0n[m][mm]= 0.105822; } 
	  if(m==14 && mm == 5) {     tw0n[m][mm]= 0.470132; } 
	  if(m==14 && mm == 6) {     tw0n[m][mm]= 0.289548; } 
	  if(m==14 && mm == 9) {     tw0n[m][mm]= 0.150028; } 
	  if(m==14 && mm == 12) {     tw0n[m][mm]= 0.473473; } 
	  if(m==14 && mm == 13) {     tw0n[m][mm]= 0.457221; } 
	  if(m==14 && mm == 15) {     tw0n[m][mm]= 0.024392; } 
	  if(m==14 && mm == 18) {     tw0n[m][mm]= 0.202624; } 
	  if(m==14 && mm == 19) {     tw0n[m][mm]= 0.047770; } 
	  if(m==14 && mm == 22) {     tw0n[m][mm]= 0.496593; } 
	  if(m==14 && mm == 23) {     tw0n[m][mm]= 0.301794; } 
	  if(m==14 && mm == 24) {     tw0n[m][mm]= 0.207889; } 
	  if(m==14 && mm == 25) {     tw0n[m][mm]= 0.107839; } 
	  if(m==14 && mm == 27) {     tw0n[m][mm]= 0.140930; } 
	  if(m==14 && mm == 30) {     tw0n[m][mm]= 0.086294; } 
	  if(m==14 && mm == 6) {     tw1n[m][mm]= 0.290641; } 
	  if(m==14 && mm == 12) {     tw1n[m][mm]= 0.825410; } 
	  if(m==14 && mm == 13) {     tw1n[m][mm]= 0.449557; } 
	  if(m==14 && mm == 15) {     tw1n[m][mm]= 0.483113; } 
	  if(m==14 && mm == 16) {     tw1n[m][mm]= 0.326791; } 
	  if(m==14 && mm == 17) {     tw1n[m][mm]= 0.006227; } 
	  if(m==14 && mm == 18) {     tw1n[m][mm]= 0.403005; } 
	  if(m==14 && mm == 19) {     tw1n[m][mm]= 0.220658; } 
	  if(m==14 && mm == 20) {     tw1n[m][mm]= 0.170623; } 
	  if(m==14 && mm == 23) {     tw1n[m][mm]= 0.623217; } 
	  if(m==14 && mm == 24) {     tw1n[m][mm]= 0.479521; } 
	  if(m==14 && mm == 30) {     tw1n[m][mm]= 0.055640; } 
	  if(m==14 && mm == 31) {     tw1n[m][mm]= 0.300138; } 
	  if(m==14 && mm == 32) {     tw1n[m][mm]= 0.273407; } 
	  if(m==14 && mm == 33) {     tw1n[m][mm]= 0.280916; } 
	  if(m==14 && mm == 34) {     tw1n[m][mm]= 0.072978; } 
	  if(m==14 && mm == 35) {     tw1n[m][mm]= 0.220118; } 
	  if(m==14 && mm == 4) {     tw2n[m][mm]= 0.679303; } 
	  if(m==14 && mm == 5) {     tw2n[m][mm]= 0.342817; } 
	  if(m==14 && mm == 6) {     tw2n[m][mm]= 0.605061; } 
	  if(m==14 && mm == 7) {     tw2n[m][mm]= 0.546299; } 
	  if(m==14 && mm == 19) {     tw2n[m][mm]= 0.257809; } 
	  if(m==14 && mm == 26) {     tw2n[m][mm]= 0.666790; } 
	  if(m==14 && mm == 27) {     tw2n[m][mm]= 0.246699; } 
	  if(m==14 && mm == 32) {     tw2n[m][mm]= 0.561384; } 
	  if(m==14 && mm == 34) {     tw2n[m][mm]= 0.111715; } 
	  if(m==14 && mm == 35) {     tw2n[m][mm]= 0.040466; } 
	  if(m==14 && mm == 6) {     tw3n[m][mm]= 0.217911; } 
	  if(m==14 && mm == 7) {     tw3n[m][mm]= 0.019605; } 
	  if(m==14 && mm == 8) {     tw3n[m][mm]= 0.287643; } 
	  if(m==14 && mm == 9) {     tw3n[m][mm]= 0.901966; } 
	  if(m==14 && mm == 10) {     tw3n[m][mm]= 0.510372; } 
	  if(m==14 && mm == 11) {     tw3n[m][mm]= 1.088296; } 
	  if(m==14 && mm == 20) {     tw3n[m][mm]= 0.015815; } 
	  if(m==14 && mm == 26) {     tw3n[m][mm]= 0.435099; } 
	  if(m==14 && mm == 27) {     tw3n[m][mm]= 1.066617; } 
	  if(m==14 && mm == 6) {     te2n[m][mm]= 0.277719; } 
	  if(m==14 && mm == 10) {     te2n[m][mm]= 0.055476; } 
	  if(m==14 && mm == 12) {     te2n[m][mm]= 0.415143; } 
	  if(m==14 && mm == 14) {     te2n[m][mm]= 2.880337; } 
	  if(m==14 && mm == 16) {     te2n[m][mm]= 0.482700; } 
	  if(m==14 && mm == 17) {     te2n[m][mm]= 0.536393; } 
	  if(m==14 && mm == 18) {     te2n[m][mm]= 0.231496; } 
	  if(m==14 && mm == 19) {     te2n[m][mm]= 0.161279; } 
	  if(m==14 && mm == 20) {     te2n[m][mm]= 0.086303; } 
	  if(m==14 && mm == 21) {     te2n[m][mm]= 0.326413; } 
	  if(m==14 && mm == 22) {     te2n[m][mm]= 0.867534; } 
	  if(m==14 && mm == 23) {     te2n[m][mm]= 0.444809; } 
	  if(m==14 && mm == 25) {     te2n[m][mm]= 0.427923; } 
	  if(m==14 && mm == 26) {     te2n[m][mm]= 0.372257; } 
	  if(m==14 && mm == 29) {     te2n[m][mm]= 0.841143; } 
	  if(m==14 && mm == 30) {     te2n[m][mm]= 0.185124; } 
	  if(m==14 && mm == 35) {     te2n[m][mm]= 0.067296; } 
	  if(m==14 && mm == 0) {     te3n[m][mm]= 1.421230; } 
	  if(m==14 && mm == 1) {     te3n[m][mm]= 0.127856; } 
	  if(m==14 && mm == 2) {     te3n[m][mm]= 0.625697; } 
	  if(m==14 && mm == 4) {     te3n[m][mm]= 0.180630; } 
	  if(m==14 && mm == 16) {     te3n[m][mm]= 0.470289; } 
	  if(m==14 && mm == 17) {     te3n[m][mm]= 0.192746; } 
	  if(m==14 && mm == 18) {     te3n[m][mm]= 0.534503; } 
	  if(m==14 && mm == 19) {     te3n[m][mm]= 2.607658; } 
	  if(m==14 && mm == 20) {     te3n[m][mm]= 0.090386; } 
	  if(m==14 && mm == 21) {     te3n[m][mm]= 1.191235; } 
	  if(m==14 && mm == 22) {     te3n[m][mm]= 0.479472; } 
	  if(m==14 && mm == 23) {     te3n[m][mm]= 1.166720; } 
	  if(m==14 && mm == 24) {     te3n[m][mm]= 0.420303; } 
	  if(m==14 && mm == 25) {     te3n[m][mm]= 0.478837; } 
	  if(m==14 && mm == 26) {     te3n[m][mm]= 0.639687; } 
	  if(m==14 && mm == 28) {     te3n[m][mm]= 0.152248; } 
	  if(m==14 && mm == 29) {     te3n[m][mm]= 0.326898; } 
	  if(m==14 && mm == 30) {     te3n[m][mm]= 0.417067; } 
	  if(m==14 && mm == 31) {     te3n[m][mm]= 0.093619; } 
	  if(m==14 && mm == 33) {     te3n[m][mm]= 0.326823; } 
	  if(m==14 && mm == 34) {     te3n[m][mm]= 0.695318; } 
	  if(m==14 && mm == 35) {     te3n[m][mm]= 0.110048; } 
	  if(m==15 && mm == 9) {     tw0p[m][mm]= 0.767721; } 
	  if(m==15 && mm == 11) {     tw0p[m][mm]= 0.545946; } 
	  if(m==15 && mm == 12) {     tw0p[m][mm]= 0.075188; } 
	  if(m==15 && mm == 13) {     tw0p[m][mm]= 0.351821; } 
	  if(m==15 && mm == 14) {     tw0p[m][mm]= 0.259388; } 
	  if(m==15 && mm == 15) {     tw0p[m][mm]= 0.200894; } 
	  if(m==15 && mm == 17) {     tw0p[m][mm]= 0.201103; } 
	  if(m==15 && mm == 18) {     tw0p[m][mm]= 0.746568; } 
	  if(m==15 && mm == 19) {     tw0p[m][mm]= 0.024643; } 
	  if(m==15 && mm == 21) {     tw0p[m][mm]= 0.362738; } 
	  if(m==15 && mm == 22) {     tw0p[m][mm]= 0.626754; } 
	  if(m==15 && mm == 23) {     tw0p[m][mm]= 0.406688; } 
	  if(m==15 && mm == 24) {     tw0p[m][mm]= 0.382649; } 
	  if(m==15 && mm == 25) {     tw0p[m][mm]= 0.195114; } 
	  if(m==15 && mm == 26) {     tw0p[m][mm]= 0.158563; } 
	  if(m==15 && mm == 27) {     tw0p[m][mm]= 0.138074; } 
	  if(m==15 && mm == 28) {     tw0p[m][mm]= 0.026840; } 
	  if(m==15 && mm == 31) {     tw0p[m][mm]= 0.560981; } 
	  if(m==15 && mm == 32) {     tw0p[m][mm]= 0.516013; } 
	  if(m==15 && mm == 33) {     tw0p[m][mm]= 0.844724; } 
	  if(m==15 && mm == 34) {     tw0p[m][mm]= 0.782444; } 
	  if(m==15 && mm == 35) {     tw0p[m][mm]= 0.740633; } 
	  if(m==15 && mm == 8) {     tw1p[m][mm]= 0.138111; } 
	  if(m==15 && mm == 20) {     tw1p[m][mm]= 0.443180; } 
	  if(m==15 && mm == 21) {     tw1p[m][mm]= 0.041028; } 
	  if(m==15 && mm == 23) {     tw1p[m][mm]= 1.011056; } 
	  if(m==15 && mm == 24) {     tw1p[m][mm]= 0.808419; } 
	  if(m==15 && mm == 25) {     tw1p[m][mm]= 0.607862; } 
	  if(m==15 && mm == 26) {     tw1p[m][mm]= 0.390504; } 
	  if(m==15 && mm == 27) {     tw1p[m][mm]= 1.035106; } 
	  if(m==15 && mm == 28) {     tw1p[m][mm]= 0.384926; } 
	  if(m==15 && mm == 29) {     tw1p[m][mm]= 0.493516; } 
	  if(m==15 && mm == 31) {     tw1p[m][mm]= 0.474855; } 
	  if(m==15 && mm == 32) {     tw1p[m][mm]= 0.607509; } 
	  if(m==15 && mm == 33) {     tw1p[m][mm]= 1.180109; } 
	  if(m==15 && mm == 34) {     tw1p[m][mm]= 0.883111; } 
	  if(m==15 && mm == 35) {     tw1p[m][mm]= 0.536689; } 
	  if(m==15 && mm == 6) {     tw2p[m][mm]= 0.639451; } 
	  if(m==15 && mm == 7) {     tw2p[m][mm]= 0.496854; } 
	  if(m==15 && mm == 8) {     tw2p[m][mm]= 0.269680; } 
	  if(m==15 && mm == 9) {     tw2p[m][mm]= 0.173769; } 
	  if(m==15 && mm == 10) {     tw2p[m][mm]= 0.175198; } 
	  if(m==15 && mm == 11) {     tw2p[m][mm]= 0.168584; } 
	  if(m==15 && mm == 18) {     tw2p[m][mm]= 0.469395; } 
	  if(m==15 && mm == 21) {     tw2p[m][mm]= 0.319305; } 
	  if(m==15 && mm == 22) {     tw2p[m][mm]= 0.189920; } 
	  if(m==15 && mm == 28) {     tw2p[m][mm]= 0.844838; } 
	  if(m==15 && mm == 29) {     tw2p[m][mm]= 0.503407; } 
	  if(m==15 && mm == 30) {     tw2p[m][mm]= 0.384707; } 
	  if(m==15 && mm == 31) {     tw2p[m][mm]= 0.210394; } 
	  if(m==15 && mm == 32) {     tw2p[m][mm]= 0.509890; } 
	  if(m==15 && mm == 33) {     tw2p[m][mm]= 0.336786; } 
	  if(m==15 && mm == 34) {     tw2p[m][mm]= 0.733211; } 
	  if(m==15 && mm == 35) {     tw2p[m][mm]= 0.663840; } 
	  if(m==15 && mm == 6) {     tw3p[m][mm]= 1.042019; } 
	  if(m==15 && mm == 7) {     tw3p[m][mm]= 0.392124; } 
	  if(m==15 && mm == 8) {     tw3p[m][mm]= 0.516500; } 
	  if(m==15 && mm == 9) {     tw3p[m][mm]= 0.825764; } 
	  if(m==15 && mm == 10) {     tw3p[m][mm]= 0.900891; } 
	  if(m==15 && mm == 11) {     tw3p[m][mm]= 0.953168; } 
	  if(m==15 && mm == 18) {     tw3p[m][mm]= 0.104656; } 
	  if(m==15 && mm == 22) {     tw3p[m][mm]= 0.021856; } 
	  if(m==15 && mm == 23) {     tw3p[m][mm]= 0.114689; } 
	  if(m==15 && mm == 26) {     tw3p[m][mm]= 0.752122; } 
	  if(m==15 && mm == 27) {     tw3p[m][mm]= 0.173595; } 
	  if(m==15 && mm == 30) {     tw3p[m][mm]= 0.157630; } 
	  if(m==15 && mm == 31) {     tw3p[m][mm]= 0.342072; } 
	  if(m==15 && mm == 34) {     tw3p[m][mm]= 0.507967; } 
	  if(m==15 && mm == 1) {     te2p[m][mm]= 0.008473; } 
	  if(m==15 && mm == 2) {     te2p[m][mm]= 0.991151; } 
	  if(m==15 && mm == 3) {     te2p[m][mm]= 0.076252; } 
	  if(m==15 && mm == 5) {     te2p[m][mm]= 0.043389; } 
	  if(m==15 && mm == 6) {     te2p[m][mm]= 0.016995; } 
	  if(m==15 && mm == 7) {     te2p[m][mm]= 0.102981; } 
	  if(m==15 && mm == 8) {     te2p[m][mm]= 1.934645; } 
	  if(m==15 && mm == 9) {     te2p[m][mm]= 0.642761; } 
	  if(m==15 && mm == 10) {     te2p[m][mm]= 0.455621; } 
	  if(m==15 && mm == 11) {     te2p[m][mm]= 0.111191; } 
	  if(m==15 && mm == 12) {     te2p[m][mm]= 0.280562; } 
	  if(m==15 && mm == 13) {     te2p[m][mm]= 0.293609; } 
	  if(m==15 && mm == 14) {     te2p[m][mm]= 0.297136; } 
	  if(m==15 && mm == 15) {     te2p[m][mm]= 0.463283; } 
	  if(m==15 && mm == 16) {     te2p[m][mm]= 0.517181; } 
	  if(m==15 && mm == 17) {     te2p[m][mm]= 0.033737; } 
	  if(m==15 && mm == 18) {     te2p[m][mm]= 0.714276; } 
	  if(m==15 && mm == 19) {     te2p[m][mm]= 0.913696; } 
	  if(m==15 && mm == 20) {     te2p[m][mm]= 0.223886; } 
	  if(m==15 && mm == 21) {     te2p[m][mm]= 0.430389; } 
	  if(m==15 && mm == 22) {     te2p[m][mm]= 0.678479; } 
	  if(m==15 && mm == 23) {     te2p[m][mm]= 0.776600; } 
	  if(m==15 && mm == 25) {     te2p[m][mm]= 0.362547; } 
	  if(m==15 && mm == 26) {     te2p[m][mm]= 0.279131; } 
	  if(m==15 && mm == 27) {     te2p[m][mm]= 0.022513; } 
	  if(m==15 && mm == 28) {     te2p[m][mm]= 0.661965; } 
	  if(m==15 && mm == 30) {     te2p[m][mm]= 0.278838; } 
	  if(m==15 && mm == 31) {     te2p[m][mm]= 0.159298; } 
	  if(m==15 && mm == 5) {     te3p[m][mm]= 0.410319; } 
	  if(m==15 && mm == 6) {     te3p[m][mm]= 0.493005; } 
	  if(m==15 && mm == 7) {     te3p[m][mm]= 1.002972; } 
	  if(m==15 && mm == 8) {     te3p[m][mm]= 0.533271; } 
	  if(m==15 && mm == 10) {     te3p[m][mm]= 0.137085; } 
	  if(m==15 && mm == 11) {     te3p[m][mm]= 0.377723; } 
	  if(m==15 && mm == 12) {     te3p[m][mm]= 0.169574; } 
	  if(m==15 && mm == 13) {     te3p[m][mm]= 0.005306; } 
	  if(m==15 && mm == 14) {     te3p[m][mm]= 0.276071; } 
	  if(m==15 && mm == 16) {     te3p[m][mm]= 0.340934; } 
	  if(m==15 && mm == 17) {     te3p[m][mm]= 0.507877; } 
	  if(m==15 && mm == 18) {     te3p[m][mm]= 0.632655; } 
	  if(m==15 && mm == 19) {     te3p[m][mm]= 0.373023; } 
	  if(m==15 && mm == 20) {     te3p[m][mm]= 0.297306; } 
	  if(m==15 && mm == 21) {     te3p[m][mm]= 1.328027; } 
	  if(m==15 && mm == 22) {     te3p[m][mm]= 0.850157; } 
	  if(m==15 && mm == 23) {     te3p[m][mm]= 0.892828; } 
	  if(m==15 && mm == 0) {     tw0n[m][mm]= 0.331997; } 
	  if(m==15 && mm == 2) {     tw0n[m][mm]= 1.205865; } 
	  if(m==15 && mm == 4) {     tw0n[m][mm]= 0.003700; } 
	  if(m==15 && mm == 5) {     tw0n[m][mm]= 0.661246; } 
	  if(m==15 && mm == 6) {     tw0n[m][mm]= 0.693720; } 
	  if(m==15 && mm == 7) {     tw0n[m][mm]= 0.142591; } 
	  if(m==15 && mm == 11) {     tw0n[m][mm]= 0.659564; } 
	  if(m==15 && mm == 13) {     tw0n[m][mm]= 0.289581; } 
	  if(m==15 && mm == 14) {     tw0n[m][mm]= 0.518500; } 
	  if(m==15 && mm == 15) {     tw0n[m][mm]= 0.182877; } 
	  if(m==15 && mm == 17) {     tw0n[m][mm]= 0.388035; } 
	  if(m==15 && mm == 18) {     tw0n[m][mm]= 0.609672; } 
	  if(m==15 && mm == 21) {     tw0n[m][mm]= 0.023677; } 
	  if(m==15 && mm == 23) {     tw0n[m][mm]= 0.174901; } 
	  if(m==15 && mm == 24) {     tw0n[m][mm]= 0.178544; } 
	  if(m==15 && mm == 25) {     tw0n[m][mm]= 0.138299; } 
	  if(m==15 && mm == 26) {     tw0n[m][mm]= 0.041590; } 
	  if(m==15 && mm == 30) {     tw0n[m][mm]= 1.580261; } 
	  if(m==15 && mm == 7) {     tw1n[m][mm]= 0.109449; } 
	  if(m==15 && mm == 8) {     tw1n[m][mm]= 0.165758; } 
	  if(m==15 && mm == 11) {     tw1n[m][mm]= 0.066299; } 
	  if(m==15 && mm == 12) {     tw1n[m][mm]= 0.098735; } 
	  if(m==15 && mm == 13) {     tw1n[m][mm]= 0.316474; } 
	  if(m==15 && mm == 14) {     tw1n[m][mm]= 0.418927; } 
	  if(m==15 && mm == 15) {     tw1n[m][mm]= 0.109439; } 
	  if(m==15 && mm == 16) {     tw1n[m][mm]= 0.352275; } 
	  if(m==15 && mm == 18) {     tw1n[m][mm]= 0.522297; } 
	  if(m==15 && mm == 19) {     tw1n[m][mm]= 0.852007; } 
	  if(m==15 && mm == 20) {     tw1n[m][mm]= 0.387188; } 
	  if(m==15 && mm == 21) {     tw1n[m][mm]= 0.325508; } 
	  if(m==15 && mm == 22) {     tw1n[m][mm]= 0.647328; } 
	  if(m==15 && mm == 23) {     tw1n[m][mm]= 0.478899; } 
	  if(m==15 && mm == 24) {     tw1n[m][mm]= 0.506097; } 
	  if(m==15 && mm == 31) {     tw1n[m][mm]= 0.285996; } 
	  if(m==15 && mm == 32) {     tw1n[m][mm]= 0.389889; } 
	  if(m==15 && mm == 33) {     tw1n[m][mm]= 0.715136; } 
	  if(m==15 && mm == 34) {     tw1n[m][mm]= 0.483058; } 
	  if(m==15 && mm == 35) {     tw1n[m][mm]= 0.286311; } 
	  if(m==15 && mm == 1) {     tw2n[m][mm]= 0.055257; } 
	  if(m==15 && mm == 2) {     tw2n[m][mm]= 0.247003; } 
	  if(m==15 && mm == 4) {     tw2n[m][mm]= 0.201490; } 
	  if(m==15 && mm == 5) {     tw2n[m][mm]= 2.145854; } 
	  if(m==15 && mm == 6) {     tw2n[m][mm]= 0.143113; } 
	  if(m==15 && mm == 7) {     tw2n[m][mm]= 0.378953; } 
	  if(m==15 && mm == 18) {     tw2n[m][mm]= 0.332680; } 
	  if(m==15 && mm == 21) {     tw2n[m][mm]= 0.038663; } 
	  if(m==15 && mm == 22) {     tw2n[m][mm]= 0.142668; } 
	  if(m==15 && mm == 26) {     tw2n[m][mm]= 0.192146; } 
	  if(m==15 && mm == 27) {     tw2n[m][mm]= 0.212110; } 
	  if(m==15 && mm == 28) {     tw2n[m][mm]= 0.424445; } 
	  if(m==15 && mm == 29) {     tw2n[m][mm]= 0.366301; } 
	  if(m==15 && mm == 30) {     tw2n[m][mm]= 0.013637; } 
	  if(m==15 && mm == 32) {     tw2n[m][mm]= 0.340402; } 
	  if(m==15 && mm == 33) {     tw2n[m][mm]= 0.085400; } 
	  if(m==15 && mm == 34) {     tw2n[m][mm]= 0.387439; } 
	  if(m==15 && mm == 35) {     tw2n[m][mm]= 0.460040; } 
	  if(m==15 && mm == 6) {     tw3n[m][mm]= 1.241331; } 
	  if(m==15 && mm == 7) {     tw3n[m][mm]= 0.459925; } 
	  if(m==15 && mm == 8) {     tw3n[m][mm]= 0.491503; } 
	  if(m==15 && mm == 9) {     tw3n[m][mm]= 0.937468; } 
	  if(m==15 && mm == 10) {     tw3n[m][mm]= 1.116768; } 
	  if(m==15 && mm == 11) {     tw3n[m][mm]= 1.002342; } 
	  if(m==15 && mm == 18) {     tw3n[m][mm]= 0.024504; } 
	  if(m==15 && mm == 23) {     tw3n[m][mm]= 0.046384; } 
	  if(m==15 && mm == 8) {     te2n[m][mm]= 0.782223; } 
	  if(m==15 && mm == 9) {     te2n[m][mm]= 0.496936; } 
	  if(m==15 && mm == 10) {     te2n[m][mm]= 0.098177; } 
	  if(m==15 && mm == 14) {     te2n[m][mm]= 0.063911; } 
	  if(m==15 && mm == 15) {     te2n[m][mm]= 0.486076; } 
	  if(m==15 && mm == 16) {     te2n[m][mm]= 0.362241; } 
	  if(m==15 && mm == 17) {     te2n[m][mm]= 0.259277; } 
	  if(m==15 && mm == 18) {     te2n[m][mm]= 0.214111; } 
	  if(m==15 && mm == 20) {     te2n[m][mm]= 0.023918; } 
	  if(m==15 && mm == 22) {     te2n[m][mm]= 1.010797; } 
	  if(m==15 && mm == 23) {     te2n[m][mm]= 0.270271; } 
	  if(m==15 && mm == 24) {     te2n[m][mm]= 0.123857; } 
	  if(m==15 && mm == 25) {     te2n[m][mm]= 0.371379; } 
	  if(m==15 && mm == 26) {     te2n[m][mm]= 0.141385; } 
	  if(m==15 && mm == 27) {     te2n[m][mm]= 0.194642; } 
	  if(m==15 && mm == 28) {     te2n[m][mm]= 0.326986; } 
	  if(m==15 && mm == 29) {     te2n[m][mm]= 0.121999; } 
	  if(m==15 && mm == 30) {     te2n[m][mm]= 0.623507; } 
	  if(m==15 && mm == 31) {     te2n[m][mm]= 0.272813; } 
	  if(m==15 && mm == 32) {     te2n[m][mm]= 0.174903; } 
	  if(m==15 && mm == 33) {     te2n[m][mm]= 0.266002; } 
	  if(m==15 && mm == 34) {     te2n[m][mm]= 0.678500; } 
	  if(m==15 && mm == 35) {     te2n[m][mm]= 0.319848; } 
	  if(m==15 && mm == 0) {     te3n[m][mm]= 0.005773; } 
	  if(m==15 && mm == 1) {     te3n[m][mm]= 0.298576; } 
	  if(m==15 && mm == 2) {     te3n[m][mm]= 0.205881; } 
	  if(m==15 && mm == 4) {     te3n[m][mm]= 2.404660; } 
	  if(m==15 && mm == 6) {     te3n[m][mm]= 0.368170; } 
	  if(m==15 && mm == 16) {     te3n[m][mm]= 0.966310; } 
	  if(m==15 && mm == 17) {     te3n[m][mm]= 0.584051; } 
	  if(m==15 && mm == 19) {     te3n[m][mm]= 0.736857; } 
	  if(m==15 && mm == 20) {     te3n[m][mm]= 0.019105; } 
	  if(m==15 && mm == 21) {     te3n[m][mm]= 1.060782; } 
	  if(m==15 && mm == 22) {     te3n[m][mm]= 0.868653; } 
	  if(m==15 && mm == 23) {     te3n[m][mm]= 0.629032; } 
	  if(m==15 && mm == 27) {     te3n[m][mm]= 0.477666; } 
	  if(m==15 && mm == 29) {     te3n[m][mm]= 0.083083; } 
	  if(m==15 && mm == 31) {     te3n[m][mm]= 0.198289; } 
	  if(m==15 && mm == 32) {     te3n[m][mm]= 0.094272; } 
	  if(m==15 && mm == 33) {     te3n[m][mm]= 0.542663; } 
	  if(m==16 && mm == 8) {     tw0p[m][mm]= 0.163061; } 
	  if(m==16 && mm == 9) {     tw0p[m][mm]= 0.058149; } 
	  if(m==16 && mm == 12) {     tw0p[m][mm]= 0.158957; } 
	  if(m==16 && mm == 13) {     tw0p[m][mm]= 0.010340; } 
	  if(m==16 && mm == 17) {     tw0p[m][mm]= 0.126547; } 
	  if(m==16 && mm == 18) {     tw0p[m][mm]= 0.358564; } 
	  if(m==16 && mm == 19) {     tw0p[m][mm]= 0.395544; } 
	  if(m==16 && mm == 21) {     tw0p[m][mm]= 0.188294; } 
	  if(m==16 && mm == 22) {     tw0p[m][mm]= 0.160583; } 
	  if(m==16 && mm == 30) {     tw0p[m][mm]= 0.544787; } 
	  if(m==16 && mm == 31) {     tw0p[m][mm]= 0.377438; } 
	  if(m==16 && mm == 32) {     tw0p[m][mm]= 0.399488; } 
	  if(m==16 && mm == 33) {     tw0p[m][mm]= 0.633007; } 
	  if(m==16 && mm == 34) {     tw0p[m][mm]= 0.230140; } 
	  if(m==16 && mm == 35) {     tw0p[m][mm]= 0.011075; } 
	  if(m==16 && mm == 20) {     tw1p[m][mm]= 0.317998; } 
	  if(m==16 && mm == 21) {     tw1p[m][mm]= 0.208911; } 
	  if(m==16 && mm == 23) {     tw1p[m][mm]= 0.057524; } 
	  if(m==16 && mm == 24) {     tw1p[m][mm]= 0.765723; } 
	  if(m==16 && mm == 25) {     tw1p[m][mm]= 0.437834; } 
	  if(m==16 && mm == 26) {     tw1p[m][mm]= 0.240972; } 
	  if(m==16 && mm == 27) {     tw1p[m][mm]= 0.883967; } 
	  if(m==16 && mm == 28) {     tw1p[m][mm]= 0.106378; } 
	  if(m==16 && mm == 29) {     tw1p[m][mm]= 0.518284; } 
	  if(m==16 && mm == 31) {     tw1p[m][mm]= 0.473466; } 
	  if(m==16 && mm == 33) {     tw1p[m][mm]= 0.382931; } 
	  if(m==16 && mm == 34) {     tw1p[m][mm]= 0.624178; } 
	  if(m==16 && mm == 35) {     tw1p[m][mm]= 0.464241; } 
	  if(m==16 && mm == 5) {     tw2p[m][mm]= 0.070803; } 
	  if(m==16 && mm == 6) {     tw2p[m][mm]= 0.179580; } 
	  if(m==16 && mm == 7) {     tw2p[m][mm]= 0.414055; } 
	  if(m==16 && mm == 8) {     tw2p[m][mm]= 0.031170; } 
	  if(m==16 && mm == 9) {     tw2p[m][mm]= 0.134310; } 
	  if(m==16 && mm == 10) {     tw2p[m][mm]= 0.147589; } 
	  if(m==16 && mm == 11) {     tw2p[m][mm]= 0.476489; } 
	  if(m==16 && mm == 16) {     tw2p[m][mm]= 0.091855; } 
	  if(m==16 && mm == 17) {     tw2p[m][mm]= 0.150136; } 
	  if(m==16 && mm == 19) {     tw2p[m][mm]= 0.009137; } 
	  if(m==16 && mm == 20) {     tw2p[m][mm]= 0.362843; } 
	  if(m==16 && mm == 23) {     tw2p[m][mm]= 0.005725; } 
	  if(m==16 && mm == 28) {     tw2p[m][mm]= 0.520832; } 
	  if(m==16 && mm == 29) {     tw2p[m][mm]= 0.268242; } 
	  if(m==16 && mm == 30) {     tw2p[m][mm]= 0.349043; } 
	  if(m==16 && mm == 32) {     tw2p[m][mm]= 0.492506; } 
	  if(m==16 && mm == 33) {     tw2p[m][mm]= 0.033607; } 
	  if(m==16 && mm == 34) {     tw2p[m][mm]= 0.356391; } 
	  if(m==16 && mm == 0) {     tw3p[m][mm]= 1.159004; } 
	  if(m==16 && mm == 1) {     tw3p[m][mm]= 0.601262; } 
	  if(m==16 && mm == 2) {     tw3p[m][mm]= 0.096266; } 
	  if(m==16 && mm == 3) {     tw3p[m][mm]= 0.251017; } 
	  if(m==16 && mm == 5) {     tw3p[m][mm]= 0.150583; } 
	  if(m==16 && mm == 6) {     tw3p[m][mm]= 0.079479; } 
	  if(m==16 && mm == 7) {     tw3p[m][mm]= 0.190467; } 
	  if(m==16 && mm == 8) {     tw3p[m][mm]= 0.296813; } 
	  if(m==16 && mm == 9) {     tw3p[m][mm]= 0.474489; } 
	  if(m==16 && mm == 11) {     tw3p[m][mm]= 0.347606; } 
	  if(m==16 && mm == 19) {     tw3p[m][mm]= 0.144268; } 
	  if(m==16 && mm == 23) {     tw3p[m][mm]= 0.269983; } 
	  if(m==16 && mm == 31) {     tw3p[m][mm]= 0.665827; } 
	  if(m==16 && mm == 33) {     tw3p[m][mm]= 0.817523; } 
	  if(m==16 && mm == 3) {     te2p[m][mm]= 0.529331; } 
	  if(m==16 && mm == 4) {     te2p[m][mm]= 0.116788; } 
	  if(m==16 && mm == 8) {     te2p[m][mm]= 0.142768; } 
	  if(m==16 && mm == 10) {     te2p[m][mm]= 0.330679; } 
	  if(m==16 && mm == 11) {     te2p[m][mm]= 0.111351; } 
	  if(m==16 && mm == 12) {     te2p[m][mm]= 0.434374; } 
	  if(m==16 && mm == 13) {     te2p[m][mm]= 0.727358; } 
	  if(m==16 && mm == 14) {     te2p[m][mm]= 1.167896; } 
	  if(m==16 && mm == 15) {     te2p[m][mm]= 0.986747; } 
	  if(m==16 && mm == 16) {     te2p[m][mm]= 0.918590; } 
	  if(m==16 && mm == 17) {     te2p[m][mm]= 0.066729; } 
	  if(m==16 && mm == 18) {     te2p[m][mm]= 0.743071; } 
	  if(m==16 && mm == 19) {     te2p[m][mm]= 0.828167; } 
	  if(m==16 && mm == 30) {     te2p[m][mm]= 0.489386; } 
	  if(m==16 && mm == 5) {     te3p[m][mm]= 0.341224; } 
	  if(m==16 && mm == 7) {     te3p[m][mm]= 0.062693; } 
	  if(m==16 && mm == 8) {     te3p[m][mm]= 0.139841; } 
	  if(m==16 && mm == 9) {     te3p[m][mm]= 0.268441; } 
	  if(m==16 && mm == 11) {     te3p[m][mm]= 0.046689; } 
	  if(m==16 && mm == 12) {     te3p[m][mm]= 0.055547; } 
	  if(m==16 && mm == 15) {     te3p[m][mm]= 0.223166; } 
	  if(m==16 && mm == 16) {     te3p[m][mm]= 0.111404; } 
	  if(m==16 && mm == 17) {     te3p[m][mm]= 0.045922; } 
	  if(m==16 && mm == 18) {     te3p[m][mm]= 0.324606; } 
	  if(m==16 && mm == 19) {     te3p[m][mm]= 0.115941; } 
	  if(m==16 && mm == 20) {     te3p[m][mm]= 0.009483; } 
	  if(m==16 && mm == 21) {     te3p[m][mm]= 0.162689; } 
	  if(m==16 && mm == 22) {     te3p[m][mm]= 0.265654; } 
	  if(m==16 && mm == 0) {     tw0n[m][mm]= 0.078898; } 
	  if(m==16 && mm == 2) {     tw0n[m][mm]= 0.430095; } 
	  if(m==16 && mm == 7) {     tw0n[m][mm]= 0.207258; } 
	  if(m==16 && mm == 9) {     tw0n[m][mm]= 0.145777; } 
	  if(m==16 && mm == 12) {     tw0n[m][mm]= 0.311805; } 
	  if(m==16 && mm == 13) {     tw0n[m][mm]= 0.166461; } 
	  if(m==16 && mm == 15) {     tw0n[m][mm]= 0.026051; } 
	  if(m==16 && mm == 17) {     tw0n[m][mm]= 0.300214; } 
	  if(m==16 && mm == 18) {     tw0n[m][mm]= 0.479403; } 
	  if(m==16 && mm == 19) {     tw0n[m][mm]= 0.349300; } 
	  if(m==16 && mm == 21) {     tw0n[m][mm]= 0.124710; } 
	  if(m==16 && mm == 30) {     tw0n[m][mm]= 0.325628; } 
	  if(m==16 && mm == 7) {     tw1n[m][mm]= 1.469976; } 
	  if(m==16 && mm == 10) {     tw1n[m][mm]= 0.244482; } 
	  if(m==16 && mm == 12) {     tw1n[m][mm]= 0.069453; } 
	  if(m==16 && mm == 14) {     tw1n[m][mm]= 0.305533; } 
	  if(m==16 && mm == 16) {     tw1n[m][mm]= 0.160090; } 
	  if(m==16 && mm == 17) {     tw1n[m][mm]= 0.042257; } 
	  if(m==16 && mm == 18) {     tw1n[m][mm]= 0.047236; } 
	  if(m==16 && mm == 19) {     tw1n[m][mm]= 0.044717; } 
	  if(m==16 && mm == 20) {     tw1n[m][mm]= 0.291040; } 
	  if(m==16 && mm == 21) {     tw1n[m][mm]= 0.003100; } 
	  if(m==16 && mm == 22) {     tw1n[m][mm]= 0.032229; } 
	  if(m==16 && mm == 24) {     tw1n[m][mm]= 0.350808; } 
	  if(m==16 && mm == 30) {     tw1n[m][mm]= 0.469569; } 
	  if(m==16 && mm == 31) {     tw1n[m][mm]= 0.080757; } 
	  if(m==16 && mm == 33) {     tw1n[m][mm]= 0.019706; } 
	  if(m==16 && mm == 34) {     tw1n[m][mm]= 0.162009; } 
	  if(m==16 && mm == 35) {     tw1n[m][mm]= 0.283071; } 
	  if(m==16 && mm == 2) {     tw2n[m][mm]= 0.060079; } 
	  if(m==16 && mm == 5) {     tw2n[m][mm]= 0.495512; } 
	  if(m==16 && mm == 6) {     tw2n[m][mm]= 0.379017; } 
	  if(m==16 && mm == 7) {     tw2n[m][mm]= 1.002040; } 
	  if(m==16 && mm == 16) {     tw2n[m][mm]= 0.086695; } 
	  if(m==16 && mm == 17) {     tw2n[m][mm]= 0.174215; } 
	  if(m==16 && mm == 19) {     tw2n[m][mm]= 0.099186; } 
	  if(m==16 && mm == 20) {     tw2n[m][mm]= 0.248824; } 
	  if(m==16 && mm == 28) {     tw2n[m][mm]= 0.075598; } 
	  if(m==16 && mm == 29) {     tw2n[m][mm]= 0.195898; } 
	  if(m==16 && mm == 32) {     tw2n[m][mm]= 0.192912; } 
	  if(m==16 && mm == 34) {     tw2n[m][mm]= 0.057936; } 
	  if(m==16 && mm == 0) {     tw3n[m][mm]= 1.297098; } 
	  if(m==16 && mm == 1) {     tw3n[m][mm]= 0.767726; } 
	  if(m==16 && mm == 2) {     tw3n[m][mm]= 0.097590; } 
	  if(m==16 && mm == 3) {     tw3n[m][mm]= 0.148908; } 
	  if(m==16 && mm == 4) {     tw3n[m][mm]= 1.155756; } 
	  if(m==16 && mm == 5) {     tw3n[m][mm]= 0.333978; } 
	  if(m==16 && mm == 6) {     tw3n[m][mm]= 0.115425; } 
	  if(m==16 && mm == 7) {     tw3n[m][mm]= 0.282171; } 
	  if(m==16 && mm == 8) {     tw3n[m][mm]= 0.421465; } 
	  if(m==16 && mm == 9) {     tw3n[m][mm]= 0.534623; } 
	  if(m==16 && mm == 11) {     tw3n[m][mm]= 0.225176; } 
	  if(m==16 && mm == 19) {     tw3n[m][mm]= 0.118540; } 
	  if(m==16 && mm == 23) {     tw3n[m][mm]= 0.087298; } 
	  if(m==16 && mm == 10) {     te2n[m][mm]= 0.045442; } 
	  if(m==16 && mm == 12) {     te2n[m][mm]= 0.539184; } 
	  if(m==16 && mm == 13) {     te2n[m][mm]= 0.517002; } 
	  if(m==16 && mm == 14) {     te2n[m][mm]= 1.138063; } 
	  if(m==16 && mm == 15) {     te2n[m][mm]= 0.812908; } 
	  if(m==16 && mm == 16) {     te2n[m][mm]= 0.651080; } 
	  if(m==16 && mm == 18) {     te2n[m][mm]= 0.743730; } 
	  if(m==16 && mm == 19) {     te2n[m][mm]= 0.802438; } 
	  if(m==16 && mm == 24) {     te2n[m][mm]= 0.018055; } 
	  if(m==16 && mm == 30) {     te2n[m][mm]= 0.574375; } 
	  if(m==16 && mm == 32) {     te2n[m][mm]= 0.078408; } 
	  if(m==16 && mm == 35) {     te2n[m][mm]= 0.450597; } 
	  if(m==16 && mm == 0) {     te3n[m][mm]= 1.906376; } 
	  if(m==16 && mm == 1) {     te3n[m][mm]= 0.299835; } 
	  if(m==16 && mm == 2) {     te3n[m][mm]= 0.653675; } 
	  if(m==16 && mm == 16) {     te3n[m][mm]= 0.623714; } 
	  if(m==16 && mm == 17) {     te3n[m][mm]= 0.160672; } 
	  if(m==16 && mm == 18) {     te3n[m][mm]= 0.486249; } 
	  if(m==16 && mm == 20) {     te3n[m][mm]= 0.080132; } 
	  if(m==16 && mm == 21) {     te3n[m][mm]= 0.104999; } 
	  if(m==16 && mm == 22) {     te3n[m][mm]= 0.324189; } 
	  if(m==16 && mm == 24) {     te3n[m][mm]= 0.214937; } 
	  if(m==16 && mm == 25) {     te3n[m][mm]= 0.166070; } 
	  if(m==16 && mm == 26) {     te3n[m][mm]= 0.347365; } 
	  if(m==16 && mm == 30) {     te3n[m][mm]= 0.565647; } 
	  if(m==16 && mm == 31) {     te3n[m][mm]= 0.707763; } 
	  if(m==16 && mm == 32) {     te3n[m][mm]= 0.076849; } 
	  if(m==16 && mm == 33) {     te3n[m][mm]= 0.132169; } 
	  if(m==16 && mm == 34) {     te3n[m][mm]= 0.219523; } 
	  if(m==17 && mm == 9) {     tw0p[m][mm]= 0.236934; } 
	  if(m==17 && mm == 11) {     tw0p[m][mm]= 0.002785; } 
	  if(m==17 && mm == 12) {     tw0p[m][mm]= 0.130257; } 
	  if(m==17 && mm == 13) {     tw0p[m][mm]= 0.253711; } 
	  if(m==17 && mm == 14) {     tw0p[m][mm]= 0.195010; } 
	  if(m==17 && mm == 15) {     tw0p[m][mm]= 0.224259; } 
	  if(m==17 && mm == 16) {     tw0p[m][mm]= 0.148107; } 
	  if(m==17 && mm == 18) {     tw0p[m][mm]= 0.216512; } 
	  if(m==17 && mm == 19) {     tw0p[m][mm]= 0.121684; } 
	  if(m==17 && mm == 20) {     tw0p[m][mm]= 0.020866; } 
	  if(m==17 && mm == 21) {     tw0p[m][mm]= 0.325901; } 
	  if(m==17 && mm == 22) {     tw0p[m][mm]= 0.300458; } 
	  if(m==17 && mm == 23) {     tw0p[m][mm]= 0.051947; } 
	  if(m==17 && mm == 30) {     tw0p[m][mm]= 0.719615; } 
	  if(m==17 && mm == 31) {     tw0p[m][mm]= 0.214269; } 
	  if(m==17 && mm == 32) {     tw0p[m][mm]= 0.537218; } 
	  if(m==17 && mm == 33) {     tw0p[m][mm]= 0.182280; } 
	  if(m==17 && mm == 34) {     tw0p[m][mm]= 0.285905; } 
	  if(m==17 && mm == 35) {     tw0p[m][mm]= 0.517731; } 
	  if(m==17 && mm == 5) {     tw1p[m][mm]= 0.075517; } 
	  if(m==17 && mm == 8) {     tw1p[m][mm]= 0.000239; } 
	  if(m==17 && mm == 20) {     tw1p[m][mm]= 0.381387; } 
	  if(m==17 && mm == 21) {     tw1p[m][mm]= 0.596458; } 
	  if(m==17 && mm == 22) {     tw1p[m][mm]= 0.351875; } 
	  if(m==17 && mm == 23) {     tw1p[m][mm]= 0.397545; } 
	  if(m==17 && mm == 24) {     tw1p[m][mm]= 0.000585; } 
	  if(m==17 && mm == 25) {     tw1p[m][mm]= 0.523215; } 
	  if(m==17 && mm == 26) {     tw1p[m][mm]= 0.014469; } 
	  if(m==17 && mm == 27) {     tw1p[m][mm]= 1.707351; } 
	  if(m==17 && mm == 28) {     tw1p[m][mm]= 0.367339; } 
	  if(m==17 && mm == 29) {     tw1p[m][mm]= 0.441408; } 
	  if(m==17 && mm == 30) {     tw1p[m][mm]= 0.659007; } 
	  if(m==17 && mm == 31) {     tw1p[m][mm]= 1.279535; } 
	  if(m==17 && mm == 32) {     tw1p[m][mm]= 0.728094; } 
	  if(m==17 && mm == 33) {     tw1p[m][mm]= 1.058424; } 
	  if(m==17 && mm == 34) {     tw1p[m][mm]= 0.308821; } 
	  if(m==17 && mm == 5) {     tw2p[m][mm]= 0.187913; } 
	  if(m==17 && mm == 6) {     tw2p[m][mm]= 0.879646; } 
	  if(m==17 && mm == 8) {     tw2p[m][mm]= 0.572513; } 
	  if(m==17 && mm == 9) {     tw2p[m][mm]= 0.249400; } 
	  if(m==17 && mm == 10) {     tw2p[m][mm]= 0.445947; } 
	  if(m==17 && mm == 11) {     tw2p[m][mm]= 0.328955; } 
	  if(m==17 && mm == 20) {     tw2p[m][mm]= 0.295343; } 
	  if(m==17 && mm == 21) {     tw2p[m][mm]= 0.073211; } 
	  if(m==17 && mm == 23) {     tw2p[m][mm]= 0.444899; } 
	  if(m==17 && mm == 28) {     tw2p[m][mm]= 0.331643; } 
	  if(m==17 && mm == 29) {     tw2p[m][mm]= 0.338758; } 
	  if(m==17 && mm == 30) {     tw2p[m][mm]= 0.410006; } 
	  if(m==17 && mm == 31) {     tw2p[m][mm]= 0.219282; } 
	  if(m==17 && mm == 32) {     tw2p[m][mm]= 0.377292; } 
	  if(m==17 && mm == 33) {     tw2p[m][mm]= 0.132616; } 
	  if(m==17 && mm == 34) {     tw2p[m][mm]= 0.322518; } 
	  if(m==17 && mm == 35) {     tw2p[m][mm]= 0.444709; } 
	  if(m==17 && mm == 0) {     tw3p[m][mm]= 0.956049; } 
	  if(m==17 && mm == 1) {     tw3p[m][mm]= 0.345594; } 
	  if(m==17 && mm == 2) {     tw3p[m][mm]= 0.120125; } 
	  if(m==17 && mm == 4) {     tw3p[m][mm]= 0.499159; } 
	  if(m==17 && mm == 5) {     tw3p[m][mm]= 0.981033; } 
	  if(m==17 && mm == 6) {     tw3p[m][mm]= 0.461761; } 
	  if(m==17 && mm == 7) {     tw3p[m][mm]= 0.687700; } 
	  if(m==17 && mm == 8) {     tw3p[m][mm]= 0.736325; } 
	  if(m==17 && mm == 9) {     tw3p[m][mm]= 0.531850; } 
	  if(m==17 && mm == 10) {     tw3p[m][mm]= 0.336152; } 
	  if(m==17 && mm == 11) {     tw3p[m][mm]= 0.413028; } 
	  if(m==17 && mm == 18) {     tw3p[m][mm]= 0.387001; } 
	  if(m==17 && mm == 22) {     tw3p[m][mm]= 0.368417; } 
	  if(m==17 && mm == 23) {     tw3p[m][mm]= 0.284956; } 
	  if(m==17 && mm == 24) {     tw3p[m][mm]= 0.181364; } 
	  if(m==17 && mm == 29) {     tw3p[m][mm]= 0.085497; } 
	  if(m==17 && mm == 0) {     te2p[m][mm]= 0.113451; } 
	  if(m==17 && mm == 2) {     te2p[m][mm]= 0.177265; } 
	  if(m==17 && mm == 4) {     te2p[m][mm]= 0.187582; } 
	  if(m==17 && mm == 5) {     te2p[m][mm]= 0.148306; } 
	  if(m==17 && mm == 6) {     te2p[m][mm]= 0.260885; } 
	  if(m==17 && mm == 8) {     te2p[m][mm]= 0.104833; } 
	  if(m==17 && mm == 9) {     te2p[m][mm]= 0.264175; } 
	  if(m==17 && mm == 10) {     te2p[m][mm]= 0.243693; } 
	  if(m==17 && mm == 12) {     te2p[m][mm]= 0.769072; } 
	  if(m==17 && mm == 13) {     te2p[m][mm]= 0.413233; } 
	  if(m==17 && mm == 14) {     te2p[m][mm]= 0.387710; } 
	  if(m==17 && mm == 15) {     te2p[m][mm]= 1.536896; } 
	  if(m==17 && mm == 16) {     te2p[m][mm]= 0.951640; } 
	  if(m==17 && mm == 17) {     te2p[m][mm]= 0.433183; } 
	  if(m==17 && mm == 18) {     te2p[m][mm]= 0.625543; } 
	  if(m==17 && mm == 19) {     te2p[m][mm]= 0.197174; } 
	  if(m==17 && mm == 28) {     te2p[m][mm]= 0.112147; } 
	  if(m==17 && mm == 29) {     te2p[m][mm]= 0.100111; } 
	  if(m==17 && mm == 30) {     te2p[m][mm]= 0.137037; } 
	  if(m==17 && mm == 5) {     te3p[m][mm]= 0.684901; } 
	  if(m==17 && mm == 6) {     te3p[m][mm]= 0.706879; } 
	  if(m==17 && mm == 7) {     te3p[m][mm]= 0.062570; } 
	  if(m==17 && mm == 8) {     te3p[m][mm]= 0.058177; } 
	  if(m==17 && mm == 9) {     te3p[m][mm]= 0.240096; } 
	  if(m==17 && mm == 11) {     te3p[m][mm]= 0.105819; } 
	  if(m==17 && mm == 12) {     te3p[m][mm]= 0.057856; } 
	  if(m==17 && mm == 15) {     te3p[m][mm]= 0.238582; } 
	  if(m==17 && mm == 18) {     te3p[m][mm]= 0.414269; } 
	  if(m==17 && mm == 19) {     te3p[m][mm]= 0.048830; } 
	  if(m==17 && mm == 22) {     te3p[m][mm]= 0.777111; } 
	  if(m==17 && mm == 0) {     tw0n[m][mm]= 0.109215; } 
	  if(m==17 && mm == 4) {     tw0n[m][mm]= 0.019095; } 
	  if(m==17 && mm == 6) {     tw0n[m][mm]= 0.047476; } 
	  if(m==17 && mm == 11) {     tw0n[m][mm]= 0.148177; } 
	  if(m==17 && mm == 12) {     tw0n[m][mm]= 0.217972; } 
	  if(m==17 && mm == 13) {     tw0n[m][mm]= 0.231342; } 
	  if(m==17 && mm == 14) {     tw0n[m][mm]= 0.186821; } 
	  if(m==17 && mm == 15) {     tw0n[m][mm]= 0.575452; } 
	  if(m==17 && mm == 16) {     tw0n[m][mm]= 0.247652; } 
	  if(m==17 && mm == 18) {     tw0n[m][mm]= 0.426688; } 
	  if(m==17 && mm == 19) {     tw0n[m][mm]= 0.102723; } 
	  if(m==17 && mm == 21) {     tw0n[m][mm]= 0.176687; } 
	  if(m==17 && mm == 22) {     tw0n[m][mm]= 0.133123; } 
	  if(m==17 && mm == 30) {     tw0n[m][mm]= 0.335006; } 
	  if(m==17 && mm == 8) {     tw1n[m][mm]= 0.321770; } 
	  if(m==17 && mm == 9) {     tw1n[m][mm]= 0.374855; } 
	  if(m==17 && mm == 12) {     tw1n[m][mm]= 0.384823; } 
	  if(m==17 && mm == 13) {     tw1n[m][mm]= 0.401992; } 
	  if(m==17 && mm == 14) {     tw1n[m][mm]= 0.364939; } 
	  if(m==17 && mm == 15) {     tw1n[m][mm]= 0.328909; } 
	  if(m==17 && mm == 16) {     tw1n[m][mm]= 0.282231; } 
	  if(m==17 && mm == 18) {     tw1n[m][mm]= 0.383372; } 
	  if(m==17 && mm == 19) {     tw1n[m][mm]= 0.056512; } 
	  if(m==17 && mm == 20) {     tw1n[m][mm]= 0.124876; } 
	  if(m==17 && mm == 24) {     tw1n[m][mm]= 0.521912; } 
	  if(m==17 && mm == 29) {     tw1n[m][mm]= 0.156712; } 
	  if(m==17 && mm == 30) {     tw1n[m][mm]= 0.301725; } 
	  if(m==17 && mm == 31) {     tw1n[m][mm]= 0.186943; } 
	  if(m==17 && mm == 32) {     tw1n[m][mm]= 0.424141; } 
	  if(m==17 && mm == 34) {     tw1n[m][mm]= 0.070717; } 
	  if(m==17 && mm == 2) {     tw2n[m][mm]= 0.150255; } 
	  if(m==17 && mm == 3) {     tw2n[m][mm]= 0.736674; } 
	  if(m==17 && mm == 4) {     tw2n[m][mm]= 0.080461; } 
	  if(m==17 && mm == 5) {     tw2n[m][mm]= 0.246393; } 
	  if(m==17 && mm == 6) {     tw2n[m][mm]= 0.071616; } 
	  if(m==17 && mm == 7) {     tw2n[m][mm]= 0.026018; } 
	  if(m==17 && mm == 8) {     tw2n[m][mm]= 0.707855; } 
	  if(m==17 && mm == 20) {     tw2n[m][mm]= 0.537172; } 
	  if(m==17 && mm == 23) {     tw2n[m][mm]= 0.281007; } 
	  if(m==17 && mm == 25) {     tw2n[m][mm]= 0.037830; } 
	  if(m==17 && mm == 26) {     tw2n[m][mm]= 0.038985; } 
	  if(m==17 && mm == 30) {     tw2n[m][mm]= 0.009365; } 
	  if(m==17 && mm == 32) {     tw2n[m][mm]= 0.059356; } 
	  if(m==17 && mm == 34) {     tw2n[m][mm]= 0.256499; } 
	  if(m==17 && mm == 35) {     tw2n[m][mm]= 0.164864; } 
	  if(m==17 && mm == 0) {     tw3n[m][mm]= 0.651720; } 
	  if(m==17 && mm == 1) {     tw3n[m][mm]= 1.471791; } 
	  if(m==17 && mm == 2) {     tw3n[m][mm]= 0.235972; } 
	  if(m==17 && mm == 4) {     tw3n[m][mm]= 0.327468; } 
	  if(m==17 && mm == 5) {     tw3n[m][mm]= 0.322094; } 
	  if(m==17 && mm == 6) {     tw3n[m][mm]= 0.624310; } 
	  if(m==17 && mm == 7) {     tw3n[m][mm]= 0.731851; } 
	  if(m==17 && mm == 8) {     tw3n[m][mm]= 0.735733; } 
	  if(m==17 && mm == 9) {     tw3n[m][mm]= 0.570136; } 
	  if(m==17 && mm == 10) {     tw3n[m][mm]= 0.410265; } 
	  if(m==17 && mm == 11) {     tw3n[m][mm]= 0.421001; } 
	  if(m==17 && mm == 18) {     tw3n[m][mm]= 0.227631; } 
	  if(m==17 && mm == 22) {     tw3n[m][mm]= 0.351887; } 
	  if(m==17 && mm == 23) {     tw3n[m][mm]= 0.361084; } 
	  if(m==17 && mm == 10) {     te2n[m][mm]= 0.157314; } 
	  if(m==17 && mm == 11) {     te2n[m][mm]= 0.122237; } 
	  if(m==17 && mm == 12) {     te2n[m][mm]= 0.601417; } 
	  if(m==17 && mm == 13) {     te2n[m][mm]= 0.211148; } 
	  if(m==17 && mm == 14) {     te2n[m][mm]= 0.602377; } 
	  if(m==17 && mm == 15) {     te2n[m][mm]= 1.221452; } 
	  if(m==17 && mm == 16) {     te2n[m][mm]= 0.742908; } 
	  if(m==17 && mm == 17) {     te2n[m][mm]= 0.224609; } 
	  if(m==17 && mm == 18) {     te2n[m][mm]= 0.295618; } 
	  if(m==17 && mm == 19) {     te2n[m][mm]= 0.003203; } 
	  if(m==17 && mm == 21) {     te2n[m][mm]= 0.008188; } 
	  if(m==17 && mm == 28) {     te2n[m][mm]= 0.173663; } 
	  if(m==17 && mm == 30) {     te2n[m][mm]= 0.261986; } 
	  if(m==17 && mm == 32) {     te2n[m][mm]= 0.079407; } 
	  if(m==17 && mm == 34) {     te2n[m][mm]= 0.939178; } 
	  if(m==17 && mm == 35) {     te2n[m][mm]= 0.371101; } 
	  if(m==17 && mm == 1) {     te3n[m][mm]= 2.317076; } 
	  if(m==17 && mm == 2) {     te3n[m][mm]= 0.907174; } 
	  if(m==17 && mm == 5) {     te3n[m][mm]= 0.025577; } 
	  if(m==17 && mm == 6) {     te3n[m][mm]= 0.342183; } 
	  if(m==17 && mm == 19) {     te3n[m][mm]= 0.063048; } 
	  if(m==17 && mm == 22) {     te3n[m][mm]= 0.093878; } 
	  if(m==17 && mm == 24) {     te3n[m][mm]= 0.078923; } 
	  if(m==17 && mm == 27) {     te3n[m][mm]= 0.167536; } 
	  if(m==17 && mm == 30) {     te3n[m][mm]= 0.262304; } 
	  if(m==17 && mm == 32) {     te3n[m][mm]= 0.240589; } 
	  if(m==17 && mm == 33) {     te3n[m][mm]= 0.064421; } 
	  if(m==17 && mm == 34) {     te3n[m][mm]= 0.383269; } 
	  if(m==17 && mm == 35) {     te3n[m][mm]= 0.250671; } 
	  if(m==18 && mm == 7) {     tw0p[m][mm]= 0.202316; } 
	  if(m==18 && mm == 10) {     tw0p[m][mm]= 0.435412; } 
	  if(m==18 && mm == 16) {     tw0p[m][mm]= 0.255661; } 
	  if(m==18 && mm == 17) {     tw0p[m][mm]= 0.272407; } 
	  if(m==18 && mm == 18) {     tw0p[m][mm]= 0.282365; } 
	  if(m==18 && mm == 19) {     tw0p[m][mm]= 0.022252; } 
	  if(m==18 && mm == 22) {     tw0p[m][mm]= 0.152481; } 
	  if(m==18 && mm == 24) {     tw0p[m][mm]= 0.348770; } 
	  if(m==18 && mm == 25) {     tw0p[m][mm]= 0.399758; } 
	  if(m==18 && mm == 26) {     tw0p[m][mm]= 0.057123; } 
	  if(m==18 && mm == 27) {     tw0p[m][mm]= 0.084534; } 
	  if(m==18 && mm == 28) {     tw0p[m][mm]= 0.102543; } 
	  if(m==18 && mm == 30) {     tw0p[m][mm]= 0.665116; } 
	  if(m==18 && mm == 31) {     tw0p[m][mm]= 0.429401; } 
	  if(m==18 && mm == 32) {     tw0p[m][mm]= 0.407324; } 
	  if(m==18 && mm == 33) {     tw0p[m][mm]= 0.088394; } 
	  if(m==18 && mm == 34) {     tw0p[m][mm]= 0.095032; } 
	  if(m==18 && mm == 35) {     tw0p[m][mm]= 0.253870; } 
	  if(m==18 && mm == 2) {     tw1p[m][mm]= 0.006983; } 
	  if(m==18 && mm == 3) {     tw1p[m][mm]= 0.810804; } 
	  if(m==18 && mm == 19) {     tw1p[m][mm]= 0.162823; } 
	  if(m==18 && mm == 20) {     tw1p[m][mm]= 0.372284; } 
	  if(m==18 && mm == 21) {     tw1p[m][mm]= 0.136683; } 
	  if(m==18 && mm == 22) {     tw1p[m][mm]= 0.443130; } 
	  if(m==18 && mm == 23) {     tw1p[m][mm]= 0.502830; } 
	  if(m==18 && mm == 24) {     tw1p[m][mm]= 0.218581; } 
	  if(m==18 && mm == 25) {     tw1p[m][mm]= 0.391350; } 
	  if(m==18 && mm == 26) {     tw1p[m][mm]= 0.278290; } 
	  if(m==18 && mm == 27) {     tw1p[m][mm]= 0.117579; } 
	  if(m==18 && mm == 28) {     tw1p[m][mm]= 0.139107; } 
	  if(m==18 && mm == 29) {     tw1p[m][mm]= 0.121881; } 
	  if(m==18 && mm == 30) {     tw1p[m][mm]= 0.573402; } 
	  if(m==18 && mm == 31) {     tw1p[m][mm]= 0.223449; } 
	  if(m==18 && mm == 32) {     tw1p[m][mm]= 0.263741; } 
	  if(m==18 && mm == 34) {     tw1p[m][mm]= 0.790866; } 
	  if(m==18 && mm == 35) {     tw1p[m][mm]= 0.969737; } 
	  if(m==18 && mm == 5) {     tw2p[m][mm]= 0.320198; } 
	  if(m==18 && mm == 7) {     tw2p[m][mm]= 0.172987; } 
	  if(m==18 && mm == 11) {     tw2p[m][mm]= 0.244999; } 
	  if(m==18 && mm == 13) {     tw2p[m][mm]= 0.239627; } 
	  if(m==18 && mm == 14) {     tw2p[m][mm]= 0.131215; } 
	  if(m==18 && mm == 15) {     tw2p[m][mm]= 0.053742; } 
	  if(m==18 && mm == 17) {     tw2p[m][mm]= 0.059771; } 
	  if(m==18 && mm == 18) {     tw2p[m][mm]= 0.089484; } 
	  if(m==18 && mm == 19) {     tw2p[m][mm]= 0.239189; } 
	  if(m==18 && mm == 20) {     tw2p[m][mm]= 0.278251; } 
	  if(m==18 && mm == 21) {     tw2p[m][mm]= 0.530450; } 
	  if(m==18 && mm == 22) {     tw2p[m][mm]= 0.147755; } 
	  if(m==18 && mm == 31) {     tw2p[m][mm]= 0.246769; } 
	  if(m==18 && mm == 33) {     tw2p[m][mm]= 0.020981; } 
	  if(m==18 && mm == 34) {     tw2p[m][mm]= 0.387820; } 
	  if(m==18 && mm == 7) {     tw3p[m][mm]= 0.192218; } 
	  if(m==18 && mm == 8) {     tw3p[m][mm]= 0.131143; } 
	  if(m==18 && mm == 10) {     tw3p[m][mm]= 0.453655; } 
	  if(m==18 && mm == 11) {     tw3p[m][mm]= 0.561182; } 
	  if(m==18 && mm == 14) {     tw3p[m][mm]= 0.000576; } 
	  if(m==18 && mm == 15) {     tw3p[m][mm]= 0.326181; } 
	  if(m==18 && mm == 23) {     tw3p[m][mm]= 0.696941; } 
	  if(m==18 && mm == 24) {     tw3p[m][mm]= 0.221299; } 
	  if(m==18 && mm == 27) {     tw3p[m][mm]= 0.146482; } 
	  if(m==18 && mm == 32) {     tw3p[m][mm]= 0.502001; } 
	  if(m==18 && mm == 0) {     te2p[m][mm]= 0.079349; } 
	  if(m==18 && mm == 1) {     te2p[m][mm]= 0.282207; } 
	  if(m==18 && mm == 2) {     te2p[m][mm]= 0.241054; } 
	  if(m==18 && mm == 3) {     te2p[m][mm]= 1.207088; } 
	  if(m==18 && mm == 4) {     te2p[m][mm]= 0.124563; } 
	  if(m==18 && mm == 7) {     te2p[m][mm]= 0.319204; } 
	  if(m==18 && mm == 9) {     te2p[m][mm]= 0.078156; } 
	  if(m==18 && mm == 10) {     te2p[m][mm]= 0.176990; } 
	  if(m==18 && mm == 11) {     te2p[m][mm]= 0.313232; } 
	  if(m==18 && mm == 12) {     te2p[m][mm]= 0.447252; } 
	  if(m==18 && mm == 13) {     te2p[m][mm]= 0.062301; } 
	  if(m==18 && mm == 14) {     te2p[m][mm]= 0.259096; } 
	  if(m==18 && mm == 15) {     te2p[m][mm]= 0.174838; } 
	  if(m==18 && mm == 17) {     te2p[m][mm]= 0.412064; } 
	  if(m==18 && mm == 18) {     te2p[m][mm]= 0.697157; } 
	  if(m==18 && mm == 19) {     te2p[m][mm]= 0.102336; } 
	  if(m==18 && mm == 20) {     te2p[m][mm]= 0.008039; } 
	  if(m==18 && mm == 21) {     te2p[m][mm]= 0.248915; } 
	  if(m==18 && mm == 22) {     te2p[m][mm]= 0.543553; } 
	  if(m==18 && mm == 23) {     te2p[m][mm]= 0.054302; } 
	  if(m==18 && mm == 4) {     te3p[m][mm]= 0.432662; } 
	  if(m==18 && mm == 7) {     te3p[m][mm]= 0.360205; } 
	  if(m==18 && mm == 8) {     te3p[m][mm]= 0.253679; } 
	  if(m==18 && mm == 9) {     te3p[m][mm]= 0.304333; } 
	  if(m==18 && mm == 10) {     te3p[m][mm]= 0.078820; } 
	  if(m==18 && mm == 11) {     te3p[m][mm]= 0.303074; } 
	  if(m==18 && mm == 12) {     te3p[m][mm]= 0.107219; } 
	  if(m==18 && mm == 13) {     te3p[m][mm]= 0.110914; } 
	  if(m==18 && mm == 14) {     te3p[m][mm]= 0.120898; } 
	  if(m==18 && mm == 20) {     te3p[m][mm]= 0.181254; } 
	  if(m==18 && mm == 21) {     te3p[m][mm]= 0.244110; } 
	  if(m==18 && mm == 22) {     te3p[m][mm]= 0.065499; } 
	  if(m==18 && mm == 0) {     tw0n[m][mm]= 0.121010; } 
	  if(m==18 && mm == 4) {     tw0n[m][mm]= 0.022404; } 
	  if(m==18 && mm == 5) {     tw0n[m][mm]= 0.848900; } 
	  if(m==18 && mm == 7) {     tw0n[m][mm]= 0.310127; } 
	  if(m==18 && mm == 8) {     tw0n[m][mm]= 0.013572; } 
	  if(m==18 && mm == 10) {     tw0n[m][mm]= 0.896245; } 
	  if(m==18 && mm == 11) {     tw0n[m][mm]= 0.030869; } 
	  if(m==18 && mm == 13) {     tw0n[m][mm]= 0.034492; } 
	  if(m==18 && mm == 16) {     tw0n[m][mm]= 0.100132; } 
	  if(m==18 && mm == 17) {     tw0n[m][mm]= 0.256685; } 
	  if(m==18 && mm == 18) {     tw0n[m][mm]= 0.301267; } 
	  if(m==18 && mm == 22) {     tw0n[m][mm]= 0.012356; } 
	  if(m==18 && mm == 24) {     tw0n[m][mm]= 0.107638; } 
	  if(m==18 && mm == 26) {     tw0n[m][mm]= 0.922473; } 
	  if(m==18 && mm == 30) {     tw0n[m][mm]= 0.189341; } 
	  if(m==18 && mm == 8) {     tw1n[m][mm]= 0.042938; } 
	  if(m==18 && mm == 12) {     tw1n[m][mm]= 0.060796; } 
	  if(m==18 && mm == 14) {     tw1n[m][mm]= 0.706974; } 
	  if(m==18 && mm == 15) {     tw1n[m][mm]= 0.421190; } 
	  if(m==18 && mm == 16) {     tw1n[m][mm]= 0.091920; } 
	  if(m==18 && mm == 18) {     tw1n[m][mm]= 0.790821; } 
	  if(m==18 && mm == 19) {     tw1n[m][mm]= 0.110593; } 
	  if(m==18 && mm == 22) {     tw1n[m][mm]= 0.322383; } 
	  if(m==18 && mm == 30) {     tw1n[m][mm]= 0.191637; } 
	  if(m==18 && mm == 32) {     tw1n[m][mm]= 0.024313; } 
	  if(m==18 && mm == 34) {     tw1n[m][mm]= 0.345756; } 
	  if(m==18 && mm == 35) {     tw1n[m][mm]= 0.753083; } 
	  if(m==18 && mm == 0) {     tw2n[m][mm]= 0.292491; } 
	  if(m==18 && mm == 4) {     tw2n[m][mm]= 0.495974; } 
	  if(m==18 && mm == 5) {     tw2n[m][mm]= 0.072065; } 
	  if(m==18 && mm == 7) {     tw2n[m][mm]= 0.147089; } 
	  if(m==18 && mm == 8) {     tw2n[m][mm]= 0.200905; } 
	  if(m==18 && mm == 14) {     tw2n[m][mm]= 0.115491; } 
	  if(m==18 && mm == 15) {     tw2n[m][mm]= 0.066669; } 
	  if(m==18 && mm == 19) {     tw2n[m][mm]= 0.008877; } 
	  if(m==18 && mm == 20) {     tw2n[m][mm]= 0.165191; } 
	  if(m==18 && mm == 21) {     tw2n[m][mm]= 0.316905; } 
	  if(m==18 && mm == 25) {     tw2n[m][mm]= 0.133803; } 
	  if(m==18 && mm == 26) {     tw2n[m][mm]= 0.048025; } 
	  if(m==18 && mm == 28) {     tw2n[m][mm]= 0.080434; } 
	  if(m==18 && mm == 29) {     tw2n[m][mm]= 0.128662; } 
	  if(m==18 && mm == 30) {     tw2n[m][mm]= 0.027939; } 
	  if(m==18 && mm == 31) {     tw2n[m][mm]= 0.028215; } 
	  if(m==18 && mm == 32) {     tw2n[m][mm]= 0.132269; } 
	  if(m==18 && mm == 34) {     tw2n[m][mm]= 0.049162; } 
	  if(m==18 && mm == 7) {     tw3n[m][mm]= 0.279291; } 
	  if(m==18 && mm == 8) {     tw3n[m][mm]= 0.078458; } 
	  if(m==18 && mm == 11) {     tw3n[m][mm]= 0.395128; } 
	  if(m==18 && mm == 14) {     tw3n[m][mm]= 0.002278; } 
	  if(m==18 && mm == 15) {     tw3n[m][mm]= 0.294917; } 
	  if(m==18 && mm == 23) {     tw3n[m][mm]= 0.270574; } 
	  if(m==18 && mm == 4) {     te2n[m][mm]= 0.197462; } 
	  if(m==18 && mm == 7) {     te2n[m][mm]= 0.003785; } 
	  if(m==18 && mm == 9) {     te2n[m][mm]= 0.101894; } 
	  if(m==18 && mm == 11) {     te2n[m][mm]= 0.029716; } 
	  if(m==18 && mm == 12) {     te2n[m][mm]= 0.143044; } 
	  if(m==18 && mm == 13) {     te2n[m][mm]= 0.393254; } 
	  if(m==18 && mm == 14) {     te2n[m][mm]= 0.153025; } 
	  if(m==18 && mm == 15) {     te2n[m][mm]= 0.489375; } 
	  if(m==18 && mm == 17) {     te2n[m][mm]= 0.451979; } 
	  if(m==18 && mm == 18) {     te2n[m][mm]= 0.675459; } 
	  if(m==18 && mm == 19) {     te2n[m][mm]= 0.102003; } 
	  if(m==18 && mm == 22) {     te2n[m][mm]= 0.818199; } 
	  if(m==18 && mm == 23) {     te2n[m][mm]= 0.172649; } 
	  if(m==18 && mm == 26) {     te2n[m][mm]= 0.147855; } 
	  if(m==18 && mm == 31) {     te2n[m][mm]= 0.565142; } 
	  if(m==18 && mm == 32) {     te2n[m][mm]= 0.249564; } 
	  if(m==18 && mm == 34) {     te2n[m][mm]= 0.275251; } 
	  if(m==18 && mm == 35) {     te2n[m][mm]= 0.181729; } 
	  if(m==18 && mm == 2) {     te3n[m][mm]= 0.648189; } 
	  if(m==18 && mm == 21) {     te3n[m][mm]= 0.033795; } 
	  if(m==18 && mm == 24) {     te3n[m][mm]= 0.510454; } 
	  if(m==18 && mm == 25) {     te3n[m][mm]= 0.142913; } 
	  if(m==18 && mm == 26) {     te3n[m][mm]= 0.052743; } 
	  if(m==18 && mm == 27) {     te3n[m][mm]= 0.094123; } 
	  if(m==18 && mm == 28) {     te3n[m][mm]= 0.129019; } 
	  if(m==18 && mm == 30) {     te3n[m][mm]= 0.297530; } 
	  if(m==18 && mm == 35) {     te3n[m][mm]= 0.175714; } 
	  if(m==19 && mm == 9) {     tw0p[m][mm]= 0.247442; } 
	  if(m==19 && mm == 10) {     tw0p[m][mm]= 0.319714; } 
	  if(m==19 && mm == 12) {     tw0p[m][mm]= 0.166958; } 
	  if(m==19 && mm == 13) {     tw0p[m][mm]= 0.178164; } 
	  if(m==19 && mm == 14) {     tw0p[m][mm]= 0.139989; } 
	  if(m==19 && mm == 16) {     tw0p[m][mm]= 0.274892; } 
	  if(m==19 && mm == 17) {     tw0p[m][mm]= 0.126021; } 
	  if(m==19 && mm == 18) {     tw0p[m][mm]= 0.640459; } 
	  if(m==19 && mm == 19) {     tw0p[m][mm]= 0.527083; } 
	  if(m==19 && mm == 23) {     tw0p[m][mm]= 0.404981; } 
	  if(m==19 && mm == 24) {     tw0p[m][mm]= 0.788620; } 
	  if(m==19 && mm == 25) {     tw0p[m][mm]= 0.592259; } 
	  if(m==19 && mm == 27) {     tw0p[m][mm]= 0.182080; } 
	  if(m==19 && mm == 28) {     tw0p[m][mm]= 0.074309; } 
	  if(m==19 && mm == 30) {     tw0p[m][mm]= 0.325322; } 
	  if(m==19 && mm == 31) {     tw0p[m][mm]= 0.146869; } 
	  if(m==19 && mm == 32) {     tw0p[m][mm]= 0.233061; } 
	  if(m==19 && mm == 33) {     tw0p[m][mm]= 0.448408; } 
	  if(m==19 && mm == 34) {     tw0p[m][mm]= 0.715308; } 
	  if(m==19 && mm == 35) {     tw0p[m][mm]= 0.018184; } 
	  if(m==19 && mm == 1) {     tw1p[m][mm]= 0.032670; } 
	  if(m==19 && mm == 19) {     tw1p[m][mm]= 0.306601; } 
	  if(m==19 && mm == 20) {     tw1p[m][mm]= 0.095875; } 
	  if(m==19 && mm == 22) {     tw1p[m][mm]= 0.155624; } 
	  if(m==19 && mm == 23) {     tw1p[m][mm]= 0.387972; } 
	  if(m==19 && mm == 24) {     tw1p[m][mm]= 0.692966; } 
	  if(m==19 && mm == 25) {     tw1p[m][mm]= 0.101145; } 
	  if(m==19 && mm == 26) {     tw1p[m][mm]= 0.361926; } 
	  if(m==19 && mm == 27) {     tw1p[m][mm]= 0.641814; } 
	  if(m==19 && mm == 28) {     tw1p[m][mm]= 0.378289; } 
	  if(m==19 && mm == 29) {     tw1p[m][mm]= 0.232945; } 
	  if(m==19 && mm == 30) {     tw1p[m][mm]= 0.163275; } 
	  if(m==19 && mm == 31) {     tw1p[m][mm]= 0.119995; } 
	  if(m==19 && mm == 32) {     tw1p[m][mm]= 0.222286; } 
	  if(m==19 && mm == 33) {     tw1p[m][mm]= 0.795190; } 
	  if(m==19 && mm == 34) {     tw1p[m][mm]= 0.483919; } 
	  if(m==19 && mm == 35) {     tw1p[m][mm]= 0.525669; } 
	  if(m==19 && mm == 5) {     tw2p[m][mm]= 0.006315; } 
	  if(m==19 && mm == 6) {     tw2p[m][mm]= 0.574154; } 
	  if(m==19 && mm == 7) {     tw2p[m][mm]= 0.262318; } 
	  if(m==19 && mm == 8) {     tw2p[m][mm]= 0.242538; } 
	  if(m==19 && mm == 9) {     tw2p[m][mm]= 0.381356; } 
	  if(m==19 && mm == 11) {     tw2p[m][mm]= 0.150340; } 
	  if(m==19 && mm == 15) {     tw2p[m][mm]= 0.046503; } 
	  if(m==19 && mm == 17) {     tw2p[m][mm]= 0.179208; } 
	  if(m==19 && mm == 21) {     tw2p[m][mm]= 0.094630; } 
	  if(m==19 && mm == 29) {     tw2p[m][mm]= 0.100754; } 
	  if(m==19 && mm == 30) {     tw2p[m][mm]= 0.494895; } 
	  if(m==19 && mm == 31) {     tw2p[m][mm]= 0.334012; } 
	  if(m==19 && mm == 32) {     tw2p[m][mm]= 0.365618; } 
	  if(m==19 && mm == 34) {     tw2p[m][mm]= 0.028257; } 
	  if(m==19 && mm == 35) {     tw2p[m][mm]= 0.248494; } 
	  if(m==19 && mm == 8) {     tw3p[m][mm]= 0.356825; } 
	  if(m==19 && mm == 9) {     tw3p[m][mm]= 0.297407; } 
	  if(m==19 && mm == 10) {     tw3p[m][mm]= 0.516142; } 
	  if(m==19 && mm == 14) {     tw3p[m][mm]= 0.080162; } 
	  if(m==19 && mm == 15) {     tw3p[m][mm]= 0.239639; } 
	  if(m==19 && mm == 28) {     tw3p[m][mm]= 0.361033; } 
	  if(m==19 && mm == 30) {     tw3p[m][mm]= 0.179995; } 
	  if(m==19 && mm == 34) {     tw3p[m][mm]= 0.449149; } 
	  if(m==19 && mm == 35) {     tw3p[m][mm]= 0.062676; } 
	  if(m==19 && mm == 1) {     te2p[m][mm]= 0.280679; } 
	  if(m==19 && mm == 2) {     te2p[m][mm]= 0.220188; } 
	  if(m==19 && mm == 6) {     te2p[m][mm]= 0.442604; } 
	  if(m==19 && mm == 7) {     te2p[m][mm]= 0.400297; } 
	  if(m==19 && mm == 8) {     te2p[m][mm]= 0.400414; } 
	  if(m==19 && mm == 9) {     te2p[m][mm]= 0.007946; } 
	  if(m==19 && mm == 10) {     te2p[m][mm]= 0.694433; } 
	  if(m==19 && mm == 11) {     te2p[m][mm]= 0.325468; } 
	  if(m==19 && mm == 12) {     te2p[m][mm]= 0.606564; } 
	  if(m==19 && mm == 13) {     te2p[m][mm]= 0.464711; } 
	  if(m==19 && mm == 15) {     te2p[m][mm]= 0.179422; } 
	  if(m==19 && mm == 16) {     te2p[m][mm]= 0.086268; } 
	  if(m==19 && mm == 17) {     te2p[m][mm]= 0.046448; } 
	  if(m==19 && mm == 18) {     te2p[m][mm]= 0.492949; } 
	  if(m==19 && mm == 19) {     te2p[m][mm]= 0.488925; } 
	  if(m==19 && mm == 21) {     te2p[m][mm]= 0.158537; } 
	  if(m==19 && mm == 22) {     te2p[m][mm]= 0.667522; } 
	  if(m==19 && mm == 23) {     te2p[m][mm]= 0.314301; } 
	  if(m==19 && mm == 26) {     te2p[m][mm]= 0.213399; } 
	  if(m==19 && mm == 27) {     te2p[m][mm]= 0.063334; } 
	  if(m==19 && mm == 29) {     te2p[m][mm]= 0.461883; } 
	  if(m==19 && mm == 5) {     te3p[m][mm]= 0.255352; } 
	  if(m==19 && mm == 6) {     te3p[m][mm]= 0.521914; } 
	  if(m==19 && mm == 7) {     te3p[m][mm]= 0.223289; } 
	  if(m==19 && mm == 8) {     te3p[m][mm]= 0.276482; } 
	  if(m==19 && mm == 9) {     te3p[m][mm]= 0.237060; } 
	  if(m==19 && mm == 10) {     te3p[m][mm]= 0.481513; } 
	  if(m==19 && mm == 11) {     te3p[m][mm]= 0.988085; } 
	  if(m==19 && mm == 12) {     te3p[m][mm]= 0.102641; } 
	  if(m==19 && mm == 13) {     te3p[m][mm]= 0.260104; } 
	  if(m==19 && mm == 14) {     te3p[m][mm]= 0.658220; } 
	  if(m==19 && mm == 15) {     te3p[m][mm]= 0.256251; } 
	  if(m==19 && mm == 18) {     te3p[m][mm]= 0.015891; } 
	  if(m==19 && mm == 20) {     te3p[m][mm]= 1.049188; } 
	  if(m==19 && mm == 21) {     te3p[m][mm]= 0.020903; } 
	  if(m==19 && mm == 22) {     te3p[m][mm]= 0.371530; } 
	  if(m==19 && mm == 0) {     tw0n[m][mm]= 0.473484; } 
	  if(m==19 && mm == 3) {     tw0n[m][mm]= 0.075697; } 
	  if(m==19 && mm == 5) {     tw0n[m][mm]= 0.666570; } 
	  if(m==19 && mm == 8) {     tw0n[m][mm]= 0.666415; } 
	  if(m==19 && mm == 9) {     tw0n[m][mm]= 0.372907; } 
	  if(m==19 && mm == 10) {     tw0n[m][mm]= 0.369321; } 
	  if(m==19 && mm == 12) {     tw0n[m][mm]= 0.268631; } 
	  if(m==19 && mm == 13) {     tw0n[m][mm]= 0.043934; } 
	  if(m==19 && mm == 17) {     tw0n[m][mm]= 0.098057; } 
	  if(m==19 && mm == 19) {     tw0n[m][mm]= 0.591420; } 
	  if(m==19 && mm == 23) {     tw0n[m][mm]= 0.181166; } 
	  if(m==19 && mm == 24) {     tw0n[m][mm]= 0.205914; } 
	  if(m==19 && mm == 25) {     tw0n[m][mm]= 0.196225; } 
	  if(m==19 && mm == 27) {     tw0n[m][mm]= 0.682734; } 
	  if(m==19 && mm == 8) {     tw1n[m][mm]= 0.035152; } 
	  if(m==19 && mm == 11) {     tw1n[m][mm]= 0.322936; } 
	  if(m==19 && mm == 12) {     tw1n[m][mm]= 0.177604; } 
	  if(m==19 && mm == 13) {     tw1n[m][mm]= 0.260000; } 
	  if(m==19 && mm == 14) {     tw1n[m][mm]= 0.349133; } 
	  if(m==19 && mm == 17) {     tw1n[m][mm]= 0.329427; } 
	  if(m==19 && mm == 18) {     tw1n[m][mm]= 0.081395; } 
	  if(m==19 && mm == 19) {     tw1n[m][mm]= 0.053910; } 
	  if(m==19 && mm == 20) {     tw1n[m][mm]= 0.061166; } 
	  if(m==19 && mm == 21) {     tw1n[m][mm]= 0.165012; } 
	  if(m==19 && mm == 22) {     tw1n[m][mm]= 0.265956; } 
	  if(m==19 && mm == 30) {     tw1n[m][mm]= 0.004616; } 
	  if(m==19 && mm == 34) {     tw1n[m][mm]= 0.270427; } 
	  if(m==19 && mm == 35) {     tw1n[m][mm]= 0.382472; } 
	  if(m==19 && mm == 4) {     tw2n[m][mm]= 0.125268; } 
	  if(m==19 && mm == 7) {     tw2n[m][mm]= 0.649149; } 
	  if(m==19 && mm == 8) {     tw2n[m][mm]= 0.715139; } 
	  if(m==19 && mm == 14) {     tw2n[m][mm]= 0.063348; } 
	  if(m==19 && mm == 15) {     tw2n[m][mm]= 0.018664; } 
	  if(m==19 && mm == 17) {     tw2n[m][mm]= 0.116644; } 
	  if(m==19 && mm == 21) {     tw2n[m][mm]= 0.118405; } 
	  if(m==19 && mm == 22) {     tw2n[m][mm]= 0.196065; } 
	  if(m==19 && mm == 24) {     tw2n[m][mm]= 0.010339; } 
	  if(m==19 && mm == 25) {     tw2n[m][mm]= 0.070213; } 
	  if(m==19 && mm == 26) {     tw2n[m][mm]= 0.010175; } 
	  if(m==19 && mm == 28) {     tw2n[m][mm]= 0.037769; } 
	  if(m==19 && mm == 29) {     tw2n[m][mm]= 0.175240; } 
	  if(m==19 && mm == 30) {     tw2n[m][mm]= 0.187457; } 
	  if(m==19 && mm == 32) {     tw2n[m][mm]= 0.048499; } 
	  if(m==19 && mm == 35) {     tw2n[m][mm]= 0.102621; } 
	  if(m==19 && mm == 8) {     tw3n[m][mm]= 0.344766; } 
	  if(m==19 && mm == 9) {     tw3n[m][mm]= 0.209584; } 
	  if(m==19 && mm == 10) {     tw3n[m][mm]= 0.413209; } 
	  if(m==19 && mm == 11) {     tw3n[m][mm]= 0.011096; } 
	  if(m==19 && mm == 15) {     tw3n[m][mm]= 0.121841; } 
	  if(m==19 && mm == 6) {     te2n[m][mm]= 0.117971; } 
	  if(m==19 && mm == 10) {     te2n[m][mm]= 0.130902; } 
	  if(m==19 && mm == 11) {     te2n[m][mm]= 0.238492; } 
	  if(m==19 && mm == 12) {     te2n[m][mm]= 0.427110; } 
	  if(m==19 && mm == 13) {     te2n[m][mm]= 0.158771; } 
	  if(m==19 && mm == 14) {     te2n[m][mm]= 0.098428; } 
	  if(m==19 && mm == 16) {     te2n[m][mm]= 0.134824; } 
	  if(m==19 && mm == 17) {     te2n[m][mm]= 0.082004; } 
	  if(m==19 && mm == 18) {     te2n[m][mm]= 0.036934; } 
	  if(m==19 && mm == 19) {     te2n[m][mm]= 0.296893; } 
	  if(m==19 && mm == 20) {     te2n[m][mm]= 0.020600; } 
	  if(m==19 && mm == 22) {     te2n[m][mm]= 0.701842; } 
	  if(m==19 && mm == 23) {     te2n[m][mm]= 0.475396; } 
	  if(m==19 && mm == 25) {     te2n[m][mm]= 0.032278; } 
	  if(m==19 && mm == 26) {     te2n[m][mm]= 0.286325; } 
	  if(m==19 && mm == 27) {     te2n[m][mm]= 0.314256; } 
	  if(m==19 && mm == 28) {     te2n[m][mm]= 0.170161; } 
	  if(m==19 && mm == 29) {     te2n[m][mm]= 0.703630; } 
	  if(m==19 && mm == 31) {     te2n[m][mm]= 0.003871; } 
	  if(m==19 && mm == 33) {     te2n[m][mm]= 0.208041; } 
	  if(m==19 && mm == 34) {     te2n[m][mm]= 0.631997; } 
	  if(m==19 && mm == 35) {     te2n[m][mm]= 0.116687; } 
	  if(m==19 && mm == 6) {     te3n[m][mm]= 0.578360; } 
	  if(m==19 && mm == 20) {     te3n[m][mm]= 2.093101; } 
	  if(m==19 && mm == 21) {     te3n[m][mm]= 0.038865; } 
	  if(m==19 && mm == 22) {     te3n[m][mm]= 0.181914; } 
	  if(m==19 && mm == 23) {     te3n[m][mm]= 0.036564; } 
	  if(m==19 && mm == 24) {     te3n[m][mm]= 0.187175; } 
	  if(m==19 && mm == 25) {     te3n[m][mm]= 0.046517; } 
	  if(m==19 && mm == 26) {     te3n[m][mm]= 1.053381; } 
	  if(m==19 && mm == 28) {     te3n[m][mm]= 0.080410; } 
	  if(m==19 && mm == 30) {     te3n[m][mm]= 0.060339; } 
	  if(m==19 && mm == 32) {     te3n[m][mm]= 0.521921; } 
	  if(m==19 && mm == 35) {     te3n[m][mm]= 0.697201; } 
	  if(m==20 && mm == 7) {     tw0p[m][mm]= 0.194149; } 
	  if(m==20 && mm == 11) {     tw0p[m][mm]= 0.285988; } 
	  if(m==20 && mm == 12) {     tw0p[m][mm]= 0.306925; } 
	  if(m==20 && mm == 13) {     tw0p[m][mm]= 0.138495; } 
	  if(m==20 && mm == 14) {     tw0p[m][mm]= 0.126583; } 
	  if(m==20 && mm == 16) {     tw0p[m][mm]= 0.005905; } 
	  if(m==20 && mm == 19) {     tw0p[m][mm]= 0.022679; } 
	  if(m==20 && mm == 20) {     tw0p[m][mm]= 0.016062; } 
	  if(m==20 && mm == 23) {     tw0p[m][mm]= 0.007921; } 
	  if(m==20 && mm == 25) {     tw0p[m][mm]= 0.106565; } 
	  if(m==20 && mm == 30) {     tw0p[m][mm]= 0.711258; } 
	  if(m==20 && mm == 31) {     tw0p[m][mm]= 0.137003; } 
	  if(m==20 && mm == 32) {     tw0p[m][mm]= 0.333137; } 
	  if(m==20 && mm == 33) {     tw0p[m][mm]= 0.192397; } 
	  if(m==20 && mm == 7) {     tw1p[m][mm]= 0.197237; } 
	  if(m==20 && mm == 21) {     tw1p[m][mm]= 0.064944; } 
	  if(m==20 && mm == 22) {     tw1p[m][mm]= 0.161990; } 
	  if(m==20 && mm == 23) {     tw1p[m][mm]= 0.207334; } 
	  if(m==20 && mm == 24) {     tw1p[m][mm]= 0.318150; } 
	  if(m==20 && mm == 25) {     tw1p[m][mm]= 0.793624; } 
	  if(m==20 && mm == 26) {     tw1p[m][mm]= 0.783796; } 
	  if(m==20 && mm == 27) {     tw1p[m][mm]= 0.529522; } 
	  if(m==20 && mm == 28) {     tw1p[m][mm]= 0.170534; } 
	  if(m==20 && mm == 29) {     tw1p[m][mm]= 0.423466; } 
	  if(m==20 && mm == 30) {     tw1p[m][mm]= 0.582622; } 
	  if(m==20 && mm == 31) {     tw1p[m][mm]= 0.022931; } 
	  if(m==20 && mm == 32) {     tw1p[m][mm]= 0.316463; } 
	  if(m==20 && mm == 33) {     tw1p[m][mm]= 0.074528; } 
	  if(m==20 && mm == 34) {     tw1p[m][mm]= 0.454870; } 
	  if(m==20 && mm == 35) {     tw1p[m][mm]= 0.109969; } 
	  if(m==20 && mm == 5) {     tw2p[m][mm]= 0.305824; } 
	  if(m==20 && mm == 8) {     tw2p[m][mm]= 0.515671; } 
	  if(m==20 && mm == 10) {     tw2p[m][mm]= 0.141085; } 
	  if(m==20 && mm == 11) {     tw2p[m][mm]= 0.099586; } 
	  if(m==20 && mm == 12) {     tw2p[m][mm]= 0.096782; } 
	  if(m==20 && mm == 13) {     tw2p[m][mm]= 0.226859; } 
	  if(m==20 && mm == 14) {     tw2p[m][mm]= 0.416797; } 
	  if(m==20 && mm == 16) {     tw2p[m][mm]= 0.304810; } 
	  if(m==20 && mm == 17) {     tw2p[m][mm]= 0.235011; } 
	  if(m==20 && mm == 18) {     tw2p[m][mm]= 0.019857; } 
	  if(m==20 && mm == 19) {     tw2p[m][mm]= 0.420471; } 
	  if(m==20 && mm == 20) {     tw2p[m][mm]= 0.047218; } 
	  if(m==20 && mm == 23) {     tw2p[m][mm]= 0.221797; } 
	  if(m==20 && mm == 28) {     tw2p[m][mm]= 0.422958; } 
	  if(m==20 && mm == 32) {     tw2p[m][mm]= 0.504005; } 
	  if(m==20 && mm == 33) {     tw2p[m][mm]= 0.137569; } 
	  if(m==20 && mm == 34) {     tw2p[m][mm]= 0.663768; } 
	  if(m==20 && mm == 35) {     tw2p[m][mm]= 0.107003; } 
	  if(m==20 && mm == 0) {     tw3p[m][mm]= 0.246072; } 
	  if(m==20 && mm == 1) {     tw3p[m][mm]= 0.388857; } 
	  if(m==20 && mm == 2) {     tw3p[m][mm]= 0.183080; } 
	  if(m==20 && mm == 3) {     tw3p[m][mm]= 0.407403; } 
	  if(m==20 && mm == 4) {     tw3p[m][mm]= 0.404846; } 
	  if(m==20 && mm == 5) {     tw3p[m][mm]= 0.352124; } 
	  if(m==20 && mm == 6) {     tw3p[m][mm]= 0.235473; } 
	  if(m==20 && mm == 7) {     tw3p[m][mm]= 0.531327; } 
	  if(m==20 && mm == 8) {     tw3p[m][mm]= 0.351909; } 
	  if(m==20 && mm == 9) {     tw3p[m][mm]= 0.326169; } 
	  if(m==20 && mm == 10) {     tw3p[m][mm]= 0.117089; } 
	  if(m==20 && mm == 11) {     tw3p[m][mm]= 0.397725; } 
	  if(m==20 && mm == 12) {     tw3p[m][mm]= 0.127717; } 
	  if(m==20 && mm == 13) {     tw3p[m][mm]= 0.313926; } 
	  if(m==20 && mm == 20) {     tw3p[m][mm]= 0.194919; } 
	  if(m==20 && mm == 21) {     tw3p[m][mm]= 0.198481; } 
	  if(m==20 && mm == 22) {     tw3p[m][mm]= 0.215217; } 
	  if(m==20 && mm == 23) {     tw3p[m][mm]= 0.003525; } 
	  if(m==20 && mm == 33) {     tw3p[m][mm]= 0.740243; } 
	  if(m==20 && mm == 0) {     te2p[m][mm]= 0.457238; } 
	  if(m==20 && mm == 1) {     te2p[m][mm]= 0.165062; } 
	  if(m==20 && mm == 2) {     te2p[m][mm]= 0.017165; } 
	  if(m==20 && mm == 3) {     te2p[m][mm]= 0.073853; } 
	  if(m==20 && mm == 4) {     te2p[m][mm]= 0.270544; } 
	  if(m==20 && mm == 5) {     te2p[m][mm]= 0.101594; } 
	  if(m==20 && mm == 6) {     te2p[m][mm]= 0.163230; } 
	  if(m==20 && mm == 7) {     te2p[m][mm]= 0.010936; } 
	  if(m==20 && mm == 8) {     te2p[m][mm]= 1.308157; } 
	  if(m==20 && mm == 9) {     te2p[m][mm]= 2.204064; } 
	  if(m==20 && mm == 11) {     te2p[m][mm]= 0.082322; } 
	  if(m==20 && mm == 12) {     te2p[m][mm]= 0.284646; } 
	  if(m==20 && mm == 13) {     te2p[m][mm]= 0.523328; } 
	  if(m==20 && mm == 15) {     te2p[m][mm]= 0.505525; } 
	  if(m==20 && mm == 16) {     te2p[m][mm]= 0.392349; } 
	  if(m==20 && mm == 17) {     te2p[m][mm]= 0.301468; } 
	  if(m==20 && mm == 18) {     te2p[m][mm]= 0.976907; } 
	  if(m==20 && mm == 19) {     te2p[m][mm]= 0.308554; } 
	  if(m==20 && mm == 22) {     te2p[m][mm]= 0.264825; } 
	  if(m==20 && mm == 24) {     te2p[m][mm]= 0.339937; } 
	  if(m==20 && mm == 26) {     te2p[m][mm]= 0.049456; } 
	  if(m==20 && mm == 27) {     te2p[m][mm]= 0.331318; } 
	  if(m==20 && mm == 30) {     te2p[m][mm]= 0.045480; } 
	  if(m==20 && mm == 5) {     te3p[m][mm]= 1.338816; } 
	  if(m==20 && mm == 6) {     te3p[m][mm]= 0.761239; } 
	  if(m==20 && mm == 9) {     te3p[m][mm]= 0.325058; } 
	  if(m==20 && mm == 12) {     te3p[m][mm]= 0.074914; } 
	  if(m==20 && mm == 13) {     te3p[m][mm]= 0.260062; } 
	  if(m==20 && mm == 16) {     te3p[m][mm]= 0.329612; } 
	  if(m==20 && mm == 17) {     te3p[m][mm]= 0.363796; } 
	  if(m==20 && mm == 22) {     te3p[m][mm]= 0.012938; } 
	  if(m==20 && mm == 3) {     tw0n[m][mm]= 0.031157; } 
	  if(m==20 && mm == 6) {     tw0n[m][mm]= 0.217453; } 
	  if(m==20 && mm == 7) {     tw0n[m][mm]= 0.071864; } 
	  if(m==20 && mm == 9) {     tw0n[m][mm]= 0.079365; } 
	  if(m==20 && mm == 13) {     tw0n[m][mm]= 0.504149; } 
	  if(m==20 && mm == 19) {     tw0n[m][mm]= 0.167479; } 
	  if(m==20 && mm == 30) {     tw0n[m][mm]= 0.141772; } 
	  if(m==20 && mm == 6) {     tw1n[m][mm]= 0.454729; } 
	  if(m==20 && mm == 7) {     tw1n[m][mm]= 0.204599; } 
	  if(m==20 && mm == 8) {     tw1n[m][mm]= 0.096139; } 
	  if(m==20 && mm == 10) {     tw1n[m][mm]= 0.219154; } 
	  if(m==20 && mm == 11) {     tw1n[m][mm]= 0.243314; } 
	  if(m==20 && mm == 12) {     tw1n[m][mm]= 0.363912; } 
	  if(m==20 && mm == 13) {     tw1n[m][mm]= 0.149390; } 
	  if(m==20 && mm == 14) {     tw1n[m][mm]= 0.141487; } 
	  if(m==20 && mm == 17) {     tw1n[m][mm]= 0.428096; } 
	  if(m==20 && mm == 30) {     tw1n[m][mm]= 0.388564; } 
	  if(m==20 && mm == 34) {     tw1n[m][mm]= 0.300969; } 
	  if(m==20 && mm == 1) {     tw2n[m][mm]= 0.372041; } 
	  if(m==20 && mm == 2) {     tw2n[m][mm]= 0.241349; } 
	  if(m==20 && mm == 3) {     tw2n[m][mm]= 0.078724; } 
	  if(m==20 && mm == 4) {     tw2n[m][mm]= 0.162637; } 
	  if(m==20 && mm == 5) {     tw2n[m][mm]= 0.351951; } 
	  if(m==20 && mm == 7) {     tw2n[m][mm]= 0.020170; } 
	  if(m==20 && mm == 8) {     tw2n[m][mm]= 0.178090; } 
	  if(m==20 && mm == 14) {     tw2n[m][mm]= 0.570885; } 
	  if(m==20 && mm == 16) {     tw2n[m][mm]= 0.108599; } 
	  if(m==20 && mm == 17) {     tw2n[m][mm]= 0.165343; } 
	  if(m==20 && mm == 24) {     tw2n[m][mm]= 0.259138; } 
	  if(m==20 && mm == 28) {     tw2n[m][mm]= 0.032557; } 
	  if(m==20 && mm == 32) {     tw2n[m][mm]= 0.110214; } 
	  if(m==20 && mm == 34) {     tw2n[m][mm]= 0.195765; } 
	  if(m==20 && mm == 0) {     tw3n[m][mm]= 0.050656; } 
	  if(m==20 && mm == 1) {     tw3n[m][mm]= 0.435351; } 
	  if(m==20 && mm == 2) {     tw3n[m][mm]= 0.607730; } 
	  if(m==20 && mm == 3) {     tw3n[m][mm]= 0.872547; } 
	  if(m==20 && mm == 4) {     tw3n[m][mm]= 0.488453; } 
	  if(m==20 && mm == 5) {     tw3n[m][mm]= 0.458704; } 
	  if(m==20 && mm == 6) {     tw3n[m][mm]= 0.189439; } 
	  if(m==20 && mm == 7) {     tw3n[m][mm]= 0.512031; } 
	  if(m==20 && mm == 8) {     tw3n[m][mm]= 0.378906; } 
	  if(m==20 && mm == 9) {     tw3n[m][mm]= 0.484469; } 
	  if(m==20 && mm == 10) {     tw3n[m][mm]= 0.086459; } 
	  if(m==20 && mm == 11) {     tw3n[m][mm]= 0.344966; } 
	  if(m==20 && mm == 12) {     tw3n[m][mm]= 0.064429; } 
	  if(m==20 && mm == 13) {     tw3n[m][mm]= 0.317901; } 
	  if(m==20 && mm == 20) {     tw3n[m][mm]= 0.130752; } 
	  if(m==20 && mm == 21) {     tw3n[m][mm]= 0.058148; } 
	  if(m==20 && mm == 22) {     tw3n[m][mm]= 0.074014; } 
	  if(m==20 && mm == 7) {     te2n[m][mm]= 0.566836; } 
	  if(m==20 && mm == 8) {     te2n[m][mm]= 1.032009; } 
	  if(m==20 && mm == 15) {     te2n[m][mm]= 0.308819; } 
	  if(m==20 && mm == 16) {     te2n[m][mm]= 0.293799; } 
	  if(m==20 && mm == 17) {     te2n[m][mm]= 0.109914; } 
	  if(m==20 && mm == 18) {     te2n[m][mm]= 0.978835; } 
	  if(m==20 && mm == 19) {     te2n[m][mm]= 0.568163; } 
	  if(m==20 && mm == 22) {     te2n[m][mm]= 0.363696; } 
	  if(m==20 && mm == 24) {     te2n[m][mm]= 0.467730; } 
	  if(m==20 && mm == 26) {     te2n[m][mm]= 0.163888; } 
	  if(m==20 && mm == 27) {     te2n[m][mm]= 0.348510; } 
	  if(m==20 && mm == 30) {     te2n[m][mm]= 0.289883; } 
	  if(m==20 && mm == 32) {     te2n[m][mm]= 0.030491; } 
	  if(m==20 && mm == 33) {     te2n[m][mm]= 0.096599; } 
	  if(m==20 && mm == 34) {     te2n[m][mm]= 0.131928; } 
	  if(m==20 && mm == 35) {     te2n[m][mm]= 0.230980; } 
	  if(m==20 && mm == 0) {     te3n[m][mm]= 0.151089; } 
	  if(m==20 && mm == 1) {     te3n[m][mm]= 0.018848; } 
	  if(m==20 && mm == 3) {     te3n[m][mm]= 1.937085; } 
	  if(m==20 && mm == 15) {     te3n[m][mm]= 0.151836; } 
	  if(m==20 && mm == 17) {     te3n[m][mm]= 0.046375; } 
	  if(m==20 && mm == 22) {     te3n[m][mm]= 0.137979; } 
	  if(m==20 && mm == 23) {     te3n[m][mm]= 0.003072; } 
	  if(m==20 && mm == 25) {     te3n[m][mm]= 0.338581; } 
	  if(m==20 && mm == 26) {     te3n[m][mm]= 0.617056; } 
	  if(m==21 && mm == 8) {     tw0p[m][mm]= 0.073327; } 
	  if(m==21 && mm == 11) {     tw0p[m][mm]= 0.179041; } 
	  if(m==21 && mm == 13) {     tw0p[m][mm]= 0.166043; } 
	  if(m==21 && mm == 15) {     tw0p[m][mm]= 0.006616; } 
	  if(m==21 && mm == 17) {     tw0p[m][mm]= 0.183577; } 
	  if(m==21 && mm == 18) {     tw0p[m][mm]= 0.203518; } 
	  if(m==21 && mm == 19) {     tw0p[m][mm]= 0.086681; } 
	  if(m==21 && mm == 20) {     tw0p[m][mm]= 0.022408; } 
	  if(m==21 && mm == 21) {     tw0p[m][mm]= 0.194707; } 
	  if(m==21 && mm == 23) {     tw0p[m][mm]= 0.273875; } 
	  if(m==21 && mm == 24) {     tw0p[m][mm]= 0.661533; } 
	  if(m==21 && mm == 25) {     tw0p[m][mm]= 0.185706; } 
	  if(m==21 && mm == 26) {     tw0p[m][mm]= 0.331811; } 
	  if(m==21 && mm == 27) {     tw0p[m][mm]= 0.120476; } 
	  if(m==21 && mm == 28) {     tw0p[m][mm]= 0.413680; } 
	  if(m==21 && mm == 29) {     tw0p[m][mm]= 0.329698; } 
	  if(m==21 && mm == 30) {     tw0p[m][mm]= 0.453738; } 
	  if(m==21 && mm == 31) {     tw0p[m][mm]= 0.376860; } 
	  if(m==21 && mm == 32) {     tw0p[m][mm]= 0.174809; } 
	  if(m==21 && mm == 33) {     tw0p[m][mm]= 0.061289; } 
	  if(m==21 && mm == 6) {     tw1p[m][mm]= 0.324362; } 
	  if(m==21 && mm == 19) {     tw1p[m][mm]= 0.200910; } 
	  if(m==21 && mm == 20) {     tw1p[m][mm]= 0.064217; } 
	  if(m==21 && mm == 21) {     tw1p[m][mm]= 0.015376; } 
	  if(m==21 && mm == 22) {     tw1p[m][mm]= 0.321746; } 
	  if(m==21 && mm == 25) {     tw1p[m][mm]= 0.677170; } 
	  if(m==21 && mm == 26) {     tw1p[m][mm]= 0.655443; } 
	  if(m==21 && mm == 27) {     tw1p[m][mm]= 0.801220; } 
	  if(m==21 && mm == 28) {     tw1p[m][mm]= 0.387532; } 
	  if(m==21 && mm == 29) {     tw1p[m][mm]= 0.298955; } 
	  if(m==21 && mm == 30) {     tw1p[m][mm]= 0.808701; } 
	  if(m==21 && mm == 31) {     tw1p[m][mm]= 0.348038; } 
	  if(m==21 && mm == 32) {     tw1p[m][mm]= 0.232762; } 
	  if(m==21 && mm == 33) {     tw1p[m][mm]= 0.078457; } 
	  if(m==21 && mm == 34) {     tw1p[m][mm]= 0.387828; } 
	  if(m==21 && mm == 35) {     tw1p[m][mm]= 0.316945; } 
	  if(m==21 && mm == 5) {     tw2p[m][mm]= 0.104379; } 
	  if(m==21 && mm == 7) {     tw2p[m][mm]= 0.299417; } 
	  if(m==21 && mm == 9) {     tw2p[m][mm]= 0.533950; } 
	  if(m==21 && mm == 10) {     tw2p[m][mm]= 0.249965; } 
	  if(m==21 && mm == 11) {     tw2p[m][mm]= 0.187592; } 
	  if(m==21 && mm == 16) {     tw2p[m][mm]= 0.219207; } 
	  if(m==21 && mm == 17) {     tw2p[m][mm]= 0.232746; } 
	  if(m==21 && mm == 18) {     tw2p[m][mm]= 0.319196; } 
	  if(m==21 && mm == 19) {     tw2p[m][mm]= 0.523139; } 
	  if(m==21 && mm == 21) {     tw2p[m][mm]= 0.256129; } 
	  if(m==21 && mm == 23) {     tw2p[m][mm]= 0.439377; } 
	  if(m==21 && mm == 28) {     tw2p[m][mm]= 0.638414; } 
	  if(m==21 && mm == 29) {     tw2p[m][mm]= 0.613696; } 
	  if(m==21 && mm == 30) {     tw2p[m][mm]= 0.180418; } 
	  if(m==21 && mm == 31) {     tw2p[m][mm]= 0.344567; } 
	  if(m==21 && mm == 32) {     tw2p[m][mm]= 0.408582; } 
	  if(m==21 && mm == 33) {     tw2p[m][mm]= 0.172516; } 
	  if(m==21 && mm == 34) {     tw2p[m][mm]= 0.474116; } 
	  if(m==21 && mm == 35) {     tw2p[m][mm]= 0.503491; } 
	  if(m==21 && mm == 0) {     tw3p[m][mm]= 0.603433; } 
	  if(m==21 && mm == 1) {     tw3p[m][mm]= 0.271736; } 
	  if(m==21 && mm == 2) {     tw3p[m][mm]= 0.299390; } 
	  if(m==21 && mm == 3) {     tw3p[m][mm]= 0.327136; } 
	  if(m==21 && mm == 4) {     tw3p[m][mm]= 0.202562; } 
	  if(m==21 && mm == 5) {     tw3p[m][mm]= 0.236978; } 
	  if(m==21 && mm == 6) {     tw3p[m][mm]= 0.253163; } 
	  if(m==21 && mm == 7) {     tw3p[m][mm]= 0.321236; } 
	  if(m==21 && mm == 8) {     tw3p[m][mm]= 0.251526; } 
	  if(m==21 && mm == 9) {     tw3p[m][mm]= 0.530204; } 
	  if(m==21 && mm == 10) {     tw3p[m][mm]= 0.417739; } 
	  if(m==21 && mm == 12) {     tw3p[m][mm]= 0.010305; } 
	  if(m==21 && mm == 13) {     tw3p[m][mm]= 0.105956; } 
	  if(m==21 && mm == 20) {     tw3p[m][mm]= 0.360109; } 
	  if(m==21 && mm == 21) {     tw3p[m][mm]= 0.303477; } 
	  if(m==21 && mm == 22) {     tw3p[m][mm]= 0.061412; } 
	  if(m==21 && mm == 23) {     tw3p[m][mm]= 0.222831; } 
	  if(m==21 && mm == 0) {     te2p[m][mm]= 0.125508; } 
	  if(m==21 && mm == 1) {     te2p[m][mm]= 0.424850; } 
	  if(m==21 && mm == 2) {     te2p[m][mm]= 0.024705; } 
	  if(m==21 && mm == 4) {     te2p[m][mm]= 0.096482; } 
	  if(m==21 && mm == 8) {     te2p[m][mm]= 0.191078; } 
	  if(m==21 && mm == 9) {     te2p[m][mm]= 0.216512; } 
	  if(m==21 && mm == 10) {     te2p[m][mm]= 0.078094; } 
	  if(m==21 && mm == 11) {     te2p[m][mm]= 0.459648; } 
	  if(m==21 && mm == 14) {     te2p[m][mm]= 0.364671; } 
	  if(m==21 && mm == 18) {     te2p[m][mm]= 0.315366; } 
	  if(m==21 && mm == 21) {     te2p[m][mm]= 0.073300; } 
	  if(m==21 && mm == 22) {     te2p[m][mm]= 0.498706; } 
	  if(m==21 && mm == 25) {     te2p[m][mm]= 0.100080; } 
	  if(m==21 && mm == 26) {     te2p[m][mm]= 0.335240; } 
	  if(m==21 && mm == 28) {     te2p[m][mm]= 0.036693; } 
	  if(m==21 && mm == 30) {     te2p[m][mm]= 0.146831; } 
	  if(m==21 && mm == 31) {     te2p[m][mm]= 0.821527; } 
	  if(m==21 && mm == 4) {     te3p[m][mm]= 0.139130; } 
	  if(m==21 && mm == 5) {     te3p[m][mm]= 0.043651; } 
	  if(m==21 && mm == 8) {     te3p[m][mm]= 0.474322; } 
	  if(m==21 && mm == 9) {     te3p[m][mm]= 0.203357; } 
	  if(m==21 && mm == 14) {     te3p[m][mm]= 0.013107; } 
	  if(m==21 && mm == 15) {     te3p[m][mm]= 0.049359; } 
	  if(m==21 && mm == 17) {     te3p[m][mm]= 0.390090; } 
	  if(m==21 && mm == 18) {     te3p[m][mm]= 0.269253; } 
	  if(m==21 && mm == 22) {     te3p[m][mm]= 0.866312; } 
	  if(m==21 && mm == 4) {     tw0n[m][mm]= 0.053203; } 
	  if(m==21 && mm == 8) {     tw0n[m][mm]= 0.278812; } 
	  if(m==21 && mm == 10) {     tw0n[m][mm]= 0.143516; } 
	  if(m==21 && mm == 11) {     tw0n[m][mm]= 0.250574; } 
	  if(m==21 && mm == 12) {     tw0n[m][mm]= 0.219842; } 
	  if(m==21 && mm == 13) {     tw0n[m][mm]= 0.482169; } 
	  if(m==21 && mm == 14) {     tw0n[m][mm]= 0.074814; } 
	  if(m==21 && mm == 18) {     tw0n[m][mm]= 0.082870; } 
	  if(m==21 && mm == 19) {     tw0n[m][mm]= 0.046857; } 
	  if(m==21 && mm == 21) {     tw0n[m][mm]= 0.086103; } 
	  if(m==21 && mm == 23) {     tw0n[m][mm]= 0.026736; } 
	  if(m==21 && mm == 26) {     tw0n[m][mm]= 0.430706; } 
	  if(m==21 && mm == 27) {     tw0n[m][mm]= 0.040326; } 
	  if(m==21 && mm == 9) {     tw1n[m][mm]= 0.087873; } 
	  if(m==21 && mm == 10) {     tw1n[m][mm]= 0.143955; } 
	  if(m==21 && mm == 12) {     tw1n[m][mm]= 0.472487; } 
	  if(m==21 && mm == 13) {     tw1n[m][mm]= 0.286265; } 
	  if(m==21 && mm == 14) {     tw1n[m][mm]= 0.495367; } 
	  if(m==21 && mm == 17) {     tw1n[m][mm]= 0.051039; } 
	  if(m==21 && mm == 18) {     tw1n[m][mm]= 0.321732; } 
	  if(m==21 && mm == 19) {     tw1n[m][mm]= 0.041151; } 
	  if(m==21 && mm == 24) {     tw1n[m][mm]= 0.696424; } 
	  if(m==21 && mm == 30) {     tw1n[m][mm]= 0.420533; } 
	  if(m==21 && mm == 34) {     tw1n[m][mm]= 0.192015; } 
	  if(m==21 && mm == 35) {     tw1n[m][mm]= 0.099514; } 
	  if(m==21 && mm == 1) {     tw2n[m][mm]= 0.093676; } 
	  if(m==21 && mm == 4) {     tw2n[m][mm]= 0.001358; } 
	  if(m==21 && mm == 7) {     tw2n[m][mm]= 0.334843; } 
	  if(m==21 && mm == 8) {     tw2n[m][mm]= 0.081331; } 
	  if(m==21 && mm == 16) {     tw2n[m][mm]= 0.140595; } 
	  if(m==21 && mm == 17) {     tw2n[m][mm]= 0.004230; } 
	  if(m==21 && mm == 18) {     tw2n[m][mm]= 0.123521; } 
	  if(m==21 && mm == 19) {     tw2n[m][mm]= 0.090849; } 
	  if(m==21 && mm == 23) {     tw2n[m][mm]= 0.231674; } 
	  if(m==21 && mm == 28) {     tw2n[m][mm]= 0.257266; } 
	  if(m==21 && mm == 29) {     tw2n[m][mm]= 0.115733; } 
	  if(m==21 && mm == 32) {     tw2n[m][mm]= 0.273442; } 
	  if(m==21 && mm == 33) {     tw2n[m][mm]= 0.008316; } 
	  if(m==21 && mm == 34) {     tw2n[m][mm]= 0.248525; } 
	  if(m==21 && mm == 35) {     tw2n[m][mm]= 0.271876; } 
	  if(m==21 && mm == 0) {     tw3n[m][mm]= 0.498852; } 
	  if(m==21 && mm == 2) {     tw3n[m][mm]= 0.572117; } 
	  if(m==21 && mm == 3) {     tw3n[m][mm]= 0.351271; } 
	  if(m==21 && mm == 4) {     tw3n[m][mm]= 0.369989; } 
	  if(m==21 && mm == 5) {     tw3n[m][mm]= 0.213708; } 
	  if(m==21 && mm == 6) {     tw3n[m][mm]= 0.265986; } 
	  if(m==21 && mm == 7) {     tw3n[m][mm]= 0.315089; } 
	  if(m==21 && mm == 8) {     tw3n[m][mm]= 0.283484; } 
	  if(m==21 && mm == 10) {     tw3n[m][mm]= 0.495154; } 
	  if(m==21 && mm == 20) {     tw3n[m][mm]= 0.214065; } 
	  if(m==21 && mm == 21) {     tw3n[m][mm]= 0.168527; } 
	  if(m==21 && mm == 23) {     tw3n[m][mm]= 0.162131; } 
	  if(m==21 && mm == 8) {     te2n[m][mm]= 0.124042; } 
	  if(m==21 && mm == 11) {     te2n[m][mm]= 0.133825; } 
	  if(m==21 && mm == 14) {     te2n[m][mm]= 0.242819; } 
	  if(m==21 && mm == 22) {     te2n[m][mm]= 0.348654; } 
	  if(m==21 && mm == 25) {     te2n[m][mm]= 0.169870; } 
	  if(m==21 && mm == 26) {     te2n[m][mm]= 0.323042; } 
	  if(m==21 && mm == 28) {     te2n[m][mm]= 0.276730; } 
	  if(m==21 && mm == 30) {     te2n[m][mm]= 0.025011; } 
	  if(m==21 && mm == 32) {     te2n[m][mm]= 0.368794; } 
	  if(m==21 && mm == 34) {     te2n[m][mm]= 0.052046; } 
	  if(m==21 && mm == 35) {     te2n[m][mm]= 0.728144; } 
	  if(m==21 && mm == 4) {     te3n[m][mm]= 0.040109; } 
	  if(m==21 && mm == 14) {     te3n[m][mm]= 0.129316; } 
	  if(m==21 && mm == 15) {     te3n[m][mm]= 0.118210; } 
	  if(m==21 && mm == 17) {     te3n[m][mm]= 0.096053; } 
	  if(m==21 && mm == 18) {     te3n[m][mm]= 0.020986; } 
	  if(m==21 && mm == 34) {     te3n[m][mm]= 0.025901; } 
	  if(m==21 && mm == 35) {     te3n[m][mm]= 0.286795; } 
	  if(m==22 && mm == 10) {     tw0p[m][mm]= 0.209575; } 
	  if(m==22 && mm == 12) {     tw0p[m][mm]= 0.264401; } 
	  if(m==22 && mm == 13) {     tw0p[m][mm]= 0.157453; } 
	  if(m==22 && mm == 14) {     tw0p[m][mm]= 0.007266; } 
	  if(m==22 && mm == 16) {     tw0p[m][mm]= 0.596505; } 
	  if(m==22 && mm == 17) {     tw0p[m][mm]= 0.073287; } 
	  if(m==22 && mm == 18) {     tw0p[m][mm]= 0.320423; } 
	  if(m==22 && mm == 21) {     tw0p[m][mm]= 0.148792; } 
	  if(m==22 && mm == 22) {     tw0p[m][mm]= 0.181956; } 
	  if(m==22 && mm == 23) {     tw0p[m][mm]= 0.057053; } 
	  if(m==22 && mm == 26) {     tw0p[m][mm]= 0.284406; } 
	  if(m==22 && mm == 28) {     tw0p[m][mm]= 0.195000; } 
	  if(m==22 && mm == 29) {     tw0p[m][mm]= 0.028820; } 
	  if(m==22 && mm == 30) {     tw0p[m][mm]= 0.055109; } 
	  if(m==22 && mm == 31) {     tw0p[m][mm]= 0.357749; } 
	  if(m==22 && mm == 32) {     tw0p[m][mm]= 0.822101; } 
	  if(m==22 && mm == 33) {     tw0p[m][mm]= 0.382272; } 
	  if(m==22 && mm == 34) {     tw0p[m][mm]= 0.107722; } 
	  if(m==22 && mm == 35) {     tw0p[m][mm]= 0.085743; } 
	  if(m==22 && mm == 20) {     tw1p[m][mm]= 0.688118; } 
	  if(m==22 && mm == 22) {     tw1p[m][mm]= 0.424144; } 
	  if(m==22 && mm == 23) {     tw1p[m][mm]= 0.355729; } 
	  if(m==22 && mm == 24) {     tw1p[m][mm]= 0.680312; } 
	  if(m==22 && mm == 25) {     tw1p[m][mm]= 0.547900; } 
	  if(m==22 && mm == 26) {     tw1p[m][mm]= 0.728013; } 
	  if(m==22 && mm == 27) {     tw1p[m][mm]= 0.287588; } 
	  if(m==22 && mm == 28) {     tw1p[m][mm]= 0.074444; } 
	  if(m==22 && mm == 29) {     tw1p[m][mm]= 0.539768; } 
	  if(m==22 && mm == 30) {     tw1p[m][mm]= 0.381698; } 
	  if(m==22 && mm == 31) {     tw1p[m][mm]= 0.511372; } 
	  if(m==22 && mm == 33) {     tw1p[m][mm]= 0.084403; } 
	  if(m==22 && mm == 34) {     tw1p[m][mm]= 0.401008; } 
	  if(m==22 && mm == 35) {     tw1p[m][mm]= 0.428761; } 
	  if(m==22 && mm == 6) {     tw2p[m][mm]= 0.085708; } 
	  if(m==22 && mm == 7) {     tw2p[m][mm]= 0.006752; } 
	  if(m==22 && mm == 8) {     tw2p[m][mm]= 0.356114; } 
	  if(m==22 && mm == 9) {     tw2p[m][mm]= 0.113227; } 
	  if(m==22 && mm == 10) {     tw2p[m][mm]= 0.189432; } 
	  if(m==22 && mm == 11) {     tw2p[m][mm]= 0.031062; } 
	  if(m==22 && mm == 13) {     tw2p[m][mm]= 0.191654; } 
	  if(m==22 && mm == 18) {     tw2p[m][mm]= 0.072393; } 
	  if(m==22 && mm == 30) {     tw2p[m][mm]= 0.184799; } 
	  if(m==22 && mm == 32) {     tw2p[m][mm]= 0.289518; } 
	  if(m==22 && mm == 33) {     tw2p[m][mm]= 0.473010; } 
	  if(m==22 && mm == 34) {     tw2p[m][mm]= 0.542319; } 
	  if(m==22 && mm == 0) {     tw3p[m][mm]= 0.537181; } 
	  if(m==22 && mm == 1) {     tw3p[m][mm]= 0.457930; } 
	  if(m==22 && mm == 3) {     tw3p[m][mm]= 0.242307; } 
	  if(m==22 && mm == 4) {     tw3p[m][mm]= 0.683396; } 
	  if(m==22 && mm == 5) {     tw3p[m][mm]= 0.180848; } 
	  if(m==22 && mm == 7) {     tw3p[m][mm]= 0.091329; } 
	  if(m==22 && mm == 9) {     tw3p[m][mm]= 0.471131; } 
	  if(m==22 && mm == 12) {     tw3p[m][mm]= 0.134809; } 
	  if(m==22 && mm == 13) {     tw3p[m][mm]= 0.241562; } 
	  if(m==22 && mm == 20) {     tw3p[m][mm]= 0.274551; } 
	  if(m==22 && mm == 22) {     tw3p[m][mm]= 0.369762; } 
	  if(m==22 && mm == 23) {     tw3p[m][mm]= 0.375102; } 
	  if(m==22 && mm == 25) {     tw3p[m][mm]= 0.304854; } 
	  if(m==22 && mm == 26) {     tw3p[m][mm]= 0.010608; } 
	  if(m==22 && mm == 30) {     tw3p[m][mm]= 1.372952; } 
	  if(m==22 && mm == 0) {     te2p[m][mm]= 0.345633; } 
	  if(m==22 && mm == 1) {     te2p[m][mm]= 0.420641; } 
	  if(m==22 && mm == 2) {     te2p[m][mm]= 0.627815; } 
	  if(m==22 && mm == 4) {     te2p[m][mm]= 0.043832; } 
	  if(m==22 && mm == 5) {     te2p[m][mm]= 0.041457; } 
	  if(m==22 && mm == 6) {     te2p[m][mm]= 0.171654; } 
	  if(m==22 && mm == 7) {     te2p[m][mm]= 0.005969; } 
	  if(m==22 && mm == 8) {     te2p[m][mm]= 0.567834; } 
	  if(m==22 && mm == 9) {     te2p[m][mm]= 0.415760; } 
	  if(m==22 && mm == 10) {     te2p[m][mm]= 0.274657; } 
	  if(m==22 && mm == 17) {     te2p[m][mm]= 0.203232; } 
	  if(m==22 && mm == 18) {     te2p[m][mm]= 0.150134; } 
	  if(m==22 && mm == 20) {     te2p[m][mm]= 0.006034; } 
	  if(m==22 && mm == 22) {     te2p[m][mm]= 0.040419; } 
	  if(m==22 && mm == 23) {     te2p[m][mm]= 0.259242; } 
	  if(m==22 && mm == 3) {     te3p[m][mm]= 0.441822; } 
	  if(m==22 && mm == 4) {     te3p[m][mm]= 0.229823; } 
	  if(m==22 && mm == 6) {     te3p[m][mm]= 0.952483; } 
	  if(m==22 && mm == 8) {     te3p[m][mm]= 0.100229; } 
	  if(m==22 && mm == 9) {     te3p[m][mm]= 0.369816; } 
	  if(m==22 && mm == 11) {     te3p[m][mm]= 0.030893; } 
	  if(m==22 && mm == 13) {     te3p[m][mm]= 0.715766; } 
	  if(m==22 && mm == 15) {     te3p[m][mm]= 0.162002; } 
	  if(m==22 && mm == 19) {     te3p[m][mm]= 0.061305; } 
	  if(m==22 && mm == 20) {     te3p[m][mm]= 0.163798; } 
	  if(m==22 && mm == 21) {     te3p[m][mm]= 0.756297; } 
	  if(m==22 && mm == 0) {     tw0n[m][mm]= 0.045917; } 
	  if(m==22 && mm == 1) {     tw0n[m][mm]= 0.255939; } 
	  if(m==22 && mm == 4) {     tw0n[m][mm]= 0.282184; } 
	  if(m==22 && mm == 5) {     tw0n[m][mm]= 0.364957; } 
	  if(m==22 && mm == 6) {     tw0n[m][mm]= 0.090071; } 
	  if(m==22 && mm == 10) {     tw0n[m][mm]= 0.002888; } 
	  if(m==22 && mm == 12) {     tw0n[m][mm]= 0.336726; } 
	  if(m==22 && mm == 13) {     tw0n[m][mm]= 0.330440; } 
	  if(m==22 && mm == 14) {     tw0n[m][mm]= 0.064586; } 
	  if(m==22 && mm == 18) {     tw0n[m][mm]= 0.257374; } 
	  if(m==22 && mm == 21) {     tw0n[m][mm]= 0.074530; } 
	  if(m==22 && mm == 22) {     tw0n[m][mm]= 0.224367; } 
	  if(m==22 && mm == 23) {     tw0n[m][mm]= 0.047603; } 
	  if(m==22 && mm == 28) {     tw0n[m][mm]= 0.055912; } 
	  if(m==22 && mm == 9) {     tw1n[m][mm]= 0.243368; } 
	  if(m==22 && mm == 14) {     tw1n[m][mm]= 0.112003; } 
	  if(m==22 && mm == 16) {     tw1n[m][mm]= 0.079234; } 
	  if(m==22 && mm == 17) {     tw1n[m][mm]= 0.657266; } 
	  if(m==22 && mm == 18) {     tw1n[m][mm]= 0.237032; } 
	  if(m==22 && mm == 22) {     tw1n[m][mm]= 0.128868; } 
	  if(m==22 && mm == 23) {     tw1n[m][mm]= 0.048343; } 
	  if(m==22 && mm == 24) {     tw1n[m][mm]= 0.357384; } 
	  if(m==22 && mm == 30) {     tw1n[m][mm]= 0.332833; } 
	  if(m==22 && mm == 31) {     tw1n[m][mm]= 0.001352; } 
	  if(m==22 && mm == 32) {     tw1n[m][mm]= 0.547544; } 
	  if(m==22 && mm == 35) {     tw1n[m][mm]= 0.548959; } 
	  if(m==22 && mm == 0) {     tw2n[m][mm]= 0.026816; } 
	  if(m==22 && mm == 1) {     tw2n[m][mm]= 0.197037; } 
	  if(m==22 && mm == 2) {     tw2n[m][mm]= 0.556237; } 
	  if(m==22 && mm == 3) {     tw2n[m][mm]= 0.236270; } 
	  if(m==22 && mm == 4) {     tw2n[m][mm]= 0.209302; } 
	  if(m==22 && mm == 6) {     tw2n[m][mm]= 0.302500; } 
	  if(m==22 && mm == 7) {     tw2n[m][mm]= 0.040441; } 
	  if(m==22 && mm == 8) {     tw2n[m][mm]= 0.361924; } 
	  if(m==22 && mm == 23) {     tw2n[m][mm]= 0.073783; } 
	  if(m==22 && mm == 28) {     tw2n[m][mm]= 0.085283; } 
	  if(m==22 && mm == 33) {     tw2n[m][mm]= 0.019408; } 
	  if(m==22 && mm == 34) {     tw2n[m][mm]= 0.167564; } 
	  if(m==22 && mm == 0) {     tw3n[m][mm]= 0.463795; } 
	  if(m==22 && mm == 1) {     tw3n[m][mm]= 0.656535; } 
	  if(m==22 && mm == 2) {     tw3n[m][mm]= 0.068737; } 
	  if(m==22 && mm == 3) {     tw3n[m][mm]= 0.414604; } 
	  if(m==22 && mm == 4) {     tw3n[m][mm]= 0.599017; } 
	  if(m==22 && mm == 5) {     tw3n[m][mm]= 0.246518; } 
	  if(m==22 && mm == 7) {     tw3n[m][mm]= 0.098782; } 
	  if(m==22 && mm == 9) {     tw3n[m][mm]= 0.361461; } 
	  if(m==22 && mm == 13) {     tw3n[m][mm]= 0.068939; } 
	  if(m==22 && mm == 20) {     tw3n[m][mm]= 0.143633; } 
	  if(m==22 && mm == 22) {     tw3n[m][mm]= 0.215963; } 
	  if(m==22 && mm == 23) {     tw3n[m][mm]= 0.187653; } 
	  if(m==22 && mm == 4) {     te2n[m][mm]= 0.156108; } 
	  if(m==22 && mm == 9) {     te2n[m][mm]= 0.156515; } 
	  if(m==22 && mm == 12) {     te2n[m][mm]= 0.168772; } 
	  if(m==22 && mm == 14) {     te2n[m][mm]= 0.402304; } 
	  if(m==22 && mm == 15) {     te2n[m][mm]= 0.591671; } 
	  if(m==22 && mm == 17) {     te2n[m][mm]= 0.101483; } 
	  if(m==22 && mm == 23) {     te2n[m][mm]= 0.342956; } 
	  if(m==22 && mm == 30) {     te2n[m][mm]= 0.304975; } 
	  if(m==22 && mm == 32) {     te2n[m][mm]= 0.185561; } 
	  if(m==22 && mm == 33) {     te2n[m][mm]= 0.656995; } 
	  if(m==22 && mm == 0) {     te3n[m][mm]= 0.723670; } 
	  if(m==22 && mm == 1) {     te3n[m][mm]= 1.512289; } 
	  if(m==22 && mm == 6) {     te3n[m][mm]= 0.453586; } 
	  if(m==22 && mm == 16) {     te3n[m][mm]= 0.234589; } 
	  if(m==22 && mm == 25) {     te3n[m][mm]= 0.157272; } 
	  if(m==22 && mm == 31) {     te3n[m][mm]= 0.058600; } 
	  if(m==22 && mm == 32) {     te3n[m][mm]= 0.080655; } 
	  if(m==22 && mm == 34) {     te3n[m][mm]= 0.023481; } 
	  if(m==23 && mm == 9) {     tw0p[m][mm]= 0.089739; } 
	  if(m==23 && mm == 10) {     tw0p[m][mm]= 0.058826; } 
	  if(m==23 && mm == 12) {     tw0p[m][mm]= 0.327160; } 
	  if(m==23 && mm == 13) {     tw0p[m][mm]= 0.109709; } 
	  if(m==23 && mm == 15) {     tw0p[m][mm]= 0.050481; } 
	  if(m==23 && mm == 18) {     tw0p[m][mm]= 0.092979; } 
	  if(m==23 && mm == 19) {     tw0p[m][mm]= 0.112425; } 
	  if(m==23 && mm == 20) {     tw0p[m][mm]= 0.351806; } 
	  if(m==23 && mm == 21) {     tw0p[m][mm]= 0.179823; } 
	  if(m==23 && mm == 22) {     tw0p[m][mm]= 0.139914; } 
	  if(m==23 && mm == 23) {     tw0p[m][mm]= 0.136465; } 
	  if(m==23 && mm == 26) {     tw0p[m][mm]= 0.021728; } 
	  if(m==23 && mm == 27) {     tw0p[m][mm]= 0.024100; } 
	  if(m==23 && mm == 28) {     tw0p[m][mm]= 0.155217; } 
	  if(m==23 && mm == 29) {     tw0p[m][mm]= 0.088097; } 
	  if(m==23 && mm == 30) {     tw0p[m][mm]= 0.396977; } 
	  if(m==23 && mm == 31) {     tw0p[m][mm]= 0.741343; } 
	  if(m==23 && mm == 32) {     tw0p[m][mm]= 0.563644; } 
	  if(m==23 && mm == 33) {     tw0p[m][mm]= 0.190244; } 
	  if(m==23 && mm == 6) {     tw1p[m][mm]= 0.087476; } 
	  if(m==23 && mm == 21) {     tw1p[m][mm]= 0.422558; } 
	  if(m==23 && mm == 23) {     tw1p[m][mm]= 0.274379; } 
	  if(m==23 && mm == 24) {     tw1p[m][mm]= 0.508935; } 
	  if(m==23 && mm == 25) {     tw1p[m][mm]= 0.472499; } 
	  if(m==23 && mm == 26) {     tw1p[m][mm]= 0.493349; } 
	  if(m==23 && mm == 27) {     tw1p[m][mm]= 0.238254; } 
	  if(m==23 && mm == 28) {     tw1p[m][mm]= 0.144069; } 
	  if(m==23 && mm == 30) {     tw1p[m][mm]= 0.463644; } 
	  if(m==23 && mm == 31) {     tw1p[m][mm]= 0.647876; } 
	  if(m==23 && mm == 32) {     tw1p[m][mm]= 0.702276; } 
	  if(m==23 && mm == 33) {     tw1p[m][mm]= 0.161903; } 
	  if(m==23 && mm == 34) {     tw1p[m][mm]= 0.533093; } 
	  if(m==23 && mm == 35) {     tw1p[m][mm]= 0.748518; } 
	  if(m==23 && mm == 4) {     tw2p[m][mm]= 0.377743; } 
	  if(m==23 && mm == 5) {     tw2p[m][mm]= 0.511626; } 
	  if(m==23 && mm == 6) {     tw2p[m][mm]= 0.228714; } 
	  if(m==23 && mm == 7) {     tw2p[m][mm]= 0.059229; } 
	  if(m==23 && mm == 8) {     tw2p[m][mm]= 0.071152; } 
	  if(m==23 && mm == 10) {     tw2p[m][mm]= 0.015253; } 
	  if(m==23 && mm == 11) {     tw2p[m][mm]= 0.519784; } 
	  if(m==23 && mm == 12) {     tw2p[m][mm]= 0.009565; } 
	  if(m==23 && mm == 13) {     tw2p[m][mm]= 0.304838; } 
	  if(m==23 && mm == 18) {     tw2p[m][mm]= 0.146419; } 
	  if(m==23 && mm == 20) {     tw2p[m][mm]= 0.502733; } 
	  if(m==23 && mm == 22) {     tw2p[m][mm]= 0.090831; } 
	  if(m==23 && mm == 28) {     tw2p[m][mm]= 0.180943; } 
	  if(m==23 && mm == 29) {     tw2p[m][mm]= 0.108775; } 
	  if(m==23 && mm == 30) {     tw2p[m][mm]= 0.208247; } 
	  if(m==23 && mm == 32) {     tw2p[m][mm]= 0.627023; } 
	  if(m==23 && mm == 33) {     tw2p[m][mm]= 0.245932; } 
	  if(m==23 && mm == 34) {     tw2p[m][mm]= 0.244467; } 
	  if(m==23 && mm == 35) {     tw2p[m][mm]= 0.236041; } 
	  if(m==23 && mm == 0) {     tw3p[m][mm]= 0.634263; } 
	  if(m==23 && mm == 1) {     tw3p[m][mm]= 0.622155; } 
	  if(m==23 && mm == 2) {     tw3p[m][mm]= 0.328172; } 
	  if(m==23 && mm == 3) {     tw3p[m][mm]= 0.469720; } 
	  if(m==23 && mm == 4) {     tw3p[m][mm]= 0.454626; } 
	  if(m==23 && mm == 5) {     tw3p[m][mm]= 0.749558; } 
	  if(m==23 && mm == 8) {     tw3p[m][mm]= 0.144844; } 
	  if(m==23 && mm == 12) {     tw3p[m][mm]= 0.035580; } 
	  if(m==23 && mm == 13) {     tw3p[m][mm]= 0.068440; } 
	  if(m==23 && mm == 20) {     tw3p[m][mm]= 0.223461; } 
	  if(m==23 && mm == 22) {     tw3p[m][mm]= 0.496761; } 
	  if(m==23 && mm == 23) {     tw3p[m][mm]= 0.274860; } 
	  if(m==23 && mm == 26) {     tw3p[m][mm]= 0.099511; } 
	  if(m==23 && mm == 28) {     tw3p[m][mm]= 0.016981; } 
	  if(m==23 && mm == 31) {     tw3p[m][mm]= 0.335903; } 
	  if(m==23 && mm == 0) {     te2p[m][mm]= 0.123108; } 
	  if(m==23 && mm == 1) {     te2p[m][mm]= 0.199452; } 
	  if(m==23 && mm == 6) {     te2p[m][mm]= 0.163302; } 
	  if(m==23 && mm == 8) {     te2p[m][mm]= 0.260568; } 
	  if(m==23 && mm == 11) {     te2p[m][mm]= 0.055270; } 
	  if(m==23 && mm == 12) {     te2p[m][mm]= 0.131230; } 
	  if(m==23 && mm == 13) {     te2p[m][mm]= 0.956912; } 
	  if(m==23 && mm == 14) {     te2p[m][mm]= 0.166893; } 
	  if(m==23 && mm == 15) {     te2p[m][mm]= 0.337327; } 
	  if(m==23 && mm == 16) {     te2p[m][mm]= 0.017501; } 
	  if(m==23 && mm == 18) {     te2p[m][mm]= 0.308250; } 
	  if(m==23 && mm == 19) {     te2p[m][mm]= 0.353390; } 
	  if(m==23 && mm == 20) {     te2p[m][mm]= 0.084078; } 
	  if(m==23 && mm == 21) {     te2p[m][mm]= 0.107097; } 
	  if(m==23 && mm == 23) {     te2p[m][mm]= 0.084196; } 
	  if(m==23 && mm == 30) {     te2p[m][mm]= 0.276303; } 
	  if(m==23 && mm == 3) {     te3p[m][mm]= 0.042720; } 
	  if(m==23 && mm == 6) {     te3p[m][mm]= 0.143829; } 
	  if(m==23 && mm == 7) {     te3p[m][mm]= 0.498976; } 
	  if(m==23 && mm == 9) {     te3p[m][mm]= 0.215945; } 
	  if(m==23 && mm == 11) {     te3p[m][mm]= 0.137292; } 
	  if(m==23 && mm == 12) {     te3p[m][mm]= 0.065983; } 
	  if(m==23 && mm == 17) {     te3p[m][mm]= 0.408567; } 
	  if(m==23 && mm == 21) {     te3p[m][mm]= 0.059102; } 
	  if(m==23 && mm == 0) {     tw0n[m][mm]= 0.306482; } 
	  if(m==23 && mm == 2) {     tw0n[m][mm]= 0.094012; } 
	  if(m==23 && mm == 8) {     tw0n[m][mm]= 0.354942; } 
	  if(m==23 && mm == 9) {     tw0n[m][mm]= 0.156193; } 
	  if(m==23 && mm == 14) {     tw0n[m][mm]= 0.999920; } 
	  if(m==23 && mm == 15) {     tw0n[m][mm]= 0.052995; } 
	  if(m==23 && mm == 18) {     tw0n[m][mm]= 0.141591; } 
	  if(m==23 && mm == 19) {     tw0n[m][mm]= 0.023729; } 
	  if(m==23 && mm == 20) {     tw0n[m][mm]= 0.080100; } 
	  if(m==23 && mm == 22) {     tw0n[m][mm]= 0.036201; } 
	  if(m==23 && mm == 26) {     tw0n[m][mm]= 0.131052; } 
	  if(m==23 && mm == 6) {     tw1n[m][mm]= 0.614272; } 
	  if(m==23 && mm == 12) {     tw1n[m][mm]= 0.005329; } 
	  if(m==23 && mm == 15) {     tw1n[m][mm]= 0.197054; } 
	  if(m==23 && mm == 16) {     tw1n[m][mm]= 0.033995; } 
	  if(m==23 && mm == 17) {     tw1n[m][mm]= 0.105944; } 
	  if(m==23 && mm == 18) {     tw1n[m][mm]= 0.087682; } 
	  if(m==23 && mm == 21) {     tw1n[m][mm]= 0.304219; } 
	  if(m==23 && mm == 24) {     tw1n[m][mm]= 0.139208; } 
	  if(m==23 && mm == 30) {     tw1n[m][mm]= 0.083355; } 
	  if(m==23 && mm == 31) {     tw1n[m][mm]= 0.257075; } 
	  if(m==23 && mm == 32) {     tw1n[m][mm]= 0.149959; } 
	  if(m==23 && mm == 34) {     tw1n[m][mm]= 0.137233; } 
	  if(m==23 && mm == 35) {     tw1n[m][mm]= 0.473568; } 
	  if(m==23 && mm == 0) {     tw2n[m][mm]= 0.302773; } 
	  if(m==23 && mm == 1) {     tw2n[m][mm]= 0.717548; } 
	  if(m==23 && mm == 3) {     tw2n[m][mm]= 0.261824; } 
	  if(m==23 && mm == 4) {     tw2n[m][mm]= 0.284221; } 
	  if(m==23 && mm == 5) {     tw2n[m][mm]= 0.584878; } 
	  if(m==23 && mm == 6) {     tw2n[m][mm]= 0.380768; } 
	  if(m==23 && mm == 7) {     tw2n[m][mm]= 0.197907; } 
	  if(m==23 && mm == 8) {     tw2n[m][mm]= 0.299899; } 
	  if(m==23 && mm == 18) {     tw2n[m][mm]= 0.019402; } 
	  if(m==23 && mm == 20) {     tw2n[m][mm]= 0.231853; } 
	  if(m==23 && mm == 21) {     tw2n[m][mm]= 0.345260; } 
	  if(m==23 && mm == 22) {     tw2n[m][mm]= 0.109390; } 
	  if(m==23 && mm == 23) {     tw2n[m][mm]= 0.056893; } 
	  if(m==23 && mm == 28) {     tw2n[m][mm]= 0.084167; } 
	  if(m==23 && mm == 30) {     tw2n[m][mm]= 0.025360; } 
	  if(m==23 && mm == 32) {     tw2n[m][mm]= 0.313225; } 
	  if(m==23 && mm == 34) {     tw2n[m][mm]= 0.018426; } 
	  if(m==23 && mm == 35) {     tw2n[m][mm]= 0.109911; } 
	  if(m==23 && mm == 0) {     tw3n[m][mm]= 1.103766; } 
	  if(m==23 && mm == 1) {     tw3n[m][mm]= 1.126622; } 
	  if(m==23 && mm == 2) {     tw3n[m][mm]= 0.521288; } 
	  if(m==23 && mm == 3) {     tw3n[m][mm]= 0.421710; } 
	  if(m==23 && mm == 4) {     tw3n[m][mm]= 0.305848; } 
	  if(m==23 && mm == 5) {     tw3n[m][mm]= 0.859810; } 
	  if(m==23 && mm == 8) {     tw3n[m][mm]= 0.444025; } 
	  if(m==23 && mm == 12) {     tw3n[m][mm]= 0.209639; } 
	  if(m==23 && mm == 20) {     tw3n[m][mm]= 0.096684; } 
	  if(m==23 && mm == 22) {     tw3n[m][mm]= 0.228784; } 
	  if(m==23 && mm == 23) {     tw3n[m][mm]= 0.145663; } 
	  if(m==23 && mm == 8) {     te2n[m][mm]= 0.148894; } 
	  if(m==23 && mm == 15) {     te2n[m][mm]= 0.028357; } 
	  if(m==23 && mm == 16) {     te2n[m][mm]= 0.092165; } 
	  if(m==23 && mm == 18) {     te2n[m][mm]= 0.060004; } 
	  if(m==23 && mm == 19) {     te2n[m][mm]= 0.272961; } 
	  if(m==23 && mm == 20) {     te2n[m][mm]= 0.031126; } 
	  if(m==23 && mm == 23) {     te2n[m][mm]= 0.116471; } 
	  if(m==23 && mm == 30) {     te2n[m][mm]= 0.193383; } 
	  if(m==23 && mm == 32) {     te2n[m][mm]= 0.164955; } 
	  if(m==23 && mm == 34) {     te2n[m][mm]= 0.223795; } 
	  if(m==23 && mm == 1) {     te3n[m][mm]= 0.087080; } 
	  if(m==23 && mm == 3) {     te3n[m][mm]= 0.131705; } 
	  if(m==23 && mm == 6) {     te3n[m][mm]= 0.065946; } 
	  if(m==23 && mm == 17) {     te3n[m][mm]= 0.371569; } 
	  if(m==23 && mm == 21) {     te3n[m][mm]= 0.459262; } 
	  if(m==23 && mm == 23) {     te3n[m][mm]= 0.532102; } 
	  if(m==23 && mm == 24) {     te3n[m][mm]= 0.076031; } 
	  if(m==23 && mm == 26) {     te3n[m][mm]= 0.316473; } 
	  if(m==23 && mm == 31) {     te3n[m][mm]= 0.024436; } 
	  if(m==24 && mm == 9) {     tw0p[m][mm]= 0.198463; } 
	  if(m==24 && mm == 10) {     tw0p[m][mm]= 0.372308; } 
	  if(m==24 && mm == 12) {     tw0p[m][mm]= 0.174882; } 
	  if(m==24 && mm == 16) {     tw0p[m][mm]= 0.171686; } 
	  if(m==24 && mm == 17) {     tw0p[m][mm]= 0.154536; } 
	  if(m==24 && mm == 18) {     tw0p[m][mm]= 0.348098; } 
	  if(m==24 && mm == 19) {     tw0p[m][mm]= 0.139844; } 
	  if(m==24 && mm == 22) {     tw0p[m][mm]= 0.496584; } 
	  if(m==24 && mm == 23) {     tw0p[m][mm]= 0.150374; } 
	  if(m==24 && mm == 25) {     tw0p[m][mm]= 0.140999; } 
	  if(m==24 && mm == 27) {     tw0p[m][mm]= 0.069086; } 
	  if(m==24 && mm == 28) {     tw0p[m][mm]= 0.271780; } 
	  if(m==24 && mm == 29) {     tw0p[m][mm]= 0.099440; } 
	  if(m==24 && mm == 30) {     tw0p[m][mm]= 0.252326; } 
	  if(m==24 && mm == 31) {     tw0p[m][mm]= 0.346411; } 
	  if(m==24 && mm == 32) {     tw0p[m][mm]= 0.272501; } 
	  if(m==24 && mm == 34) {     tw0p[m][mm]= 0.288696; } 
	  if(m==24 && mm == 0) {     tw1p[m][mm]= 0.002836; } 
	  if(m==24 && mm == 2) {     tw1p[m][mm]= 0.302641; } 
	  if(m==24 && mm == 3) {     tw1p[m][mm]= 0.278416; } 
	  if(m==24 && mm == 4) {     tw1p[m][mm]= 0.235912; } 
	  if(m==24 && mm == 5) {     tw1p[m][mm]= 0.393179; } 
	  if(m==24 && mm == 6) {     tw1p[m][mm]= 0.044388; } 
	  if(m==24 && mm == 19) {     tw1p[m][mm]= 0.295202; } 
	  if(m==24 && mm == 21) {     tw1p[m][mm]= 0.020482; } 
	  if(m==24 && mm == 22) {     tw1p[m][mm]= 0.164997; } 
	  if(m==24 && mm == 23) {     tw1p[m][mm]= 0.119729; } 
	  if(m==24 && mm == 28) {     tw1p[m][mm]= 0.143055; } 
	  if(m==24 && mm == 30) {     tw1p[m][mm]= 0.302460; } 
	  if(m==24 && mm == 32) {     tw1p[m][mm]= 0.218259; } 
	  if(m==24 && mm == 33) {     tw1p[m][mm]= 0.307356; } 
	  if(m==24 && mm == 35) {     tw1p[m][mm]= 0.174849; } 
	  if(m==24 && mm == 5) {     tw2p[m][mm]= 0.214592; } 
	  if(m==24 && mm == 6) {     tw2p[m][mm]= 0.088796; } 
	  if(m==24 && mm == 7) {     tw2p[m][mm]= 0.172949; } 
	  if(m==24 && mm == 8) {     tw2p[m][mm]= 0.405003; } 
	  if(m==24 && mm == 13) {     tw2p[m][mm]= 0.197330; } 
	  if(m==24 && mm == 17) {     tw2p[m][mm]= 0.032207; } 
	  if(m==24 && mm == 30) {     tw2p[m][mm]= 0.515229; } 
	  if(m==24 && mm == 32) {     tw2p[m][mm]= 0.226691; } 
	  if(m==24 && mm == 33) {     tw2p[m][mm]= 0.102028; } 
	  if(m==24 && mm == 34) {     tw2p[m][mm]= 0.358176; } 
	  if(m==24 && mm == 0) {     tw3p[m][mm]= 0.342823; } 
	  if(m==24 && mm == 1) {     tw3p[m][mm]= 0.183691; } 
	  if(m==24 && mm == 2) {     tw3p[m][mm]= 0.163574; } 
	  if(m==24 && mm == 3) {     tw3p[m][mm]= 0.193234; } 
	  if(m==24 && mm == 4) {     tw3p[m][mm]= 0.469252; } 
	  if(m==24 && mm == 5) {     tw3p[m][mm]= 0.127295; } 
	  if(m==24 && mm == 6) {     tw3p[m][mm]= 0.426981; } 
	  if(m==24 && mm == 7) {     tw3p[m][mm]= 0.300298; } 
	  if(m==24 && mm == 8) {     tw3p[m][mm]= 0.384349; } 
	  if(m==24 && mm == 9) {     tw3p[m][mm]= 0.407670; } 
	  if(m==24 && mm == 10) {     tw3p[m][mm]= 0.537649; } 
	  if(m==24 && mm == 11) {     tw3p[m][mm]= 0.505170; } 
	  if(m==24 && mm == 19) {     tw3p[m][mm]= 1.852748; } 
	  if(m==24 && mm == 32) {     tw3p[m][mm]= 0.085580; } 
	  if(m==24 && mm == 34) {     tw3p[m][mm]= 0.846417; } 
	  if(m==24 && mm == 35) {     tw3p[m][mm]= 0.201985; } 
	  if(m==24 && mm == 0) {     te2p[m][mm]= 0.587158; } 
	  if(m==24 && mm == 1) {     te2p[m][mm]= 0.443935; } 
	  if(m==24 && mm == 3) {     te2p[m][mm]= 0.140355; } 
	  if(m==24 && mm == 4) {     te2p[m][mm]= 0.184970; } 
	  if(m==24 && mm == 5) {     te2p[m][mm]= 0.584239; } 
	  if(m==24 && mm == 6) {     te2p[m][mm]= 0.314851; } 
	  if(m==24 && mm == 7) {     te2p[m][mm]= 0.642954; } 
	  if(m==24 && mm == 9) {     te2p[m][mm]= 0.316479; } 
	  if(m==24 && mm == 11) {     te2p[m][mm]= 0.195643; } 
	  if(m==24 && mm == 12) {     te2p[m][mm]= 0.408453; } 
	  if(m==24 && mm == 19) {     te2p[m][mm]= 0.293310; } 
	  if(m==24 && mm == 21) {     te2p[m][mm]= 0.124136; } 
	  if(m==24 && mm == 22) {     te2p[m][mm]= 0.217968; } 
	  if(m==24 && mm == 23) {     te2p[m][mm]= 0.277361; } 
	  if(m==24 && mm == 24) {     te2p[m][mm]= 0.182864; } 
	  if(m==24 && mm == 26) {     te2p[m][mm]= 0.028556; } 
	  if(m==24 && mm == 27) {     te2p[m][mm]= 0.092834; } 
	  if(m==24 && mm == 3) {     te3p[m][mm]= 0.354039; } 
	  if(m==24 && mm == 5) {     te3p[m][mm]= 0.046229; } 
	  if(m==24 && mm == 6) {     te3p[m][mm]= 0.150868; } 
	  if(m==24 && mm == 8) {     te3p[m][mm]= 0.704544; } 
	  if(m==24 && mm == 11) {     te3p[m][mm]= 0.327382; } 
	  if(m==24 && mm == 15) {     te3p[m][mm]= 0.005372; } 
	  if(m==24 && mm == 16) {     te3p[m][mm]= 0.044757; } 
	  if(m==24 && mm == 20) {     te3p[m][mm]= 0.254285; } 
	  if(m==24 && mm == 21) {     te3p[m][mm]= 0.365150; } 
	  if(m==24 && mm == 22) {     te3p[m][mm]= 0.178406; } 
	  if(m==24 && mm == 0) {     tw0n[m][mm]= 0.210507; } 
	  if(m==24 && mm == 3) {     tw0n[m][mm]= 0.436109; } 
	  if(m==24 && mm == 4) {     tw0n[m][mm]= 0.073086; } 
	  if(m==24 && mm == 5) {     tw0n[m][mm]= 0.212308; } 
	  if(m==24 && mm == 10) {     tw0n[m][mm]= 0.504200; } 
	  if(m==24 && mm == 16) {     tw0n[m][mm]= 0.122380; } 
	  if(m==24 && mm == 17) {     tw0n[m][mm]= 0.144256; } 
	  if(m==24 && mm == 18) {     tw0n[m][mm]= 0.117047; } 
	  if(m==24 && mm == 22) {     tw0n[m][mm]= 0.186977; } 
	  if(m==24 && mm == 25) {     tw0n[m][mm]= 0.067929; } 
	  if(m==24 && mm == 30) {     tw0n[m][mm]= 0.003310; } 
	  if(m==24 && mm == 12) {     tw1n[m][mm]= 0.227754; } 
	  if(m==24 && mm == 19) {     tw1n[m][mm]= 0.168631; } 
	  if(m==24 && mm == 22) {     tw1n[m][mm]= 0.057840; } 
	  if(m==24 && mm == 33) {     tw1n[m][mm]= 0.223090; } 
	  if(m==24 && mm == 34) {     tw1n[m][mm]= 0.346207; } 
	  if(m==24 && mm == 1) {     tw2n[m][mm]= 0.158359; } 
	  if(m==24 && mm == 3) {     tw2n[m][mm]= 0.131543; } 
	  if(m==24 && mm == 5) {     tw2n[m][mm]= 0.100600; } 
	  if(m==24 && mm == 23) {     tw2n[m][mm]= 0.067296; } 
	  if(m==24 && mm == 30) {     tw2n[m][mm]= 0.227637; } 
	  if(m==24 && mm == 35) {     tw2n[m][mm]= 0.094651; } 
	  if(m==24 && mm == 0) {     tw3n[m][mm]= 0.313005; } 
	  if(m==24 && mm == 1) {     tw3n[m][mm]= 0.432333; } 
	  if(m==24 && mm == 2) {     tw3n[m][mm]= 0.165708; } 
	  if(m==24 && mm == 3) {     tw3n[m][mm]= 0.248665; } 
	  if(m==24 && mm == 4) {     tw3n[m][mm]= 0.514876; } 
	  if(m==24 && mm == 5) {     tw3n[m][mm]= 0.213972; } 
	  if(m==24 && mm == 7) {     tw3n[m][mm]= 0.355807; } 
	  if(m==24 && mm == 8) {     tw3n[m][mm]= 0.316997; } 
	  if(m==24 && mm == 9) {     tw3n[m][mm]= 0.455803; } 
	  if(m==24 && mm == 10) {     tw3n[m][mm]= 0.379160; } 
	  if(m==24 && mm == 11) {     tw3n[m][mm]= 0.145210; } 
	  if(m==24 && mm == 5) {     te2n[m][mm]= 0.296192; } 
	  if(m==24 && mm == 7) {     te2n[m][mm]= 0.160008; } 
	  if(m==24 && mm == 11) {     te2n[m][mm]= 0.210644; } 
	  if(m==24 && mm == 20) {     te2n[m][mm]= 0.351789; } 
	  if(m==24 && mm == 31) {     te2n[m][mm]= 0.086357; } 
	  if(m==24 && mm == 32) {     te2n[m][mm]= 0.453458; } 
	  if(m==24 && mm == 33) {     te2n[m][mm]= 0.219428; } 
	  if(m==24 && mm == 34) {     te2n[m][mm]= 0.604220; } 
	  if(m==24 && mm == 35) {     te2n[m][mm]= 0.043264; } 
	  if(m==24 && mm == 0) {     te3n[m][mm]= 0.038235; } 
	  if(m==24 && mm == 23) {     te3n[m][mm]= 0.135040; } 
	  if(m==24 && mm == 33) {     te3n[m][mm]= 0.149725; } 
	  if(m==25 && mm == 8) {     tw0p[m][mm]= 0.231520; } 
	  if(m==25 && mm == 9) {     tw0p[m][mm]= 0.014793; } 
	  if(m==25 && mm == 13) {     tw0p[m][mm]= 0.338557; } 
	  if(m==25 && mm == 15) {     tw0p[m][mm]= 0.061237; } 
	  if(m==25 && mm == 17) {     tw0p[m][mm]= 0.111614; } 
	  if(m==25 && mm == 18) {     tw0p[m][mm]= 0.406128; } 
	  if(m==25 && mm == 22) {     tw0p[m][mm]= 0.069317; } 
	  if(m==25 && mm == 23) {     tw0p[m][mm]= 0.048941; } 
	  if(m==25 && mm == 24) {     tw0p[m][mm]= 0.154145; } 
	  if(m==25 && mm == 25) {     tw0p[m][mm]= 0.383212; } 
	  if(m==25 && mm == 27) {     tw0p[m][mm]= 0.465148; } 
	  if(m==25 && mm == 28) {     tw0p[m][mm]= 0.083368; } 
	  if(m==25 && mm == 29) {     tw0p[m][mm]= 0.171390; } 
	  if(m==25 && mm == 31) {     tw0p[m][mm]= 0.629555; } 
	  if(m==25 && mm == 34) {     tw0p[m][mm]= 0.453284; } 
	  if(m==25 && mm == 35) {     tw0p[m][mm]= 0.237762; } 
	  if(m==25 && mm == 0) {     tw1p[m][mm]= 0.346667; } 
	  if(m==25 && mm == 1) {     tw1p[m][mm]= 0.167248; } 
	  if(m==25 && mm == 2) {     tw1p[m][mm]= 0.101523; } 
	  if(m==25 && mm == 4) {     tw1p[m][mm]= 0.104032; } 
	  if(m==25 && mm == 5) {     tw1p[m][mm]= 0.234325; } 
	  if(m==25 && mm == 6) {     tw1p[m][mm]= 0.202883; } 
	  if(m==25 && mm == 20) {     tw1p[m][mm]= 0.242592; } 
	  if(m==25 && mm == 21) {     tw1p[m][mm]= 0.022118; } 
	  if(m==25 && mm == 23) {     tw1p[m][mm]= 0.070672; } 
	  if(m==25 && mm == 28) {     tw1p[m][mm]= 0.034448; } 
	  if(m==25 && mm == 31) {     tw1p[m][mm]= 0.426361; } 
	  if(m==25 && mm == 32) {     tw1p[m][mm]= 0.806961; } 
	  if(m==25 && mm == 33) {     tw1p[m][mm]= 1.048357; } 
	  if(m==25 && mm == 34) {     tw1p[m][mm]= 0.450632; } 
	  if(m==25 && mm == 6) {     tw2p[m][mm]= 0.103757; } 
	  if(m==25 && mm == 7) {     tw2p[m][mm]= 0.039816; } 
	  if(m==25 && mm == 9) {     tw2p[m][mm]= 0.133629; } 
	  if(m==25 && mm == 10) {     tw2p[m][mm]= 0.317494; } 
	  if(m==25 && mm == 12) {     tw2p[m][mm]= 0.244830; } 
	  if(m==25 && mm == 16) {     tw2p[m][mm]= 0.123633; } 
	  if(m==25 && mm == 17) {     tw2p[m][mm]= 0.448722; } 
	  if(m==25 && mm == 19) {     tw2p[m][mm]= 0.116118; } 
	  if(m==25 && mm == 23) {     tw2p[m][mm]= 0.260869; } 
	  if(m==25 && mm == 28) {     tw2p[m][mm]= 0.408778; } 
	  if(m==25 && mm == 31) {     tw2p[m][mm]= 0.292374; } 
	  if(m==25 && mm == 32) {     tw2p[m][mm]= 0.171965; } 
	  if(m==25 && mm == 1) {     tw3p[m][mm]= 0.145464; } 
	  if(m==25 && mm == 2) {     tw3p[m][mm]= 0.266781; } 
	  if(m==25 && mm == 3) {     tw3p[m][mm]= 0.001575; } 
	  if(m==25 && mm == 4) {     tw3p[m][mm]= 0.683695; } 
	  if(m==25 && mm == 5) {     tw3p[m][mm]= 0.248712; } 
	  if(m==25 && mm == 6) {     tw3p[m][mm]= 0.236576; } 
	  if(m==25 && mm == 7) {     tw3p[m][mm]= 0.184600; } 
	  if(m==25 && mm == 8) {     tw3p[m][mm]= 0.226778; } 
	  if(m==25 && mm == 9) {     tw3p[m][mm]= 0.537876; } 
	  if(m==25 && mm == 10) {     tw3p[m][mm]= 0.322350; } 
	  if(m==25 && mm == 11) {     tw3p[m][mm]= 0.558522; } 
	  if(m==25 && mm == 32) {     tw3p[m][mm]= 0.772609; } 
	  if(m==25 && mm == 33) {     tw3p[m][mm]= 0.070815; } 
	  if(m==25 && mm == 34) {     tw3p[m][mm]= 0.664733; } 
	  if(m==25 && mm == 35) {     tw3p[m][mm]= 0.307664; } 
	  if(m==25 && mm == 0) {     te2p[m][mm]= 0.258542; } 
	  if(m==25 && mm == 3) {     te2p[m][mm]= 0.067214; } 
	  if(m==25 && mm == 4) {     te2p[m][mm]= 0.385370; } 
	  if(m==25 && mm == 5) {     te2p[m][mm]= 0.389437; } 
	  if(m==25 && mm == 7) {     te2p[m][mm]= 0.238696; } 
	  if(m==25 && mm == 8) {     te2p[m][mm]= 0.511053; } 
	  if(m==25 && mm == 9) {     te2p[m][mm]= 0.566610; } 
	  if(m==25 && mm == 11) {     te2p[m][mm]= 0.243926; } 
	  if(m==25 && mm == 13) {     te2p[m][mm]= 0.048990; } 
	  if(m==25 && mm == 20) {     te2p[m][mm]= 0.068762; } 
	  if(m==25 && mm == 21) {     te2p[m][mm]= 0.271832; } 
	  if(m==25 && mm == 22) {     te2p[m][mm]= 0.365000; } 
	  if(m==25 && mm == 23) {     te2p[m][mm]= 0.547542; } 
	  if(m==25 && mm == 24) {     te2p[m][mm]= 0.194327; } 
	  if(m==25 && mm == 25) {     te2p[m][mm]= 0.167232; } 
	  if(m==25 && mm == 3) {     te3p[m][mm]= 0.271769; } 
	  if(m==25 && mm == 4) {     te3p[m][mm]= 0.303914; } 
	  if(m==25 && mm == 6) {     te3p[m][mm]= 0.163226; } 
	  if(m==25 && mm == 7) {     te3p[m][mm]= 2.038435; } 
	  if(m==25 && mm == 10) {     te3p[m][mm]= 0.261298; } 
	  if(m==25 && mm == 11) {     te3p[m][mm]= 0.290920; } 
	  if(m==25 && mm == 12) {     te3p[m][mm]= 0.047862; } 
	  if(m==25 && mm == 13) {     te3p[m][mm]= 0.024208; } 
	  if(m==25 && mm == 16) {     te3p[m][mm]= 0.146660; } 
	  if(m==25 && mm == 17) {     te3p[m][mm]= 0.018980; } 
	  if(m==25 && mm == 22) {     te3p[m][mm]= 0.234772; } 
	  if(m==25 && mm == 0) {     tw0n[m][mm]= 0.528732; } 
	  if(m==25 && mm == 4) {     tw0n[m][mm]= 0.017180; } 
	  if(m==25 && mm == 5) {     tw0n[m][mm]= 0.180585; } 
	  if(m==25 && mm == 6) {     tw0n[m][mm]= 0.454985; } 
	  if(m==25 && mm == 7) {     tw0n[m][mm]= 0.422668; } 
	  if(m==25 && mm == 14) {     tw0n[m][mm]= 0.105679; } 
	  if(m==25 && mm == 15) {     tw0n[m][mm]= 0.192138; } 
	  if(m==25 && mm == 17) {     tw0n[m][mm]= 0.120886; } 
	  if(m==25 && mm == 18) {     tw0n[m][mm]= 0.276212; } 
	  if(m==25 && mm == 22) {     tw0n[m][mm]= 0.200843; } 
	  if(m==25 && mm == 24) {     tw0n[m][mm]= 0.051764; } 
	  if(m==25 && mm == 25) {     tw0n[m][mm]= 0.140562; } 
	  if(m==25 && mm == 6) {     tw1n[m][mm]= 0.598773; } 
	  if(m==25 && mm == 7) {     tw1n[m][mm]= 0.147437; } 
	  if(m==25 && mm == 8) {     tw1n[m][mm]= 0.122268; } 
	  if(m==25 && mm == 13) {     tw1n[m][mm]= 0.072882; } 
	  if(m==25 && mm == 14) {     tw1n[m][mm]= 0.039233; } 
	  if(m==25 && mm == 18) {     tw1n[m][mm]= 0.211490; } 
	  if(m==25 && mm == 0) {     tw2n[m][mm]= 0.007261; } 
	  if(m==25 && mm == 2) {     tw2n[m][mm]= 0.087995; } 
	  if(m==25 && mm == 3) {     tw2n[m][mm]= 0.204052; } 
	  if(m==25 && mm == 6) {     tw2n[m][mm]= 0.163897; } 
	  if(m==25 && mm == 7) {     tw2n[m][mm]= 0.365293; } 
	  if(m==25 && mm == 8) {     tw2n[m][mm]= 0.041749; } 
	  if(m==25 && mm == 16) {     tw2n[m][mm]= 0.023084; } 
	  if(m==25 && mm == 17) {     tw2n[m][mm]= 0.195535; } 
	  if(m==25 && mm == 23) {     tw2n[m][mm]= 0.155996; } 
	  if(m==25 && mm == 24) {     tw2n[m][mm]= 0.043290; } 
	  if(m==25 && mm == 25) {     tw2n[m][mm]= 0.008993; } 
	  if(m==25 && mm == 32) {     tw2n[m][mm]= 0.048993; } 
	  if(m==25 && mm == 1) {     tw3n[m][mm]= 0.334134; } 
	  if(m==25 && mm == 2) {     tw3n[m][mm]= 0.400916; } 
	  if(m==25 && mm == 3) {     tw3n[m][mm]= 0.124025; } 
	  if(m==25 && mm == 4) {     tw3n[m][mm]= 0.886119; } 
	  if(m==25 && mm == 5) {     tw3n[m][mm]= 0.367934; } 
	  if(m==25 && mm == 6) {     tw3n[m][mm]= 0.288505; } 
	  if(m==25 && mm == 7) {     tw3n[m][mm]= 0.166998; } 
	  if(m==25 && mm == 8) {     tw3n[m][mm]= 0.238042; } 
	  if(m==25 && mm == 9) {     tw3n[m][mm]= 0.261157; } 
	  if(m==25 && mm == 10) {     tw3n[m][mm]= 0.383494; } 
	  if(m==25 && mm == 11) {     tw3n[m][mm]= 0.670012; } 
	  if(m==25 && mm == 8) {     te2n[m][mm]= 0.315100; } 
	  if(m==25 && mm == 9) {     te2n[m][mm]= 0.179721; } 
	  if(m==25 && mm == 20) {     te2n[m][mm]= 0.149862; } 
	  if(m==25 && mm == 21) {     te2n[m][mm]= 0.074257; } 
	  if(m==25 && mm == 22) {     te2n[m][mm]= 0.070395; } 
	  if(m==25 && mm == 23) {     te2n[m][mm]= 0.300273; } 
	  if(m==25 && mm == 24) {     te2n[m][mm]= 0.162930; } 
	  if(m==25 && mm == 25) {     te2n[m][mm]= 0.175360; } 
	  if(m==25 && mm == 26) {     te2n[m][mm]= 0.166649; } 
	  if(m==25 && mm == 29) {     te2n[m][mm]= 0.148146; } 
	  if(m==25 && mm == 30) {     te2n[m][mm]= 0.023005; } 
	  if(m==25 && mm == 4) {     te3n[m][mm]= 0.097793; } 
	  if(m==25 && mm == 22) {     te3n[m][mm]= 0.149671; } 
	  if(m==26 && mm == 9) {     tw0p[m][mm]= 0.150650; } 
	  if(m==26 && mm == 22) {     tw0p[m][mm]= 0.024753; } 
	  if(m==26 && mm == 23) {     tw0p[m][mm]= 0.299848; } 
	  if(m==26 && mm == 24) {     tw0p[m][mm]= 0.095784; } 
	  if(m==26 && mm == 26) {     tw0p[m][mm]= 0.384845; } 
	  if(m==26 && mm == 27) {     tw0p[m][mm]= 0.072478; } 
	  if(m==26 && mm == 28) {     tw0p[m][mm]= 0.296066; } 
	  if(m==26 && mm == 29) {     tw0p[m][mm]= 0.109645; } 
	  if(m==26 && mm == 30) {     tw0p[m][mm]= 0.406214; } 
	  if(m==26 && mm == 31) {     tw0p[m][mm]= 0.531654; } 
	  if(m==26 && mm == 32) {     tw0p[m][mm]= 0.175648; } 
	  if(m==26 && mm == 33) {     tw0p[m][mm]= 0.165465; } 
	  if(m==26 && mm == 34) {     tw0p[m][mm]= 0.232633; } 
	  if(m==26 && mm == 35) {     tw0p[m][mm]= 0.053250; } 
	  if(m==26 && mm == 0) {     tw1p[m][mm]= 0.292327; } 
	  if(m==26 && mm == 2) {     tw1p[m][mm]= 0.010140; } 
	  if(m==26 && mm == 6) {     tw1p[m][mm]= 0.168564; } 
	  if(m==26 && mm == 7) {     tw1p[m][mm]= 0.103342; } 
	  if(m==26 && mm == 23) {     tw1p[m][mm]= 0.109003; } 
	  if(m==26 && mm == 28) {     tw1p[m][mm]= 0.141867; } 
	  if(m==26 && mm == 30) {     tw1p[m][mm]= 0.578660; } 
	  if(m==26 && mm == 31) {     tw1p[m][mm]= 0.097223; } 
	  if(m==26 && mm == 32) {     tw1p[m][mm]= 0.432167; } 
	  if(m==26 && mm == 33) {     tw1p[m][mm]= 0.211158; } 
	  if(m==26 && mm == 34) {     tw1p[m][mm]= 0.075823; } 
	  if(m==26 && mm == 35) {     tw1p[m][mm]= 0.352938; } 
	  if(m==26 && mm == 5) {     tw2p[m][mm]= 0.308260; } 
	  if(m==26 && mm == 7) {     tw2p[m][mm]= 0.160316; } 
	  if(m==26 && mm == 8) {     tw2p[m][mm]= 0.426466; } 
	  if(m==26 && mm == 9) {     tw2p[m][mm]= 0.427880; } 
	  if(m==26 && mm == 12) {     tw2p[m][mm]= 0.078553; } 
	  if(m==26 && mm == 17) {     tw2p[m][mm]= 0.158471; } 
	  if(m==26 && mm == 22) {     tw2p[m][mm]= 0.106098; } 
	  if(m==26 && mm == 1) {     tw3p[m][mm]= 0.452789; } 
	  if(m==26 && mm == 3) {     tw3p[m][mm]= 0.209360; } 
	  if(m==26 && mm == 5) {     tw3p[m][mm]= 0.027465; } 
	  if(m==26 && mm == 6) {     tw3p[m][mm]= 0.486189; } 
	  if(m==26 && mm == 7) {     tw3p[m][mm]= 0.337827; } 
	  if(m==26 && mm == 8) {     tw3p[m][mm]= 0.623014; } 
	  if(m==26 && mm == 9) {     tw3p[m][mm]= 0.068169; } 
	  if(m==26 && mm == 10) {     tw3p[m][mm]= 0.002980; } 
	  if(m==26 && mm == 11) {     tw3p[m][mm]= 0.668380; } 
	  if(m==26 && mm == 12) {     tw3p[m][mm]= 0.405151; } 
	  if(m==26 && mm == 13) {     tw3p[m][mm]= 0.310290; } 
	  if(m==26 && mm == 14) {     tw3p[m][mm]= 0.683036; } 
	  if(m==26 && mm == 15) {     tw3p[m][mm]= 0.357355; } 
	  if(m==26 && mm == 18) {     tw3p[m][mm]= 0.430636; } 
	  if(m==26 && mm == 19) {     tw3p[m][mm]= 0.589493; } 
	  if(m==26 && mm == 20) {     tw3p[m][mm]= 0.314525; } 
	  if(m==26 && mm == 21) {     tw3p[m][mm]= 0.291345; } 
	  if(m==26 && mm == 22) {     tw3p[m][mm]= 0.736717; } 
	  if(m==26 && mm == 23) {     tw3p[m][mm]= 0.398698; } 
	  if(m==26 && mm == 33) {     tw3p[m][mm]= 0.222648; } 
	  if(m==26 && mm == 34) {     tw3p[m][mm]= 0.433503; } 
	  if(m==26 && mm == 35) {     tw3p[m][mm]= 0.315490; } 
	  if(m==26 && mm == 0) {     te2p[m][mm]= 0.246718; } 
	  if(m==26 && mm == 1) {     te2p[m][mm]= 0.285673; } 
	  if(m==26 && mm == 2) {     te2p[m][mm]= 0.255836; } 
	  if(m==26 && mm == 3) {     te2p[m][mm]= 0.798649; } 
	  if(m==26 && mm == 5) {     te2p[m][mm]= 0.315520; } 
	  if(m==26 && mm == 6) {     te2p[m][mm]= 0.110423; } 
	  if(m==26 && mm == 7) {     te2p[m][mm]= 0.317494; } 
	  if(m==26 && mm == 9) {     te2p[m][mm]= 0.247393; } 
	  if(m==26 && mm == 10) {     te2p[m][mm]= 0.150010; } 
	  if(m==26 && mm == 11) {     te2p[m][mm]= 0.026284; } 
	  if(m==26 && mm == 12) {     te2p[m][mm]= 0.254922; } 
	  if(m==26 && mm == 22) {     te2p[m][mm]= 0.219380; } 
	  if(m==26 && mm == 23) {     te2p[m][mm]= 0.273780; } 
	  if(m==26 && mm == 26) {     te2p[m][mm]= 0.272662; } 
	  if(m==26 && mm == 4) {     te3p[m][mm]= 0.092904; } 
	  if(m==26 && mm == 5) {     te3p[m][mm]= 0.212793; } 
	  if(m==26 && mm == 6) {     te3p[m][mm]= 0.094378; } 
	  if(m==26 && mm == 7) {     te3p[m][mm]= 0.090826; } 
	  if(m==26 && mm == 9) {     te3p[m][mm]= 0.420956; } 
	  if(m==26 && mm == 11) {     te3p[m][mm]= 0.032645; } 
	  if(m==26 && mm == 13) {     te3p[m][mm]= 0.161876; } 
	  if(m==26 && mm == 16) {     te3p[m][mm]= 0.105771; } 
	  if(m==26 && mm == 1) {     tw0n[m][mm]= 0.088541; } 
	  if(m==26 && mm == 2) {     tw0n[m][mm]= 0.229043; } 
	  if(m==26 && mm == 3) {     tw0n[m][mm]= 0.256410; } 
	  if(m==26 && mm == 4) {     tw0n[m][mm]= 0.962148; } 
	  if(m==26 && mm == 7) {     tw0n[m][mm]= 0.154014; } 
	  if(m==26 && mm == 9) {     tw0n[m][mm]= 0.550641; } 
	  if(m==26 && mm == 22) {     tw0n[m][mm]= 0.154637; } 
	  if(m==26 && mm == 25) {     tw0n[m][mm]= 0.002398; } 
	  if(m==26 && mm == 26) {     tw0n[m][mm]= 0.018397; } 
	  if(m==26 && mm == 6) {     tw1n[m][mm]= 0.497520; } 
	  if(m==26 && mm == 7) {     tw1n[m][mm]= 0.374192; } 
	  if(m==26 && mm == 8) {     tw1n[m][mm]= 0.319910; } 
	  if(m==26 && mm == 11) {     tw1n[m][mm]= 1.701587; } 
	  if(m==26 && mm == 20) {     tw1n[m][mm]= 0.156499; } 
	  if(m==26 && mm == 30) {     tw1n[m][mm]= 0.182746; } 
	  if(m==26 && mm == 35) {     tw1n[m][mm]= 0.478289; } 
	  if(m==26 && mm == 0) {     tw2n[m][mm]= 0.511809; } 
	  if(m==26 && mm == 1) {     tw2n[m][mm]= 0.106086; } 
	  if(m==26 && mm == 2) {     tw2n[m][mm]= 0.044481; } 
	  if(m==26 && mm == 3) {     tw2n[m][mm]= 0.482530; } 
	  if(m==26 && mm == 4) {     tw2n[m][mm]= 0.226430; } 
	  if(m==26 && mm == 5) {     tw2n[m][mm]= 0.501178; } 
	  if(m==26 && mm == 7) {     tw2n[m][mm]= 0.052698; } 
	  if(m==26 && mm == 15) {     tw2n[m][mm]= 0.005093; } 
	  if(m==26 && mm == 25) {     tw2n[m][mm]= 0.119973; } 
	  if(m==26 && mm == 1) {     tw3n[m][mm]= 0.707339; } 
	  if(m==26 && mm == 3) {     tw3n[m][mm]= 0.382789; } 
	  if(m==26 && mm == 5) {     tw3n[m][mm]= 0.042503; } 
	  if(m==26 && mm == 6) {     tw3n[m][mm]= 0.399746; } 
	  if(m==26 && mm == 7) {     tw3n[m][mm]= 0.271140; } 
	  if(m==26 && mm == 8) {     tw3n[m][mm]= 0.668219; } 
	  if(m==26 && mm == 9) {     tw3n[m][mm]= 0.677812; } 
	  if(m==26 && mm == 10) {     tw3n[m][mm]= 0.043517; } 
	  if(m==26 && mm == 11) {     tw3n[m][mm]= 0.552467; } 
	  if(m==26 && mm == 13) {     tw3n[m][mm]= 0.336510; } 
	  if(m==26 && mm == 14) {     tw3n[m][mm]= 0.495701; } 
	  if(m==26 && mm == 15) {     tw3n[m][mm]= 0.318445; } 
	  if(m==26 && mm == 18) {     tw3n[m][mm]= 0.200738; } 
	  if(m==26 && mm == 19) {     tw3n[m][mm]= 0.133582; } 
	  if(m==26 && mm == 20) {     tw3n[m][mm]= 0.030326; } 
	  if(m==26 && mm == 21) {     tw3n[m][mm]= 0.394929; } 
	  if(m==26 && mm == 22) {     tw3n[m][mm]= 0.391985; } 
	  if(m==26 && mm == 23) {     tw3n[m][mm]= 0.164147; } 
	  if(m==26 && mm == 7) {     te2n[m][mm]= 0.020487; } 
	  if(m==26 && mm == 19) {     te2n[m][mm]= 0.173176; } 
	  if(m==26 && mm == 22) {     te2n[m][mm]= 0.206935; } 
	  if(m==26 && mm == 31) {     te2n[m][mm]= 0.092938; } 
	  if(m==26 && mm == 32) {     te2n[m][mm]= 0.006543; } 
	  if(m==26 && mm == 33) {     te2n[m][mm]= 0.559384; } 
	  if(m==26 && mm == 34) {     te2n[m][mm]= 0.096229; } 
	  if(m==26 && mm == 22) {     te3n[m][mm]= 0.006724; } 
	  if(m==26 && mm == 35) {     te3n[m][mm]= 0.024468; } 
	  if(m==27 && mm == 17) {     tw0p[m][mm]= 0.127613; } 
	  if(m==27 && mm == 18) {     tw0p[m][mm]= 0.108386; } 
	  if(m==27 && mm == 20) {     tw0p[m][mm]= 0.057473; } 
	  if(m==27 && mm == 22) {     tw0p[m][mm]= 0.192578; } 
	  if(m==27 && mm == 23) {     tw0p[m][mm]= 0.168654; } 
	  if(m==27 && mm == 24) {     tw0p[m][mm]= 0.340747; } 
	  if(m==27 && mm == 25) {     tw0p[m][mm]= 0.254811; } 
	  if(m==27 && mm == 26) {     tw0p[m][mm]= 0.413772; } 
	  if(m==27 && mm == 28) {     tw0p[m][mm]= 0.101368; } 
	  if(m==27 && mm == 29) {     tw0p[m][mm]= 0.140725; } 
	  if(m==27 && mm == 30) {     tw0p[m][mm]= 0.571353; } 
	  if(m==27 && mm == 31) {     tw0p[m][mm]= 0.100815; } 
	  if(m==27 && mm == 32) {     tw0p[m][mm]= 0.498635; } 
	  if(m==27 && mm == 33) {     tw0p[m][mm]= 0.359751; } 
	  if(m==27 && mm == 34) {     tw0p[m][mm]= 0.862043; } 
	  if(m==27 && mm == 35) {     tw0p[m][mm]= 0.199711; } 
	  if(m==27 && mm == 0) {     tw1p[m][mm]= 0.493643; } 
	  if(m==27 && mm == 1) {     tw1p[m][mm]= 0.262071; } 
	  if(m==27 && mm == 6) {     tw1p[m][mm]= 0.046707; } 
	  if(m==27 && mm == 20) {     tw1p[m][mm]= 0.093087; } 
	  if(m==27 && mm == 22) {     tw1p[m][mm]= 0.199025; } 
	  if(m==27 && mm == 23) {     tw1p[m][mm]= 0.386513; } 
	  if(m==27 && mm == 28) {     tw1p[m][mm]= 0.145645; } 
	  if(m==27 && mm == 29) {     tw1p[m][mm]= 0.322608; } 
	  if(m==27 && mm == 32) {     tw1p[m][mm]= 0.718098; } 
	  if(m==27 && mm == 33) {     tw1p[m][mm]= 0.217459; } 
	  if(m==27 && mm == 34) {     tw1p[m][mm]= 0.156120; } 
	  if(m==27 && mm == 8) {     tw2p[m][mm]= 0.014382; } 
	  if(m==27 && mm == 11) {     tw2p[m][mm]= 0.098535; } 
	  if(m==27 && mm == 12) {     tw2p[m][mm]= 0.115881; } 
	  if(m==27 && mm == 16) {     tw2p[m][mm]= 0.303063; } 
	  if(m==27 && mm == 17) {     tw2p[m][mm]= 0.090834; } 
	  if(m==27 && mm == 21) {     tw2p[m][mm]= 0.009240; } 
	  if(m==27 && mm == 22) {     tw2p[m][mm]= 0.229062; } 
	  if(m==27 && mm == 1) {     tw3p[m][mm]= 0.094898; } 
	  if(m==27 && mm == 6) {     tw3p[m][mm]= 0.555436; } 
	  if(m==27 && mm == 8) {     tw3p[m][mm]= 0.623343; } 
	  if(m==27 && mm == 9) {     tw3p[m][mm]= 0.419716; } 
	  if(m==27 && mm == 10) {     tw3p[m][mm]= 0.420016; } 
	  if(m==27 && mm == 11) {     tw3p[m][mm]= 0.291827; } 
	  if(m==27 && mm == 12) {     tw3p[m][mm]= 0.473158; } 
	  if(m==27 && mm == 13) {     tw3p[m][mm]= 0.430085; } 
	  if(m==27 && mm == 14) {     tw3p[m][mm]= 0.289642; } 
	  if(m==27 && mm == 19) {     tw3p[m][mm]= 0.447580; } 
	  if(m==27 && mm == 20) {     tw3p[m][mm]= 0.665063; } 
	  if(m==27 && mm == 21) {     tw3p[m][mm]= 0.622856; } 
	  if(m==27 && mm == 22) {     tw3p[m][mm]= 0.854133; } 
	  if(m==27 && mm == 23) {     tw3p[m][mm]= 0.451229; } 
	  if(m==27 && mm == 33) {     tw3p[m][mm]= 0.252450; } 
	  if(m==27 && mm == 34) {     tw3p[m][mm]= 0.197542; } 
	  if(m==27 && mm == 35) {     tw3p[m][mm]= 0.091218; } 
	  if(m==27 && mm == 0) {     te2p[m][mm]= 0.155424; } 
	  if(m==27 && mm == 1) {     te2p[m][mm]= 0.193712; } 
	  if(m==27 && mm == 2) {     te2p[m][mm]= 0.858072; } 
	  if(m==27 && mm == 4) {     te2p[m][mm]= 0.157991; } 
	  if(m==27 && mm == 5) {     te2p[m][mm]= 0.089119; } 
	  if(m==27 && mm == 6) {     te2p[m][mm]= 0.094495; } 
	  if(m==27 && mm == 7) {     te2p[m][mm]= 0.195005; } 
	  if(m==27 && mm == 20) {     te2p[m][mm]= 0.022683; } 
	  if(m==27 && mm == 22) {     te2p[m][mm]= 0.037707; } 
	  if(m==27 && mm == 23) {     te2p[m][mm]= 0.091554; } 
	  if(m==27 && mm == 7) {     te3p[m][mm]= 0.075697; } 
	  if(m==27 && mm == 9) {     te3p[m][mm]= 0.262926; } 
	  if(m==27 && mm == 11) {     te3p[m][mm]= 0.269871; } 
	  if(m==27 && mm == 13) {     te3p[m][mm]= 0.228803; } 
	  if(m==27 && mm == 3) {     tw0n[m][mm]= 0.332200; } 
	  if(m==27 && mm == 5) {     tw0n[m][mm]= 0.386216; } 
	  if(m==27 && mm == 8) {     tw0n[m][mm]= 0.026222; } 
	  if(m==27 && mm == 12) {     tw0n[m][mm]= 0.433952; } 
	  if(m==27 && mm == 20) {     tw0n[m][mm]= 0.024833; } 
	  if(m==27 && mm == 23) {     tw0n[m][mm]= 0.003054; } 
	  if(m==27 && mm == 6) {     tw1n[m][mm]= 0.222378; } 
	  if(m==27 && mm == 18) {     tw1n[m][mm]= 0.218268; } 
	  if(m==27 && mm == 20) {     tw1n[m][mm]= 0.124809; } 
	  if(m==27 && mm == 22) {     tw1n[m][mm]= 0.173198; } 
	  if(m==27 && mm == 0) {     tw2n[m][mm]= 0.232632; } 
	  if(m==27 && mm == 1) {     tw2n[m][mm]= 0.445499; } 
	  if(m==27 && mm == 2) {     tw2n[m][mm]= 0.184304; } 
	  if(m==27 && mm == 4) {     tw2n[m][mm]= 0.673488; } 
	  if(m==27 && mm == 23) {     tw2n[m][mm]= 0.045199; } 
	  if(m==27 && mm == 0) {     tw3n[m][mm]= 0.072524; } 
	  if(m==27 && mm == 1) {     tw3n[m][mm]= 0.131420; } 
	  if(m==27 && mm == 6) {     tw3n[m][mm]= 0.660237; } 
	  if(m==27 && mm == 7) {     tw3n[m][mm]= 0.833478; } 
	  if(m==27 && mm == 8) {     tw3n[m][mm]= 0.762660; } 
	  if(m==27 && mm == 9) {     tw3n[m][mm]= 0.338697; } 
	  if(m==27 && mm == 11) {     tw3n[m][mm]= 0.260498; } 
	  if(m==27 && mm == 12) {     tw3n[m][mm]= 0.234917; } 
	  if(m==27 && mm == 13) {     tw3n[m][mm]= 0.531473; } 
	  if(m==27 && mm == 14) {     tw3n[m][mm]= 0.093693; } 
	  if(m==27 && mm == 19) {     tw3n[m][mm]= 0.129521; } 
	  if(m==27 && mm == 21) {     tw3n[m][mm]= 0.233121; } 
	  if(m==27 && mm == 22) {     tw3n[m][mm]= 0.180211; } 
	  if(m==27 && mm == 23) {     tw3n[m][mm]= 0.232567; } 
	  if(m==27 && mm == 22) {     te2n[m][mm]= 0.050398; } 
	  if(m==27 && mm == 32) {     te2n[m][mm]= 0.157980; } 
	  if(m==27 && mm == 34) {     te2n[m][mm]= 0.308887; } 
	  if(m==27 && mm == 2) {     te3n[m][mm]= 0.092309; } 
	  if(m==28 && mm == 8) {     tw0p[m][mm]= 0.098668; } 
	  if(m==28 && mm == 11) {     tw0p[m][mm]= 0.288845; } 
	  if(m==28 && mm == 18) {     tw0p[m][mm]= 0.116016; } 
	  if(m==28 && mm == 19) {     tw0p[m][mm]= 0.095104; } 
	  if(m==28 && mm == 26) {     tw0p[m][mm]= 0.077397; } 
	  if(m==28 && mm == 27) {     tw0p[m][mm]= 0.108011; } 
	  if(m==28 && mm == 28) {     tw0p[m][mm]= 0.068947; } 
	  if(m==28 && mm == 30) {     tw0p[m][mm]= 0.872129; } 
	  if(m==28 && mm == 31) {     tw0p[m][mm]= 0.991076; } 
	  if(m==28 && mm == 32) {     tw0p[m][mm]= 0.566957; } 
	  if(m==28 && mm == 33) {     tw0p[m][mm]= 0.230897; } 
	  if(m==28 && mm == 34) {     tw0p[m][mm]= 0.231787; } 
	  if(m==28 && mm == 35) {     tw0p[m][mm]= 0.356549; } 
	  if(m==28 && mm == 1) {     tw1p[m][mm]= 0.097719; } 
	  if(m==28 && mm == 5) {     tw1p[m][mm]= 0.110498; } 
	  if(m==28 && mm == 6) {     tw1p[m][mm]= 0.265829; } 
	  if(m==28 && mm == 19) {     tw1p[m][mm]= 0.128307; } 
	  if(m==28 && mm == 20) {     tw1p[m][mm]= 0.244781; } 
	  if(m==28 && mm == 22) {     tw1p[m][mm]= 0.012304; } 
	  if(m==28 && mm == 23) {     tw1p[m][mm]= 0.348111; } 
	  if(m==28 && mm == 28) {     tw1p[m][mm]= 0.150012; } 
	  if(m==28 && mm == 29) {     tw1p[m][mm]= 0.028837; } 
	  if(m==28 && mm == 34) {     tw1p[m][mm]= 0.471343; } 
	  if(m==28 && mm == 35) {     tw1p[m][mm]= 0.415606; } 
	  if(m==28 && mm == 6) {     tw2p[m][mm]= 0.018415; } 
	  if(m==28 && mm == 8) {     tw2p[m][mm]= 0.032515; } 
	  if(m==28 && mm == 9) {     tw2p[m][mm]= 0.289123; } 
	  if(m==28 && mm == 11) {     tw2p[m][mm]= 0.234416; } 
	  if(m==28 && mm == 15) {     tw2p[m][mm]= 0.035458; } 
	  if(m==28 && mm == 16) {     tw2p[m][mm]= 0.069400; } 
	  if(m==28 && mm == 20) {     tw2p[m][mm]= 0.055501; } 
	  if(m==28 && mm == 30) {     tw2p[m][mm]= 0.178640; } 
	  if(m==28 && mm == 31) {     tw2p[m][mm]= 0.616255; } 
	  if(m==28 && mm == 33) {     tw2p[m][mm]= 0.078076; } 
	  if(m==28 && mm == 34) {     tw2p[m][mm]= 0.965386; } 
	  if(m==28 && mm == 35) {     tw2p[m][mm]= 0.181052; } 
	  if(m==28 && mm == 0) {     tw3p[m][mm]= 0.435256; } 
	  if(m==28 && mm == 1) {     tw3p[m][mm]= 0.416482; } 
	  if(m==28 && mm == 2) {     tw3p[m][mm]= 0.213146; } 
	  if(m==28 && mm == 4) {     tw3p[m][mm]= 0.032635; } 
	  if(m==28 && mm == 5) {     tw3p[m][mm]= 0.115452; } 
	  if(m==28 && mm == 6) {     tw3p[m][mm]= 0.194483; } 
	  if(m==28 && mm == 7) {     tw3p[m][mm]= 0.452550; } 
	  if(m==28 && mm == 8) {     tw3p[m][mm]= 0.040734; } 
	  if(m==28 && mm == 9) {     tw3p[m][mm]= 0.251312; } 
	  if(m==28 && mm == 10) {     tw3p[m][mm]= 0.243154; } 
	  if(m==28 && mm == 11) {     tw3p[m][mm]= 0.279456; } 
	  if(m==28 && mm == 12) {     tw3p[m][mm]= 0.415075; } 
	  if(m==28 && mm == 13) {     tw3p[m][mm]= 0.371753; } 
	  if(m==28 && mm == 14) {     tw3p[m][mm]= 0.479055; } 
	  if(m==28 && mm == 15) {     tw3p[m][mm]= 0.840658; } 
	  if(m==28 && mm == 18) {     tw3p[m][mm]= 0.134687; } 
	  if(m==28 && mm == 34) {     tw3p[m][mm]= 0.916738; } 
	  if(m==28 && mm == 35) {     tw3p[m][mm]= 0.766189; } 
	  if(m==28 && mm == 1) {     te2p[m][mm]= 0.274015; } 
	  if(m==28 && mm == 2) {     te2p[m][mm]= 0.824995; } 
	  if(m==28 && mm == 3) {     te2p[m][mm]= 0.268628; } 
	  if(m==28 && mm == 4) {     te2p[m][mm]= 0.325479; } 
	  if(m==28 && mm == 8) {     te2p[m][mm]= 0.017713; } 
	  if(m==28 && mm == 9) {     te2p[m][mm]= 0.061159; } 
	  if(m==28 && mm == 10) {     te2p[m][mm]= 0.001001; } 
	  if(m==28 && mm == 11) {     te2p[m][mm]= 0.139719; } 
	  if(m==28 && mm == 12) {     te2p[m][mm]= 0.271534; } 
	  if(m==28 && mm == 18) {     te2p[m][mm]= 0.187881; } 
	  if(m==28 && mm == 19) {     te2p[m][mm]= 0.069033; } 
	  if(m==28 && mm == 21) {     te2p[m][mm]= 0.053213; } 
	  if(m==28 && mm == 23) {     te2p[m][mm]= 0.516027; } 
	  if(m==28 && mm == 24) {     te2p[m][mm]= 0.413253; } 
	  if(m==28 && mm == 30) {     te2p[m][mm]= 0.143798; } 
	  if(m==28 && mm == 5) {     te3p[m][mm]= 0.623625; } 
	  if(m==28 && mm == 7) {     te3p[m][mm]= 0.249751; } 
	  if(m==28 && mm == 8) {     te3p[m][mm]= 0.326213; } 
	  if(m==28 && mm == 9) {     te3p[m][mm]= 0.223586; } 
	  if(m==28 && mm == 10) {     te3p[m][mm]= 0.415824; } 
	  if(m==28 && mm == 11) {     te3p[m][mm]= 0.048434; } 
	  if(m==28 && mm == 17) {     te3p[m][mm]= 0.127932; } 
	  if(m==28 && mm == 20) {     te3p[m][mm]= 0.119868; } 
	  if(m==28 && mm == 0) {     tw0n[m][mm]= 0.393626; } 
	  if(m==28 && mm == 2) {     tw0n[m][mm]= 0.262836; } 
	  if(m==28 && mm == 3) {     tw0n[m][mm]= 0.101066; } 
	  if(m==28 && mm == 4) {     tw0n[m][mm]= 0.028318; } 
	  if(m==28 && mm == 8) {     tw0n[m][mm]= 0.337278; } 
	  if(m==28 && mm == 9) {     tw0n[m][mm]= 0.061724; } 
	  if(m==28 && mm == 19) {     tw0n[m][mm]= 0.044079; } 
	  if(m==28 && mm == 7) {     tw1n[m][mm]= 0.133012; } 
	  if(m==28 && mm == 10) {     tw1n[m][mm]= 0.063183; } 
	  if(m==28 && mm == 23) {     tw1n[m][mm]= 0.150691; } 
	  if(m==28 && mm == 34) {     tw1n[m][mm]= 0.023288; } 
	  if(m==28 && mm == 2) {     tw2n[m][mm]= 0.459691; } 
	  if(m==28 && mm == 6) {     tw2n[m][mm]= 0.135320; } 
	  if(m==28 && mm == 31) {     tw2n[m][mm]= 0.169050; } 
	  if(m==28 && mm == 35) {     tw2n[m][mm]= 0.169885; } 
	  if(m==28 && mm == 0) {     tw3n[m][mm]= 0.512464; } 
	  if(m==28 && mm == 1) {     tw3n[m][mm]= 0.557534; } 
	  if(m==28 && mm == 2) {     tw3n[m][mm]= 0.480740; } 
	  if(m==28 && mm == 3) {     tw3n[m][mm]= 0.010176; } 
	  if(m==28 && mm == 4) {     tw3n[m][mm]= 0.080141; } 
	  if(m==28 && mm == 5) {     tw3n[m][mm]= 0.261075; } 
	  if(m==28 && mm == 6) {     tw3n[m][mm]= 0.256398; } 
	  if(m==28 && mm == 7) {     tw3n[m][mm]= 0.040920; } 
	  if(m==28 && mm == 8) {     tw3n[m][mm]= 0.000902; } 
	  if(m==28 && mm == 9) {     tw3n[m][mm]= 0.229202; } 
	  if(m==28 && mm == 10) {     tw3n[m][mm]= 0.050027; } 
	  if(m==28 && mm == 12) {     tw3n[m][mm]= 0.243545; } 
	  if(m==28 && mm == 13) {     tw3n[m][mm]= 0.407457; } 
	  if(m==28 && mm == 14) {     tw3n[m][mm]= 0.505809; } 
	  if(m==28 && mm == 21) {     te2n[m][mm]= 0.140410; } 
	  if(m==28 && mm == 28) {     te2n[m][mm]= 0.073188; } 
	  if(m==28 && mm == 30) {     te2n[m][mm]= 0.266338; } 
	  if(m==28 && mm == 32) {     te2n[m][mm]= 0.378592; } 
	  if(m==28 && mm == 35) {     te2n[m][mm]= 0.070543; } 
	  if(m==28 && mm == 2) {     te3n[m][mm]= 0.112054; } 
	  if(m==28 && mm == 22) {     te3n[m][mm]= 0.177718; } 
	  if(m==29 && mm == 8) {     tw0p[m][mm]= 0.075760; } 
	  if(m==29 && mm == 16) {     tw0p[m][mm]= 0.018017; } 
	  if(m==29 && mm == 18) {     tw0p[m][mm]= 0.105221; } 
	  if(m==29 && mm == 19) {     tw0p[m][mm]= 0.048102; } 
	  if(m==29 && mm == 24) {     tw0p[m][mm]= 0.161876; } 
	  if(m==29 && mm == 26) {     tw0p[m][mm]= 0.111679; } 
	  if(m==29 && mm == 29) {     tw0p[m][mm]= 0.420281; } 
	  if(m==29 && mm == 30) {     tw0p[m][mm]= 0.296837; } 
	  if(m==29 && mm == 31) {     tw0p[m][mm]= 0.192197; } 
	  if(m==29 && mm == 32) {     tw0p[m][mm]= 0.449923; } 
	  if(m==29 && mm == 33) {     tw0p[m][mm]= 0.465611; } 
	  if(m==29 && mm == 34) {     tw0p[m][mm]= 0.307696; } 
	  if(m==29 && mm == 3) {     tw1p[m][mm]= 0.039726; } 
	  if(m==29 && mm == 5) {     tw1p[m][mm]= 0.113932; } 
	  if(m==29 && mm == 21) {     tw1p[m][mm]= 0.066160; } 
	  if(m==29 && mm == 29) {     tw1p[m][mm]= 0.603415; } 
	  if(m==29 && mm == 31) {     tw1p[m][mm]= 0.086234; } 
	  if(m==29 && mm == 32) {     tw1p[m][mm]= 0.749626; } 
	  if(m==29 && mm == 33) {     tw1p[m][mm]= 1.927510; } 
	  if(m==29 && mm == 35) {     tw1p[m][mm]= 0.239241; } 
	  if(m==29 && mm == 8) {     tw2p[m][mm]= 0.073109; } 
	  if(m==29 && mm == 9) {     tw2p[m][mm]= 0.411987; } 
	  if(m==29 && mm == 10) {     tw2p[m][mm]= 0.120757; } 
	  if(m==29 && mm == 11) {     tw2p[m][mm]= 0.130380; } 
	  if(m==29 && mm == 16) {     tw2p[m][mm]= 0.067529; } 
	  if(m==29 && mm == 17) {     tw2p[m][mm]= 0.116127; } 
	  if(m==29 && mm == 18) {     tw2p[m][mm]= 0.376212; } 
	  if(m==29 && mm == 19) {     tw2p[m][mm]= 0.174517; } 
	  if(m==29 && mm == 21) {     tw2p[m][mm]= 0.325832; } 
	  if(m==29 && mm == 29) {     tw2p[m][mm]= 0.444796; } 
	  if(m==29 && mm == 30) {     tw2p[m][mm]= 0.316989; } 
	  if(m==29 && mm == 31) {     tw2p[m][mm]= 0.300325; } 
	  if(m==29 && mm == 32) {     tw2p[m][mm]= 0.551766; } 
	  if(m==29 && mm == 33) {     tw2p[m][mm]= 0.574819; } 
	  if(m==29 && mm == 34) {     tw2p[m][mm]= 0.614690; } 
	  if(m==29 && mm == 35) {     tw2p[m][mm]= 0.067057; } 
	  if(m==29 && mm == 0) {     tw3p[m][mm]= 0.424000; } 
	  if(m==29 && mm == 1) {     tw3p[m][mm]= 0.325623; } 
	  if(m==29 && mm == 2) {     tw3p[m][mm]= 0.137597; } 
	  if(m==29 && mm == 3) {     tw3p[m][mm]= 0.221853; } 
	  if(m==29 && mm == 4) {     tw3p[m][mm]= 0.348316; } 
	  if(m==29 && mm == 5) {     tw3p[m][mm]= 0.162702; } 
	  if(m==29 && mm == 6) {     tw3p[m][mm]= 0.538545; } 
	  if(m==29 && mm == 7) {     tw3p[m][mm]= 0.348806; } 
	  if(m==29 && mm == 8) {     tw3p[m][mm]= 0.141554; } 
	  if(m==29 && mm == 9) {     tw3p[m][mm]= 0.141602; } 
	  if(m==29 && mm == 10) {     tw3p[m][mm]= 0.052594; } 
	  if(m==29 && mm == 11) {     tw3p[m][mm]= 0.436087; } 
	  if(m==29 && mm == 12) {     tw3p[m][mm]= 0.121295; } 
	  if(m==29 && mm == 13) {     tw3p[m][mm]= 0.505375; } 
	  if(m==29 && mm == 14) {     tw3p[m][mm]= 0.160105; } 
	  if(m==29 && mm == 15) {     tw3p[m][mm]= 0.478991; } 
	  if(m==29 && mm == 21) {     tw3p[m][mm]= 0.220119; } 
	  if(m==29 && mm == 34) {     tw3p[m][mm]= 1.115454; } 
	  if(m==29 && mm == 35) {     tw3p[m][mm]= 0.415276; } 
	  if(m==29 && mm == 0) {     te2p[m][mm]= 0.198184; } 
	  if(m==29 && mm == 1) {     te2p[m][mm]= 0.140320; } 
	  if(m==29 && mm == 2) {     te2p[m][mm]= 0.181936; } 
	  if(m==29 && mm == 3) {     te2p[m][mm]= 0.296951; } 
	  if(m==29 && mm == 5) {     te2p[m][mm]= 0.110467; } 
	  if(m==29 && mm == 6) {     te2p[m][mm]= 0.089555; } 
	  if(m==29 && mm == 9) {     te2p[m][mm]= 0.143661; } 
	  if(m==29 && mm == 12) {     te2p[m][mm]= 0.288731; } 
	  if(m==29 && mm == 18) {     te2p[m][mm]= 0.097369; } 
	  if(m==29 && mm == 20) {     te2p[m][mm]= 0.021200; } 
	  if(m==29 && mm == 22) {     te2p[m][mm]= 0.182409; } 
	  if(m==29 && mm == 24) {     te2p[m][mm]= 0.037548; } 
	  if(m==29 && mm == 26) {     te2p[m][mm]= 0.406033; } 
	  if(m==29 && mm == 30) {     te2p[m][mm]= 0.020452; } 
	  if(m==29 && mm == 4) {     te3p[m][mm]= 0.418778; } 
	  if(m==29 && mm == 6) {     te3p[m][mm]= 0.056648; } 
	  if(m==29 && mm == 8) {     te3p[m][mm]= 0.598221; } 
	  if(m==29 && mm == 9) {     te3p[m][mm]= 0.339577; } 
	  if(m==29 && mm == 10) {     te3p[m][mm]= 0.145408; } 
	  if(m==29 && mm == 11) {     te3p[m][mm]= 0.563466; } 
	  if(m==29 && mm == 12) {     te3p[m][mm]= 0.013157; } 
	  if(m==29 && mm == 17) {     te3p[m][mm]= 0.237848; } 
	  if(m==29 && mm == 20) {     te3p[m][mm]= 0.409818; } 
	  if(m==29 && mm == 0) {     tw0n[m][mm]= 0.259005; } 
	  if(m==29 && mm == 2) {     tw0n[m][mm]= 0.650228; } 
	  if(m==29 && mm == 5) {     tw0n[m][mm]= 0.088214; } 
	  if(m==29 && mm == 8) {     tw0n[m][mm]= 0.356406; } 
	  if(m==29 && mm == 11) {     tw0n[m][mm]= 0.018802; } 
	  if(m==29 && mm == 24) {     tw0n[m][mm]= 0.030785; } 
	  if(m==29 && mm == 29) {     tw0n[m][mm]= 0.010047; } 
	  if(m==29 && mm == 30) {     tw0n[m][mm]= 0.131046; } 
	  if(m==29 && mm == 8) {     tw1n[m][mm]= 0.333496; } 
	  if(m==29 && mm == 22) {     tw1n[m][mm]= 0.386877; } 
	  if(m==29 && mm == 33) {     tw1n[m][mm]= 0.777324; } 
	  if(m==29 && mm == 34) {     tw1n[m][mm]= 0.216468; } 
	  if(m==29 && mm == 35) {     tw1n[m][mm]= 0.097878; } 
	  if(m==29 && mm == 0) {     tw2n[m][mm]= 0.015779; } 
	  if(m==29 && mm == 8) {     tw2n[m][mm]= 0.430042; } 
	  if(m==29 && mm == 16) {     tw2n[m][mm]= 0.066894; } 
	  if(m==29 && mm == 18) {     tw2n[m][mm]= 0.253098; } 
	  if(m==29 && mm == 29) {     tw2n[m][mm]= 0.048767; } 
	  if(m==29 && mm == 32) {     tw2n[m][mm]= 0.142662; } 
	  if(m==29 && mm == 33) {     tw2n[m][mm]= 0.480845; } 
	  if(m==29 && mm == 34) {     tw2n[m][mm]= 0.218786; } 
	  if(m==29 && mm == 0) {     tw3n[m][mm]= 0.555124; } 
	  if(m==29 && mm == 1) {     tw3n[m][mm]= 0.502487; } 
	  if(m==29 && mm == 2) {     tw3n[m][mm]= 0.147748; } 
	  if(m==29 && mm == 4) {     tw3n[m][mm]= 0.248353; } 
	  if(m==29 && mm == 5) {     tw3n[m][mm]= 0.160501; } 
	  if(m==29 && mm == 6) {     tw3n[m][mm]= 0.618332; } 
	  if(m==29 && mm == 7) {     tw3n[m][mm]= 0.500230; } 
	  if(m==29 && mm == 8) {     tw3n[m][mm]= 0.136755; } 
	  if(m==29 && mm == 9) {     tw3n[m][mm]= 0.092794; } 
	  if(m==29 && mm == 10) {     tw3n[m][mm]= 0.175026; } 
	  if(m==29 && mm == 13) {     tw3n[m][mm]= 0.372143; } 
	  if(m==29 && mm == 14) {     tw3n[m][mm]= 0.041036; } 
	  if(m==29 && mm == 15) {     tw3n[m][mm]= 0.407184; } 
	  if(m==29 && mm == 21) {     tw3n[m][mm]= 0.047201; } 
	  if(m==29 && mm == 13) {     te2n[m][mm]= 0.027750; } 
	  if(m==29 && mm == 18) {     te2n[m][mm]= 0.097233; } 
	  if(m==29 && mm == 19) {     te2n[m][mm]= 0.074862; } 
	  if(m==29 && mm == 20) {     te2n[m][mm]= 0.021866; } 
	  if(m==29 && mm == 22) {     te2n[m][mm]= 0.141761; } 
	  if(m==29 && mm == 26) {     te2n[m][mm]= 0.305645; } 
	  if(m==29 && mm == 30) {     te2n[m][mm]= 0.248554; } 
	  if(m==29 && mm == 32) {     te2n[m][mm]= 0.397471; } 
	  if(m==29 && mm == 3) {     te3n[m][mm]= 0.106200; } 
	  if(m==29 && mm == 20) {     te3n[m][mm]= 0.715190; } 
	  if(m==30 && mm == 11) {     tw0p[m][mm]= 0.307225; } 
	  if(m==30 && mm == 17) {     tw0p[m][mm]= 0.025649; } 
	  if(m==30 && mm == 18) {     tw0p[m][mm]= 0.091016; } 
	  if(m==30 && mm == 19) {     tw0p[m][mm]= 0.070848; } 
	  if(m==30 && mm == 20) {     tw0p[m][mm]= 0.002535; } 
	  if(m==30 && mm == 26) {     tw0p[m][mm]= 0.297863; } 
	  if(m==30 && mm == 27) {     tw0p[m][mm]= 0.114658; } 
	  if(m==30 && mm == 28) {     tw0p[m][mm]= 0.211059; } 
	  if(m==30 && mm == 30) {     tw0p[m][mm]= 0.483468; } 
	  if(m==30 && mm == 31) {     tw0p[m][mm]= 0.325754; } 
	  if(m==30 && mm == 32) {     tw0p[m][mm]= 0.044855; } 
	  if(m==30 && mm == 33) {     tw0p[m][mm]= 0.063295; } 
	  if(m==30 && mm == 34) {     tw0p[m][mm]= 0.183550; } 
	  if(m==30 && mm == 35) {     tw0p[m][mm]= 0.242358; } 
	  if(m==30 && mm == 0) {     tw1p[m][mm]= 0.093924; } 
	  if(m==30 && mm == 1) {     tw1p[m][mm]= 0.100723; } 
	  if(m==30 && mm == 3) {     tw1p[m][mm]= 0.041791; } 
	  if(m==30 && mm == 4) {     tw1p[m][mm]= 0.108284; } 
	  if(m==30 && mm == 5) {     tw1p[m][mm]= 0.227729; } 
	  if(m==30 && mm == 19) {     tw1p[m][mm]= 0.089696; } 
	  if(m==30 && mm == 21) {     tw1p[m][mm]= 0.131863; } 
	  if(m==30 && mm == 22) {     tw1p[m][mm]= 0.150685; } 
	  if(m==30 && mm == 23) {     tw1p[m][mm]= 0.272199; } 
	  if(m==30 && mm == 28) {     tw1p[m][mm]= 0.181704; } 
	  if(m==30 && mm == 29) {     tw1p[m][mm]= 0.358742; } 
	  if(m==30 && mm == 31) {     tw1p[m][mm]= 0.165706; } 
	  if(m==30 && mm == 32) {     tw1p[m][mm]= 0.173757; } 
	  if(m==30 && mm == 34) {     tw1p[m][mm]= 0.115409; } 
	  if(m==30 && mm == 6) {     tw2p[m][mm]= 0.477861; } 
	  if(m==30 && mm == 7) {     tw2p[m][mm]= 0.205350; } 
	  if(m==30 && mm == 8) {     tw2p[m][mm]= 0.044877; } 
	  if(m==30 && mm == 11) {     tw2p[m][mm]= 0.106219; } 
	  if(m==30 && mm == 16) {     tw2p[m][mm]= 0.058648; } 
	  if(m==30 && mm == 28) {     tw2p[m][mm]= 0.191145; } 
	  if(m==30 && mm == 29) {     tw2p[m][mm]= 0.320819; } 
	  if(m==30 && mm == 31) {     tw2p[m][mm]= 0.001286; } 
	  if(m==30 && mm == 32) {     tw2p[m][mm]= 0.357366; } 
	  if(m==30 && mm == 33) {     tw2p[m][mm]= 0.221107; } 
	  if(m==30 && mm == 6) {     tw3p[m][mm]= 0.415777; } 
	  if(m==30 && mm == 7) {     tw3p[m][mm]= 0.218394; } 
	  if(m==30 && mm == 10) {     tw3p[m][mm]= 0.622109; } 
	  if(m==30 && mm == 11) {     tw3p[m][mm]= 0.251955; } 
	  if(m==30 && mm == 13) {     tw3p[m][mm]= 0.238470; } 
	  if(m==30 && mm == 14) {     tw3p[m][mm]= 0.282191; } 
	  if(m==30 && mm == 18) {     tw3p[m][mm]= 0.100042; } 
	  if(m==30 && mm == 21) {     tw3p[m][mm]= 0.196788; } 
	  if(m==30 && mm == 23) {     tw3p[m][mm]= 0.647013; } 
	  if(m==30 && mm == 32) {     tw3p[m][mm]= 0.182415; } 
	  if(m==30 && mm == 33) {     tw3p[m][mm]= 0.119817; } 
	  if(m==30 && mm == 34) {     tw3p[m][mm]= 1.521615; } 
	  if(m==30 && mm == 35) {     tw3p[m][mm]= 1.243482; } 
	  if(m==30 && mm == 0) {     te2p[m][mm]= 0.061376; } 
	  if(m==30 && mm == 1) {     te2p[m][mm]= 0.130758; } 
	  if(m==30 && mm == 3) {     te2p[m][mm]= 0.075542; } 
	  if(m==30 && mm == 4) {     te2p[m][mm]= 0.010630; } 
	  if(m==30 && mm == 5) {     te2p[m][mm]= 0.256566; } 
	  if(m==30 && mm == 6) {     te2p[m][mm]= 0.123230; } 
	  if(m==30 && mm == 7) {     te2p[m][mm]= 0.051530; } 
	  if(m==30 && mm == 8) {     te2p[m][mm]= 0.077510; } 
	  if(m==30 && mm == 12) {     te2p[m][mm]= 0.217549; } 
	  if(m==30 && mm == 18) {     te2p[m][mm]= 0.082249; } 
	  if(m==30 && mm == 21) {     te2p[m][mm]= 0.016951; } 
	  if(m==30 && mm == 24) {     te2p[m][mm]= 0.335735; } 
	  if(m==30 && mm == 2) {     te3p[m][mm]= 0.004473; } 
	  if(m==30 && mm == 6) {     te3p[m][mm]= 0.231634; } 
	  if(m==30 && mm == 7) {     te3p[m][mm]= 0.487382; } 
	  if(m==30 && mm == 8) {     te3p[m][mm]= 0.635334; } 
	  if(m==30 && mm == 10) {     te3p[m][mm]= 0.246251; } 
	  if(m==30 && mm == 11) {     te3p[m][mm]= 0.378373; } 
	  if(m==30 && mm == 15) {     te3p[m][mm]= 0.008010; } 
	  if(m==30 && mm == 18) {     te3p[m][mm]= 0.238379; } 
	  if(m==30 && mm == 19) {     te3p[m][mm]= 0.057713; } 
	  if(m==30 && mm == 0) {     tw0n[m][mm]= 0.251401; } 
	  if(m==30 && mm == 3) {     tw0n[m][mm]= 0.064746; } 
	  if(m==30 && mm == 11) {     tw0n[m][mm]= 0.136968; } 
	  if(m==30 && mm == 12) {     tw0n[m][mm]= 0.104940; } 
	  if(m==30 && mm == 24) {     tw0n[m][mm]= 0.100341; } 
	  if(m==30 && mm == 26) {     tw0n[m][mm]= 0.150390; } 
	  if(m==30 && mm == 6) {     tw1n[m][mm]= 0.002466; } 
	  if(m==30 && mm == 7) {     tw1n[m][mm]= 0.051160; } 
	  if(m==30 && mm == 10) {     tw1n[m][mm]= 0.041866; } 
	  if(m==30 && mm == 14) {     tw1n[m][mm]= 0.196150; } 
	  if(m==30 && mm == 16) {     tw1n[m][mm]= 0.169276; } 
	  if(m==30 && mm == 17) {     tw1n[m][mm]= 0.190946; } 
	  if(m==30 && mm == 2) {     tw2n[m][mm]= 0.030443; } 
	  if(m==30 && mm == 4) {     tw2n[m][mm]= 0.021661; } 
	  if(m==30 && mm == 6) {     tw2n[m][mm]= 0.400032; } 
	  if(m==30 && mm == 7) {     tw2n[m][mm]= 0.293929; } 
	  if(m==30 && mm == 8) {     tw2n[m][mm]= 0.006589; } 
	  if(m==30 && mm == 23) {     tw2n[m][mm]= 0.271984; } 
	  if(m==30 && mm == 32) {     tw2n[m][mm]= 0.032288; } 
	  if(m==30 && mm == 6) {     tw3n[m][mm]= 0.475932; } 
	  if(m==30 && mm == 7) {     tw3n[m][mm]= 0.280919; } 
	  if(m==30 && mm == 10) {     tw3n[m][mm]= 0.409820; } 
	  if(m==30 && mm == 11) {     tw3n[m][mm]= 0.284486; } 
	  if(m==30 && mm == 13) {     tw3n[m][mm]= 0.083082; } 
	  if(m==30 && mm == 14) {     tw3n[m][mm]= 0.232657; } 
	  if(m==30 && mm == 15) {     tw3n[m][mm]= 1.241257; } 
	  if(m==30 && mm == 18) {     tw3n[m][mm]= 0.083480; } 
	  if(m==30 && mm == 23) {     tw3n[m][mm]= 0.324959; } 
	  if(m==30 && mm == 24) {     te2n[m][mm]= 0.244564; } 
	  if(m==30 && mm == 25) {     te2n[m][mm]= 0.057734; } 
	  if(m==30 && mm == 26) {     te2n[m][mm]= 0.133215; } 
	  if(m==30 && mm == 28) {     te2n[m][mm]= 0.298465; } 
	  if(m==30 && mm == 29) {     te2n[m][mm]= 0.198727; } 
	  if(m==30 && mm == 32) {     te2n[m][mm]= 0.044860; } 
	  if(m==30 && mm == 34) {     te2n[m][mm]= 0.520522; } 
	  if(m==30 && mm == 18) {     te3n[m][mm]= 0.418329; } 
	  if(m==30 && mm == 32) {     te3n[m][mm]= 0.529230; } 
	  if(m==31 && mm == 16) {     tw0p[m][mm]= 0.050520; } 
	  if(m==31 && mm == 19) {     tw0p[m][mm]= 0.493907; } 
	  if(m==31 && mm == 26) {     tw0p[m][mm]= 0.219179; } 
	  if(m==31 && mm == 27) {     tw0p[m][mm]= 0.044580; } 
	  if(m==31 && mm == 28) {     tw0p[m][mm]= 0.205459; } 
	  if(m==31 && mm == 29) {     tw0p[m][mm]= 0.165090; } 
	  if(m==31 && mm == 30) {     tw0p[m][mm]= 0.453091; } 
	  if(m==31 && mm == 31) {     tw0p[m][mm]= 0.212228; } 
	  if(m==31 && mm == 33) {     tw0p[m][mm]= 0.141036; } 
	  if(m==31 && mm == 34) {     tw0p[m][mm]= 0.379079; } 
	  if(m==31 && mm == 35) {     tw0p[m][mm]= 0.407370; } 
	  if(m==31 && mm == 4) {     tw1p[m][mm]= 0.089765; } 
	  if(m==31 && mm == 5) {     tw1p[m][mm]= 0.001508; } 
	  if(m==31 && mm == 19) {     tw1p[m][mm]= 0.010282; } 
	  if(m==31 && mm == 22) {     tw1p[m][mm]= 0.029479; } 
	  if(m==31 && mm == 23) {     tw1p[m][mm]= 0.042900; } 
	  if(m==31 && mm == 29) {     tw1p[m][mm]= 0.079858; } 
	  if(m==31 && mm == 31) {     tw1p[m][mm]= 0.408031; } 
	  if(m==31 && mm == 32) {     tw1p[m][mm]= 0.107991; } 
	  if(m==31 && mm == 33) {     tw1p[m][mm]= 0.205818; } 
	  if(m==31 && mm == 34) {     tw1p[m][mm]= 0.388447; } 
	  if(m==31 && mm == 7) {     tw2p[m][mm]= 0.046515; } 
	  if(m==31 && mm == 9) {     tw2p[m][mm]= 0.039949; } 
	  if(m==31 && mm == 11) {     tw2p[m][mm]= 0.073443; } 
	  if(m==31 && mm == 28) {     tw2p[m][mm]= 0.663564; } 
	  if(m==31 && mm == 29) {     tw2p[m][mm]= 0.113724; } 
	  if(m==31 && mm == 31) {     tw2p[m][mm]= 0.329229; } 
	  if(m==31 && mm == 32) {     tw2p[m][mm]= 0.338083; } 
	  if(m==31 && mm == 33) {     tw2p[m][mm]= 0.365062; } 
	  if(m==31 && mm == 6) {     tw3p[m][mm]= 0.444473; } 
	  if(m==31 && mm == 7) {     tw3p[m][mm]= 0.282092; } 
	  if(m==31 && mm == 10) {     tw3p[m][mm]= 0.470335; } 
	  if(m==31 && mm == 11) {     tw3p[m][mm]= 0.541328; } 
	  if(m==31 && mm == 12) {     tw3p[m][mm]= 0.259855; } 
	  if(m==31 && mm == 13) {     tw3p[m][mm]= 0.076073; } 
	  if(m==31 && mm == 14) {     tw3p[m][mm]= 0.506657; } 
	  if(m==31 && mm == 15) {     tw3p[m][mm]= 0.029213; } 
	  if(m==31 && mm == 18) {     tw3p[m][mm]= 0.328512; } 
	  if(m==31 && mm == 32) {     tw3p[m][mm]= 0.767917; } 
	  if(m==31 && mm == 33) {     tw3p[m][mm]= 0.541080; } 
	  if(m==31 && mm == 34) {     tw3p[m][mm]= 1.422777; } 
	  if(m==31 && mm == 35) {     tw3p[m][mm]= 0.540239; } 
	  if(m==31 && mm == 0) {     te2p[m][mm]= 0.009392; } 
	  if(m==31 && mm == 1) {     te2p[m][mm]= 0.113373; } 
	  if(m==31 && mm == 4) {     te2p[m][mm]= 0.102353; } 
	  if(m==31 && mm == 5) {     te2p[m][mm]= 0.072348; } 
	  if(m==31 && mm == 6) {     te2p[m][mm]= 0.125422; } 
	  if(m==31 && mm == 10) {     te2p[m][mm]= 0.089771; } 
	  if(m==31 && mm == 11) {     te2p[m][mm]= 0.328249; } 
	  if(m==31 && mm == 18) {     te2p[m][mm]= 0.135874; } 
	  if(m==31 && mm == 19) {     te2p[m][mm]= 0.356565; } 
	  if(m==31 && mm == 21) {     te2p[m][mm]= 0.105303; } 
	  if(m==31 && mm == 28) {     te2p[m][mm]= 0.074363; } 
	  if(m==31 && mm == 3) {     te3p[m][mm]= 0.292559; } 
	  if(m==31 && mm == 4) {     te3p[m][mm]= 0.555614; } 
	  if(m==31 && mm == 6) {     te3p[m][mm]= 0.419766; } 
	  if(m==31 && mm == 7) {     te3p[m][mm]= 0.375420; } 
	  if(m==31 && mm == 8) {     te3p[m][mm]= 0.257335; } 
	  if(m==31 && mm == 10) {     te3p[m][mm]= 0.251188; } 
	  if(m==31 && mm == 11) {     te3p[m][mm]= 0.029692; } 
	  if(m==31 && mm == 18) {     te3p[m][mm]= 0.013915; } 
	  if(m==31 && mm == 23) {     te3p[m][mm]= 0.021638; } 
	  if(m==31 && mm == 0) {     tw0n[m][mm]= 0.085564; } 
	  if(m==31 && mm == 2) {     tw0n[m][mm]= 0.208396; } 
	  if(m==31 && mm == 3) {     tw0n[m][mm]= 0.314478; } 
	  if(m==31 && mm == 5) {     tw0n[m][mm]= 0.460930; } 
	  if(m==31 && mm == 7) {     tw0n[m][mm]= 0.121545; } 
	  if(m==31 && mm == 11) {     tw1n[m][mm]= 0.467810; } 
	  if(m==31 && mm == 35) {     tw1n[m][mm]= 0.031341; } 
	  if(m==31 && mm == 5) {     tw2n[m][mm]= 0.239586; } 
	  if(m==31 && mm == 7) {     tw2n[m][mm]= 0.160871; } 
	  if(m==31 && mm == 28) {     tw2n[m][mm]= 0.215473; } 
	  if(m==31 && mm == 32) {     tw2n[m][mm]= 0.024128; } 
	  if(m==31 && mm == 6) {     tw3n[m][mm]= 0.478666; } 
	  if(m==31 && mm == 7) {     tw3n[m][mm]= 0.337870; } 
	  if(m==31 && mm == 10) {     tw3n[m][mm]= 0.502075; } 
	  if(m==31 && mm == 12) {     tw3n[m][mm]= 0.302803; } 
	  if(m==31 && mm == 15) {     tw3n[m][mm]= 0.129703; } 
	  if(m==31 && mm == 18) {     tw3n[m][mm]= 0.239773; } 
	  if(m==31 && mm == 20) {     te2n[m][mm]= 0.153584; } 
	  if(m==31 && mm == 28) {     te2n[m][mm]= 0.401120; } 
	  if(m==31 && mm == 30) {     te2n[m][mm]= 0.104026; } 
	  if(m==31 && mm == 33) {     te2n[m][mm]= 0.122249; } 
	  if(m==31 && mm == 34) {     te2n[m][mm]= 0.063951; } 
	  if(m==31 && mm == 3) {     te3n[m][mm]= 0.011641; } 
	  if(m==32 && mm == 16) {     tw0p[m][mm]= 0.510132; } 
	  if(m==32 && mm == 21) {     tw0p[m][mm]= 0.147787; } 
	  if(m==32 && mm == 23) {     tw0p[m][mm]= 0.143474; } 
	  if(m==32 && mm == 24) {     tw0p[m][mm]= 0.007113; } 
	  if(m==32 && mm == 27) {     tw0p[m][mm]= 0.262165; } 
	  if(m==32 && mm == 28) {     tw0p[m][mm]= 0.389005; } 
	  if(m==32 && mm == 29) {     tw0p[m][mm]= 0.144064; } 
	  if(m==32 && mm == 30) {     tw0p[m][mm]= 0.455125; } 
	  if(m==32 && mm == 31) {     tw0p[m][mm]= 0.215103; } 
	  if(m==32 && mm == 32) {     tw0p[m][mm]= 0.329480; } 
	  if(m==32 && mm == 33) {     tw0p[m][mm]= 0.198355; } 
	  if(m==32 && mm == 34) {     tw0p[m][mm]= 0.449737; } 
	  if(m==32 && mm == 0) {     tw1p[m][mm]= 0.044013; } 
	  if(m==32 && mm == 19) {     tw1p[m][mm]= 0.113457; } 
	  if(m==32 && mm == 26) {     tw1p[m][mm]= 0.148007; } 
	  if(m==32 && mm == 29) {     tw1p[m][mm]= 0.097044; } 
	  if(m==32 && mm == 30) {     tw1p[m][mm]= 0.015474; } 
	  if(m==32 && mm == 31) {     tw1p[m][mm]= 0.117648; } 
	  if(m==32 && mm == 32) {     tw1p[m][mm]= 0.106728; } 
	  if(m==32 && mm == 34) {     tw1p[m][mm]= 0.370273; } 
	  if(m==32 && mm == 35) {     tw1p[m][mm]= 0.044024; } 
	  if(m==32 && mm == 6) {     tw2p[m][mm]= 0.098662; } 
	  if(m==32 && mm == 9) {     tw2p[m][mm]= 0.075041; } 
	  if(m==32 && mm == 16) {     tw2p[m][mm]= 0.205157; } 
	  if(m==32 && mm == 19) {     tw2p[m][mm]= 0.000083; } 
	  if(m==32 && mm == 20) {     tw2p[m][mm]= 0.183374; } 
	  if(m==32 && mm == 21) {     tw2p[m][mm]= 0.309746; } 
	  if(m==32 && mm == 32) {     tw2p[m][mm]= 0.318156; } 
	  if(m==32 && mm == 34) {     tw2p[m][mm]= 0.082144; } 
	  if(m==32 && mm == 35) {     tw2p[m][mm]= 0.225217; } 
	  if(m==32 && mm == 6) {     tw3p[m][mm]= 0.994294; } 
	  if(m==32 && mm == 7) {     tw3p[m][mm]= 0.788452; } 
	  if(m==32 && mm == 8) {     tw3p[m][mm]= 0.510607; } 
	  if(m==32 && mm == 10) {     tw3p[m][mm]= 0.904681; } 
	  if(m==32 && mm == 12) {     tw3p[m][mm]= 0.313923; } 
	  if(m==32 && mm == 14) {     tw3p[m][mm]= 0.226561; } 
	  if(m==32 && mm == 15) {     tw3p[m][mm]= 0.010465; } 
	  if(m==32 && mm == 20) {     tw3p[m][mm]= 0.394230; } 
	  if(m==32 && mm == 21) {     tw3p[m][mm]= 0.140988; } 
	  if(m==32 && mm == 22) {     tw3p[m][mm]= 0.752865; } 
	  if(m==32 && mm == 23) {     tw3p[m][mm]= 0.406436; } 
	  if(m==32 && mm == 34) {     tw3p[m][mm]= 0.151826; } 
	  if(m==32 && mm == 35) {     tw3p[m][mm]= 0.087623; } 
	  if(m==32 && mm == 0) {     te2p[m][mm]= 0.387438; } 
	  if(m==32 && mm == 2) {     te2p[m][mm]= 0.221166; } 
	  if(m==32 && mm == 4) {     te2p[m][mm]= 0.637647; } 
	  if(m==32 && mm == 5) {     te2p[m][mm]= 0.161342; } 
	  if(m==32 && mm == 8) {     te2p[m][mm]= 0.764466; } 
	  if(m==32 && mm == 9) {     te2p[m][mm]= 0.324484; } 
	  if(m==32 && mm == 15) {     te2p[m][mm]= 0.098333; } 
	  if(m==32 && mm == 5) {     te3p[m][mm]= 0.532151; } 
	  if(m==32 && mm == 10) {     te3p[m][mm]= 0.383345; } 
	  if(m==32 && mm == 11) {     te3p[m][mm]= 0.069240; } 
	  if(m==32 && mm == 0) {     tw0n[m][mm]= 0.396769; } 
	  if(m==32 && mm == 1) {     tw0n[m][mm]= 0.758704; } 
	  if(m==32 && mm == 2) {     tw0n[m][mm]= 0.106345; } 
	  if(m==32 && mm == 5) {     tw0n[m][mm]= 0.339053; } 
	  if(m==32 && mm == 13) {     tw0n[m][mm]= 0.050981; } 
	  if(m==32 && mm == 22) {     tw0n[m][mm]= 0.521500; } 
	  if(m==32 && mm == 28) {     tw0n[m][mm]= 0.171408; } 
	  if(m==32 && mm == 30) {     tw0n[m][mm]= 0.163999; } 
	  if(m==32 && mm == 6) {     tw1n[m][mm]= 0.333537; } 
	  if(m==32 && mm == 11) {     tw1n[m][mm]= 0.146264; } 
	  if(m==32 && mm == 12) {     tw1n[m][mm]= 0.024262; } 
	  if(m==32 && mm == 18) {     tw1n[m][mm]= 0.224130; } 
	  if(m==32 && mm == 34) {     tw1n[m][mm]= 0.036364; } 
	  if(m==32 && mm == 0) {     tw2n[m][mm]= 0.324543; } 
	  if(m==32 && mm == 1) {     tw2n[m][mm]= 0.244431; } 
	  if(m==32 && mm == 5) {     tw2n[m][mm]= 0.532308; } 
	  if(m==32 && mm == 6) {     tw2n[m][mm]= 0.012513; } 
	  if(m==32 && mm == 20) {     tw2n[m][mm]= 0.030283; } 
	  if(m==32 && mm == 6) {     tw3n[m][mm]= 1.246565; } 
	  if(m==32 && mm == 7) {     tw3n[m][mm]= 1.057537; } 
	  if(m==32 && mm == 8) {     tw3n[m][mm]= 0.145402; } 
	  if(m==32 && mm == 10) {     tw3n[m][mm]= 1.123875; } 
	  if(m==32 && mm == 20) {     tw3n[m][mm]= 0.251997; } 
	  if(m==32 && mm == 22) {     tw3n[m][mm]= 0.051671; } 
	  if(m==32 && mm == 23) {     tw3n[m][mm]= 0.172245; } 
	  if(m==32 && mm == 14) {     te2n[m][mm]= 0.371690; } 
	  if(m==32 && mm == 24) {     te2n[m][mm]= 0.128282; } 
	  if(m==32 && mm == 30) {     te2n[m][mm]= 0.096850; } 
	  if(m==32 && mm == 31) {     te2n[m][mm]= 0.050811; } 
	  if(m==32 && mm == 34) {     te2n[m][mm]= 0.135841; } 
	  if(m==32 && mm == 35) {     te2n[m][mm]= 0.171912; } 
	  if(m==32 && mm == 0) {     te3n[m][mm]= 0.220454; } 
	  if(m==32 && mm == 26) {     te3n[m][mm]= 0.168120; } 
	  if(m==33 && mm == 14) {     tw0p[m][mm]= 0.283177; } 
	  if(m==33 && mm == 17) {     tw0p[m][mm]= 0.152348; } 
	  if(m==33 && mm == 24) {     tw0p[m][mm]= 0.102626; } 
	  if(m==33 && mm == 26) {     tw0p[m][mm]= 0.210477; } 
	  if(m==33 && mm == 27) {     tw0p[m][mm]= 0.030732; } 
	  if(m==33 && mm == 28) {     tw0p[m][mm]= 0.017403; } 
	  if(m==33 && mm == 29) {     tw0p[m][mm]= 0.054441; } 
	  if(m==33 && mm == 30) {     tw0p[m][mm]= 0.230348; } 
	  if(m==33 && mm == 31) {     tw0p[m][mm]= 0.459206; } 
	  if(m==33 && mm == 32) {     tw0p[m][mm]= 0.193031; } 
	  if(m==33 && mm == 33) {     tw0p[m][mm]= 0.201504; } 
	  if(m==33 && mm == 34) {     tw0p[m][mm]= 0.556629; } 
	  if(m==33 && mm == 35) {     tw0p[m][mm]= 0.226709; } 
	  if(m==33 && mm == 0) {     tw1p[m][mm]= 0.172226; } 
	  if(m==33 && mm == 3) {     tw1p[m][mm]= 0.058626; } 
	  if(m==33 && mm == 20) {     tw1p[m][mm]= 0.070537; } 
	  if(m==33 && mm == 31) {     tw1p[m][mm]= 0.060638; } 
	  if(m==33 && mm == 34) {     tw1p[m][mm]= 0.228560; } 
	  if(m==33 && mm == 35) {     tw1p[m][mm]= 0.731222; } 
	  if(m==33 && mm == 5) {     tw2p[m][mm]= 0.081468; } 
	  if(m==33 && mm == 17) {     tw2p[m][mm]= 0.011973; } 
	  if(m==33 && mm == 18) {     tw2p[m][mm]= 0.260718; } 
	  if(m==33 && mm == 22) {     tw2p[m][mm]= 0.012330; } 
	  if(m==33 && mm == 28) {     tw2p[m][mm]= 0.301517; } 
	  if(m==33 && mm == 29) {     tw2p[m][mm]= 0.505635; } 
	  if(m==33 && mm == 30) {     tw2p[m][mm]= 0.262524; } 
	  if(m==33 && mm == 31) {     tw2p[m][mm]= 0.562454; } 
	  if(m==33 && mm == 33) {     tw2p[m][mm]= 0.342791; } 
	  if(m==33 && mm == 35) {     tw2p[m][mm]= 0.126649; } 
	  if(m==33 && mm == 6) {     tw3p[m][mm]= 0.538183; } 
	  if(m==33 && mm == 7) {     tw3p[m][mm]= 0.803078; } 
	  if(m==33 && mm == 10) {     tw3p[m][mm]= 0.440478; } 
	  if(m==33 && mm == 12) {     tw3p[m][mm]= 1.147077; } 
	  if(m==33 && mm == 18) {     tw3p[m][mm]= 0.682681; } 
	  if(m==33 && mm == 19) {     tw3p[m][mm]= 0.272726; } 
	  if(m==33 && mm == 20) {     tw3p[m][mm]= 0.420513; } 
	  if(m==33 && mm == 21) {     tw3p[m][mm]= 0.479636; } 
	  if(m==33 && mm == 22) {     tw3p[m][mm]= 0.694904; } 
	  if(m==33 && mm == 23) {     tw3p[m][mm]= 0.538529; } 
	  if(m==33 && mm == 34) {     tw3p[m][mm]= 0.139100; } 
	  if(m==33 && mm == 4) {     te2p[m][mm]= 0.063002; } 
	  if(m==33 && mm == 5) {     te2p[m][mm]= 0.121190; } 
	  if(m==33 && mm == 8) {     te2p[m][mm]= 0.211956; } 
	  if(m==33 && mm == 11) {     te2p[m][mm]= 0.498632; } 
	  if(m==33 && mm == 19) {     te2p[m][mm]= 0.416049; } 
	  if(m==33 && mm == 5) {     te3p[m][mm]= 0.478827; } 
	  if(m==33 && mm == 8) {     te3p[m][mm]= 0.552361; } 
	  if(m==33 && mm == 11) {     te3p[m][mm]= 0.185988; } 
	  if(m==33 && mm == 12) {     te3p[m][mm]= 0.038815; } 
	  if(m==33 && mm == 16) {     te3p[m][mm]= 0.058751; } 
	  if(m==33 && mm == 6) {     tw0n[m][mm]= 0.108729; } 
	  if(m==33 && mm == 7) {     tw0n[m][mm]= 0.058688; } 
	  if(m==33 && mm == 8) {     tw0n[m][mm]= 0.023835; } 
	  if(m==33 && mm == 13) {     tw0n[m][mm]= 0.053286; } 
	  if(m==33 && mm == 14) {     tw0n[m][mm]= 0.501717; } 
	  if(m==33 && mm == 20) {     tw0n[m][mm]= 0.007642; } 
	  if(m==33 && mm == 24) {     tw0n[m][mm]= 0.018277; } 
	  if(m==33 && mm == 26) {     tw0n[m][mm]= 0.081743; } 
	  if(m==33 && mm == 27) {     tw0n[m][mm]= 0.014554; } 
	  if(m==33 && mm == 17) {     tw1n[m][mm]= 0.078222; } 
	  if(m==33 && mm == 34) {     tw1n[m][mm]= 0.086489; } 
	  if(m==33 && mm == 0) {     tw2n[m][mm]= 0.043838; } 
	  if(m==33 && mm == 1) {     tw2n[m][mm]= 0.036441; } 
	  if(m==33 && mm == 4) {     tw2n[m][mm]= 0.172684; } 
	  if(m==33 && mm == 5) {     tw2n[m][mm]= 0.273632; } 
	  if(m==33 && mm == 6) {     tw2n[m][mm]= 0.280771; } 
	  if(m==33 && mm == 7) {     tw2n[m][mm]= 1.865101; } 
	  if(m==33 && mm == 14) {     tw2n[m][mm]= 0.008270; } 
	  if(m==33 && mm == 18) {     tw2n[m][mm]= 0.087532; } 
	  if(m==33 && mm == 29) {     tw2n[m][mm]= 0.057635; } 
	  if(m==33 && mm == 30) {     tw2n[m][mm]= 0.130883; } 
	  if(m==33 && mm == 33) {     tw2n[m][mm]= 0.000844; } 
	  if(m==33 && mm == 6) {     tw3n[m][mm]= 0.166114; } 
	  if(m==33 && mm == 7) {     tw3n[m][mm]= 1.744574; } 
	  if(m==33 && mm == 9) {     tw3n[m][mm]= 0.253983; } 
	  if(m==33 && mm == 10) {     tw3n[m][mm]= 0.143012; } 
	  if(m==33 && mm == 15) {     tw3n[m][mm]= 0.348211; } 
	  if(m==33 && mm == 18) {     tw3n[m][mm]= 0.182252; } 
	  if(m==33 && mm == 20) {     tw3n[m][mm]= 0.179734; } 
	  if(m==33 && mm == 21) {     tw3n[m][mm]= 0.220383; } 
	  if(m==33 && mm == 22) {     tw3n[m][mm]= 0.350541; } 
	  if(m==33 && mm == 23) {     tw3n[m][mm]= 0.116548; } 
	  if(m==33 && mm == 30) {     te2n[m][mm]= 0.035484; } 
	  if(m==33 && mm == 32) {     te2n[m][mm]= 0.213287; } 
	  if(m==33 && mm == 35) {     te2n[m][mm]= 0.413718; } 
	  if(m==34 && mm == 12) {     tw0p[m][mm]= 0.196014; } 
	  if(m==34 && mm == 13) {     tw0p[m][mm]= 0.005858; } 
	  if(m==34 && mm == 15) {     tw0p[m][mm]= 0.200854; } 
	  if(m==34 && mm == 18) {     tw0p[m][mm]= 0.167262; } 
	  if(m==34 && mm == 20) {     tw0p[m][mm]= 0.067624; } 
	  if(m==34 && mm == 28) {     tw0p[m][mm]= 0.399037; } 
	  if(m==34 && mm == 29) {     tw0p[m][mm]= 0.213314; } 
	  if(m==34 && mm == 30) {     tw0p[m][mm]= 0.130134; } 
	  if(m==34 && mm == 31) {     tw0p[m][mm]= 0.361656; } 
	  if(m==34 && mm == 32) {     tw0p[m][mm]= 0.580122; } 
	  if(m==34 && mm == 33) {     tw0p[m][mm]= 0.431163; } 
	  if(m==34 && mm == 34) {     tw0p[m][mm]= 0.231612; } 
	  if(m==34 && mm == 35) {     tw0p[m][mm]= 0.388859; } 
	  if(m==34 && mm == 0) {     tw1p[m][mm]= 0.372798; } 
	  if(m==34 && mm == 7) {     tw1p[m][mm]= 0.144713; } 
	  if(m==34 && mm == 22) {     tw1p[m][mm]= 0.066348; } 
	  if(m==34 && mm == 28) {     tw1p[m][mm]= 0.186939; } 
	  if(m==34 && mm == 29) {     tw1p[m][mm]= 0.060460; } 
	  if(m==34 && mm == 32) {     tw1p[m][mm]= 0.212060; } 
	  if(m==34 && mm == 33) {     tw1p[m][mm]= 0.844592; } 
	  if(m==34 && mm == 34) {     tw1p[m][mm]= 0.685826; } 
	  if(m==34 && mm == 6) {     tw2p[m][mm]= 0.077768; } 
	  if(m==34 && mm == 7) {     tw2p[m][mm]= 0.007340; } 
	  if(m==34 && mm == 9) {     tw2p[m][mm]= 0.643023; } 
	  if(m==34 && mm == 12) {     tw2p[m][mm]= 0.036759; } 
	  if(m==34 && mm == 16) {     tw2p[m][mm]= 0.188508; } 
	  if(m==34 && mm == 17) {     tw2p[m][mm]= 0.159036; } 
	  if(m==34 && mm == 18) {     tw2p[m][mm]= 0.111024; } 
	  if(m==34 && mm == 28) {     tw2p[m][mm]= 0.043373; } 
	  if(m==34 && mm == 29) {     tw2p[m][mm]= 0.187574; } 
	  if(m==34 && mm == 30) {     tw2p[m][mm]= 0.446793; } 
	  if(m==34 && mm == 31) {     tw2p[m][mm]= 0.203572; } 
	  if(m==34 && mm == 32) {     tw2p[m][mm]= 1.262686; } 
	  if(m==34 && mm == 33) {     tw2p[m][mm]= 0.220382; } 
	  if(m==34 && mm == 34) {     tw2p[m][mm]= 0.032007; } 
	  if(m==34 && mm == 35) {     tw2p[m][mm]= 0.008342; } 
	  if(m==34 && mm == 2) {     tw3p[m][mm]= 0.105616; } 
	  if(m==34 && mm == 5) {     tw3p[m][mm]= 0.059743; } 
	  if(m==34 && mm == 6) {     tw3p[m][mm]= 0.346065; } 
	  if(m==34 && mm == 7) {     tw3p[m][mm]= 0.268415; } 
	  if(m==34 && mm == 8) {     tw3p[m][mm]= 0.352608; } 
	  if(m==34 && mm == 9) {     tw3p[m][mm]= 0.182101; } 
	  if(m==34 && mm == 11) {     tw3p[m][mm]= 0.048245; } 
	  if(m==34 && mm == 34) {     tw3p[m][mm]= 0.360246; } 
	  if(m==34 && mm == 35) {     tw3p[m][mm]= 0.129579; } 
	  if(m==34 && mm == 1) {     te2p[m][mm]= 0.611134; } 
	  if(m==34 && mm == 2) {     te2p[m][mm]= 0.051002; } 
	  if(m==34 && mm == 4) {     te2p[m][mm]= 0.442948; } 
	  if(m==34 && mm == 5) {     te2p[m][mm]= 0.554097; } 
	  if(m==34 && mm == 6) {     te2p[m][mm]= 0.033209; } 
	  if(m==34 && mm == 7) {     te2p[m][mm]= 0.286698; } 
	  if(m==34 && mm == 8) {     te2p[m][mm]= 0.140713; } 
	  if(m==34 && mm == 9) {     te2p[m][mm]= 0.520920; } 
	  if(m==34 && mm == 13) {     te2p[m][mm]= 0.063497; } 
	  if(m==34 && mm == 22) {     te2p[m][mm]= 0.080086; } 
	  if(m==34 && mm == 4) {     te3p[m][mm]= 0.043680; } 
	  if(m==34 && mm == 7) {     te3p[m][mm]= 0.154163; } 
	  if(m==34 && mm == 23) {     te3p[m][mm]= 0.054175; } 
	  if(m==34 && mm == 0) {     tw0n[m][mm]= 0.097708; } 
	  if(m==34 && mm == 1) {     tw0n[m][mm]= 0.015573; } 
	  if(m==34 && mm == 4) {     tw0n[m][mm]= 1.029637; } 
	  if(m==34 && mm == 6) {     tw0n[m][mm]= 0.050927; } 
	  if(m==34 && mm == 7) {     tw0n[m][mm]= 0.032316; } 
	  if(m==34 && mm == 13) {     tw0n[m][mm]= 0.004547; } 
	  if(m==34 && mm == 21) {     tw0n[m][mm]= 0.012673; } 
	  if(m==34 && mm == 26) {     tw0n[m][mm]= 0.061118; } 
	  if(m==34 && mm == 7) {     tw1n[m][mm]= 0.545304; } 
	  if(m==34 && mm == 11) {     tw1n[m][mm]= 0.162454; } 
	  if(m==34 && mm == 15) {     tw1n[m][mm]= 0.453528; } 
	  if(m==34 && mm == 34) {     tw1n[m][mm]= 0.094049; } 
	  if(m==34 && mm == 1) {     tw2n[m][mm]= 0.278264; } 
	  if(m==34 && mm == 4) {     tw2n[m][mm]= 0.108541; } 
	  if(m==34 && mm == 5) {     tw2n[m][mm]= 0.146166; } 
	  if(m==34 && mm == 6) {     tw2n[m][mm]= 0.205919; } 
	  if(m==34 && mm == 7) {     tw2n[m][mm]= 0.046036; } 
	  if(m==34 && mm == 16) {     tw2n[m][mm]= 0.206519; } 
	  if(m==34 && mm == 17) {     tw2n[m][mm]= 0.043769; } 
	  if(m==34 && mm == 2) {     tw3n[m][mm]= 0.097862; } 
	  if(m==34 && mm == 5) {     tw3n[m][mm]= 0.084472; } 
	  if(m==34 && mm == 6) {     tw3n[m][mm]= 0.322818; } 
	  if(m==34 && mm == 7) {     tw3n[m][mm]= 0.177572; } 
	  if(m==34 && mm == 8) {     tw3n[m][mm]= 0.425664; } 
	  if(m==34 && mm == 9) {     tw3n[m][mm]= 0.175179; } 
	  if(m==34 && mm == 11) {     tw3n[m][mm]= 0.028561; } 
	  if(m==34 && mm == 4) {     te2n[m][mm]= 0.187543; } 
	  if(m==34 && mm == 9) {     te2n[m][mm]= 0.113100; } 
	  if(m==34 && mm == 32) {     te2n[m][mm]= 0.347231; } 
	  if(m==34 && mm == 34) {     te2n[m][mm]= 0.360164; } 
	  if(m==34 && mm == 35) {     te2n[m][mm]= 0.045144; } 
	  if(m==35 && mm == 16) {     tw0p[m][mm]= 0.257447; } 
	  if(m==35 && mm == 18) {     tw0p[m][mm]= 0.505695; } 
	  if(m==35 && mm == 20) {     tw0p[m][mm]= 0.042937; } 
	  if(m==35 && mm == 22) {     tw0p[m][mm]= 0.126596; } 
	  if(m==35 && mm == 23) {     tw0p[m][mm]= 0.230109; } 
	  if(m==35 && mm == 26) {     tw0p[m][mm]= 0.189279; } 
	  if(m==35 && mm == 27) {     tw0p[m][mm]= 0.196512; } 
	  if(m==35 && mm == 29) {     tw0p[m][mm]= 0.096613; } 
	  if(m==35 && mm == 30) {     tw0p[m][mm]= 0.660218; } 
	  if(m==35 && mm == 31) {     tw0p[m][mm]= 0.452296; } 
	  if(m==35 && mm == 32) {     tw0p[m][mm]= 0.572147; } 
	  if(m==35 && mm == 33) {     tw0p[m][mm]= 0.442393; } 
	  if(m==35 && mm == 34) {     tw0p[m][mm]= 0.214098; } 
	  if(m==35 && mm == 35) {     tw0p[m][mm]= 0.387315; } 
	  if(m==35 && mm == 0) {     tw1p[m][mm]= 0.236031; } 
	  if(m==35 && mm == 2) {     tw1p[m][mm]= 0.313122; } 
	  if(m==35 && mm == 3) {     tw1p[m][mm]= 0.011136; } 
	  if(m==35 && mm == 21) {     tw1p[m][mm]= 0.058840; } 
	  if(m==35 && mm == 28) {     tw1p[m][mm]= 0.277022; } 
	  if(m==35 && mm == 29) {     tw1p[m][mm]= 0.184692; } 
	  if(m==35 && mm == 30) {     tw1p[m][mm]= 0.076360; } 
	  if(m==35 && mm == 31) {     tw1p[m][mm]= 0.153934; } 
	  if(m==35 && mm == 32) {     tw1p[m][mm]= 0.239603; } 
	  if(m==35 && mm == 33) {     tw1p[m][mm]= 0.374092; } 
	  if(m==35 && mm == 34) {     tw1p[m][mm]= 0.359412; } 
	  if(m==35 && mm == 6) {     tw2p[m][mm]= 0.307046; } 
	  if(m==35 && mm == 9) {     tw2p[m][mm]= 0.016003; } 
	  if(m==35 && mm == 10) {     tw2p[m][mm]= 0.010006; } 
	  if(m==35 && mm == 16) {     tw2p[m][mm]= 0.808680; } 
	  if(m==35 && mm == 18) {     tw2p[m][mm]= 0.051865; } 
	  if(m==35 && mm == 19) {     tw2p[m][mm]= 0.043980; } 
	  if(m==35 && mm == 20) {     tw2p[m][mm]= 0.144403; } 
	  if(m==35 && mm == 21) {     tw2p[m][mm]= 0.204486; } 
	  if(m==35 && mm == 22) {     tw2p[m][mm]= 0.111501; } 
	  if(m==35 && mm == 30) {     tw2p[m][mm]= 0.546668; } 
	  if(m==35 && mm == 33) {     tw2p[m][mm]= 0.800782; } 
	  if(m==35 && mm == 34) {     tw2p[m][mm]= 0.240343; } 
	  if(m==35 && mm == 5) {     tw3p[m][mm]= 0.400361; } 
	  if(m==35 && mm == 6) {     tw3p[m][mm]= 0.392699; } 
	  if(m==35 && mm == 7) {     tw3p[m][mm]= 0.483358; } 
	  if(m==35 && mm == 8) {     tw3p[m][mm]= 0.332105; } 
	  if(m==35 && mm == 9) {     tw3p[m][mm]= 0.361444; } 
	  if(m==35 && mm == 11) {     tw3p[m][mm]= 0.083444; } 
	  if(m==35 && mm == 34) {     tw3p[m][mm]= 0.585859; } 
	  if(m==35 && mm == 35) {     tw3p[m][mm]= 0.506993; } 
	  if(m==35 && mm == 3) {     te2p[m][mm]= 0.328694; } 
	  if(m==35 && mm == 5) {     te2p[m][mm]= 0.158274; } 
	  if(m==35 && mm == 6) {     te2p[m][mm]= 0.257183; } 
	  if(m==35 && mm == 9) {     te2p[m][mm]= 0.052883; } 
	  if(m==35 && mm == 18) {     te2p[m][mm]= 0.140249; } 
	  if(m==35 && mm == 21) {     te2p[m][mm]= 0.740719; } 
	  if(m==35 && mm == 4) {     te3p[m][mm]= 0.366043; } 
	  if(m==35 && mm == 5) {     te3p[m][mm]= 0.107720; } 
	  if(m==35 && mm == 7) {     te3p[m][mm]= 0.029641; } 
	  if(m==35 && mm == 9) {     te3p[m][mm]= 0.290347; } 
	  if(m==35 && mm == 12) {     te3p[m][mm]= 0.172659; } 
	  if(m==35 && mm == 17) {     te3p[m][mm]= 0.115458; } 
	  if(m==35 && mm == 18) {     te3p[m][mm]= 0.067662; } 
	  if(m==35 && mm == 4) {     tw0n[m][mm]= 0.492234; } 
	  if(m==35 && mm == 9) {     tw0n[m][mm]= 0.091455; } 
	  if(m==35 && mm == 19) {     tw0n[m][mm]= 0.043509; } 
	  if(m==35 && mm == 20) {     tw0n[m][mm]= 0.169806; } 
	  if(m==35 && mm == 7) {     tw1n[m][mm]= 0.306855; } 
	  if(m==35 && mm == 11) {     tw1n[m][mm]= 0.049584; } 
	  if(m==35 && mm == 12) {     tw1n[m][mm]= 0.060450; } 
	  if(m==35 && mm == 13) {     tw1n[m][mm]= 0.010969; } 
	  if(m==35 && mm == 21) {     tw1n[m][mm]= 0.045406; } 
	  if(m==35 && mm == 32) {     tw1n[m][mm]= 0.011449; } 
	  if(m==35 && mm == 0) {     tw2n[m][mm]= 0.099539; } 
	  if(m==35 && mm == 1) {     tw2n[m][mm]= 0.493618; } 
	  if(m==35 && mm == 4) {     tw2n[m][mm]= 0.239762; } 
	  if(m==35 && mm == 5) {     tw2n[m][mm]= 0.085066; } 
	  if(m==35 && mm == 6) {     tw2n[m][mm]= 0.662645; } 
	  if(m==35 && mm == 16) {     tw2n[m][mm]= 0.257098; } 
	  if(m==35 && mm == 20) {     tw2n[m][mm]= 0.011362; } 
	  if(m==35 && mm == 5) {     tw3n[m][mm]= 0.707851; } 
	  if(m==35 && mm == 6) {     tw3n[m][mm]= 0.406372; } 
	  if(m==35 && mm == 7) {     tw3n[m][mm]= 0.402642; } 
	  if(m==35 && mm == 8) {     tw3n[m][mm]= 0.261165; } 
	  if(m==35 && mm == 11) {     tw3n[m][mm]= 0.055598; } 
	  if(m==35 && mm == 14) {     te2n[m][mm]= 0.019647; } 
	  if(m==35 && mm == 30) {     te2n[m][mm]= 0.185018; } 
	  if(m==35 && mm == 32) {     te2n[m][mm]= 0.097179; } 
	  if(m==35 && mm == 35) {     te2n[m][mm]= 0.222609; } 
	  if(m==35 && mm == 0) {     te3n[m][mm]= 0.707661; } 
	  if(m==36 && mm == 14) {     tw0p[m][mm]= 0.112134; } 
	  if(m==36 && mm == 20) {     tw0p[m][mm]= 0.163553; } 
	  if(m==36 && mm == 21) {     tw0p[m][mm]= 0.587028; } 
	  if(m==36 && mm == 35) {     tw0p[m][mm]= 0.064339; } 
	  if(m==36 && mm == 2) {     tw1p[m][mm]= 0.112188; } 
	  if(m==36 && mm == 5) {     tw1p[m][mm]= 0.405007; } 
	  if(m==36 && mm == 8) {     tw1p[m][mm]= 0.154230; } 
	  if(m==36 && mm == 20) {     tw1p[m][mm]= 0.078162; } 
	  if(m==36 && mm == 21) {     tw1p[m][mm]= 0.807145; } 
	  if(m==36 && mm == 22) {     tw1p[m][mm]= 0.018166; } 
	  if(m==36 && mm == 23) {     tw1p[m][mm]= 0.390105; } 
	  if(m==36 && mm == 24) {     tw1p[m][mm]= 0.482472; } 
	  if(m==36 && mm == 26) {     tw1p[m][mm]= 0.449873; } 
	  if(m==36 && mm == 27) {     tw1p[m][mm]= 0.070182; } 
	  if(m==36 && mm == 28) {     tw1p[m][mm]= 0.459859; } 
	  if(m==36 && mm == 29) {     tw1p[m][mm]= 0.681346; } 
	  if(m==36 && mm == 30) {     tw1p[m][mm]= 0.574391; } 
	  if(m==36 && mm == 31) {     tw1p[m][mm]= 0.398177; } 
	  if(m==36 && mm == 32) {     tw1p[m][mm]= 0.733471; } 
	  if(m==36 && mm == 33) {     tw1p[m][mm]= 0.102483; } 
	  if(m==36 && mm == 34) {     tw1p[m][mm]= 0.258341; } 
	  if(m==36 && mm == 35) {     tw1p[m][mm]= 0.189312; } 
	  if(m==36 && mm == 6) {     tw2p[m][mm]= 0.117696; } 
	  if(m==36 && mm == 7) {     tw2p[m][mm]= 0.263276; } 
	  if(m==36 && mm == 8) {     tw2p[m][mm]= 0.311714; } 
	  if(m==36 && mm == 9) {     tw2p[m][mm]= 0.844315; } 
	  if(m==36 && mm == 0) {     tw3p[m][mm]= 0.146273; } 
	  if(m==36 && mm == 1) {     tw3p[m][mm]= 0.112546; } 
	  if(m==36 && mm == 5) {     tw3p[m][mm]= 0.264704; } 
	  if(m==36 && mm == 7) {     tw3p[m][mm]= 0.267192; } 
	  if(m==36 && mm == 8) {     tw3p[m][mm]= 0.049928; } 
	  if(m==36 && mm == 10) {     tw3p[m][mm]= 0.255807; } 
	  if(m==36 && mm == 13) {     tw3p[m][mm]= 0.113357; } 
	  if(m==36 && mm == 18) {     tw3p[m][mm]= 0.098063; } 
	  if(m==36 && mm == 19) {     tw3p[m][mm]= 0.179706; } 
	  if(m==36 && mm == 21) {     tw3p[m][mm]= 0.018897; } 
	  if(m==36 && mm == 23) {     tw3p[m][mm]= 0.408847; } 
	  if(m==36 && mm == 24) {     tw3p[m][mm]= 3.695403; } 
	  if(m==36 && mm == 25) {     tw3p[m][mm]= 3.160094; } 
	  if(m==36 && mm == 26) {     tw3p[m][mm]= 4.138298; } 
	  if(m==36 && mm == 27) {     tw3p[m][mm]= 3.650518; } 
	  if(m==36 && mm == 28) {     tw3p[m][mm]= 3.555481; } 
	  if(m==36 && mm == 30) {     tw3p[m][mm]= 3.747621; } 
	  if(m==36 && mm == 31) {     tw3p[m][mm]= 3.453882; } 
	  if(m==36 && mm == 32) {     tw3p[m][mm]= 4.097270; } 
	  if(m==36 && mm == 33) {     tw3p[m][mm]= 3.916296; } 
	  if(m==36 && mm == 35) {     tw3p[m][mm]= 5.759182; } 
	  if(m==36 && mm == 0) {     te2p[m][mm]= 0.313653; } 
	  if(m==36 && mm == 1) {     te2p[m][mm]= 0.510070; } 
	  if(m==36 && mm == 2) {     te2p[m][mm]= 0.310011; } 
	  if(m==36 && mm == 5) {     te2p[m][mm]= 0.009306; } 
	  if(m==36 && mm == 6) {     te2p[m][mm]= 0.154326; } 
	  if(m==36 && mm == 7) {     te2p[m][mm]= 0.299617; } 
	  if(m==36 && mm == 8) {     te2p[m][mm]= 0.301384; } 
	  if(m==36 && mm == 10) {     te2p[m][mm]= 0.151385; } 
	  if(m==36 && mm == 11) {     te2p[m][mm]= 0.031972; } 
	  if(m==36 && mm == 12) {     te2p[m][mm]= 0.192802; } 
	  if(m==36 && mm == 22) {     te2p[m][mm]= 0.182922; } 
	  if(m==36 && mm == 24) {     te2p[m][mm]= 0.014405; } 
	  if(m==36 && mm == 31) {     te2p[m][mm]= 0.313619; } 
	  if(m==36 && mm == 7) {     te3p[m][mm]= 0.644260; } 
	  if(m==36 && mm == 11) {     te3p[m][mm]= 0.335410; } 
	  if(m==36 && mm == 13) {     te3p[m][mm]= 0.131153; } 
	  if(m==36 && mm == 14) {     te3p[m][mm]= 0.233365; } 
	  if(m==36 && mm == 16) {     te3p[m][mm]= 0.316758; } 
	  if(m==36 && mm == 20) {     te3p[m][mm]= 0.057980; } 
	  if(m==36 && mm == 0) {     tw0n[m][mm]= 0.009544; } 
	  if(m==36 && mm == 2) {     tw0n[m][mm]= 0.199774; } 
	  if(m==36 && mm == 4) {     tw0n[m][mm]= 0.260625; } 
	  if(m==36 && mm == 5) {     tw0n[m][mm]= 0.309622; } 
	  if(m==36 && mm == 6) {     tw0n[m][mm]= 0.180039; } 
	  if(m==36 && mm == 7) {     tw0n[m][mm]= 0.437339; } 
	  if(m==36 && mm == 10) {     tw0n[m][mm]= 0.223175; } 
	  if(m==36 && mm == 11) {     tw0n[m][mm]= 0.086119; } 
	  if(m==36 && mm == 12) {     tw0n[m][mm]= 0.145269; } 
	  if(m==36 && mm == 20) {     tw0n[m][mm]= 0.015890; } 
	  if(m==36 && mm == 21) {     tw0n[m][mm]= 0.108587; } 
	  if(m==36 && mm == 8) {     tw1n[m][mm]= 0.411512; } 
	  if(m==36 && mm == 9) {     tw1n[m][mm]= 0.254924; } 
	  if(m==36 && mm == 11) {     tw1n[m][mm]= 0.059351; } 
	  if(m==36 && mm == 22) {     tw1n[m][mm]= 0.311903; } 
	  if(m==36 && mm == 23) {     tw1n[m][mm]= 0.070683; } 
	  if(m==36 && mm == 32) {     tw1n[m][mm]= 0.281898; } 
	  if(m==36 && mm == 35) {     tw1n[m][mm]= 0.008642; } 
	  if(m==36 && mm == 4) {     tw2n[m][mm]= 0.015383; } 
	  if(m==36 && mm == 6) {     tw2n[m][mm]= 0.100816; } 
	  if(m==36 && mm == 7) {     tw2n[m][mm]= 0.291286; } 
	  if(m==36 && mm == 0) {     tw3n[m][mm]= 0.166944; } 
	  if(m==36 && mm == 1) {     tw3n[m][mm]= 0.073958; } 
	  if(m==36 && mm == 4) {     tw3n[m][mm]= 0.053945; } 
	  if(m==36 && mm == 5) {     tw3n[m][mm]= 0.079252; } 
	  if(m==36 && mm == 6) {     tw3n[m][mm]= 0.000154; } 
	  if(m==36 && mm == 7) {     tw3n[m][mm]= 0.279093; } 
	  if(m==36 && mm == 8) {     tw3n[m][mm]= 0.073432; } 
	  if(m==36 && mm == 10) {     tw3n[m][mm]= 0.209110; } 
	  if(m==36 && mm == 11) {     tw3n[m][mm]= 0.014512; } 
	  if(m==36 && mm == 19) {     tw3n[m][mm]= 0.026883; } 
	  if(m==36 && mm == 23) {     tw3n[m][mm]= 0.080148; } 
	  if(m==36 && mm == 24) {     tw3n[m][mm]= 3.541289; } 
	  if(m==36 && mm == 25) {     tw3n[m][mm]= 3.078073; } 
	  if(m==36 && mm == 26) {     tw3n[m][mm]= 3.912242; } 
	  if(m==36 && mm == 30) {     te2n[m][mm]= 0.105146; } 
	  if(m==36 && mm == 31) {     te2n[m][mm]= 0.589783; } 
	  if(m==36 && mm == 32) {     te2n[m][mm]= 0.143194; } 
	  if(m==36 && mm == 33) {     te2n[m][mm]= 0.130876; } 
	  if(m==36 && mm == 24) {     te3n[m][mm]= 0.149356; } 
	  if(m==36 && mm == 27) {     te3n[m][mm]= 0.133805; } 
	  if(m==36 && mm == 29) {     te3n[m][mm]= 0.113066; } 
	  if(m==36 && mm == 30) {     te3n[m][mm]= 0.464306; } 
	  if(m==36 && mm == 31) {     te3n[m][mm]= 0.613656; } 
	  if(m==36 && mm == 32) {     te3n[m][mm]= 0.241556; } 
	  if(m==36 && mm == 33) {     te3n[m][mm]= 0.151990; } 
	  if(m==36 && mm == 34) {     te3n[m][mm]= 0.489334; } 
	  if(m==37 && mm == 8) {     tw0p[m][mm]= 0.013595; } 
	  if(m==37 && mm == 9) {     tw0p[m][mm]= 0.084230; } 
	  if(m==37 && mm == 22) {     tw0p[m][mm]= 0.342532; } 
	  if(m==37 && mm == 0) {     tw1p[m][mm]= 0.186782; } 
	  if(m==37 && mm == 1) {     tw1p[m][mm]= 0.176811; } 
	  if(m==37 && mm == 21) {     tw1p[m][mm]= 0.007309; } 
	  if(m==37 && mm == 22) {     tw1p[m][mm]= 0.534992; } 
	  if(m==37 && mm == 25) {     tw1p[m][mm]= 0.125788; } 
	  if(m==37 && mm == 26) {     tw1p[m][mm]= 0.188883; } 
	  if(m==37 && mm == 27) {     tw1p[m][mm]= 0.729989; } 
	  if(m==37 && mm == 30) {     tw1p[m][mm]= 0.368857; } 
	  if(m==37 && mm == 31) {     tw1p[m][mm]= 0.169613; } 
	  if(m==37 && mm == 33) {     tw1p[m][mm]= 0.386692; } 
	  if(m==37 && mm == 34) {     tw1p[m][mm]= 0.063762; } 
	  if(m==37 && mm == 35) {     tw1p[m][mm]= 0.641711; } 
	  if(m==37 && mm == 6) {     tw2p[m][mm]= 0.250627; } 
	  if(m==37 && mm == 9) {     tw2p[m][mm]= 0.504453; } 
	  if(m==37 && mm == 15) {     tw2p[m][mm]= 0.212431; } 
	  if(m==37 && mm == 0) {     tw3p[m][mm]= 0.139812; } 
	  if(m==37 && mm == 2) {     tw3p[m][mm]= 0.174617; } 
	  if(m==37 && mm == 3) {     tw3p[m][mm]= 0.296422; } 
	  if(m==37 && mm == 5) {     tw3p[m][mm]= 0.125133; } 
	  if(m==37 && mm == 6) {     tw3p[m][mm]= 0.022629; } 
	  if(m==37 && mm == 7) {     tw3p[m][mm]= 0.159952; } 
	  if(m==37 && mm == 11) {     tw3p[m][mm]= 0.259721; } 
	  if(m==37 && mm == 12) {     tw3p[m][mm]= 0.159136; } 
	  if(m==37 && mm == 19) {     tw3p[m][mm]= 0.358751; } 
	  if(m==37 && mm == 20) {     tw3p[m][mm]= 0.005908; } 
	  if(m==37 && mm == 22) {     tw3p[m][mm]= 1.628067; } 
	  if(m==37 && mm == 24) {     tw3p[m][mm]= 3.677918; } 
	  if(m==37 && mm == 25) {     tw3p[m][mm]= 2.915667; } 
	  if(m==37 && mm == 27) {     tw3p[m][mm]= 3.944097; } 
	  if(m==37 && mm == 28) {     tw3p[m][mm]= 4.142537; } 
	  if(m==37 && mm == 29) {     tw3p[m][mm]= 4.374286; } 
	  if(m==37 && mm == 32) {     tw3p[m][mm]= 4.083276; } 
	  if(m==37 && mm == 35) {     tw3p[m][mm]= 7.890721; } 
	  if(m==37 && mm == 0) {     te2p[m][mm]= 0.673847; } 
	  if(m==37 && mm == 1) {     te2p[m][mm]= 0.712342; } 
	  if(m==37 && mm == 2) {     te2p[m][mm]= 0.183976; } 
	  if(m==37 && mm == 3) {     te2p[m][mm]= 0.080650; } 
	  if(m==37 && mm == 4) {     te2p[m][mm]= 0.116416; } 
	  if(m==37 && mm == 6) {     te2p[m][mm]= 0.049779; } 
	  if(m==37 && mm == 8) {     te2p[m][mm]= 0.105217; } 
	  if(m==37 && mm == 9) {     te2p[m][mm]= 0.046845; } 
	  if(m==37 && mm == 10) {     te2p[m][mm]= 0.066328; } 
	  if(m==37 && mm == 14) {     te2p[m][mm]= 0.050336; } 
	  if(m==37 && mm == 17) {     te2p[m][mm]= 0.081745; } 
	  if(m==37 && mm == 18) {     te2p[m][mm]= 0.408212; } 
	  if(m==37 && mm == 19) {     te2p[m][mm]= 0.012713; } 
	  if(m==37 && mm == 30) {     te2p[m][mm]= 0.096047; } 
	  if(m==37 && mm == 8) {     te3p[m][mm]= 0.332497; } 
	  if(m==37 && mm == 9) {     te3p[m][mm]= 0.217079; } 
	  if(m==37 && mm == 12) {     te3p[m][mm]= 0.108555; } 
	  if(m==37 && mm == 13) {     te3p[m][mm]= 0.003892; } 
	  if(m==37 && mm == 0) {     tw0n[m][mm]= 0.041349; } 
	  if(m==37 && mm == 3) {     tw0n[m][mm]= 0.108843; } 
	  if(m==37 && mm == 5) {     tw0n[m][mm]= 0.108128; } 
	  if(m==37 && mm == 8) {     tw0n[m][mm]= 0.029861; } 
	  if(m==37 && mm == 9) {     tw0n[m][mm]= 0.259031; } 
	  if(m==37 && mm == 11) {     tw0n[m][mm]= 0.113442; } 
	  if(m==37 && mm == 22) {     tw0n[m][mm]= 0.219245; } 
	  if(m==37 && mm == 9) {     tw1n[m][mm]= 0.052931; } 
	  if(m==37 && mm == 30) {     tw1n[m][mm]= 0.360372; } 
	  if(m==37 && mm == 35) {     tw1n[m][mm]= 0.487209; } 
	  if(m==37 && mm == 6) {     tw2n[m][mm]= 0.253903; } 
	  if(m==37 && mm == 23) {     tw2n[m][mm]= 0.067888; } 
	  if(m==37 && mm == 0) {     tw3n[m][mm]= 0.126526; } 
	  if(m==37 && mm == 2) {     tw3n[m][mm]= 0.124361; } 
	  if(m==37 && mm == 3) {     tw3n[m][mm]= 0.480772; } 
	  if(m==37 && mm == 5) {     tw3n[m][mm]= 0.317076; } 
	  if(m==37 && mm == 7) {     tw3n[m][mm]= 0.208558; } 
	  if(m==37 && mm == 11) {     tw3n[m][mm]= 0.300845; } 
	  if(m==37 && mm == 12) {     tw3n[m][mm]= 0.088666; } 
	  if(m==37 && mm == 19) {     tw3n[m][mm]= 0.071454; } 
	  if(m==37 && mm == 20) {     tw3n[m][mm]= 0.055650; } 
	  if(m==37 && mm == 24) {     tw3n[m][mm]= 3.543281; } 
	  if(m==37 && mm == 25) {     tw3n[m][mm]= 2.829499; } 
	  if(m==37 && mm == 18) {     te2n[m][mm]= 0.034562; } 
	  if(m==37 && mm == 30) {     te2n[m][mm]= 0.155466; } 
	  if(m==37 && mm == 32) {     te2n[m][mm]= 0.199446; } 
	  if(m==37 && mm == 33) {     te2n[m][mm]= 0.192838; } 
	  if(m==37 && mm == 24) {     te3n[m][mm]= 0.125057; } 
	  if(m==37 && mm == 27) {     te3n[m][mm]= 0.190155; } 
	  if(m==37 && mm == 29) {     te3n[m][mm]= 0.052699; } 
	  if(m==37 && mm == 30) {     te3n[m][mm]= 0.364591; } 
	  if(m==37 && mm == 31) {     te3n[m][mm]= 0.290348; } 
	  if(m==37 && mm == 32) {     te3n[m][mm]= 0.558055; } 
	  if(m==37 && mm == 34) {     te3n[m][mm]= 0.094201; } 
	  if(m==37 && mm == 35) {     te3n[m][mm]= 0.092023; } 
	  if(m==38 && mm == 9) {     tw0p[m][mm]= 0.236046; } 
	  if(m==38 && mm == 10) {     tw0p[m][mm]= 0.141692; } 
	  if(m==38 && mm == 11) {     tw0p[m][mm]= 0.086073; } 
	  if(m==38 && mm == 22) {     tw0p[m][mm]= 0.006160; } 
	  if(m==38 && mm == 32) {     tw0p[m][mm]= 0.004838; } 
	  if(m==38 && mm == 33) {     tw0p[m][mm]= 0.006413; } 
	  if(m==38 && mm == 22) {     tw1p[m][mm]= 0.352102; } 
	  if(m==38 && mm == 23) {     tw1p[m][mm]= 0.085027; } 
	  if(m==38 && mm == 24) {     tw1p[m][mm]= 0.398546; } 
	  if(m==38 && mm == 26) {     tw1p[m][mm]= 0.017194; } 
	  if(m==38 && mm == 28) {     tw1p[m][mm]= 0.198605; } 
	  if(m==38 && mm == 29) {     tw1p[m][mm]= 0.088210; } 
	  if(m==38 && mm == 31) {     tw1p[m][mm]= 0.246099; } 
	  if(m==38 && mm == 32) {     tw1p[m][mm]= 0.106030; } 
	  if(m==38 && mm == 33) {     tw1p[m][mm]= 0.403749; } 
	  if(m==38 && mm == 34) {     tw1p[m][mm]= 0.367354; } 
	  if(m==38 && mm == 8) {     tw2p[m][mm]= 0.057193; } 
	  if(m==38 && mm == 10) {     tw2p[m][mm]= 0.059942; } 
	  if(m==38 && mm == 14) {     tw2p[m][mm]= 0.040561; } 
	  if(m==38 && mm == 17) {     tw2p[m][mm]= 0.201076; } 
	  if(m==38 && mm == 30) {     tw2p[m][mm]= 0.377571; } 
	  if(m==38 && mm == 32) {     tw2p[m][mm]= 0.491967; } 
	  if(m==38 && mm == 34) {     tw2p[m][mm]= 0.552552; } 
	  if(m==38 && mm == 0) {     tw3p[m][mm]= 0.310404; } 
	  if(m==38 && mm == 1) {     tw3p[m][mm]= 0.044890; } 
	  if(m==38 && mm == 2) {     tw3p[m][mm]= 0.256377; } 
	  if(m==38 && mm == 6) {     tw3p[m][mm]= 0.023784; } 
	  if(m==38 && mm == 7) {     tw3p[m][mm]= 0.195779; } 
	  if(m==38 && mm == 8) {     tw3p[m][mm]= 0.158618; } 
	  if(m==38 && mm == 9) {     tw3p[m][mm]= 0.210999; } 
	  if(m==38 && mm == 10) {     tw3p[m][mm]= 0.273345; } 
	  if(m==38 && mm == 11) {     tw3p[m][mm]= 0.278550; } 
	  if(m==38 && mm == 13) {     tw3p[m][mm]= 0.136500; } 
	  if(m==38 && mm == 14) {     tw3p[m][mm]= 0.102118; } 
	  if(m==38 && mm == 16) {     tw3p[m][mm]= 0.011607; } 
	  if(m==38 && mm == 18) {     tw3p[m][mm]= 0.310113; } 
	  if(m==38 && mm == 19) {     tw3p[m][mm]= 0.000586; } 
	  if(m==38 && mm == 22) {     tw3p[m][mm]= 0.254715; } 
	  if(m==38 && mm == 23) {     tw3p[m][mm]= 0.388343; } 
	  if(m==38 && mm == 24) {     tw3p[m][mm]= 5.079229; } 
	  if(m==38 && mm == 25) {     tw3p[m][mm]= 5.293271; } 
	  if(m==38 && mm == 27) {     tw3p[m][mm]= 4.607653; } 
	  if(m==38 && mm == 28) {     tw3p[m][mm]= 5.286033; } 
	  if(m==38 && mm == 32) {     tw3p[m][mm]= 6.297039; } 
	  if(m==38 && mm == 33) {     tw3p[m][mm]= 6.121166; } 
	  if(m==38 && mm == 35) {     tw3p[m][mm]= 6.510252; } 
	  if(m==38 && mm == 0) {     te2p[m][mm]= 0.100527; } 
	  if(m==38 && mm == 1) {     te2p[m][mm]= 0.414227; } 
	  if(m==38 && mm == 2) {     te2p[m][mm]= 0.175629; } 
	  if(m==38 && mm == 4) {     te2p[m][mm]= 0.111735; } 
	  if(m==38 && mm == 5) {     te2p[m][mm]= 0.273556; } 
	  if(m==38 && mm == 7) {     te2p[m][mm]= 0.383294; } 
	  if(m==38 && mm == 9) {     te2p[m][mm]= 0.109738; } 
	  if(m==38 && mm == 10) {     te2p[m][mm]= 0.207396; } 
	  if(m==38 && mm == 12) {     te2p[m][mm]= 0.078297; } 
	  if(m==38 && mm == 14) {     te2p[m][mm]= 0.141131; } 
	  if(m==38 && mm == 15) {     te2p[m][mm]= 0.036651; } 
	  if(m==38 && mm == 24) {     te2p[m][mm]= 0.132258; } 
	  if(m==38 && mm == 26) {     te2p[m][mm]= 0.423741; } 
	  if(m==38 && mm == 30) {     te2p[m][mm]= 0.091843; } 
	  if(m==38 && mm == 31) {     te2p[m][mm]= 0.081427; } 
	  if(m==38 && mm == 6) {     te3p[m][mm]= 0.016166; } 
	  if(m==38 && mm == 12) {     te3p[m][mm]= 0.138088; } 
	  if(m==38 && mm == 13) {     te3p[m][mm]= 0.089260; } 
	  if(m==38 && mm == 16) {     te3p[m][mm]= 0.232388; } 
	  if(m==38 && mm == 18) {     te3p[m][mm]= 0.044148; } 
	  if(m==38 && mm == 20) {     te3p[m][mm]= 0.552009; } 
	  if(m==38 && mm == 0) {     tw0n[m][mm]= 0.673209; } 
	  if(m==38 && mm == 1) {     tw0n[m][mm]= 0.426120; } 
	  if(m==38 && mm == 2) {     tw0n[m][mm]= 0.918499; } 
	  if(m==38 && mm == 3) {     tw0n[m][mm]= 0.293213; } 
	  if(m==38 && mm == 5) {     tw0n[m][mm]= 0.348949; } 
	  if(m==38 && mm == 6) {     tw0n[m][mm]= 0.044829; } 
	  if(m==38 && mm == 7) {     tw0n[m][mm]= 0.364657; } 
	  if(m==38 && mm == 8) {     tw0n[m][mm]= 0.262432; } 
	  if(m==38 && mm == 9) {     tw0n[m][mm]= 0.521694; } 
	  if(m==38 && mm == 10) {     tw0n[m][mm]= 0.151386; } 
	  if(m==38 && mm == 11) {     tw0n[m][mm]= 0.359110; } 
	  if(m==38 && mm == 7) {     tw1n[m][mm]= 0.021262; } 
	  if(m==38 && mm == 13) {     tw1n[m][mm]= 0.070362; } 
	  if(m==38 && mm == 24) {     tw1n[m][mm]= 0.024082; } 
	  if(m==38 && mm == 35) {     tw1n[m][mm]= 0.429186; } 
	  if(m==38 && mm == 0) {     tw2n[m][mm]= 0.011826; } 
	  if(m==38 && mm == 1) {     tw2n[m][mm]= 0.334299; } 
	  if(m==38 && mm == 2) {     tw2n[m][mm]= 0.250348; } 
	  if(m==38 && mm == 3) {     tw2n[m][mm]= 0.756639; } 
	  if(m==38 && mm == 7) {     tw2n[m][mm]= 0.035677; } 
	  if(m==38 && mm == 17) {     tw2n[m][mm]= 0.041580; } 
	  if(m==38 && mm == 22) {     tw2n[m][mm]= 0.092391; } 
	  if(m==38 && mm == 34) {     tw2n[m][mm]= 0.187289; } 
	  if(m==38 && mm == 0) {     tw3n[m][mm]= 0.443738; } 
	  if(m==38 && mm == 1) {     tw3n[m][mm]= 0.054691; } 
	  if(m==38 && mm == 2) {     tw3n[m][mm]= 0.380038; } 
	  if(m==38 && mm == 6) {     tw3n[m][mm]= 0.066245; } 
	  if(m==38 && mm == 7) {     tw3n[m][mm]= 0.106730; } 
	  if(m==38 && mm == 8) {     tw3n[m][mm]= 0.169507; } 
	  if(m==38 && mm == 9) {     tw3n[m][mm]= 0.320100; } 
	  if(m==38 && mm == 10) {     tw3n[m][mm]= 0.398273; } 
	  if(m==38 && mm == 11) {     tw3n[m][mm]= 0.336038; } 
	  if(m==38 && mm == 13) {     tw3n[m][mm]= 0.039964; } 
	  if(m==38 && mm == 14) {     tw3n[m][mm]= 0.170833; } 
	  if(m==38 && mm == 18) {     tw3n[m][mm]= 0.168486; } 
	  if(m==38 && mm == 21) {     tw3n[m][mm]= 0.712234; } 
	  if(m==38 && mm == 24) {     tw3n[m][mm]= 4.975342; } 
	  if(m==38 && mm == 25) {     tw3n[m][mm]= 5.170681; } 
	  if(m==38 && mm == 27) {     tw3n[m][mm]= 4.243591; } 
	  if(m==38 && mm == 26) {     te2n[m][mm]= 0.398526; } 
	  if(m==38 && mm == 28) {     te2n[m][mm]= 0.199960; } 
	  if(m==38 && mm == 29) {     te2n[m][mm]= 0.029823; } 
	  if(m==38 && mm == 30) {     te2n[m][mm]= 0.226579; } 
	  if(m==38 && mm == 32) {     te2n[m][mm]= 0.469470; } 
	  if(m==38 && mm == 33) {     te2n[m][mm]= 0.967483; } 
	  if(m==38 && mm == 34) {     te2n[m][mm]= 0.833708; } 
	  if(m==39 && mm == 8) {     tw0p[m][mm]= 0.198777; } 
	  if(m==39 && mm == 9) {     tw0p[m][mm]= 0.111723; } 
	  if(m==39 && mm == 11) {     tw0p[m][mm]= 0.150952; } 
	  if(m==39 && mm == 21) {     tw0p[m][mm]= 0.045252; } 
	  if(m==39 && mm == 31) {     tw0p[m][mm]= 0.108087; } 
	  if(m==39 && mm == 32) {     tw0p[m][mm]= 0.132580; } 
	  if(m==39 && mm == 33) {     tw0p[m][mm]= 0.697457; } 
	  if(m==39 && mm == 34) {     tw0p[m][mm]= 0.654553; } 
	  if(m==39 && mm == 1) {     tw1p[m][mm]= 0.179355; } 
	  if(m==39 && mm == 24) {     tw1p[m][mm]= 0.168434; } 
	  if(m==39 && mm == 29) {     tw1p[m][mm]= 0.205638; } 
	  if(m==39 && mm == 30) {     tw1p[m][mm]= 0.068591; } 
	  if(m==39 && mm == 31) {     tw1p[m][mm]= 0.093837; } 
	  if(m==39 && mm == 32) {     tw1p[m][mm]= 0.628053; } 
	  if(m==39 && mm == 33) {     tw1p[m][mm]= 0.323486; } 
	  if(m==39 && mm == 34) {     tw1p[m][mm]= 0.150934; } 
	  if(m==39 && mm == 35) {     tw1p[m][mm]= 0.297518; } 
	  if(m==39 && mm == 10) {     tw2p[m][mm]= 0.096534; } 
	  if(m==39 && mm == 11) {     tw2p[m][mm]= 0.079799; } 
	  if(m==39 && mm == 16) {     tw2p[m][mm]= 0.170693; } 
	  if(m==39 && mm == 17) {     tw2p[m][mm]= 0.133065; } 
	  if(m==39 && mm == 18) {     tw2p[m][mm]= 0.067241; } 
	  if(m==39 && mm == 19) {     tw2p[m][mm]= 0.222585; } 
	  if(m==39 && mm == 22) {     tw2p[m][mm]= 0.415630; } 
	  if(m==39 && mm == 28) {     tw2p[m][mm]= 0.426417; } 
	  if(m==39 && mm == 29) {     tw2p[m][mm]= 0.137517; } 
	  if(m==39 && mm == 30) {     tw2p[m][mm]= 0.106896; } 
	  if(m==39 && mm == 31) {     tw2p[m][mm]= 0.118640; } 
	  if(m==39 && mm == 32) {     tw2p[m][mm]= 0.129105; } 
	  if(m==39 && mm == 33) {     tw2p[m][mm]= 0.031930; } 
	  if(m==39 && mm == 34) {     tw2p[m][mm]= 0.460152; } 
	  if(m==39 && mm == 1) {     tw3p[m][mm]= 0.310438; } 
	  if(m==39 && mm == 10) {     tw3p[m][mm]= 0.039270; } 
	  if(m==39 && mm == 12) {     tw3p[m][mm]= 0.195668; } 
	  if(m==39 && mm == 13) {     tw3p[m][mm]= 0.077129; } 
	  if(m==39 && mm == 18) {     tw3p[m][mm]= 0.305863; } 
	  if(m==39 && mm == 22) {     tw3p[m][mm]= 0.244881; } 
	  if(m==39 && mm == 23) {     tw3p[m][mm]= 0.511174; } 
	  if(m==39 && mm == 24) {     tw3p[m][mm]= 4.886227; } 
	  if(m==39 && mm == 25) {     tw3p[m][mm]= 4.946846; } 
	  if(m==39 && mm == 28) {     tw3p[m][mm]= 5.623592; } 
	  if(m==39 && mm == 31) {     tw3p[m][mm]= 5.353895; } 
	  if(m==39 && mm == 32) {     tw3p[m][mm]= 5.963889; } 
	  if(m==39 && mm == 35) {     tw3p[m][mm]= 6.553385; } 
	  if(m==39 && mm == 0) {     te2p[m][mm]= 0.450234; } 
	  if(m==39 && mm == 1) {     te2p[m][mm]= 0.049458; } 
	  if(m==39 && mm == 2) {     te2p[m][mm]= 0.377195; } 
	  if(m==39 && mm == 6) {     te2p[m][mm]= 0.376783; } 
	  if(m==39 && mm == 7) {     te2p[m][mm]= 0.180808; } 
	  if(m==39 && mm == 8) {     te2p[m][mm]= 0.164903; } 
	  if(m==39 && mm == 9) {     te2p[m][mm]= 0.262230; } 
	  if(m==39 && mm == 10) {     te2p[m][mm]= 0.097254; } 
	  if(m==39 && mm == 12) {     te2p[m][mm]= 0.006132; } 
	  if(m==39 && mm == 13) {     te2p[m][mm]= 0.109144; } 
	  if(m==39 && mm == 15) {     te2p[m][mm]= 0.514420; } 
	  if(m==39 && mm == 17) {     te2p[m][mm]= 0.302635; } 
	  if(m==39 && mm == 22) {     te2p[m][mm]= 0.019236; } 
	  if(m==39 && mm == 30) {     te2p[m][mm]= 0.048896; } 
	  if(m==39 && mm == 13) {     te3p[m][mm]= 0.049060; } 
	  if(m==39 && mm == 0) {     tw0n[m][mm]= 0.145115; } 
	  if(m==39 && mm == 1) {     tw0n[m][mm]= 0.469811; } 
	  if(m==39 && mm == 2) {     tw0n[m][mm]= 0.143452; } 
	  if(m==39 && mm == 3) {     tw0n[m][mm]= 0.065002; } 
	  if(m==39 && mm == 4) {     tw0n[m][mm]= 0.323483; } 
	  if(m==39 && mm == 5) {     tw0n[m][mm]= 0.319924; } 
	  if(m==39 && mm == 7) {     tw0n[m][mm]= 0.345894; } 
	  if(m==39 && mm == 8) {     tw0n[m][mm]= 0.403628; } 
	  if(m==39 && mm == 9) {     tw0n[m][mm]= 0.187220; } 
	  if(m==39 && mm == 11) {     tw0n[m][mm]= 0.432262; } 
	  if(m==39 && mm == 13) {     tw1n[m][mm]= 0.036622; } 
	  if(m==39 && mm == 32) {     tw1n[m][mm]= 0.190448; } 
	  if(m==39 && mm == 33) {     tw1n[m][mm]= 0.179008; } 
	  if(m==39 && mm == 35) {     tw1n[m][mm]= 0.032902; } 
	  if(m==39 && mm == 0) {     tw2n[m][mm]= 0.103960; } 
	  if(m==39 && mm == 2) {     tw2n[m][mm]= 0.022916; } 
	  if(m==39 && mm == 5) {     tw2n[m][mm]= 0.045244; } 
	  if(m==39 && mm == 14) {     tw2n[m][mm]= 0.043431; } 
	  if(m==39 && mm == 16) {     tw2n[m][mm]= 0.261094; } 
	  if(m==39 && mm == 17) {     tw2n[m][mm]= 0.062580; } 
	  if(m==39 && mm == 22) {     tw2n[m][mm]= 0.265538; } 
	  if(m==39 && mm == 23) {     tw2n[m][mm]= 0.040533; } 
	  if(m==39 && mm == 34) {     tw2n[m][mm]= 0.252655; } 
	  if(m==39 && mm == 1) {     tw3n[m][mm]= 0.378243; } 
	  if(m==39 && mm == 2) {     tw3n[m][mm]= 0.002140; } 
	  if(m==39 && mm == 3) {     tw3n[m][mm]= 0.017730; } 
	  if(m==39 && mm == 7) {     tw3n[m][mm]= 0.013484; } 
	  if(m==39 && mm == 10) {     tw3n[m][mm]= 0.068548; } 
	  if(m==39 && mm == 11) {     tw3n[m][mm]= 0.044912; } 
	  if(m==39 && mm == 15) {     tw3n[m][mm]= 0.017940; } 
	  if(m==39 && mm == 18) {     tw3n[m][mm]= 0.284766; } 
	  if(m==39 && mm == 23) {     tw3n[m][mm]= 0.218488; } 
	  if(m==39 && mm == 24) {     tw3n[m][mm]= 4.817142; } 
	  if(m==39 && mm == 25) {     tw3n[m][mm]= 4.973940; } 
	  if(m==39 && mm == 25) {     te2n[m][mm]= 0.097834; } 
	  if(m==39 && mm == 28) {     te2n[m][mm]= 0.224341; } 
	  if(m==39 && mm == 32) {     te2n[m][mm]= 1.239396; } 
	  if(m==39 && mm == 33) {     te2n[m][mm]= 0.781821; } 
	  if(m==39 && mm == 34) {     te2n[m][mm]= 0.216200; } 
	  if(m==39 && mm == 35) {     te2n[m][mm]= 0.860981; } 
	  if(m==40 && mm == 10) {     tw0p[m][mm]= 0.159833; } 
	  if(m==40 && mm == 26) {     tw0p[m][mm]= 0.010523; } 
	  if(m==40 && mm == 29) {     tw0p[m][mm]= 0.056475; } 
	  if(m==40 && mm == 31) {     tw0p[m][mm]= 0.000805; } 
	  if(m==40 && mm == 0) {     tw1p[m][mm]= 0.126261; } 
	  if(m==40 && mm == 1) {     tw1p[m][mm]= 0.127741; } 
	  if(m==40 && mm == 4) {     tw1p[m][mm]= 0.124464; } 
	  if(m==40 && mm == 5) {     tw1p[m][mm]= 0.065097; } 
	  if(m==40 && mm == 20) {     tw1p[m][mm]= 0.453670; } 
	  if(m==40 && mm == 21) {     tw1p[m][mm]= 0.080814; } 
	  if(m==40 && mm == 23) {     tw1p[m][mm]= 0.040940; } 
	  if(m==40 && mm == 27) {     tw1p[m][mm]= 0.447950; } 
	  if(m==40 && mm == 31) {     tw1p[m][mm]= 0.030042; } 
	  if(m==40 && mm == 32) {     tw1p[m][mm]= 0.252606; } 
	  if(m==40 && mm == 34) {     tw1p[m][mm]= 0.318980; } 
	  if(m==40 && mm == 35) {     tw1p[m][mm]= 0.246649; } 
	  if(m==40 && mm == 9) {     tw2p[m][mm]= 0.113598; } 
	  if(m==40 && mm == 10) {     tw2p[m][mm]= 0.688897; } 
	  if(m==40 && mm == 11) {     tw2p[m][mm]= 0.169788; } 
	  if(m==40 && mm == 13) {     tw2p[m][mm]= 0.082238; } 
	  if(m==40 && mm == 17) {     tw2p[m][mm]= 0.294439; } 
	  if(m==40 && mm == 18) {     tw2p[m][mm]= 0.275879; } 
	  if(m==40 && mm == 20) {     tw2p[m][mm]= 0.204516; } 
	  if(m==40 && mm == 29) {     tw2p[m][mm]= 0.257427; } 
	  if(m==40 && mm == 30) {     tw2p[m][mm]= 0.164408; } 
	  if(m==40 && mm == 31) {     tw2p[m][mm]= 0.333103; } 
	  if(m==40 && mm == 32) {     tw2p[m][mm]= 0.037404; } 
	  if(m==40 && mm == 33) {     tw2p[m][mm]= 0.021068; } 
	  if(m==40 && mm == 3) {     tw3p[m][mm]= 0.034235; } 
	  if(m==40 && mm == 4) {     tw3p[m][mm]= 0.097562; } 
	  if(m==40 && mm == 5) {     tw3p[m][mm]= 0.194940; } 
	  if(m==40 && mm == 6) {     tw3p[m][mm]= 0.273422; } 
	  if(m==40 && mm == 7) {     tw3p[m][mm]= 0.276628; } 
	  if(m==40 && mm == 8) {     tw3p[m][mm]= 0.334020; } 
	  if(m==40 && mm == 9) {     tw3p[m][mm]= 0.193451; } 
	  if(m==40 && mm == 11) {     tw3p[m][mm]= 0.103391; } 
	  if(m==40 && mm == 29) {     tw3p[m][mm]= 2.191999; } 
	  if(m==40 && mm == 0) {     te2p[m][mm]= 0.435837; } 
	  if(m==40 && mm == 2) {     te2p[m][mm]= 0.374269; } 
	  if(m==40 && mm == 3) {     te2p[m][mm]= 0.838601; } 
	  if(m==40 && mm == 6) {     te2p[m][mm]= 0.197982; } 
	  if(m==40 && mm == 7) {     te2p[m][mm]= 0.122009; } 
	  if(m==40 && mm == 8) {     te2p[m][mm]= 0.386962; } 
	  if(m==40 && mm == 9) {     te2p[m][mm]= 0.387496; } 
	  if(m==40 && mm == 10) {     te2p[m][mm]= 0.194153; } 
	  if(m==40 && mm == 11) {     te2p[m][mm]= 0.214011; } 
	  if(m==40 && mm == 14) {     te2p[m][mm]= 0.545788; } 
	  if(m==40 && mm == 20) {     te2p[m][mm]= 0.048563; } 
	  if(m==40 && mm == 21) {     te2p[m][mm]= 0.007232; } 
	  if(m==40 && mm == 6) {     te3p[m][mm]= 0.746505; } 
	  if(m==40 && mm == 8) {     te3p[m][mm]= 0.142572; } 
	  if(m==40 && mm == 9) {     te3p[m][mm]= 0.063959; } 
	  if(m==40 && mm == 10) {     te3p[m][mm]= 0.133372; } 
	  if(m==40 && mm == 16) {     te3p[m][mm]= 0.006681; } 
	  if(m==40 && mm == 0) {     tw0n[m][mm]= 0.311826; } 
	  if(m==40 && mm == 2) {     tw0n[m][mm]= 0.155812; } 
	  if(m==40 && mm == 3) {     tw0n[m][mm]= 0.090193; } 
	  if(m==40 && mm == 6) {     tw0n[m][mm]= 0.094369; } 
	  if(m==40 && mm == 7) {     tw0n[m][mm]= 0.365578; } 
	  if(m==40 && mm == 11) {     tw0n[m][mm]= 0.119645; } 
	  if(m==40 && mm == 14) {     tw0n[m][mm]= 0.142447; } 
	  if(m==40 && mm == 29) {     tw0n[m][mm]= 0.076311; } 
	  if(m==40 && mm == 7) {     tw1n[m][mm]= 0.018765; } 
	  if(m==40 && mm == 8) {     tw1n[m][mm]= 2.070451; } 
	  if(m==40 && mm == 20) {     tw1n[m][mm]= 0.214678; } 
	  if(m==40 && mm == 22) {     tw1n[m][mm]= 0.784704; } 
	  if(m==40 && mm == 34) {     tw1n[m][mm]= 0.059030; } 
	  if(m==40 && mm == 1) {     tw2n[m][mm]= 0.236834; } 
	  if(m==40 && mm == 2) {     tw2n[m][mm]= 0.049669; } 
	  if(m==40 && mm == 20) {     tw2n[m][mm]= 0.014968; } 
	  if(m==40 && mm == 30) {     tw2n[m][mm]= 0.075320; } 
	  if(m==40 && mm == 1) {     tw3n[m][mm]= 0.017609; } 
	  if(m==40 && mm == 2) {     tw3n[m][mm]= 0.117201; } 
	  if(m==40 && mm == 3) {     tw3n[m][mm]= 0.135412; } 
	  if(m==40 && mm == 4) {     tw3n[m][mm]= 0.345894; } 
	  if(m==40 && mm == 5) {     tw3n[m][mm]= 0.289631; } 
	  if(m==40 && mm == 6) {     tw3n[m][mm]= 0.468353; } 
	  if(m==40 && mm == 7) {     tw3n[m][mm]= 0.473834; } 
	  if(m==40 && mm == 8) {     tw3n[m][mm]= 0.309906; } 
	  if(m==40 && mm == 9) {     tw3n[m][mm]= 0.274201; } 
	  if(m==40 && mm == 10) {     tw3n[m][mm]= 0.090807; } 
	  if(m==40 && mm == 11) {     tw3n[m][mm]= 0.146077; } 
	  if(m==40 && mm == 10) {     te2n[m][mm]= 0.016944; } 
	  if(m==40 && mm == 11) {     te2n[m][mm]= 0.008064; } 
	  if(m==40 && mm == 20) {     te2n[m][mm]= 0.000066; } 
	  if(m==40 && mm == 21) {     te2n[m][mm]= 0.013163; } 
	  if(m==40 && mm == 32) {     te2n[m][mm]= 0.508664; } 
	  if(m==40 && mm == 34) {     te2n[m][mm]= 0.379669; } 
	  if(m==40 && mm == 24) {     te3n[m][mm]= 0.703960; } 
	  if(m==40 && mm == 25) {     te3n[m][mm]= 0.073094; } 
	  if(m==40 && mm == 30) {     te3n[m][mm]= 0.063374; } 
	  if(m==40 && mm == 33) {     te3n[m][mm]= 0.424976; } 
	  if(m==40 && mm == 34) {     te3n[m][mm]= 0.199608; } 
	  if(m==41 && mm == 0) {     tw1p[m][mm]= 0.603363; } 
	  if(m==41 && mm == 2) {     tw1p[m][mm]= 0.013526; } 
	  if(m==41 && mm == 20) {     tw1p[m][mm]= 0.076553; } 
	  if(m==41 && mm == 22) {     tw1p[m][mm]= 0.015967; } 
	  if(m==41 && mm == 24) {     tw1p[m][mm]= 0.248256; } 
	  if(m==41 && mm == 26) {     tw1p[m][mm]= 0.086886; } 
	  if(m==41 && mm == 27) {     tw1p[m][mm]= 0.124254; } 
	  if(m==41 && mm == 30) {     tw1p[m][mm]= 0.047876; } 
	  if(m==41 && mm == 31) {     tw1p[m][mm]= 0.105755; } 
	  if(m==41 && mm == 32) {     tw1p[m][mm]= 0.491318; } 
	  if(m==41 && mm == 33) {     tw1p[m][mm]= 0.032383; } 
	  if(m==41 && mm == 34) {     tw1p[m][mm]= 0.027651; } 
	  if(m==41 && mm == 35) {     tw1p[m][mm]= 0.278900; } 
	  if(m==41 && mm == 11) {     tw2p[m][mm]= 0.031047; } 
	  if(m==41 && mm == 16) {     tw2p[m][mm]= 0.114383; } 
	  if(m==41 && mm == 18) {     tw2p[m][mm]= 0.024990; } 
	  if(m==41 && mm == 20) {     tw2p[m][mm]= 0.074513; } 
	  if(m==41 && mm == 21) {     tw2p[m][mm]= 0.030355; } 
	  if(m==41 && mm == 29) {     tw2p[m][mm]= 0.101174; } 
	  if(m==41 && mm == 30) {     tw2p[m][mm]= 0.004103; } 
	  if(m==41 && mm == 31) {     tw2p[m][mm]= 0.328949; } 
	  if(m==41 && mm == 32) {     tw2p[m][mm]= 0.427491; } 
	  if(m==41 && mm == 33) {     tw2p[m][mm]= 0.429176; } 
	  if(m==41 && mm == 0) {     tw3p[m][mm]= 0.589806; } 
	  if(m==41 && mm == 6) {     tw3p[m][mm]= 0.316169; } 
	  if(m==41 && mm == 7) {     tw3p[m][mm]= 0.315281; } 
	  if(m==41 && mm == 8) {     tw3p[m][mm]= 0.222125; } 
	  if(m==41 && mm == 20) {     tw3p[m][mm]= 0.112561; } 
	  if(m==41 && mm == 28) {     tw3p[m][mm]= 2.696221; } 
	  if(m==41 && mm == 29) {     tw3p[m][mm]= 2.725898; } 
	  if(m==41 && mm == 35) {     tw3p[m][mm]= 0.079479; } 
	  if(m==41 && mm == 2) {     te2p[m][mm]= 0.181679; } 
	  if(m==41 && mm == 3) {     te2p[m][mm]= 0.298266; } 
	  if(m==41 && mm == 5) {     te2p[m][mm]= 0.199400; } 
	  if(m==41 && mm == 6) {     te2p[m][mm]= 0.389874; } 
	  if(m==41 && mm == 8) {     te2p[m][mm]= 0.354135; } 
	  if(m==41 && mm == 9) {     te2p[m][mm]= 0.002828; } 
	  if(m==41 && mm == 10) {     te2p[m][mm]= 0.061572; } 
	  if(m==41 && mm == 11) {     te2p[m][mm]= 0.062435; } 
	  if(m==41 && mm == 15) {     te2p[m][mm]= 0.064788; } 
	  if(m==41 && mm == 17) {     te2p[m][mm]= 0.017288; } 
	  if(m==41 && mm == 18) {     te2p[m][mm]= 0.246410; } 
	  if(m==41 && mm == 20) {     te2p[m][mm]= 0.263954; } 
	  if(m==41 && mm == 21) {     te2p[m][mm]= 0.001763; } 
	  if(m==41 && mm == 7) {     te3p[m][mm]= 0.799040; } 
	  if(m==41 && mm == 8) {     te3p[m][mm]= 0.107496; } 
	  if(m==41 && mm == 9) {     te3p[m][mm]= 0.049739; } 
	  if(m==41 && mm == 10) {     te3p[m][mm]= 1.021403; } 
	  if(m==41 && mm == 12) {     te3p[m][mm]= 0.059929; } 
	  if(m==41 && mm == 13) {     te3p[m][mm]= 0.038749; } 
	  if(m==41 && mm == 18) {     te3p[m][mm]= 0.149875; } 
	  if(m==41 && mm == 0) {     tw0n[m][mm]= 0.225131; } 
	  if(m==41 && mm == 1) {     tw0n[m][mm]= 0.451748; } 
	  if(m==41 && mm == 4) {     tw0n[m][mm]= 0.212830; } 
	  if(m==41 && mm == 5) {     tw0n[m][mm]= 0.285449; } 
	  if(m==41 && mm == 6) {     tw0n[m][mm]= 0.110150; } 
	  if(m==41 && mm == 7) {     tw1n[m][mm]= 0.124895; } 
	  if(m==41 && mm == 9) {     tw1n[m][mm]= 0.282292; } 
	  if(m==41 && mm == 13) {     tw1n[m][mm]= 0.046868; } 
	  if(m==41 && mm == 0) {     tw2n[m][mm]= 0.010625; } 
	  if(m==41 && mm == 2) {     tw2n[m][mm]= 0.123212; } 
	  if(m==41 && mm == 3) {     tw2n[m][mm]= 0.303862; } 
	  if(m==41 && mm == 16) {     tw2n[m][mm]= 0.039018; } 
	  if(m==41 && mm == 31) {     tw2n[m][mm]= 0.103680; } 
	  if(m==41 && mm == 32) {     tw2n[m][mm]= 0.001033; } 
	  if(m==41 && mm == 0) {     tw3n[m][mm]= 0.609599; } 
	  if(m==41 && mm == 6) {     tw3n[m][mm]= 0.289144; } 
	  if(m==41 && mm == 7) {     tw3n[m][mm]= 0.321609; } 
	  if(m==41 && mm == 8) {     tw3n[m][mm]= 0.186635; } 
	  if(m==41 && mm == 5) {     te2n[m][mm]= 0.058088; } 
	  if(m==41 && mm == 11) {     te2n[m][mm]= 0.005679; } 
	  if(m==41 && mm == 20) {     te2n[m][mm]= 0.057829; } 
	  if(m==41 && mm == 32) {     te2n[m][mm]= 0.040259; } 
	  if(m==41 && mm == 33) {     te2n[m][mm]= 0.058620; } 
	  if(m==41 && mm == 34) {     te2n[m][mm]= 0.080826; } 
	  if(m==41 && mm == 6) {     te3n[m][mm]= 0.162373; } 
	  if(m==41 && mm == 30) {     te3n[m][mm]= 0.023982; } 
	  if(m==41 && mm == 32) {     te3n[m][mm]= 0.649260; } 
	  if(m==41 && mm == 33) {     te3n[m][mm]= 0.153387; } 
	  if(m==41 && mm == 34) {     te3n[m][mm]= 1.589451; } 
	  if(m==42 && mm == 15) {     tw0p[m][mm]= 0.047870; } 
	  if(m==42 && mm == 34) {     tw0p[m][mm]= 0.098917; } 
	  if(m==42 && mm == 4) {     tw1p[m][mm]= 0.045432; } 
	  if(m==42 && mm == 5) {     tw1p[m][mm]= 0.053023; } 
	  if(m==42 && mm == 20) {     tw1p[m][mm]= 0.107087; } 
	  if(m==42 && mm == 21) {     tw1p[m][mm]= 0.395956; } 
	  if(m==42 && mm == 22) {     tw1p[m][mm]= 0.228872; } 
	  if(m==42 && mm == 25) {     tw1p[m][mm]= 0.028068; } 
	  if(m==42 && mm == 26) {     tw1p[m][mm]= 0.123270; } 
	  if(m==42 && mm == 27) {     tw1p[m][mm]= 0.303801; } 
	  if(m==42 && mm == 28) {     tw1p[m][mm]= 0.319171; } 
	  if(m==42 && mm == 30) {     tw1p[m][mm]= 0.414299; } 
	  if(m==42 && mm == 31) {     tw1p[m][mm]= 0.198462; } 
	  if(m==42 && mm == 32) {     tw1p[m][mm]= 0.084703; } 
	  if(m==42 && mm == 33) {     tw1p[m][mm]= 0.190551; } 
	  if(m==42 && mm == 34) {     tw1p[m][mm]= 0.086118; } 
	  if(m==42 && mm == 35) {     tw1p[m][mm]= 0.049515; } 
	  if(m==42 && mm == 8) {     tw2p[m][mm]= 0.533341; } 
	  if(m==42 && mm == 9) {     tw2p[m][mm]= 0.465778; } 
	  if(m==42 && mm == 16) {     tw2p[m][mm]= 0.100777; } 
	  if(m==42 && mm == 17) {     tw2p[m][mm]= 0.095067; } 
	  if(m==42 && mm == 18) {     tw2p[m][mm]= 0.207791; } 
	  if(m==42 && mm == 19) {     tw2p[m][mm]= 0.018513; } 
	  if(m==42 && mm == 20) {     tw2p[m][mm]= 0.319742; } 
	  if(m==42 && mm == 21) {     tw2p[m][mm]= 0.015584; } 
	  if(m==42 && mm == 22) {     tw2p[m][mm]= 0.513184; } 
	  if(m==42 && mm == 28) {     tw2p[m][mm]= 0.102784; } 
	  if(m==42 && mm == 30) {     tw2p[m][mm]= 0.346224; } 
	  if(m==42 && mm == 31) {     tw2p[m][mm]= 0.304662; } 
	  if(m==42 && mm == 32) {     tw2p[m][mm]= 0.398242; } 
	  if(m==42 && mm == 33) {     tw2p[m][mm]= 0.308600; } 
	  if(m==42 && mm == 34) {     tw2p[m][mm]= 1.308238; } 
	  if(m==42 && mm == 5) {     tw3p[m][mm]= 0.026118; } 
	  if(m==42 && mm == 13) {     tw3p[m][mm]= 0.147190; } 
	  if(m==42 && mm == 15) {     tw3p[m][mm]= 0.107025; } 
	  if(m==42 && mm == 16) {     tw3p[m][mm]= 0.244213; } 
	  if(m==42 && mm == 17) {     tw3p[m][mm]= 0.538457; } 
	  if(m==42 && mm == 18) {     tw3p[m][mm]= 0.358706; } 
	  if(m==42 && mm == 19) {     tw3p[m][mm]= 0.474831; } 
	  if(m==42 && mm == 20) {     tw3p[m][mm]= 0.063728; } 
	  if(m==42 && mm == 21) {     tw3p[m][mm]= 0.083934; } 
	  if(m==42 && mm == 22) {     tw3p[m][mm]= 0.446877; } 
	  if(m==42 && mm == 23) {     tw3p[m][mm]= 0.406795; } 
	  if(m==42 && mm == 24) {     tw3p[m][mm]= 3.354193; } 
	  if(m==42 && mm == 25) {     tw3p[m][mm]= 2.918133; } 
	  if(m==42 && mm == 26) {     tw3p[m][mm]= 2.972799; } 
	  if(m==42 && mm == 28) {     tw3p[m][mm]= 3.928985; } 
	  if(m==42 && mm == 30) {     tw3p[m][mm]= 4.753683; } 
	  if(m==42 && mm == 32) {     tw3p[m][mm]= 5.385206; } 
	  if(m==42 && mm == 35) {     tw3p[m][mm]= 6.383601; } 
	  if(m==42 && mm == 3) {     te2p[m][mm]= 0.147068; } 
	  if(m==42 && mm == 4) {     te2p[m][mm]= 0.006130; } 
	  if(m==42 && mm == 5) {     te2p[m][mm]= 0.197613; } 
	  if(m==42 && mm == 7) {     te2p[m][mm]= 0.514300; } 
	  if(m==42 && mm == 10) {     te2p[m][mm]= 0.018703; } 
	  if(m==42 && mm == 12) {     te2p[m][mm]= 0.133443; } 
	  if(m==42 && mm == 13) {     te2p[m][mm]= 0.102020; } 
	  if(m==42 && mm == 17) {     te2p[m][mm]= 0.063319; } 
	  if(m==42 && mm == 18) {     te2p[m][mm]= 0.355667; } 
	  if(m==42 && mm == 20) {     te2p[m][mm]= 0.088518; } 
	  if(m==42 && mm == 21) {     te2p[m][mm]= 0.410094; } 
	  if(m==42 && mm == 7) {     te3p[m][mm]= 0.119385; } 
	  if(m==42 && mm == 8) {     te3p[m][mm]= 0.114744; } 
	  if(m==42 && mm == 10) {     te3p[m][mm]= 0.089713; } 
	  if(m==42 && mm == 14) {     te3p[m][mm]= 0.186462; } 
	  if(m==42 && mm == 4) {     tw0n[m][mm]= 1.079635; } 
	  if(m==42 && mm == 9) {     tw0n[m][mm]= 0.005258; } 
	  if(m==42 && mm == 11) {     tw0n[m][mm]= 0.139962; } 
	  if(m==42 && mm == 15) {     tw0n[m][mm]= 0.141415; } 
	  if(m==42 && mm == 24) {     tw0n[m][mm]= 0.020056; } 
	  if(m==42 && mm == 8) {     tw1n[m][mm]= 0.543183; } 
	  if(m==42 && mm == 9) {     tw1n[m][mm]= 0.233733; } 
	  if(m==42 && mm == 22) {     tw1n[m][mm]= 0.018204; } 
	  if(m==42 && mm == 30) {     tw1n[m][mm]= 0.175523; } 
	  if(m==42 && mm == 0) {     tw2n[m][mm]= 0.358597; } 
	  if(m==42 && mm == 3) {     tw2n[m][mm]= 0.155125; } 
	  if(m==42 && mm == 6) {     tw2n[m][mm]= 0.215535; } 
	  if(m==42 && mm == 7) {     tw2n[m][mm]= 0.104256; } 
	  if(m==42 && mm == 8) {     tw2n[m][mm]= 0.333064; } 
	  if(m==42 && mm == 16) {     tw2n[m][mm]= 0.029625; } 
	  if(m==42 && mm == 17) {     tw2n[m][mm]= 0.100941; } 
	  if(m==42 && mm == 20) {     tw2n[m][mm]= 0.146011; } 
	  if(m==42 && mm == 22) {     tw2n[m][mm]= 0.195919; } 
	  if(m==42 && mm == 32) {     tw2n[m][mm]= 0.182435; } 
	  if(m==42 && mm == 33) {     tw2n[m][mm]= 0.105704; } 
	  if(m==42 && mm == 15) {     tw3n[m][mm]= 0.024208; } 
	  if(m==42 && mm == 16) {     tw3n[m][mm]= 0.102409; } 
	  if(m==42 && mm == 17) {     tw3n[m][mm]= 0.141948; } 
	  if(m==42 && mm == 22) {     tw3n[m][mm]= 0.205878; } 
	  if(m==42 && mm == 24) {     tw3n[m][mm]= 3.160331; } 
	  if(m==42 && mm == 25) {     tw3n[m][mm]= 2.624688; } 
	  if(m==42 && mm == 26) {     tw3n[m][mm]= 2.750485; } 
	  if(m==42 && mm == 5) {     te2n[m][mm]= 0.099713; } 
	  if(m==42 && mm == 18) {     te2n[m][mm]= 0.438090; } 
	  if(m==42 && mm == 22) {     te2n[m][mm]= 0.148119; } 
	  if(m==42 && mm == 33) {     te2n[m][mm]= 1.311472; } 
	  if(m==42 && mm == 34) {     te2n[m][mm]= 0.376998; } 
	  if(m==42 && mm == 24) {     te3n[m][mm]= 0.060988; } 
	  if(m==42 && mm == 26) {     te3n[m][mm]= 0.174342; } 
	  if(m==42 && mm == 28) {     te3n[m][mm]= 0.166002; } 
	  if(m==42 && mm == 30) {     te3n[m][mm]= 0.179262; } 
	  if(m==42 && mm == 32) {     te3n[m][mm]= 0.386434; } 
	  if(m==42 && mm == 33) {     te3n[m][mm]= 0.519059; } 
	  if(m==42 && mm == 34) {     te3n[m][mm]= 0.245455; } 
	  if(m==42 && mm == 35) {     te3n[m][mm]= 0.975364; } 
	  if(m==43 && mm == 7) {     tw0p[m][mm]= 0.256101; } 
	  if(m==43 && mm == 28) {     tw0p[m][mm]= 0.043414; } 
	  if(m==43 && mm == 34) {     tw0p[m][mm]= 0.057179; } 
	  if(m==43 && mm == 4) {     tw1p[m][mm]= 0.210426; } 
	  if(m==43 && mm == 20) {     tw1p[m][mm]= 0.085559; } 
	  if(m==43 && mm == 23) {     tw1p[m][mm]= 0.004432; } 
	  if(m==43 && mm == 24) {     tw1p[m][mm]= 0.252159; } 
	  if(m==43 && mm == 30) {     tw1p[m][mm]= 0.000436; } 
	  if(m==43 && mm == 35) {     tw1p[m][mm]= 0.041811; } 
	  if(m==43 && mm == 5) {     tw2p[m][mm]= 0.212542; } 
	  if(m==43 && mm == 7) {     tw2p[m][mm]= 0.354115; } 
	  if(m==43 && mm == 8) {     tw2p[m][mm]= 0.266631; } 
	  if(m==43 && mm == 9) {     tw2p[m][mm]= 0.497120; } 
	  if(m==43 && mm == 10) {     tw2p[m][mm]= 0.270917; } 
	  if(m==43 && mm == 18) {     tw2p[m][mm]= 0.360601; } 
	  if(m==43 && mm == 28) {     tw2p[m][mm]= 0.175323; } 
	  if(m==43 && mm == 30) {     tw2p[m][mm]= 0.312800; } 
	  if(m==43 && mm == 31) {     tw2p[m][mm]= 0.762420; } 
	  if(m==43 && mm == 32) {     tw2p[m][mm]= 0.446795; } 
	  if(m==43 && mm == 33) {     tw2p[m][mm]= 0.290968; } 
	  if(m==43 && mm == 14) {     tw3p[m][mm]= 0.034308; } 
	  if(m==43 && mm == 15) {     tw3p[m][mm]= 0.423332; } 
	  if(m==43 && mm == 16) {     tw3p[m][mm]= 0.530490; } 
	  if(m==43 && mm == 17) {     tw3p[m][mm]= 0.581258; } 
	  if(m==43 && mm == 18) {     tw3p[m][mm]= 0.060767; } 
	  if(m==43 && mm == 19) {     tw3p[m][mm]= 0.108270; } 
	  if(m==43 && mm == 20) {     tw3p[m][mm]= 0.063823; } 
	  if(m==43 && mm == 21) {     tw3p[m][mm]= 0.271089; } 
	  if(m==43 && mm == 22) {     tw3p[m][mm]= 0.325484; } 
	  if(m==43 && mm == 24) {     tw3p[m][mm]= 3.204459; } 
	  if(m==43 && mm == 25) {     tw3p[m][mm]= 3.610628; } 
	  if(m==43 && mm == 27) {     tw3p[m][mm]= 3.053636; } 
	  if(m==43 && mm == 28) {     tw3p[m][mm]= 3.689902; } 
	  if(m==43 && mm == 30) {     tw3p[m][mm]= 6.030373; } 
	  if(m==43 && mm == 35) {     tw3p[m][mm]= 6.722440; } 
	  if(m==43 && mm == 4) {     te2p[m][mm]= 0.141504; } 
	  if(m==43 && mm == 5) {     te2p[m][mm]= 0.309530; } 
	  if(m==43 && mm == 6) {     te2p[m][mm]= 0.171403; } 
	  if(m==43 && mm == 10) {     te2p[m][mm]= 0.010115; } 
	  if(m==43 && mm == 14) {     te2p[m][mm]= 0.084878; } 
	  if(m==43 && mm == 16) {     te2p[m][mm]= 0.149938; } 
	  if(m==43 && mm == 17) {     te2p[m][mm]= 0.369977; } 
	  if(m==43 && mm == 19) {     te2p[m][mm]= 0.247613; } 
	  if(m==43 && mm == 20) {     te2p[m][mm]= 0.237198; } 
	  if(m==43 && mm == 21) {     te2p[m][mm]= 0.024074; } 
	  if(m==43 && mm == 6) {     te3p[m][mm]= 0.083494; } 
	  if(m==43 && mm == 8) {     te3p[m][mm]= 0.296924; } 
	  if(m==43 && mm == 10) {     te3p[m][mm]= 0.146750; } 
	  if(m==43 && mm == 13) {     te3p[m][mm]= 0.104912; } 
	  if(m==43 && mm == 20) {     te3p[m][mm]= 0.126513; } 
	  if(m==43 && mm == 4) {     tw0n[m][mm]= 0.201587; } 
	  if(m==43 && mm == 6) {     tw0n[m][mm]= 0.170436; } 
	  if(m==43 && mm == 7) {     tw0n[m][mm]= 0.268455; } 
	  if(m==43 && mm == 9) {     tw0n[m][mm]= 0.073786; } 
	  if(m==43 && mm == 14) {     tw1n[m][mm]= 0.041243; } 
	  if(m==43 && mm == 35) {     tw1n[m][mm]= 0.024499; } 
	  if(m==43 && mm == 5) {     tw2n[m][mm]= 0.247345; } 
	  if(m==43 && mm == 6) {     tw2n[m][mm]= 0.021713; } 
	  if(m==43 && mm == 18) {     tw2n[m][mm]= 0.077855; } 
	  if(m==43 && mm == 30) {     tw2n[m][mm]= 0.022307; } 
	  if(m==43 && mm == 31) {     tw2n[m][mm]= 0.048748; } 
	  if(m==43 && mm == 32) {     tw2n[m][mm]= 0.100496; } 
	  if(m==43 && mm == 14) {     tw3n[m][mm]= 0.000071; } 
	  if(m==43 && mm == 16) {     tw3n[m][mm]= 0.108042; } 
	  if(m==43 && mm == 21) {     tw3n[m][mm]= 0.038444; } 
	  if(m==43 && mm == 22) {     tw3n[m][mm]= 0.133853; } 
	  if(m==43 && mm == 24) {     tw3n[m][mm]= 3.134651; } 
	  if(m==43 && mm == 25) {     tw3n[m][mm]= 3.453327; } 
	  if(m==43 && mm == 27) {     tw3n[m][mm]= 2.921420; } 
	  if(m==43 && mm == 17) {     te2n[m][mm]= 0.073427; } 
	  if(m==43 && mm == 20) {     te2n[m][mm]= 0.003829; } 
	  if(m==43 && mm == 33) {     te2n[m][mm]= 0.126404; } 
	  if(m==43 && mm == 34) {     te2n[m][mm]= 0.465278; } 
	  if(m==43 && mm == 35) {     te2n[m][mm]= 0.032387; } 
	  if(m==43 && mm == 20) {     te3n[m][mm]= 0.119897; } 
	  if(m==43 && mm == 23) {     te3n[m][mm]= 0.345875; } 
	  if(m==43 && mm == 25) {     te3n[m][mm]= 0.195032; } 
	  if(m==43 && mm == 27) {     te3n[m][mm]= 0.563225; } 
	  if(m==43 && mm == 28) {     te3n[m][mm]= 0.031738; } 
	  if(m==43 && mm == 30) {     te3n[m][mm]= 0.475950; } 
	  if(m==43 && mm == 31) {     te3n[m][mm]= 0.125620; } 
	  if(m==43 && mm == 32) {     te3n[m][mm]= 1.073869; } 
	  if(m==43 && mm == 33) {     te3n[m][mm]= 0.178984; } 
	  if(m==43 && mm == 34) {     te3n[m][mm]= 0.271186; } 
	  if(m==43 && mm == 35) {     te3n[m][mm]= 0.391741; } 
	  if(m==44 && mm == 8) {     tw0p[m][mm]= 0.021802; } 
	  if(m==44 && mm == 9) {     tw0p[m][mm]= 0.021278; } 
	  if(m==44 && mm == 28) {     tw0p[m][mm]= 0.176665; } 
	  if(m==44 && mm == 0) {     tw1p[m][mm]= 0.252512; } 
	  if(m==44 && mm == 4) {     tw1p[m][mm]= 0.278094; } 
	  if(m==44 && mm == 6) {     tw1p[m][mm]= 0.294299; } 
	  if(m==44 && mm == 19) {     tw1p[m][mm]= 0.042631; } 
	  if(m==44 && mm == 22) {     tw1p[m][mm]= 0.664539; } 
	  if(m==44 && mm == 23) {     tw1p[m][mm]= 0.465510; } 
	  if(m==44 && mm == 24) {     tw1p[m][mm]= 0.164230; } 
	  if(m==44 && mm == 25) {     tw1p[m][mm]= 0.030101; } 
	  if(m==44 && mm == 26) {     tw1p[m][mm]= 0.109083; } 
	  if(m==44 && mm == 28) {     tw1p[m][mm]= 0.159582; } 
	  if(m==44 && mm == 29) {     tw1p[m][mm]= 0.137837; } 
	  if(m==44 && mm == 32) {     tw1p[m][mm]= 1.025614; } 
	  if(m==44 && mm == 33) {     tw1p[m][mm]= 0.076755; } 
	  if(m==44 && mm == 34) {     tw1p[m][mm]= 0.391091; } 
	  if(m==44 && mm == 35) {     tw1p[m][mm]= 0.076991; } 
	  if(m==44 && mm == 6) {     tw2p[m][mm]= 0.175243; } 
	  if(m==44 && mm == 8) {     tw2p[m][mm]= 0.314815; } 
	  if(m==44 && mm == 12) {     tw2p[m][mm]= 0.164306; } 
	  if(m==44 && mm == 16) {     tw2p[m][mm]= 0.107368; } 
	  if(m==44 && mm == 17) {     tw2p[m][mm]= 0.119841; } 
	  if(m==44 && mm == 18) {     tw2p[m][mm]= 0.516934; } 
	  if(m==44 && mm == 29) {     tw2p[m][mm]= 0.018331; } 
	  if(m==44 && mm == 30) {     tw2p[m][mm]= 0.089583; } 
	  if(m==44 && mm == 31) {     tw2p[m][mm]= 0.228386; } 
	  if(m==44 && mm == 32) {     tw2p[m][mm]= 0.218222; } 
	  if(m==44 && mm == 33) {     tw2p[m][mm]= 0.060693; } 
	  if(m==44 && mm == 34) {     tw2p[m][mm]= 0.101278; } 
	  if(m==44 && mm == 35) {     tw2p[m][mm]= 0.674285; } 
	  if(m==44 && mm == 0) {     tw3p[m][mm]= 0.429079; } 
	  if(m==44 && mm == 1) {     tw3p[m][mm]= 0.235445; } 
	  if(m==44 && mm == 3) {     tw3p[m][mm]= 0.331010; } 
	  if(m==44 && mm == 5) {     tw3p[m][mm]= 0.984487; } 
	  if(m==44 && mm == 6) {     tw3p[m][mm]= 0.164127; } 
	  if(m==44 && mm == 7) {     tw3p[m][mm]= 0.064811; } 
	  if(m==44 && mm == 8) {     tw3p[m][mm]= 0.373247; } 
	  if(m==44 && mm == 9) {     tw3p[m][mm]= 0.320230; } 
	  if(m==44 && mm == 10) {     tw3p[m][mm]= 0.546999; } 
	  if(m==44 && mm == 11) {     tw3p[m][mm]= 0.544715; } 
	  if(m==44 && mm == 12) {     tw3p[m][mm]= 0.624450; } 
	  if(m==44 && mm == 13) {     tw3p[m][mm]= 0.299398; } 
	  if(m==44 && mm == 14) {     tw3p[m][mm]= 0.390687; } 
	  if(m==44 && mm == 15) {     tw3p[m][mm]= 0.420770; } 
	  if(m==44 && mm == 16) {     tw3p[m][mm]= 0.462574; } 
	  if(m==44 && mm == 17) {     tw3p[m][mm]= 0.197850; } 
	  if(m==44 && mm == 18) {     tw3p[m][mm]= 0.461121; } 
	  if(m==44 && mm == 19) {     tw3p[m][mm]= 0.244067; } 
	  if(m==44 && mm == 20) {     tw3p[m][mm]= 0.436766; } 
	  if(m==44 && mm == 21) {     tw3p[m][mm]= 0.285413; } 
	  if(m==44 && mm == 22) {     tw3p[m][mm]= 0.571609; } 
	  if(m==44 && mm == 23) {     tw3p[m][mm]= 0.309073; } 
	  if(m==44 && mm == 25) {     tw3p[m][mm]= 3.977300; } 
	  if(m==44 && mm == 26) {     tw3p[m][mm]= 4.198247; } 
	  if(m==44 && mm == 27) {     tw3p[m][mm]= 3.997771; } 
	  if(m==44 && mm == 29) {     tw3p[m][mm]= 3.256161; } 
	  if(m==44 && mm == 30) {     tw3p[m][mm]= 3.091446; } 
	  if(m==44 && mm == 35) {     tw3p[m][mm]= 4.542699; } 
	  if(m==44 && mm == 1) {     te2p[m][mm]= 0.111377; } 
	  if(m==44 && mm == 2) {     te2p[m][mm]= 0.203168; } 
	  if(m==44 && mm == 3) {     te2p[m][mm]= 0.610430; } 
	  if(m==44 && mm == 4) {     te2p[m][mm]= 0.365284; } 
	  if(m==44 && mm == 5) {     te2p[m][mm]= 0.219043; } 
	  if(m==44 && mm == 7) {     te2p[m][mm]= 0.287693; } 
	  if(m==44 && mm == 9) {     te2p[m][mm]= 0.165930; } 
	  if(m==44 && mm == 10) {     te2p[m][mm]= 0.095393; } 
	  if(m==44 && mm == 12) {     te2p[m][mm]= 0.091827; } 
	  if(m==44 && mm == 13) {     te2p[m][mm]= 0.085866; } 
	  if(m==44 && mm == 20) {     te2p[m][mm]= 0.073780; } 
	  if(m==44 && mm == 6) {     te3p[m][mm]= 0.337003; } 
	  if(m==44 && mm == 7) {     te3p[m][mm]= 0.567154; } 
	  if(m==44 && mm == 9) {     te3p[m][mm]= 0.349288; } 
	  if(m==44 && mm == 10) {     te3p[m][mm]= 0.479078; } 
	  if(m==44 && mm == 11) {     te3p[m][mm]= 0.266236; } 
	  if(m==44 && mm == 12) {     te3p[m][mm]= 0.292164; } 
	  if(m==44 && mm == 13) {     te3p[m][mm]= 0.003462; } 
	  if(m==44 && mm == 14) {     te3p[m][mm]= 0.007538; } 
	  if(m==44 && mm == 16) {     te3p[m][mm]= 0.156494; } 
	  if(m==44 && mm == 21) {     te3p[m][mm]= 0.025453; } 
	  if(m==44 && mm == 3) {     tw0n[m][mm]= 0.042200; } 
	  if(m==44 && mm == 5) {     tw0n[m][mm]= 0.788252; } 
	  if(m==44 && mm == 6) {     tw0n[m][mm]= 0.021323; } 
	  if(m==44 && mm == 7) {     tw0n[m][mm]= 0.069395; } 
	  if(m==44 && mm == 8) {     tw0n[m][mm]= 0.169229; } 
	  if(m==44 && mm == 9) {     tw0n[m][mm]= 0.349194; } 
	  if(m==44 && mm == 23) {     tw0n[m][mm]= 0.010576; } 
	  if(m==44 && mm == 6) {     tw1n[m][mm]= 0.194055; } 
	  if(m==44 && mm == 7) {     tw1n[m][mm]= 1.190279; } 
	  if(m==44 && mm == 10) {     tw1n[m][mm]= 0.237166; } 
	  if(m==44 && mm == 13) {     tw1n[m][mm]= 0.431304; } 
	  if(m==44 && mm == 15) {     tw1n[m][mm]= 0.191373; } 
	  if(m==44 && mm == 17) {     tw1n[m][mm]= 0.200331; } 
	  if(m==44 && mm == 19) {     tw1n[m][mm]= 0.009667; } 
	  if(m==44 && mm == 22) {     tw1n[m][mm]= 0.153960; } 
	  if(m==44 && mm == 23) {     tw1n[m][mm]= 0.345745; } 
	  if(m==44 && mm == 34) {     tw1n[m][mm]= 0.107332; } 
	  if(m==44 && mm == 2) {     tw2n[m][mm]= 0.291749; } 
	  if(m==44 && mm == 6) {     tw2n[m][mm]= 0.202146; } 
	  if(m==44 && mm == 7) {     tw2n[m][mm]= 0.061778; } 
	  if(m==44 && mm == 16) {     tw2n[m][mm]= 0.048838; } 
	  if(m==44 && mm == 17) {     tw2n[m][mm]= 0.101987; } 
	  if(m==44 && mm == 18) {     tw2n[m][mm]= 0.335054; } 
	  if(m==44 && mm == 35) {     tw2n[m][mm]= 0.382466; } 
	  if(m==44 && mm == 0) {     tw3n[m][mm]= 0.545990; } 
	  if(m==44 && mm == 1) {     tw3n[m][mm]= 0.289874; } 
	  if(m==44 && mm == 3) {     tw3n[m][mm]= 0.361352; } 
	  if(m==44 && mm == 5) {     tw3n[m][mm]= 0.655513; } 
	  if(m==44 && mm == 7) {     tw3n[m][mm]= 0.149727; } 
	  if(m==44 && mm == 8) {     tw3n[m][mm]= 0.195618; } 
	  if(m==44 && mm == 9) {     tw3n[m][mm]= 0.228264; } 
	  if(m==44 && mm == 10) {     tw3n[m][mm]= 0.571075; } 
	  if(m==44 && mm == 11) {     tw3n[m][mm]= 0.564910; } 
	  if(m==44 && mm == 13) {     tw3n[m][mm]= 0.167670; } 
	  if(m==44 && mm == 14) {     tw3n[m][mm]= 0.209401; } 
	  if(m==44 && mm == 15) {     tw3n[m][mm]= 0.225980; } 
	  if(m==44 && mm == 16) {     tw3n[m][mm]= 0.139698; } 
	  if(m==44 && mm == 18) {     tw3n[m][mm]= 0.358376; } 
	  if(m==44 && mm == 19) {     tw3n[m][mm]= 0.121030; } 
	  if(m==44 && mm == 20) {     tw3n[m][mm]= 0.163187; } 
	  if(m==44 && mm == 21) {     tw3n[m][mm]= 0.468249; } 
	  if(m==44 && mm == 22) {     tw3n[m][mm]= 0.315875; } 
	  if(m==44 && mm == 23) {     tw3n[m][mm]= 0.087317; } 
	  if(m==44 && mm == 25) {     tw3n[m][mm]= 2.975067; } 
	  if(m==44 && mm == 26) {     tw3n[m][mm]= 4.032598; } 
	  if(m==44 && mm == 27) {     tw3n[m][mm]= 4.042038; } 
	  if(m==44 && mm == 4) {     te2n[m][mm]= 0.092294; } 
	  if(m==44 && mm == 6) {     te2n[m][mm]= 0.110609; } 
	  if(m==44 && mm == 20) {     te2n[m][mm]= 0.223194; } 
	  if(m==44 && mm == 26) {     te2n[m][mm]= 0.046617; } 
	  if(m==44 && mm == 28) {     te2n[m][mm]= 0.111557; } 
	  if(m==44 && mm == 32) {     te2n[m][mm]= 0.363804; } 
	  if(m==44 && mm == 34) {     te2n[m][mm]= 0.763809; } 
	  if(m==44 && mm == 35) {     te2n[m][mm]= 0.387732; } 
	  if(m==44 && mm == 26) {     te3n[m][mm]= 0.308348; } 
	  if(m==44 && mm == 27) {     te3n[m][mm]= 0.134373; } 
	  if(m==44 && mm == 28) {     te3n[m][mm]= 0.775109; } 
	  if(m==44 && mm == 31) {     te3n[m][mm]= 0.210060; } 
	  if(m==45 && mm == 17) {     tw0p[m][mm]= 0.146731; } 
	  if(m==45 && mm == 18) {     tw0p[m][mm]= 0.102018; } 
	  if(m==45 && mm == 22) {     tw0p[m][mm]= 0.014966; } 
	  if(m==45 && mm == 23) {     tw0p[m][mm]= 0.290235; } 
	  if(m==45 && mm == 1) {     tw1p[m][mm]= 0.049775; } 
	  if(m==45 && mm == 5) {     tw1p[m][mm]= 0.197088; } 
	  if(m==45 && mm == 6) {     tw1p[m][mm]= 0.156599; } 
	  if(m==45 && mm == 21) {     tw1p[m][mm]= 1.061483; } 
	  if(m==45 && mm == 22) {     tw1p[m][mm]= 0.209091; } 
	  if(m==45 && mm == 24) {     tw1p[m][mm]= 0.241691; } 
	  if(m==45 && mm == 25) {     tw1p[m][mm]= 0.119442; } 
	  if(m==45 && mm == 26) {     tw1p[m][mm]= 0.059175; } 
	  if(m==45 && mm == 27) {     tw1p[m][mm]= 0.449710; } 
	  if(m==45 && mm == 29) {     tw1p[m][mm]= 0.277098; } 
	  if(m==45 && mm == 30) {     tw1p[m][mm]= 0.312918; } 
	  if(m==45 && mm == 31) {     tw1p[m][mm]= 0.270854; } 
	  if(m==45 && mm == 32) {     tw1p[m][mm]= 0.128713; } 
	  if(m==45 && mm == 34) {     tw1p[m][mm]= 0.594624; } 
	  if(m==45 && mm == 7) {     tw2p[m][mm]= 0.011316; } 
	  if(m==45 && mm == 9) {     tw2p[m][mm]= 0.317524; } 
	  if(m==45 && mm == 11) {     tw2p[m][mm]= 0.301896; } 
	  if(m==45 && mm == 16) {     tw2p[m][mm]= 0.205636; } 
	  if(m==45 && mm == 22) {     tw2p[m][mm]= 0.138436; } 
	  if(m==45 && mm == 29) {     tw2p[m][mm]= 0.105616; } 
	  if(m==45 && mm == 30) {     tw2p[m][mm]= 0.300969; } 
	  if(m==45 && mm == 31) {     tw2p[m][mm]= 0.245394; } 
	  if(m==45 && mm == 32) {     tw2p[m][mm]= 0.282874; } 
	  if(m==45 && mm == 33) {     tw2p[m][mm]= 0.071467; } 
	  if(m==45 && mm == 34) {     tw2p[m][mm]= 0.086051; } 
	  if(m==45 && mm == 0) {     tw3p[m][mm]= 0.419618; } 
	  if(m==45 && mm == 1) {     tw3p[m][mm]= 0.456297; } 
	  if(m==45 && mm == 2) {     tw3p[m][mm]= 0.368974; } 
	  if(m==45 && mm == 4) {     tw3p[m][mm]= 0.571493; } 
	  if(m==45 && mm == 5) {     tw3p[m][mm]= 0.459752; } 
	  if(m==45 && mm == 7) {     tw3p[m][mm]= 0.472647; } 
	  if(m==45 && mm == 9) {     tw3p[m][mm]= 0.371076; } 
	  if(m==45 && mm == 10) {     tw3p[m][mm]= 0.502893; } 
	  if(m==45 && mm == 11) {     tw3p[m][mm]= 0.145796; } 
	  if(m==45 && mm == 12) {     tw3p[m][mm]= 0.454812; } 
	  if(m==45 && mm == 13) {     tw3p[m][mm]= 0.373911; } 
	  if(m==45 && mm == 15) {     tw3p[m][mm]= 0.231686; } 
	  if(m==45 && mm == 16) {     tw3p[m][mm]= 0.254534; } 
	  if(m==45 && mm == 17) {     tw3p[m][mm]= 0.225338; } 
	  if(m==45 && mm == 18) {     tw3p[m][mm]= 0.115270; } 
	  if(m==45 && mm == 19) {     tw3p[m][mm]= 0.367451; } 
	  if(m==45 && mm == 20) {     tw3p[m][mm]= 0.394494; } 
	  if(m==45 && mm == 21) {     tw3p[m][mm]= 0.260383; } 
	  if(m==45 && mm == 23) {     tw3p[m][mm]= 0.122849; } 
	  if(m==45 && mm == 24) {     tw3p[m][mm]= 4.113306; } 
	  if(m==45 && mm == 25) {     tw3p[m][mm]= 5.436959; } 
	  if(m==45 && mm == 26) {     tw3p[m][mm]= 4.779539; } 
	  if(m==45 && mm == 27) {     tw3p[m][mm]= 4.510088; } 
	  if(m==45 && mm == 28) {     tw3p[m][mm]= 4.764968; } 
	  if(m==45 && mm == 29) {     tw3p[m][mm]= 4.095206; } 
	  if(m==45 && mm == 30) {     tw3p[m][mm]= 3.445609; } 
	  if(m==45 && mm == 33) {     tw3p[m][mm]= 1.714069; } 
	  if(m==45 && mm == 35) {     tw3p[m][mm]= 5.294349; } 
	  if(m==45 && mm == 1) {     te2p[m][mm]= 0.428179; } 
	  if(m==45 && mm == 2) {     te2p[m][mm]= 0.214616; } 
	  if(m==45 && mm == 3) {     te2p[m][mm]= 0.409909; } 
	  if(m==45 && mm == 4) {     te2p[m][mm]= 0.175861; } 
	  if(m==45 && mm == 7) {     te2p[m][mm]= 0.582906; } 
	  if(m==45 && mm == 8) {     te2p[m][mm]= 0.317255; } 
	  if(m==45 && mm == 13) {     te2p[m][mm]= 0.083038; } 
	  if(m==45 && mm == 17) {     te2p[m][mm]= 0.017869; } 
	  if(m==45 && mm == 18) {     te2p[m][mm]= 0.051766; } 
	  if(m==45 && mm == 8) {     te3p[m][mm]= 0.690967; } 
	  if(m==45 && mm == 9) {     te3p[m][mm]= 0.811722; } 
	  if(m==45 && mm == 10) {     te3p[m][mm]= 0.388342; } 
	  if(m==45 && mm == 11) {     te3p[m][mm]= 0.194655; } 
	  if(m==45 && mm == 17) {     te3p[m][mm]= 0.115677; } 
	  if(m==45 && mm == 0) {     tw0n[m][mm]= 1.029809; } 
	  if(m==45 && mm == 2) {     tw0n[m][mm]= 0.313947; } 
	  if(m==45 && mm == 3) {     tw0n[m][mm]= 0.396411; } 
	  if(m==45 && mm == 4) {     tw0n[m][mm]= 0.287425; } 
	  if(m==45 && mm == 7) {     tw0n[m][mm]= 0.176788; } 
	  if(m==45 && mm == 10) {     tw0n[m][mm]= 0.022548; } 
	  if(m==45 && mm == 17) {     tw0n[m][mm]= 0.172512; } 
	  if(m==45 && mm == 9) {     tw1n[m][mm]= 0.423079; } 
	  if(m==45 && mm == 11) {     tw1n[m][mm]= 1.478142; } 
	  if(m==45 && mm == 12) {     tw1n[m][mm]= 0.273901; } 
	  if(m==45 && mm == 13) {     tw1n[m][mm]= 0.200021; } 
	  if(m==45 && mm == 14) {     tw1n[m][mm]= 0.754626; } 
	  if(m==45 && mm == 15) {     tw1n[m][mm]= 0.572416; } 
	  if(m==45 && mm == 16) {     tw1n[m][mm]= 0.671387; } 
	  if(m==45 && mm == 18) {     tw1n[m][mm]= 0.112868; } 
	  if(m==45 && mm == 23) {     tw1n[m][mm]= 1.008423; } 
	  if(m==45 && mm == 35) {     tw1n[m][mm]= 0.310308; } 
	  if(m==45 && mm == 1) {     tw2n[m][mm]= 0.110000; } 
	  if(m==45 && mm == 4) {     tw2n[m][mm]= 0.248947; } 
	  if(m==45 && mm == 7) {     tw2n[m][mm]= 0.155342; } 
	  if(m==45 && mm == 16) {     tw2n[m][mm]= 0.136602; } 
	  if(m==45 && mm == 35) {     tw2n[m][mm]= 0.959277; } 
	  if(m==45 && mm == 0) {     tw3n[m][mm]= 0.425941; } 
	  if(m==45 && mm == 1) {     tw3n[m][mm]= 0.676031; } 
	  if(m==45 && mm == 2) {     tw3n[m][mm]= 0.403398; } 
	  if(m==45 && mm == 4) {     tw3n[m][mm]= 0.764961; } 
	  if(m==45 && mm == 5) {     tw3n[m][mm]= 0.605023; } 
	  if(m==45 && mm == 7) {     tw3n[m][mm]= 0.368576; } 
	  if(m==45 && mm == 8) {     tw3n[m][mm]= 0.034857; } 
	  if(m==45 && mm == 9) {     tw3n[m][mm]= 0.392466; } 
	  if(m==45 && mm == 10) {     tw3n[m][mm]= 0.602522; } 
	  if(m==45 && mm == 11) {     tw3n[m][mm]= 0.058064; } 
	  if(m==45 && mm == 12) {     tw3n[m][mm]= 0.373185; } 
	  if(m==45 && mm == 18) {     tw3n[m][mm]= 0.018976; } 
	  if(m==45 && mm == 21) {     tw3n[m][mm]= 0.011322; } 
	  if(m==45 && mm == 24) {     tw3n[m][mm]= 4.127691; } 
	  if(m==45 && mm == 26) {     tw3n[m][mm]= 4.608101; } 
	  if(m==45 && mm == 27) {     tw3n[m][mm]= 4.181980; } 
	  if(m==45 && mm == 26) {     te2n[m][mm]= 0.039212; } 
	  if(m==45 && mm == 28) {     te2n[m][mm]= 0.013652; } 
	  if(m==45 && mm == 34) {     te2n[m][mm]= 0.519316; } 
	  if(m==45 && mm == 35) {     te2n[m][mm]= 0.281937; } 
	  if(m==45 && mm == 6) {     te3n[m][mm]= 0.548801; } 
	  if(m==45 && mm == 25) {     te3n[m][mm]= 0.110865; } 
	  if(m==45 && mm == 26) {     te3n[m][mm]= 0.324781; } 
	  if(m==45 && mm == 28) {     te3n[m][mm]= 0.007982; } 
	  if(m==45 && mm == 30) {     te3n[m][mm]= 0.438852; } 
	  if(m==46 && mm == 8) {     tw0p[m][mm]= 0.187406; } 
	  if(m==46 && mm == 11) {     tw0p[m][mm]= 0.028705; } 
	  if(m==46 && mm == 15) {     tw0p[m][mm]= 0.261893; } 
	  if(m==46 && mm == 20) {     tw0p[m][mm]= 0.070177; } 
	  if(m==46 && mm == 25) {     tw0p[m][mm]= 0.083276; } 
	  if(m==46 && mm == 28) {     tw0p[m][mm]= 0.404919; } 
	  if(m==46 && mm == 29) {     tw0p[m][mm]= 0.177358; } 
	  if(m==46 && mm == 32) {     tw0p[m][mm]= 0.287888; } 
	  if(m==46 && mm == 0) {     tw1p[m][mm]= 0.322205; } 
	  if(m==46 && mm == 1) {     tw1p[m][mm]= 0.257197; } 
	  if(m==46 && mm == 2) {     tw1p[m][mm]= 0.020969; } 
	  if(m==46 && mm == 4) {     tw1p[m][mm]= 0.021125; } 
	  if(m==46 && mm == 5) {     tw1p[m][mm]= 0.262931; } 
	  if(m==46 && mm == 20) {     tw1p[m][mm]= 0.336571; } 
	  if(m==46 && mm == 22) {     tw1p[m][mm]= 0.255416; } 
	  if(m==46 && mm == 23) {     tw1p[m][mm]= 0.339883; } 
	  if(m==46 && mm == 24) {     tw1p[m][mm]= 0.368321; } 
	  if(m==46 && mm == 25) {     tw1p[m][mm]= 0.051198; } 
	  if(m==46 && mm == 26) {     tw1p[m][mm]= 0.524799; } 
	  if(m==46 && mm == 27) {     tw1p[m][mm]= 0.245925; } 
	  if(m==46 && mm == 30) {     tw1p[m][mm]= 0.115122; } 
	  if(m==46 && mm == 31) {     tw1p[m][mm]= 0.220335; } 
	  if(m==46 && mm == 32) {     tw1p[m][mm]= 0.451678; } 
	  if(m==46 && mm == 33) {     tw1p[m][mm]= 1.026495; } 
	  if(m==46 && mm == 7) {     tw2p[m][mm]= 0.128026; } 
	  if(m==46 && mm == 11) {     tw2p[m][mm]= 0.288506; } 
	  if(m==46 && mm == 12) {     tw2p[m][mm]= 0.149799; } 
	  if(m==46 && mm == 15) {     tw2p[m][mm]= 0.172049; } 
	  if(m==46 && mm == 16) {     tw2p[m][mm]= 0.486374; } 
	  if(m==46 && mm == 17) {     tw2p[m][mm]= 0.155314; } 
	  if(m==46 && mm == 18) {     tw2p[m][mm]= 0.290700; } 
	  if(m==46 && mm == 19) {     tw2p[m][mm]= 0.018186; } 
	  if(m==46 && mm == 28) {     tw2p[m][mm]= 0.459518; } 
	  if(m==46 && mm == 29) {     tw2p[m][mm]= 0.040896; } 
	  if(m==46 && mm == 30) {     tw2p[m][mm]= 0.202315; } 
	  if(m==46 && mm == 34) {     tw2p[m][mm]= 0.114311; } 
	  if(m==46 && mm == 3) {     tw3p[m][mm]= 0.220681; } 
	  if(m==46 && mm == 12) {     tw3p[m][mm]= 0.402012; } 
	  if(m==46 && mm == 13) {     tw3p[m][mm]= 0.107507; } 
	  if(m==46 && mm == 16) {     tw3p[m][mm]= 0.133283; } 
	  if(m==46 && mm == 17) {     tw3p[m][mm]= 0.411720; } 
	  if(m==46 && mm == 20) {     tw3p[m][mm]= 0.430748; } 
	  if(m==46 && mm == 21) {     tw3p[m][mm]= 0.580829; } 
	  if(m==46 && mm == 22) {     tw3p[m][mm]= 0.423040; } 
	  if(m==46 && mm == 23) {     tw3p[m][mm]= 0.451753; } 
	  if(m==46 && mm == 24) {     tw3p[m][mm]= 5.798319; } 
	  if(m==46 && mm == 25) {     tw3p[m][mm]= 5.366194; } 
	  if(m==46 && mm == 26) {     tw3p[m][mm]= 5.943258; } 
	  if(m==46 && mm == 27) {     tw3p[m][mm]= 5.013999; } 
	  if(m==46 && mm == 28) {     tw3p[m][mm]= 5.984605; } 
	  if(m==46 && mm == 29) {     tw3p[m][mm]= 5.564940; } 
	  if(m==46 && mm == 30) {     tw3p[m][mm]= 4.398279; } 
	  if(m==46 && mm == 32) {     tw3p[m][mm]= 5.079960; } 
	  if(m==46 && mm == 35) {     tw3p[m][mm]= 5.822699; } 
	  if(m==46 && mm == 0) {     te2p[m][mm]= 0.642342; } 
	  if(m==46 && mm == 2) {     te2p[m][mm]= 0.080191; } 
	  if(m==46 && mm == 3) {     te2p[m][mm]= 1.034816; } 
	  if(m==46 && mm == 4) {     te2p[m][mm]= 0.029107; } 
	  if(m==46 && mm == 5) {     te2p[m][mm]= 0.265496; } 
	  if(m==46 && mm == 6) {     te2p[m][mm]= 0.253442; } 
	  if(m==46 && mm == 7) {     te2p[m][mm]= 0.307057; } 
	  if(m==46 && mm == 9) {     te2p[m][mm]= 0.194689; } 
	  if(m==46 && mm == 10) {     te2p[m][mm]= 0.126271; } 
	  if(m==46 && mm == 11) {     te2p[m][mm]= 0.120197; } 
	  if(m==46 && mm == 13) {     te2p[m][mm]= 0.084803; } 
	  if(m==46 && mm == 15) {     te2p[m][mm]= 0.046374; } 
	  if(m==46 && mm == 20) {     te2p[m][mm]= 0.394238; } 
	  if(m==46 && mm == 21) {     te2p[m][mm]= 0.182600; } 
	  if(m==46 && mm == 26) {     te2p[m][mm]= 0.033945; } 
	  if(m==46 && mm == 30) {     te2p[m][mm]= 0.370715; } 
	  if(m==46 && mm == 31) {     te2p[m][mm]= 0.500713; } 
	  if(m==46 && mm == 6) {     te3p[m][mm]= 0.159164; } 
	  if(m==46 && mm == 7) {     te3p[m][mm]= 1.269141; } 
	  if(m==46 && mm == 9) {     te3p[m][mm]= 0.445717; } 
	  if(m==46 && mm == 11) {     te3p[m][mm]= 0.023794; } 
	  if(m==46 && mm == 15) {     te3p[m][mm]= 0.059746; } 
	  if(m==46 && mm == 18) {     te3p[m][mm]= 0.170932; } 
	  if(m==46 && mm == 0) {     tw0n[m][mm]= 0.717912; } 
	  if(m==46 && mm == 1) {     tw0n[m][mm]= 0.683782; } 
	  if(m==46 && mm == 2) {     tw0n[m][mm]= 0.108422; } 
	  if(m==46 && mm == 3) {     tw0n[m][mm]= 0.200265; } 
	  if(m==46 && mm == 4) {     tw0n[m][mm]= 0.723921; } 
	  if(m==46 && mm == 5) {     tw0n[m][mm]= 1.215230; } 
	  if(m==46 && mm == 7) {     tw0n[m][mm]= 0.053097; } 
	  if(m==46 && mm == 8) {     tw0n[m][mm]= 0.178546; } 
	  if(m==46 && mm == 9) {     tw0n[m][mm]= 0.174994; } 
	  if(m==46 && mm == 11) {     tw0n[m][mm]= 0.082545; } 
	  if(m==46 && mm == 13) {     tw0n[m][mm]= 0.113886; } 
	  if(m==46 && mm == 19) {     tw0n[m][mm]= 0.198145; } 
	  if(m==46 && mm == 20) {     tw0n[m][mm]= 0.117341; } 
	  if(m==46 && mm == 8) {     tw1n[m][mm]= 0.263651; } 
	  if(m==46 && mm == 14) {     tw1n[m][mm]= 1.375085; } 
	  if(m==46 && mm == 15) {     tw1n[m][mm]= 0.452418; } 
	  if(m==46 && mm == 18) {     tw1n[m][mm]= 0.218398; } 
	  if(m==46 && mm == 20) {     tw1n[m][mm]= 0.416109; } 
	  if(m==46 && mm == 22) {     tw1n[m][mm]= 0.080456; } 
	  if(m==46 && mm == 23) {     tw1n[m][mm]= 0.002816; } 
	  if(m==46 && mm == 31) {     tw1n[m][mm]= 0.030271; } 
	  if(m==46 && mm == 33) {     tw1n[m][mm]= 0.222438; } 
	  if(m==46 && mm == 0) {     tw2n[m][mm]= 0.318821; } 
	  if(m==46 && mm == 1) {     tw2n[m][mm]= 0.785064; } 
	  if(m==46 && mm == 2) {     tw2n[m][mm]= 0.717796; } 
	  if(m==46 && mm == 3) {     tw2n[m][mm]= 0.642822; } 
	  if(m==46 && mm == 4) {     tw2n[m][mm]= 0.358849; } 
	  if(m==46 && mm == 6) {     tw2n[m][mm]= 0.040270; } 
	  if(m==46 && mm == 7) {     tw2n[m][mm]= 0.190510; } 
	  if(m==46 && mm == 14) {     tw2n[m][mm]= 1.018982; } 
	  if(m==46 && mm == 15) {     tw2n[m][mm]= 0.410533; } 
	  if(m==46 && mm == 16) {     tw2n[m][mm]= 0.546014; } 
	  if(m==46 && mm == 17) {     tw2n[m][mm]= 0.000110; } 
	  if(m==46 && mm == 18) {     tw2n[m][mm]= 0.196459; } 
	  if(m==46 && mm == 34) {     tw2n[m][mm]= 0.055469; } 
	  if(m==46 && mm == 3) {     tw3n[m][mm]= 0.274129; } 
	  if(m==46 && mm == 12) {     tw3n[m][mm]= 0.279755; } 
	  if(m==46 && mm == 13) {     tw3n[m][mm]= 0.096088; } 
	  if(m==46 && mm == 16) {     tw3n[m][mm]= 0.007944; } 
	  if(m==46 && mm == 17) {     tw3n[m][mm]= 0.225710; } 
	  if(m==46 && mm == 20) {     tw3n[m][mm]= 0.059744; } 
	  if(m==46 && mm == 21) {     tw3n[m][mm]= 0.045266; } 
	  if(m==46 && mm == 22) {     tw3n[m][mm]= 0.446908; } 
	  if(m==46 && mm == 23) {     tw3n[m][mm]= 0.121261; } 
	  if(m==46 && mm == 24) {     tw3n[m][mm]= 5.462478; } 
	  if(m==46 && mm == 25) {     tw3n[m][mm]= 5.785478; } 
	  if(m==46 && mm == 26) {     tw3n[m][mm]= 5.779988; } 
	  if(m==46 && mm == 27) {     tw3n[m][mm]= 5.009793; } 
	  if(m==46 && mm == 7) {     te2n[m][mm]= 0.267981; } 
	  if(m==46 && mm == 9) {     te2n[m][mm]= 0.123257; } 
	  if(m==46 && mm == 10) {     te2n[m][mm]= 0.054718; } 
	  if(m==46 && mm == 12) {     te2n[m][mm]= 0.145420; } 
	  if(m==46 && mm == 16) {     te2n[m][mm]= 0.066897; } 
	  if(m==46 && mm == 20) {     te2n[m][mm]= 0.063992; } 
	  if(m==46 && mm == 26) {     te2n[m][mm]= 0.489860; } 
	  if(m==46 && mm == 27) {     te2n[m][mm]= 0.101823; } 
	  if(m==46 && mm == 30) {     te2n[m][mm]= 0.874343; } 
	  if(m==46 && mm == 31) {     te2n[m][mm]= 1.235908; } 
	  if(m==46 && mm == 32) {     te2n[m][mm]= 0.759588; } 
	  if(m==46 && mm == 34) {     te2n[m][mm]= 0.825617; } 
	  if(m==46 && mm == 15) {     te3n[m][mm]= 0.018863; } 
	  if(m==46 && mm == 21) {     te3n[m][mm]= 0.099245; } 
	  if(m==46 && mm == 24) {     te3n[m][mm]= 0.566608; } 
	  if(m==46 && mm == 25) {     te3n[m][mm]= 0.559481; } 
	  if(m==46 && mm == 27) {     te3n[m][mm]= 0.709850; } 
	  if(m==46 && mm == 28) {     te3n[m][mm]= 0.531989; } 
	  if(m==46 && mm == 29) {     te3n[m][mm]= 0.684164; } 
	  if(m==46 && mm == 30) {     te3n[m][mm]= 0.586051; } 
	  if(m==46 && mm == 31) {     te3n[m][mm]= 0.431888; } 
	  if(m==46 && mm == 32) {     te3n[m][mm]= 0.540785; } 
	  if(m==46 && mm == 33) {     te3n[m][mm]= 0.005738; } 
	  if(m==46 && mm == 34) {     te3n[m][mm]= 0.801050; } 
	  if(m==47 && mm == 8) {     tw0p[m][mm]= 0.168674; } 
	  if(m==47 && mm == 10) {     tw0p[m][mm]= 0.108764; } 
	  if(m==47 && mm == 11) {     tw0p[m][mm]= 0.074501; } 
	  if(m==47 && mm == 13) {     tw0p[m][mm]= 0.031060; } 
	  if(m==47 && mm == 16) {     tw0p[m][mm]= 0.019746; } 
	  if(m==47 && mm == 26) {     tw0p[m][mm]= 0.083518; } 
	  if(m==47 && mm == 27) {     tw0p[m][mm]= 0.104647; } 
	  if(m==47 && mm == 28) {     tw0p[m][mm]= 0.263802; } 
	  if(m==47 && mm == 0) {     tw1p[m][mm]= 0.451499; } 
	  if(m==47 && mm == 4) {     tw1p[m][mm]= 0.086907; } 
	  if(m==47 && mm == 5) {     tw1p[m][mm]= 0.000850; } 
	  if(m==47 && mm == 7) {     tw1p[m][mm]= 0.106798; } 
	  if(m==47 && mm == 22) {     tw1p[m][mm]= 0.046167; } 
	  if(m==47 && mm == 24) {     tw1p[m][mm]= 0.375946; } 
	  if(m==47 && mm == 25) {     tw1p[m][mm]= 0.276034; } 
	  if(m==47 && mm == 26) {     tw1p[m][mm]= 0.208142; } 
	  if(m==47 && mm == 27) {     tw1p[m][mm]= 0.173494; } 
	  if(m==47 && mm == 30) {     tw1p[m][mm]= 0.208735; } 
	  if(m==47 && mm == 31) {     tw1p[m][mm]= 0.449538; } 
	  if(m==47 && mm == 32) {     tw1p[m][mm]= 0.659931; } 
	  if(m==47 && mm == 33) {     tw1p[m][mm]= 0.484535; } 
	  if(m==47 && mm == 34) {     tw1p[m][mm]= 0.429679; } 
	  if(m==47 && mm == 35) {     tw1p[m][mm]= 0.429836; } 
	  if(m==47 && mm == 6) {     tw2p[m][mm]= 0.151171; } 
	  if(m==47 && mm == 13) {     tw2p[m][mm]= 0.064335; } 
	  if(m==47 && mm == 14) {     tw2p[m][mm]= 0.033451; } 
	  if(m==47 && mm == 18) {     tw2p[m][mm]= 0.194168; } 
	  if(m==47 && mm == 19) {     tw2p[m][mm]= 0.105741; } 
	  if(m==47 && mm == 33) {     tw2p[m][mm]= 0.195389; } 
	  if(m==47 && mm == 12) {     tw3p[m][mm]= 0.062258; } 
	  if(m==47 && mm == 13) {     tw3p[m][mm]= 0.143441; } 
	  if(m==47 && mm == 16) {     tw3p[m][mm]= 0.383867; } 
	  if(m==47 && mm == 17) {     tw3p[m][mm]= 0.465375; } 
	  if(m==47 && mm == 22) {     tw3p[m][mm]= 0.630181; } 
	  if(m==47 && mm == 23) {     tw3p[m][mm]= 0.346167; } 
	  if(m==47 && mm == 24) {     tw3p[m][mm]= 6.315368; } 
	  if(m==47 && mm == 25) {     tw3p[m][mm]= 6.402980; } 
	  if(m==47 && mm == 26) {     tw3p[m][mm]= 5.827967; } 
	  if(m==47 && mm == 28) {     tw3p[m][mm]= 6.245444; } 
	  if(m==47 && mm == 29) {     tw3p[m][mm]= 6.161250; } 
	  if(m==47 && mm == 32) {     tw3p[m][mm]= 4.809936; } 
	  if(m==47 && mm == 35) {     tw3p[m][mm]= 6.801403; } 
	  if(m==47 && mm == 0) {     te2p[m][mm]= 0.456047; } 
	  if(m==47 && mm == 1) {     te2p[m][mm]= 0.211174; } 
	  if(m==47 && mm == 3) {     te2p[m][mm]= 0.393604; } 
	  if(m==47 && mm == 4) {     te2p[m][mm]= 0.015047; } 
	  if(m==47 && mm == 6) {     te2p[m][mm]= 0.742388; } 
	  if(m==47 && mm == 11) {     te2p[m][mm]= 0.152894; } 
	  if(m==47 && mm == 13) {     te2p[m][mm]= 0.105153; } 
	  if(m==47 && mm == 15) {     te2p[m][mm]= 0.347974; } 
	  if(m==47 && mm == 20) {     te2p[m][mm]= 0.148055; } 
	  if(m==47 && mm == 21) {     te2p[m][mm]= 0.257714; } 
	  if(m==47 && mm == 26) {     te2p[m][mm]= 0.047006; } 
	  if(m==47 && mm == 31) {     te2p[m][mm]= 0.733585; } 
	  if(m==47 && mm == 6) {     te3p[m][mm]= 0.331426; } 
	  if(m==47 && mm == 7) {     te3p[m][mm]= 0.342854; } 
	  if(m==47 && mm == 8) {     te3p[m][mm]= 1.184416; } 
	  if(m==47 && mm == 9) {     te3p[m][mm]= 0.405764; } 
	  if(m==47 && mm == 10) {     te3p[m][mm]= 0.250670; } 
	  if(m==47 && mm == 20) {     te3p[m][mm]= 0.102061; } 
	  if(m==47 && mm == 0) {     tw0n[m][mm]= 0.324017; } 
	  if(m==47 && mm == 3) {     tw0n[m][mm]= 0.410435; } 
	  if(m==47 && mm == 5) {     tw0n[m][mm]= 0.369357; } 
	  if(m==47 && mm == 6) {     tw0n[m][mm]= 0.113103; } 
	  if(m==47 && mm == 7) {     tw0n[m][mm]= 0.176156; } 
	  if(m==47 && mm == 8) {     tw0n[m][mm]= 0.369192; } 
	  if(m==47 && mm == 9) {     tw0n[m][mm]= 0.190234; } 
	  if(m==47 && mm == 10) {     tw0n[m][mm]= 0.119009; } 
	  if(m==47 && mm == 11) {     tw0n[m][mm]= 0.183830; } 
	  if(m==47 && mm == 13) {     tw0n[m][mm]= 0.318053; } 
	  if(m==47 && mm == 19) {     tw0n[m][mm]= 0.168489; } 
	  if(m==47 && mm == 8) {     tw1n[m][mm]= 0.464186; } 
	  if(m==47 && mm == 12) {     tw1n[m][mm]= 0.075636; } 
	  if(m==47 && mm == 13) {     tw1n[m][mm]= 0.310896; } 
	  if(m==47 && mm == 14) {     tw1n[m][mm]= 0.547767; } 
	  if(m==47 && mm == 17) {     tw1n[m][mm]= 0.032449; } 
	  if(m==47 && mm == 23) {     tw1n[m][mm]= 0.084045; } 
	  if(m==47 && mm == 1) {     tw2n[m][mm]= 0.748615; } 
	  if(m==47 && mm == 3) {     tw2n[m][mm]= 0.659909; } 
	  if(m==47 && mm == 4) {     tw2n[m][mm]= 0.208324; } 
	  if(m==47 && mm == 5) {     tw2n[m][mm]= 0.756908; } 
	  if(m==47 && mm == 6) {     tw2n[m][mm]= 0.079012; } 
	  if(m==47 && mm == 14) {     tw2n[m][mm]= 0.077342; } 
	  if(m==47 && mm == 18) {     tw2n[m][mm]= 0.051604; } 
	  if(m==47 && mm == 28) {     tw2n[m][mm]= 0.179721; } 
	  if(m==47 && mm == 29) {     tw2n[m][mm]= 0.182243; } 
	  if(m==47 && mm == 16) {     tw3n[m][mm]= 0.087268; } 
	  if(m==47 && mm == 17) {     tw3n[m][mm]= 0.194480; } 
	  if(m==47 && mm == 22) {     tw3n[m][mm]= 0.279574; } 
	  if(m==47 && mm == 24) {     tw3n[m][mm]= 6.183983; } 
	  if(m==47 && mm == 25) {     tw3n[m][mm]= 6.116952; } 
	  if(m==47 && mm == 26) {     tw3n[m][mm]= 5.716058; } 
	  if(m==47 && mm == 20) {     te2n[m][mm]= 0.250568; } 
	  if(m==47 && mm == 26) {     te2n[m][mm]= 0.222593; } 
	  if(m==47 && mm == 30) {     te2n[m][mm]= 0.800920; } 
	  if(m==47 && mm == 31) {     te2n[m][mm]= 0.490921; } 
	  if(m==47 && mm == 32) {     te2n[m][mm]= 0.090810; } 
	  if(m==47 && mm == 33) {     te2n[m][mm]= 0.718499; } 
	  if(m==47 && mm == 34) {     te2n[m][mm]= 0.258139; } 
	  if(m==47 && mm == 35) {     te2n[m][mm]= 0.035822; } 
	  if(m==47 && mm == 18) {     te3n[m][mm]= 0.910007; } 
	  if(m==47 && mm == 20) {     te3n[m][mm]= 0.143617; } 
	  if(m==47 && mm == 24) {     te3n[m][mm]= 0.012357; } 
	  if(m==47 && mm == 25) {     te3n[m][mm]= 0.320653; } 
	  if(m==47 && mm == 26) {     te3n[m][mm]= 0.265016; } 
	  if(m==47 && mm == 27) {     te3n[m][mm]= 0.250583; } 
	  if(m==47 && mm == 28) {     te3n[m][mm]= 0.459317; } 
	  if(m==47 && mm == 29) {     te3n[m][mm]= 0.593985; } 
	  if(m==47 && mm == 30) {     te3n[m][mm]= 0.262647; } 
	  if(m==47 && mm == 31) {     te3n[m][mm]= 0.621601; } 
	  if(m==47 && mm == 32) {     te3n[m][mm]= 0.388629; } 
	  if(m==47 && mm == 33) {     te3n[m][mm]= 0.437843; } 
	  if(m==47 && mm == 34) {     te3n[m][mm]= 1.399318; } 
	  if(m==47 && mm == 35) {     te3n[m][mm]= 0.687581; } 
	  if(m==48 && mm == 12) {     tw0p[m][mm]= 0.153624; } 
	  if(m==48 && mm == 13) {     tw0p[m][mm]= 0.253584; } 
	  if(m==48 && mm == 14) {     tw0p[m][mm]= 0.124581; } 
	  if(m==48 && mm == 20) {     tw0p[m][mm]= 0.187781; } 
	  if(m==48 && mm == 23) {     tw0p[m][mm]= 0.076869; } 
	  if(m==48 && mm == 24) {     tw0p[m][mm]= 0.156351; } 
	  if(m==48 && mm == 26) {     tw0p[m][mm]= 0.133260; } 
	  if(m==48 && mm == 27) {     tw0p[m][mm]= 0.272873; } 
	  if(m==48 && mm == 28) {     tw0p[m][mm]= 0.229798; } 
	  if(m==48 && mm == 29) {     tw0p[m][mm]= 0.121089; } 
	  if(m==48 && mm == 30) {     tw0p[m][mm]= 0.466392; } 
	  if(m==48 && mm == 31) {     tw0p[m][mm]= 0.451303; } 
	  if(m==48 && mm == 32) {     tw0p[m][mm]= 0.604649; } 
	  if(m==48 && mm == 33) {     tw0p[m][mm]= 0.000131; } 
	  if(m==48 && mm == 34) {     tw0p[m][mm]= 0.855336; } 
	  if(m==48 && mm == 0) {     tw1p[m][mm]= 0.004483; } 
	  if(m==48 && mm == 2) {     tw1p[m][mm]= 0.144993; } 
	  if(m==48 && mm == 3) {     tw1p[m][mm]= 0.014651; } 
	  if(m==48 && mm == 6) {     tw1p[m][mm]= 0.059098; } 
	  if(m==48 && mm == 20) {     tw1p[m][mm]= 0.189447; } 
	  if(m==48 && mm == 25) {     tw1p[m][mm]= 0.130360; } 
	  if(m==48 && mm == 27) {     tw1p[m][mm]= 0.033221; } 
	  if(m==48 && mm == 29) {     tw1p[m][mm]= 0.170555; } 
	  if(m==48 && mm == 30) {     tw1p[m][mm]= 0.058953; } 
	  if(m==48 && mm == 31) {     tw1p[m][mm]= 0.298031; } 
	  if(m==48 && mm == 33) {     tw1p[m][mm]= 0.230040; } 
	  if(m==48 && mm == 34) {     tw1p[m][mm]= 0.584000; } 
	  if(m==48 && mm == 8) {     tw2p[m][mm]= 0.140553; } 
	  if(m==48 && mm == 10) {     tw2p[m][mm]= 0.429695; } 
	  if(m==48 && mm == 12) {     tw2p[m][mm]= 0.129427; } 
	  if(m==48 && mm == 14) {     tw2p[m][mm]= 0.080871; } 
	  if(m==48 && mm == 18) {     tw2p[m][mm]= 0.071343; } 
	  if(m==48 && mm == 20) {     tw2p[m][mm]= 0.477699; } 
	  if(m==48 && mm == 21) {     tw2p[m][mm]= 0.275057; } 
	  if(m==48 && mm == 22) {     tw2p[m][mm]= 0.099769; } 
	  if(m==48 && mm == 29) {     tw2p[m][mm]= 0.195502; } 
	  if(m==48 && mm == 30) {     tw2p[m][mm]= 1.056624; } 
	  if(m==48 && mm == 31) {     tw2p[m][mm]= 0.410403; } 
	  if(m==48 && mm == 32) {     tw2p[m][mm]= 0.152463; } 
	  if(m==48 && mm == 33) {     tw2p[m][mm]= 0.620021; } 
	  if(m==48 && mm == 34) {     tw2p[m][mm]= 0.543985; } 
	  if(m==48 && mm == 35) {     tw2p[m][mm]= 0.196350; } 
	  if(m==48 && mm == 0) {     tw3p[m][mm]= 0.307330; } 
	  if(m==48 && mm == 1) {     tw3p[m][mm]= 1.010710; } 
	  if(m==48 && mm == 23) {     tw3p[m][mm]= 0.122225; } 
	  if(m==48 && mm == 32) {     tw3p[m][mm]= 0.609644; } 
	  if(m==48 && mm == 33) {     tw3p[m][mm]= 0.125963; } 
	  if(m==48 && mm == 34) {     tw3p[m][mm]= 0.457041; } 
	  if(m==48 && mm == 0) {     te2p[m][mm]= 0.236912; } 
	  if(m==48 && mm == 1) {     te2p[m][mm]= 0.379556; } 
	  if(m==48 && mm == 2) {     te2p[m][mm]= 0.162684; } 
	  if(m==48 && mm == 4) {     te2p[m][mm]= 0.127083; } 
	  if(m==48 && mm == 5) {     te2p[m][mm]= 0.258584; } 
	  if(m==48 && mm == 6) {     te2p[m][mm]= 0.078697; } 
	  if(m==48 && mm == 7) {     te2p[m][mm]= 0.281039; } 
	  if(m==48 && mm == 8) {     te2p[m][mm]= 0.461410; } 
	  if(m==48 && mm == 12) {     te2p[m][mm]= 0.084047; } 
	  if(m==48 && mm == 13) {     te2p[m][mm]= 0.021171; } 
	  if(m==48 && mm == 14) {     te2p[m][mm]= 0.013486; } 
	  if(m==48 && mm == 16) {     te2p[m][mm]= 0.040271; } 
	  if(m==48 && mm == 17) {     te2p[m][mm]= 0.249766; } 
	  if(m==48 && mm == 18) {     te2p[m][mm]= 0.096370; } 
	  if(m==48 && mm == 21) {     te2p[m][mm]= 0.078668; } 
	  if(m==48 && mm == 25) {     te2p[m][mm]= 0.035643; } 
	  if(m==48 && mm == 26) {     te2p[m][mm]= 0.196425; } 
	  if(m==48 && mm == 4) {     te3p[m][mm]= 0.050092; } 
	  if(m==48 && mm == 7) {     te3p[m][mm]= 0.111263; } 
	  if(m==48 && mm == 12) {     te3p[m][mm]= 0.127971; } 
	  if(m==48 && mm == 13) {     te3p[m][mm]= 0.136587; } 
	  if(m==48 && mm == 14) {     te3p[m][mm]= 0.012701; } 
	  if(m==48 && mm == 16) {     te3p[m][mm]= 0.297202; } 
	  if(m==48 && mm == 17) {     te3p[m][mm]= 0.322629; } 
	  if(m==48 && mm == 18) {     te3p[m][mm]= 0.297344; } 
	  if(m==48 && mm == 20) {     te3p[m][mm]= 0.179462; } 
	  if(m==48 && mm == 21) {     te3p[m][mm]= 0.410999; } 
	  if(m==48 && mm == 2) {     tw0n[m][mm]= 0.024536; } 
	  if(m==48 && mm == 3) {     tw0n[m][mm]= 0.035840; } 
	  if(m==48 && mm == 14) {     tw0n[m][mm]= 0.162251; } 
	  if(m==48 && mm == 19) {     tw0n[m][mm]= 0.005700; } 
	  if(m==48 && mm == 21) {     tw0n[m][mm]= 0.065531; } 
	  if(m==48 && mm == 22) {     tw0n[m][mm]= 0.003307; } 
	  if(m==48 && mm == 24) {     tw0n[m][mm]= 0.036356; } 
	  if(m==48 && mm == 26) {     tw0n[m][mm]= 0.041325; } 
	  if(m==48 && mm == 6) {     tw1n[m][mm]= 0.016758; } 
	  if(m==48 && mm == 11) {     tw1n[m][mm]= 0.170030; } 
	  if(m==48 && mm == 12) {     tw1n[m][mm]= 0.062351; } 
	  if(m==48 && mm == 16) {     tw1n[m][mm]= 0.082509; } 
	  if(m==48 && mm == 31) {     tw1n[m][mm]= 0.148287; } 
	  if(m==48 && mm == 34) {     tw1n[m][mm]= 0.401714; } 
	  if(m==48 && mm == 4) {     tw2n[m][mm]= 0.045681; } 
	  if(m==48 && mm == 6) {     tw2n[m][mm]= 0.028180; } 
	  if(m==48 && mm == 20) {     tw2n[m][mm]= 0.324242; } 
	  if(m==48 && mm == 21) {     tw2n[m][mm]= 0.280468; } 
	  if(m==48 && mm == 28) {     tw2n[m][mm]= 0.053034; } 
	  if(m==48 && mm == 30) {     tw2n[m][mm]= 0.756381; } 
	  if(m==48 && mm == 31) {     tw2n[m][mm]= 0.086219; } 
	  if(m==48 && mm == 32) {     tw2n[m][mm]= 0.063356; } 
	  if(m==48 && mm == 34) {     tw2n[m][mm]= 0.376946; } 
	  if(m==48 && mm == 35) {     tw2n[m][mm]= 0.121359; } 
	  if(m==48 && mm == 0) {     tw3n[m][mm]= 0.047973; } 
	  if(m==48 && mm == 0) {     te2n[m][mm]= 0.171886; } 
	  if(m==48 && mm == 4) {     te2n[m][mm]= 0.036057; } 
	  if(m==48 && mm == 8) {     te2n[m][mm]= 0.066541; } 
	  if(m==48 && mm == 11) {     te2n[m][mm]= 0.255008; } 
	  if(m==48 && mm == 18) {     te2n[m][mm]= 0.011419; } 
	  if(m==48 && mm == 23) {     te2n[m][mm]= 0.042980; } 
	  if(m==48 && mm == 24) {     te2n[m][mm]= 0.080880; } 
	  if(m==48 && mm == 26) {     te2n[m][mm]= 0.158769; } 
	  if(m==48 && mm == 28) {     te2n[m][mm]= 0.061788; } 
	  if(m==48 && mm == 30) {     te2n[m][mm]= 0.167704; } 
	  if(m==48 && mm == 31) {     te2n[m][mm]= 0.205358; } 
	  if(m==48 && mm == 32) {     te2n[m][mm]= 0.298644; } 
	  if(m==48 && mm == 34) {     te2n[m][mm]= 0.404620; } 
	  if(m==48 && mm == 35) {     te2n[m][mm]= 0.288233; } 
	  if(m==48 && mm == 0) {     te3n[m][mm]= 0.139292; } 
	  if(m==48 && mm == 16) {     te3n[m][mm]= 0.214742; } 
	  if(m==48 && mm == 20) {     te3n[m][mm]= 0.117559; } 
	  if(m==48 && mm == 21) {     te3n[m][mm]= 0.076273; } 
	  if(m==48 && mm == 23) {     te3n[m][mm]= 0.003766; } 
	  if(m==48 && mm == 26) {     te3n[m][mm]= 0.034798; } 
	  if(m==48 && mm == 30) {     te3n[m][mm]= 0.245487; } 
	  if(m==49 && mm == 12) {     tw0p[m][mm]= 0.211817; } 
	  if(m==49 && mm == 13) {     tw0p[m][mm]= 0.042142; } 
	  if(m==49 && mm == 17) {     tw0p[m][mm]= 0.149116; } 
	  if(m==49 && mm == 18) {     tw0p[m][mm]= 0.247207; } 
	  if(m==49 && mm == 22) {     tw0p[m][mm]= 0.047900; } 
	  if(m==49 && mm == 23) {     tw0p[m][mm]= 0.288518; } 
	  if(m==49 && mm == 24) {     tw0p[m][mm]= 0.082902; } 
	  if(m==49 && mm == 29) {     tw0p[m][mm]= 0.573083; } 
	  if(m==49 && mm == 30) {     tw0p[m][mm]= 0.215021; } 
	  if(m==49 && mm == 31) {     tw0p[m][mm]= 0.531085; } 
	  if(m==49 && mm == 32) {     tw0p[m][mm]= 0.204416; } 
	  if(m==49 && mm == 33) {     tw0p[m][mm]= 0.243740; } 
	  if(m==49 && mm == 34) {     tw0p[m][mm]= 0.122950; } 
	  if(m==49 && mm == 35) {     tw0p[m][mm]= 0.269868; } 
	  if(m==49 && mm == 2) {     tw1p[m][mm]= 0.093578; } 
	  if(m==49 && mm == 3) {     tw1p[m][mm]= 0.136113; } 
	  if(m==49 && mm == 4) {     tw1p[m][mm]= 0.131222; } 
	  if(m==49 && mm == 25) {     tw1p[m][mm]= 0.005733; } 
	  if(m==49 && mm == 26) {     tw1p[m][mm]= 0.003850; } 
	  if(m==49 && mm == 27) {     tw1p[m][mm]= 0.422171; } 
	  if(m==49 && mm == 30) {     tw1p[m][mm]= 0.184956; } 
	  if(m==49 && mm == 32) {     tw1p[m][mm]= 0.205204; } 
	  if(m==49 && mm == 33) {     tw1p[m][mm]= 0.169176; } 
	  if(m==49 && mm == 34) {     tw1p[m][mm]= 0.175652; } 
	  if(m==49 && mm == 35) {     tw1p[m][mm]= 0.731702; } 
	  if(m==49 && mm == 9) {     tw2p[m][mm]= 0.033195; } 
	  if(m==49 && mm == 11) {     tw2p[m][mm]= 0.235025; } 
	  if(m==49 && mm == 13) {     tw2p[m][mm]= 0.016483; } 
	  if(m==49 && mm == 14) {     tw2p[m][mm]= 0.095596; } 
	  if(m==49 && mm == 20) {     tw2p[m][mm]= 0.168612; } 
	  if(m==49 && mm == 21) {     tw2p[m][mm]= 0.010703; } 
	  if(m==49 && mm == 22) {     tw2p[m][mm]= 0.095579; } 
	  if(m==49 && mm == 32) {     tw2p[m][mm]= 0.434956; } 
	  if(m==49 && mm == 33) {     tw2p[m][mm]= 0.386234; } 
	  if(m==49 && mm == 34) {     tw2p[m][mm]= 0.457296; } 
	  if(m==49 && mm == 30) {     tw3p[m][mm]= 0.153291; } 
	  if(m==49 && mm == 32) {     tw3p[m][mm]= 0.195901; } 
	  if(m==49 && mm == 33) {     tw3p[m][mm]= 0.432630; } 
	  if(m==49 && mm == 34) {     tw3p[m][mm]= 0.438969; } 
	  if(m==49 && mm == 35) {     tw3p[m][mm]= 0.184377; } 
	  if(m==49 && mm == 0) {     te2p[m][mm]= 0.161275; } 
	  if(m==49 && mm == 1) {     te2p[m][mm]= 0.323096; } 
	  if(m==49 && mm == 2) {     te2p[m][mm]= 0.066373; } 
	  if(m==49 && mm == 4) {     te2p[m][mm]= 0.260878; } 
	  if(m==49 && mm == 5) {     te2p[m][mm]= 0.286639; } 
	  if(m==49 && mm == 7) {     te2p[m][mm]= 0.122509; } 
	  if(m==49 && mm == 8) {     te2p[m][mm]= 0.025815; } 
	  if(m==49 && mm == 11) {     te2p[m][mm]= 0.302241; } 
	  if(m==49 && mm == 16) {     te2p[m][mm]= 0.113513; } 
	  if(m==49 && mm == 18) {     te2p[m][mm]= 0.016204; } 
	  if(m==49 && mm == 28) {     te2p[m][mm]= 0.116596; } 
	  if(m==49 && mm == 1) {     te3p[m][mm]= 1.079338; } 
	  if(m==49 && mm == 2) {     te3p[m][mm]= 0.055813; } 
	  if(m==49 && mm == 3) {     te3p[m][mm]= 0.566550; } 
	  if(m==49 && mm == 4) {     te3p[m][mm]= 0.820046; } 
	  if(m==49 && mm == 11) {     te3p[m][mm]= 0.101105; } 
	  if(m==49 && mm == 16) {     te3p[m][mm]= 0.078801; } 
	  if(m==49 && mm == 18) {     te3p[m][mm]= 0.122028; } 
	  if(m==49 && mm == 22) {     te3p[m][mm]= 0.028267; } 
	  if(m==49 && mm == 1) {     tw0n[m][mm]= 0.007543; } 
	  if(m==49 && mm == 12) {     tw0n[m][mm]= 0.500100; } 
	  if(m==49 && mm == 13) {     tw0n[m][mm]= 0.062413; } 
	  if(m==49 && mm == 19) {     tw0n[m][mm]= 0.220060; } 
	  if(m==49 && mm == 20) {     tw0n[m][mm]= 0.018849; } 
	  if(m==49 && mm == 21) {     tw0n[m][mm]= 0.081763; } 
	  if(m==49 && mm == 23) {     tw0n[m][mm]= 0.025305; } 
	  if(m==49 && mm == 30) {     tw1n[m][mm]= 0.047205; } 
	  if(m==49 && mm == 35) {     tw1n[m][mm]= 0.230063; } 
	  if(m==49 && mm == 5) {     tw2n[m][mm]= 0.179045; } 
	  if(m==49 && mm == 14) {     tw2n[m][mm]= 0.089596; } 
	  if(m==49 && mm == 20) {     tw2n[m][mm]= 0.002017; } 
	  if(m==49 && mm == 22) {     tw2n[m][mm]= 0.102853; } 
	  if(m==49 && mm == 23) {     tw2n[m][mm]= 0.067162; } 
	  if(m==49 && mm == 25) {     tw2n[m][mm]= 0.216965; } 
	  if(m==49 && mm == 32) {     tw2n[m][mm]= 0.100277; } 
	  if(m==49 && mm == 33) {     tw2n[m][mm]= 0.276358; } 
	  if(m==49 && mm == 34) {     tw2n[m][mm]= 0.217920; } 
	  if(m==49 && mm == 7) {     te2n[m][mm]= 0.108804; } 
	  if(m==49 && mm == 11) {     te2n[m][mm]= 0.009932; } 
	  if(m==49 && mm == 20) {     te2n[m][mm]= 0.003386; } 
	  if(m==49 && mm == 22) {     te2n[m][mm]= 0.107421; } 
	  if(m==49 && mm == 30) {     te2n[m][mm]= 0.673007; } 
	  if(m==49 && mm == 31) {     te2n[m][mm]= 0.208604; } 
	  if(m==49 && mm == 33) {     te2n[m][mm]= 0.487623; } 
	  if(m==49 && mm == 34) {     te2n[m][mm]= 0.086570; } 
	  if(m==49 && mm == 1) {     te3n[m][mm]= 0.300548; } 
	  if(m==49 && mm == 3) {     te3n[m][mm]= 0.257485; } 
	  if(m==49 && mm == 4) {     te3n[m][mm]= 0.234494; } 
	  if(m==49 && mm == 16) {     te3n[m][mm]= 0.084415; } 
	  if(m==49 && mm == 26) {     te3n[m][mm]= 0.765550; } 
	  if(m==49 && mm == 27) {     te3n[m][mm]= 0.285932; } 
	  if(m==49 && mm == 28) {     te3n[m][mm]= 0.043245; } 
	  if(m==50 && mm == 8) {     tw0p[m][mm]= 0.213395; } 
	  if(m==50 && mm == 12) {     tw0p[m][mm]= 0.010621; } 
	  if(m==50 && mm == 13) {     tw0p[m][mm]= 0.040560; } 
	  if(m==50 && mm == 14) {     tw0p[m][mm]= 0.153702; } 
	  if(m==50 && mm == 18) {     tw0p[m][mm]= 0.144872; } 
	  if(m==50 && mm == 19) {     tw0p[m][mm]= 0.491596; } 
	  if(m==50 && mm == 20) {     tw0p[m][mm]= 0.010017; } 
	  if(m==50 && mm == 21) {     tw0p[m][mm]= 0.063504; } 
	  if(m==50 && mm == 22) {     tw0p[m][mm]= 0.076708; } 
	  if(m==50 && mm == 23) {     tw0p[m][mm]= 0.082887; } 
	  if(m==50 && mm == 24) {     tw0p[m][mm]= 0.064271; } 
	  if(m==50 && mm == 25) {     tw0p[m][mm]= 0.183838; } 
	  if(m==50 && mm == 26) {     tw0p[m][mm]= 0.207374; } 
	  if(m==50 && mm == 27) {     tw0p[m][mm]= 0.411997; } 
	  if(m==50 && mm == 29) {     tw0p[m][mm]= 0.479035; } 
	  if(m==50 && mm == 30) {     tw0p[m][mm]= 0.441187; } 
	  if(m==50 && mm == 31) {     tw0p[m][mm]= 0.298252; } 
	  if(m==50 && mm == 32) {     tw0p[m][mm]= 0.579573; } 
	  if(m==50 && mm == 33) {     tw0p[m][mm]= 0.197071; } 
	  if(m==50 && mm == 34) {     tw0p[m][mm]= 0.328502; } 
	  if(m==50 && mm == 5) {     tw1p[m][mm]= 0.172112; } 
	  if(m==50 && mm == 6) {     tw1p[m][mm]= 0.085914; } 
	  if(m==50 && mm == 22) {     tw1p[m][mm]= 0.082316; } 
	  if(m==50 && mm == 23) {     tw1p[m][mm]= 0.082774; } 
	  if(m==50 && mm == 24) {     tw1p[m][mm]= 0.433154; } 
	  if(m==50 && mm == 25) {     tw1p[m][mm]= 0.126084; } 
	  if(m==50 && mm == 26) {     tw1p[m][mm]= 0.507320; } 
	  if(m==50 && mm == 27) {     tw1p[m][mm]= 0.234595; } 
	  if(m==50 && mm == 31) {     tw1p[m][mm]= 0.146524; } 
	  if(m==50 && mm == 32) {     tw1p[m][mm]= 0.501158; } 
	  if(m==50 && mm == 33) {     tw1p[m][mm]= 0.030112; } 
	  if(m==50 && mm == 34) {     tw1p[m][mm]= 0.228067; } 
	  if(m==50 && mm == 35) {     tw1p[m][mm]= 0.307084; } 
	  if(m==50 && mm == 6) {     tw2p[m][mm]= 0.337246; } 
	  if(m==50 && mm == 7) {     tw2p[m][mm]= 0.893128; } 
	  if(m==50 && mm == 8) {     tw2p[m][mm]= 0.124754; } 
	  if(m==50 && mm == 9) {     tw2p[m][mm]= 0.103261; } 
	  if(m==50 && mm == 10) {     tw2p[m][mm]= 0.050845; } 
	  if(m==50 && mm == 11) {     tw2p[m][mm]= 0.384541; } 
	  if(m==50 && mm == 13) {     tw2p[m][mm]= 0.112769; } 
	  if(m==50 && mm == 17) {     tw2p[m][mm]= 0.438379; } 
	  if(m==50 && mm == 18) {     tw2p[m][mm]= 0.052867; } 
	  if(m==50 && mm == 19) {     tw2p[m][mm]= 0.261148; } 
	  if(m==50 && mm == 20) {     tw2p[m][mm]= 0.139464; } 
	  if(m==50 && mm == 22) {     tw2p[m][mm]= 0.319298; } 
	  if(m==50 && mm == 30) {     tw2p[m][mm]= 0.208701; } 
	  if(m==50 && mm == 32) {     tw2p[m][mm]= 0.527914; } 
	  if(m==50 && mm == 33) {     tw2p[m][mm]= 0.083382; } 
	  if(m==50 && mm == 34) {     tw2p[m][mm]= 0.524149; } 
	  if(m==50 && mm == 0) {     tw3p[m][mm]= 0.257227; } 
	  if(m==50 && mm == 32) {     tw3p[m][mm]= 0.078713; } 
	  if(m==50 && mm == 33) {     tw3p[m][mm]= 0.568279; } 
	  if(m==50 && mm == 34) {     tw3p[m][mm]= 1.292786; } 
	  if(m==50 && mm == 35) {     tw3p[m][mm]= 1.041843; } 
	  if(m==50 && mm == 0) {     te2p[m][mm]= 0.565707; } 
	  if(m==50 && mm == 2) {     te2p[m][mm]= 0.169292; } 
	  if(m==50 && mm == 3) {     te2p[m][mm]= 0.168291; } 
	  if(m==50 && mm == 4) {     te2p[m][mm]= 0.630585; } 
	  if(m==50 && mm == 6) {     te2p[m][mm]= 0.561222; } 
	  if(m==50 && mm == 8) {     te2p[m][mm]= 0.242890; } 
	  if(m==50 && mm == 9) {     te2p[m][mm]= 0.176143; } 
	  if(m==50 && mm == 10) {     te2p[m][mm]= 0.198593; } 
	  if(m==50 && mm == 11) {     te2p[m][mm]= 0.052055; } 
	  if(m==50 && mm == 12) {     te2p[m][mm]= 0.429865; } 
	  if(m==50 && mm == 14) {     te2p[m][mm]= 0.033116; } 
	  if(m==50 && mm == 16) {     te2p[m][mm]= 0.123359; } 
	  if(m==50 && mm == 18) {     te2p[m][mm]= 0.062038; } 
	  if(m==50 && mm == 19) {     te2p[m][mm]= 0.087779; } 
	  if(m==50 && mm == 21) {     te2p[m][mm]= 0.259574; } 
	  if(m==50 && mm == 22) {     te2p[m][mm]= 0.185326; } 
	  if(m==50 && mm == 23) {     te2p[m][mm]= 0.221377; } 
	  if(m==50 && mm == 24) {     te2p[m][mm]= 0.053172; } 
	  if(m==50 && mm == 29) {     te2p[m][mm]= 0.132168; } 
	  if(m==50 && mm == 2) {     te3p[m][mm]= 0.522815; } 
	  if(m==50 && mm == 4) {     te3p[m][mm]= 0.323240; } 
	  if(m==50 && mm == 5) {     te3p[m][mm]= 0.196044; } 
	  if(m==50 && mm == 6) {     te3p[m][mm]= 0.126873; } 
	  if(m==50 && mm == 7) {     te3p[m][mm]= 0.149026; } 
	  if(m==50 && mm == 10) {     te3p[m][mm]= 0.117689; } 
	  if(m==50 && mm == 11) {     te3p[m][mm]= 0.429351; } 
	  if(m==50 && mm == 12) {     te3p[m][mm]= 0.160111; } 
	  if(m==50 && mm == 18) {     te3p[m][mm]= 0.339305; } 
	  if(m==50 && mm == 20) {     te3p[m][mm]= 0.031476; } 
	  if(m==50 && mm == 21) {     te3p[m][mm]= 0.024099; } 
	  if(m==50 && mm == 22) {     te3p[m][mm]= 0.167933; } 
	  if(m==50 && mm == 23) {     te3p[m][mm]= 0.200304; } 
	  if(m==50 && mm == 0) {     tw0n[m][mm]= 0.168684; } 
	  if(m==50 && mm == 2) {     tw0n[m][mm]= 0.206342; } 
	  if(m==50 && mm == 3) {     tw0n[m][mm]= 0.613277; } 
	  if(m==50 && mm == 7) {     tw0n[m][mm]= 0.051664; } 
	  if(m==50 && mm == 8) {     tw0n[m][mm]= 0.148373; } 
	  if(m==50 && mm == 13) {     tw0n[m][mm]= 0.300572; } 
	  if(m==50 && mm == 14) {     tw0n[m][mm]= 0.231159; } 
	  if(m==50 && mm == 16) {     tw0n[m][mm]= 0.647169; } 
	  if(m==50 && mm == 19) {     tw0n[m][mm]= 0.251197; } 
	  if(m==50 && mm == 22) {     tw0n[m][mm]= 0.022414; } 
	  if(m==50 && mm == 24) {     tw0n[m][mm]= 0.031188; } 
	  if(m==50 && mm == 25) {     tw0n[m][mm]= 0.024913; } 
	  if(m==50 && mm == 26) {     tw0n[m][mm]= 0.128930; } 
	  if(m==50 && mm == 27) {     tw0n[m][mm]= 0.126655; } 
	  if(m==50 && mm == 29) {     tw0n[m][mm]= 0.153190; } 
	  if(m==50 && mm == 11) {     tw1n[m][mm]= 0.091970; } 
	  if(m==50 && mm == 12) {     tw1n[m][mm]= 0.142217; } 
	  if(m==50 && mm == 16) {     tw1n[m][mm]= 0.383948; } 
	  if(m==50 && mm == 18) {     tw1n[m][mm]= 0.157992; } 
	  if(m==50 && mm == 23) {     tw1n[m][mm]= 0.063124; } 
	  if(m==50 && mm == 32) {     tw1n[m][mm]= 0.250432; } 
	  if(m==50 && mm == 34) {     tw1n[m][mm]= 0.000107; } 
	  if(m==50 && mm == 35) {     tw1n[m][mm]= 0.025698; } 
	  if(m==50 && mm == 2) {     tw2n[m][mm]= 0.278329; } 
	  if(m==50 && mm == 3) {     tw2n[m][mm]= 0.213942; } 
	  if(m==50 && mm == 6) {     tw2n[m][mm]= 0.176170; } 
	  if(m==50 && mm == 7) {     tw2n[m][mm]= 1.159633; } 
	  if(m==50 && mm == 15) {     tw2n[m][mm]= 0.120262; } 
	  if(m==50 && mm == 19) {     tw2n[m][mm]= 0.148617; } 
	  if(m==50 && mm == 20) {     tw2n[m][mm]= 0.032095; } 
	  if(m==50 && mm == 22) {     tw2n[m][mm]= 0.144064; } 
	  if(m==50 && mm == 23) {     tw2n[m][mm]= 0.008474; } 
	  if(m==50 && mm == 30) {     tw2n[m][mm]= 0.014223; } 
	  if(m==50 && mm == 32) {     tw2n[m][mm]= 0.256391; } 
	  if(m==50 && mm == 34) {     tw2n[m][mm]= 0.456883; } 
	  if(m==50 && mm == 0) {     tw3n[m][mm]= 0.216522; } 
	  if(m==50 && mm == 8) {     tw3n[m][mm]= 0.042598; } 
	  if(m==50 && mm == 4) {     te2n[m][mm]= 1.116125; } 
	  if(m==50 && mm == 6) {     te2n[m][mm]= 0.051870; } 
	  if(m==50 && mm == 12) {     te2n[m][mm]= 0.101466; } 
	  if(m==50 && mm == 16) {     te2n[m][mm]= 0.135972; } 
	  if(m==50 && mm == 20) {     te2n[m][mm]= 0.009757; } 
	  if(m==50 && mm == 21) {     te2n[m][mm]= 0.082543; } 
	  if(m==50 && mm == 22) {     te2n[m][mm]= 0.200348; } 
	  if(m==50 && mm == 23) {     te2n[m][mm]= 0.107196; } 
	  if(m==50 && mm == 24) {     te2n[m][mm]= 0.233728; } 
	  if(m==50 && mm == 26) {     te2n[m][mm]= 0.032509; } 
	  if(m==50 && mm == 28) {     te2n[m][mm]= 0.178247; } 
	  if(m==50 && mm == 29) {     te2n[m][mm]= 0.008351; } 
	  if(m==50 && mm == 32) {     te2n[m][mm]= 0.403654; } 
	  if(m==50 && mm == 33) {     te2n[m][mm]= 0.355742; } 
	  if(m==50 && mm == 34) {     te2n[m][mm]= 0.436983; } 
	  if(m==50 && mm == 0) {     te3n[m][mm]= 0.297648; } 
	  if(m==50 && mm == 1) {     te3n[m][mm]= 0.139656; } 
	  if(m==50 && mm == 3) {     te3n[m][mm]= 0.022663; } 
	  if(m==50 && mm == 4) {     te3n[m][mm]= 0.391445; } 
	  if(m==50 && mm == 23) {     te3n[m][mm]= 0.065757; } 
	  if(m==50 && mm == 25) {     te3n[m][mm]= 0.079122; } 
	  if(m==50 && mm == 27) {     te3n[m][mm]= 1.351658; } 
	  if(m==50 && mm == 32) {     te3n[m][mm]= 0.022338; } 
	  if(m==50 && mm == 34) {     te3n[m][mm]= 0.130589; } 
	  if(m==50 && mm == 35) {     te3n[m][mm]= 0.240806; } 
	  if(m==51 && mm == 18) {     tw0p[m][mm]= 0.216368; } 
	  if(m==51 && mm == 22) {     tw0p[m][mm]= 0.698020; } 
	  if(m==51 && mm == 23) {     tw0p[m][mm]= 0.530139; } 
	  if(m==51 && mm == 24) {     tw0p[m][mm]= 0.146819; } 
	  if(m==51 && mm == 26) {     tw0p[m][mm]= 0.355445; } 
	  if(m==51 && mm == 27) {     tw0p[m][mm]= 0.025321; } 
	  if(m==51 && mm == 28) {     tw0p[m][mm]= 0.195553; } 
	  if(m==51 && mm == 29) {     tw0p[m][mm]= 0.397749; } 
	  if(m==51 && mm == 30) {     tw0p[m][mm]= 0.218208; } 
	  if(m==51 && mm == 31) {     tw0p[m][mm]= 0.784494; } 
	  if(m==51 && mm == 32) {     tw0p[m][mm]= 0.263823; } 
	  if(m==51 && mm == 33) {     tw0p[m][mm]= 0.270588; } 
	  if(m==51 && mm == 34) {     tw0p[m][mm]= 0.336378; } 
	  if(m==51 && mm == 35) {     tw0p[m][mm]= 0.335188; } 
	  if(m==51 && mm == 0) {     tw1p[m][mm]= 0.217036; } 
	  if(m==51 && mm == 4) {     tw1p[m][mm]= 0.166689; } 
	  if(m==51 && mm == 6) {     tw1p[m][mm]= 0.381828; } 
	  if(m==51 && mm == 19) {     tw1p[m][mm]= 0.547676; } 
	  if(m==51 && mm == 21) {     tw1p[m][mm]= 0.064391; } 
	  if(m==51 && mm == 23) {     tw1p[m][mm]= 0.417463; } 
	  if(m==51 && mm == 24) {     tw1p[m][mm]= 0.068448; } 
	  if(m==51 && mm == 25) {     tw1p[m][mm]= 0.026502; } 
	  if(m==51 && mm == 26) {     tw1p[m][mm]= 0.032681; } 
	  if(m==51 && mm == 27) {     tw1p[m][mm]= 0.144331; } 
	  if(m==51 && mm == 29) {     tw1p[m][mm]= 0.232291; } 
	  if(m==51 && mm == 30) {     tw1p[m][mm]= 0.271773; } 
	  if(m==51 && mm == 33) {     tw1p[m][mm]= 0.207421; } 
	  if(m==51 && mm == 34) {     tw1p[m][mm]= 0.530978; } 
	  if(m==51 && mm == 6) {     tw2p[m][mm]= 0.108416; } 
	  if(m==51 && mm == 9) {     tw2p[m][mm]= 0.068240; } 
	  if(m==51 && mm == 10) {     tw2p[m][mm]= 0.006985; } 
	  if(m==51 && mm == 11) {     tw2p[m][mm]= 0.264869; } 
	  if(m==51 && mm == 13) {     tw2p[m][mm]= 0.036240; } 
	  if(m==51 && mm == 14) {     tw2p[m][mm]= 0.097800; } 
	  if(m==51 && mm == 16) {     tw2p[m][mm]= 0.295514; } 
	  if(m==51 && mm == 18) {     tw2p[m][mm]= 0.058068; } 
	  if(m==51 && mm == 19) {     tw2p[m][mm]= 0.131080; } 
	  if(m==51 && mm == 28) {     tw2p[m][mm]= 0.062935; } 
	  if(m==51 && mm == 29) {     tw2p[m][mm]= 0.053720; } 
	  if(m==51 && mm == 31) {     tw2p[m][mm]= 0.444941; } 
	  if(m==51 && mm == 34) {     tw2p[m][mm]= 0.089193; } 
	  if(m==51 && mm == 32) {     tw3p[m][mm]= 0.581415; } 
	  if(m==51 && mm == 34) {     tw3p[m][mm]= 1.591158; } 
	  if(m==51 && mm == 35) {     tw3p[m][mm]= 0.472596; } 
	  if(m==51 && mm == 0) {     te2p[m][mm]= 0.403990; } 
	  if(m==51 && mm == 1) {     te2p[m][mm]= 0.124170; } 
	  if(m==51 && mm == 4) {     te2p[m][mm]= 0.135444; } 
	  if(m==51 && mm == 5) {     te2p[m][mm]= 0.008889; } 
	  if(m==51 && mm == 6) {     te2p[m][mm]= 0.392951; } 
	  if(m==51 && mm == 7) {     te2p[m][mm]= 0.339627; } 
	  if(m==51 && mm == 8) {     te2p[m][mm]= 0.268893; } 
	  if(m==51 && mm == 9) {     te2p[m][mm]= 0.071193; } 
	  if(m==51 && mm == 10) {     te2p[m][mm]= 0.078694; } 
	  if(m==51 && mm == 11) {     te2p[m][mm]= 0.003025; } 
	  if(m==51 && mm == 13) {     te2p[m][mm]= 0.104753; } 
	  if(m==51 && mm == 14) {     te2p[m][mm]= 0.202782; } 
	  if(m==51 && mm == 19) {     te2p[m][mm]= 0.020792; } 
	  if(m==51 && mm == 21) {     te2p[m][mm]= 0.019388; } 
	  if(m==51 && mm == 22) {     te2p[m][mm]= 0.127408; } 
	  if(m==51 && mm == 23) {     te2p[m][mm]= 0.120614; } 
	  if(m==51 && mm == 26) {     te2p[m][mm]= 0.035332; } 
	  if(m==51 && mm == 29) {     te2p[m][mm]= 0.183941; } 
	  if(m==51 && mm == 30) {     te2p[m][mm]= 0.196043; } 
	  if(m==51 && mm == 3) {     te3p[m][mm]= 0.857791; } 
	  if(m==51 && mm == 5) {     te3p[m][mm]= 0.081064; } 
	  if(m==51 && mm == 6) {     te3p[m][mm]= 0.824257; } 
	  if(m==51 && mm == 7) {     te3p[m][mm]= 0.282559; } 
	  if(m==51 && mm == 10) {     te3p[m][mm]= 0.463606; } 
	  if(m==51 && mm == 11) {     te3p[m][mm]= 0.290163; } 
	  if(m==51 && mm == 12) {     te3p[m][mm]= 0.231949; } 
	  if(m==51 && mm == 14) {     te3p[m][mm]= 0.148809; } 
	  if(m==51 && mm == 18) {     te3p[m][mm]= 0.374337; } 
	  if(m==51 && mm == 21) {     te3p[m][mm]= 0.037710; } 
	  if(m==51 && mm == 23) {     te3p[m][mm]= 0.065012; } 
	  if(m==51 && mm == 23) {     tw0n[m][mm]= 0.153679; } 
	  if(m==51 && mm == 26) {     tw0n[m][mm]= 0.047185; } 
	  if(m==51 && mm == 28) {     tw0n[m][mm]= 0.001519; } 
	  if(m==51 && mm == 7) {     tw1n[m][mm]= 0.212458; } 
	  if(m==51 && mm == 8) {     tw1n[m][mm]= 0.402809; } 
	  if(m==51 && mm == 9) {     tw1n[m][mm]= 0.253789; } 
	  if(m==51 && mm == 33) {     tw1n[m][mm]= 0.033866; } 
	  if(m==51 && mm == 2) {     tw2n[m][mm]= 0.115359; } 
	  if(m==51 && mm == 4) {     tw2n[m][mm]= 0.078905; } 
	  if(m==51 && mm == 6) {     tw2n[m][mm]= 0.229953; } 
	  if(m==51 && mm == 14) {     tw2n[m][mm]= 0.131233; } 
	  if(m==51 && mm == 16) {     tw2n[m][mm]= 0.100073; } 
	  if(m==51 && mm == 19) {     tw2n[m][mm]= 0.088060; } 
	  if(m==51 && mm == 25) {     tw2n[m][mm]= 0.082123; } 
	  if(m==51 && mm == 0) {     tw3n[m][mm]= 0.039264; } 
	  if(m==51 && mm == 7) {     te2n[m][mm]= 0.135519; } 
	  if(m==51 && mm == 8) {     te2n[m][mm]= 0.025597; } 
	  if(m==51 && mm == 11) {     te2n[m][mm]= 0.342549; } 
	  if(m==51 && mm == 19) {     te2n[m][mm]= 0.119026; } 
	  if(m==51 && mm == 21) {     te2n[m][mm]= 0.056067; } 
	  if(m==51 && mm == 22) {     te2n[m][mm]= 0.030820; } 
	  if(m==51 && mm == 24) {     te2n[m][mm]= 0.144360; } 
	  if(m==51 && mm == 26) {     te2n[m][mm]= 0.298078; } 
	  if(m==51 && mm == 29) {     te2n[m][mm]= 0.339171; } 
	  if(m==51 && mm == 30) {     te2n[m][mm]= 0.320410; } 
	  if(m==51 && mm == 32) {     te2n[m][mm]= 0.142017; } 
	  if(m==51 && mm == 34) {     te2n[m][mm]= 0.075065; } 
	  if(m==51 && mm == 35) {     te2n[m][mm]= 0.020427; } 
	  if(m==51 && mm == 1) {     te3n[m][mm]= 0.045165; } 
	  if(m==51 && mm == 2) {     te3n[m][mm]= 0.155322; } 
	  if(m==51 && mm == 14) {     te3n[m][mm]= 0.227785; } 
	  if(m==51 && mm == 18) {     te3n[m][mm]= 0.367937; } 
	  if(m==51 && mm == 21) {     te3n[m][mm]= 0.035421; } 
	  if(m==51 && mm == 23) {     te3n[m][mm]= 0.012424; } 
	  if(m==51 && mm == 28) {     te3n[m][mm]= 0.906004; } 
	  if(m==51 && mm == 29) {     te3n[m][mm]= 0.448233; } 
	  if(m==51 && mm == 30) {     te3n[m][mm]= 0.476356; } 
	  if(m==51 && mm == 31) {     te3n[m][mm]= 0.116957; } 
	  if(m==51 && mm == 32) {     te3n[m][mm]= 0.075114; } 
	  if(m==51 && mm == 34) {     te3n[m][mm]= 0.122889; } 
	  if(m==51 && mm == 35) {     te3n[m][mm]= 0.288615; } 
	  if(m==52 && mm == 13) {     tw0p[m][mm]= 0.193642; } 
	  if(m==52 && mm == 14) {     tw0p[m][mm]= 0.048473; } 
	  if(m==52 && mm == 15) {     tw0p[m][mm]= 0.381037; } 
	  if(m==52 && mm == 16) {     tw0p[m][mm]= 0.196784; } 
	  if(m==52 && mm == 18) {     tw0p[m][mm]= 0.072196; } 
	  if(m==52 && mm == 20) {     tw0p[m][mm]= 0.411487; } 
	  if(m==52 && mm == 21) {     tw0p[m][mm]= 0.225614; } 
	  if(m==52 && mm == 22) {     tw0p[m][mm]= 0.061621; } 
	  if(m==52 && mm == 23) {     tw0p[m][mm]= 0.486992; } 
	  if(m==52 && mm == 24) {     tw0p[m][mm]= 0.315196; } 
	  if(m==52 && mm == 25) {     tw0p[m][mm]= 0.422060; } 
	  if(m==52 && mm == 26) {     tw0p[m][mm]= 0.150698; } 
	  if(m==52 && mm == 27) {     tw0p[m][mm]= 0.126034; } 
	  if(m==52 && mm == 28) {     tw0p[m][mm]= 0.462830; } 
	  if(m==52 && mm == 29) {     tw0p[m][mm]= 0.215923; } 
	  if(m==52 && mm == 30) {     tw0p[m][mm]= 0.386973; } 
	  if(m==52 && mm == 31) {     tw0p[m][mm]= 0.163660; } 
	  if(m==52 && mm == 32) {     tw0p[m][mm]= 0.354773; } 
	  if(m==52 && mm == 33) {     tw0p[m][mm]= 0.225385; } 
	  if(m==52 && mm == 34) {     tw0p[m][mm]= 0.513462; } 
	  if(m==52 && mm == 35) {     tw0p[m][mm]= 0.206096; } 
	  if(m==52 && mm == 4) {     tw1p[m][mm]= 0.223204; } 
	  if(m==52 && mm == 19) {     tw1p[m][mm]= 0.318894; } 
	  if(m==52 && mm == 22) {     tw1p[m][mm]= 0.071323; } 
	  if(m==52 && mm == 23) {     tw1p[m][mm]= 0.050989; } 
	  if(m==52 && mm == 25) {     tw1p[m][mm]= 0.170901; } 
	  if(m==52 && mm == 26) {     tw1p[m][mm]= 0.009479; } 
	  if(m==52 && mm == 27) {     tw1p[m][mm]= 0.500884; } 
	  if(m==52 && mm == 30) {     tw1p[m][mm]= 0.179236; } 
	  if(m==52 && mm == 31) {     tw1p[m][mm]= 0.342709; } 
	  if(m==52 && mm == 34) {     tw1p[m][mm]= 0.835895; } 
	  if(m==52 && mm == 5) {     tw2p[m][mm]= 0.056025; } 
	  if(m==52 && mm == 6) {     tw2p[m][mm]= 0.081295; } 
	  if(m==52 && mm == 7) {     tw2p[m][mm]= 0.195775; } 
	  if(m==52 && mm == 9) {     tw2p[m][mm]= 0.165707; } 
	  if(m==52 && mm == 11) {     tw2p[m][mm]= 0.292122; } 
	  if(m==52 && mm == 12) {     tw2p[m][mm]= 0.278431; } 
	  if(m==52 && mm == 13) {     tw2p[m][mm]= 0.067351; } 
	  if(m==52 && mm == 15) {     tw2p[m][mm]= 0.096799; } 
	  if(m==52 && mm == 16) {     tw2p[m][mm]= 0.016153; } 
	  if(m==52 && mm == 18) {     tw2p[m][mm]= 0.230426; } 
	  if(m==52 && mm == 19) {     tw2p[m][mm]= 0.131120; } 
	  if(m==52 && mm == 20) {     tw2p[m][mm]= 0.351911; } 
	  if(m==52 && mm == 21) {     tw2p[m][mm]= 0.197368; } 
	  if(m==52 && mm == 22) {     tw2p[m][mm]= 0.678737; } 
	  if(m==52 && mm == 23) {     tw2p[m][mm]= 0.208142; } 
	  if(m==52 && mm == 29) {     tw2p[m][mm]= 0.247548; } 
	  if(m==52 && mm == 30) {     tw2p[m][mm]= 0.721894; } 
	  if(m==52 && mm == 31) {     tw2p[m][mm]= 0.293640; } 
	  if(m==52 && mm == 32) {     tw2p[m][mm]= 0.272604; } 
	  if(m==52 && mm == 33) {     tw2p[m][mm]= 0.106150; } 
	  if(m==52 && mm == 34) {     tw2p[m][mm]= 0.131428; } 
	  if(m==52 && mm == 0) {     tw3p[m][mm]= 0.272372; } 
	  if(m==52 && mm == 7) {     tw3p[m][mm]= 0.001025; } 
	  if(m==52 && mm == 9) {     tw3p[m][mm]= 0.018297; } 
	  if(m==52 && mm == 17) {     tw3p[m][mm]= 0.224423; } 
	  if(m==52 && mm == 32) {     tw3p[m][mm]= 0.373252; } 
	  if(m==52 && mm == 34) {     tw3p[m][mm]= 1.156536; } 
	  if(m==52 && mm == 35) {     tw3p[m][mm]= 0.426089; } 
	  if(m==52 && mm == 0) {     te2p[m][mm]= 0.261549; } 
	  if(m==52 && mm == 1) {     te2p[m][mm]= 0.064556; } 
	  if(m==52 && mm == 2) {     te2p[m][mm]= 0.240484; } 
	  if(m==52 && mm == 4) {     te2p[m][mm]= 0.274674; } 
	  if(m==52 && mm == 6) {     te2p[m][mm]= 0.158778; } 
	  if(m==52 && mm == 7) {     te2p[m][mm]= 0.218320; } 
	  if(m==52 && mm == 9) {     te2p[m][mm]= 0.311023; } 
	  if(m==52 && mm == 11) {     te2p[m][mm]= 0.227478; } 
	  if(m==52 && mm == 12) {     te2p[m][mm]= 0.008132; } 
	  if(m==52 && mm == 14) {     te2p[m][mm]= 0.598484; } 
	  if(m==52 && mm == 15) {     te2p[m][mm]= 0.042759; } 
	  if(m==52 && mm == 17) {     te2p[m][mm]= 0.099293; } 
	  if(m==52 && mm == 18) {     te2p[m][mm]= 0.131496; } 
	  if(m==52 && mm == 20) {     te2p[m][mm]= 0.230110; } 
	  if(m==52 && mm == 23) {     te2p[m][mm]= 0.211685; } 
	  if(m==52 && mm == 27) {     te2p[m][mm]= 0.026281; } 
	  if(m==52 && mm == 28) {     te2p[m][mm]= 0.029059; } 
	  if(m==52 && mm == 30) {     te2p[m][mm]= 0.463791; } 
	  if(m==52 && mm == 2) {     te3p[m][mm]= 0.581438; } 
	  if(m==52 && mm == 5) {     te3p[m][mm]= 0.522646; } 
	  if(m==52 && mm == 6) {     te3p[m][mm]= 0.530831; } 
	  if(m==52 && mm == 10) {     te3p[m][mm]= 0.146203; } 
	  if(m==52 && mm == 11) {     te3p[m][mm]= 0.194752; } 
	  if(m==52 && mm == 13) {     te3p[m][mm]= 0.421630; } 
	  if(m==52 && mm == 14) {     te3p[m][mm]= 0.203150; } 
	  if(m==52 && mm == 17) {     te3p[m][mm]= 0.066855; } 
	  if(m==52 && mm == 18) {     te3p[m][mm]= 0.096306; } 
	  if(m==52 && mm == 19) {     te3p[m][mm]= 0.432478; } 
	  if(m==52 && mm == 20) {     te3p[m][mm]= 0.171925; } 
	  if(m==52 && mm == 23) {     te3p[m][mm]= 0.507924; } 
	  if(m==52 && mm == 6) {     tw0n[m][mm]= 0.131152; } 
	  if(m==52 && mm == 13) {     tw0n[m][mm]= 0.013102; } 
	  if(m==52 && mm == 15) {     tw0n[m][mm]= 0.675518; } 
	  if(m==52 && mm == 16) {     tw0n[m][mm]= 0.151374; } 
	  if(m==52 && mm == 18) {     tw0n[m][mm]= 0.149071; } 
	  if(m==52 && mm == 20) {     tw0n[m][mm]= 0.177056; } 
	  if(m==52 && mm == 21) {     tw0n[m][mm]= 0.072480; } 
	  if(m==52 && mm == 22) {     tw0n[m][mm]= 0.035529; } 
	  if(m==52 && mm == 23) {     tw0n[m][mm]= 0.178147; } 
	  if(m==52 && mm == 24) {     tw0n[m][mm]= 0.230682; } 
	  if(m==52 && mm == 26) {     tw0n[m][mm]= 0.129146; } 
	  if(m==52 && mm == 27) {     tw0n[m][mm]= 0.124069; } 
	  if(m==52 && mm == 9) {     tw1n[m][mm]= 0.123217; } 
	  if(m==52 && mm == 10) {     tw1n[m][mm]= 0.058040; } 
	  if(m==52 && mm == 11) {     tw1n[m][mm]= 0.113557; } 
	  if(m==52 && mm == 13) {     tw1n[m][mm]= 0.053444; } 
	  if(m==52 && mm == 16) {     tw1n[m][mm]= 0.106662; } 
	  if(m==52 && mm == 19) {     tw1n[m][mm]= 0.043835; } 
	  if(m==52 && mm == 30) {     tw1n[m][mm]= 0.004434; } 
	  if(m==52 && mm == 32) {     tw1n[m][mm]= 0.127162; } 
	  if(m==52 && mm == 34) {     tw1n[m][mm]= 0.256795; } 
	  if(m==52 && mm == 1) {     tw2n[m][mm]= 0.612504; } 
	  if(m==52 && mm == 5) {     tw2n[m][mm]= 0.069207; } 
	  if(m==52 && mm == 6) {     tw2n[m][mm]= 0.154176; } 
	  if(m==52 && mm == 7) {     tw2n[m][mm]= 0.154356; } 
	  if(m==52 && mm == 14) {     tw2n[m][mm]= 0.104245; } 
	  if(m==52 && mm == 16) {     tw2n[m][mm]= 0.078899; } 
	  if(m==52 && mm == 18) {     tw2n[m][mm]= 0.197647; } 
	  if(m==52 && mm == 19) {     tw2n[m][mm]= 0.001801; } 
	  if(m==52 && mm == 20) {     tw2n[m][mm]= 0.222495; } 
	  if(m==52 && mm == 21) {     tw2n[m][mm]= 0.150463; } 
	  if(m==52 && mm == 22) {     tw2n[m][mm]= 0.209925; } 
	  if(m==52 && mm == 29) {     tw2n[m][mm]= 0.015979; } 
	  if(m==52 && mm == 30) {     tw2n[m][mm]= 0.359146; } 
	  if(m==52 && mm == 31) {     tw2n[m][mm]= 0.083257; } 
	  if(m==52 && mm == 0) {     tw3n[m][mm]= 0.236192; } 
	  if(m==52 && mm == 9) {     tw3n[m][mm]= 0.012249; } 
	  if(m==52 && mm == 1) {     te2n[m][mm]= 0.036142; } 
	  if(m==52 && mm == 4) {     te2n[m][mm]= 0.232728; } 
	  if(m==52 && mm == 11) {     te2n[m][mm]= 0.055958; } 
	  if(m==52 && mm == 14) {     te2n[m][mm]= 0.500296; } 
	  if(m==52 && mm == 18) {     te2n[m][mm]= 0.136539; } 
	  if(m==52 && mm == 20) {     te2n[m][mm]= 0.197816; } 
	  if(m==52 && mm == 22) {     te2n[m][mm]= 0.021064; } 
	  if(m==52 && mm == 27) {     te2n[m][mm]= 0.221341; } 
	  if(m==52 && mm == 28) {     te2n[m][mm]= 0.124050; } 
	  if(m==52 && mm == 30) {     te2n[m][mm]= 0.661141; } 
	  if(m==52 && mm == 31) {     te2n[m][mm]= 0.433901; } 
	  if(m==52 && mm == 32) {     te2n[m][mm]= 0.407138; } 
	  if(m==52 && mm == 33) {     te2n[m][mm]= 0.182846; } 
	  if(m==52 && mm == 35) {     te2n[m][mm]= 0.334895; } 
	  if(m==52 && mm == 2) {     te3n[m][mm]= 0.291141; } 
	  if(m==52 && mm == 5) {     te3n[m][mm]= 0.261624; } 
	  if(m==52 && mm == 6) {     te3n[m][mm]= 0.139613; } 
	  if(m==52 && mm == 14) {     te3n[m][mm]= 0.057369; } 
	  if(m==52 && mm == 19) {     te3n[m][mm]= 0.334546; } 
	  if(m==52 && mm == 23) {     te3n[m][mm]= 0.238419; } 
	  if(m==52 && mm == 24) {     te3n[m][mm]= 0.173322; } 
	  if(m==52 && mm == 28) {     te3n[m][mm]= 0.552114; } 
	  if(m==52 && mm == 29) {     te3n[m][mm]= 0.575312; } 
	  if(m==52 && mm == 30) {     te3n[m][mm]= 0.250241; } 
	  if(m==52 && mm == 32) {     te3n[m][mm]= 0.243560; } 
	  if(m==52 && mm == 34) {     te3n[m][mm]= 0.568625; } 
	  if(m==53 && mm == 12) {     tw0p[m][mm]= 0.198171; } 
	  if(m==53 && mm == 13) {     tw0p[m][mm]= 0.161844; } 
	  if(m==53 && mm == 16) {     tw0p[m][mm]= 0.214673; } 
	  if(m==53 && mm == 20) {     tw0p[m][mm]= 0.506951; } 
	  if(m==53 && mm == 21) {     tw0p[m][mm]= 0.430893; } 
	  if(m==53 && mm == 22) {     tw0p[m][mm]= 0.282689; } 
	  if(m==53 && mm == 24) {     tw0p[m][mm]= 0.603894; } 
	  if(m==53 && mm == 25) {     tw0p[m][mm]= 0.195744; } 
	  if(m==53 && mm == 28) {     tw0p[m][mm]= 0.255480; } 
	  if(m==53 && mm == 30) {     tw0p[m][mm]= 0.050557; } 
	  if(m==53 && mm == 31) {     tw0p[m][mm]= 0.318886; } 
	  if(m==53 && mm == 32) {     tw0p[m][mm]= 0.356094; } 
	  if(m==53 && mm == 33) {     tw0p[m][mm]= 0.191167; } 
	  if(m==53 && mm == 35) {     tw0p[m][mm]= 0.295870; } 
	  if(m==53 && mm == 7) {     tw1p[m][mm]= 0.203675; } 
	  if(m==53 && mm == 8) {     tw1p[m][mm]= 0.081909; } 
	  if(m==53 && mm == 23) {     tw1p[m][mm]= 0.365466; } 
	  if(m==53 && mm == 25) {     tw1p[m][mm]= 0.063349; } 
	  if(m==53 && mm == 29) {     tw1p[m][mm]= 0.441823; } 
	  if(m==53 && mm == 30) {     tw1p[m][mm]= 0.454203; } 
	  if(m==53 && mm == 33) {     tw1p[m][mm]= 0.159404; } 
	  if(m==53 && mm == 34) {     tw1p[m][mm]= 0.475062; } 
	  if(m==53 && mm == 35) {     tw1p[m][mm]= 0.083789; } 
	  if(m==53 && mm == 9) {     tw2p[m][mm]= 0.043266; } 
	  if(m==53 && mm == 10) {     tw2p[m][mm]= 0.219883; } 
	  if(m==53 && mm == 11) {     tw2p[m][mm]= 0.060464; } 
	  if(m==53 && mm == 13) {     tw2p[m][mm]= 0.034444; } 
	  if(m==53 && mm == 14) {     tw2p[m][mm]= 0.044601; } 
	  if(m==53 && mm == 15) {     tw2p[m][mm]= 0.020746; } 
	  if(m==53 && mm == 16) {     tw2p[m][mm]= 0.158266; } 
	  if(m==53 && mm == 18) {     tw2p[m][mm]= 0.054209; } 
	  if(m==53 && mm == 19) {     tw2p[m][mm]= 0.076160; } 
	  if(m==53 && mm == 20) {     tw2p[m][mm]= 0.385868; } 
	  if(m==53 && mm == 21) {     tw2p[m][mm]= 0.130530; } 
	  if(m==53 && mm == 23) {     tw2p[m][mm]= 0.591650; } 
	  if(m==53 && mm == 28) {     tw2p[m][mm]= 0.355532; } 
	  if(m==53 && mm == 30) {     tw2p[m][mm]= 0.308055; } 
	  if(m==53 && mm == 31) {     tw2p[m][mm]= 1.035990; } 
	  if(m==53 && mm == 32) {     tw2p[m][mm]= 0.253153; } 
	  if(m==53 && mm == 33) {     tw2p[m][mm]= 0.329216; } 
	  if(m==53 && mm == 34) {     tw2p[m][mm]= 0.367767; } 
	  if(m==53 && mm == 17) {     tw3p[m][mm]= 0.012981; } 
	  if(m==53 && mm == 32) {     tw3p[m][mm]= 0.250052; } 
	  if(m==53 && mm == 33) {     tw3p[m][mm]= 0.002899; } 
	  if(m==53 && mm == 34) {     tw3p[m][mm]= 1.419277; } 
	  if(m==53 && mm == 35) {     tw3p[m][mm]= 0.679488; } 
	  if(m==53 && mm == 0) {     te2p[m][mm]= 0.205885; } 
	  if(m==53 && mm == 1) {     te2p[m][mm]= 0.664059; } 
	  if(m==53 && mm == 4) {     te2p[m][mm]= 0.090440; } 
	  if(m==53 && mm == 5) {     te2p[m][mm]= 0.403792; } 
	  if(m==53 && mm == 6) {     te2p[m][mm]= 0.365708; } 
	  if(m==53 && mm == 7) {     te2p[m][mm]= 0.243977; } 
	  if(m==53 && mm == 8) {     te2p[m][mm]= 0.218479; } 
	  if(m==53 && mm == 9) {     te2p[m][mm]= 0.410701; } 
	  if(m==53 && mm == 12) {     te2p[m][mm]= 0.220088; } 
	  if(m==53 && mm == 14) {     te2p[m][mm]= 0.279221; } 
	  if(m==53 && mm == 24) {     te2p[m][mm]= 0.337468; } 
	  if(m==53 && mm == 28) {     te2p[m][mm]= 0.052568; } 
	  if(m==53 && mm == 30) {     te2p[m][mm]= 0.333638; } 
	  if(m==53 && mm == 1) {     te3p[m][mm]= 0.116403; } 
	  if(m==53 && mm == 2) {     te3p[m][mm]= 0.143630; } 
	  if(m==53 && mm == 4) {     te3p[m][mm]= 0.538942; } 
	  if(m==53 && mm == 5) {     te3p[m][mm]= 0.258499; } 
	  if(m==53 && mm == 6) {     te3p[m][mm]= 0.002829; } 
	  if(m==53 && mm == 10) {     te3p[m][mm]= 0.131582; } 
	  if(m==53 && mm == 11) {     te3p[m][mm]= 0.543577; } 
	  if(m==53 && mm == 13) {     te3p[m][mm]= 0.284851; } 
	  if(m==53 && mm == 15) {     te3p[m][mm]= 0.316177; } 
	  if(m==53 && mm == 16) {     te3p[m][mm]= 0.061016; } 
	  if(m==53 && mm == 17) {     te3p[m][mm]= 0.491255; } 
	  if(m==53 && mm == 19) {     te3p[m][mm]= 0.386132; } 
	  if(m==53 && mm == 20) {     te3p[m][mm]= 0.245929; } 
	  if(m==53 && mm == 21) {     te3p[m][mm]= 0.440889; } 
	  if(m==53 && mm == 22) {     te3p[m][mm]= 0.189302; } 
	  if(m==53 && mm == 12) {     tw0n[m][mm]= 0.578602; } 
	  if(m==53 && mm == 13) {     tw0n[m][mm]= 0.217823; } 
	  if(m==53 && mm == 16) {     tw0n[m][mm]= 0.140792; } 
	  if(m==53 && mm == 20) {     tw0n[m][mm]= 0.554516; } 
	  if(m==53 && mm == 22) {     tw0n[m][mm]= 0.136662; } 
	  if(m==53 && mm == 7) {     tw1n[m][mm]= 0.615233; } 
	  if(m==53 && mm == 8) {     tw1n[m][mm]= 0.250712; } 
	  if(m==53 && mm == 12) {     tw1n[m][mm]= 0.064834; } 
	  if(m==53 && mm == 14) {     tw1n[m][mm]= 0.606070; } 
	  if(m==53 && mm == 15) {     tw1n[m][mm]= 0.111468; } 
	  if(m==53 && mm == 30) {     tw1n[m][mm]= 0.090552; } 
	  if(m==53 && mm == 35) {     tw1n[m][mm]= 0.083125; } 
	  if(m==53 && mm == 14) {     tw2n[m][mm]= 0.029658; } 
	  if(m==53 && mm == 16) {     tw2n[m][mm]= 0.069621; } 
	  if(m==53 && mm == 19) {     tw2n[m][mm]= 0.035399; } 
	  if(m==53 && mm == 20) {     tw2n[m][mm]= 0.357140; } 
	  if(m==53 && mm == 21) {     tw2n[m][mm]= 0.093904; } 
	  if(m==53 && mm == 23) {     tw2n[m][mm]= 0.297615; } 
	  if(m==53 && mm == 28) {     tw2n[m][mm]= 0.133705; } 
	  if(m==53 && mm == 30) {     tw2n[m][mm]= 0.323782; } 
	  if(m==53 && mm == 33) {     tw2n[m][mm]= 0.162775; } 
	  if(m==53 && mm == 34) {     tw2n[m][mm]= 0.228981; } 
	  if(m==53 && mm == 0) {     tw3n[m][mm]= 0.062069; } 
	  if(m==53 && mm == 5) {     te2n[m][mm]= 0.155600; } 
	  if(m==53 && mm == 6) {     te2n[m][mm]= 0.112710; } 
	  if(m==53 && mm == 8) {     te2n[m][mm]= 0.010604; } 
	  if(m==53 && mm == 9) {     te2n[m][mm]= 0.158850; } 
	  if(m==53 && mm == 14) {     te2n[m][mm]= 0.059389; } 
	  if(m==53 && mm == 17) {     te2n[m][mm]= 0.093201; } 
	  if(m==53 && mm == 20) {     te2n[m][mm]= 0.011514; } 
	  if(m==53 && mm == 24) {     te2n[m][mm]= 0.450505; } 
	  if(m==53 && mm == 25) {     te2n[m][mm]= 0.002734; } 
	  if(m==53 && mm == 27) {     te2n[m][mm]= 0.504963; } 
	  if(m==53 && mm == 28) {     te2n[m][mm]= 0.074739; } 
	  if(m==53 && mm == 29) {     te2n[m][mm]= 0.084254; } 
	  if(m==53 && mm == 30) {     te2n[m][mm]= 0.199951; } 
	  if(m==53 && mm == 31) {     te2n[m][mm]= 0.034220; } 
	  if(m==53 && mm == 32) {     te2n[m][mm]= 0.552600; } 
	  if(m==53 && mm == 33) {     te2n[m][mm]= 0.128131; } 
	  if(m==53 && mm == 4) {     te3n[m][mm]= 0.184147; } 
	  if(m==53 && mm == 15) {     te3n[m][mm]= 0.307773; } 
	  if(m==53 && mm == 17) {     te3n[m][mm]= 0.015908; } 
	  if(m==53 && mm == 21) {     te3n[m][mm]= 0.457732; } 
	  if(m==53 && mm == 22) {     te3n[m][mm]= 0.130162; } 
	  if(m==53 && mm == 26) {     te3n[m][mm]= 0.004979; } 
	  if(m==53 && mm == 28) {     te3n[m][mm]= 0.168718; } 
	  if(m==53 && mm == 31) {     te3n[m][mm]= 0.016966; } 
	  if(m==53 && mm == 32) {     te3n[m][mm]= 0.275722; } 
	  if(m==53 && mm == 33) {     te3n[m][mm]= 0.134073; } 
	  if(m==54 && mm == 12) {     tw0p[m][mm]= 0.558578; } 
	  if(m==54 && mm == 13) {     tw0p[m][mm]= 0.004823; } 
	  if(m==54 && mm == 14) {     tw0p[m][mm]= 0.401824; } 
	  if(m==54 && mm == 16) {     tw0p[m][mm]= 0.086817; } 
	  if(m==54 && mm == 17) {     tw0p[m][mm]= 0.456865; } 
	  if(m==54 && mm == 18) {     tw0p[m][mm]= 0.326465; } 
	  if(m==54 && mm == 19) {     tw0p[m][mm]= 0.365307; } 
	  if(m==54 && mm == 20) {     tw0p[m][mm]= 0.305213; } 
	  if(m==54 && mm == 21) {     tw0p[m][mm]= 0.214193; } 
	  if(m==54 && mm == 22) {     tw0p[m][mm]= 0.169853; } 
	  if(m==54 && mm == 23) {     tw0p[m][mm]= 0.251151; } 
	  if(m==54 && mm == 24) {     tw0p[m][mm]= 0.359618; } 
	  if(m==54 && mm == 25) {     tw0p[m][mm]= 0.068161; } 
	  if(m==54 && mm == 26) {     tw0p[m][mm]= 0.058241; } 
	  if(m==54 && mm == 27) {     tw0p[m][mm]= 0.364160; } 
	  if(m==54 && mm == 28) {     tw0p[m][mm]= 0.383206; } 
	  if(m==54 && mm == 29) {     tw0p[m][mm]= 0.717721; } 
	  if(m==54 && mm == 30) {     tw0p[m][mm]= 0.342180; } 
	  if(m==54 && mm == 31) {     tw0p[m][mm]= 0.091272; } 
	  if(m==54 && mm == 32) {     tw0p[m][mm]= 0.610441; } 
	  if(m==54 && mm == 33) {     tw0p[m][mm]= 0.948357; } 
	  if(m==54 && mm == 34) {     tw0p[m][mm]= 1.596764; } 
	  if(m==54 && mm == 35) {     tw0p[m][mm]= 0.397070; } 
	  if(m==54 && mm == 1) {     tw1p[m][mm]= 0.245742; } 
	  if(m==54 && mm == 4) {     tw1p[m][mm]= 0.201256; } 
	  if(m==54 && mm == 8) {     tw1p[m][mm]= 0.396506; } 
	  if(m==54 && mm == 21) {     tw1p[m][mm]= 0.272334; } 
	  if(m==54 && mm == 23) {     tw1p[m][mm]= 0.424393; } 
	  if(m==54 && mm == 26) {     tw1p[m][mm]= 0.403546; } 
	  if(m==54 && mm == 28) {     tw1p[m][mm]= 0.344879; } 
	  if(m==54 && mm == 30) {     tw1p[m][mm]= 0.175606; } 
	  if(m==54 && mm == 31) {     tw1p[m][mm]= 0.254437; } 
	  if(m==54 && mm == 32) {     tw1p[m][mm]= 0.466460; } 
	  if(m==54 && mm == 33) {     tw1p[m][mm]= 0.353775; } 
	  if(m==54 && mm == 34) {     tw1p[m][mm]= 0.444655; } 
	  if(m==54 && mm == 35) {     tw1p[m][mm]= 0.216918; } 
	  if(m==54 && mm == 11) {     tw2p[m][mm]= 0.241092; } 
	  if(m==54 && mm == 13) {     tw2p[m][mm]= 0.455909; } 
	  if(m==54 && mm == 14) {     tw2p[m][mm]= 0.395386; } 
	  if(m==54 && mm == 15) {     tw2p[m][mm]= 0.295625; } 
	  if(m==54 && mm == 18) {     tw2p[m][mm]= 0.135567; } 
	  if(m==54 && mm == 19) {     tw2p[m][mm]= 0.395589; } 
	  if(m==54 && mm == 20) {     tw2p[m][mm]= 0.158389; } 
	  if(m==54 && mm == 21) {     tw2p[m][mm]= 0.014104; } 
	  if(m==54 && mm == 22) {     tw2p[m][mm]= 0.297231; } 
	  if(m==54 && mm == 30) {     tw2p[m][mm]= 0.596668; } 
	  if(m==54 && mm == 32) {     tw2p[m][mm]= 0.260188; } 
	  if(m==54 && mm == 33) {     tw2p[m][mm]= 0.219642; } 
	  if(m==54 && mm == 34) {     tw2p[m][mm]= 0.533749; } 
	  if(m==54 && mm == 35) {     tw2p[m][mm]= 0.087380; } 
	  if(m==54 && mm == 6) {     tw3p[m][mm]= 0.117017; } 
	  if(m==54 && mm == 7) {     tw3p[m][mm]= 0.095235; } 
	  if(m==54 && mm == 8) {     tw3p[m][mm]= 0.014655; } 
	  if(m==54 && mm == 9) {     tw3p[m][mm]= 0.011335; } 
	  if(m==54 && mm == 10) {     tw3p[m][mm]= 0.133273; } 
	  if(m==54 && mm == 11) {     tw3p[m][mm]= 0.165947; } 
	  if(m==54 && mm == 32) {     tw3p[m][mm]= 0.006499; } 
	  if(m==54 && mm == 33) {     tw3p[m][mm]= 0.145477; } 
	  if(m==54 && mm == 34) {     tw3p[m][mm]= 0.094946; } 
	  if(m==54 && mm == 1) {     te2p[m][mm]= 0.036919; } 
	  if(m==54 && mm == 3) {     te2p[m][mm]= 0.096844; } 
	  if(m==54 && mm == 6) {     te2p[m][mm]= 0.106445; } 
	  if(m==54 && mm == 7) {     te2p[m][mm]= 0.004621; } 
	  if(m==54 && mm == 8) {     te2p[m][mm]= 0.710535; } 
	  if(m==54 && mm == 9) {     te2p[m][mm]= 0.183319; } 
	  if(m==54 && mm == 10) {     te2p[m][mm]= 0.314960; } 
	  if(m==54 && mm == 11) {     te2p[m][mm]= 0.391737; } 
	  if(m==54 && mm == 13) {     te2p[m][mm]= 0.153825; } 
	  if(m==54 && mm == 18) {     te2p[m][mm]= 0.327683; } 
	  if(m==54 && mm == 19) {     te2p[m][mm]= 0.326492; } 
	  if(m==54 && mm == 20) {     te2p[m][mm]= 0.088186; } 
	  if(m==54 && mm == 22) {     te2p[m][mm]= 0.057934; } 
	  if(m==54 && mm == 24) {     te2p[m][mm]= 0.764148; } 
	  if(m==54 && mm == 26) {     te2p[m][mm]= 0.254345; } 
	  if(m==54 && mm == 27) {     te2p[m][mm]= 0.014470; } 
	  if(m==54 && mm == 28) {     te2p[m][mm]= 0.089057; } 
	  if(m==54 && mm == 29) {     te2p[m][mm]= 0.144167; } 
	  if(m==54 && mm == 30) {     te2p[m][mm]= 0.037445; } 
	  if(m==54 && mm == 31) {     te2p[m][mm]= 0.291931; } 
	  if(m==54 && mm == 2) {     te3p[m][mm]= 0.123530; } 
	  if(m==54 && mm == 6) {     te3p[m][mm]= 0.217527; } 
	  if(m==54 && mm == 11) {     te3p[m][mm]= 0.277939; } 
	  if(m==54 && mm == 12) {     te3p[m][mm]= 0.426043; } 
	  if(m==54 && mm == 13) {     te3p[m][mm]= 0.479586; } 
	  if(m==54 && mm == 14) {     te3p[m][mm]= 0.692609; } 
	  if(m==54 && mm == 15) {     te3p[m][mm]= 0.124650; } 
	  if(m==54 && mm == 16) {     te3p[m][mm]= 0.242942; } 
	  if(m==54 && mm == 18) {     te3p[m][mm]= 0.118181; } 
	  if(m==54 && mm == 19) {     te3p[m][mm]= 0.266551; } 
	  if(m==54 && mm == 20) {     te3p[m][mm]= 0.415706; } 
	  if(m==54 && mm == 21) {     te3p[m][mm]= 0.193710; } 
	  if(m==54 && mm == 22) {     te3p[m][mm]= 0.333019; } 
	  if(m==54 && mm == 7) {     tw0n[m][mm]= 0.036896; } 
	  if(m==54 && mm == 12) {     tw0n[m][mm]= 0.385692; } 
	  if(m==54 && mm == 13) {     tw0n[m][mm]= 0.238165; } 
	  if(m==54 && mm == 14) {     tw0n[m][mm]= 0.447387; } 
	  if(m==54 && mm == 16) {     tw0n[m][mm]= 0.295340; } 
	  if(m==54 && mm == 17) {     tw0n[m][mm]= 0.450671; } 
	  if(m==54 && mm == 18) {     tw0n[m][mm]= 0.276244; } 
	  if(m==54 && mm == 19) {     tw0n[m][mm]= 0.172377; } 
	  if(m==54 && mm == 20) {     tw0n[m][mm]= 0.004315; } 
	  if(m==54 && mm == 21) {     tw0n[m][mm]= 0.180277; } 
	  if(m==54 && mm == 22) {     tw0n[m][mm]= 0.031742; } 
	  if(m==54 && mm == 23) {     tw0n[m][mm]= 0.139716; } 
	  if(m==54 && mm == 26) {     tw0n[m][mm]= 0.070265; } 
	  if(m==54 && mm == 27) {     tw0n[m][mm]= 0.082038; } 
	  if(m==54 && mm == 28) {     tw0n[m][mm]= 0.142906; } 
	  if(m==54 && mm == 29) {     tw0n[m][mm]= 0.390880; } 
	  if(m==54 && mm == 6) {     tw1n[m][mm]= 0.092263; } 
	  if(m==54 && mm == 7) {     tw1n[m][mm]= 0.011783; } 
	  if(m==54 && mm == 9) {     tw1n[m][mm]= 0.117366; } 
	  if(m==54 && mm == 11) {     tw1n[m][mm]= 0.076161; } 
	  if(m==54 && mm == 17) {     tw1n[m][mm]= 0.171985; } 
	  if(m==54 && mm == 23) {     tw1n[m][mm]= 0.004375; } 
	  if(m==54 && mm == 32) {     tw1n[m][mm]= 0.247391; } 
	  if(m==54 && mm == 34) {     tw1n[m][mm]= 0.046168; } 
	  if(m==54 && mm == 35) {     tw1n[m][mm]= 0.017311; } 
	  if(m==54 && mm == 13) {     tw2n[m][mm]= 0.119099; } 
	  if(m==54 && mm == 18) {     tw2n[m][mm]= 0.147123; } 
	  if(m==54 && mm == 19) {     tw2n[m][mm]= 0.226889; } 
	  if(m==54 && mm == 20) {     tw2n[m][mm]= 0.140362; } 
	  if(m==54 && mm == 22) {     tw2n[m][mm]= 0.093075; } 
	  if(m==54 && mm == 23) {     tw2n[m][mm]= 0.188951; } 
	  if(m==54 && mm == 27) {     tw2n[m][mm]= 0.030694; } 
	  if(m==54 && mm == 28) {     tw2n[m][mm]= 0.009153; } 
	  if(m==54 && mm == 30) {     tw2n[m][mm]= 0.604718; } 
	  if(m==54 && mm == 34) {     tw2n[m][mm]= 0.294059; } 
	  if(m==54 && mm == 35) {     tw2n[m][mm]= 0.066673; } 
	  if(m==54 && mm == 0) {     tw3n[m][mm]= 0.338893; } 
	  if(m==54 && mm == 6) {     tw3n[m][mm]= 0.202895; } 
	  if(m==54 && mm == 7) {     tw3n[m][mm]= 0.030413; } 
	  if(m==54 && mm == 8) {     tw3n[m][mm]= 0.044079; } 
	  if(m==54 && mm == 10) {     tw3n[m][mm]= 0.082203; } 
	  if(m==54 && mm == 11) {     tw3n[m][mm]= 0.118872; } 
	  if(m==54 && mm == 10) {     te2n[m][mm]= 0.405386; } 
	  if(m==54 && mm == 11) {     te2n[m][mm]= 0.178144; } 
	  if(m==54 && mm == 13) {     te2n[m][mm]= 0.091051; } 
	  if(m==54 && mm == 18) {     te2n[m][mm]= 0.332771; } 
	  if(m==54 && mm == 19) {     te2n[m][mm]= 0.289709; } 
	  if(m==54 && mm == 20) {     te2n[m][mm]= 0.129353; } 
	  if(m==54 && mm == 22) {     te2n[m][mm]= 0.185119; } 
	  if(m==54 && mm == 24) {     te2n[m][mm]= 0.723759; } 
	  if(m==54 && mm == 26) {     te2n[m][mm]= 0.190517; } 
	  if(m==54 && mm == 27) {     te2n[m][mm]= 0.258324; } 
	  if(m==54 && mm == 28) {     te2n[m][mm]= 0.101492; } 
	  if(m==54 && mm == 29) {     te2n[m][mm]= 0.483431; } 
	  if(m==54 && mm == 30) {     te2n[m][mm]= 0.160837; } 
	  if(m==54 && mm == 31) {     te2n[m][mm]= 0.411775; } 
	  if(m==54 && mm == 32) {     te2n[m][mm]= 0.646789; } 
	  if(m==54 && mm == 33) {     te2n[m][mm]= 0.450989; } 
	  if(m==54 && mm == 34) {     te2n[m][mm]= 1.451197; } 
	  if(m==54 && mm == 0) {     te3n[m][mm]= 0.419412; } 
	  if(m==54 && mm == 2) {     te3n[m][mm]= 0.232036; } 
	  if(m==54 && mm == 3) {     te3n[m][mm]= 0.753769; } 
	  if(m==54 && mm == 6) {     te3n[m][mm]= 0.160943; } 
	  if(m==54 && mm == 14) {     te3n[m][mm]= 0.310768; } 
	  if(m==54 && mm == 16) {     te3n[m][mm]= 0.109008; } 
	  if(m==54 && mm == 17) {     te3n[m][mm]= 1.774274; } 
	  if(m==54 && mm == 20) {     te3n[m][mm]= 0.375544; } 
	  if(m==54 && mm == 21) {     te3n[m][mm]= 0.011771; } 
	  if(m==54 && mm == 22) {     te3n[m][mm]= 0.249027; } 
	  if(m==54 && mm == 30) {     te3n[m][mm]= 0.330280; } 
	  if(m==54 && mm == 33) {     te3n[m][mm]= 0.511739; } 
	  if(m==54 && mm == 34) {     te3n[m][mm]= 0.281049; } 
	  if(m==55 && mm == 10) {     tw0p[m][mm]= 0.002990; } 
	  if(m==55 && mm == 12) {     tw0p[m][mm]= 0.361577; } 
	  if(m==55 && mm == 13) {     tw0p[m][mm]= 0.016277; } 
	  if(m==55 && mm == 14) {     tw0p[m][mm]= 0.098025; } 
	  if(m==55 && mm == 15) {     tw0p[m][mm]= 0.399191; } 
	  if(m==55 && mm == 18) {     tw0p[m][mm]= 0.550604; } 
	  if(m==55 && mm == 20) {     tw0p[m][mm]= 0.352576; } 
	  if(m==55 && mm == 21) {     tw0p[m][mm]= 0.154108; } 
	  if(m==55 && mm == 22) {     tw0p[m][mm]= 0.565225; } 
	  if(m==55 && mm == 23) {     tw0p[m][mm]= 0.474424; } 
	  if(m==55 && mm == 25) {     tw0p[m][mm]= 0.116909; } 
	  if(m==55 && mm == 26) {     tw0p[m][mm]= 0.223977; } 
	  if(m==55 && mm == 27) {     tw0p[m][mm]= 0.437737; } 
	  if(m==55 && mm == 28) {     tw0p[m][mm]= 0.202381; } 
	  if(m==55 && mm == 29) {     tw0p[m][mm]= 0.669169; } 
	  if(m==55 && mm == 30) {     tw0p[m][mm]= 0.098209; } 
	  if(m==55 && mm == 31) {     tw0p[m][mm]= 0.582503; } 
	  if(m==55 && mm == 33) {     tw0p[m][mm]= 0.039593; } 
	  if(m==55 && mm == 34) {     tw0p[m][mm]= 0.262804; } 
	  if(m==55 && mm == 35) {     tw0p[m][mm]= 0.225414; } 
	  if(m==55 && mm == 2) {     tw1p[m][mm]= 0.067252; } 
	  if(m==55 && mm == 7) {     tw1p[m][mm]= 0.266586; } 
	  if(m==55 && mm == 22) {     tw1p[m][mm]= 0.214148; } 
	  if(m==55 && mm == 23) {     tw1p[m][mm]= 0.185410; } 
	  if(m==55 && mm == 26) {     tw1p[m][mm]= 0.316197; } 
	  if(m==55 && mm == 31) {     tw1p[m][mm]= 0.098050; } 
	  if(m==55 && mm == 32) {     tw1p[m][mm]= 0.098064; } 
	  if(m==55 && mm == 33) {     tw1p[m][mm]= 0.314953; } 
	  if(m==55 && mm == 34) {     tw1p[m][mm]= 0.318000; } 
	  if(m==55 && mm == 12) {     tw2p[m][mm]= 0.012675; } 
	  if(m==55 && mm == 15) {     tw2p[m][mm]= 0.176960; } 
	  if(m==55 && mm == 18) {     tw2p[m][mm]= 0.417653; } 
	  if(m==55 && mm == 19) {     tw2p[m][mm]= 0.193633; } 
	  if(m==55 && mm == 21) {     tw2p[m][mm]= 0.284969; } 
	  if(m==55 && mm == 22) {     tw2p[m][mm]= 0.040013; } 
	  if(m==55 && mm == 32) {     tw2p[m][mm]= 0.095817; } 
	  if(m==55 && mm == 33) {     tw2p[m][mm]= 1.527380; } 
	  if(m==55 && mm == 34) {     tw2p[m][mm]= 0.274744; } 
	  if(m==55 && mm == 0) {     tw3p[m][mm]= 0.194423; } 
	  if(m==55 && mm == 5) {     tw3p[m][mm]= 0.029701; } 
	  if(m==55 && mm == 8) {     tw3p[m][mm]= 0.136911; } 
	  if(m==55 && mm == 30) {     tw3p[m][mm]= 0.158343; } 
	  if(m==55 && mm == 31) {     tw3p[m][mm]= 0.141251; } 
	  if(m==55 && mm == 32) {     tw3p[m][mm]= 3.051574; } 
	  if(m==55 && mm == 34) {     tw3p[m][mm]= 1.017850; } 
	  if(m==55 && mm == 0) {     te2p[m][mm]= 0.284974; } 
	  if(m==55 && mm == 1) {     te2p[m][mm]= 0.467257; } 
	  if(m==55 && mm == 2) {     te2p[m][mm]= 0.118189; } 
	  if(m==55 && mm == 3) {     te2p[m][mm]= 0.238026; } 
	  if(m==55 && mm == 4) {     te2p[m][mm]= 0.505461; } 
	  if(m==55 && mm == 5) {     te2p[m][mm]= 0.576708; } 
	  if(m==55 && mm == 6) {     te2p[m][mm]= 0.546740; } 
	  if(m==55 && mm == 9) {     te2p[m][mm]= 0.284369; } 
	  if(m==55 && mm == 10) {     te2p[m][mm]= 0.425586; } 
	  if(m==55 && mm == 11) {     te2p[m][mm]= 0.151479; } 
	  if(m==55 && mm == 12) {     te2p[m][mm]= 0.097797; } 
	  if(m==55 && mm == 13) {     te2p[m][mm]= 0.442576; } 
	  if(m==55 && mm == 19) {     te2p[m][mm]= 0.087040; } 
	  if(m==55 && mm == 22) {     te2p[m][mm]= 0.038453; } 
	  if(m==55 && mm == 24) {     te2p[m][mm]= 0.000998; } 
	  if(m==55 && mm == 26) {     te2p[m][mm]= 0.812096; } 
	  if(m==55 && mm == 28) {     te2p[m][mm]= 0.288253; } 
	  if(m==55 && mm == 29) {     te2p[m][mm]= 0.297209; } 
	  if(m==55 && mm == 2) {     te3p[m][mm]= 0.597502; } 
	  if(m==55 && mm == 3) {     te3p[m][mm]= 0.025144; } 
	  if(m==55 && mm == 6) {     te3p[m][mm]= 0.366239; } 
	  if(m==55 && mm == 10) {     te3p[m][mm]= 0.471286; } 
	  if(m==55 && mm == 11) {     te3p[m][mm]= 0.248365; } 
	  if(m==55 && mm == 13) {     te3p[m][mm]= 0.519341; } 
	  if(m==55 && mm == 14) {     te3p[m][mm]= 0.623129; } 
	  if(m==55 && mm == 16) {     te3p[m][mm]= 0.713659; } 
	  if(m==55 && mm == 18) {     te3p[m][mm]= 0.194058; } 
	  if(m==55 && mm == 19) {     te3p[m][mm]= 0.419344; } 
	  if(m==55 && mm == 2) {     tw0n[m][mm]= 0.104719; } 
	  if(m==55 && mm == 12) {     tw0n[m][mm]= 0.338430; } 
	  if(m==55 && mm == 13) {     tw0n[m][mm]= 0.067463; } 
	  if(m==55 && mm == 14) {     tw0n[m][mm]= 0.067867; } 
	  if(m==55 && mm == 15) {     tw0n[m][mm]= 0.805132; } 
	  if(m==55 && mm == 16) {     tw0n[m][mm]= 0.524673; } 
	  if(m==55 && mm == 18) {     tw0n[m][mm]= 0.512699; } 
	  if(m==55 && mm == 19) {     tw0n[m][mm]= 1.853925; } 
	  if(m==55 && mm == 20) {     tw0n[m][mm]= 0.027877; } 
	  if(m==55 && mm == 25) {     tw0n[m][mm]= 0.062073; } 
	  if(m==55 && mm == 26) {     tw0n[m][mm]= 0.211829; } 
	  if(m==55 && mm == 27) {     tw0n[m][mm]= 0.205804; } 
	  if(m==55 && mm == 28) {     tw0n[m][mm]= 0.106127; } 
	  if(m==55 && mm == 30) {     tw0n[m][mm]= 0.524612; } 
	  if(m==55 && mm == 7) {     tw1n[m][mm]= 0.411712; } 
	  if(m==55 && mm == 8) {     tw1n[m][mm]= 0.550729; } 
	  if(m==55 && mm == 9) {     tw1n[m][mm]= 0.397510; } 
	  if(m==55 && mm == 10) {     tw1n[m][mm]= 0.070086; } 
	  if(m==55 && mm == 15) {     tw1n[m][mm]= 0.078058; } 
	  if(m==55 && mm == 16) {     tw1n[m][mm]= 1.870326; } 
	  if(m==55 && mm == 21) {     tw1n[m][mm]= 0.519200; } 
	  if(m==55 && mm == 23) {     tw1n[m][mm]= 0.039110; } 
	  if(m==55 && mm == 34) {     tw1n[m][mm]= 0.399907; } 
	  if(m==55 && mm == 15) {     tw2n[m][mm]= 0.249854; } 
	  if(m==55 && mm == 18) {     tw2n[m][mm]= 0.220248; } 
	  if(m==55 && mm == 19) {     tw2n[m][mm]= 0.070553; } 
	  if(m==55 && mm == 25) {     tw2n[m][mm]= 0.701453; } 
	  if(m==55 && mm == 33) {     tw2n[m][mm]= 0.818335; } 
	  if(m==55 && mm == 34) {     tw2n[m][mm]= 0.041606; } 
	  if(m==55 && mm == 0) {     tw3n[m][mm]= 0.285718; } 
	  if(m==55 && mm == 5) {     tw3n[m][mm]= 0.082383; } 
	  if(m==55 && mm == 8) {     tw3n[m][mm]= 0.180082; } 
	  if(m==55 && mm == 4) {     te2n[m][mm]= 0.574294; } 
	  if(m==55 && mm == 5) {     te2n[m][mm]= 0.489273; } 
	  if(m==55 && mm == 7) {     te2n[m][mm]= 0.460711; } 
	  if(m==55 && mm == 13) {     te2n[m][mm]= 0.281966; } 
	  if(m==55 && mm == 19) {     te2n[m][mm]= 0.304437; } 
	  if(m==55 && mm == 22) {     te2n[m][mm]= 0.183471; } 
	  if(m==55 && mm == 25) {     te2n[m][mm]= 0.005332; } 
	  if(m==55 && mm == 26) {     te2n[m][mm]= 1.068623; } 
	  if(m==55 && mm == 27) {     te2n[m][mm]= 0.015560; } 
	  if(m==55 && mm == 28) {     te2n[m][mm]= 0.182447; } 
	  if(m==55 && mm == 32) {     te2n[m][mm]= 0.284588; } 
	  if(m==55 && mm == 34) {     te2n[m][mm]= 0.311894; } 
	  if(m==55 && mm == 35) {     te2n[m][mm]= 4.017259; } 
	  if(m==55 && mm == 5) {     te3n[m][mm]= 0.278638; } 
	  if(m==55 && mm == 14) {     te3n[m][mm]= 0.333554; } 
	  if(m==55 && mm == 16) {     te3n[m][mm]= 0.763768; } 
	  if(m==55 && mm == 18) {     te3n[m][mm]= 0.371499; } 
	  if(m==55 && mm == 19) {     te3n[m][mm]= 0.376773; } 
	  if(m==55 && mm == 22) {     te3n[m][mm]= 0.095487; } 
	  if(m==55 && mm == 31) {     te3n[m][mm]= 0.239904; } 
	  if(m==55 && mm == 34) {     te3n[m][mm]= 0.513657; } 
	  if(m==55 && mm == 35) {     te3n[m][mm]= 0.916474; } 
	  if(m==56 && mm == 12) {     tw0p[m][mm]= 0.175043; } 
	  if(m==56 && mm == 14) {     tw0p[m][mm]= 0.875406; } 
	  if(m==56 && mm == 15) {     tw0p[m][mm]= 0.028953; } 
	  if(m==56 && mm == 16) {     tw0p[m][mm]= 1.331006; } 
	  if(m==56 && mm == 17) {     tw0p[m][mm]= 1.049244; } 
	  if(m==56 && mm == 18) {     tw0p[m][mm]= 0.656251; } 
	  if(m==56 && mm == 19) {     tw0p[m][mm]= 0.744840; } 
	  if(m==56 && mm == 21) {     tw0p[m][mm]= 1.158087; } 
	  if(m==56 && mm == 22) {     tw0p[m][mm]= 0.482673; } 
	  if(m==56 && mm == 23) {     tw0p[m][mm]= 0.536513; } 
	  if(m==56 && mm == 24) {     tw0p[m][mm]= 0.612201; } 
	  if(m==56 && mm == 26) {     tw0p[m][mm]= 0.925245; } 
	  if(m==56 && mm == 27) {     tw0p[m][mm]= 0.332834; } 
	  if(m==56 && mm == 28) {     tw0p[m][mm]= 0.256117; } 
	  if(m==56 && mm == 29) {     tw0p[m][mm]= 0.228549; } 
	  if(m==56 && mm == 31) {     tw0p[m][mm]= 0.147175; } 
	  if(m==56 && mm == 32) {     tw0p[m][mm]= 0.888755; } 
	  if(m==56 && mm == 33) {     tw0p[m][mm]= 0.285215; } 
	  if(m==56 && mm == 34) {     tw0p[m][mm]= 1.201731; } 
	  if(m==56 && mm == 35) {     tw0p[m][mm]= 0.362882; } 
	  if(m==56 && mm == 0) {     tw1p[m][mm]= 0.209557; } 
	  if(m==56 && mm == 3) {     tw1p[m][mm]= 0.478828; } 
	  if(m==56 && mm == 4) {     tw1p[m][mm]= 0.336220; } 
	  if(m==56 && mm == 7) {     tw1p[m][mm]= 0.395459; } 
	  if(m==56 && mm == 19) {     tw1p[m][mm]= 0.278488; } 
	  if(m==56 && mm == 20) {     tw1p[m][mm]= 0.429390; } 
	  if(m==56 && mm == 21) {     tw1p[m][mm]= 0.084662; } 
	  if(m==56 && mm == 22) {     tw1p[m][mm]= 0.463304; } 
	  if(m==56 && mm == 23) {     tw1p[m][mm]= 0.110520; } 
	  if(m==56 && mm == 24) {     tw1p[m][mm]= 0.370916; } 
	  if(m==56 && mm == 25) {     tw1p[m][mm]= 0.448764; } 
	  if(m==56 && mm == 27) {     tw1p[m][mm]= 0.883787; } 
	  if(m==56 && mm == 30) {     tw1p[m][mm]= 0.090429; } 
	  if(m==56 && mm == 31) {     tw1p[m][mm]= 0.164445; } 
	  if(m==56 && mm == 32) {     tw1p[m][mm]= 0.215276; } 
	  if(m==56 && mm == 35) {     tw1p[m][mm]= 0.092594; } 
	  if(m==56 && mm == 5) {     tw2p[m][mm]= 0.171115; } 
	  if(m==56 && mm == 8) {     tw2p[m][mm]= 0.435911; } 
	  if(m==56 && mm == 9) {     tw2p[m][mm]= 0.147278; } 
	  if(m==56 && mm == 10) {     tw2p[m][mm]= 0.190201; } 
	  if(m==56 && mm == 11) {     tw2p[m][mm]= 0.114033; } 
	  if(m==56 && mm == 12) {     tw2p[m][mm]= 0.300324; } 
	  if(m==56 && mm == 13) {     tw2p[m][mm]= 0.029092; } 
	  if(m==56 && mm == 15) {     tw2p[m][mm]= 0.262675; } 
	  if(m==56 && mm == 16) {     tw2p[m][mm]= 0.503489; } 
	  if(m==56 && mm == 17) {     tw2p[m][mm]= 0.666144; } 
	  if(m==56 && mm == 18) {     tw2p[m][mm]= 0.090862; } 
	  if(m==56 && mm == 20) {     tw2p[m][mm]= 0.464911; } 
	  if(m==56 && mm == 21) {     tw2p[m][mm]= 0.648387; } 
	  if(m==56 && mm == 22) {     tw2p[m][mm]= 0.168958; } 
	  if(m==56 && mm == 23) {     tw2p[m][mm]= 0.129324; } 
	  if(m==56 && mm == 24) {     tw3p[m][mm]= 0.941219; } 
	  if(m==56 && mm == 25) {     tw3p[m][mm]= 0.286975; } 
	  if(m==56 && mm == 27) {     tw3p[m][mm]= 0.816342; } 
	  if(m==56 && mm == 28) {     tw3p[m][mm]= 1.256463; } 
	  if(m==56 && mm == 33) {     tw3p[m][mm]= 0.924859; } 
	  if(m==56 && mm == 34) {     tw3p[m][mm]= 0.212548; } 
	  if(m==56 && mm == 35) {     tw3p[m][mm]= 0.714032; } 
	  if(m==56 && mm == 0) {     te2p[m][mm]= 0.201784; } 
	  if(m==56 && mm == 1) {     te2p[m][mm]= 0.379942; } 
	  if(m==56 && mm == 4) {     te2p[m][mm]= 0.325225; } 
	  if(m==56 && mm == 5) {     te2p[m][mm]= 0.210217; } 
	  if(m==56 && mm == 6) {     te2p[m][mm]= 1.299375; } 
	  if(m==56 && mm == 7) {     te2p[m][mm]= 0.287563; } 
	  if(m==56 && mm == 8) {     te2p[m][mm]= 0.172656; } 
	  if(m==56 && mm == 9) {     te2p[m][mm]= 0.138085; } 
	  if(m==56 && mm == 10) {     te2p[m][mm]= 0.007024; } 
	  if(m==56 && mm == 11) {     te2p[m][mm]= 0.412662; } 
	  if(m==56 && mm == 13) {     te2p[m][mm]= 0.249946; } 
	  if(m==56 && mm == 16) {     te2p[m][mm]= 0.214254; } 
	  if(m==56 && mm == 17) {     te2p[m][mm]= 0.053167; } 
	  if(m==56 && mm == 18) {     te2p[m][mm]= 0.058913; } 
	  if(m==56 && mm == 19) {     te2p[m][mm]= 0.339271; } 
	  if(m==56 && mm == 20) {     te2p[m][mm]= 0.021839; } 
	  if(m==56 && mm == 21) {     te2p[m][mm]= 0.238887; } 
	  if(m==56 && mm == 22) {     te2p[m][mm]= 0.231259; } 
	  if(m==56 && mm == 24) {     te2p[m][mm]= 0.112860; } 
	  if(m==56 && mm == 25) {     te2p[m][mm]= 0.473848; } 
	  if(m==56 && mm == 28) {     te2p[m][mm]= 0.009330; } 
	  if(m==56 && mm == 30) {     te2p[m][mm]= 0.069582; } 
	  if(m==56 && mm == 3) {     te3p[m][mm]= 0.249356; } 
	  if(m==56 && mm == 4) {     te3p[m][mm]= 0.414840; } 
	  if(m==56 && mm == 5) {     te3p[m][mm]= 1.397984; } 
	  if(m==56 && mm == 6) {     te3p[m][mm]= 0.058306; } 
	  if(m==56 && mm == 10) {     te3p[m][mm]= 0.441807; } 
	  if(m==56 && mm == 11) {     te3p[m][mm]= 0.578222; } 
	  if(m==56 && mm == 12) {     te3p[m][mm]= 0.553784; } 
	  if(m==56 && mm == 13) {     te3p[m][mm]= 0.694162; } 
	  if(m==56 && mm == 15) {     te3p[m][mm]= 0.004544; } 
	  if(m==56 && mm == 16) {     te3p[m][mm]= 0.814884; } 
	  if(m==56 && mm == 17) {     te3p[m][mm]= 1.055517; } 
	  if(m==56 && mm == 18) {     te3p[m][mm]= 0.843329; } 
	  if(m==56 && mm == 20) {     te3p[m][mm]= 0.589252; } 
	  if(m==56 && mm == 21) {     te3p[m][mm]= 0.935671; } 
	  if(m==56 && mm == 22) {     te3p[m][mm]= 1.175502; } 
	  if(m==56 && mm == 1) {     tw0n[m][mm]= 0.026630; } 
	  if(m==56 && mm == 4) {     tw0n[m][mm]= 0.020411; } 
	  if(m==56 && mm == 5) {     tw0n[m][mm]= 0.008112; } 
	  if(m==56 && mm == 12) {     tw0n[m][mm]= 0.264881; } 
	  if(m==56 && mm == 14) {     tw0n[m][mm]= 1.240198; } 
	  if(m==56 && mm == 16) {     tw0n[m][mm]= 1.067872; } 
	  if(m==56 && mm == 17) {     tw0n[m][mm]= 0.962183; } 
	  if(m==56 && mm == 18) {     tw0n[m][mm]= 0.806118; } 
	  if(m==56 && mm == 21) {     tw0n[m][mm]= 1.329378; } 
	  if(m==56 && mm == 22) {     tw0n[m][mm]= 0.421839; } 
	  if(m==56 && mm == 23) {     tw0n[m][mm]= 0.503170; } 
	  if(m==56 && mm == 24) {     tw0n[m][mm]= 0.382764; } 
	  if(m==56 && mm == 26) {     tw0n[m][mm]= 0.557644; } 
	  if(m==56 && mm == 27) {     tw0n[m][mm]= 0.341253; } 
	  if(m==56 && mm == 28) {     tw0n[m][mm]= 0.107810; } 
	  if(m==56 && mm == 29) {     tw0n[m][mm]= 0.131133; } 
	  if(m==56 && mm == 30) {     tw0n[m][mm]= 0.581181; } 
	  if(m==56 && mm == 6) {     tw1n[m][mm]= 0.056178; } 
	  if(m==56 && mm == 7) {     tw1n[m][mm]= 0.422499; } 
	  if(m==56 && mm == 9) {     tw1n[m][mm]= 0.513817; } 
	  if(m==56 && mm == 15) {     tw1n[m][mm]= 0.041392; } 
	  if(m==56 && mm == 16) {     tw1n[m][mm]= 0.543595; } 
	  if(m==56 && mm == 17) {     tw1n[m][mm]= 0.019922; } 
	  if(m==56 && mm == 18) {     tw1n[m][mm]= 0.199650; } 
	  if(m==56 && mm == 19) {     tw1n[m][mm]= 0.272075; } 
	  if(m==56 && mm == 20) {     tw1n[m][mm]= 0.420841; } 
	  if(m==56 && mm == 21) {     tw1n[m][mm]= 0.110643; } 
	  if(m==56 && mm == 22) {     tw1n[m][mm]= 0.171434; } 
	  if(m==56 && mm == 23) {     tw1n[m][mm]= 0.017320; } 
	  if(m==56 && mm == 24) {     tw1n[m][mm]= 0.647777; } 
	  if(m==56 && mm == 35) {     tw1n[m][mm]= 0.028559; } 
	  if(m==56 && mm == 2) {     tw2n[m][mm]= 0.318535; } 
	  if(m==56 && mm == 3) {     tw2n[m][mm]= 0.076285; } 
	  if(m==56 && mm == 4) {     tw2n[m][mm]= 0.087240; } 
	  if(m==56 && mm == 8) {     tw2n[m][mm]= 0.499900; } 
	  if(m==56 && mm == 13) {     tw2n[m][mm]= 0.180013; } 
	  if(m==56 && mm == 14) {     tw2n[m][mm]= 1.698865; } 
	  if(m==56 && mm == 15) {     tw2n[m][mm]= 0.537518; } 
	  if(m==56 && mm == 16) {     tw2n[m][mm]= 0.238122; } 
	  if(m==56 && mm == 17) {     tw2n[m][mm]= 0.519874; } 
	  if(m==56 && mm == 20) {     tw2n[m][mm]= 0.380248; } 
	  if(m==56 && mm == 21) {     tw2n[m][mm]= 0.504175; } 
	  if(m==56 && mm == 22) {     tw2n[m][mm]= 0.334690; } 
	  if(m==56 && mm == 24) {     tw3n[m][mm]= 0.793151; } 
	  if(m==56 && mm == 25) {     tw3n[m][mm]= 0.384994; } 
	  if(m==56 && mm == 27) {     tw3n[m][mm]= 0.628646; } 
	  if(m==56 && mm == 28) {     tw3n[m][mm]= 1.109302; } 
	  if(m==56 && mm == 29) {     tw3n[m][mm]= 1.872219; } 
	  if(m==56 && mm == 4) {     te2n[m][mm]= 0.410818; } 
	  if(m==56 && mm == 5) {     te2n[m][mm]= 0.596259; } 
	  if(m==56 && mm == 6) {     te2n[m][mm]= 0.743155; } 
	  if(m==56 && mm == 8) {     te2n[m][mm]= 0.013382; } 
	  if(m==56 && mm == 9) {     te2n[m][mm]= 0.064765; } 
	  if(m==56 && mm == 10) {     te2n[m][mm]= 0.007889; } 
	  if(m==56 && mm == 11) {     te2n[m][mm]= 0.351470; } 
	  if(m==56 && mm == 13) {     te2n[m][mm]= 0.167229; } 
	  if(m==56 && mm == 16) {     te2n[m][mm]= 0.160619; } 
	  if(m==56 && mm == 18) {     te2n[m][mm]= 0.082613; } 
	  if(m==56 && mm == 19) {     te2n[m][mm]= 0.277652; } 
	  if(m==56 && mm == 20) {     te2n[m][mm]= 0.029617; } 
	  if(m==56 && mm == 21) {     te2n[m][mm]= 0.300666; } 
	  if(m==56 && mm == 22) {     te2n[m][mm]= 0.231507; } 
	  if(m==56 && mm == 24) {     te2n[m][mm]= 0.177522; } 
	  if(m==56 && mm == 25) {     te2n[m][mm]= 0.760457; } 
	  if(m==56 && mm == 27) {     te2n[m][mm]= 0.056138; } 
	  if(m==56 && mm == 28) {     te2n[m][mm]= 0.278178; } 
	  if(m==56 && mm == 29) {     te2n[m][mm]= 0.466960; } 
	  if(m==56 && mm == 30) {     te2n[m][mm]= 0.840320; } 
	  if(m==56 && mm == 31) {     te2n[m][mm]= 0.390710; } 
	  if(m==56 && mm == 32) {     te2n[m][mm]= 0.427991; } 
	  if(m==56 && mm == 33) {     te2n[m][mm]= 0.349044; } 
	  if(m==56 && mm == 34) {     te2n[m][mm]= 0.233930; } 
	  if(m==56 && mm == 35) {     te2n[m][mm]= 0.224662; } 
	  if(m==56 && mm == 0) {     te3n[m][mm]= 0.477902; } 
	  if(m==56 && mm == 1) {     te3n[m][mm]= 0.246362; } 
	  if(m==56 && mm == 2) {     te3n[m][mm]= 0.762657; } 
	  if(m==56 && mm == 4) {     te3n[m][mm]= 0.187760; } 
	  if(m==56 && mm == 5) {     te3n[m][mm]= 0.672089; } 
	  if(m==56 && mm == 17) {     te3n[m][mm]= 0.217525; } 
	  if(m==56 && mm == 18) {     te3n[m][mm]= 0.831209; } 
	  if(m==56 && mm == 20) {     te3n[m][mm]= 0.395001; } 
	  if(m==56 && mm == 21) {     te3n[m][mm]= 1.123207; } 
	  if(m==56 && mm == 22) {     te3n[m][mm]= 0.940945; } 
	  if(m==56 && mm == 24) {     te3n[m][mm]= 0.330665; } 
	  if(m==56 && mm == 28) {     te3n[m][mm]= 0.017961; } 
	  if(m==56 && mm == 29) {     te3n[m][mm]= 0.100606; } 
	  if(m==56 && mm == 30) {     te3n[m][mm]= 0.017647; } 
	  if(m==56 && mm == 31) {     te3n[m][mm]= 0.164564; } 
	  if(m==56 && mm == 32) {     te3n[m][mm]= 0.190950; } 
	  if(m==56 && mm == 33) {     te3n[m][mm]= 0.441767; } 
	  if(m==56 && mm == 34) {     te3n[m][mm]= 0.706100; } 
	  if(m==56 && mm == 35) {     te3n[m][mm]= 0.813268; } 
	  if(m==57 && mm == 6) {     tw0p[m][mm]= 0.096825; } 
	  if(m==57 && mm == 12) {     tw0p[m][mm]= 0.439068; } 
	  if(m==57 && mm == 13) {     tw0p[m][mm]= 0.171161; } 
	  if(m==57 && mm == 14) {     tw0p[m][mm]= 1.261162; } 
	  if(m==57 && mm == 17) {     tw0p[m][mm]= 0.347232; } 
	  if(m==57 && mm == 22) {     tw0p[m][mm]= 0.396298; } 
	  if(m==57 && mm == 24) {     tw0p[m][mm]= 0.134425; } 
	  if(m==57 && mm == 25) {     tw0p[m][mm]= 0.479683; } 
	  if(m==57 && mm == 26) {     tw0p[m][mm]= 0.202531; } 
	  if(m==57 && mm == 27) {     tw0p[m][mm]= 1.210758; } 
	  if(m==57 && mm == 28) {     tw0p[m][mm]= 0.318334; } 
	  if(m==57 && mm == 29) {     tw0p[m][mm]= 3.605203; } 
	  if(m==57 && mm == 30) {     tw0p[m][mm]= 0.940289; } 
	  if(m==57 && mm == 31) {     tw0p[m][mm]= 0.052702; } 
	  if(m==57 && mm == 33) {     tw0p[m][mm]= 0.322865; } 
	  if(m==57 && mm == 34) {     tw0p[m][mm]= 0.227785; } 
	  if(m==57 && mm == 1) {     tw1p[m][mm]= 0.068474; } 
	  if(m==57 && mm == 5) {     tw1p[m][mm]= 0.054181; } 
	  if(m==57 && mm == 21) {     tw1p[m][mm]= 0.292727; } 
	  if(m==57 && mm == 22) {     tw1p[m][mm]= 0.290755; } 
	  if(m==57 && mm == 23) {     tw1p[m][mm]= 0.252195; } 
	  if(m==57 && mm == 24) {     tw1p[m][mm]= 0.036519; } 
	  if(m==57 && mm == 30) {     tw1p[m][mm]= 0.037740; } 
	  if(m==57 && mm == 34) {     tw1p[m][mm]= 0.333717; } 
	  if(m==57 && mm == 5) {     tw2p[m][mm]= 0.038857; } 
	  if(m==57 && mm == 6) {     tw2p[m][mm]= 0.192559; } 
	  if(m==57 && mm == 9) {     tw2p[m][mm]= 0.472297; } 
	  if(m==57 && mm == 12) {     tw2p[m][mm]= 0.615078; } 
	  if(m==57 && mm == 13) {     tw2p[m][mm]= 0.085965; } 
	  if(m==57 && mm == 14) {     tw2p[m][mm]= 0.052459; } 
	  if(m==57 && mm == 15) {     tw2p[m][mm]= 0.127758; } 
	  if(m==57 && mm == 17) {     tw2p[m][mm]= 0.083682; } 
	  if(m==57 && mm == 19) {     tw2p[m][mm]= 0.523329; } 
	  if(m==57 && mm == 21) {     tw2p[m][mm]= 0.260823; } 
	  if(m==57 && mm == 22) {     tw2p[m][mm]= 0.249936; } 
	  if(m==57 && mm == 24) {     tw3p[m][mm]= 1.791975; } 
	  if(m==57 && mm == 25) {     tw3p[m][mm]= 0.906185; } 
	  if(m==57 && mm == 27) {     tw3p[m][mm]= 1.374875; } 
	  if(m==57 && mm == 28) {     tw3p[m][mm]= 0.282208; } 
	  if(m==57 && mm == 29) {     tw3p[m][mm]= 2.105105; } 
	  if(m==57 && mm == 31) {     tw3p[m][mm]= 0.209439; } 
	  if(m==57 && mm == 32) {     tw3p[m][mm]= 0.234101; } 
	  if(m==57 && mm == 34) {     tw3p[m][mm]= 1.154559; } 
	  if(m==57 && mm == 35) {     tw3p[m][mm]= 0.154900; } 
	  if(m==57 && mm == 5) {     te2p[m][mm]= 0.039120; } 
	  if(m==57 && mm == 7) {     te2p[m][mm]= 0.391873; } 
	  if(m==57 && mm == 9) {     te2p[m][mm]= 0.383217; } 
	  if(m==57 && mm == 11) {     te2p[m][mm]= 0.021826; } 
	  if(m==57 && mm == 12) {     te2p[m][mm]= 0.305869; } 
	  if(m==57 && mm == 13) {     te2p[m][mm]= 0.120559; } 
	  if(m==57 && mm == 14) {     te2p[m][mm]= 0.077887; } 
	  if(m==57 && mm == 16) {     te2p[m][mm]= 0.035893; } 
	  if(m==57 && mm == 18) {     te2p[m][mm]= 0.268520; } 
	  if(m==57 && mm == 20) {     te2p[m][mm]= 0.139209; } 
	  if(m==57 && mm == 23) {     te2p[m][mm]= 0.018191; } 
	  if(m==57 && mm == 25) {     te2p[m][mm]= 0.087215; } 
	  if(m==57 && mm == 26) {     te2p[m][mm]= 0.157883; } 
	  if(m==57 && mm == 28) {     te2p[m][mm]= 0.058001; } 
	  if(m==57 && mm == 3) {     te3p[m][mm]= 0.653663; } 
	  if(m==57 && mm == 4) {     te3p[m][mm]= 0.206215; } 
	  if(m==57 && mm == 5) {     te3p[m][mm]= 0.300251; } 
	  if(m==57 && mm == 12) {     te3p[m][mm]= 0.560075; } 
	  if(m==57 && mm == 13) {     te3p[m][mm]= 0.064992; } 
	  if(m==57 && mm == 14) {     te3p[m][mm]= 0.384138; } 
	  if(m==57 && mm == 15) {     te3p[m][mm]= 0.101588; } 
	  if(m==57 && mm == 16) {     te3p[m][mm]= 1.189352; } 
	  if(m==57 && mm == 19) {     te3p[m][mm]= 1.772679; } 
	  if(m==57 && mm == 20) {     te3p[m][mm]= 0.146931; } 
	  if(m==57 && mm == 21) {     te3p[m][mm]= 0.167525; } 
	  if(m==57 && mm == 22) {     te3p[m][mm]= 0.888500; } 
	  if(m==57 && mm == 23) {     te3p[m][mm]= 0.235752; } 
	  if(m==57 && mm == 0) {     tw0n[m][mm]= 0.386488; } 
	  if(m==57 && mm == 1) {     tw0n[m][mm]= 0.157810; } 
	  if(m==57 && mm == 12) {     tw0n[m][mm]= 0.707956; } 
	  if(m==57 && mm == 13) {     tw0n[m][mm]= 0.262456; } 
	  if(m==57 && mm == 15) {     tw0n[m][mm]= 0.133046; } 
	  if(m==57 && mm == 17) {     tw0n[m][mm]= 0.819697; } 
	  if(m==57 && mm == 24) {     tw0n[m][mm]= 0.052846; } 
	  if(m==57 && mm == 25) {     tw0n[m][mm]= 0.462970; } 
	  if(m==57 && mm == 26) {     tw0n[m][mm]= 0.164508; } 
	  if(m==57 && mm == 27) {     tw0n[m][mm]= 0.980587; } 
	  if(m==57 && mm == 28) {     tw0n[m][mm]= 0.112771; } 
	  if(m==57 && mm == 30) {     tw0n[m][mm]= 1.163379; } 
	  if(m==57 && mm == 6) {     tw1n[m][mm]= 0.170290; } 
	  if(m==57 && mm == 7) {     tw1n[m][mm]= 0.036344; } 
	  if(m==57 && mm == 10) {     tw1n[m][mm]= 0.177148; } 
	  if(m==57 && mm == 11) {     tw1n[m][mm]= 0.273330; } 
	  if(m==57 && mm == 13) {     tw1n[m][mm]= 0.083305; } 
	  if(m==57 && mm == 16) {     tw1n[m][mm]= 0.862205; } 
	  if(m==57 && mm == 19) {     tw1n[m][mm]= 0.098818; } 
	  if(m==57 && mm == 21) {     tw1n[m][mm]= 0.166947; } 
	  if(m==57 && mm == 22) {     tw1n[m][mm]= 0.095051; } 
	  if(m==57 && mm == 23) {     tw1n[m][mm]= 0.193920; } 
	  if(m==57 && mm == 29) {     tw1n[m][mm]= 0.017658; } 
	  if(m==57 && mm == 30) {     tw1n[m][mm]= 0.039653; } 
	  if(m==57 && mm == 2) {     tw2n[m][mm]= 0.011146; } 
	  if(m==57 && mm == 5) {     tw2n[m][mm]= 0.034891; } 
	  if(m==57 && mm == 15) {     tw2n[m][mm]= 0.197761; } 
	  if(m==57 && mm == 17) {     tw2n[m][mm]= 0.167602; } 
	  if(m==57 && mm == 19) {     tw2n[m][mm]= 0.247353; } 
	  if(m==57 && mm == 21) {     tw2n[m][mm]= 0.263788; } 
	  if(m==57 && mm == 22) {     tw2n[m][mm]= 0.075899; } 
	  if(m==57 && mm == 24) {     tw3n[m][mm]= 1.444692; } 
	  if(m==57 && mm == 25) {     tw3n[m][mm]= 1.174369; } 
	  if(m==57 && mm == 26) {     tw3n[m][mm]= 1.162822; } 
	  if(m==57 && mm == 27) {     tw3n[m][mm]= 1.294842; } 
	  if(m==57 && mm == 28) {     tw3n[m][mm]= 1.991109; } 
	  if(m==57 && mm == 4) {     te2n[m][mm]= 8.915510; } 
	  if(m==57 && mm == 7) {     te2n[m][mm]= 0.107801; } 
	  if(m==57 && mm == 9) {     te2n[m][mm]= 0.205630; } 
	  if(m==57 && mm == 10) {     te2n[m][mm]= 0.597514; } 
	  if(m==57 && mm == 12) {     te2n[m][mm]= 0.062376; } 
	  if(m==57 && mm == 16) {     te2n[m][mm]= 0.012320; } 
	  if(m==57 && mm == 18) {     te2n[m][mm]= 0.293401; } 
	  if(m==57 && mm == 20) {     te2n[m][mm]= 0.242946; } 
	  if(m==57 && mm == 23) {     te2n[m][mm]= 0.061547; } 
	  if(m==57 && mm == 24) {     te2n[m][mm]= 0.008084; } 
	  if(m==57 && mm == 25) {     te2n[m][mm]= 0.123827; } 
	  if(m==57 && mm == 26) {     te2n[m][mm]= 0.212646; } 
	  if(m==57 && mm == 27) {     te2n[m][mm]= 0.048243; } 
	  if(m==57 && mm == 28) {     te2n[m][mm]= 0.187311; } 
	  if(m==57 && mm == 29) {     te2n[m][mm]= 0.054934; } 
	  if(m==57 && mm == 32) {     te2n[m][mm]= 0.327608; } 
	  if(m==57 && mm == 33) {     te2n[m][mm]= 0.173819; } 
	  if(m==57 && mm == 34) {     te2n[m][mm]= 0.615589; } 
	  if(m==57 && mm == 0) {     te3n[m][mm]= 0.109503; } 
	  if(m==57 && mm == 2) {     te3n[m][mm]= 0.297828; } 
	  if(m==57 && mm == 3) {     te3n[m][mm]= 0.111321; } 
	  if(m==57 && mm == 4) {     te3n[m][mm]= 0.138759; } 
	  if(m==57 && mm == 5) {     te3n[m][mm]= 0.517879; } 
	  if(m==57 && mm == 14) {     te3n[m][mm]= 0.209857; } 
	  if(m==57 && mm == 16) {     te3n[m][mm]= 1.063394; } 
	  if(m==57 && mm == 19) {     te3n[m][mm]= 1.779062; } 
	  if(m==57 && mm == 20) {     te3n[m][mm]= 0.962288; } 
	  if(m==57 && mm == 21) {     te3n[m][mm]= 0.356913; } 
	  if(m==57 && mm == 22) {     te3n[m][mm]= 0.067645; } 
	  if(m==57 && mm == 23) {     te3n[m][mm]= 0.055709; } 
	  if(m==57 && mm == 24) {     te3n[m][mm]= 0.230036; } 
	  if(m==57 && mm == 26) {     te3n[m][mm]= 2.327513; } 
	  if(m==57 && mm == 28) {     te3n[m][mm]= 2.189161; } 
	  if(m==57 && mm == 29) {     te3n[m][mm]= 0.303025; } 
	  if(m==57 && mm == 30) {     te3n[m][mm]= 1.517627; } 
	  if(m==57 && mm == 33) {     te3n[m][mm]= 0.153178; } 
	  if(m==57 && mm == 35) {     te3n[m][mm]= 0.165879; } 
	  if(m==58 && mm == 14) {     tw0p[m][mm]= 0.011889; } 
	  if(m==58 && mm == 15) {     tw0p[m][mm]= 0.022731; } 
	  if(m==58 && mm == 16) {     tw0p[m][mm]= 0.581247; } 
	  if(m==58 && mm == 17) {     tw0p[m][mm]= 0.325997; } 
	  if(m==58 && mm == 18) {     tw0p[m][mm]= 0.528225; } 
	  if(m==58 && mm == 19) {     tw0p[m][mm]= 0.222816; } 
	  if(m==58 && mm == 20) {     tw0p[m][mm]= 0.615776; } 
	  if(m==58 && mm == 22) {     tw0p[m][mm]= 0.116374; } 
	  if(m==58 && mm == 23) {     tw0p[m][mm]= 1.760841; } 
	  if(m==58 && mm == 24) {     tw0p[m][mm]= 0.279464; } 
	  if(m==58 && mm == 25) {     tw0p[m][mm]= 0.381700; } 
	  if(m==58 && mm == 26) {     tw0p[m][mm]= 0.277464; } 
	  if(m==58 && mm == 27) {     tw0p[m][mm]= 0.487982; } 
	  if(m==58 && mm == 28) {     tw0p[m][mm]= 0.337884; } 
	  if(m==58 && mm == 29) {     tw0p[m][mm]= 0.237859; } 
	  if(m==58 && mm == 30) {     tw0p[m][mm]= 0.596758; } 
	  if(m==58 && mm == 31) {     tw0p[m][mm]= 0.469756; } 
	  if(m==58 && mm == 32) {     tw0p[m][mm]= 0.593128; } 
	  if(m==58 && mm == 34) {     tw0p[m][mm]= 0.718626; } 
	  if(m==58 && mm == 0) {     tw1p[m][mm]= 0.342688; } 
	  if(m==58 && mm == 1) {     tw1p[m][mm]= 0.158152; } 
	  if(m==58 && mm == 2) {     tw1p[m][mm]= 0.039678; } 
	  if(m==58 && mm == 3) {     tw1p[m][mm]= 0.137817; } 
	  if(m==58 && mm == 6) {     tw1p[m][mm]= 0.113192; } 
	  if(m==58 && mm == 7) {     tw1p[m][mm]= 0.213265; } 
	  if(m==58 && mm == 8) {     tw1p[m][mm]= 0.234343; } 
	  if(m==58 && mm == 19) {     tw1p[m][mm]= 0.486069; } 
	  if(m==58 && mm == 21) {     tw1p[m][mm]= 0.143167; } 
	  if(m==58 && mm == 22) {     tw1p[m][mm]= 0.485304; } 
	  if(m==58 && mm == 23) {     tw1p[m][mm]= 0.567800; } 
	  if(m==58 && mm == 24) {     tw1p[m][mm]= 2.005853; } 
	  if(m==58 && mm == 25) {     tw1p[m][mm]= 0.353028; } 
	  if(m==58 && mm == 26) {     tw1p[m][mm]= 0.220601; } 
	  if(m==58 && mm == 28) {     tw1p[m][mm]= 0.273078; } 
	  if(m==58 && mm == 29) {     tw1p[m][mm]= 0.570836; } 
	  if(m==58 && mm == 30) {     tw1p[m][mm]= 0.811813; } 
	  if(m==58 && mm == 31) {     tw1p[m][mm]= 0.154873; } 
	  if(m==58 && mm == 32) {     tw1p[m][mm]= 0.092926; } 
	  if(m==58 && mm == 33) {     tw1p[m][mm]= 0.015137; } 
	  if(m==58 && mm == 34) {     tw1p[m][mm]= 0.567332; } 
	  if(m==58 && mm == 8) {     tw2p[m][mm]= 0.948820; } 
	  if(m==58 && mm == 9) {     tw2p[m][mm]= 2.197935; } 
	  if(m==58 && mm == 10) {     tw2p[m][mm]= 0.610935; } 
	  if(m==58 && mm == 11) {     tw2p[m][mm]= 0.230704; } 
	  if(m==58 && mm == 12) {     tw2p[m][mm]= 0.335282; } 
	  if(m==58 && mm == 14) {     tw2p[m][mm]= 0.310779; } 
	  if(m==58 && mm == 15) {     tw2p[m][mm]= 0.524598; } 
	  if(m==58 && mm == 18) {     tw2p[m][mm]= 0.262990; } 
	  if(m==58 && mm == 19) {     tw2p[m][mm]= 0.136066; } 
	  if(m==58 && mm == 21) {     tw2p[m][mm]= 0.498708; } 
	  if(m==58 && mm == 22) {     tw2p[m][mm]= 0.317742; } 
	  if(m==58 && mm == 23) {     tw2p[m][mm]= 0.510456; } 
	  if(m==58 && mm == 30) {     tw2p[m][mm]= 0.444329; } 
	  if(m==58 && mm == 31) {     tw2p[m][mm]= 0.160436; } 
	  if(m==58 && mm == 32) {     tw2p[m][mm]= 0.400253; } 
	  if(m==58 && mm == 33) {     tw2p[m][mm]= 0.358228; } 
	  if(m==58 && mm == 34) {     tw2p[m][mm]= 1.149570; } 
	  if(m==58 && mm == 35) {     tw2p[m][mm]= 0.256131; } 
	  if(m==58 && mm == 0) {     tw3p[m][mm]= 0.579302; } 
	  if(m==58 && mm == 4) {     tw3p[m][mm]= 0.151617; } 
	  if(m==58 && mm == 7) {     tw3p[m][mm]= 0.377027; } 
	  if(m==58 && mm == 10) {     tw3p[m][mm]= 0.209856; } 
	  if(m==58 && mm == 11) {     tw3p[m][mm]= 0.239057; } 
	  if(m==58 && mm == 34) {     tw3p[m][mm]= 0.174344; } 
	  if(m==58 && mm == 35) {     tw3p[m][mm]= 0.759274; } 
	  if(m==58 && mm == 2) {     te2p[m][mm]= 0.763717; } 
	  if(m==58 && mm == 3) {     te2p[m][mm]= 0.343948; } 
	  if(m==58 && mm == 4) {     te2p[m][mm]= 0.375107; } 
	  if(m==58 && mm == 6) {     te2p[m][mm]= 0.636376; } 
	  if(m==58 && mm == 7) {     te2p[m][mm]= 0.550828; } 
	  if(m==58 && mm == 8) {     te2p[m][mm]= 1.093006; } 
	  if(m==58 && mm == 9) {     te2p[m][mm]= 0.250986; } 
	  if(m==58 && mm == 10) {     te2p[m][mm]= 1.416333; } 
	  if(m==58 && mm == 11) {     te2p[m][mm]= 0.314721; } 
	  if(m==58 && mm == 12) {     te2p[m][mm]= 0.428015; } 
	  if(m==58 && mm == 16) {     te2p[m][mm]= 1.943270; } 
	  if(m==58 && mm == 19) {     te2p[m][mm]= 0.248060; } 
	  if(m==58 && mm == 20) {     te2p[m][mm]= 0.125175; } 
	  if(m==58 && mm == 21) {     te2p[m][mm]= 0.592633; } 
	  if(m==58 && mm == 22) {     te2p[m][mm]= 0.123653; } 
	  if(m==58 && mm == 23) {     te2p[m][mm]= 0.167593; } 
	  if(m==58 && mm == 24) {     te2p[m][mm]= 0.090875; } 
	  if(m==58 && mm == 27) {     te2p[m][mm]= 0.079508; } 
	  if(m==58 && mm == 30) {     te2p[m][mm]= 0.774281; } 
	  if(m==58 && mm == 2) {     te3p[m][mm]= 0.623166; } 
	  if(m==58 && mm == 4) {     te3p[m][mm]= 0.234555; } 
	  if(m==58 && mm == 5) {     te3p[m][mm]= 0.184688; } 
	  if(m==58 && mm == 6) {     te3p[m][mm]= 0.054687; } 
	  if(m==58 && mm == 7) {     te3p[m][mm]= 0.822398; } 
	  if(m==58 && mm == 11) {     te3p[m][mm]= 0.031884; } 
	  if(m==58 && mm == 12) {     te3p[m][mm]= 0.070200; } 
	  if(m==58 && mm == 14) {     te3p[m][mm]= 0.783951; } 
	  if(m==58 && mm == 15) {     te3p[m][mm]= 0.270410; } 
	  if(m==58 && mm == 16) {     te3p[m][mm]= 0.128392; } 
	  if(m==58 && mm == 17) {     te3p[m][mm]= 0.072790; } 
	  if(m==58 && mm == 19) {     te3p[m][mm]= 1.545497; } 
	  if(m==58 && mm == 20) {     te3p[m][mm]= 0.325886; } 
	  if(m==58 && mm == 21) {     te3p[m][mm]= 0.582436; } 
	  if(m==58 && mm == 22) {     te3p[m][mm]= 0.811955; } 
	  if(m==58 && mm == 23) {     te3p[m][mm]= 0.140930; } 
	  if(m==58 && mm == 0) {     tw0n[m][mm]= 0.085602; } 
	  if(m==58 && mm == 5) {     tw0n[m][mm]= 0.302375; } 
	  if(m==58 && mm == 14) {     tw0n[m][mm]= 0.100784; } 
	  if(m==58 && mm == 15) {     tw0n[m][mm]= 0.197552; } 
	  if(m==58 && mm == 16) {     tw0n[m][mm]= 0.957040; } 
	  if(m==58 && mm == 17) {     tw0n[m][mm]= 0.340958; } 
	  if(m==58 && mm == 18) {     tw0n[m][mm]= 0.560429; } 
	  if(m==58 && mm == 19) {     tw0n[m][mm]= 0.105999; } 
	  if(m==58 && mm == 20) {     tw0n[m][mm]= 0.544966; } 
	  if(m==58 && mm == 22) {     tw0n[m][mm]= 0.046472; } 
	  if(m==58 && mm == 23) {     tw0n[m][mm]= 0.801163; } 
	  if(m==58 && mm == 24) {     tw0n[m][mm]= 0.105644; } 
	  if(m==58 && mm == 25) {     tw0n[m][mm]= 0.033811; } 
	  if(m==58 && mm == 26) {     tw0n[m][mm]= 0.541808; } 
	  if(m==58 && mm == 27) {     tw0n[m][mm]= 0.328077; } 
	  if(m==58 && mm == 28) {     tw0n[m][mm]= 0.458539; } 
	  if(m==58 && mm == 29) {     tw0n[m][mm]= 0.088088; } 
	  if(m==58 && mm == 30) {     tw0n[m][mm]= 0.370974; } 
	  if(m==58 && mm == 6) {     tw1n[m][mm]= 0.799746; } 
	  if(m==58 && mm == 7) {     tw1n[m][mm]= 0.609575; } 
	  if(m==58 && mm == 8) {     tw1n[m][mm]= 0.411525; } 
	  if(m==58 && mm == 9) {     tw1n[m][mm]= 0.115749; } 
	  if(m==58 && mm == 10) {     tw1n[m][mm]= 0.244338; } 
	  if(m==58 && mm == 11) {     tw1n[m][mm]= 0.246410; } 
	  if(m==58 && mm == 12) {     tw1n[m][mm]= 0.274769; } 
	  if(m==58 && mm == 13) {     tw1n[m][mm]= 0.334715; } 
	  if(m==58 && mm == 14) {     tw1n[m][mm]= 0.041661; } 
	  if(m==58 && mm == 15) {     tw1n[m][mm]= 0.278762; } 
	  if(m==58 && mm == 17) {     tw1n[m][mm]= 0.045728; } 
	  if(m==58 && mm == 18) {     tw1n[m][mm]= 0.225167; } 
	  if(m==58 && mm == 20) {     tw1n[m][mm]= 0.438884; } 
	  if(m==58 && mm == 21) {     tw1n[m][mm]= 0.061827; } 
	  if(m==58 && mm == 22) {     tw1n[m][mm]= 0.368603; } 
	  if(m==58 && mm == 23) {     tw1n[m][mm]= 0.198693; } 
	  if(m==58 && mm == 29) {     tw1n[m][mm]= 0.593535; } 
	  if(m==58 && mm == 30) {     tw1n[m][mm]= 0.145209; } 
	  if(m==58 && mm == 34) {     tw1n[m][mm]= 0.353530; } 
	  if(m==58 && mm == 35) {     tw1n[m][mm]= 0.810904; } 
	  if(m==58 && mm == 0) {     tw2n[m][mm]= 0.303879; } 
	  if(m==58 && mm == 5) {     tw2n[m][mm]= 0.154014; } 
	  if(m==58 && mm == 14) {     tw2n[m][mm]= 0.228107; } 
	  if(m==58 && mm == 15) {     tw2n[m][mm]= 0.411640; } 
	  if(m==58 && mm == 16) {     tw2n[m][mm]= 0.026761; } 
	  if(m==58 && mm == 18) {     tw2n[m][mm]= 0.169512; } 
	  if(m==58 && mm == 19) {     tw2n[m][mm]= 0.080189; } 
	  if(m==58 && mm == 22) {     tw2n[m][mm]= 0.304686; } 
	  if(m==58 && mm == 23) {     tw2n[m][mm]= 0.251362; } 
	  if(m==58 && mm == 26) {     tw2n[m][mm]= 0.151110; } 
	  if(m==58 && mm == 28) {     tw2n[m][mm]= 0.220529; } 
	  if(m==58 && mm == 30) {     tw2n[m][mm]= 0.217045; } 
	  if(m==58 && mm == 32) {     tw2n[m][mm]= 0.637847; } 
	  if(m==58 && mm == 33) {     tw2n[m][mm]= 0.185459; } 
	  if(m==58 && mm == 34) {     tw2n[m][mm]= 0.394873; } 
	  if(m==58 && mm == 35) {     tw2n[m][mm]= 1.049224; } 
	  if(m==58 && mm == 0) {     tw3n[m][mm]= 0.543593; } 
	  if(m==58 && mm == 4) {     tw3n[m][mm]= 0.182339; } 
	  if(m==58 && mm == 7) {     tw3n[m][mm]= 0.255121; } 
	  if(m==58 && mm == 10) {     tw3n[m][mm]= 0.239184; } 
	  if(m==58 && mm == 11) {     tw3n[m][mm]= 0.224111; } 
	  if(m==58 && mm == 28) {     tw3n[m][mm]= 0.732327; } 
	  if(m==58 && mm == 6) {     te2n[m][mm]= 0.636139; } 
	  if(m==58 && mm == 7) {     te2n[m][mm]= 0.651825; } 
	  if(m==58 && mm == 8) {     te2n[m][mm]= 0.613419; } 
	  if(m==58 && mm == 9) {     te2n[m][mm]= 0.099879; } 
	  if(m==58 && mm == 10) {     te2n[m][mm]= 0.709695; } 
	  if(m==58 && mm == 11) {     te2n[m][mm]= 0.123605; } 
	  if(m==58 && mm == 12) {     te2n[m][mm]= 0.144236; } 
	  if(m==58 && mm == 16) {     te2n[m][mm]= 1.409379; } 
	  if(m==58 && mm == 19) {     te2n[m][mm]= 0.308228; } 
	  if(m==58 && mm == 20) {     te2n[m][mm]= 0.164141; } 
	  if(m==58 && mm == 22) {     te2n[m][mm]= 0.116812; } 
	  if(m==58 && mm == 23) {     te2n[m][mm]= 0.248119; } 
	  if(m==58 && mm == 24) {     te2n[m][mm]= 0.223181; } 
	  if(m==58 && mm == 26) {     te2n[m][mm]= 0.080898; } 
	  if(m==58 && mm == 27) {     te2n[m][mm]= 0.270730; } 
	  if(m==58 && mm == 28) {     te2n[m][mm]= 0.069078; } 
	  if(m==58 && mm == 30) {     te2n[m][mm]= 0.670176; } 
	  if(m==58 && mm == 31) {     te2n[m][mm]= 0.254396; } 
	  if(m==58 && mm == 32) {     te2n[m][mm]= 0.618601; } 
	  if(m==58 && mm == 34) {     te2n[m][mm]= 1.005637; } 
	  if(m==58 && mm == 1) {     te3n[m][mm]= 0.712655; } 
	  if(m==58 && mm == 2) {     te3n[m][mm]= 0.301523; } 
	  if(m==58 && mm == 3) {     te3n[m][mm]= 0.234632; } 
	  if(m==58 && mm == 4) {     te3n[m][mm]= 0.294606; } 
	  if(m==58 && mm == 6) {     te3n[m][mm]= 0.770228; } 
	  if(m==58 && mm == 15) {     te3n[m][mm]= 0.200143; } 
	  if(m==58 && mm == 16) {     te3n[m][mm]= 0.098975; } 
	  if(m==58 && mm == 20) {     te3n[m][mm]= 0.241851; } 
	  if(m==58 && mm == 21) {     te3n[m][mm]= 0.754922; } 
	  if(m==58 && mm == 22) {     te3n[m][mm]= 0.780390; } 
	  if(m==58 && mm == 23) {     te3n[m][mm]= 0.259523; } 
	  if(m==58 && mm == 30) {     te3n[m][mm]= 1.805379; } 
	  if(m==58 && mm == 31) {     te3n[m][mm]= 0.387496; } 
	  if(m==58 && mm == 32) {     te3n[m][mm]= 0.093573; } 
	  if(m==58 && mm == 33) {     te3n[m][mm]= 0.269797; } 
	  if(m==58 && mm == 34) {     te3n[m][mm]= 0.032744; } 
	  if(m==59 && mm == 9) {     tw0p[m][mm]= 0.046378; } 
	  if(m==59 && mm == 12) {     tw0p[m][mm]= 0.342246; } 
	  if(m==59 && mm == 13) {     tw0p[m][mm]= 0.230526; } 
	  if(m==59 && mm == 15) {     tw0p[m][mm]= 0.531340; } 
	  if(m==59 && mm == 16) {     tw0p[m][mm]= 0.160372; } 
	  if(m==59 && mm == 17) {     tw0p[m][mm]= 0.526902; } 
	  if(m==59 && mm == 18) {     tw0p[m][mm]= 0.977935; } 
	  if(m==59 && mm == 19) {     tw0p[m][mm]= 0.345734; } 
	  if(m==59 && mm == 20) {     tw0p[m][mm]= 0.064638; } 
	  if(m==59 && mm == 21) {     tw0p[m][mm]= 0.144493; } 
	  if(m==59 && mm == 22) {     tw0p[m][mm]= 1.720424; } 
	  if(m==59 && mm == 23) {     tw0p[m][mm]= 0.243879; } 
	  if(m==59 && mm == 25) {     tw0p[m][mm]= 0.127591; } 
	  if(m==59 && mm == 27) {     tw0p[m][mm]= 0.426898; } 
	  if(m==59 && mm == 28) {     tw0p[m][mm]= 0.409486; } 
	  if(m==59 && mm == 30) {     tw0p[m][mm]= 0.501379; } 
	  if(m==59 && mm == 31) {     tw0p[m][mm]= 0.037947; } 
	  if(m==59 && mm == 33) {     tw0p[m][mm]= 0.343438; } 
	  if(m==59 && mm == 35) {     tw0p[m][mm]= 0.427686; } 
	  if(m==59 && mm == 0) {     tw1p[m][mm]= 0.623619; } 
	  if(m==59 && mm == 1) {     tw1p[m][mm]= 0.204605; } 
	  if(m==59 && mm == 2) {     tw1p[m][mm]= 0.862935; } 
	  if(m==59 && mm == 3) {     tw1p[m][mm]= 0.121159; } 
	  if(m==59 && mm == 6) {     tw1p[m][mm]= 0.283540; } 
	  if(m==59 && mm == 19) {     tw1p[m][mm]= 0.220181; } 
	  if(m==59 && mm == 20) {     tw1p[m][mm]= 0.032560; } 
	  if(m==59 && mm == 23) {     tw1p[m][mm]= 0.221497; } 
	  if(m==59 && mm == 24) {     tw1p[m][mm]= 0.222678; } 
	  if(m==59 && mm == 25) {     tw1p[m][mm]= 0.076658; } 
	  if(m==59 && mm == 26) {     tw1p[m][mm]= 0.357173; } 
	  if(m==59 && mm == 27) {     tw1p[m][mm]= 0.118002; } 
	  if(m==59 && mm == 28) {     tw1p[m][mm]= 0.436506; } 
	  if(m==59 && mm == 29) {     tw1p[m][mm]= 0.294815; } 
	  if(m==59 && mm == 30) {     tw1p[m][mm]= 0.234880; } 
	  if(m==59 && mm == 31) {     tw1p[m][mm]= 0.200257; } 
	  if(m==59 && mm == 32) {     tw1p[m][mm]= 0.237532; } 
	  if(m==59 && mm == 33) {     tw1p[m][mm]= 0.608008; } 
	  if(m==59 && mm == 34) {     tw1p[m][mm]= 0.191683; } 
	  if(m==59 && mm == 5) {     tw2p[m][mm]= 0.223012; } 
	  if(m==59 && mm == 8) {     tw2p[m][mm]= 0.778030; } 
	  if(m==59 && mm == 9) {     tw2p[m][mm]= 0.925702; } 
	  if(m==59 && mm == 10) {     tw2p[m][mm]= 0.165513; } 
	  if(m==59 && mm == 11) {     tw2p[m][mm]= 0.190234; } 
	  if(m==59 && mm == 12) {     tw2p[m][mm]= 0.455050; } 
	  if(m==59 && mm == 15) {     tw2p[m][mm]= 0.471410; } 
	  if(m==59 && mm == 16) {     tw2p[m][mm]= 0.049334; } 
	  if(m==59 && mm == 17) {     tw2p[m][mm]= 0.117368; } 
	  if(m==59 && mm == 18) {     tw2p[m][mm]= 0.236933; } 
	  if(m==59 && mm == 20) {     tw2p[m][mm]= 0.043823; } 
	  if(m==59 && mm == 21) {     tw2p[m][mm]= 0.162829; } 
	  if(m==59 && mm == 22) {     tw2p[m][mm]= 0.331393; } 
	  if(m==59 && mm == 30) {     tw2p[m][mm]= 0.564267; } 
	  if(m==59 && mm == 32) {     tw2p[m][mm]= 0.703564; } 
	  if(m==59 && mm == 33) {     tw2p[m][mm]= 0.117837; } 
	  if(m==59 && mm == 34) {     tw2p[m][mm]= 1.033730; } 
	  if(m==59 && mm == 1) {     tw3p[m][mm]= 0.159977; } 
	  if(m==59 && mm == 4) {     tw3p[m][mm]= 0.241374; } 
	  if(m==59 && mm == 5) {     tw3p[m][mm]= 0.041030; } 
	  if(m==59 && mm == 10) {     tw3p[m][mm]= 0.043074; } 
	  if(m==59 && mm == 28) {     tw3p[m][mm]= 0.023393; } 
	  if(m==59 && mm == 29) {     tw3p[m][mm]= 0.297590; } 
	  if(m==59 && mm == 32) {     tw3p[m][mm]= 0.759437; } 
	  if(m==59 && mm == 34) {     tw3p[m][mm]= 0.915564; } 
	  if(m==59 && mm == 35) {     tw3p[m][mm]= 0.140559; } 
	  if(m==59 && mm == 2) {     te2p[m][mm]= 0.230928; } 
	  if(m==59 && mm == 3) {     te2p[m][mm]= 1.373094; } 
	  if(m==59 && mm == 5) {     te2p[m][mm]= 0.236415; } 
	  if(m==59 && mm == 6) {     te2p[m][mm]= 0.742050; } 
	  if(m==59 && mm == 7) {     te2p[m][mm]= 0.076131; } 
	  if(m==59 && mm == 9) {     te2p[m][mm]= 0.527091; } 
	  if(m==59 && mm == 10) {     te2p[m][mm]= 0.272790; } 
	  if(m==59 && mm == 11) {     te2p[m][mm]= 0.360409; } 
	  if(m==59 && mm == 16) {     te2p[m][mm]= 0.027774; } 
	  if(m==59 && mm == 17) {     te2p[m][mm]= 0.439242; } 
	  if(m==59 && mm == 18) {     te2p[m][mm]= 0.760216; } 
	  if(m==59 && mm == 20) {     te2p[m][mm]= 0.336479; } 
	  if(m==59 && mm == 21) {     te2p[m][mm]= 0.138660; } 
	  if(m==59 && mm == 22) {     te2p[m][mm]= 0.158037; } 
	  if(m==59 && mm == 24) {     te2p[m][mm]= 0.363140; } 
	  if(m==59 && mm == 25) {     te2p[m][mm]= 0.222176; } 
	  if(m==59 && mm == 27) {     te2p[m][mm]= 0.069715; } 
	  if(m==59 && mm == 30) {     te2p[m][mm]= 0.764196; } 
	  if(m==59 && mm == 2) {     te3p[m][mm]= 0.542544; } 
	  if(m==59 && mm == 3) {     te3p[m][mm]= 0.544289; } 
	  if(m==59 && mm == 5) {     te3p[m][mm]= 0.188219; } 
	  if(m==59 && mm == 6) {     te3p[m][mm]= 0.462567; } 
	  if(m==59 && mm == 7) {     te3p[m][mm]= 0.681523; } 
	  if(m==59 && mm == 10) {     te3p[m][mm]= 0.633803; } 
	  if(m==59 && mm == 11) {     te3p[m][mm]= 0.551925; } 
	  if(m==59 && mm == 12) {     te3p[m][mm]= 0.859169; } 
	  if(m==59 && mm == 13) {     te3p[m][mm]= 0.530546; } 
	  if(m==59 && mm == 14) {     te3p[m][mm]= 1.163537; } 
	  if(m==59 && mm == 15) {     te3p[m][mm]= 0.122151; } 
	  if(m==59 && mm == 16) {     te3p[m][mm]= 0.696330; } 
	  if(m==59 && mm == 17) {     te3p[m][mm]= 0.548475; } 
	  if(m==59 && mm == 19) {     te3p[m][mm]= 0.457869; } 
	  if(m==59 && mm == 20) {     te3p[m][mm]= 0.444097; } 
	  if(m==59 && mm == 21) {     te3p[m][mm]= 0.294212; } 
	  if(m==59 && mm == 0) {     tw0n[m][mm]= 0.059404; } 
	  if(m==59 && mm == 1) {     tw0n[m][mm]= 0.276126; } 
	  if(m==59 && mm == 2) {     tw0n[m][mm]= 0.147163; } 
	  if(m==59 && mm == 3) {     tw0n[m][mm]= 0.438598; } 
	  if(m==59 && mm == 5) {     tw0n[m][mm]= 0.402855; } 
	  if(m==59 && mm == 9) {     tw0n[m][mm]= 0.216788; } 
	  if(m==59 && mm == 12) {     tw0n[m][mm]= 0.759887; } 
	  if(m==59 && mm == 13) {     tw0n[m][mm]= 0.321512; } 
	  if(m==59 && mm == 15) {     tw0n[m][mm]= 0.547151; } 
	  if(m==59 && mm == 16) {     tw0n[m][mm]= 0.182781; } 
	  if(m==59 && mm == 17) {     tw0n[m][mm]= 0.342097; } 
	  if(m==59 && mm == 18) {     tw0n[m][mm]= 0.800689; } 
	  if(m==59 && mm == 19) {     tw0n[m][mm]= 0.181859; } 
	  if(m==59 && mm == 20) {     tw0n[m][mm]= 0.640900; } 
	  if(m==59 && mm == 21) {     tw0n[m][mm]= 0.127146; } 
	  if(m==59 && mm == 22) {     tw0n[m][mm]= 1.301276; } 
	  if(m==59 && mm == 23) {     tw0n[m][mm]= 0.341604; } 
	  if(m==59 && mm == 24) {     tw0n[m][mm]= 0.730653; } 
	  if(m==59 && mm == 25) {     tw0n[m][mm]= 0.118272; } 
	  if(m==59 && mm == 26) {     tw0n[m][mm]= 0.410117; } 
	  if(m==59 && mm == 27) {     tw0n[m][mm]= 0.232731; } 
	  if(m==59 && mm == 28) {     tw0n[m][mm]= 0.053793; } 
	  if(m==59 && mm == 30) {     tw0n[m][mm]= 0.086018; } 
	  if(m==59 && mm == 11) {     tw1n[m][mm]= 0.153140; } 
	  if(m==59 && mm == 12) {     tw1n[m][mm]= 0.976470; } 
	  if(m==59 && mm == 13) {     tw1n[m][mm]= 0.161579; } 
	  if(m==59 && mm == 14) {     tw1n[m][mm]= 0.203220; } 
	  if(m==59 && mm == 15) {     tw1n[m][mm]= 1.383869; } 
	  if(m==59 && mm == 16) {     tw1n[m][mm]= 0.092946; } 
	  if(m==59 && mm == 17) {     tw1n[m][mm]= 1.234743; } 
	  if(m==59 && mm == 18) {     tw1n[m][mm]= 0.411447; } 
	  if(m==59 && mm == 19) {     tw1n[m][mm]= 0.058507; } 
	  if(m==59 && mm == 20) {     tw1n[m][mm]= 0.107720; } 
	  if(m==59 && mm == 21) {     tw1n[m][mm]= 0.406727; } 
	  if(m==59 && mm == 23) {     tw1n[m][mm]= 0.293085; } 
	  if(m==59 && mm == 24) {     tw1n[m][mm]= 0.632831; } 
	  if(m==59 && mm == 29) {     tw1n[m][mm]= 0.154257; } 
	  if(m==59 && mm == 30) {     tw1n[m][mm]= 0.124091; } 
	  if(m==59 && mm == 31) {     tw1n[m][mm]= 0.568848; } 
	  if(m==59 && mm == 33) {     tw1n[m][mm]= 0.367404; } 
	  if(m==59 && mm == 34) {     tw1n[m][mm]= 0.144898; } 
	  if(m==59 && mm == 0) {     tw2n[m][mm]= 0.086123; } 
	  if(m==59 && mm == 2) {     tw2n[m][mm]= 0.068116; } 
	  if(m==59 && mm == 4) {     tw2n[m][mm]= 0.387426; } 
	  if(m==59 && mm == 5) {     tw2n[m][mm]= 0.449251; } 
	  if(m==59 && mm == 15) {     tw2n[m][mm]= 0.819175; } 
	  if(m==59 && mm == 17) {     tw2n[m][mm]= 0.053516; } 
	  if(m==59 && mm == 20) {     tw2n[m][mm]= 0.036190; } 
	  if(m==59 && mm == 22) {     tw2n[m][mm]= 0.299433; } 
	  if(m==59 && mm == 23) {     tw2n[m][mm]= 0.286270; } 
	  if(m==59 && mm == 24) {     tw2n[m][mm]= 0.360040; } 
	  if(m==59 && mm == 25) {     tw2n[m][mm]= 0.553097; } 
	  if(m==59 && mm == 26) {     tw2n[m][mm]= 0.060669; } 
	  if(m==59 && mm == 28) {     tw2n[m][mm]= 2.122031; } 
	  if(m==59 && mm == 29) {     tw2n[m][mm]= 0.719082; } 
	  if(m==59 && mm == 30) {     tw2n[m][mm]= 0.038218; } 
	  if(m==59 && mm == 31) {     tw2n[m][mm]= 0.378690; } 
	  if(m==59 && mm == 32) {     tw2n[m][mm]= 0.446835; } 
	  if(m==59 && mm == 34) {     tw2n[m][mm]= 0.645231; } 
	  if(m==59 && mm == 1) {     tw3n[m][mm]= 0.179439; } 
	  if(m==59 && mm == 4) {     tw3n[m][mm]= 0.234788; } 
	  if(m==59 && mm == 5) {     tw3n[m][mm]= 0.024678; } 
	  if(m==59 && mm == 6) {     te2n[m][mm]= 0.413308; } 
	  if(m==59 && mm == 9) {     te2n[m][mm]= 0.294293; } 
	  if(m==59 && mm == 10) {     te2n[m][mm]= 0.136528; } 
	  if(m==59 && mm == 11) {     te2n[m][mm]= 0.078782; } 
	  if(m==59 && mm == 17) {     te2n[m][mm]= 0.401670; } 
	  if(m==59 && mm == 18) {     te2n[m][mm]= 1.150232; } 
	  if(m==59 && mm == 20) {     te2n[m][mm]= 0.242758; } 
	  if(m==59 && mm == 22) {     te2n[m][mm]= 0.185552; } 
	  if(m==59 && mm == 24) {     te2n[m][mm]= 0.220937; } 
	  if(m==59 && mm == 25) {     te2n[m][mm]= 0.295182; } 
	  if(m==59 && mm == 26) {     te2n[m][mm]= 0.163666; } 
	  if(m==59 && mm == 27) {     te2n[m][mm]= 0.113084; } 
	  if(m==59 && mm == 31) {     te2n[m][mm]= 0.423768; } 
	  if(m==59 && mm == 32) {     te2n[m][mm]= 0.167394; } 
	  if(m==59 && mm == 33) {     te2n[m][mm]= 0.382842; } 
	  if(m==59 && mm == 34) {     te2n[m][mm]= 0.292837; } 
	  if(m==59 && mm == 35) {     te2n[m][mm]= 0.176518; } 
	  if(m==59 && mm == 0) {     te3n[m][mm]= 0.681782; } 
	  if(m==59 && mm == 1) {     te3n[m][mm]= 0.801626; } 
	  if(m==59 && mm == 2) {     te3n[m][mm]= 0.456980; } 
	  if(m==59 && mm == 3) {     te3n[m][mm]= 0.562890; } 
	  if(m==59 && mm == 4) {     te3n[m][mm]= 0.049996; } 
	  if(m==59 && mm == 5) {     te3n[m][mm]= 0.002692; } 
	  if(m==59 && mm == 6) {     te3n[m][mm]= 0.184246; } 
	  if(m==59 && mm == 14) {     te3n[m][mm]= 2.019297; } 
	  if(m==59 && mm == 15) {     te3n[m][mm]= 0.009967; } 
	  if(m==59 && mm == 16) {     te3n[m][mm]= 0.914155; } 
	  if(m==59 && mm == 17) {     te3n[m][mm]= 0.368552; } 
	  if(m==59 && mm == 19) {     te3n[m][mm]= 0.412728; } 
	  if(m==59 && mm == 20) {     te3n[m][mm]= 0.632692; } 
	  if(m==59 && mm == 21) {     te3n[m][mm]= 0.165410; } 
	  if(m==59 && mm == 22) {     te3n[m][mm]= 0.080677; } 
	  if(m==59 && mm == 23) {     te3n[m][mm]= 1.498528; } 
	  if(m==59 && mm == 30) {     te3n[m][mm]= 0.520824; } 
	  if(m==59 && mm == 31) {     te3n[m][mm]= 0.572363; } 
	  if(m==59 && mm == 35) {     te3n[m][mm]= 0.023708; } 
	  if(m==60 && mm == 7) {     tw0p[m][mm]= 1.674279; } 
	  if(m==60 && mm == 8) {     tw0p[m][mm]= 0.679198; } 
	  if(m==60 && mm == 9) {     tw0p[m][mm]= 0.122649; } 
	  if(m==60 && mm == 10) {     tw0p[m][mm]= 0.132315; } 
	  if(m==60 && mm == 11) {     tw0p[m][mm]= 0.159967; } 
	  if(m==60 && mm == 12) {     tw0p[m][mm]= 0.516735; } 
	  if(m==60 && mm == 14) {     tw0p[m][mm]= 0.046096; } 
	  if(m==60 && mm == 17) {     tw0p[m][mm]= 0.358145; } 
	  if(m==60 && mm == 18) {     tw0p[m][mm]= 0.580474; } 
	  if(m==60 && mm == 19) {     tw0p[m][mm]= 0.197493; } 
	  if(m==60 && mm == 20) {     tw0p[m][mm]= 0.537609; } 
	  if(m==60 && mm == 21) {     tw0p[m][mm]= 0.245455; } 
	  if(m==60 && mm == 22) {     tw0p[m][mm]= 1.935625; } 
	  if(m==60 && mm == 23) {     tw0p[m][mm]= 0.427469; } 
	  if(m==60 && mm == 24) {     tw0p[m][mm]= 0.274943; } 
	  if(m==60 && mm == 26) {     tw0p[m][mm]= 0.046256; } 
	  if(m==60 && mm == 28) {     tw0p[m][mm]= 0.506999; } 
	  if(m==60 && mm == 30) {     tw0p[m][mm]= 0.365664; } 
	  if(m==60 && mm == 32) {     tw0p[m][mm]= 0.072296; } 
	  if(m==60 && mm == 33) {     tw0p[m][mm]= 0.015624; } 
	  if(m==60 && mm == 34) {     tw0p[m][mm]= 0.067477; } 
	  if(m==60 && mm == 35) {     tw0p[m][mm]= 0.545581; } 
	  if(m==60 && mm == 0) {     tw1p[m][mm]= 0.105919; } 
	  if(m==60 && mm == 1) {     tw1p[m][mm]= 0.099404; } 
	  if(m==60 && mm == 2) {     tw1p[m][mm]= 0.000331; } 
	  if(m==60 && mm == 7) {     tw1p[m][mm]= 0.114825; } 
	  if(m==60 && mm == 8) {     tw1p[m][mm]= 0.046106; } 
	  if(m==60 && mm == 19) {     tw1p[m][mm]= 0.219602; } 
	  if(m==60 && mm == 20) {     tw1p[m][mm]= 0.905685; } 
	  if(m==60 && mm == 22) {     tw1p[m][mm]= 3.419108; } 
	  if(m==60 && mm == 23) {     tw1p[m][mm]= 0.637562; } 
	  if(m==60 && mm == 25) {     tw1p[m][mm]= 0.154060; } 
	  if(m==60 && mm == 28) {     tw1p[m][mm]= 0.226817; } 
	  if(m==60 && mm == 29) {     tw1p[m][mm]= 0.173397; } 
	  if(m==60 && mm == 32) {     tw1p[m][mm]= 0.197085; } 
	  if(m==60 && mm == 34) {     tw1p[m][mm]= 0.977889; } 
	  if(m==60 && mm == 35) {     tw1p[m][mm]= 0.707861; } 
	  if(m==60 && mm == 11) {     tw2p[m][mm]= 0.433635; } 
	  if(m==60 && mm == 20) {     tw2p[m][mm]= 0.770388; } 
	  if(m==60 && mm == 21) {     tw2p[m][mm]= 0.260069; } 
	  if(m==60 && mm == 0) {     tw3p[m][mm]= 0.766416; } 
	  if(m==60 && mm == 1) {     tw3p[m][mm]= 0.053514; } 
	  if(m==60 && mm == 2) {     tw3p[m][mm]= 0.426253; } 
	  if(m==60 && mm == 3) {     tw3p[m][mm]= 0.694471; } 
	  if(m==60 && mm == 4) {     tw3p[m][mm]= 0.151666; } 
	  if(m==60 && mm == 5) {     tw3p[m][mm]= 0.368582; } 
	  if(m==60 && mm == 6) {     tw3p[m][mm]= 0.694845; } 
	  if(m==60 && mm == 7) {     tw3p[m][mm]= 0.448840; } 
	  if(m==60 && mm == 8) {     tw3p[m][mm]= 0.076413; } 
	  if(m==60 && mm == 9) {     tw3p[m][mm]= 0.606757; } 
	  if(m==60 && mm == 10) {     tw3p[m][mm]= 0.727317; } 
	  if(m==60 && mm == 11) {     tw3p[m][mm]= 1.270833; } 
	  if(m==60 && mm == 12) {     tw3p[m][mm]= 0.912183; } 
	  if(m==60 && mm == 13) {     tw3p[m][mm]= 0.350318; } 
	  if(m==60 && mm == 18) {     tw3p[m][mm]= 4.280660; } 
	  if(m==60 && mm == 22) {     tw3p[m][mm]= 1.336035; } 
	  if(m==60 && mm == 32) {     tw3p[m][mm]= 0.481168; } 
	  if(m==60 && mm == 33) {     tw3p[m][mm]= 0.615892; } 
	  if(m==60 && mm == 34) {     tw3p[m][mm]= 1.162487; } 
	  if(m==60 && mm == 35) {     tw3p[m][mm]= 0.953226; } 
	  if(m==60 && mm == 0) {     te2p[m][mm]= 0.275282; } 
	  if(m==60 && mm == 1) {     te2p[m][mm]= 0.633742; } 
	  if(m==60 && mm == 3) {     te2p[m][mm]= 0.181693; } 
	  if(m==60 && mm == 6) {     te2p[m][mm]= 0.111511; } 
	  if(m==60 && mm == 7) {     te2p[m][mm]= 0.286499; } 
	  if(m==60 && mm == 8) {     te2p[m][mm]= 0.719662; } 
	  if(m==60 && mm == 9) {     te2p[m][mm]= 0.347556; } 
	  if(m==60 && mm == 10) {     te2p[m][mm]= 0.015318; } 
	  if(m==60 && mm == 11) {     te2p[m][mm]= 0.337527; } 
	  if(m==60 && mm == 12) {     te2p[m][mm]= 0.381919; } 
	  if(m==60 && mm == 13) {     te2p[m][mm]= 0.435237; } 
	  if(m==60 && mm == 14) {     te2p[m][mm]= 0.361793; } 
	  if(m==60 && mm == 15) {     te2p[m][mm]= 0.096758; } 
	  if(m==60 && mm == 16) {     te2p[m][mm]= 0.318724; } 
	  if(m==60 && mm == 17) {     te2p[m][mm]= 1.431415; } 
	  if(m==60 && mm == 19) {     te2p[m][mm]= 1.483709; } 
	  if(m==60 && mm == 20) {     te2p[m][mm]= 0.193081; } 
	  if(m==60 && mm == 21) {     te2p[m][mm]= 0.629007; } 
	  if(m==60 && mm == 22) {     te2p[m][mm]= 0.309217; } 
	  if(m==60 && mm == 23) {     te2p[m][mm]= 0.406067; } 
	  if(m==60 && mm == 24) {     te2p[m][mm]= 0.264613; } 
	  if(m==60 && mm == 25) {     te2p[m][mm]= 0.380325; } 
	  if(m==60 && mm == 28) {     te2p[m][mm]= 0.474182; } 
	  if(m==60 && mm == 30) {     te2p[m][mm]= 0.180553; } 
	  if(m==60 && mm == 3) {     te3p[m][mm]= 0.502394; } 
	  if(m==60 && mm == 4) {     te3p[m][mm]= 0.889523; } 
	  if(m==60 && mm == 6) {     te3p[m][mm]= 0.341036; } 
	  if(m==60 && mm == 7) {     te3p[m][mm]= 0.842212; } 
	  if(m==60 && mm == 8) {     te3p[m][mm]= 0.304822; } 
	  if(m==60 && mm == 9) {     te3p[m][mm]= 0.055061; } 
	  if(m==60 && mm == 10) {     te3p[m][mm]= 0.907886; } 
	  if(m==60 && mm == 19) {     te3p[m][mm]= 0.529933; } 
	  if(m==60 && mm == 20) {     te3p[m][mm]= 0.177752; } 
	  if(m==60 && mm == 22) {     te3p[m][mm]= 0.386348; } 
	  if(m==60 && mm == 23) {     te3p[m][mm]= 0.104374; } 
	  if(m==60 && mm == 0) {     tw0n[m][mm]= 0.597784; } 
	  if(m==60 && mm == 1) {     tw0n[m][mm]= 0.390002; } 
	  if(m==60 && mm == 3) {     tw0n[m][mm]= 0.540356; } 
	  if(m==60 && mm == 4) {     tw0n[m][mm]= 1.983352; } 
	  if(m==60 && mm == 5) {     tw0n[m][mm]= 0.880070; } 
	  if(m==60 && mm == 6) {     tw0n[m][mm]= 2.307022; } 
	  if(m==60 && mm == 7) {     tw0n[m][mm]= 1.655355; } 
	  if(m==60 && mm == 8) {     tw0n[m][mm]= 1.028569; } 
	  if(m==60 && mm == 9) {     tw0n[m][mm]= 0.328975; } 
	  if(m==60 && mm == 10) {     tw0n[m][mm]= 0.114998; } 
	  if(m==60 && mm == 11) {     tw0n[m][mm]= 0.218882; } 
	  if(m==60 && mm == 12) {     tw0n[m][mm]= 0.717675; } 
	  if(m==60 && mm == 14) {     tw0n[m][mm]= 0.086123; } 
	  if(m==60 && mm == 18) {     tw0n[m][mm]= 0.681884; } 
	  if(m==60 && mm == 19) {     tw0n[m][mm]= 0.028591; } 
	  if(m==60 && mm == 20) {     tw0n[m][mm]= 0.646195; } 
	  if(m==60 && mm == 21) {     tw0n[m][mm]= 0.200405; } 
	  if(m==60 && mm == 22) {     tw0n[m][mm]= 1.834616; } 
	  if(m==60 && mm == 23) {     tw0n[m][mm]= 0.787393; } 
	  if(m==60 && mm == 24) {     tw0n[m][mm]= 0.342410; } 
	  if(m==60 && mm == 28) {     tw0n[m][mm]= 0.369338; } 
	  if(m==60 && mm == 8) {     tw1n[m][mm]= 0.386916; } 
	  if(m==60 && mm == 9) {     tw1n[m][mm]= 0.150292; } 
	  if(m==60 && mm == 10) {     tw1n[m][mm]= 0.389592; } 
	  if(m==60 && mm == 11) {     tw1n[m][mm]= 0.358962; } 
	  if(m==60 && mm == 12) {     tw1n[m][mm]= 0.005991; } 
	  if(m==60 && mm == 17) {     tw1n[m][mm]= 0.235896; } 
	  if(m==60 && mm == 19) {     tw1n[m][mm]= 0.220450; } 
	  if(m==60 && mm == 20) {     tw1n[m][mm]= 1.010787; } 
	  if(m==60 && mm == 21) {     tw1n[m][mm]= 1.305411; } 
	  if(m==60 && mm == 22) {     tw1n[m][mm]= 2.714180; } 
	  if(m==60 && mm == 23) {     tw1n[m][mm]= 0.328604; } 
	  if(m==60 && mm == 24) {     tw1n[m][mm]= 0.273077; } 
	  if(m==60 && mm == 5) {     tw2n[m][mm]= 1.518426; } 
	  if(m==60 && mm == 20) {     tw2n[m][mm]= 0.685157; } 
	  if(m==60 && mm == 21) {     tw2n[m][mm]= 0.075539; } 
	  if(m==60 && mm == 24) {     tw2n[m][mm]= 0.280566; } 
	  if(m==60 && mm == 25) {     tw2n[m][mm]= 0.036854; } 
	  if(m==60 && mm == 26) {     tw2n[m][mm]= 0.244264; } 
	  if(m==60 && mm == 0) {     tw3n[m][mm]= 0.638725; } 
	  if(m==60 && mm == 1) {     tw3n[m][mm]= 0.136707; } 
	  if(m==60 && mm == 2) {     tw3n[m][mm]= 0.579540; } 
	  if(m==60 && mm == 3) {     tw3n[m][mm]= 0.813689; } 
	  if(m==60 && mm == 4) {     tw3n[m][mm]= 0.107222; } 
	  if(m==60 && mm == 5) {     tw3n[m][mm]= 0.549552; } 
	  if(m==60 && mm == 6) {     tw3n[m][mm]= 0.740592; } 
	  if(m==60 && mm == 7) {     tw3n[m][mm]= 0.412432; } 
	  if(m==60 && mm == 8) {     tw3n[m][mm]= 0.875593; } 
	  if(m==60 && mm == 9) {     tw3n[m][mm]= 0.991804; } 
	  if(m==60 && mm == 10) {     tw3n[m][mm]= 1.077339; } 
	  if(m==60 && mm == 11) {     tw3n[m][mm]= 1.194591; } 
	  if(m==60 && mm == 12) {     tw3n[m][mm]= 0.869336; } 
	  if(m==60 && mm == 13) {     tw3n[m][mm]= 0.361811; } 
	  if(m==60 && mm == 22) {     tw3n[m][mm]= 0.981561; } 
	  if(m==60 && mm == 1) {     te2n[m][mm]= 0.744573; } 
	  if(m==60 && mm == 6) {     te2n[m][mm]= 0.226591; } 
	  if(m==60 && mm == 7) {     te2n[m][mm]= 0.212462; } 
	  if(m==60 && mm == 8) {     te2n[m][mm]= 0.602963; } 
	  if(m==60 && mm == 9) {     te2n[m][mm]= 0.227702; } 
	  if(m==60 && mm == 11) {     te2n[m][mm]= 0.214818; } 
	  if(m==60 && mm == 12) {     te2n[m][mm]= 0.247197; } 
	  if(m==60 && mm == 13) {     te2n[m][mm]= 0.486439; } 
	  if(m==60 && mm == 14) {     te2n[m][mm]= 0.137702; } 
	  if(m==60 && mm == 15) {     te2n[m][mm]= 0.107219; } 
	  if(m==60 && mm == 16) {     te2n[m][mm]= 0.274806; } 
	  if(m==60 && mm == 17) {     te2n[m][mm]= 0.928043; } 
	  if(m==60 && mm == 19) {     te2n[m][mm]= 1.389966; } 
	  if(m==60 && mm == 20) {     te2n[m][mm]= 0.172300; } 
	  if(m==60 && mm == 21) {     te2n[m][mm]= 0.571248; } 
	  if(m==60 && mm == 22) {     te2n[m][mm]= 0.189980; } 
	  if(m==60 && mm == 23) {     te2n[m][mm]= 0.303743; } 
	  if(m==60 && mm == 24) {     te2n[m][mm]= 0.512063; } 
	  if(m==60 && mm == 25) {     te2n[m][mm]= 0.170791; } 
	  if(m==60 && mm == 28) {     te2n[m][mm]= 0.625062; } 
	  if(m==60 && mm == 30) {     te2n[m][mm]= 0.218088; } 
	  if(m==60 && mm == 31) {     te2n[m][mm]= 0.894592; } 
	  if(m==60 && mm == 32) {     te2n[m][mm]= 0.672586; } 
	  if(m==60 && mm == 34) {     te2n[m][mm]= 0.512419; } 
	  if(m==60 && mm == 35) {     te2n[m][mm]= 0.270012; } 
	  if(m==60 && mm == 0) {     te3n[m][mm]= 0.203402; } 
	  if(m==60 && mm == 3) {     te3n[m][mm]= 0.193445; } 
	  if(m==60 && mm == 4) {     te3n[m][mm]= 0.377174; } 
	  if(m==60 && mm == 5) {     te3n[m][mm]= 1.052759; } 
	  if(m==60 && mm == 6) {     te3n[m][mm]= 0.183226; } 
	  if(m==60 && mm == 19) {     te3n[m][mm]= 0.307329; } 
	  if(m==60 && mm == 20) {     te3n[m][mm]= 0.125407; } 
	  if(m==60 && mm == 22) {     te3n[m][mm]= 0.405307; } 
	  if(m==60 && mm == 23) {     te3n[m][mm]= 0.302138; } 
	  if(m==60 && mm == 24) {     te3n[m][mm]= 0.491119; } 
	  if(m==60 && mm == 25) {     te3n[m][mm]= 1.048645; } 
	  if(m==60 && mm == 28) {     te3n[m][mm]= 0.260338; } 
	  if(m==60 && mm == 30) {     te3n[m][mm]= 0.113103; } 
	  if(m==60 && mm == 31) {     te3n[m][mm]= 0.511034; } 
	  if(m==60 && mm == 33) {     te3n[m][mm]= 0.209465; } 
	  if(m==60 && mm == 34) {     te3n[m][mm]= 0.531097; } 
	  if(m==60 && mm == 35) {     te3n[m][mm]= 1.171188; } 
	  if(m==61 && mm == 6) {     tw0p[m][mm]= 0.272168; } 
	  if(m==61 && mm == 7) {     tw0p[m][mm]= 1.014124; } 
	  if(m==61 && mm == 8) {     tw0p[m][mm]= 0.201465; } 
	  if(m==61 && mm == 9) {     tw0p[m][mm]= 0.140742; } 
	  if(m==61 && mm == 11) {     tw0p[m][mm]= 0.463336; } 
	  if(m==61 && mm == 16) {     tw0p[m][mm]= 0.104136; } 
	  if(m==61 && mm == 18) {     tw0p[m][mm]= 0.530851; } 
	  if(m==61 && mm == 19) {     tw0p[m][mm]= 0.071382; } 
	  if(m==61 && mm == 21) {     tw0p[m][mm]= 0.292285; } 
	  if(m==61 && mm == 22) {     tw0p[m][mm]= 1.491275; } 
	  if(m==61 && mm == 23) {     tw0p[m][mm]= 0.332181; } 
	  if(m==61 && mm == 26) {     tw0p[m][mm]= 0.070917; } 
	  if(m==61 && mm == 27) {     tw0p[m][mm]= 0.192259; } 
	  if(m==61 && mm == 29) {     tw0p[m][mm]= 0.085866; } 
	  if(m==61 && mm == 33) {     tw0p[m][mm]= 0.240570; } 
	  if(m==61 && mm == 0) {     tw1p[m][mm]= 0.257763; } 
	  if(m==61 && mm == 1) {     tw1p[m][mm]= 0.330010; } 
	  if(m==61 && mm == 2) {     tw1p[m][mm]= 0.074555; } 
	  if(m==61 && mm == 7) {     tw1p[m][mm]= 0.057935; } 
	  if(m==61 && mm == 21) {     tw1p[m][mm]= 0.731513; } 
	  if(m==61 && mm == 22) {     tw1p[m][mm]= 0.067152; } 
	  if(m==61 && mm == 23) {     tw1p[m][mm]= 0.041270; } 
	  if(m==61 && mm == 25) {     tw1p[m][mm]= 0.221478; } 
	  if(m==61 && mm == 26) {     tw1p[m][mm]= 0.332340; } 
	  if(m==61 && mm == 29) {     tw1p[m][mm]= 0.165995; } 
	  if(m==61 && mm == 34) {     tw1p[m][mm]= 0.356190; } 
	  if(m==61 && mm == 12) {     tw2p[m][mm]= 0.064095; } 
	  if(m==61 && mm == 0) {     tw3p[m][mm]= 0.382192; } 
	  if(m==61 && mm == 1) {     tw3p[m][mm]= 0.665001; } 
	  if(m==61 && mm == 2) {     tw3p[m][mm]= 0.510337; } 
	  if(m==61 && mm == 3) {     tw3p[m][mm]= 0.547126; } 
	  if(m==61 && mm == 5) {     tw3p[m][mm]= 0.503672; } 
	  if(m==61 && mm == 6) {     tw3p[m][mm]= 0.365526; } 
	  if(m==61 && mm == 7) {     tw3p[m][mm]= 0.052730; } 
	  if(m==61 && mm == 8) {     tw3p[m][mm]= 0.767887; } 
	  if(m==61 && mm == 9) {     tw3p[m][mm]= 0.759721; } 
	  if(m==61 && mm == 10) {     tw3p[m][mm]= 0.590833; } 
	  if(m==61 && mm == 30) {     tw3p[m][mm]= 0.251679; } 
	  if(m==61 && mm == 31) {     tw3p[m][mm]= 0.063746; } 
	  if(m==61 && mm == 32) {     tw3p[m][mm]= 1.028506; } 
	  if(m==61 && mm == 33) {     tw3p[m][mm]= 0.961229; } 
	  if(m==61 && mm == 34) {     tw3p[m][mm]= 1.333509; } 
	  if(m==61 && mm == 35) {     tw3p[m][mm]= 0.838064; } 
	  if(m==61 && mm == 0) {     te2p[m][mm]= 0.360784; } 
	  if(m==61 && mm == 2) {     te2p[m][mm]= 0.049522; } 
	  if(m==61 && mm == 4) {     te2p[m][mm]= 0.312786; } 
	  if(m==61 && mm == 7) {     te2p[m][mm]= 0.291084; } 
	  if(m==61 && mm == 8) {     te2p[m][mm]= 0.373838; } 
	  if(m==61 && mm == 9) {     te2p[m][mm]= 0.220361; } 
	  if(m==61 && mm == 10) {     te2p[m][mm]= 0.033535; } 
	  if(m==61 && mm == 11) {     te2p[m][mm]= 0.330366; } 
	  if(m==61 && mm == 12) {     te2p[m][mm]= 1.235156; } 
	  if(m==61 && mm == 13) {     te2p[m][mm]= 0.277490; } 
	  if(m==61 && mm == 14) {     te2p[m][mm]= 0.480087; } 
	  if(m==61 && mm == 15) {     te2p[m][mm]= 0.661729; } 
	  if(m==61 && mm == 17) {     te2p[m][mm]= 0.366722; } 
	  if(m==61 && mm == 18) {     te2p[m][mm]= 0.217868; } 
	  if(m==61 && mm == 19) {     te2p[m][mm]= 0.866454; } 
	  if(m==61 && mm == 20) {     te2p[m][mm]= 0.053420; } 
	  if(m==61 && mm == 21) {     te2p[m][mm]= 0.316703; } 
	  if(m==61 && mm == 22) {     te2p[m][mm]= 0.078713; } 
	  if(m==61 && mm == 23) {     te2p[m][mm]= 0.699400; } 
	  if(m==61 && mm == 24) {     te2p[m][mm]= 0.071145; } 
	  if(m==61 && mm == 25) {     te2p[m][mm]= 0.405792; } 
	  if(m==61 && mm == 29) {     te2p[m][mm]= 0.358559; } 
	  if(m==61 && mm == 30) {     te2p[m][mm]= 1.144649; } 
	  if(m==61 && mm == 31) {     te2p[m][mm]= 0.933677; } 
	  if(m==61 && mm == 6) {     te3p[m][mm]= 1.101382; } 
	  if(m==61 && mm == 7) {     te3p[m][mm]= 0.599283; } 
	  if(m==61 && mm == 8) {     te3p[m][mm]= 1.124635; } 
	  if(m==61 && mm == 9) {     te3p[m][mm]= 0.121507; } 
	  if(m==61 && mm == 10) {     te3p[m][mm]= 0.454538; } 
	  if(m==61 && mm == 11) {     te3p[m][mm]= 0.651645; } 
	  if(m==61 && mm == 12) {     te3p[m][mm]= 1.433344; } 
	  if(m==61 && mm == 13) {     te3p[m][mm]= 0.549119; } 
	  if(m==61 && mm == 18) {     te3p[m][mm]= 0.077149; } 
	  if(m==61 && mm == 19) {     te3p[m][mm]= 0.127052; } 
	  if(m==61 && mm == 20) {     te3p[m][mm]= 0.178361; } 
	  if(m==61 && mm == 21) {     te3p[m][mm]= 0.033148; } 
	  if(m==61 && mm == 0) {     tw0n[m][mm]= 0.069667; } 
	  if(m==61 && mm == 1) {     tw0n[m][mm]= 0.808671; } 
	  if(m==61 && mm == 2) {     tw0n[m][mm]= 1.103441; } 
	  if(m==61 && mm == 3) {     tw0n[m][mm]= 0.636768; } 
	  if(m==61 && mm == 4) {     tw0n[m][mm]= 0.463325; } 
	  if(m==61 && mm == 5) {     tw0n[m][mm]= 0.606843; } 
	  if(m==61 && mm == 6) {     tw0n[m][mm]= 0.531768; } 
	  if(m==61 && mm == 7) {     tw0n[m][mm]= 0.949863; } 
	  if(m==61 && mm == 8) {     tw0n[m][mm]= 0.534650; } 
	  if(m==61 && mm == 9) {     tw0n[m][mm]= 0.385240; } 
	  if(m==61 && mm == 10) {     tw0n[m][mm]= 0.000388; } 
	  if(m==61 && mm == 11) {     tw0n[m][mm]= 0.916835; } 
	  if(m==61 && mm == 15) {     tw0n[m][mm]= 0.245385; } 
	  if(m==61 && mm == 16) {     tw0n[m][mm]= 0.202877; } 
	  if(m==61 && mm == 18) {     tw0n[m][mm]= 0.398835; } 
	  if(m==61 && mm == 19) {     tw0n[m][mm]= 0.016459; } 
	  if(m==61 && mm == 21) {     tw0n[m][mm]= 0.475679; } 
	  if(m==61 && mm == 22) {     tw0n[m][mm]= 1.477934; } 
	  if(m==61 && mm == 23) {     tw0n[m][mm]= 0.148888; } 
	  if(m==61 && mm == 26) {     tw0n[m][mm]= 0.031607; } 
	  if(m==61 && mm == 7) {     tw1n[m][mm]= 0.092569; } 
	  if(m==61 && mm == 9) {     tw1n[m][mm]= 0.137600; } 
	  if(m==61 && mm == 16) {     tw1n[m][mm]= 0.092491; } 
	  if(m==61 && mm == 21) {     tw1n[m][mm]= 0.656819; } 
	  if(m==61 && mm == 34) {     tw1n[m][mm]= 0.262508; } 
	  if(m==61 && mm == 35) {     tw1n[m][mm]= 0.096905; } 
	  if(m==61 && mm == 24) {     tw2n[m][mm]= 0.253901; } 
	  if(m==61 && mm == 26) {     tw2n[m][mm]= 0.263560; } 
	  if(m==61 && mm == 27) {     tw2n[m][mm]= 0.186729; } 
	  if(m==61 && mm == 31) {     tw2n[m][mm]= 0.485427; } 
	  if(m==61 && mm == 0) {     tw3n[m][mm]= 0.472628; } 
	  if(m==61 && mm == 1) {     tw3n[m][mm]= 0.455715; } 
	  if(m==61 && mm == 3) {     tw3n[m][mm]= 0.455251; } 
	  if(m==61 && mm == 4) {     tw3n[m][mm]= 0.926447; } 
	  if(m==61 && mm == 5) {     tw3n[m][mm]= 0.601591; } 
	  if(m==61 && mm == 6) {     tw3n[m][mm]= 0.394199; } 
	  if(m==61 && mm == 7) {     tw3n[m][mm]= 0.108932; } 
	  if(m==61 && mm == 8) {     tw3n[m][mm]= 0.540969; } 
	  if(m==61 && mm == 9) {     tw3n[m][mm]= 0.544301; } 
	  if(m==61 && mm == 10) {     tw3n[m][mm]= 0.648969; } 
	  if(m==61 && mm == 7) {     te2n[m][mm]= 0.078542; } 
	  if(m==61 && mm == 8) {     te2n[m][mm]= 0.141130; } 
	  if(m==61 && mm == 9) {     te2n[m][mm]= 0.117883; } 
	  if(m==61 && mm == 10) {     te2n[m][mm]= 0.001751; } 
	  if(m==61 && mm == 11) {     te2n[m][mm]= 0.241769; } 
	  if(m==61 && mm == 12) {     te2n[m][mm]= 0.930080; } 
	  if(m==61 && mm == 13) {     te2n[m][mm]= 0.171449; } 
	  if(m==61 && mm == 14) {     te2n[m][mm]= 0.372106; } 
	  if(m==61 && mm == 15) {     te2n[m][mm]= 0.531586; } 
	  if(m==61 && mm == 17) {     te2n[m][mm]= 0.219605; } 
	  if(m==61 && mm == 18) {     te2n[m][mm]= 2.716817; } 
	  if(m==61 && mm == 19) {     te2n[m][mm]= 0.803923; } 
	  if(m==61 && mm == 20) {     te2n[m][mm]= 0.134071; } 
	  if(m==61 && mm == 21) {     te2n[m][mm]= 0.661258; } 
	  if(m==61 && mm == 22) {     te2n[m][mm]= 0.183634; } 
	  if(m==61 && mm == 23) {     te2n[m][mm]= 1.185548; } 
	  if(m==61 && mm == 24) {     te2n[m][mm]= 0.244195; } 
	  if(m==61 && mm == 25) {     te2n[m][mm]= 0.249775; } 
	  if(m==61 && mm == 29) {     te2n[m][mm]= 0.730853; } 
	  if(m==61 && mm == 30) {     te2n[m][mm]= 0.392379; } 
	  if(m==61 && mm == 31) {     te2n[m][mm]= 0.066934; } 
	  if(m==61 && mm == 32) {     te2n[m][mm]= 0.515566; } 
	  if(m==61 && mm == 34) {     te2n[m][mm]= 1.465756; } 
	  if(m==61 && mm == 35) {     te2n[m][mm]= 0.102705; } 
	  if(m==61 && mm == 2) {     te3n[m][mm]= 0.198965; } 
	  if(m==61 && mm == 4) {     te3n[m][mm]= 2.542352; } 
	  if(m==61 && mm == 5) {     te3n[m][mm]= 1.345690; } 
	  if(m==61 && mm == 6) {     te3n[m][mm]= 0.793652; } 
	  if(m==61 && mm == 18) {     te3n[m][mm]= 0.245983; } 
	  if(m==61 && mm == 19) {     te3n[m][mm]= 0.215682; } 
	  if(m==61 && mm == 20) {     te3n[m][mm]= 0.779371; } 
	  if(m==61 && mm == 21) {     te3n[m][mm]= 0.538913; } 
	  if(m==61 && mm == 24) {     te3n[m][mm]= 0.366639; } 
	  if(m==61 && mm == 25) {     te3n[m][mm]= 1.077936; } 
	  if(m==61 && mm == 26) {     te3n[m][mm]= 0.010873; } 
	  if(m==61 && mm == 27) {     te3n[m][mm]= 0.182990; } 
	  if(m==61 && mm == 30) {     te3n[m][mm]= 0.194304; } 
	  if(m==61 && mm == 32) {     te3n[m][mm]= 0.380218; } 
	  if(m==61 && mm == 34) {     te3n[m][mm]= 0.594377; } 
	  if(m==62 && mm == 8) {     tw0p[m][mm]= 0.116830; } 
	  if(m==62 && mm == 9) {     tw0p[m][mm]= 0.078194; } 
	  if(m==62 && mm == 10) {     tw0p[m][mm]= 0.458569; } 
	  if(m==62 && mm == 11) {     tw0p[m][mm]= 1.089768; } 
	  if(m==62 && mm == 14) {     tw0p[m][mm]= 0.049183; } 
	  if(m==62 && mm == 16) {     tw0p[m][mm]= 0.339150; } 
	  if(m==62 && mm == 17) {     tw0p[m][mm]= 0.032017; } 
	  if(m==62 && mm == 18) {     tw0p[m][mm]= 0.133842; } 
	  if(m==62 && mm == 19) {     tw0p[m][mm]= 0.418068; } 
	  if(m==62 && mm == 20) {     tw0p[m][mm]= 0.112647; } 
	  if(m==62 && mm == 21) {     tw0p[m][mm]= 2.321105; } 
	  if(m==62 && mm == 22) {     tw0p[m][mm]= 0.498304; } 
	  if(m==62 && mm == 23) {     tw0p[m][mm]= 0.110393; } 
	  if(m==62 && mm == 25) {     tw0p[m][mm]= 0.327709; } 
	  if(m==62 && mm == 26) {     tw0p[m][mm]= 0.055795; } 
	  if(m==62 && mm == 29) {     tw0p[m][mm]= 0.640782; } 
	  if(m==62 && mm == 32) {     tw0p[m][mm]= 0.218970; } 
	  if(m==62 && mm == 34) {     tw0p[m][mm]= 0.640366; } 
	  if(m==62 && mm == 0) {     tw1p[m][mm]= 0.255223; } 
	  if(m==62 && mm == 5) {     tw1p[m][mm]= 0.150365; } 
	  if(m==62 && mm == 19) {     tw1p[m][mm]= 0.027271; } 
	  if(m==62 && mm == 21) {     tw1p[m][mm]= 2.551885; } 
	  if(m==62 && mm == 22) {     tw1p[m][mm]= 0.417398; } 
	  if(m==62 && mm == 23) {     tw1p[m][mm]= 0.261271; } 
	  if(m==62 && mm == 26) {     tw1p[m][mm]= 0.117817; } 
	  if(m==62 && mm == 30) {     tw1p[m][mm]= 0.785230; } 
	  if(m==62 && mm == 31) {     tw1p[m][mm]= 0.220183; } 
	  if(m==62 && mm == 32) {     tw1p[m][mm]= 0.476384; } 
	  if(m==62 && mm == 34) {     tw1p[m][mm]= 0.217702; } 
	  if(m==62 && mm == 35) {     tw1p[m][mm]= 0.906917; } 
	  if(m==62 && mm == 6) {     tw2p[m][mm]= 0.677954; } 
	  if(m==62 && mm == 7) {     tw2p[m][mm]= 0.947331; } 
	  if(m==62 && mm == 8) {     tw2p[m][mm]= 0.556664; } 
	  if(m==62 && mm == 9) {     tw2p[m][mm]= 0.325765; } 
	  if(m==62 && mm == 10) {     tw2p[m][mm]= 0.960089; } 
	  if(m==62 && mm == 11) {     tw2p[m][mm]= 0.411949; } 
	  if(m==62 && mm == 12) {     tw2p[m][mm]= 0.456025; } 
	  if(m==62 && mm == 14) {     tw2p[m][mm]= 0.101531; } 
	  if(m==62 && mm == 15) {     tw2p[m][mm]= 0.143644; } 
	  if(m==62 && mm == 16) {     tw2p[m][mm]= 0.156532; } 
	  if(m==62 && mm == 17) {     tw2p[m][mm]= 0.018843; } 
	  if(m==62 && mm == 18) {     tw2p[m][mm]= 0.102566; } 
	  if(m==62 && mm == 19) {     tw2p[m][mm]= 0.014100; } 
	  if(m==62 && mm == 20) {     tw2p[m][mm]= 0.138260; } 
	  if(m==62 && mm == 22) {     tw2p[m][mm]= 0.164691; } 
	  if(m==62 && mm == 23) {     tw2p[m][mm]= 0.264986; } 
	  if(m==62 && mm == 30) {     tw2p[m][mm]= 0.553175; } 
	  if(m==62 && mm == 32) {     tw2p[m][mm]= 1.368287; } 
	  if(m==62 && mm == 33) {     tw2p[m][mm]= 1.086208; } 
	  if(m==62 && mm == 34) {     tw2p[m][mm]= 1.899084; } 
	  if(m==62 && mm == 35) {     tw2p[m][mm]= 0.685449; } 
	  if(m==62 && mm == 2) {     tw3p[m][mm]= 0.777102; } 
	  if(m==62 && mm == 3) {     tw3p[m][mm]= 0.477090; } 
	  if(m==62 && mm == 4) {     tw3p[m][mm]= 0.749342; } 
	  if(m==62 && mm == 5) {     tw3p[m][mm]= 0.772860; } 
	  if(m==62 && mm == 6) {     tw3p[m][mm]= 0.896263; } 
	  if(m==62 && mm == 8) {     tw3p[m][mm]= 0.993311; } 
	  if(m==62 && mm == 9) {     tw3p[m][mm]= 0.577643; } 
	  if(m==62 && mm == 11) {     tw3p[m][mm]= 1.904385; } 
	  if(m==62 && mm == 14) {     tw3p[m][mm]= 0.491946; } 
	  if(m==62 && mm == 15) {     tw3p[m][mm]= 0.512088; } 
	  if(m==62 && mm == 18) {     tw3p[m][mm]= 0.391036; } 
	  if(m==62 && mm == 19) {     tw3p[m][mm]= 1.083715; } 
	  if(m==62 && mm == 20) {     tw3p[m][mm]= 0.642528; } 
	  if(m==62 && mm == 28) {     tw3p[m][mm]= 0.128173; } 
	  if(m==62 && mm == 30) {     tw3p[m][mm]= 0.990913; } 
	  if(m==62 && mm == 31) {     tw3p[m][mm]= 0.730929; } 
	  if(m==62 && mm == 32) {     tw3p[m][mm]= 0.319812; } 
	  if(m==62 && mm == 33) {     tw3p[m][mm]= 0.659097; } 
	  if(m==62 && mm == 34) {     tw3p[m][mm]= 0.230842; } 
	  if(m==62 && mm == 35) {     tw3p[m][mm]= 0.436787; } 
	  if(m==62 && mm == 0) {     te2p[m][mm]= 0.358645; } 
	  if(m==62 && mm == 2) {     te2p[m][mm]= 0.600237; } 
	  if(m==62 && mm == 3) {     te2p[m][mm]= 1.031921; } 
	  if(m==62 && mm == 4) {     te2p[m][mm]= 0.318297; } 
	  if(m==62 && mm == 5) {     te2p[m][mm]= 1.364059; } 
	  if(m==62 && mm == 6) {     te2p[m][mm]= 0.466979; } 
	  if(m==62 && mm == 7) {     te2p[m][mm]= 0.432246; } 
	  if(m==62 && mm == 8) {     te2p[m][mm]= 0.165672; } 
	  if(m==62 && mm == 9) {     te2p[m][mm]= 0.084341; } 
	  if(m==62 && mm == 10) {     te2p[m][mm]= 0.434908; } 
	  if(m==62 && mm == 11) {     te2p[m][mm]= 0.528603; } 
	  if(m==62 && mm == 12) {     te2p[m][mm]= 0.344437; } 
	  if(m==62 && mm == 13) {     te2p[m][mm]= 0.240277; } 
	  if(m==62 && mm == 15) {     te2p[m][mm]= 0.283370; } 
	  if(m==62 && mm == 16) {     te2p[m][mm]= 2.369179; } 
	  if(m==62 && mm == 17) {     te2p[m][mm]= 0.500858; } 
	  if(m==62 && mm == 18) {     te2p[m][mm]= 0.959590; } 
	  if(m==62 && mm == 20) {     te2p[m][mm]= 1.447513; } 
	  if(m==62 && mm == 22) {     te2p[m][mm]= 0.559285; } 
	  if(m==62 && mm == 23) {     te2p[m][mm]= 0.448272; } 
	  if(m==62 && mm == 24) {     te2p[m][mm]= 0.274145; } 
	  if(m==62 && mm == 28) {     te2p[m][mm]= 0.489895; } 
	  if(m==62 && mm == 30) {     te2p[m][mm]= 0.806563; } 
	  if(m==62 && mm == 6) {     te3p[m][mm]= 0.401477; } 
	  if(m==62 && mm == 7) {     te3p[m][mm]= 1.681236; } 
	  if(m==62 && mm == 8) {     te3p[m][mm]= 0.405800; } 
	  if(m==62 && mm == 9) {     te3p[m][mm]= 0.714017; } 
	  if(m==62 && mm == 10) {     te3p[m][mm]= 0.412261; } 
	  if(m==62 && mm == 11) {     te3p[m][mm]= 0.911490; } 
	  if(m==62 && mm == 12) {     te3p[m][mm]= 1.927236; } 
	  if(m==62 && mm == 13) {     te3p[m][mm]= 1.415114; } 
	  if(m==62 && mm == 21) {     te3p[m][mm]= 0.449772; } 
	  if(m==62 && mm == 22) {     te3p[m][mm]= 0.248540; } 
	  if(m==62 && mm == 0) {     tw0n[m][mm]= 1.225681; } 
	  if(m==62 && mm == 1) {     tw0n[m][mm]= 0.467398; } 
	  if(m==62 && mm == 2) {     tw0n[m][mm]= 0.772808; } 
	  if(m==62 && mm == 3) {     tw0n[m][mm]= 2.370183; } 
	  if(m==62 && mm == 4) {     tw0n[m][mm]= 0.468708; } 
	  if(m==62 && mm == 6) {     tw0n[m][mm]= 0.648999; } 
	  if(m==62 && mm == 8) {     tw0n[m][mm]= 0.710897; } 
	  if(m==62 && mm == 9) {     tw0n[m][mm]= 0.197071; } 
	  if(m==62 && mm == 10) {     tw0n[m][mm]= 0.613931; } 
	  if(m==62 && mm == 14) {     tw0n[m][mm]= 0.135860; } 
	  if(m==62 && mm == 16) {     tw0n[m][mm]= 0.294914; } 
	  if(m==62 && mm == 17) {     tw0n[m][mm]= 0.046963; } 
	  if(m==62 && mm == 18) {     tw0n[m][mm]= 0.233133; } 
	  if(m==62 && mm == 19) {     tw0n[m][mm]= 0.523981; } 
	  if(m==62 && mm == 20) {     tw0n[m][mm]= 0.133227; } 
	  if(m==62 && mm == 21) {     tw0n[m][mm]= 1.174668; } 
	  if(m==62 && mm == 22) {     tw0n[m][mm]= 0.469880; } 
	  if(m==62 && mm == 25) {     tw0n[m][mm]= 0.317460; } 
	  if(m==62 && mm == 29) {     tw0n[m][mm]= 0.400985; } 
	  if(m==62 && mm == 11) {     tw1n[m][mm]= 0.448640; } 
	  if(m==62 && mm == 13) {     tw1n[m][mm]= 0.307055; } 
	  if(m==62 && mm == 15) {     tw1n[m][mm]= 0.273563; } 
	  if(m==62 && mm == 19) {     tw1n[m][mm]= 0.009568; } 
	  if(m==62 && mm == 20) {     tw1n[m][mm]= 0.000302; } 
	  if(m==62 && mm == 22) {     tw1n[m][mm]= 0.222529; } 
	  if(m==62 && mm == 23) {     tw1n[m][mm]= 0.207458; } 
	  if(m==62 && mm == 30) {     tw1n[m][mm]= 0.472957; } 
	  if(m==62 && mm == 31) {     tw1n[m][mm]= 0.436975; } 
	  if(m==62 && mm == 32) {     tw1n[m][mm]= 0.225235; } 
	  if(m==62 && mm == 34) {     tw1n[m][mm]= 0.012949; } 
	  if(m==62 && mm == 35) {     tw1n[m][mm]= 0.838492; } 
	  if(m==62 && mm == 0) {     tw2n[m][mm]= 0.822408; } 
	  if(m==62 && mm == 1) {     tw2n[m][mm]= 0.756581; } 
	  if(m==62 && mm == 6) {     tw2n[m][mm]= 0.464806; } 
	  if(m==62 && mm == 14) {     tw2n[m][mm]= 0.068126; } 
	  if(m==62 && mm == 15) {     tw2n[m][mm]= 0.226830; } 
	  if(m==62 && mm == 16) {     tw2n[m][mm]= 0.108736; } 
	  if(m==62 && mm == 17) {     tw2n[m][mm]= 0.015097; } 
	  if(m==62 && mm == 20) {     tw2n[m][mm]= 0.142122; } 
	  if(m==62 && mm == 22) {     tw2n[m][mm]= 0.036971; } 
	  if(m==62 && mm == 23) {     tw2n[m][mm]= 0.152772; } 
	  if(m==62 && mm == 24) {     tw2n[m][mm]= 0.196178; } 
	  if(m==62 && mm == 25) {     tw2n[m][mm]= 0.035174; } 
	  if(m==62 && mm == 26) {     tw2n[m][mm]= 0.600950; } 
	  if(m==62 && mm == 27) {     tw2n[m][mm]= 0.121439; } 
	  if(m==62 && mm == 28) {     tw2n[m][mm]= 0.381823; } 
	  if(m==62 && mm == 29) {     tw2n[m][mm]= 0.518749; } 
	  if(m==62 && mm == 30) {     tw2n[m][mm]= 0.337278; } 
	  if(m==62 && mm == 32) {     tw2n[m][mm]= 0.805388; } 
	  if(m==62 && mm == 33) {     tw2n[m][mm]= 0.876483; } 
	  if(m==62 && mm == 2) {     tw3n[m][mm]= 1.161426; } 
	  if(m==62 && mm == 3) {     tw3n[m][mm]= 0.629404; } 
	  if(m==62 && mm == 4) {     tw3n[m][mm]= 0.965983; } 
	  if(m==62 && mm == 5) {     tw3n[m][mm]= 0.793324; } 
	  if(m==62 && mm == 6) {     tw3n[m][mm]= 0.383944; } 
	  if(m==62 && mm == 8) {     tw3n[m][mm]= 1.522665; } 
	  if(m==62 && mm == 9) {     tw3n[m][mm]= 0.536890; } 
	  if(m==62 && mm == 11) {     tw3n[m][mm]= 2.309807; } 
	  if(m==62 && mm == 14) {     tw3n[m][mm]= 0.434131; } 
	  if(m==62 && mm == 18) {     tw3n[m][mm]= 0.192227; } 
	  if(m==62 && mm == 19) {     tw3n[m][mm]= 0.127177; } 
	  if(m==62 && mm == 20) {     tw3n[m][mm]= 0.662892; } 
	  if(m==62 && mm == 5) {     te2n[m][mm]= 0.982336; } 
	  if(m==62 && mm == 6) {     te2n[m][mm]= 0.340419; } 
	  if(m==62 && mm == 7) {     te2n[m][mm]= 0.226240; } 
	  if(m==62 && mm == 8) {     te2n[m][mm]= 0.020623; } 
	  if(m==62 && mm == 10) {     te2n[m][mm]= 0.453201; } 
	  if(m==62 && mm == 11) {     te2n[m][mm]= 0.415861; } 
	  if(m==62 && mm == 12) {     te2n[m][mm]= 0.044399; } 
	  if(m==62 && mm == 15) {     te2n[m][mm]= 0.111340; } 
	  if(m==62 && mm == 16) {     te2n[m][mm]= 1.028770; } 
	  if(m==62 && mm == 17) {     te2n[m][mm]= 0.273881; } 
	  if(m==62 && mm == 18) {     te2n[m][mm]= 1.417013; } 
	  if(m==62 && mm == 19) {     te2n[m][mm]= 2.017188; } 
	  if(m==62 && mm == 20) {     te2n[m][mm]= 1.290732; } 
	  if(m==62 && mm == 21) {     te2n[m][mm]= 0.012002; } 
	  if(m==62 && mm == 22) {     te2n[m][mm]= 0.401139; } 
	  if(m==62 && mm == 23) {     te2n[m][mm]= 0.299516; } 
	  if(m==62 && mm == 24) {     te2n[m][mm]= 0.189878; } 
	  if(m==62 && mm == 28) {     te2n[m][mm]= 1.118332; } 
	  if(m==62 && mm == 29) {     te2n[m][mm]= 0.252222; } 
	  if(m==62 && mm == 30) {     te2n[m][mm]= 1.422365; } 
	  if(m==62 && mm == 32) {     te2n[m][mm]= 0.271259; } 
	  if(m==62 && mm == 34) {     te2n[m][mm]= 1.207992; } 
	  if(m==62 && mm == 0) {     te3n[m][mm]= 0.725330; } 
	  if(m==62 && mm == 1) {     te3n[m][mm]= 0.770352; } 
	  if(m==62 && mm == 2) {     te3n[m][mm]= 1.506682; } 
	  if(m==62 && mm == 3) {     te3n[m][mm]= 1.406813; } 
	  if(m==62 && mm == 4) {     te3n[m][mm]= 1.142282; } 
	  if(m==62 && mm == 6) {     te3n[m][mm]= 0.258990; } 
	  if(m==62 && mm == 21) {     te3n[m][mm]= 0.356558; } 
	  if(m==62 && mm == 22) {     te3n[m][mm]= 0.065146; } 
	  if(m==62 && mm == 24) {     te3n[m][mm]= 0.107374; } 
	  if(m==62 && mm == 25) {     te3n[m][mm]= 0.072937; } 
	  if(m==62 && mm == 26) {     te3n[m][mm]= 0.737109; } 
	  if(m==62 && mm == 28) {     te3n[m][mm]= 0.081781; } 
	  if(m==62 && mm == 29) {     te3n[m][mm]= 0.040259; } 
	  if(m==62 && mm == 30) {     te3n[m][mm]= 0.880702; } 
	  if(m==62 && mm == 31) {     te3n[m][mm]= 0.216317; } 
	  if(m==62 && mm == 32) {     te3n[m][mm]= 0.571103; } 
	  if(m==62 && mm == 33) {     te3n[m][mm]= 0.771778; } 
	  if(m==62 && mm == 34) {     te3n[m][mm]= 0.649661; } 
	  if(m==63 && mm == 7) {     tw0p[m][mm]= 0.260295; } 
	  if(m==63 && mm == 8) {     tw0p[m][mm]= 0.657323; } 
	  if(m==63 && mm == 9) {     tw0p[m][mm]= 0.396666; } 
	  if(m==63 && mm == 10) {     tw0p[m][mm]= 0.518843; } 
	  if(m==63 && mm == 11) {     tw0p[m][mm]= 0.453350; } 
	  if(m==63 && mm == 12) {     tw0p[m][mm]= 0.064244; } 
	  if(m==63 && mm == 14) {     tw0p[m][mm]= 0.401042; } 
	  if(m==63 && mm == 15) {     tw0p[m][mm]= 0.137008; } 
	  if(m==63 && mm == 19) {     tw0p[m][mm]= 0.130756; } 
	  if(m==63 && mm == 20) {     tw0p[m][mm]= 0.053874; } 
	  if(m==63 && mm == 21) {     tw0p[m][mm]= 0.154317; } 
	  if(m==63 && mm == 22) {     tw0p[m][mm]= 0.171547; } 
	  if(m==63 && mm == 24) {     tw0p[m][mm]= 0.477966; } 
	  if(m==63 && mm == 25) {     tw0p[m][mm]= 0.327966; } 
	  if(m==63 && mm == 27) {     tw0p[m][mm]= 0.171018; } 
	  if(m==63 && mm == 28) {     tw0p[m][mm]= 0.170569; } 
	  if(m==63 && mm == 29) {     tw0p[m][mm]= 0.384692; } 
	  if(m==63 && mm == 32) {     tw0p[m][mm]= 0.397561; } 
	  if(m==63 && mm == 33) {     tw0p[m][mm]= 0.173470; } 
	  if(m==63 && mm == 34) {     tw0p[m][mm]= 0.225341; } 
	  if(m==63 && mm == 35) {     tw0p[m][mm]= 0.684371; } 
	  if(m==63 && mm == 6) {     tw1p[m][mm]= 0.318577; } 
	  if(m==63 && mm == 8) {     tw1p[m][mm]= 0.051863; } 
	  if(m==63 && mm == 19) {     tw1p[m][mm]= 0.402452; } 
	  if(m==63 && mm == 20) {     tw1p[m][mm]= 0.087002; } 
	  if(m==63 && mm == 21) {     tw1p[m][mm]= 0.296899; } 
	  if(m==63 && mm == 22) {     tw1p[m][mm]= 0.590536; } 
	  if(m==63 && mm == 23) {     tw1p[m][mm]= 0.184321; } 
	  if(m==63 && mm == 26) {     tw1p[m][mm]= 0.894343; } 
	  if(m==63 && mm == 30) {     tw1p[m][mm]= 0.401158; } 
	  if(m==63 && mm == 34) {     tw1p[m][mm]= 0.387440; } 
	  if(m==63 && mm == 6) {     tw2p[m][mm]= 0.345699; } 
	  if(m==63 && mm == 7) {     tw2p[m][mm]= 0.475828; } 
	  if(m==63 && mm == 8) {     tw2p[m][mm]= 0.367260; } 
	  if(m==63 && mm == 9) {     tw2p[m][mm]= 0.658233; } 
	  if(m==63 && mm == 10) {     tw2p[m][mm]= 0.184835; } 
	  if(m==63 && mm == 11) {     tw2p[m][mm]= 0.126901; } 
	  if(m==63 && mm == 13) {     tw2p[m][mm]= 0.014155; } 
	  if(m==63 && mm == 14) {     tw2p[m][mm]= 0.288535; } 
	  if(m==63 && mm == 16) {     tw2p[m][mm]= 0.441495; } 
	  if(m==63 && mm == 17) {     tw2p[m][mm]= 0.153794; } 
	  if(m==63 && mm == 19) {     tw2p[m][mm]= 0.043298; } 
	  if(m==63 && mm == 21) {     tw2p[m][mm]= 0.091737; } 
	  if(m==63 && mm == 22) {     tw2p[m][mm]= 0.237879; } 
	  if(m==63 && mm == 23) {     tw2p[m][mm]= 0.380244; } 
	  if(m==63 && mm == 29) {     tw2p[m][mm]= 0.415065; } 
	  if(m==63 && mm == 30) {     tw2p[m][mm]= 0.039744; } 
	  if(m==63 && mm == 31) {     tw2p[m][mm]= 0.056605; } 
	  if(m==63 && mm == 32) {     tw2p[m][mm]= 2.492761; } 
	  if(m==63 && mm == 34) {     tw2p[m][mm]= 0.732564; } 
	  if(m==63 && mm == 2) {     tw3p[m][mm]= 0.477750; } 
	  if(m==63 && mm == 3) {     tw3p[m][mm]= 0.655737; } 
	  if(m==63 && mm == 4) {     tw3p[m][mm]= 0.493390; } 
	  if(m==63 && mm == 5) {     tw3p[m][mm]= 0.675028; } 
	  if(m==63 && mm == 6) {     tw3p[m][mm]= 0.908763; } 
	  if(m==63 && mm == 7) {     tw3p[m][mm]= 0.327226; } 
	  if(m==63 && mm == 8) {     tw3p[m][mm]= 0.418289; } 
	  if(m==63 && mm == 9) {     tw3p[m][mm]= 0.802070; } 
	  if(m==63 && mm == 12) {     tw3p[m][mm]= 0.284605; } 
	  if(m==63 && mm == 18) {     tw3p[m][mm]= 0.225744; } 
	  if(m==63 && mm == 19) {     tw3p[m][mm]= 0.657221; } 
	  if(m==63 && mm == 20) {     tw3p[m][mm]= 0.588472; } 
	  if(m==63 && mm == 28) {     tw3p[m][mm]= 0.337664; } 
	  if(m==63 && mm == 29) {     tw3p[m][mm]= 0.477328; } 
	  if(m==63 && mm == 30) {     tw3p[m][mm]= 1.206752; } 
	  if(m==63 && mm == 31) {     tw3p[m][mm]= 0.091927; } 
	  if(m==63 && mm == 32) {     tw3p[m][mm]= 0.523830; } 
	  if(m==63 && mm == 33) {     tw3p[m][mm]= 0.595532; } 
	  if(m==63 && mm == 34) {     tw3p[m][mm]= 1.161547; } 
	  if(m==63 && mm == 35) {     tw3p[m][mm]= 0.197036; } 
	  if(m==63 && mm == 0) {     te2p[m][mm]= 0.003272; } 
	  if(m==63 && mm == 1) {     te2p[m][mm]= 0.389963; } 
	  if(m==63 && mm == 2) {     te2p[m][mm]= 0.346964; } 
	  if(m==63 && mm == 5) {     te2p[m][mm]= 0.659774; } 
	  if(m==63 && mm == 6) {     te2p[m][mm]= 0.130697; } 
	  if(m==63 && mm == 7) {     te2p[m][mm]= 2.059002; } 
	  if(m==63 && mm == 8) {     te2p[m][mm]= 0.211332; } 
	  if(m==63 && mm == 9) {     te2p[m][mm]= 0.357017; } 
	  if(m==63 && mm == 10) {     te2p[m][mm]= 0.353655; } 
	  if(m==63 && mm == 11) {     te2p[m][mm]= 0.296984; } 
	  if(m==63 && mm == 12) {     te2p[m][mm]= 0.284370; } 
	  if(m==63 && mm == 15) {     te2p[m][mm]= 0.547023; } 
	  if(m==63 && mm == 16) {     te2p[m][mm]= 0.085352; } 
	  if(m==63 && mm == 17) {     te2p[m][mm]= 0.450346; } 
	  if(m==63 && mm == 18) {     te2p[m][mm]= 0.897307; } 
	  if(m==63 && mm == 19) {     te2p[m][mm]= 0.916460; } 
	  if(m==63 && mm == 20) {     te2p[m][mm]= 0.508445; } 
	  if(m==63 && mm == 22) {     te2p[m][mm]= 0.119708; } 
	  if(m==63 && mm == 23) {     te2p[m][mm]= 0.699149; } 
	  if(m==63 && mm == 24) {     te2p[m][mm]= 0.818216; } 
	  if(m==63 && mm == 25) {     te2p[m][mm]= 0.097358; } 
	  if(m==63 && mm == 28) {     te2p[m][mm]= 0.064990; } 
	  if(m==63 && mm == 29) {     te2p[m][mm]= 1.851231; } 
	  if(m==63 && mm == 30) {     te2p[m][mm]= 0.550710; } 
	  if(m==63 && mm == 4) {     te3p[m][mm]= 0.143323; } 
	  if(m==63 && mm == 5) {     te3p[m][mm]= 1.857583; } 
	  if(m==63 && mm == 6) {     te3p[m][mm]= 0.721112; } 
	  if(m==63 && mm == 7) {     te3p[m][mm]= 1.191426; } 
	  if(m==63 && mm == 8) {     te3p[m][mm]= 0.014852; } 
	  if(m==63 && mm == 9) {     te3p[m][mm]= 1.844271; } 
	  if(m==63 && mm == 10) {     te3p[m][mm]= 0.362912; } 
	  if(m==63 && mm == 11) {     te3p[m][mm]= 0.383618; } 
	  if(m==63 && mm == 12) {     te3p[m][mm]= 0.465508; } 
	  if(m==63 && mm == 14) {     te3p[m][mm]= 0.624390; } 
	  if(m==63 && mm == 1) {     tw0n[m][mm]= 0.614076; } 
	  if(m==63 && mm == 2) {     tw0n[m][mm]= 0.209838; } 
	  if(m==63 && mm == 3) {     tw0n[m][mm]= 0.658875; } 
	  if(m==63 && mm == 5) {     tw0n[m][mm]= 0.069100; } 
	  if(m==63 && mm == 6) {     tw0n[m][mm]= 0.467773; } 
	  if(m==63 && mm == 7) {     tw0n[m][mm]= 1.018976; } 
	  if(m==63 && mm == 8) {     tw0n[m][mm]= 0.809331; } 
	  if(m==63 && mm == 9) {     tw0n[m][mm]= 1.150649; } 
	  if(m==63 && mm == 10) {     tw0n[m][mm]= 0.611858; } 
	  if(m==63 && mm == 11) {     tw0n[m][mm]= 0.394642; } 
	  if(m==63 && mm == 12) {     tw0n[m][mm]= 0.222293; } 
	  if(m==63 && mm == 14) {     tw0n[m][mm]= 0.481652; } 
	  if(m==63 && mm == 15) {     tw0n[m][mm]= 0.178439; } 
	  if(m==63 && mm == 17) {     tw0n[m][mm]= 0.092040; } 
	  if(m==63 && mm == 19) {     tw0n[m][mm]= 0.155849; } 
	  if(m==63 && mm == 20) {     tw0n[m][mm]= 0.003507; } 
	  if(m==63 && mm == 22) {     tw0n[m][mm]= 0.034007; } 
	  if(m==63 && mm == 24) {     tw0n[m][mm]= 0.313674; } 
	  if(m==63 && mm == 25) {     tw0n[m][mm]= 0.309522; } 
	  if(m==63 && mm == 27) {     tw0n[m][mm]= 0.118971; } 
	  if(m==63 && mm == 28) {     tw0n[m][mm]= 0.086566; } 
	  if(m==63 && mm == 6) {     tw1n[m][mm]= 0.119116; } 
	  if(m==63 && mm == 11) {     tw1n[m][mm]= 0.341652; } 
	  if(m==63 && mm == 19) {     tw1n[m][mm]= 0.239337; } 
	  if(m==63 && mm == 21) {     tw1n[m][mm]= 0.337936; } 
	  if(m==63 && mm == 22) {     tw1n[m][mm]= 0.468542; } 
	  if(m==63 && mm == 23) {     tw1n[m][mm]= 0.233644; } 
	  if(m==63 && mm == 30) {     tw1n[m][mm]= 0.229227; } 
	  if(m==63 && mm == 34) {     tw1n[m][mm]= 0.613554; } 
	  if(m==63 && mm == 0) {     tw2n[m][mm]= 0.170493; } 
	  if(m==63 && mm == 1) {     tw2n[m][mm]= 0.261774; } 
	  if(m==63 && mm == 13) {     tw2n[m][mm]= 0.257917; } 
	  if(m==63 && mm == 14) {     tw2n[m][mm]= 0.168059; } 
	  if(m==63 && mm == 16) {     tw2n[m][mm]= 0.485028; } 
	  if(m==63 && mm == 19) {     tw2n[m][mm]= 0.024096; } 
	  if(m==63 && mm == 21) {     tw2n[m][mm]= 0.052322; } 
	  if(m==63 && mm == 22) {     tw2n[m][mm]= 0.114373; } 
	  if(m==63 && mm == 24) {     tw2n[m][mm]= 0.049206; } 
	  if(m==63 && mm == 25) {     tw2n[m][mm]= 0.245198; } 
	  if(m==63 && mm == 27) {     tw2n[m][mm]= 0.281837; } 
	  if(m==63 && mm == 28) {     tw2n[m][mm]= 0.284986; } 
	  if(m==63 && mm == 29) {     tw2n[m][mm]= 0.479359; } 
	  if(m==63 && mm == 30) {     tw2n[m][mm]= 1.487262; } 
	  if(m==63 && mm == 31) {     tw2n[m][mm]= 0.006164; } 
	  if(m==63 && mm == 32) {     tw2n[m][mm]= 1.462227; } 
	  if(m==63 && mm == 34) {     tw2n[m][mm]= 0.390407; } 
	  if(m==63 && mm == 2) {     tw3n[m][mm]= 0.180765; } 
	  if(m==63 && mm == 3) {     tw3n[m][mm]= 0.422773; } 
	  if(m==63 && mm == 4) {     tw3n[m][mm]= 0.710787; } 
	  if(m==63 && mm == 5) {     tw3n[m][mm]= 0.770761; } 
	  if(m==63 && mm == 6) {     tw3n[m][mm]= 1.252434; } 
	  if(m==63 && mm == 7) {     tw3n[m][mm]= 0.267718; } 
	  if(m==63 && mm == 8) {     tw3n[m][mm]= 0.435201; } 
	  if(m==63 && mm == 9) {     tw3n[m][mm]= 0.832922; } 
	  if(m==63 && mm == 12) {     tw3n[m][mm]= 0.331817; } 
	  if(m==63 && mm == 18) {     tw3n[m][mm]= 0.209159; } 
	  if(m==63 && mm == 20) {     tw3n[m][mm]= 0.396428; } 
	  if(m==63 && mm == 28) {     tw3n[m][mm]= 0.044456; } 
	  if(m==63 && mm == 4) {     te2n[m][mm]= 0.659342; } 
	  if(m==63 && mm == 5) {     te2n[m][mm]= 0.490363; } 
	  if(m==63 && mm == 6) {     te2n[m][mm]= 0.030821; } 
	  if(m==63 && mm == 7) {     te2n[m][mm]= 1.550327; } 
	  if(m==63 && mm == 8) {     te2n[m][mm]= 0.160499; } 
	  if(m==63 && mm == 9) {     te2n[m][mm]= 0.185782; } 
	  if(m==63 && mm == 11) {     te2n[m][mm]= 0.048980; } 
	  if(m==63 && mm == 12) {     te2n[m][mm]= 0.296806; } 
	  if(m==63 && mm == 14) {     te2n[m][mm]= 0.003650; } 
	  if(m==63 && mm == 15) {     te2n[m][mm]= 0.372405; } 
	  if(m==63 && mm == 16) {     te2n[m][mm]= 0.171169; } 
	  if(m==63 && mm == 17) {     te2n[m][mm]= 0.042571; } 
	  if(m==63 && mm == 18) {     te2n[m][mm]= 1.008277; } 
	  if(m==63 && mm == 19) {     te2n[m][mm]= 2.358330; } 
	  if(m==63 && mm == 20) {     te2n[m][mm]= 0.804052; } 
	  if(m==63 && mm == 22) {     te2n[m][mm]= 0.336080; } 
	  if(m==63 && mm == 23) {     te2n[m][mm]= 0.785381; } 
	  if(m==63 && mm == 24) {     te2n[m][mm]= 1.069578; } 
	  if(m==63 && mm == 25) {     te2n[m][mm]= 0.031421; } 
	  if(m==63 && mm == 28) {     te2n[m][mm]= 0.163326; } 
	  if(m==63 && mm == 29) {     te2n[m][mm]= 2.036803; } 
	  if(m==63 && mm == 30) {     te2n[m][mm]= 0.363549; } 
	  if(m==63 && mm == 31) {     te2n[m][mm]= 0.694608; } 
	  if(m==63 && mm == 32) {     te2n[m][mm]= 0.423176; } 
	  if(m==63 && mm == 33) {     te2n[m][mm]= 0.314654; } 
	  if(m==63 && mm == 34) {     te2n[m][mm]= 0.970285; } 
	  if(m==63 && mm == 35) {     te2n[m][mm]= 0.429199; } 
	  if(m==63 && mm == 0) {     te3n[m][mm]= 0.251808; } 
	  if(m==63 && mm == 1) {     te3n[m][mm]= 0.742087; } 
	  if(m==63 && mm == 2) {     te3n[m][mm]= 0.312263; } 
	  if(m==63 && mm == 3) {     te3n[m][mm]= 0.478261; } 
	  if(m==63 && mm == 4) {     te3n[m][mm]= 0.092889; } 
	  if(m==63 && mm == 5) {     te3n[m][mm]= 1.325006; } 
	  if(m==63 && mm == 6) {     te3n[m][mm]= 0.434739; } 
	  if(m==63 && mm == 23) {     te3n[m][mm]= 0.301116; } 
	  if(m==63 && mm == 24) {     te3n[m][mm]= 0.786482; } 
	  if(m==63 && mm == 28) {     te3n[m][mm]= 0.685134; } 
	  if(m==63 && mm == 29) {     te3n[m][mm]= 0.255810; } 
	  if(m==63 && mm == 30) {     te3n[m][mm]= 0.428818; } 
	  if(m==63 && mm == 31) {     te3n[m][mm]= 0.205344; } 
	  if(m==63 && mm == 32) {     te3n[m][mm]= 1.486028; } 
	  if(m==63 && mm == 34) {     te3n[m][mm]= 0.104128; } 
	  if(m==63 && mm == 35) {     te3n[m][mm]= 0.206509; } 
	  if(m==64 && mm == 7) {     tw0p[m][mm]= 0.269283; } 
	  if(m==64 && mm == 8) {     tw0p[m][mm]= 1.820215; } 
	  if(m==64 && mm == 10) {     tw0p[m][mm]= 0.174755; } 
	  if(m==64 && mm == 14) {     tw0p[m][mm]= 0.333711; } 
	  if(m==64 && mm == 15) {     tw0p[m][mm]= 0.565659; } 
	  if(m==64 && mm == 16) {     tw0p[m][mm]= 0.544151; } 
	  if(m==64 && mm == 18) {     tw0p[m][mm]= 0.425302; } 
	  if(m==64 && mm == 19) {     tw0p[m][mm]= 0.522479; } 
	  if(m==64 && mm == 20) {     tw0p[m][mm]= 0.097419; } 
	  if(m==64 && mm == 21) {     tw0p[m][mm]= 1.808686; } 
	  if(m==64 && mm == 22) {     tw0p[m][mm]= 0.311766; } 
	  if(m==64 && mm == 23) {     tw0p[m][mm]= 0.813292; } 
	  if(m==64 && mm == 25) {     tw0p[m][mm]= 0.309078; } 
	  if(m==64 && mm == 26) {     tw0p[m][mm]= 0.201217; } 
	  if(m==64 && mm == 27) {     tw0p[m][mm]= 0.509560; } 
	  if(m==64 && mm == 29) {     tw0p[m][mm]= 0.174577; } 
	  if(m==64 && mm == 30) {     tw0p[m][mm]= 0.464552; } 
	  if(m==64 && mm == 31) {     tw0p[m][mm]= 0.082067; } 
	  if(m==64 && mm == 32) {     tw0p[m][mm]= 0.253373; } 
	  if(m==64 && mm == 33) {     tw0p[m][mm]= 0.097735; } 
	  if(m==64 && mm == 34) {     tw0p[m][mm]= 0.871604; } 
	  if(m==64 && mm == 35) {     tw0p[m][mm]= 0.087686; } 
	  if(m==64 && mm == 1) {     tw1p[m][mm]= 0.041454; } 
	  if(m==64 && mm == 2) {     tw1p[m][mm]= 0.149120; } 
	  if(m==64 && mm == 3) {     tw1p[m][mm]= 0.393046; } 
	  if(m==64 && mm == 5) {     tw1p[m][mm]= 0.577300; } 
	  if(m==64 && mm == 7) {     tw1p[m][mm]= 1.001465; } 
	  if(m==64 && mm == 21) {     tw1p[m][mm]= 0.964740; } 
	  if(m==64 && mm == 22) {     tw1p[m][mm]= 0.167039; } 
	  if(m==64 && mm == 23) {     tw1p[m][mm]= 0.639268; } 
	  if(m==64 && mm == 24) {     tw1p[m][mm]= 0.161685; } 
	  if(m==64 && mm == 26) {     tw1p[m][mm]= 0.348258; } 
	  if(m==64 && mm == 27) {     tw1p[m][mm]= 0.242171; } 
	  if(m==64 && mm == 28) {     tw1p[m][mm]= 0.272949; } 
	  if(m==64 && mm == 31) {     tw1p[m][mm]= 0.246504; } 
	  if(m==64 && mm == 32) {     tw1p[m][mm]= 0.745554; } 
	  if(m==64 && mm == 33) {     tw1p[m][mm]= 1.020780; } 
	  if(m==64 && mm == 34) {     tw1p[m][mm]= 0.413806; } 
	  if(m==64 && mm == 13) {     tw2p[m][mm]= 0.356674; } 
	  if(m==64 && mm == 14) {     tw2p[m][mm]= 0.308376; } 
	  if(m==64 && mm == 15) {     tw2p[m][mm]= 0.377881; } 
	  if(m==64 && mm == 17) {     tw2p[m][mm]= 0.256320; } 
	  if(m==64 && mm == 18) {     tw2p[m][mm]= 0.739152; } 
	  if(m==64 && mm == 19) {     tw2p[m][mm]= 0.620087; } 
	  if(m==64 && mm == 20) {     tw2p[m][mm]= 1.010078; } 
	  if(m==64 && mm == 21) {     tw2p[m][mm]= 0.699323; } 
	  if(m==64 && mm == 22) {     tw2p[m][mm]= 0.325979; } 
	  if(m==64 && mm == 23) {     tw2p[m][mm]= 0.291355; } 
	  if(m==64 && mm == 30) {     tw2p[m][mm]= 0.144269; } 
	  if(m==64 && mm == 31) {     tw2p[m][mm]= 2.744437; } 
	  if(m==64 && mm == 32) {     tw2p[m][mm]= 0.725970; } 
	  if(m==64 && mm == 34) {     tw2p[m][mm]= 1.053826; } 
	  if(m==64 && mm == 35) {     tw2p[m][mm]= 0.593780; } 
	  if(m==64 && mm == 0) {     tw3p[m][mm]= 0.794090; } 
	  if(m==64 && mm == 1) {     tw3p[m][mm]= 0.779010; } 
	  if(m==64 && mm == 2) {     tw3p[m][mm]= 0.542958; } 
	  if(m==64 && mm == 3) {     tw3p[m][mm]= 1.156319; } 
	  if(m==64 && mm == 4) {     tw3p[m][mm]= 0.741508; } 
	  if(m==64 && mm == 5) {     tw3p[m][mm]= 0.881720; } 
	  if(m==64 && mm == 6) {     tw3p[m][mm]= 1.087654; } 
	  if(m==64 && mm == 7) {     tw3p[m][mm]= 0.893794; } 
	  if(m==64 && mm == 9) {     tw3p[m][mm]= 0.904662; } 
	  if(m==64 && mm == 10) {     tw3p[m][mm]= 0.843367; } 
	  if(m==64 && mm == 11) {     tw3p[m][mm]= 1.431405; } 
	  if(m==64 && mm == 13) {     tw3p[m][mm]= 0.501258; } 
	  if(m==64 && mm == 28) {     tw3p[m][mm]= 0.144466; } 
	  if(m==64 && mm == 30) {     tw3p[m][mm]= 0.554885; } 
	  if(m==64 && mm == 31) {     tw3p[m][mm]= 0.285341; } 
	  if(m==64 && mm == 32) {     tw3p[m][mm]= 0.449968; } 
	  if(m==64 && mm == 33) {     tw3p[m][mm]= 0.445375; } 
	  if(m==64 && mm == 34) {     tw3p[m][mm]= 1.440940; } 
	  if(m==64 && mm == 35) {     tw3p[m][mm]= 1.389641; } 
	  if(m==64 && mm == 1) {     te2p[m][mm]= 0.122463; } 
	  if(m==64 && mm == 2) {     te2p[m][mm]= 0.709408; } 
	  if(m==64 && mm == 3) {     te2p[m][mm]= 0.635345; } 
	  if(m==64 && mm == 4) {     te2p[m][mm]= 0.368311; } 
	  if(m==64 && mm == 5) {     te2p[m][mm]= 0.165863; } 
	  if(m==64 && mm == 6) {     te2p[m][mm]= 0.713027; } 
	  if(m==64 && mm == 7) {     te2p[m][mm]= 0.150540; } 
	  if(m==64 && mm == 8) {     te2p[m][mm]= 0.709361; } 
	  if(m==64 && mm == 9) {     te2p[m][mm]= 0.536782; } 
	  if(m==64 && mm == 10) {     te2p[m][mm]= 0.179882; } 
	  if(m==64 && mm == 11) {     te2p[m][mm]= 0.850781; } 
	  if(m==64 && mm == 12) {     te2p[m][mm]= 0.301357; } 
	  if(m==64 && mm == 13) {     te2p[m][mm]= 0.506297; } 
	  if(m==64 && mm == 14) {     te2p[m][mm]= 1.375921; } 
	  if(m==64 && mm == 15) {     te2p[m][mm]= 0.739164; } 
	  if(m==64 && mm == 16) {     te2p[m][mm]= 0.565689; } 
	  if(m==64 && mm == 17) {     te2p[m][mm]= 0.344450; } 
	  if(m==64 && mm == 18) {     te2p[m][mm]= 2.921026; } 
	  if(m==64 && mm == 19) {     te2p[m][mm]= 1.115410; } 
	  if(m==64 && mm == 20) {     te2p[m][mm]= 0.223204; } 
	  if(m==64 && mm == 21) {     te2p[m][mm]= 0.603797; } 
	  if(m==64 && mm == 22) {     te2p[m][mm]= 0.605176; } 
	  if(m==64 && mm == 23) {     te2p[m][mm]= 1.091993; } 
	  if(m==64 && mm == 24) {     te2p[m][mm]= 0.507910; } 
	  if(m==64 && mm == 25) {     te2p[m][mm]= 0.546484; } 
	  if(m==64 && mm == 28) {     te2p[m][mm]= 0.492297; } 
	  if(m==64 && mm == 29) {     te2p[m][mm]= 0.349115; } 
	  if(m==64 && mm == 30) {     te2p[m][mm]= 0.498918; } 
	  if(m==64 && mm == 4) {     te3p[m][mm]= 0.450673; } 
	  if(m==64 && mm == 5) {     te3p[m][mm]= 0.910616; } 
	  if(m==64 && mm == 6) {     te3p[m][mm]= 2.276193; } 
	  if(m==64 && mm == 7) {     te3p[m][mm]= 0.187163; } 
	  if(m==64 && mm == 8) {     te3p[m][mm]= 1.329058; } 
	  if(m==64 && mm == 10) {     te3p[m][mm]= 0.342891; } 
	  if(m==64 && mm == 11) {     te3p[m][mm]= 0.166269; } 
	  if(m==64 && mm == 14) {     te3p[m][mm]= 0.783668; } 
	  if(m==64 && mm == 1) {     tw0n[m][mm]= 0.401698; } 
	  if(m==64 && mm == 2) {     tw0n[m][mm]= 1.122418; } 
	  if(m==64 && mm == 3) {     tw0n[m][mm]= 2.480486; } 
	  if(m==64 && mm == 4) {     tw0n[m][mm]= 0.555471; } 
	  if(m==64 && mm == 5) {     tw0n[m][mm]= 0.956104; } 
	  if(m==64 && mm == 6) {     tw0n[m][mm]= 0.584364; } 
	  if(m==64 && mm == 7) {     tw0n[m][mm]= 0.452314; } 
	  if(m==64 && mm == 9) {     tw0n[m][mm]= 0.170482; } 
	  if(m==64 && mm == 10) {     tw0n[m][mm]= 0.444388; } 
	  if(m==64 && mm == 14) {     tw0n[m][mm]= 0.436530; } 
	  if(m==64 && mm == 15) {     tw0n[m][mm]= 0.807913; } 
	  if(m==64 && mm == 16) {     tw0n[m][mm]= 0.478490; } 
	  if(m==64 && mm == 18) {     tw0n[m][mm]= 0.852822; } 
	  if(m==64 && mm == 19) {     tw0n[m][mm]= 0.559880; } 
	  if(m==64 && mm == 20) {     tw0n[m][mm]= 0.059789; } 
	  if(m==64 && mm == 21) {     tw0n[m][mm]= 1.543505; } 
	  if(m==64 && mm == 22) {     tw0n[m][mm]= 0.638543; } 
	  if(m==64 && mm == 23) {     tw0n[m][mm]= 0.802054; } 
	  if(m==64 && mm == 25) {     tw0n[m][mm]= 0.120518; } 
	  if(m==64 && mm == 26) {     tw0n[m][mm]= 0.014785; } 
	  if(m==64 && mm == 27) {     tw0n[m][mm]= 0.357801; } 
	  if(m==64 && mm == 7) {     tw1n[m][mm]= 1.042231; } 
	  if(m==64 && mm == 10) {     tw1n[m][mm]= 1.633108; } 
	  if(m==64 && mm == 11) {     tw1n[m][mm]= 0.321295; } 
	  if(m==64 && mm == 14) {     tw1n[m][mm]= 0.052638; } 
	  if(m==64 && mm == 16) {     tw1n[m][mm]= 0.106612; } 
	  if(m==64 && mm == 17) {     tw1n[m][mm]= 0.438763; } 
	  if(m==64 && mm == 18) {     tw1n[m][mm]= 0.010593; } 
	  if(m==64 && mm == 19) {     tw1n[m][mm]= 0.233271; } 
	  if(m==64 && mm == 22) {     tw1n[m][mm]= 0.329906; } 
	  if(m==64 && mm == 23) {     tw1n[m][mm]= 0.565883; } 
	  if(m==64 && mm == 31) {     tw1n[m][mm]= 0.002945; } 
	  if(m==64 && mm == 32) {     tw1n[m][mm]= 0.375020; } 
	  if(m==64 && mm == 33) {     tw1n[m][mm]= 0.247303; } 
	  if(m==64 && mm == 34) {     tw1n[m][mm]= 0.195112; } 
	  if(m==64 && mm == 13) {     tw2n[m][mm]= 0.214544; } 
	  if(m==64 && mm == 14) {     tw2n[m][mm]= 0.289005; } 
	  if(m==64 && mm == 15) {     tw2n[m][mm]= 0.258823; } 
	  if(m==64 && mm == 17) {     tw2n[m][mm]= 0.386437; } 
	  if(m==64 && mm == 18) {     tw2n[m][mm]= 1.272004; } 
	  if(m==64 && mm == 19) {     tw2n[m][mm]= 0.490989; } 
	  if(m==64 && mm == 21) {     tw2n[m][mm]= 0.371483; } 
	  if(m==64 && mm == 22) {     tw2n[m][mm]= 0.340699; } 
	  if(m==64 && mm == 23) {     tw2n[m][mm]= 0.395431; } 
	  if(m==64 && mm == 24) {     tw2n[m][mm]= 0.365340; } 
	  if(m==64 && mm == 26) {     tw2n[m][mm]= 0.296940; } 
	  if(m==64 && mm == 28) {     tw2n[m][mm]= 0.628755; } 
	  if(m==64 && mm == 29) {     tw2n[m][mm]= 0.341919; } 
	  if(m==64 && mm == 30) {     tw2n[m][mm]= 0.011478; } 
	  if(m==64 && mm == 31) {     tw2n[m][mm]= 0.997603; } 
	  if(m==64 && mm == 32) {     tw2n[m][mm]= 0.479034; } 
	  if(m==64 && mm == 33) {     tw2n[m][mm]= 0.431356; } 
	  if(m==64 && mm == 34) {     tw2n[m][mm]= 0.544313; } 
	  if(m==64 && mm == 0) {     tw3n[m][mm]= 0.961093; } 
	  if(m==64 && mm == 1) {     tw3n[m][mm]= 0.720784; } 
	  if(m==64 && mm == 2) {     tw3n[m][mm]= 0.657027; } 
	  if(m==64 && mm == 3) {     tw3n[m][mm]= 1.009454; } 
	  if(m==64 && mm == 4) {     tw3n[m][mm]= 0.354168; } 
	  if(m==64 && mm == 6) {     tw3n[m][mm]= 1.028783; } 
	  if(m==64 && mm == 7) {     tw3n[m][mm]= 0.862958; } 
	  if(m==64 && mm == 9) {     tw3n[m][mm]= 1.069105; } 
	  if(m==64 && mm == 10) {     tw3n[m][mm]= 0.721958; } 
	  if(m==64 && mm == 11) {     tw3n[m][mm]= 1.148527; } 
	  if(m==64 && mm == 13) {     tw3n[m][mm]= 0.472232; } 
	  if(m==64 && mm == 5) {     te2n[m][mm]= 0.365250; } 
	  if(m==64 && mm == 6) {     te2n[m][mm]= 0.560784; } 
	  if(m==64 && mm == 7) {     te2n[m][mm]= 0.041194; } 
	  if(m==64 && mm == 8) {     te2n[m][mm]= 0.384888; } 
	  if(m==64 && mm == 9) {     te2n[m][mm]= 0.469465; } 
	  if(m==64 && mm == 10) {     te2n[m][mm]= 0.134673; } 
	  if(m==64 && mm == 11) {     te2n[m][mm]= 0.891565; } 
	  if(m==64 && mm == 12) {     te2n[m][mm]= 0.341545; } 
	  if(m==64 && mm == 13) {     te2n[m][mm]= 0.381596; } 
	  if(m==64 && mm == 14) {     te2n[m][mm]= 0.781040; } 
	  if(m==64 && mm == 15) {     te2n[m][mm]= 0.849417; } 
	  if(m==64 && mm == 16) {     te2n[m][mm]= 0.371340; } 
	  if(m==64 && mm == 17) {     te2n[m][mm]= 0.280132; } 
	  if(m==64 && mm == 18) {     te2n[m][mm]= 2.567169; } 
	  if(m==64 && mm == 19) {     te2n[m][mm]= 1.159909; } 
	  if(m==64 && mm == 20) {     te2n[m][mm]= 0.501463; } 
	  if(m==64 && mm == 21) {     te2n[m][mm]= 0.386479; } 
	  if(m==64 && mm == 22) {     te2n[m][mm]= 0.733792; } 
	  if(m==64 && mm == 23) {     te2n[m][mm]= 0.869269; } 
	  if(m==64 && mm == 24) {     te2n[m][mm]= 0.577428; } 
	  if(m==64 && mm == 25) {     te2n[m][mm]= 0.692482; } 
	  if(m==64 && mm == 28) {     te2n[m][mm]= 0.561824; } 
	  if(m==64 && mm == 29) {     te2n[m][mm]= 0.425141; } 
	  if(m==64 && mm == 30) {     te2n[m][mm]= 0.561999; } 
	  if(m==64 && mm == 32) {     te2n[m][mm]= 1.311206; } 
	  if(m==64 && mm == 33) {     te2n[m][mm]= 0.332270; } 
	  if(m==64 && mm == 35) {     te2n[m][mm]= 0.187974; } 
	  if(m==64 && mm == 0) {     te3n[m][mm]= 0.356762; } 
	  if(m==64 && mm == 1) {     te3n[m][mm]= 0.282649; } 
	  if(m==64 && mm == 2) {     te3n[m][mm]= 0.455496; } 
	  if(m==64 && mm == 3) {     te3n[m][mm]= 1.000508; } 
	  if(m==64 && mm == 4) {     te3n[m][mm]= 0.330064; } 
	  if(m==64 && mm == 5) {     te3n[m][mm]= 0.622862; } 
	  if(m==64 && mm == 6) {     te3n[m][mm]= 2.171820; } 
	  if(m==64 && mm == 24) {     te3n[m][mm]= 0.055911; } 
	  if(m==64 && mm == 26) {     te3n[m][mm]= 0.641621; } 
	  if(m==64 && mm == 27) {     te3n[m][mm]= 0.148247; } 
	  if(m==64 && mm == 28) {     te3n[m][mm]= 0.432731; } 
	  if(m==64 && mm == 29) {     te3n[m][mm]= 0.212675; } 
	  if(m==64 && mm == 30) {     te3n[m][mm]= 0.686165; } 
	  if(m==64 && mm == 31) {     te3n[m][mm]= 0.443901; } 
	  if(m==64 && mm == 32) {     te3n[m][mm]= 0.444384; } 
	  if(m==64 && mm == 33) {     te3n[m][mm]= 1.010194; } 
	  if(m==64 && mm == 34) {     te3n[m][mm]= 0.962626; } 
	  if(m==65 && mm == 6) {     tw0p[m][mm]= 0.159174; } 
	  if(m==65 && mm == 7) {     tw0p[m][mm]= 0.327750; } 
	  if(m==65 && mm == 9) {     tw0p[m][mm]= 0.120380; } 
	  if(m==65 && mm == 16) {     tw0p[m][mm]= 0.008991; } 
	  if(m==65 && mm == 17) {     tw0p[m][mm]= 0.371956; } 
	  if(m==65 && mm == 19) {     tw0p[m][mm]= 1.212415; } 
	  if(m==65 && mm == 20) {     tw0p[m][mm]= 0.584583; } 
	  if(m==65 && mm == 21) {     tw0p[m][mm]= 0.650360; } 
	  if(m==65 && mm == 23) {     tw0p[m][mm]= 0.004802; } 
	  if(m==65 && mm == 24) {     tw0p[m][mm]= 0.017687; } 
	  if(m==65 && mm == 29) {     tw0p[m][mm]= 0.151267; } 
	  if(m==65 && mm == 30) {     tw0p[m][mm]= 0.360366; } 
	  if(m==65 && mm == 31) {     tw0p[m][mm]= 0.016954; } 
	  if(m==65 && mm == 32) {     tw0p[m][mm]= 0.403424; } 
	  if(m==65 && mm == 35) {     tw0p[m][mm]= 0.229859; } 
	  if(m==65 && mm == 0) {     tw1p[m][mm]= 0.249492; } 
	  if(m==65 && mm == 3) {     tw1p[m][mm]= 0.302040; } 
	  if(m==65 && mm == 4) {     tw1p[m][mm]= 0.367357; } 
	  if(m==65 && mm == 7) {     tw1p[m][mm]= 0.097411; } 
	  if(m==65 && mm == 8) {     tw1p[m][mm]= 0.265390; } 
	  if(m==65 && mm == 21) {     tw1p[m][mm]= 1.089260; } 
	  if(m==65 && mm == 22) {     tw1p[m][mm]= 1.112758; } 
	  if(m==65 && mm == 23) {     tw1p[m][mm]= 0.325318; } 
	  if(m==65 && mm == 24) {     tw1p[m][mm]= 0.784132; } 
	  if(m==65 && mm == 25) {     tw1p[m][mm]= 1.173592; } 
	  if(m==65 && mm == 26) {     tw1p[m][mm]= 0.515014; } 
	  if(m==65 && mm == 29) {     tw1p[m][mm]= 0.117259; } 
	  if(m==65 && mm == 32) {     tw1p[m][mm]= 0.677119; } 
	  if(m==65 && mm == 35) {     tw1p[m][mm]= 0.247845; } 
	  if(m==65 && mm == 12) {     tw2p[m][mm]= 0.090197; } 
	  if(m==65 && mm == 13) {     tw2p[m][mm]= 0.200683; } 
	  if(m==65 && mm == 14) {     tw2p[m][mm]= 0.107736; } 
	  if(m==65 && mm == 15) {     tw2p[m][mm]= 0.857825; } 
	  if(m==65 && mm == 18) {     tw2p[m][mm]= 1.064675; } 
	  if(m==65 && mm == 19) {     tw2p[m][mm]= 0.710234; } 
	  if(m==65 && mm == 20) {     tw2p[m][mm]= 0.866525; } 
	  if(m==65 && mm == 21) {     tw2p[m][mm]= 0.190595; } 
	  if(m==65 && mm == 23) {     tw2p[m][mm]= 0.026786; } 
	  if(m==65 && mm == 29) {     tw2p[m][mm]= 0.472071; } 
	  if(m==65 && mm == 30) {     tw2p[m][mm]= 0.019822; } 
	  if(m==65 && mm == 31) {     tw2p[m][mm]= 2.181608; } 
	  if(m==65 && mm == 32) {     tw2p[m][mm]= 1.758408; } 
	  if(m==65 && mm == 33) {     tw2p[m][mm]= 0.392799; } 
	  if(m==65 && mm == 35) {     tw2p[m][mm]= 0.122986; } 
	  if(m==65 && mm == 0) {     tw3p[m][mm]= 0.335053; } 
	  if(m==65 && mm == 1) {     tw3p[m][mm]= 0.737876; } 
	  if(m==65 && mm == 2) {     tw3p[m][mm]= 0.708428; } 
	  if(m==65 && mm == 3) {     tw3p[m][mm]= 0.480415; } 
	  if(m==65 && mm == 4) {     tw3p[m][mm]= 0.396965; } 
	  if(m==65 && mm == 5) {     tw3p[m][mm]= 0.707610; } 
	  if(m==65 && mm == 6) {     tw3p[m][mm]= 0.343614; } 
	  if(m==65 && mm == 7) {     tw3p[m][mm]= 0.539715; } 
	  if(m==65 && mm == 10) {     tw3p[m][mm]= 0.643014; } 
	  if(m==65 && mm == 11) {     tw3p[m][mm]= 0.651444; } 
	  if(m==65 && mm == 15) {     tw3p[m][mm]= 0.629270; } 
	  if(m==65 && mm == 30) {     tw3p[m][mm]= 0.771271; } 
	  if(m==65 && mm == 31) {     tw3p[m][mm]= 0.341361; } 
	  if(m==65 && mm == 32) {     tw3p[m][mm]= 1.212728; } 
	  if(m==65 && mm == 33) {     tw3p[m][mm]= 1.018058; } 
	  if(m==65 && mm == 34) {     tw3p[m][mm]= 2.025268; } 
	  if(m==65 && mm == 35) {     tw3p[m][mm]= 1.490834; } 
	  if(m==65 && mm == 2) {     te2p[m][mm]= 0.368845; } 
	  if(m==65 && mm == 3) {     te2p[m][mm]= 0.445584; } 
	  if(m==65 && mm == 5) {     te2p[m][mm]= 0.750379; } 
	  if(m==65 && mm == 6) {     te2p[m][mm]= 0.216308; } 
	  if(m==65 && mm == 7) {     te2p[m][mm]= 0.419119; } 
	  if(m==65 && mm == 9) {     te2p[m][mm]= 1.090346; } 
	  if(m==65 && mm == 10) {     te2p[m][mm]= 0.027452; } 
	  if(m==65 && mm == 12) {     te2p[m][mm]= 0.601492; } 
	  if(m==65 && mm == 13) {     te2p[m][mm]= 0.748562; } 
	  if(m==65 && mm == 14) {     te2p[m][mm]= 0.672977; } 
	  if(m==65 && mm == 15) {     te2p[m][mm]= 0.679955; } 
	  if(m==65 && mm == 16) {     te2p[m][mm]= 0.046459; } 
	  if(m==65 && mm == 17) {     te2p[m][mm]= 0.788082; } 
	  if(m==65 && mm == 18) {     te2p[m][mm]= 1.914279; } 
	  if(m==65 && mm == 19) {     te2p[m][mm]= 0.643375; } 
	  if(m==65 && mm == 20) {     te2p[m][mm]= 0.306347; } 
	  if(m==65 && mm == 21) {     te2p[m][mm]= 0.162383; } 
	  if(m==65 && mm == 22) {     te2p[m][mm]= 0.212446; } 
	  if(m==65 && mm == 24) {     te2p[m][mm]= 0.675287; } 
	  if(m==65 && mm == 25) {     te2p[m][mm]= 0.426904; } 
	  if(m==65 && mm == 26) {     te2p[m][mm]= 1.259732; } 
	  if(m==65 && mm == 28) {     te2p[m][mm]= 0.965057; } 
	  if(m==65 && mm == 30) {     te2p[m][mm]= 0.854915; } 
	  if(m==65 && mm == 5) {     te3p[m][mm]= 0.909158; } 
	  if(m==65 && mm == 6) {     te3p[m][mm]= 0.094227; } 
	  if(m==65 && mm == 7) {     te3p[m][mm]= 0.169047; } 
	  if(m==65 && mm == 9) {     te3p[m][mm]= 1.616868; } 
	  if(m==65 && mm == 0) {     tw0n[m][mm]= 0.470063; } 
	  if(m==65 && mm == 1) {     tw0n[m][mm]= 0.842380; } 
	  if(m==65 && mm == 2) {     tw0n[m][mm]= 0.650749; } 
	  if(m==65 && mm == 3) {     tw0n[m][mm]= 0.101675; } 
	  if(m==65 && mm == 4) {     tw0n[m][mm]= 1.233339; } 
	  if(m==65 && mm == 5) {     tw0n[m][mm]= 0.053555; } 
	  if(m==65 && mm == 6) {     tw0n[m][mm]= 0.071491; } 
	  if(m==65 && mm == 7) {     tw0n[m][mm]= 0.386453; } 
	  if(m==65 && mm == 8) {     tw0n[m][mm]= 0.314353; } 
	  if(m==65 && mm == 9) {     tw0n[m][mm]= 0.239135; } 
	  if(m==65 && mm == 11) {     tw0n[m][mm]= 0.100885; } 
	  if(m==65 && mm == 17) {     tw0n[m][mm]= 0.454156; } 
	  if(m==65 && mm == 19) {     tw0n[m][mm]= 0.934636; } 
	  if(m==65 && mm == 20) {     tw0n[m][mm]= 0.651796; } 
	  if(m==65 && mm == 21) {     tw0n[m][mm]= 0.674059; } 
	  if(m==65 && mm == 22) {     tw0n[m][mm]= 2.027953; } 
	  if(m==65 && mm == 23) {     tw0n[m][mm]= 0.100755; } 
	  if(m==65 && mm == 24) {     tw0n[m][mm]= 0.041420; } 
	  if(m==65 && mm == 25) {     tw0n[m][mm]= 0.008421; } 
	  if(m==65 && mm == 7) {     tw1n[m][mm]= 0.248389; } 
	  if(m==65 && mm == 12) {     tw1n[m][mm]= 1.189348; } 
	  if(m==65 && mm == 13) {     tw1n[m][mm]= 0.328603; } 
	  if(m==65 && mm == 15) {     tw1n[m][mm]= 0.266091; } 
	  if(m==65 && mm == 18) {     tw1n[m][mm]= 0.519899; } 
	  if(m==65 && mm == 21) {     tw1n[m][mm]= 0.873379; } 
	  if(m==65 && mm == 22) {     tw1n[m][mm]= 0.995336; } 
	  if(m==65 && mm == 32) {     tw1n[m][mm]= 0.373910; } 
	  if(m==65 && mm == 35) {     tw1n[m][mm]= 0.351206; } 
	  if(m==65 && mm == 13) {     tw2n[m][mm]= 0.390529; } 
	  if(m==65 && mm == 14) {     tw2n[m][mm]= 0.293680; } 
	  if(m==65 && mm == 15) {     tw2n[m][mm]= 0.686085; } 
	  if(m==65 && mm == 17) {     tw2n[m][mm]= 0.006420; } 
	  if(m==65 && mm == 18) {     tw2n[m][mm]= 0.392395; } 
	  if(m==65 && mm == 19) {     tw2n[m][mm]= 0.406970; } 
	  if(m==65 && mm == 20) {     tw2n[m][mm]= 0.532498; } 
	  if(m==65 && mm == 21) {     tw2n[m][mm]= 0.242402; } 
	  if(m==65 && mm == 25) {     tw2n[m][mm]= 0.688697; } 
	  if(m==65 && mm == 28) {     tw2n[m][mm]= 0.274156; } 
	  if(m==65 && mm == 30) {     tw2n[m][mm]= 0.115163; } 
	  if(m==65 && mm == 31) {     tw2n[m][mm]= 2.164701; } 
	  if(m==65 && mm == 35) {     tw2n[m][mm]= 1.238609; } 
	  if(m==65 && mm == 0) {     tw3n[m][mm]= 0.558579; } 
	  if(m==65 && mm == 1) {     tw3n[m][mm]= 0.909715; } 
	  if(m==65 && mm == 2) {     tw3n[m][mm]= 0.582688; } 
	  if(m==65 && mm == 3) {     tw3n[m][mm]= 0.496585; } 
	  if(m==65 && mm == 4) {     tw3n[m][mm]= 0.314605; } 
	  if(m==65 && mm == 5) {     tw3n[m][mm]= 0.478180; } 
	  if(m==65 && mm == 6) {     tw3n[m][mm]= 0.622882; } 
	  if(m==65 && mm == 7) {     tw3n[m][mm]= 0.581925; } 
	  if(m==65 && mm == 10) {     tw3n[m][mm]= 0.817495; } 
	  if(m==65 && mm == 11) {     tw3n[m][mm]= 0.423982; } 
	  if(m==65 && mm == 15) {     tw3n[m][mm]= 0.774223; } 
	  if(m==65 && mm == 5) {     te2n[m][mm]= 0.190760; } 
	  if(m==65 && mm == 6) {     te2n[m][mm]= 0.881728; } 
	  if(m==65 && mm == 7) {     te2n[m][mm]= 0.224389; } 
	  if(m==65 && mm == 9) {     te2n[m][mm]= 0.841865; } 
	  if(m==65 && mm == 12) {     te2n[m][mm]= 0.162645; } 
	  if(m==65 && mm == 14) {     te2n[m][mm]= 0.317635; } 
	  if(m==65 && mm == 15) {     te2n[m][mm]= 0.460250; } 
	  if(m==65 && mm == 17) {     te2n[m][mm]= 0.696381; } 
	  if(m==65 && mm == 18) {     te2n[m][mm]= 2.022609; } 
	  if(m==65 && mm == 19) {     te2n[m][mm]= 0.845389; } 
	  if(m==65 && mm == 21) {     te2n[m][mm]= 0.243782; } 
	  if(m==65 && mm == 22) {     te2n[m][mm]= 0.260812; } 
	  if(m==65 && mm == 24) {     te2n[m][mm]= 0.772545; } 
	  if(m==65 && mm == 25) {     te2n[m][mm]= 0.531856; } 
	  if(m==65 && mm == 28) {     te2n[m][mm]= 1.108340; } 
	  if(m==65 && mm == 30) {     te2n[m][mm]= 1.891228; } 
	  if(m==65 && mm == 31) {     te2n[m][mm]= 1.091456; } 
	  if(m==65 && mm == 32) {     te2n[m][mm]= 2.037005; } 
	  if(m==65 && mm == 33) {     te2n[m][mm]= 0.867261; } 
	  if(m==65 && mm == 34) {     te2n[m][mm]= 0.436477; } 
	  if(m==65 && mm == 35) {     te2n[m][mm]= 0.480797; } 
	  if(m==65 && mm == 0) {     te3n[m][mm]= 0.282989; } 
	  if(m==65 && mm == 2) {     te3n[m][mm]= 0.357531; } 
	  if(m==65 && mm == 3) {     te3n[m][mm]= 0.708500; } 
	  if(m==65 && mm == 6) {     te3n[m][mm]= 0.002413; } 
	  if(m==65 && mm == 16) {     te3n[m][mm]= 2.324317; } 
	  if(m==65 && mm == 25) {     te3n[m][mm]= 0.921904; } 
	  if(m==65 && mm == 26) {     te3n[m][mm]= 0.281520; } 
	  if(m==65 && mm == 30) {     te3n[m][mm]= 2.038606; } 
	  if(m==65 && mm == 31) {     te3n[m][mm]= 1.575822; } 
	  if(m==65 && mm == 32) {     te3n[m][mm]= 0.291475; } 
	  if(m==65 && mm == 33) {     te3n[m][mm]= 0.112532; } 
	  if(m==65 && mm == 34) {     te3n[m][mm]= 0.292893; } 
	  if(m==66 && mm == 7) {     tw0p[m][mm]= 1.314128; } 
	  if(m==66 && mm == 8) {     tw0p[m][mm]= 0.388494; } 
	  if(m==66 && mm == 9) {     tw0p[m][mm]= 0.095788; } 
	  if(m==66 && mm == 10) {     tw0p[m][mm]= 0.226171; } 
	  if(m==66 && mm == 11) {     tw0p[m][mm]= 0.304203; } 
	  if(m==66 && mm == 12) {     tw0p[m][mm]= 0.835532; } 
	  if(m==66 && mm == 14) {     tw0p[m][mm]= 0.218388; } 
	  if(m==66 && mm == 16) {     tw0p[m][mm]= 0.498552; } 
	  if(m==66 && mm == 17) {     tw0p[m][mm]= 0.374288; } 
	  if(m==66 && mm == 18) {     tw0p[m][mm]= 0.610587; } 
	  if(m==66 && mm == 19) {     tw0p[m][mm]= 0.063784; } 
	  if(m==66 && mm == 20) {     tw0p[m][mm]= 0.308242; } 
	  if(m==66 && mm == 22) {     tw0p[m][mm]= 0.526633; } 
	  if(m==66 && mm == 23) {     tw0p[m][mm]= 0.099005; } 
	  if(m==66 && mm == 24) {     tw0p[m][mm]= 0.056254; } 
	  if(m==66 && mm == 25) {     tw0p[m][mm]= 0.179955; } 
	  if(m==66 && mm == 27) {     tw0p[m][mm]= 0.169555; } 
	  if(m==66 && mm == 28) {     tw0p[m][mm]= 1.003263; } 
	  if(m==66 && mm == 29) {     tw0p[m][mm]= 0.384601; } 
	  if(m==66 && mm == 30) {     tw0p[m][mm]= 1.018217; } 
	  if(m==66 && mm == 31) {     tw0p[m][mm]= 0.427366; } 
	  if(m==66 && mm == 32) {     tw0p[m][mm]= 0.716114; } 
	  if(m==66 && mm == 33) {     tw0p[m][mm]= 0.354686; } 
	  if(m==66 && mm == 0) {     tw1p[m][mm]= 0.280682; } 
	  if(m==66 && mm == 4) {     tw1p[m][mm]= 0.106738; } 
	  if(m==66 && mm == 5) {     tw1p[m][mm]= 0.246284; } 
	  if(m==66 && mm == 6) {     tw1p[m][mm]= 0.793859; } 
	  if(m==66 && mm == 7) {     tw1p[m][mm]= 0.298991; } 
	  if(m==66 && mm == 9) {     tw1p[m][mm]= 0.977417; } 
	  if(m==66 && mm == 20) {     tw1p[m][mm]= 0.290415; } 
	  if(m==66 && mm == 21) {     tw1p[m][mm]= 0.414223; } 
	  if(m==66 && mm == 22) {     tw1p[m][mm]= 0.262116; } 
	  if(m==66 && mm == 23) {     tw1p[m][mm]= 1.180762; } 
	  if(m==66 && mm == 24) {     tw1p[m][mm]= 0.529319; } 
	  if(m==66 && mm == 27) {     tw1p[m][mm]= 0.151888; } 
	  if(m==66 && mm == 29) {     tw1p[m][mm]= 0.472539; } 
	  if(m==66 && mm == 30) {     tw1p[m][mm]= 0.115977; } 
	  if(m==66 && mm == 31) {     tw1p[m][mm]= 0.550068; } 
	  if(m==66 && mm == 32) {     tw1p[m][mm]= 1.197577; } 
	  if(m==66 && mm == 33) {     tw1p[m][mm]= 0.142753; } 
	  if(m==66 && mm == 34) {     tw1p[m][mm]= 0.626501; } 
	  if(m==66 && mm == 6) {     tw2p[m][mm]= 0.724551; } 
	  if(m==66 && mm == 7) {     tw2p[m][mm]= 1.448682; } 
	  if(m==66 && mm == 8) {     tw2p[m][mm]= 0.149555; } 
	  if(m==66 && mm == 9) {     tw2p[m][mm]= 0.270042; } 
	  if(m==66 && mm == 10) {     tw2p[m][mm]= 2.054743; } 
	  if(m==66 && mm == 11) {     tw2p[m][mm]= 0.513232; } 
	  if(m==66 && mm == 12) {     tw2p[m][mm]= 0.321365; } 
	  if(m==66 && mm == 14) {     tw2p[m][mm]= 0.300177; } 
	  if(m==66 && mm == 15) {     tw2p[m][mm]= 0.319823; } 
	  if(m==66 && mm == 16) {     tw2p[m][mm]= 0.474145; } 
	  if(m==66 && mm == 17) {     tw2p[m][mm]= 0.659714; } 
	  if(m==66 && mm == 18) {     tw2p[m][mm]= 1.729020; } 
	  if(m==66 && mm == 19) {     tw2p[m][mm]= 0.743335; } 
	  if(m==66 && mm == 20) {     tw2p[m][mm]= 0.300890; } 
	  if(m==66 && mm == 23) {     tw2p[m][mm]= 0.307858; } 
	  if(m==66 && mm == 31) {     tw2p[m][mm]= 0.429787; } 
	  if(m==66 && mm == 32) {     tw2p[m][mm]= 1.167470; } 
	  if(m==66 && mm == 33) {     tw2p[m][mm]= 1.000495; } 
	  if(m==66 && mm == 35) {     tw2p[m][mm]= 1.842293; } 
	  if(m==66 && mm == 6) {     tw3p[m][mm]= 0.683907; } 
	  if(m==66 && mm == 7) {     tw3p[m][mm]= 0.824678; } 
	  if(m==66 && mm == 8) {     tw3p[m][mm]= 1.149753; } 
	  if(m==66 && mm == 9) {     tw3p[m][mm]= 1.072851; } 
	  if(m==66 && mm == 10) {     tw3p[m][mm]= 0.769713; } 
	  if(m==66 && mm == 11) {     tw3p[m][mm]= 0.786724; } 
	  if(m==66 && mm == 12) {     tw3p[m][mm]= 0.086379; } 
	  if(m==66 && mm == 14) {     tw3p[m][mm]= 0.179672; } 
	  if(m==66 && mm == 21) {     tw3p[m][mm]= 0.583019; } 
	  if(m==66 && mm == 28) {     tw3p[m][mm]= 0.379387; } 
	  if(m==66 && mm == 30) {     tw3p[m][mm]= 0.197494; } 
	  if(m==66 && mm == 33) {     tw3p[m][mm]= 1.295784; } 
	  if(m==66 && mm == 34) {     tw3p[m][mm]= 1.009911; } 
	  if(m==66 && mm == 35) {     tw3p[m][mm]= 1.174572; } 
	  if(m==66 && mm == 0) {     te2p[m][mm]= 0.142169; } 
	  if(m==66 && mm == 1) {     te2p[m][mm]= 0.096108; } 
	  if(m==66 && mm == 2) {     te2p[m][mm]= 0.563121; } 
	  if(m==66 && mm == 3) {     te2p[m][mm]= 0.868773; } 
	  if(m==66 && mm == 4) {     te2p[m][mm]= 0.135226; } 
	  if(m==66 && mm == 5) {     te2p[m][mm]= 0.255319; } 
	  if(m==66 && mm == 6) {     te2p[m][mm]= 0.435898; } 
	  if(m==66 && mm == 8) {     te2p[m][mm]= 0.557053; } 
	  if(m==66 && mm == 9) {     te2p[m][mm]= 0.086501; } 
	  if(m==66 && mm == 10) {     te2p[m][mm]= 0.720138; } 
	  if(m==66 && mm == 11) {     te2p[m][mm]= 0.039473; } 
	  if(m==66 && mm == 12) {     te2p[m][mm]= 0.383700; } 
	  if(m==66 && mm == 13) {     te2p[m][mm]= 0.354321; } 
	  if(m==66 && mm == 14) {     te2p[m][mm]= 1.226450; } 
	  if(m==66 && mm == 15) {     te2p[m][mm]= 0.241041; } 
	  if(m==66 && mm == 16) {     te2p[m][mm]= 0.801080; } 
	  if(m==66 && mm == 17) {     te2p[m][mm]= 0.516648; } 
	  if(m==66 && mm == 18) {     te2p[m][mm]= 0.980144; } 
	  if(m==66 && mm == 19) {     te2p[m][mm]= 2.473354; } 
	  if(m==66 && mm == 20) {     te2p[m][mm]= 1.428246; } 
	  if(m==66 && mm == 22) {     te2p[m][mm]= 0.413866; } 
	  if(m==66 && mm == 23) {     te2p[m][mm]= 0.701005; } 
	  if(m==66 && mm == 24) {     te2p[m][mm]= 0.586452; } 
	  if(m==66 && mm == 25) {     te2p[m][mm]= 0.972386; } 
	  if(m==66 && mm == 28) {     te2p[m][mm]= 0.347944; } 
	  if(m==66 && mm == 29) {     te2p[m][mm]= 0.283722; } 
	  if(m==66 && mm == 5) {     te3p[m][mm]= 0.226509; } 
	  if(m==66 && mm == 6) {     te3p[m][mm]= 0.811395; } 
	  if(m==66 && mm == 8) {     te3p[m][mm]= 0.755237; } 
	  if(m==66 && mm == 9) {     te3p[m][mm]= 0.389789; } 
	  if(m==66 && mm == 10) {     te3p[m][mm]= 1.346749; } 
	  if(m==66 && mm == 11) {     te3p[m][mm]= 1.236598; } 
	  if(m==66 && mm == 13) {     te3p[m][mm]= 1.356572; } 
	  if(m==66 && mm == 1) {     tw0n[m][mm]= 0.448631; } 
	  if(m==66 && mm == 3) {     tw0n[m][mm]= 0.604150; } 
	  if(m==66 && mm == 4) {     tw0n[m][mm]= 0.679640; } 
	  if(m==66 && mm == 5) {     tw0n[m][mm]= 0.501066; } 
	  if(m==66 && mm == 6) {     tw0n[m][mm]= 1.367185; } 
	  if(m==66 && mm == 7) {     tw0n[m][mm]= 0.580306; } 
	  if(m==66 && mm == 8) {     tw0n[m][mm]= 0.975440; } 
	  if(m==66 && mm == 9) {     tw0n[m][mm]= 0.088087; } 
	  if(m==66 && mm == 10) {     tw0n[m][mm]= 0.349662; } 
	  if(m==66 && mm == 12) {     tw0n[m][mm]= 0.602573; } 
	  if(m==66 && mm == 14) {     tw0n[m][mm]= 0.110537; } 
	  if(m==66 && mm == 15) {     tw0n[m][mm]= 0.265805; } 
	  if(m==66 && mm == 16) {     tw0n[m][mm]= 0.200200; } 
	  if(m==66 && mm == 17) {     tw0n[m][mm]= 0.449700; } 
	  if(m==66 && mm == 18) {     tw0n[m][mm]= 0.339195; } 
	  if(m==66 && mm == 20) {     tw0n[m][mm]= 0.209381; } 
	  if(m==66 && mm == 22) {     tw0n[m][mm]= 0.813075; } 
	  if(m==66 && mm == 23) {     tw0n[m][mm]= 0.105811; } 
	  if(m==66 && mm == 25) {     tw0n[m][mm]= 0.159856; } 
	  if(m==66 && mm == 27) {     tw0n[m][mm]= 0.126230; } 
	  if(m==66 && mm == 30) {     tw0n[m][mm]= 0.759463; } 
	  if(m==66 && mm == 6) {     tw1n[m][mm]= 1.023814; } 
	  if(m==66 && mm == 9) {     tw1n[m][mm]= 0.702512; } 
	  if(m==66 && mm == 10) {     tw1n[m][mm]= 0.584476; } 
	  if(m==66 && mm == 11) {     tw1n[m][mm]= 0.555718; } 
	  if(m==66 && mm == 12) {     tw1n[m][mm]= 0.228488; } 
	  if(m==66 && mm == 13) {     tw1n[m][mm]= 0.226675; } 
	  if(m==66 && mm == 14) {     tw1n[m][mm]= 0.276884; } 
	  if(m==66 && mm == 16) {     tw1n[m][mm]= 0.410110; } 
	  if(m==66 && mm == 17) {     tw1n[m][mm]= 0.179373; } 
	  if(m==66 && mm == 18) {     tw1n[m][mm]= 1.042561; } 
	  if(m==66 && mm == 20) {     tw1n[m][mm]= 0.281946; } 
	  if(m==66 && mm == 21) {     tw1n[m][mm]= 0.105843; } 
	  if(m==66 && mm == 23) {     tw1n[m][mm]= 1.165911; } 
	  if(m==66 && mm == 30) {     tw1n[m][mm]= 0.107107; } 
	  if(m==66 && mm == 31) {     tw1n[m][mm]= 0.319882; } 
	  if(m==66 && mm == 34) {     tw1n[m][mm]= 0.158898; } 
	  if(m==66 && mm == 0) {     tw2n[m][mm]= 0.152384; } 
	  if(m==66 && mm == 1) {     tw2n[m][mm]= 0.370417; } 
	  if(m==66 && mm == 4) {     tw2n[m][mm]= 0.322997; } 
	  if(m==66 && mm == 5) {     tw2n[m][mm]= 0.270612; } 
	  if(m==66 && mm == 6) {     tw2n[m][mm]= 0.359768; } 
	  if(m==66 && mm == 14) {     tw2n[m][mm]= 0.218724; } 
	  if(m==66 && mm == 15) {     tw2n[m][mm]= 0.053102; } 
	  if(m==66 && mm == 16) {     tw2n[m][mm]= 0.973700; } 
	  if(m==66 && mm == 17) {     tw2n[m][mm]= 0.629936; } 
	  if(m==66 && mm == 18) {     tw2n[m][mm]= 1.571176; } 
	  if(m==66 && mm == 19) {     tw2n[m][mm]= 0.537024; } 
	  if(m==66 && mm == 20) {     tw2n[m][mm]= 0.235380; } 
	  if(m==66 && mm == 21) {     tw2n[m][mm]= 0.713633; } 
	  if(m==66 && mm == 23) {     tw2n[m][mm]= 0.442811; } 
	  if(m==66 && mm == 25) {     tw2n[m][mm]= 0.002573; } 
	  if(m==66 && mm == 26) {     tw2n[m][mm]= 0.075291; } 
	  if(m==66 && mm == 28) {     tw2n[m][mm]= 0.540681; } 
	  if(m==66 && mm == 29) {     tw2n[m][mm]= 0.236342; } 
	  if(m==66 && mm == 30) {     tw2n[m][mm]= 0.018969; } 
	  if(m==66 && mm == 31) {     tw2n[m][mm]= 0.450473; } 
	  if(m==66 && mm == 32) {     tw2n[m][mm]= 1.255841; } 
	  if(m==66 && mm == 33) {     tw2n[m][mm]= 1.021499; } 
	  if(m==66 && mm == 6) {     tw3n[m][mm]= 0.315487; } 
	  if(m==66 && mm == 7) {     tw3n[m][mm]= 0.663432; } 
	  if(m==66 && mm == 8) {     tw3n[m][mm]= 1.326876; } 
	  if(m==66 && mm == 9) {     tw3n[m][mm]= 1.527226; } 
	  if(m==66 && mm == 10) {     tw3n[m][mm]= 0.271717; } 
	  if(m==66 && mm == 11) {     tw3n[m][mm]= 0.758684; } 
	  if(m==66 && mm == 12) {     tw3n[m][mm]= 1.405166; } 
	  if(m==66 && mm == 21) {     tw3n[m][mm]= 0.556382; } 
	  if(m==66 && mm == 6) {     te2n[m][mm]= 0.386172; } 
	  if(m==66 && mm == 10) {     te2n[m][mm]= 0.435496; } 
	  if(m==66 && mm == 11) {     te2n[m][mm]= 0.098297; } 
	  if(m==66 && mm == 13) {     te2n[m][mm]= 0.365908; } 
	  if(m==66 && mm == 14) {     te2n[m][mm]= 0.593012; } 
	  if(m==66 && mm == 15) {     te2n[m][mm]= 0.042998; } 
	  if(m==66 && mm == 16) {     te2n[m][mm]= 0.829836; } 
	  if(m==66 && mm == 17) {     te2n[m][mm]= 0.603311; } 
	  if(m==66 && mm == 18) {     te2n[m][mm]= 0.908152; } 
	  if(m==66 && mm == 19) {     te2n[m][mm]= 2.333859; } 
	  if(m==66 && mm == 20) {     te2n[m][mm]= 1.744821; } 
	  if(m==66 && mm == 22) {     te2n[m][mm]= 0.428565; } 
	  if(m==66 && mm == 24) {     te2n[m][mm]= 0.895440; } 
	  if(m==66 && mm == 25) {     te2n[m][mm]= 1.369224; } 
	  if(m==66 && mm == 28) {     te2n[m][mm]= 0.304197; } 
	  if(m==66 && mm == 29) {     te2n[m][mm]= 0.556405; } 
	  if(m==66 && mm == 30) {     te2n[m][mm]= 2.092272; } 
	  if(m==66 && mm == 31) {     te2n[m][mm]= 1.070500; } 
	  if(m==66 && mm == 34) {     te2n[m][mm]= 0.774680; } 
	  if(m==66 && mm == 35) {     te2n[m][mm]= 0.435665; } 
	  if(m==66 && mm == 0) {     te3n[m][mm]= 1.605884; } 
	  if(m==66 && mm == 2) {     te3n[m][mm]= 0.896189; } 
	  if(m==66 && mm == 3) {     te3n[m][mm]= 0.928114; } 
	  if(m==66 && mm == 4) {     te3n[m][mm]= 0.108987; } 
	  if(m==66 && mm == 5) {     te3n[m][mm]= 0.236966; } 
	  if(m==66 && mm == 6) {     te3n[m][mm]= 0.899787; } 
	  if(m==66 && mm == 20) {     te3n[m][mm]= 0.012932; } 
	  if(m==66 && mm == 24) {     te3n[m][mm]= 0.742157; } 
	  if(m==66 && mm == 25) {     te3n[m][mm]= 0.430608; } 
	  if(m==66 && mm == 26) {     te3n[m][mm]= 0.323977; } 
	  if(m==66 && mm == 27) {     te3n[m][mm]= 1.076241; } 
	  if(m==66 && mm == 29) {     te3n[m][mm]= 0.488917; } 
	  if(m==66 && mm == 30) {     te3n[m][mm]= 0.146183; } 
	  if(m==66 && mm == 31) {     te3n[m][mm]= 0.953067; } 
	  if(m==66 && mm == 32) {     te3n[m][mm]= 0.140015; } 
	  if(m==66 && mm == 33) {     te3n[m][mm]= 0.179225; } 
	  if(m==67 && mm == 7) {     tw0p[m][mm]= 1.345125; } 
	  if(m==67 && mm == 9) {     tw0p[m][mm]= 2.168622; } 
	  if(m==67 && mm == 10) {     tw0p[m][mm]= 0.746903; } 
	  if(m==67 && mm == 11) {     tw0p[m][mm]= 1.529678; } 
	  if(m==67 && mm == 12) {     tw0p[m][mm]= 1.015267; } 
	  if(m==67 && mm == 13) {     tw0p[m][mm]= 0.217219; } 
	  if(m==67 && mm == 17) {     tw0p[m][mm]= 0.058172; } 
	  if(m==67 && mm == 19) {     tw0p[m][mm]= 1.060336; } 
	  if(m==67 && mm == 20) {     tw0p[m][mm]= 0.791528; } 
	  if(m==67 && mm == 22) {     tw0p[m][mm]= 1.153343; } 
	  if(m==67 && mm == 23) {     tw0p[m][mm]= 0.789337; } 
	  if(m==67 && mm == 24) {     tw0p[m][mm]= 0.745616; } 
	  if(m==67 && mm == 27) {     tw0p[m][mm]= 0.125631; } 
	  if(m==67 && mm == 29) {     tw0p[m][mm]= 0.412922; } 
	  if(m==67 && mm == 30) {     tw0p[m][mm]= 0.155068; } 
	  if(m==67 && mm == 31) {     tw0p[m][mm]= 0.327500; } 
	  if(m==67 && mm == 32) {     tw0p[m][mm]= 0.398887; } 
	  if(m==67 && mm == 33) {     tw0p[m][mm]= 0.500813; } 
	  if(m==67 && mm == 34) {     tw0p[m][mm]= 0.255522; } 
	  if(m==67 && mm == 35) {     tw0p[m][mm]= 0.121215; } 
	  if(m==67 && mm == 0) {     tw1p[m][mm]= 0.073706; } 
	  if(m==67 && mm == 1) {     tw1p[m][mm]= 0.493275; } 
	  if(m==67 && mm == 5) {     tw1p[m][mm]= 0.096305; } 
	  if(m==67 && mm == 7) {     tw1p[m][mm]= 0.443016; } 
	  if(m==67 && mm == 8) {     tw1p[m][mm]= 0.107540; } 
	  if(m==67 && mm == 19) {     tw1p[m][mm]= 0.069576; } 
	  if(m==67 && mm == 21) {     tw1p[m][mm]= 0.127139; } 
	  if(m==67 && mm == 22) {     tw1p[m][mm]= 0.541624; } 
	  if(m==67 && mm == 23) {     tw1p[m][mm]= 1.885883; } 
	  if(m==67 && mm == 6) {     tw2p[m][mm]= 0.269223; } 
	  if(m==67 && mm == 7) {     tw2p[m][mm]= 0.167505; } 
	  if(m==67 && mm == 9) {     tw2p[m][mm]= 0.002196; } 
	  if(m==67 && mm == 10) {     tw2p[m][mm]= 0.554408; } 
	  if(m==67 && mm == 12) {     tw2p[m][mm]= 0.826669; } 
	  if(m==67 && mm == 13) {     tw2p[m][mm]= 0.472387; } 
	  if(m==67 && mm == 14) {     tw2p[m][mm]= 0.012893; } 
	  if(m==67 && mm == 15) {     tw2p[m][mm]= 0.130541; } 
	  if(m==67 && mm == 16) {     tw2p[m][mm]= 0.509527; } 
	  if(m==67 && mm == 17) {     tw2p[m][mm]= 0.922316; } 
	  if(m==67 && mm == 18) {     tw2p[m][mm]= 0.719891; } 
	  if(m==67 && mm == 19) {     tw2p[m][mm]= 1.242313; } 
	  if(m==67 && mm == 20) {     tw2p[m][mm]= 0.502201; } 
	  if(m==67 && mm == 21) {     tw2p[m][mm]= 0.345218; } 
	  if(m==67 && mm == 22) {     tw2p[m][mm]= 0.129061; } 
	  if(m==67 && mm == 29) {     tw2p[m][mm]= 0.309627; } 
	  if(m==67 && mm == 30) {     tw2p[m][mm]= 1.822675; } 
	  if(m==67 && mm == 31) {     tw2p[m][mm]= 2.244728; } 
	  if(m==67 && mm == 32) {     tw2p[m][mm]= 2.113463; } 
	  if(m==67 && mm == 3) {     tw3p[m][mm]= 0.014173; } 
	  if(m==67 && mm == 7) {     tw3p[m][mm]= 0.093345; } 
	  if(m==67 && mm == 8) {     tw3p[m][mm]= 0.975789; } 
	  if(m==67 && mm == 9) {     tw3p[m][mm]= 1.762663; } 
	  if(m==67 && mm == 10) {     tw3p[m][mm]= 0.547626; } 
	  if(m==67 && mm == 11) {     tw3p[m][mm]= 0.820193; } 
	  if(m==67 && mm == 15) {     tw3p[m][mm]= 0.147343; } 
	  if(m==67 && mm == 16) {     tw3p[m][mm]= 0.708642; } 
	  if(m==67 && mm == 19) {     tw3p[m][mm]= 0.907142; } 
	  if(m==67 && mm == 20) {     tw3p[m][mm]= 0.292626; } 
	  if(m==67 && mm == 28) {     tw3p[m][mm]= 0.506262; } 
	  if(m==67 && mm == 29) {     tw3p[m][mm]= 0.482262; } 
	  if(m==67 && mm == 30) {     tw3p[m][mm]= 0.534184; } 
	  if(m==67 && mm == 31) {     tw3p[m][mm]= 0.215036; } 
	  if(m==67 && mm == 32) {     tw3p[m][mm]= 1.768842; } 
	  if(m==67 && mm == 33) {     tw3p[m][mm]= 0.859143; } 
	  if(m==67 && mm == 34) {     tw3p[m][mm]= 1.285127; } 
	  if(m==67 && mm == 35) {     tw3p[m][mm]= 0.815861; } 
	  if(m==67 && mm == 1) {     te2p[m][mm]= 0.005958; } 
	  if(m==67 && mm == 3) {     te2p[m][mm]= 0.500203; } 
	  if(m==67 && mm == 5) {     te2p[m][mm]= 0.674050; } 
	  if(m==67 && mm == 9) {     te2p[m][mm]= 0.327587; } 
	  if(m==67 && mm == 13) {     te2p[m][mm]= 0.170093; } 
	  if(m==67 && mm == 14) {     te2p[m][mm]= 0.542340; } 
	  if(m==67 && mm == 16) {     te2p[m][mm]= 0.317870; } 
	  if(m==67 && mm == 17) {     te2p[m][mm]= 0.396634; } 
	  if(m==67 && mm == 18) {     te2p[m][mm]= 1.579380; } 
	  if(m==67 && mm == 19) {     te2p[m][mm]= 0.912367; } 
	  if(m==67 && mm == 20) {     te2p[m][mm]= 1.033945; } 
	  if(m==67 && mm == 21) {     te2p[m][mm]= 3.514863; } 
	  if(m==67 && mm == 22) {     te2p[m][mm]= 0.486465; } 
	  if(m==67 && mm == 23) {     te2p[m][mm]= 0.444830; } 
	  if(m==67 && mm == 24) {     te2p[m][mm]= 1.305056; } 
	  if(m==67 && mm == 28) {     te2p[m][mm]= 0.791155; } 
	  if(m==67 && mm == 29) {     te2p[m][mm]= 0.079222; } 
	  if(m==67 && mm == 7) {     te3p[m][mm]= 0.372685; } 
	  if(m==67 && mm == 8) {     te3p[m][mm]= 0.869433; } 
	  if(m==67 && mm == 9) {     te3p[m][mm]= 0.936321; } 
	  if(m==67 && mm == 10) {     te3p[m][mm]= 0.525107; } 
	  if(m==67 && mm == 11) {     te3p[m][mm]= 0.286856; } 
	  if(m==67 && mm == 13) {     te3p[m][mm]= 2.248847; } 
	  if(m==67 && mm == 16) {     te3p[m][mm]= 0.629574; } 
	  if(m==67 && mm == 0) {     tw0n[m][mm]= 0.928110; } 
	  if(m==67 && mm == 2) {     tw0n[m][mm]= 1.423117; } 
	  if(m==67 && mm == 3) {     tw0n[m][mm]= 1.109975; } 
	  if(m==67 && mm == 4) {     tw0n[m][mm]= 0.673924; } 
	  if(m==67 && mm == 5) {     tw0n[m][mm]= 0.613100; } 
	  if(m==67 && mm == 6) {     tw0n[m][mm]= 1.079496; } 
	  if(m==67 && mm == 7) {     tw0n[m][mm]= 1.242492; } 
	  if(m==67 && mm == 8) {     tw0n[m][mm]= 0.283570; } 
	  if(m==67 && mm == 9) {     tw0n[m][mm]= 1.018901; } 
	  if(m==67 && mm == 10) {     tw0n[m][mm]= 0.653802; } 
	  if(m==67 && mm == 11) {     tw0n[m][mm]= 2.238562; } 
	  if(m==67 && mm == 12) {     tw0n[m][mm]= 0.545316; } 
	  if(m==67 && mm == 13) {     tw0n[m][mm]= 0.531210; } 
	  if(m==67 && mm == 15) {     tw0n[m][mm]= 0.089875; } 
	  if(m==67 && mm == 17) {     tw0n[m][mm]= 0.024319; } 
	  if(m==67 && mm == 19) {     tw0n[m][mm]= 1.332276; } 
	  if(m==67 && mm == 20) {     tw0n[m][mm]= 0.845329; } 
	  if(m==67 && mm == 22) {     tw0n[m][mm]= 0.595338; } 
	  if(m==67 && mm == 23) {     tw0n[m][mm]= 0.230394; } 
	  if(m==67 && mm == 24) {     tw0n[m][mm]= 0.836923; } 
	  if(m==67 && mm == 7) {     tw1n[m][mm]= 0.708520; } 
	  if(m==67 && mm == 8) {     tw1n[m][mm]= 0.072462; } 
	  if(m==67 && mm == 9) {     tw1n[m][mm]= 0.691980; } 
	  if(m==67 && mm == 14) {     tw1n[m][mm]= 0.077097; } 
	  if(m==67 && mm == 19) {     tw1n[m][mm]= 0.095162; } 
	  if(m==67 && mm == 21) {     tw1n[m][mm]= 0.198884; } 
	  if(m==67 && mm == 22) {     tw1n[m][mm]= 0.901260; } 
	  if(m==67 && mm == 4) {     tw2n[m][mm]= 0.596558; } 
	  if(m==67 && mm == 5) {     tw2n[m][mm]= 1.038481; } 
	  if(m==67 && mm == 14) {     tw2n[m][mm]= 0.047161; } 
	  if(m==67 && mm == 15) {     tw2n[m][mm]= 0.123684; } 
	  if(m==67 && mm == 16) {     tw2n[m][mm]= 0.652403; } 
	  if(m==67 && mm == 17) {     tw2n[m][mm]= 1.040667; } 
	  if(m==67 && mm == 18) {     tw2n[m][mm]= 1.842080; } 
	  if(m==67 && mm == 19) {     tw2n[m][mm]= 0.148409; } 
	  if(m==67 && mm == 20) {     tw2n[m][mm]= 0.326325; } 
	  if(m==67 && mm == 21) {     tw2n[m][mm]= 0.606176; } 
	  if(m==67 && mm == 23) {     tw2n[m][mm]= 0.243973; } 
	  if(m==67 && mm == 24) {     tw2n[m][mm]= 0.387344; } 
	  if(m==67 && mm == 25) {     tw2n[m][mm]= 0.415658; } 
	  if(m==67 && mm == 27) {     tw2n[m][mm]= 0.035202; } 
	  if(m==67 && mm == 29) {     tw2n[m][mm]= 0.457511; } 
	  if(m==67 && mm == 30) {     tw2n[m][mm]= 0.641328; } 
	  if(m==67 && mm == 31) {     tw2n[m][mm]= 0.928701; } 
	  if(m==67 && mm == 33) {     tw2n[m][mm]= 0.758042; } 
	  if(m==67 && mm == 3) {     tw3n[m][mm]= 0.451851; } 
	  if(m==67 && mm == 7) {     tw3n[m][mm]= 0.125168; } 
	  if(m==67 && mm == 8) {     tw3n[m][mm]= 0.073885; } 
	  if(m==67 && mm == 10) {     tw3n[m][mm]= 0.560249; } 
	  if(m==67 && mm == 11) {     tw3n[m][mm]= 0.440991; } 
	  if(m==67 && mm == 15) {     tw3n[m][mm]= 0.246974; } 
	  if(m==67 && mm == 16) {     tw3n[m][mm]= 0.363889; } 
	  if(m==67 && mm == 19) {     tw3n[m][mm]= 1.026339; } 
	  if(m==67 && mm == 20) {     tw3n[m][mm]= 0.075867; } 
	  if(m==67 && mm == 28) {     tw3n[m][mm]= 0.122092; } 
	  if(m==67 && mm == 5) {     te2n[m][mm]= 0.765459; } 
	  if(m==67 && mm == 8) {     te2n[m][mm]= 0.751135; } 
	  if(m==67 && mm == 9) {     te2n[m][mm]= 0.003819; } 
	  if(m==67 && mm == 10) {     te2n[m][mm]= 0.056304; } 
	  if(m==67 && mm == 12) {     te2n[m][mm]= 0.151182; } 
	  if(m==67 && mm == 14) {     te2n[m][mm]= 0.535087; } 
	  if(m==67 && mm == 16) {     te2n[m][mm]= 0.404183; } 
	  if(m==67 && mm == 17) {     te2n[m][mm]= 0.379408; } 
	  if(m==67 && mm == 18) {     te2n[m][mm]= 1.318325; } 
	  if(m==67 && mm == 20) {     te2n[m][mm]= 0.840192; } 
	  if(m==67 && mm == 21) {     te2n[m][mm]= 3.422219; } 
	  if(m==67 && mm == 22) {     te2n[m][mm]= 0.641880; } 
	  if(m==67 && mm == 23) {     te2n[m][mm]= 0.428005; } 
	  if(m==67 && mm == 24) {     te2n[m][mm]= 1.279474; } 
	  if(m==67 && mm == 28) {     te2n[m][mm]= 0.906419; } 
	  if(m==67 && mm == 29) {     te2n[m][mm]= 1.144316; } 
	  if(m==67 && mm == 30) {     te2n[m][mm]= 1.839306; } 
	  if(m==67 && mm == 31) {     te2n[m][mm]= 0.147993; } 
	  if(m==67 && mm == 32) {     te2n[m][mm]= 2.040032; } 
	  if(m==67 && mm == 33) {     te2n[m][mm]= 0.310492; } 
	  if(m==67 && mm == 35) {     te2n[m][mm]= 0.127660; } 
	  if(m==67 && mm == 2) {     te3n[m][mm]= 2.181232; } 
	  if(m==67 && mm == 3) {     te3n[m][mm]= 0.490191; } 
	  if(m==67 && mm == 4) {     te3n[m][mm]= 0.338673; } 
	  if(m==67 && mm == 24) {     te3n[m][mm]= 0.100273; } 
	  if(m==67 && mm == 25) {     te3n[m][mm]= 0.547404; } 
	  if(m==67 && mm == 26) {     te3n[m][mm]= 1.186984; } 
	  if(m==67 && mm == 27) {     te3n[m][mm]= 0.033260; } 
	  if(m==67 && mm == 28) {     te3n[m][mm]= 0.082527; } 
	  if(m==67 && mm == 29) {     te3n[m][mm]= 0.147822; } 
	  if(m==67 && mm == 30) {     te3n[m][mm]= 0.167570; } 
	  if(m==67 && mm == 31) {     te3n[m][mm]= 0.112650; } 
	  if(m==67 && mm == 32) {     te3n[m][mm]= 0.539308; } 
	  if(m==67 && mm == 33) {     te3n[m][mm]= 0.255369; } 
	  if(m==67 && mm == 34) {     te3n[m][mm]= 0.472394; } 
	  if(m==67 && mm == 35) {     te3n[m][mm]= 1.046553; } 
	  if(m==68 && mm == 7) {     tw0p[m][mm]= 0.274356; } 
	  if(m==68 && mm == 8) {     tw0p[m][mm]= 0.346433; } 
	  if(m==68 && mm == 9) {     tw0p[m][mm]= 1.125068; } 
	  if(m==68 && mm == 10) {     tw0p[m][mm]= 0.588903; } 
	  if(m==68 && mm == 12) {     tw0p[m][mm]= 0.058613; } 
	  if(m==68 && mm == 14) {     tw0p[m][mm]= 0.520925; } 
	  if(m==68 && mm == 15) {     tw0p[m][mm]= 0.219544; } 
	  if(m==68 && mm == 16) {     tw0p[m][mm]= 0.564286; } 
	  if(m==68 && mm == 18) {     tw0p[m][mm]= 1.084569; } 
	  if(m==68 && mm == 19) {     tw0p[m][mm]= 0.465016; } 
	  if(m==68 && mm == 21) {     tw0p[m][mm]= 0.642056; } 
	  if(m==68 && mm == 22) {     tw0p[m][mm]= 1.776927; } 
	  if(m==68 && mm == 23) {     tw0p[m][mm]= 0.732978; } 
	  if(m==68 && mm == 24) {     tw0p[m][mm]= 0.107153; } 
	  if(m==68 && mm == 26) {     tw0p[m][mm]= 0.463045; } 
	  if(m==68 && mm == 27) {     tw0p[m][mm]= 0.691558; } 
	  if(m==68 && mm == 28) {     tw0p[m][mm]= 0.779255; } 
	  if(m==68 && mm == 30) {     tw0p[m][mm]= 0.478205; } 
	  if(m==68 && mm == 31) {     tw0p[m][mm]= 0.390910; } 
	  if(m==68 && mm == 32) {     tw0p[m][mm]= 0.069881; } 
	  if(m==68 && mm == 33) {     tw0p[m][mm]= 0.647515; } 
	  if(m==68 && mm == 34) {     tw0p[m][mm]= 0.162235; } 
	  if(m==68 && mm == 35) {     tw0p[m][mm]= 1.343853; } 
	  if(m==68 && mm == 0) {     tw1p[m][mm]= 0.254298; } 
	  if(m==68 && mm == 3) {     tw1p[m][mm]= 0.413981; } 
	  if(m==68 && mm == 7) {     tw1p[m][mm]= 0.358202; } 
	  if(m==68 && mm == 8) {     tw1p[m][mm]= 0.868429; } 
	  if(m==68 && mm == 21) {     tw1p[m][mm]= 0.308591; } 
	  if(m==68 && mm == 22) {     tw1p[m][mm]= 1.501964; } 
	  if(m==68 && mm == 24) {     tw1p[m][mm]= 0.321454; } 
	  if(m==68 && mm == 26) {     tw1p[m][mm]= 0.564888; } 
	  if(m==68 && mm == 27) {     tw1p[m][mm]= 0.695185; } 
	  if(m==68 && mm == 29) {     tw1p[m][mm]= 0.070804; } 
	  if(m==68 && mm == 30) {     tw1p[m][mm]= 0.179020; } 
	  if(m==68 && mm == 31) {     tw1p[m][mm]= 0.435704; } 
	  if(m==68 && mm == 34) {     tw1p[m][mm]= 0.615075; } 
	  if(m==68 && mm == 35) {     tw1p[m][mm]= 0.042865; } 
	  if(m==68 && mm == 7) {     tw2p[m][mm]= 0.456764; } 
	  if(m==68 && mm == 8) {     tw2p[m][mm]= 0.573280; } 
	  if(m==68 && mm == 9) {     tw2p[m][mm]= 0.517142; } 
	  if(m==68 && mm == 10) {     tw2p[m][mm]= 0.782078; } 
	  if(m==68 && mm == 11) {     tw2p[m][mm]= 0.532502; } 
	  if(m==68 && mm == 12) {     tw2p[m][mm]= 0.523631; } 
	  if(m==68 && mm == 14) {     tw2p[m][mm]= 0.442686; } 
	  if(m==68 && mm == 15) {     tw2p[m][mm]= 0.144066; } 
	  if(m==68 && mm == 16) {     tw2p[m][mm]= 1.168310; } 
	  if(m==68 && mm == 18) {     tw2p[m][mm]= 0.459660; } 
	  if(m==68 && mm == 19) {     tw2p[m][mm]= 0.663519; } 
	  if(m==68 && mm == 20) {     tw2p[m][mm]= 0.668488; } 
	  if(m==68 && mm == 21) {     tw2p[m][mm]= 0.534859; } 
	  if(m==68 && mm == 31) {     tw2p[m][mm]= 0.518233; } 
	  if(m==68 && mm == 32) {     tw2p[m][mm]= 0.430559; } 
	  if(m==68 && mm == 33) {     tw2p[m][mm]= 0.611123; } 
	  if(m==68 && mm == 35) {     tw2p[m][mm]= 1.799618; } 
	  if(m==68 && mm == 0) {     tw3p[m][mm]= 0.656136; } 
	  if(m==68 && mm == 1) {     tw3p[m][mm]= 0.018835; } 
	  if(m==68 && mm == 2) {     tw3p[m][mm]= 0.730413; } 
	  if(m==68 && mm == 3) {     tw3p[m][mm]= 1.303664; } 
	  if(m==68 && mm == 6) {     tw3p[m][mm]= 0.605965; } 
	  if(m==68 && mm == 7) {     tw3p[m][mm]= 1.192776; } 
	  if(m==68 && mm == 8) {     tw3p[m][mm]= 0.767959; } 
	  if(m==68 && mm == 9) {     tw3p[m][mm]= 0.304088; } 
	  if(m==68 && mm == 10) {     tw3p[m][mm]= 1.391100; } 
	  if(m==68 && mm == 11) {     tw3p[m][mm]= 1.309521; } 
	  if(m==68 && mm == 12) {     tw3p[m][mm]= 0.099725; } 
	  if(m==68 && mm == 16) {     tw3p[m][mm]= 0.518915; } 
	  if(m==68 && mm == 18) {     tw3p[m][mm]= 2.345162; } 
	  if(m==68 && mm == 19) {     tw3p[m][mm]= 0.806202; } 
	  if(m==68 && mm == 22) {     tw3p[m][mm]= 1.244485; } 
	  if(m==68 && mm == 23) {     tw3p[m][mm]= 0.264806; } 
	  if(m==68 && mm == 30) {     tw3p[m][mm]= 0.671108; } 
	  if(m==68 && mm == 32) {     tw3p[m][mm]= 1.355339; } 
	  if(m==68 && mm == 33) {     tw3p[m][mm]= 1.261841; } 
	  if(m==68 && mm == 34) {     tw3p[m][mm]= 1.541772; } 
	  if(m==68 && mm == 35) {     tw3p[m][mm]= 1.673418; } 
	  if(m==68 && mm == 1) {     te2p[m][mm]= 0.607728; } 
	  if(m==68 && mm == 2) {     te2p[m][mm]= 0.222194; } 
	  if(m==68 && mm == 6) {     te2p[m][mm]= 0.154271; } 
	  if(m==68 && mm == 7) {     te2p[m][mm]= 1.853006; } 
	  if(m==68 && mm == 9) {     te2p[m][mm]= 0.533184; } 
	  if(m==68 && mm == 10) {     te2p[m][mm]= 0.690019; } 
	  if(m==68 && mm == 11) {     te2p[m][mm]= 1.172054; } 
	  if(m==68 && mm == 12) {     te2p[m][mm]= 1.414448; } 
	  if(m==68 && mm == 14) {     te2p[m][mm]= 1.840229; } 
	  if(m==68 && mm == 15) {     te2p[m][mm]= 3.629233; } 
	  if(m==68 && mm == 16) {     te2p[m][mm]= 2.109571; } 
	  if(m==68 && mm == 17) {     te2p[m][mm]= 2.971308; } 
	  if(m==68 && mm == 18) {     te2p[m][mm]= 4.377928; } 
	  if(m==68 && mm == 19) {     te2p[m][mm]= 3.792689; } 
	  if(m==68 && mm == 20) {     te2p[m][mm]= 1.333084; } 
	  if(m==68 && mm == 22) {     te2p[m][mm]= 2.268478; } 
	  if(m==68 && mm == 23) {     te2p[m][mm]= 1.335931; } 
	  if(m==68 && mm == 24) {     te2p[m][mm]= 1.743950; } 
	  if(m==68 && mm == 25) {     te2p[m][mm]= 1.054302; } 
	  if(m==68 && mm == 28) {     te2p[m][mm]= 0.147563; } 
	  if(m==68 && mm == 29) {     te2p[m][mm]= 0.220472; } 
	  if(m==68 && mm == 7) {     te3p[m][mm]= 0.670937; } 
	  if(m==68 && mm == 8) {     te3p[m][mm]= 0.908255; } 
	  if(m==68 && mm == 10) {     te3p[m][mm]= 0.453086; } 
	  if(m==68 && mm == 14) {     te3p[m][mm]= 0.111077; } 
	  if(m==68 && mm == 15) {     te3p[m][mm]= 1.249886; } 
	  if(m==68 && mm == 17) {     te3p[m][mm]= 0.467263; } 
	  if(m==68 && mm == 18) {     te3p[m][mm]= 0.182113; } 
	  if(m==68 && mm == 20) {     te3p[m][mm]= 0.002964; } 
	  if(m==68 && mm == 21) {     te3p[m][mm]= 0.294147; } 
	  if(m==68 && mm == 0) {     tw0n[m][mm]= 2.373702; } 
	  if(m==68 && mm == 1) {     tw0n[m][mm]= 0.721154; } 
	  if(m==68 && mm == 3) {     tw0n[m][mm]= 1.017523; } 
	  if(m==68 && mm == 5) {     tw0n[m][mm]= 0.770510; } 
	  if(m==68 && mm == 6) {     tw0n[m][mm]= 0.536603; } 
	  if(m==68 && mm == 7) {     tw0n[m][mm]= 0.315533; } 
	  if(m==68 && mm == 8) {     tw0n[m][mm]= 0.338378; } 
	  if(m==68 && mm == 9) {     tw0n[m][mm]= 1.155030; } 
	  if(m==68 && mm == 10) {     tw0n[m][mm]= 0.284815; } 
	  if(m==68 && mm == 14) {     tw0n[m][mm]= 0.176821; } 
	  if(m==68 && mm == 15) {     tw0n[m][mm]= 0.166214; } 
	  if(m==68 && mm == 16) {     tw0n[m][mm]= 0.194607; } 
	  if(m==68 && mm == 17) {     tw0n[m][mm]= 0.289991; } 
	  if(m==68 && mm == 18) {     tw0n[m][mm]= 1.115572; } 
	  if(m==68 && mm == 19) {     tw0n[m][mm]= 0.723431; } 
	  if(m==68 && mm == 20) {     tw0n[m][mm]= 0.062169; } 
	  if(m==68 && mm == 21) {     tw0n[m][mm]= 0.596513; } 
	  if(m==68 && mm == 22) {     tw0n[m][mm]= 1.393490; } 
	  if(m==68 && mm == 23) {     tw0n[m][mm]= 0.751854; } 
	  if(m==68 && mm == 26) {     tw0n[m][mm]= 0.428883; } 
	  if(m==68 && mm == 27) {     tw0n[m][mm]= 0.014982; } 
	  if(m==68 && mm == 28) {     tw0n[m][mm]= 0.817674; } 
	  if(m==68 && mm == 8) {     tw1n[m][mm]= 1.163528; } 
	  if(m==68 && mm == 9) {     tw1n[m][mm]= 0.929862; } 
	  if(m==68 && mm == 11) {     tw1n[m][mm]= 0.705741; } 
	  if(m==68 && mm == 12) {     tw1n[m][mm]= 0.224992; } 
	  if(m==68 && mm == 14) {     tw1n[m][mm]= 0.052914; } 
	  if(m==68 && mm == 16) {     tw1n[m][mm]= 0.812169; } 
	  if(m==68 && mm == 17) {     tw1n[m][mm]= 0.259999; } 
	  if(m==68 && mm == 19) {     tw1n[m][mm]= 0.108882; } 
	  if(m==68 && mm == 21) {     tw1n[m][mm]= 0.288158; } 
	  if(m==68 && mm == 22) {     tw1n[m][mm]= 1.526689; } 
	  if(m==68 && mm == 30) {     tw1n[m][mm]= 0.188340; } 
	  if(m==68 && mm == 31) {     tw1n[m][mm]= 0.350914; } 
	  if(m==68 && mm == 14) {     tw2n[m][mm]= 0.524199; } 
	  if(m==68 && mm == 16) {     tw2n[m][mm]= 1.296208; } 
	  if(m==68 && mm == 18) {     tw2n[m][mm]= 0.569497; } 
	  if(m==68 && mm == 19) {     tw2n[m][mm]= 0.527406; } 
	  if(m==68 && mm == 20) {     tw2n[m][mm]= 1.058631; } 
	  if(m==68 && mm == 21) {     tw2n[m][mm]= 0.369454; } 
	  if(m==68 && mm == 22) {     tw2n[m][mm]= 1.925696; } 
	  if(m==68 && mm == 24) {     tw2n[m][mm]= 1.464223; } 
	  if(m==68 && mm == 25) {     tw2n[m][mm]= 0.951924; } 
	  if(m==68 && mm == 26) {     tw2n[m][mm]= 0.669433; } 
	  if(m==68 && mm == 27) {     tw2n[m][mm]= 1.318192; } 
	  if(m==68 && mm == 28) {     tw2n[m][mm]= 0.496477; } 
	  if(m==68 && mm == 30) {     tw2n[m][mm]= 1.083005; } 
	  if(m==68 && mm == 31) {     tw2n[m][mm]= 0.315715; } 
	  if(m==68 && mm == 32) {     tw2n[m][mm]= 0.790152; } 
	  if(m==68 && mm == 33) {     tw2n[m][mm]= 0.385712; } 
	  if(m==68 && mm == 0) {     tw3n[m][mm]= 0.681627; } 
	  if(m==68 && mm == 1) {     tw3n[m][mm]= 0.071361; } 
	  if(m==68 && mm == 2) {     tw3n[m][mm]= 0.440494; } 
	  if(m==68 && mm == 3) {     tw3n[m][mm]= 1.182856; } 
	  if(m==68 && mm == 6) {     tw3n[m][mm]= 0.620851; } 
	  if(m==68 && mm == 7) {     tw3n[m][mm]= 0.684296; } 
	  if(m==68 && mm == 8) {     tw3n[m][mm]= 0.935776; } 
	  if(m==68 && mm == 9) {     tw3n[m][mm]= 0.290669; } 
	  if(m==68 && mm == 10) {     tw3n[m][mm]= 1.754562; } 
	  if(m==68 && mm == 11) {     tw3n[m][mm]= 1.322403; } 
	  if(m==68 && mm == 16) {     tw3n[m][mm]= 0.433331; } 
	  if(m==68 && mm == 18) {     tw3n[m][mm]= 1.759598; } 
	  if(m==68 && mm == 19) {     tw3n[m][mm]= 0.739271; } 
	  if(m==68 && mm == 22) {     tw3n[m][mm]= 1.029164; } 
	  if(m==68 && mm == 23) {     tw3n[m][mm]= 0.106114; } 
	  if(m==68 && mm == 6) {     te2n[m][mm]= 0.651750; } 
	  if(m==68 && mm == 7) {     te2n[m][mm]= 1.443291; } 
	  if(m==68 && mm == 9) {     te2n[m][mm]= 0.431000; } 
	  if(m==68 && mm == 10) {     te2n[m][mm]= 0.345548; } 
	  if(m==68 && mm == 12) {     te2n[m][mm]= 1.272532; } 
	  if(m==68 && mm == 13) {     te2n[m][mm]= 1.854147; } 
	  if(m==68 && mm == 14) {     te2n[m][mm]= 4.803788; } 
	  if(m==68 && mm == 15) {     te2n[m][mm]= 2.409581; } 
	  if(m==68 && mm == 16) {     te2n[m][mm]= 2.064836; } 
	  if(m==68 && mm == 17) {     te2n[m][mm]= 1.199910; } 
	  if(m==68 && mm == 18) {     te2n[m][mm]= 5.366474; } 
	  if(m==68 && mm == 19) {     te2n[m][mm]= 3.493110; } 
	  if(m==68 && mm == 20) {     te2n[m][mm]= 1.514063; } 
	  if(m==68 && mm == 21) {     te2n[m][mm]= 4.313274; } 
	  if(m==68 && mm == 22) {     te2n[m][mm]= 2.411304; } 
	  if(m==68 && mm == 23) {     te2n[m][mm]= 1.791828; } 
	  if(m==68 && mm == 24) {     te2n[m][mm]= 1.223601; } 
	  if(m==68 && mm == 25) {     te2n[m][mm]= 1.602671; } 
	  if(m==68 && mm == 28) {     te2n[m][mm]= 0.177207; } 
	  if(m==68 && mm == 29) {     te2n[m][mm]= 0.281724; } 
	  if(m==68 && mm == 32) {     te2n[m][mm]= 0.768491; } 
	  if(m==68 && mm == 0) {     te3n[m][mm]= 0.280490; } 
	  if(m==68 && mm == 1) {     te3n[m][mm]= 0.473488; } 
	  if(m==68 && mm == 2) {     te3n[m][mm]= 0.384679; } 
	  if(m==68 && mm == 4) {     te3n[m][mm]= 0.928731; } 
	  if(m==68 && mm == 20) {     te3n[m][mm]= 0.070937; } 
	  if(m==68 && mm == 22) {     te3n[m][mm]= 0.817684; } 
	  if(m==68 && mm == 23) {     te3n[m][mm]= 0.404324; } 
	  if(m==68 && mm == 24) {     te3n[m][mm]= 0.998304; } 
	  if(m==68 && mm == 26) {     te3n[m][mm]= 0.220867; } 
	  if(m==68 && mm == 27) {     te3n[m][mm]= 0.367743; } 
	  if(m==68 && mm == 28) {     te3n[m][mm]= 0.772373; } 
	  if(m==68 && mm == 30) {     te3n[m][mm]= 1.007816; } 
	  if(m==68 && mm == 31) {     te3n[m][mm]= 0.325097; } 
	  if(m==68 && mm == 33) {     te3n[m][mm]= 0.242084; } 
	  if(m==69 && mm == 8) {     tw0p[m][mm]= 0.487131; } 
	  if(m==69 && mm == 10) {     tw0p[m][mm]= 0.220306; } 
	  if(m==69 && mm == 11) {     tw0p[m][mm]= 0.272048; } 
	  if(m==69 && mm == 13) {     tw0p[m][mm]= 0.348097; } 
	  if(m==69 && mm == 14) {     tw0p[m][mm]= 0.975617; } 
	  if(m==69 && mm == 15) {     tw0p[m][mm]= 0.220312; } 
	  if(m==69 && mm == 16) {     tw0p[m][mm]= 0.383124; } 
	  if(m==69 && mm == 17) {     tw0p[m][mm]= 0.060651; } 
	  if(m==69 && mm == 18) {     tw0p[m][mm]= 0.276305; } 
	  if(m==69 && mm == 23) {     tw0p[m][mm]= 1.902981; } 
	  if(m==69 && mm == 26) {     tw0p[m][mm]= 0.386289; } 
	  if(m==69 && mm == 27) {     tw0p[m][mm]= 0.271104; } 
	  if(m==69 && mm == 29) {     tw0p[m][mm]= 1.930333; } 
	  if(m==69 && mm == 30) {     tw0p[m][mm]= 0.647975; } 
	  if(m==69 && mm == 32) {     tw0p[m][mm]= 0.490108; } 
	  if(m==69 && mm == 33) {     tw0p[m][mm]= 0.333868; } 
	  if(m==69 && mm == 34) {     tw0p[m][mm]= 1.638822; } 
	  if(m==69 && mm == 35) {     tw0p[m][mm]= 0.783373; } 
	  if(m==69 && mm == 1) {     tw1p[m][mm]= 1.159397; } 
	  if(m==69 && mm == 3) {     tw1p[m][mm]= 0.064642; } 
	  if(m==69 && mm == 5) {     tw1p[m][mm]= 0.490424; } 
	  if(m==69 && mm == 8) {     tw1p[m][mm]= 0.382515; } 
	  if(m==69 && mm == 20) {     tw1p[m][mm]= 0.169202; } 
	  if(m==69 && mm == 21) {     tw1p[m][mm]= 0.557095; } 
	  if(m==69 && mm == 26) {     tw1p[m][mm]= 0.388667; } 
	  if(m==69 && mm == 27) {     tw1p[m][mm]= 0.357027; } 
	  if(m==69 && mm == 28) {     tw1p[m][mm]= 1.418557; } 
	  if(m==69 && mm == 29) {     tw1p[m][mm]= 0.073816; } 
	  if(m==69 && mm == 33) {     tw1p[m][mm]= 0.140689; } 
	  if(m==69 && mm == 35) {     tw1p[m][mm]= 0.006031; } 
	  if(m==69 && mm == 7) {     tw2p[m][mm]= 0.223929; } 
	  if(m==69 && mm == 9) {     tw2p[m][mm]= 0.061418; } 
	  if(m==69 && mm == 10) {     tw2p[m][mm]= 0.349176; } 
	  if(m==69 && mm == 14) {     tw2p[m][mm]= 1.402122; } 
	  if(m==69 && mm == 15) {     tw2p[m][mm]= 0.259060; } 
	  if(m==69 && mm == 16) {     tw2p[m][mm]= 0.269565; } 
	  if(m==69 && mm == 19) {     tw2p[m][mm]= 0.391537; } 
	  if(m==69 && mm == 21) {     tw2p[m][mm]= 0.782367; } 
	  if(m==69 && mm == 1) {     tw3p[m][mm]= 0.627458; } 
	  if(m==69 && mm == 2) {     tw3p[m][mm]= 0.227962; } 
	  if(m==69 && mm == 3) {     tw3p[m][mm]= 0.813550; } 
	  if(m==69 && mm == 4) {     tw3p[m][mm]= 0.141580; } 
	  if(m==69 && mm == 7) {     tw3p[m][mm]= 0.054164; } 
	  if(m==69 && mm == 9) {     tw3p[m][mm]= 0.303326; } 
	  if(m==69 && mm == 10) {     tw3p[m][mm]= 0.522385; } 
	  if(m==69 && mm == 11) {     tw3p[m][mm]= 0.107433; } 
	  if(m==69 && mm == 12) {     tw3p[m][mm]= 1.477340; } 
	  if(m==69 && mm == 15) {     tw3p[m][mm]= 0.118312; } 
	  if(m==69 && mm == 17) {     tw3p[m][mm]= 0.817781; } 
	  if(m==69 && mm == 18) {     tw3p[m][mm]= 0.713784; } 
	  if(m==69 && mm == 21) {     tw3p[m][mm]= 0.397908; } 
	  if(m==69 && mm == 27) {     tw3p[m][mm]= 0.722061; } 
	  if(m==69 && mm == 29) {     tw3p[m][mm]= 0.101760; } 
	  if(m==69 && mm == 30) {     tw3p[m][mm]= 0.618164; } 
	  if(m==69 && mm == 32) {     tw3p[m][mm]= 2.119220; } 
	  if(m==69 && mm == 33) {     tw3p[m][mm]= 1.365415; } 
	  if(m==69 && mm == 34) {     tw3p[m][mm]= 1.890012; } 
	  if(m==69 && mm == 35) {     tw3p[m][mm]= 1.556808; } 
	  if(m==69 && mm == 3) {     te2p[m][mm]= 0.339836; } 
	  if(m==69 && mm == 4) {     te2p[m][mm]= 0.157284; } 
	  if(m==69 && mm == 5) {     te2p[m][mm]= 0.312154; } 
	  if(m==69 && mm == 6) {     te2p[m][mm]= 0.487386; } 
	  if(m==69 && mm == 7) {     te2p[m][mm]= 0.132937; } 
	  if(m==69 && mm == 8) {     te2p[m][mm]= 0.691428; } 
	  if(m==69 && mm == 16) {     te2p[m][mm]= 1.843519; } 
	  if(m==69 && mm == 18) {     te2p[m][mm]= 1.833129; } 
	  if(m==69 && mm == 19) {     te2p[m][mm]= 2.726960; } 
	  if(m==69 && mm == 20) {     te2p[m][mm]= 3.713931; } 
	  if(m==69 && mm == 21) {     te2p[m][mm]= 3.935972; } 
	  if(m==69 && mm == 23) {     te2p[m][mm]= 1.107414; } 
	  if(m==69 && mm == 28) {     te2p[m][mm]= 0.002889; } 
	  if(m==69 && mm == 8) {     te3p[m][mm]= 1.523358; } 
	  if(m==69 && mm == 9) {     te3p[m][mm]= 0.558712; } 
	  if(m==69 && mm == 10) {     te3p[m][mm]= 0.379924; } 
	  if(m==69 && mm == 11) {     te3p[m][mm]= 0.351119; } 
	  if(m==69 && mm == 14) {     te3p[m][mm]= 0.790448; } 
	  if(m==69 && mm == 18) {     te3p[m][mm]= 0.210289; } 
	  if(m==69 && mm == 20) {     te3p[m][mm]= 0.081859; } 
	  if(m==69 && mm == 1) {     tw0n[m][mm]= 1.035937; } 
	  if(m==69 && mm == 3) {     tw0n[m][mm]= 0.605838; } 
	  if(m==69 && mm == 4) {     tw0n[m][mm]= 0.454894; } 
	  if(m==69 && mm == 5) {     tw0n[m][mm]= 0.333256; } 
	  if(m==69 && mm == 6) {     tw0n[m][mm]= 1.541485; } 
	  if(m==69 && mm == 7) {     tw0n[m][mm]= 1.350102; } 
	  if(m==69 && mm == 8) {     tw0n[m][mm]= 0.452031; } 
	  if(m==69 && mm == 9) {     tw0n[m][mm]= 0.860531; } 
	  if(m==69 && mm == 11) {     tw0n[m][mm]= 0.634567; } 
	  if(m==69 && mm == 12) {     tw0n[m][mm]= 0.120731; } 
	  if(m==69 && mm == 13) {     tw0n[m][mm]= 0.861637; } 
	  if(m==69 && mm == 14) {     tw0n[m][mm]= 0.795964; } 
	  if(m==69 && mm == 15) {     tw0n[m][mm]= 0.330650; } 
	  if(m==69 && mm == 16) {     tw0n[m][mm]= 0.560280; } 
	  if(m==69 && mm == 17) {     tw0n[m][mm]= 0.066868; } 
	  if(m==69 && mm == 18) {     tw0n[m][mm]= 0.210281; } 
	  if(m==69 && mm == 23) {     tw0n[m][mm]= 1.379148; } 
	  if(m==69 && mm == 26) {     tw0n[m][mm]= 0.528786; } 
	  if(m==69 && mm == 27) {     tw0n[m][mm]= 0.285899; } 
	  if(m==69 && mm == 7) {     tw1n[m][mm]= 0.527481; } 
	  if(m==69 && mm == 8) {     tw1n[m][mm]= 0.508828; } 
	  if(m==69 && mm == 9) {     tw1n[m][mm]= 0.342194; } 
	  if(m==69 && mm == 11) {     tw1n[m][mm]= 0.285404; } 
	  if(m==69 && mm == 14) {     tw1n[m][mm]= 0.865241; } 
	  if(m==69 && mm == 16) {     tw1n[m][mm]= 1.459611; } 
	  if(m==69 && mm == 19) {     tw1n[m][mm]= 0.604218; } 
	  if(m==69 && mm == 14) {     tw2n[m][mm]= 1.015584; } 
	  if(m==69 && mm == 16) {     tw2n[m][mm]= 0.181227; } 
	  if(m==69 && mm == 19) {     tw2n[m][mm]= 0.307641; } 
	  if(m==69 && mm == 20) {     tw2n[m][mm]= 0.255040; } 
	  if(m==69 && mm == 22) {     tw2n[m][mm]= 1.078593; } 
	  if(m==69 && mm == 23) {     tw2n[m][mm]= 0.350660; } 
	  if(m==69 && mm == 28) {     tw2n[m][mm]= 0.434483; } 
	  if(m==69 && mm == 29) {     tw2n[m][mm]= 0.607927; } 
	  if(m==69 && mm == 1) {     tw3n[m][mm]= 0.798652; } 
	  if(m==69 && mm == 2) {     tw3n[m][mm]= 0.413157; } 
	  if(m==69 && mm == 3) {     tw3n[m][mm]= 0.778709; } 
	  if(m==69 && mm == 4) {     tw3n[m][mm]= 0.098176; } 
	  if(m==69 && mm == 9) {     tw3n[m][mm]= 0.862965; } 
	  if(m==69 && mm == 10) {     tw3n[m][mm]= 0.424730; } 
	  if(m==69 && mm == 12) {     tw3n[m][mm]= 1.619292; } 
	  if(m==69 && mm == 17) {     tw3n[m][mm]= 0.427665; } 
	  if(m==69 && mm == 18) {     tw3n[m][mm]= 1.536287; } 
	  if(m==69 && mm == 19) {     tw3n[m][mm]= 0.259112; } 
	  if(m==69 && mm == 21) {     tw3n[m][mm]= 0.403249; } 
	  if(m==69 && mm == 27) {     tw3n[m][mm]= 0.237805; } 
	  if(m==69 && mm == 8) {     te2n[m][mm]= 0.047647; } 
	  if(m==69 && mm == 16) {     te2n[m][mm]= 2.012558; } 
	  if(m==69 && mm == 17) {     te2n[m][mm]= 1.957060; } 
	  if(m==69 && mm == 18) {     te2n[m][mm]= 3.385109; } 
	  if(m==69 && mm == 19) {     te2n[m][mm]= 3.383029; } 
	  if(m==69 && mm == 20) {     te2n[m][mm]= 3.686291; } 
	  if(m==69 && mm == 21) {     te2n[m][mm]= 1.878305; } 
	  if(m==69 && mm == 23) {     te2n[m][mm]= 1.063850; } 
	  if(m==69 && mm == 28) {     te2n[m][mm]= 0.374707; } 
	  if(m==69 && mm == 31) {     te2n[m][mm]= 0.398960; } 
	  if(m==69 && mm == 35) {     te2n[m][mm]= 1.355166; } 
	  if(m==69 && mm == 1) {     te3n[m][mm]= 0.013669; } 
	  if(m==69 && mm == 2) {     te3n[m][mm]= 2.195771; } 
	  if(m==69 && mm == 3) {     te3n[m][mm]= 0.450334; } 
	  if(m==69 && mm == 25) {     te3n[m][mm]= 0.691492; } 
	  if(m==69 && mm == 26) {     te3n[m][mm]= 1.274718; } 
	  if(m==69 && mm == 28) {     te3n[m][mm]= 1.689568; } 
	  if(m==69 && mm == 30) {     te3n[m][mm]= 0.121658; } 
	  if(m==69 && mm == 32) {     te3n[m][mm]= 0.013081; } 
	  if(m==70 && mm == 8) {     tw0p[m][mm]= 0.351005; } 
	  if(m==70 && mm == 10) {     tw0p[m][mm]= 0.899377; } 
	  if(m==70 && mm == 12) {     tw0p[m][mm]= 1.393848; } 
	  if(m==70 && mm == 14) {     tw0p[m][mm]= 0.722903; } 
	  if(m==70 && mm == 15) {     tw0p[m][mm]= 1.158827; } 
	  if(m==70 && mm == 16) {     tw0p[m][mm]= 1.649168; } 
	  if(m==70 && mm == 18) {     tw0p[m][mm]= 0.282769; } 
	  if(m==70 && mm == 19) {     tw0p[m][mm]= 1.098638; } 
	  if(m==70 && mm == 20) {     tw0p[m][mm]= 1.327666; } 
	  if(m==70 && mm == 22) {     tw0p[m][mm]= 1.851568; } 
	  if(m==70 && mm == 23) {     tw0p[m][mm]= 0.856069; } 
	  if(m==70 && mm == 24) {     tw0p[m][mm]= 0.499932; } 
	  if(m==70 && mm == 25) {     tw0p[m][mm]= 1.017963; } 
	  if(m==70 && mm == 26) {     tw0p[m][mm]= 0.815563; } 
	  if(m==70 && mm == 28) {     tw0p[m][mm]= 0.536735; } 
	  if(m==70 && mm == 29) {     tw0p[m][mm]= 0.419386; } 
	  if(m==70 && mm == 30) {     tw0p[m][mm]= 0.277068; } 
	  if(m==70 && mm == 32) {     tw0p[m][mm]= 0.314913; } 
	  if(m==70 && mm == 33) {     tw0p[m][mm]= 0.180286; } 
	  if(m==70 && mm == 34) {     tw0p[m][mm]= 0.528884; } 
	  if(m==70 && mm == 1) {     tw1p[m][mm]= 0.180677; } 
	  if(m==70 && mm == 2) {     tw1p[m][mm]= 2.001602; } 
	  if(m==70 && mm == 4) {     tw1p[m][mm]= 0.239825; } 
	  if(m==70 && mm == 5) {     tw1p[m][mm]= 1.215580; } 
	  if(m==70 && mm == 6) {     tw1p[m][mm]= 0.192491; } 
	  if(m==70 && mm == 7) {     tw1p[m][mm]= 1.114746; } 
	  if(m==70 && mm == 21) {     tw1p[m][mm]= 0.521288; } 
	  if(m==70 && mm == 22) {     tw1p[m][mm]= 0.878077; } 
	  if(m==70 && mm == 23) {     tw1p[m][mm]= 0.695663; } 
	  if(m==70 && mm == 24) {     tw1p[m][mm]= 0.543326; } 
	  if(m==70 && mm == 25) {     tw1p[m][mm]= 1.899664; } 
	  if(m==70 && mm == 26) {     tw1p[m][mm]= 0.516963; } 
	  if(m==70 && mm == 28) {     tw1p[m][mm]= 0.492081; } 
	  if(m==70 && mm == 29) {     tw1p[m][mm]= 0.770289; } 
	  if(m==70 && mm == 30) {     tw1p[m][mm]= 1.444045; } 
	  if(m==70 && mm == 31) {     tw1p[m][mm]= 1.132638; } 
	  if(m==70 && mm == 32) {     tw1p[m][mm]= 0.627462; } 
	  if(m==70 && mm == 35) {     tw1p[m][mm]= 0.081533; } 
	  if(m==70 && mm == 8) {     tw2p[m][mm]= 0.914140; } 
	  if(m==70 && mm == 12) {     tw2p[m][mm]= 0.650456; } 
	  if(m==70 && mm == 13) {     tw2p[m][mm]= 0.632977; } 
	  if(m==70 && mm == 15) {     tw2p[m][mm]= 0.699842; } 
	  if(m==70 && mm == 16) {     tw2p[m][mm]= 2.717157; } 
	  if(m==70 && mm == 18) {     tw2p[m][mm]= 0.381414; } 
	  if(m==70 && mm == 19) {     tw2p[m][mm]= 0.635455; } 
	  if(m==70 && mm == 20) {     tw2p[m][mm]= 1.783379; } 
	  if(m==70 && mm == 21) {     tw2p[m][mm]= 0.303379; } 
	  if(m==70 && mm == 0) {     tw3p[m][mm]= 1.445622; } 
	  if(m==70 && mm == 5) {     tw3p[m][mm]= 0.479252; } 
	  if(m==70 && mm == 7) {     tw3p[m][mm]= 0.665204; } 
	  if(m==70 && mm == 8) {     tw3p[m][mm]= 1.770651; } 
	  if(m==70 && mm == 10) {     tw3p[m][mm]= 0.540090; } 
	  if(m==70 && mm == 13) {     tw3p[m][mm]= 1.810269; } 
	  if(m==70 && mm == 16) {     tw3p[m][mm]= 0.500634; } 
	  if(m==70 && mm == 17) {     tw3p[m][mm]= 2.182408; } 
	  if(m==70 && mm == 19) {     tw3p[m][mm]= 1.396274; } 
	  if(m==70 && mm == 20) {     tw3p[m][mm]= 0.727970; } 
	  if(m==70 && mm == 21) {     tw3p[m][mm]= 0.497567; } 
	  if(m==70 && mm == 23) {     tw3p[m][mm]= 1.313164; } 
	  if(m==70 && mm == 25) {     tw3p[m][mm]= 0.502528; } 
	  if(m==70 && mm == 28) {     tw3p[m][mm]= 1.356626; } 
	  if(m==70 && mm == 29) {     tw3p[m][mm]= 0.163295; } 
	  if(m==70 && mm == 32) {     tw3p[m][mm]= 0.561992; } 
	  if(m==70 && mm == 33) {     tw3p[m][mm]= 0.129609; } 
	  if(m==70 && mm == 34) {     tw3p[m][mm]= 1.368600; } 
	  if(m==70 && mm == 35) {     tw3p[m][mm]= 1.499091; } 
	  if(m==70 && mm == 2) {     te2p[m][mm]= 1.681926; } 
	  if(m==70 && mm == 4) {     te2p[m][mm]= 1.409674; } 
	  if(m==70 && mm == 22) {     te2p[m][mm]= 0.946254; } 
	  if(m==70 && mm == 28) {     te2p[m][mm]= 1.105636; } 
	  if(m==70 && mm == 19) {     te3p[m][mm]= 0.425296; } 
	  if(m==70 && mm == 0) {     tw0n[m][mm]= 0.903865; } 
	  if(m==70 && mm == 1) {     tw0n[m][mm]= 0.570080; } 
	  if(m==70 && mm == 2) {     tw0n[m][mm]= 1.293227; } 
	  if(m==70 && mm == 4) {     tw0n[m][mm]= 0.014803; } 
	  if(m==70 && mm == 5) {     tw0n[m][mm]= 2.306138; } 
	  if(m==70 && mm == 6) {     tw0n[m][mm]= 0.994941; } 
	  if(m==70 && mm == 7) {     tw0n[m][mm]= 1.700102; } 
	  if(m==70 && mm == 9) {     tw0n[m][mm]= 2.006577; } 
	  if(m==70 && mm == 10) {     tw0n[m][mm]= 0.954391; } 
	  if(m==70 && mm == 12) {     tw0n[m][mm]= 1.883086; } 
	  if(m==70 && mm == 14) {     tw0n[m][mm]= 1.754636; } 
	  if(m==70 && mm == 17) {     tw0n[m][mm]= 2.142382; } 
	  if(m==70 && mm == 18) {     tw0n[m][mm]= 0.375110; } 
	  if(m==70 && mm == 19) {     tw0n[m][mm]= 1.749731; } 
	  if(m==70 && mm == 20) {     tw0n[m][mm]= 1.101283; } 
	  if(m==70 && mm == 21) {     tw0n[m][mm]= 1.718058; } 
	  if(m==70 && mm == 22) {     tw0n[m][mm]= 1.462616; } 
	  if(m==70 && mm == 23) {     tw0n[m][mm]= 0.641132; } 
	  if(m==70 && mm == 24) {     tw0n[m][mm]= 0.652420; } 
	  if(m==70 && mm == 25) {     tw0n[m][mm]= 1.103489; } 
	  if(m==70 && mm == 26) {     tw0n[m][mm]= 0.952356; } 
	  if(m==70 && mm == 27) {     tw0n[m][mm]= 2.053923; } 
	  if(m==70 && mm == 7) {     tw1n[m][mm]= 1.196477; } 
	  if(m==70 && mm == 8) {     tw1n[m][mm]= 0.635585; } 
	  if(m==70 && mm == 9) {     tw1n[m][mm]= 0.327190; } 
	  if(m==70 && mm == 10) {     tw1n[m][mm]= 0.869045; } 
	  if(m==70 && mm == 11) {     tw1n[m][mm]= 1.756619; } 
	  if(m==70 && mm == 12) {     tw1n[m][mm]= 0.398043; } 
	  if(m==70 && mm == 13) {     tw1n[m][mm]= 1.373586; } 
	  if(m==70 && mm == 14) {     tw1n[m][mm]= 1.166020; } 
	  if(m==70 && mm == 16) {     tw1n[m][mm]= 1.181329; } 
	  if(m==70 && mm == 17) {     tw1n[m][mm]= 0.031511; } 
	  if(m==70 && mm == 18) {     tw1n[m][mm]= 0.559689; } 
	  if(m==70 && mm == 20) {     tw1n[m][mm]= 1.568835; } 
	  if(m==70 && mm == 21) {     tw1n[m][mm]= 0.485613; } 
	  if(m==70 && mm == 22) {     tw1n[m][mm]= 1.220942; } 
	  if(m==70 && mm == 23) {     tw1n[m][mm]= 0.547323; } 
	  if(m==70 && mm == 31) {     tw1n[m][mm]= 1.104953; } 
	  if(m==70 && mm == 32) {     tw1n[m][mm]= 0.448377; } 
	  if(m==70 && mm == 0) {     tw2n[m][mm]= 1.690084; } 
	  if(m==70 && mm == 1) {     tw2n[m][mm]= 1.660687; } 
	  if(m==70 && mm == 15) {     tw2n[m][mm]= 0.570678; } 
	  if(m==70 && mm == 17) {     tw2n[m][mm]= 1.132567; } 
	  if(m==70 && mm == 19) {     tw2n[m][mm]= 0.486779; } 
	  if(m==70 && mm == 20) {     tw2n[m][mm]= 2.260697; } 
	  if(m==70 && mm == 21) {     tw2n[m][mm]= 0.046511; } 
	  if(m==70 && mm == 22) {     tw2n[m][mm]= 2.022436; } 
	  if(m==70 && mm == 23) {     tw2n[m][mm]= 0.675748; } 
	  if(m==70 && mm == 25) {     tw2n[m][mm]= 1.204558; } 
	  if(m==70 && mm == 28) {     tw2n[m][mm]= 1.018645; } 
	  if(m==70 && mm == 29) {     tw2n[m][mm]= 0.198271; } 
	  if(m==70 && mm == 30) {     tw2n[m][mm]= 0.615510; } 
	  if(m==70 && mm == 0) {     tw3n[m][mm]= 0.694436; } 
	  if(m==70 && mm == 5) {     tw3n[m][mm]= 1.425587; } 
	  if(m==70 && mm == 7) {     tw3n[m][mm]= 0.930625; } 
	  if(m==70 && mm == 8) {     tw3n[m][mm]= 0.871705; } 
	  if(m==70 && mm == 11) {     tw3n[m][mm]= 2.522619; } 
	  if(m==70 && mm == 13) {     tw3n[m][mm]= 1.492177; } 
	  if(m==70 && mm == 17) {     tw3n[m][mm]= 2.368204; } 
	  if(m==70 && mm == 18) {     tw3n[m][mm]= 6.553635; } 
	  if(m==70 && mm == 19) {     tw3n[m][mm]= 1.325039; } 
	  if(m==70 && mm == 20) {     tw3n[m][mm]= 0.804310; } 
	  if(m==70 && mm == 23) {     tw3n[m][mm]= 1.269812; } 
	  if(m==70 && mm == 25) {     tw3n[m][mm]= 0.390230; } 
	  if(m==70 && mm == 28) {     tw3n[m][mm]= 1.159161; } 
	  if(m==70 && mm == 13) {     te2n[m][mm]= 1.393427; } 
	  if(m==70 && mm == 16) {     te2n[m][mm]= 0.284374; } 
	  if(m==70 && mm == 22) {     te2n[m][mm]= 2.021207; } 
	  if(m==70 && mm == 26) {     te3n[m][mm]= 0.790845; } 
	  if(m==70 && mm == 28) {     te3n[m][mm]= 1.442288; } 
	  if(m==71 && mm == 11) {     tw0p[m][mm]= 0.433944; } 
	  if(m==71 && mm == 13) {     tw0p[m][mm]= 0.195881; } 
	  if(m==71 && mm == 14) {     tw0p[m][mm]= 1.076872; } 
	  if(m==71 && mm == 15) {     tw0p[m][mm]= 0.566097; } 
	  if(m==71 && mm == 17) {     tw0p[m][mm]= 0.399189; } 
	  if(m==71 && mm == 18) {     tw0p[m][mm]= 0.401824; } 
	  if(m==71 && mm == 19) {     tw0p[m][mm]= 0.059752; } 
	  if(m==71 && mm == 21) {     tw0p[m][mm]= 0.125359; } 
	  if(m==71 && mm == 25) {     tw0p[m][mm]= 0.013602; } 
	  if(m==71 && mm == 26) {     tw0p[m][mm]= 0.355514; } 
	  if(m==71 && mm == 29) {     tw0p[m][mm]= 0.362944; } 
	  if(m==71 && mm == 33) {     tw0p[m][mm]= 0.204291; } 
	  if(m==71 && mm == 35) {     tw0p[m][mm]= 0.516591; } 
	  if(m==71 && mm == 0) {     tw1p[m][mm]= 0.357740; } 
	  if(m==71 && mm == 1) {     tw1p[m][mm]= 0.181234; } 
	  if(m==71 && mm == 2) {     tw1p[m][mm]= 1.852380; } 
	  if(m==71 && mm == 3) {     tw1p[m][mm]= 0.107302; } 
	  if(m==71 && mm == 4) {     tw1p[m][mm]= 0.300443; } 
	  if(m==71 && mm == 5) {     tw1p[m][mm]= 0.331786; } 
	  if(m==71 && mm == 6) {     tw1p[m][mm]= 0.254570; } 
	  if(m==71 && mm == 23) {     tw1p[m][mm]= 0.468842; } 
	  if(m==71 && mm == 30) {     tw1p[m][mm]= 0.315390; } 
	  if(m==71 && mm == 31) {     tw1p[m][mm]= 0.053476; } 
	  if(m==71 && mm == 33) {     tw1p[m][mm]= 0.194526; } 
	  if(m==71 && mm == 14) {     tw2p[m][mm]= 0.231895; } 
	  if(m==71 && mm == 15) {     tw2p[m][mm]= 0.240734; } 
	  if(m==71 && mm == 19) {     tw2p[m][mm]= 0.675868; } 
	  if(m==71 && mm == 15) {     tw3p[m][mm]= 0.075563; } 
	  if(m==71 && mm == 20) {     tw3p[m][mm]= 0.754468; } 
	  if(m==71 && mm == 21) {     tw3p[m][mm]= 0.509446; } 
	  if(m==71 && mm == 22) {     tw3p[m][mm]= 1.481542; } 
	  if(m==71 && mm == 23) {     tw3p[m][mm]= 0.061692; } 
	  if(m==71 && mm == 30) {     tw3p[m][mm]= 0.086666; } 
	  if(m==71 && mm == 31) {     tw3p[m][mm]= 0.015453; } 
	  if(m==71 && mm == 33) {     tw3p[m][mm]= 0.936209; } 
	  if(m==71 && mm == 34) {     tw3p[m][mm]= 1.085520; } 
	  if(m==71 && mm == 0) {     tw0n[m][mm]= 0.017303; } 
	  if(m==71 && mm == 3) {     tw0n[m][mm]= 1.888029; } 
	  if(m==71 && mm == 6) {     tw0n[m][mm]= 0.554255; } 
	  if(m==71 && mm == 13) {     tw0n[m][mm]= 0.291782; } 
	  if(m==71 && mm == 17) {     tw0n[m][mm]= 2.172041; } 
	  if(m==71 && mm == 18) {     tw0n[m][mm]= 0.258968; } 
	  if(m==71 && mm == 21) {     tw0n[m][mm]= 0.383064; } 
	  if(m==71 && mm == 25) {     tw0n[m][mm]= 0.003491; } 
	  if(m==71 && mm == 26) {     tw0n[m][mm]= 0.060632; } 
	  if(m==71 && mm == 27) {     tw0n[m][mm]= 0.698232; } 
	  if(m==71 && mm == 13) {     tw1n[m][mm]= 0.029753; } 
	  if(m==71 && mm == 17) {     tw1n[m][mm]= 0.010840; } 
	  if(m==71 && mm == 19) {     tw1n[m][mm]= 0.171277; } 
	  if(m==71 && mm == 20) {     tw1n[m][mm]= 0.096865; } 
	  if(m==71 && mm == 33) {     tw1n[m][mm]= 0.067045; } 
	  if(m==71 && mm == 19) {     tw2n[m][mm]= 0.624151; } 
	  if(m==71 && mm == 20) {     tw2n[m][mm]= 0.479041; } 
	  if(m==71 && mm == 21) {     tw2n[m][mm]= 0.668584; } 
	  if(m==71 && mm == 23) {     tw2n[m][mm]= 0.198635; } 
	  if(m==71 && mm == 29) {     tw2n[m][mm]= 1.897319; } 
	  if(m==71 && mm == 31) {     tw2n[m][mm]= 1.703440; } 
	  if(m==71 && mm == 15) {     tw3n[m][mm]= 0.065740; } 
	  if(m==71 && mm == 19) {     tw3n[m][mm]= 1.351513; } 
	  if(m==71 && mm == 20) {     tw3n[m][mm]= 0.592971; } 
	  if(m==71 && mm == 21) {     tw3n[m][mm]= 0.306262; } 
	  if(m==71 && mm == 22) {     tw3n[m][mm]= 1.423189; } 
	}
    }

  // shift_run3dAu.txt
RUN[0]=67282; 
w0mp[0]=0.415615; 
w1mp[0]=0.296110; 
w2mp[0]=0.019346; 
w3mp[0]=-0.123222; 
e2mp[0]=0.288604; 
e3mp[0]=0.442942; 
w0mn[0]=0.325143; 
w1mn[0]=0.325132; 
w2mn[0]=0.032553; 
w3mn[0]=-0.130870; 
e2mn[0]=0.165705; 
e3mn[0]=0.249117; 
RUN[1]=67283; 
w0mp[1]=0.385282; 
w1mp[1]=0.305534; 
w2mp[1]=-0.013442; 
w3mp[1]=-0.231853; 
e2mp[1]=0.257887; 
e3mp[1]=0.381615; 
w0mn[1]=0.288568; 
w1mn[1]=0.359890; 
w2mn[1]=0.037477; 
w3mn[1]=-0.118397; 
e2mn[1]=0.128122; 
e3mn[1]=0.264476; 
RUN[2]=67284; 
w0mp[2]=0.418163; 
w1mp[2]=0.278278; 
w2mp[2]=0.094822; 
w3mp[2]=-0.170079; 
e2mp[2]=0.265039; 
e3mp[2]=0.436325; 
w0mn[2]=0.321054; 
w1mn[2]=0.318105; 
w2mn[2]=0.069260; 
w3mn[2]=-0.114541; 
e2mn[2]=0.196371; 
e3mn[2]=0.277074; 
RUN[3]=67289; 
w0mp[3]=0.413250; 
w1mp[3]=0.339902; 
w2mp[3]=-0.021726; 
w3mp[3]=-0.390389; 
e2mp[3]=0.270522; 
e3mp[3]=0.228363; 
w0mn[3]=0.316193; 
w1mn[3]=0.337208; 
w2mn[3]=-0.042607; 
w3mn[3]=-0.267019; 
e2mn[3]=0.181509; 
e3mn[3]=0.095355; 
RUN[4]=67500; 
w0mp[4]=0.470072; 
w1mp[4]=0.403757; 
w2mp[4]=0.113078; 
w3mp[4]=-0.136147; 
e2mp[4]=0.318066; 
e3mp[4]=0.445374; 
w0mn[4]=0.391678; 
w1mn[4]=0.320477; 
w2mn[4]=0.105525; 
w3mn[4]=-0.071373; 
e2mn[4]=0.279532; 
e3mn[4]=0.340453; 
RUN[5]=67502; 
w0mp[5]=0.460621; 
w1mp[5]=0.235496; 
w2mp[5]=0.119485; 
w3mp[5]=-0.158710; 
e2mp[5]=0.346035; 
e3mp[5]=0.442967; 
w0mn[5]=0.382415; 
w1mn[5]=0.131135; 
w2mn[5]=0.109185; 
w3mn[5]=-0.024782; 
e2mn[5]=0.301158; 
e3mn[5]=0.316936; 
RUN[6]=67508; 
w0mp[6]=0.481203; 
w1mp[6]=0.133057; 
w2mp[6]=0.171859; 
w3mp[6]=-0.150742; 
e2mp[6]=0.356905; 
e3mp[6]=0.472828; 
w0mn[6]=0.388362; 
w1mn[6]=-0.027752; 
w2mn[6]=0.159868; 
w3mn[6]=-0.031348; 
e2mn[6]=0.299483; 
e3mn[6]=0.344097; 
RUN[7]=67657; 
w0mp[7]=0.334441; 
w1mp[7]=0.218470; 
w2mp[7]=-0.040185; 
w3mp[7]=-0.342214; 
e2mp[7]=0.198558; 
e3mp[7]=0.238201; 
w0mn[7]=0.279289; 
w1mn[7]=0.169408; 
w2mn[7]=0.020372; 
w3mn[7]=-0.113127; 
e2mn[7]=0.142092; 
e3mn[7]=0.124103; 
RUN[8]=67658; 
w0mp[8]=0.378510; 
w1mp[8]=0.231620; 
w2mp[8]=-0.094777; 
w3mp[8]=-0.439757; 
e2mp[8]=0.228858; 
e3mp[8]=0.326638; 
w0mn[8]=0.317260; 
w1mn[8]=0.201766; 
w2mn[8]=-0.010372; 
w3mn[8]=-0.216075; 
e2mn[8]=0.149093; 
e3mn[8]=0.256047; 
RUN[9]=67798; 
w0mp[9]=0.337826; 
w1mp[9]=0.338202; 
w2mp[9]=0.057468; 
w3mp[9]=-0.242199; 
e2mp[9]=0.234758; 
e3mp[9]=0.383116; 
w0mn[9]=0.297188; 
w1mn[9]=0.316447; 
w2mn[9]=0.026806; 
w3mn[9]=-0.164006; 
e2mn[9]=0.160163; 
e3mn[9]=0.218844; 
RUN[10]=67799; 
w0mp[10]=0.335943; 
w1mp[10]=0.318626; 
w2mp[10]=0.050275; 
w3mp[10]=-0.250126; 
e2mp[10]=0.200134; 
e3mp[10]=0.368333; 
w0mn[10]=0.247115; 
w1mn[10]=0.298475; 
w2mn[10]=0.048625; 
w3mn[10]=-0.139429; 
e2mn[10]=0.172159; 
e3mn[10]=0.196954; 
RUN[11]=67802; 
w0mp[11]=0.374096; 
w1mp[11]=0.317179; 
w2mp[11]=0.014171; 
w3mp[11]=-0.251794; 
e2mp[11]=0.249413; 
e3mp[11]=0.409406; 
w0mn[11]=0.276058; 
w1mn[11]=0.283545; 
w2mn[11]=-0.035916; 
w3mn[11]=-0.202874; 
e2mn[11]=0.160300; 
e3mn[11]=0.225873; 
RUN[12]=67805; 
w0mp[12]=0.244342; 
w1mp[12]=0.299911; 
w2mp[12]=0.155412; 
w3mp[12]=-0.202559; 
e2mp[12]=0.202751; 
e3mp[12]=0.235424; 
w0mn[12]=0.054167; 
w1mn[12]=0.081723; 
w2mn[12]=0.106794; 
w3mn[12]=-0.383688; 
e2mn[12]=0.236095; 
e3mn[12]=0.310432; 
RUN[13]=67806; 
w0mp[13]=0.424972; 
w1mp[13]=0.359335; 
w2mp[13]=0.166463; 
w3mp[13]=-0.257077; 
e2mp[13]=0.112779; 
e3mp[13]=0.398971; 
w0mn[13]=0.204826; 
w1mn[13]=0.313774; 
w2mn[13]=0.021853; 
w3mn[13]=-0.135860; 
e2mn[13]=0.016359; 
e3mn[13]=0.141773; 
RUN[14]=67849; 
w0mp[14]=0.463607; 
w1mp[14]=0.409888; 
w2mp[14]=0.116482; 
w3mp[14]=-0.203021; 
e2mp[14]=0.268862; 
e3mp[14]=0.463754; 
w0mn[14]=0.325218; 
w1mn[14]=0.415078; 
w2mn[14]=0.050299; 
w3mn[14]=-0.143420; 
e2mn[14]=0.199892; 
e3mn[14]=0.287471; 
RUN[15]=67851; 
w0mp[15]=0.379046; 
w1mp[15]=0.496015; 
w2mp[15]=0.156213; 
w3mp[15]=-0.102830; 
e2mp[15]=0.236251; 
e3mp[15]=0.581251; 
w0mn[15]=0.440345; 
w1mn[15]=0.296022; 
w2mn[15]=0.181793; 
w3mn[15]=0.072581; 
e2mn[15]=0.231632; 
e3mn[15]=0.427987; 
RUN[16]=68023; 
w0mp[16]=0.596269; 
w1mp[16]=0.447920; 
w2mp[16]=0.109010; 
w3mp[16]=-0.116958; 
e2mp[16]=0.338544; 
e3mp[16]=0.552852; 
w0mn[16]=0.444585; 
w1mn[16]=0.573786; 
w2mn[16]=0.124864; 
w3mn[16]=0.037221; 
e2mn[16]=0.335297; 
e3mn[16]=0.405548; 
RUN[17]=68024; 
w0mp[17]=0.563010; 
w1mp[17]=0.379203; 
w2mp[17]=0.109239; 
w3mp[17]=-0.083169; 
e2mp[17]=0.391529; 
e3mp[17]=0.637994; 
w0mn[17]=0.429280; 
w1mn[17]=0.449455; 
w2mn[17]=0.137174; 
w3mn[17]=-0.011174; 
e2mn[17]=0.315404; 
e3mn[17]=0.373792; 
RUN[18]=68025; 
w0mp[18]=0.603415; 
w1mp[18]=0.440915; 
w2mp[18]=0.052054; 
w3mp[18]=-0.089894; 
e2mp[18]=0.349624; 
e3mp[18]=0.527393; 
w0mn[18]=0.263395; 
w1mn[18]=0.433571; 
w2mn[18]=0.204480; 
w3mn[18]=-0.173618; 
e2mn[18]=0.367198; 
e3mn[18]=0.307550; 
RUN[19]=68026; 
w0mp[19]=0.524762; 
w1mp[19]=0.380405; 
w2mp[19]=0.198681; 
w3mp[19]=-0.202897; 
e2mp[19]=0.319315; 
e3mp[19]=0.608559; 
w0mn[19]=0.339775; 
w1mn[19]=0.419540; 
w2mn[19]=0.103852; 
w3mn[19]=-0.126362; 
e2mn[19]=0.311926; 
e3mn[19]=0.324414; 
RUN[20]=68028; 
w0mp[20]=0.525037; 
w1mp[20]=0.283906; 
w2mp[20]=0.170395; 
w3mp[20]=-0.230969; 
e2mp[20]=0.366229; 
e3mp[20]=0.500057; 
w0mn[20]=0.338172; 
w1mn[20]=0.605083; 
w2mn[20]=0.108745; 
w3mn[20]=0.144703; 
e2mn[20]=0.401235; 
e3mn[20]=0.399761; 
RUN[21]=68029; 
w0mp[21]=0.406678; 
w1mp[21]=0.340796; 
w2mp[21]=0.205353; 
w3mp[21]=-0.027037; 
e2mp[21]=0.328164; 
e3mp[21]=0.689635; 
w0mn[21]=0.458549; 
w1mn[21]=0.387541; 
w2mn[21]=0.104686; 
w3mn[21]=0.108224; 
e2mn[21]=0.355706; 
e3mn[21]=0.380300; 
RUN[22]=68030; 
w0mp[22]=0.678028; 
w1mp[22]=0.357107; 
w2mp[22]=0.098303; 
w3mp[22]=-0.083065; 
e2mp[22]=0.321606; 
e3mp[22]=0.560609; 
w0mn[22]=0.451528; 
w1mn[22]=0.387669; 
w2mn[22]=0.166897; 
w3mn[22]=-0.150214; 
e2mn[22]=0.349815; 
e3mn[22]=0.339680; 
RUN[23]=68032; 
w0mp[23]=0.499511; 
w1mp[23]=0.370661; 
w2mp[23]=0.134495; 
w3mp[23]=-0.069193; 
e2mp[23]=0.390712; 
e3mp[23]=0.630392; 
w0mn[23]=0.310946; 
w1mn[23]=0.309584; 
w2mn[23]=0.090254; 
w3mn[23]=-0.057616; 
e2mn[23]=0.261709; 
e3mn[23]=0.306527; 
RUN[24]=68036; 
w0mp[24]=0.496204; 
w1mp[24]=0.255154; 
w2mp[24]=0.350885; 
w3mp[24]=-0.107245; 
e2mp[24]=0.396729; 
e3mp[24]=0.455908; 
w0mn[24]=0.384233; 
w1mn[24]=0.415366; 
w2mn[24]=0.022830; 
w3mn[24]=-0.030462; 
e2mn[24]=0.275973; 
e3mn[24]=0.297844; 
RUN[25]=68039; 
w0mp[25]=0.511894; 
w1mp[25]=0.403513; 
w2mp[25]=0.131555; 
w3mp[25]=-0.040715; 
e2mp[25]=0.300797; 
e3mp[25]=0.386519; 
w0mn[25]=0.430412; 
w1mn[25]=0.466204; 
w2mn[25]=0.067265; 
w3mn[25]=0.047555; 
e2mn[25]=0.300593; 
e3mn[25]=0.366707; 
RUN[26]=68040; 
w0mp[26]=0.508024; 
w1mp[26]=0.338936; 
w2mp[26]=0.113308; 
w3mp[26]=-0.120842; 
e2mp[26]=0.316245; 
e3mp[26]=0.504065; 
w0mn[26]=0.452007; 
w1mn[26]=0.667056; 
w2mn[26]=0.048364; 
w3mn[26]=-0.073631; 
e2mn[26]=0.309419; 
e3mn[26]=0.257292; 
RUN[27]=68041; 
w0mp[27]=0.540837; 
w1mp[27]=0.409392; 
w2mp[27]=0.277834; 
w3mp[27]=-0.148083; 
e2mp[27]=0.385877; 
e3mp[27]=0.506779; 
w0mn[27]=0.420163; 
w1mn[27]=0.482445; 
w2mn[27]=0.107945; 
w3mn[27]=-0.028352; 
e2mn[27]=0.230135; 
e3mn[27]=0.338357; 
RUN[28]=68042; 
w0mp[28]=0.664683; 
w1mp[28]=0.320494; 
w2mp[28]=0.257003; 
w3mp[28]=-0.094503; 
e2mp[28]=0.281793; 
e3mp[28]=0.490271; 
w0mn[28]=0.371572; 
w1mn[28]=0.361787; 
w2mn[28]=0.240917; 
w3mn[28]=0.147736; 
e2mn[28]=0.253000; 
e3mn[28]=0.286838; 
RUN[29]=68043; 
w0mp[29]=0.474826; 
w1mp[29]=0.344007; 
w2mp[29]=0.149489; 
w3mp[29]=-0.160965; 
e2mp[29]=0.331500; 
e3mp[29]=0.511193; 
w0mn[29]=0.414419; 
w1mn[29]=0.452100; 
w2mn[29]=0.147738; 
w3mn[29]=-0.001511; 
e2mn[29]=0.286604; 
e3mn[29]=0.349578; 
RUN[30]=68046; 
w0mp[30]=0.416732; 
w1mp[30]=0.393898; 
w2mp[30]=0.144052; 
w3mp[30]=-0.372167; 
e2mp[30]=0.372393; 
e3mp[30]=0.468750; 
w0mn[30]=0.321214; 
w1mn[30]=0.337477; 
w2mn[30]=0.076490; 
w3mn[30]=0.103395; 
e2mn[30]=0.201869; 
e3mn[30]=0.493984; 
RUN[31]=68047; 
w0mp[31]=0.498767; 
w1mp[31]=0.367769; 
w2mp[31]=0.134070; 
w3mp[31]=-0.139422; 
e2mp[31]=0.316506; 
e3mp[31]=0.509525; 
w0mn[31]=0.314313; 
w1mn[31]=0.323702; 
w2mn[31]=0.082783; 
w3mn[31]=-0.055222; 
e2mn[31]=0.220509; 
e3mn[31]=0.313373; 
RUN[32]=68048; 
w0mp[32]=0.521256; 
w1mp[32]=0.254363; 
w2mp[32]=0.074375; 
w3mp[32]=-0.118077; 
e2mp[32]=0.276083; 
e3mp[32]=0.398721; 
w0mn[32]=0.462957; 
w1mn[32]=0.279299; 
w2mn[32]=0.042512; 
w3mn[32]=-0.130903; 
e2mn[32]=0.200042; 
e3mn[32]=0.357163; 
RUN[33]=68049; 
w0mp[33]=0.431244; 
w1mp[33]=0.296801; 
w2mp[33]=0.392751; 
w3mp[33]=-0.169973; 
e2mp[33]=0.308306; 
e3mp[33]=0.438098; 
w0mn[33]=0.536769; 
w1mn[33]=0.424457; 
w2mn[33]=0.029949; 
w3mn[33]=0.069327; 
e2mn[33]=0.248934; 
e3mn[33]=0.209036; 
RUN[34]=68125; 
w0mp[34]=0.754033; 
w1mp[34]=0.761581; 
w2mp[34]=0.605544; 
w3mp[34]=0.350520; 
e2mp[34]=0.687900; 
e3mp[34]=0.964522; 
w0mn[34]=0.703841; 
w1mn[34]=0.692345; 
w2mn[34]=0.662213; 
w3mn[34]=0.175301; 
e2mn[34]=0.680000; 
e3mn[34]=0.787169; 
RUN[35]=68126; 
w0mp[35]=0.809893; 
w1mp[35]=0.689922; 
w2mp[35]=0.583805; 
w3mp[35]=0.318513; 
e2mp[35]=0.755112; 
e3mp[35]=0.926237; 
w0mn[35]=0.773707; 
w1mn[35]=0.725114; 
w2mn[35]=0.590880; 
w3mn[35]=0.441804; 
e2mn[35]=0.657673; 
e3mn[35]=0.691637; 
RUN[36]=68140; 
w0mp[36]=0.793458; 
w1mp[36]=0.714714; 
w2mp[36]=0.540531; 
w3mp[36]=0.326828; 
e2mp[36]=0.767740; 
e3mp[36]=0.841865; 
w0mn[36]=0.762061; 
w1mn[36]=0.833301; 
w2mn[36]=0.607986; 
w3mn[36]=0.418062; 
e2mn[36]=0.666191; 
e3mn[36]=0.686167; 
RUN[37]=69482; 
w0mp[37]=0.305808; 
w1mp[37]=0.270995; 
w2mp[37]=0.184263; 
w3mp[37]=-0.143363; 
e2mp[37]=0.300912; 
e3mp[37]=0.443931; 
w0mn[37]=0.186979; 
w1mn[37]=0.223412; 
w2mn[37]=0.141535; 
w3mn[37]=-0.095235; 
e2mn[37]=0.212851; 
e3mn[37]=0.263047; 
RUN[38]=69483; 
w0mp[38]=0.429979; 
w1mp[38]=0.308319; 
w2mp[38]=0.188156; 
w3mp[38]=-0.027196; 
e2mp[38]=0.344244; 
e3mp[38]=0.321523; 
w0mn[38]=0.238469; 
w1mn[38]=0.481924; 
w2mn[38]=0.203372; 
w3mn[38]=-0.051931; 
e2mn[38]=0.235911; 
e3mn[38]=0.325063; 
RUN[39]=69484; 
w0mp[39]=0.286134; 
w1mp[39]=0.235949; 
w2mp[39]=0.025088; 
w3mp[39]=-0.225986; 
e2mp[39]=0.242485; 
e3mp[39]=0.350500; 
w0mn[39]=0.273465; 
w1mn[39]=0.280880; 
w2mn[39]=0.149875; 
w3mn[39]=-0.137984; 
e2mn[39]=0.173468; 
e3mn[39]=0.307622; 
RUN[40]=69485; 
w0mp[40]=0.497896; 
w1mp[40]=0.190369; 
w2mp[40]=0.198552; 
w3mp[40]=-0.162604; 
e2mp[40]=0.353667; 
e3mp[40]=0.492361; 
w0mn[40]=0.179179; 
w1mn[40]=0.413745; 
w2mn[40]=0.192128; 
w3mn[40]=-0.133977; 
e2mn[40]=0.318490; 
e3mn[40]=0.311559; 
RUN[41]=69486; 
w0mp[41]=0.390860; 
w1mp[41]=0.312299; 
w2mp[41]=0.145992; 
w3mp[41]=-0.126506; 
e2mp[41]=0.286807; 
e3mp[41]=0.416394; 
w0mn[41]=0.169883; 
w1mn[41]=0.239731; 
w2mn[41]=0.191880; 
w3mn[41]=-0.102121; 
e2mn[41]=0.241716; 
e3mn[41]=0.330824; 
RUN[42]=69488; 
w0mp[42]=0.247271; 
w1mp[42]=0.288365; 
w2mp[42]=0.154668; 
w3mp[42]=-0.261138; 
e2mp[42]=0.244304; 
e3mp[42]=0.384990; 
w0mn[42]=0.161071; 
w1mn[42]=0.182190; 
w2mn[42]=0.071126; 
w3mn[42]=-0.155725; 
e2mn[42]=0.165632; 
e3mn[42]=0.220816; 
RUN[43]=69490; 
w0mp[43]=0.400285; 
w1mp[43]=0.314924; 
w2mp[43]=0.276003; 
w3mp[43]=-0.159108; 
e2mp[43]=0.372354; 
e3mp[43]=0.477336; 
w0mn[43]=0.262469; 
w1mn[43]=0.337991; 
w2mn[43]=0.198779; 
w3mn[43]=-0.026216; 
e2mn[43]=0.275652; 
e3mn[43]=0.390059; 
RUN[44]=69491; 
w0mp[44]=0.385771; 
w1mp[44]=0.382190; 
w2mp[44]=0.183309; 
w3mp[44]=-0.176328; 
e2mp[44]=0.331547; 
e3mp[44]=0.481919; 
w0mn[44]=0.373170; 
w1mn[44]=0.235602; 
w2mn[44]=0.227114; 
w3mn[44]=-0.122806; 
e2mn[44]=0.329936; 
e3mn[44]=0.317758; 
RUN[45]=69492; 
w0mp[45]=0.390640; 
w1mp[45]=0.487032; 
w2mp[45]=0.150472; 
w3mp[45]=-0.048625; 
e2mp[45]=0.383194; 
e3mp[45]=0.402623; 
w0mn[45]=0.315224; 
w1mn[45]=0.337710; 
w2mn[45]=0.201901; 
w3mn[45]=-0.100197; 
e2mn[45]=0.286215; 
e3mn[45]=0.365711; 
RUN[46]=69493; 
w0mp[46]=0.345134; 
w1mp[46]=0.455128; 
w2mp[46]=0.314512; 
w3mp[46]=-0.138861; 
e2mp[46]=0.392617; 
e3mp[46]=0.436837; 
w0mn[46]=0.107988; 
w1mn[46]=0.272230; 
w2mn[46]=0.144545; 
w3mn[46]=-0.215912; 
e2mn[46]=0.207241; 
e3mn[46]=0.437260; 
RUN[47]=69496; 
w0mp[47]=0.396527; 
w1mp[47]=0.304421; 
w2mp[47]=0.302966; 
w3mp[47]=-0.048835; 
e2mp[47]=0.300934; 
e3mp[47]=0.490799; 
w0mn[47]=0.225845; 
w1mn[47]=0.194529; 
w2mn[47]=0.245347; 
w3mn[47]=-0.168890; 
e2mn[47]=0.271073; 
e3mn[47]=0.361590; 
RUN[48]=69498; 
w0mp[48]=0.399274; 
w1mp[48]=0.256178; 
w2mp[48]=0.128097; 
w3mp[48]=-0.069221; 
e2mp[48]=0.407616; 
e3mp[48]=7.719453; 
w0mn[48]=0.246264; 
w1mn[48]=0.518504; 
w2mn[48]=0.189390; 
w3mn[48]=-0.214617; 
e2mn[48]=0.233522; 
e3mn[48]=0.284053; 
RUN[49]=69499; 
w0mp[49]=0.370447; 
w1mp[49]=0.322350; 
w2mp[49]=0.244696; 
w3mp[49]=-0.192449; 
e2mp[49]=0.286593; 
e3mp[49]=0.243987; 
w0mn[49]=0.277292; 
w1mn[49]=0.206958; 
w2mn[49]=0.222218; 
w3mn[49]=-0.083302; 
e2mn[49]=0.164679; 
e3mn[49]=0.455719; 
RUN[50]=69500; 
w0mp[50]=0.461829; 
w1mp[50]=0.244674; 
w2mp[50]=0.320599; 
w3mp[50]=-0.145379; 
e2mp[50]=0.331648; 
e3mp[50]=0.460212; 
w0mn[50]=0.321645; 
w1mn[50]=0.233906; 
w2mn[50]=0.113979; 
w3mn[50]=-0.168997; 
e2mn[50]=0.149687; 
e3mn[50]=0.391109; 
RUN[51]=69501; 
w0mp[51]=0.402699; 
w1mp[51]=0.340837; 
w2mp[51]=0.231373; 
w3mp[51]=-0.000530; 
e2mp[51]=0.337018; 
e3mp[51]=0.335158; 
w0mn[51]=0.194575; 
w1mn[51]=0.338394; 
w2mn[51]=0.245498; 
w3mn[51]=-0.041680; 
e2mn[51]=0.262889; 
e3mn[51]=0.274389; 
RUN[52]=69648; 
w0mp[52]=0.354468; 
w1mp[52]=0.448090; 
w2mp[52]=0.105448; 
w3mp[52]=-0.253183; 
e2mp[52]=0.335596; 
e3mp[52]=0.387433; 
w0mn[52]=0.310234; 
w1mn[52]=0.242292; 
w2mn[52]=0.010322; 
w3mn[52]=-0.342007; 
e2mn[52]=0.189988; 
e3mn[52]=0.449185; 
RUN[53]=69649; 
w0mp[53]=0.363741; 
w1mp[53]=0.326821; 
w2mp[53]=0.155677; 
w3mp[53]=-0.142106; 
e2mp[53]=0.365727; 
e3mp[53]=0.487956; 
w0mn[53]=0.279569; 
w1mn[53]=0.308075; 
w2mn[53]=0.129831; 
w3mn[53]=-0.191067; 
e2mn[53]=0.274245; 
e3mn[53]=0.355184; 
RUN[54]=69650; 
w0mp[54]=0.381315; 
w1mp[54]=0.354731; 
w2mp[54]=0.157903; 
w3mp[54]=-0.185646; 
e2mp[54]=0.331105; 
e3mp[54]=0.512555; 
w0mn[54]=0.259113; 
w1mn[54]=0.321805; 
w2mn[54]=0.173120; 
w3mn[54]=-0.129551; 
e2mn[54]=0.289977; 
e3mn[54]=0.425719; 
RUN[55]=69652; 
w0mp[55]=0.307070; 
w1mp[55]=0.347759; 
w2mp[55]=0.161091; 
w3mp[55]=-0.159683; 
e2mp[55]=0.369944; 
e3mp[55]=0.547094; 
w0mn[55]=0.262836; 
w1mn[55]=0.318872; 
w2mn[55]=0.156149; 
w3mn[55]=-0.109487; 
e2mn[55]=0.326921; 
e3mn[55]=0.360472; 
RUN[56]=69656; 
w0mp[56]=0.348090; 
w1mp[56]=0.368214; 
w2mp[56]=0.143960; 
w3mp[56]=-0.183606; 
e2mp[56]=0.299781; 
e3mp[56]=0.429469; 
w0mn[56]=0.306144; 
w1mn[56]=0.285916; 
w2mn[56]=0.176821; 
w3mn[56]=-0.222663; 
e2mn[56]=0.308485; 
e3mn[56]=0.316314; 
RUN[57]=69657; 
w0mp[57]=0.318936; 
w1mp[57]=0.383856; 
w2mp[57]=0.083764; 
w3mp[57]=-0.178781; 
e2mp[57]=0.373872; 
e3mp[57]=0.538527; 
w0mn[57]=0.283497; 
w1mn[57]=0.270610; 
w2mn[57]=0.141741; 
w3mn[57]=-0.212666; 
e2mn[57]=0.305067; 
e3mn[57]=0.398534; 
RUN[58]=69658; 
w0mp[58]=0.370889; 
w1mp[58]=0.346445; 
w2mp[58]=0.191776; 
w3mp[58]=-0.182690; 
e2mp[58]=0.289339; 
e3mp[58]=0.477480; 
w0mn[58]=0.276258; 
w1mn[58]=0.317981; 
w2mn[58]=0.199916; 
w3mn[58]=-0.132176; 
e2mn[58]=0.276563; 
e3mn[58]=0.298174; 
RUN[59]=69659; 
w0mp[59]=0.374089; 
w1mp[59]=0.286576; 
w2mp[59]=0.113089; 
w3mp[59]=-0.120590; 
e2mp[59]=0.396593; 
e3mp[59]=0.580359; 
w0mn[59]=0.222957; 
w1mn[59]=0.267445; 
w2mn[59]=0.101890; 
w3mn[59]=-0.124598; 
e2mn[59]=0.315706; 
e3mn[59]=0.584329; 
RUN[60]=69709; 
w0mp[60]=0.319715; 
w1mp[60]=0.379505; 
w2mp[60]=0.181451; 
w3mp[60]=-0.074626; 
e2mp[60]=0.252185; 
e3mp[60]=0.642019; 
w0mn[60]=0.230811; 
w1mn[60]=0.334582; 
w2mn[60]=0.205604; 
w3mn[60]=-0.075785; 
e2mn[60]=0.281336; 
e3mn[60]=0.513115; 
RUN[61]=69717; 
w0mp[61]=0.440599; 
w1mp[61]=0.435131; 
w2mp[61]=0.242081; 
w3mp[61]=-0.151304; 
e2mp[61]=0.365978; 
e3mp[61]=0.511035; 
w0mn[61]=0.324238; 
w1mn[61]=0.327346; 
w2mn[61]=0.134283; 
w3mn[61]=-0.157685; 
e2mn[61]=0.294444; 
e3mn[61]=0.387982; 
RUN[62]=69718; 
w0mp[62]=0.257938; 
w1mp[62]=0.411870; 
w2mp[62]=0.312058; 
w3mp[62]=-0.246796; 
e2mp[62]=0.322395; 
e3mp[62]=0.475508; 
w0mn[62]=0.379925; 
w1mn[62]=0.312030; 
w2mn[62]=0.223816; 
w3mn[62]=-0.207040; 
e2mn[62]=0.276810; 
e3mn[62]=0.396718; 
RUN[63]=69748; 
w0mp[63]=0.299655; 
w1mp[63]=0.527052; 
w2mp[63]=0.269436; 
w3mp[63]=-0.078823; 
e2mp[63]=0.294317; 
e3mp[63]=0.584338; 
w0mn[63]=0.358137; 
w1mn[63]=0.323716; 
w2mn[63]=0.215918; 
w3mn[63]=-0.059146; 
e2mn[63]=0.260552; 
e3mn[63]=0.269501; 
RUN[64]=69754; 
w0mp[64]=0.484629; 
w1mp[64]=0.485883; 
w2mp[64]=0.218457; 
w3mp[64]=-0.027259; 
e2mp[64]=0.437792; 
e3mp[64]=0.526592; 
w0mn[64]=0.327304; 
w1mn[64]=0.263874; 
w2mn[64]=0.203705; 
w3mn[64]=-0.012979; 
e2mn[64]=0.353670; 
e3mn[64]=0.417341; 
RUN[65]=69818; 
w0mp[65]=0.425410; 
w1mp[65]=0.333179; 
w2mp[65]=0.155724; 
w3mp[65]=-0.172347; 
e2mp[65]=0.381414; 
e3mp[65]=0.423700; 
w0mn[65]=0.295759; 
w1mn[65]=0.365116; 
w2mn[65]=0.211356; 
w3mn[65]=-0.337749; 
e2mn[65]=0.215756; 
e3mn[65]=0.358509; 
RUN[66]=69821; 
w0mp[66]=0.402011; 
w1mp[66]=0.322189; 
w2mp[66]=0.147867; 
w3mp[66]=-0.162696; 
e2mp[66]=0.351188; 
e3mp[66]=0.442535; 
w0mn[66]=0.267197; 
w1mn[66]=0.296347; 
w2mn[66]=0.172508; 
w3mn[66]=-0.097963; 
e2mn[66]=0.323976; 
e3mn[66]=0.370346; 
RUN[67]=69822; 
w0mp[67]=0.473106; 
w1mp[67]=0.444086; 
w2mp[67]=0.250216; 
w3mp[67]=-0.237543; 
e2mp[67]=0.363980; 
e3mp[67]=0.501403; 
w0mn[67]=0.349507; 
w1mn[67]=0.387161; 
w2mn[67]=0.157273; 
w3mn[67]=-0.123032; 
e2mn[67]=0.303995; 
e3mn[67]=0.416499; 
RUN[68]=69823; 
w0mp[68]=0.361211; 
w1mp[68]=0.373248; 
w2mp[68]=0.179778; 
w3mp[68]=-0.231122; 
e2mp[68]=0.311003; 
e3mp[68]=0.430315; 
w0mn[68]=0.292452; 
w1mn[68]=0.274870; 
w2mn[68]=0.159183; 
w3mn[68]=-0.214717; 
e2mn[68]=0.237378; 
e3mn[68]=0.285782; 
RUN[69]=69826; 
w0mp[69]=0.351881; 
w1mp[69]=0.312494; 
w2mp[69]=0.117090; 
w3mp[69]=-0.236901; 
e2mp[69]=0.328862; 
e3mp[69]=0.471650; 
w0mn[69]=0.257419; 
w1mn[69]=0.307831; 
w2mn[69]=0.183126; 
w3mn[69]=-0.174109; 
e2mn[69]=0.239141; 
e3mn[69]=0.422396; 
RUN[70]=69827; 
w0mp[70]=0.215146; 
w1mp[70]=0.358323; 
w2mp[70]=0.105695; 
w3mp[70]=-0.266744; 
e2mp[70]=0.226095; 
e3mp[70]=0.375534; 
w0mn[70]=0.230007; 
w1mn[70]=0.230507; 
w2mn[70]=-0.043326; 
w3mn[70]=-0.263600; 
e2mn[70]=0.261130; 
e3mn[70]=0.330170; 
RUN[71]=69830; 
w0mp[71]=0.281511; 
w1mp[71]=0.191536; 
w2mp[71]=0.110110; 
w3mp[71]=-0.207139; 
e2mp[71]=0.240864; 
e3mp[71]=0.338937; 
w0mn[71]=0.235407; 
w1mn[71]=0.195612; 
w2mn[71]=0.029130; 
w3mn[71]=-0.249973; 
e2mn[71]=0.230936; 
e3mn[71]=0.232519; 
RUN[72]=69831; 
w0mp[72]=0.219792; 
w1mp[72]=0.259074; 
w2mp[72]=0.071873; 
w3mp[72]=-0.161371; 
e2mp[72]=0.262583; 
e3mp[72]=0.341957; 
w0mn[72]=0.284049; 
w1mn[72]=0.120242; 
w2mn[72]=0.116861; 
w3mn[72]=-0.428832; 
e2mn[72]=0.218406; 
e3mn[72]=0.417131; 
RUN[73]=69832; 
w0mp[73]=0.317287; 
w1mp[73]=0.290275; 
w2mp[73]=0.049428; 
w3mp[73]=-0.336459; 
e2mp[73]=0.190077; 
e3mp[73]=0.285220; 
w0mn[73]=0.202486; 
w1mn[73]=0.284642; 
w2mn[73]=0.102841; 
w3mn[73]=-0.281811; 
e2mn[73]=0.212688; 
e3mn[73]=0.257267; 
RUN[74]=69834; 
w0mp[74]=0.317092; 
w1mp[74]=0.297997; 
w2mp[74]=0.158024; 
w3mp[74]=-0.177401; 
e2mp[74]=0.359841; 
e3mp[74]=0.457952; 
w0mn[74]=0.267897; 
w1mn[74]=0.245054; 
w2mn[74]=0.153529; 
w3mn[74]=-0.224765; 
e2mn[74]=0.271844; 
e3mn[74]=0.343205; 
RUN[75]=69838; 
w0mp[75]=0.158172; 
w1mp[75]=0.404930; 
w2mp[75]=0.409688; 
w3mp[75]=-0.066304; 
e2mp[75]=0.298733; 
e3mp[75]=0.352315; 
w0mn[75]=0.249499; 
w1mn[75]=0.362505; 
w2mn[75]=0.057821; 
w3mn[75]=0.176570; 
e2mn[75]=0.298513; 
e3mn[75]=0.229176; 
RUN[76]=70072; 
w0mp[76]=0.283250; 
w1mp[76]=0.322264; 
w2mp[76]=0.101125; 
w3mp[76]=-0.242021; 
e2mp[76]=0.266005; 
e3mp[76]=0.511651; 
w0mn[76]=0.238731; 
w1mn[76]=0.307597; 
w2mn[76]=0.073983; 
w3mn[76]=-0.308739; 
e2mn[76]=0.223613; 
e3mn[76]=0.305292; 
RUN[77]=70073; 
w0mp[77]=0.235893; 
w1mp[77]=0.332954; 
w2mp[77]=0.066455; 
w3mp[77]=-0.282346; 
e2mp[77]=0.299578; 
e3mp[77]=0.401981; 
w0mn[77]=0.217664; 
w1mn[77]=0.313510; 
w2mn[77]=0.070063; 
w3mn[77]=-0.220762; 
e2mn[77]=0.223964; 
e3mn[77]=0.306231; 
RUN[78]=70074; 
w0mp[78]=0.330967; 
w1mp[78]=0.375925; 
w2mp[78]=0.122083; 
w3mp[78]=-0.173668; 
e2mp[78]=0.288761; 
e3mp[78]=0.504254; 
w0mn[78]=0.184923; 
w1mn[78]=0.319309; 
w2mn[78]=0.131186; 
w3mn[78]=-0.218134; 
e2mn[78]=0.214100; 
e3mn[78]=0.373571; 
RUN[79]=70075; 
w0mp[79]=0.325251; 
w1mp[79]=0.307939; 
w2mp[79]=0.131636; 
w3mp[79]=-0.287895; 
e2mp[79]=0.304239; 
e3mp[79]=0.445713; 
w0mn[79]=0.237233; 
w1mn[79]=0.268828; 
w2mn[79]=0.057038; 
w3mn[79]=-0.157387; 
e2mn[79]=0.160616; 
e3mn[79]=0.325556; 
RUN[80]=70076; 
w0mp[80]=0.303951; 
w1mp[80]=0.327378; 
w2mp[80]=0.086518; 
w3mp[80]=-0.270046; 
e2mp[80]=0.290107; 
e3mp[80]=0.429183; 
w0mn[80]=0.189068; 
w1mn[80]=0.324903; 
w2mn[80]=0.105366; 
w3mn[80]=-0.179732; 
e2mn[80]=0.229759; 
e3mn[80]=0.301228; 
RUN[81]=70078; 
w0mp[81]=0.261840; 
w1mp[81]=0.319611; 
w2mp[81]=0.101430; 
w3mp[81]=0.062823; 
e2mp[81]=0.260906; 
e3mp[81]=0.486972; 
w0mn[81]=0.262017; 
w1mn[81]=0.225870; 
w2mn[81]=0.164479; 
w3mn[81]=-0.273239; 
e2mn[81]=0.182162; 
e3mn[81]=0.335221; 
RUN[82]=70083; 
w0mp[82]=0.348085; 
w1mp[82]=0.309478; 
w2mp[82]=0.124824; 
w3mp[82]=-0.258832; 
e2mp[82]=0.273622; 
e3mp[82]=0.433661; 
w0mn[82]=0.145511; 
w1mn[82]=0.226363; 
w2mn[82]=0.125946; 
w3mn[82]=-0.184143; 
e2mn[82]=0.294722; 
e3mn[82]=0.329707; 
RUN[83]=70087; 
w0mp[83]=0.284651; 
w1mp[83]=0.297757; 
w2mp[83]=0.123221; 
w3mp[83]=-0.246493; 
e2mp[83]=0.264816; 
e3mp[83]=0.423600; 
w0mn[83]=0.182731; 
w1mn[83]=0.244618; 
w2mn[83]=0.108033; 
w3mn[83]=-0.225300; 
e2mn[83]=0.195304; 
e3mn[83]=0.299937; 
RUN[84]=70088; 
w0mp[84]=0.261103; 
w1mp[84]=0.402697; 
w2mp[84]=0.123135; 
w3mp[84]=-0.214402; 
e2mp[84]=0.236843; 
e3mp[84]=0.453340; 
w0mn[84]=0.195899; 
w1mn[84]=0.363490; 
w2mn[84]=0.136328; 
w3mn[84]=-0.209878; 
e2mn[84]=0.264657; 
e3mn[84]=0.314945; 
RUN[85]=70090; 
w0mp[85]=0.260152; 
w1mp[85]=0.375857; 
w2mp[85]=0.287530; 
w3mp[85]=-0.402731; 
e2mp[85]=0.365155; 
e3mp[85]=0.776370; 
w0mn[85]=0.404104; 
w1mn[85]=0.188714; 
w2mn[85]=0.158768; 
w3mn[85]=-0.091627; 
e2mn[85]=0.248430; 
e3mn[85]=0.391616; 
RUN[86]=70091; 
w0mp[86]=0.462387; 
w1mp[86]=0.212300; 
w2mp[86]=0.450742; 
w3mp[86]=-0.190577; 
e2mp[86]=0.171513; 
e3mp[86]=0.342690; 
w0mn[86]=0.146452; 
w1mn[86]=0.187849; 
w2mn[86]=-0.001999; 
w3mn[86]=-0.022698; 
e2mn[86]=0.262784; 
e3mn[86]=0.586033; 
RUN[87]=70092; 
w0mp[87]=0.279263; 
w1mp[87]=0.330854; 
w2mp[87]=0.081191; 
w3mp[87]=-0.286460; 
e2mp[87]=0.276825; 
e3mp[87]=0.410314; 
w0mn[87]=0.246871; 
w1mn[87]=0.218128; 
w2mn[87]=0.065967; 
w3mn[87]=-0.247147; 
e2mn[87]=0.232074; 
e3mn[87]=0.296379; 
RUN[88]=70093; 
w0mp[88]=0.316167; 
w1mp[88]=0.330307; 
w2mp[88]=0.121008; 
w3mp[88]=-0.259856; 
e2mp[88]=0.283680; 
e3mp[88]=0.411468; 
w0mn[88]=0.231789; 
w1mn[88]=0.246822; 
w2mn[88]=0.082665; 
w3mn[88]=-0.118408; 
e2mn[88]=0.210679; 
e3mn[88]=0.293559; 
RUN[89]=70094; 
w0mp[89]=0.251715; 
w1mp[89]=0.289180; 
w2mp[89]=0.137328; 
w3mp[89]=-0.309476; 
e2mp[89]=0.253092; 
e3mp[89]=0.471260; 
w0mn[89]=0.135301; 
w1mn[89]=0.268613; 
w2mn[89]=0.077331; 
w3mn[89]=-0.151119; 
e2mn[89]=0.219607; 
e3mn[89]=0.194624; 
RUN[90]=70231; 
w0mp[90]=0.372883; 
w1mp[90]=0.350224; 
w2mp[90]=0.191838; 
w3mp[90]=-0.227967; 
e2mp[90]=0.306527; 
e3mp[90]=0.389185; 
w0mn[90]=0.285278; 
w1mn[90]=0.330386; 
w2mn[90]=0.186218; 
w3mn[90]=-0.137013; 
e2mn[90]=0.231675; 
e3mn[90]=0.363867; 
RUN[91]=70236; 
w0mp[91]=0.366597; 
w1mp[91]=0.315379; 
w2mp[91]=0.183936; 
w3mp[91]=-0.177105; 
e2mp[91]=0.288035; 
e3mp[91]=0.497325; 
w0mn[91]=0.280203; 
w1mn[91]=0.302643; 
w2mn[91]=0.097705; 
w3mn[91]=-0.104519; 
e2mn[91]=0.241758; 
e3mn[91]=0.299720; 
RUN[92]=70240; 
w0mp[92]=0.378016; 
w1mp[92]=0.362416; 
w2mp[92]=0.153009; 
w3mp[92]=-0.188293; 
e2mp[92]=0.341192; 
e3mp[92]=0.475041; 
w0mn[92]=0.274933; 
w1mn[92]=0.315992; 
w2mn[92]=0.115906; 
w3mn[92]=-0.109105; 
e2mn[92]=0.239305; 
e3mn[92]=0.369554; 
RUN[93]=70244; 
w0mp[93]=0.342723; 
w1mp[93]=0.195417; 
w2mp[93]=0.243145; 
w3mp[93]=-0.218689; 
e2mp[93]=0.283836; 
e3mp[93]=0.472472; 
w0mn[93]=0.261815; 
w1mn[93]=0.395066; 
w2mn[93]=0.161565; 
w3mn[93]=-0.132088; 
e2mn[93]=0.270189; 
e3mn[93]=0.314912; 
RUN[94]=70245; 
w0mp[94]=0.357959; 
w1mp[94]=0.352357; 
w2mp[94]=0.214078; 
w3mp[94]=-0.228762; 
e2mp[94]=0.294483; 
e3mp[94]=0.336286; 
w0mn[94]=0.245500; 
w1mn[94]=0.317450; 
w2mn[94]=0.101109; 
w3mn[94]=-0.111026; 
e2mn[94]=0.152143; 
e3mn[94]=0.420068; 
RUN[95]=70246; 
w0mp[95]=0.366813; 
w1mp[95]=0.322079; 
w2mp[95]=0.239215; 
w3mp[95]=-0.254730; 
e2mp[95]=0.345772; 
e3mp[95]=0.453863; 
w0mn[95]=0.337089; 
w1mn[95]=0.356876; 
w2mn[95]=0.106439; 
w3mn[95]=-0.077901; 
e2mn[95]=0.390914; 
e3mn[95]=0.433441; 
RUN[96]=70247; 
w0mp[96]=0.358651; 
w1mp[96]=0.326779; 
w2mp[96]=0.153954; 
w3mp[96]=-0.201435; 
e2mp[96]=0.326377; 
e3mp[96]=0.409520; 
w0mn[96]=0.268028; 
w1mn[96]=0.323383; 
w2mn[96]=0.201897; 
w3mn[96]=-0.156488; 
e2mn[96]=0.288182; 
e3mn[96]=0.405690; 
RUN[97]=70253; 
w0mp[97]=0.365018; 
w1mp[97]=0.381531; 
w2mp[97]=0.199269; 
w3mp[97]=-0.199609; 
e2mp[97]=0.354376; 
e3mp[97]=0.505742; 
w0mn[97]=0.257023; 
w1mn[97]=0.325028; 
w2mn[97]=0.169115; 
w3mn[97]=-0.111059; 
e2mn[97]=0.274160; 
e3mn[97]=0.345007; 
RUN[98]=70255; 
w0mp[98]=0.482955; 
w1mp[98]=0.379490; 
w2mp[98]=0.272762; 
w3mp[98]=-0.135300; 
e2mp[98]=0.429105; 
e3mp[98]=0.470064; 
w0mn[98]=0.379298; 
w1mn[98]=0.295653; 
w2mn[98]=0.168050; 
w3mn[98]=-0.115748; 
e2mn[98]=0.343206; 
e3mn[98]=0.350344; 
RUN[99]=74405; 
w0mp[99]=0.311150; 
w1mp[99]=0.410164; 
w2mp[99]=0.182224; 
w3mp[99]=-0.228798; 
e2mp[99]=0.363960; 
e3mp[99]=0.504844; 
w0mn[99]=0.289306; 
w1mn[99]=0.304405; 
w2mn[99]=0.171767; 
w3mn[99]=-0.154133; 
e2mn[99]=0.334036; 
e3mn[99]=0.395773; 
RUN[100]=74406; 
w0mp[100]=0.284175; 
w1mp[100]=0.449308; 
w2mp[100]=0.233243; 
w3mp[100]=-0.171859; 
e2mp[100]=0.334079; 
e3mp[100]=0.389974; 
w0mn[100]=0.382388; 
w1mn[100]=0.345426; 
w2mn[100]=0.169559; 
w3mn[100]=-0.152872; 
e2mn[100]=0.404216; 
e3mn[100]=0.332199; 
RUN[101]=74407; 
w0mp[101]=0.315835; 
w1mp[101]=0.363433; 
w2mp[101]=0.300034; 
w3mp[101]=-0.300013; 
e2mp[101]=0.424996; 
e3mp[101]=0.508565; 
w0mn[101]=0.240393; 
w1mn[101]=0.399344; 
w2mn[101]=0.123379; 
w3mn[101]=-0.213375; 
e2mn[101]=0.349731; 
e3mn[101]=0.562862; 
RUN[102]=74410; 
w0mp[102]=0.394867; 
w1mp[102]=0.391930; 
w2mp[102]=0.240281; 
w3mp[102]=-0.156050; 
e2mp[102]=0.361294; 
e3mp[102]=0.515116; 
w0mn[102]=0.242872; 
w1mn[102]=0.311757; 
w2mn[102]=0.188311; 
w3mn[102]=-0.214665; 
e2mn[102]=0.304147; 
e3mn[102]=0.272765; 
RUN[103]=74417; 
w0mp[103]=0.322496; 
w1mp[103]=0.387980; 
w2mp[103]=0.207282; 
w3mp[103]=-0.242288; 
e2mp[103]=0.373418; 
e3mp[103]=0.488494; 
w0mn[103]=0.280004; 
w1mn[103]=0.390255; 
w2mn[103]=0.146104; 
w3mn[103]=-0.125386; 
e2mn[103]=0.331132; 
e3mn[103]=0.339099; 
RUN[104]=74420; 
w0mp[104]=0.367690; 
w1mp[104]=0.377410; 
w2mp[104]=0.242336; 
w3mp[104]=-0.178882; 
e2mp[104]=0.382318; 
e3mp[104]=0.558096; 
w0mn[104]=0.259957; 
w1mn[104]=0.397803; 
w2mn[104]=0.168715; 
w3mn[104]=-0.090096; 
e2mn[104]=0.362769; 
e3mn[104]=0.407502; 
RUN[105]=74425; 
w0mp[105]=0.342546; 
w1mp[105]=0.332800; 
w2mp[105]=0.147589; 
w3mp[105]=-0.285199; 
e2mp[105]=0.282177; 
e3mp[105]=0.532469; 
w0mn[105]=0.219023; 
w1mn[105]=0.361920; 
w2mn[105]=0.056004; 
w3mn[105]=-0.194562; 
e2mn[105]=0.319554; 
e3mn[105]=0.383962; 
RUN[106]=74448; 
w0mp[106]=0.516379; 
w1mp[106]=0.231527; 
w2mp[106]=-0.009437; 
w3mp[106]=-0.309064; 
e2mp[106]=0.375318; 
e3mp[106]=0.601748; 
w0mn[106]=0.328479; 
w1mn[106]=0.300283; 
w2mn[106]=0.156128; 
w3mn[106]=-0.156652; 
e2mn[106]=0.388927; 
e3mn[106]=0.468040; 
RUN[107]=74463; 
w0mp[107]=0.352666; 
w1mp[107]=0.394810; 
w2mp[107]=0.157331; 
w3mp[107]=-0.171641; 
e2mp[107]=0.370620; 
e3mp[107]=0.493241; 
w0mn[107]=0.266250; 
w1mn[107]=0.375847; 
w2mn[107]=0.172567; 
w3mn[107]=-0.311302; 
e2mn[107]=0.294709; 
e3mn[107]=0.399130; 
RUN[108]=74475; 
w0mp[108]=0.298947; 
w1mp[108]=0.358668; 
w2mp[108]=0.158693; 
w3mp[108]=-0.391311; 
e2mp[108]=0.293775; 
e3mp[108]=0.420831; 
w0mn[108]=0.241271; 
w1mn[108]=0.259911; 
w2mn[108]=0.123466; 
w3mn[108]=-0.317675; 
e2mn[108]=0.307487; 
e3mn[108]=0.309307; 
RUN[109]=74477; 
w0mp[109]=0.287592; 
w1mp[109]=0.307621; 
w2mp[109]=0.068524; 
w3mp[109]=-0.282166; 
e2mp[109]=0.299663; 
e3mp[109]=0.432019; 
w0mn[109]=0.238395; 
w1mn[109]=0.313645; 
w2mn[109]=0.092321; 
w3mn[109]=-0.180947; 
e2mn[109]=0.286565; 
e3mn[109]=0.266526; 
RUN[110]=74479; 
w0mp[110]=0.313532; 
w1mp[110]=0.396971; 
w2mp[110]=0.134606; 
w3mp[110]=-0.206985; 
e2mp[110]=0.386810; 
e3mp[110]=0.520235; 
w0mn[110]=0.267673; 
w1mn[110]=0.328538; 
w2mn[110]=0.173480; 
w3mn[110]=-0.112625; 
e2mn[110]=0.293710; 
e3mn[110]=0.400314; 
RUN[111]=74558; 
w0mp[111]=0.369468; 
w1mp[111]=0.399335; 
w2mp[111]=0.211143; 
w3mp[111]=-0.067907; 
e2mp[111]=0.412146; 
e3mp[111]=0.483979; 
w0mn[111]=0.271684; 
w1mn[111]=0.397336; 
w2mn[111]=0.243144; 
w3mn[111]=0.004311; 
e2mn[111]=0.360030; 
e3mn[111]=0.468435; 
RUN[112]=74662; 
w0mp[112]=0.450213; 
w1mp[112]=0.457977; 
w2mp[112]=0.271597; 
w3mp[112]=-0.038213; 
e2mp[112]=0.510060; 
e3mp[112]=0.527239; 
w0mn[112]=0.369328; 
w1mn[112]=0.488040; 
w2mn[112]=0.310064; 
w3mn[112]=0.004449; 
e2mn[112]=0.411275; 
e3mn[112]=0.552828; 
RUN[113]=74664; 
w0mp[113]=0.361175; 
w1mp[113]=0.427102; 
w2mp[113]=0.230728; 
w3mp[113]=-0.077957; 
e2mp[113]=0.445455; 
e3mp[113]=0.506853; 
w0mn[113]=0.321979; 
w1mn[113]=0.463062; 
w2mn[113]=0.262657; 
w3mn[113]=-0.017253; 
e2mn[113]=0.369985; 
e3mn[113]=0.494059; 
RUN[114]=74669; 
w0mp[114]=0.423546; 
w1mp[114]=0.424156; 
w2mp[114]=0.268166; 
w3mp[114]=-0.068503; 
e2mp[114]=0.424066; 
e3mp[114]=0.489422; 
w0mn[114]=0.337731; 
w1mn[114]=0.491483; 
w2mn[114]=0.271740; 
w3mn[114]=-0.036936; 
e2mn[114]=0.369823; 
e3mn[114]=0.502726; 
RUN[115]=74674; 
w0mp[115]=0.192522; 
w1mp[115]=0.444114; 
w2mp[115]=0.048184; 
w3mp[115]=0.021046; 
e2mp[115]=0.462836; 
e3mp[115]=0.370255; 
w0mn[115]=0.456872; 
w1mn[115]=0.376901; 
w2mn[115]=0.488072; 
w3mn[115]=-0.017583; 
e2mn[115]=0.264942; 
e3mn[115]=0.580769; 
RUN[116]=74675; 
w0mp[116]=0.346801; 
w1mp[116]=0.314409; 
w2mp[116]=0.321448; 
w3mp[116]=-0.107814; 
e2mp[116]=0.202606; 
e3mp[116]=0.245628; 
w0mn[116]=0.255621; 
w1mn[116]=0.574955; 
w2mn[116]=0.350151; 
w3mn[116]=-0.008149; 
e2mn[116]=0.448464; 
e3mn[116]=0.522758; 
RUN[117]=74678; 
w0mp[117]=0.403204; 
w1mp[117]=0.429028; 
w2mp[117]=0.271367; 
w3mp[117]=-0.096754; 
e2mp[117]=0.440077; 
e3mp[117]=0.398075; 
w0mn[117]=0.349425; 
w1mn[117]=0.423382; 
w2mn[117]=0.272089; 
w3mn[117]=-0.034768; 
e2mn[117]=0.362128; 
e3mn[117]=0.487793; 
RUN[118]=74680; 
w0mp[118]=0.321215; 
w1mp[118]=0.387738; 
w2mp[118]=0.263945; 
w3mp[118]=-0.135935; 
e2mp[118]=0.397027; 
e3mp[118]=0.442357; 
w0mn[118]=0.267068; 
w1mn[118]=0.360275; 
w2mn[118]=0.206054; 
w3mn[118]=-0.018660; 
e2mn[118]=0.326888; 
e3mn[118]=0.416404; 
RUN[119]=74684; 
w0mp[119]=0.373080; 
w1mp[119]=0.377426; 
w2mp[119]=0.224462; 
w3mp[119]=-0.156295; 
e2mp[119]=0.394309; 
e3mp[119]=0.498649; 
w0mn[119]=0.352307; 
w1mn[119]=0.377622; 
w2mn[119]=0.193341; 
w3mn[119]=-0.064274; 
e2mn[119]=0.306161; 
e3mn[119]=0.524705; 
RUN[120]=74686; 
w0mp[120]=0.293451; 
w1mp[120]=0.416291; 
w2mp[120]=0.270113; 
w3mp[120]=-0.208695; 
e2mp[120]=0.430954; 
e3mp[120]=0.402607; 
w0mn[120]=0.327630; 
w1mn[120]=0.296507; 
w2mn[120]=0.335503; 
w3mn[120]=0.045569; 
e2mn[120]=0.335711; 
e3mn[120]=0.395283; 
RUN[121]=74691; 
w0mp[121]=0.339471; 
w1mp[121]=0.354502; 
w2mp[121]=0.220902; 
w3mp[121]=-0.108149; 
e2mp[121]=0.387715; 
e3mp[121]=0.401605; 
w0mn[121]=0.256797; 
w1mn[121]=0.372808; 
w2mn[121]=0.185810; 
w3mn[121]=-0.031413; 
e2mn[121]=0.351937; 
e3mn[121]=0.431983; 
RUN[122]=74743; 
w0mp[122]=0.354430; 
w1mp[122]=0.375996; 
w2mp[122]=0.249821; 
w3mp[122]=-0.136393; 
e2mp[122]=0.342420; 
e3mp[122]=0.396341; 
w0mn[122]=0.297939; 
w1mn[122]=0.379449; 
w2mn[122]=0.266963; 
w3mn[122]=-0.076205; 
e2mn[122]=0.334146; 
e3mn[122]=0.482838; 
RUN[123]=74844; 
w0mp[123]=0.240401; 
w1mp[123]=0.546865; 
w2mp[123]=0.347327; 
w3mp[123]=0.034177; 
e2mp[123]=0.480057; 
e3mp[123]=0.367004; 
w0mn[123]=0.326188; 
w1mn[123]=0.360263; 
w2mn[123]=0.238654; 
w3mn[123]=-0.061878; 
e2mn[123]=0.349136; 
e3mn[123]=0.596318; 
RUN[124]=74852; 
w0mp[124]=0.400116; 
w1mp[124]=0.446751; 
w2mp[124]=0.290060; 
w3mp[124]=-0.089663; 
e2mp[124]=0.417966; 
e3mp[124]=0.447143; 
w0mn[124]=0.302510; 
w1mn[124]=0.372979; 
w2mn[124]=0.260823; 
w3mn[124]=-0.035921; 
e2mn[124]=0.352851; 
e3mn[124]=0.430367; 
RUN[125]=74854; 
w0mp[125]=0.331204; 
w1mp[125]=0.375742; 
w2mp[125]=0.323420; 
w3mp[125]=-0.038755; 
e2mp[125]=0.441132; 
e3mp[125]=0.688052; 
w0mn[125]=0.274788; 
w1mn[125]=0.432840; 
w2mn[125]=0.234667; 
w3mn[125]=-0.042754; 
e2mn[125]=0.367944; 
e3mn[125]=0.524981; 
RUN[126]=74855; 
w0mp[126]=0.423973; 
w1mp[126]=0.531902; 
w2mp[126]=0.301348; 
w3mp[126]=-0.009188; 
e2mp[126]=0.496521; 
e3mp[126]=0.554765; 
w0mn[126]=0.446601; 
w1mn[126]=0.463310; 
w2mn[126]=0.330567; 
w3mn[126]=0.068734; 
e2mn[126]=0.486093; 
e3mn[126]=0.534998; 
RUN[127]=74857; 
w0mp[127]=0.409276; 
w1mp[127]=0.477831; 
w2mp[127]=0.374055; 
w3mp[127]=-0.019435; 
e2mp[127]=0.494518; 
e3mp[127]=0.468295; 
w0mn[127]=0.384139; 
w1mn[127]=0.444396; 
w2mn[127]=0.293202; 
w3mn[127]=0.072032; 
e2mn[127]=0.414896; 
e3mn[127]=0.546420; 
RUN[128]=75365; 
w0mp[128]=0.191424; 
w1mp[128]=0.226861; 
w2mp[128]=0.097724; 
w3mp[128]=-0.245000; 
e2mp[128]=0.197233; 
e3mp[128]=0.267493; 
w0mn[128]=0.095699; 
w1mn[128]=0.116909; 
w2mn[128]=0.070663; 
w3mn[128]=-0.181228; 
e2mn[128]=0.111443; 
e3mn[128]=0.337397; 
RUN[129]=75377; 
w0mp[129]=0.374080; 
w1mp[129]=0.335087; 
w2mp[129]=0.355079; 
w3mp[129]=-0.143561; 
e2mp[129]=0.292058; 
e3mp[129]=0.413823; 
w0mn[129]=0.222010; 
w1mn[129]=0.275823; 
w2mn[129]=0.238950; 
w3mn[129]=-0.042838; 
e2mn[129]=0.288322; 
e3mn[129]=0.344281; 
RUN[130]=75379; 
w0mp[130]=0.416560; 
w1mp[130]=0.413037; 
w2mp[130]=0.261272; 
w3mp[130]=-0.091406; 
e2mp[130]=0.405812; 
e3mp[130]=0.493761; 
w0mn[130]=0.189067; 
w1mn[130]=0.323234; 
w2mn[130]=0.312440; 
w3mn[130]=-0.004039; 
e2mn[130]=0.380074; 
e3mn[130]=0.436629; 
RUN[131]=75399; 
w0mp[131]=0.399192; 
w1mp[131]=0.444303; 
w2mp[131]=0.313264; 
w3mp[131]=-0.039869; 
e2mp[131]=0.399304; 
e3mp[131]=0.483328; 
w0mn[131]=0.343981; 
w1mn[131]=0.376525; 
w2mn[131]=0.305014; 
w3mn[131]=0.050947; 
e2mn[131]=0.344546; 
e3mn[131]=0.426292; 
RUN[132]=75401; 
w0mp[132]=0.432193; 
w1mp[132]=0.453935; 
w2mp[132]=0.365518; 
w3mp[132]=0.065145; 
e2mp[132]=0.460523; 
e3mp[132]=0.493043; 
w0mn[132]=0.368983; 
w1mn[132]=0.464205; 
w2mn[132]=0.331937; 
w3mn[132]=0.101471; 
e2mn[132]=0.429713; 
e3mn[132]=0.488346; 
RUN[133]=75403; 
w0mp[133]=0.478594; 
w1mp[133]=0.559410; 
w2mp[133]=0.385371; 
w3mp[133]=0.095653; 
e2mp[133]=0.455489; 
e3mp[133]=0.522168; 
w0mn[133]=0.380956; 
w1mn[133]=0.504397; 
w2mn[133]=0.481327; 
w3mn[133]=0.149165; 
e2mn[133]=0.450422; 
e3mn[133]=0.554213; 
RUN[134]=75532; 
w0mp[134]=0.467169; 
w1mp[134]=0.454692; 
w2mp[134]=0.308046; 
w3mp[134]=-0.040431; 
e2mp[134]=0.511237; 
e3mp[134]=0.534487; 
w0mn[134]=0.409414; 
w1mn[134]=0.529754; 
w2mn[134]=0.305246; 
w3mn[134]=0.024371; 
e2mn[134]=0.448823; 
e3mn[134]=0.530731; 
RUN[135]=75533; 
w0mp[135]=0.535281; 
w1mp[135]=0.636566; 
w2mp[135]=0.419999; 
w3mp[135]=0.019033; 
e2mp[135]=0.667414; 
e3mp[135]=0.696212; 
w0mn[135]=0.346564; 
w1mn[135]=0.488018; 
w2mn[135]=0.351140; 
w3mn[135]=0.087154; 
e2mn[135]=0.605272; 
e3mn[135]=0.630138; 
RUN[136]=75547; 
w0mp[136]=0.481816; 
w1mp[136]=0.427471; 
w2mp[136]=0.390281; 
w3mp[136]=0.032688; 
e2mp[136]=0.471671; 
e3mp[136]=0.474243; 
w0mn[136]=0.454399; 
w1mn[136]=0.408061; 
w2mn[136]=0.438114; 
w3mn[136]=-0.012831; 
e2mn[136]=0.387801; 
e3mn[136]=0.429178; 
RUN[137]=75550; 
w0mp[137]=0.502318; 
w1mp[137]=0.512485; 
w2mp[137]=0.374212; 
w3mp[137]=-0.007417; 
e2mp[137]=0.505344; 
e3mp[137]=0.563600; 
w0mn[137]=0.427295; 
w1mn[137]=0.483132; 
w2mn[137]=0.403594; 
w3mn[137]=0.111305; 
e2mn[137]=0.421280; 
e3mn[137]=0.547932; 
RUN[138]=75587; 
w0mp[138]=0.416800; 
w1mp[138]=0.475850; 
w2mp[138]=0.298665; 
w3mp[138]=-0.031202; 
e2mp[138]=0.416024; 
e3mp[138]=0.460608; 
w0mn[138]=0.324130; 
w1mn[138]=0.393799; 
w2mn[138]=0.323148; 
w3mn[138]=0.007511; 
e2mn[138]=0.385493; 
e3mn[138]=0.463123; 
RUN[139]=75619; 
w0mp[139]=0.387424; 
w1mp[139]=0.450624; 
w2mp[139]=0.305273; 
w3mp[139]=-0.062388; 
e2mp[139]=0.391588; 
e3mp[139]=0.438246; 
w0mn[139]=0.323813; 
w1mn[139]=0.403914; 
w2mn[139]=0.275053; 
w3mn[139]=0.009959; 
e2mn[139]=0.368006; 
e3mn[139]=0.433997; 
RUN[140]=75622; 
w0mp[140]=0.374036; 
w1mp[140]=0.418229; 
w2mp[140]=0.300433; 
w3mp[140]=-0.058314; 
e2mp[140]=0.395292; 
e3mp[140]=0.420489; 
w0mn[140]=0.304896; 
w1mn[140]=0.367576; 
w2mn[140]=0.288628; 
w3mn[140]=0.042867; 
e2mn[140]=0.394102; 
e3mn[140]=0.434418; 
RUN[141]=75631; 
w0mp[141]=0.353952; 
w1mp[141]=0.448768; 
w2mp[141]=0.287271; 
w3mp[141]=-0.042830; 
e2mp[141]=0.411371; 
e3mp[141]=0.454662; 
w0mn[141]=0.276581; 
w1mn[141]=0.383331; 
w2mn[141]=0.325273; 
w3mn[141]=0.004498; 
e2mn[141]=0.389908; 
e3mn[141]=0.442762; 
RUN[142]=75636; 
w0mp[142]=0.379532; 
w1mp[142]=0.465064; 
w2mp[142]=0.353415; 
w3mp[142]=-0.038055; 
e2mp[142]=0.384009; 
e3mp[142]=0.470748; 
w0mn[142]=0.320355; 
w1mn[142]=0.331861; 
w2mn[142]=0.247404; 
w3mn[142]=0.006843; 
e2mn[142]=0.318834; 
e3mn[142]=0.383754; 
RUN[143]=75747; 
w0mp[143]=0.401040; 
w1mp[143]=0.445067; 
w2mp[143]=0.269232; 
w3mp[143]=-0.038121; 
e2mp[143]=0.336572; 
e3mp[143]=0.418780; 
w0mn[143]=0.271247; 
w1mn[143]=0.284880; 
w2mn[143]=0.286438; 
w3mn[143]=-0.085988; 
e2mn[143]=0.302121; 
e3mn[143]=0.365130; 
RUN[144]=75794; 
w0mp[144]=0.325365; 
w1mp[144]=0.292160; 
w2mp[144]=0.267865; 
w3mp[144]=-0.207230; 
e2mp[144]=0.361951; 
e3mp[144]=0.375454; 
w0mn[144]=0.231462; 
w1mn[144]=0.326276; 
w2mn[144]=0.206401; 
w3mn[144]=-0.144738; 
e2mn[144]=0.302022; 
e3mn[144]=0.421923; 
RUN[145]=75795; 
w0mp[145]=0.313307; 
w1mp[145]=0.368425; 
w2mp[145]=0.182836; 
w3mp[145]=-0.135025; 
e2mp[145]=0.317808; 
e3mp[145]=0.029571; 
w0mn[145]=0.241448; 
w1mn[145]=0.328014; 
w2mn[145]=0.204316; 
w3mn[145]=-0.041416; 
e2mn[145]=0.290135; 
e3mn[145]=0.284011; 
RUN[146]=75796; 
w0mp[146]=0.332660; 
w1mp[146]=0.358006; 
w2mp[146]=0.206217; 
w3mp[146]=-0.141342; 
e2mp[146]=0.328086; 
e3mp[146]=0.253102; 
w0mn[146]=0.225648; 
w1mn[146]=0.333323; 
w2mn[146]=0.196971; 
w3mn[146]=-0.100457; 
e2mn[146]=0.276525; 
e3mn[146]=0.332947; 
RUN[147]=75800; 
w0mp[147]=0.352756; 
w1mp[147]=0.400624; 
w2mp[147]=0.217373; 
w3mp[147]=-0.126854; 
e2mp[147]=0.340835; 
e3mp[147]=0.412503; 
w0mn[147]=0.271325; 
w1mn[147]=0.351096; 
w2mn[147]=0.212638; 
w3mn[147]=-0.065505; 
e2mn[147]=0.316630; 
e3mn[147]=0.417930; 
RUN[148]=75983; 
w0mp[148]=0.547982; 
w1mp[148]=0.553071; 
w2mp[148]=0.509723; 
w3mp[148]=0.190883; 
e2mp[148]=0.606006; 
e3mp[148]=0.682791; 
w0mn[148]=0.417294; 
w1mn[148]=0.550631; 
w2mn[148]=0.455800; 
w3mn[148]=0.320281; 
e2mn[148]=0.543516; 
e3mn[148]=0.773628; 
RUN[149]=75994; 
w0mp[149]=0.529056; 
w1mp[149]=0.537815; 
w2mp[149]=0.367526; 
w3mp[149]=0.134798; 
e2mp[149]=0.541544; 
e3mp[149]=0.587210; 
w0mn[149]=0.367826; 
w1mn[149]=0.494896; 
w2mn[149]=0.432280; 
w3mn[149]=0.228888; 
e2mn[149]=0.516682; 
e3mn[149]=0.614485; 
RUN[150]=76050; 
w0mp[150]=0.459622; 
w1mp[150]=0.435362; 
w2mp[150]=0.360341; 
w3mp[150]=0.211906; 
e2mp[150]=0.452969; 
e3mp[150]=0.561565; 
w0mn[150]=0.240487; 
w1mn[150]=0.422565; 
w2mn[150]=0.353616; 
w3mn[150]=0.106028; 
e2mn[150]=0.443087; 
e3mn[150]=0.447346; 
RUN[151]=76053; 
w0mp[151]=0.425031; 
w1mp[151]=0.480505; 
w2mp[151]=0.411984; 
w3mp[151]=0.160254; 
e2mp[151]=0.497328; 
e3mp[151]=0.606885; 
w0mn[151]=0.369881; 
w1mn[151]=0.466691; 
w2mn[151]=0.408285; 
w3mn[151]=0.207272; 
e2mn[151]=0.455423; 
e3mn[151]=0.580018; 
RUN[152]=76070; 
w0mp[152]=0.395958; 
w1mp[152]=0.479882; 
w2mp[152]=0.579949; 
w3mp[152]=0.180698; 
e2mp[152]=0.489372; 
e3mp[152]=0.540648; 
w0mn[152]=0.307847; 
w1mn[152]=0.448949; 
w2mn[152]=0.347108; 
w3mn[152]=0.111262; 
e2mn[152]=0.508861; 
e3mn[152]=0.558533; 
RUN[153]=76274; 
w0mp[153]=0.423190; 
w1mp[153]=0.513014; 
w2mp[153]=0.325531; 
w3mp[153]=0.016677; 
e2mp[153]=0.513011; 
e3mp[153]=0.439448; 
w0mn[153]=0.377905; 
w1mn[153]=0.476289; 
w2mn[153]=0.357753; 
w3mn[153]=0.165930; 
e2mn[153]=0.501862; 
e3mn[153]=0.438093; 
RUN[154]=76276; 
w0mp[154]=0.536333; 
w1mp[154]=0.431868; 
w2mp[154]=0.361038; 
w3mp[154]=-0.013900; 
e2mp[154]=0.461851; 
e3mp[154]=0.562981; 
w0mn[154]=0.447172; 
w1mn[154]=0.424699; 
w2mn[154]=0.492493; 
w3mn[154]=-0.058612; 
e2mn[154]=0.520372; 
e3mn[154]=0.484530; 
RUN[155]=76284; 
w0mp[155]=0.523749; 
w1mp[155]=0.567000; 
w2mp[155]=0.337485; 
w3mp[155]=0.070355; 
e2mp[155]=0.521611; 
e3mp[155]=0.581131; 
w0mn[155]=0.352658; 
w1mn[155]=0.505015; 
w2mn[155]=0.398968; 
w3mn[155]=0.067285; 
e2mn[155]=0.501828; 
e3mn[155]=0.531791; 
RUN[156]=76691; 
w0mp[156]=0.535705; 
w1mp[156]=0.521249; 
w2mp[156]=0.411250; 
w3mp[156]=0.246497; 
e2mp[156]=0.579774; 
e3mp[156]=0.602786; 
w0mn[156]=0.439468; 
w1mn[156]=0.622613; 
w2mn[156]=0.591636; 
w3mn[156]=0.354732; 
e2mn[156]=0.724155; 
e3mn[156]=0.749939; 
RUN[157]=76693; 
w0mp[157]=0.565349; 
w1mp[157]=0.622256; 
w2mp[157]=0.604159; 
w3mp[157]=0.298890; 
e2mp[157]=0.665174; 
e3mp[157]=0.791272; 
w0mn[157]=0.555332; 
w1mn[157]=0.658640; 
w2mn[157]=0.590843; 
w3mn[157]=0.300881; 
e2mn[157]=0.636046; 
e3mn[157]=0.723363; 
RUN[158]=76763; 
w0mp[158]=0.479388; 
w1mp[158]=0.532159; 
w2mp[158]=0.395435; 
w3mp[158]=0.301083; 
e2mp[158]=0.480078; 
e3mp[158]=0.601936; 
w0mn[158]=0.420078; 
w1mn[158]=0.444805; 
w2mn[158]=0.469563; 
w3mn[158]=0.182698; 
e2mn[158]=0.496950; 
e3mn[158]=0.534173; 
RUN[159]=76765; 
w0mp[159]=0.406186; 
w1mp[159]=0.366175; 
w2mp[159]=0.416608; 
w3mp[159]=0.059134; 
e2mp[159]=0.505238; 
e3mp[159]=0.672879; 
w0mn[159]=0.360576; 
w1mn[159]=0.670588; 
w2mn[159]=0.409838; 
w3mn[159]=0.465928; 
e2mn[159]=0.359951; 
e3mn[159]=0.894240; 
RUN[160]=76785; 
w0mp[160]=0.513395; 
w1mp[160]=0.629296; 
w2mp[160]=0.487799; 
w3mp[160]=0.136554; 
e2mp[160]=0.553372; 
e3mp[160]=0.568933; 
w0mn[160]=0.452047; 
w1mn[160]=0.487812; 
w2mn[160]=0.449361; 
w3mn[160]=0.221902; 
e2mn[160]=0.528624; 
e3mn[160]=0.638061; 
RUN[161]=76789; 
w0mp[161]=0.508349; 
w1mp[161]=0.463163; 
w2mp[161]=0.406338; 
w3mp[161]=0.143886; 
e2mp[161]=0.597839; 
e3mp[161]=0.613607; 
w0mn[161]=0.447338; 
w1mn[161]=0.468128; 
w2mn[161]=0.483325; 
w3mn[161]=0.252013; 
e2mn[161]=0.518061; 
e3mn[161]=0.611760; 
RUN[162]=76797; 
w0mp[162]=0.527819; 
w1mp[162]=0.537331; 
w2mp[162]=0.497739; 
w3mp[162]=0.184335; 
e2mp[162]=0.568172; 
e3mp[162]=0.671850; 
w0mn[162]=0.474065; 
w1mn[162]=0.603977; 
w2mn[162]=0.530504; 
w3mn[162]=0.243419; 
e2mn[162]=0.548612; 
e3mn[162]=0.573137; 
RUN[163]=76852; 
w0mp[163]=0.605376; 
w1mp[163]=0.603663; 
w2mp[163]=0.535388; 
w3mp[163]=-0.117087; 
e2mp[163]=0.635079; 
e3mp[163]=0.563385; 
w0mn[163]=0.510727; 
w1mn[163]=0.609819; 
w2mn[163]=0.435931; 
w3mn[163]=0.130207; 
e2mn[163]=0.652618; 
e3mn[163]=0.692263; 
RUN[164]=76853; 
w0mp[164]=0.596071; 
w1mp[164]=0.572088; 
w2mp[164]=0.513782; 
w3mp[164]=0.106435; 
e2mp[164]=0.603879; 
e3mp[164]=0.674967; 
w0mn[164]=0.514250; 
w1mn[164]=0.555378; 
w2mn[164]=0.476770; 
w3mn[164]=0.229776; 
e2mn[164]=0.476695; 
e3mn[164]=0.669120; 
RUN[165]=76864; 
w0mp[165]=0.579457; 
w1mp[165]=0.572414; 
w2mp[165]=0.556547; 
w3mp[165]=0.222422; 
e2mp[165]=0.625549; 
e3mp[165]=0.676661; 
w0mn[165]=0.480335; 
w1mn[165]=0.546166; 
w2mn[165]=0.546124; 
w3mn[165]=0.303189; 
e2mn[165]=0.607333; 
e3mn[165]=0.667071; 
RUN[166]=76983; 
w0mp[166]=0.433615; 
w1mp[166]=0.506984; 
w2mp[166]=0.373729; 
w3mp[166]=-0.026696; 
e2mp[166]=0.531461; 
e3mp[166]=0.629276; 
w0mn[166]=0.337785; 
w1mn[166]=0.485885; 
w2mn[166]=0.443141; 
w3mn[166]=0.036083; 
e2mn[166]=0.489541; 
e3mn[166]=0.461096; 
RUN[167]=76985; 
w0mp[167]=0.447385; 
w1mp[167]=0.487635; 
w2mp[167]=0.382039; 
w3mp[167]=0.010396; 
e2mp[167]=0.476481; 
e3mp[167]=0.551310; 
w0mn[167]=0.354085; 
w1mn[167]=0.427965; 
w2mn[167]=0.367026; 
w3mn[167]=0.102180; 
e2mn[167]=0.437041; 
e3mn[167]=0.577303; 
RUN[168]=76995; 
w0mp[168]=0.403597; 
w1mp[168]=0.459990; 
w2mp[168]=0.349921; 
w3mp[168]=-0.007928; 
e2mp[168]=0.478014; 
e3mp[168]=0.557913; 
w0mn[168]=0.355384; 
w1mn[168]=0.411341; 
w2mn[168]=0.367858; 
w3mn[168]=0.082985; 
e2mn[168]=0.427912; 
e3mn[168]=0.473678; 
RUN[169]=77096; 
w0mp[169]=0.428001; 
w1mp[169]=0.397029; 
w2mp[169]=0.243724; 
w3mp[169]=-0.107112; 
e2mp[169]=0.467043; 
e3mp[169]=0.505330; 
w0mn[169]=0.342260; 
w1mn[169]=0.436229; 
w2mn[169]=0.285774; 
w3mn[169]=-0.054257; 
e2mn[169]=0.435389; 
e3mn[169]=0.470465; 
RUN[170]=77255; 
w0mp[170]=0.348926; 
w1mp[170]=0.245048; 
w2mp[170]=0.153128; 
w3mp[170]=-0.273911; 
e2mp[170]=0.268811; 
e3mp[170]=0.329148; 
w0mn[170]=0.273058; 
w1mn[170]=0.255042; 
w2mn[170]=0.176600; 
w3mn[170]=-0.184607; 
e2mn[170]=0.229876; 
e3mn[170]=0.515399; 
RUN[171]=77256; 
w0mp[171]=0.298651; 
w1mp[171]=0.300832; 
w2mp[171]=0.135613; 
w3mp[171]=-0.189537; 
e2mp[171]=0.312395; 
e3mp[171]=0.395197; 
w0mn[171]=0.205826; 
w1mn[171]=0.283364; 
w2mn[171]=0.214284; 
w3mn[171]=-0.108368; 
e2mn[171]=0.277448; 
e3mn[171]=0.397918; 
RUN[172]=77266; 
w0mp[172]=0.271637; 
w1mp[172]=0.312899; 
w2mp[172]=0.203008; 
w3mp[172]=-0.160581; 
e2mp[172]=0.291007; 
e3mp[172]=0.311645; 
w0mn[172]=0.217741; 
w1mn[172]=0.217399; 
w2mn[172]=0.220273; 
w3mn[172]=-0.114519; 
e2mn[172]=0.273498; 
e3mn[172]=0.295733; 
RUN[173]=77312; 
w0mp[173]=0.360462; 
w1mp[173]=0.415602; 
w2mp[173]=0.274052; 
w3mp[173]=-0.113240; 
e2mp[173]=0.328761; 
e3mp[173]=0.319637; 
w0mn[173]=0.242627; 
w1mn[173]=0.294969; 
w2mn[173]=0.253390; 
w3mn[173]=-0.031580; 
e2mn[173]=0.405378; 
e3mn[173]=0.418880; 
RUN[174]=77313; 
w0mp[174]=0.350917; 
w1mp[174]=0.377808; 
w2mp[174]=0.328215; 
w3mp[174]=-0.090079; 
e2mp[174]=0.364515; 
e3mp[174]=0.404509; 
w0mn[174]=0.222875; 
w1mn[174]=0.307413; 
w2mn[174]=0.193216; 
w3mn[174]=-0.000267; 
e2mn[174]=0.320079; 
e3mn[174]=0.388823; 
RUN[175]=77314; 
w0mp[175]=0.347620; 
w1mp[175]=0.357439; 
w2mp[175]=0.361363; 
w3mp[175]=-0.088919; 
e2mp[175]=0.419981; 
e3mp[175]=0.429208; 
w0mn[175]=0.239144; 
w1mn[175]=0.376377; 
w2mn[175]=0.237490; 
w3mn[175]=-0.031049; 
e2mn[175]=0.349673; 
e3mn[175]=0.465908; 
RUN[176]=77319; 
w0mp[176]=0.395492; 
w1mp[176]=0.341020; 
w2mp[176]=0.213240; 
w3mp[176]=-0.033184; 
e2mp[176]=0.352839; 
e3mp[176]=0.586988; 
w0mn[176]=0.325875; 
w1mn[176]=0.269093; 
w2mn[176]=0.294075; 
w3mn[176]=0.056632; 
e2mn[176]=0.336572; 
e3mn[176]=0.965512; 
RUN[177]=77320; 
w0mp[177]=0.441059; 
w1mp[177]=0.442874; 
w2mp[177]=0.318152; 
w3mp[177]=-0.012227; 
e2mp[177]=0.433776; 
e3mp[177]=0.477987; 
w0mn[177]=0.344386; 
w1mn[177]=0.381110; 
w2mn[177]=0.287636; 
w3mn[177]=0.021660; 
e2mn[177]=0.371221; 
e3mn[177]=0.423552; 
RUN[178]=77322; 
w0mp[178]=0.402699; 
w1mp[178]=0.441574; 
w2mp[178]=0.378563; 
w3mp[178]=0.021213; 
e2mp[178]=0.479848; 
e3mp[178]=0.548736; 
w0mn[178]=0.327252; 
w1mn[178]=0.439103; 
w2mn[178]=0.355521; 
w3mn[178]=0.083178; 
e2mn[178]=0.433576; 
e3mn[178]=0.503397; 
RUN[179]=77324; 
w0mp[179]=0.373658; 
w1mp[179]=0.463564; 
w2mp[179]=0.386143; 
w3mp[179]=0.148819; 
e2mp[179]=0.447807; 
e3mp[179]=0.486229; 
w0mn[179]=0.372420; 
w1mn[179]=0.472668; 
w2mn[179]=0.400775; 
w3mn[179]=0.106682; 
e2mn[179]=0.426927; 
e3mn[179]=0.539975; 
RUN[180]=77326; 
w0mp[180]=0.308829; 
w1mp[180]=0.453898; 
w2mp[180]=0.362726; 
w3mp[180]=-0.023280; 
e2mp[180]=0.513042; 
e3mp[180]=0.335950; 
w0mn[180]=0.351687; 
w1mn[180]=0.430627; 
w2mn[180]=0.341586; 
w3mn[180]=0.169570; 
e2mn[180]=0.416189; 
e3mn[180]=0.484995; 
RUN[181]=77374; 
w0mp[181]=0.505338; 
w1mp[181]=0.515158; 
w2mp[181]=0.442575; 
w3mp[181]=0.195640; 
e2mp[181]=0.497339; 
e3mp[181]=0.628506; 
w0mn[181]=0.396681; 
w1mn[181]=0.498166; 
w2mn[181]=0.438878; 
w3mn[181]=0.192752; 
e2mn[181]=0.484050; 
e3mn[181]=0.598016; 
RUN[182]=77380; 
w0mp[182]=0.532682; 
w1mp[182]=0.530881; 
w2mp[182]=0.475752; 
w3mp[182]=0.152735; 
e2mp[182]=0.545115; 
e3mp[182]=0.687417; 
w0mn[182]=0.392603; 
w1mn[182]=0.538696; 
w2mn[182]=0.461791; 
w3mn[182]=0.264639; 
e2mn[182]=0.507691; 
e3mn[182]=0.645414; 
RUN[183]=77390; 
w0mp[183]=0.482094; 
w1mp[183]=0.527306; 
w2mp[183]=0.541712; 
w3mp[183]=0.129002; 
e2mp[183]=0.657935; 
e3mp[183]=0.624666; 
w0mn[183]=0.431598; 
w1mn[183]=0.577220; 
w2mn[183]=0.484622; 
w3mn[183]=0.241562; 
e2mn[183]=0.524484; 
e3mn[183]=0.591153; 
RUN[184]=77391; 
w0mp[184]=0.528520; 
w1mp[184]=0.548867; 
w2mp[184]=0.490533; 
w3mp[184]=0.219793; 
e2mp[184]=0.576961; 
e3mp[184]=0.712214; 
w0mn[184]=0.442316; 
w1mn[184]=0.502879; 
w2mn[184]=0.509022; 
w3mn[184]=0.272944; 
e2mn[184]=0.508485; 
e3mn[184]=0.610406; 
RUN[185]=77392; 
w0mp[185]=0.564439; 
w1mp[185]=0.546296; 
w2mp[185]=0.458631; 
w3mp[185]=0.085490; 
e2mp[185]=0.586809; 
e3mp[185]=0.633064; 
w0mn[185]=0.454584; 
w1mn[185]=0.539602; 
w2mn[185]=0.464926; 
w3mn[185]=0.227227; 
e2mn[185]=0.523599; 
e3mn[185]=0.664268; 
RUN[186]=77394; 
w0mp[186]=0.627902; 
w1mp[186]=0.532442; 
w2mp[186]=0.433670; 
w3mp[186]=0.188022; 
e2mp[186]=0.595752; 
e3mp[186]=0.752503; 
w0mn[186]=0.456331; 
w1mn[186]=0.459432; 
w2mn[186]=0.438971; 
w3mn[186]=0.177416; 
e2mn[186]=0.500536; 
e3mn[186]=0.626491; 
RUN[187]=77414; 
w0mp[187]=0.567577; 
w1mp[187]=0.529482; 
w2mp[187]=0.416481; 
w3mp[187]=-0.012955; 
e2mp[187]=0.537126; 
e3mp[187]=0.589518; 
w0mn[187]=0.364897; 
w1mn[187]=0.489234; 
w2mn[187]=0.322238; 
w3mn[187]=0.172237; 
e2mn[187]=0.520136; 
e3mn[187]=0.459977; 
RUN[188]=77415; 
w0mp[188]=0.608023; 
w1mp[188]=0.572639; 
w2mp[188]=0.356036; 
w3mp[188]=0.115110; 
e2mp[188]=0.545945; 
e3mp[188]=0.564687; 
w0mn[188]=0.374155; 
w1mn[188]=0.363531; 
w2mn[188]=0.314027; 
w3mn[188]=0.139151; 
e2mn[188]=0.417562; 
e3mn[188]=0.579865; 
RUN[189]=77520; 
w0mp[189]=0.608547; 
w1mp[189]=0.428202; 
w2mp[189]=0.531154; 
w3mp[189]=0.057915; 
e2mp[189]=0.559372; 
e3mp[189]=0.618872; 
w0mn[189]=0.372197; 
w1mn[189]=0.589478; 
w2mn[189]=0.480905; 
w3mn[189]=0.221209; 
e2mn[189]=0.478609; 
e3mn[189]=0.600464; 
RUN[190]=77521; 
w0mp[190]=0.597997; 
w1mp[190]=0.648747; 
w2mp[190]=0.387139; 
w3mp[190]=0.256025; 
e2mp[190]=0.597427; 
e3mp[190]=0.499579; 
w0mn[190]=0.560480; 
w1mn[190]=0.492844; 
w2mn[190]=0.631579; 
w3mn[190]=0.227901; 
e2mn[190]=0.512199; 
e3mn[190]=0.560536; 
RUN[191]=77533; 
w0mp[191]=0.598086; 
w1mp[191]=0.658754; 
w2mp[191]=0.774052; 
w3mp[191]=0.175703; 
e2mp[191]=0.543524; 
e3mp[191]=0.653925; 
w0mn[191]=0.510246; 
w1mn[191]=0.474413; 
w2mn[191]=0.588802; 
w3mn[191]=0.180272; 
e2mn[191]=0.639373; 
e3mn[191]=0.511135; 
RUN[192]=77547; 
w0mp[192]=0.591827; 
w1mp[192]=0.578222; 
w2mp[192]=0.527784; 
w3mp[192]=0.203665; 
e2mp[192]=0.634187; 
e3mp[192]=0.677363; 
w0mn[192]=0.559806; 
w1mn[192]=0.586008; 
w2mn[192]=0.523476; 
w3mn[192]=0.304733; 
e2mn[192]=0.624214; 
e3mn[192]=0.646256; 
RUN[193]=77678; 
w0mp[193]=0.437839; 
w1mp[193]=0.547599; 
w2mp[193]=0.391537; 
w3mp[193]=-0.191954; 
e2mp[193]=0.469963; 
e3mp[193]=0.287560; 
w0mn[193]=0.344837; 
w1mn[193]=0.289768; 
w2mn[193]=0.372364; 
w3mn[193]=-0.066702; 
e2mn[193]=0.421431; 
e3mn[193]=0.440548; 
RUN[194]=77683; 
w0mp[194]=0.453803; 
w1mp[194]=0.366026; 
w2mp[194]=0.267651; 
w3mp[194]=-0.106113; 
e2mp[194]=0.443208; 
e3mp[194]=0.528700; 
w0mn[194]=0.447317; 
w1mn[194]=0.430064; 
w2mn[194]=0.416669; 
w3mn[194]=0.015480; 
e2mn[194]=0.454912; 
e3mn[194]=0.499752; 
RUN[195]=77686; 
w0mp[195]=0.365831; 
w1mp[195]=0.398707; 
w2mp[195]=0.208496; 
w3mp[195]=0.072763; 
e2mp[195]=0.491902; 
e3mp[195]=0.371103; 
w0mn[195]=0.512203; 
w1mn[195]=0.458977; 
w2mn[195]=0.507952; 
w3mn[195]=-0.228239; 
e2mn[195]=0.513893; 
e3mn[195]=0.516407; 
RUN[196]=77688; 
w0mp[196]=0.338780; 
w1mp[196]=0.378154; 
w2mp[196]=0.300282; 
w3mp[196]=-0.068246; 
e2mp[196]=0.404885; 
e3mp[196]=0.438222; 
w0mn[196]=0.307337; 
w1mn[196]=0.364663; 
w2mn[196]=0.273187; 
w3mn[196]=0.039274; 
e2mn[196]=0.350550; 
e3mn[196]=0.454914; 
RUN[197]=78033; 
w0mp[197]=0.228725; 
w1mp[197]=0.234681; 
w2mp[197]=-0.052154; 
w3mp[197]=-0.433205; 
e2mp[197]=0.351612; 
e3mp[197]=0.407956; 
w0mn[197]=0.135920; 
w1mn[197]=0.188735; 
w2mn[197]=-0.005504; 
w3mn[197]=-0.350537; 
e2mn[197]=0.316313; 
e3mn[197]=0.375495; 
RUN[198]=78035; 
w0mp[198]=0.260529; 
w1mp[198]=0.245523; 
w2mp[198]=-0.026421; 
w3mp[198]=-0.384283; 
e2mp[198]=0.342001; 
e3mp[198]=0.442075; 
w0mn[198]=0.158865; 
w1mn[198]=0.234229; 
w2mn[198]=-0.052166; 
w3mn[198]=-0.342496; 
e2mn[198]=0.312561; 
e3mn[198]=0.309573; 
RUN[199]=78181; 
w0mp[199]=0.240810; 
w1mp[199]=0.253129; 
w2mp[199]=0.054214; 
w3mp[199]=-0.252067; 
e2mp[199]=0.288245; 
e3mp[199]=0.357488; 
w0mn[199]=0.092589; 
w1mn[199]=0.238703; 
w2mn[199]=0.074141; 
w3mn[199]=-0.334700; 
e2mn[199]=0.245791; 
e3mn[199]=0.316978; 
RUN[200]=78182; 
w0mp[200]=0.268981; 
w1mp[200]=0.277342; 
w2mp[200]=0.080242; 
w3mp[200]=-0.202032; 
e2mp[200]=0.333747; 
e3mp[200]=0.417171; 
w0mn[200]=0.181372; 
w1mn[200]=0.261205; 
w2mn[200]=0.074140; 
w3mn[200]=-0.195121; 
e2mn[200]=0.345740; 
e3mn[200]=0.329168; 
RUN[201]=78207; 
w0mp[201]=0.335260; 
w1mp[201]=0.348678; 
w2mp[201]=0.204794; 
w3mp[201]=0.034674; 
e2mp[201]=0.379682; 
e3mp[201]=0.505195; 
w0mn[201]=0.263088; 
w1mn[201]=0.354288; 
w2mn[201]=0.371682; 
w3mn[201]=0.005335; 
e2mn[201]=0.357550; 
e3mn[201]=0.444438; 
RUN[202]=78208; 
w0mp[202]=0.323109; 
w1mp[202]=0.592547; 
w2mp[202]=0.315563; 
w3mp[202]=0.117256; 
e2mp[202]=0.509366; 
e3mp[202]=0.508094; 
w0mn[202]=0.214219; 
w1mn[202]=0.312825; 
w2mn[202]=0.285880; 
w3mn[202]=0.067990; 
e2mn[202]=0.388231; 
e3mn[202]=0.430908; 
RUN[203]=78209; 
w0mp[203]=0.319982; 
w1mp[203]=0.381579; 
w2mp[203]=0.305305; 
w3mp[203]=-0.095059; 
e2mp[203]=0.492438; 
e3mp[203]=0.581379; 
w0mn[203]=0.289446; 
w1mn[203]=0.420707; 
w2mn[203]=0.347906; 
w3mn[203]=-0.081320; 
e2mn[203]=0.385789; 
e3mn[203]=0.487893; 
RUN[204]=78210; 
w0mp[204]=0.376862; 
w1mp[204]=0.406554; 
w2mp[204]=0.303083; 
w3mp[204]=-0.061966; 
e2mp[204]=0.502338; 
e3mp[204]=0.556403; 
w0mn[204]=0.311805; 
w1mn[204]=0.415188; 
w2mn[204]=0.267415; 
w3mn[204]=-0.050874; 
e2mn[204]=0.458336; 
e3mn[204]=0.556700; 
RUN[205]=78213; 
w0mp[205]=0.446421; 
w1mp[205]=0.497889; 
w2mp[205]=0.375881; 
w3mp[205]=0.050646; 
e2mp[205]=0.522313; 
e3mp[205]=0.655672; 
w0mn[205]=0.413797; 
w1mn[205]=0.482271; 
w2mn[205]=0.343965; 
w3mn[205]=0.029732; 
e2mn[205]=0.533485; 
e3mn[205]=0.549713; 
RUN[206]=78269; 
w0mp[206]=0.491615; 
w1mp[206]=0.556402; 
w2mp[206]=0.309511; 
w3mp[206]=0.098141; 
e2mp[206]=0.508291; 
e3mp[206]=0.514561; 
w0mn[206]=0.312926; 
w1mn[206]=0.449066; 
w2mn[206]=0.311888; 
w3mn[206]=0.137133; 
e2mn[206]=0.468394; 
e3mn[206]=0.662927; 
RUN[207]=78270; 
w0mp[207]=0.526135; 
w1mp[207]=0.551141; 
w2mp[207]=0.323717; 
w3mp[207]=0.022172; 
e2mp[207]=0.577682; 
e3mp[207]=0.557119; 
w0mn[207]=0.373855; 
w1mn[207]=0.481579; 
w2mn[207]=0.329671; 
w3mn[207]=-0.031735; 
e2mn[207]=0.478731; 
e3mn[207]=0.842723; 
RUN[208]=78306; 
w0mp[208]=0.480319; 
w1mp[208]=0.331119; 
w2mp[208]=0.331849; 
w3mp[208]=-0.176771; 
e2mp[208]=0.478437; 
e3mp[208]=0.409329; 
w0mn[208]=0.431577; 
w1mn[208]=0.574949; 
w2mn[208]=0.292101; 
w3mn[208]=-0.183451; 
e2mn[208]=0.377183; 
e3mn[208]=0.558304; 
RUN[209]=78307; 
w0mp[209]=0.314358; 
w1mp[209]=0.428053; 
w2mp[209]=0.255132; 
w3mp[209]=-0.048831; 
e2mp[209]=0.375333; 
e3mp[209]=0.422265; 
w0mn[209]=0.488561; 
w1mn[209]=0.529219; 
w2mn[209]=0.222009; 
w3mn[209]=-0.112569; 
e2mn[209]=0.302901; 
e3mn[209]=0.517005; 
RUN[210]=78435; 
w0mp[210]=0.303795; 
w1mp[210]=0.345948; 
w2mp[210]=0.204764; 
w3mp[210]=-0.193506; 
e2mp[210]=0.443777; 
e3mp[210]=0.580756; 
w0mn[210]=0.281064; 
w1mn[210]=0.301294; 
w2mn[210]=0.256016; 
w3mn[210]=-0.079794; 
e2mn[210]=0.405190; 
e3mn[210]=0.431913; 
RUN[211]=78508; 
w0mp[211]=0.239862; 
w1mp[211]=0.303149; 
w2mp[211]=0.119072; 
w3mp[211]=-0.309948; 
e2mp[211]=0.355065; 
e3mp[211]=0.410723; 
w0mn[211]=0.194926; 
w1mn[211]=0.206669; 
w2mn[211]=0.026204; 
w3mn[211]=-0.349237; 
e2mn[211]=0.225010; 
e3mn[211]=0.463283; 
RUN[212]=78509; 
w0mp[212]=0.317808; 
w1mp[212]=0.298369; 
w2mp[212]=0.007350; 
w3mp[212]=-0.243983; 
e2mp[212]=0.295225; 
e3mp[212]=0.364426; 
w0mn[212]=0.195969; 
w1mn[212]=0.255205; 
w2mn[212]=0.062077; 
w3mn[212]=-0.246764; 
e2mn[212]=0.406613; 
e3mn[212]=0.374416; 
RUN[213]=78511; 
w0mp[213]=0.372363; 
w1mp[213]=0.325537; 
w2mp[213]=0.296939; 
w3mp[213]=-0.264796; 
e2mp[213]=0.260058; 
e3mp[213]=0.441598; 
w0mn[213]=0.289764; 
w1mn[213]=0.350667; 
w2mn[213]=0.087915; 
w3mn[213]=0.061696; 
e2mn[213]=0.185272; 
e3mn[213]=0.145821; 
RUN[214]=78512; 
w0mp[214]=0.245602; 
w1mp[214]=0.303940; 
w2mp[214]=0.035600; 
w3mp[214]=-0.287972; 
e2mp[214]=0.320483; 
e3mp[214]=0.406037; 
w0mn[214]=0.225895; 
w1mn[214]=0.287550; 
w2mn[214]=0.035439; 
w3mn[214]=-0.213759; 
e2mn[214]=0.310515; 
e3mn[214]=0.345652; 
RUN[215]=78532; 
w0mp[215]=0.190098; 
w1mp[215]=0.213461; 
w2mp[215]=-0.081042; 
w3mp[215]=-0.498673; 
e2mp[215]=0.246950; 
e3mp[215]=0.386049; 
w0mn[215]=0.176537; 
w1mn[215]=0.246146; 
w2mn[215]=-0.052347; 
w3mn[215]=-0.288854; 
e2mn[215]=0.217729; 
e3mn[215]=0.354622; 
RUN[216]=78549; 
w0mp[216]=0.307955; 
w1mp[216]=0.338900; 
w2mp[216]=0.010747; 
w3mp[216]=-0.367843; 
e2mp[216]=0.329231; 
e3mp[216]=0.398027; 
w0mn[216]=0.202425; 
w1mn[216]=0.363940; 
w2mn[216]=-0.004626; 
w3mn[216]=-0.310749; 
e2mn[216]=0.267061; 
e3mn[216]=0.368266; 
RUN[217]=78553; 
w0mp[217]=0.327305; 
w1mp[217]=0.336886; 
w2mp[217]=0.069055; 
w3mp[217]=-0.377818; 
e2mp[217]=0.336337; 
e3mp[217]=0.414239; 
w0mn[217]=0.234609; 
w1mn[217]=0.289164; 
w2mn[217]=0.036602; 
w3mn[217]=-0.275073; 
e2mn[217]=0.311772; 
e3mn[217]=0.360932; 
RUN[218]=78578; 
w0mp[218]=0.318884; 
w1mp[218]=0.340266; 
w2mp[218]=0.064990; 
w3mp[218]=-0.345407; 
e2mp[218]=0.331930; 
e3mp[218]=0.396082; 
w0mn[218]=0.219119; 
w1mn[218]=0.270049; 
w2mn[218]=0.039668; 
w3mn[218]=-0.254570; 
e2mn[218]=0.287409; 
e3mn[218]=0.395306; 
RUN[219]=78632; 
w0mp[219]=0.254462; 
w1mp[219]=0.259108; 
w2mp[219]=0.055777; 
w3mp[219]=-0.398712; 
e2mp[219]=0.330757; 
e3mp[219]=0.350848; 
w0mn[219]=0.206722; 
w1mn[219]=0.237193; 
w2mn[219]=0.023041; 
w3mn[219]=-0.297141; 
e2mn[219]=0.239844; 
e3mn[219]=0.386156; 
RUN[220]=78633; 
w0mp[220]=0.247065; 
w1mp[220]=0.234366; 
w2mp[220]=0.009741; 
w3mp[220]=-0.434813; 
e2mp[220]=0.263335; 
e3mp[220]=0.368120; 
w0mn[220]=0.143344; 
w1mn[220]=0.173557; 
w2mn[220]=-0.037521; 
w3mn[220]=-0.368480; 
e2mn[220]=0.217711; 
e3mn[220]=0.318738; 
RUN[221]=78808; 
w0mp[221]=0.235557; 
w1mp[221]=0.328309; 
w2mp[221]=0.038223; 
w3mp[221]=-0.476401; 
e2mp[221]=0.188355; 
e3mp[221]=0.366125; 
w0mn[221]=0.183839; 
w1mn[221]=0.171614; 
w2mn[221]=-0.066332; 
w3mn[221]=-0.337035; 
e2mn[221]=0.213681; 
e3mn[221]=0.276839; 
RUN[222]=78810; 
w0mp[222]=0.305309; 
w1mp[222]=0.168080; 
w2mp[222]=-0.015215; 
w3mp[222]=-0.470526; 
e2mp[222]=0.244182; 
e3mp[222]=0.307549; 
w0mn[222]=0.092994; 
w1mn[222]=0.193652; 
w2mn[222]=0.057152; 
w3mn[222]=-0.301258; 
e2mn[222]=0.169997; 
e3mn[222]=0.361119; 
RUN[223]=78817; 
w0mp[223]=0.227507; 
w1mp[223]=0.234969; 
w2mp[223]=-0.098193; 
w3mp[223]=-0.426812; 
e2mp[223]=0.261706; 
e3mp[223]=0.388009; 
w0mn[223]=0.131927; 
w1mn[223]=0.207349; 
w2mn[223]=-0.073080; 
w3mn[223]=-0.375765; 
e2mn[223]=0.217973; 
e3mn[223]=0.336126; 
RUN[224]=78838; 
w0mp[224]=0.264514; 
w1mp[224]=0.313367; 
w2mp[224]=0.054714; 
w3mp[224]=-0.435853; 
e2mp[224]=0.334160; 
e3mp[224]=0.417814; 
w0mn[224]=0.250458; 
w1mn[224]=0.275973; 
w2mn[224]=-0.037222; 
w3mn[224]=-0.372186; 
e2mn[224]=0.272120; 
e3mn[224]=0.416518; 
RUN[225]=79047; 
w0mp[225]=0.192696; 
w1mp[225]=0.205287; 
w2mp[225]=0.098077; 
w3mp[225]=-0.365054; 
e2mp[225]=0.249097; 
e3mp[225]=0.354368; 
w0mn[225]=0.125883; 
w1mn[225]=0.265062; 
w2mn[225]=0.062913; 
w3mn[225]=-0.294187; 
e2mn[225]=0.206384; 
e3mn[225]=0.277768; 
RUN[226]=79048; 
w0mp[226]=0.191954; 
w1mp[226]=0.213693; 
w2mp[226]=0.046604; 
w3mp[226]=-0.352597; 
e2mp[226]=0.230467; 
e3mp[226]=0.331837; 
w0mn[226]=0.094596; 
w1mn[226]=0.125541; 
w2mn[226]=0.013464; 
w3mn[226]=-0.298739; 
e2mn[226]=0.198322; 
e3mn[226]=0.261991; 
RUN[227]=79050; 
w0mp[227]=0.138698; 
w1mp[227]=0.201027; 
w2mp[227]=-0.046296; 
w3mp[227]=-0.404398; 
e2mp[227]=0.252912; 
e3mp[227]=0.261113; 
w0mn[227]=0.057445; 
w1mn[227]=0.150958; 
w2mn[227]=-0.045375; 
w3mn[227]=-0.237493; 
e2mn[227]=0.190426; 
e3mn[227]=0.280446; 
RUN[228]=79066; 
w0mp[228]=0.112254; 
w1mp[228]=0.158824; 
w2mp[228]=-0.069213; 
w3mp[228]=-0.414215; 
e2mp[228]=0.217834; 
e3mp[228]=0.244075; 
w0mn[228]=0.065320; 
w1mn[228]=0.119500; 
w2mn[228]=-0.013845; 
w3mn[228]=-0.340302; 
e2mn[228]=0.176227; 
e3mn[228]=0.274165; 
RUN[229]=79067; 
w0mp[229]=0.119066; 
w1mp[229]=0.122003; 
w2mp[229]=-0.050733; 
w3mp[229]=-0.412424; 
e2mp[229]=0.185448; 
e3mp[229]=0.259449; 
w0mn[229]=0.039372; 
w1mn[229]=0.114823; 
w2mn[229]=-0.059273; 
w3mn[229]=-0.397991; 
e2mn[229]=0.150706; 
e3mn[229]=0.246618; 
RUN[230]=79340; 
w0mp[230]=0.107303; 
w1mp[230]=0.170801; 
w2mp[230]=0.086580; 
w3mp[230]=-0.306528; 
e2mp[230]=0.254130; 
e3mp[230]=0.341470; 
w0mn[230]=0.084191; 
w1mn[230]=0.189089; 
w2mn[230]=0.114992; 
w3mn[230]=-0.202421; 
e2mn[230]=0.216099; 
e3mn[230]=0.346215; 
RUN[231]=79341; 
w0mp[231]=0.158602; 
w1mp[231]=0.168217; 
w2mp[231]=0.149781; 
w3mp[231]=-0.223504; 
e2mp[231]=0.250944; 
e3mp[231]=0.382894; 
w0mn[231]=0.047699; 
w1mn[231]=0.148992; 
w2mn[231]=0.149255; 
w3mn[231]=-0.207303; 
e2mn[231]=0.218918; 
e3mn[231]=0.345589; 
RUN[232]=79343; 
w0mp[232]=0.163633; 
w1mp[232]=0.228404; 
w2mp[232]=0.009563; 
w3mp[232]=-0.246578; 
e2mp[232]=0.213474; 
e3mp[232]=0.251026; 
w0mn[232]=-0.007696; 
w1mn[232]=0.130878; 
w2mn[232]=0.097168; 
w3mn[232]=-0.249593; 
e2mn[232]=0.156299; 
e3mn[232]=0.287376; 
RUN[233]=79560; 
w0mp[233]=0.063707; 
w1mp[233]=0.277716; 
w2mp[233]=0.215675; 
w3mp[233]=-0.373012; 
e2mp[233]=0.248425; 
e3mp[233]=0.309369; 
w0mn[233]=0.103791; 
w1mn[233]=0.233693; 
w2mn[233]=0.108615; 
w3mn[233]=-0.196620; 
e2mn[233]=0.235005; 
e3mn[233]=0.210274; 
RUN[234]=79579; 
w0mp[234]=0.104300; 
w1mp[234]=0.185616; 
w2mp[234]=0.087602; 
w3mp[234]=-0.329651; 
e2mp[234]=0.217055; 
e3mp[234]=0.392472; 
w0mn[234]=0.079404; 
w1mn[234]=0.256518; 
w2mn[234]=0.099173; 
w3mn[234]=-0.216432; 
e2mn[234]=0.191758; 
e3mn[234]=0.230132; 
RUN[235]=79603; 
w0mp[235]=0.146744; 
w1mp[235]=0.208295; 
w2mp[235]=0.128941; 
w3mp[235]=-0.204889; 
e2mp[235]=0.270629; 
e3mp[235]=0.386639; 
w0mn[235]=0.069280; 
w1mn[235]=0.144265; 
w2mn[235]=0.165658; 
w3mn[235]=-0.104289; 
e2mn[235]=0.253203; 
e3mn[235]=0.365025; 
RUN[236]=79615; 
w0mp[236]=0.171797; 
w1mp[236]=0.218119; 
w2mp[236]=0.160606; 
w3mp[236]=-0.206112; 
e2mp[236]=0.261696; 
e3mp[236]=0.344378; 
w0mn[236]=0.098401; 
w1mn[236]=0.161286; 
w2mn[236]=0.121126; 
w3mn[236]=-0.122826; 
e2mn[236]=0.228436; 
e3mn[236]=0.369087; 
RUN[237]=79626; 
w0mp[237]=0.187409; 
w1mp[237]=0.225040; 
w2mp[237]=0.232969; 
w3mp[237]=-0.184000; 
e2mp[237]=0.230980; 
e3mp[237]=0.379160; 
w0mn[237]=0.051245; 
w1mn[237]=0.110349; 
w2mn[237]=0.209243; 
w3mn[237]=-0.154173; 
e2mn[237]=0.177043; 
e3mn[237]=0.389240; 
RUN[238]=79629; 
w0mp[238]=0.143472; 
w1mp[238]=0.075857; 
w2mp[238]=0.218429; 
w3mp[238]=-0.244618; 
e2mp[238]=0.234759; 
e3mp[238]=0.315149; 
w0mn[238]=0.106791; 
w1mn[238]=0.073827; 
w2mn[238]=0.069176; 
w3mn[238]=-0.157145; 
e2mn[238]=0.244658; 
e3mn[238]=0.298185; 
RUN[239]=79630; 
w0mp[239]=0.224995; 
w1mp[239]=0.160883; 
w2mp[239]=0.341572; 
w3mp[239]=-0.223585; 
e2mp[239]=0.273387; 
e3mp[239]=0.303660; 
w0mn[239]=0.011522; 
w1mn[239]=0.349937; 
w2mn[239]=0.032076; 
w3mn[239]=-0.255697; 
e2mn[239]=0.139536; 
e3mn[239]=0.320310; 
RUN[240]=79632; 
w0mp[240]=0.152624; 
w1mp[240]=0.209555; 
w2mp[240]=0.117059; 
w3mp[240]=-0.156983; 
e2mp[240]=0.230656; 
e3mp[240]=0.366664; 
w0mn[240]=0.118086; 
w1mn[240]=0.206617; 
w2mn[240]=0.143400; 
w3mn[240]=-0.075246; 
e2mn[240]=0.336771; 
e3mn[240]=0.306827; 
RUN[241]=79641; 
w0mp[241]=0.148082; 
w1mp[241]=0.171063; 
w2mp[241]=0.000836; 
w3mp[241]=-0.355466; 
e2mp[241]=0.221322; 
e3mp[241]=0.476106; 
w0mn[241]=-0.055667; 
w1mn[241]=0.249200; 
w2mn[241]=0.085712; 
w3mn[241]=-0.220574; 
e2mn[241]=0.088799; 
e3mn[241]=0.216710; 
RUN[242]=79642; 
w0mp[242]=0.102628; 
w1mp[242]=0.146926; 
w2mp[242]=0.052626; 
w3mp[242]=-0.353960; 
e2mp[242]=0.232087; 
e3mp[242]=0.296852; 
w0mn[242]=0.001530; 
w1mn[242]=0.086337; 
w2mn[242]=0.002139; 
w3mn[242]=-0.210166; 
e2mn[242]=0.139579; 
e3mn[242]=0.302394; 
RUN[243]=79643; 
w0mp[243]=0.056187; 
w1mp[243]=0.142661; 
w2mp[243]=0.092374; 
w3mp[243]=-0.320524; 
e2mp[243]=0.214502; 
e3mp[243]=0.273379; 
w0mn[243]=0.004402; 
w1mn[243]=0.046776; 
w2mn[243]=0.060781; 
w3mn[243]=-0.239058; 
e2mn[243]=0.109279; 
e3mn[243]=0.269054; 
RUN[244]=79740; 
w0mp[244]=0.123920; 
w1mp[244]=0.245737; 
w2mp[244]=-0.107897; 
w3mp[244]=-0.288358; 
e2mp[244]=0.253703; 
e3mp[244]=0.329180; 
w0mn[244]=0.070374; 
w1mn[244]=0.169010; 
w2mn[244]=-0.052076; 
w3mn[244]=-0.227498; 
e2mn[244]=0.175627; 
e3mn[244]=0.299987; 
RUN[245]=79749; 
w0mp[245]=0.290403; 
w1mp[245]=0.267921; 
w2mp[245]=0.019890; 
w3mp[245]=-0.342591; 
e2mp[245]=0.216137; 
e3mp[245]=0.420360; 
w0mn[245]=0.115066; 
w1mn[245]=0.164079; 
w2mn[245]=0.090294; 
w3mn[245]=-0.273257; 
e2mn[245]=0.144715; 
e3mn[245]=0.422585; 
RUN[246]=79750; 
w0mp[246]=0.147803; 
w1mp[246]=0.200009; 
w2mp[246]=0.076922; 
w3mp[246]=-0.334647; 
e2mp[246]=0.251788; 
e3mp[246]=0.312950; 
w0mn[246]=0.065022; 
w1mn[246]=0.153795; 
w2mn[246]=0.037497; 
w3mn[246]=-0.251577; 
e2mn[246]=0.200004; 
e3mn[246]=0.373886; 
RUN[247]=79762; 
w0mp[247]=0.207064; 
w1mp[247]=0.265715; 
w2mp[247]=0.047296; 
w3mp[247]=-0.231520; 
e2mp[247]=0.473213; 
e3mp[247]=0.614362; 
w0mn[247]=0.232047; 
w1mn[247]=0.280882; 
w2mn[247]=0.081257; 
w3mn[247]=-0.298223; 
e2mn[247]=0.264620; 
e3mn[247]=0.421777; 
RUN[248]=79764; 
w0mp[248]=0.206924; 
w1mp[248]=0.210049; 
w2mp[248]=-0.030854; 
w3mp[248]=-0.458179; 
e2mp[248]=0.224456; 
e3mp[248]=0.476267; 
w0mn[248]=0.022480; 
w1mn[248]=0.291073; 
w2mn[248]=0.098906; 
w3mn[248]=-0.273070; 
e2mn[248]=0.349712; 
e3mn[248]=0.392615; 
RUN[249]=79765; 
w0mp[249]=0.129646; 
w1mp[249]=0.212190; 
w2mp[249]=0.028259; 
w3mp[249]=-0.320810; 
e2mp[249]=0.186097; 
e3mp[249]=0.318558; 
w0mn[249]=0.062718; 
w1mn[249]=0.134762; 
w2mn[249]=0.040297; 
w3mn[249]=-0.293958; 
e2mn[249]=0.158996; 
e3mn[249]=0.270079; 
RUN[250]=79768; 
w0mp[250]=0.096831; 
w1mp[250]=0.117935; 
w2mp[250]=0.061754; 
w3mp[250]=-0.372696; 
e2mp[250]=0.188727; 
e3mp[250]=0.255606; 
w0mn[250]=-0.008275; 
w1mn[250]=0.108970; 
w2mn[250]=0.000907; 
w3mn[250]=-0.281760; 
e2mn[250]=0.131560; 
e3mn[250]=0.204418; 
RUN[251]=79769; 
w0mp[251]=0.209897; 
w1mp[251]=0.249225; 
w2mp[251]=0.049413; 
w3mp[251]=-0.308766; 
e2mp[251]=0.295679; 
e3mp[251]=0.408484; 
w0mn[251]=0.156862; 
w1mn[251]=0.171870; 
w2mn[251]=0.039061; 
w3mn[251]=-0.243137; 
e2mn[251]=0.254000; 
e3mn[251]=0.352469; 
RUN[252]=79770; 
w0mp[252]=0.212087; 
w1mp[252]=0.259939; 
w2mp[252]=0.352208; 
w3mp[252]=-0.497411; 
e2mp[252]=0.302179; 
e3mp[252]=0.343247; 
w0mn[252]=0.100034; 
w1mn[252]=0.128752; 
w2mn[252]=0.020597; 
w3mn[252]=-0.222802; 
e2mn[252]=0.202849; 
e3mn[252]=0.308251; 
RUN[253]=79863; 
w0mp[253]=0.246865; 
w1mp[253]=0.308277; 
w2mp[253]=0.111420; 
w3mp[253]=-0.706584; 
e2mp[253]=0.347505; 
e3mp[253]=0.652472; 
w0mn[253]=0.160929; 
w1mn[253]=0.227255; 
w2mn[253]=-0.010960; 
w3mn[253]=-0.235831; 
e2mn[253]=0.317418; 
e3mn[253]=0.519096; 
RUN[254]=79866; 
w0mp[254]=0.162920; 
w1mp[254]=0.219973; 
w2mp[254]=0.033124; 
w3mp[254]=-0.345593; 
e2mp[254]=0.228009; 
e3mp[254]=0.357534; 
w0mn[254]=0.044333; 
w1mn[254]=0.111140; 
w2mn[254]=0.026568; 
w3mn[254]=-0.322716; 
e2mn[254]=0.174975; 
e3mn[254]=0.258920; 
RUN[255]=79872; 
w0mp[255]=0.154237; 
w1mp[255]=0.281098; 
w2mp[255]=0.010342; 
w3mp[255]=-0.247774; 
e2mp[255]=0.249767; 
e3mp[255]=0.325162; 
w0mn[255]=0.062799; 
w1mn[255]=0.199661; 
w2mn[255]=0.079349; 
w3mn[255]=-0.238822; 
e2mn[255]=0.267496; 
e3mn[255]=0.300272; 
RUN[256]=79875; 
w0mp[256]=0.179787; 
w1mp[256]=0.225227; 
w2mp[256]=-0.009377; 
w3mp[256]=-0.364725; 
e2mp[256]=0.248789; 
e3mp[256]=0.328712; 
w0mn[256]=0.073804; 
w1mn[256]=0.190684; 
w2mn[256]=-0.016682; 
w3mn[256]=-0.291046; 
e2mn[256]=0.201026; 
e3mn[256]=0.339551; 
RUN[257]=79885; 
w0mp[257]=0.280992; 
w1mp[257]=0.331580; 
w2mp[257]=-0.110468; 
w3mp[257]=-0.465701; 
e2mp[257]=0.354956; 
e3mp[257]=0.422828; 
w0mn[257]=0.156987; 
w1mn[257]=0.304695; 
w2mn[257]=-0.120109; 
w3mn[257]=-0.390297; 
e2mn[257]=0.335438; 
e3mn[257]=0.444940; 
RUN[258]=79888; 
w0mp[258]=0.252070; 
w1mp[258]=0.262694; 
w2mp[258]=-0.112837; 
w3mp[258]=-0.523780; 
e2mp[258]=0.292142; 
e3mp[258]=0.370439; 
w0mn[258]=0.109288; 
w1mn[258]=0.273548; 
w2mn[258]=-0.136434; 
w3mn[258]=-0.447737; 
e2mn[258]=0.215309; 
e3mn[258]=0.335749; 
RUN[259]=79960; 
w0mp[259]=0.140323; 
w1mp[259]=0.092896; 
w2mp[259]=0.038853; 
w3mp[259]=-0.446095; 
e2mp[259]=0.270172; 
e3mp[259]=0.336200; 
w0mn[259]=0.188559; 
w1mn[259]=0.256131; 
w2mn[259]=-0.174567; 
w3mn[259]=-0.486889; 
e2mn[259]=0.123337; 
e3mn[259]=0.178039; 
RUN[260]=79961; 
w0mp[260]=0.255740; 
w1mp[260]=0.333747; 
w2mp[260]=-0.080500; 
w3mp[260]=-0.418069; 
e2mp[260]=0.375952; 
e3mp[260]=0.403755; 
w0mn[260]=0.191489; 
w1mn[260]=0.284981; 
w2mn[260]=-0.020614; 
w3mn[260]=-0.395600; 
e2mn[260]=0.313689; 
e3mn[260]=0.384523; 
RUN[261]=79962; 
w0mp[261]=0.193949; 
w1mp[261]=0.234247; 
w2mp[261]=-0.012333; 
w3mp[261]=-0.441667; 
e2mp[261]=0.318248; 
e3mp[261]=0.352804; 
w0mn[261]=0.063195; 
w1mn[261]=0.235105; 
w2mn[261]=-0.011006; 
w3mn[261]=-0.377491; 
e2mn[261]=0.291011; 
e3mn[261]=0.400673; 
RUN[262]=79963; 
w0mp[262]=0.084924; 
w1mp[262]=0.171127; 
w2mp[262]=0.003121; 
w3mp[262]=-0.423367; 
e2mp[262]=0.182240; 
e3mp[262]=0.268693; 
w0mn[262]=-0.052677; 
w1mn[262]=0.180859; 
w2mn[262]=-0.058442; 
w3mn[262]=-0.351048; 
e2mn[262]=0.182445; 
e3mn[262]=0.214603; 
RUN[263]=79964; 
w0mp[263]=0.081219; 
w1mp[263]=0.081313; 
w2mp[263]=-0.001417; 
w3mp[263]=-0.406544; 
e2mp[263]=0.206307; 
e3mp[263]=0.270999; 
w0mn[263]=-0.019163; 
w1mn[263]=0.085273; 
w2mn[263]=-0.077092; 
w3mn[263]=-0.337333; 
e2mn[263]=0.122293; 
e3mn[263]=0.259245; 
RUN[264]=80127; 
w0mp[264]=0.007355; 
w1mp[264]=0.078769; 
w2mp[264]=-0.210526; 
w3mp[264]=-0.504767; 
e2mp[264]=0.055979; 
e3mp[264]=0.043547; 
w0mn[264]=-0.033499; 
w1mn[264]=0.069058; 
w2mn[264]=-0.170689; 
w3mn[264]=-0.493373; 
e2mn[264]=0.010906; 
e3mn[264]=0.162187; 
RUN[265]=80128; 
w0mp[265]=0.124077; 
w1mp[265]=0.073553; 
w2mp[265]=-0.120754; 
w3mp[265]=-0.447373; 
e2mp[265]=0.013092; 
e3mp[265]=0.150163; 
w0mn[265]=-0.088296; 
w1mn[265]=0.033263; 
w2mn[265]=-0.098720; 
w3mn[265]=-0.518664; 
e2mn[265]=0.011629; 
e3mn[265]=0.087642; 
RUN[266]=80139; 
w0mp[266]=0.043708; 
w1mp[266]=0.101374; 
w2mp[266]=-0.119008; 
w3mp[266]=-0.462831; 
e2mp[266]=0.064466; 
e3mp[266]=0.169247; 
w0mn[266]=-0.035164; 
w1mn[266]=0.073047; 
w2mn[266]=-0.160580; 
w3mn[266]=-0.482240; 
e2mn[266]=0.021524; 
e3mn[266]=0.166732; 
RUN[267]=80141; 
w0mp[267]=0.049460; 
w1mp[267]=0.068892; 
w2mp[267]=-0.102151; 
w3mp[267]=-0.455602; 
e2mp[267]=0.118445; 
e3mp[267]=0.111773; 
w0mn[267]=-0.002840; 
w1mn[267]=0.120119; 
w2mn[267]=-0.054335; 
w3mn[267]=-0.421408; 
e2mn[267]=0.036215; 
e3mn[267]=0.135696; 
RUN[268]=80152; 
w0mp[268]=0.077122; 
w1mp[268]=0.063278; 
w2mp[268]=-0.158333; 
w3mp[268]=-0.531796; 
e2mp[268]=0.071514; 
e3mp[268]=0.148147; 
w0mn[268]=-0.011520; 
w1mn[268]=0.050189; 
w2mn[268]=-0.187948; 
w3mn[268]=-0.472739; 
e2mn[268]=0.014933; 
e3mn[268]=0.107976; 
RUN[269]=80304; 
w0mp[269]=0.052310; 
w1mp[269]=0.180157; 
w2mp[269]=-0.122352; 
w3mp[269]=-0.380179; 
e2mp[269]=0.116360; 
e3mp[269]=0.166905; 
w0mn[269]=0.013872; 
w1mn[269]=0.093864; 
w2mn[269]=-0.052121; 
w3mn[269]=-0.356260; 
e2mn[269]=0.168585; 
e3mn[269]=0.172899; 
RUN[270]=80312; 
w0mp[270]=0.003670; 
w1mp[270]=0.135395; 
w2mp[270]=-0.094902; 
w3mp[270]=-0.494238; 
e2mp[270]=0.086202; 
e3mp[270]=0.180685; 
w0mn[270]=-0.040016; 
w1mn[270]=0.061387; 
w2mn[270]=-0.128158; 
w3mn[270]=-0.422733; 
e2mn[270]=0.048641; 
e3mn[270]=0.193112; 

//slewcorr_only_run3dAu.txt
w0p0=-0.581737;
w0p1=0.386820;
w0p2=0.508952;
w1p0=-0.570022;
w1p1=0.361083;
w1p2=0.401784;
w2p0=-0.431391;
w2p1=0.381383;
w2p2=0.588911;
w3p0=-0.453745;
w3p1=0.298597;
w3p2=0.482855;
e2p0=-0.588619;
e2p1=0.389236;
e2p2=0.477815;
e3p0=-0.632011;
e3p1=0.411054;
e3p2=0.457165;
w0n0=-0.419868;
w0n1=0.396839;
w0n2=0.562497;
w1n0=-0.480877;
w1n1=0.365940;
w1n2=0.418476;
w2n0=-0.335586;
w2n1=0.411226;
w2n2=0.647026;
w3n0=-0.333389;
w3n1=0.314864;
w3n2=0.586176;
e2n0=-0.458641;
e2n1=0.412198;
e2n2=0.498897;
e3n0=-0.508345;
e3n1=0.410857;
e3n2=0.485581;

//momcorr_only_run3dAu.txt
w0p0m=-0.106582;
w0p1m=0.000766;
w0p2m=0.030827;
w1p0m=-0.158652;
w1p1m=-0.027608;
w1p2m=0.044439;
w2p0m=-0.149432;
w2p1m=0.023983;
w2p2m=0.023067;
w3p0m=-0.058646;
w3p1m=-0.042943;
w3p2m=0.031172;
e2p0m=-0.100298;
e2p1m=-0.029901;
e2p2m=0.038669;
e3p0m=-0.096977;
e3p1m=-0.024606;
e3p2m=0.037146;
w0n0m=-0.144385;
w0n1m=0.031380;
w0n2m=0.025335;
w1n0m=-0.031658;
w1n1m=-0.081031;
w1n2m=0.043899;
w2n0m=-0.175818;
w2n1m=0.055143;
w2n2m=0.017243;
w3n0m=-0.109452;
w3n1m=0.026269;
w3n2m=0.016794;
e2n0m=-0.109620;
e2n1m=-0.018574;
e2n2m=0.033393;
e3n0m=-0.100837;
e3n1m=-0.046636;
e3n2m=0.043103;

//M2_pi_fit_run3dAu.txt
w0mp0_pi=0.021309;
w0mp1_pi=0.003254;
w0mp2_pi=-0.011113;
w0mp3_pi=0.007291;
w1mp0_pi=0.023021;
w1mp1_pi=-0.009701;
w1mp2_pi=0.008845;
w1mp3_pi=-0.000504;
w2mp0_pi=0.020487;
w2mp1_pi=0.000933;
w2mp2_pi=-0.007332;
w2mp3_pi=0.004751;
w3mp0_pi=0.018807;
w3mp1_pi=0.006098;
w3mp2_pi=-0.010233;
w3mp3_pi=0.004854;
e2mp0_pi=0.021406;
e2mp1_pi=0.000102;
e2mp2_pi=-0.006275;
e2mp3_pi=0.004629;
e3mp0_pi=0.022942;
e3mp1_pi=-0.001432;
e3mp2_pi=-0.008099;
e3mp3_pi=0.006137;
w0mn0_pi=0.016182;
w0mn1_pi=0.029866;
w0mn2_pi=-0.044710;
w0mn3_pi=0.019087;
w1mn0_pi=0.019416;
w1mn1_pi=0.011505;
w1mn2_pi=-0.016690;
w1mn3_pi=0.005888;
w2mn0_pi=0.017338;
w2mn1_pi=0.020726;
w2mn2_pi=-0.035835;
w2mn3_pi=0.017079;
w3mn0_pi=0.017270;
w3mn1_pi=0.018795;
w3mn2_pi=-0.027345;
w3mn3_pi=0.011205;
e2mn0_pi=0.017710;
e2mn1_pi=0.020125;
e2mn2_pi=-0.031616;
e2mn3_pi=0.013114;
e3mn0_pi=0.016863;
e3mn1_pi=0.025792;
e3mn2_pi=-0.039208;
e3mn3_pi=0.016538;
w0sn0_pi=0.009301;
w0sn1_pi=-0.016523;
w0sn2_pi=0.065226;
w1sn0_pi=0.010275;
w1sn1_pi=-0.022306;
w1sn2_pi=0.070276;
w2sn0_pi=0.009800;
w2sn1_pi=-0.019422;
w2sn2_pi=0.069436;
w3sn0_pi=0.010578;
w3sn1_pi=-0.021397;
w3sn2_pi=0.075729;
e2sn0_pi=0.008765;
e2sn1_pi=-0.015765;
e2sn2_pi=0.064913;
e3sn0_pi=0.012177;
e3sn1_pi=-0.024627;
e3sn2_pi=0.073610;
w0sn0_pi=0.007412;
w0sn1_pi=-0.006327;
w0sn2_pi=0.057903;
w1sn0_pi=0.009769;
w1sn1_pi=-0.014150;
w1sn2_pi=0.064607;
w2sn0_pi=0.008517;
w2sn1_pi=-0.012320;
w2sn2_pi=0.062186;
w3sn0_pi=0.007042;
w3sn1_pi=-0.004583;
w3sn2_pi=0.061487;
e2sn0_pi=0.007319;
e2sn1_pi=-0.007257;
e2sn2_pi=0.060564;
e3sn0_pi=0.009180;
e3sn1_pi=-0.009250;
e3sn2_pi=0.061234;

//M2_K_fit_run3dAu.txt
w0mp0_K=0.292404;
w0mp1_K=0.026323;
w0mp2_K=-0.108153;
w1mp0_K=0.285976;
w1mp1_K=0.049083;
w1mp2_K=-0.122486;
w2mp0_K=0.298036;
w2mp1_K=-0.020791;
w2mp2_K=-0.068876;
w3mp0_K=0.145493;
w3mp1_K=0.426104;
w3mp2_K=-0.397298;
e2mp0_K=0.275403;
e2mp1_K=0.056680;
e2mp2_K=-0.114000;
e3mp0_K=0.274386;
e3mp1_K=0.066835;
e3mp2_K=-0.135143;
w0mn0_K=0.233565;
w0mn1_K=0.164949;
w0mn2_K=-0.192479;
w1mn0_K=0.243570;
w1mn1_K=0.141269;
w1mn2_K=-0.176878;
w2mn0_K=0.235097;
w2mn1_K=0.129300;
w2mn2_K=-0.155130;
w3mn0_K=0.212854;
w3mn1_K=0.213233;
w3mn2_K=-0.246105;
e2mn0_K=0.229091;
e2mn1_K=0.155621;
e2mn2_K=-0.177405;
e3mn0_K=0.227590;
e3mn1_K=0.166318;
e3mn2_K=-0.200174;
w0sp0_K=0.063127;
w0sp1_K=-0.141062;
w0sp2_K=0.175324;
w1sp0_K=0.050144;
w1sp1_K=-0.103675;
w1sp2_K=0.152186;
w2sp0_K=0.061323;
w2sp1_K=-0.135791;
w2sp2_K=0.165905;
w3sp0_K=0.107103;
w3sp1_K=-0.244907;
w3sp2_K=0.249903;
e2sp0_K=0.068352;
e2sp1_K=-0.151327;
e2sp2_K=0.173120;
e3sp0_K=0.088286;
e3sp1_K=-0.200599;
e3sp2_K=0.212141;
w0sn0_K=0.055540;
w0sn1_K=-0.146865;
w0sn2_K=0.198434;
w1sn0_K=0.064594;
w1sn1_K=-0.180781;
w1sn2_K=0.221527;
w2sn0_K=0.046295;
w2sn1_K=-0.118252;
w2sn2_K=0.166256;
w3sn0_K=0.048798;
w3sn1_K=-0.127170;
w3sn2_K=0.193273;
e2sn0_K=0.046014;
e2sn1_K=-0.116171;
e2sn2_K=0.170354;
e3sn0_K=0.051575;
e3sn1_K=-0.136350;
e3sn2_K=0.193516;


//M2_p_fit_run3dAu.txt
w0mp0_p=0.988650;
w0mp1_p=-0.005322;
w0mp2_p=-0.027817;
w1mp0_p=0.989797;
w1mp1_p=0.003326;
w1mp2_p=-0.029383;
w2mp0_p=0.994802;
w2mp1_p=-0.078905;
w2mp2_p=0.006322;
w3mp0_p=0.966443;
w3mp1_p=-0.016658;
w3mp2_p=-0.035271;
e2mp0_p=1.011308;
e2mp1_p=-0.054261;
e2mp2_p=-0.001282;
e3mp0_p=1.031542;
e3mp1_p=-0.103369;
e3mp2_p=0.016105;
w0mn0_p=0.985928;
w0mn1_p=0.048887;
w0mn2_p=-0.057195;
w1mn0_p=1.002188;
w1mn1_p=0.013635;
w1mn2_p=-0.046078;
w2mn0_p=1.021060;
w2mn1_p=-0.079604;
w2mn2_p=-0.004690;
w3mn0_p=1.005443;
w3mn1_p=-0.056078;
w3mn2_p=-0.031812;
e2mn0_p=0.998091;
e2mn1_p=-0.013354;
e2mn2_p=-0.040885;
e3mn0_p=0.998056;
e3mn1_p=-0.030553;
e3mn2_p=-0.028062;
w0sp0_p=0.163624;
w0sp1_p=-0.222889;
w0sp2_p=0.158377;
w1sp0_p=0.171021;
w1sp1_p=-0.235983;
w1sp2_p=0.167877;
w2sp0_p=0.158880;
w2sp1_p=-0.217583;
w2sp2_p=0.156555;
w3sp0_p=0.170066;
w3sp1_p=-0.202509;
w3sp2_p=0.156431;
e2sp0_p=0.135898;
e2sp1_p=-0.182511;
e2sp2_p=0.140659;
e3sp0_p=0.150103;
e3sp1_p=-0.193901;
e3sp2_p=0.147776;
w0sn0_p=0.065255;
w0sn1_p=-0.039228;
w0sn2_p=0.088708;
w1sn0_p=0.059788;
w1sn1_p=-0.032082;
w1sn2_p=0.086842;
w2sn0_p=0.051152;
w2sn1_p=-0.007455;
w2sn2_p=0.069177;
w3sn0_p=0.022747;
w3sn1_p=0.067168;
w3sn2_p=0.049857;
e2sn0_p=0.056725;
e2sn1_p=-0.030212;
e2sn2_p=0.086724;
e3sn0_p=0.048886;
e3sn1_p=-0.002410;
e3sn2_p=0.072319;


}
