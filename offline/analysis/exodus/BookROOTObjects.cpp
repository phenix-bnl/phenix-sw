//-----------------------------------------------------------------------------
//
//  Book ROOT objects declared in DeclareROOTObjects
//
//-----------------------------------------------------------------------------

#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include <TNtuple.h>
#include <cmath>
#include "BookROOTObjects.h"

#define INCLUDEFLAG
#include "DeclareROOTObjects.h"

void BookROOTObjects()
{
  primaries = new TNtuple("primaries","primary-particle ntuple",
      "mass:weight:p:th:ph:pid");
  pairs = new TNtuple("pairs","electron-pair ntuple",
      "minv:opangle:weight:sece:pe:the:phe:secp:pp:thp:php:p:th:ph:pid");
  singles = new TNtuple("singles","single-electron ntuple",
      "weight:charge:sece:pe:the:phe:pide:p:th:ph:pid");

  hpairinvm = new TH1D("hpairinvm","Mass",150,0.0,1.5);

  acc_exodusphi = new TH2D("acc_exodusphi","acc_exodusphi",450,-4,6.5,250,-6,6);
  acc = new TH2D("acc","acc",250,-2,5,250,-6,6);
  acc_fid1 = new TH2D("acc_fid1","acc_fid1",250,-2,5,250,-6,6);
  acc_fid2 = new TH2D("acc_fid2","acc_fid2",250,-2,5,250,-6,6);
  acc_fid3 = new TH2D("acc_fid3","acc_fid3",250,-2,5,250,-6,6);
  acc_fid4 = new TH2D("acc_fid4","acc_fid4",250,-2,5,250,-6,6);

  acc_the0 = new TH2D("acc_the0","acc_the0",60,-30,30,100,1,2);
  acc_the0_fid = new TH2D("acc_the0_fid","acc_the0_fid",60,-30,30,100,1,2);

  pte           = new TH1D("pte","pte",100,0.,10.);
  pte_trig      = new TH1D("pte_trig","pte_trig",100,0.,10.);
  pteC          = new TH1D("pteC","pteC",100,0.,10.);
  ptePion       = new TH1D("ptePion","ptePion",100,0.,10.);
  pteEta        = new TH1D("pteEta","pteEta",100,0.,10.);
  pteEtaprime   = new TH1D("pteEtaprime","pteEtaprime",100,0.,10.);
  pteRho        = new TH1D("pteRho","pteRho",100,0.,10.);
  pteOmega      = new TH1D("pteOmega","pteOmega",100,0.,10.);
  ptePhi        = new TH1D("ptePhi","ptePhi",100,0.,10.);
  pteJPsi       = new TH1D("pteJPsi","pteJPsi",100,0.,10.);
  pteCPion      = new TH1D("pteCPion","pteCPion",100,0.,10.);
  pteCEta       = new TH1D("pteCEta","pteCEta",100,0.,10.);
  pteCEtaprime  = new TH1D("pteCEtaprime","pteCEtaprime",100,0.,10.);
  pteCOmega     = new TH1D("pteCOmega","pteCOmega",100,0.,10.);
  pteGamma      = new TH1D("pteGamma","pteGamma",100,0.,10.);
  pteCGamma     = new TH1D("pteCGamma","pteCGamma",100,0.,10.);
  pteKe3        = new TH1D("pteKe3","pteKe3",100,0.,10.);
  pte2          = new TH1D("pte2","pte2",100,0.,10.);
  pte2C         = new TH1D("pte2C","pte2C",100,0.,10.);
  pte2Pion      = new TH1D("pte2Pion","pte2Pion",100,0.,10.);
  pte2Eta       = new TH1D("pte2Eta","pte2Eta",100,0.,10.);
  pte2Etaprime  = new TH1D("pte2Etaprime","pte2Etaprime",100,0.,10.);
  pte2Rho       = new TH1D("pte2Rho","pte2Rho",100,0.,10.);
  pte2Omega     = new TH1D("pte2Omega","pte2Omega",100,0.,10.);
  pte2Phi       = new TH1D("pte2Phi","pte2Phi",100,0.,10.);
  pte2JPsi      = new TH1D("pte2JPsi","pte2JPsi",100,0.,10.);
  pte2CPion     = new TH1D("pte2CPion","pte2CPion",100,0.,10.);
  pte2CEta      = new TH1D("pte2CEta","pte2CEta",100,0.,10.);
  pte2CEtaprime = new TH1D("pte2CEtaprime","pte2CEtaprime",100,0.,10.);
  pte2COmega    = new TH1D("pte2COmega","pte2COmega",100,0.,10.);
  pte2Gamma     = new TH1D("pte2Gamma","pte2Gamma",100,0.,10.);
  pte2CGamma    = new TH1D("pte2CGamma","pte2CGamma",100,0.,10.);
  pte2Ke3       = new TH1D("pte2Ke3","pte2Ke3",100,0.,10.);

  const int nbin = 13;
  const double bin[nbin] = { 0.4, 0.5, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 2.0,
    2.5, 3.0, 4.0, 5.0};
  pteR           = new TH1D("pteR","pteR",nbin-1,bin);
  pteRC          = new TH1D("pteRC","pteRC",nbin-1,bin);
  pteRPion       = new TH1D("pteRPion","pteRPion",nbin-1,bin);
  pteREta        = new TH1D("pteREta","pteREta",nbin-1,bin);
  pteREtaprime   = new TH1D("pteREtaprime","pteREtaprime",nbin-1,bin);
  pteRRho        = new TH1D("pteRRho","pteRRho",nbin-1,bin);
  pteROmega      = new TH1D("pteROmega","pteROmega",nbin-1,bin);
  pteRPhi        = new TH1D("pteRPhi","pteRPhi",nbin-1,bin);
  pteRCPion      = new TH1D("pteRCPion","pteRCPion",nbin-1,bin);
  pteRCEta       = new TH1D("pteRCEta","pteRCEta",nbin-1,bin);
  pteRCEtaprime  = new TH1D("pteRCEtaprime","pteRCEtaprime",nbin-1,bin);
  pteRCOmega     = new TH1D("pteRCOmega","pteRCOmega",nbin-1,bin);
  pteRGamma      = new TH1D("pteRGamma","pteRGamma",nbin-1,bin);
  pteRCGamma     = new TH1D("pteRCGamma","pteRCGamma",nbin-1,bin);
  pteRKe3        = new TH1D("pteRKe3","pteRKe3",nbin-1,bin);
  pteR2          = new TH1D("pteR2","pteR2",nbin-1,bin);
  pteR2C         = new TH1D("pteR2C","pteR2C",nbin-1,bin);
  pteR2Pion      = new TH1D("pteR2Pion","pteR2Pion",nbin-1,bin);
  pteR2Eta       = new TH1D("pteR2Eta","pteR2Eta",nbin-1,bin);
  pteR2Etaprime  = new TH1D("pteR2Etaprime","pteR2Etaprime",nbin-1,bin);
  pteR2Rho       = new TH1D("pteR2Rho","pteR2Rho",nbin-1,bin);
  pteR2Omega     = new TH1D("pteR2Omega","pteR2Omega",nbin-1,bin);
  pteR2Phi       = new TH1D("pteR2Phi","pteR2Phi",nbin-1,bin);
  pteR2CPion     = new TH1D("pteR2CPion","pteR2CPion",nbin-1,bin);
  pteR2CEta      = new TH1D("pteR2CEta","pteR2CEta",nbin-1,bin);
  pteR2CEtaprime = new TH1D("pteR2CEtaprime","pteR2CEtaprime",nbin-1,bin);
  pteR2COmega    = new TH1D("pteR2COmega","pteR2COmega",nbin-1,bin);
  pteR2Gamma     = new TH1D("pteR2Gamma","pteR2Gamma",nbin-1,bin);
  pteR2CGamma    = new TH1D("pteR2CGamma","pteR2CGamma",nbin-1,bin);
  pteR2Ke3       = new TH1D("pteR2Ke3","pteR2Ke3",nbin-1,bin);

  const int nbinT = 11;
  const double binT[nbinT] = { 0.4, 0.5, 0.6, 0.8, 1.0, 1.2, 1.4, 2.0, 3.0, 4.0, 5.0};
  pteRT           = new TH1D("pteRT","pteRT",nbinT-1,binT);
  pteRTC          = new TH1D("pteRTC","pteRTC",nbinT-1,binT);
  pteRTPion       = new TH1D("pteRTPion","pteRTPion",nbinT-1,binT);
  pteRTEta        = new TH1D("pteRTEta","pteRTEta",nbinT-1,binT);
  pteRTEtaprime   = new TH1D("pteRTEtaprime","pteRTEtaprime",nbinT-1,binT);
  pteRTRho        = new TH1D("pteRTRho","pteRTRho",nbinT-1,binT);
  pteRTOmega      = new TH1D("pteRTOmega","pteRTOmega",nbinT-1,binT);
  pteRTPhi        = new TH1D("pteRTPhi","pteRTPhi",nbinT-1,binT);
  pteRTCPion      = new TH1D("pteRTCPion","pteRTCPion",nbinT-1,binT);
  pteRTCEta       = new TH1D("pteRTCEta","pteRTCEta",nbinT-1,binT);
  pteRTCEtaprime  = new TH1D("pteRTCEtaprime","pteRTCEtaprime",nbinT-1,binT);
  pteRTCOmega     = new TH1D("pteRTCOmega","pteRTCOmega",nbinT-1,binT);
  pteRTGamma      = new TH1D("pteRTGamma","pteRTGamma",nbinT-1,binT);
  pteRTCGamma     = new TH1D("pteRTCGamma","pteRTCGamma",nbinT-1,binT);
  pteRTKe3        = new TH1D("pteRTKe3","pteRTKe3",nbinT-1,binT);
  pteRT2          = new TH1D("pteRT2","pteRT2",nbinT-1,binT);
  pteRT2C         = new TH1D("pteRT2C","pteRT2C",nbinT-1,binT);
  pteRT2Pion      = new TH1D("pteRT2Pion","pteRT2Pion",nbinT-1,binT);
  pteRT2Eta       = new TH1D("pteRT2Eta","pteRT2Eta",nbinT-1,binT);
  pteRT2Etaprime  = new TH1D("pteRT2Etaprime","pteRT2Etaprime",nbinT-1,binT);
  pteRT2Rho       = new TH1D("pteRT2Rho","pteRT2Rho",nbinT-1,binT);
  pteRT2Omega     = new TH1D("pteRT2Omega","pteRT2Omega",nbinT-1,binT);
  pteRT2Phi       = new TH1D("pteRT2Phi","pteRT2Phi",nbinT-1,binT);
  pteRT2CPion     = new TH1D("pteRT2CPion","pteRT2CPion",nbinT-1,binT);
  pteRT2CEta      = new TH1D("pteRT2CEta","pteRT2CEta",nbinT-1,binT);
  pteRT2CEtaprime = new TH1D("pteRT2CEtaprime","pteRT2CEtaprime",nbinT-1,binT);
  pteRT2COmega    = new TH1D("pteRT2COmega","pteRT2COmega",nbinT-1,binT);
  pteRT2Gamma     = new TH1D("pteRT2Gamma","pteRT2Gamma",nbinT-1,binT);
  pteRT2CGamma    = new TH1D("pteRT2CGamma","pteRT2CGamma",nbinT-1,binT);
  pteRT2Ke3       = new TH1D("pteRT2Ke3","pteRT2Ke3",nbinT-1,binT);

  ptg            = new TH1D("ptg","ptg",100,0.,10.);
  ptgPion        = new TH1D("ptgPion","ptgPion",100,0.,10.);
  ptgEta         = new TH1D("ptgEta","ptgEta",100,0.,10.);
  ptgEtaprime    = new TH1D("ptgEtaprime","ptgEtaprime",100,0.,10.);
  ptgOmega       = new TH1D("ptgOmega","ptgOmega",100,0.,10.);
  ptgPhi         = new TH1D("ptgPhi","ptgPhi",100,0.,10.);
  ptgGamma       = new TH1D("ptgGamma","ptgGamma",100,0.,10.);

  mee            = new TH1D("mee","mee",14000,0.,14.);
  mee_accP       = new TH1D("mee_accP","mee_accS",14000,0.,14.);
  mee_accS       = new TH1D("mee_accS","mee_accS",14000,0.,14.);
  meePion        = new TH1D("meePion","meePion",14000,0.,14.);
  meeEta         = new TH1D("meeEta","meeEta",14000,0.,14.);
  meeEtaprime    = new TH1D("meeEtaprime","meeEtaprime",14000,0.,14.);
  meeRho         = new TH1D("meeRho","meeRho",14000,0.,14.);
  meeOmega       = new TH1D("meeOmega","meeOmega",14000,0.,14.);
  meePhi         = new TH1D("meePhi","meePhi",14000,0.,14.);
  meeJPsi        = new TH1D("meeJPsi","meeJPsi",14000,0.,14.);
  meePsiprime    = new TH1D("meePsiprime","meePsiprime",14000,0.,14.);
  meeUpsilon1S   = new TH1D("meeUpsilon1S","meeUpsilon1S",14000,0.,14.);
  meeUpsilon2S   = new TH1D("meeUpsilon2S","meeUpsilon2S",14000,0.,14.);
  meeUpsilon3S   = new TH1D("meeUpsilon3S","meeUpsilon3S",14000,0.,14.);

  mptee          = new TH2D("mptee","mptee",2800,0.,14., 160,0.,8.);
  mptee_eid      = new TH2D("mptee_eid","mptee_eid",800,0.,4., 80,0.,8.);
  mptee_ert      = new TH2D("mptee_ert","mptee_ert",800,0.,4., 80,0.,8.);
  mptee_accS     = new TH2D("mptee_accS","mptee_accS",800,0.,4., 80,0.,8.);
  mptee_accS_fid1= new TH2D("mptee_accS_fid1","mptee_accS_fid1",800,0.,4., 80,0.,8.);
  mptee_accS_fid2= new TH2D("mptee_accS_fid2","mptee_accS_fid2",800,0.,4., 80,0.,8.);
  mptee_accS_fid3= new TH2D("mptee_accS_fid3","mptee_accS_fid3",800,0.,4., 80,0.,8.);
  mptee_accS_fid4= new TH2D("mptee_accS_fid4","mptee_accS_fid4",800,0.,4., 80,0.,8.);
  mptee_accS_eid_fid1= new TH2D("mptee_accS_eid_fid1","mptee_accS_eid_fid1",800,0.,4., 80,0.,8.);
  mptee_accS_eid_fid2= new TH2D("mptee_accS_eid_fid2","mptee_accS_eid_fid2",800,0.,4., 80,0.,8.);
  mptee_accS_eid_fid3= new TH2D("mptee_accS_eid_fid3","mptee_accS_eid_fid3",800,0.,4., 80,0.,8.);
  mptee_accS_eid_fid4= new TH2D("mptee_accS_eid_fid4","mptee_accS_eid_fid4",800,0.,4., 80,0.,8.);
  mptee_accP     = new TH2D("mptee_accP","mptee_accP",800,0.,4., 80,0.,8.);
  mptee_EMCghost = new TH2D("mptee_EMCghost","mptee_EMCghost",800,0.,4., 80,0.,8.);
  mptee_Pion     = new TH2D("mptee_Pion","mptee_Pion",1400,0.,14., 80,0.,8.);
  mptee_Eta      = new TH2D("mptee_Eta","mptee_Eta",1400,0.,14., 80,0.,8.);
  mptee_Etaprime = new TH2D("mptee_Etaprime","mptee_Etaprime",1400,0.,14., 80,0.,8.);
  mptee_Rho      = new TH2D("mptee_Rho","mptee_Rho",1400,0.,14., 80,0.,8.);
  mptee_Omega    = new TH2D("mptee_Omega","mptee_Omega",1400,0.,14., 80,0.,8.);
  mptee_Phi      = new TH2D("mptee_Phi","mptee_Phi",1400,0.,14., 80,0.,8.);
  mptee_JPsi     = new TH2D("mptee_JPsi","mptee_JPsi",1400,0.,14., 80,0.,8.);
  mptee_Psiprime = new TH2D("mptee_Psiprime","mptee_Psiprime",1400,0.,14., 80,0.,8.);
  mptee_Upsilon1S= new TH2D("mptee_Upsilon1S","mptee_Upsilon1S",1400,0.,14., 80,0.,8.);
  mptee_Upsilon2S= new TH2D("mptee_Upsilon2S","mptee_Upsilon2S",1400,0.,14., 80,0.,8.);
  mptee_Upsilon3S= new TH2D("mptee_Upsilon3S","mptee_Upsilon3S",1400,0.,14., 80,0.,8.);
  mphiVee        = new TH2D("mphiVee","mphiVee",180,0.,std::acos(-1.),800,0.,4.);
  mptphiVee      = new TH3D("mptphiVee","mptphiVee",10,0.0,5.0, 800,0.0,4.0, 180,0.0,std::acos(-1.0) );
  mptphiVee_rec  = new TH3D("mptphiVee_rec","mptphiVee_rec",10,0.0,5.0, 800,0.0,4.0, 180,0.0,std::acos(-1.0) );
  ptJPsi         = new TH1D("ptJPsi","ptJPsi",60,0.,6.);
  ptPhi          = new TH1D("ptPhi","ptPhi",60,0.,6.);

  ptyeePion        = new TH2D("ptyeePion","ptyeePion",100,-0.5,0.5,80,0,8);
  ptyeeEta         = new TH2D("ptyeeEta","ptyeeEta",100,-0.5,0.5,80,0,8);
  ptyeeEtaprime    = new TH2D("ptyeeEtaprime","ptyeeEtaprime",100,-0.5,0.5,80,0,8);
  ptyeeRho         = new TH2D("ptyeeRho","ptyeeRho",100,-0.5,0.5,80,0,8);
  ptyeeOmega       = new TH2D("ptyeeOmega","ptyeeOmega",100,-0.5,0.5,80,0,8);
  ptyeePhi         = new TH2D("ptyeePhi","ptyeePhi",100,-0.5,0.5,80,0,8);
  ptyeeJPsi        = new TH2D("ptyeeJPsi","ptyeeJPsi",100,-0.5,0.5,80,0,8);
  ptyeePsiprime    = new TH2D("ptyeePsiprime","ptyeePsiprime",100,-0.5,0.5,80,0,8);

  mgg            = new TH1D("mgg","mgg",200,0.,5.);
  mggPion        = new TH1D("mggPion","mggPion",200,0.,5.);
  mggEta         = new TH1D("mggEta","mggEta",200,0.,5.);
  mggEtaprime    = new TH1D("mggEtaprime","mggEtaprime",200,0.,5.);
  mggRho         = new TH1D("mggRho","mggRho",200,0.,5.);
  mggOmega       = new TH1D("mggOmega","mggOmega",200,0.,5.);
  mggPhi         = new TH1D("mggPhi","mggPhi",200,0.,5.);

  acc->Sumw2();
  acc_fid1->Sumw2();
  acc_fid2->Sumw2();
  acc_fid3->Sumw2();
  acc_fid4->Sumw2();

  pte->Sumw2();
  pteC->Sumw2();
  ptePion->Sumw2();
  pteEta->Sumw2();
  pteEtaprime->Sumw2();
  pteRho->Sumw2();
  pteOmega->Sumw2();
  ptePhi->Sumw2();
  pteCPion->Sumw2();
  pteCEta->Sumw2();
  pteCEtaprime->Sumw2();
  pteCOmega->Sumw2();
  pteGamma->Sumw2();
  pteCGamma->Sumw2();
  pteKe3->Sumw2();
  pte2->Sumw2();
  pte2C->Sumw2();
  pte2Pion->Sumw2();
  pte2Eta->Sumw2();
  pte2Etaprime->Sumw2();
  pte2Rho->Sumw2();
  pte2Omega->Sumw2();
  pte2Phi->Sumw2();
  pte2CPion->Sumw2();
  pte2CEta->Sumw2();
  pte2CEtaprime->Sumw2();
  pte2COmega->Sumw2();
  pte2Gamma->Sumw2();
  pte2CGamma->Sumw2();
  pte2Ke3->Sumw2();
  pteR->Sumw2();
  pteRC->Sumw2();
  pteRPion->Sumw2();
  pteREta->Sumw2();
  pteREtaprime->Sumw2();
  pteRRho->Sumw2();
  pteROmega->Sumw2();
  pteRPhi->Sumw2();
  pteRCPion->Sumw2();
  pteRCEta->Sumw2();
  pteRCEtaprime->Sumw2();
  pteRCOmega->Sumw2();
  pteRGamma->Sumw2();
  pteRCGamma->Sumw2();
  pteRKe3->Sumw2();
  pteR2->Sumw2();
  pteR2C->Sumw2();
  pteR2Pion->Sumw2();
  pteR2Eta->Sumw2();
  pteR2Etaprime->Sumw2();
  pteR2Rho->Sumw2();
  pteR2Omega->Sumw2();
  pteR2Phi->Sumw2();
  pteR2CPion->Sumw2();
  pteR2CEta->Sumw2();
  pteR2CEtaprime->Sumw2();
  pteR2COmega->Sumw2();
  pteR2Gamma->Sumw2();
  pteR2CGamma->Sumw2();
  pteR2Ke3->Sumw2();
  pteRT->Sumw2();
  pteRTC->Sumw2();
  pteRTPion->Sumw2();
  pteRTEta->Sumw2();
  pteRTEtaprime->Sumw2();
  pteRTRho->Sumw2();
  pteRTOmega->Sumw2();
  pteRTPhi->Sumw2();
  pteRTCPion->Sumw2();
  pteRTCEta->Sumw2();
  pteRTCEtaprime->Sumw2();
  pteRTCOmega->Sumw2();
  pteRTGamma->Sumw2();
  pteRTCGamma->Sumw2();
  pteRTKe3->Sumw2();
  pteRT2->Sumw2();
  pteRT2C->Sumw2();
  pteRT2Pion->Sumw2();
  pteRT2Eta->Sumw2();
  pteRT2Etaprime->Sumw2();
  pteRT2Rho->Sumw2();
  pteRT2Omega->Sumw2();
  pteRT2Phi->Sumw2();
  pteRT2CPion->Sumw2();
  pteRT2CEta->Sumw2();
  pteRT2CEtaprime->Sumw2();
  pteRT2COmega->Sumw2();
  pteRT2Gamma->Sumw2();
  pteRT2CGamma->Sumw2();
  pteRT2Ke3->Sumw2();
  ptg->Sumw2();
  ptgPion->Sumw2();
  ptgEta->Sumw2();
  ptgEtaprime->Sumw2();
  ptgOmega->Sumw2();
  ptgPhi->Sumw2();
  ptgGamma->Sumw2();
  mee->Sumw2();
  mee_accP->Sumw2();
  mee_accS->Sumw2();
  meePion->Sumw2();
  meeEta->Sumw2();
  meeEtaprime->Sumw2();
  meeRho->Sumw2();
  meeOmega->Sumw2();
  meePhi->Sumw2();
  meeJPsi->Sumw2();
  meePsiprime->Sumw2();
  meeUpsilon1S->Sumw2();
  meeUpsilon2S->Sumw2();
  meeUpsilon3S->Sumw2();
  ptyeePion->Sumw2();
  ptyeeEta->Sumw2();
  ptyeeEtaprime->Sumw2();
  ptyeeRho->Sumw2();
  ptyeeOmega->Sumw2();
  ptyeePhi->Sumw2();
  ptyeeJPsi->Sumw2();
  ptyeePsiprime->Sumw2();
  ptJPsi->Sumw2();
  ptPhi->Sumw2();
  mptee->Sumw2();
  mptee_Pion->Sumw2();
  mptee_Eta->Sumw2();
  mptee_Etaprime->Sumw2();
  mptee_Rho->Sumw2();
  mptee_Omega->Sumw2();
  mptee_Phi->Sumw2();
  mptee_JPsi->Sumw2();
  mptee_Psiprime->Sumw2();
  mptee_Upsilon1S->Sumw2();
  mptee_Upsilon2S->Sumw2();
  mptee_Upsilon3S->Sumw2();
  mptee_eid->Sumw2();
  mptee_ert->Sumw2();
  mptee_EMCghost->Sumw2();
  mptee_accP->Sumw2();
  mptee_accS->Sumw2();
  mptee_accS_fid1->Sumw2();
  mptee_accS_fid2->Sumw2();
  mptee_accS_fid3->Sumw2();
  mptee_accS_fid4->Sumw2();
  mptee_accS_eid_fid1->Sumw2();
  mptee_accS_eid_fid2->Sumw2();
  mptee_accS_eid_fid3->Sumw2();
  mptee_accS_eid_fid4->Sumw2();
  mphiVee->Sumw2();
  mptphiVee->Sumw2();
  mptphiVee_rec->Sumw2();
  mgg->Sumw2();
  mggPion->Sumw2();
  mggEta->Sumw2();
  mggEtaprime->Sumw2();
  mggRho->Sumw2();
  mggOmega->Sumw2();
  mggPhi->Sumw2();

  return;
}
