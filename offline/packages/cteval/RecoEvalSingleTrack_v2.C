#include "RecoEvalSingleTrack_v2.h"

ClassImp(RecoEvalSingleTrack_v2)

void RecoEvalSingleTrack_v2::Init()
{
  RECOID      = -999;
  QUALITY     = -999;
  MOMENTUM    = -999;
  THETA0      = -999; 
  PHI0        = -999;
  PHI         = -999; 
  ALPHA       = -999;
  ZED         = -999; 
  BETA        = -999;
  AVERAGETIME = -999;
  XHITS       = -999; 
  UVHITS      = -999;
  MULMAIN     = -999; 
  MULXMAIN    = -999; 
  MULUVMAIN   = -999;
  MAIN        = -999; 
  XMAIN       = -999; 
  UVMAIN      = -999;
  AMBIGUITY   = -999;
  PURITY      = -999; 
  XPURITY     = -999; 
  UVPURITY    = -999;

  PC1CLUSID     = -999; 
  PC2CLUSID     = -999; 
  PC3CLUSID     = -999;
  PC1CLUSIDTRUE = -999; 
  PC2CLUSIDTRUE = -999; 
  PC3CLUSIDTRUE = -999;
  PC1CLUSIDG    = -999; 
  PC2CLUSIDG    = -999; 
  PC3CLUSIDG    = -999;
  PC1POINTXG    = -999; 
  PC1POINTYG    = -999; 
  PC1POINTZG    = -999; 
  PC2POINTXG    = -999; 
  PC2POINTYG    = -999; 
  PC2POINTZG    = -999; 
  PC3POINTXG    = -999; 
  PC3POINTYG    = -999; 
  PC3POINTZG    = -999; 

  TOFID      = -999;
  TOFIDTRUE  = -999;
  TOFIDG     = -999;
  TOFPOINTXG = -999; 
  TOFPOINTYG = -999; 
  TOFPOINTZG = -999; 
  TOFG       = -999; 
  TOFELOSSG  = -999;

  EMCCLUSID     = -999; 
  EMCCLUSIDTRUE = -999; 
  EMCCLUSIDG    = -999;
  EMCANCTRK0    = -999; 
  EMCANCTRK1    = -999; 
  EMCANCTRK2    = -999;
  EMCANCTWRHIT0 = -999; 
  EMCANCTWRHIT1 = -999; 
  EMCANCTWRHIT2 = -999;
  EMCANCPID0    = -999; 
  EMCANCPID1    = -999; 
  EMCANCPID2    = -999;
  EMCANCEDEP0   = -999; 
  EMCANCEDEP1   = -999; 
  EMCANCEDEP2   = -999;
  EMCANCPTOT0   = -999; 
  EMCANCPTOT1   = -999; 
  EMCANCPTOT2   = -999;
  EMCPOINTXG    = -999; 
  EMCPOINTYG    = -999; 
  EMCPOINTZG    = -999; 
  EMCEFRACG     = -999; 
  EMCECOREG     = -999; 
  EMCMEASEG     = -999; 
  EMCTOFG       = -999;

  CRKACC   = -999;
  CRKNPMT0 = -999; 
  CRKNPMT1 = -999; 
  CRKNPMT3 = -999;
  CRKNPE0  = -999; 
  CRKNPE1  = -999; 
  CRKNPE3  = -999;
  CRKCHI2  = -999; 
  CRKDISP  = -999; 
  CRKPATH  = -999;
}

RecoEvalSingleTrack_v2::RecoEvalSingleTrack_v2()
{
  Init();
}

RecoEvalSingleTrack_v2::RecoEvalSingleTrack_v2(RecoEvalSingleTrack *track)
{
  Init();
  if(!track) return;

  RECOID      = track->get_recoid();
  QUALITY     = track->get_quality();
  MOMENTUM    = track->get_momentum();
  THETA0      = track->get_theta0(); 
  PHI0        = track->get_phi0();
  PHI         = track->get_phi(); 
  ALPHA       = track->get_alpha();
  ZED         = track->get_zed(); 
  BETA        = track->get_beta();
  AVERAGETIME = track->get_averagetime();
  XHITS       = track->get_xhits(); 
  UVHITS      = track->get_uvhits();
  MULMAIN     = track->get_mulmain(); 
  MULXMAIN    = track->get_mulxmain(); 
  MULUVMAIN   = track->get_muluvmain();
  MAIN        = track->get_main(); 
  XMAIN       = track->get_xmain(); 
  UVMAIN      = track->get_uvmain();
  AMBIGUITY   = track->get_ambiguity();
  PURITY      = track->get_purity(); 
  XPURITY     = track->get_xpurity(); 
  UVPURITY    = track->get_uvpurity();

  PC1CLUSID     = track->get_pc1clusid(); 
  PC2CLUSID     = track->get_pc2clusid(); 
  PC3CLUSID     = track->get_pc3clusid();
  PC1CLUSIDTRUE = track->get_pc1clusidtrue(); 
  PC2CLUSIDTRUE = track->get_pc2clusidtrue(); 
  PC3CLUSIDTRUE = track->get_pc3clusidtrue();
  PC1CLUSIDG    = track->get_pc1clusidg(); 
  PC2CLUSIDG    = track->get_pc2clusidg(); 
  PC3CLUSIDG    = track->get_pc3clusidg();
  PC1POINTXG    = track->get_pc1pointxg();
  PC1POINTYG    = track->get_pc1pointyg();
  PC1POINTZG    = track->get_pc1pointzg();
  PC2POINTXG    = track->get_pc2pointxg();
  PC2POINTYG    = track->get_pc2pointyg();
  PC2POINTZG    = track->get_pc2pointzg();
  PC3POINTXG    = track->get_pc3pointxg();
  PC3POINTYG    = track->get_pc3pointyg();
  PC3POINTZG    = track->get_pc3pointzg();

  TOFID      = track->get_tofid();
  TOFIDTRUE  = track->get_tofidtrue();
  TOFIDG     = track->get_tofidg();
  TOFPOINTXG = track->get_tofpointxg();
  TOFPOINTYG = track->get_tofpointyg();
  TOFPOINTZG = track->get_tofpointzg();
  TOFG       = track->get_tofg(); 
  TOFELOSSG  = track->get_tofelossg();

  EMCCLUSID     = track->get_emcclusid(); 
  EMCCLUSIDTRUE = track->get_emcclusidtrue(); 
  EMCCLUSIDG    = track->get_emcclusidg();
  EMCANCTRK0    = track->get_emcanctrk0(); 
  EMCANCTRK1    = track->get_emcanctrk1(); 
  EMCANCTRK2    = track->get_emcanctrk2();
  EMCANCTWRHIT0 = track->get_emcanctwrhit0(); 
  EMCANCTWRHIT1 = track->get_emcanctwrhit1(); 
  EMCANCTWRHIT2 = track->get_emcanctwrhit2();
  EMCANCPID0    = track->get_emcancpid0(); 
  EMCANCPID1    = track->get_emcancpid1(); 
  EMCANCPID2    = track->get_emcancpid2();
  EMCANCEDEP0   = track->get_emcancedep0(); 
  EMCANCEDEP1   = track->get_emcancedep1(); 
  EMCANCEDEP2   = track->get_emcancedep2();
  EMCANCPTOT0   = track->get_emcancptot0(); 
  EMCANCPTOT1   = track->get_emcancptot1(); 
  EMCANCPTOT2   = track->get_emcancptot2();
  EMCPOINTXG    = track->get_emcpointxg();
  EMCPOINTYG    = track->get_emcpointyg();
  EMCPOINTZG    = track->get_emcpointzg();
  EMCEFRACG     = track->get_emcefracg(); 
  EMCECOREG     = track->get_emcecoreg(); 
  EMCMEASEG     = track->get_emcmeaseg(); 
  EMCTOFG       = track->get_emctofg();

  CRKACC   = track->get_crkacc();
  CRKNPMT0 = track->get_crknpmt0(); 
  CRKNPMT1 = track->get_crknpmt1(); 
  CRKNPMT3 = track->get_crknpmt3();
  CRKNPE0  = track->get_crknpe0(); 
  CRKNPE1  = track->get_crknpe1(); 
  CRKNPE3  = track->get_crknpe3();
  CRKCHI2  = track->get_crkchi2(); 
  CRKDISP  = track->get_crkdisp(); 
  CRKPATH  = track->get_crkpath();
}






