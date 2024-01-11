#ifndef RECOEVALSINGLETRACK_v2_H
#define RECOEVALSINGLETRACK_v2_H

#include "PHObject.h"
#include "RecoEvalSingleTrack.h"

class RecoEvalSingleTrack_v2 : public RecoEvalSingleTrack
{
public:
  RecoEvalSingleTrack_v2();
  RecoEvalSingleTrack_v2(RecoEvalSingleTrack *track);  
  virtual ~RecoEvalSingleTrack_v2(){};

  void set_recoid(short val) {RECOID = val;}
  void set_quality(short val) {QUALITY = val;}
  void set_momentum(float val) {MOMENTUM = val;}
  void set_theta0(float val) {THETA0 = val;} 
  void set_phi0(float val) {PHI0 = val;}
  void set_phi(float val) {PHI = val;} 
  void set_alpha(float val) {ALPHA = val;}
  void set_zed(float val) {ZED = val;} 
  void set_beta(float val) {BETA = val;}
  void set_averagetime(float val) {AVERAGETIME = val;}
  void set_xhits(short val) {XHITS = val;} 
  void set_uvhits(short val) {UVHITS = val;}
  void set_mulmain(short val) {MULMAIN = val;} 
  void set_mulxmain(short val) {MULXMAIN = val;} 
  void set_muluvmain(short val) {MULUVMAIN = val;}
  void set_main(short val) {MAIN = val;} 
  void set_xmain(short val) {XMAIN = val;} 
  void set_uvmain(short val) {UVMAIN = val;}
  void set_ambiguity(short val) {AMBIGUITY = val;}
  void set_purity(float val) {PURITY = val;} 
  void set_xpurity(float val) {XPURITY = val;} 
  void set_uvpurity(float val) {UVPURITY = val;}

  void set_pc1clusid(short val) {PC1CLUSID = val;} 
  void set_pc2clusid(short val) {PC2CLUSID = val;} 
  void set_pc3clusid(short val) {PC3CLUSID = val;}
  void set_pc1clusidtrue(short val) {PC1CLUSIDTRUE = val;} 
  void set_pc2clusidtrue(short val) {PC2CLUSIDTRUE = val;} 
  void set_pc3clusidtrue(short val) {PC3CLUSIDTRUE = val;}
  void set_pc1clusidg(short val) {PC1CLUSIDG = val;} 
  void set_pc2clusidg(short val) {PC2CLUSIDG = val;} 
  void set_pc3clusidg(short val) {PC3CLUSIDG = val;}
  void set_pc1pointxg(float val) {PC1POINTXG = val;} 
  void set_pc1pointyg(float val) {PC1POINTYG = val;} 
  void set_pc1pointzg(float val) {PC1POINTZG = val;} 
  void set_pc2pointxg(float val) {PC2POINTXG = val;} 
  void set_pc2pointyg(float val) {PC2POINTYG = val;} 
  void set_pc2pointzg(float val) {PC2POINTZG = val;} 
  void set_pc3pointxg(float val) {PC3POINTXG = val;} 
  void set_pc3pointyg(float val) {PC3POINTYG = val;} 
  void set_pc3pointzg(float val) {PC3POINTZG = val;} 

  void set_tofid(short val) {TOFID = val;}
  void set_tofidtrue(short val) {TOFIDTRUE = val;}
  void set_tofidg(short val) {TOFIDG = val;}
  void set_tofpointxg(float val) {TOFPOINTXG = val;}
  void set_tofpointyg(float val) {TOFPOINTYG = val;}
  void set_tofpointzg(float val) {TOFPOINTZG = val;}
  void set_tofg(float val) {TOFG = val;} 
  void set_tofelossg(float val) {TOFELOSSG = val;}

  void set_emcclusid(short val) {EMCCLUSID = val;} 
  void set_emcclusidtrue(short val) {EMCCLUSIDTRUE = val;} 
  void set_emcclusidg(short val) {EMCCLUSIDG = val;}
  void set_emcanctrk0(short val) {EMCANCTRK0 = val;} 
  void set_emcanctrk1(short val) {EMCANCTRK1 = val;} 
  void set_emcanctrk2(short val) {EMCANCTRK2 = val;}
  void set_emcanctwrhit0(short val) {EMCANCTWRHIT0 = val;} 
  void set_emcanctwrhit1(short val) {EMCANCTWRHIT1 = val;} 
  void set_emcanctwrhit2(short val) {EMCANCTWRHIT2 = val;}
  void set_emcancpid0(short val) {EMCANCPID0 = val;} 
  void set_emcancpid1(short val) {EMCANCPID1 = val;} 
  void set_emcancpid2(short val) {EMCANCPID2 = val;}
  void set_emcancedep0(float val) {EMCANCEDEP0 = val;} 
  void set_emcancedep1(float val) {EMCANCEDEP1 = val;} 
  void set_emcancedep2(float val) {EMCANCEDEP2 = val;}
  void set_emcancptot0(float val) {EMCANCPTOT0 = val;} 
  void set_emcancptot1(float val) {EMCANCPTOT1 = val;} 
  void set_emcancptot2(float val) {EMCANCPTOT2 = val;}
  void set_emcpointxg(float val) {EMCPOINTXG = val;}
  void set_emcpointyg(float val) {EMCPOINTYG = val;}
  void set_emcpointzg(float val) {EMCPOINTZG = val;}
  void set_emcefracg(float val) {EMCEFRACG = val;} 
  void set_emcecoreg(float val) {EMCECOREG = val;} 
  void set_emcmeaseg(float val) {EMCMEASEG = val;} 
  void set_emctofg(float val) {EMCTOFG = val;}

  void set_crkacc(short val) {CRKACC = val;}
  void set_crknpmt0(short val) {CRKNPMT0 = val;} 
  void set_crknpmt1(short val) {CRKNPMT1 = val;} 
  void set_crknpmt3(short val) {CRKNPMT3 = val;}
  void set_crknpe0(float val) {CRKNPE0 = val;} 
  void set_crknpe1(float val) {CRKNPE1 = val;} 
  void set_crknpe3(float val) {CRKNPE3 = val;}
  void set_crkchi2(float val) {CRKCHI2 = val;} 
  void set_crkdisp(float val) {CRKDISP = val;} 
  void set_crkpath(float val) {CRKPATH = val;}

  short get_recoid() {return RECOID;}
  short get_quality() {return QUALITY;}
  float get_momentum() {return MOMENTUM;}
  float get_theta0() {return THETA0;} 
  float get_phi0() {return PHI0;}
  float get_phi() {return PHI;} 
  float get_alpha() {return ALPHA;}
  float get_zed() {return ZED;} 
  float get_beta() {return BETA;}
  float get_averagetime() {return AVERAGETIME;}
  short get_xhits() {return XHITS;} 
  short get_uvhits() {return UVHITS;}
  short get_mulmain() {return MULMAIN;} 
  short get_mulxmain() {return MULXMAIN;} 
  short get_muluvmain() {return MULUVMAIN;}
  short get_main() {return MAIN;} 
  short get_xmain() {return XMAIN;} 
  short get_uvmain() {return UVMAIN;}
  short get_ambiguity() {return AMBIGUITY;}
  float get_purity() {return PURITY;} 
  float get_xpurity() {return XPURITY;} 
  float get_uvpurity() {return UVPURITY;}

  short get_pc1clusid() {return PC1CLUSID;} 
  short get_pc2clusid() {return PC2CLUSID;} 
  short get_pc3clusid() {return PC3CLUSID;}
  short get_pc1clusidtrue() {return PC1CLUSIDTRUE;} 
  short get_pc2clusidtrue() {return PC2CLUSIDTRUE;} 
  short get_pc3clusidtrue() {return PC3CLUSIDTRUE;}
  short get_pc1clusidg() {return PC1CLUSIDG;} 
  short get_pc2clusidg() {return PC2CLUSIDG;} 
  short get_pc3clusidg() {return PC3CLUSIDG;}
  float get_pc1pointxg() {return PC1POINTXG;} 
  float get_pc1pointyg() {return PC1POINTYG;} 
  float get_pc1pointzg() {return PC1POINTZG;} 
  float get_pc2pointxg() {return PC2POINTXG;} 
  float get_pc2pointyg() {return PC2POINTYG;} 
  float get_pc2pointzg() {return PC2POINTZG;} 
  float get_pc3pointxg() {return PC3POINTXG;} 
  float get_pc3pointyg() {return PC3POINTYG;} 
  float get_pc3pointzg() {return PC3POINTZG;} 

  short get_tofid() {return TOFID;}
  short get_tofidtrue() {return TOFIDTRUE;}
  short get_tofidg() {return TOFIDG;}
  float get_tofpointxg() {return TOFPOINTXG;}
  float get_tofpointyg() {return TOFPOINTYG;}
  float get_tofpointzg() {return TOFPOINTZG;}
  float get_tofg() {return TOFG;} 
  float get_tofelossg() {return TOFELOSSG;}

  short get_emcclusid() {return EMCCLUSID;} 
  short get_emcclusidtrue() {return EMCCLUSIDTRUE;} 
  short get_emcclusidg() {return EMCCLUSIDG;}
  short get_emcanctrk0() {return EMCANCTRK0;} 
  short get_emcanctrk1() {return EMCANCTRK1;} 
  short get_emcanctrk2() {return EMCANCTRK2;}
  short get_emcanctwrhit0() {return EMCANCTWRHIT0;} 
  short get_emcanctwrhit1() {return EMCANCTWRHIT1;} 
  short get_emcanctwrhit2() {return EMCANCTWRHIT2;}
  short get_emcancpid0() {return EMCANCPID0;} 
  short get_emcancpid1() {return EMCANCPID1;} 
  short get_emcancpid2() {return EMCANCPID2;}
  float get_emcancedep0() {return EMCANCEDEP0;} 
  float get_emcancedep1() {return EMCANCEDEP1;} 
  float get_emcancedep2() {return EMCANCEDEP2;}
  float get_emcancptot0() {return EMCANCPTOT0;} 
  float get_emcancptot1() {return EMCANCPTOT1;} 
  float get_emcancptot2() {return EMCANCPTOT2;}
  float get_emcpointxg() {return EMCPOINTXG;}
  float get_emcpointyg() {return EMCPOINTYG;}
  float get_emcpointzg() {return EMCPOINTZG;}
  float get_emcefracg() {return EMCEFRACG;} 
  float get_emcecoreg() {return EMCECOREG;} 
  float get_emcmeaseg() {return EMCMEASEG;} 
  float get_emctofg() {return EMCTOFG;}

  short get_crkacc() {return CRKACC;}
  short get_crknpmt0() {return CRKNPMT0;} 
  short get_crknpmt1() {return CRKNPMT1;} 
  short get_crknpmt3() {return CRKNPMT3;}
  float get_crknpe0() {return CRKNPE0;} 
  float get_crknpe1() {return CRKNPE1;} 
  float get_crknpe3() {return CRKNPE3;}
  float get_crkchi2() {return CRKCHI2;} 
  float get_crkdisp() {return CRKDISP;} 
  float get_crkpath() {return CRKPATH;}

protected:

  void Init();

  short RECOID;
  short QUALITY;
  float MOMENTUM;
  float THETA0, PHI0;
  float PHI, ALPHA;
  float ZED, BETA;
  float AVERAGETIME;
  short XHITS, UVHITS;
  short MULMAIN, MULXMAIN, MULUVMAIN;
  short MAIN, XMAIN, UVMAIN;
  short AMBIGUITY;
  float PURITY, XPURITY, UVPURITY;

  short PC1CLUSID, PC2CLUSID, PC3CLUSID;
  short PC1CLUSIDTRUE, PC2CLUSIDTRUE, PC3CLUSIDTRUE;
  short PC1CLUSIDG, PC2CLUSIDG, PC3CLUSIDG;
  float PC1POINTXG, PC2POINTXG, PC3POINTXG;
  float PC1POINTYG, PC2POINTYG, PC3POINTYG;
  float PC1POINTZG, PC2POINTZG, PC3POINTZG;

  short TOFID;
  short TOFIDTRUE;
  short TOFIDG;
  float TOFPOINTXG, TOFPOINTYG, TOFPOINTZG;
  float TOFG, TOFELOSSG;

  short EMCCLUSID, EMCCLUSIDTRUE, EMCCLUSIDG;
  short EMCANCTRK0, EMCANCTRK1, EMCANCTRK2;
  short EMCANCTWRHIT0, EMCANCTWRHIT1, EMCANCTWRHIT2;
  short EMCANCPID0, EMCANCPID1, EMCANCPID2;
  float EMCANCEDEP0, EMCANCEDEP1, EMCANCEDEP2;
  float EMCANCPTOT0, EMCANCPTOT1, EMCANCPTOT2;
  float EMCPOINTXG, EMCPOINTYG, EMCPOINTZG;
  float EMCEFRACG, EMCECOREG, EMCMEASEG, EMCTOFG;

  short CRKACC;
  short CRKNPMT0, CRKNPMT1, CRKNPMT3;
  float CRKNPE0, CRKNPE1, CRKNPE3;
  float CRKCHI2, CRKDISP, CRKPATH;

  ClassDef(RecoEvalSingleTrack_v2,1)
};

#endif /*__RECOEVALSINGLETRACK_v2_H*/
