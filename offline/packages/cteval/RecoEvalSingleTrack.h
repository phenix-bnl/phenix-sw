#ifndef RECOEVALSINGLETRACK_H
#define RECOEVALSINGLETRACK_H

#include "phool.h"
#include "PHObject.h"

class RecoEvalSingleTrack : public PHObject {
public:
  RecoEvalSingleTrack() {};
  RecoEvalSingleTrack(RecoEvalSingleTrack *track) {};
  virtual ~RecoEvalSingleTrack() {};

  virtual void set_recoid(short val) = 0;
  virtual void set_quality(short val) = 0;
  virtual void set_momentum(float val) = 0;
  virtual void set_theta0(float val) = 0;
  virtual void set_phi0(float val) = 0;
  virtual void set_phi(float val) = 0;
  virtual void set_alpha(float val) = 0;
  virtual void set_zed(float val) = 0;
  virtual void set_beta(float val) = 0;
  virtual void set_averagetime(float val) = 0;
  virtual void set_xhits(short val) = 0;
  virtual void set_uvhits(short val) = 0;
  virtual void set_mulmain(short val) = 0;
  virtual void set_mulxmain(short val) = 0;
  virtual void set_muluvmain(short val) = 0;
  virtual void set_main(short val) = 0;
  virtual void set_xmain(short val) = 0;
  virtual void set_uvmain(short val) = 0;
  virtual void set_ambiguity(short val) = 0;
  virtual void set_purity(float val) = 0;
  virtual void set_xpurity(float val) = 0;
  virtual void set_uvpurity(float val) = 0;

  virtual void set_pc1clusid(short val) = 0;
  virtual void set_pc2clusid(short val) = 0;
  virtual void set_pc3clusid(short val) = 0;
  virtual void set_pc1clusidtrue(short val) = 0;
  virtual void set_pc2clusidtrue(short val) = 0;
  virtual void set_pc3clusidtrue(short val) = 0;
  virtual void set_pc1clusidg(short val) = 0;
  virtual void set_pc2clusidg(short val) = 0;
  virtual void set_pc3clusidg(short val) = 0;
  virtual void set_pc1pointxg(float val) = 0;
  virtual void set_pc1pointyg(float val) = 0;
  virtual void set_pc1pointzg(float val) = 0;
  virtual void set_pc2pointxg(float val) = 0;
  virtual void set_pc2pointyg(float val) = 0;
  virtual void set_pc2pointzg(float val) = 0;
  virtual void set_pc3pointxg(float val) = 0;
  virtual void set_pc3pointyg(float val) = 0;
  virtual void set_pc3pointzg(float val) = 0;

  virtual void set_tofid(short val) = 0;
  virtual void set_tofidtrue(short val) = 0;
  virtual void set_tofidg(short val) = 0;
  virtual void set_tofpointxg(float val) = 0;
  virtual void set_tofpointyg(float val) = 0;
  virtual void set_tofpointzg(float val) = 0;
  virtual void set_tofg(float val) = 0;
  virtual void set_tofelossg(float val) = 0;

  virtual void set_emcclusid(short val) = 0;
  virtual void set_emcclusidtrue(short val) = 0;
  virtual void set_emcclusidg(short val) = 0;
  virtual void set_emcanctrk0(short val) = 0;
  virtual void set_emcanctrk1(short val) = 0;
  virtual void set_emcanctrk2(short val) = 0;
  virtual void set_emcanctwrhit0(short val) = 0;
  virtual void set_emcanctwrhit1(short val) = 0;
  virtual void set_emcanctwrhit2(short val) = 0;
  virtual void set_emcancpid0(short val) = 0;
  virtual void set_emcancpid1(short val) = 0;
  virtual void set_emcancpid2(short val) = 0;
  virtual void set_emcancedep0(float val) = 0;
  virtual void set_emcancedep1(float val) = 0;
  virtual void set_emcancedep2(float val) = 0;
  virtual void set_emcancptot0(float val) = 0;
  virtual void set_emcancptot1(float val) = 0;
  virtual void set_emcancptot2(float val) = 0;
  virtual void set_emcpointxg(float val) = 0;
  virtual void set_emcpointyg(float val) = 0;
  virtual void set_emcpointzg(float val) = 0;
  virtual void set_emcefracg(float val) = 0;
  virtual void set_emcecoreg(float val) = 0;
  virtual void set_emcmeaseg(float val) = 0;
  virtual void set_emctofg(float val) = 0;

  virtual void set_crkacc(short val) = 0;
  virtual void set_crknpmt0(short val) = 0;
  virtual void set_crknpmt1(short val) = 0;
  virtual void set_crknpmt3(short val) = 0;
  virtual void set_crknpe0(float val) = 0;
  virtual void set_crknpe1(float val) = 0;
  virtual void set_crknpe3(float val) = 0;
  virtual void set_crkchi2(float val) = 0;
  virtual void set_crkdisp(float val) = 0;
  virtual void set_crkpath(float val) = 0;

  virtual short get_recoid() = 0;
  virtual short get_quality() = 0;
  virtual float get_momentum() = 0;
  virtual float get_theta0() = 0;
  virtual float get_phi0() = 0;
  virtual float get_phi() = 0;
  virtual float get_alpha() = 0;
  virtual float get_zed() = 0;
  virtual float get_beta() = 0;
  virtual float get_averagetime() = 0;
  virtual short get_xhits() = 0;
  virtual short get_uvhits() = 0;
  virtual short get_mulmain() = 0;
  virtual short get_mulxmain() = 0;
  virtual short get_muluvmain() = 0;
  virtual short get_main() = 0;
  virtual short get_xmain() = 0;
  virtual short get_uvmain() = 0;
  virtual short get_ambiguity() = 0;
  virtual float get_purity() = 0;
  virtual float get_xpurity() = 0;
  virtual float get_uvpurity() = 0;

  virtual short get_pc1clusid() = 0;
  virtual short get_pc2clusid() = 0;
  virtual short get_pc3clusid() = 0;
  virtual short get_pc1clusidtrue() = 0;
  virtual short get_pc2clusidtrue() = 0;
  virtual short get_pc3clusidtrue() = 0;
  virtual short get_pc1clusidg() = 0;
  virtual short get_pc2clusidg() = 0;
  virtual short get_pc3clusidg() = 0;
  virtual float get_pc1pointxg() = 0;
  virtual float get_pc1pointyg() = 0;
  virtual float get_pc1pointzg() = 0;
  virtual float get_pc2pointxg() = 0;
  virtual float get_pc2pointyg() = 0;
  virtual float get_pc2pointzg() = 0;
  virtual float get_pc3pointxg() = 0;
  virtual float get_pc3pointyg() = 0;
  virtual float get_pc3pointzg() = 0;

  virtual short get_tofid() = 0;
  virtual short get_tofidtrue() = 0;
  virtual short get_tofidg() = 0;
  virtual float get_tofpointxg() = 0;
  virtual float get_tofpointyg() = 0;
  virtual float get_tofpointzg() = 0;
  virtual float get_tofg() = 0;
  virtual float get_tofelossg() = 0;

  virtual short get_emcclusid() = 0;
  virtual short get_emcclusidtrue() = 0;
  virtual short get_emcclusidg() = 0;
  virtual short get_emcanctrk0() = 0;
  virtual short get_emcanctrk1() = 0;
  virtual short get_emcanctrk2() = 0;
  virtual short get_emcanctwrhit0() = 0;
  virtual short get_emcanctwrhit1() = 0;
  virtual short get_emcanctwrhit2() = 0;
  virtual short get_emcancpid0() = 0;
  virtual short get_emcancpid1() = 0;
  virtual short get_emcancpid2() = 0;
  virtual float get_emcancedep0() = 0;
  virtual float get_emcancedep1() = 0;
  virtual float get_emcancedep2() = 0;
  virtual float get_emcancptot0() = 0;
  virtual float get_emcancptot1() = 0;
  virtual float get_emcancptot2() = 0;
  virtual float get_emcpointxg() = 0;
  virtual float get_emcpointyg() = 0;
  virtual float get_emcpointzg() = 0;
  virtual float get_emcefracg() = 0;
  virtual float get_emcecoreg() = 0;
  virtual float get_emcmeaseg() = 0;
  virtual float get_emctofg() = 0;

  virtual short get_crkacc() = 0;
  virtual short get_crknpmt0() = 0;
  virtual short get_crknpmt1() = 0;
  virtual short get_crknpmt3() = 0;
  virtual float get_crknpe0() = 0;
  virtual float get_crknpe1() = 0;
  virtual float get_crknpe3() = 0;
  virtual float get_crkchi2() = 0;
  virtual float get_crkdisp() = 0;
  virtual float get_crkpath() = 0;

protected:
  virtual void Init() {};
  ClassDef(RecoEvalSingleTrack, 1)
};

#endif /*__RECOEVALSINGLETRACK_H*/
