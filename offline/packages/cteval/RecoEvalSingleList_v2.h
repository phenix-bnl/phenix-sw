#ifndef RECOEVALSINGLELIST_v2_H
#define RECOEVALSINGLELIST_v2_H

#include "phool.h"
#include "PHObject.h"
#include "TClonesArray.h"
#include "RecoEvalSingleList.h"

class RecoEvalSingleTrack;

class RecoEvalSingleList_v2 : public RecoEvalSingleList
{
public:
  RecoEvalSingleList_v2();
  virtual ~RecoEvalSingleList_v2();

  void Reset();
  int set_TClonesArraySize(unsigned int fullsize);

  int AddRecoEvalSingleTrack(RecoEvalSingleTrack *newtrack, const int i);  

  unsigned int get_RecoEvalSingleTrackN() const 
    {return RecoEvalSingleTrackN;}
  void set_RecoEvalSingleTrackN(const unsigned int ntrk) 
    {RecoEvalSingleTrackN = ntrk; return;}

  void set_recoid(const unsigned int i, const short val);
  void set_quality(const unsigned int i, const short val);
  void set_momentum(const unsigned int i, const float val);
  void set_theta0(const unsigned int i, const float val);
  void set_phi0(const unsigned int i, const float val);
  void set_phi(const unsigned int i, const float val);
  void set_alpha(const unsigned int i, const float val);
  void set_zed(const unsigned int i, const float val);
  void set_beta(const unsigned int i, const float val);
  void set_averagetime(const unsigned int i, const float val);
  void set_xhits(const unsigned int i, const short val);
  void set_uvhits(const unsigned int i, const short val);
  void set_mulmain(const unsigned int i, const short val);
  void set_mulxmain(const unsigned int i, const short val);
  void set_muluvmain(const unsigned int i, const short val);
  void set_main(const unsigned int i, const short val);
  void set_xmain(const unsigned int i, const short val);
  void set_uvmain(const unsigned int i, const short val);
  void set_ambiguity(const unsigned int i, const short val);
  void set_purity(const unsigned int i, const float val);
  void set_xpurity(const unsigned int i, const float val);
  void set_uvpurity(const unsigned int i, const float val);

  void set_pc1clusid(const unsigned int i, const short val);
  void set_pc2clusid(const unsigned int i, const short val);
  void set_pc3clusid(const unsigned int i, const short val);
  void set_pc1clusidtrue(const unsigned int i, const short val);
  void set_pc2clusidtrue(const unsigned int i, const short val);
  void set_pc3clusidtrue(const unsigned int i, const short val);
  void set_pc1clusidg(const unsigned int i, const short val);
  void set_pc2clusidg(const unsigned int i, const short val);
  void set_pc3clusidg(const unsigned int i, const short val);
  void set_pc1pointxg(const unsigned int i, const float val);
  void set_pc1pointyg(const unsigned int i, const float val);
  void set_pc1pointzg(const unsigned int i, const float val);
  void set_pc2pointxg(const unsigned int i, const float val);
  void set_pc2pointyg(const unsigned int i, const float val);
  void set_pc2pointzg(const unsigned int i, const float val);
  void set_pc3pointxg(const unsigned int i, const float val);
  void set_pc3pointyg(const unsigned int i, const float val);
  void set_pc3pointzg(const unsigned int i, const float val);

  void set_tofid(const unsigned int i, const short val);
  void set_tofidtrue(const unsigned int i, const short val);
  void set_tofidg(const unsigned int i, const short val);
  void set_tofpointxg(const unsigned int i, const float val);
  void set_tofpointyg(const unsigned int i, const float val);
  void set_tofpointzg(const unsigned int i, const float val);
  void set_tofg(const unsigned int i, const float val);
  void set_tofelossg(const unsigned int i, const float val);

  void set_emcclusid(const unsigned int i, const short val);
  void set_emcclusidtrue(const unsigned int i, const short val);
  void set_emcclusidg(const unsigned int i, const short val);
  void set_emcanctrk0(const unsigned int i, const short val);
  void set_emcanctrk1(const unsigned int i, const short val);
  void set_emcanctrk2(const unsigned int i, const short val);
  void set_emcanctwrhit0(const unsigned int i, const short val);
  void set_emcanctwrhit1(const unsigned int i, const short val);
  void set_emcanctwrhit2(const unsigned int i, const short val);
  void set_emcancpid0(const unsigned int i, const short val);
  void set_emcancpid1(const unsigned int i, const short val);
  void set_emcancpid2(const unsigned int i, const short val);
  void set_emcancedep0(const unsigned int i, const float val);
  void set_emcancedep1(const unsigned int i, const float val);
  void set_emcancedep2(const unsigned int i, const float val);
  void set_emcancptot0(const unsigned int i, const float val);
  void set_emcancptot1(const unsigned int i, const float val);
  void set_emcancptot2(const unsigned int i, const float val);
  void set_emcpointxg(const unsigned int i, const float val);
  void set_emcpointyg(const unsigned int i, const float val);
  void set_emcpointzg(const unsigned int i, const float val);
  void set_emcefracg(const unsigned int i, const float val);
  void set_emcecoreg(const unsigned int i, const float val);
  void set_emcmeaseg(const unsigned int i, const float val);
  void set_emctofg(const unsigned int i, const float val);

  void set_crkacc(const unsigned int i, const short val);
  void set_crknpmt0(const unsigned int i, const short val);
  void set_crknpmt1(const unsigned int i, const short val);
  void set_crknpmt3(const unsigned int i, const short val);
  void set_crknpe0(const unsigned int i, const float val);
  void set_crknpe1(const unsigned int i, const float val);
  void set_crknpe3(const unsigned int i, const float val);
  void set_crkchi2(const unsigned int i, const float val);
  void set_crkdisp(const unsigned int i, const float val);
  void set_crkpath(const unsigned int i, const float val);

  short get_recoid(const unsigned int i) const;
  short get_quality(const unsigned int i) const;
  float get_momentum(const unsigned int i) const;
  float get_theta0(const unsigned int i) const;
  float get_phi0(const unsigned int i) const;
  float get_phi(const unsigned int i) const;
  float get_alpha(const unsigned int i) const;
  float get_zed(const unsigned int i) const;
  float get_beta(const unsigned int i) const;
  float get_averagetime(const unsigned int i) const;
  short get_xhits(const unsigned int i) const;
  short get_uvhits(const unsigned int i) const;
  short get_mulmain(const unsigned int i) const;
  short get_mulxmain(const unsigned int i) const;
  short get_muluvmain(const unsigned int i) const;
  short get_main(const unsigned int i) const;
  short get_xmain(const unsigned int i) const;
  short get_uvmain(const unsigned int i) const;
  short get_ambiguity(const unsigned int i) const;
  float get_purity(const unsigned int i) const;
  float get_xpurity(const unsigned int i) const;
  float get_uvpurity(const unsigned int i) const;

  short get_pc1clusid(const unsigned int i) const;
  short get_pc2clusid(const unsigned int i) const;
  short get_pc3clusid(const unsigned int i) const;
  short get_pc1clusidtrue(const unsigned int i) const;
  short get_pc2clusidtrue(const unsigned int i) const;
  short get_pc3clusidtrue(const unsigned int i) const;
  short get_pc1clusidg(const unsigned int i) const;
  short get_pc2clusidg(const unsigned int i) const;
  short get_pc3clusidg(const unsigned int i) const;
  float get_pc1pointxg(const unsigned int i) const;
  float get_pc1pointyg(const unsigned int i) const;
  float get_pc1pointzg(const unsigned int i) const;
  float get_pc2pointxg(const unsigned int i) const;
  float get_pc2pointyg(const unsigned int i) const;
  float get_pc2pointzg(const unsigned int i) const;
  float get_pc3pointxg(const unsigned int i) const;
  float get_pc3pointyg(const unsigned int i) const;
  float get_pc3pointzg(const unsigned int i) const;

  short get_tofid(const unsigned int i) const;
  short get_tofidtrue(const unsigned int i) const;
  short get_tofidg(const unsigned int i) const;
  float get_tofpointxg(const unsigned int i) const;
  float get_tofpointyg(const unsigned int i) const;
  float get_tofpointzg(const unsigned int i) const;
  float get_tofg(const unsigned int i) const;
  float get_tofelossg(const unsigned int i) const;

  short get_emcclusid(const unsigned int i) const;
  short get_emcclusidtrue(const unsigned int i) const;
  short get_emcclusidg(const unsigned int i) const;
  short get_emcanctrk0(const unsigned int i) const;
  short get_emcanctrk1(const unsigned int i) const;
  short get_emcanctrk2(const unsigned int i) const;
  short get_emcanctwrhit0(const unsigned int i) const;
  short get_emcanctwrhit1(const unsigned int i) const;
  short get_emcanctwrhit2(const unsigned int i) const;
  short get_emcancpid0(const unsigned int i) const;
  short get_emcancpid1(const unsigned int i) const;
  short get_emcancpid2(const unsigned int i) const;
  float get_emcancedep0(const unsigned int i) const;
  float get_emcancedep1(const unsigned int i) const;
  float get_emcancedep2(const unsigned int i) const;
  float get_emcancptot0(const unsigned int i) const;
  float get_emcancptot1(const unsigned int i) const;
  float get_emcancptot2(const unsigned int i) const;
  float get_emcpointxg(const unsigned int i) const;
  float get_emcpointyg(const unsigned int i) const;
  float get_emcpointzg(const unsigned int i) const;
  float get_emcefracg(const unsigned int i) const;
  float get_emcecoreg(const unsigned int i) const;
  float get_emcmeaseg(const unsigned int i) const;
  float get_emctofg(const unsigned int i) const;

  short get_crkacc(const unsigned int i) const;
  short get_crknpmt0(const unsigned int i) const;
  short get_crknpmt1(const unsigned int i) const;
  short get_crknpmt3(const unsigned int i) const;
  float get_crknpe0(const unsigned int i) const;
  float get_crknpe1(const unsigned int i) const;
  float get_crknpe3(const unsigned int i) const;
  float get_crkchi2(const unsigned int i) const;
  float get_crkdisp(const unsigned int i) const;
  float get_crkpath(const unsigned int i) const;

protected: 
  
  TClonesArray *GetRecoSingleEvalList() const {return RecoList;}
  void Clear(Option_t * = "");
  RecoEvalSingleTrack* get_RecoEvalSingleTrack(const int i) const;

  unsigned int RecoEvalSingleTrackN;
  TClonesArray *RecoList;

  ClassDef(RecoEvalSingleList_v2,1)
};

#endif


