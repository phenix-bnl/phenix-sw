#ifndef MCEVALSINGLELIST_v1_H
#define MCEVALSINGLELIST_v1_H

#include "McEvalSingleList.h"

class McEvalSingleTrack;
class TClonesArray;

class McEvalSingleList_v1 : public McEvalSingleList
{
 public:
  McEvalSingleList_v1();
  virtual ~McEvalSingleList_v1();

  void Reset();

  int set_TClonesArraySize(unsigned int fullsize);

  int AddMcEvalSingleTrack(McEvalSingleTrack *newtrack, const int i);  

  unsigned int get_McEvalSingleTrackN() const 
    {return McEvalSingleTrackN;}
  void set_McEvalSingleTrackN(const unsigned int ntrk) 
    {McEvalSingleTrackN = ntrk; return;}

  void set_eventid(const unsigned int i, const int val);
  void set_mctrackid(const unsigned int i, const int val);
  void set_generation(const unsigned int i, const int val);
  void set_particleid(const unsigned int i, const int val);
  void set_parentid(const unsigned int i, const int val);
  void set_primaryid(const unsigned int i, const int val);
  void set_vertexx(const unsigned int i, const float val);
  void set_vertexy(const unsigned int i, const float val);
  void set_vertexz(const unsigned int i, const float val);
  void set_parentvertexx(const unsigned int i, const float val);
  void set_parentvertexy(const unsigned int i, const float val);
  void set_parentvertexz(const unsigned int i, const float val);
  void set_primaryvertexx(const unsigned int i, const float val);
  void set_primaryvertexy(const unsigned int i, const float val);
  void set_primaryvertexz(const unsigned int i, const float val);
  void set_momentumx(const unsigned int i, const float val);
  void set_momentumy(const unsigned int i, const float val);
  void set_momentumz(const unsigned int i, const float val);
  void set_parentmomentumx(const unsigned int i, const float val);
  void set_parentmomentumy(const unsigned int i, const float val);
  void set_parentmomentumz(const unsigned int i, const float val);
  void set_primarymomentumx(const unsigned int i, const float val);
  void set_primarymomentumy(const unsigned int i, const float val);
  void set_primarymomentumz(const unsigned int i, const float val);
  void set_quality(const unsigned int i, const int val);
  void set_momentumr(const unsigned int i, const double val);
  void set_theta0(const unsigned int i, const double val);
  void set_phi0(const unsigned int i, const double val);
  void set_phi(const unsigned int i, const double val);
  void set_alpha(const unsigned int i, const double val);
  void set_zed(const unsigned int i, const double val);
  void set_beta(const unsigned int i, const double val);

  void set_recoid(const unsigned int i, const unsigned int j, 
		  const short val);
  void set_quality(const unsigned int i, const unsigned int j, 
		   const short val);
  void set_momentum(const unsigned int i, const unsigned int j, 
		    const float val);
  void set_theta0(const unsigned int i, const unsigned int j, 
		  const float val);
  void set_phi0(const unsigned int i, const unsigned int j, 
		const float val);
  void set_phi(const unsigned int i, const unsigned int j, 
	       const float val);
  void set_alpha(const unsigned int i, const unsigned int j, 
		 const float val);
  void set_zed(const unsigned int i, const unsigned int j, 
	       const float val);
  void set_beta(const unsigned int i, const unsigned int j, 
		const float val);
  void set_averagetime(const unsigned int i, const unsigned int j, 
		       const float val);
  void set_xhits(const unsigned int i, const unsigned int j, 
		 const short val);
  void set_uvhits(const unsigned int i, const unsigned int j, 
		  const short val);
  void set_mulmain(const unsigned int i, const unsigned int j,
		   const short val);
  void set_mulxmain(const unsigned int i, const unsigned int j, 
		    const short val);
  void set_muluvmain(const unsigned int i, const unsigned int j, 
		     const short val);
  void set_main(const unsigned int i, const unsigned int j,
		const short val);
  void set_xmain(const unsigned int i, const unsigned int j,
		 const short val);
  void set_uvmain(const unsigned int i, const unsigned int j,
		  const short val);
  void set_ambiguity(const unsigned int i, const unsigned int j,
		     const short val);
  void set_purity(const unsigned int i, const unsigned int j, 
		  const float val);
  void set_xpurity(const unsigned int i, const unsigned int j,
		   const float val);
  void set_uvpurity(const unsigned int i, const unsigned int j,
		    const float val);
  void set_pc1clusid(const unsigned int i, const unsigned int j,
		     const short val);
  void set_pc2clusid(const unsigned int i, const unsigned int j,
		     const short val);
  void set_pc3clusid(const unsigned int i, const unsigned int j,
		     const short val);
  void set_pc1clusidtrue(const unsigned int i, const unsigned int j,
			 const short val);
  void set_pc2clusidtrue(const unsigned int i, const unsigned int j,
			 const short val);
  void set_pc3clusidtrue(const unsigned int i, const unsigned int j,
			 const short val);
  void set_pc1clusidg(const unsigned int i, const unsigned int j,
		      const short val);
  void set_pc2clusidg(const unsigned int i, const unsigned int j,
		      const short val);
  void set_pc3clusidg(const unsigned int i, const unsigned int j,
		      const short val);
  void set_pc1pointxg(const unsigned int i, const unsigned int j,
		      const float val);
  void set_pc1pointyg(const unsigned int i, const unsigned int j,
		      const float val);
  void set_pc1pointzg(const unsigned int i, const unsigned int j,
		      const float val);
  void set_pc2pointxg(const unsigned int i, const unsigned int j,
		     const float val);
  void set_pc2pointyg(const unsigned int i, const unsigned int j,
		     const float val);
  void set_pc2pointzg(const unsigned int i, const unsigned int j,
		     const float val);
  void set_pc3pointxg(const unsigned int i, const unsigned int j, 
		     const float val);
  void set_pc3pointyg(const unsigned int i, const unsigned int j, 
		     const float val);
  void set_pc3pointzg(const unsigned int i, const unsigned int j, 
		     const float val);
  void set_tofid(const unsigned int i, const unsigned int j,
		 const short val);
  void set_tofidtrue(const unsigned int i, const unsigned int j,
		     const short val);
  void set_tofidg(const unsigned int i, const unsigned int j, 
		  const short val);
  void set_tofpointxg(const unsigned int i, const unsigned int j, 
		      const float val);
  void set_tofpointyg(const unsigned int i, const unsigned int j, 
		      const float val);
  void set_tofpointzg(const unsigned int i, const unsigned int j, 
		      const float val);
  void set_tofg(const unsigned int i, const unsigned int j,
		const float val);
  void set_tofelossg(const unsigned int i, const unsigned int j, 
		     const float val);
  void set_emcclusid(const unsigned int i, const unsigned int j,
		     const short val);
  void set_emcclusidtrue(const unsigned int i, const unsigned int j,
			 const short val);
  void set_emcclusidg(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcanctrk0(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcanctrk1(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcanctrk2(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcanctwrhit0(const unsigned int i, const unsigned int j,
			 const short val);
  void set_emcanctwrhit1(const unsigned int i, const unsigned int j,
			 const short val);
  void set_emcanctwrhit2(const unsigned int i, const unsigned int j,
			 const short val);
  void set_emcancpid0(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcancpid1(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcancpid2(const unsigned int i, const unsigned int j,
		      const short val);
  void set_emcancedep0(const unsigned int i, const unsigned int j,
		       const float val);
  void set_emcancedep1(const unsigned int i, const unsigned int j,
		       const float val);
  void set_emcancedep2(const unsigned int i, const unsigned int j,
		       const float val);
  void set_emcancptot0(const unsigned int i, const unsigned int j,
		       const float val);
  void set_emcancptot1(const unsigned int i, const unsigned int j,
		       const float val);
  void set_emcancptot2(const unsigned int i, const unsigned int j,
		       const float val);
  void set_emcpointxg(const unsigned int i, const unsigned int j,
		      const float val);
  void set_emcpointyg(const unsigned int i, const unsigned int j,
		      const float val);
  void set_emcpointzg(const unsigned int i, const unsigned int j,
		      const float val);
  void set_emcefracg(const unsigned int i, const unsigned int j,
		     const float val);
  void set_emcecoreg(const unsigned int i, const unsigned int j,
		     const float val);
  void set_emcmeaseg(const unsigned int i, const unsigned int j,
		     const float val);
  void set_emctofg(const unsigned int i, const unsigned int j,
		   const float val);
  void set_crkacc(const unsigned int i, const unsigned int j,
		  const short val);
  void set_crknpmt0(const unsigned int i, const unsigned int j,
		    const short val);
  void set_crknpmt1(const unsigned int i, const unsigned int j,
		    const short val);
  void set_crknpmt3(const unsigned int i, const unsigned int j,
		    const short val);
  void set_crknpe0(const unsigned int i, const unsigned int j,
		   const float val);
  void set_crknpe1(const unsigned int i, const unsigned int j,
		   const float val);
  void set_crknpe3(const unsigned int i, const unsigned int j,
		   const float val);
  void set_crkchi2(const unsigned int i, const unsigned int j,
		   const float val);
  void set_crkdisp(const unsigned int i, const unsigned int j,
		   const float val);
  void set_crkpath(const unsigned int i, const unsigned int j,
		   const float val);

  int get_eventid(const unsigned int i) const;
  int get_mctrackid(const unsigned int i) const;
  int get_generation(const unsigned int i) const;
  int get_particleid(const unsigned int i) const;
  int get_parentid(const unsigned int i) const;
  int get_primaryid(const unsigned int i) const;
  float get_vertexx(const unsigned int i) const;
  float get_vertexy(const unsigned int i) const;
  float get_vertexz(const unsigned int i) const;
  float get_parentvertexx(const unsigned int i) const;
  float get_parentvertexy(const unsigned int i) const;
  float get_parentvertexz(const unsigned int i) const;
  float get_primaryvertexx(const unsigned int i) const;
  float get_primaryvertexy(const unsigned int i) const;
  float get_primaryvertexz(const unsigned int i) const;
  float get_momentumx(const unsigned int i) const;
  float get_momentumy(const unsigned int i) const;
  float get_momentumz(const unsigned int i) const;
  float get_parentmomentumx(const unsigned int i) const;
  float get_parentmomentumy(const unsigned int i) const;
  float get_parentmomentumz(const unsigned int i) const;
  float get_primarymomentumx(const unsigned int i) const;
  float get_primarymomentumy(const unsigned int i) const;
  float get_primarymomentumz(const unsigned int i) const;
  int get_quality(const unsigned int i) const;
  double get_momentumr(const unsigned int i) const;
  double get_theta0(const unsigned int i) const;
  double get_phi0(const unsigned int i) const;
  double get_phi(const unsigned int i) const;
  double get_alpha(const unsigned int i) const;
  double get_zed(const unsigned int i) const;
  double get_beta(const unsigned int i) const;

  short get_Nreco(const unsigned int i) const;

  short get_recoid(const unsigned int i, const unsigned int j) const;
  short get_quality(const unsigned int i, const unsigned int j) const;
  float get_momentum(const unsigned int i, const unsigned int j) const;
  float get_theta0(const unsigned int i, const unsigned int j) const;
  float get_phi0(const unsigned int i, const unsigned int j) const;
  float get_phi(const unsigned int i, const unsigned int j) const;
  float get_alpha(const unsigned int i, const unsigned int j) const;
  float get_zed(const unsigned int i, const unsigned int j) const;
  float get_beta(const unsigned int i, const unsigned int j) const;
  float get_averagetime(const unsigned int i, const unsigned int j) const;
  short get_xhits(const unsigned int i, const unsigned int j) const;
  short get_uvhits(const unsigned int i, const unsigned int j) const;
  short get_mulmain(const unsigned int i, const unsigned int j) const;
  short get_mulxmain(const unsigned int i, const unsigned int j) const;
  short get_muluvmain(const unsigned int i, const unsigned int j) const;
  short get_main(const unsigned int i, const unsigned int j) const;
  short get_xmain(const unsigned int i, const unsigned int j) const;
  short get_uvmain(const unsigned int i, const unsigned int j) const;
  short get_ambiguity(const unsigned int i, const unsigned int j) const;
  float get_purity(const unsigned int i, const unsigned int j) const;
  float get_xpurity(const unsigned int i, const unsigned int j) const;
  float get_uvpurity(const unsigned int i, const unsigned int j) const;
  short get_pc1clusid(const unsigned int i, const unsigned int j) const;
  short get_pc2clusid(const unsigned int i, const unsigned int j) const;
  short get_pc3clusid(const unsigned int i, const unsigned int j) const;
  short get_pc1clusidtrue(const unsigned int i, const unsigned int j) const;
  short get_pc2clusidtrue(const unsigned int i, const unsigned int j) const;
  short get_pc3clusidtrue(const unsigned int i, const unsigned int j) const;
  short get_pc1clusidg(const unsigned int i, const unsigned int j) const;
  short get_pc2clusidg(const unsigned int i, const unsigned int j) const;
  short get_pc3clusidg(const unsigned int i, const unsigned int j) const;
  float get_pc1pointxg(const unsigned int i, const unsigned int j) const;
  float get_pc1pointyg(const unsigned int i, const unsigned int j) const;
  float get_pc1pointzg(const unsigned int i, const unsigned int j) const;
  float get_pc2pointxg(const unsigned int i, const unsigned int j) const;
  float get_pc2pointyg(const unsigned int i, const unsigned int j) const;
  float get_pc2pointzg(const unsigned int i, const unsigned int j) const;
  float get_pc3pointxg(const unsigned int i, const unsigned int j) const;
  float get_pc3pointyg(const unsigned int i, const unsigned int j) const;
  float get_pc3pointzg(const unsigned int i, const unsigned int j) const;
  short get_tofid(const unsigned int i, const unsigned int j) const;
  short get_tofidtrue(const unsigned int i, const unsigned int j) const;
  short get_tofidg(const unsigned int i, const unsigned int j) const;
  float get_tofpointxg(const unsigned int i, const unsigned int j) const;
  float get_tofpointyg(const unsigned int i, const unsigned int j) const;
  float get_tofpointzg(const unsigned int i, const unsigned int j) const;
  float get_tofg(const unsigned int i, const unsigned int j) const;
  float get_tofelossg(const unsigned int i, const unsigned int j) const;
  short get_emcclusid(const unsigned int i, const unsigned int j) const;
  short get_emcclusidtrue(const unsigned int i, const unsigned int j) const;
  short get_emcclusidg(const unsigned int i, const unsigned int j) const;
  short get_emcanctrk0(const unsigned int i, const unsigned int j) const;
  short get_emcanctrk1(const unsigned int i, const unsigned int j) const;
  short get_emcanctrk2(const unsigned int i, const unsigned int j) const;
  short get_emcanctwrhit0(const unsigned int i, const unsigned int j) const;
  short get_emcanctwrhit1(const unsigned int i, const unsigned int j) const;
  short get_emcanctwrhit2(const unsigned int i, const unsigned int j) const;
  short get_emcancpid0(const unsigned int i, const unsigned int j) const;
  short get_emcancpid1(const unsigned int i, const unsigned int j) const;
  short get_emcancpid2(const unsigned int i, const unsigned int j) const;
  float get_emcancedep0(const unsigned int i, const unsigned int j) const;
  float get_emcancedep1(const unsigned int i, const unsigned int j) const;
  float get_emcancedep2(const unsigned int i, const unsigned int j) const;
  float get_emcancptot0(const unsigned int i, const unsigned int j) const;
  float get_emcancptot1(const unsigned int i, const unsigned int j) const;
  float get_emcancptot2(const unsigned int i, const unsigned int j) const;
  float get_emcpointxg(const unsigned int i, const unsigned int j) const;
  float get_emcpointyg(const unsigned int i, const unsigned int j) const;
  float get_emcpointzg(const unsigned int i, const unsigned int j) const;
  float get_emcefracg(const unsigned int i, const unsigned int j) const;
  float get_emcecoreg(const unsigned int i, const unsigned int j) const;
  float get_emcmeaseg(const unsigned int i, const unsigned int j) const;
  float get_emctofg(const unsigned int i, const unsigned int j) const;
  short get_crkacc(const unsigned int i, const unsigned int j) const;
  short get_crknpmt0(const unsigned int i, const unsigned int j) const;
  short get_crknpmt1(const unsigned int i, const unsigned int j) const;
  short get_crknpmt3(const unsigned int i, const unsigned int j) const;
  float get_crknpe0(const unsigned int i, const unsigned int j) const;
  float get_crknpe1(const unsigned int i, const unsigned int j) const;
  float get_crknpe3(const unsigned int i, const unsigned int j) const;
  float get_crkchi2(const unsigned int i, const unsigned int j) const;
  float get_crkdisp(const unsigned int i, const unsigned int j) const;
  float get_crkpath(const unsigned int i, const unsigned int j) const;

  //--------------------------------
protected: 
  
  TClonesArray *GetMcSingleEvalList() const {return McList;}
  void Clear(Option_t * = "");
  McEvalSingleTrack* get_McEvalSingleTrack(const int i) const;

  unsigned int McEvalSingleTrackN;
  TClonesArray *McList;

  ClassDef(McEvalSingleList_v1,1)
};

#endif
