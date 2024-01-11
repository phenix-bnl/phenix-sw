#ifndef MCEVALSINGLELIST_H
#define MCEVALSINGLELIST_H

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class McEvalSingleTrack;

class McEvalSingleList : public PHObject {
public:
  McEvalSingleList() {};
  virtual ~McEvalSingleList() {};

  virtual void Reset() = 0;

  virtual int set_TClonesArraySize(unsigned int fullsize) = 0;

  virtual int AddMcEvalSingleTrack(McEvalSingleTrack *newtrack,
                                   const int i) = 0;

  virtual unsigned int get_McEvalSingleTrackN() const = 0;
  virtual void set_McEvalSingleTrackN(const unsigned int ntrk) = 0;

  virtual void set_eventid(const unsigned int i, const int val) = 0;
  virtual void set_mctrackid(const unsigned int i, const int val) = 0;
  virtual void set_generation(const unsigned int i, const int val) = 0;
  virtual void set_particleid(const unsigned int i, const int val) = 0;
  virtual void set_parentid(const unsigned int i, const int val) = 0;
  virtual void set_primaryid(const unsigned int i, const int val) = 0;
  virtual void set_vertexx(const unsigned int i, const float val) = 0;
  virtual void set_vertexy(const unsigned int i, const float val) = 0;
  virtual void set_vertexz(const unsigned int i, const float val) = 0;
  virtual void set_parentvertexx(const unsigned int i, const float val) = 0;
  virtual void set_parentvertexy(const unsigned int i, const float val) = 0;
  virtual void set_parentvertexz(const unsigned int i, const float val) = 0;
  virtual void set_primaryvertexx(const unsigned int i, const float val) = 0;
  virtual void set_primaryvertexy(const unsigned int i, const float val) = 0;
  virtual void set_primaryvertexz(const unsigned int i, const float val) = 0;
  virtual void set_momentumx(const unsigned int i, const float val) = 0;
  virtual void set_momentumy(const unsigned int i, const float val) = 0;
  virtual void set_momentumz(const unsigned int i, const float val) = 0;
  virtual void set_parentmomentumx(const unsigned int i, const float val) = 0;
  virtual void set_parentmomentumy(const unsigned int i, const float val) = 0;
  virtual void set_parentmomentumz(const unsigned int i, const float val) = 0;
  virtual void set_primarymomentumx(const unsigned int i, const float val) = 0;
  virtual void set_primarymomentumy(const unsigned int i, const float val) = 0;
  virtual void set_primarymomentumz(const unsigned int i, const float val) = 0;
  virtual void set_quality(const unsigned int i, const int val) = 0;
  virtual void set_momentumr(const unsigned int i, const double val) = 0;
  virtual void set_theta0(const unsigned int i, const double val) = 0;
  virtual void set_phi0(const unsigned int i, const double val) = 0;
  virtual void set_phi(const unsigned int i, const double val) = 0;
  virtual void set_alpha(const unsigned int i, const double val) = 0;
  virtual void set_zed(const unsigned int i, const double val) = 0;
  virtual void set_beta(const unsigned int i, const double val) = 0;

  virtual void set_recoid(const unsigned int i, const unsigned int j,
                          const short val) = 0;
  virtual void set_quality(const unsigned int i, const unsigned int j,
                           const short val) = 0;
  virtual void set_momentum(const unsigned int i, const unsigned int j,
                            const float val) = 0;
  virtual void set_theta0(const unsigned int i, const unsigned int j,
                          const float val) = 0;
  virtual void set_phi0(const unsigned int i, const unsigned int j,
                        const float val) = 0;
  virtual void set_phi(const unsigned int i, const unsigned int j,
                       const float val) = 0;
  virtual void set_alpha(const unsigned int i, const unsigned int j,
                         const float val) = 0;
  virtual void set_zed(const unsigned int i, const unsigned int j,
                       const float val) = 0;
  virtual void set_beta(const unsigned int i, const unsigned int j,
                        const float val) = 0;
  virtual void set_averagetime(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_xhits(const unsigned int i, const unsigned int j,
                         const short val) = 0;
  virtual void set_uvhits(const unsigned int i, const unsigned int j,
                          const short val) = 0;
  virtual void set_mulmain(const unsigned int i, const unsigned int j,
                           const short val) = 0;
  virtual void set_mulxmain(const unsigned int i, const unsigned int j,
                            const short val) = 0;
  virtual void set_muluvmain(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_main(const unsigned int i, const unsigned int j,
                        const short val) = 0;
  virtual void set_xmain(const unsigned int i, const unsigned int j,
                         const short val) = 0;
  virtual void set_uvmain(const unsigned int i, const unsigned int j,
                          const short val) = 0;
  virtual void set_ambiguity(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_purity(const unsigned int i, const unsigned int j,
                          const float val) = 0;
  virtual void set_xpurity(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_uvpurity(const unsigned int i, const unsigned int j,
                            const float val) = 0;
  virtual void set_pc1clusid(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_pc2clusid(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_pc3clusid(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_pc1clusidtrue(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_pc2clusidtrue(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_pc3clusidtrue(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_pc1clusidg(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_pc2clusidg(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_pc3clusidg(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_pc1pointxg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc1pointyg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc1pointzg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc2pointxg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc2pointyg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc2pointzg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc3pointxg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc3pointyg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_pc3pointzg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_tofid(const unsigned int i, const unsigned int j,
                         const short val) = 0;
  virtual void set_tofidtrue(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_tofidg(const unsigned int i, const unsigned int j,
                          const short val) = 0;
  virtual void set_tofpointxg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_tofpointyg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_tofpointzg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_tofg(const unsigned int i, const unsigned int j,
                        const float val) = 0;
  virtual void set_tofelossg(const unsigned int i, const unsigned int j,
                             const float val) = 0;
  virtual void set_emcclusid(const unsigned int i, const unsigned int j,
                             const short val) = 0;
  virtual void set_emcclusidtrue(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_emcclusidg(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcanctrk0(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcanctrk1(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcanctrk2(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcanctwrhit0(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_emcanctwrhit1(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_emcanctwrhit2(const unsigned int i, const unsigned int j,
                                 const short val) = 0;
  virtual void set_emcancpid0(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcancpid1(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcancpid2(const unsigned int i, const unsigned int j,
                              const short val) = 0;
  virtual void set_emcancedep0(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_emcancedep1(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_emcancedep2(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_emcancptot0(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_emcancptot1(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_emcancptot2(const unsigned int i, const unsigned int j,
                               const float val) = 0;
  virtual void set_emcpointxg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_emcpointyg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_emcpointzg(const unsigned int i, const unsigned int j,
                              const float val) = 0;
  virtual void set_emcefracg(const unsigned int i, const unsigned int j,
                             const float val) = 0;
  virtual void set_emcecoreg(const unsigned int i, const unsigned int j,
                             const float val) = 0;
  virtual void set_emcmeaseg(const unsigned int i, const unsigned int j,
                             const float val) = 0;
  virtual void set_emctofg(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_crkacc(const unsigned int i, const unsigned int j,
                          const short val) = 0;
  virtual void set_crknpmt0(const unsigned int i, const unsigned int j,
                            const short val) = 0;
  virtual void set_crknpmt1(const unsigned int i, const unsigned int j,
                            const short val) = 0;
  virtual void set_crknpmt3(const unsigned int i, const unsigned int j,
                            const short val) = 0;
  virtual void set_crknpe0(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_crknpe1(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_crknpe3(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_crkchi2(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_crkdisp(const unsigned int i, const unsigned int j,
                           const float val) = 0;
  virtual void set_crkpath(const unsigned int i, const unsigned int j,
                           const float val) = 0;

  virtual int get_eventid(const unsigned int i) const = 0;
  virtual int get_mctrackid(const unsigned int i) const = 0;
  virtual int get_generation(const unsigned int i) const = 0;
  virtual int get_particleid(const unsigned int i) const = 0;
  virtual int get_parentid(const unsigned int i) const = 0;
  virtual int get_primaryid(const unsigned int i) const = 0;
  virtual float get_vertexx(const unsigned int i) const = 0;
  virtual float get_vertexy(const unsigned int i) const = 0;
  virtual float get_vertexz(const unsigned int i) const = 0;
  virtual float get_parentvertexx(const unsigned int i) const = 0;
  virtual float get_parentvertexy(const unsigned int i) const = 0;
  virtual float get_parentvertexz(const unsigned int i) const = 0;
  virtual float get_primaryvertexx(const unsigned int i) const = 0;
  virtual float get_primaryvertexy(const unsigned int i) const = 0;
  virtual float get_primaryvertexz(const unsigned int i) const = 0;
  virtual float get_momentumx(const unsigned int i) const = 0;
  virtual float get_momentumy(const unsigned int i) const = 0;
  virtual float get_momentumz(const unsigned int i) const = 0;
  virtual float get_parentmomentumx(const unsigned int i) const = 0;
  virtual float get_parentmomentumy(const unsigned int i) const = 0;
  virtual float get_parentmomentumz(const unsigned int i) const = 0;
  virtual float get_primarymomentumx(const unsigned int i) const = 0;
  virtual float get_primarymomentumy(const unsigned int i) const = 0;
  virtual float get_primarymomentumz(const unsigned int i) const = 0;
  virtual int get_quality(const unsigned int i) const = 0;
  virtual double get_momentumr(const unsigned int i) const = 0;
  virtual double get_theta0(const unsigned int i) const = 0;
  virtual double get_phi0(const unsigned int i) const = 0;
  virtual double get_phi(const unsigned int i) const = 0;
  virtual double get_alpha(const unsigned int i) const = 0;
  virtual double get_zed(const unsigned int i) const = 0;
  virtual double get_beta(const unsigned int i) const = 0;

  virtual short get_Nreco(const unsigned int i) const = 0;

  virtual short get_recoid(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual short get_quality(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_momentum(const unsigned int i,
                             const unsigned int j) const = 0;
  virtual float get_theta0(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual float get_phi0(const unsigned int i, const unsigned int j) const = 0;
  virtual float get_phi(const unsigned int i, const unsigned int j) const = 0;
  virtual float get_alpha(const unsigned int i, const unsigned int j) const = 0;
  virtual float get_zed(const unsigned int i, const unsigned int j) const = 0;
  virtual float get_beta(const unsigned int i, const unsigned int j) const = 0;
  virtual float get_averagetime(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual short get_xhits(const unsigned int i, const unsigned int j) const = 0;
  virtual short get_uvhits(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual short get_mulmain(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual short get_mulxmain(const unsigned int i,
                             const unsigned int j) const = 0;
  virtual short get_muluvmain(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_main(const unsigned int i, const unsigned int j) const = 0;
  virtual short get_xmain(const unsigned int i, const unsigned int j) const = 0;
  virtual short get_uvmain(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual short get_ambiguity(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual float get_purity(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual float get_xpurity(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_uvpurity(const unsigned int i,
                             const unsigned int j) const = 0;
  virtual short get_pc1clusid(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_pc2clusid(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_pc3clusid(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_pc1clusidtrue(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_pc2clusidtrue(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_pc3clusidtrue(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_pc1clusidg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_pc2clusidg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_pc3clusidg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc1pointxg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc1pointyg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc1pointzg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc2pointxg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc2pointyg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc2pointzg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc3pointxg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc3pointyg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_pc3pointzg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_tofid(const unsigned int i, const unsigned int j) const = 0;
  virtual short get_tofidtrue(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_tofidg(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual float get_tofpointxg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_tofpointyg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_tofpointzg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_tofg(const unsigned int i, const unsigned int j) const = 0;
  virtual float get_tofelossg(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_emcclusid(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual short get_emcclusidtrue(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_emcclusidg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_emcanctrk0(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_emcanctrk1(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_emcanctrk2(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_emcanctwrhit0(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_emcanctwrhit1(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_emcanctwrhit2(const unsigned int i,
                                  const unsigned int j) const = 0;
  virtual short get_emcancpid0(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_emcancpid1(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual short get_emcancpid2(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_emcancedep0(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual float get_emcancedep1(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual float get_emcancedep2(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual float get_emcancptot0(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual float get_emcancptot1(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual float get_emcancptot2(const unsigned int i,
                                const unsigned int j) const = 0;
  virtual float get_emcpointxg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_emcpointyg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_emcpointzg(const unsigned int i,
                               const unsigned int j) const = 0;
  virtual float get_emcefracg(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual float get_emcecoreg(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual float get_emcmeaseg(const unsigned int i,
                              const unsigned int j) const = 0;
  virtual float get_emctofg(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual short get_crkacc(const unsigned int i,
                           const unsigned int j) const = 0;
  virtual short get_crknpmt0(const unsigned int i,
                             const unsigned int j) const = 0;
  virtual short get_crknpmt1(const unsigned int i,
                             const unsigned int j) const = 0;
  virtual short get_crknpmt3(const unsigned int i,
                             const unsigned int j) const = 0;
  virtual float get_crknpe0(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_crknpe1(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_crknpe3(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_crkchi2(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_crkdisp(const unsigned int i,
                            const unsigned int j) const = 0;
  virtual float get_crkpath(const unsigned int i,
                            const unsigned int j) const = 0;

protected:
  ClassDef(McEvalSingleList, 1)
};

#endif
