#ifndef PADVTXRECO_H__
#define PADVTXRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class PadVertexFunction;
class mEmcClusterizerv0;

class PadVtxReco: public SubsysReco
{
 public:
  PadVtxReco(const std::string &name = "PADDCHVTX");
  virtual ~PadVtxReco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  PadVertexFunction * padvtx;
  float zdc_t0_offset;
  float bbc_t0_offset;

  mEmcClusterizerv0* fClusterizer;

  static const float fgTowerThresholdPbSc;
  static const float fgTowerThresholdPbGl;
  static const float fgMinClusterEnergyPbSc;
  static const float fgMinClusterEnergyPbGl;
};

#endif /* __PADVTXRECO_H__ */
