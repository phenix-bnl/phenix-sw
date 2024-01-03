#ifndef __emCalibMaster_HH__
#define __emCalibMaster_HH__

#include "TString.h"

class emcClusterContainer;
class emcClusterContent;
class emcTowerContainer;
class PHCompositeNode;

class emCalibMaster {
 public:
  emCalibMaster();
  emCalibMaster(const emCalibMaster &cpy);
  emCalibMaster& operator=(const emCalibMaster &cpy);
  virtual ~emCalibMaster();
  void InitTables(PHCompositeNode *topNode);
  int LoadGlobals(PHCompositeNode *topNode);
  void Read_EMCTOF_Tables(int run,TString sel="");
  void Read_EMC_Files(TString deadmap, TString gainmap, int trun, bool rvc);
  float NLC_EMC_PbSc(float ene);
  float NLC_EMC_PbGl(float ene);
  float ECoreCorr(emcClusterContent *emc);
  float TOF(emcClusterContent *emc,float len);
  float GetECore(int id);
  float GetETOF(int id, float len, int &tid);

 protected:
  emcClusterContainer *fEMCCont; // made datamembers because of
  emcTowerContainer *fEMTCont;   // recurrence in loops

  float fT0;
  float fVZ;
  int fEMCDead[8][48][96];
  float fEMCSector[8];
  float fEMCTOF_s0[8];
  float fEMCTOF_s1[8];
  float fEMCTOF_s2[8];
  float fEMCTOF_t0[24768][3];
  float fEMCTOF_t1[24768][2];
  float fEMCTOF_lc[24768];
};
#endif /* __emCalibMaster_H__ */
