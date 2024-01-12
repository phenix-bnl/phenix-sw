#ifndef __TECRECO_H__
#define __TECRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;
class mTecCalibModule;
class mTecUnpackModule;
class mTecHitClusteringModule;
class mTecDrawModule;
class TecAddressObject;
class TecGeometryObject;
class TecCalibrationObject;

class TecReco: public SubsysReco
{
 public:
  TecReco(const std::string &name = "TEC");
  virtual ~TecReco();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

  mTecCalibModule* get_mTecCalib() {return mTecCalib;}
  mTecUnpackModule* get_mTecUnpack() {return mTecUnpack;}
  mTecDrawModule* get_mTecDraw() {return mTecDraw;}
  TecAddressObject* get_TecAddress() {return TecAddress;}
  TecGeometryObject* get_TecGeometry() {return TecGeometry;}
  TecCalibrationObject* get_TecCalibration() {return TecCalibration;}

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);

  mTecCalibModule* mTecCalib;
  mTecUnpackModule* mTecUnpack;
  mTecHitClusteringModule* mTecHitClustering;
  mTecDrawModule* mTecDraw;
  TecAddressObject* TecAddress;
  TecGeometryObject* TecGeometry;
  TecCalibrationObject *TecCalibration;

  int TecDrawFlag;

};

#endif /* __TECRECO_H__ */
