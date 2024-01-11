#ifndef __FILLCNT_TECHITS_H__
#define __FILLCNT_TECHITS_H__

#include "dumpflag.h"

#include <SubsysReco.h>
#include <fstream>


class TecClusterContainerV1;
class TecClusterV1;
class TecGeometryObject;
class TecCalibrationObject;

class PHCentralTrack;

class FillCNT_TecHits: public SubsysReco
{
 public:
  FillCNT_TecHits(const std::string &name = "FILLCNT_TOFEHITS");
  virtual ~FillCNT_TecHits();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:

  TecClusterContainerV1 *cluscont;
  TecClusterV1 *clus;
  TecGeometryObject* TGO;
  TecCalibrationObject* TCO;

#ifdef DUMP
  std::ofstream dumpfile;
#endif

};

#endif
