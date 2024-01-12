#ifndef __ERTSIMRECO_H__
#define __ERTSIMRECO_H__

#include "SubsysReco.h"
#include "TObject.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHTypedNodeIterator.h"
#include "PHNodeIterator.h"
#include "PHNodeReset.h"
#include "recoConsts.h"

#include "RunHeader.h"
#include <cmath>

#include <dEmcRawDataWrapper.h>
#include "EmcSimuRawDataReCal.h"


// crk
#include "CrkDAO.h"
#include "TCrkModule.h"

#include "crkghitWrapper.h"
#include "dCrkCalWrapper.h"
#include "dCrkDCMparWrapper.h"
#include "dCrkDCMWrapper.h"
#include "dCrkFEMWrapper.h"
#include "dCrkGeoWrapper.h"
#include "dCrkGhitRawParWrapper.h"
#include "dCrkHitWrapper.h"
#include "dCrkRawFEMparWrapper.h"
#include "dCrkRawHitParWrapper.h"
#include "dCrkRawWrapper.h"
#include "dCrkRel2sWrapper.h"
#include "dCrkUcalWrapper.h"
#include "CrkSimuRawReCal.h"
#include "CrkHitv1.h"
#include "CrkGetGEA.h"

#include "dErtDcmDataWrapper.h"
#include "dErtFemDataWrapper.h"
#include "ErtOut.h"
#include "ErtOutv1.h"
#include "ERTSimulator.h"


class PHCompositeNode;
class ERTSimulator;


class ErtSimreco: public SubsysReco
{
 public:
  int RunNumber;

  ErtSimreco(const char *name = "ERT", const int run = 80312, const char *node_name = "ErtOut" );
  virtual ~ErtSimreco() {}


  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int Reset(PHCompositeNode *topNode) {return 0;}
  int ResetEvent(PHCompositeNode *topNode);
  void Print(const std::string&) const {}

 protected:
  size_t mr, nrc;

  int CreateNodeTree(PHCompositeNode *topNode);

  ERTSimulator       *ertsim;
  TString NodeName;

};

#endif /* __ERTSIMRECO_H__ */
