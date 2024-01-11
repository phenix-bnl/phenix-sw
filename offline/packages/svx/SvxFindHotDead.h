// ===============
// FILE: SvxFindHotDead.h
// ===============
#ifndef __SVXFINDHOTDEAD_H__
#define __SVXFINDHOTDEAD_H__

#include <SubsysReco.h>
//#include "svxAddress.hh"
#include "SvxParameters.h"

class SvxDeadMap;
class PHCompositeNode;
class TH2F;

/**
 * @brief  A SubsysReco module to find hot or dead channel and write them
 *         to the database.
 * @date  Created by Sasha Lebedev in September 2010
 */
class SvxFindHotDead : public SubsysReco
{

 public:

  SvxFindHotDead(const std::string &name = "SVXFINDHOTDEAD");
  virtual ~SvxFindHotDead() {}

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void set_OutputFileName(std::string name) {OutputFileName=name;}

 protected:

  TH2F** h_hotdead_layer0;
  TH2F** h_hotdead_layer1;
  TH2F** h_hotdead_layer2;
  TH2F** h_hotdead_layer3;

 // svxAddress SvxAddressObject;
  SvxDeadMap* theMap;
  int EventNumber;
  bool WriteToDatabase;
  std::string OutputFileName;

};
#endif 


