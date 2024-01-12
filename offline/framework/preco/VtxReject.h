#ifndef __VTXREJECT_H__
#define __VTXREJECT_H__

#include <SubsysReco.h>
#include <TriggerHelper.h>
#include <TriggerUtilities.h>

#include <set>
#include <string>

class PHCompositeNode;

class VtxReject: public SubsysReco
{
 public:
  VtxReject(const char *name = "VTXREJ", const float a=-20, const float b=20);
  virtual ~VtxReject() {}

  int process_event(PHCompositeNode *topNode);

  void SetVertexRange(const float a, const float b) {min_vtx=a; max_vtx=b;return;}
  void SetBbcChargeRange(const float a, const float b) {min_charge = a; max_charge=b;}
  void IgnoreTrigger(const std::string &name) {ignoretrig.insert(name);}

 protected:

  TriggerHelper _trigger_helper;
  std::set<std::string> ignoretrig;
  float min_vtx;
  float max_vtx;
  float min_charge;
  float max_charge;
};

#endif /* __VTXREJECT_H__ */
