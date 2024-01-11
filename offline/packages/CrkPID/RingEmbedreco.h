#ifndef __RINGEMBEDRECO_H__
#define __RINGEMBEDRECO_H__

#include <SubsysReco.h>
#include <PHLine.h>

class PHCompositeNode;
class CrkPID;

class RingEmbedreco: public SubsysReco
{
 public:
  RingEmbedreco(const std::string &name="RINGEMBED");
  virtual ~RingEmbedreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:
  int CreateNodeTree(PHCompositeNode *topNode);
  PHLine ReflectInZ(const PHLine &trk);
  CrkPID *crkpid;
  int EventNumber;

};

#endif /* __RINGRECO_H__ */
