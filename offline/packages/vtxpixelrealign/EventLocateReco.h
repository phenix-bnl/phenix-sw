#ifndef EVENTLOCATERECO_H__
#define EVENTLOCATERECO_H__

#include <SubsysReco.h>
#include <string>
#include <sstream>

class PHCompositeNode;

class SvxPixelRawHitList;
class SvxStripRawHitList;

class EventLocateReco: public SubsysReco
{
 public:
  EventLocateReco(const std::string &name = "EVENTLOCATE");
  virtual ~EventLocateReco() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

 protected:
  int evtcnt;
  SvxPixelRawHitList *d_pixel;
  SvxStripRawHitList *d_strip;
  std::ostringstream nodename;

};

#endif /* EVENTLOCATERECO_H__ */
