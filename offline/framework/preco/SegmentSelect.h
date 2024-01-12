#ifndef __SEGMENTSELECT_H__
#define __SEGMENTSELECT_H__

#include <SubsysReco.h>
#include <set>

class SegmentSelect: public SubsysReco
{
 public:

  SegmentSelect(const std::string &name = "SEGMENTSELECT");

  virtual ~SegmentSelect() {}

  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void Print(const std::string &what="ALL") const;
  void AddSegment(const int iseg);
  void DeleteSegment(const int iseg);
  void CheckDuplicateEvents(const int i=1) {check_dupl = i;}

 protected:

  int events;
  int dupl_events;
  int check_dupl;
  std::set<int> segmentlist;
  std::set<int> eventnumber;
};

#endif /* __SEGMENTSELECT_H__ */
