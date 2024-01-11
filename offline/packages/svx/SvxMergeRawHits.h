#ifndef __SVXMERGERAWHITS_H__
#define __SVXMERGERAWHITS_H__

#include <SubsysReco.h>

#include <phool.h>

class SvxMergeRawHits : public SubsysReco
{

 public:

  SvxMergeRawHits(const std::string &name = "SVXMERGERAWHITS");
  virtual ~SvxMergeRawHits();

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);

  void set_merge(bool m) {merge=m;}
  void set_mergefilename(std::string &filename);
  void set_ZVtxCut(float a, float b) {ZVtx1=a; ZVtx2=b;}
  void set_BbcCut(float a, float b)  {BbcCut1=a; BbcCut2=b;}
  void set_CentralityCut(float a, float b)  {CentCut1=a; CentCut2=b;}
  void set_whichVertex(std::string wv) {whichVertex=wv;}
  void set_Nskip(int a) {Nskip=a;}

 protected:
  int process_event_merge(PHCompositeNode *topNode);
  int process_event_dump(PHCompositeNode *topNode);

 private:
  bool merge;
  int EventNumber;
  std::string MergeFileName;
  float ZVtx1;
  float ZVtx2;
  float BbcCut1;
  float BbcCut2;
  float CentCut1;
  float CentCut2;
  std::string whichVertex;  
  int Nskip;

};
#endif 


