#ifndef __FCLCOMPARE_H__
#define __FCLCOMPARE_H__

class FclIndexer;
class PHCompositeNode;
class TFile;
class TNtuple;

class FclCompare
{
 public:

  FclCompare();                          
  virtual ~FclCompare() {}

  int compare(PHCompositeNode *topNode); // the work-horse routine
  void saveCompare();

 protected:

  FclIndexer* indexer;
  TFile* FclCompareFile;
  TNtuple* FclCompareNt;

};

#endif







