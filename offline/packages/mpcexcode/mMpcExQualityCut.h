/*
  qc->SetZVertexLimit(30);
  qc->SetMaxHits(5000);   -1 takes all
  qc->SetTrigger("BBCLL1(>0 tubes) narrowvtx");   "All" takes all trigggers, look in .C for a list of triggers
  qc->SetPrintInterval(1000);
  gives stats at end
*/

#ifndef __MPCEXQUALITYCUT_HH__
#define __MPCEXQUALITYCUT_HH__

#include <string>
#include <vector>

#include "SubsysReco.h"
class PHCompositeNode;
class MpcExRawHit;
class TMpcExCalibContainer;
class MpcExEventHeader;

class mMpcExQualityCut: public SubsysReco {
 public:
  mMpcExQualityCut( const char* name = "MMPCEXQUALITYCUT" );
  virtual ~mMpcExQualityCut();

  virtual int Init(PHCompositeNode*);
  virtual int InitRun(PHCompositeNode*);
  virtual int process_event(PHCompositeNode*);
  virtual int End(PHCompositeNode *topNode);

  void SetTrigger( string );
  void SetZVertexLimit( float );
  void SetMaxHits( int );
  void SetPrintInterval( int );

 private:
  bool EvaluateTrigger(PHCompositeNode*);
  bool IsEventOK();
  MpcExRawHit *fHits;
  MpcExEventHeader *fMpcExEventHeader;
  int maxhits;
  float zvertexlimit;
  int nevents_tot;
  int nevents_pass_stackcut;
  int nevents_pass_vertexcut;
  int nevents_pass_triggercut;
  int nevents_pass_hitscut;
  int nevents_pass;
  int printinterval;

  bool using_set_list;
  bool using_All;
  std::vector<std::string> trigger_list;
  std::vector<std::string> set_trigger_list;
  std::vector<int> set_trigger_list_count;

};

#endif /* __MPCEXQUALITYCUT_H__ */ 
