/*
  
  BbcMultipleVtxChecker.cc
  
  Created: 2011/09/09
  Last Update: 2011/09/09
  Author: Hideyuki Oide
  
  
  Description:
  
  A simple checker to print BbcMultipleVtx DST node.
  
 */


#ifndef __BBCMULTIPLEVTXCHECKER__
#define __BBCMULTIPLEVTXCHECKER__

// PHENIX
#include <SubsysReco.h>
#include <Bbc.hh>

class PHCompositeNode;
class TrigLvl1;
class RunHeader;
class EventHeader;

// BbcMultipleVtx
class BbcMultipleVtx;

class BbcMultipleVtxChecker : public SubsysReco {
public:
  // Subsystem
  BbcMultipleVtxChecker(const std::string &name="BbcMultipleVtxChecker");
  virtual ~BbcMultipleVtxChecker(void){;}
  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  
  
private:

  // Nodes
  RunHeader *fRunHeader;
  EventHeader *fEventHeader;
  TrigLvl1 *fTrigLvl1;
  BbcMultipleVtx *fBbcMvtx;
 
};



#endif /* __BBCMULTIPLEVTXRECO__ */
