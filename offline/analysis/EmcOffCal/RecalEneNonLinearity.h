#ifndef RECALENENONLINEARITY_H__
#define RECALENENONLINEARITY_H__

#include "EmcAnaCommon.h"

#include <SubsysReco.h>


class RecalEneNonLinearity: public SubsysReco
{
 public:
   RecalEneNonLinearity() {return;}
   virtual ~RecalEneNonLinearity() {}

   //  Routines for Fun4All
   //  For this analysis we only use Init and process_event;
   int Init(PHCompositeNode *topNode) {return 0;}
   int InitRun(PHCompositeNode *topNode) {return 0;}
   int process_event(PHCompositeNode *topNode);
   int ResetEvent(PHCompositeNode *topNode) {return 0;}
   int Reset(PHCompositeNode *topNode) {return 0;}
   int End(PHCompositeNode *topNode) {return 0;}
   void Print(const std::string&) const { return; }

 private:
   void RecalRun3(float& ecore, int armsect);
   void RecalRun5(int run, float& ecore, int armsect);
};

#endif 
