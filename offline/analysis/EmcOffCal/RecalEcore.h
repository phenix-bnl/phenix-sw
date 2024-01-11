#ifndef __RECALECORE_H__
#define __RECALECORE_H__

#include "EmcAnaCommon.h"
#include <SubsysReco.h>
#include <vector>

class PHCompositeNode;
class emcClusterContent;
class Coefficient;

class RecalEcore: public SubsysReco {
   Coefficient* m_coef;
   emcClusterContent* m_cluscont; //!

 public:
   RecalEcore(const char* name = "RECALECORE");
   virtual ~RecalEcore() {}

   int End(PHCompositeNode *topNode); // called at EndRun
   int Init(PHCompositeNode *topNode); // Initialization at startup
   int InitRun(PHCompositeNode *topNode); // Initializations which need the run number
   int process_event(PHCompositeNode *topNode);
   //  int Reset(PHCompositeNode *topNode) { return EVENT_OK; };
   //  int ResetEvent(PHCompositeNode *topNode) { return EVENT_OK; };
   void Print(const std::string&) const { return; }

   void ReadCoef(const char* coef_table, const char* coef_table_supermod);
   double GetCoef(const int as, const int y, const int z);
   void ExecRecal(emcClusterContent* cluscont);
   void ExecRecal(Int_t n_clus, Float_t* ecore, Int_t* multiplicity, Int_t* towerid, Float_t* partesum);
   
};

#endif
