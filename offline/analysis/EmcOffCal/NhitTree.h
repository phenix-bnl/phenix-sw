// the base class SubsysReco provides the stl string variable "ThisName"
// and an int variable verbosity which can be set via
// Verbosity(const int ival);
// It can be used to e.g. configure printouts from the NhitTree Module

#ifndef NHITTREE_H__
#define NHITTREE_H__

#include "EmcAnaCommon.h"

#include <SubsysReco.h>

#include <TString.h>

class PHCompositeNode;
class TH3D;
class TFile;

class NhitTree: public SubsysReco
{
 public:
   NhitTree(
      const int run, const char* ofilename, const char *name = "NHITREE");
   virtual ~NhitTree();
   
   int End(PHCompositeNode *topNode); // called at EndRun
   int Init(PHCompositeNode *topNode); // Initialization at startup - create histos here
   int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
   int process_event(PHCompositeNode *topNode); // your analysis code goes here
   int Reset(PHCompositeNode *topNode); // called when new run opened (experimental)
   int ResetEvent(PHCompositeNode *topNode); // called at end of each event (for cleanup)
   void Print(const std::string&) const { return; }

 protected:
   int m_run;
   TH3D* m_h3_nhit[EmcAnaCommon::N_ARMSECT];
   TFile* m_file;
   TString m_ofilename;
   
};

#endif // NHITTREE_H__
