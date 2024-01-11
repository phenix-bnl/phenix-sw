// the base class SubsysReco provides the stl string variable "ThisName"
// and an int variable verbosity which can be set via
// Verbosity(const int ival);
// It can be used to e.g. configure printouts from the NonLinTree Module

#ifndef __NONLINTREE_H__
#define __NONLINTREE_H__

#include "EmcAnaCommon.h"

#include <SubsysReco.h>

#include <string>

class PHCompositeNode;
class TH2;
class TFile;
class Warnmap;

class NonLinTree: public SubsysReco
{
   static const int N_BIN_MOM  = 40;
   static const double MOM_MIN =  0;
   static const double MOM_MAX = 10;

   static const int N_BIN_MASS  = 600;
   static const double MASS_MIN = 0;
   static const double MASS_MAX = 0.3;

   static const double ECORE_MIN = 0.3;
   static const double ENE_ASYM_MAX = 0.2;

   int      m_run;
   std::string   m_ofilename;
   Warnmap* m_warnmap;
   TFile*   m_file;
   TH2*     m_h2_pi0mass[EmcAnaCommon::N_ARMSECT]; //!

 public:
   NonLinTree(
      const int run, const char* ofilename, 
      const char* fn_warnmap, const char *name = "NONLINTREE" );
   virtual ~NonLinTree();
   
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);
      
};

#endif // __NONLINTREE_H__
