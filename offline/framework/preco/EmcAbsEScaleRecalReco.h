// $Author: pinkenbu $
// $Date: 2011/12/05 16:36:11 $
// $Name:  $
// $Source: /afs/rhic.bnl.gov/phenix/PHENIX_CVS/offline/framework/preco/EmcAbsEScaleRecalReco.h,v $

// the base class SubsysReco provides the stl string variable "ThisName"
// and an int variable verbosity which can be set via
// Verbosity(const int ival);
// It can be used to e.g. configure printouts from the Analysis Module

#ifndef EMCABSESCALERECALRECO_H__
#define EMCABSESCALERECALRECO_H__

#include <SubsysReco.h>

class PHCompositeNode;

class EmcAbsEScaleRecalReco: public SubsysReco
{
 public:
  EmcAbsEScaleRecalReco(const char *name = "EMCABSESCALERECAL");
  virtual ~EmcAbsEScaleRecalReco() {}

  int End(PHCompositeNode *topNode); // called at EndRun
  int Init(PHCompositeNode *topNode); // Initialization at startup - create histos here
  int InitRun(PHCompositeNode *topNode);  // Initializations which need the run number
  int process_event(PHCompositeNode *topNode); // your analysis code goes here
  int Reset(PHCompositeNode *topNode); // called when new run opened (experimental)
  int ResetEvent(PHCompositeNode *topNode); // called at end of each event (for cleanup)
  void Print(const std::string&) const {}

  int isvalid;

 protected:
  int arm;
  int sector;

  int run;
  float get_correction( int arm, int sec, float ecore);

};

#endif /* __EMCABSESCALERECALRECO_H__ */

// $Log: EmcAbsEScaleRecalReco.h,v $
// Revision 1.3  2011/12/05 16:36:11  pinkenbu
// install includes in preco subdir
//
// Revision 1.2  2011/02/25 15:17:45  bbannier
// Correctly overload SubsysReco::Print
//
// Revision 1.1  2004/05/28 23:56:02  ptarjan
// Added EmcAbsEScaleRecalReco
//
