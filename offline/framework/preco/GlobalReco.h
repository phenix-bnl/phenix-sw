#ifndef __GLOBALRECO_H__
#define __GLOBALRECO_H__

#include "SubsysReco.h"
#include <iostream>

class PHCompositeNode;

class GlobalReco: public SubsysReco
{
 public:

  //! constructor
  /*! 
  Default is version (v=1) for Run7 production
  version      Run
  0         Run5 Cu+Cu,  Run6 p+p
  1         Run7 Au+Au
  */
  GlobalReco(const int v = 1, const std::string &name = "GLOBAL" );

  //! destructor
  virtual ~GlobalReco() {}

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

 protected:

  //! version control for PHGlobal
  const int version; 
  
  //! separate timing information for south and north global detectors
  bool TimingSN;     

  //! common variables
  int fillCommon(PHCompositeNode *topNode);
  
  //! version specific variables
  int fillTimingSN(PHCompositeNode* topNode);

};

#endif
