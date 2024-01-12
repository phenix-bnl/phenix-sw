#ifndef GlobalReco_muons_h
#define GlobalReco_muons_h

#include "SubsysReco.h"
#include <iostream>

class PHCompositeNode;

class GlobalReco_muons: public SubsysReco
{
 public:
  
  //! constructor
  GlobalReco_muons( const std::string &name = "GLOBALRECO_MUONS" );
  
  //! destructor
  virtual ~GlobalReco_muons() 
  {}

  //! initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);

  protected:

  //! fill muon variables
  int fillMuon(PHCompositeNode *topNode);

};

#endif
