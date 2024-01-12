#ifndef GlobalReco_central_h
#define GlobalReco_central_h

#include "SubsysReco.h"
#include <iostream>

class PHCompositeNode;

class GlobalReco_central: public SubsysReco
{
 public:
  
  //! constructor
  GlobalReco_central( const std::string &name = "GLOBALRECO_CENTRAL" );
  
  //! destructor
  virtual ~GlobalReco_central() 
  {}

  //! initialization
  int InitRun(PHCompositeNode *topNode);
  
  //! event method
  int process_event(PHCompositeNode *topNode);

  protected:

  //! fill central variables
  int fillCentral(PHCompositeNode *topNode);

};

#endif
