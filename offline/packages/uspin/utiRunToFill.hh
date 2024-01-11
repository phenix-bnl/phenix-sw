#ifndef UTIRUNTOFILL_H
#define UTIRUNTOFILL_H
///////////////////////////////////////////////////////////////////////
//
// utiRunToFill class
//
// Author:  F. Messer, BNL
// 
// Description:  
//  class that return the fill number given a run number  
//
// When: 10/29/02  
//  At present, it is ascii file based, later the database will be used
//
//
//////////////////////////////////////////////////////////////////////
#include "PHString.h"

class utiRunToFill {

 public:

  enum{
    kMaxERT = 2000
  };

  utiRunToFill(const char* file ="/phenix/workarea/federica/outgoing/fillVsRun_year02.txt");
  virtual ~utiRunToFill() {}

  int getFillNumber(int run);
 
  void set_verbose(bool val) { verbose = val;}

  void set_readFromFile(const char* file="/phenix/workarea/federica/outgoing/fillVsRun_year02.txt"); // temporary location 
  void set_readFromDB(); 

 private:

  
  int verbose;
  int readFromFile;
  int readFromDB;

  const char* filename;

}; 

#endif /* UTIRUNTOFILL_H */







