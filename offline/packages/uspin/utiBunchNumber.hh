#ifndef UTIBUNCHNUMBER_H
#define UTIBUNCHNUMBER_H
///////////////////////////////////////////////////////////////////////
//
// utiBunchNumber class
//
// Author:  F. Messer, BNL
// 
// Description:  
//  class that return the official bunch number (SpinCollaboration) 

//  return if yellow or blue is good using the official bunch number (getYellowBunchGood()) 
//  NOTE: this has to be called after the getBunchNumber

//  When: 10/29/02  
//  
//
//////////////////////////////////////////////////////////////////////
#include "utiRunToFill.hh"

#include <PHString.h>

#include <string>

class utiBunchNumber {


 public:

  utiBunchNumber();
  virtual ~utiBunchNumber() {}

  static const int NBUNCHES = 120;

  void setBunchSelectionFile(const char* file) { bunchfile = file;}
  int  getBunchNumber(const int run, const int clockcross);
  int  getYellowBunchFlag() const { return yellowGood;}
  int  getBlueBunchFlag() const { return blueGood;}

  int  numberOfBunches() const { return NBUNCHES;}

private:

  void LoadBunchSelection(const int fill); 
  int getYellowBunchFlag(const int fill);  //uses the official bunch number 
  int getBlueBunchFlag(const int fill);    //uses the official bunch number 


  utiRunToFill utiFill;

  int original;
  int official;
  
  int oldrun;
  int oldfill;
  int newfill;
  
  std::string bunchfile;
  int  blue_bsel[NBUNCHES];
  int  yell_bsel[NBUNCHES];

  int blueGood;
  int yellowGood;

}; 

#endif /* UTIBUNCHNUMBER_H */












