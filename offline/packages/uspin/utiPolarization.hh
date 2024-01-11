#ifndef UTIPOLARIZATION_H
#define UTIPOLARIZATION_H
///////////////////////////////////////////////////////////////////////
//
// utiPolarization class
//
// Author:  F. Messer, BNL
// 
// Description:  
//  class that return the polarization of the two beams 
//
// NOTE: this may be year dependent (bit may change year to year)
//
// When: 10/29/02  
//
//
//////////////////////////////////////////////////////////////////////
#include "utiBunchNumber.hh"

class TrigLvl1;

class utiPolarization {
  
public:

  utiPolarization(TrigLvl1 *lvl1, int run0x);
  utiPolarization(int run0x = 2);
  virtual ~utiPolarization(){}
  
  void loadDefaultForYear(int run0x = 2); // bit may be different year to year
  void setBunchSelectionFile(char* file) { utiBunch.setBunchSelectionFile(file);} // for utiBunch

  void set_verbose(bool val) { verbose = val;}
  void set_ppData(bool val)  { ppData = val; }
  bool get_ppData()          { return ppData; }

  void setTrigLvl1(TrigLvl1* val) { triglvl1 = val; loadInfo();}

  int  getBluePolarization()  ; 
  int  getYellowPolarization(); 

  unsigned int getLvl1_rbits_bit_BU() {return lvl1_rbits_bit_BU;}
  unsigned int getLvl1_rbits_bit_BD() {return lvl1_rbits_bit_BD;}
  unsigned int getLvl1_rbits_bit_BZ() {return lvl1_rbits_bit_BZ;}
  unsigned int getLvl1_rbits_bit_YU() {return lvl1_rbits_bit_YU;}
  unsigned int getLvl1_rbits_bit_YD() {return lvl1_rbits_bit_YD;}
  unsigned int getLvl1_rbits_bit_YZ() {return lvl1_rbits_bit_YZ;}

  unsigned int getLvl1_trigscaled()  {return lvl1_trigscaled;}
  unsigned int getLvl1_triglive()    {return lvl1_triglive;}
  unsigned int getLvl1_trigraw()     {return lvl1_trigraw;}
  unsigned int getLvl1_clock_cross() {return lvl1_clock_cross;}

  unsigned int getLvl1_rbits(int i) { return lvl1_rbits[i];}

  // require the utiBunch ---------- SET UP
  int    printTotalErrorPolarization();
  int    getOfficialBunchNumber(int run);
  int    getBlueBunchFlag();   // uses the official bunch  (call getOfficialBunchNumber BEFORE)
  int    getYellowBunchFlag(); // uses the official bunch (call getOfficialBunchNumber BEFORE)
  //-------------------------------------



private:
  void loadInfo();
   
private:


  int totalErrorBlue;
  int totalErrorYellow;
  int ppData;
  int verbose;
  TrigLvl1   *triglvl1;
  utiBunchNumber utiBunch;

  //----------------------------------------
  int           bluePolarization;
  int           yellowPolarization;

  int           blueUp;
  int           blueDown;
  int           blueZero;
  int           yellowUp;
  int           yellowDown;
  int           yellowZero;
  
  unsigned int          lvl1_trigscaled;
  unsigned int          lvl1_triglive;
  unsigned int          lvl1_trigraw;
  unsigned int          lvl1_clock_cross;
  unsigned int          lvl1_rbits[5];
  unsigned int          lvl1_rbits_bit_BU;
  unsigned int          lvl1_rbits_bit_BD;
  unsigned int          lvl1_rbits_bit_BZ;
  unsigned int          lvl1_rbits_bit_YU;
  unsigned int          lvl1_rbits_bit_YD;
  unsigned int          lvl1_rbits_bit_YZ;
  //-----------------------------------

}; 

#endif /* UTIPOLARIZATION_H */







