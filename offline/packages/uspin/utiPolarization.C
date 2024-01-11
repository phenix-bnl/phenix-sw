///////////////////////////////////////////////////////////////////////
//
// utiPolarization class
//
// Author:  F.Messer, BNL
//
// Description: see .hh file
//
//////////////////////////////////////////////////////////////////////


#include "utiPolarization.hh"
#include "TrigLvl1.h"

#include <cmath>
using namespace std;

utiPolarization::utiPolarization(TrigLvl1* lvl1, int run0x)
{
  ppData = 1;
  triglvl1 = lvl1;
  verbose = 0;
  totalErrorBlue = 0;
  totalErrorYellow = 0;
  loadDefaultForYear(run0x);  // set the bits for the year in question
  loadInfo();

}

utiPolarization::utiPolarization(int run0x)
{
  ppData = 1;
  verbose = 0;
  totalErrorBlue = 0;
  totalErrorYellow = 0;
  loadDefaultForYear(run0x);  // set the bits for the year in question
}

void utiPolarization::loadDefaultForYear(int run0X)
{
  // the bit to extract the polarization values my change from year to year

  if (run0X == 2 && ppData)
    {  // pp DATA - run 02 (runs: 39000 to 40700)
      yellowUp = 54;
      yellowDown = 56;
      yellowZero = 58;
      blueUp = 55;
      blueDown = 57;
      blueZero = 59;
      //      cout << "-------------------------------------------------------------------" << endl;
      //      cout << "Default values for Polarization Bit for Year "<< run0X << " loaded " << endl;
      //      cout << "Blue Up   :" << blueUp   << " Yellow Up   :" << yellowUp   << endl;
      //      cout << "Blue Down :" << blueDown << " Yellow Down :" << yellowDown << endl;
      //      cout << "Blue Zero :" << blueZero << " Yellow Zero :" << yellowZero << endl;
      //      cout << "-------------------------------------------------------------------" << endl;
    }
  else
    {
      yellowUp = 0;
      yellowDown = 0;
      yellowZero = 0;
      blueUp = 0;
      blueDown = 0;
      blueZero = 0;

      cout << "utiPolarization:: Bad initialization...Check Year or ppFlag " << endl;
      //      cout << "----------------------------------------------------------------------" << endl;
      //      cout << "Default values for Polarization Bit for Year "<< run0X << " NOT loaded " << endl;
      //      cout << "Blue Up   :" << blueUp   << " Yellow Up   :" << yellowUp   << endl;
      //      cout << "Blue Down :" << blueDown << " Yellow Down :" << yellowDown << endl;
      //      cout << "Blue Zero :" << blueZero << " Yellow Zero :" << yellowZero << endl;
      //      cout << "-----------------------------------------------------------------------" << endl;
    }

}

int utiPolarization::getOfficialBunchNumber(int run)
{
  if (triglvl1)
    {
      return utiBunch.getBunchNumber(run, lvl1_clock_cross);
    }
  else
    {
      cout << "TrigLvl1 pointer is bad: can not extract Bunch Number " << endl;
      return -9999;
    }
}

int utiPolarization::getBlueBunchFlag()
{
  if (triglvl1)
    {
      return utiBunch.getBlueBunchFlag();
    }
  else
    {
      cout << "TrigLvl1 pointer is bad: can not extract Good Bunch Flag  " << endl;
      return -9999;
    }
}
int utiPolarization::getYellowBunchFlag()
{

  if (triglvl1)
    {
      return utiBunch.getYellowBunchFlag();
    }
  else
    {
      cout << "TrigLvl1 pointer is bad: can not extract Good Bunch Flag  " << endl;
      return -9999;
    }

}

void utiPolarization::loadInfo()
{

  //---------------------------------------------------------------------------
  // extract the information from the data structure (must be done each event)
  //---------------------------------------------------------------------------
  if (triglvl1)
    {
      lvl1_trigscaled = triglvl1->get_lvl1_trigscaled();
      lvl1_triglive = triglvl1->get_lvl1_triglive();
      lvl1_trigraw = triglvl1->get_lvl1_trigraw();
      lvl1_clock_cross = triglvl1->get_lvl1_clock_cross();
      for ( int s = 0; s < 5 ; s++)
        {
          lvl1_rbits[s] = triglvl1->get_lvl1_rbits(s);
        }
      lvl1_rbits_bit_BU = triglvl1->get_lvl1_rbits_bit(blueUp);
      lvl1_rbits_bit_YU = triglvl1->get_lvl1_rbits_bit(yellowUp);
      lvl1_rbits_bit_BD = triglvl1->get_lvl1_rbits_bit(blueDown);
      lvl1_rbits_bit_YD = triglvl1->get_lvl1_rbits_bit(yellowDown);
      lvl1_rbits_bit_BZ = triglvl1->get_lvl1_rbits_bit(blueZero);
      lvl1_rbits_bit_YZ = triglvl1->get_lvl1_rbits_bit(yellowZero);

    }
  else
    {

      cout << "TrigLvl1:: pointer not valid " << endl;
      return ;

    }

  //-------------------------------------------------------------------
  // determine from which bit is set the polarization for both beams
  // print an error message if more than one bit is set
  //-------------------------------------------------------------------

  if (lvl1_rbits_bit_BU && !lvl1_rbits_bit_BD && !lvl1_rbits_bit_BZ)
    {  // blue up
      bluePolarization = 1;
    }
  else if (!lvl1_rbits_bit_BU && lvl1_rbits_bit_BD && !lvl1_rbits_bit_BZ)
    { // blue down
      bluePolarization = -1;
    }
  else if (!lvl1_rbits_bit_BU && !lvl1_rbits_bit_BD && lvl1_rbits_bit_BZ)
    { // blue zero
      bluePolarization = 0;
    }
  else
    {
      bluePolarization = -9999;
      //cout << "Problems for the Blue Beam Polatization " << endl;
    }


  if (lvl1_rbits_bit_YU && !lvl1_rbits_bit_YD && !lvl1_rbits_bit_YZ)
    {  // yellow up
      yellowPolarization = 1;
    }
  else if (!lvl1_rbits_bit_YU && lvl1_rbits_bit_YD && !lvl1_rbits_bit_YZ)
    { // yellow down
      yellowPolarization = -1;
    }
  else if (!lvl1_rbits_bit_YU && !lvl1_rbits_bit_YD && lvl1_rbits_bit_YZ)
    { // yellow zero
      yellowPolarization = 0;
    }
  else
    {
      yellowPolarization = -9999;
      //cout << "Problems for the Yellow Beam Polatization " << endl;
    }


}
int utiPolarization::getYellowPolarization()
{
  // return the polarization, print a message if problems
  if (yellowPolarization < -100)
    {
      totalErrorYellow++;
      //  cout << "ATTENTION: Error in Yellow Beam Polarization " << endl;
      //  cout << "ATTENTION: Error in Yellow Beam Polarization " << endl;
      //  cout << "ATTENTION: Error in Yellow Beam Polarization " << endl;
    }
  return yellowPolarization;

}
int utiPolarization::getBluePolarization()
{
  // return the polarization, print a message if problems
  if (bluePolarization < -100)
    {
      totalErrorBlue++;
      //  cout << "ATTENTION: Error in Blue Beam Polarization " << endl;
      //  cout << "ATTENTION: Error in Blue Beam Polarization " << endl;
      //  cout << "ATTENTION: Error in Blue Beam Polarization " << endl;
      //
    }
  return bluePolarization;

}


int utiPolarization::printTotalErrorPolarization()
{
  cout << "--------------------------------" << endl;
  cout << "Summary for Polarization Errors " << endl;
  cout << "Blue   :" << totalErrorBlue << endl;
  cout << "Yellow :" << totalErrorYellow << endl;
  cout << "--------------------------------" << endl;

  return 1;
}
