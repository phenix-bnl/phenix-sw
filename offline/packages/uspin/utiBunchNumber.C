/****************************************************************************
 ****************************************************************************

 utiBunchNumber
 -----------

 DESCRIPTION: Converts the clockcross value written by PHENIX to the bunch number as 
              defined by the Spin Collaboration. The run number is needed because 
              the number to sum is different every year.


 AUTHOR/CONTACT: F. Messer, BNL

 When : 10/29/02

 REVISIONS:
       Date            Author          Description


 INPUT VARIABLES: run, clockcross

 OUTPUT VARIABLES: bunch number

 ***************************************************************************
 ***************************************************************************/

#include "utiBunchNumber.hh"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;

utiBunchNumber::utiBunchNumber():
  original(-1),
  official(-1),
  oldrun(-1),
  oldfill(-1),
  newfill(-1),
  blueGood(-1),
  yellowGood(-1)
{
  fill(blue_bsel,blue_bsel+sizeof(blue_bsel)/sizeof(int),-1);
  fill(yell_bsel,yell_bsel+sizeof(yell_bsel)/sizeof(int),-1);
}

int utiBunchNumber::getBunchNumber(const int run, int const clockcross)
{
  original = clockcross;
  if (oldrun != run) {
    newfill  = utiFill.getFillNumber(run);
    oldrun   = run;
  }
  official = -9999;

  if (run>=39000 && run <=40700) {            // run02   pp data
    official = ((clockcross + 5)%NBUNCHES);   // even if only 60 bunches are used in run02
  }

  getYellowBunchFlag(newfill);
  getBlueBunchFlag(newfill);

  if (official < 0 || official>= NBUNCHES) {
    cout << "utiBunchNumber:: Bunch Number Out of range"<< endl;
  }

  return official;
}


int utiBunchNumber::getYellowBunchFlag(const int fill)
{

  LoadBunchSelection(fill);  // load the info for the fill
  if (official >=0 && official < NBUNCHES) {  // if bunch number in range
    yellowGood = yell_bsel[official];
    return yellowGood;
  }else{
    cout << "Bunch Number (YELLOW GOOD) out of range "<< endl;
    return -9999;
  } 
}

int utiBunchNumber::getBlueBunchFlag(const int fill)
{
  
  LoadBunchSelection(fill);  // load the info for the fill
   if (official >=0 && official < NBUNCHES) {  // if bunch number in range
     blueGood = blue_bsel[official];
     return blueGood;
   }else{
     cout << "Bunch Number (BLUE GOOD) out of range "<< endl;
     return -9999;
   } 
    
}

void utiBunchNumber::LoadBunchSelection(int const fillval)
{
  if (oldfill == fillval) {
    return;                     // if same fill info then exit
  }else {
    oldfill = fillval;          // set the newfill to be the oldfill and then fill arrays
  }

  if (bunchfile.empty()) {
    cout << "BunchSelection File not yet initialized"<< endl;
    cout << "please use the command : setBunchSelectionFile(char*) "<< endl;
    return ;
  }

  ifstream file;
  file.open(bunchfile.c_str());
  if (!file) {
    cout << " could not open input Bunch Selection file   !!" << endl;
    return ;
  }

  int i=0;
  float f;
  int cnirun,ring,ndisc;
  
  
  int bflag[NBUNCHES];
  fill(blue_bsel,blue_bsel+sizeof(blue_bsel)/sizeof(int),-1);
  fill(yell_bsel,yell_bsel+sizeof(yell_bsel)/sizeof(int),-1);
  fill(bflag,bflag+sizeof(bflag)/sizeof(int),-1);

  while (!file.eof()) {
    file >> f >> cnirun >> ring >> ndisc >> bflag[0] >> 
            bflag[2]  >> bflag[4]  >> bflag[6]  >> bflag[8]  >> bflag[10]  >>
            bflag[12] >> bflag[14] >> bflag[16] >> bflag[18] >> bflag[20] >> 
            bflag[22] >> bflag[24] >> bflag[26] >> bflag[28] >> bflag[30] >> 
            bflag[32] >> bflag[34] >> bflag[36] >> bflag[38] >> bflag[40] >>
            bflag[42] >> bflag[44] >> bflag[46] >> bflag[48] >> bflag[50] >> 
            bflag[52] >> bflag[54] >> bflag[56] >> bflag[58] >> bflag[60] >> 
            bflag[62] >> bflag[64] >> bflag[66] >> bflag[68] >> bflag[70] >>
            bflag[72] >> bflag[74] >> bflag[76] >> bflag[78] >> bflag[80] >> 
            bflag[82] >> bflag[84] >> bflag[86] >> bflag[88] >> bflag[90] >>
            bflag[92] >> bflag[94] >> bflag[96] >> bflag[98] >> bflag[100] >> 
            bflag[102] >> bflag[104] >> bflag[106] >> bflag[108] >> bflag[110] >>
            bflag[112] >> bflag[114] >> bflag[116] >> bflag[118];

    int tmpfill = (int) f;
    
    if (tmpfill == fillval) {
      cout << "LoadBunchSelection():: Wanted fill found " << endl;
      if (ring == 1) { // blue
	cout << "blue ring " << endl;
	for (i =0; i < NBUNCHES; i++) blue_bsel[i] = bflag[i];
      }else if (ring ==2) { // yellow
	cout << "yellow ring "<< endl;
	for (i =0; i < NBUNCHES; i++) yell_bsel[i] = bflag[i];
      }
    }else {
      //cout << " fill in list is " << f << endl;
    }// found fill

  }// while

}
