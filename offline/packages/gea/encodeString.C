#include <cstdlib>
#include <iostream>

using namespace std;

void encodeString(char xxxPRDF[11], int iRow) 
{
  //
  // Assume iRow goes from 0 to 999 maximum
  //
  if(iRow< 0 || iRow>999) {
    cout << "\n encodeString <E>: bad iRow value " << iRow << endl;
    exit(1);
  }
  int iHundred = iRow/100;
  int iTen = (iRow - iHundred*100)/10;
  int iUnit = (iRow - iHundred*100 - iTen*10);
  xxxPRDF[9] = 48 + iUnit;
  xxxPRDF[8] = 48 + iTen;
  xxxPRDF[7] = 48 + iHundred;
  return;
}
