#include <iostream>
#include "ZdcDCMv1.h"
#include <iostream>
//INCLUDECHECKER: Removed this line: #include "ZdcReturncodes.h"

using namespace std;

ClassImp(ZdcDCMv1)

ZdcDCMv1::ZdcDCMv1()
{
  Reset();
}

void ZdcDCMv1::identify(ostream& out) const
{
  out << "identify yourself: I am a ZdcDCMv1 object" << endl;
}


short ZdcDCMv1::set_DCM(short iword, long ival)
{
  if (iword>32 || iword < 0)
    {
      cout << "ZdcDCMv1::set_DCM bad index: " << iword << endl;
      return(INVALID_SHORT);
    }
  DCM[iword]=ival;
  return(SUCCESS_SHORT);
}

short ZdcDCMv1::set_DCM(long *ival)
{
  int i;
  for(i=0;i<33;i++)  DCM[i]=*ival++;
  return(SUCCESS_SHORT);
}

void ZdcDCMv1::Reset()
{
  int i;
  nWord = 0;
  scheme = INVALID_LONG;
  packetID = INVALID_LONG;
  for (i=0;i<33;i++)
    DCM[i]=INVALID_LONG;
}

