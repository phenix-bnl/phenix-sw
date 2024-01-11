#include "ZdcOut.h"
#include <iostream>

ClassImp(ZdcOut);

using namespace std;

void ZdcOut::FillFromClass(const ZdcOut &old)
{
  //copied from offline/packages/framework/udst/zdcfuncs.C
  Reset();
  for(int iarm = 0; iarm < 2; iarm++)
  {
    AddZdcNS(old.get_Energy(iarm),
             old.get_Timing(iarm),iarm);
  }
  for(int ipmt = 0; ipmt < 8; ipmt++)
  {
    AddZdcHit(old.get_Charge(ipmt),
             old.get_Time0(ipmt),
              old.get_Time1(ipmt),ipmt);
  }
  set_TimeVertex(
                 old.get_TimeZero(),
                 old.get_TimeZeroError(),
                 old.get_Zvertex(),
                 old.get_ZvertexError()
                 );
}

float ZdcOut::get_AnalogSum(const int arm) const
{
  if ( arm==Zdc::South ) return get_Charge(0);
  else if ( arm==Zdc::North ) return get_Charge(4);
  else
    {
      cout << PHWHERE << "arm " << arm << " is not valid" << endl;
      return INVALID_FLOAT;
    }
}

float ZdcOut::get_DigitalSum(const int arm) const
{
  int begin_ch = 0;
  int end_ch = 0;
  
  //-*** get channel ranges for south and north PMTs
  if ( arm==Zdc::South )
    {
      begin_ch = 1;
      end_ch = 3;
    }
  else if ( arm==Zdc::North )
    {
      begin_ch = 5;
      end_ch = 7;
    }
  else
    {
      cout << PHWHERE << "arm " << arm << " is not valid" << endl;
      return INVALID_FLOAT;
    }

  //-*** now calculate digital sum of three channels
  float digsum = 0.;
  int   nhits = 0;
  for (int ich=begin_ch; ich<=end_ch; ich++)
    {
      float chtemp = get_Charge( ich );
      if ( chtemp > (INVALID_FLOAT+1.0) )
        {
          digsum += chtemp;
          nhits++;
        }
    }

  if ( nhits == 0 ) return INVALID_FLOAT;

  return digsum;
}

