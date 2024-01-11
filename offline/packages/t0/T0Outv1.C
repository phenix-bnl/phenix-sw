#include "T0Outv1.h"
#include <iostream>

ClassImp(T0Outv1)

using namespace std;

T0Outv1::T0Outv1()
{
  Reset(); // Init all data members 
  return;
}

void T0Outv1::identify(ostream& out) const
{
  float t0, t0err;
  out << "identify yourself: I am a T0Outv1 object, list of Time Zero's:" << endl;
  if (T0List & BBCBIT)
    {
      out << "Bbc: T0 = " << BbcT0 << ", dT0 = " << BbcT0Err << endl;
    }
  if (T0List & ZDCBIT)
    {
      out << "Zdc: T0 = " << ZdcT0 << ", dT0 = " << ZdcT0Err << endl;
    }
  if (T0List & TZRBIT)
    {
      out << "Tzr T0 = " << TzrT0 << ", dT0 = " << TzrT0Err << endl;
    }
  if (T0List & NTCBIT)
    {
      out << "Ntc T0 = " << NtcT0 << ", dT0 = " << NtcT0Err << endl;
    }
  if (!T0List)
    {
      out << "No Time Zero found by any subsystem" << endl;
    }

  if (get_T0List())
    {
      t0 = get_T0();
      t0err = get_T0Error();
      cout << "T0 of choice:"; 
	switch(ChooseT0())
	  {
	    case BBCT0:
	      out << " BBC";
	      break;
	    case NTCT0:
	      out << " NTC";
	      break;
	    case TZRT0:
	      out << " TZR";
	      break;
	    case ZDCT0:
	      out << " ZDC";
	      break;
	  default:
	      out << "ERROR ChooseT0 returned funky value!!!!";
	      break;
	  }
      out << endl;
      out <<   "t0 = " << t0 << ", dt0 = " << t0err << endl;
    }
  return;
}

void T0Outv1::Reset()  // reset data members (called by phool node reset)
{
  T0List = 0;
  BbcT0 = T0_INVALID_FLOAT;
  BbcT0Err = T0_INVALID_FLOAT;
  NtcT0 = T0_INVALID_FLOAT;
  NtcT0Err = T0_INVALID_FLOAT;
  TzrT0 = T0_INVALID_FLOAT;
  TzrT0Err = T0_INVALID_FLOAT;
  ZdcT0 = T0_INVALID_FLOAT;
  ZdcT0Err = T0_INVALID_FLOAT;
  return;
}

float T0Outv1::get_T0() const
{
  int it0;
  it0 = ChooseT0();
  switch(it0)                 // and try to fill z here
    {
    case BBCT0:
      return BbcT0;
      break;
    case ZDCT0:
      return ZdcT0;
      break;
    case TZRT0:
      return TzrT0;
      break;
    case NTCT0:
      return NtcT0;
      break;
    default:
      break;
    }
   return T0_INVALID_FLOAT;
}

float T0Outv1::get_T0Error() const
{
  int it0;
  it0 = ChooseT0();
  switch(it0)
    {
    case BBCT0:
      return BbcT0Err;
      break;
    case NTCT0:
      return NtcT0Err;
      break;
    case TZRT0:
      return TzrT0Err;
      break;
    case ZDCT0:
      return ZdcT0Err;
      break;
    default:
      break;
    }
   return T0_INVALID_FLOAT;
}

/* 
here we set the order in which the T0's are used:
1. Bbc
2. Zdc
3. Tzr
4. Ntc
*/
int T0Outv1::ChooseT0() const
{
  if (T0List & BBCBIT)
    {
      return (BBCT0);
    }
  if (T0List & ZDCBIT)
    {
      return (ZDCT0);
    }
  if (T0List & TZRBIT)
    {
      return (TZRT0);
    }
  if (T0List & NTCBIT)
    {
      return (NTCT0);
    }
  return (T0_INVALID_INT);
}


int T0Outv1::set_BbcT0(const float t0, const float t0err)
{
  BbcT0 = t0;
  BbcT0Err = t0err;
  T0List |= BBCBIT;
  return (T0_SUCCESS_INT);
}

int T0Outv1::set_NtcT0(const float t0, const float t0err)
{
  NtcT0 = t0;
  NtcT0Err = t0err;
  T0List |= NTCBIT;
  return (T0_SUCCESS_INT);
}

int T0Outv1::set_TzrT0(const float t0, const float t0err)
{
  TzrT0 = t0;
  TzrT0Err = t0err;
  T0List |= TZRBIT;
  return (T0_SUCCESS_INT);
}

int T0Outv1::set_ZdcT0(const float t0, const float t0err)
{
  ZdcT0 = t0;
  ZdcT0Err = t0err;
  T0List |= ZDCBIT;
  return (T0_SUCCESS_INT);
}
