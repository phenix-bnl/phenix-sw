//  Implementation of class PdbEmcTrackMatch
//  Author: Cesar

#include "PdbEmcTrackMatch.hh"
#include <iostream>

using namespace std;

PdbEmcTrackMatch::PdbEmcTrackMatch()
{
   zero();
}

void
PdbEmcTrackMatch::zero()
{
  memset(match,0,sizeof(match));
}

float 
PdbEmcTrackMatch::getParameter(const size_t ipar, const size_t iipar) const
{
  if (ipar<NPAR && iipar<NMOMPAR)
    {
      return match[ipar][iipar];
    }  
  return 0.0;
}

void 
PdbEmcTrackMatch::setParameter(const size_t ipar, const size_t iipar, const float val)
{
  if (ipar<NPAR && iipar<NMOMPAR)
    {
      match[ipar][iipar] = val;
    }
  else
    {
      cout << "PdbEmcTrackMatch::SetParameter - "
	   << " Parameter " << ipar
	   << " Mom. Parameter " << iipar
	   << " is out of range."  << endl;
    }

}

void 
PdbEmcTrackMatch::print() const
{
  for (int ipar=0; ipar<NPAR; ipar++)
    {
      for (int iipar=0; iipar<NMOMPAR; iipar++)
	{
  	  cout << match[ipar][iipar];
	}
      cout << " , ";
    }
  cout << endl;
}
