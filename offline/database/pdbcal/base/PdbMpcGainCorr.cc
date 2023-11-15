//
// $Id: PdbMpcGainCorr.cc,v 1.5 2015/12/11 01:12:51 pinkenbu Exp $
//

#include <PdbMpcGainCorr.hh>
#include <phool.h>
#include <iomanip>

using namespace std;

PdbMpcGainCorr::PdbMpcGainCorr()
{
  reset();
}

PdbMpcGainCorr& PdbMpcGainCorr::operator=(const PdbMpcGainCorr& p)
{
  version = p.getVersion();
  for (UInt_t i = 0; i<11; i++)
    {
      par[i] = p.getPar(i);
    }
  chi2 = p.getChi2();

  return *this;
}

float PdbMpcGainCorr::getGainCorr(const time_t meantime) const
{
  if ( version == 1 )
    {
      double time_in_days = meantime/86400.;	// convert to days
      double t = time_in_days - par[2];
      float gc = par[0] + par[1]*t + par[3]*t*t;
      return gc;
    }
  static int counter = 0;
  if ( counter++ < 10 )
    {
      cout << PHWHERE << " ERROR MpcGainCorr has invalid version " << version << endl;
      print();
    }
  return 1.;
}

void PdbMpcGainCorr::reset()
{
  version = -1;
  for (UInt_t i=0; i<11; i++)
    {
      par[i] = 0.;
    }
  chi2 = -1.;
}

void PdbMpcGainCorr::print() const
{
  cout << "MpcGainCorr v." << version << " chi2/ndf " << chi2;
  cout.precision(5);
  for (UInt_t i=0; i<11; i++)
    {
       cout << setw(10) << scientific << par[i];
    }
  cout << endl;
}

