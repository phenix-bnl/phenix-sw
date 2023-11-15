#include "PdbEmcTowerReject.hh"
#include <iostream>
#include <iomanip>

using namespace std;

//_____________________________________________________________________________
PdbEmcTowerReject::PdbEmcTowerReject()
  : fTowerId(-1),
    fAmplitudeError(0),
    fAmplitudeWarning(0),
    fTimingError(0),
    fTimingWarning(0)
{
}

//_____________________________________________________________________________
void
PdbEmcTowerReject::print() const
{
  std::ostream::fmtflags oldflags = cout.flags();

  std::cout << "TOWERID=" << setw(6) << fTowerId
	    << " AMP ERROR=" << setw(12) << hex << fAmplitudeError
	    << " AMP WARN =" << setw(12) << hex << fAmplitudeWarning
	    << " TOF ERROR=" << setw(12) << hex << fTimingError
	    << " TOF WARN =" << setw(12) << hex << fTimingWarning
	    << std::endl;
  cout.setf(oldflags);
}

//_____________________________________________________________________________
void
PdbEmcTowerReject::set(int towerid, int amp_error, int amp_warning, 
		       int timing_error, int timing_warning)
{
  fTowerId=towerid;
  fAmplitudeError=amp_error;
  fAmplitudeWarning=amp_warning;
  fTimingError=timing_error;
  fTimingWarning=timing_warning;
}
