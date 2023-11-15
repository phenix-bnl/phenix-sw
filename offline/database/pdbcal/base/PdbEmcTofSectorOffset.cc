#include "PdbEmcTofSectorOffset.hh"
#include <iostream>

ClassImp(PdbEmcTofSectorOffset)

//_____________________________________________________________________________
PdbEmcTofSectorOffset::PdbEmcTofSectorOffset()
    : PdbCalChan()
{
  fRunNumber=-1;
  fNevents=0;
  fPeak=fWidth=0.0;
  fGausPeak=fGausWidth=0.0;
  fBBCT0=fBBCT0rms=0.0;
  fTOFT0=fTOFT0rms=0.0;
}

//_____________________________________________________________________________
PdbEmcTofSectorOffset::~PdbEmcTofSectorOffset()
{
}

//_____________________________________________________________________________
void
PdbEmcTofSectorOffset::print() const
{
  std::cout << "Run " << runnumber() << " nevents=" << numberOfEvents()
	    << std::endl;
  std::cout << "Peak=" << peak()
	    << " Width=" << width()
	    << std::endl;
  std::cout << "GausPeak=" << gausPeak()
	    << " GausWidth=" << gausWidth()
	    << std::endl;
  std::cout << "BBC T0=" << BBCT0() << " rms=" << BBCT0rms() << std::endl;
  std::cout << "TOF T0=" << TOFT0() << " rms=" << TOFT0rms() << std::endl;
}

//_____________________________________________________________________________
void
PdbEmcTofSectorOffset::setPeak(float peak, float width)
{
  fPeak=peak;
  fWidth=width;
}

//_____________________________________________________________________________
void
PdbEmcTofSectorOffset::setGausPeak(float peak, float width)
{
  fGausPeak=peak;
  fGausWidth=width;
}

//_____________________________________________________________________________
void
PdbEmcTofSectorOffset::setProcessInfo(int runnumber, int numberOfEvents)
{
  fRunNumber = runnumber;
  fNevents = numberOfEvents;
}

//_____________________________________________________________________________
void
PdbEmcTofSectorOffset::setBBC(float t0, float rms)
{
  fBBCT0 = t0;
  fBBCT0rms = rms;
}

//_____________________________________________________________________________
void
PdbEmcTofSectorOffset::setTOF(float t0, float rms)
{
  fTOFT0 = t0;
  fTOFT0rms = rms;
}

