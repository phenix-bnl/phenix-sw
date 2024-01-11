#include "emcTowerContentv1S.h"
#include <iostream>
#include <iomanip>
#include "EmcIndexer.h"
#include "emcDataError.h"

ClassImp(emcTowerContentv1S)

  using namespace std;

float emcTowerContentv1S::fEnergyThreshold = 0.0001; // 0.1 MeV

//_____________________________________________________________________________
  emcTowerContentv1S::emcTowerContentv1S()
{
  Reset();
}

//_____________________________________________________________________________
emcTowerContentv1S::emcTowerContentv1S(const emcTowerContentv1S& o)
  : emcTowerContent()
{
  o.copyTo(*this);
}

//_____________________________________________________________________________
emcTowerContentv1S&
emcTowerContentv1S::operator=(const emcTowerContentv1S& o)
{
  if ( this != &o )
    {
      o.copyTo(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContentv1S::~emcTowerContentv1S()
{
}

//_____________________________________________________________________________
void
emcTowerContentv1S::copyTo(emcTowerContentv1S& dest) const
{
  dest.fHasCalib=fHasCalib;
  dest.fHasDC=fHasDC;
  dest.fFEM=fFEM;
  dest.fChannel=fChannel;
  dest.fErrorNeighbours=fErrorNeighbours;
  dest.fWarnNeighbours=fWarnNeighbours;
  dest.fTDC=fTDC;
  dest.fADC=fADC;
  dest.fTowerID=fTowerID;
  dest.fEnergy=fEnergy;
  dest.fTOF=fTOF;
  dest.fUncorrectedTOF=fUncorrectedTOF;
}

//_____________________________________________________________________________
void
emcTowerContentv1S::identify(ostream& os) const
{
  os << "emcTowerContentv1S::identify" << endl;
}

//_____________________________________________________________________________
int
emcTowerContentv1S::isValid(void) const
{
  if (fFEM>=0)
    {
      return 1;
    }
  else
    {
      return 0;
    }
}

//_____________________________________________________________________________
bool
emcTowerContentv1S::isZero(void) const
{
  return fEnergy <= fEnergyThreshold;
}

//_____________________________________________________________________________
void
emcTowerContentv1S::print(ostream& out, int /*level*/) const
{
  std::ostream::fmtflags oldflags = out.flags();

  int arm,sector,iy,iz;

  int iS,iST;
  EmcIndexer::iPXiSiST(TowerID(),iS,iST);
  EmcIndexer::iSTxyST(iS,iST,iz,iy);

  EmcIndexer::sectorOnlineToOffline(iS,arm,sector);

  out << "FEM" << setw(3) << dec << FEM()
      << " CH" << setw(4) << dec << Channel()
      << " TID " << setw(7) << TowerID() 
      << " ARM " << setw(2) << arm
      << " SEC " << setw(2) << sector
      << " Y   " << setw(3) << iy
      << " z   " << setw(3) << iz;
  if ( hasDC() )
    {
      out << " ADC " << setw(7) << ADC() 
	  << " TDC " << setw(6) << TDC();
      out.flags(oldflags);
    }
  if ( hasCalib() ) 
    {
      out << " E   ";
      out.setf(ios::scientific);
      out.precision(3);
      out << Energy()
	  << " TOF " << ToF()
	  << endl;
      out.flags(oldflags);
    }

  out   << " ERN " << hex << setw(5) << "0x" << ErrorNeighbours()
       << " WRN " << hex << setw(5) << "0x" << WarnNeighbours()
       << dec;

  out << endl;

  out.flags(oldflags);
}

//_____________________________________________________________________________
void
emcTowerContentv1S::Reset()
{
  fFEM=-1;
  fChannel=-1;
  fTowerID=-1;
 
  fHasCalib = false;
  fHasDC = false;
  
  Zero();
}

//_____________________________________________________________________________
void 
emcTowerContentv1S::SetADCTDC(int adc, int tdc, int, int)
{
  fADC=adc;
  fTDC=tdc;
  fHasDC=true;
}

//_____________________________________________________________________________
void 
emcTowerContentv1S::SetCalibrated(float energy, float tof)
{
  fEnergy = energy;
  fTOF=fUncorrectedTOF=tof;
  fHasCalib=true;
}

//_____________________________________________________________________________
void
emcTowerContentv1S::SetToF(float tof)
{
  fTOF=tof;
}

//_____________________________________________________________________________
void
emcTowerContentv1S::SetID(int fem, int channel)
{
  fFEM = fem;
  fChannel = channel;
  fTowerID = EmcIndexer::PXSM144iCH_iPX(fFEM,channel);
}

//_____________________________________________________________________________
void 
emcTowerContentv1S::SetNeighbours(unsigned int error, unsigned int warning)
{
  fErrorNeighbours = error;
  fWarnNeighbours = warning;
}

//_____________________________________________________________________________
void
emcTowerContentv1S::Zero(void)
{
  fADC = 0;
  fTDC = 0;

  fEnergy = 0.0;
  fTOF = 0.0;
  fUncorrectedTOF = 0.0;

  fErrorNeighbours = 0;
  fWarnNeighbours = 0;
}
