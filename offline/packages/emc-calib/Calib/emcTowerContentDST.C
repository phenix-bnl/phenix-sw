#include "emcTowerContentDST.h"
#include <iostream>
#include <iomanip>
#include "EmcIndexer.h"
#include "emcDataError.h"
#include <math.h>
ClassImp(emcTowerContentDST)

  using namespace std;

//_____________________________________________________________________________
emcTowerContentDST::emcTowerContentDST() : emcTowerContent()
{
  //    cout << __FILE__ << " " << __LINE__ << " in emcTowerContentDST::emcTowerContentDST()" << endl;
  Reset();
}

//_____________________________________________________________________________
emcTowerContentDST::emcTowerContentDST(const emcTowerContentDST& o)
  : emcTowerContent()
{
  // cout << __FILE__ << " " << __LINE__ << " in emcTowerContentDST::emcTowerContentDST()" << endl;
  o.copyTo(*this);
}

//_____________________________________________________________________________
emcTowerContentDST&
emcTowerContentDST::operator=(const emcTowerContentDST& o)
{
  if ( this != &o )
    {
      o.copyTo(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContentDST::~emcTowerContentDST()
{
}

//_____________________________________________________________________________
void
emcTowerContentDST::copyTo(emcTowerContentDST& dest) const
{
  dest.fErrorNeighbours=fErrorNeighbours;
  dest.fWarnNeighbours=fWarnNeighbours;
  dest.fTowerID=fTowerID;
  dest.fEnergy=fEnergy;
  dest.fTOF=fTOF;
  dest.fClusterID=fClusterID;
  dest.fADC=fADC;
  dest.fTDC=fTDC;

}

//_____________________________________________________________________________
void
emcTowerContentDST::identify(ostream& os) const
{
  os << "emcTowerContentDST::identify" << endl;
}

//_____________________________________________________________________________
void
emcTowerContentDST::print(ostream& out, int level) const
{
  std::ostream::fmtflags oldflags = out.flags();

  int arm,sector,iy,iz;

  int iS,iST;
  EmcIndexer::iPXiSiST(TowerID(),iS,iST);
  EmcIndexer::iSTxyST(iS,iST,iz,iy);

  EmcIndexer::sectorOnlineToOffline(iS,arm,sector);

  out << " TID " << setw(7) << TowerID() 
      << " ARM " << setw(2) << arm
      << " SEC " << setw(2) << sector
      << " Y   " << setw(3) << iy
      << " z   " << setw(3) << iz
      << " adc " << setw(3) << ADC();
  out << " ClusterID = " << fClusterID; 

  if ( hasCalib() ) 
    {
      out << " E   ";
      out.setf(ios::scientific);
      out.precision(3);
      out << Energy()
	  << " TOF " << ToF();
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
emcTowerContentDST::Reset()
{
  fTowerID=-1;
  Zero();
}


void emcTowerContentDST::SetCalibrated(float energy, float tof)
{
  fEnergy = energy;
  fTOF = tof;
  if ( isnan (fTOF) )
  {
    cout << __FILE__ << " " << __LINE__ << " nan :"  << fTOF << endl;
  }

}

//_____________________________________________________________________________
void
emcTowerContentDST::SetToF(float tof)
{
  fTOF=tof;
  if ( isnan (fTOF) )
  {
    cout << __FILE__ << " " << __LINE__ << " nan :"  << fTOF << endl;
  }
}


//_____________________________________________________________________________
void 
emcTowerContentDST::SetNeighbours(unsigned int error, unsigned int warning)
{
  fErrorNeighbours = error;
  fWarnNeighbours = warning;
}



//_____________________________________________________________________________
void
emcTowerContentDST::Zero(void)
{

  fEnergy = 0.0;
  fTOF = 0.0;

  fErrorNeighbours = 0;
  fWarnNeighbours = 0;

}
