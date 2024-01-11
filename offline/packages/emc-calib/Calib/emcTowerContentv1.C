#include "emcTowerContentv1.h"
#include <iostream>
#include <iomanip>
#include "EmcIndexer.h"
#include "emcDataError.h"

ClassImp(emcTowerContentv1)

  using namespace std;

//_____________________________________________________________________________
  emcTowerContentv1::emcTowerContentv1()
{
  Reset();
}

//_____________________________________________________________________________
emcTowerContentv1::~emcTowerContentv1()
{
}

//_____________________________________________________________________________
void
emcTowerContentv1::identify(ostream& os) const
{
  os << "emcTowerContentv1::identify" << std::endl;
}

//_____________________________________________________________________________
int
emcTowerContentv1::isValid(void) const
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
emcTowerContentv1::isZero(void) const
{
 //  if ( fTAC != 4095 ||
//        fHGPost != 4095 ||
//        fLGPost != 4095 ||
//        fHGPre != 4095 ||
//        fLGPre != 4095 ) {
//     return false ;
//   }

  if ( fDataError & emcDataError::CHANNEL_DISABLED() )
    {
      return true;
    }
  else
    {
      return false;
    }
}

//_____________________________________________________________________________
void
emcTowerContentv1::print(ostream& out, int level) const
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
	  << " TOF " << ToF();
      out.flags(oldflags);
    }

  if ( level > 0 )
    {
      if ( hasRaw() )
	{
	  out << endl 
	      << "             "
	      << " HPre" << setw(6) << HGPre() 
	      << " HPos" << setw(6) << HGPost() 
	      << " LPre" << setw(6) << LGPre() 
	      << " LPos" << setw(6) << LGPost() 
	      << " TAC " << setw(6) << TAC() 
	      << " LG" << setw(6) << LG() 
	      << " HG " << setw(6) << HG() 
	      << endl
	      << "             "
	      << " APre" << setw(7) << AMUPre()
	      << " APos" << setw(6) << AMUPost()
	      << " ATAC" << setw(6) << AMUTAC()
	      << endl
	      << "             ";
	  out.flags(oldflags);
	}
      out  << " DER " << hex << setw(5) << "0x" << DataError();     
    }

  out   << " ERN " << hex << setw(5) << "0x" << ErrorNeighbours()
       << " WRN " << hex << setw(5) << "0x" << WarnNeighbours()
       << dec;

  out << endl;

  out.flags(oldflags);
}

//_____________________________________________________________________________
void
emcTowerContentv1::Reset()
{
  fFEM=-1;
  fChannel=-1;
  fTowerID=-1;
 
  fAMUPre = 0;
  fAMUPost = 0;
  fAMUTAC = 0;

  fHasCalib = false;
  fHasDC = false;
  fHasRaw = false;
  
  Zero();
}

//_____________________________________________________________________________
void 
emcTowerContentv1::SetADCTDC(int adc, int tdc, int hg, int lg)
{
  fADC=adc;
  fTDC=tdc;
  fHG=hg;
  fLG=lg;
  fHasDC=true;
}

//_____________________________________________________________________________
void 
emcTowerContentv1::SetCalibrated(float energy, float tof)
{
  fEnergy = energy;
  fTOF = tof;
  fHasCalib=true;
}

//_____________________________________________________________________________
void 
emcTowerContentv1::SetDataError(int dataerror)
{
  fDataError = dataerror;
}

//_____________________________________________________________________________
void
emcTowerContentv1::SetID(int fem, int channel)
{
  fFEM = fem;
  fChannel = channel;
  fTowerID = EmcIndexer::PXSM144iCH_iPX(fFEM,channel);
}

//_____________________________________________________________________________
void 
emcTowerContentv1::SetNeighbours(unsigned int error, unsigned int warning)
{
  fErrorNeighbours = error;
  fWarnNeighbours = warning;
}

//_____________________________________________________________________________
void 
emcTowerContentv1::SetRaw(int hgpost, int hgpre, int lgpost, int lgpre,
			  int tac,
			  int amupre, int amupost, int amutac, int beamclock)
{
  fHGPost = hgpost;
  fHGPre = hgpre;
  fLGPost = lgpost;
  fLGPre = lgpre;
  fTAC = tac;
  fAMUPre = amupre;
  fAMUPost = amupost;
  fAMUTAC = amutac;
  fBeamClock = beamclock;
  fHasRaw=true;
}

//_____________________________________________________________________________
void
emcTowerContentv1::Zero(void)
{
  fHGPost = 4095;
  fHGPre = 4095;
  fLGPost = 4095;
  fLGPre = 4095;
  fTAC = 4095;

  fHG = 0;
  fLG = 0;

  fADC = 0;
  fTDC = 0;

  fEnergy = 0.0;
  fTOF = 0.0;

  fDataError = 0;
  fErrorNeighbours = 0;
  fWarnNeighbours = 0;
}
