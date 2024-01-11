#include "emcTowerContentv3.h"
#include <iostream>
#include <iomanip>
#include "EmcIndexer.h"
#include "emcDataError.h"

ClassImp(emcTowerContentv3)

  using namespace std;

//_____________________________________________________________________________
emcTowerContentv3::emcTowerContentv3() : emcTowerContent()
{
  Reset();
}

//_____________________________________________________________________________
emcTowerContentv3::emcTowerContentv3(const emcTowerContentv3& o)
  : emcTowerContent()
{
  o.copyTo(*this);
}

//_____________________________________________________________________________
emcTowerContentv3&
emcTowerContentv3::operator=(const emcTowerContentv3& o)
{
  if ( this != &o )
    {
      o.copyTo(*this);
    }
  return *this;
}

//_____________________________________________________________________________
emcTowerContentv3::~emcTowerContentv3()
{
}

//_____________________________________________________________________________
void
emcTowerContentv3::copyTo(emcTowerContentv3& dest) const
{
  dest.fHasCalib=fHasCalib;
  dest.fHasDC=fHasDC;
  dest.fHasRaw=fHasRaw;
  dest.fFEM=fFEM;
  dest.fChannel=fChannel;
  dest.fDataError=fDataError;
  dest.fErrorNeighbours=fErrorNeighbours;
  dest.fWarnNeighbours=fWarnNeighbours;
  dest.fHGPost=fHGPost;
  dest.fHGPre=fHGPre;
  dest.fLGPost=fLGPost;
  dest.fLGPre=fLGPre;
  dest.fTAC=fTAC;
  dest.fTDC=fTDC;
  dest.fADC=fADC;
  dest.fHG=fHG;
  dest.fLG=fLG;
  dest.fTowerID=fTowerID;
  dest.fBeamClock=fBeamClock;
  dest.fAMUPre=fAMUPre;
  dest.fAMUPost=fAMUPost;
  dest.fAMUTAC=fAMUTAC;
  dest.fEnergy=fEnergy;
  dest.fTOF=fTOF;
  dest.fGain=fGain;
  dest.fSimFrac=fSimFrac;
  dest.fUncorrectedTOF=fUncorrectedTOF;
}

//_____________________________________________________________________________
void
emcTowerContentv3::identify(ostream& os) const
{
  os << "emcTowerContentv3::identify" << endl;
}

//_____________________________________________________________________________
int
emcTowerContentv3::isValid(void) const
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
emcTowerContentv3::isZero(void) const
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
emcTowerContentv3::print(ostream& out, int level) const
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
	  << " Gain " << Gain();
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
emcTowerContentv3::Reset()
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
  
  fGain=0;

  Zero();
}

//_____________________________________________________________________________
void 
emcTowerContentv3::SetADCTDC(int adc, int tdc, int hg, int lg)
{
  fADC=adc;
  fTDC=tdc;
  fHG=hg;
  fLG=lg;
  fHasDC=true;
}

//_____________________________________________________________________________
void 
emcTowerContentv3::SetCalibrated(float energy, float tof)
{
  fEnergy = energy;
  fTOF=fUncorrectedTOF=tof;
  fHasCalib=true;
}

//_____________________________________________________________________________
void
emcTowerContentv3::SetToF(float tof)
{
  fTOF=tof;
}

//_____________________________________________________________________________
void 
emcTowerContentv3::SetDataError(int dataerror)
{
  fDataError = dataerror;
}

//_____________________________________________________________________________
void
emcTowerContentv3::SetGain(float gain)
{
  fGain = gain;
}

//_____________________________________________________________________________
void
emcTowerContentv3::SetID(int fem, int channel)
{
  fFEM = fem;
  fChannel = channel;
  fTowerID = EmcIndexer::PXSM144iCH_iPX(fFEM,channel);
}

//_____________________________________________________________________________
void 
emcTowerContentv3::SetNeighbours(unsigned int error, unsigned int warning)
{
  fErrorNeighbours = error;
  fWarnNeighbours = warning;
}

//_____________________________________________________________________________
void 
emcTowerContentv3::SetRaw(int hgpost, int hgpre, int lgpost, int lgpre,
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
emcTowerContentv3::SetSimFrac(float simfrac)
{
  fSimFrac = simfrac;
}

//_____________________________________________________________________________
void
emcTowerContentv3::Zero(void)
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
  fUncorrectedTOF = 0.0;
  fGain = 0.0;

  fDataError = 0;
  fErrorNeighbours = 0;
  fWarnNeighbours = 0;

  fSimFrac = 0;
}
