#include "emcCalFEM.h"
#include <iostream>
#include <cassert>

using std::ostream;
using std::string;
using std::endl;

int emcCalFEM::fgNemcCalFEM = 0 ;

//_____________________________________________________________________________
emcCalFEM::emcCalFEM(int absPosition)
  : emcManageable("emcCalFEM","Calibration data for one FEM (base class)",
		  "emcCalFEM"),
  fStart(0),fEnd(0),
  fAbsPosition(absPosition),  
  fXmin(0),fXmax(0),fDraft(false),fVersion(0),fDescription(""),
  fPinNumber(0),fDefault(false)
{
  fgNemcCalFEM++ ;
}

//_____________________________________________________________________________
emcCalFEM::emcCalFEM(int absPosition,
                     const PHTimeStamp& t1, const PHTimeStamp& t2)
  : emcManageable("emcCalFEM","Calibration data for one FEM (base class)",
		  "emcCalFEM"),
    fStart(t1),fEnd(t2),
    fAbsPosition(absPosition),
    fXmin(0),fXmax(0),fDraft(false),fVersion(0),fDescription(""),
    fPinNumber(0),fDefault(false)
{
  fgNemcCalFEM++ ;
}

//_____________________________________________________________________
emcCalFEM::emcCalFEM(const emcCalFEM& o) : emcManageable()
{
  o.Copy(*this) ;
  fgNemcCalFEM++ ;
}

//_____________________________________________________________________________
emcCalFEM&
emcCalFEM::operator=(const emcCalFEM& o)
{
  if ( this == &o ) return *this ;
  o.Copy(*this) ;
  return *this ;
}

//_____________________________________________________________________
void emcCalFEM::Copy(emcCalFEM& o) const
{
  o.fStart = fStart ;
  o.fEnd = fEnd ;
  o.fAbsPosition = fAbsPosition ;
  o.fXmin = fXmin ;
  o.fXmax = fXmax ;
  o.fDraft = fDraft ; 
  o.fVersion = fVersion ;
  o.fDescription = fDescription ;   
  o.fPinNumber = fPinNumber ;
  o.fDefault = fDefault ;
}

//_____________________________________________________________________________
emcCalFEM::~emcCalFEM()
{
  fgNemcCalFEM-- ;
}

//_____________________________________________________________________________
int emcCalFEM::FEMCode(int absPosition, int pinNumber, 
		       int /*post_pre*/, int /*tac_pre*/)
{
  // Make a single integer value from 4 characteristic values of a FEM.
  int code ;

  assert(absPosition>=0 && absPosition<200);
  assert(pinNumber>=0 && pinNumber<2048);

  code = ( ( absPosition ) & 0xFFF ) +
    ( ( pinNumber << 12 ) & 0xFFF000 );

  return code ;
}

//_____________________________________________________________________________
void emcCalFEM::FEMDecode(int code, int& absPosition, int& pinNumber, 
			    int& post_pre, int& tac_pre)
{
  absPosition = (code & 0xFFF) ;
  pinNumber = ( ( code & 0xFFF000 ) >> 12 ) ;
  post_pre = 0 ;
  tac_pre  = 0 ;
}

//_____________________________________________________________________________
bool emcCalFEM::IsValid(const PHTimeStamp& cwhen) const
{
  PHTimeStamp& when = const_cast<PHTimeStamp&>(cwhen) ;
  return when.isInRange(fStart,fEnd) ;
}

//_____________________________________________________________________________
ostream&
emcCalFEM::Print(ostream& out, int /*level*/) const
{
  out << string(50,'-') << endl ;
  out << " AbsPosition=" << fAbsPosition ;
  if (fPinNumber>0) {
    out << " PinNumber=" << fPinNumber ;
  }
  out << " tStart=" << fStart
       << " tEnd=" << fEnd << endl ;
  out << " Flavour = " << GetCategory() << " Nchannels=" << size() ;
  if (IsDefault()) {
    out << " (DEFAULT)" ;
  }
  out << endl ;
  out << string(50,'-') << endl ;
  return out ;
}
