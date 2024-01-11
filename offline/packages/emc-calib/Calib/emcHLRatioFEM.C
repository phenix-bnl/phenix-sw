#include "emcHLRatioFEM.h"
#include "emcDefines.h"
#include "emcCalFEMFactory.h"
#include <iostream>

float emcHLRatioFEM::fGlobalAverage = 15.4 ;
float emcHLRatioFEM::fGlobalRMS = 1.7 ;

using namespace std;

namespace 
{
  static string name = "emcHLRatioFEM";
  static string title = "H/L ratio calibration data";
  static string classname = "emcHLRatioFEM";

  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcHLRatioFEM::Default(absPosition,start,end);
      }
    else
      {
	return new emcHLRatioFEM(absPosition,start,end);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("HLRatios",
						    creator);
}

//_____________________________________________________________________________
emcHLRatioFEM::emcHLRatioFEM(int absPosition)
  : emcCalFEM(absPosition)
{
  NameIt(name,title,classname);
}

//_____________________________________________________________________________
emcHLRatioFEM::emcHLRatioFEM(int absPosition, 
			     const PHTimeStamp& t1, const PHTimeStamp& t2)
  : emcCalFEM(absPosition,t1,t2)

{
  NameIt(name,title,classname);
}

//_____________________________________________________________________________
emcHLRatioFEM::emcHLRatioFEM(const emcHLRatioFEM& o)
  : emcCalFEM(o.AbsolutePosition())
{
  o.Copy(*this) ;  
}

//_____________________________________________________________________________
emcHLRatioFEM&
emcHLRatioFEM::operator=(const emcHLRatioFEM& o)
{
  if ( this == &o ) return *this ;
  Reset() ;
  o.Copy(*this) ;
  return *this ;
}

//_____________________________________________________________________________
void
emcHLRatioFEM::Copy(emcHLRatioFEM& o) const
{
  emcCalFEM::Copy(o) ;
  o.fHLRatioVector.resize(fHLRatioVector.size()) ;
  size_t i ;
  for ( i = 0 ; i < fHLRatioVector.size() ; i++) {
    o.fHLRatioVector[i] = fHLRatioVector[i] ;
  }
}

//_____________________________________________________________________________
emcHLRatioFEM::~emcHLRatioFEM()
{
  Reset() ;
}

//_____________________________________________________________________________
void emcHLRatioFEM::AppendOneChannel(float average, float rms, 
				   float intercept, float slope)
{
  Ratio tmp ;
  
  tmp.push_back(average) ;
  tmp.push_back(rms) ;
  tmp.push_back(intercept) ;
  tmp.push_back(slope) ;
  if (fHLRatioVector.size()<144) {
    fHLRatioVector.push_back(tmp) ;
  }
  else {
    cerr << "<E-EMCAL> emcHLRatioFEM::AppendOneChannel : fem is full" << endl ;
  }
}

//_____________________________________________________________________________
emcHLRatioFEM* 
emcHLRatioFEM::Default(const int& absPosition, 
                       const PHTimeStamp& t1, 
                       const PHTimeStamp& t2)
{
  emcHLRatioFEM* fem = new emcHLRatioFEM(absPosition,t1,t2) ;

  size_t i ;
  for ( i = 0 ; i < 144 ; i++ ) {
    fem->AppendOneChannel(fGlobalAverage,fGlobalRMS,0,16) ;
  }

  return fem ;
}


//_____________________________________________________________________________
float
emcHLRatioFEM::getValue(int ichannel, int what) const 
{
  if (what<0 || what>3) {

    cerr << EMC_WARNING_MSG
         << " emcHLRatioFEM::getValue(ichannel,what) : what="
         << what << " is incorrect. Returning default value of "
         << DefaultReturnValue() << " instead" << endl ;

    return DefaultReturnValue() ;

  }

  if (ichannel>=0 && ichannel<static_cast<int>(fHLRatioVector.size())) {
    return getValueFast(ichannel,what) ;
  }
  else {
    return DefaultReturnValue() ;
  }
}

//_____________________________________________________________________________
float
emcHLRatioFEM::getValueFast(int ichannel, int what) const
{
  return (fHLRatioVector[ichannel])[what] ;
}

//_____________________________________________________________________________
bool
emcHLRatioFEM::IsEqual(const emcCalFEM& obj) const
{
  if ( !dynamic_cast<const emcHLRatioFEM*>(&obj) ) return false ;

  if ( obj.size() != size() ) return false ;
  for ( size_t i = 0 ; i < size() ; i++ ) {
    for ( int j = 0 ; j < 4 ; j++ ) {
      if ( getValue(i,j) != obj.getValue(i,j) ) return false ;
    }
  }
  return true ;
}

//_____________________________________________________________________________
void emcHLRatioFEM::Reset(void)
{
  fHLRatioVector.clear() ;
}

//_____________________________________________________________________________
ostream&
emcHLRatioFEM::Print(ostream& out, int level) const
{
  emcCalFEM::Print(out,level) ;

  if (level) {
    for ( size_t i = 0  ; i < fHLRatioVector.size() ; i++)
      {
	out << "#" << i 
	     << " Average=" << getValue(i,0)
	     << " RMS=" << getValue(i,1)
	     << " Intercept=" << getValue(i,2)
	     << " Slope=" << getValue(i,3)
	     << endl ;
      }
  }
  return out ;
}
