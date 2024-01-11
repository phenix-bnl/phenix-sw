#include "emcLCTofFEM.h"
#include "emcDefines.h"
#include "emcCalFEMFactory.h"
#include <string>
#include <cassert>

using namespace std;

namespace
{
  static string name = "emcLCTofFEM" ;
  static string title = "Least-Count calibration data" ;
  static string classname = "emcLCTofFEM" ;

  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcLCTofFEM::Default(absPosition,start,end);
      }
    else
      {
	return new emcLCTofFEM(absPosition,start,end);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("LCTofs",
						    creator);
}

//_____________________________________________________________________________
emcLCTofFEM::emcLCTofFEM(int absPosition)
  : emcCalFEM(absPosition)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
emcLCTofFEM::emcLCTofFEM(int absPosition, 
			 const PHTimeStamp& t1, const PHTimeStamp& t2)
  : emcCalFEM(absPosition,t1,t2)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
emcLCTofFEM::emcLCTofFEM(const emcLCTofFEM& o)
  : emcCalFEM(o.AbsolutePosition(),o.GetStartValTime(),
	      o.GetEndValTime())
{
  o.Copy(*this) ;
}

//_____________________________________________________________________________
emcLCTofFEM&
emcLCTofFEM::operator=(const emcLCTofFEM& o)
{
  if ( this == &o ) return *this ;
  Reset() ;
  o.Copy(*this) ;
  return *this ;
}

//_____________________________________________________________________________
void
emcLCTofFEM::Copy(emcLCTofFEM& o) const
{
  emcCalFEM::Copy(o) ;
  o.fLCTof.resize(fLCTof.size()) ;
  size_t i ;
  for ( i = 0 ; i < fLCTof.size() ; i++) {
    o.fLCTof[i] = fLCTof[i] ;
  }  
}

//_____________________________________________________________________________
emcLCTofFEM::~emcLCTofFEM()
{
  Reset() ;
}

//_____________________________________________________________________________
void emcLCTofFEM::AppendOneChannel(const float value1, const float value2)
{
  vfloat tmp;

  tmp.push_back(value1);
  tmp.push_back(value2);

  if (fLCTof.size()<144) {
    fLCTof.push_back(tmp);
  }
  else {
    cerr << "<E-EMCAL> emcLCTofFEM::AppendOneChannel : fem is full" << endl ;
    assert(0);
  }
}

//_____________________________________________________________________________
emcLCTofFEM* 
emcLCTofFEM::Default(const int& absPosition, 
                     const PHTimeStamp& t1, 
                     const PHTimeStamp& t2)
{
  emcLCTofFEM* fem = new emcLCTofFEM(absPosition,t1,t2) ;

  size_t i ;

  for ( i = 0 ; i < 144 ; i++ ) {
    // FIXME: what's a sensible default value here ?
    fem->AppendOneChannel(0,0) ;
  }
  return fem ;
}

//_____________________________________________________________________________
float
emcLCTofFEM::getValue(int ichannel, int what) const 
{
  if (what!=0 && what!=1) {

    cerr << EMC_WARNING_MSG
         << " emcLCTofFEM::getValue(ichannel,what) : what="
         << what << " is incorrect. Returning default value of "
         << DefaultReturnValue() << " instead" << endl ;

    return DefaultReturnValue() ;

  }

  if (ichannel>=0 && ichannel<static_cast<int>(fLCTof.size())) {
    return getValueFast(ichannel,what) ;
  }
  else {
    return DefaultReturnValue() ;
  }
}

//_____________________________________________________________________________
void
emcLCTofFEM::setValue(int ichannel, int what, float value) 
{
  if((what>=0&&what<=1)&&(ichannel>=0&&ichannel<144)) fLCTof[ichannel][what] = value; else 
    cerr << EMC_WARNING_MSG
	 << "<emcLCTofFEM> Attempt to set value outside address field - ignored"<<endl;
}

//_____________________________________________________________________________
float
emcLCTofFEM::getValueFast(int ichannel, int what) const
{
  return (fLCTof[ichannel])[what] ;
}

//_____________________________________________________________________________
bool
emcLCTofFEM::IsEqual(const emcCalFEM& obj) const
{
  if ( !dynamic_cast<const emcCalFEM*>(&obj) ) return false ;

  if ( size() != obj.size() ) return false ;

  for ( size_t i = 0 ; i < size() ; i++) {
    for ( int j = 0 ; j < 2 ; j++ ) {
      if ( getValue(i,j) != obj.getValue(i,j) ) return false ;
    }
  }

  return true ;
}

//_____________________________________________________________________________
void emcLCTofFEM::Reset(void)
{
  fLCTof.clear();
}

//_____________________________________________________________________________
ostream&
emcLCTofFEM::Print(ostream& out, int level) const
{
  emcCalFEM::Print(out,level) ;

  if (level) {
    
    int size = fLCTof.size() ;
     
    for ( int i = 0  ; i <  size ; i++)
      {
	out << "#" << i 
	    << " " << getValue(i,0) 
	    << " " << getValue(i,1) 
	    << endl ;	
      }
  }
  return out ;
}
