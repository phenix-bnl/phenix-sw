#include "emcWalkTofFEM.h"
#include "emcDataManager.h"
#include "emcDefines.h"
#include "emcCalFEMFactory.h"
#include <string>
#include <cassert>

using namespace std;

namespace
{
  static string name = "emcWalkTofFEM" ;
  static string title = "WalkTof calibration data" ;
  static string classname = "emcWalkTofFEM" ;
  
  emcCalFEM* creator(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcWalkTofFEM::Default(absPosition,start,end);
      }
    else
      {
	return new emcWalkTofFEM(absPosition,start,end);
      }
  }

  static bool r = emcCalFEMFactory::registerCreator("WalkTofs",
						    creator);
}

//_____________________________________________________________________________
emcWalkTofFEM::emcWalkTofFEM(int absPosition)
  : emcCalFEM(absPosition)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
emcWalkTofFEM::emcWalkTofFEM(int absPosition, 
			 const PHTimeStamp& t1, const PHTimeStamp& t2)
  : emcCalFEM(absPosition,t1,t2)
{
  NameIt(name,title,classname) ;
}

//_____________________________________________________________________________
emcWalkTofFEM::emcWalkTofFEM(const emcWalkTofFEM& o)
  : emcCalFEM(o.AbsolutePosition(),o.GetStartValTime(),
	      o.GetEndValTime())
{
  o.Copy(*this) ;
}

//_____________________________________________________________________________
emcWalkTofFEM&
emcWalkTofFEM::operator=(const emcWalkTofFEM& o)
{
  if ( this == &o ) return *this ;
  Reset() ;
  o.Copy(*this) ;
  return *this ;
}

//_____________________________________________________________________________
void
emcWalkTofFEM::Copy(emcWalkTofFEM& o) const
{
  emcCalFEM::Copy(o) ;
  o.fWalkTof.resize(fWalkTof.size()) ;
  size_t i ;
  for ( i = 0 ; i < fWalkTof.size() ; i++) {
    o.fWalkTof[i] = fWalkTof[i] ;
  }  
}

//_____________________________________________________________________________
emcWalkTofFEM::~emcWalkTofFEM()
{
  Reset() ;
}

//_____________________________________________________________________________
void emcWalkTofFEM::AppendOneChannel(const float value1, const float value2)
{
  vfloat tmp;

  tmp.push_back(value1);
  tmp.push_back(value2);

  if (fWalkTof.size()<144) {
    fWalkTof.push_back(tmp);
  }
  else {
    cerr << "<E-EMCAL> emcWalkTofFEM::AppendOneChannel : fem is full" << endl ;
    assert(0);
  }
}

//_____________________________________________________________________________
emcWalkTofFEM* 
emcWalkTofFEM::Default(const int& absPosition, 
                     const PHTimeStamp& t1, 
                     const PHTimeStamp& t2)
{
  emcWalkTofFEM* fem = new emcWalkTofFEM(absPosition,t1,t2) ;

  size_t i ;

  for ( i = 0 ; i < 144 ; i++ ) {
    // FIXME: what's a sensible default value here ?
    fem->AppendOneChannel(0,0) ;
  }
  return fem ;
}

//_____________________________________________________________________________
float
emcWalkTofFEM::getValue(int ichannel, int what) const 
{
  if (what!=0 && what!=1) {

    cerr << EMC_WARNING_MSG
         << " emcWalkTofFEM::getValue(ichannel,what) : what="
         << what << " is incorrect. Returning default value of "
         << DefaultReturnValue() << " instead" << endl ;

    return DefaultReturnValue() ;

  }

  if (ichannel>=0 && ichannel<static_cast<int>(fWalkTof.size())) {
    return getValueFast(ichannel,what) ;
  }
  else {
    return DefaultReturnValue() ;
  }
}

//_____________________________________________________________________________
void
emcWalkTofFEM::setValue(int ichannel, int what, float value) 
{
  if((what>=0&&what<=1)&&(ichannel>=0&&ichannel<144)) fWalkTof[ichannel][what] = value; else 
    cerr << EMC_WARNING_MSG
	 << "<emcWalkTofFEM> Attempt to set value outside address field - ignored"<<endl;
}

//_____________________________________________________________________________
float
emcWalkTofFEM::getValueFast(int ichannel, int what) const
{
  return (fWalkTof[ichannel])[what] ;
}

//_____________________________________________________________________________
bool
emcWalkTofFEM::IsEqual(const emcCalFEM& obj) const
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
void emcWalkTofFEM::Reset(void)
{
  fWalkTof.clear();
}

//_____________________________________________________________________________
ostream&
emcWalkTofFEM::Print(ostream& out, int level) const
{
  emcCalFEM::Print(out,level) ;

  if (level) {
    
    int size = fWalkTof.size() ;
     
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
