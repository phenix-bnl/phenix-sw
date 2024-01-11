#include "emcPedestalFEM.h"
#include "emcCalFEMFactory.h"
#include <cstdlib>
#include <cstdio>
#include <iterator>
#include <map>
#include <iomanip>
#include <cassert>
#include <algorithm>
#include <string>
#include <vector>
#include "emcDefines.h"
#include <cmath>

using namespace std;

namespace 
{
  static string name = "emcPedestalFEM" ;
  static string title = "Pedestal calibration data";
  string Title(const string& title, int version)
  {
    string rv = title;
    if ( version==0 ) 
      {
	rv += "(3 per AMU)";
      }
    else
      {
	rv += "(5 per AMU)";
      }
    return rv;
  }

  static string classname = "emcPedestalFEM" ;
  static float ERROR_VALUE = -9999.99 ;

  emcCalFEM* creator3(int absPosition,
		     const PHTimeStamp& start,
		     const PHTimeStamp& end,
		     bool isDefault)
  {
    if ( isDefault )
      {
	return emcPedestalFEM::Default(absPosition,start,end,0);
      }
    else
      {
	return new emcPedestalFEM(absPosition,start,end,0);
      }
  }
 
  emcCalFEM* creator5(int absPosition,
		      const PHTimeStamp& start,
		      const PHTimeStamp& end,
		      bool isDefault)
  {
    if ( isDefault )
      {
	return emcPedestalFEM::Default(absPosition,start,end,1);
      }
    else
      {
	return new emcPedestalFEM(absPosition,start,end,1);
      }
  }

  static bool r3 = emcCalFEMFactory::registerCreator("Pedestals",
						     creator3);
  static bool r5 = emcCalFEMFactory::registerCreator("Pedestals5",
						     creator5);
}

//_____________________________________________________________________________
emcPedestalFEM::emcPedestalFEM(int absPosition,
			       const PHTimeStamp& t1, const PHTimeStamp& t2,
			       int version)
  : emcCalFEM(absPosition,t1,t2)
{
  NameIt(name,Title(title,version),classname) ;
  SetVersion(version) ;
  fValidTypes = emcPedestalFEM::ValidTypes(version) ;
}

//_____________________________________________________________________________
emcPedestalFEM::emcPedestalFEM(int absPosition,			       
			       int version)
  : emcCalFEM(absPosition)
{
  NameIt(name,Title(title,version),classname) ;
  SetVersion(version) ;
  fValidTypes = emcPedestalFEM::ValidTypes(version) ;
}

//_____________________________________________________________________________
emcPedestalFEM::emcPedestalFEM(const emcPedestalFEM& o)
  : emcCalFEM(o.AbsolutePosition())
{
  o.Copy(*this) ;
}

//_____________________________________________________________________________
emcPedestalFEM&
emcPedestalFEM::operator=(const emcPedestalFEM& o)
{
  if ( this == &o ) return *this ;
  Reset() ;
  o.Copy(*this) ;
  return *this ;
}

//_____________________________________________________________________________
void
emcPedestalFEM::Copy(emcPedestalFEM& o) const
{
  emcCalFEM::Copy(o) ;
  o.SetVersion(Version()) ;
  o.Reset() ;
  o.fValidTypes = emcPedestalFEM::ValidTypes(Version()) ;
  size_t i ;

  for ( i = 0 ; i < fValidTypes.size() ; i++) {
    string type = fValidTypes[i] ;
    o.fPed[type] = new ChannelVector ;
    ChannelVector* vec = fPed[type] ;
    assert(vec!=0) ;
    size_t j ;
    for ( j = 0 ; j < vec->size() ; j++) {
      o.fPed[type]->push_back((*vec)[j]) ;
    }
  }  
}

//_____________________________________________________________________________
emcPedestalFEM::~emcPedestalFEM()
{
  Reset() ;
}

//_____________________________________________________________________________
void emcPedestalFEM::AppendOneChannel(const char* sped_type, AmuVector& vec)
{
  assert (IsValidType(sped_type)) ;
  assert (vec.size()==64) ;

  string ped_type = sped_type ;

  if (fPed[ped_type]==0) {
    fPed[ped_type] = new ChannelVector ;
  }

  int thesize = fPed[ped_type]->size() ;

  if (thesize<144) {
    float average = 0 ;
    float rms = 0 ;
    for ( size_t i = 0 ; i < vec.size() ; i++) {
      average += vec[i] ;
    }
    average /= vec.size() ;
    if ( vec.size() > 1 ) {
      for ( size_t i = 0 ; i < vec.size() ; i++) {
	rms += (vec[i]-average)*(vec[i]-average) ;
      }
      rms /= vec.size()-1 ;
    }
    rms = sqrt(rms) ;
    fPed[ped_type]->push_back(vec) ;
    int iaverage = static_cast<int>(floor(average+0.5));
    fPedAverage[ped_type].push_back(iaverage) ;
    fPedRMS[ped_type].push_back(rms) ;
    if ( ped_type == "TAC" ) 
      {
	const std::string ksTACDEV = "TACDEV";

	AmuVector vecdev = vec;
	float devmean = 0.0;

	for ( size_t i = 0; i < vecdev.size(); ++i )
	  {
	    vecdev[i] -= iaverage;
	    devmean += vecdev[i];
	  }
	devmean /= vecdev.size();
	float devrms = 0.0;
	for ( size_t i = 0; i < vecdev.size(); ++i )
	  {
	    devrms += (vecdev[i]-devmean)*(vecdev[i]-devmean);
	  }
	devrms /= vecdev.size() - 1;

	if (!fPed[ksTACDEV])
	  {
	    fPed[ksTACDEV] = new ChannelVector;
	  }
	fPed[ksTACDEV]->push_back(vecdev);
	fPedAverage[ksTACDEV].push_back(static_cast<int>(floor(devmean+0.5)));
	fPedRMS[ksTACDEV].push_back(devrms);
      }
  }
  else {
    cerr << EMC_ERROR_MSG 
	 << " emcPedestalFEM::AppendOneChannel : FEM is already full" << endl ;
  }
}

//_____________________________________________________________________________
emcPedestalFEM*
emcPedestalFEM::Default(const int& absPosition, 
                        const PHTimeStamp& t1, 
                        const PHTimeStamp& t2,
                        int version) 
{
  emcPedestalFEM* fem = new emcPedestalFEM(absPosition,t1,t2,version) ;

  AmuVector vec(64,0) ;

  size_t ch, i ;

  vector<string> ped_types = emcPedestalFEM::ValidTypes(fem->Version()) ;

  for ( ch = 0 ; ch < 144 ; ch++ ) {
    for ( i = 0 ; i < ped_types.size() ; i++ ) {
      fem->AppendOneChannel(ped_types[i].c_str(),vec) ;
    }
  }

  return fem ;
}

//_____________________________________________________________________________
const char* 
emcPedestalFEM::GetCategory(void) const
{
  if (Version()==0) return "Pedestals" ;
  if (Version()==1) return "Pedestals5" ;
  std::cerr << "<E> " << __FILE__ << ":" << __LINE__ << " Version unknown!!!"
	    << std::endl;
  return "Unknown";
}

//_____________________________________________________________________________
size_t emcPedestalFEM::GetNumberOfChannels(void) const
{ 
  /* FIXME: The logic of this function might be a little too complicated
     for what it really does... But it's better to warn the user in case
     the object has not been filled completely (i.e. not all ped types
     have the same size, which we call here not 'consistent' state).
   */

  size_t i ;
  size_t thesize = 0 ;
  ChannelVector* cv = 0 ;

  // we first search for the first ped array which is not void
  for ( i = 0 ; i < fValidTypes.size() && cv == 0; i++ ) {
    cv = fPed[fValidTypes[i]] ;
    if (cv) {
      thesize = cv->size() ;
    }
  }

  if (thesize==0) return 0 ; // everything is void

  // we then check that all ped arrays have the same size
  
  bool consistent = true ;

  for ( i = 0 ; i < fValidTypes.size() ; i++ ) {
    cv = fPed[fValidTypes[i]] ;
    if (cv) {
      if ( cv->size() != thesize ) consistent = false ;
    } else {
      consistent = false ;
    }
  }

  if (!consistent) {
    cerr << EMC_ERROR_MSG 
	 << " Pedestal object not in consistent state: " << endl ;
    for ( i = 0 ; i<fValidTypes.size() ; i++ ) {
      cv = fPed[fValidTypes[i]] ;
      if (cv) {
	cerr << fValidTypes[i] << " has a size of " <<  cv->size() << endl ;
      } else {
	cerr << fValidTypes[i] << " has a size of " <<  0 << endl ;
      }
    }
  }

  // warning: if not in consistent state, thesize is the size
  // of the first ped type which is filled ! FIXME ? to return the max value?
  return thesize ;
}

//_____________________________________________________________________________
float
emcPedestalFEM::getValue(int ich, const string& ped_type, float& rms) const
{
  rms = 0.0 ;
  if ( !IsValidType(ped_type) ) return ERROR_VALUE ;
  vector<int>& vec = fPedAverage[ped_type] ;
  if ( ich < 0 || ich >= static_cast<int>(vec.size()) ) {
    return ERROR_VALUE ;
  }
  return getValueFast(ich,ped_type,rms) ;
}

//_____________________________________________________________________________
float
emcPedestalFEM::getValueFast(int ich, const string& ped_type, float& rms) const
{
  rms = fPedRMS[ped_type][ich] ;
  return fPedAverage[ped_type][ich] ;
}

//_____________________________________________________________________________
float
emcPedestalFEM::getValue(int ch_index, int amu_number, 
			 const string& ped_type) const
{
  float err_value = ERROR_VALUE ;

  if (!IsValidType(ped_type)) return err_value ;
  ChannelVector* cv = fPed[ped_type] ;

  if (cv==0) {
    static bool first = true ;
    if (first) {
      cerr << EMC_ERROR_MSG << "emcPedestalFEM::getValue : cv is NULL. "
	   << endl << "Please check that. "
	   << "This message will _not_ be repeated even if " << endl
	   << "the same error occurs again." << endl ;
    }
    first=false ;
    return err_value ;
  }

  if ( ch_index < 0 || ch_index >= static_cast<int>(cv->size()) ) {
    return err_value ;
  }
  if ( amu_number < 0 || amu_number >= static_cast<int>((*cv)[ch_index].size()) ) {
    return err_value ;
  }
  return (*cv)[ch_index][amu_number] ;
}

//_____________________________________________________________________________
float 
emcPedestalFEM::getValueFast(int ch_index, int amu_number, 
                             const string& ped_type) const
{
  return (*(fPed[ped_type]))[ch_index][amu_number] ;
}

//_____________________________________________________________________________
void emcPedestalFEM::GetValues(int ch_index, int amu_number, 
			       int& low, int& high, int& tac)
{
  // This method assumes that LG_Pre-Post, HG_Pre-Post and TAC are
  // valid ped types.  WARNING: No check on ch_index and amu_number !

  low  = (*fPed["LG_Pre-Post"])[ch_index][amu_number] ;
  high = (*fPed["HG_Pre-Post"])[ch_index][amu_number] ;
  tac  = (*fPed["TAC"])[ch_index][amu_number] ;
}

//_____________________________________________________________________________
bool
emcPedestalFEM::IsEqual(const emcCalFEM& obj) const
{
  if ( !dynamic_cast<const emcPedestalFEM*>(&obj) ) return false ;

  if ( obj.size() != size() ) return false ;

  if ( Version() != obj.Version() ) return false ;

  vector<string> obj_types = ValidTypes(obj.Version()) ;

  if ( obj_types.size() != fValidTypes.size() ) return false ;

  for ( size_t t = 0 ; t < fValidTypes.size() ; t++ ) {

    string type = fValidTypes[t] ;

    for ( size_t ichannel = 0 ; ichannel < size() ; ichannel++) {
      for ( size_t amu = 0 ; amu < 64 ; amu++) {       
	if ( getValue(ichannel,amu,type) != obj.getValue(ichannel,amu,type) ) {
	  return false ;
	}
      }
    }

  }

  return true ;
}

//_____________________________________________________________________________
bool emcPedestalFEM::IsValidType(const string& sped) const
{
  vector<string>::const_iterator result ;

  result = find(fValidTypes.begin(),fValidTypes.end(),sped) ;

  if (result!=fValidTypes.end()) return true ;

  cout << "<E> Invalid ped type : " << sped << endl ;
  return false ;
}

//_____________________________________________________________________________
void emcPedestalFEM::Reset(void)
{
  size_t i ;

  for ( i = 0 ; i < fValidTypes.size() ; i++ ) {
    delete fPed[fValidTypes[i]] ;
  }
  fPed.clear() ;
  fPedAverage.clear() ;
  fPedRMS.clear() ;
}

//_____________________________________________________________________________
ostream&
emcPedestalFEM::Print(ostream& out, int level) const
{
  emcCalFEM::Print(out,level);

  if (level) {

    map<string,emcPedestalFEM::ChannelVector*>::const_iterator it ;
    emcPedestalFEM::ChannelVector* cv ;
    size_t ch,amu ;
    size_t rc ;
    
    for (it=fPed.begin();it!=fPed.end();it++) {
      out << string(50,'-')  << endl ;
      out << "-- Pedestals for " << it->first << endl ;
      out << string(50,'-') << endl ;
      cv = it->second ;
      assert (cv!=0) ;
      for (ch=0;ch<cv->size();ch++) {
	out << "CH# " << ch << "\t : " ;
	rc = 0 ;
	for (amu=0;amu<(*cv)[ch].size();amu++) {
	  rc++ ;
	  out << static_cast<int>((*cv)[ch][amu]) << " " ;
	  if (rc==10) {
	    out << endl ;
	    out << "\t : " ;
	    rc = 0 ;
	  }
	}
	out << "Average=" << fPedAverage[it->first][ch]
	    << " RMS=" << fPedRMS[it->first][ch] << endl ;
	out << endl ;
      }
    }
  }
  return out ;
}

//_____________________________________________________________________________
vector<string>
emcPedestalFEM::ValidTypes(int version) 
{
  vector<string> validtypes ;

  if (version==1) {
    validtypes.push_back("LG_Pre") ;
    validtypes.push_back("HG_Pre") ;
    validtypes.push_back("LG_Post") ;
    validtypes.push_back("HG_Post") ;
    validtypes.push_back("TAC") ;
    validtypes.push_back("TACDEV") ;
  }
  else if (version==0) {
    validtypes.push_back("LG_Pre-Post") ;
    validtypes.push_back("HG_Pre-Post") ;
    validtypes.push_back("TAC") ;
  }
  else {
    assert(0==1) ;
  }

  return validtypes ;
}
