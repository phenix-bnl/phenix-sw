#include "emcCalibrationDataHelper.h"

#include <cassert>
#include <cmath>
#include <algorithm>
#include <cmath>
#include <cctype>
#include <sstream>

#include "emcCalibrationData.h"
#include "emcCalFEM.h"
#include "emcCalFEMFactory.h"
#include "emcDataManager.h"
#include "emcFEMList.h"
#include "emcGainBaseLineCalculator.h"
#include "EmcIndexer.h"
#include "emcTimeStamp.h"
#include "emcTracedFEM.h"
#include "emcTracedValue.h"

#ifdef DEBUG
#  include "TBenchmark.h"
#  include "TSystem.h"
#endif

using namespace std;

//_____________________________________________________________________________
emcCalibrationDataHelper::emcCalibrationDataHelper(int runnumber,
						   bool initall,
						   emcManageable::EStorage src,
						   const char* parts)
{
  emcTimeStamp ets;

  ets.SetSource(src);

  emcDataManager* dm = emcDataManager::GetInstance();

  bool ok = dm->Read(ets, runnumber);

  PHTimeStamp ts;

  if ( ok )
    {
      ts = ets.getTimeStamp();
    }
  else
    {
      ts.setToSystemTime();
      cerr << "emcCalibrationDataHelper::emcCalibrationDataHelper : "
	   << "could not find timestamp for run " << runnumber
	   << "Using timestamp=now=" << ts << endl;
    }
  ctor(runnumber, ts, initall, src, parts);
}

//_____________________________________________________________________________
emcCalibrationDataHelper::emcCalibrationDataHelper(int runnumber,
						   const PHTimeStamp& ts,
						   bool initall,
						   emcManageable::EStorage src,
						   const char* parts)
{
  ctor(runnumber, ts, initall, src, parts);
}

//_____________________________________________________________________________
void
emcCalibrationDataHelper::ctor(int runnumber,
                               const PHTimeStamp& ts,
                               bool initall,
                               emcManageable::EStorage src,
                               const char* parts)
{
  fInitAll = initall;

  fRunNumber = runnumber;

  fTimeStamp = ts;

  fMaxNumberOfFEMs = 172; // FIXME: for online, need more than that.

  fFemList = new emcFEMList(parts);

  std::vector<std::string> flavours;

  flavours.push_back("HLRatios");
  flavours.push_back("Pedestals5");
  flavours.push_back("Gains");
  flavours.push_back("Gains:BLR:0:xmax:-3:-9:-15:92446");
  //  reference run for run5-run8
  flavours.push_back("Gains:BLR:0:x:ZS:AVOFRATIO:164777");
  //  reference run for run9
  flavours.push_back("Gains:BLR:0:x:ZS:AVOFRATIO:270593");
  flavours.push_back("LCTofs");
  flavours.push_back("WalkTofs");
  //  flavours.push_back("TofT0s");
  flavours.push_back("TofT0Bs");

  for ( size_t i = 0; i < flavours.size(); ++i )
    {
      Flavour f(flavours[i]);

      for ( int ifem = 0; ifem < fMaxNumberOfFEMs; ++ifem )
        {
          if ( fFemList->hasFEM(ifem) )
            {
              f.append(ifem);
            }
        }

      fKnownFlavours.push_back(f);
    }

  for ( size_t i = 0; i < fKnownFlavours.size(); ++i )
    {
      Flavour& f = fKnownFlavours[i];
      fData[f.name()].resize(f.size(), 0);
      fSources[f.name()] = src;
    }

  fCalibData["IniCal"].resize(8, 0);
  fCalibData["TofSectorOffset"].resize(8, 0);

  fSources["TofSectorOffset"] = src;
  fSources["IniCal"] = src;

  if ( initall )
    {
      initAll();
    }
}

//_____________________________________________________________________________
emcCalibrationDataHelper::~emcCalibrationDataHelper()
{
  TMAPITER it;

  for ( it = fData.begin(); it != fData.end(); ++it )
    {
      std::vector<emcCalFEM*>& vec = it->second;
      for ( size_t i = 0; i < vec.size(); ++i )
        {
          delete vec[i];
          vec[i] = 0;
        }
    }

  TCMAPITER itc;

  for ( itc = fCalibData.begin(); itc != fCalibData.end(); ++itc )
    {
      std::vector<emcCalibrationData*>& vec = itc->second;
      for ( size_t i = 0; i < vec.size(); ++i )
        {
          delete vec[i];
          vec[i] = 0;
        }
    }

  delete fFemList;
}

//_____________________________________________________________________________
emcCalFEM*
emcCalibrationDataHelper::collect(int femAbsolutePosition,
                                  const string& what)
{
  //
  // This method is for collecting emcCalFEM-type objects
  // 
  // the what parameter MUST correspond to something which is collectable
  // otherwise you'll get a null pointer back.
  //

  assert(fTimeStamp != 0);

  emcDataManager* dm = emcDataManager::GetInstance();

  if ( source(what) == emcManageable::kNone )
    {
      // create default here.
      return emcCalFEMFactory::CreateDefault(what.c_str(), femAbsolutePosition);
    }

  emcCalFEM* calfem =
    emcCalFEMFactory::Create(what.c_str(), femAbsolutePosition);

  if (!calfem)
    {
      return 0;
    }

  calfem->SetSource(source(what));

  int code = femAbsolutePosition;

  if ( dm->GetVerboseLevel() )
    {
      cout << "<I> emcCalibrationDataHelper::collect "
	   << "for FEM#" << femAbsolutePosition
	   << " (flavor " << calfem->GetCategory()
	   << ")"
	   << endl;
    }

  bool ok = dm->Read(*calfem, fTimeStamp, code);

  if (!ok)
    {

      string category = calfem->GetCategory();

      // create default objects instead
      cout << "<W> emcCalibrationDataHelper::collect : "
	   << "Creating default calibration object (flavour "
	   << setw(10) << setfill(' ') << category
	   << ") for FEM#"
	   << setw(3) << setfill('0') << femAbsolutePosition
	   << setfill(' ') << endl;


      PHTimeStamp t1, t2;
      t1.setTics(0);
      t2.setToFarFuture();
      delete calfem;

      calfem = emcCalFEMFactory::CreateDefault(category.c_str(),
					       femAbsolutePosition,
					       t1, t2);
    } // ! ok

  return calfem;
}

//_____________________________________________________________________________
emcCalibrationData*
emcCalibrationDataHelper::collectCalibData(const std::string& what,
					   size_t number)
{
  //
  // This method is for collecting emcCalibrationData-type objects.
  //

  assert(fTimeStamp != 0);

  emcDataManager* dm = emcDataManager::GetInstance();

  emcCalibrationData::EType type;

  if ( what == "IniCal" )
    {
      type = emcCalibrationData::kIniCal;
    }
  else if ( what == "TofSectorOffset" )
    {
      type = emcCalibrationData::kTofSectorOffset;
    }
  else
    {
      cerr << "emcCalibrationDataHelper::collectCalibData : type "
	   << what << " is not supported"
	   << endl;
      return 0;
    }

  emcCalibrationData* cd = new emcCalibrationData(type, number);

  if ( source(what) == emcManageable::kNone )
    {
      cout << "<WARNING> emcCalibrationDataHelper::collectCalibData : "
	   << "source of "
	   << what << " is \"none\". "
	   << "That may be fine. I return a default object."
	   << endl;
      // should create a default object here.
      // for now, assume the default ctor is ok
      return cd;
    }

  cd->SetSource(source(what));

  if ( dm->GetVerboseLevel() )
    {
      cout << "<I> emcCalibrationDataHelper::collectCalibData for type "
	   << what << " number " << number
	   << endl;
    }

  bool ok = false;

  if ( type == emcCalibrationData::kTofSectorOffset && 
       source(what) == emcManageable::kFile_ASCII)
    {
      ok = dm->Read(*cd,fRunNumber);
    }
  else
    {
      ok = dm->Read(*cd, fTimeStamp);
    }

  if ( !ok )
    {
      cerr << "<E> emcCalibrationDataHelper::collectCalibData for type "
	   << what << " number " << number << " failed !"
	   << endl;
      return 0;
    }
  else
    {
      return cd;
    }
}

//_____________________________________________________________________________
const
emcCalFEM*
emcCalibrationDataHelper::getCalibration(int femAbsolutePosition,
					 const char* whatKind)
{
  return getCalibration(femAbsolutePosition, string(whatKind));
}

//_____________________________________________________________________________
const emcCalFEM*
emcCalibrationDataHelper::getCalibration(int femAbsolutePosition,
					 const string& whatKind)
{
  TMAPITER it = fData.find(whatKind);
  
  if ( it != fData.end() )
    {
      vector<emcCalFEM*>& vec = it->second;
      if ( femAbsolutePosition < 0 ||
           femAbsolutePosition >= static_cast<int>(vec.size()) )
        {
          cerr << "emcCalibrationDataHelper::getCalibration : "
	       << "femAbsolutePosition (" << femAbsolutePosition
	       << ") out of bounds (0.." << vec.size()-1
	       << ") kind=" << whatKind
	       << endl;
          return 0;
        }
      else
        {
          emcCalFEM* calfem = vec[femAbsolutePosition];
          if ( !calfem )
          {
	        std::string kind = whatKind;
	        std::string::size_type pos = whatKind.find_first_of(':');
	        if ( pos != std::string::npos )
		    {
		      kind = whatKind.substr(0,pos);
		  	  calfem = vec[femAbsolutePosition] 
		    		= getCalibration(femAbsolutePosition,kind)->clone();
		  	  patch(*calfem,whatKind.substr(pos+1));
		    }
	        else
		    {
		  	  calfem = vec[femAbsolutePosition]
		    		= collect(femAbsolutePosition, kind);
		    }
          }
          return calfem;
        }
    }
  else
    {
      cerr << "emcCalibrationDataHelper::getCalibration : unknown kind "
	   << whatKind
	   << endl;
      return 0;
    }
}

//_____________________________________________________________________________
const emcCalibrationData*
emcCalibrationDataHelper::getCalibrationData(const std::string& what,
					     size_t number)
{
  TCMAPITER it = fCalibData.find(what);

  if ( it != fCalibData.end() )
    {
      std::vector<emcCalibrationData*>& vec = it->second;
      if ( number >= vec.size() )
        {
          cerr << "emcCalibrationDataHelper::getCalibrationData : number ("
	       << number << " out of bounds (0.." << vec.size()
	       << endl;
          return 0;
        }
      else
        {
          emcCalibrationData* cd = vec[number];
          if ( !cd )
            {
              cd = vec[number] = collectCalibData(what, number);
            }
          return cd;
        }
    }
  else
    {
      cerr << "emcCalibrationDataHelper::getCalibrationData : unknown kind "
	   << what
	   << endl;
      return 0;
    }
}

//_____________________________________________________________________________
const emcCalibrationData*
emcCalibrationDataHelper::getCalibrationData(const char* what, size_t number)
{
  return getCalibrationData(string(what), number);
}

//_____________________________________________________________________________
const emcCalFEM*
emcCalibrationDataHelper::getCalibrationFast(int femAbsolutePosition,
					     const char* whatKind)
{
  return getCalibrationFast(femAbsolutePosition, string(whatKind));
}

//_____________________________________________________________________________
const emcCalFEM*
emcCalibrationDataHelper::getCalibrationFast(int femAbsolutePosition,
					     const string& whatKind)
{
  TMAPITER it = fData.find(whatKind);

  if ( it != fData.end() )
    {
      vector<emcCalFEM*>& vec = it->second;
      return vec[femAbsolutePosition];
    }
  else
    {
      return 0;
    }
}

//_____________________________________________________________________________
float
emcCalibrationDataHelper::getEnergyCalibration(int towerid)
{
  if ( fECalAtT0.empty() )
    {
      initECalAtT0(true);
    }

  if ( towerid >= 0 && towerid < static_cast<int>(fECalAtT0.size()) )
    {
      return fECalAtT0[towerid];
    }
  else
    {
      return 0.0;
    }
}


//_____________________________________________________________________________
float
emcCalibrationDataHelper::getEnergyCalibrationFast(int towerid)
{
  return fECalAtT0[towerid];
}

//_____________________________________________________________________________
float
emcCalibrationDataHelper::getGainBaseLine(int isector,
					  const char* what,
					  const char* details,
					  bool reallySilent)
{
  std::string previousDetails(gainBaseLineCalculationDetails(isector));
  	
  if ( !previousDetails.empty() &&
       previousDetails != details )
    {
      // Force re-initialization
      if ( !reallySilent )
	{
	  std::cout << "<W> emcCalibrationDataHelper::getGainBaseLine : "
		    << "details changed. Recomputing average for sector "
		    << isector
		    << std::endl;
	}
      initGainBaseLine(isector, details);
    }

  TBLMAPITER it = fGainBaseLine.find(isector);

  if ( ! ( it != fGainBaseLine.end() ) )
    {
      initGainBaseLine(isector, details);
      it = fGainBaseLine.find(isector);
    }

  std::string swhat = what;
  if ( swhat == "value" )
    {
      return it->second.value();
    }

  if ( swhat == "error" )
    {
      return it->second.error();
    }

  if ( swhat == "skewness" ) 
    {
      return it->second.skewness();
    }

  if ( swhat == "kurtosis" )
    {
      return it->second.kurtosis();
    }

  // by default, return value
  return it->second.value();
}
//_____________________________________________________________________________
void emcCalibrationDataHelper::print(){
  cout<<"<emcCalibrationDataHelper::print> Helper Status"<<endl;
  cout<<"fInitAll     = "<<(fInitAll? "true" : "false")<<endl;
  cout<<"fRunNumber   = "<<fRunNumber<<endl;
  cout<<"fTimeStamp   = "<<fTimeStamp<<endl;
  cout<<"flavorsKnown = "<<fKnownFlavours.size()<<endl;
  for ( size_t i = 0; i < fKnownFlavours.size(); ++i )
    {
      Flavour& f = fKnownFlavours[i];
      cout<<"   flavor "<<i<<"   "<<f.name()<<"  Source "<<fSources[f.name()]<<endl;
    }
}
//_____________________________________________________________________________
const char* 
emcCalibrationDataHelper::gainBaseLineCalculationDetails(int isector) const
{
	std::map<int,std::string>::const_iterator it = 
			fGainBaseLineCalculationDetails.find(isector);
	if ( it != fGainBaseLineCalculationDetails.end() )
	{
		return it->second.c_str();
	}
	return "";
}

//_____________________________________________________________________________
void
emcCalibrationDataHelper::initAll(void)
{
  cout << "emcCalibrationDataHelper::initAll : using TimeStamp="
       << fTimeStamp << endl;

#ifdef DEBUG
  TBenchmark* g = new TBenchmark;
#endif

  for ( size_t i = 0; i < fKnownFlavours.size(); ++i )
    {
      Flavour& f = fKnownFlavours[i];

#ifdef DEBUG
      std::ostringstream name;
      name << gSystem->BaseName(__FILE__) << ":" << __LINE__ 
	   << ":Collect time for " << f.name();
      g->Start(name.str().c_str());
#endif

      Flavour::const_iterator it;

      size_t n = 0;
      for ( it = f.begin(); it != f.end(); ++it )
        {
          const emcCalFEM* calfem =
            getCalibration(*it, f.name().c_str());
          assert(calfem != 0);
	  ++n;
        }
#ifdef DEBUG
      g->Show(name.str().c_str());
#endif
    }
  initECalAtT0(true);

  for ( size_t is = 0; is < 8; ++is ) 
    {
      if ( fFemList->hasSector(EmcIndexer::EmcSectorId(is)) )
	{
	  getCalibrationData("TofSectorOffset", is);
	}
    }
}

//_____________________________________________________________________________
void
emcCalibrationDataHelper::initECalAtT0(bool normalizationON)
{
  size_t ntowers = 172 * 144;

  fECalAtT0.clear();
  fECalAtT0.resize(ntowers);

  // Populate the fECalAtT0 array from sectors initial calibrations

  for ( size_t item = 0; item < ntowers; ++item )
    {
      int sn, ist;

      EmcIndexer::iPXiSiST(item, sn, ist);

      if ( fFemList->hasSector(EmcIndexer::EmcSectorId(sn)) )
        {

          float encal, norm0, one;

          const emcCalibrationData* sector = getCalibrationData("IniCal", sn);
          assert(sector != 0);

          if ( sn < 6 )
            {
              encal = sector->GetValue(ist, 0);         
              norm0 = sector->GetValue(ist, 1);
              one = sector->GetValue(ist, 2);
	      assert( (fRunNumber>50000 && one == 1.0) || // Run2->
		      (one==0.0)); // Up to Run2
              fECalAtT0[item] = encal * ( normalizationON ? norm0 : 1. ) ;
            }
          else
            {
              encal = sector->GetValue(ist, 0) *
		sector->GetValue(ist, 1) *
		sector->GetValue(ist, 2);
              fECalAtT0[item] = encal;
            }
        }
    }
}

//_____________________________________________________________________________
void
emcCalibrationDataHelper::initGainBaseLine(int isector,
					   const char* details)
{
  float x;
  float sigma;
  float skewness;
  float kurtosis;

  bool ok = emcGainBaseLineCalculator::getBaseLine(this,
						   isector,details,
						   x,sigma,skewness,kurtosis);

  if ( ok )
    {
      fGainBaseLine[isector] = BaseLine(x, sqrt(sigma), skewness, kurtosis); 
    }
  else
    {
      std::cerr << "emcCalibrationDataHelper::initGainBaseLine : "
		<< "something went wrong."
		<< "Returning default value of 1.0"
		<< std::endl;
      fGainBaseLine[isector] = BaseLine(1, 0, 0, 0);
    }
  fGainBaseLineCalculationDetails[isector] = details;
}

//_____________________________________________________________________________
void
emcCalibrationDataHelper::patch(emcCalFEM& calfem, 
				const std::string& how)
{
  // ok, this is not the best flexible code ever written...
  // it does its job, it was written quickly and required
  // minimum amount of code changes...
  // But please be my guest if you want to improve it...

  std::string::size_type p1 = how.find_first_of(':');
  std::string::size_type p2 = how.find_last_of(':');

  if ( p1 == std::string::npos || p2 == std::string::npos )
    {
      std::cerr << __FILE__ << ":" << __LINE__ << " ERROR ! how="
		<< how << " is not correct ! Patch inactive"
		<< std::endl;      
      return;
    }

  std::string what = how.substr(0,p1);

  if ( what != "BLR" )
    {
       std::cerr << __FILE__ << ":" << __LINE__ << " ERROR ! how="
		<< how << " not known to me! Patch inactive"
		<< std::endl;     
       return;
    }

  std::string details = how.substr(p1+1);

  emcTracedFEM* gains = dynamic_cast<emcTracedFEM*>(&calfem);

  int isector,ism;
  EmcIndexer::PXSM144_iSiSM144(gains->AbsolutePosition(),isector,ism);
   
  float factor = 1.0/getGainBaseLine(isector,"value",details.c_str(),true);

  for ( size_t ichannel = 0; ichannel < gains->size(); ++ichannel )
    {
      gains->FirstItem(ichannel);
      emcTracedValue* tv;
      while ( ( tv = gains->NextItem() ) )
	{
	  tv->Set(tv->GetX(),tv->GetConstant()*factor,
		  tv->GetSlope()*factor,
		  tv->isConstant());
	}      
    }
}

//_____________________________________________________________________________
emcManageable::EStorage
emcCalibrationDataHelper::source(const string& what)
{
  map<string, emcManageable::EStorage>::const_iterator it =
    fSources.find(what);
  if ( it != fSources.end() )
    {
      return it->second;
    }
  else
    {
      return emcManageable::kNone;
    }
}

//_____________________________________________________________________________
bool
emcCalibrationDataHelper::setSource(const string& what, 
				    emcManageable::EStorage datasource)
{
  if ( fInitAll )
    {
      std::cerr << __FILE__ << ":" << __LINE__ 
		<< " Can no longer change data source now, as "
		<< " initall constructor argument was used."
		<< std::endl;
      return false;
    }

  map<string, emcManageable::EStorage>::iterator it =
    fSources.find(what);
  if ( it != fSources.end() )
    {
      emcManageable::EStorage old = it->second;
      it->second = datasource;
      std::cout << __FILE__ << ":" << __LINE__ << " Changed " << what
		<< " data source from " 
		<< emcManageable::GetStorageName(old)
		<< " to " 
		<< emcManageable::GetStorageName(datasource)
		<< std::endl;
      return true;
    }
  else
    {
      if ( what == "QAs" || what == "RejectList" )
	{
	  // those 2 are not dealt with by this object, but
	  // by emcBadModules class.
	  // Nevertheless, do not clutter standard output
	  // with a warning about that.
	  return false;
	}
      else
	{
	  // this case really warrants a message on the standard error.
	  std::cerr << __FILE__ << ":" << __LINE__ << " Unknown calibration type : "
		    << what
		    << std::endl;
	  return false;
	}
    }
}








