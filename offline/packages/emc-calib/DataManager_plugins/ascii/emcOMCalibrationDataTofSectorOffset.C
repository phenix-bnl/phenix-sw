#include "emcObjectManager.h"
#include "emcManageable.h"
#include <string>

/** DM plugin to R/W emcCalibrationData (flavour T0Sector) 
    from/to ASCII files. 
    see CanRead and CanWrite for exact emcCalibrationData's flavor supported.
    @ingroup dmplugins
 */

class emcOMCalibrationDataTofSectorOffset : public emcObjectManager
{
public:
  
  emcOMCalibrationDataTofSectorOffset(const char* name="", const char* title="");
  ~emcOMCalibrationDataTofSectorOffset();

  bool CanCollect(const emcManageable&) const
  { 
    return false;
  }

  /** We can read objects of type \c emcCalibrationData 
      (flavour T0Sector only).
  */
  bool CanRead(const emcManageable& object) const;

  /** We can write objects of type \c emcCalibrationData 
      (flavour T0Sector only).
  */
  bool CanWrite(const emcManageable& object) const;

  using emcObjectManager::Read;

  /// Not implemented. Use Read(object,runnumber) instead.
  bool Read(emcManageable& object,
	    const PHTimeStamp& time_stamp,
	    int id);
  
  /** Read an emcCalibrationData object from ASCII file. 
   *  The file read is named [topdir]/ToF/[runnumber]/[SECTOR].TOF_OFFSET, 
   *  where:
   *  - topdir is emcDataManager::GetSourceDir(),
   *
   *  The format of the file is (i_=integer,f_=float) :
   *
   *  i_runnumber i_events_used
   *  f_peak f_width
   *  f_gaussian_peak f_gaussian_width
   *  f_bbct0 f_bbct0rms
   *  f_toft0 f_toft0rms
   *
   *  @ingroup dmasciifiles
   */
  bool Read(emcManageable& object, int runnumber);

  void Reset() {}

  /** Write an emcCalibrationData (flavour T0Sector) object to ASCII file.
      @sa Read
      @ingroup dmasciifiles
   */
  bool Write(const emcManageable&, const PHTimeStamp&,int);
  
private:
  
  class changeName
  {
  public:
    changeName(const char* name)
    {
      name_ = emcManageable::GetStorageName(emcManageable::kFile_ASCII);
      name_ += ":";
      name_ += name;
    }
    
    const char* c_str() const
    {
      return name_.c_str();
    }
    
  private:
    std::string name_;
  };
};

#include "emcCalibrationData.h"
#include "emcDataManager.h"
#include "EmcIndexer.h"
#include "asciitimestamp.h"
#include "dirfilemanip.h"
#include <fstream>
#include <iostream>
#include <cassert>
#include <sstream>

//_____________________________________________________________________________
emcOMCalibrationDataTofSectorOffset::emcOMCalibrationDataTofSectorOffset(const char* name,
					   const char* title)
  : emcObjectManager(changeName(name).c_str(),title)
{
}

//_____________________________________________________________________________
emcOMCalibrationDataTofSectorOffset::~emcOMCalibrationDataTofSectorOffset()
{
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataTofSectorOffset::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const emcCalibrationData* test = 
    dynamic_cast<const emcCalibrationData*>(&object);
  
  if ( test && test->GetType() == emcCalibrationData::kTofSectorOffset )
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataTofSectorOffset::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const emcCalibrationData* test = 
    dynamic_cast<const emcCalibrationData*>(&object);
  
  if ( test && test->GetType() == emcCalibrationData::kTofSectorOffset )
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataTofSectorOffset::Read(emcManageable& /*object*/,
					  const PHTimeStamp& /*time_stamp*/,
					  int /*id*/)
{
  std::cerr << __FILE__ << ":" << __LINE__ << " is not implemented for this "
	    << "object and data source="
	    << emcManageable::GetStorageName(emcManageable::kFile_ASCII)
	    << ". Please use Read(object,runnumber) instead"
	    << std::endl;
  return false;
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataTofSectorOffset::Read(emcManageable& object,
					  int runnumber)
{ 
  emcCalibrationData& cal = static_cast<emcCalibrationData&>(object);

  cal.SetTypeAndSize(cal.GetType(),1);

  std::ostringstream str;

  emcDataManager* dm = emcDataManager::GetInstance();

  str << dm->GetSourceDir() << "/ToF/" << std::setfill('0')
      << std::setw(10) << runnumber << "/"
      << EmcIndexer::EmcSectorId(cal.GetNumber())
      << ".TOF_OFFSET";

  if ( dm->GetVerboseLevel() ) 
    {
      std::cout << "<I> " << __FILE__ << ":" << __LINE__ 
		<< " reading from "
		<< str.str() << std::endl;
    }

  std::ifstream in(str.str().c_str());
  if ( !in ) 
    {    
      std::cerr << "<E> Cannot open file "
	   << str.str().c_str() << std::endl;
      std::cout << "Returning default object instead" << std::endl;
      cal.Set(0,0.0,0.0,0);
      cal.Set(0,0.0,0.0,1);
      cal.Set(0,0.0,0.0,2);
      cal.Set(0,0.0,0.0,3);
      cal.Set(0,0.0,0.0,4);
      return true;
    }

  int dim = 0;
  float a,b;

  while ( in >> a >> b )
    {
      cal.Set(0,a,b,dim);
      ++dim;
    }

  in.close();

  return true;
}

//_____________________________________________________________________________
bool 
emcOMCalibrationDataTofSectorOffset::Write(const emcManageable& object, 
			    const PHTimeStamp&,int)
{
  const emcCalibrationData& cal = 
    static_cast<const emcCalibrationData&>(object);

  // sanity checks, again
  if ( cal.GetType() != emcCalibrationData::kTofSectorOffset )
    {
      std::cerr << __FILE__ << ":" << __LINE__  << " cal is of wrong "
		<< "type!!! This should not happen as it should be "
		<< "checked by the CanRead/CanWrite methods" << std::endl;
      return false;
    }
  if ( !cal.GetSize() == 1 )
    {
      std::cerr << __FILE__ << ":" << __LINE__  << " cal is of wrong "
		<< " size. Should be 1." 
		<< std::endl;
      return false;
    }

  std::ostringstream dirname;

  emcDataManager* dm = emcDataManager::GetInstance();

  const int dim_run = 0;

  dirname << dm->GetDestinationDir() << "/ToF/" 
	  << std::setfill('0') << std::setw(10) << cal.GetValue(0,dim_run);

  if (!createDirectory(dirname.str()))
    {
      return false;
    }
  
  std::ostringstream filename;

  int isector = cal.GetNumber();

  filename << expand(dirname.str()) << "/" << EmcIndexer::EmcSectorId(isector)
	   << ".TOF_OFFSET";

  std::ofstream out(filename.str().c_str());

  if (!out)
    {
      return false;
    }

  for ( size_t i = 0; i < 5; ++i )
    {
      out << cal.GetValue(0,i) << " " << cal.GetError(0,i)
	  << std::endl;
    }

  out.close();

  return true;      
}

emcOMCalibrationDataTofSectorOffset gemcOMCalibrationDataTofSectorOffset("emcOMCalibrationDataTofSectorOffset","Handle Run-by-Run Sector T0 offsets");
