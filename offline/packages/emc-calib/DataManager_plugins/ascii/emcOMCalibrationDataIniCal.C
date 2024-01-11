#include "emcObjectManager.h"
#include "emcManageable.h"
#include <string>

/** DM plugin to R/W emcCalibrationData (flavour IniCal) from/to ASCII files. 
    see CanRead and CanWrite for exact emcCalibrationData's flavor supported.
    @ingroup dmplugins
 */

class emcOMCalibrationDataIniCal : public emcObjectManager
{
public:
  
  emcOMCalibrationDataIniCal(const char* name="", const char* title="");
  ~emcOMCalibrationDataIniCal();

  bool CanCollect(const emcManageable&) const
  { 
    return false;
  }

  /// We can read objects of type \c emcCalibrationData (flavour kIniCal only).
  bool CanRead(const emcManageable& object) const;

  /// We can write objects of type \c emcCalibrationData (flavour kIniCal only).
  bool CanWrite(const emcManageable& object) const;

  using emcObjectManager::Read;

  /** Read an emcCalibrationData object from ASCII file. 
   *  The file read is named [topdir]/IniCal/[SECTOR].INICAL, where:
   *  - topdir is emcDataManager::GetSourceDir(),
   *  - SECTOR (=W0,W1,etc...) is obtained through EmcIndexer::SectorId(number),
   *  where number is emcCalibrationData::GetNumber().
   *
   *  The format of each line of the file is:
   *
   *  FEM FEMCHANNEL C0 C1 C2
   *  - where (FEM,FEMCHANNEL) indentifies the tower, and (C0,C1,C2) are
   *  initial calibration values to be used to convert ADC values into GeV
   *  (usually by doing ADC *= C0*C1*C2).
   *
   *  @sa EmcIndexer class for numbering stuff.
   *  @ingroup dmasciifiles
   */
  bool Read(emcManageable& object,
	    const PHTimeStamp& time_stamp,
	    int id);
  
  void Reset() {}

  /** Write an emcCalibrationData object to ASCII file.
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
#include "dirfilemanip.h"
#include <fstream>
#include <iostream>
#include <cassert>
#include <sstream>

//_____________________________________________________________________________
emcOMCalibrationDataIniCal::emcOMCalibrationDataIniCal(const char* name,
					   const char* title)
  : emcObjectManager(changeName(name).c_str(),title)
{
}

//_____________________________________________________________________________
emcOMCalibrationDataIniCal::~emcOMCalibrationDataIniCal()
{
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataIniCal::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const emcCalibrationData* test = 
    dynamic_cast<const emcCalibrationData*>(&object);
  
  if ( test && test->GetType() == emcCalibrationData::kIniCal )
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataIniCal::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != emcManageable::kFile_ASCII )
    {
      return false;
    }
  const emcCalibrationData* test = 
    dynamic_cast<const emcCalibrationData*>(&object);
  
  if ( test && test->GetType() == emcCalibrationData::kIniCal )
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
bool
emcOMCalibrationDataIniCal::Read(emcManageable& object,
			   const PHTimeStamp& /*time_stamp*/,
			   int /*id*/)
{ 
  emcCalibrationData& cal = static_cast<emcCalibrationData&>(object);

  size_t length = 2592; // # of PbSc towers per PbSc sector
  if ( cal.GetNumber() > 5 ) 
    {
      length = 4608; // # of PbGl towers per PbGl sector
    }

  cal.SetTypeAndSize(cal.GetType(),length);

  std::ostringstream str;

  emcDataManager* dm = emcDataManager::GetInstance();

  str << dm->GetSourceDir() << "/IniCal/" 
      << EmcIndexer::EmcSectorId(cal.GetNumber())
      << ".INICAL";

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
      return false;
    }
  int fem, femchannel;
  float c0,g0,one;

  int sectorNumber = cal.GetNumber();

  while ( in >> fem >> femchannel >> c0 >> g0 >> one )
    {
      int towerid = EmcIndexer::PXSM144iCH_iPX(fem,femchannel);
      int ist;
      EmcIndexer::iPXiSiST(towerid,sectorNumber,ist);
      cal.Set(ist,c0,0,0);
      cal.Set(ist,g0,0,1);
      cal.Set(ist,one,0,2);
    }

  return true;
}

//_____________________________________________________________________________
bool 
emcOMCalibrationDataIniCal::Write(const emcManageable& object, 
			    const PHTimeStamp&,int)
{
  const emcCalibrationData& cal = 
    static_cast<const emcCalibrationData&>(object);
  
  std::ostringstream dirname;

  emcDataManager* dm = emcDataManager::GetInstance();

  dirname << dm->GetDestinationDir() << "/IniCal";

  if (!createDirectory(dirname.str()))
    {
      return false;
    }
  
  std::ostringstream filename;

  int isector = cal.GetNumber();

  filename << expand(dirname.str()) << "/"
	   << EmcIndexer::EmcSectorId(isector)
	   << ".INICAL";

  std::ofstream out(filename.str().c_str());

  if (!out)
    {
      return false;
    }
 
  for ( size_t i = 0; i < cal.GetSize(); ++i )
    {
      // i = ist = sector tower number, i.e. within the sector.
      int towerid = EmcIndexer::iSiSTiPX(isector,i);
      int fem, femchannel;
      EmcIndexer::PXPXSM144CH(towerid,fem,femchannel);
      out << fem << " " << femchannel << " ";
      for ( size_t j = 0; j < cal.GetDimension(); ++j )
	{
	  out << cal.GetValue(i,j) << " ";
	}
      out << std::endl;
    }

  out.close();

  return true;      
}

emcOMCalibrationDataIniCal gemcOMCalibrationDataIniCal("emcOMCalibrationDataIniCal","Handle IniCal (absolute EMCAL calibration)");
