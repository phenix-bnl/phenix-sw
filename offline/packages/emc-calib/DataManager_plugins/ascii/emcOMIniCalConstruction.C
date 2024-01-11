#include <emcCalibrationData.h>
#include <emcObjectManagerRegister.h>
#include <emcRawDataAccessor.h>
#include <EmcStaticData.h>
#include <EmcIndexer.h>
#include <EmcSector.h>

#include <emcObjectManager.h>

#include <cassert>
/** DM plugin to read emcCalibrationData (kIniCal) from construction DB. 
    @ingroup dmplugins
*/

class emcOMIniCalConstruction : public emcObjectManager
{
 public:
  emcOMIniCalConstruction(const char* name="", const char* title="");
  virtual ~emcOMIniCalConstruction();

  virtual bool CanCollect(const emcManageable&) const { return false; }

  virtual bool CanRead(const emcManageable& object) const;

  virtual bool CanWrite(const emcManageable&) const { return false; }

  virtual emcManageable* Collect(const emcManageable&,
				 const PHTimeStamp&) { return 0; }

  bool GetTypeAndNumber(int id, size_t& type, 
                        size_t& number) const;

  int GetID(size_t type, size_t number);

  using emcObjectManager::Read;

  virtual bool Read(emcManageable& object,
                    const PHTimeStamp& time_stamp,
                    int id);

  virtual void Reset(void);

  virtual bool Write(const emcManageable&, const PHTimeStamp&, int)
  { return false; }
};

using namespace std;

namespace
{
  emcOMIniCalConstruction gemcOMIniCalConstruction(
			       "Construction DB:emcOMIniCalConstruction",
			       "Very specific OM to read inical PbSc calibrations from Construction DB");
}

//_____________________________________________________________________________
  emcOMIniCalConstruction::emcOMIniCalConstruction(const char* name, const char* title) : emcObjectManager(name,title)
{
}  

//_____________________________________________________________________________
emcOMIniCalConstruction::~emcOMIniCalConstruction()
{
}

//_____________________________________________________________________________
bool 
emcOMIniCalConstruction::CanRead(const emcManageable& object) const
{
  bool rv = false;
  const emcManageable* object_ptr = &object;
  const emcCalibrationData* test = 
    dynamic_cast<const emcCalibrationData*>(object_ptr);
  
  if (test && object.GetSource() == emcManageable::kDB_Construction) 
    {
      rv = true;
    }

  return rv;
}

//_____________________________________________________________________________
bool 
emcOMIniCalConstruction::GetTypeAndNumber(int id,
					  size_t& type, size_t& number) const
{
  // Get the type and the numbe from the bankID
  // return false if the bankID is not valid.
  // FIXME: no test is done for the moment.

  type = ( id & 0xFF000000 ) >> 24;
  number = ( id & 0xFFFFFF );

  return true;
}

//_____________________________________________________________________________
int
emcOMIniCalConstruction::GetID(size_t type, size_t number)
{
  // Get the bankID from the type and the number
  // return false if type and/or number are not valid
  // FIXME: no test is done for the moment.

  int id; // assumes int = 4 bytes !

  id = ( ( type << 24 ) & ( 0xFF000000 ) ) + ( number & 0xFFFFFF );

  return id;
}

//_____________________________________________________________________________
bool
emcOMIniCalConstruction::Read(emcManageable& object,
			      const PHTimeStamp& /*time_stamp*/,
			      int id)
{
  emcCalibrationData& cal = static_cast<emcCalibrationData&>(object);
  
  cal.Reset();

  size_t type;
  size_t number;

  if ( id >= 0 )
    {
      GetTypeAndNumber(id,type,number);
      if (type!=static_cast<size_t>(cal.GetType()) ||
	  number!=cal.GetNumber())
	{
	  cerr << "<W> emcOMIniCalConstruction::Read : "
	       << "the 3rd parameter is not"
	       << " consistent with the object you give. Might well be what "
	       << " you want, but hum, are you really sure you know what "
	       << " you are doing here ?! "
	       << endl;
	}
    }
  else
    {
      type = cal.GetType();
      number = cal.GetNumber();
      id = GetID(type,number);
    }

  cout << "emcOMIniCalConstruction::Read !!! (" << id << ")" << endl;

  EmcStaticData* sd = EmcStaticData::buildEmcStaticData() ;
  
  if ( !sd ) 
    {
      cerr << "<E> emcOMIniCalConstruction::Read : "
	   << "cannot build EmcStaticData ?!"
	   << endl;
      return false;
    }

  EmcSector * sector = sd->getSector(number) ;

  if( !sector ) 
    {
      sd ->buildEmcSector( EmcIndexer::EmcSectorId(number));
      sector = sd->getSector(number) ;
    }
 
  assert(sector->IsOK()) ;

  int length = 2592; // # of PbSc towers per PbSc sector
  if ( number > 5 ) 
    {
      length = 4608; // # of PbGl towers per PbGl sector
    }

  cal.SetTypeAndSize(static_cast<emcCalibrationData::EType>(type),length);
   
  for ( int ist = 0; ist < length; ++ist ) 
    {
      float one,two,three;
      float error = 0.0;
      sector->GetEnergyCalibration(ist,one,two,three);
      cal.Set(ist,one,error,0);
      cal.Set(ist,two,error,1);
      cal.Set(ist,three,error,2);
    }

  return true;
}

//_____________________________________________________________________________
void
emcOMIniCalConstruction::Reset()
{
}

