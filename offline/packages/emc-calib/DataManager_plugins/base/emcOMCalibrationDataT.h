#ifndef __EMCOMCALIBRATIONDATAT_H__
#define __EMCOMCALIBRATIONDATAT_H__

#include <emcManageable.h>
#include <emcObjectManager.h>
#include <map>
#include <vector>
#include <cassert>

class emcCalibrationData;
class PdbBankID;

/**
   (Template) Plugin class for emcCalibrationData objects.
@ingroup dmplugins
 */

template <class BM>
class emcOMCalibrationDataT : public emcObjectManager
{
public:

  emcOMCalibrationDataT(const char* name = "", const char* title = "");
  virtual ~emcOMCalibrationDataT();

  virtual bool CanCollect(const emcManageable& object) const;

  virtual bool CanRead(const emcManageable& object) const;

  virtual bool CanWrite(const emcManageable& object) const;

  virtual emcManageable* Collect(const emcManageable& object,
				 const PHTimeStamp& when);

  bool GetBankID(size_t type, size_t number, PdbBankID& bankID) const;
  bool GetTypeAndNumber(const PdbBankID& bankID, size_t& type,
			size_t& number) const;

  using emcObjectManager::Read;

  virtual bool Read(emcManageable& object,
		    const PHTimeStamp& time_stamp,
		    int id);

  virtual void Reset(void);

  virtual bool Write(const emcManageable& object,
		     const PHTimeStamp& tdummy,
		     int dummy = -1);

private:
  emcCalibrationData* GetCalibrationData(const emcCalibrationData& cal);
  emcCalibrationData* AllocateCalibrationData(const emcCalibrationData& cal);
  void FreeCalibrationData(const emcCalibrationData& cal);
  bool ReadFromFile(const PdbBankID& bankID, emcCalibrationData& cal);

private:

  std::map<int, std::vector<emcCalibrationData*> * > fCalibrationMap;

private:
  class changeName
  {
  public:
    changeName(const char* name)
    {
      name_ = emcManageable::GetStorageName(BM::storage());
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

  BM* fBM;
};


//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include <emcDataManager.h>
#include <emcCalibrationData.h>
#include <PdbEmcLCTof.hh>
#include <PdbEmcTofSectorOffset.hh>
#include <PdbCoordinate.hh>
#include <PdbBankList.hh>
#include <PdbBankID.hh>
#include <PdbBankListIterator.hh>
#include <PdbApplication.hh>
#include <PdbBankManager.hh>
#include <Pdb.hh>
#include <sstream>
#include <fstream>
#include <EmcIndexer.h>
#include <cstdlib>
#include <vector>
#include <cmath>

//_____________________________________________________________________________
template <class BM>
emcOMCalibrationDataT<BM>::emcOMCalibrationDataT(const char* name,
						 const char* title)
  : emcObjectManager(changeName(name).c_str(), title),
    fBM(new BM)
{}

//_____________________________________________________________________________
template <class BM>
emcOMCalibrationDataT<BM>::~emcOMCalibrationDataT()
{
  Reset();
  delete fBM;
}

//_____________________________________________________________________________
template <class BM>
emcCalibrationData*
emcOMCalibrationDataT<BM>::AllocateCalibrationData(const emcCalibrationData& cal)
{
  emcCalibrationData::EType type = cal.GetType();
  size_t number = cal.GetNumber();

  if (fCalibrationMap.find(type) == fCalibrationMap.end())
    {
      // gee, we don't know this type yet !
      fCalibrationMap[type] =
        new std::vector<emcCalibrationData*>(cal.GetRange());
      std::vector<emcCalibrationData*> * vec = fCalibrationMap[type];
      size_t i;
      for (i = 0;i < vec->size();i++)
        {
          (*vec)[i] = 0;
        }
    }

  std::vector<emcCalibrationData*> * vec = fCalibrationMap[type];
  assert(number < vec->size());
  delete (*vec)[number];
  (*vec)[number] = new emcCalibrationData(type, number);
  return (*vec)[number];
}

//_____________________________________________________________________________
template <class BM>
void
emcOMCalibrationDataT<BM>::FreeCalibrationData(const emcCalibrationData& cal)
{
  emcCalibrationData::EType type = cal.GetType();
  size_t number = cal.GetNumber();

  if (fCalibrationMap.find(type) != fCalibrationMap.end())
    {
      std::vector<emcCalibrationData*> * vec = fCalibrationMap[type];
      delete (*vec)[number];
      (*vec)[number] = 0;
    }
}


//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::CanCollect(const emcManageable& object) const
  {
    return CanRead(object);
  }

//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != fBM->storage() )
    {
      return false;
    }
  
  const emcManageable* object_ptr = &object;
  const emcCalibrationData* test =
    dynamic_cast<const emcCalibrationData*>(object_ptr);
  
  if (test)
    {
      return true;
    }
  
  return false;
}

//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != fBM->storage() )
    {
      return false;
    }
  
  const emcManageable* object_ptr = &object;
  const emcCalibrationData* test =
    dynamic_cast<const emcCalibrationData*>(object_ptr);
  
  if (test)
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
template <class BM>
emcManageable*
emcOMCalibrationDataT<BM>::Collect(const emcManageable& object,
                                   const PHTimeStamp& when)
{
  const emcCalibrationData& cal =
    static_cast<const emcCalibrationData&>(object);

  emcCalibrationData* calibdata;

  calibdata = GetCalibrationData(cal);

  if (calibdata)
    {
      // we already have this one in memory.
      if (calibdata->IsValid(when))
        {
          // it's still valid, so we return it.
          return calibdata;
        }
    }
  else
    {
      calibdata = AllocateCalibrationData(cal);
    }

  emcManageable::EStorage data_source = object.GetSource();

  if ( DM()->GetVerboseLevel() )
    {
      std::cout << "<I-EMCAL> Fetching " << object.GetCategory() << " from "
		<< emcManageable::GetStorageName(data_source) << std::endl;
    }

  calibdata->SetSource(cal.GetSource());

  PdbBankID id;
  GetBankID(cal.GetType(), cal.GetNumber(), id);

  bool ok = Read(*calibdata, when, id.getInternalValue());

  if (ok)
    {
      return calibdata;
    }
  else
    {
      if ( DM()->GetVerboseLevel() )
        {
          std::cerr << "<E-EMCAL> emcOMCalibrationT<"
		    << fBM->name()
		    << ">::Collect failed for "
		    << object.GetCategory() << std::endl;
        }
      FreeCalibrationData(cal);
      return 0;
    }
}

//_____________________________________________________________________________
template <class BM>
emcCalibrationData*
emcOMCalibrationDataT<BM>::GetCalibrationData(const emcCalibrationData& cal)
{
  emcCalibrationData::EType type = cal.GetType();
  size_t number = cal.GetNumber();
  emcCalibrationData* rv = 0;

  if (fCalibrationMap.find(type) != fCalibrationMap.end())
    {
      // we already know about this type
      std::vector<emcCalibrationData*> *vec = fCalibrationMap[type];
      assert(vec != 0);
      assert(number < vec->size());
      return (*vec)[number];
    }

  return rv;
}

//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::GetBankID(size_t type, size_t number,
				     PdbBankID& bankID) const
{
  // Get the bankID from the type and the number
  // return false if type and/or number are not valid
  // FIXME: no test is done for the moment.

  if ( type == emcCalibrationData::kTofSectorOffset )
    {
      bankID.setInternalValue(number);
    }
  else
    {
      int id; // assumes int = 4 bytes !

      id = ( ( type << 24 ) & ( 0xFF000000 ) ) + ( number & 0xFFFFFF );
      bankID.setInternalValue(id);
    }

  return true;
}

//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::GetTypeAndNumber(const PdbBankID& bankID,
					    size_t& type, size_t& number) const
{
  // Get the type and the numbe from the bankID
  // return false if the bankID is not valid.
  // FIXME: no test is done for the moment.

  int id = bankID.getInternalValue();
  type = ( id & 0xFF000000 ) >> 24;
  number = ( id & 0xFFFFFF );

  return true;
}

//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::Read(emcManageable& object,
                                const PHTimeStamp& time_stamp,
                                int id)
{
  bool kIsRead = false;
  emcCalibrationData& cal = static_cast<emcCalibrationData&>(object);

  size_t type;
  size_t number;

  PdbBankID bankID;

  if ( id >= 0 )
    {
      bankID.setInternalValue(id);
      GetTypeAndNumber(bankID, type, number);
      if (type != static_cast<size_t>(cal.GetType()) ||
          number != cal.GetNumber())
        {
          std::cerr << "<W> emcOMCalibrationDataT<" << BM::name()
		    << ">::Read : the 3rd parameter is not"
		    << " consistent with the object you give. "
		    << "Might well be what "
		    << " you want, but hum, are you really sure you know what "
		    << " you are doing here ?! "
		    << std::endl;
        }
    }
  else
    {
      type = cal.GetType();
      number = cal.GetNumber();
      GetBankID(type, number, bankID);
    }
  
  // Try to open a read transaction
  PdbStatus status = fBM->DBApplication()->startRead();

  if (status)
    {
      if (DM()->GetVerboseLevel())
        {
          std::cout << "<I> emcOMCalibrationDataT<" << BM::name()
		    << ">::Read : I'm going to read the following"
		    << " emcCalibrationData : ";
          cal.Print();
        }

      // Convert here from calBank to emcCalibration (cal) object

      std::string pdb_classname = "PdbEmcLCTofBank";
      if (type == emcCalibrationData::kIniCal)
        {
          pdb_classname = "PdbCoordinateBank";
        }
      else if (type == emcCalibrationData::kTofSectorOffset)
        {
          pdb_classname = "PdbEmcTofSectorOffsetBank";
        }
      std::string basename = "calib.emc.";
      basename += cal.GetCategory();

      PdbCalBank* emcBank =
        fBM->BankManager()->fetchBank(pdb_classname.c_str(),
				      bankID,
				      basename.c_str(),
				      const_cast<PHTimeStamp&>(time_stamp));

      if (!emcBank)
        {        
	  if ( cal.GetType() == emcCalibrationData::kTofSectorOffset )
	    {
	      // we're allowed to fail.
	      // In that case, return a default empty object,
	      // where offsets are 0 simply.
	      
	      std::cout << "<W-EMCAL> emcOMCalibrationDataT<" << BM::name()
			<< ">::Read : "
			<< "cannot fetch bank (flavour = "
			<< cal.GetCategory() << ",type=0x"
			<< std::hex << static_cast<int>(type)
			<< std::dec
			<< ",number=" << number
			<< ") at time " << time_stamp
			<< " in DB(s) " << basename << " : " 
			<< "Returning default object instead"
			<< std::endl;

	      cal.SetTypeAndSize(static_cast<emcCalibrationData::EType>(type),
				 1);
	      cal.Set(0,0.0,0.0,0);
	      cal.Set(0,0.0,0.0,1);
	      cal.Set(0,0.0,0.0,2);
	      cal.Set(0,0.0,0.0,3);
	      cal.Set(0,0.0,0.0,4);
	      kIsRead = true;
	    }
	  else
	    {
	      std::cerr << "<E-EMCAL> emcOMCalibrationDataT<" << BM::name()
			<< ">::Read : "
			<< "cannot fetch bank (flavour = "
			<< cal.GetCategory() << ",type= "
			<< std::hex << static_cast<int>(type) << std::dec
			<< ",number=" << number
			<< ") at time " << time_stamp
			<< " in DB(s) " << basename << "*" << std::endl;
	    }
        }
      else
        {
          kIsRead = true;

          size_t i;

          // warning : the type MUST be set before the size,
          // as it decides on the number of dimensions for the internals
          // of cal.

          switch (cal.GetType())
            {
            case emcCalibrationData::kIniCal:
              {
                cal.SetTypeAndSize(static_cast<emcCalibrationData::EType>(type),
                                   emcBank->getLength());
                PdbCoordinate* pdb;
                for ( i = 0; i < cal.GetSize(); i++ )
                  {
                    pdb = dynamic_cast<PdbCoordinate*>(&(emcBank->getEntry(i)));
                    size_t j;
                    for ( j = 0; j < cal.GetDimension(); j++ )
                      {
                        cal.Set(i, pdb->getParameter(j), pdb->getParError(j), j);
                      }
                  }
              }
	    break;
            case emcCalibrationData::kTofSectorOffset:
              {
                assert(emcBank->getLength() == 1);
                cal.SetTypeAndSize(static_cast<emcCalibrationData::EType>(type),
                                   emcBank->getLength());
                PdbEmcTofSectorOffset* pdb = 
		  dynamic_cast<PdbEmcTofSectorOffset*>(&(emcBank->getEntry(0)));

                assert(cal.GetSize() == 1);

		cal.Set(0,pdb->runnumber()*1.0,pdb->numberOfEvents()*1.0,0);
		cal.Set(0,pdb->peak(),pdb->width(),1);
		cal.Set(0,pdb->gausPeak(),pdb->gausWidth(),2);
		cal.Set(0,pdb->BBCT0(),pdb->BBCT0rms(),3);
		cal.Set(0,pdb->TOFT0(),pdb->TOFT0rms(),4);
              }
	    break;
            default:
              {
                cal.SetTypeAndSize(static_cast<emcCalibrationData::EType>(type),
                                   emcBank->getLength());
                PdbEmcLCTof* pdb;
                for ( i = 0; i < cal.GetSize(); i++ )
                  {
                    pdb = dynamic_cast<PdbEmcLCTof*>(&(emcBank->getEntry(i)));
                    float value;
                    float error;
                    pdb->GetLCTofs(value, error);
                    cal.Set(i, value, error);
                  }
              }
            }


          cal.SetNumber(number);
          cal.SetValidityPeriod(emcBank->getStartValTime(),
                                emcBank->getEndValTime());

        }
      bool commit = fBM->DBApplication()->commit();
      kIsRead = commit && kIsRead;
      delete emcBank;
    } // if (status)

  if (!kIsRead)
    {
      cal.Reset();
    }
  else
    {
      if (DM()->GetVerboseLevel())
        {
          std::cout << "<I> emcOMCalibrationDataT<" << BM::name()
		    << ">::Read : I've read the following "
		    << "emcCalibrationData from "
		    << emcManageable::GetStorageName(cal.GetSource());
          cal.Print();
        }
    }

  return kIsRead;
}

//_____________________________________________________________________________
template <class BM>
void
emcOMCalibrationDataT<BM>::Reset(void)
{
  std::map<int, std::vector<emcCalibrationData*> * >::iterator p;

  for (p = fCalibrationMap.begin();p != fCalibrationMap.end();p++)
    {
      std::vector<emcCalibrationData*> *vec = p->second;
      size_t i;
      for (i = 0;i < vec->size();i++)
        {
          delete (*vec)[i];
        }
      delete vec;
    }
  fCalibrationMap.erase(fCalibrationMap.begin(), fCalibrationMap.end());
}

//_____________________________________________________________________________
template <class BM>
bool
emcOMCalibrationDataT<BM>::Write(const emcManageable& object,
                                 const PHTimeStamp&, int)
{
  bool kWritten = false;

  const emcCalibrationData& ccal =
    static_cast<const emcCalibrationData&>(object);
  emcCalibrationData& cal =
    const_cast<emcCalibrationData&>(ccal);

  // Start a write transaction
  PdbStatus status = fBM->DBApplication()->startUpdate();

  if (!status)
    {
      if ( DM()->GetVerboseLevel() )
        {
          std::cerr << "<E-EMCAL> emcOMCalibrationDataT<" << BM::name()
		    << ">::Write : "
		    << "Cannot open a write transaction" << std::endl;
        }
      fBM->DBApplication()->abort();
      return false;
    }

  PdbCalBank* emcBank = 0;
  PHTimeStamp begin, end;
  PdbBankID bankID;
  GetBankID(cal.GetType(), cal.GetNumber(), bankID);

  std::string pdb_classname = "PdbEmcLCTofBank";
  if (cal.GetType() == emcCalibrationData::kIniCal)
    {
      pdb_classname = "PdbCoordinateBank";
    }
  else if (cal.GetType() == emcCalibrationData::kTofSectorOffset)
    {
      pdb_classname = "PdbEmcTofSectorOffsetBank";
    }
  else
    {
      std::cerr << __FILE__ << ":" << __LINE__
		<< " Do not know how to handle this type"
		<< std::endl;
      return false;
    }

  std::string description = "Written by emcOMCalibrationData (flavour ";
  description += cal.GetCategory();
  description += ").";
  std::string basename = "calib.emc.";
  basename += cal.GetCategory();

  begin = cal.GetStartValTime();
  end = cal.GetEndValTime();


  emcBank = fBM->BankManager()->createBank(pdb_classname.c_str(),
					   bankID,
					   description.c_str(),
					   begin, end,
					   basename.c_str());

  assert(emcBank != 0);

  // We then fill the PdbCalBank from the values in cal.
  size_t i;

  emcBank->setLength(cal.GetSize());

  switch (cal.GetType())
    {
    case emcCalibrationData::kIniCal:
      {
        PdbCoordinate* pdb;

        for ( i = 0; i < emcBank->getLength(); i++ )
          {
            pdb = (PdbCoordinate*) & (emcBank->getEntry(i));
            size_t j;
            for ( j = 0; j < cal.GetDimension(); j++ )
              {
                pdb->setParameter(j, cal.GetValue(i, j));
                pdb->setParError(j, cal.GetError(i, j));
              }
          }
      }
      break;
    case emcCalibrationData::kTofSectorOffset:
      {
	assert(emcBank->getLength() == 1);
	PdbEmcTofSectorOffset* pdb = 
	  (PdbEmcTofSectorOffset*) & (emcBank->getEntry(0));
	int runnumber = static_cast<int>(floor(cal.GetValue(0,0)));
	int nevents = static_cast<int>(floor(cal.GetError(0,0)));
	pdb->setProcessInfo(runnumber,nevents);
	pdb->setPeak(cal.GetValue(0,1),cal.GetError(0,1));
	pdb->setGausPeak(cal.GetValue(0,2),cal.GetError(0,2));
	pdb->setBBC(cal.GetValue(0,3),cal.GetError(0,3));
	pdb->setTOF(cal.GetValue(0,4),cal.GetError(0,4));
	pdb->print();
	break;
      }
    default:
      {
        PdbEmcLCTof* pdb;

        for ( i = 0; i < emcBank->getLength(); i++ )
          {
            pdb = (PdbEmcLCTof*) & (emcBank->getEntry(i));
            assert(pdb != 0);
            pdb->SetLCTofs(cal.GetValue(i), cal.GetError(i));
          }
      }
    }

  status = fBM->DBApplication()->commit();
  if (status)
    {
      kWritten = true;
    }
  else
    {
      std::cerr << "<E> emcOMCalibrationDataT<" << BM::name()
		<< ">::Write : commit failed ?!" << std::endl;
      kWritten = false;
    }

  delete emcBank;

  return kWritten;
}


#endif
