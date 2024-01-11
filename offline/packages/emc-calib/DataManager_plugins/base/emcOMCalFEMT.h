#ifndef __EMCOMCALFEMT_H__
#define __EMCOMCALFEMT_H__

#ifndef __EMCOBJECTMANAGER_H__
#include "emcObjectManager.h"
#endif
#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif
#ifndef __EMCDATAMANAGER_H__
#include "emcDataManager.h"
#endif
#ifndef __EMCOBJECTMANAGERREGISTER_H__
#include "emcObjectManagerRegister.h"
#endif
#ifndef __EMCDEFINES_H__
#include "emcDefines.h"
#endif
#ifndef __EMCCALFEM_H__
#include "emcCalFEM.h"
#endif

//#ifndef __EMCOMCALFEM_H__
//#include "emcOMCalFEM.h"
//#endif

#include "Pdb.hh"
#include "PdbApplication.hh"
#include "PdbBankManager.hh"
#include "PdbCalBank.hh"
#include <cassert>

/** Base class for all the plugins handling object deriving from emcCalFEM.
    It implements the logic common to all plugins dealing with daughter
    classes of emcCalFEM, for the Read and Write methods :

    a) Read. Reading of ONE FEM, from a given storage.

    b) Write. Make PdbCalBank from the object, and put it into the Objy FD.

    Plugins deriving from this one MUST implement the following methods:

    1) FromPdbCalBank(), which fills an emcCalFEM object from a PdbCalBank. 
    (Called by Read)

    2) ToPdbCalBank() : reverse operation of 1). 
    (Called by Write)

    3) GetPersistentClassName() : give the name of the low-level objy class
    the plugin is dealing with, i.e. one of the PdbEmc??? classes.

@ingroup dmplugins
*/

class emcOMCalFEM : public emcObjectManager
{
public:
  emcOMCalFEM(const char* name="", const char* title="")
    : emcObjectManager(name,title) {}
  virtual ~emcOMCalFEM() {}

  virtual void FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank) = 0;
};

template<class BM>
class emcOMCalFEMT : public emcOMCalFEM
{
public:
  emcOMCalFEMT(const char* name="", const char* title="");
  virtual ~emcOMCalFEMT();
  
  /// Collect object valid at a given time = not possible.
  virtual emcManageable* Collect(const emcManageable& /*object*/,
				 const PHTimeStamp& /*when*/) 
  {
    return 0; 
  }

  /// Fill an emcCalFEM object from a PdbCalBank
  virtual void FromPdbCalBank(emcCalFEM& calfem, PdbCalBank& bank) = 0;

  /// Get the name of the Objy classname we are dealing with.
  virtual std::string GetPersistentClassName(void) const = 0;

  using emcObjectManager::Read;

  /// Read object.
  virtual bool Read(emcManageable& object,
		    const PHTimeStamp& time_stamp,
		    int code=-1);

  /** Special read method to retrieve shadowed banks. */
  bool ReadPreviousVersion(emcManageable& object, 
			   const PHTimeStamp& time_stamp,
			   int code=-1,
			   int version=0);

  /// Reset this object.
  virtual void Reset(void);

  /// Fill a PdbCalBank from an emcCalFEM object.
  virtual void ToPdbCalBank(const emcCalFEM& calfem, PdbCalBank& bank) = 0;

  /// Write object
  virtual bool Write(const emcManageable& object,
		     const PHTimeStamp& tdummy,
		     int dummy=-1);

protected:

  emcManageable::EStorage storage() const 
  { return fBM->storage(); }

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

//_____________________________________________________________________________
template <class BM>
emcOMCalFEMT<BM>::emcOMCalFEMT(const char* name, const char* title)
  : emcOMCalFEM(changeName(name).c_str(), title),
    fBM(new BM)
{
}

//_____________________________________________________________________________
template <class BM>
emcOMCalFEMT<BM>::~emcOMCalFEMT()
{
  delete fBM;
}

//_____________________________________________________________________________
template<class BM>
bool 
emcOMCalFEMT<BM>::Read(emcManageable& object,
		       const PHTimeStamp& time_stamp,
		       int code)
{
  // Read object from DB.

  emcCalFEM& calfem = static_cast<emcCalFEM&>(object);

  int femAbsPosition;
  int pinNumber;
  int idummy;

  if ( code < 0 )
    {
      code = calfem.AbsolutePosition();
    }

  emcCalFEM::FEMDecode(code, femAbsPosition, pinNumber, idummy, idummy);

  if ( femAbsPosition != calfem.AbsolutePosition() )
    {
      std::cerr << EMC_ERROR_MSG 
		<< " emcOMCalFEMT<BM>::Read : you requested the read "
		<< " of FEM " << femAbsPosition 
		<< " whereas the object you gave "
		<< " is FEM " << calfem.AbsolutePosition() << std::endl
		<< " Read was NOT done. Object unchanged. " << std::endl;
      return false;
    }

  calfem.Reset();

  assert(object.GetSource()==fBM->storage());

  if (DM()->GetVerboseLevel())
    {
      std::cout << EMC_INFO_MSG << " : Entering " << GetName()
		<< "::Read for FEM# " << femAbsPosition
		<< " flavour=" << calfem.GetCategory()
		<< " at time " << time_stamp
		<< std::endl;
    }

  bool kIsRead = false;

  // Try to open a read transaction
  PdbStatus status = true;

  status = fBM->DBApplication()->startRead();

  if (status)
    {

      PdbBankID bankID;

      bankID.setInternalValue(femAbsPosition);

      std::string pdb_classname = GetPersistentClassName();
      std::string basename = "calib.emc.";
      basename += calfem.GetCategory();

      PdbCalBank* emcBank =
        fBM->fetchBank(pdb_classname.c_str(),
		       bankID,
		       basename.c_str(),
		       const_cast<PHTimeStamp&>(time_stamp),
		       std::string(calfem.GetCategory()));

      if (!emcBank)
        {
          std::cerr << EMC_ERROR_MSG
		    << GetName()
		    << "::Read : cannot fetch bank (flavour = "
		    << object.GetCategory() << ",absPosition= "
		    << femAbsPosition << ") at time " << time_stamp 
		    << std::endl
		    << "fetchBank(" << pdb_classname.c_str() << ","
		    << bankID.getInternalValue() 
		    << "," << basename.c_str() << ","
		    << const_cast<PHTimeStamp&>(time_stamp) << std::endl;
        }
      else
        {
          kIsRead = true;
	  
          calfem.SetValidityPeriod(emcBank->getStartValTime(),
                                   emcBank->getEndValTime());
	  
          // Fill the calfem from emcBank.
          FromPdbCalBank(calfem, *emcBank);
	  
          calfem.SetDescription(emcBank->getDescription().getString());
        }

      bool status_commit = fBM->DBApplication()->commit();

      delete emcBank;

      kIsRead = status_commit && kIsRead;

    }
  
  if (!kIsRead)
    {
      calfem.Reset();
    }

  return kIsRead;
}

//_____________________________________________________________________________
template<class BM>
bool
emcOMCalFEMT<BM>::ReadPreviousVersion(emcManageable& object,
                                      const PHTimeStamp& time_stamp,
                                      int code,
                                      int version)
{
  emcCalFEM& calfem = static_cast<emcCalFEM&>(object);

  int femAbsPosition;
  int pinNumber;
  int idummy;

  if ( code < 0 )
    {
      code = calfem.AbsolutePosition();
    }

  emcCalFEM::FEMDecode(code, femAbsPosition, pinNumber, idummy, idummy);

  if ( femAbsPosition != calfem.AbsolutePosition() )
    {
      std::cerr << EMC_ERROR_MSG 
		<< " emcOMCalFEMT<BM>::Read : you requested the read "
		<< " of FEM " << femAbsPosition 
		<< " whereas the object you gave "
		<< " is FEM " << calfem.AbsolutePosition() << std::endl
		<< " Read was NOT done. Object unchanged. " << std::endl;
      return false;
    }

  calfem.Reset();

  assert(object.GetSource()==fBM->storage());

  if (DM()->GetVerboseLevel())
    {
      std::cout << EMC_INFO_MSG << " : Entering " << GetName()
		<< "::Read for FEM# " << femAbsPosition
		<< " flavour=" << calfem.GetCategory()
		<< " at time " << time_stamp
		<< std::endl;
    }

  bool kIsRead = false;

  // Try to open a read transaction
  PdbStatus status = true;

  status = fBM->DBApplication()->startRead();

  if (status)
    {

      PdbBankID bankID;

      bankID.setInternalValue(femAbsPosition);

      std::string pdb_classname = GetPersistentClassName();
      std::string basename = "calib.emc.";
      basename += calfem.GetCategory();

      PdbCalBank* emcBank =
        fBM->fetchPreviousVersionBank(pdb_classname.c_str(),
				      bankID,
				      basename.c_str(),
				      const_cast<PHTimeStamp&>(time_stamp),
				      std::string(calfem.GetCategory()),
				      version);


      if (!emcBank)
        {
          std::cerr << EMC_ERROR_MSG
		    << GetName()
		    << "::ReadPreviousVersion : cannot fetch bank (flavour = "
		    << object.GetCategory() << ",absPosition= "
		    << femAbsPosition << ") at time " << time_stamp 
		    << std::endl
		    << "fetchBank(" << pdb_classname.c_str() << ","
		    << bankID.getInternalValue() 
		    << "," << basename.c_str() << ","
		    << const_cast<PHTimeStamp&>(time_stamp) << std::endl;
        }
      else
        {

          kIsRead = true;

          calfem.SetValidityPeriod(emcBank->getStartValTime(),
                                   emcBank->getEndValTime());

          // Fill the calfem from emcBank.
          FromPdbCalBank(calfem, *emcBank);

          calfem.SetDescription(emcBank->getDescription().getString());
        }

      bool status_commit = fBM->DBApplication()->commit();

      kIsRead = status_commit && kIsRead;

      delete emcBank;
    }

  if (!kIsRead)
    {
      calfem.Reset();
    }

  return kIsRead;
}

//_____________________________________________________________________________
template<class BM>
void
emcOMCalFEMT<BM>::Reset(void)
{
  fBM->Reset();
}

//_____________________________________________________________________________
template<class BM>
bool 
emcOMCalFEMT<BM>::Write(const emcManageable& object,
			const PHTimeStamp&,int)
{
  // Write a manageable object to db.

  bool kWritten = false;

  const emcCalFEM& calfem = static_cast<const emcCalFEM&>(object);

  if (DM()->GetVerboseLevel())
    {
      std::cout << EMC_INFO_MSG << " : Entering "
		<< GetName() << "::Write for FEM "
		<< calfem.AbsolutePosition()
		<< " flavour=" << calfem.GetCategory()
		<< " valid from " << calfem.GetStartValTime()
		<< " up to " << calfem.GetEndValTime()
		<< std::endl;
    }

  PdbStatus status = true;

  assert(object.GetDestination() == fBM->storage());
    
  status = fBM->DBApplication()->startUpdate();

  if (!status)
    {
      if ( DM()->GetVerboseLevel() )
        {
          std::cerr << EMC_ERROR_MSG << "emcOMCalFEMT<BM>::Write : "
		    << "Cannot open a write transaction to Objy" 
		    << std::endl;
        }
      fBM->DBApplication()->abort();
      return false;
    }

  PdbCalBank* emcBank = 0;
  PHTimeStamp begin, end;
  PdbBankID bankID;

  bankID.setInternalValue(calfem.AbsolutePosition());

  std::string pdb_classname = GetPersistentClassName();
  std::string description = calfem.Description();
  std::string basename = "calib.emc.";
  basename += calfem.GetCategory();

  begin = calfem.GetStartValTime();
  end = calfem.GetEndValTime();

  // Is calfem is a draft, we first look if we find in the DB
  // a fem for this time, being also a draft...
  // in which case we'll _overwrite_ it.

  if (calfem.IsDraft())
    {
      emcBank = fBM->BankManager()->fetchBank(pdb_classname.c_str(),
					      bankID,
					      basename.c_str(),
					      const_cast<PHTimeStamp&>(begin));

      if (emcBank && emcBank->getUserName() == "draft")
        {
          emcBank->setEndValTime(end);
          emcBank->setStartValTime(begin);
          PHTimeStamp now;
          now.setToSystemTime();
          emcBank->setInsertTime(now);
        }
      else
        {
          delete emcBank;
          emcBank = 0;
        }
    }

  if (!emcBank)
    {

      if (!calfem.IsDraft())
        {
#if 1
          emcBank = fBM->BankManager()->createBank(pdb_classname.c_str(),
						   bankID,
						   description.c_str(),
						   begin, end,
						   basename.c_str());
#endif

#if 0
          // disconnect this to speed up writing, so
          // it's useable for online calibrations (OnlCal framework)
          emcBank = fBM->createBankWithUpdate(pdb_classname.c_str(),
					      bankID,
					      description.c_str(),
					      begin, end,
					      basename.c_str());
#endif	 
        }
      else
        {
	  
          emcBank = fBM->BankManager()->createBank(pdb_classname.c_str(),
						   bankID,
						   description.c_str(),
						   begin, end,
						   basename.c_str());
	  
          emcBank->setUserName("draft");
        }
    }
  
  assert(emcBank != 0);

  // We then fill the PdbCalBank from the values in calfem.
  ToPdbCalBank(calfem, *emcBank);
  
  status = fBM->DBApplication()->commit();

  if (status)
    {
      kWritten = true;
    }
  else
    {
      std::cerr << EMC_INFO_MSG << GetName() 
		<< "::Write : commit failed ?!" << std::endl;
      kWritten = false;
    }

  delete emcBank;

  return kWritten;
}




#endif
