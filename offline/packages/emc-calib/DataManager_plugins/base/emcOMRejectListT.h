#ifndef __EMCOMREJECTLISTT_H__
#define __EMCOMREJECTLISTT_H__

#include <emcManageable.h>
#include <emcObjectManager.h>

class emcRejectList;
class PdbCalBank;

/**
   (Template) Plugin class for emcRejectList objects.
   @ingroup dmplugins
*/

template <class BM>
class emcOMRejectListT : public emcObjectManager
{
public:

  emcOMRejectListT(const char* name = "", const char* title = "");

  virtual ~emcOMRejectListT();

  bool CanRead(const emcManageable&) const;

  bool CanWrite(const emcManageable&) const;

  using emcObjectManager::Read;

  bool Read(emcManageable& object,
	    const PHTimeStamp& time_stamp, int dummy);

  void Reset(void);

  bool Write(const emcManageable& object,
	     const PHTimeStamp& tdummy,
	     int dummy = -1);

private:
  const char* GetPersistentClassName()
  {
    return "PdbEmcTowerRejectBank";
  }
  void FromPdbCalBank(emcRejectList&, PdbCalBank&);
  bool ReadFromFile(emcRejectList&);
  void ToPdbCalBank(const emcRejectList&, PdbCalBank&);

  BM* fBM;

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

};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include <cassert>
#include <iostream>
#include <sstream>
#include <cstdio>
#include <emcDataManager.h>
#include <emcRejectList.h>
#include <EmcIndexer.h>
#include <PdbCalBank.hh>
#include <PdbEmcTowerReject.hh>
#include <emcDefines.h>
#include <Pdb.hh>
#include <PdbApplication.hh>
#include <PdbBankManager.hh>
#include <fstream>

//_____________________________________________________________________________
template<class BM>
emcOMRejectListT<BM>::emcOMRejectListT(const char* name, const char* title)
  : emcObjectManager(changeName(name).c_str(),title), fBM(new BM)
{
}

//_____________________________________________________________________________
template<class BM>
emcOMRejectListT<BM>::~emcOMRejectListT()
{
  Reset();
  delete fBM;
}

//_____________________________________________________________________________
template<class BM>
bool
emcOMRejectListT<BM>::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != fBM->storage() )
    {
      return false;
    }

  const emcRejectList* rl = dynamic_cast<const emcRejectList*>(&object);
  
  if ( rl )
    {
      return true;
    }
  return false;
}

//_____________________________________________________________________________
template<class BM>
bool
emcOMRejectListT<BM>::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != fBM->storage() )
    {
      return false;
    }

  const emcRejectList* rl = dynamic_cast<const emcRejectList*>(&object);
  
  if ( rl )
    {
      return true;
    }
  return false;
}


//_____________________________________________________________________________
template<class BM>
void
emcOMRejectListT<BM>::FromPdbCalBank(emcRejectList& rl, PdbCalBank& bank)
{
  for ( size_t i = 0; i < bank.getLength(); ++i ) 
    {
      PdbEmcTowerReject* tr = static_cast<PdbEmcTowerReject*>
	(&(bank.getEntry(i)));
      assert(tr!=0);
      rl.set_or( tr->TowerId(), tr->AmplitudeError(), tr->AmplitudeWarning(),
		 tr->TimingError(), tr->TimingWarning());
    }
}

//_____________________________________________________________________________
template<class BM>
bool
emcOMRejectListT<BM>::Read(emcManageable& object, 
			   const PHTimeStamp& time_stamp,int)
{
  assert(CanRead(object));
  emcRejectList& rl = static_cast<emcRejectList&>(object);

  rl.Reset();

 if ( DM()->GetVerboseLevel() ) 
    {
      std::cout << EMC_INFO_MSG << " : Entering " << GetName()
	     << "::Read (from Objy)" << std::endl;
    }

  PdbStatus status = fBM->DBApplication()->startRead() ;

  if (!status) 
    {
      std::cerr << EMC_ERROR_MSG << GetName() 
		<< "::Read : Could not open read "
		<< "transaction to Objy." << std::endl;
      fBM->DBApplication()->abort();
      return false;
    }

  PdbBankID bankID;

  bankID.setInternalValue(0);

  std::string pdb_classname = GetPersistentClassName();
  std::string basename = "calib.emc.";
  basename += rl.GetCategory();

  PdbCalBank* emcBank = 
    fBM->BankManager()->fetchBank(pdb_classname.c_str(),
				  bankID,
				  basename.c_str(),
				  const_cast<PHTimeStamp&>(time_stamp));
  
  if (!emcBank) 
    {
      std::cerr << EMC_ERROR_MSG 
		<< GetName() << "::Read : Cannot fetch reject list bank "
		<< "at time " << time_stamp << std::endl;
      return false;
    }
  
  FromPdbCalBank(rl,*emcBank);

  delete emcBank;

  return fBM->DBApplication()->commit() ;
}

//_____________________________________________________________________________
template<class BM>
void
emcOMRejectListT<BM>::Reset()
{
}

//_____________________________________________________________________________
template<class BM>
void
emcOMRejectListT<BM>::ToPdbCalBank(const emcRejectList& rl, PdbCalBank& bank)
{
  size_t n = rl.size(); // this is the size of non-zeros 

  bank.setLength(n);

  size_t j = 0;

  for ( size_t i = 0; i < rl.maxsize(); ++i ) 
    {
      if (rl.nonZero(i))
	{
	  PdbEmcTowerReject* tr = static_cast<PdbEmcTowerReject*>
	    (&(bank.getEntry(j)));
	  assert(tr!=0);
	  tr->set(i,rl.AmplitudeError(i),rl.AmplitudeWarning(i),
		  rl.TimingError(i),rl.TimingWarning(i));
	  ++j;
	}
    }

  assert(j==n);
}

//_____________________________________________________________________________
template<class BM>
bool 
emcOMRejectListT<BM>::Write(const emcManageable& object, 
			    const PHTimeStamp&,int)
{
  assert(CanWrite(object));
  const emcRejectList& rl = static_cast<const emcRejectList&>(object);

  bool written = false;

  if ( DM()->GetVerboseLevel() ) 
    {
      std::cout << EMC_INFO_MSG << " : Entering " 
		<< GetName() << "::Write " << std::endl;
    }

  PdbBankID bankID ;

  bankID.setInternalValue(0);

  std::string pdb_classname = GetPersistentClassName();
  std::string description = "Written by emcOMRejectList";
  std::string basename = "calib.emc.";
  basename += rl.GetCategory();

  PdbStatus status = fBM->DBApplication()->startUpdate();

  if ( !status ) 
    {
      std::cerr << EMC_ERROR_MSG << GetName() << "::Write : "
		<< "Cannot open a write transaction to Objy"
		<< std::endl;
      fBM->DBApplication()->abort();
      return false;
    }

  PHTimeStamp start = rl.GetStartValTime();
  PHTimeStamp end = rl.GetEndValTime();

  PdbCalBank* emcBank =  
    fBM->BankManager()->createBank(pdb_classname.c_str(),
				   bankID,
				   description.c_str(),
				   start,
				   end,
				   basename.c_str()) ;
  if (!emcBank) 
    {
      return false;
    }

  ToPdbCalBank(rl,*emcBank);

  status = fBM->DBApplication()->commit();

  delete emcBank;

  if (status)
    {
      written=true;
    }

  return written;
} 


#endif
