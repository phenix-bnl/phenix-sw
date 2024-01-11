
#ifndef __EMCOMFEMTUPLET_H__
#define __EMCOMFEMTUPLET_H__

#include <emcManageable.h>
#include <emcObjectManager.h>

#include <cassert>
#include <cstring>
#include <string>

class emcFEMtuple;
class emcCalFEM;
class PdbCalBank;
class PdbBankID;

/** (OLD) Base class for all the plugins handling object deriving from emcFEMtuple.

\deprecated

    It implements the logic common to all plugins dealing with daughter
    classes of emcFEMtuple, for the Collect, Read and Write methods :
 
    a) Collect. It's a loop over all the FEM of EMCAL (the one stated
    in the configuration file, which is retrieved thanks to the 
    emcRawDataAccessor).
 
    b) Read. Reading of ONE FEM, either from Objy DB, or from file (in which
    case actual work is delegated to the ReadFromFile method.
 
    c) Write. Splits the emcFEMtuple into pieces of one FEM-long,
    make PdbCalBank from those, and put them into the Objy FD.
 
@ingroup dmplugins
@ingroup deprecated
*/

template <class BM, class FT>
class emcOMFEMtupleT : public emcObjectManager
{
public:
  emcOMFEMtupleT(const char* name = "", const char* title = "");
  virtual ~emcOMFEMtupleT();

  /** We announce that we can collect some objects. */
  virtual bool CanCollect(const emcManageable& object) const
  { return CanRead(object); }

  /** We announce that we can read some objects. */
  virtual bool CanRead(const emcManageable& object) const;

  /** We announce that we can write some objects. */
  virtual bool CanWrite(const emcManageable& object) const;
    
  /// Collect object valid at a given time.
  virtual emcManageable* Collect(const emcManageable& object,
				 const PHTimeStamp& when);

  using emcObjectManager::Read;

  /// Read object.
  virtual bool Read(emcManageable& object,
		    const PHTimeStamp& time_stamp,
		    int femCode);

  /// Reset this object.
  virtual void Reset(void);

  /// Write object
  virtual bool Write(const emcManageable& object,
		     const PHTimeStamp& tdummy,
		     int dummy = -1);

private:

  class changeName
  {
  public:
    changeName(const char* name)
    {
      name_ = emcManageable::GetStorageName(BM::storage());
      name_ += ":Forwarder:";
      name_ += name;
    }

    const char* c_str() const
    {
      return name_.c_str();
    }

  private:
    std::string name_;
  };

protected:
  FT* fFEMs;
  BM* fBM;
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include <emcRawDataAccessor.h>
#include <emcFEMtupleFactory.h>
#include <emcDataManager.h>
#include <emcDefines.h>
#include <emcCalFEM.h>
#include <emcCalFEMFactory.h>
#include <EmcDynamicData.h>
#include <emcFEMtuple.h>
#include <emcFEMtupleFactory.h>
#include <emcObjectManagerRegister.h>
#include <emcOMHelper.h>

//_____________________________________________________________________________
template <class BM, class FT>
emcOMFEMtupleT<BM,FT>::emcOMFEMtupleT(const char* name, const char* title)
  : emcObjectManager(changeName(name).c_str(), title),
    fFEMs(0),
    fBM(new BM)
{}

//_____________________________________________________________________________
template <class BM, class FT>
emcOMFEMtupleT<BM,FT>::~emcOMFEMtupleT()
{
  Reset();
  delete fFEMs;
  delete fBM;
}

//_____________________________________________________________________________
template <class BM, class FT>
bool
emcOMFEMtupleT<BM,FT>::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != fBM->storage() )
    {
      return false;
    }
  const FT* test = dynamic_cast<const FT*>(&object);
  if ( test )
    {
      emcCalFEM* cal = emcCalFEMFactory::Create(test->GetCategory(),0);
      cal->SetSource(test->GetSource());
      emcObjectManager* plug = emcOMHelper::findOM(*cal,0);
      delete cal;
      if ( plug ) 
	{
	  return true;
	}
    }
  return false;
}

//_____________________________________________________________________________
template <class BM, class FT>
bool
emcOMFEMtupleT<BM,FT>::CanWrite(const emcManageable& object) const
{
  if ( object.GetDestination() != fBM->storage() )
    {
      return false;
    }
  const FT* test = dynamic_cast<const FT*>(&object);
  if ( test )
    {
      emcCalFEM* cal = emcCalFEMFactory::Create(test->GetCategory(),0);
      cal->SetDestination(test->GetDestination());
      emcObjectManager* plug = emcOMHelper::findOM(*cal,1);
      delete cal;
      if ( plug ) 
	{
	  return true;
	}
    }
  return false;
}

//_____________________________________________________________________________
template <class BM, class FT>
emcManageable*
emcOMFEMtupleT<BM,FT>::Collect(const emcManageable& object,
			   const PHTimeStamp& when)
{

  if ( object.GetSource() != fBM->storage() )
    {
      std::cerr << "emcOMFEMtupleT<" << BM::name() <<">::Collect : "
		<< "object source is not compatible"
		<< std::endl;
      return 0;
    }

  if (fFEMs)
    {
      // If what we have already is still valid, returns it.
      if (fFEMs->IsValid(when))
        {
          return fFEMs;
        }
    }

  if ( DM()->GetVerboseLevel() )
    {
      std::cout << "<I-EMCAL> Fetching " << object.GetCategory() << " from "
		<< emcManageable::GetStorageName(object.GetSource()) 
		<< std::endl;
    }

  emcRawDataAccessor* rda = emcRawDataAccessor::GetInstance();
  assert(rda != 0);
  int nSM = rda->GetDynamicData()->getnSM();
  const SuperModule* SMMap = rda->GetDynamicData()->getSMMap();

  bool kEmpty = false;

  if (!fFEMs)
    {
      fFEMs = static_cast<FT*>
	(emcFEMtupleFactory::Create(object.GetCategory()));
      assert(fFEMs != 0);
      kEmpty = true;
    }
  else
    {
      // Check that the data source has not changed
      if (fFEMs->GetSource() != object.GetSource())
        {
          // Data source has changed. Just forget what we have.
          delete fFEMs;
          fFEMs = static_cast<FT*>
	    (emcFEMtupleFactory::Create(object.GetCategory()));
          assert(fFEMs != 0);
          kEmpty = true;
        }
    }

  // Tells from where we get the data.
  fFEMs->SetSource(object.GetSource());

  int ifem; // fem index
  int code;
  bool ok = true;
  emcFEMtuple* oneFEM;

  // Loop over FEMs

  for ( ifem = 0; ifem < nSM && ok == true; ifem++ )
    {

      // Skip the monitoring crates (if any) for anything which is
      // not Pedestals or HLRatios
      // -- Corrected Oct. 3, 2000 by HKD
      // Now skip the monitoring crates (if any) for Gains ONLY
      const char* category = object.GetCategory();
      assert(category != 0);
      if ( (strcmp(category, "Gains") == 0 ||
            strcmp(category, "TofT0Bs") == 0) &&
           SMMap[ifem].absPosition >= 172 )
        break;

      oneFEM = emcFEMtupleFactory::Create(object.GetCategory());
      assert (oneFEM != 0);
      oneFEM->SetSource(object.GetSource());

      code = emcCalFEM::FEMCode(SMMap[ifem].absPosition, SMMap[ifem].femPin,
                                SMMap[ifem].post_pre, SMMap[ifem].tac_pre);

      if (kEmpty)
        {
          // First time we are filling fFEMs : we read all the fems.
          ok = Read((*oneFEM), when, code);
          if (ok)
            {
              ok = fFEMs->Add((*oneFEM));
              if (!ok)
                {
                  std::cerr << "<E-EMCAL> emcOMFEMtupleT::Collect : "
			    << "Add failed." << std::endl;
		  break;
                }
              else
                {
                  if (DM()->GetVerboseLevel())
                    {
                      oneFEM->Print();
                    }
                }
            }
	  else
	    {
	      std::cerr << "<E-EMCAL> emcOMFEMtupleT::Collect : "
			<< "Read failed for fem " << ifem 
			<< std::endl;
	    }
        }
      else
        {
          // fFEMs is already filled up, se we first check
          // that we really need to fetch new values for this FEM
          if ( ! fFEMs->IsValid(when, ifem) )
            {
              ok = Read((*oneFEM), when, code);
              if (ok)
                {
                  if (DM()->GetVerboseLevel())
                    {
                      oneFEM->Print();
                    }
                  ok = fFEMs->ReplaceFEM(ifem, (*oneFEM));
                  if (!ok)
                    {
                      std::cerr << "<E-EMCAL> emcOMFEMtupleT::Collect : "
				<< "ReplaceFEM failed. FEM is : " << std::endl;
                      oneFEM->Print();
		      break;
                    }
                }
            }
        } // if (kEmpty)

      delete oneFEM;

    } // end loop over FEMs

  if (ok)
    {
      if ( DM()->GetVerboseLevel() )
        {
          std::cout << "done." << std::endl;
        }
      return fFEMs;
    }
  else
    {
      if ( DM()->GetVerboseLevel() )
        {
          std::cerr << "<E-EMCAL> emcOMFEMtupleT::Collect failed for "
		    << object.GetCategory() << std::endl;
        }
      delete fFEMs;
      fFEMs = 0;
      return 0;
    }
}

//_____________________________________________________________________________
template <class BM, class FT>
bool
emcOMFEMtupleT<BM,FT>::Read(emcManageable& object,
                        const PHTimeStamp& time_stamp,
                        int femCode)
{

  emcFEMtuple& femtuple = static_cast<emcFEMtuple&>(object);
  femtuple.Reset();

  int femAbsPosition, femPin, dummy;
  emcCalFEM::FEMDecode(femCode, femAbsPosition, femPin, dummy, dummy);

  PHTimeStamp tdummy(0);

  emcCalFEM* calfem = femtuple.AppendFEM(femAbsPosition, tdummy, tdummy);
  assert(calfem != 0);
  calfem->SetSource(object.GetSource());

  return DM()->Read(*calfem, time_stamp, femCode);
}

//_____________________________________________________________________________
template <class BM, class FT>
void
emcOMFEMtupleT<BM,FT>::Reset(void)
{
  delete fFEMs;
  fFEMs = 0;
}

//_____________________________________________________________________________
template <class BM, class FT>
bool
emcOMFEMtupleT<BM,FT>::Write(const emcManageable& object,
			     const PHTimeStamp& /*tdummy*/,
			     int /*dummy*/)
{
  // Write a manageable object to db.
  // object can handle any number of FEMs, we will split it in
  // different banks.

  bool kWritten = false;

  const emcFEMtuple& cfemtuple = static_cast<const emcFEMtuple&>(object);
  emcFEMtuple& femtuple = const_cast<emcFEMtuple&>(cfemtuple);

  // We loop over the fems to split the object
  for ( int ifem = 0; ifem < femtuple.GetNumberOfFEMs(); ifem++ )
    {
      // Get one FEM
      emcCalFEM* calfem = femtuple.GetFEM(ifem);
      assert (calfem != 0);
      if (DM()->GetVerboseLevel())
        {
          std::cout << "<I> emcOMFEMtupleT::Write : writing one FEM (flavour "
          << femtuple.GetCategory() << ")..." << std::endl;
          calfem->Print();
        }

      bool ok = DM()->Write(*calfem);
      if (!ok)
	{
	  std::cerr << "<E> Could not write FEM " << ifem 
		    << "(flavour "
		    << femtuple.GetCategory() << ")..." << std::endl;	  
	}
    }

  return kWritten;
}

#endif
