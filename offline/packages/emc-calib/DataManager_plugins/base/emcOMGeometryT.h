#ifndef __EMCOMGEOMETRYT_H__
#define __EMCOMGEOMETRYT_H__

#include <emcManageable.h>
#include <emcObjectManager.h>

#include <string>
#include <iostream>
#include <cassert> 
#include <cstdlib>

class emcGeometry;

/**
   (Template) Plugin class for emcGeometry objects.
   @ingroup dmplugins
*/

template <class BM>
class emcOMGeometryT : public emcObjectManager
{

public:

  emcOMGeometryT(const char* name = "", const char* title = "");
  virtual ~emcOMGeometryT();

  /** Can we collect such object ? */
  virtual bool CanCollect(const emcManageable& object) const;

  /** Can we write a given object type ? */
  virtual bool CanWrite(const emcManageable& object) const;

  /** Can we read a given object type ? */
  virtual bool CanRead(const emcManageable& object) const;

  /** Collect a manageable from the DB. \\
      See the DM interface for parameters.\\
      Unless allowed explicitely by a particular Object Manager,\\
      in general the returned pointer {\bf should not be deleted} by\\
      the receiver !
  */
  virtual emcManageable* Collect(const emcManageable& object,
				 const PHTimeStamp& when);


  using emcObjectManager::Read;

  /** Read a manageable object from the DB. \\
      See the DM interface for parameters.
  */
  virtual bool Read(emcManageable& object,
		    const PHTimeStamp& time_stamp,
		    int id);

  /** Reset the OM. \\
      After a call to Reset, the OM must behave as if it was newly created.
  */
  virtual void Reset(void);

  /** Write a manageable object to the DB.
      See the DM interface for parameters.
  */
  virtual bool Write(const emcManageable& object,
		     const PHTimeStamp& tStart,
		     int id = -1);

private:
  emcGeometry* fGeometry;
  BM* fBM;

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
};

//_____________________________________________________________________________
//_____________________________________________________________________________
//_____________________________________________________________________________

#include <iostream>
#include <vector>

#include <emcDataManager.h>
#include <emcGeometry.h>
#include <PdbCalBank.hh>
#include <PdbBankList.hh>
#include <PdbBankListIterator.hh>
#include <PdbEmcSecGeom.hh>
#include <Pdb.hh>
#include <PdbApplication.hh>
#include <PdbBankManager.hh>

//_____________________________________________________________________________
template <class BM>
emcOMGeometryT<BM>::emcOMGeometryT(const char* name, const char* title) :
  emcObjectManager(changeName(name).c_str(), title),
  fGeometry(0),
  fBM(new BM)
{
}

//_____________________________________________________________________________
template <class BM>
emcOMGeometryT<BM>::~emcOMGeometryT()
{
  Reset();
  delete fBM;
}


//_____________________________________________________________________________
template <class BM>
bool
emcOMGeometryT<BM>::CanCollect(const emcManageable& object) const
{
  return CanRead(object);
}


//_____________________________________________________________________________
template <class BM>
bool
emcOMGeometryT<BM>::CanWrite(const emcManageable& object) const
{
  if (object.GetDestination() != fBM->storage())
    {
      return false;
    }

  const emcManageable* object_ptr = &object;

  const emcGeometry* test =
    dynamic_cast<const emcGeometry*>(object_ptr);

  if ( test )
    {
      return true;
    }

  return false;
}

//_____________________________________________________________________________
template <class BM>
bool
emcOMGeometryT<BM>::CanRead(const emcManageable& object) const
{
  if ( object.GetSource() != fBM->storage() )
    {
      return false;
    }

  const emcManageable* object_ptr = &object;

  const emcGeometry* test =
    dynamic_cast<const emcGeometry*>(object_ptr);

  if ( test )
    {
      return true;
    }

  return false;
}

//_____________________________________________________________________________
template <class BM>
emcManageable*
emcOMGeometryT<BM>::Collect(const emcManageable&,
                            const PHTimeStamp& when)
{
  if ( fGeometry )
    {
      if (fGeometry->IsValid(when))
        {
          return fGeometry;
        }
    }

  if ( !fGeometry )
    {
      fGeometry = new emcGeometry();
    }

  bool ok = Read(*fGeometry, when, 1);

  if (!ok)
    {
      delete fGeometry;
      fGeometry = 0;
    }

  return fGeometry;

}

//_____________________________________________________________________________
template<class BM>
bool
emcOMGeometryT<BM>::Read(emcManageable& object,
			 const PHTimeStamp& time_stamp,
			 int id)
{
  bool kIsRead = false;

  emcGeometry& geometry = static_cast<emcGeometry&>(object);
  geometry.Reset();

  PdbStatus status = fBM->DBApplication()->startRead();

  if (!status)
    {
      std::cerr << "<E> emcOMGeometryT<" << BM::name()
		<< ">::Read : cannot open a read transaction"
	   << std::endl;
      fBM->DBApplication()->abort();
      return false;
    }

  if (id<0) id=1;

  PdbBankID bankID(id);
  std::string pdb_classname = "PdbEmcSecGeomBank";
  std::string basename = geometry.GetCategory();

  PdbCalBank* emcBank =
    fBM->BankManager()->fetchBank(pdb_classname.c_str(),
				  bankID,
				  basename.c_str(),
				  const_cast<PHTimeStamp&>(time_stamp));

  if (!emcBank)
    {
      std::cerr << "<E> emcOMGeometryT<" << BM::name()
	   << ">::Read : cannot fetch geometry for time = "
	   << time_stamp << std::endl;
      fBM->DBApplication()->abort();
      return false;
    }


  kIsRead = true;

  size_t nsectors = emcBank->getLength();
  assert (nsectors > 0 && nsectors <= emcGeometry::MaxNumberOfSectors());
  PdbEmcSecGeom* pdbsec;

  std::vector<SecGeom> theSectors(nsectors);

  for ( size_t iS = 0; iS < nsectors; iS++ )
    {

      SecGeom& sec = theSectors[iS];

      pdbsec = static_cast<PdbEmcSecGeom*>(&(emcBank->getEntry(iS)));

      size_t nx, ny;
      double xs, ys;

      pdbsec->NxNy(nx, ny);
      sec.SetNxNy(nx, ny);

      pdbsec->TowerXYSize(xs, ys);
      sec.SetTowerXYSize(xs, ys);

      sec.SetDirectTransformation(pdbsec->RotationMatrix(),
                                  pdbsec->TranslationVector());

    } // iS

  geometry.Set(theSectors);

  bool status_commit = fBM->DBApplication()->commit();
  kIsRead = status_commit && kIsRead;

  delete emcBank;

  return kIsRead;
}


//_____________________________________________________________________________
template<class BM>
void
emcOMGeometryT<BM>::Reset(void)
{
  delete fGeometry;
  fGeometry = 0;
}

//_____________________________________________________________________________
template<class BM>
bool
emcOMGeometryT<BM>::Write(const emcManageable& object,
			  const PHTimeStamp&,int)
{
  bool kWritten = false;

  std::cout <<"<E>  emcOMGeometryT<" << BM::name() 
<<">::Write : usage of FetchAllBanks which has been discontinued" << std::endl;
std::cout << "Someone has to fix this, Exiting now" <<  std::endl;
 std::exit(0);

  return kWritten;
}

#endif
