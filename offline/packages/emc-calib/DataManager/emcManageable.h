#ifndef __EMCMANAGEABLE_H__
#define __EMCMANAGEABLE_H__

#ifndef __EMCNAMED_H__
#include "emcNamed.h"
#endif
#include <string>

class PHTimeStamp;

/**
 * (ABC) Object that can be passed to the emcDataManager
 * to be read/written from/to files/database(s).
 * It allows to define data source and data destination.
 * The GetCategory pure virtual method 
 * must be overriden in child classes and give a name which can be used
 * to find the correct database or file basename for a given object type.
 * @author Laurent Aphecetche
 * @ingroup datamanager
 * @ingroup interface
 */

class emcManageable : public emcNamed
{

public:

  /// Storage type.
  enum EStorage {
    /// nowhere
    kNone = -1,
    /// Objectivity
    kDB_Objy,
    /// Plain ASCII files.
    kFile_ASCII,
    /// "DB" at time of PbSc construction (ascii files also, really)
    kDB_Construction,
    /// PostgreSQL
    kDB_Pg
  };

  /// Default ctor
  emcManageable() : emcNamed(), fDestination(kNone), fSource(kNone)
  {}

  /// Ctor with name, title and classname (required).
  emcManageable(const char* name, const char* title, const char* classname)
    : emcNamed(name, title, classname), fDestination(kNone), fSource(kNone)
  {}

  /// Dtor (does nothing).
  virtual ~emcManageable()
  {}

  /// Copy ctor.
  emcManageable(const emcManageable& obj) : emcNamed()
  {
    obj.Copy(*this);
  }

  /// Assignment operator.
  emcManageable& operator=(const emcManageable& obj)
  {
    if ( &obj != this )
      {
	obj.Copy(*this);
      }
    return *this;
  }

  /** Category of the object = GAIN,PED,HL,etc...
   *  This category is used to determine the top-level container of this
   *   object in the database. 
   */
  virtual const char* GetCategory(void) const = 0;

  /// Get storage name from enum number.
  static const char* GetStorageName(emcManageable::EStorage storage)
  {
    if ( storage == emcManageable::kNone )
      return "None";
    if ( storage == emcManageable::kDB_Objy )
      return "Objy FD";
    if ( storage == emcManageable::kFile_ASCII )
      return "ASCII File";
    if ( storage == emcManageable::kDB_Construction )
      return "Construction DB";
    if ( storage == emcManageable::kDB_Pg )
      return "Pg";

    return "not valid";
  }

  /// Get storage enum value from storage name.
  static emcManageable::EStorage GetStorage(const char* storagename)
  {
    std::string name = storagename;
    if ( name == "Objy FD" || name == "Objy" || name == "Objectivity" )
      {
	return emcManageable::kDB_Objy;
      }
    else if ( name == "PostgreSQL" || name == "Pg" || name == "pg" )
      {
	return emcManageable::kDB_Pg;
      }
    else if ( name == "ASCII File" )
      {
	return emcManageable::kFile_ASCII;
      }
    else if ( name == "Construction DB" )
      {
	return emcManageable::kDB_Construction;
      }
    return emcManageable::kNone;
  }

  /// Get the storage destination.
  EStorage GetDestination(void) const
  {
    return fDestination;
  }

  /// Get the storage origine.
  EStorage GetSource(void) const
  {
    return fSource;
  }

  /** Tells if this object is still valid at that time.
      @return false by default. Must be overriden.
   */
  virtual bool IsValid(const PHTimeStamp& /*when*/) const
  {
    return false;
  }

  /// Reset the object.
  virtual void Reset(void)
  {}

  /// Set the storage destination.
  virtual void SetDestination(emcManageable::EStorage destination)
  {
    fDestination = destination;
  }
  
  /// Set the storage origine.
  virtual void SetSource(emcManageable::EStorage origine)
  {
    fSource = origine;
  }

 protected:
  
  void Copy(emcNamed& obj) const
  {
    emcManageable& man = static_cast<emcManageable&>(obj);
    emcNamed::Copy(obj);
    man.fSource = fSource;
    man.fDestination = fDestination;
  }
  
 private:
  EStorage fDestination;
  EStorage fSource;
};

#endif





