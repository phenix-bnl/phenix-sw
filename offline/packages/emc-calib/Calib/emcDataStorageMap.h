#ifndef __EMCDATASTORAGEMAP_H__
#define __EMCDATASTORAGEMAP_H__

#ifndef __EMCMANAGEABLE_H__
#  include "emcManageable.h"
#endif
#include <map>
#include <iostream>
#include <string>
#include <vector>

/**
   Small utility class to gather calibration data storage sources or
   destinations.

@ingroup calibration

*/

class emcDataStorageMap
{
 public:

  /** The ctor param will be the default storage returned when
      using the storage() method.
   */
  
  emcDataStorageMap(emcManageable::EStorage def = emcManageable::kDB_Pg);

  /// Make this object empty.
  void clear();

  /** Whether this object is empty or not.
      By empty we mean that the internal map is empty, i.e.
      storage(what,source) has never been called for instance.
  */
  bool empty() const;

  /** Fill this object by parsing the given string str.
      where str is of the form "what1:source1,what2:source2,...",
      e.g. "Pedestals5:None,Gains:Pg,HLRatios:ASCII File".
      For the list of valid source strings, please see class emcManageable.
      @return true if str is well formed (in which case the internal map is
      replaced with the content of str).
      or false otherwise (in which case the object will be unchanged).
  */      
  bool parse(const std::string& str);

  /// Return the default storage, irrespective of calib. data type
  emcManageable::EStorage storage() const;

  /// Return the storage for calib. data type = what
  emcManageable::EStorage storage(const std::string& what) const;

  /** Set the storage source/destination of calib. type = what.
      Please note that we're picky on the storage name syntax.
      See emcManageable class.
  */
  bool storage(const std::string& what, const std::string& storagename);

  /** For access from CINT, same as above, but with const char* instead
      of std::string
  */
  //@{
  emcManageable::EStorage storage(const char* what) const;
  ///
  bool storage(const char* what, const char* storagename);
  //@}

  /// Return the list of storages for which we have sources/destinations.
  std::vector<std::string> knownStorages() const;

  /// 
  void print(std::ostream& out = std::cout) const;

 private:
  emcManageable::EStorage fDefaultStorage;
  typedef std::map<const std::string, emcManageable::EStorage> TMAP;
  TMAP fMap;
};

std::ostream& operator<<(std::ostream& out, const emcDataStorageMap&);

#endif
