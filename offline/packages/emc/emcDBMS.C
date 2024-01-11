#include "emcDBMS.h"
#include <iostream>
#include "phool.h"

emcManageable::EStorage emcDBMS::fgStorage = emcManageable::kDB_Pg;

//_____________________________________________________________________________
const char* 
emcDBMS::asString()
{
  return emcManageable::GetStorageName(fgStorage);
}

//_____________________________________________________________________________
bool
emcDBMS::set(const char* value)
{
  emcManageable::EStorage v = emcManageable::GetStorage(value);
  if ( v == emcManageable::kNone )
    {
      std::cout << PHWHERE << " WARNING. SETTING DEFAULT DATA SOURCE "
		<< " TO NONE !"
		<< std::endl;
      return false;
    }
  fgStorage = v;
  return true;
}

//_____________________________________________________________________________
emcManageable::EStorage
emcDBMS::get()
{
  return fgStorage;
}
