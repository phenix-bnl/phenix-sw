#ifndef __EMCASCIISTORAGEMANAGER_H__
#define __EMCASCIISTORAGEMANAGER_H__

#ifndef __EMCMANAGEABLE_H__
#include "emcManageable.h"
#endif

/** Tiny adapter class to be used with DataManager_plugins/base templates. */

class emcAsciiStorageManager
{
public:
  static emcManageable::EStorage storage() 
  { return emcManageable::kFile_ASCII; }

  static const char* name() { return "emcAsciiStorageManager"; }
};

#endif
