#ifndef __EMCDBMS_H__
#define __EMCDBMS_H__

#ifndef __EMCMANAGEABLE_H__
#  include "emcManageable.h"
#endif
#include <string>

/** Utility class to give the default data storage to be used.

Defaults to Postgres currently.

*/

class emcDBMS
{
 public:

  static const char* asString();

  static bool set(const char*);

  static emcManageable::EStorage get();

 private:
  static emcManageable::EStorage fgStorage;

};

#endif
