#include "DepObj.h"

#include <iostream>


using namespace std;

DepObj *DepObj::__instance = NULL;

DepObj::~DepObj()
{
  __instance = NULL;
}

DepObj *DepObj::instance()
{
  if (__instance)
    return __instance;

  __instance = new DepObj();  
  return __instance;
}

void DepObj::Warning(const char* func) const
{
  cout << "WARNING: DepObj::using virtual function, doing nothing! " << endl;
  cout << "         Offending function: " << func << endl;

  return;
}
