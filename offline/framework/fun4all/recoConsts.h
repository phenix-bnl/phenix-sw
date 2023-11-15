// Do yourself and others a favour, please sort variable/function name
// according to the roman alphabet

#ifndef __RECOCONSTS_H__
#define __RECOCONSTS_H__

#include <PHFlag.h>
#include <PHTimeStamp.h>

class recoConsts : public PHFlag
{

 public:

  static recoConsts * instance()
    {
      if (__instance) return __instance;
      __instance =  new recoConsts();
      return __instance;
    }

  PHTimeStamp get_TimeStamp() const {return TimeStamp;};
  void set_TimeStamp(PHTimeStamp t);  

  void Print() const;

 protected: 
  recoConsts();
  
  static recoConsts *__instance;

  PHTimeStamp TimeStamp;

};


#endif /* __RECOCONSTS_H__ */


