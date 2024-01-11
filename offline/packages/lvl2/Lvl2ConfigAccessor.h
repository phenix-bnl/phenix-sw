#ifndef __LVL2CONFIGACCESSOR_H__
#define __LVL2CONFIGACCESSOR_H__

#include <Lvl2TrigConfigObjyV2.h>
#include <BoolDefs.h>
#include <stdlib.h>


class Lvl2ConfigAccessor : public Lvl2TrigConfigObjyV2 {

public:
  Lvl2ConfigAccessor(){
    bootFile = getenv("OO_FD_BOOT");
    //    bootFile ="phenixls.phenix.bnl.gov::/home/phoncs/gobinda/databases/Level2DB_temp1.boot";
  }
  ~Lvl2ConfigAccessor(){;}

  ooRef(Lvl2TrigConfigObjyBase) restoreLvl2TrigSeq(int runNum);
  void printLvl2Mode(Lvl2OperationMode &);

private:
  
  char* bootFile;
  
};

#endif // __LVL2CONFIGACCESSOR_H__

