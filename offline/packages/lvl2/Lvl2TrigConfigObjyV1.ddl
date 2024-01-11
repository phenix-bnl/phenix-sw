#ifndef __LVL2TRIGCONFIGOBJYV1_DDL__
#define __LVL2TRIGCONFIGOBJYV1_DDL__
#include <Lvl2TrigConfigObjy.h>

class Lvl2TrigConfigObjyV1 : public Lvl2TrigConfigObjy {

public:

    Lvl2TrigConfigObjyV1(){ 
      runNumber = -1;
      trigConfigName =NULL;
      trigConfigVersion = 0;
  };

  ~Lvl2TrigConfigObjyV1(){;}
  
  virtual int getRunNumber() const {return runNumber;};
  virtual void setRunNumber(const int r){runNumber=r;};

  virtual const char* getTrigConfigName() const {return trigConfigName;};
  virtual void setTrigConfigName(const char* name){trigConfigName = name;};

  virtual int getConfigVersion() const {return trigConfigVersion;};
  virtual void setConfigVersion(const int r){trigConfigVersion=r;};

  virtual int getLvl2TrigArrayLength() const { return TrigAlgoArray.size(); }; 	
  
protected:
  int runNumber;
  ooVString trigConfigName;
  int trigConfigVersion;
};
#endif //__LVL2TRIGCONFIGOBJYV1_DDL__
