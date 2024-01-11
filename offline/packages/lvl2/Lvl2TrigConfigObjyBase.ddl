#ifndef __LVL2TRIGCONFIGOBJYBASE_DDL__
#define __LVL2TRIGCONFIGOBJYBASE_DDL__
#include <Lvl2Struct.h>
#include <Lvl1Struct.h>
#include <PHTimeStamp.h>

class Lvl2TrigConfigObjyBase : public ooObj {
  
public:
  virtual PHTimeStamp getTimeStamp() const = 0;
  virtual void getLvl2TrigSeqFromATP(ooVArrayT<TrigAlgo> _ta) = 0;
  virtual TrigAlgo getLvl2TrigAlgoObjy(int n) = 0;
  virtual int getLvl2TrigArrayLength() const = 0; 
  //  virtual ooVArrayT<TrigAlgo> getLvl2TrigConfSeqObjy() = 0;  
  virtual int getVersion() const = 0;
  virtual void setVersion(int v) = 0;

  virtual Lvl2Algo getLvl2Algo(int n) {
          Lvl2Algo t = {"",0};
          return t;
  }
  virtual void setLvl2Algo(ooVArrayT<Lvl2Algo> _ta) {};
  virtual int getLvl2AlgoArrayLength() const {return 0;} 

  virtual int getRunNumber() const {return 0;}
  virtual void setRunNumber(const int r) {};

  virtual const char* getTrigConfigName() const {return "";}
  virtual void setTrigConfigName(const char* name) {};

  virtual int getConfigVersion() const {return 0;}
  virtual void setConfigVersion(const int r) {};

  virtual void setLvl2Status(const int n) {}
  virtual int getLvl2Status() const {return 0;}

  virtual void setLvl1TrigStruct(ooVArrayT<Lvl1Trig> _ta) {}
  virtual Lvl1Trig getLvl1TrigStructObjy(int n){
		Lvl1Trig t = {"",0,0,0,0,0.0};
		return t;
	}
  virtual int getLvl1TrigArrayLength() const {return 0;}

};
#endif //__LVL2TRIGCONFIGOBJYBASE_DDL__








