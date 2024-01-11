#ifndef __LVL2TRIGCONFIGOBJY_DDL__
#define __LVL2TRIGCONFIGOBJY_DDL__
#include <Lvl2TrigConfigObjyBase.h>

class Lvl2TrigConfigObjy : public Lvl2TrigConfigObjyBase {

public:

    Lvl2TrigConfigObjy(){ 
    PHTimeStamp timeStamp(time(0));
    storeTime = timeStamp;
    version = 0;
  };

  ~Lvl2TrigConfigObjy(){;}
  
  virtual void getLvl2TrigSeqFromATP(ooVArrayT<TrigAlgo> taArray){
     TrigAlgoArray = taArray; };

//virtual ooVArrayT<TrigAlgo> getLvl2TrigConfSeqObjy() {return TrigAlgoArray;}

  virtual TrigAlgo getLvl2TrigAlgoObjy(int n) {return TrigAlgoArray.elem(n);};
  virtual int getLvl2TrigArrayLength() const { return TrigAlgoArray.size(); }; 
  virtual PHTimeStamp getTimeStamp() const {return storeTime;};
  virtual int getVersion() const {return version;};
  virtual void setVersion(int v){version=v;};

  
protected:
  ooVArrayT<TrigAlgo> TrigAlgoArray;
  PHTimeStamp storeTime;
  int version;
};
#endif //__LVL2TRIGCONFIGOBJY_DDL__








