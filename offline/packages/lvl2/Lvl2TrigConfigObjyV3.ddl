#ifndef __LVL2TRIGCONFIGOBJYV3_DDL__
#define __LVL2TRIGCONFIGOBJYV3_DDL__
#include <Lvl2TrigConfigObjyV2.h>

class Lvl2TrigConfigObjyV3 : public Lvl2TrigConfigObjyV2 {

public:

    Lvl2TrigConfigObjyV3(){ 
  };

  ~Lvl2TrigConfigObjyV3(){;}

  virtual Lvl2Algo getLvl2Algo(int n){return Lvl2AlgoArray.elem(n); };
  virtual void setLvl2Algo(ooVArrayT<Lvl2Algo> taArray){
	Lvl2AlgoArray = taArray; };
  virtual int getLvl2AlgoArrayLength() const {return Lvl2AlgoArray.size(); }; 
  
protected:
  ooVArrayT<Lvl2Algo> Lvl2AlgoArray;
};
#endif //__LVL2TRIGCONFIGOBJYV3_DDL__








