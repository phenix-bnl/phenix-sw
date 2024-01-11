#ifndef __LVL2TRIGCONFIGOBJYV2_DDL__
#define __LVL2TRIGCONFIGOBJYV2_DDL__
#include <Lvl2TrigConfigObjyV1.h>

class Lvl2TrigConfigObjyV2 : public Lvl2TrigConfigObjyV1 {
	
public:

    Lvl2TrigConfigObjyV2(){ 
      Lvl2Status = 0; // Lvl2 is off by default.
  };

  ~Lvl2TrigConfigObjyV2(){;}
  
  virtual void setLvl1TrigStruct(ooVArrayT<Lvl1Trig> taArray){ Lvl1TrigArray = taArray; }
  virtual Lvl1Trig getLvl1TrigStructObjy(int n) {return Lvl1TrigArray.elem(n); }
  virtual int getLvl1TrigArrayLength() const {return Lvl1TrigArray.size(); } 
  virtual void setLvl2Status(const int t){Lvl2Status = t;}
  virtual int getLvl2Status() const {return Lvl2Status;}
  
protected:
  ooVArrayT<Lvl1Trig> Lvl1TrigArray;
  int Lvl2Status; // 1 = ON, 0 = OFF
};
#endif // __LVL2TRIGCONFIGOBJYV2_DDL__
