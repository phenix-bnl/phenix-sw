#ifndef __L2TofTwoHitTriggerMicroV1_H__
#define __L2TofTwoHitTriggerMicroV1_H__

#include <L2TofTwoHitTriggerDST.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

class L2TofTwoHitTriggerMicroV1 : public L2TofTwoHitTriggerDST
{

 public:
  L2TofTwoHitTriggerMicroV1();
  ~L2TofTwoHitTriggerMicroV1();

  //PHObject stuff
  void identify(std::ostream& os = std::cout) const
    {
      os << "I am L2TofTwoHitTriggerMicroV1" <<std::endl;
      os << "Number of slats hit is "<<numSlatsHit<<std::endl;
    }
  void Reset();
  int isValid() const;

  //TofTrigger Primitive stuff
  void addHit(int index);

  void set_numTofHits(int hits) { numSlatsHit = hits; }
  void set_SlatID(int index, int id);
  void set_EnergyLoss(int index, float loss);
  void set_ySlat(int index, float y);

  int get_numTofHits() { return numSlatsHit; }
  int get_SlatID(int index);
  float get_EnergyLoss(int index);
  float get_ySlat(int index);

 protected:
  int numSlatsHit;
  TClonesArray *L2TofHits;

  //define the class
  ClassDef(L2TofTwoHitTriggerMicroV1, 1)
};

#endif /* __L2TOFTWOHITTRIGGERMICROV1_H__ */

