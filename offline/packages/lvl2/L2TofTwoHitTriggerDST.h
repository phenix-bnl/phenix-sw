#ifndef __L2TofTwoHitTriggerDST_H__
#define __L2TofTwoHitTriggerDST_H__

#include <PHObject.h>

class L2TofTwoHitTriggerDST : public PHObject
{
 public:

  L2TofTwoHitTriggerDST() {}
  virtual ~L2TofTwoHitTriggerDST() {}

  //PHObject stuff
  virtual void identify(std::ostream& os = std::cout) const
    {
      os << "virtual L2TofTwoHitTriggerDST object";
    }
  virtual void Reset() = 0;
  virtual int isValid() const = 0;

  //TofTrigger Primitive stuff
  virtual void addHit(int index) = 0;

  virtual void set_numTofHits(int hits) = 0;
  virtual void set_SlatID(int index, int id) = 0;
  virtual void set_EnergyLoss(int index, float loss) = 0;
  virtual void set_ySlat(int index, float y) = 0;

  virtual int get_numTofHits() = 0;
  virtual int get_SlatID(int index) = 0;
  virtual float get_EnergyLoss(int index) = 0;
  virtual float get_ySlat(int index) = 0;

  //define the class
  ClassDef(L2TofTwoHitTriggerDST, 1)
};

#endif /* __L2TofTwoHitTriggerDST_H__ */

