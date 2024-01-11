#ifndef __L2TOFHITMICROV1_H__
#define __L2TOFHITMICROV1_H__

#include <PHObject.h>

class L2TofHitMicroV1 : public TObject
{
 protected:
  int SlatID;
  float EnergyLoss;
  float ySlat;

 public:
  L2TofHitMicroV1();
  ~L2TofHitMicroV1() {}

  void set_SlatID(int id) { SlatID = id; }
  void set_EnergyLoss(float eloss) { EnergyLoss = eloss; }
  void set_ySlat(float y) { ySlat = y; }

  int get_SlatID() { return SlatID; }
  float get_EnergyLoss() { return EnergyLoss; }
  float get_ySlat() {return ySlat; }

  ClassDef(L2TofHitMicroV1, 1)
};

#endif /* __L2TOFHITMICROV1_H__ */
