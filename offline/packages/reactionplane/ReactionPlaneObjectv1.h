
#ifndef __REACTIONPLANEOBJECTV1_H__
#define __REACTIONPLANEOBJECTV1_H__

#include <iostream>
#include "ReactionPlaneObject.h"

class ReactionPlaneObjectv1 : public ReactionPlaneObject 
{

 public:
  ReactionPlaneObjectv1();
  virtual ~ReactionPlaneObjectv1() {}

  ReactionPlaneObjectv1* clone() const { return new ReactionPlaneObjectv1(*this); }

  void Reset();
  void identify(std::ostream &os = std::cout) const;
  int isValid() const;
  // Get
  float getBBCrp00() const { return BBCrp00; }
  float getBBCrp01() const { return BBCrp01; }
  float getBBCrp02() const { return BBCrp02; }
  float getBBCrp10() const { return BBCrp10; }
  float getBBCrp11() const { return BBCrp11; }
  float getBBCrp12() const { return BBCrp12; }
                                           
  float getSMDrp00() const { return SMDrp00; }
  float getSMDrp01() const { return SMDrp01; }
  float getSMDrp02() const { return SMDrp02; }
                                           
  float getMVDrp00() const { return MVDrp00; }
  float getMVDrp01() const { return MVDrp01; }
  float getMVDrp02() const { return MVDrp02; }
  float getMVDrp10() const { return MVDrp10; }
  float getMVDrp11() const { return MVDrp11; }
  float getMVDrp12() const { return MVDrp12; }
                                           
  float getFCLrp00() const { return FCLrp00; }
  float getFCLrp01() const { return FCLrp01; }
  float getFCLrp02() const { return FCLrp02; }
                                           
  float getCNTrp10() const { return CNTrp10; }
  float getCNTrp11() const { return CNTrp11; }
  float getCNTrp12() const { return CNTrp12; }
  float getCNTrp13() const { return CNTrp13; }
  float getCNTrp14() const { return CNTrp14; }

  // Set
  void setBBCrp00(const float val) { BBCrp00 = val; }
  void setBBCrp01(const float val) { BBCrp01 = val; }
  void setBBCrp02(const float val) { BBCrp02 = val; }
  void setBBCrp10(const float val) { BBCrp10 = val; }
  void setBBCrp11(const float val) { BBCrp11 = val; }
  void setBBCrp12(const float val) { BBCrp12 = val; }

  void setSMDrp00(const float val) { SMDrp00 = val; }
  void setSMDrp01(const float val) { SMDrp01 = val; }
  void setSMDrp02(const float val) { SMDrp02 = val; }

  void setMVDrp00(const float val) { MVDrp00 = val; }
  void setMVDrp01(const float val) { MVDrp01 = val; }
  void setMVDrp02(const float val) { MVDrp02 = val; }
  void setMVDrp10(const float val) { MVDrp10 = val; }
  void setMVDrp11(const float val) { MVDrp11 = val; }
  void setMVDrp12(const float val) { MVDrp12 = val; }

  void setFCLrp00(const float val) { FCLrp00 = val; }
  void setFCLrp01(const float val) { FCLrp01 = val; }
  void setFCLrp02(const float val) { FCLrp02 = val; }

  void setCNTrp10(const float val) { CNTrp10 = val; }
  void setCNTrp11(const float val) { CNTrp11 = val; }
  void setCNTrp12(const float val) { CNTrp12 = val; }
  void setCNTrp13(const float val) { CNTrp13 = val; }
  void setCNTrp14(const float val) { CNTrp14 = val; }

 private:

  float BBCrp00;
  float BBCrp01;
  float BBCrp02;
  float BBCrp10;
  float BBCrp11;
  float BBCrp12;

  float SMDrp00;
  float SMDrp01;
  float SMDrp02;

  float MVDrp00;
  float MVDrp01;
  float MVDrp02;
  float MVDrp10;
  float MVDrp11;
  float MVDrp12;

  float FCLrp00;
  float FCLrp01;
  float FCLrp02;

  float CNTrp10;
  float CNTrp11;
  float CNTrp12;
  float CNTrp13;
  float CNTrp14;

  ClassDef(ReactionPlaneObjectv1,1)
};

#endif
