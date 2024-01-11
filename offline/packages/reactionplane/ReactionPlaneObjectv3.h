
#ifndef __REACTIONPLANEOBJECTV3_H__
#define __REACTIONPLANEOBJECTV3_H__

#include <iostream>
#include <ReactionPlaneObjectv2.h>

class ReactionPlaneObjectv3 : public ReactionPlaneObjectv2 
{

 public:
  ReactionPlaneObjectv3();
  virtual ~ReactionPlaneObjectv3() {}

  ReactionPlaneObjectv3* clone() const { return new ReactionPlaneObjectv3(*this); }

  void Reset();
  void identify(std::ostream &os = std::cout) const;
  int isValid() const;

  // Get
  // Weight

  float getRXNsumW00() const { return RXNsumW00;}
  float getRXNsumW01() const { return RXNsumW01;}
  float getRXNsumW02() const { return RXNsumW02;}
  float getRXNsumW03() const { return RXNsumW03;}
  float getRXNsumW04() const { return RXNsumW04;}
  float getRXNsumW05() const { return RXNsumW05;}
  float getRXNsumW06() const { return RXNsumW06;}
  float getRXNsumW07() const { return RXNsumW07;}
  float getRXNsumW08() const { return RXNsumW08;}

  float getRXNsumW10() const { return RXNsumW10;}
  float getRXNsumW11() const { return RXNsumW11;}
  float getRXNsumW12() const { return RXNsumW12;}
  float getRXNsumW13() const { return RXNsumW13;}
  float getRXNsumW14() const { return RXNsumW14;}
  float getRXNsumW15() const { return RXNsumW15;}
  float getRXNsumW16() const { return RXNsumW16;}
  float getRXNsumW17() const { return RXNsumW17;}
  float getRXNsumW18() const { return RXNsumW18;}

  float getMPCsumW00() const { return MPCsumW00;}
  float getMPCsumW01() const { return MPCsumW01;}
  float getMPCsumW02() const { return MPCsumW02;}

  float getMPCsumW10() const { return MPCsumW10;}
  float getMPCsumW11() const { return MPCsumW11;}
  float getMPCsumW12() const { return MPCsumW12;}

//#############################################################//

  // sumX and sumY

  float getRXNrp00() const { return RXNrp00; }
  float getRXNrp01() const { return RXNrp01; }
  float getRXNrp02() const { return RXNrp02; }
  float getRXNrp03() const { return RXNrp03; }
  float getRXNrp04() const { return RXNrp04; }
  float getRXNrp05() const { return RXNrp05; }
  float getRXNrp06() const { return RXNrp06; }
  float getRXNrp07() const { return RXNrp07; }
  float getRXNrp08() const { return RXNrp08; }

  float getRXNrp10() const { return RXNrp10; }
  float getRXNrp11() const { return RXNrp11; }
  float getRXNrp12() const { return RXNrp12; }
  float getRXNrp13() const { return RXNrp13; }
  float getRXNrp14() const { return RXNrp14; }
  float getRXNrp15() const { return RXNrp15; }
  float getRXNrp16() const { return RXNrp16; }
  float getRXNrp17() const { return RXNrp17; }
  float getRXNrp18() const { return RXNrp18; }

  float getMPCrp00() const { return MPCrp00; }
  float getMPCrp01() const { return MPCrp01; }
  float getMPCrp02() const { return MPCrp02; }

  float getMPCrp10() const { return MPCrp10; }
  float getMPCrp11() const { return MPCrp11; }
  float getMPCrp12() const { return MPCrp12; }

//###################################################################//

  // Set
  // Weight
  void setRXNsumW00(const float w) {RXNsumW00 = w; return; }
  void setRXNsumW01(const float w) {RXNsumW01 = w; return; }
  void setRXNsumW02(const float w) {RXNsumW02 = w; return; }
  void setRXNsumW03(const float w) {RXNsumW03 = w; return; }
  void setRXNsumW04(const float w) {RXNsumW04 = w; return; }
  void setRXNsumW05(const float w) {RXNsumW05 = w; return; }
  void setRXNsumW06(const float w) {RXNsumW06 = w; return; }
  void setRXNsumW07(const float w) {RXNsumW07 = w; return; }
  void setRXNsumW08(const float w) {RXNsumW08 = w; return; }

  void setRXNsumW10(const float w) {RXNsumW10 = w; return; }
  void setRXNsumW11(const float w) {RXNsumW11 = w; return; }
  void setRXNsumW12(const float w) {RXNsumW12 = w; return; }
  void setRXNsumW13(const float w) {RXNsumW13 = w; return; }
  void setRXNsumW14(const float w) {RXNsumW14 = w; return; }
  void setRXNsumW15(const float w) {RXNsumW15 = w; return; }
  void setRXNsumW16(const float w) {RXNsumW16 = w; return; }
  void setRXNsumW17(const float w) {RXNsumW17 = w; return; }
  void setRXNsumW18(const float w) {RXNsumW18 = w; return; }

  void setMPCsumW00(const float w) {MPCsumW00 = w; return; }
  void setMPCsumW01(const float w) {MPCsumW01 = w; return; }
  void setMPCsumW02(const float w) {MPCsumW02 = w; return; }

  void setMPCsumW10(const float w) {MPCsumW10 = w; return; }
  void setMPCsumW11(const float w) {MPCsumW11 = w; return; }
  void setMPCsumW12(const float w) {MPCsumW12 = w; return; }

//################################################################//

  // sumX and sumY
  // RP

  void setRXNrp00(const float val) { RXNrp00 = val; }
  void setRXNrp01(const float val) { RXNrp01 = val; }
  void setRXNrp02(const float val) { RXNrp02 = val; }
  void setRXNrp03(const float val) { RXNrp03 = val; }
  void setRXNrp04(const float val) { RXNrp04 = val; }
  void setRXNrp05(const float val) { RXNrp05 = val; }
  void setRXNrp06(const float val) { RXNrp06 = val; }
  void setRXNrp07(const float val) { RXNrp07 = val; }
  void setRXNrp08(const float val) { RXNrp08 = val; }

  void setRXNrp10(const float val) { RXNrp10 = val; }
  void setRXNrp11(const float val) { RXNrp11 = val; }
  void setRXNrp12(const float val) { RXNrp12 = val; }
  void setRXNrp13(const float val) { RXNrp13 = val; }
  void setRXNrp14(const float val) { RXNrp14 = val; }
  void setRXNrp15(const float val) { RXNrp15 = val; }
  void setRXNrp16(const float val) { RXNrp16 = val; }
  void setRXNrp17(const float val) { RXNrp17 = val; }
  void setRXNrp18(const float val) { RXNrp18 = val; }

  void setMPCrp00(const float val) { MPCrp00 = val; }
  void setMPCrp01(const float val) { MPCrp01 = val; }
  void setMPCrp02(const float val) { MPCrp02 = val; }

  void setMPCrp10(const float val) { MPCrp10 = val; }
  void setMPCrp11(const float val) { MPCrp11 = val; }
  void setMPCrp12(const float val) { MPCrp12 = val; }
  
//######################################################################//

 private:

  // Weight
  float RXNsumW00;
  float RXNsumW01;
  float RXNsumW02;
  float RXNsumW03;
  float RXNsumW04;
  float RXNsumW05;
  float RXNsumW06;
  float RXNsumW07;
  float RXNsumW08;
  float RXNsumW10;
  float RXNsumW11;
  float RXNsumW12;
  float RXNsumW13;
  float RXNsumW14;
  float RXNsumW15;
  float RXNsumW16;
  float RXNsumW17;
  float RXNsumW18;

  float MPCsumW00;
  float MPCsumW01;
  float MPCsumW02;
  float MPCsumW10;
  float MPCsumW11;
  float MPCsumW12;

//#############################################################//

  // sumX and sumY
  // RP

  float RXNrp00;
  float RXNrp01;
  float RXNrp02;
  float RXNrp03;
  float RXNrp04;
  float RXNrp05;
  float RXNrp06;
  float RXNrp07;
  float RXNrp08;
  float RXNrp10;
  float RXNrp11;
  float RXNrp12;
  float RXNrp13;
  float RXNrp14;
  float RXNrp15;
  float RXNrp16;
  float RXNrp17;
  float RXNrp18;

  float MPCrp00;
  float MPCrp01;
  float MPCrp02;
  float MPCrp10;
  float MPCrp11;
  float MPCrp12;

//################################################################//

  ClassDef(ReactionPlaneObjectv3,1)
};

#endif
