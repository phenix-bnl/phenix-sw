
#ifndef __RPSUMXYOBJECTV2_H__
#define __RPSUMXYOBJECTV2_H__

#include <iostream>
#include <RpSumXYObjectv1.h>

class RpSumXYObjectv2 : public RpSumXYObjectv1
{

 public:
  RpSumXYObjectv2();
  virtual ~RpSumXYObjectv2() {}

  RpSumXYObjectv2* clone() const { return new RpSumXYObjectv2(*this); }

  void Reset();
  void identify(std::ostream &os = std::cout) const;
  int isValid() const;

  // Get
  // Weight
  float getRXNsumW0() const { return RXNsumW0;}
  float getRXNsumW1() const { return RXNsumW1;}
  float getRXNsumW2() const { return RXNsumW2;}
  float getRXNsumW3() const { return RXNsumW3;}
  float getRXNsumW4() const { return RXNsumW4;}
  float getRXNsumW5() const { return RXNsumW5;}
  float getRXNsumW6() const { return RXNsumW6;}
  float getRXNsumW7() const { return RXNsumW7;}
  float getRXNsumW8() const { return RXNsumW8;}

  float getMPCsumW0() const { return MPCsumW0;} 
  float getMPCsumW1() const { return MPCsumW1;}
  float getMPCsumW2() const { return MPCsumW2;} 

//#############################################################//

  // sumX and sumY
  float getRXNsumX00() const { return RXNsumX00;}
  float getRXNsumX01() const { return RXNsumX01;}
  float getRXNsumX02() const { return RXNsumX02;}
  float getRXNsumX03() const { return RXNsumX03;}
  float getRXNsumX04() const { return RXNsumX04;}
  float getRXNsumX05() const { return RXNsumX05;}
  float getRXNsumX06() const { return RXNsumX06;}
  float getRXNsumX07() const { return RXNsumX07;}
  float getRXNsumX08() const { return RXNsumX08;}
  float getRXNsumY00() const { return RXNsumY00;}
  float getRXNsumY01() const { return RXNsumY01;}
  float getRXNsumY02() const { return RXNsumY02;}
  float getRXNsumY03() const { return RXNsumY03;}
  float getRXNsumY04() const { return RXNsumY04;}
  float getRXNsumY05() const { return RXNsumY05;}
  float getRXNsumY06() const { return RXNsumY06;}
  float getRXNsumY07() const { return RXNsumY07;}
  float getRXNsumY08() const { return RXNsumY08;}

  float getRXNsumX10() const { return RXNsumX10;}
  float getRXNsumX11() const { return RXNsumX11;}
  float getRXNsumX12() const { return RXNsumX12;}
  float getRXNsumX13() const { return RXNsumX13;}
  float getRXNsumX14() const { return RXNsumX14;}
  float getRXNsumX15() const { return RXNsumX15;}
  float getRXNsumX16() const { return RXNsumX16;}
  float getRXNsumX17() const { return RXNsumX17;}
  float getRXNsumX18() const { return RXNsumX18;}
  float getRXNsumY10() const { return RXNsumY10;}
  float getRXNsumY11() const { return RXNsumY11;}
  float getRXNsumY12() const { return RXNsumY12;}
  float getRXNsumY13() const { return RXNsumY13;}
  float getRXNsumY14() const { return RXNsumY14;}
  float getRXNsumY15() const { return RXNsumY15;}
  float getRXNsumY16() const { return RXNsumY16;}
  float getRXNsumY17() const { return RXNsumY17;}
  float getRXNsumY18() const { return RXNsumY18;}

  float getMPCsumX00() const { return MPCsumX00;}
  float getMPCsumX01() const { return MPCsumX01;}
  float getMPCsumX02() const { return MPCsumX02;}
  float getMPCsumY00() const { return MPCsumY00;}
  float getMPCsumY01() const { return MPCsumY01;}
  float getMPCsumY02() const { return MPCsumY02;}
  
  float getMPCsumX10() const { return MPCsumX10;}
  float getMPCsumX11() const { return MPCsumX11;}
  float getMPCsumX12() const { return MPCsumX12;}
  float getMPCsumY10() const { return MPCsumY10;}
  float getMPCsumY11() const { return MPCsumY11;}
  float getMPCsumY12() const { return MPCsumY12;}

//###################################################################//

  // Set
  // Weight
  void setRXNsumW0(const float w) {RXNsumW0 = w; return; }
  void setRXNsumW1(const float w) {RXNsumW1 = w; return; }
  void setRXNsumW2(const float w) {RXNsumW2 = w; return; }
  void setRXNsumW3(const float w) {RXNsumW3 = w; return; }
  void setRXNsumW4(const float w) {RXNsumW4 = w; return; }
  void setRXNsumW5(const float w) {RXNsumW5 = w; return; }
  void setRXNsumW6(const float w) {RXNsumW6 = w; return; }
  void setRXNsumW7(const float w) {RXNsumW7 = w; return; }
  void setRXNsumW8(const float w) {RXNsumW8 = w; return; }

  void setMPCsumW0(const float w) {MPCsumW0 = w; return; }
  void setMPCsumW1(const float w) {MPCsumW1 = w; return; }
  void setMPCsumW2(const float w) {MPCsumW2 = w; return; }

//####################################################################//

  // sumX and sumY
  void setRXNsumX00(const float sumx) {RXNsumX00 = sumx; return; }
  void setRXNsumX01(const float sumx) {RXNsumX01 = sumx; return; }
  void setRXNsumX02(const float sumx) {RXNsumX02 = sumx; return; }
  void setRXNsumX03(const float sumx) {RXNsumX03 = sumx; return; }
  void setRXNsumX04(const float sumx) {RXNsumX04 = sumx; return; }
  void setRXNsumX05(const float sumx) {RXNsumX05 = sumx; return; }
  void setRXNsumX06(const float sumx) {RXNsumX06 = sumx; return; }
  void setRXNsumX07(const float sumx) {RXNsumX07 = sumx; return; }
  void setRXNsumX08(const float sumx) {RXNsumX08 = sumx; return; }
  void setRXNsumY00(const float sumy) {RXNsumY00 = sumy; return; }
  void setRXNsumY01(const float sumy) {RXNsumY01 = sumy; return; }
  void setRXNsumY02(const float sumy) {RXNsumY02 = sumy; return; }
  void setRXNsumY03(const float sumy) {RXNsumY03 = sumy; return; }
  void setRXNsumY04(const float sumy) {RXNsumY04 = sumy; return; }
  void setRXNsumY05(const float sumy) {RXNsumY05 = sumy; return; }
  void setRXNsumY06(const float sumy) {RXNsumY06 = sumy; return; }
  void setRXNsumY07(const float sumy) {RXNsumY07 = sumy; return; }
  void setRXNsumY08(const float sumy) {RXNsumY08 = sumy; return; }

  void setRXNsumX10(const float sumx) {RXNsumX10 = sumx; return; }
  void setRXNsumX11(const float sumx) {RXNsumX11 = sumx; return; }
  void setRXNsumX12(const float sumx) {RXNsumX12 = sumx; return; }
  void setRXNsumX13(const float sumx) {RXNsumX13 = sumx; return; }
  void setRXNsumX14(const float sumx) {RXNsumX14 = sumx; return; }
  void setRXNsumX15(const float sumx) {RXNsumX15 = sumx; return; }
  void setRXNsumX16(const float sumx) {RXNsumX16 = sumx; return; }
  void setRXNsumX17(const float sumx) {RXNsumX17 = sumx; return; }
  void setRXNsumX18(const float sumx) {RXNsumX18 = sumx; return; }
  void setRXNsumY10(const float sumy) {RXNsumY10 = sumy; return; }
  void setRXNsumY11(const float sumy) {RXNsumY11 = sumy; return; }
  void setRXNsumY12(const float sumy) {RXNsumY12 = sumy; return; }
  void setRXNsumY13(const float sumy) {RXNsumY13 = sumy; return; }
  void setRXNsumY14(const float sumy) {RXNsumY14 = sumy; return; }
  void setRXNsumY15(const float sumy) {RXNsumY15 = sumy; return; }
  void setRXNsumY16(const float sumy) {RXNsumY16 = sumy; return; }
  void setRXNsumY17(const float sumy) {RXNsumY17 = sumy; return; }
  void setRXNsumY18(const float sumy) {RXNsumY18 = sumy; return; }

  void setMPCsumX00(const float sumx) {MPCsumX00 = sumx; return; }
  void setMPCsumX01(const float sumx) {MPCsumX01 = sumx; return; }
  void setMPCsumX02(const float sumx) {MPCsumX02 = sumx; return; }
  void setMPCsumY00(const float sumy) {MPCsumY00 = sumy; return; }
  void setMPCsumY01(const float sumy) {MPCsumY01 = sumy; return; }
  void setMPCsumY02(const float sumy) {MPCsumY02 = sumy; return; }
  void setMPCsumX10(const float sumx) {MPCsumX10 = sumx; return; }
  void setMPCsumX11(const float sumx) {MPCsumX11 = sumx; return; }
  void setMPCsumX12(const float sumx) {MPCsumX12 = sumx; return; }
  void setMPCsumY10(const float sumy) {MPCsumY10 = sumy; return; }
  void setMPCsumY11(const float sumy) {MPCsumY11 = sumy; return; }
  void setMPCsumY12(const float sumy) {MPCsumY12 = sumy; return; }

//#########################################################################//

 private:

  // Weight
  float RXNsumW0;
  float RXNsumW1;
  float RXNsumW2;
  float RXNsumW3;
  float RXNsumW4;
  float RXNsumW5;
  float RXNsumW6;
  float RXNsumW7;
  float RXNsumW8;

  float MPCsumW0;
  float MPCsumW1;
  float MPCsumW2;

//#########################################################################//

  // sumX and sumY
  float RXNsumX00;
  float RXNsumX01;
  float RXNsumX02;
  float RXNsumX03;
  float RXNsumX04;
  float RXNsumX05;
  float RXNsumX06;
  float RXNsumX07;
  float RXNsumX08;
  float RXNsumY00;
  float RXNsumY01;
  float RXNsumY02;
  float RXNsumY03;
  float RXNsumY04;
  float RXNsumY05;
  float RXNsumY06;
  float RXNsumY07;
  float RXNsumY08;

  float RXNsumX10;
  float RXNsumX11;
  float RXNsumX12;
  float RXNsumX13;
  float RXNsumX14;
  float RXNsumX15;
  float RXNsumX16;
  float RXNsumX17;
  float RXNsumX18;
  float RXNsumY10;
  float RXNsumY11;
  float RXNsumY12;
  float RXNsumY13;
  float RXNsumY14;
  float RXNsumY15;
  float RXNsumY16;
  float RXNsumY17;
  float RXNsumY18;

  float MPCsumX00;
  float MPCsumX01;
  float MPCsumX02;
  float MPCsumY00;
  float MPCsumY01;
  float MPCsumY02;

  float MPCsumX10;
  float MPCsumX11;
  float MPCsumX12;
  float MPCsumY10;
  float MPCsumY11;
  float MPCsumY12;

//######################################################################//

  ClassDef(RpSumXYObjectv2,1)
};

#endif
