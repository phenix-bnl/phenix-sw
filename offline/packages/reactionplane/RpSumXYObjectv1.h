
#ifndef __RPSUMXYOBJECTV1_H__
#define __RPSUMXYOBJECTV1_H__

#include <iostream>
#include "RpSumXYObject.h"

class RpSumXYObjectv1 : public RpSumXYObject 
{

 public:
  RpSumXYObjectv1();
  virtual ~RpSumXYObjectv1() {}

  RpSumXYObjectv1* clone() const { return new RpSumXYObjectv1(*this); }

  void Reset();
  void identify(std::ostream &os = std::cout) const;
  int isValid() const;

  // Get
  // Weight
  float getBBCsumW0() const { return BBCsumW0;}
  float getBBCsumW1() const { return BBCsumW1;}
  float getBBCsumW2() const { return BBCsumW2;}

  float getFCLsumW0() const { return FCLsumW0;}
  float getFCLsumW1() const { return FCLsumW1;}
  float getFCLsumW2() const { return FCLsumW2;}

  float getCNTsumW0() const { return CNTsumW0;}
  float getCNTsumW1() const { return CNTsumW1;}
  float getCNTsumW2() const { return CNTsumW2;}
  float getCNTsumW3() const { return CNTsumW3;}
  float getCNTsumW4() const { return CNTsumW4;}

  // sumX and sumY
  float getBBCsumX00() const { return BBCsumX00;}
  float getBBCsumX01() const { return BBCsumX01;}
  float getBBCsumX02() const { return BBCsumX02;}
  float getBBCsumX10() const { return BBCsumX10;}
  float getBBCsumX11() const { return BBCsumX11;}
  float getBBCsumX12() const { return BBCsumX12;}
  float getBBCsumY00() const { return BBCsumY00;}
  float getBBCsumY01() const { return BBCsumY01;}
  float getBBCsumY02() const { return BBCsumY02;}
  float getBBCsumY10() const { return BBCsumY10;}
  float getBBCsumY11() const { return BBCsumY11;}
  float getBBCsumY12() const { return BBCsumY12;}

  float getSMDsumX00() const { return SMDsumX00;}
  float getSMDsumX01() const { return SMDsumX01;}
  float getSMDsumX02() const { return SMDsumX02;}
  float getSMDsumY00() const { return SMDsumY00;}
  float getSMDsumY01() const { return SMDsumY01;}
  float getSMDsumY02() const { return SMDsumY02;}

  float getFCLsumX00() const { return FCLsumX00;}
  float getFCLsumX01() const { return FCLsumX01;}
  float getFCLsumX02() const { return FCLsumX02;}
  float getFCLsumY00() const { return FCLsumY00;}
  float getFCLsumY01() const { return FCLsumY01;}
  float getFCLsumY02() const { return FCLsumY02;}

  float getCNTsumX10() const { return CNTsumX10;}
  float getCNTsumX11() const { return CNTsumX11;}
  float getCNTsumX12() const { return CNTsumX12;}
  float getCNTsumX13() const { return CNTsumX13;}
  float getCNTsumX14() const { return CNTsumX14;}
  float getCNTsumY10() const { return CNTsumY10;}
  float getCNTsumY11() const { return CNTsumY11;}
  float getCNTsumY12() const { return CNTsumY12;}
  float getCNTsumY13() const { return CNTsumY13;}
  float getCNTsumY14() const { return CNTsumY14;}

  // Set
  // Weight
  void setBBCsumW0(const float w) {BBCsumW0 = w; return; }
  void setBBCsumW1(const float w) {BBCsumW1 = w; return; }
  void setBBCsumW2(const float w) {BBCsumW2 = w; return; }

  void setFCLsumW0(const float w) {FCLsumW0 = w; return; }
  void setFCLsumW1(const float w) {FCLsumW1 = w; return; }
  void setFCLsumW2(const float w) {FCLsumW2 = w; return; }

  void setCNTsumW0(const float w) {CNTsumW0 = w; return; }
  void setCNTsumW1(const float w) {CNTsumW1 = w; return; }
  void setCNTsumW2(const float w) {CNTsumW2 = w; return; }
  void setCNTsumW3(const float w) {CNTsumW3 = w; return; }
  void setCNTsumW4(const float w) {CNTsumW4 = w; return; }

  // sumX and sumY
  void setBBCsumX00(const float sumx) {BBCsumX00 = sumx; return; }
  void setBBCsumX01(const float sumx) {BBCsumX01 = sumx; return; }
  void setBBCsumX02(const float sumx) {BBCsumX02 = sumx; return; }
  void setBBCsumX10(const float sumx) {BBCsumX10 = sumx; return; }
  void setBBCsumX11(const float sumx) {BBCsumX11 = sumx; return; }
  void setBBCsumX12(const float sumx) {BBCsumX12 = sumx; return; }
  void setBBCsumY00(const float sumy) {BBCsumY00 = sumy; return; }
  void setBBCsumY01(const float sumy) {BBCsumY01 = sumy; return; }
  void setBBCsumY02(const float sumy) {BBCsumY02 = sumy; return; }
  void setBBCsumY10(const float sumy) {BBCsumY10 = sumy; return; }
  void setBBCsumY11(const float sumy) {BBCsumY11 = sumy; return; }
  void setBBCsumY12(const float sumy) {BBCsumY12 = sumy; return; }

  void setSMDsumX00(const float sumx) {SMDsumX00 = sumx; return; }
  void setSMDsumX01(const float sumx) {SMDsumX01 = sumx; return; }
  void setSMDsumX02(const float sumx) {SMDsumX02 = sumx; return; }
  void setSMDsumY00(const float sumy) {SMDsumY00 = sumy; return; }
  void setSMDsumY01(const float sumy) {SMDsumY01 = sumy; return; }
  void setSMDsumY02(const float sumy) {SMDsumY02 = sumy; return; }

  void setFCLsumX00(const float sumx) {FCLsumX00 = sumx; return; }
  void setFCLsumX01(const float sumx) {FCLsumX01 = sumx; return; }
  void setFCLsumX02(const float sumx) {FCLsumX02 = sumx; return; }
  void setFCLsumY00(const float sumy) {FCLsumY00 = sumy; return; }
  void setFCLsumY01(const float sumy) {FCLsumY01 = sumy; return; }
  void setFCLsumY02(const float sumy) {FCLsumY02 = sumy; return; }

  void setCNTsumX10(const float sumx) {CNTsumX10 = sumx; return; }
  void setCNTsumX11(const float sumx) {CNTsumX11 = sumx; return; }
  void setCNTsumX12(const float sumx) {CNTsumX12 = sumx; return; }
  void setCNTsumX13(const float sumx) {CNTsumX13 = sumx; return; }
  void setCNTsumX14(const float sumx) {CNTsumX14 = sumx; return; }
  void setCNTsumY10(const float sumy) {CNTsumY10 = sumy; return; }
  void setCNTsumY11(const float sumy) {CNTsumY11 = sumy; return; }
  void setCNTsumY12(const float sumy) {CNTsumY12 = sumy; return; }
  void setCNTsumY13(const float sumy) {CNTsumY13 = sumy; return; }
  void setCNTsumY14(const float sumy) {CNTsumY14 = sumy; return; }


 private:

  // Weight
  float BBCsumW0;
  float BBCsumW1;
  float BBCsumW2;

  float FCLsumW0;
  float FCLsumW1;
  float FCLsumW2;
  
  float CNTsumW0;
  float CNTsumW1;
  float CNTsumW2;
  float CNTsumW3;
  float CNTsumW4;

  // sumX and sumY
  float BBCsumX00;
  float BBCsumX01;
  float BBCsumX02;
  float BBCsumX10;
  float BBCsumX11;
  float BBCsumX12;
  float BBCsumY00;
  float BBCsumY01;
  float BBCsumY02;
  float BBCsumY10;
  float BBCsumY11;
  float BBCsumY12;

  float SMDsumX00;
  float SMDsumX01;
  float SMDsumX02;
  float SMDsumY00;
  float SMDsumY01;
  float SMDsumY02;
 
  float FCLsumX00;
  float FCLsumX01;
  float FCLsumX02;
  float FCLsumY00;
  float FCLsumY01;
  float FCLsumY02;
  
  float CNTsumX10;
  float CNTsumX11;
  float CNTsumX12;
  float CNTsumX13;
  float CNTsumX14;
  float CNTsumY10;
  float CNTsumY11;
  float CNTsumY12;
  float CNTsumY13;
  float CNTsumY14;


  ClassDef(RpSumXYObjectv1,1)
};

#endif
