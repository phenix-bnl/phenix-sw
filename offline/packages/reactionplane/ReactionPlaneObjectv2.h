
#ifndef __REACTIONPLANEOBJECTV2_H__
#define __REACTIONPLANEOBJECTV2_H__

#include <iostream>
#include "ReactionPlaneObject.h"

class ReactionPlaneObjectv2 : public ReactionPlaneObject 
{

 public:
  ReactionPlaneObjectv2();
  virtual ~ReactionPlaneObjectv2() {}

  ReactionPlaneObjectv2* clone() const { return new ReactionPlaneObjectv2(*this); }

  void Reset();
  void identify(std::ostream &os = std::cout) const;
  int isValid() const;

  // Get
  // Weight
  float getBBCsumW00() const { return BBCsumW00;}
  float getBBCsumW01() const { return BBCsumW01;}
  float getBBCsumW02() const { return BBCsumW02;}
  float getBBCsumW10() const { return BBCsumW10;}
  float getBBCsumW11() const { return BBCsumW11;}
  float getBBCsumW12() const { return BBCsumW12;}

  float getSMDsumW00() const { return SMDsumW00;}
  float getSMDsumW01() const { return SMDsumW01;}
  float getSMDsumW02() const { return SMDsumW02;}

  float getFCLsumW00() const { return FCLsumW00;}
  float getFCLsumW01() const { return FCLsumW01;}
  float getFCLsumW02() const { return FCLsumW02;}

  float getCNTsumW10() const { return CNTsumW10;}
  float getCNTsumW11() const { return CNTsumW11;}
  float getCNTsumW12() const { return CNTsumW12;}
  float getCNTsumW13() const { return CNTsumW13;}
  float getCNTsumW14() const { return CNTsumW14;}

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

  // RP
  float getBBCrp00() const { return BBCrp00; }
  float getBBCrp01() const { return BBCrp01; }
  float getBBCrp02() const { return BBCrp02; }
  float getBBCrp10() const { return BBCrp10; }
  float getBBCrp11() const { return BBCrp11; }
  float getBBCrp12() const { return BBCrp12; }
                                           
  float getSMDrp00() const { return SMDrp00; }
  float getSMDrp01() const { return SMDrp01; }
  float getSMDrp02() const { return SMDrp02; }
                                           
  float getFCLrp00() const { return FCLrp00; }
  float getFCLrp01() const { return FCLrp01; }
  float getFCLrp02() const { return FCLrp02; }
                                           
  float getCNTrp10() const { return CNTrp10; }
  float getCNTrp11() const { return CNTrp11; }
  float getCNTrp12() const { return CNTrp12; }
  float getCNTrp13() const { return CNTrp13; }
  float getCNTrp14() const { return CNTrp14; }

  // Set
  // Weight
  void setBBCsumW00(const float w) {BBCsumW00 = w; return; }
  void setBBCsumW01(const float w) {BBCsumW01 = w; return; }
  void setBBCsumW02(const float w) {BBCsumW02 = w; return; }
  void setBBCsumW10(const float w) {BBCsumW10 = w; return; }
  void setBBCsumW11(const float w) {BBCsumW11 = w; return; }
  void setBBCsumW12(const float w) {BBCsumW12 = w; return; }

  void setSMDsumW00(const float w) {SMDsumW00 = w; return; }
  void setSMDsumW01(const float w) {SMDsumW01 = w; return; }
  void setSMDsumW02(const float w) {SMDsumW02 = w; return; }

  void setFCLsumW00(const float w) {FCLsumW00 = w; return; }
  void setFCLsumW01(const float w) {FCLsumW01 = w; return; }
  void setFCLsumW02(const float w) {FCLsumW02 = w; return; }

  void setCNTsumW10(const float w) {CNTsumW10 = w; return; }
  void setCNTsumW11(const float w) {CNTsumW11 = w; return; }
  void setCNTsumW12(const float w) {CNTsumW12 = w; return; }
  void setCNTsumW13(const float w) {CNTsumW13 = w; return; }
  void setCNTsumW14(const float w) {CNTsumW14 = w; return; }

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


  // RP
  void setBBCrp00(const float val) { BBCrp00 = val; }
  void setBBCrp01(const float val) { BBCrp01 = val; }
  void setBBCrp02(const float val) { BBCrp02 = val; }
  void setBBCrp10(const float val) { BBCrp10 = val; }
  void setBBCrp11(const float val) { BBCrp11 = val; }
  void setBBCrp12(const float val) { BBCrp12 = val; }

  void setSMDrp00(const float val) { SMDrp00 = val; }
  void setSMDrp01(const float val) { SMDrp01 = val; }
  void setSMDrp02(const float val) { SMDrp02 = val; }

  void setFCLrp00(const float val) { FCLrp00 = val; }
  void setFCLrp01(const float val) { FCLrp01 = val; }
  void setFCLrp02(const float val) { FCLrp02 = val; }

  void setCNTrp10(const float val) { CNTrp10 = val; }
  void setCNTrp11(const float val) { CNTrp11 = val; }
  void setCNTrp12(const float val) { CNTrp12 = val; }
  void setCNTrp13(const float val) { CNTrp13 = val; }
  void setCNTrp14(const float val) { CNTrp14 = val; }

 private:

  // Weight
  float BBCsumW00;
  float BBCsumW01;
  float BBCsumW02;
  float BBCsumW10;
  float BBCsumW11;
  float BBCsumW12;

  float SMDsumW00;
  float SMDsumW01;
  float SMDsumW02;
 
  float FCLsumW00;
  float FCLsumW01;
  float FCLsumW02;
  
  float CNTsumW10;
  float CNTsumW11;
  float CNTsumW12;
  float CNTsumW13;
  float CNTsumW14;

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


  // RP
  float BBCrp00;
  float BBCrp01;
  float BBCrp02;
  float BBCrp10;
  float BBCrp11;
  float BBCrp12;

  float SMDrp00;
  float SMDrp01;
  float SMDrp02;

  float FCLrp00;
  float FCLrp01;
  float FCLrp02;

  float CNTrp10;
  float CNTrp11;
  float CNTrp12;
  float CNTrp13;
  float CNTrp14;

  ClassDef(ReactionPlaneObjectv2,1)
};

#endif
