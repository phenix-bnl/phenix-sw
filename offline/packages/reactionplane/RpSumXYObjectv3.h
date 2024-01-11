
#ifndef __RPSUMXYOBJECTV3_H__
#define __RPSUMXYOBJECTV3_H__

#include <iostream>
#include "RpSumXYObject.h"
#include "RpConst.h"

#include <map>

/**
 * @brief  The implementation v3 class for a storage of reaction plane element 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */

class TClonesArray;

class RpSumXYObjectv3 : public RpSumXYObject 
{
 private:
  enum {
    E_QX=0,
    E_QY=1,
    E_W=2
  };

 public:
  RpSumXYObjectv3();
  RpSumXYObjectv3(const RpSumXYObjectv3& source);
  virtual ~RpSumXYObjectv3();


  virtual void copyTo(RpSumXYObjectv3& dest) const;

  virtual void Reset();
  virtual void identify(std::ostream &os = std::cout) const;
  virtual int  isValid() const;

  // I do not recommend  these obsolete functions.
  // Qx, Qy, W should be a group.

  // Get
  // Weight
  float getBBCsumW0() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,0), E_W); }
  float getBBCsumW1() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,0), E_W); }
  float getBBCsumW2() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,0), E_W); }

  float getCNTsumW0() const { return getValue(RP::calcIdCode(RP::ID_CNT,0,1), E_W); }
  float getCNTsumW1() const { return getValue(RP::calcIdCode(RP::ID_CNT,1,1), E_W); }
  float getCNTsumW2() const { return getValue(RP::calcIdCode(RP::ID_CNT,2,1), E_W); }
  float getCNTsumW3() const { return getValue(RP::calcIdCode(RP::ID_CNT,3,1), E_W); }
  float getCNTsumW4() const { return getValue(RP::calcIdCode(RP::ID_CNT,4,1), E_W); }

  float getMPCsumW0() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,0), E_W); }
  float getMPCsumW1() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,0), E_W); }
  float getMPCsumW2() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,0), E_W); }

  // sumX and sumY
  float getBBCsumX00() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,0), E_QX); }
  float getBBCsumX01() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,0), E_QX); }
  float getBBCsumX02() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,0), E_QX); }
  float getBBCsumX10() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,1), E_QX); }
  float getBBCsumX11() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,1), E_QX); }
  float getBBCsumX12() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,1), E_QX); }
  float getBBCsumY00() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,0), E_QY); }
  float getBBCsumY01() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,0), E_QY); }
  float getBBCsumY02() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,0), E_QY); }
  float getBBCsumY10() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,1), E_QY); }
  float getBBCsumY11() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,1), E_QY); }
  float getBBCsumY12() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,1), E_QY); }

  float getCNTsumX10() const { return getValue(RP::calcIdCode(RP::ID_CNT,0,1), E_QX); }
  float getCNTsumX11() const { return getValue(RP::calcIdCode(RP::ID_CNT,1,1), E_QX); }
  float getCNTsumX12() const { return getValue(RP::calcIdCode(RP::ID_CNT,2,1), E_QX); }
  float getCNTsumX13() const { return getValue(RP::calcIdCode(RP::ID_CNT,3,1), E_QX); }
  float getCNTsumX14() const { return getValue(RP::calcIdCode(RP::ID_CNT,4,1), E_QX); }
  float getCNTsumY10() const { return getValue(RP::calcIdCode(RP::ID_CNT,0,1), E_QY); }
  float getCNTsumY11() const { return getValue(RP::calcIdCode(RP::ID_CNT,1,1), E_QY); }
  float getCNTsumY12() const { return getValue(RP::calcIdCode(RP::ID_CNT,2,1), E_QY); }
  float getCNTsumY13() const { return getValue(RP::calcIdCode(RP::ID_CNT,3,1), E_QY); }
  float getCNTsumY14() const { return getValue(RP::calcIdCode(RP::ID_CNT,4,1), E_QY); }

  float getMPCsumX00() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,0), E_QX); }
  float getMPCsumX01() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,0), E_QX); }
  float getMPCsumX02() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,0), E_QX); }
  float getMPCsumX10() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,1), E_QX); }
  float getMPCsumX11() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,1), E_QX); }
  float getMPCsumX12() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,1), E_QX); }
  float getMPCsumY00() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,0), E_QY); }
  float getMPCsumY01() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,0), E_QY); }
  float getMPCsumY02() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,0), E_QY); }
  float getMPCsumY10() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,1), E_QY); }
  float getMPCsumY11() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,1), E_QY); }
  float getMPCsumY12() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,1), E_QY); }


  // Set
  // Weight
  void setBBCsumW0(const float w) { setValue("BBCSum00", RP::calcIdCode(RP::ID_BBC,0,0), E_W, w); }
  void setBBCsumW1(const float w) { setValue("BBCSum01", RP::calcIdCode(RP::ID_BBC,1,0), E_W, w); }
  void setBBCsumW2(const float w) { setValue("BBCSum02", RP::calcIdCode(RP::ID_BBC,2,0), E_W, w); }

  void setCNTsumW0(const float w) { setValue("CNTSum00", RP::calcIdCode(RP::ID_CNT,0,1), E_W, w); }
  void setCNTsumW1(const float w) { setValue("CNTSum01", RP::calcIdCode(RP::ID_CNT,1,1), E_W, w); }
  void setCNTsumW2(const float w) { setValue("CNTSum02", RP::calcIdCode(RP::ID_CNT,2,1), E_W, w); }
  void setCNTsumW3(const float w) { setValue("CNTSum03", RP::calcIdCode(RP::ID_CNT,3,1), E_W, w); }
  void setCNTsumW4(const float w) { setValue("CNTSum04", RP::calcIdCode(RP::ID_CNT,4,1), E_W, w); }

  void setMPCsumW0(const float w) { setValue("MPCSum00", RP::calcIdCode(RP::ID_MPC,0,0), E_W, w); }
  void setMPCsumW1(const float w) { setValue("MPCSum01", RP::calcIdCode(RP::ID_MPC,1,0), E_W, w); }
  void setMPCsumW2(const float w) { setValue("MPCSum02", RP::calcIdCode(RP::ID_MPC,2,0), E_W, w); }


  // sumX and sumY
  void setBBCsumX00(const float sumx) { setValue("BBCSum00", RP::calcIdCode(RP::ID_BBC,0,0), E_QX, sumx); }
  void setBBCsumX01(const float sumx) { setValue("BBCSum01", RP::calcIdCode(RP::ID_BBC,1,0), E_QX, sumx); }
  void setBBCsumX02(const float sumx) { setValue("BBCSum02", RP::calcIdCode(RP::ID_BBC,2,0), E_QX, sumx); }
  void setBBCsumX10(const float sumx) { setValue("BBCSum10", RP::calcIdCode(RP::ID_BBC,0,1), E_QX, sumx); }
  void setBBCsumX11(const float sumx) { setValue("BBCSum11", RP::calcIdCode(RP::ID_BBC,1,1), E_QX, sumx); }
  void setBBCsumX12(const float sumx) { setValue("BBCSum12", RP::calcIdCode(RP::ID_BBC,2,1), E_QX, sumx); }
  void setBBCsumY00(const float sumy) { setValue("BBCSum00", RP::calcIdCode(RP::ID_BBC,0,0), E_QY, sumy); }
  void setBBCsumY01(const float sumy) { setValue("BBCSum01", RP::calcIdCode(RP::ID_BBC,1,0), E_QY, sumy); }
  void setBBCsumY02(const float sumy) { setValue("BBCSum02", RP::calcIdCode(RP::ID_BBC,2,0), E_QY, sumy); }
  void setBBCsumY10(const float sumy) { setValue("BBCSum10", RP::calcIdCode(RP::ID_BBC,0,1), E_QY, sumy); }
  void setBBCsumY11(const float sumy) { setValue("BBCSum11", RP::calcIdCode(RP::ID_BBC,1,1), E_QY, sumy); }
  void setBBCsumY12(const float sumy) { setValue("BBCSum12", RP::calcIdCode(RP::ID_BBC,2,1), E_QY, sumy); }

  void setCNTsumX10(const float sumx) { setValue("CNTSum10", RP::calcIdCode(RP::ID_CNT,0,1), E_QX, sumx); }
  void setCNTsumX11(const float sumx) { setValue("CNTSum11", RP::calcIdCode(RP::ID_CNT,1,1), E_QX, sumx); }
  void setCNTsumX12(const float sumx) { setValue("CNTSum12", RP::calcIdCode(RP::ID_CNT,2,1), E_QX, sumx); }
  void setCNTsumX13(const float sumx) { setValue("CNTSum13", RP::calcIdCode(RP::ID_CNT,3,1), E_QX, sumx); }
  void setCNTsumX14(const float sumx) { setValue("CNTSum14", RP::calcIdCode(RP::ID_CNT,4,1), E_QX, sumx); }
  void setCNTsumY10(const float sumy) { setValue("CNTSum10", RP::calcIdCode(RP::ID_CNT,0,1), E_QY, sumy); }
  void setCNTsumY11(const float sumy) { setValue("CNTSum11", RP::calcIdCode(RP::ID_CNT,1,1), E_QY, sumy); }
  void setCNTsumY12(const float sumy) { setValue("CNTSum12", RP::calcIdCode(RP::ID_CNT,2,1), E_QY, sumy); }
  void setCNTsumY13(const float sumy) { setValue("CNTSum13", RP::calcIdCode(RP::ID_CNT,3,1), E_QY, sumy); }
  void setCNTsumY14(const float sumy) { setValue("CNTSum14", RP::calcIdCode(RP::ID_CNT,4,1), E_QY, sumy); }

  void setMPCsumX00(const float sumx) { setValue("MPCSum00", RP::calcIdCode(RP::ID_MPC,0,0), E_QX, sumx); }
  void setMPCsumX01(const float sumx) { setValue("MPCSum01", RP::calcIdCode(RP::ID_MPC,1,0), E_QX, sumx); }
  void setMPCsumX02(const float sumx) { setValue("MPCSum02", RP::calcIdCode(RP::ID_MPC,2,0), E_QX, sumx); }
  void setMPCsumX10(const float sumx) { setValue("MPCSum10", RP::calcIdCode(RP::ID_MPC,0,1), E_QX, sumx); }
  void setMPCsumX11(const float sumx) { setValue("MPCSum11", RP::calcIdCode(RP::ID_MPC,1,1), E_QX, sumx); }
  void setMPCsumX12(const float sumx) { setValue("MPCSum12", RP::calcIdCode(RP::ID_MPC,2,1), E_QX, sumx); }
  void setMPCsumY00(const float sumy) { setValue("MPCSum00", RP::calcIdCode(RP::ID_MPC,0,0), E_QY, sumy); }
  void setMPCsumY01(const float sumy) { setValue("MPCSum01", RP::calcIdCode(RP::ID_MPC,1,0), E_QY, sumy); }
  void setMPCsumY02(const float sumy) { setValue("MPCSum02", RP::calcIdCode(RP::ID_MPC,2,0), E_QY, sumy); }
  void setMPCsumY10(const float sumy) { setValue("MPCSum10", RP::calcIdCode(RP::ID_MPC,0,1), E_QY, sumy); }
  void setMPCsumY11(const float sumy) { setValue("MPCSum11", RP::calcIdCode(RP::ID_MPC,1,1), E_QY, sumy); }
  void setMPCsumY12(const float sumy) { setValue("MPCSum12", RP::calcIdCode(RP::ID_MPC,2,1), E_QY, sumy); }


  //////////
  virtual void AddRpSumXY(const char *name, const int id, const float qx, const float qy, const float w);

  virtual bool isRpSumXY(const char *name) const;

  virtual RpSnglSumXY* getRpSumXY(const int idcode) const;
  virtual int          getnRpSumXY() const;


  void identifyv3(std::ostream &os = std::cout) const;

 private:
  // wrapper for set method
  void  setValue(const char *name, const int idcode, const int type, const float val); // type:0=Qx, 1=Qy, 2=W
  float getValue(const int idcode, const int type) const; // type:0=Qx, 1=Qy, 2=W


 private:
  TClonesArray* RpSnglArray;
  std::map<int, int> m_vAryIndex; // int:idcode, int:aryIndex


  ClassDef(RpSumXYObjectv3,1)
};

#endif
