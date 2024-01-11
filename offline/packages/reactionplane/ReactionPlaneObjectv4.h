#ifndef __REACTIONPLANEOBJECTv4_H__                                                   
#define __REACTIONPLANEOBJECTv4_H__     

#include <iostream>
#include "ReactionPlaneObject.h"
#include "RpConst.h"

#include <map>

/**
 * @brief  The implementation v4 class for a storage of reaction plane element 
 *
 * Created on 11/16/2011 by Takashi Hachiya.
 */

class TClonesArray;

class ReactionPlaneObjectv4 : public ReactionPlaneObject 
{
 private:
  /*  enum {
    E_Psi=0,
    //E_QX=0,
    //E_QY=1,
    //E_W=2
  };
  */
 public:
  ReactionPlaneObjectv4();
  ReactionPlaneObjectv4(const ReactionPlaneObjectv4& source);
  virtual ~ReactionPlaneObjectv4();


  virtual void copyTo(ReactionPlaneObjectv4& dest) const;

  virtual void Reset();
  virtual void identify(std::ostream &os = std::cout) const;
  virtual int  isValid() const;

  // I do not recommend  these obsolete functions.
  // Qx, Qy, W should be a group.

  // Get
  // Weight
  /*
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
  */
  // sumX and sumY
  float getBBCrp00() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,0)); }
  float getBBCrp01() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,0)); }
  float getBBCrp02() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,0)); }
  float getBBCrp10() const { return getValue(RP::calcIdCode(RP::ID_BBC,0,1)); }
  float getBBCrp11() const { return getValue(RP::calcIdCode(RP::ID_BBC,1,1)); }
  float getBBCrp12() const { return getValue(RP::calcIdCode(RP::ID_BBC,2,1)); }

  float getCNTrp10() const { return getValue(RP::calcIdCode(RP::ID_CNT,0,1)); }
  float getCNTrp11() const { return getValue(RP::calcIdCode(RP::ID_CNT,1,1)); }
  float getCNTrp12() const { return getValue(RP::calcIdCode(RP::ID_CNT,2,1)); }
  float getCNTrp13() const { return getValue(RP::calcIdCode(RP::ID_CNT,3,1)); }
  float getCNTrp14() const { return getValue(RP::calcIdCode(RP::ID_CNT,4,1)); }

  float getMPCrp00() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,0)); }
  float getMPCrp01() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,0)); }
  float getMPCrp02() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,0)); }
  float getMPCrp10() const { return getValue(RP::calcIdCode(RP::ID_MPC,0,1)); }
  float getMPCrp11() const { return getValue(RP::calcIdCode(RP::ID_MPC,1,1)); }
  float getMPCrp12() const { return getValue(RP::calcIdCode(RP::ID_MPC,2,1)); }

  // Set
  // Weight
  /*
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
  */

  // sumX and sumY
  void setBBCrp00(const float rp) { setValue("BBCSum00", RP::calcIdCode(RP::ID_BBC,0,0), rp); }
  void setBBCrp01(const float rp) { setValue("BBCSum01", RP::calcIdCode(RP::ID_BBC,1,0), rp); }
  void setBBCrp02(const float rp) { setValue("BBCSum02", RP::calcIdCode(RP::ID_BBC,2,0), rp); }
  void setBBCrp10(const float rp) { setValue("BBCSum10", RP::calcIdCode(RP::ID_BBC,0,1), rp); }
  void setBBCrp11(const float rp) { setValue("BBCSum11", RP::calcIdCode(RP::ID_BBC,1,1), rp); }
  void setBBCrp12(const float rp) { setValue("BBCSum12", RP::calcIdCode(RP::ID_BBC,2,1), rp); }
 
  void setCNTrp10(const float rp) { setValue("CNTSum10", RP::calcIdCode(RP::ID_CNT,0,1), rp); }
  void setCNTrp11(const float rp) { setValue("CNTSum11", RP::calcIdCode(RP::ID_CNT,1,1), rp); }
  void setCNTrp12(const float rp) { setValue("CNTSum12", RP::calcIdCode(RP::ID_CNT,2,1), rp); }
  void setCNTrp13(const float rp) { setValue("CNTSum13", RP::calcIdCode(RP::ID_CNT,3,1), rp); }
  void setCNTrp14(const float rp) { setValue("CNTSum14", RP::calcIdCode(RP::ID_CNT,4,1), rp); }
 
  void setMPCrp00(const float rp) { setValue("MPCSum00", RP::calcIdCode(RP::ID_MPC,0,0), rp); }
  void setMPCrp01(const float rp) { setValue("MPCSum01", RP::calcIdCode(RP::ID_MPC,1,0), rp); }
  void setMPCrp02(const float rp) { setValue("MPCSum02", RP::calcIdCode(RP::ID_MPC,2,0), rp); }
  void setMPCrp10(const float rp) { setValue("MPCSum10", RP::calcIdCode(RP::ID_MPC,0,1), rp); }
  void setMPCrp11(const float rp) { setValue("MPCSum11", RP::calcIdCode(RP::ID_MPC,1,1), rp); }
  void setMPCrp12(const float rp) { setValue("MPCSum12", RP::calcIdCode(RP::ID_MPC,2,1), rp); }
 

  //////////
  virtual void AddReactionPlane(const char *name, const int id, const float rp);
  
  virtual bool isReactionPlane(const char *name) const;

  virtual ReactionPlaneSngl* getReactionPlane(const int idcode) const;
  virtual int          getnReactionPlane() const;


  void identifyv4(std::ostream &os = std::cout) const;

 private:
  // wrapper for set method
  void  setValue(const char *name, const int idcode, const float val); // type:0=Qx, 1=Qy, 2=W
  float getValue(const int idcode) const; // type:0=Qx, 1=Qy, 2=W


 private:
  TClonesArray* ReactionPlaneSnglArray;
  std::map<int, int> m_vAryIndex; // int:idcode, int:aryIndex


  ClassDef(ReactionPlaneObjectv4,1)
};

#endif
