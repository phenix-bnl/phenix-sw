
#ifndef __RPSUMXYOBJECT_H__
#define __RPSUMXYOBJECT_H__

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class RpSnglSumXY;

class RpSumXYObject : public PHObject 
{

 public:
  RpSumXYObject() {}
  virtual ~RpSumXYObject() {}

  virtual RpSumXYObject* clone() const { warning("clone"); return 0; }

  using TObject::Copy;
  virtual void Copy(const RpSumXYObject &src);

  // Get
  virtual float getBBCsumW0() const { warning("BBCsumW0"); return -9999.;}
  virtual float getBBCsumW1() const { warning("BBCsumW1"); return -9999.;}
  virtual float getBBCsumW2() const { warning("BBCsumW2"); return -9999.;}

  virtual float getSMDsumW0() const { warning("SMDsumW0"); return -9999.;}
  virtual float getSMDsumW1() const { warning("SMDsumW1"); return -9999.;}
  virtual float getSMDsumW2() const { warning("SMDsumW2"); return -9999.;}

  virtual float getMVDsumW0() const { warning("MVDsumW0"); return -9999.;}
  virtual float getMVDsumW1() const { warning("MVDsumW1"); return -9999.;}
  virtual float getMVDsumW2() const { warning("MVDsumW2"); return -9999.;}

  virtual float getFCLsumW0() const { warning("FCLsumW0"); return -9999.;}
  virtual float getFCLsumW1() const { warning("FCLsumW1"); return -9999.;}
  virtual float getFCLsumW2() const { warning("FCLsumW2"); return -9999.;}

  virtual float getCNTsumW0() const { warning("CNTsumW0"); return -9999.;}
  virtual float getCNTsumW1() const { warning("CNTsumW1"); return -9999.;}
  virtual float getCNTsumW2() const { warning("CNTsumW2"); return -9999.;}
  virtual float getCNTsumW3() const { warning("CNTsumW3"); return -9999.;}
  virtual float getCNTsumW4() const { warning("CNTsumW4"); return -9999.;}

//#######################add sumW RXN & MPC 07/3/12########################//

  virtual float getRXNsumW0() const { warning("RXNsumW0"); return -9999.;}
  virtual float getRXNsumW1() const { warning("RXNsumW1"); return -9999.;}
  virtual float getRXNsumW2() const { warning("RXNsumW2"); return -9999.;}
  virtual float getRXNsumW3() const { warning("RXNsumW3"); return -9999.;}
  virtual float getRXNsumW4() const { warning("RXNsumW4"); return -9999.;}
  virtual float getRXNsumW5() const { warning("RXNsumW5"); return -9999.;}
  virtual float getRXNsumW6() const { warning("RXNsumW6"); return -9999.;}
  virtual float getRXNsumW7() const { warning("RXNsumW7"); return -9999.;}
  virtual float getRXNsumW8() const { warning("RXNsumW8"); return -9999.;}

  virtual float getMPCsumW0() const { warning("MPCsumW0"); return -9999.;}
  virtual float getMPCsumW1() const { warning("MPCsumW1"); return -9999.;}
  virtual float getMPCsumW2() const { warning("MPCsumW2"); return -9999.;}
 
//#########################################################################//

  virtual float getBBCsumX00() const { warning("BBCsumX00"); return -9999.;}
  virtual float getBBCsumX01() const { warning("BBCsumX01"); return -9999.;}
  virtual float getBBCsumX02() const { warning("BBCsumX02"); return -9999.;}
  virtual float getBBCsumX10() const { warning("BBCsumX10"); return -9999.;}
  virtual float getBBCsumX11() const { warning("BBCsumX11"); return -9999.;}
  virtual float getBBCsumX12() const { warning("BBCsumX12"); return -9999.;}
  virtual float getBBCsumY00() const { warning("BBCsumY00"); return -9999.;}
  virtual float getBBCsumY01() const { warning("BBCsumY01"); return -9999.;}
  virtual float getBBCsumY02() const { warning("BBCsumY02"); return -9999.;}
  virtual float getBBCsumY10() const { warning("BBCsumY10"); return -9999.;}
  virtual float getBBCsumY11() const { warning("BBCsumY11"); return -9999.;}
  virtual float getBBCsumY12() const { warning("BBCsumY12"); return -9999.;}

  virtual float getSMDsumX00() const { warning("SMDsumX00"); return -9999.;}
  virtual float getSMDsumX01() const { warning("SMDsumX01"); return -9999.;}
  virtual float getSMDsumX02() const { warning("SMDsumX02"); return -9999.;}
  virtual float getSMDsumY00() const { warning("SMDsumY00"); return -9999.;}
  virtual float getSMDsumY01() const { warning("SMDsumY01"); return -9999.;}
  virtual float getSMDsumY02() const { warning("SMDsumY02"); return -9999.;}

  virtual int getMVDhits0() const { warning("MVDhits0"); return -9999; }
  virtual int getMVDhits1() const { warning("MVDhits1"); return -9999; }
  virtual int getMVDhits2() const { warning("MVDhits2"); return -9999; }
  virtual float getMVDsumX00() const { warning("MVDsumX00"); return -9999.;}
  virtual float getMVDsumX01() const { warning("MVDsumX01"); return -9999.;}
  virtual float getMVDsumX02() const { warning("MVDsumX02"); return -9999.;}
  virtual float getMVDsumX10() const { warning("MVDsumX10"); return -9999.;}
  virtual float getMVDsumX11() const { warning("MVDsumX11"); return -9999.;}
  virtual float getMVDsumX12() const { warning("MVDsumX12"); return -9999.;}
  virtual float getMVDsumY00() const { warning("MVDsumY00"); return -9999.;}
  virtual float getMVDsumY01() const { warning("MVDsumY01"); return -9999.;}
  virtual float getMVDsumY02() const { warning("MVDsumY02"); return -9999.;}
  virtual float getMVDsumY10() const { warning("MVDsumY10"); return -9999.;}
  virtual float getMVDsumY11() const { warning("MVDsumY11"); return -9999.;}
  virtual float getMVDsumY12() const { warning("MVDsumY12"); return -9999.;}

  virtual float getFCLsumX00() const { warning("FCLsumX00"); return -9999.;}
  virtual float getFCLsumX01() const { warning("FCLsumX01"); return -9999.;}
  virtual float getFCLsumX02() const { warning("FCLsumX02"); return -9999.;}
  virtual float getFCLsumY00() const { warning("FCLsumY00"); return -9999.;}
  virtual float getFCLsumY01() const { warning("FCLsumY01"); return -9999.;}
  virtual float getFCLsumY02() const { warning("FCLsumY02"); return -9999.;}

  virtual float getCNTsumX10() const { warning("CNTsumX10"); return -9999.;}
  virtual float getCNTsumX11() const { warning("CNTsumX11"); return -9999.;}
  virtual float getCNTsumX12() const { warning("CNTsumX12"); return -9999.;}
  virtual float getCNTsumX13() const { warning("CNTsumX13"); return -9999.;}
  virtual float getCNTsumX14() const { warning("CNTsumX14"); return -9999.;}
  virtual float getCNTsumY10() const { warning("CNTsumY10"); return -9999.;}
  virtual float getCNTsumY11() const { warning("CNTsumY11"); return -9999.;}
  virtual float getCNTsumY12() const { warning("CNTsumY12"); return -9999.;}
  virtual float getCNTsumY13() const { warning("CNTsumY13"); return -9999.;}
  virtual float getCNTsumY14() const { warning("CNTsumY14"); return -9999.;}

//##################add sumX,Y RXN & MPC 07/3/12#################################//

  virtual float getRXNsumX00() const { warning("RXNsumX00"); return -9999.;}
  virtual float getRXNsumX01() const { warning("RXNsumX01"); return -9999.;}
  virtual float getRXNsumX02() const { warning("RXNsumX02"); return -9999.;}
  virtual float getRXNsumX03() const { warning("RXNsumX03"); return -9999.;}
  virtual float getRXNsumX04() const { warning("RXNsumX04"); return -9999.;}
  virtual float getRXNsumX05() const { warning("RXNsumX05"); return -9999.;}
  virtual float getRXNsumX06() const { warning("RXNsumX06"); return -9999.;}
  virtual float getRXNsumX07() const { warning("RXNsumX07"); return -9999.;}
  virtual float getRXNsumX08() const { warning("RXNsumX08"); return -9999.;}
  virtual float getRXNsumY00() const { warning("RXNsumY00"); return -9999.;}
  virtual float getRXNsumY01() const { warning("RXNsumY01"); return -9999.;}
  virtual float getRXNsumY02() const { warning("RXNsumY02"); return -9999.;}
  virtual float getRXNsumY03() const { warning("RXNsumY03"); return -9999.;}
  virtual float getRXNsumY04() const { warning("RXNsumY04"); return -9999.;}
  virtual float getRXNsumY05() const { warning("RXNsumY05"); return -9999.;}
  virtual float getRXNsumY06() const { warning("RXNsumY06"); return -9999.;}
  virtual float getRXNsumY07() const { warning("RXNsumY07"); return -9999.;}
  virtual float getRXNsumY08() const { warning("RXNsumY08"); return -9999.;}

  virtual float getRXNsumX10() const { warning("RXNsumX10"); return -9999.;}
  virtual float getRXNsumX11() const { warning("RXNsumX11"); return -9999.;}
  virtual float getRXNsumX12() const { warning("RXNsumX12"); return -9999.;}
  virtual float getRXNsumX13() const { warning("RXNsumX13"); return -9999.;}
  virtual float getRXNsumX14() const { warning("RXNsumX14"); return -9999.;}
  virtual float getRXNsumX15() const { warning("RXNsumX15"); return -9999.;}
  virtual float getRXNsumX16() const { warning("RXNsumX16"); return -9999.;}
  virtual float getRXNsumX17() const { warning("RXNsumX17"); return -9999.;}
  virtual float getRXNsumX18() const { warning("RXNsumX18"); return -9999.;}
  virtual float getRXNsumY10() const { warning("RXNsumY10"); return -9999.;}
  virtual float getRXNsumY11() const { warning("RXNsumY11"); return -9999.;}
  virtual float getRXNsumY12() const { warning("RXNsumY12"); return -9999.;}
  virtual float getRXNsumY13() const { warning("RXNsumY13"); return -9999.;}
  virtual float getRXNsumY14() const { warning("RXNsumY14"); return -9999.;}
  virtual float getRXNsumY15() const { warning("RXNsumY15"); return -9999.;}
  virtual float getRXNsumY16() const { warning("RXNsumY16"); return -9999.;}
  virtual float getRXNsumY17() const { warning("RXNsumY17"); return -9999.;}
  virtual float getRXNsumY18() const { warning("RXNsumY18"); return -9999.;}

  virtual float getMPCsumX00() const { warning("MPCsumX00"); return -9999.;}
  virtual float getMPCsumX01() const { warning("MPCsumX01"); return -9999.;}
  virtual float getMPCsumX02() const { warning("MPCsumX02"); return -9999.;}
  virtual float getMPCsumY00() const { warning("MPCsumY00"); return -9999.;}
  virtual float getMPCsumY01() const { warning("MPCsumY01"); return -9999.;}
  virtual float getMPCsumY02() const { warning("MPCsumY02"); return -9999.;}   

  virtual float getMPCsumX10() const { warning("MPCsumX10"); return -9999.;}
  virtual float getMPCsumX11() const { warning("MPCsumX11"); return -9999.;}
  virtual float getMPCsumX12() const { warning("MPCsumX12"); return -9999.;}
  virtual float getMPCsumY10() const { warning("MPCsumY10"); return -9999.;}
  virtual float getMPCsumY11() const { warning("MPCsumY11"); return -9999.;}
  virtual float getMPCsumY12() const { warning("MPCsumY12"); return -9999.;}

//##############################################################################//

  // Set
  virtual void setBBCsumW0(const float w) { warning("BBCsumW0"); }
  virtual void setBBCsumW1(const float w) { warning("BBCsumW1"); }
  virtual void setBBCsumW2(const float w) { warning("BBCsumW2"); }

  virtual void setSMDsumW0(const float w) { warning("SMDsumW0"); }
  virtual void setSMDsumW1(const float w) { warning("SMDsumW1"); }
  virtual void setSMDsumW2(const float w) { warning("SMDsumW2"); }

  virtual void setMVDsumW0(const float w) { warning("MVDsumW0"); }
  virtual void setMVDsumW1(const float w) { warning("MVDsumW1"); }
  virtual void setMVDsumW2(const float w) { warning("MVDsumW2"); }

  virtual void setFCLsumW0(const float w) { warning("FCLsumW0"); }
  virtual void setFCLsumW1(const float w) { warning("FCLsumW1"); }
  virtual void setFCLsumW2(const float w) { warning("FCLsumW2"); }

  virtual void setCNTsumW0(const float w) { warning("CNTsumW0"); }
  virtual void setCNTsumW1(const float w) { warning("CNTsumW1"); }
  virtual void setCNTsumW2(const float w) { warning("CNTsumW2"); }
  virtual void setCNTsumW3(const float w) { warning("CNTsumW3"); }
  virtual void setCNTsumW4(const float w) { warning("CNTsumW4"); }

//########################add sumW RXN & MPC 07/3/12##########################//

  virtual void setRXNsumW0(const float w) { warning("RXNsumW0"); }
  virtual void setRXNsumW1(const float w) { warning("RXNsumW1"); }
  virtual void setRXNsumW2(const float w) { warning("RXNsumW2"); }
  virtual void setRXNsumW3(const float w) { warning("RXNsumW3"); }
  virtual void setRXNsumW4(const float w) { warning("RXNsumW4"); }
  virtual void setRXNsumW5(const float w) { warning("RXNsumW5"); }
  virtual void setRXNsumW6(const float w) { warning("RXNsumW6"); }
  virtual void setRXNsumW7(const float w) { warning("RXNsumW7"); }
  virtual void setRXNsumW8(const float w) { warning("RXNsumW8"); }

  virtual void setMPCsumW0(const float w) { warning("MPCsumW0"); }
  virtual void setMPCsumW1(const float w) { warning("MPCsumW1"); }
  virtual void setMPCsumW2(const float w) { warning("MPCsumW2"); }

//##########################################################################//

  virtual void setBBCsumX00(const float sumx) { warning("BBCsumX00"); }
  virtual void setBBCsumX01(const float sumx) { warning("BBCsumX01"); }
  virtual void setBBCsumX02(const float sumx) { warning("BBCsumX02"); }
  virtual void setBBCsumX10(const float sumx) { warning("BBCsumX10"); }
  virtual void setBBCsumX11(const float sumx) { warning("BBCsumX11"); }
  virtual void setBBCsumX12(const float sumx) { warning("BBCsumX12"); }
  virtual void setBBCsumY00(const float sumy) { warning("BBCsumY00"); }
  virtual void setBBCsumY01(const float sumy) { warning("BBCsumY01"); }
  virtual void setBBCsumY02(const float sumy) { warning("BBCsumY02"); }
  virtual void setBBCsumY10(const float sumy) { warning("BBCsumY10"); }
  virtual void setBBCsumY11(const float sumy) { warning("BBCsumY11"); }
  virtual void setBBCsumY12(const float sumy) { warning("BBCsumY12"); }

  virtual void setSMDsumX00(const float sumx) { warning("SMDsumX00"); }
  virtual void setSMDsumX01(const float sumx) { warning("SMDsumX01"); }
  virtual void setSMDsumX02(const float sumx) { warning("SMDsumX02"); }
  virtual void setSMDsumY00(const float sumy) { warning("SMDsumY00"); }
  virtual void setSMDsumY01(const float sumy) { warning("SMDsumY01"); }
  virtual void setSMDsumY02(const float sumy) { warning("SMDsumY02"); }

  virtual void setMVDhits0(const int hits){ warning("MVDhits0"); }
  virtual void setMVDhits1(const int hits){ warning("MVDhits1"); }
  virtual void setMVDhits2(const int hits){ warning("MVDhits2"); }
  virtual void setMVDsumX00(const float sumx) { warning("MVDsumX00"); }
  virtual void setMVDsumX01(const float sumx) { warning("MVDsumX01"); }
  virtual void setMVDsumX02(const float sumx) { warning("MVDsumX02"); }
  virtual void setMVDsumX10(const float sumx) { warning("MVDsumX10"); }
  virtual void setMVDsumX11(const float sumx) { warning("MVDsumX11"); }
  virtual void setMVDsumX12(const float sumx) { warning("MVDsumX12"); }
  virtual void setMVDsumY00(const float sumy) { warning("MVDsumY00"); }
  virtual void setMVDsumY01(const float sumy) { warning("MVDsumY01"); }
  virtual void setMVDsumY02(const float sumy) { warning("MVDsumY02"); }
  virtual void setMVDsumY10(const float sumy) { warning("MVDsumY10"); }
  virtual void setMVDsumY11(const float sumy) { warning("MVDsumY11"); }
  virtual void setMVDsumY12(const float sumy) { warning("MVDsumY12"); }

  virtual void setFCLsumX00(const float sumx) { warning("FCLsumX00"); }
  virtual void setFCLsumX01(const float sumx) { warning("FCLsumX01"); }
  virtual void setFCLsumX02(const float sumx) { warning("FCLsumX02"); }
  virtual void setFCLsumY00(const float sumy) { warning("FCLsumY00"); }
  virtual void setFCLsumY01(const float sumy) { warning("FCLsumY01"); }
  virtual void setFCLsumY02(const float sumy) { warning("FCLsumY02"); }

  virtual void setCNTsumX10(const float sumx) { warning("CNTsumX10"); }
  virtual void setCNTsumX11(const float sumx) { warning("CNTsumX11"); }
  virtual void setCNTsumX12(const float sumx) { warning("CNTsumX12"); }
  virtual void setCNTsumX13(const float sumx) { warning("CNTsumX13"); }
  virtual void setCNTsumX14(const float sumx) { warning("CNTsumX14"); }
  virtual void setCNTsumY10(const float sumy) { warning("CNTsumY10"); }
  virtual void setCNTsumY11(const float sumy) { warning("CNTsumY11"); }
  virtual void setCNTsumY12(const float sumy) { warning("CNTsumY12"); }
  virtual void setCNTsumY13(const float sumy) { warning("CNTsumY13"); }
  virtual void setCNTsumY14(const float sumy) { warning("CNTsumY14"); }

//######################add sumX,Y RXN & MPC 07/3/12###########################//

  virtual void setRXNsumX00(const float sumx) { warning("RXNsumX00"); }
  virtual void setRXNsumX01(const float sumx) { warning("RXNsumX01"); }
  virtual void setRXNsumX02(const float sumx) { warning("RXNsumX02"); }
  virtual void setRXNsumX03(const float sumx) { warning("RXNsumX03"); }
  virtual void setRXNsumX04(const float sumx) { warning("RXNsumX04"); }
  virtual void setRXNsumX05(const float sumx) { warning("RXNsumX05"); }
  virtual void setRXNsumX06(const float sumx) { warning("RXNsumX06"); }
  virtual void setRXNsumX07(const float sumx) { warning("RXNsumX07"); }
  virtual void setRXNsumX08(const float sumx) { warning("RXNsumX08"); }
  virtual void setRXNsumY00(const float sumy) { warning("RXNsumY00"); }
  virtual void setRXNsumY01(const float sumy) { warning("RXNsumY01"); }
  virtual void setRXNsumY02(const float sumy) { warning("RXNsumY02"); }
  virtual void setRXNsumY03(const float sumy) { warning("RXNsumY03"); }
  virtual void setRXNsumY04(const float sumy) { warning("RXNsumY04"); }
  virtual void setRXNsumY05(const float sumy) { warning("RXNsumY05"); }
  virtual void setRXNsumY06(const float sumy) { warning("RXNsumY06"); }
  virtual void setRXNsumY07(const float sumy) { warning("RXNsumY07"); }
  virtual void setRXNsumY08(const float sumy) { warning("RXNsumY08"); }

  virtual void setRXNsumX10(const float sumx) { warning("RXNsumX10"); }
  virtual void setRXNsumX11(const float sumx) { warning("RXNsumX11"); }
  virtual void setRXNsumX12(const float sumx) { warning("RXNsumX12"); }
  virtual void setRXNsumX13(const float sumx) { warning("RXNsumX13"); }
  virtual void setRXNsumX14(const float sumx) { warning("RXNsumX14"); }
  virtual void setRXNsumX15(const float sumx) { warning("RXNsumX15"); }
  virtual void setRXNsumX16(const float sumx) { warning("RXNsumX16"); }
  virtual void setRXNsumX17(const float sumx) { warning("RXNsumX17"); }
  virtual void setRXNsumX18(const float sumx) { warning("RXNsumX18"); }
  virtual void setRXNsumY10(const float sumy) { warning("RXNsumY10"); }
  virtual void setRXNsumY11(const float sumy) { warning("RXNsumY11"); }
  virtual void setRXNsumY12(const float sumy) { warning("RXNsumY12"); }
  virtual void setRXNsumY13(const float sumy) { warning("RXNsumY13"); }
  virtual void setRXNsumY14(const float sumy) { warning("RXNsumY14"); }
  virtual void setRXNsumY15(const float sumy) { warning("RXNsumY15"); }
  virtual void setRXNsumY16(const float sumy) { warning("RXNsumY16"); }
  virtual void setRXNsumY17(const float sumy) { warning("RXNsumY17"); }
  virtual void setRXNsumY18(const float sumy) { warning("RXNsumY18"); }

  virtual void setMPCsumX00(const float sumx) { warning("MPCsumX00"); }
  virtual void setMPCsumX01(const float sumx) { warning("MPCsumX01"); }
  virtual void setMPCsumX02(const float sumx) { warning("MPCsumX02"); }
  virtual void setMPCsumY00(const float sumy) { warning("MPCsumY00"); }
  virtual void setMPCsumY01(const float sumy) { warning("MPCsumY01"); }
  virtual void setMPCsumY02(const float sumy) { warning("MPCsumY02"); }

  virtual void setMPCsumX10(const float sumx) { warning("MPCsumX10"); }
  virtual void setMPCsumX11(const float sumx) { warning("MPCsumX11"); }
  virtual void setMPCsumX12(const float sumx) { warning("MPCsumX12"); }
  virtual void setMPCsumY10(const float sumy) { warning("MPCsumY10"); }
  virtual void setMPCsumY11(const float sumy) { warning("MPCsumY11"); }
  virtual void setMPCsumY12(const float sumy) { warning("MPCsumY12"); }

// function for v3
  virtual void AddRpSumXY(const char *name, const int id, const float qx, const float qy, const float w)
               { warning("AddRpSumXY");}
  virtual bool isRpSumXY(const char *name) const{ warning("isRpSumXY"); return false;}

  virtual RpSnglSumXY* getRpSumXY(const int idcode) const { warning("getRpSumXY"); return NULL;}
  virtual int          getnRpSumXY() const { warning("getnRpSumXY"); return 0;}


//########################################################################//

  // Standard functions of all virtual classes...
  virtual void Reset() {
    std::cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << std::endl;
    return;
  }

  virtual int isValid() const {
    std::cout << PHWHERE << "isValid() not implemented by daughter function" << std::endl;
    return 0;
  }

  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual RpSumXYObject" << std::endl;
    return;
  }

  void ShutUp(const int i); // kill virtual warnings

 private:
  void warning(const char* field) const;

  ClassDef(RpSumXYObject,1)
};

#endif
