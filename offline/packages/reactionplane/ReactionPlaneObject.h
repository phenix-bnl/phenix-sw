#ifndef __REACTIONPLANEOBJECT_H__
#define __REACTIONPLANEOBJECT_H__

#include <iostream>
#include "phool.h"
#include "PHObject.h"

class ReactionPlaneSngl;

class ReactionPlaneObject : public PHObject 
{

 public:
  ReactionPlaneObject() {}
  virtual ~ReactionPlaneObject() {}

  virtual ReactionPlaneObject* clone() const { warning("clone"); return 0; }

  using TObject::Copy;

  virtual void Copy(const ReactionPlaneObject &src);
  //virtual void Copy(ReactionPlaneObject &src);

  // Get
  virtual float getBBCsumW00() const { warning("BBCsumW00"); return -9999.;}
  virtual float getBBCsumW01() const { warning("BBCsumW01"); return -9999.;}
  virtual float getBBCsumW02() const { warning("BBCsumW02"); return -9999.;}
  virtual float getBBCsumW10() const { warning("BBCsumW10"); return -9999.;}
  virtual float getBBCsumW11() const { warning("BBCsumW11"); return -9999.;}
  virtual float getBBCsumW12() const { warning("BBCsumW12"); return -9999.;}

  virtual float getSMDsumW00() const { warning("SMDsumW00"); return -9999.;}
  virtual float getSMDsumW01() const { warning("SMDsumW01"); return -9999.;}
  virtual float getSMDsumW02() const { warning("SMDsumW02"); return -9999.;}

  virtual float getMVDsumW00() const { warning("MVDsumW00"); return -9999.;}
  virtual float getMVDsumW01() const { warning("MVDsumW01"); return -9999.;}
  virtual float getMVDsumW02() const { warning("MVDsumW02"); return -9999.;}
  virtual float getMVDsumW10() const { warning("MVDsumW10"); return -9999.;}
  virtual float getMVDsumW11() const { warning("MVDsumW11"); return -9999.;}
  virtual float getMVDsumW12() const { warning("MVDsumW12"); return -9999.;}

  virtual float getFCLsumW00() const { warning("FCLsumW00"); return -9999.;}
  virtual float getFCLsumW01() const { warning("FCLsumW01"); return -9999.;}
  virtual float getFCLsumW02() const { warning("FCLsumW02"); return -9999.;}

  virtual float getCNTsumW10() const { warning("CNTsumW10"); return -9999.;}
  virtual float getCNTsumW11() const { warning("CNTsumW11"); return -9999.;}
  virtual float getCNTsumW12() const { warning("CNTsumW12"); return -9999.;}
  virtual float getCNTsumW13() const { warning("CNTsumW13"); return -9999.;}
  virtual float getCNTsumW14() const { warning("CNTsumW14"); return -9999.;}

//#####################add sumW RXN & MPC 07/3/14###############################//

  virtual float getRXNsumW00() const { warning("RXNsumW00"); return -9999.;}
  virtual float getRXNsumW01() const { warning("RXNsumW01"); return -9999.;}
  virtual float getRXNsumW02() const { warning("RXNsumW02"); return -9999.;}
  virtual float getRXNsumW03() const { warning("RXNsumW03"); return -9999.;}
  virtual float getRXNsumW04() const { warning("RXNsumW04"); return -9999.;}
  virtual float getRXNsumW05() const { warning("RXNsumW05"); return -9999.;}
  virtual float getRXNsumW06() const { warning("RXNsumW06"); return -9999.;}
  virtual float getRXNsumW07() const { warning("RXNsumW07"); return -9999.;}
  virtual float getRXNsumW08() const { warning("RXNsumW08"); return -9999.;}

  virtual float getRXNsumW10() const { warning("RXNsumW10"); return -9999.;}
  virtual float getRXNsumW11() const { warning("RXNsumW11"); return -9999.;}
  virtual float getRXNsumW12() const { warning("RXNsumW12"); return -9999.;}
  virtual float getRXNsumW13() const { warning("RXNsumW13"); return -9999.;}
  virtual float getRXNsumW14() const { warning("RXNsumW14"); return -9999.;}
  virtual float getRXNsumW15() const { warning("RXNsumW15"); return -9999.;}
  virtual float getRXNsumW16() const { warning("RXNsumW16"); return -9999.;}
  virtual float getRXNsumW17() const { warning("RXNsumW17"); return -9999.;}
  virtual float getRXNsumW18() const { warning("RXNsumW18"); return -9999.;}

  virtual float getMPCsumW00() const { warning("MPCsumW00"); return -9999.;}
  virtual float getMPCsumW01() const { warning("MPCsumW01"); return -9999.;}
  virtual float getMPCsumW02() const { warning("MPCsumW02"); return -9999.;}
  virtual float getMPCsumW10() const { warning("MPCsumW10"); return -9999.;}
  virtual float getMPCsumW11() const { warning("MPCsumW11"); return -9999.;}
  virtual float getMPCsumW12() const { warning("MPCsumW12"); return -9999.;}

//#################################################################################//

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
/*
//####################add sumX,Y RXN & MPC 07/3/14########################//

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

//############################################################################//
*/
  virtual float getBBCrp00() const { warning("BBCrp00"); return -9999; }
  virtual float getBBCrp01() const { warning("BBCrp01"); return -9999; }
  virtual float getBBCrp02() const { warning("BBCrp02"); return -9999; }
  virtual float getBBCrp10() const { warning("BBCrp10"); return -9999; }
  virtual float getBBCrp11() const { warning("BBCrp11"); return -9999; }
  virtual float getBBCrp12() const { warning("BBCrp12"); return -9999; }

  virtual float getSMDrp00() const { warning("SMDrp00"); return -9999; }
  virtual float getSMDrp01() const { warning("SMDrp01"); return -9999; }
  virtual float getSMDrp02() const { warning("SMDrp02"); return -9999; }

  virtual float getMVDrp00() const { warning("MVDrp00"); return -9999; }
  virtual float getMVDrp01() const { warning("MVDrp01"); return -9999; }
  virtual float getMVDrp02() const { warning("MVDrp02"); return -9999; }
  virtual float getMVDrp10() const { warning("MVDrp10"); return -9999; }
  virtual float getMVDrp11() const { warning("MVDrp11"); return -9999; }
  virtual float getMVDrp12() const { warning("MVDrp12"); return -9999; }

  virtual float getFCLrp00() const { warning("FCLrp00"); return -9999; }
  virtual float getFCLrp01() const { warning("FCLrp01"); return -9999; }
  virtual float getFCLrp02() const { warning("FCLrp02"); return -9999; }

  virtual float getCNTrp10() const { warning("CNTrp10"); return -9999; }
  virtual float getCNTrp11() const { warning("CNTrp11"); return -9999; }
  virtual float getCNTrp12() const { warning("CNTrp12"); return -9999; }
  virtual float getCNTrp13() const { warning("CNTrp13"); return -9999; }
  virtual float getCNTrp14() const { warning("CNTrp14"); return -9999; }

//#####################add rp RXN & MPC 07/3/14#######################//

  virtual float getRXNrp00() const { warning("RXNrp00"); return -9999; }
  virtual float getRXNrp01() const { warning("RXNrp01"); return -9999; }
  virtual float getRXNrp02() const { warning("RXNrp02"); return -9999; }
  virtual float getRXNrp03() const { warning("RXNrp03"); return -9999; }
  virtual float getRXNrp04() const { warning("RXNrp04"); return -9999; }
  virtual float getRXNrp05() const { warning("RXNrp05"); return -9999; }
  virtual float getRXNrp06() const { warning("RXNrp06"); return -9999; }
  virtual float getRXNrp07() const { warning("RXNrp07"); return -9999; }
  virtual float getRXNrp08() const { warning("RXNrp08"); return -9999; }

  virtual float getRXNrp10() const { warning("RXNrp10"); return -9999; }
  virtual float getRXNrp11() const { warning("RXNrp11"); return -9999; }
  virtual float getRXNrp12() const { warning("RXNrp12"); return -9999; }
  virtual float getRXNrp13() const { warning("RXNrp13"); return -9999; }
  virtual float getRXNrp14() const { warning("RXNrp14"); return -9999; }
  virtual float getRXNrp15() const { warning("RXNrp15"); return -9999; }
  virtual float getRXNrp16() const { warning("RXNrp16"); return -9999; }
  virtual float getRXNrp17() const { warning("RXNrp17"); return -9999; }
  virtual float getRXNrp18() const { warning("RXNrp18"); return -9999; }

  virtual float getMPCrp00() const { warning("MPCrp00"); return -9999; }
  virtual float getMPCrp01() const { warning("MPCrp01"); return -9999; }
  virtual float getMPCrp02() const { warning("MPCrp02"); return -9999; }

  virtual float getMPCrp10() const { warning("MPCrp10"); return -9999; }
  virtual float getMPCrp11() const { warning("MPCrp11"); return -9999; }
  virtual float getMPCrp12() const { warning("MPCrp12"); return -9999; }

//######################################################################//

  // Set
  virtual void setBBCsumW00(const float) { warning("BBCsumW00"); }
  virtual void setBBCsumW01(const float) { warning("BBCsumW01"); }
  virtual void setBBCsumW02(const float) { warning("BBCsumW02"); }
  virtual void setBBCsumW10(const float) { warning("BBCsumW10"); }
  virtual void setBBCsumW11(const float) { warning("BBCsumW11"); }
  virtual void setBBCsumW12(const float) { warning("BBCsumW12"); }

  virtual void setSMDsumW00(const float) { warning("SMDsumW00"); }
  virtual void setSMDsumW01(const float) { warning("SMDsumW01"); }
  virtual void setSMDsumW02(const float) { warning("SMDsumW02"); }

  virtual void setMVDsumW00(const float) { warning("MVDsumW00"); }
  virtual void setMVDsumW01(const float) { warning("MVDsumW01"); }
  virtual void setMVDsumW02(const float) { warning("MVDsumW02"); }
  virtual void setMVDsumW10(const float) { warning("MVDsumW10"); }
  virtual void setMVDsumW11(const float) { warning("MVDsumW11"); }
  virtual void setMVDsumW12(const float) { warning("MVDsumW12"); }

  virtual void setFCLsumW00(const float) { warning("FCLsumW00"); }
  virtual void setFCLsumW01(const float) { warning("FCLsumW01"); }
  virtual void setFCLsumW02(const float) { warning("FCLsumW02"); }

  virtual void setCNTsumW10(const float) { warning("CNTsumW10"); }
  virtual void setCNTsumW11(const float) { warning("CNTsumW11"); }
  virtual void setCNTsumW12(const float) { warning("CNTsumW12"); }
  virtual void setCNTsumW13(const float) { warning("CNTsumW13"); }
  virtual void setCNTsumW14(const float) { warning("CNTsumW14"); }

//################add sumW RXN & MPC 07/3/14#######################//

  virtual void setRXNsumW00(const float) { warning("RXNsumW00"); }
  virtual void setRXNsumW01(const float) { warning("RXNsumW01"); }
  virtual void setRXNsumW02(const float) { warning("RXNsumW02"); }
  virtual void setRXNsumW03(const float) { warning("RXNsumW03"); }
  virtual void setRXNsumW04(const float) { warning("RXNsumW04"); }
  virtual void setRXNsumW05(const float) { warning("RXNsumW05"); }
  virtual void setRXNsumW06(const float) { warning("RXNsumW06"); }
  virtual void setRXNsumW07(const float) { warning("RXNsumW07"); }
  virtual void setRXNsumW08(const float) { warning("RXNsumW08"); }

  virtual void setRXNsumW10(const float) { warning("RXNsumW10"); }
  virtual void setRXNsumW11(const float) { warning("RXNsumW11"); }
  virtual void setRXNsumW12(const float) { warning("RXNsumW12"); }
  virtual void setRXNsumW13(const float) { warning("RXNsumW13"); }
  virtual void setRXNsumW14(const float) { warning("RXNsumW14"); }
  virtual void setRXNsumW15(const float) { warning("RXNsumW15"); }
  virtual void setRXNsumW16(const float) { warning("RXNsumW16"); }
  virtual void setRXNsumW17(const float) { warning("RXNsumW17"); }
  virtual void setRXNsumW18(const float) { warning("RXNsumW18"); }

  virtual void setMPCsumW00(const float) { warning("MPCsumW00"); }
  virtual void setMPCsumW01(const float) { warning("MPCsumW01"); }
  virtual void setMPCsumW02(const float) { warning("MPCsumW02"); }

  virtual void setMPCsumW10(const float) { warning("MPCsumW10"); }
  virtual void setMPCsumW11(const float) { warning("MPCsumW11"); }
  virtual void setMPCsumW12(const float) { warning("MPCsumW12"); }

//###################################################################//

  virtual void setBBCsumX00(const float) { warning("BBCsumX00"); }
  virtual void setBBCsumX01(const float) { warning("BBCsumX01"); }
  virtual void setBBCsumX02(const float) { warning("BBCsumX02"); }
  virtual void setBBCsumX10(const float) { warning("BBCsumX10"); }
  virtual void setBBCsumX11(const float) { warning("BBCsumX11"); }
  virtual void setBBCsumX12(const float) { warning("BBCsumX12"); }
  virtual void setBBCsumY00(const float) { warning("BBCsumY00"); }
  virtual void setBBCsumY01(const float) { warning("BBCsumY01"); }
  virtual void setBBCsumY02(const float) { warning("BBCsumY02"); }
  virtual void setBBCsumY10(const float) { warning("BBCsumY10"); }
  virtual void setBBCsumY11(const float) { warning("BBCsumY11"); }
  virtual void setBBCsumY12(const float) { warning("BBCsumY12"); }

  virtual void setSMDsumX00(const float) { warning("SMDsumX00"); }
  virtual void setSMDsumX01(const float) { warning("SMDsumX01"); }
  virtual void setSMDsumX02(const float) { warning("SMDsumX02"); }
  virtual void setSMDsumY00(const float) { warning("SMDsumY00"); }
  virtual void setSMDsumY01(const float) { warning("SMDsumY01"); }
  virtual void setSMDsumY02(const float) { warning("SMDsumY02"); }

  virtual void setMVDhits0(const int) { warning("MVDhits0"); }
  virtual void setMVDhits1(const int) { warning("MVDhits1"); }
  virtual void setMVDhits2(const int) { warning("MVDhits2"); }
  virtual void setMVDsumX00(const float) { warning("MVDsumX00"); }
  virtual void setMVDsumX01(const float) { warning("MVDsumX01"); }
  virtual void setMVDsumX02(const float) { warning("MVDsumX02"); }
  virtual void setMVDsumX10(const float) { warning("MVDsumX10"); }
  virtual void setMVDsumX11(const float) { warning("MVDsumX11"); }
  virtual void setMVDsumX12(const float) { warning("MVDsumX12"); }
  virtual void setMVDsumY00(const float) { warning("MVDsumY00"); }
  virtual void setMVDsumY01(const float) { warning("MVDsumY01"); }
  virtual void setMVDsumY02(const float) { warning("MVDsumY02"); }
  virtual void setMVDsumY10(const float) { warning("MVDsumY10"); }
  virtual void setMVDsumY11(const float) { warning("MVDsumY11"); }
  virtual void setMVDsumY12(const float) { warning("MVDsumY12"); }

  virtual void setFCLsumX00(const float) { warning("FCLsumX00"); }
  virtual void setFCLsumX01(const float) { warning("FCLsumX01"); }
  virtual void setFCLsumX02(const float) { warning("FCLsumX02"); }
  virtual void setFCLsumY00(const float) { warning("FCLsumY00"); }
  virtual void setFCLsumY01(const float) { warning("FCLsumY01"); }
  virtual void setFCLsumY02(const float) { warning("FCLsumY02"); }

  virtual void setCNTsumX10(const float) { warning("CNTsumX10"); }
  virtual void setCNTsumX11(const float) { warning("CNTsumX11"); }
  virtual void setCNTsumX12(const float) { warning("CNTsumX12"); }
  virtual void setCNTsumX13(const float) { warning("CNTsumX13"); }
  virtual void setCNTsumX14(const float) { warning("CNTsumX14"); }
  virtual void setCNTsumY10(const float) { warning("CNTsumY10"); }
  virtual void setCNTsumY11(const float) { warning("CNTsumY11"); }
  virtual void setCNTsumY12(const float) { warning("CNTsumY12"); }
  virtual void setCNTsumY13(const float) { warning("CNTsumY13"); }
  virtual void setCNTsumY14(const float) { warning("CNTsumY14"); }
/*
//################add sumX,Y RXN & MPC 07/3/14##########################//

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
  
//#####################################################################//
*/
  virtual void setBBCrp00(const float) { warning("BBCrp00"); }
  virtual void setBBCrp01(const float) { warning("BBCrp01"); }
  virtual void setBBCrp02(const float) { warning("BBCrp02"); }
  virtual void setBBCrp10(const float) { warning("BBCrp10"); }
  virtual void setBBCrp11(const float) { warning("BBCrp11"); }
  virtual void setBBCrp12(const float) { warning("BBCrp12"); }

  virtual void setSMDrp00(const float) { warning("SMDrp00"); }
  virtual void setSMDrp01(const float) { warning("SMDrp01"); }
  virtual void setSMDrp02(const float) { warning("SMDrp02"); }

  virtual void setMVDrp00(const float) { warning("MVDrp00"); }
  virtual void setMVDrp01(const float) { warning("MVDrp01"); }
  virtual void setMVDrp02(const float) { warning("MVDrp02"); }
  virtual void setMVDrp10(const float) { warning("MVDrp10"); }
  virtual void setMVDrp11(const float) { warning("MVDrp11"); }
  virtual void setMVDrp12(const float) { warning("MVDrp12"); }

  virtual void setFCLrp00(const float) { warning("FCLrp00"); }
  virtual void setFCLrp01(const float) { warning("FCLrp01"); }
  virtual void setFCLrp02(const float) { warning("FCLrp02"); }

  virtual void setCNTrp10(const float) { warning("CNTrp10"); }
  virtual void setCNTrp11(const float) { warning("CNTrp11"); }
  virtual void setCNTrp12(const float) { warning("CNTrp12"); }
  virtual void setCNTrp13(const float) { warning("CNTrp13"); }
  virtual void setCNTrp14(const float) { warning("CNTrp14"); }

//#####################add rp RXN & MPC 07/3/14#######################//

  virtual void setRXNrp00(const float) { warning("RXNrp00"); }
  virtual void setRXNrp01(const float) { warning("RXNrp01"); }
  virtual void setRXNrp02(const float) { warning("RXNrp02"); }
  virtual void setRXNrp03(const float) { warning("RXNrp03"); }
  virtual void setRXNrp04(const float) { warning("RXNrp04"); }
  virtual void setRXNrp05(const float) { warning("RXNrp05"); }
  virtual void setRXNrp06(const float) { warning("RXNrp06"); }
  virtual void setRXNrp07(const float) { warning("RXNrp07"); }
  virtual void setRXNrp08(const float) { warning("RXNrp08"); }

  virtual void setRXNrp10(const float) { warning("RXNrp10"); }
  virtual void setRXNrp11(const float) { warning("RXNrp11"); }
  virtual void setRXNrp12(const float) { warning("RXNrp12"); }
  virtual void setRXNrp13(const float) { warning("RXNrp13"); }
  virtual void setRXNrp14(const float) { warning("RXNrp14"); }
  virtual void setRXNrp15(const float) { warning("RXNrp15"); }
  virtual void setRXNrp16(const float) { warning("RXNrp16"); }
  virtual void setRXNrp17(const float) { warning("RXNrp17"); }
  virtual void setRXNrp18(const float) { warning("RXNrp18"); }

  virtual void setMPCrp00(const float) { warning("MPCrp00"); }
  virtual void setMPCrp01(const float) { warning("MPCrp01"); }
  virtual void setMPCrp02(const float) { warning("MPCrp02"); }

  virtual void setMPCrp10(const float) { warning("MPCrp10"); }
  virtual void setMPCrp11(const float) { warning("MPCrp11"); }
  virtual void setMPCrp12(const float) { warning("MPCrp12"); }
  
  // function for v4
  virtual void AddReactionPlane(const char *name, const int id, const float rp)
  { warning("AddReactionPlane");}
  virtual bool isReactionPlane(const char *name) const{ warning("isReactionPlane"); return false;}
  
  virtual ReactionPlaneSngl* getReactionPlane(const int idcode) const { warning("getReactionPlane"); return NULL;}
  virtual int          getnReactionPlane() const { warning("getnReactionPlane"); return 0;}
  
  //#####################################################################//
  
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
    os << "identify yourself: virtual ReactionPlaneObject" << std::endl;
    return;
  }
  
  void ShutUp(const int i); // kill virtual warnings

 private:
  void warning(const char* field) const;

  ClassDef(ReactionPlaneObject,1)
};

#endif
