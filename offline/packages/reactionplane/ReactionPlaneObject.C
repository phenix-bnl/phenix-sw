
#include "ReactionPlaneObject.h"

ClassImp(ReactionPlaneObject)

using namespace std;

static int shutup = 0;

void ReactionPlaneObject::ShutUp(const int i)
{
  shutup = i;
}


void ReactionPlaneObject::warning(const char* field) const
{
  if (!shutup) {
    cout << "ReactionPlaneObject::using virtual function, doing nothing" << endl;
    cout << "Offending field == " << field << endl;
  }
}

void ReactionPlaneObject::Copy(const ReactionPlaneObject &src)
{
  ShutUp(1);
  setBBCsumW00( src.getBBCsumW00() );
  setBBCsumW01( src.getBBCsumW01() );
  setBBCsumW02( src.getBBCsumW02() );
  setBBCsumW10( src.getBBCsumW10() );
  setBBCsumW11( src.getBBCsumW11() );
  setBBCsumW12( src.getBBCsumW12() );
  setSMDsumW00( src.getSMDsumW00() );
  setSMDsumW01( src.getSMDsumW01() );
  setSMDsumW02( src.getSMDsumW02() );
  setMVDsumW00( src.getMVDsumW00() );
  setMVDsumW01( src.getMVDsumW01() );
  setMVDsumW02( src.getMVDsumW02() );
  setMVDsumW10( src.getMVDsumW10() );
  setMVDsumW11( src.getMVDsumW11() );
  setMVDsumW12( src.getMVDsumW12() );
  setFCLsumW00( src.getFCLsumW00() );
  setFCLsumW01( src.getFCLsumW01() );
  setFCLsumW02( src.getFCLsumW02() );
  setCNTsumW10( src.getCNTsumW10() );
  setCNTsumW11( src.getCNTsumW11() );
  setCNTsumW12( src.getCNTsumW12() );
  setCNTsumW13( src.getCNTsumW13() );
  setCNTsumW14( src.getCNTsumW14() );

//##############add sumW RXN & MPC 07/3/14##############//

  setRXNsumW00( src.getRXNsumW00() );
  setRXNsumW01( src.getRXNsumW01() );
  setRXNsumW02( src.getRXNsumW02() );
  setRXNsumW03( src.getRXNsumW03() );
  setRXNsumW04( src.getRXNsumW04() );
  setRXNsumW05( src.getRXNsumW05() );
  setRXNsumW06( src.getRXNsumW06() );
  setRXNsumW07( src.getRXNsumW07() );
  setRXNsumW08( src.getRXNsumW08() );
  setRXNsumW10( src.getRXNsumW10() );
  setRXNsumW11( src.getRXNsumW11() );
  setRXNsumW12( src.getRXNsumW12() );
  setRXNsumW13( src.getRXNsumW13() );
  setRXNsumW14( src.getRXNsumW14() );
  setRXNsumW15( src.getRXNsumW15() );
  setRXNsumW16( src.getRXNsumW16() );
  setRXNsumW17( src.getRXNsumW17() );
  setRXNsumW18( src.getRXNsumW18() );

  setMPCsumW00( src.getRXNsumW00() );
  setMPCsumW01( src.getRXNsumW01() ); 
  setMPCsumW02( src.getRXNsumW02() );
  setMPCsumW10( src.getRXNsumW10() );
  setMPCsumW11( src.getRXNsumW11() );
  setMPCsumW12( src.getRXNsumW12() );

//###################################################//

  setBBCsumX00( src.getBBCsumX00() );
  setBBCsumX01( src.getBBCsumX01() );
  setBBCsumX02( src.getBBCsumX02() );
  setBBCsumX10( src.getBBCsumX10() );
  setBBCsumX11( src.getBBCsumX11() );
  setBBCsumX12( src.getBBCsumX12() );
  setBBCsumY00( src.getBBCsumY00() );
  setBBCsumY01( src.getBBCsumY01() );
  setBBCsumY02( src.getBBCsumY02() );
  setBBCsumY10( src.getBBCsumY10() );
  setBBCsumY11( src.getBBCsumY11() );
  setBBCsumY12( src.getBBCsumY12() );
  setSMDsumX00( src.getSMDsumX00() );
  setSMDsumX01( src.getSMDsumX01() );
  setSMDsumX02( src.getSMDsumX02() );
  setSMDsumY00( src.getSMDsumY00() );
  setSMDsumY01( src.getSMDsumY01() );
  setSMDsumY02( src.getSMDsumY02() );
  setMVDhits0 ( src.getMVDhits0 () );
  setMVDhits1 ( src.getMVDhits1 () );
  setMVDhits2 ( src.getMVDhits2 () );
  setMVDsumX00( src.getMVDsumX00() );
  setMVDsumX01( src.getMVDsumX01() );
  setMVDsumX02( src.getMVDsumX02() );
  setMVDsumX10( src.getMVDsumX10() );
  setMVDsumX11( src.getMVDsumX11() );
  setMVDsumX12( src.getMVDsumX12() );
  setMVDsumY00( src.getMVDsumY00() );
  setMVDsumY01( src.getMVDsumY01() );
  setMVDsumY02( src.getMVDsumY02() );
  setMVDsumY10( src.getMVDsumY10() );
  setMVDsumY11( src.getMVDsumY11() );
  setMVDsumY12( src.getMVDsumY12() );
  setFCLsumX00( src.getFCLsumX00() );
  setFCLsumX01( src.getFCLsumX01() );
  setFCLsumX02( src.getFCLsumX02() );
  setFCLsumY00( src.getFCLsumY00() );
  setFCLsumY01( src.getFCLsumY01() );
  setFCLsumY02( src.getFCLsumY02() );
  setCNTsumX10( src.getCNTsumX10() );
  setCNTsumX11( src.getCNTsumX11() );
  setCNTsumX12( src.getCNTsumX12() );
  setCNTsumX13( src.getCNTsumX13() );
  setCNTsumX14( src.getCNTsumX14() );
  setCNTsumY10( src.getCNTsumY10() );
  setCNTsumY11( src.getCNTsumY11() );
  setCNTsumY12( src.getCNTsumY12() );
  setCNTsumY13( src.getCNTsumY13() );
  setCNTsumY14( src.getCNTsumY14() );
/*
//##############add sumX,Y RXN & MPC 07/3/14##############//

  setRXNsumX00( src.getRXNsumX00() );
  setRXNsumX01( src.getRXNsumX01() );
  setRXNsumX02( src.getRXNsumX02() );
  setRXNsumX03( src.getRXNsumX03() );
  setRXNsumX04( src.getRXNsumX04() );
  setRXNsumX05( src.getRXNsumX05() );
  setRXNsumX06( src.getRXNsumX06() );
  setRXNsumX07( src.getRXNsumX07() );
  setRXNsumX08( src.getRXNsumX08() );
  setRXNsumY00( src.getRXNsumY00() );
  setRXNsumY01( src.getRXNsumY01() );
  setRXNsumY02( src.getRXNsumY02() );
  setRXNsumY03( src.getRXNsumY03() );
  setRXNsumY04( src.getRXNsumY04() );
  setRXNsumY05( src.getRXNsumY05() );
  setRXNsumY06( src.getRXNsumY06() );
  setRXNsumY07( src.getRXNsumY07() );
  setRXNsumY08( src.getRXNsumY08() );

  setRXNsumX10( src.getRXNsumX10() );
  setRXNsumX11( src.getRXNsumX11() );
  setRXNsumX12( src.getRXNsumX12() );
  setRXNsumX13( src.getRXNsumX13() );
  setRXNsumX14( src.getRXNsumX14() );
  setRXNsumX15( src.getRXNsumX15() );
  setRXNsumX16( src.getRXNsumX16() );
  setRXNsumX17( src.getRXNsumX17() );
  setRXNsumX18( src.getRXNsumX18() );
  setRXNsumY10( src.getRXNsumY10() );
  setRXNsumY11( src.getRXNsumY11() );
  setRXNsumY12( src.getRXNsumY12() );
  setRXNsumY13( src.getRXNsumY13() );
  setRXNsumY14( src.getRXNsumY14() );
  setRXNsumY15( src.getRXNsumY15() );
  setRXNsumY16( src.getRXNsumY16() );
  setRXNsumY17( src.getRXNsumY17() );
  setRXNsumY18( src.getRXNsumY18() );

  setMPCsumX00( src.getMPCsumX00() );
  setMPCsumX01( src.getMPCsumX01() );
  setMPCsumX02( src.getMPCsumX02() );
  setMPCsumY00( src.getMPCsumY00() );
  setMPCsumY01( src.getMPCsumY01() );
  setMPCsumY02( src.getMPCsumY02() );

  setMPCsumX10( src.getMPCsumX10() );
  setMPCsumX11( src.getMPCsumX11() );
  setMPCsumX12( src.getMPCsumX12() );
  setMPCsumY10( src.getMPCsumY10() );
  setMPCsumY11( src.getMPCsumY11() );
  setMPCsumY02( src.getMPCsumY12() );

//###################################################//
*/
  setBBCrp00( src.getBBCrp00() );
  setBBCrp01( src.getBBCrp01() );
  setBBCrp02( src.getBBCrp02() );
  setBBCrp10( src.getBBCrp10() );
  setBBCrp11( src.getBBCrp11() );
  setBBCrp12( src.getBBCrp12() );

  setSMDrp00( src.getSMDrp00() );
  setSMDrp01( src.getSMDrp01() );
  setSMDrp02( src.getSMDrp02() );

  setMVDrp00( src.getMVDrp00() );
  setMVDrp01( src.getMVDrp01() );
  setMVDrp02( src.getMVDrp02() );
  setMVDrp10( src.getMVDrp10() );
  setMVDrp11( src.getMVDrp11() );
  setMVDrp12( src.getMVDrp12() );

  setFCLrp00( src.getFCLrp00() );
  setFCLrp01( src.getFCLrp01() );
  setFCLrp02( src.getFCLrp02() );

  setCNTrp10( src.getCNTrp10() );
  setCNTrp11( src.getCNTrp11() );
  setCNTrp12( src.getCNTrp12() );
  setCNTrp13( src.getCNTrp13() );
  setCNTrp14( src.getCNTrp14() );

//##############add rp RXN & MPC 07/3/14################//

  setRXNrp00( src.getRXNrp00() );
  setRXNrp01( src.getRXNrp01() );
  setRXNrp02( src.getRXNrp02() );
  setRXNrp03( src.getRXNrp03() );
  setRXNrp04( src.getRXNrp04() );
  setRXNrp05( src.getRXNrp05() );
  setRXNrp06( src.getRXNrp06() );
  setRXNrp07( src.getRXNrp07() );
  setRXNrp08( src.getRXNrp08() );

  setRXNrp10( src.getRXNrp10() );
  setRXNrp11( src.getRXNrp11() );
  setRXNrp12( src.getRXNrp12() );
  setRXNrp13( src.getRXNrp13() );
  setRXNrp14( src.getRXNrp14() );
  setRXNrp15( src.getRXNrp15() );
  setRXNrp16( src.getRXNrp16() );
  setRXNrp17( src.getRXNrp17() );
  setRXNrp18( src.getRXNrp18() );

  setMPCrp00( src.getMPCrp00() );
  setMPCrp01( src.getMPCrp01() );
  setMPCrp02( src.getMPCrp02() );

  setMPCrp10( src.getMPCrp10() );
  setMPCrp11( src.getMPCrp11() );
  setMPCrp12( src.getMPCrp12() );

//####################################################//

  ShutUp(0);
}
