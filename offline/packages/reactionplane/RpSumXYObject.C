
#include "RpSumXYObject.h"

ClassImp(RpSumXYObject)

using namespace std;

static int shutup = 0;

void RpSumXYObject::ShutUp(const int i)
{
  shutup = i;
}


void RpSumXYObject::warning(const char* field) const
{
  if (!shutup) {
    cout << "RpSumXYObject::using virtual function, doing nothing" << endl;
    cout << "Offending field == " << field << endl;
  }
}

void RpSumXYObject::Copy(const RpSumXYObject &src)
{
  ShutUp(1);
  setBBCsumW0( src.getBBCsumW0() );
  setBBCsumW1( src.getBBCsumW1() );
  setBBCsumW2( src.getBBCsumW2() );
  setSMDsumW0( src.getSMDsumW0() );
  setSMDsumW1( src.getSMDsumW1() );
  setSMDsumW2( src.getSMDsumW2() );
  setMVDsumW0( src.getMVDsumW0() );
  setMVDsumW1( src.getMVDsumW1() );
  setMVDsumW2( src.getMVDsumW2() );
  setFCLsumW0( src.getFCLsumW0() );
  setFCLsumW1( src.getFCLsumW1() );
  setFCLsumW2( src.getFCLsumW2() );
  setCNTsumW0( src.getCNTsumW0() );
  setCNTsumW1( src.getCNTsumW1() );
  setCNTsumW2( src.getCNTsumW2() );
  setCNTsumW3( src.getCNTsumW3() );
  setCNTsumW4( src.getCNTsumW4() );

//########add sumW RXN & MPC ...07/03/12#########//

  setRXNsumW0( src.getRXNsumW0() );
  setRXNsumW1( src.getRXNsumW1() );
  setRXNsumW2( src.getRXNsumW2() );
  setRXNsumW3( src.getRXNsumW3() );
  setRXNsumW4( src.getRXNsumW4() );
  setRXNsumW5( src.getRXNsumW5() );
  setRXNsumW6( src.getRXNsumW6() );
  setRXNsumW7( src.getRXNsumW7() );
  setRXNsumW8( src.getRXNsumW8() );
  
 
  setMPCsumW0( src.getMPCsumW0() );
  setMPCsumW1( src.getMPCsumW1() );
  setMPCsumW2( src.getMPCsumW2() );

//###########################################//

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


//#############add sumX,Y RXN & MPC 07/3/12############//

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
  setMPCsumY12( src.getMPCsumY12() );
  
//################################################//

  ShutUp(0);
}
