#ifndef __PACKET_MPCEXTEST_FPGA_H__
#define __PACKET_MPCEXTEST_FPGA_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PXL\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_mpcextest_fpga : public Packet_w4 {
#else
class  Packet_mpcextest_fpga : public Packet_w4 {
#endif

public:
  Packet_mpcextest_fpga();
  Packet_mpcextest_fpga(PACKET_ptr);
  ~Packet_mpcextest_fpga();

/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up 
the misc. items in the FEM headers and trailers.


In addition, there is 
\begin{verbatim}
  packet->iValue(0,"EVTNR")    
  packet->iValue(0,"DETID")
  packet->iValue(0,"DIBID")
  packet->iValue(0,"CIBID")
  packet->iValue(0,"FLAG")
  packet->iValue(0,"BCLCK")
  packet->iValue(0,"NR_RCC")
  packet->iValue(0,"FIRMWARE")
  packet->iValue(0,"CHANNELDATA")
  packet->iValue(0,"PEDCORRECTED")
  packet->iValue(rcc,"ENABLED")
  packet->iValue(rcc,"RCCADDR")
  packet->iValue(rcc,"RCCHYBRID")
  packet->iValue(rcc,"RCCBCLK")
  packet->iValue(rcc,"PARITY")
  packet->iValue(rcc,"DCMPARITY")
  packet->iValue(0,"ERROR")

// new fields as of 3/18/15
  packet->iValue(0,"STACK")
  packet->iValue(0,"STATEPHASE")

\end{verbatim}
*/


  int    iValue(const int ch);
  int    iValue(const int hybrid,const char *what);
  int    iValue(const int channel, const int iy, const char *what);
  int    iValue(const int chip,const int row);
  void  dump ( OSTREAM& ) ;


protected:
  virtual int preset();
  virtual int *decode (int *);
  virtual int calculate_parity();

  int grayToBinary(int num);
  int binaryToGray(int num);

  // define thos tso the following becomes easier to read
#define CHAINS 4
#define CHIPS 12

  int chip_enabled[CHIPS][CHAINS];  // just a flag if this hybrid is enabled [ it shopud never be able to be disabled]  
  int cellnumber[CHIPS][CHAINS];      // 48 cell numbers
  int nr_chips[CHAINS];                // the per-chain number of hybrids
  int chips[128][CHIPS][CHAINS];               // the actual payload - this is what all this crap is for. 

  int evt_nr;
  int chainmask;
  int maxnrchips;
  int CBtestMode;
  int FEMTestMode;
  int graydecoding;
  int femnr;
  int detid;
  int bclk;
  int bclk_msb;
  int bclk_extended;
  int firmwareversion;
  int headerlength;
  int trailerlength;
  int BEM[4];  // mask of enabled modules in chain x

  int PARstTIME;
  int TrigPhase;
  int HFStat;
  int EOEStamp;
  int CRC;
  int Stack;
  int StatePhase;

  int parityok;
  int _error;

  int parity_comparison;
  int calculated_parity;
  int parity_is_calculated;

  int is_decoded;

};

#endif /* __PACKET_MPCEXTESTo_FPGA_H__ */



