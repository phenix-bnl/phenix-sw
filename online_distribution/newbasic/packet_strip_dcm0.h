#ifndef __PACKET_STRIP_DCM0_H__
#define __PACKET_STRIP_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PXL\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_strip_dcm0 : public Packet_w4 {
#else
class  Packet_strip_dcm0 : public Packet_w4 {
#endif

public:
  Packet_strip_dcm0();
  Packet_strip_dcm0(PACKET_ptr);
  ~Packet_strip_dcm0();

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

\end{verbatim}
*/


  int    iValue(const int ch);
  int    iValue(const int channel,const char *what);
  int    iValue(const int chip,const int row);
  void  dump ( OSTREAM& ) ;


protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);
  int rcc_enabled[6];
  int nr_rcc;
  int channeldata;
  int ped_corr;
  int dib_version;
  int parity;
  int parity_2;

};

#endif /* __PACKET_STRIP_DCM0_H__ */



