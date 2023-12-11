#ifndef __PACKET_EMC_OLDDCM32_H__
#define __PACKET_EMC_OLDDCM32_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PBSC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_emc_olddcm32 : public Packet_w4 {
#else
class  Packet_emc_olddcm32 : public Packet_w4 {
#endif

public:
  Packet_emc_olddcm32(PACKET_ptr);


  /** with the "what" parameter you can decide which aspect of
 the data is made available. This class is one of those which have
 several different "kinds" of data; we use this to bring up the AMU
 cell information and all the misc. items in the FEM headers and
 trailers.


The AMU info is available as
\begin{verbatim}
 packet->iValue(0,"AMU") AMU cell from timing conversion 
 packet->iValue(1,"AMU") AMU cell from "pre" conversion
 packet->iValue(2,"AMU") AMU cell from "post" conversion
\end{verbatim}
In addition, there is 
\begin{verbatim}
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(0,"EVTNR")  The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(i,"USERWORD") The USERWORD i, where i is from 0 through 7
 packet->iValue(i,"PARITY")   The longitudinal parity
\end{verbatim}
  */

  virtual int    iValue(const int channel,const char *what);

/** This gives access to the 5 data words per channel, in case the
 use wants to look at that raw information.

 We chose to
implement the 5 words of information as the two-dimensional
interface, so packet->iValue(k,i) gives you the word "i" of
channel k, where i is 
\begin{verbatim}
packet->iValue(k,0) timing tag (TDC)
packet->iValue(k,1) high gain post sample
packet->iValue(k,2) low gain post sample
packet->iValue(k,3) high gain pre sample
packet->iValue(k,4) low gain pre sample
\end{verbatim}
and "k" is the channel number from 0 through 143. 
*/
 virtual int    iValue(const int channel,const int iy);

 virtual void  dump ( std::ostream& ) ;
  
protected:
  virtual int *decode (int *);
  virtual int *decode_amu (int *);
  virtual int *decode_misc (int *);

};

#endif /* __PACKET_EMC_OLDDCM32_H__ */
