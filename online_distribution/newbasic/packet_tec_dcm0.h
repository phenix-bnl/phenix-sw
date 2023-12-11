#ifndef __PACKET_TEC_DCM0_H__
#define __PACKET_TEC_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in TEC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_tec_dcm0 : public Packet_w4 {
#else
class  Packet_tec_dcm0 : public Packet_w4 {
#endif

public:
  Packet_tec_dcm0(PACKET_ptr);

/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up the misc. 
items in the FEM headers and trailers.

There is 
\begin{verbatim}
 packet->iValue(0,"EVTNR")  The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"FLAGS")    The "FLAGS: word (who can explain what this is) 
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(0,"USERWORD")   The User word
 packet->iValue(0,"PARITY")   The longitudinal parity
\end{verbatim}
*/

  virtual int   iValue(const int channel,const char *what);


/** 
The  iValue(const int channel,const int time) 
interface returns the (channel-number, time bin) value. 
It is the main interface to the data.

There are also the fillIntArray and getIntArray interfaces,
which return all 64*80 samples in one big linear array. 

*/

  virtual int   iValue(const int channel,const int time);

  int    fillIntArray (int iarr[], const int nlen, int *nwout,
                        const char *what="");

/**
The fillIntArray takes a special "SPARSE" interface that will 
return only the non-zero channels in a special tecChannelList
structure.

*/
 
 

  virtual void  dump ( OSTREAM& ) ;

protected:
  virtual int *decode (int *);

  virtual int decode_to_sparse ( int *p );

};

#endif /* __PACKET_TEC_DCM0_H__ */
