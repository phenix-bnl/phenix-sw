#ifndef __PACKET_DCH_DCM0_H__
#define __PACKET_DCH_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in DCH\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_dch_dcm0 : public Packet_w4 {
#else
class  Packet_dch_dcm0 : public Packet_w4 {
#endif

public:
  Packet_dch_dcm0(PACKET_ptr);

/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up the misc. i
tems in the FEM headers and trailers.

There is 
\begin{verbatim}
 packet->iValue(0,"EVTNR")  The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(i,"PARITY")   The longitudinal parity
\end{verbatim}
*/

  virtual int    iValue(const int channel,const char *what);
  virtual void  dump ( OSTREAM& ) ;

protected:
  virtual int *decode (int *);
};

#endif /* __PACKET_DCH_DCM0_H__ */
