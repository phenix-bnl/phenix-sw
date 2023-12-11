#ifndef __PACKET_MUID_DCM0_H__
#define __PACKET_MUID_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in MUID\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_muid_dcm0 : public Packet_w4 {
#else
class  Packet_muid_dcm0 : public Packet_w4 {
#endif

public:
  Packet_muid_dcm0(PACKET_ptr);

  int iValue(const int channel);
  int    iValue(const int channel,const char *what);
  int    iValue(const int channel,const int roc);
  int fillIntArray (int iarr[],
			const int nlen, int *nwout,
			const char *what="");
  virtual void  dump ( OSTREAM& ) ;


/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up 
the misc. items in the FEM headers and trailers.

The standard "what" parameters for an FEM packet are 
\begin{verbatim}
 packet->iValue(0,"EVTNR")  The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(0,"PARITY")   The longitudinal parity
 packet->iValue(i,"USERWORD") The userword i
\end{verbatim}

The fillIntArray function has received another "what" parameter. 

\begin{verbatim}
fillIntArray( iarr, length, &numberofwords, "SPARSE") 
\end{verbatim} 

will return a list of all non-zero words. You get a shortlist of
all fired cells. 


*/



protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);

};

#endif /* __PACKET_MUID_DCM0_H__ */
