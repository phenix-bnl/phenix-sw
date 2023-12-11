#ifndef __PACKET_PC_FPGA_H__
#define __PACKET_PC_FPGA_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PC\_DCM"3"  format.
   It has the 13 words per row, where the 1st word is a check to verify/detect
   byte shifts.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_pc_fpga : public Packet_w4 {
#else
class  Packet_pc_fpga : public Packet_w4 {
#endif

public:
  Packet_pc_fpga(PACKET_ptr);

  int iValue(const int channel);
  int   iValue(const int channel,const char *what);
  void  dump ( OSTREAM& os=COUT) ;


/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up 
the misc. items in the FEM headers and trailers.

There are a few standard ones listed below. The one special for this
packet is the iValue(i, "CHECKCTR") call, which gives you the check 
counter (someone can give me a better word) for row i.

A new "MISMATCH" query was introduced. 
iValue(0, "MISMATCH") returns 0 (false) when all the check values match.
If not, you will get the number of the row where the first deviating 
check wird appears, which is true (you have to look with 
"CHECKCTR" if there are more).

So you can just do if (p->iValue(0, "MISMATCH") ) do_something();


The standard "what" parameters for an FEM packet are 
\begin{verbatim}
 packet->iValue(0,"EVTNR")  The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(i,"PARITY")   The longitudinal parity
\end{verbatim}
*/



protected:


  int *decode (int *);
  int *decode_misc (int *);

};

#endif /* __PACKET_PC_FPGA_H__ */
