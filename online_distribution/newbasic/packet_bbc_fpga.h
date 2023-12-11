#ifndef __PACKET_BBC_FPGA_H__
#define __PACKET_BBC_FPGA_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in BBC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_bbc_fpga : public Packet_w4 {
#else
class  Packet_bbc_fpga : public Packet_w4 {
#endif

public:
  Packet_bbc_fpga();
  Packet_bbc_fpga(PACKET_ptr);
  ~Packet_bbc_fpga();

/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up 
the misc. items in the FEM headers and trailers.

BBC has charge information and two time infos. Charge is considered
the default return value, so iValue(i) gives you the charge info of channel i.

With iValue(i, "T1") you get the discriminator 1 time, and
iValue(i, "T2") gives you the discriminator 2 time.

Each of the boards has an ID embedded in the data stream. There are at most 
16 boards. With 
\begin{verbatim}
 packet->iValue(i,"BOARD")
\end{verbatim}
you get the board id of board i.


In addition, there is 
\begin{verbatim}
 packet->iValue(0,"EVTNR")    The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"FLAG")     The Module ID
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(i,"PARITY")   The longitudinal parity
 packet->iValue(i,"SUMMARY")  The DCM summary word
\end{verbatim}
*/


  int    iValue(const int channel);
  int    iValue(const int channel,const char *what);
  int    iValue(const int channel,const int board);
  void  dump ( OSTREAM& ) ;


protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);
  int no_boards;
};

#endif /* __PACKET_BBC_FPGA_H__ */



