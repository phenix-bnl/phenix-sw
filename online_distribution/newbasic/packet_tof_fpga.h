#ifndef __PACKET_TOF_FPGA_H__
#define __PACKET_TOF_FPGA_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in TOF\_DCM2 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_tof_fpga : public Packet_w4 {
#else
class  Packet_tof_fpga : public Packet_w4 {
#endif

public:
  Packet_tof_fpga(PACKET_ptr);

 /** with the "what" parameter you can decide which aspect of
 the data is made available.

Note that the recent change has changed the readout from a 
Q2 value to Q3, so we eliminated "QC2" and made it "QC3". 


\begin{verbatim}
packet->iValue(channel_nr, "QC1")   Q value Crossing 1
packet->iValue(channel_nr, "QC3")   Q value Crossing 3
packet->iValue(channel_nr, "TC3")   T value Crossing 3
packet->iValue(channel_nr, "TC4")   T value Crossing 4
\end{verbatim}

The per-board information:

\begin{verbatim}
packet->iValue(board_nr, "FEM")            The board number
packet->iValue(board_nr, "AMU1")           the AMU C1 number
packet->iValue(board_nr, "AMU3")           the AMU C2 number
packet->iValue(board_nr, "AMU4")           the AMU C2 number
\end{verbatim}

Note that we also eliminated "AMU2" - the field is no longer there.

the values from the FEM header:

\begin{verbatim}
 packet->iValue(0,"EVTNR")    The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"FLAG")     The Module ID
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(i,"PARITY")   The longitudinal parity
 packet->iValue(i,"SUMMARY")  The DCM summary word
\end{verbatim}

 */

  int    iValue(const int channel,const char *what);
  void  dump ( OSTREAM& ) ;
  ~Packet_tof_fpga() {};

protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);
};


#endif /* __PACKET_TOF_FPGA_H__ */
