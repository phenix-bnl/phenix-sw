#ifndef __PACKET_TOF_DCM16_H__
#define __PACKET_TOF_DCM16_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in TOF\_DCM16 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_tof_dcm16 : public Packet_w4 {
#else
class  Packet_tof_dcm16 : public Packet_w4 {
#endif

public:
  Packet_tof_dcm16(PACKET_ptr);

 /** with the "what" parameter you can decide which aspect of
 the data is made available. 

\begin{verbatim}
packet->iValue(channel_nr, "QC1")   Q value Crossing 1
packet->iValue(channel_nr, "TC1")   T value Crossing 1
packet->iValue(channel_nr, "QC2")   Q value Crossing 2
packet->iValue(channel_nr, "TC2")   T value Crossing 3



the per-board information:

packet->iValue(board_nr, "FEM")            The board number
packet->iValue(board_nr, "AMU1")           the AMU C1 number
packet->iValue(board_nr, "AMU2")           the AMU C2 number



the values from the FEM header:

packet->iValue(0, "SMARKER")        The start marker
packet->iValue(0, "DETID")          Detector id
packet->iValue(0, "EVTNR")          Event number from FEM
packet->iValue(0, "MODULE")         Module number
packet->iValue(0, "FLAGWORD")       Flagword, whatever that is
packet->iValue(0, "BCLK")           Beam Clock counter value

packet->iValue(0, "PARITY")         Longitudinal Parity
\end{verbatim}

 */

  virtual int    iValue(const int channel,const char *what);

  virtual void  dump ( OSTREAM& ) ;
  

protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);
};

typedef struct tof_dcm16_crossing
{
  int AMUcell;
  int channeldata[32];
} tof_dcm16_crossing;

typedef struct tof_dcm16_board
{
  int evnr;
  int FEMaddr;

  tof_dcm16_crossing c1;
  tof_dcm16_crossing c2;

  int FEMLinkStatus;

} tof_dcm16_board;






#endif /* __PACKET_TOF_DCM16_H__ */
