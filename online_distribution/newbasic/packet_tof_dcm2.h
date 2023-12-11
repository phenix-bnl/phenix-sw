#ifndef __PACKET_TOF_DCM2_H__
#define __PACKET_TOF_DCM2_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in TOF\_DCM2 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_tof_dcm2 : public Packet_w4 {
#else
class  Packet_tof_dcm2 : public Packet_w4 {
#endif

public:
  Packet_tof_dcm2(PACKET_ptr);

 /** with the "what" parameter you can decide which aspect of
 the data is made available.

\begin{verbatim}
packet->iValue(channel_nr, "QC1")   Q value Crossing 1
packet->iValue(channel_nr, "QC2")   Q value Crossing 2
packet->iValue(channel_nr, "TC3")   T value Crossing 3
packet->iValue(channel_nr, "TC4")   T value Crossing 4

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

typedef struct tof_dcm2_crossing
{
  int AMUcell;			// the AMU cell number
  int channeldata[16];		// the 16 q or t data
} tof_dcm2_crossing;

typedef struct tof_dcm2_board
{
  int evnr;			// event number
  int FEMaddr;			// fem address

  tof_dcm2_crossing c1;		// 1st crossing, q1
  tof_dcm2_crossing c2;		// 2nd crossing, q2
  tof_dcm2_crossing c3;		// 3rd crossing, t3
  tof_dcm2_crossing c4;		// 4th crossing, t4

  int FEMLinkStatus;		// fem link status

} tof_dcm2_board;

#endif /* __PACKET_TOF_DCM2_H__ */
