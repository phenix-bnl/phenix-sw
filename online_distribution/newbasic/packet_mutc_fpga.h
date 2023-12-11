#ifndef __PACKET_mutc_fpga_H__
#define __PACKET_mutc_fpga_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in mutc_fpga format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/

#ifndef __CINT__
class WINDOWSEXPORT Packet_mutc_fpga : public Packet_w4{ 
#else
class  Packet_mutc_fpga : public Packet_w4 {
#endif

public:
  Packet_mutc_fpga(PACKET_ptr);

  /** with the "what" parameter you can decide which aspect of
 the data is made available. This class is one of those which have
 several different "kinds" of data; we use this to bring up the AMU
 cell information and all the misc. items in the FEM headers and
 trailers.


The AMU info is available as
\begin{verbatim}
 packet->iValue(0,"AMU") AMU cell 0
 packet->iValue(1,"AMU") AMU cell 1
 packet->iValue(2,"AMU") AMU cell 2
 packet->iValue(3,"AMU") AMU cell 3
\end{verbatim}
In addition, there is 
\begin{verbatim}
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(0,"EVTNR")  The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(i,"USERWORD") The USERWORD i, where i is from 0 through 7
 packet->iValue(i,"PARITY")   The longitudinal parity
 packet->iValue(0,"SUMMARY")   The summary word
\end{verbatim}

In order to get a sparsified readout, we provide the "CHN" "WRDCNT", and "SPARSE"
interfaces. 

fillIntArray (iarr1,len, &nw, "xxx") returns as many ords as there are fired
channels, where xxx is

\begin{verbatim}
CHN       the list of channels
WRDCNT    the sample number
SPARSE    the data

\end{verbatim}

so if you do the three calls, 
\begin{verbatim}
fillIntArray (iarr1,len, &nw, "CHN")
fillIntArray (iarr2,len, &nw, "WRDCNT")
fillIntArray (iarr3,len, &nw, "SPARSE")

\end{verbatim}

then you can loop over the nw fired channels and get the channel from iarr1[i], 
the sample as iarr2[i], the data as iarr3[i].

  */



  virtual int    iValue(const int channel,const char *what);
  virtual int    iValue(const int channel,const int y);
  virtual int    fillIntArray (int iarr[], const int nlen, int *nwout,
                        const char *what="");
  virtual void   dump ( OSTREAM& );

protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);
};

#endif /* __PACKET_mutc_fpga_H__ */


