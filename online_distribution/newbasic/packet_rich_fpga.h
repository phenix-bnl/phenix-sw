#ifndef __PACKET_RICH_FPGA_H__
#define __PACKET_RICH_FPGA_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in RICH_FPGA format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
class Packet_rich_fpga : public Packet_w4 {

public:
  Packet_rich_fpga(PACKET_ptr);


  int iValue(const int ich);



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
 packet->iValue(0,"ID")       The detector id
 packet->iValue(0,"EVTNR")    The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"FLAG")     The Flag value
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(i,"PARITY")   The longitudinal parity
 packet->iValue(i,"SUMMARY")  The DCM summary word
 packet->iValue(i,"USERWORD") The USERWORD i, where i is from 0 through 7


\end{verbatim}

  */


  int iValue(const int ich, const char *what);

/** This gives access to the 3 data words per channel, in case the
 use wants to look at that raw information.

 We chose to
implement the 3 words of information as the two-dimensional
interface, so packet->iValue(k,i) gives you the word "i" of
channel k, where i is 
\begin{verbatim}
packet->iValue(k,0) timing tag (TDC)
packet->iValue(k,1) post sample
packet->iValue(k,2) pre sample
\end{verbatim}
and "k" is the channel number from 0 through 160. 

There are definitions which make this a bit more trasnparent; you can use

\begin{verbatim}
packet->iValue(k,RICH_TIME) timing tag (TDC)
packet->iValue(k,RICH_POSTSAMPLE) post sample
packet->iValue(k,RICH_PRESAMPLE) pre sample
\end{verbatim}


*/


  int iValue(const int ich, const int iy);
  void dump ( OSTREAM& os = COUT ) ;
  
/** this interface got a special keyword for the benefit of the LVL2 Trigger. 
Calling 

\begin{verbatim}
struct richChannelList rcl[160];
int nw, NumberOfChannels;
NumberOfChannels = p->fillIntArray ( (int *) rcl, 160*sizeof(*rcl), &nw, "SPARSE");
\end{verbatim}

returns you the actually fired channels.

richChannelList is defined as
\begin{verbatim}
struct richChannelList
{
  int channel;
  int time;
  int post;
  int pre;
};
\end{verbatim}

*/

  int    fillIntArray (int iarr[], const int nlen, int *nwout,
                        const char *what="");

protected:
  virtual int *decode (int *);
  int *decode_misc (int *nwout);
  int decode_to_sparse ( int *p );

};

#endif /* __PACKET_RICH_FPGA_H__ */
