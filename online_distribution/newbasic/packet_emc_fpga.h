#ifndef __EMC_FPGA_DATA_STRUCT__
#define __EMC_FPGA_DATA_STRUCT__
  typedef struct 
  {
    int highpost;
    int highpre;
    int lowpost;
    int lowpre;
    int time;
  } *emcfpga;
#endif // __EMC_FPGA_DATA_STRUCT__
#ifndef __PACKET_EMC_FPGA_H__
#define __PACKET_EMC_FPGA_H__


#include <packet_emc.h>

/**
   This is the packet which deals with data in EMC\_FPGA format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_emc_fpga : public Packet_emc {
#else
class  Packet_emc_fpga : public Packet_emc {
#endif

public:
  Packet_emc_fpga(PACKET_ptr);


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
 packet->iValue(0,"EVTNR")    The FEM event number
 packet->iValue(0,"MODULE")   The Module ID
 packet->iValue(0,"FLAG")     The Module ID
 packet->iValue(0,"BCLK")     Beam clock value from FEM
 packet->iValue(i,"PARITY")   The longitudinal parity
 packet->iValue(i,"SUMMARY")  The DCM summary word
\end{verbatim}

  */

  virtual int    iValue(const int channel,const char *what);

/** This gives access to the 5 data words per channel, in case the
 use wants to look at that raw information.

 We chose to
implement the 5 words of information as the two-dimensional
interface, so packet->iValue(k,i) gives you the word "i" of
channel k, where i is 
\begin{verbatim}
packet->iValue(k,0) timing tag (TDC)
packet->iValue(k,1) high gain post sample
packet->iValue(k,2) low gain post sample
packet->iValue(k,3) high gain pre sample
packet->iValue(k,4) low gain pre sample
\end{verbatim}
and "k" is the channel number from 0 through 143. 
*/
  int    iValue(const int channel,const int iy);

  int    fillIntArray (int iarr[], const int nlen, int *nwout,
                        const char *what="");

/** this interface got a special keyword for the benefit of the LVL2 Trigger. 
Calling 


struct emcChannelLongList ecl[144];
int nw, NumberOfChannels;
NumberOfChannels = p->fillIntArray ( (int *) ecl, sizeof(*ecl), &nw, "SPARSE");

returns you the actually fired channels.

*/



  void  dump ( OSTREAM& ) ;
  virtual int filliList5x144(int** array2D, int* channelN, int arrayOffset, int channelOffset, int threshold);

  virtual int fillfList5x144(float** array2D, int* channelN, int arrayOffset, int channelOffset, int threshold);

  virtual int filliList6x144(int** array2D, int arrayOffset, int channelOffset, int threshold);

  virtual int filliList144x6(int* array1D, int channelOffset, int threshold);

  virtual int fillArray144x5(int *array5);

  virtual int filliList5x192(int** array2D, int* address, int arrayOffset, int addressOffset, int threshold);

  virtual int fillfList5x192(float** array2D, int* address, int arrayOffset, int addressOffset, int threshold);

  virtual int filliList6x192(int** array2D, int arrayOffset, int channelOffset, int threshold);

  virtual int filliList192x6(int* array1D, int channelOffset, int threshold);

  virtual int fillArray192x5(int *array5);

  virtual int fillCoarseEnergyList(int *Energy, int* channelN, int channelOffset, int threshold); 

  virtual int fillCoarseEnergyArray(int* Energy);

  virtual int fillQtileArray(int* qtileEnergy);

  virtual int fillQtileList(int* qtileE, int*qtileN, int qtileOffset, int threshold);  
  
protected:
  virtual int *decode (int *);
  virtual int *decode_amu (int *);
  virtual int *decode_misc (int *);
  virtual int decode_to_sparse ( int *p, const int nlen );

  int hitlength;
  int hitlist[192];

};

#endif /* __PACKET_EMC_FPGA_H__ */
