#ifndef __PACKET_EMC_H__
#define __PACKET_EMC_H__

#define MIN_HIGH_GAIN 512
#define MAX_HIGH_GAIN 3500
#include <packet_w124.h>

/**
   This is the packet which deals with data in PBSC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_emc : public Packet_w4 {
#else
class  Packet_emc : public Packet_w4 {
#endif

public:
  Packet_emc(PACKET_ptr);
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


  virtual int filliList5x144(int **array2D, int* channelN, int arrayOffset, int channelOffset, int threshold)=0;
/** Any EMCal data format.
    Fills the list of hit towers and their 5 values.
    Five values are:  time, high post, low post, high pre, low pre.
    Skips disconnected AMUADC channels, thus checking only 144 channels.
    Returns number of list entries.
    Parameters:
 int **array2D     - pointer to array[5][N] where data will be stored.
                     N must be at least 144.
                     Array IS NOT created by function, you must supply function with
                     pointer to already existing array!  
 int array Offset  - offset of 2D array filling: data will be filled starting from
                     array[i][arrayOffset], where i=0,...4.
                     Necessary when array of more then one EMCal FEM channels is filled.
 int* channelN     - array where list of hit towers will be stored.
                     Must be at least 144 long
                     Tower number in the list is FEM_channel + channelNumberOffset.
 int channelOffset - offset used to calculate tower number.
 int threshold     - used as additional threshold for zero suppression: only 
                     channels with Energy > threshold are added into the list
*/
 
  virtual int fillfList5x144(float** array2D, int* channelN, int arrayOffset, int channelOffset, int threshold)=0;
  /** The same as filliList5x144, but fills 2D float array.
      Created just to correspond to the old EEMCal software */
  virtual int filliList6x144(int** array2D, int arrayOffset, int channelOffset, int threshold)=0;
  /** The same as filliList5x144, but array channelN is added to array2D as 6-th element */ 
  virtual int filliList144x6(int* array1D, int channelOffset, int threshold)=0;
  /** The same as filliList5x144, but array1D is an array of the size [Nx6],
   and 6 values are: time, high post, low post, high pre, low pre, chennel# */ 
  virtual int filliList5x192(int** array2D, int* address, int arrayOffset, int addressOffset, int threshold)=0;
/** EMCal long data format only.
    Fills the list of hit towers and their 5 values.
    Five values are:  time, high post, low post, high pre, low pre.
    Checks all 192 channels.
    Returns number of list entries.
    Parameters:
 int **array2D      - pointer to array[5][N] where data will be stored.
                      N must be at least 192.
                      Array IS NOT created by function, you must supply function with
                      pointer to already existing array!  
 int array Offset   - offset of 2D array filling: array will be filled starting from
                      array[i][arrayOffset], where i=0,...4.
                      Necessary when array of more then one EMCal FEM channels is filled.
 int* channelN      - array where list of hit towers will be stored.
                      Must be at least 192 long.
                      Tower number in the list is FEM_channel + channelNumberOffset.
 int channelOffset  - offset used to calculate tower number.
 int threshold      - used as additional threshold for zero suppression: only 
                      channels with Energy > threshold are added into the list
*/ 

  virtual int fillfList5x192(float** array2D, int* address, int arrayOffset, int addressOffset, int threshold)=0;
  /** The same as filliList5x192, but fills float 2D array */
  virtual int filliList6x192(int** array2D, int arrayOffset, int channelOffset, int threshold)=0;
  /** The same as filliList5x192, but array channelN is added to array2D as 6-th element */ 
  virtual int filliList192x6(int* array1D, int channelOffset, int threshold)=0;
  /** The same as filliList5x192, but array1D is an array of the size [Nx6],
   and 6 values are: time, high post, low post, high pre, low pre, chennel# */ 
  
  virtual int fillArray192x5(int *array5)=0;
/** EMCal long data format only.
    Fills EMC array[192x5]: 192x{time, high post, low post, high pre, low pre}
   Missed channels will have zeros in all 5 values.
   Returns number of channels = 192 if no errors, or 0 if errors were encountered.
   Parameters:
 int *array5      - pointer to 1D array[144x5] where data will be stored.
                    Array IS NOT created by function, you must supply function with
                    pointer to already existing array!  
*/
  virtual int fillArray144x5(int *array5)=0;
/** Any EMCal data format.
   Fills EMC array[144x5]: 144x{time, high post, low post, high pre, low pre}
   Checks only connected AMUADC channels, that is only 144 channels.
   Missed channels will have zeros in all 5 values.
   Returns number of channels = 144 if no errors, or 0 if errors were encountered.
   Parameters:
 int *array5      - pointer to 1D array[144x5] where data will be stored.
                    Array IS NOT created by function, you must supply function with
                    pointer to already existing array!  
*/
  virtual int fillCoarseEnergyList(int *Energy, int* channelN, int channelOffset, int threshold)=0;
/** EMCal any data format.
    Fills the list of towers energy (in High Gain ADC counts).
    If necessary makes rough convertion of Low Gain ADC counts into that of High Gain by
    shifting LG<<4 (assumes that High/Low ratio is 16).
    Skips disconnected AMUADC channels, thus checking only 144 channels.
    Returns number of list entries. 
    Parameters:
 int *Energy             - pointer to array[N] where towers energy will be stored.
                           N must be at least 144.
                           Array IS NOT created by function, you must supply function with
                           pointer to already existing array!  
 int* channelN           - array where list of hit towers will be stored.
                           Must be at least 144 long
                           Array IS NOT created by function, you must supply function with
                           pointer to already existing array!  
                           Tower number in the list is FEM_channel + channelNumberOffset.
 int channelOffset       - offset used to calculate tower number.
 int threshold           - used as additional threshold for zero suppression: only 
                           channels with Energy > threshold are added into the list
*/ 
 
  virtual int fillCoarseEnergyArray(int* Energy)=0;
/** EMCal any data format.
    Fills array[144] with EMC energy values (in high gain ADC counts).
    Skips disconnected AMUADC channels, thus checking only 144 channels.
    Parameters:
    Returns number of channels = 144 if no errors, or 0 if errors were encountered.
 int *Energy             - pointer to array[N] where towers energy will be stored.
                           N must be at least 144.
                           Array IS NOT created by function, you must supply function with
                           pointer to already existing array!  
*/ 
  virtual int fillQtileArray(int* qtileEnergy)=0;
/** EMCal any data format.
    Fills array[36] of 2x2 tiles  energy value
    Skips disconnected AMUADC channels, thus checking only 144 channels.
    If no errors returns 36 = number of qtiles, else returns 0.
    Parameters:
 int *qtileEnergy        - pointer to array[N], N must be at least 36.
                           Array IS NOT created by function, you must supply function with
                           pointer to already existing array!  
*/ 
  virtual int fillQtileList(int* qtileE, int*qtileN, int qtileOffset, int threshold)=0;  
/** EMCal any data format.
    Fills the list of 2x2 tiles energy (in High Gain ADC counts). 
    Skips disconnected AMUADC channels, thus checking only 144 channels.
    Returns number of tiles with energy > threshold.
    Parameters:
 PACKET_ptr packet       - pointer to EMC packet
 int *qtileE             - pointer to array[N] where tiles energy will be stored.
                           N must be at least 36.
                           Array IS NOT created by function, you must supply function with
                           pointer to already existing array!  
 int* qtileN             - array with list of hit tiles. Must be at least 36 long
                           Tile number in the list is FEM_qtile + qtileNumberOffset.
                           Array IS NOT created by function, you must supply function with
                           pointer to already existing array!  
 int qtileOffset         - offset used to calculate qtile number.
 int threshold           - used as additional threshold for zero suppression: only 
                           qtiles with Energy > threshold are added into the list
*/ 




 protected:
  int max_channels;
  virtual int *decode_amu (int *)=0;
  virtual int *decode_misc (int *)=0;
};

#endif /* __PACKET_PBSC_DCM32_H__ */
