#ifndef __EMC_LONG_DATA_STRUCT__
#define __EMC_LONG_DATA_STRUCT__
  typedef struct 
  {
    int time;
    int highpost;
    int lowpost;
    int highpre;
    int lowpre;
  } *emclong;
#endif // __EMC_LONG_DATA_STRUCT__
#ifndef __PACKET_EMC_DCM32_H__
#define __PACKET_EMC_DCM32_H__

#define MIN_HIGH_GAIN 512

#include <packet_emc.h>

/**
   This is the packet which deals with EMC long format.
   It inherits from Packet_pbsc_dcm32 and adds some usefull functions.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_emc_dcm32 : public Packet_emc {
#else
class  Packet_emc_dcm32 : public Packet_emc {
#endif

public:
    Packet_emc_dcm32(PACKET_ptr);


  virtual void  dump ( OSTREAM& ) ;

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

 int    fillIntArray (int iarr[], const int nlen, int *nwout,
                        const char *what="");

/** this interface got a special keyword for the benefit of the LVL2 Trigger. 
Calling 


struct emcChannelLongList ecl[144];
int nw, NumberOfChannels;
NumberOfChannels = p->fillIntArray ( (int *) ecl, sizeof(*ecl), &nw, "SPARSE");

returns you the actually fired channels.

*/


protected:
  virtual int *decode (int *);
  virtual int *decode_amu (int *);
  virtual int *decode_misc (int *);
  int decode_to_sparse ( int *p );

};

#endif /* __PACKET_EMC_DCMS_H__ */











