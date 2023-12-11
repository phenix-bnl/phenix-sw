#ifndef __EMC_SHORT_DATA_STRUCT__
#define __EMC_SHORT_DATA_STRUCT__
  typedef struct 
  {
    int timing;
    int post;
    int pre;
  } *emcshort;
#endif // __EMC_SHORT_DATA_STRUCT__
#ifndef __PACKET_EMC_DCMS_H__
#define __PACKET_EMC_DCMS_H__


#include <packet_emc.h>

/**
   This is the packet which deals with EMC short format.
   It inherits from Packet_pbsc_dcms and adds some usefull functions.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_emc_dcms : public Packet_emc {
#else
class  Packet_emc_dcms : public Packet_emc {
#endif

public:
  Packet_emc_dcms(PACKET_ptr);
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

protected:
  virtual int *decode (int *);
  virtual int *decode_amu (int *);
  virtual int *decode_misc (int *);
};

#endif /* __PACKET_EMC_DCMS_H__ */








