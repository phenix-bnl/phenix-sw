#ifndef __EMC_SHORT_DATA_STRUCT__
#define __EMC_SHORT_DATA_STRUCT__
  typedef struct 
  {
    int timing;
    int post;
    int pre;
  } *emchannel;
#endif // __EMC_SHORT_DATA_STRUCT__
#ifndef __PACKET_PBSC_DCM0_NEW_H__
#define __PACKET_PBSC_DCM0_NEW_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PBSC\_DCM2 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_pbsc_dcm0_new : public Packet_pbsc_dcm0 {
#else
class  Packet_pbsc_dcm0_new : public Packet_pbsc_dcm0 {
#endif

public:
  Packet_pbsc_dcm0_new(PACKET_ptr);
  int* getData();
  /// Fills internal 144x5 array and returns its address.
  int fillList5(int *array5, int* channelN, int channelOffset);
  /// Fills the list of hit towers and returns n_towers_hit:
  /// n_towers_hit x{time, high post, low post, high pre, low pre};
  ///  n_towers_hit x{channelNumber+channelOffset}
  void fillArray144x5(int *array5);
 /// Fills EMC array 144x5: 144x{time, high post, low post, high pre, low pre}
  int fillCoarseEnergyList(int *Energy, int* channelN, int channelOffset); 
  void fillCoarseEnergyArray(int* Energy);
  int fillQtileList(int* qtileE, int*qtileN, int qtileOffset);  
protected:
  virtual int *decode (int *);
  virtual int *decode_amu (int *);
  virtual int *decode_misc (int *);
};

#endif /* __PACKET_PBSC_DCM2_H__ */








