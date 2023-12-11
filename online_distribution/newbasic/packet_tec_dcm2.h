#ifndef __PACKET_TEC_DCM2_H__
#define __PACKET_TEC_DCM2_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in TEC\_DCM2 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_tec_dcm2 : public Packet_w4 {
#else
class  Packet_tec_dcm2 : public Packet_w4 {
#endif

public:
  Packet_tec_dcm2(PACKET_ptr);

protected:
  virtual int *decode (int *);
};

#endif /* __PACKET_TEC_DCM2_H__ */
