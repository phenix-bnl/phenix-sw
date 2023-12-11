#ifndef __PACKET_ZDC_DCM1_H__
#define __PACKET_ZDC_DCM1_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in ZDC\_DCM1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_zdc_dcm1 : public Packet_w4 {
#else
class  Packet_zdc_dcm1 : public Packet_w4 {
#endif

public:
  Packet_zdc_dcm1(PACKET_ptr);

protected:
  virtual int *decode (int *);
};

#endif /* __PACKET_ZDC_DCM1_H__ */
