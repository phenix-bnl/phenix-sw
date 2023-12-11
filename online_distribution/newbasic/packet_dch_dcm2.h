#ifndef __PACKET_DCH_DCM2_H__
#define __PACKET_DCH_DCM2_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in DCH\_DCM2 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_dch_dcm2 : public Packet_w4 {
#else
class  Packet_dch_dcm2 : public Packet_w4 {
#endif

public:
  Packet_dch_dcm2(PACKET_ptr);

protected:
  virtual int *decode (int *);
};

#endif /* __PACKET_DCH_DCM2_H__ */
