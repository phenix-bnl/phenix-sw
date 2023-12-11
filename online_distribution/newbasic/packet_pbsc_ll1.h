#ifndef __PACKET_PBSC_LL1_H__
#define __PACKET_PBSC_LL1_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PBSC\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_pbsc_ll1 : public Packet_w4 {
#else
class  Packet_pbsc_ll1 : public Packet_w4 {
#endif

public:
  Packet_pbsc_ll1(PACKET_ptr);

protected:
  virtual int *decode (int *);
};

#endif /* __PACKET_PBSC_LL1_H__ */
