#ifndef __PACKET_MUIDV_LL1_H__
#define __PACKET_MUIDV_LL1_H__
#include <packet_muid_ll1.h>

/**
   This is the packet which deals with data in MUID\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
  class WINDOWSEXPORT Packet_muid_ver_ll1 : public Packet_muid_ll1 {
#else
  class  Packet_muid_ver_ll1 : public Packet_muid_ll1 {
#endif

public:
    Packet_muid_ver_ll1(PACKET_ptr data) : Packet_muid_ll1(data) {};

protected:
  virtual int demangle ();
};

#endif /* __PACKET_MUIDV_LL1_H__ */
