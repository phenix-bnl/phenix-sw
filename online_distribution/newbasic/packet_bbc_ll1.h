#ifndef __PACKET_BBC_LL1_H__
#define __PACKET_BBC_LL1_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in BBC\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_bbc_ll1 : public Packet_w4 {
#else
class  Packet_bbc_ll1 : public Packet_w4 {
#endif

public:
  Packet_bbc_ll1(PACKET_ptr);
  virtual int  iValue(const int channel, const char *what);
  virtual void dump (OSTREAM &)  ; // added by S. Belikov

protected:
  virtual int *decode (int *);
};

#endif /* __PACKET_BBC_LL1_H__ */
