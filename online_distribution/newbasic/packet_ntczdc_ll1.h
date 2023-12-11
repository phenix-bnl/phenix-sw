#ifndef __PACKET_NTCZDC_LL1_H__
#define __PACKET_NTCZDC_LL1_H__
#include <packet_w124.h>
#include <string.h>

/**
   This is the packet which deals with data in NTCZDC\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
  class WINDOWSEXPORT Packet_ntczdc_ll1 : public Packet_w4 {
#else
  class  Packet_ntczdc_ll1 : public Packet_w4 {
#endif

public:
  Packet_ntczdc_ll1(PACKET_ptr);
  virtual int  iValue(const int channel, const char *what);
  virtual void dump ( OSTREAM& );

protected:

  virtual int *decode (int *); 

};

#endif /* __PACKET_NTCZDC_LL1_H__ */
