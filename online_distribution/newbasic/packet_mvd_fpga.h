#ifndef __PACKET_MVD_FPGA_H__
#define __PACKET_MVD_FPGA_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in MVD_FPGA format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
class Packet_mvd_fpga : public Packet_w4 {

public:
  Packet_mvd_fpga(PACKET_ptr);

  int iValue(const int ich);
  int iValue(const int ich, const char *what);
  void  dump ( OSTREAM& os = COUT ) ;

protected:
  virtual int *decode (int *);
  int *decode_misc (int *nwout);

};

#endif /* __PACKET_MVD_FPGA_H__ */
