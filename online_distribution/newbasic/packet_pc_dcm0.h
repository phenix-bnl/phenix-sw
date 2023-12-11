#ifndef __PACKET_PC_DCM0_H__
#define __PACKET_PC_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_pc_dcm0 : public Packet_w4 {
#else
class  Packet_pc_dcm0 : public Packet_w4 {
#endif

public:
  Packet_pc_dcm0(PACKET_ptr);
  virtual int    iValue(const int channel,const char *what);
 virtual void  dump ( OSTREAM& ) ;

protected:


  virtual int *decode (int *);
  virtual int *decode_pad (int *);
  virtual int *decode_misc (int *);

};

#endif /* __PACKET_PC_DCM0_H__ */
