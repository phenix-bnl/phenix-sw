#ifndef __PACKET_TEC_DCM1_H__
#define __PACKET_TEC_DCM1_H__

#include <packet_tec_dcm0.h>

/**
   This is the packet which deals with data in TEC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
  This packet at this poiint inherits basically all features from
  packet_tec_dcm0, except the dump routine, which is special. 

*/


#ifndef __CINT__
class WINDOWSEXPORT Packet_tec_dcm1 : public Packet_tec_dcm0 {
#else
class  Packet_tec_dcm1 : public Packet_tec_dcm0 {
#endif

public:
  Packet_tec_dcm1(PACKET_ptr);

  virtual int   iValue(const int channel,const char *what);
  virtual void  dump ( OSTREAM& ) ;

protected:

};

#endif /* __PACKET_TEC_DCM1_H__ */
