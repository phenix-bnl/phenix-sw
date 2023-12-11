#ifndef __PACKET_PXL_DCM0_H__
#define __PACKET_PXL_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in PXL\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_pxl_dcm0 : public Packet_w4 {
#else
class  Packet_pxl_dcm0 : public Packet_w4 {
#endif

public:
  Packet_pxl_dcm0();
  Packet_pxl_dcm0(PACKET_ptr);
  ~Packet_pxl_dcm0();

/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data; we use this to bring up 
the misc. items in the FEM headers and trailers.


In addition, there is 
\begin{verbatim}
  packet->iValue(0,"EVTNR")
  packet->iValue(0,"DETID")
  packet->iValue(0,"MODADDR")
  packet->iValue(0,"FLAG")
  packet->iValue(0,"BCLCK")
  packet->iValue(0,"S_BCLCK03")
  packet->iValue(0,"EVTNR03")
  packet->iValue(0,"S_BCLCK47")
  packet->iValue(0,"EVTNR47")
  packet->iValue(0,"NEGBCLK03")
  packet->iValue(0,"PIXMASK03")
  packet->iValue(0,"NEGBCLK47")
  packet->iValue(0,"PIXMASK47")
  packet->iValue(0,"FAST_OR")
  packet->iValue(0,"TEMPERATURE1")
  packet->iValue(0,"TEMPERATURE2")
  packet->iValue(0,"STATUS")
  packet->iValue(0,"PARITY")

\end{verbatim}
*/


  virtual int    iValue(const int channel,const char *what);
  virtual int    iValue(const int chip,const int row);
  virtual void  dump ( OSTREAM& ) ;


protected:
  virtual int *decode (int *);
  virtual int *decode_misc (int *);

};

#endif /* __PACKET_PXL_DCM0_H__ */



