#ifndef __PACKET_FCAL_FPGA_H__
#define __PACKET_FCAL_FPGA_H__

#include <packet_w124.h>



/**
   This is the packet which deals with data in FCAL\_FPGA format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_fcal_fpga : public Packet_w4 {
#else
class  Packet_fcal_fpga : public Packet_w4 {
#endif

public:
  Packet_fcal_fpga(PACKET_ptr);


  /** with the "what" parameter you can decide which aspect of
 the data is made available. This class is one of those which have
 several different "kinds" of data; we use this to bring up the AMU
 cell information and all the misc. items in the FEM headers and
 trailers.


  */

  virtual int    iValue(const int channel,const char *what);


  int    iValue(const int channel,const int iy);

  int    fillIntArray (int iarr[], const int nlen, int *nwout,
                        const char *what="");


  void  dump ( OSTREAM& ) ;

  
protected:
  virtual int *decode (int *);
  virtual int *decode_amu (int *);
  virtual int *decode_misc (int *);
  int decode_to_sparse ( int * );
};


#endif /* __PACKET_FCAL_FPGA_H__ */
