#ifndef __PACKET_EMC_FPGASHORT_H__
#define __PACKET_EMC_FPGASHORT_H__


#include <packet_emc_fpga.h>

/**
   This is the packet which deals with data in EMC\_FPGA format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_emc_fpgashort : public Packet_emc_fpga {
#else
class  Packet_emc_fpgashort : public Packet_emc_fpga {
#endif

public:
  Packet_emc_fpgashort(PACKET_ptr);


  /** with the "what" parameter you can decide which aspect of
 the data is made available. This class is one of those which have
 several different "kinds" of data; we use this to bring up the AMU
 cell information and all the misc. items in the FEM headers and
 trailers.

 Most of this is inherited from the parent.

  */

  
protected:
  virtual int *decode (int *);
  //  virtual int *decode_amu (int *);
  //  virtual int *decode_misc (int *);
  int decode_to_sparse ( int *p,  const int nlen );
};

#endif /* __PACKET_EMC_FPGA_H__ */
