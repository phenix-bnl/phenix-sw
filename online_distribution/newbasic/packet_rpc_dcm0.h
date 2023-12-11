#ifndef __PACKET_RPC_DCM0_H__
#define __PACKET_RPC_DCM0_H__


#include <packet_w124.h>

/**
   This is the packet which deals with data in RPC\_DCM0 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
class WINDOWSEXPORT Packet_rpc_dcm0 : public Packet_w4 {
#else
class  Packet_rpc_dcm0 : public Packet_w4 {
#endif

public:
  Packet_rpc_dcm0();
  Packet_rpc_dcm0(PACKET_ptr);
  ~Packet_rpc_dcm0();

/** with the "what" parameter you can decide which aspect of
the data is made available. This class is one of those which have
several different "kinds" of data. 

\begin{verbatim}
  packet->iValue(channel) 
\end{verbatim}
returns the TDC value of the channel; 0 if the channel number is out of range

In addition, there is 
\begin{verbatim}
  packet->iValue(channel,"TRIGBIT")  trigger bits
  packet->iValue(module,"MODNR")     module number of the module
  packet->iValue(module,"EVTNR")     event nr recorded by that module 
  packet->iValue(module,"CLOCK")     clock value recorded by that module 
  packet->iValue(0,"MODULECOUNT")    how many modules does this packet contain
  packet->iValue(0,"CHANNELCOUNT")   how many channels does this packet contain

\end{verbatim}
*/


  int    iValue(const int channel);
  int    iValue(const int channel,const char *what);
  void  dump ( OSTREAM& ) ;


protected:
  int *decode (int *);

  int  n_modules;
  int  expected_length;


};

#endif /* __PACKET_RPC_DCM0_H__ */



