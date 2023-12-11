#ifndef __PACKET_ERT_LL1_H__
#define __PACKET_ERT_LL1_H__
#include <packet_w124.h>
#include <string.h>
typedef struct{

  unsigned short header;            // Data packet header (std trans. card)     
  unsigned long  FiberErr;          // Sync/Data error bits for 8 fibers 
  unsigned long  AcceptCtr;         // 32-bit accept counter
  short PreLut2;                    // output to GL1 (8 bits)
  short FourByFourA;                // Algorithm bits (from ALG chip) 
  short FourByFourB;
  short FourByFourC;
  short TwoByTwo;
  short electron;
  short electron_alg_ver;           // Electron algorithm version bits
  short two_electron;
  short RxNP_S;
  short RxNP_N;
  short ModeBits;                   // GTM mode bits 9 bits
  short ChipVer;                    // FPGA code version 0-255
  short Scaler[2];                  // sync counters
} ERT_LL1_BOARD;
/**
   This is the packet which deals with data in ERT\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
  class WINDOWSEXPORT Packet_ert_ll1 : public Packet_w4 {
#else
  class  Packet_ert_ll1 : public Packet_w4 {
#endif

public:
  Packet_ert_ll1(PACKET_ptr);
  ~Packet_ert_ll1(){if(ertll1) delete ertll1;}
  virtual int  iValue(const int channel, const char *what);
  virtual int  iValue(const int channel, const int what);
  virtual int  fillIntArray(int destination[],    // the data go here 
			       const int length,      // space we have in destination
			       int * nw,              // words actually used
			       const char * what); // type of data

  virtual void dump ( OSTREAM& );

protected:
  enum ERT_Types {HEADER, FIBERERR, PRELUT2, MODEBITS, CHIPVER, SCALER,
                  FOURBYFOURA, FOURBYFOURB, FOURBYFOURC, TWOBYTWO, ELECTRON, E_ALGVER, TWOELECTRON, ACCEPT, RXNP_N, RXNP_S};

  virtual int *decode (int *);
  virtual void demangle();
  ERT_LL1_BOARD* ertll1;

};

#endif /* __PACKET_ERT_LL1_H__ */
