#ifndef __PACKET_MUID_LL1_H__
#define __PACKET_MUID_LL1_H__
#include <packet_w124.h>
#include <string.h>
typedef struct{

  unsigned short header; 
  unsigned long  Road[5];           // road's hit bits
  short PartialSum[6];              // partial sum (0-63)
  unsigned long  DataErr[6];        // Data error bits for 20 fibers 
  unsigned long  SyncErr[6];        // Sync error bits for 20 fibers
  short Strip;                      // Strip value (0-255) 
  short OrientStrip;                // Horizontal or vertical strip
  short Lut2;                       // output to GL1 (16 bits), 4 bits from each projection board
  short Sum;                        // total deep sum (0-255)
  short SSum;                       // total shallow sum (0-255)
  short ModeBits;                   // GTM mode bits 9 bits
  short ChipType[6];                // Chip Type (0-3)
  short AlgChip[6];                 // Algorithm Chip # (1-5)
  short ChipVer[6];                 // FPGA code version 0-255
  short MUIDvert[6];                // Orientation. Vertical if bit is set, horizontal otherwise
  short Scaler[6];                  // sync counters
  unsigned short StripLut;          // Strip LUT (?)
  unsigned short MuonAlign;         // MUON Aligned 
  // Additional words for Run-4
  unsigned long ShallowRoad[5];     // shallow roads hit bits
  short ShallowPartialSum[6];       // shallow partial sum
  short UE_sum[2];                  // cosmic trigger sums
  short UW_sum[2];                  // first index is ctl chip, second is alg. chip    
  short LE_sum[2];
  short LW_sum[2];
  short cosmic_out;                 // cosmic trigger output bits (horizontal only)
  short diagLT[6][13];
  short CtlSqCsmcSel;               // cosmics/square hole select bit (control)
  short AlgSqCsmcSel[2];            // cosmics/square hole select bit (alg 2, 4)
  short int SqHole[4];              // square hole sums 
  short int SqHoleShal[4];          // first index is ctl chip, followed by alg chips
  int DecodeStatus;                 // 0 if OK, -1 if failure
} MUID_LL1_BOARD;
/**
   This is the packet which deals with data in MUID\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
  class WINDOWSEXPORT Packet_muid_ll1 : public Packet_w4 {
#else
  class  Packet_muid_ll1 : public Packet_w4 {
#endif

public:
  Packet_muid_ll1(PACKET_ptr);
  ~Packet_muid_ll1(){if(muidll1) delete muidll1;}
  virtual int  iValue(const int channel, const char *what);
  virtual int  iValue(const int channel, const int what);
  virtual int  fillIntArray(int destination[],    // the data go here 
			       const int length,      // space we have in destination
			       int * nw,              // words actually used
			       const char * what); // type of data

  virtual void dump ( OSTREAM& );

protected:
enum MuID_Types {HEADER, ROADS, PARTSUM, DATAERR, SYNCERR, STRIP, ORIENTSTRIP, LUT2, TOTALSUM, MODEBITS,
                 CHIPTYPE, ALGCHIP, CHIPVER, MUIDVERT, SCALER, STRIPLUT, ALIGN, DCODESTAT};
  virtual int *decode (int *);
  // abstract functoins. Must be defined in Packet_muid_hor_ll1 and Packet_muid_ver_ll1 !!! 
  virtual int demangle ()=0;
  MUID_LL1_BOARD* muidll1;
  int fiberAssign[6][20];
};

#endif /* __PACKET_MUID_LL1_H__ */
