#ifndef __PACKET_BIG_LL1_H__
#define __PACKET_BIG_LL1_H__
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
  short diagLT[5][13];
  short CtlSqCsmcSel;               // cosmics/square hole select bit (control)
  short AlgSqCsmcSel[2];            // cosmics/square hole select bit (alg 2, 4)
  short int SqHole[4];              // square hole sums 
  short int SqHoleShal[4];          // first index is ctl chip, followed by alg chips
  int DecodeStatus;                 // 0 if OK, -1 if failure
} MUID_BIGLL1_BOARD;

typedef struct{

  unsigned short header;
  short scaler[2];                  // ESN scalers
  short accept;                     // CTL chip accept counter
  short mode1;                      // mode bits
  short mode5;
  short mode6;
  short mode7;
  short mode8;
  short ZDCA;                       // ZDCA hit
  short ZDCB;                       // ZDCB hit
  short AlgVer;                     // alg. version
  char Vertex;                      // vertex
  short SouthTDC_OK;                // TDC values in range
  short NorthTDC_OK; 
  short VTXA_OK;                    // VTX A,B OK?
  short VTXB_OK; 
  int DecodeStatus;                 // 0 if OK, -1 if failure
} ZDC_BIGLL1_BOARD; 


/**
   This is the packet which deals with data in NTCZDC\_LL1 format.
   It inherits from Packet\_w4 because the data are 32bit entities.
*/
#ifndef __CINT__
  class WINDOWSEXPORT Packet_big_ll1 : public Packet_w4 {
#else
  class  Packet_big_ll1 : public Packet_w4 {
#endif

public:
  Packet_big_ll1(PACKET_ptr);
  ~Packet_big_ll1();
  virtual int  iValue(const int channel, const char *what);
  virtual void dump ( OSTREAM& );

protected:

  virtual int *decode (int *); 
  int vert_muidlvl1_demangle(MUID_BIGLL1_BOARD *&muidll1, unsigned int *buf, int shortWord,OSTREAM &os);
  int horiz_muidlvl1_demangle(MUID_BIGLL1_BOARD *&muidll1, unsigned int *buf, int shortWord,OSTREAM &os);
  int zdclvl1_demangle(unsigned int *buf, int ShortWord,OSTREAM &os);
  void printOutMuid(MUID_BIGLL1_BOARD *muidll1, OSTREAM &os);
  void fillDataStructures ( OSTREAM &os);

  MUID_BIGLL1_BOARD* muidll1_nv;
  MUID_BIGLL1_BOARD* muidll1_nh;
  MUID_BIGLL1_BOARD* muidll1_sv;
  MUID_BIGLL1_BOARD* muidll1_sh;
  int fiberAssignHoriz[6][20];
  int fiberAssignVert[6][20];

  ZDC_BIGLL1_BOARD* zdclvl1; 

};

#endif /* __PACKET_BIG_LL1_H__ */
