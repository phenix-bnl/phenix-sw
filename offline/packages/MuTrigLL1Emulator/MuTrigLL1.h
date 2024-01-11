
#ifndef __MuTrigLl1_h__
#define __MuTrigLl1_h__

#include <string>
#include <vector>

class Event;
class Packet; 
class MutrgUnpack; 
class MutrgDecode; 
class MutrgHitArray; 
class MutrgHeaderArray; 

class MuTrigLL1{

  protected:

  enum { MAX_ARM=2};
  enum { MAX_OCT=8};
  enum { MAX_STATION=3}; 
  enum { SOUTH=0, NORTH=1};
  enum { HORIZONTAL=0, VERTICAL=1};
  enum { ARM0_ST0_STRIPS = 96 }; 
  enum { ARM0_ST1_STRIPS = 160 }; 
  enum { ARM0_ST2_STRIPS = 240 }; 
  enum { ARM1_ST0_STRIPS = 96 }; 
  enum { ARM1_ST1_STRIPS = 192 }; 
  enum { ARM1_ST2_STRIPS = 320 }; 
  enum { RPC3_NUM_STRIPS = 128};  
  enum { RPC1_NUM_STRIPS = 128};  

  MutrgUnpack *mutrg_unpack; 
  MutrgDecode *mutrg_decode; 
  MutrgHitArray *mutrg_hits;
  MutrgHeaderArray *mutrg_headers;

  // Arrays for MRG board hits by station
  short int Station00[MAX_OCT][ARM0_ST0_STRIPS]; 
  short int Station01[MAX_OCT][ARM0_ST1_STRIPS]; 
  short int Station02[MAX_OCT][ARM0_ST2_STRIPS]; 
  short int Station10[MAX_OCT][ARM1_ST0_STRIPS]; 
  short int Station11[MAX_OCT][ARM1_ST1_STRIPS]; 
  short int Station12[MAX_OCT][ARM1_ST2_STRIPS]; 

  // Arrays for clustered hits
  short int Station00C[MAX_OCT][ARM0_ST0_STRIPS]; 
  short int Station01C[MAX_OCT][ARM0_ST1_STRIPS]; 
  short int Station02C[MAX_OCT][ARM0_ST2_STRIPS]; 
  short int Station10C[MAX_OCT][ARM1_ST0_STRIPS]; 
  short int Station11C[MAX_OCT][ARM1_ST1_STRIPS]; 
  short int Station12C[MAX_OCT][ARM1_ST2_STRIPS]; 

  // ANTIMASK arrays for clustered hits
  short int Station00C_AM[MAX_OCT][ARM0_ST0_STRIPS]; 
  short int Station01C_AM[MAX_OCT][ARM0_ST1_STRIPS]; 
  short int Station02C_AM[MAX_OCT][ARM0_ST2_STRIPS]; 
  short int Station10C_AM[MAX_OCT][ARM1_ST0_STRIPS]; 
  short int Station11C_AM[MAX_OCT][ARM1_ST1_STRIPS]; 
  short int Station12C_AM[MAX_OCT][ARM1_ST2_STRIPS]; 

  // Arrays for RPC3 hits by region (A,B,C)
  short int RPC3A[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3B[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3C[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3A1[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3B1[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3C1[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3A0[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3B0[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 
  short int RPC3C0[MAX_ARM][MAX_OCT][RPC3_NUM_STRIPS]; 

  // Arrays for RPC1 hits by region (A,B)
  short int RPC1A[MAX_ARM][MAX_OCT][RPC1_NUM_STRIPS]; 
  short int RPC1B[MAX_ARM][MAX_OCT][RPC1_NUM_STRIPS]; 
  short int RPC1A1[MAX_ARM][MAX_OCT][RPC1_NUM_STRIPS]; 
  short int RPC1B1[MAX_ARM][MAX_OCT][RPC1_NUM_STRIPS]; 
  short int RPC1A0[MAX_ARM][MAX_OCT][RPC1_NUM_STRIPS]; 
  short int RPC1B0[MAX_ARM][MAX_OCT][RPC1_NUM_STRIPS]; 

  // trigger arrays

  unsigned int mutr_trigger_sg0[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_trigger_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_trigger_sg2[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_trigger_sg3[MAX_ARM][MAX_OCT]; 
  
  unsigned int mutr_rpc_trigger_A_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc_trigger_B_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc_trigger_C_sg1[MAX_ARM][MAX_OCT]; 

  unsigned int mutr_rpc_trigger_A_sg3[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc_trigger_B_sg3[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc_trigger_C_sg3[MAX_ARM][MAX_OCT]; 

  unsigned int mutr_rpc1_trigger_A_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc1_trigger_B_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc1_trigger_C_sg1[MAX_ARM][MAX_OCT]; 

  unsigned int mutr_rpc_only_trigger_A_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc_only_trigger_B_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc_only_trigger_C_sg1[MAX_ARM][MAX_OCT]; 

  unsigned int mutr_rpc1A_trigger_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc1B_trigger_sg1[MAX_ARM][MAX_OCT]; 
  unsigned int mutr_rpc1C_trigger_sg1[MAX_ARM][MAX_OCT]; 

  // utility functions

  int unpackRPCHits(int fThisPacket); 
  int getRPCCh( int fPacket, int fGlobalChannel, int &fOctant, int &fHalfOct, int &fRadSeg, int &fStrip);
  void ClusterMutr(short int *Station, short int *Cluster, int data_size); 

  // RPC packet pointer
  Packet* _packet;
  
  // RPC TDC cuts
  int RPC_TDCmin; 
  int RPC_TDCmax; 
  int RPC1_TDCmin; 
  int RPC1_TDCmax;
 
  // MRG BCLK window
  int MRGBCLK_N; 
  int MRGBCLK_S; 

  // Data source
  bool simData; 

  // RPC operating mode
  int RPCMode; 

  // flag for dependent trigger calculations
  bool RPC13SG1_done; 

  public:

  // constructor
  MuTrigLL1();
  
  // destructor
  virtual ~MuTrigLL1(); 

  // get data from MRG event data
  int getDataFromMRGPackets(Event*);
  int getDataFromRPCPackets(Event*);

  // process input to trigger decision
  int calculateMuTr(); 

  int calculateMuTr_SG0(); 
  int calculateMuTr_SG1(); 
  int calculateMuTr_SG2(); 
  int calculateMuTr_SG3(); 

  int calculateMuTrRPC(); 
  int calculateMuTrRPC_SG1(); 
  int calculateMuTrRPC_SG3(); 
  int calculateMuTrRPC1_SG1();  
  int calculateMuTrRPC1C_SG1();  
  int calculateMuTrRPC_ONLY(); 

  // trigger result accessors
  bool getMuTrTrigSG0(int arm); 
  bool getMuTrTrigSG1(int arm); 
  bool getMuTrTrigSG2(int arm); 
  bool getMuTrTrigSG3(int arm);  
  bool getMuTrTrigSG0(int arm, int octant); 
  bool getMuTrTrigSG1(int arm, int octant); 
  bool getMuTrTrigSG2(int arm, int octant); 
  bool getMuTrTrigSG3(int arm, int octant); 

  bool getMuTrRPCTrigA(int arm); 
  bool getMuTrRPCTrigB(int arm); 
  bool getMuTrRPCTrigC(int arm); 
  bool getMuTrRPCTrigA(int arm, int octant); 
  bool getMuTrRPCTrigB(int arm, int octant); 
  bool getMuTrRPCTrigC(int arm, int octant); 

  bool getMuTrRPCTrigA_SG3(int arm); 
  bool getMuTrRPCTrigB_SG3(int arm); 
  bool getMuTrRPCTrigC_SG3(int arm); 
  bool getMuTrRPCTrigA_SG3(int arm, int octant); 
  bool getMuTrRPCTrigB_SG3(int arm, int octant); 
  bool getMuTrRPCTrigC_SG3(int arm, int octant); 

  bool getMuTrRPC1TrigA(int arm); 
  bool getMuTrRPC1TrigB(int arm); 
  bool getMuTrRPC1TrigC(int arm); 
  bool getMuTrRPC1CTrig(int arm); 
  bool getMuTrRPC1TrigA(int arm, int octant); 
  bool getMuTrRPC1TrigB(int arm, int octant); 
  bool getMuTrRPC1TrigC(int arm, int octant); 
  bool getMuTrRPC1CTrig(int arm, int octant); 

  bool getMuTrRPCONLYTrigA(int arm); 
  bool getMuTrRPCONLYTrigB(int arm); 
  bool getMuTrRPCONLYTrigC(int arm); 
  bool getMuTrRPCONLYTrigA(int arm, int octant); 
  bool getMuTrRPCONLYTrigB(int arm, int octant); 
  bool getMuTrRPCONLYTrigC(int arm, int octant); 

  // hit strips counts

  int getRPC1Hits(int arm, int octant);
  int getRPC1AHits(int arm, int octant);
  int getRPC1BHits(int arm, int octant);
  int getRPC3AHits(int arm, int octant);
  int getRPC3BHits(int arm, int octant);
  int getRPC3CHits(int arm, int octant);
  int getMuTrHits(int arm, int station, int octant); 

  // Set/get MRG BCLK window
  void SetMRGBCLK(int nval){MRGBCLK_N=nval;MRGBCLK_S=nval;}; 
  void SetMRGBCLK_N(int nval){MRGBCLK_N=nval;}; 
  void SetMRGBCLK_S(int nval){MRGBCLK_S=nval;}; 
  int GetMRGBCLK(int arm){float MRGBCLK; if(arm==SOUTH) MRGBCLK = MRGBCLK_S; else MRGBCLK = MRGBCLK_N; return MRGBCLK;}; 

  // Set RPC time window
  void SetRPCTimeWindow(int min, int max){ RPC_TDCmin = min; RPC_TDCmax = max; };
  void SetRPC1TimeWindow(int min, int max){ RPC1_TDCmin = min; RPC1_TDCmax = max; };

  // Set PRDF source
  void SetSourceSimulation(){simData = true;}; 
  void SetSourceReal(){simData = false;}; 

  enum RPCMODE {USE_TRIG_BIT,USE_TDC,USE_TDC_CHECK_TRIG}; 

  void SetRPCMode(int in){RPCMode = in;}; 
  
};

// Autogenerate function definitions:

void func_mutr_alg_arm0_octant0_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant1_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant2_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant3_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant4_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant5_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant6_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant7_sg0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant0_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant1_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant2_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant3_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant4_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant5_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant6_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant7_sg0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant0_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant1_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant2_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant3_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant4_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant5_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant6_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant7_sg1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant0_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant1_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant2_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant3_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant4_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant5_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant6_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant7_sg1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant0_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant1_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant2_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant3_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant4_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant5_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant6_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant7_sg2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant0_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant1_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant2_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant3_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant4_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant5_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant6_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant7_sg2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant0_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant1_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant2_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant3_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant4_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant5_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant6_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant7_sg3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant0_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant1_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant2_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant3_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant4_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant5_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant6_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant7_sg3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant0_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant1_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant2_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant3_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant4_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant5_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant6_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm0_octant7_sg5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant0_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant1_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant2_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant3_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant4_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant5_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant6_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 

void func_mutr_alg_arm1_octant7_sg5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       unsigned int trig[2][8]); 




void func_mutr_rpc_alg_arm0_octant0_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant6_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant7_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 




void func_mutr_rpc_alg_arm1_octant0_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant6_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant7_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant0_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant1_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant2_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant3_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant4_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant5_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant6_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm0_octant7_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][160],
	       short int Station12[8][240],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant0_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant1_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant2_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant3_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant4_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant5_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant6_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 


void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_0(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_1(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_2(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_3(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_4(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_5(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_6(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_7(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_8(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_9(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_10(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

void func_mutr_rpc_alg_arm1_octant7_rpc1_sg1_11(
	       short int Station10[8][96],
	       short int Station11[8][192],
	       short int Station12[8][320],
	       short int RPC3A[2][8][128],
	       short int RPC3B[2][8][128],
	       short int RPC3C[2][8][128],
	       short int RPC1A[2][8][128],
	       short int RPC1B[2][8][128],
	       unsigned int trigA[2][8], 
	       unsigned int trigB[2][8], 
	       unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant0_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant1_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant2_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant3_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant4_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant5_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant6_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm0_octant7_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][160], 
	 short int Station12[8][240], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant0_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant1_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant2_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant3_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant4_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant5_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant6_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_0(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_1(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_2(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_3(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_4(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_5(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_6(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_7(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_8(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_9(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_10(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_11(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_12(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_13(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_14(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_15(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_16(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_17(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_18(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_19(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_20(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_21(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_22(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_23(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_24(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

  void func_mutr_rpc_alg_arm1_octant7_rpc1_sg3_25(
	 short int Station10[8][96], 
	 short int Station11[8][192], 
	 short int Station12[8][320], 
	 short int RPC3A[2][8][128], 
	 short int RPC3B[2][8][128], 
	 short int RPC3C[2][8][128], 
	 short int RPC1A[2][8][128], 
	 short int RPC1B[2][8][128], 
	 unsigned int trigA[2][8],  
	 unsigned int trigB[2][8],  
	 unsigned int trigC[2][8]); 

 


#endif
