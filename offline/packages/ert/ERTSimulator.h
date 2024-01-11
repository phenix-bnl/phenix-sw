#ifndef __ERTSimulator_H__
#define __ERTSimulator_H__

#include "phool.h"

#include "TString.h"

class PHCompositeNode;
class TRandom;
class PdbErtSMEff;

#define N_CRKTT  256   //Number of CRK4x5 Trigger Tile
#define N_EMCTT  172   //Number of EMC2x2 Trigger Tile
#define N_MODULE 6192  //Number of EMC Module
#define N_ARM 2          // number of arms  1: east arm,   0: west arm
#define N_SECTOR 4       // number of EMCal/RICH sector in each arm
#define N_SIDE 2         // North: 1,   south: 0
#define N_SM 32       // number of SM in a sector for RICH/EMCal
#define N_ROC 20         //number of ROCs ,  from 0 - 19
#define N_WORD 6         // number of words in a ROC   from 0 - 5
#define N_BIT 16         // number of bits in a word
#define WESTPACKETID 14200 // packet ID of west arm
#define EASTPACKETID 14201 // packet ID of east arm



class ERTSimulator
{
  private:
   TRandom   *rdm;
   TString NodeName;

   bool  isGaussNoise;
   bool  isContNoise;
   int   runNumber;
   int   bitMask[N_BIT]; //__ 0/1 mask to see if a bit is 0/1
   int   wordEMC4x4ACoord[N_SECTOR][N_SIDE][2]; //for 4x4A, 
                                                //0: ROC(0-19), 1: word(0-5)
   int   wordEMC4x4BCoord[N_SECTOR][N_SIDE][2]; //for 4x4B, 
   int   wordEMC4x4CCoord[N_SECTOR][N_SIDE][2]; //for 4x4C, 
   int   wordEMC2x2_Coord[N_SECTOR][N_SIDE][2]; //for 2x2, 
   int   wordRICH4x5Coord[N_SECTOR][N_SIDE][2]; //for 4x5, 

   int*  SMCoordModeAPBGL;  //.. bit position of a SM in a word
   int*  SMCoordModeBPBGL;
   int*  SMCoordModeAPBSC;
   int*  SMCoordModeBPBSC;
   int*  SMCoordModeARICH;
   int*  SMCoordModeBRICH;

   int*  SMBitPosPBSC;
   int*  SMBitPosPBGL;
   int*  SMBitPosRICH;

   bool  IsTileHit[N_EMCTT];
   bool  IsCrkTileHit[N_CRKTT];
   float TileMax2x2E[N_EMCTT]; //.. max 2x2 energy in a trigger tile
   float TileMax4x4E[N_EMCTT]; //.. max 4x4 ....
   

   float EMCModuleDepositE[N_MODULE];  //Deposit Energy in a Module[moduleid]
   float CrkTriggerTileNPE[N_CRKTT]; //Number of Photoelectron in Trigger Tile
   bool  TriggerMatch[N_EMCTT][N_CRKTT];
   float emcthreshold;     // Common EMCal Thresshold for all modules
   float crkthreshold;
  
   int   ModulesInEMCTile[36];
   int   moduleid;
   float sumA16;
   int   neighbours[4];
   int   EMC_Mask[2][4][32][4]; //..arm/sect/sm/triggerMode


   //Constant for all over (arm, sector)  
   float noise2x2[N_ARM][N_SECTOR];
   float noise4x4[N_ARM][N_SECTOR];

   //Sigma value of Gaussian Noise  
   float noise_4x4Sigma[N_ARM][N_SECTOR]; 
   float noise_2x2Sigma[N_ARM][N_SECTOR]; 
   float noise_crkSigma[N_ARM][N_SECTOR];

   //Threshold setting
   float emc4x4Athres[N_ARM][N_SECTOR];
   float emc4x4Bthres[N_ARM][N_SECTOR];
   float emc4x4Cthres[N_ARM][N_SECTOR];
   float emc2x2_thres[N_ARM][N_SECTOR];
   float crk4x5_thres[N_ARM][N_SECTOR];

   float gain[2][4][48][96]; //gaussian gain variation
   float gain_additional;    //additional gain varition on top of gain[][][];

   bool  bitEMC2x2Tile[N_EMCTT];
   bool  bitCRK4x5Tile[2][4][32];

   //-----------------------------------------
   // arm, roc, word, value 
   long PacketData[N_ARM][N_ROC][N_WORD];

   //--------------------------------------------------------------------
   //  arm, sctor, side(North or South), hit/no hit for half sector level
   int Bit4x4AHalfSectEMC[N_ARM][N_SECTOR][N_SIDE];
   int Bit4x4BHalfSectEMC[N_ARM][N_SECTOR][N_SIDE];
   int Bit4x4CHalfSectEMC[N_ARM][N_SECTOR][N_SIDE];
   int Bit2x2HalfSectEMC[N_ARM][N_SECTOR][N_SIDE];
   int Bit4x5HalfSectRICH[N_ARM][N_SECTOR][N_SIDE];

   //--------------------------------------------------------------------
   // arm, sector, sm, hit/no hit
   int Bit4x4ASMEMC[N_ARM][N_SECTOR][N_SM];
   int Bit4x4BSMEMC[N_ARM][N_SECTOR][N_SM];
   int Bit4x4CSMEMC[N_ARM][N_SECTOR][N_SM];
   int Bit2x2SMEMC[N_ARM][N_SECTOR][N_SM];
   int Bit4x5SMRICH[N_ARM][N_SECTOR][N_SM];
   
   
   int GetBit4x4ASMEMC(int arm, int sector, int sm)
       { return Bit4x4ASMEMC[arm][sector][sm]; }
   int GetBit4x4BSMEMC(int arm, int sector, int sm)
       { return Bit4x4BSMEMC[arm][sector][sm]; }
   int GetBit4x4CSMEMC(int arm, int sector, int sm)
       { return Bit4x4CSMEMC[arm][sector][sm]; }

   int GetBit2x2SMEMC(int arm, int sector, int sm)
       { return Bit2x2SMEMC[arm][sector][sm]; }
   int GetBit4x5SMRICH(int arm, int sector, int sm)
       { return Bit4x5SMRICH[arm][sector][sm]; }

   //--------------------------------------------------------------------
   bool EMC4x4A[N_ARM][N_SECTOR][N_SM];
   bool EMC4x4B[N_ARM][N_SECTOR][N_SM];
   bool EMC4x4C[N_ARM][N_SECTOR][N_SM];
   bool EMC_2x2[N_ARM][N_SECTOR][N_SM];
   bool RICH4x5[N_ARM][N_SECTOR][N_SM];
   
   void Reset();
   void ClearforCRK();
   void ClearforEMC();
   void FillRawBit(int arm,int sector,int side,int roc,int word,int bit,int* smcoord,int tileType);

   PdbErtSMEff *ertsmeff;

  public:
   ERTSimulator();
   virtual ~ERTSimulator();

   void SetRunNumber(int input=80312);
   void SetNodeName( const char *node_name = "ErtOut" );
   void FetchSMEff_fromfile();
   bool FetchSMEff(int run_number);
//   void FetchSMMask(int run_number);
   void FetchSMMask_fromfile();
   void FetchSector(int run_number);
   void AddPMT(int pmt,float npe);
   float GetEMCTTMaxSum_no_noise(int emctt,int emcopt);
   float GetCRKTTMaxSum(int crktt,int crkopt, int arm, int sect);
   void AddTower(int towerkey, float DepositE);
   void GetRocWordBitPacket(int arm,int sector,int trgType,int sm,int &roc,int &word,int &packet,int &bitEMC,int &bitRICH);
   void GetRocWord(int arm,int sector,int trgType,int sm,int& roc,int& word);
   int  GetPacket(int arm);
   void GetBitPosition(int arm,int sector,int sm,int &bitEMC,int &bitRICH);
   PHBoolean EventLoopforCRK(PHCompositeNode *topNode);
   PHBoolean EventLoopforEMC(PHCompositeNode *topNode);
   void Decode(PHCompositeNode *topNode);
   void SetPacketData(int packetID, int roc, int word, long value); 
   void Calculate();
   void DecisionMaking(PHCompositeNode *topNode);
   void DstStore(PHCompositeNode *topNode);

};
#endif /*__ERTSimulator_H__*/
