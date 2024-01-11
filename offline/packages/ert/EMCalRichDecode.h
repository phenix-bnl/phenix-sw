//////////////////////////////////////////////////////////
//  EMCal/RICH Trigger bit decoding software.
//  See http://www.phenix.bnl.gov/WWW/p/draft/xiewei/EMCal-RICH-Trigger/emc_rich_decode.html,  for details
//  problems report to: xiewei@rcf.rhic.bnl.gov
//////////////////////////////////////////////////////////

#ifndef __EMCalRichDecode_h__
#define __EMCalRichDecode_h__

#include <phool.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>

#include <Event.h>

#include <ErtOut.h>

#include <cstdio>
#include <iostream>

#define NARM 2 		// number of arms  1: east arm,   0: west arm
#define NSECTOR 4	// number of EMCal/RICH sector in each arm
#define NSIDE 2         // North: 1,   south: 0

#define NSM 32       // number of SM in a sector for RICH/EMCal

#define NROC 20 	//number of ROCs ,  from 0 - 19
#define NWORD 6 	// number of words in a ROC   from 0 - 5
#define NBIT 16         // number of bits in a word

#define EASTPACKETID 14201 // packet ID of east arm
#define WESTPACKETID 14200 // packet ID of west arm

class EMCalRichDecode
{
  public:

    EMCalRichDecode(const int runnumber=40925);  

    virtual ~EMCalRichDecode(){}

    //----------------------------------------
    // read in packet data and calculate bit
    //
    void SetPacketData(int packetID, int roc, int word, long value);
    void Calculate();  //..  do all BLL calculation  and mapping
      
    void Reset();
    //----------------------------------------
    // get all trigger bit.  1: fired, 0: not fired.
    //
    int GetTriggerBitPi0Low() 		{ return BitPi0Low;}
    int GetTriggerBitPi0High() 		{ return BitPi0High;}
    int GetTriggerBitPHI() 		{ return BitPHI;}
    int GetTriggerBitSingleElectron() 	{ return BitSingleElectron;}
    int GetTriggerBitChargedHadron() 	{ return BitChargedHadron;}

    //----------------------------------------------
    //  get SM bit information 
    //
    int GetBit4x4ASMEMC(int arm, int sector, int sm)
		 { return Bit4x4ASMEMC[arm][sector][sm]; }

    int GetBit4x4BSMEMC(int arm, int sector, int sm)
		 { return Bit4x4BSMEMC[arm][sector][sm]; }

    int GetBit4x4CSMEMC(int arm, int sector, int sm)
		 { return Bit4x4CSMEMC[arm][sector][sm]; }

    int GetBit2x2SMEMC(int arm, int sector, int sm)
		 { return Bit2x2SMEMC[arm][sector][sm]; }

    int GetBit4x5SMRICH(int arm, int sector, int sm)
		 { return Bit4x5SMRICH[arm][sector][sm];}


    //------------------------------------------------------
    //  Get Raw bits information. half sector trigger tile 
    //
    bool GetRawBitPi0Low(int& arm, int& sector, int& side);

    bool GetRawBitPi0High(int& arm, int& sector, int& side);

    bool GetRawBitPHI(int& arm1, int& sector1, int& side1, 
		      int& arm2, int& sector2, int& side2);

    bool GetRawBitSingleElectron(int& arm, int& sector, int& side);

    bool GetRawBitChargedHadron(int& arm, int& sector, int& side);

    //------------------------------------------------------
    // input:   arm, sector, sm, triggertype
    // output:  ROC, word, bits, packets 
    //

    void GetRocWordBitPacket(int arm, int sector, int trgType, int sm, int& roc, int& word, int& packet, int& bitEMC, int& bitRICH);



    int Decode(PHCompositeNode *root, int simulationFlag);

    void DstStore(PHCompositeNode *root);

    int GetRichBitPos(int arm, int sect, int side, int sm); 

  private:
   
    int bitMask[16]; //__ 0/1 mask to see if a bit is 0/1
    int word4x4ACoord[NSECTOR][NSIDE][2];  // for 4x4A, 0: ROC(0-19), 
				  	   //           1: word(0-5)
    int word4x4BCoord[NSECTOR][NSIDE][2];  // 4x4B
    int word4x4CCoord[NSECTOR][NSIDE][2];  // 4x4C
    int word2x2Coord[NSECTOR][NSIDE][2];   // 2x2
    int word4x5CoordRICH[NSECTOR][NSIDE][2];   // 2x2

    int SMCoordModeAPBGL[NBIT];  //.. bit position of a SM in a word
    int SMCoordModeBPBGL[NBIT];
    int SMCoordModeAPBSC[NBIT];
    int SMCoordModeBPBSC[NBIT];
    int SMCoordModeARICH[NBIT];
    int SMCoordModeBRICH[NBIT];

    int SMBitPosPBSC[NSM];
    int SMBitPosPBGL[NSM];
    int SMBitPosRICH[NSM];

  //---------------------------------------
  //  all data in the two trigger packets 
  //
    long PacketData[NARM][NROC][NWORD];

  //---------------------------------------------
  //  bits mode for Half sector level trigger tile 
  //
    int Bit4x4AHalfSectEMC[NARM][NSECTOR][NSIDE];
    int Bit4x4BHalfSectEMC[NARM][NSECTOR][NSIDE];
    int Bit4x4CHalfSectEMC[NARM][NSECTOR][NSIDE];
    int Bit2x2HalfSectEMC[NARM][NSECTOR][NSIDE];
    int Bit4x5HalfSectRICH[NARM][NSECTOR][NSIDE];
     
  //------------------------------------------------
  //  bits mode for SM level trigger tile
  //
    int Bit4x4ASMEMC[NARM][NSECTOR][NSM];
    int Bit4x4BSMEMC[NARM][NSECTOR][NSM];
    int Bit4x4CSMEMC[NARM][NSECTOR][NSM];
    int Bit2x2SMEMC[NARM][NSECTOR][NSM];
    int Bit4x5SMRICH[NARM][NSECTOR][NSM];

  //-----------------------------------------
  //  Blue Logic Trigger bits
  //
    int BitPi0Low;
    int BitPi0High;
    int BitPHI;
    int BitSingleElectron;
    int BitChargedHadron;

  //-----------------------------------------------------
  //  Raw bit information. If there're multiple input, 
  //  this is random one.
  //   
    int arm4x4A, sector4x4A, side4x4A;
    int arm4x4B, sector4x4B, side4x4B;
    int arm4x4C, sector4x4C, side4x4C;
    int arm2x2,  sector2x2,  side2x2;
    int earm, esector, eside;
    int phiarm1, phisector1, phiside1, phiarm2, phisector2, phiside2;

  //------------------------------
  //  private functions 

   //__  Fill tile/trigger bits information ...
    void FillRawBit(int arm, int sector, int side, int roc, int word, int bit, int* smcoord, int tileType);
    void FillPHITriggerBit();
    void FillElectronTriggerBit();

  //--------------------------------------
  // get packet, bit position, roc, word
  //
    int  GetPacket(int arm);

    void GetBitPosition(int arm, int sector, int sm, int& bitEMC, int& bitRICH);

    void GetRocWord(int arm, int sector, int trgType, int sm, int& roc, int& word);

};

#endif /*  _EMCalRichDecode_h_ */

