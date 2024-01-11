
#include <PHCompositeNode.h>
#include <TMuiHitMapO.h>
#include <recoConsts.h>

#include <iostream>
#include <cstdlib>
#include <sstream>

#include <Event.h>
#include <packet.h>

#include <vector>
#include <mMuiInitModule.h>

#include "MuIDLl1.h"

//_______________________________________________________________
// forward declaration
void MuiMapInit_Trigger(hashVector <TMuiChannelId, int> &map);

using namespace std;

//________________________________________________________-
MuIDLl1::MuIDLl1(const char* CONFIG_path)
{

  char CONFIG_path_current[BUFSIZ];

  int FiberToRocLUT[6][16] = {
    {92,93,19,85,18,73, 0,10,30,23,24,27,34,44,68,65},
    {50,94,82,54,76,20,15,28,31, 1,32,26,29,69,67,60},
    {56,91,80,90,78,75,16, 6, 2,22,36,25,12,62,70,43},
    {63,51,81,52,83,88,72, 7,35, 4,38, 5,11,61,48,45},
    {49,87,17,53,84,89,71, 8,42,21,40,33,14,57,66,37},
    {59,86,77,55,79,95,74, 9,47, 3,46,41,13,58,64,39},
  };
  
  // Generate the reverse LUT

  for(int i=0; i<6; i++){
    for(int j=0; j<16; j++){
      RocToFiber[FiberToRocLUT[i][j]].frame = i;
      RocToFiber[FiberToRocLUT[i][j]].bit = j;
    }
  }
   
  strcpy(CONFIG_path_current,CONFIG_path);
  strcpy(filename,"NO_FILENAME");  
  strcat(CONFIG_path_current,filename);
  success=getFiberBitMasks(CONFIG_path_current);

  ROCNoiseCut = false; 

  NewMuidPanelAlg = false; 
  RequireFourGaps = false; 
  OneHadronTrigger = true; 

  mutooInitialized = false; 

  tmuihitmap_name = "TMuiHitMapO";

}

//__________________________________________________________________
int MuIDLl1::getDataFromMuidPacket(Event *e)
{

  Packet* mui_packet;
  const int packet_id_offset = 12001;
  int packet_id;
  int status = 0;
 
  // variables for zero-suppressed format
  short iroc, iword;
  short roc_ok[MAX_ROC], beam_ok[MAX_ROC];
  long usr_word[MAX_FEM][USR_WORDS];

  for ( short iarm = 0; iarm < MAX_ARM; iarm++ ) 
    {
      for ( short iorient = 0; iorient < MAX_ORIENTATION; iorient++)
        {
          short ifem = iarm * MAX_ORIENTATION + iorient;

          packet_id = packet_id_offset + ifem;
          mui_packet = e->getPacket( packet_id );

          // word initialize
          for ( int i = 0; i < WORD_PER_ROC; i++ )
            {
              for ( int j = 0; j < MAX_ROC; j++) ROCData[iarm][iorient][j][i] = 0;
            }

          // no MuID packet found
          if ( ! mui_packet )
            {
	      status = -1; 
            }
          else
            {

	      //cout << packet_id << endl; 

              // This is for variable length(zero-suppressed) format

              for ( int iusr_word = 0; iusr_word < USR_WORDS; iusr_word++ )
                {
                  usr_word[ ifem ][ iusr_word ] =
                    mui_packet->iValue(iusr_word, "USERWORD" );
                }
              for (iroc = 0; iroc < MAX_ROC; iroc++)
                {
                  if ( iroc < 16 )
                    {
                      roc_ok[ iroc ] = short((usr_word[ifem][0] & (1 << iroc)) >> iroc);
                    }
                  else
                    {
                      roc_ok[iroc] = short((usr_word[ifem][1] & (1 << (iroc - 16)))
                                           >> (iroc - 16) );
                    }
                  if ( iroc < 12 )
                    {
                      beam_ok[iroc] = short((usr_word[ifem][1] & (1 << (iroc + 4)))
                                            >> (iroc + 4) );
                    }
                  else
                    {
                      beam_ok[iroc] = short((usr_word[ifem][2] & (1 << (iroc - 12)))
                                            >> (iroc - 12) );
                    }

                  if ( (roc_ok[iroc] == 0) || (beam_ok[iroc] == 0) ){
                    // ROC status is not good
		    //cout << "MuIDLl1: WARNING - ROC " << iroc << " status is bad " << endl; 
		    status = -1; 
                  }
              
                  for (iword = 0; iword < WORD_PER_ROC; iword++){

	            // We've got the ROC data the MuID LL1 needs...
                    ROCData[iarm][iorient][iroc][iword] = mui_packet->iValue(iword, iroc);

		    //cout << " Got data for: iarm = " << iarm << " iorient = " << iorient 
		    //     << " iroc = " << iroc << " iword = " << iword << endl; 

                  } // for iword = 0...

                } // for roc = 0...

            } // if mui_packet

          delete mui_packet;

        } // for orient...

    } // for arm ...

  return status;
}

//_____________________________________________________________________
bool MuIDLl1::getDataFromMutoo(PHCompositeNode *topNode)
{
  
  // Get the muon hit data from MUTOO, assigns channels and calculates 
  // hit symsets

  bool status = true;

  static bool firstCall = true; 
  static hashVector<TMuiChannelId, int> trigger_map(TMuiChannelId::kTwoPacksMaxTotal, &TwoPackHash);

  // Get the MuID->LL1 channel mapping

  if(firstCall){
    cout << PHWHERE << "Initializing MuIDLl1 trigger maps." << endl; 
    MuiMapInit_Trigger(trigger_map);
    firstCall = false; 
  }

  // Get the MuID->LL1 channel mapping
  if( !mutooInitialized ) initializeMutoo( topNode );
  
  // switch between algorithms
  if(!NewMuidPanelAlg)
  {

    // Zero the symset vectors
    memset(SymsetVector, 0, sizeof(SymsetVector));
    memset(ShallowSymsetVector, 0, sizeof(ShallowSymsetVector));

    // Old procedure - all logical tubes at once

    // Clear the logical tubes
    memset(LT, 0, sizeof(LT));

    // Get the MUTOO hits, assign them to logical tubes
    TMuiHitMapO* muihit_map = TMutNode<TMuiHitMapO>::find_node( topNode, tmuihitmap_name );
    if (muihit_map) {
      TMuiHitMapO::const_iterator muihit_iter = muihit_map->range();
      while( TMuiHitMapO::const_pointer muihit_ptr = muihit_iter.next() )
	{

        int arm = muihit_ptr->get()->get_arm();
        int plane = muihit_ptr->get()->get_plane();
        int panel = muihit_ptr->get()->get_panel();
        int orientation = muihit_ptr->get()->get_orientation();
        int twopack = muihit_ptr->get()->get_twopack();
        //int index = muihit_ptr->get()->get_index();
 
        short logical = trigger_map[TMuiChannelId(arm, plane, panel, (EOrient_t) orientation, twopack)];
        LT[arm][orientation][plane][logical] = true; 

      }

      // Apply the bad fibers: SH2 and NH5
      // This is valid for Run-9 and Run-10
      // Should be updated in a more general way - JGL 2/5/2010
      
      // Removed - 5/14/2010 JGL

      /*
      // SH2 - plane 0, LT 58-74
      for(int i=58; i<=84; i++) LT[0][0][0][i] = true; 

      // NH5 - plane 1, LT 35-60
      for(int i=35; i<=60; i++) LT[1][0][1][i] = true; 
      */

      // process the symsets 

      for(int iarm = 0; iarm < MAX_ARM; iarm++){
        for(int iorient = 0; iorient < MAX_ORIENTATION; iorient++){
          for(int chip = 1; chip < 6; chip++){
	    numUsedROCBitsSet[iarm][iorient][chip-1] = 0; 
            processSymsets(iarm,iorient,chip);
          }
        }
      }

    }
    else{
      cout << PHWHERE << " Unable to find TMuiHitMapO in node tree!" << endl; 
      status = false; 
    }
    
  }
  else{

    // Zero the symset vectors
    memset(PanelSymsetVector, 0, sizeof(PanelSymsetVector));
    memset(PanelShallowSymsetVector, 0, sizeof(PanelShallowSymsetVector));
  
    // New Algorithm - process symsets by panel

    for(int panel=0; panel<PANEL_PER_GAP; panel++){
    
      // Clear the logical tubes

      for(int iarm = 0; iarm < MAX_ARM; iarm++){
        for(int iorient = 0; iorient < MAX_ORIENTATION; iorient++){
	  numUsedROCBitsSetPanel[iarm][iorient][panel] = 0; 
          for(int j=0;j<GAP_PER_ARM;j++){
            for(int k=0;k<MAX_SYMSETS;k++){
              LT[iarm][iorient][j][k] = false; 
            }
          }
        }
      }

      // Get the MUTOO hits, assign them to logical tubes
      TMuiHitMapO* muihit_map = TMutNode<TMuiHitMapO>::find_node( topNode, tmuihitmap_name );
      if (muihit_map) {
        TMuiHitMapO::const_iterator muihit_iter = muihit_map->range();
        while( TMuiHitMapO::const_pointer muihit_ptr = muihit_iter.next() )
        {

          int arm = muihit_ptr->get()->get_arm();
          int plane = muihit_ptr->get()->get_plane();
          int upanel = muihit_ptr->get()->get_panel();
          int orientation = muihit_ptr->get()->get_orientation();
          int twopack = muihit_ptr->get()->get_twopack();
          //int index = muihit_ptr->get()->get_index();
 
	  if(upanel==panel){
            short logical = trigger_map[TMuiChannelId(arm, plane, upanel, (EOrient_t) orientation, twopack)];
            LT[arm][orientation][plane][logical] = true;
	  }

        }

        // process the symsets 

	for(int iarm = 0; iarm < MAX_ARM; iarm++){
	  for(int iorient = 0; iorient < MAX_ORIENTATION; iorient++){
	      processSymsetsByPanel(iarm,iorient,panel);
	  }
	}

      }
      else{
        cout << PHWHERE << " Unable to find TMuiHitMapO in node tree!" << endl; 
        status = false; 
      }

    }

  }  

  return status; 



}

//___________________________________________________________________
void MuIDLl1::initializeMutoo( PHCompositeNode* topNode )  

{
  
  assert( !mutooInitialized );
  
  cout << "MuIDLl1::initializeMutoo - Initializing MuIDLl1 trigger maps." << endl; 
  PHTimeStamp time_stamp( recoConsts::instance()->get_TimeStamp() );

  // one should check whether the module has been initialized already or not.
  mMuiInitModule mMuiInitMod;
  mMuiInitMod.SetSearchTimeStamp(time_stamp);
  mMuiInitMod.event(topNode);
  
  mutooInitialized = true;
  
}
  
//___________________________________________________________________
int MuIDLl1::getFiberBitMasks(const char *filename)
{

  // Set up the fiber bit mask - a zero means the channel is masked OFF
  memset(fiberBitMask, 1, sizeof(fiberBitMask));

  // Bad fiber MuID north vertical
  //memset(fiberBitMask[1][0][5],0,WORD_PER_ROC*BIT_PER_WORD); 
  //memset(fiberBitMask[1][1][15],0,WORD_PER_ROC*BIT_PER_WORD); 

  return 1;
}

//___________________________________________________________________
int MuIDLl1::calculate(short SimSyncErr[2][2][5][20]){

  // Zero the fiber data arrays
  memset(fiberBitData, 0, sizeof(fiberBitData));
    
  // Map the ROC bits into fiber bits
  // (note that iroc==fiber)
  // Note that the fiber bit number mapping is complicated by the fact 
  // that in the FPGA code Harold mapped the fiber inputs to the MuID mapping
  // as <bit><frame>, not <frame><bit> as you would expect.
  memset(numROCBitsSet, 0, sizeof(numROCBitsSet));
  for(int iarm = 0; iarm < MAX_ARM; iarm++){
    for(int iorient = 0; iorient < MAX_ORIENTATION; iorient++){
      for(int iroc = 0; iroc < MAX_ROC; iroc++){

	flagBit[iarm][iorient][iroc][0] = 0; 
	flagBit[iarm][iorient][iroc][1] = 0; 

        for(int iword = 0; iword < WORD_PER_ROC; iword++){
          for(int bit = 0; bit < BIT_PER_WORD; bit++){
	    if( (ROCData[iarm][iorient][iroc][iword]&(0x1<<bit)) ){
	      int frame = RocToFiber[(iword*BIT_PER_WORD)+bit].frame;
	      int fbit = RocToFiber[(iword*BIT_PER_WORD)+bit].bit;
	      int dbit = frame + fbit*6;	      
	      if(fiberBitMask[iarm][iorient][iroc][dbit]!=0) fiberBitData[iarm][iorient][iroc][dbit] = 1;
	      numROCBitsSet[iarm][iorient][iroc]++;
	      
	      // Generate the FLAG bits (cosmics OR signals)

	      if(iorient==0){
		if( ((iword*BIT_PER_WORD)+bit)<=63 )  flagBit[iarm][iorient][iroc][0] = 1; 
		if( ((iword*BIT_PER_WORD)+bit)>=32 && ((iword*BIT_PER_WORD)+bit)<=95 ) flagBit[iarm][iorient][iroc][1] = 1;
	      }
	      else{
		if( ((iword*BIT_PER_WORD)+bit)<=47 ) flagBit[iarm][iorient][iroc][0] = 1; 
		if( ((iword*BIT_PER_WORD)+bit)>=48 && ((iword*BIT_PER_WORD)+bit)<=95 ) flagBit[iarm][iorient][iroc][1] = 1; 
	      }

	    }
          }
        }
      }
    }
  }

  // Clear the logical tubes
  memset(LT, 0, sizeof(LT));

  if(!NewMuidPanelAlg){

    // The old way - not panel oriented

    // The symsets are processed in groups according to the 
    // grouping into the algorithm chips. The logical tubes are
    // mapped for each chip, the symsets processed, and then
    // the next tube is mapped.  This insures that the symsets
    // are processed using only the logical tubes that are mapped by 
    // the FPGA code. 

    // Zero the symset vectors
    memset(SymsetVector, 0, sizeof(SymsetVector));
    memset(ShallowSymsetVector, 0, sizeof(ShallowSymsetVector));

    // South Horizontal Chip 1:

    mapMuidSouthHorizLI(1,SimSyncErr,1);
    processSymsets(SOUTH,HORIZONTAL,1);

    // South Horizontal Chip 2:

    mapMuidSouthHorizLI(2,SimSyncErr,1);
    processSymsets(SOUTH,HORIZONTAL,2);

    // South Horizontal Chip 3:

    mapMuidSouthHorizLI(3,SimSyncErr,1);
    processSymsets(SOUTH,HORIZONTAL,3);

    // South Horizontal Chip 4:

    mapMuidSouthHorizLI(4,SimSyncErr,1);
    processSymsets(SOUTH,HORIZONTAL,4);

    // South Horizontal Chip 5:

    mapMuidSouthHorizLI(5,SimSyncErr,1);
    processSymsets(SOUTH,HORIZONTAL,5);

    // South Vertical Chip 1:

    mapMuidSouthVertLI(1,SimSyncErr,1);
    processSymsets(SOUTH,VERTICAL,1);

    // South Vertical Chip 2:

    mapMuidSouthVertLI(2,SimSyncErr,1);
    processSymsets(SOUTH,VERTICAL,2);

    // South Vertical Chip 3:

    mapMuidSouthVertLI(3,SimSyncErr,1);
    processSymsets(SOUTH,VERTICAL,3);

    // South Vertical Chip 4:

    mapMuidSouthVertLI(4,SimSyncErr,1);
    processSymsets(SOUTH,VERTICAL,4);

    // South Vertical Chip 5:

    mapMuidSouthVertLI(5,SimSyncErr,1);
    processSymsets(SOUTH,VERTICAL,5);

    // NORTH ARM

    // North Horizontal Chip 1:

    mapMuidNorthHorizLI(1,SimSyncErr,1);
    processSymsets(NORTH,HORIZONTAL,1);

    // North Horizontal Chip 2:

    mapMuidNorthHorizLI(2,SimSyncErr,1);
    processSymsets(NORTH,HORIZONTAL,2);

    // North Horizontal Chip 3:

    mapMuidNorthHorizLI(3,SimSyncErr,1);
    processSymsets(NORTH,HORIZONTAL,3);

    // North Horizontal Chip 4:

    mapMuidNorthHorizLI(4,SimSyncErr,1);
    processSymsets(NORTH,HORIZONTAL,4);

    // North Horizontal Chip 5:

    mapMuidNorthHorizLI(5,SimSyncErr,1);
    processSymsets(NORTH,HORIZONTAL,5);

    // North Vertical Chip 1:

    mapMuidNorthVertLI(1,SimSyncErr,1);
    processSymsets(NORTH,VERTICAL,1);

    // North Vertical Chip 2:

    mapMuidNorthVertLI(2,SimSyncErr,1);
    processSymsets(NORTH,VERTICAL,2);

    // North Vertical Chip 3:

    mapMuidNorthVertLI(3,SimSyncErr,1);
    processSymsets(NORTH,VERTICAL,3);

    // North Vertical Chip 4:

    mapMuidNorthVertLI(4,SimSyncErr,1);
    processSymsets(NORTH,VERTICAL,4);

    // North Vertical Chip 5:

    mapMuidNorthVertLI(5,SimSyncErr,1);
    processSymsets(NORTH,VERTICAL,5);

    // COSMICS OUTPUT
    // Straightforward, based on specific fiber bits...

    unsigned int LE[MAX_ARM][MAX_ORIENTATION];
    unsigned int LW[MAX_ARM][MAX_ORIENTATION];
    unsigned int UE[MAX_ARM][MAX_ORIENTATION];
    unsigned int UW[MAX_ARM][MAX_ORIENTATION];

    memset(LE, 0, sizeof(LE));
    memset(LW, 0, sizeof(LW));
    memset(UE, 0, sizeof(UE));
    memset(UW, 0, sizeof(UW));

    //cout << endl << endl; 

    for(int iarm = 0; iarm < MAX_ARM; iarm++){
      for(int iorient = 0; iorient < MAX_ORIENTATION; iorient++){

	if(iorient==0){

	  //cout << "HORIZ:" << endl; 

	  // LOWER EAST 

	  LE[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][0][0]>0) || (SimSyncErr[iarm][iorient][1][0]>0))  || 
	      ((flagBit[iarm][iorient][1][0]>0) || (SimSyncErr[iarm][iorient][1][1]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][4][0]>0) || (SimSyncErr[iarm][iorient][1][4]>0))  || 
	      ((flagBit[iarm][iorient][5][0]>0) || (SimSyncErr[iarm][iorient][1][5]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][8][0]>0) || (SimSyncErr[iarm][iorient][1][8]>0))  || 
	      ((flagBit[iarm][iorient][9][0]>0) || (SimSyncErr[iarm][iorient][1][9]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][12][0]>0) || (SimSyncErr[iarm][iorient][1][12]>0))  || 
	      ((flagBit[iarm][iorient][13][0]>0) || (SimSyncErr[iarm][iorient][1][13]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][16][0]>0) || (SimSyncErr[iarm][iorient][1][16]>0))  || 
	      ((flagBit[iarm][iorient][17][0]>0) || (SimSyncErr[iarm][iorient][1][17]>0)) ) LE[iarm][iorient]++;

	  //cout << LE[iarm][iorient] << endl; 

	  // LOWER WEST 

	  LW[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][0][1]>0) || (SimSyncErr[iarm][iorient][1][0]>0))  || 
	      ((flagBit[iarm][iorient][1][1]>0) || (SimSyncErr[iarm][iorient][1][1]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][4][1]>0) || (SimSyncErr[iarm][iorient][1][4]>0))  || 
	      ((flagBit[iarm][iorient][5][1]>0) || (SimSyncErr[iarm][iorient][1][5]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][8][1]>0) || (SimSyncErr[iarm][iorient][1][8]>0))  || 
	      ((flagBit[iarm][iorient][9][1]>0) || (SimSyncErr[iarm][iorient][1][9]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][12][1]>0) || (SimSyncErr[iarm][iorient][1][12]>0))  || 
	      ((flagBit[iarm][iorient][13][1]>0) || (SimSyncErr[iarm][iorient][1][13]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][16][1]>0) || (SimSyncErr[iarm][iorient][1][16]>0))  || 
	      ((flagBit[iarm][iorient][17][1]>0) || (SimSyncErr[iarm][iorient][1][17]>0)) ) LW[iarm][iorient]++;

	  //cout << LW[iarm][iorient] << endl; 

	  // UPPER EAST

	  UE[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][2][0]>0) || (SimSyncErr[iarm][iorient][3][2]>0))  || 
	      ((flagBit[iarm][iorient][3][0]>0) || (SimSyncErr[iarm][iorient][3][3]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][6][0]>0) || (SimSyncErr[iarm][iorient][3][6]>0))  || 
	      ((flagBit[iarm][iorient][7][0]>0) || (SimSyncErr[iarm][iorient][3][7]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][10][0]>0) || (SimSyncErr[iarm][iorient][3][10]>0))  || 
	      ((flagBit[iarm][iorient][11][0]>0) || (SimSyncErr[iarm][iorient][3][11]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][14][0]>0) || (SimSyncErr[iarm][iorient][3][14]>0))  || 
	      ((flagBit[iarm][iorient][15][0]>0) || (SimSyncErr[iarm][iorient][3][15]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][18][0]>0) || (SimSyncErr[iarm][iorient][3][18]>0))  || 
	      ((flagBit[iarm][iorient][19][0]>0) || (SimSyncErr[iarm][iorient][3][19]>0)) ) UE[iarm][iorient]++;

	  //cout << UE[iarm][iorient] << endl; 

	  // UPPER WEST 

	  UW[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][2][1]>0) || (SimSyncErr[iarm][iorient][3][2]>0))  || 
	      ((flagBit[iarm][iorient][3][1]>0) || (SimSyncErr[iarm][iorient][3][3]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][6][1]>0) || (SimSyncErr[iarm][iorient][3][6]>0))  || 
	      ((flagBit[iarm][iorient][7][1]>0) || (SimSyncErr[iarm][iorient][3][7]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][10][1]>0) || (SimSyncErr[iarm][iorient][3][10]>0))  || 
	      ((flagBit[iarm][iorient][11][1]>0) || (SimSyncErr[iarm][iorient][3][11]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][14][1]>0) || (SimSyncErr[iarm][iorient][3][14]>0))  || 
	      ((flagBit[iarm][iorient][15][1]>0) || (SimSyncErr[iarm][iorient][3][15]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][18][1]>0) || (SimSyncErr[iarm][iorient][3][18]>0))  || 
	      ((flagBit[iarm][iorient][19][1]>0) || (SimSyncErr[iarm][iorient][3][19]>0)) ) UW[iarm][iorient]++;

	  //cout << UW[iarm][iorient] << endl; 

	}
	else{

	  //cout << "VERT:" << endl; 

	  // LOWER EAST 

	  LE[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][0][0]>0) || (SimSyncErr[iarm][iorient][1][0]>0))  || 
	      ((flagBit[iarm][iorient][1][0]>0) || (SimSyncErr[iarm][iorient][1][1]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][4][0]>0) || (SimSyncErr[iarm][iorient][1][4]>0))  || 
	      ((flagBit[iarm][iorient][5][0]>0) || (SimSyncErr[iarm][iorient][1][5]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][8][0]>0) || (SimSyncErr[iarm][iorient][1][8]>0))  || 
	      ((flagBit[iarm][iorient][9][0]>0) || (SimSyncErr[iarm][iorient][1][9]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][12][0]>0) || (SimSyncErr[iarm][iorient][1][12]>0))  || 
	      ((flagBit[iarm][iorient][13][0]>0) || (SimSyncErr[iarm][iorient][1][13]>0)) ) LE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][16][0]>0) || (SimSyncErr[iarm][iorient][1][16]>0))  || 
	      ((flagBit[iarm][iorient][17][0]>0) || (SimSyncErr[iarm][iorient][1][17]>0)) ) LE[iarm][iorient]++;

	  //cout << LE[iarm][iorient] << endl; 

	  // UPPER EAST 

	  UE[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][0][1]>0) || (SimSyncErr[iarm][iorient][1][0]>0))  || 
	      ((flagBit[iarm][iorient][1][1]>0) || (SimSyncErr[iarm][iorient][1][1]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][4][1]>0) || (SimSyncErr[iarm][iorient][1][4]>0))  || 
	      ((flagBit[iarm][iorient][5][1]>0) || (SimSyncErr[iarm][iorient][1][5]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][8][1]>0) || (SimSyncErr[iarm][iorient][1][8]>0))  || 
	      ((flagBit[iarm][iorient][9][1]>0) || (SimSyncErr[iarm][iorient][1][9]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][12][1]>0) || (SimSyncErr[iarm][iorient][1][12]>0))  || 
	      ((flagBit[iarm][iorient][13][1]>0) || (SimSyncErr[iarm][iorient][1][13]>0)) ) UE[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][16][1]>0) || (SimSyncErr[iarm][iorient][1][16]>0))  || 
	      ((flagBit[iarm][iorient][17][1]>0) || (SimSyncErr[iarm][iorient][1][17]>0)) ) UE[iarm][iorient]++;

	  //cout << UE[iarm][iorient] << endl; 

	  // LOWER WEST

	  LW[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][2][0]>0) || (SimSyncErr[iarm][iorient][3][2]>0))  || 
	      ((flagBit[iarm][iorient][3][0]>0) || (SimSyncErr[iarm][iorient][3][3]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][6][0]>0) || (SimSyncErr[iarm][iorient][3][6]>0))  || 
	      ((flagBit[iarm][iorient][7][0]>0) || (SimSyncErr[iarm][iorient][3][7]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][10][0]>0) || (SimSyncErr[iarm][iorient][3][10]>0))  || 
	      ((flagBit[iarm][iorient][11][0]>0) || (SimSyncErr[iarm][iorient][3][11]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][14][0]>0) || (SimSyncErr[iarm][iorient][3][14]>0))  || 
	      ((flagBit[iarm][iorient][15][0]>0) || (SimSyncErr[iarm][iorient][3][15]>0)) ) LW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][18][0]>0) || (SimSyncErr[iarm][iorient][3][18]>0))  || 
	      ((flagBit[iarm][iorient][19][0]>0) || (SimSyncErr[iarm][iorient][3][19]>0)) ) LW[iarm][iorient]++;

	  //cout << LW[iarm][iorient] << endl; 

	  // UPPER WEST

	  UW[iarm][iorient] = 0; 
	  if( ((flagBit[iarm][iorient][2][1]>0) || (SimSyncErr[iarm][iorient][3][2]>0))  || 
	      ((flagBit[iarm][iorient][3][1]>0) || (SimSyncErr[iarm][iorient][3][3]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][6][1]>0) || (SimSyncErr[iarm][iorient][3][6]>0))  || 
	      ((flagBit[iarm][iorient][7][1]>0) || (SimSyncErr[iarm][iorient][3][7]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][10][1]>0) || (SimSyncErr[iarm][iorient][3][10]>0))  || 
	      ((flagBit[iarm][iorient][11][1]>0) || (SimSyncErr[iarm][iorient][3][11]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][14][1]>0) || (SimSyncErr[iarm][iorient][3][14]>0))  || 
	      ((flagBit[iarm][iorient][15][1]>0) || (SimSyncErr[iarm][iorient][3][15]>0)) ) UW[iarm][iorient]++;

	  if( ((flagBit[iarm][iorient][18][1]>0) || (SimSyncErr[iarm][iorient][3][18]>0))  || 
	      ((flagBit[iarm][iorient][19][1]>0) || (SimSyncErr[iarm][iorient][3][19]>0)) ) UW[iarm][iorient]++;

	  //cout << LW[iarm][iorient] << endl; 

	}


      }

      if( (LE[iarm][HORIZONTAL]+LE[iarm][VERTICAL])>=COSMICS_SUM ) Cosmics[iarm][0] = 1; else Cosmics[iarm][0] = 0;   
      if( (LW[iarm][HORIZONTAL]+LW[iarm][VERTICAL])>=COSMICS_SUM ) Cosmics[iarm][1] = 1; else Cosmics[iarm][1] = 0; 
      if( (UE[iarm][HORIZONTAL]+UE[iarm][VERTICAL])>=COSMICS_SUM ) Cosmics[iarm][2] = 1; else Cosmics[iarm][2] = 0; 
      if( (UW[iarm][HORIZONTAL]+UW[iarm][VERTICAL])>=COSMICS_SUM ) Cosmics[iarm][3] = 1; else Cosmics[iarm][3] = 0; 

      //cout << "UE Sum: " << UE[iarm][HORIZONTAL]+UE[iarm][VERTICAL] << " " << Cosmics[iarm][2] << endl;

    }

  }
  else{

    // Zero the symset vectors
    memset(PanelSymsetVector, 0, sizeof(PanelSymsetVector));
    memset(PanelShallowSymsetVector, 0, sizeof(PanelShallowSymsetVector));

    // The new way - calculate symsets by panel

      // South Horizontal Panel 0
      mapMuidPanelLI(SOUTH, HORIZONTAL,0,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, HORIZONTAL, 0); 
      // South Horizontal Panel 1
      mapMuidPanelLI(SOUTH, HORIZONTAL,1,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, HORIZONTAL, 1); 
      // South Horizontal Panel 2
      mapMuidPanelLI(SOUTH, HORIZONTAL,2,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, HORIZONTAL, 2); 
      // South Horizontal Panel 3
      mapMuidPanelLI(SOUTH, HORIZONTAL,3,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, HORIZONTAL, 3); 
      // South Horizontal Panel 4
      mapMuidPanelLI(SOUTH, HORIZONTAL,4,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, HORIZONTAL, 4); 
      // South Horizontal Panel 5
      mapMuidPanelLI(SOUTH, HORIZONTAL,5,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, HORIZONTAL, 5); 

      // South Vertical Panel 0
      mapMuidPanelLI(SOUTH, VERTICAL,0,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, VERTICAL, 0); 
      // South Vertical Panel 1
      mapMuidPanelLI(SOUTH, VERTICAL,1,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, VERTICAL, 1); 
      // South Vertical Panel 2
      mapMuidPanelLI(SOUTH, VERTICAL,2,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, VERTICAL, 2); 
      // South Vertical Panel 3
      mapMuidPanelLI(SOUTH, VERTICAL,3,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, VERTICAL, 3); 
      // South Vertical Panel 4
      mapMuidPanelLI(SOUTH, VERTICAL,4,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, VERTICAL, 4); 
      // South Vertical Panel 5
      mapMuidPanelLI(SOUTH, VERTICAL,5,SimSyncErr,1);
      processSymsetsByPanel(SOUTH, VERTICAL, 5); 


      // North Horizontal Panel 0
      mapMuidPanelLI(NORTH, HORIZONTAL,0,SimSyncErr,1);
      processSymsetsByPanel(NORTH, HORIZONTAL, 0); 
      // North Horizontal Panel 1
      mapMuidPanelLI(NORTH, HORIZONTAL,1,SimSyncErr,1);
      processSymsetsByPanel(NORTH, HORIZONTAL, 1); 
      // North Horizontal Panel 2
      mapMuidPanelLI(NORTH, HORIZONTAL,2,SimSyncErr,1);
      processSymsetsByPanel(NORTH, HORIZONTAL, 2); 
      // North Horizontal Panel 3
      mapMuidPanelLI(NORTH, HORIZONTAL,3,SimSyncErr,1);
      processSymsetsByPanel(NORTH, HORIZONTAL, 3); 
      // North Horizontal Panel 4
      mapMuidPanelLI(NORTH, HORIZONTAL,4,SimSyncErr,1);
      processSymsetsByPanel(NORTH, HORIZONTAL, 4); 
      // North Horizontal Panel 5
      mapMuidPanelLI(NORTH, HORIZONTAL,5,SimSyncErr,1);
      processSymsetsByPanel(NORTH, HORIZONTAL, 5); 

      // North Vertical Panel 0
      mapMuidPanelLI(NORTH, VERTICAL,0,SimSyncErr,1);
      processSymsetsByPanel(NORTH, VERTICAL, 0); 
      // North Vertical Panel 1
      mapMuidPanelLI(NORTH, VERTICAL,1,SimSyncErr,1);
      processSymsetsByPanel(NORTH, VERTICAL, 1); 
      // North Vertical Panel 2
      mapMuidPanelLI(NORTH, VERTICAL,2,SimSyncErr,1);
      processSymsetsByPanel(NORTH, VERTICAL, 2); 
      // North Vertical Panel 3
      mapMuidPanelLI(NORTH, VERTICAL,3,SimSyncErr,1);
      processSymsetsByPanel(NORTH, VERTICAL, 3); 
      // North Vertical Panel 4
      mapMuidPanelLI(NORTH, VERTICAL,4,SimSyncErr,1);
      processSymsetsByPanel(NORTH, VERTICAL, 4); 
      // North Vertical Panel 5
      mapMuidPanelLI(NORTH, VERTICAL,5,SimSyncErr,1);
      processSymsetsByPanel(NORTH, VERTICAL, 5); 

  }

  return 1; 
}

void MuIDLl1::mapMuidPanelLI(int arm, int orient, int panel, short SimSyncErr[2][2][5][20], int Cflag)
{

  // Clear the logical tubes

  if(Cflag){
    for(int j=0;j<GAP_PER_ARM;j++){
      for(int k=0;k<MAX_SYMSETS;k++){
        LT[arm][orient][j][k] = false; 
      }
    }
  }

  numUsedROCBitsSetPanel[arm][orient][panel] = 0; 

  // Combine sync error flags (no chips separation yet)
  
  short CombinedSimSyncErr[MAX_ROC];
  for(int fiber=0;fiber<MAX_ROC;fiber++){
    CombinedSimSyncErr[fiber] = 0; 
    for(int i=0; i<5; i++){
      if(SimSyncErr[arm][orient][i][fiber])
        CombinedSimSyncErr[fiber] = 1;
    }
  }


  switch (arm) {

    case 0:
      {
	// South Arm

	switch (orient) {

	case 0:
	  {
	    // Horizontal
	    
	    switch (panel) {

	    case 0:
	      {
                #include "muid_map_panel0_orient0_arm0.fpga"
		break; 
	      }
	    case 1: 
	      {
                #include "muid_map_panel1_orient0_arm0.fpga"
		break; 
	      }
	    case 2: 
	      {
                #include "muid_map_panel2_orient0_arm0.fpga"
		break; 
	      }
	    case 3: 
	      {
                #include "muid_map_panel3_orient0_arm0.fpga"
		break; 
	      }
	    case 4: 
	      {
                #include "muid_map_panel4_orient0_arm0.fpga"
		break; 
	      }
	    case 5: 
	      {
                #include "muid_map_panel5_orient0_arm0.fpga"
		break; 
	      }
	    }

	    break; 
	  }

	case 1:
	  {
	    // Vertical
	    
	    switch (panel) {

	    case 0:
	      {
                #include "muid_map_panel0_orient1_arm0.fpga"
		break; 
	      }
	    case 1: 
	      {
                #include "muid_map_panel1_orient1_arm0.fpga"
		break; 
	      }
	    case 2: 
	      {
                #include "muid_map_panel2_orient1_arm0.fpga"
		break; 
	      }
	    case 3: 
	      {
                #include "muid_map_panel3_orient1_arm0.fpga"
		break; 
	      }
	    case 4: 
	      {
                #include "muid_map_panel4_orient1_arm0.fpga"
		break; 
	      }
	    case 5: 
	      {
                #include "muid_map_panel5_orient1_arm0.fpga"
		break; 
	      }
	    }

	    break; 
	  }

	}
	    
      
        break;
      }
    case 1:
     {

	// North Arm

	switch (orient) {

	case 0:
	  {
	    // Horizontal
	    
	    switch (panel) {

	    case 0:
	      {
                #include "muid_map_panel0_orient0_arm1.fpga"
		break; 
	      }
	    case 1: 
	      {
                #include "muid_map_panel1_orient0_arm1.fpga"
		break; 
	      }
	    case 2: 
	      {
                #include "muid_map_panel2_orient0_arm1.fpga"
		break; 
	      }
	    case 3: 
	      {
                #include "muid_map_panel3_orient0_arm1.fpga"
		break; 
	      }
	    case 4: 
	      {
                #include "muid_map_panel4_orient0_arm1.fpga"
		break; 
	      }
	    case 5: 
	      {
                #include "muid_map_panel5_orient0_arm1.fpga"
		break; 
	      }
	    }

	    break; 
	  }

	case 1:
	  {
	    // Vertical
	    
	    switch (panel) {

	    case 0:
	      {
                #include "muid_map_panel0_orient1_arm1.fpga"
		break; 
	      }
	    case 1: 
	      {
                #include "muid_map_panel1_orient1_arm1.fpga"
		break; 
	      }
	    case 2: 
	      {
                #include "muid_map_panel2_orient1_arm1.fpga"
		break; 
	      }
	    case 3: 
	      {
                #include "muid_map_panel3_orient1_arm1.fpga"
		break; 
	      }
	    case 4: 
	      {
                #include "muid_map_panel4_orient1_arm1.fpga"
		break; 
	      }
	    case 5: 
	      {
                #include "muid_map_panel5_orient1_arm1.fpga"
		break; 
	      }
	    }

	    break; 
	  }

	}

        break;
      }


  }


}

void MuIDLl1::mapMuidSouthHorizLI(int chip,short SimSyncErr[2][2][5][20], int Cflag)
{

  // Clear the logical tubes

  if(Cflag){
    for(int j=0;j<GAP_PER_ARM;j++){
      for(int k=0;k<MAX_HORIZ_SYMSETS;k++){
        LT[SOUTH][HORIZONTAL][j][k] = false; 
      }
    }
  }

  // Use the FPGA mapping for the appropriate chip

  numUsedROCBitsSet[SOUTH][HORIZONTAL][chip-1] = 0; 

  switch (chip) {

  case 1:
    {
      #include "muid_h1_map_S.fpga"
      break;
    }
  case 2:
    {
      #include "muid_h2_map_S.fpga"
      break;
    }
  case 3:
    {
      #include "muid_h3_map_S.fpga"
      break;
    }
  case 4:
    {
      #include "muid_h4_map_S.fpga"
      break;
    }
  case 5:
    {
      #include "muid_h5_map_S.fpga"
      break;
    }

  }


}

void MuIDLl1::mapMuidSouthVertLI(int chip,short SimSyncErr[2][2][5][20], int Cflag)
{

  // Clear the logical tubes

  if(Cflag){
    for(int j=0;j<GAP_PER_ARM;j++){
      for(int k=0;k<MAX_VERT_SYMSETS;k++){
       LT[SOUTH][VERTICAL][j][k] = false; 
      }
    }
  }

  // Use the FPGA mapping for the appropriate chip

  numUsedROCBitsSet[SOUTH][VERTICAL][chip-1] = 0; 

  switch(chip) {

  case 1:
    {
      #include "muid_v1_map_S.fpga"
      break;
    }
  case 2:
    {
      #include "muid_v2_map_S.fpga"
      break;
    }
  case 3:
    {
      #include "muid_v3_map_S.fpga"
      break;
    }
  case 4:
    {
      #include "muid_v4_map_S.fpga"
      break;
    }
  case 5:
    {
      #include "muid_v5_map_S.fpga"
      break;
    }

  }


}

void MuIDLl1::mapMuidNorthHorizLI(int chip,short SimSyncErr[2][2][5][20], int Cflag)
{

  // Clear the logical tubes

  if(Cflag){
    for(int j=0;j<GAP_PER_ARM;j++){
      for(int k=0;k<MAX_HORIZ_SYMSETS;k++){
        LT[NORTH][HORIZONTAL][j][k] = false; 
      }
    }
  }

  // Use the FPGA mapping for the appropriate chip

  numUsedROCBitsSet[NORTH][HORIZONTAL][chip-1] = 0; 

  switch (chip) {

  case 1:
    {
      #include "muid_h1_map_N.fpga"
      break;
    }
  case 2:
    {
      #include "muid_h2_map_N.fpga"
      break;
    }
  case 3:
    {
      #include "muid_h3_map_N.fpga"
      break;
    }
  case 4:
    {
      #include "muid_h4_map_N.fpga"
      break;
    }
  case 5:
    {
      #include "muid_h5_map_N.fpga"
      break;
    }

  }


}

void MuIDLl1::mapMuidNorthVertLI(int chip,short SimSyncErr[2][2][5][20], int Cflag)
{

  // Clear the logical tubes

  if(Cflag){
    for(int j=0;j<GAP_PER_ARM;j++){
      for(int k=0;k<MAX_VERT_SYMSETS;k++){
       LT[NORTH][VERTICAL][j][k] = false; 
      }
    }
  }

  // Use the FPGA mapping for the appropriate chip

  numUsedROCBitsSet[NORTH][VERTICAL][chip-1] = 0; 

  switch(chip) {

  case 1:
    {
      #include "muid_v1_map_N.fpga"
      break;
    }
  case 2:
    {
      #include "muid_v2_map_N.fpga"
      break;
    }
  case 3:
    {
      #include "muid_v3_map_N.fpga"
      break;
    }
  case 4:
    {
      #include "muid_v4_map_N.fpga"
      break;
    }
  case 5:
    {
      #include "muid_v5_map_N.fpga"
      break;
    }

  }


}

void MuIDLl1::processSymsets(int arm, int orient, int chip)
{

  bool m0, m1[7], m2[13], m3[19], m4[25];
  int dtube(0);
  int start(0);
  int stop(0);

  // Diagnostic logical tubes information

  diagBitvector[arm][orient][chip-1] = 0;

  if( (arm==SOUTH) && (orient==HORIZONTAL) ){

    switch(chip){

    case 1:
      // South Horizontal Chip 1:
      dtube = 14;
      start = 0;
      stop = 25; 
      break;
    case 2:
      // South Horizontal Chip 2:
      dtube = 36;
      start = 26; 
      stop = 47; 
      break;
    case 3:
      // South Horizontal Chip 3:
      dtube = 58;
      start = 48; 
      stop = 69;
      break;
    case 4:
      // South Horizontal Chip 4:
      dtube = 81;
      start = 70;
      stop = 91;
      break;
    case 5:
      // South Horizontal Chip 5:
      dtube = 100;
      start = 92;
      stop = 117; 
      break;
    default:
      cout << " Bad chip number in MuIDLl1::processSymsets! " << endl;

    }
    
  }

  if( (arm==SOUTH) && (orient==VERTICAL) ){

    switch(chip){

    case 1:
      // South Vertical Chip 1:
      dtube = 20;
      start = 0; 
      stop = 33; 
      break;
    case 2:
      // South Vertical Chip 2:
      dtube = 44;
      start = 34; 
      stop = 63;
      break;
    case 3:
      // South Vertical Chip 3:
      dtube = 74;
      start = 64; 
      stop = 93; 
      break;
    case 4:
      // South Vertical Chip 4:
      dtube = 109;
      start = 94;
      stop = 123; 
      break;
    case 5:
      // South Vertical Chip 5:
      dtube = 140;
      start = 124; 
      stop = 157; 
      break;
    default:
      cout << " Bad chip number in MuIDLl1::processSymsets! " << endl;
    }

  }

  if( (arm==NORTH) && (orient==HORIZONTAL) ){

    switch(chip){

    case 1:
      // North Horizontal Chip 1:
      dtube = 14;
      start = 0; 
      stop = 25;
      break;
    case 2:
      // North Horizontal Chip 2:
      dtube = 36;
      start = 26; 
      stop = 47;
      break;
    case 3:
      // North Horizontal Chip 3:
      dtube = 58;
      start = 48; 
      stop = 69;
      break;
    case 4:
      // North Horizontal Chip 4:
      dtube = 81;
      start = 70; 
      stop = 91;
      break;
    case 5:
      // North Horizontal Chip 5:
      dtube = 100;
      start = 92; 
      stop = 117;
      break;
    default:
      cout << " Bad chip number in MuIDLl1::processSymsets! " << endl;
    }

  }

  if( (arm==NORTH) && (orient==VERTICAL) ){

    switch(chip){

    case 1:
      // North Vertical Chip 1:
      dtube = 20;
      start = 0; 
      stop = 33;
      break;
    case 2:
      // North Vertical Chip 2:
      dtube = 44;
      start = 34; 
      stop = 63;
      break;
    case 3:
      // North Vertical Chip 3:
      dtube = 74;
      start = 64; 
      stop = 93;
      break;
    case 4:
      // North Vertical Chip 4:
      dtube = 109;
      start = 94; 
      stop = 123;
      break;
    case 5:
      // North Vertical Chip 5:
      dtube = 140;
      start = 124; 
      stop = 157;
      break;
    default:
      cout << " Bad chip number in MuIDLl1::processSymsets! " << endl;
    }

  }    

  // Loop over tubes, process symsets and diagnostic tube information

  for(int i=start; i<=stop; i++){

    m0 = LT[arm][orient][0][i];
    if(i==dtube) {
      if(m0) diagBitvector[arm][orient][chip-1] |= 0x1;
    }

    for(int j=0;j<7;j++){
        int tube = i-(3-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m1[j] = LT[arm][orient][1][tube];
        else
          m1[j] = false;

	if( (i==dtube) && (j>=2) && (j<=4) ){
	  if(m1[j]) diagBitvector[arm][orient][chip-1] |= (0x1<<(1+j-2));
	}

    }

    for(int j=0;j<13;j++){
        int tube = i-(6-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m2[j] = LT[arm][orient][2][tube];
        else
          m2[j] = false;

	if( (i==dtube) && (j>=5) && (j<=7) ){
	    if(m2[j]) diagBitvector[arm][orient][chip-1] |= (0x1<<(4+j-5));
	}

    }

    for(int j=0;j<19;j++){
        int tube = i-(9-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m3[j] = LT[arm][orient][3][tube];
        else
          m3[j] = false;

	if( (i==dtube) && (j>=8) && (j<=10) ){
	    if(m3[j]) diagBitvector[arm][orient][chip-1] |= (0x1<<(7+j-8));
	}

    }

    for(int j=0;j<25;j++){
        int tube = i-(12-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m4[j] = LT[arm][orient][4][tube];
        else
          m4[j] = false;

	if( (i==dtube) && (j>=11) && (j<=13) ){
	    if(m4[j]) diagBitvector[arm][orient][chip-1] |= (0x1<<(10+j-11));
	}

    }

    if(Symset(m0,m1,m2,m3,m4)) {
      int word = (int)(i/32);
      int bit = i-(32*word);
      //cout << " Symset " << i << " projection " << orient << " fired, word = " << word << " bit = " << bit << endl;

      //if((orient==HORIZONTAL)&&(arm==NORTH)){ 
      //  word = (int)((i-2)/32);
      //  bit = (i-2)-(32*word);
      //	SymsetVector[arm][orient][word]|=(0x1<<bit);
      //}
      //else{
      //  SymsetVector[arm][orient][word]|=(0x1<<bit);
      //}

      SymsetVector[arm][orient][word]|=(0x1<<bit);

    }

    if(ShallowSymset(m0,m1,m2,m3,m4)) {
      int word = (int)(i/32);
      int bit = i-(32*word);
      //cout << " Symset " << i << " projection " << orient << " fired, word = " << word << " bit = " << bit << endl;

      //if((orient==HORIZONTAL)&&(arm==NORTH)){ 
      //  word = (int)((i-2)/32);
      //  bit = (i-2)-(32*word);
      //  ShallowSymsetVector[arm][orient][word]|=(0x1<<bit);
      //}
      //else{
      //  ShallowSymsetVector[arm][orient][word]|=(0x1<<bit);
      //}

      ShallowSymsetVector[arm][orient][word]|=(0x1<<bit);

    }

  }

}

bool MuIDLl1::Symset(bool m0, bool m1[], bool m2[], bool m3[], bool m4[])
{

  bool s1, s2, s3, s4;
  bool sp1, sp2, sp3;

  // DEEP ALGORITHM
  
  s1 = (m1[2] || m1[3] || m1[4]);
  s2 = (m2[5] || m2[6] || m2[7]);
  s3 = (m3[8] || m3[9] || m3[10]);
  s4 = (m4[11] || m4[12] ||  m4[13]);

  if(!RequireFourGaps){

    // At least three gaps hit
    // changed for Run-5 (last half) JGL 2/14/2005
    sp1 = (s1 && s2 && s3) || (s1 && s2 && s4) || (s2 && s3 && s4) ||
          (s1 && s3 && s4) || (m0 && s1 && s2) || (m0 && s1 && s3) ||
          (m0 && s1 && s4) || (m0 && s2 && s3) || (m0 && s2 && s4) ||
          (m0 && s3 && s4); 

  }
  else{

    // At least four gaps hit (1/30/04)
    sp1 = (m0 && s1 && s2 && s3) || (m0 && s1 && s2 && s4) || (m0 && s2 && s3 && s4) ||
          (m0 && s1 && s3 && s4) || (s1 && s2 && s3 && s4);
  }

  // Gap 3 or Gap 4 required
  sp2 = (s3 || s4);
  
  // Central index in Gap0 or Gap1
  sp3 = (m0 || m1[3]);
  
  // Check gaps for showering:

  int tubesHit[4]; 
  for(int i=0;i<4;i++){
    tubesHit[i] = 0; 

    switch(i){

      case 0:
      
        for(int j=0;j<7;j++) 
          if(m1[j]) tubesHit[i]++;
	break;

      case 1:
      
        for(int j=0;j<13;j++) 
          if(m2[j]) tubesHit[i]++;
	break;

      case 2:
      
        for(int j=0;j<19;j++) 
          if(m3[j]) tubesHit[i]++;
	break;

      case 3:
      
        for(int j=0;j<25;j++) 
          if(m4[j]) tubesHit[i]++;
	break;

    }

  }

  bool shower = false; 
  //if( (tubesHit[0]>5) || (tubesHit[1]>5) || (tubesHit[2]>5) || (tubesHit[3]>5) ) shower = true; 
  
  // Final Logic 

  if(sp1 && sp2 && sp3 && !shower) 
    return true;
  else
    return false;

}

bool MuIDLl1::ShallowSymset(bool m0, bool m1[], bool m2[], bool m3[], bool m4[])
{

  bool s1, s2, s3, s4;
  bool sps;

  // SHALLOW ALGORITHM
  
  s1 = (m1[2] || m1[3] || m1[4]);
  s2 = (m2[5] || m2[6] || m2[7]);
  s3 = (m3[8] || m3[9] || m3[10]);
  s4 = (m4[11] || m4[12] ||  m4[13]);

  if(!OneHadronTrigger){

    sps = (m0 && s1) || (m0 && s2) || (s1 && s2); 
    //sps = (m0 && s1 && s2); 

    if( (m0||s1) && sps  ) 
      return true;
    else
      return false;

  }
  else{

    // Algorithm changed for 1H (hadron) definition for Run-8
    // JGL 12/9/2007

    sps = (m0||m1[3]) && (s2||s3) && (!s4);

    if (( (m0 && s2 && s3) || (s1 && s2 && s3) || (m0 && s1 && s2) || (m0 && s1 && s3) ) && sps)
      return true; 
    else
      return false; 

  }

}

void MuIDLl1::getSymsetVector(int arm, int orient, long SymVec[])
{

  if(SymVec==NULL) return;

  for(int i=0;i<5;i++) 
    SymVec[i] = SymsetVector[arm][orient][i];

}

void MuIDLl1::getShallowSymsetVector(int arm, int orient, long SymVec[])
{

  if(SymVec==NULL) return;

  for(int i=0;i<5;i++) 
    SymVec[i] = ShallowSymsetVector[arm][orient][i];

}

void MuIDLl1::getPanelSymsetVector(int arm, int orient, int panel, long SymVec[])
{

  if(SymVec==NULL) return;

  for(int i=0;i<5;i++) 
    SymVec[i] = PanelSymsetVector[arm][orient][panel][i];

}

void MuIDLl1::getPanelShallowSymsetVector(int arm, int orient, int panel, long SymVec[])
{

  if(SymVec==NULL) return;

  for(int i=0;i<5;i++) 
    SymVec[i] = PanelShallowSymsetVector[arm][orient][panel][i];

}

void MuIDLl1::getCosmics(int arm, long CosVec[])
{

  if(CosVec==NULL) return;

  for(int i=0;i<4;i++) 
    CosVec[i] = Cosmics[arm][i];

}

long MuIDLl1::getNumROCBitsSet(int arm, int orient, int iroc)
{

  return numROCBitsSet[arm][orient][iroc];

}

long MuIDLl1::getNumROCBitsSet(int arm, int orient)
{

  int sum = 0; 

  for(int i=0; i<MAX_ROC; i++){
    sum += numROCBitsSet[arm][orient][i];
  }

  return sum;

}

long MuIDLl1::getNumUsedROCBitsSet(int arm, int orient, int chip)
{

  return numUsedROCBitsSet[arm][orient][chip];

}


long MuIDLl1::getDiagLT(int arm, int orient, int chip)
{

  return diagBitvector[arm][orient][chip-1];
  
}

int MuIDLl1::getRoadSum(int arm, int orient, int flag)
{

  int RoadSum = 0; 
  
  if(!NewMuidPanelAlg){
    if(orient==0) { // horizontal
      if((numUsedROCBitsSet[arm][orient][0]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,0,25, flag);   // chip 1
      if((numUsedROCBitsSet[arm][orient][1]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,26,47, flag);  // chip 2
      if((numUsedROCBitsSet[arm][orient][2]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,48,69, flag);  // chip 3
      if((numUsedROCBitsSet[arm][orient][3]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,70,91, flag);  // chip 4
      if((numUsedROCBitsSet[arm][orient][4]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,92,117, flag); // chip 5
    }
    else if(orient==1) { // vertical
      if((numUsedROCBitsSet[arm][orient][0]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,0,33, flag);    // chip 1
      if((numUsedROCBitsSet[arm][orient][1]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,34,63, flag);   // chip 2
      if((numUsedROCBitsSet[arm][orient][2]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,64,93, flag);   // chip 3
      if((numUsedROCBitsSet[arm][orient][3]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,94,123, flag);  // chip 4
      if((numUsedROCBitsSet[arm][orient][4]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) RoadSum += EdgeCount(arm,orient,124,157, flag); // chip 5
    }
  }
  else{

    // This is a sum over all panels

    for(int i=0; i<PANEL_PER_GAP; i++) {
      if((numUsedROCBitsSetPanel[arm][orient][i]<FPGA_ROC_BITS_CUT)||(!ROCNoiseCut)) 
        RoadSum += getRoadSumByPanel(arm,orient,i,flag);
    }
      

  }

  return RoadSum; 
}

int MuIDLl1::getRoadSumByPanel(int arm, int orient, int panel, int flag)
{

  int RoadSum = 0; 
  
  if(arm==NORTH){

    if(orient==HORIZONTAL) { // horizontal
      switch(panel){
      case(0):
	RoadSum += EdgeCountByPanel(arm,orient,61,117,panel,flag); 
	break; 
      case(1):
	RoadSum += EdgeCountByPanel(arm,orient,71,117,panel,flag); 
	break; 
      case(2):
	RoadSum += EdgeCountByPanel(arm,orient,61,117,panel,flag); 
	break; 
      case(3):
	RoadSum += EdgeCountByPanel(arm,orient,1,60,panel,flag); 
	break; 
      case(4):
	RoadSum += EdgeCountByPanel(arm,orient,1,46,panel,flag); 
	break; 
      case(5):
	RoadSum += EdgeCountByPanel(arm,orient,1,60,panel,flag); 
	break; 
      default:
	break;
      }

    }
    else if(orient==VERTICAL) { // vertical
      switch(panel){
      case(0):
	RoadSum += EdgeCountByPanel(arm,orient,91,155,panel,flag);    
	break;
      case(1):
	RoadSum += EdgeCountByPanel(arm,orient,65,90,panel,flag);    
	break;
      case(2):
	RoadSum += EdgeCountByPanel(arm,orient,1,64,panel,flag);    
	break;
      case(3):
	RoadSum += EdgeCountByPanel(arm,orient,1,64,panel,flag);    
	break;
      case(4):
	RoadSum += EdgeCountByPanel(arm,orient,65,90,panel,flag);    
	break;
      case(5):
	RoadSum += EdgeCountByPanel(arm,orient,91,154,panel,flag);    
	break;
      default:
	break;
      }

    }

  }

  if(arm==SOUTH){

    if(orient==HORIZONTAL) { // horizontal
      switch(panel){
      case(0):
	RoadSum += EdgeCountByPanel(arm,orient,60,117,panel,flag); 
	break; 
      case(1):
	RoadSum += EdgeCountByPanel(arm,orient,70,115,panel,flag); 
	break; 
      case(2):
	RoadSum += EdgeCountByPanel(arm,orient,60,117,panel,flag); 
	break; 
      case(3):
	RoadSum += EdgeCountByPanel(arm,orient,1,59,panel,flag); 
	break; 
      case(4):
	RoadSum += EdgeCountByPanel(arm,orient,1,45,panel,flag); 
	break; 
      case(5):
	RoadSum += EdgeCountByPanel(arm,orient,1,59,panel,flag);
	break; 
      default:
	break;
      }

    }
    else if(orient==VERTICAL) { // vertical
      switch(panel){
      case(0):
	RoadSum += EdgeCountByPanel(arm,orient,91,155,panel,flag);    
	break;
      case(1):
	RoadSum += EdgeCountByPanel(arm,orient,65,90,panel,flag);    
	break;
      case(2):
	RoadSum += EdgeCountByPanel(arm,orient,1,64,panel,flag);    
	break;
      case(3):
	RoadSum += EdgeCountByPanel(arm,orient,1,64,panel,flag);    
	break;
      case(4):
	RoadSum += EdgeCountByPanel(arm,orient,65,90,panel,flag);    
	break;
      case(5):
	RoadSum += EdgeCountByPanel(arm,orient,91,154,panel,flag);    
	break;
      default:
	break;
      }

    }

  }

  return RoadSum; 
}

int MuIDLl1::EdgeCount(int arm, int orient, int start, int stop, int flag)
{

  int edgeCt = 0;
  int prev_symset = 0; 

  // This simulates the FPGA hardware edge counter

  for (int i = 0; i < 5; i++){
    for (int j = 0; j < 32; j++) {
      int symset = i*32 + j;
      if((symset>=start)&&(symset<=stop)){
	
	if(flag==0) {
	  if( ((SymsetVector[arm][orient][i]&(0x1<<j))!=0) && (prev_symset==0) ) edgeCt++; 
	  prev_symset =  ((SymsetVector[arm][orient][i]&(0x1<<j))!=0) ? 1 : 0;
	}
	else{
	  if( ((ShallowSymsetVector[arm][orient][i]&(0x1<<j))!=0) && (prev_symset==0) ) edgeCt++; 
	  prev_symset =  ((ShallowSymsetVector[arm][orient][i]&(0x1<<j))!=0) ? 1 : 0;
	}
	  
      }
    }
  }

  /*
  // Diagnostics
  if(edgeCt){
    if(flag==0){
      cout << " Symset Vector = " << SETW(8) << hex << SymsetVector[arm][orient][4] << " " << SETW(8) << SymsetVector[arm][orient][3] << " " 
           << SETW(8) << SymsetVector[arm][orient][2] << " " << SETW(8) << SymsetVector[arm][orient][1] << " " << SETW(8) 
  	   << SymsetVector[arm][orient][0] << dec << endl;
    }
    else{
      cout << " Shallow Symset Vector = " << SETW(8) << hex << ShallowSymsetVector[arm][orient][4] << " " << SETW(8) 
	   << ShallowSymsetVector[arm][orient][3] << " " << SETW(8) << ShallowSymsetVector[arm][orient][2] << " " << SETW(8) 
	   << ShallowSymsetVector[arm][orient][1] << " " << SETW(8) << ShallowSymsetVector[arm][orient][0] << dec << endl;
    }
    cout << " Start = " << start << " Stop = " << stop << endl;
    cout << " Edge Count = " << edgeCt << endl << endl; 
  }
  */

  return edgeCt; 

}

int MuIDLl1::EdgeCountByPanel(int arm, int orient, int start, int stop, int panel, int flag)
{

  int edgeCt = 0;
  int edgeCt_reduced = 0; 
  int prev_symset = 0; 

  // This simulates the FPGA hardware edge counter

  for (int i = 0; i < 5; i++){
    for (int j = 0; j < 32; j++) {
      int symset = i*32 + j;
      if((symset>=start)&&(symset<=stop)){
	
	if(flag==0) {
	  if( ((PanelSymsetVector[arm][orient][panel][i]&(0x1<<j))!=0) && (prev_symset==0) ) edgeCt++; 
	  prev_symset =  ((PanelSymsetVector[arm][orient][panel][i]&(0x1<<j))!=0) ? 1 : 0;
	}
	else{
	  if( ((PanelShallowSymsetVector[arm][orient][panel][i]&(0x1<<j))!=0) && (prev_symset==0) ) edgeCt++; 
	  prev_symset =  ((PanelShallowSymsetVector[arm][orient][panel][i]&(0x1<<j))!=0) ? 1 : 0;
	}
	  
      }
    }
  }

  
  /*
  //if(edgeCt){
    if(flag==0){
      cout << " Symset Vector = " << SETW(8) << hex << PanelSymsetVector[arm][orient][panel][4] << " " << SETW(8) << PanelSymsetVector[arm][orient][panel][3] << " " 
           << SETW(8) << PanelSymsetVector[arm][orient][panel][2] << " " << SETW(8) << PanelSymsetVector[arm][orient][panel][1] << " " << SETW(8) 
  	   << PanelSymsetVector[arm][orient][panel][0] << dec << endl;
    }
    else{
      cout << " Shallow Symset Vector = " << SETW(8) << hex << PanelShallowSymsetVector[arm][orient][panel][4] << " " << SETW(8) 
	   << PanelShallowSymsetVector[arm][orient][panel][3] << " " << SETW(8) << PanelShallowSymsetVector[arm][orient][panel][2] << " " << SETW(8) 
	   << PanelShallowSymsetVector[arm][orient][panel][1] << " " << SETW(8) << PanelShallowSymsetVector[arm][orient][panel][0] << dec << endl;
    }
    cout << " Panel = " << panel << " Arm = " << arm << " Orient = " << orient << " Start = " << start << " Stop = " << stop << endl;
    cout << " Edge Count = " << edgeCt << endl << endl; 
    //}
  */


  if(edgeCt==1) 
    edgeCt_reduced = 1; 
  else if(edgeCt==2) 
    edgeCt_reduced = 2; 
  else if(edgeCt>=3) 
    edgeCt_reduced = 3; 
  else
    edgeCt_reduced = 0; 


  return edgeCt_reduced; 

}

bool MuIDLl1::GL1_1Deep_S(){

    return( (getRoadSum(SOUTH,HORIZONTAL,0)>0) && (getRoadSum(SOUTH,VERTICAL,0)>0) );

}

bool MuIDLl1::GL1_1Deep_N(){

    return( (getRoadSum(NORTH,HORIZONTAL,0)>0) && (getRoadSum(NORTH,VERTICAL,0)>0) );

}

bool MuIDLl1::GL1_2Deep_S(){

    return( (getRoadSum(SOUTH,HORIZONTAL,0)>1) && (getRoadSum(SOUTH,VERTICAL,0)>1) );

}

bool MuIDLl1::GL1_2Deep_N(){

    return( (getRoadSum(NORTH,HORIZONTAL,0)>1) && (getRoadSum(NORTH,VERTICAL,0)>1) );

}

bool MuIDLl1::GL1_1Deep1Shallow_S(){

    return( (getRoadSum(SOUTH,HORIZONTAL,0)>0) && (getRoadSum(SOUTH,VERTICAL,0)>0) && (getRoadSum(SOUTH,HORIZONTAL,1)>1) && (getRoadSum(SOUTH,VERTICAL,1)>1) );

}

bool MuIDLl1::GL1_1Deep1Shallow_N(){

    return( (getRoadSum(NORTH,HORIZONTAL,0)>0) && (getRoadSum(NORTH,VERTICAL,0)>0) && (getRoadSum(NORTH,HORIZONTAL,1)>1) && (getRoadSum(NORTH,VERTICAL,1)>1) );

}

bool MuIDLl1::GL1_1Shal_S(){

    return( (getRoadSum(SOUTH,HORIZONTAL,1)>0) && (getRoadSum(SOUTH,VERTICAL,1)>0) );

}

bool MuIDLl1::GL1_1Shal_N(){

    return( (getRoadSum(NORTH,HORIZONTAL,1)>0) && (getRoadSum(NORTH,VERTICAL,1)>0) );
 
}

void MuIDLl1::getHitSymsetList(int arm, int orientation, int flag, vector<short>& list){ 

  // generate a list of hit symsets

  list.clear(); 

  for (int i = 0; i < 5; i++){
    //cout << orientation << " " << hex << (unsigned long) SymsetVector[arm][orientation][i] << endl; 

    for (int j = 0; j < 32; j++) {
      int symset = i*32 + j;
	
      if(flag==0) {
        if( (SymsetVector[arm][orientation][i]&(0x1<<j))!=0 ) list.push_back(symset);
      }
      else{
	if( (ShallowSymsetVector[arm][orientation][i]&(0x1<<j))!=0 ) list.push_back(symset);  	  
      }

    }
  }


}


#define MAX_CLUSTER_SIZE 5

void MuIDLl1::getHitClusterList(int arm, int orient, int flag, vector<short>& list){ 

  // generate a list of hit clusters
  // typically more than one symset fires a for a given deep track, so 
  // organize all contiguous hit symsets as a cluster, and return one entry 
  // corresponding to the center of the cluster

  list.clear(); 

  bool clusterBegin = false; 
  bool clusterEnd = false; 
  int clustSize = 0; 
  int prev_symset = 0; 

  for (int i = 0; i < 5; i++){
    for (int j = 0; j < 32; j++) {
        int symset = i*32 + j;
	
	  // Detect first occurence of a "1", indicating the start of a cluster
	  // Count the number of symsets until you hit the 1->0 edge. 

	if(flag==0) {
	  if( ((SymsetVector[arm][orient][i]&(0x1<<j))!=0) && (prev_symset==0) ) clusterBegin = true; 
	  if( ((SymsetVector[arm][orient][i]&(0x1<<j))==0) && (prev_symset==1) ) clusterEnd = true; 
	}
	else{
	  if( ((ShallowSymsetVector[arm][orient][i]&(0x1<<j))!=0) && (prev_symset==0) ) clusterBegin = true; 
	  if( ((ShallowSymsetVector[arm][orient][i]&(0x1<<j))==0) && (prev_symset==1) ) clusterEnd = true; 
	}

	if(clusterBegin && !clusterEnd) clustSize++; 

	if(clusterBegin && clusterEnd){
  	  // Completed cluster
	  int cluster = (int) (symset - 0.5*clustSize); 
	  list.push_back(cluster);	

	  //cout << "MuID cluster = " << cluster << " size = " << clustSize << endl;  
    
	  clusterBegin = false; 
	  clusterEnd = false; 
	  clustSize=0; 
	}
	else if(clusterBegin && !clusterEnd && (clustSize>MAX_CLUSTER_SIZE)){
	  // Cluster is getting too large!
	  // End it here and start a new cluster
	  int cluster = (int) (symset - 0.5*(clustSize-1) - 1); 
	  list.push_back(cluster);	    

	  //cout << "MuID cluster (terminating) = " << cluster << " size = " << (clustSize-1) << endl;  

	  clusterBegin = true; 
	  clusterEnd = false; 
	  clustSize=1; 
	}

	if(flag==0)
	  prev_symset =  ((SymsetVector[arm][orient][i]&(0x1<<j))!=0) ? 1 : 0;
	else
	  prev_symset =  ((ShallowSymsetVector[arm][orient][i]&(0x1<<j))!=0) ? 1 : 0;
	  
      
    }
  }


}

void MuIDLl1::processSymsetsByPanel(int arm, int orient, int panel)
{

  bool m0, m1[7], m2[13], m3[19], m4[25];

  // Loop over tubes, process symsets

  for(int i=0; i<=MAX_SYMSETS; i++){

    m0 = LT[arm][orient][0][i];

    for(int j=0;j<7;j++){
        int tube = i-(3-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m1[j] = LT[arm][orient][1][tube];
        else
          m1[j] = false;
    }

    for(int j=0;j<13;j++){
        int tube = i-(6-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m2[j] = LT[arm][orient][2][tube];
        else
          m2[j] = false;
    }

    for(int j=0;j<19;j++){
        int tube = i-(9-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m3[j] = LT[arm][orient][3][tube];
        else
          m3[j] = false;
    }

    for(int j=0;j<25;j++){
        int tube = i-(12-j);
        if( tube>=0 && ( ((orient==VERTICAL)&&(tube<MAX_VERT_SYMSETS)) || ((orient==HORIZONTAL)&&(tube<MAX_HORIZ_SYMSETS)) ) )
          m4[j] = LT[arm][orient][4][tube];
        else
          m4[j] = false;
    }

    if(Symset(m0,m1,m2,m3,m4)) {
      int word = (int)(i/32);
      int bit = i-(32*word);
      PanelSymsetVector[arm][orient][panel][word]|=(0x1<<bit);
    }

    if(ShallowSymset(m0,m1,m2,m3,m4)) {
      int word = (int)(i/32);
      int bit = i-(32*word);
      PanelShallowSymsetVector[arm][orient][panel][word]|=(0x1<<bit);
    }

  }

}
