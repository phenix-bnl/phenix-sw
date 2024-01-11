#ifndef PADADDRESSOBJECT_H
#define PADADDRESSOBJECT_H

//--------------------------------------------------------------- 
//                                                                
// Created by: David Silvermyr
//                                                                
// Description:  Header for PADAddressObject class
//
// One object corresponds to one readout channel (a pad)
//                                                                
//----------------------------------------------------------------

#include <cstdlib>
#include <iostream>
#include <cstdio>

#include "phool.h"

/**
Class PADAddressObject contains routines for Hardware/Software indices conversion.
@memo PAD Address Class
*/

class PadAddressObject {
  
public:

  // Indices of PadAddressObject
  enum{DET, ARM, SIDE, SECTOR, PACKETID, GROW, GCOL, TGL, CHANNEL, HOLE, CHANNELID, PADZ, PADX, CELLSECT, WIRESECT};
  
private:

  // current indices for each variable
  int current_det;        // 0,1,2 for PC1,2,3                         
  int current_arm;	  // 0 for West, 1 for East                    
  int current_side;	  // 0 for South, 1 for North                  
  int current_sector;	  // 0-7 for PC1, 0-3 for PC2/3                
  int current_packetid;	  // 4001-4096                                 
  int current_grow;	  // (grouprow) 0 to 4                         
  int current_gcol;	  // (groupcolumn) 0 to 8                      
  int current_tgl;	  // 1 to 3, chipnumber on the ROC             
  int current_channel;	  // 1 to 16,                                  
  int current_hole;       // index of pixel used by Mike, Vicki et al. 
  int current_channelid;  // 0 to 2159                                   
  int current_padz;       // 0 to 107 for a chamberhalf                
  int current_padx;	  // 0 to 19 for PC1, 0 to 39 for PC2/3        

  // and some entities related to hits.. coordinate of a pad in cell(z) and wire space
  // valid within the whole _sector_  
  int current_cellsect;	  
  int current_wiresect;

public: 
 
  // Constructor
  PadAddressObject(); 
  // Destructor
  ~PadAddressObject(); 

  // Set current indices
  PHBoolean setSoft(int idet, int iarm, int iside, int isector, int padz, int padx);

  // Set current indices
  PHBoolean setHard(int packetid, int channelid);

  // get current values
  int getDet() { return current_det; }
  int getArm() { return current_arm; }
  int getSide() { return current_side; }
  int getSector() { return current_sector; }
  int getPacketid() { return current_packetid; }
  int getGrow() { return current_grow; }
  int getGcol() { return current_gcol; }
  int getTgl() { return current_tgl; }
  int getChannel() { return current_channel; }
  int getHole() { return current_hole; }
  int getChannelid() { return current_channelid; }
  int getPadz() { return current_padz; }
  int getPadx() { return current_padx; }
  int getCellsect() { return current_cellsect; }
  int getWiresect() { return current_wiresect; }
  
  // Get software indices from hardware indices 
  int getDet(int ipacketid);
  int getArm(int ipacketid);
  int getSide(int ipacketid);
  int getSector(int ipacketid);
  int getPadz(int ichannelid);
  int getPadx(int ipacketid, int ichannelid);

  // Get other software indices from software indices.. 
  int getCellsect(int side, int ipadz);
  int getWiresect(int det, int arm, int side, int ipadz, int ipadx);
  // and the reverse..
  int getPadzFromCellsect(int side, int icellsect);
  int getPadxFromCellWiresect(int det, int iarm, int side, int icellsect, int iwiresect);

  // Get hardware indices from software indices 
  int getPacketid(int idet, int iarm, int iside, int isector, int ipadx);
  int getChannelid(int idet, int ipadz, int ipadx);

  // Get other hardware indices from hardware indices.. 
  int getGrow(int ichannelid);
  int getGcol(int ichannelid);
  int getTgl(int ichannelid);
  int getChannel(int ichannelid);
  int getHole(int itgl, int ichannel);

  // Print information about object
  void print();
  
private:
  
  // Convert from Software indices to hardware
  PHBoolean FromSoftToHard();

  // Convert from Hardvare indices to software
  PHBoolean FromHardToSoft();

 // Status of the object (0 = ok/initialized/set functions called)
  int iFlag;

}; 

#endif /* PADADDRESSOBJECT_H */ 

