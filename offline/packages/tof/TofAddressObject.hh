//-----------------------------------------------------------------------------
//  Declaration of class TofAddressObject
//
//  Purpose: Channel Mapping organization for Time-of-Flight
//
//  Description: TOF Address Object
//
//  Author: Akio Kiyomichi (Univ.of Tsukuba)
//
//  History: 04/10/00  A.Kiyomichi  First Version
//           06/05/00  A.Kiyomichi  Add getPmt()
//           06/15/00  A.Kiyomichi  Add cable map
//           07/28/00  A.Kiyomichi  Add FEM board/channel validity 
//           02/26/01  A.Kiyomichi  Add map_slatid[CRATE][BOARD][CHANNEL];
//           06/09/01  H.Masui      Add hvmap_slatid[CRATE][BORARD][CHANNEL]
//                                   &  getSlatIDfromHV()
//           11/29/01  A.Kiyomichi  Add getSlatIDfromPISA()
//           
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// TOF Nomenclature
//                                                 :      slat
//       +---+---+---+---+---+---+---+---+ sector  :  +-----------+
//       |   |   |   |   |   |   |   |   |         :  |           |
// panel | 3 | 2 | 1 | 0 | 0 | 1 | 2 | 3 |         :  | 95 ---- 80|
//       |   |   |   |   |   |   |   |   |   1     :  |           |
//       | I | H | G | F | D | C | B | A |         :  |79 ---- 64 |
//       |   |   |   |   |   |   |   |   |         :  |           |
//       +---+---+---+---+---+---+---+---+         :  |           |
//                   +---+---+                     :  | 63 ---- 48|
//                   |   |   |                     :  |           |
//                   | 0 | 0 |                     :  |47 ---- 32 |
//                   |   |   |               0     :  |           |
//                   | J | E |                     :  |||         |
//                   |   |   |                     :  ||31 ---- 16|
//                   +---+---+                     :  |||||       |
//    - - - - - - - - - - - - - - - - - -          :  |15|| -- 00 |
//     side      1               0                 :  |[][] ...[] |
//            (North)         (South)              :  +-----------+
// 
//  panel_seq  = 0:1:2:3:4:5:6:7:8:9
//  panel_char = A:B:C:D:F:G:H:I:E:J
//     sector  = 1:1:1:1:1:1:1:1:0:0
//     side    = 0:0:0:0:1:1:1:1:0:1
//     panel   = 3:2:1:0:0:1:2:3:0:0
//
// TOF FEM Rack
//       +-----------+       +-----------+
//       | +-------+ |       | +-------+ |     +--+
//      Crate  0   | |       | |   4   | |     |  |
//       | +-------+ |       | +-------+ |     |15|
//       | +-------+ |       | +-------+ |     |  |
//       | |   1   | |       | |   5   | |     |  |
//       | +15----0+ |       | +-------+ |     |  |
//       |    slot   |       |           |     |  |
//       | +-------+ |       | +-------+ |     | 1|
//       | |   2   | |       | |   6   | |     | 0| channel
//       | +-------+ |       | +-------+ |     |  |
//       | +-------+ |       | +-------+ |     |  |
//       | |   3   | |       | |   7   | |     +--+
//       | +-------+ |       | +-------+ |
//       +-----------+       +-----------+
//          (North)             (South)
//
//  http://www.phenix-j.rhic.bnl.gov/tof/doc/femmap/
//
//-----------------------------------------------------------------------------

#ifndef __TOFADDRESSOBJECT_HH__
#define __TOFADDRESSOBJECT_HH__

#include "Tof.hh"
#include "TofBasicObject.hh"

#include "PdbIndex.hh" 
#include "PHPointerList.h"

class PHCompositeNode;

/** 
This is TOF Address Object class. <br>
Created: 04/10/00. 
@author Akio Kiyomichi (Univ. of Tsukuba)
<a href="mailto:kiyo@bnl.gov">kiyo@bnl.gov</a> 
@memo TOF Address Object Class 
*/

class TofAddressObject : public TofBasicObject{
public:
  enum {SLATID, CRATE, SLOT_0, SLOT_1, CHANNEL_0, CHANNEL_1};

public:
  // Constructor
  TofAddressObject();
  // Destructor
  virtual ~TofAddressObject();
  
  // Get Name
  PHString getName() {return "TOF Address Object";} 
 
  // Set current indices
  PHBoolean setSlatID(int id);
  // Set FEM map
  PHBoolean setHardMap(int id, int icrate, int islot0, int islot1, 
		       int ichannel0, int ichannel1);
  PHBoolean setFemMap(int id, int icrate, int islot0, int islot1, 
		      int ichannel0, int ichannel1);
  PHBoolean setCableLengthMap(int id, float length0, float length1);

  PHBoolean setHvMap(int id, int hvcrate, int hvboard, int hvchannel);

  // Set current indices
  PHBoolean setHard(int icrate, int islot, int ichannel);
  // Set current indices
  PHBoolean setSoft(int iarm,  int isector, int iside, int ipanel, int islat);
  PHBoolean setSoft(int ipanel_seq, int islat);
  PHBoolean setSoft(const char *ipanel_char, int islat);

  // Get current value
  int getSlatID() {return slatid->getValue();}
  int getArm() {return arm->getValue();}
  int getSector() {return sector->getValue();}
  int getSide() {return side->getValue();}
  int getPanel() {return panel->getValue();}
  int getSlat() {return slat->getValue();}

  int getPanelSeq() {return panel_seq;}
  const char *getPanelChar() {return panel_char;}

  int getCrate() {return crate->getValue();}
  int getSlot0() {return slot0->getValue();}
  int getSlot1() {return slot1->getValue();}
  int getChannel0() {return channel0->getValue();}
  int getChannel1() {return channel1->getValue();}

  // Get slatid
  int getSlatID(int iarm, int isector, int iside, int ipanel, int islat);
  int getSlatID(int ipanel_seq, int islat);
  int getSlatID(const char *ipanel_char, int islat);
  int getSlatID(int icrate, int islot, int ichannel);
  int getPmt(int icrate, int islot, int ichannel); // 0:lower 1:upper
  int getSlatIDfromHV(int icrate,int iboard, int ichannel);
  int getSlatIDfromPISA(int ipanel, int column, int pslat);

  // Get from slatid
  int getArm(int id);
  int getSector(int id);
  int getSide(int id);
  int getPanel(int id);
  int getSlat(int id);

  int getPanelSeq(int id);
  const char *getPanelChar(int id);

  int getCrate(int id);
  int getSlot(int id);
  int getSlot(int i, int id);
  int getChannel(int i, int id);

  float getCableLength(int i, int id);

  int getHvCrate(int id);
  int getHvBoard(int id);
  int getHvChannel(int id);

  void setFemMapName(PHString string) { setCalibName(string); } 

  // FEM board/channel validity [true: valid, false: invalid]
  void setSlotValidity(int icrate, int islot, bool v)
    {slot_validity[icrate][islot] = v;}
  bool getSlotValidity(int icrate, int islot)
    {return slot_validity[icrate][islot];} 
  
  void setChannelValidity(int icrate, int islot, int ichannel, bool v)
    {channel_validity[icrate][islot][ichannel] = v;}
  bool getChannelValidity(int icrate, int islot, int ichannel)
    {return channel_validity[icrate][islot][ichannel];}
  
  // Fetch information from Objy Database 
  PHBoolean fetch();
  PHBoolean fetch(const int run);

  // Update information to Objy Database 
  PHBoolean update(const int beginrun, const int endrun);
  PHBoolean update(PHTimeStamp tStart, PHTimeStamp tStop);

  // Fetch FEM map information from Table
  PHBoolean fetchFEMmapTable(PHCompositeNode *top);
  // Write FEM map information to Table
  PHBoolean writeFEMmapTable(PHCompositeNode *top);

  // Fetch information from default ASCII file
  PHBoolean fetchFromFile();
  // Fetch information from an ASCII file "filename" 
  PHBoolean fetchFromFile(const char *filename);
  PHBoolean fetchFromFile(const char *filename, const char *cablemap);
  PHBoolean fetchFromFile(const char *filename, const char *cablemap, const char *hvmap);
  // Write information to an ASCII file "filename" 
  PHBoolean writeToFile(const char* filename);

  void print();
  void print(int id);
private:
  PdbIndex  *slatid;       // Slat ID (0-959)

  //TOF Geometrical Address
  PdbIndex  *arm;           // West=0, East=1
  PdbIndex  *sector;        // Sector (0-1)
  PdbIndex  *side;          // South=0,North=1
  PdbIndex  *panel;         // Panel  (0-3)
  PdbIndex  *slat;          // Slat   (0-95)

  int  panel_seq;           // seq.number Panel (0-9)
  char panel_char[2];       // character  Panel (A-J)

  // TOF FEM Address
  PdbIndex  *crate;         // FEM crate ID (0-7)
  PdbIndex  *slot0;         // FEM slot ID (0-15)[Lower PMT]
  PdbIndex  *slot1;         // FEM slot ID (0-15)[Upper PMT]
  PdbIndex  *channel0;      // FEM channel (0-7) [Lower PMT]
  PdbIndex  *channel1;      // FEM channel (0-7) [Upper PMT]

  PHPointerList<PdbIndex> TofIndex; 

  int  map_slatid[TOF_NCRATE][TOF_NBOARD][TOF_NCHANNEL];
  int  map_crate[TOF_NSLAT_ALL];
  int  map_slot[2][TOF_NSLAT_ALL];
  int  map_ch[2][TOF_NSLAT_ALL];

  float map_cable[2][TOF_NSLAT_ALL];

  int map_hvcrate[TOF_NSLAT_ALL];
  int map_hvboard[TOF_NSLAT_ALL];
  int map_hvch[TOF_NSLAT_ALL];
  int hvmap_slatid[TOF_HV_NCRATE][TOF_HV_NBOARD][TOF_HV_NCHANNEL];

  // FEM board/chanel validity
  bool slot_validity[TOF_NCRATE][TOF_NBOARD];
  bool channel_validity[TOF_NCRATE][TOF_NBOARD][TOF_NCHANNEL];

};

#endif /* __TOFADDRESSOBJECT_HH__ */
