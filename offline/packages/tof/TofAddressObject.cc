//  Implementation of class TofAddressObject
//
//  Author: Akio Kiyomichi (Univ.of Tsukuba)
//
//  History: 04/10/00  A.Kiyomichi  First Version
//           04/26/00  A.Kiyomichi  Release with ObjyDB access
//           05/27/00  A.Kiyomichi  Add dTofFEMmap table I/O
//           06/05/00  A.Kiyomichi  Add getPmt()
//           06/15/00  A.Kiyomichi  Add cable map
//           07/28/00  A.Kiyomichi  Add FEM board/channel validity
//           08/16/00  A.Kiyomichi  Change default database file [GEANT -> Run]
//           02/26/01  A.Kiyomichi  Speed up getSlatID(int,int,int)
//           06/09/01  H.Masui      Add hvmap_slatid[CRATE][BORARD][CHANNEL]
//                                   &  getSlatIDfromHV()
//           11/29/01  A.Kiyomichi  Add getSlatIDfromPISA()
//           12/18/01  A.Kiyomichi  Bug fix for dTofFEMmap table I/O

#include "PdbCalBank.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"

#include "PHIODataNode.h"
#include "PHTable.hh"

#include "dTofFEMmapWrapper.h"

#include "TofAddressObject.hh"

#include <cstdlib>
#include <fstream>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

//========================
// member functions
//========================
// Constructor
TofAddressObject::TofAddressObject()
{
  // initialize
  slatid = new PdbIndex(0, 959, -1, "SLATID");
  arm = new PdbIndex(0, 1, 1, "ARM");
  sector = new PdbIndex(0, 1, -1, "SECTOR");
  side = new PdbIndex(0, 1, -1, "SIDE");
  panel = new PdbIndex(0, 3, -1, "PANEL");
  slat = new PdbIndex(0, 95, -1, "SLAT");
  panel_seq = -1;
  strcpy(panel_char , "Z");
  crate = new PdbIndex(0, 7, -1, "CRATE");
  slot0 = new PdbIndex(0, 15, -1, "SLOT0");
  slot1 = new PdbIndex(0, 15, -1, "SLOT1");
  channel0 = new PdbIndex(0, 15, -1, "CHANNEL0");
  channel1 = new PdbIndex(0, 15, -1, "CHANNEL1");

  TofIndex.append(slatid);    // SLATID = 0;
  TofIndex.append(crate);     // CRATE = 1
  TofIndex.append(slot0);     // SLOT_0 = 2
  TofIndex.append(slot1);     // SLOT_1 = 3
  TofIndex.append(channel0);  // CHANNEL_0 = 4
  TofIndex.append(channel1);  // CHANNEL_1 = 5


  setDebugLevel(0);
  setFemMapName("map.tof.femmap0"); // for Run
  //setFemMapName("map.tof.geantfemmap0"); // for GEANT
  setBankNumber(7100);
  setBankID(BankNumber);

  Tsearch.setToSystemTime();
  //setTimeStamp(PHTimeStamp(2000,3,10,0,0,0));

  iFlag = -1; // don't have any data yet

  // Initialize
  for (int i = 0; i < TOF_NSLAT_ALL;i++)
    {
      map_crate[i] = -1;
      map_slot[0][i] = -1;
      map_slot[1][i] = -1;
      map_ch[0][i] = -1;
      map_ch[1][i] = -1;

      map_cable[0][i] = 0.0;
      map_cable[1][i] = 0.0;

      map_hvcrate[i] = -1;
      map_hvboard[i] = -1;
      map_hvch[i] = -1;
    }
  for (int i = 0; i < TOF_NCRATE; i++)
    {
      for (int j = 0; j < TOF_NBOARD; j++)
        {
          slot_validity[i][j] = false;
          for (int k = 0; k < TOF_NCHANNEL; k++)
            {
              channel_validity[i][j][k] = false;
            }
        }
    }
}
// Destructor
TofAddressObject::~TofAddressObject()
{
  delete slatid;
  delete arm;
  delete sector;
  delete side;
  delete panel;
  delete slat;
  delete crate;
  delete slot0;
  delete slot1;
  delete channel0;
  delete channel1;
  return;
}

PHBoolean
TofAddressObject::setSlatID(int id)
{
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return False;
    }
  // TOF
  slatid->setValue(id);
  arm->setValue(getArm(id));
  sector->setValue(getSector(id));
  side->setValue(getSide(id));
  panel->setValue(getPanel(id));
  slat->setValue(getSlat(id));
  panel_seq = getPanelSeq(id);
  strcpy(panel_char , getPanelChar(id));
  // FEM
  crate->setValue(getCrate(id));
  slot0->setValue(getSlot(0, id));
  slot1->setValue(getSlot(1, id));
  channel0->setValue(getChannel(0, id));
  channel1->setValue(getChannel(1, id));
  return True;
}

PHBoolean
TofAddressObject::setHardMap(int id, int icrate,
			     int islot0, int islot1,
			     int ichannel0, int ichannel1)
{
  iFlag = 0;
  setFemMap(id, icrate, islot0, islot1, ichannel0, ichannel1);
  return True;
}

PHBoolean
TofAddressObject::setFemMap(int id, int icrate,
			    int islot0, int islot1,
			    int ichannel0, int ichannel1)
{
  iFlag = 0; // data setting
  // Set FEM map
  map_slatid[icrate][islot0][ichannel0] = id;
  map_slatid[icrate][islot1][ichannel1] = id;
  map_crate[id] = icrate;
  map_slot[0][id] = islot0;
  map_slot[1][id] = islot1;
  map_ch[0][id] = ichannel0;
  map_ch[1][id] = ichannel1;

  // Set board/channel validity
  setSlotValidity(icrate, islot0, true);
  setSlotValidity(icrate, islot1, true);
  setChannelValidity(icrate, islot0, ichannel0, true);
  setChannelValidity(icrate, islot1, ichannel1, true);

  // Set current indices
  setSlatID(id);
  return True;
}

PHBoolean
TofAddressObject::setCableLengthMap(int id, float length0,
    float length1)
{
  // Set cable map
  map_cable[0][id] = length0;
  map_cable[1][id] = length1;

  return True;
}

PHBoolean
TofAddressObject::setHvMap(int id, int hvcrate,
                                     int hvboard, int hvchannel)
{
  // Set HV map
  map_hvcrate[id] = hvcrate;
  map_hvboard[id] = hvboard;
  map_hvch[id] = hvchannel;
  hvmap_slatid[hvcrate][hvboard][hvchannel] = id;

  return True;
}

PHBoolean
TofAddressObject::setHard(int icrate, int islot, int ichannel)
{
  int id = -1;
  id = getSlatID(icrate, islot, ichannel);
  if (id < 0 || id >= TOF_NSLAT_ALL)
    return False;

  slatid->setValue(id);
  arm->setValue(getArm(id));
  sector->setValue(getSector(id));
  side->setValue(getSide(id));
  panel->setValue(getPanel(id));
  slat->setValue(getSlat(id));
  panel_seq = getPanelSeq(id);
  strcpy(panel_char , getPanelChar(id));

  crate->setValue(icrate);
  slot0->setValue(getSlot(0, id));
  slot1->setValue(getSlot(1, id));
  channel0->setValue(getChannel(0, id));
  channel1->setValue(getChannel(1, id));
  return True;
}

PHBoolean
TofAddressObject::setSoft(int iarm, int isector, int iside,
                                    int ipanel, int islat)
{
  int id = -1;
  id = getSlatID(iarm, isector, iside, ipanel, islat);
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return False;
    }

  slatid->setValue(id);
  arm->setValue(1);
  sector->setValue(isector);
  side->setValue(iside);
  panel->setValue(ipanel);
  slat->setValue(islat);

  panel_seq = getPanelSeq(id);
  strcpy(panel_char , getPanelChar(id));

  crate->setValue(getCrate(id));
  slot0->setValue(getSlot(0, id));
  slot1->setValue(getSlot(1, id));
  channel0->setValue(getChannel(0, id));
  channel1->setValue(getChannel(1, id));
  return True;
}

PHBoolean
TofAddressObject::setSoft(int ipanel_seq, int islat)
{
  int id = -1;
  id = getSlatID(ipanel_seq, islat);
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return False;
    }
  setSlatID(id);

  return True;
}

PHBoolean
TofAddressObject::setSoft(const char *ipanel_char, int islat)
{
  int id = -1;
  id = getSlatID(*ipanel_char, islat);
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return False;
    }
  setSlatID(id);

  return True;
}

//====================================================================
//  Simple conversion
//====================================================================
//   /* old mTofSetGeo.c */
//   n_panel = i/NSLAT_PANEL;
//   slat  = i%NSLAT_PANEL;
//   switch(n_panel + 1)
//     {
//     // 1-10 : panel number in PISA
//     case  1: sector  =  1;  side  =  0;  panel  =  3;  break;
//     case  2: sector  =  1;  side  =  0;  panel  =  2;  break;
//     case  3: sector  =  1;  side  =  0;  panel  =  1;  break;
//     case  4: sector  =  1;  side  =  0;  panel  =  0;  break;
//     case  5: sector  =  1;  side  =  1;  panel  =  0;  break;
//     case  6: sector  =  1;  side  =  1;  panel  =  1;  break;
//     case  7: sector  =  1;  side  =  1;  panel  =  2;  break;
//     case  8: sector  =  1;  side  =  1;  panel  =  3;  break;
//     case  9: sector  =  0;  side  =  0;  panel  =  0;  break;
//     case 10: sector  =  0;  side  =  1;  panel  =  0;  break;
//     case 11: sector  = -1;  side  = -1;  panel  = -1;  break;
//     default: sector  = -1;  side  = -1;  panel  = -1;
//     }

int
TofAddressObject::getSlatID(int iarm, int isector, int iside,
                                int ipanel, int islat)
{
  int id = -1, ipanel_seq = -1;
  if (isector == 1 && iside == 0 && ipanel == 3)
    {
      ipanel_seq = 0;
    }
  if (isector == 1 && iside == 0 && ipanel == 2)
    {
      ipanel_seq = 1;
    }
  if (isector == 1 && iside == 0 && ipanel == 1)
    {
      ipanel_seq = 2;
    }
  if (isector == 1 && iside == 0 && ipanel == 0)
    {
      ipanel_seq = 3;
    }
  if (isector == 1 && iside == 1 && ipanel == 0)
    {
      ipanel_seq = 4;
    }
  if (isector == 1 && iside == 1 && ipanel == 1)
    {
      ipanel_seq = 5;
    }
  if (isector == 1 && iside == 1 && ipanel == 2)
    {
      ipanel_seq = 6;
    }
  if (isector == 1 && iside == 1 && ipanel == 3)
    {
      ipanel_seq = 7;
    }
  if (isector == 0 && iside == 0 && ipanel == 0)
    {
      ipanel_seq = 8;
    }
  if (isector == 0 && iside == 1 && ipanel == 0)
    {
      ipanel_seq = 9;
    }

  if (ipanel_seq < 0)
    {
      cerr << "TofAddressObject ERROR getSlatID: Input values are wrong." 
	   << endl;
      return -1;
    }
  id = getSlatID(ipanel_seq, islat);
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  return id;
}

int
TofAddressObject::getSlatID(int ipanel_seq, int islat)
{
  int id = -1;
  id = ipanel_seq * TOF_NSLAT_PANEL + islat;
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  return id;
}

int
TofAddressObject::getSlatID(const char *ipanel_char, int islat)
{
  static char a[] = "ABCDFGHIEJ ";
  int i = -1;

  // Put a sentinel value at the end of the list and search for it.
  a[10] = *ipanel_char;
  while (a[++i] != *ipanel_char)
  {}
  if (i == 10)
    {
      return -1;
    }

  return getSlatID(i, islat);
}

int
TofAddressObject::getSlatID(int icrate, int islot, int ichannel)
{
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getSlatID: TofDAO not initialized." 
	   << endl;
      return -1;
    }
  int id = -1;

  if (icrate < 0 || icrate >= TOF_NCRATE)
    {
      return -1;
    }
  if (islot < 0 || islot >= TOF_NBOARD)
    {
      return -1;
    }
  if (ichannel < 0 || ichannel >= TOF_NCHANNEL)
    {
      return -1;
    }
  id = map_slatid[icrate][islot][ichannel];
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }

  return id;
}

int
TofAddressObject::getPmt(int icrate, int islot, int ichannel)
{
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getPmt: TofDAO not initialized." << endl;
      return -1;
    }
  int id, pmt = -1;
  id = getSlatID(icrate, islot, ichannel);
  if (getChannel(0, id) == ichannel)
    {
      pmt = 0; // Lower PMT
    }
  if (getChannel(1, id) == ichannel)
    {
      pmt = 1; // Upper PMT
    }

  return pmt;
}

int
TofAddressObject::getSlatIDfromHV(int icrate, int iboard, int ichannel)
{
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getSlatID: TofDAO not initialized." 
	   << endl;
      return -1;
    }
  int id = -1;

  if (icrate < 0 || icrate >= TOF_HV_NCRATE)
    {
      return -1;
    }
  if (iboard < 0 || iboard >= TOF_HV_NBOARD)
    {
      return -1;
    }
  if (ichannel < 0 || ichannel >= TOF_HV_NCHANNEL)
    {
      return -1;
    }
  id = hvmap_slatid[icrate][iboard][ichannel];
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }

  return id;
}

int
TofAddressObject::getSlatIDfromPISA(int ipanel, int column, int pslat){
  //+--------------------------------------------------------+
  //|      Conversion slat ID from PISA-TOF ghit data        |
  //|           panel,column,pslat   -->   slatid            |
  //|    [same as tofghit_slatid in mTofGhitRawModule.C]     |
  //+--------------------------------------------------------+
  int id, islat;
  if     ((column % 2 == 0)&&(pslat == 0))
    {
      islat =  0 + (column - 1)/2;
    } 
  else if ((column % 2 == 1)&&(pslat == 2))
    {
      islat = 16 + (column - 1)/2;
    } 
  else if ((column % 2 == 0)&&(pslat == 1))
    {
      islat = 32 + (column - 1)/2;
    } 
  else if ((column % 2 == 1)&&(pslat == 1))
    {
      islat = 48 + (column - 1)/2;
    } 
  else if ((column % 2 == 0)&&(pslat == 2))
    {
      islat = 64 + (column - 1)/2;
    } 
  else if ((column % 2 == 1)&&(pslat == 0))
    {
      islat = 80 + (column - 1)/2;
    } 
  else 
    {
      islat = -1000;
    }
  id = islat + (ipanel - 1)*TOF_NSLAT_PANEL;
  return(id);
}

// TOF Map
int
TofAddressObject::getArm(int id)
{
  // Gets Software ARM from slatid
  // All TOF panels are in East Arm
  return 1;
}

int
TofAddressObject::getSector(int id)
{
  // Gets Software SECTOR from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int isector = -1;
  switch (getPanelSeq(id))
    {
    case 0:
      isector = 1;
      break;
    case 1:
      isector = 1;
      break;
    case 2:
      isector = 1;
      break;
    case 3:
      isector = 1;
      break;
    case 4:
      isector = 1;
      break;
    case 5:
      isector = 1;
      break;
    case 6:
      isector = 1;
      break;
    case 7:
      isector = 1;
      break;
    case 8:
      isector = 0;
      break;
    case 9:
      isector = 0;
      break;
    default:
      isector = -1;
    }

  return isector;
}

int
TofAddressObject::getSide(int id)
{
  // Gets Software SIDE from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int iside = -1;
  switch (getPanelSeq(id))
    {
    case 0:
      iside = 0;
      break;
    case 1:
      iside = 0;
      break;
    case 2:
      iside = 0;
      break;
    case 3:
      iside = 0;
      break;
    case 4:
      iside = 1;
      break;
    case 5:
      iside = 1;
      break;
    case 6:
      iside = 1;
      break;
    case 7:
      iside = 1;
      break;
    case 8:
      iside = 0;
      break;
    case 9:
      iside = 1;
      break;
    default:
      iside = -1;
    }
  return iside;
}

int
TofAddressObject::getPanel(int id)
{
  // Gets Software PANEL from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int ipanel = -1;
  switch (getPanelSeq(id))
    {
    case 0:
      ipanel = 3;
      break;
    case 1:
      ipanel = 2;
      break;
    case 2:
      ipanel = 1;
      break;
    case 3:
      ipanel = 0;
      break;
    case 4:
      ipanel = 0;
      break;
    case 5:
      ipanel = 1;
      break;
    case 6:
      ipanel = 2;
      break;
    case 7:
      ipanel = 3;
      break;
    case 8:
      ipanel = 0;
      break;
    case 9:
      ipanel = 0;
      break;
    default:
      ipanel = -1;
    }

  return ipanel;
}

const char * 
TofAddressObject::getPanelChar(int id)
{
  // Gets Software PANEL Character from slatid
  switch (getPanelSeq(id))
    {
    case 0:
      return "A";
    case 1:
      return "B";
    case 2:
      return "C";
    case 3:
      return "D";
    case 4:
      return "F";
    case 5:
      return "G";
    case 6:
      return "H";
    case 7:
      return "I";
    case 8:
      return "E";
    case 9:
      return "J";
    default:
      return "Z";
    }
}

int
TofAddressObject::getPanelSeq(int id)
{
  // Gets Software PANEL Sequential # from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int ipanel_seq;
  ipanel_seq = id / TOF_NSLAT_PANEL;

  return ipanel_seq;
}

int
TofAddressObject::getSlat(int id)
{
  // Gets Software SLAT from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int islat = -1;
  islat = id % TOF_NSLAT_PANEL;

  return islat;
}

// FEM Map
int
TofAddressObject::getCrate(int id)
{
  // Gets Hardware FEM CRATE from slatid
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getCrate: TofDAO not initialized."
	   << endl;
      return -1;
    }
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int icrate = -1;
  icrate = map_crate[id];

  return icrate;
}

int
TofAddressObject::getSlot(int id)
{
  // Gets Hardware FEM SLOT from slatid
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getSlot: TofDAO not initialized." 
	   << endl;
      return -1;
    }
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int islot = -1;
  islot = map_slot[0][id];

  return islot;
}

int
TofAddressObject::getSlot(int i, int id)
{
  // Gets Hardware FEM SLOT from slatid
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getSlot: TofDAO not initialized." 
	   << endl;
      return -1;
    }
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int islot = -1;
  islot = map_slot[i][id];

  return islot;
}

int
TofAddressObject::getChannel(int i, int id)
{
  // Gets Hardware FEM CHANNEL from slatid and pmtid
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR getChannel: TofDAO not initialized." 
	   << endl;
      return -1;
    }
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int ichannel = -1;
  ichannel = map_ch[i][id];

  return ichannel;
}

// Cable Length map
float
TofAddressObject::getCableLength(int i, int id)
{
  // Gets Hardware Cable Length from slatid and pmtid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  float length = map_cable[i][id];

  return length;
}

// HV map
int
TofAddressObject::getHvCrate(int id)
{
  // Gets Hardware HV Crate from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int hvcrate = -1;
  hvcrate = map_hvcrate[id];

  return hvcrate;
}

int
TofAddressObject::getHvBoard(int id)
{
  // Gets Hardware HV Board from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int hvboard = -1;
  hvboard = map_hvboard[id];

  return hvboard;
}

int
TofAddressObject::getHvChannel(int id)
{
  // Gets Hardware HV Channel from slatid
  if (id < 0 || id >= TOF_NSLAT_ALL)
    {
      return -1;
    }
  int hvchannel = -1;
  hvchannel = map_hvch[id];

  return hvchannel;
}

// Fetch information from Objy Database
PHBoolean
TofAddressObject::fetch()
{
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();

  if (application->startRead())
    {
      PHTimeStamp tSearch = Tsearch;
      PdbBankID bankID(BankNumber);
      const char *calname = CalibName.getString();

      int length = TofIndex.length();
      int total = length * TOF_NSLAT_ALL;
      if (Debug > 1)
        {
          cout << "  length = " << length << "  n_slat = " << TOF_NSLAT_ALL;
          cout << "  total = " << total << endl;
        }
      PdbCalBank *tofBank =
        bankManager->fetchBank("PdbIndexBank", bankID, calname, tSearch);

      PdbIndex *index;
      PdbIndex *thisindex;

      if (tofBank)
        {
          for (int i = 0; i < TOF_NSLAT_ALL ; i++)
            {
              TofIndex.clearAndDestroy();
              for (int j = 0; j < length; j++)
                {
                  int k = i * length + j;
                  index = (PdbIndex*) & (tofBank->getEntry(k));
                  thisindex = new PdbIndex();
                  *thisindex = *index;
                  TofIndex.append(thisindex);
                }
              slatid = TofIndex[SLATID];
              crate = TofIndex[CRATE];
              slot0 = TofIndex[SLOT_0];
              slot1 = TofIndex[SLOT_1];
              channel0 = TofIndex[CHANNEL_0];
              channel1 = TofIndex[CHANNEL_1];

              int id = slatid->getValue();
              int icrate = crate->getValue();
              int islot0 = slot0->getValue();
              int islot1 = slot1->getValue();
              int ichannel0 = channel0->getValue();
              int ichannel1 = channel1->getValue();
              setFemMap(id, icrate, islot0, islot1, ichannel0, ichannel1);
            } // for(i = 0; i < n_slat; i++){
	  delete tofBank;
        } // if(tofBank){
    } // if(application->startRead()){

  return True;
}

PHBoolean TofAddressObject::fetch(const int run)
{
  Tsearch = getTimeStamp(run);

  return fetch();
}

// Update information to Objy Database
PHBoolean TofAddressObject::update(const int beginrun, const int endrun)
{
  PHTimeStamp start = getTimeStamp(beginrun);
  PHTimeStamp stop  = getTimeStamp(endrun);

  return update(start, stop);
}

PHBoolean
TofAddressObject::update(PHTimeStamp tStart, PHTimeStamp tStop)
{
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class.
  PdbApplication *application = bankManager->getApplication();
  // Open the federation in update mode
  cout << "Opening FD in update mode..  " << CalibName.getString() << endl;

  if (application->startUpdate())
    {
      PdbBankID bankID(BankNumber);
      const char *descrip = "TOF Address (FEM) map";
      const char *calname = CalibName.getString();

      PdbCalBank *tofBank =
        bankManager->createBank("PdbIndexBank", bankID, descrip,
                                tStart, tStop, calname);

      tofBank->setUserName(UserName);

      int length = TofIndex.length();
      int total = length * TOF_NSLAT_ALL;

      tofBank->setLength(total);
      PdbIndex *index;
      for (int i = 0; i < TOF_NSLAT_ALL; i++)
        {
          setSlatID(i);
          for (int j = 0; j < length; j++)
            {
              int k = i * length + j;
              index = (PdbIndex*) & (tofBank->getEntry(k));
              *index = *(TofIndex[j]);
            }
        }
      application->commit();
    }

  return True;
}

// Fetch FEM map information from Table
PHBoolean
TofAddressObject::fetchFEMmapTable(PHCompositeNode *top)
{
  PHNodeIterator *jjj;
  TableNode_t *d;

  // Extract the data from the dTofFEMmap
  dTofFEMmapWrapper *TofFEMmapWrap;
  jjj = new PHNodeIterator(top);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode", "dTofFEMmap"));
  if (!d)
    {
      cerr << "  Error " << endl;
      exit(1);
    }
  else
    {
      TofFEMmapWrap = static_cast<dTofFEMmapWrapper*>(d->getData());
      if (!TofFEMmapWrap)
        {
          cerr << " Error" << endl;
          exit(1);
        }
    }
  delete jjj;

  for (unsigned int i = 0; i < TofFEMmapWrap->RowCount() ;i++)
    {
      int id = TofFEMmapWrap->get_slatid(i);
      int icrate = TofFEMmapWrap->get_crate(i);
      int islot0 = TofFEMmapWrap->get_slot(i);
      int islot1 = TofFEMmapWrap->get_slot(i);
      int ichannel0 = TofFEMmapWrap->get_ch(0, i);
      int ichannel1 = TofFEMmapWrap->get_ch(1, i);
      if (id >= 0 && id < TOF_NSLAT_ALL)
        {
          setFemMap(id, icrate, islot0, islot1, ichannel0, ichannel1);
        }
    }

  return True;
}

// Write FEM map information to Table
PHBoolean
TofAddressObject::writeFEMmapTable(PHCompositeNode *top)
{
  PHNodeIterator *jjj;
  TableNode_t *d;

  // Extract the data from the dTofFEMmap
  dTofFEMmapWrapper *TofFEMmapWrap;
  jjj = new PHNodeIterator(top);
  d = static_cast<TableNode_t*>(jjj->findFirst("PHIODataNode", "dTofFEMmap"));
  if (!d)
    {
      cerr << "  Error " << endl;
      exit(1);
    }
  else
    {
      TofFEMmapWrap = static_cast<dTofFEMmapWrapper*>(d->getData());
      if (!TofFEMmapWrap)
        {
          cerr << " Error" << endl;
          exit(1);
        }
      TofFEMmapWrap->SetMaxRowCount(TOF_NSLAT_ALL);
    }
  delete jjj;

  for (int i = 0; i < TOF_NSLAT_ALL;i++)
    {
      short id = i;
      TofFEMmapWrap->set_slatid(i, id);
      short icrate = (short)getCrate(id);
      TofFEMmapWrap->set_crate(i, icrate);
      short islot0 = (short)getSlot(id);
      TofFEMmapWrap->set_slot(i, islot0);
      short ichannel0 = (short)getChannel(0, id);
      TofFEMmapWrap->set_ch(0, i, ichannel0);
      short ichannel1 = (short)getChannel(1, id);
      TofFEMmapWrap->set_ch(1, i, ichannel1);
    }
  TofFEMmapWrap->SetRowCount(TOF_NSLAT_ALL);

  return True;
}

// Fetch information from default ASCII file
PHBoolean
TofAddressObject::fetchFromFile()
{
  const char *tofmapdb = "toffemmap.txt";

  return TofAddressObject::fetchFromFile(tofmapdb);
}

// Fetch information from an ASCII file "filename"
PHBoolean
TofAddressObject::fetchFromFile(const char *filename)
{
  ifstream file;
  file.open(filename);
  int tmp[11];

  if (!file)
    {
      cerr << "TofAddressObject::fetchFromFile ERROR:" << endl;
      cerr << "  Can not open " << filename << " file." << endl;
      cerr << endl;
      return False;
    }
  else
    {
      for (int i = 0; i < TOF_NSLAT_ALL; i++)
        {
          file >> tmp[0] >> tmp[1] >> tmp[2] >> tmp[3] >> tmp[4] 
	       >> tmp[5] >> tmp[6] >> tmp[7] >> tmp[8] >> tmp[9] 
	       >> tmp[10];
          if (file.eof())
	    {
	      break;
	    }
          int id = tmp[ 0];
          int icrate = tmp[ 6];
          int islot0 = tmp[ 7];
          int islot1 = tmp[ 8];
          int ichannel0 = tmp[ 9];
          int ichannel1 = tmp[10];
          setFemMap(id, icrate, islot0, islot1, ichannel0, ichannel1);
        }
      cout << "Fetch FEE information from " << filename 
	   << "  [ASCII file]" << endl;
    }

  return True;
}

PHBoolean 
TofAddressObject::fetchFromFile(const char *filename, const char *cablemap)
{
  ifstream file;
  file.open(cablemap);
  int slatid, slat;
  float length0, length1;
  char panel[2];

  if (!file)
    {
      cerr << "TofAddressObject::fetchFromFile ERROR:" << endl;
      cerr << "  Can not open " << cablemap << " file." << endl;
      cerr << endl;
      return False;
    }
  else
    {
      for (int i = 0; i < TOF_NSLAT_ALL; i++)
        {
          file >> panel >> slat >> length0 >> length1;
          slatid = getSlatID(panel, slat);
          setCableLengthMap(slatid, length0, length1);
        }
      cout << "Fetch Cable information from " << filename << endl;
    }

  return TofAddressObject::fetchFromFile(filename);
}

PHBoolean
TofAddressObject::fetchFromFile(const char *filename, 
				const char *cablemap,
				const char *hvmap)
{
  ifstream file;
  file.open(hvmap);
  int slatid, slat, hvcrate, hvboard, hvchannel;
  char panel[2];

  if (!file)
    {
      cerr << "TofAddressObject::fetchFromFile ERROR:" << endl;
      cerr << "  Can not open " << hvmap << " file." << endl;
      cerr << endl;
      return False;
    }
  else
    {
      for (int i = 0; i < TOF_NSLAT_ALL; i++)
        {
          file >> panel >> slat >> hvcrate >> hvboard >> hvchannel;
          slatid = getSlatID(panel, slat);
          setHvMap(slatid, hvcrate, hvboard, hvchannel);
        }
      cout << "Fetch HV information from " << filename << endl;
    }

  return TofAddressObject::fetchFromFile(filename, cablemap);
}

// Write information to ASCII file
PHBoolean
TofAddressObject::writeToFile(const char* filename)
{
  ofstream file;
  file.open(filename);
  for (int i = 0; i < TOF_NSLAT_ALL; i++)
    {
      file << "   " << i << "\t" << getArm(i) << "   " << getSector(i)
	   << "   " << getSide(i) << "   " << getPanel(i) << "   " 
	   << getSlat(i) << "\t" << getCrate(i) << "\t" << getSlot(0, i) 
	   << "\t" << getSlot(1, i) << "\t" << getChannel(0, i) << "\t" 
	   << getChannel(1, i) << endl;
    }
  cout << "Write information to " << filename << "  [ASCII file]" << endl;

  return True;
}

//  Print
void
TofAddressObject::print()
{
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR print: TofDAO not initialized." << endl;
    }
  cout << " " << endl;
  cout << "##### TofAddressObject #####" << endl;
  cout << "  SLATID = " << getSlatID() << endl;
  cout << " " << endl;
  cout << "dTofGeo" << endl;
  cout << "  ARM    = " << getArm() << endl;
  cout << "  SECTOR = " << getSector() << endl;
  cout << "  SIDE   = " << getSide() << endl;
  cout << "  PANEL  = " << getPanel() << endl;
  cout << "  SLAT   = " << getSlat() << endl;
  cout << "  panel_seq  = " << getPanelSeq() << endl;
  cout << "  panel_char = " << getPanelChar() << endl;
  cout << " " << endl;
  cout << "dTofFEMmap" << endl;
  cout << "  CRATE  = " << getCrate() << endl;
  cout << "  SLOT[0]= " << getSlot0();
  cout << "\tSLOT[1]= " << getSlot1() << endl;
  cout << "  CH[0]  = " << getChannel0();
  cout << "\tCH[1]  = " << getChannel1() << endl;
  cout << " " << endl;
  cout << "TofIndex.length() = " << TofIndex.length() << endl;
  cout << " " << endl;
  cout << " PdbCal::" << endl;
  cout << "    CalibName  = " << CalibName << endl;
  cout << "    BankNumber = " << BankNumber << endl;
  cout << "    TimeStamp  = " << Tsearch << endl;
}

void
TofAddressObject::print(int id)
{
  if (iFlag != 0)
    {
      cerr << "TofAddressObject ERROR print: TofDAO not initialized." << endl;
    }
  cout << " " << endl;
  cout << "##### TofAddressObject #####" << endl;
  cout << "  SLATID = " << id << endl;
  cout << " " << endl;
  cout << "dTofGeo" << endl;
  cout << "  ARM    = " << getArm(id) << endl;
  cout << "  SECTOR = " << getSector(id) << endl;
  cout << "  SIDE   = " << getSide(id) << endl;
  cout << "  PANEL  = " << getPanel(id) << endl;
  cout << "  SLAT   = " << getSlat(id) << endl;
  cout << "  panel_seq  = " << getPanelSeq(id) << endl;
  cout << "  panel_char = " << getPanelChar(id) << endl;
  cout << " " << endl;
  cout << "dTofFEMmap" << endl;
  cout << "  CRATE  = " << getCrate(id) << endl;
  cout << "  SLOT[0]= " << getSlot(0, id);
  cout << "\tSLOT[1]= " << getSlot(1, id) << endl;
  cout << "  CH[0]  = " << getChannel(0, id);
  cout << "\tCH[1]  = " << getChannel(1, id) << endl;
  cout << " " << endl;
  cout << " PdbCal::" << endl;
  cout << "    CalibName  = " << CalibName << endl;
  cout << "    BankNumber = " << BankNumber << endl;
  cout << "    TimeStamp  = " << Tsearch << endl;
}

