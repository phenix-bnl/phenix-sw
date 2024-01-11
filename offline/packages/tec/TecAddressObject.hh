#ifndef TECADDRESSOBJECT_H
#define TECADDRESSOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// Created by: Sasha Lebedev (ISU) lebedev@iastate.edu 06/10/99   
// Updated by: Sasha Lebedev (ISU) lebedev@iastate.edu 11/15/99   
//                                                                
// Description:  Header for TECAddressObject class
//                                                                
//----------------------------------------------------------------

#include "phool.h"
#include "TObject.h"
#include "TecBasicObject.hh"

class PHTimeStamp;

/**
Class TECAddressObject contains a Map for Hardware/Software indices conversion.
This class is used by TecGeometryObject class. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/software/map/index.html}
@author Sasha Lebedev (ISU) 
<a href="mailto:lebedev@iastate.edu">lebedev@iastate.edu</a>
@memo TEC Address Class
*/
class TecAddressObject : public TecBasicObject, public TObject {

public:

// Indices of TecAddressObject
  enum{ARM, SECTOR, SIDE, PLANE, WIRE, CRATE, SLOT, PSADD, CHANNEL};

private:
 
/// 
  int arm[TECMAPDBMAXENT];
/// 
  int sector[TECMAPDBMAXENT];
/// 
  int plane[TECMAPDBMAXENT];
/// 
  int side[TECMAPDBMAXENT];
/// 
  int fwire[TECMAPDBMAXENT];
/// 
  int lwire[TECMAPDBMAXENT];
/// 
  int crate[TECMAPDBMAXENT];
/// 
  int slot[TECMAPDBMAXENT];
/// 
  int psadd[TECMAPDBMAXENT];
/// 
  int fchannel[TECMAPDBMAXENT];
/// 
  int lchannel[TECMAPDBMAXENT];
/// 
  int packetid[TECMAPDBMAXENT];

///
  int current_arm;
/// 
  int current_sector;
/// 
  int current_side;
/// 
  int current_plane;
/// 
  int current_wire;
/// 
  int current_crate;
/// 
  int current_slot;
/// 
  int current_psadd;
/// 
  int current_channel;
///
  int current_index;
///
  int current_packetid;

public: 
 
/// Constructor
  TecAddressObject(); 
/// Destructor
  ~TecAddressObject(); 

/// Set current indices
  PHBoolean setSoft(int iarm,  int isector, int iside, int iplane, int iwire);

/// Set current indices
  PHBoolean setHard(int icrate, int islot, int ipsadd, int ichannel);

/// Set current indices
  PHBoolean setHard(int ipacketid, int ichannel);

///
  void UseSimulationDatabase();
///
  void UseRealDatabase();

///
  int getArm() { return current_arm; }
///
  int getSector() { return current_sector; }
///
  int getSide() { return current_side; }
///
  int getPlane() { return current_plane; }
///
  int getWire() { return current_wire; }
///
  int getCrate() { return current_crate; }
///
  int getSlot() { return current_slot; }
///
  int getPsadd() { return current_psadd; }
///
  int getChannel() { return current_channel; }
///
  int getIndex() { return current_index; }
///
  int getPacketID() { return current_packetid; }
/// get crate number from packet ID
  int getCrate(int ipacketid);
/// get slot (fem) number from packet ID
  int getSlot(int ipacketid);
/// get first ps address number from packet ID
  int getPsadd1(int ipacketid);
/// get second ps address number from packet ID
  int getPsadd2(int ipacketid);

/** Get software ARM from hardware indices
  @param icrate   crate number
  @param islot    slot number
  @param ipsadd   PS address
  @param ichannel channel number
  @return arm number
*/
  int getArm(int icrate, int islot, int ipsadd, int ichannel);
/// Get software SECTOR from hardware indices
  int getSector(int icrate, int islot, int ipsadd, int ichannel);
/// Get software SIDE from hardware indices
  int getSide(int icrate, int islot, int ipsadd, int ichannel);
/// Get software PLANE from hardware indices
  int getPlane(int icrate, int islot, int ipsadd, int ichannel);
/// Get software WIRE from hardware indices
  int getWire(int icrate, int islot, int ipsadd, int ichannel);
/// Get Packet ID from hardware indices
  int getPacketID(int icrate, int islot, int ichannel);

/** Get hardware CRATE from software indices
  @param iarm     arm number
  @param isector  sector number
  @param iside    side number
  @param iplane   plane number
  @param iwire    wire number
  @return crate number
*/
  int getCrate(int iarm, int isector, int iside, int iplane, int iwire );
/// Get hardware SLOT from software indices
  int getSlot(int iarm, int isector, int iside, int iplane, int iwire);
/// Get hardware PS Address from software indices
  int getPsadd(int iarm, int isector, int iside, int iplane, int iwire);
/// Get hardware CHANNEL from software indices
  int getChannel(int iarm, int isector, int iside, int iplane, int iwire);
/// Get PacketID from software indices
  int getPacketID(int iarm, int isector, int iside, int iplane, int iwire);

/// Get index from software coordinates
  int getIndex(int iarm, int isector, int iside, int iplane) {
      return iside+iplane*TECMAXSIDE+isector*TECMAXSIDE*TECMAXPLANE;
      }

/// Get the name of the object
  const char* getName() {return "TEC Address Object";}

/// Fetch information from Objy Database
  PHBoolean Fetch();

/// Fetch information from default ASCII file
  PHBoolean FetchFromFile();

/// Fetch information from an ASCII file "filename"
  PHBoolean FetchFromFile(const char* filename);

/// Update database from current TAO in memory
  PHBoolean Update(PHTimeStamp* Tbeg, PHTimeStamp* Tend);

PHBoolean HardToSoft(int icrate, int islot, int ich, int& isect, int& iside, int& iplane, int& iwire);

PHBoolean SoftToHard(int isect, int iside, int iplane, int iwire, int& icrate, int& islot, int& ips1, int& ips2, int& ich);

private:

/// Convert from Software indices to hardware
PHBoolean FromSoftToHard();

/// Convert from Hardvare indices to software
PHBoolean FromHardToSoft();
 
}; 

#endif /* TECADDRESSOBJECT_H */ 

