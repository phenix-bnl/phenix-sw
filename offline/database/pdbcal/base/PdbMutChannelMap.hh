//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2001
//
//  Declaration of class PdbMutMap
//
//  Purpose: Store MuTr Channel Map 
//
//  Description:
//
//  Author: DongJo Kim(djkim@bnl.gov)
//-----------------------------------------------------------------------------
#ifndef __PDBMUTCHANNELMAP_HH__
#define __PDBMUTCHANNELMAP_HH__

#include "PdbCalChan.hh"
static const int NumberOfChannel=128;    
// Max # of strips(at each cathode plane per HalfOctant) is 187

class PdbMutChannelMap : public PdbCalChan {

public:
  PdbMutChannelMap();
//  PdbMutChannelMap (const PdbMutChannelMap& rhs);
  ~PdbMutChannelMap();

  virtual void print() const;

  int getPacketID  () const {return  PacketID;}

  int get_Arm        (int index) const {return        ArmNum[index];} 
  int get_Station    (int index) const {return    StationNum[index];}
  int get_Octant     (int index) const {return     OctantNum[index];}
  int get_HalfOctant (int index) const {return HalfOctantNum[index];}
  int get_Gap        (int index) const {return        GapNum[index];}
  int get_Plane      (int index) const {return      PlaneNum[index];}
  int get_Strip      (int index) const {return      StripNum[index];}
  int get_Flag       (int index) const {return          Flag[index];}

  void setPacketID  (int temp){PacketID   =temp;}

  void set_Arm        (int index,int temp){ ArmNum[index]       =temp;}
  void set_Station    (int index,int temp){ StationNum[index]   =temp;}
  void set_Octant     (int index,int temp){ OctantNum[index]    =temp;}
  void set_HalfOctant (int index,int temp){ HalfOctantNum[index]=temp;}
  void set_Gap        (int index,int temp){ GapNum[index]       =temp;}
  void set_Plane      (int index,int temp){ PlaneNum[index]     =temp;}
  void set_Strip      (int index,int temp){ StripNum[index]     =temp;}
  void set_Flag       (int index,int temp){    Flag[index]      =temp;}


private:
  int PacketID;
  int ArmNum[NumberOfChannel]; 
  int StationNum[NumberOfChannel];
  int OctantNum[NumberOfChannel];
  int HalfOctantNum[NumberOfChannel];
  int GapNum[NumberOfChannel];
  int PlaneNum[NumberOfChannel];
  int StripNum[NumberOfChannel];
  int Flag[NumberOfChannel];

  ClassDef(PdbMutChannelMap,1);
};

#endif /* __PDBMUTCHANNELMAP_HH__ */
