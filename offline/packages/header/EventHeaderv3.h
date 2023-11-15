#ifndef EVENTHEADERv3_H
#define EVENTHEADERv3_H

#include "EventHeaderv2.h"
#include <iostream>
#include <set>

///
class EventHeaderv3: public EventHeaderv2
{
 public:

  /// ctor
  EventHeaderv3();
  /// dtor
  virtual ~EventHeaderv3() {}

  EventHeaderv3 * clone() const;

  ///  Clear Event
  void Reset();

  /** identify Function from PHObject
      @param os Output Stream 
   */
  void identify(std::ostream& os = std::cout) const;

  void AddBadPacket(const unsigned int ibad);
  int isBadPacket(const unsigned int ibad) const;
  const std::set<unsigned int> *GetBadPacketSet() const {return &badpackets;}
  int GetPacketInfo(const unsigned int id, std::set<unsigned int> &returnset) const;

 protected:

  // this set contained initially the bad packet ids. As of Run11 we store
  // the packetid in the lower 16 bits (which is compatible with the previous
  // storing of the packet id - they do not exceed 2^16 = 64000), the upper
  // 16 bits were used to store the failure code (enum from 1-8 as of time of
  // this writing)
  // as of the Run11 reproduction for the vtx - the failure code is stored in 
  // 8 bits, still compatible to read back the failure code but it leaves the
  // upper 8 bits for additional storage. Here we can store the cell ids for the
  // vtx strips which are not in sync with the others. Those packets will not
  // show up as bad packet in the isBadPacket() method because we do not want
  // to remove those packets wholesale in the reconstruction
  std::set<unsigned int> badpackets;

 private: // prevent doc++ from showing ClassDef
  ClassDef(EventHeaderv3,1)
};

#endif
