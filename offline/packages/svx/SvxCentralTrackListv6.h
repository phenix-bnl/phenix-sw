// ========================
// FILE: SvxCentralTrackListv6.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV6_H
#define __SVXCENTRALTRACKLISTV6_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv6.h"

class SvxCentralTrackListv6 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv6();
  SvxCentralTrackListv6(const SvxCentralTrackListv6&);
  SvxCentralTrackListv6& operator=(const SvxCentralTrackListv6&);
  virtual ~SvxCentralTrackListv6();

  SvxCentralTrackListv6* clone() const;

  // The "standard PHObject response" functions...
  void Reset();
  int  isValid() const;
  void identify(std::ostream &os=std::cout) const;

  // Implementations of the set/get methods...
  int  get_nCentralTracks () const {return CentralTrack->GetEntries();}

  // Routines to manipulate the cluster array...
  int set_TClonesArraySize(const unsigned int nseg);
  void addCentralTrack          (const unsigned int iseg);
  void removeCentralTrack       (const unsigned int iseg);
  SvxCentralTrackv6* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv6* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv6& dest) const;

  ClassDef(SvxCentralTrackListv6,1)

};

#endif /* __SVXSEGMENTLISTV6_H */

