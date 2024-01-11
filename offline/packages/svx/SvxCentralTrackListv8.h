// ========================
// FILE: SvxCentralTrackListv8.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV8_H
#define __SVXCENTRALTRACKLISTV8_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv8.h"

class SvxCentralTrackListv8 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv8();
  SvxCentralTrackListv8(const SvxCentralTrackListv8&);
  SvxCentralTrackListv8& operator=(const SvxCentralTrackListv8&);
  virtual ~SvxCentralTrackListv8();

  SvxCentralTrackListv8* clone() const;

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
  SvxCentralTrackv8* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv8* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv8& dest) const;

  ClassDef(SvxCentralTrackListv8,1)

};

#endif /* __SVXSEGMENTLISTV7_H */

