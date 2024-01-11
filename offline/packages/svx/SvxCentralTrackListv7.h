// ========================
// FILE: SvxCentralTrackListv7.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV7_H
#define __SVXCENTRALTRACKLISTV7_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv7.h"

class SvxCentralTrackListv7 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv7();
  SvxCentralTrackListv7(const SvxCentralTrackListv7&);
  SvxCentralTrackListv7& operator=(const SvxCentralTrackListv7&);
  virtual ~SvxCentralTrackListv7();

  SvxCentralTrackListv7* clone() const;

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
  SvxCentralTrackv7* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv7* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv7& dest) const;

  ClassDef(SvxCentralTrackListv7,1)

};

#endif /* __SVXSEGMENTLISTV7_H */

