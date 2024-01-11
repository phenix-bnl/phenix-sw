// ========================
// FILE: SvxCentralTrackListv9.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV9_H
#define __SVXCENTRALTRACKLISTV9_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv9.h"

class SvxCentralTrackListv9 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv9();
  SvxCentralTrackListv9(const SvxCentralTrackListv9&);
  SvxCentralTrackListv9& operator=(const SvxCentralTrackListv9&);
  virtual ~SvxCentralTrackListv9();

  SvxCentralTrackListv9* clone() const;

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
  SvxCentralTrackv9* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv9* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv9& dest) const;

  ClassDef(SvxCentralTrackListv9,1)

};

#endif /* __SVXSEGMENTLISTV7_H */

