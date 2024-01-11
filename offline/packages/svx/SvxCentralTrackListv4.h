// ========================
// FILE: SvxCentralTrackListv4.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV4_H
#define __SVXCENTRALTRACKLISTV4_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv4.h"

class SvxCentralTrackListv4 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv4();
  SvxCentralTrackListv4(const SvxCentralTrackListv4&);
  SvxCentralTrackListv4& operator=(const SvxCentralTrackListv4&);
  virtual ~SvxCentralTrackListv4();

  SvxCentralTrackListv4* clone() const;

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
  SvxCentralTrackv4* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv4* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv4& dest) const;

  ClassDef(SvxCentralTrackListv4,1)

};

#endif /* __SVXSEGMENTLISTV4_H */

