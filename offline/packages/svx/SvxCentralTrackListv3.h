// ========================
// FILE: SvxCentralTrackListv3.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV3_H
#define __SVXCENTRALTRACKLISTV3_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv3.h"

class SvxCentralTrackListv3 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv3();
  SvxCentralTrackListv3(const SvxCentralTrackListv3&);
  SvxCentralTrackListv3& operator=(const SvxCentralTrackListv3&);
  virtual ~SvxCentralTrackListv3();

  SvxCentralTrackListv3* clone() const;

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
  SvxCentralTrackv3* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv3* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv3& dest) const;

  ClassDef(SvxCentralTrackListv3,1)

};

#endif /* __SVXSEGMENTLISTV3_H */

