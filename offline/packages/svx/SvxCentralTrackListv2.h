// ========================
// FILE: SvxCentralTrackListv2.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV2_H
#define __SVXCENTRALTRACKLISTV2_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv2.h"

class SvxCentralTrackListv2 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv2();
  SvxCentralTrackListv2(const SvxCentralTrackListv2&);
  SvxCentralTrackListv2& operator=(const SvxCentralTrackListv2&);
  virtual ~SvxCentralTrackListv2();

  SvxCentralTrackListv2* clone() const;

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
  SvxCentralTrackv2* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv2* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv2& dest) const;

  ClassDef(SvxCentralTrackListv2,1)

};

#endif /* __SVXSEGMENTLISTV1_H */

