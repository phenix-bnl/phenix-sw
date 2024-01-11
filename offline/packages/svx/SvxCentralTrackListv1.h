// ========================
// FILE: SvxCentralTrackListv1.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV1_H
#define __SVXCENTRALTRACKLISTV1_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv1.h"

class SvxCentralTrackListv1 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv1();
  SvxCentralTrackListv1(const SvxCentralTrackListv1&);
  SvxCentralTrackListv1& operator=(const SvxCentralTrackListv1&);
  virtual ~SvxCentralTrackListv1();

  SvxCentralTrackListv1* clone() const;

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
  SvxCentralTrackv1* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv1* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv1& dest) const;

  ClassDef(SvxCentralTrackListv1,1)

};

#endif /* __SVXSEGMENTLISTV1_H */

