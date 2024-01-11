// ========================
// FILE: SvxCentralTrackListv5.h
// ========================

#ifndef __SVXCENTRALTRACKLISTV5_H
#define __SVXCENTRALTRACKLISTV5_H

#include <iostream>
#include "phool.h"
#include "TClonesArray.h"
#include "PHObject.h"

#include "SvxCentralTrackList.h"
#include "SvxCentralTrackv5.h"

class SvxCentralTrackListv5 : public SvxCentralTrackList
{

 public:

  SvxCentralTrackListv5();
  SvxCentralTrackListv5(const SvxCentralTrackListv5&);
  SvxCentralTrackListv5& operator=(const SvxCentralTrackListv5&);
  virtual ~SvxCentralTrackListv5();

  SvxCentralTrackListv5* clone() const;

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
  SvxCentralTrackv5* addCentralTrack (const unsigned int iseg, 
			    const SvxCentralTrack& seg);
  SvxCentralTrackv5* getCentralTrack(const unsigned int iseg) const;

 protected:

  TClonesArray *getCentralTrackObject() const {return CentralTrack;}
  TClonesArray *CentralTrack;

private:
  void copyto(SvxCentralTrackListv5& dest) const;

  ClassDef(SvxCentralTrackListv5,1)

};

#endif /* __SVXSEGMENTLISTV5_H */

