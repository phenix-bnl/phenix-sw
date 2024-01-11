// ======================
// FILE: SvxCentralTrackList.h
// ======================

#ifndef __SVXCENTRALTRACKLIST_HH_
#define __SVXCENTRALTRACKLIST_HH_

#include <iostream>
#include "phool.h"
#include "PHObject.h"

// Container for the SvxCentralTrack.
// This contains a list of all SvxCentralTracks in an event.
// 
// Created by Takashi Hachiya on 11/11/2011.
//


class SvxCentralTrack;

class SvxCentralTrackList : public PHObject
{

 public:
  virtual ~SvxCentralTrackList() {}

  // Standard functions of all inheritors of PHObject classes...
  virtual void Reset()        { PHOOL_VIRTUAL_WARN("Reset"); }
  virtual int isValid() const { PHOOL_VIRTUAL_WARN("isValid"); return 0; }
  virtual void identify(std::ostream &os=std::cout) const {
    os << "identify yourself: virtual SvxCentralTrackList object" << std::endl;
  }


  virtual int  get_nCentralTracks () const 
    {
      std::cout << "SvxCentralTrackList::Error get_nCentralTracks not overridden" << std::endl;
      return 0;
    }

  // Set data members
  virtual int  set_TClonesArraySize(const unsigned int nseg) { PHOOL_VIRTUAL_WARN("set_TCloneArraySize"); return 0;}
  virtual void addCentralTrack     (const unsigned int iseg) { PHOOL_VIRTUAL_WARN("addCentralTrack");}
  virtual void removeCentralTrack  (const unsigned int iseg) { PHOOL_VIRTUAL_WARN("removeCentralTrack");}
  virtual SvxCentralTrack* addCentralTrack(const unsigned int iseg, 
				 const SvxCentralTrack &track) { PHOOL_VIRTUAL_WARN("addCentralTrack");return NULL;}

  // Get data members
  virtual SvxCentralTrack* getCentralTrack(const unsigned int iseg) const 
    {
      std::cout << "Single CentralTrack return not implemented for your version of CentralTrack list" << std::endl;
      return 0;
    }

  virtual SvxCentralTrackList* clone() const
    {
      std::cout << "Clone method not implemented for your version of SvxSegmentList" << std::endl;
      return 0;
    }

  ClassDef(SvxCentralTrackList, 1);
};
#endif /* __SVXCENTRALTRACKLIST_HH_ */
