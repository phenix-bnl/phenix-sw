#ifndef ACCINCLUSIVENANOCUTSV1_H
#define ACCINCLUSIVENANOCUTSV1_H

#include "PHInclusiveNanoCuts.h"
#include "phool.h"

class PHCentralTrack;
class PHCompositeNode;
class PHMuoTracksOut;

//
//  This class inherits from the PHnanoCuts class
//  and implements the global event and track cuts
//  which are appropriate for the Central Track Working 
//  Group.  The structure is largely due to the 
//  original PHCut code from Sasha Lebedev.  It has
//  been adapted to inherit from the PHnanoCuts base
//  class as a mechanism to inforce the interface upon
//  all the cut objects used for the formation of
//  nDSTs.
//                   TKH  3-12-2002
//
//  This routine is a shell into which the CNT should
//  code their actual cutting routines.  It has already
//  satisfied the inheritance from the base class and
//  has "examples" of a pt cut.  Noone forces you to actually
//  *use* a pt cut, but this is at least an example of a
//  cut which any particle could have applied to it.
//  I hope you find coding in here convenient and to your
//  liking.
//
//                  TKH  3-13-2002
//
//  We have experienced a paradigm shift.  To save space and efficiently 
//  share work, the electron, hadron, and hard scattering working groups
//  have decided to make a single "loose-cuts" ndst which would be useful
//  to all these groups.  This generates a new group called CNT.  This is
//  not just the example, but is now intended to be the correct version
//  of the nanoDST cut application which is used in common by EWG, HWG, 
//  and HRD.
//                 TKH 3-29-2002
//
//  Hi gang.  Now we have a new detector called the Aerogel Cherenkov Counter
//  or ACC for short.  The ACC group wants to have a special output stream
//  containing only those central tracks that hit the ACC as well as the ACC_RAW 
//  data structure.  This is most easily accomplished using the cutter classes.
//  Our task in this cutter is simple...we cut on the index of the ACC box being 
//  greater than or equal to zero.  SIMPLE!!
//                
//                 TKH 6-3-2004


class ACCInclusiveNanoCutsv1 : public PHInclusiveNanoCuts 
{
  
public:
  ACCInclusiveNanoCutsv1() {}
  virtual ~ACCInclusiveNanoCutsv1() {}

  //  All the action is here...
  PHBoolean CentralTrackOK(PHCentralTrack* php, const unsigned int itrk);
  
  // These are necessary for all PHObjects:
  int  isValid() const {return(1);}
  void identify(std::ostream &os=std::cout) const {os << "identify yourself: ACCInclusiveNanoCutsv1 Object" << std::endl;}

  // Not used, but must be overridden...
  PHBoolean MuonOK   (PHMuoTracksOut* php, const unsigned int itrk) {return True;}
  PHBoolean GlobalOK            (PHCompositeNode* topnode)          {return True;}
  PHBoolean ParticleCollectionOK(PHCompositeNode* topNode)          {return True;}

protected:
  ClassDef(ACCInclusiveNanoCutsv1,1)

};
#endif


