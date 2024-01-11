#ifndef CNTINCLUSIVENANOCUTSV2_H
#define CNTINCLUSIVENANOCUTSV2_H

#include "PHInclusiveNanoCuts.h"
#include "PHEmcConstants.h"

class PHCentralTrack;

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


/**
Class representing a collection of cuts. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html}
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Collection of Cuts
*/
class CNTInclusiveNanoCutsv2 : public PHInclusiveNanoCuts {

public:

  CNTInclusiveNanoCutsv2();
  virtual ~CNTInclusiveNanoCutsv2() { }

  // These are necessary for all PHObjects:
  int  isValid() const {return(1);}
  void identify(std::ostream &os=std::cout) const {os << "identify yourself: CNTInclusiveNanoCutsv2 Object" << std::endl;}

  // These routines override and implement the pure virtual 
  // ones from the base class
  PHBoolean CentralTrackOK(PHCentralTrack* php, const unsigned int itrk);
  PHBoolean GlobalOK  (PHCompositeNode* topnode);

  // These routines regard Particle Species that the EWG group does
  // not put in its output file.
  PHBoolean MuonOK              (PHMuoTracksOut* php,   const unsigned int itrk) {return False;}

  PHBoolean ParticleCollectionOK(PHCompositeNode* topNode) {return True;}

  void Reset();
  void Initialize(PHCompositeNode* topnode=0);

  //------------------------
  // Below this point, we implement variables and functions
  // which are specific to the application of CNT cuts.
  // Since this is an example routine, I have tried to
  // confine the variable choices to those variables
  // which any analysis might consider.  However, you don't
  // *have* to use these...
  //------------------------

  // Sets...
  void set_ptlowcut	(const float val) { ptlowcut = val;} 
  void set_pthighcut	(const float val) { pthighcut = val;} 
  void set_vertexcut	(const float val) { vertexcut = val;} 
  void set_pc3Phicut	(const float val) { pc3Phicut = val;} 
  void set_pc3Zcut	(const float val) { pc3Zcut = val;} 
  void set_emcPhicut	(const float val) { emcPhicut = val;} 
  void set_emcZcut	(const float val) { emcZcut = val;} 
  void set_nx1cut	(const short val) { nx1cut = val;} 
  void set_nx2cut	(const short val) { nx2cut = val;} 
  void set_elowcut	(const int idet, const float val) { elowcut[idet]  = val;} 
  void set_ehighcut	(const int idet, const float val) { ehighcut[idet] = val;} 
  void set_tofcut	(const int idet, const float val) { tofcut[idet]   = val;} 

  // Gets...
  float get_ptlowcut	() const { return ptlowcut;} 
  float get_pthighcut	() const { return pthighcut;} 
  float get_vertexcut	() const { return vertexcut;} 
  float get_pc3Phicut	() const { return pc3Phicut;} 
  float get_pc3Zcut	() const { return pc3Zcut;} 
  float get_emcPhicut	() const { return emcPhicut;} 
  float get_emcZcut	() const { return emcZcut;} 
  short get_nx1cut	() const { return nx1cut;} 
  short get_nx2cut	() const { return nx2cut;} 
  float get_elowcut	(const int idet) const { return elowcut[idet];} 
  float get_ehighcut	(const int idet) const { return ehighcut[idet];} 
  float get_tofcut	(const int idet) const { return tofcut[idet];} 

protected:
  float ptlowcut;
  float pthighcut;
  float vertexcut;
  float pc3Phicut;
  float pc3Zcut;
  float emcPhicut;
  float emcZcut;
  short nx1cut;
  short nx2cut;
  float elowcut[NUMEMCTYPES];
  float ehighcut[NUMEMCTYPES];
  float tofcut[NUMEMCTYPES];

  ClassDef(CNTInclusiveNanoCutsv2,1)

};
#endif


