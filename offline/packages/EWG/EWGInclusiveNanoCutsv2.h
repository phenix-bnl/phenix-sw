#ifndef EWGINCLUSIVENANOCUTSV2_H
#define EWGINCLUSIVENANOCUTSV2_H


//
//  This class inherits from the PHnanoCuts class
//  and implements the global event and track cuts
//  which are appropriate for the Electron Working 
//  Group.  The structure is largely due to the 
//  original PHCut code from Sasha Lebedev.  It has
//  been adapted to inherit from the PHnanoCuts base
//  class as a mechanism to inforce the interface upon
//  all the cut objects used for the formation of
//  nDSTs.
//                   TKH  3-12-2002
//
//  Howdy.  This file is now fully implementing the PHENIX
//  schema evolution technology.  All its functions exist 
//  for the purpose of over-riding the virtual one in the base 
//  class.  As always, to change this file, *please* increment 
//  the version and add new functionality to the new class.
//                    TKH  5-29-2002
//


/**
Class representing a collection of cuts. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html}
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Collection of Cuts
*/

#include "PHInclusiveNanoCuts.h"
#include "phool.h"

#include <iostream>

class PHCentralTrack;
class PHCompositeNode;

class EWGInclusiveNanoCutsv2 : public PHInclusiveNanoCuts {

public:

  EWGInclusiveNanoCutsv2();
  virtual ~EWGInclusiveNanoCutsv2() { }

  // These are necessary for all PHObjects:
  int  isValid() const {return(1);}
  void identify(std::ostream &os=std::cout) const {os << "identify yourself: EWGInclusiveNanoCutsv2 Object" << std::endl;}

  // These routines override and implement the pure virtual 
  // ones from the base class
  PHBoolean GlobalOK      (PHCompositeNode* topnode);
  PHBoolean CentralTrackOK(PHCentralTrack* php, const unsigned int itrk);

  // These routines regard Particle Species that the EWG group does
  // not put in its output file.
  PHBoolean MuonOK              (PHMuoTracksOut* php,   const unsigned int itrk) {return False;}
  PHBoolean ParticleCollectionOK(PHCompositeNode* topNode) {return True;}

  void Reset();
  void Initialize(PHCompositeNode* topnode=0);

  //------------------------
  // Below this point, we implement variables and functions
  // which are specific to the application of EWGInclusive cuts.
  // Many of these will be inappropriate for non-electron
  // of non-central track analysis.
  //------------------------

  // Set the necessary cutting variables...
  void set_ptlowcut	(const float val) { ptlowcut = val;} 
  void set_pthighcut	(const float val) { pthighcut = val;} 
  void set_vertexcut	(const float val) { vertexcut = val;} 
  void set_pc3Phicut	(const float val) { pc3Phicut = val;} 
  void set_pc3Zcut	(const float val) { pc3Zcut = val;} 
  void set_emcPhicut	(const float val) { emcPhicut = val;} 
  void set_emcZcut	(const float val) { emcZcut = val;} 
  void set_nx1cut	(const short val) { nx1cut = val;} 
  void set_nx2cut	(const short val) { nx2cut = val;} 
  void set_chi2overnpe0max	(const float val) { chi2overnpe0max = val;} 
  void set_n0min	(const short val) { n0min = val;} 
  void set_sn0min	(const short val) { sn0min = val;} 
  void set_qualitymin	(const short val) { qualitymin = val;} 
  void set_eoverpmin	(const float val) { eoverpmin = val;} 
  void set_eoverpmax	(const float val) { eoverpmax = val;} 
  void set_dispcut      (const float val) { dispcut = val;}
  void set_chi2overnpecut (const float val) {chi2overnpecut = val;}

  // Get the necessary cutting variables...
  float get_ptlowcut	() const { return ptlowcut;} 
  float get_pthighcut	() const { return pthighcut;} 
  float get_vertexcut	() const { return vertexcut;} 
  float get_pc3Phicut	() const { return pc3Phicut;} 
  float get_pc3Zcut	() const { return pc3Zcut;} 
  float get_emcPhicut	() const { return emcPhicut;} 
  float get_emcZcut	() const { return emcZcut;} 
  short get_nx1cut	() const { return nx1cut;} 
  short get_nx2cut	() const { return nx2cut;} 
  float get_chi2overnpe0max	() const { return chi2overnpe0max;} 
  short get_n0min	() const { return n0min;} 
  short get_sn0min	() const { return sn0min;} 
  short get_qualitymin	() const { return qualitymin;} 
  float get_eoverpmin	() const { return eoverpmin;} 
  float get_eoverpmax	() const { return eoverpmax;} 
  float get_dispcut     () const { return dispcut;}
  float get_chi2overnpecut () const { return chi2overnpecut;}


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
  float chi2overnpe0max;
  short n0min;
  short sn0min;
  short qualitymin;
  float eoverpmin;
  float eoverpmax;
  float dispcut;
  float chi2overnpecut;

  ClassDef(EWGInclusiveNanoCutsv2,1)

};
#endif


