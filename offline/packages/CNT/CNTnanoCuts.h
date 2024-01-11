#ifndef CNTNANOCUTS_H
#define CNTNANOCUTS_H

#include "PHnanoCuts.h"
#include "PHCentralTrack.h"

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
class CNTnanoCuts : public PHnanoCuts {

public:

  CNTnanoCuts();
  CNTnanoCuts(const CNTnanoCuts& t);
  virtual ~CNTnanoCuts() { }

  // These are necessary for all PHObjects:
  int  isValid() const {return(1);}
  void identify(std::ostream &os=std::cout) const {os << "identify yourself: CNTnanoCuts Object" << std::endl;}

  // These routines override and implement the pure virtual 
  // ones from the base class
  PHBoolean ParticleOK(PHParticle* php, const unsigned int itrk);
  PHBoolean GlobalOK  (PHCompositeNode* topnode);
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
  PHBoolean CentralTrackOK(PHCentralTrack* php, const unsigned int itrk);

  void  set_CentralTrackCuts();  // Internal helper function to Initialize...

  // Sets...
  void  set_ptlowcut  (float a) {ptlowcut   = a;}  // GeV
  void  set_pthighcut (float a) {pthighcut  = a;}  // GeV
  void  set_vertexcut (float a) {vertexcut  = a;}  // cm
  void  set_pc3Phicut (float a) {pc3Phicut  = a;}  // sigma
  void  set_pc3Zcut   (float a) {pc3Zcut    = a;}  // sigma
  void  set_emcPhicut (float a) {emcPhicut  = a;}  // sigma
  void  set_emcZcut   (float a) {emcZcut    = a;}  // sigma
  void  set_nx1cut    (short a) {nx1cut     = a;}  // sigma
  void  set_nx2cut    (short a) {nx2cut     = a;}  // sigma

  // Gets...
  float get_ptlowcut ()         {return  ptlowcut;}
  float get_pthighcut()         {return pthighcut;}
  float get_vertexcut()         {return vertexcut;}
  float get_pc3Phicut()         {return pc3Phicut;}
  float get_pc3Zcut  ()         {return   pc3Zcut;}
  float get_emcPhicut()         {return emcPhicut;}
  float get_emcZcut  ()         {return   emcZcut;}
  short get_nx1cut  ()          {return    nx1cut;}
  short get_nx2cut  ()          {return    nx2cut;}

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

  ClassDef(CNTnanoCuts,1)

};
#endif


