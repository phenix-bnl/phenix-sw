#ifndef PHNANOCUTS_H
#define PHNANOCUTS_H

#include "PHParticle.h"
#include "PHCompositeNode.h"

//
//  This routine is based upon the original PHCut routine
//  written by Sasha Lebedev.  It has been modified to be a purely
//  virtual base class which matches the interface that is 
//  the most convenient and hereafter required for the nDST 
//  production.
//
//  Users of the nDST code are *required* to inherit from this
//  base class whose purely virtual functions must be overwritten.
//  In this way we have a cut class with at least some level of
//  universality in its interface.
//
//  Specific additional functionality which is not universal 
//  enough to be included in this class is, of course, 
//  encouraged and expected in the classes that inherit 
//  from here.
//
//                                  TKH 3-12-2002
//

const float BIGPOSITIVEFLOAT =  9999.;
const float BIGNEGATIVEFLOAT = -9999.;
const int   BIGPOSITIVEINT   =  9999;
const int   BIGNEGATIVEINT   = -9999;

/**
Class representing a collection of cuts. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html}
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Collection of Cuts
*/

class PHnanoCuts : public PHObject {

public:
  PHnanoCuts() {}
  virtual ~PHnanoCuts() {}
  
  // These are purely virtual and must be overridden
  // in the inherited classes..
  // They should return true when all cuts are passed
  virtual PHBoolean ParticleOK(PHParticle* php, const unsigned int itrk)=0;
  virtual PHBoolean ParticleOK(PHCompositeNode* /*topNode*/, const unsigned int /*itrk*/) { return False; }
  virtual PHBoolean GlobalOK  (PHCompositeNode* topnode)=0;
  virtual PHBoolean ParticleCollectionOK  (PHCompositeNode* /*topnode*/) {return True; }

  // Reset restores cuts to default levels
  // Initialize sets best present values 
  //     (possibly based upon run#, hence the topnode arguement)
  virtual void Reset()=0;
  virtual void Initialize(PHCompositeNode* topnode=0 )=0;
 
  ClassDef(PHnanoCuts,1)

};
#endif











