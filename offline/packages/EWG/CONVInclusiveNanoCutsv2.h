#ifndef CONVINCLUSIVENANOCUTSV2_H
#define CONVINCLUSIVENANOCUTSV2_H

#include "EWGInclusiveNanoCutsv2.h"


//
//  This class inherits from the PHInclusiveNanoCuts class
//  and implements the global event and track cuts
//  which are appropriate for selection of conversion pairs. 
//  SL, July 2004


/**
Class representing a collection of cuts for conversion pair selection. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html}
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Collection of Cuts
*/

#include <iostream>

class PHCentralTrack;
class PHCompositeNode;

class CONVInclusiveNanoCutsv2 : public EWGInclusiveNanoCutsv2 {

public:

  CONVInclusiveNanoCutsv2();
  CONVInclusiveNanoCutsv2(const CONVInclusiveNanoCutsv2& t);
  virtual ~CONVInclusiveNanoCutsv2() { }

// These routines override methods  
// from the base class
  
  void identify(std::ostream &os=std::cout) const {os << "CONVInclusiveNanoCutsv2 Object" << std::endl;}
  void Reset();
  void Initialize(PHCompositeNode* topnode=0);
  PHBoolean CentralTrackOK(PHCentralTrack* php, const unsigned int itrk);

// Below this point, we implement variables and functions
// which are specific to the conversion pair selection.  
  
     float get_n1min() 		{return n1min;}
     float get_dispcut() 	{return dispcut;}
     float get_chi2cut() 	{return chi2cut;}
     float get_eplowcut() 	{return eplowcut;}
     float get_ephighcut() 	{return ephighcut;}
     float get_phivcut1() 	{return phivcut1;}
     float get_phivcut2() 	{return phivcut2;}
     float get_invmasscut() 	{return invmasscut;}
     float get_identifyElectrons() {return identifyElectrons;}

     void set_n1min(float a) 	{n1min=a;}
     void set_dispcut(float a) 	{dispcut=a;}
     void set_chi2cut(float a) 	{chi2cut=a;}
     void set_eplowcut(float a) {eplowcut=a;}
     void set_ephighcut(float a) {ephighcut=a;}
     void set_phivcut1(float a) {phivcut1=a;}
     void set_phivcut2(float a) {phivcut2=a;}
     void set_invmasscut(float a) {invmasscut=a;}
     void set_identifyElectrons(int a) {identifyElectrons=a;}

protected:

  float get_phiv(PHCentralTrack* tracks, int i1, int i2);
  float get_invmass(PHCentralTrack* tracks, int i1, int i2);

  float n1min;
  float dispcut;
  float chi2cut;
  float eplowcut;
  float ephighcut;
  float phivcut1;
  float phivcut2;
  float invmasscut;
  int identifyElectrons;

  ClassDef(CONVInclusiveNanoCutsv2,1)

};
#endif


