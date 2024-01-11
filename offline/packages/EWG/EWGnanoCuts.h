#ifndef EWGNANOCUTS_H
#define EWGNANOCUTS_H

#include "PHnanoCuts.h"

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


/**
Class representing a collection of cuts. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html}
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Collection of Cuts
*/

#include <iostream>

class PHCentralTrack;
class PHCompositeNode;
class PHParticle;

class EWGnanoCuts : public PHnanoCuts {

public:

  EWGnanoCuts();
  EWGnanoCuts(const EWGnanoCuts& t);
  virtual ~EWGnanoCuts() { }

  // These are necessary for all PHObjects:
  int  isValid() const {return(1);}
  void identify(std::ostream &os=std::cout) const {os << "identify yourself: EWGnanoCuts Object" << std::endl;}

  // These routines override and implement the pure virtual 
  // ones from the base class
  PHBoolean ParticleOK(PHParticle* php, const unsigned int itrk);
  PHBoolean GlobalOK  (PHCompositeNode* topnode);
  void Reset();
  void Initialize(PHCompositeNode* topnode=0);

  //------------------------
  // Below this point, we implement variables and functions
  // which are specific to the application of EWG cuts.
  // Many of these will be inappropriate for non-electron
  // of non-central track analysis.
  //------------------------
  PHBoolean CentralTrackOK(PHCentralTrack* php, const unsigned int itrk);
 
/// Set cuts to select electrons
  void set_ElectronCuts();
/// Returns low Pt cut value
  float get_vertexcut() {return vertexcut;}
/// Returns low Pt cut value
  float get_ptlowcut() {return ptlowcut;}
/// Returns high Pt cut value
  float get_pthighcut() {return pthighcut;}
/// Returns cut on distance to EMCal cluster in Phi
  float get_demcphicut() {return demcphicut;}
/// Returns cut on distance to EMCal cluster in Z
  float get_demczcut() {return demczcut;}
/// Returns cut on distance to Pc1 cluster in Phi
  float get_dpc1phicut() {return dpc1phicut;}
/// Returns cut on distance to Pc1 cluster in Z
  float get_dpc1zcut() {return dpc1zcut;}
/// Returns cut on distance to Pc2 cluster in Phi
  float get_dpc2phicut() {return dpc2phicut;}
/// Returns cut on distance to Pc2 cluster in Z
  float get_dpc2zcut() {return dpc2zcut;}
/// Returns cut on distance to Pc3 cluster in Phi
  float get_dpc3phicut() {return dpc3phicut;}
/// Returns cut on distance to Pc3 cluster in Z
  float get_dpc3zcut() {return dpc3zcut;}
/// Returns cut on distance to Tof cluster in Phi
  float get_dtofphicut() {return dtofphicut;}
/// Returns cut on distance to Tof cluster in Z
  float get_dtofzcut() {return dtofzcut;}
/// Returns cut on distance to Tec track in Phi
  float get_dtecphicut() {return dtecphicut;}
/// Returns cut on distance to Tec track in Alpha
  float get_dtecalphacut() {return dtecalphacut;}
/// Returns cut on Dch track quality
  short int get_qualitycut() {return qualitycut;}
/// Returns lower e/p ratio cut
  float get_eplowcut() {return eplowcut;}
/// Returns upper e/p ratio cut
  float get_ephighcut() {return ephighcut;}
/// Returns cut on number of fired RICH PMT
  short int get_npmt0cut() {return npmt0cut;}
/// Returns RICH chi-square cut value
  float get_chi2cut() {return chi2cut;}

/// Set vertex cut
  void set_vertexcut(float a) {vertexcut=a;}
/// Set low Pt cut value
  void set_ptlowcut(float a) {ptlowcut=a;}
/// Set high Pt cut value
  void set_pthighcut(float a) {pthighcut=a;}
/// Set cut on distance to EMCal cluster in Phi
  void set_demcphicut(float a) {demcphicut=a;}
/// Set cut on distance to EMCal cluster in Phi
  void set_demczcut(float a) {demczcut=a;}
/// Set cut on distance to Pc1 cluster in Phi
  void set_dpc1phicut(float a) {dpc1phicut=a;}
/// Set cut on distance to Pc1 cluster in Z
  void set_dpc1zcut(float a) {dpc1zcut=a;}
/// Set cut on distance to Pc2 cluster in Phi
  void set_dpc2phicut(float a) {dpc2phicut=a;}
/// Set cut on distance to Pc2 cluster in Z
  void set_dpc2zcut(float a) {dpc2zcut=a;}
/// Set cut on distance to Pc3 cluster in Phi
  void set_dpc3phicut(float a) {dpc3phicut=a;}
/// Set cut on distance to Pc1 cluster in Z
  void set_dpc3zcut(float a) {dpc3zcut=a;}
/// Set cut on distance to Tof cluster in Phi
  void set_dtofphicut(float a) {dtofphicut=a;}
/// Set cut on distance to Tof cluster in Z
  void set_dtofzcut(float a) {dtofzcut=a;}
/// Set cut on distance to Tec track in Phi
  void set_dtecphicut(float a) {dtecphicut=a;}
/// Set cut on distance to Tec track in Alpha
  void set_dtecalphacut(float a) {dtecalphacut=a;}
/// Set cut on Dch track quality
  void set_qualitycut(short int a) {qualitycut=a;}
/// Set lower cut on e/p ratio
  void set_eplowcut(float a) {eplowcut=a;}
/// Set upper cut on e/p ratio
  void set_ephighcut(float a) {ephighcut=a;}
/// Set cut on number of fired RICH PMT
  void set_npmt0cut(short int a) {npmt0cut=a;}
/// Set RICH chi-square cut value
  void set_chi2cut(float a) {chi2cut=a;}

protected:

  float vertexcut;

/// Cut on Dch track quality
  short int qualitycut;
/// Low Pt cut	
  float ptlowcut;
/// High Pt cut	
  float pthighcut;
/// Cut on distance to EMCal cluster in Phi		
  float demcphicut;		
/// Cut on distance to EMCal cluster in Z
  float demczcut;		
/// Cut on distance to Pc1 cluster in Phi
  float dpc1phicut;		
/// Cut on distance to Pc1 cluster in Z
  float dpc1zcut;		
/// Cut on distance to Pc2 cluster in Phi
  float dpc2phicut;		
/// Cut on distance to Pc2 cluster in Z
  float dpc2zcut;		
/// Cut on distance to Pc3 cluster in Phi
  float dpc3phicut;		
/// Cut on distance to Pc3 cluster in Z
  float dpc3zcut;		
/// Cut on distance to Tof cluster in Phi
  float dtofphicut;		
/// Cut on distance to Tof cluster in Z
  float dtofzcut;		
/// Cut on distance to Tec track in Phi
  float dtecphicut;		
/// Cut on distance to Tec track in Alpha
  float dtecalphacut;		
/// Lower cut on e/p ratio		
  float eplowcut;
/// Upper cut on e/p ratio
  float ephighcut;
/// Cut on number of fired RICH PMT
  short int npmt0cut; 
/// RICH Chi2 cut
  float chi2cut; 

  ClassDef(EWGnanoCuts,1)

};
#endif


