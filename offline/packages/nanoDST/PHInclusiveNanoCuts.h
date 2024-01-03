#ifndef PHINCLUSIVENANOCUTS_H
#define PHINCLUSIVENANOCUTS_H

#include "PHObject.h"
#include "phool.h"

class PHCentralTrack;
class PHCompositeNode;
class PHMuoTracksOut;

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
//  Hi gang!!
//       We're at it again...changes in structure for the convenience of all.
//  Here is the scoop.  We now have working groups that want more than one
//  type of particle in a single file.  In that case, they need to supply 
//  *independent* cutters for each particle species.  We presently have three
//  base class species:
//   PHCentralTracks  (CentralTracks, Electrons, Hadrons, Hard)
//   PHMuons
//  in this latest implementation we will supply a logical method that accepts
//  each of these types in performing its duty.
//
//       But wait...there's more!  In the original version of these files we
//  used purely virtual functions.  This is *not* convenient for scheme evolution
//  So, this base class is reworked so that we use the normal schema evolution 
//  techniques and thereby don't "break old files"
//
//                                 TKH 5-29-2002
//
//
/**
Class representing a collection of cuts. <br>
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/nanoDST/index.html}
@author Sasha Lebedev (ISU) lebedev@iastate.edu
@memo Collection of Cuts
*/
// Frédéric Fleuret 7-17-2002: Add event selection on minimum number of 
//      muon tracks (mintrackcut) per event. Add event selection on maximum number of 
//      muon tracks (maxtrackcut) per event.

class PHInclusiveNanoCuts : public PHObject {

public:
  PHInclusiveNanoCuts() {
    BIGPOSITIVEFLOAT =  9999.;
    BIGNEGATIVEFLOAT = -9999.;
    BIGPOSITIVEINT   =  9999;
    BIGNEGATIVEINT   = -9999;
  }

  virtual ~PHInclusiveNanoCuts() {}
  
  // Reset restores cuts to default levels
  // Initialize sets best present values 
  //     (possibly based upon run#, hence the topnode arguement)
  virtual void Reset()                                 {warning("Reset");}
  virtual void Initialize(PHCompositeNode* /*topnode=0*/ ) {warning("Initialize");}

  // These are virtual and should  be overridden
  // in the inherited classes..
  // They should return true when all cuts are passed
  virtual PHBoolean GlobalOK(PHCompositeNode* /*topnode*/)
      {warning("GlobalOK"); return True; }
  virtual PHBoolean CentralTrackOK(PHCentralTrack* /*php*/, const unsigned int /*itrk*/)
      {warning("CentralTrackOK"); return True; }
  virtual PHBoolean MuonOK(PHMuoTracksOut* /*php*/, const unsigned int /*itrk*/)
      {warning("MuonOK"); return True;}
  virtual PHBoolean diMuonOK(PHMuoTracksOut* /*php*/, const unsigned int /*idimu*/)
      {warning("diMuonOK"); return True;}
  virtual PHBoolean diMuonFilterOK(PHMuoTracksOut* /*php*/)
      {warning("diMuonFilterOK"); return True;}
  virtual PHBoolean ParticleCollectionOK  (PHCompositeNode* /*topnode*/)
      {warning("ParticleCollectionOK"); return True; }

  // Here are the virtual setter functions...
  virtual void set_ptlowcut	(const float /*val*/)  {warning("ptlowcut ");}
  virtual void set_pthighcut	(const float /*val*/)  {warning("pthighcut ");}
  virtual void set_vertexcut	(const float /*val*/)  {warning("vertexcut ");}
  virtual void set_mintrackcut  (const int /*val*/)    {warning("mintrackcut");}
  virtual void set_maxtrackcut  (const int /*val*/)    {warning("maxtrackcut");}
  virtual void set_minhitcut    (const int /*val*/)    {warning("minhitcut ");}

  virtual void set_dodimu       (const bool /*val*/)   {warning("dodimu ");}
  virtual void set_dimasscut    (const float /*val*/)  {warning("dimasscut ");}

  virtual void set_dofilter         ( const bool /*val*/)  { warning("dofilter "); }
  virtual void set_min_dimass_filter( const float /*val*/) { warning("min_dimass_filter "); }
  virtual void set_max_dimass_filter( const float /*val*/) { warning("max_dimass_filter "); }

  virtual void set_ghostsel     (const bool /*val*/)   {warning("ghostsel ");}
  virtual void set_bbct0cut	(const float /*val*/)  {warning("bbct0cut ");}
  virtual void set_pc3Phicut	(const float /*val*/)  {warning("pc3Phicut ");}
  virtual void set_pc3Zcut	(const float /*val*/)  {warning("pc3Zcut ");}
  virtual void set_emcPhicut	(const float /*val*/)  {warning("emcPhicut ");}
  virtual void set_emcZcut	(const float /*val*/)  {warning("emcZcut ");}
  virtual void set_tofPhicut	(const float /*val*/)  {warning("tofPhicut ");}
  virtual void set_tofZcut	(const float /*val*/)  {warning("tofZcut ");}
  virtual void set_nx1cut	(const short /*val*/)  {warning("nx1cut ");}
  virtual void set_nx2cut	(const short /*val*/)  {warning("nx2cut ");}
  virtual void set_chi2overnpe0max(const float /*val*/)  {warning("chi2overnpe0max ");}
  virtual void set_n0min	(const short /*val*/)  {warning("n0min ");}
  virtual void set_sn0min	(const short /*val*/)  {warning("sn0min ");}
  virtual void set_qualitymin	(const short /*val*/)  {warning("qualitymin ");}
  virtual void set_eoverpmin	(const float /*val*/)  {warning("eoverpmin ");}
  virtual void set_eoverpmax	(const float /*val*/)  {warning("eoverpmax ");}
  virtual void set_plowcut	(const float /*val*/)  {warning("plowcut ");}
  virtual void set_phighcut	(const float /*val*/)  {warning("phighcut ");}
  virtual void set_elowcut	(const int /*idet*/, const float /*val*/) {warning("elowcut[NUMEMCTYPES] ");}
  virtual void set_ehighcut	(const int /*idet*/, const float /*val*/) {warning("ehighcut[NUMEMCTYPES] ");}
  virtual void set_tofcut	(const int /*idet*/, const float /*val*/) {warning("tofcut[NUMEMCTYPES] ");}

  // Here are the virtual getter functions...
  virtual float get_ptlowcut	 () const {warning("ptlowcut "); return -9999;}
  virtual float get_pthighcut	 () const {warning("pthighcut "); return -9999;}
  virtual float get_vertexcut	 () const {warning("vertexcut "); return -9999;}
  virtual int   get_mintrackcut  () const {warning("mintrackcut "); return -9999;}
  virtual int   get_maxtrackcut  () const {warning("maxtrackcut "); return -9999;}
  virtual int   get_minhitcut	 () const {warning("minhitcut "); return -9999;}
  virtual bool  get_dodimu       () const {warning("dodimu "); return false;}
  virtual float get_dimasscut    () const {warning("dimasscut "); return -9999;}
  
  virtual bool  get_dofilter ()           const { warning("dofilter "); return false; }
  virtual float get_min_dimass_filter ()  const { warning("min_dimass_filter "); return -9999; }
  virtual float get_max_dimass_filter ()  const { warning("max_dimass_filter "); return  9999; }
  
  virtual bool  get_ghostsel     () const {warning("ghostsel "); return false;}
  virtual float get_bbct0cut	 () const {warning("bbct0cut "); return -9999;}
  virtual float get_pc3Phicut	 () const {warning("pc3Phicut "); return -9999;}
  virtual float get_pc3Zcut	 () const {warning("pc3Zcut "); return -9999;}
  virtual float get_tofPhicut	 () const {warning("tofPhicut "); return -9999;}
  virtual float get_tofZcut	 () const {warning("tofZcut "); return -9999;}
  virtual float get_emcPhicut	 () const {warning("emcPhicut "); return -9999;}
  virtual float get_emcZcut	 () const {warning("emcZcut "); return -9999;}
  virtual short get_nx1cut	 () const {warning("nx1cut "); return -9999;}
  virtual short get_nx2cut	 () const {warning("nx2cut "); return -9999;}
  virtual float get_chi2overnpe0max	 () const {warning("chi2overnpe0max "); return -9999;}
  virtual short get_n0min	 () const {warning("n0min "); return -9999;}
  virtual short get_sn0min	 () const {warning("sn0min "); return -9999;}
  virtual short get_qualitymin	 () const {warning("qualitymin "); return -9999;}
  virtual float get_eoverpmin	 () const {warning("eoverpmin "); return -9999;}
  virtual float get_eoverpmax	 () const {warning("eoverpmax "); return -9999;}
  virtual float get_plowcut	 () const {warning("plowcut "); return -9999;}
  virtual float get_phighcut	 () const {warning("phighcut "); return -9999;}
  virtual float get_elowcut	 (const int /*idet*/) const {warning("elowcut[NUMEMCTYPES] "); return -9999;}
  virtual float get_ehighcut	 (const int /*idet*/) const {warning("ehighcut[NUMEMCTYPES] "); return -9999;}
  virtual float get_tofcut	 (const int /*idet*/) const {warning("tofcut[NUMEMCTYPES] "); return -9999;}

  float BIGPOSITIVEFLOAT ;//!
  float BIGNEGATIVEFLOAT ;//!
  int   BIGPOSITIVEINT   ;//!
  int   BIGNEGATIVEINT   ;//!

 private:
  void warning(const char* field) const { 
    std::cout << PHWHERE << "using virtual function, doing nothing" << std::endl;
    std::cout << "Offending field == " << field << std::endl;
  }

 public:  
  ClassDef(PHInclusiveNanoCuts,1)

};
#endif
