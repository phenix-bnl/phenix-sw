// $Id: TMutTrk.hh,v 1.40 2014/01/27 00:28:20 bbannier Exp $

/*!
  \file TMutTrk.h
  \brief The Muon tracker Track object 
  \author S. Kelly
  \version $Revision: 1.40 $
  \date $Date: 2014/01/27 00:28:20 $
*/

#ifndef __TMUTTRK_H__
#define __TMUTTRK_H__

#include<TDataType.h>
#include<PHKey.hh>

#include<TMutTrkPar.hh>
#include<TMutRecoPar.hh>
#include<TMutTrkRes.hh>
#include<TMutBPPar.hh>
#include<MUTOO.h>

/*! @ingroup interface */
//!  The Muon tracker Track object 

/*! 
  <b>The MUTR Track Object</b><br>

  The MUTR track object presents an interface to track parameter objects
  TMutTrkPar.   The TMutTrkPar object is in physical units (momentum and
  position). It includes an interface to the full  covariance matrix.  The
  track parameters are stored for each coordinate belonging to the track..

  <p>
  The track object presents an interface to a list of TMutTrkRes objects.
  TMutTrkRes objects are track residual objects used for detector 
  performance studies.  They contain an interface to data that is specific
  to an active detector elements contribution to the track.  Each TMutCoord
  associated with the track will generate a TMutTrkRes object if the 
  appropriate run-time flag is set in the mMutTrackFit module.
  <p>
  To access all the hits associated with a track one uses the standard
  syntax for accessing associated objects in MUTOO.  The code frag below
  shows how this is done.<br><br>
  \code
  
  // Pseudo Code
  //
  //Get iterator to associated coordinates
  //   Loop over coordinates [
  //     print coordinate
  //   ]
  //]
  
  // Actual Code
  // Assume we have a TMutTrkMap::pointer trk_ptr in local scope
  //
  
  // local typedefs for readability
  //
  typedef TMutCoordMap::key_iterator coord_iterator;
  
  coord_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
  while(TMutCoordMap::pointer coord_ptr = coord_iter.next()){
    // print coordinate to std::cout
    //
    coord_ptr->get()->print();
  }
      
  \endcode
*/

class TMutTrk : public PHKey
{
  
 public:

  //! Name of the list of TMutTrkRes contained in this object 
  typedef std::vector<TMutTrkRes> residual_list;
  
  //! Name of a const iterator to residual list 
  typedef std::vector<TMutTrkRes>::const_iterator const_residual_iterator;
  
  //! Name of the list of TMutTrkPar contained in this object 
  typedef std::vector<TMutTrkPar> trk_par_list;
  
  //! Name of a const iterator to residual list 
  typedef std::vector<TMutTrkPar>::const_iterator const_trk_par_iterator;
  
  //! Name of the list of TMutRecoPar contained in this object 
  typedef std::vector<TMutRecoPar> reco_par_list;
  
  //! Name of a const iterator to residual list 
  typedef std::vector<TMutRecoPar>::const_iterator const_reco_par_iterator;

  //! track status enumeration 
  /*! 
  important note: one shoud _never_ remove bits from here, or change bit values
  because it would break backward compatibility.
  Adding new bits is safe, though.
  */
  enum Status {
    //! no fit was performed
    NO_FIT = 0, 
        
    //! regular track fit was performed (obsolete)
    GLOBAL_FIT=1, 
        
    //! kalman fit was performed
    KALMAN_FIT=2, 
    
    //! global fit performed and failed
    GLOBAL_FAIL=3, 
    
    //! kalman fit performed and failed
    KALMAN_FAIL=4, 
    
    //! track has too few hits
    RECO_MIN_HIT=5, 
    
    //! track is identified as ghost
    GHOST=6, 
    
    //! track has low momentum
    LOW_MOM=7, 
    
    //! no starting initial momentum estimate could be done
    NO_ESTIMATE=8, 
    
    //! track Bend Plane fit using stations 2 and 3 was performed
    BP_FIT2=9, 
    
    //! track Bend Plane fit using stations 1 2 and 3 was performed
    BP_FIT3=10,
    
    //! true when straight track fit is used
    STRAIGHT_TRACK = 11,
    
    //! track goes accross both arms
    BOTH_ARMS = 12
    
  };

  //! @name Constructors/Destructors
  //@{    

  //! Default constructor 
  TMutTrk()
  {}

  //! Destructor 
  virtual ~TMutTrk()
  {}

  //! Construct with key and location 
  TMutTrk(const Key& key) : PHKey(key) 
  {} 

  //@}

  //! @name Track Parameters Interface
  //@{  

  //! Read only pointer to TMutTrkPar at z reference 
  virtual const TMutTrkPar* get_trk_par() const 
  {
    return 0;
  }

  //! TMutTrkPar (track pars in physics units) 
  virtual void set_trk_par(const TMutTrkPar& trk_par) 
  {}
  
  //! Read only pointer to TMutTrkPar extrapolated to primary vertex 
  virtual const TMutTrkPar* get_trk_par_vtx() const 
  {return 0;}

  //! TMutTrkPar (track pars in physics units) extrapolated to primary vertex 
  virtual void set_trk_par_vtx(const TMutTrkPar& trk_par) 
  {}

  //! calculate rapidity at vertex, assuming muo
  virtual double get_rapidity( void ) const;

  //! Push a TMutTrkPar object onto TMutTrkPar list 
  virtual void push_trk_par(const TMutTrkPar& trk_par)
  {}

  //! Clear TMutTrkPar list 
  virtual void clear_trk_par_list()
  {}

  //! Read only access to TMutTrkPar list 
  virtual const trk_par_list* get_trk_par_list() const 
  {return 0;} 

  /*! 
    Returns a const pointer to the TMutTrkPar object closest to
    at gap 0 of the requested station by searching the trk_par_list.
    If the trk_par_list is empty then it returns a const pointer to
    the trk_par object at z_reference.
  */
  virtual const TMutTrkPar* get_trk_par_station( UShort_t arm, UShort_t station ) const;

  /*! 
    Returns a const pointer to the TMutTrkPar object closest to
    at gap 0 of the requested station by searching the trk_par_list.
    If the trk_par_list is empty then it returns a const pointer to
    the trk_par object at z_reference.
  */
  virtual const TMutTrkPar* get_trk_par_station(UShort_t station ) const
  { return get_trk_par_station( get_arm(), station ); }

  //! size of the TMutTrkPar list  
  virtual size_t get_n_trk_par() const 
  { return 0; }

  //@}

  //! @name Bend Plane Track Parameters Interface
  //@{  
  //! Bend plane track parameters 
  virtual const TMutBPPar* get_bp_par() const 
  { 
    static TMutBPPar* val = new TMutBPPar();
    return val;
  }

  //! Non-const bend plane track parameters 
  virtual TMutBPPar* get_bp_par() 
  { 
    static TMutBPPar* val = new TMutBPPar();
    return val;
  }
  
  //! Bend plane track parameters 
  virtual void set_bp_par(const TMutBPPar& bp_par) 
  {}
  //@}

  //! @name Fit Parameters Interface (Linear Model)
  //@{  
  //! Read only pointer to TMutFitPar 
  virtual const TMutFitPar* get_fit_par() const 
  {return 0;}
  
  //! TMutFitPar (track pars in geometric units) 
  virtual void set_fit_par(const TMutFitPar* fit_par) 
  {}
  
  //! TMutFitPar (track pars in geometric units) 
  virtual void set_fit_par(const TMutFitPar& fit_par) 
  {}
  
  //@}

  //! @name Merit Statistics Interface
  //@{  
  /*! \brief
    unless overloaded, returns track number of degrees of freedom.
    this is the number of associated coordinates - 5
  */
  virtual size_t get_ndf() const;
  
  //! Chi square statistic of w fit (sum of square of w pulls) 
  virtual double get_w_chi_square() const 
  {return 0;}
  
  //! Chi square statistic of w fit (sum of square of w pulls) per degree of freedom 
  virtual double get_w_chi_square_pdf() const 
  {return 0;}
  
  //! Chi square statistic of r fit (sum of square of w pulls) 
  virtual double get_r_chi_square() const 
  {return 0;}
  
  //! Chi square statistic of r fit (sum of square of w pulls) per degree of freedom 
  virtual double get_r_chi_square_pdf() const 
  {return 0;}
  
  //! Total chi square (w_chi_square + r_chi_square) 
  virtual double get_chi_square() const 
  {return 0;}
  
  //! Chi square statistic of w fit (sum of square of w pulls)   
  virtual void set_w_chi_square(double w_chi_square) 
  {}
  
  //! Chi square statistic of r fit (sum of square of r pulls) 
  virtual void set_r_chi_square(double r_chi_square) 
  {}
  
  /*! \brief
    sets track number of degrees of freedom.
  */
  virtual void set_ndf( size_t ndf )
  {}
  
  //! Returns the depth of the deepest associated road 
  //virtual int get_max_road_depth() const;
  
  /*! 
    \brief
    returns hit (TMutCoord) pattern. 
    unless overloaded, it is calculated using the associtions between 
    TMutTrk and TMutCoord
  */
  
  virtual UShort_t get_hit_pattern() const;
  
  //! sets hit (TMutCoord) pattern 
  virtual void set_hit_pattern( UShort_t pattern )
  {}
  
  //@}

  //! @name Stub Queries
  //@{  
  //! Returns true if track has an associated stub in given station 
  virtual bool has_stub(UShort_t station) const; 
  
  //! Calculates R * Delta Phi between station 1 and 2 stubs 
  virtual double get_rdphi_12() const;
  
  //! Calculates R * Delta Phi between station 2 and 3 stubs 
  virtual double get_rdphi_23() const;

  //! Use delta phi between station 2 and station 3 to estimate the momentum
  double calculate_momentum_point_point( void ) const;  

  //! Use delta phi between station 2 and station 3 to estimate the momentum
  double calculate_momentum_point_point2( void ) const;  
  
#ifndef __CINT__
  
  //! Use stub to estimate tangent in given station
  std::pair<bool,PHVector> estimate_tangent( UShort_t station=MUTOO::Station2) const;

  //!  Use stub to estimate charge 
  std::pair<bool,int> estimate_charge( void ) const;

#endif
    
  //! Use delta phi (sta 12 + sta 23) corrected by stub dwdz to estimate the momentum.
  double calculate_momentum_stub_point( void ) const;
  
  //@}

  //! @name Windows Interface
  //@{  
  //! Phi window (radians) 
  virtual double get_phi_min(UShort_t station) const 
  { return 0; }
  
  //! Phi window (radians) 
  virtual double get_phi_max(UShort_t station) const 
  { return 0; }
  
  //! Phi window (radians) 
  virtual void set_phi_min(UShort_t station, double phi_min)
  {}
  
  //! Phi window (radians) 
  virtual void set_phi_max(UShort_t station, double phi_max)
  {}
  
  //! Theta window (radians) 
  virtual double get_theta_min(UShort_t station) const 
  { return 0; }
  
  //! Theta window (radians) 
  virtual double get_theta_max(UShort_t station) const 
  { return 0; }
  
  //! Theta window (radians) 
  virtual void set_theta_min(UShort_t station, double theta_min)
  {}
  
  //! Theta window (radians) 
  virtual void set_theta_max(UShort_t station, double theta_max){;}
  //@}

  //! @name Track Residuals Interface
  //@{  
  //! Push a TMutTrkRes object onto w residual list 
  virtual void push_w_residual(const TMutTrkRes& residual)
  {}

  //! Clear residual list 
  virtual void clear_w_residual_list()
  {}

  //! Number of residual in this track object 
  virtual size_t get_n_w_residual() const 
  {return 0;}

  //! Read only access to w residual list 
  virtual const residual_list* get_w_residual_list() const 
  {return 0;} 

  //! Push a r residual onto TMutTrk r residual list 
  virtual void push_r_residual(double residual)
  {}

  //! Clear residual list 
  virtual void clear_r_residual_list()
  {}

  //! Number of residual in this track object 
  virtual size_t get_n_r_residual() const 
  {return 0;}

  //! Read only access to w residual list 
  virtual const std::vector<double>* get_r_residual_list() const 
  {return 0;} 

  //@}
  
  //! @name Locators
  //@{    
  
  #ifndef __CINT__
  //! return class ID, mapped from class name
  virtual PHClassId::id_type get_class_id( void ) const
  { return (_class_id) ? _class_id : (_class_id = PHClassId::get( GetName() ) ); }
  #endif

  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
  {return 0;}
  
  //! Octant [0,7] 
  virtual UShort_t  get_octant() const 
  {return 0;}
  
  //! Index [0,1024] 
  virtual UShort_t  get_index() const 
  {return 0;}  
  
  //! Arm [0,1] 
  virtual void set_arm(UShort_t arm)
  {}
  
  //! Octant [0,7] 
  virtual void set_octant(UShort_t octant)
  {}               
  
  //! Index [0,1024] 
  virtual void  set_index(UShort_t index){}               
  //@}

  //! @name Global Track Attributes
  //@{  

  //! Get the charge of the track 
  virtual Float_t get_charge() const 
  {return 0;}
  
  //! Get the charge of the track 
  virtual void set_charge(int charge) 
  {}  
  
  /*! \brief 
    Get the number of TMutCoord objects
    associated with this track.
  */
  virtual size_t get_n_coord() const; 
  
  /*! \brief
    Get the number of TMutGapCoord objects
    associated with this track.
  */
  virtual size_t get_n_gap_coord() const; 
  
  //@}

  //! @name Track Status
  //@{  

  //! combine status flags to tag reconstruction succesfull 
  virtual bool get_reco_success() const 
  { return false; }
  
  //! set status
  virtual void set_status( Status, bool ) 
  {}

  //! get status 
  virtual bool get_status( Status ) const
  { return false; }

  //! Get the status word 
  virtual UShort_t get_status() const 
  { return 0;}
  
  //! Set the status word 
  virtual void copy_status( const TMutTrk& )
  {}
  
  //! Clear the status word 
  virtual void clear_status() 
  {}

  //! Set the global bit 
  virtual void set_global_fit() 
  { set_status( GLOBAL_FIT, true ); }
  
  //! Test the global bit 
  virtual bool get_global_fit() const 
  { return get_status( GLOBAL_FIT ); }

  //! Set the kalman bit 
  virtual void set_kalman_fit() 
  { set_status( KALMAN_FIT, true ); }
  
  //! Test the kalman bit 
  virtual bool get_kalman_fit() const 
  { return get_status( KALMAN_FIT ); }

  //! Set the global fail bit 
  virtual void set_global_fail() 
  { set_status( GLOBAL_FAIL, true ); }
  
  //! Test the global fail bit 
  virtual bool get_global_fail() const 
  { return get_status( GLOBAL_FAIL ); }

  //! Set the kalman fail bit 
  virtual void set_kalman_fail() 
  { set_status( KALMAN_FAIL, true ); }
  
  //! Test the kalman fail bit 
  virtual bool get_kalman_fail() const 
  { return get_status( KALMAN_FAIL ); }

  //! Set the min hits bit 
  virtual void set_reco_min_hits() 
  { set_status( RECO_MIN_HIT, true ); }
  
  //! Test the min hits bit 
  virtual bool get_reco_min_hits() const 
  { return get_status( RECO_MIN_HIT ); }

  //! set ghost bit to value passed in argument
  virtual void set_ghost( bool value = true )
  { set_status( GHOST, value ); }
  
  //! Test the ghost bit 
  virtual bool get_ghost() const 
  { return get_status( GHOST ); }

  //! Set the low momentum bit 
  virtual void set_low_mom() 
  { set_status( LOW_MOM, true ); }
  
  //! Test the low momentum bit 
  virtual bool get_low_mom() const 
  { return get_status( LOW_MOM ); }

  //! Set the no estimate bit 
  virtual void set_no_estimate() 
  { set_status( NO_ESTIMATE, true ); }
  
  //! Test the no estimate bit 
  virtual bool get_no_estimate() const 
  { return get_status( NO_ESTIMATE ); }

  //! Set the no fit bit 
  virtual void set_no_fit() 
  { set_status( NO_FIT, true ); }
  
  //! Test the no fit bit 
  virtual bool get_no_fit() const 
  { return get_status( NO_FIT ); }

  //! Set the bp fit 2 bit 
  virtual void set_bp_fit2() 
  { set_status( BP_FIT2, true ); }
  
  //! Test the bp fit 2 bit 
  virtual bool get_bp_fit2() const 
  { return get_status( BP_FIT2 ); }

  //! Set the bp fit 3 bit 
  virtual void set_bp_fit3() 
  { set_status( BP_FIT3, true ); }
  
  //! Test the bp fit 3 bit 
  virtual bool get_bp_fit3() const 
  { return get_status( BP_FIT3 ); }

  //! print status
  virtual void print_status( std::ostream& os = std::cout ) const;
  
  //@}

  //! @name Dumpers
  //@{    
  //! Print data members to ostream os, defaults to std::cout
  virtual void print(std::ostream& os, bool max) const {}
  virtual void print(std::ostream& os = std::cout) const { print(os, false); }
  //@}

  protected:

#ifndef __CINT__

  std::pair<bool,PHVector> estimate_tangent_sta12( UShort_t station=MUTOO::Station2) const;
  
  std::pair<bool,PHVector> estimate_tangent_sta3( void ) const; 
  
  std::pair<bool,double> estimate_charge_sta12( void ) const;
  
  std::pair<bool,double> estimate_charge_sta23( void ) const;

#endif
 
  private:
  
#ifndef __CINT__
  
  //! static class ID 
  static PHClassId::id_type _class_id;

#endif   
  ClassDef(TMutTrk,1)
};
  
#endif /* __TMUTTRK_H__*/
