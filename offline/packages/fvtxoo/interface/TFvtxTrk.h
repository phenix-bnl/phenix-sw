// $Id: TFvtxTrk.h,v 1.15 2014/01/24 09:55:32 bbannier Exp $

/*!
  \file TFvtxTrk.h
  \brief The Forward Silicon (FVTX) Track object 
  \author M. Brooks
  \version $Revision: 1.15 $
  \date    $Date: 2014/01/24 09:55:32 $
*/

#ifndef __TFVTXTRK_H__
#define __TFVTXTRK_H__

#include<TDataType.h>
#include<PHKey.hh>

#include<TMutTrkPar.hh>
#include<TMutTrkRes.hh>
#include<FVTXOO.h>

/*! @ingroup interface */
//!  The Forward Silicon (FVTX) Track object 

/*! 
  <b>The FvtxR Track Object</b><br>

  The Fvtx track object presents an interface to track parameter objects
  TMutTrkPar.   The TMutTrkPar object is in physical units (momentum and
  position). It includes an interface to the full  covariance matrix.  The
  track parameters are stored for each coordinate belonging to the track..

  <p>
  The track object presents an interface to a list of TMutTrkRes objects.
  TMutTrkRes objects are track residual objects used for detector 
  performance studies.  They contain an interface to data that is specific
  to an active detector elements contribution to the track.  Each TFvtxCoord
  associated with the track will generate a TMutTrkRes object if the 
  appropriate run-time flag is set in the mFvtxTrackFit module.
  <p>
  To access all the hits associated with a track one uses the standard
  syntax for accessing associated objects in FVTXOO.  The code frag below
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
  // Assume we have a TFvtxTrkMap::pointer trk_ptr in local scope
  //
  
  // local typedefs for readability
  //
  typedef TFvtxCoordMap::key_iterator coord_iterator;
  
  coord_iterator coord_iter = trk_ptr->get()->get_associated<TFvtxCoord>();
  while(TFvtxCoordMap::pointer coord_ptr = coord_iter.next()){
    // print coordinate to std::cout
    //
    coord_ptr->get()->print();
  }
      
  \endcode
*/

class TFvtxTrk : public PHKey
{
  
 public:

  /*! Name of the list of TMutTrkRes contained in this object */
  typedef std::vector<TMutTrkRes> residual_list;
  
  /*! Name of a const iterator to residual list */
  typedef std::vector<TMutTrkRes>::const_iterator const_residual_iterator;
  
  /*! Name of the list of TMutTrkPar contained in this object */
  typedef std::vector<TMutTrkPar> trk_par_list;
  
  /*! Name of a const iterator to residual list */
  typedef std::vector<TMutTrkPar>::const_iterator const_trk_par_iterator;

  //! track status enumeration 
  enum Status {

    //! no fit was performed
    NO_FIT = 0, 
        
    //! kalman fit was performed
    KALMAN_FIT=1, 
    
    //! kalman fit performed and failed
    KALMAN_FAIL=2, 
    
    //! track has too few hits
    RECO_MIN_HIT=3, 
    
    //! track is identified as ghost
    GHOST=4, 
    
    //! track is associated to a MuTr track
    ASSOCIATED=5, 

    //! track is swapped associated to a MuTr track
    SWAPPED_ASSOCIATED=6,

  };

  //! @name Constructors/Destructors
  //@{    

  /*! Default constructor */
  TFvtxTrk()
  {}

  /*! Destructor */
  virtual ~TFvtxTrk()
  {}

  /*! Construct with key and location */
  TFvtxTrk(const Key& key) : PHKey(key) 
  {} 

  //@}

  //! @name Track Parameters Interface
  //@{  

  /*! Read only pointer to TMutTrkPar at z reference */
  virtual const TMutTrkPar* get_trk_par() const 
  {return 0;}

  /*! TMutTrkPar (track pars in physics units) */
  virtual void set_trk_par(const TMutTrkPar& trk_par) 
  {}
  
  /*! Read only pointer to TMutTrkPar extrapolated to primary vertex */
  virtual const TMutTrkPar* get_trk_par_vtx() const 
  {return 0;}

  /*! TMutTrkPar (track pars in physics units) extrapolated to primary vertex */
  virtual void set_trk_par_vtx(const TMutTrkPar& trk_par) 
  {}

  /*! Read only pointer to TMutTrkPar with original mutr fit */
  virtual const TMutTrkPar* get_trk_par_mutr() const
  {return 0;}

  /*! TMutTrkPar (track pars in physics units) with original mutr fit */
  virtual void set_trk_par_mutr(const TMutTrkPar& trk_par)
  {}

  //! calculate rapidity at vertex, assuming muo
  virtual double get_rapidity( void ) const;

  /*! Push a TMutTrkPar object onto TMutTrkPar list */
  virtual void push_trk_par(const TMutTrkPar& trk_par)
  {}

  /*! Clear TMutTrkPar list */
  virtual void clear_trk_par_list()
  {}

  /*! Read only access to TMutTrkPar list */
  virtual const trk_par_list* get_trk_par_list() const 
  {return 0;} 

  /*! 
    Returns a const pointer to the TMutTrkPar object closest to
    at gap 0 of the requested station by searching the trk_par_list.
    If the trk_par_list is empty then it returns a const pointer to
    the trk_par object at z_reference.
  */
  virtual const TMutTrkPar* get_trk_par_station(unsigned short) const;

  /*! size of the TMutTrkPar list */ 
  virtual size_t get_n_trk_par() const 
  { return 0; }

  //@}

  //! @name Merit Statistics Interface
  //@{  
  /*! \brief
    unless overloaded, returns track number of degrees of freedom.
    this is the number of associated coordinates - 5
  */
  virtual size_t get_ndf() const;
  
  /*! Chi square statistic of w fit (sum of square of w pulls) */
  virtual double get_w_chi_square() const 
  {return 0;}
  
  /*! Chi square statistic of w fit (sum of square of w pulls) per degree of freedom */
  virtual double get_w_chi_square_pdf() const 
  {return 0;}
  
  /*! Chi square statistic of r fit (sum of square of w pulls) */
  virtual double get_r_chi_square() const 
  {return 0;}
  
  /*! Chi square statistic of r fit (sum of square of w pulls) per degree of freedom */
  virtual double get_r_chi_square_pdf() const 
  {return 0;}
  
  /*! Total chi square (w_chi_square + r_chi_square) */
  virtual double get_chi_square() const 
  {return 0;}
  
  /*! Chi square statistic of w fit (sum of square of w pulls) */  
  virtual void set_w_chi_square(double w_chi_square) 
  {}
  
  /*! Chi square statistic of r fit (sum of square of r pulls) */
  virtual void set_r_chi_square(double r_chi_square) 
  {}
  
  /*! \brief
    sets track number of degrees of freedom.
  */
  virtual void set_ndf( size_t ndf )
  {}
  
  /*! Returns the depth of the deepest associated road */
  //virtual int get_max_road_depth() const;
  
  /*! 
    \brief
    returns hit (TFvtxCoord) pattern. 
    unless overloaded, it is calculated using the associtions between 
    TFvtxTrk and TFvtxCoord
  */
  
  virtual unsigned short get_hit_pattern() const;

  /*! returns the hit pattern for the SVX layers containing a hit in this track*/
  virtual unsigned short get_svxhit_pattern() const;
  
  /*! sets hit (TFvtxCoord) pattern */
  virtual void set_hit_pattern( unsigned short pattern )
  {}
  
  /*! Check if there is a hit in a given station, sector, column on this track */
  virtual bool has_coord(unsigned short cage, unsigned short station, unsigned short sector, unsigned short column) const;

  /*! Check if there is a hit in a given station, sector on this track */
  virtual bool has_coord(unsigned short cage, unsigned short station, unsigned short sector) const;
  
  /*! Check if there is a hit in a given station on this track */
  virtual bool has_coord(unsigned short station) const;
  
  /*! Check if track contains at least one SVX hit in a given layer */
  bool has_svx_coord(unsigned short layer) const;

  //@}

  //! @name Windows Interface
  //@{  
  /*! Phi window (radians) */
  virtual double get_phi_min(unsigned short station) const 
  { return 0; }
  
  /*! Phi window (radians) */
  virtual double get_phi_max(unsigned short station) const 
  { return 0; }
  
  /*! Phi window (radians) */
  virtual void set_phi_min(unsigned short station, double phi_min)
  {}
  
  /*! Phi window (radians) */
  virtual void set_phi_max(unsigned short station, double phi_max)
  {}
  
  /*! Theta window (radians) */
  virtual double get_theta_min(unsigned short station ) const 
  { return 0; }
  
  /*! Theta window (radians) */
  virtual double get_theta_max(unsigned short station) const 
  { return 0; }
  
  /*! Theta window (radians) */
  virtual void set_theta_min(unsigned short station, double theta_min)
  {}
  
  /*! Theta window (radians) */
  virtual void set_theta_max(unsigned short station, double theta_max){;}
  //@}

  //! @name Track Residuals Interface
  //@{  
  /*! Push a TMutTrkRes object onto w residual list */
  virtual void push_w_residual(const TMutTrkRes& residual)
  {}

  /*! Clear residual list */
  virtual void clear_w_residual_list()
  {}

  /*! Number of residual in this track object */
  virtual size_t get_n_w_residual() const 
  {return 0;}

  /*! Read only access to w residual list */
  virtual const residual_list* get_w_residual_list() const 
  {return 0;} 

  /*! Push a r residual onto TFvtxTrk r residual list */
  virtual void push_r_residual(double residual)
  {}

  /*! Clear residual list */
  virtual void clear_r_residual_list()
  {}

  /*! Number of residual in this track object */
  virtual size_t get_n_r_residual() const 
  {return 0;}

  /*! Read only access to w residual list */
  virtual const std::vector<double>* get_r_residual_list() const 
  {return 0;} 

  //@}
  
  //! @name Locators
  //@{    
  /*! Arm [0,1] */
  virtual unsigned short  get_arm() const 
  {return 0;}
  
  /*! Index [0,1024] */
  virtual unsigned short  get_index() const 
  {return 0;}  
  
  /*! Arm [0,1] */
  virtual void set_arm(unsigned short arm)
  {}
  
  /*! Index [0,1024] */
  virtual void  set_index(unsigned short index){}               
  //@}

  //! @name Global Track Attributes
  //@{  

  /*! Get the charge of the track */
  virtual Float_t get_charge() const 
  {return 0;}
  
  /*! Get the charge of the track */
  virtual void set_charge(int charge) 
  {}  
  
  /*! 
    Get the number of TFvtxCoord objects
    associated with this track.
  */
  virtual size_t get_n_coord() const; 
  
  //@}

  //! @name Track Status
  //@{  

  /*! Set the global bit */
  virtual void set_global_fit() 
  {}
  
  /*! Test the global bit */
  virtual bool get_global_fit() const 
  { return 0; }

  /*! Set the kalman bit */
  virtual void set_kalman_fit() {}
  
  /*! Test the kalman bit */
  virtual bool get_kalman_fit() const 
  { return 0; }

  /*! Test the kalman | global bit */
  virtual bool get_reco_success() const 
  { return 0; }

  /*! Set the global fail bit */
  virtual void set_global_fail() 
  {}
  
  /*! Test the global fail bit */
  virtual bool get_global_fail() const 
  { return 0; }

  /*! Set the kalman fail bit */
  virtual void set_kalman_fail() 
  {}
  
  /*! Test the kalman fail bit */
  virtual bool get_kalman_fail() const 
  { return 0; }

  /*! Set the min hits bit */
  virtual void set_reco_min_hits() 
  {}
  
  /*! Test the min hits bit */
  virtual bool get_reco_min_hits() const 
  { return 0; }

  /*! Set the ghost bit */
  virtual void set_ghost() 
  {}
  
  /*! Test the ghost bit */
  virtual bool get_ghost() const 
  { return 0; }

  /*! Set the no fit bit */
  virtual void set_no_fit() 
  {}
  
  /*! Test the no fit bit */
  virtual bool get_no_fit() const
  { return 0; }

  /*! Set association */
  virtual void set_mutr_associated(bool a)
  { std::cout << "Virtual function, doing nothing." << std::endl;}
  
  /*! Test the association */
  virtual bool get_mutr_associated() const
  { return 0; }

  /*! Set the phi swapped association */
  virtual void set_swapped_mutr_associated(bool a)
  { std::cout << "Virtual function, doing nothing." << std::endl;}
  
  /*! Test the phi swapped association */
  virtual bool get_swapped_mutr_associated() const
  { return 0; }

  /*! Set pr cheat bit */
  virtual void set_pr_cheat() 
  {}
  
  /*! Get pr cheat bit */
  virtual bool get_pr_cheat() const 
  { return 0; }

  /*! Get the status word */
  virtual unsigned short get_status() const 
  { return 0;}

  /*! Set the status word */
  virtual void copy_status( const TFvtxTrk& )
  {}
  
  /*! Clear the status word */
  virtual void clear_status() 
  {}

  //@}

  //! @name Dumpers
  //@{    
  /*! Print data members to ostream os, defaults to std::cout */
  virtual void print(std::ostream& os = std::cout) const {}
  //@}

  protected:

  ClassDef(TFvtxTrk,1)
};
  
#endif /* __TFVTXTRK_H__*/



