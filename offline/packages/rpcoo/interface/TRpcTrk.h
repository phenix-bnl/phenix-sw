// $Id: TRpcTrk.h,v 1.6 2014/01/26 17:43:36 bbannier Exp $

#ifndef __TRpcTrk_H__
#define __TRpcTrk_H__

/*!
	\file TRpcTrk.h
	\brief The RPC Track object 
	\author H. Pereira Da Costa
  \version $Revision: 1.6 $
  \date    $Date: 2014/01/26 17:43:36 $
*/

#include <TDataType.h>
#include "TRpcClus.h"
#include <PHKey.hh>

#include <TMutTrkPar.hh>
#include <TMutFitPar.hh>

/*! @ingroup interface */
//!  The RPC Track object 

/*! 
  <b>The RPC Track Object</b><br>

  The RPC track object presents an interface to track parameter
  objects, TMutTrkPar.  
  The TMutTrkPar object is in physical units (momentum and position).
  It includes an interface to the full 
  covariance matrix.  The track parameters are stored at the first gap
  of each station at the primary vertex and at a reference surface used
  in the global fit.

  <p>
  To access all the hits associated with a track one uses the standard
  syntax for accessing associated objects in MUTOO.

*/

class TRpcClus;

class TRpcTrk : public PHKey
{
  
 public:

  //! Name of the list of TMutTrkPar contained in this object 
  typedef std::vector<TMutTrkPar> trk_par_list;
 
  //! Name of a const iterator to residual list 
  typedef std::vector<TMutTrkPar>::const_iterator const_trk_par_iterator;

	//! track status enumeration 
	/*! 
		note: the integer values are meant to match the
		TMutTrk status values. There is nothing wrong if they dont
		but they make the status integer (bitwise) inconsistant when 
		looked at directly
	*/ 
  enum Status {
		
		//! no fit was performed
    NO_FIT = 0, 
				
		//! kalman fit was performed
    KALMAN_FIT=2, 
				
		//! kalman fit performed and failed
	  KALMAN_FAIL=4, 
	
		//! track is identified as ghost
    GHOST=6, 
		
		//! no starting initial momentum estimate could be done
    NO_ESTIMATE=8, 
		
  };

  //! @name Constructors/Destructors
  //@{    

  //! Default constructor 
  TRpcTrk()
  {}

  //! Destructor 
  virtual ~TRpcTrk()
  {}

  //! Construct with key and location 
  TRpcTrk(const Key& key) : PHKey(key) 
  {} 

  //@}

  //! @name Track Parameters Interface
  //@{  

  //! Read only pointer to TMutTrkPar at z reference 
  virtual const TMutTrkPar* get_trk_par() const 
  {return 0;}

  //! TMutTrkPar (track pars in physics units) 
  virtual void set_trk_par(const TMutTrkPar& trk_par) 
  {}
  
  //! Read only pointer to TMutTrkPar extrapolated to primary vertex 
  virtual const TMutTrkPar* get_trk_par_vtx() const 
  {return 0;}

  //! TMutTrkPar (track pars in physics units) extrapolated to primary vertex 
  virtual void set_trk_par_vtx(const TMutTrkPar& trk_par) 
  {}

  //! Push a TMutTrkPar object onto TMutTrkPar list 
  virtual void push_trk_par(const TMutTrkPar& trk_par)
  {}

  //! Clear TMutTrkPar list 
  virtual void clear_trk_par_list()
  {}

  //! Read only access to TMutTrkPar list 
  virtual const trk_par_list* get_trk_par_list() const 
  {return 0;} 

	//! Returns a const pointer to the TMutTrkPar object in requested station. 
  /*! 
    Search the trk_par_list. Look for TMutTrkPar with z the closest to the station
    If the trk_par_list is empty then it returns a const pointer to
    the trk_par object at z_reference.
  */
  virtual const TMutTrkPar* get_trk_par_station(UShort_t) const;

  //! size of the TMutTrkPar list  
  virtual size_t get_n_trk_par() const 
  { return 0; }

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
    this is 2 x get_n_coords - 5
  */
  virtual size_t get_ndf() const;
  
  //! sets track number of degrees of freedom.
  virtual void set_ndf( size_t ndf )
  {}
  
  //! Chi square statistic
  virtual double get_chi_square() const 
  {return 0;}
  
  //! Chi square statistic per degree of freedom 
  virtual double get_chi_square_pdf() const 
  {return (get_ndf()>0) ? get_chi_square()/get_ndf():0;}

  //! Chi square statistic
  virtual void set_chi_square(double chi_square) 
  {}
  
  /*! 
    \brief
    returns hit (TRPCCoord) pattern. 
    unless overloaded, it is calculated using the associtions between 
    TRpcTrk and TMutCoord
  */
  virtual UShort_t get_hit_pattern() const;
  
  //! sets hit (TMutCoord) pattern 
  virtual void set_hit_pattern( UShort_t pattern )
  {}
  
  //! Store extropolated hit positions
  //! From the track
  virtual void set_trk_extrapolated_hit_x( double hitx ) {}
  virtual double get_trk_extrapolated_hit_x() const { return -9999.; }
  virtual void set_trk_extrapolated_hit_y( double hity ) {}
  virtual double get_trk_extrapolated_hit_y() const { return -9999.; }
  //! From the vertex
  virtual void set_trk_vtx_extrapolated_hit_x( double hitx ) {}
  virtual double get_trk_vtx_extrapolated_hit_x() const { return -9999.; }
  virtual void set_trk_vtx_extrapolated_hit_y( double hity ) {}
  virtual double get_trk_vtx_extrapolated_hit_y() const { return -9999.; }
  
  //! DCA from track to RPC
  virtual void set_dca_trk( double dca ) {}
  virtual double get_dca_trk() const { return -9999.; }

  //! DCA from vertex to RPC
  virtual void set_dca_trk_vtx( double dca ) {}
  virtual double get_dca_trk_vtx() const { return -9999.; }
  
  //! DCA Diff from vertex to RPC
  virtual void set_dca_diff( double dcadiff ) {}
  virtual double get_dca_diff() const { return -9999.; }

  //! Associated RPC Cluster in station 3
  //! From the track
  virtual void set_rpcclus3( TRpcClus *clus ) {}
  virtual TRpcClus* get_rpcclus3() const { return NULL; }
  virtual void set_rpcclus3time( double clustime ) {}
  virtual double get_rpcclus3time() const { return 0; }
  virtual void set_rpcclus3hitpos( double clushitpos ) {}
  virtual double get_rpcclus3hitpos() const { return 0; }
  //! From the vertex
  virtual void set_rpcclus3_vtx( TRpcClus *clus ) {}
  virtual TRpcClus* get_rpcclus3_vtx() const { return NULL; }
  virtual void set_rpcclus3time_vtx( double clustime ) {}
  virtual double get_rpcclus3time_vtx() const { return 0; }
  virtual void set_rpcclus3hitpos_vtx( double clushitpos ) {}
  virtual double get_rpcclus3hitpos_vtx() const { return 0; }

  //! Corrected pT
  virtual void set_corr_pT( double corrpT ) {}
  virtual double get_corr_pT() const { return -9999.; }
  
  //! Store extropolated hit positions
  //! From the track
  virtual void set_trk_extrapolated_hit_x1( double hitx ) {}
  virtual double get_trk_extrapolated_hit_x1() const { return -9999.; }
  virtual void set_trk_extrapolated_hit_y1( double hity ) {}
  virtual double get_trk_extrapolated_hit_y1() const { return -9999.; }
  //! From the vertex
  virtual void set_trk_vtx_extrapolated_hit_x1( double hitx ) {}
  virtual double get_trk_vtx_extrapolated_hit_x1() const { return -9999.; }
  virtual void set_trk_vtx_extrapolated_hit_y1( double hity ) {}
  virtual double get_trk_vtx_extrapolated_hit_y1() const { return -9999.; }
  
  //! DCA from track to RPC
  virtual void set_dca_trk1( double dca ) {}
  virtual double get_dca_trk1() const { return -9999.; }
  virtual void set_rpcclus1time( double clustime ) { }
  virtual double get_rpcclus1time() const { return -9999; }

  //! DCA from vertex to RPC
  virtual void set_dca_trk_vtx1( double dca ) {}
  virtual double get_dca_trk_vtx1() const { return -9999.; }
  virtual void set_rpcclus1time_vtx( double clustime ) { }
  virtual double get_rpcclus1time_vtx() const { return -9999; }
  
  //@}
  
  //! @name Locators
  //@{    
  //! Arm [0,1] 
  virtual UShort_t  get_arm() const 
  {return 0;}
  
  //! Index [0,1024] 
  virtual UShort_t  get_index() const 
  {return 0;}  
  
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
  
  //! number of TRpcCoord objects
  virtual size_t get_n_coord() const; 
  
  //@}

  //! @name Track Status
  //@{  

  /*! Set the kalman bit */
  virtual void set_kalman_fit() {}
	
  /*! Test the kalman bit */
  virtual bool get_kalman_fit() const 
  { return 0; }

  /*! Set the kalman fail bit */
  virtual void set_kalman_fail() 
  {}
  
  /*! Test the kalman fail bit */
  virtual bool get_kalman_fail() const 
  { return 0; }

  /*! Set the ghost bit */
  virtual void set_ghost() 
  {}
  
  /*! Test the ghost bit */
  virtual bool get_ghost() const 
  { return 0; }

  /*! Set the no estimate bit */
  virtual void set_no_estimate() 
  {}
 
  /*! Set the no fit bit */
  virtual void set_no_fit() 
  {}
 
  /*! Test the no estimate bit */
  virtual bool get_no_estimate() const 
  { return 0; }

  /*! Test the kalman | global bit */
  virtual bool get_reco_success() const 
  { return 0; }

  //! Get the status word 
  virtual UShort_t get_status() const 
  { return 0;}

  //! Clear the status word 
  virtual void clear_status() 
  {}

  //@}

  //! @name Dumpers
  //@{    
  //! Print data members to ostream os, defaults to std::cout
  virtual void print(std::ostream& os, bool max) const {}
  virtual void print(std::ostream& os = std::cout) const { print(os, false); }
  //@}

  ClassDef(TRpcTrk,1)
};
  
#endif 


