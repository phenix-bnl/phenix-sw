#ifndef __TRpcTrk_v2_H__
#define __TRpcTrk_v2_H__

/*!
	\file TRpcTrk_v2.h
	\brief The RPC Track object 
	\author H. Pereira Da Costa
  \version $Revision: 1.2 $
  \date    $Date: 2014/02/15 15:51:01 $
*/

#include "TRpcTrk.h"
#include <vector>

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

class TRpcTrk_v2 : public TRpcTrk
{
  
 public:

  //! @name Constructors/Destructors
  //@{    

  //! constructor 
  TRpcTrk_v2();

  //! constructor 
  TRpcTrk_v2(const Key&, 
	     UShort_t arm, 
	     UShort_t index);

  //! constructor 
  TRpcTrk_v2(const TRpcTrk* base_ptr);

  //! constructor 
  TRpcTrk_v2(const TRpcTrk& base_ref);

  //! Destructor 
  virtual ~TRpcTrk_v2()
  {}

  //@}

  //! @name Track Parameters Interface
  //@{  

  //! Read only pointer to TMutTrkPar at z reference 
  virtual const TMutTrkPar* get_trk_par() const 
  {return &_trk_par;}

  //! TMutTrkPar (track pars in physics units) 
  virtual void set_trk_par(const TMutTrkPar& trk_par) 
  { _trk_par = trk_par; }
  
  //! Read only pointer to TMutTrkPar extrapolated to primary vertex 
  virtual const TMutTrkPar* get_trk_par_vtx() const 
  {return &_trk_par_vtx;}

  //! TMutTrkPar (track pars in physics units) extrapolated to primary vertex 
  virtual void set_trk_par_vtx(const TMutTrkPar& trk_par) 
  { _trk_par_vtx = trk_par; }

  //! Push a TMutTrkPar object onto TMutTrkPar list 
  virtual void push_trk_par(const TMutTrkPar& trk_par)
  { _trk_par_list.push_back(trk_par);}

  //! Clear TMutTrkPar list 
  virtual void clear_trk_par_list()
  {_trk_par_list.clear();}

  //! Read only access to TMutTrkPar list 
  virtual const trk_par_list* get_trk_par_list() const 
  {return &_trk_par_list;} 

  //! size of the TMutTrkPar list  
  virtual size_t get_n_trk_par() const 
	{ return _trk_par_list.size(); }

  //@}

  //! @name Fit Parameters Interface (Linear Model)
  //@{  
  //! Read only pointer to TMutFitPar 
  virtual const TMutFitPar* get_fit_par() const 
  {return &_fit_par;}
  
  //! TMutFitPar (track pars in geometric units) 
  virtual void set_fit_par(const TMutFitPar* fit_par) 
  {_fit_par = *fit_par;}
  
  //! TMutFitPar (track pars in geometric units) 
  virtual void set_fit_par(const TMutFitPar& fit_par) 
  {_fit_par = fit_par;}
  
  //@}

  //! @name Merit Statistics Interface
  //@{  
  /*! \brief
    unless overloaded, returns track number of degrees of freedom.
    this is 2 x get_n_coords - 5
  */
  virtual size_t get_ndf() const
	{ return _ndf; }
  
  //! sets track number of degrees of freedom.
  virtual void set_ndf( size_t ndf )
  { _ndf = ndf; }
  
  //! Chi square statistic
  virtual double get_chi_square() const 
  {return _chi_square;}

  //! Chi square statistic  
  virtual void set_chi_square(double chi_square) 
  { _chi_square = chi_square; }
  
  /*! 
    \brief
    returns hit (TRPCCoord) pattern. 
    unless overloaded, it is calculated using the associtions between 
    TRpcTrk and TMutCoord
  */
  virtual UShort_t get_hit_pattern() const
	{ return _hit_pattern; }
  
  //! sets hit (TMutCoord) pattern 
  virtual void set_hit_pattern( UShort_t pattern )
  { _hit_pattern = pattern; }
  

  //! Store extropolated hit positions
  //! From the track
  virtual void set_trk_extrapolated_hit_x( double hitx ) { _extrap_trk_hit_x = hitx; }
  virtual double get_trk_extrapolated_hit_x() const { return _extrap_trk_hit_x; }
  virtual void set_trk_extrapolated_hit_y( double hity ) { _extrap_trk_hit_y = hity; }
  virtual double get_trk_extrapolated_hit_y() const { return _extrap_trk_hit_y; }
  //! From the vertex
  virtual void set_trk_vtx_extrapolated_hit_x( double hitx ) { _extrap_vtx_hit_x = hitx; }
  virtual double get_trk_vtx_extrapolated_hit_x() const { return _extrap_vtx_hit_x; }
  virtual void set_trk_vtx_extrapolated_hit_y( double hity ) { _extrap_vtx_hit_y = hity; }
  virtual double get_trk_vtx_extrapolated_hit_y() const { return _extrap_vtx_hit_y; }
  
  //! Store the distance of closest approach
  //! DCA from track to RPC
  virtual void set_dca_trk( double dca ) { _dca_trk = dca; }
  virtual double get_dca_trk() const { return _dca_trk; }
  //! DCA from vertex to RPC
  virtual void set_dca_trk_vtx( double dca ) { _dca_trk_vtx = dca; }
  virtual double get_dca_trk_vtx() const { return _dca_trk_vtx; }
  //! DCA Diff from vertex to RPC
  virtual void set_dca_diff( double dcadiff ) { _dca_diff = dcadiff; }
  virtual double get_dca_diff() const { return _dca_diff; }

  //! Associated RPC Cluster in station 3
  //! From the track
  //virtual void set_rpcclus3( TRpcClus *clus ) { _rpc_clus3 = clus; }
  //virtual TRpcClus* get_rpcclus3() const { return _rpc_clus3; }
  virtual void set_rpcclus3time( double clustime ) { _rpc_clus3_time = clustime; }
  virtual double get_rpcclus3time() const { return _rpc_clus3_time; }
  virtual void set_rpcclus3hitpos( double clushitpos ) { _rpc_clus3_hit_pos = clushitpos; }
  virtual double get_rpcclus3hitpos() const { return _rpc_clus3_hit_pos; }
  //! From the vertex
  //virtual void set_rpcclus3_vtx( TRpcClus *clus ) { _rpc_clus3_vtx = clus; }
  //virtual TRpcClus* get_rpcclus3_vtx() const { return _rpc_clus3_vtx; }
  virtual void set_rpcclus3time_vtx( double clustime ) { _rpc_clus3_vtx_time = clustime; }
  virtual double get_rpcclus3time_vtx() const { return _rpc_clus3_vtx_time; }
  virtual void set_rpcclus3hitpos_vtx( double clushitpos ) { _rpc_clus3_vtx_hit_pos = clushitpos; }
  virtual double get_rpcclus3hitpos_vtx() const { return _rpc_clus3_vtx_hit_pos; }

  //! Corrected pT
  virtual void set_corr_pT( double corrpT ) { _corr_pT = corrpT; }
  virtual double get_corr_pT() const { return _corr_pT; }


  //@}
  
  //! @name Locators
  //@{    
  //! Arm 
  virtual UShort_t  get_arm() const 
  {return _arm;}
 
  //! Arm
  virtual void  set_arm(UShort_t arm)
	{ _arm = arm; }         
  
  //! Index
  virtual UShort_t  get_index() const 
  {return _index;}  
  
  //! Index
  virtual void  set_index(UShort_t index)
	{ _index = index; }               
  //@}

  //! @name Global Track Attributes
  //@{  

  //! Get the charge of the track 
  virtual Float_t get_charge() const 
  {return _trk_par.get_charge();}
  
  //! Get the charge of the track 
  virtual void set_charge(int charge) 
  {_trk_par.set_charge(charge);}
  
  //@}

  //! @name Track Status
  //@{  

  //! Get the status word 
  virtual UShort_t get_status() const 
  { return _status;}

  //! Clear the status word 
  virtual void clear_status() 
  { _status = 0; }

  void set_kalman_fit() 
  {_status |= 1<<KALMAN_FIT;}

  bool get_kalman_fit() const 
  { return (_status & 1<<KALMAN_FIT) != 0;}

  void set_ghost() 
  {_status |= 1<<GHOST;}

  bool get_ghost() const 
  { return (_status & 1<<GHOST) != 0;}

  void set_no_estimate() 
  {_status |= 1<<NO_ESTIMATE;}
  
  bool get_no_estimate() const 
  { return (_status & 1<<NO_ESTIMATE) != 0;}

  void set_no_fit() 
  {_status |= 1<<NO_FIT;}
  
  bool get_no_fit() const 
  { return (_status & 1<<NO_FIT) != 0;}

  bool get_reco_success() const 
  { return get_kalman_fit() && !get_kalman_fail(); }

  //@}

  //! @name Dumpers
  //@{    
  //! Print data members to ostream os, defaults to std::cout   
  virtual void print(std::ostream& os = std::cout, bool max=false) const; 
			
  //@}

	private:
			
	//! arm location
	UShort_t _arm;
	
	//! trk index
	UShort_t _index;

	//! main track parameter (usually at station 1)
  TMutTrkPar _trk_par;  
	
	//! vtx track parameter (either at z=0 or Z=Z_BBC)
  TMutTrkPar _trk_par_vtx;
	
	//! list of track parameters for each associated coordinate
  std::vector<TMutTrkPar> _trk_par_list;
	
	//! fit parameters for straight track fit
  TMutFitPar _fit_par;

  //! chisquare
  double _chi_square;
  
  //! track number of degrees of freedom
  size_t _ndf;
  
  //! track hit pattern
  UShort_t _hit_pattern;
   
  //! track status 
  UShort_t _status;
	
  //! extrapolatedhits
  double _extrap_trk_hit_x;
  double _extrap_trk_hit_y;
  double _extrap_vtx_hit_x;
  double _extrap_vtx_hit_y;
  
  //! RPC-DCA variables
  double _dca_trk;
  double _dca_trk_vtx;
  double _dca_diff;
  
  //! Associated RPC cluster (third station)
  //TRpcClus *_rpc_clus3;
  //TRpcClus *_rpc_clus3_vtx;
  double _rpc_clus3_time;
  double _rpc_clus3_vtx_time;
  double _rpc_clus3_hit_pos;
  double _rpc_clus3_vtx_hit_pos;
  
  //! RPC-Corrected pT
  double _corr_pT;


  ClassDef(TRpcTrk_v2,2)
};
  
#endif 


