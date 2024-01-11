#ifndef __TRpcMuoTrk_H__
#define __TRpcMuoTrk_H__

/*!
	\file TRpcMuoTrk.h
	\brief The RPC Track object - linked to PHMuoTrackOut, not MutTrk
	\author Richard Hollis (UCR)
  \version $Revision: 1.2 $
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

class TRpcMuoTrk : public PHKey
{
  
 public:

  //! @name Constructors/Destructors
  //@{    

  //! Default constructor 
  TRpcMuoTrk()
  {}

  //! Destructor 
  virtual ~TRpcMuoTrk()
  {}

  //! Construct with key and location 
  TRpcMuoTrk(const Key& key) : PHKey(key) 
  {} 

  //@}

  //! @name Setters/Getters
  //@{    
  //! Store extropolated hit positions
  //! From the track (specify which mutr_station is the reference and which rpc station is the target)
  virtual void set_muo_trk_number( int number ) {}
  virtual double get_muo_trk_number( ) const { return -9999.; }
  virtual void set_muo_trk_momentum( double px, double py, double pz ) { }
  virtual double get_muo_trk_momentum( int num ) const { return -9999; }
  
  virtual void set_trk_extrapolated_hit_x( int mutr_station, int rpc_station, double hitx ) {}
  virtual double get_trk_extrapolated_hit_x( int mutr_station, int rpc_station ) const { return -9999.; }
  virtual void set_trk_extrapolated_hit_y( int mutr_station, int rpc_station, double hity ) {}
  virtual double get_trk_extrapolated_hit_y( int mutr_station, int rpc_station ) const { return -9999.; }
  //! From the vertex
  virtual void set_vtx_extrapolated_hit_x( int rpc_station, double hitx ) {}
  virtual double get_vtx_extrapolated_hit_x( int rpc_station ) const { return -9999.; }
  virtual void set_vtx_extrapolated_hit_y( int rpc_station, double hity ) {}
  virtual double get_vtx_extrapolated_hit_y( int rpc_station ) const { return -9999.; }
  //! From the "best" associated muID road
  virtual void set_muid_extrapolated_hit_x( int rpc_station, double hitx ) {}
  virtual double get_muid_extrapolated_hit_x( int rpc_station ) const { return -9999.; }
  virtual void set_muid_extrapolated_hit_y( int rpc_station, double hity ) {}
  virtual double get_muid_extrapolated_hit_y( int rpc_station ) const { return -9999.; }
  
  //! Store DCA between track and closest cluster
  //! DCA from track to RPC
  virtual void set_dca_trk( int mutr_station, int rpc_station, double dca ) {}
  virtual double get_dca_trk(int mutr_station, int rpc_station ) const { return -9999.; }
  //! DCA from vertex to RPC
  virtual void set_dca_vtx( int rpc_station, double dca ) {}
  virtual double get_dca_vtx( int rpc_station ) const { return -9999.; }
  //! DCA from muid to RPC (although RPC1 information may not make too much sense)
  virtual void set_dca_muid( int rpc_station, double dca ) {}
  virtual double get_dca_muid( int rpc_station ) const { return -9999.; }
  
  //! Associated RPC Cluster in stations 1,3
  //! From the track
  virtual void set_rpcclus_time( int mutr_station, int rpc_station, double clustime ) {}
  virtual double get_rpcclus_time(int mutr_station, int rpc_station ) const { return 0; }
  virtual void set_rpcclus_hitpos( int mutr_station, int rpc_station, double clushitpos ) {}
  virtual double get_rpcclus_hitpos(int mutr_station, int rpc_station ) const { return 0; }
  //! From the vertex
  virtual void set_rpcclus_time_vtx( int rpc_station, double clustime ) {}
  virtual double get_rpcclus_time_vtx( int rpc_station ) const { return 0; }
  virtual void set_rpcclus_hitpos_vtx( int rpc_station, double clushitpos ) {}
  virtual double get_rpcclus_hitpos_vtx( int rpc_station ) const { return 0; }
  //! From the muid
  virtual void set_rpcclus_time_muid( int rpc_station, double clustime ) {}
  virtual double get_rpcclus_time_muid( int rpc_station ) const { return 0; }
  virtual void set_rpcclus_hitpos_muid( int rpc_station, double clushitpos ) {}
  virtual double get_rpcclus_hitpos_muid( int rpc_station ) const { return 0; }

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

  //! @name Dumpers
  //@{    
  //! Print data members to ostream os, defaults to std::cout
  virtual void print(std::ostream& os, bool max) const {}
  virtual void print(std::ostream& os = std::cout) const { print(os, false); }
  //@}

  ClassDef(TRpcMuoTrk,1)
};
  
#endif 


