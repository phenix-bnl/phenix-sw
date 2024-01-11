// $Id: TRpcMuoTrk_v1.h,v 1.1 2012/04/03 18:47:21 phnxrpc Exp $

#ifndef __TRpcMuoTrk_v1_H__
#define __TRpcMuoTrk_v1_H__

/*!
	\file TRpcMuoTrk_v1.h
	\brief The RPC Track object 
	\author Richard Hollis
  \version $Revision: 1.1 $
  \date    $Date: 2012/04/03 18:47:21 $
*/

#include "TRpcMuoTrk.h"
#include <vector>

/*! @ingroup interface */
//!  The RPC Track object 

/*! 
  <b>The RPC Muo Track Object</b><br>

  The RPC track object presents an interface to track parameter
  objects, PHMuoTracksOut.  

*/
class TRpcMuoTrk_v1 : public TRpcMuoTrk
{
  
 public:

  //! @name Constructors/Destructors
  //@{    

  //! constructor 
  TRpcMuoTrk_v1();

  //! constructor 
  TRpcMuoTrk_v1(const Key&, 
	     UShort_t arm, 
	     UShort_t index);

  //! constructor 
  TRpcMuoTrk_v1(const TRpcMuoTrk* base_ptr);

  //! constructor 
  TRpcMuoTrk_v1(const TRpcMuoTrk& base_ref);

  //! Destructor 
  virtual ~TRpcMuoTrk_v1()
  {}

  //@}

  //! @name Setters/Getters
  //@{    
  //! Store extropolated hit positions
  //! From the track (specify which mutr_station is the reference and which rpc station is the target)
  virtual void set_muo_trk_number( int number ) { _muo_trk_number = number; }
  virtual double get_muo_trk_number( ) const { return _muo_trk_number; }
  virtual void set_muo_trk_momentum( double px, double py, double pz ) { _muo_trk_px = px; _muo_trk_py = py; _muo_trk_pz = pz; }
  virtual double get_muo_trk_momentum( int num ) const { return (num==0 ? _muo_trk_px: (num==1 ? _muo_trk_py:_muo_trk_pz)); }

  virtual void set_trk_extrapolated_hit_x( int mutr_station, int rpc_station, double hitx ) { _extrap_trk_hit_x[mutr_station][rpc_station]=hitx; }
  virtual double get_trk_extrapolated_hit_x( int mutr_station, int rpc_station ) const { return _extrap_trk_hit_x[mutr_station][rpc_station]; }
  virtual void set_trk_extrapolated_hit_y( int mutr_station, int rpc_station, double hity ) { _extrap_trk_hit_y[mutr_station][rpc_station]=hity; }
  virtual double get_trk_extrapolated_hit_y( int mutr_station, int rpc_station ) const { return _extrap_trk_hit_y[mutr_station][rpc_station]; }
  //! From the vertex
  virtual void set_vtx_extrapolated_hit_x( int rpc_station, double hitx ) { _extrap_vtx_hit_x[rpc_station]=hitx; }
  virtual double get_vtx_extrapolated_hit_x( int rpc_station ) const { return _extrap_vtx_hit_x[rpc_station]; }
  virtual void set_vtx_extrapolated_hit_y( int rpc_station, double hity ) { _extrap_vtx_hit_y[rpc_station]=hity; }
  virtual double get_vtx_extrapolated_hit_y( int rpc_station ) const { return _extrap_vtx_hit_y[rpc_station]; }
  //! From the "best" associated muID road
  virtual void set_muid_extrapolated_hit_x( int rpc_station, double hitx ) { _extrap_muid_hit_x[rpc_station]=hitx; }
  virtual double get_muid_extrapolated_hit_x( int rpc_station ) const { return _extrap_muid_hit_x[rpc_station]; }
  virtual void set_muid_extrapolated_hit_y( int rpc_station, double hity ) { _extrap_muid_hit_y[rpc_station]=hity; }
  virtual double get_muid_extrapolated_hit_y( int rpc_station ) const { return _extrap_muid_hit_y[rpc_station]; }
  
  //! Store DCA between track and closest cluster
  //! DCA from track to RPC
  virtual void set_dca_trk( int mutr_station, int rpc_station, double dca ) { _dca_trk[mutr_station][rpc_station]=dca; }
  virtual double get_dca_trk(int mutr_station, int rpc_station ) const { return _dca_trk[mutr_station][rpc_station]; }
  //! DCA from vertex to RPC
  virtual void set_dca_vtx( int rpc_station, double dca ) { _dca_vtx[rpc_station]=dca; }
  virtual double get_dca_vtx( int rpc_station ) const { return _dca_vtx[rpc_station]; }
  //! DCA from muid to RPC (although RPC1 information may not make too much sense)
  virtual void set_dca_muid( int rpc_station, double dca ) { _dca_muid[rpc_station]=dca; }
  virtual double get_dca_muid( int rpc_station ) const { return _dca_muid[rpc_station]; }
  
  //! Associated RPC Cluster in stations 1,3
  //! From the track
  virtual void set_rpcclus_time( int mutr_station, int rpc_station, double clustime ) { _rpc_clus_time[mutr_station][rpc_station]=clustime; }
  virtual double get_rpcclus_time(int mutr_station, int rpc_station ) const { return _rpc_clus_time[mutr_station][rpc_station]; }
  virtual void set_rpcclus_hitpos( int mutr_station, int rpc_station, double clushitpos ) { _rpc_clus_hit_pos[mutr_station][rpc_station]=clushitpos; }
  virtual double get_rpcclus_hitpos(int mutr_station, int rpc_station ) const { return _rpc_clus_hit_pos[mutr_station][rpc_station]; }
  //! From the vertex
  virtual void set_rpcclus_time_vtx( int rpc_station, double clustime ) { _rpc_clus_vtx_time[rpc_station]=clustime; }
  virtual double get_rpcclus_time_vtx( int rpc_station ) const { return _rpc_clus_vtx_time[rpc_station]; }
  virtual void set_rpcclus_hitpos_vtx( int rpc_station, double clushitpos ) { _rpc_clus_vtx_hit_pos[rpc_station]=clushitpos; }
  virtual double get_rpcclus_hitpos_vtx( int rpc_station ) const { return _rpc_clus_vtx_hit_pos[rpc_station]; }
  //! From the muid
  virtual void set_rpcclus_time_muid( int rpc_station, double clustime ) { _rpc_clus_muid_time[rpc_station]=clustime; }
  virtual double get_rpcclus_time_muid( int rpc_station ) const { return _rpc_clus_muid_time[rpc_station]; }
  virtual void set_rpcclus_hitpos_muid( int rpc_station, double clushitpos ) { _rpc_clus_muid_hit_pos[rpc_station]=clushitpos; }
  virtual double get_rpcclus_hitpos_muid( int rpc_station ) const { return _rpc_clus_muid_hit_pos[rpc_station]; }

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

	//! Muo Track Number
	int _muo_trk_number;

	//! Muo Track Momentum
	double _muo_trk_px;
	double _muo_trk_py;
	double _muo_trk_pz;
	
	//! extrapolatedhits
	double _extrap_trk_hit_x[2][2];//[mutrstation][rpcstation]
	double _extrap_trk_hit_y[2][2];//[mutrstation][rpcstation]
	double _extrap_vtx_hit_x[2];//[rpcstation]
	double _extrap_vtx_hit_y[2];//[rpcstation]
	double _extrap_muid_hit_x[2];//[rpcstation]
	double _extrap_muid_hit_y[2];//[rpcstation]
	
	//! RPC-DCA variables
	double _dca_trk[2][2];//[mutrstation][rpcstation]
	double _dca_vtx[2];//[rpcstation]
	double _dca_muid[2];//[rpcstation]
	
	//! Associated RPC cluster (third station)
	double _rpc_clus_time[2][2];//[mutrstation][rpcstation]
	double _rpc_clus_vtx_time[2];//[rpcstation]
	double _rpc_clus_muid_time[2];//[rpcstation]
	double _rpc_clus_hit_pos[2][2];//[mutrstation][rpcstation]
	double _rpc_clus_vtx_hit_pos[2];//[rpcstation]
	double _rpc_clus_muid_hit_pos[2];//[rpcstation]
  
  ClassDef(TRpcMuoTrk_v1,1)
};
  
#endif 


