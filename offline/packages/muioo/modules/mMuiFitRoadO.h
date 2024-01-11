// $Id: mMuiFitRoadO.h,v 1.1 2006/04/22 01:59:14 hpereira Exp $
#ifndef __mMuiFitRoad_h__
#define __mMuiFitRoad_h__

/*!
  \file    mMuiFitRoadO.h
  \brief   road straight track fit
  \author  Hugo Pereira
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:59:14 $
*/

#include <PHGslMatrix.h>
#include <PHTimeServer.h>
#include <list>

#include "TMuiClusterMapO.h"
#include "TMuiRoadMapO.h"

// forward declaration
class PHCompositeNode;
class mMuiFitRoadOPar;

//! road straight track fit
class mMuiFitRoadO
{

	public:
	
  //! constructor
	mMuiFitRoadO(); 
  
  //! destructor
	virtual ~mMuiFitRoadO()
	{}

	//! event method
	virtual PHBoolean event(PHCompositeNode*);	

	private:
	
	//! Set IOC pointers
	void set_interface_ptrs(PHCompositeNode* top_node);
	
	//! Loop over roads
	void road_loop();
	
 	//! store necessary matrix for measurement
  class Node
  {
  	
  	public:
  	
  	//! constructor (take a cluster and a reference z
  	Node( TMuiClusterMapO::pointer clus_ptr, const double &z );
  	
  	//! vector of measurement matrices
  	PHGslMatrix _m;
  	
  	//! vector of associated gain matrices
  	PHGslMatrix _g;
  	
  	//! vector of associated transfer matrix
  	PHGslMatrix _h;
  
  };
  
	//! shortcut for list of nodes
	typedef std::list< Node > node_list;
  
	//! store fit parameters
	class RoadFit
	{
		
		public:
		
		//! constructor
		RoadFit( void );
		
		//! add a coordinate to the fit
		void add_cluster( TMuiClusterMapO::pointer clus_ptr )
		{ _nodes.push_back( Node( clus_ptr, _z ) ); }
		
		//! perform the fit. Return true if successful
		bool fit( void );
		
		//! reference z
		void set_z( const double z )
		{ _z = z; }
		
		//! reference z
		double get_z( void ) const
		{ return _z; }
		
		//! retrieve chisquare
		double get_chisquare( void ) const 
		{ return _chisquare; }
		
		//! number of clusters
		int get_n_clusters( void ) const 
		{ return _nodes.size(); }
		
		//! degrees of freedom
		int get_ndf( void ) const
		{ return get_n_clusters() - 4; }
		
		//! retrieve state vector matrix
		const PHGslMatrix& get_state( void )  const
		{ return _state; }
		
		//! retrieve gain matrix
		const PHGslMatrix& get_gain( void )  const
		{ return _gain; }
		
		private:
		
		//! road chisquare
		double _chisquare;
		
		//! road parameters (4x1) (state vector)
		PHGslMatrix _state;
		
		//! gain matrix (4x4)
		PHGslMatrix _gain;
	
		//! reference z
		double _z;
  	
		//! list of measurement
		node_list _nodes;
	
	};

	//! fill road with fit results
	void fill_road( TMuiRoadMapO::pointer, const RoadFit& ) const;
	
	//! pointer to parameters module
	mMuiFitRoadOPar *_mod_par;
	
	//! pointer to the Road Map
	TMuiRoadMapO *_road_map;

	//! module timer
	PHTimeServer::timer _timer;

};

#endif
