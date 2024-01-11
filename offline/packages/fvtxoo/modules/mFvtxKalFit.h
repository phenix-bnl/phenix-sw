// $Id: mFvtxKalFit.h,v 1.19 2016/04/03 21:01:21 slash Exp $
#ifndef __mFvtxKalFit_h__
#define __mFvtxKalFit_h__

/*!
	\file		mFvtxKalFit.h
	\brief	 track fit kalman filter module
	\author	 Melynda Brooks
	\version $Revision: 1.19 $
	\date		$Date: 2016/04/03 21:01:21 $
*/

#include<FVTXOO.h>
#include<PHTimeServer.h>
#include<TMutKalmanUtil.h>
#include<TFvtxTrkMap.h>
#include<TFvtxCompactTrkMap.h>
#include<TFvtxCompactCoordMap.h>
#include<TMutTrkPar.hh>
#include<TFvtxMCHitMap.h>
#include<TFvtxHitMap.h>
#include<TFvtxCoordMap.h>
#include<TFvtxSvxClusterMap.h>
#include<TMutGapCoordMap.h>
#include<TMutCoordMap.h>

#include<TMutKalmanFilter.h>

#include <mFvtxModuleBase.h>

#include "mFvtxKalFitPar.h"
#include "PHGslMatrix.h"
#include <TTree.h>
#include <PHGslRng.h>

#include<list>
#include<boost/array.hpp>

class PHCompositeNode;
class PHTrackIntegratorKF;
/*! \ingroup modules */
//! single track kalman filter fit. 
/*!
 single track kalman filter fit. 
<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mFvtxKalFitPar*</td>
<td> Parameter Table </td>
<td> imFvtxable </td>
</tr>
<tr>
<td> TFvtxTrkMap*</td>
<td> Track IOC</td>
<td> Fvtxable </td>
</table>
*/

#include <SvxSnglPisaHit.h>

class mFvtxKalFit : public mFvtxModuleBase 
{

	public: 

	//! constructor
	mFvtxKalFit(); 
	
	//! destructor
	virtual ~mFvtxKalFit();

	//! event method
	virtual PHBoolean event(PHCompositeNode*);	
	
	//! output evaluation tree initialisation 
	void init_tree( void );
	
	//! close output evaluation tree
	void finish_evaluation( void );

	void init(PHCompositeNode* top_node) {}
	void init_run(PHCompositeNode* top_node);
	void end(PHCompositeNode* top_node);
	
        //! set using svx pisa hit or cluster
        void set_use_svx_cluster(bool flag) { _use_svx_cluster = flag;}
	
	//! fill a single track in the compact track container
	static void add_compact_track(TFvtxTrkMap::pointer trk_ptr, TFvtxCompactTrkMap* ctrk_map);

	//! fill a compact coordinate container
	static void add_compact_coord(TFvtxCoordMap::pointer coord_ptr, TFvtxCompactCoordMap* ccoord_map);

  //! fill compact track in the container
  void fill_compact_track( void );

  //! fill compact coordinate in the container
  void fill_compact_coordinate( void );

	private:	

	//! Set IOC pointers
	void set_interface_ptrs(PHCompositeNode* top_node);
	
	//! Loop over tracks
	void track_loop();
	
	//! returns true if track passes basic checks
	bool accept_track( TFvtxTrkMap::const_pointer trk_ptr ) const;

	//! initialize kalmanfilter object starting parameters and internal list of nodes from track pointer
	void init( TFvtxTrkMap::const_pointer trk_ptr, TMutKalmanFilter& kalman_filter );
		 	
	//! fit all nodes (iterative prediction/filter/smoothing)
	bool fit_nodes( TMutKalmanFilter& kalman_filter );
 		 	
	//! fill_track. Get best parameters for each coord 
	bool fill_track( TFvtxTrkMap::pointer trk_ptr );
 		 	
	//! extrapolate track parameters from first gap to z located upstream of absorber (z_ref)
	bool extrapolate_to_vertex( TFvtxTrkMap::pointer trk_ptr );
 		 	
	//! extrapolate track parameters from last gap to z of first MuID gap
	bool extrapolate_to_mutr( TFvtxTrkMap::pointer trk_ptr );
	
	//! fill track with predicted filtered and smoothed residuals for all nodes
	void fill_tree( TFvtxTrkMap::pointer trk_ptr );

	//! returns r error (copied from mFvtxTrkFit but left private)
	static double get_r_error();
		
	//! sort pointers of TFvtxCoord from vertex to muid
	struct coord_less_ftor		
	{ 
		bool operator() ( const TFvtxCoordMap::value_type & coord_0, const TFvtxCoordMap::value_type & coord_1 ) 
		{ 
			return ( ( coord_0.get()->get_arm() == FVTXOO::North ) ?
				( coord_0.get()->get_mean_z() < coord_1.get()->get_mean_z() ):
				( coord_0.get()->get_mean_z() > coord_1.get()->get_mean_z() ) );
		} 
	};
		
	//! adds r residual calculated from coordinate to track
	void push_r_residual( TFvtxTrkMap::pointer, const TMutTrkPar&, TFvtxCoordMap::value_type); 
 
	/*! 
		adds w residual calculated from coordinate to track
		returns the corresponding chi_square contribution: (delta_w/w_fit_error)**2
	*/
	double push_w_residual( TFvtxTrkMap::pointer, const TMutTrkPar&, TFvtxCoordMap::value_type); 
	
	//! reset all nodes
	void reset_nodes( void )
	{
		for( node_iterator it = _nodes.begin(); it != _nodes.end(); it++ )
		it->reset_flags();
	}
	
        bool _use_svx_cluster;
	
	//! Timer
	PHTimeServer::timer _timer;		

	//! main analysis node
	PHCompositeNode *_top_node;	
	
	//! pointer to mFvtxKalFit parameters module
	mFvtxKalFitPar* _mod_par;		
	
	//! pointer to tFvtxTrk object map
	TFvtxTrkMap* _trk_map;				

	//! pointer to tFvtxCompactTrk object map
	TFvtxCompactTrkMap* _ctrk_map;				

	//! pointer to tFvtxCoord object map
	TFvtxCoordMap* _coord_map;				

	//! pointer to tFvtxCompactCoord object map
	TFvtxCompactCoordMap* _ccoord_map;				

	/*! 
		KalmanFilter node, deriving deriving from TMutKalmanFilter::Node to 
		add constructors from TMuiClusterO
	*/
	class KalmanFilterNode: public TMutKalmanFilter::Node
	{
		
		public:

		//! filled constructor using muid clusters
		KalmanFilterNode( const TFvtxCoordMap::pointer& coord_ptr, const int phiflag );
		
                //! filled constructor using VTX mc hits
                KalmanFilterNode( const SvxSnglPisaHit& svxhit, const int xyflag, const float zsmear,
                  const float phismear );

                //! filled constructor using VTX cluster (x-y-z space)
                KalmanFilterNode( const SvxCluster& clus, const int xyflag, const double phi_error,
                  const double r_error, double z_error, double dxdz, double dydz );

                //! filled constructor using VTX cluster (r-phi-z space)
                KalmanFilterNode( const SvxCluster& clus, const int phiflag, const double phi_error,
                  const double r_error, double z_error, double drdz );

		//! destructor
		virtual ~KalmanFilterNode( void )
		{}
    
		//! reference to associated TFvtxCoord, if any
		TFvtxCoordMap::value_type& get_fvtx_coord( void )
		{ return _fvtx_coord; }
   
		//! reference to associated TFvtxCoord, if any
		TFvtxCoordMap::value_type get_fvtx_coord( void ) const
		{ return _fvtx_coord; }

                //! reference to associated VTX mc hit, if any
                SvxSnglPisaHit get_svx_mchit( void )
                { return _svx_mchit; }
			
                //! reference to associated VTX cluster, if any
                SvxCluster get_svx_cluster( void )
                { return _svx_cluster; }

		private:
		
		//! the fvtx coordinate
		TFvtxCoordMap::value_type _fvtx_coord;

                //! the vtx MC hit
                SvxSnglPisaHit _svx_mchit;

                //! the vtx cluster
                SvxCluster _svx_cluster; 
		
	};

	//! shortcut to list of nodes
	typedef std::list< KalmanFilterNode > node_list;
	
	//! shortcut to list of nodes
	typedef node_list::iterator node_iterator;
	
	//! shortcut to list of nodes
	typedef node_list::const_iterator const_node_iterator;
	
	//! shortcut to list of nodes
	typedef node_list::reverse_iterator reverse_node_iterator;

	//! list of nodes belonging to a track
	node_list _nodes;	
	
	//! evaluation filename
	std::string _filename;	 
	
	//! output tree, if any
	TTree *_tree;

        //! output tree for VTX residuals:
        TTree *_tree2;
	
	//! track arm
	unsigned int _arm;

        //! node cage
        unsigned int _cage;
	
	//! node station
	unsigned int _station;
	
	//! node sector
	unsigned int _sector;
	
	//! node column
	unsigned int _column;

        //! px for the track
        double _px;

        //! py for the track
        double _py;

        //! pz for the track
        double _pz;

        //! node z
        double _znode;

        //! node meas
        double _meas;

        //! xmeas for node
        double _xmeas;

        //! ymeas for node 
        double _ymeas;

        //! phimeas for node 
        double _phimeas;

        //! xfit eas for node
        double _xfit;

        //! yfit for node 
        double _yfit;

        //! number of hits on track
        unsigned int _nhits;

        //! number of FVTX hits on track
        unsigned int _nfvtx;
	
        //! FVTX planes hit on track
        unsigned int _fplanes;

        //! inode for this measurement
        unsigned int _inode;
	
	//! node residual
	boost::array<double,3> _res;
	
	//! corresponding error
	boost::array<double,3> _res_cov;

	//! node contribution to track chisquare
	double _chi_square;

        //! vtx x hit location:
        double _x;

        //! vtx y hit location:
        double _y;

        //! vtx z hit location:
        double _z;

        //! vtx x error for  hit location:
        double _xerror;

        //! vtx y error for hit location:
        double _yerror;


  //! Random number generator
  PHGslRng _rng;

  //! Reference to PHTrackIntegratorKF
  PHTrackIntegratorKF *integrator;

};


#endif /* __MFVTXKALFIT_HH__ */














