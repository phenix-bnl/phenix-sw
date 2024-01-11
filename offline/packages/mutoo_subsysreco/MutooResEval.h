// $Id: MutooResEval.h,v 1.6 2010/09/16 17:08:29 hpereira Exp $
#ifndef MutooResEval_h
#define MutooResEval_h

//////////////////////////////////////////////////////////////
/*!
\file MutooResEval.h
\brief resolution/residuals evaluation ntuple
\author  Hugo Pereira
\version $Revision: 1.6 $
\date $Date: 2010/09/16 17:08:29 $
*/
//////////////////////////////////////////////////////////////

#include <string>
#include <SubsysReco.h>
#include <TMutTrkPar.hh>

#ifndef __CINT__
#include <mMutBPFit.h>
#include <mMutKalFit.h>
#include <mMutStraightFit.h>
#include <boost/array.hpp>
#include <PHTimeServer.h>
#endif

class mMutKalFitPar;
class mMutStraightFitPar;

class PHCompositeNode;
class TMutTrkMap;
class TTree;

//////////////////////////////////////////////////////////////
/*!
\class MutooResEval
\brief resolution/residuals evaluation ntuple
*/
//////////////////////////////////////////////////////////////

class MutooResEval: public SubsysReco
{
    public:

    //! constructor
    MutooResEval(
        const char* name = "MUTOORESEVAL",
        const char* file_name = "resolution.root"
        );


    //! destructor
    virtual ~MutooResEval()
    {}

    //! run initialisation
    int InitRun( PHCompositeNode* );

    //! unique initialisation
    int Init( PHCompositeNode* );

    //! event processing
    int process_event( PHCompositeNode* );

    //! full end
    int End( PHCompositeNode* );

    //! supermodule name
    const char* Name() const{return ThisName.c_str(); }

    //! defines mutoo evaluation filename
    void set_file_name( const char* file )
    { if( file ) _file_name = std::string( file ); }

    //! pattern recognition flags
    enum Flag {

        NONE = 0,

        //! do refit for each disabled cathodes
        REFIT = (1<<0),

        //! magnets are on
        MAGNETS_ON = (1<<1),

        //! fill muid residuals
        USE_MUID = (1<<2)

    };

    //! flags
    void set_flags( const unsigned long int& value )
    { _flags = value; }

    //! flags
    void set_flag( const Flag& flag, const bool& value )
    {
        if( value ) { _flags |= flag; }
        else { _flags &= (~flag); }
    }

    //! flags
    bool get_flag( const Flag& flag ) const
    { return _flags & flag; }

    //! refit
    void set_refit( bool value )
    { set_flag( REFIT, value ); }

    //! magnets on
    void set_magnets_on( bool value )
    { set_flag( MAGNETS_ON, value ); }

    protected:

    //! create nodes needed for the analysis
    void create_node_tree( PHCompositeNode* );

    //! retrieves all used maps from top_node_vtx_map
    void set_interface_ptrs( PHCompositeNode* );

    //! fill tree for a given cathode
    void fill_tree( void );

    #ifndef __CINT__

    //! check track before filling tree
    bool accept_trk( TMutTrkMap::const_pointer ) const;

    /*!
    returns the distances between the track and the road, calculated at Gap0.
    Return -1 if no road found
    */
    double get_dg0( const TMutTrkMap::value_type& , const TMuiRoadMapO::value_type& ) const;

    //! returns true if coord pointer comes from a multi track cluster
    bool is_multi_cluster( TMutCoordMap::const_pointer ) const;

    #endif

    private:

    //! used to get closest Track parameter to a given z
    class closest_z_ftor
    {
        public:

        //! constructor
        closest_z_ftor( const double &z ):
            _z( z )
            {}

        //! predicate
        bool operator() (const TMutTrkPar& first, const TMutTrkPar& second )
        { return fabs( first.get_z() - _z ) < fabs( second.get_z() - _z ); }

        private:

        //! z coordinate to match
        double _z;

    };

    //! mutoo working node
    PHCompositeNode* _mutoo_node;

    #ifndef __CINT__

    //! bend plane fit module
    mMutBPFit _mMutBPFitMod;


    //! kalman filter module
    mMutKalFit _mMutKalFitMod;

    //! straight track fit
    mMutStraightFit _mMutStraightFitMod;

    //! Timer
    PHTimeServer::timer _timer;

    #endif

    //! kalman filter module
    mMutKalFitPar* _mMutKalFit_par;
    mMutStraightFitPar* _mMutStraightFit_par;

    //! track map
    TMutTrkMap* _trk_map;

    //! mutoo evaluation file
    std::string _file_name;

    //!@name resolution tree
    //@{
    TTree* _resolution;

    //! locator
    int _arm;
    int _station;
    int _octant;
    int _half;
    int _gap;
    int _cathode;

    int _plane;
    int _panel;
    int _orientation;
    bool _is_muid;

    #ifndef __CINT__

    //! track hits per station
    boost::array<int,3> _hits_station;

    #endif

    //! track chisquare
    float _trk_chisquare;

    //! track number of degrees of freedom
    int _trk_ndf;

    //! associated road depth
    int _trk_depth;

    //! DG0 between road and track
    float _trk_dg0;

    //! reconstructed x position
    float _reco_x;

    //! reconstructed y position
    float _reco_y;

    //! reconstruction z position
    float _reco_z;

    //! reconstructed momentum along x
    float _reco_px;

    //! reconstructed momentum along y
    float _reco_py;

    //! reconstructed momentum along z
    float _reco_pz;

    //! reconstructed position along the strips
    float _reco_v;

    //! reconstructed position perp to strips
    float _reco_w;

    //! coordinate position (perp to the strips)
    float _coord_w;

    //! error on coordinate
    float _coord_err;

    //! coord position along the beam
    float _coord_z;

    //! coord status bit
    int _coord_status;

    //! clus status bit
    int _clus_status;

    //! cluster size
    int _clus_size;

    //! true if coord is from multi hit cluster
    bool _is_multi;

    //! true if coord is from stereo plane
    bool _is_stereo;

    //@}

    //! patttern recognition configuration flags
    /*! it is a bitwise or of the Flags enumaration */
    unsigned long int _flags;

};

#endif
