// $Id: MuonGlobalAlign.h,v 1.16 2012/10/17 17:40:06 bbannier Exp $

#ifndef __MuonGlobalAlign_h__
#define __MuonGlobalAlign_h__

/*!
\file  MuonGlobalAlign.h
\brief   muon global alignment module
\author  Hugo Pereira/Catherine Silvestre
\version $Revision: 1.16 $
\date  $Date: 2012/10/17 17:40:06 $
*/

#include "MuonSubsysReco.h"

#include <MUTOO.h>
#include <MUIOO.h>
#include <TTree.h>

#include <string>
#include <cstdarg>
#include <cstdio>

#ifndef __CINT__
#include <boost/array.hpp>
#include <TMutTrkMap.h>
#include <TMutAlignParMap.h>
#include <PHTimeServer.h>
#else
class TMutTrkMap;
class TMutAlignParMap;
#endif

#include <TMutTrkPar.hh>
#include <MILLEPEDE.h>

// forward declaration for MUTOO classes.
class TMuiClusterMapO;
class TMutCoordMap;

#ifndef __CINT__

namespace Alignment
{

    //! mutr detector id
    class MutrDetId
    {
        public:

        //! constructor
        MutrDetId( int arm = 0, int station = 0, int gap = 0, int cathode = 0 ):
            _arm( arm ),
            _station( station ),
            _gap( gap ),
            _cathode( cathode )
        {}

        //! equal to operator
        bool operator == (const MutrDetId& id ) const
        {
            return
                _arm == id._arm &&
                _station == id._station &&
                _gap == id._gap &&
                _cathode == id._cathode;
        }

        //! less than operator
        bool operator < (const MutrDetId& id ) const
        {
            if( _arm != id._arm ) return _arm < id._arm;
            else if( _station != id._station ) return _station < id._station;
            else if( _gap != id._gap ) return _gap < id._gap;
            else if( _cathode != id._cathode ) return _cathode < id._cathode;
            else return false;
        }

        int _arm;
        int _station;
        int _gap;
        int _cathode;

        //! map mutr detector id to fixed alignment flags
        typedef std::map< MutrDetId, unsigned int > Map;

    };


    //! muid detector id
    class MuidDetId
    {
        public:

        //! constructor
        MuidDetId( int arm = 0, int plane = 0, int orientation = 0 ):
            _arm( arm ),
            _plane( plane ),
            _orientation( orientation )
        {}

        //! equal to operator
        bool operator == (const MuidDetId& id ) const
        {
            return
                _arm == id._arm &&
                _plane == id._plane &&
                _orientation == id._orientation;
        }


        //! less than operator
        bool operator < (const MuidDetId& id ) const
        {
            if( _arm != id._arm ) return _arm < id._arm;
            else if( _plane != id._plane ) return _plane < id._plane;
            else if( _orientation != id._orientation ) return _orientation < id._orientation;
            else return false;
        }

        int _arm;
        int _plane;
        int _orientation;

        //! map muid detector id to fixed parameter flags
        typedef std::map< MuidDetId, unsigned int > Map;

    };
}

#endif

/*!
\class   MuonGlobalAlign
\brief   muon global alignment tree generation.
*/
class MuonGlobalAlign: public MuonSubsysReco
{
    public:

    //! constructor
    MuonGlobalAlign( const char* name = "MUONGLOBALALIGN" );

    //! destructor
    virtual ~MuonGlobalAlign()
    {}

    //! run initialization
    int Init(PHCompositeNode *top_node);

    //! event method
    int process_event(PHCompositeNode *top_node);

    //! finish run
    int End(PHCompositeNode *top_node);

    //! output filename
    void set_output_filename( const char* file )
    { if( file ) _output_misalignment = file; }

    //! changes evaluation tree filename
    void set_evaluation_filename( const char* file )
    { if( file ) _evaluation_filename = file; }

    //! changes alignment tree filename
    void set_alignment_filename( const char* file )
    { if( file ) _alignment_filename = file; }

    //! changes dump filename (for millepede log)
    void set_dump_filename( const char* file )
    { if( file ) _dumpfile_name = file; }

    //!@name flags
    //@{
    enum Flag
    {
        NONE = 0,
        MAGNETS_ON = 1<<0,
        USE_CUTS = 1<<1,
        USE_THETA_CUT = 1<<2,
        USE_THETA_CUT_CLOSE_TRACK = 1<<3,
        ALIGN_MUID = 1<<4,
        ALIGN_MUTR = 1<<5,
        ALIGN_W = 1<<6,
        ALIGN_Z = 1<<7,
        ALIGN_PHI = 1<<8,
        USE_CONSTRAINTS = 1<<9,
        ITERATE = 1<<10,
        DO_ALIGNMENT = 1<<11,
        DO_EVALUATION = 1<<12,

        //! store alignment parameters but do not perform minimization
        WRITE_MEM = 1<<14,

        //! read alignment parameters from node and not from reconstructed maps
        READ_MEM = 1<<13
    };

    void set_flags( const unsigned long int& value )
    { _flags = value; }

    void set_flag( const Flag& flag, const bool& value )
    {
        if( value ) { _flags |= flag; }
        else { _flags &= (~flag); }
    }

    bool get_flag( const Flag& flag ) const
    { return _flags & flag; }

    //! magnets on/off flags
    void set_magnets_on( bool value )
    { set_flag( MAGNETS_ON, value ); }

    //! muid alignment flag
    void set_align_muid( bool value )
    { set_flag( ALIGN_MUID, value ); }

    //! mutr alignment flag
    void set_align_mutr( bool value )
    { set_flag( ALIGN_MUTR, value ); }

    //! w alignment flag
    void set_align_w( bool value )
    { set_flag( ALIGN_W, value ); }

    //! z alignment flag
    void set_align_z( bool value )
    { set_flag( ALIGN_Z, value ); }

    //! phi alignment flag
    void set_align_phi( bool value )
    { set_flag( ALIGN_PHI, value ); }

    //! constraint flag
    void set_constraint( bool value )
    { set_flag( USE_CONSTRAINTS, value ); }

    //! iterations
    void set_iterate( const bool& value )
    { set_flag( ITERATE, value ); }

    //! alignment flag
    void set_do_alignment( bool value )
    { set_flag( DO_ALIGNMENT, value ); }

    //! evaluation flag
    void set_do_evaluation( bool value )
    { set_flag( DO_EVALUATION, value ); }

    //! theta cut flag
    void set_theta_cut( bool value )
    { set_flag( USE_THETA_CUT, value ); }

    //! theta cut flag
    void set_theta_cut_keeping_closeTracks( bool value )
    { set_flag( USE_THETA_CUT_CLOSE_TRACK, value ); }

    //@}

    //! scratch filename needed for iteration
    void set_scratch_filename( const char* filename )
    { _scratch_filename = filename; }

    //! dump all alignment parameters to a given file
    bool print_to_file( const char* filename );

    //! return module name
    const char *Name() const
    {return ThisName.c_str(); }

    //! bit values for the alignment parameters
    enum ParameterBit
    {
        PAR_W = 1 << 0,
        PAR_Z = 1 << 1,
        PAR_PHI = 1 << 2,
        ALL = PAR_Z|PAR_W|PAR_PHI
    };

    //! fix mutr cathode
    void fix_mutr_cathode( int arm, int station, int gap, int cathode, unsigned int flags );

    //! fix mutr gap
    void fix_mutr_gap( int arm, int station, int gap, unsigned int flags )
    {
        fix_mutr_cathode( arm, station, gap, 0, flags );
        fix_mutr_cathode( arm, station, gap, 1, flags );
    }

    //! fix muid planes
    void fix_muid_plane( int arm, int plane, int orientation, unsigned int flags );

    //! fix muid plane
    void fix_muid_plane( int arm, int plane, unsigned int flags )
    {
        fix_muid_plane( arm, plane, 0, flags );
        fix_muid_plane( arm, plane, 1, flags );
    }

    //! calculate detector unique index for a given arm,station,gap and cathode tuple
    static int get_index_cathode( int arm, int station, int gap, int cathode ) ;

    //! calculate detector unique index for a given arm,station,octant, half octant, gap, cathode tuple
    static int get_index_half_octant( int arm, int station, int octant, int half_octant, int gap, int cathode );


    /*! \brief
    calculate detector unique index for a given arm,plane,panel,orientation tuple
    adds an offset so that it does not overlap with mutr unique ids
    */
    int get_index_panel( int arm, int plane, int panel, int orientation ) const;

    //! offset is added to avoid overlaping of MuID over MuTr
    int get_index_orientation( int arm, int plane, int orientation ) const;

    #ifndef __CINT__

    //! calculate detector unique index for a given cathode locator
    static int get_index_half_octant( const MUTOO::cathode_locator& location )
    { return get_index_half_octant( location.get<0>(), location.get<1>(), location.get<2>(),location.get<3>(), location.get<4>(), location.get<5>() ); }

    //! calculate detector unique index for a given cathode locator
    static int get_index_cathode( const MUTOO::cathode_locator& location )
    { return get_index_cathode( location.get<0>(), location.get<1>(), location.get<4>(), location.get<5>() ); }

    /*! \brief
    calculate detector unique index for a given muid orientation locator
    adds an offset so that it does not overlap with mutr unique ids
    */
    int get_index_panel( const MUIOO::panel_orient_locator& location ) const
    { return get_index_panel( location.get<0>(), location.get<1>(), location.get<2>(),location.get<3>() ); }

    //! offset is added to avoid overlaping of MuID over MuTr
    int get_index_orientation( const MUIOO::panel_orient_locator& location ) const
    { return get_index_orientation( location.get<0>(), location.get<1>(), location.get<3>() ); }


    #endif

    //! initialize minimization parameters
    bool init_parameters( int nStdDev, bool dumpMille );

    // initialize minimization
    bool init_minimize();

    //! dump the results of the minimisation to ostream out
    void print_mutr_parameters_to_stream( std::ostream &out = std::cout );

    //! dump the results of the minimisation to ostream out
    void print_muid_parameters_to_stream( std::ostream &out = std::cout );

    //! dump the fixed parameters to ostream out
    void print_fixed_parameters( std::ostream &out = std::cout ) const;

    //! check if several detectors have same index [debug]
    void check_detector_index( void ) const;

    //! make a backup copy of an existing file
    static std::string make_backup( const std::string& filename );

    protected:

    //! create all mutoo/muioo nodes
    int createNodeTree(PHCompositeNode *top_node);

    //! retrieve pointers to the needed maps
    void set_interface_pointers(PHCompositeNode *top_node);

    //! evaluation method
    void initialize_evaluation_tree( void );

    //! evaluation method
    void initialize_alignment_tree( void );

    //! evaluation method
    void fill_evaluation_tree( void );

    //!@name predefined set of fixed detectors
    //@{

    //! fix all mutr cathodes
    void fix_mutr_all ( void );

    //! fix default mutr gaps (station0 gap 1 and station1 gap0)
    void fix_mutr_2gaps ( void );

    //! fix mutr station0 and station1 entirely
    void fix_mutr_2stations( void );

    //! fix default muid planes (plane 0 and plane 1)
    void fix_muid_2planes( void );

    //@}

    //!@name low level parameter fixing methods
    //@{

    //! pass registered fixed detectors to millepede
    void register_fixed_detectors( void );

    //! fix parameters for MuTr for a given detector arm station octant cathode
    void fix_parameter_mutr(int arm, int station, int gap, int cathode , int parameter_bit );

    // fix parameters for MuID for a given detector arm plane orientation
    void fix_parameter_muid(int arm, int plane, int orientation, int parameter_bit );

    // constraint the 2 half octants of each octant to move at the same time
    bool constraint_halfs(int arm, int station, int gap, int cath, int octant);

    //@}

    #ifndef __CINT__
    //! evaluation method
    void fill_mutr_evaluation_tree( TMutTrkMap::pointer );

    //! evaluation method
    void fill_muid_evaluation_tree( TMutTrkMap::pointer );

    //! make the cuts to select the appropriated tracks when runing on reeldata
    bool accept_trk( TMutTrkMap::pointer) ;

    //! make the cuts to select the appropriated tracks when reading from mem tree
    bool accept_trk( TMutAlignParMap::pointer );

    #endif

    //! calculate detector unique index for station gap and cathode (used in ntuple)
    static int get_index_ntp( int station, int gap, int cathode )
    { return cathode + MUTOO::NumberOfCathodePlanes*( gap + MUTOO::NumberOfGaps*station ); }

    //! calculate detector unique index for plane, panel and orientation (used in ntuple)
    static int get_muid_index_ntp( int plane, int panel, int orientation )
    { return orientation + MUIOO::MAX_ORIENTATION*( panel + MUIOO::MAX_PANEL*plane ); }


    //! alignemnt minimization for tracks in given event
    void minimize();

    #ifndef __CINT__

    //! magnets on minimization. fit linear corrections to the bend track
    void minimize_magnets_on( TMutTrkMap::pointer ptr );

    //! magnets off minimization. fit straight track
    void minimize_magnets_off( TMutTrkMap::pointer ptr );

    //! magnets off minimization. fit straight track
    void minimize_magnets_off( TMutAlignParMap::pointer ptr );

    #endif

    //! end minimization
    void end_minimize();

    //! return formated string
    static std::string format( const char* format, ... );

    //! print to a file the angle (O, x, r) for each octant
    void get_octant_angle( std::ostream& out  ) const;
    bool print_angle_to_file( const char* filename_align );

    private :

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

    #ifndef __CINT__
    //!@name fixed detectors classes
    //@{

    //! map mutr detector id to fixed parameter flags
    Alignment::MutrDetId::Map _fixed_mutr_detectors;

    //! map muid detector id to fixed parameter flags
    Alignment::MuidDetId::Map _fixed_muid_detectors;

    //@}
    #endif

    //! local pointer to the track map
    TMutTrkMap* _trk_map;

    //! local pointer to the cluster map
    TMuiClusterMapO* _cluster_map;

    //! local pointer to the coord map
    TMutCoordMap* _coord_map;

    //! local pointer to the alignment parameter map
    TMutAlignParMap* _align_par_map;

    //! total number of recorded tracks
    int _n_tracks_total;

    //! total number of mutr detectors
    static const int _nb_mutr_det =
        MUTOO::NumberOfArms*
        MUTOO::NumberOfStations*
        MUTOO::NumberOfOctants*
        MUTOO::NumberOfHalfOctants*
        MUTOO::NumberOfGaps*
        MUTOO::NumberOfCathodePlanes;

    //! total number of muid detectors
    static const int _nb_muid_det =
        MUIOO::MAX_ARM*
        MUIOO::MAX_PLANE*
        MUIOO::MAX_PANEL*
        MUIOO::MAX_ORIENTATION;

    //! offset to add to muid unique ids to avoid overlap with mutr
    static const int _muid_offset =
        MUTOO::NumberOfArms*
        MUTOO::NumberOfStations*
        MUTOO::NumberOfOctants*
        MUTOO::NumberOfHalfOctants*
        MUTOO::NumberOfGaps*
        MUTOO::NumberOfCathodePlanes;

    //! total number of detectors
    int _nb_det;

    //! scratch filename needed for iterations
    const char* _scratch_filename;

    //! controls the track quality cut during matrix inversion iterations
    int _n_std_dev;

    //! true when InitParameters was called successfuly
    bool _par_init;

    //! configuration flags
    unsigned long int _flags;

    //! Set an output file for the alignment parameter output
    const char* _output_misalignment;

    //! Set an output file for the Dumpfile output
    const char* _dumpfile_name;

    //!@name output tree marameters
    //@{
    enum { BUFFER_SIZE=32000 };
    enum { AUTO_SAVE=16000 };
    //@}

    #ifndef __CINT__

    //! vector of global derivatives
    boost::array< float, MILLEPEDE::NGLB > _dergb;

    //! vector of local derivatives
    boost::array< float, MILLEPEDE::NPARTRK > _derlc;

    //! vector of parameters
    boost::array< float, MILLEPEDE::NGLB > _par;

    //! number of tracks used for each detector
    boost::array< int, MILLEPEDE::NPLAN > _n_tracks;

    //! number of hit in the last cathode
    int _hit;

    #endif

    //! output TFile name for evaluation tree
    std::string _evaluation_filename;

    //! alignment tree (stores alignment evaluation informations track by track)
    TTree* _evaluation_tree;

    #ifndef __CINT__

    //! number of mutr detectors
    static const int n_mutr_det =
        MUTOO::NumberOfCathodePlanes*
        MUTOO::NumberOfGaps*
        MUTOO::NumberOfStations;

    //! measured w position in mutr (perp to the strip)
    boost::array< double, n_mutr_det > _w_det;

    //! measured z position in mutr
    boost::array< double, n_mutr_det > _z_det;

    //! best matching fit parameters z
    boost::array< double, n_mutr_det > _z_fit;

    //! best matching fitted x position in mutr
    boost::array< double, n_mutr_det > _x_fit;

    //! best matching fitted y position in mutr
    boost::array< double, n_mutr_det > _y_fit;

    //! extrapolated fitted x position in mutr
    boost::array< double, n_mutr_det > _x_not_extrapo;

    //! extrapolated fitted y position in mutr
    boost::array< double, n_mutr_det > _y_not_extrapo;

    //! extrapolated fitted w position in mutr
    boost::array< double, n_mutr_det > _w_fit;

    //! extrapolated fitted v position in mutr (along the strips)
    boost::array< double, n_mutr_det > _v_fit;

    //! residual in mutr
    boost::array< double, n_mutr_det > _residu;

    //! set to 1 when matching fit parameters are found
    boost::array< int, n_mutr_det > _flag;

    //! track half octant for given detector
    boost::array< int, n_mutr_det > _half_octant;

    //! angular lenght from the beam axis
    boost::array< float, n_mutr_det > _theta;

    //! number of muid detectors
    static const int n_muid_det =
        MUIOO::MAX_PLANE*
        MUIOO::MAX_PANEL*
        MUIOO::MAX_ORIENTATION;

    //! set to 1 when matching fit parameters are found for muid detectors
    boost::array< int, n_muid_det > _muid_flag;

    //! measured w position in muid plane/panel (perp to tubes)
    boost::array< double, n_muid_det > _muid_w_det;

    //! z position in muid plane/panel
    boost::array< double, n_muid_det > _muid_z_det;

    //! fitted x position in muid plane/panel
    boost::array< double, n_muid_det > _muid_x_fit;

    //! fitted x position in muid plane/panel
    boost::array< double, n_muid_det > _muid_y_fit;

    //! fitted v position in muid plane/panel (along tubes)
    boost::array< double, n_muid_det > _muid_v_fit;

    //! fitted w position in muid plane/panel (perp tubes)
    boost::array< double, n_muid_det > _muid_w_fit;

    //! residual in muid plane/panel
    boost::array< double, n_muid_det > _muid_z_fit;

    //! residual in muid plane/panel
    boost::array< double, n_muid_det > _muid_residu;

    //! residual in muid plane/panel
    boost::array< int, n_muid_det > _plane_eval;

    //! residual in muid plane/panel
    boost::array< int, n_muid_det > _panel_eval;

    //! residual in muid plane/panel
    boost::array< int, n_muid_det > _orientation_eval;

    //! module timer
    PHTimeServer::timer _timer;

    #endif

    //! track arm
    int _arm;

    //! track octant
    int _octant;

    //! track number of coordinates
    int _mutr_coords;

    //! track number of muid clusters
    int _muid_clusters;

    //! track chisquare
    double _chi_square;

    //! track number of degrees of freedom
    int _ndf;

    //! track chisquare probability
    double _prob;

    //! single track vertex chisquare
    double _chi_square_vtx;

    //! single track number of degrees of freedom
    int _ndf_vtx;

    //! single track vertex fit chisquare probability
    double _prob_vtx;


    //! to know if the track is accepted or not
    int _accept_trk;

    //! output TFile name for alignment tree
    std::string _alignment_filename;

    //! initial offsets vs millepede tree
    TTree* _alignment_tree;

    //! millepede w correction
    double _delta_w_millepede;

    //! error on millepede w correction
    double _error_w;

    // desalignment tree parameters
    //! millepede z correction
    double _delta_z_millepede;

    //! millepede x correction
    double _delta_x_millepede;

    //! millepede y correction
    double _delta_y_millepede;

    //! millepede phi correction
    double _delta_phi_millepede;

    //! input x misalignment
    double _delta_x;

    //! input y misalignment
    double _delta_y;

    //! error on millepede x correction
    double _error_x;

    //! error on millepede y correction
    double _error_y;

    //! input z misalignment
    double _delta_z;

    //! input w misalignment
    double _delta_w;

    //! input phi misalignment
    double _delta_phi;

    //! station location
    int _station;

    //! gap location
    int _gap ;

    //! half octant location
    int _half ;

    //! cathode location
    int _cathode;

    //! set to 1 if the parameter corresponds to a muid detector
    int _is_muid;

    //! plane location
    int _plane;

    //! panel location
    int _panel;

    //! orientation location
    int _orientation;

    //! detector index
    int _detector_index;

    //! error on millepede z correction
    double _error_z;

    //! error on millepede phi correction
    double _error_phi;

    //! number of tracks in the detector
    int _nb_tracks;

    //! strip angle
    double _angle;

    //!@name variables for accept_trk cut
    /*! these need to be stored temporarily to be filled into TMutAlignPar */
    //@{
    double _mem_trk_theta0;

    double _mem_trk_chi_square_ndf;

    double _mem_trk_chi_square_vtx_ndf;

    double _mem_trk_st2_hit;
    //@}

};

#endif

