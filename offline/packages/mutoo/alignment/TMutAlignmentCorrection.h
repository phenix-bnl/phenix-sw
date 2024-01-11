// $Id: TMutAlignmentCorrection.h,v 1.3 2010/08/18 14:28:28 hpereira Exp $
#ifndef __TMutAlignmentCorrection_h__
#define __TMutAlignmentCorrection_h__

/*!
\file    TMutAlignmentCorrection.h
\brief   Class to ordonate misalignment output of Millepede inorder to write the aliment input file correction
\author  Catherine Silvestre
\version $Revision: 1.3 $
\date    $Date: 2010/08/18 14:28:28 $
*/
#include<set>
#include <TTree.h>

//! Class to ordonate misalignment output of Millepede inorder to write the aliment input file correction
class TMutAlignmentCorrection
{
    public:

    //! constructor
    TMutAlignmentCorrection( void ):
        _z_alignment_enabled( true ),
        _mutr_alignment_enabled( true ),
        _muid_alignment_enabled( true )
        {}

    //! destructor
    virtual ~TMutAlignmentCorrection( void )
    {}

    void set_z_alignment_enabled( bool value )
    { _z_alignment_enabled = value; }

    void set_mutr_alignment_enabled( bool value )
    { _mutr_alignment_enabled = value; }

    void set_muid_alignment_enabled( bool value )
    { _muid_alignment_enabled = value; }

    //! read corrections from root file and write it in output
    void initialize(const char* input, const char* output = "alignment_corrections_test.txt" );

    //! read correction from tree and write it in output
    void initialize(TTree* tree, const char* output = "alignment_corrections_test.txt");

    //! make a backup copy of an existing file
    std::string make_backup( const std::string& filename );

    class TMutAlignmentCorrection_mutr
    {
        public:

        //! default constructor
        TMutAlignmentCorrection_mutr(
            int arm=0,
            int station = 0,
            int octant = 0,
            int gap = 0,
            int cathode = 0,
            int half = 0,
            double delta_x = 0,
            double delta_y = 0,
            double delta_z = 0,
            double delta_phi = 0,
            double delta_w = 0,
            double error_w = 0,
            double error_z = 0,
            double error_phi = 0,
            int nb_tracks =0
            ):
            _arm (arm),
            _station( station ),
            _octant( octant ),
            _gap( gap ),
            _cathode( cathode ),
            _half( half ),
            _delta_x( delta_x ),
            _delta_y( delta_y ),
            _delta_z( delta_z ),
            _delta_phi( delta_phi ),
            _delta_w( delta_w ),
            _error_w( error_w ),
            _error_z( error_z ),
            _error_phi( error_phi ),
            _nb_tracks( nb_tracks )
        {};

        //! destructor
        virtual ~TMutAlignmentCorrection_mutr()
        {}

        //! arm location
        int _arm;

        //! station location
        int _station;

        //! octant location
        int _octant;

        //! gap location
        int _gap;

        //! cathode location
        int _cathode;

        //! half location
        int _half;

        //! detector misalignment along x
        double _delta_x;

        //! detector Misalignment along y
        double _delta_y;

        //! detector Misalignment along z
        double _delta_z;

        //! detector angular Misalignment (in xOy)
        double _delta_phi;

        //! detector Misalignment along w
        double _delta_w;

        //! detector error Misalignment along w
        double _error_w;

        //! detector error Misalignment along z
        double _error_z;

        //! detector error Misalignment along phi
        double _error_phi;

        //! number of tracks in the detector
        double _nb_tracks;

        //! equal to operator (compare parameters location)
        bool operator == (const TMutAlignmentCorrection_mutr& par ) const
        {
            return(
                _arm == par._arm &&
                _station == par._station &&
                _octant == par._octant &&
                _gap == par._gap &&
                _cathode == par._cathode &&
                _half == par._half
                );
        }

        //! inferior to operator
        bool operator < (const TMutAlignmentCorrection_mutr& par ) const
        {
            if( _arm != par._arm ) return _arm < par._arm;
            else if( _station != par._station )  return _station < par._station;
            else if( _gap != par._gap )  return _gap < par._gap;
            else if( _cathode != par._cathode )  return _cathode < par._cathode;
            else if( _half != par._half )  return _half < par._half;
            else if( _octant != par._octant )  return _octant < par._octant;
            else return false;
        }

    };


    class TMutAlignmentCorrection_muid
    {
        public:

        //! default constructor
        TMutAlignmentCorrection_muid(
            int arm=0,
            int plane = 0,
            int panel = 0,
            int orientation = 0,
            double delta_x = 0,
            double delta_y = 0,
            double delta_z = 0,
            double delta_phi = 0,
            double delta_w = 0,
            double error_w = 0,
            double error_z = 0,
            double error_phi = 0,
            int nb_tracks =0
            ):
            _arm (arm),
            _plane( plane ),
            _panel( panel ),
            _orientation( orientation ),
            _delta_x( delta_x ),
            _delta_y( delta_y ),
            _delta_z( delta_z ),
            _delta_phi( delta_phi ),
            _delta_w( delta_w ),
            _error_w( error_w ),
            _error_z( error_z ),
            _error_phi( error_phi ),
            _nb_tracks( nb_tracks )
        {};

        //! destructor
        virtual ~TMutAlignmentCorrection_muid()
        {}

        //! arm location
        int _arm;

        //! plane location
        int _plane;

        //! panel location
        int _panel;

        //! orientation location
        int _orientation;

        //! detector Misalignment along x
        double _delta_x;

        //! detector Misalignment along y
        double _delta_y;

        //! detector Misalignment along z
        double _delta_z;

        //! detector angular Misalignment (in xOy)
        double _delta_phi;

        //! detector Misalignment along w
        double _delta_w;

        //! detector error Misalignment along w
        double _error_w;

        //! detector error Misalignment along z
        double _error_z;

        //! detector error Misalignment along phi
        double _error_phi;

        //! number of tracks in the detector
        double _nb_tracks;

        //! equal to operator (compare parameters location)
        bool operator == (const TMutAlignmentCorrection_muid& par ) const
        {
            return(
                _arm == par._arm &&
                _plane == par._plane &&
                _panel == par._panel &&
                _orientation == par._orientation
                );
        }

        //! inferior to operator
        bool operator < (const TMutAlignmentCorrection_muid& par ) const
        {
            if( _arm != par._arm ) return _arm < par._arm;
            else if( _plane != par._plane )  return _plane < par._plane;
            else if( _panel != par._panel )  return _panel < par._panel;
            else if( _orientation != par._orientation )  return _orientation < par._orientation;
            else return false;
        }

    };

    //! gets mutr alignment corrections
    TMutAlignmentCorrection_mutr get_mutr_parameters( int arm, int station, int octant, int gap, int  cathode,int half );

    //! gets muid alignment
    TMutAlignmentCorrection_muid get_muid_parameters( int arm, int plane, int panel, int orientation );

    private:

    //! true if z alignment is enambled
    /*! if not enabled, the corresponding parameters are _not_ printed to the output file */
    bool _z_alignment_enabled;

    //! true if mutr alignment is enabled
    bool _mutr_alignment_enabled;

    //! true if muid alignment is enabled
    bool _muid_alignment_enabled;

    //! set where to keep values
    typedef std::set< TMutAlignmentCorrection_mutr > mutr_set;

    //! parameter set (from file)
    mutr_set _mutr_parameters;

    //! set where to keep values
    typedef std::set< TMutAlignmentCorrection_muid > muid_set;

    //! parameter set (from file)
    muid_set _muid_parameters;
};

#endif
