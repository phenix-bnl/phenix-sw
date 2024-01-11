// $Id: TFvtxAlignmentCorrection.h,v 1.2 2011/02/08 23:12:11 youzy Exp $
#ifndef __TFvtxAlignmentCorrection_h__
#define __TFvtxAlignmentCorrection_h__

/*!
\file    TFvtxAlignmentCorrection.h
\brief   Class to ordonate misalignment output of Millepede inorder to write the aliment input file correction
\author  Zhengyun You
\version $Revision: 1.2 $
\date    $Date: 2011/02/08 23:12:11 $
*/
#include<set>
#include <TTree.h>

//! Class to ordonate misalignment output of Millepede inorder to write the aliment input file correction
class TFvtxAlignmentCorrection
{
    public:

    //! constructor
    TFvtxAlignmentCorrection( void ):
        _z_alignment_enabled( true ),
        _fvtx_alignment_enabled( true )
        {}

    //! destructor
    virtual ~TFvtxAlignmentCorrection( void )
    {}

    void set_z_alignment_enabled( bool value )
    { _z_alignment_enabled = value; }

    void set_fvtx_alignment_enabled( bool value )
    { _fvtx_alignment_enabled = value; }

    //! read corrections from root file and write it in output
    void initialize(const char* input, const char* output = "fvtx_alignment_corrections_test.txt" );

    //! read correction from tree and write it in output
    void initialize(TTree* tree, const char* output = "fvtx_alignment_corrections_test.txt");

    //! make a backup copy of an existing file
    std::string make_backup( const std::string& filename );

    class TFvtxAlignmentCorrection_fvtx
    {
        public:

        //! default constructor
        TFvtxAlignmentCorrection_fvtx(
            int arm=0,
            int cage = 0,
            int station = 0,
            int sector = 0,
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
            _cage( cage ),
            _station( station ),
            _sector( sector ),
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
        virtual ~TFvtxAlignmentCorrection_fvtx()
        {}

        //! arm location
        int _arm;

        //! cage location
        int _cage;

        //! station location
        int _station;

        //! sector location
        int _sector;

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
        bool operator == (const TFvtxAlignmentCorrection_fvtx& par ) const
        {
            return(
                _arm == par._arm &&
                _cage == par._cage &&
                _station == par._station &&
                _sector == par._sector &&
                _half == par._half
                );
        }

        //! inferior to operator
        bool operator < (const TFvtxAlignmentCorrection_fvtx& par ) const
        {
            if( _arm != par._arm ) return _arm < par._arm;
            else if( _cage != par._cage )  return _cage < par._cage;
            else if( _station != par._station )  return _station < par._station;
            else if( _sector != par._sector )  return _sector < par._sector;
            else if( _half != par._half )  return _half < par._half;
            else return false;
        }

    };

    //! gets fvtx alignment corrections
    TFvtxAlignmentCorrection_fvtx get_fvtx_parameters( int arm, int cage, int station, int sector, int half );

    private:

    //! true if z alignment is enambled
    /*! if not enabled, the corresponding parameters are _not_ printed to the output file */
    bool _z_alignment_enabled;

    //! true if fvtx alignment is enabled
    bool _fvtx_alignment_enabled;

    //! set where to keep values
    typedef std::set< TFvtxAlignmentCorrection_fvtx > fvtx_set;

    //! parameter set (from file)
    fvtx_set _fvtx_parameters;

};

#endif
