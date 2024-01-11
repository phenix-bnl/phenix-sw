#ifndef TFvtxAlignPar_v1_hh
#define TFvtxAlignPar_v1_hh

// $Id: TFvtxAlignPar_v1.hh,v 1.6 2012/10/10 00:21:27 jinhuang Exp $

/*!
   \file TFvtxAlignPar_v1.hh
   \brief Class for Fvtx global alignment parameters
   \author Zhengyun You
   \version $Revision: 1.6 $
   \date $Date: 2012/10/10 00:21:27 $
*/

/*! @ingroup Int_terface */
//! PHENIX Muon tracker global alignment parameters object */

#include "TFvtxAlignPar.hh"

#include <PHException.h>

class TFvtxAlignPar_v1: public TFvtxAlignPar
{

    public:

    //! empty constructor
    TFvtxAlignPar_v1( void );

    //! filled constructor
    TFvtxAlignPar_v1( const Key&,
      unsigned short arm,  
      unsigned short index );

    //! constructor from base pointer
    TFvtxAlignPar_v1( const TFvtxAlignPar* );

    //! constructor from base reference
    TFvtxAlignPar_v1( const TFvtxAlignPar& );

    //! destructor
    virtual ~TFvtxAlignPar_v1()
    {}

    //! @name Functional Interface
    //@{

    //! arm
    void set_arm( unsigned short value )
    { _arm = value; }

    //! arm
    unsigned short get_arm( void ) const
    { return _arm; }

    //! cage for given coord (array_index, cage)
    virtual void set_cage( unsigned short index, unsigned short value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _cage[index] = value; }

    //! cage for given coord (array_index, cage)
    virtual unsigned short get_cage( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _cage[index]; }

    //! station for given coord (array_index, station)
    virtual void set_station( unsigned short index, unsigned short value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _station[index] = value; }

    //! station for given coord (array_index, station)
    virtual unsigned short get_station( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _station[index]; }

    //! sector for given coord (array_index, sector)
    virtual void set_sector( unsigned short index, unsigned short value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _sector[index] = value; }

    //! sector for given coord (array_index, sector)
    virtual unsigned short get_sector( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _sector[index]; }

    //! half for given coord (array_index, half)
    virtual void set_half( unsigned short index, unsigned short value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _half[index] = value; }

    //! half for given coord (array_index, half)
    virtual unsigned short get_half( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _half[index]; }

    //! index
    virtual void set_index(unsigned short index)
    { _index = index; }

    //! index
    virtual unsigned short get_index() const
    {return _index;}

    /*! detector type */
    virtual void set_detector_type( DetType value )
    { _det_type = value; }

    /*! detector type */
    virtual DetType get_detector_type( void ) const
    { return (DetType) _det_type; }

    //! track angle
    virtual void set_trk_theta0( Float_t value )
    { _trk_theta0 = value; }

    //! track angle
    virtual Float_t get_trk_theta0( void ) const
    { return _trk_theta0; }

    //! track chisquare
    virtual void set_trk_chi_square_ndf( Float_t value )
    { _trk_chi_square_ndf = value; }

    //! track chisquare
    virtual Float_t get_trk_chi_square_ndf( void ) const
    { return _trk_chi_square_ndf; }

    //! track vertex chisquare
    virtual void set_trk_chi_square_vtx_ndf( Float_t value )
    { _trk_chi_square_vtx_ndf = value; }

    //! track vertex chisquare
    virtual Float_t get_trk_chi_square_vtx_ndf( void ) const
    { return _trk_chi_square_vtx_ndf; }

    //! hit flag on stations
    virtual void set_trk_st_hit_flag( Float_t value )
    { _trk_st_hit_flag = value; }

    //! hit flag on stations
    virtual Float_t get_trk_st_hit_flag( void ) const
    { return _trk_st_hit_flag; }

    //! average strip angle
    virtual void set_trk_phi_avg( Float_t value )
    { _trk_phi_avg = value; }

    //! average strip angle
    virtual Float_t get_trk_phi_avg( void ) const
    { return _trk_phi_avg; }

    //! number of coordinate on track
    virtual void set_n_coord( unsigned short value )
    { _n_coord = value; }

    //! number of coordinate on track
    virtual unsigned short get_n_coord( void ) const
    { return _n_coord; }

    //! detector index for given coord (array_index, det_index)
    virtual void set_detector_index( unsigned short index, unsigned short value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _det_index[index] = value; }

    //! det index for given coord (array_index, det_index)
    virtual unsigned short get_detector_index( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _det_index[index]; }

    //! vertex x
    virtual void set_vtx_x( Float_t value )
    {_vtx_x = value;}

    //! vertex x
    virtual Float_t get_vtx_x( void ) const
    { return _vtx_x; }

    //! vertex y
    virtual void set_vtx_y( Float_t value )
    {_vtx_y = value;}

    //! vertex y
    virtual Float_t get_vtx_y( void ) const
    { return _vtx_y; }

    //! vertex z
    virtual void set_vtx_z( Float_t value )
    {_vtx_z = value;}

    //! vertex z
    virtual Float_t get_vtx_z( void ) const
    { return _vtx_z; }

    //! residual for given index
    virtual void set_w_det( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _w_det[index] = value; }

    //! residual for given index
    virtual Float_t get_w_det( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _w_det[index]; }

    //! error
    virtual void set_sigma( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _sigma[index] = value; }

    //! error
    virtual Float_t get_sigma( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _sigma[index]; }

    //! dw/dx
    virtual void set_dwdx( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _dwdx[index] = value; }

    //! dw/dx
    virtual Float_t get_dwdx( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _dwdx[index]; }

    //! dw/dtx
    virtual void set_dwdtx( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _dwdtx[index] = value; }

    //! dw/dtx
    virtual Float_t get_dwdtx( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _dwdtx[index]; }

    //! dw/dy
    virtual void set_dwdy( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _dwdy[index] = value; }

    //! dw/dy
    virtual Float_t get_dwdy( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _dwdy[index]; }

    //! dw/dty
    virtual void set_dwdty( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _dwdty[index] = value; }

    //! dw/dty
    virtual Float_t get_dwdty( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _dwdty[index]; }

    //! dw/dz
    virtual void set_wrt_z( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _wrt_z[index] = value; }

    //! dw/dz
    virtual Float_t get_wrt_z( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _wrt_z[index]; }

    //! dw/dphi
    virtual void set_wrt_phi( unsigned short index, Float_t value )
    { BOUNDS_CHECK( index, PAR_SIZE ); _wrt_phi[index] = value; }

    //! dw/dphi
    virtual Float_t get_wrt_phi( unsigned short index ) const
    { BOUNDS_CHECK( index, PAR_SIZE ); return _wrt_phi[index]; }

    //@}

    //! @name Dumpers
    //@{
    virtual void print(std::ostream& os = std::cout) const;

    //@}

    private:

    enum { PAR_SIZE = 16 };

    //! arm
    unsigned short _arm;

    //! index
    unsigned short _index;

    //! detector type
    unsigned short _det_type;

    Float_t _trk_theta0;

    Float_t _trk_chi_square_ndf;

    Float_t _trk_chi_square_vtx_ndf;

    Float_t _trk_st_hit_flag;

    Float_t _trk_phi_avg;

    Float_t _vtx_x;

    Float_t _vtx_y;

    Float_t _vtx_z;

    //! number of coordinate on track
    unsigned short _n_coord;

    //! cage
    unsigned short _cage[PAR_SIZE];

    //! station
    unsigned short _station[PAR_SIZE];

    //! sector
    unsigned short _sector[PAR_SIZE];

    //! half
    unsigned short _half[PAR_SIZE];

    //! detector index
    unsigned short _det_index[PAR_SIZE];

    //! w_det
    Float_t _w_det[PAR_SIZE];

    //! sigma
    Float_t _sigma[PAR_SIZE];

    //! dwdx
    Float_t _dwdx[PAR_SIZE];

    //! dwdtx
    Float_t _dwdtx[PAR_SIZE];

    //! dwdy
    Float_t _dwdy[PAR_SIZE];

    //! dwdty
    Float_t _dwdty[PAR_SIZE];

    //! wrt_z
    Float_t _wrt_z[PAR_SIZE];

    //! wrt_phi
    Float_t _wrt_phi[PAR_SIZE];

    ClassDef(TFvtxAlignPar_v1,1)

};

#endif
