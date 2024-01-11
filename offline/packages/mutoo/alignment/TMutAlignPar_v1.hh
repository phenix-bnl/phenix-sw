#ifndef TMutAlignPar_v1_hh
#define TMutAlignPar_v1_hh

// $Id: TMutAlignPar_v1.hh,v 1.5 2011/12/24 04:48:18 slash Exp $

/*!
   \file TMutAlignPar_v1.hh
   \brief Class for Muon global alignment parameters
   \author H. Pereira
   \version $Revision: 1.5 $
   \date $Date: 2011/12/24 04:48:18 $
*/

/*! @ingroup Int_terface */
//! PHENIX Muon tracker global alignment parameters object */

#include "TMutAlignPar.hh"

#include <PHException.h>

class TMutAlignPar_v1: public TMutAlignPar
{

    public:

    //! empty constructor
    TMutAlignPar_v1( void );

    //! filled constructor
    TMutAlignPar_v1( const Key&,
        unsigned short arm,
        unsigned short index );

    //! constructor from base pointer
    TMutAlignPar_v1( const TMutAlignPar* );

    //! constructor from base reference
    TMutAlignPar_v1( const TMutAlignPar& );

    //! destructor
    virtual ~TMutAlignPar_v1()
    {}

    //! @name Functional Interface
    //@{

    //! arm
    void set_arm( unsigned short value )
    { _arm = value; }

    //! arm
    unsigned short get_arm( void ) const
    { return _arm; }

    //! octant (for MuTR only)
    virtual void set_octant( unsigned short value )
    { _octant = value; }

    //! octant
    virtual unsigned short get_octant( void ) const
    { return _octant; }

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

    //! number of hits in station 2
    virtual void set_trk_st2_hit( Float_t value )
    { _trk_st2_hit = value; }

    //! number of hits in station 2
    virtual Float_t get_trk_st2_hit( void ) const
    { return _trk_st2_hit; }

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

    //! octant (for MuTR tracks only)
    unsigned short _octant;

    //! index
    unsigned short _index;

    //! detector type
    unsigned short _det_type;

    Float_t _trk_theta0;

    Float_t _trk_chi_square_ndf;

    Float_t _trk_chi_square_vtx_ndf;

    Float_t _trk_st2_hit;

    //! number of coordinate on track
    unsigned short _n_coord;

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

    ClassDef(TMutAlignPar_v1,1)

};

#endif
