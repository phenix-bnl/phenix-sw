#ifndef TFvtxAlignPar_hh
#define TFvtxAlignPar_hh

// $Id: TFvtxAlignPar.hh,v 1.6 2012/10/10 00:21:27 jinhuang Exp $

/*!
   \file TFvtxAlignPar.hh
   \brief Class for Fvtx global alignment parameters
   \author Zhengyun You
   \version $Revision: 1.6 $
   \date $Date: 2012/10/10 00:21:27 $
*/

/*! @ingroup Int_terface */
//! PHENIX Fvtx global alignment parameters object */

#include<PHKey.hh>
#include<MUTOO.h>

class TFvtxAlignPar: public PHKey
{

    public:

    enum DetType{ Fvtx, Svx };

    //! constructor
    TFvtxAlignPar( void )
    {}

    //! constructor from key
    TFvtxAlignPar( const Key& key):
        PHKey( key )
    {}

    //! destructor
    virtual ~TFvtxAlignPar( void )
    {}

    //! @name Functional Interface
    //@{

    //! arm
    virtual void set_arm( unsigned short )
    {}

    //! arm
    virtual unsigned short get_arm( void ) const
    { return 0; }

    //! cage
    virtual void set_cage( unsigned short, unsigned short )
    {}

    //! cage
    virtual unsigned short get_cage( unsigned short ) const
    { return 0; }

    //! station
    virtual void set_station( unsigned short, unsigned short )
    {}

    //! station
    virtual unsigned short get_station( unsigned short ) const
    { return 0; }

    //! sector
    virtual void set_sector( unsigned short, unsigned short )
    {}

    //! sector
    virtual unsigned short get_sector( unsigned short ) const
    { return 0; }

    //! half
    virtual void set_half( unsigned short, unsigned short )
    {}

    //! half
    virtual unsigned short get_half( unsigned short ) const
    { return 0; }

    /*! index */
    virtual void set_index(unsigned short)
    {}

    /*! index [0,1024] */
    virtual unsigned short  get_index() const
    {return 0;}

    /*! detector type */
    virtual void set_detector_type( DetType )
    {}

    /*! detector type */
    virtual DetType get_detector_type( void ) const
    { return (DetType) 0; }

    //! track angle
    virtual void set_trk_theta0( Float_t )
    {}

    //! track angle
    virtual Float_t get_trk_theta0( void ) const
    { return 0; }

    //! track chisquare
    virtual void set_trk_chi_square_ndf( Float_t )
    {}

    //! track chisquare
    virtual Float_t get_trk_chi_square_ndf( void ) const
    { return 0; }

    //! track vertex chisquare
    virtual void set_trk_chi_square_vtx_ndf( Float_t )
    {}

    //! track vertex chisquare
    virtual Float_t get_trk_chi_square_vtx_ndf( void ) const
    { return 0; }

    //! hit flag on stations
    virtual void set_trk_st_hit_flag( Float_t )
    {}

    //! hit flag on stations
    virtual Float_t get_trk_st_hit_flag( void ) const
    { return 0; }

    //! average strip angle
    virtual void set_trk_phi_avg( Float_t value )
    {}

    //! average strip angle
    virtual Float_t get_trk_phi_avg( void ) const
    { return 0; }

    //! vertex x
    virtual void set_vtx_x( Float_t value )
    {}

    //! vertex x
    virtual Float_t get_vtx_x( void ) const
    { return 0; }

    //! vertex y
    virtual void set_vtx_y( Float_t value )
    {}

    //! vertex y
    virtual Float_t get_vtx_y( void ) const
    { return 0; }

    //! vertex z
    virtual void set_vtx_z( Float_t value )
    {}

    //! vertex z
    virtual Float_t get_vtx_z( void ) const
    { return 0; }

    //! number of coordinate on track
    virtual void set_n_coord( unsigned short )
    {}

    //! number of coordinate on track
    virtual unsigned short get_n_coord( void ) const
    { return 0; }

    //! fvtx index for given coord (array_index, det_index)
    virtual void set_detector_index( unsigned short, unsigned short )
    {}

    //! fvtx index for given coord (array_index, det_index)
    virtual unsigned short get_detector_index( unsigned short ) const
    { return 0; }

    //! residual for given index
    virtual void set_w_det( unsigned short, Float_t )
    {}

    //! residual for given index
    virtual Float_t get_w_det( unsigned short ) const
    { return 0; }

    //! error
    virtual void set_sigma( unsigned short, Float_t )
    {}

    //! error
    virtual Float_t get_sigma( unsigned short ) const
    { return 0; }

    //! dw/dx
    virtual void set_dwdx( unsigned short, Float_t )
    {}

    //! dw/dx
    virtual Float_t get_dwdx( unsigned short ) const
    { return 0; }

    //! dw/dtx
    virtual void set_dwdtx( unsigned short, Float_t )
    {}

    //! dw/dtx
    virtual Float_t get_dwdtx( unsigned short ) const
    { return 0; }

    //! dw/dy
    virtual void set_dwdy( unsigned short, Float_t )
    {}

    //! dw/dy
    virtual Float_t get_dwdy( unsigned short ) const
    { return 0; }

    //! dw/dty
    virtual void set_dwdty( unsigned short, Float_t )
    {}

    //! dw/dty
    virtual Float_t get_dwdty( unsigned short ) const
    { return 0; }

    //! dw/dz
    virtual void set_wrt_z( unsigned short, Float_t )
    {}

    //! dw/dz
    virtual Float_t get_wrt_z( unsigned short ) const
    { return 0; }

    //! dw/dphi
    virtual void set_wrt_phi( unsigned short, Float_t )
    {}

    //! dw/dphi
    virtual Float_t get_wrt_phi( unsigned short ) const
    { return 0; }

    //@}

    //! @name Dumpers
    //@{
    virtual void print(std::ostream& os = std::cout) const
    {}

    //@}

    private:

    #ifndef __CINT__

    //! static class ID
    static PHClassId::id_type _class_id;

    #endif

	ClassDef(TFvtxAlignPar,1)

};

#endif
