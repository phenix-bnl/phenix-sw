#ifndef TMutAlignPar_hh
#define TMutAlignPar_hh

// $Id: TMutAlignPar.hh,v 1.4 2011/12/24 04:48:17 slash Exp $

/*!
   \file TMutAlignPar.hh
   \brief Class for Muon global alignment parameters
   \author H. Pereira
   \version $Revision: 1.4 $
   \date $Date: 2011/12/24 04:48:17 $
*/

/*! @ingroup Int_terface */
//! PHENIX Muon tracker global alignment parameters object */

#include<PHKey.hh>
#include<MUTOO.h>

class TMutAlignPar: public PHKey
{

    public:

    /*! Enumeration for the charge combination of muon pairs. */
    enum DetType{ MuTr, MuId };

    //! constructor
    TMutAlignPar( void )
    {}

    //! constructor from key
    TMutAlignPar( const Key& key):
        PHKey( key )
    {}

    //! destructor
    virtual ~TMutAlignPar( void )
    {}

    //! @name Functional Interface
    //@{

    //! arm
    virtual void set_arm( unsigned short )
    {}

    //! arm
    virtual unsigned short get_arm( void ) const
    { return 0; }

    //! octant (for MuTR only)
    virtual void set_octant( unsigned short )
    {}

    //! octant
    virtual unsigned short get_octant( void ) const
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

    //! number of hits in station 2
    virtual void set_trk_st2_hit( Float_t )
    {}

    //! number of hits in station 2
    virtual Float_t get_trk_st2_hit( void ) const
    { return 0; }

    //! number of coordinate on track
    virtual void set_n_coord( unsigned short )
    {}

    //! number of coordinate on track
    virtual unsigned short get_n_coord( void ) const
    { return 0; }

    //! mutr index for given coord (array_index, det_index)
    virtual void set_detector_index( unsigned short, unsigned short )
    {}

    //! mutr index for given coord (array_index, det_index)
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

	ClassDef(TMutAlignPar,1)

};

#endif
