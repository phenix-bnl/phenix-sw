// $Id: TMutAlignPar_v1.cxx,v 1.4 2011/12/24 04:48:18 slash Exp $
#include "TMutAlignPar_v1.hh"

ClassImp(TMutAlignPar_v1)

using namespace std;

//_________________________________________________________________________
TMutAlignPar_v1::TMutAlignPar_v1( void ):
    _arm(0),
    _octant(0),
    _index(0),
    _det_type(0),
    _trk_theta0(0),
    _trk_chi_square_ndf(0),
    _trk_chi_square_vtx_ndf(0),
    _trk_st2_hit(0),
    _n_coord(0)
{
    // initialize arrays
    std::fill( _det_index, _det_index+PAR_SIZE, 0 );
    std::fill( _w_det, _w_det+PAR_SIZE, 0 );
    std::fill( _sigma, _sigma+PAR_SIZE, 0 );
    std::fill( _dwdx, _dwdx+PAR_SIZE, 0 );
    std::fill( _dwdtx, _dwdtx+PAR_SIZE, 0 );
    std::fill( _dwdy, _dwdy+PAR_SIZE, 0 );
    std::fill( _dwdty, _dwdty+PAR_SIZE, 0 );
    std::fill( _wrt_z, _wrt_z+PAR_SIZE, 0 );
    std::fill( _wrt_phi, _wrt_phi+PAR_SIZE, 0 );

}

//_________________________________________________________________________
TMutAlignPar_v1::TMutAlignPar_v1(
    const Key& key,
    unsigned short arm,
    unsigned short index):
    TMutAlignPar( key ),
    _arm( arm ),
    _octant( 0 ),
    _index( index ),
    _det_type(0),
    _trk_theta0(0),
    _trk_chi_square_ndf(0),
    _trk_chi_square_vtx_ndf(0),
    _trk_st2_hit(0),
    _n_coord(0)
{
    // initialize arrays
    std::fill( _det_index, _det_index+PAR_SIZE, 0 );
    std::fill( _w_det, _w_det+PAR_SIZE, 0 );
    std::fill( _sigma, _sigma+PAR_SIZE, 0 );
    std::fill( _dwdx, _dwdx+PAR_SIZE, 0 );
    std::fill( _dwdtx, _dwdtx+PAR_SIZE, 0 );
    std::fill( _dwdy, _dwdy+PAR_SIZE, 0 );
    std::fill( _dwdty, _dwdty+PAR_SIZE, 0 );
    std::fill( _wrt_z, _wrt_z+PAR_SIZE, 0 );
    std::fill( _wrt_phi, _wrt_phi+PAR_SIZE, 0 );

}

//_________________________________________________________________________
TMutAlignPar_v1::TMutAlignPar_v1( const TMutAlignPar* base_ptr ):
    TMutAlignPar( *base_ptr ),
    _arm( base_ptr->get_arm() ),
    _octant( base_ptr->get_octant() ),
    _index( base_ptr->get_index() ),
    _det_type( base_ptr->get_detector_type() ),
    _trk_theta0( base_ptr->get_trk_theta0() ),
    _trk_chi_square_ndf( base_ptr->get_trk_chi_square_ndf() ),
    _trk_chi_square_vtx_ndf( base_ptr->get_trk_chi_square_vtx_ndf() ),
    _trk_st2_hit( base_ptr->get_trk_st2_hit() ),
    _n_coord( base_ptr->get_n_coord() )
{

    for( unsigned int index = 0; index < PAR_SIZE; index++ )
    {
        set_detector_index( index, base_ptr->get_detector_index( index ) );
        set_w_det( index, base_ptr->get_w_det( index ) );
        set_sigma( index, base_ptr->get_sigma( index ) );
        set_dwdx( index, base_ptr->get_dwdx( index ) );
        set_dwdtx( index, base_ptr->get_dwdtx( index ) );
        set_dwdy( index, base_ptr->get_dwdy( index ) );
        set_dwdty( index, base_ptr->get_dwdty( index ) );
        set_wrt_z( index, base_ptr->get_wrt_z( index ) );
        set_wrt_phi( index, base_ptr->get_wrt_phi( index ) );
    }
}


//_________________________________________________________________________
TMutAlignPar_v1::TMutAlignPar_v1( const TMutAlignPar& base_ref ):
    TMutAlignPar( base_ref ),
    _arm( base_ref.get_arm() ),
    _octant( base_ref.get_octant() ),
    _index( base_ref.get_index() ),
    _det_type( base_ref.get_detector_type() ),
    _trk_theta0( base_ref.get_trk_theta0() ),
    _trk_chi_square_ndf( base_ref.get_trk_chi_square_ndf() ),
    _trk_chi_square_vtx_ndf( base_ref.get_trk_chi_square_vtx_ndf() ),
    _trk_st2_hit( base_ref.get_trk_st2_hit() ),
    _n_coord( base_ref.get_n_coord() )
{

    for( unsigned int index = 0; index < PAR_SIZE; index++ )
    {
        set_detector_index( index, base_ref.get_detector_index( index ) );
        set_w_det( index, base_ref.get_w_det( index ) );
        set_sigma( index, base_ref.get_sigma( index ) );
        set_dwdx( index, base_ref.get_dwdx( index ) );
        set_dwdtx( index, base_ref.get_dwdtx( index ) );
        set_dwdy( index, base_ref.get_dwdy( index ) );
        set_dwdty( index, base_ref.get_dwdty( index ) );
        set_wrt_z( index, base_ref.get_wrt_z( index ) );
        set_wrt_phi( index, base_ref.get_wrt_phi( index ) );
    }
}


//_________________________________________________________________________
void TMutAlignPar_v1::print(ostream& os) const
{
    MUTOO::PRINT(os,GetName());

    os << "arm: " << get_arm() << " index: " << get_index() << " octant: " << get_octant();
    os << " type: " << (get_detector_type() == MuTr ? "MuTr":"MuId" );
    os << " n_coords: " << get_n_coord() << endl;
    for( int i=0; i < get_n_coord(); i++ )
    {
        os
            << "  index: " << i
            << " det: " << get_detector_index(i)
            << " w: " << get_w_det(i)
            << " sigma: " << get_sigma(i)
            << " derivatives:"
            << " (" << get_dwdx(i) << "." << get_dwdtx(i) << "," << get_dwdy(i) << "," << get_dwdty(i) << ")"
            << endl;
    }
    MUTOO::PRINT(os,"**");
}
