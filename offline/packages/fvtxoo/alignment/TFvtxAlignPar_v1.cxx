// $Id: TFvtxAlignPar_v1.cxx,v 1.7 2014/03/03 03:16:07 jinhuang Exp $
#include "TFvtxAlignPar_v1.hh"

ClassImp(TFvtxAlignPar_v1)

using namespace std;

//_________________________________________________________________________
TFvtxAlignPar_v1::TFvtxAlignPar_v1( void ):
    _arm(0),
    _index(0),_det_type(0),
    _trk_theta0(0),
    _trk_chi_square_ndf(0),
    _trk_chi_square_vtx_ndf(0),
    _trk_st_hit_flag(0),
    _trk_phi_avg(0),
    _n_coord(0)
{
    // initialize arrays
    std::fill( _cage, _cage+PAR_SIZE, 0 );
    std::fill( _station, _station+PAR_SIZE, 0 );
    std::fill( _sector, _sector+PAR_SIZE, 0 );
    std::fill( _half, _half+PAR_SIZE, 0 );
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
TFvtxAlignPar_v1::TFvtxAlignPar_v1(
    const Key& key,
    unsigned short arm,
    unsigned short index):
    TFvtxAlignPar( key ),
    _arm( arm ),
    _index( index ),_det_type(0),
    _trk_theta0(0),
    _trk_chi_square_ndf(0),
    _trk_chi_square_vtx_ndf(0),
    _trk_st_hit_flag(0),
    _trk_phi_avg(0),
    _n_coord(0)
{
    // initialize arrays
    std::fill( _cage, _cage+PAR_SIZE, 0 );
    std::fill( _station, _station+PAR_SIZE, 0 );
    std::fill( _sector, _sector+PAR_SIZE, 0 );
    std::fill( _half, _half+PAR_SIZE, 0 );
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
TFvtxAlignPar_v1::TFvtxAlignPar_v1( const TFvtxAlignPar* base_ptr ):
    TFvtxAlignPar( *base_ptr ),
    _arm( base_ptr->get_arm() ),
    _index( base_ptr->get_index() ),_det_type(base_ptr->get_detector_type()),
    _trk_theta0( base_ptr->get_trk_theta0() ),
    _trk_chi_square_ndf( base_ptr->get_trk_chi_square_ndf() ),
    _trk_chi_square_vtx_ndf( base_ptr->get_trk_chi_square_vtx_ndf() ),
    _trk_st_hit_flag( base_ptr->get_trk_st_hit_flag() ),
    _trk_phi_avg(base_ptr->get_trk_phi_avg()),
    _n_coord( base_ptr->get_n_coord() )
{

    for( unsigned int index = 0; index < PAR_SIZE; index++ )
    {
        set_cage( index, base_ptr->get_cage( index ) );
        set_station( index, base_ptr->get_station( index ) );
        set_sector( index, base_ptr->get_sector( index ) );
        set_half( index, base_ptr->get_half( index ) );
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
TFvtxAlignPar_v1::TFvtxAlignPar_v1( const TFvtxAlignPar& base_ref ):
    TFvtxAlignPar( base_ref ),
    _arm( base_ref.get_arm() ),
    _index( base_ref.get_index() ),_det_type(base_ref.get_detector_type()),
    _trk_theta0( base_ref.get_trk_theta0() ),
    _trk_chi_square_ndf( base_ref.get_trk_chi_square_ndf() ),
    _trk_chi_square_vtx_ndf( base_ref.get_trk_chi_square_vtx_ndf() ),
    _trk_st_hit_flag( base_ref.get_trk_st_hit_flag() ),
    _trk_phi_avg(base_ref.get_trk_phi_avg()),
    _n_coord( base_ref.get_n_coord() )
{

    for( unsigned int index = 0; index < PAR_SIZE; index++ )
    {
        set_cage( index, base_ref.get_cage( index ) );
        set_station( index, base_ref.get_station( index ) );
        set_sector( index, base_ref.get_sector( index ) );
        set_half( index, base_ref.get_half( index ) );
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
void TFvtxAlignPar_v1::print(ostream& os) const
{
    MUTOO::PRINT(os,GetName());

    os << "arm : " << get_arm() << " index: " << get_index();
    os << " n_coords: " << get_n_coord() << endl;
    for( int i=0; i < get_n_coord(); i++ )
    {
        os
            << " index: " << i
            << " det: " << get_detector_index(i)
            << " w: " << get_w_det(i)
            << " sigma: " << get_sigma(i)
            << " derivatives:"
            << " (" << get_dwdx(i) << "." << get_dwdtx(i) << "," << get_dwdy(i) << "," << get_dwdty(i) << ")"
            << endl;
    }
    MUTOO::PRINT(os,"**");
}
