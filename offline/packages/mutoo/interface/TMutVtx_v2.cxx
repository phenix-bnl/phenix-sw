#include<TMutVtx_v2.hh>
#include<TMutTrkMap.h>
ClassImp(TMutVtx_v2)

using namespace std;

//_________________________________________________________________________
TMutVtx_v2::TMutVtx_v2() :
  _arm(0),
  _index(0),
	_sign(0)
{}


//_________________________________________________________________________
TMutVtx_v2::TMutVtx_v2(const Key& key,
		       UShort_t arm,
		       UShort_t index) :
  TMutVtx(key),
  _arm(arm),
  _index(index),
	_sign(0)
{}

//_________________________________________________________________________
TMutVtx_v2::TMutVtx_v2(const TMutVtx* base_ptr) :
  TMutVtx(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index()),
	_sign(base_ptr->get_sign())
{
  set_x( base_ptr->get_x() );
  set_y( base_ptr->get_y() );
  set_z( base_ptr->get_z() );
  
  set_x_bp( base_ptr->get_x_bp() );
  set_y_bp( base_ptr->get_y_bp() );
  set_z_bp( base_ptr->get_z_bp() );
  set_dca_bp( base_ptr->get_dca_bp() );
  
  // update track informations
  set_px1( base_ptr->get_px1() );
  set_py1( base_ptr->get_py1() );
  set_pz1( base_ptr->get_pz1() );
  set_charge1( base_ptr->get_charge1() );
  
  set_px2( base_ptr->get_px2() );
  set_py2( base_ptr->get_py2() );
  set_pz2( base_ptr->get_pz2() );
  set_charge2( base_ptr->get_charge2() );
  
  // copy chisquare 
  set_chi_square( base_ptr->get_chi_square() );
  set_ndf( base_ptr->get_ndf() );
  
  // copy covariance matrix
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
  set_covar( i, j, base_ptr->get_covar( i, j ) );
}

//_________________________________________________________________________
TMutVtx_v2::TMutVtx_v2(const TMutVtx& base_ref) :
  TMutVtx(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index()),
	_sign(base_ref.get_sign())
{

  set_x( base_ref.get_x() );
  set_y( base_ref.get_y() );
  set_z( base_ref.get_z() );

  set_x_bp( base_ref.get_x_bp() );
  set_y_bp( base_ref.get_y_bp() );
  set_z_bp( base_ref.get_z_bp() );
  set_dca_bp( base_ref.get_dca_bp() );
  
  // update track informations
  set_px1( base_ref.get_px1() );
  set_py1( base_ref.get_py1() );
  set_pz1( base_ref.get_pz1() );
  set_charge1( base_ref.get_charge1() );
  
  set_px2( base_ref.get_px2() );
  set_py2( base_ref.get_py2() );
  set_pz2( base_ref.get_pz2() );
  set_charge2( base_ref.get_charge2() );
  
  // copy chisquare 
  set_chi_square( base_ref.get_chi_square() );
  set_ndf( base_ref.get_ndf() );
  
  // copy covariance matrix
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
  set_covar( i, j, base_ref.get_covar( i, j ) );
  
}

//_________________________________________________________________________
void TMutVtx_v2::print(ostream& os) const
{
  MUTOO::PRINT(os,GetName());  
  
  os 
    << " key: " << get_key().get_obj_key() 
    << " arm: " << _arm << endl;

  _vtx_par.print(os);
  
  TMutTrkMap::const_key_iterator trk_iter = get_associated<TMutTrk>();
  os << " number of associated tracks: " << trk_iter.count() << endl;
  os << " associated tracks: ";
  
  // dump associated tracks ids
  while( TMutTrkMap::const_pointer trk_ptr = trk_iter.next() )
  os << trk_ptr->get()->get_key().get_obj_key() << " ";
  os << endl;
  
  // dump invariant mass
  os << " invariant mass: " << get_mass() << endl;
  MUTOO::PRINT(os,"**");
}

