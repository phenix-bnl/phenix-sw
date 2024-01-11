// $Id: TMutVtx_v1.cxx,v 1.12 2011/12/29 20:19:31 slash Exp $
#include "TMutVtx_v1.hh"
#include "TMutTrkMap.h"
ClassImp(TMutVtx_v1)

using namespace std;

//_________________________________________________________________________
TMutVtx_v1::TMutVtx_v1() :
    _arm(0),
    _index(0),
    _sign(0)
{}


//_________________________________________________________________________
TMutVtx_v1::TMutVtx_v1(const Key& key,
		       UShort_t arm,
		       UShort_t index) :
  TMutVtx(key),
  _arm(arm),
  _index(index),
	_sign(0)
{}

//_________________________________________________________________________
TMutVtx_v1::TMutVtx_v1(const TMutVtx* base_ptr) :
  TMutVtx(*base_ptr),
  _arm(base_ptr->get_arm()),
  _index(base_ptr->get_index()),
	_sign(base_ptr->get_sign())
{
  // update track informations
  set_z( base_ptr->get_z() );
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

  // copy covariance matrix
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
  set_covar( i, j, base_ptr->get_covar( i, j ) );
}

//_________________________________________________________________________
TMutVtx_v1::TMutVtx_v1(const TMutVtx& base_ref) :
  TMutVtx(base_ref),
  _arm(base_ref.get_arm()),
  _index(base_ref.get_index()),
	_sign(base_ref.get_sign())
{

  // update track informations
  set_z( base_ref.get_z() );
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

  // copy covariance matrix
  for( unsigned int i=0; i<COVAR_ROW; i++ )
  for( unsigned int j=0; j<COVAR_ROW; j++ )
  set_covar( i, j, base_ref.get_covar( i, j ) );

}

//_________________________________________________________________________
void TMutVtx_v1::print(ostream& os) const
{
  MUTOO::PRINT(os,GetName());
  _vtx_par.print(os);
  os << " number of associated tracks: " << get_associated<TMutTrk>().count() << endl;
  os << " invariant mass: " << get_mass() << endl;
  MUTOO::PRINT(os,"**");
}
