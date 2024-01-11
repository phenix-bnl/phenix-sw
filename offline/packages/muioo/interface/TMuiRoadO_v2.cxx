// $Id: TMuiRoadO_v2.cxx,v 1.1 2006/04/22 01:58:29 hpereira Exp $
#include "TMuiRoadO_v2.h"
#include<TMuiClusterMapO.h>
#include<TMutTrackUtil.h>

//INCLUDECHECKER: Removed this line: #include <cmath>

using namespace std;

ClassImp(TMuiRoadO_v2)

//__________________________________________________  
TMuiRoadO_v2::TMuiRoadO_v2()
{
  _arm = 0;
  _index = 0;
  _depth = 0;
  _nhit = 0;
  _max_hit_plane = 0;
  _road_quality = 0.0;
  _freedom = 0;
  _ghost_flag = 0;
  _gapbit = 0;
  _pass2_gapbit = 0;
  _pass2_depth = 0;
  _group = 0;
  _golden = 0;
}

//__________________________________________________  
TMuiRoadO_v2::TMuiRoadO_v2(const Key& key,
			       UShort_t arm,
			       UShort_t index) :
  TMuiRoadO(key)
{
  _arm = arm;
  _index = index;
  _depth = 0;
  _nhit = 0;
  _max_hit_plane = 0;
  _road_quality = 0.0;
  _freedom = 0;
  _ghost_flag = 0;
  _gapbit = 0;
  _pass2_gapbit = 0;
  _pass2_depth = 0;
  _group = 0;
  _golden = 0;
}


//__________________________________________________  
TMuiRoadO_v2::TMuiRoadO_v2(const TMuiRoadO* base_ptr) :
  TMuiRoadO(*base_ptr)
{
  _arm = base_ptr->get_arm();
  _index = base_ptr->get_index();
  _fit_par = base_ptr->get_fit_par();
  _depth = base_ptr->get_depth();
  _nhit = base_ptr->get_nhit();
  _max_hit_plane = base_ptr->get_max_hit_plane();
  _road_quality = base_ptr->get_road_quality();
  _freedom = base_ptr->get_freedom();
  _ghost_flag = base_ptr->get_ghost_flag();
  _gapbit = base_ptr->get_gapbit();
  _pass2_gapbit = base_ptr->get_pass2_gapbit();
  _pass2_depth = base_ptr->get_pass2_depth();
  _group = base_ptr->get_group();
  _golden = base_ptr->get_golden();
}

//__________________________________________________  
TMuiRoadO_v2::TMuiRoadO_v2(const TMuiRoadO& base_ref) :
  TMuiRoadO(base_ref)
{
  _arm = base_ref.get_arm();
  _index = base_ref.get_index();
  _fit_par = base_ref.get_fit_par();
  _depth = base_ref.get_depth();
  _nhit = base_ref.get_nhit();
  _max_hit_plane = base_ref.get_max_hit_plane();
  _road_quality = base_ref.get_road_quality();
  _freedom = base_ref.get_freedom();
  _ghost_flag = base_ref.get_ghost_flag();
  _gapbit = base_ref.get_gapbit();
  _pass2_gapbit = base_ref.get_pass2_gapbit();
  _pass2_depth = base_ref.get_pass2_depth();
  _group = base_ref.get_group();
  _golden = base_ref.get_golden();
}

//__________________________________________________  
PHPoint
TMuiRoadO_v2::get_gap0_point() const {

  float mini_z = 10000000.0;
  float mini_z_abs = 10000000.0;

  TMuiClusterMapO::const_key_iterator clus_iter = get_associated<TMuiClusterO>();
  while(TMuiClusterMapO::const_pointer clust_ptr = clus_iter.next()){
    if( fabs( (clust_ptr->get()->get_centroidpos()).getZ() ) < mini_z_abs) 
    {
      mini_z     = (clust_ptr->get()->get_centroidpos()).getZ();
      mini_z_abs = fabs( mini_z );
    }
  }
  return TMutTrackUtil::linear_track_model(get_const_fitpar(),
					   mini_z);
}
  
//____________________________________________________________
void TMuiRoadO_v2::print( ostream &os ) const
{
  MUIOO::PRINT(os,GetName());
  os << " key: " << get_key().get_obj_key() << endl;
  os << " arm: " << _arm << endl;
  os << " index: " << _index << endl;
  os << " depth: " << _depth << endl;
  os << " quality: " << _road_quality << endl;
  os << " freedom: " << _freedom << endl;
  os << " ghostflag: " << _ghost_flag << endl;
  os << " group: " << _group << endl;
  os << " golden: " << _golden << endl;
  os << " nhit: " << _nhit << endl;
  os << " chi_square: " << _fit_par.get_chi_square() << " " << endl;
  os << " ref position: " << _fit_par.get_x() << " " <<
    _fit_par.get_y() << " " <<
    _fit_par.get_z() << " " << endl;

  os 
			<< " ref direction: " 
			<< _fit_par.get_dxdz() << " " 
			<< _fit_par.get_dydz() << " "
			<< endl;

  os 
			<< " gap0 point: " 
			<< get_gap0_point().getX() << " "
			<< get_gap0_point().getY() << " "
			<< get_gap0_point().getZ() << endl;
  
	os 
			<< " phi: " 
			<< atan2( _fit_par.get_y(), _fit_par.get_x() ) << " , "
	 		<< atan2( _fit_par.get_dydz(), _fit_par.get_dxdz() )
			<< endl;
	os  << " r: "
			<< sqrt( MUIOO::SQUARE( _fit_par.get_x() ) + MUIOO::SQUARE( _fit_par.get_y() ) ) << " , "
			<< sqrt( MUIOO::SQUARE( _fit_par.get_dxdz() ) + MUIOO::SQUARE( _fit_par.get_dydz() ) ) << " , "
			<< endl;
	
  os << " number of associated clusters: " 
    << get_associated<TMuiClusterO>().count() 
    << endl;
 
  MUIOO::PRINT(os,"**");
}
