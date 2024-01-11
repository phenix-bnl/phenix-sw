// $Id: TMuiRoadO_v1.cxx,v 1.1 2006/04/22 01:58:29 hpereira Exp $
//INCLUDECHECKER: Removed this line: #include <cmath>
#include "TMuiRoadO_v1.h"
#include <TMuiClusterMapO.h>
#include <TMutTrackUtil.h>

ClassImp(TMuiRoadO_v1);
  
TMuiRoadO_v1::TMuiRoadO_v1()
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
  _group = 0;
  _golden = 0;
}

TMuiRoadO_v1::TMuiRoadO_v1(const Key& key,
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
  _group = 0;
  _golden = 0;
}


TMuiRoadO_v1::TMuiRoadO_v1(const TMuiRoadO* base_ptr) :
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
  _group = base_ptr->get_group();
  _golden = base_ptr->get_golden();
}

TMuiRoadO_v1::TMuiRoadO_v1(const TMuiRoadO& base_ref) :
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
  _group = base_ref.get_group();
  _golden = base_ref.get_golden();
}

PHPoint
TMuiRoadO_v1::get_gap0_point() const {

  float mini_z = 10000000.0;
  float mini_z_abs = 10000000.0;

  TMuiClusterMapO::const_key_iterator clus_iter = get_associated<TMuiClusterO>();
  while(TMuiClusterMapO::const_pointer clust_ptr = clus_iter.next()){
    if(fabs((clust_ptr->get()->get_centroidpos()).getZ())<mini_z_abs) {
      mini_z     = (clust_ptr->get()->get_centroidpos()).getZ();
      mini_z_abs = fabs(mini_z);
    }
  }
  return TMutTrackUtil::linear_track_model(get_const_fitpar(),
					   mini_z);
}
  
//____________________________________________________________
void TMuiRoadO_v1::print( std::ostream &os ) const
{
  MUIOO::PRINT(os,GetName());
  os << " arm: " << _arm << std::endl;
  os << " index: " << _index << std::endl;
  os << " depth: " << _depth << std::endl;
  os << " quality: " << _road_quality << std::endl;
  os << " freedom: " << _freedom << std::endl;
  os << " ghostflag: " << _ghost_flag << std::endl;
  os << " group: " << _group << std::endl;
  os << " golden: " << _golden << std::endl;
  os << " nhit: " << _nhit << std::endl;
  os << " chi_square: " << _fit_par.get_chi_square() << " " << std::endl;
  os << " ref position: " << _fit_par.get_x() << " " <<
    _fit_par.get_y() << " " <<
    _fit_par.get_z() << " " << std::endl;

  os << " ref direction: " 
     << _fit_par.get_dxdz() << " " 
     << _fit_par.get_dydz() << " "
     << std::endl;

  os << " gap0 point: " 
     << get_gap0_point().getX() << " "
     << get_gap0_point().getY() << " "
     << get_gap0_point().getZ() << std::endl;
  
  os << " number of associated clusters: " 
    << get_associated<TMuiClusterO>().count() 
    << std::endl;
 
  MUIOO::PRINT(os,"**");
}








