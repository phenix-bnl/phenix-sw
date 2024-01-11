// $Id: TMui1DRoadO_v1.cxx,v 1.1 2006/04/22 01:58:27 hpereira Exp $

/*!
	\file TMui1DRoadO_v1.cxx
	\brief Interface Object Class : TMui1DRoadO
	\author Jason Newby
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:58:27 $
*/

#include<TMui1DRoadO_v1.h>
#include<TDataType.h>
#include<algorithm>
#include<TMuiKeyGen.h>

ClassImp(TMui1DRoadO_v1)
  
TMui1DRoadO_v1::TMui1DRoadO_v1() :
  _arm(0),
  _panel(0),
  _orientation(0),
  _index(0),
  _fit_par(),
  _depth(0),
  _nhit(0),
  _max_hit_plane(0),
  _road_quality(0),
  _freedom(0),
  _ghost_flag(0),
  _gapbit(0)
{;}

TMui1DRoadO_v1::TMui1DRoadO_v1(const Key& key,
			       UShort_t arm,
                               UShort_t panel,
                               UShort_t orientation,
			       UShort_t index) :
  TMui1DRoadO(key),
  _arm(arm),
  _panel(panel),
  _orientation(orientation),
  _index(index),
  _fit_par(),
  _depth(0),
  _nhit(0),
  _max_hit_plane(0),
  _road_quality(0),
  _freedom(0),
  _ghost_flag(0),
  _gapbit(0)
{
    std::fill(_fitweight,
              _fitweight+FITWEIGHT_SIZE,
              0.0);
}


TMui1DRoadO_v1::TMui1DRoadO_v1(const TMui1DRoadO* base_ptr) :
  TMui1DRoadO(*base_ptr),
  _arm(base_ptr->get_arm()),
  _panel(base_ptr->get_panel()),
  _orientation(base_ptr->get_orientation()),
  _index(base_ptr->get_index()),
  _fit_par(base_ptr->get_fit_par()),
  _depth(base_ptr->get_depth()),
  _nhit(base_ptr->get_nhit()),
  _max_hit_plane(base_ptr->get_max_hit_plane()),
  _road_quality(base_ptr->get_road_quality()),
  _freedom(base_ptr->get_freedom()),
  _ghost_flag(base_ptr->get_ghost_flag()),
  _gapbit(base_ptr->get_gapbit())
{
    for(int rowid = 0; rowid < FITWEIGHT_SIZE; rowid++)
    {
      _fitweight[rowid]=base_ptr->get_fitweight(rowid);
    }
}


TMui1DRoadO_v1::TMui1DRoadO_v1(const TMui1DRoadO& base_ref) :
  TMui1DRoadO(base_ref),
  _arm(base_ref.get_arm()),
  _panel(base_ref.get_panel()),
  _orientation(base_ref.get_orientation()),
  _index(base_ref.get_index()),
  _fit_par(base_ref.get_fit_par()),
  _depth(base_ref.get_depth()),
  _nhit(base_ref.get_nhit()),
  _max_hit_plane(base_ref.get_max_hit_plane()),
  _road_quality(base_ref.get_road_quality()),
  _freedom(base_ref.get_freedom()),
  _ghost_flag(base_ref.get_ghost_flag()),
  _gapbit(base_ref.get_gapbit())
{
    for(int rowid = 0; rowid < FITWEIGHT_SIZE; rowid++)
    {
      _fitweight[rowid]=base_ref.get_fitweight(rowid);
    }
}

TMui1DRoadO_v1::TMui1DRoadO_v1(const Key& baseKey, const TMui1DRoadO* base_ptr) :
TMui1DRoadO(baseKey),
_arm(base_ptr->get_arm()),
_panel(base_ptr->get_panel()),
_orientation(base_ptr->get_orientation()),
_index(TMuiKeyGen::get_index(baseKey.get_obj_key())),
_fit_par(base_ptr->get_fit_par()),
_depth(base_ptr->get_depth()),
_nhit(base_ptr->get_nhit()),
_max_hit_plane(base_ptr->get_max_hit_plane()),
_road_quality(base_ptr->get_road_quality()),
_freedom(base_ptr->get_freedom()),
_ghost_flag(base_ptr->get_ghost_flag()),
_gapbit(base_ptr->get_gapbit())
{

  // Since we initialized with an "empty" key,
  // our associations have not been copy constructed
  // we'll do this in the IOC that calls this method
  
  for(int rowid = 0; rowid < FITWEIGHT_SIZE; rowid++)
  {
    _fitweight[rowid]=base_ptr->get_fitweight(rowid);
  }
}

int TMui1DRoadO_v1::get_numfired() const
{
  int num_fired = 0;
  for(int i = 0; i <= _depth; i++)
  {
    num_fired+=0x1&(_gapbit>>i);
  }
  return num_fired;
}

int TMui1DRoadO_v1::get_numskipped() const
{
  return (_depth + 1 - get_numfired() );
}

