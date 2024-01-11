// $Id: TMuiHitO_v1.cxx,v 1.1 2006/04/22 01:58:28 hpereira Exp $

/*!
	\file TMuiHitO-v1.cxx
	\brief Interface Object Class : TMuiHitO
	\author Jason Newby
  \version $Revision: 1.1 $
  \date    $Date: 2006/04/22 01:58:28 $
*/

#include<TMuiHitO_v1.h>
#include<TDataType.h>
#include<TMuiKeyGen.h>

ClassImp(TMuiHitO_v1)

using namespace std;
  
//_____________________________________
TMuiHitO_v1::TMuiHitO_v1()
{  
  _arm = 0;
  _plane = 0;
  _panel = 0;
  _orientation = 0;
  _twopack = 0;
  _index = 0;
}

//_____________________________________
TMuiHitO_v1::TMuiHitO_v1(const Key& key,
                           UShort_t arm,
                           UShort_t plane,
                           UShort_t panel,
                           UShort_t orientation,
                           UShort_t twopack,
                           UShort_t index) :
  TMuiHitO(key)
{
  _arm = arm;
  _plane = plane;
  _panel = panel;
  _orientation = orientation;
  _twopack = twopack;
  _index = index;
}


//_____________________________________
TMuiHitO_v1::TMuiHitO_v1(const TMuiHitO* base_ptr) :
  TMuiHitO(*base_ptr)
{
  _arm = base_ptr->get_arm();
  _plane = base_ptr->get_plane();
  _panel = base_ptr->get_panel();
  _orientation = base_ptr->get_orientation();
  _twopack = base_ptr->get_twopack();
  _index = base_ptr->get_index();
}

//_____________________________________
TMuiHitO_v1::TMuiHitO_v1(const TMuiHitO& base_ref) :
  TMuiHitO(base_ref)
{
  _arm = base_ref.get_arm();
  _plane = base_ref.get_plane();
  _panel = base_ref.get_panel();
  _orientation = base_ref.get_orientation();
  _twopack = base_ref.get_twopack();
  _index = base_ref.get_index();
}

//_____________________________________
void TMuiHitO_v1::print( ostream& os ) const
{
  MUIOO::PRINT(os,GetName());
  os 
    << " index: " << _index << endl;
  os  << " arm: " << _arm
    << " plane: " << _plane
    << " panel: " << _panel
    << " orientation: " << _orientation
    << " twopack: " << _twopack << endl;
  MUIOO::PRINT(os,"**");
}
