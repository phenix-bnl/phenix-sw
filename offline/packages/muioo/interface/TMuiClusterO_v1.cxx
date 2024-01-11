// $Id: TMuiClusterO_v1.cxx,v 1.1 2006/04/22 01:58:27 hpereira Exp $
#include<TMuiClusterO_v1.h>
#include<TMuiHitMapO.h>
#include<TDataType.h>

ClassImp(TMuiClusterO_v1)
  
//___________________________________________
/*! Version v1*/
TMuiClusterO_v1::TMuiClusterO_v1()
{
  _arm = 0;
  _plane = 0;
  _panel = 0;
  _orientation = 0;
  _size = 0;
  _index = 0;
  std::fill(_centroidpos,_centroidpos+POINT_SIZE,0);
  std::fill(_centroidsigma,_centroidsigma+POINT_SIZE,0);
}

//___________________________________________
TMuiClusterO_v1::TMuiClusterO_v1(const Key& key,
                           UShort_t arm,
                           UShort_t plane,
                           UShort_t panel,
                           UShort_t orientation,
                           UShort_t index) :
  TMuiClusterO(key)
{
  _arm = arm;
  _plane = plane;
  _panel = panel;
  _orientation = orientation;
  _size = 0;
  _index = index;
  
  std::fill(_centroidpos,_centroidpos+POINT_SIZE,0);
  std::fill(_centroidsigma,_centroidsigma+POINT_SIZE,0);
}


//___________________________________________
TMuiClusterO_v1::TMuiClusterO_v1(const TMuiClusterO* base_ptr) :
  TMuiClusterO(*base_ptr)
{
  _arm = base_ptr->get_arm();
  _plane = base_ptr->get_plane();
  _panel = base_ptr->get_panel();
  _orientation = base_ptr->get_orientation();
  _size = base_ptr->get_size();
  _index = base_ptr->get_index();
  set_centroidpos(base_ptr->get_centroidpos());
  set_centroidsigma(base_ptr->get_centroidsigma());
}

//___________________________________________
TMuiClusterO_v1::TMuiClusterO_v1(const TMuiClusterO& base_ref) :
  TMuiClusterO(base_ref)
{
  _arm = base_ref.get_arm();
  _plane = base_ref.get_plane();
  _panel = base_ref.get_panel();
  _orientation = base_ref.get_orientation();
  _size = base_ref.get_size();
  _index = base_ref.get_index();
  set_centroidpos(base_ref.get_centroidpos());
  set_centroidsigma(base_ref.get_centroidsigma());
}

//___________________________________________
void TMuiClusterO_v1::set_centroidpos(const PHPoint& point)
{
  _centroidpos[0] = point.getX();
  _centroidpos[1] = point.getY();
  _centroidpos[2] = point.getZ();
}

//___________________________________________
void TMuiClusterO_v1::set_centroidsigma(const PHPoint& point)
{
  _centroidsigma[0] = point.getX();
  _centroidsigma[1] = point.getY();
  _centroidsigma[2] = point.getZ();
}

//___________________________________________
PHPoint TMuiClusterO_v1::get_centroidpos() const
{
  return PHPoint(_centroidpos[0],
                 _centroidpos[1],
                 _centroidpos[2]);
}

//___________________________________________
PHPoint TMuiClusterO_v1::get_centroidsigma() const
{
  return PHPoint(_centroidsigma[0],
                 _centroidsigma[1],
                 _centroidsigma[2]);
}

//___________________________________________
void TMuiClusterO_v1::print( std::ostream& os ) const 
{
	MUIOO::PRINT(os,GetName());
	 os << " index: " << _index << std::endl;
	 os << " arm: " << _arm << std::endl;
	 os << " plane: " << _plane << std::endl;
	 os << " panel: " << _panel << std::endl;
	 os << " orientation: " << _orientation << std::endl;
	 os << " centroidpos: (" << _centroidpos[0] << ","
	    << _centroidpos[1] << ","
	    << _centroidpos[2] <<")" << std::endl;
	 os << " w_absolute: " << get_w_absolute() << std::endl;
	MUIOO::PRINT(os,"**");
}
