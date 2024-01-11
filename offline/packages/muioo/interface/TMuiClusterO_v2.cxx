// $Id: TMuiClusterO_v2.cxx,v 1.1 2006/04/22 01:58:27 hpereira Exp $
#include<TMuiClusterO_v2.h>
#include<TMuiHitMapO.h>
#include<TDataType.h>
#include<TMuiGeo.h>

ClassImp(TMuiClusterO_v2)

  using namespace std;

/*! Version v2*/
TMuiClusterO_v2::TMuiClusterO_v2()
{
  _arm = 0;
  _plane = 0;
  _panel = 0;
  _orientation = 0;
  _size = 0;
  _index = 0;
  
  fill(_centroidpos,_centroidpos+POINT_SIZE,0);
  fill(_centroidsigma,_centroidsigma+POINT_SIZE,0);
  fill(_point_1,_point_1+POINT_SIZE,0);
  fill(_point_2,_point_2+POINT_SIZE,0);
}

TMuiClusterO_v2::TMuiClusterO_v2(const Key& key,
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
  fill(_centroidpos, _centroidpos + POINT_SIZE, 0);
  fill(_centroidsigma, _centroidsigma + POINT_SIZE, 0);
  fill(_point_1, _point_1 + POINT_SIZE, 0);
  fill(_point_2, _point_2 + POINT_SIZE, 0);
}


TMuiClusterO_v2::TMuiClusterO_v2(const TMuiClusterO* base_ptr) :
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
  set_coord(PHLine(base_ptr->get_coord_begin(),
                   base_ptr->get_coord_end()));
}

TMuiClusterO_v2::TMuiClusterO_v2(const TMuiClusterO& base_ref) :
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
  set_coord(PHLine(base_ref.get_coord_begin(),
		   base_ref.get_coord_end()));
}

void TMuiClusterO_v2::set_centroidpos(const PHPoint& point)
{
  _centroidpos[0] = point.getX();
  _centroidpos[1] = point.getY();
  _centroidpos[2] = point.getZ();
}

void TMuiClusterO_v2::set_centroidsigma(const PHPoint& point)
{
  _centroidsigma[0] = point.getX();
  _centroidsigma[1] = point.getY();
  _centroidsigma[2] = point.getZ();
}

PHPoint
TMuiClusterO_v2::get_centroidpos() const
{
  return PHPoint(_centroidpos[0],
                 _centroidpos[1],
                 _centroidpos[2]);
}

PHPoint
TMuiClusterO_v2::get_centroidsigma() const
{
  return PHPoint(_centroidsigma[0],
                 _centroidsigma[1],
                 _centroidsigma[2]);
}

PHLine TMuiClusterO_v2::get_coord() const
{
  return PHLine(PHPoint(_point_1[0],
  			_point_1[1],
  			_point_1[2]),
  		PHPoint(_point_2[0],
  			_point_2[1],
  			_point_2[2]));
  return PHLine();
}

PHPoint TMuiClusterO_v2::get_coord_end() const
{
  return PHPoint(_point_2[0],
		 _point_2[1],
		 _point_2[2]);
}

PHPoint TMuiClusterO_v2::get_coord_begin() const
{
  return PHPoint(_point_1[0],
		 _point_1[1],
		 _point_1[2]);
}

void TMuiClusterO_v2::set_coord(const PHLine& line) 
{
  _point_1[0] = line.getBasepoint().getX();
  _point_1[1] = line.getBasepoint().getY();
  _point_1[2] = line.getBasepoint().getZ();
  _point_2[0] = _point_1[0] + line.getDirection().getX();
  _point_2[1] = _point_1[1] + line.getDirection().getY();
  _point_2[2] = _point_1[2] + line.getDirection().getZ();
}

void TMuiClusterO_v2::set_coord_end(const PHPoint& point) 
{
  _point_2[0] = point.getX();
  _point_2[1] = point.getY();
  _point_2[2] = point.getZ();
}

void TMuiClusterO_v2::set_coord_begin(const PHPoint& point) 
{
  _point_1[0] = point.getX();
  _point_1[1] = point.getY();
  _point_1[2] = point.getZ();
}

//___________________________________________________________
void TMuiClusterO_v2::print(ostream& os ) const
{
  MUIOO::PRINT(os,GetName());
  os << " index: " << _index << endl;
  os 
    << " arm: " << _arm 
    << " plane: " << _plane 
    << " panel: " << _panel 
    << " orientation: " << _orientation << endl;
 
  os << " centroidpos: (" << _centroidpos[0] << "," << _centroidpos[1] << "," << _centroidpos[2] <<")" << endl;
  os << " centroidsigma: (" << _centroidsigma[0] << "," << _centroidsigma[1] << "," << _centroidsigma[2] <<")" << endl;
	
  os << " w_absolute: " << get_w_absolute() << " error: " << get_error() << endl;
  os << " x1: " << _point_1[0] << " y1: " << _point_1[1] << " z1: " << _point_1[2] << endl;
  os << " x2: " << _point_2[0] << " y2: " << _point_2[1] << " z2: " << _point_2[2] << endl;
	
  // calculate the foot of the cluster
  double alpha = 
    ( _point_1[0]*( _point_1[0] - _point_2[0] ) + _point_1[1]*( _point_1[1] - _point_2[1] ) )/
    ( MUIOO::SQUARE( _point_1[0] - _point_2[0] ) + MUIOO::SQUARE( _point_1[1] - _point_2[1] ) );
	
  double x_foot = _point_1[0] + alpha*(_point_2[0] - _point_1[0]);
  double y_foot = _point_1[1] + alpha*(_point_2[1] - _point_1[1]);
  os 
			<< " xf: " << x_foot 
			<< " yf: " << y_foot 
			<< " rf: " << sqrt( MUIOO::SQUARE( x_foot ) + MUIOO::SQUARE( y_foot ) )
			<< endl;
		
  os 
    << " angle panel: " << TMuiGeo::get_panel_angle( get_location() )
    << " cluster: " << atan2( _point_2[1] - _point_1[1], _point_2[0] - _point_1[0] ) << endl;
	
  os << " number of associated hits: " 
     << get_associated<TMuiHitO>().count()
     << endl;
  MUIOO::PRINT(os,"**");
}
