#include<TFvtxCompactCoord_v1.h>
#include <FvtxGeom.h>

ClassImp(TFvtxCompactCoord_v1)

TFvtxCompactCoord_v1::TFvtxCompactCoord_v1() :
  _peak_strip(0)
{
  std::fill(_point_1,_point_1+POINT_SIZE,0);
  std::fill(_point_2,_point_1+POINT_SIZE,0);
}

TFvtxCompactCoord_v1::TFvtxCompactCoord_v1(const Key& key,
			   unsigned short arm,
                           unsigned short cage,
			   unsigned short station,
			   unsigned short sector,
			   unsigned short column,
			   unsigned short index) :
  TFvtxCompactCoord(key),
  _peak_strip(0)
{
  set_arm(arm);
  set_cage(cage);
  set_station(station);
  set_sector(sector);
  set_column(column);
  set_index(index);
  std::fill(_point_1,_point_1+POINT_SIZE,0);
  std::fill(_point_2,_point_1+POINT_SIZE,0);
}

TFvtxCompactCoord_v1::TFvtxCompactCoord_v1(const TFvtxCompactCoord* base_ptr) :
  TFvtxCompactCoord(*base_ptr),
  _peak_strip(base_ptr->get_peak_strip())
{
  set_coord(PHLine(base_ptr->get_coord_begin(),
		    base_ptr->get_coord_end()));
}

TFvtxCompactCoord_v1::TFvtxCompactCoord_v1(const TFvtxCompactCoord& base_ref) :
  TFvtxCompactCoord(base_ref),
  _peak_strip(base_ref.get_peak_strip())
{
  set_coord(PHLine(base_ref.get_coord_begin(),
		   base_ref.get_coord_end()));
}


void TFvtxCompactCoord_v1::print(std::ostream& os) const {
  FVTXOO::PRINT(os,GetName());
  os << " arm: " << get_arm()
     << "  cage: " << get_cage()
     << "  station: " << get_station()
     << "  sector: " << get_sector()
     << "  column: " << get_column()
     << "  strip: " << get_index() << std::endl;

  // dump the status
  //
  os << "key: " << get_key().get_obj_key() << " " << os << " status: ";
  os << std::endl;

  os << " x1: " << _point_1[0] << " y1: " << _point_1[1] << " z1: " << _point_1[2] << std::endl;
  os << " x2: " << _point_2[0] << " y2: " << _point_2[1] << " z2: " << _point_2[2] << std::endl;
  FVTXOO::PRINT(os,"**");
}

PHLine TFvtxCompactCoord_v1::get_coord() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHLine(PHPoint(_point_1[0],
  			_point_1[1],
  			_point_1[2]),
  		PHPoint(_point_2[0],
  			_point_2[1],
  			_point_2[2]));
}

PHPoint TFvtxCompactCoord_v1::get_coord_end() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(_point_2[0],
		 _point_2[1],
		 _point_2[2]);
}


PHPoint TFvtxCompactCoord_v1::get_coord_begin() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(_point_1[0],
		 _point_1[1],
		 _point_1[2]);
}

//_______________________________________________
double TFvtxCompactCoord_v1::get_mean_z() const
{ return 0.5*(_point_1[2] + _point_2[2]); }

void TFvtxCompactCoord_v1::set_coord(const PHLine& line)
{
  _point_1[0] = line.getBasepoint().getX();
  _point_1[1] = line.getBasepoint().getY();
  _point_1[2] = line.getBasepoint().getZ();
  _point_2[0] = _point_1[0] + line.getDirection().getX();
  _point_2[1] = _point_1[1] + line.getDirection().getY();
  _point_2[2] = _point_1[2] + line.getDirection().getZ();
}
