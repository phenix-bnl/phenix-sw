#include<TFvtxCoord_v1.h>
#include <FvtxGeom.h>

ClassImp(TFvtxCoord_v1)

TFvtxCoord_v1::TFvtxCoord_v1() :
  _arm(0),
  _cage(0),
  _station(0),
  _sector(0),
  _column(0),
  _index(0),
  _q_peak(0),
  _q_tot(0),
  _peak_strip(0),
  _error(0),
  _w(0),
  _status(0),
  _q_error(0)
{
  std::fill(_point_1,_point_1+POINT_SIZE,0);
  std::fill(_point_2,_point_1+POINT_SIZE,0);
}

TFvtxCoord_v1::TFvtxCoord_v1(const Key& key,
			   unsigned short arm,
                           unsigned short cage,
			   unsigned short station,
			   unsigned short sector,
			   unsigned short column,
			   unsigned short index) :
  TFvtxCoord(key),
  _arm(arm),
  _cage(cage),
  _station(station),
  _sector(sector),
  _column(column),
  _index(index),
  _q_peak(0),
  _q_tot(0),
  _peak_strip(0),
  _error(0),
  _w(0),
  _status(0),
  _q_error(0)
{
  std::fill(_point_1,_point_1+POINT_SIZE,0);
  std::fill(_point_2,_point_1+POINT_SIZE,0);
}

TFvtxCoord_v1::TFvtxCoord_v1(const TFvtxCoord* base_ptr) :
  TFvtxCoord(*base_ptr),
  _arm(base_ptr->get_arm()),
  _cage(base_ptr->get_cage()),
  _station(base_ptr->get_station()),
  _sector(base_ptr->get_sector()),
  _column(base_ptr->get_column()),
  _index(base_ptr->get_index()),
  _q_peak(base_ptr->get_q_peak()),
  _q_tot(base_ptr->get_q_tot()),
  _peak_strip(base_ptr->get_peak_strip()),
  _error(base_ptr->get_error()),
  _w(base_ptr->get_w()),
  _status(base_ptr->get_status()),
  _q_error(base_ptr->get_q_error())
{
  set_coord(PHLine(base_ptr->get_coord_begin(),
		    base_ptr->get_coord_end()));
}

TFvtxCoord_v1::TFvtxCoord_v1(const TFvtxCoord& base_ref) :
  TFvtxCoord(base_ref),
  _arm(base_ref.get_arm()),
  _cage(base_ref.get_cage()),
  _station(base_ref.get_station()),
  _sector(base_ref.get_sector()),
  _column(base_ref.get_column()),
  _index(base_ref.get_index()),
  _q_peak(base_ref.get_q_peak()),
  _q_tot(base_ref.get_q_tot()),
  _peak_strip(base_ref.get_peak_strip()),
  _error(base_ref.get_error()),
  _w(base_ref.get_w()),
  _status(base_ref.get_status()),
  _q_error(base_ref.get_q_error())
{
  set_coord(PHLine(base_ref.get_coord_begin(),
		   base_ref.get_coord_end()));
}


void TFvtxCoord_v1::print(std::ostream& os) const {
  FVTXOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << "  cage: " << _cage
     << "  station: " << _station
     << "  sector: " << _sector
     << "  column: " << _column
     << "  peak_strip: " << _peak_strip
     << "  index: " << _index << std::endl;

  // dump the status
  //
  os << "key: " << get_key().get_obj_key() << " " << os << " status: ";
  os << std::endl;

  os << " x1: " << _point_1[0] << " y1: " << _point_1[1] << " z1: " << _point_1[2] << std::endl;
  os << " x2: " << _point_2[0] << " y2: " << _point_2[1] << " z2: " << _point_2[2] << std::endl;
  os << " q_peak: " << _q_peak << " q_tot: " << _q_tot << std::endl;
  os << " q error: " << _q_error << std::endl;
  os << " w: " << _w << std::endl;
  os << " w error: " << _error << std::endl;
  FVTXOO::PRINT(os,"**");
}

PHLine TFvtxCoord_v1::get_coord() const
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

PHPoint TFvtxCoord_v1::get_coord_end() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(_point_2[0],
		 _point_2[1],
		 _point_2[2]);
}


PHPoint TFvtxCoord_v1::get_coord_begin() const
{
  // No BOUNDS_CHECK here because this
  // is not part of the public interface
  //
  return PHPoint(_point_1[0],
		 _point_1[1],
		 _point_1[2]);
}

//_______________________________________________
double TFvtxCoord_v1::get_mean_z() const
{ return 0.5*(_point_1[2] + _point_2[2]); }

void TFvtxCoord_v1::set_coord(const PHLine& line)
{
  _point_1[0] = line.getBasepoint().getX();
  _point_1[1] = line.getBasepoint().getY();
  _point_1[2] = line.getBasepoint().getZ();
  _point_2[0] = _point_1[0] + line.getDirection().getX();
  _point_2[1] = _point_1[1] + line.getDirection().getY();
  _point_2[2] = _point_1[2] + line.getDirection().getZ();
}
