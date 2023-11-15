#include "PdbMpcExCoordinateMap.hh"

PdbMpcExCoordinateMap::PdbMpcExCoordinateMap(){
  Reset();
}

void PdbMpcExCoordinateMap::print() const {
  std::cout<<_arm<<" "<<_packet<<" "<<_chipmap<<" "<<_nx<<" "<<_ny<<" "<<_x<<" "<<_y<<" "<<_z<<" "<<_layer<<" "<<_topbottom<<" "<<_chain<<" "<<_lx<<" "<<_ly<<" "<<_chip<<" "<<_quadrant<<" "<<_sensor_in_quadrant<<std::endl;
}

void PdbMpcExCoordinateMap::Reset() {
  //set garbage values...
  _arm = 2;
  _packet = 8;
  _chipmap = 3072;
  _nx = 196;
  _ny = 196;
  _layer = 8;
  _topbottom = 2;
  _chain = 4;
  _lx = 32;
  _ly = 4;
  _chip = 48;
  _quadrant = 6;
  _sensor_in_quadrant = 6;
  _x = -9999.;
  _y = -9999.;
  _z = -9999.;
}
