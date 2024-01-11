#include "TMpcExHit.h"
#include "MpcExMapper.h"
#include <iostream>
#include <bitset>

TMpcExHit::TMpcExHit(unsigned int key) : _key(key), _low(-9999.), _high(-9999.), _combined(0.0), _status_low(0), _status_high(0), _state_low(UNKNOWN), _state_high(UNKNOWN), _state_combined(CS_UNKNOWN) {
  MpcExMapper *mapper = MpcExMapper::instance();
  _x = mapper->get_x(_key);
  _y = mapper->get_y(_key);
  _z = mapper->get_z(_key);
  _layer = mapper->get_layer(_key);
  _x_width = mapper->get_minipad_x_width(_key);
  _y_width = mapper->get_minipad_y_width(_key);
}

void TMpcExHit::Reset(){
  _low = -9999.;
  _high = -9999.;
  _combined = 0.0;
  _status_low = 0;
  _status_high = 0;
  _state_low = UNKNOWN;
  _state_high = UNKNOWN;
  _state_combined = CS_UNKNOWN;
}

unsigned short TMpcExHit::arm() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_arm(_key);
}

unsigned short TMpcExHit::packet() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_packet(_key);
}

unsigned short TMpcExHit::chipmap() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_chipmap(_key);
}

unsigned short TMpcExHit::chain() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_chain(_key);
}

unsigned short TMpcExHit::chip() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_chip(_key);
}

unsigned short TMpcExHit::rocbond() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_rocbond(_key);
}

float TMpcExHit::x() const {
  return _x;
  //  MpcExMapper *mapper = MpcExMapper::instance();
  //  return mapper->get_x(_key);
}

float TMpcExHit::y() const {
  return _y;
  //  MpcExMapper *mapper = MpcExMapper::instance();
  //  return mapper->get_y(_key);
}

float TMpcExHit::z() const {
  return _z;
  //  MpcExMapper *mapper = MpcExMapper::instance();
  //  return mapper->get_z(_key);
}

float TMpcExHit::hsx(float z_vertex) const {
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_hsx(_key,z_vertex);
}

float TMpcExHit::hsy(float z_vertex) const {
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_hsy(_key,z_vertex);
}

unsigned short TMpcExHit::layer() const { 
  return _layer;
  //  MpcExMapper *mapper = MpcExMapper::instance();
  //  return mapper->get_layer(_key);
}

unsigned short TMpcExHit::quadrant() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_quadrant(_key);
}

unsigned short TMpcExHit::sensor_in_quadrant() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_sensor_in_quadrant(_key);
}

unsigned short TMpcExHit::lx() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_lx(_key);
}

unsigned short TMpcExHit::ly() const { 
  MpcExMapper *mapper = MpcExMapper::instance();
  return mapper->get_ly(_key);
}

float TMpcExHit::minipad_x_width() const {
  // MpcExMapper *mapper = MpcExMapper::instance();
  // return mapper->get_minipad_x_width(_key);
  return _x_width; 
}

float TMpcExHit::minipad_y_width() const {
  // MpcExMapper *mapper = MpcExMapper::instance();
  // return mapper->get_minipad_y_width(_key);
  return _y_width; 
}
