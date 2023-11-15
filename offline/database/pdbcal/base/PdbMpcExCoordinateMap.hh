#ifndef __PDBMPCEXCOORDINATEMAP_HH__
#define __PDBMPCEXCOORDINATEMAP_HH__

#include <PdbCalChan.hh>
#include <iostream>

class PdbMpcExCoordinateMap : public PdbCalChan {

 public:

  PdbMpcExCoordinateMap();

  virtual ~PdbMpcExCoordinateMap() {}

  //! Override the print function
  void print() const;

  void Reset();

  //! 0=south, 1=north
  void set_arm(const unsigned short arm) { _arm = arm; }

  //! 0-7 for the numerical paket index for each arm
  void set_packet(const unsigned short packet) { _packet = packet; }

  //! 0-3071 = chain*12*64+chip*64+rocbond
  void set_chipmap(const unsigned short chipmap) { _chipmap = chipmap; }

  //! 0-196 for global x index in a layer
  void set_nx(const unsigned short nx) { _nx = nx; }

  //! 0-196 for global y index in a layer
  void set_ny(const unsigned short ny) { _ny = ny; }

  //! 0-7 the layer in the arm
  void set_layer(const unsigned short layer) { _layer = layer; }

  //! 0=top 1=bottom for the half of the arm
  void set_topbottom(const unsigned short topbottom) { _topbottom = topbottom; }

  //! 0-3 the chain within a packet
  void set_chain(const unsigned short chain) { _chain = chain; }

  //! 0-31 local "x" coordinate for the smaller side of the minipads
  void set_lx(const unsigned short lx) { _lx = lx; }

  //! 0-3 local "y" coordinate for the longer side of the minipads
  void set_ly(const unsigned short ly) { _ly = ly; }

  //! 0-47 for the chip within a packet
  void set_chip(const unsigned short chip) { _chip = chip; }

  //! 0-3 the quadrant number of a layer counting from 0 upper left as looking at a layer and increasing clockwise.
  void set_quadrant(const unsigned short quadrant) { _quadrant = quadrant; }

  //! 0-6 the chip within a quadrant indexed according to readout order
  void set_sensor_in_quadrant(const unsigned short sensor_in_quadrant) { _sensor_in_quadrant = sensor_in_quadrant; }

  //! global x coordinate
  void set_x(const float x) { _x = x; }
  
  //! global y coordinate
  void set_y(const float y) { _y = y; }

  //! global z coordinate
  void set_z(const float z) { _z = z; }

  //! 0=south, 1=north
  unsigned short arm() const { return _arm; }

  //! 0-7 for the numerical paket index for each arm
  unsigned short packet() const { return _packet; }

  //! 0-3071 = chain*12*64+chip*64+rocbond
  unsigned short chipmap() const { return _chipmap; }

  //! 0-196 for global x index in a layer
  unsigned short nx() const { return _nx; }

  //! 0-196 for global y index in a layer
  unsigned short ny() const { return _ny; }

  //! 0-7 the layer in the arm
  unsigned short layer() const { return _layer; }

  //! 0=top 1=bottom for the half of the arm
  unsigned short topbottom() const { return _topbottom; }

  //! 0-4 for the chain within a packet
  unsigned short chain() const { return _chain; }

  //! 0-31 local "x" coordinate for the smaller side of the minipads
  unsigned short lx() const { return _lx; }

  //! 0-3 local "y" coordinate for the longer side of the minipads
  unsigned short ly() const { return _ly; }

  //! 0-47 for the chip within a packet
  unsigned short chip() const { return _chip; }

  //! 0-3 the quadrant number of a layer counting from 0 upper left as looking at a layer and increasing clockwise.
  unsigned short quadrant() const { return _quadrant; }

  //! 0-6 the chip within a quadrant indexed according to readout order
  unsigned short sensor_in_quadrant() const { return _sensor_in_quadrant; }

  //! global x coordinate
  float x() const { return _x; }

  //! global y coordinate
  float y() const { return _y; }

  //! global z coordinate
  float z() const { return _z; }

 private:

  //for each minipad the following values are kept
  unsigned short _arm;
  unsigned short _packet;
  unsigned short _chipmap;
  unsigned short _nx;
  unsigned short _ny;
  float _x;
  float _y;
  float _z;
  unsigned short _layer;
  unsigned short _topbottom; //0=top, 1=bottom
  unsigned short _chain;
  unsigned short _lx;
  unsigned short _ly;
  unsigned short _chip;
  unsigned short _quadrant;
  unsigned short _sensor_in_quadrant;

};

#endif /* __PDBMPCEXCOORDINATEMAP_HH__ */
