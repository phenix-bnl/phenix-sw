#ifndef __MPCEXMAPPER_H__
#define __MPCEXMAPPER_H__

/**
 * @class  MpcExMapper
 * @author ngrau@augie.edu
 * @date   June 2015
 *
 * @brief  A singleton object that can map between the existing coordinate systems.
 */

#include <cstdlib>
#include <set>

class MpcExMapper {

 public:

  //! The destructor
  virtual ~MpcExMapper() {}

  //! Grab the single instance of the mapper
  static MpcExMapper* instance() {
    if(_instance == NULL){
      _instance = new MpcExMapper();
    }
    return _instance;
  }

  //! generate the unique key of a minipad given the arm, packet index and chipmap
  virtual unsigned int generate_key(const unsigned short arm, const unsigned short packet, const unsigned short chipmap) const;

  //! generate the chipmap variable from the given chain, chip, and rocbond coordinate for the minipad
  virtual unsigned short get_chipmap(const unsigned short chain, const unsigned short chip, const unsigned short rocbond) const;

  //! get the arm
  virtual unsigned short get_arm(const unsigned int key) const;

  //! get the packet index
  virtual unsigned short get_packet(const unsigned int key) const;

  //! get the chipmap
  virtual unsigned short get_chipmap(const unsigned int key) const;

  //! get the chain in a packet
  virtual unsigned short get_chain(const unsigned int key) const;

  //! get the chip in a packet
  virtual unsigned short get_chip_in_packet(const unsigned int key) const;

  //! get the chip in a chain
  virtual unsigned short get_chip(const unsigned int key) const;

  //! get the minipad index within a chip
  virtual unsigned short get_rocbond(const unsigned int key) const;

  //! get the global x position for the center of a minipad
  virtual float get_x(const unsigned int key) const;

  //! get the global y position
  virtual float get_y(const unsigned int key) const;

  //! get the global z position
  virtual float get_z(const unsigned int key) const;

  //! get the hough slope of the minipad in the x direction for a given collisions vertex
  virtual float get_hsx(const unsigned int key, float z_vertex) const;

  //! get the hough slope of the minipad in the y direction for a given collisions vertex
  virtual float get_hsy(const unsigned int key, float z_vertex) const;

  //! get the minipad width in the x direction 
  virtual float get_minipad_x_width(const unsigned int key) const;

  virtual float get_minipad_y_width(const unsigned int key) const;

  //! get the layer
  virtual unsigned short get_layer(const unsigned int key) const;

  //! get the top/bottom 0=top, 1=bottom
  virtual unsigned short get_topbottom(const unsigned int key) const;

  //! get the quadrant
  virtual unsigned short get_quadrant(const unsigned int key) const;

  //! get the sensor in quadrant
  virtual unsigned short get_sensor_in_quadrant(const unsigned int key) const;

  //! get the local x coordiante of a minipad
  virtual unsigned short get_lx(const unsigned int key) const;

  //! get the local y coordiante of a minipad
  virtual unsigned short get_ly(const unsigned int key) const;

  //! get the global x coordiante of a minipad
  virtual unsigned short get_nx(const unsigned int key) const;

  //! get the global y coordiante of a minipad
  virtual unsigned short get_ny(const unsigned int key) const;

  //! get the chain given the arm, layer, sx, and sy
  virtual unsigned short get_chain(const unsigned short arm, const unsigned short layer, const unsigned short sx, const unsigned short sy) const;

 protected:

  //! Protected Constructor that fills out small internal LUTs
  MpcExMapper();

  //! The single instance of the object
  static MpcExMapper* _instance;

  //! Internal coordinate structure to hold the huge mapping
  class MinipadCoordinate {

  public:
  MinipadCoordinate() : arm(-1), packet(-1), chipmap(-1), nx(-1), ny(-1), x(-9999.), y(-9999.), z(-9999.), layer(-1), topbottom(-1), chain(-1), lx(-1), ly(-1), chip(-1), quadrant(-1), qsensor(-1), key(50000) {}
    virtual ~MinipadCoordinate() {}

    //! 0=south, 1=north
    int arm;
    //! 0-7 for the numerical paket index for each arm
    int packet;
    //! 0-3071 = chain*12*64+chip*64+rocbond
    int chipmap;
    //! 0-191 for global x index in a layer
    int nx;
    //! 0-191 for global y index in a layer
    int ny;
    //! global x coordinate
    float x;
    //! global y coordinate
    float y;
    //! global z coordinate
    float z;
    //! 0-7 the layer in the arm
    int layer;
    //! 0=top 1=bottom for the half of the arm
    int topbottom;
    //! 0-3 the chain within a packet
    int chain;
    //! 0-31 local "x" coordinate for the smaller side of the minipads
    int lx;
    //! 0-3 local "y" coordinate for the longer side of the minipads
    int ly;
    //! 0-47 for the chip within a packet
    int chip;
    //! 0-3 the quadrant number of a layer counting from 0 upper left as looking at a layer and increasing clockwise.
    int quadrant;
    //! 0-6 the chip within a quadrant indexed according to readout order
    int qsensor;
    //! the generated coordinate from arm/packet/chipmap
    unsigned int key;

  };

  //! helper class to order the coordinates within a set by their key
  class CompareMinipadCoordinateByKey {
  public:
    bool operator()(MinipadCoordinate* m1, MinipadCoordinate* m2) const {
      return m1->key < m2->key;
    }
  };

  //! Grab the MinipadCoordinate object for a given key
  MinipadCoordinate* coordinate_from_key(const unsigned int key) const;

  //! the internal lookup of coordinates by key
  std::set<MinipadCoordinate*,CompareMinipadCoordinateByKey> _big_lut;

};

#endif /* __MPCEXMAPPER_H__ */
