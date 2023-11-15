#ifndef __PDBMPCEXOFFLINEPEDESTALS_HH__
#define __PDBMPCEXOFFLINEPEDESTALS_HH__

#include <PdbCalChan.hh>
#include <phool.h>
#include <iostream>

class PdbMpcExOfflinePedestals : public PdbCalChan {

public:

  //! constructor
  PdbMpcExOfflinePedestals() {
    _arm = 9999;
    _layer = 9999;
    _type = 9999;
    _sensor_x = 9999;
    _sensor_y = 9999;
    _minipad_x = 9999;
    _minipad_y = 9999;
    _high_pedestal = -9999.0;
    _high_pedestal_width = -9999.0;
    _high_pedestal_chi2 = -9999.0;
    _low_pedestal = -9999.0;
    _low_pedestal_width = -9999.0;
    _low_pedestal_chi2 = -9999.0;
  }

  //! destructor
  virtual ~PdbMpcExOfflinePedestals() {}

  //! Overridden print method
  void print() const {
    std::cout<<_arm<<"\t"
	     <<_layer<<"\t"
	     <<_type<<"\t"
	     <<_sensor_x<<"\t"
	     <<_sensor_y<<"\t"
	     <<_minipad_x<<"\t"
	     <<_minipad_y<<"\t"
	     <<_high_pedestal<<"\t"
	     <<_high_pedestal_width<<"\t"
	     <<_high_pedestal_chi2<<"\t"
	     <<_low_pedestal<<"\t"
	     <<_low_pedestal_width<<"\t"
	     <<_high_pedestal_chi2<<std::endl;
  }

  //! the single setting function for all information for a minipad
  void set_data(unsigned short arm, unsigned short layer, unsigned short type, unsigned short sensor_x, unsigned short sensor_y, unsigned short minipad_x, unsigned short minipad_y, float high_pedestal, float high_pedestal_width, float high_pedestal_chi2, float low_pedestal, float low_pedestal_width, float low_pedestal_chi2){
    _arm = arm;
    _layer = layer;
    _type = type;
    _sensor_x = sensor_x;
    _sensor_y = sensor_y;
    _minipad_x = minipad_x;
    _minipad_y = minipad_y;
    _high_pedestal = high_pedestal;
    _high_pedestal_width = high_pedestal_width;
    _high_pedestal_chi2 = high_pedestal_chi2;
    _low_pedestal = low_pedestal;
    _low_pedestal_width = low_pedestal_width;
    _low_pedestal_chi2 = low_pedestal_chi2;
  }

  //! get the arm of the minipad corresponding to this pedestal
  int get_arm() const {
    return _arm;
  }

  //! get the layer of the minipad corresponding to this pedestal
  int get_layer() const {
    return _layer;
  }

  //! get the xy type of the minipad corresponding to this pedestal
  int get_type() const {
    return _type;
  }

  //! get the sensor_x of the minipad corresponding to this pedestal
  int get_sensor_x() const {
    return _sensor_x;
  }

  //! get the sensor_y of the minipad corresponding to this pedestal
  int get_sensor_y() const {
    return _sensor_y;
  }

  //! get the minipad_x of the minipad corresponding to this pedestal
  int get_minipad_x() const {
    return _minipad_x;
  }

  //! get the minipad_y of the minipad corresponding to this pedestal
  int get_minipad_y() const {
    return _minipad_y;
  }

  //! get the minipad high gain pedestal for this minipad
  float get_high_pedestal() const {
    return _high_pedestal;
  }

  //! get the minipad high gain pedestal width for this minipad
  float get_high_pedestal_width() const {
    return _high_pedestal_width;
  }

  //! get the minipad high gain pedestal fit chi2 for this minipad
  float get_high_pedestal_chi2() const {
    return _high_pedestal_chi2;
  }

  //! get the minipad low gain pedestal for this minipad
  float get_low_pedestal() const {
    return _low_pedestal;
  }

  //! get the minipad low gain pedestal width for this minipad
  float get_low_pedestal_width() const {
    return _low_pedestal_width;
  }

  //! get the minipad low gain pedestal fit chi2 for this minipad
  float get_low_pedestal_chi2() const {
    return _low_pedestal_chi2;
  }

private:

  //! arm of a given minipad
  unsigned short _arm;

  //! layer of a given minipad
  unsigned short _layer;

  //! xy type of a given minipad
  unsigned short _type;

  //! sensor x of a given minipad
  unsigned short _sensor_x;

  //! sensor y of a given minipad
  unsigned short _sensor_y;

  //! minipad x of a given minipad
  unsigned short _minipad_x;

  //! minipad y of a given minipad
  unsigned short _minipad_y;

  //! The high gain pedestal for each minipad
  float _high_pedestal;

  //! The high gain pedestal width for each minipad
  float _high_pedestal_width;

  //! The high gain pedestal fit chi2 for each minipad
  float _high_pedestal_chi2;

  //! The low gain pedestal for each minipad
  float _low_pedestal;

  //! The low gain pedestal width for each minipad
  float _low_pedestal_width;

  //! The low gain pedestal fit chi2 for each minipad
  float _low_pedestal_chi2;

  ClassDef(PdbMpcExOfflinePedestals,1)
};

#endif /* __PDBMPCOFFLINEPEDESTALS_HH__ */
