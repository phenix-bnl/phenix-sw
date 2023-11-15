#ifndef __PDBMPCEXMIPS_HH__
#define __PDBMPCEXMIPS_HH__

#include <PdbCalChan.hh>

class PdbMpcExMips : public PdbCalChan {

public:

  //! constructor -- note a bad low-to-high ratio will be 0 so that bad low mips will also be 0
  PdbMpcExMips() {
    _arm = 9999;
    _layer = 9999;
    _type = 9999;
    _sensor_x = 9999;
    _sensor_y = 9999;
    _minipad_x = 9999;
    _minipad_y = 9999;
    _high_gain_mip = -9999.;
    _low_high_slope = 0;
    _low_high_fit = 0;
  }

  //! destructor
  virtual ~PdbMpcExMips() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short arm, unsigned short layer, unsigned short type, unsigned short sensor_x, unsigned short sensor_y, unsigned short minipad_x, unsigned short minipad_y, float high_gain_mip, float low_high_slope, float low_high_fit){
    _arm = arm;
    _layer = layer;
    _type = type;
    _sensor_x = sensor_x;
    _sensor_y = sensor_y;
    _minipad_x = minipad_x;
    _minipad_y = minipad_y;
    _high_gain_mip = high_gain_mip;
    _low_high_slope = low_high_slope;
    _low_high_fit = low_high_fit;
  }

  //! get the arm of the minipad corresponding to this mip information
  unsigned short get_arm() const {
    return _arm;
  }

  //! get the layer of the minipad corresponding to this mip information
  unsigned short get_layer() const {
    return _layer;
  }

  //! get the xy type of the minipad corresponding to this mip information
  unsigned short get_type() const {
    return _type;
  }

  //! get the sensor_x of the minipad corresponding to this mip information
  unsigned short get_sensor_x() const {
    return _sensor_x;
  }

  //! get the sensor_y of the minipad corresponding to this mip information
  unsigned short get_sensor_y() const {
    return _sensor_y;
  }

  //! get the minipad_x of the minipad corresponding to this mip information
  unsigned short get_minipad_x() const {
    return _minipad_x;
  }

  //! get the minipad_y of the minipad corresponding to this mip information
  unsigned short get_minipad_y() const {
    return _minipad_y;
  }

  //! get the high gain MIP after pedestal subtraction for this minipad
  float get_high_gain_mip() const {
    return _high_gain_mip;
  }

  //! get the low-to-high gain from high vs. low slope after pedestal subtraction for this minipad
  float get_low_high_slope() const {
    return _low_high_slope;
  }

  //! get the low-to-high gain from simultaneous high and low MIP fit after pedestal subtraction for this minipad
  float get_low_high_fit() const {
    return _low_high_fit;
  }

  //! get the low gain MIP as high gain mip * low-to-high ratio from slope determination
  float get_low_gain_mip_slope() const {
    return _high_gain_mip*_low_high_slope;
  }

  //! get the low gain MIP as high gain mip * low-to-high ratio from simultaneous fit
  float get_low_gain_mip_fit() const {
    return _high_gain_mip*_low_high_fit;
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

  //! high gain mip adc position from fit (after pedestal subtraction)
  float _high_gain_mip;

  //! low-to-high ratio from slope of high vs. low (after pedestal subtraction)
  float _low_high_slope;

  //! low-to-high ratio from fitting MIP in high and low simultaneously (after pedestal subtraction)
  float _low_high_fit;

  ClassDef(PdbMpcExMips,1)
};

#endif /* __PDBMPCEXMIPS_HH__ */
