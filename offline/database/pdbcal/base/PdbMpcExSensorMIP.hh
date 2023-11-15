#ifndef __PDBMPCEXSENSORMIP_HH__
#define __PDBMPCEXSENSORMIP_HH__

#include <PdbCalChan.hh>

class PdbMpcExSensorMIP : public PdbCalChan {

public:

  //! constructor
  PdbMpcExSensorMIP() {
    _arm = 9999;    //obtainable directly from key with mapper
    _packet = 9999; //obtainable directly from key with mapper
    _sensor = 9999; //chipmap/128
    _mip = -9999.;
    _mip_error = -9999.;
  }

  //! destructor
  virtual ~PdbMpcExSensorMIP() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short arm, unsigned short packet, unsigned short sensor, float mip, float mip_error)
  {
    _arm = arm;
    _packet = packet;
    _sensor = sensor;
    _mip = mip;
    _mip_error = mip_error;
  }

  //! get the arm of the sensor corresponding to this mip information
  unsigned short get_arm() const {
    return _arm;
  }

  //! get the packet of the sensor corresponding to this mip information
  unsigned short get_packet() const {
    return _packet;
  }

  //! get the sensor in packet of the sensor corresponding to this mip information
  //! this is equivalent to chipmap/128
  unsigned short get_sensor() const {
    return _sensor;
  }

  //! get the mip for this sensor
  float get_mip() const {
    return _mip;
  }

  //! get the mip error for this sensor
  float get_mip_error() const {
    return _mip_error;
  }

private:

  //! arm of a given sensor
  unsigned short _arm;

  //! packet of a given sensor
  unsigned short _packet;

  //! sensor in packet of a given sensor
  unsigned short _sensor;

  //! mip for this sensor
  float _mip;

  //! mip error for this sensor
  float _mip_error;

  ClassDef(PdbMpcExSensorMIP,1)
};

#endif /* __PDBMPCEXSENSORMIP_HH__ */
