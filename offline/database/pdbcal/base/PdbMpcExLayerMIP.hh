#ifndef __PDBMPCEXLAYERMIP_HH__
#define __PDBMPCEXLAYERMIP_HH__

#include <PdbCalChan.hh>

class PdbMpcExLayerMIP : public PdbCalChan {

public:

  //! constructor
  PdbMpcExLayerMIP() {
    _arm = 9999;    //obtainable directly from key with mapper
    _layer = 9999;  //obtainable directly from key with mapper
    _mip_mpv = -9999.;
    _mip_sigma = -9999.;
  }

  //! destructor
  virtual ~PdbMpcExLayerMIP() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information
  void set_data(unsigned short arm, unsigned short layer, float mpv, float sigma)
  {
    _arm = arm;
    _layer = layer;
    _mip_mpv = mpv;
    _mip_sigma = sigma;
  }

  //! get the arm of the layer corresponding to this mip information
  unsigned short get_arm() const {
    return _arm;
  }

  //! get the layer corresponding to this mip information
  unsigned short get_layer() const {
    return _layer;
  }

  //! get the mip_mpv for this layer
  float get_mip_mpv() const {
    return _mip_mpv;
  }

  //! get the mip sigma for this layer
  float get_mip_sigma() const {
    return _mip_sigma;
  }

private:

  //! arm of a given layer
  unsigned short _arm;

  //! layer [0,8)
  unsigned short _layer;

  //! mip most probable value for this layer
  float _mip_mpv;

  //! mip sigma for this layer
  float _mip_sigma;

  ClassDef(PdbMpcExLayerMIP,1)
};

#endif /* __PDBMPCEXLAYERMIP_HH__ */
