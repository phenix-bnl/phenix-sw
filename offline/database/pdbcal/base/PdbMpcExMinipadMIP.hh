#ifndef __PDBMPCEXMINIPADMIP_HH__
#define __PDBMPCEXMINIPADMIP_HH__

#include <PdbCalChan.hh>

class PdbMpcExMinipadMIP : public PdbCalChan {

public:

  //! constructor
  PdbMpcExMinipadMIP() {
    _key = 50000;
    _mip_correction = -9999.;
    _mip_correction_error = -9999.;
  }

  //! destructor
  virtual ~PdbMpcExMinipadMIP() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short key, float mip_correction, float mip_correction_error)
  {
    _key = key;
    _mip_correction = mip_correction;
    _mip_correction_error = mip_correction_error;
  }

  //! get the key of the minipad corresponding to this hot/dead information
  unsigned short get_key() const {
    return _key;
  }

  //! get the mip position correction for this minipad
  float get_mip_correction() const {
    return _mip_correction;
  }

  //! get the mip position correction error for this minipad
  float get_mip_correction_error() const {
    return _mip_correction;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! mip position correction for this minipad
  float _mip_correction;

  //! mip position correction error for this minipad
  float _mip_correction_error;

  ClassDef(PdbMpcExMinipadMIP,1)
};

#endif /* __PDBMPCEXMINIPADMIP_HH__ */
