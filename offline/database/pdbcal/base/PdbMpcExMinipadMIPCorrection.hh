#ifndef __PDBMPCEXMINIPADMIPCORRECTION_HH__
#define __PDBMPCEXMINIPADMIPCORRECTION_HH__

#include <PdbCalChan.hh>

class PdbMpcExMinipadMIPCorrection : public PdbCalChan {

public:

  //! constructor
  PdbMpcExMinipadMIPCorrection() {
    _key = 50000;
    _mip_correction = -9999.;
    _fit_mpv = -9999.;
    _fit_sigma = -9999.;
    _cutoff_eff = -9999.;
    _cutoff_pos = -9999.;
  }

  //! destructor
  virtual ~PdbMpcExMinipadMIPCorrection() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short key, float mip_correction, float fit_mpv, float fit_sigma, float cutoff_eff, float cutoff_pos)
  {
    _key = key;
    _mip_correction = mip_correction;
    _fit_mpv = fit_mpv;
    _fit_sigma = fit_sigma;
    _cutoff_eff = cutoff_eff;
    _cutoff_pos = cutoff_pos;
  }

  //! get the key of the minipad corresponding to this hot/dead information
  unsigned short get_key() const {
    return _key;
  }

  //! get the mip position correction for this minipad
  float get_mip_correction() const {
    return _mip_correction;
  }

  //! get the Landau fit most probable value
  float get_fit_mpv() const {
    return _fit_mpv;
  }

  //! get the Landau fit most probable value
  float get_fit_sigma() const {
    return _fit_sigma;
  }

  //! get the cutoff efficiency
  float get_cutoff_efficiency() const {
    return _cutoff_eff;
  }

  //! get the cutoff position
  float get_cutoff_position() const {
    return _cutoff_pos;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! mip position correction for this minipad
  float _mip_correction;

  //! most probable value of the Landau fit
  float _fit_mpv;

  //! sigma of the Landau fit
  float _fit_sigma;

  //! effeciency of cutoff
  float _cutoff_eff;

  //! position of cutoff
  float _cutoff_pos;

  ClassDef(PdbMpcExMinipadMIPCorrection,1)
};

#endif /* __PDBMPCEXMINIPADMIPCORRECTION_HH__ */
