#ifndef __PDBMPCEXHIGHLOW_HH__
#define __PDBMPCEXHIGHLOW_HH__

#include <PdbCalChan.hh>

class PdbMpcExHighLow : public PdbCalChan {

public:

  //! constructor
  PdbMpcExHighLow() {
    _key = 50000;
    _high_low_ratio = -9999.;
    _high_low_ratio_error = -9999.;
    _high_low_offset = -9999.;
    _high_low_offset_error = -9999.;
    _high_low_sigma = -9999.;
    _high_low_sigma_error = -9999.;
  }

  //! destructor
  virtual ~PdbMpcExHighLow() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short key, float high_low, float high_low_error, float offset, float offset_error, float sigma, float sigma_error)
  {
    _key = key;
    _high_low_ratio = high_low;
    _high_low_ratio_error = high_low_error;
    _high_low_offset = offset;
    _high_low_offset_error = offset_error;
    _high_low_sigma = sigma;
    _high_low_sigma_error = sigma_error;
  }

  //! get the key of the minipad corresponding to this hot/dead information
  unsigned short get_key() const {
    return _key;
  }

  //! get the high/low ratio for this minipad
  float get_high_low() const {
    return _high_low_ratio;
  }

  //! get the error on the high/low ratio for this minipad
  float get_high_low_error() const {
    return _high_low_ratio_error;
  }

  //! get the offset in high vs. low for this minipad
  float get_offset() const {
    return _high_low_offset;
  }

  //! get the error on the offset for this minipad
  float get_offset_error() const {
    return _high_low_offset_error;
  }

  //! get the width of the high/low ratio for this minipad
  float get_sigma() const {
    return _high_low_sigma;
  }

  //! get the error on the width of the high/low ratio for this minipad
  float get_sigma_error() const {
    return _high_low_sigma_error;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! high/low ratio for this minipad
  float _high_low_ratio;

  //! high/low ratio error for this minipad
  float _high_low_ratio_error;

  //! high/low offset for this minipad
  float _high_low_offset;

  //! high/low offset error for this minipad
  float _high_low_offset_error;

  //! high/low width for this minipad
  float _high_low_sigma;

  //! high/low width error for this minipad
  float _high_low_sigma_error;

  ClassDef(PdbMpcExHighLow,1)
};

#endif /* __PDBMPCEXHIGHLOW_HH__ */
