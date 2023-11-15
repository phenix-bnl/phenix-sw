#ifndef __PDBMPCEDPEDESTALSHIFT_HH__
#define __PDBMPCEDPEDESTALSHIFT_HH__

#include <PdbCalChan.hh>

class PdbMpcExPedestalShift : public PdbCalChan {

public:

  //! Constructor
  PdbMpcExPedestalShift() {
    _key = 50000;
    _low_shift = 0.;
    _high_shift = 0.;
  }

  //! Destructor
  virtual ~PdbMpcExPedestalShift() {}

  //! Override the print function
  void print() const;

  //! set the pedestal shifts of the given minipad
  void set_pedestal_shift(unsigned int key, float high_shift, float low_shift){
    _key = key;
    _low_shift = low_shift;
    _high_shift = high_shift;
  }

  //! get the key corresponding to this minipad
  unsigned short get_key() const {
    return _key;
  }

  //! get the low-gain pedestal shift
  float get_low_shift() const {
    return _low_shift;
  }

  //! get the high-gain pedestal shift
  float get_high_shift() const {
    return _high_shift;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! shift of low-gain channel relative to online pedestal table
  float _low_shift;

  //! shift of high-gain channel relative to online pedestal table
  float _high_shift;

  ClassDef(PdbMpcExPedestalShift,1)

};

#endif /* __PDBMPCEDPEDESTALSHIFT_HH__ */
