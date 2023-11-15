#ifndef __PDBMPCEXHOTDEAD_HH__
#define __PDBMPCEXHOTDEAD_HH__

#include <PdbCalChan.hh>

class PdbMpcExHotDead : public PdbCalChan {

public:

  //! constructor
  PdbMpcExHotDead() {
    _key = 50000;
    _high_gain_status = 9999;
    _low_gain_status = 9999;
  }

  //! destructor
  virtual ~PdbMpcExHotDead() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short key, unsigned short high_gain_status, unsigned short low_gain_status) {
    _key = key;
    _high_gain_status = high_gain_status;
    _low_gain_status = low_gain_status;
  }

  //! get the key of the minipad corresponding to this hot/dead information
  unsigned short get_key() const {
    return _key;
  }

  //! get the hot/dead status word for the high gain channel of this minipad
  unsigned short get_high_gain_status() const {
    return _high_gain_status;
  }

  //! get the hot/dead status word for the low gain channel of this minipad
  unsigned short get_low_gain_status() const {
    return _low_gain_status;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! hot/dead status word for high gain channel
  unsigned short _high_gain_status;

  //! hot/dead status word for low gain channel
  unsigned short _low_gain_status;

  ClassDef(PdbMpcExHotDead,2)
};

#endif /* __PDBMPCEXMIPS_HH__ */
