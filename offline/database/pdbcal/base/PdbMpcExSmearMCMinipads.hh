#ifndef __PDBMPCEXSMEARMCMINIPADS_HH__
#define __PDBMPCEXSMEARMCMINIPADS_HH__

#include <PdbCalChan.hh>

class PdbMpcExSmearMCMinipads : public PdbCalChan {

public:

  //! constructor
  PdbMpcExSmearMCMinipads() {
    _key = 50000;
    _scale = 0.0;
    _smear = 0.0;
  }

  //! destructor
  virtual ~PdbMpcExSmearMCMinipads() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short key, float scale, float smear)
  {
    _key = key;
    _scale = scale;
    _smear = smear;
  }

  //! get the key of the minipad corresponding to this hot/dead information
  unsigned short get_key() const {
    return _key;
  }

  //! get the minipad scale factor
  float get_scale() const {
    return _scale;
  }

  //! get the minipad smear factor
  float get_smear() const {
    return _smear;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! scale factor of a given minipad
  float _scale;

  //! smear factor of a given minipad
  float _smear;

  ClassDef(PdbMpcExSmearMCMinipads,1)
};

#endif /* __PDBMPCEXMINIPADMIPCORRECTION_HH__ */
