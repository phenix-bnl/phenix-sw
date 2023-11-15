#ifndef __PDBMPCEXMINIPADNEIGHBORS_HH__
#define __PDBMPCEXMINIPADNEIGHBORS_HH__

#include <PdbCalChan.hh>

class PdbMpcExMinipadNeighbors : public PdbCalChan {

public:

  //! constructor
  PdbMpcExMinipadNeighbors() {
    _key = 50000;
    _neighborkey = 50000;
  }

  //! destructor
  virtual ~PdbMpcExMinipadNeighbors() {}

  //! Overridden print method
  void print() const;

  //! the single setting function for all information for a minipad
  void set_data(unsigned short key, unsigned short neighbor)
  {
    _key = key;
    _neighborkey = neighbor;
  }

  //! get the key of the minipad corresponding to this blob 
  unsigned short get_key() const {
    return _key;
  }

  //! get the minipad's neighbor key
  float get_neighbor_key() const {
    return _neighborkey;
  }

private:

  //! offline key of a given minipad
  unsigned short _key;

  //! offline key of this minipad's neighbor
  unsigned short _neighborkey;

  ClassDef(PdbMpcExMinipadNeighbors,1)
};

#endif /* __PDBMPCEXMINIPADNEIGHBORS_HH__ */
