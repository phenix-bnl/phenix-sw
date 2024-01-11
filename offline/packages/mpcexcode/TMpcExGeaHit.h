#ifndef __TMPCEXGEAHIT_H__
#define __TMPCEXGEAHIT_H__

/**
 * @class  TMpcExGeaHit
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  The persisted GEANT hit within the key framework
 */

#include "PHObject.h"
#include <map>

class TMpcExGeaHit : public PHObject {

 public:

  //! default constructor - necessary since this for ROOT I/O
  TMpcExGeaHit() : _key(0xffff), _total_e(0) {}

  //! Construct with the key, low gain adc and high gain adc
  TMpcExGeaHit(unsigned int key) : _key(key), _total_e(0) {}

  //! Destructor
  virtual ~TMpcExGeaHit() {}

  //! Reset the object, give it garbage values
  void Reset() {
    _key = 0xFFFFFFFF;
    _total_e = -9999.;
    contributors.clear(); 
  }

  //! set the total energy deposited in the given minipad
  void set_e(const float e) { _total_e = e; }

  //! the key
  unsigned int key() const { return _key; }

  //! the low gain value
  float e() const { return _total_e; }

  //! list of contributors by true track number and energy
  
  void add_contributor(int mctrack, float energy)
  {
    std::map<int,float>::iterator it = contributors.find(mctrack); 
    if(it!=contributors.end()){
      // update the energy for this track in the map
      float e = it->second; 
      it->second = e + energy; 
    }
    else{
      // add a new entry to the map
      contributors[mctrack] = energy; 
    }

  }

  std::pair<std::map<int,float>::iterator,std::map<int,float>::iterator>  get_contributors(){
    return std::make_pair(contributors.begin(),contributors.end()); 
  }

 private:

  //! key of the minipad
  unsigned int _key;

  //! the total energy deposited in the minipad
  float _total_e;

  //! map between true track and energy deposited
  std::map<int,float> contributors; 

  ClassDef(TMpcExGeaHit,2)

};

#endif /* __TMPCEXGEAHIT_H__ */
