#ifndef __TMPCEXSHOWERCONTAINER_H__
#define __TMPCEXSHOWERCONTAINER_H__

/**
 * @class  TMpcExShowerContainer
 * @author lajoie@iastate.edu
 * @date   July 2015
 * @brief  The container of showers. This container owns the pointers.
 */

#include "PHObject.h"
#include "TMpcExShower.h"
#include <vector>

class TMpcExShowerContainer : public PHObject {

 public:

  //! Constructor - an empty container
  TMpcExShowerContainer() : _showers() {}

  //! Destructor -- deletes the contents of the container
  virtual ~TMpcExShowerContainer() {
    Reset();
  }

  //! Add a shower to the container -- the container owns the pointer
  void addShower(TMpcExShower* shower){
    _showers.push_back(shower);
  }

  //! Get the hit at a given index 0-size() for random access
  TMpcExShower* getShower(const size_t index) const{
    if(index<size()){
      return _showers[index];
    }
    return NULL;
  }

  //! the number of showers in the container
  size_t size() const {
    return _showers.size();
  }

  //! Deletes the contents of the container and clears the container so that it is empty
  void Reset();
  
  // Delete showers that are not the closest associated to an MPC cluster
  void Clean(); 

 private:

  typedef std::vector<TMpcExShower*> container;

  //! the internal container of showers
  container _showers;

};

#endif /* __TMPCEXSHOWERCONTAINER_H__ */
