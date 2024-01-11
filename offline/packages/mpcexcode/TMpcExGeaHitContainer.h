#ifndef __TMPCEXGEAHITCONTAINER_H__
#define __TMPCEXGEAHITCONTAINER_H__

/**
 * @class  TMpcExGeaHitContainer
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  A container of TMpcExGeaHit pointers and on the node tree and can be persistified.
 */

#include <vector>
#include "PHObject.h"
#include "TMpcExGeaHit.h"

class TMpcExGeaHitContainer : public PHObject {

 public:

  //! Constructor of empy hit container
  TMpcExGeaHitContainer() : _hits() {}

  //! Destructor
  virtual ~TMpcExGeaHitContainer() {
    Reset();
  }

  //! add a hit into the container, the container takes ownership of the pointer.
  void addHit(TMpcExGeaHit* hit);

  //! the number of hits in the container
  size_t size() const {
    return _hits.size();
  }

  //! Get the hit at a given index 0-size() for random access
  TMpcExGeaHit* getHit(const size_t index) const;

  //! delete all of the hits in the container and clear it so there is nothing in the container
  void Reset();

  //! Grab a hit from the container by key
  TMpcExGeaHit* get_hit_by_key(unsigned int key) const;

 private:

  typedef std::vector<TMpcExGeaHit*> container;

  //! the internal container geant hits
  container _hits;

  ClassDef(TMpcExGeaHitContainer,1)
};

#endif /* __TMPCEXHITCONTAINER_H__ */
