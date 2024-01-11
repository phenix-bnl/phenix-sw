#ifndef __TMPCEXHITCONTAINER_H__
#define __TMPCEXHITCONTAINER_H__

/**
 * @class  TMpcExHitContainer
 * @author ngrau@augie.edu
 * @date   July 2015
 * @brief  A container of TMpcExHit pointers and on the node tree.
 *
 *         Internally a complete set of TMpcExHit pointers for all 
 *         minipads are kept. When "good" hit is determined by the
 *         mMpcExApplyCalibration, the hit pointer is copied into a
 *         separate container. When you call getHit(...) or 
 *         get_hit_by_key(...) a non-NULL pointer indicates that the
 *         requested hit was in the "good" hit list.
 */

#include <vector>
#include "PHObject.h"
#include "TMpcExHit.h"

class TMpcExHitContainer : public PHObject {

 public:

  //! Constructor of empty good hit container
  TMpcExHitContainer();

  //! Destructor
  virtual ~TMpcExHitContainer();

  //! get one of all of the minipads by key
  TMpcExHit* getUncalHit(unsigned int key) const;

  //! add a good hit into the container
  void addHit(TMpcExHit* hit);

  //! the number of good hits in the container
  size_t size() const {
    return _good_hits.size();
  }

  //! Get the good hit at a given index 0-size() for random access
  TMpcExHit* getHit(const size_t index) const;

  //! clear the good hits list, reset all the minipads
  void Reset();

  //! Grab a good hit from the container by key
  TMpcExHit* get_hit_by_key(unsigned int key) const;

  //! Erase this hit from the good list container
  //! This is an extremely expensive call! 
  //! But we cannot be efficient for everything. Right now we only 
  //! use this in the mMpcExDigitizeHits when running on simulated 
  //! data. If we ever want this functionality with real data, then 
  //! we might think about list or forward_list instead of vector.
  void erase(TMpcExHit *hit);

  //! The number of good hits in the north arm
  int nHits(){return _hits_north;}

  //! The number of good hits in the south arm
  int sHits(){return _hits_south;}

 private:

  typedef std::vector<TMpcExHit*> container;

  //! the internal container of the "good" hits determined by mMpcExApplyCalibrations
  container _good_hits;

  //! the full set of minipads so we only have to create TMpcExHit objects once
  container _all_hits;

  //! the number of "good" hits in the south arm
  int _hits_south; 

  //! the number of "good" hits in the north arm
  int _hits_north;

};

#endif /* __TMPCEXHITCONTAINER_H__ */
