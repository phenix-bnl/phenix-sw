#ifndef __TMPCEXHITSET_H__
#define __TMPCEXHITSET_H__

/**
 * @class  TMpcExHitSet
 * @author purschke@bnl.gov
 * @date   July 2015
 * @brief  On-demand sorting of TMpcExHit*.
 *
 *         This class provides a generic access to the in-memory
 *         TMpcExHit objects. Internally a std::set of the hit 
 *         pointers can be iterated over.
 *
 *         The hits are ordered in the set by the template 
 *         parameter that is a class that inherits from
 *         std::binary_function<TMpcExHit*,TMpcExHit*,bool>. A
 *         set of standard sorting classes are given in 
 *         the file TMpcExHitSort.h and are contained within the 
 *         namespace TMpcExHitSort. You are, of course, free to 
 *         provide your own. If the template parameter is empty 
 *         the default is to sort by key.
 *
 *         The get_iterator() method builds the set on demand and
 *         returns the const_iterator to the beginning of the 
 *         set. This is a STL iterator and can be treated as such.
 *         Dereferencing the iterator is the TMpcExHit pointer in
 *         the set. The pointers in the set are owned by this 
 *         class. You should not save the pointer as it isn't 
 *         guaranteed to be in scope at all times.
 */

#include <set>
#include "MpcExRawHit.h"
#include "TMpcExHit.h"
#include "TMpcExHitContainer.h"
#include "MpcExMapper.h"
#include "TMpcExHitSort.h"
#include <functional>

template <class T = TMpcExHitSort::SortByKey> 
class TMpcExHitSet {

 public:

  typedef typename std::set<TMpcExHit*,T> container;

  typedef typename container::const_iterator const_iterator;

  //! Construct the set from the compact prdf hits
  TMpcExHitSet<T>(const MpcExRawHit* raw): _raw_hits(raw), _calib_hits(NULL), _done(false), _hit_set() {}

  //! Construct the set from the in-memory hits
  TMpcExHitSet(const TMpcExHitContainer* hits): _raw_hits(NULL), _calib_hits(hits), _done(false), _hit_set() {}

  //! Destructor -- delete all pointers and clear the set
  virtual ~TMpcExHitSet();

  //! Build the hits on the fly from the input container given 
  //! in the constructor. Return a const_iterator to the 
  //! beginning of the set
  virtual const_iterator  get_iterator();

  //! Return a const_iterator to the end of the internal set
  virtual const_iterator  end() const;

 protected:

  //! Build the internal set from the compact PRDF hits
  void fill_from_raw_hits();

  //! Build the internal set from the in-memory hits
  void fill_from_calib_hits();

  //! The compact PRDF hit container
  const MpcExRawHit* _raw_hits;

  //! The in-memory hit container
  const TMpcExHitContainer* _calib_hits;

  //! The check to see if the internal set has been constructed
  bool _done;

  //! The internal on-demand set built based on the given templated parameter ordering class
  container _hit_set;
};

template<class T>
TMpcExHitSet<T>::~TMpcExHitSet<T>(){
  while(!_hit_set.empty()){
    delete *(_hit_set.begin());
    _hit_set.erase(_hit_set.begin());
  }
}

template <class T> 
typename TMpcExHitSet<T>::const_iterator TMpcExHitSet<T>::get_iterator() 
{
  if ( !_done)
    {
      // ok, now we are asked. we now do the hard work.
      if(_raw_hits != NULL && _calib_hits == NULL){
	fill_from_raw_hits();
      } else {
	fill_from_calib_hits();
      }
      _done = true;
    }
  return _hit_set.begin();
}

template <class T> 
typename TMpcExHitSet<T>::const_iterator TMpcExHitSet<T>::end() const
{
  return _hit_set.end();
}

template<class T>
void TMpcExHitSet<T>::fill_from_raw_hits()
{
  for ( unsigned int i = 0; i < _raw_hits->getnhits() ; i++)
    {
      TMpcExHit *hit = new TMpcExHit(_raw_hits->getOnlineKey(i));
      hit->set_low(_raw_hits->getladc(i));
      hit->set_high(_raw_hits->gethadc(i));
      hit->set_state_low(TMpcExHit::ADC);
      hit->set_state_high(TMpcExHit::ADC);
      _hit_set.insert(hit);
    }
}

template<class T>
void TMpcExHitSet<T>::fill_from_calib_hits()
{
  for ( unsigned int i = 0; i < _calib_hits->size() ; i++)
    {
      _hit_set.insert(_calib_hits->getHit(i)->clone());
    }
}

#endif /* __TMPCEXHITSET_H__ */
