#include "TMpcExGeaHitContainer.h"

ClassImp(TMpcExGeaHitContainer)

void TMpcExGeaHitContainer::addHit(TMpcExGeaHit* hit){
  _hits.push_back(hit);
}

TMpcExGeaHit* TMpcExGeaHitContainer::getHit(const size_t index) const {
  if(index<size()){
    return _hits[index];
  }
  return NULL;
}

void TMpcExGeaHitContainer::Reset() {
  container::iterator itr = _hits.begin();
  container::iterator last = _hits.end();
  for(; itr!=last; ++itr){
    delete *itr;
  }
  _hits.clear();
}

TMpcExGeaHit* TMpcExGeaHitContainer::get_hit_by_key(unsigned int key) const {  
  container::const_iterator itr = _hits.begin();
  container::const_iterator last = _hits.end();
  for(; itr!=last; ++itr){
    if((*itr)->key() == key)
      return *itr;
  }
  return NULL;
}
