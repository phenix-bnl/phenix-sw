#include "TMpcExHitContainer.h"
#include "MpcExConstants.h"
#include "MpcExMapper.h"
#include <iostream>
#include <algorithm>

TMpcExHitContainer::TMpcExHitContainer() : _good_hits(), _hits_south(0),_hits_north(0) {
  for(unsigned int key = 0; key<MpcExConstants::NMINIPADS; key++){
    _all_hits.push_back(new TMpcExHit(key));
  }
}

TMpcExHitContainer::~TMpcExHitContainer(){
  _good_hits.clear();
  container::const_iterator itr = _all_hits.begin();
  container::const_iterator last = _all_hits.end();
  for(; itr!=last; ++itr){
    delete *itr;
  }
  _all_hits.clear();
}

TMpcExHit* TMpcExHitContainer::getUncalHit(unsigned int key) const {
  if(key < MpcExConstants::NMINIPADS)
    return _all_hits[key];
  return NULL;
}

void TMpcExHitContainer::addHit(TMpcExHit* hit){
  _good_hits.push_back(hit);
  if(hit->arm()==0)
    _hits_south++;
  else
    _hits_north++; 
}

TMpcExHit* TMpcExHitContainer::getHit(const size_t index) const {
  if(index<size()){
    return _good_hits[index];
  }
  return NULL;
}

void TMpcExHitContainer::Reset() {
  //clear all of the good hits
  _good_hits.clear();
  _hits_south = 0; 
  _hits_north = 0; 
  //reset all of the minipads
  container::const_iterator itr = _all_hits.begin();
  container::const_iterator last = _all_hits.end();
  for(; itr!=last; ++itr){
    (*itr)->Reset();
  }
}

TMpcExHit* TMpcExHitContainer::get_hit_by_key(unsigned int key) const {  
  container::const_iterator itr = _good_hits.begin();
  container::const_iterator last = _good_hits.end();
  for(; itr!=last; ++itr){
    if((*itr)->key() == key)
      return *itr;
  }
  return NULL;
}

void TMpcExHitContainer::erase(TMpcExHit* hit){
  container::iterator itr = std::find(_good_hits.begin(),_good_hits.end(),hit);
  if(itr!=_good_hits.end()){
    if( (*itr)->arm()==0)
      _hits_south--;
    else
      _hits_north--; 
    _good_hits.erase(itr);
  }
}
