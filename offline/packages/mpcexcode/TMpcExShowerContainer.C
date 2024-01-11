#include "TMpcExShowerContainer.h"
#include "MpcExConstants.h"

void TMpcExShowerContainer::Reset() {
  container::iterator itr = _showers.begin();
  container::iterator last = _showers.end();
  for(; itr!=last; ++itr)
    delete *itr;
  _showers.clear();
}

void TMpcExShowerContainer::Clean() {
  for(container::iterator itr = _showers.begin(); itr!= _showers.end(); ){
    // Delete showers that have been merged into other showers
    TMpcExShower *shower_v1 = *itr; 
    if( shower_v1->get_delete_me() ){
      delete *itr;
      itr = _showers.erase(itr);
    }
    else
      ++itr;
  }
}
