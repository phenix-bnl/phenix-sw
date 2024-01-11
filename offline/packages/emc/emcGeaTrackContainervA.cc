//////////////////////////////////////////////////////////////////////////////////
//
// implements the emcGeaTrackContainer interface using the emcContainerT
// class.
//
//////////////////////////////////////////////////////////////////////////////////

#include <cassert>

#include <list>

#include <emcGeaTowerContainer.h>
#include <emcGeaClusterContainer.h>

#include <emcGeaTrackContainervA.h>



template class emcContainerTNotObject< emcGeaTrackContent >;
ClassImp( emcGeaTrackContainervA );







int emcGeaTrackContainervA::add(emcGeaTrackContent const * d ){
  int rc = emcContainerTNotObject<emcGeaTrackContent>::add( d );
  if( rc == -1 ) return -1;

  this->invcache(); // this will clear the cache of the object being added, too
  if( GetTowers() ) GetTowers()->invcache();
  if( GetClusters() ) GetClusters()->invcache();

  return rc;
}






void emcGeaTrackContainervA::SetTowers(emcGeaTowerContainer * towers){
  assert( this->towers == NULL || this->towers == towers ); // todo: better error handling
  this->towers = towers;
}



void emcGeaTrackContainervA::SetClusters(emcGeaClusterContainer * clusters){
  assert( this->clusters == NULL || this->clusters == clusters ); // todo: better error handling
  this->clusters = clusters;
}

