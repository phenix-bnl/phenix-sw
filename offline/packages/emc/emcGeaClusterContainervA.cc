//////////////////////////////////////////////////////////////////////////////////
//
// implements the emcGeaClusterContainer interface using the emcContainerT
// class.
//
//////////////////////////////////////////////////////////////////////////////////

#include <emcGeaTrackContainer.h>
#include <emcGeaTowerContainer.h>

#include <emcGeaClusterContainervA.h>



template class emcContainerTNotObject< emcGeaClusterContent >;
ClassImp( emcGeaClusterContainervA );







int emcGeaClusterContainervA::add(emcGeaClusterContent const * d ){
  int rc = emcContainerTNotObject<emcGeaClusterContent>::add( d );
  if( rc == -1 ) return -1;

  if( GetTracks() ) GetTracks()->invcache();
  if( GetTowers() ) GetTowers()->invcache();
  this->invcache(); // this will clear the cache of the object being added, too

  return rc;
}






void emcGeaClusterContainervA::SetTowers(emcGeaTowerContainer * towers){
  assert( this->towers == NULL || this->towers == towers ); // todo: better error handling
  this->towers = towers;
}



void emcGeaClusterContainervA::SetTracks(emcGeaTrackContainer * tracks){
  assert( this->tracks == NULL || this->tracks == tracks ); // todo: better error handling
  this->tracks = tracks;
}


