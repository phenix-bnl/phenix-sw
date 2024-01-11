//////////////////////////////////////////////////////////////////////////////////
//
// implements the emcGeaTowerContainer interface using the emcContainerT
// class.
//
//////////////////////////////////////////////////////////////////////////////////

#include <emcGeaTrackContent.h>
#include <emcGeaTrackContainer.h>
#include <emcGeaClusterContainer.h>

#include <emcGeaTowerContainervA.h>



template class emcContainerTNotObject< emcGeaTowerContent >;					\
ClassImp( emcGeaTowerContainervA );





int emcGeaTowerContainervA::add(emcGeaTowerContent const * d ){
  int rc = emcContainerTNotObject<emcGeaTowerContent>::add( d );
  if( rc == -1 ) return -1;

  if( GetTracks() ) GetTracks()->invcache();
  this->invcache(); // this will clear the cache of the object being added, too
  if( GetClusters() ) GetClusters()->invcache();

  return rc;
}







void emcGeaTowerContainervA::SetTracks(emcGeaTrackContainer * tracks){
  assert( this->tracks == NULL || this->tracks == tracks ); // todo: better error handling
  this->tracks = tracks;
}



void emcGeaTowerContainervA::SetClusters(emcGeaClusterContainer * clusters){
  assert( this->clusters == NULL || this->clusters == clusters ); // todo: better error handling
  this->clusters = clusters;
}


