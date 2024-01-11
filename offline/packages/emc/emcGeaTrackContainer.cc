//////////////////////////////////////////////////////////////////////////////////
//
// container for emcGeaTrackContent objects.
//
//////////////////////////////////////////////////////////////////////////////////

#include <cassert>

#include <list>

#include <emcGeaTrackContent.h>
#include <emcGeaTowerContainer.h>
#include <emcGeaTrackContainer.h>
#include <emcGeaTrackContainervA.h>



ClassImp( emcGeaTrackContainer );





emcGeaTrackContainer * emcGeaTrackContainer::createdef(void){
  return new emcGeaTrackContainervA;
}



emcGeaTrackContainer* emcGeaTrackContainer::clone(void) const {
  emcGeaTrackContainer * ret = create();
  ret->copy(this);
  return ret;
}



void emcGeaTrackContainer::identify(std::ostream& os) const {
  std::streambuf *outbuf = std::cout.rdbuf(os.rdbuf());
  Dump();
  std::cout.rdbuf(outbuf);
}



void emcGeaTrackContainer::copy(emcGeaTrackContainer const * from){
  Reset();
  for(unsigned int i = 0; i < from->size(); i++)
    add( from->get(i)->clone() ); // TODO: add might fail!
  SetTowers( from->GetTowers() );
  SetClusters( from->GetClusters() );
}



void emcGeaTrackContainer::invcache(emcGeaDepositHolder::cachetype_t type){
  for(size_t i = 0; i < size(); i++)
    get(i)->invcache(type);
}



emcGeaTrackContent * emcGeaTrackContainer::get_common_parent(emc_trkno_t trkno1, emc_trkno_t trkno2){
  emcGeaTrackContent * trk1 = find( trkno1 );
  emcGeaTrackContent * trk2 = find( trkno2 );

  return get_common_parent(trk1, trk2);
}



emcGeaTrackContent * emcGeaTrackContainer::get_common_parent(emcGeaTrackContent * trk1, emcGeaTrackContent * trk2){
  if( trk1 == NULL  ||  trk2 == NULL ) return NULL;

  assert( trk1 == find(trk1->get_key()) );
  assert( trk2 == find(trk2->get_key()) );


  std::list<emcGeaTrackContent *> trk1list;
  for(;;){
    trk1list.push_back( trk1 );
    if( trk1->get_parent_trkno() == 0 ) break;
    trk1 = find( trk1->get_parent_trkno() );
  };


  std::list<emcGeaTrackContent *> trk2list;
  for(;;){
    trk2list.push_back( trk2 );
    if( trk2->get_parent_trkno() == 0 ) break;
    trk2 = find( trk2->get_parent_trkno() );
  };


  std::list<emcGeaTrackContent *>::reverse_iterator i1 = trk1list.rbegin();
  std::list<emcGeaTrackContent *>::reverse_iterator i2 = trk2list.rbegin();

  // their grand-grand{-grand}* parents are not the same
  if( *i1 != *i2 ) return NULL;


  // find their youngest common parent
  for(;;){
    emcGeaTrackContent * saved = *i1;

    if( ++i1 == trk1list.rend() ) return saved;
    if( ++i2 == trk2list.rend() ) return saved;
    if( *i1 != *i2 ) return saved; 
  }


  return NULL;
}
