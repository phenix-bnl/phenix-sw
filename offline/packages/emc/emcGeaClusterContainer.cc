//////////////////////////////////////////////////////////////////////////////////
// 
// class that holds emcGeaClusterContent s. this class also inherits from
// the emcClusterContainer class, so analysis macros written to 
// process real data can process simulated (and embedded) data without
// modifications.
//
//////////////////////////////////////////////////////////////////////////////////

#include <phool.h>

#include <emcGeaClusterContent.h>
#include <emcGeaClusterContainer.h>
#include <emcGeaClusterContainervA.h>




emcGeaClusterContainer * emcGeaClusterContainer::createdef(void){
  return new emcGeaClusterContainervA;
}



emcGeaClusterContainer* emcGeaClusterContainer::clone(void) const {
  emcGeaClusterContainer * ret = create();
  ret->copy(this);
  return ret;
}



void emcGeaClusterContainer::identify(std::ostream& os) const {
  std::streambuf *outbuf = std::cout.rdbuf(os.rdbuf());
  Dump();
  std::cout.rdbuf(outbuf);
}



void emcGeaClusterContainer::copy(emcGeaClusterContainer const * from){
  clear();
  for(unsigned int i = 0; i < from->size(); i++)
    add( from->get(i)->clone() ); /// TODO: add might fail!
  SetTracks( from->GetTracks() );
  SetTowers( from->GetTowers() );
}



void emcGeaClusterContainer::invcache(emcGeaDepositHolder::cachetype_t type){
  for(size_t i = 0; i < size(); i++)
    get(i)->invcache(type);
}



emcGeaClusterContent* emcGeaClusterContainer::addCluster(unsigned int index){

  emcGeaClusterContent * tmp = find(index);
  if( find(index) != NULL ) return tmp; // already in table

  tmp = emcGeaClusterContent::createdef(index);

  int rc = add( tmp );
  if( rc == -1 ){ delete tmp; tmp = NULL; }
  else tmp = get(rc);

  return tmp;
}



emcGeaClusterContent* emcGeaClusterContainer::addCluster(unsigned int index, const emcClusterContent & c){
  
  emcGeaClusterContent * tmp = addCluster(index); assert( tmp != NULL ); /// TODO: better error handling
  tmp->Copy(c);

  return tmp;
}



emcGeaClusterContent* emcGeaClusterContainer::addCluster(const emcClusterContent &c){
  return addCluster( c.id(), c );
}

