//////////////////////////////////////////////////////////////////////////////////
// 
// class that holds emcGeaTowerContent s. this class also inherits from
// the emcTowerContainer class, so analysis macros written to 
// process real data can process simulated (and embedded) data without
// modifications.
//
//////////////////////////////////////////////////////////////////////////////////


#include <cassert>

#include <phool.h>

#include <emcGeaTowerContent.h>
#include <emcGeaTowerContainer.h>
#include <emcGeaTowerContainervA.h>




emcGeaTowerContainer * emcGeaTowerContainer::createdef(void){
  return new emcGeaTowerContainervA;
}



emcGeaTowerContainer* emcGeaTowerContainer::clone(void) const {
  emcGeaTowerContainer * ret = create();
  ret->copy(this);
  return ret;
}



void emcGeaTowerContainer::identify(std::ostream& os) const {
  std::streambuf *outbuf = std::cout.rdbuf(os.rdbuf());
  Dump();
  std::cout.rdbuf(outbuf);
}





void emcGeaTowerContainer::copy(emcGeaTowerContainer const * from){
  Reset();
  for(unsigned int i = 0; i < from->size(); i++){
    emcGeaTowerContent * clone = from->get(i)->clone();
    assert( add(clone) != -1 );
  }
  SetTracks( from->GetTracks() );
  SetClusters( from->GetClusters() );
}



void emcGeaTowerContainer::invcache(emcGeaDepositHolder::cachetype_t type){
  for(size_t i = 0; i < size(); i++)
    get(i)->invcache(type);
}



emcGeaTowerContent* emcGeaTowerContainer::addTower(unsigned int index){

  emcGeaTowerContent * tmp = find(index);
  if( find(index) != NULL ) return tmp; // already in table

  tmp = emcGeaTowerContent::createdef(index);

  int rc = add( tmp );
  if( rc == -1 ){ delete tmp; tmp = NULL; }
  else tmp = get(rc);

  return tmp;
}



emcGeaTowerContent* emcGeaTowerContainer::addTower(unsigned int index, const emcTowerContent & c){
  
  emcGeaTowerContent * tmp = addTower(index); assert( tmp != NULL ); // TODO: better error handling
  tmp->Copy(c);

  return tmp;
}



emcGeaTowerContent* emcGeaTowerContainer::addTower(const emcTowerContent &c){
  return addTower( c.towerid(), c );
}



