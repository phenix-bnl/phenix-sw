#ifndef __EMC_GEACLUSTER_CONTAINER_VA_H__
#define __EMC_GEACLUSTER_CONTAINER_VA_H__





#include <emcContainerT.h>
#include <emcGeaClusterContent.h>
#include <emcGeaClusterContainer.h>





class emcGeaClusterContainervA: public emcGeaClusterContainer, public emcContainerTNotObject< emcGeaClusterContent > {

public:
  typedef emcGeaClusterContainer::key_type key_type;
  //  typedef EmcTableHashNotObject< EmcGeaCluster >::value_type value_type;
  //  typedef EmcTableHashNotObject< EmcGeaCluster >::iterator iterator;
  //  typedef EmcTableHashNotObject< EmcGeaCluster >::const_iterator const_iterator;



public:
  emcGeaClusterContainervA(){ tracks = NULL; towers = NULL; }

  emcGeaClusterContainervA * create(void) const { return new emcGeaClusterContainervA; }
  //  virtual void identify(std::ostream& os=std::cout) const;
  virtual void Reset(){ clear(); }
  virtual int isValid() const { return 1; };
  // virtual void print(std::ostream& os=std::cout, int level=0) const;

  virtual TClass * gettype() const { return emcContainerTNotObject<emcGeaClusterContent>::gettype(); }
  virtual unsigned int capacity() const { return emcContainerTNotObject<emcGeaClusterContent>::capacity(); }
  virtual unsigned int size() const { return emcContainerTNotObject<emcGeaClusterContent>::size(); }
  virtual bool resize(unsigned int size){ return emcContainerTNotObject<emcGeaClusterContent>::resize(size); }
  virtual int add(emcGeaClusterContent const * d);
  virtual emcGeaClusterContent * find(key_type const & key) const { return emcContainerTNotObject<emcGeaClusterContent>::find(key); }
  virtual emcGeaClusterContent * get(unsigned int index) const { return emcContainerTNotObject<emcGeaClusterContent>::get(index); }
  virtual int teg(emcGeaClusterContent * const addr) const { return emcContainerTNotObject<emcGeaClusterContent>::teg(addr); }
  virtual void clear(){ emcContainerTNotObject<emcGeaClusterContent>::clear(); invcache(); }
  
  virtual emcGeaTrackContainer * GetTracks() const { return tracks; }
  virtual void SetTracks(emcGeaTrackContainer * table);
  emcGeaTowerContainer * GetTowers() const { return towers; }
  void SetTowers(emcGeaTowerContainer * towers);


protected:
  emcGeaTrackContainer * tracks; //! link to container holding tracks
  emcGeaTowerContainer * towers; //! link to container holding towers


  ClassDef(emcGeaClusterContainervA, 1)
};





#endif /* ! __EMC_GEACLUSTER_CONTAINER_VA_H__ */

