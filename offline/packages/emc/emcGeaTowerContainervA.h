#ifndef __EMC_GEATOWER_CONTAINER_VA_H__
#define __EMC_GEATOWER_CONTAINER_VA_H__





#include <emcContainerT.h>
#include <emcGeaTowerContainer.h>





class emcGeaTowerContainervA: public emcGeaTowerContainer, public emcContainerTNotObject< emcGeaTowerContent > {

public:
  typedef emcGeaTowerContainer::key_type key_type;
  //  typedef EmcTableHashNotObject< EmcGeaCluster >::value_type value_type;
  //  typedef EmcTableHashNotObject< EmcGeaCluster >::iterator iterator;
  //  typedef EmcTableHashNotObject< EmcGeaCluster >::const_iterator const_iterator;



public:
  emcGeaTowerContainervA(){ tracks = NULL; clusters = NULL; }

  emcGeaTowerContainervA * create(void) const { return new emcGeaTowerContainervA; }
  //  virtual void identify(std::ostream& os=std::cout) const;
  virtual void Reset(){ clear(); }
  virtual int isValid() const { return 1; };
  // virtual void print(std::ostream& os=std::cout, int level=0) const;

  virtual TClass * gettype() const { return emcContainerTNotObject<emcGeaTowerContent>::gettype(); }
  virtual unsigned int capacity() const { return emcContainerTNotObject<emcGeaTowerContent>::capacity(); }
  virtual unsigned int size() const { return emcContainerTNotObject<emcGeaTowerContent>::size(); }
  virtual bool resize(unsigned int size){ return emcContainerTNotObject<emcGeaTowerContent>::resize(size); }
  virtual int add(emcGeaTowerContent const * d);//{ return emcContainerTNotObject<emcGeaTowerContent>::add(d); }
  virtual emcGeaTowerContent * find(key_type const & key) const { return emcContainerTNotObject<emcGeaTowerContent>::find(key); }
  virtual emcGeaTowerContent * get(unsigned int index) const { return emcContainerTNotObject<emcGeaTowerContent>::get(index); }
  virtual int teg(emcGeaTowerContent * const addr) const { return emcContainerTNotObject<emcGeaTowerContent>::teg(addr); }
  virtual void clear(){ emcContainerTNotObject<emcGeaTowerContent>::clear(); invcache(); }

  emcGeaTrackContainer * GetTracks() const { return tracks; }
  void SetTracks(emcGeaTrackContainer * tracks);
  virtual emcGeaClusterContainer * GetClusters() const { return clusters; }
  virtual void SetClusters(emcGeaClusterContainer * table);


protected:
  emcGeaTrackContainer * tracks; //! link to container holding tracks
  emcGeaClusterContainer * clusters; //! link to container holding clusters


  ClassDef(emcGeaTowerContainervA, 1)
};





#endif /* ! __EMC_GEATOWER_CONTAINER_VA_H__ */

