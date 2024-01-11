#ifndef __EMC_GEATRACK_CONTAINER_VA_H__
#define __EMC_GEATRACK_CONTAINER_VA_H__





#include <emcContainerT.h>
#include <emcGeaTrackContainer.h>




class emcGeaTrackContainervA: public emcGeaTrackContainer, public emcContainerTNotObject<emcGeaTrackContent> {

public:
  typedef emcGeaTrackContainer::key_type key_type;
  //  typedef emcContainerT<emcGeaTrackContent>::value_type value_type;
  //  typedef emcContainerT<emcGeaTrackContent>::iterator iterator;
  //  typedef emcContainerT<emcGeaTrackContent>::const_iterator const_iterator;


public:
  emcGeaTrackContainervA(){ towers = NULL; clusters = NULL; }
  
  emcGeaTrackContainervA * create(void) const { return new emcGeaTrackContainervA; }
  //  virtual void identify(std::ostream& os=std::cout) const;
  virtual void Reset(){ clear(); }
  virtual int isValid() const { return 1; };
  // virtual void print(std::ostream& os=std::cout, int level=0) const;

  virtual TClass * gettype() const { return emcContainerTNotObject<emcGeaTrackContent>::gettype(); }
  virtual unsigned int capacity() const { return emcContainerTNotObject<emcGeaTrackContent>::capacity(); }
  virtual unsigned int size() const { return emcContainerTNotObject<emcGeaTrackContent>::size(); }
  virtual bool resize(unsigned int size){ return emcContainerTNotObject<emcGeaTrackContent>::resize(size); }
  virtual int add(emcGeaTrackContent const * d);
  virtual emcGeaTrackContent * find(key_type const & key) const { return emcContainerTNotObject<emcGeaTrackContent>::find(key); }
  virtual emcGeaTrackContent * get(unsigned int index) const { return emcContainerTNotObject<emcGeaTrackContent>::get(index); }
  virtual int teg(emcGeaTrackContent * const addr) const { return emcContainerTNotObject<emcGeaTrackContent>::teg(addr); }
  virtual void clear(){ emcContainerTNotObject<emcGeaTrackContent>::clear(); invcache(); }

  emcGeaTowerContainer * GetTowers() const { return towers; }
  void SetTowers(emcGeaTowerContainer * towers);
  virtual emcGeaClusterContainer * GetClusters() const { return clusters; }
  virtual void SetClusters(emcGeaClusterContainer * table);


protected:
  emcGeaTowerContainer * towers; //!
  emcGeaClusterContainer * clusters; //!


  ClassDef(emcGeaTrackContainervA, 1)
};





#endif /* ! __EMC_GEATRACK_CONTAINER_VA_H__ */

