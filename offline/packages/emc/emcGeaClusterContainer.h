#ifndef __EMC_GEACLUSTER_CONTAINER_H__
#define __EMC_GEACLUSTER_CONTAINER_H__





#include <phool.h>

#include <emcClusterContainer.h>
#include <emcGeaClusterContent.h>



class emcGeaTrackContainer;
class emcGeaTowerContainer;



class emcGeaClusterContainer : public emcClusterContainer {
  
protected: /* PHObject+ interface: some of them can be resonably implemented even on abstract level */
  emcGeaClusterContainer(){}

public:
  virtual ~emcGeaClusterContainer(){}
  virtual emcGeaClusterContainer* clone(void) const;
  virtual emcGeaClusterContainer* create(void) const { PHOOL_VIRTUAL_WARNING; return 0; }
  static emcGeaClusterContainer * createdef(void);
  virtual void identify(std::ostream& os=std::cout) const;
  //virtual void Reset(){ clear(); }
  //virtual int isValid() const { return 1; };
  //virtual void print(std::ostream& os=std::cout, int level=0) const;



  
public: /* emcContainer interface, use this interface */
  typedef /*typename*/ emcGeaClusterContent::key_type key_type; // key type of stored objects

  virtual void copy(emcGeaClusterContainer const * from);
  virtual TClass * gettype() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual unsigned int capacity() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual unsigned int size() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual bool resize(unsigned int size){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual int add(emcGeaClusterContent const * d){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaClusterContent * find(key_type const & key) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaClusterContent * get(unsigned int index) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual int teg(emcGeaClusterContent * const addr) const { PHOOL_VIRTUAL_WARNING; return -1; }
  virtual void clear(){ PHOOL_VIRTUAL_WARNING; }

  virtual emcGeaTrackContainer * GetTracks() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetTracks(emcGeaTrackContainer * table){ PHOOL_VIRTUAL_WARNING; }
  virtual emcGeaTowerContainer * GetTowers() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetTowers(emcGeaTowerContainer * towers){ PHOOL_VIRTUAL_WARNING; }

  // instructs objects stored in this container to discard cached data of a 
  // given type, because the datasoruces have changed.
  virtual void invcache(emcGeaDepositHolder::cachetype_t type = emcGeaDepositHolder::CACHED_ALL);


public:  /* emcClusterContainer interface. */ 
  virtual emcGeaClusterContent* addCluster(unsigned int index);
  virtual emcGeaClusterContent* addCluster(unsigned int index, const emcClusterContent & c);
  virtual emcGeaClusterContent* addCluster(const emcClusterContent &c);
  virtual emcGeaClusterContent* findCluster(int clusterID) const { return find(clusterID); }
  virtual emcGeaClusterContent* getCluster(unsigned int i) const { return get(i); }
  virtual bool removeCluster(unsigned int i){ PHOOL_VIRTUAL_WARNING; return false; }



  ClassDef(emcGeaClusterContainer, 1)
};





#endif /* ! __EMC_GEACLUSTER_CONTAINER_H__ */

