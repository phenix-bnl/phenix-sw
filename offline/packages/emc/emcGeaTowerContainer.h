#ifndef __EMC_GEATOWER_CONTAINER_H__
#define __EMC_GEATOWER_CONTAINER_H__





#include <phool.h>

#include <emcTowerContainer.h>
#include <emcGeaTowerContent.h>



class emcGeaTrackContainer;
class emcGeaClusterContainer;



class emcGeaTowerContainer : public emcTowerContainer {
  
protected:  /* PHObject+ interface: some of them can be resonably implemented even on abstract level */ 
  emcGeaTowerContainer(){}

public:
  virtual ~emcGeaTowerContainer(){}
  virtual emcGeaTowerContainer* clone(void) const;
  virtual emcGeaTowerContainer* create(void) const { PHOOL_VIRTUAL_WARNING; return 0; }
  static emcGeaTowerContainer * createdef(void);
  virtual void identify(std::ostream& os=std::cout) const;
  //  virtual void Reset(){ clear(); }
  //  virtual int isValid() const { return 1; };
  //virtual void print(std::ostream& os=std::cout, int level=0) const;



  
public: // emcContainer interface, use this interface
  typedef /*typename*/ emcGeaTowerContent::key_type key_type; ///< key type of stored objects

  virtual void copy(emcGeaTowerContainer const * from);
  virtual TClass * gettype() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual unsigned int capacity() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual unsigned int size() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual bool resize(unsigned int size){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual int add(emcGeaTowerContent const * d){ PHOOL_VIRTUAL_WARNING; return 0; }
  //virtual int add(emcGeaTowerContent const & d){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaTowerContent * find(key_type const & key) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaTowerContent * get(unsigned int index) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual int teg(emcGeaTowerContent * const addr) const { PHOOL_VIRTUAL_WARNING; return -1; }
  virtual void clear(){ PHOOL_VIRTUAL_WARNING; }

  virtual emcGeaTrackContainer * GetTracks() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetTracks(emcGeaTrackContainer * table){ PHOOL_VIRTUAL_WARNING; }
  virtual emcGeaClusterContainer * GetClusters() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetClusters(emcGeaClusterContainer * table){ PHOOL_VIRTUAL_WARNING; }

  // instructs objects stored in this container to discard cached data of a 
  // given type, because the datasoruces have changed.
  virtual void invcache(emcGeaDepositHolder::cachetype_t type = emcGeaDepositHolder::CACHED_ALL);



public: /* emcTowerContainer interface */
  virtual emcGeaTowerContent* addTower(unsigned int index);
  virtual emcGeaTowerContent* addTower(unsigned int index, const emcTowerContent & c);
  virtual emcGeaTowerContent* addTower(const emcTowerContent &c);
  virtual emcGeaTowerContent* findTower(int clusterID) const { return find(clusterID); }
  virtual emcGeaTowerContent* getTower(unsigned int i) const { return get(i); }
  virtual bool removeTower(unsigned int i){ PHOOL_VIRTUAL_WARNING; return false; }



  ClassDef(emcGeaTowerContainer, 1)
};





#endif /* ! __EMC_GEATOWER_CONTAINER_H__ */

