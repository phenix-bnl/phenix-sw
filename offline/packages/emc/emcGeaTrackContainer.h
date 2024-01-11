#ifndef __EMC_GEATRACK_CONTAINER_H__
#define __EMC_GEATRACK_CONTAINER_H__




#include <phool.h>
#include <PHObject.h>

#include <emcGeaTrackContent.h>


class emcGeaTowerContainer;
class emcGeaClusterContainer;



class emcGeaTrackContainer: public PHObject {
  //public:
  //  typedef emcContainerT<emcGeaTrackContent>::key_type key_type;
  //  typedef emcContainerT<emcGeaTrackContent>::value_type value_type;
  //  typedef emcContainerT<emcGeaTrackContent>::iterator iterator;
  //  typedef emcContainerT<emcGeaTrackContent>::const_iterator const_iterator;


protected: /* PHObject+ interface */
  emcGeaTrackContainer(){}

public:
  virtual ~emcGeaTrackContainer(){}
  virtual emcGeaTrackContainer* clone(void) const;
  virtual emcGeaTrackContainer* create(void) const { PHOOL_VIRTUAL_WARNING; return 0; }
  static emcGeaTrackContainer * createdef(void);
  virtual void identify(std::ostream& os=std::cout) const;
  //virtual void Reset(){ clear(); }
  //  virtual int isValid() const { return 1; };
  //virtual void print(std::ostream& os=std::cout, int level=0) const;

  

public: /* emcContainer interface, use this interface */
  typedef /*typename*/ emcGeaTrackContent::key_type key_type; ///< key type of stored objects

  virtual void copy(emcGeaTrackContainer const * from);
  virtual TClass * gettype() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual unsigned int capacity() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual unsigned int size() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual bool resize(unsigned int size){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual int add(emcGeaTrackContent const * d){ PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaTrackContent * find(key_type const & key) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual emcGeaTrackContent * get(unsigned int index) const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual int teg(emcGeaTrackContent * const addr) const { PHOOL_VIRTUAL_WARNING; return -1; }
  virtual void clear(){ PHOOL_VIRTUAL_WARNING; }

  virtual emcGeaTowerContainer * GetTowers() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetTowers(emcGeaTowerContainer * towers){ PHOOL_VIRTUAL_WARNING; }
  virtual emcGeaClusterContainer * GetClusters() const { PHOOL_VIRTUAL_WARNING; return 0; }
  virtual void SetClusters(emcGeaClusterContainer * clusters){ PHOOL_VIRTUAL_WARNING; }

  // instructs objects stored in this container to discard cached data of a 
  // given type, because the datasoruces have changed.
  virtual void invcache(emcGeaDepositHolder::cachetype_t type = emcGeaDepositHolder::CACHED_ALL);


public:
  emcGeaTrackContent * get_common_parent(emc_trkno_t trkno1, emc_trkno_t trkno2);
  emcGeaTrackContent * get_common_parent(emcGeaTrackContent * trk1, emcGeaTrackContent * trk2);

  

  ClassDef(emcGeaTrackContainer, 1)
};





#endif /* ! __EMC_GEATRACK_CONTAINER_H__ */

