#ifndef __EMC_CONTENT_T_H__
#define __EMC_CONTENT_T_H__





#include <Rtypes.h>



template<class T> class emcContainerTNotObject;


template<class T> class emcContentT {
public:
  //  typedef key key_type; ///< key type of stored objects


public:
  emcContentT(){ container = NULL; }
  virtual ~emcContentT(){ container = NULL; }

  //  virtual key_type get_key() const = 0;


#ifndef __CINT__   // fuck cint..

  emcContainerTNotObject<T> * get_container() const { return (emcContainerTNotObject<T> *)container; }
  void set_container(emcContainerTNotObject<T> * container){ this->container = container; }
  //emcContainerTNotObject<T> * container;

#endif

  void * container; //!



  ClassDef(emcContentT, 1)
};


templateClassImp(emcContentT);












#endif /* ! __EMC_CONTENT_T_H__ */

