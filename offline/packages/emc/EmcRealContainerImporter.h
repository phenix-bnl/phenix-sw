/*
 *
 */

#ifndef __EMC_REALCONTAINER_IMPORTER_H__
#define __EMC_REALCONTAINER_IMPORTER_H__





#include <Rtypes.h>

#include <SubsysRecoStack.h>
#include <PHCompositeNode.h>


class EmcRealContainerImporter: public SubsysRecoStack {
public:
  typedef enum { UNSET = 0, TOWER = 1, CLUSTER = 2 } srctype_t;


public:
  EmcRealContainerImporter(PHCompositeNode * root = NULL, srctype_t srctype = UNSET): 
    SubsysRecoStack("EmcRealContainerImporter", root){ 
    this->srctype = srctype; 
    this->switched = false;
  }

  virtual int InitRun(PHCompositeNode * root);
  virtual int ResetEvent(PHCompositeNode * root);
  virtual int process_event(PHCompositeNode * root);


protected:
  srctype_t srctype;
  bool switched;

  int process_event_cluster(PHCompositeNode * srcdst, PHCompositeNode * destdst);
  int process_event_tower(PHCompositeNode * srcdst, PHCompositeNode * destdst);


  ClassDef(EmcRealContainerImporter, 0)
};





#endif /* ! __EMC_REALCONTAINER_IMPORTER_H__ */

