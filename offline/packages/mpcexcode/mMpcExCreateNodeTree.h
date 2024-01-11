#ifndef __MMPCEXCREATENODETREE_H__
#define __MMPCEXCREATENODETREE_H__

#include "SubsysReco.h"

class mMpcExCreateNodeTree : public SubsysReco {

 public:

  //! constructor
  mMpcExCreateNodeTree();

  //! destructor
  virtual ~mMpcExCreateNodeTree();

  //! create the in-memory nodes on the node tree
  int InitRun(PHCompositeNode *topNode);

 private:

};

#endif /* __MMPCEXCREATENODETREE_H__ */
