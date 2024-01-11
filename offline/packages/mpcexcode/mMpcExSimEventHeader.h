#ifndef __MMPCEXSIMEVENTHEADER_H__
#define __MMPCEXSIMEVENTHEADER_H__

/**
 * @class  mMpcExSimEventHeader
 * @author ngrau@augie.edu
 * @date   August 2015
 * @brief  This class creates a "dummy" MpcExEventHeader for simulated data.
 */

#include "SubsysReco.h"
class PHCompositeNode;

class mMpcExSimEventHeader : public SubsysReco {

 public:
  mMpcExSimEventHeader();

  virtual ~mMpcExSimEventHeader();

  int process_event(PHCompositeNode *topNode);

};

#endif /* __MMPCEXSIMEVENTHEADER_H__ */
