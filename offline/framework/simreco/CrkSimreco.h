#ifndef __CRKSIMRECO_H__
#define __CRKSIMRECO_H__

#include "SubsysReco.h"

class PHCompositeNode;

#include "CrkDAO.h"
#include "CrkSimuRawReCal.h"

class CrkSimreco: public SubsysReco
{
  public:
  
  //! constructor
  CrkSimreco(const std::string &name = "CRK");
  
  //! destructor
  virtual ~CrkSimreco();

  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int ResetEvent(PHCompositeNode *topNode);

 protected:
  int copyWrapper(PHCompositeNode *);
  
  //!@name modules
  /*! 
  direct reference of the objects is used in place
  of pointer so that they are created in parent object constructor
  and deleted in parent object destructor
  */
  //@{
  CrkDAO crkdao;
  CrkSimuRawReCal mCrkSimuRawReCal;
  //@}

};

#endif /* __CRKSIMRECO_H__ */
