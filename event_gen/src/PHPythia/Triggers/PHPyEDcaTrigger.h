#ifndef __PHPYEDCATRIGGER_H__
#define __PHPYEDCATRIGGER_H__

#include <PHPyTrigger.h>

class PHPyEDcaTrigger: public PHPyTrigger
{
 public:  
  //! constructor
  PHPyEDcaTrigger(const std::string& name="PHPyEDcaTrigger");
  
  //! destructor 
  virtual ~PHPyEDcaTrigger(void) {}
  
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode); 
  //@}
  
 protected:
  //! true if e is found in final state that enters the central arm
  bool EfromHeavy(PHPythiaContainer *phpylist);
};

#endif	
