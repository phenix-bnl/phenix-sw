#ifndef __PHPYETRIGGER_H__
#define __PHPYETRIGGER_H__

#include <PHPyTrigger.h>

class PHPyETrigger: public PHPyTrigger
{
 public:  
  //! constructor
  PHPyETrigger(const std::string& name="PHPyETrigger");
  
  //! destructor 
  virtual ~PHPyETrigger(void) {}
  
  int Init(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);
  int End(PHCompositeNode *topNode); 
  //@}
  
 protected:
  //! true if e is found in final state that enters the central arm
  bool EfromHeavy(PHPythiaContainer *phpylist);
};

#endif	
