#ifndef __PHPYCHIC2JPSICENTRALARMRIGGER_H__
#define __PHPYCHIC2JPSICENTRALARMRIGGER_H__

#include "PHPyTrigger.h"


 class PHAcceptParticleCentralArm;

class PHPyChic2JpsiCentralArmTrigger: public PHPyTrigger
{
  
  public:
  
  PHPyChic2JpsiCentralArmTrigger( const std::string& name = "PHPyChic2JpsiCentralArmTrigger" );
  
  virtual ~PHPyChic2JpsiCentralArmTrigger( void ) {}
  
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  
  protected:
  
  bool Chic2JpsiInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

  double min_pT;
};

#endif	
