#ifndef __PHPYEFROMBINCENTRALARMRIGGER_H__
#define __PHPYEFROMBINCENTRALARMRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyEfromBInCentralArmTrigger.h
   \brief trigger module to select events that contains a B->e in the Central Arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2010/01/12 20:48:35 $
*/

 class PHAcceptParticleCentralArm;

//!  trigger module to select events that contains an electron from B in the central arm acceptance
class PHPyEfromBInCentralArmTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyEfromBInCentralArmTrigger( const std::string& name = "PHPyEfromBInCentralArmTrigger" );
  
  //! destructor 
  virtual ~PHPyEfromBInCentralArmTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters the central arm
  bool EfromBInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

};

#endif	
