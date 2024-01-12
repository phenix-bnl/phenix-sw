#ifndef __PHPYEFROMCINCENTRALARMRIGGER_H__
#define __PHPYEFROMCINCENTRALARMRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyEfromCInCentralArmTrigger.h
   \brief trigger module to select events that contain an electron from open charm in the Central Arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2012/08/31 14:58:24 $
*/

 class PHAcceptParticleCentralArm;

//!  trigger module to select events that contains an electron from open charm in the central arm acceptance
class PHPyEfromCInCentralArmTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyEfromCInCentralArmTrigger( const std::string& name = "PHPyEfromCInCentralArmTrigger" );
  
  //! destructor 
  virtual ~PHPyEfromCInCentralArmTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters the central arm
  bool EfromCInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

};

#endif	
