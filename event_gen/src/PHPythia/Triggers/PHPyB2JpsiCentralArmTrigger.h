#ifndef __PHPYB2JPSICENTRALARMRIGGER_H__
#define __PHPYB2JPSICENTRALARMRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyB2JpsiCentralArmTrigger.h
   \brief trigger module to select events that contains a B->J/psi->e+e- in the Central Arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2010/01/13 16:41:48 $
*/

 class PHAcceptParticleCentralArm;

//!  trigger module to select events that contains an electron from B in the central arm acceptance
class PHPyB2JpsiCentralArmTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyB2JpsiCentralArmTrigger( const std::string& name = "PHPyB2JpsiCentralArmTrigger" );
  
  //! destructor 
  virtual ~PHPyB2JpsiCentralArmTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e+e- pair from B->Jpsi is found in final state that enters the central arm
  bool B2JpsiInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

  double min_pT;
};

#endif	
