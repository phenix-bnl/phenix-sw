#ifndef __PHPYD02KPITRIGGER_H__
#define __PHPYD02KPITRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyD02KpiTrigger.h
   \brief trigger module to select events that contains a D0 -> K-pi+ in the Central Arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2010/03/12 15:32:35 $
*/

 class PHAcceptParticleCentralArm;

//!  trigger module to select events that contains an electron from B in the central arm acceptance
class PHPyD02KpiTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyD02KpiTrigger( const std::string& name = "PHPyD02KpiTrigger" );
  
  //! destructor 
  virtual ~PHPyD02KpiTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e+e- pair from B->Jpsi is found in final state that enters the central arm
  bool D02KpiInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

  double min_pT;
};

#endif	
