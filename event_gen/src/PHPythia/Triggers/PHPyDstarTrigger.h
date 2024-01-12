#ifndef __PHPYDSTARTRIGGER_H__
#define __PHPYDSTARTRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyDstarTrigger.h
   \brief trigger module to select events that contains a D*0 -> D0 -> K-pi+ 
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2013/06/06 19:33:44 $
*/

 class PHAcceptParticleCentralArm;

class PHPyDstarTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyDstarTrigger( const std::string& name = "PHPyDstarTrigger" );
  
  //! destructor 
  virtual ~PHPyDstarTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  bool DstarInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

  double min_pT;
};

#endif	
