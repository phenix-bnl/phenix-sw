#ifndef __PHPYDBCORRTRIGGER_H__
#define __PHPYDBCORRTRIGGER_H__

#include "PHPyTrigger.h"

/*!
   \file PHPyDBCorrTrigger.h
   \brief trigger module to select events that contains a D0 -> K-pi+ and
   \brief a B -> e in the Central Arm acceptance
   \author Sasha Lebedev
   \version $Revision: 1.1 $
   \date $Date: 2013/06/06 15:48:44 $
*/

 class PHAcceptParticleCentralArm;

class PHPyDBCorrTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyDBCorrTrigger( const std::string& name = "PHPyDBCorrTrigger" );
  
  //! destructor 
  virtual ~PHPyDBCorrTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int Init(PHCompositeNode *topNode); 

  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  bool D02KpiandB2eInCentralArm( PHPythiaContainer *phpylist );
  
  PHAcceptParticleCentralArm* westArm;
  PHAcceptParticleCentralArm* eastArm;

  double min_pT;
};

#endif	
