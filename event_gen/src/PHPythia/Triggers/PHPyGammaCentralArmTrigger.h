#ifndef PHPyGammaCentralArmTrigger_h
#define PHPyGammaCentralArmTrigger_h

#include "PHPyTrigger.h"

/*!
   \file PHPyGammaCentralArmTrigger.h
   \brief trigger module to select events that contains a photon in CentralArms acceptance
   \author Sasha Lebedev (lebedev@iastate.edu)
   \date $Date: 2010/03/30 18:57:30 $
*/

class PHPyGammaCentralArmTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyGammaCentralArmTrigger(const float minpt, const std::string& name = "PHPyGammaCentralArmTrigger" );
  
  //! destructor 
  virtual ~PHPyGammaCentralArmTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters NCC
  bool GammaInCentralArm( PHPythiaContainer *phpylist );
  float minptcut;
  
};

#endif	// PHPyGammaTrigger_h
