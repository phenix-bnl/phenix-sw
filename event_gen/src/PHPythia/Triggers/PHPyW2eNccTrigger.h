#ifndef PHPyW2eTrigger_h
#define PHPyW2eTrigger_h

// $Id: PHPyW2eNccTrigger.h,v 1.1 2008/10/28 15:38:09 chiu Exp $

#include "PHPyTrigger.h"

/*!
   \file PHPyW2eNccTrigger.h
   \brief trigger module to select events that contains a W->e in the NCC acceptance
   \author Mickey Chiu
   \version $Revision: 1.1 $
   \date $Date: 2008/10/28 15:38:09 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyW2eNccTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyW2eNccTrigger( const std::string& name = "PHPyW2eNccTrigger" );
  
  //! destructor 
  virtual ~PHPyW2eNccTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters NCC
  bool w2eInNcc( PHPythiaContainer *phpylist );
  
};

#endif	// PHPyW2eTrigger_h
