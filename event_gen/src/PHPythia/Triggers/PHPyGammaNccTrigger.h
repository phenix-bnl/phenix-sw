#ifndef PHPyGammaTrigger_h
#define PHPyGammaTrigger_h

// $Id: PHPyGammaNccTrigger.h,v 1.1 2008/10/28 15:38:08 chiu Exp $

#include "PHPyTrigger.h"

/*!
   \file PHPyGammaNccTrigger.h
   \brief trigger module to select events that contains a W->e in the NCC acceptance
   \author Mickey Chiu
   \version $Revision: 1.1 $
   \date $Date: 2008/10/28 15:38:08 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyGammaNccTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyGammaNccTrigger( const std::string& name = "PHPyGammaNccTrigger" );
  
  //! destructor 
  virtual ~PHPyGammaNccTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters NCC
  bool GammaInNcc( PHPythiaContainer *phpylist );
  
};

#endif	// PHPyGammaTrigger_h
