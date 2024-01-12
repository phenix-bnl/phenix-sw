#ifndef PHPyJetNccTrigger_h
#define PHPyJetNccTrigger_h

// $Id: PHPyJetNccTrigger.h,v 1.1 2008/11/18 22:44:36 chiu Exp $

#include "PHPyTrigger.h"

/*!
   \file PHPyJetNccTrigger.h
   \brief trigger module to select events that contains a jet in the NCC acceptance, 0.9<|eta|<2.7
   \author Mickey Chiu
   \version $Revision: 1.1 $
   \date $Date: 2008/11/18 22:44:36 $
*/

//!  trigger module to select events that contains a Jet
class PHPyJetNccTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyJetNccTrigger( const std::string& name = "PHPyJetNccTrigger" );
  
  //! destructor 
  virtual ~PHPyJetNccTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if jet is found that enters NCC
  bool JetInNcc( PHPythiaContainer *phpylist );
  
};

#endif	// PHPyJetNccTrigger_h
