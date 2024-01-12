#ifndef __PHPYMPCTRIGGER_H__
#define __PHPYMPCTRIGGER_H__

// $Id: PHPyMpcTrigger.h,v 1.1 2009/03/17 16:57:46 chiu Exp $

#include "PHPyTrigger.h"

/*!
   \file PHPyMpcTrigger.h
   \brief trigger module to select events that contains something in the MPC acceptance
   \author Mickey Chiu
   \version $Revision: 1.1 $
   \date $Date: 2009/03/17 16:57:46 $
*/

//!  trigger module to select events that contains something in the MPC acceptance
class PHPyMpcTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyMpcTrigger( const std::string& name = "PHPyMpcTrigger" );
  
  //! destructor 
  virtual ~PHPyMpcTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters NCC
  bool PizeroInMpc( PHPythiaContainer *phpylist );
  
};

#endif	// __PHPYMPCTRIGGER_H__
