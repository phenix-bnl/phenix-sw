#ifndef __PHPYW2ECENTTRIGGER_H__
#define __PHPYW2ECENTTRIGGER_H__

// $Id: PHPyW2eCentTrigger.h,v 1.1 2009/02/12 21:25:40 chiu Exp $

#include "PHPyTrigger.h"

/*!
   \file PHPyW2eCentTrigger.h
   \brief trigger module to select events that contains a W->e in the Central Arm acceptance
   \author Mickey Chiu
   \version $Revision: 1.1 $
   \date $Date: 2009/02/12 21:25:40 $
*/

//!  trigger module to select events that contains an electron from W in the central arm acceptance
class PHPyW2eCentTrigger: public PHPyTrigger
{
  
  public:
  
  //! constructor
  PHPyW2eCentTrigger( const std::string& name = "PHPyW2eCentTrigger" );
  
  //! destructor 
  virtual ~PHPyW2eCentTrigger( void )
  {}
  
  //!@name Methods Derived from SubsysReco
  //@{
  int process_event(PHCompositeNode *topNode);
  
  int End(PHCompositeNode *topNode); 
  //@}
  
  protected:
  
  //! true if e is found in final state that enters the central arm
  bool W2eInCent( PHPythiaContainer *phpylist );
  
};

#endif	// __PHPYW2ECENTTRIGGER_H__
