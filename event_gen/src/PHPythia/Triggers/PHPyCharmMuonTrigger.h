#ifndef PHPyCharmMuonTrigger_h
#define PHPyCharmMuonTrigger_h

// $Id: PHPyCharmMuonTrigger.h,v 1.1 2009/05/07 19:31:54 xrwang Exp $
#include "PHPyOpenMuonTrigger.h"

/*!
   \file PHPyCharmMuonTrigger.h
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.1 $
   \date $Date: 2009/05/07 19:31:54 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyCharmMuonTrigger: public PHPyOpenMuonTrigger
{
  
  public:
  
  //! constructor
  PHPyCharmMuonTrigger( const std::string& name = "PHPyCharmMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyCharmMuonTrigger( void );
  
};

#endif
