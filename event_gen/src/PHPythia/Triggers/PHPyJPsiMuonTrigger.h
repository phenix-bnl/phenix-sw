#ifndef PHPyJPsiMuonTrigger_h
#define PHPyJPsiMuonTrigger_h

// $Id: PHPyJPsiMuonTrigger.h,v 1.2 2009/03/16 17:30:13 hpereira Exp $
#include "PHPyOniaMuonTrigger.h"

/*!
   \file PHPyJPsiMuonTrigger.h
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.2 $
   \date $Date: 2009/03/16 17:30:13 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyJPsiMuonTrigger: public PHPyOniaMuonTrigger
{
  
  public:
  
  //! constructor
  PHPyJPsiMuonTrigger( const std::string& name = "PHPyJPsiMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyJPsiMuonTrigger( void );
  
};

#endif
