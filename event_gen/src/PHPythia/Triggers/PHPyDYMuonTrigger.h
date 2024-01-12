#ifndef PHPyDYMuonTrigger_h
#define PHPyDYMuonTrigger_h

// $Id: PHPyDYMuonTrigger.h,v 1.1 2011/12/15 03:41:06 kblee Exp $
#include "PHPyOniaMuonTrigger.h"

/*!
   \file PHPyDYMuonTrigger.h
   \brief trigger module to select events that contains a Drell-Yan in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.1 $
   \date $Date: 2011/12/15 03:41:06 $
*/

//!  trigger module to select events that contains a Drell-Yan in the muon arm acceptance
class PHPyDYMuonTrigger: public PHPyOniaMuonTrigger
{
  
  public:
  
  //! constructor
  PHPyDYMuonTrigger( const std::string& name = "PHPyDYMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyDYMuonTrigger( void );
  
};

#endif
