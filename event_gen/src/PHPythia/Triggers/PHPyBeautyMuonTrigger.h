#ifndef PHPyBeautyMuonTrigger_h
#define PHPyBeautyMuonTrigger_h

// $Id: PHPyBeautyMuonTrigger.h,v 1.1 2009/05/07 19:31:54 xrwang Exp $
#include "PHPyOpenMuonTrigger.h"

/*!
   \file PHPyBeautyMuonTrigger.h
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.1 $
   \date $Date: 2009/05/07 19:31:54 $
*/

//!  trigger module to select events that contains a J/Psi in the muon arm acceptance
class PHPyBeautyMuonTrigger: public PHPyOpenMuonTrigger
{
  
  public:
  
  //! constructor
  PHPyBeautyMuonTrigger( const std::string& name = "PHPyBeautyMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyBeautyMuonTrigger( void );
  
};

#endif
