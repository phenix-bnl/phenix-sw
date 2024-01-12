#ifndef PHPyUpsilonMuonTrigger_h
#define PHPyUpsilonMuonTrigger_h


#include "PHPyOniaMuonTrigger.h"

/*!
   \file PHPyUpsilonMuonTrigger.h
   \brief trigger module to select events that contains a Upsilon in the muon arm acceptance
   \author Kwangbok Lee
*/

//!  trigger module to select events that contains a Upsilon in the muon arm acceptance
class PHPyUpsilonMuonTrigger: public PHPyOniaMuonTrigger
{
  
  public:
  
  //! constructor
  PHPyUpsilonMuonTrigger( const std::string& name = "PHPyUpsilonMuonTrigger" );
  
  //! destructor 
  virtual ~PHPyUpsilonMuonTrigger( void )
  {}
  
};

#endif
