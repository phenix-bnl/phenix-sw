// $Id: PHPyUpsilonMuonTrigger.C,v 1.2 2009/03/16 17:30:59 hpereira Exp $


/*!
   \file PHPyUpsilonMuonTrigger.C
   \brief trigger module to select events that contains a Upsilon in the muon arm acceptance
   \author Kwangbok Lee
*/

#include "PHPyUpsilonMuonTrigger.h"

using namespace std;

//___________________________________________________________________________
PHPyUpsilonMuonTrigger::PHPyUpsilonMuonTrigger(const string &name): 
  PHPyOniaMuonTrigger(name)
  { set_parent_selection( Upsilon ); }
  
