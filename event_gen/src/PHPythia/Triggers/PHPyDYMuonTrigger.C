// $Id: PHPyDYMuonTrigger.C,v 1.1 2011/12/15 03:41:04 kblee Exp $


/*!
   \file PHPyDYMuonTrigger.C
   \brief trigger module to select events that contains a Drell-Yan in the muon arm acceptance
   \author Kwangbok Lee
   \version $Revision: 1.1 $
   \date $Date: 2011/12/15 03:41:04 $
*/

#include "PHPyDYMuonTrigger.h"

using namespace std;

//___________________________________________________________________________
PHPyDYMuonTrigger::PHPyDYMuonTrigger(const string &name): 
  PHPyOniaMuonTrigger(name)
{ set_parent_selection( Z ); }

//___________________________________________________________________________
PHPyDYMuonTrigger::~PHPyDYMuonTrigger( void )
{}

