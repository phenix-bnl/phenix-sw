// $Id: PHPyCharmMuonTrigger.C,v 1.1 2009/05/07 19:31:54 xrwang Exp $


/*!
   \file PHPyCharmMuonTrigger.C
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.1 $
   \date $Date: 2009/05/07 19:31:54 $
*/

#include "PHPyCharmMuonTrigger.h"

using namespace std;

//___________________________________________________________________________
PHPyCharmMuonTrigger::PHPyCharmMuonTrigger(const string &name): 
  PHPyOpenMuonTrigger(name)
{ set_parent_selection( AllD ); }

//___________________________________________________________________________
PHPyCharmMuonTrigger::~PHPyCharmMuonTrigger( void )
{}

