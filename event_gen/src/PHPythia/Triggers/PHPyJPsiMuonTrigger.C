// $Id: PHPyJPsiMuonTrigger.C,v 1.2 2009/03/16 17:30:13 hpereira Exp $


/*!
   \file PHPyJPsiMuonTrigger.C
   \brief trigger module to select events that contains a J/Psi in the muon arm acceptance
   \author Hugo Pereira
   \version $Revision: 1.2 $
   \date $Date: 2009/03/16 17:30:13 $
*/

#include "PHPyJPsiMuonTrigger.h"

using namespace std;

//___________________________________________________________________________
PHPyJPsiMuonTrigger::PHPyJPsiMuonTrigger(const string &name): 
  PHPyOniaMuonTrigger(name)
{ set_parent_selection( JPsi ); }

//___________________________________________________________________________
PHPyJPsiMuonTrigger::~PHPyJPsiMuonTrigger( void )
{}

