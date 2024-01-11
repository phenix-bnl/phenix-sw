// $Id: MuonCloseTFiles.cxx,v 1.2 2007/10/27 14:30:15 hpereira Exp $

#include <iostream>
#include <MUTOO.h>
#include <PHCompositeNode.h>
#include <PHTFileServer.h>

#include "MuonCloseTFiles.h"

using namespace std;

//_____________________________________________________
MuonCloseTFiles::MuonCloseTFiles( const char* name ):
	SubsysReco( name )
{
	MUTOO::PRINT( cout, "MuonCloseTFiles::MuonCloseTFiles" );	
}

//_____________________________________________________
int MuonCloseTFiles::End(PHCompositeNode *topNode)
{ 
	MUTOO::PRINT( cout, "MuonCloseTFiles::End" );
  cout << "This module should now be useless. " << endl;
  cout << "You can safely remove the \"se->registerSubsystem( new MuonCloseTFiles() )\" " << endl;
  cout << "from your macro." << endl;
	//PHTFileServer::get().close(); 
	MUTOO::PRINT( cout, "**" );
	return 0;
}
