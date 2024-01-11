// $Id: readDB.C,v 1.3 2008/10/10 19:38:03 hpereira Exp $
/*! 
  this method reads the database using specified timestamp
  and dumps corresponding DB entries to stdout
*/
void readDB( void )
{
  
  // load libraries
  gSystem->Load("libfun4all.so");
  gSystem->Load("libmuigeom.so");
  
  // initialize module
  mMuiInitModule* init = new mMuiInitModule();
  init->EnableDatabaseMode();
  
  // database entry timestamp
  // PHTimeStamp start=PHTimeStamp(2006,2,1,16,0,0);
  PHTimeStamp start( 2006, 05, 4, 0, 0, 0 );
  
  // fetch database
  init->SetSearchTimeStamp(start);
  PHCompositeNode *topnode = new PHCompositeNode("TOPNODE");
  init->event(topnode); 
  
  // print maps to screen
  init->dumpmaps();
}
