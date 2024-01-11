// $Id: fetch_geometry.C,v 1.3 2008/06/24 13:35:02 hpereira Exp $
/*! 
  this method dumps both Muon tracker arms internal alignment
  for a given timestamp
*/
void fetch_geometry()
{

  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  PdbBankID bankID = 0;
  
  // 2006 run 198981
  // PHTimeStamp tsearch( 2006, 05, 4, 0, 0, 0 );

  // 2003 run 78307
  PHTimeStamp tsearch( 2003, 03, 14, 0, 0, 0 );
  
  // 2003 run 92446
  // PHTimeStamp tsearch( 2003,05, 30, 0, 0, 0 );
  cout << "fetch_geometry - search time: " << tsearch << endl;
  
  // comment/uncomment any of the following line depending on the type of information to be printed to screen.
  // TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::STRIP_SPACING, TMutDatabaseCntrl::MAX );
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::STRIP_GEOMETRY, TMutDatabaseCntrl::MAX );
  // TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::WIRE_GEOMETRY, TMutDatabaseCntrl::MAX );

  TMutDatabaseInit::set_time_stamp( tsearch );
  TMutDatabaseInit::initialize();
  
}
