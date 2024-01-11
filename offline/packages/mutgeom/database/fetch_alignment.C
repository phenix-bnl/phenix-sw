// $Id: fetch_alignment.C,v 1.2 2009/01/23 15:39:22 hpereira Exp $
/*! 
  this method dumps both Muon tracker arms internal alignment
  for a given timestamp
*/
void fetch_alignment()
{

  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  PdbBankID bankID = 0;
  
  // 2003 run 78307
  PHTimeStamp tsearch( 2002, 09, 04, 0, 0, 0 );
  PHTimeStamp tsearch( 2003, 11, 02, 0, 0, 0 );
  PHTimeStamp tsearch( 2006, 03, 04, 0, 0, 0 );
  PHTimeStamp tsearch( 2006, 09, 19, 0, 0, 0 );
  cout << "fetch_alignment - search time: " << tsearch << endl;
  
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::INTERNAL_ALIGN, TMutDatabaseCntrl::MAX );
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::GLOBAL_ALIGN, TMutDatabaseCntrl::MAX );

  // South arm	
  cout << "fetch_alignment - south arm:" << endl;
  MutArm *south = new MutArm( 0 );
  
  cout << "fetch_alignment - global alignment:" << endl;
  if( south->fetchGlobalAligConsts(tsearch, bankID, "mut.globalAligConsts.dat")) cout<<"South arm global alignment read from DB\n" << endl;

  cout << "fetch_alignment - internal alignment:" << endl;
  if( south->fetchInternalAligConsts(tsearch, bankID, "mut.internalAligConsts.dat")) cout<<"South arm internal alignment read from DB\n" << endl;
  
  delete south;
  cout << endl;

  // North arm
  cout << "fetch_alignment - north arm:" << endl;
  MutArm *north = new MutArm( 1 );

  cout << "fetch_alignment - global alignment:" << endl;
  if( north->fetchGlobalAligConsts(tsearch, bankID)) cout<<"north arm global alignment read from DB\n" << endl;

  cout << "fetch_alignment - internal alignment:" << endl;
  if( north->fetchInternalAligConsts(tsearch, bankID)) cout<<"north arm internal alignment read from DB\n" << endl;
  
  delete north;
  cout << endl;
  
  cout << "fetch_alignment - done." << endl;
}
