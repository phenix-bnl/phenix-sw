// $Id: fetch_mutr_alignment.C,v 1.1 2013/10/16 22:34:23 jinhuang Exp $
/*! 
  this method dumps both Muon tracker arms internal alignment
  for a given timestamp
*/
void fetch_mutr_alignment()
{

  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  PdbBankID bankID = 0;
  
  // 2013 run 78307
  PHTimeStamp tsearch( 2013, 03, 11, 0, 0, 0 );
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
