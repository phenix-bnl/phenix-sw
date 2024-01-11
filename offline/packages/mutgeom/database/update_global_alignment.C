// $Id: update_global_alignment.C,v 1.1 2007/04/05 08:42:55 hpereira Exp $
/*! 
  this method writed the internal alignment read from a file to the database.
  The filename and timestamps are to be changed depending on the validity of the 
  alignment constants. 
  Note that even though only the north arm is created, the database is 
  updated for both arms.
*/

void update_global_alignment()
{
  
  printf("Loading PHOOL libraries\n");
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  PHTimeStamp Tstart( 2006, 3, 4, 2, 16, 20, 0 );
  PHTimeStamp Tstop( 2006, 6, 5, 13, 52, 50, 0 )
  Tstop.setToFarFuture();

  char *descrip = "run 6 mutr global alignment";
  char *fileName = "mut.globalAligConsts.dat";

  PdbBankID bankID = 0;
  MutArm *arm = new MutArm(1);	
  if(arm->updateGlobalAligConsts(Tstart, Tstop, bankID, descrip, fileName)) 
  cout<<"update_global_alignment - arm global alignment written to DB\n";
  
  delete arm;
  
}
