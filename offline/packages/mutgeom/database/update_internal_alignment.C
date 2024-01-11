// $Id: update_internal_alignment.C,v 1.1 2007/04/05 08:42:55 hpereira Exp $
/*! 
  this method writed the internal alignment read from a file to the database.
  The filename and timestamps are to be changed depending on the validity of the 
  alignment constants. 
  Note that even though only the north arm is created, the database is 
  updated for both arms.
*/

void update_internal_alignment()
{
  
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  //used for run2 + run3 d+Au and p+p
  PHTimeStamp Tstart( 2002, 9, 3, 0, 0, 0 );	
  PHTimeStamp Tstop( 2003, 5, 31, 0, 0, 0);

//   //used for 2006 and later
//   PHTimeStamp Tstart( 2006, 9, 18, 8, 30, 00, 0 );	
//   PHTimeStamp Tstop; Tstop.setToFarFuture();

  char *descrip = "run 3 alignment, back ported from run6 for repass";
  char *fileName = "mut.internalAligConsts.dat";

  PdbBankID bankID = 0;
  MutArm *arm = new MutArm(1);	
  if(arm->updateInternalAligConsts(Tstart, Tstop, bankID, descrip, fileName)) 
  cout<<"update_local_alignment - arm internal alignment written to DB\n";
  
  delete arm;
  
}
