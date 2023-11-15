// $Id: writeDB.C,v 1.1 2007/03/22 15:13:48 hpereira Exp $
/*! 
  reads the phnx.par file and write it to the database
  with given timestamps
*/
void writeDB( void )
{

  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libfun4all.so");
  gSystem->Load("libPISApdbutil.so");

  PisaPdbUtil util;
  util.read( "phnx.par" );
  
  // corresponds to run 5000
  PHTimeStamp start=PHTimeStamp(2002,10,3,0,0,0);
  PHTimeStamp stop;
  stop.setToFarFuture();
  
  util.write( start, stop );
 
}
