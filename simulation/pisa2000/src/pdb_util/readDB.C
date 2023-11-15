// $Id: readDB.C,v 1.2 2007/03/27 15:38:31 hpereira Exp $
/*! 
  read phnx.par from database at a given time stamp and write it to file
*/
void readDB( void )
{

  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libfun4all.so");
  gSystem->Load("libPISApdbutil.so");

  PisaPdbUtil util;  
  
  /* you can use a run number or specify the rhic run number */
  
  // PHTimeStamp stamp=PHTimeStamp(2007,3,22,1,0,0);
  // util.read( PHTimeStamp(2007,3,22,1,0,0) );
  
  int rhic_run = 4;
  util.read( rhic_run );
  
  util.write( "phnx_clone.par" );
 
}
