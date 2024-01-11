// $Id: fetch_dcm_channel_map.C,v 1.3 2009/07/02 21:01:01 hpereira Exp $
//! read dcm channel map from database for a given time stamp, and print to files
void fetch_dcm_channel_map()
{
  
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  //PHTimeStamp search( 2007, 11, 30, 12, 0, 0 );
  // PHTimeStamp search( 2008, 11, 30, 12, 0, 0 );
  PHTimeStamp search( 2009, 07, 01, 06, 43, 0 );
  string tag = "";
  
  for( int i_arm = 0; i_arm < 2; i_arm++ )
  { MutArm( i_arm, search ).printDCMChannelMap( tag ); }
      
}
