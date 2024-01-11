// $Id: update_dcm_channel_map.C,v 1.2 2009/07/02 21:01:01 hpereira Exp $
//! write dead channels from file to database
/*! 
  takes a of disabled channels as input.
  user must provide the validity range for the database insertion, namely 
  the time interval between which the channels are to be disabled
*/
void update_dcm_channel_map( void )
{
    
  /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  PHTimeStamp begin( 1233441120 );
  PHTimeStamp end( 1241021639 );
  
  // make sure channels are read from files
  TMutDatabaseCntrl::set_database_access( "use_local_dcm_map_file", true );
  
  // read channels from files
  // MutArm south( 0 );
  MutArm north( 1 );
  
  // update the database
  north.updateDCMChannelMap( begin, end, "North arm station3 Run9 dcm channel map, with swapped cables", 2 );
  
  return;
  
}
