// $Id: fetch_dead_channels.C,v 1.2 2009/01/23 15:39:22 hpereira Exp $
//! read dead (FEM) channels from database for a given run
void fetch_dead_channels()
{
  
   /* 
    note: need to load libfun4all.so to make sure 
    postgres database is updated properly
  */
  gSystem->Load("libmutgeom.so");
  gSystem->Load("libfun4all.so");

  // run4 typical runnumber
  // int run_number = 109656; 
  
  // run8 typical runnumber
  int run_number = 259576; 
  
  RunToTime *runTime = RunToTime::instance();
  PHTimeStamp* time = runTime->getBeginTime( run_number );
  if( !time ) 
  {
    cout << "fetch_dead_channels - unable to get time for run " << run_number << " - aborting." << endl;
    return;
  }
  cout << "update_dead_channels - runnumber: " << run_number << " time: " << *time << " (" << time->getTics() << ")" << endl;
  
  cout << "update_dead_channels - processing south arm." << endl;
  MutArm *south = new MutArm( 0 );  
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::DEAD_CHANNELS, TMutDatabaseCntrl::MAX );
  south->fetchDeadChannels( *time, 0 );
  
  cout << endl;
  cout << "update_dead_channels - processing north arm." << endl;
  MutArm *north = new MutArm( 1 );  
  TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::DEAD_CHANNELS, TMutDatabaseCntrl::MAX );
  north->fetchDeadChannels( *time, 0 );
  
}
