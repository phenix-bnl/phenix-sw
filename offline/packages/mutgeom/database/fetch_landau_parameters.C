// $Id: fetch_landau_parameters.C,v 1.1 2010/09/10 18:07:42 hpereira Exp $
/*!
  this method dumps the MuTr landau parameters
  for a given timestamp
*/
void fetch_landau_parameters( void )
{

    gSystem->Load("libmutgeom.so");
    gSystem->Load("libfun4all.so");

    // timestamps

    int run_number = 289000;
    RunToTime *runTime = RunToTime::instance();
    PHTimeStamp* time = runTime->getBeginTime( run_number );
    TMutDatabaseInit::set_time_stamp( *time );
    TMutDatabaseCntrl::set_verbosity( TMutDatabaseCntrl::LANDAU_PARAMETERS, TMutDatabaseCntrl::MAX );
    MutCalibSingleton::get().setUseGainCorrections( true );

    for( int arm = 0; arm < 2; arm++ )
    { MutArm(arm).fetchLandauParameters( run_number ); }

}
