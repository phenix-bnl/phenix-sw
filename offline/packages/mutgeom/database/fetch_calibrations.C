// $Id: fetch_calibrations.C,v 1.3 2010/09/10 17:21:59 hpereira Exp $
/*!
  this method dumps the MuTr strip calibration constants
  for a given timestamp
*/
void fetch_calibrations( void )
{

    gSystem->Load("libmutgeom.so");
    gSystem->Load("libfun4all.so");

    // timestamps
    PHTimeStamp tsearch( 2009, 06, 13, 0, 0, 0 );
    TMutDatabaseInit::set_time_stamp( tsearch );

    MutCalibSingleton::get().setUseGainCorrections( false );
    MutCalibSingleton::get().reset();
    MutCalibSingleton::get().initialize();

    // get calibration pointer
    MutCalibStrip* calib = &MutCalibSingleton::get().get_calibrations();
    calib->txtPutAll( "mutcalibstrips.txt" );

}
