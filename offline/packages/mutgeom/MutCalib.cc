// $Id: MutCalib.cc,v 1.13 2013/07/24 18:48:57 slash Exp $

/*!
  \file MutCalib.cc
  \brief wrapper around muon tracker calibrations initialization
  \author Douglas Fields, Nicki Bruner, Hugo Pereira
  \version $Revision: 1.13 $
  \date $Date: 2013/07/24 18:48:57 $
*/


#include "MutCalib.h"
#include "TMutDatabaseInit.h"
#include "TMutDatabaseCntrl.h"
#include "PHTimeStamp.h"

#include "MUTGEOM.h"

#include <cstdlib>
#include <iostream>

//______________________________________________
MutCalibStrip* MutCalib()
{
  if( !MutCalibSingleton::get().initialized() )
  { MutCalibSingleton::get().initialize(); }

  return &MutCalibSingleton::get().get_calibrations();

}

//______________________________________________
MutCalibSingleton& MutCalibSingleton::get( void )
{
  static MutCalibSingleton singleton;
  return singleton;
}


//______________________________________________
MutCalibSingleton::MutCalibSingleton( void ):
  _initialized( false )
  {}

//______________________________________________
MutCalibSingleton::~MutCalibSingleton( void )
{}

//______________________________________________
void MutCalibSingleton::initialize()
{
  assert( !initialized() );

  setUseNewCalibrationMethod( true );
  if ( TMutDatabaseCntrl::get_CalibrationMethod() == TMutDatabaseCntrl::CURVE)
    {
      std::cout << "MutCalibSingleton::initialize - Using the old curve based ADC->charge calibration" << std::endl;
      setUseNewCalibrationMethod( false );
    }

  if( TMutDatabaseCntrl::get_database_access( "use_local_calibration_file" ) )
  {
    std::string filename( TMutDatabaseCntrl::get_filename( "use_local_calibration_file" ) );
    std::cout << "MutCalibSingleton::initialize - reading calibrations from local file: " << filename << std::endl;

    // textGetAll returns non-zero on failure.
    if( get_calibrations().txtGetAll( filename.c_str() ) )
    {
      std::cout << "MutCalibSingleton::initialize - unable to read calibrations" << std::endl;
      std::cout << "MutCalibSingleton::initialize - aborting" << std::endl;
      exit(0);
    }

  } else {

    PHTimeStamp timeStamp = TMutDatabaseInit::get_time_stamp();
    std::cout << "MutCalibSingleton::initialize - time stamp: " << timeStamp << " (" << timeStamp.getTics() << ")" << std::endl;

    if( !timeStamp.getTics() )
    {
      std::cout << "MutCalibSingleton::initialize - time stamp is invalid. No point continuing" << std::endl;
      std::cout << "MutCalibSingleton::initialize - You should either do the initialization by calling " << std::endl;
      std::cout << "MutCalibSingleton::initialize - TMutDatabaseInit::initialize( PHCompositeNode *);, " << std::endl;
      std::cout << "MutCalibSingleton::initialize - in which case run number will be used to set the time stamp; " << std::endl;
      std::cout << "MutCalibSingleton::initialize - or set a valid timestamp manually, using " << std::endl;
      std::cout << "MutCalibSingleton::initialize - TMutDatabaseInit::setTimeStamp( PHTimeStamp ); in your macro. " << std::endl;
      exit(0);
    }

    get_calibrations().dbGetAll(timeStamp);

  }

  set_initialized( true );

  if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::CALIBRATIONS ) >= TMutDatabaseCntrl::MAX )
  { get_calibrations().print( std::cout ); }

  return;
}

//______________________________________________
void MutCalibSingleton::reset( void )
{

  get_calibrations().reset();
  set_initialized( false );

}
