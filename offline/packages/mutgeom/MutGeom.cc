// $Id: MutGeom.cc,v 1.20 2011/10/19 13:32:33 hpereira Exp $

/*!
   \file		MutGeom.cc
   \brief	 Muon tracker Geometry
   \author	Hugo Pereira
   \version $Revision: 1.20 $
   \date		$Date: 2011/10/19 13:32:33 $
*/

#include "MutGeom.h"
#include "TMutDatabaseInit.h"
#include "TMutDatabaseCntrl.h"

#include <PHTimeStamp.h>
#include <recoConsts.h>

#include <cstdlib>
#include <iostream>

using namespace std;
using namespace MUTGEOM;

//______________________________________________
MutArm* SouthArm()
{ return MutGeom::get().south_arm(); }

//______________________________________________
MutArm* NorthArm()
{ return MutGeom::get().north_arm(); }

//______________________________________________
MutDCMChannelMap* SouthArmChannelMap()
{ return MutGeom::get().south_dcm_map(); }

//______________________________________________
MutDCMChannelMap* NorthArmChannelMap()
{ return MutGeom::get().north_dcm_map(); }

//______________________________________________
void MutGeom::reset( void )
{
  if( _south_arm ) { delete _south_arm; _south_arm = 0; }
  if( _north_arm ) { delete _north_arm; _north_arm = 0; }

  if( _south_dcm_map ) { delete _south_dcm_map; _south_dcm_map = 0; }
  if( _north_dcm_map ) { delete _north_dcm_map; _north_dcm_map = 0; }

}

//______________________________________________
MutGeom::MutGeom( void ):
  _south_arm(0),
  _north_arm(0),
  _south_dcm_map(0),
  _north_dcm_map(0),
  _mut_pisa_parameters(0)
{}

//______________________________________________
MutGeom::~MutGeom( void )
{
  if( _south_dcm_map ) delete _south_dcm_map;
  if( _north_dcm_map ) delete _north_dcm_map;
  if( _south_arm ) delete _south_arm;
  if( _north_arm ) delete _north_arm;
}

//______________________________________________
MutArm* MutGeom::south_arm()
{

  if (!_south_arm)
  {

    // retrieve configuration from TMutDatabaseInit
    PHTimeStamp time_stamp( TMutDatabaseInit::get_time_stamp() );
    PdbBankID bank_id = TMutDatabaseInit::get_alignment_bank_id();

    cout
      << "MutGeom::south_arm -"
      << " TimeStamp = " << time_stamp
      << " bank_id = " << bank_id.getInternalValue()
      << endl;

    if( !time_stamp.getTics() )
    {
      cout << "MutGeom::south_arm - time stamp is invalid. No point continuing." << endl;
      cout << "MutGeom::south_arm - You should either do the initialization by calling " << endl;
      cout << "MutGeom::south_arm - TMutDatabaseInit::initialize( PHCompositeNode *);, " << endl;
      cout << "MutGeom::south_arm - in which case run number will be used to set the time stamp; " << endl;
      cout << "MutGeom::south_arm - or set a valid timestamp manually, using " << endl;
      cout << "MutGeom::south_arm - TMutDatabaseInit::set_time_stamp( PHTimeStamp ); in your macro. " << endl;
      exit(0);
    }

    if (_mut_pisa_parameters)
    {

      cout << "MutGeom::south_arm - initializing geometry for Monte Carlo" << endl;
      _south_arm = new MutArm(South, _mut_pisa_parameters);

    } else if(TMutDatabaseCntrl::get_database_access("arms_from_database")==false)
    {

      cout << "MutGeom::south_arm" << endl;
      cout << "You have requested to initialize the South MuTr geometry WITHOUT using the database!" << endl;
      cout << "The surveys retrieved from afs are not guaranteed to be correct for this run."<< endl;
      cout << "There are some channel mapping fixes which you will also not get."<< endl;
      cout << "You should not use these results for anything other than debugging!"<< endl;
      _south_arm = new MutArm(South);

    } else {

      cout << "MutGeom::south_arm - initializing geometry for real data" << endl;
      _south_arm = new MutArm(South, time_stamp);

    }

    // database/file initialization
    initialize_arm( _south_arm, time_stamp, bank_id );

  }

  return _south_arm;

}

//______________________________________________
MutArm* MutGeom::north_arm()
{

  if (!_north_arm)
  {

    // retrieve configuration from TMutDatabaseInit
    PHTimeStamp time_stamp( TMutDatabaseInit::get_time_stamp() );
    PdbBankID bank_id = TMutDatabaseInit::get_alignment_bank_id();

    cout
      << "MutGeom::north_arm -"
      << " TimeStamp = " << time_stamp
      << " bank_id = " << bank_id.getInternalValue()
      << endl;

    if( !time_stamp.getTics() )
    {
      cout << "MutGeom::north_arm - time stamp is invalid. No point continuing." << endl;
      cout << "MutGeom::north_arm - You should either do the initialization by calling " << endl;
      cout << "MutGeom::north_arm - TMutDatabaseInit::initialize( PHCompositeNode *);, " << endl;
      cout << "MutGeom::north_arm - in which case run number will be used to set the time stamp; " << endl;
      cout << "MutGeom::north_arm - or set a valid timestamp manually, using " << endl;
      cout << "MutGeom::north_arm - TMutDatabaseInit::set_time_stamp( PHTimeStamp ); in your macro. " << endl;
      exit(0);
    }

    if (_mut_pisa_parameters)
    {

      cout << "MutGeom::north_arm - initializing geometry for Monte Carlo" << endl;
      _north_arm = new MutArm(North, _mut_pisa_parameters);

    } else if(TMutDatabaseCntrl::get_database_access("arms_from_database")==false)
    {

      cout << "MutGeom::north_arm" << endl;
      cout << "You have requested to initialize the South MuTr geometry WITHOUT using the database!" << endl;
      cout << "The surveys retrieved from afs are not guaranteed to be correct for this run."<< endl;
      cout << "There are some channel mapping fixes which you will also not get."<< endl;
      cout << "You should not use these results for anything other than debugging!"<< endl;
      _north_arm = new MutArm(North);

    } else {

      cout << "MutGeom::north_arm - initializing geometry for real data" << endl;
      _north_arm = new MutArm(North, time_stamp);

    }

    initialize_arm( _north_arm, time_stamp, bank_id );

  }

  return _north_arm;

}

//______________________________________________
MutDCMChannelMap* MutGeom::south_dcm_map()
{
  if( !_south_dcm_map ) _south_dcm_map = new MutDCMChannelMap(south_arm());
  return _south_dcm_map;
}

//______________________________________________
MutDCMChannelMap* MutGeom::north_dcm_map()
{
  if( !_north_dcm_map ) _north_dcm_map = new MutDCMChannelMap(north_arm());
  return _north_dcm_map;
}

//_________________________________________________________________________
void MutGeom::initialize_arm( MutArm* arm, PHTimeStamp time_stamp, PdbBankID bank_id ) const
{

  // dead channels
  int run_number = recoConsts::instance()->get_IntFlag("RUNNUMBER", 0);
  if(TMutDatabaseCntrl::get_database_access("use_local_landau_parameters_file")) {
    
    arm->fetchLandauParameters( TMutDatabaseCntrl::get_filename( "use_local_landau_parameters_file" ).c_str() );
  
  } else if( run_number ) {

    arm->fetchLandauParameters( run_number );

  } else {

    cout
      << "MutGeom::initialize_arm - RUNNUMBER not set. Unable to fetch landau parameters" << endl
      << "MutGeom::initialize_arm - this is harmless on real data" << endl;

  }

  // Disabled anodes
  if(TMutDatabaseCntrl::get_database_access("disable_HV"))
  {

    /* for disabled anodes, the bankID used to access the database must be the run number */
    if(TMutDatabaseCntrl::get_database_access("use_local_dead_HV_file")) arm->fetchDisabledWires( TMutDatabaseCntrl::get_filename( "use_local_dead_HV_file" ).c_str() );
    else arm->fetchDisabledWires( time_stamp, bank_id );

    if(TMutDatabaseCntrl::get_database_access("use_local_dead_wire_file")) arm->fetchDisconnectedWires( TMutDatabaseCntrl::get_filename( "use_local_dead_wire_file" ).c_str() );
    else arm->fetchDisconnectedWires(time_stamp, bank_id);

  }

  //Duplicate or other dead channels
  if(TMutDatabaseCntrl::get_database_access("dead_channels"))
  {
    if(TMutDatabaseCntrl::get_database_access("use_local_dead_channel_file")) arm->fetchDeadChannels( TMutDatabaseCntrl::get_filename( "use_local_dead_channel_file" ).c_str() );
    else arm->fetchDeadChannels(time_stamp, bank_id);
  }

  //adjust for strips with attenuated charge
  if(TMutDatabaseCntrl::get_database_access("attenuated_channels"))
  {
    if(TMutDatabaseCntrl::get_database_access("use_local_attenuated_chnl_file")) arm->fetchAttenuatedChannels( TMutDatabaseCntrl::get_filename( "use_local_attenuated_chnl_file" ).c_str() );
    else arm->fetchAttenuatedChannels(time_stamp, bank_id);
  }

  //Internal alignment
  if(TMutDatabaseCntrl::get_database_access("internal_alignment_corrections"))
  {
    if(TMutDatabaseCntrl::get_database_access("use_local_internal_align_file")) arm->fetchInternalAligConsts( TMutDatabaseCntrl::get_filename( "use_local_internal_align_file" ).c_str() );
    else arm->fetchInternalAligConsts(time_stamp, bank_id);
  }

  //Global alignment
  if(TMutDatabaseCntrl::get_database_access("global_alignment_corrections"))
  {
    if(TMutDatabaseCntrl::get_database_access("use_local_global_align_file")) arm->fetchGlobalAligConsts( TMutDatabaseCntrl::get_filename( "use_local_global_align_file" ).c_str() );
    else arm->fetchGlobalAligConsts(time_stamp, bank_id);
  }

  return;

}
