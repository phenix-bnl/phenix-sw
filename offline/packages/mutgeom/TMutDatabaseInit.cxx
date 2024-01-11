// $Id: TMutDatabaseInit.cxx,v 1.36 2010/05/07 18:19:57 hpereira Exp $

#include <gconst.h>

// PHOOL
#include <mMfmMT.h>
#include <PHGeant.hh>

// FUN4ALL
#include <RunNumberRanges.h>
#include <recoConsts.h>

#include "TMutDatabaseCntrl.h"
#include "TMutDatabaseInit.h"
#include "MutGeom.h"
#include "MutCalib.h"
#include "MutPlane.h"

#include <cstdlib>
#include <iostream>
#include <unistd.h>

using namespace std;

// make sure the gconsx and gconst are initialized
Gconsx gconsx;
Gconst gconst;

//__________________________________________________________
bool TMutDatabaseInit::_init_done = false;
bool TMutDatabaseInit::_time_stamp_set = false;

int TMutDatabaseInit::_alignment_bank_id = 0;
PHTimeStamp TMutDatabaseInit::_time_stamp = 0;

//__________________________________________________________
void TMutDatabaseInit::reset( void )
{
  cout << "TMutDatabaseInit::reset" << endl;

  // reset geometry and calibrations
  MutGeom::get().reset();
  MutCalibSingleton::get().reset();

  _init_done = false;
  _time_stamp_set = false;

}

//__________________________________________________________
void TMutDatabaseInit::initialize( PHCompositeNode* )
{ if( !_init_done ) _init_done = do_init(); }

//__________________________________________________
bool TMutDatabaseInit::do_init( void )
{

  // Calibrations and geometry
  /* if timestamp is not set, retrieve it from recoConsts */
  recoConsts *rc = recoConsts::instance();
  if( !_time_stamp_set ) {

    _time_stamp = rc->get_TimeStamp();
    cout << "TMutDatabaseInit::do_init - using recoConsts timestamp: " << _time_stamp << endl;

  } else cout << "TMutDatabaseInit::do_init - using manually set timestamp: " << _time_stamp << endl;

  if( !_time_stamp.getTics() )
  {

    cout << "TMutDatabaseInit::do_init - Time stamp is invalid. No point continuing." << endl;
    cout << "TMutDatabaseInit::do_init - You should either make sure the run number is set properly for your job, " << endl;
    cout << "TMutDatabaseInit::do_init - in which case it will be used to set the time stamp; or set a valid timestamp " << endl;
    cout << "TMutDatabaseInit::do_init - manually, using TMutDatabaseInit::setTimeStamp( PHTimeStamp ); in your macro. " << endl;
    exit(0);

  }

  cout << endl;
  MutGeom::get().south_arm();

  cout << endl;
  MutGeom::get().north_arm();

  // print strip and wire geometry
  if(
    ( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::STRIP_GEOMETRY ) >= TMutDatabaseCntrl::MAX ) ||
    ( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::STRIP_SPACING ) >= TMutDatabaseCntrl::MAX ) ||
    ( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::WIRE_GEOMETRY ) >= TMutDatabaseCntrl::MAX ) )
  {
    for( unsigned int i_arm = 0; i_arm < MUTGEOM::NumberOfArms; i_arm++ )
    for( unsigned int i_station = 0; i_station < MUTGEOM::NumberOfStations; i_station++ )
    for( unsigned int i_octant = 0; i_octant < MUTGEOM::NumberOfOctants; i_octant++ )
    for( unsigned int i_half = 0; i_half < MUTGEOM::NumberOfHalfOctants; i_half++ )
    for( unsigned int i_gap = 0; i_gap < MUTGEOM::NumberOfGaps; i_gap++ )
    {

      if( i_station == MUTGEOM::Station3 && i_gap == MUTGEOM::Gap3 ) continue;
      for( unsigned int i_plane = 0; i_plane < MUTGEOM::NumberOfPlanes; i_plane++ )
      {

        MutArm* arm( ( i_arm == MUTGEOM::South ) ? SouthArm():NorthArm() );
        MutPlane* plane( arm
          ->f_pMutStations[i_station]
          ->f_pMutOctants[i_octant]
          ->f_pMutHalfOctants[i_half]
          ->f_pMutGaps[i_gap]
          ->f_pMutPlanes[i_plane] );

        if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::STRIP_SPACING ) >= TMutDatabaseCntrl::MAX && (i_plane == MUTGEOM::Cathode1 || i_plane == MUTGEOM::Cathode2) )
        { plane->printStripSpacing(); }

        if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::STRIP_GEOMETRY ) >= TMutDatabaseCntrl::MAX && (i_plane == MUTGEOM::Cathode1 || i_plane == MUTGEOM::Cathode2) )
        { plane->print(); }

        if( TMutDatabaseCntrl::get_verbosity( TMutDatabaseCntrl::WIRE_GEOMETRY ) >= TMutDatabaseCntrl::MAX && i_plane == MUTGEOM::Wire )
        { plane->print(); }

      }
    }
  }

  cout << endl;
  MutCalibSingleton::get().initialize();

  return true;
}
