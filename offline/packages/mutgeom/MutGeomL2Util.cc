// $Id: MutGeomL2Util.cc,v 1.4 2009/06/13 01:24:37 hpereira Exp $

//////////////////////////////////////////////////////////////
/*! 
	\file MutGeomL2Util.C
	\brief level2 utility to dump calibration text files from postgres
	\author	Hugo Pereira
	\version $Revision: 1.4 $
	\date $Date: 2009/06/13 01:24:37 $
*/
//////////////////////////////////////////////////////////////

#include <iostream>
#include <fstream>
#include <set>

#include "MutCalib.h"
#include "MutGeom.h"
#include "MutGeomL2Util.h"
#include "MutStrip.h"

using namespace std;
using namespace MUTGEOM;

//_______________________________________________
string MutGeomL2Util::_calib_file = "L2MutrCalibDBPrimitives.adb";
string MutGeomL2Util::_geom_file = "L2MutrGeomDBPrimitives.adb";

//_________________________________________________
bool MutGeomL2Util::dump_calib( )
{

  // initialize calibration list  
  MutCalibStrip* calibration = MutCalib();  
  
  // initialize arms
  SouthArm();
  NorthArm();
  
  // prepare calibration structure storage
  set< Lvl2Calib > calibs;
  
  // loop over all strips
  for( unsigned int arm=0; arm<2; arm++ ) {
    MutArm* arm_ptr( (arm==0) ? SouthArm():NorthArm() );
    for( unsigned int station = 0; station < 3; station++ ) 
    for( unsigned int octant = 0; octant < 8; octant++ )
    for( unsigned int half = 0; half<2; half++ ) {
      unsigned int n_gaps( arm_ptr->f_pMutStations[station]
        ->f_pMutOctants[octant]
        ->f_pMutHalfOctants[half]->getNumberOfGaps() );
      for( unsigned int gap = 0; gap < n_gaps; gap++ )
      for( unsigned int cathode = 0; cathode < 2; cathode++ ) { 
        unsigned int cathode_id( ( cathode == 0 ) ? Cathode1:Cathode2 );
        unsigned int n_strip( arm_ptr->f_pMutStations[station]
          ->f_pMutOctants[octant]
          ->f_pMutHalfOctants[half]
          ->f_pMutGaps[gap]
          ->f_pMutPlanes[cathode_id]->f_pMutStrips.size( ) );
        for( unsigned int strip = 0; strip < n_strip; strip++ ) {
            
          // retrieve packet number and dcm channel
          unsigned int channel( arm_ptr->f_pMutStations[station]
            ->f_pMutOctants[octant]
            ->f_pMutHalfOctants[half]
            ->f_pMutGaps[gap]
            ->f_pMutPlanes[cathode_id]
            ->f_pMutStrips[strip]->getDCMChannel() );
          
          unsigned int packet( arm_ptr->f_pMutStations[station]
            ->f_pMutOctants[octant]
            ->f_pMutHalfOctants[half]
            ->f_pMutGaps[gap]
            ->f_pMutPlanes[cathode_id]
            ->f_pMutStrips[strip]->getPacket_ID() );       
         
          // retrieve pedestal and gain
          const PdbMutCalibStrip *strip_calib( calibration->getPdbMutCalibStrip( arm, station, octant, half, gap, cathode, strip ) );
          
          if( !strip_calib ) {
            cerr << "MutGeomL2Util::dump_calib - invalid strip calibration.\n";
            continue;
          }        
          
          float gain( strip_calib->get_gain() );
          float pedestal( strip_calib->get_pedestal() );
          
          Lvl2Calib calib_st( packet, channel, gain, pedestal );
          if( calibs.find( calib_st ) != calibs.end() ) {
            cerr << "MutGeomL2Util::dump_calib - packet (" << packet << "," << channel << ") already in list.\n";
            continue;
          }
          
          calibs.insert( calib_st  );
          
        } // strip loop
      }   // gap/cathode loop
    }     // station/octant/half loop
  }       // arm loop  
  
  // dump everything to _calib_file
  ofstream out( _calib_file.c_str() );
  if( !out ) {
    cerr << "MutGeomL2Util::dump_calib - cannot write to file \"" << _calib_file << "\".\n";
    return false;
  }
  
  for( set<Lvl2Calib>::iterator It = calibs.begin(); It != calibs.end(); It++ ) out << *It;
  out.close();
  
  cout << "MutGeomL2Util::dump_calib - " << calibs.size() << " calibration entries written to file \"" << _calib_file << "\".\n";
  
  return true;
}
  
//_________________________________________________
bool MutGeomL2Util::dump_geom( void )
{  
  // initialize arms
  SouthArm();
  NorthArm();
  
  // prepare geometry structure storage
  set< Lvl2Geom > geoms;
  
  // loop over all strips
  for( unsigned int arm=0; arm<2; arm++ ) {
    MutArm* arm_ptr( (arm==0) ? SouthArm():NorthArm() );
    for( unsigned int station = 0; station < 3; station++ ) 
    for( unsigned int octant = 0; octant < 8; octant++ )
    for( unsigned int half = 0; half<2; half++ ) {
      unsigned int n_gaps( arm_ptr->f_pMutStations[station]
        ->f_pMutOctants[octant]
        ->f_pMutHalfOctants[half]->getNumberOfGaps() );
      for( unsigned int gap = 0; gap < n_gaps; gap++ )
      for( unsigned int cathode = 0; cathode < 2; cathode++ ) { 
        unsigned int cathode_id( ( cathode == 0 ) ? Cathode1:Cathode2 );
        unsigned int n_strip( arm_ptr->f_pMutStations[station]
          ->f_pMutOctants[octant]
          ->f_pMutHalfOctants[half]
          ->f_pMutGaps[gap]
          ->f_pMutPlanes[cathode_id]->f_pMutStrips.size( ) );
        for( unsigned int strip = 0; strip < n_strip; strip++ ) {
                    
          Lvl2Geom geom_st;
          geom_st._packet = arm_ptr->f_pMutStations[station]
            ->f_pMutOctants[octant]
            ->f_pMutHalfOctants[half]
            ->f_pMutGaps[gap]
            ->f_pMutPlanes[cathode_id]
            ->f_pMutStrips[strip]->getPacket_ID();
            
          geom_st._dcm_channel = arm_ptr->f_pMutStations[station]
            ->f_pMutOctants[octant]
            ->f_pMutHalfOctants[half]
            ->f_pMutGaps[gap]
            ->f_pMutPlanes[cathode_id]
            ->f_pMutStrips[strip]->getDCMChannel();
          
          geom_st._arm          = arm;
          geom_st._station      = station;
          geom_st._half_octant  = octant*2+half;
          geom_st._gap          = gap;
          geom_st._cathode      = cathode;
          geom_st._strip        = strip;
          geom_st._first_point = arm_ptr->f_pMutStations[station]
            ->f_pMutOctants[octant]
            ->f_pMutHalfOctants[half]
            ->f_pMutGaps[gap]
            ->f_pMutPlanes[cathode_id]
            ->f_pMutStrips[strip]->getGlobalPositionBegin();
          geom_st._second_point = arm_ptr->f_pMutStations[station]
            ->f_pMutOctants[octant]
            ->f_pMutHalfOctants[half]
            ->f_pMutGaps[gap]
            ->f_pMutPlanes[cathode_id]
            ->f_pMutStrips[strip]->getGlobalPositionEnd();
            
          if( geoms.find( geom_st ) != geoms.end() ) {
            cerr << "MutGeomL2Util::dump_geom - packet (" << geom_st._packet << "," << geom_st._dcm_channel << ") already in list.\n";
            continue;
          }
          
          geoms.insert( geom_st  );
          
        } // strip loop
      }   // gap/cathode loop
    }     // station/octant/half loop
  }       // arm loop  
  
  // dump everything to file
  ofstream out( _geom_file.c_str() );
  if( !out ) {
    cerr << "MutGeomL2Util::dump_geom - cannot write to file \"" << _geom_file << "\".\n";
    return false;
  }
  
  for( set<Lvl2Geom>::iterator It = geoms.begin(); It != geoms.end(); It++ ) out << *It;
  out.close();
  
  cout << "MutGeomL2Util::dump_geom - " << geoms.size() << " geom entries written to file \"" << _geom_file << "\".\n";
  
  return true;
}
