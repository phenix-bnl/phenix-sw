// $Id: TMutAlign.cxx,v 1.29 2014/10/01 22:18:16 jinhuang Exp $

/*!
  \file TMutAlign.cxx
  \brief Statically scoped class for muon tracker alignment correctons from file
  \author Hugo Pereira
  \version $Revision: 1.29 $
  \date $Date: 2014/10/01 22:18:16 $
*/

#include <algorithm>
#include <fstream>
#include <sstream>
#include <set>

#include "MutGeom.h"
#include "MutStrip.h"
#include "TMutAlign.h"

using namespace std;
using namespace MUTGEOM;

//________________________________________________________
const string TMutAlign::CATHODE = "cathode";
const string TMutAlign::ANODE = "anode";

TMutAlign::cathode_parameter_set TMutAlign::_cathode_parameters;
TMutAlign::anode_parameter_set TMutAlign::_anode_parameters;

//________________________________________________________
void TMutAlign::init( const string& filename )
{
  // open file
  ifstream in( filename.c_str() );

  if( !in )
  {
    cout << "TMutAlign::init - unable to open file " << filename << endl;
    return;
  }
  cout << "TMutAlign::init - misalignments loaded, file " << filename << endl;

  char line[512];
  while( !(in.rdstate() & ios::failbit ) )
  {
    in.getline( line, 512, '\n' );
    if( !strlen( line ) ) continue;
    if( strncmp( line, "//", 2 ) == 0 ) continue;

    istringstream line_stream( line );

    // read tag to decide if parameter set is from anode/cathode
    string tag;
    line_stream >> tag;
    if( line_stream.rdstate() & ios::failbit ) continue;

    if( tag == CATHODE )
    {

      CathodeParameters par;
      line_stream >> par;
      if(not par._valid) continue;

      // check if alignment parameters already exists
      cathode_parameter_set::iterator cathode_iter = _cathode_parameters.find( par );
      if( cathode_iter != _cathode_parameters.end() ) {
          par._delta_x += cathode_iter->_delta_x;
          par._delta_y += cathode_iter->_delta_y;
          par._delta_phi += cathode_iter->_delta_phi;
          for( int half=0; half<NumberOfHalfOctants; half++ )
          par._delta_phi_halfs[half] += cathode_iter->_delta_phi_halfs[half];
          _cathode_parameters.erase( par );
          _cathode_parameters.insert( par );
      }
        // insert new parameter set
      else _cathode_parameters.insert( par );

    } else if( tag == ANODE ) {

      AnodeParameters par;
      line_stream >> par;
      if( line_stream.rdstate() & ios::failbit ) continue;

      // check if alignment parameters already exists
        anode_parameter_set::iterator anode_iter = _anode_parameters.find( par );
      if(  anode_iter != _anode_parameters.end() ) {

          par._delta_z += anode_iter->_delta_z;
          _anode_parameters.erase( par );
          _anode_parameters.insert( par );

      }

      // insert new parameter set
      else _anode_parameters.insert( par );
    }
  }

};
//________________________________________________________
void TMutAlign::update_geometry()
{
  // loop over cathode parameters
  for( cathode_parameter_set::iterator iter = _cathode_parameters.begin(); iter != _cathode_parameters.end(); iter++ )
  {

    // retrieve matching arm
    MutArm *geometry = (iter->_arm == South) ? SouthArm():NorthArm();

    // retrieve plane number
    unsigned int plane_id = (iter->_cathode == 0 ) ? Cathode1:Cathode2;

    // loop over half octants
    for( int half=0; half<NumberOfHalfOctants; half++ )
    {

      MutPlane *plane = geometry
          ->f_pMutStations[iter->_station]
          ->f_pMutOctants[iter->_octant]
          ->f_pMutHalfOctants[half]
          ->f_pMutGaps[iter->_gap]
          ->f_pMutPlanes[plane_id];

      // create offset origin
      PHPoint offset_origin( iter->_delta_x, iter->_delta_y, 0 );

      // create rotated vectors
      // plane->rotate( iter->_delta_phi, 'z');

      PHVector rot_u( cos( iter->_delta_phi_halfs[half] ), sin( iter->_delta_phi_halfs[half] ), 0 );
      PHVector rot_v( -sin( iter->_delta_phi_halfs[half] ), cos( iter->_delta_phi_halfs[half] ), 0 );
      PHVector rot_w( 0, 0, 1 );

      // create PHFrames
      PHFrame source( offset_origin, rot_u, rot_v, rot_w );
      PHFrame dest;

      // move plane according to offset frame
      plane->transformToNewFrame( source, dest );


    }
  }

  // loop over anode parameters
  for( anode_parameter_set::iterator iter = _anode_parameters.begin(); iter != _anode_parameters.end(); iter++ )
  {
    // retrieve matching arm
    MutArm *geometry = (iter->_arm == South) ? SouthArm():NorthArm();

    // loop over half octants and planes (cathode and anodes)
    for( int half=0; half<NumberOfHalfOctants; half++ )
    for( int plane_id = 0; plane_id < NumberOfPlanes; plane_id++ )
    {

      // retrieve MutPlane
      MutPlane *plane = geometry
          ->f_pMutStations[iter->_station]
          ->f_pMutOctants[iter->_octant]
          ->f_pMutHalfOctants[half]
          ->f_pMutGaps[iter->_gap]
          ->f_pMutPlanes[plane_id];

      // create offset origin
      PHPoint offset_origin( 0, 0, iter->_delta_z );

      // create PHFrames
      PHFrame source;
      PHFrame dest;
      source.setOrigin( offset_origin );

      // move plane according to offset frame
      plane->transformToNewFrame( source, dest );

    }

  }
};

//________________________________________________________
void TMutAlign::print_geometry( ostream& out )
{
  PRINT( out, "TMutAlign::print_geometry" );
  for( unsigned int arm=0; arm < NumberOfArms; arm++ )
  for( unsigned int station=0; station < NumberOfStations; station++ )
  for( unsigned int octant=0; octant < NumberOfOctants; octant++ )
  for( unsigned int half=0; half < NumberOfHalfOctants; half++ )
  for( unsigned int gap=0; gap < NumberOfGaps; gap++ )

  // reject station3 gap3 since it does not exist
  if( station != Station3 || gap != Gap3 )
  for( unsigned int cathode=0; cathode < NumberOfCathodePlanes; cathode++ )
  {

    // retrieve matching plane
    unsigned int plane_id = (cathode == 0 ) ? Cathode1:Cathode2;
    MutArm* geometry( (arm == South) ? SouthArm():NorthArm() );
    MutHalfOctant *half_octant = geometry
        ->f_pMutStations[station]
        ->f_pMutOctants[octant]
        ->f_pMutHalfOctants[half];

    MutPlane *plane = half_octant
        ->f_pMutGaps[gap]
        ->f_pMutPlanes[plane_id];

    // print plane global position and vector (rotations)
    // dump index
    out << "[" << arm << "," << station << "," << octant << "," << half << "," << gap << "," << cathode << "]";

    // dump plane position
    char formated_out[512];
    sprintf( formated_out, "%7.4f, %7.4f, %7.4f",
      plane->getGlobalPosition().getX(),
      plane->getGlobalPosition().getY(),
      plane->getGlobalPosition().getZ() );
    out << " fGlobalPosition: (" << formated_out << ")";

    // dump plane vector
    sprintf( formated_out, "%7.4f, %7.4f, %7.4f",
      plane->getGlobalVector().getX(),
      plane->getGlobalVector().getY(),
      plane->getGlobalVector().getZ() );
    out << " fGlobalVector: (" << formated_out << ")";
    out << endl;
  }

  PRINT( out, "**" );

}

//________________________________________________________
void TMutAlign::convert_to_db( ostream& out )
{

  PRINT( out, "TMutAlign::convert_to_db" );

  // loop over arms
  for( int arm = 0; arm < MUTGEOM::NumberOfArms; arm++ )
  {

    // retrieve matching arm
    MutArm *geometry = (arm == South) ? SouthArm():NorthArm();

    // get list of constants
    MutArm::InternalAligConstList aligConsts( geometry->internalAligConsts() );

    // loop over cathode parameters
    for( cathode_parameter_set::iterator iter = _cathode_parameters.begin(); iter != _cathode_parameters.end(); iter++ )
    {

      // skipp if arm does not match
      if( iter->_arm != arm ) continue;

      // retrieve offsets
      PHPoint offset_origin( iter->_delta_x, iter->_delta_y, 0 );

      // retrieve plane number
      unsigned int plane_id = (iter->_cathode == 0 ) ? Cathode1:Cathode2;

      // loop over half octants
      for( int half=0; half<NumberOfHalfOctants; half++ )
      {

        // retrieve half octant
        MutHalfOctant* half_octant = geometry
          ->f_pMutStations[iter->_station]
          ->f_pMutOctants[iter->_octant]
          ->f_pMutHalfOctants[half];

        // retrieve plane
        MutPlane *plane = half_octant->f_pMutGaps[iter->_gap]->f_pMutPlanes[plane_id];

        // retrieve plane center position
        PHPoint global_position_begin( plane->getGlobalPosition() );
        PHPoint global_position_end( global_position_begin );

        // translate
        global_position_end = global_position_end + offset_origin;

        // rotate by delta_phi around z in global frame
        PHMatrix rotation = PHGeometry::rotationMatrix( iter->_delta_phi_halfs[half], PHVector( 0,0,1 ) );
        global_position_end = rotation*global_position_end;

        /*
        rotate back by -delta_phi around z in half octant frame.
        Note that depending on the arm, the convention for phi is different
        so that an additional sign is to be added to the delta_phi calculated in the global frame.
        */
        PHFrame global_frame;
        PHFrame half_octant_frame = half_octant->getBodyCenteredFrame();
        global_position_end = MutGeomObject::rotateAndTranslate(global_frame, global_position_end, half_octant_frame);
        PHMatrix rotation_inv = (iter->_arm == South) ?
          PHGeometry::rotationMatrix( -iter->_delta_phi_halfs[half], PHVector( 0,0,1 ) ):
          PHGeometry::rotationMatrix( iter->_delta_phi_halfs[half], PHVector( 0,0,1 ) );
        global_position_end = rotation_inv*global_position_end;

        // convert back to global_frame
        global_position_end = MutGeomObject::rotateAndTranslate(half_octant_frame, global_position_end, global_frame);

        /*
        local translation (accounting for both offset and rotation)
        is the difference between the begin and end point
        */
        PHPoint local_translation( global_position_end-global_position_begin );

        // convert translation to half octant reference frame
        local_translation = MutGeomObject::rotateAndTranslate(
          global_frame,
          local_translation + half_octant_frame.getOrigin(),
          half_octant_frame );

        // create InternalAligConst matching the obtained parameters
        MutArm::InternalAligConst alig;
        alig.arm = arm;
        alig.station = iter->_station;
        alig.octant = iter->_octant;
        alig.half = half;
        alig.gap = iter->_gap;
        alig.plane = plane_id;

        // retrieve translation
        alig.ds = local_translation.getX();
        alig.dr = local_translation.getY();
        alig.dz = local_translation.getZ();

        // retrieve rotation
        /*
        Note that depending on the arm, the convention for phi is different.
        so that an additional sign is to be added to the delta_phi calculated in the global frame.
        */
        alig.roll = (iter->_arm == South) ?  iter->_delta_phi_halfs[half]:-iter->_delta_phi_halfs[half];
        alig.pitch = 0;
        alig.yaw = 0;

        // print
        // out << alig;

        // find matching par in list and update
        MutArm::InternalAligConstList::iterator aligIter( std::find_if(
          aligConsts.begin(),
          aligConsts.end(),
          MutArm::SameDetectorFTor( alig ) ) );

        if( aligIter != aligConsts.end() )
        {

          aligIter->ds += alig.ds;
          aligIter->dr += alig.dr;
          aligIter->dz += alig.dz;

          aligIter->roll += alig.roll;
          aligIter->pitch += alig.pitch;
          aligIter->yaw += alig.yaw;

        } else {

          cout << "TMutAlign::convert_to_db - could not find " << alig.arm << alig.station << alig.octant << alig.half << alig.gap << alig.plane << endl;

        }

      }

    }

    // print all updated parameters to file
    for( MutArm::InternalAligConstList::const_iterator aligIter = aligConsts.begin(); aligIter != aligConsts.end(); ++aligIter )
    { out << *aligIter; }

    out << endl;

  }

  PRINT( out, "**" );

}

//________________________________________________________
TMutAlign::CathodeParameters TMutAlign::get_cathode_parameters( int arm, int station, int octant, int gap, int cathode )
{
  CathodeParameters tmp( arm, station, octant, gap, cathode );
  cathode_parameter_set::iterator iter( _cathode_parameters.find( tmp ) );
  return (iter == _cathode_parameters.end() ) ? tmp:*iter;
}

//________________________________________________________
TMutAlign::AnodeParameters TMutAlign::get_anode_parameters( int arm, int station, int octant, int gap )
{
  AnodeParameters tmp( arm, station, octant, gap );
  anode_parameter_set::iterator iter( _anode_parameters.find( tmp ) );
  return (iter == _anode_parameters.end() ) ? tmp:*iter;
}

//________________________________________________________
void TMutAlign::print_parameters( ostream& out )
{


  PRINT( out, "TMutAlign::print_parameters" );

  // do nothing if no parameters were loaded
  if( !( _cathode_parameters.size() || _anode_parameters.size() ) )
    out <<"do nothing if no parameters were loaded"<<endl;

  // print cathode parameters
  for( cathode_parameter_set::iterator iter = _cathode_parameters.begin(); iter != _cathode_parameters.end(); iter++ )
  {
    out << CATHODE << " " << (*iter) << " ";

    // retrieve matching arm
    MutArm *geometry = (iter->_arm == South) ? SouthArm():NorthArm();

    // retrieve plane_id
    unsigned int plane_id = (iter->_cathode == 0 ) ? Cathode1:Cathode2;

    for( int half=0; half<NumberOfHalfOctants; half++ )
    {
      // retrieve matching plane
      MutPlane *plane = geometry
        ->f_pMutStations[iter->_station]
        ->f_pMutOctants[iter->_octant]
        ->f_pMutHalfOctants[half]
        ->f_pMutGaps[iter->_gap]
        ->f_pMutPlanes[plane_id];

      MutStrip *strip( plane->f_pMutStrips[0] );
      double angle = strip->getAngle();
      angle = (angle < -M_PI_2) ? angle + M_PI : angle;
      angle = (angle > M_PI_2) ? angle - M_PI : angle;

      // print delta_w alignment corrections
      out
        << (half==0 ? "delta_w1=":"delta_w2=" )
        << -sin( angle )*iter->_delta_x+cos( angle )*iter->_delta_y;
      out << "cm ";
    }
    out << endl;
  }

  out << endl;

  // print anode parameters
  for( anode_parameter_set::iterator iter = _anode_parameters.begin(); iter != _anode_parameters.end(); iter++ )
  out << ANODE << " " << (*iter) << endl;
  out << endl;


  PRINT( out, "**" );

}

