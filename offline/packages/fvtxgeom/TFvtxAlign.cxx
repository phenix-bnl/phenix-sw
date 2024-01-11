// $Id: TFvtxAlign.cxx,v 1.9 2015/08/05 21:15:24 jinhuang Exp $

/*!
 \file TFvtxAlign.cxx
 \brief Statically scoped class for fvtx alignment correctons from file
 \author Zhengyun You
 \version $Revision: 1.9 $
 \date $Date: 2015/08/05 21:15:24 $
 */

#include <algorithm>
#include <fstream>
#include <sstream>
#include <stdexcept>
#include <set>

#include "FvtxGeom.h"
#include "FvtxStrip.h"
#include "TFvtxAlign.h"

using namespace std;
//using namespace FVTXGEOM;

//________________________________________________________
const string TFvtxAlign::STATION = "station";
const string TFvtxAlign::WEDGE = "wedge";

TFvtxAlign::station_parameter_set TFvtxAlign::_station_parameters;
TFvtxAlign::wedge_parameter_set TFvtxAlign::_wedge_parameters;

//________________________________________________________
void
TFvtxAlign::init(const string& filename)
{
  // open file
  ifstream in(filename.c_str());

  if (!in)
    {
      cout << "TFvtxAlign::init - unable to open file " << filename << endl;
      return;
    }
  cout << "TFvtxAlign::init - misalignments loaded, file " << filename << endl;

  char line[512];
  while (!(in.rdstate() & ios::failbit))
    {
      in.getline(line, 512, '\n');
      if (!strlen(line))
        continue;
      if (strncmp(line, "//", 2) == 0)
        continue;

      istringstream line_stream(line);

      // read tag to decide if parameter set is from station wedge
      string tag;
      line_stream >> tag;
      if (line_stream.rdstate() & ios::failbit)
        continue;

      if (tag == STATION)
        {

          StationParameters par;
          line_stream >> par;
          if (line_stream.rdstate() & ios::failbit)
            continue;

          // check if alignment parameters already exists
          station_parameter_set::iterator station_iter =
              _station_parameters.find(par);
          if (station_iter != _station_parameters.end())
            {
              par._delta_x += station_iter->_delta_x;
              par._delta_y += station_iter->_delta_y;
              par._delta_z += station_iter->_delta_z;
              par._delta_phi += station_iter->_delta_phi;
              par._delta_psix += station_iter->_delta_psix;
              par._delta_psiy += station_iter->_delta_psiy;
              _station_parameters.erase(par);
              _station_parameters.insert(par);
            }
          // insert new parameter set
          else
            _station_parameters.insert(par);

        }
      else if (tag == WEDGE)
        {

          WedgeParameters par;
          line_stream >> par;
          if (line_stream.rdstate() & ios::failbit)
            continue;

          // check if alignment parameters already exists
          wedge_parameter_set::iterator wedge_iter = _wedge_parameters.find(
              par);
          if (wedge_iter != _wedge_parameters.end())
            {
              par._delta_x += wedge_iter->_delta_x;
              par._delta_y += wedge_iter->_delta_y;
              par._delta_z += wedge_iter->_delta_z;
              par._delta_phi += wedge_iter->_delta_phi;
              _wedge_parameters.erase(par);
              _wedge_parameters.insert(par);
            }
          // insert new parameter set
          else
            _wedge_parameters.insert(par);

        }
    }

}
;
//________________________________________________________
void
TFvtxAlign::update_geometry()
{

  const bool bloadgeom = FvtxGeom::is_geometry_loaded();

  if (bloadgeom)
    {
      cout << "TFvtxAlign::update_geometry - "
          << "Fvtx geometry already loaded, will not update again. " << endl
          << "WARNING: TFvtxAlign do not support replay two runs in the same FUN4ALL cycle!"
          << endl;
      return;
    }
  else
    {
      cout << "TFvtxAlign::update_geometry - "
          << "Successful initial call of FvtxGeom::load_geometry()" << endl;
    }

  FvtxGeom::create_arms();

  // preserve gGeoManager, which will be overwritten during loading
  FvtxGeom::UseFvtxGeomManager _use;

  // loop over station parameters
  for (station_parameter_set::iterator iter = _station_parameters.begin();
      iter != _station_parameters.end(); iter++)
    {
      TGeoNode *node_station = FvtxGeom::get_node_station(iter->_arm,
          iter->_cage, iter->_station);
      TGeoPhysicalNode *phy_cage = FvtxGeom::get_phy_cage(iter->_arm,
          iter->_cage);
      TGeoPhysicalNode *phy_station = FvtxGeom::get_phy_station(iter->_arm,
          iter->_cage, iter->_station);

      if (!node_station)
        {
          stringstream s;
          s << "TFvtxAlign::update_geometry, node_station(" << iter->_arm
              << ", " << iter->_cage << ", " << iter->_station
              << ") not found ";

          std::cerr << s.str() << std::endl;

          throw runtime_error(s.str());
        }
      else
        std::cout << "TFvtxAlign::update_geometry, node_station(" << iter->_arm
            << ", " << iter->_cage << ", " << iter->_station << ") found "
            << std::endl;

      TGeoMatrix *mat_station = node_station->GetMatrix();
      std::cout << "before update " << std::endl;
      mat_station->Print();

      const double delta_at_Hall[3] =
        { iter->_delta_x, iter->_delta_y, iter->_delta_z };
      double delta_at_cage[3] =
        { 0,0,0 };

      if (phy_cage)
        phy_cage->GetMatrix(-1)->MasterToLocalVect(delta_at_Hall,
            &delta_at_cage[0]);
      else
      {
          static bool once = true;
          if (once)
            {
              once = false;
              std::cout <<"TFvtxAlign::update_geometry - WARNIGN - "
                  <<"TGeoPhysicalNode for Cage is missing. This is normal if you are using new format of the FVTX Geometry objects"<<std::endl;
            }
      }

      const Double_t *translation = mat_station->GetTranslation();
      mat_station->SetDx(translation[0] + delta_at_cage[0]);
      mat_station->SetDy(translation[1] + delta_at_cage[1]);
      mat_station->SetDz(translation[2] + delta_at_cage[2]);
      mat_station->RotateZ(iter->_delta_phi);
      mat_station->RotateX(iter->_delta_psix);
      mat_station->RotateY(iter->_delta_psiy);

      phy_station->Refresh();

      std::cout << "after update " << std::endl;
      mat_station->Print();

    }

  // loop over wedge parameters
  for (wedge_parameter_set::iterator iter = _wedge_parameters.begin();
      iter != _wedge_parameters.end(); iter++)
    {
      TGeoNode *node_wedge = FvtxGeom::get_node_sector(iter->_arm, iter->_cage,
          iter->_station, iter->_sector);

      TGeoPhysicalNode *phy_station = NULL;
      TGeoPhysicalNode *phy_wedge = NULL;
      try
        {
          phy_station = FvtxGeom::get_phy_station(iter->_arm, iter->_cage,
              iter->_station);
          phy_wedge = FvtxGeom::get_phy_sector(iter->_arm, iter->_cage,
              iter->_station, iter->_sector);
        }
      catch (std::runtime_error &e)
        {
          std::cerr << "TFvtxAlign::update_geometry, error " << e.what()
              << " @ get_phy_station/sector(" << iter->_arm << ", "
              << iter->_cage << ", " << iter->_station << ") not found "
              << std::endl;
        }

      if (!node_wedge)
        std::cerr << "TFvtxAlign::update_geometry, node_wedge(" << iter->_arm
            << ", " << iter->_cage << ", " << iter->_station << ", "
            << iter->_sector << ") not found " << std::endl;
      else
        std::cout << "TFvtxAlign::update_geometry, node_wedge(" << iter->_arm
            << ", " << iter->_cage << ", " << iter->_station << ", "
            << iter->_sector << ") found " << std::endl;

      TGeoMatrix *mat_wedge = node_wedge->GetMatrix();
      std::cout << "before update " << std::endl;
      mat_wedge->Print();

      const double delta_at_Hall[3] =
        { iter->_delta_x, iter->_delta_y, iter->_delta_z };
      double delta_at_station[3] =
        { 0 };

      assert(phy_station);

      phy_station->GetMatrix(-1)->MasterToLocalVect(delta_at_Hall,
          &delta_at_station[0]);

      const Double_t *translation = mat_wedge->GetTranslation();
      mat_wedge->SetDx(translation[0] + delta_at_station[0]);
      mat_wedge->SetDy(translation[1] + delta_at_station[1]);
      mat_wedge->SetDz(translation[2] + delta_at_station[2]);
      mat_wedge->RotateZ(iter->_delta_phi);

      phy_wedge->Refresh();

      std::cout << "after update " << std::endl;
      mat_wedge->Print();

      /*
       TGeoPhysicalNode *phy_wedge = FvtxGeom::get_phy_radius( iter->_arm, iter->_station, iter->_sector, iter->_plane, 0);
       if ( !phy_wedge )
       std::cerr << "TFvtxAlign::update_geometry, phy_wedge(" << iter->_arm << ", " << iter->_station << ", " << iter->_sector << ", " << iter->_plane << ") not found " << std::endl;
       else
       std::cout << "TFvtxAlign::update_geometry, phy_wedge(" << iter->_arm << ", " << iter->_station << ", " << iter->_sector << ", " << iter->_plane << ") found " << std::endl;

       TGeoMatrix *mat_wedge = phy_wedge->GetOriginalMatrix();
       std::cout << phy_wedge->GetName() << " before update " << std::endl;
       mat_wedge->Print();
       std::cout << "global " << std::endl;
       phy_wedge->GetMatrix()->Print();

       TGeoMatrix *new_mat_wedge = mat_wedge->MakeClone();
       const Double_t *translation = new_mat_wedge->GetTranslation();
       new_mat_wedge->SetDx( translation[0] + iter->_delta_x );
       new_mat_wedge->SetDy( translation[1] + iter->_delta_y );
       new_mat_wedge->SetDz( translation[2] + iter->_delta_z );
       new_mat_wedge->RotateZ( iter->_delta_phi );
       phy_wedge->Align( new_mat_wedge );

       std::cout << "after update " << std::endl;
       phy_wedge->GetNode()->GetMatrix()->Print();
       std::cout << "global " << std::endl;
       phy_wedge->GetMatrix()->Print();

       for (int i_column = 0; i_column < FVTXGEOM::NumberOfColumns; i_column++)
       {

       TGeoPhysicalNode *phy_column = FvtxGeom::get_phy_column( iter->_arm, iter->_station, iter->_sector, iter->_plane, 0, i_column);
       if ( !phy_column )
       std::cerr << "TFvtxAlign::update_geometry, phy_column(" << iter->_arm << ", " << iter->_station << ", " << iter->_sector << ", " << iter->_plane << ", " << i_column << ") not found " << std::endl;
       else
       std::cout << "TFvtxAlign::update_geometry, phy_column(" << iter->_arm << ", " << iter->_station << ", " << iter->_sector << ", " << iter->_plane << ", " << i_column << ") found " << std::endl;

       TGeoMatrix *mat_column = phy_column->GetOriginalMatrix();
       std::cout << phy_column->GetName() << " before update " << std::endl;
       mat_column->Print();
       std::cout << "global " << std::endl;
       phy_column->GetMatrix()->Print();

       TGeoMatrix *new_mat_column = mat_column->MakeClone();
       const Double_t *translation = new_mat_column->GetTranslation();
       new_mat_column->SetDx( translation[0] + iter->_delta_x );
       new_mat_column->SetDy( translation[1] + iter->_delta_y );
       new_mat_column->SetDz( translation[2] + iter->_delta_z );
       new_mat_column->RotateZ( iter->_delta_phi );
       phy_column->Align( new_mat_column );

       std::cout << "after update " << std::endl;
       phy_column->GetNode()->GetMatrix()->Print();
       std::cout << "global " << std::endl;
       phy_column->GetMatrix()->Print();
       }
       */
      /*
       // retrieve matching arm
       FvtxArm *geometry = (iter->_arm == South) ? SouthArm():NorthArm();

       // retrieve plane number
       unsigned int plane_id = (iter->_wedge == 0 ) ? Wedge1:Wedge2;

       // loop over half sectors
       for( int half=0; half<NumberOfHalfSectors; half++ )
       {

       FvtxPlane *plane = geometry
       ->f_pFvtxStations[iter->_station]
       ->f_pFvtxSectors[iter->_sector]
       ->f_pFvtxHalfSectors[half]
       ->f_pFvtxPlanes[iter->_plane]
       ->f_pFvtxPlanes[plane_id];

       // create offset origin
       PHPoint offset_origin( iter->_delta_x, iter->_delta_y, 0 );

       // create rotated vectors
       // plane->rotate( iter->_delta_phi, 'z');

       PHVector rot_u( cos( iter->_delta_phi ), sin( iter->_delta_phi ), 0 );
       PHVector rot_v( -sin( iter->_delta_phi ), cos( iter->_delta_phi ), 0 );
       PHVector rot_w( 0, 0, 1 );

       // create PHFrames
       PHFrame source( offset_origin, rot_u, rot_v, rot_w );
       PHFrame dest;

       // move plane according to offset frame
       plane->transformToNewFrame( source, dest );


       }
       */
    } // loop over wedge parameters

  // Update globle geometry
  FvtxGeom::refresh_geometry();

//  for (int arm_id = 1; arm_id < 2; arm_id++)
//    {
//      FvtxArm *arm = FvtxGeom::get_arm(arm_id);
//      for (int cage_id = 1; cage_id < 2; cage_id++)
//        {
//          FvtxCage *cage = arm->get_cage(cage_id);
//          for (int station_id = 1; station_id < 2; station_id++)
//            {
//              FvtxStation *station = cage->get_station(station_id);
//              for (int sector_id = 0; sector_id < 2; sector_id++)
//                {
//                  FvtxSector *sector = station->get_sector(sector_id);
//                  for (int column_id = 0; column_id < 2; column_id++)
//                    {
//                      FvtxColumn *column = sector->get_column(column_id);
//
//                      std::cout << " arm " << arm_id << " cage " << cage_id
//                          << " station " << station_id << " sector "
//                          << sector_id << " column " << column_id << " z "
//                          << column->get_z() << " rmin "
//                          << column->get_inner_radius() << " rmax "
//                          << column->get_outer_radius() << " overlap "
//                          << column->get_overlap() << " phibegin "
//                          << column->get_phi_begin() << " phiend "
//                          << column->get_phi_end() << std::endl;
//                    }
//                }
//            }
//        }
//    }
//  std::cout << std::endl;
}

//________________________________________________________
void
TFvtxAlign::print_geometry(ostream& out)
{
  out << "TFvtxAlign::print_geometry" << endl;
  /*  for( unsigned int arm=0; arm < NumberOfArms; arm++ )
   for( unsigned int station=0; station < NumberOfStations; station++ )
   for( unsigned int sector=0; sector < NumberOfSectors; sector++ )
   for( unsigned int half=0; half < NumberOfHalfSectors; half++ )
   for( unsigned int plane=0; plane < NumberOfPlanes; plane++ )

   // reject station3 plane3 since it does not exist
   if( station != Station3 || plane != Plane3 )
   for( unsigned int wedge=0; wedge < NumberOfWedgePlanes; wedge++ )
   {

   // retrieve matching plane
   unsigned int plane_id = (wedge == 0 ) ? Wedge1:Wedge2;
   FvtxArm* geometry( (arm == South) ? SouthArm():NorthArm() );
   FvtxHalfSector *half_sector = geometry
   ->f_pFvtxStations[station]
   ->f_pFvtxSectors[sector]
   ->f_pFvtxHalfSectors[half];

   FvtxPlane *plane = half_sector
   ->f_pFvtxPlanes[plane]
   ->f_pFvtxPlanes[plane_id];

   // print plane global position and vector (rotations)
   // dump index
   out << "[" << arm << "," << station << "," << sector << "," << half << "," << plane << "," << wedge << "]";

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
   */
  out << "**" << endl;

}

//________________________________________________________
void
TFvtxAlign::convert_to_db(ostream& out)
{

  out << "TFvtxAlign::convert_to_db" << endl;
  /*
   // loop over arms
   for( int arm = 0; arm < MUTGEOM::NumberOfArms; arm++ )
   {

   // retrieve matching arm
   FvtxArm *geometry = (arm == South) ? SouthArm():NorthArm();

   // get list of constants
   FvtxArm::InternalAligConstList aligConsts( geometry->internalAligConsts() );

   // loop over wedge parameters
   for( wedge_parameter_set::iterator iter = _wedge_parameters.begin(); iter != _wedge_parameters.end(); iter++ )
   {

   // skipp if arm does not match
   if( iter->_arm != arm ) continue;

   // retrieve offsets
   PHPoint offset_origin( iter->_delta_x, iter->_delta_y, 0 );

   // retrieve plane number
   unsigned int plane_id = (iter->_wedge == 0 ) ? Wedge1:Wedge2;

   // loop over half sectors
   for( int half=0; half<NumberOfHalfSectors; half++ )
   {

   // retrieve half sector
   FvtxHalfSector* half_sector = geometry
   ->f_pFvtxStations[iter->_station]
   ->f_pFvtxSectors[iter->_sector]
   ->f_pFvtxHalfSectors[half];

   // retrieve plane
   FvtxPlane *plane = half_sector->f_pFvtxPlanes[iter->_plane]->f_pFvtxPlanes[plane_id];

   // retrieve plane center position
   PHPoint global_position_begin( plane->getGlobalPosition() );
   PHPoint global_position_end( global_position_begin );

   // translate
   global_position_end = global_position_end + offset_origin;

   // rotate by delta_phi around z in global frame
   PHMatrix rotation = PHGeometry::rotationMatrix( iter->_delta_phi, PHVector( 0,0,1 ) );
   global_position_end = rotation*global_position_end;

   //rotate back by -delta_phi around z in half sector frame.
   //Note that depending on the arm, the convention for phi is different
   //so that an additional sign is to be added to the delta_phi calculated in the global frame.
   PHFrame global_frame;
   PHFrame half_sector_frame = half_sector->getBodyCenteredFrame();
   global_position_end = FvtxGeomObject::rotateAndTranslate(global_frame, global_position_end, half_sector_frame);
   PHMatrix rotation_inv = (iter->_arm == South) ?
   PHGeometry::rotationMatrix( -iter->_delta_phi, PHVector( 0,0,1 ) ):
   PHGeometry::rotationMatrix( iter->_delta_phi, PHVector( 0,0,1 ) );
   global_position_end = rotation_inv*global_position_end;

   // convert back to global_frame
   global_position_end = FvtxGeomObject::rotateAndTranslate(half_sector_frame, global_position_end, global_frame);


   //local translation (accounting for both offset and rotation)
   //is the difference between the begin and end point
   PHPoint local_translation( global_position_end-global_position_begin );

   // convert translation to half sector reference frame
   local_translation = FvtxGeomObject::rotateAndTranslate(
   global_frame,
   local_translation + half_sector_frame.getOrigin(),
   half_sector_frame );

   // create InternalAligConst matching the obtained parameters
   FvtxArm::InternalAligConst alig;
   alig.arm = arm;
   alig.station = iter->_station;
   alig.sector = iter->_sector;
   alig.half = half;
   alig.plane = iter->_plane;
   alig.plane = plane_id;

   // retrieve translation
   alig.ds = local_translation.getX();
   alig.dr = local_translation.getY();
   alig.dz = local_translation.getZ();

   // retrieve rotation
   //Note that depending on the arm, the convention for phi is different.
   //so that an additional sign is to be added to the delta_phi calculated in the global frame.
   alig.roll = (iter->_arm == South) ?  iter->_delta_phi:-iter->_delta_phi;
   alig.pitch = 0;
   alig.yaw = 0;

   // print
   // out << alig;

   // find matching par in list and update
   FvtxArm::InternalAligConstList::iterator aligIter( std::find_if(
   aligConsts.begin(),
   aligConsts.end(),
   FvtxArm::SameDetectorFTor( alig ) ) );

   if( aligIter != aligConsts.end() )
   {

   aligIter->ds += alig.ds;
   aligIter->dr += alig.dr;
   aligIter->dz += alig.dz;

   aligIter->roll += alig.roll;
   aligIter->pitch += alig.pitch;
   aligIter->yaw += alig.yaw;

   } else {

   cout << "TFvtxAlign::convert_to_db - could not find " << alig.arm << alig.station << alig.sector << alig.half << alig.plane << alig.plane << endl;

   }

   }

   }

   // print all updated parameters to file
   for( FvtxArm::InternalAligConstList::const_iterator aligIter = aligConsts.begin(); aligIter != aligConsts.end(); ++aligIter )
   { out << *aligIter; }

   out << endl;

   }
   */
  out << "**" << endl;

}

//________________________________________________________
TFvtxAlign::StationParameters
TFvtxAlign::get_station_parameters(int arm, int cage, int station)
{
  StationParameters tmp(arm, cage, station);
  station_parameter_set::iterator iter(_station_parameters.find(tmp));
  return (iter == _station_parameters.end()) ? tmp : *iter;
}

//________________________________________________________
TFvtxAlign::WedgeParameters
TFvtxAlign::get_wedge_parameters(int arm, int cage, int station, int sector)
{
  WedgeParameters tmp(arm, cage, station, sector);
  wedge_parameter_set::iterator iter(_wedge_parameters.find(tmp));
  return (iter == _wedge_parameters.end()) ? tmp : *iter;
}

//________________________________________________________
void
TFvtxAlign::print_parameters(ostream& out)
{
  /*
   // do nothing if no parameters were loaded
   if( !( _wedge_parameters.size() || _anode_parameters.size() ) ) return;

   PRINT( out, "TFvtxAlign::print_parameters" );

   // print wedge parameters
   for( wedge_parameter_set::iterator iter = _wedge_parameters.begin(); iter != _wedge_parameters.end(); iter++ )
   {
   out << CATHODE << " " << (*iter) << " ";

   // retrieve matching arm
   FvtxArm *geometry = (iter->_arm == South) ? SouthArm():NorthArm();

   // retrieve plane_id
   unsigned int plane_id = (iter->_wedge == 0 ) ? Wedge1:Wedge2;

   for( int half=0; half<NumberOfHalfSectors; half++ )
   {
   // retrieve matching plane
   FvtxPlane *plane = geometry
   ->f_pFvtxStations[iter->_station]
   ->f_pFvtxSectors[iter->_sector]
   ->f_pFvtxHalfSectors[half]
   ->f_pFvtxPlanes[iter->_plane]
   ->f_pFvtxPlanes[plane_id];

   FvtxStrip *strip( plane->f_pFvtxStrips[0] );
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

   */
  out << "**" << endl;

}

