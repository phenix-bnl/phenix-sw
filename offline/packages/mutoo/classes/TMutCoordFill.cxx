//////////////////////////////////////////////////////////////////
/*
  \file TMutCoordFill.cxx
  \author: M. Brooks
  \version $Revision: 1.5 $
  \date    $Date: 2011/12/24 04:48:21 $
  \brief 
    Takes centroid values from a Cluster object and creates corresponding
    Coord objects.
*/
//////////////////////////////////////////////////////////////////

#include<PHGeometry.h>
#include <MUTOO_FEM.h>
#include <PHException.h>
#include <TMutGeo.h>

#include <MutStrip.h>

#include "TMutCoordFill.h"

// PHENIX headers

// STL
#include <iostream>

using namespace std;

// static variables initialization
MUTOO::Verbosity TMutCoordFill::_verbosity = MUTOO::NONE;

//________________________________________________________________
list<TMutCoordMap::pointer> TMutCoordFill::create_coords( TMutClusMap::pointer clus_ptr, TMutCoordMap* coord_map )
{

  list< TMutCoordMap::pointer > out;

  // Loop over centroids
  for(size_t index = 0; index < clus_ptr->get()->get_n_centroid(); index++ ){

    const TMutClusCentroid *centroid( clus_ptr->get()->get_centroid( index ) );
    if( !centroid ) continue;

    // Create a new TMutCoord with same location
    // as cluster.
    TMutCoordMap::iterator coord_iter = coord_map->insert_new(clus_ptr->get()->get_location());

    // Get centroid offset and peak strip
    double offset = centroid->get_w();
    unsigned short peak_strip = centroid->get_peak_strip();

    // Get the geometry object for the peak strip
    const MutStrip* strip_geo = TMutGeo::get_strip_geom(clus_ptr->get()->get_location(), peak_strip);

    if(!strip_geo){
      MUTOO::TRACE("mMutFitClus::fill_coord_map: geometry returns null");
      continue;
    }

    // Here we calculate a line offset from the peak
    // strip in the direction normal to the strip.
    // We approximate the z of the strip to be a
    // constant along its length.
    PHPoint begin = strip_geo->getGlobalPositionBegin();
    PHPoint end = strip_geo->getGlobalPositionEnd();

    // At this point we don't know which anode is associated with this TMutCoord so
    // we average use the global anode plane position to determine the z coord.
    unsigned short arm = coord_iter->get()->get_arm();
    unsigned short station = coord_iter->get()->get_station();
    unsigned short octant = coord_iter->get()->get_octant();
    unsigned short half_octant = coord_iter->get()->get_half_octant();
    unsigned short gap = coord_iter->get()->get_gap();

    double global_z = TMutGeo::get_anode_plane_position(arm,
                                                        station,
                                                        octant,
                                                        half_octant,
                                                        gap).getZ();
    begin.setZ(global_z);
    end.setZ(global_z);

    // Coordinate is PHLine defined by
    // strip endpoints + offset in direction normal to strip axis:
    // offset is positive towards increasing strip number
    // i.e. increasing phi
    // i.e. towards the direction given by
    //      the strip vector rotated by +90 degrees,
    //      since origin of strip vector is at the smaller radius
    double stripLength = PHGeometry::distancePointToPoint(begin,end);
    double dx = - offset * (end.getY() - begin.getY()) / stripLength;
    double dy = offset * (end.getX() - begin.getX()) / stripLength;
    begin.setX(begin.getX() + dx);
    end.setX(end.getX() + dx);
    begin.setY(begin.getY() + dy);
    end.setY(end.getY() + dy);
    PHLine coord(begin,end);
    coord_iter->get()->set_coord(coord);

    // Cluster centroid data copied to TMutCoord
    coord_iter->get()->set_q_tot(centroid->get_q_tot());
    coord_iter->get()->set_q_peak(centroid->get_q_peak());
    coord_iter->get()->set_peak_strip(peak_strip);
    coord_iter->get()->set_error(centroid->get_w_error());
    coord_iter->get()->set_status(clus_ptr->get()->get_status());
    coord_iter->get()->set_q_error(centroid->get_q_tot_error());

    // W-axis offset copied to coord
    coord_iter->get()->set_w(offset);

    // Cos angle between strip and anode for (effects resolution)
    double angle = TMutGeo::get_angle_cathode_anode(coord_iter->get()->get_location());
    double cos_th = cos(TMutGeo::get_angle_cathode_anode(coord_iter->get()->get_location()));
    coord_iter->get()->set_cos_theta_wire(cos_th);

    // Set the stereo plane bit
    if(MUTOO::RAD_TO_DEG*angle < 89 || MUTOO::RAD_TO_DEG*angle > 91)
      coord_iter->get()->set_stereo();

    // Associate the TMutCoord with TMutClus
    PHKey::associate(coord_iter.current(),clus_ptr);
    
    // add TMutCoord to output list
    out.push_back( coord_iter.current() );
    
  }

  return out;  

}

