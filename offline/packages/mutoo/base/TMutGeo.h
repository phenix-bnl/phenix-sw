// $Id: TMutGeo.h,v 1.7 2011/12/24 04:48:19 slash Exp $
//////////////////////////////////////////////////////////////////
/*
	\file TMutGeo.h
	\brief Utility class to wrap up some of the pointer indirection in using the geometry classes
	\author S.Kelly 
	\version $Revision: 1.7 $
	\date    $Date: 2011/12/24 04:48:19 $
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMUTGEO_H__
#define __TMUTGEO_H__

#include <set>

#include<TDataType.h>
#include<MUTOO.h>
#include<MutGeom.h>
#include<MutArm.h>
#include<MutStation.h>
#include<MutPlane.h>

class MutStrip;
class MutWire;
class PHPoint;
class PHVector;

/*! \ingroup classes */
//! Geometry tree utility class
/*! 
Class that hides some of the pointer indirection in using 
the MUT geometry classes.  In addition commonly used geometry
calculations are encapsulated here.
*/

class TMutGeo 
{

 public:

  //! Get position of survey point for specified cathode plane 
  static PHPoint get_cathode_plane_position(unsigned short arm,
					    unsigned short station,
					    unsigned short octant,
					    unsigned short half_octant,
					    unsigned short gap,
					    unsigned short cathode);

  //! Get position of survey point for specified anode plane 
  static PHPoint get_anode_plane_position(unsigned short arm,
					  unsigned short station,
					  unsigned short octant,
					  unsigned short half_octant,
					  unsigned short gap);
  
  //! Get unit vecto in direction of anode wire 
  static PHVector get_anode_direction(unsigned short arm, 
				      unsigned short station, 
				      unsigned short octant, 
				      unsigned short half_octant, 
				      unsigned short gap, 
				      unsigned short wire);
  
  //! MutWire object at specified location 
  static  MutWire* get_wire_geom(const MUTOO::gap_locator& location , const unsigned short& wire)
  {
    return get_wire_geom( 
      location.get<0>(), 
      location.get<1>(), 
      location.get<2>(), 
      location.get<3>(), 
      location.get<4>(),
      wire );
  }

  //! MutWire object at specified location 
  static  MutWire* get_wire_geom(
    const unsigned short& arm,
    const unsigned short& station,
    const unsigned short& octant,
    const unsigned short& half_octant,
    const unsigned short& gap,
    const unsigned short& wire);
  
  //! set of disabled anode cards 
  static const std::set<int>& get_disabled_anode_cards(const MUTOO::gap_locator& location )
  { 
    return get_disabled_anode_cards( 
      location.get<0>(),
      location.get<1>(),
      location.get<2>(),
      location.get<3>(),
      location.get<4>() );
  }
  
  //! set of disabled anode cards 
  static const std::set<int>& get_disabled_anode_cards(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant,
    unsigned short gap);

  //! Get MutStrip object at specified location 
  static  MutStrip* get_strip_geom( const MUTOO::cathode_locator& location, const unsigned short& strip)
  { 
    return get_strip_geom(
      location.get<0>(),
      location.get<1>(),
      location.get<2>(),
      location.get<3>(),
      location.get<4>(),
      location.get<5>(),
      strip );
  }
  
  //! Get MutStrip object at specified location 
  static  MutStrip* get_strip_geom(
    const unsigned short& arm,
    const unsigned short& station,
    const unsigned short& octant,
    const unsigned short& half_octant,
    const unsigned short& gap,
    const unsigned short& cathode,
    const unsigned short& strip);
  

  //! Get normalized tangent to strip 
  static  PHVector get_strip_direction(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant,
    unsigned short gap,
    unsigned short cathode,
    unsigned short strip);

  //! Get normalized tangent to strip 
  static  PHVector get_strip_direction( const MUTOO::cathode_locator&, unsigned short strip);
  
  /*! \brief
  Project given point onto axis of cathode strip and return distance from
  projection to endpoint of strip 
  */
  static double get_point_cathode_rdist(
    const PHPoint& point,
    const MUTOO::cathode_locator&,
    unsigned short strip);					
  
  //! Returns the axis normal to strip axis and PHENIX z-axis 
  static PHVector get_w_axis(const MUTOO::cathode_locator&,
			     unsigned short strip);					
  
  //! Returns the axis normal first strip in first gap and PHENIX z-axis 
  static PHVector get_w_axis(unsigned short arm,
			     unsigned short station,
			     unsigned short octant,
			     unsigned short half_octant);
			    			  
  //! Get number of gaps associated with given station 
  static unsigned short get_n_gaps(unsigned short arm, unsigned short station);

  //! Check if cathodes in given locations are parallel 
  static bool are_parallel(const MUTOO::cathode_locator&,
				    const MUTOO::cathode_locator&);

  //! Check to see if given point is in fiducial volume 
  static bool in_fiducial(unsigned short arm,
				   unsigned short station,
				   unsigned short octant,
				   unsigned short half_octant,
				   const PHPoint& point);
  
  //! Check to see if given point is in fiducial volume 
  static bool in_phi_fiducial(unsigned short arm,
				       unsigned short octant,
				       const PHPoint& point);
  
  //! Get the cathode angle in absolute coords [-pi/2, pi/2] 
  static double get_cathode_angle(unsigned short arm, 
					   unsigned short station,
					   unsigned short octant, 
					   unsigned short half_octant,
					   unsigned short gap, 
					   unsigned short cathode,
					   unsigned short strip);

  //! Get the angle between the cathode strip and the anode wire at specified location 
  static double get_angle_cathode_anode(unsigned short arm,
					unsigned short station,
					unsigned short octant,
					unsigned short half_octant,
					unsigned short gap,
					unsigned short cathode);
  
  //! Get the angle between the anode wire and cathode strip at specified location 
  static double get_angle_cathode_anode(const MUTOO::cathode_locator& location);

  /*! \brief 
    Returns anode number and dca for anode closest to specified point in a std::pair.  If
    no wire is found the returned wire number (pair.first) will be negative 
  */
  static std::pair<unsigned short,double> find_anode_dca(unsigned short arm, const PHPoint& point);
  
  //! Returns the number of strips in a cathode plane. 
  static unsigned short get_n_strip(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant,
    unsigned short gap,
    unsigned short cathod); 

  //! Returns the number of strips in a cathode plane. 
  static unsigned short get_n_strip( const MUTOO::cathode_locator& location )
  { 
    return get_n_strip( 
      location.get<0>(), 
      location.get<1>(), 
      location.get<2>(),
      location.get<3>(),
      location.get<4>(),
      location.get<5>() );
  }
  
  //! Returns pointer to anode closest to specified point. If no wire found returns null 
  static MutWire* find_nearest_anode(unsigned short arm, const PHPoint& point);
  private:
  
  static void test_invariant(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant,
    unsigned short gap,
    unsigned short cathode,
    unsigned short strip);
  
  static void test_invariant(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant,
    unsigned short gap,
    unsigned short cathode);
  
  static void test_invariant(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant,
    unsigned short gap);
  
  static void test_invariant(
    unsigned short arm,
    unsigned short station,
    unsigned short octant,
    unsigned short half_octant);

  static void test_invariant(
    unsigned short arm,
    unsigned short station);
};

#endif








