#ifndef __TMUTUNSTABLE_H__
#define __TMUTUNSTABLE_H__

#include "MUTGEOM.h"

#include <string>
#include <set>
#include <map>

class TMutUnstable
{
 public:
  
  static void set_hv_trip( std::string filename );
  
  static void set_unstable_strip( std::string filename );
  
  static bool use_hv_trip() { return _use_hv_trip; }
  
  static bool use_unstable_strip() { return _use_unstable_strip; }
  
  static void load_status( const float rnd_uniform );
  
  static bool check_hv_status( unsigned short arm,
			       unsigned short station,
			       unsigned short octant,
			       unsigned short gap,
			       unsigned short wire );
  
  static bool check_strip_status( unsigned short arm,
				  unsigned short station,
				  unsigned short octant,
				  unsigned short half_octant,
				  unsigned short gap,
				  unsigned short cathode,
				  unsigned short strip );
  
  static unsigned int strip_id( unsigned short arm,
				unsigned short station,
				unsigned short octant,
				unsigned short half_octant,
				unsigned short gap,
				unsigned short cathode,
				unsigned short strip );
  
 private:
  
  typedef std::set< std::string > SetHVTrip;
  typedef std::map< float, SetHVTrip > MapHVTrip;
  
  typedef std::set< unsigned int > SetStrip;
  typedef std::map< float, SetStrip > MapStrip;
  
  static bool _use_hv_trip;
  
  static bool _use_unstable_strip;
  
  static MapHVTrip _map_hv_trip[MUTGEOM::NumberOfArms];
  
  static SetHVTrip _trip_hv[MUTGEOM::NumberOfArms];
  
  static MapStrip _map_strip[MUTGEOM::NumberOfArms];
  
  static SetStrip _dead_strip[MUTGEOM::NumberOfArms];
  
};

#endif // __TMUTUNSTABLE_H__
