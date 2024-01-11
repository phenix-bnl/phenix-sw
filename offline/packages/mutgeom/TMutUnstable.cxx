#include <fstream>

#include "MutAnodeMap.h"
#include "TMutUnstable.h"

using namespace std;

bool TMutUnstable::_use_hv_trip = false;
bool TMutUnstable::_use_unstable_strip = false;

TMutUnstable::MapHVTrip TMutUnstable::_map_hv_trip[MUTGEOM::NumberOfArms];
TMutUnstable::SetHVTrip TMutUnstable::_trip_hv[MUTGEOM::NumberOfArms];
TMutUnstable::MapStrip TMutUnstable::_map_strip[MUTGEOM::NumberOfArms];
TMutUnstable::SetStrip TMutUnstable::_dead_strip[MUTGEOM::NumberOfArms];

//____________________________________________________________________
void TMutUnstable::set_hv_trip( string filename )
{
  for( int iarm=0; iarm<MUTGEOM::NumberOfArms; iarm++ )
    _map_hv_trip[iarm].clear();
  
  cout << "TMutUnstable::set_hv_trip - open hv trip file : "
       << filename << endl;
  
  ifstream infile( filename.c_str() );
  string str_line;
  int cur_arm = 0;
  while( getline( infile, str_line ) )
    {
      if( !str_line.size() ) continue;
      stringstream stream_line( str_line );
      
      float edge;
      stream_line >> edge;
      
      SetHVTrip tmp_set;
      string hv_name;
      while( stream_line >> hv_name ) tmp_set.insert( hv_name );
      
      std::pair<MapHVTrip::iterator,bool> retval =
	_map_hv_trip[cur_arm].insert( MapHVTrip::value_type( edge, tmp_set ) );
      
      if( !retval.second )
	{
	  cerr << "TMutUnstable::set_hv_trip - error when initializing,"
	       << " Edge : " << edge << ", Size : " << tmp_set.size() << endl;
	}
      
      if( edge == 1.0 ) cur_arm++;
    }
  infile.close();
  
  if( cur_arm != MUTGEOM::NumberOfArms )
    {
      cerr << "TMutUnstable::set_hv_trip - error, wrong text input file : "
	   << filename << endl;
    }
  
  cout << "TMutUnstable::set_hv_trip - # of patterns for south : "
       << _map_hv_trip[MUTGEOM::South].size() << endl
       << "                                              north : "
       << _map_hv_trip[MUTGEOM::North].size() << endl;
  
  _use_hv_trip = true;
  
}

//____________________________________________________________________
void TMutUnstable::set_unstable_strip( string filename )
{
  for( int iarm=0; iarm<MUTGEOM::NumberOfArms; iarm++ )
    _map_strip[iarm].clear();
  
  cout << "TMutUnstable::set_unstable_strip - open unstable strip file : "
       << filename << endl;
  
  ifstream infile( filename.c_str() );
  string str_line;
  int cur_arm = 0;
  while( getline( infile, str_line ) )
    {
      if( !str_line.size() ) continue;
      stringstream stream_line( str_line );
      
      float edge;
      stream_line >> edge;
      
      SetStrip tmp_set;
      unsigned short arm;
      unsigned short station;
      unsigned short octant;
      unsigned short half_octant;
      unsigned short gap;
      unsigned short plane;
      unsigned short strip;
      
      while( stream_line >> arm >> station >> octant >> half_octant >> gap >> plane >> strip )
	{
	  unsigned int id = strip_id( arm, station, octant, half_octant,
				      gap, plane, strip );
	  tmp_set.insert( id );
	}
      
      std::pair<MapStrip::iterator,bool> retval =
	_map_strip[cur_arm].insert( MapStrip::value_type( edge, tmp_set ) );
      
      if( !retval.second )
	{
	  cerr << "TMutUnstable::set_unstable_strip - error when initializing,"
	       << " Edge : " << edge << ", Size : " << tmp_set.size() << endl;
	}
      
      if( edge == 1.0 ) cur_arm++;
    }
  infile.close();
  
  if( cur_arm != MUTGEOM::NumberOfArms )
    {
      cerr << "TMutUnstable::set_unstable_strip - error,"
	   << " wrong text input file : " << filename << endl;
    }
  
  cout << "TMutUnstable::set_unstable_strip - # of patterns for south : "
       << _map_strip[MUTGEOM::South].size() << endl
       << "                                                     north : "
       << _map_strip[MUTGEOM::North].size() << endl;
  
  _use_unstable_strip = true;
  
}

//____________________________________________________________________
void TMutUnstable::load_status( const float rnd_uniform )
{
  if( use_hv_trip() )
    {
      for( int iarm=0; iarm<MUTGEOM::NumberOfArms; iarm++ )
	{
	  MapHVTrip::const_iterator itr;
	  itr = _map_hv_trip[iarm].upper_bound( rnd_uniform );
	  if(itr == _map_hv_trip[iarm].end())
	    itr = _map_hv_trip[iarm].lower_bound( rnd_uniform );
	  
	  _trip_hv[iarm].clear();
	  _trip_hv[iarm] = itr->second;
	}
    }
  
  if( use_unstable_strip() )
    {
      for( int iarm=0; iarm<MUTGEOM::NumberOfArms; iarm++ )
	{
	  MapStrip::const_iterator itr;
	  itr = _map_strip[iarm].upper_bound( rnd_uniform );
	  if(itr == _map_strip[iarm].end())
	    itr = _map_strip[iarm].lower_bound( rnd_uniform );
	  
	  _dead_strip[iarm].clear();
	  _dead_strip[iarm] = itr->second;
	}
    }
  
}

//____________________________________________________________________
bool TMutUnstable::check_hv_status( unsigned short arm,
				    unsigned short station,
				    unsigned short octant,
				    unsigned short gap,
				    unsigned short wire )
{
  if( !use_hv_trip() ) return true;
  
  string hv_name =
    MutAnodeMap::get_anode_card( arm, station, octant, gap, wire );
  
  if( _trip_hv[arm].count( hv_name ) ) return false;
  return true;
}

//____________________________________________________________________
bool TMutUnstable::check_strip_status( unsigned short arm,
				       unsigned short station,
				       unsigned short octant,
				       unsigned short half_octant,
				       unsigned short gap,
				       unsigned short cathode,
				       unsigned short strip )
{
  if( !use_unstable_strip() ) return true;
  
  unsigned int id = strip_id( arm, station,
			      octant, half_octant,
			      gap, cathode, strip );
  
  if( _dead_strip[arm].count( id ) ) return false;
  return true;
}

//____________________________________________________________________
unsigned int TMutUnstable::strip_id( unsigned short arm,
				     unsigned short station,
				     unsigned short octant,
				     unsigned short half_octant,
				     unsigned short gap,
				     unsigned short cathode,
				     unsigned short strip )
{
  unsigned short plane =
    (cathode == 0) ? MUTGEOM::Cathode1 : MUTGEOM::Cathode2;
  
  unsigned int id =
    ( ((arm&1)<<19) | ((station&3)<<17) |
      ((octant&7)<<14) | ((half_octant&1)<<13) |
      ((gap&3)<<11) | ((plane&3)<<9) | (strip&0x1ff) );
  
  return id;
}
