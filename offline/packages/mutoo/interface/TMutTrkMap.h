#ifndef __TMUTTRKMAP_H__
#define __TMUTTRKMAP_H__

/*!
  \file    TMutTrkMap.h
  \brief   Interface Object Container Class : TFvtxTrkMap
  \author  S. Kelly, H.Pereira
  \version $Revision: 1.15 $
  \date    $Date: 2011/12/29 20:19:31 $
*/

// BOOST headers
#include<boost/smart_ptr.hpp>
#include<boost/array.hpp>

// PHENIX headers
#include<TMutTrk_v4.hh>
#include<TMutKeyGen.h>
#include<TMutMapIO.h>
#include<PHMap.h>
#include<PHMapIterator.h>
#include<PHConstMapIterator.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>
#include<PHKey.hh>
#include<MUTOO.h>

/*! \ingroup container */
//! Container for MUTR TMutTrk objects

/*!   
  Interface Object Container for TMutTrk objects.
*/
class TMutTrkMap : 
public PHMap< PHKey::key_type, TMutTrk, TMutTrk_v4 >  
{
  
public:

  //! @name Constructors/Destructors
  //@{    
 
  //! Default constructor  
  TMutTrkMap():
    _write_ghost_tracks( false )
  { _counts.assign(0); }

  //! Construct with map key  
  TMutTrkMap(PHKey::map_key_type map_key)  : 
    PHMap<PHKey::key_type, TMutTrk, TMutTrk_v4>(map_key),
    _write_ghost_tracks( false )
  { _counts.assign(0); }

  //! Virtual destructor
  virtual ~TMutTrkMap() {;}

  //@}

  //! @name Extractors
  //@{    
  iterator insert_new(const UShort_t& arm, const UShort_t& octant); 
  
  //! clone a track, insert it in the map.
  iterator insert_clone( const TMutTrkMap::pointer trk_ptr ); 
  
  //! Get an iterator to all TMutStub in given station      
  iterator get(const UShort_t& arm);

  //! Get an const iterator to all TMutStub in given station    
  iterator get(const UShort_t& arm, const UShort_t& octant);

  //! Get an iterator to all TMutStub in given half octant    
  const_iterator get(const UShort_t& arm) const; 

  //! Get an const iterator to all TMutStub in given half octant    
  const_iterator get(const UShort_t& arm, const UShort_t& octant) const;

  //@}

  //! write ghost tracks to output node
  void set_write_ghost_tracks( const bool& value )
  { _write_ghost_tracks = value; }
  
  //! @name Clear
  //@{
  void clear() 
  { 
    _counts.assign(0); 
    PHMap<PHKey::key_type, TMutTrk, TMutTrk_v4>::clear(); 
  }
  //@}
  
  //! write map to output tracks
  /*! overload PHMap::write_array method */
  virtual void write_array();
  
 private:
  
  //! rolling counts for each arm
  UShort_t get_roll_count( const UShort_t& arm, const UShort_t& octant ) 
  { 
    UShort_t index = octant + MUTOO::NumberOfOctants*arm;
    return _counts[index]++%TMutKeyGen::get_max_index();
  }
  
  //! rolling counts for each arm
  boost::array< UShort_t, MUTOO::NumberOfArms*MUTOO::NumberOfOctants > _counts;
  
  //! set to true if the "ghost" tracks are written to output node
  /*! the default is false */
  bool _write_ghost_tracks;
  
};

#endif




