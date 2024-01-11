// $Id: mMutMatchCoord.h,v 1.14 2011/12/24 04:48:30 slash Exp $
#ifndef __MMUTMATCHCOORD_H__
#define __MMUTMATCHCOORD_H__

//////////////////////////////////////////////////////////////
/*!
  \file		mMutMatchCoord.h
  \brief	 Derives best combination on coordinates between cathodes 
  to minimize the number of gap coordinates/plane. Warning: this is a pretty
  tricky class (to keep computation time low) and must be handled with 
  extreme care
  \author	H. Pereira
  \version $Revision: 1.14 $
  \date		$Date: 2011/12/24 04:48:30 $
*/
//////////////////////////////////////////////////////////////

// MUTOO
#include "mMutMatchCoordPar.h"
#include <PHTimeServer.h>
#include <TMutClusMap.h>
#include <TMutCoordMap.h>
#include <TMutGapCoordMap.h>
#include <TMutHitMap.h>

// STL/BOOST
#include <boost/array.hpp>
#include <vector>
#include <iostream>

// ROOT
#include<TTree.h>

class PHCompositeNode;

/*! \ingroup modules */
//! Derives best combination on coordinates between cathodes to minimize the number of gap coordinates/plane
/*!
Matches two TMutCoord across a gap. Try minimize 1/ the number 
of coordinates in one cathode wich are associated with more than one coordinate
on the second	2/ the number of coordinates which have no associate on the second cathode.
3/ the charge difference between pairs (or multiplet) of cathodes associated<br>
<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutMatchCoordPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutCoordMap*</td>
<td> IOC</td>
<td> immutable </td>
</tr>
</table>
*/

class mMutMatchCoord
{
 public: 
  
  //! constructor
  mMutMatchCoord(); 
  
  //! Destructor
  virtual ~mMutMatchCoord()
    {} 

  //! event method
  virtual PHBoolean event(PHCompositeNode*);

  //! event method
  /*! 
    only find gap coordinates in specified arm, station and octant.
    \warning: set_interface_ptrs is not called. Must be called externally
  */
  virtual PHBoolean event( 
    PHCompositeNode*, 
    const unsigned short& arm, 
    const unsigned short& station, 
    const unsigned short& octant );

  //! tree initialization
  bool init_tree( void );
	
  //! close tree
  void finish_evaluation( void );

  //! print statistics
  void print_summary( std::ostream& out = std::cout );
  
  //! retrieves mutoo objects for analysis 
  void set_interface_ptrs( PHCompositeNode* top_node ); 

 private:	
	
  //! find coordinates in given arm, station, octant
  void find_coords( const unsigned short& arm, const unsigned short& station, const unsigned short& octant );
  
  //! get coordinates into local vectors
  bool load_coords(		 
		   const int& arm, 
		   const int& station, 
		   const int& octant, 
		   const int& half_octant, 
		   const int& gap );
	
  //! associates the coordinates belonging to the same gap_coordinates
  void associate_coords( void );
	
  //! get the current number of combinations
  double get_n_combinations( void );
  
  //! try make the minimum amount of matching coordinates in a given gap
  void match_coords( void );
  
  /*! \brief
    special iterator to build coordinates combinations. 
    it stores a pointer to the coordinate in the first cathode 
    and an iterator over the coordinates associated to it in the second cathode
  */
  class AssociationIterator
  {
      
    public:
      
    //! constructor
    AssociationIterator( const unsigned int& front_index = 0 ):
      _front_index( front_index ),
      _back_index_list(),
      _back_index_iter( _back_index_list.begin() )
      {}
		
    //! add a candidate to the back_coord_iter
    void add_back_index( const unsigned int& back_index )
      { _back_index_list.push_back( back_index ); }
    
    //! clead back_index list
    void clear_back_index_list( void )
      { 
	_back_index_list.clear(); 
	_back_index_iter = _back_index_list.begin();
      }
		
    //! reset iterator
    void begin( void )
      { _back_index_iter = _back_index_list.begin(); }
		
    //! reset iterator
    bool at_end( void ) const
      { return _back_index_iter == _back_index_list.end(); }
    
    //! increment iterator, return true if moved iterator is valid
    void advance_one( void )
      { _back_index_iter++; }
    
    //! get front coordinate
    const unsigned int& front_index( void ) const
      { return _front_index; }
    
    //! get current back coordinate
    const unsigned int& back_index( void ) const
      { return *_back_index_iter; }
    
    //! gets the number of associates
    size_t back_index_count( void ) const
      { return _back_index_list.size(); }
    
  private:
    
    //! coordinate on first gap
    unsigned int _front_index;
    
    //! list of possibly associated coordinates on the second cathode
    std::list< unsigned int > _back_index_list;
    
    //! iterator over the list of associated coordinates on the second cathode
    std::list< unsigned int >::iterator _back_index_iter;
    
  };
  
  /*! 
    associate the two coordinates if they pass the fiducial cuts
    there are three available cuts: 
    1/ cut on the charge difference between matching coords
    2/ cut on the relative charge difference between matching coords
    3/ cut on the distance of the crossing point to the closest anode wire
  */
  void associate( 
		 AssociationIterator& iterator, 
		 const unsigned int& back_index );
  
  /*! \brief
    create a new combination. Returns true if any available.
    For each combination, one try to increment the first list element iterator. If the iterator is at end,
    it is reseted and one tries to increment the second list element iterator. As soon as an iterator is incremented and
    is valid, a new combination is found. If the last list element iterator reaches the end, it means there is no more
    combination available.
  */
  bool get_next_combination();
  
  //! refit clusters for used_twice coordinates in the best combination
  void refit_clusters( void );
  
  /*! \brief
    erase (i.e. set to 0) a coordinate the vector given in argument
    and from the coordinate map. Needed to keep synchronization
  */
  void erase_coordinate( std::vector< TMutCoordMap::pointer >&, TMutCoordMap::pointer );
  
  /*!	\brief
    make gap coordinates from all associations in the association_list list
    not taking care of coordinate matching
  */
  void make_gap_coords_no_matching( void );
  
  //! make gap coordinates from all associations in the best_combination vector
  void make_gap_coords( void );
  
  //! create a gap coordinate from matching coordinates
  void make_gap_coord( TMutCoordMap::pointer first_coord, TMutCoordMap::pointer second_coord );
  
  //!@name Refitting methods
  //@{

  //! refit both clusters to fewer tracks
  void refit_both_clusters( AssociationIterator &association );
  
  //! refit (back_index) cluster to fewer tracks; update association iterator
  void refit_fewer_tracks( AssociationIterator &association );
  
  //! refit (front_index) cluster to more tracks; update association iterator
  void refit_more_tracks( AssociationIterator &association );
  
  //@}
	
  //! utility method to get hits associated to a cluster into a vector
  static std::vector< TMutHitMap::pointer > get_hits( TMutClusMap::pointer clus_ptr )
    {
      std::vector<TMutHitMap::pointer> samples;
      TMutHitMap::key_iterator hit_iter = clus_ptr->get()->get_associated<TMutHit>();
      while( TMutHitMap::pointer hit_ptr = hit_iter.next() )
	samples.push_back( hit_ptr );
      return samples;
    }
	 
  // Interface pointers
  //! parameter table
  const mMutMatchCoordPar* _mod_par;	
  
  //! coordinate IOC
  TMutCoordMap* _coord_map;		 
  
  //! gap coordinate IOC
  TMutGapCoordMap* _gap_coord_map;		 
  
  //! vector of pointers to coordinates belonging to the cathode with max number of coords
  std::vector< TMutCoordMap::pointer > _front_coord_vect;
  
  //! vector of pointers to coordinates belonging to the cathode with min number of coords
  std::vector< TMutCoordMap::pointer > _back_coord_vect;
  
  /*! 
    current list of association iterators
    the coordinate pointer corresponds to the cathode with max coords
    the list of coordinate pointers cooresponds to the associated coords in the other cathode
  */
  std::list< AssociationIterator > _association_list;
  
  
  /*! 
    best combination found. Warning: the iterator are reversed:
    the coordinate pointer corresponds to the cathode with less coords
    the list of coordinate pointers cooresponds to the associated coords in the other cathode
  */
  std::vector< AssociationIterator > _best_combination;
  
  //! internal timer to get time/combination
  PHTimer _combination_timer;
  
  //! internal counter on total number of combinations (made a double since can be _very_ big)
  double _combinations;
  
  //! internal counter of total number of cluster pairs refitted together
  unsigned int _refit_both;
  
  //! internal counter on total number of clusters refitted with more tracks
  unsigned int _refit_more;
  
  //! internal counter on total number of clusters reffited with fewer tracks
  unsigned int _refit_less;
  
  //! internal counter on total number of coordinates
  unsigned int _total_coords;
  
  //! number of overflow
  unsigned int _overflow;
  
  //! module Timer
  PHTimeServer::timer _timer;
  
  //! output TFile filename
  std::string _filename;	 
  
  //! output tree, if any
  TTree *_tree;
  
  //! arm index
  int _arm;
  
  //! station index
  int _station;
  
  //! octant index
  int _octant;
  
  //! halfoctant index
  int _half;
  
  //! gap index
  int _gap;
  
  //! number of coordinates in each cathode
  boost::array< int, 2 > _coords;
  
  /*! 
    number of coordinates in second plane which have no associate
    in the first, for a given combination
  */
  int _missed_min;
  
  /*!
    number of coordinates in second plane which have more than one 
    associate in the first, for a given combination
  */
  int _used_twice_min;
  
  //! quality estimator for a given combination
  double _delta_q_min;	 

};

#endif 
