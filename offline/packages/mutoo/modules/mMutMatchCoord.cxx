// $Id: mMutMatchCoord.cxx,v 1.22 2011/12/24 04:48:30 slash Exp $

/*!
	 \file mMutMatchCoord.cxx
	 \brief Derives best combination on coordinates between cathodes 
	 to minimize the number of gap coordinates/plane
	 \author H. Pereira
	 \version $Revision: 1.22 $
	 \date $Date: 2011/12/24 04:48:30 $
*/


// MUTOO
#include "mMutMatchCoord.h"
#include "mMutMatchCoordPar.h"

#include <MUTOO.h>
#include <PHTFileServer.h>
#include <TMutGeo.h>
#include <TMutNode.h>
#include <TMutChargeCorrection.h>
#include <TMutClusterFit.h>
#include <TMutCoordFill.h>

// PHENIX
#include <PHGeometry.h>

// STL
#include <iostream>
#include <string>
#include <map>
#include <list>

using namespace std;

//_________________________________________________________
mMutMatchCoord::mMutMatchCoord():
  _combination_timer( "mMutMatchCoord::_combination_timer" ),
  _combinations( 0 ),
  _refit_both( 0 ),
  _refit_more( 0 ),
  _refit_less( 0 ),
  _total_coords( 0 ),
  _overflow( 0 ),
  _timer( PHTimeServer::get()->insert_new( "mMutMatchCoord")),
  _filename( "mMutMatchCoord.root" ), 
  _tree( 0 )
{
  MUTOO::TRACE( "initializing module mMutMatchCoord",MUTOO::ALOT);
}

//______________________________________________________________________
bool mMutMatchCoord::init_tree()
{
  MUTOO::PRINT( cout, "mMutMatchCoord::init_tree");
  enum { BUFFER_SIZE = 32000 };
  enum { AUTO_SAVE = 16000 };
  
  // check if file exist
  if( _tree) return false;
  
  _filename = _mod_par->get_evaluation_file();
  PHTFileServer::get().open( _filename );
  _tree = new TTree( "delta_q_tree", "delta_q");
  _tree->Branch( "arm", &_arm, "arm/I", BUFFER_SIZE);
  _tree->Branch( "station", &_station, "station/I", BUFFER_SIZE);
  _tree->Branch( "octant", &_octant, "octant/I", BUFFER_SIZE);
  _tree->Branch( "half", &_half, "half/I", BUFFER_SIZE);
  _tree->Branch( "gap", &_gap, "gap/I", BUFFER_SIZE);
  _tree->Branch( "coords", &_coords[0], "coords[2]/I", BUFFER_SIZE);
  _tree->Branch( "missed", &_missed_min, "missed/I", BUFFER_SIZE);
  _tree->Branch( "used_twice", &_used_twice_min, "used_twice/I", BUFFER_SIZE);
  _tree->Branch( "delta_q", &_delta_q_min, "delta_q/D", BUFFER_SIZE);
  _tree->SetAutoSave( AUTO_SAVE);
  MUTOO::PRINT( cout, "**");
  return true;
}

//______________________________________________________________________
void mMutMatchCoord::finish_evaluation( void)
{
  
  MUTOO::TRACE( "mMutMatchCoord::finish_evaluation");
  if( !_tree ) return;
  PHTFileServer::get().write( _filename );
  return;
}

//_________________________________________________________
PHBoolean mMutMatchCoord::event( PHCompositeNode * top_node)
{
  
  _timer.get()->restart();
  try  {
      
    // Reset IOC pointers
    set_interface_ptrs( top_node);
    
    // initialize tree
    if( _mod_par->get_do_evaluation())
    {	static bool init_done __attribute__ ((unused)) = init_tree(); }

    // Loop over all gaps( gap iterator would help here)
    for( int arm = 0; arm < MUTOO::NumberOfArms; ++arm)
    for( int station = 0; station < MUTOO::NumberOfStations; ++station)
    for( int octant = 0; octant < MUTOO::NumberOfOctants; ++octant)
    { find_coords( arm, station, octant ); }
    
    if( _mod_par->get_verbosity() >= MUTOO::ALOT ) _gap_coord_map->print();

  } catch( exception & e)
  {
    MUTOO::TRACE( e.what());
    return False;
  }
  
  // stop timer
  _timer.get()->stop();

  return True;
}


//_________________________________________________________
PHBoolean mMutMatchCoord::event( 
  PHCompositeNode * top_node,
  const unsigned short& arm,
  const unsigned short& station,
  const unsigned short& octant )
{

  _timer.get()->restart();

  try {

    // initialize tree
    if( _mod_par->get_do_evaluation())
    {	static bool init_done __attribute__ ((unused)) = init_tree(); }

    find_coords( arm, station, octant );

    if( _mod_par->get_verbosity() >= MUTOO::ALOT ) _gap_coord_map->print();

  } catch( exception & e) {
    MUTOO::TRACE( e.what());
    return False;
  }
  
  // If verbose dump the contents of the cluster map
  _timer.get()->stop();

  return True;

}

//_________________________________________________________________
void mMutMatchCoord::print_summary( ostream& out )
{

  MUTOO::PRINT( out, "mMutMatchCoord::print_summary" );
  
  // dump the combination timer
  _combination_timer.print_stat();
  
  // dump total number of combinations
  cout << "_combinations=" << _combinations << endl;
  
  // dump number of refitted clusters
  cout << "_refit_both=" << _refit_both << endl;
  cout << "_refit_more=" << _refit_more << endl;
  cout << "_refit_less=" << _refit_less << endl;
  cout << "_total_coords=" << _total_coords << endl;
  cout << "_overflow=" << _overflow << endl;
  MUTOO::PRINT( out, "**" );

}

//_________________________________________________________
void mMutMatchCoord::set_interface_ptrs( PHCompositeNode * top_node )
{
  
  // module runtime parameters
  _mod_par = TMutNode < mMutMatchCoordPar >::find_node( top_node, "mMutMatchCoordPar");
  
  // TMutCoord IOC
  _coord_map = TMutNode < TMutCoordMap >::find_node( top_node, "TMutCoordMap");
  
  // TMutGapCoord IOC
  _gap_coord_map = TMutNode < TMutGapCoordMap >::find_node( top_node, "TMutGapCoordMap");
  _gap_coord_map->clear();
  

}

//_______________________________________________________________________________________
void mMutMatchCoord::find_coords( const unsigned short& arm, const unsigned short& station, const unsigned short& octant )
{
  
  // Number of gaps varies station to station
  unsigned short n_gaps = TMutGeo::get_n_gaps( arm, station);
  
  for( int half = 0; half < MUTOO::NumberOfHalfOctants; ++half)
  {
    
    for( int gap = 0; gap < n_gaps; ++gap) 
    {
      if( !load_coords( arm, station, octant, half, gap))
      { continue; }
      
      associate_coords();
      
      // check the number of combinations, compare to the cut
      double combinations( get_n_combinations() );
      if( _mod_par->get_max_combinations() >= 0 && combinations > _mod_par->get_max_combinations()) 
      {
        make_gap_coords_no_matching();
        _overflow++;
      } else {
            
        _combinations += combinations;
        match_coords();
        if( _mod_par->get_do_refit()) refit_clusters();
        make_gap_coords();
        
      }
    }
  }
  
  return;
}

//_________________________________________________________
bool mMutMatchCoord::load_coords( 
		const int & arm, 
		const int & station, 
		const int & octant, 
		const int & half_octant, 
		const int & gap )
{

  // needed for tree filling
  _arm = arm;
  _station = station;
  _octant = octant;
  _half = half_octant;
  _gap = gap;
  
  // get coords from cathode 0 and 1;
  TMutCoordMap::iterator coord_iter_0 = _coord_map->get( arm, station, octant, half_octant, gap, 0);
  TMutCoordMap::iterator coord_iter_1 = _coord_map->get( arm, station, octant, half_octant, gap, 1);
  
  // store sizes for tree
  _coords[0] = coord_iter_0.count();
  _coords[1] = coord_iter_1.count();
  _total_coords += _coords[0]+_coords[1];
  
  // check both cathodes have coords
  if( !(_coords[0] && _coords[1]))
    return false;
  
  // allocate and fill the front coordinate vector
  unsigned int index = 0;
  _front_coord_vect = vector < TMutCoordMap::pointer >( _coords[0]);
  while( TMutCoordMap::pointer coord_ptr = coord_iter_0.next())
    {
      _front_coord_vect[index] = coord_ptr;
      index++;
    }
  
  // allocate and fill the back coordinate vector
  index = 0;
  _back_coord_vect = vector < TMutCoordMap::pointer >( _coords[1]);
  while( TMutCoordMap::pointer coord_ptr = coord_iter_1.next())
    {
      _back_coord_vect[index] = coord_ptr;
      index++;
    }
  
  // swap everything so that front has the largest number of coordinates
  if( _coords[0] < _coords[1])
    {
      swap < vector < TMutCoordMap::pointer > >(_front_coord_vect, _back_coord_vect);
      swap < int >(_coords[0], _coords[1]);
    }

  return true;
  
}

//_________________________________________________________
void mMutMatchCoord::associate_coords( void )
{

  // reset association list
  _association_list.clear();
  
  // loop over all front_vector coordinates
  for( unsigned int front_index = 0; front_index < _front_coord_vect.size(); front_index++)
  {
    
    // create a new association iterator
    AssociationIterator iter( front_index);
    
    // reset iterator over the second cathode
    for( unsigned int back_index = 0; back_index < _back_coord_vect.size(); back_index++)
    { associate( iter, back_index); }
    
    // put iterator in list
    if( iter.back_index_count())
    { _association_list.push_back( iter); }
      
  }
  
  /*
  initialize all iterators 
  it is mandatory for the get_next_combination method to work
  */
  if( _mod_par->get_verbosity() >= MUTOO::MAX)
  {
    for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
    {
      
      cout 
        << "mMutMatchCoord::associate_coords - front=" 
        << _front_coord_vect[iter->front_index()]->get()->get_key().get_obj_key();
      cout << " associates_count=" << iter->back_index_count();
      cout << " associates= [";
      
      for( iter->begin(); !iter->at_end(); iter->advance_one() )
      { cout << _back_coord_vect[iter->back_index()]->get()->get_key().get_obj_key() << " " ; }
      cout << "]" << endl;
    }
    
  }
  
  for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
  { iter->begin(); }
  
}

//________________________________________________________
double mMutMatchCoord::get_n_combinations( void )
{

  // check number of association iterator
  if( !_association_list.size()) return 0;
  
  MUTOO::Verbosity verbosity( MUTOO::ALOT);
  
  // calculate and dump the total number of combinations
  if( _mod_par->get_verbosity() >= verbosity)
    cout << "mMutMatchCoord::get_n_combinations -"
	 << " [" << _arm << "," << _station << "," << _octant << "," << _half
	 << "," << _gap << "]" << " front_coord_size=" << _front_coord_vect.size() 
	 << " back_coord_size=" << _back_coord_vect.size() << " [ ";

  double out( 1);
  for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
  {
    if( iter->back_index_count()) out *= iter->back_index_count();
    if( _mod_par->get_verbosity() >= verbosity) cout << iter->back_index_count() << " ";
  }
  
  if( _mod_par->get_verbosity() >= verbosity)
    cout << "] n_combinations=" << out << endl;
  
  return out;
  
}

//_________________________________________________________
void mMutMatchCoord::associate( mMutMatchCoord::AssociationIterator & iter, const unsigned int &back_index )
{

  // retrieve first coordinate
  TMutCoordMap::pointer first_coord( _front_coord_vect[iter.front_index()]);
  TMutCoordMap::pointer second_coord( _back_coord_vect[back_index]);
  
  // Check charge difference is below max
  const float &q0( first_coord->get()->get_q_tot());
  const float &q1( second_coord->get()->get_q_tot());
  if( fabs( q1 - q0) > _mod_par->get_q_match_threshold()) 
  {
    if( _mod_par->get_verbosity() == MUTOO::ALOT)
    {
      MUTOO::PRINT( cout, "failed charge matching cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT( cout, "**");
    }
    
    return;
  }

  // check charge asymetry is below max
  if( _mod_par->get_q_rel_match_threshold() >= 0
    && fabs( q1 - q0) / fabs( q1 + q0) >
    _mod_par->get_q_rel_match_threshold())
  {
    if( _mod_par->get_verbosity() == MUTOO::ALOT)
    {
      MUTOO::PRINT( cout, "failed charge relative matching cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT( cout, "**");
    }
    
    return;
  }
  
  // fiducial cuts
  // Point on second_coord closest to first_coord
  PHPoint point =
    PHGeometry::closestApproachLineLine( first_coord->get()->get_coord(),
    second_coord->get()->get_coord());
  
  // Check point is in fiducial volume
  if( !TMutGeo::in_fiducial( first_coord->get()->get_arm(),
    first_coord->get()->get_station(),
    first_coord->get()->get_octant(),
    first_coord->get()->get_half_octant(), point))
  {
    
    if( _mod_par->get_verbosity() == MUTOO::ALOT)
    {
      MUTOO::PRINT( cout, "failed fiducial cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT( cout, "**");
    }
    
    return;
  }
  
  // Get wire number and distance to closest anode
  pair < int, double >anode_data;
  try
  {
    anode_data = TMutGeo::find_anode_dca( first_coord->get()->get_arm(), point);
  } catch( exception & e) {
    if( _mod_par->get_verbosity() == MUTOO::ALOT)
    {
      MUTOO::PRINT( cout, "failed to find anode");
      first_coord->get()->print();
      second_coord->get()->print();
      cout << "point = {" << point.getX() << "," << point.getY() << "," << point.getZ() << "}" << endl;
    }
    return;
  }
  
  // Check that point is sufficiently close to anode wire
  if( anode_data.second > _mod_par->get_max_anode_dca()) 
  {
    if( _mod_par->get_verbosity() == MUTOO::ALOT)
    {
      MUTOO::PRINT( cout, "failed anode proximity cut");
      first_coord->get()->print();
      second_coord->get()->print();
      MUTOO::PRINT( cout, "**");
    }
    return;
  }
  
  // insert second coordinate in the list of possible associates
  iter.add_back_index( back_index);
  return;
  
}

//_________________________________________________________
void mMutMatchCoord::match_coords( void )
{

  // clear the _best_combination vector
  _best_combination.clear();
  
  // check number of association iterator
  if( !_association_list.size()) return;
  
  // initialize best_combination parameters
  _delta_q_min = -1;
  _missed_min = 0;
  _used_twice_min = 0;
  
  /*
    allocate map of the coordinates in the 'second' cathode 
    and a 'reversed' association iterator
  */
  vector < AssociationIterator > reversed_map( _back_coord_vect.size());
  for( unsigned int back_index = 0; back_index < _back_coord_vect.size(); back_index++)
    reversed_map[back_index] = AssociationIterator( back_index);
  
  // allocating
  register unsigned int back_index;
  
  // make all possible combinations
  _combination_timer.restart();
  do {
      
    // reset the reversed map
    for( back_index = 0; back_index < reversed_map.size(); back_index++)
      reversed_map[back_index].clear_back_index_list();
    
    /*
      loop over the coordinates iterator, for each charge in the second cathode, store the sum of the charge of the associated
      coords found in first cathode. At the same time count the number of used twice coordinates
    */
    int used_twice( 0 );
    for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
      {
	if( reversed_map[iter->back_index()].back_index_count())
	  used_twice++;
	
	if( used_twice > _mod_par->get_max_used_twice())
	  break;
	
	reversed_map[iter->back_index()].add_back_index( iter->front_index());
      }
    
    // cut on the number of used twice coordinates
    if( used_twice > _mod_par->get_max_used_twice()) continue;
    
    // count back coordinates which do not have coordinates on the front cathode
    double delta_q( 0 );
    int missed( 0 );
    for( back_index = 0; back_index < reversed_map.size(); back_index++)
      if( !reversed_map[back_index].back_index_count())
	{
	  
	  // coordinates on back cathode which do not have associates count as 1 in delta_q
	  delta_q += 1;
	  missed++;
	  if( _delta_q_min >= 0 && delta_q >= _delta_q_min) break;
	  
	}
    
    if( _delta_q_min >= 0 && delta_q >= _delta_q_min) continue;
    
    /*
      calculate delta_q
      we loop over the map of the coordinates used in the second cathode. For each, we build the delta_q
      between its charge and the sum of the charges of the coordinates found matching in the first cathode
    */
    for( back_index = 0; back_index < reversed_map.size(); back_index++)
      if( reversed_map[back_index].back_index_count())
	{
	  
	  // coordinates on back cathode which do have associates are counted as charge asymetry
	  const double &back_charge( _back_coord_vect
				     [reversed_map[back_index].front_index()]->
				     get()->get_q_tot());
	  
	  // loop over all associate get total corresponding charge
	  double front_charge( 0);
	  for( reversed_map[back_index].begin(); !reversed_map[back_index].at_end(); reversed_map[back_index].advance_one() )
	    front_charge += _front_coord_vect[reversed_map[back_index].back_index()]->get()->get_q_tot();
	  
			// calculate charge asymetry
	  delta_q += 2 * fabs( (front_charge - back_charge) /( front_charge + back_charge));
	  if( _delta_q_min >= 0 && delta_q >= _delta_q_min) break;
	  
	}
    
    // check delta_q versus delta_q_min; store the combination if is smaller
    if( _delta_q_min < 0 || delta_q < _delta_q_min)
      {
	_delta_q_min = delta_q;
	_used_twice_min = used_twice;
	_missed_min = missed;
	
	// save the combination
	_best_combination = reversed_map;
	
      }
    
  } while( get_next_combination());
  _combination_timer.stop();
  
  
  if( _mod_par->get_verbosity() >= MUTOO::MAX) {
    
    // dump best combination
    cout << "mMutMatchCoords::match_coords - ";
    for( unsigned int index = 0; index < _best_combination.size(); index++)
      if( _best_combination[index].back_index_count())
	{
	  for( _best_combination[index].begin(); !_best_combination[index].at_end(); _best_combination[index].advance_one() )
	    cout
	      << "[" 
	      << _back_coord_vect[_best_combination[index].front_index()]->get()->get_key().get_obj_key() << "," 
	      << _front_coord_vect[_best_combination[index].back_index()]->get()->get_key().get_obj_key() << "]";
	  cout << " delta_q_min=" << _delta_q_min << endl;
	}
    
  }
  
  // store the parameters corresponding to the minimum delta_q combination
  if( _mod_par->get_do_evaluation() && _tree) _tree->Fill();
  
  return;

}

//_________________________________________________________
bool mMutMatchCoord::get_next_combination( void )
{
  /*
    Each item in the list keeps a pointer to the coordinate in the first cathode
    For each combination, one try to increment the first iterator. If it reaches the end,
    it is reseted and one try to increment the second. As soon as an iterator is incremented and
    is valid, a new combination is found. If the last iterator reaches the end, it means there is no more
    combination available.
  */
  
  bool valid( false);
  for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
    {
      
      // skip objects with no associates
      if( !iter->back_index_count())
	continue;
      
      /*
	try increment current iterator
	if valid return true, since a valid combination has been found;
	if not valid, then reset the iterator; try increment the next one.
      */
      iter->advance_one();		 
      if( iter->at_end() ) iter->begin();
      else {
	valid = true;
	break;
      } 
      
    }
  
  if( _mod_par->get_verbosity() >= MUTOO::MAX) 
    {
      if( !valid)
	cout << "mMutMatchCoord::get_next_combination - no combination left" << endl;
      else
	{
	  cout << "mMutMatchCoord::get_next_combination - [ ";
	  for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
	    if( iter->back_index_count())
	      cout << iter->back_index() << " ";
	  cout << "]" << endl;
	}

    }
  
  return valid;
}

//_________________________________________________________
void mMutMatchCoord::refit_clusters( void )
{

  // check that best_combination makes sense.
  if( !_best_combination.size()) return;
  
  // make a local copy of the _best_combination
  vector < AssociationIterator > local_combination = _best_combination;
  _best_combination.clear();
  
  // loop over 'reversed' association iterator, find coordinates which are used twice
  for( unsigned int back_index = 0; back_index < local_combination.size(); back_index++)
  {
    
    // local storage of the current association iterator
    AssociationIterator & association( local_combination[back_index]);
    
    
    // check that the front_coord still makes sense( it may have been erased from previous refit)
    if( !( _back_coord_vect[association.front_index()] && association.back_index_count())) continue;
    
    // reset association iterator
    association.begin();
    
    // one coordinate( back_index) associated
    if( association.back_index_count() == 1 && _front_coord_vect[association.back_index()] )
    {
      
      // get cluster associated to front and back coordinates
      TMutClusMap::key_iterator back_clus_iter( _back_coord_vect[association.front_index()]->get()->get_associated<TMutClus>() );
      TMutClusMap::key_iterator front_clus_iter( _front_coord_vect[association.back_index()]->get()->get_associated<TMutClus>() );
      
      // from melynda, following configuration is _very_ likely to be wrong. both clusters are reffited to 1 centroid
      bool refit_both = 
        (	back_clus_iter->get()->get_n_strip()==3) &&( back_clus_iter->get()->get_n_centroid()==2) && 
        (	front_clus_iter->get()->get_n_strip()==3) &&( front_clus_iter->get()->get_n_centroid()==2); 
      
      if( refit_both ){
        refit_both_clusters( association );
        continue;
      } else {
        //! nothing wrong with that association. Keep it unchanged
        _best_combination.push_back( association);
        continue;
      }	 
      
    }
    
    // more than 1 coordinate( back_index) are associated
    // get the set of associated clusters
    vector < TMutClus * >clusters;
    for( association.begin(); !association.at_end(); association.advance_one() )
    {
      
      // check that the back_coord still makes sense( it may have been erased from previous refit)
      if( !_front_coord_vect[association.back_index()])
      { continue; }
      
      // get cluster associated to the current coordinate
      TMutClusMap::key_iterator clus_iter = _front_coord_vect[association.back_index()]->get()->get_associated<TMutClus>();
      clusters.push_back( clus_iter->get());
      
    }
    
    // check the size of associated clusters vector
    if( clusters.size() < 2)
    {
      _best_combination.push_back( association);
      continue;
    }
    
    /*
    decide if we will refit the cluster to a smaller or a larger number of tracks
    we check that the first to cluster keys are equal; that the cluster is not too wide; that it's number
    of centroids is two. Should maximize the amount of time where the clusters get properly refitted.
    */
    bool fewer_tracks =( (clusters[0]->get_key().get_obj_key() ==
      clusters[1]->get_key().get_obj_key())
      &&( clusters[0]->get_n_strip() == 3)
      &&( clusters[0]->get_n_centroid() == 2));
    
    if( fewer_tracks) {
      refit_fewer_tracks( association );
      continue;
    }
    
    bool more_tracks = ( clusters[0]->get_key().get_obj_key() !=clusters[1]->get_key().get_obj_key() );
    if( more_tracks ) {
      refit_more_tracks( association );
      continue;
    }
    
    // nothing worth being done with this association keep it unchanged
    _best_combination.push_back( association);
    
  }				
  // loop over association iterators
  
}

//_________________________________________________________
void mMutMatchCoord::erase_coordinate( std::vector < TMutCoordMap::pointer > &coord_vect, TMutCoordMap::pointer coord_ptr )
{
	
  // set matching coordinates in argument vector to 0
  for( unsigned int index = 0; index < coord_vect.size(); index++)
  {
    if( coord_vect[index] && coord_vect[index]->get()->get_key().get_obj_key() == coord_ptr->get()->get_key().get_obj_key())
    {
      // reset the index
      coord_vect[index] = 0;
      
      // only one matching coordinate can be found. Break here to spare time
      break;
    }
  }
  
  // erase the coordinate from the map
  _coord_map->erase(coord_ptr->get()->get_key());
  
}

//_________________________________________________________
void mMutMatchCoord::make_gap_coords_no_matching( void )
{
  
  // convert the best combination into GapCoordinates
  for( list < AssociationIterator >::iterator iter = _association_list.begin(); iter != _association_list.end(); iter++)
  {
    if( _front_coord_vect[iter->front_index()])
    {
	
      // loop over back_index coordinates, create gap coordinates
      for( iter->begin(); !iter->at_end(); iter->advance_one() )
      {
        if( _back_coord_vect[iter->back_index()])
        { make_gap_coord( _front_coord_vect[iter->front_index()], _back_coord_vect[iter->back_index()]); }
	
      }
    }
  }
  
  return;
  
}

//_________________________________________________________
void mMutMatchCoord::make_gap_coords( void )
{
  
  // convert the best combination into GapCoordinates
  for( unsigned int back_index = 0; back_index < _best_combination.size(); back_index++)
  {
    
    if( !_back_coord_vect[_best_combination[back_index].front_index()]) continue;
    
    // check number of back_index coordinates
    if( !_best_combination[back_index].back_index_count()) continue;
    
    // loop over back_index coordinates, create gap coordinates
    for( _best_combination[back_index].begin(); !_best_combination[back_index].at_end(); _best_combination[back_index].advance_one() )
    {
      if( _front_coord_vect[_best_combination[back_index].back_index()])
      {
        make_gap_coord( 
          _back_coord_vect[_best_combination[back_index].front_index()],
          _front_coord_vect[_best_combination[back_index].back_index()]);
      }
      
    }
  }
  
  return;
  
}

//_________________________________________________________________
void mMutMatchCoord::make_gap_coord( TMutCoordMap::pointer first_coord, TMutCoordMap::pointer second_coord)
{
      
  // this new implementation does not assume that coordinates are in a plane perpendicular to the beam axis
  // Point on second_coord closest to first_coord
  PHPoint first = PHGeometry::closestApproachLineLine( first_coord->get()->get_coord(), second_coord->get()->get_coord());
  
  // Point on first_coord closest to second_coord
  PHPoint second = PHGeometry::closestApproachLineLine( second_coord->get()->get_coord(), first_coord->get()->get_coord());
  
  // true coordinate is at the middle of the two points above
  // it should lie on an anode wire
  PHPoint point = (first + second )*0.5;

  // retrieve anode data
  pair < int, double >anode_data;
  try
  {
    anode_data = TMutGeo::find_anode_dca( first_coord->get()->get_arm(), point);
  } catch( exception & e)
  {
	
    if( _mod_par->get_verbosity() == MUTOO::ALOT)
    {
      MUTOO::PRINT( cout, "failed to find anode");
      first_coord->get()->print();
      second_coord->get()->print();
      cout << "point = {" << point.getX() << "," << point.getY() << "," << point.getZ() << "}" << endl;
      
    }
  
    return;
    
  }
  
  // Make a new gap coordinate
  TMutGapCoordMap::iterator gap_iter =
    _gap_coord_map->insert_new( first_coord->get()->get_arm(),
    first_coord->get()->get_station(),
    first_coord->get()->get_octant(),
    first_coord->get()->get_half_octant(),
    first_coord->get()->get_gap());
  
  // Set the coordinate
  gap_iter->get()->set_coord( point);
  gap_iter->get()->set_anode( anode_data.first);
  gap_iter->get()->set_anode_dca( anode_data.second);
  gap_iter->get()->
    set_charge_corr( TMutChargeCorrection::
    get_correction( gap_iter->get()->get_location(),
    point.getX(), point.getY()));
  
  // associate the coordinates to the gap coordinates
  PHKey::associate( first_coord, gap_iter.current());
  PHKey::associate( second_coord, gap_iter.current());
  
  // associate the two coordinates together
  PHKey::associate( first_coord, second_coord);
  
}


//________________________________________________________
void mMutMatchCoord::refit_both_clusters( mMutMatchCoord::AssociationIterator& association )
{
  
  // retrieve front_index cluster
  TMutClusMap::key_iterator back_clus_iter = _back_coord_vect[association.front_index()]->get()->get_associated<TMutClus >();
  TMutClusMap::pointer back_clus_ptr( back_clus_iter.current());
  
  // retrieve associated hits, check ndf 
  int back_ntracks = back_clus_ptr->get()->get_n_centroid() - 1;
  vector < TMutHitMap::pointer > back_samples( get_hits( back_clus_ptr ) );
  if(( back_samples.size() + 2) / back_ntracks < 2) 
  {
    _best_combination.push_back( association );
    return;
  }
  
  // retrieve back_index cluster
  association.begin();
  TMutClusMap::key_iterator front_clus_iter = _front_coord_vect[association.back_index()]->get()->get_associated<TMutClus >();
  TMutClusMap::pointer front_clus_ptr( front_clus_iter.current() );
  
  // retrieve associated hits, check ndf 
  int front_ntracks( front_clus_ptr->get()->get_n_centroid() - 1 );
  vector < TMutHitMap::pointer > front_samples( get_hits( front_clus_ptr ) );
  if(( front_samples.size() + 2) / front_ntracks < 2) 
  {
    _best_combination.push_back( association );
    return;
  }
  
  // increment counter
  _refit_both++;
  
  // remove back_cluster associated coords, refit, and store new coords:
  TMutCoordMap::key_iterator key_iter = back_clus_ptr->get()->get_associated < TMutCoord >();
  while( TMutCoordMap::pointer coord_ptr = key_iter.next())
  { erase_coordinate( _back_coord_vect, coord_ptr); }
  
  // refit back_cluster
  TMutClusterFit::gsl_mathieson_fit(back_samples, back_clus_ptr, back_ntracks);
  list< TMutCoordMap::pointer > back_coord_list = TMutCoordFill::create_coords( back_clus_ptr, _coord_map);
  back_clus_ptr->get()->set_refit_less();
  
  // remove front_cluster associated coords, refit, and store new coords:
  key_iter = front_clus_ptr->get()->get_associated < TMutCoord >();
  while( TMutCoordMap::pointer coord_ptr = key_iter.next())
  { erase_coordinate(_front_coord_vect, coord_ptr); }
  
  // refit front_cluster
  TMutClusterFit::gsl_mathieson_fit( front_samples, front_clus_ptr, front_ntracks);
  list< TMutCoordMap::pointer > front_coord_list = TMutCoordFill::create_coords( front_clus_ptr, _coord_map );
  front_clus_ptr->get()->set_refit_less();
  
  // add coordinates to the vectors an create new AssociationIterators
  list< AssociationIterator > local_associations;
  for( list< TMutCoordMap::pointer >::iterator back_iter = back_coord_list.begin(); back_iter != back_coord_list.end(); back_iter++ )
  {
    _back_coord_vect.push_back( *back_iter );
    local_associations.push_back( AssociationIterator( _back_coord_vect.size() - 1 ) );
  }
  
  // add back_index coordinates to the newly created Association iterators
  for( list< TMutCoordMap::pointer >::iterator front_iter = front_coord_list.begin(); front_iter != front_coord_list.end(); front_iter++ ) 
  {
    _front_coord_vect.push_back( *front_iter );
    for( list< AssociationIterator >::iterator iter = local_associations.begin(); iter != local_associations.end(); iter++ )
    { iter->add_back_index( _front_coord_vect.size() - 1 ); }
  }
  
  // put new AssociationIterators in _best_combination list
  for( list< AssociationIterator >::iterator iter = local_associations.begin(); iter != local_associations.end(); iter++ )
  { _best_combination.push_back( *iter ); }
  
  return;
  
}

//________________________________________________________
void mMutMatchCoord::refit_fewer_tracks( mMutMatchCoord::AssociationIterator& association )
{
  
  // refit back_index cluster to fewer tracks
  // keep pointer to the first back_index coordinate
  TMutCoordMap::pointer coord_ptr( 0 );
  for( association.begin(); !association.at_end(); association.advance_one() )
  {
    if( _front_coord_vect[association.back_index()] ) 
    {
      coord_ptr = _front_coord_vect[association.back_index()];
      break;
    }
  }
  
  // retrieve associated cluster
  TMutClusMap::key_iterator clus_iter = coord_ptr->get()->get_associated<TMutClus>();
  TMutClusMap::pointer clus_ptr = clus_iter.current();
  
  // decrement number of tracks to pass through the cluster
  int ntracks = clus_ptr->get()->get_n_centroid() - 1;
  
  // retrieve associated hits, check ndf
  vector < TMutHitMap::pointer >samples( get_hits( clus_ptr ) );
  if(( samples.size() + 2) / ntracks < 2) 
  {
    _best_combination.push_back( association );
    return;
  }
  
  // increment counter
  _refit_less++;
  
  // remove currently associated coords, refit, and store new coords:
  TMutCoordMap::key_iterator key_iter = clus_ptr->get()->get_associated < TMutCoord >();
  while( TMutCoordMap::pointer coord_ptr = key_iter.next())
  { erase_coordinate( _front_coord_vect, coord_ptr); }
  
  // redo the fit
  TMutClusterFit::gsl_mathieson_fit( samples, clus_ptr, ntracks);
  list< TMutCoordMap::pointer > coord_list = TMutCoordFill::create_coords( clus_ptr, _coord_map);
  clus_ptr->get()->set_refit_less();
  
  /*
    reset the current associationIterator
    and re-add the newly created coordinates to the front_coord_vect
  */
  association.clear_back_index_list();
  for( list< TMutCoordMap::pointer >::iterator coord_iter = coord_list.begin(); coord_iter != coord_list.end(); coord_iter++ )
  {
    
    _front_coord_vect.push_back( *coord_iter );
    association.add_back_index( _front_coord_vect.size() - 1);
    
  }
  
  _best_combination.push_back(association);
  
  return;
  
}

//________________________________________________________
void mMutMatchCoord::refit_more_tracks( mMutMatchCoord::AssociationIterator& association )
{
  
  // refit the front_index cluster to more tracks
  TMutClusMap::key_iterator clus_iter = _back_coord_vect[association.front_index()]->get()->get_associated < TMutClus >();
  TMutClusMap::pointer clus_ptr = clus_iter.current();
  
  // increment the number of tracks passing through the cluster
  int ntracks = clus_ptr->get()->get_n_centroid() + 1;
  
  // retrieve associated hits, check ndf
  vector < TMutHitMap::pointer > samples( get_hits( clus_ptr ) );
  if(( samples.size() + 2) / ntracks < 2) 
  {
    _best_combination.push_back( association );
    return;
  }
  
  // increment counter
  _refit_more++;
  
  // remove currently associated coords, refit, and store new coords:
  TMutCoordMap::key_iterator key_iter = clus_ptr->get()->get_associated < TMutCoord >();
  while( TMutCoordMap::pointer coord_ptr = key_iter.next())
  { erase_coordinate( _back_coord_vect, coord_ptr); }
  
  // redo the fit
  TMutClusterFit::gsl_mathieson_fit( samples, clus_ptr, ntracks);
  list< TMutCoordMap::pointer > coord_list = TMutCoordFill::create_coords( clus_ptr, _coord_map);
  clus_ptr->get()->set_refit_more();
  
  // redo the associations
  for( list< TMutCoordMap::pointer >::iterator coord_iter = coord_list.begin(); coord_iter != coord_list.end(); coord_iter++ )
  {
    
    _back_coord_vect.push_back(	*coord_iter );
    AssociationIterator new_association( _back_coord_vect.size() - 1);
    
    // put all associations from the old iterator into the new
    for( association.begin(); !association.at_end(); association.advance_one() )
    {
      if( _front_coord_vect[association.back_index()])
      { new_association.add_back_index( association.back_index()); }
      _best_combination.push_back( new_association);
      
    }
  }
  
  return;
}
