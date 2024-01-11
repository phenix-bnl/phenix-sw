// $Id: mMutMatchCoordPar.h,v 1.5 2005/02/19 06:05:12 hpereira Exp $
#ifndef __MMUTMATCHCOORDPAR_H__
#define __MMUTMATCHCOORDPAR_H__

//////////////////////////////////////////////////////////////
/*!
  \file    mMutMatchCoordPar.h
  \brief  Runtime parameter object for mMutMatchCoord analysis module
  \author  H. Pereira
  \version $Revision: 1.5 $
  \date    $Date: 2005/02/19 06:05:12 $
*/
//////////////////////////////////////////////////////////////

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>

#include <string>

//!  Runtime parameter object for mMutMatchCoord analysis module
class mMutMatchCoordPar: public TMutParBase
{
 public:
  
  //! constructor
  mMutMatchCoordPar():
    _q_match_threshold(1000),
    _q_rel_match_threshold( -1 ),
    _max_anode_dca(0.5),
    _max_used_twice( 2 ),
    _max_combinations( -1 ),
    _do_refit( true ),
    _do_evaluation( false ),
    _evaluation_file( "mMutMatchCoord.root" )
    {
      TMutParameterDB::get().get<float>( "mMutMatchCoord_q_match_threshold", _q_match_threshold );
      TMutParameterDB::get().get<float>( "mMutMatchCoord_q_rel_match_threshold", _q_rel_match_threshold );
      TMutParameterDB::get().get<float>( "mMutMatchCoord_max_anode_dca", _max_anode_dca );
      TMutParameterDB::get().get<int>( "mMutMatchCoord_max_used_twice", _max_used_twice );
      TMutParameterDB::get().get<long long int>( "mMutMatchCoord_max_combinations", _max_combinations );
      TMutParameterDB::get().get<bool>( "mMutMatchCoord_do_refit", _do_refit );
      TMutParameterDB::get().get<bool>( "mMutMatchCoord_do_evaluation", _do_evaluation );
      TMutParameterDB::get().get<std::string>( "mMutMatchCoord_evaluation_file", _evaluation_file );
    }
  
  //! destructor
  virtual ~mMutMatchCoordPar()
    {}
  
  //! Minimum delta Q for match 
  float get_q_match_threshold() const 
    {return _q_match_threshold;} 
  
  //! Minimum delta Q for match 
  void set_q_match_threshold(float threshold) 
    {_q_match_threshold = threshold;}
  
  //! Minimum delta Q/Q for match. Negative means no cut 
  float get_q_rel_match_threshold() const 
    {return _q_rel_match_threshold;} 
 
  //! Minimum delta Q/Q for match 
  void set_q_rel_match_threshold(float threshold) 
    {_q_rel_match_threshold = threshold;}
  
  //! Maximum distance from gap coord to anode wire 
  float get_max_anode_dca() const 
    {return _max_anode_dca;} 
  
  //! Maximum distance from gap coord to anode wire 
  void set_max_anode_dca(float max_anode_dca) 
    {_max_anode_dca = max_anode_dca;}
  
  //! Max number of accepted 'used_twice' coordinates 
  int get_max_used_twice( void ) const
    { return _max_used_twice; }
  
  //! Max number of accepted 'used_twice' coordinates 
  void set_max_used_twice( int value ) 
    { _max_used_twice = value; } 
  
  //! Max number of combinations. -1 means no cut
  long long int get_max_combinations( void ) const
    { return _max_combinations; }
	
  /*! \brief 
    Max number of combinations. 
    -1 means no cut
    0 means no matching. All possible gap coordinates are created.
    Any other makes a cut
  */
  void set_max_combinations( long long int value )
    { _max_combinations = value; }
  
  //! cluster refitting switch
  bool get_do_refit( void ) const 
    { return _do_refit; }
  
  //! cluster refitting switch
  void set_do_refit( bool value ) 
    { _do_refit = value; }
  
  //! evaluation histogram switch
  bool get_do_evaluation( void ) const 
    { return _do_evaluation; }
	
  //! evaluation histogram switch
  void set_do_evaluation( bool value ) 
    { _do_evaluation = value; }
  
  //! histogram file
  std::string get_evaluation_file( void ) const 
    { return _evaluation_file; }

  //! histogram file
  void set_evaluation_file( const std::string& value )
    { _evaluation_file = value; }
  
  //! dump all parameters	
  void print( void ) {
    MUTOO::PRINT( std::cout, "mMutMatchCoord" );
    std::cout << "_verbosity = " << _verbosity << ".\n";
    std::cout << "_q_match_threshold = " << _q_match_threshold << " [charge].\n";
    std::cout << "_q_rel_match_threshold = " << _q_rel_match_threshold << ".\n";
    std::cout << "_max_anode_dca = " << _max_anode_dca << ".\n"; 
    std::cout << "_max_used_twice = " << _max_used_twice << ".\n"; 
    std::cout << "_max_combinations = " << _max_combinations << ".\n";
    std::cout << "_do_refit = " << _do_refit << ".\n";
    std::cout << "_do_evaluation = " << _do_evaluation << ".\n";
    std::cout << "_evaluation_file = " << _evaluation_file << ".\n";
    MUTOO::PRINT( std::cout, "**" );
  }

 private:
  
  //! is the max charge difference between matching coordinates
  float _q_match_threshold;
  
  //! is the max relative charge difference between matching coordinates
  float _q_rel_match_threshold;
  
  //! is the max distance of matching coordinates crossing to closest anode wire
  float _max_anode_dca;

  //! max number of used_twice coordinates
  int _max_used_twice;

  /*! 
    max number of combinations. 
    If exceeded do not make the matching, make all possible gap coordinates
    instead. -1 means no cut.
  */
  long long int _max_combinations; 

  /*! 
    if true, clusters are refited to minimize the number of coordinates
    on one cathode associated to more than one on the other
  */
  bool _do_refit;
	
  //! if true evaluation histograms are booked and filled
  bool _do_evaluation;
  
  //! filname where to write evaluation histograms
  std::string _evaluation_file;
    
};

#endif 
