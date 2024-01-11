//////////////////////////////////////////////////////////////////
/*
  \file TMutClusterFitEval.h
  \author: H. Pereira
  \version $Revision: 1.3 $
  \date    $Date: 2007/06/08 17:50:21 $
  \brief runtime evaluator for the TMutClusterFit
*/
//////////////////////////////////////////////////////////////////

#ifndef __TMUTCLUSTERFITEVAL_H__
#define __TMUTCLUSTERFITEVAL_H__

#include <map>
#include <boost/array.hpp>
#include <TNtuple.h>

#include "TMutClusMap.h"

//! runtime evaluator for the TMutClusterFit
class TMutClusterFitEval
{

  public:
  
  //! do_evaluation flag
  static void set_do_evaluation( bool value )
  { _do_evaluation = value; }
  
  //! do_evaluation flag
  static bool get_do_evaluation( void )
  { return _do_evaluation; }
  
  //! evaluation ntuple filename
  static void set_filename( const std::string& filename )
  { _filename = filename; }
  
  //! initialize evaluation tree
  static void initialize_evaluation( void );
  
  //! close evaluation tree
  static void finish_evaluation( void );
	
  //! fill local evaluation tree
  static void fill_evaluation_ntuple( TMutClusMap *map );
  
  //! debugging information structure to fill in ntuple
  class DebugInfo {
    public:
    
    //! constructor (initialize everything to 0)
    DebugInfo( void ) :
      is_multiple( 0 ),
      width_over_int( 0 ),
      width_over_double( 0 ),
      ac( 0 ), cc( 0 ),
      n_tracks_1( 0 ), chi2_1( 0 ), 
      n_tracks_2( 0 ), chi2_2( 0 ),
      error_from_fit( 0.),
      error_from_pulldist( 0.),
      fit_2_success( 0 ), 
      gsl_was_called( 0 )
    {
      x_fit_1.assign( 0 );
      q_fit_1.assign( 0 );
      x_err_1.assign( 0 );
      q_err_1.assign( 0 );
 
      x_fit_2.assign( 0 );
      q_fit_2.assign( 0 );
      x_err_2.assign( 0 );
      q_err_2.assign( 0 );

      min_charges.assign( -1000. );
      gains.assign( -1000. );
      pedestals.assign( -1000. );
      rms.assign( -1000. );
    };
    
    //! max number of tracks stored locally
    static const int max_tracks = 4;
    int is_multiple;          //!< set to 1 if cluster have multiple peaks
    int width_over_int;       //!< cluster width, integer version    
    double width_over_double; //!< cluster width, double version
    double ac;                //!< anode coupling
    double cc;                //!< cathode coupling
    
    int n_tracks_1; //!< number of fitted tracks after first try
    boost::array< double, max_tracks > x_fit_1;   //!< fit position 
    boost::array< double, max_tracks > q_fit_1;   //!< fit charge
    boost::array< double, max_tracks > x_err_1;   //!< error on position
    boost::array< double, max_tracks > q_err_1;   //!< error on charge
    double chi2_1;    //!< chisquare
    
    int n_tracks_2; //!< number of fitted tracks after second try
    boost::array< double, max_tracks > x_fit_2;   //!< fit position
    boost::array< double, max_tracks > q_fit_2;   //!< fit charge 
    boost::array< double, max_tracks > x_err_2;   //!< error on position
    boost::array< double, max_tracks > q_err_2;   //!< error on charge
    double chi2_2;    //!< chisquare

    boost::array< double, max_tracks > min_charges;   //!< error on charge
    boost::array< double, max_tracks > gains;         //!< calib gains
    boost::array< double, max_tracks > pedestals;     //!< calib pedestals
    boost::array< double, max_tracks > rms;           //!< rms on calib samples 2,3,4

    double error_from_fit;
    double error_from_pulldist;
    
    int fit_2_success;  //!< set to 1 if second fit was successful and accepted
    int gsl_was_called; //!< set to 1 if gsl mathieson fit was called
  };

  //! reset debug info map
  static void reset( void )
  { _debug_map.clear(); }
 
  //! adds a debug info to local_map
  static void add_debug_info( unsigned int key, const DebugInfo& debug_info )
  { _debug_map.insert( std::pair< unsigned int, DebugInfo >( key, debug_info ) ); }
  
  private:
  
  //! true if evaluation is to be done
  static bool _do_evaluation;

  //! map of all debug info fitted clusters vs cluster object key
  static std::map< unsigned int, DebugInfo > _debug_map;
  
  //! ntuple filename
  static std::string _filename;
  
  //! evaluation ntuple
  static TNtuple* _ntuple;  

};

#endif
