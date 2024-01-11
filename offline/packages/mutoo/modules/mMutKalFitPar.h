// $Id: mMutKalFitPar.h,v 1.49 2013/08/22 15:26:48 brooks Exp $
#ifndef __MMUTKALFITPAR_H__
#define __MMUTKALFITPAR_H__

/*!
\file mMutKalFitPar.h
\brief Runtime parameter object for mMutKalFit analysis module
\author Sean Kelly, Hugo Pereira
\version $Revision: 1.49 $
\date $Date: 2013/08/22 15:26:48 $
*/

#include<MUIOO.h>
#include<MUTOO.h>
#include<PHObject.h>
#include<PHException.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>

#include<set>
#include<boost/array.hpp>

/*!
\class mMutKalFitPar
\brief Runtime parameter object for mMutKalFit analysis module
*/

class mMutKalFitPar : public TMutParBase
{
  public:

  /*! default constructor */
  mMutKalFitPar():
    _momentum_resolution( 0.5 ),
    _angular_resolution( 0.2 ),
    _position_resolution( 100 ),
    _z_reference( 0 ),
    _chi_cut( 0.01 ),
    _min_n_coord( 6 ),
    _max_iterations( 100 ),
    _use_anodes( false ),
    _use_muid( false ),
    _update_cov_matrix( true ),
    _extrapolate_to_muid( true ),
    _evaluation_mode( NONE ),
    _evaluation_file( "mMutKalFit.root" )
  {

    _mutr_detector_weight.assign( 1 );

    TMutParameterDB::get().get<unsigned short>( "mMutKalFit_verbosity", _verbosity );
    TMutParameterDB::get().get<Double_t>( "mMutKalFit_momentum_resolution", _momentum_resolution );
    TMutParameterDB::get().get<Double_t>( "mMutKalFit_angular_resolution", _angular_resolution );
    TMutParameterDB::get().get<Double_t>( "mMutKalFit_position_resolution", _position_resolution );
    TMutParameterDB::get().get<Double_t>( "mMutKalFit_z_reference", _z_reference );
    TMutParameterDB::get().get<Double_t>( "mMutKalFit_chi_cut", _chi_cut );
    TMutParameterDB::get().get<unsigned short>( "mMutKalFit_min_n_coord", _min_n_coord );
    TMutParameterDB::get().get<unsigned short>( "mMutKalFit_max_iterations", _max_iterations );

    TMutParameterDB::get().get<bool>( "mMutKalFit_use_anodes", _use_anodes );
    TMutParameterDB::get().get<bool>( "mMutKalFit_use_muid", _use_muid );
    TMutParameterDB::get().get<bool>( "mMutKalFit_update_cov_matrix", _update_cov_matrix );
    TMutParameterDB::get().get<bool>( "mMutKalFit_extrapolate_to_muid", _extrapolate_to_muid );
    TMutParameterDB::get().get<unsigned int>( "mMutKalFit_evaluation_mode", _evaluation_mode );
    TMutParameterDB::get().get<std::string>( "mMutKalFit_evaluation_file", _evaluation_file );
  }

  //! destructor
  ~mMutKalFitPar(){ }

  //! shortcut for pdetector set
  typedef std::set< int > detector_set;

  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const
  { os << "mMutKalFitPar";}

  //! gets starting point momentum resolution
  Double_t get_momentum_resolution( void ) const
  { return _momentum_resolution; }

  //! sets starting point momentum resolution
  void set_momentum_resolution( Double_t value )
  { _momentum_resolution = value; }

  //! gets starting point angular resolution
  Double_t get_angular_resolution( void ) const
  { return _angular_resolution; }

  //! sets starting point angular resolution
  void set_angular_resolution( Double_t value )
  { _angular_resolution = value; }

  //! gets starting point position resolution
  Double_t get_position_resolution( void ) const
  { return _position_resolution; }

  //! sets starting point position resolution
  void set_position_resolution( Double_t value )
  { _position_resolution = value; }

  //! gets reference z for extrapolation upstream of the absorber
  Double_t get_z_reference( void ) const
  { return _z_reference; }

  //! sets reference z for extrapolation upstream of the absorber
  void set_z_reference( Double_t value )
  { _z_reference = value; }

  //! gets min number of coordinates / track to be fitted
  unsigned short get_min_n_coord( void ) const
  { return _min_n_coord; }

  //! sets min number of coordinates / track to be fitted
  void set_min_n_coord( unsigned short value )
  { _min_n_coord = value; }

  //! sets max number of iterations during fit
  void set_max_iterations( unsigned short value )
  { _max_iterations = value; }

  //! gets max number of iterations during fit
  unsigned short get_max_iterations( void ) const
  { return _max_iterations; }

  //! gets cut on relative chi_square difference to stop iterations
  Double_t get_chi_cut( void ) const
  { return _chi_cut; }

  //! sets cut on relative chi_square difference to stop iterations
  void set_chi_cut( Double_t value )
  { _chi_cut = value; }

  //! gets use_anodes correction flag
  bool get_use_anodes() const
  { return _use_anodes; }

  //! sets use_anodes correction flag
  void set_use_anodes( bool value )
  { _use_anodes = value; }

  //! gets use_muid flag
  bool get_use_muid() const
  { return _use_muid; }

  //! sets use_muid flag
  void set_use_muid( bool value )
  { _use_muid = value; }

  //! gets update_cov_matrix (from starting point, between iterations)
  bool get_update_cov_matrix() const
  { return _update_cov_matrix; }

  //! sets update_cov_matrix (from starting point, between iterations)
  void set_update_cov_matrix( bool value )
  { _update_cov_matrix = value; }

  //! extrapolation to muid
  bool get_extrapolate_to_muid( void ) const
  { return _extrapolate_to_muid; }

  //! extrapolation to muid
  void set_extrapolate_to_muid( bool value )
  { _extrapolate_to_muid = value; }

  //! evaluation mode
  enum EvalMode
  {
    NONE = 0,
    RESIDUAL = 1<<0,
    VERTEX_EXTRAPOLATION = 1 << 1
  };

  //! evaluation mode (bitwise or of EvalMode
  const unsigned int& get_evaluation_mode( void ) const
  { return _evaluation_mode; }

  //! evaluation mode (bitwise or of EvalMode)
  void set_evaluation_mode( const unsigned int& mode )
  { _evaluation_mode = mode; }

  //! gets histogram file
  std::string get_evaluation_file( void ) const
  { return _evaluation_file; }

  //! sets histogram file
  void set_evaluation_file( const std::string& value )
  { _evaluation_file = value; }

  //! desactivated mutr detectors
  bool get_mutr_desactivated( int arm, int station, int gap, int cathode )
  {
    int index( get_mutr_index( arm, station, gap, cathode ) );
    return !( _mutr_desactivated.find( index ) == _mutr_desactivated.end() );
  }

  //! desactivate mutr detectors
  void set_mutr_desactivated( int arm, int station, int gap, int cathode )
  { _mutr_desactivated.insert( get_mutr_index( arm, station, gap, cathode ) ); }

  //! activate mutr detectors
  void set_mutr_activated( int arm, int station, int gap, int cathode )
  { _mutr_desactivated.erase( get_mutr_index( arm, station, gap, cathode ) ); }

  //! desactivated muid detectors
  bool get_muid_desactivated( int arm, int plane, int panel )
  {
    int index( get_muid_index( arm, plane, panel ) );
    return !( _muid_desactivated.find( index ) == _muid_desactivated.end() );
  }

  //! desactivate  muid detectors
  void set_muid_desactivated( int arm, int plane, int panel )
  { _muid_desactivated.insert( get_muid_index( arm, plane, panel ) ); }

  //! activate  muid detectors
  void set_muid_activated( int arm, int plane, int panel )
  { _muid_desactivated.erase( get_muid_index( arm, plane, panel ) ); }

  //! mutr detector weight in chisquare
  double get_mutr_detector_weight( int arm, int station, int gap, int cathode ) const
  { return _mutr_detector_weight[ get_mutr_index( arm, station, gap, cathode ) ]; }

  //! mutr detector weight in chisquare
  void set_mutr_detector_weight( int arm, int station, int gap, int cathode, double weight )
  { _mutr_detector_weight[ get_mutr_index( arm, station, gap, cathode ) ] = weight; }

  //! dump all parameters
  void print( std::ostream& out = std::cout )
  {
    MUTOO::PRINT( out, "mMutKalFitPar" );
    out << "_verbosity = " << _verbosity << ".\n";
    out << "_momentum_resolution = " << _momentum_resolution << "GeV.\n";
    out << "_angular_resolution = " << _angular_resolution << "rad.\n";
    out << "_position_resolution = " << _position_resolution << "cm.\n";
    out << "_z_reference = " << _z_reference << "cm.\n";
    out << "_use_anodes = " << _use_anodes << ".\n";
    out << "_update_cov_matrix = " << _update_cov_matrix << ".\n";
    out << "_min_n_coord = " << _min_n_coord << ".\n";
    out << "_max_iterations = " << _max_iterations << ".\n";
    out << "_chi_cut = " << _chi_cut << ".\n";

    out << "_extrapolate_to_muid = " << _extrapolate_to_muid << "\n";
    out << "_evaluation_mode = " << _evaluation_mode << ".\n";
    out << "_evaluation_file = " << _evaluation_file << ".\n";

    // print desactivated detectors
    for( int arm = 0; arm < MUTOO::NumberOfArms; arm++ )
      for( int station = 0; station < MUTOO::NumberOfStations; station++ )
      for( int gap = 0; gap < MUTOO::NumberOfGaps; gap++ )
      for( int cathode = 0; cathode < MUTOO::NumberOfCathodePlanes; cathode++ )
      if( get_mutr_desactivated( arm, station, gap, cathode ) )
      out
      << "mutr arm=" << arm << " station=" << station << " gap=" << gap << " cathode=" << cathode
      << " desactivated"
      << std::endl;

    // print desactivated detectors
    for( int arm = 0; arm < MUTOO::NumberOfArms; arm++ )
      for( int plane = 0; plane < MUIOO::MAX_PLANE; plane++ )
      for( int panel = 0; panel < MUIOO::MAX_PANEL; panel++ )
      if( get_muid_desactivated( arm, plane, panel ) )
      out
      << "muid arm=" << arm << " plane=" << plane << " panel=" << panel
      << " desactivated"
      << std::endl;

    MUTOO::PRINT( out, "**" );
  }

  private:

  //! get unique index from mutr arm, station gap and cathode.
  static int get_mutr_index( int arm, int station, int gap, int cathode )
  {
    return
      cathode +
      MUTOO::NumberOfCathodePlanes*( gap +
      MUTOO::NumberOfGaps*( station +
      MUTOO::NumberOfStations*arm ) );
  }

  //! get unique index from muid arm, plane and panel.
  static int get_muid_index( int arm, int plane, int panel )
  {
    return
      panel +
      MUIOO::MAX_PANEL*( plane +
      MUIOO::MAX_PLANE*arm );
  }

  //! relative momentum resolution for starting covariance matrix [NO UNIT]. default is 0.5.
  Double_t _momentum_resolution;

  //! absolute angular resolution for starting covariance matrix [tangent(rad)]. default is 0.2.
  Double_t _angular_resolution;

  //! position along the beam for kalman filter end-extrapolation. default is 0.
  Double_t _position_resolution;

  //! position along the beam for kalman filter end-extrapolation. default is 0
  Double_t _z_reference;

  //! cut on relative chi2 increment to stop fit iterations [NO UNIT] default is 0.01.
  Double_t _chi_cut;

  //! minimal number of TMutCoord in track before filter [NO UNIT]. default is 6.
  unsigned short _min_n_coord;

  //! max number of filter/smooth iterations [NO UNIT]. default is 50.
  unsigned short _max_iterations;

  //! if true, anode wire position is used when calculating w_residuals. default is true
  bool _use_anodes;

  //! if true, adds the muid cluster parameters to the fit
  bool _use_muid;

  //! shortcut to total number of cathodes
  enum {
    number_of_cathodes =
    MUTOO::NumberOfArms*
    MUTOO::NumberOfStations*
    MUTOO::NumberOfGaps*
    MUTOO::NumberOfCathodePlanes
  };

  //! boolean array to tell if a detector is active or not
  boost::array< double, number_of_cathodes > _mutr_detector_weight;

  //! set of desactivated mutr detectors
  detector_set _mutr_desactivated;

  //! set of desactivated muid detectors
  detector_set _muid_desactivated;

  /*!
  if true, starting covariance matrix is updated between KF iteration using
  last hit smoothed parameters from previous iteration
  */
  bool _update_cov_matrix;

  //! if true, track parameters at station 3 are swimmed to muid gap0 through the field
  bool _extrapolate_to_muid;

  //! evaluation mode (bitwise or of EvalMode)
  unsigned int _evaluation_mode;

  //! filname where to write evaluation histograms
  std::string _evaluation_file;

};

#endif
