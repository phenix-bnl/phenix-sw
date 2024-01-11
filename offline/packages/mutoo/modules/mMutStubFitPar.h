// $Id: mMutStubFitPar.h,v 1.9 2013/08/22 15:27:45 brooks Exp $
#ifndef __MMUTSTUBFITPAR_HH__
#define __MMUTSTUBFITPAR_HH__

#include<PHObject.h>
#include<PHException.h>
#include<TMutParBase.h>
#include<MUTOO.h>
#include<boost/array.hpp>

//!Runtime parameter object for mMutStubFit analysis module
/*! 
Runtime parameter object for mMutStubFit analysis module
*/
class mMutStubFitPar : public TMutParBase
{  
 public: 
  /*! default constructor */
  mMutStubFitPar() :
    _epsabs(1e-8),
    _max_iterations(100),
    _min_n_coord(4),
    _residual_mode(NORMAL),
    _use_anodes( false ),
    _use_fast_stub_fit( false ),
    _min_n_gap_coord(2),
    _ghost_reject(true),
    _min_stub_diff(0.2),
    _common_coord_threshold(4)
    {
      // All stations active by default
      //
      set_station_active(0,true);
      set_station_active(1,true);
      set_station_active(2,true);
    }
  
  /*! destructor */
  ~mMutStubFitPar(){;}
  
  /*! Convergence criteria for linear fit */
  void set_epsabs(double epsabs)
  {_epsabs = epsabs;}
  
  /*! Convergence criteria for linear fit */
  double get_epsabs() const 
  {return _epsabs;}
  
  /*! Maximum number of iterations */
  void set_max_iterations(unsigned short max_iterations)
  {_max_iterations = max_iterations;}
  
  /*! Maximum number of iterations */
  unsigned short get_max_iterations() const 
  {return _max_iterations;}
  
  /*! Minimum number of coordinates to proceed with fit */
  void set_min_n_coord(unsigned short min_n_coord)
  {_min_n_coord = min_n_coord;}
  
  /*! Minimum number of coordinates to proceed with fit */
  unsigned short get_min_n_coord() const 
  {return _min_n_coord;}

  /*! Minimum number of gap coordinates to proceed with fit */
  void set_min_n_gap_coord(unsigned short min_n_gap_coord)
  {_min_n_gap_coord = min_n_gap_coord;}
  
  /*! Minimum number of gap coordinates to proceed with fit */
  unsigned short get_min_n_gap_coord() const 
  {return _min_n_gap_coord;}
  
  enum ResidualMode {NO_RESIDUAL, NORMAL, UNBIASED};

  /*! Residual mode */
  void set_residual_mode(ResidualMode residual_mode)
  {_residual_mode = residual_mode;}

  /*! Residual mode */
  ResidualMode get_residual_mode() const 
  {return _residual_mode;}

  /*! Include anode wires in fit */
  bool get_use_anodes() const 
  { return _use_anodes; }

  /*! Include anode wires in fit */
  void set_use_anodes(bool value)
  {_use_anodes = value;}

  /*! use fast stub fit (non gsl chisquare analytic minimization */
  bool get_use_fast_stub_fit() const 
  { return _use_fast_stub_fit; }

  /*! use fast stub fit (non gsl chisquare analytic minimization */
  void set_use_fast_stub_fit( bool value )
  {_use_fast_stub_fit = value;}

  /*! Station mask */
  bool get_station_active(unsigned short station) const {
    BOUNDS_CHECK(station,MUTOO::NumberOfStations);
    return _station_active[station];
  }

  /*! 
    Station mask.  
  */
  void set_station_active(unsigned short station, bool value)
  {
    BOUNDS_CHECK(station,MUTOO::NumberOfStations);
    _station_active[station] = value;
  }

  /*! Ghost reject enable */
  void set_ghost_reject(double ghost_reject)
  {_ghost_reject = ghost_reject;}
  
  /*! Ghost reject enable */
  double get_ghost_reject() const
  {return _ghost_reject;}
  
  /*! Ghost rejection parameter */
  void set_min_stub_diff(double min_stub_diff)
  {_min_stub_diff = min_stub_diff;}
  
  /*! Ghost rejection parameter */
  double get_min_stub_diff() const 
  {return _min_stub_diff;}

  /*! Ghost rejection parameter */
  void set_common_coord_threshold(unsigned short common_coord_threshold)
  {_common_coord_threshold = common_coord_threshold;}
  
  /*! Ghost rejection parameter */
  unsigned short get_common_coord_threshold() const 
  {return _common_coord_threshold;}

  //! dump all parameters	
  void print( void ) {
    MUTOO::PRINT( std::cout, "mMutStubFitPar" );
    std::cout << "_verbosity = " << _verbosity << ".\n";
    std::cout << "_max_iterations = " << _max_iterations << ".\n";
    std::cout << "_min_n_coord = " << _min_n_coord << ".\n";
    std::cout << "_station_active: " << std::endl;
    
    for( int i=0; i<MUTOO::NumberOfStations; i++ )
    std::cout << "  station " << i << ": " << _station_active[i] << ".\n";
    
    std::cout << "_residual_mode = " << _residual_mode << ".\n";
    std::cout << "_use_anodes = " << _use_anodes << ".\n";
    std::cout << "_use_fast_stub_fit = " << _use_fast_stub_fit << ".\n";
    std::cout << "_min_n_gap_coord = " << _min_n_gap_coord << ".\n";
    std::cout << "_ghost_reject = " << _ghost_reject << ".\n";
    std::cout << "_min_stub_diff = " << _min_stub_diff << " cm.\n";
    std::cout << "_common_coord_threshold = " << _common_coord_threshold << ".\n";
    MUTOO::PRINT( std::cout, "**" );
  }

 private:  

  double _epsabs;
  unsigned short _max_iterations; //!< max number of iterations in gsl non linear fit
  unsigned short _min_n_coord;    //!< min number of coordinate/stub to perform fit
  
  //! residual caluclation mode
  ResidualMode _residual_mode;
  
  //! if 1, corresponding station is active 
  boost::array<bool, MUTOO::NumberOfStations > _station_active;
  
  bool _use_anodes;           //!< if true, use anode in gsl fit. Makes no sense if _use_fast_stub_fit is 1.
  bool _use_fast_stub_fit;    //!< if true, fast stub fit is used (no gsl). default is 1.
  unsigned short _min_n_gap_coord;      //!< min number of gap coordinates to perform fit
  bool _ghost_reject;             //!< if true, stub rejection is done 
  double _min_stub_diff;          //!< minimum distance between two stub for ghost rejection
  unsigned short _common_coord_threshold;
};

#endif /* __MMUTSTUBFIT_HH__ */







