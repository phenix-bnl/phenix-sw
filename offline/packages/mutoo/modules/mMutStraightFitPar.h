// $Id: mMutStraightFitPar.h,v 1.8 2010/06/03 19:20:55 hpereira Exp $
#ifndef __mMutStraightFitPar_h__
#define __mMutStraightFitPar_h__

/*!
\file    mMutStraightFitPar.h
\brief   Runtime parameter object for mMutStraightFit analysis module
\author  Catherine Silvestre
\version $Revision: 1.8 $
\date    $Date: 2010/06/03 19:20:55 $
*/
#include <MUIOO.h>
#include <MUTOO.h>
#include <PHGslMatrix.h>
#include <PHTimeServer.h>
#include <list>
#include <set>

#include "TMutParBase.h"

/*!
\class mMutStraightFitPar
\brief Runtime parameter object for mMutStraightFit analysis module
*/
class mMutStraightFitPar : public TMutParBase
{

  public:

  //! default constructor
  mMutStraightFitPar():
    _use_muid( false ),
    _z_reference( 0 )
  {}

  //! destructor
  virtual ~mMutStraightFitPar(){ }

  //! shortcut for pdetector set
  typedef std::set< int > detector_set;

  //! use muid clusters in fit
  bool get_use_muid( void ) const
  { return _use_muid; }

  //! use muid clusters in fit
  void set_use_muid( bool value )
  { _use_muid = value; }

  //! gets reference z for extrapolation upstream of the absorber
  Double_t get_z_reference( void ) const
  { return _z_reference; }

  //! sets reference z for extrapolation upstream of the absorber
  void set_z_reference( Double_t value )
  { _z_reference = value; }

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

  //! print module parameters
  void print( std::ostream& out = std::cout )
  {

    MUTOO::PRINT( out, "mMutStraightFitPar::print" );
    out << "_use_muid: " << _use_muid << std::endl;
    out << "_z_reference = " << _z_reference << "cm.\n";

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
    for( int arm = 0; arm < MUIOO::MAX_ARM; arm++ )
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

  //! use muid clusters in fit
  bool _use_muid;

  //! position along the beam for kalman filter end-extrapolation. default is 0
  Double_t _z_reference;

  //! set of desactivated mutr detectors
  detector_set _mutr_desactivated;

  //! set of desactivated muid detectors
  detector_set _muid_desactivated;

};

#endif

