// $Id: mMutSlowSimPar.h,v 1.24 2013/01/18 13:25:04 rseidl Exp $
#ifndef __MMUTSLOWSIMPAR_HH__
#define __MMUTSLOWSIMPAR_HH__

//////////////////////////////////////////////////////////////////
/*!
\file    mMutSlowSimPar.h
\brief   Runtime parameter object for mMutSlowSim analysis module
\author  S. Kelly
\version $Revision: 1.24 $
\date    $Date: 2013/01/18 13:25:04 $
*/
//////////////////////////////////////////////////////////////////

#include<PHObject.h>
#include<PHException.h>
#include<TMutParBase.h>
#include<boost/array.hpp>
#include<iostream>

//! Runtime parameter object for mMutSlowSim analysis module
class mMutSlowSimPar : public TMutParBase
{

  public:

  /*! default constructor */
  mMutSlowSimPar() :
    _lorentz_base( 0 ),
    _w_smear(0.0),
    _primary_mode(UNKNOWN)
  {
    // correlation width
    set_correlation_width(0.0);
    
    // angular acceptance
    set_angular_acceptance(MUTOO::North,MUTOO::Station1,std::make_pair(10,35));
    set_angular_acceptance(MUTOO::North,MUTOO::Station2,std::make_pair(10,35));
    set_angular_acceptance(MUTOO::North,MUTOO::Station3,std::make_pair(10,35));
    set_angular_acceptance(MUTOO::South,MUTOO::Station1,std::make_pair(12,35));
    set_angular_acceptance(MUTOO::South,MUTOO::Station2,std::make_pair(12,35));
    set_angular_acceptance(MUTOO::South,MUTOO::Station3,std::make_pair(12,35));
  }

  /*! destructor */
  ~mMutSlowSimPar(){;}

  //! PHOOL inteface requirement
  void identify(std::ostream& os = std::cout) const
  { os << "mMutSlowSimPar";}

  //! number of parameters
  enum {
    n_acceptance_parameters = MUTOO::NumberOfArms*MUTOO::NumberOfStations,
  };

  //! shortcut for angular acceptance parameters
  typedef std::pair< double, double > angular_acceptance;

  //! Theta acceptance range for specified arm, station
  void set_angular_acceptance(int arm, int station, const angular_acceptance& range)
  { _angular_acceptance[get_acceptance_index( arm, station )] = range; }

  //! Theta acceptance range for specified arm, station
  angular_acceptance get_angular_acceptance(int arm, int station) const
  { return _angular_acceptance[get_acceptance_index( arm, station )]; }

  /*! Smear in w coordinate  (Gaussian sigma in cm) */
  double get_w_smear() const
  { return _w_smear; }

  /*! Smear in w coordinate  (Gaussian sigma in cm) */
  void set_w_smear(double w_smear)
  { _w_smear = w_smear; }

  /*! charge correlation width between cathodes */
  void set_correlation_width( double value )
  {
    for(int iarm=0;iarm<MUTOO::NumberOfArms;iarm++)
    for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
    for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
      set_correlation_width(iarm,ista,igap,value);
  }

  /*! charge correlation_width between cathodes */
  void set_correlation_width( const int& arm, const int& station, const int& gap, const double& value )
  { _correlation_width[gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm) ] = value; }
  
  /*! charge correlation_width between cathodes */
  const double& get_correlation_width( const int& arm, const int& station, const int& gap ) const
  { return _correlation_width[gap + MUTOO::NumberOfGaps*( station + MUTOO::NumberOfStations* arm )]; }
  
  /*! Base parameter for determing Lorentz angle error */
  double get_lorentz_base() const
  { return _lorentz_base;}

  /*! Base parameter for determing Lorentz angle error */
  void set_lorentz_base(double lorentz_base)
  { _lorentz_base = lorentz_base;}

  /*! Enumeration indicating primary inputs */
  enum PrimaryMode {UNKNOWN, JPSI_MUON, SINGLE};

  /*! Enumeration indicating primary inputs */
  PrimaryMode get_primary_mode() const
  { return _primary_mode;}

  /*! Enumeration indicating primary inputs */
  void set_primary_mode(PrimaryMode mode)
  {_primary_mode = mode;}

  //! dump all parameters
  void print( std::ostream& out = std::cout )
  {
    MUTOO::PRINT( out, "mMutSlowSimPar" );
    out << "_lorentz_base = " << _lorentz_base << "rad.\n";
    out << "_w_smear = " << _w_smear << "cm.\n";
    out << "_primary_mode = " << _primary_mode << ".\n";

    // correlation width
    for(int iarm=0;iarm<MUTOO::NumberOfArms;iarm++)
    for(int ista=0;ista<MUTOO::NumberOfStations;ista++)
    for(int igap=0;igap<MUTOO::NumberOfGaps;igap++)
      out << "_correlation_width[" << iarm << "," << ista << "," << igap << "] = "
	  << get_correlation_width(iarm,ista,igap)
	  << std::endl;
    
    // angular acceptance
    for( int arm=0; arm<MUTOO::NumberOfArms; arm++ )
      for( int station=0; station < MUTOO::NumberOfStations; station++ )
      out << "_angular_acceptance[" << arm << "," << station << "]=("
      << _angular_acceptance[get_acceptance_index( arm, station )].first << ","
      <<_angular_acceptance[get_acceptance_index( arm, station )].second << ")"
      << std::endl;

    MUTOO::PRINT( out, "**" );
  }

  //! \brief return unique index for arm/station
  static int get_acceptance_index( int arm, int station )
  { return station + MUTOO::NumberOfStations*arm; }


  //! angular acceptance
  boost::array< angular_acceptance, n_acceptance_parameters  > _angular_acceptance;

  double _lorentz_base;
  double _w_smear;

  /*! charge correlation width between cathodes */
  boost::array<double,18> _correlation_width;

  PrimaryMode _primary_mode;
};

#endif /* __MMUTSLOWSIMEPAR_HH__ */
