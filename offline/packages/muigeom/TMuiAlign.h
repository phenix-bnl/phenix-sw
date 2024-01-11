#ifndef __TMuiAligh_h__
#define __TMuiAligh_h__

// $Id: TMuiAlign.h,v 1.6 2006/12/20 17:04:58 hpereira Exp $

/*!
  \file    TMuiAlign.h
  \brief   Statically scoped class for muon identifier alignment corrections from file
  \author  Hugo Pereira
  \version $Revision: 1.6 $
  \date    $Date: 2006/12/20 17:04:58 $
*/

#include<iostream>
#include<set>
#include<string>

//!   Statically scoped class for muon identifier alignment correctons from file
class TMuiAlign
{

  public:

  //! read alignment from file, update geometry consequently
  static void init( const std::string& filename = "alignment_corrections.txt" );

  //! reset stored alignment parameters
  /*! warning: Reset do not re-update the geometry */
  static void reset( void )
  { _panel_parameters.clear(); }

  //! update geometry consequently
  static void update_geometry();

  //! write alignment parameters
  static void print_parameters( std::ostream& out = std::cout );

  //! reset parameters
  static void reset_parameters ( void)
  { _panel_parameters.clear(); }

  //! anode tag in alignment
  static const std::string MUID_PANEL;

  //! set of alignment parameters for a given panel
  class PanelParameters {

    public:

    //! default constructor
    PanelParameters(
        int arm = 0, int plane = 0, int panel = 0,
        double delta_x = 0, double delta_y = 0, double delta_z = 0,
        double delta_phi = 0 ):
      _arm( arm ),
      _plane( plane ),
      _panel( panel ),
      _delta_x( delta_x ),
      _delta_y( delta_y ),
      _delta_z( delta_z ),
      _delta_phi( delta_phi )
    {};

    //! arm index [0 to 1]
    int _arm;

    //! plane index [0 to 4]
    int _plane;

    //! panel index [0 to 5]
    int _panel;

    //! detector misalignment along x
    double _delta_x;

    //! detector misalignment along y
    double _delta_y;

    //! detector misalignment along z
    double _delta_z;

    //! detector angular misalignment
    double _delta_phi;

    //! equal to operator (compare parameters location)
    bool operator == (const PanelParameters& par ) const
    {
      return(
        _arm == par._arm &&
        _plane == par._plane &&
        _panel == par._panel
      );
    }

    //! inferior to operator
    bool operator < (const PanelParameters& par ) const
    {
      if( _arm != par._arm ) return _arm < par._arm;
      else if( _plane != par._plane )  return _plane < par._plane;
      else if( _panel != par._panel )  return _panel < par._panel;
      else return false;
    }


    //! read parameters from stream
    friend std::istream &operator >> ( std::istream &in, PanelParameters& par )
    {
      in
        >> par._arm >> par._plane >> par._panel
        >> par._delta_x >> par._delta_y >> par._delta_z >> par._delta_phi;
      return in;
    }

    //! write parameters to stream
    friend std::ostream &operator << ( std::ostream &out, const PanelParameters& par )
    {
      out << "["
        << par._arm << ","
        << par._plane << ","
        << par._panel << "] "
        << "delta_x=" << par._delta_x << "cm "
        << "delta_y=" << par._delta_y << "cm "
        << "delta_z=" << par._delta_z << "cm "
        << "delta_phi=" << par._delta_phi << "rad";
      return out;
    }

  };

  //! gets alignment parameters for given cathode
  static PanelParameters get_panel_parameters( int arm, int plane, int panel );

  private:

  //! shortcut for cathode parameter set
  typedef std::set< PanelParameters > panel_parameter_set;

  //! parameter set (from file)
  static panel_parameter_set _panel_parameters;

};

#endif
