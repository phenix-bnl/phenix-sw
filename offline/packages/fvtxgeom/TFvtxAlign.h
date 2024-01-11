// $Id: TFvtxAlign.h,v 1.4 2012/08/23 21:19:20 jinhuang Exp $
#ifndef __TFvtxAligh_h__
#define __TFvtxAligh_h__

/*!
  \file TFvtxAlign.h
  \brief Statically scoped class for fvtx alignment corrections from file
  \author Zhengyun You
  \version $Revision: 1.4 $
  \date $Date: 2012/08/23 21:19:20 $
*/

#include<iostream>
#include<set>
#include<string>

//! Statically scoped class for muon tracker alignment correctons from file
class TFvtxAlign
{
  
  public:

  static void TFvtxAlign_Test() {std::cout<<"TFvtxAlign::TFvtxAlign()"<<std::endl;};
      
  //! read alignment from files
  static void init( const std::string& filename = "fvtx_alignment_corrections.txt" );
  
  //! reset all stored parameters
  /*! warning: Reset do not re-update the geometry */
  static void reset( void )
  {
    _station_parameters.clear();
    _wedge_parameters.clear();
  }
  
  //! update geometry according to alignment parameters
  static void update_geometry();
  
  //! print geometry
  static void print_geometry( std::ostream& out = std::cout );
  
  //! print disalignments in database format
  static void convert_to_db( std::ostream& out = std::cout );
  
  //! write alignment parameters
  static void print_parameters( std::ostream& out = std::cout );
    
  //! reset parameters
  static void reset_parameters ( void) 
  {
    _station_parameters.clear();
    _wedge_parameters.clear();
  }
  
  //! tag in alignment file
  static const std::string STATION;
  static const std::string WEDGE;
  
  //! set of alignment parameters for a given station
  class StationParameters
  {

    public:
    
    //! default constructor
    StationParameters(
        int arm = 0,
        int cage = 0,
        int station = 0,
        double delta_x = 0,
        double delta_y = 0,
        double delta_z = 0,
        double delta_phi = 0,
	double delta_psix = 0,
	double delta_psiy = 0
    ):
      _arm( arm ),
      _cage( cage ),
      _station( station ),
      _delta_x( delta_x ),
      _delta_y( delta_y ),
      _delta_z( delta_z ),
      _delta_phi( delta_phi ),
      _delta_psix( delta_psix ),
      _delta_psiy( delta_psiy )
    {};
            
    //! arm location
    int _arm;
    
    //! cage location
    int _cage;

    //! station location
    int _station;
    
    //! detector misalignment along x
    double _delta_x; 
    
    //! detector misalignment along y
    double _delta_y; 
    
    //! detector misalignment along z
    double _delta_z;
    
    //! detector angular misalignment (in xOy)
    double _delta_phi;    

    //! detector angular misalignment (in yOz)
    double _delta_psix;

    //! detector angular misalignment (in xOz)
    double _delta_psiy;


    //! equal to operator (compare parameters location)
    bool operator == (const StationParameters& par ) const
    {
      return(
        _arm == par._arm &&
        _cage == par._cage &&
        _station == par._station
      );
    }

    //! inferior to operator
    bool operator < (const StationParameters& par ) const
    {
      if( _arm != par._arm ) return _arm < par._arm;
      else if( _cage != par._cage )  return _cage < par._cage;
      else if( _station != par._station )  return _station < par._station;
      else return false;
    }


    //! read parameters from stream
    friend std::istream &operator >> ( std::istream &in, StationParameters& par )
    {
      in  >> par._arm >> par._cage >> par._station 
          >> par._delta_x >> par._delta_y >>  par._delta_z
	  >> par._delta_phi >> par._delta_psix >> par._delta_psiy;
      return in;
    }

    //! write parameters to stream
    friend std::ostream &operator << ( std::ostream &out, const StationParameters& par )
    {
      out << "["
          << par._arm << ","
          << par._cage << ","
          << par._station << ","
          << "delta_x=" << par._delta_x << "cm "
          << "delta_y=" << par._delta_y << "cm "
          << "delta_z=" << par._delta_z << "cm "
          << "delta_phi=" << par._delta_phi << "rad"
          << "delta_psix=" << par._delta_psix << "rad"
          << "delta_psiy=" << par._delta_psiy << "rad";
      return out;
    }
    
  };

  //! set of alignment parameters for a given wedge
  class WedgeParameters
  {
    
    public:
    
    //! default constructor
    WedgeParameters( 
        int arm = 0,
        int cage = 0,
        int station = 0,
        int sector = 0,
        double delta_x = 0,
        double delta_y = 0,
	double delta_z = 0,
        double delta_phi = 0
    ):
      _arm( arm ),
      _cage( cage ),
      _station( station ),
      _sector( sector ),
      _delta_x( delta_x ),
      _delta_y( delta_y ),
      _delta_z( delta_z ),
      _delta_phi( delta_phi )
    {};
            
    //! arm location
    int _arm;
    
    //! cage location
    int _cage;

    //! station location
    int _station;
    
    //! sector location
    int _sector;
    
    //! detector misalignment along x
    double _delta_x; 
    
    //! detector misalignment along y
    double _delta_y; 
   
    //! detector misalignment along z
    double _delta_z;
 
    //! detector angular misalignment (in xOy)
    double _delta_phi;    

    //! equal to operator (compare parameters location)
    bool operator == (const WedgeParameters& par ) const
    { 
      return( 
        _arm == par._arm &&
        _cage == par._cage &&
        _station == par._station &&
        _sector == par._sector
      ); 
    }  
    
    //! inferior to operator
    bool operator < (const WedgeParameters& par ) const
    { 
      if( _arm != par._arm ) return _arm < par._arm;
      else if( _cage != par._cage )  return _cage < par._cage;
      else if( _station != par._station )  return _station < par._station;
      else if( _sector != par._sector )  return _sector < par._sector;
      else return false;
    }  
    
    
    //! read parameters from stream
    friend std::istream &operator >> ( std::istream &in, WedgeParameters& par )
    {
      in  >> par._arm >> par._cage >> par._station >> par._sector
          >> par._delta_x >> par._delta_y >>  par._delta_z >> par._delta_phi;
      return in;
    }
    
    //! write parameters to stream
    friend std::ostream &operator << ( std::ostream &out, const WedgeParameters& par )
    {
      out << "[" 
          << par._arm << "," 
          << par._cage << ","
          << par._station << "," 
          << par._sector << "]"
          << "delta_x=" << par._delta_x << "cm "
          << "delta_y=" << par._delta_y << "cm "
          << "delta_z=" << par._delta_z << "cm "
          << "delta_phi=" << par._delta_phi << "rad";
      return out;
    }
    
  };

  //! gets alignment parameters for given station
  static StationParameters get_station_parameters( int arm, int cage, int station );
  
  //! gets alignment parameters for given wedge
  static WedgeParameters get_wedge_parameters( int arm, int cage, int station, int sector );

  private:
 
  //! shortcut for station parameter set
  typedef std::set< StationParameters > station_parameter_set;
  
  //! parameter set (from file)
  static station_parameter_set _station_parameters;
 
  //! shortcut for wedge parameter set
  typedef std::set< WedgeParameters > wedge_parameter_set;
  
  //! parameter set (from file)
  static wedge_parameter_set _wedge_parameters;    
  
};

#endif
