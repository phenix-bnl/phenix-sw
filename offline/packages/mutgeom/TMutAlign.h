// $Id: TMutAlign.h,v 1.12 2014/10/01 22:18:16 jinhuang Exp $
#ifndef __TMutAligh_h__
#define __TMutAligh_h__

/*!
  \file TMutAlign.h
  \brief Statically scoped class for muon tracker alignment corrections from file
  \author Hugo Pereira
  \version $Revision: 1.12 $
  \date $Date: 2014/10/01 22:18:16 $
*/

#include<iostream>
#include<set>
#include<string>
#include<cassert>
#include "MutGeom.h"

//! Statically scoped class for muon tracker alignment correctons from file
class TMutAlign
{
  
  public:
      
  //! read alignment from files
  static void init( const std::string& filename = "alignment_corrections.txt" );
  
  //! reset all stored parameters
  /*! warning: Reset do not re-update the geometry */
  static void reset( void )
  {
    _cathode_parameters.clear();
    _anode_parameters.clear();
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
    _anode_parameters.clear();
    _cathode_parameters.clear();
  }
  
  //! cathode tag in alignment file
  static const std::string CATHODE;
  
  //! anode tag in alignment
  static const std::string ANODE;
  
  //! set of alignment parameters for a given cathode
  class CathodeParameters
  {
    
    public:
    
    //! default constructor
    CathodeParameters( 
        int arm = 0,
        int station = 0,
        int octant = 0,
        int gap = 0,
        int cathode = 0,
        double delta_x = 0,
        double delta_y = 0,
        double delta_phi = 0
    ):
      _valid(false),
      _arm( arm ),
      _station( station ),
      _octant( octant ),
      _gap( gap ),
      _cathode( cathode ),
      _delta_x( delta_x ),
      _delta_y( delta_y ),
      _delta_phi( delta_phi )
    {
      for( int half=0; half<MUTGEOM::NumberOfHalfOctants; half++ )
      _delta_phi_halfs[half] = 0;
    };

    bool _valid;
            
    //! arm location
    int _arm;
    
    //! station location
    int _station;
    
    //! octant location
    int _octant;
    
    //! gap location
    int _gap;
    
    //! cathode location
    int _cathode;
    
    //! detector misalignment along x
    double _delta_x; 
    
    //! detector misalignment along y
    double _delta_y; 
    
    //! detector angular misalignment (in xOy)
    double _delta_phi;    

    //! detector angular misalignment if two halfs use different rotations
    double _delta_phi_halfs[MUTGEOM::NumberOfHalfOctants];

    //! equal to operator (compare parameters location)
    bool operator == (const CathodeParameters& par ) const
    { 
      return( 
        _arm == par._arm &&
        _station == par._station &&
        _octant == par._octant &&
        _gap == par._gap &&
        _cathode == par._cathode
      ); 
    }  
    
    //! inferior to operator
    bool operator < (const CathodeParameters& par ) const
    { 
      if( _arm != par._arm ) return _arm < par._arm;
      else if( _station != par._station )  return _station < par._station;
      else if( _octant != par._octant )  return _octant < par._octant;
      else if( _gap != par._gap )  return _gap < par._gap;
      else if( _cathode != par._cathode )  return _cathode < par._cathode;
      else return false;
    }  
    
    
    //! read parameters from stream
    friend std::istream &operator >> ( std::istream &in, CathodeParameters& par )
    {
      in 
          >> par._arm >> par._station >> par._octant >> par._gap >> par._cathode 
          >> par._delta_x >> par._delta_y;
//      in >> par._delta_phi;

      double phi0=0, phi1=0;

      in >> phi0;
      assert(( !(in.rdstate() & std::ios::failbit ) ));

      par._valid = true;

      in >> phi1;
      if (( !(in.rdstate() & std::ios::failbit ) ))
        {
          static bool once = true;
          if (once)
            {
              once = false;
              std::cout << "CathodeParameters::operator >>: - input rotation"
                  << " for two halfs separately as " << phi0 << ", " << phi1
                  << " for the first cathode" << std::endl;
            }

          par._delta_phi = 0.5*(phi0 + phi1);
          par._delta_phi_halfs[0] = phi0;
          par._delta_phi_halfs[1] = phi1;
        }
      else
        {
          static bool once = true;
          if (once)
            {
              once = false;
              std::cout << "CathodeParameters::operator >>: - input rotation"
                  << " for two halfs with same angle as " << phi0 << " for the first cathode" << std::endl;
            }
          par._delta_phi = phi0;
          par._delta_phi_halfs[0] = phi0;
          par._delta_phi_halfs[1] = phi0;
        }

      return in;
    }
    
    //! write parameters to stream
    friend std::ostream &operator << ( std::ostream &out, const CathodeParameters& par )
    {
      out << "[" 
          << par._arm << "," 
          << par._station << "," 
          << par._octant << ","
          << par._gap << "," 
          << par._cathode << "] "
          << "delta_x=" << par._delta_x << "cm "
          << "delta_y=" << par._delta_y << "cm "
          << "delta_phi[halfs]=" << par._delta_phi_halfs[0] <<", "<< par._delta_phi_halfs[1] << "rad";
      return out;
    }
    
  };
  
  /*! \brief
    set of alignment parameters for a given anode
    only the anode z is taken care of in the reconstruction code
  */
  class AnodeParameters
  {
    
    public:
    
    //! default constructor
    AnodeParameters( 
        int arm = 0,
        int station = 0,
        int octant = 0,
        int gap = 0,
        double delta_z = 0
    ):
      _arm( arm ),
      _station( station ),
      _octant( octant ),
      _gap( gap ),
      _delta_z( delta_z )
    {}
        
    //! arm location
    int _arm;
    
    //! station location
    int _station;
    
    //! octant location
    int _octant;
    
    //! gap location
    int _gap;
            
    //! detector misalignment along y
    double _delta_z; 

    //! equal to operator (compare parameters location)
    bool operator == (const AnodeParameters& par ) const
    { 
      return( 
        _arm == par._arm &&
        _station == par._station &&
        _octant == par._octant &&
        _gap == par._gap
      ); 
    }  
    
    //! inferior to operator
    bool operator < (const AnodeParameters& par ) const
    { 
      if( _arm != par._arm ) return _arm < par._arm;
      else if( _station != par._station )  return _station < par._station;
      else if( _octant != par._octant )  return _octant < par._octant;
      else if( _gap != par._gap )  return _gap < par._gap;
      else return false;
    }  
    
    
    //! read parameters from stream
    friend std::istream &operator >> ( std::istream &in, AnodeParameters& par )
    {
      in 
          >> par._arm >> par._station >> par._octant >> par._gap  
          >> par._delta_z;
      return in;
    }
    
    //! write parameters to stream
    friend std::ostream &operator << ( std::ostream &out, const AnodeParameters& par )
    {
      out << "[" 
          << par._arm << "," 
          << par._station << "," 
          << par._octant << ","
          << par._gap << "] "
          << "delta_z=" << par._delta_z << "cm ";
      return out;
    }
    
  };
    
  //! gets alignment parameters for given cathode
  static CathodeParameters get_cathode_parameters( int arm, int station, int octant, int gap, int cathode );

  //! gets alignment parameters for given anode
  static AnodeParameters get_anode_parameters( int arm, int station, int octant, int gap );
  
  
  private:
  
  //! shortcut for cathode parameter set
  typedef std::set< CathodeParameters > cathode_parameter_set;
  
  //! parameter set (from file)
  static cathode_parameter_set _cathode_parameters;    
  
  //! shortcut for cathode parameter set
  typedef std::set< AnodeParameters > anode_parameter_set;
  
  //! parameter set (from file)
  static anode_parameter_set _anode_parameters;    
    
};

#endif
