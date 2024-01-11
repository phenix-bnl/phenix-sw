#ifndef __MUTGEOML2UTIL_H__
#define __MUTGEOML2UTIL_H__

// $Id: MutGeomL2Util.h,v 1.2 2006/12/21 14:59:56 hpereira Exp $

/*! 
  \file MutGeomL2Util.h
  \brief level2 utility to dump calibration text files from postgres
  \author Hugo Pereira
  \version $Revision: 1.2 $
  \date $Date: 2006/12/21 14:59:56 $
*/

#include <iostream>
#include <string>
#include <PHPoint.h>

//! level2 utility to dump calibration text files from postgres
class MutGeomL2Util
{
  public:
    
  //! dump level2 calibration table
  static bool dump_calib( void );
  
  //! dump level2 calibration table
  static bool dump_geom( void );
    
  //! changes lvl2 calibration file name
  static void set_calib_file( const char* file )
  { if( file ) _calib_file = file; }
  
  //! changes lvl2 geometry file name
  static void set_geom_file( const char* file )
  { if( file ) _geom_file = file; }
  
  protected:
  static std::string _calib_file;   //!< level2 db calibration filename
  static std::string _geom_file;    //!< level2 db geometry filename

  //! structure to store/sort calibrations
  struct Lvl2Calib {
    
    //! empty constructor
    Lvl2Calib( void ):
      _packet( 0 ),
      _dcm_channel( 0 ),
      _gain( 0 ),
      _pedestal( 0 )
    {}
    
    //! filled constructor
    Lvl2Calib( const unsigned int &packet, const unsigned int &channel, const float& gain, const float& pedestal ):
      _packet( packet ),
      _dcm_channel( channel ),
      _gain( gain ),
      _pedestal( pedestal )
    {}
    
    //! lower than operator
    bool operator < ( const Lvl2Calib& calib ) const
    { 
      if( _packet < calib._packet ) return true;    
      else if( _packet == calib._packet ) return ( _dcm_channel < calib._dcm_channel );
      return false;
    }
     
    //! equal operator
    bool operator == ( const Lvl2Calib& calib ) const
    { return _packet == calib._packet && _dcm_channel == calib._dcm_channel; }
   
    //! print operator
    friend std::ostream &operator << (std::ostream &out,const Lvl2Calib &calib) 
    { 
      out << calib._packet << " " << calib._dcm_channel << " " << calib._gain << " " << calib._pedestal << std::endl;
      return out;
    }
    
    //! packet id
    unsigned int _packet;       

    //! dcm channel number
    unsigned int _dcm_channel;  

    //! strip gain
    float _gain;  
    
    //! strip pedestal
    float _pedestal;
    
  };

  //! structure to store/sort geometry
  struct Lvl2Geom {
    
    //! empty constructor
    Lvl2Geom( void ):
      _packet( 0 ),
      _dcm_channel( 0 ),
      _arm( 0 ),
      _station( 0 ),
      _half_octant( 0 ),
      _gap( 0 ),
      _cathode( 0 ),
      _strip( 0 )
    {}
    
    //! lower than operator
    bool operator < ( const Lvl2Geom& geom ) const
    { 
      if( _packet < geom._packet ) return true;    
      else if( _packet == geom._packet ) return ( _dcm_channel < geom._dcm_channel );
      return false;
    }
     
    //! equal operator
    bool operator == ( const Lvl2Geom& geom ) const
    { return _packet == geom._packet && _dcm_channel == geom._dcm_channel; }
   
    //! print operator
    friend std::ostream &operator << (std::ostream &out,const Lvl2Geom &geom) 
    { 
      out << geom._packet << " " << geom._dcm_channel << " " 
        << geom._arm << " " << geom._station << " " << geom._half_octant << " " << geom._gap << " " << geom._cathode << " " << geom._strip << " "
        << geom._first_point.getX()  << " " << geom._first_point.getY()  << " " << geom._first_point.getZ()  << " "
        << geom._second_point.getX() << " " << geom._second_point.getY() << " " << geom._second_point.getZ() << std::endl;
      return out;
    }
    
    //! packet id
    unsigned int _packet;       
    
    //! dcm channel number
    unsigned int _dcm_channel;  

    //! parent arm id
    unsigned int _arm; 
    
    //! parent station id
    unsigned int _station; 
    
    //! parent half octant id
    unsigned int _half_octant; 

    //! parent gap id
    unsigned int _gap; 
    
    //! parent cathode id
    unsigned int _cathode; 
    
    //! parent strip id
    unsigned int _strip;
    
    //! strip first point
    PHPoint _first_point;
    
    //! strip second point
    PHPoint _second_point;
    
  };

};

#endif
