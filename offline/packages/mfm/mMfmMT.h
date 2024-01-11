#ifndef __MMFMMT_H__
#define __MMFMMT_H__

#include <string>

/*!
   \file mMfmMT.h
   \brief static magnetic field initialization 
   \author Sean Kelly, Hugo Pereira Da Costa
   \version $Revision: 1.8 $
   \date $Date: 2008/07/28 10:11:42 $
*/

/*! \brief
  pure static class for magnetic field initialization 
*/
class mMfmMT
{

  public:

  //! enumeration to describe magnetic field maps
  enum MapFileFlag 
  {
    //! unknown map. Used for error condition
    UNKNOWN = -1,
      
    //! 1997 two dimensional map
    MAP_2D_1997 = 0,

    //! 2001 two dimensional map
    MAP_2D_2001 = 1,

    //! 2001 three dimensional map (south muon arm only)
    MAP_3D_2001 = 2,

    //! 2003 three dimensional map (both muon arms, single coil)
    MAP_3D_2003 = 3,
    
    //! 2004 (and later) three dimensional map (dual coils, ++ configuration)
    MAP_3D_PLUS_PLUS = 4,
    
    //! 2004 (and later) three dimensional map (dual coils, +- configuration)
    MAP_3D_PLUS_MINUS = 5
  };
  
  //! initialization
  static int initialize( void );

  //! get flag name matching enumeration
  static std::string getMapFileFlagName( void )
  { return getMapFileFlagName( getMapFileFlag() ); }

  //! get flag name matching enumeration
  static std::string getMapFileFlagName( int mapFile )
  {
    switch( mapFile )
    {
      case MAP_2D_1997: return "mMfmMT::MAP_2D_1997";
      case MAP_2D_2001: return "mMfmMT::MAP_2D_2001";
      case MAP_3D_2001: return "mMfmMT::MAP_3D_2001";
      case MAP_3D_2003: return "mMfmMT::MAP_3D_2003";
      case MAP_3D_PLUS_PLUS: return "mMfmMT::MAP_3D_PLUS_PLUS";
      case MAP_3D_PLUS_MINUS: return "mMfmMT::MAP_3D_PLUS_MINUS";
      default: return "unknown";
    }
  }
  
  /*! map file flag */
  static void setMapFileFlag(int mapFile);
  
  /*! map file flag */
  static int getMapFileFlag();

  /*! map file scale */
  static float getMapFileScale();

  /*! map file scale */
  static void setMapFileScale(float);

  private:
  
  /*! \brief
    default value for the magnetic field scale
    it is actually not used since it gets overridden by some 
    Fortran variable. Needs to be fixed.
  */
  static float mapFileScale;
  
  //! default value for the map file flag
  static int mapFileFlag;

};

#endif /*__MMFMMT_H__*/
