// $Id: TFvtxKeyGen.h,v 1.7 2011/12/01 04:16:19 slash Exp $
#ifndef _TFvtxKeyGen_h_
#define _TFvtxKeyGen_h_

/*!
  \file TFvtxKeyGen.h
  \brief static class to generate keys for forward vertex objects
  \author H. Pereira Da Costa
  \version $Revision: 1.7 $
  \date $Date: 2011/12/01 04:16:19 $
*/

#include <PHKey.hh>

//! Static utility class for generating forward vertex objects unique keys
/*!	
  Static utility class for generating unique keys from specification of
  map_key, arm, station. The allowed range of input parameters is specified in
  the TFvtxKeygen::get_key description. The get_key method is overloaded to
  provide appropriate keys for each type of interface object.
*/
class TFvtxKeyGen
{

 public:

  /*! Import name of Key object from PHKey */
  typedef PHKey::object_key_type key_type; 
 
 	//! shortcut for first last key pair
  typedef std::pair<key_type,key_type> key_range;

  /*! Generate key for all objects in given column */
  static key_type get_key(
      const unsigned short& i_arm, 
      const unsigned short& i_cage,
      const unsigned short& i_station, 
      const unsigned short& i_sector,
      const unsigned short& i_column,
      const unsigned short& i_index);

  /*! Generate a pair of keys with specified range */
  static key_range get_key_range(
      const unsigned short& i_arm, 
      const unsigned short& i_cage,
      const unsigned short& i_station, 
      const unsigned short& i_sector,
      const unsigned short& i_column);

  /*! Generate key for all objects in given sector */
  static key_type get_key(
      const unsigned short& i_arm, 
      const unsigned short& i_cage,
      const unsigned short& i_station, 
      const unsigned short& i_sector,
      const unsigned short& i_index);

  /*! Generate a pair of keys with specified range */
  static key_range get_key_range(
      const unsigned short& i_arm, 
      const unsigned short& i_cage,
      const unsigned short& i_station, 
      const unsigned short& i_sector);

  /*! Generate key for all objects in given station*/
  static key_type get_key(
      const unsigned short& i_arm, 
      const unsigned short& i_cage,
      const unsigned short& i_station, 
      const unsigned short& i_index);

  /*! Generate a pair of keys with specified range */
  static key_range get_key_range(
      const unsigned short& i_arm, 
      const unsigned short& i_cage,
      const unsigned short& i_station); 

  /*! Generate key for all objects in given cage*/
  static key_type get_key(
      const unsigned short& i_arm,
      const unsigned short& i_cage,
      const unsigned short& i_index);

  /*! Generate a pair of keys with specified range */
  static key_range get_key_range(
      const unsigned short& i_arm,
      const unsigned short& i_cage);

  /*! Generate key for all objects in given arm */
  static key_type get_key(
      const unsigned short& i_arm, 
      const unsigned short& i_index);

  /*! Generate a pair of keys with specified range */
  static key_range get_key_range(const unsigned short& i_arm);

  /*! Generate key for all objects disreagarding the arm */
  static key_type get_key( const unsigned short& i_index);

  /*! Generate a pair of keys with specified range */
  static key_range get_key_range( void );

  /*! Returns the maximum value of index */
  static unsigned long get_max_index() { return INDEX_MASK; }
    
  private:
  
  // Note: we must define sufficients bits 
  // for arm_max+1.. station_max+1 etc
  static const unsigned long ARM_BITS;
  static const unsigned long CAGE_BITS;
  static const unsigned long STATION_BITS;
  static const unsigned long SECTOR_BITS;
  static const unsigned long COLUMN_BITS;
  static const unsigned long INDEX_BITS;	
  
  // mask to retrieve index from global bit map
  static const unsigned long ARM_MASK;
  static const unsigned long CAGE_MASK;
  static const unsigned long STATION_MASK;
  static const unsigned long SECTOR_MASK;
  static const unsigned long COLUMN_MASK;
  static const unsigned long INDEX_MASK; 

  // position of the different indexes in the global bit map
  static const unsigned long INDEX_SHIFT;
  static const unsigned long COLUMN_SHIFT;
  static const unsigned long SECTOR_SHIFT;
  static const unsigned long STATION_SHIFT;
  static const unsigned long CAGE_SHIFT;
  static const unsigned long ARM_SHIFT;
  
};

#endif // __TMutKeyGen_H__
