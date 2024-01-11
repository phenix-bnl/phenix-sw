#include<TFvtxCompactCoordMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>

/*! Default contructor */
TFvtxCompactCoordMap::TFvtxCompactCoordMap() : 
  _count(0) {;}

/*! Construct with map key */
TFvtxCompactCoordMap::TFvtxCompactCoordMap(PHKey::map_key_type map_key) : 
  PHMap<PHKey::key_type, TFvtxCompactCoord, TFvtxCompactCoord_v1>(map_key), 
  _count(0){;}


/*! Create a new TFvtxCompactCoord object.  Returns an iterator to 
  the newly created object */

TFvtxCompactCoordMap::iterator TFvtxCompactCoordMap::insert_new(unsigned short arm, 
                                                  unsigned short cage,
                                                  unsigned short station, 
                                                  unsigned short sector, 
                                                  unsigned short column)
  
  
{

  unsigned short index = get_roll_count();
  
  // get the key for the new coord
  //
  TFvtxKeyGen::key_type key = TFvtxKeyGen::get_key(arm, 
                                                   cage,
                                                   station, 
                                                   sector, 
                                                   column,
                                                   index);						 
  // full key
  //
  Key full_key(get_map_key(),key);
  
  // insert coordinate
  //
  insert(full_key, new TFvtxCompactCoord_v1(full_key,
                                     arm,
                                     cage,
                                     station,
                                     sector,
                                     column,
                                     index));
  
  // okay not so efficient 
  //
  return find(full_key);
}

/*! 
  Create a new TFvtxCompactCoord object at specified location. The location
  is specified by a boost tuple. Returns an iterator to the newly created 
  object 
*/

/*! Get an iterator to all TFvtxCompactCoord objects in given column*/
TFvtxCompactCoordMap::iterator TFvtxCompactCoordMap::get(unsigned short arm, 
                                           unsigned short cage,
                                           unsigned short station, 
                                           unsigned short sector, 
                                           unsigned short column)
  
{
  // key range associated with this plane
  //
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm, 
                                                            cage,
                                                            station, 
                                                            sector, 
                                                            column);
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}

/*! 
  Get an iterator to all TFvtxClus objects in given plane.
  The plane is specified by the input boost tuple
*/

/*! Get a const_iterator to all hits in given column */
TFvtxCompactCoordMap::const_iterator TFvtxCompactCoordMap::get(unsigned short arm, 
                                                 unsigned short cage,
                                                 unsigned short station, 
                                                 unsigned short sector, 
                                                 unsigned short column) const  
{
  // key range associated with this column
  //
  TFvtxKeyGen::key_range range = TFvtxKeyGen::get_key_range(arm,        
                                                            cage,
                                                            station,    
                                                            sector,     
                                                            column);        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),range.first);
  Key upper(get_map_key(),range.second);
  return find(lower,upper);
}


/*! 
  Get an iterator to all TFvtxClus objects in given plane.
  The plane is specified by the input boost tuple
*/
TFvtxCompactCoordMap::const_iterator TFvtxCompactCoordMap::get(unsigned short arm, 
                                                 unsigned short cage,
                                                 unsigned short station, 
                                                 unsigned short sector) const  
  
{
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station,    
                                                         sector,     
                                                         0);        
  
  
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station,    
                                                         sector+1,     
                                                         0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TFvtxCompactCoordMap::iterator TFvtxCompactCoordMap::get(unsigned short arm, 
                                           unsigned short cage,
                                           unsigned short station, 
                                           unsigned short sector)   
					       
{
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station,    
                                                         sector,     
                                                         0);        
  
  
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station,
                                                         sector+1,     
                                                         0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}
/*! 
  Get an iterator to all TFvtxClus objects in given station.
  The station is specified by the input boost tuple
*/
TFvtxCompactCoordMap::const_iterator TFvtxCompactCoordMap::get(unsigned short arm, 
                                                 unsigned short cage,
                                                 unsigned short station ) const  
{
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station,    
                                                         0,
                                                         0);        
  
  
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station+1,    
                                                         0,     
                                                         0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TFvtxCompactCoordMap::iterator TFvtxCompactCoordMap::get(unsigned short arm, 
                                           unsigned short cage,
                                           unsigned short station) 					       
{
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station,    
                                                         0,     
                                                         0);        
  
  
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm,        
                                                         cage,
                                                         station+1,    
                                                         0,     
                                                         0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TFvtxCompactCoordMap::const_iterator TFvtxCompactCoordMap::get(unsigned short arm) const  
{
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,        
                                                         0,
                                                         0,    
                                                         0,
                                                         0);        
  
  
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,        
                                                         0,
                                                         0,    
                                                         0,     
                                                         0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}

TFvtxCompactCoordMap::iterator TFvtxCompactCoordMap::get(unsigned short arm)
{
  TFvtxKeyGen::key_type lower_key = TFvtxKeyGen::get_key(arm,        
                                                         0,
                                                         0,    
                                                         0,     
                                                         0);        
  
  
  TFvtxKeyGen::key_type upper_key = TFvtxKeyGen::get_key(arm+1,        
                                                         0,
                                                         0,    
                                                         0,     
                                                         0)-1;        
  
  // return the iterator with specified range
  //
  Key lower(get_map_key(),lower_key);
  Key upper(get_map_key(),upper_key);
  return find(lower,upper);
}
