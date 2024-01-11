// $Id: TFvtxKeyGen.cxx,v 1.8 2011/12/01 04:16:19 slash Exp $

/*!
  \file TFvtxKeyGen.cxx
  \brief static class to generate keys for forward vertex objects
  \author H. Pereira Da Costa
  \version $Revision: 1.8 $
  \date $Date: 2011/12/01 04:16:19 $
*/


#include<TFvtxKeyGen.h>
  
  // Note: we must define sufficients bits 
  // for arm_max+1.. station_max+1 etc
const unsigned long TFvtxKeyGen::ARM_BITS=2;
const unsigned long TFvtxKeyGen::CAGE_BITS=2;
const unsigned long TFvtxKeyGen::STATION_BITS=3;
const unsigned long TFvtxKeyGen::SECTOR_BITS=5;
const unsigned long TFvtxKeyGen::COLUMN_BITS = 2;
const unsigned long TFvtxKeyGen::INDEX_BITS=16;	
  
  // mask to retrieve index from global bit map
const unsigned long TFvtxKeyGen::ARM_MASK     = 0x00000003;
const unsigned long TFvtxKeyGen::CAGE_MASK    = 0x00000003;
const unsigned long TFvtxKeyGen::STATION_MASK = 0x00000007;
const unsigned long TFvtxKeyGen::SECTOR_MASK  = 0x0000001F;
const unsigned long TFvtxKeyGen::COLUMN_MASK  = 0x00000003;
const unsigned long TFvtxKeyGen::INDEX_MASK   = 0x0000FFFF; 

  // position of the different indexes in the global bit map
const unsigned long TFvtxKeyGen::INDEX_SHIFT   = 0;
const unsigned long TFvtxKeyGen::COLUMN_SHIFT  = TFvtxKeyGen::INDEX_SHIFT + TFvtxKeyGen::INDEX_BITS;
const unsigned long TFvtxKeyGen::SECTOR_SHIFT  = TFvtxKeyGen::COLUMN_SHIFT + TFvtxKeyGen::COLUMN_BITS;
const unsigned long TFvtxKeyGen::STATION_SHIFT = TFvtxKeyGen::SECTOR_SHIFT + TFvtxKeyGen::SECTOR_BITS;
const unsigned long TFvtxKeyGen::CAGE_SHIFT    = TFvtxKeyGen::STATION_SHIFT + TFvtxKeyGen::STATION_BITS;
const unsigned long TFvtxKeyGen::ARM_SHIFT     = TFvtxKeyGen::CAGE_SHIFT + TFvtxKeyGen::CAGE_BITS;

//________________________________________________
PHKey::object_key_type TFvtxKeyGen::get_key(
  const unsigned short& arm, 
  const unsigned short& cage,
  const unsigned short& station, 
  const unsigned short& sector, 
  const unsigned short& column,
  const unsigned short& index)
{
  // shift and mask					 
  //
  return	
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((CAGE_MASK & cage) << CAGE_SHIFT) |
    ((STATION_MASK & station) << STATION_SHIFT) |
    ((SECTOR_MASK & sector) << SECTOR_SHIFT) |
    ((COLUMN_MASK & column) << COLUMN_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//________________________________________________
TFvtxKeyGen::key_range TFvtxKeyGen::get_key_range(
  const unsigned short& arm, 
  const unsigned short& cage,
  const unsigned short& station, 
  const unsigned short& sector, 
  const unsigned short& column)
{
            
  return std::make_pair(
    get_key(arm,
      cage,
      station,
      sector,
      column,
      0),
    get_key(arm,
      cage,
      station,
      sector,
      column+1,
      0)-1);
}


//________________________________________________
PHKey::object_key_type TFvtxKeyGen::get_key(
  const unsigned short& arm, 
  const unsigned short& cage,
  const unsigned short& station, 
  const unsigned short& sector, 
  const unsigned short& index)
{
  // shift and mask					 
  //
  return	
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((CAGE_MASK & cage) << CAGE_SHIFT) |
    ((STATION_MASK & station) << STATION_SHIFT) |
    ((SECTOR_MASK & sector) << SECTOR_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//________________________________________________
TFvtxKeyGen::key_range TFvtxKeyGen::get_key_range(
  const unsigned short& arm, 
  const unsigned short& cage,
  const unsigned short& station, 
  const unsigned short& sector) 
{
            
  return std::make_pair(
    get_key(arm,
      cage,
      station,
      sector,
      0),
    get_key(arm,
      cage,
      station,
      sector+1,
      0)-1);
}

//________________________________________________
PHKey::object_key_type TFvtxKeyGen::get_key(
  const unsigned short& arm, 
  const unsigned short& cage,
  const unsigned short& station, 
  const unsigned short& index)
{
  // shift and mask					 
  //
  return	
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((CAGE_MASK & cage) << CAGE_SHIFT) |
    ((STATION_MASK & station) << STATION_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//________________________________________________
TFvtxKeyGen::key_range TFvtxKeyGen::get_key_range(
  const unsigned short& arm, 
  const unsigned short& cage,
  const unsigned short& station)
{
            
  return std::make_pair(
    get_key(arm,
      cage,
      station,
      0),
    get_key(arm,
      cage,
      station+1,
      0)-1);
}

//________________________________________________
PHKey::object_key_type TFvtxKeyGen::get_key(
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& index)
{
  // shift and mask
  //
  return
    ((ARM_MASK & arm) << ARM_SHIFT) |
    ((CAGE_MASK & cage) << CAGE_SHIFT) |
    ((INDEX_MASK & index) << INDEX_SHIFT);
}

//________________________________________________
TFvtxKeyGen::key_range TFvtxKeyGen::get_key_range(
  const unsigned short& arm,
  const unsigned short& cage)
{

  return std::make_pair(
    get_key(arm,
      cage,
      0),
    get_key(arm,
      cage+1,
      0)-1);
}

//________________________________________________
PHKey::object_key_type TFvtxKeyGen::get_key(
  const unsigned short& arm, 
  const unsigned short& index)
{

  // shift and mask					 
  return ((ARM_MASK & arm) << ARM_SHIFT) | ((INDEX_MASK & index) << INDEX_SHIFT);

}

//________________________________________________
TFvtxKeyGen::key_range TFvtxKeyGen::get_key_range( const unsigned short& arm)
{ return std::make_pair( get_key(arm, 0),  get_key(arm+1, 0)-1); }

//________________________________________________
PHKey::object_key_type TFvtxKeyGen::get_key( const unsigned short& index)
{

  // shift and mask					 
  return (INDEX_MASK & index) << INDEX_SHIFT;

}

//________________________________________________
TFvtxKeyGen::key_range TFvtxKeyGen::get_key_range( void )
{ return std::make_pair( get_key(0),  get_key(INDEX_MASK) ); }
