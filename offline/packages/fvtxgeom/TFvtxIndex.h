// $Id: TFvtxIndex.h,v 1.7 2013/11/05 00:04:47 jinhuang Exp $

#ifndef __TFvtxIndex_H__
#define __TFvtxIndex_H__

/*!
  \file TFvtxIndex.h
  \brief returns unique integer associated to plane location
  \author H. Pereira Da Costa
  \version $Revision: 1.7 $
  \date $Date: 2013/11/05 00:04:47 $
*/

#include <FVTXGEOM.h>
#include <iostream>

/*!
  \class TFvtxIndex
  \brief returns unique integer associated to mutr cathode location
*/

class TFvtxIndex {

  public:

  //! constructor
  TFvtxIndex(
    unsigned long arm = 0,
    unsigned long cage = 0,
    unsigned long station = 0,
    unsigned long sector = 0,
    unsigned long column = 0 )
  {
    update( arm, cage, station, sector, column );
    if( !check() ) std::cout << "TFvtxIndex::TFvtxIndex - invalid index " << *this << std::endl;
  }

  //! check indexes
  bool check( void )
  {
    return
      (
        ( arm() < FVTXGEOM::NumberOfArms ) &&
        ( cage() < FVTXGEOM::NumberOfCages ) &&
        ( station() < FVTXGEOM::NumberOfStations ) &&
	( sector() < FVTXGEOM::NumberOfSectors )  &&
        ( column() < FVTXGEOM::NumberOfColumns )
      );
  }

  //! equal to operator
  bool operator == ( const TFvtxIndex& index ) const
  { return _index == index._index; }

    //! equal to operator
  bool operator < ( const TFvtxIndex& index ) const
  { return _index < index._index; }

  //! print
  friend std::ostream& operator << (std::ostream &out,const TFvtxIndex &index)
  {
    out << "("
      << index.arm() << ","
      << index.cage() << ","
      << index.station() << ","
      << index.sector() << ","
      << index.column() << ") ("
      << index._index << ")";
    return out;
  }

  //! arm
  unsigned long arm( void ) const
  { return ( _index >> ARM_SHIFT ) & ARM_MASK; }

  //! cage
  unsigned long cage( void ) const
  { return ( _index >> CAGE_SHIFT ) & CAGE_MASK; }

  //! station
  unsigned long station( void ) const
  { return ( _index >> STATION_SHIFT ) & STATION_MASK; }

  //! sector
  unsigned long sector( void ) const
  { return ( _index >> SECTOR_SHIFT ) & SECTOR_MASK; }

  //! column
  unsigned long column( void ) const
  { return ( _index >> COLUMN_SHIFT ) & COLUMN_MASK; }

  //! arm
  TFvtxIndex& set_arm( unsigned long arm )
  { return update( arm, cage(), station(), sector(), column() ); }

  //! cage
  TFvtxIndex& set_cage( unsigned long cage )
  { return update( arm(), cage, station(), sector(), column() ); }

  //! station
  TFvtxIndex& set_station( unsigned long station )
  { return update( arm(), cage(), station, sector(), column() ); }

  //! sector
  TFvtxIndex& set_sector( unsigned long sector )
  { return update( arm(), cage(), station(), sector, column() ); }

  //! column
  TFvtxIndex& set_column( unsigned long column )
  { return update( arm(), cage(), station(), sector(), column ); }

  //! get the index word
  unsigned long get() const {return _index;}

  //! get the index word as int
  operator unsigned long() const
    {
       return _index;
    }

  private:

  //! build index from arm, cage, station, etc. location
  TFvtxIndex& update(
      unsigned long arm,
      unsigned long cage,
      unsigned long station,
      unsigned long sector,
      unsigned long column )
  {
    _index =
      (arm << ARM_SHIFT)|
      (cage << CAGE_SHIFT)|
      (station << STATION_SHIFT)|
      (sector << SECTOR_SHIFT)|
      (column << COLUMN_SHIFT);
    return *this;
  }

  //! unique id
  unsigned long _index;

  static const unsigned long ARM_BITS;
  static const unsigned long CAGE_BITS;
  static const unsigned long STATION_BITS;
  static const unsigned long SECTOR_BITS;
  static const unsigned long COLUMN_BITS;

  // mask to retrieve index from global bit map
  static const unsigned long ARM_MASK;
  static const unsigned long CAGE_MASK;
  static const unsigned long STATION_MASK;
  static const unsigned long SECTOR_MASK;
  static const unsigned long COLUMN_MASK;

  // position of the different indexes in the global bit map
  static const unsigned long COLUMN_SHIFT;
  static const unsigned long SECTOR_SHIFT;
  static const unsigned long STATION_SHIFT;
  static const unsigned long CAGE_SHIFT;
  static const unsigned long ARM_SHIFT;

};

#endif
