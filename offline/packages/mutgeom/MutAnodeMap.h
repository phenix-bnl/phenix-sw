#ifndef MutAnodeMap_h
#define MutAnodeMap_h

// $Id: MutAnodeMap.h,v 1.8 2009/08/27 07:24:20 shoji Exp $

/*!
  \file MutAnodeMap.h
  \brief map anode HV cards to wire ids
  \author Hugo Pereira
  \version $Revision: 1.8 $
  \date $Date: 2009/08/27 07:24:20 $
*/

#include <MUTGEOM.h>

#include<TDataType.h>

#include <boost/tuple/tuple.hpp>

#include <set>
#include <sstream>
#include <stdexcept>

//! map anode HV cars to wire
class MutAnodeMap
{

  public:

  class CardId: public boost::tuple<UShort_t,UShort_t,UShort_t,UShort_t,UShort_t>
  {
    public:
    
    //! default constructor
    CardId( UShort_t arm = 0, UShort_t station = 0, UShort_t octant = 0, UShort_t gap = 0, UShort_t card = 0 ):
    boost::tuple<UShort_t,UShort_t,UShort_t,UShort_t,UShort_t>( arm, station, octant, gap, card )
    {}
    
    //! less than operator
    bool operator < (const CardId& id ) const
    { 
      if( arm() != id.arm() ) return arm() < id.arm();
      if(station() != id.station() ) return  station() < id.station();
      if(octant() != id.octant() ) return  octant() < id.octant();
      if(gap() != id.gap() ) return  gap() < id.gap();
      if(card() != id.card() ) return  card() < id.card();
      return false;
    }
    
    //! arm
    const UShort_t& arm( void ) const
    { return get<0>(); }
    
    //! station
    const UShort_t& station( void ) const
    { return get<1>(); }
    
    //! octant
    const UShort_t& octant( void ) const
    { return get<2>(); }

    //! gap
    const UShort_t& gap( void ) const
    { return get<3>(); }

    //! card
    const UShort_t& card( void ) const
    { return get<4>(); }    
    
  };
  
  typedef std::set<CardId> CardSet;
  
  //! find cards that match a given module geometrical name
  static CardSet find_cards( const std::string& name, bool is_run2 )
  {
    std::set<std::string> names;
    names.insert( name );
    return find_cards( names, is_run2 );
  }
  
  //! find cards that match a set of module names
  static CardSet find_cards( const std::set<std::string>&, bool );
  
  //! find hard-coded disconnected cards (for Run2)
  static CardSet find_disconnected_cards( void );
  
  //! start wire id for each station/card number
  static int get_start_wire( unsigned int arm, unsigned int station, unsigned int octant, unsigned int card );

  //! end wire id (+1) for each station/card number
  static int get_end_wire( unsigned int arm, unsigned int station, unsigned int octant, unsigned int card );

  //! number of cards per arm
  static unsigned int get_num_cards( unsigned int arm, unsigned int station);

  //! map anode name to card ID for run2
  static const char *mutr_anode2HV_run2[MUTGEOM::NumberOfArms][MUTGEOM::NumberOfStations][MUTGEOM::NumberOfOctants][MUTGEOM::NumberOfGaps][10];
  
  //! map anode name to card ID
  static const char *mutr_anode2HV[MUTGEOM::NumberOfArms][MUTGEOM::NumberOfStations][MUTGEOM::NumberOfOctants][MUTGEOM::NumberOfGaps][10];
  
  static const std::string get_anode_card( unsigned int arm,
					   unsigned int station,
					   unsigned int octant,
					   unsigned int gap,
					   unsigned int wire );
  
  private:

  static const unsigned int _numCards[MUTGEOM::NumberOfArms][MUTGEOM::NumberOfStations];

};

#endif
