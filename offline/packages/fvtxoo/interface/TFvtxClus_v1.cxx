
// $Id: TFvtxClus_v1.cxx,v 1.6 2011/12/01 04:16:20 slash Exp $

/*!
   \file TFvtxClus_v1.cxx
   \brief The forward vertex hit object 
   \author Hugo Pereira Da Costa
   \version $Revision: 1.6 $
   \date $Date: 2011/12/01 04:16:20 $
*/

#include <FVTXOO.h>
#include "TFvtxClus_v1.h"
ClassImp( TFvtxClus_v1 );

using namespace std;

//________________________________________________________
TFvtxClus_v1::TFvtxClus_v1( void ):
  _arm( 0 ),
  _cage( 0 ),
  _station( 0 ),
  _sector( 0 ),
  _column( 0 ),
  _index( 0 ),
  _status( 0 )
{}
    
//_____________________________________________
TFvtxClus_v1::TFvtxClus_v1( 
    const Key& key,
    const unsigned short& arm,
    const unsigned short& cage,
    const unsigned short& station,
    const unsigned short& sector,
    const unsigned short& column,
    const unsigned short& index ):
  TFvtxClus(key), 
  _arm( arm ),
  _cage( cage ),
  _station( station ),
  _sector( sector ),
  _column( column ),
  _index( index ),
  _status( 0 )
{}

//_______________________________________________________
TFvtxClus_v1::TFvtxClus_v1(const TFvtxClus* base_ptr) : 
  TFvtxClus(*base_ptr),
  _arm( base_ptr->get_arm() ),
  _cage( base_ptr->get_cage() ),
  _station( base_ptr->get_station() ),
  _sector( base_ptr->get_sector() ),
  _column( base_ptr->get_column() ),
  _index( base_ptr->get_index() ),
  _status( base_ptr->get_status() )
{}

//_______________________________________________________
TFvtxClus_v1::TFvtxClus_v1(const TFvtxClus& base_ref) : 
  TFvtxClus(base_ref),
  _arm( base_ref.get_arm() ),
  _cage( base_ref.get_cage() ),
  _station( base_ref.get_station() ),
  _sector( base_ref.get_sector() ),
  _column( base_ref.get_column() ),
  _index( base_ref.get_index() ),
  _status( base_ref.get_status() )
{}

//_______________________________________________________
void TFvtxClus_v1::print( ostream& os ) const
{
  FVTXOO::PRINT(os,GetName());
  os 
    << " arm: " << _arm
    << " cage: " << _cage
    << " station: " << _station
    << " sector: " << _sector
    << " column: " << _column
    << " index: " << _index << endl;
  os 
    << " status: " << _status << endl;
  
  FVTXOO::PRINT(os,"**");
}
