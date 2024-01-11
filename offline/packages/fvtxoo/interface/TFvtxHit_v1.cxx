
// $Id: TFvtxHit_v1.cxx,v 1.11 2011/12/01 04:16:20 slash Exp $

/*!
   \file TFvtxHit_v1.cxx
   \brief The forward vertex hit object 
   \author Hugo Pereira Da Costa
   \version $Revision: 1.11 $
   \date $Date: 2011/12/01 04:16:20 $
*/

#include <FVTXOO.h>
#include "TFvtxHit_v1.h"
ClassImp( TFvtxHit_v1 );

using namespace std;

//________________________________________________________
TFvtxHit_v1::TFvtxHit_v1( void ):
  _arm( 0 ),
  _cage( 0 ),
  _station( 0 ),
  _sector( 0 ),
  _column( 0 ),
  _strip( 0 ),
  _q( 0 ),
  _error_q( 0 ),
  _adc( 0 ),
  _amu( 0 ),
  _status( 0 )
{}
    
//_____________________________________________
TFvtxHit_v1::TFvtxHit_v1( 
  const Key& key,
  const unsigned short& arm,
  const unsigned short& cage,
  const unsigned short& station,
  const unsigned short& sector,
  const unsigned short& column,
  const unsigned short& strip ):
  TFvtxHit(key), 
  _arm( arm ),
  _cage( cage ),
  _station( station ),
  _sector( sector ),
  _column( column ),
  _strip( strip ),
  _q( 0 ),
  _error_q( 0 ),
  _adc( 0 ),
  _amu( 0 ),
  _status( 0 )
{}

//_______________________________________________________
TFvtxHit_v1::TFvtxHit_v1(const TFvtxHit* base_ptr) : 
  TFvtxHit(*base_ptr),
  _arm( base_ptr->get_arm() ),
  _cage( base_ptr->get_cage() ),
  _station( base_ptr->get_station() ),
  _sector( base_ptr->get_sector() ),
  _column( base_ptr->get_column() ),
  _strip( base_ptr->get_strip() ),
  _q( base_ptr->get_q() ),
  _error_q( base_ptr->get_error_q() ),
  _adc( base_ptr->get_adc() ),
  _amu( base_ptr->get_amu() ),
  _status( base_ptr->get_status() )
{}

//_______________________________________________________
TFvtxHit_v1::TFvtxHit_v1(const TFvtxHit& base_ref) : 
  TFvtxHit(base_ref),
  _arm( base_ref.get_arm() ),
  _cage( base_ref.get_cage() ),
  _station( base_ref.get_station() ),
  _sector( base_ref.get_sector() ),
  _column( base_ref.get_column() ),
  _strip( base_ref.get_strip() ),
  _q( base_ref.get_q() ),
  _error_q( base_ref.get_error_q() ),
  _adc( base_ref.get_adc() ),
  _amu( base_ref.get_amu() ),
  _status( base_ref.get_status() )
{}

//_______________________________________________________
void TFvtxHit_v1::print( ostream& os ) const
{
  FVTXOO::PRINT(os,GetName());
  os 
    << " arm: " << _arm
    << " cage: " << _cage
    << " station: " << _station
    << " sector: " << _sector
    << " column: " << _column
    << " strip: " << _strip << endl;
  os 
    << " q: " << _q 
    << " error_q: " << _error_q 
    << " adc: " << _adc 
    << " amu: " << _amu << endl;
  os 
    << " status: " << _status << endl;
  
  FVTXOO::PRINT(os,"**");
}
