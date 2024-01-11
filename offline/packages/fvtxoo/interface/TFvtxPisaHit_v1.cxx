// $Id: TFvtxPisaHit_v1.cxx,v 1.3 2011/12/01 04:16:20 slash Exp $

/*!
  \file TFvtxPisaHit_v1.cxx
  \brief The forward vertex hit object 
  \author Hugo Pereira Da Costa
  \version $Revision: 1.3 $
  \date $Date: 2011/12/01 04:16:20 $
*/

#include <FVTXOO.h>
#include "TFvtxPisaHit_v1.h"
ClassImp( TFvtxPisaHit_v1 );
using namespace std; 

//____________________________________________________
TFvtxPisaHit_v1::TFvtxPisaHit_v1( void ):
  _index(0)
{}

//_____________________________________________
TFvtxPisaHit_v1::TFvtxPisaHit_v1( const Key& key, const unsigned short& index ):
  TFvtxPisaHit( key ),
  _index( index )
{}

//_____________________________________________ 
TFvtxPisaHit_v1::TFvtxPisaHit_v1(const TFvtxPisaHit& base_ref ):
  TFvtxPisaHit(base_ref)
{ 
  set_index( base_ref.get_index() );
  set_pisa_hit( base_ref.get_pisa_hit() ); 
}

//_____________________________________________ 
TFvtxPisaHit_v1::TFvtxPisaHit_v1(const TFvtxPisaHit* base_ptr ):
  TFvtxPisaHit(*base_ptr)
{ 
  set_index( base_ptr->get_index() );
  set_pisa_hit( base_ptr->get_pisa_hit() ); 
}

//_______________________________________________________
void TFvtxPisaHit_v1::print( ostream& os ) const
{
  FVTXOO::PRINT(os,GetName());
  
  os << "index: " << get_index() << endl;
  os << " position=(" << get_pisa_hit()->GetXGlobal() << "," << get_pisa_hit()->GetYGlobal() << "," << get_pisa_hit()->GetYGlobal() << ")" << endl;
  os << " layer=" << get_pisa_hit()->GetLayer() << endl;
  os << " volume= [ ";
  for( int i=0; i<9; i++ ) os << get_pisa_hit()->GetHitVolume(i) << " ";
  os << "]" << endl;
    
  FVTXOO::PRINT(os,"**");
}
