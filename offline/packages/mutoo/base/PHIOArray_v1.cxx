#include "PHIOArray_v1.hh"
ClassImp(PHIOArray_v1)

//____________________________________________________
PHIOArray_v1::~PHIOArray_v1()
{}

//____________________________________________________
void PHIOArray_v1::Reset( void )
{ _array_ptr->Delete(); }

//____________________________________________________
void PHIOArray_v1::print(std::ostream& os ) const 
{
  MUTOO::TRACE("PHIOArray_v1::print()");
  if(_array_ptr) {
    for(int i = 0; i<_array_ptr->GetEntries(); ++i)
    { _array_ptr->At(i)->Print(); }
  }
}
