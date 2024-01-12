
#include "SvxQAInfov1.h"
#include "svxAddress.hh"

#include <iostream>

using namespace std;

ClassImp(SvxQAInfov1)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxQAInfov1::SvxQAInfov1(SvxQAInfo* info) : SvxQAInfo(info)
{
  Reset();
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxQAInfov1::Reset()
{
  //////////////////
  for( int imod=0; imod<SVXNMODULESTRIP; imod++){
    m_CellStuckEvent[imod] = -9999;
  }

}

void SvxQAInfov1::set_CellStuckEvent(const int module, const int val)
{

  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_CellStuckEvent[module] = val;

}

int SvxQAInfov1::get_CellStuckEvent(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_CellStuckEvent[module];
}
