// =================== // FILE: SvxEventInfov1.C
// ===================

#include "SvxEventInfov1.h"
#include "svxAddress.hh"

#include <iostream>

using namespace std;

// **************************************************************
// Implementation of Silicon EventInfo
// ---
// Created  by T. Hachiya  Jun-23-2011
// ***********************************************************************

ClassImp(SvxEventInfov1)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxEventInfov1::SvxEventInfov1(SvxEventInfo* info)
  : SvxEventInfo(info)
{
  Reset();
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxEventInfov1::Reset()
{
  //////////////////
  for( int imod=0; imod<SVXNMODULEPIXEL; imod++){
    m_pixelStatus[imod] = -9999;
  }

  for( int imod=0; imod<SVXNMODULESTRIP; imod++){
    m_stripStatus[imod] = -9999;
    for( int isens=0; isens<6; isens++){
      m_stripCellID[imod][isens] = -9999;
    }
  }

}

void SvxEventInfov1::set_pixelStatus (const int module, const int val)
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_pixelStatus[module] = val;
}

void SvxEventInfov1::set_stripCellID (const int module, const int chip, const int val)
{
  if(!svxAddress::isStripModuleID(module)||!(0<=chip&&chip<6)){
    cerr<<PHWHERE<<" out of range module:"<<module<<" chip:"<<chip<<endl;
    return;
  }
  m_stripCellID[module][chip] = val;
}
void SvxEventInfov1::set_stripStatus (const int module, const int val)
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_stripStatus[module] = val;
}

//get
int SvxEventInfov1::get_pixelStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelStatus[module];
}

int SvxEventInfov1::get_stripCellID(const int module, const int chip) const
{
  if(!svxAddress::isStripModuleID(module)||!(0<=chip&&chip<6)){
    cerr<<PHWHERE<<" out of range module:"<<module<<" chip:"<<chip<<endl;
    return -9999;
  }
  return m_stripCellID[module][chip];
}

int SvxEventInfov1::get_stripStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripStatus[module];
}

