// =================== // FILE: SvxEventInfov2.C
// ===================

#include <SvxEventInfov2.h>
#include <svxAddress.hh>
#include <Event.h>
#include <packet_gl1.h>

#include <iostream>

using namespace std;

// **************************************************************
// Implementation of Silicon EventInfo
// ---
// Created  by R. Akimoto  Mar-19-2012
// ***********************************************************************

ClassImp(SvxEventInfov2)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxEventInfov2::SvxEventInfov2(SvxEventInfo* info)
  : SvxEventInfo(info)
{
  Reset();
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxEventInfov2::Reset()
{
  m_event = 0;
  m_gl1clk_count = 0;

  //////////////////
  for( int imod=0; imod<SVXNMODULEPIXEL; imod++){
    m_pixelStatus[imod]      = -9999;
    m_pixelSubsysErr[imod] = -9999;
    m_pixelDcmErr[imod]    = -9999;
    m_pixelBeamClock[imod] = -9999;
    m_pixelEvtCount[imod]  = -9999;
  }

  for( int imod=0; imod<SVXNMODULESTRIP; imod++){
    m_stripStatus[imod]      = -9999;
    m_stripSubsysErr[imod] = -9999;
    m_stripDcmErr[imod]    = -9999;
    m_stripBeamClock[imod] = -9999;
    m_stripEvtCount[imod]  = -9999;
    for( int isens=0; isens<6; isens++){
      m_stripCellID[imod][isens] = -9999;
    }
  }

}

void SvxEventInfov2::set_GlobalInfo (Event *e)
{
  m_event = e->getEvtSequence();
  /// m_event should be compared to iValue(0,"EVTNR")

  Packet *p14001 = e->getPacket(14001);
  if(p14001){
    m_gl1clk_count = p14001->iValue(0, BEAMCTR0) & 0xFFFF;
    /// m_gl1clk_count should be compared to iValue(0,"BCLCK")
    /// only lower 2 bytes may be checked.
    delete p14001;
  }
}

void SvxEventInfov2::set_pixelStatus (const int module, const int val)
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_pixelStatus[module] = val;
}

void SvxEventInfov2::set_pixelStatus (const int module, Packet *p)
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_pixelSubsysErr[module] = p->iValue(0, "STATUS");
  m_pixelDcmErr[module]    = p->iValue(0, "PARITYOK");
  m_pixelBeamClock[module] = p->iValue(0, "BCLCK");
  m_pixelEvtCount[module]  = p->iValue(0, "EVTNR");
}

void SvxEventInfov2::set_stripCellID (const int module, const int chip, const int val)
{
  if(!svxAddress::isStripModuleID(module)||!(0<=chip&&chip<6)){
    cerr<<PHWHERE<<" out of range module:"<<module<<" chip:"<<chip<<endl;
    return;
  }
  m_stripCellID[module][chip] = val;
}

void SvxEventInfov2::set_stripStatus (const int module, const int val)
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_stripStatus[module] = val;
}

void SvxEventInfov2::set_stripStatus (const int module, Packet *p)
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  /// subsys error
  int rawdata[5000];
  int nw = 0;
  p->fillIntArray(rawdata, 5000, &nw, "DATA");
  m_stripSubsysErr[module] = rawdata[nw-5] & (1<<7);

  /// dcm error
  int fem_parity = p->iValue(0, "PARITY");
  int dcm_parity = p->iValue(0, "DCMPARITY");
  m_stripDcmErr[module] = fem_parity - dcm_parity;

  m_stripBeamClock[module] = p->iValue(0, "BCLCK");
  m_stripEvtCount[module]  = p->iValue(0, "EVTNR");  
}

//get
int SvxEventInfov2::get_pixelStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelStatus[module];
}

int SvxEventInfov2::get_pixelSubsysStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelSubsysErr[module];
}

int SvxEventInfov2::get_pixelDcmStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelDcmErr[module];
}

int SvxEventInfov2::get_pixelBeamClock(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelBeamClock[module];
}

int SvxEventInfov2::get_pixelEventCount(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelEvtCount[module];
}

int SvxEventInfov2::get_stripCellID(const int module, const int chip) const
{
  if(!svxAddress::isStripModuleID(module)||!(0<=chip&&chip<6)){
    cerr<<PHWHERE<<" out of range module:"<<module<<" chip:"<<chip<<endl;
    return -9999;
  }
  return m_stripCellID[module][chip];
}

int SvxEventInfov2::get_stripStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripStatus[module];
}

int SvxEventInfov2::get_stripSubsysStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripSubsysErr[module];
}

int SvxEventInfov2::get_stripDcmStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripDcmErr[module];
}

int SvxEventInfov2::get_stripBeamClock(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripBeamClock[module];
}

int SvxEventInfov2::get_stripEventCount(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripEvtCount[module];
}
