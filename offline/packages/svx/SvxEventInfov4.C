// =================== // FILE: SvxEventInfov4.C
// ===================

#include <SvxEventInfov4.h>
#include <svxAddress.hh>
#include <Event.h>
#include <packet_gl1.h>

#include <iostream>

using namespace std;

// **************************************************************
// Implementation of Silicon EventInfo
// ---
// Created  by H. Hachiya  Oct-21-2013
// ***********************************************************************

ClassImp(SvxEventInfov4)

// Constructor(s) & destructor
// """""""""""""""""""""""""""
SvxEventInfov4::SvxEventInfov4(SvxEventInfo* info)
  : SvxEventInfo(info)
{
  Reset();
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxEventInfov4::Reset()
{
  m_event = 0;
  m_gl1clk_count = 0;

  //////////////////
  for( int imod=0; imod<SVXNMODULEPIXEL; imod++){
    m_pixelStatus[imod]    = -9999;
    m_pixelSubsysErr[imod] = -9999;
    m_pixelDcmErr[imod]    = -9999;
    m_pixelBeamClock[imod] = -9999;
    m_pixelEvtCount[imod]  = -9999;
  }
  for( int ild=0; ild<SVXLADDERSLAYER0*2+SVXLADDERSLAYER1*2; ild++){
    for( int isn=0; isn<SVXSENSORSLAYER0; isn++){
      m_pixelNRawhits[ild][isn]  = 0;
      m_pixelNClusters[ild][isn] = 0;
    }
  }

  for( int imod=0; imod<SVXNMODULESTRIP; imod++){
    m_stripStatus[imod]    = -9999;
    m_stripSubsysErr[imod] = -9999;
    m_stripDcmErr[imod]    = -9999;
    m_stripBeamClock[imod] = -9999;
    m_stripEvtCount[imod]  = -9999;
    m_stripNRawhits[imod]  = 0;
    m_stripNClusters[imod] = 0;
    for( int isens=0; isens<6; isens++){
      for( int ichip=0; ichip<12; ichip++){
        m_stripCellID[imod][isens][ichip] = 0; // unsigned char
      }
    }
  }

}

void SvxEventInfov4::set_GlobalInfo (Event *e)
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

void SvxEventInfov4::set_pixelStatus (const int module, const int val)
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_pixelStatus[module] = val;
}

void SvxEventInfov4::set_pixelStatus (const int module, Packet *p)
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

void SvxEventInfov4::set_stripCellID (const int module, const int sensor, const int chip, const unsigned char val)
{
  if(!svxAddress::isStripModuleID(module)||
     !(0<=sensor&&sensor<6)||
     !(0<=chip&&chip<12)
    )
  {
    cerr<<PHWHERE<<" out of range module:"<<module<<" sensor:"<<sensor<<" chip:"<<chip<<endl;
    return;
  }
  m_stripCellID[module][sensor][chip] = val;
}

void SvxEventInfov4::set_stripStatus (const int module, const int val)
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  m_stripStatus[module] = val;
}

void SvxEventInfov4::set_stripStatus (const int module, Packet *p)
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

void SvxEventInfov4::set_pixelNRawhits(const int ladder, const int sensor, const int nrawhits)
{
  if(ladder<0||ladder>=30||sensor<0||sensor>=4){
    cerr<<PHWHERE<<" out of range ladder/sensor:"<<ladder<<"/"<<sensor<<endl;
    return;
  }
  if(nrawhits<256){
    m_pixelNRawhits[ladder][sensor] = (unsigned char) nrawhits;
  } else {
    m_pixelNRawhits[ladder][sensor] = 255;
  }    
}

void SvxEventInfov4::set_pixelNClusters(const int ladder, const int sensor, const int nclusters)
{
  if(ladder<0||ladder>=30||sensor<0||sensor>=4){
    cerr<<PHWHERE<<" out of range ladder/sensor:"<<ladder<<"/"<<sensor<<endl;
    return;
  }
  if(nclusters<256){
    m_pixelNClusters[ladder][sensor] = (unsigned char) nclusters;
  } else {
    m_pixelNClusters[ladder][sensor] = 255;
  }    
}

void SvxEventInfov4::set_stripNRawhits(const int module, const int nrawhits)
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  if(nrawhits<256){
    m_stripNRawhits[module] = (unsigned char) nrawhits;
  } else {
    m_stripNRawhits[module] = 255;
  }
}

void SvxEventInfov4::set_stripNClusters(const int module, const int nclusters)
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return;
  }
  if(nclusters<256){
    m_stripNClusters[module] = (unsigned char) nclusters;
  } else {
    m_stripNClusters[module] = 255;
  }
}


//get
int SvxEventInfov4::get_pixelStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelStatus[module];
}

int SvxEventInfov4::get_pixelSubsysStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelSubsysErr[module];
}

int SvxEventInfov4::get_pixelDcmStatus(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelDcmErr[module];
}

int SvxEventInfov4::get_pixelBeamClock(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelBeamClock[module];
}

int SvxEventInfov4::get_pixelEventCount(const int module) const
{
  if(!svxAddress::isPixelModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_pixelEvtCount[module];
}

unsigned char SvxEventInfov4::get_stripCellID(const int module, const int sensor, const int chip) const
{
  if(!svxAddress::isStripModuleID(module) ||
     !(0<=sensor&&sensor<6) ||
     !(0<=chip&&chip<12)
    )
  {
    cerr<<PHWHERE<<" out of range module:"<<module<<" sensor:"<<sensor<<" chip:"<<chip<<endl;
    return 0;
  }
  return m_stripCellID[module][sensor][chip];
}

int SvxEventInfov4::get_stripStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripStatus[module];
}

int SvxEventInfov4::get_stripSubsysStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripSubsysErr[module];
}

int SvxEventInfov4::get_stripDcmStatus(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripDcmErr[module];
}

int SvxEventInfov4::get_stripBeamClock(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripBeamClock[module];
}

int SvxEventInfov4::get_stripEventCount(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return -9999;
  }
  return m_stripEvtCount[module];
}

unsigned char SvxEventInfov4::get_pixelNRawhits(const int ladder, const int sensor) const
{
  if(ladder<0||ladder>=30||sensor<0||sensor>=4){
    cerr<<PHWHERE<<" out of range ladder/sensor:"<<ladder<<"/"<<sensor<<endl;
    return 0;
  }
  return m_pixelNRawhits[ladder][sensor];
}

unsigned char SvxEventInfov4::get_pixelNClusters(const int ladder, const int sensor) const
{
  if(ladder<0||ladder>=30||sensor<0||sensor>=4){
    cerr<<PHWHERE<<" out of range ladder/sensor:"<<ladder<<"/"<<sensor<<endl;
    return 0;
  }
  return m_pixelNClusters[ladder][sensor];
}

unsigned char SvxEventInfov4::get_stripNRawhits(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return 0;
  }
  return m_stripNRawhits[module];
}

unsigned char SvxEventInfov4::get_stripNClusters(const int module) const
{
  if(!svxAddress::isStripModuleID(module)){
    cerr<<PHWHERE<<" out of range module:"<<module<<endl;
    return 0;
  }
  return m_stripNClusters[module];
}
