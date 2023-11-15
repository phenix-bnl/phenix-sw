//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2002
//
//  Implementation of class PdbMuiHVMap
//
//  Author: A.M. Glenn
//-----------------------------------------------------------------------------

#include <iostream>
#include "PdbMuiHVMap.hh"

PdbMuiHVMap::PdbMuiHVMap()
  : fMainFrame(-1), fSlot(-1), fSupplyChannel(-1),
    fArm(-1), fOrientation(-1),
    fGap(-1), fPanel(-1), fChain(-1),
    fFirstTwoPack(-1),    fLastTwoPack(-1)
{
  //  std::cout << "PdbMuiHVMap(ctor) default ctor called" << std::endl;
}

PdbMuiHVMap:: PdbMuiHVMap(short mainFrame,
			  short slot, short supply_channel,
			  short arm, short gap, short panel, short orient,
			  short chain, short first_twopack, short last_twopack)

  :  fMainFrame(mainFrame), fSlot(slot), fSupplyChannel(supply_channel),
     fArm(arm), fOrientation(orient),
     fGap(gap), fPanel(panel), fChain(chain),
     fFirstTwoPack(first_twopack),    fLastTwoPack(last_twopack)
{
  //  std::cout << "PdbMuiHVMap(ctor) full ctor called" << std::endl;
}

PdbMuiHVMap::PdbMuiHVMap(const PdbMuiHVMap& rhs)
  :  fMainFrame(rhs.fMainFrame),
     fSlot(rhs.fSlot), fSupplyChannel(rhs.fSupplyChannel),
     fArm(rhs.fArm), fOrientation(rhs.fOrientation),
     fGap(rhs.fGap), fPanel(rhs.fPanel), fChain(rhs.fChain),
     fFirstTwoPack(rhs.fFirstTwoPack),  fLastTwoPack(rhs.fLastTwoPack)
  
{
  //  std::cout << "PdbMuiHVMap(ctor) copy ctor called" << std::endl;
}

PdbMuiHVMap&
PdbMuiHVMap::operator=(const PdbMuiHVMap& rhs)
{
  if (this == &rhs) return *this;  // check for self-assignment
  fMainFrame = rhs.fMainFrame;
  fSlot = rhs.fSlot;
  fSupplyChannel = rhs.fSupplyChannel;
  fArm = rhs.fArm;
  fOrientation = rhs.fOrientation;
  fGap = rhs.fGap;
  fPanel = rhs.fPanel;
  fChain = rhs.fChain;
  fFirstTwoPack = rhs.fFirstTwoPack;
  fLastTwoPack = rhs.fLastTwoPack; 

  return *this;
}

void
PdbMuiHVMap::print() const
{
  std::cout << " A" << fArm << " G" << fGap << " P" << fPanel
       << " O" << fOrientation
       << " Chain" << fChain
       << " T" << fFirstTwoPack  << "-" << fLastTwoPack
       << " M" << fMainFrame 
       << " S" << fSlot 
       << " C" << fSupplyChannel
       << std::endl;
}

void 
PdbMuiHVMap::SetSoftwareIndex(short arm, short gap, short panel,
				   short orient, short chain)
{
  fArm           = arm;
  fOrientation   = orient;
  fGap           = gap;
  fPanel         = panel;
  fChain         = chain;
}

void 
PdbMuiHVMap::SetHardwareIndex(short mainFrame,
				short slot, short supply_channel)
{
  fMainFrame = mainFrame;
  fSlot = slot;
  fSupplyChannel = supply_channel;
}


void
PdbMuiHVMap::SetTwoPackRange(const short& first, const short& last)
{
  fFirstTwoPack = first;
  fLastTwoPack  = last;
  return;
}

void
PdbMuiHVMap::SoftwareIndex(short& arm, short& gap, short& panel,
				short& orient, short& chain) const
{

//    if (fFirstTwoPack < 0) {
//      // FIXME?  set some return fields in this case?
//  //     std::cout << "PdbMuiHVMap::SoftwareIndex-E1  unused channels" << std::endl;
//  //     print();

//    } else {
    arm      = fArm;
    gap      = fGap;
    panel    = fPanel;
    orient   = fOrientation;
    chain    = fChain;
//    }

  return;
}

void
PdbMuiHVMap::HardwareIndex (short& mainFrame, short& slot, 
			    short& supply_channel) const
{
  mainFrame      = fMainFrame;
  slot           = fSlot;
  supply_channel = fSupplyChannel;

  return;
}

void
PdbMuiHVMap::TwoPackRange(short& first, short& last) const
{
  first = fFirstTwoPack;
  last  = fLastTwoPack;
  return;
}

bool
PdbMuiHVMap::HasHardwareIndex(const short& mainFrame, const short& slot, 
				const short& supply_channel) const
{
  if ( (fMainFrame == mainFrame) && (fSlot == slot) &&
       (fSupplyChannel == supply_channel) ) {
    return true;
  } else {
      return false;
  }
}

bool
PdbMuiHVMap::HasSoftwareIndex(const short& arm, const short& gap,
				const short& panel,
				const short& orient,
				const short& chain,
				const short& twopack) const
{
  if ( (fArm == arm) && (fGap == gap) && (fPanel == panel) &&
       (fOrientation == orient) && (fChain == chain)) {

    if (twopack < 0) {
      // a negative twopack argument is not checked ...
      return true;
    } else if ( (twopack >= Min(fFirstTwoPack,fLastTwoPack)) &&
		(twopack <= Max(fFirstTwoPack,fLastTwoPack)) ) {
      // otherwise, see if the twopack index is within the range.
      return true;
    }
    
  }
  
  return false;
}

bool
PdbMuiHVMap::HasSoftwareIndexLayer(const short& arm, const short& gap,
				const short& panel,
				const short& orient,
				const short& layer,
				const short& twopack) const
{
  if ( (fArm == arm) && (fGap == gap) && (fPanel == panel) &&
       (fOrientation == orient) && (fChain%2 == layer)) {
    if ( (twopack >= Min(fFirstTwoPack,fLastTwoPack)) &&
	 (twopack <= Max(fFirstTwoPack,fLastTwoPack)) ) {
      // otherwise, see if the twopack index is within the range.
      return true;
    }
  }
  
  return false;
}

short
PdbMuiHVMap::Max(const short& num1, const short& num2) const
{
  if (num1>num2){
    return num1;
  } else{
    return num2;
  }
}


short
PdbMuiHVMap::Min(const short& num1, const short& num2) const
{
  if (num1<num2){
    return num1;
  } else{
    return num2;
  }
}
