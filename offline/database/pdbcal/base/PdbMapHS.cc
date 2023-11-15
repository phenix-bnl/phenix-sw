//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMapHS
//
//  Author: lebedev
//-----------------------------------------------------------------------------
#include "PdbMapHS.hh"

#include <iostream>

PdbMapHS::PdbMapHS()
{
  nDim = 12;
  iArm = 0;
  iSector = 0;
  iSide = 0;
  iPlane = 0;
  iFirstWire = 0;
  iLastWire = 0;
  iCrate = 0;
  iSlot = 0;
  iPSAddress = 0;
  iFirstChannel = 0;
  iLastChannel = 0;
  iFlag = -1;
}

PdbMapHS::PdbMapHS(const PdbMapHS &rhs)
{
  nDim = 12;
  iArm = rhs.iArm;
  iSector = rhs.iSector;
  iSide = rhs.iSide;
  iPlane = rhs.iPlane;
  iFirstWire = rhs.iFirstWire;
  iLastWire = rhs.iLastWire;
  iCrate = rhs.iCrate;
  iSlot = rhs.iSlot;
  iPSAddress = rhs.iPSAddress;
  iFirstChannel = rhs.iFirstChannel;
  iLastChannel = rhs.iLastChannel;
  iFlag = rhs.iFlag;
}

PdbMapHS::PdbMapHS(int iarm, int isector, int iside, int iplane, int ifirstwire, int ilastwire, int icrate,
		   int islot, int ipsaddress, int ifirstchannel, int ilastchannel, int iflag)
{
  nDim = 12;
  iArm = iarm;
  iSector = isector;
  iSide = iside;
  iPlane = iplane;
  iFirstWire = ifirstwire;
  iLastWire = ilastwire;
  iCrate = icrate;
  iSlot = islot;
  iPSAddress = ipsaddress;
  iFirstChannel = ifirstchannel;
  iLastChannel = ilastchannel;
  iFlag = iflag;
}


PdbMapHS::~PdbMapHS()
{
}

const char* PdbMapHS::getParName(size_t index) const
{
  switch (index) {
  case arm:
      return "Arm";
  case sector:
      return "Sector";
  case side:
      return "Z-Side";
  case plane:
      return "Plane";
  case firstwire:
      return "First Wire";
  case lastwire:
      return "Last Wire";
  case crate:
      return "Crate";
  case slot:
      return "Slot";
  case psaddress:
      return "PS Address";
  case firstchannel:
      return "First Channel";
  case lastchannel:
      return "Last Channel";
  case flag:
      return "Flag";
  default:
      return 0;
  }
}

void PdbMapHS::setParameter(size_t index, int theParValue)
{
  switch (index) {
  case arm:
    iArm=theParValue;
    break;
  case sector:
    iSector=theParValue;
    break;
  case side:
    iSide=theParValue;
    break;
  case plane:
    iPlane=theParValue;
    break;
  case firstwire:
    iFirstWire=theParValue;
    break;
  case lastwire:
    iLastWire=theParValue;
    break;
  case crate:
    iCrate=theParValue;
    break;
  case slot:
    iSlot=theParValue;
    break;
  case psaddress:
    iPSAddress=theParValue;
    break;
  case firstchannel:
    iFirstChannel=theParValue;
    break;
  case lastchannel:
    iLastChannel=theParValue;
    break;
  case flag:
    iFlag=theParValue;
    break;
  default:
    std::cout << "PdbMapHS::setParameter - Index value = "
         << index  << " is out of range. [0.."
         << nDim-1 << "] is valid." << std::endl;
  }
}

void PdbMapHS::setAllParameters(int iarm, int isector, int iside, int iplane, int ifirstwire, int ilastwire,
				int icrate, int islot, int ipsaddress, int ifirstchannel, int ilastchannel, int iflag)
{
  iArm = iarm;
  iSector = isector;
  iSide = iside;
  iPlane = iplane;
  iFirstWire = ifirstwire;
  iLastWire = ilastwire;
  iCrate = icrate;
  iSlot = islot;
  iPSAddress = ipsaddress;
  iFirstChannel = ifirstchannel;
  iLastChannel = ilastchannel;
  iFlag = iflag;
}

int PdbMapHS::getParameter(size_t index) const
{
  switch (index) {
  case arm:
    return iArm;
  case sector:
    return iSector;
  case side:
    return iSide;
  case plane:
    return iPlane;
  case firstwire:
    return iFirstWire;
  case lastwire:
    return iLastWire;
  case crate:
    return iCrate;
  case slot:
    return iSlot;
  case psaddress:
    return iPSAddress;
  case firstchannel:
    return iFirstChannel;
  case lastchannel:
    return iLastChannel;
  case flag:
    return iFlag;
  default:
    return 0;
  }
}

void PdbMapHS::print() const
{
  std::cout << "Hardware/Software Map Entry. Flag = " << getParameter(11) << std::endl;
}

