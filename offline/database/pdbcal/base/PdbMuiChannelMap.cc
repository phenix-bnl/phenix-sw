//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbMuiChannelMap
//
//  Author: pope
//-----------------------------------------------------------------------------

#include <iostream>
#include "PdbMuiChannelMap.hh"

using namespace std;

PdbMuiChannelMap::PdbMuiChannelMap()
  : fModuleID(0xffff),    fArm(-1),       fOrientation(-1),
    fGap(-1), fPanel(-1), fWordIndex(-1), fROC(-1), fWordInROC(-1),
    fFirstChannel(-1),    fLastChannel(-1),
    fFirstTwoPack(-1),    fLastTwoPack(-1),
    fFirstLocation(-1),   fLastLocation(-1)
{
  //  cout << "PdbMuiChannelMap(ctor) default ctor called" << endl;
}

PdbMuiChannelMap::PdbMuiChannelMap(unsigned int module_id,
				   short roc, short cable,
				   short sequence, 
				   short first_channel,
				   short last_channel,
				   short arm, short gap,
				   short panel, short orient,
				   short first_twopack,
				   short last_twopack,
				   short first_location,
				   short last_location)
  : fModuleID(module_id),
    fArm(arm), fOrientation(orient), fGap(gap), fPanel(panel),
    fWordIndex(sequence), fROC(roc), fWordInROC(cable),
    fFirstChannel(first_channel),    fLastChannel(last_channel),
    fFirstTwoPack(first_twopack),    fLastTwoPack(last_twopack),
    fFirstLocation(first_location),  fLastLocation(last_location)
{
  //  cout << "PdbMuiChannelMap(ctor) full ctor called" << endl;
}

PdbMuiChannelMap::PdbMuiChannelMap(const PdbMuiChannelMap& rhs)
  : fModuleID(rhs.fModuleID),       fArm(rhs.fArm),
    fOrientation(rhs.fOrientation), fGap(rhs.fGap),
    fPanel(rhs.fPanel),             fWordIndex(rhs.fWordIndex),
    fROC(rhs.fROC),                 fWordInROC(rhs.fWordInROC),
    fFirstChannel(rhs.fFirstChannel),
    fLastChannel (rhs.fLastChannel),
    fFirstTwoPack(rhs.fFirstTwoPack),
    fLastTwoPack (rhs.fLastTwoPack),
    fFirstLocation(rhs.fFirstLocation),
    fLastLocation (rhs.fLastLocation)
{
  //  cout << "PdbMuiChannelMap(ctor) copy ctor called" << endl;
}

PdbMuiChannelMap::~PdbMuiChannelMap()
{ }

PdbMuiChannelMap&
PdbMuiChannelMap::operator=(const PdbMuiChannelMap& rhs)
{
  if (this == &rhs) return *this;  // check for self-assignment

  fModuleID      = rhs.fModuleID;
  fArm           = rhs.fArm;
  fOrientation   = rhs.fOrientation;
  fGap           = rhs.fGap;
  fPanel         = rhs.fPanel;
  fWordIndex     = rhs.fWordIndex;
  fROC           = rhs.fROC;
  fWordInROC     = rhs.fWordInROC;
  fFirstChannel  = rhs.fFirstChannel;
  fLastChannel   = rhs.fLastChannel;
  fFirstTwoPack  = rhs.fFirstTwoPack;
  fLastTwoPack   = rhs.fLastTwoPack;
  fFirstLocation = rhs.fFirstLocation;
  fLastLocation  = rhs.fLastLocation;

  return *this;
}

void
PdbMuiChannelMap::print() const
{
  cout << " A" << fArm << " G" << fGap << " P" << fPanel
       << " O" << fOrientation
       << " T" << fFirstTwoPack  << "-" << fLastTwoPack
       << " X" << fFirstLocation << "-" << fLastLocation << " : "
       << " M" << hex << fModuleID << dec
       << " S" << fWordIndex
       << " R" << fROC << " I" << fWordInROC
       << " C" << fFirstChannel  << "-" << fLastChannel
       << endl;
}

void 
PdbMuiChannelMap::SetSoftwareIndex(short arm, short gap, short panel,
				   short orient)
{
  fArm           = arm;
  fOrientation   = orient;
  fGap           = gap;
  fPanel         = panel;
}

void 
PdbMuiChannelMap::SetHardwareIndex(unsigned int module_id, short roc,
				   short cable, short sequence)
{
  fModuleID      = module_id;
  fWordIndex     = sequence;
  fROC           = roc;
  fWordInROC     = cable;
}

void
PdbMuiChannelMap::SetChannelRange(const short& first, const short& last)
{
  fFirstChannel = first;
  fLastChannel  = last;
  return;
}

void
PdbMuiChannelMap::SetTwoPackRange(const short& first, const short& last)
{
  fFirstTwoPack = first;
  fLastTwoPack  = last;
  return;
}

void
PdbMuiChannelMap::SetLocationRange(const short& first, const short& last)
{
  fFirstLocation = first;
  fLastLocation  = last;
  return;
}

void
PdbMuiChannelMap::SoftwareIndex(const short& channel,
				short& arm, short& gap, short& panel,
				short& orient, short& twopack) const
{

  if (fFirstTwoPack < 0) {
    // FIXME?  set some return fields in this case?
//     cout << "PdbMuiChannelMap::SoftwareIndex-E1  unused channels" << endl;
//     print();
    return;

  } else {
    short j;

    if (fFirstTwoPack < fLastTwoPack){
      j = fFirstTwoPack + channel - fFirstChannel;
    }
    else{
      j = fLastTwoPack + fLastChannel - channel;
    }	

    if ((j < Min(fFirstTwoPack, fLastTwoPack))||(j >Max(fFirstTwoPack, fLastTwoPack))) {

      cout << "PdbMuiChannelMap::SoftwareIndex-E2  two-pack " << j
	   << " out of range "
	   << Min(fFirstTwoPack, fLastTwoPack)
	   << " - " << Max(fFirstTwoPack, fLastTwoPack) << endl;
      print();

      return;

    } else {
      arm      = fArm;
      gap      = fGap;
      panel    = fPanel;
      orient   = fOrientation;
      twopack  = j;
    }
  }

}

void
PdbMuiChannelMap::SoftwareIndex(short& arm, short& gap, short& panel,
				short& orient) const
{

  if (fFirstTwoPack < 0) {
    // FIXME?  set some return fields in this case?
//     cout << "PdbMuiChannelMap::SoftwareIndex-E1  unused channels" << endl;
//     print();

  } else {
    arm      = fArm;
    gap      = fGap;
    panel    = fPanel;
    orient   = fOrientation;
  }

  return;
}

void
PdbMuiChannelMap::LocationIndex(const short& twopack,
				short& arm, short& gap, short& orient,
				short& location) const
{
  if (fFirstLocation < 0) {
    // FIXME?  set some return fields in this case?
//     cout << "PdbMuiChannelMap::LocationIndex-E1  unused channels" << endl;
//     print();
    return;

  } else {
    short j = twopack - Min(fFirstTwoPack, fLastTwoPack) + fFirstLocation;

    if ( (j < fFirstLocation) || (j > fLastLocation) ) {

      cout << "PdbMuiChannelMap::LocationIndex-E2  location " << j
	   << " out of range "
	   << fFirstLocation << " - " << fLastLocation << endl;
      print();

      return;

    } else {
      arm      = fArm;
      gap      = fGap;
      orient   = fOrientation;
      location = j;
    }
  }

}

void
PdbMuiChannelMap::HardwareIndex(const short& twopack,
				unsigned int& module, short& roc,
				short& sequence, short& cable,
				short& channel) const
{
  if (fFirstChannel < 0) {
    // FIXME?  set some return fields in this case?
//     cout << "PdbMuiChannelMap::HardwareIndex-E1  unused channels" << endl;
//     print();
    return;

  } else {
    short j;
    if(fFirstTwoPack<fLastTwoPack)
      {
	j = twopack - Min(fFirstTwoPack,fLastTwoPack) + fFirstChannel;
      }else{
	//cout<<"FChannel("<<fFirstChannel
	//    <<") LChannel("<<fLastChannel
	//    <<") FTPack("<<fFirstTwoPack
	//    <<") LTPack("<<fLastTwoPack
	//    <<") TP("<<twopack;
	j = fFirstChannel + (fFirstTwoPack) - twopack;
	//cout<<") Bit("<<j<<endl;
      }
    if ( (j < fFirstChannel) || (j > fLastChannel) ) {

      cout << "PdbMuiChannelMap::HardwareIndex-E2  channel " << j
	   << " out of range "
	   << fFirstChannel << " - " << fLastChannel << endl;
      print();

      return;

    } else {
      module   = fModuleID;
      roc      = fROC;
      sequence = fWordIndex;
      cable    = fWordInROC;
      channel  = j;
    }
  }

}

void
PdbMuiChannelMap::HardwareIndex(unsigned int& module, short& roc,
				short& sequence, short& cable) const
{
  if (fFirstChannel < 0) {
    // FIXME?  set some return fields in this case?
//     cout << "PdbMuiChannelMap::HardwareIndex-E1  unused channels" << endl;
//     print();

  } else {
    module   = fModuleID;
    roc      = fROC;
    sequence = fWordIndex;
    cable    = fWordInROC;
  }

  return;
}

void
PdbMuiChannelMap::ChannelRange(short& first, short& last) const
{
  first = fFirstChannel;
  last  = fLastChannel;
  return;
}

void
PdbMuiChannelMap::TwoPackRange(short& first, short& last) const
{
  first = fFirstTwoPack;
  last  = fLastTwoPack;
  return;
}

void
PdbMuiChannelMap::LocationRange(short& first, short& last) const
{
  first = fFirstLocation;
  last  = fLastLocation;
  return;
}

bool
PdbMuiChannelMap::HasHardwareIndex(const unsigned int& module_id,
				   const short& sequence) const
{
  if ( (fModuleID == module_id) && (fWordIndex == sequence) ) {
    return true;
  } else {
    return false;
  }
}

bool
PdbMuiChannelMap::HasHardwareIndex(const unsigned int& module_id,
				   const short& roc,
				   const short& cable,
				   const short& channel) const
{
  if ( (fModuleID == module_id) && (fROC == roc) &&
       (fWordInROC == cable) ) {

    if (channel < 0) {
      // a negative channel argument is not checked ...
      return true;
    } else if ( (channel >= fFirstChannel) &&
		(channel <= fLastChannel )   ) {
      // otherwise, see if the channel index is within the range.
      return true;
    }

  }

  return false;
}

bool
PdbMuiChannelMap::HasSoftwareIndex(const short& arm,
				   const short& gap,
				   const short& panel,
				   const short& orient,
				   const short& twopack) const
{
  if ( (fArm == arm) && (fGap == gap) && (fPanel == panel) &&
       (fOrientation == orient) ) {

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


short
PdbMuiChannelMap::Max(const short& num1, const short& num2) const
{
  if (num1>num2){
    return num1;
  } else{
    return num2;
  }
}


short
PdbMuiChannelMap::Min(const short& num1, const short& num2) const
{
  if (num1<num2){
    return num1;
  } else{
    return num2;
  }
}
