#include "MutrgKey.hh"

const unsigned short MutrgKey::KEY_SHIFT_ARM       =17;
const unsigned short MutrgKey::KEY_SHIFT_STATION   =15;
const unsigned short MutrgKey::KEY_SHIFT_OCTANT    =12;
const unsigned short MutrgKey::KEY_SHIFT_HALFOCTANT=11;
const unsigned short MutrgKey::KEY_SHIFT_GAP       = 9;
const unsigned short MutrgKey::KEY_SHIFT_CATHODE   = 8;
const unsigned short MutrgKey::KEY_SHIFT_STRIP     = 0;
const unsigned short MutrgKey::KEY_MASK_ARM       =0x01;
const unsigned short MutrgKey::KEY_MASK_STATION   =0x03;
const unsigned short MutrgKey::KEY_MASK_OCTANT    =0x07;
const unsigned short MutrgKey::KEY_MASK_HALFOCTANT=0x01;
const unsigned short MutrgKey::KEY_MASK_GAP       =0x03;
const unsigned short MutrgKey::KEY_MASK_CATHODE   =0x01;
const unsigned short MutrgKey::KEY_MASK_STRIP     =0xff;

//////////////////////////////////////////////////////////////

unsigned int MutrgKey::LocToKey(int arm,int station,int octant,
				int halfoctant,int gap,int cathode,
				int strip){
  unsigned int key=0;
  key+=(arm       <<KEY_SHIFT_ARM);
  key+=(station   <<KEY_SHIFT_STATION);
  key+=(octant    <<KEY_SHIFT_OCTANT);
  key+=(halfoctant<<KEY_SHIFT_HALFOCTANT);
  key+=(gap       <<KEY_SHIFT_GAP);
  key+=(cathode   <<KEY_SHIFT_CATHODE);
  key+=(strip     <<KEY_SHIFT_STRIP);
  return key;
}

////////////////////////////////////////////////////////////////

unsigned int MutrgKey::LocToKey(int arm,int station,int octant,
				int halfoctant,int strip){
  // Set gap=0 and cathode=0
  return LocToKey(arm,station,octant,halfoctant,0,0,strip);
}

////////////////////////////////////////////////////////////////

void MutrgKey::KeyToLoc(unsigned int key,
			int &arm,int &station,int &octant,int &halfoctant,
			int &gap,int &cathode,int &strip){
  strip     =KeyToStrip(key);
  cathode   =KeyToCathode(key);
  gap       =KeyToGap(key);
  halfoctant=KeyToHalfOctant(key);
  octant    =KeyToOctant(key);
  station   =KeyToStation(key);
  arm       =KeyToArm(key);
  return;
}

///////////////////////////////////////////////////////////////

void MutrgKey::KeyToLoc(unsigned int key,
			int &arm,int &station,int &octant,int &halfoctant,
			int &strip){
  int gap,cathode; // discard gap and cathode
  KeyToLoc(key,arm,station,octant,halfoctant,gap,cathode,strip);
  return;
}

///////////////////////////////////////////////////////////////

int MutrgKey::KeyToArm(unsigned int key){
  return (int)((key>>KEY_SHIFT_ARM)&KEY_MASK_ARM);
}

///////////////////////////////////////////////////////////////

int MutrgKey::KeyToStation(unsigned int key){
  return (int)((key>>KEY_SHIFT_STATION)&KEY_MASK_STATION);
}

///////////////////////////////////////////////////////////////

int MutrgKey::KeyToOctant(unsigned int key){
  return (int)((key>>KEY_SHIFT_OCTANT)&KEY_MASK_OCTANT);
}

///////////////////////////////////////////////////////////////

int MutrgKey::KeyToHalfOctant(unsigned int key){
  return (int)((key>>KEY_SHIFT_HALFOCTANT)&KEY_MASK_HALFOCTANT);
}

//////////////////////////////////////////////////////////////

int MutrgKey::KeyToGap(unsigned int key){
  return (int)((key>>KEY_SHIFT_GAP)&KEY_MASK_GAP);
}

///////////////////////////////////////////////////////////////

int MutrgKey::KeyToCathode(unsigned int key){
  return (int)((key>>KEY_SHIFT_CATHODE)&KEY_MASK_CATHODE);
}

///////////////////////////////////////////////////////////////

int MutrgKey::KeyToStrip(unsigned int key){
  return (int)((key>>KEY_SHIFT_STRIP)&KEY_MASK_STRIP);
}

///////////////////////////////////////////////////////////////

unsigned int MutrgKey::Mask(bool arm,bool station,bool octant,
			    bool halfoctant,bool gap,bool cathode,bool strip){
  unsigned int mask=0;
  mask|=(arm        ? KEY_MASK_ARM       <<KEY_SHIFT_ARM        : 0);
  mask|=(station    ? KEY_MASK_STATION   <<KEY_SHIFT_STATION    : 0);
  mask|=(octant     ? KEY_MASK_OCTANT    <<KEY_SHIFT_OCTANT     : 0);
  mask|=(halfoctant ? KEY_MASK_HALFOCTANT<<KEY_SHIFT_HALFOCTANT : 0);
  mask|=(gap        ? KEY_MASK_GAP       <<KEY_SHIFT_GAP        : 0);
  mask|=(cathode    ? KEY_MASK_CATHODE   <<KEY_SHIFT_CATHODE    : 0);
  mask|=(strip      ? KEY_MASK_STRIP     <<KEY_SHIFT_STRIP      : 0);
  return mask;
}

////////////////////////////////////////////////////////////////

unsigned int MutrgKey::Mask(bool arm,bool station,bool octant,
			    bool halfoctant,bool strip){
  return Mask(arm,station,octant,halfoctant,false,false,strip);
}

////////////////////////////////////////////////////////////////

unsigned int MutrgKey::Mask(unsigned int key,
			    bool arm,bool station,bool octant,
			    bool halfoctant,bool gap,bool cathode,bool strip){
  return key&Mask(arm,station,octant,halfoctant,gap,cathode,strip);
}

/////////////////////////////////////////////////////////////////

unsigned int MutrgKey::Mask(unsigned int key,
			    bool arm,bool station,bool octant,
			    bool halfoctant,bool strip){
  return key&Mask(arm,station,octant,halfoctant,false,false,strip);
}

////////////////////////////////////////////////////////////////////
