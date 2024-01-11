//$Id: EmcSector.C,v 1.3 2001/06/18 22:10:57 aphecetc Exp $

#include "EmcSector.h"

//____________________________________________________________________
EmcSector::EmcSector()
{
  SectorId = "unknown" ;
  SectorN  = 9999 ;
}

//____________________________________________________________________
EmcSector::~EmcSector()
{
}

//____________________________________________________________________
void EmcSector::setSectorId(const char* sectorName)
{
  SectorId = sectorName ;
}
