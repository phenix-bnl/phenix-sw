//  Implementation of class PdbSvxBeamOffset
//  Author: Takashi Hachiya
//  Date  : Dec/20/2011
#include "PdbSvxBeamCenter.hh"

#include <iostream>

PdbSvxBeamCenter::PdbSvxBeamCenter()
{
  m_usedRun       = -9999;
  m_beamCenter[0] = -9999.9;
  m_beamCenter[1] = -9999.9;
  m_geomVersion   = -9999;
}

void
PdbSvxBeamCenter::print() const
{
  std::cout << std::endl;
  std::cout << "Used RunNumber   : " << m_usedRun << std::endl;
  std::cout << "BeamCenter x-y   : " << m_beamCenter[0]<<" "<<m_beamCenter[1]<< std::endl;
  std::cout << "Geometry Version : " << m_geomVersion << std::endl;
}
