//  Implementation of class PdbSvxCoordinateOffset
//  Author: Takashi Hachiya
//  Date  : Dec/21/2011
#include "PdbSvxCoordinateOffset.hh"

#include <iostream>

PdbSvxCoordinateOffset::PdbSvxCoordinateOffset()
{
  m_usedRun       = -9999;
  for(int idx=0; idx<3; idx++){
    m_offsetVtxEtoW[idx]  = -9999.0;
    m_offsetVtxtoCnt[idx] = -9999.0;
    m_offsetCntEtoW[idx]  = -9999.0;
  }
  m_geomVersion   = -9999;
}

void
PdbSvxCoordinateOffset::print() const
{
  std::cout << std::endl;
  std::cout << "Used RunNumber   : " << m_usedRun << std::endl;

  std::cout << "Offset Vtx East to West : " << std::flush;
  std::cout << m_offsetVtxEtoW[0]<<", "<<m_offsetVtxEtoW[1]<<", "<<m_offsetVtxEtoW[2]<< std::endl;
  std::cout << "Offset Vtx to Cnt      : " << std::flush;
  std::cout << m_offsetVtxtoCnt[0]<<", "<<m_offsetVtxtoCnt[1]<<", "<<m_offsetVtxtoCnt[2]<< std::endl;
  std::cout << "Offset Cnt East to West : " << std::flush;
  std::cout << m_offsetCntEtoW[0]<<", "<<m_offsetCntEtoW[1]<<", "<<m_offsetCntEtoW[2]<< std::endl;

  std::cout << "Geometry Version : " << m_geomVersion << std::endl;
}
