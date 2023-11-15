//---------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  
//  Implementation of class PdbSvxDaqErrorMap
//
//  Author: Takashi Hachiya
//---------------------------------------------------------------------

#include "PdbSvxDaqErrorMap.hh"

#include <iostream>

PdbSvxDaqErrorMap::PdbSvxDaqErrorMap(): 
  m_detector(0), 
  m_module  (0), 
  m_status  (0)
{
}

PdbSvxDaqErrorMap::~PdbSvxDaqErrorMap()
{
}
   

void PdbSvxDaqErrorMap::setStatus (const int detector,
				   const int module, 
				   const unsigned int status) 
{
  m_detector = detector;
  m_module   = module;
  m_status   = status;
}

int PdbSvxDaqErrorMap::getDetector() const {
  return m_detector;
}

int PdbSvxDaqErrorMap::getModule() const {
  return m_module;
}

unsigned int PdbSvxDaqErrorMap::getStatus() const {
  return m_status;
}


void PdbSvxDaqErrorMap::print() const 
{
  std::cout << "Daq Error Map Entry. " << std::endl;
  std::cout << "DetectorID " << getDetector() << ", "
            << "Module "     << getModule() << ", "
	    << "staus  "     << getStatus() <<" "
	    << "with status of "
	    << (m_status==0 ? "Good" : "Dead")
	    << std::endl;

}

