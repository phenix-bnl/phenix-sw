//---------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  
//  Implementation of class PdbSvxPixelHotDeadChipMap
//
//  Author: Hiroyuki Sako
//---------------------------------------------------------------------

#include "PdbSvxPixelHotDeadChipMap.hh"

#include <iostream>

PdbSvxPixelHotDeadChipMap::PdbSvxPixelHotDeadChipMap() 
{
  cStatus = SVX_CHIP_UNKNOWN;
}

PdbSvxPixelHotDeadChipMap::PdbSvxPixelHotDeadChipMap (const PdbSvxPixelHotDeadChipMap &rhs)
{
  cStatus = rhs.getStatusNoRangeCheck();
}



PdbSvxPixelHotDeadChipMap::~PdbSvxPixelHotDeadChipMap()
{
}
   
void PdbSvxPixelHotDeadChipMap::setRawStatus (const int module,
				       const int ROC, 
				       const signed char rawStatus) 
{
  if (rangeCheck(module,ROC)) {
    cModule = (unsigned char)module;
    cROC = (unsigned char)ROC;
    cStatus = rawStatus;
  }
}

void PdbSvxPixelHotDeadChipMap::setStatus (const int module,
				       const int ROC, 
				       const int status) 
{
  setRawStatus(module,ROC,(signed char)status);
}



bool PdbSvxPixelHotDeadChipMap::rangeCheck(const int module, const int ROC) const {
  if ((0<=module)&&(module<NMODULE)&&(0<=ROC)&&(ROC<NROC)) {
    return true;
  } else {
    std::cerr << "PdbSvxPixelHotDeadChipMap::rangeCheck, out of range" << std::endl;
    std::cerr << "module = " << module << std::endl;
    std::cerr << "ROC = " << ROC << std::endl;

    return false;
  }
}

bool PdbSvxPixelHotDeadChipMap::rangeCheck() const {
  return rangeCheck(getModule(),getROC());
}

int PdbSvxPixelHotDeadChipMap::getStatus() const {
  if (rangeCheck()) {
    return (int)cStatus;
  } else {
    return SVX_CHIP_OUTOFRANGE;
  }
}

int PdbSvxPixelHotDeadChipMap::getStatusNoRangeCheck() const {
  return (int)cStatus;
}

char PdbSvxPixelHotDeadChipMap::getRawStatus() const {
  if (rangeCheck()) {
    return cStatus;
  } else {
    return (char)SVX_CHIP_OUTOFRANGE;
  }
}

int PdbSvxPixelHotDeadChipMap::getModule() const {
  return (int)cModule;
}

int PdbSvxPixelHotDeadChipMap::getROC() const {
  return (int)cROC;
} 

void PdbSvxPixelHotDeadChipMap::print() const 
{
  std::cout << "Hot/Dead Chip Map Entry. " << std::endl;
  std::cout << "Module " << getModule() << ", "
	    << "ROC "    << getROC() << ", "
	    << "with status of "
	    << (cStatus == (char)SVX_CHIP_DEAD ? "DEAD" : cStatus == (char)SVX_CHIP_HOT ? "HOT" : cStatus == (char)SVX_CHIP_OUTOFRANGE ? "OUTOFRANGE" : cStatus == (char)SVX_CHIP_UNKNOWN ? "UNKNOWN" : "NORMAL")
	    << "." 
	    << std::endl;

}

