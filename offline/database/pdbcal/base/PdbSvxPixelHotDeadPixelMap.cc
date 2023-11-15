//---------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  
//  Implementation of class PdbSvxPixelHotDeadPixelMap
//
//  Author: Hiroyuki Sako
//---------------------------------------------------------------------

#include "PdbSvxPixelHotDeadPixelMap.hh"

#include <iostream>

PdbSvxPixelHotDeadPixelMap::PdbSvxPixelHotDeadPixelMap() 
{
  cStatus = SVX_PIXEL_UNKNOWN;
}

PdbSvxPixelHotDeadPixelMap::PdbSvxPixelHotDeadPixelMap (const PdbSvxPixelHotDeadPixelMap &rhs)
{
  cStatus = rhs.getStatusNoRangeCheck();
}


PdbSvxPixelHotDeadPixelMap::~PdbSvxPixelHotDeadPixelMap()
{
}
   
void PdbSvxPixelHotDeadPixelMap::setRawStatus (const int module,
				       const int ROC, 
				       const int column,
				       const int row, 
				       const signed char rawStatus) 
{
  if (rangeCheck(module,ROC,column,row)) {
    cModule = (unsigned char)module;
    cROC    = (unsigned char)ROC;
    cColumn = (unsigned char)column;
    cRow = (unsigned char)row;
    cStatus = rawStatus;
  }
}

void PdbSvxPixelHotDeadPixelMap::setStatus (const int module,
				       const int ROC, 
				       const int column,
				       const int row, 
				       const int status) 
{
  setRawStatus(module,ROC,column,row, (signed char)status);
}

bool PdbSvxPixelHotDeadPixelMap::rangeCheck(const int module, const int ROC, const int column, const int row) const {
  if ((0<=module)&&(module<NMODULE)&&(0<=ROC)&&(ROC<NROC)&&(0<=column)&&(column<NCOLUMN)&&(0<=row)&&(row<NROW)) {
    return true;
  } else {
    std::cerr << "PdbSvxPixelHotDeadPixelMap::rangeCheck, out of range" << std::endl;
    std::cerr << "module = " << module << std::endl;
    std::cerr << "ROC = " << ROC << std::endl;
    std::cerr << "column = " << column << std::endl;
    std::cerr << "row = " << row << std::endl;

    return false;
  }
}

bool PdbSvxPixelHotDeadPixelMap::rangeCheck() const {
  return rangeCheck(getModule(),getROC(),getColumn(),getRow());
}

int PdbSvxPixelHotDeadPixelMap::getStatus() const {
  if (rangeCheck()) {
    return (int)cStatus;
  } else {
    return SVX_PIXEL_OUTOFRANGE;
  }
}

int PdbSvxPixelHotDeadPixelMap::getStatusNoRangeCheck() const {
  return (int)cStatus;
}

char PdbSvxPixelHotDeadPixelMap::getRawStatus() const {
  if (rangeCheck()) {
    return cStatus;
  } else {
    return (char)SVX_PIXEL_OUTOFRANGE;
  }
}

int PdbSvxPixelHotDeadPixelMap::getModule() const {
  return (int)cModule;
}

int PdbSvxPixelHotDeadPixelMap::getROC() const {
  return (int)cROC;
}

int PdbSvxPixelHotDeadPixelMap::getColumn() const {
  return (int)cColumn;
}

int PdbSvxPixelHotDeadPixelMap::getRow() const {
  return (int)cRow;
}



void PdbSvxPixelHotDeadPixelMap::print() const 
{
  unsigned char status = (unsigned char)(cStatus);
  unsigned char pix_status = status & 0x07;// status & 0b00000111;
  unsigned char clus_status = status & 0x38;// status & 0b00111000;
  
  std::cout << "Hot/Dead Pixel Map Entry. " << std::endl;
  std::cout << "Module " << getModule() << ", "
  << "ROC "    << getROC() << ", "
  << "column " << getColumn() << ", "
  << "row "    << getRow() << ", "
  << "with pixel status of "
  << (status == SVX_PIXEL_OUTOFRANGE ? "OUTOFRANGE" : pix_status == (unsigned char)SVX_PIXEL_COLD ? "COLD" : pix_status == (unsigned char)SVX_PIXEL_HOT ? "HOT" : pix_status == (unsigned char)SVX_PIXEL_UNSTABLE ? "UNSTABLE" : pix_status == (unsigned char)SVX_PIXEL_VERYCOLD ? "VERYCOLD" : pix_status == (unsigned char)SVX_PIXEL_VERYHOT ? "VERYHOT" : pix_status == (unsigned char)SVX_PIXEL_DEAD ? "DEAD" : pix_status == (unsigned char)SVX_PIXEL_UNKNOWN ? "UNKNOWN" : "NORMAL")
  <<" and cluster status of "
  << (status == SVX_PIXEL_OUTOFRANGE ? "OUTOFRANGE" : clus_status == (unsigned char)SVX_CLUSTER_COLD ? "COLD" : clus_status == (unsigned char)SVX_CLUSTER_HOT ? "HOT" : clus_status == (unsigned char)SVX_CLUSTER_UNSTABLE ? "UNSTABLE" : clus_status == (unsigned char)SVX_CLUSTER_VERYCOLD ? "VERYCOLD" : clus_status == (unsigned char)SVX_CLUSTER_VERYHOT ? "VERYHOT" : clus_status == (unsigned char)SVX_CLUSTER_DEAD ? "DEAD" : clus_status == (unsigned char)SVX_CLUSTER_UNKNOWN ? "UNKNOWN" : "NORMAL")
  << "." 
  << std::endl;
  
}

