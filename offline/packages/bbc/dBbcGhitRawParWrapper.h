#ifndef __DBBCGHITRAWPARWRAPPER_H__
#define __DBBCGHITRAWPARWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcGhitRawPar.h"
class dBbcGhitRawParWrapper: public PHTable
{
public:
  dBbcGhitRawParWrapper(const char* name = "dBbcGhitRawPar", const size_t& max_rows = 1);
  dBbcGhitRawParWrapper(const dBbcGhitRawParWrapper& source);
  dBbcGhitRawParWrapper& operator=(const dBbcGhitRawParWrapper& source);

  ~dBbcGhitRawParWrapper();

  void* RawTableData();
  DBBCGHITRAWPAR_ST* TableData();

  DBBCGHITRAWPAR_ST& operator[](const size_t& row);
  const DBBCGHITRAWPAR_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

// Setting run number 
  void set_RunNumber(size_t n, long v) {
    fTableData[n].RunNumber = v;
  }
  long get_RunNumber(size_t n) const {
    return fTableData[n].RunNumber;
  }

  void set_CalibVersion(size_t n, long v) {
    fTableData[n].CalibVersion = v;
  }
  long get_CalibVersion(size_t n) const {
    return fTableData[n].CalibVersion;
  }

  void set_SimFlag(size_t n, long v) {
    fTableData[n].SimFlag = v;
  }
  long get_SimFlag(size_t n) const {
    return fTableData[n].SimFlag;
  }

  void set_randseed(size_t n, long v) {
    fTableData[n].randseed = v;
  }

  long get_randseed(size_t n) const {
    return fTableData[n].randseed;
  }
  void set_AngleCut(size_t n, float v) {
    fTableData[n].AngleCut = v;
  }
  float get_AngleCut(size_t n) const {
    return fTableData[n].AngleCut;
  }
  void set_Nindex(size_t n, float v) {
    fTableData[n].Nindex = v;
  }
  float get_Nindex(size_t n) const {
    return fTableData[n].Nindex;
  }
  void set_N0(size_t n, float v) {
    fTableData[n].N0 = v;
  }
  float get_N0(size_t n) const {
    return fTableData[n].N0;
  }
  void set_MomLowerLim(size_t n, float v) {
    fTableData[n].MomLowerLim = v;
  }
  float get_MomLowerLim(size_t n) const {
    return fTableData[n].MomLowerLim;
  }
  void set_MaxAdc(size_t n, float v) {
    fTableData[n].MaxAdc = v;
  }
  float get_MaxAdc(size_t n) const {
    return fTableData[n].MaxAdc;
  }
  void set_MaxTdc(size_t n, float v) {
    fTableData[n].MaxTdc = v;
  }
  float get_MaxTdc(size_t n) const {
    return fTableData[n].MaxTdc;
  }
  void set_Z0overC_offset(size_t n, float v) {
    fTableData[n].Z0overC_off = v;
  }
  float get_Z0overC_offset(size_t n) const {
    return fTableData[n].Z0overC_off;
  }
  void set_MeanTDC_offset(size_t n, float v) {
    fTableData[n].MeanTDC_off = v;
  }
  float get_MeanTDC_offset(size_t n) const {
    return fTableData[n].MeanTDC_off;
  }
  void set_RunByRun_offset(size_t n, float v) {
    fTableData[n].RunByRun_off = v;
  }
  float get_RunByRun_offset(size_t n) const {
    return fTableData[n].RunByRun_off;
  }
  void set_ThresholdFactor(size_t n, float v) {
    fTableData[n].ThresholdFactor = v;
  }
  float get_ThresholdFactor(size_t n) const {
    return fTableData[n].ThresholdFactor;
  }

private:
  DBBCGHITRAWPAR_ST* fTableData;

  ClassDef(dBbcGhitRawParWrapper,1)
};
#endif /*__DBBCGHITRAWPARWRAPPER_H__*/
