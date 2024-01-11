#ifndef __DBBCUCALWRAPPER_H__
#define __DBBCUCALWRAPPER_H__

#include <stddef.h>
#include "PHTable.hh"
#include "dBbcUcal.h"
class dBbcUcalWrapper: public PHTable
{
public:
  dBbcUcalWrapper(const char* name = "dBbcUcal", const size_t& max_rows = 1);
  dBbcUcalWrapper(const dBbcUcalWrapper& source);
  dBbcUcalWrapper& operator=(const dBbcUcalWrapper& source);

  ~dBbcUcalWrapper();

  void* RawTableData();
  DBBCUCAL_ST* TableData();

  DBBCUCAL_ST& operator[](const size_t& row);
  const DBBCUCAL_ST& operator[](const size_t& row) const;
  virtual void Print(const size_t num_rows, const size_t first_row = 0) const;
  virtual void Print(Option_t* option) const;

  void SetRowSize(const size_t& row_size);
  void SetRowCount(const size_t& n);
  void SetMaxRowCount(const size_t& max_rows);

  void set_Pmt(size_t n, short v) {
    fTableData[n].Pmt = v;
  }
  short get_Pmt(size_t n) const {
    return fTableData[n].Pmt;
  }
  void set_AdcChGain(size_t n, float v) {
    fTableData[n].AdcChGain = v;
  }
  float get_AdcChGain(size_t n) const {
    return fTableData[n].AdcChGain;
  }
  void set_TdcChGain0(size_t n, float v) {
    fTableData[n].TdcChGain0 = v;
  }
  float get_TdcChGain0(size_t n) const {
    return fTableData[n].TdcChGain0;
  }
  void set_TdcChGain1(size_t n, float v) {
    fTableData[n].TdcChGain1 = v;
  }
  float get_TdcChGain1(size_t n) const {
    return fTableData[n].TdcChGain1;
  }
  void set_TdcOffset0(size_t n, float v) {
    fTableData[n].TdcOffset0 = v;
  }
  float get_TdcOffset0(size_t n) const {
    return fTableData[n].TdcOffset0;
  }
  void set_TdcOffset1(size_t n, float v) {
    fTableData[n].TdcOffset1 = v;
  }
  float get_TdcOffset1(size_t n) const {
    return fTableData[n].TdcOffset1;
  }
  void set_TdcOver0_mean(size_t n, float v) {
    fTableData[n].TdcOver0_mean = v;
  }
  void set_TdcOver0_sigma(size_t n, float v) {
    fTableData[n].TdcOver0_sigma = v;
  }
  void set_TdcOver1_mean(size_t n, float v) {
    fTableData[n].TdcOver1_mean = v;
  }
  void set_TdcOver1_sigma(size_t n, float v) {
    fTableData[n].TdcOver1_sigma = v;
  }
  void set_TdcThreshold0(size_t n, float v) {
    fTableData[n].TdcThreshold0 = v;
  }
  void set_TdcThreshold1(size_t n, float v) {
    fTableData[n].TdcThreshold1 = v;
  }
  void set_PulseHeightReso(size_t n, float v) {
    fTableData[n].PulseHeightReso = v;
  }
  void set_PMTGain(size_t n, float v) {
    fTableData[n].PMTGain = v;
  }
  void set_Pedestal(size_t n, float v) {
    fTableData[n].Pedestal = v;
  }
  float get_Pedestal(size_t n) const {
    return fTableData[n].Pedestal;
  }
  void set_AdcGainFac(size_t n, float v) {
    fTableData[n].AdcGainFac = v;
  }
  float get_AdcGainFac(size_t n) const {
    return fTableData[n].AdcGainFac;
  }
  void set_SlewParA(size_t n, float v) {
    fTableData[n].SlewParA = v;
  }
  void set_SlewParA0(size_t n, float v) {
    fTableData[n].SlewParA0 = v;
  }
  void set_SlewParB0(size_t n, float v) {
    fTableData[n].SlewParB0 = v;
  }
  void set_SlewParC0(size_t n, float v) {
    fTableData[n].SlewParC0 = v;
  }
  void set_SlewParA1(size_t n, float v) {
    fTableData[n].SlewParA1 = v;
  }
  void set_SlewParB1(size_t n, float v) {
    fTableData[n].SlewParB1 = v;
  }
  void set_SlewParC1(size_t n, float v) {
    fTableData[n].SlewParC1 = v;
  }
  void set_Z0overC_offset(size_t n, float v) {
    fTableData[n].Z0overC_off = v;
  }
  void set_dif_off(size_t n, float v) {
    fTableData[n].dif_off = v;
  }
  void set_MeanTDC_offset(size_t n, float v) {
    fTableData[n].MeanTDC_off = v;
  }
  void set_NoiseHeight(size_t n, float v) {
    fTableData[n].NoiseHeight = v;
  }
  float get_NoiseHeight(size_t n) const {
    return fTableData[n].NoiseHeight;
  }
  void set_NoiseHitProb(size_t n, float v) {
    fTableData[n].NoiseHitProb = v;
  }
  float get_NoiseHitProb(size_t n) const {
    return fTableData[n].NoiseHitProb;
  }
  void set_TimeReso(size_t n, float v) {
    fTableData[n].TimeReso = v;
  }
  float get_TimeReso(size_t n) const {
    return fTableData[n].TimeReso;
  }
  float set_FakePede_mean(size_t n) const {
    return fTableData[n].FakePede_mean;
  }
  float set_FakePede_sigma(size_t n) const {
    return fTableData[n].FakePede_sigma;
  }

private:
  DBBCUCAL_ST* fTableData;

  ClassDef(dBbcUcalWrapper,1)
};
#endif /*__DBBCUCALWRAPPER_H__*/
