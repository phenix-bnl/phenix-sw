#ifndef CRKCALIB_H_INCLUDED
#define CRKCALIB_H_INCLUDED

#include "CrkCal.h"
#include "PdbRichADC.hh"
#include "PdbRichPar.hh"
#include "PHTimeStamp.h"

class CrkCalib {
public:
  CrkCalib():d_adc_cal_phe("adcphe"),d_adc_cal_ped("adcped"),d_t0_cal("T0"),d_tac_cal("TAC_GAIN"),
    d_slew_cal("SLEW"){}

  void print_adc_phe(void)      {d_adc_cal_phe.print(1);}
  void read_file_adc_phe(void)  {d_adc_cal_phe.read_file();}
  void read_file_adc_phe(char *fname) {d_adc_cal_phe.read_file(fname);}
  void write_file_adc_phe(void) {d_adc_cal_phe.write_file();}
  void fetch_DB_adc_phe(void)   {d_adc_cal_phe.fetch_DB();}
  void fetch_DB_adc_phe(PHTimeStamp time) {d_adc_cal_phe.fetch_DB(time);}
  void store_DB_adc_phe(void)   {d_adc_cal_phe.store_DB();}
  void make_default_adc_phe(void) {
    d_adc_cal_phe.resize(5120);
    for(int i=0;i<5120;i++) {
      PdbRichADC adc(i,10.,2.,40.,9.,10000,1000,100,10,1.1);
      d_adc_cal_phe.setval(i,adc);
    }
  }

  void print_adc_ped(void)      {d_adc_cal_ped.print(1);}
  void read_file_adc_ped(void)  {d_adc_cal_ped.read_file();}
  void read_file_adc_ped(char *fname) {d_adc_cal_ped.read_file(fname);}
  void write_file_adc_ped(void) {d_adc_cal_ped.write_file();}
  void fetch_DB_adc_ped(void)   {d_adc_cal_ped.fetch_DB();}
  void fetch_DB_adc_ped(PHTimeStamp time) {d_adc_cal_ped.fetch_DB(time);}
  void store_DB_adc_ped(void)   {d_adc_cal_ped.store_DB();}
  void make_default_adc_ped(void) {
    d_adc_cal_ped.resize(5120);
    for(int i=0;i<5120;i++) {
      PdbRichADC adc(i,10.,2.,40.,9.,10000,1000,100,10,1.1);
      d_adc_cal_ped.setval(i,adc);
    }
  }

  void print_t0(void)      {d_t0_cal.print(1);}
  void read_file_t0(void)  {d_t0_cal.read_file();}
  void write_file_t0(void) {d_t0_cal.write_file();}
  void fetch_DB_t0(void)   {d_t0_cal.fetch_DB();}
  void fetch_DB_t0(PHTimeStamp time) {d_t0_cal.fetch_DB(time);}
  void store_DB_t0(void)   {d_t0_cal.store_DB();}
  void make_default_t0(void) {
    d_t0_cal.resize(5120);
    for(int i=0;i<5120;i++) {
      PdbRichPar t0(i,100.);
      d_t0_cal.setval(i,t0);
    }
  }

  void print_tac(void)      {d_tac_cal.print(1);}
  void read_file_tac(void)  {d_tac_cal.read_file();}
  void write_file_tac(void) {d_tac_cal.write_file();}
  void fetch_DB_tac(void)   {d_tac_cal.fetch_DB();}
  void fetch_DB_tac(PHTimeStamp time) {d_tac_cal.fetch_DB(time);}
  void store_DB_tac(void)   {d_tac_cal.store_DB();}
  void make_default_tac(void) {
    d_tac_cal.resize(5120);
    for(int i=0;i<5120;i++) {
      PdbRichPar tac_gain(i,0.1);
      d_tac_cal.setval(i,tac_gain);
    }
  }

  void print_slew(void)      {d_slew_cal.print(1);}
  void read_file_slew(void)  {d_slew_cal.read_file();}
  void write_file_slew(void) {d_slew_cal.write_file();}
  void fetch_DB_slew(void)   {d_slew_cal.fetch_DB();}
  void fetch_DB_slew(PHTimeStamp time) {d_slew_cal.fetch_DB(time);}
  void store_DB_slew(void)   {d_slew_cal.store_DB();}
  void make_default_slew(void) {
    d_slew_cal.resize(5120);
    for(int i=0;i<5120;i++) {
      PdbRichPar slew(i,0.0);
      d_slew_cal.setval(i,slew);
    }
  }

private:
  CrkCal<PdbRichADC> d_adc_cal_ped;
  CrkCal<PdbRichADC> d_adc_cal_phe;
  CrkCal<PdbRichPar> d_t0_cal;
  CrkCal<PdbRichPar> d_tac_cal;
  CrkCal<PdbRichPar> d_slew_cal;
};

#endif
