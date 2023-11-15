//-----------------------------------------------------------------------------
//  $header$
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Implementation of class PdbRichADC
//
//  Author: akiba
//-----------------------------------------------------------------------------
#include <iostream>
#include <string>

#include "PdbRichADC.hh"

using namespace std;

PdbRichADC::PdbRichADC(): 
  d_status(STAT_BAD),
  d_PMTid(-1), 
  d_pedestal(0), 
  d_ped_width(0), 
  d_PE_pos(0),
  d_PE_width(0.), 
  d_Nped(0),
  d_N1PE(0),
  d_N2PE(0),
  d_N3PE(0),
  d_chisqr(0),
  d_cal_time(0,0,0,0,0,0)
  {}

PdbRichADC::PdbRichADC(PdbRichADC const &rhs):
  d_status(rhs.d_status),
  d_PMTid(rhs.d_PMTid),
  d_pedestal(rhs.d_pedestal),
  d_ped_width(rhs.d_ped_width),
  d_PE_pos(rhs.d_PE_pos),
  d_PE_width(rhs.d_PE_width),
  d_Nped(rhs.d_Nped),
  d_N1PE(rhs.d_N1PE),
  d_N2PE(rhs.d_N2PE),
  d_N3PE(rhs.d_N3PE),
  d_chisqr(rhs.d_chisqr),
  d_cal_time(rhs.d_cal_time)
  {}

PdbRichADC & PdbRichADC::operator = (PdbRichADC const& rhs){
  d_status = rhs.d_status;
  d_PMTid = rhs.d_PMTid;
  d_pedestal = rhs.d_pedestal;
  d_ped_width = rhs.d_ped_width;
  d_PE_pos = rhs.d_PE_pos;
  d_PE_width = rhs.d_PE_width;
  d_Nped = rhs.d_Nped;
  d_N1PE = rhs.d_N1PE;
  d_N2PE = rhs.d_N2PE;
  d_N3PE = rhs.d_N3PE;
  d_chisqr = rhs.d_chisqr;
  d_cal_time = rhs.d_cal_time;
  return *this;
}

PdbRichADC::PdbRichADC(int PMTid,
		       float ped, float pedw, float PE, float PEw,
		       float Nped, float N1, float N2, float N3,
		       float chisqr):
  d_status(STAT_OK),
  d_PMTid(PMTid),
  d_pedestal(ped),
  d_ped_width(pedw),
  d_PE_pos(PE),
  d_PE_width(PEw),
  d_Nped(Nped),
  d_N1PE(N1),
  d_N2PE(N2),
  d_N3PE(N3),
  d_chisqr(chisqr),
  d_cal_time(0,0,0,0,0,0)
  {}


PdbRichADC::~PdbRichADC() {}

void PdbRichADC::print() const
{
  write(cout);
}

void PdbRichADC::write(ostream &os) const
{
  std::string status;

  if(d_status == STAT_OK)  status = "OK ";
  else if(d_status == STAT_BAD) status = "BAD ";
  else if(d_status == STAT_PREV) status = "PREV ";
  else if(d_status == STAT_SM) status = "SM ";
  else if(d_status == STAT_EM) status = "EM ";
  else if(d_status == STAT_NH) status = "NH ";
  else status = "BAD ";

  os << status;
  if(d_status != STAT_BAD) {
    os << d_PMTid << " ";
    os << d_pedestal << " ";
    os << d_ped_width << " "; 
    os << d_PE_pos << " ";
    os << d_PE_width <<" ";
    os << d_Nped << " ";
    os << d_N1PE << " ";
    os << d_N2PE << " ";
    os << d_N3PE << " ";
    os << d_chisqr << " ";
    if(d_status == STAT_PREV) os << d_cal_time;
    os << endl;
  }
}

bool PdbRichADC::read(istream&is) {
  string status;

  is >> status;
  if(!is.good()) return false;

  if(status == "OK") d_status = STAT_OK;
  else if(status == "BAD")  d_status = STAT_BAD;
  else if(status == "PREV") d_status = STAT_PREV;
  else if(status == "SM")   d_status = STAT_SM;  //small yield histogram
  else if(status == "EM")   d_status = STAT_EM;  //empty histogram
  else if(status == "NH")   d_status = STAT_NH;  //no histogram
  else {
    cout << "error. Unknown status: " << status << endl;
    d_status = STAT_BAD;
  }

  if(d_status != STAT_BAD) {
    is >> d_PMTid
       >> d_pedestal
       >> d_ped_width
       >> d_PE_pos
       >> d_PE_width
       >> d_Nped
       >> d_N1PE
       >> d_N2PE
       >> d_N3PE
       >> d_chisqr;
    if(d_status == STAT_PREV) is >> d_cal_time;
  }
  is.ignore(10000,'\n');  // skip to the end of line.

  if(is.good()) return true;
  else return false;
}

void PdbRichADC::setValues(int PMTid,
			   float ped, float pedw, float PE, float PEw,
			   float Nped, float N1, float N2, float N3,
			   float chisqr) {
  d_PMTid     = PMTid;
  d_pedestal  = ped;
  d_ped_width = pedw;
  d_PE_pos    = PE;
  d_PE_width  = PEw;
  d_Nped      = Nped;
  d_N1PE      = N1;
  d_N2PE      = N2;
  d_N3PE      = N3;
  d_chisqr    = chisqr;
}

void
PdbRichADC::getValues(int &PMTid,
		      float &ped, float &pedw, float &PE, float &PEw,
		      float &Nped, float &N1, float &N2, float &N3,
		      float &chisqr)
{
  PMTid = d_PMTid; 
  ped   = d_pedestal;
  pedw  = d_ped_width;
  PE    = d_PE_pos;
  PEw   = d_PE_width;
  Nped  = d_Nped;
  N1    = d_N1PE;
  N2    = d_N2PE;
  N3    = d_N3PE;
  chisqr = d_chisqr;
}


