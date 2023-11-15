//  Implementation of class PdbRichPar
//  Author: akiba

#include <iostream>
#include <string>

#include "PdbRichPar.hh"

using namespace std;

PdbRichPar::PdbRichPar():
  d_status(STAT_BAD), 
  d_PMTid(0),
  d_param(0.),
  d_cal_time(0,0,0,0,0,0)
  {}

PdbRichPar::PdbRichPar(int PMTid, float param):
  d_status(STAT_OK),
  d_PMTid(PMTid),
  d_param(param),
  d_cal_time(0,0,0,0,0,0)
 {}

PdbRichPar::~PdbRichPar() {}

void PdbRichPar::print() const {
  write(cout);
}

void PdbRichPar::write(ostream& os) const
{
  std::string status;

  if(d_status == STAT_OK) status = "OK ";
  else if(d_status == STAT_BAD) status = "BAD ";
  else if(d_status == STAT_PREV) status = "PREV ";
  else status = "BAD ";

  os << status;
  if(d_status != STAT_BAD) {
    os << d_PMTid << " ";
    os << d_param << " ";
    if(d_status == STAT_PREV) os << d_cal_time;
    os << endl;
  }
}

bool PdbRichPar::read(istream &is) {
  string status;
  is >> status;
  if(!is.good()) return false;

  if(status == "OK") d_status = STAT_OK;
  else if(status == "BAD") d_status = STAT_BAD;
  else if(status == "PREV") d_status = STAT_PREV;
  else {
    cout << "error. Unknown status: " << status << endl;
    d_status = STAT_BAD;
  }

  if(d_status == STAT_OK) {
    is >> d_PMTid
       >> d_param;
    if(d_status == STAT_PREV) is >> d_cal_time;
  }
  is.ignore(10000,'\n');  // skip to the end of line.

  if(is.good()) return true;
  else return false;
}

void PdbRichPar::setValues(int PMTid, float param) {
  d_PMTid    = PMTid;
  d_param    = param;
}

void PdbRichPar::getValues(int &PMTid, float &param) {
  PMTid    = d_PMTid;
  param    = d_param;
}
