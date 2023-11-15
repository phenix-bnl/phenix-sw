#include <iostream>
#include <string.h>
#include <PdbSpinGL1p.hh>

PdbSpinGL1p::PdbSpinGL1p()
{
  _runnum = 0;
  _stat_offset_beamx = false;
  _offset_beamx = 0;
}
void PdbSpinGL1p::set(const int runnum,const int offset,const bool stat)
{
  _runnum = runnum;
  _offset_beamx = offset;
  _stat_offset_beamx = stat;
}

PdbSpinGL1p::PdbSpinGL1p(const  PdbSpinGL1p&p){
  _runnum = p._runnum;
  _offset_beamx = p._offset_beamx;
  _stat_offset_beamx = p._stat_offset_beamx;
}

PdbSpinGL1p& PdbSpinGL1p::operator=(const  PdbSpinGL1p&p){
  _runnum = p._runnum;
  _offset_beamx = p._offset_beamx;
  _stat_offset_beamx = p._stat_offset_beamx;
  return *this;
}

void PdbSpinGL1p::print() const
{
  std::cout<<" PdbSpinGL1p:: run number = "<<_runnum<<"  offset = "<<_offset_beamx<<" status = ";
  if( _stat_offset_beamx ){
    std::cout<<" good."<<std::endl;
  } else {
    std::cout<<" BAD !!!!!!!!. "<<std::endl;
  }
  return;
}
//


