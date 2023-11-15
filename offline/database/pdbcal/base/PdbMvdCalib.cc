// Sangsu Ryu Nov/13/2000

#include "PdbMvdCalib.hh"
#include <iostream>
#include <iomanip>

using namespace std;

PdbMvdCalib::PdbMvdCalib()
{
  for (int i = 0; i < size; i++)
    {
      pedestal[i] = -1;
      pedestalwidth[i] = -1;
      mip[i] = -1;
      cspare0[i] = -1;
      cspare1[i] = -1;
      cspare2[i] = -1;
    }
}

PdbMvdCalib::~PdbMvdCalib()
{}

void
PdbMvdCalib::print() const
{
  PdbMvdMap::print();
  for ( int i = 0; i < size; i++)
    {
      print(i);
      std::cout << std::endl;
    }
}

void
PdbMvdCalib::print(std::ostream& os) const
{
  PdbMvdMap::print(os);
  os << std::endl;
  for ( int i = 0; i < size; i++)
    {
      print(i, os);
      os << std::endl;
    }
}

PdbMvdCalib & PdbMvdCalib::operator=(const PdbMvdCalib& p){
  this->PdbMvdMap::operator=(p);
  for(int i = 0; i < 256; i++){
    pedestal[i] = p.pedestal[i];
    pedestalwidth[i] = p.pedestalwidth[i];
    mip[i] = p.mip[i];
    cspare0[i] = p.cspare0[i];
    cspare1[i] = p.cspare1[i];
    cspare2[i] = p.cspare2[i];
  }
  return *this;
}
//actual printout goes here
void
PdbMvdCalib::print(int index, std::ostream& os) const
{
  // Add a space because sometimes a combination of
  // a floating point number which happens to
  // be an integer and gets concatentated with
  // the following number with it happens to use
  // all 10 digits of the field (typically meaning it is very
  // small or very large ) and the two are
  // interpreted as one floating point number. This
  // is not a theoretical problem -- it happened
  // and the "read" method did not correctly read
  // the resulting file.

  os << setw(10) << index << " ";
  os << setw(10) << pedestal[index] << " ";
  os << setw(10) << pedestalwidth[index] << " ";
  os << setw(10) << mip[index] << " ";
  os << setw(10) << cspare0[index] << " ";
  os << setw(10) << cspare1[index] << " ";
  os << setw(10) << cspare2[index];
}

void
PdbMvdCalib::Read(std::istream& is)
{
  PdbMvdMap::Read(is);
  for (int i = 0; i < PdbMvdCalib::size; i++)
    {
      Read(i, is);
    }
}

void
PdbMvdCalib::Read(int index, std::istream& is)
{
  // This gets called once for each channel in each packet.
  // index = channel number expected
  int dummy;
  is >> dummy;
  if (dummy == index)
    {
      is >> pedestal[index] >> pedestalwidth[index] >> mip[index] >> cspare0[index] >> cspare1[index] >> cspare2[index];
    }
  else
    {
      // If this happens, it means that the first number in the database entry
      // for this channel (=dummy) does not agree with the next channel
      // which should apear in the file (=index). For the MVD, these
      // numbers should be from 0 to 255. If this happens, it usually means
      // something is wrong in the format of an ascii database file which is being
      // read in. Either the number of lines in the file is wrong, or the
      // number of entries in one or more lines is wrong, or the spaces between
      // two numbers have been lost and two numbers get interpreted as one.
      cerr << "PdbMvdCalib::Read error: index mismatch: index=" << dummy
	      << " : usually means format problem in ascii database file" << std::endl;
    }
}

std::ostream&
operator<<(std::ostream& os, const PdbMvdCalib& pdbmvdcalib)
{
  pdbmvdcalib.print(os);
  return os;
}

std::istream&
operator>>(std::istream& is, PdbMvdCalib& pdbmvdcalib)
{
  pdbmvdcalib.Read(is);
  return is;
}

float
PdbMvdCalib::get_pedestal_soft(int i) const
{
  int index_hard;
  Soft2Hard(i, &index_hard);
  return get_pedestal(index_hard);
}

float
PdbMvdCalib::get_pedestalwidth_soft(int i) const
{
  int index_hard;
  Soft2Hard(i, &index_hard);
  return get_pedestalwidth(index_hard);
}

float
PdbMvdCalib::get_mip_soft(int i) const
{
  int index_hard;
  Soft2Hard(i, &index_hard);
  return get_mip(index_hard);
}

float
PdbMvdCalib::get_cspare0_soft(int i) const
{
  int index_hard;
  Soft2Hard(i, &index_hard);
  return get_cspare0(index_hard);
}

float
PdbMvdCalib::get_cspare1_soft(int i) const
{
  int index_hard;
  Soft2Hard(i, &index_hard);
  return get_cspare1(index_hard);
}

float
PdbMvdCalib::get_cspare2_soft(int i) const
{
  int index_hard;
  Soft2Hard(i, &index_hard);
  return get_cspare2(index_hard);
}

float
PdbMvdCalib::get_pedestal_soft(int column, int row) const
{
  int index_hard;
  Soft2Hard(column, row, &index_hard);
  return get_pedestal(index_hard);
}

float
PdbMvdCalib::get_pedestalwidth_soft(int column, int row) const

{
  int index_hard;
  Soft2Hard(column, row, &index_hard);
  return get_pedestalwidth(index_hard);
}

float
PdbMvdCalib::get_mip_soft(int column, int row) const

{
  int index_hard;
  Soft2Hard(column, row, &index_hard);
  return get_mip(index_hard);
}

float
PdbMvdCalib::get_cspare0_soft(int column, int row) const

{
  int index_hard;
  Soft2Hard(column, row, &index_hard);
  return get_cspare0(index_hard);
}

float
PdbMvdCalib::get_cspare1_soft(int column, int row) const
{
  int index_hard;
  Soft2Hard(column, row, &index_hard);
  return get_cspare1(index_hard);
}

float
PdbMvdCalib::get_cspare2_soft(int column, int row) const
{
  int index_hard;
  Soft2Hard(column, row, &index_hard);
  return get_cspare2(index_hard);
}
