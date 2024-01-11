#include "MpcExEventHeaderv1.h"

#include <vector>
#include <bitset>

using namespace std;

MpcExEventHeaderv1::MpcExEventHeaderv1()
{}

MpcExEventHeaderv1::~MpcExEventHeaderv1()
{
  Reset();
  return;
}

unsigned int
MpcExEventHeaderv1::setStatephase(const vector<unsigned short> &vec)
{
  statephases = vec;
  return statephases.size();
}
unsigned int
MpcExEventHeaderv1::getStatephase(const unsigned int index) const
{
  if (index < statephases.size())
    {
      return statephases[index];
    }
  cout << "index: " << index << " outside range 0 - " << statephases.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getStatephaseArm(const unsigned int index) const
{
  if (index < statephases.size())
    {
      return ( statephases[index] & 0x8000) >> 15;
    }
  cout << "index: " << index << " outside range 0 - " << statephases.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getStatephasePkt(const unsigned int index) const
{
  if (index < statephases.size())
    {
      return (statephases[index] & 0x7000) >> 12;
    }
  cout << "index: " << index << " outside range 0 - " << statephases.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getStatephaseValue(const unsigned int index) const
{
  if (index < statephases.size())
    {
      return (statephases[index] & 0x07FF);
    }
  cout << "index: " << index << " outside range 0 - " << statephases.size() << endl;
  return 0x0;
}

unsigned int
MpcExEventHeaderv1::setCellIDs(const vector<unsigned short> &vec)
{
  cellid = vec;
  return cellid.size();
}
unsigned int
MpcExEventHeaderv1::getCellIDs(const unsigned int index) const
{
  if (index < cellid.size())
    {
      return cellid[index];
    }
  cout << "index: " << index << " outside range 0 - " << cellid.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getCellIDsArm(const unsigned int index) const
{
  if (index < cellid.size())
    {
      return (cellid[index] & 0x8000) >> 15;
    }
  cout << "index: " << index << " outside range 0 - " << cellid.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getCellIDsPkt(const unsigned int index) const
{
  if (index < cellid.size())
    {
      return (cellid[index] & 0x7000) >> 12;
    }
  cout << "index: " << index << " outside range 0 - " << cellid.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getCellIDsSVXID(const unsigned int index) const
{
  if (index < cellid.size())
    {
      return (cellid[index] & 0x0FC0) >> 6;
    }
  cout << "index: " << index << " outside range 0 - " << cellid.size() << endl;
  return 0x0;
}
unsigned int
MpcExEventHeaderv1::getCellIDsValue(const unsigned int index) const
{
  if (index < cellid.size())
    {
      return (cellid[index] & 0x003F);
    }
  cout << "index: " << index << " outside range 0 - " << cellid.size() << endl;
  return 0x0;
}
