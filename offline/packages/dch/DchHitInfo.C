#include "DchHitInfo.hh"

DchHitInfo::DchHitInfo()
{
  raw1 = 0;
  raw2 = 0;
  mirror = 0;
  trackId = -1;
  used = 0;
  idraw1 = -1;     // id of leading edge raw data hit
  idraw2 = -1;     // id of trailing edge raw data hit
  idmirror = -1;   // id of hit if ambigous copy exists
  blind = -1;
  residual = 10000;
  out = -1;
  trackId = -1;

}
DchHitInfo::~DchHitInfo()
{
}

PHLine DchHitInfo::getLine()
{
  PHLine line(basepoint,direction);
  return line;
}
