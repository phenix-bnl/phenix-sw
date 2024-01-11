#include "DchHitLineOutv2.hh"

#include <cstdlib>
#include <iostream> 

ClassImp(DchHitLineOutv2);

using namespace std;

static float wx[2][2][40][80], wy[2][2][40][80];
static int _initialized = 0;

static void initialize()
{
  if (_initialized)
    return ;
  cout << "DC hit offset look up table Initalized" << endl;
  _initialized = 1;

  int side, plane, cell, i;
  float r[] = {
    204.55, 205.15, 205.75, 206.35, 206.95, 207.55, 208.15, 208.75, 209.35, 209.95,
    210.55, 211.15, 214.85, 215.45, 216.05, 216.65, 220.35, 220.95, 221.55, 222.15,
    225.85, 226.45, 227.05, 227.65, 228.25, 228.85, 229.45, 230.05, 230.65, 231.25,
    231.85, 232.45, 236.15, 236.75, 237.35, 237.95, 241.65, 242.25, 242.85, 243.45
  };
  float phiw[80], phie[80];
  for (i = 0;i < 80;i++)
    {
      phiw[i] = (i * 1.125 - 33.75 + 0.50625) / 57.295779513;
      phie[79 - i] = (i * 1.125 + 123.75 + 0.50625) / 57.295779513;
    }
  for (side = 0;side < 2;side++)
    {
      for (plane = 0;plane < 40;plane++)
        {
          for (cell = 0;cell < 80;cell++)
            {
              wx[0][side][plane][cell] = r[plane] * cos(phie[cell]);
              wy[0][side][plane][cell] = r[plane] * sin(phie[cell]);
              wx[1][side][plane][cell] = r[plane] * cos(phiw[cell]);
              wy[1][side][plane][cell] = r[plane] * sin(phiw[cell]);
            }
        }
    }
}

void
DchHitLineOutv2::Init()
{
  initialize();
  id = MAXUNSIGNED;
  bitindex = 0;
  width = -999;
  time1 = -999;
  xyz_x = xyz_y = 0;
  xyz_z = 0;
}
DchHitLineOutv2::DchHitLineOutv2()
{
  Init();
}

DchHitLineOutv2::DchHitLineOutv2(DchHitLineOut* hit)
{
  Init();
  int arm, side, plane, cell, idmirror;
  arm = hit->getArm();
  side = hit->getSide();
  plane = hit->getPlane();
  cell = hit->getCell();
  idmirror = hit->getIdmirror();

  setArm(arm);
  setSide(side);
  setPlane(plane);
  setCell(cell);
  setIdmirror(idmirror);

  int hitid = hit->getId();
  if (hitid < 0)
    {
      id = MAXUNSIGNED;
    }
  else
    {
      id = hitid;
    }
  setWidth(hit->getWidth());
  time1 = hit->getTime1();
  xyz_x = float2short(hit->getX() - wx[arm][side][plane][cell]);
  xyz_y = float2short(hit->getY() - wy[arm][side][plane][cell]);
  xyz_z = float2short(hit->getZ());
}

int
DchHitLineOutv2::getId()
{
  if (id == MAXUNSIGNED)
    return -1;
  return id;
}

int
DchHitLineOutv2::getIdmirror()
{
  int ret = (bitindex % IDMIRRORMASK) / ARMMASK;
  return ret - 1;
}

short
DchHitLineOutv2::getArm()
{
  return (bitindex % ARMMASK) / SIDEMASK;
}

short
DchHitLineOutv2::getSide()
{
  return (bitindex % SIDEMASK) / PLANEMASK;
}

short
DchHitLineOutv2::getPlane()
{
  return (bitindex % PLANEMASK) / CELLMASK;
}

short
DchHitLineOutv2::getCell()
{
  return bitindex % CELLMASK;
}

void
DchHitLineOutv2::setId(int val)
{
  if (val < 0)
    id = MAXUNSIGNED;
  else
    id = val;
}

void
DchHitLineOutv2::setIdmirror(int val)
{
  int type = 1;
  if (val < 0)
    type = 0;
  else if (val > 0)
    type = 2;
  val = type;
  bitindex += (val - (bitindex % IDMIRRORMASK) / ARMMASK) * ARMMASK;
}

void
DchHitLineOutv2::setArm(short val)
{
  if (val)
    val = 1;
  bitindex += (val - (bitindex % ARMMASK) / SIDEMASK) * SIDEMASK;
}

void
DchHitLineOutv2::setSide(short val)
{
  if (val)
    val = 1;
  bitindex += (val - (bitindex % SIDEMASK) / PLANEMASK) * PLANEMASK;
}

void
DchHitLineOutv2::setPlane(short val)
{
  if (val < 0 || val > 39)
    {
      cout << "Invalid plane number" << endl;
      return ;
    }
  bitindex += (val - (bitindex % PLANEMASK) / CELLMASK) * CELLMASK ;
}

void
DchHitLineOutv2::setCell(short val)
{
  if (val < 0 || val > 79)
    {
      cout << "Invalid cell number" << endl;
      return ;
    }
  bitindex += val - (bitindex % CELLMASK);
}

PHPoint
DchHitLineOutv2::getXYZ()
{
  PHPoint val;
  float x, y, z;

  if (abs(xyz_x) == MAXDIGITIZE)
    {
      x = -1000;
    }
  else
    {
      x = short2float(xyz_x) + wx[getArm()][getSide()][getPlane()][getCell()];
    }
  if (abs(xyz_y) == MAXDIGITIZE)
    {
      y = -1000;
    }
  else
    {
      y = short2float(xyz_y) + wy[getArm()][getSide()][getPlane()][getCell()];
    }
  if (abs(xyz_z) == MAXDIGITIZE)
    {
      z = -1000;
    }
  else
    {
      z = short2float(xyz_z);
    }

  val.setX(x);
  val.setY(y);
  val.setZ(z);
  return val;
}

float
DchHitLineOutv2::getX()
{
  if (abs(xyz_x) == MAXDIGITIZE)
    {
      return -1000;
    }
  return short2float(xyz_x) + wx[getArm()][getSide()][getPlane()][getCell()];
}

float
DchHitLineOutv2::getY()
{
  if (abs(xyz_y) == MAXDIGITIZE)
    {
      return -1000;
    }
  return short2float(xyz_y) + wy[getArm()][getSide()][getPlane()][getCell()];
}

float
DchHitLineOutv2::getZ()
{
  if (abs(xyz_z) == MAXDIGITIZE)
    {
      return -1000;
    }
  return short2float(xyz_z);
}

void
DchHitLineOutv2::setXYZ(PHPoint val)
{
  xyz_x = float2short(val.getX() - wx[getArm()][getSide()][getPlane()][getCell()]);
  xyz_y = float2short(val.getY() - wy[getArm()][getSide()][getPlane()][getCell()]);
  xyz_z = float2short(val.getZ());
  return ;
}

short
DchHitLineOutv2::float2short(float val)
{
  short ret;
  if (fabs(val) > MAXDIGITIZE / 250.)
    {
      ret = MAXDIGITIZE;
      if (val < 0)
        {
          ret = -ret;
        }
      return ret;
    }

  float fol = val * 250;
  ret = static_cast <short> (fol);
  if (fol - ret < -0.5)
    {
      ret--;
    }
  else if (fol - ret > 0.5)
    {
      ret++;
    }
  return ret;
}

float
DchHitLineOutv2::short2float(short val)
{
  float ret = static_cast <float>(val);
  ret /= 250.;
  return ret;
}

void
DchHitLineOutv2::print()
{
  cout << id << " " << getArm() << " " << getSide() << " " << getPlane() << " " <<
  getCell() << " " << getIdmirror() << " " << width << " " << time1 << endl;
}

