#include <TecClusterV1.hh>
#include <TecCalibrationObject.hh>
#include <TecGeometryObject.hh>
#include "PHNodeIterator.h"
#include <PHGeometry.h>
#include <getClass.h>
#include <getClass.h>
#include <half.h>
#include <iostream>

ClassImp(TecClusterV1)

using namespace std;

enum SIDE{ S, N};

typedef union FLOAT_32 FLOAT_32;
union FLOAT_32
{
    float    f32;
    uint32_t u32;
};

TecClusterV1::TecClusterV1()
{
  index = -1;
  wire = -1;
  avgtime = 0;
  ntimebins = 0;
  charge = 0;
  xyz[0] = -9999.9;
  xyz[1] = -9999.9;
  xyz[2] = -9999.9;
  xyz_calculated = false;
}

TecClusterV1::TecClusterV1(const TecCluster &source)
{
  index     = source.get_index();
  wire      = source.get_wire();
  avgtime   = source.get_avgtime();
  ntimebins = source.get_ntimebins();
  float charge_  = source.get_charge();
  set_charge(charge_);
  xyz[0] = -9999.9;
  xyz[1] = -9999.9;
  xyz[2] = -9999.9;
  xyz_calculated = false;
}

TecClusterV1::TecClusterV1(short side_, short sector_, short plane_,
	     short wire_, short avgtime_, short ntimebins_,
	     float charge_)
{
  set_index(side_ + plane_*2 + sector_*2*6);
  set_wire(wire_);
  set_avgtime(avgtime_);
  set_ntimebins(ntimebins_);
  set_charge(charge_);
  xyz[0] = -9999.9;
  xyz[1] = -9999.9;
  xyz[2] = -9999.9;
  xyz_calculated = false; 
  return;
}

TecClusterV1::TecClusterV1(short index_,
	     short wire_, short avgtime_, short ntimebins_,
	     float charge_)
{
  set_index(index_);
  set_wire(wire_);
  set_avgtime(avgtime_);
  set_ntimebins(ntimebins_);
  set_charge(charge_); 
  xyz[0] = -9999.9;
  xyz[1] = -9999.9;
  xyz[2] = -9999.9;
  xyz_calculated = false;
  return;
}

void TecClusterV1::identify(ostream& out) const {
  out << "I am a TecClusterV1 object." << endl;
}

void
TecClusterV1::set_charge(const float a)
{
  FLOAT_32 f;
  f.f32 = a;
  charge = half_from_float(f.u32);
  return;
}

float
TecClusterV1::get_charge() const
{
  FLOAT_32 f;
  f.u32 = half_to_float(charge);
  return f.f32;
}

float
TecClusterV1::get_xyz_global(TecCalibrationObject* TCO, TecGeometryObject* TGO, unsigned short i)
{
  if (i>2)
    {
      std::cout << "TecClusterV1::get_xyz_global: parameter i=" << i << " invalid" << std::endl;
      return -9999.0;
    }
  
  if (xyz_calculated)
    return xyz[i];

  if (!TGO)
    {
      std::cout << "TecClusterV1::get_xyz_global: no TecGeometryObject." << std::endl;
      return -9999.0;
    }
  if (!TCO)
    {
      std::cout << "TecClusterV1::get_xyz_global: no TecCalibrationObject. Using standard first and last timebin as reference." << std::endl;
    }
  calc_position(TGO,TCO);
  return xyz[i];
}

float
TecClusterV1::get_xyz_global(PHCompositeNode *topNode, unsigned short i)
{
  if (i>2)
    {
      std::cout << "TecClusterV1::get_xyz_global: parameter i=" << i << " invalid" << std::endl;
      return -9999.0;
    }
  if (xyz_calculated)
    return xyz[i];
  TecGeometryObject * TGO = findNode::getClass<TecGeometryObject>(topNode, "TecGeometry");
  if (!TGO)
    {
      std::cout << "TecClusterV1::get_xyz_global: no TecGeometryObject." << std::endl;
      return -9999.0;
    }
  TecCalibrationObject * TCO = findNode::getClass<TecCalibrationObject>(topNode, "TecCalibration");
  if (!TCO)
    {
      std::cout << "TecClusterV1::get_xyz_global: no TecCalibrationObject. Using standard first and last timebin as reference." << std::endl;
    }
  calc_position(TGO,TCO);
  return xyz[i];
}

void
TecClusterV1::calc_position(TecGeometryObject *TGO, TecCalibrationObject *TCO)
{
      float xwire = TGO-> getGlobalX(index, wire);
      float ywire = TGO-> getGlobalY(index, wire);
      float difference1 = 75 - 10;
      if (TCO)
	difference1 = TCO->getLastTimeBin(index) - TCO->getFirstTimeBin(index);
      float difference2 = (float)avgtime - 10.0;
      if (TCO)
	difference2 = (float)avgtime - TCO->getFirstTimeBin(index);
      float relativebin = difference2 / difference1;
      PHPanel tecpanel = TGO->getTecPanel (index);
      PHVector normal = tecpanel.getNormal ();
      float x_n = normal.getX ();
      float y_n = normal.getY ();
      float MidPhi = M_PI + atan (y_n / x_n);
      if (x_n == 0)
	MidPhi = M_PI;
      float SinAlpha = -sin(MidPhi);
      xyz[0] = xwire + (TecGeometryObject::get_ywidth() - 0.3)*sqrt(1.0-SinAlpha*SinAlpha)*relativebin;
      xyz[1] = ywire + (TecGeometryObject::get_ywidth() - 0.3)*SinAlpha*relativebin;
      xyz[2] = -1;
      if (get_side()==1)
	xyz[2] = 1;
      xyz_calculated = true;
}



