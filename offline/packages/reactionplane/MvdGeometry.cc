// Author: 	Jeong Hwan Park
// Revision:	Sangsu Ryu	Mar/17/2001
//		Makes "Radius(short shell)const" return 0 when argument "shell"
//		is invalid
//		Makes "Z(short end,short ext)const" return 0
//		when argument "ext" is not given

#include "gsl/gsl_math.h"
#include "MvdGeometry.hh"

using namespace std;

MvcGeometry *MvcGeometry::sInstance;

MvbGeometry::MvbGeometry(MvdParameter Par, MvdGeoParameter Geo) :
  par(Par), geo(Geo)
{}

void MvbGeometry :: Init()
{

  float zoff, ztmp, sleng;
  short i, j;

  //  setup Geometry of Barrels
  sleng = geo.nstrip * geo.pitch / 2.;
  zoff = geo.dvisl[2];
  for (i = 0;i < geo.nwperp;i++)
    {
      zpanel.push_back((2*i + 1)*geo.dvwr1[2] - zoff);

      ztmp = zpanel[i] - sleng + geo.pitch / 2.;
      for (j = 0;j < par.nstrip;j++)
        zstrip.push_back(ztmp + j*geo.pitch);
    }

  radius.push_back(geo.vislr + geo.dvisl[0]);
  radius.push_back(geo.voslr + geo.dvisl[0]);

  float DELPHI = 2 * M_PI / par.nphdim;

  for ( i = 0;i < par.nphdim;++i)
    phi.push_back(i*DELPHI);

}

void MvbGeometry::Show()
{

  cout << endl << "radius" << endl;
  for (unsigned short i = 0;i < radius.size();i++)
    cout << "  [" << i << "]=" << radius[i];

  cout << endl << "phi" << endl;
  for (unsigned short i = 0;i < phi.size();i++)
    cout << "  [" << i << "]=" << phi[i];

  cout << endl << "zpanel" << endl;
  for (unsigned short i = 0;i < zpanel.size();i++)
    cout << "  [" << i << "]=" << zpanel[i];

  cout << endl << "zstrip" << endl;
  for (unsigned short i = 0;i < zstrip.size();i++)
    cout << "  [" << i << "]=" << zstrip[i];
}


void MvbGeometry :: Update()
{}

float MvbGeometry :: Radius(short shell)const
{
  if ( shell == 0)
    return radius[0];
  else if (shell == 1)
    return radius[1];
  else
    {
      cerr << "Error: invalid shell" << shell << endl;
      return 0.;
    }
}

float MvbGeometry :: Phi(short row, short ext)
{
  if ( ext == -1)
    return phi[row];
  else
    return phi[row];
}

float MvbGeometry :: Z(short panel, short strip)const
{

  if ( strip == -1)
    return zpanel[panel];
  else
    return zstrip[panel*par.nstrip + strip];   //geo.nstrip
}

MvcGeometry :: MvcGeometry (MvdParameter Par, MvdGeoParameter Geo) :
  par(Par), geo(Geo)
{}

void MvcGeometry :: Init()
{
  int i, j;

  rcuts.push_back(5.25);
  rcuts.push_back(5.45);
  rcuts.push_back(5.65);
  rcuts.push_back(5.85);
  rcuts.push_back(6.10);
  rcuts.push_back(6.35);
  rcuts.push_back(6.60);
  rcuts.push_back(6.90);
  rcuts.push_back(7.20);
  rcuts.push_back(7.50);
  rcuts.push_back(7.80);
  rcuts.push_back(8.10);
  rcuts.push_back(8.40);
  rcuts.push_back(8.75);
  rcuts.push_back(9.10);
  rcuts.push_back(9.45);
  rcuts.push_back(9.80);
  rcuts.push_back(10.20);
  rcuts.push_back(10.60);
  rcuts.push_back(11.00);
  rcuts.push_back(11.45);
  rcuts.push_back(11.90);

  for (i = 0;i < par.nvpad_rbins;i++)
    rvals.push_back((rcuts[i] + rcuts[i + 1]) / 2);
  for (i = 0; i < par.nvpad_segs;i++)
    phicent.push_back(par.vpad_phi1 + i*par.vpad_delphi);

  for (i = 0;i < par.nvpad_segs;i++)
    for (j = 0; j < par.nvpad_segs2;j++)
      phivals.push_back(phicent[i] + (2*j + 1 - par.nvpad_segs2)*par.vpad_delphi2 / 2);

  zend.push_back(geo.ver_z_center - geo.z_ver_pad); /* 0: -z(south) pad */
  zend.push_back(geo.ver_z_center + geo.z_ver_pad); /* 1: +z(north) pad */

}

void MvcGeometry::Show()
{

  for (unsigned short i = 0;i < rcuts.size();i++)
    cout << endl << "rcuts[" << i << "]=" << rcuts[i];
  for (unsigned short i = 0;i < rvals.size();i++)
    cout << endl << "rvals[" << i << "]=" << rvals[i];
  for (unsigned short i = 0;i < phicent.size();i++)
    cout << endl << "phicent[" << i << "]=" << phicent[i];
  for (unsigned short i = 0;i < phivals.size();i++)
    cout << endl << "phivals[" << i << "]=" << phivals[i];
  for (unsigned short i = 0;i < zend.size();i++)
    cout << endl << "zend[" << i << "]=" << zend[i];
}

MvcGeometry *MvcGeometry::instance(){
	if(sInstance==NULL){
		MvdParameter par;
		par.Init();
		MvdGeoParameter geo;
		geo.Init();
		sInstance = new MvcGeometry(par, geo);
		sInstance->Init();
	}

	return sInstance;
}

float MvcGeometry :: Radius(short row)const
{
  return rvals[row];
}

float MvcGeometry :: Phi(short wedge, short column)
{
  if ( column == -1)
    return phicent[wedge];
  else
    return phivals[wedge*par.nvpad_segs2 + column];  // par.>nvpad_segs2
}

float MvcGeometry :: Z(short end, short ext)const
{
  if ( ext == -1 )
    return zend[end];
  else
    return 0.;
}

float MvcGeometry ::Rcuts(int i)const
  {
    return rcuts[i];
  }

float MvcGeometry ::PhiCent(int i)const
  {
    return phicent[i];
  }

float MvcGeometry ::Phivals(int i)const
  {
    return phivals[i];
  }


