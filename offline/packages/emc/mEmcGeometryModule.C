
// EMC Geometry class.
// Alexander Bazilevsky Sep-00
// modifications by Julia Velkovska Jan 2002
// -read geometry from DB 
// -allow transformations
// -added PHPanels as data members for cgl to use 
// modificationes by Wenqing Fan June 2019 (wenqing.fan@stonybrook.edu)   
// new geometry implemented for both simulation and real data (which matched better the real EMCal geometry)    
// -kPISA geometry hard coded inside the module in becasue loading external files via TOAD does not work as well on rcas (when running pisaToDST) comparing to taxi

#include <iostream>
#include <fstream>
#include <cstdio>
#include <ctype.h>
#include <cstring>
#include <cmath>
#include <cstdlib>
#include <cassert>

#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>

#include <gsl/gsl_math.h>
#include <mEmcGeometryModule.h>
#include <emcGeometry.h>
#include <emcDataManager.h>
#include <KinPISAHit.h>
#include <emcDBMS.h>
//#include <EmcIndexer.h>
#include <emcNodeHelper.h>

#include <RunToTimePg.hh>
#include "recoConsts.h"
#include "TOAD.h"

#define ABSURD -999999.

ClassImp(mEmcGeometryModule)

using namespace PHGeometry;

mEmcGeometryModule::mEmcGeometryModule(mEmcGeometryModule::ERealm type): SubsysReco("mEmcGeometryModule")
{

  switch(type){
  case kReal: BuildGeometry(); break;
  case kPISA: BuildGeometryPISA(); break;
  default: assert( (type == kReal) || (type == kPISA) ); break;
  }

  BuildPanels();
}

void 
mEmcGeometryModule::BuildGeometryPISA()
{
  int geo_version = 0;

  // for compatibility reasons introducing rc flag rather than changing the interface
  recoConsts* rc = recoConsts::instance();
  assert(rc!=NULL);
  if ( rc->FlagExist("EMCGEOFLAG") && rc->get_IntFlag("EMCGEOFLAG") )
  {
    PHMESSAGE("EMCGEOFLAG set: now you are able switch between the new geomerty and old geometry.");
    geo_version = rc->get_IntFlag("EMCGEOFLAG");
  }

  const int nvar = 18;
  //  std::string var_name[nvar] = {"rpos_emc_pbsc","rpos_emc_pbgl","emc_phi_sec1","emc_phi_sec2","emc_phi_sec3","emc_phi_sec4","emc_phi_sec5","emc_phi_sec6","emc_phi_sec7","emc_phi_sec8","tower_xsize_pbsc","tower_ysize_pbsc","tower_xsize_pbgl","tower_ysize_pbgl","emc_westarm_deltay","emc_westarm_deltaz","emc_eastarm_deltay","emc_eastarm_deltaz"};
  float var_value[nvar] = {0};

  switch(geo_version){
  case 0: std::cout<<"Use previous geometry setting -- reference: phnx.par (before 2002)"<<std::endl;
  var_value[0] = 510.0;
  var_value[1] = 543.2;
  var_value[2] = 22.632;
  var_value[3] = 0.0;
  var_value[4] = -22.632;
  var_value[5] = -45.264;
  var_value[6] = -45.264;
  var_value[7] = -22.632;
  var_value[8] = 0.0;
  var_value[9] = 22.0;
  var_value[10] = 5.542;
  var_value[11] = 5.542;
  var_value[12] = 4.1040;
  var_value[13] = 4.1105;
  var_value[14] = 0.0;
  var_value[15] = 0.0;
  var_value[16] = 0.0;
  var_value[17] = 0.0;
  break;
  case 1: std::cout<<"Use new geometry setting -- reference: phnx.par (after 2002)"<<std::endl; 
  var_value[0] = 511.2;
  var_value[1] = 541.2;
  var_value[2] = 22.152;
  var_value[3] = -0.2354;
  var_value[4] = -22.785;
  var_value[5] = -45.392;
  var_value[6] = -45.343;
  var_value[7] = -22.900;
  var_value[8] = 0.0;
  var_value[9] = 22.0;
  var_value[10] = 5.58;
  var_value[11] = 5.58;
  var_value[12] = 4.1040;
  var_value[13] = 4.1040;
  var_value[14] = 0.649;
  var_value[15] = 0.816;
  var_value[16] = 0.511;
  var_value[17] = 0.742;
  break;
  default: assert( geo_version==0 || geo_version ==1 ); break;
  }

  // // print out geometry setting values
  // for (int ivar = 0; ivar < nvar; ++ivar)
  // {
  //   std::cout<<"index "<<ivar<<" variable name "<<var_name[ivar]<<" = "<<var_value[ivar]<<std::endl;
  // }

  float deg = M_PI/180.0;
  // Sector dimensions
  int NxSc = 72;
  int NySc = 36;
  int NxGl = 96;
  int NyGl = 48;
  // Tower Size (cm)
  float TowerXSizeSc = var_value[10];
  float TowerYSizeSc = var_value[11];
  float TowerXSizeGl = var_value[12];
  float TowerYSizeGl = var_value[13];

  // Rotation angles (deg)
  float emcRA[8][2] =      
  {                      // SECTOR  ARM(*)   ARM(**)  SECTOR(*)  iS(***)  iS(****)
    {0.0, var_value[2]}, //  W0       1        0         0        0        0
    {0.0, var_value[3]}, //  W1       1        0         1        1        1
    {0.0, var_value[4]}, //  W2       1        0         2        2        2
    {0.0, var_value[5]}, //  W3       1        0         3        3        3
    
    {0.0, var_value[6]}, //  E3       0        1         3        4        5
    {0.0, var_value[7]}, //  E2       0        1         2        5        4
    {0.0, var_value[8]}, //  E1       0        1         1        6        7
    {0.0, var_value[9]}  //  E0       0        1         0        7        6
  };
  // (*) = PHENIX convention
  // (**) = EMC Offline convention (e.g. getters/setters of dEmcCalibTower or dEmcClusterLocal)
  // (***) = EMC mEmcGeometryModule convention (this module)
  // (****) = EMC Online convention

  // X position
  float emcX[8] = {var_value[0], var_value[0], var_value[0], var_value[0], var_value[0], var_value[0], var_value[1], var_value[1]};

  PHVector perm(3, 1, 2);

  // loop over EMCal Sectors
  for (int iS = 0; iS<8; iS++){
    float sign = ((iS>=4)? -1. : 1.);
    nx[iS] = ( (iS<6) ? NxSc : NxGl );
    ny[iS] = ( (iS<6) ? NySc : NyGl );
    tower_xsize[iS] = ( (iS<6) ? TowerXSizeSc : TowerXSizeGl );
    tower_ysize[iS] = ( (iS<6) ? TowerYSizeSc : TowerYSizeGl );
    // Create rotation matrix
    PHMatrix mm( perm, 0., emcRA[iS][1]*deg, (emcRA[iS][0]+90.*sign)*deg );
    emcrm[iS] = mm;
    float z0 = (nx[iS]-1.)/2. * tower_xsize[iS] * sign;
    float y0 = -emcX[iS]*sinf(emcRA[iS][1]*deg) - 
      (ny[iS]-1.)/2. * tower_ysize[iS] * cosf(emcRA[iS][1]*deg);
    float x0 =  emcX[iS]*cosf(emcRA[iS][1]*deg) - 
      (ny[iS]-1.)/2. * tower_ysize[iS] * sinf(emcRA[iS][1]*deg);
    if (iS<4) {
      y0 += var_value[14];
      z0 += var_value[15];
    }
    else if (iS<6) {
      y0 += var_value[16];
      z0 += var_value[17];
    }
    else {}; 
    x0 *= sign;
    PHVector vv0(x0, y0, z0);
    emctr[iS] = vv0;
  }

  for( int is=0; is<MAX_SECTORS; is++ ) {
    PHFrame local;
    PHFrame global = MatrixAndVector2frames(local,emcrm[is],emctr[is]);
    frames2MatrixAndVector(global,local,invemcrm[is],invemctr[is]);
  }

  printf("<I> mEmcGeometryModule: PISA Geometry Initialized\n");
}

void 
mEmcGeometryModule::printSectorNumberingConventions()
{

  const char *name[] = {"W0","W1","W2","W3","E3","E2","E1","E0"};
  const char *type[] = {"PbSc","PbSc","PbSc","PbSc","PbSc","PbSc","PbGl","PbGl"};

  std::cout << "<I> mEmcGeometryModule::printSectorNumberingConventions() : " << std::endl;
  std::cout << "Sector   Type   (arm,sector)[PHENIX]   (arm,sector)[EMC-offline]   " 
	    << "(sector)[EMC-online]   (sector)[EMC-mEmcGeometryModule] " << std::endl;

  short arm,sector;

  for( int is=0; is<MAX_SECTORS; is++ ) 
    {
      emcToPhenix(is,arm,sector);
      std::cout << "  " << name[is] << "      " << type[is] << "  ( " ;
      std::cout << arm << " ,   " << sector << "  ) " ;
      std::cout << "          ( " << 1-arm << " ,   " << sector << "  )" ;
      std::cout << "                    " << emcToEmcOnline(is);
      std::cout << "                      " << is << std::endl;
    }
}

void 
mEmcGeometryModule::BuildGeometry()
{
  readFromDB();

  // loop over EMCal Sectors
  for (int iS = 0; iS<MAX_SECTORS; iS++){
    // create the inverse matrices and vectors after reading from DB 
    // this is just to make sure that the inverse corresponds to the direct,
    // since it is possible to write into DB just one or the other
    PHFrame local;
    PHFrame global= MatrixAndVector2frames(local,emcrm[iS],emctr[iS]);
    frames2MatrixAndVector(global,local,invemcrm[iS],invemctr[iS]);

    std::cout.setf(std::ios::fixed,std::ios::floatfield);
    std::cout.precision(3);
    std::cout<<"vector element: X "<<emctr[iS].getX()<<" Y "<<emctr[iS].getY()<<" Z "<<emctr[iS].getZ()<<std::endl;
    std::cout<<"matrix element: A00 "<<emcrm[iS].getA00()<<" A01 "<<emcrm[iS].getA01()<<" A02 "<<emcrm[iS].getA02()<<std::endl;
    std::cout<<"matrix element: A10 "<<emcrm[iS].getA10()<<" A11 "<<emcrm[iS].getA11()<<" A12 "<<emcrm[iS].getA12()<<std::endl;
    std::cout<<"matrix element: A20 "<<emcrm[iS].getA20()<<" A21 "<<emcrm[iS].getA21()<<" A22 "<<emcrm[iS].getA22()<<std::endl;
  }

}

void mEmcGeometryModule::readFromDB()
{
  // create a data manager
  emcGeometry geom;
  emcDataManager *dm = emcDataManager::GetInstance();

  geom.SetSource(emcDBMS::get());

  PHTimeStamp now;

  // for compatibility reasons introducing rc flag rather than changing the interface
  recoConsts* rc = recoConsts::instance();
  assert(rc!=NULL);
  RunToTime *runTime = RunToTime::instance();
  int runNumber = rc->get_IntFlag("RUNNUMBER");
  PHTimeStamp *runBeginTime = runTime->getBeginTime(runNumber);

  int geo_version = 0;
  if ( rc->FlagExist("EMCGEOFLAG") && rc->get_IntFlag("EMCGEOFLAG") )
  {
    PHMESSAGE("EMCGEOFLAG set: now you are able switch between the new geomerty and old geometry.");
    geo_version = rc->get_IntFlag("EMCGEOFLAG");
  }

  bool ok = false;
  switch (geo_version){
  case 0: std::cout<<"readFromDB called using now"<<std::endl;
          ok = dm->Read(geom,now,1);
          break;
  case 1: std::cout<<"readFromDB called using run number "<<runNumber<<std::endl;
          if (runBeginTime!=0) ok = dm->Read(geom,*runBeginTime,1);
          break;
  default: assert( geo_version==0 || geo_version ==1 ); break;
  }
  delete runBeginTime;

  if(!ok){
    printf("<I> Fail to build REAL Geometry, use PISA Geometry instead\n");
    BuildGeometryPISA();
  }
  else{
    assert(MAX_SECTORS==geom.NumberOfSectors());
    for(int is = 0; is <MAX_SECTORS; is ++){
      emcrm[is] = geom.Sector(is).RotationMatrix();
      emctr[is] = geom.Sector(is).TranslationVector();
      nx[is] = geom.Sector(is).nx();
      ny[is] = geom.Sector(is).ny();
      tower_xsize[is] = geom.Sector(is).Tower_xSize(); 
      tower_ysize[is] = geom.Sector(is).Tower_ySize(); 
    }
    printf("<I> mEmcGeometryModule: REAL Geometry Initialized\n");
  }
}

void 
mEmcGeometryModule::Retract()
{
  float west[]={+41,0,0};
  float east[]={-44,0,0};
  Retract( east, west );
}

void 
mEmcGeometryModule::Retract( float* east, float* west )
{
    int is;
    PHVector vv;

    PHVector veast(east[0], east[1], east[2]);
    PHVector vwest(west[0], west[1], west[2]);
    for( is=0; is<4; is++ ) {
      vv = emctr[is]+vwest;
      emctr[is] = vv;
    }
    for( is=4; is<8; is++ ) {
      vv = emctr[is]+veast;
      emctr[is] = vv;
    }
    printf("<I> mEmcGeometryModule::Retract: retracted geometry east=(%f,%f,%f) west=(%f,%f,%f)\n",
	   east[0],east[1],east[2],west[0],west[1],west[2]);
}

void 
mEmcGeometryModule::print()
{
  printf("\nmEmcGeometryModule::print: Sector geometry\n\n");
  for (int is=0; is<8; is++ ) 
    {
      printf("Sec %d (Nx,Ny) = (%d,%d)\n",is,nx[is],ny[is]);
      printf("  Tower Size = (%f,%f)\n",tower_xsize[is],tower_ysize[is]);
      printf("  Zero Tower Pos = (%f,%f,%f)\n\n",
	     emctr[is].getX(),
	     emctr[is].getY(),
	     emctr[is].getZ());
    }
}

void 
mEmcGeometryModule::printCorners()
{
  float xl, yl, zl;
  float xg, yg, zg;
  printf("\nmEmcGeometryModule::printCorners: Sector corner towers\n\n");
  for (int is=0; is<8; is++ ) 
    {
      printf("Sec %d\n",is);
      xl = 0 - tower_xsize[is]/2.;
      yl = 0 - tower_ysize[is]/2.;
      zl = 0;
      LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
      printf("  (%7.2f,%7.2f,%7.2f)", xg, yg, zg);
      xl = 0 + (nx[is]-0.5)*tower_xsize[is];
      yl = 0 - tower_ysize[is]/2.;
      zl = 0;
      LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
      printf("  (%7.2f,%7.2f,%7.2f)", xg, yg, zg);
      xl = 0 - tower_xsize[is]/2.;
      yl = 0 + (ny[is]-0.5)*tower_ysize[is];
      zl = 0;
      LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
      printf("  (%7.2f,%7.2f,%7.2f)\n\n", xg, yg, zg);
      xl = 0 + (nx[is]-0.5)*tower_xsize[is];
      yl = 0 + (ny[is]-0.5)*tower_ysize[is];
      zl = 0;
      LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
      printf("  (%7.2f,%7.2f,%7.2f)\n\n", xg, yg, zg);

    }
}

/////////// Set Geometry //////////////////////

void 
mEmcGeometryModule::SetSectorDim( int is, int nX, int nY )
{
  if (is<0 || is >= MAX_SECTORS) 
    { 
      printf("!!! mEmcGeometry::SetSectorDim: Wrong sector index: %d\n",is);
      return;
    }
  nx[is] = nX;
  ny[is] = nY;
}

void 
mEmcGeometryModule::SetTowerSize( int is, float xsz, float ysz )
{
  if (is<0 || is >= MAX_SECTORS) 
    { 
      printf("!!! mEmcGeometry::SetTowerSize: Wrong sector index: %d\n",is);
      return;
  }
  tower_xsize[is] = xsz;
  tower_ysize[is] = ysz;
}

void 
mEmcGeometryModule::SetMatrixVector( int is, PHMatrix mx, PHVector vt )
{
  if (is<0 || is >= MAX_SECTORS) 
    { 
      printf("!!! mEmcGeometry::SetMatrixVector: Wrong sector index: %d\n",is);
      return;
    }
  emcrm[is] = mx;
  emctr[is] = vt;
  // we also need to set the inverse transformation and the panel geo
  PHFrame local;
  PHFrame global = MatrixAndVector2frames(local,emcrm[is],emctr[is]);
  frames2MatrixAndVector(global,local,invemcrm[is],invemctr[is]);
  short arm,sector;
  emcToPhenix(is,arm,sector);
  std::cout << " mEmcGeometryModule::SetMatrixVector -> Transformed Panels " << std::endl;
  BuildPanels();
}

void 
mEmcGeometryModule::GetSectorDim( int is, int &nX, int &nY )
{
  if (is < 0 || is >= MAX_SECTORS) 
    { 
      printf("!!! mEmcGeometry::GetSectorDim: Wrong sector index: %d\n",is);
      nX = 0;
      nY = 0;
      return;
    }
  nX = nx[is];
  nY = ny[is];
}

void 
mEmcGeometryModule::GetTowerSize( int is, float &xsz, float &ysz )
{
  if (is < 0 || is >= MAX_SECTORS) 
    { 
    printf("!!! mEmcGeometry::GetTowerSize: Wrong sector index: %d\n",is);
    xsz = 0;
    ysz = 0;
    return;
  }
  xsz = tower_xsize[is];
  ysz = tower_ysize[is];
}

void 
mEmcGeometryModule::GetMatrixVector (int is, PHMatrix &mx, PHVector &vt)
{
  if (is < 0 || is >= MAX_SECTORS) 
    { 
      printf("!!! mEmcGeometry::GetMatrixVector: Wrong sector index: %d\n",is);
      return;
    }
  mx = emcrm[is];
  vt = emctr[is];
}

int
mEmcGeometryModule::GetTowerPosLocal (int is, int ind, 
				      float &x, float &y, float &z)
  // Calculates tower position in Sector frame,
  // is - sector index (0-7)
  // ind - tower index (ix+iy*nx, ix=0,... ; iy=0,... )
{
  x = ABSURD;
  y = ABSURD;
  z = ABSURD;
  if (is<0 || is >= MAX_SECTORS)  
    { 
      printf("mEmcGeometry::GetTowerPosLocal: Wrong sector index: %d\n",is);
      return 0;
    }
  z = 0;
  int ix = ind%nx[is];
  int iy = ind/nx[is];
  if (iy >= ny[is]) 
    {
      printf("mEmcGeometry::GetTowerPosLocal: Wrong tower index for sector %d: %d\n",is,ind);
      //return 0; // fixme: why not return 0 ?
    }
  x = tower_xsize[is] * ix;
  y = tower_ysize[is] * iy;
  return 1;
}

int
mEmcGeometryModule::GetTowerPosLocal (int is, int ix, int iy, 
				      float &x, float &y, float &z)
  // Calculates tower position in Sector frame,
  // is - sector index (0-7)
  // ix - x tower index (column number starting from the bottom right, front view)
  // iy - y tower index (row number starting from the bottom)
{
  x = ABSURD;
  y = ABSURD;
  z = ABSURD;
  if (is<0 || is >= MAX_SECTORS)  
    { 
      printf("mEmcGeometry::GetTowerPosLocal: Wrong sector index: %d\n",is);
      return 0;
    }
  z = 0;
  if (iy >= ny[is]) 
    {
      printf("mEmcGeometry::GetTowerPosLocal: Wrong tower index (%d,%d) for sector %d\n",ix,iy,is);
      //return 0; // fixme: why not return 0 ?
    }
  x = tower_xsize[is] * ix;
  y = tower_ysize[is] * iy;
  return 1;
}

int 
mEmcGeometryModule::GetTowerPosGlobal (int is, int ind, 
				       float &x, float &y, float &z)
  // Calculates tower position in Sector frame,
  // is - sector index (0-7)
  // ind - tower index (ix+iy*nx, ix=0,... ; iy=0,... )
{
  float xl, yl, zl;
  int st = GetTowerPosLocal( is, ind, xl, yl, zl );
  if( !st ) 
    { 
      x = ABSURD;
      y = ABSURD;
      z = ABSURD;
      return 0;
    }

  PHPoint emcHit(xl, yl, zl);
  PHPoint phnxHit  =  transformPoint(emcrm[is], emctr[is], emcHit);
  x =  phnxHit.getX();
  y =  phnxHit.getY();
  z =  phnxHit.getZ();
  return 1;
}

int 
mEmcGeometryModule::GetTowerPosGlobal (int is, int ix, int iy, 
				       float &x, float &y, float &z)
  // Calculates tower position in Sector frame,
  // is - sector index (0-7)
  // ix - x tower index
  // iy - y tower index
{
  float xl, yl, zl;
  int st = GetTowerPosLocal( is, ix, iy, xl, yl, zl );
  if( !st ) 
    { 
      x = ABSURD;
      y = ABSURD;
      z = ABSURD;
      return 0;
    }

  PHPoint emcHit(xl, yl, zl);
  PHPoint phnxHit  =  transformPoint(emcrm[is], emctr[is], emcHit);
  x =  phnxHit.getX();
  y =  phnxHit.getY();
  z =  phnxHit.getZ();
  return 1;
}

double
mEmcGeometryModule::GetSectorCenterInGlobalCoords( int is, int xyz )
{
  assert(is>=0 && is<8);
  assert(xyz>=0 && xyz <3);
 
  short arm, sector;
  emcToPhenix(is,arm,sector);

  PHPanel panel = GetPanel(arm,sector);

  PHPoint center = panel.getCenter();

  double coord = 0;

  switch ( xyz )
    {
    case 0:
      coord = center.getX();
      break;
    case 1:
      coord = center.getY();
      break;
    case 2:
      coord = center.getZ();
      break;
    default:
      std::cerr << PHWHERE << " Improper xyz=" << xyz << " Expecting 0 (x),"
		<< " 1 (y) or 2 (z). Aborting."
		<< std::endl;
      exit(1);
    }

  return coord;
}

void 
mEmcGeometryModule::LocalToGlobal (int is,
				   float xl, float yl, float zl, 
				   float &xg, float &yg, float &zg)
{
  if (is < 0 || is >= MAX_SECTORS ) 
    { 
      printf("!!! mEmcGeometry::LocalToGlobal: Wrong sector index: %d\n",is);
      return;
    }
  PHPoint emcHit(xl, yl, zl);
  PHPoint phnxHit  =  transformPoint(emcrm[is], emctr[is], emcHit);
  xg =  phnxHit.getX();
  yg =  phnxHit.getY();
  zg =  phnxHit.getZ();
}

void 
mEmcGeometryModule::GlobalToLocal (float xg, float yg, float zg, 
				   int is, 
				   float &xl, float &yl, float &zl)
{
  if (is<0 || is >= MAX_SECTORS ) 
    { 
      printf("!!! mEmcGeometry::GlobalToLocal: Wrong sector index: %d\n",is);
      return;
    }
  PHPoint phnxHit(xg, yg, zg);
  PHPoint emcHit  = 
    transformPoint(invemcrm[is], invemctr[is], phnxHit);
  xl =  emcHit.getX();
  yl =  emcHit.getY();
  zl =  emcHit.getZ();
}

/// julia's additions
PHPanel mEmcGeometryModule::GetPanel(short arm, short sector)
{
  PHPanel out;
  out = emcSectors[arm][sector];
  return out;
}  

//_____________________________________________________________________________
void mEmcGeometryModule::emcToPhenix(int is,short &arm,short &sector)
{
  if( is < 4 ) { // WEST!!
    arm = 1;
    sector = is;
  }
  else { // EAST
    arm =0; 
    sector = 7-is;
  }
}

//_____________________________________________________________________________
int
mEmcGeometryModule::emcToEmcOnline(int is)
{
  assert(is>=0 && is<8);

  int iso=-1;

  if ( is < 4 )
    {
      iso=is;
    }
  else
    {
      if ( is==4 ) iso= 5;
      if ( is==5 ) iso= 4;
      if ( is==6 ) iso= 7;
      if ( is==7 ) iso= 6; 
    }
  return iso;
}

//_____________________________________________________________________________
int
mEmcGeometryModule::emcOfflineToEmc(short arm, short sector)
{
  assert(arm>=0 && arm<=1 && sector>=0 && sector<=3);
  return PhenixToEmc(1-arm,sector);
}

//_____________________________________________________________________________
int
mEmcGeometryModule::emcOfflineToEmcOnline(short arm, short sector)
{
  assert(arm>=0 && arm<=1 && sector>=0 && sector<=3);
  return emcToEmcOnline(emcOfflineToEmc(arm,sector));
}

//_____________________________________________________________________________
int
mEmcGeometryModule::emcOnlineToEmc(int is)
{
  return emcToEmcOnline(is);
}

//_____________________________________________________________________________
int 
mEmcGeometryModule::PhenixToEmc(short arm, short sector)
{
  assert(arm>=0 && arm<=1 && sector>=0 && sector<=3);

  int iS=-1;

  if ( arm == 1 ) 
    {
      // WEST ARM
      iS = sector;
    }
  else if ( arm == 0 )
    {
      // EAST ARM
      iS = 7 - sector;
    }
  return iS;
}

void 
mEmcGeometryModule::BuildPanels()
{
  // West arm

  short arm,sec;
  float xl, yl, zl;
  float xg, yg, zg;
  PHPoint p0,p1,p2;

  zl = 0;
  for (int is=0; is<8; is++ ) 
    {
      emcToPhenix(is,arm,sec);
      // West
      if(arm ==1){
	xl = 0 + (nx[is]-0.5)*tower_xsize[is];
	yl = 0 - tower_ysize[is]/2.;
	LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
	p0.setX(xg);
	p0.setY(yg);
	p0.setZ(zg);

	xl = 0 + (nx[is]-0.5)*tower_xsize[is];
	yl = 0 + (ny[is]-0.5)*tower_ysize[is];
	LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
	p1.setX(xg);
	p1.setY(yg);
	p1.setZ(zg);

	xl = 0 - tower_xsize[is]/2.;
	yl = 0 - tower_ysize[is]/2.;
	LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
	p2.setX(xg);
	p2.setY(yg);
	p2.setZ(zg);

	PHPanel temp(p0,p1,p2);
	emcSectors[arm][sec]=temp;
      }
      else {
	// EAST
	xl = 0 - tower_xsize[is]/2.;
	yl = 0 + (ny[is]-0.5)*tower_ysize[is];
	LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
	p0.setX(xg);
	p0.setY(yg);
	p0.setZ(zg);

	xl = 0 - tower_xsize[is]/2.;
	yl = 0 - tower_ysize[is]/2.;
	LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
	p1.setX(xg);
	p1.setY(yg);
	p1.setZ(zg);

	xl = 0 + (nx[is]-0.5)*tower_xsize[is];
	yl = 0 + (ny[is]-0.5)*tower_ysize[is];
	LocalToGlobal( is, xl, yl, zl, xg, yg, zg);
	p2.setX(xg);
	p2.setY(yg);
	p2.setZ(zg);

	PHPanel temp(p0,p1,p2);
	emcSectors[arm][sec]=temp;

      }
      //emcSectors[arm][sec].print();
    }
}

PHBoolean
mEmcGeometryModule::isIntersection(PHLine &line,const short& is){

  PHPoint proj;
  return Intersection(line,is,proj);

}

PHBoolean
mEmcGeometryModule::Intersection(PHLine &line,const short& is,PHPoint &proj){

  // CKB 
  // routine to decide wether a given PHLine
  // intersects with sector is (in EMC-mEmcGeometryModule convention)
  //        / 3   \  4
  // West  |  2    | 5 East
  //       |  1    | 6 
  //        \ 0   /  7

  short arm,sec;

  emcToPhenix(is,arm,sec);

  PHBoolean projectionFound = false;


  projectionFound = intersectionLinePanel(line,emcSectors[arm][sec],proj);
 
  return projectionFound;

}

//_____________________________________________________________________________
//
// If the particle of momentum p and vertex v heads into EMCal
// return the sector number (iS = 0-7). Otherwise return -1.
int
mEmcGeometryModule::HitInEMCalAcceptance(const float *v, const float *p)
{

  // Separate the "Directions" of the track
  // p[0] < 0 is East, p[0] > 0 is West  
  // This is important because a line has no starting point
  // like a track

  // this is a real Point
  PHPoint vtx(v[0],v[1],v[2]);

  // take care that p gives only the direction of the momentum vector
  PHVector hit(p[0],p[1],p[2]);
  PHLine line(vtx,hit);

  int is;

  // isIntersection routine tells you wether a given PHLine
  // intersects with sector is (in EMC-mEmcGeometryModule convention)
  //        / 3   \  4
  // West  |  2    | 5 East
  //       |  1    | 6 
  //        \ 0   /  7

  if(p[0]>0){
    for( is = 0; is<4; is++ ){ // loop over the west arm
      if(isIntersection(line,is)) return is;
    }
  }
  else if(p[0]<0){
    for( is = 4 ; is<8 ; is++ ){ // loop over the east arm
      if(isIntersection(line,is)) return is;
    }
  }
  
  return -1;
 
}

//_____________________________________________________________________________
//
// If the single direct gamma or the 2 decay-gammas from a pi0/eta decay
// head into EMCal return 1. Otherwise, return 0.
int 
mEmcGeometryModule::EventInEMCalAcceptance(const PISAEvent *pisaEvent, 
					   const int kevent, TTree *T)
{
 
  T->GetEvent(kevent);
  int kpart = pisaEvent->GetKinNhit();
 
  float ptot;
  float pthet;
  float pphi;

  float p[3];
  float v[3];

  int GEANT_gamma_code = 1;
  int GEANT_pi0_code = 7;
  int GEANT_eta_code = 17;

  // loop on pisaEvent
  for( int ipart = 0; ipart<kpart; ipart++)
    {
      TClonesArray *fpisaKinHits = pisaEvent->GetKinHits();
      KinPISAHit *KinHit = (KinPISAHit*)fpisaKinHits->UncheckedAt(ipart);
      
      int idpart  = KinHit->GetIdpart();
      int idparent = KinHit->GetIdparent();
      int itparent = KinHit->GetItparent();
      
      int nGamma = 0;
      int nGamAcc = 0;

      // Found a primary photon
      if( idparent==0 && idpart==GEANT_gamma_code && itparent < 0 )
	{
	  //std::cout << " <I> checking forced acceptance for gamma ..." << std::endl;
	  v[0] = 0; // Pi0 decays instantly
	  v[1] = 0;
	  v[2] = KinHit->GetZvertex();
	  
	  ptot = KinHit->GetPtot() ;
	  pphi = KinHit->GetPhi() * M_PI/180.;
	  pthet = KinHit->GetPthet() * M_PI/180.;
	  
	  p[0] = ptot * sin (pthet) * cos (pphi);
	  p[1] = ptot * sin (pthet) * sin (pphi);
	  p[2] = ptot * cos (pthet);
      
	  if (HitInEMCalAcceptance(v,p)>=0) nGamAcc++; // 0 is a valid sector: W0 !
	  if ( nGamAcc!=1 ) return 0; // gamma not on detector

	  //std::cout << " <I> gamma in EMCal acceptance ..." << std::endl;
	  return 1;

	} // photon
      
      // Found a primary pion
      // We accept only Two-Photon Decays where
      // both can hit the detector
      if( idparent==0 && idpart==GEANT_pi0_code && itparent < 0 )
	{
	  //std::cout << " <I> checking forced acceptance for pi0 ..." << std::endl;
     	  if(kpart<3) // at least one decay photon not even stored 
	    { 
	      return 0;  // ==> no hit on active surface so skip it
	    }
 
	  for(int i = 0;i < kpart;i++)
	    {
	      // Search for Decay gammas
	      KinPISAHit *Hit = (KinPISAHit*)fpisaKinHits->UncheckedAt(i);
	      if( Hit->GetIdparent () == GEANT_pi0_code && 
		  Hit->GetIdpart () == GEANT_gamma_code )
		{
		  if(nGamma<2)
		    {
		      // std::cout << "Photon Nr " << nGamma << std::endl;
		      v[0] = 0; // Pi0 decays instantly
		      v[1] = 0;
		      v[2] = Hit->GetZvertex();
	     
		      ptot = Hit->GetPtot() ;
		      pphi = Hit->GetPhi() * M_PI/180.;
		      pthet = Hit->GetPthet() * M_PI/180.;
		      
		      p[0] = ptot * sin (pthet) * cos (pphi);
		      p[1] = ptot * sin (pthet) * sin (pphi);
		      p[2] = ptot * cos (pthet);
		      
		      if (HitInEMCalAcceptance(v,p)>=0) nGamAcc++; // 0 is a valid sector: W0 !
		    }
		  nGamma++;	    
		}
	    }
     
	  if( nGamma!=2 ) { return 0; } // Not a 2-gamma Decay !
	  if( nGamAcc!=2 ){ return 0; } // Not both gammas on Detector 

	  //std::cout << " <I> pi0 in EMCal acceptance ..." << std::endl;
	  return 1;
	  
	} // pi0
  
      // Found a primary eta
      // We accept only Two-Photon Decays where
      // both head to the detector
      if( idparent==0 && idpart==GEANT_eta_code && itparent < 0)
	{
	  //std::cout << " <I> checking forced acceptance for eta ..." << std::endl;
	  if(kpart<3) // at  least one decay photon not even stored 
	    {
	      return 0;  // ==> no hit on active surface so skip it
	    }
      
	  for(int i = 0;i < kpart;i++)
	    {
	      // Search for Decay gammas
	      KinPISAHit *Hit = (KinPISAHit*)fpisaKinHits->UncheckedAt(i);
	      if( Hit->GetIdparent () == GEANT_eta_code && 
		  Hit->GetIdpart () == GEANT_gamma_code )
		{
		  if(nGamma<2)
		    {
		      //std::cout << "Photon Nr " << nGamma << std::endl;
		      v[0] = 0; // eta decays instantly
		      v[1] = 0;
		      v[2] = Hit->GetZvertex();
		      
		      ptot = Hit->GetPtot() ;
		      pphi = Hit->GetPhi() * M_PI/180.;
		      pthet = Hit->GetPthet() * M_PI/180.;
		      
		      p[0] = ptot * sin (pthet) * cos (pphi);
		      p[1] = ptot * sin (pthet) * sin (pphi);
		      p[2] = ptot * cos (pthet);
		      
		      if (HitInEMCalAcceptance(v,p)>=0) nGamAcc++; // 0 is a valid sector: W0 !
		    }
		  nGamma++;		    
		} 
	    }

	  if( nGamma!=2 )  { return 0; } // Not a 2-gamma Decay !
	  if( nGamAcc!=2 ) { return 0; } // Not both gammas heading to the detector
	  
	  //std::cout << " <I> eta in EMCal acceptance ..." << std::endl;
	  return 1;

	} // eta

    } // end loop on pisaEvent

  std::cout << "<W> EventInEMCalAcceptance: " 
	    << "No primary gamma, pi0, eta found ... Event completely skipped !" << std::endl;

  return 0;
}


//_____________________________________________________________________________
//
// If the particle of momentum p and vertex v heads into PbSc
// return true. Otherwise return false.
bool
mEmcGeometryModule::HitInPbSc(const float *v, const float *p, int& sector)
{

  sector = HitInEMCalAcceptance(v,p);

  // PbSc sectors (in EMC-mEmcGeometryModule and EMC-Online conventions)
  if ( sector == 0 || sector == 1 || sector == 2 || sector == 3 || sector == 4 || sector == 5 ) 
    return true;

  return false; // case: -1 (outside EMCal) or 6,7 (PbGl)

}

//_____________________________________________________________________________
//
// If the particle of momentum p and vertex v heads into PbGl
// return true. Otherwise return false.
bool
mEmcGeometryModule::HitInPbGl(const float *v, const float *p, int& sector)
{

  sector = HitInEMCalAcceptance(v,p);
  if ( sector == 7 || sector == 6 ) return true; // PbGl sectors (in EMC-mEmcGeometryModule and EMC-Online conventions)

  return false; // case: -1 (outside EMCal) or 0-5 (PbSc)

}

















int mEmcGeometryModule::Init(PHCompositeNode * root){
  // TODO: should delete myself from tree when destructed...

  PHCompositeNode* emcNode = emcNodeHelper::findCompositeNode(root, "EMC");

  if( !emcNode ){
    if ( !emcNodeHelper::makeCompositeNode(root, "EMC", "-p") ) return ABORTRUN;
    emcNode = emcNodeHelper::findCompositeNode(root, "EMC");
  }

  EMCNODEASSERT( emcNode );


  PHDataNode<mEmcGeometryModule> * geometryNode = new PHIODataNode<mEmcGeometryModule>(NULL, "mEmcGeometryModule");
  emcNode->addNode(geometryNode);
  geometryNode->setData( this );    
  geometryNode->setResetFlag( 0 );

//  emcNodeHelper::insertObject<mEmcGeometryModule>(emcNode, this, "mEmcGeometryModule");

  return 0;
}
