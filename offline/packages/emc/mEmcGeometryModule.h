#ifndef __MEMCGEOMETRYMODULE_H__
#define __MEMCGEOMETRYMODULE_H__

//
// EMC Geometry class.
//
// Alexander Bazilevsky (Sep-00)
//
// Modifications by Julia Velkovska to allow geometry transformations (Jan 2002)
//
// Utilities added by D.d'Enterria (Apr. 2003)
//
// New geometry (PISA geometry after 2002) updated by Wenqing Fan (May 2019)

#include <PHGeometry.h>
#include <iostream>
#include <PHTimeStamp.h>
#include <TObject.h>
#include <PISAEvent.h>
#include <SubsysReco.h>

#define MAX_SECTORS 8
#define NARMS 2
#define NSECTORS 4

/** EMCAL Geometry utilities. */

class PHCompositeNode;

class mEmcGeometryModule: public TObject, public SubsysReco
{
public:

  enum ERealm { kReal=0, kPISA=1 };

  mEmcGeometryModule(ERealm type=kReal);

  virtual ~mEmcGeometryModule() {};

  void BuildGeometry();
  void BuildGeometryPISA();

  void Retract();
  void Retract( float* eastEmc, float* westEmc );
  void SetSectorDim( int is, int nX, int nY );
  void SetTowerSize( int is, float xsz, float ysz );
  void SetMatrixVector( int is, PHMatrix mx, PHVector vt );
  void GetSectorDim( int is, int &nX, int &nY );
  void GetTowerSize( int is, float &xsz, float &ysz );
  void GetMatrixVector( int is, PHMatrix &mx, PHVector &vt );
  int GetTowerPosLocal( int is, int ind, float &x, float &y, float &z);
  int GetTowerPosLocal( int is, int ix, int iy, float &x, float &y, float &z);
  int GetTowerPosGlobal( int is, int ind, float &x, float &y, float &z);
  int GetTowerPosGlobal( int is, int ix, int iy, float &x, float &y, float &z);
  double GetSectorCenterInGlobalCoords( int is, int xyz );
  void LocalToGlobal( int is, float xl, float yl, float zl, float &xg, float &yg, float &zg);
  void GlobalToLocal( float xg, float yg, float zg, int is, float &xl, float &yl, float &zl);

  void print();
  void printCorners();
  void printSectorNumberingConventions();
  PHPanel GetPanel(short arm, short sector);  //get the sector geometry

  /** From EMCAL offline (arm,sector) convention to this module
      sector number convention.*/
  static int emcOfflineToEmc(short arm, short sector);

  /** From EMCAL offline (arm,sector) to EMCAL online (sector)
      sector number convention.*/
  static int emcOfflineToEmcOnline(short arm, short sector);

  /** From sector number (0-7) (this module convention)
      get ARM and SECTOR in PHENIX convention.*/
  static void emcToPhenix(int i, short &arm,short &sector);

  /** From sector number (this module convention) to sector
      number (emc online convention). */
  static int emcToEmcOnline(int iS);

  /** From sector number (emc online convention) to sector
      number (this module convention). */
  static int emcOnlineToEmc(int is);

  /** From ARM and SECTOR (PHENIX convention) get sector number
      (this module convention).*/
  static int PhenixToEmc(short arm, short sector);

  void BuildPanels();
  void readFromDB();
  PHBoolean isIntersection(PHLine&,const short&); // true if Line intersects with sector int is  
  PHBoolean Intersection(PHLine&,const short&,PHPoint&); // project line onto sector is

  // utilities

  int HitInEMCalAcceptance(const float*, const float*);
  int EventInEMCalAcceptance(const PISAEvent *pisaEvent,const int kevent, TTree *T);
  bool HitInPbSc(const float*, const float* , int&);
  bool HitInPbGl(const float*, const float* , int&);


  // SubsysReco stuff: so you can easily add to tree
  virtual int Init(PHCompositeNode * root);

private:

#ifndef __CINT__

  int nx[MAX_SECTORS];
  int ny[MAX_SECTORS];
  float tower_xsize[MAX_SECTORS];
  float tower_ysize[MAX_SECTORS];
  PHMatrix emcrm[MAX_SECTORS];
  PHVector emctr[MAX_SECTORS];
  PHMatrix invemcrm[MAX_SECTORS];
  PHVector invemctr[MAX_SECTORS];
  PHPanel emcSectors[NARMS][NSECTORS];

#endif

  ClassDef(mEmcGeometryModule,1)
};

#endif /*__MEMCGEOMETRYMODULE_H__*/
