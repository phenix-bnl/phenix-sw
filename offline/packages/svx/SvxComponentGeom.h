
#ifndef __SVXCOMPONENTGEOM_H__
#define __SVXCOMPONENTGEOM_H__

#include <vector>

class SvxPixelHotDeadMapv2;
class SvxPixelHotDeadMap;
class SvxDeadMap;
class svxDetectorGeo;
class SvxSensor;
class svxAddress;
class TGeoMaterial;
class TGeoMedium;
class TGeoVolume;
class TGeoNode;
class TGeoManager;
class TGeoTranslation;
class TGeoCombiTrans;

class ScgHit
{
public:
  ScgHit() :
    layer(-1),
    ladder(-1),
    sensor(-1),
    component(-1),
    tile(-1),
    x(0.),
    y(0.),
    z(0.),
    xs(0.),
    ys(0.),
    zs(0.),
    ds(-9999.),
    dz(-9999.),
    xsigma(0.),
    zsigma(0.),
    status(-1),
    livefrac(-1),
    node(0)
  { }

  ~ScgHit() {};

  int layer,ladder,sensor,component,tile;
  double x,y,z;    // Global position
  double xs,ys,zs; // Position on sensor wrt center point
  double ds,dz;    // Residuals to projected track position (s = Rdphi)
  double xsigma;   // Cluster size/resolution in x ~ s coord (cm)
  double zsigma;   // Cluster size/resolution in z coord (cm)
  int status;      // 0 bad, 1+ good.
  float livefrac;  // Fraction of channels in tile that are good
  TGeoNode *node;
};

class ScgTrack
{
public:
  ScgTrack() :
    nhits(0),
    charge(0),
    vx(0.),
    vy(0.),
    vz(0.),
    mom(0.),
    phi0(0.),
    the0(0.),
    bfield(0.)
  { }

  ~ScgTrack() {};

  ScgHit GetHit(int i)
  {
    return hits.at(i);
  }

  int nhits;
  int charge;
  double vx,vy,vz;
  double mom, phi0, the0;
  double bfield;
  std::vector<ScgHit> hits;
};

class ScgBox
{
public:
  ScgBox() :
    x(0.),
    y(0.),
    z(0.),
    phi(0.),
    theta(0.),
    psi(0.),
    xhw(0.),
    yhw(0.),
    zhw(0.)
  { }

  ~ScgBox() {};

  // Position at centroid (relative to parent node)
  double x,y,z;

  // Rotation angle in degrees
  double phi, theta, psi;

  // Half-widths
  double xhw, yhw, zhw;
};

class SvxComponentGeom
{
public:
  SvxComponentGeom();
  SvxComponentGeom(svxDetectorGeo *geo);
  ~SvxComponentGeom();

  void InitMaterials();
  bool ValidIndexCombination(int ily, int ildr, int isen);
  bool ValidIndexCombination(int ily, int ildr, int isen, int isec, int itl);
  TGeoVolume *SensorVolume(int ily, int ildr, int isen);
  TGeoVolume *PixelChipVolume(int ily, int ildr, int isen, int iroc);
  TGeoVolume *StripHalfModuleVolume(int ily, int ildr, int isen, int isec);
  TGeoVolume *TileVolume(int ily, int ildr, int isen, int isec, int itl);
  TGeoCombiTrans *SensorPlacement(int ily, int ildr, int isen);

  void SetVerbosity(int v = 0)
  {
    fVerbosity = v;
  }
  void BuildVtxModel();
  bool AssignMap(int runnumber, svxAddress *address = 0x0);
  bool AssignMap(SvxPixelHotDeadMapv2 *pixelHotDead, SvxDeadMap *stripMap, svxAddress *address = 0x0);
  bool IsGoodVolume(TGeoNode *node);
  bool IsGoodVolume(TGeoVolume *vol);
  bool IsGoodVolume(int layer, int ladder, int sensor, int component);
  bool IsActiveVolume(TGeoVolume *vol);

  // assign address[0-4] to layer, ladder, sensor, component, tile.
  void GetAddress(TGeoNode *node, int *address);
  void GetAddress(TGeoNode *node, ScgHit &hit);

  ScgBox SensorGeom(int ily, int ildr, int isen);
  ScgBox   ChipGeom(int ily, int ildr, int isen, int isec);
  ScgBox   TileGeom(int ily, int ildr, int isen, int isec, int itl);

  ScgTrack FindHitsFromVertex(double vx, double vy, double vz,
                              double mom, double phi0, double the0,
                              double charge, double magField);

  int GetNearestSensorSegment(double x, double y, double z, SvxSensor *sensor);
  void PrintMask(unsigned int number); // Print binary rep. of number to stdout

  TGeoManager *GetGeoManager()
  {
    return fGeoMgr;
  }

  // Returns fraction of channels with good QA status in a tile.
  // Info currently available for pixel layers only.
  float GetTileGoodFrac(int ily, int ildr, int isen, int isec, int itl);

  // Returns tile index 0-15 given local position on sensor.
  int GetTile(int ily, int ildr, int isen, float xl, float yl, float zl);

  // Returns chip/halfmodule given local position on sensor.
  int GetComponent(int ily, int ildr, int isen, float xl, float yl, float zl);

protected:

  svxAddress         *fAddress;
  SvxPixelHotDeadMapv2 *fPixelMap;
  SvxDeadMap         *fStripMap;
  svxDetectorGeo *fDetGeo;
  bool fNewGeo;

  TGeoManager *fGeoMgr;
  TGeoVolume *fTopVolume;

  // Materials
  TGeoMaterial *fVacuumMaterial;
  TGeoMaterial *fSiliconMaterial;
  TGeoMaterial *fAluminumMaterial;

  // Media
  TGeoMedium *fVacuumMedia;
  TGeoMedium *fSiliconMedia;
  TGeoMedium *fAluminumMedia;

  int fComponentId; // Unique identifier for all elements
  int fVerbosity;

  std::vector<ScgHit> fHits;

};
#endif //__SVXCOMPONENTGEOM_H__
