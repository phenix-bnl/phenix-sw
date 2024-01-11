
// SVX headers
#include "SvxComponentGeom.h"
#include "SvxSensor.h"
#include "svxDetectorGeo.hh"

// For dead/hot map
#include "RunToTime.hh"
#include "PHTimeStamp.h"
#include "svxAddress.hh"
#include "SvxPixelHotDeadMapv2.h"
#include "SvxDeadMap.h"

// ROOT headers
#include "TCanvas.h"
#include "TMath.h"
#include "TString.h"
#include "TObjString.h"
#include "TRegexp.h"
#include "TObjArray.h"
#include "TGeoMaterial.h"
#include "TGeoMedium.h"
#include "TGeoVolume.h"
#include "TGeoManager.h"
#include "TGeoNavigator.h"
#include "TGeoMatrix.h"

#include <iostream>

using namespace std;

SvxComponentGeom::SvxComponentGeom() :
  fAddress(NULL),
  fPixelMap(NULL),
  fStripMap(NULL),
  fDetGeo(NULL),
  fNewGeo(true),
  fComponentId(2),
  fVerbosity(0),
  fHits(4)
{
  fDetGeo = new svxDetectorGeo();
  fDetGeo->Read_svxPISApar();
  if (gGeoManager)
  {
    cout << PHWHERE
         << "NOTE: A TGeoManager object already exists. The current TGeoManager "
         << gGeoManager->GetName()
         << " will be replaced now." << endl;
    gGeoManager = 0;
  }
  fGeoMgr = new TGeoManager("fGeoMgr", "PHENIX VTX Geometry");
  BuildVtxModel();
}

SvxComponentGeom::SvxComponentGeom(svxDetectorGeo *geo) :
  fAddress(0),
  fPixelMap(0),
  fStripMap(0),
  fDetGeo(geo),
  fNewGeo(false),
  fComponentId(2),
  fVerbosity(0),
  fHits(4)
{
  if (gGeoManager)
  {
    cout << PHWHERE
         << "NOTE: A TGeoManager object already exists. The current TGeoManager "
         << gGeoManager->GetName()
         << " will be replaced now." << endl;
    gGeoManager = 0;
  }
  fGeoMgr = new TGeoManager("fGeoMgr", "PHENIX VTX Geometry");
  BuildVtxModel();
}

SvxComponentGeom::~SvxComponentGeom()
{
  if (fNewGeo) // Wipe out only if newed in this class
    delete fDetGeo;
  delete fGeoMgr;
}

void
SvxComponentGeom::InitMaterials()
{
  // Materals: name, A, Z, rho (g/cm3)
  fVacuumMaterial   = new TGeoMaterial("Vacuum material",0,0,0);
  fSiliconMaterial  = new TGeoMaterial("Silicon", 28.085, 14, 2.329);
  fAluminumMaterial = new TGeoMaterial("Aluminum", 26.98,  13, 2.7);

  // Media
  fVacuumMedia   = new TGeoMedium("fVacuumMedia",   1, fVacuumMaterial);
  fSiliconMedia  = new TGeoMedium("fSiliconMedia",  2, fSiliconMaterial);
  fAluminumMedia = new TGeoMedium("fAluminumMedia", 3, fAluminumMaterial);
  return;
}

bool
SvxComponentGeom::ValidIndexCombination(int ily, int ildr, int isen)
{
  if (ily < 0 || ily > 3)
  {
    cerr << PHWHERE << "Invalid layer index " << ily
         << ". Must be [0-3]." << endl;
    return false;
  }
  if (ily < 2)
  {
    if (isen < 0 || isen > 3)
    {
      cerr << PHWHERE << "Invalid sensor index for layer" << ily
           << ": " << isen << " Must be [0-3]." << endl;
      return false;
    }
  }
  if (ily == 2)
  {
    if (isen < 0 || isen > 4)
    {
      cerr << PHWHERE << "Invalid sensor index for layer" << ily
           << ": " << isen << " Must be [0-4]." << endl;
      return false;
    }
  }
  if (ily == 3)
  {
    if (isen < 0 || isen > 5)
    {
      cerr << PHWHERE << "Invalid sensor index for layer" << ily
           << ": " << isen << " Must be [0-4]." << endl;
      return false;
    }
  }
  return true;
}

bool
SvxComponentGeom::ValidIndexCombination(int ily, int ildr, int isen, int isec, int itl)
{
  bool isvalid = false;
  if (ValidIndexCombination(ily, ildr, isen))
  {
    if (ily < 2 && isec >=0 && isec < 4)
      isvalid = true;
    if (ily > 1 && isec >=0 && isec < 2)
      isvalid = true;
  }
  if (itl < 0 || itl > 15)
    isvalid = false;

  return isvalid;
}

ScgBox
SvxComponentGeom::SensorGeom(int ily, int ildr, int isen)
{
  ScgBox b;

  if (!ValidIndexCombination(ily, ildr, isen))
  {
    cerr << PHWHERE << "Invalid index combination: layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return b;
  }

  SvxSensor *sensor = fDetGeo->GetSensorPtr(ily, ildr, isen);
  if (!sensor)
  {
    cerr << PHWHERE << "No sensor found for layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return b;
  }

  // Sensor orientation
  double cosphi   = sensor->get_rotMatrix(0, 0);
  double sinphi   = sensor->get_rotMatrix(0, 1);
  double phirad   = TMath::ATan2(sinphi, cosphi);
  double phideg   = phirad/TMath::Pi()*180.;
  double costheta = sensor->get_rotMatrix(2, 2);
  double sintheta = sensor->get_rotMatrix(1, 2);
  double thetarad = TMath::ATan2(sintheta, costheta);
  double thetadeg = thetarad/TMath::Pi()*180.;
  double psideg   = 0;

  // Sensor dimensions
  double xHalfWidth = sensor->get_xhalfWidth();
  double yHalfWidth = (ily < 2) ? 0.02 : 0.0625;
  double zHalfWidth = 0;
  int nsc = sensor->get_nSection();
  int readout = 0; // This remains zero
  for (int isc=0; isc<nsc; isc++)
    zHalfWidth += sensor->get_zhalfWidth(isc,readout);

  // Output struct
  b.x     = sensor->get_correctedTransVector(0);
  b.y     = sensor->get_correctedTransVector(1);
  b.z     = sensor->get_correctedTransVector(2);
  b.phi   = phideg;
  b.theta = thetadeg;
  b.psi   = psideg;
  b.xhw   = xHalfWidth;
  b.yhw   = yHalfWidth;
  b.zhw   = zHalfWidth;

  return b;
}

ScgBox
SvxComponentGeom::ChipGeom(int ily, int ildr, int isen, int isec)
{
  // Here, "chip" refers to pixel chip _or_ strip half-module.
  ScgBox chip;
  ScgBox sensor = SensorGeom(ily, ildr, isen);

  // Pixel (ily 0,1) or Stripixel (2,3) z-offset
  static const int nPixelZHalfWidths[4] = {3,1,-1,-3};
  static const int nStripZHalfWidths[2] = {-1,1};
  bool idxcheck=false;
  if(ily<2) {
    if(0<=isec&&isec<4) idxcheck=true;
  }
  else {
    if(0<=isec&&isec<2) idxcheck=true;
  }
  if(!idxcheck) {
    cerr<<"out of range : layer="<<ily<<", section="<<isec<<" blank chip is returned"<<endl;
    return chip; 
  }

  int nhw = (ily < 2)? nPixelZHalfWidths[isec] : nStripZHalfWidths[isec];

  // Dimensions
  double nsegs = (ily < 2)? 4 : 2;
  chip.xhw = sensor.xhw;
  chip.yhw = sensor.yhw;
  chip.zhw = sensor.zhw/nsegs;

  // Position wrt center of sensor
  chip.x = 0;
  chip.y = 0;
  chip.z = nhw*chip.zhw;

  // Global orientation
  chip.phi   = sensor.phi;
  chip.theta = sensor.theta;
  chip.psi   = sensor.psi;

  return chip;
}

ScgBox
SvxComponentGeom::TileGeom(int ily, int ildr, int isen, int isec, int itl)
{
  // Tiles are identical rectangles formed by splitting a
  // chip/halfmodule element in a 4x4 grid. They are used just for hit
  // position specification, with no direct correspondence to pixel or
  // strip positions. They are indexed as follows:
  /*
    |----+----+----+----|
    |  0 |  1 |  2 |  3 |
    |----+----+----+----|
    |  4 |  5 |  6 |  7 |
    |----+----+----+----|
    |  8 |  9 | 10 | 11 |
    |----+----+----+----|
    | 12 | 13 | 14 | 15 |
    |----+----+----+----|
  */

  ScgBox tile;
  ScgBox elem = ChipGeom(ily, ildr, isen, isec);
  int row = itl/4;
  int col = itl%4;

  // Tile half-widths from chip center to tile center
  int nhw[4] = {-3,-1,1,3};

  // Dimensions
  tile.xhw = elem.xhw/4;
  tile.yhw = elem.yhw;
  tile.zhw = elem.zhw/4;

  // Position on chip/halfmod element
  tile.x = nhw[3-row]*tile.xhw;
  tile.y = 0;
  tile.z = nhw[col]*tile.zhw;

  // Global orientation
  tile.phi   = elem.phi;
  tile.theta = elem.theta;
  tile.psi   = elem.psi;

  return tile;
}


TGeoCombiTrans *
SvxComponentGeom::SensorPlacement(int ily, int ildr, int isen)
{
  if (!ValidIndexCombination(ily, ildr, isen))
  {
    cerr << PHWHERE << "Invalid index combination: layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return 0;
  }

  ScgBox b = SensorGeom(ily,ildr,isen);
  TGeoRotation *rot = new TGeoRotation("rot", b.phi, b.theta, b.psi);
  TGeoCombiTrans *placement = new TGeoCombiTrans(b.x, b.y, b.z, rot);
  return placement;
}

TGeoVolume *
SvxComponentGeom::SensorVolume(int ily, int ildr, int isen)
{
  if (!ValidIndexCombination(ily, ildr, isen))
  {
    cerr << PHWHERE << "Invalid index combination: layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return 0;
  }

  ScgBox b = SensorGeom(ily,ildr,isen);
  TString sensorName = Form("sensor_%d_%d_%d", ily, ildr, isen);
  TGeoVolume *sensorVol = fGeoMgr->MakeBox(sensorName.Data(), fSiliconMedia,
                          b.xhw, b.yhw, b.zhw);
  return sensorVol;
}

TGeoVolume *
SvxComponentGeom::PixelChipVolume(int ily, int ildr, int isen, int iroc)
{
  if (ily > 1)
  {
    cerr << PHWHERE << "Invalid layer "
         << ily << endl;
    return 0;
  }

  if (!ValidIndexCombination(ily, ildr, isen))
  {
    cerr << PHWHERE << "Invalid index combination: layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return 0;
  }

  ScgBox chip = ChipGeom(ily, ildr, isen, iroc);
  TString name = Form("roc_%d_%d_%d_%d", ily, ildr, isen, iroc);
  TGeoVolume *chipVol = fGeoMgr->MakeBox(name.Data(), fSiliconMedia,
                                         chip.xhw, chip.yhw, chip.zhw);
  return chipVol;
}

TGeoVolume *
SvxComponentGeom::StripHalfModuleVolume(int ily, int ildr, int isen, int isec)
{
  if (ily < 2)
  {
    cerr << PHWHERE << "Invalid layer "
         << ily << endl;
    return 0;
  }

  if (!ValidIndexCombination(ily, ildr, isen))
  {
    cerr << PHWHERE << "Invalid index combination: layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return 0;
  }

  ScgBox hmod = ChipGeom(ily, ildr, isen, isec);
  TString name = Form("halfmod_%d_%d_%d_%d", ily, ildr, isen, isec);
  TGeoVolume *halfmodVol = fGeoMgr->MakeBox(name.Data(), fSiliconMedia,
                           hmod.xhw, hmod.yhw, hmod.zhw);
  return halfmodVol;
}

TGeoVolume *
SvxComponentGeom::TileVolume(int ily, int ildr, int isen, int isec, int itl)
{
  if (!ValidIndexCombination(ily, ildr, isen))
  {
    cerr << PHWHERE << "Invalid index combination: layer, ladder, sensor "
         << ily << ", " << ildr <<", " << isen << endl;
    return 0;
  }

  ScgBox tile = TileGeom(ily, ildr, isen, isec, itl);
  TString name = Form("tile_%d_%d_%d_%d_%d", ily, ildr, isen, isec, itl);
  TGeoVolume *vol = fGeoMgr->MakeBox(name.Data(), fSiliconMedia,
                                     tile.xhw, tile.yhw, tile.zhw);
  return vol;
}

int
SvxComponentGeom::GetComponent(int ily, int ildr, int isen, float xl, float yl, float zl)
{
  // (xl,yl,zl) is local position on sensor. (0,0,0) is center of sensor.
  ScgBox sensor = SensorGeom(ily, ildr, isen);
  int nchips = ily<2? 4 : 2;

  return nchips*(sensor.zhw + zl)/(2*sensor.zhw);
}

int
SvxComponentGeom::GetTile(int ily, int ildr, int isen, float xl, float yl, float zl)
{
  // (xl,yl,zl) is local position on sensor. (0,0,0) is center of sensor.
  ScgBox sensor = SensorGeom(ily, ildr, isen);
  int nchips = ily<2? 4 : 2;
  int col_on_sensor = 4*nchips*(sensor.zhw + zl)/(2*sensor.zhw);
  int row_on_sensor = 4*(sensor.xhw - xl)/(2*sensor.xhw);
  int col_on_chip = col_on_sensor % nchips;
  int row_on_chip = row_on_sensor;

  return 4*row_on_chip + col_on_chip;
}

float
SvxComponentGeom::GetTileGoodFrac(int ily, int ildr, int isen, int isec, int itl)
{
  // Return fraction of good channels (# good / 512) in pixel tile.

  // Validate inputs
  if (!ValidIndexCombination(ily, ildr, isen, isec, itl))
  {
    cerr << PHWHERE
         << "Invalid index combination: layer, ladder, sensor, section, tile "
         << ily << ", " << ildr << ", " << isen << ", " << isec << ", " << itl
         << endl;
    return -1;
  }
  if (!fAddress)
  {
    cerr << PHWHERE << "Null svxAddress pointer" << endl;
    return -1;
  }
  if (!fPixelMap)
  {
    cerr << PHWHERE << "Null SvxPixelHotDeadMapv2 pointer" << endl;
    return -1;
  }
  if (!fStripMap)
  {
    cerr << PHWHERE << "Null SvxDeadMap pointer" << endl;
    return -1;
  }

  // Pixel layers
  if (ily < 2)
  {
    int module = fAddress->getModuleSensor0(ily, ildr, isen, 0, 0);
    int ROC    = fAddress->getROCSensor0(ily, ildr, isen, 0, isec*32);
    float f    = fPixelMap->getTileGoodFrac(module, ROC, itl);
    return f;
  }

  // Stripixel layers
  //
  // For now, since tile info unavailable for stripixels, use
  // halfmodule level QA info for tiles.
  TString name = Form("halfmod_%d_%d_%d_%d", ily,ildr,isen,isec);
  TGeoVolume *hmod = fGeoMgr->GetVolume(name.Data());
  if (!hmod)
    Printf("!hmod %s", name.Data());

  return IsGoodVolume(hmod)? 1. : 0.;

  //return -1.;
}

void
SvxComponentGeom::BuildVtxModel()
{
  InitMaterials();

  // Outermost "top" volume in the hierarchy
  fTopVolume = fGeoMgr->MakeBox("fTopVolume",fVacuumMedia,100.,100.,100.); // cm
  fTopVolume->SetLineColor(kBlue);
  fGeoMgr->SetTopVolume(fTopVolume);
  fGeoMgr->SetTopVisible(true);

  for (int ily=0; ily<4; ily++)
  {
    int nLadder = fDetGeo->get_nBarLadder(ily);
    int nSensor = fDetGeo->get_nBarSensor(ily);
    for (int ildr=0; ildr<nLadder; ildr++)
    {
      for (int isen=0; isen<nSensor; isen++)
      {

        TGeoVolume *sensorVol = SensorVolume(ily, ildr, isen);
        sensorVol->SetLineColor(kGray+2);

        if (ily < 2)   // pixel layers
        {
          for (int iroc=0; iroc<4; iroc++)
          {

            // Add chip to sensor
            TGeoVolume *chip = PixelChipVolume(ily, ildr, isen, iroc);
            ScgBox b = ChipGeom(ily, ildr, isen, iroc);
            sensorVol->AddNode(chip, fComponentId++,
                               new TGeoTranslation(b.x, b.y, b.z));
            chip->SetLineColor(kWhite);

            // Add tiles to chip
            for (int itl=0; itl<16; itl++)
            {
              TGeoVolume *tile = TileVolume(ily, ildr, isen, iroc, itl);
              ScgBox t = TileGeom(ily, ildr, isen, iroc, itl);
              chip->AddNode(tile, fComponentId++,
                            new TGeoTranslation(t.x, t.y, t.z));
              tile->SetLineColor(kYellow);
              tile->SetLineWidth(2);
              tile->SetVisibility(false); // In simulations, turn on hit tiles only
            }
          }
        }

        else   // stripixel layers
        {
          for (int isec=0; isec<2; isec++)
          {

            // Add halfmodule to sensor
            TGeoVolume *hmod = StripHalfModuleVolume(ily, ildr, isen, isec);
            ScgBox b = ChipGeom(ily, ildr, isen, isec);
            sensorVol->AddNode(hmod, fComponentId++,
                               new TGeoTranslation(b.x, b.y, b.z));
            hmod->SetLineColor(kWhite);

            // Add tiles to halfmodule
            for (int itl=0; itl<16; itl++)
            {
              TGeoVolume *tile = TileVolume(ily, ildr, isen, isec, itl);
              ScgBox t = TileGeom(ily, ildr, isen, isec, itl);
              hmod->AddNode(tile, fComponentId++,
                            new TGeoTranslation(t.x, t.y, t.z));
              tile->SetLineColor(kYellow);
              tile->SetLineWidth(2);
              tile->SetVisibility(false);
            }
          }
        }
        fTopVolume->AddNode(sensorVol, fComponentId++, SensorPlacement(ily, ildr, isen));
      }
    }
  }

  fGeoMgr->CloseGeometry();
  fGeoMgr->SetVisLevel(4);
  fTopVolume->SetVisContainers(true);

  return;
}

bool
SvxComponentGeom::AssignMap(SvxPixelHotDeadMapv2 *pixelMap, SvxDeadMap *stripMap, svxAddress *address)
{
  // check that pixelMap and stripMap exist
  if (!pixelMap)
  {
    cout << PHWHERE << "SvxPixelHotDeadMapv2 not provided, abort"
         << endl;
    return false;
  }
  if (!stripMap)
  {
    cout << PHWHERE << "SvxDeadMap not provided, abort"
         << endl;
    return false;
  }
  if (!address)
  {
    cout << PHWHERE << "svxAddress not provided, abort"
         << endl;
  }

  // Assign fAddress, fPixelMap, fStripMap
  fAddress  = address;
  fPixelMap = pixelMap;
  fStripMap = stripMap;

  for (int ily=0; ily<4; ily++)
  {
    int nLadder = fDetGeo->get_nBarLadder(ily);
    int nSensor = fDetGeo->get_nBarSensor(ily);
    for (int ildr=0; ildr<nLadder; ildr++)
    {
      for (int isen=0; isen<nSensor; isen++)
      {

        // ix,iz not needed (set to 0)
        int imodule = fAddress->getModuleSensor0(ily, ildr, isen, 0, 0);

        if (ily<2)   // pixel layers
        {
          for (int i=0; i<4; i++)
          {

            int ix = 0, iz = i*32; // ix not needed
            int iroc = fAddress->getROCSensor0(ily, ildr, isen, ix, iz);
            int pix_status = fPixelMap->getChipStatus(imodule,iroc);

            if (fVerbosity)
            {
              Printf("layer,ladder,sensor,iroc %d %d %d %d: chip status %d",
                     ily,ildr,isen,iroc,pix_status);
            }

            if (pix_status != 0)   // Bad chip
            {

              // Flag chip as bad by setting TGeoVolume::kVolumeSelected bit
              TString chipName = Form("roc_%d_%d_%d_%d", ily,ildr,isen,i);
              TGeoVolume *chip = fGeoMgr->GetVolume(chipName.Data());

              if (chip)
              {
                chip->SetLineColor(kGray+2);
                chip->SelectVolume(0);

                // Also set daughter nodes to have same properties
                for (int itl=0; itl<16; itl++)
                {
                  TString tileName = Form("tile_%d_%d_%d_%d_%d", ily,ildr,isen,i,itl);
                  TGeoVolume *tile = fGeoMgr->GetVolume(tileName.Data());
                  tile->SetLineColor(kRed);
                  tile->SelectVolume(0);
                }
              }
              else
                Printf("!chip %s", chipName.Data());
            }

            if (false)   // AMA check
            {
              for (int itl=0; itl<16; itl++)
              {
                float frac = GetTileGoodFrac(ily, ildr, isen, i, itl);
                printf(" %.2f", frac);
                if (itl==15) cout << endl;
              }
            }
          }
        }
        else   // stripixel layers
        {
          for (int isc=0; isc<2; isc++)   // 2 stripixel sensor sections
          {
            int striplayer = ily - 2;
            int readout0_status = stripMap->readoutStatus(striplayer,ildr,isen,isc,0);
            int readout1_status = stripMap->readoutStatus(striplayer,ildr,isen,isc,1);
            int status = -1;

            // status 0: both readouts good
            if (readout0_status==0 && readout1_status==0)
              status = 0;

            // status 1: one readout bad
            if ((readout0_status==0 && readout1_status!=0) ||
                (readout0_status!=0 && readout1_status==0))
              status = 1;

            // status 2: both readouts bad
            if (readout0_status!=0 && readout1_status!=0)
              status = 2;

            if (fVerbosity)
            {
              Printf("layer,ladder,sensor,halfmod %d %d %d %d: status %d",
                     ily,ildr,isen,isc,status);
            }

            if (status != 0)
            {
              // Flag halfmodule as bad by setting TGeoVolume::kVolumeSelected bit
              TString name = Form("halfmod_%d_%d_%d_%d", ily,ildr,isen,isc);
              TGeoVolume *hmod = fGeoMgr->GetVolume(name.Data());

              if (!hmod)
                Printf("!hmod %s", name.Data());

              else
              {
                hmod->SelectVolume(0);
                hmod->SetLineColor(kGray+2);

                // Also set daughter nodes to have same properties
                for (int itl=0; itl<16; itl++)
                {
                  TString tileName = Form("tile_%d_%d_%d_%d_%d", ily,ildr,isen,isc,itl);
                  TGeoVolume *tile = fGeoMgr->GetVolume(tileName.Data());
                  tile->SetLineColor(kRed);
                  tile->SelectVolume(0);
                }
              }
            }
          }
        }
      }
    }
  }

  cout << endl;

  Info("SvxComponentGeom::AssignMap()",
       "Assigned deadmap");

  return true;
}

bool
SvxComponentGeom::AssignMap(int runnumber, svxAddress *address)
{
  // This version of AssignMap() is intended for standalone use
  // (i.e. when the deadmap class objects are not already available in
  // memory). If SvxPixelHotDeadMap and SvxDeadMap objects exist
  // externally, e.g. on Fun4All nodes, then get them off the tree and
  // use the other version of this function, don't re-instantiate the
  // deadmap classes.
  //
  // Currently, this function gets maps from DB.
  // Low-priority TODO: Add a text file i/o option.

  // Get timestamp from run number - used by svxAddress
  RunToTime *rt = RunToTime::instance();
  PHTimeStamp *tStamp = rt->getBeginTime(runnumber);
  tStamp->print(); cout << endl;


  // svxAddress handles mapping between hardware and software address
  //--bool newAddress = false;
  if (!address)
  {
    cout << PHWHERE << "No svxAddress provided. Constructing new instance."
         << endl;
    //--newAddress = true;
    fAddress = new svxAddress;
  }

  fAddress->set_Verbose(0);
  fAddress->set_usedatabase(1);
  fAddress->setFetchTime(tStamp);
  fAddress->Initialize();

  // Pixel hot/dead map
  SvxPixelHotDeadMapv2 *pixelMap = new SvxPixelHotDeadMapv2();
  pixelMap->readFromDatabase(runnumber, true);
  pixelMap->setTileMap();

  // Strip hot/dead map
  SvxDeadMap *stripMap = new SvxDeadMap();
  stripMap->readReadoutsFromDatabase(runnumber);
  stripMap->readFromDatabase(runnumber);

  // Assign the map.
  // pixelMap, stripMap, address pointers will be assigned to
  // member pointers (so don't delete these!)
  bool assign = AssignMap(pixelMap, stripMap, fAddress);

  Info("SvxComponentGeom::AssignMap()",
       "Assigned deadmap from DB for run %d\n", runnumber);

  return assign;
}

bool
SvxComponentGeom::IsActiveVolume(TGeoVolume *vol)
{
  if (vol->IsTopVolume())
    return false;

  if (TString(vol->GetName()).Contains("sensor"))
    return false;

  return true;
}

bool
SvxComponentGeom::IsGoodVolume(TGeoNode *node)
{
  bool isgood = true;

  TGeoVolume *vol = node->GetVolume();
  if(vol==NULL){
    cerr<<"SvxComponentGeom::IsGoodVolume == no TGeoVolume is NULL"<<endl;
    return false;
  }

  if (vol->IsSelected())
    isgood = false;

  return isgood;
}

bool
SvxComponentGeom::IsGoodVolume(TGeoVolume *vol)
{
  if(vol==NULL){
    cerr<<"SvxComponentGeom::IsGoodVolume == TGeoVolume is NULL"<<endl;
    return false;
  }

  bool isgood = true;

  if (vol->IsSelected())
    isgood = false;

  return isgood;
}

bool
SvxComponentGeom::IsGoodVolume(int layer, int ladder, int sensor, int component)
{
  TString name;
  if (layer < 2)
    name = Form("roc_%d_%d_%d_%d", layer, ladder, sensor, component);
  else
    name = Form("halfmod_%d_%d_%d_%d", layer, ladder, sensor, component);

  TGeoVolume *vol = fGeoMgr->GetVolume(name.Data());

  if (!vol)
  {
    cerr << PHWHERE << " No TGeoVolume found with name"
         << name.Data() << endl;
    return false;
  }

  bool isgood = true;

  if (vol->IsSelected())
    isgood = false;

  return isgood;
}

void
SvxComponentGeom::GetAddress(TGeoNode *node, int *address)
{
  TString name = node->GetName();
  TRegexp re("_[0-9]+_[0-9]+_[0-9]+_[0-9]+_[0-9]+_");
  TString s = name(re);
  TObjArray *arr = s.Tokenize("_");

  for (int i=0; i<5; i++)
  {
    address[i] = ((TObjString *)arr->At(i))->GetString().Atoi();
  }

  delete arr;
  return;
}

void
SvxComponentGeom::GetAddress(TGeoNode *node, ScgHit &hit)
{
  TString name = node->GetName();
  TRegexp re("_[0-9]+_[0-9]+_[0-9]+_[0-9]+_[0-9]+_");
  TString s = name(re);
  TObjArray *arr = s.Tokenize("_");

  hit.layer     = ((TObjString *)arr->At(0))->GetString().Atoi();
  hit.ladder    = ((TObjString *)arr->At(1))->GetString().Atoi();
  hit.sensor    = ((TObjString *)arr->At(2))->GetString().Atoi();
  hit.component = ((TObjString *)arr->At(3))->GetString().Atoi();
  hit.tile      = ((TObjString *)arr->At(4))->GetString().Atoi();

  delete arr;
  return;
}

ScgTrack
SvxComponentGeom::FindHitsFromVertex(double vx, double vy, double vz,
                                     double mom, double phi0, double the0,
                                     double charge, double magField)
{
  ScgTrack track;
  double hits[4] = {-1,-1,-1,-1};
  double layerRadius[4] = {2.63, 5.13, 11.77, 16.69}; // (cm) nominal radii

  track.charge = charge;
  track.vx     = vx;
  track.vy     = vy;
  track.vz     = vz;
  track.mom    = mom;
  track.phi0   = phi0;
  track.the0   = the0;
  track.bfield = magField;

  if (TMath::IsNaN(vx) || TMath::IsNaN(vy) || TMath::IsNaN(vz))
  {
    if (fVerbosity) cout << PHWHERE << "NaN value in provided vertex. Exiting.\n" << endl;
    track.vx     = 9999;
    track.vy     = 9999;
    track.vz     = 9999;
    return track;
  }

  double R = -1.;      // (cm) bend radius
  double L = -1.;      // (cm) nominal radius at VTX layer
  double d = 0.;       // (cm) transverse deflection distance over a step
  double phiKick = 0.; // (rad) angular phi deflection over a step

  // Deflect phi in mag. field
  if (fabs(magField) > 0.)
  {

    // Use tan(phi) = d/L where d = R - sqrt(R*R - L*L) and R = |pt/0.003B|
    // Field polarity is determined from the sign of magField.
    R = TMath::Abs(mom*TMath::Sin(the0)/0.003/magField);
    L = layerRadius[0];
    if (R < L)
    {
      cout << PHWHERE << "Bend radius < projection distance. Exiting.\n"
           << endl;
      return track;
    }

    d = R - TMath::Sqrt(R*R - L*L);
    phiKick = charge*TMath::ATan2(d, L);

    if (TMath::IsNaN(phiKick))
    {
      if (fVerbosity)
        cout << PHWHERE << "phi deflection is NaN! Exiting.\n" << endl;
      return track;
    }
    if (magField < 0)
    {
      phiKick *= -1.0;
    }

    phi0 += phiKick;
  }

  // Initialize direction and postion
  double nx = TMath::Sin(the0)*TMath::Cos(phi0);
  double ny = TMath::Sin(the0)*TMath::Sin(phi0);
  double nz = TMath::Cos(the0);
  TGeoNode *currentNode = fGeoMgr->InitTrack(vx,vy,vz,nx,ny,nz);
  if(currentNode==NULL&&fVerbosity) cerr<<"no currentNode"<<endl;

  if (fVerbosity)
    Printf("Initialized track at (vx,vy,vz) %.2f,%.2f,%.2f with "
           "(nx,ny,nz) %.2f,%.2f,%.2f "
           "charge %.2f, magField %.2f, mom %.2f, R %.2f, d %.2f, phikick %.2f",
           fGeoMgr->GetCurrentPoint()[0],
           fGeoMgr->GetCurrentPoint()[1],
           fGeoMgr->GetCurrentPoint()[2],
           fGeoMgr->GetCurrentDirection()[0],
           fGeoMgr->GetCurrentDirection()[1],
           fGeoMgr->GetCurrentDirection()[2],
           charge,
           magField,
           mom,
           R,
           d,
           phiKick);

  int nSteps = 0;
  while (!fGeoMgr->IsOutside())
  {
    TGeoVolume *vol = fGeoMgr->GetCurrentVolume();

    nSteps++;
    if (nSteps > 10)
    {
      if (fVerbosity)
        cout << PHWHERE << "Forcing exit after 10 steps.\n" << endl;
      return track;
    }

    // Check for traversal of a tile.
    if (TString(vol->GetName()).Contains("tile"))
    {

      ScgHit hit;
      currentNode = fGeoMgr->GetCurrentNode();
      if (currentNode)
      {
        GetAddress(currentNode, hit);
        int layer = hit.layer;

        // Start out assuming this hit is bad, then update.
        if (hits[layer] == -1)
          hits[layer] = 0;
        if (IsGoodVolume(vol))
        {
          hits[layer] += 1;
        }

        hit.x      = fGeoMgr->GetCurrentPoint()[0];
        hit.y      = fGeoMgr->GetCurrentPoint()[1];
        hit.z      = fGeoMgr->GetCurrentPoint()[2];
        hit.status = hits[layer];
        hit.livefrac = GetTileGoodFrac(hit.layer, hit.ladder, hit.sensor, hit.component, hit.tile);
        hit.node   = currentNode;

        // Compute hit position on sensor from global x,y,z
        //if (currentNode)
        //{}

        // Current node is a tile. Sensor matrix is 2 steps up the tree.
        TGeoMatrix *M = fGeoMgr->GetMotherMatrix(2);
        if (M)
        {
          double xyzglobal[3] = {hit.x, hit.y, hit.z};
          double xyzsensor[3] = {0,0,0};
          M->MasterToLocal(xyzglobal, xyzsensor);
          hit.xs = xyzsensor[0];
          hit.ys = xyzsensor[1];
          hit.zs = xyzsensor[2];
        }

        if (TMath::IsNaN(hit.x) || TMath::IsNaN(hit.y) || TMath::IsNaN(hit.z))
          return track;

        track.hits.push_back(hit);
        track.nhits++;

        // Print summary of this hit
        if (fVerbosity)
          Printf("   Node: %24s\txyz: (%#6.2f, %#6.2f, %#6.2f)\tstatus %d",
                 currentNode->GetName(), hit.x, hit.y, hit.z, hit.status);

        // Step into the next volume & update the TGeoManager.
        // But first adjust the angle if there is a B field
        if (fabs(magField) > 0. && layer < 3)
        {
          L = layerRadius[layer+1] - layerRadius[layer];
          if (R < L)
          {
            cout << PHWHERE << "Bend radius < projection distance. Exiting.\n"
                 << endl;
            return track;
          }
          d = R - TMath::Sqrt(R*R - L*L);
          phiKick = charge*TMath::ATan2(d, L);

          if (TMath::IsNaN(phiKick))
          {
            if (fVerbosity)
              cout << PHWHERE << "phi deflection is NaN! Exiting.\n" << endl;
            return track;
          }
          if (magField < 0)
          {
            phiKick *= -1.0;
          }

          nx = fGeoMgr->GetCurrentDirection()[0];
          ny = fGeoMgr->GetCurrentDirection()[1];
          double phi = TMath::ATan2(ny, nx) + phiKick;

          nx = TMath::Sin(the0)*TMath::Cos(phi);
          ny = TMath::Sin(the0)*TMath::Sin(phi);
          nz = TMath::Cos(the0);
          fGeoMgr->SetCurrentDirection(nx,ny,nz);
        }
      } // if (currentNode)
    } // if (TString(vol->GetName()).Contains("tile"))

    TGeoNode *nextNode = fGeoMgr->FindNextBoundaryAndStep();

    if (!nextNode)
    {
      if (fVerbosity) cout << endl;
      break;
    }
  }

  return track;
}

int
SvxComponentGeom::GetNearestSensorSegment(double x, double y, double z, SvxSensor *sensor)
{
  int seg = -1;
  if (!sensor)
  {
    sensor = fDetGeo->get_nearestSensor(x,y,z);
    if(sensor == NULL){
      cerr<<"SvxComponentGeom::"<<__FUNCTION__<<" : "
          <<"No nearest sensor was found (sensor object is NULL)"<<endl;
      return -1;
    }
  }

  int sensorLayer = sensor->get_layer();

  // 4 chips/sensor for pixel detector, 2 half-modules/sensor for stripixel
  int nSegs = (sensorLayer < 2) ? 4 : 2;
  double segHalfWidth = -1.;
  double /*sx = 0, sy = 0,*/ sz = 0;
  //--sx = sensor->get_correctedTransVector(0);
  //--sy = sensor->get_correctedTransVector(1);
  sz = sensor->get_correctedTransVector(2);

  // Compute halfwidth of sensor
  int readout = 0; // This remains zero
  double zHalfWidth = 0;
  for (int isc=0; isc < sensor->get_nSection(); isc++)
    zHalfWidth += sensor->get_zhalfWidth(isc,readout);

  // Compute halfwidth of segment
  segHalfWidth = (sensorLayer < 2) ? zHalfWidth/4 : zHalfWidth/2;

  double pixelZOffsets[4] = {-3,-1,1,3};
  double stripZOffsets[2] = {-1,1};

  if (sensorLayer < 2)
  {
    double dzmin = 123456789.;
    for (int i=0; i<nSegs; i++)
    {
      double dz = sz + pixelZOffsets[i]*segHalfWidth - z;
      dz *= dz;
      if (dz < dzmin)
      {
        dzmin = dz;
        seg = i;
      }
    }
  }
  else
  {
    double dzmin = 123456789.;
    for (int i=0; i<nSegs; i++)
    {
      double dz = (sz + stripZOffsets[i]*segHalfWidth - z);
      dz *= dz;
      if (dz < dzmin)
      {
        dzmin = dz;
        seg = i;
      }
    }
  }

  return seg;
}

void
SvxComponentGeom::PrintMask(unsigned int number)
{
  // Print binary representation of number to stdout.

  if (number <= 1)
  {
    cout << number;
    return;
  }

  int remainder = number%2;
  PrintMask(number >> 1);
  cout << remainder;
}
