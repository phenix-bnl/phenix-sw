
#include "SvxTGeo.h"

#include <TMath.h>
#include <TString.h>
#include <TSystem.h>
#include <TGeoMaterial.h>
#include <TGeoMedium.h>
#include <TGeoVolume.h>
#include <TGeoManager.h>
#include <TGeoNavigator.h>
#include <TGeoMatrix.h>
#include <TPolyLine.h>
#include <TVectorD.h>
#include <TMatrixD.h>

#include <string>
#include <iostream>
#include <fstream>
#include <istream>
#include <iterator>
#include <sstream>
#include <cassert>

#if !defined(__CINT__)
ClassImp(SvxTGeo);
#endif

using namespace std;

SvxTGeo::SvxTGeo() :
  fGeoMgr(0),
  fTopVolume(0),
  fVacuumMaterial(0),
  fSiliconMaterial(0),
  fAluminumMaterial(0),
  fVacuumMedia(0),
  fSiliconMedia(0),
  fAluminumMedia(0),
  fNewGeo(true),
  fComponentId(0),
  fVerbosity(0),
  fColWidth(12),
  fPrec(5)
{
  if (gGeoManager)
  {
    cout << "NOTE: A TGeoManager already exists. The current TGeoManager "
         << gGeoManager->GetName()
         << " will be replaced now." << endl;
    gGeoManager = 0;
  }

  fGeoMgr = new TGeoManager("fGeoMgr", "PHENIX VTX Geometry");
}

SvxTGeo::~SvxTGeo()
{
  if (fNewGeo) // Wipe out only if newed in this class
    delete fGeoMgr;
}

void
SvxTGeo::InitMaterials()
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

TGeoVolume *
SvxTGeo::MakeTopVolume(double x, double y, double z)
{
  InitMaterials();
  fTopVolume = fGeoMgr->MakeBox("fTopVolume",fVacuumMedia,x,y,z); // cm
  fTopVolume->SetLineColor(kBlue);
  fGeoMgr->SetTopVolume(fTopVolume);
  fGeoMgr->SetTopVisible(true);
  return fTopVolume;
}

TGeoVolume *
SvxTGeo::MakeBox(double xhw, double yhw, double zhw, const char *name)
{
  static int id = 0;
  TGeoVolume *box = fGeoMgr->MakeBox(name ? name : Form("box%d", ++id),
                                     fSiliconMedia, xhw, yhw, zhw);
  return box;
}

void
SvxTGeo::AddVolume(TGeoVolume *parent, TGeoVolume *daughter,
                   double x, double y, double z,
                   double phi, double theta, double psi)
{
  TGeoRotation *rot = new TGeoRotation("rot", phi, theta, psi);
  TGeoCombiTrans *placement = new TGeoCombiTrans(x, y, z, rot);
  parent->AddNode(daughter, 1, placement);
  return;
}

void
SvxTGeo::AddLadder(int lyr, int ldr, double x, double y, double zoff,
                   double phi, double theta, double psi)
{
  // Add a sensor that was not in the original par file.
  // Phi, theta, and psi should be in radians.
  if (ldr < fNLadders[lyr])
  {
    Printf("SvxTGeo::AddLadder(): "
           "Provided ladder index %d must be larger than %d",
           ldr, fNLadders[lyr]-1);
    return;
  }

  for (int sns=0; sns<fNSensors[lyr]; sns++)
  {
    double xhw = fSensorXHW[lyr];
    double yhw = fSensorYHW[lyr];
    double zhw = fSensorZHW[lyr];
    double z = zoff + SensorNode(lyr,0,sns)->GetMatrix()->GetTranslation()[2];
    double r2d = 180./TMath::Pi();

    TGeoRotation *rot = new TGeoRotation("rot", r2d*phi, r2d*theta, r2d*psi);
    TGeoCombiTrans *placement = new TGeoCombiTrans(x, y, z, rot);
    TGeoVolume *vol = fGeoMgr->MakeBox(Form("sensor_%d_%d_%d",lyr,ldr,sns),
                                       fSiliconMedia, xhw, yhw, zhw);
    vol->SetLineColor(kRed);
    fTopVolume->AddNode(vol, 1, placement);
    indx[lyr][ldr][sns] = sensors.size();
    GBox s;
    s.x     = x;
    s.y     = y;
    s.z     = z;
    s.xhw   = xhw;
    s.yhw   = yhw;
    s.zhw   = zhw;
    s.phi   = phi;
    s.theta = theta;
    s.psi   = psi;
    for (int i = 0; i < 3; i++)
      for (int j = 0; j < 3; j++)
        s.R(i,j) = rot->GetRotationMatrix()[3*j + i];

    sensors.push_back(s);
  }

  fNLadders[lyr]++;
  if (x>0)
    fNLaddersW[lyr]++;
  else
    fNLaddersE[lyr]++;
  return;
}

void
SvxTGeo::AddSensor(int lyr, int ldr, int sns)
{
  GBox s = sensors[indx[lyr][ldr][sns]];
  TGeoVolume *vol = fGeoMgr->MakeBox(Form("sensor_%d_%d_%d",lyr,ldr,sns),
                                     fSiliconMedia, s.xhw, s.yhw, s.zhw);
  vol->SetLineColor(kGray+2);
  AddVolume(fTopVolume, vol, s.x, s.y, s.z, s.phi, s.theta, s.psi);
  return;
}

void
SvxTGeo::AddSensors()
{
  for (int lyr=0; lyr<fNLayers; lyr++)
    for (int ldr=0; ldr<fNLadders[lyr]; ldr++)
      for (int sns=0; sns<fNSensors[lyr]; sns++)
        AddSensor(lyr, ldr, sns);

  return;
}

TGeoNode *
SvxTGeo::SensorNode(int lyr, int ldr, int sns)
{
  return fTopVolume->GetNode(indx[lyr][ldr][sns]);
}

void
SvxTGeo::ReadParFile(const char *filename)
{
  // Expected format for variables at each line
  // ==========================================
  // 1: " SVX cage parameters:"
  // 2-8: Not used.
  // 9: " SVX barrel parameters:"
  // 10: # layers.
  // 11: Layer radii (well, fRadii[4] is stagger)
  // 12: Layer z-offset. Always zero.
  // 13: # sensors/ladder in layer i.
  // 14-16: Sensor x,y,z half-width.
  // 17-20: z-gap, x0add, dphi, tilt. Not used.
  // 21, 24, 27, 30: Number of arms in layers 0-3.
  // 22, 25, 28, 31: Number of ladders in E,W (or W,E???) arm.
  // 23, 26, 29, 32: Not used.
  // 33: " SVX barrel sensor rotation matrices and translation vectors:"
  // 34+: Sensor address and geometry.

  if (gSystem->AccessPathName(filename) != 0)
  {
    Printf("ERROR from SvxTGeo::ReadParFile(): "
           "PISA file %s not found.", filename);
    gSystem->Exit(-1);
  }

  int ln = 0;    // Line number in file
  int first = 0; // Line number of first sensor text block
  ifstream filein(filename);

  for (string line; getline(filein, line);)
  {
    ln++;

    // Tokenize line into a vector of std::strings.
    stringstream ss(line);
    istream_iterator<string> it(ss);
    istream_iterator<string> end;
    vector<string> words(it, end);
    vector<float> pars; // words --> pars (chars become 0.0)

    if (fVerbosity > 0)
      cout << line << endl;

    for (unsigned int i=0; i<words.size(); ++i)
      pars.push_back(atof(words[i].data()));

    if (ln < 34) // Cage and barrel parameters
    {
      if (ln== 6) fCage1 = pars;
      if (ln== 7) fCage2 = pars;
      if (ln==10) fNLayers = (int)pars[0];
      if (ln==11)
      {
        double r[5] = {2.63000, 5.13000, 11.76500, 16.68700, 1.21400};
        for (int i=0; i<fNLayers+1; i++)
          fRadii.push_back(pars[i] > 0 ? pars[i] : r[i]);
      }

      if (ln==13)
        for (int i=0; i<fNLayers; i++)
          fNSensors.push_back((int)pars[i]);

      if (ln==14)
      {
        double w[4] = {0.69600, 0.69600, 1.74500, 1.74500};
        for (int i=0; i<4; i++)
          fSensorXHW.push_back(pars[i] > 0 ? pars[i] : w[i]);
      }
      if (ln==15)
      {
        double w[4] = {0.01000, 0.01000, 0.03125, 0.03125};
        for (int i=0; i<4; i++)
          fSensorYHW.push_back(pars[i] > 0 ? pars[i] : w[i]);
      }
      if (ln==16)
      {
        double w[4] = {2.83600, 2.83600, 3.18770, 3.18770};
        for (int i=0; i<4; i++)
          fSensorZHW.push_back(pars[i] > 0 ? pars[i] : w[i]);
      }
      if (ln==17) fSensorZGap = pars;
      if (ln==18) fX0add = pars;
      if (ln==19) fDPhi  = pars;
      if (ln==20) fTilt  = pars;
      if (ln==21 || ln==24 || ln==27 || ln==30)
        fNArms.push_back((int)pars[0]);
      if (ln==22 || ln==25 || ln==28 || ln==31)
      {
        fNLaddersE.push_back((int)pars[0]); // E=0, W=1 or vice versa? ???
        fNLaddersW.push_back((int)pars[1]);
        fNLadders.push_back((int)pars[0] + (int)pars[1]);
      }
    }
    else // Individual sensor parameters
    {
      int lyr = -1, ldr = -1, sns = -1;
      if (words.size() == 4) // New sensor = new block of text
      {
        first = ln;
        lyr = (int)pars[1] - 1; // -1 to convert from PISA 1-based indexing
        ldr = (int)pars[2] - 1;
        sns = (int)pars[3] - 1;

        // This a map between HW address and index into std::vector.
        indx[lyr][ldr][sns] = sensors.size();

        GBox s;
        sensors.push_back(s);
      }
      else
      {
        // Set x, y, z
        if (ln == first+1)
        {
          sensors.back().x = pars[0];
          sensors.back().y = pars[1];
          sensors.back().z = pars[2];
        }

        // Set rotation matrix components
        if (ln == first+2)
        {
          sensors.back().R(0, 0) = pars[0];
          sensors.back().R(0, 1) = pars[1];
          sensors.back().R(0, 2) = pars[2];
        }
        if (ln == first+3)
        {
          sensors.back().R(1, 0) = pars[0];
          sensors.back().R(1, 1) = pars[1];
          sensors.back().R(1, 2) = pars[2];
        }
        if (ln == first+4)
        {
          sensors.back().R(2, 0) = pars[0];
          sensors.back().R(2, 1) = pars[1];
          sensors.back().R(2, 2) = pars[2];
        }
      }
    }
  }

  for (int lyr=0; lyr<fNLayers; lyr++)
    for (int ldr=0; ldr<fNLadders[lyr]; ldr++)
      for (int sns=0; sns<fNSensors[lyr]; sns++)
      {
        int i = indx[lyr][ldr][sns];
        GBox s = sensors[i];

        sensors[i].xhw = fSensorXHW[lyr];
        sensors[i].yhw = fSensorYHW[lyr];
        sensors[i].zhw = fSensorZHW[lyr];

        double r2d = 180./TMath::Pi();
        sensors[i].phi   = r2d*TMath::ATan2(s.R(0,1), s.R(0,0));
        sensors[i].theta = r2d*TMath::ATan2(s.R(1,2), s.R(2,2));
        sensors[i].psi   = 0;
      }

  return;
}

void
SvxTGeo::WritePar(ofstream &fs, const int par)
{
  fs << Form("%*d", fColWidth, par) << endl;
  return;
}

void
SvxTGeo::WritePar(ofstream &fs, const float par)
{
  fs << Form("%*.*f", fColWidth, fPrec, par) << endl;
  return;
}

void
SvxTGeo::WritePars(ofstream &fs, vector<int> &vec)
{
  for (unsigned int i=0; i<vec.size(); i++)
    fs << Form("%*d", fColWidth, vec[i]);
  fs << endl;
  return;
}

void
SvxTGeo::WritePars(ofstream &fs, vector<float> &vec)
{
  for (unsigned int i=0; i<vec.size(); i++)
    fs << Form("%*.*f", fColWidth, fPrec, vec[i]);
  fs << endl;
  return;
}

void
SvxTGeo::WriteParFile(const char *name)
{
  ofstream fs(name);
  WriteGlobalPars(fs);
  WriteBarrelPars(fs);
  WriteSensorPars(fs);
  fs.close();
  return;
}

void
SvxTGeo::WriteGlobalPars(ofstream &fs)
{
  vector<float> zeros3(3,0.0);
  vector<float> tmp;
  tmp.push_back(0.5);
  tmp.push_back(0.2);

  fs << " SVX cage parameters:" << endl;
  WritePar(fs, (float)2.2);
  WritePars(fs, tmp);
  WritePar(fs, (float)0.0);
  WritePar(fs, (int)6);
  WritePars(fs, fCage1);
  WritePars(fs, fCage2);
  WritePars(fs, zeros3);
  return;
}

void
SvxTGeo::WriteBarrelPars(ofstream &fs)
{
  vector<float> zeros4(4,0.0);

  fs << " SVX barrel parameters:" << endl;
  WritePar(fs, fNLayers);
  WritePars(fs, fRadii);
  WritePars(fs, zeros4);
  WritePars(fs, fNSensors);
  WritePars(fs, fSensorXHW);
  WritePars(fs, fSensorYHW);
  WritePars(fs, fSensorZHW);
  WritePars(fs, fSensorZGap);
  WritePars(fs, fX0add);
  WritePars(fs, fDPhi);
  WritePars(fs, fTilt);

  // Ladders per arm
  for (int i=0; i<fNLayers; i++)
  {
    fs << Form("%*d", fColWidth, fNArms[i]) << endl;
    fs << Form("%*d", fColWidth, fNLaddersE[i])
       << Form("%*d", fColWidth, fNLaddersW[i])
       << endl;

    // Hard-coded / never used in svxDetectorGeo.
    fs << Form("%*.*f", fColWidth, fPrec, 0.)
       << Form("%*.*f", fColWidth, fPrec, 180.) << endl;
  }

  return;
}

void
SvxTGeo::WriteSensorPars(ofstream &fs)
{
  fs << " SVX barrel sensor rotation matrices and translation vectors:" << endl;

  for (int lyr = 0; lyr < fNLayers; lyr++)
    for (int ldr = 0; ldr < fNLadders[lyr]; ldr++)
      for (int sns = 0; sns < fNSensors[lyr]; sns++)
      {
        TGeoNode *sn = SensorNode(lyr,ldr,sns);
        assert(sn);
        TGeoMatrix *M = sn->GetMatrix();
        assert(M);
        float x = M->GetTranslation()[0];
        float y = M->GetTranslation()[1];
        float z = M->GetTranslation()[2];

        fs << Form("%*d%*d%*d%*d",
                   fColWidth, 0,
                   fColWidth, lyr+1,
                   fColWidth, ldr+1,
                   fColWidth, sns+1) << endl;

        fs << Form("%*.*f%*.*f%*.*f",
                   fColWidth, fPrec, x,
                   fColWidth, fPrec, y,
                   fColWidth, fPrec, z) << endl;

        for (int i = 0; i < 3; i++)
        {
          for (int j = 0; j < 3; j++)
          {
            float rij = M->GetRotationMatrix()[3*j + i];
            fs << Form("%*.*f", fColWidth, fPrec, rij);
          }
          fs << endl;
        }
      }

  return;
}

void
SvxTGeo::GetSensorXYZ(int lyr, int ldr, int sns, double *xyz)
{
  TGeoNode *sn = SensorNode(lyr,ldr,sns);
  assert(sn);
  TGeoMatrix *M = sn->GetMatrix();
  assert(M);
  xyz[0] = M->GetTranslation()[0];
  xyz[1] = M->GetTranslation()[1];
  xyz[2] = M->GetTranslation()[2];
}

void
SvxTGeo::TranslateLadder(int layer, int ladder, float x, float y, float z)
{
  // Move ladder from current position by x,y,z.
  for (int i=0; i<fNSensors[layer]; i++)
  {
    TGeoMatrix *m = SensorNode(layer, ladder, i)->GetMatrix();
    m->SetDx(m->GetTranslation()[0] + x);
    m->SetDy(m->GetTranslation()[1] + y);
    m->SetDz(m->GetTranslation()[2] + z);
  }
  return;
}

void
SvxTGeo::MoveLadderRadially(int layer, int ladder, float dr /*cm*/)
{
  // Change radial position by dr.
  for (int i=0; i<fNSensors[layer]; i++)
  {
    float phi = SensorPhiRad(layer, ladder, i);
    float dx = dr*TMath::Cos(phi);
    float dy = dr*TMath::Sin(phi);

    TGeoMatrix *m = SensorNode(layer, ladder, i)->GetMatrix();
    m->SetDx(m->GetTranslation()[0] + dx);
    m->SetDy(m->GetTranslation()[1] + dy);
  }
}

void
SvxTGeo::RotateLadder(int layer, int ladder, 
                      float aboutx, float abouty, float aboutz)
{
  // Rotate from current position about center of VTX.
  // aboutx, abouty, and aboutz are angles w.r.t. the x,y,z axes in radians.
  for (int i=0; i<fNSensors[layer]; i++)
  {
    TGeoMatrix *m = SensorNode(layer, ladder, i)->GetMatrix();
    m->RotateX(180./TMath::Pi() * aboutx);
    m->RotateY(180./TMath::Pi() * abouty);
    m->RotateZ(180./TMath::Pi() * aboutz);
  }
  return;
}

void
SvxTGeo::RotateLadderRPhi(int layer, int ladder, float rphi)
{
  // Rotate from current position about beamline.
  // Rotate by dphi as calculated from s = R*phi (cm).
  for (int sensor=0; sensor<fNSensors[layer]; sensor++)
  {
    float r = SensorRadius(layer, ladder, sensor);
    float dphi = TMath::ATan2(rphi,r) * 180./TMath::Pi();
    TGeoMatrix *m = SensorNode(layer, ladder, sensor)->GetMatrix();
    m->RotateZ(dphi); // dphi in degrees
  }
  return;
}

void
SvxTGeo::TranslateHalfLayer(int layer, int arm, float x, float y, float z)
{
  int first = -1, last  = -1;
  LadderRange(layer, arm, first, last);

  for (int ldr = first; ldr <= last; ldr++)
    TranslateLadder(layer, ldr, x, y, z);

  return;
}

void
SvxTGeo::TranslateArm(int arm, float x, float y, float z)
{
  for (int lyr=0; lyr<fNLayers; lyr++)
    TranslateHalfLayer(lyr, arm, x, y, z);

  return;
}

void
SvxTGeo::RotateHalfLayer(int layer, int arm,
                         float aboutx, float abouty, float aboutz)
{
  // The parameters aboutx (theta), abouty, and aboutz (phi) are the angles
  // in RADIANS about the x, y, and z axes.
  // Consider a boat or plane pointed in the +z direction:
  // aboutx = pitch; abouty = yaw; aboutz = roll.

  int first = -1, last  = -1;
  LadderRange(layer, arm, first, last);

  for (int ldr = first; ldr <= last; ldr++)
    RotateLadder(layer, ldr, aboutx, abouty, aboutz);
  return;
}

void
SvxTGeo::RotateHalfLayerRPhi(int layer, int arm, float rphi)
{
  // Rotate half-layer from current position about beamline.
  // Rotate by dphi as calculated from s = R*phi (cm).
  int first = -1, last  = -1;
  LadderRange(layer, arm, first, last);

  for (int ldr = first; ldr <= last; ldr++)
    RotateLadderRPhi(layer, ldr, rphi);

  return;
}

void
SvxTGeo::RotateArm(int arm, float aboutx, float abouty, float aboutz)
{
  // The parameters aboutx (theta), abouty, and aboutz (phi) are the angles
  // in RADIANS about the x, y, and z axes.
  // Consider a boat or plane pointed in the +z direction:
  // aboutx = pitch; abouty = yaw; aboutz = roll.

  for (int lyr=0; lyr<fNLayers; lyr++)
    RotateHalfLayer(lyr, arm, aboutx, abouty, aboutz);

  return;
}

void
SvxTGeo::LadderRange(int layer, int arm, int &first, int &last)
{
  // Assign index of first and last ladder in this layer and arm.
  if (arm == 0) // East
  {
    first = GetNLadders(layer)/2;
    last  = GetNLadders(layer) - 1;
  }
  else if (arm == 1) // West
  {
    first = 0;
    last  = GetNLadders(layer)/2 - 1;
  }
  else
    Printf("WARNING from SvxTGeo::LadderRange(): Ladders not assigned!");
  return;
}

float
SvxTGeo::LayerRadius(int layer)
{
  return fRadii.at(layer);
}

float
SvxTGeo::SensorRadius(int layer, int ladder, int sensor)
{
  // Return cylindrical distance from beamline in cm.
  TGeoMatrix *m = SensorNode(layer, ladder, sensor)->GetMatrix();
  float x = m->GetTranslation()[0];
  float y = m->GetTranslation()[1];
  float r = TMath::Sqrt(x*x + y*y);
  return r;
}

float
SvxTGeo::SensorPhiRad(int layer, int ladder, int sensor)
{
  // Return phi position of sensor in radians.
  TGeoMatrix *m = SensorNode(layer, ladder, sensor)->GetMatrix();
  float x = m->GetTranslation()[0];
  float y = m->GetTranslation()[1];

  return TMath::ATan2(y,x);
}

float
SvxTGeo::SensorPhiDeg(int layer, int ladder, int sensor)
{
  // Return phi position of sensor in degrees.
  return SensorPhiRad(layer, ladder, sensor) * 180./TMath::Pi();
}

float
SvxTGeo::GetLadderPhiTilt(int layer, int ladder)
{
  TGeoMatrix *m = SensorNode(layer, ladder, 0)->GetMatrix();
  return TMath::ACos(m->GetRotationMatrix()[0]);
}

TPolyLine *
SvxTGeo::LadderOutlineXY(int lyr, int ldr)
{
  TGeoMatrix *M = SensorNode(lyr, ldr, 0)->GetMatrix();
  assert(M);
  double xhw=fSensorXHW[lyr], yhw=fSensorYHW[lyr];

  // Ladder corners:
  // d------c
  // |      |
  // a------b
  TVectorD a(2);
  TVectorD b(2);
  TVectorD c(2);
  TVectorD d(2);
  a(0) = -xhw; a(1) = -yhw;
  b(0) = +xhw; b(1) = -yhw;
  c(0) = +xhw; c(1) = +yhw;
  d(0) = -xhw; d(1) = +yhw;

  // Rotate corner positions
  TMatrixD R(2,2);
  R(0,0) = M->GetRotationMatrix()[0]; // cos
  R(0,1) = M->GetRotationMatrix()[1]; // -sin
  R(1,0) = M->GetRotationMatrix()[3]; // sin
  R(1,1) = M->GetRotationMatrix()[4]; // cos
  a *= R; b *= R; c *= R; d *= R;

  // Translate
  TVectorD r(2);
  r(0) = M->GetTranslation()[0];
  r(1) = M->GetTranslation()[1];
  a += r; b += r; c += r; d += r;

  Double_t xarr[5] = {a(0),b(0),c(0),d(0),a(0)};
  Double_t yarr[5] = {a(1),b(1),c(1),d(1),a(1)};
  return new TPolyLine(5,xarr,yarr);
}
