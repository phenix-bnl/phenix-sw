#include <iostream>
#include <fstream>
#include <cmath>

#include "svxDetectorGeo.hh"
#include "SvxSensor.h"
#include "SvxPixel1v1.h"
#include "SvxStrip11v2.h"

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCoordinate.hh>
#include <PdbSvxCoordinateOffset.hh>
#include <PdbCalBank.hh>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PHGeometry.h>
#include <PHTimeStamp.h>

#include <RunToTime.hh>

using namespace std;
using namespace PHGeometry;

int svxDetectorGeo::m_count = 0;

// Default Constructor for svxDetectorGeo
svxDetectorGeo::svxDetectorGeo()
{
  Verbose = 0;

  for (int i = 0; i < SVXLAYERNUMBER; i++) { nLadders[i]=0; }

  for (int i = 0; i < SVXLAYERNUMBER; i++) {
    for (int j = 0; j < SVXLADDERNUMBER; j++) {
      svxActive[0][i][j] = 0; //east
      svxActive[1][i][j] = 0; //west
    }
  }

  xyz0.setX(0.0);
  xyz0.setY(0.0);
  xyz0.setZ(0.0);

  for (int i = 0; i < 2; i++) { Theta0[i] = 0.0; }

  for(int ilayer=0; ilayer<SVXLAYERNUMBER; ilayer++){
    svxRadius[ilayer] = 0.0;
    svxZWidth[ilayer] = 0.0;
  }

  Tsearch.setToSystemTime();

  TUpdateStart.set(2001, 5, 1, 0, 0, 0, 0); // beginning of run2
  TUpdateStop.setToFarFuture();

  m_geoBankId_For_Oldproduction = 0; // default value is 0. the value is used when value is not 0, 
  m_offsetBankId_For_Oldproduction = 0; // default value is 0. the value is used when value is not 0, 

  barSenType[0] = 1; // sensor type pixel
  barSenType[1] = 1; // sensor type pixel
  barSenType[2] = 11; // sensor type stripixel
  barSenType[3] = 11; // sensor type stripixel

  // setup default frames
  PHFrame XYZ;  //this is a default frame
  phenixFrame = XYZ;

  for (int ilayer = 0; ilayer < SVXLAYERNUMBER; ilayer++) {
    for (int iladder = 0; iladder < SVXLADDERNUMBER; iladder++) {
      svxLadderFrame[ilayer][iladder] = XYZ;
    }
  }
  
  is_SensorCreated = false;
  
  /////////////////////////////////////
  // instance SvxSensor added by T.Hachiya 2011.12.23
  barSensor = new SvxSensor* [SVXLAYERNUMBER][SVXLADDERNUMBER][SVXSENSORNUMBER];
  for (unsigned int ilr=0; ilr<SVXLAYERNUMBER; ilr++) {
    for (unsigned int ild=0; ild<SVXLADDERNUMBER; ild++) {
      for (unsigned int isn=0; isn<SVXSENSORNUMBER; isn++) {
        if (barSenType[ilr] ==  1) {
	  barSensor[ilr][ild][isn] = new SvxPixel1v1(0,ilr,ild,isn);
	} 
        else if (barSenType[ilr] == 11) { 
	  barSensor[ilr][ild][isn] = new SvxStrip11v2(0,ilr,ild,isn);
	} 
        else {
	  barSensor[ilr][ild][isn] = NULL;
          cerr << PHWHERE << " ERROR: Unknown sensor type " << barSenType[ilr] << endl;
          exit(EXIT_FAILURE);
        }


        //obtain active area on the sensor
        if(ild==0&&isn==0){
          svx_sensor_xsize_active[ilr] = barSensor[ilr][ild][isn]->get_xhalfWidth(0,0);
         
          svx_sensor_zsize_active[ilr]=0.;
          for(int isec=0; isec<barSensor[ilr][ild][isn]->get_nSection(); isec++){
            for(int ird=0; ird<barSensor[ilr][ild][isn]->get_nReadout(); ird++){
              svx_sensor_zsize_active[ilr]+=barSensor[ilr][ild][isn]->get_zhalfWidth(isec, ird);
            }
          }
        }

      }
    }
  }
  nBarLadder[0] = 2*SVXLADDERSLAYER0;
  nBarLadder[1] = 2*SVXLADDERSLAYER1;
  nBarLadder[2] = 2*SVXLADDERSLAYER2;
  nBarLadder[3] = 2*SVXLADDERSLAYER3;
  nBarSensor[0] = SVXSENSORSLAYER0;
  nBarSensor[1] = SVXSENSORSLAYER1;
  nBarSensor[2] = SVXSENSORSLAYER2;
  nBarSensor[3] = SVXSENSORSLAYER3;
  
  /////////////////////////////////////

  /// nominal radius of sublayers :  svxPISA.par.ideal Revision 1.5
  SvxRsublayer[0] = 2.63;  // radius of sublayer 0      
  SvxRsublayer[1] = 5.13;  // radius of sublayer 1      
  SvxRsublayer[2] = 10.365; // radius of sublayer 2     
  SvxRsublayer[3] = 11.765; // radius of sublayer 3     
  SvxRsublayer[4] = 12.845; // radius of sublayer 4     
  SvxRsublayer[5] = 15.475; // radius of sublayer 5     
  SvxRsublayer[6] = 16.687; // radius of sublayer 6     
  SvxRsublayer[7] = 17.905; // radius of sublayer 7     

  m_count++;
  if(m_count>1){
    cerr<<"You are using Nth="<<m_count<<" svxDetectorGeo."<<endl;
    cerr<<"Should use common svxDetectorGeo object."<<endl;
  }

}

// Destructor for svxDetectorGeo
svxDetectorGeo::~svxDetectorGeo()
{
  for ( unsigned int ilr=0; ilr<SVXLAYERNUMBER; ilr++ )  {
    for ( unsigned int ild=0; ild<SVXLADDERNUMBER; ild++ ) {
      for ( unsigned int isn=0; isn<SVXSENSORNUMBER; isn++ ) {
        if ( barSensor[ilr][ild][isn] ) { delete barSensor[ilr][ild][isn]; }
      }
    }
  }    
  delete [] barSensor;

  m_count--;

  if(m_count<0){
  }
}

short
svxDetectorGeo::get_svxGeo(short arm, short layer, PHPanel outPlane[])
{
  short ladd, module;
  module = 0;

  for (ladd = 0; ladd < nLadders[layer]; ladd++)
    {
      outPlane[module] = svxLadders[arm][layer][ladd];
      module++;
    }


  return module;
}

// Fetch the geometry information from the default file svxGeometry.txt
PHBoolean
svxDetectorGeo::FetchFromFile()
{
  return FetchFromFile("svxGeometry.txt"); /// \todo this file should be installed to OFFLINE_MAIN/share and opened from there if there is no file with that name in the PWD.
}

// Fetch the geometry information from an ASCII file
PHBoolean
svxDetectorGeo::FetchFromFile(const char* filename)
{
  /// \todo Evaluate what we really need here for the future. Resurrect this or just remove all of it?
  return True;
}

// Put the geometry information into the default file svxGeometry.txt
PHBoolean svxDetectorGeo::PutIntoFile() {
  return PutIntoFile("svxGeometry.txt");
}

// Put the geometry information into an ASCII file
PHBoolean svxDetectorGeo::PutIntoFile(const char* filename) {

  return True;
}

void
svxDetectorGeo::set_Verbose(short setverb)
{
  Verbose = setverb;
}

short
svxDetectorGeo::get_Verbose()
{
  return Verbose;
}

void
svxDetectorGeo::set_svxActive(const short arm, const short layer, short svxin[])
{
  int i;
  for (i = 0; i < SVXLADDERNUMBER; i++)
    {
      svxActive[arm][layer][i] = svxin[i];
    }
}

void
svxDetectorGeo::get_svxActive(const short arm, const short layer, short svxout[])
{
  int i;
  for (i = 0; i < SVXLADDERNUMBER; i++)
    {
      svxout[i] = svxActive[arm][layer][i];
    }
}

void
svxDetectorGeo::set_svxRadius(const short layer, double svxrad)
{
  svxRadius[layer] = svxrad;
}

double
svxDetectorGeo::get_svxRadius(const short layer)
{
  return svx_layer_rpos[layer];
}

void
svxDetectorGeo::set_svxZWidth(const short layer, double svxz)
{
  svxZWidth[layer] = svxz;
}

double
svxDetectorGeo::get_svxZWidth(const short layer)
{
  return svxZWidth[layer];
}

void
svxDetectorGeo::set_xOffset(double xOffsetIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      xOffset[i] = xOffsetIn[i];
    }
}

void
svxDetectorGeo::get_xOffset(double xOffsetOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      xOffsetOut[i] = xOffset[i];
    }
}

void
svxDetectorGeo::set_zOffset(double zOffsetIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      zOffset[i] = zOffsetIn[i];
    }
}

void
svxDetectorGeo::get_zOffset(double zOffsetOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      zOffsetOut[i] = zOffset[i];
    }
}

void
svxDetectorGeo::set_gasAtten(double gasAttenIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      gasAtten[i] = gasAttenIn[i];
    }
}

void
svxDetectorGeo::get_gasAtten(double gasAttenOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      gasAttenOut[i] = gasAtten[i];
    }
}

void
svxDetectorGeo::set_anodeSpacing(double anodeSpacingIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      anodeSpacing[i] = anodeSpacingIn[i];
    }
}

void
svxDetectorGeo::get_anodeSpacing(double anodeSpacingOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      anodeSpacingOut[i] = anodeSpacing[i];
    }
}

void
svxDetectorGeo::set_pixelLength(double pixelLengthIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      pixelLength[i] = pixelLengthIn[i];
    }
}

void
svxDetectorGeo::get_pixelLength(double pixelLengthOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      pixelLengthOut[i] = pixelLength[i];
    }
}

void
svxDetectorGeo::set_sideWidth(double sideWidthIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      sideWidth[i] = sideWidthIn[i];
    }
}

void
svxDetectorGeo::get_sideWidth(double sideWidthOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      sideWidthOut[i] = sideWidth[i];
    }
}

void
svxDetectorGeo::set_centerWidth(double centerWidthIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      centerWidth[i] = centerWidthIn[i];
    }
}

void
svxDetectorGeo::get_centerWidth(double centerWidthOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      centerWidthOut[i] = centerWidth[i];
    }
}

void
svxDetectorGeo::set_pixelSpacing(double pixelSpacingIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      pixelSpacing[i] = pixelSpacingIn[i];
    }
}

void
svxDetectorGeo::get_pixelSpacing(double pixelSpacingOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      pixelSpacingOut[i] = pixelSpacing[i];
    }
}

void
svxDetectorGeo::set_cellSpacing(double cellSpacingIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      cellSpacing[i] = cellSpacingIn[i];
    }
}

void
svxDetectorGeo::get_cellSpacing(double cellSpacingOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      cellSpacingOut[i] = cellSpacing[i];
    }
}

void
svxDetectorGeo::set_z0Gap(double z0GapIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      z0Gap[i] = z0GapIn[i];
    }
}

void
svxDetectorGeo::get_z0Gap(double z0GapOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      z0GapOut[i] = z0Gap[i];
    }
}

void
svxDetectorGeo::set_nWiresPerLadd(short nWiresPerLaddIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      nWiresPerLadd[i] = nWiresPerLaddIn[i];
    }
}

void
svxDetectorGeo::get_nWiresPerLadd(short nWiresPerLaddOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      nWiresPerLaddOut[i] = nWiresPerLadd[i];
    }
}

void
svxDetectorGeo::set_nSvxsAcrossWire(short nSvxsAcrossWireIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      nSvxsAcrossWire[i] = nSvxsAcrossWireIn[i];
    }
}

void
svxDetectorGeo::get_nSvxsAcrossWire(short nSvxsAcrossWireOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      nSvxsAcrossWireOut[i] = nSvxsAcrossWire[i];
    }
}

void
svxDetectorGeo::set_nSvxsAlongWire(short nSvxsAlongWireIn[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      nSvxsAlongWire[i] = nSvxsAlongWireIn[i];
    }
}

void
svxDetectorGeo::get_nSvxsAlongWire(short nSvxsAlongWireOut[])
{
  for (int i = 0; i < SVXLAYERNUMBER; i++)
    {
      nSvxsAlongWireOut[i] = nSvxsAlongWire[i];
    }
}

void
svxDetectorGeo::set_xyz0(const PHPoint& inPoint)
{
  xyz0 = inPoint;
}

void
svxDetectorGeo::set_xyz0(double inPoint[])
{
  xyz0.setX(inPoint[0]);
  xyz0.setY(inPoint[1]);
  xyz0.setZ(inPoint[2]);
}

PHPoint
svxDetectorGeo::get_xyz0()
{
  return xyz0;
}

void
svxDetectorGeo::set_Theta0(PHAngle thetain[])
{
  Theta0[0] = thetain[0];
  Theta0[1] = thetain[1];
}

void
svxDetectorGeo::get_Theta0(PHAngle thetaout[])
{
  thetaout[0] = Theta0[0];
  thetaout[1] = Theta0[1];
}

void
svxDetectorGeo::set_Theta0(double thetain[])
{
  Theta0[0] = thetain[0];
  Theta0[1] = thetain[1];
}

void
svxDetectorGeo::get_Theta0(double thetaout[])
{
  thetaout[0] = double(Theta0[0]);
  thetaout[1] = double(Theta0[1]);
}

void
svxDetectorGeo::set_PhiTop(float phiin[])
{
  PhiTop[0] = phiin[0];
  PhiTop[1] = phiin[1];
}

void
svxDetectorGeo::get_PhiTop(float phiout[])
{
  phiout[0] = PhiTop[0];
  phiout[1] = PhiTop[1];
}

void
svxDetectorGeo::set_PhiBottom(float phiin[])
{
  PhiBottom[0] = phiin[0];
  PhiBottom[1] = phiin[1];
}

void
svxDetectorGeo::get_PhiBottom(float phiout[])
{
  phiout[0] = PhiBottom[0];
  phiout[1] = PhiBottom[1];
}

// Print out the constructed geometry
void
svxDetectorGeo::PrintGeo(short layer)
{
  int i;
  double theta[2];
  // Print overall information
  cout << "svxDetectorGeo:\n";
  cout << "  Verbose = " << Verbose << "\n";
  cout << "  Global Origin:\n";
  xyz0.print();
  theta[0] = Theta0[0];
  theta[1] = Theta0[1];
  cout << "  Theta0: layer 0 = " << theta[0] << " , layer 1 = " <<
    theta[1] << "\n";

  // Print SVX information
  cout << "  svxDetectorGeo for SVX:\n";
  cout << "   Active ladders for layer " << layer << ": ";
  for (i = 0; i < SVXLADDERNUMBER; i++)
    {
      cout << svxActive[0][layer][i] << ","; //east
      cout << svxActive[1][layer][i] << ","; //west
    }
  cout << "\n";
  cout << "   Generated planes for layer " << layer << ":\n";
  for (i = 0; i < SVXLADDERNUMBER; i++)
    {
      if (svxActive[0][layer][i] == 1) {
	cout << "0-" << i << " "; svxLadders[0][layer][i].print(); //east
      }
      if (svxActive[1][layer][i] == 1) {
	cout << "1-" << i << " "; svxLadders[1][layer][i].print(); //west
      }
    }
}

// Print the parameter information from dSvxGeom
void
svxDetectorGeo::PrintParams()
{
  int i;
  cout << "svxDetectorGeo Parameters: \n";
  for (i = 0; i < SVXLAYERNUMBER; i++)
    {
      cout << "  Svx Layer " << i << ":\n";
      cout << "    xOffset = " << xOffset[i] << "\n";
      cout << "    zOffset = " << zOffset[i] << "\n";
      cout << "    gasAtten = " << gasAtten[i] << "\n";
      cout << "    anodeSpacing = " << anodeSpacing[i] << "\n";
      cout << "    pixelLength = " << pixelLength[i] << "\n";
      cout << "    sideWidth = " << sideWidth[i] << "\n";
      cout << "    centerWidth = " << centerWidth[i] << "\n";
      cout << "    pixelSpacing = " << pixelSpacing[i] << "\n";
      cout << "    cellSpacing = " << cellSpacing[i] << "\n";
      cout << "    z0Gap = " << z0Gap[i] << "\n";
      cout << "    nWiresPerLadd = " << nWiresPerLadd[i] << "\n";
      cout << "    nSvxsAcrossWire = " << nSvxsAcrossWire[i] << "\n";
      cout << "    nSvxsAlongWire = " << nSvxsAlongWire[i] << "\n";
    }
}

// additions by Julia Velkovska to apply geometry transformations
void
svxDetectorGeo::set_phenixFrame(PHFrame& ph)
{
  phenixFrame = ph;
}

PHFrame
svxDetectorGeo::get_phenixFrame()
{
  return phenixFrame;
}

void
svxDetectorGeo::set_svxFrame(PHFrame& ph, short &svxlayer, short &svxladder)
{
  svxLadderFrame[svxlayer][svxladder] = ph;
}

PHFrame
svxDetectorGeo::get_svxFrame(short &svxlayer, short &svxladder)
{
  return svxLadderFrame[svxlayer][svxladder];
}

double svxDetectorGeo::get_SensorCenter(int ilr, int ild, int isn, int icoord) {
  double xyz=0.;
  if((icoord>=0 && icoord<=2) && (ilr>=0 && ilr<4) && (ild>=0 && ild<SVXLADDERNUMBER) && (isn>=0 && isn<SVXSENSORNUMBER)) {
    //    xyz = barSensor[ilr][ild][isn]->get_transVector(icoord);
    xyz = barSensor[ilr][ild][isn]->get_correctedTransVector(icoord);
  }
  else {
    cerr << "svxDetectorGeo::get_SensorCenter() ERROR getting coordinate." << endl; 
  }
  return xyz;
}

PHBoolean svxDetectorGeo::Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend) {
  return True;
}

PHBoolean svxDetectorGeo::Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend, int ilayer) {
  if(ilayer>3 || ilayer<0) return False;

  PHBoolean success = True;
  PHTimeStamp Tstart = *Tbeg;
  PHTimeStamp Tstop  = *Tend;
  PdbCalBank *svxBank = 0;
  PdbBankID bankID;
  const char *description = "";
  const char *tableName = "";
  //const char *dummy = "SVX.GEOM.SIM.LAYER0";
  int nnn=0;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if(application->startUpdate()) {
    
    //bankID.set(dummy);
    bankID.setInternalValue(ilayer);
    if(ilayer==0) {
      tableName = "svx.geom.simlayer0";
      description = "Svx Geometry for Simulation, Layer 0";
      //dummy = "SVX.GEOM.SIM.LAYER0";
      nnn = SVXLADDERSLAYER0;
    }
    else if(ilayer==1) {
      tableName = "svx.geom.simlayer1";
      description = "Svx Geometry for Simulation, Layer 1";
      //dummy = "SVX.GEOM.SIM.LAYER1";
      nnn = SVXLADDERSLAYER1;
    }
    else if(ilayer==2) {
      tableName = "svx.geom.simlayer2";
      description = "Svx Geometry for Simulation, Layer 2";
      //dummy = "SVX.GEOM.SIM.LAYER2";
      nnn = SVXLADDERSLAYER2;
    } 
    else if(ilayer==3) { 
      tableName = "svx.geom.simlayer3";
      description = "Svx Geometry for Simulation, Layer 3";
      //dummy = "SVX.GEOM.SIM.LAYER3";
      nnn = SVXLADDERSLAYER3;
    }
    if(Verbose) {cout << "--------- layer " << ilayer << " -------------------" << endl;}
    if(Verbose) {cout << "tableName = " << tableName << endl;}
    if(Verbose) {cout << "description = " << description << endl;}
    if(Verbose) {cout << "bankID = " << bankID.getInternalValue() << endl;}
    cout << Tstart << " - " << Tstop << endl;
    
    svxBank = bankManager->createBank("PdbCoordinateBank", bankID, description, Tstart, Tstop, tableName);
    
    if(svxBank) {
      
      int banklength = nnn*SVXLAYERNUMBER*4*2; // 4 = number of PHPoints per PHPanel, 2 = number of arms
      if(Verbose) {cout << "bank length = " << banklength << endl;}
      svxBank->setLength(banklength); 
      PdbCoordinate* xyz = 0;
      
      for(int iladder=0; iladder<nnn; iladder++) { 
	
	// east (arm=0, negative X)
	if(Verbose) {cout << "   ladder # " << iladder << " east: " << endl;}
	for(int ipoint=0; ipoint<4; ipoint++) {
	  int iindex = iladder*4*2 + ipoint;
	  float X = ((svxLadders[0][ilayer][iladder]).getPoint(ipoint)).getX();
	  float Y = ((svxLadders[0][ilayer][iladder]).getPoint(ipoint)).getY();
	  float Z = ((svxLadders[0][ilayer][iladder]).getPoint(ipoint)).getZ();
	  if (Verbose) {cout << "              " << X << " " << Y << " " << Z << endl;}
	  xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
	  xyz->setParameter(0, X);
	  xyz->setParameter(1, Y);
	  xyz->setParameter(2, Z);
	  xyz->setParError (0, 0.);
	  xyz->setParError (1, 0.);
	  xyz->setParError (2, 0.);
	}

	// west (arm=1, positive X)
	if(Verbose) {cout << "   ladder # " << iladder << " west: " << endl;}
	for(int ipoint=0; ipoint<4; ipoint++) {
	  int iindex = iladder*4*2 + ipoint + 4;
	  float X = ((svxLadders[1][ilayer][iladder]).getPoint(ipoint)).getX();
	  float Y = ((svxLadders[1][ilayer][iladder]).getPoint(ipoint)).getY();
	  float Z = ((svxLadders[1][ilayer][iladder]).getPoint(ipoint)).getZ();
	  if(Verbose) {cout << "              " << X << " " << Y << " " << Z << endl;}
	  xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
	  xyz->setParameter(0, X);
	  xyz->setParameter(1, Y);
	  xyz->setParameter(2, Z);
	  xyz->setParError (0, 0.);
	  xyz->setParError (1, 0.);
	  xyz->setParError (2, 0.);
	}
	
	
      }
      
    }
    else {
      if(Verbose) {cerr << PHWHERE << " ERROR: can not create svxBank. layer = " << ilayer << endl;}
      success = False;
    }
  }
  else {
    if(Verbose) {cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;}
    success = False;
  }
  
  if(success) {application->commit();} else {application->abort();}
  
  if(svxBank) delete svxBank;
  
  if(Verbose) {PrintGeo(ilayer);}
  
  return success;
}

//------------------------------------------------------------------------------

PHBoolean svxDetectorGeo::Fetch() {
  return Fetch(&Tsearch);
}

PHBoolean svxDetectorGeo::Fetch(PHTimeStamp *T) {
  PHBoolean s0 = Fetch(T, 0);
  PHBoolean s1 = Fetch(T, 1);
  PHBoolean s2 = Fetch(T, 2);
  PHBoolean s3 = Fetch(T, 3);
  return s0*s1*s2*s3;                                                                                                   
}

//----------------------------------------------------------------------------------------------------

PHBoolean svxDetectorGeo::Fetch(PHTimeStamp *T, int ilayer) {
  if(ilayer>3 || ilayer<0) return False;
  
  PHBoolean success = True;
  PHTimeStamp TSearch = *T;
  PdbCoordinate* achan=0;
  PdbBankID bankID;
  PdbCalBank *svxBank = 0;
  PHPoint point[4];
  const char *tableName = "";
  int nnn=0;
  
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if(!application->startRead()) {
    application->abort();
    if (Verbose) {cerr << PHWHERE << " ERROR -> Transaction aborted. Database NOT available." << endl;}
    return False;
  }

  if(ilayer==0) {
    tableName = "svx.geom.simlayer0";
    nnn = SVXLADDERSLAYER0;
    nLadders[0]=SVXLADDERSLAYER0;
  }
  else if(ilayer==1) {
    tableName = "svx.geom.simlayer1";
    nnn = SVXLADDERSLAYER1;
    nLadders[1]=SVXLADDERSLAYER1;
  }
  else if(ilayer==2) {
    tableName = "svx.geom.simlayer2";
    nnn = SVXLADDERSLAYER2;
    nLadders[2]=SVXLADDERSLAYER2;
  }
  else if(ilayer==3) {
    tableName = "svx.geom.simlayer3";
    nnn = SVXLADDERSLAYER3;
    nLadders[3]=SVXLADDERSLAYER3;
  }
  
  bankID.setInternalValue(ilayer);
  if(Verbose) {cout << "--------- layer " << ilayer << " -------------------" << endl;}
  if(Verbose) {cout << "tableName = " << tableName << endl;}
  if(Verbose) {cout << "bankID = " << bankID.getInternalValue() << endl;}
  if(Verbose) {cout << Tsearch << endl;}
  
  svxBank = bankManager->fetchBank("PdbCoordinateBank", bankID, tableName, Tsearch);

  if(svxBank){
  
    int banklength = (int)svxBank->getLength();
    if(Verbose) {cout << "banklength = " << banklength << " " << nnn*SVXLAYERNUMBER*4*2 << endl;}
    if(banklength != nnn*SVXLAYERNUMBER*4*2) {
      if(Verbose) {cerr << PHWHERE << " ERROR: wrong bank length." << endl;}
      success = False;
    }
    
    if(success) {
      
      for(int iladder=0; iladder<nnn; iladder++) {
        
        // east (arm=0, negative X)
        svxActive[0][ilayer][iladder] = 1;
        for(int ipoint=0; ipoint<4; ipoint++) {
          int iindex = iladder*4*2 + ipoint;
          achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
          float X = achan->getParameter(0);
          float Y = achan->getParameter(1);
          float Z = achan->getParameter(2);
          (point[ipoint]).setX(X); 
          (point[ipoint]).setY(Y); 
          (point[ipoint]).setZ(Z); 
          if(Verbose) {
            cout << "   ladder # " << iladder << " east:        " << X << " " << Y << " " << Z << endl;
          }
        }
        PHPanel panelE(point[0], point[1], point[2]);
        svxLadders[0][ilayer][iladder] = panelE;
        
        // west (arm=1, positive X)
        svxActive[1][ilayer][iladder] = 1;
        for(int ipoint=0; ipoint<4; ipoint++) {
          int iindex = iladder*4*2 + ipoint + 4;
          achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
          float X = achan->getParameter(0);
          float Y = achan->getParameter(1);
          float Z = achan->getParameter(2);
          (point[ipoint]).setX(X);
          (point[ipoint]).setY(Y);
          (point[ipoint]).setZ(Z);
          if(Verbose) {
            cout << "   ladder # " << iladder << " west:        " << X << " " << Y << " " << Z << endl;
          }
        }
        PHPanel panelW(point[0], point[1], point[2]);
        svxLadders[1][ilayer][iladder] = panelW;
        
      }
      
    }
  }
  else {
    if(Verbose) {
      cerr << PHWHERE << " ERROR -> bankManager returned zero-pointer. layer = " << ilayer << endl;
    }
    success = False;
  }
  
  if(success) {application->commit();} else {application->abort();}
  
  if(svxBank) delete svxBank;
  
  if(Verbose) {PrintGeo(ilayer);}

  return success;
}

PHBoolean svxDetectorGeo::AdjustGeometry(int whichlayer, double* shiftx, double* shifty, double* shiftz,double* rotationtheta) {

  PHBoolean success = True;
  float rotradius=0;
  int ilr = whichlayer;
  for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {  // loop over ladders on both sides
    for (unsigned int isn=0; isn < nBarSensor[ilr]; isn++) {  // loop over sensors
      //svx_sensor_zsize:sensor half length + snzgap 
      if(whichlayer == 0 || whichlayer == 1){ //pixel
	rotradius = svx_sensor_zsize[ilr]*(2*isn-3);
      }
      if(whichlayer == 2){ //stripixel layer 2 # of sensor 5
	rotradius = svx_sensor_zsize[ilr]*(2*isn-4);
      } 
      if(whichlayer == 3){ //stripixel layer 3 # of sensor 6
	rotradius = svx_sensor_zsize[ilr]*(2*isn-5);
      }
      // Get sensor positions
      float x = barSensor[ilr][ild][isn]->get_transVector(0); // x,y,z are for the center of the sensor
      float y = barSensor[ilr][ild][isn]->get_transVector(1); // The size is defined by svx_sensor_*size data members
      float z = barSensor[ilr][ild][isn]->get_transVector(2); // Read from svxPISA.par file (or database)
      float rottmp[3][3];
      float rotadjust[3][3];
      for ( int i = 0; i < 3; i++ ) {
	for ( int j = 0; j < 3; j++ ) {
	  rottmp[i][j] = barSensor[ilr][ild][isn]->get_rotMatrix(i,j);
	}
      }
      
      // shifts and rotations are assumed to be the same for all sensors in a ladder
      rotadjust[0][0] = 1 ; rotadjust[0][1] = 0                      ; rotadjust[0][2] = 0;
      rotadjust[1][0] = 0 ; rotadjust[1][1] = cos(rotationtheta[ilr]); rotadjust[1][2] = -sin(rotationtheta[ilr]);
      rotadjust[2][0] = 0 ; rotadjust[2][1] = sin(rotationtheta[ilr]); rotadjust[2][2] = cos(rotationtheta[ilr]);
      
      for ( int i = 0; i < 3; i++ ) {
	for ( int j = 0; j < 3; j++ ) {
	  rottmp[i][j] += rotadjust[j][i]*rottmp[i][j];
	}
      }
      x += 0;
      y += -sin(rotationtheta[ilr])*rotradius;
      z += (cos(rotationtheta[ilr])-1.)*rotradius;
      
      x += shiftx[ild]; 
      y += shifty[ild]; 
      z += shiftz[ild]; 
      barSensor[ilr][ild][isn]->set_transVector(0,x);
      barSensor[ilr][ild][isn]->set_transVector(1,y);
      barSensor[ilr][ild][isn]->set_transVector(2,z);
      
    }
  } 
  
  return success;
}

PHBoolean svxDetectorGeo::CreatePanels() {
  
  PHPoint panelPoint[3];
  float tmpx,tmpy,tmpz;
  int icount=0;
  unsigned int tmpnumsn[4];
  tmpnumsn[0]=SVXSENSORSLAYER0; tmpnumsn[1]=SVXSENSORSLAYER1; tmpnumsn[2]=SVXSENSORSLAYER2; tmpnumsn[3]=SVXSENSORSLAYER3;
  unsigned int tmpnumld[4];
  tmpnumld[0]=SVXLADDERSLAYER0; tmpnumld[1]=SVXLADDERSLAYER1; tmpnumld[2]=SVXLADDERSLAYER2; tmpnumld[3]=SVXLADDERSLAYER3;
  
  // Create PHPanels for ladders (4,4,5,6 sensors per ladder)
  icount=0;
  for (unsigned int ilr = 0; ilr < svxSecNlayers; ilr++)  {     // loop over layers
    for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {  // loop over ladders
      
      // Get position and rotation angles
      // Rotation is assumed always the same for all sensors in a ladder (sensor 0 is used).
      float x = barSensor[ilr][ild][0]->get_transVector(0); // Center of the sensor
      float y = barSensor[ilr][ild][0]->get_transVector(1); // The size is defined by svx_sensor_*size data members
      if(Verbose>0 && ilr==1) {std::cout << "svxDetectorGeo::CreatePanels(): censor centers X and Y = " << ilr << " " << ild << " " << x << " " << y << std::endl;}
      float z = 0.; // Center (in Z) of the ladder is at 0. by default
      
      // note :Rotation matrix  defined by GEANT is below: 
      //  costheta*cosphi     -sinphi    sintheta*cospi
      //  costheta*sinphi      cosphi    sintheta*sinpi
      //     -sintheta            0          costheta
      //
      // "phi" is the rotation angle around z-axis (call new coordinate x'y'z'-axis after this rotation )
      // "theta" is the rotation angle around y'-axis 
      // Currently,sensor's rotation around x(x')-axis is not assumed in offline code.
      // Coordinate transformation is performed as
      // xglobal[i] = rotMatrix[j][i]*xlocal[j] + transVector[i] 
      // (i=0-2, j=0-2) 
      // this transformation is defined in SvxSensor class
      // rotation matrix is transposed one when you transform each sensor's local coordinate into global coordinate.
      // If theta=0,the sensor's rotation angle from x-z plane in PHENIX is "-phi"
      float costheta = barSensor[ilr][ild][0]->get_rotMatrix(2, 2);   // Rotation angles. Rotation matrix defined by GEANT:
      float cosphi = barSensor[ilr][ild][0]->get_rotMatrix(1, 1);     //      costheta*cosphi     -sinphi    sintheta*cosphi
      float sintheta = -barSensor[ilr][ild][0]->get_rotMatrix(0, 2);  //      costheta*sinphi      cosphi    sintheta*sinphi
      float sinphi = -barSensor[ilr][ild][0]->get_rotMatrix(1, 0);    //         -sintheta            0          costheta
      
      
      if(Verbose>9 && ilr==0) {std::cout << "svxDetectorGeo::CreatePanels(): angles = " << ilr << " " << ild << " " << costheta << " " << cosphi << " " << sintheta << " " << sinphi << std::endl;}
      
      float halfwidth = svx_sensor_xsize[ilr];
      float halflength = svx_sensor_zsize[ilr]*tmpnumsn[ilr] + (tmpnumsn[ilr]-1)*svx_sensor_snzgap[ilr];
      if(Verbose>9 && ilr==0) {std::cout << "svxDetectorGeo::CreatePanels(): sensor sizes: " << halfwidth << " " << halflength << " " << svx_sensor_zsize[ilr] << endl;}

      // PHPanel is defined by 3 points. Z axis points right
      //  Y axis points up, X axis points out of plane.
      //
      //       1  *---------------------* 3
      //          |                     |
      //          |                     |
      //       2  *---------------------*
      //
      // No rotation in theta is assumed
      tmpx = x + halfwidth * cosphi;
      tmpy = y + halfwidth * sinphi;
      tmpz = z - halflength;
      (panelPoint[0]).setX(tmpx); (panelPoint[0]).setY(tmpy); (panelPoint[0]).setZ(tmpz);
      tmpx = x - halfwidth * cosphi;
      tmpy = y - halfwidth * sinphi;
      tmpz = z - halflength;
      (panelPoint[1]).setX(tmpx); (panelPoint[1]).setY(tmpy); (panelPoint[1]).setZ(tmpz);
      tmpx = x + halfwidth * cosphi;
      tmpy = y + halfwidth * sinphi;
      tmpz = z + halflength;
      (panelPoint[2]).setX(tmpx); (panelPoint[2]).setY(tmpy); (panelPoint[2]).setZ(tmpz);
      
      PHPanel tmpPanel(panelPoint[0],panelPoint[1],panelPoint[2]);
      
      
      // global tracking counts panels separately in each arm
      unsigned int iarm = 1; // west arm goes first in Geant Geometry (barSensor array)
      unsigned int tmpladder = ild;
      if(ild>=tmpnumld[ilr]) {iarm=0; tmpladder=ild-tmpnumld[ilr];}
      if(ilr==1) {cout << "   filling svxLadder " << iarm << " " << ilr << " " << tmpladder << " " << tmpx << " " << tmpy << endl;}
      svxLadders[iarm][ilr][tmpladder] = tmpPanel;  
      icount++;

    } // ild
  } // ilr
  std::cout << "svxDetectorGeo::CreatePanels(): " << icount << " Ladder panels created." << endl;
  
  std::cout << "svxDetectorGeo::CreatePanels(): Execution completed." << endl;
  return true;
}

PHBoolean svxDetectorGeo::Read_StripSurvey(std::string filename) {

  if(Verbose>0) cout << PHWHERE << "Reading SVX Strip Survey Geometry from " << filename << " file..."  << endl;
  ifstream fin(filename.c_str());
  if ( !fin ) {std::cerr << PHWHERE << filename << " ERROR: Can not open input file." << endl; return False;}
  if(Verbose>0) {std::cout << PHWHERE << " " << filename <<   " file opened." << std::endl;}

  int layer=-1;
  int ladder=-1;
  int sensor=-1;
  char cdummy[1];

  // ladder, sensor, point, xyz
  float sens3[16][5][4][3];
  float sens4[24][6][4][3];
  float xyz0[3],xyz1[3],xyz2[3],xyz3[3];

  for(int itmp=0; itmp<224; itmp++) {

    fin >> layer >> cdummy >> ladder >> cdummy >> sensor >> cdummy
	>> xyz0[0] >> cdummy >> xyz0[1] >> cdummy >> xyz0[2] >> cdummy
	>> xyz1[0] >> cdummy >> xyz1[1] >> cdummy >> xyz1[2] >> cdummy
	>> xyz2[0] >> cdummy >> xyz2[1] >> cdummy >> xyz2[2] >> cdummy
	>> xyz3[0] >> cdummy >> xyz3[1] >> cdummy >> xyz3[2];

    layer = layer-1;
    sensor = sensor-1;

    // divide by 10 because survey results are in mm
    if(layer==2 && ladder<8) {
      sens3[ladder][sensor][0][0]=xyz1[0]/10.;
      sens3[ladder][sensor][0][1]=xyz1[1]/10.;
      sens3[ladder][sensor][0][2]=xyz1[2]/10.;
      sens3[ladder][sensor][1][0]=xyz2[0]/10.;
      sens3[ladder][sensor][1][1]=xyz2[1]/10.;
      sens3[ladder][sensor][1][2]=xyz2[2]/10.;
      sens3[ladder][sensor][2][0]=xyz0[0]/10.;
      sens3[ladder][sensor][2][1]=xyz0[1]/10.;
      sens3[ladder][sensor][2][2]=xyz0[2]/10.;
      sens3[ladder][sensor][3][0]=xyz3[0]/10.;
      sens3[ladder][sensor][3][1]=xyz3[1]/10.;
      sens3[ladder][sensor][3][2]=xyz3[2]/10.;
    }
    if(layer==2 && ladder>=8 && ladder<16) {
      sens3[ladder][sensor][0][0]=xyz3[0]/10.;
      sens3[ladder][sensor][0][1]=xyz3[1]/10.;
      sens3[ladder][sensor][0][2]=xyz3[2]/10.;
      sens3[ladder][sensor][1][0]=xyz0[0]/10.;
      sens3[ladder][sensor][1][1]=xyz0[1]/10.;
      sens3[ladder][sensor][1][2]=xyz0[2]/10.;
      sens3[ladder][sensor][2][0]=xyz2[0]/10.;
      sens3[ladder][sensor][2][1]=xyz2[1]/10.;
      sens3[ladder][sensor][2][2]=xyz2[2]/10.;
      sens3[ladder][sensor][3][0]=xyz1[0]/10.;
      sens3[ladder][sensor][3][1]=xyz1[1]/10.;
      sens3[ladder][sensor][3][2]=xyz1[2]/10.;
    }
    if(layer==3 && ladder<12) {
      sens4[ladder][sensor][0][0]=xyz1[0]/10.;
      sens4[ladder][sensor][0][1]=xyz1[1]/10.;
      sens4[ladder][sensor][0][2]=xyz1[2]/10.;
      sens4[ladder][sensor][1][0]=xyz2[0]/10.;
      sens4[ladder][sensor][1][1]=xyz2[1]/10.;
      sens4[ladder][sensor][1][2]=xyz2[2]/10.;
      sens4[ladder][sensor][2][0]=xyz0[0]/10.;
      sens4[ladder][sensor][2][1]=xyz0[1]/10.;
      sens4[ladder][sensor][2][2]=xyz0[2]/10.;
      sens4[ladder][sensor][3][0]=xyz3[0]/10.;
      sens4[ladder][sensor][3][1]=xyz3[1]/10.;
      sens4[ladder][sensor][3][2]=xyz3[2]/10.;
    }
    if(layer==3 && ladder>=12 && ladder<24) {
      sens4[ladder][sensor][0][0]=xyz3[0]/10.;
      sens4[ladder][sensor][0][1]=xyz3[1]/10.;
      sens4[ladder][sensor][0][2]=xyz3[2]/10.;
      sens4[ladder][sensor][1][0]=xyz0[0]/10.;
      sens4[ladder][sensor][1][1]=xyz0[1]/10.;
      sens4[ladder][sensor][1][2]=xyz0[2]/10.;
      sens4[ladder][sensor][2][0]=xyz2[0]/10.;
      sens4[ladder][sensor][2][1]=xyz2[1]/10.;
      sens4[ladder][sensor][2][2]=xyz2[2]/10.;
      sens4[ladder][sensor][3][0]=xyz1[0]/10.;
      sens4[ladder][sensor][3][1]=xyz1[1]/10.;
      sens4[ladder][sensor][3][2]=xyz1[2]/10.;
    }

  } // end loop over lines in the input file

  fin.close();

  // Calculate sensor centers and rotation matrices
  // Rotation matrix  defined by GEANT as follows: 
  //  costheta*cosphi     -sinphi    sintheta*cospi
  //  costheta*sinphi      cosphi    sintheta*sinpi
  //     -sintheta            0          costheta
  //
  // "phi" is the rotation angle around z-axis (call new coordinate x'y'z'-axis after this rotation )
  // "theta" is the rotation angle around y'-axis 
  // Currently,sensor's rotation around x(x')-axis is not assumed in offline code.

  PHPoint tmpPoint[3];

  // if one of the first three points was not measured, use the last one instead

  float nladd[4];
  float nsens[4];
  nladd[2]=16; nsens[2]=5;
  nladd[3]=24; nsens[3]=6;

  // PHPanel requires only 3 points, second and third point must be
  // adjacent to the first one. Fourth point is diagonally located from the
  // first point ad is calculated automatically
  //   3  4
  //   1  2
  // The order of points in the survey is circular:
  //   4  3
  //   1  2

  int ilayer=2;
  for(int iladder=0; iladder<nladd[ilayer]; iladder++) {
    for(int isensor=0; isensor<nsens[ilayer]; isensor++) {
      float tmpx = sens3[iladder][isensor][0][0]; // first point
      float tmpy = sens3[iladder][isensor][0][1]; 
      float tmpz = sens3[iladder][isensor][0][2];
      (tmpPoint[0]).setX(tmpx); (tmpPoint[0]).setY(tmpy); (tmpPoint[0]).setZ(tmpz);
      tmpx = sens3[iladder][isensor][1][0]; // second point 
      tmpy = sens3[iladder][isensor][1][1]; 
      tmpz = sens3[iladder][isensor][1][2];
      if(tmpx==0 && tmpy==0 && tmpz==0) {
	tmpx = sens3[iladder][isensor][0][0]; 
	tmpy = sens3[iladder][isensor][0][1]; 
	tmpz = sens3[iladder][isensor][2][2];
      }
      (tmpPoint[1]).setX(tmpx); (tmpPoint[1]).setY(tmpy); (tmpPoint[1]).setZ(tmpz);
      tmpx = sens3[iladder][isensor][3][0]; // third point in PHPanel, fourth point in survey
      tmpy = sens3[iladder][isensor][3][1]; 
      tmpz = sens3[iladder][isensor][3][2];
      if(tmpx==0 && tmpy==0 && tmpz==0) {
	tmpx = sens3[iladder][isensor][2][0]; 
	tmpy = sens3[iladder][isensor][2][1]; 
	tmpz = sens3[iladder][isensor][0][2];
      }
      (tmpPoint[2]).setX(tmpx); (tmpPoint[2]).setY(tmpy); (tmpPoint[2]).setZ(tmpz);

      PHPlane tmpPlane(tmpPoint[0],tmpPoint[1],tmpPoint[2]);
      PHPanel tmpPanel(tmpPoint[0],tmpPoint[1],tmpPoint[2]);
      PHPoint tmpcenter = tmpPanel.getCenter();
      float centerx = tmpcenter.getX(); float centery = tmpcenter.getY(); float centerz = tmpcenter.getZ();
      PHVector tmpnormal = tmpPlane.getNormal(); tmpnormal.normalize();
      // Geant volumes are created "horizontally"?
      float cosphi = -tmpnormal.getY();
      float sinphi = tmpnormal.getX();
      float sintheta = tmpnormal.getZ();
      float costheta = sqrt(1.0-sintheta*sintheta);

      barSensor[ilayer][iladder][isensor]->set_transVector(0,centerx);
      barSensor[ilayer][iladder][isensor]->set_transVector(1,centery);
      barSensor[ilayer][iladder][isensor]->set_transVector(2,centerz);
     
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(0, 0, costheta*cosphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(1, 0, -sinphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(2, 0, sintheta*cosphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(0, 1, costheta*sinphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(1, 1, cosphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(2, 1, sintheta*sinphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(0, 2, -sintheta);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(1, 2, 0.);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(2, 2, costheta);

    } // end loop over sensors
  }   // end loop over ladders

  ilayer=3;
  for(int iladder=0; iladder<nladd[ilayer]; iladder++) {
    for(int isensor=0; isensor<nsens[ilayer]; isensor++) {
      float tmpx = sens4[iladder][isensor][0][0];
      float tmpy = sens4[iladder][isensor][0][1];
      float tmpz = sens4[iladder][isensor][0][2];
      if(tmpx==0 && tmpy==0 && tmpz==0) {
	tmpx = sens4[iladder][isensor][3][0];
	tmpy = sens4[iladder][isensor][3][1];
	tmpz = sens4[iladder][isensor][3][2];
      }
      (tmpPoint[0]).setX(tmpx); (tmpPoint[0]).setY(tmpy); (tmpPoint[0]).setZ(tmpz);
      tmpx = sens4[iladder][isensor][1][0];
      tmpy = sens4[iladder][isensor][1][1];
      tmpz = sens4[iladder][isensor][2][2];
      if(tmpx==0 && tmpy==0 && tmpz==0) {
	tmpx = sens4[iladder][isensor][3][0];
	tmpy = sens4[iladder][isensor][3][1];
	tmpz = sens4[iladder][isensor][3][2];
      }
      (tmpPoint[1]).setX(tmpx); (tmpPoint[1]).setY(tmpy); (tmpPoint[1]).setZ(tmpz);
      tmpx = sens4[iladder][isensor][2][0];
      tmpy = sens4[iladder][isensor][2][1];
      tmpz = sens4[iladder][isensor][2][2];
      if(tmpx==0 && tmpy==0 && tmpz==0) {
	tmpx = sens4[iladder][isensor][3][0];
	tmpy = sens4[iladder][isensor][3][1];
	tmpz = sens4[iladder][isensor][3][2];
      }
      (tmpPoint[2]).setX(tmpx); (tmpPoint[2]).setY(tmpy); (tmpPoint[2]).setZ(tmpz);

      PHPlane tmpPlane(tmpPoint[0],tmpPoint[1],tmpPoint[2]);
      PHPanel tmpPanel(tmpPoint[0],tmpPoint[1],tmpPoint[2]);
      PHPoint tmpcenter = tmpPanel.getCenter();
      float centerx = tmpcenter.getX(); float centery = tmpcenter.getY(); float centerz = tmpcenter.getZ();
      PHVector tmpnormal = tmpPlane.getNormal(); tmpnormal.normalize();
      // Geant volumes are created "horizontally"?
      float cosphi = -tmpnormal.getY();
      float sinphi = tmpnormal.getX();
      float sintheta = tmpnormal.getZ();
      float costheta = sqrt(1.0-sintheta*sintheta);

      barSensor[ilayer][iladder][isensor]->set_transVector(0,centerx);
      barSensor[ilayer][iladder][isensor]->set_transVector(1,centery);
      barSensor[ilayer][iladder][isensor]->set_transVector(2,centerz);

      barSensor[ilayer][iladder][isensor]->set_rotMatrix(0, 0, costheta*cosphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(1, 0, -sinphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(2, 0, sintheta*cosphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(0, 1, costheta*sinphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(1, 1, cosphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(2, 1, sintheta*sinphi);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(0, 2, -sintheta);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(1, 2, 0.);
      barSensor[ilayer][iladder][isensor]->set_rotMatrix(2, 2, costheta);

    } // end loop over sensors
  }   // end loop over ladders

  return True;
}

PHBoolean svxDetectorGeo::Write_svxPISApar() {
  return Write_svxPISApar("svxPISA.par");
}

PHBoolean svxDetectorGeo::Write_svxPISApar(std::string filename) {
  ofstream fout(filename.c_str());
  if(!fout) {cout << "svxDetectorGeo::Write_svxPISApar(): Cannot open output file." << endl; return False;}
  if(Verbose>0) {std::cout << "svxDetectorGeo::Write_svxPISApar(): " << filename << " output file opened." << std::endl;}
  
  fout << " SVX cage parameters:" << endl;
  fout << "   2.2000000" << endl;
  fout << "  0.50000000      0.20000000" << endl;
  fout << "   0.0000000" << endl;
  fout << "           6" << endl;
  fout << "  -40.000000      -26.500000      -26.500000       26.500000       26.500000       40.000000" << endl;
  fout << "   53.000000       53.000000       22.000000       22.000000       53.000000       53.000000" << endl;
  fout << "   0.0000000       0.0000000       0.0000000" << endl;
  fout << " SVX barrel parameters:" << endl;
  // Write number of VTX layers
  fout << svxSecNlayers << endl;
  // Write sensor R positions (last number is staggering parameter)
  for ( unsigned int i = 0; i < svxSecNlayers+1; i++ ) { fout << svx_layer_rpos[i] << " "; } fout << endl;
  // Write sensor Z positions
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << svx_layer_zpos[i] << " "; } fout << endl;
  // Write number of sensor/ladder
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << nBarSensor[i] << " "; } fout << endl;
  // Write sensor width (X)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << svx_sensor_xsize[i] << " "; } fout << endl;
  // Write sensor thickness (Y)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << svx_sensor_ysize[i] << " ";} fout << endl;
  // Write sensor length (Z)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << svx_sensor_zsize[i] << " ";} fout << endl;
  // Write x0add
  float x0add[4] = {1.89999994E-02, 1.89999994E-02, 0.0000000, 0.0000000};
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << x0add[i] << " ";} fout << endl;
  // Write snzgap (gap between sensors in z direction)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << svx_sensor_snzgap[i] << " ";} fout << endl;
  // Write dphi
  float dphi[4] = {26.700001, 13.600000, 17.000000, 12.000000};
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << dphi[i] << " ";} fout << endl;
  // Write tilt
  float tilt[4] = {-13.000000,-13.020000, 0.0000000, 0.0000000};
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) { fout << tilt[i] << " ";} fout << endl;
  // Read number of ladders per layer (both sides)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    int nsec=2;
    fout << nsec << endl;
    fout << nBarLadder[i]/2 << " " << nBarLadder[i]/2 << endl;
    fout << "   0.0000000       180.00000" << endl; 
  }

  fout << " SVX barrel sensor rotation matrices and translation vectors" << endl;

  int isec=0;
  for (unsigned int ilr = 0; ilr < svxSecNlayers; ilr++)  {
    for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {
      for (unsigned int isn=0; isn < nBarSensor[ilr]; isn++) {
        fout << " " << isec << " " << ilr+1 << " " << ild+1 << " " << isn+1 << endl; // all counters start from 1, not 0
        fout << " " << barSensor[ilr][ild][isn]->get_transVector(0) << " " << barSensor[ilr][ild][isn]->get_transVector(1) << " " << barSensor[ilr][ild][isn]->get_transVector(2) << endl;
        for ( int i = 0; i < 3; i++ ) {
          for ( int j = 0; j < 3; j++ ) {
            fout << " " << barSensor[ilr][ild][isn]->get_rotMatrix(i, j);
          }
          fout << endl;
        }
      } // isn
    } // ild
  } // ilr


  fout.close();
  return True;
}

PHBoolean svxDetectorGeo::Read_svxPISApar(string filename) {

  if (is_SensorCreated) {
    cout<<"svxDetectorGeo: Parameters are replaced."<<endl;
  }

  static const char *c_errhead = "svxDetectorGeo: ERROR reading ";

  // Reading SVX parameters from svxPISA.par file
  // """"""""""""""""""""""""""""""""""""""""""""
  if (Verbose>0) 
    cout << "svxDetectorGeo::Read_svxPISApar(): " 
	 << "Reading SVX sensors parameters from " << filename << " file"  << endl;
  FILE *svxParPISA = fopen(filename.c_str(), "r");
  if ( !svxParPISA ) {
    std::cerr << "svxDetectorGeo::Read_svxPISApar(): ERROR reading " 
	      << filename << " file" << endl; 
    return False;
  }
  char cdummy[120];
  if(Verbose>0) {
    std::cout << filename <<   " file opened." << std::endl;
  }
  
  // Skip SVX cage parameters
  for ( int i = 0; i < 9; i++ ) {
    if ( fgets(cdummy, 120, svxParPISA) != cdummy ) {
      std::cerr << c_errhead  << filename << " file " << endl;
    }
  }

  // Read number of barrel layers
  if ( fscanf(svxParPISA,"%u",&svxSecNlayers) != 1 ) {
    std::cerr << c_errhead  << filename << " file  1" << endl;
  }
  if(svxSecNlayers!=SVXLAYERNUMBER){
    std::cerr << c_errhead << filename << " file  2" << endl;
  } 
  if(Verbose>0) {std::cout << "Number of layers = " << svxSecNlayers << std::endl;}


  // Read radial positions of ladders
  for ( unsigned int i = 0; i < svxSecNlayers+1; i++ ) {
    if ( fscanf(svxParPISA,"%e",&svx_layer_rpos[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Radial position ["<<i<<"] = " << svx_layer_rpos[i] << std::endl;}
  }
  
  // Read Z positions of the detector center (should always be 0) 
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&svx_layer_zpos[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Z position ["<<i<<"] = " << svx_layer_zpos[i] << std::endl;}
  }
  
  // Read number of sensor/ladder
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%u",&nBarSensor[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Number of sensors ["<<i<<"] = " << nBarSensor[i] << std::endl;}
  }
  
  // Read sensor width (X)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&svx_sensor_xsize[i]) != 1 )  {std::cerr << c_errhead  << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor width ["<<i<<"] = " << svx_sensor_xsize[i] << std::endl;}
  }
  
  // Read sensor thickness (Y)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&svx_sensor_ysize[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor thickness ["<<i<<"] = " << svx_sensor_ysize[i] << std::endl;}
  }
  
  // Read sensor length (Z)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&svx_sensor_zsize[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor length ["<<i<<"] = " << svx_sensor_zsize[i] << std::endl;}
  }
  
  // Read x0add
  float x0add[4];
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&x0add[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor x0add ["<<i<<"] = " << x0add[i] << std::endl;}
  }
  
  // Read snzgap (gap between sensors in z direction)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&svx_sensor_snzgap[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor snzgap ["<<i<<"] = " << svx_sensor_snzgap[i] << std::endl;}
  }
  
  // Read dphi
  float dphi[4];
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&dphi[i]) != 1 )  {std::cerr << c_errhead << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor dphi ["<<i<<"] = " << dphi[i] << std::endl;}
  }
  
  // Read tilt
  float tilt[4];
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    if ( fscanf(svxParPISA,"%e",&tilt[i]) != 1 )  {std::cerr << c_errhead  << filename << " file " << endl;}
    if(Verbose>0) {std::cout << "Sensor tilt ["<<i<<"] = " << tilt[i] << std::endl;}
  }
  
  // Read number of ladders per layer (both sides)
  for ( unsigned int i = 0; i < svxSecNlayers; i++ ) {
    int nsec;
    if ( fscanf(svxParPISA,"%d",&nsec) != 1 )           {std::cerr << c_errhead << filename << " file " << endl;}
    int nlad;
    nBarLadder[i] = 0;
    for ( int j = 0; j < nsec; j++ ) {
      if ( fscanf(svxParPISA,"%d",&nlad) != 1 )         {std::cerr << c_errhead << filename << " file " << endl;}
      nBarLadder[i] += nlad;
    }
    if(Verbose>0) {std::cout << "Number of ladders on both sides["<<i<<"] = " << nBarLadder[i] << std::endl;}
    // Skip sector azimuth position
    for ( int j = 0; j < 2; j++ ) {
      if ( fgets(cdummy, 80, svxParPISA) != cdummy )    {std::cerr << c_errhead << filename << " file " << endl;}
    }
  }
  
  //  Skip to barrel translation vectors & rotation matrices
  if ( fgets(cdummy, 80, svxParPISA) != cdummy )      {std::cerr << c_errhead << filename << " file " << endl;}
  
  // Create SvxSensor objects
  // """"""""""""""""""""""""
  for (unsigned int ilr = 0; ilr < svxSecNlayers; ilr++)  {
    unsigned int itemp;
    float        temp;
    for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {
      for (unsigned int isn=0; isn < nBarSensor[ilr]; isn++) {
	
        // in Geant, all counters start from 1, not 0, that's why ilr+1, not ilr
        // svx section is always 0 (barrel)
        if ( fscanf(svxParPISA,"%u",&itemp) != 1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
        if ( itemp != 0 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
	if ( fscanf(svxParPISA,"%u",&itemp) != 1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
	if ( itemp != ilr+1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
	if ( fscanf(svxParPISA,"%u",&itemp) != 1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
	if ( itemp != ild+1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
	if ( fscanf(svxParPISA,"%u",&itemp) != 1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
	if ( itemp != isn+1 ) {
	  std::cerr << c_errhead << filename << " file " << endl;
	}
        
	// Read and set translation vector
	for ( int i = 0; i < 3; i++ ) {
	  if ( fscanf(svxParPISA,"%e",&temp) != 1 ) {
	    std::cerr << c_errhead << filename << " file " << endl;
	  }
	  barSensor[ilr][ild][isn]->set_transVector(i, temp);
	}
	if (Verbose && ilr==1 && isn==0) {
	  cout << "Read_svxPISApar: " << ild << " ";
	  cout << barSensor[ilr][ild][isn]->get_transVector(0) << " ";
	  cout << barSensor[ilr][ild][isn]->get_transVector(1) << endl;
	}
	// Read and set rotation matrix
	for ( int i = 0; i < 3; i++ ) {
	  for ( int j = 0; j < 3; j++ ) {
	    if ( fscanf(svxParPISA,"%e",&temp) != 1 ) {
	      std::cerr << c_errhead  << filename << " file " << endl;
	    }
	    barSensor[ilr][ild][isn]->set_rotMatrix(i, j, temp);
	  }
	}
	
      } // isn
    } // ild
  } // ilr
  
  ////////////////////////////
  // set origin offset
  setOffsetToSensor();
  
  
  cout << "svxDetectorGeo: SVX sensors read the parameter from the file." << endl;
  
  if ( fclose(svxParPISA) == EOF ) {std::cerr << c_errhead << filename << " file " << endl;}
  
  if (Verbose) {
    for(int i=0; i<20; i++) {
      cout << "final Read_svxPISApar: " << i << " ";
      cout << barSensor[1][i][0]->get_transVector(0) << " ";
      cout << barSensor[1][i][0]->get_transVector(1) << endl;
    }
  }
  
  is_SensorCreated = true;
  return True;
}

PHBoolean svxDetectorGeo::Fetch_svxPISApar() {
  return Fetch_svxPISApar(&Tsearch);
}

PHBoolean svxDetectorGeo::Fetch_svxPISApar(PHTimeStamp *T) {
  if(is_SensorCreated) { cout<<"svxDetectorGeo: Parameters are replaced."<<endl;}

  if(Verbose>0) cout << "svxDetectorGeo::Fetch_svxPISApar(): " << "Reading SVX sensors parameters from database." << endl;
  
  PHBoolean success = True;
  PHTimeStamp Tsearch = *T;
  PdbCoordinate* achan=0;
  PdbBankID bankID;
  PdbCalBank *svxBank = 0;
  const char *tableName;
  int iindex=0;
  float X,Y,Z;
  
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  
  if(!application->startRead()) {
    application->abort();
    cerr << PHWHERE << " ERROR -> Transaction aborted. Database NOT available." << endl;
    return False;
  }
  
  tableName = "svx.par.sim0";
  bankID.setInternalValue(0);
  if(m_geoBankId_For_Oldproduction>0){
    bankID.setInternalValue(m_geoBankId_For_Oldproduction);
    cout<<"svxDetectorGeo::Fetch_svxPISApar bankID for oldproduction is set. "
        <<"BankID is "<<m_geoBankId_For_Oldproduction<<endl;
  }
  if (Verbose) {
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Search TimeStamp: " << Tsearch << endl;
  }

  svxBank = bankManager->fetchBank("PdbCoordinateBank", bankID, tableName, Tsearch);
  
  if(svxBank) {
    svxBank->printHeader();

    int banklength = (int)svxBank->getLength();
    if(Verbose>0) {cout << "Banklength = " << banklength <<  endl;}
    
    achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
    svxSecNlayers = (unsigned int)achan->getParameter(0);
    int nsides    = (int)achan->getParameter(1);
    if(svxSecNlayers!=SVXLAYERNUMBER) { cout<<"Fetch_svxPISApar Error svxSecNlayers is : "<<svxSecNlayers<<endl; }
    iindex++;

    achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
    nBarLadder[0] = (int)achan->getParameter(0);
    nBarLadder[1] = (int)achan->getParameter(1);
    nBarLadder[2] = (int)achan->getParameter(2);
    nBarLadder[3] = (int)achan->getParError(0);
    iindex++;
    achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
    nBarSensor[0] = (int)achan->getParameter(0);
    nBarSensor[1] = (int)achan->getParameter(1);
    nBarSensor[2] = (int)achan->getParameter(2);
    nBarSensor[3] = (int)achan->getParError(0);
    iindex++;
    
    if (Verbose>0) {
      cout << "# of layers = " << svxSecNlayers << " " << nsides << endl;
      cout << nBarLadder[0] << " " << nBarLadder[1] << " " << nBarLadder[2] << " " << nBarLadder[3] << endl;
      cout << nBarSensor[0] << " " << nBarSensor[1] << " " << nBarSensor[2] << " " << nBarSensor[3] << endl;
    }
    
    for (unsigned int ilr = 0; ilr < svxSecNlayers; ilr++)  {
      for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {
        for (unsigned int isn=0; isn < nBarSensor[ilr]; isn++) {
          
          // just sanity check
          achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
          unsigned int iX = (unsigned int)achan->getParameter(0);
          unsigned int iY = (unsigned int)achan->getParameter(1);
          unsigned int iZ = (unsigned int)achan->getParameter(2);
          if ( iX != ilr ) {std::cerr << "svxDetectorGeo: ERROR reading svxPISA.par info from DATABASE" << endl;}
          if ( iY != ild ) {std::cerr << "svxDetectorGeo: ERROR reading svxPISA.par info from DATABASE" << endl;}
          if ( iZ != isn ) {std::cerr << "svxDetectorGeo: ERROR reading svxPISA.par info from DATABASE" << endl;}
          iindex++;
          
          // Read and set translation vector
          achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
          X = achan->getParameter(0);
          Y = achan->getParameter(1);
          Z = achan->getParameter(2);
          barSensor[ilr][ild][isn]->set_transVector(0, X);
          barSensor[ilr][ild][isn]->set_transVector(1, Y);
          barSensor[ilr][ild][isn]->set_transVector(2, Z);
          iindex++;
          
          // Read and set rotation matrix
          
          for ( int i = 0; i < 3; i++ ) {
            achan = (PdbCoordinate*) & (svxBank->getEntry(iindex));
            X = achan->getParameter(0);
            Y = achan->getParameter(1);
            Z = achan->getParameter(2);
            barSensor[ilr][ild][isn]->set_rotMatrix(i, 0, X);
            barSensor[ilr][ild][isn]->set_rotMatrix(i, 1, Y);
            barSensor[ilr][ild][isn]->set_rotMatrix(i, 2, Z);
            iindex++;
          }
          
          if(Verbose>0 && ilr==3 && ild==(nBarLadder[3]-1) && isn==(nBarSensor[3]-1)) {
            cout << " -------------- last entry ------------ " << endl;
            cout << iX << " " << iY << " " << iZ << endl;
            cout << barSensor[ilr][ild][isn]->get_transVector(0) << " "
        	 << barSensor[ilr][ild][isn]->get_transVector(1) << " "
        	 << barSensor[ilr][ild][isn]->get_transVector(2) << endl;
            cout << barSensor[ilr][ild][isn]->get_rotMatrix(0,0) << " "
        	 << barSensor[ilr][ild][isn]->get_rotMatrix(0,1) << " "
        	 << barSensor[ilr][ild][isn]->get_rotMatrix(0,2) << endl;
            cout << barSensor[ilr][ild][isn]->get_rotMatrix(1,0) << " "
        	 << barSensor[ilr][ild][isn]->get_rotMatrix(1,1) << " "
        	 << barSensor[ilr][ild][isn]->get_rotMatrix(1,2) << endl;
            cout << barSensor[ilr][ild][isn]->get_rotMatrix(2,0) << " "
        	 << barSensor[ilr][ild][isn]->get_rotMatrix(2,1) << " "
        	 << barSensor[ilr][ild][isn]->get_rotMatrix(2,2) << endl;
          }
          
        } // isn
      } // ild
    } // ilr

    ////////////////////////////
    // set origin offset
    setOffsetToSensor();

  } // svxBank 
  else {
    cerr << PHWHERE << " ERROR -> bankManager returned zero-pointer." << endl;
    success = False;
  }
  
  if(success) {application->commit();} else {application->abort();}
  
  if(svxBank) delete svxBank;
  
  is_SensorCreated = true;
  return success;
}

//function for updating VTX geometry in the database
PHBoolean svxDetectorGeo::Update_svxPISApar(PHTimeStamp *Tbeg, PHTimeStamp *Tend, const char *description="update svx geometry") {

  PHBoolean success = True;
  PHTimeStamp Tstart = *Tbeg;
  PHTimeStamp Tstop  = *Tend;
  PdbCalBank *svxBank = 0;
  PdbBankID bankID;
  const char *tableName;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if(application->startUpdate()) {

    bankID.setInternalValue(0);
    tableName = "svx.par.sim0";
    
    if (Verbose>0) {
      cout << "tableName = " << tableName << endl;
      cout << "description = " << description << endl;
      cout << "bankID = " << bankID.getInternalValue() << endl;
      cout << "Validity range: " << Tstart << " - " << Tstop << endl;
    }
    // calculate total number of sensors in one side of the barrel
    int bl=0;
    for(unsigned int ilr=0; ilr<svxSecNlayers; ilr++) {
      for(unsigned int ild=0; ild<nBarLadder[ilr]; ild++) {
	for(unsigned int isn=0; isn<nBarSensor[ilr]; isn++) {
	  bl++;
	}
      }
    }
    if (Verbose>0) {cout << "Number of sensors = " << bl << endl;}
    
    svxBank = bankManager->createBank("PdbCoordinateBank", bankID, description, Tstart, Tstop, tableName);
    int iindex=0;
    
    if(svxBank) {
      
      int banklength = 3 + bl*2*5; // 2 sides, 5 = 1 index + 1 translation vector + 3 rotation matrices
      if (Verbose>0) {cout << "Bank length = " << banklength << endl;}
      svxBank->setLength(banklength);
      PdbCoordinate* xyz = 0;
      float X,Y,Z;
      
      xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
      xyz->setParameter(0, ((float)svxSecNlayers));
      xyz->setParameter(1, 2.);
      xyz->setParameter(2, 0.);
      xyz->setParError (0, 0.);
      xyz->setParError (1, 0.);
      xyz->setParError (2, 0.);
      iindex++;
      xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
      xyz->setParameter(0, ((float)nBarLadder[0]));
      xyz->setParameter(1, ((float)nBarLadder[1]));
      xyz->setParameter(2, ((float)nBarLadder[2]));
      xyz->setParError (0, ((float)nBarLadder[3]));
      xyz->setParError (1, 0.);
      xyz->setParError (2, 0.);
      iindex++;
      xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
      xyz->setParameter(0, ((float)nBarSensor[0]));
      xyz->setParameter(1, ((float)nBarSensor[1]));
      xyz->setParameter(2, ((float)nBarSensor[2]));
      xyz->setParError (0, ((float)nBarSensor[3]));
      xyz->setParError (1, 0.);
      xyz->setParError (2, 0.);
      iindex++;
      
      for(unsigned int ilr=0; ilr<svxSecNlayers; ilr++) {
	for(unsigned int ild=0; ild<nBarLadder[ilr]; ild++) {
	  for(unsigned int isn=0; isn<nBarSensor[ilr]; isn++) {
	    xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
	    xyz->setParameter(0, ((float)ilr));
	    xyz->setParameter(1, ((float)ild));
	    xyz->setParameter(2, ((float)isn));
	    xyz->setParError (0, 0.); xyz->setParError (1, 0.); xyz->setParError (2, 0.);
	    iindex++;
	    X = barSensor[ilr][ild][isn]->get_transVector(0);
	    Y = barSensor[ilr][ild][isn]->get_transVector(1);
	    Z = barSensor[ilr][ild][isn]->get_transVector(2);
	    xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
	    xyz->setParameter(0, X); xyz->setParameter(1, Y); xyz->setParameter(2, Z);
	    xyz->setParError (0, 0.); xyz->setParError (1, 0.); xyz->setParError (2, 0.);
	    iindex++;
	    for(int i=0; i<3; i++) {
	      X = barSensor[ilr][ild][isn]->get_rotMatrix(i, 0);
	      Y = barSensor[ilr][ild][isn]->get_rotMatrix(i, 1);
	      Z = barSensor[ilr][ild][isn]->get_rotMatrix(i, 2);
	      xyz = (PdbCoordinate*) & (svxBank->getEntry(iindex));
	      xyz->setParameter(0, X); xyz->setParameter(1, Y); xyz->setParameter(2, Z);
	      xyz->setParError (0, 0.); xyz->setParError (1, 0.); xyz->setParError (2, 0.);
	      iindex++;
	    }
	  }
	}
      }
      
    }
    else {
      cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
      success = False;
    }
    
  }
  else {
    cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;
    success = False;
  }
  
  if(success) {application->commit();} else {application->abort();}
  
  if(svxBank) delete svxBank;

  return success;
}



// This function must be called after the svxSensor is instatiated
void svxDetectorGeo::setOffsetToSensor() {
  if(barSensor==NULL) {
    cout<<"svxDetectorGeo::setOffsetToSensor barSensor is not initialized. Call Read_svxPISApar or Fetch_svxPISApar first!!"<<endl;
    return;
  }
  ////////////////////////////
  // set origin offset
  for (unsigned int ilr = 0; ilr < svxSecNlayers; ilr++)  {
    for (unsigned int ild = 0; ild < nBarLadder[ilr]; ild++) {
      float offset[3];
      if ( (ilr == 0 && ild < 5) ||
	   (ilr == 1 && ild < 10) ||
	   (ilr == 2 && ild < 8) ||
	   (ilr == 3 && ild < 12)
	   ) {
        offset[0] = m_coordinateoffset.getOffsetVtxToCnt(0);
        offset[1] = m_coordinateoffset.getOffsetVtxToCnt(1);
        offset[2] = m_coordinateoffset.getOffsetVtxToCnt(2);
      } else {
        offset[0] = m_coordinateoffset.getOffsetVtxToCnt(0) + m_coordinateoffset.getOffsetVtxEastToWest(0);
        offset[1] = m_coordinateoffset.getOffsetVtxToCnt(1) + m_coordinateoffset.getOffsetVtxEastToWest(1);
        offset[2] = m_coordinateoffset.getOffsetVtxToCnt(2) + m_coordinateoffset.getOffsetVtxEastToWest(2);
      }

      for (unsigned int isn=0; isn < nBarSensor[ilr]; isn++) {

        for ( int i = 0; i < 3; i++ ) {
          barSensor[ilr][ild][isn]->set_originOffset(i, offset[i]);
        }

      }
    }
  }

}


PHBoolean svxDetectorGeo::Fetch_coordinateOffset(const int run) {
  if ((run<0)||(run>99999999)) {
    cerr << "SvxBeamCenter::fetch run= "  << run << endl;
    return false;
  }

  RunToTime   *rt      = RunToTime::instance();
  PHTimeStamp *Tsearch = rt->getBeginTime(run);
  if(Verbose>5) {
    cout<<"RunNumber : "<<run<<endl;
    cout<<"Time      : "<<flush;
    Tsearch->print();
    cout<<endl;
  }

  PHBoolean result = Fetch_coordinateOffset(Tsearch);
  delete Tsearch;

  return result;
}

PHBoolean svxDetectorGeo::Fetch_coordinateOffset(const PHTimeStamp *T) {
  if(Verbose>0) cout << "svxDetectorGeo::Fetch_coordinateOffset(): " << "Reading the offset of the coordinate system." << endl;

  PdbCalBank *svxBank   = NULL;
  const char *tableName = "calibsvxcoordinateoffset";
  PdbBankID bankID;

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  if (!application->startRead()) {
    application->abort();
    std::cerr << PHWHERE
              << " ERROR -> Transaction aborted. Database NOT available."
              << std::endl;
    return false;
  }
  
  int keyvalue=0;
  bankID.setInternalValue(keyvalue);//important!!
  if(m_offsetBankId_For_Oldproduction>0){
    bankID.setInternalValue(m_offsetBankId_For_Oldproduction);
    cout<<"svxDetectorGeo::Fetch_coordinateOffset bankID for oldproduction is set. "
        <<"BankID is "<<m_offsetBankId_For_Oldproduction<<endl;
  }


  const PHTimeStamp *Tsearch = T;
  if (Tsearch == NULL) {
    cerr << "SvxBeamCenterPar::fetch Error Tsearch==null" <<endl;
    return false;
  }
  
  const char *bankName =   "PdbSvxCoordinateOffsetBank";
  

  if (Verbose>10) {
    cout << "tableName = " << tableName << endl;
    cout << "bankID = " << bankID.getInternalValue() << endl;
    cout << "Tsearch: " << *Tsearch << endl;
  }


  svxBank = bankManager->fetchBank(bankName, bankID, tableName, *Tsearch);


  PHBoolean success = True;
  if (svxBank) {
    svxBank->printHeader();

    unsigned numRecords = (unsigned)svxBank->getLength();

    if (Verbose>90) cout << "fetch numRecords = " << numRecords << endl;
    
    for (unsigned recordNumber=0; recordNumber<numRecords; recordNumber++) {
      m_coordinateoffset = (PdbSvxCoordinateOffset &) (svxBank->getEntry(recordNumber));
      if(Verbose>5) { cout<<"DBout print"<<endl; m_coordinateoffset.print();}
    }
  }
  else {
    cerr << PHWHERE << " ERROR -> bankManager returned null pointer."<<endl;
    success = False;
  }

  if(success) {
    application->commit();
    if (Verbose>90) cout << "fetch commit()" << endl; 
  }
  else {
    application->abort();
    if (Verbose>90) cout << "fetch abort()" << endl;
  }
  
  if(svxBank) delete svxBank;
  
  return success;
}

PHBoolean svxDetectorGeo::Update_coordinateOffset(const PHTimeStamp *Tbeg, const PHTimeStamp *Tend, const char *desc) {
  PHBoolean success = true;
  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  const char *tableName = "calibsvxcoordinateoffset";

  PdbCalBank *svxBank = NULL;

  if (!application->startUpdate()) {
    application->abort();
    cerr << PHWHERE << "ERROR: Database not writable, aborting." << endl;
    return false;
  }

  if (Tbeg == NULL) {
    cerr << "SvxCoordinateOffset::updateToDB> Error Tbegin==NULL" << endl;
    return false;
  }
  if (Tend == NULL) {
    cerr << "SvxCoordinateOffset::updateToDB> Error Tend==NULL"<< endl;
    return false;
  }

  // description
  string s_desc;
  if(desc==NULL) {
    s_desc = string("BeamOffset Period : ") + Tbeg->formatTimeString() + " - " + Tend->formatTimeString();
  }
  else {
   s_desc = string(desc);
  }
  if(s_desc.size()>256) {
    s_desc = s_desc.substr(0, 256);
    cout<<"Description should be less than 256. Longer part is cut."<<endl;
    if(Verbose>5) cout<<"Description : "<<endl<<s_desc.c_str()<<endl;

  }

  const char *bankName = "PdbSvxCoordinateOffsetBank";

  int keyvalue = 0;
  PdbBankID bankID;
  bankID.setInternalValue(keyvalue);

  cout << "tableName = " << tableName << endl;
  cout << "description = " << s_desc.c_str() << endl;
  cout << "bankID = " << bankID.getInternalValue() << endl;
  cout << "Validity range: " << *Tbeg << " - " << *Tend << endl;


  //here we have correspondence between PdbSvxPixelHotDeadMap structure and bank record length
  svxBank = bankManager->createBank(bankName, bankID, s_desc.c_str(),
				    (PHTimeStamp&)(*Tbeg), (PHTimeStamp&)(*Tend), tableName);



  if (svxBank) {
    // The number of records is same as the number of data in the map array
    svxBank->setLength(1);

    // And now write the value to DB
    unsigned recordNumber = 0;
    PdbSvxCoordinateOffset *rec = dynamic_cast<PdbSvxCoordinateOffset*> (&(svxBank->getEntry(recordNumber)));

    *rec = m_coordinateoffset;
    if(Verbose>5) { cout<<"DB object "<<endl; m_coordinateoffset.print();}
  }
  else {
    cerr << PHWHERE << " ERROR: can not create svxBank." << endl;
    success = false;
  }


  if (Verbose>90) cout << "updateToDB success = " << success << endl;

  if (success) {
    application->commit();
    if (Verbose>90) cout << "updateToDB commit()" << endl;

  }
  else {
    application->abort();
    if (Verbose>90) cout << "updateToDB abort()" << endl;
  }

  if (svxBank!=NULL) delete svxBank;

  return success;
}

int svxDetectorGeo::get_nearestLayer(double r)
{
  // Determine nearest layer to radius r
  int layer = -1, sublayer = -1;
  double dr = 0, rmin = 1e99;

  for (int isublayer=0; isublayer<8; isublayer++) {
    dr = fabs(r - get_Rsublayer(isublayer));
    //    Printf("Layer %d Radius: %.2f.    dr = %.2g", isublayer, get_svxRadius(isublayer) , dr);
    if (dr < rmin) {
      rmin = dr;
      sublayer = isublayer;
    }
  }

  if (sublayer >= 0) {
    if (sublayer < 2) layer = sublayer;
    else if (sublayer < 5) layer = 2;
    else if (sublayer < 8) layer = 3;
  }

  return layer;
}

SvxSensor* svxDetectorGeo::get_nearestSensor(double x, double y, double z, int layer)
{
  // Determine nearest sensor to point (x,y,z).  Sensor position
  // defined by SvxSensor::get_correctedTransVector(). If a layer (0-3) is
  // supplied as an optional 4th parameter, search is constrained to
  // that layer.
  //
  // input : global position of cluster and layer 
  // output : SvxSensor object of nearest sensor

  SvxSensor* nearest = 0;
  int ily1 = 0, ily2 = 3;

  if (layer >= 0 && layer < 4) 
    ily1 = ily2 = layer;
  
  //double min_localr2 = -1.;
  double min_abslocaly = -100.;
  double abslocalx = -100.;
  double pos_local[3]={-9999.,-9999.,-9999.};
  double min_pos_local[3] = {-9999.,-9999.,-9999.};
  int thislayer  = -1;
  int ladder = -1;
  int ncandidate=0;
  for (int ily=ily1; ily<=ily2; ily++) {
    int nLadder = get_nBarLadder(ily);
    int nSensor = get_nBarSensor(ily);
    for (int ildr=0; ildr<nLadder; ildr++) {
      for (int isen=0; isen<nSensor; isen++) {
	SvxSensor *sens = GetSensorPtr(ily, ildr, isen);

        double pos_global[3] = {x, y, z};
        sens->position_global2local(pos_global, pos_local);

    //--cout<<"find ladder : "<<ily1<<" "<<ildr<<" "<<isen<<", "
    //--    <<pos_local[0]<<" "<<pos_local[1]<<" : "
    //--    <<pos_local[0]*pos_local[0]+pos_local[1]*pos_local[1]<<endl;
  
 
        if ( 0.001 > fabs(pos_local[1]) && 
            (svx_sensor_xsize_active[ily]+0.001) > fabs(pos_local[0]) // additional 10um to be safe
           )
        {
         // min_localr2 = (pos_local[0]*pos_local[0]+pos_local[1]*pos_local[1]);
          min_abslocaly = fabs(pos_local[1]);
          abslocalx     = fabs(pos_local[0]);

          min_pos_local[0] = pos_local[0];
          min_pos_local[1] = pos_local[1];

          if(thislayer!=ily && ladder!=ildr) ncandidate++;

          //find ladder at first;
          thislayer  = ily;
          ladder = ildr;
        }

      }
    }
  }

  static       int nerror    = 0;
  static const int MAXNERROR = 100;

  static bool underMax = true;
  if(nerror>=MAXNERROR){
    if(underMax){
      cerr<<"svxDetectorGeo::nearestSensor : MaxNerror is exceeded. No more error written"<<endl;
      underMax=false;
    }
  }

  if(ncandidate!=1){
    if(nerror<MAXNERROR) {
      cerr<<"nearestSensor : too many candidate. n="<<ncandidate<<" "<<x<<" "<<y<<" "<<z<<endl;
    }
    nerror++;
  }

  if(thislayer==-1){
    if(nerror<MAXNERROR) {
      cerr<<"svxDetectorGeo"<<"::"<<__FUNCTION__<<" "
          <<" No proper layer is found : layer="<<thislayer<<" "<<x<<" "<<y<<" "<<z<<endl;
    }
    nerror++;

    return NULL;
  }

  //////////////////////
  // local_y should be always 0. 
  // but it may have some value due to rounding error when converting compactCNT
  // if local_y is more than 10um, should be error
  bool checkRng=true;
  if(min_abslocaly>0.001) {
    if(nerror<MAXNERROR) {
      cerr<<"LocalY is too large. probably this ladder was wrong, localy="<<min_abslocaly
          <<" : "<<thislayer<<" "<<ladder<<" : "<<x<<" "<<y<<" "<<z
          <<", local(x,y)=("<<min_pos_local[0]<<","<<min_pos_local[1]<<")"<<endl;
    }
    checkRng=false;
  }
  // local_x should be always within active sensor range.
  // but it may have some extra value due to rounding error when converting compactCNT, adding 10um
  if(abslocalx>svx_sensor_xsize_active[thislayer]+0.001){ // additional 10um to be safe
    if(nerror<MAXNERROR) {
      cerr<<"LocalX is too large. probably this ladder was wrong, localx="<<abslocalx
          <<" : "<<thislayer<<" "<<ladder<<" : "<<x<<" "<<y<<" "<<z
          <<", local(x,y)=("<<min_pos_local[0]<<","<<min_pos_local[1]<<")"<<endl;
    }
    checkRng=false;
  }


  //////////////////////
  //find sensor
  double min_abslocalz = -1.;
  //int nSensor = get_nBarSensor(layer);
  int nSensor = get_nBarSensor(thislayer);
  for ( int isn=0; isn<nSensor; isn++ ) {
    SvxSensor *svxsen = GetSensorPtr(thislayer, ladder, isn);
    double pos_global[3] = {x, y, z};
    svxsen->position_global2local(pos_global, pos_local);
    if ( min_abslocalz<0 || min_abslocalz>fabs(pos_local[2]) ) {
      min_abslocalz = fabs(pos_local[2]);
      //sensor = isn;
      nearest = svxsen;
    }
  }

  // local_z should be always within active sensor range.
  // but it may have some extra value due to rounding error when converting compactCNT, adding 10um
  if(min_abslocalz>svx_sensor_zsize_active[thislayer]+0.001){ // additional 10um to be safe
    if(nerror<MAXNERROR) {
      cerr<<"LocalZ is too large. probably this ladder was wrong, localz="<<min_abslocalz
          <<" "<<svx_sensor_zsize_active[thislayer]
          <<" : "<<thislayer<<" "<<ladder<<" : "<<x<<" "<<y<<" "<<z<<endl;
    }
    checkRng=false;
  }
  if(!checkRng){ nerror++; }

  return nearest;
}
