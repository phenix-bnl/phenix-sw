//-----------------------------------------------------------------------------
//  Implementation of class TofGeometryObject
//
//  Author: Akio Kiyomichi
//
//  History: 04/12/00  A.Kiyomichi  First Version
//           04/26/00  A.Kiyomichi  Release with ObjyDB access
//           06/13/00  A.Kiyomichi  add getXpos, Y, Z, Radius, Phi 
//           07/04/00  A.Kiyomichi  add EastCarriage. used by getXXX
//           08/16/00  A.Kiyomichi  Change default database file [GEANT -> Run]
//           09/14/00  J.Velkovska  add frames for coordinate transformations 
//                                  allow for separate transformation in each
//                                  panel
//           09/16/00  A.Kiyomichi  Update and commit
//           10/29/01  T.Chujo      Using namespace PHGeometry 
//           10/29/01  A.Kiyomichi  move TofAddressObject declaration to **.hh
//           01/09/04  H.Masui      Update DB access
//-----------------------------------------------------------------------------

#include "PdbCalBank.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCoordinate.hh"

#include "TofAddressObject.hh"
#include "TofGeometryObject.hh"

#include <string>
#include <fstream> 
#include <iostream>
#include "gsl/gsl_math.h"

using namespace std;
using namespace PHGeometry;

//========================
// member functions
//========================
// Constructor
TofGeometryObject::TofGeometryObject(){
	
  PHPoint  p0(0.0, 0.0, 0.0);
  PHPoint  p1(1.0, 0.0, 0.0);
  PHPoint  p2(0.0, 1.0, 0.0);
  PHVector v0(1.0, 0.0, 0.0);
  PHLine   line0(p0,v0);
  PHPanel  panel0(p0,p1,p2);
  PHFrame XYZ; // this builds a default coordinate system
  EastCarriage = XYZ;

  for(int i = 0;i < TOF_NPANEL_ALL;i++){
    TofPanel[i] = panel0;
    TofPanelFrame[i] = XYZ;
  }
  for(int i = 0;i < TOF_NSLAT_ALL;i++){
    TofSlat[i] = line0;
    TofSlatCenter[i] = p0;
    pointOffset[i] = p0;
    vectorOffset[i] = p0;
  }

  setDebugLevel(0);
  setGeomPanelName("geom.tof.panel0");     // for Run
  setGeomSlatOffName("geom.tof.slatoff0"); // for Run
  //setGeomPanelName("geom.tof.geantpanel0");     // for GEANT
  //setGeomSlatOffName("geom.tof.geantslatoff0"); // for GEANT
  setBankNumber(7200); 
  setBankID(BankNumber); 
  
  TofAddress = 0;
 
  Tsearch.setToSystemTime(); 
  //setTimeStamp(PHTimeStamp(2000,3,10,0,0,0)); 

  iFlag = -1; // don't have any data yet
  
}
// Destructor
TofGeometryObject::~TofGeometryObject(){
}
// Set East Carriage Position
PHBoolean TofGeometryObject::setEastCarriage(PHFrame &f){
  EastCarriage = f;
  return True;
}
PHBoolean TofGeometryObject::setEastCarriage(PHPoint p){
  PHPoint origin = p;
  PHVector Xaxis(1,0,0);
  PHVector Yaxis(0,1,0);
  PHFrame east(origin,Xaxis,Yaxis);

  return TofGeometryObject::setEastCarriage(east);
}
PHBoolean TofGeometryObject::setEastCarriage(float x, float y, float z){
  PHPoint origin(fabs((double)x),(double)y,(double)z);
  PHVector Xaxis(1,0,0);
  PHVector Yaxis(0,1,0);
  PHFrame east(origin,Xaxis,Yaxis);
  return TofGeometryObject::setEastCarriage(east);
}
// Set Panel Frame
PHBoolean TofGeometryObject::setPanelFrame(int ipanel_seq, PHFrame &f){
  TofPanelFrame[ipanel_seq]=f;
  return True;
}
// Set Panel Geometry
PHBoolean TofGeometryObject::setPanelGeo(int ipanel_seq, PHPanel panel){
  iFlag = 0;
  fromPanelToSlat(ipanel_seq, panel);
  return True;
}
PHBoolean TofGeometryObject::setPanelGeo(int ipanel_seq, PHPoint p0, 
					 PHPoint p1, PHPoint p2){
  iFlag = 0;
  PHPanel panel(p0, p1, p2);
  if(Debug>1){
    cout<<"  center = "<<panel.getCenter()<<endl;
    cout<<"  normal = "<<panel.getNormal()<<endl;
  }
  fromPanelToSlat(ipanel_seq, panel);
  return True;
}
PHBoolean TofGeometryObject::setPanelErr(int ipanel_seq, PHPoint p0, 
					 PHPoint p1, PHPoint p2){
  panelError[0][ipanel_seq] = p0;
  panelError[1][ipanel_seq] = p1;
  panelError[2][ipanel_seq] = p2;
  return True;
}
// Get Panel Geometry
PHPanel TofGeometryObject::getPanelGeo(int panel_seq){ 
//   PHPoint p0 = TofPanel[panel_seq].getPoint(0) + EastCarriage;
//   PHPoint p1 = TofPanel[panel_seq].getPoint(1) + EastCarriage;
//   PHPoint p2 = TofPanel[panel_seq].getPoint(2) + EastCarriage;
//   PHPanel panel(p0,p1,p2);
//   return panel;

// JV: The above construction only works if all panels are translated together
//     It does not allow for individual transformations.
//     It also ignores any possible rotations !
//     With the code below one can do idividual transformations as well as 
//     moving the whole arm together.
//     If one wants to move tha whole arm, the EastCarriage frame should be 
//     set only
//     the individual frames remain XYZ - (origin(0,0,0) 
//                                         Xaxis(1,0,0)
//                                         Yaxis(0,1,0)
//                                         Zaxis(0,0,1)
//     If one wants to move the panels individually, then
//     EastCarrige should be set to XYZ and each of the individual
//     frame tofPanelsFrame[panel_seq] should be set to what's needed

  PHFrame XYZ;
  // transform to panel's frame - if the same as XYZ - no transformation
  PHPanel panel = transformPanel(XYZ, TofPanel[panel_seq],
				 TofPanelFrame[panel_seq]);
  // transform to carriage frame
  PHPanel panelOut = transformPanel(XYZ, panel, EastCarriage);
  return panelOut;

}
// Get Slat Geometry
PHLine TofGeometryObject::getSlatGeo(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatGeo: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
//   PHPoint  basepoint = TofSlat[slatid].getBasepoint()+ EastCarriage;
//   PHVector direction = TofSlat[slatid].getDirection();
//   PHLine line(basepoint, direction);
//   return line;

  PHFrame XYZ;
  // first get the panelFrame for the panel where this slat is
  // use TOF address object for this
  PHFrame newSlatFrame = TofPanelFrame[TofAddress->getPanelSeq(slatid)];

  // transform the slat to the panel's frame
  PHLine slatLineInPanel = transformLine(XYZ, TofSlat[slatid],
						   newSlatFrame);
  // now transform to the east carriage frame
  PHLine lineOut = transformLine(XYZ, slatLineInPanel, EastCarriage);
  return lineOut;
}
PHPoint TofGeometryObject::getSlatXYZ(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatXYZ: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
//   PHVector vector = TofSlat[slatid].getDirection();
//   PHPoint point = TofSlat[slatid].getBasepoint() + (PHPoint)(vector*0.5);
//   //PHPoint point = TofSlatCenter[slatid];
//   point = point + EastCarriage;
//   return point;

  // here let's get the slatLine that is already transformed into the proper 
  // coordinate system
  PHLine slatLine = getSlatGeo(slatid);
  PHPoint pointOut = slatLine.getBasepoint() + 
    (PHPoint)(slatLine.getDirection()*0.5);
  return pointOut;
}
PHCylPoint TofGeometryObject::getSlatRPhiZ(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatRPhiZ: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  PHCylPoint point = getSlatXYZ(slatid);
  //PHCylPoint point = TofSlatCenter[slatid];
  return point;
}
PHVector TofGeometryObject::getSlatVector(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }

//   PHVector vector = TofSlat[slatid].getDirection();
//   //PHVector vector = TofSlatVector[slatid];
//   vector.normalize();
//   return vector;

  // JV : If the panels are rotated, then we need to transform the slat vectors
  // Let's get the already transformed slatLine and go on

  PHLine slatLine = getSlatGeo(slatid);
  PHVector vectorOut = slatLine.getDirection();
  vectorOut.normalize();
  return vectorOut;
}
float TofGeometryObject::getSlatLength(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  float length;
  length = TofSlat[slatid].length();
  return length;
}
float TofGeometryObject::getSlatWidth(int slatid){
  return TOF_SLAT_WIDTH;
}
float TofGeometryObject::getXpos(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  float x;
  //x = TofSlatCenter[slatid].getX() + EastCarriage.getX();
  PHPoint slatPoint = getSlatXYZ(slatid);
  x = slatPoint.getX();
  return x;
}
float TofGeometryObject::getYpos(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  float y;
  //y = TofSlatCenter[slatid].getY() + EastCarriage.getY();
  PHPoint slatPoint = getSlatXYZ(slatid);
  y = slatPoint.getY();
  return y;
}
float TofGeometryObject::getZpos(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  float z;
  //z = TofSlatCenter[slatid].getZ() + EastCarriage.getZ();
  PHPoint slatPoint = getSlatXYZ(slatid);
  z = slatPoint.getZ();
  return z;
}
float TofGeometryObject::getRadius(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  float r;
  PHCylPoint point = getSlatXYZ(slatid);
  r = point.getR();
  return r;
}
float TofGeometryObject::getPhi(int slatid){
  if(iFlag!=0) {
    cerr<<"TofGeometryObject ERROR getSlatVector: TofDGO not initialized."<<endl;
    cerr<< "    Use Fetch() or FetchFromFile() methods first." << endl; 
  }
  float phi,pphi;
  PHCylPoint point = getSlatXYZ(slatid);
  phi = point.getPhi();
  pphi = phi * 180 / M_PI;
  return pphi;
}

//====================================================================
//  DB access
//====================================================================
//
// Fetch information from Objy Database 
PHBoolean TofGeometryObject::fetch(){
  TofGeometryObject::fetchPanel();
  TofGeometryObject::fetchSlatOff();
  return True;
}

PHBoolean TofGeometryObject::fetch(const int run)
{
  Tsearch = getTimeStamp(run);

  return fetch();
}


PHBoolean TofGeometryObject::fetchEastCarriage(){
  return True;
}
PHBoolean TofGeometryObject::fetchPanel(){

  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication(); 

  if (application->startRead()) { 
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(BankNumber);
    const char *geoname = Geom0Name.getString(); // Geom0 - panel geometry

    int length = 3;

    PdbCalBank *tofBank = 
      bankManager->fetchBank("PdbCoordinateBank", bankID, geoname, tSearch);

    PdbCoordinate *coordinate;
    PHPoint p0,p1,p2;
    PHPoint dp0,dp1,dp2;

    if (tofBank){
      if(Debug > 0) tofBank->print();
      if(Debug > 1) 
	cout << "Number of Channels = " << tofBank->getLength()<< endl; 
      for(int i = 0; i < TOF_NPANEL_ALL ; i++){
	int panel_seq = i;
	for(int point = 0; point < length; point++){
	  int k = i*length + point;
	  coordinate = (PdbCoordinate*)&(tofBank->getEntry(k));
	  float x = coordinate->getParameter(0);
	  float y = coordinate->getParameter(1);
	  float z = coordinate->getParameter(2);
	  float dx = coordinate->getParError(0);
	  float dy = coordinate->getParError(1);
	  float dz = coordinate->getParError(2);
	  if(point == 0){
	    p0.setX((double)x); p0.setY((double)y); p0.setZ((double)z);
	    dp0.setX((double)dx); dp0.setY((double)dy); dp0.setZ((double)dz);
	  } else if(point == 1){
	    p1.setX((double)x); p1.setY((double)y); p1.setZ((double)z);
	    dp1.setX((double)dx); dp1.setY((double)dy); dp1.setZ((double)dz);
	  } else if(point == 2){
	    p2.setX((double)x); p2.setY((double)y); p2.setZ((double)z);
	    dp2.setX((double)dx); dp2.setY((double)dy); dp2.setZ((double)dz);
	    if(Debug>1){
	      cout<<"panel_seq = "<<panel_seq<<endl;
	      cout<<"  p0 = "; p0.print();
	      cout<<"  p1 = "; p1.print();
	      cout<<"  p2 = "; p2.print();
	    }
	    setPanelGeo(panel_seq, p0, p1, p2);
	    setPanelErr(panel_seq, dp0, dp1, dp2);
	  }
	}
      } //for(i = 0; i < n_panel; i++){
      delete tofBank;
    } // if(tofBank)
  } // if(application->startRead()){
  return True;
}
PHBoolean TofGeometryObject::fetchSlatOff(){

  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication(); 

  if (application->startRead()) { 
    PHTimeStamp tSearch = Tsearch;
    PdbBankID bankID(BankNumber);
    const char *geoname = Geom1Name.getString(); // Geom1 - slat geometry offset

    PdbCalBank *tofBank = bankManager->fetchBank("PdbCoordinateBank", bankID, geoname, tSearch);

    PdbCoordinate *coordinate;
    PHPoint point, vector;

    if (tofBank){
      if(Debug > 0) tofBank->print();
      if(Debug > 1) 
	cout << "Number of Channels = " << tofBank->getLength()<< endl; 
      for(int i = 0; i < TOF_NSLAT; i++){
	coordinate = (PdbCoordinate*)&(tofBank->getEntry(i));
	float x = coordinate->getParameter(0);
	float y = coordinate->getParameter(1);
	float z = coordinate->getParameter(2);
	float dx = coordinate->getParError(0);
	float dy = coordinate->getParError(1);
	float dz = coordinate->getParError(2);

	point.setX((double)x);
	point.setY((double)y);
	point.setZ((double)z);
	vector.setX((double)dx);
	vector.setY((double)dy);
	vector.setZ((double)dz);

	if(Debug>2){
	  cout<<" i = "<<i<<endl;
	  cout<<"  point = "; point.print();
	  cout<<"  vector = "; vector.print();
	}
	setPointOffset(i, point);
	setVectorOffset(i, vector);
      }
      delete tofBank;
    } // if(tofBank)
  } // if(application->startRead()){
  return True;
}

// Update information to Objy Database 
PHBoolean 
TofGeometryObject::update(const int beginrun, const int endrun)
{
  PHTimeStamp start = getTimeStamp(beginrun);
  PHTimeStamp stop  = getTimeStamp(endrun);

  return update(start, stop);
}

PHBoolean 
TofGeometryObject::update(PHTimeStamp tStart, 
			  PHTimeStamp tStop)
{

  updatePanel(tStart, tStop);
  updateSlatOff(tStart, tStop);

  return True;
}

PHBoolean 
TofGeometryObject::updateEastCarriage(PHTimeStamp tStart, 
				      PHTimeStamp tStop)
{
  return True;
}

PHBoolean 
TofGeometryObject::updatePanel(PHTimeStamp tStart, 
			       PHTimeStamp tStop)
{
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication(); 
  // Open the federation in update mode
  cout << "Opening FD in update mode..  " <<Geom0Name.getString()<<endl;

  if (application->startUpdate()) { 
    PdbBankID bankID(BankNumber);
    const char *descrip = "TOF Panel Geometry";
    const char *geoname = Geom0Name.getString(); // Geom0 - panel geometry

    PdbCalBank *tofBank = 
      bankManager->createBank("PdbCoordinateBank", bankID, descrip, 
			      tStart, tStop, geoname);

    tofBank->setUserName(UserName);

    int length = 3;
    int n_panel = TOF_NPANEL_ALL;
    int total = length*n_panel;
    tofBank->setLength(total);
    if(Debug>0)tofBank->print();

    for(int i = 0; i < n_panel ; i++){
      int panel_seq = i;
      for(int point = 0; point < length; point++){
	float x  = (float)getPanelGeo(panel_seq).getPoint(point).getX();
	float y  = (float)getPanelGeo(panel_seq).getPoint(point).getY();
	float z  = (float)getPanelGeo(panel_seq).getPoint(point).getZ();
	float dx = (float)panelError[point][panel_seq].getX();
	float dy = (float)panelError[point][panel_seq].getY();
	float dz = (float)panelError[point][panel_seq].getZ();

	int k = i*length + point;
        PdbCoordinate *coordinate = (PdbCoordinate*)&(tofBank->getEntry(k));
	coordinate->setParameter(0, x);
	coordinate->setParameter(1, y);
	coordinate->setParameter(2, z);
	coordinate->setParError(0, dx);
	coordinate->setParError(1, dy);
	coordinate->setParError(2, dz);

	if(Debug > 1) tofBank->printEntry(k); //debug
      }
    }
    application->commit();
  } // Geom0 -- panel geometry
  return True;
}

PHBoolean 
TofGeometryObject::updateSlatOff(PHTimeStamp tStart, 
				 PHTimeStamp tStop)
{
  // Select Objy implementation.
  PdbBankManager *bankManager = PdbBankManager::instance();
  // Get application manager class. 
  PdbApplication *application = bankManager->getApplication(); 
  // Open the federation in update mode
  cout << "Opening FD in update mode..  " <<Geom1Name.getString()<<endl;

  if (application->startUpdate()) { 
    PdbBankID bankID(BankNumber);
    const char *descrip = "TOF Slat Geometry offset";
    const char *geoname = Geom1Name.getString(); // Geom1 - slat geometry offset

    PdbCalBank *tofBank = 
      bankManager->createBank("PdbCoordinateBank", bankID, descrip, 
			      tStart, tStop, geoname);

    tofBank->setUserName(UserName);

    tofBank->setLength(TOF_NSLAT);
    if(Debug>0)tofBank->print();

    PdbCoordinate *coordinate;
    for(unsigned int i = 0; i < tofBank->getLength() ; i++){
      float x  = (float)pointOffset[i].getX();
      float y  = (float)pointOffset[i].getY();
      float z  = (float)pointOffset[i].getZ();
      float dx = (float)vectorOffset[i].getX();
      float dy = (float)vectorOffset[i].getY();
      float dz = (float)vectorOffset[i].getZ();
      
      coordinate = (PdbCoordinate*)&(tofBank->getEntry(i));
      coordinate->setParameter(0, x);
      coordinate->setParameter(1, y);
      coordinate->setParameter(2, z);
      coordinate->setParError(0, dx);
      coordinate->setParError(1, dy);
      coordinate->setParError(2, dz);
      
      if(Debug > 1) tofBank->printEntry(i); //debug
    }
    application->commit();
  } // Geom1 -- slat geometry offset
  return True;
}

// Fetch Geometry information from Table
PHBoolean 
TofGeometryObject::fetchGeoTable(PHCompositeNode *top)
{
  cout << " *** Under development" << endl;
  return True;
}

// Write FEM map information to Table
PHBoolean 
TofGeometryObject::writeGeoTable(PHCompositeNode *top)
{
  cout << " *** Under development" << endl;
  return True;
}

// Fetch information from default ASCII file
PHBoolean TofGeometryObject::fetchFromFile(){
  // default file is tofpanelgeom.txt
  // default slat offset are zero.
  const char *tofmapdb = "tofpanelgeo.txt";
  return TofGeometryObject::fetchFromFile(tofmapdb); 
}
  // Fetch information from an ASCII file "filename" 
PHBoolean TofGeometryObject::fetchFromFile(const char* filename){

  ifstream file(filename);
  int panel_seq, point;
  float x, y, z, dx, dy, dz;
  PHPoint p0,p1,p2;
  PHPoint dp0,dp1,dp2;

  if(!file){ 
    cerr << "TofGeometryObject::fetchFromFile ERROR:" << endl;
    cerr << "  Can not open "<< filename <<" file." << endl;
    cerr << endl;
    return False;
  }else{ 
    //int ibuf=0; 
    for(int i = 0; i < TOF_NPANEL_ALL*3; i++){
      file >> panel_seq >> point >> x >> y >> z >> dx >> dy >> dz;
      if (file.eof()) break; 
      if(point == 0){
	p0.setX((double)x); p0.setY((double)y); p0.setZ((double)z);
	dp0.setX((double)dx); dp0.setY((double)dy); dp0.setZ((double)dz);
      } else if(point == 1){
	p1.setX((double)x); p1.setY((double)y); p1.setZ((double)z);
	dp1.setX((double)dx); dp1.setY((double)dy); dp1.setZ((double)dz);
      } else if(point == 2){
	p2.setX((double)x); p2.setY((double)y); p2.setZ((double)z);
	dp2.setX((double)dx); dp2.setY((double)dy); dp2.setZ((double)dz);
	if(Debug>1){
	  cout<<"panel_seq = "<<panel_seq<<endl;
	  cout<<"  p0 = "; p0.print();
	  cout<<"  p1 = "; p1.print();
	  cout<<"  p2 = "; p2.print();
	}
	setPanelGeo(panel_seq, p0, p1, p2);
	setPanelErr(panel_seq, dp0, dp1, dp2);
      }
    }
    cout<<"Fetch information from "<< filename <<"  [ASCII file]"<<endl;
  }
  file.close();

  return True;
}
PHBoolean TofGeometryObject::fetchFromFile(const char* filename, const char* offsetfile){

  ifstream file(offsetfile);
  int slatid;
  float p[3], v[3];
  PHPoint point, vector;
  if(!file){ 
    cerr << "TofGeometryObject::fetchFromFile ERROR:" << endl;
    cerr << "  Can not open "<< offsetfile <<" file." << endl;
    cerr << endl;
    return False;
  }else{ 
    for(int i = 0; i < TOF_NSLAT_ALL; i++){
      file >> slatid >> p[0] >> p[1] >> p[2] >> v[0] >> v[1] >> v[2];
      if (file.eof()) break; 
      point.setX((double)p[0]);
      point.setY((double)p[1]);
      point.setZ((double)p[2]);
      vector.setX((double)v[0]);
      vector.setY((double)v[1]);
      vector.setZ((double)v[2]);

      setPointOffset(slatid, point);
      setVectorOffset(slatid, vector);
    }
  }
  cout<<"Fetch information from "<< offsetfile <<endl;
  cout<<"                and"<<endl;
  file.close();

  return TofGeometryObject::fetchFromFile(filename); 
}

// Write information to an ASCII file "filename" 
PHBoolean TofGeometryObject::writeToFile(const char* filename){

  ofstream file(filename);

  for(int i = 0; i < TOF_NPANEL_ALL; i++){
    int panel_seq = i;
    for(int point = 0; point < 3; point++){
      float x = (float)TofPanel[panel_seq].getPoint(point).getX();
      float y = (float)TofPanel[panel_seq].getPoint(point).getY();
      float z = (float)TofPanel[panel_seq].getPoint(point).getZ();
      float dx = (float)panelError[point][panel_seq].getX();
      float dy = (float)panelError[point][panel_seq].getY();
      float dz = (float)panelError[point][panel_seq].getZ();
      if(Debug>10){
	dx = (float)getPanelGeo(i).getPoint(point).getX();
	dy = (float)getPanelGeo(i).getPoint(point).getY();
	dz = (float)getPanelGeo(i).getPoint(point).getZ();
      }
      file<<"  "<< panel_seq<<"  "<< point
	  <<"\t"<< x <<"\t"<< y <<"\t"<< z <<"  "
	  <<"\t"<< dx <<"\t"<< dy <<"\t"<< dz<<endl;
    }
  }
  cout<<"Write information to "<<filename<<endl;
  return True;
}

PHBoolean TofGeometryObject::writeToFile(const char* filename, const char* offsetfile){

  ofstream file(offsetfile);

  for(int i = 0; i < TOF_NSLAT_ALL; i++){
    float x  = pointOffset[i].getX();
    float y  = pointOffset[i].getY();
    float z  = pointOffset[i].getZ();
    float dx = vectorOffset[i].getX();
    float dy = vectorOffset[i].getY();
    float dz = vectorOffset[i].getZ();
    if(Debug>5){
      x  = TofSlatCenter[i].getX();
      y  = TofSlatCenter[i].getY();
      z  = TofSlatCenter[i].getZ();
      dx = TofSlatVector[i].getX();
      dy = TofSlatVector[i].getY();
      dz = TofSlatVector[i].getZ();
    }
    if(Debug>10){
      x  = getSlatXYZ(i).getX();
      y  = getSlatXYZ(i).getY();
      z  = getSlatXYZ(i).getZ();
      dx = getSlatVector(i).getX();
      dy = getSlatVector(i).getY();
      dz = getSlatVector(i).getZ();
    }
    file<<"  "<< i <<"\t"<< x <<"\t"<< y <<"\t"<< z 
	<<"\t"<< dx <<"\t"<< dy <<"\t"<< dz<<endl;
  }
  cout<<"Write information to "<<offsetfile<<endl;
  file.close();

  return TofGeometryObject::writeToFile(filename);
}

//----------------------------------------------------------------------
//  private function  [transplant from mTofSetGeo.c]
//  TofPanel, pointOffset, vectorOffset ===> TofSlat
//----------------------------------------------------------------------
PHBoolean 
TofGeometryObject::fromPanelToSlat(int ipanel_seq, PHPanel panel)
{


  TofPanel[ipanel_seq] = panel;

  PHPoint panelXYZ(panel.getCenter());
  PHVector panelX(panel.getNormal());

  PHLine panelYline(panel.getPoint(0),panel.getPoint(2));
  PHVector panelY = panelYline.getDirection();
  panelY.normalize();

  PHLine panelZline(panel.getPoint(0),panel.getPoint(1));
  PHVector panelZ = panelZline.getDirection();
  panelZ.normalize();

  for(int islat = 0; islat < TOF_NSLAT_PANEL; islat++){
    int id = TofAddress->getSlatID(ipanel_seq, islat); // slatid

    float rslat = TOF_RDIST_SLATPANEL;    // slat R-position from panel front surface
    float slat_width  = getSlatWidth(id); // slat width

    float slat_length;  // Slat length
    float scint_y;      // Slat local-Y position
    float scint_z;      // Slat local-Z position
    if((0<=islat)&&(islat<=15)){
      slat_length = TOF_SSLAT_LENGTH;
      scint_y = - TOF_SCINTZ_0;
      scint_z = slat_width*( -16 + 1.5 + (islat%16)*2 );
    } 
    else if((16<=islat)&&(islat<=31)){
      slat_length = TOF_LSLAT_LENGTH;
      scint_y = - TOF_SCINTZ_2;
      scint_z = slat_width*( -16 + 0.5 + (islat%16)*2 );
    } 
    else if((32<=islat)&&(islat<=47)){
      slat_length = TOF_LSLAT_LENGTH;
      scint_y = - TOF_SCINTZ_1;
      scint_z = slat_width*( -16 + 1.5 + (islat%16)*2 );
    } 
    else if((48<=islat)&&(islat<=63)){
      slat_length = TOF_LSLAT_LENGTH;
      scint_y = TOF_SCINTZ_1;
      scint_z = slat_width*( -16 + 0.5 + (islat%16)*2 );
    } 
    else if((64<=islat)&&(islat<=79)){
      slat_length = TOF_LSLAT_LENGTH;
      scint_y = TOF_SCINTZ_2;
      scint_z = slat_width*( -16 + 1.5 + (islat%16)*2 );
    } 
    else if((80<=islat)&&(islat<=95)){
      slat_length = TOF_SSLAT_LENGTH;
      scint_y = TOF_SCINTZ_0;
      scint_z = slat_width*( -16 + 0.5 + (islat%16)*2 );
    } 
    else {
      slat_length = TOF_LSLAT_LENGTH;
      scint_y = 0.;
      scint_z = 0.;
    }

    double pos[3];      // TOF slat position

    // set Slat center position from Panel position
    pos[0] = panelXYZ.getX() + rslat*panelX.getX()
      + scint_y*panelY.getX() + scint_z*panelZ.getX();
    pos[1] = panelXYZ.getY() + rslat*panelX.getY() 
      + scint_y*panelY.getY() + scint_z*panelZ.getY();
    pos[2] = panelXYZ.getZ() + rslat*panelX.getZ() 
      + scint_y*panelY.getZ() + scint_z*panelZ.getZ();

    PHPoint  slatCenter;

    slatCenter.setX(pos[0]);
    slatCenter.setY(pos[1]);
    slatCenter.setZ(pos[2]);

    PHVector slatVector = panelY;

    TofSlatCenter[id] = slatCenter + pointOffset[id];
    TofSlatVector[id] = slatVector + (PHVector)vectorOffset[id];
    TofSlatVector[id].normalize();

    PHVector direction = TofSlatVector[id]*(double)slat_length;
    PHPoint basepoint = TofSlatCenter[id] -  (PHPoint)(direction*0.5);

    TofSlat[id].setBasepoint(basepoint);
    TofSlat[id].setDirection(direction);
  }
  return True;
}

//----------------------------------------------------------------------
//  Print
//----------------------------------------------------------------------

void TofGeometryObject::print(){
  if(iFlag!=0) {
    cerr << "TofGeometryObject ERROR print: TofDGO not initialized." << endl;
  }
  cout<<" "<<endl;
  cout<<"##### TofGeometryObject #####"<<endl;
  cout<<" "<<endl;
  cout<<" PdbCal::"<<endl;
  cout<<"    Geom0Name  = "<< Geom0Name <<endl;
  cout<<"    Geom1Name  = "<< Geom1Name <<endl;
  cout<<"    BankNumber = "<< BankNumber <<endl;
  cout<<"    TimeStamp  = "<< Tsearch<<endl;
}

void TofGeometryObject::print(int id){
  if(iFlag!=0) {
    cerr << "TofGeometryObject ERROR print: TofDGO not initialized." << endl;
  }
  cout<<" "<<endl;
  cout<<"##### TofGeometryObject #####"<<endl;
  cout<<"  SLATID = "<< id <<endl;
  cout<<" "<<endl;
  cout<<"dTofGeo"<<endl;
  cout<<"  ARM    = "<< TofAddress->getArm(id) <<endl;
  cout<<"  SECTOR = "<< TofAddress->getSector(id) <<endl;
  cout<<"  SIDE   = "<< TofAddress->getSide(id) <<endl;
  cout<<"  PANEL  = "<< TofAddress->getPanel(id) <<endl;
  cout<<"  SLAT   = "<< TofAddress->getSlat(id) <<endl;
  cout<<"  panel_seq  = " << TofAddress->getPanelSeq(id) <<endl;
  cout<<"  panel_char = " << TofAddress->getPanelChar(id) <<endl;
  cout<<" "<<endl;
  cout<<"  EastCarriage: "<<endl; EastCarriage.print();
  cout<<" "<<endl;
  cout<<" PanelCenter(public) = "; 
  getPanelGeo(TofAddress->getPanelSeq(id)).getCenter().print();
  cout<<" PanelCenter(private)= "; 
  TofPanel[TofAddress->getPanelSeq(id)].getCenter().print();
  cout<<" SlatXYZ(public)     = "; getSlatXYZ(id).print();
  cout<<" SlatXYZ(private)    = "; TofSlatCenter[id].print();
  cout<<" SlatRPhiZ(public)   = "; getSlatRPhiZ(id).print();
  cout<<" SlatVector(public)  = "; getSlatVector(id).print();
  cout<<" SlatVector(private) = "; TofSlatVector[id].print();
  cout<<" SlatLength  = "<<getSlatLength(id)<<" [cm]  ";
  cout<<" SlatWidth   = "<<getSlatWidth(id)<<" [cm]" <<endl;
}
