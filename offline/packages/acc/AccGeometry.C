//-------------------------------------------------------------------
// Implementation of class AccGeometry
// Author : Hiroshi Masui (Univ. of Tsukuba)   e-mail : masui@bnl.gov
//
//
//-------------------------------------------------------------------

#include "PdbApplication.hh"
#include "PdbBankManager.hh"
#include "PdbCalBank.hh"
#include "PdbCoordinate.hh"

#include "AccGeometry.h"
#include "PHGeometry.h"
//INCLUDECHECKER: Removed this line: #include "PHTimeStamp.h"

#include <iostream>
#include <fstream>

using namespace std;

//_______________________________________________________________
AccGeometry::AccGeometry()
{
  //------------AccGeometry defalt constructor------------

  PHPoint p0(0.0, 0.0, 0.0); 
  PHPoint p1(1.0, 0.0, 0.0); 
  PHPoint p2(0.0, 1.0, 0.0); 
  PHVector v0(1.0, 0.0, 0.0);
  PHLine line0(p0, v0);
  PHPanel panel0(p0, p1, p2);
  PHFrame XYZ; // this build a default coordinate system
  DetectorFrame = XYZ;

  for(int i=0;i<ACC::ACC_NBOX;i++){
    AccPanel[i]      = panel0;
    AccPanelFrame[i] = XYZ;
  }

  AccGeometry::set_geomName("acc.geo.box");

  isGeoOK = -1;

  debug = 0;
}

//_______________________________________________________________
PHBoolean AccGeometry::setPanelFrame(int ipanel, PHFrame& frame)
{
  // Set each panel frame : AccPanelFrame[ipanel]

  AccPanelFrame[ipanel] = frame;
  return True;
}

//_______________________________________________________________
PHBoolean AccGeometry::setDetectorFrame(PHFrame& frame)
{
  // Set detector frame : "DetectorFrame"

  DetectorFrame = frame;
  return True;
}

//_______________________________________________________________
PHBoolean AccGeometry::setPanelGeo(int ipanel, PHPanel panel)
{
  // Set panel geometry : AccPanel[ipanel]

  AccPanel[ipanel] = panel;

  return True;
}

//_______________________________________________________________
PHBoolean AccGeometry::setPanelGeo(int ipanel, PHPoint p0, PHPoint p1, PHPoint p2)
{
  // Set panel geometry (AccPanel[ipanel]) from 3 PHPoint p0,p1,p2.

  PHPanel panel(p0, p1, p2);

  if(debug){ // for debug
    cout << "AccGeometry::setPanelGeo(), ipanel = " << ipanel << " "
         << "center = " << panel.getCenter() << " "
         << "normal = " << panel.getNormal() << endl;
  }

  return AccGeometry::setPanelGeo(ipanel, panel);
}

//_______________________________________________________________
PHBoolean AccGeometry::setPanelErr(int ipanel, PHPanel panel)
{
  // Set panel geometry : AccPanelErr[ipanel]

  AccPanelErr[ipanel] = panel;

  return True;
}

//_______________________________________________________________
PHBoolean AccGeometry::setPanelErr(int ipanel, PHPoint p0, PHPoint p1, PHPoint p2)
{
  // Set panel geometry (AccPanelErr[ipanel]) from 3 PHPoint p0,p1,p2.

//  PHPanel panel(p0, p1, p2);

//  if(debug){ // for debug
//    cout << "AccGeometry::setPanelErr(), ipanel = " << ipanel << " "
//         << "center = " << panel.getCenter() << " "
//         << "normal = " << panel.getNormal() << endl;
//  }

  panelErr[0][ipanel] = p0;
  panelErr[1][ipanel] = p1;
  panelErr[2][ipanel] = p2;

  return True;
//  return AccGeometry::setPanelErr(ipanel, panel);
}

//_______________________________________________________________
PHBoolean AccGeometry::fetch(const int run)
{

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();

  if(application->startRead()){
    PdbBankID bankID(ACC::ACC_BANKID_GEOM);

    PdbCalBank* accBank = bankManager->fetchBank("PdbCoordinateBank", bankID, geomName.getString(), run);

    if(accBank){
      if(debug){
        accBank->print();
	cout << "# of channels = " << accBank->getLength() << endl;
      }

      const int np = 3;
      PHPoint p[np];
      PHPoint dp[np];

      for(int ibox=0;ibox<ACC::ACC_NBOX;ibox++){
        int panel = ibox;
        for(int ip=0;ip<np;ip++){
          int ic = panel*np + ip;

          PdbCoordinate* coordinate = (PdbCoordinate*)&(accBank->getEntry(ic));
	  float x = coordinate->getParameter(0);
	  float y = coordinate->getParameter(1);
	  float z = coordinate->getParameter(2);
	  float dx = coordinate->getParError(0);
	  float dy = coordinate->getParError(1);
	  float dz = coordinate->getParError(2);

          p[ip].setX(x); dp[ip].setX(dx);
          p[ip].setY(y); dp[ip].setY(dy);
          p[ip].setZ(z); dp[ip].setZ(dz);

	  if(ip==2){
            if(debug){
              cout << "box = " << panel << endl;
              cout << "p0 "; p[0].print();
              cout << "p1 "; p[1].print();
              cout << "p2 "; p[2].print();
            }

            setPanelGeo(panel, p[0], p[1], p[2]);
            setPanelErr(panel, dp[0], dp[1], dp[2]);
	  }
	  
        }
      }
      delete accBank;
    }
  }

  isGeoOK = 0;

  return True;
}

//_______________________________________________________________
PHBoolean AccGeometry::fetch(const char* filename)
{
  // Fetch geometry from ascii file.

  ifstream fin(filename);
  if(!fin){
    cout << PHWHERE << " can't open " << filename << endl;
    return False;
  }

//  while(fin){
  for(int i=0;i<ACC::ACC_NBOX;i++){
    int panel,point;
    double x,y,z;
    PHPoint p0,p1,p2;

    for(int j=0;j<3;j++){
      fin >> panel >> point >> x >> y >> z;
 
      cout << "panel " << panel << " point " << point 
           << " x " << x 
           << " y " << y 
           << " z " << z << endl; 

      if(point == 0){
        p0.setX(x); 
        p0.setY(y); 
        p0.setZ(z); 
      }
      else if(point == 1){
        p1.setX(x); 
        p1.setY(y); 
        p1.setZ(z); 
      }
      else if(point == 2){
        p2.setX(x); 
        p2.setY(y); 
        p2.setZ(z); 

        if( debug ){
          cout << "panel = " << panel << endl;
          cout << "p0 = "; p0.print();
          cout << "p1 = "; p1.print();
          cout << "p2 = "; p2.print();
        }

        // set panel geometry
        setPanelGeo(panel, p0, p1, p2);
      }
    }
  }
  cout << endl << "Fetch geometry information from " << filename << " [ASCII file]" << endl;
  fin.close();

  isGeoOK = 0;

  fromFile = True;
  return fromFile;

}

//_______________________________________________________________
PHBoolean AccGeometry::write(const char* filename)
{
  // Write geometry information to ascii file.

  ofstream fout(filename);

  for(int i=0;i<ACC::ACC_NBOX;i++){
    int panel = i;

    for(int ip=0;ip<3;ip++){
      double x = AccPanel[panel].getPoint(ip).getX();
      double y = AccPanel[panel].getPoint(ip).getY();
      double z = AccPanel[panel].getPoint(ip).getZ();
      double dx = 0.0;
      double dy = 0.0;
      double dz = 0.0;

      fout << "  " << panel << "  " << ip 
           << "\t" << x  << "\t" << y  << "\t" << z 
           << "\t" << dx << "\t" << dy << "\t" << dz
	   << endl;
    }
  }
  cout << "Write geometry information to " << filename << endl;
  fout.close();

  return True;
}

//_______________________________________________________________
PHBoolean AccGeometry::update(PHTimeStamp& tStart, PHTimeStamp& tStop)
{

  PdbBankManager* bankManager = PdbBankManager::instance();
  PdbApplication* application = bankManager->getApplication();
  cout << "opening FD in update mode " << geomName.getString() << endl;

  if(application->startUpdate()){
    PdbBankID bankID(ACC::ACC_BANKID_GEOM);

    PdbCalBank* accBank = bankManager->createBank("PdbCoordinateBank",
                                                  bankID,
						  AccGeometry::getDescription().getString(),
                                                  tStart,
                                                  tStop,
                                                  geomName.getString() );
    const int np = 3;
    const int total = np * ACC::ACC_NBOX;

    accBank->setLength(total);
    if(debug) accBank->print();

    for(int ibox=0;ibox<ACC::ACC_NBOX;ibox++){
      int panel = ibox;
      for(int ip=0;ip<np;ip++){
        float x  = (float)AccGeometry::getPanelGeo(panel).getPoint(ip).getX();
        float y  = (float)AccGeometry::getPanelGeo(panel).getPoint(ip).getY();
        float z  = (float)AccGeometry::getPanelGeo(panel).getPoint(ip).getZ();
        float dx = (float)AccPanelErr[panel].getPoint(ip).getX();
        float dy = (float)AccPanelErr[panel].getPoint(ip).getY();
        float dz = (float)AccPanelErr[panel].getPoint(ip).getZ();
  
        int ic = panel * np + ip;
        PdbCoordinate* coordinate = (PdbCoordinate*)&(accBank->getEntry(ic));
        coordinate->setParameter(0, x);
        coordinate->setParameter(1, y);
        coordinate->setParameter(2, z);
        coordinate->setParError(0, dx);
        coordinate->setParError(1, dy);
        coordinate->setParError(2, dz);

        if(debug) accBank->printEntry(ic);
      }
    }

    // please put user name by hand
    char username[20];
    cout << endl << "Please put your name" << endl;
    cin.getline(username,20);
    PHString UserName(username);
    accBank->setUserName(UserName);

    application->commit(accBank);
  }

  return True;
}


//_______________________________________________________________
PHPoint AccGeometry::fromLocalToGlobal(int ipanel, PHPoint& local)
{
  // The local data gives the position along the box
  // assuming the box is centered at z = 0
  // now define a box frame and transform to it

  PHPanel panel  = AccGeometry::getPanelGeo(ipanel);
  PHPoint origin = panel.getCenter();
  PHVector Xaxis = panel.getNormal();
  PHVector Yaxis = (PHVector)(panel.getPoint(1) - panel.getPoint(0));

  PHFrame localFrame(origin, Xaxis, Yaxis);
  PHFrame globalFrame; // set to default

  return PHGeometry::transformPoint(localFrame, local, globalFrame);
}

//_______________________________________________________________
float AccGeometry::getX(const int ibox) const
{
  if(isGeoOK!=0){
    cout << PHWHERE << " AccDGO not initialized " << endl;
    return -9999.;
  }
  if( ibox < 0 || ibox >= ACC::ACC_NBOX ){
    cout << PHWHERE << " Invalid box number , ibox= " << ibox << endl;
    return -9999.;
  }

  return AccPanel[ibox].getCenter().getX();
}

//_______________________________________________________________
float AccGeometry::getY(const int ibox) const
{
  if(isGeoOK!=0){
    cout << PHWHERE << " AccDGO not initialized " << endl;
    return -9999;
  }
  if( ibox < 0 || ibox >= ACC::ACC_NBOX ){
    cout << PHWHERE << " Invalid box number , ibox= " << ibox << endl;
    return -9999.;
  }

  return AccPanel[ibox].getCenter().getY();
}

//_______________________________________________________________
float AccGeometry::getZ(const int ibox) const
{
  if(isGeoOK!=0){
    cout << PHWHERE << " AccDGO not initialized " << endl;
    return -9999;
  }
  if( ibox < 0 || ibox >= ACC::ACC_NBOX ){
    cout << PHWHERE << " Invalid box number , ibox= " << ibox << endl;
    return -9999.;
  }

  return AccPanel[ibox].getCenter().getZ();
}

//_______________________________________________________________
void AccGeometry::print()
{
  // Print geometry information

  if(isGeoOK != 0){
    cout << PHWHERE << " AccDGO not initializaed " << endl << endl;
    return;
  }

  if(fromFile){
    cout << "##### " << AccGeometry::get_geomName()
         << " Initialized from file ##### " << endl << endl;
  }
  else{
    cout << " PdbCal::"<<endl; 
    cout << "\t geomName   = " << geomName << endl;
    cout << "\t bankName   = " << ACC::Name() << endl;
    cout << "\t BankNumber = " << ACC::ACC_BANKID_GEOM << endl;
  }

  for(int i=0;i<ACC::ACC_NBOX;i++){
    AccGeometry::print(i);
  }

  return;
}

//_______________________________________________________________
void AccGeometry::print(const int ibox)
{
  if( ibox < 0 || ibox >= ACC::ACC_NBOX ){
    cout << PHWHERE << " Invalid box number , ibox= " << ibox << endl;
    return;
  }

  cout << " BOX # " << ibox << " ";
  AccGeometry::getPanelGeo(ibox).print();

  return;
}

