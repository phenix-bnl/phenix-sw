// Class: hbdDetectorGeo (implementation)
// Created by: Ilia Ravinovich
// Description: Class that generates the pad chamber geometry from
//              either a default, or from the information in the database
// last update: 04/15/03 simulation geometry in OBJY database, Indrani Ojha
// modified by C. Aidala and A. Sickles to use the database 3/6/2006
// modified by S. Rolnick to incorporate pad coordinates. 
//-------------------------------------------------------------------------
#include <iostream>
#include <fstream>
#include <hbdDetectorGeo.hh>
#include <Hbd.h>
#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHNodeIterator.h>
#include <PdbApplication.hh>
#include <PdbBankManager.hh>
#include <PdbCalBank.hh>
#include <PdbCoordinate.hh>
#include <PHTimeStamp.h>
#include <PHGeometry.h>

using std::cout;
using std::cin;
using std::endl;

//
// Default Constructor for hbdDetectorGeo
//
hbdDetectorGeo::hbdDetectorGeo()
{
	// make this flag settable from the outside
	// so you do not piss everybody off with your blabbering
	debug=0;
	int i, j;
	
	Verbose = 0;
	for (i = 0; i < 2; i++)
    {
		for (j = 0; j < HBD::HBD_NSECTARM; j++)
        {
			hbdActive[i][j] = 0;
			hbdSectors[i][j] = PHPanel();
        }
    } 
	PadInfo();		
}

//
// Destructor for hbdDetectorGeo
//
hbdDetectorGeo::~hbdDetectorGeo()
{
}

//
// Build the active HBD detector geometry
//

short
hbdDetectorGeo::get_hbdGeo(short arm, PHPanel outPlane[])
{
	short isect, nSector;
	nSector = 0;
	for (isect = 0; isect < HBD::HBD_NSECTARM; isect++)
    {
		if (hbdActive[arm][isect] == 1)
        {
			outPlane[nSector] = hbdSectors[arm][isect];
			nSector++;
        }
    }
	return nSector;
}


//
// Pad Info
//
void
hbdDetectorGeo::getPadInfo(int ADCch, int &arm, int &sec,
						   int &seqsec, int&padid)
{
	arm = padgeom[ADCch].arm;
	sec = padgeom[ADCch].sec;
	seqsec = padgeom[ADCch].seqsec;
	padid = padgeom[ADCch].padid;
	
}

//
// Fetch the geometry information from the database
//
PHBoolean
hbdDetectorGeo::fetch(const int run)
{
	
	const char *geomName = "geom.hbd.sectors";
	PdbBankManager* bankManager = PdbBankManager::instance();
	PdbApplication* application = bankManager->getApplication();
	if(application->startRead()){
		PdbBankID bankID(1);
		PdbCalBank* hbdBank = bankManager->fetchBank("PdbCoordinateBank", bankID, geomName, run);
		
		if(hbdBank){
			if(debug){
				hbdBank->print();
				cout << "# of channels = " << hbdBank->getLength() << endl;
			}
			const int np = 3;
			PHPoint p[np];
			
			for(int isect=0;isect<HBD::HBD_NSECT;isect++){
				int panel = isect;
				
				for(int ip=0;ip<np;ip++){
					int ic = panel*np + ip;
					PdbCoordinate* coordinate = (PdbCoordinate*)&(hbdBank->getEntry(ic));
					
					double x = coordinate->getParameter(0);
					double y = coordinate->getParameter(1);
					double z = coordinate->getParameter(2);
					p[ip].setX(x); 
					p[ip].setY(y);
					p[ip].setZ(z);
					
					if(ip==2){
						if(debug){
							cout << "box = " << panel << endl;
							cout << "p0 "; p[0].print();
							cout << "p1 "; p[1].print();
							cout << "p2 "; p[2].print();
						}
						PHPanel pan(p[0], p[1], p[2]);
						setPanelGeo(panel, pan);
					}
				}
			}
			delete hbdBank;
		}
	}
	return True;
}


PHBoolean
hbdDetectorGeo::fetchPad(const int run)
{
	
	const char *padName = "geom.hbd.pads";
	PdbBankManager* bankManager = PdbBankManager::instance();
	PdbApplication* application = bankManager->getApplication();
	if(application->startRead()){
		PdbBankID bankID(1);
		PdbCalBank *hbdBank = bankManager->fetchBank("PdbCoordinateBank", bankID, padName, run);
		
		if(hbdBank){
			if(debug){
				hbdBank->print();
				cout << "# of ADCchs = " << hbdBank->getLength() << endl;
			}
			
			for(int iADC=0;iADC<2304;iADC++){
				PdbCoordinate* coordinate = (PdbCoordinate*)&(hbdBank->getEntry(iADC));
				
				double arm = coordinate->getParameter(0);
				double sec = coordinate->getParameter(1);
				double padid = coordinate->getParameter(2);   // be careful. returns pad 1-192!
				double LocY = pad_center[(int)padid-1][1];
				double LocZ = pad_center[(int)padid-1][0];
				double globX;
				double globY;
				double globZ;
				int    sector = (int) (arm*HBD::HBD_NSECTARM + sec);
				LocToGlob(LocY, LocZ, globX, globY, globZ, sector);
				
				padgeom[iADC].ADCch = iADC;
				padgeom[iADC].arm = (int)arm;
				padgeom[iADC].sec = (int)sec;
				padgeom[iADC].seqsec = (int)(arm*HBD::HBD_NSECTARM+sec);
				padgeom[iADC].padid = (int)padid;	// be careful. returns pad 1-192!
				// new stuff
				padgeom[iADC].CenterInLocalY = (double) LocY;
				padgeom[iADC].CenterInLocalZ = (double) LocZ;
				padgeom[iADC].globalX = (double) globX;
				padgeom[iADC].globalY = (double) globY;
				padgeom[iADC].globalZ = (double) globZ;
				
				if(debug){
					cout << "ADC = " << iADC << " ";
					cout << "arm = " << arm << " ";
					cout << "sec = " << sec << " ";
					cout << "seqsec = " << (int)(arm*HBD::HBD_NSECTARM+sec) << " ";
					cout << "padid = " << padid - 1<< endl;  // valid range 0-191
				}
			}
			delete hbdBank;
		}
	}
	return True;
}

PHBoolean
hbdDetectorGeo::fetchPad(PHTimeStamp& tNow)
{
	
	const char *padName = "geom.hbd.pads";
	PdbBankManager* bankManager = PdbBankManager::instance();
	PdbApplication* application = bankManager->getApplication();
	if(application->startRead()){
		PdbBankID bankID(1);
		PdbCalBank *hbdBank = bankManager->fetchBank("PdbCoordinateBank", bankID, padName, tNow);
		
		if(hbdBank){
			if(debug){
				hbdBank->print();
				cout << "# of ADCchs = " << hbdBank->getLength() << endl;
			}
			
			for(int iADC=0;iADC<2304;iADC++){
				PdbCoordinate* coordinate = (PdbCoordinate*)&(hbdBank->getEntry(iADC));
				
				double arm = coordinate->getParameter(0);
				double sec = coordinate->getParameter(1);
				double padid = coordinate->getParameter(2);   // be careful. returns pad 1-192!
				double  LocY = pad_center[(int)padid-1][1];
				double LocZ = pad_center[(int)padid-1][0];
				double globX;
				double globY;
				double globZ;
				int   sector = (int) (arm*HBD::HBD_NSECTARM + sec);
				LocToGlob(LocY, LocZ, globX, globY, globZ, sector);
				
				
				padgeom[iADC].ADCch = iADC;
				padgeom[iADC].arm = (int)arm;
				padgeom[iADC].sec = (int)sec;
				padgeom[iADC].seqsec = (int)(arm*HBD::HBD_NSECTARM+sec);
				padgeom[iADC].padid = (int)padid;
				padgeom[iADC].CenterInLocalY = (double) LocY;
				padgeom[iADC].CenterInLocalZ = (double) LocZ;
				padgeom[iADC].globalX = (double) globX;
				padgeom[iADC].globalY = (double) globY;
				padgeom[iADC].globalZ = (double) globZ;
				
				if(debug){
					cout << "ADC = " << iADC << " ";
					cout << "arm = " << arm << " ";
					cout << "sec = " << sec << " ";
					cout << "seqsec = " << (int)(arm*HBD::HBD_NSECTARM+sec) << " ";
					cout << "padid = " << padid - 1 << endl;  // print out pads 0-191
				}
			}
			delete hbdBank;
		}
	}
	return True;
}

//
// Put the geometry information into the database
//
///////////////////////////////////////////////////////////////////////////

PHBoolean hbdDetectorGeo::update(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
	
	const char *geomName = "geom.hbd.sectors";
	const char *hbddescript = "HBD geometry information";
	PdbBankManager* bankManager = PdbBankManager::instance();
	PdbApplication* application = bankManager->getApplication();
	cout << "opening db in update mode " << geomName << endl;
	
	if(application->startUpdate()){
		PdbBankID bankID(1); // bankID used at subsystem discretion.  Currently not for hbd. 
		PdbCalBank* hbdBank = bankManager->createBank("PdbCoordinateBank",
													  bankID,
													  hbddescript,
													  tStart,tStop,	 
													  geomName );
		const int np = 3;
		const int total = np * HBD::HBD_NSECT;
		
		hbdBank->setLength(total);
		if(debug) hbdBank->print();
		for(int isect=0;isect<HBD::HBD_NSECT;isect++){
			int panel = isect;
			for(int ip=0;ip<np;ip++){
				double x  = (double)hbdDetectorGeo::getPanelGeo(panel).getPoint(ip).getX();
				double y  = (double)hbdDetectorGeo::getPanelGeo(panel).getPoint(ip).getY();
				double z  = (double)hbdDetectorGeo::getPanelGeo(panel).getPoint(ip).getZ();
				int ic = panel * np + ip;
				PdbCoordinate* coordinate = (PdbCoordinate*)&(hbdBank->getEntry(ic));
				coordinate->setParameter(0, x);
				coordinate->setParameter(1, y);
				coordinate->setParameter(2, z);
				if(debug) hbdBank->printEntry(ic);
			}
		}
		// please put user name by hand
     	char username[20];
		cout << endl << "Please put your name" << endl;
		cin.getline(username,20);
		PHString UserName(username);
		hbdBank->setUserName(UserName);
		application->commit(hbdBank);
	}
	return True;
}


PHBoolean hbdDetectorGeo::updatePad(PHTimeStamp& tStart, PHTimeStamp& tStop)
{
	
	const char *padName = "geom.hbd.pads";
	const char *paddescript = "HBD pad information";
	PdbBankManager* bankManager = PdbBankManager::instance();
	PdbApplication* application = bankManager->getApplication();
	cout << "opening db in update mode " << padName << endl;
	
	if(application->startUpdate()){
		PdbBankID bankID(1); // bankID used at subsystem discretion.  Currently not for hbd. 
		PdbCalBank* hbdBank = bankManager->createBank("PdbCoordinateBank",
													  bankID,
													  paddescript,
													  tStart,tStop,	 
													  padName );
		hbdBank->setLength(2304);
		if(debug) hbdBank->print();
		for(int iADC=0;iADC<2304;iADC++){
			PdbCoordinate* coordinate = (PdbCoordinate*)&(hbdBank->getEntry(iADC));
			coordinate->setParameter(0, padgeom[iADC].arm);
			coordinate->setParameter(1, padgeom[iADC].sec);
			coordinate->setParameter(2, padgeom[iADC].padid);
			if(debug) hbdBank->printEntry(iADC);
		}
		// please put user name by hand
     	char username[20];
		cout << endl << "Please put your name" << endl;
		cin.getline(username,20);
		PHString UserName(username);
		hbdBank->setUserName(UserName);
		application->commit(hbdBank);
	}
	return True;
}

void
hbdDetectorGeo::readGeoFromFile(const char *filename)
{
  std::ifstream fgeo;
	fgeo.open(filename);
	int i=0, j=0;
	int arm, sect, tmpin, nsect=0;
	double tmpx, tmpy, tmpz;
	PHPoint p[3];
	while(1){
		if(i==0) fgeo >> tmpin;
		else if((i-1)%5==0){
			fgeo >> arm >> sect;
			nsect = sect + arm*HBD::HBD_NSECTARM; //six sectors/arm
			j=0;
		}
		else{
			fgeo >> tmpx >> tmpy >> tmpz;
			if(j<3){
				p[j].setX(tmpx);
				p[j].setY(tmpy);
				p[j].setZ(tmpz);
			}else{
				PHPanel tmppan(p[0],p[1],p[2]);
				setPanelGeo(nsect, tmppan);
			}
			j++;
		}
		if(!fgeo.good())break;
		i++;
	}
}

void
hbdDetectorGeo::readPadInfoFromFile(const char *filename)
{
	int tmpADCch,tmparm,tmpsec,tmppadid;
	double tmplocy, tmplocz, tmpglobx, tmpgloby, tmpglobz;
        std::ifstream fpad(filename);
	while(fpad>>tmpADCch>>tmparm>>tmpsec>>tmppadid>>tmplocy>>tmplocz>>tmpglobx>>tmpgloby>>tmpglobz)
	{
		padgeom[tmpADCch].ADCch = tmpADCch;
		padgeom[tmpADCch].arm = tmparm;
		padgeom[tmpADCch].sec = tmpsec;
		padgeom[tmpADCch].seqsec = tmpsec+tmparm*HBD::HBD_NSECTARM;
		padgeom[tmpADCch].padid = tmppadid;
		padgeom[tmpADCch].CenterInLocalY = tmplocy;
		padgeom[tmpADCch].CenterInLocalZ = tmplocz;
		padgeom[tmpADCch].globalX = tmpglobx;
		padgeom[tmpADCch].globalY = tmpgloby;
		padgeom[tmpADCch].globalZ = tmpglobz;
	}
}

void
hbdDetectorGeo::set_Verbose(short setverb)
{
	Verbose = setverb;
}

short
hbdDetectorGeo::get_Verbose()
{
	return Verbose;
}

void
hbdDetectorGeo::set_hbdActive(short arm, short hbdin[])
{
	int i;
	for (i = 0; i < HBD::HBD_NSECTARM; i++)
    {
		hbdActive[arm][i] = hbdin[i];
    }
}

void
hbdDetectorGeo::get_hbdActive(short arm, short hbdout[])
{
	int i;
	for (i = 0; i < HBD::HBD_NSECTARM; i++)
    {
		hbdout[i] = hbdActive[arm][i];
    }
}

PHBoolean 
hbdDetectorGeo::setPanelGeo(int sect, const PHPanel& ipanel)
{
	int arm=0, isect=0;
	if(sect<HBD::HBD_NSECTARM){
	  arm = 0;
	  isect = sect;
	}else{
	  arm=1;
	  isect = sect-HBD::HBD_NSECTARM;
	}
	hbdSectors[arm][isect] = ipanel;
	return True;
}

const PHPanel&
hbdDetectorGeo::getPanelGeo(int ipanel)
{  
	int arm=0, isect=0;
	if(ipanel<HBD::HBD_NSECTARM){
		arm = 0;
		isect = ipanel;
	}else{
		arm=1;
		isect = ipanel-HBD::HBD_NSECTARM;
	}
	return hbdSectors[arm][isect]; 
}

void 
hbdDetectorGeo::printGeo()
{
	for(int i=0; i<2; i++){
		for(int j=0; j<HBD::HBD_NSECTARM; j++){
			hbdSectors[i][j].print();
		}
	}
}


void 
hbdDetectorGeo::printPadInfo()
{
	for(int i=0; i<2304; i++){
		cout <<"ADCch: " << padgeom[i].ADCch;
		cout <<", arm: " << padgeom[i].arm;
		cout <<", sec: " << padgeom[i].sec;
		cout <<", seqsec: " << padgeom[i].seqsec;
		cout <<", PadID: " << padgeom[i].padid -1; // Always print pads 0-191
		cout <<", LocY: " << padgeom[i].CenterInLocalY;
		cout <<", LocZ: " << padgeom[i].CenterInLocalZ;
		cout <<", GlobX: " << padgeom[i].globalX;
		cout <<", GlobY: " << padgeom[i].globalY;
		cout <<", GlobZ: " << padgeom[i].globalZ;
		
		cout <<endl;
	}
}


int
hbdDetectorGeo::LocToGlob(double LocY,
                          double LocZ,
                          double& GlobX,
                          double& GlobY,
                          double& GlobZ,
                          int ipanel)
{

  if (ipanel  < 0)
    {
      cout << PHWHERE << "Error with ipanel: " << ipanel << endl;
      return -1;
    }
  const PHPanel& NowPanel = getPanelGeo(ipanel);
  const PHTriPanel& panel1 = NowPanel.getPanel1();

  // Obtain OriginalPoint
  const PHVector& OrgPoint = NowPanel.getOrigin();

  // Obtain Y, Z norm vector in global coordinate
  // And, normalize them
  PHVector LocalYInGlobal = panel1.getVectorV1();
  PHVector LocalZInGlobal = panel1.getVectorV2();
  LocalYInGlobal.normalize();
  LocalZInGlobal.normalize();

  // Corrects for differing coordinate centers...
  double LY = LocY + 11.45; //this is half the width of the PCB and accounts
  double LZ = ipanel < 6 ? LocZ + 31.5  : -LocZ + 31.5 ;
  PHVector glCoord = OrgPoint
    + LocalYInGlobal * LY
    + LocalZInGlobal * LZ;

  GlobX = glCoord.getX();
  GlobY = glCoord.getY();
  GlobZ = glCoord.getZ();

  if (Verbose)
    {
      cout << LocY << " " ;
      cout << LocZ << " " ;
      cout << LY << " " ;
      cout << LZ << " " ;
      cout << GlobX << " " ;
      cout << GlobY << " " ;
      cout << GlobZ << " " << endl;
    }
  return 0;
}

void
hbdDetectorGeo::GlobToLoc(double globX,
                          double globY,
                          double globZ,
                          double& LocY,
                          double& LocZ,
			  int& LocSect)
{

   int ipanel;
   double GlobX = globX;
   double GlobY = globY;
   double GlobZ = globZ;
   FindNearestPanel(GlobX,  GlobY,  GlobZ, ipanel);
   if(!(ipanel>0)) { LocY = -9999.; LocZ = -9999.; LocSect = 0; return;} // cannot convert coords
   if( (globX!=globX || globY!=globY || globZ!= globZ) ) { LocY = -9999.; LocZ = -9999.; LocSect = 0; return;} // cannot convert coords
   const PHPanel& NowPanel = getPanelGeo(ipanel);
   const PHTriPanel& panel1 = NowPanel.getPanel1();
   LocSect = ipanel;
   if(Verbose){

      cout << PHWHERE << "FindNearestPanel: panel is " << LocSect << endl;
   }	
	// Obtain OriginalPoint
	const PHVector& OrgPoint = NowPanel.getOrigin();

	// Obtain Y, Z norm vector in global coordinate
	// And, normalize them
	PHVector LocalYInGlobal = panel1.getVectorV1();
	PHVector LocalZInGlobal = panel1.getVectorV2();
	LocalYInGlobal.normalize();
	LocalZInGlobal.normalize();
	
	// Conversion
	const PHVector glCoord(GlobX,GlobY,GlobZ);

	const PHVector loCoord = glCoord-OrgPoint;

	// Just take dot products
	double LY = loCoord.dot(LocalYInGlobal); //see comment in LocToGlob
	double LZ = loCoord.dot(LocalZInGlobal); //see comment in LocToGlob

	// Make up for difference in origin and orientation...
	LocY = LY - 11.45;
	LocZ = ipanel<6 ? LZ - 31.5  : -LZ + 31.5 ;

	if (Verbose) 
	  {
	    cout << LocY << " " ;
	    cout << LocZ << " " ;
	    cout << LY << " " ;
	    cout << LZ << " " ;
	    cout << GlobX << " " ;
	    cout << GlobY << " " ;
	    cout << GlobZ << " " << endl;
	  }

}





void
hbdDetectorGeo::FindNearestPad(double locY, double locZ, int & padid)
{
        double LocY = locY;
  	double LocZ = locZ;	
	double dist=1e42, temp;
	double padY, padZ;
	padid = 0;	
	for (int ipad = 0; ipad < 192; ipad ++)
	{
		padY = pad_center[ipad][1];
		padZ = pad_center[ipad][0];

		temp = (LocY-padY)*(LocY-padY)+(LocZ-padZ)*(LocZ-padZ);

		if(temp<=dist)
		{
			dist=temp;
			padid = ipad;
		}
		
	}

	if (Verbose) {
          std::cout << "Closest pad is " << padid << " " ;
          std::cout << "with distance ..." << std::sqrt(dist) << std::endl;
	}

	return;
}


void
hbdDetectorGeo::FindNearestPad(double GlobX,
			   double GlobY,	double GlobZ, int & padid)
{
	double LocY, LocZ;	
	double globX = 0;
	double globY = 0;
	double globZ = 0;
	
	globX = GlobX;
	globY = GlobY;
	globZ = GlobZ;
	
	// First find nearest panel.
	int ipanel;
	padid = 0;
	FindNearestPanel(globX,  globY,  globZ, ipanel);

	// Convert Global to Local coords based on panel. 
	GlobToLoc(globX,globY,globZ, LocY, LocZ, ipanel);
	
	// Search only all 192 pads on a given panel for closest pad.
	FindNearestPad(LocY, LocZ, padid);
	
	if(Verbose){
	
	cout << "Finding Nearest Pad ... " << endl;
	cout << "padid: " << padid << " sect: " << ipanel;	
	}
	
	return;
}

void hbdDetectorGeo::FindNearestPanel(double X, double Y, double Z, int & ipanel)
{
	double dist = 9999;
	double x, y, z;
	x = X;
	y = Y;
	z = Z;
	ipanel =0;

	if(Verbose){

	cout << PHWHERE << "FindNearestPanel : " ;
	cout << " X " << x ;
	cout << " Y " << y ;
	cout << " Z " << z ;
	cout << endl;
	}
	
	for (int i = 0; i< HBD::HBD_NSECT; i++)
	  {
	    // Only call DistanceFromPanel ONCE!!!
	    double dist1 = DistanceFromPanel(x,y,z, i);
	    if(dist1<dist)
	      {
		dist = dist1;
		ipanel = i;
	      }
	  }
	
	if ( !(ipanel >= 0 && ipanel < HBD::HBD_NSECT) && debug ) {
        cout << PHWHERE << "Finding Nearest Panel... " ;
	cout << "Something wrong. Panel not found. " << endl;	

	}
	if(Verbose){
	
	cout << PHWHERE << "Finding Nearest Panel... " ;
	cout << " sect: " << ipanel << endl;
	}
	
	
	return;
}


double hbdDetectorGeo::DistanceFromPanel(double X,double Y, double Z, int ipanel)
{
	// Quicker calculation of distance:
	PHPanel crap = getPanelGeo(ipanel);
	double dist2 = fabs(crap.getNormal().dot(PHVector(X,Y,Z)) + crap.getD());

	if (Verbose)
	  {
	    cout << PHWHERE << "Distance to Panel: " << dist2 << endl;
	  }


	return dist2;
}




// change this to PanelToSect
void hbdDetectorGeo::PanelToSect(int ipanel, short & arm, short & sector)
{
	
	if(ipanel<HBD::HBD_NSECTARM){
		arm=0;
		sector = ipanel;
	}else{
		arm=1;
		sector=ipanel-HBD::HBD_NSECTARM;
	}
	return;
}


void hbdDetectorGeo::getPadLocal(int padnum, double & LocY, double & LocZ)
{
	// valid range of padnum is 0-191
	if(padnum >=0 && padnum < 192){
		//LocY = pad_center[padnum][1]+22.9*sect;  // Panel width is 22.9cm
        LocY = pad_center[padnum][1];
		LocZ = pad_center[padnum][0];
		//cout << "LocY: "<< LocY << " , " << "LocZ: " << LocZ << endl;
	}
	else{
		cout << PHWHERE << "Error: Padnum and sector are out of range." << endl;
		
	}
	return;
}

void hbdDetectorGeo::getPadGlobal(int padnum, int sect, double & GlobX, double & GlobY, double & GlobZ)
{
	// valid range of padnum is 0-191
	if(padnum >=0 && padnum < 192 && sect >=0 && sect < 12){
		double LocY, LocZ;
		getPadLocal(padnum, LocY, LocZ);
		//cout << "LocY : " << LocY << " LocZ : " << LocZ << endl;
		
		LocToGlob(LocY, LocZ, GlobX, GlobY, GlobZ, sect);
		
		//cout << "GlobX : " << GlobX << " GlobY : " << GlobY <<  " GlobZ : " << GlobZ<< endl;
		
	}
	else{
		cout << PHWHERE << "Error: Padnum and sector are out of range." << endl;
		
	}
	return;
}






/*	Use the local PCB pad coordinates to get the global coordinates. 
 */

void hbdDetectorGeo::PadInfo()
{
	for(int i=0;i<192;i++)
	{
		pad_type[i]=0;
	}
	
	pad_type[19]=12;  pad_type[36]=12;  pad_type[53]=12;
	pad_type[70]=12;  pad_type[104]=12; pad_type[121]=12;
	pad_type[138]=12; pad_type[155]=12; pad_type[172]=12;
	
	pad_type[27]=11;  pad_type[44]=11;  pad_type[61]=11;
	pad_type[78]=11;  pad_type[112]=11; pad_type[129]=11;
	pad_type[146]=11; pad_type[163]=11; pad_type[180]=11;
	
	pad_type[181]=31; pad_type[182]=31; pad_type[183]=31;
	pad_type[184]=31; pad_type[185]=31; pad_type[186]=31;
	pad_type[187]=31; pad_type[188]=31; pad_type[88]=32;
	pad_type[89]=32;  pad_type[90]=32;  pad_type[91]=32;
	pad_type[92]=32;  pad_type[93]=32;  pad_type[94]=32;
	
	pad_type[96]=33;  pad_type[97]=33;  pad_type[98]=33;
	pad_type[99]=33;  pad_type[100]=33; pad_type[101]=33;
	pad_type[102]=33; pad_type[103]=33;
	
	pad_type[3]=34;
	pad_type[4]=34;   pad_type[5]=34;   pad_type[6]=34;
	pad_type[7]=34;   pad_type[8]=34;   pad_type[9]=34;
	
	pad_type[2]=62;   pad_type[10]=61;  pad_type[87]=64;
	pad_type[95]=63;  pad_type[0]=52;   pad_type[1]=52;
	pad_type[189]=51; pad_type[190]=51; pad_type[191]=51;
	
	// This is for Local Y      // This is for Local Z
	pad_center[0][1]=-5.445;    pad_center[0][0]=-26.532;
	pad_center[1][1]=5.445;     pad_center[1][0]=-26.532;
	pad_center[2][1]=-10.171;   pad_center[2][0]=-25.000;
	pad_center[3][1]=-8.108;    pad_center[3][0]=-25.000;
	pad_center[4][1]=-5.405;    pad_center[4][0]=-25.000;
	pad_center[5][1]=-2.703;    pad_center[5][0]=-25.000;
	pad_center[6][1]=0;         pad_center[6][0]=-25.000;
	pad_center[7][1]=2.703;     pad_center[7][0]=-25.000;
	pad_center[8][1]=5.405;     pad_center[8][0]=-25.000;
	pad_center[9][1]=8.108;     pad_center[9][0]=-25.000;
	pad_center[10][1]=10.171;   pad_center[10][0]=-25.000;
	pad_center[11][1]=-9.459;   pad_center[11][0]=-22.655;
	pad_center[12][1]=-6.756;   pad_center[12][0]=-22.655;
	pad_center[13][1]=-4.054;   pad_center[13][0]=-22.655;
	pad_center[14][1]=-1.351;   pad_center[14][0]=-22.655;
	pad_center[15][1]=1.351;    pad_center[15][0]=-22.655;
	pad_center[16][1]=4.054;    pad_center[16][0]=-22.655;
	pad_center[17][1]=6.756;    pad_center[17][0]=-22.655;
	pad_center[18][1]=9.459;    pad_center[18][0]=-22.655;
	pad_center[19][1]=-10.208;  pad_center[19][0]=-20.314;
	pad_center[20][1]=-8.108;   pad_center[20][0]=-20.314;
	pad_center[21][1]=-5.405;   pad_center[21][0]=-20.314;
	pad_center[22][1]=-2.703;   pad_center[22][0]=-20.314;
	pad_center[23][1]=0;        pad_center[23][0]=-20.314;
	pad_center[24][1]=2.703;    pad_center[24][0]=-20.314;
	pad_center[25][1]=5.405;    pad_center[25][0]=-20.314;
	pad_center[26][1]=8.108;    pad_center[26][0]=-20.314;
	pad_center[27][1]=10.208;   pad_center[27][0]=-20.314;
	pad_center[28][1]=-9.459;   pad_center[28][0]=-17.974;
	pad_center[29][1]=-6.756;   pad_center[29][0]=-17.974;
	pad_center[30][1]=-4.054;   pad_center[30][0]=-17.974;
	pad_center[31][1]=-1.351;   pad_center[31][0]=-17.974;
	pad_center[32][1]=1.351;    pad_center[32][0]=-17.974;
	pad_center[33][1]=4.054;    pad_center[33][0]=-17.974;
	pad_center[34][1]=6.756;    pad_center[34][0]=-17.974;
	pad_center[35][1]=9.459;    pad_center[35][0]=-17.974;
	pad_center[36][1]=-10.208;  pad_center[36][0]=-15.633;
	pad_center[37][1]=-8.108;   pad_center[37][0]=-15.633;
	pad_center[38][1]=-5.405;   pad_center[38][0]=-15.633;
	pad_center[39][1]=-2.703;   pad_center[39][0]=-15.633;
	pad_center[40][1]=0;        pad_center[40][0]=-15.633;
	pad_center[41][1]=2.703;    pad_center[41][0]=-15.633;
	pad_center[42][1]=5.405;    pad_center[42][0]=-15.633;
	pad_center[43][1]=8.108;    pad_center[43][0]=-15.633;
	pad_center[44][1]=10.208;   pad_center[44][0]=-15.633;
	pad_center[45][1]=-9.459;   pad_center[45][0]=-13.293;
	pad_center[46][1]=-6.756;   pad_center[46][0]=-13.293;
	pad_center[47][1]=-4.054;   pad_center[47][0]=-13.293;
	pad_center[48][1]=-1.351;   pad_center[48][0]=-13.293;
	pad_center[49][1]=1.351;    pad_center[49][0]=-13.293;
	pad_center[50][1]=4.054;    pad_center[50][0]=-13.293;
	pad_center[51][1]=6.756;    pad_center[51][0]=-13.293;
	pad_center[52][1]=9.459;    pad_center[52][0]=-13.293;
	pad_center[53][1]=-10.208;  pad_center[53][0]=-10.952;
	pad_center[54][1]=-8.108;   pad_center[54][0]=-10.952;
	pad_center[55][1]=-5.405;   pad_center[55][0]=-10.952;
	pad_center[56][1]=-2.703;   pad_center[56][0]=-10.952;
	pad_center[57][1]=0;        pad_center[57][0]=-10.952;
	pad_center[58][1]=2.703;    pad_center[58][0]=-10.952;
	pad_center[59][1]=5.405;    pad_center[59][0]=-10.952;
	pad_center[60][1]=8.108;    pad_center[60][0]=-10.952;
	pad_center[61][1]=10.208;   pad_center[61][0]=-10.952;
	pad_center[62][1]=-9.459;   pad_center[62][0]=-8.612;
	pad_center[63][1]=-6.756;   pad_center[63][0]=-8.612;
	pad_center[64][1]=-4.054;   pad_center[64][0]=-8.612;
	pad_center[65][1]=-1.351;   pad_center[65][0]=-8.612;
	pad_center[66][1]=1.351;    pad_center[66][0]=-8.612;
	pad_center[67][1]=4.054;    pad_center[67][0]=-8.612;
	pad_center[68][1]=6.756;    pad_center[68][0]=-8.612;
	pad_center[69][1]=9.459;    pad_center[69][0]=-8.612;
	pad_center[70][1]=-10.208;  pad_center[70][0]=-6.272;
	pad_center[71][1]=-8.108;   pad_center[71][0]=-6.272;
	pad_center[72][1]=-5.405;   pad_center[72][0]=-6.272;
	pad_center[73][1]=-2.703;   pad_center[73][0]=-6.272;
	pad_center[74][1]=0;        pad_center[74][0]=-6.272;
	pad_center[75][1]=2.703;    pad_center[75][0]=-6.272;
	pad_center[76][1]=5.405;    pad_center[76][0]=-6.272;
	pad_center[77][1]=8.108;    pad_center[77][0]=-6.272;
	pad_center[78][1]=10.208;   pad_center[78][0]=-6.272;
	pad_center[79][1]=-9.459;   pad_center[79][0]=-3.931;
	pad_center[80][1]=-6.756;   pad_center[80][0]=-3.931;
	pad_center[81][1]=-4.054;   pad_center[81][0]=-3.931;
	pad_center[82][1]=-1.351;   pad_center[82][0]=-3.931;
	pad_center[83][1]=1.351;    pad_center[83][0]=-3.931;
	pad_center[84][1]=4.054;    pad_center[84][0]=-3.931;
	pad_center[85][1]=6.756;    pad_center[85][0]=-3.931;
	pad_center[86][1]=9.459;    pad_center[86][0]=-3.931;
	pad_center[87][1]=-10.208;  pad_center[87][0]=-1.65;
	pad_center[88][1]=-8.108;   pad_center[88][0]=-1.65;
	pad_center[89][1]=-5.405;   pad_center[89][0]=-1.65;
	pad_center[90][1]=-2.703;   pad_center[90][0]=-1.65;
	pad_center[91][1]=0;        pad_center[91][0]=-1.65;
	pad_center[92][1]=2.703;    pad_center[92][0]=-1.65;
	pad_center[93][1]=5.405;    pad_center[93][0]=-1.65;
	pad_center[94][1]=8.108;    pad_center[94][0]=-1.65;
	pad_center[95][1]=10.208;   pad_center[95][0]=-1.65;
	pad_center[96][1]=-9.45;    pad_center[96][0]=1.65;
	pad_center[97][1]=-6.756;   pad_center[97][0]=1.65;
	pad_center[98][1]=-4.054;   pad_center[98][0]=1.65;
	pad_center[99][1]=-1.351;   pad_center[99][0]=1.65;
	pad_center[100][1]=1.351;   pad_center[100][0]=1.65;
	pad_center[101][1]=4.054;   pad_center[101][0]=1.65;
	pad_center[102][1]=6.756;   pad_center[102][0]=1.65;
	pad_center[103][1]=9.45;    pad_center[103][0]=1.65;
	pad_center[104][1]=-10.208; pad_center[104][0]=3.931;
	pad_center[105][1]=-8.108;  pad_center[105][0]=3.931;
	pad_center[106][1]=-5.405;  pad_center[106][0]=3.931;
	pad_center[107][1]=-2.703;  pad_center[107][0]=3.931;
	pad_center[108][1]=0;       pad_center[108][0]=3.931;
	pad_center[109][1]=2.703;   pad_center[109][0]=3.931;
	pad_center[110][1]=5.405;   pad_center[110][0]=3.931;
	pad_center[111][1]=8.108;   pad_center[111][0]=3.931;
	pad_center[112][1]=10.208;  pad_center[112][0]=3.931;
	pad_center[113][1]=-9.459;  pad_center[113][0]=6.271;
	pad_center[114][1]=-6.756;  pad_center[114][0]=6.271;
	pad_center[115][1]=-4.054;  pad_center[115][0]=6.271;
	pad_center[116][1]=-1.351;  pad_center[116][0]=6.271;
	pad_center[117][1]=1.351;   pad_center[117][0]=6.271;
	pad_center[118][1]=4.054;   pad_center[118][0]=6.271;
	pad_center[119][1]=6.756;   pad_center[119][0]=6.271;
	pad_center[120][1]=9.459;   pad_center[120][0]=6.271;
	pad_center[121][1]=-10.208; pad_center[121][0]=8.612;
	pad_center[122][1]=-8.108;  pad_center[122][0]=8.612;
	pad_center[123][1]=-5.405;  pad_center[123][0]=8.612;
	pad_center[124][1]=-2.703;  pad_center[124][0]=8.612;
	pad_center[125][1]=0;       pad_center[125][0]=8.612;
	pad_center[126][1]=2.703;   pad_center[126][0]=8.612;
	pad_center[127][1]=5.405;   pad_center[127][0]=8.612;
	pad_center[128][1]=8.108;   pad_center[128][0]=8.612;
	pad_center[129][1]=10.208;  pad_center[129][0]=8.612;
	pad_center[130][1]=-9.459;  pad_center[130][0]=10.952;
	pad_center[131][1]=-6.756;  pad_center[131][0]=10.952;
	pad_center[132][1]=-4.054;  pad_center[132][0]=10.952;
	pad_center[133][1]=-1.351;  pad_center[133][0]=10.952;
	pad_center[134][1]=1.351;   pad_center[134][0]=10.952;
	pad_center[135][1]=4.054;   pad_center[135][0]=10.952;
	pad_center[136][1]=6.756;   pad_center[136][0]=10.952;
	pad_center[137][1]=9.459;   pad_center[137][0]=10.952;
	pad_center[138][1]=-10.208; pad_center[138][0]=13.293;
	pad_center[139][1]=-8.108;  pad_center[139][0]=13.293;
	pad_center[140][1]=-5.405;  pad_center[140][0]=13.293;
	pad_center[141][1]=-2.703;  pad_center[141][0]=13.293;
	pad_center[142][1]=0;       pad_center[142][0]=13.293;
	pad_center[143][1]=2.703;   pad_center[143][0]=13.293;
	pad_center[144][1]=5.405;   pad_center[144][0]=13.293;
	pad_center[145][1]=8.108;   pad_center[145][0]=13.293;
	pad_center[146][1]=10.208;  pad_center[146][0]=13.293;
	pad_center[147][1]=-9.459;  pad_center[147][0]=15.633;
	pad_center[148][1]=-6.756;  pad_center[148][0]=15.633;
	pad_center[149][1]=-4.054;  pad_center[149][0]=15.633;
	pad_center[150][1]=-1.351;  pad_center[150][0]=15.633;
	pad_center[151][1]=1.351;   pad_center[151][0]=15.633;
	pad_center[152][1]=4.054;   pad_center[152][0]=15.633;
	pad_center[153][1]=6.756;   pad_center[153][0]=15.633;
	pad_center[154][1]=9.459;   pad_center[154][0]=15.633;
	pad_center[155][1]=-10.208; pad_center[155][0]=17.974;
	pad_center[156][1]=-8.108;  pad_center[156][0]=17.974;
	pad_center[157][1]=-5.405;  pad_center[157][0]=17.974;
	pad_center[158][1]=-2.703;  pad_center[158][0]=17.974;
	pad_center[159][1]=0;       pad_center[159][0]=17.974;
	pad_center[160][1]=2.703;   pad_center[160][0]=17.974;
	pad_center[161][1]=5.405;   pad_center[161][0]=17.974;
	pad_center[162][1]=8.108;   pad_center[162][0]=17.974;
	pad_center[163][1]=10.208;  pad_center[163][0]=17.974;
	pad_center[164][1]=-9.459;  pad_center[164][0]=20.314;
	pad_center[165][1]=-6.756;  pad_center[165][0]=20.314;
	pad_center[166][1]=-4.054;  pad_center[166][0]=20.314;
	pad_center[167][1]=-1.351;  pad_center[167][0]=20.314;
	pad_center[168][1]=1.351;   pad_center[168][0]=20.314;
	pad_center[169][1]=4.054;   pad_center[169][0]=20.314;
	pad_center[170][1]=6.756;   pad_center[170][0]=20.314;
	pad_center[171][1]=9.459;   pad_center[171][0]=20.314;
	pad_center[172][1]=-10.208; pad_center[172][0]=22.655;
	pad_center[173][1]=-8.108;  pad_center[173][0]=22.655;
	pad_center[174][1]=-5.405;  pad_center[174][0]=22.655;
	pad_center[175][1]=-2.703;  pad_center[175][0]=22.655;
	pad_center[176][1]=0;       pad_center[176][0]=22.655;
	pad_center[177][1]=2.703;   pad_center[177][0]=22.655;
	pad_center[178][1]=5.405;   pad_center[178][0]=22.655;
	pad_center[179][1]=8.108;   pad_center[179][0]=22.655;
	pad_center[180][1]=10.208;  pad_center[180][0]=22.655;
	pad_center[181][1]=-9.459;  pad_center[181][0]=24.999;
	pad_center[182][1]=-6.756;  pad_center[182][0]=24.999;
	pad_center[183][1]=-4.054;  pad_center[183][0]=24.999;
	pad_center[184][1]=-1.351;  pad_center[184][0]=24.999;
	pad_center[185][1]=1.351;   pad_center[185][0]=24.999;
	pad_center[186][1]=4.054;   pad_center[186][0]=24.999;
	pad_center[187][1]=6.756;   pad_center[187][0]=24.999;
	pad_center[188][1]=9.459;   pad_center[188][0]=24.999;
	pad_center[189][1]=-7.176;  pad_center[189][0]=26.534;
	pad_center[190][1]=0;       pad_center[190][0]=26.534;
	pad_center[191][1]=7.176;   pad_center[191][0]=26.534;
	
}




