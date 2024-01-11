// Implementation of class file: PHDummyGeometryObject.h                       
// Created by: Federica Ceretto                                     
 
#include "phool.h"
#include "PHDummyGeometryObject.h"
#include "PdbCoordinate.hh"
#include "PdbParameter.hh"
#include "PdbBankManager.hh"
#include "PdbApplication.hh"
#include "PdbCalBank.hh"
#include <string>

PHDummyGeometryObject::PHDummyGeometryObject(PHDummyAddressObject *add) : PHGeometryObject(add)
{
  initialize(add);  
}

void PHDummyGeometryObject::initialize(PHAddressObject* add)
{
  verbose = 0;
  address = add;
  commit();
}

PHDummyGeometryObject::PHDummyGeometryObject(PHDummyAddressObject *add, short readFromFile) : PHGeometryObject(add)
{
  initialize(add);
  if (readFromFile == 0)
    {
      PHMessage("PHDummyGeometryObject", PHWarning, "Using info from old STAF table (hardwired)");
    }
  else
    {
      PHMessage("PHDummyGeometryObject", PHWarning, "Reading from files ");
    }

  if (readFromFile == 0)
    {
      // Informations previously contained into a table !!!
      xOffset = 0.3;
      attenuationFactor = 0.1;
      anodeToAnodeSpacing = 3.5;
      pixelLength = 4.3;
      sidePixelWidth = 3.2;
      numberOfSectors = 2;
      numberOfWiresPerSector = 45;

      bottomPhiOfEastArm = 213.75 / ToDegree;
      bottomPhiOfWestArm = -33.75 / ToDegree;
      topPhiOfWestArm = 56.25 / ToDegree;
      topPhiOfEastArm = 123.75 / ToDegree;
    }
  else
    {
      cout << "fetching geometry info from file" << endl;
      fetchGeometryInfoFromFile();
      cout << "fetching Position form file" << endl;
      fetchPositionsFromFile();
    }
}

PHPanel 
PHDummyGeometryObject::getPanel(int sector) const
{
  if (sector == 0) 
    {
      return sector0;
    }
  else if (sector == 1) 
    {
      return sector1;
    }
  else 
    {
      return sector2;
    }
}

void
PHDummyGeometryObject::printPanel(int sector) const
{
  if (sector == 0)
    {
      sector0.print();
    }
  else if (sector == 1)
    {
      sector1.print();
    }
  else
    {
      sector2.print();
    }
}

PHPoint  
PHDummyGeometryObject::getPixelPosition(PdbIndex* global) const
{
  address->setGlobalIndex(global);
  int arm    = address->getDetectorIndex(PHDummyAddressObject::ARM)->getValue();
  int module = address->getDetectorIndex(PHDummyAddressObject::MODULE)->getValue();
  int super  = address->getDetectorIndex(PHDummyAddressObject::SUPERMODULE)->getValue();
  return pixelPos[arm][module][super];
 
}

void  
PHDummyGeometryObject::printPixelPosition(PdbIndex* global) const
{
  
  address->setGlobalIndex(global);
  int arm    = address->getDetectorIndex(PHDummyAddressObject::ARM)->getValue();
  int module = address->getDetectorIndex(PHDummyAddressObject::MODULE)->getValue();
  int super  = address->getDetectorIndex(PHDummyAddressObject::SUPERMODULE)->getValue();
  pixelPos[arm][module][super].print();

}

PHBoolean 
PHDummyGeometryObject::update(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                  const char *calibname,PdbBankID bankID, char*descrip )
{
  //-----------------------------------------------------------------
  // This function updates in the database both the Geometry Info and the Wire positions
  // For detailed explanation see the corresponding functions: updateGeometryInfo() and updateWirePositions()
  //-----------------------------------------------------------------
  char genName[50];
  char infoName[50];
  char posName[50];
  strcpy(genName,calibname);
  strcpy(infoName,genName);
  strcpy(posName,genName);
  strcat(infoName,"info");
  strcat(posName,"pos");
    
  if (committed == 1) {                       
    if(!application->startUpdate()) {
      PHMessage("PHDummyGeometryObject",PHError, "Aborting ... Database not writable");
      application->abort();
    }else{
      committed = 0;
    }
  }
  geometryBank = bankManager->createBank("PdbCoordinateBank",bankID,descrip,Tstart,Tstop,posName);
  infoBank     = bankManager->createBank("PdbParameterBank",bankID,descrip,Tstart,Tstop,infoName);

  updateGeometryInfo();
  updatePositions();

  return False;
}

PHBoolean 
PHDummyGeometryObject::updatePositions()
{
  // This function will be used for writing for the first time
  // in the DB the MonteCarlo informations and/or
  // the survey measurements (if any). 
  // For the moment, the geometry in form of coordinated are read in from a File (using the initialize function) !
  // If needed every sub-system should define its own format 
      
  int length = (address->getGlobalIndex()->getNumberOf()); 
  geometryBank->setLength(length);     // the length of the bank is obtained from the address class
 
  int arm,module,super;
  PdbCoordinate *coordinate;
  
  for(int i = 0; i < length; i++) {
     address->getGlobalIndex()->setValue(i);                  // set the value of the global index
     address->setGlobalIndex(address->getGlobalIndex());          
     arm   = address->getDetectorIndex(PHDummyAddressObject::ARM)->getValue();
     module = address->getDetectorIndex(PHDummyAddressObject::MODULE)->getValue();
     super  = address->getDetectorIndex(PHDummyAddressObject::SUPERMODULE)->getValue();
     
     coordinate =  (PdbCoordinate*)&geometryBank->getEntry(i);
  }
  return True;
}

PHBoolean 
PHDummyGeometryObject::updateGeometryInfo()
{
 //-------------------------------------------------------------------------
  // At present this function introduces in the database the quantities
  // previously written in the DchGeometry  STAF table 
  //------------------------------------------------------------------------
  
 int length = 97;               // total number of  values from old Staf table + 2 
 infoBank->setLength(length);

 char parName[20];
 PdbParameter *parameter;
 
 for (int i=0; i < length; i++) {
   parameter =  (PdbParameter*)&infoBank->getEntry(i);
   parameter->setParameter(tmpValue[i]);
   strcpy(parName,parameterName[i]);
   parameter->setName(parName);
 }

 return True;   
}

PHBoolean 
PHDummyGeometryObject::updateReferenceFrame(PHTimeStamp &Tstart,PHTimeStamp &Tstop,
                                  const char *calibname,PdbBankID bankID, char*descrip )
{
  if (committed == 1) {                       
    if(!application->startUpdate()) {
      PHMessage("PHDummyGeometryObject",PHError, "Aborting ... Database not writable");
      application->abort();
    }else{
      committed = 0;
    }
  }
  
  referenceFrameBank = bankManager->createBank("PdbCoordinateBank",bankID,descrip,Tstart,Tstop,calibname);

  int length = 3;
  int translationId   = 0;
  int firstVectorId   = 1;
  int secondVectorId  = 2;
  
  referenceFrameBank->setLength(length);
  cout << "Length of the Bank "<< referenceFrameBank->getLength() << endl;
  
  PdbCoordinate *coordinate;
  for (int i = 0; i < referenceFrameBank->getLength(); i++) {
      coordinate =  (PdbCoordinate*)&referenceFrameBank->getEntry(i);
      cout << i << endl;
      if(i == translationId) {
	coordinate->setParameter(0, 0);
	coordinate->setParameter(1, 0);
	coordinate->setParameter(2, 0);
      }else if(i == firstVectorId) {   
	coordinate->setParameter(0, 1);
	coordinate->setParameter(1, 0);
	coordinate->setParameter(2, 0);
      }else if(i == secondVectorId) {  
	coordinate->setParameter(0, 0);
	coordinate->setParameter(1, 1);
	coordinate->setParameter(2, 0);
      }
      coordinate->print();
  }

  return True;
}

PHBoolean 
PHDummyGeometryObject::rotateAndTranslate(PHFrame initialE,PHFrame finalE,
						    PHFrame initialW,PHFrame finalW)
{
  cout << "TO bee implemented "<< endl;
  return True;
}

PHBoolean 
PHDummyGeometryObject::rotateAndTranslate(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID )
{
  if(committed == 1) {
    if(!application->startRead()) {
      PHMessage("PHDummyGeometryObject",PHError, "Aborting ... Database not readable");
      application->abort();
    }else{
      committed =  0;
    }
  }
  referenceFrameBank = bankManager->fetchBank("PdbCoordinateBank",bankID,calibname,Tsearch);
  
  PHPoint  translation;  // frame translation
  PHVector firstVector;  // X axis 
  PHVector secondVector; // Y axis

  int  translationId  = 0;
  int  firstVectorId  = 1;
  int  secondVectorId = 2;
  
  PdbCoordinate *pointVector;
  for(int i=0; i < referenceFrameBank->getLength(); i++) {
    pointVector =  (PdbCoordinate*)& referenceFrameBank->getEntry(i);
    cout << i << endl;
    pointVector->print();
    if (i == translationId ) {
      translation.setX(pointVector->getParameter(0));
      translation.setY(pointVector->getParameter(1));
      translation.setZ(pointVector->getParameter(2));
    }
    if ( i == firstVectorId ) {
      firstVector.setX(pointVector->getParameter(0));
      firstVector.setY(pointVector->getParameter(1));
      firstVector.setZ(pointVector->getParameter(2));
    }
    if ( i == secondVectorId) {
      secondVector.setX(pointVector->getParameter(0));
      secondVector.setY(pointVector->getParameter(1));
      secondVector.setZ(pointVector->getParameter(2));
    }
  }

  PHPoint   idealOrigin(0,0,0);
  PHVector  xAxis(1,0,0);
  PHVector  yAxis(0,1,0);
  
  PHFrame  idealFrame(idealOrigin,xAxis,yAxis) ;
  PHFrame  realFrame(translation,firstVector,secondVector);
      
  short arm, mod, sm;
  short maxarm, maxmod, maxsm;
  short minarm, minmod, minsm;

  maxarm  = address->getDetectorIndex(PHDummyAddressObject::ARM)->getMax();
  maxmod  = address->getDetectorIndex(PHDummyAddressObject::MODULE)->getMax();
  maxsm   = address->getDetectorIndex(PHDummyAddressObject::SUPERMODULE)->getMax();
  minarm  = address->getDetectorIndex(PHDummyAddressObject::ARM)->getMin();
  minmod  = address->getDetectorIndex(PHDummyAddressObject::MODULE)->getMin();
  minsm   = address->getDetectorIndex(PHDummyAddressObject::SUPERMODULE)->getMin();
  
  double xyz[3];
  double vxyz[3];
  PHPoint  newPixelPos;
  PHPoint  oldPixelPos;
  
  for (arm = minarm; arm < maxarm; arm++) {
    for (mod = minmod; mod < maxmod; mod++) {
      for (sm = minsm; sm < maxsm; sm++) {
	oldPixelPos = pixelPos[arm][mod][sm];
	
	newPixelPos = PHGeometry::transformPoint(idealFrame,oldPixelPos,  realFrame);

	pixelPos[arm][mod][sm] = newPixelPos;  
      }
    }
  }
  
  return True;
}

PHBoolean 
PHDummyGeometryObject::fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID bankID )
{
    char genName[50];
    char infoName[50];
    char posName[50];
    strcpy(genName,calibname);
    strcpy(infoName,genName);
    strcpy(posName,genName);
    strcat(infoName,"info");
    strcat(posName,"pos");
    
    if (committed == 1) {                       
      if(!application->startRead()) {
	PHMessage("PHDummyGeometryObject",PHError, "Aborting ... Database not readable");
	application->abort();
      }else{
	committed = 0;
      }
    }
    
    parameterName.clear();
    infoBank = bankManager->fetchBank("PdbParameterBank",bankID,infoName,Tsearch);
    geometryBank = bankManager->fetchBank("PdbCoordinateBank",bankID,posName,Tsearch);

    fetchGeometryInfo();
    fetchPositions();
    return True;
}

PHBoolean 
PHDummyGeometryObject::fetchGeometryInfo()
{   
  
  char* parName;
  PdbParameter *parameter;

  for(int i=0; i < infoBank->getLength(); i++) {
    parameter =  (PdbParameter*)&infoBank->getEntry(i);
    tmpValue[i] = parameter->getParameter();
    parName = parameter->getName();
    parameterName.append(parName);
  }
   
  int m=0;
  xOffset             = tmpValue[m++];        
  attenuationFactor   = tmpValue[m++];
  anodeToAnodeSpacing = tmpValue[m++];
  pixelLength         = tmpValue[m++];
  sidePixelWidth      = tmpValue[m++];
  numberOfSectors     = (int)tmpValue[m++];
  numberOfWiresPerSector = (int)tmpValue[m++];

  bottomPhiOfEastArm = tmpValue[m++];
  topPhiOfEastArm = tmpValue[m++];
  bottomPhiOfWestArm = tmpValue[m++];
  topPhiOfWestArm = tmpValue[m++];

  return True;
}
  
PHBoolean 
PHDummyGeometryObject::fetchPositions()
{                               
  int arm, mod, sm;
  int length = geometryBank->getLength();
  
  PdbCoordinate* coordinate;
  
  for(int i=0; i < length; i++) {
    coordinate =  (PdbCoordinate*)&geometryBank->getEntry(i);
    // increase the global index 
    address->getGlobalIndex()->setValue(i);
    // update all the indices according to the global index
    address->setGlobalIndex(address->getGlobalIndex());
    
    arm  = address->getDetectorIndex(PHDummyAddressObject::ARM)->getValue();
    mod  = address->getDetectorIndex(PHDummyAddressObject::MODULE)->getValue();
    sm   = address->getDetectorIndex(PHDummyAddressObject::SUPERMODULE)->getValue();

    pixelPos[arm][mod][sm].setX(coordinate->getParameter(0));
    pixelPos[arm][mod][sm].setY(coordinate->getParameter(1));
    pixelPos[arm][mod][sm].setZ(coordinate->getParameter(2));
   
  }
  return True;
}

PHBoolean 
PHDummyGeometryObject::fetchGeometryInfoFromFile()
{
  const char *geometryInfo="/phenix/workarea/federica/new/offline/database/newgeo/geometryDummy.txt";
    
  if (!geometryInfo) {
    cerr << " no file name given !! Fatal !!" << endl;
    return False;
  }
  ifstream file;
  file.open(geometryInfo);
  if (!file) {
    cerr << " could not open input file " << geometryInfo << " !!" << endl;
    return False;
  }
  
  float value;
  char parName[20];
  char* tmpName;
  int numberOfValues=0;
  int length = 11; // total number of  values from old Staf table + 2

  parameterName.clear();
  while (!file.eof() && numberOfValues < length) {
    file >> value >> parName;
    tmpName = new char[20];
    strcpy(tmpName,parName);
    tmpValue[numberOfValues] = value;
    parameterName.append(tmpName);
    numberOfValues++; 
  }

  int m=0;
  xOffset             = tmpValue[m++];        
  attenuationFactor   = tmpValue[m++];
  anodeToAnodeSpacing = tmpValue[m++];
  pixelLength         = tmpValue[m++];
  sidePixelWidth      = tmpValue[m++];
  numberOfSectors     = (int)tmpValue[m++];
  numberOfWiresPerSector = (int)tmpValue[m++];

  bottomPhiOfEastArm = tmpValue[m++];
  topPhiOfEastArm = tmpValue[m++];
  bottomPhiOfWestArm = tmpValue[m++];
  topPhiOfWestArm = tmpValue[m++];
}

PHBoolean
PHDummyGeometryObject::fetchPositionsFromFile()
{
  const char *geometry = "/phenix/workarea/federica/new/offline/database/newgeo/geofileDummy.txt";

  if (!geometry)
    {
      cerr << " no file name given !! Fatal !!" << endl;
      return False;
    }
  ifstream file;
  file.open(geometry);
  if (!file)
    {
      cerr << " could not open input file " << geometry << " !!" << endl;
      return False;
    }

  // the entry are --> basepoint, wire direction, drift direction
  int global, arm, module, super;
  int board, channel;
  float px, py, pz;
  int numberOfPixels = 0;

  while (!file.eof())
    {

      file >> global >> arm >> board >> channel >> px >> py >> pz;
      if (numberOfPixels < (address->getGlobalIndex()->getNumberOf()))
        {
          pixelPos[arm][module][super].setX(px);
          pixelPos[arm][module][super].setY(py);
          pixelPos[arm][module][super].setZ(pz);
        }
      else
        {
          break;
        }
      if (file.eof())
        {
          cout << "End of file reached !! Read in " << global << "pixel positions" << endl;
          break;
        }
    }
  return True;
}

  
