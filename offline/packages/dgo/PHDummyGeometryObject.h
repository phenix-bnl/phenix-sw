#ifndef PHDUMMYGEOMETRYOBJECT_H
#define PHDUMMYGEOMETRYOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDummyGeometryObject.h                       
//                                                                
// Created by: Federica Messer                                       
//
// Purpose: Retrieve (and write) Dummy Geometry info from the DB                             
//
// Last update:                                              
//                                                                
//----------------------------------------------------------------

#include "PHPoint.h"
#include "PHPanel.h"
#include "PHAngle.h"

#include "PHGeometryObject.h"    
#include "PHDummyAddressObject.h"

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

class PHDummyGeometryObject : public PHGeometryObject { 
 public:
 
  
  PHDummyGeometryObject(PHDummyAddressObject *add); 
  PHDummyGeometryObject(PHDummyAddressObject *add, short flagForNormalMCimput); // reading from file
  virtual ~PHDummyGeometryObject() {}

  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID );
  virtual PHBoolean fetchGeometryInfo();
  virtual PHBoolean fetchPositions();
  virtual PHBoolean fetchGeometryInfoFromFile();
  virtual PHBoolean fetchPositionsFromFile();
  
  
  virtual PHBoolean update(PHTimeStamp&,PHTimeStamp&,const char *,PdbBankID, char * );
  virtual PHBoolean updatePositions();
  virtual PHBoolean updateGeometryInfo();
  virtual PHBoolean updateReferenceFrame(PHTimeStamp&,PHTimeStamp&,const char *,PdbBankID, char * );

  virtual PHBoolean rotateAndTranslate(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);
  virtual PHBoolean rotateAndTranslate(PHFrame initialE, PHFrame finalE,PHFrame initialW, PHFrame finalW);


  virtual PHPoint getPixelPosition(PdbIndex* global) const ;
  virtual void printPixelPosition(PdbIndex* global) const ;
  
  virtual PHPanel  getPanel(int sector) const; 
  void printPanel(int sector) const;

public:
 
  float getXOffset()                { return xOffset;}
  float getAttenuationFactor()      { return attenuationFactor ;}
  float getAnodeToAnodeSpacing()    { return anodeToAnodeSpacing;}
  float getPixelLength()            { return pixelLength;}
  float getSidePixelWidth()         { return sidePixelWidth; }
  int   getNumberOfSectors()        { return numberOfSectors;}
  int   getNumberOfWiresPerSector() { return numberOfWiresPerSector;}
  PHAngle getBottomPhiOfEastArm()   { return bottomPhiOfEastArm;}
  PHAngle getBottomPhiOfWestArm()   { return bottomPhiOfWestArm;}
  PHAngle getTopPhiOfEastArm()      { return topPhiOfEastArm;}
  PHAngle getTopPhiOfWestArm()      { return topPhiOfWestArm;}

  PHPoint getPixelPos(int arm, int module, int sm) { return pixelPos[arm][module][sm];}

  
  void setXOffset(float val)                { xOffset = val;}
  void setAttenuationFactor(float val)      { attenuationFactor = val;}
  void setAnodeToAnodeSpacing(float val)    { anodeToAnodeSpacing = val;}
  void setPixelLength(float val)            { pixelLength = val;}
  void setSidePixelWidth(float val)         { sidePixelWidth = val; }
  void setNumberOfSectors(int val)          { numberOfSectors = val;}
  void setNumberOfWiresPerSector(int val)   { numberOfWiresPerSector = val;}
  void setBottomPhiOfEastArm(PHAngle val)   { bottomPhiOfEastArm = val;}
  void setBottomPhiOfWestArm(PHAngle val)   { bottomPhiOfWestArm = val;}
  void setTopPhiOfEastArm(PHAngle val)      { topPhiOfEastArm = val;}
  void setTopPhiOfWestArm(PHAngle val)      { topPhiOfWestArm = val;}
  
 
  
  //......
    
 protected:
  
 void  initialize(PHAddressObject *);
    
private:

  short verbose;
  PdbCalBank *infoBank;
  PdbCalBank *referenceFrameBank;  
  float tmpValue[20];
  PHPointerList<char> parameterName;


  float xOffset;
  float attenuationFactor;
  float anodeToAnodeSpacing;
  float pixelLength;
  float sidePixelWidth;
  int numberOfSectors;
  int numberOfWiresPerSector;

  PHAngle  bottomPhiOfEastArm;
  PHAngle  topPhiOfEastArm;
  PHAngle  bottomPhiOfWestArm;
  PHAngle  topPhiOfWestArm;

  PHPoint  pixelPos[2][9][4];

  PHPanel sector0;
  PHPanel sector1;
  PHPanel sector2;

}; 

#endif /* PHDUMMMYGEOMETRYOBJECT_H */ 
