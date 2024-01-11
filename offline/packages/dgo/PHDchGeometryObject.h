#ifndef PHDCHGEOMETRYOBJECT_H
#define PHDCHGEOMETRYOBJECT_H
 
//--------------------------------------------------------------- 
//                                                                
// File and Version Information                                   
//                                                                
// PHENIX Software                                                
//                                                                
// Implementation of class file: PHDchGeometryObject.h                       
//                                                                
// Created by: Federica Messer at Wed Feb 17 15:19:41 1999                                      
//
// Purpose: Retrieve (and write) Dch Geometry info from the DB                             
//
// Last update: Tue Dec 21 15:19:41 1999                                              
//                                                                
// TKH--Updated to allow for a vector drift displacement 
// TKH--Updated to support internal alignments via database.
//
//----------------------------------------------------------------


#include <PHGeometryObject.h>    
#include <PHDchAddressObject.h>
#include <PHCylinderSection.h>

#include <PHMatrix.h>
#include <PHPoint.h>
#include <PHVector.h>

#include <PdbDchWire.hh>

#include <vector>
#include <string>

class PdbBankManager;  
class PdbApplication;  
class PdbCalBank;     

enum DchWireType {X1, U1, V1, X2, U2, V2};  

class PHDchGeometryObject : public PHGeometryObject { 
 
  public:
  // enum DchWireType {X1, U1, V1, X2, U2, V2};
  
  //! constructor
  PHDchGeometryObject(PHDchAddressObject *add, short verb=0); 
  
  //! destructor
  virtual ~PHDchGeometryObject(); 

  PHBoolean setFileNames(const char* info, const char* wire, const char* frame, 
			 const char* alpha="NONE", const char* tilt="NONE", const char* beam="NONE",
			 const char* uvtt="NONE");

  //  Database handling of Parameters...
  virtual PHBoolean fetch(PHTimeStamp &Tsearch, const char *calibname, PdbBankID );
  virtual PHBoolean fetchGeometryInfo();
  virtual PHBoolean fetchWirePositions();
  virtual PHBoolean fetchFrames();
  virtual PHBoolean fetchAlphas(); // Read Alphas from DB
  virtual PHBoolean fetchTilts (); // Read Card Tilts from DB
  virtual PHBoolean fetchBeams (); // Read Beam Offset from DB
  virtual PHBoolean fetchUvtts (); // Read UV layer Tilts from DB

  //  ASCII file handling of Parameters...
  PHBoolean fetchFromFile();
  virtual PHBoolean fetchGeometryInfoFromFile();
  virtual PHBoolean fetchWirePositionsFromFile();
  virtual PHBoolean fetchFramesFromFile();
  virtual PHBoolean fetchAlphasFromFile(); // Read Alphas from DB
  virtual PHBoolean fetchTiltsFromFile (); // Read Card Tilts from DB
  virtual PHBoolean fetchBeamsFromFile (); // Read Beam Offset from DB
  virtual PHBoolean fetchUvttsFromFile (); // Read UV layer Tilts from DB

  //  Storage of info into database...
  virtual PHBoolean update(PHTimeStamp&,PHTimeStamp&,const char *,PdbBankID, const char * );
  virtual PHBoolean updateWirePositions();
  virtual PHBoolean updateGeometryInfo();
  virtual PHBoolean updateFrames();
  virtual PHBoolean updateAlphas();
  virtual PHBoolean updateTilts ();
  virtual PHBoolean updateBeams ();
  virtual PHBoolean updateUvtts ();
  virtual PHBoolean updateReferenceFrame(PHTimeStamp&,PHTimeStamp&,const char *,PdbBankID, const char * );
  virtual PHBoolean updateValidityTimeForLastBank(PHTimeStamp&, PHTimeStamp&, PHTimeStamp&,
						  const char *, PdbBankID, int force = 0);
  //  Screen dumps...
  virtual PHBoolean screenDump();

  virtual PHPoint   rotateAndTranslate(short, PHPoint); // debug
  virtual PHVector  rotateAndTranslate(short, PHVector); // debug
  virtual PHPoint   rotateAndTranslateInverse(short, PHPoint); // debug
  virtual PHVector  rotateAndTranslateInverse(short, PHVector); // debug
  virtual PHBoolean rotateAndTranslate(PHTimeStamp &Tsearch, const char *calibname, PdbBankID);
  virtual PHBoolean rotateAndTranslate(PHFrame initialE, PHFrame finalE,PHFrame initialW, PHFrame finalW);
  virtual PHBoolean rotateAndTranslate();
  int GetBeamPosition(int run, double *xy);
  virtual PdbDchWire getWire(PdbIndex* index) const;
  virtual PdbDchWire getWire(int arm, int plane, int cell) const; 
  void    printWire(PdbIndex* globalIndex) const;
  float   getDriftDistance(int plane) const;
  
  PHVector getLocalDriftDirection(int arm, int plane, int cell, float z) const; 

  // TKH
  void nudgeWireLocations();
  void nudgeCardAngles();

public:
 
  void setCommittingFlag(int val) {committingFlag = val;}
  short getFlagXUV(short plane)       const {return flagXUV[plane];}
  float getNcells()                   const {return ncells;}
  float getDistanceToDriftRegion()    const {return distanceToDriftRegion;}
  float getDeltaPhiOfCell()           const {return deltaPhiOfCell;}
  float getDeltaRadiusOfCell()        const {return deltaRadiusOfCell;}
  float getDeltaPhiOfCage()           const {return deltaPhiOfCage;}
  float getLengthOfXWire(short plane) const {return lengthOfXWire[plane];}
  float getInnerRadius()              const {return innerRadius;}
  float getOuterRadius()              const {return outerRadius;}
  float getUvangle()                  const {return uvangle;}
  float getPhibote()                  const {return phibote;}
  float getPhibotw()                  const {return phibotw;}
  float getPhitope()                  const {return phitope;}
  float getPhitopw()                  const {return phitopw;}
  float getPlanethick()               const {return planethick;}
  float getX1baserad()                const {return x1baserad;}
  float getX2baserad()                const {return x2baserad;}
  float getX1basez()                  const {return x1basez;}
  float getX2basez()                  const {return x2basez;}
  float getX1slotz()                  const {return x1slotz;}
  float getX2slotz()                  const {return x2slotz;}
  float getX1suppz()                  const {return x1suppz;}
  float getX2suppz()                  const {return x2suppz;}
  float getX1suppthick()              const {return x1suppthick;}
  float getX2suppthick()              const {return x2suppthick;}
  float getX1slotthick()              const {return x1slotthick;}
  float getX2slotthick()              const {return x2slotthick;}
  float getX1rextent()                const {return x1rextent;}
  float getX2rextent()                const {return x2rextent;}
  float getU1rextent()                const {return u1rextent;}
  float getU2rextent()                const {return u2rextent;}
  float getV1rextent()                const {return v1rextent;}
  float getV2rextent()                const {return v2rextent;}
  float getU1basez()                  const {return u1basez;}
  float getU2basez()                  const {return u2basez;}
  float getV1basez()                  const {return v1basez;}
  float getV2basez()                  const {return v2basez;}
  float getU1slotz()                  const {return u1slotz;}
  float getU2slotz()                  const {return u2slotz;}
  float getV1slotz()                  const {return v1slotz;}
  float getV2slotz()                  const {return v2slotz;}
  float getU1suppz()                  const {return u1suppz;}
  float getU2suppz()                  const {return u2suppz;}
  float getV1suppz()                  const {return v1suppz;}
  float getV2suppz()                  const {return v2suppz;}
  float getWinthickin()               const {return winthickin;}
  float getWinthickout()              const {return winthickout;}
  float getSupptiside()               const {return supptiside;}
  float getSuppalside()               const {return suppalside;}
  float getSuppzthick()               const {return suppzthick;}
  float getSupptibase()               const {return supptibase;}
  float getSuppalbase()               const {return suppalbase;}
  float getCfibinrad()                const {return cfibinrad;}
  float getCfiboutrad()               const {return cfiboutrad;}
  float getZvsdfactor()               const {return zvsdfactor;}
  float getGuidewiresep()             const {return guidewiresep;}
  float getNgusset()                  const {return ngusset;}
  float getTiswitch()                 const {return ti_switch;}
  float getSuppzlength()              const {return suppzlength;}
  float getPropregwidth()             const {return propregwidth;}

  float getGapBetweenXUV()            const {return gapBetweenXUV;}
  float getAngleOfUV1Slots()          const {return angleOfUV1Slots;}
  float getAngleOfUV2Slots()          const {return angleOfUV2Slots;}
  float getBottomPhiOfArm(short arm)       const {return bottomPhiOfArm[arm];}
  float getTopPhiOfArm(short arm)          const {return topPhiOfArm[arm];}
  float getRadiusAtZ0(short plane)         const { return radiusAtZ0[plane];}
  float getAverageRadius(short plane)      const { return averageRadius[plane];}
  float getRadiusAtExtremZ(short plane)    const { return radiusAtExtremZ[plane];}
  float getStereoAngleOfPlane(short plane) const {return stereoAngleOfPlane[plane];}
  float getZPositionOfChamber(short arm, short side) const { return ZPositionOfChamber[arm][side];}

  PHPoint getWireInCellCoord(short side, short plane)  const {return wireInCellCoord[side][plane];}
  PHLine  getWireLine(short plane)                           const { return wireLine[plane];}  

  PHPoint getWireInLocalCoord(short arm, short side, short plane, short cell) const
                               { return wireInLocalCoord[arm][side][plane][cell];} 
 

  void setFlagXUV(short plane, short val)      { flagXUV[plane]        = val;}
  void setDistanceToDriftRegion(float val)     { distanceToDriftRegion = val;}
  void setDeltaPhiOfCell(float val)            { deltaPhiOfCell        = val;}
  void setDeltaRadiusOfCell(float val)         { deltaRadiusOfCell     = val;}
  void setLengthOfXWire(short plane,float val) { lengthOfXWire[plane]  = val;}
  void setInnerRadius(float val)               { innerRadius           = val;}
  void setOuterRadius(float val)               { outerRadius           = val;}
  void setGapBetweenXUV(float val)             { gapBetweenXUV         = val;}  
  void setDeltaPhiOfCage(float val)            { deltaPhiOfCage        = val;}
  void setAngleOfUV1Slots(float val)           { angleOfUV1Slots       = val;}
  void setAngleOfUV2Slots(float val)           { angleOfUV2Slots       = val;}
  void setZPositionOfChamber(short arm, short side, float val)      { ZPositionOfChamber[arm][side] = val;}
  void setBottomPhiOfArm(short arm, float val)                      { bottomPhiOfArm[arm]           = val;}
  void setTopPhiOfArm(short arm, float val)                         { topPhiOfArm[arm]              = val;}
  void setRadiusAtExtremZ(short plane, float val)                   { radiusAtExtremZ[plane]        = val;}
  void setRadiusAtZ0(short plane, float val)                        { radiusAtZ0[plane]             = val;}
  void setAverageRadius(short plane, float val)                     { averageRadius[plane]          = val;}
  void setStereoAngleOfPlane(short plane, float val)                { stereoAngleOfPlane[plane]     = val;}
  void setWireInCellCoord(short side, short plane, PHPoint val)  { wireInCellCoord[side][plane]  = val;}
  void setWireLine(short plane, PHLine val)                           { wireLine[plane]               = val;}  
  void setWireInLocalCoord(short arm, short side, short plane, short cell, PHPoint val)
                          { wireInLocalCoord[arm][side][plane][cell] = val;} 
  
  PHPoint transformFromCellToLocalCoord(const short& a,          
                                           const short& p,          
                                           const short& c,         
                                           const PHPoint&);    // point in cell

  PHLine   transformDistanceToLineOld(const short& aarm,const short& pplane,
                                    const short& ccell,const float& ddistance);  // old version
  PHLine   transformDistanceToLineOld2(const short& aarm,const short& pplane,
                                    const short& ccell,const float& ddistance);  // old version
  PHLine   transformDistanceToLine(const short& aarm,const short& pplane,
                                 const short& ccell,const float& ddistance);     // scalar version
  PHLine   transformDistanceToLine(const short& aarm,const short& sside, const short& pplane,
                                 const short& ccell,const PHVector& ddistance);  // vector version
			  

  DchWireType returnWireType(short);

  PHFrame getStartFrame(short arm) { return startingFrame[arm];}
  PHFrame getEndFrame(short arm) { return endingFrame[arm];}

  const PHPoint   getCenterWestArmOnNorthSide() {return (PHPoint)(centerWest + (PHPoint)axisWest);}
  const PHPoint   getCenterEastArmOnNorthSide() {return (PHPoint)(centerEast + (PHPoint)axisEast);}
  const PHPoint&   getCenterWestArm() {return centerWest;}
  const PHPoint&   getCenterEastArm() {return centerEast;}
  const PHVector&  getAxisEastArm() {return axisEast;}
  const PHVector&  getAxisWestArm() {return axisWest;}
  
  const PHPoint&   getWireBasepointNorth(short a ,short p ,short c) const {return wireBasepointNorth[a][p][c];}
  const PHPoint&   getWireBasepointSouth(short a ,short p ,short c) const {return wireBasepointSouth[a][p][c];}
  const PHVector&  getWireDriftDirectionNorth(short a ,short p ,short c) const {return wireDriftDirectionNorth[a][p][c];}
  const PHVector&  getWireDriftDirectionSouth(short a ,short p ,short c) const {return wireDriftDirectionSouth[a][p][c];}


  short getWireSide(PHPoint point,short a, short p, short c);
  
  int getCellOfPoint(float phiLocal, float alpha=0, float zed = 0.,int plane = 0);
  
  int findCorrespondingArm(const PHPoint&);
  int findCorrespondingSide(const PHPoint&);
  int findCorrespondingPlane(const PHPoint&);
  int findCorrespondingCell(const PHPoint&);
  PHCylinderSection* getEastCylinder() { return dcEast;}
  PHCylinderSection* getWestCylinder() { return dcWest;}
  PHCylinderSection  getCylinderSectionAtPlane(short arm, short plane);
  
  PHBoolean getIntersectionTrackWirePlane(const PHLine& trackLine, int planeId, int armId, PHPoint& inter);
  PHLine  getWireAsLine(short a, short p, short c);
  PHBoolean expectSignalFrom(short arm, short plane, short cell, PHPoint point); 
  
 protected:

 void  calculateAverageRadius();
 void  initializeFlagXUV();
 short initializeWireLine();
 void  initialize(PHDchAddressObject *, short verb);
 void  initializeShapes();
 
 
public:
  // Added extra geometrical calibration data arrays...
  float A0[numberOfWireTypes][numberOfArms][numberOfSides][numberOfCells]; // Alpha of cards...
  float x1Slope [numberOfArms][numberOfCells]; // 
  float x1Offset[numberOfArms][numberOfCells]; //  Parameters for
  float x2Slope [numberOfArms][numberOfCells]; //  Card Positions
  float x2Offset[numberOfArms][numberOfCells]; //  so-called "TILT"
  float xBeamOffsetEast, yBeamOffsetEast;    // Run dependent Beam offset...
  float xBeamOffsetWest, yBeamOffsetWest;    // Run dependent Beam offset...
  float uvSlope [numberOfArms][16];  
  float uvOffset[numberOfArms][16];  
  double tmpValueFrame[24];


private:
  int committingFlag;
  const char*  geoFrameFile;
  const char*  geoInfoFile;
  const char*  geoWireFile;
  const char*  geoAlphaFile;
  const char*  geoTiltFile;
  const char*  geoBeamFile;
  const char*  geoUvttFile;
  
  PHFrame startingFrame[2];
  PHFrame endingFrame[2];
  PHMatrix  rotation[2];
  PHVector  translation[2];
  PHMatrix  rotationInverse[2];
  PHVector  translationInverse[2];
  
  //! east drift chamber volume
  PHCylinderSection *dcEast;

  //! west drift chamber volume
  PHCylinderSection *dcWest;
  
  PHPoint centerWest, centerEast;
  PHVector axisWest,axisEast;
  
  short verbose;
  PdbCalBank *infoBank;
  PdbCalBank *frameBank;  
  PdbCalBank *alphaBank;  
  PdbCalBank *tiltBank;  
  PdbCalBank *beamBank;  
  PdbCalBank *uvttBank;  
  float tmpValue[120];
  
  std::vector<std::string> parameterName;
  std::vector<std::string> parameterFrame;
  
  PHLine wireLine[numberOfPlanes];
  
  PHPoint  wireBasepointNorth[numberOfArms][numberOfPlanes][numberOfCells];
  PHPoint  wireBasepointSouth[numberOfArms][numberOfPlanes][numberOfCells];
  PHVector wireDriftDirectionNorth[numberOfArms][numberOfPlanes][numberOfCells];
  PHVector wireDriftDirectionSouth[numberOfArms][numberOfPlanes][numberOfCells];
  
  PHPoint  wireInCellCoord[numberOfSides][numberOfPlanes];
  PHPoint  wireInLocalCoord[numberOfArms][numberOfSides][numberOfPlanes][numberOfCells];
  
  float  ncells;
  float  uvangle;
  float  phibote;
  float  phibotw;
  float  phitope;
  float  phitopw;
  float  planethick;
  float  x1baserad;
  float  x2baserad;
  float  x1basez;
  float  x2basez;
  float  x1slotz;
  float  x2slotz;
  float  x1suppz;
  float  x2suppz;
  float  x1suppthick;
  float  x2suppthick;
  float  x1slotthick;
  float  x2slotthick;
  float  x1rextent;
  float  x2rextent;
  float  u1rextent;
  float  u2rextent;
  float  v1rextent;
  float  v2rextent;
  float  u1basez;
  float  u2basez;
  float  v1basez;
  float  v2basez;
  float  u1slotz;
  float  u2slotz;
  float  v1slotz;
  float  v2slotz;
  float  u1suppz;
  float  u2suppz;
  float  v1suppz;
  float  v2suppz;
  float  winthickin;
  float  winthickout;
  float  supptiside;
  float  suppalside;
  float  suppzthick;
  float  supptibase;
  float  suppalbase;
  float  cfibinrad;
  float  cfiboutrad;
  float  zvsdfactor;
  float  guidewiresep;
  float  ngusset;
  float  ti_switch;
  float  suppzlength;
  float  propregwidth;
  double dcwestxo,dcwestyo,dcwestzo;
  double dcwestxa,dcwestya,dcwestza;
  double dceastxo,dceastyo,dceastzo;
  double dceastxa,dceastya,dceastza;
  
  
  
  float  distanceToDriftRegion; 
  float  deltaPhiOfCell;
  float  deltaRadiusOfCell;
  float  lengthOfXWire[numberOfPlanes];
  
  float  innerRadius;
  float  outerRadius;
  float  gapBetweenXUV;
  
  float  deltaPhiOfCage;
  float  angleOfUV1Slots;
  float  angleOfUV2Slots; 
  
  float  ZPositionOfChamber[numberOfArms][numberOfSides];
  float  bottomPhiOfArm[numberOfArms];
  float  topPhiOfArm[numberOfArms];
  
  short  flagXUV[numberOfPlanes];
  float  radiusAtExtremZ[numberOfPlanes];
  float  averageRadius[numberOfPlanes];
  float  radiusAtZ0[numberOfPlanes];
  float  stereoAngleOfPlane[numberOfPlanes];
  
  PHDchAddressObject *dchaddress;
}; 

#endif /* PHDCHGEOMETRYOBJECT_H */ 
