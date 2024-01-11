//--------------------------------------------------- 
// Class: padDetectorGeo
// 
// Created by: Jeffery T. Mitchell
// 
// Description: Central arm global geometry generator
// 
// update: 3/28/00
// update: 4/15/03  simulation geometry in OBJY database, Indrani Ojha
// update: 12/31/03 add method to access fromSimDatabase, not yet in use (CFM)
// update: 07/14/04 simulation geometry in Postgres database (Debsankar M.)
//--------------------------------------------------- 


#ifndef __PADDETECTORGEO_HH__
#define __PADDETECTORGEO_HH__

#include "PadBasicObject.hh"
#include <phool.h>
#include <PHFrame.h>
#include <PHPanel.h>
#include <PHAngle.h>
#include <PHPoint.h>

class PHCompositeNode;


class padDetectorGeo : public PadBasicObject { 

public:
  padDetectorGeo();                             // constructor
  virtual ~padDetectorGeo();                            // destructor
  
  // Data member access methods
  void set_Verbose(const int i) {Verbose = i;}             // Set Verbose
  short get_Verbose() const {return Verbose;}                 // Get Verbose
  void set_pc1Active(short, short *);  // Set the PC1 active list for an arm
  void get_pc1Active(short, short *);  // Get the PC1 active list for an arm
  void set_pc1Radius(const double r) {pc1Radius = r;}          // Set the PC1 radius
  double get_pc1Radius() const {return pc1Radius;}              // Get the PC1 radius
  void set_pc1ZWidth(const double r) {pc1ZWidth = r;}          // Set the PC1 z width
  double get_pc1ZWidth() const {return pc1ZWidth;}              // Get the PC1 z width
  void set_pc2Active(short, short *);  // Set the PC2 active list for an arm
  void get_pc2Active(short, short *);  // Get the PC2 active list for an arm
  void set_pc2Radius(const double r) {pc2Radius = r;}          // Set the PC2 radius
  double get_pc2Radius() const {return pc2Radius;}              // Get the PC2 radius
  void set_pc2ZWidth(const double r) {pc2ZWidth = r;}          // Set the PC2 z width
  double get_pc2ZWidth() const {return pc2ZWidth;}              // Get the PC2 z width
  void set_pc3Active(short, short *);  // Set the PC3 active list for an arm
  void get_pc3Active(short, short *);  // Get the PC3 active list for an arm
  void set_pc3Radius(const double r) {pc3Radius = r;}          // Set the PC3 radius
  double get_pc3Radius() const {return pc3Radius;}              // Get the PC3 radius
  void set_pc3ZWidth(const double r) {pc3ZWidth = r;}          // Set the PC3 z width
  double get_pc3ZWidth() const {return pc3ZWidth;}              // Get the PC3 z width
  void set_xOffset(double *);          // Set xOffset
  void get_xOffset(double *);          // Get xOffset
  void set_zOffset(double *);          // Set zOffset
  void get_zOffset(double *);          // Get zOffset
  void set_gasAtten(double *);         // Set gasAtten
  void get_gasAtten(double *);         // Get gasAtten
  void set_anodeSpacing(double *);     // Set anodeSpacing
  void get_anodeSpacing(double *);     // Get anodeSpacing
  void set_pixelLength(double *);      // Set pixelLength
  void get_pixelLength(double *);      // Get pixelLength
  void set_sideWidth(double *);        // Set sideWidth
  void get_sideWidth(double *);        // Get sideWidth
  void set_centerWidth(double *);      // Set centerWidth
  void get_centerWidth(double *);      // Get centerWidth
  void set_pixelSpacing(double *);     // Set pixelSpacing
  void get_pixelSpacing(double *);     // Get pixelSpacing
  void set_cellSpacing(double *);      // Set cellSpacing
  void get_cellSpacing(double *);      // Get cellSpacing
  void set_z0Gap(double *);            // Set z0Gap
  void get_z0Gap(double *);            // Get z0Gap
  void set_nWiresPerSect(short *);     // Set nWiresPerSect
  void get_nWiresPerSect(short *);     // Get nWiresPerSect
  void set_nPadsAcrossWire(short *);   // Set nPadsAcrossWire
  void get_nPadsAcrossWire(short *);   // Get nPadsAcrossWire
  void set_nPadsAlongWire(short *);    // Set nPadsAlongWire
  void get_nPadsAlongWire(short *);    // Get nPadsAlongWire
  void set_xyz0(const PHPoint &p);     // Set the global origin
  void set_xyz0(double *);             // Set the global origin (for ROOT)
  PHPoint get_xyz0();                  // Get the global origin
  void set_Theta0(const PHAngle *);          // Set the reference angles
  void get_Theta0(PHAngle *) const;          // Get the reference angles
  void set_Theta0(const double *);           // Set the reference angles
  void get_Theta0(double *) const;           // Get the reference angles
  void set_PhiTop(float *);            // Set the reference angles
  void get_PhiTop(float *);            // Get the reference angles
  void set_PhiBottom(float *);         // Set the reference angles
  void get_PhiBottom(float *);         // Get the reference angles

  // Build the active PC1 geometries.
  // The input argument is the arm number (0 or 1)
  void BuildPC1Geo(short);

  // Build the active PC2 geometries.
  // The input argument is the arm number (0 or 1)
  void BuildPC2Geo(short);

  // Build the active PC3 geometries.
  // The input argument is the arm number (0 or 1)
  void BuildPC3Geo(short);

  // Build PC1, PC2, and PC3 active geometries for both arms
  void BuildAllGeo();

  // Fetch the corresponding data members from a filled dPadGeom table
  void Fetch_dPadGeom(PHCompositeNode *);

  // Fetch all geometry information from the database
  PHBoolean FetchFromDatabase(PHTimeStamp);

  // Fetch simualtion  geometry information from the database
  PHBoolean FetchFromSimDatabase(PHTimeStamp);

  // To distinguish survey points from file (4 per PC2/3 chamber) and database (8).
  // The method is called from PadRec.cc (it is set for both simulation and real database)
  inline PHBoolean isFromDatabase() const { return (fromDatabase); }

  // The method is not yet (12/31/2003) in use, but it could distinguish real from simulation in future
  inline PHBoolean isFromSimDatabase() const { return (fromSimDatabase); }

  // Fetch all geometry information from the standard ASCII file
  PHBoolean FetchFromFile();

  // Fetch all geometry information from an ASCII file
  PHBoolean FetchFromFile(const char* filename);

  // Fetch all geometry information from the standard ASCII file, using only what should go 
  // into the database
  PHBoolean FetchFromFileDBFormat();

  // Fetch all geometry information from an ASCII file, using only what should go into the database
  PHBoolean FetchFromFileDBFormat(const char *filename);

  // Put the corresponding data members into the dPadGeom table
  void Put_dPadGeom(PHCompositeNode *);

  // Put the geometry information into the database
  PHBoolean PutIntoGeoChamDatabase();
  PHBoolean PutIntoGeoParDatabase();

// Put the geometry information into the simulation database
  PHBoolean PutIntoSimGeoChamDatabase();

// Put the parameter information into the simulation database
  PHBoolean PutIntoSimGeoParDatabase();

  // Put the geometry information into an ASCII file
  PHBoolean PutIntoFile();  // Use default file padGeometry.txt
  PHBoolean PutIntoFile(const char* filename);

  // Put the geometry information into an ASCII file, DB format
  PHBoolean PutIntoFileDBFormat(const char* filename);

  // Fetch the geometry for an arm
  // The arguments are the arm number and the PHPanel list
  // The return value are the number of PHPanel objects returned
  short get_pc1Geo(short, PHPanel *);
  short get_pc2Geo(short, PHPanel *);
  short get_pc3Geo(short, PHPanel *);

  // Print the geometry information
  // The first argument is the pad chamber number (0,1, or 2)
  // The second argument is the arm number (0 or 1)
  void PrintGeo(short,short);

  // Print the parameter information
  void PrintParams();

  // additions by Julia Velkovska
  // ---------------------------------------------------------
  void rotateAndTranslate(); //updates the panels with the frame transformation
  // set frames - for arms and for each panel
  void set_phenixFrame(PHFrame &f); // Set the global PHENIX frame
  void set_eastFrame(PHFrame &f);   // Set the east arm frame (arm 0)
  void set_westFrame(PHFrame &f);   // Set the west arm frame (arm 1)
  void set_pc1Frame(PHFrame &f,short &pc1arm,short &pc1sector);
  void set_pc2Frame(PHFrame &f,short &pc2arm,short &pc2sector);// for sectors
  void set_pc3Frame(PHFrame &f,short &pc3arm,short &pc3sector);// for sectors
  void set_pc2Frame(PHFrame &f,short &pc2arm,short &pc2sector,short &side);// for chambers
  void set_pc3Frame(PHFrame &f,short &pc3arm,short &pc3sector,short &side);// for chambers
  // 

  // get methods for frames
  // get frames - for arms and for each sector/chamber
  PHFrame get_phenixFrame();            // Get the global PHENIX frame
  PHFrame get_eastFrame();              // Get the east arm frame
  PHFrame get_westFrame();              // Get the west arm frame
  PHFrame get_pc1Frame(short &pc1arm,short &pc1sector);
  PHFrame get_pc2Frame(short &pc2arm,short &pc2sector);// for sectors
  PHFrame get_pc3Frame(short &pc3arm,short &pc3sector);// for sectors
  PHFrame get_pc2Frame(short &pc2arm,short &pc2sector,short &side);// for chambers
  PHFrame get_pc3Frame(short &pc3arm,short &pc3sector,short &side);// for chambers
  //------------------------------------------- jv
  static const int CGLMAXSECTPERARM = 4;
  static const int PC1MAXSECTPERARM = 8;

private:
  // The geometry objects representing each sector
  PHPanel pc1Sectors[2][PC1MAXSECTPERARM];
  PHPanel pc2Sectors[2][CGLMAXSECTPERARM];
  PHPanel pc3Sectors[2][CGLMAXSECTPERARM];
  PHPanel pc2Chambers[2][CGLMAXSECTPERARM][2];
  PHPanel pc3Chambers[2][CGLMAXSECTPERARM][2];
  //   JV  frames for geometry transformations of each chamber
  PHFrame phenixFrame;
  PHFrame eastFrame;
  PHFrame westFrame;
  PHFrame pc1SectorFrame[2][PC1MAXSECTPERARM];
  PHFrame pc2SectorFrame[2][CGLMAXSECTPERARM];
  PHFrame pc3SectorFrame[2][CGLMAXSECTPERARM];
  PHFrame pc2ChamberFrame[2][CGLMAXSECTPERARM][2];
  PHFrame pc3ChamberFrame[2][CGLMAXSECTPERARM][2];

  // Verbosity level for this class
  short Verbose;

  // Quantities used in reconstruction from the old dPadGeom table
  double xOffset[3];          // x offset. (was pdxoff)
  double zOffset[3];          // z offset. (was pdzoff)
  double gasAtten[3];         // gas attenuation factor (was pdgas)
  double anodeSpacing[3];     // Anode-to-anode spacing (was aasep)
  double pixelLength[3];      // Pixel length (was pxlen)
  double sideWidth[3];        // Side pixel width (was wside)
  double centerWidth[3];      // Center pixel width (was wcent)
  double pixelSpacing[3];     // Pixel space line width (was pxsep)
  double cellSpacing[3];      // Cell space line width (was clsep)
  double z0Gap[3];            // Gap between z=0 parts along wire (was zgap)
  short  nWiresPerSect[3];    // Number of wires per sector (was npdwr)
  short  nPadsAcrossWire[3];  // Number of pads across a wire (was npdx)
  short  nPadsAlongWire[3];   // Number of pads along a wire (was npdz)

  // The following pertain to default geometry generation
  // Flag of active sectors
  short pc1Active[2][PC1MAXSECTPERARM];
  short pc2Active[2][CGLMAXSECTPERARM];
  short pc3Active[2][CGLMAXSECTPERARM];
  short nChamPc1,nChamPc2,nChamPc3;
  short nActiveSectors[3], nSectPerArm[3];

  // Logical database variable. True if geometry is read from database
  // and False if geometry is read from the padGeometry.txt ascii file
  PHBoolean fromDatabase;
  PHBoolean fromSimDatabase;    // This flag is not yet in use (CFM, 12/31/2003)

  // The PHENIX origin reference coordinate
  PHPoint xyz0;

  // The inner inscribed radius of each PC
  double pc1Radius;
  double pc2Radius;
  double pc3Radius;

  // The +/- z extent of each PC (half-width)
  double pc1ZWidth;
  double pc2ZWidth;
  double pc3ZWidth;

  // The reference theta angle for sector 0 of each arm
  PHAngle Theta0[2];

  // The reference phi angles for top and bottom of both arms (in degrees)
  float PhiTop[2], PhiBottom[2];
  float phiBottomEast[3], phiBottomWest[3], phiTopEast[3], phiTopWest[3];

}; 

#endif /* __PADDETECTORGEO_HH__ */






