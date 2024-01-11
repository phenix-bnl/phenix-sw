#ifndef __SVXDETECTORGEO_HH__
#define __SVXDETECTORGEO_HH__

#include <phool.h>
#include <PHFrame.h>
#include <PHPanel.h>
#include <PHAngle.h>
#include <PHPoint.h>
#include <PHTimeStamp.h>
class PHCompositeNode;
class SvxSensor;
class PHTimeStamp;

#include "SvxParameters.h"
#include <PdbSvxCoordinateOffset.hh>

/**
 * @brief Central layer global geometry generator
 *
 * @date  Created by: Jeffery T. Mitchell
 * @date  update: 3/28/00
 * @date  update: 4/15/03  simulation geometry in OBJY database, Indrani Ojha
 * @date  update: 12/31/03 add method to access fromSimDatabase, not yet in use (CFM)
 * @date  update: 12/21/2011 add coordinate Offset and I/F to the DB (T.Hachiya)
 */
class svxDetectorGeo {

public:
  svxDetectorGeo();                             // constructor
  virtual ~svxDetectorGeo();                            // destructor
  
  enum { SVXNSUBLAYER=8 };
  
  PHBoolean Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend);
  PHBoolean Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend, int ilayer);
  PHBoolean Fetch();
  PHBoolean Fetch(PHTimeStamp *T);
  PHBoolean Fetch(PHTimeStamp *T, int ilayer);

  PHBoolean Read_svxPISApar(std::string filename = "svxPISA.par");
  PHBoolean Write_svxPISApar();
  PHBoolean Write_svxPISApar(std::string filename);
  PHBoolean CreatePanels();
  PHBoolean Fetch_svxPISApar();
  PHBoolean Fetch_svxPISApar(PHTimeStamp *T);
  PHBoolean Update_svxPISApar(PHTimeStamp *Tbeg, PHTimeStamp *Tend,const char *desc);
  PHBoolean AdjustGeometry(int, double*, double*, double*, double*);
  PHBoolean Read_StripSurvey(std::string filename);
  void      setGeoBankIDForOldProduction(const int id) { m_geoBankId_For_Oldproduction = id; }
  void      setOffsetBankIDForOldProduction(const int id) { m_offsetBankId_For_Oldproduction = id; }

  PHBoolean Fetch_coordinateOffset(const int run);
  PHBoolean Fetch_coordinateOffset(const PHTimeStamp *T);
  PHBoolean Update_coordinateOffset(const PHTimeStamp *Tbeg, const PHTimeStamp *Tend, const char *desc);

  // Data member access methods
  void set_Verbose(short);             // Set Verbose
  short get_Verbose();                 // Get Verbose
  void set_svxActive(const short, const short, short *);  // Set the SVX active list for an layer
  void get_svxActive(const short, const short, short *);  // Get the SVX active list for an layer
  void set_svxRadius(const short, double);          // Set the SVX radius
  double get_svxRadius(const short);              // Get the SVX radius
  void set_svxZWidth(const short, double);          // Set the SVX z width
  double get_svxZWidth(const short);              // Get the SVX z width
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
  void set_nWiresPerLadd(short *);     // Set nWiresPerLadd
  void get_nWiresPerLadd(short *);     // Get nWiresPerLadd
  void set_nSvxsAcrossWire(short *);   // Set nSvxsAcrossWire
  void get_nSvxsAcrossWire(short *);   // Get nSvxsAcrossWire
  void set_nSvxsAlongWire(short *);    // Set nSvxsAlongWire
  void get_nSvxsAlongWire(short *);    // Get nSvxsAlongWire
  void set_xyz0(const PHPoint &p);     // Set the global origin
  void set_xyz0(double *);             // Set the global origin (for ROOT)
  PHPoint get_xyz0();                  // Get the global origin
  void set_Theta0(PHAngle *);          // Set the reference angles
  void get_Theta0(PHAngle *);          // Get the reference angles
  void set_Theta0(double *);           // Set the reference angles
  void get_Theta0(double *);           // Get the reference angles
  void set_PhiTop(float *);            // Set the reference angles
  void get_PhiTop(float *);            // Get the reference angles
  void set_PhiBottom(float *);         // Set the reference angles
  void get_PhiBottom(float *);         // Get the reference angles

  // Build the active SVX geometries.
  // The input argument is the layer number (0 or 1)
  void BuildSVXGeo(short){}

  // Build SVX active geometries for both layers
  void BuildAllGeo(){}

  // Fetch the corresponding data members from a filled dSvxGeom table
  void Fetch_dSvxGeom(PHCompositeNode *){}

  // Fetch all geometry information from the standard ASCII file
  PHBoolean FetchFromFile();

  // Fetch all geometry information from an ASCII file
  PHBoolean FetchFromFile(const char* filename);

  // Put the corresponding data members into the dSvxGeom table
  void Put_dSvxGeom(PHCompositeNode *){}

  // Put the geometry information into an ASCII file
  PHBoolean PutIntoFile();  // Use default file svxGeometry.txt
  PHBoolean PutIntoFile(const char* filename);

  // Fetch the geometry for an layer
  // The arguments are the layer number and the PHPanel list
  // The return value are the number of PHPanel objects returned
  short get_svxGeo(short, short, PHPanel *);

  // Print the geometry information
  // The first argument is the svx chamber number (0,1, or 2)
  // The ladond argument is the layer number (0 or 1)
  void PrintGeo(short);

  // Print the parameter information
  void PrintParams();

  // return pointers to VTX sensors objects
  SvxSensor* GetSensorPtr(int i, int j, int k) {return barSensor[i][j][k];}
  unsigned int get_nBarLadder(int i) {return nBarLadder[i];}
  unsigned int get_nBarSensor(int i) {return nBarSensor[i];}

  // additions by Julia Velkovska
  // ---------------------------------------------------------
  void rotateAndTranslate(){} //updates the panels with the frame transformation
  // set frames - for layers and for each panel
  void set_phenixFrame(PHFrame &f); // Set the global PHENIX frame
  void set_svxFrame(PHFrame &f,short &svxlayer,short &svxladder);// for ladders
  // 

  // get methods for frames
  // get frames - for layers and for each ladder/chamber
  PHFrame get_phenixFrame();            // Get the global PHENIX frame
  PHFrame get_svxFrame(short &svxlayer,short &svxladder);// for ladders
  //------------------------------------------- jv

  double get_SensorCenter(int ilayer, int iladder, int isensor, int coord);

  float get_SensorXSize(int ilayer) const { return svx_sensor_xsize[ilayer]; }
  float get_SensorYSize(int ilayer) const { return svx_sensor_ysize[ilayer]; }
  float get_SensorZSize(int ilayer) const { return svx_sensor_zsize[ilayer]; }
  float get_SensorZGap(int ilayer) const { return svx_sensor_snzgap[ilayer]; }

  // get nominal radius of sublayer
  double get_Rsublayer(int isublayer) const { return SvxRsublayer[isublayer]; }

  // Return index of layer nearest to r
  int get_nearestLayer(double r);

  // Get SvxSensor* closest to (x,y,z). Optionally search only in selected layer.
  SvxSensor* get_nearestSensor(double x, double y, double z, int layer = -1);

  //////////////////////////////////
  // coordinate offset
  /// set/get to Offset of VTX west w.r.t DCH
  void setOffsetVtxToCnt(double x, double y, double z){
       m_coordinateoffset.setOffsetVtxToCnt(x, y, z);
    }
  void getOffsetVtxToCnt(double& x, double& y, double& z){
       x = m_coordinateoffset.getOffsetVtxToCnt(0);
       y = m_coordinateoffset.getOffsetVtxToCnt(1);
       z = m_coordinateoffset.getOffsetVtxToCnt(2);
    }

  /// set/get to Offset of VTX east w.r.t VTX west
  void setOffsetEastToWest(double x, double y, double z){
       m_coordinateoffset.setOffsetVtxEastToWest(x, y, z);
    }
  void getOffsetEastToWest(double& x, double& y, double& z){
       x = m_coordinateoffset.getOffsetVtxEastToWest(0);
       y = m_coordinateoffset.getOffsetVtxEastToWest(1);
       z = m_coordinateoffset.getOffsetVtxEastToWest(2);
    }

  /// set/get to Offset of Cnt east w.r.t VTX west
  void setOffsetCntEastToWest(double x, double y, double z){
       m_coordinateoffset.setOffsetCntEastToWest(x,y,z);
    }
  void getOffsetCntEastToWest(double& x, double& y, double& z){
       x = m_coordinateoffset.getOffsetCntEastToWest(0);
       y = m_coordinateoffset.getOffsetCntEastToWest(1);
       z = m_coordinateoffset.getOffsetCntEastToWest(2);
    }

  void setOffsetUsedRunNumber(const int run){ m_coordinateoffset.setUsedRunNumber(run); }
  int  getOffsetUsedRunNumber()             { return m_coordinateoffset.getUsedRunNumber(); }

  void setOffsetGeometryVersion(const int ver){ m_coordinateoffset.setGeometryVersion(ver); }
  int  getOffsetGeometryVersion()             { return m_coordinateoffset.getGeometryVersion(); }

  void printCoordinateOffset() { m_coordinateoffset.print(); }

  //////////////////////////////////

private:

  short nLadders[SVXLAYERNUMBER];

  // The geometry objects representing each ladder
  PHPanel svxLadders[2][SVXLAYERNUMBER][SVXLADDERNUMBER];
  //   JV  frames for geometry transformations of each chamber
  PHFrame phenixFrame;
  PHFrame svxLadderFrame[SVXLAYERNUMBER][SVXLADDERNUMBER];

  // Geometry objects representing sensors (4,4,5,6 per ladder)
  PHPanel svxSensors[2][SVXLAYERNUMBER][SVXLADDERNUMBER][SVXSENSORNUMBER];
  unsigned int svxSecNlayers;                       ///< Number of layers:
  unsigned int nBarLadder[SVXLADDERNUMBER];         ///< max. number of ladders per layer
  unsigned int nBarSensor[SVXSENSORNUMBER];         ///< max. number of sensors per ladder
  unsigned int barSenType[SVXLAYERNUMBER];                        ///< sensor type
  /// Pointers to barrel sensor objects
  SvxSensor *(*barSensor)[SVXLADDERNUMBER][SVXSENSORNUMBER];
  float svx_layer_rpos[SVXLAYERNUMBER+1];
  float svx_layer_zpos[SVXLAYERNUMBER];
  float svx_sensor_xsize[SVXLAYERNUMBER];
  float svx_sensor_ysize[SVXLAYERNUMBER];
  float svx_sensor_zsize[SVXLAYERNUMBER];
  float svx_sensor_snzgap[SVXLAYERNUMBER];

  float svx_sensor_xsize_active[SVXLAYERNUMBER];
  float svx_sensor_zsize_active[SVXLAYERNUMBER];

  // Verbosity level for this class
  short Verbose;

  // Quantities used in reconstruction from the old dSvxGeom table
  double xOffset[SVXLAYERNUMBER];          // x offset. (was pdxoff)
  double zOffset[SVXLAYERNUMBER];          // z offset. (was pdzoff)
  double gasAtten[SVXLAYERNUMBER];         // gas attenuation factor (was pdgas)
  double anodeSpacing[SVXLAYERNUMBER];     // Anode-to-anode spacing (was aasep)
  double pixelLength[SVXLAYERNUMBER];      // Pixel length (was pxlen)
  double sideWidth[SVXLAYERNUMBER];        // Side pixel width (was wside)
  double centerWidth[SVXLAYERNUMBER];      // Center pixel width (was wcent)
  double pixelSpacing[SVXLAYERNUMBER];     // Pixel space line width (was pxsep)
  double cellSpacing[SVXLAYERNUMBER];      // Cell space line width (was clsep)
  double z0Gap[SVXLAYERNUMBER];            // Gap between z=0 parts along wire (was zgap)
  short  nWiresPerLadd[SVXLAYERNUMBER];    // Number of wires per ladder (was npdwr)
  short  nSvxsAcrossWire[SVXLAYERNUMBER];  // Number of svxs across a wire (was npdx)
  short  nSvxsAlongWire[SVXLAYERNUMBER];   // Number of svxs along a wire (was npdz)

  // The following pertain to default geometry generation
  // Flag of active ladders
  short svxActive[2][SVXLAYERNUMBER][SVXLADDERNUMBER];

  // The PHENIX origin reference coordinate
  PHPoint xyz0;

  // The inner inscribed radius of each SVX
  double svxRadius[SVXLAYERNUMBER];

  // The +/- z extent of each SVX (half-width)
  double svxZWidth[SVXLAYERNUMBER];

  // The reference theta angle for ladder 0 of each layer
  PHAngle Theta0[2];

  // The reference phi angles for top and bottom of both layers (in degrees)
  float PhiTop[2], PhiBottom[2];

  bool is_SensorCreated;

  /// nominal radius of sublayer
  double SvxRsublayer[SVXNSUBLAYER];

  ///////////////////////////////////
  // coordinate offset parameters
  PdbSvxCoordinateOffset m_coordinateoffset;

  void setOffsetToSensor(); /// called in Read_svxPISApar and Fetch_svxPISApar
  
  static int m_count;
  PHTimeStamp Tsearch;
  PHTimeStamp TUpdateStart, TUpdateStop;

  int         m_geoBankId_For_Oldproduction; // the value is used when value is not 0, 
  int         m_offsetBankId_For_Oldproduction; // the value is used when value is not 0, 
};

#endif /* __SVXDETECTORGEO_HH__ */
