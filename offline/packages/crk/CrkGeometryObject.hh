#ifndef __CRKGEOMETRYOBJECT_HH__
#define __CRKGEOMETRYOBJECT_HH__

//  Definition of class CrkGeometryObject (RICH Detector Geometry Object)
//  File: CrkGeometryObject.hh
//  Author: Kenta Shigaki
//  Database access addded by Sasha Lebedev 5-23-2002

#include <PHGeometryObject.h>
#include <PHPoint.h>
#include <PHVector.h>
#include <PHLine.h>
#include <PHSphereSection.h>
#include <PHCylinderSection.h>

#define CRK_NARM   2  // West (0) and East (1)
#define CRK_NSIDE  2  // per arm; South (0) and North (1)
#define CRK_NSM   40  // per side; from bottom to top
#define CRK_NPMT  32  // per supermodule; from front PMT's, smaller |z|
#define CRK_NROW  80  // per side; from bottom to top
#define CRK_NZ    16  // per row; from smaller |z|
#define CRK_NMIR  24  // per side
#define CRK_NWIN   2  // per arm

class CrkGeometryObject : public PHGeometryObject { 

public: // public methods

  CrkGeometryObject (); 
  virtual ~CrkGeometryObject (){}

  PHBoolean Fetch (int RunNumber);
  PHBoolean FetchReal (int RunNumber);
  PHBoolean FetchSim ();
  PHBoolean FetchFromFile ();
  PHBoolean FetchFromFile (const char* filename);
  PHBoolean FetchFromPhnxPar (const char* filename);
  PHBoolean Update (PHTimeStamp* Tbeg, PHTimeStamp* Tend);
  void dump();

  void UseSurvey ();
  PHBoolean FetchAlignFromFile (const char* filename);

  const char* GetName () {
    return "CrkGeometryObject - RICH Detector Geometry Object";
  }

  PHPoint GetPmtPosition (int arm, int side, int sm, int pmt);
  PHVector GetPmtVector (int arm, int side, int sm, int pmt);

  PHLine Reflect (int arm, PHLine, int& side, int& panel, double& path);
  PHPoint HitArray (int arm, PHLine, int& side);

  double GetAlignDz (int arm, int side, int panel) {
    return d_align_dz [arm][side][panel];
  }
  double GetAlignDphi (int arm, int side, int panel) {
    return d_align_dphi [arm][side][panel];
  }

  int IdToArm (int global_id);
  int IdToSide (int global_id);
  int IdToSm (int global_id);
  int IdToPmt (int global_id);
  int GlobalId (int arm, int side, int sm, int pmt);

  void print ();

private: // internal methods

  void SetDefault ();
  void PlaceSmUniformly (int arm, int side,
			 double phi_min, double phi_max, double z_offset);
  void PlaceMirrorAsSphere (int arm, int side, PHPoint center, double radius);

  PHFrame GetVesselFrame (int arm);
  PHFrame GetSmFrame (int arm, int side, int sm);
  PHCylinderSection GetPmtPlaneInVessel (int arm, int side);
  PHSphereSection GetMirrorInVessel (int arm, int side, int panel);
  PHCylinderSection GetWindowInVessel (int arm, int i);

  void SetVesselFrame (int arm, PHFrame frame) {
    d_vessel_frame [arm] = frame;
  }
  void SetSmFrame (int arm, int side, int sm, PHFrame frame) {
    d_sm_frame [arm][side][sm] = frame;
  }
  void SetPmtPlane (int arm, int side,
		    PHPoint center, double radius,
		    PHVector axis, double phi_min, double phi_max);
  void SetMirror (int arm, int side, int panel, PHPoint center, double radius,
		  double phi_min, double phi_max,
		  double theta_min, double theta_max);
  void SetWindow (int arm, int i, PHPoint center, double radius,
		  double half_z, double phi_min, double phi_max);
  void SetAlignDz (int arm, int side, int panel, double dz) {
    d_align_dz [arm][side][panel] = dz;
  }
  void SetAlignDphi (int arm, int side, int panel, double dphi) {
    d_align_dphi [arm][side][panel] = dphi;
  }

  PHBoolean Shift (PHVector*);
  PHBoolean Shift (float, float, float);

  PHLine testline () {return PHLine (PHPoint(), PHVector(1.0,0.1,0.1));}

private: // data members

  // global geometry
  PHFrame d_vessel_frame [CRK_NARM];                   // in phenix_frame
  PHFrame d_sm_frame [CRK_NARM][CRK_NSIDE][CRK_NSM];   // in vessel_frame

  // pmt geometry
  PHPoint d_pmt_position [CRK_NPMT];                   // in sm_frame
  PHVector d_pmt_vector [CRK_NPMT];                    // in sm_frame

  // pmt array plane
  PHCylinderSection d_pmt_plane [CRK_NARM][CRK_NSIDE]; // in vessel_frame

  // mirror geometry
  PHSphereSection d_mirror [CRK_NARM][CRK_NSIDE][CRK_NMIR];
                                                       // in vessel_frame
  // window geometry
  PHCylinderSection d_window [CRK_NARM][CRK_NWIN];     // in vessel_frame

  // mirror alignment parameters - should be 0 if d_mirror is perfect
  float d_align_dz   [CRK_NARM][CRK_NSIDE][CRK_NMIR];
  float d_align_dphi [CRK_NARM][CRK_NSIDE][CRK_NMIR];

private: // from the template -- somehow needed ??

  virtual PHBoolean fetch (PHTimeStamp &, const char *, PdbBankID);
  virtual PHBoolean update
  (PHTimeStamp &, PHTimeStamp &, const char *, PdbBankID, const char *);
  virtual PHBoolean updateReferenceFrame
  (PHTimeStamp &, PHTimeStamp &, const char *, PdbBankID, const char *);
  virtual PHBoolean rotateAndTranslate
  (PHTimeStamp &, const char *, PdbBankID);
  virtual PHBoolean rotateAndTranslate (PHFrame, PHFrame, PHFrame, PHFrame);

}; 

#endif /* __CRKGEOMETRYOBJECT_HH__ */
