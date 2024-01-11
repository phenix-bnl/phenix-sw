//-----------------------------------------------------------------------------
//  Implementation of class CrkGeometryObject (RICH Detector Geometry Object)
//
//  File: CrkGeometryObject.cc
//  Author: Kenta Shigaki
//
//  History: 07/12/00  K.Shigaki  First Version
//           07/15/00  K.Shigaki  derived from PHGeometryObject
//           05/23/02  Sasha Lebedev added database access
//-----------------------------------------------------------------------------

#include <CrkGeometryObject.hh>
#include <PHGeometry.h>

#include <recoConsts.h>

#include <PdbBankManager.hh>
#include <PdbApplication.hh>
#include <PdbCalBank.hh>
#include <PdbADCChan.hh>

#include <cmath>
#include <cstdlib>
#include <fstream> 
#include <iostream>
#include <string>

using namespace std;
using namespace PHGeometry;

// constructor
CrkGeometryObject::CrkGeometryObject() : PHGeometryObject () {

  const double pmt_side_offset [CRK_NPMT] =
  {  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,
     +2.424,  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,  +2.424,
     -2.424,  -2.424,  -2.424,  -2.424,  -2.424,  -2.424,  -2.424,  -2.424,
     -2.424,  -2.424,  -2.424,  -2.424,  -2.424,  -2.424,  -2.424,  -2.424};

  const double pmt_radial_position [CRK_NPMT] =
  { 263.490, 263.490, 263.490, 263.490, 263.490, 263.490, 263.490, 263.490,
    263.490, 263.490, 263.490, 263.490, 263.490, 263.490, 265.428, 269.301,
    263.490, 263.490, 263.490, 263.490, 263.490, 263.490, 263.490, 263.490,
    263.490, 263.490, 263.490, 263.490, 263.490, 263.490, 267.363, 271.239};

  const double pmt_z_position [CRK_NPMT] =
  { 141.227, 150.056, 158.885, 167.714, 176.543, 185.372, 194.201, 203.030,
    211.859, 220.688, 229.532, 237.948, 246.055, 253.983, 260.134, 264.587,
    145.53,  154.359, 163.188, 172.017, 180.846, 189.675, 198.504, 207.333,
    216.162, 225.217, 233.848, 242.047, 250.061, 257.907, 262.362, 266.815};

  const double pmt_theta_angle [CRK_NPMT] =
  {  35.487,  35.487,  35.487,  35.487,  35.487,  35.487,  35.487,  35.487,
     35.487,  35.487,  37.847,  40.347,  43.012,  45.802,  48.992,  48.992,
     35.487,  35.487,  35.487,  35.487,  35.487,  35.487,  35.487,  35.487,
     35.487,  37.847,  40.347,  43.012,  45.802,  48.992,  48.992,  48.992};
 
  for (int i = 0; i < CRK_NPMT; i ++) {
    d_pmt_position [i] = PHPoint (pmt_radial_position[i],
				  pmt_side_offset[i],
				  pmt_z_position[i]);
    d_pmt_vector [i] = PHVector ( sin(pmt_theta_angle[i] * ToRadian),
				  0.0,
				 -cos(pmt_theta_angle[i] * ToRadian));
  }

  SetDefault ();

  cout << "CGO constructed with design geometry" << endl;
}

// set default geometry
void CrkGeometryObject::SetDefault () {

  // place vassels as designed
  double vessel_phi_angle = 11.25 * ToRadian;
  SetVesselFrame (0, PHFrame (PHPoint (),
			      PHVector ( cos (vessel_phi_angle),
					 sin (vessel_phi_angle),
					 0.0),
			      PHVector (-sin (vessel_phi_angle),
					 cos (vessel_phi_angle),
					 0.0),
			      PHVector ( 0.0, 0.0, 1.0)
			      )
		  );
  SetVesselFrame (1, PHFrame (PHPoint (),
			      PHVector (-cos (vessel_phi_angle),
					 sin (vessel_phi_angle),
					 0.0),
			      PHVector ( sin (vessel_phi_angle),
					 cos (vessel_phi_angle),
					 0.0),
			      PHVector ( 0.0, 0.0, -1.0)
			      )
		  );

  // place supermodules as designed
  for (int arm = 0; arm < CRK_NARM; arm ++) {
    for (int side = 0; side < CRK_NSIDE; side ++) {
      double phi_min = -43.374;
      double phi_max =  43.374;
      double z_offset = 0.0;
      PlaceSmUniformly (arm, side, phi_min, phi_max, z_offset);
    }
  }

  // place (imaginary) PMT planes as designed
  {
    double z_min = 141.227;
    double z_max = 266.815;
    PHPoint center_pos (0.0, 0.0,   (z_min + z_max) * 0.5);
    PHPoint center_neg (0.0, 0.0, - (z_min + z_max) * 0.5);
    double radius = 263.490;
    PHVector axis (0.0, 0.0, (z_max -  z_min) * 0.5);
    double phi_min = -45. * ToRadian;
    double phi_max =  45. * ToRadian;

    for (int arm = 0; arm < CRK_NARM; arm ++) {
      for (int side = 0; side < CRK_NSIDE; side ++) {
	if ((arm+side)%2 == 1)  {  // West North, East South
	  SetPmtPlane (arm, side,
		       center_pos, radius, axis, phi_min, phi_max);
	} else {                   // West South, East North
	  SetPmtPlane (arm, side,
		       center_neg, radius, axis, phi_min, phi_max);
	}
      }
    }
  }

  // place mirror panels as designed
  {
    double z_offset = 215.0;
    double radius = 403.0;
    PlaceMirrorAsSphere (0, 0, PHPoint (0.0, 0.0, -z_offset), radius);
    PlaceMirrorAsSphere (0, 1, PHPoint (0.0, 0.0,  z_offset), radius);
    PlaceMirrorAsSphere (1, 0, PHPoint (0.0, 0.0,  z_offset), radius);
    PlaceMirrorAsSphere (1, 1, PHPoint (0.0, 0.0, -z_offset), radius);
  }
  for (int arm = 0; arm < CRK_NARM; arm ++) {
    for (int side = 0; side < CRK_NSIDE; side ++) {
      for (int panel = 0; panel < CRK_NMIR; panel ++) {
	SetAlignDz   (arm, side, panel, 0.0);
	SetAlignDphi (arm, side, panel, 0.0);
      }
    }
  }

  // place windows as designed
  {
    PHPoint center = PHPoint ();
    double radius [CRK_NMIR] = {258.770, 406.952};
    double half_z [CRK_NMIR] = {115.70,  185.60};
    double phi_min = -92.722 / 2. * ToRadian;
    double phi_max =  92.722 / 2. * ToRadian;

    for (int arm = 0; arm < CRK_NARM; arm ++) {
      for (int i = 0; i < CRK_NWIN; i ++) {
	SetWindow (arm, i, center, radius [i], half_z [i], phi_min, phi_max);
      }
    }
  }

}

// place CRK_NSM (40) supermodules uniformly in vessel
void CrkGeometryObject::PlaceSmUniformly
(int arm, int side, double phi_min, double phi_max, double z_offset) {
  for (int sm = 0; sm < CRK_NSM; sm ++) {
    double sm_phi_angle =
      (phi_min + (phi_max - phi_min) / (double)(CRK_NSM - 1) * sm) * ToRadian;
    if ((arm+side)%2 == 1)  {  // West North, East South
      SetSmFrame (arm, side, sm,
		  PHFrame (PHPoint (0.0, 0.0, z_offset),
			   PHVector ( cos (sm_phi_angle),
				      sin (sm_phi_angle),
				      0.0),
			   PHVector (-sin (sm_phi_angle),
				      cos (sm_phi_angle),
				      0.0),
			   PHVector ( 0.0, 0.0, 1.0)
			   )
		  );
    } else {  // West South, East North
      SetSmFrame (arm, side, sm,
		  PHFrame (PHPoint (0.0, 0.0, -z_offset),
			   PHVector ( cos (sm_phi_angle),
				      sin (sm_phi_angle),
				      0.0),
			   PHVector ( sin (sm_phi_angle),
				     -cos (sm_phi_angle),
				      0.0),
			   PHVector ( 0.0, 0.0, -1.0)
			   )
		  );
    }
  }
}

// place mirror panels on one side (12 panels) uniformly on a sphere
void CrkGeometryObject::PlaceMirrorAsSphere (int arm, int side,
					     PHPoint center, double radius) {

  double theta_min =  98.99 * ToRadian;
  double theta_gap = 109.32 * ToRadian;
  double theta_max = 122.24 * ToRadian;
  double dtheta_gap = 0.0004;
  double phi_min = -44.1 * ToRadian;
  double phi_max =  44.1 * ToRadian;
  double dphi_gap = 0.0004;

  for (int i = 0; i < CRK_NMIR/2; i ++) {

    double phi1 =
      phi_min
      + (phi_max - phi_min) / (double)(CRK_NMIR/2) * (double)(i)
      + dphi_gap;
    double phi2 =
      phi_min
      + (phi_max - phi_min) / (double)(CRK_NMIR/2) * (double)(i+1)
      - dphi_gap;

    double theta1 = theta_min + dtheta_gap;
    double theta2 = theta_gap - dtheta_gap;
    double theta3 = theta_gap + dtheta_gap;
    double theta4 = theta_max - dtheta_gap;

    if ((arm+side)%2 == 1)  {  // West North, East South
      SetMirror (arm, side, i,
		 center, radius, phi1, phi2, theta3, theta4);
      SetMirror (arm, side, CRK_NMIR/2 + i,
		 center, radius, phi1, phi2, theta1, theta2);
    } else {                   // West South, East North
      SetMirror (arm, side, i,
		 center, radius, phi1, phi2, Pi-theta4, Pi-theta3);
      SetMirror (arm, side, CRK_NMIR/2 + i,
		 center, radius, phi1, phi2, Pi-theta2, Pi-theta1);
    }

  }
}

// set surveyed mirror geometry
void CrkGeometryObject::UseSurvey () {

  PHFrame aframe;
  PlaceMirrorAsSphere (0, 0, // West A+B (South)
		       transformPoint
		       (aframe,
			PHPoint ( 3.00494,   0.138069, -214.680),
			d_vessel_frame [0]),
		       398.132);
  PlaceMirrorAsSphere (0, 1, // West C+D (North)
		       transformPoint
		       (aframe,
			PHPoint ( 1.80336,  -0.321662,  215.604),
			d_vessel_frame [0]),
		       399.054);
  PlaceMirrorAsSphere (1, 1, // Ease A+B (North)
		       transformPoint
		       (aframe,
			PHPoint (-0.187110, -0.560897,  215.585),
			d_vessel_frame [1]),
		       400.188);
  PlaceMirrorAsSphere (1, 0, // East C+D (South)
		       transformPoint
		       (aframe,
			PHPoint ( 0.564042, -0.208972, -214.381),
			d_vessel_frame [1]),
		        400.005);

  cout << "CGO uses surveyed mirror geometry" << endl;
};

// fetch mirror alignemnt data from ASCII file
PHBoolean CrkGeometryObject::FetchAlignFromFile (const char* filename) {

  ifstream align_f (filename);

  int arm, side, panel;
  float dz, dphi;

  if (! align_f) { 
    cerr << "CrkGeometryObject::FetchAlignFromFile ERROR:" << endl;
    cerr << "  Can not open " << filename << " file." << endl;
    cerr << endl;
    return False;
  } else {
    int aligned = 0;
    while (align_f >> arm >> side >> panel >> dz >> dphi) {
      SetAlignDz   (arm, side, panel, dz);
      SetAlignDphi (arm, side, panel, dphi);
      aligned ++;
    }
    cout << "CGO alignment data fetched from \"" << filename
	 << "\" [ASCII file]" << endl;
    cout << aligned << " out of " << CRK_NARM * CRK_NSIDE * CRK_NMIR
	 << " mirror panels aligned " << endl;
  }
  return True;
}

// return vessel frame in phenix (global) coordinates
PHFrame CrkGeometryObject::GetVesselFrame (int arm) {
  if ((arm >= 0) && (arm < CRK_NARM)) {
    return d_vessel_frame [arm];
  } else {
    PHMessage ("CrkGeometryObject::GetVesselFrame", PHError,
	       "index out of bounds");
    return PHFrame();
  }
}

// return supermodule frame in phenix (global) coordinates
PHFrame CrkGeometryObject::GetSmFrame (int arm, int side, int sm) {

  if ((arm >= 0) && (arm < CRK_NARM) &&
      (side >= 0) && (side < CRK_NSIDE) &&
      (sm >= 0) && (sm < CRK_NSM)) {


    PHPoint  o = d_sm_frame [arm][side][sm] .getOrigin ();
    PHVector u = d_sm_frame [arm][side][sm] .getU ();
    PHVector v = d_sm_frame [arm][side][sm] .getV ();
    PHVector w = d_sm_frame [arm][side][sm] .getW ();

    return PHFrame (transformPoint
		    (d_vessel_frame [arm], o, PHFrame ()),
		    transformVector
		    (d_vessel_frame [arm], u, PHFrame ()),
		    transformVector
		    (d_vessel_frame [arm], v, PHFrame ()),
		    transformVector
		    (d_vessel_frame [arm], w, PHFrame ()));

  } else {
    PHMessage ("CrkGeometryObject::GetSmFrame", PHError,
	       "index out of bounds");
    return PHFrame();
  }
}

// return PMT position in phenix (global) coordinates
PHPoint CrkGeometryObject::GetPmtPosition
(int arm, int side, int sm, int pmt) {
  if ((arm >= 0) && (arm < CRK_NARM) &&
      (side >= 0) && (side < CRK_NSIDE) &&
      (sm >= 0) && (sm < CRK_NSM) &&
      (pmt >= 0) && (pmt < CRK_NPMT)) {
    return transformPoint
      (GetSmFrame (arm, side, sm), d_pmt_position [pmt], PHFrame ());
  } else {
    PHMessage ("CrkGeometryObject::GetPmtPosition", PHError,
	       "index out of bounds");
    return PHPoint();
  }
}

// return PMT axis vector in phenix (global) coordinates
PHVector CrkGeometryObject::GetPmtVector
(int arm, int side, int sm, int pmt) {
  if ((arm >= 0) && (arm < CRK_NARM) &&
      (side >= 0) && (side < CRK_NSIDE) &&
      (sm >= 0) && (sm < CRK_NSM) &&
      (pmt >= 0) && (pmt < CRK_NPMT)) {
    return transformVector
      (GetSmFrame (arm, side, sm), d_pmt_vector [pmt], PHFrame ());
  } else {
    PHMessage ("CrkGeometryObject::GetPmtVector", PHError,
	       "index out of bounds");
    return PHVector();
  }
}

// return PMT array plane geometry in vessel coordinates
PHCylinderSection CrkGeometryObject::GetPmtPlaneInVessel (int arm, int side) {
  if ((arm >= 0) && (arm < CRK_NARM) &&
      (side >= 0) && (side < CRK_NSIDE)) {
    return d_pmt_plane [arm][side];
  } else {
    PHMessage ("CrkGeometryObject::GetPmtPlaneInVessel", PHError,
	       "index out of bounds");
    return PHCylinderSection();
  }
}

// return mirror panel geometry in vessel coordinates
PHSphereSection CrkGeometryObject::GetMirrorInVessel
(int arm, int side, int panel) {
  if ((arm >= 0) && (arm < CRK_NARM) &&
      (side >= 0) && (side < CRK_NSIDE) &&
      (panel >= 0) && (panel < CRK_NMIR)) {
    return d_mirror [arm][side][panel];
  } else {
    PHMessage ("CrkGeometryObject::GetMirrorInVessel", PHError,
	       "index out of bounds");
    return PHSphereSection();
  }
}

// return window geometry in vessel coordinates
PHCylinderSection CrkGeometryObject::GetWindowInVessel (int arm, int i) {
  if ((arm >= 0) && (arm < CRK_NARM) &&
      (i >= 0) && (i < CRK_NWIN)) {
    return d_window [arm][i];
  } else {
    PHMessage ("CrkGeometryObject::GetWindowInVessel", PHError,
	       "index out of bounds");
    return PHCylinderSection();
  }
}

// set geometry of a PMT array plane
void CrkGeometryObject::SetPmtPlane
(int arm, int side,
 PHPoint center, double radius, PHVector axis,
 double phi_min, double phi_max) {

  d_pmt_plane [arm][side] = PHCylinderSection (center, radius, axis);
  d_pmt_plane [arm][side] .setPhiRange (phi_min, phi_max);
}

// set geometry of a mirror panel
void CrkGeometryObject::SetMirror
(int arm, int side, int panel,
 PHPoint center, double radius,
 double phi_min, double phi_max, double theta_min, double theta_max) {

  d_mirror [arm][side][panel] = PHSphereSection (center, radius);
  d_mirror [arm][side][panel] .setPhiRange (phi_min, phi_max);
  d_mirror [arm][side][panel] .setThetaRange (theta_min, theta_max);
}

// set geometry of a window
void CrkGeometryObject::SetWindow
(int arm, int i, PHPoint center, double radius,
 double half_z, double phi_min, double phi_max) {

  PHVector axis (0.0, 0.0, half_z);
  d_window [arm][i] = PHCylinderSection (center, radius, axis);
  d_window [arm][i] .setPhiRange (phi_min, phi_max);
}

// check if a line hits a mirror panel from inside and return reflected line
PHLine CrkGeometryObject::Reflect
(int arm, PHLine line, int& side, int& panel, double& path) {

  if ((arm >= 0) && (arm < CRK_NARM)) {


    PHLine in =
      transformLine (PHFrame (), line, d_vessel_frame [arm]);
    in.normalize();

    for (side = 0; side < CRK_NSIDE; side ++) {
      for (panel = 0; panel < CRK_NMIR; panel ++) {
	PHSphereSection mir = GetMirrorInVessel (arm, side, panel);
	PHPoint cross [2];
	short n_cross =
	  intersectionLineSphereSection
	  (in, mir, cross[0], cross[1]);
	for (int j = 0; j < n_cross; j++) {
	  PHVector r (cross[j] - mir.getCenter());
	  r.normalize();
	  double in_dot_r = in.getDirection() .dot (r);
	  if (in_dot_r > 0.0) {
	    PHLine out (cross[j], in.getDirection() - r * 2.0 * in_dot_r);
	    PHCylinderSection win = GetWindowInVessel (arm, 0);
	    PHPoint enter [2];
	    short n_enter =
	      intersectionLineCylinderSection
	      (in, win, enter[0], enter[1]);
	    if (n_enter == 1) {
	      path = distancePointToPoint (enter[0], cross[j]);
	    } else {
	      path = 0.0;
	    }
	    return transformLine
	      (d_vessel_frame [arm], out, PHFrame ());
	  }
	}
      }
    }
  } else {
    PHMessage ("CrkGeometryObject::Reflect", PHError,
	       "index out of bounds");
  }
  side = 0;
  panel = 0;
  path = 0.0;
  return PHLine (PHPoint(), PHVector());
}

// check if a line hits a PMT array and return hit point
PHPoint CrkGeometryObject::HitArray (int arm, PHLine line, int& side) {

  if ((arm >= 0) && (arm < CRK_NARM)) {


    PHLine in =
      transformLine (PHFrame (), line, d_vessel_frame [arm]);
    in.normalize();

    short n_cross;
    PHPoint cross [2];

    for (side = 0; side < CRK_NSIDE; side ++) {

      PHCylinderSection plane = GetPmtPlaneInVessel (arm, side);
      n_cross = 
	intersectionLineCylinderSection
	(in, plane, cross[0], cross[1]);
      for (int j = 0; j < n_cross; j++) {
	PHVector r (cross[j] - plane.getCenter());
	r.setZ (0.0);
	r.normalize();
	double in_dot_r = in.getDirection() .dot (r);
	if (in_dot_r < 0.0) {
	  return transformPoint(d_vessel_frame [arm], cross[j], PHFrame ());
	}
      }
    }
  } else {
    PHMessage ("CrkGeometryObject::HitArray", PHError,
	       "index out of bounds");
  }
  side = 0;
  return PHPoint();
}

int CrkGeometryObject::IdToArm (int global_id) {
  return (global_id / CRK_NPMT / CRK_NSM / CRK_NSIDE);
}

int CrkGeometryObject::IdToSide (int global_id) {
  return ((global_id / CRK_NPMT / CRK_NSM) % CRK_NSIDE);
}

int CrkGeometryObject::IdToSm (int global_id) {
  return ((global_id / CRK_NPMT) % CRK_NSM);
}

int CrkGeometryObject::IdToPmt (int global_id) {
  int tmp_id = global_id % CRK_NPMT;
  if ((IdToArm (global_id) + IdToSide (global_id)) % 2 == 1)  {
    // West North, East South
    if (tmp_id >= CRK_NZ) {
      return (tmp_id - CRK_NZ);
    } else {
      return (tmp_id + CRK_NZ);
    }
  } else {
    // West South, East North
    return (tmp_id);
  }
}

int CrkGeometryObject::GlobalId (int arm, int side, int sm, int pmt) {
  return (((arm * CRK_NSIDE + side) * CRK_NSM + sm) * CRK_NPMT + pmt);
}

void CrkGeometryObject::print () {
  cout << endl << "##### CrkGeometryObject #####" << endl << endl;
}

// from the template -- somehow needed ??
PHBoolean
CrkGeometryObject::update(PHTimeStamp &Tstart,
			  PHTimeStamp &Tstop,
			  const char *calibname,
			  PdbBankID bankID, 
			  const char*descrip ) 
{
  return True;
}

// from the template -- somehow needed ??
PHBoolean
CrkGeometryObject::updateReferenceFrame(PHTimeStamp &Tstart,
					PHTimeStamp &Tstop,
					const char *calibname,
					PdbBankID bankID, const char*descrip) 
{
  return True;
}

// from the template -- somehow needed ??
PHBoolean
CrkGeometryObject::rotateAndTranslate(PHFrame initialE,
				      PHFrame finalE, 
				      PHFrame initialW,
				      PHFrame finalW) 
{
  return True;
}

// from the template -- somehow needed ??
PHBoolean
CrkGeometryObject::rotateAndTranslate(PHTimeStamp &Tsearch, 
				      const char *calibname, 
				      PdbBankID bankID) 
{
  return True;
}

// from the template -- somehow needed ??
PHBoolean
CrkGeometryObject::fetch(PHTimeStamp &Tsearch, 
			 const char *calibname, 
			 PdbBankID bankID) 
{
  return True;
}

// Undefined. Placeholders for the future
PHBoolean 
CrkGeometryObject::FetchFromFile() 
{
  return True;
}

PHBoolean 
CrkGeometryObject::FetchFromFile(const char *) 
{
  return True;
}

PHBoolean 
CrkGeometryObject::FetchFromPhnxPar (const char *) 
{
  return True; 
}

// SL 6-14-2002
PHBoolean CrkGeometryObject::Fetch(int RunNumber) {

  PHBoolean status = False;

  if(RunNumber>0) { status = FetchReal(RunNumber); }

  if(RunNumber<0) { status = FetchSim(); }

  if(status) {
    return True;
  }
  else {
    cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
    cout << PHWHERE << "ERROR: Database not available for run # " << RunNumber << endl;
    cout << PHWHERE << "Execution terminated." << endl;
    exit(1);
  }

}

//________________________________________________________________
// SL 6-14-2002 Read simulation (null) alignment from the database
PHBoolean CrkGeometryObject::FetchSim()
{
  
  PdbADCChan* achan = 0;
  const char* calibname = "geom.crk.run2";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);

  // RICH simulation alignment has validity range Jan 1st - Jan 31st 1999
  PHTimeStamp tSearch(1999, 1, 15, 0, 0, 0);
  recoConsts *rc = recoConsts::instance();
  if( 
    ( rc->FlagExist( "RUN3DAU" ) && rc->get_IntFlag("RUN3DAU") == 1 ) || 
    ( rc->FlagExist( "RUN4AUAU63GEV" ) && rc->get_IntFlag("RUN4AUAU63GEV") == 1 ) )
    { tSearch.set(1999, 1, 15, 3, 30, 0); }

  cout << "CrkGeometryObject::FetchSim - tSearch is " << tSearch << endl;

  if (application->startRead())
    {

      PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, tSearch);

      if (Bank)
        {
          int banklength = Bank->getLength();
          for (int i = 0; i < banklength; i++)
            {
              achan = (PdbADCChan*) & (Bank->getEntry(i));
              int tmp1 = (int)achan->getParameter(0);
              int arm = tmp1 / 1000;
              int side = (tmp1 / 100) % 10;
              int mirror = tmp1 % 100;
              float tmp2 = achan->getParameter(1);
              float tmp3 = achan->getParameter(2);
              d_align_dz[arm][side][mirror] = tmp2;
              d_align_dphi[arm][side][mirror] = tmp3;
            }
	  delete Bank;
        }
      else
        {
          cout << PHWHERE << "ERROR: Can not get rich alignment from the database." << endl;
          return False;
        }

      application->commit();

    }
  else
    {
      cout << PHWHERE << "ERROR: Can not access the database." << endl;
      return False;
    }

  return True;

}

// SL 5-23-2002 Read alignment for real data from the database
PHBoolean
CrkGeometryObject::FetchReal(int RunNumber)
{

  PdbADCChan* achan = 0;
  const char* calibname = "geom.crk.run2";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();
  PdbBankID bankID(0);

  PHTimeStamp tLatest(2030, 1, 1, 0, 0, 0);

  if (application->startRead())
    {

      PdbCalBank *Bank = bankManager->fetchBank("PdbADCChanBank", bankID, calibname, RunNumber);

      if (!Bank)
        {
          Bank = bankManager->fetchClosestBank("PdbADCChanBank", bankID, calibname, tLatest);
          cout << PHWHERE << "Warning: Can not get the period of run "
	       << RunNumber
	       << " from the database. Using latest ("
	       << Bank->getStartValTime()
	       << ") database." << endl;
          return False;
        }
      else
        {
          int banklength = Bank->getLength();
          for (int i = 0; i < banklength; i++)
            {
              achan = (PdbADCChan*) & (Bank->getEntry(i));
              int tmp1 = (int)achan->getParameter(0);
              int arm = tmp1 / 1000;
              int side = (tmp1 / 100) % 10;
              int mirror = tmp1 % 100;
              float tmp2 = achan->getParameter(1);
              float tmp3 = achan->getParameter(2);
              d_align_dz[arm][side][mirror] = tmp2;
              d_align_dphi[arm][side][mirror] = tmp3;
            }
          delete Bank;
        }
      application->commit();

    }
  else
    {
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "ERROR: Can not access the database." << endl;
      return False;
    }

  return True;
}

void CrkGeometryObject::dump() {
  
 for(int i=0; i<CRK_NARM;  i++) {
   for(int j=0; j<CRK_NSIDE; j++) {
     for(int k=0; k<CRK_NMIR;  k++) {
       cout << i << " " << j << " " << k << " " << d_align_dz[i][j][k] << " " << d_align_dphi[i][j][k] << endl;	
     }
   }
 }
}

// SL 5-23-2002 Update alignment in the database from object in memory
PHBoolean CrkGeometryObject::Update(PHTimeStamp *Tbeg, PHTimeStamp *Tend) {

  PdbADCChan *achan = 0;
  const char* calibname = "geom.crk.run2";
  const char* description = "rich alignment";

  PdbBankManager *bankManager = PdbBankManager::instance();
  PdbApplication *application = bankManager->getApplication();

  PHTimeStamp tStart = *Tbeg;
  PHTimeStamp tStop  = *Tend;
  PdbBankID bankID(0);

    if(application->startUpdate()) {

      PdbCalBank *Bank = bankManager->createBank("PdbADCChanBank",
		                                 bankID,
						 description,
						 tStart, tStop,
						 calibname);

      int nent=0;
      Bank->setLength(CRK_NARM*CRK_NSIDE*CRK_NMIR);
      for(int i=0; i<CRK_NARM;  i++) {
      for(int j=0; j<CRK_NSIDE; j++) {
      for(int k=0; k<CRK_NMIR;  k++) {
        float tmp1 = (float)(k+j*100+i*1000);
	float tmp2 = d_align_dz[i][j][k];
	float tmp3 = d_align_dphi[i][j][k];
	achan = (PdbADCChan*)&(Bank->getEntry(nent));
	achan->setParameter(0, tmp1);
	achan->setParameter(1, tmp2);
	achan->setParameter(2, tmp3);
	nent++;
      }}}	

      application->commit();
    }
    else {
      cout << PHWHERE << __FILE__ << ":" << __LINE__ << ":" << endl;
      cout << PHWHERE << "ERROR: Failed to update rich alignment in the database." << endl;
      return False;
    }
	
  return True;
}


