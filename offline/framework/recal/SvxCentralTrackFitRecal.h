#ifndef __SVXCENTRALTRACKFITRECAL_H__
#define __SVXCENTRALTRACKFITRECAL_H__

/**
 * @brief recalibrator to fit the SvxCentralTrack
 *
 * @date  Created by: Takashi Hachiya 5/11/2016
 */


#include "Recalibrator.h"

#include <svxDetectorGeo.hh>
#include <SvxTracker.h>

class PHSnglCentralTrack;
class SvxCentralTrack;
class SvxClusterList;

class SvxCentralTrackFitRecal: public Recalibrator
{
 public:
  SvxCentralTrackFitRecal(const std::string &name = "SvxCentralTrackFitRecal");
  virtual ~SvxCentralTrackFitRecal() {};

  int Init(PHCompositeNode *topNode);
  int InitRun(PHCompositeNode *topNode);
  int process_event(PHCompositeNode *topNode);

  int End(PHCompositeNode *topNode){return 0;}
  int isValidRun(const int runnumber) const;


  /**
   * Set flag for shifting PHCentralTrack into vtx coordinate system
   */
  void setRotatePHCentralTrack(bool flag) { m_shiftphcentral = flag; }

  /**
   * Set the rotations for the PHCentralTrack phi/theta for a given arm
   * arm must be 0 (East) or 1 (West)
   */
  void setPHCentralTrackDphiDtheta(int arm, float dphi, float dtheta);

  /**
   * Get the PHCentralTrack dphi values for a given arm 
   * 0: East
   * 1: West
   */
  float getPHCentralTrackDphi(int arm)
  {
    return (arm >= 0 && arm < 2) ? m_rotPHcntPhi[arm] : -9999.;
  }

  /**
   * Get the PHCentralTrack dtheta values for a given arm 
   * 0: East
   * 1: West
   */
  float getPHCentralTrackDtheta(int arm)
  {
    return (arm >= 0 && arm < 2) ? m_rotPHcntTheta[arm] : -9999.;
  }

  /**
   * Set flag for reading PHCentralTrack rotations from the DB
   * true : Read from the DB
   * fals : Use set values
   *
   * Since rotation values will attempt to be fetched from the DB during
   * InitRun(), this is needed to make sure values set in a macro
   * (ex. during calibration) are not overwritten.
   */
  void setFetchPHCentralTrackRotFromDB(bool flag) { m_readphcntrotdb = flag;}

  /**
   * Fetch the parameters for the PHCentralTrack rotations from the DB
   * for runnumber.
   */
  bool fetchDBPHCentralTrackDphiDtheta(const int runnumber);


  /**
   * set flag to fit without CNT angle : false:use CNT, true: not use CNT
   */
  void setFitNoCNT(bool flag) { m_isFitNoCNT = flag; }

  /**
   * set tracker
   */
  void setTracker(SvxTracker* tracker) { m_tracker_out = tracker; }

 protected:
  void fitSvxCNT(PHSnglCentralTrack* cnt, SvxCentralTrack *svxcnt, float vx, float vy, float vz, SvxClusterList* svxclslist);

  void shiftPHCentralTrack(float *phi0, float *the0);


  void calcDCAbyCircleProjection
    (double pt, double phi, double the,  // pt in xy-plane, phi, theta at inner most layer
     int charge,                         // charge of the track
     double hx, double hy, double hz,    // hit position at inner most layer
     double vx, double vy,               // primary vertex
     double* dx, double* dy, double* dz, // dca position
     double* d2dca);                     // return

  void CircleProjection
    (float pt, float phi0, float the0, // pt & phi, theta at the closest approach
     int charge,
     float cax, float cay, float caz,  // closest approach
     float r,                          // radius of projected cylinder
     float &xproj, float &yproj, float &zproj); // projected position
  
 protected:
  SvxTracker  m_tracker;      // fitting algorithm
  SvxTracker*  m_tracker_out;      // fitting algorithm

  svxDetectorGeo *m_geometry;  // detector geometry object

  bool  m_isFitNoCNT; // true (default): noCnt angle used for multi circle fit
                      // false: cnt angle used 

  float m_fieldScale; // B-field strength

  float m_rsublayer[svxDetectorGeo::SVXNSUBLAYER]; // Radius of 8 sublayers

  // rotation parameter
  bool m_shiftphcentral; //true: shift PHCentralTrack into vtx coor system
  bool m_readphcntrotdb; //true: read PHCentralTrack rotations from DB
  const char *m_phcntrotdbname; //table name for PHCentralTrack rotations

  float m_rotPHcntPhi[2]; // rotation of phi for CNT-VTX alignment
  float m_rotPHcntTheta[2]; // rotation of theta for CNT-VTX alignment
  
  // event index
  int m_ievt;

  // inner flag to enableModule
  bool m_enableModule;
};

#endif



