// Class: cglHitAssociate
// Author: Jeffery T. Mitchell
// Purpose: Central arm global hit association module

// Details: This class will associate hits to drift chamber tracks
// based upon the given track model.

#ifndef __CGLHITASSOCIATE_HH__
#define __CGLHITASSOCIATE_HH__

#include "phool.h"

class PHTrack;
class PHCompositeNode;
class utiMatch;

static const int CGLMAXDET = 19; // org 13, added tofw --> 14, added svx --> 15-18 added hbd -> 19

class cglHitAssociate { 

public:
  cglHitAssociate();                             // constructor
  virtual ~cglHitAssociate();                            // destructor
  PHBoolean event(PHCompositeNode *);            // run it on an event
  
public:
  // Data member access methods
  void set_Verbose(short);         // Set Verbose
  short get_Verbose();             // Get Verbose
  void set_TrackModelFlag(short);  // Set TrackModelFlag
  short get_TrackModelFlag();      // Get TrackModelFlag
  void set_PredictMomentum(short); // Set PredictMomentum
  short get_PredictMomentum();     // Get PredictMomentum
  void set_TECZ0Buffer(double);    // set TECZ0Buffer
  double get_TECZ0Buffer();        // get TECZ0Buffer
  void set_TECSlopeCut(double);    // set TECSlopeCut
  double get_TECSlopeCut();        // get TECSlopeCut
  void set_RemoveHits(short);      // set RemoveHits
  short get_RemoveHits();          // get RemoveHits
  void set_UseFlag(short *);       // set UseFlag
  void get_UseFlag(short *);       // get UseFlag
  void set_UseFlag(short detid,short value);  // set UseFlag
  short get_UseFlag(short detid);  // get UseFlag
  void set_PhiRoad(double *);      // set PhiRoad
  void get_PhiRoad(double *);      // get PhiRoad
  void set_ZRoad(double *);        // set ZRoad
  void get_ZRoad(double *);        // get ZRoad
  void set_MinPhiWidth(double);    // set MinPhiRoad
  double get_MinPhiWidth();        // get MinPhiRoad
  void set_MinZWidth(double);      // set MinZRoad
  double get_MinZWidth();          // get MinZRoad
  void set_MinDchQuality(short);   // set MinDchQuality
  short get_MinDchQuality();       // get MinDchQuality
  void set_PhiRoadOnly(short);     // set PhiRoadOnly
  short get_PhiRoadOnly();         // get PhiRoadOnly
  void set_MaxDchQuality(short);   // set MaxDchQuality
  short get_MaxDchQuality();       // get MaxDchQuality

// set/get parameters for tec/cgl association
  void set_tecalpha0(double tmp) { tecalpha0 = tmp; }
  void set_tecalpha1(double tmp) { tecalpha1 = tmp; }
  void set_tecalpha2(double tmp) { tecalpha2 = tmp; }
  void set_tecphi0(double tmp) { tecphi0 = tmp; }
  void set_tecphi1(double tmp) { tecphi1 = tmp; }
  void set_tecphi2(double tmp) { tecphi2 = tmp; }
  double get_tecalpha0() { return tecalpha0; }
  double get_tecalpha1() { return tecalpha1; }
  double get_tecalpha2() { return tecalpha2; }
  double get_tecphi0() { return tecphi0; }
  double get_tecphi1() { return tecphi1; }
  double get_tecphi2() { return tecphi2; }
  void set_tecAlphaPhiClose(double tmp) { tecAlphaPhiClose = tmp; }
  double get_tecAlphaPhiClose() { return tecAlphaPhiClose; }

  // Print out the values of the data members for debugging
  void PrintParameters();

  void set_DOSLIDEANDFLIPP(short i) {DOSLIDEANDFLIPP=i;}

  void set_SLIDINGFLIPPING(float a) {SLIDINGFLIPPING=a;}

  void set_nSigmaTEC(float a) {nSigmaTEC=a;}
  void set_TecDchAlphaRatio(float a) {TecDchAlphaRatio=a;}

private:

  short DOSLIDEANDFLIPP;

  float SLIDINGFLIPPING;

  // Verbosity output level
  short Verbose;

  // Track Model flag
  // Set to 0 for LineTrack and 2 for DchTrack
  short TrackModelFlag;

  // Predict momentum selection
  short PredictMomentum;

  // The buffer around z=0 for projections when rejecting TEC tracks
  double TECZ0Buffer;

  // The maximum allowed TEC-DC alpha difference
  double TECSlopeCut;

  // Set this flag to remove hits from the hit list as you associate
  short RemoveHits;

  // Set this flag per detector ID to associate that detector detid:
  // 0 = vtx 1 =DCH, 2=PC1, 3=PC2, 4=PC3, 5=CRK, 6=TEC, 7=TOF, 8=PbSc, 9=PbGl
  // 10 = TZR ,11 = PCR, 12 = ACC, 13 = TOFW, 14~17 = SVX
  // 18 = HBD
  short UseFlag[CGLMAXDET];

  // The +/- phi road width for association in sigma of projection error
  double PhiRoad[CGLMAXDET];

  // The +/- z road width for association in sigma of projection error
  double ZRoad[CGLMAXDET];

  // The minimum allowed phi road width for all detectors in degrees
  double MinPhiWidth;

  // The minimum allowed z road width for all detectors
  double MinZWidth;

  // The minimum allowed drift chamber track quality flag, inclusive.
  short MinDchQuality;

  // Set to do only a 2-D projection if the drift chamber track is
  // lacking z information
  short PhiRoadOnly;

  // The maximum drift chamber quality factor to apply PhiRoadOnly,
  // inclusive.  If the dch quality is above this number, then a 2-D
  // projection will not be performed when PhiRoadOnly is set.
  short MaxDchQuality;

  // Parameters for tec/cgl association;
  double tecalpha0;
  double tecalpha1;
  double tecalpha2;
  double tecphi0;
  double tecphi1;
  double tecphi2;
  double tecAlphaPhiClose;
  float TecDchAlphaRatio;
  float nSigmaTEC;

  utiMatch* match;

}; 

#endif /* __CGLHITASSOCIATE_HH__ */
