#ifndef __CGLRECODEFS_H__
#define __CGLRECODEFS_H__

//#define SVXLAYERNUMBER 4
//#define SVXLADDERNUMBER 20
#include "SvxParameters.h"

// these radii are only used in PHDchTrack to roughly define the track segment limits
  // when looking for projections.
  // The geometry comes from the individual DGOs
  static const double cglDetectorGeo_pc1Radius = 248.891;
  static const double cglDetectorGeo_pc2Radius = 419.173;
  static const double cglDetectorGeo_pc3Radius = 492.012;
  static const double cglDetectorGeo_dchRadius = 220.0;
  static const double cglDetectorGeo_crkRadius = 260.0;
  static const double cglDetectorGeo_tecRadius = 430.0;
  static const double cglDetectorGeo_tofRadius = 503.0;
  static const double cglDetectorGeo_pbscRadius = 507.2;
  static const double cglDetectorGeo_pbglRadius = 540.0;
  static const double cglDetectorGeo_accRadius = 456.0;
  static const double cglDetectorGeo_tofwRadius = 475.0;
  static const double cglDetectorGeo_hbdRadius = 58.0;
  static const double cglDetectorGeo_svxRadius[SVXLAYERNUMBER] = {2.6, 5.1, 11.8, 16.7};

  static const short cglDetectorGeo_dchActive_east = 1;
  static const short cglDetectorGeo_dchActive_west = 1;
  static const short cglDetectorGeo_crkActive_east = 1;
  static const short cglDetectorGeo_crkActive_west = 1;
  static const short cglDetectorGeo_pc1Active_east[8] = {1, 1, 1, 1, 1, 1, 1, 1};
  static const short cglDetectorGeo_pc1Active_west[8] = {1, 1, 1, 1, 1, 1, 1, 1};
  static const short cglDetectorGeo_svxActive_east[SVXLAYERNUMBER][SVXLADDERNUMBER]
  	   = {{1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  	      {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  	      {1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  	      {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
  static const short cglDetectorGeo_svxActive_west[SVXLAYERNUMBER][SVXLADDERNUMBER]
  	   = {{1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  	      {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  	      {1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0},
  	      {1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0}};
  
// PC2 EAST is enabled here because
// rich rings need the projections in case there is only
// PC1 with no PC2/PC3/EMC match. It then takes the PC2 projections
// to get an estimate of the ring position
// The order in which the RICH ring search is done is
// (analysis meeting Aug 4 2003 Kyoichiro Ozawa)
// PC1 - PC2
// PC1 - PC3
// PC1 - EMC
// PC1 - PC2 projection
// we loose the last ones if the projections are not done in cgl
// after disabling PC2 EAST
 
//changed the active sectors in simulations to the original values - 10/22/07
 
 static const short cglDetectorGeo_pc2Active_east[8] = {1, 1, 1, 1, 1, 1, 1, 1};
 static const short cglDetectorGeo_pc2Active_east_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
 static const short cglDetectorGeo_pc2Active_west[8] = {1, 1, 1, 1, 1, 1, 1, 1};
 static const short cglDetectorGeo_pc2Active_west_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
 static const short cglDetectorGeo_pc3Active_east[8] = {1, 1, 1, 1, 1, 1, 1, 1};
 static const short cglDetectorGeo_pc3Active_east_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};
 static const short cglDetectorGeo_pc3Active_west[8] = {1, 1, 1, 1, 1, 1, 1, 1};
 static const short cglDetectorGeo_pc3Active_west_sim[8] = {1, 1, 1, 1, 0, 0, 0, 0};


 static const short cglDetectorGeo_tecActive_east[4] = {0, 0, 0, 0}; // TKH--Run4 temporary
 static const short cglDetectorGeo_tecActive_west[4] = {0, 0, 0, 0};
 static const short cglDetectorGeo_tofActive_east[4] = {1, 1, 0, 0};
 static const short cglDetectorGeo_tofActive_west[4] = {0, 0, 0, 0};
 static const short cglDetectorGeo_tofwActive_east[4] = {0, 0, 0, 0};
 static const short cglDetectorGeo_tofwActive_west[4] = {0, 1, 1, 0}; // for Run7 (W2 w3)
 static const short cglDetectorGeo_pbscActive_east[4] = {0, 0, 1, 1};
 static const short cglDetectorGeo_pbscActive_west[4] = {1, 1, 1, 1};
 static const short cglDetectorGeo_pbglActive_east[4] = {1, 1, 0, 0};
 static const short cglDetectorGeo_pbglActive_west[4] = {0, 0, 0, 0};
 static const short cglDetectorGeo_accActive = 1;
 static const short cglDetectorGeo_hbdActive_east[6] = {0, 0, 0, 0, 0, 0}; // HBD out for Run-11 on
 static const short cglDetectorGeo_hbdActive_west[6] = {0, 0, 0, 0, 0, 0}; // HBD out for Run-11 on


#endif
