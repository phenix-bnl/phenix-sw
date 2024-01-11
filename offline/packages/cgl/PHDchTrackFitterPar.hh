#ifndef __PHDCHTRACKFITTERPAR_HH__
#define __PHDCHTRACKFITTERPAR_HH__

//  Purpose:  PHDchTrackFitter parameter header                           
//  What:  Constants used in the PHDchTrackFitter class.                  
//  Created by:  Jane M. Burward-Hoy                                    

#include "gsl/gsl_math.h"
#include "phool.h"

const double zDch = 180 ;        // Length of Dch along Z (cm)
const double rMaxDch = 246;     // Maximum radial distance of Dch from Z axis (cm)

const int  numZ = 17;

const int numR = 30;        // 8/15/00 for three bin sizes from (10.0,500) cm

const int refR = 15; 
const int numP = 29;                   // for 0.050 GeV bins
const int  numInitialTheta=59;

const double zBin = 5.0;                  // in cm (-40 cm, +40 cm)

const double rBin1 = 20.0;                // 10 bins (10 cm, 190.0 cm) before DC
const double rBin2 = 5.0;                  // 15 bins (190 cm, 260 cm) to DC
const double rBin3 = 40.0;                 // 7 bins (260 cm, 500 cm) outside DC

const double pBin = 0.050;                // in GeV (0.17 GeV, 1.57 GeV) 
const double initialThetaBin = 1.0 * M_PI / 180.0;  // in rad (61 deg, 119 deg)

const double initialZ = -40.0;            // cm

const double initialR = 10.0;             // cm 8/15/00
const double dcInsideR = 190.0;            // cm
const double dcOutsideR = 260.0;            // cm 8/15/00
const double lastR = 500.0;

const double initialP = 0.170;            // GeV
const double maximumP = (numP-1)*pBin + initialP;   // GeV
const double maximumZ = (numZ-1)*zBin + initialZ; // cm
const double firstInitialTheta = 61.0 * M_PI / 180.0;  // rad 
const double lastInitialTheta = (numInitialTheta-1)*initialThetaBin + firstInitialTheta;

// Error flags 
const double fieldIntegralErr = -999.0;
const int errFieldBounds = 1;
const int errThetaBounds = 2;    

#endif /* __PHDCHTRACKFITTERPAR_HH__ */
