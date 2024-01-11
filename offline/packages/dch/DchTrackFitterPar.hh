// Purpose: Variable definitions Here are a number of the variables we
// use in the drift chamber code.  Eventually we'll want these in an
// external file (or defined in a table for STAF) so that the code
// need not be recompiled every time we change one of these
// parameters.

#ifndef __DCHTRACKFITTERPAR_H__
#define __DCHTRACKFITTERPAR_H__

#include "gsl/gsl_math.h"
#include "phool.h"

const double zDch = 1.80 ;        // Length of Dch along Z
const double rMaxDch = 2.46;     // Maximum radial distance of Dch from Z axis

const int  numZ = 17;
const int  numR = 15;
const int refR = 6;
const int numP = 29;                   // for 0.050 GeV bins
const int  numInitialTheta=59;

const double zBin = 5.0;                  // in cm (-40 cm, +40 cm)
const double rBin = 5.0;                  // in cm (190 cm, 260 cm)
const double pBin = 0.050;                // in GeV (0.2 GeV, 1.6 GeV) 
const double initialThetaBin = 1.0* M_PI / 180.0;  // in rad (61 deg, 119 deg)

const double initialZ = -40.0;            // cm
const double initialR = 190.0;            // cm
const double initialP = 0.200;            // GeV
const double firstInitialTheta = 61.0 * M_PI / 180.0;  // rad 

// Error flags 
const double fieldIntegralErr = -999.0;
const double thetaErr = -999.0;
const double errLineFit = -555.0;       // utiLineFit error
const double errNegMom = -666.0;
const double errZeroMom = -111.0;
const double errNoHits = -777.0;       // no hits error
const double errThetaBounds = -888.0;       // theta bounds error   
const double errSetIndices = -333.0;

#endif


