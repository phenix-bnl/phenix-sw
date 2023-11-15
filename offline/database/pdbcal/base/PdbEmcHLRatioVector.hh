//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbEmcHLRatioVector
//
//  Purpose: Stores High to Low gain ratio for one EMCAL channel.
//
//  Description:
//
//  Author: Hugues Delagrange
//-----------------------------------------------------------------------------
#ifndef __PDBEMCHLRATIOVECTOR_HH__
#define __PDBEMCHLRATIOVECTOR_HH__

#include "PdbCalChan.hh"

/** Used to store H/L Gain ratios for a given channel as a vector :\\
    + Average ;\\
    + RMS     ;\\
    + Intecept;\\
    + Slope.   */ 

class PdbEmcHLRatioVector : public PdbCalChan {
public:
          /// Default contructor
	  PdbEmcHLRatioVector();
          /// virtual destructor
	  virtual ~PdbEmcHLRatioVector();

          /// Retrieve the values to build the vector
          void GetRatioVector(float & theAverage,   float & theRMS,
                              float & theIntercept, float & theSlope);
          /// Set the values of the vector
          void SetRatioVector(float   theAverage,   float   theRMS,
                              float   theIntercept, float   theSlope);

          ///
	  virtual void print() const;

private:
          ///
          float fAverage;
          ///
          float fRMS;
          ///
          float fIntercept;
          ///
          float fSlope;

  ClassDef(PdbEmcHLRatioVector,1);
};

#endif /* __PDBEMCHLRATIOVECTOR_HH__ */
