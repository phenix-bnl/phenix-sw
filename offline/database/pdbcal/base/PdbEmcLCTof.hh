//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999, 2000
//
//  Declaration of class PdbEmcLCTof
//
//  Purpose: Stores Least-Count Tof values for one EMCAL channel.
//
//  Description:
//
//  Author: Hugues Delagrange
//-----------------------------------------------------------------------------
#ifndef __PDBEMCLCTOF_HH__
#define __PDBEMCLCTOF_HH__

#include "PdbCalChan.hh"
/** Used to LC Tofs a given channel as a vector :\\
    + Value1 ;\\
    + Value2.\\
   */ 

class PdbEmcLCTof : public PdbCalChan {
public:
          /// Default constructor
	  PdbEmcLCTof();
          /// virtual destructor
	  virtual ~PdbEmcLCTof();
    
          /// Retrieve the values
          void GetLCTofs(float & thevalue1, float & thevalue2);
          /// Set the proper values
          void SetLCTofs(float   thevalue1, float   thevalue2);

          ///
	  virtual void print() const;

private:
          ///
          float fValue1;
          ///
          float fValue2;

  ClassDef(PdbEmcLCTof,1);
};

#endif /* __PDBEMCLCTOF_HH__ */
