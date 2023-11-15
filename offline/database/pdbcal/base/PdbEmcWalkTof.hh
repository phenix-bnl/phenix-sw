//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999, 2000
//
//  Declaration of class PdbEmcWalkTof
//
//  Purpose: Stores Walk Tofs values for one EMCAL channel.
//
//  Description:
//
//  Author: Hugues Delagrange
//-----------------------------------------------------------------------------
#ifndef __PDBEMCWALKTOF_HH__
#define __PDBEMCWALKTOF_HH__

#include "PdbCalChan.hh"
/** Used to Walk Tofs of a given channel as a vector :\\
    + Value1 ;\\
    + Value2.\\
   */ 


class PdbEmcWalkTof : public PdbCalChan {
public:
          /// Default constructor
	  PdbEmcWalkTof();
          /// dtor
	  virtual ~PdbEmcWalkTof();
    
          /// Retrieve the values
          void GetWalkTofs(float & thevalue1, float & thevalue2);
          /// Set the proper values
          void SetWalkTofs(float   thevalue1, float   thevalue2);

          ///
	  virtual void print() const;

private:
          ///
          float fValue1;
          ///
          float fValue2;

  ClassDef(PdbEmcWalkTof,1);
};

#endif /* __PDBEMCWALKTOF_HH__ */
