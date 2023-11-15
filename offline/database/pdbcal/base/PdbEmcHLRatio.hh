//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999, 2000
//
//  Declaration of class PdbEmcHLRatio
//
//  Purpose: Obsolete class (see PdbEmcHLRatioVector for actual one).
//
//  Description: Stores the H/L gain ratio for one channel
//
//  Author: H. Delagrange
//-----------------------------------------------------------------------------
#ifndef __PDBEMCHLRATIO_HH__
#define __PDBEMCHLRATIO_HH__

#include "PdbCalChan.hh"

/** Store the H/L gain ratio for one channel. */

class PdbEmcHLRatio : public PdbCalChan {
public:
	  PdbEmcHLRatio();
	  virtual ~PdbEmcHLRatio();

  /// get the ratio for this channel
          double GetRatio(void) const { return fRatio;}
	  virtual void print() const;
  /// set the ratio for this channel
          void SetRatio(const double dratio);

private:
          double fRatio;

  ClassDef(PdbEmcHLRatio,1);
};

#endif /* __PDBEMCHLRATIO_HH__ */
