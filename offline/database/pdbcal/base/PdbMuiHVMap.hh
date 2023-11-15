//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 2002
//
//  Declaration of class PdbMuiHVMap
//
//  Purpose: MuID hardware-software high voltage map entry
//
//  Description: Map between a set of MuID software indices (arm, gap,
//               panel, orientation, chain, twopack range) and the corresponding
//               hardware indices (mainframe, slot, channel).
//               The full map has one of these objects for each HV chain.
//
//  Author: A.M. Glenn
//-----------------------------------------------------------------------------

#ifndef __PDBMUIHVMAP_HH__
#define __PDBMUIHVMAP_HH__

#include "PdbCalChan.hh"

class PdbMuiHVMap : public PdbCalChan
{
public:
  PdbMuiHVMap();
  PdbMuiHVMap(short mainFrame,
	      short slot, short supply_channel,
	      short arm, short gap, short panel, short orient,
	      short chain, short first_twopack, short last_twopack);

  PdbMuiHVMap(const PdbMuiHVMap& rhs);
  virtual ~PdbMuiHVMap() {}

  PdbMuiHVMap& operator=(const PdbMuiHVMap& rhs);

  virtual void print() const;

  virtual void SetSoftwareIndex(short arm, short gap, short panel,
				short orient, short chain);

  virtual void SetHardwareIndex(short mainFrame,
				short slot, short supply_channel);


  void SetTwoPackRange(const short& first, const short& last);

  void SoftwareIndex(short& arm, short& gap, short& panel,
		     short& orient, short& chain) const;
  void HardwareIndex(short& mainFrame, short& slot, short& supply_channel) const;

  void TwoPackRange(short& first, short& last) const;

  virtual bool HasHardwareIndex(const short& mainFrame, const short& slot, 
				const short& supply_channel) const;
  virtual bool HasSoftwareIndex(const short& arm, const short& gap,
				const short& panel,
				const short& orient,
				const short& chain,
				const short& twopack = -1) const;
  virtual bool HasSoftwareIndexLayer(const short& arm, const short& gap,
				const short& panel,
				const short& orient,
				const short& layer,
				const short& twopack) const;

private:
  short fMainFrame;
  short fSlot;
  short fSupplyChannel;
  short fArm;            /** To which arm does this FEM belong? */
  short fOrientation;    /** To which orientation does this FEM belong? */
  short fGap;            /** From which gap does the data come? */
  short fPanel;          /** From which panel does the data come? */
  short fChain;
  short fFirstTwoPack;   /** Two-pack corresponding to first channel */
  short fLastTwoPack;    /** Two-pack corresponding to last channel */
  short Max(const short& num1, const short& num2) const;
  short Min(const short& num1, const short& num2) const;

  ClassDef(PdbMuiHVMap,1);
};

#endif /* __PDBMUIHVMAP_HH__ */
