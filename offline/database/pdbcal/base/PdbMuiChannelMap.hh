//-----------------------------------------------------------------------------
//
//  The pdbcal package
//  Copyright (C) PHENIX collaboration, 1999
//
//  Declaration of class PdbMuiChannelMap
//
//  Purpose: MuID hardware-software map entry
//
//  Description: Map between a set of MuID software indices (arm, gap,
//               panel, orientation, twopack range) and the corresponding
//               hardware indices (module, roc, cable, channel range).
//               The full map has one of these objects for each data
//               word (corresponding to one cable input at the ROC)
//               from the DCM.
//
//  Author: pope
//-----------------------------------------------------------------------------

#ifndef __PDBMUICHANNELMAP_HH__
#define __PDBMUICHANNELMAP_HH__

#include "PdbCalChan.hh"

class PdbMuiChannelMap : public PdbCalChan
{
public:
  PdbMuiChannelMap();
  PdbMuiChannelMap(unsigned int module_id,
		   short roc, short cable, short sequence,
		   short first_channel, short last_channel,
		   short arm, short gap, short panel, short orient,
		   short first_twopack, short last_twopack,
		   short first_location, short last_location);
  PdbMuiChannelMap(const PdbMuiChannelMap& rhs);
  virtual ~PdbMuiChannelMap();

  PdbMuiChannelMap& operator=(const PdbMuiChannelMap& rhs);

  virtual void print() const;

  virtual void SetSoftwareIndex(short arm, short gap, short panel,
				short orient);

  virtual void SetHardwareIndex(unsigned int module_id, short roc,
				short cable, short sequence);

  void SetChannelRange(const short& first, const short& last);
  void SetTwoPackRange(const short& first, const short& last);
  void SetLocationRange(const short& first, const short& last);

  void SoftwareIndex(const short& channel,
		     short& arm, short& gap, short& panel,
		     short& orient, short& twopack) const;
  void SoftwareIndex(short& arm, short& gap, short& panel,
		     short& orient) const;
  void LocationIndex(const short& twopack,
		     short& arm, short& gap, short& orient,
		     short& location) const;
  void HardwareIndex(const short& twopack,
		     unsigned int& module, short& roc, short& sequence, 
		     short& cable, short& channel) const;
  void HardwareIndex(unsigned int& module, short& roc, short& sequence, 
		     short& cable) const;

  void ChannelRange(short& first, short& last) const;
  void TwoPackRange(short& first, short& last) const;
  void LocationRange(short& first, short& last) const;

  virtual bool HasHardwareIndex(const unsigned int& module_id,
				const short& sequence) const;
  virtual bool HasHardwareIndex(const unsigned int& module_id,
				const short& roc, const short& cable,
				const short& channel = -1) const;
  virtual bool HasSoftwareIndex(const short& arm, const short& gap,
				const short& panel,
				const short& orient,
				const short& twopack = -1) const;

private:
  unsigned int fModuleID;/** Which FEM is this (0, 0x10, 0x1000, 0x1010)? */
  short fArm;            /** To which arm does this FEM belong? */
  short fOrientation;    /** To which orientation does this FEM belong? */
  short fGap;            /** From which gap does the data come? */
  short fPanel;          /** From which panel does the data come? */
  short fWordIndex;      /** Location of this word (0-119) in the data list */
  short fROC;            /** Identifier (0-19) of the ROC - unique to FEM */
  short fWordInROC;      /** Identifier (0-5) of the word - unique to ROC */
  short fFirstChannel;   /** First bit (0-15) to set in the data word */
  short fLastChannel;    /** Last bit (0-15) to set in the data word */
  short fFirstTwoPack;   /** Two-pack corresponding to first channel */
  short fLastTwoPack;    /** Two-pack corresponding to last channel */
  short fFirstLocation;  /** Integral location in plane of first two-pack */
  short fLastLocation;   /** Integral location in plane of last two-pack */
  short Max(const short& num1, const short& num2) const;
  short Min(const short& num1, const short& num2) const;

  ClassDef(PdbMuiChannelMap,1);
};

#endif /* __PDBMUICHANNELMAP_HH__ */
