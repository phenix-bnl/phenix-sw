#ifndef __PDBSPINGL1P_HH__
#define __PDBSPINGL1P_HH__
#include "PdbCalChan.hh"
//=============================================
// Offline Calibration for GL1p module.
//   The purpose is to modify the shift of the beam crossing information in GL1p.
//   This is very critical for spin physics.
//       Created by H.Torii 2003/Aug/28
//=============================================

class PdbSpinGL1p : public PdbCalChan 
{
public:
  PdbSpinGL1p();
  PdbSpinGL1p(const PdbSpinGL1p &p);
  PdbSpinGL1p& operator = (const PdbSpinGL1p &p);

  virtual ~PdbSpinGL1p(){};
  virtual void setRunNum(const int runnum){_runnum=runnum;};
  virtual void setOffsetBeamX(const int offset){_offset_beamx=offset;};
  virtual void setStatOffsetBeamX(bool stat){_stat_offset_beamx=stat;};
  virtual void set(const int runnum,const int offset,const bool stat);

  virtual int getRunNum(){return _runnum;};
  virtual int getOffsetBeamX(){return _offset_beamx;};
  virtual bool getStatOffsetBeamX(){return _stat_offset_beamx;};

  virtual void print() const;

private:
  int _runnum;
  bool _stat_offset_beamx;
  int _offset_beamx;

  ClassDef(PdbSpinGL1p,1);
};

#endif /* __PDBSPINGL1P_HH__ */
