#ifndef __MTECPIDMODULE_H__
#define __MTECPIDMODULE_H__

#include <phool.h>

class PHCompositeNode;
class TecOutV7;
class TecGeometryObject;
class TecCalibrationObject;
class CglTrack;
class DchTrack;
class CrkGeometryObject;
class TFile;
class TNtuple;

/**
Determines particle ID using TEC dE/dX and
particle momentum from global tracking.
Detailed documentation: \URL{http://www.rhic.bnl.gov/~lebedev/tec/pidnew.html}
*/

class mTecPIDModule
{
public:
///
  mTecPIDModule();
///
  virtual ~mTecPIDModule();
///
  PHBoolean event(PHCompositeNode*);
///
  int GetBeg12(TecGeometryObject* TGO, float* beg1, float* beg2);
///
  int GetTrackLength(int itrk, 
                     TecOutV7* tecout,
                     CglTrack* cgl,
                     DchTrack* dch,
                     float& TrackLength);

///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_method(int meth){method=meth;}
///
  void set_Truncation(float trnk){Truncation=trnk;}
///
  void set_mip(float mp){mip=mp;}
///
  void set_el_gaindep(int k, float eg) {el_gaindep[k]=eg;}
///
  void set_mip_gaindep(int k, float mg) {mip_gaindep[k]=mg;}
///
  void set_el_trnkdep(int k, float tg) {el_trnkdep[k]=tg;}
///
  void set_mip_trnkdep(int k, float tg) {mip_trnkdep[k]=tg;}
///
  void set_Sigma(int k, float s) {Sigma[k]=s;}
///
  void set_numbinmin(int nb) {numbinmin=nb;}
///
  void set_Calibration(int k, float cal) {Calibration[k]=cal;}
///
  void set_UseOnlyMerged(int uom) {UseOnlyMerged=uom;}
///
  void set_Write2File(int wtf) {Write2File=wtf;}
///
  void set_Write2Ntuple(int wtn) {Write2Ntuple=wtn;}
///
  void set_RejectOverlaps(int ro) {RejectOverlaps=ro;}
///
  void set_PerfectPID(int pp) {PerfectPID=pp;}
///
  void set_UseTec(int pp) {UseTec=pp;}
///
  void set_SetTecPadCuts(float c1, float c2) {TecPc1Cut=c1; TecPc3Cut=c2;}
///
  void set_TRChargeCut(int a) {TRChargeCut = a;}
///
  void set_TRTimeCut(int a) {TRTimeCut = a;}
///
  void set_CrkRingCut(float a) {CrkRingCut = a;}

private:
///
  int TRChargeCut;           /* minimum ADC for TR, NTR variables */
///
  int TRTimeCut;          /* minimum Timebin for TR, NTR variables */
///
  int Verbose;          /* set for debug printing output */
///
  int method;           /* use dE/dX per 1mm or per 1 time bin (0/1) */
///
  float Truncation;     /* truncation of the mean */
///
  float mip;            /* MIP position (FADC channels per 1mm of track
                           for 0.6 truncation and effective gas gain 10000) */
///
  float el_gaindep[3];  /* gain dependence parameters for e+- */
///
  float mip_gaindep[3]; /* gain dependence parameters for MIPs */
///
  float el_trnkdep[3];  /* truncation dependence parameters for e+- */
///
  float mip_trnkdep[3]; /* truncation dependence parameters for MIPs */
///
  float Sigma[4];       /* dE/dX fluctuations for e, pi, p, K */
///
  int numbinmin;        /* parameter for short track rejection */
///
  float Calibration[2]; /* calibration adjustment parameters */
///
  int UseOnlyMerged;    /* */
///
  int Write2File;       /* */
///
  int Write2Ntuple;     /* */
///
  int RejectOverlaps;   /* do not use hits associated with more than 1 track */
///
  int PerfectPID;   /* in simulation, use Geant momentum */
///
  int UseTec;   /* forse using momentum determined by Tec */
///
  float TecPc1Cut;
///
  float TecPc3Cut;
///
  float CrkRingCut;
///
  int hitcut;

  TFile* OutputNtupleFile;
  TNtuple* ntpcheck;
  CrkGeometryObject* cgo;
};
#endif /*__MTECPIDMODULE_H__*/

