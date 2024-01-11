///////////////////////////////////////////////////////////////
//
// SpinDBContent class
// Author      : Y. Fukao
// Description : Content of spin database
// Created     : 2005-03-04
//
// ERROR_VALUE       : Error value
// NCROSS            : Number of crossing (120)
// runnum            : Run number
// qa_level          : Level of QA for stored data. 
// fillnum           : Fill number
// badrun            : Run QA by spin analysis, 1:bad 0:good
// cross_shift       : Crossing sfhit, corrected=(raw+cross_shift)%NCROSS
// bpol              : Blue beam polarization
// bpolerr           : Blue beam polarization error
// bpolsys           : Blue beam polarization systematic error
// ypol              : Yellow beam polarization
// ypolerr           : Yellow beam polarization error
// ypolsys           : Yellow beam polarization systematic error
// bpat              : Spin pattern at IP12. (NOT at PHENIX)
// ypat              : Spin pattern at IP12. (NOT at PHENIX)
// scaler_bbc_vtxcut : scaler (BBCLL1 vertex cut)
// scaler_bbc_nocut  : scaler (BBCLL1 without vertex cut)
// scaler_zdc_wide   : scaler (ZDCLL1 wide cut)
// scaler_zdc_narrow : scaler (ZDCLL1 narrow cut)
// bad_bunch         : Bad bunch QA, 0:good else:bad
// tc_x_blue         : Transverse component, horizontal direction, blue beam
// tc_x_blueerr      : Error of tc_x_blue
// tc_y_blue         : Transverse component, vertical direction, blue beam
// tc_y_blueerr      : Error of tc_y_blue
// tc_x_yellow       : Transverse component, horizontal direction, yellow beam
// tc_x_yellowerr    : Error of tc_x_yellow
// tc_y_yellow       : Transverse component, vertical direction, yellow beam
// tc_y_yellowerr    : Error of tc_y_yellow
//
//
//----- NOTE: by Y.Fukao (2007-03-25) -----
//
// int SetScaler(int channel,int bunch,long long value) is added.
// "channel" corresponds to that of GetScaler(int channel,int bunch)
// channel 0: SetScalerBbcVertexCut(bunch,value);
// channel 1: SetScalerBbcNoCut(bunch,value);
// channel 2: SetScalerZdcWide(bunch,value);
// channel 3: SetScalerZdcNarrow(bunch,value);
//
//----- NOTE: by K.Aoki (2006-06-14) -----
//
// scaler content changes in Run6. Thus "GetScaler(int channel,int bunch)"
// was added. You can still use "GetScalerBbcNoCut(int bunch)" etc. , but you have
// to be careful about what you get.
//
//----- NOTE: by T.Kempel (2008-03-07) -----
//
// Added qa_level to have versioning in the DB.  Default QA is most reliable
// but others can be used to get older (or newer) versions of information.
// See 
//
// https://www.phenix.bnl.gov/WWW/offline/wikioffline/index.php/Spin_DataBase:When%2C_what_where_why_and_how
//
// for details on other versions.  Version info should be maintained there.
//
////////////////////////////////////////////////////////////////

#ifndef _SPINDBCONTENT_
#define _SPINDBCONTENT_

#include <stdio.h>
#include <TObject.h>

class SpinDBContent : public TObject{
public:
  SpinDBContent(void){Initialize();}
  ~SpinDBContent(void){;}
  void Initialize(void);
  static int GetNCrossing(void){return(NCROSS);}
  static int GetErrorValue(void){return(ERROR_VALUE);}
  int CheckBunchNumber(int bunch);
  void Print(Option_t* ="") const;

  int GetRunNumber(void){return(runnum);}
  int GetQALevel(void){return(qa_level);}
  int GetFillNumber(void){return(fillnum);}
  int GetBadRunFlag(void){return(badrun);}
  int GetCrossingShift(void){return(cross_shift);}

  // bunch xing id from SpinDataEventOut::SpinGL1CrossingID
  int GetPolarizationBlue(int bunch,float &value,float &error);
  int GetPolarizationBlue(int bunch,float &value,float &error,float &syserr);
  int GetPolarizationBlue(int bunch,double &value,double &error);
  int GetPolarizationBlue(int bunch,double &value,double &error,double &syserr);
  int GetPolarizationYellow(int bunch,float &value,float &error);
  int GetPolarizationYellow(int bunch,float &value,float &error,float &syserr);
  int GetPolarizationYellow(int bunch,double &value,double &error);
  int GetPolarizationYellow(int bunch,double &value,double &error,double &syserr);
  int GetSpinPatternBlue(int bunch);
  int GetSpinPatternYellow(int bunch);
  long long GetScalerBbcVertexCut(int bunch);
  long long GetScalerBbcNoCut(int bunch);
  long long GetScalerZdcWide(int bunch);
  long long GetScalerZdcNarrow(int bunch);
  long long GetScaler(int channel,int bunch);
  int GetBadBunchFlag(int bunch);

  // compatible with bunch xing id from TrigLvl1::get_lvl1_clock_cross
  int GetPolarizationBluePHENIX(int bunch,float &value,float &error) {return GetPolarizationBlue((bunch + cross_shift)%120, value, error);}
  int GetPolarizationBluePHENIX(int bunch,float &value,float &error,float &syserr) {return GetPolarizationBlue((bunch + cross_shift)%120, value, error, syserr);}
  int GetPolarizationBluePHENIX(int bunch,double &value,double &error) {return GetPolarizationBlue((bunch + cross_shift)%120, value, error);}
  int GetPolarizationBluePHENIX(int bunch,double &value,double &error,double &syserr) {return GetPolarizationBlue((bunch + cross_shift)%120, value, error, syserr);}
  int GetPolarizationYellowPHENIX(int bunch,float &value,float &error) {return GetPolarizationYellow((bunch + cross_shift)%120, value, error);}
  int GetPolarizationYellowPHENIX(int bunch,float &value,float &error,float &syserr) {return GetPolarizationYellow((bunch + cross_shift)%120, value, error, syserr);}
  int GetPolarizationYellowPHENIX(int bunch,double &value,double &error) {return GetPolarizationYellow((bunch + cross_shift)%120, value, error);}
  int GetPolarizationYellowPHENIX(int bunch,double &value,double &error,double &syserr) {return GetPolarizationYellow((bunch + cross_shift)%120, value, error, syserr);}
  int GetSpinPatternBluePHENIX(int bunch) {return GetSpinPatternBlue((bunch + cross_shift)%120);}
  int GetSpinPatternYellowPHENIX(int bunch) {return GetSpinPatternYellow((bunch + cross_shift)%120);}
  long long GetScalerBbcVertexCutPHENIX(int bunch) {return GetScalerBbcVertexCut((bunch + cross_shift)%120);}
  long long GetScalerBbcNoCutPHENIX(int bunch) {return GetScalerBbcNoCut((bunch + cross_shift)%120);}
  long long GetScalerZdcWidePHENIX(int bunch) {return GetScalerZdcWide((bunch + cross_shift)%120);}
  long long GetScalerZdcNarrowPHENIX(int bunch) {return GetScalerZdcNarrow((bunch + cross_shift)%120);}
  long long GetScalerPHENIX(int channel,int bunch) {return GetScaler(channel, (bunch + cross_shift)%120);}
  int GetBadBunchFlagPHENIX(int bunch) {return GetBadBunchFlag((bunch + cross_shift)%120);}

  void GetTransCompBlueX(float &value,float &error);
  void GetTransCompBlueX(double &value,double &error);
  void GetTransCompBlueY(float &value,float &error);
  void GetTransCompBlueY(double &value,double &error);
  void GetTransCompYellowX(float &value,float &error);
  void GetTransCompYellowX(double &value,double &error);
  void GetTransCompYellowY(float &value,float &error);
  void GetTransCompYellowY(double &value,double &error);

  void SetRunNumber(int run){runnum=run; return;}
  void SetQALevel(int qa){qa_level=qa; return;}
  void SetFillNumber(int fill){fillnum=fill; return;}
  void SetBadRunFlag(int flag){badrun=flag; return;}
  void SetCrossingShift(int shift){cross_shift=shift; return;}
  int SetPolarizationBlue(int bunch,float value,float error);
  int SetPolarizationYellow(int bunch,float value,float error);
  int SetPolarizationBlue(int bunch,float value,float error,float syserr);
  int SetPolarizationYellow(int bunch,float value,float error,float syserr);
  int SetSpinPatternBlue(int bunch,int value);
  int SetSpinPatternYellow(int bunch,int value);
  int SetScalerBbcVertexCut(int bunch,long long value);
  int SetScalerBbcNoCut(int bunch,long long value);
  int SetScalerZdcWide(int bunch,long long value);
  int SetScalerZdcNarrow(int bunch,long long value);
  int SetScaler(int channel,int bunch,long long value);
  int SetBadBunchFlag(int bunch,int value);
  void SetTransCompBlueX(float value,float error);
  void SetTransCompBlueY(float value,float error);
  void SetTransCompYellowX(float value,float error);
  void SetTransCompYellowY(float value,float error);

private:
  static const int NCROSS;
  static const int ERROR_VALUE;

  int runnum;
  int qa_level;
  int fillnum;
  int badrun;
  int cross_shift;
  float bpol[120];
  float bpolerr[120];
  float bpolsys[120];
  float ypol[120];
  float ypolerr[120];
  float ypolsys[120];
  int bpat[120];
  int ypat[120];
  long long scaler_bbc_vtxcut[120];
  long long scaler_bbc_nocut[120];
  long long scaler_zdc_wide[120];
  long long scaler_zdc_narrow[120];
  int bad_bunch[120];
  float tc_x_blue;
  float tc_x_blueerr;
  float tc_y_blue;
  float tc_y_blueerr;
  float tc_x_yellow;
  float tc_x_yellowerr;
  float tc_y_yellow;
  float tc_y_yellowerr;

  ClassDef(SpinDBContent,1)
};

#endif /* _SPINDBCONTENT_ */
