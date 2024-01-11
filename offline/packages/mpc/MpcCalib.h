#ifndef __MPCCALIB_H__
#define __MPCCALIB_H__

#include <string>
#include <PdbPmtPeak.hh>
#include <PdbMpcGainCorr.hh>
#include <PdbMpcLed.hh>
#include <PdbMpcShape.hh>
//#include <PdbMpcLeakage.hh>
#include <PHTimeStamp.h>
#include <PHObject.h>
#include <vector>

class TH2;
class PHCompositeNode;

class MpcCalib : public PHObject
{
public:

  static const int MAXCH = 576;
  static const int MAXAMU = 64;

  MpcCalib(PHCompositeNode *topNode = 0, const int do_download = 1);
  virtual ~MpcCalib();
  static MpcCalib *instance(PHCompositeNode *topNode = 0);

  float get_adc_gain(int ch) const { return adc_gain[ch].getPeakChannel(); }
  float get_ped(int ch) const { return ped[ch].getPeakChannel(); }
  float get_pedrms(int ch) const { return ped[ch].getDeviation(); }
  int   get_pedcounts(int ch) const { return ped[ch].getStatus(); }
  float get_ped_hgpre(int ch, int amu) const { return hgpre[ch*MAXAMU+amu].getPeakChannel(); }
  float get_ped_hgpost(int ch, int amu) const { return hgpost[ch*MAXAMU+amu].getPeakChannel(); }
  float get_ped_lgpre(int ch, int amu) const { return lgpre[ch*MAXAMU+amu].getPeakChannel(); }
  float get_ped_lgpost(int ch, int amu) const { return lgpost[ch*MAXAMU+amu].getPeakChannel(); }
  float get_hilo_ratio(int ch) const { return hilo_ratio[ch].getPeakChannel(); }
  float get_hilo_limit(int ch) const { return hilo_limit[ch].getPeakChannel(); }
  float get_hilo_limit_rms(int ch) const { return hilo_limit[ch].getDeviation(); }
  float get_tdc_leastcount(int ch) const { return tdc_leastcount[ch].getPeakChannel(); }

  float get_slewcor0(int ch) const { return slewcor0[ch]; }
  float get_slewcor1(int ch) const { return slewcor1[ch]; }
  float get_t0(int ch) const { return t0[ch]; }
  int get_deadhot(int ch) const { return deadhot[ch]; }
  float get_gaincorr(const int ich) const;
  float get_led(const int ch) { return led[ch].get_mean(); }

  PdbMpcShape *get_pshape(int ch) { return &pshapes[ch]; }
  PdbMpcShape *get_psherr(int ch) { return &psherrs[ch]; }
  PdbMpcShape *get_np1shape(int ch) { return &np1shapes[ch]; }
  PdbMpcShape *get_np1sherr(int ch) { return &np1sherrs[ch]; }

  // added to facilitate Run-16 gain calibrations (7/17/2020 JGL)
  void set_adc_gain(int ch, float gain) { if((ch>=0)&&(ch<MAXCH)) adc_gain[ch].setPeakChannel(gain); }

  int  Download_Gains(const std::string& dbase_file);
  int  Download_Pedestals(const std::string& dbase_file, const std::string& pedtype = "ped");
  int  Download_HiLoRatio(const std::string& dbase_file);
  int  Download_HiLoLimit(const std::string& dbase_file);
  int  Download_TDC_Overflow(const std::string& dbase_file);
  int  Download_TDC_LeastCount(const std::string& dbase_file);
  int  Download_SlewCorrection(const std::string& dbase_file);
  int  Download_T0(const std::string& dbase_file);
  int  Download_GainCorr(const std::string& dbase_file);
  int  Download_Led(const std::string& dbase_file);
  int  Download_Shapes(const std::string& dbase_file, const std::string& shapetype = "p");

  // This will eventually be cleaned up once we have fine tuned this a bit
  //int  Download_LeakageCorrection(const std::string& dbase_file);
  //const float get_leakage_correction(const int iarm, const float e, const float x, const float y);

  int  Download_All(const std::string& dbase_directory);
  int  Download_All(const PHTimeStamp& tstamp);
  int  Download_All(const PHTimeStamp& tstamp, int run_number);
  int  Download_All(PHCompositeNode *topNode);

  // This will eventually be moved out to another class.
  int  Download_DeadHot(const int run = 9999999);     // get dead/hot tower list
 
  int  StoreInDatabase(PHTimeStamp& tStart, const std::string& what, const std::string& username = "", const std::string& description = "", const PHTimeStamp& tStop = PHTimeStamp(2147483647) );
  int  Download(const PHTimeStamp& tstamp, const std::string& what);
  //int  Download(const std::string& dbase_file, const std::string& what);

  void AddToNodeTree(PHCompositeNode *topNode);

  void Dump_to_file(const std::string& what = "ALL");

  PHTimeStamp *getStartTime() { return &StartTime; }
  PHTimeStamp *getEndTime() { return &EndTime; }

  void Reset();
  void Print(Option_t* option) const;
  int IsValid(const int verbosity = 0) const;

private:
  static MpcCalib *__instance;

  int status;

  PHTimeStamp StartTime;        // calibration start and end time
  PHTimeStamp EndTime;

  PdbPmtPeak tdc_oflow[MAXCH];
  PdbPmtPeak tdc_leastcount[MAXCH];

  PdbPmtPeak adc_gain[MAXCH];

  PdbPmtPeak hgpre[MAXCH*MAXAMU];	// Pedestals
  PdbPmtPeak hgpost[MAXCH*MAXAMU];
  PdbPmtPeak lgpre[MAXCH*MAXAMU];
  PdbPmtPeak lgpost[MAXCH*MAXAMU];

  PdbPmtPeak  ped[MAXCH];               // New HBD Pedestals
  PdbMpcShape pshapes[MAXCH];           // New HBD Photon Shapes
  PdbMpcShape psherrs[MAXCH];            // New HBD Photon Error
  PdbMpcShape np1shapes[MAXCH];         // New HBD non-photonic1 Shapes
  PdbMpcShape np1sherrs[MAXCH];          // New HBD non-photonic1 Error

  PdbPmtPeak hilo_ratio[MAXCH];
  PdbPmtPeak hilo_limit[MAXCH];	// low adc limit at which one cannot use the high gain data

  float slewcor0[MAXCH];
  float slewcor1[MAXCH];
  float t0[MAXCH];
  int   deadhot[MAXCH];		// 0=good, +1 = hot, -1 = dead  

  PdbMpcGainCorr gain_corr[MAXCH];	// Gain Correction
  PdbMpcLed led[MAXCH];			// Gain correction from LED

  // We have removed fine tuned leakage corrections for now
  //int nleakage;				// number of leakage data
  //PdbMpcLeakage pdbleakage[80000];	// Leakage Data from Database
  //TH2 *h2_leakage[2][4];	// [arm][energybin], ebin 0-5,5-10,10-20,20-30+

  ClassDef(MpcCalib,0)
};


#endif	// __MPCCALIB_H__

