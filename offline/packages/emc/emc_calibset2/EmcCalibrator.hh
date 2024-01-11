#ifndef EmcCalibrator_HH
#define EmcCalibrator_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include "TNamedDir.hh"
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include "CalibRuns.hh"
#include "CalibObj.hh"
#define DEFAULT_DEBUG false

class EmcCalibratorQA;

class EmcCalibrator : public TNamedDir {
private:
  TGraph _gra_qa_twr;
  TGraph _gra_qa_run;
  int _qa_bit;
  int _cut_run[2];
protected:
  int _nch;
  int _cur_run;
  int _cur_seq;
  bool _debug;
  //  bool initialized;
  //  void Initialize(int nch,int nbins);
public:
  EmcCalibrator();
  EmcCalibrator(const char* name, const char* title);
  EmcCalibrator(const char* name, const char* title,int nch);
  ~EmcCalibrator();

  // Analysis method.
  virtual int AnalyzeCalibRuns(CalibRuns* calibruns,char* opt="");
  virtual int AnalyzeCalibObj(CalibObj* claibobj,char* opt="") ;
  virtual int AnalyzeTwr(char* opt="");
  virtual int Reset(char* option="");
  virtual void Print(Option_t* option="") const{/*   */};

  // Access for QA 
  int SetEmcCalibratorQA(EmcCalibratorQA& calibqa);
  int AddEmcCalibratorQA(EmcCalibratorQA& calibqa);
  void SetQAbit(int qabit){_qa_bit = qabit;};
  int GetQAbit(){return _qa_bit;};
  // Access for QA twr
  int GetNQAtwr(int qabit=0) const;
  int GetNQAtwr_all() const;
  int GetQAtwr(int ch);
  TGraph& GetQAtwr(){ return _gra_qa_twr; };
  bool SetQAtwr(int ch,int value=-1);
  bool AddQAtwr(int ch,int value=-1);
  bool ResetQAtwr();
  // Access for QA run
  int GetNQArun(int qabit=0) const;
  int GetNQArun_all() const;
  int GetQArun(int run);
  bool SetQArun(int run,int value=-1);
  bool AddQArun(int run,int value=-1);
  TGraph& GetQArun(){ return _gra_qa_run; };
  bool ResetQArun();

  // Data Accessor.
  int GetNch(){return _nch;};
  void SetCutRun(int min,int max){_cut_run[0]=min;_cut_run[1]=max;};
  int GetCutRunMin(){return _cut_run[0];};
  int GetCutRunMax(){return _cut_run[1];};
  virtual bool WriteFile(char* run_file,char* twr_file){return false;};
  virtual bool WriteFileQArun(char* qa_run_file);
  virtual bool WriteFileQAtwr(char* qa_twr_file);
  virtual bool ReadFileQArun(char* qa_run_file);
  virtual bool ReadFileQAtwr(char* qa_twr_file);
  void SetOptDebug(bool debug=1){_debug = debug;};

  ClassDef(EmcCalibrator,2) //EMC Calibrator base class
};
//
#endif
//
