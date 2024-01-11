#ifndef CalibRuns_HH
#define CalibRuns_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TNamed.h>
#include "TNamedDir.hh"
#include <TH1.h>
#include <TH2.h>
#include <TF1.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TGraph.h>
#include <TGraphErrors.h>
#include <TMap.h>
#include "TH1Xch.hh"
#include "fitpeak.hh"
#define DEFAULT_BUFFERSIZE 300

class CalibRuns : public TNamedDir {
protected:
  int _run;
  int _seq;
  int _nch;
  static int _glb_run;
  static int _glb_seq;
  bool _debug;             //!
protected:
  // -- global --
  int _all_num;
  float _all_sum,_all_sum2;
  float _all_low,_all_high;
  int FillGlobal(float x);
  void ResetGlobal();
  // -- buffer --
  int _buf_size;
  int _buf_totsize;
  int* _buf_ch;     //[_buf_totsize]
  float* _buf_x;    //[_buf_totsize]
  float* _buf_y;    //[_buf_totsize]
  float* _buf_z;    //[_buf_totsize]
  int FillBuffer(int ch,float x,float y,float z);
  void InitializeBuffer(int buffersize);
public:
  void ResetBuffer();
  void SetBuffersize(int buffersize);
  inline int GetBuffersize(){ return _buf_size; };
  inline float GetBufferX(int n){if( n<_buf_size) return(_buf_x[n]); else return 0; };
  inline float GetBufferY(int n){if( n<_buf_size) return(_buf_y[n]); else return 0; };
  inline int GetBufferCh(int n){if( n<_buf_size) return(_buf_ch[n]); else return 0; };

public:
  //protected:
  TDirectory*  _writedir;
  TMap*      _keymap;    //[1]  //TKey(for CalibObj) and TFile map which are read from root files.
  TObjArray* _objarray;  //[1]  //CalibObj lists which will be written into root files
  CalibObj*  _calibobj;  //[1]  //current object
  TObjArray* _filearray; //!    //Garbage collection for Read(char* filename) method.
protected:
  bool _initialized;
  virtual int Initialize();  // User must implement this to create CAlibObj.
public:
  CalibRuns();
  CalibRuns(const char* name, const char* title,int nch);
  ~CalibRuns();

  // --- basic method
  virtual int Fill(int run,int seq,int ch,float x,float y = 1.,float z = 1.);
  virtual void Reset(char* option="");
  virtual int Append(CalibRuns* calibr,float scale=1.);
  virtual int Append(TDirectory* dir,float scale=1.);
  virtual bool Append(CalibObj* calibo,int run,int seq,float scale=1.);
  virtual int ReadList(char* filename);
  virtual int Read(char* filename);
  virtual int Read(TDirectory* dir);
  virtual int Readable(TDirectory* dir);
  virtual bool Read(int run,int seq,TDirectory* dir=NULL);
  virtual int Write(TDirectory* dir=gDirectory);
  virtual void Print(Option_t* option="") const;

  // --- Accessor to the data
  inline int GetRun(){return _run;};
  inline int GetSeq(){return _seq;};
  inline int GetNch(){return _nch;};
  float GetMean(){return _all_sum/_all_num; };
  float GetRMS(){return sqrt(_all_sum2/_all_num - _all_sum/_all_num*_all_sum/_all_num); };
  void SetOptDebug(bool d=true){_debug=d;};

  ClassDef(CalibRuns,2) //Base class of runs collection for calibration
};
//
#endif
//
