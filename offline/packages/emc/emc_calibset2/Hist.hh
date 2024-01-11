#ifndef Hist_HH
#define Hist_HH

#include <Rtypes.h>
#include <TObject.h>
#include <TDirectory.h>
#include <TCanvas.h>
#include <TPostScript.h>
#include <TClonesArray.h>
#include <TH1.h>
#include <TH2.h>
#include <TH3.h>
#include "Evt.hh"
#include "Pid.hh"
#include "TNamedDir.hh"

class Hist : public TNamedDir {
public:
  Hist();
  ~Hist();
  Hist(char* pname,char* ptitle,char* opt="");
  //
  virtual bool Add(Hist* hist);
  virtual bool Add(TFile* f);
  virtual bool Reset();
  virtual void Draw(TCanvas* c1=0,TPostScript* ps=0){/*     */};
  virtual bool Fill(Global& glb,Track& trk,Clust& clt,Pid& emcpid,float weight=1.);
  virtual void Print();
  unsigned int GetEntries(){ return ientries; };
  bool IsValidSector(int isect);
  static void SetOptDebug(bool d=true){_debug = d;};
  static bool GetOptDebug(){return _debug; };

  // --- Array of histgrams ---
  TObjArray _ptarray;               // array to THxF histgram's pointer's pointer
  TH1* Register(TH1*);

private:
  void CalculateAngle();

protected:
  // --- Global ---
  char hname[256],htitle[256];
  int isect,iz,iy;
  int ism_sect,ismz_sect,ismy_sect;
  int itwr,itwr_sect;
  int ientries;
  int iarm,ipbscgl;
  float v_angsect[3][8];
  float angle;
  // --- PID --- // This is obsolete. Will be deleted in the future.
  int ipid,iepid;
  int iemce;
  int iprob;
  int imul;
  // --- DEBUG ---
  static bool _debug;                         //! debug option

  ClassDef(Hist,1)
};
//
#endif
//
