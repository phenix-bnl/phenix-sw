#ifndef _PHEMBEDHISTOGRAMMER_HH
#define _PHEMBEDHISTOGRAMMER_HH
// ----------------------------
// Created by:  Jiangyong Jia
//-----------------------------

#include "phool.h"
#include "TString.h"

class PHEmbedMcRecoTrack;

class TFile;
class TH1;
class TList;
class TTree;

class PHEmbedHistogrammer {
private:
  static PHEmbedHistogrammer* _instance;
  PHEmbedHistogrammer();
public:
   virtual ~PHEmbedHistogrammer();
  static PHEmbedHistogrammer* instance();
  PHBoolean setFileName(TString name);
private:
  TString     filename;
  short       flagInit;
  TFile*      Datafile;
  int         verbose;
protected:
  TList *     ntupleList;
  TList *     histoList;
public:
  TFile*   getFile() const {return Datafile;}
  int      getVerbose() const {return verbose;}
  void     setVerbose(int val) {verbose = val;}
public:
  int      initializeFile(char* name=0);
  int      saveToFile(int option=0);
  int      addNtuple(TTree* val);
  int      addHistogram(TH1*histo);
  void     flush();
  void     CloseOutputFile();
private:
  TTree* embedMcRecoTrack;
  PHEmbedMcRecoTrack* mctrk;
public:
  PHBoolean fillPHEmbedMcRecoTrack(PHEmbedMcRecoTrack* track);
};
#endif
