#ifndef __MTECTRACKEVALMODULE_H__
#define __MTECTRACKEVALMODULE_H__

#include "TecTrackHit.hh"

#include "phool.h"

#include <vector>

class PHCompositeNode;
class TNtuple;
class TFile;

/** TEC Tracking Efficiency Evaluator. Detailed documentation: not yet ready. */

class mTecTrackEvalModule
{

public:
///
  mTecTrackEvalModule();
///
  virtual ~mTecTrackEvalModule(){}
///
  PHBoolean event(PHCompositeNode *);
///
  int findDominantContributor(PHCompositeNode* root, int itrk, float& NHITS);
///
  int WriteSimulatedEvent(PHCompositeNode* root);
///
  void WriteSingleTracks(PHCompositeNode* root);
///
  int ReadSingleTracks(PHCompositeNode* root);
///
  void MergeSimulatedEvent(PHCompositeNode* root);
///
  void MergeSingleTracks(PHCompositeNode* root);
///
  void Evaluate(PHCompositeNode* root);
///
  void SaveEvalFile();
///
  void set_Verbose(int verbose){Verbose=verbose;}
///
  void set_Chi2Max(float chi2max){Chi2Max=chi2max;}
///
  void set_Merge(int merge){Merge=merge;}
///
  void set_Ecut(float ecut){Ecut=ecut;}
///
  void set_LineCut(float lcut){LineCut=lcut;}
///
  void set_Write2File(int w2f){Write2File=w2f;}
///
  void set_OutFileName(const char* name){OutFileName=name;}
///
  void set_InFileName(const char* name){InFileName=name;}
///
  void set_EvalFileName(const char* name){EvalFileName=name;}

private:
///
  int Verbose;
///
  float Chi2Max;
///
  int Merge;
///
  float Ecut;
///
  float LineCut;
///
  int Write2File;
///
  const char* OutFileName;
///
  const char* InFileName;
///
  std::vector<TecTrackHit> TRACKS;
///
  int currentTrack;
///
  TNtuple* ntpeval;
///
  TFile* EvalFile;
///
  const char* EvalFileName;
  
};
#endif /*__MTECTRACKEVALMODULE_H__*/
