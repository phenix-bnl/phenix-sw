#ifndef __MTECGAINSMODULE_H__
#define __MTECGAINSMODULE_H__

#include "phool.h"

class PHCompositeNode;
class TFile;
class TNtuple;

/** Calibrates TEC Raw Data.
Detailed documentation: not yet ready.
*/
class mTecGainsModule
{

public:

  mTecGainsModule();
  virtual ~mTecGainsModule(){}
  PHBoolean event(PHCompositeNode *);

  void set_Verbose(int verbose) {Verbose=verbose;}
  void set_RejectOverlaps(int rjov) {RejectOverlaps=rjov;}
  void set_WriteHistograms(int wh) {WriteHistograms=wh;}
  void set_RunNumber(int fh) {RunNumber=fh;}
  void set_RunNumber(const char* str) {RunNumberS=str;}

  void saveToFile();

  TNtuple* tecgains;
  TNtuple* tecgainsrun;
  TNtuple* tectrks;
  TFile* TecGainsFile;

private:

  int Verbose;
  int RejectOverlaps;
  int WriteHistograms;
  int RunNumber;
  const char* RunNumberS;

};
#endif /*__MTECGAINSMODULE_H__*/

