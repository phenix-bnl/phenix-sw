#ifndef __TSINGLEDYGENERATOR_H__
#define __TSINGLEDYGENERATOR_H__

#include <TSingleParticleGenerator.h>

class TDatabasePDG;
class TH2D;
class TFile;

class TSingleDYGenerator : public TSingleParticleGenerator {

public:

  TSingleDYGenerator(const char* name = "TSingleDYGenerator",
                     const char* title = "TSingleDYGenerator",
                     const char* dy_fname = "vrap_dy_nnlo.root");
  virtual ~TSingleDYGenerator();

  //void GenerateEvent();

protected:

  virtual void GenerateMomentum();

private:

  TFile *dy_tfile;
  TH2D *h2_dy;

  ClassDef(TSingleDYGenerator,0);              
};

#endif // __TSINGLEDYGENERATOR_H__

