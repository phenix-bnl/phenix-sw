#ifndef __TSINGLEPARTICLECUSTOMGENERATOR_H__
#define __TSINGLEPARTICLECUSTOMGENERATOR_H__

#include <TSingleParticleGenerator.h>

class TDatabasePDG;

/*

Example code for generating one's own custom vertex and momentum distributions

*/
class TSingleParticleCustomGenerator : public TSingleParticleGenerator {

public:

  TSingleParticleCustomGenerator(const char* name = "TSingleParticleCustomGenerator",
			   const char* title = "TSingleParticleCustomGenerator");
  virtual ~TSingleParticleCustomGenerator() {};

  //void GenerateEvent();

protected:

  virtual void GenerateVertex();
  virtual void GenerateMomentum();

  ClassDef(TSingleParticleCustomGenerator,0);              
};

#endif // __TSINGLEPARTICLECUSTOMGENERATOR_H__

