#ifndef __MPCSINGLEPARTICLEGEN_H__
#define __MPCSINGLEPARTICLEGEN_H__

#include <TSingleParticleGenerator.h>
#include <fstream>

class TDatabasePDG;

/*

Generate Particles in MPC Acceptance

*/
class MpcSingleParticleGen : public TSingleParticleGenerator {

public:

  MpcSingleParticleGen(const char* name = "MpcSingleParticleGen",
                       const char* title = "MpcSingleParticleGen");
  virtual ~MpcSingleParticleGen();

  virtual void SetArm(int a) { _arm = a; }

protected:

  //virtual void GenerateVertex();
  virtual void GenerateMomentum();

  int _arm;

  ClassDef(MpcSingleParticleGen,0);              
};

#endif // __MPCSINGLEPARTICLEGEN_H__

