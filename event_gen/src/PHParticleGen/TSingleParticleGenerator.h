#ifndef __TSINGLEPARTICLEGENERATOR_H__
#define __TSINGLEPARTICLEGENERATOR_H__

#ifndef __CINT__
#include <gsl/gsl_rng.h>
#include <gsl/gsl_randist.h>
#endif

#include <TGenerator.h>
#include <string>
#include <fstream>

class TDatabasePDG;
class TPythia6Decayer;

class TSingleParticleGenerator : public TGenerator {

public:

  TSingleParticleGenerator(const char* name = "TSingleParticleGenerator",
			   const char* title = "TSingleParticleGenerator");

  virtual ~TSingleParticleGenerator();

  virtual void GenerateEvent();
  void SetParameter(const char* name, double val);
  void SetParameter(const char* name, const char *val);

  virtual Double_t GetParameter(const char* name) const;
  
  TObjArray* ImportParticles(Option_t*);

  void setSeed(int seed); 

  void setDecayer(TPythia6Decayer *d) {
    _decayer = d;
  }

  int getEventNumber() const { return _eventNumber; }

  enum { FLAT_PT, FLAT_P, FLAT_E, STEPS_ALL, EXP_PT };
  // type of momentum dist.
  enum { FLAT, GAUSSIAN };
  // type of vertex dist.

protected:

  virtual void GenerateVertex();
  virtual void GenerateMomentum();

  TDatabasePDG* _PDGdb;

  int _eventNumber;

  std::ifstream _vtxfile;
  int _vtxflag;	// type of vertex to generate
  double _vtx0[3];		// center (cm)
  double _vtxWidth[3];		// width  (cm)

  double _etaMin;
  double _etaMax;

  int _momflag;	// type of momentum to generate
  double _pMin;	// note: p can be momentum, pt, e, ...
  double _pMax;

  double  _nstepse;        // for STEPS_ALL # steps in Log E
  double  _nstepstheta;     // # steps in theta 
  double  _nstepsphi;      // # steps in phi
  double _phitot;          // coverage in phi (units of pi)
  int _forcedecay; // flag for which type of forced decay (cf TPythia6Decayer documentation)

  // decay table, for decay generator
  std::string _decayfile;
  TPythia6Decayer *_decayer;

  // Particle properties
  int _pid;
  double _mass;		// should be coupled to PID
  double _lifetime;	// should be coupled to PID
  double _vtx[4];	// particle vertex (cm), (x,y,z,t)
                        // NB: this is recorded as mm in phpythia
  double _p[4];		// particle 4-momentum, (px,py,pz,E)

  // GSL random number generator
#ifndef __CINT__
  const gsl_rng_type *gsl_rand_type;
  gsl_rng *gsl_rand_gen;
#endif

  ClassDef(TSingleParticleGenerator,0);              
};

#endif // __TSINGLEPARTICLEGENERATOR_H__

