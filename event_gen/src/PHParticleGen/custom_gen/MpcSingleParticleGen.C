#include <iostream>

#include <MpcSingleParticleGen.h>
#include <TClonesArray.h>
//#include <Hepevt.h>
#include <TMCParticle.h>
#include <TLorentzVector.h>
#include <TDatabasePDG.h>

static const float MM2CM = 0.1;	// conversion of mm to cm

using namespace std;

ClassImp(MpcSingleParticleGen);              

MpcSingleParticleGen::MpcSingleParticleGen(const char* name, const char* title) :
  TSingleParticleGenerator(name,title)
{
  _arm = 0;	// South
}

MpcSingleParticleGen::~MpcSingleParticleGen()
{
}

void MpcSingleParticleGen::GenerateMomentum()
{
  // for events with no vertex, we create a pi0 at rest, creates the least trouble
  if ( fabs(_vtx[2])>300. )
    {
      _p[0] = 0.;
      _p[1] = 0.;
      _p[2] = 0.;
      _p[3] = _mass;

      return;
    }

  double R_MIN = 6.0;		// minimum angle
  //const double R_MAX = 24.0;	// minimum angle
  double R_MAX = 24.0;	// minimum angle
  double MPC_Z = 220.947;
  if ( _arm == 0 )
    {
      R_MIN = 8.0;
      MPC_Z = -MPC_Z;
    }

  // Clearly good part of MPC
  // Maybe this needs to be added to Options
  //R_MIN = 13.0;
  //R_MAX = 17.0;

  double e = 0.;
  double pt = 0.;

  if ( _momflag == FLAT_E )
    {
      e = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
    }
  else if ( _momflag == FLAT_PT )
    {
      pt = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
    }

  // Pick a random x,y, make sure it is within the mpc annulus 
  // also check that it doesn't violate a maximum energy limit
  double x = R_MAX*(2.0*gsl_rng_uniform(gsl_rand_gen)-1.0);
  double y = R_MAX*(2.0*gsl_rng_uniform(gsl_rand_gen)-1.0);
  double z = MPC_Z - _vtx[2];
  double rsqr = x*x + y*y;

  int count = 0;
  while ( rsqr<R_MIN*R_MIN || rsqr>R_MAX*R_MAX )
    {
      x = R_MAX*(2.0*gsl_rng_uniform(gsl_rand_gen)-1.0);
      y = R_MAX*(2.0*gsl_rng_uniform(gsl_rand_gen)-1.0);
      rsqr = x*x + y*y;

      // force a re-throw if the momentum is too high
      if ( _momflag == FLAT_PT )
        {
          double p = pt*sqrt(rsqr+z*z)/sqrt(rsqr);
          if ( p > 80. )
            {
              pt = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
              rsqr = 0.;
            }
        }

      if ( count++>100000 )
        {
          // okay, give up
          cout << "ERROR, failed to find a valid throw, " << _vtx[2] << endl;
          _p[0] = 0.;
          _p[1] = 0.;
          _p[2] = 0.;
          _p[3] = _mass;
          return;
        }
    }

  double r = sqrt(rsqr);
  double phi = atan2(y,x);
  double theta = atan2(r,z);
  double eta = -log(tan(0.5*theta));

  if ( _momflag == FLAT_E )
    {
      double p = sqrt(e*e - _mass*_mass);
      pt = p*sin(theta);
    }

  TLorentzVector v;
  v.SetPtEtaPhiM(pt,eta,phi,_mass);

  _p[0] = v.Px();	// px
  _p[1] = v.Py();	// py
  _p[2] = v.Pz();	// pz
  _p[3] = v.E();

}

