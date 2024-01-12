#include <iostream>
#include <stdexcept>
#include <sstream>

#include "TStarlightGenerator.h"
#include <TClonesArray.h>
//#include <Hepevt.h>
#include <TMCParticle.h>
#include <TParticle.h>
#include <TLorentzVector.h>
#include <TDatabasePDG.h>
#include <TPythia6Decayer.h>
#include <TMath.h>
#include <TF1.h>

static const double CM2MM = 10.;

using namespace std;

ClassImp(TStarlightGenerator);              

TStarlightGenerator::TStarlightGenerator(const char* name, const char* title) :
  TSingleParticleGenerator(name,title),
  _forcedecay(TPythia6Decayer::kAll),
  _decayer(0)
{
  _eventNumber = 0;
  /*
  _etaMin = -3.0;
  _etaMax = 3.0;
  _pMin = 0.0;
  _pMax = 1.0;
  _pid = 0;
  _mass = 0.0;
  _lifetime = 0.0;
  _nstepse = 20.;
  _nstepstheta = 20.;
  _nstepsphi = 20.;
  _phitot = 2.0;
  */
  _PDGdb = TDatabasePDG::Instance();
  fParticles = new TClonesArray("TMCParticle",100);

  std::fill(_vtx0,_vtx0+3,0.0);
  std::fill(_vtxWidth,_vtxWidth+3,0.0);
  std::fill(_vtx,_vtx+4,0.0);
  //std::fill(_p,_p+4,0.0);

  _vtxflag = TStarlightGenerator::FLAT;
  //_momflag = TStarlightGenerator::FLAT_PT;
  _vtxfile.open("vertex.txt");

  // random number generator initialization
  //gsl_rng_env_setup();
  gsl_rand_type = gsl_rng_default;
  gsl_rand_gen = gsl_rng_alloc( gsl_rand_type );
}

TStarlightGenerator::~TStarlightGenerator()
{
  _vtxfile.close();
  gsl_rng_free( gsl_rand_gen );
  if ( _decayer!=0 ) delete _decayer;
}

TObjArray* TStarlightGenerator::ImportParticles(Option_t*)
{
  return fParticles;
}

Double_t TStarlightGenerator::GetParameter(const char *name) const
{
  if ( strcasecmp("xvtx",name) == 0 ) return _vtx[0];
  else if ( strcasecmp("yvtx",name) == 0 ) return _vtx[1];
  else if ( strcasecmp("zvtx",name) == 0 )
    {
      return _vtx[2];
    }
  else
    {
      cout << "TStarlightGenerator::GetParameter(), ERROR, parameter "
           << name << " is unknown, returning nan" << endl;
      return sqrt(-1);
    }
}

int TStarlightGenerator::OpenStarlightFile(const char *fname)
{
  _starlightfile.open(fname);
  if ( _starlightfile.is_open() )
    {
      return 1;
    }

  return 0;
}

void TStarlightGenerator::GenerateEvent()
{
  _eventNumber++;


  //
  // Choose the parameters for the particle
  //
  GenerateVertex();

  fParticles->Clear();

  TClonesArray &a = *((TClonesArray*)fParticles);

  std::string label;
  int j1, j2, j3;
  int v1, v2, v3, v4, v5, v6, v7, v8;
  int t1, t2, t3, t4;

  if (_starlightfile.eof())                      // check for EOF
  {
    return;
  }

  // We assume the starlight output has only two tracks!
  _starlightfile >> label >> j1 >> j2 >> j3;
  _starlightfile >> label >> v1 >> v2 >> v3 >> v4 >> v5 >> v6 >> v7 >> v8;

  // Particle 1
  _starlightfile >> label >> t1 >> _p[0] >> _p[1] >> _p[2] >> t2 >> t3 >> t4 >> _pid;
  TParticlePDG* p = _PDGdb->GetParticle(_pid);
  _mass = p->Mass();
  _lifetime = p->Lifetime();
  _p[3] = sqrt(_mass*_mass + _p[0]*_p[0] + _p[1]*_p[1] + _p[2]*_p[2]);

  TMCParticle part1(1, // KS = 1 (stable particle)
      _pid , // KF flag
      1, // parent 
      0, // first child
      0, // second child
      _p[0], // px
      _p[1], // py
      _p[2], // pz
      _p[3], // e
      _mass, // mass
      _vtx[0]*CM2MM, // x (mm)
      _vtx[1]*CM2MM, // y 
      _vtx[2]*CM2MM, // z
      0.0,  // time (mm/c)
      _lifetime);  // lifetime

  // Particle 2
  _starlightfile >> label >> t1 >> _p[0] >> _p[1] >> _p[2] >> t2 >> t3 >> t4 >> _pid;
  p = _PDGdb->GetParticle(_pid);
  _mass = p->Mass();
  _lifetime = p->Lifetime();
  _p[3] = sqrt(_mass*_mass + _p[0]*_p[0] + _p[1]*_p[1] + _p[2]*_p[2]);

  TMCParticle part2(1, // KS = 1 (stable particle)
      _pid , // KF flag
      1, // parent 
      0, // first child
      0, // second child
      _p[0], // px
      _p[1], // py
      _p[2], // pz
      _p[3], // e
      _mass, // mass
      _vtx[0]*CM2MM, // x (mm)
      _vtx[1]*CM2MM, // y 
      _vtx[2]*CM2MM, // z
      0.0,  // time (mm/c)
      _lifetime);  // lifetime

  // Now generate the parent (typically a J/Psi or virtual color dipole, etc)
  TLorentzVector vect1(part1.GetPx(),part1.GetPy(),part1.GetPz(),part1.GetEnergy());
  TLorentzVector vect2(part2.GetPx(),part2.GetPy(),part2.GetPz(),part2.GetEnergy());
  TLorentzVector vectp = vect1 + vect2;
 
  // Store into fParticles 
  new(a[0]) TMCParticle(11, // KS = 11 (unstable particle)
      0, // KF flag - make this undefined for the parent!
      0, // parent
      2, // first child
      3, // second child
      vectp.Px(), // px
      vectp.Py(), // px
      vectp.Pz(), // px
      vectp.Energy(), // px
      vectp.M(), // mass
      _vtx[0]*CM2MM, // x (mm)
      _vtx[1]*CM2MM, // y 
      _vtx[2]*CM2MM, // z
      0.0,  // time (mm/c)
      0.0);  // lifetime


  new(a[1]) TMCParticle(part1);
  new(a[2]) TMCParticle(part2);
}

void TStarlightGenerator::GenerateVertex()
{
  // Take vertex.txt first if it exists
  if ( _vtxfile.is_open() )
    {
      int evt = 0;
      float zvtx = 0.;
      int junk1, junk2;
      // vtxfile is in cm
      _vtxfile >> evt >> zvtx >> junk1 >> junk2;
      _vtx[2] = zvtx;
    }
  else
    {
      for (int i=0; i<3; i++)
        {
          _vtx[i] = _vtx0[i];	// set center

          if ( _vtxWidth[i] == 0. ) continue;
    
          // Get distribution around center
          double val = 0.;
          if ( _vtxflag == FLAT )
            {
              val = _vtxWidth[i]*(2.0*gsl_rng_uniform(gsl_rand_gen) - 1.0);
            }
          else if ( _vtxflag == GAUSSIAN )
            {
              val = gsl_ran_gaussian(gsl_rand_gen,_vtxWidth[i]);
            }
    
          _vtx[i] += val;
        }
    }
}

/*
void TStarlightGenerator::SetParameter(const char* name, const char *val)
{
  std::cout << "TStarlightGenerator::SetParameter: set par "
            << name << " to value " << val << std::endl;

  if ( strcasecmp("decayfile",name) == 0 )
    {
      _decayfile = std::string(val);
      if ( _decayer == 0 )
        {
          _decayer = TPythia6Decayer::Instance();
        }

      //_decayer->SetForceDecay(TPythia6Decayer::kAll);
      std::cout << "Setting force decay to " << _forcedecay << std::endl;
      _decayer->SetForceDecay(_forcedecay);
      _decayer->Init();
      
      if ( _decayfile != "default" )
        {
          if ( access(_decayfile.c_str(),R_OK) )
            {
              std::ostringstream s;
              s << "WARNING: Unable to access file " << _decayfile << ": " << strerror(errno);
              std::cout << s.str() << std::endl;
            }
          else
            {
              std::cout << "Reading in " << _decayfile << std::endl;
              _decayer->SetDecayTableFile(_decayfile.c_str());
              //_decayer->WriteDecayTable();
              _decayer->ReadDecayTable();
            }
        }
    }
}
*/

void TStarlightGenerator::setSeed(int seed)
{
  srand48(seed);
}

