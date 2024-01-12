#include <iostream>
#include <stdexcept>
#include <sstream>

#include <TSingleParticleGenerator.h>
#include <TClonesArray.h>
#include <Hepevt.h>
#include <TMCParticle.h>
#include <TParticle.h>
#include <TLorentzVector.h>
#include <TDatabasePDG.h>
#include <TPythia6Decayer.h>
#include <TMath.h>
#include <TF1.h>

static const double CM2MM = 10.;

using namespace std;

ClassImp(TSingleParticleGenerator);              

TSingleParticleGenerator::TSingleParticleGenerator(const char* name, const char* title) :
  TGenerator(name,title),
  _forcedecay(TPythia6Decayer::kAll),
  _decayer(0)
{
  _eventNumber = 0;
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
  _PDGdb = TDatabasePDG::Instance();
  fParticles = new TClonesArray("TMCParticle",100);

  std::fill(_vtx0,_vtx0+3,0.0);
  std::fill(_vtxWidth,_vtxWidth+3,0.0);
  std::fill(_vtx,_vtx+4,0.0);
  std::fill(_p,_p+4,0.0);

  _vtxflag = TSingleParticleGenerator::FLAT;
  _momflag = TSingleParticleGenerator::FLAT_PT;
  _vtxfile.open("vertex.txt");

  // random number generator initialization
  //gsl_rng_env_setup();
  gsl_rand_type = gsl_rng_default;
  gsl_rand_gen = gsl_rng_alloc( gsl_rand_type );
}

TSingleParticleGenerator::~TSingleParticleGenerator()
{
  _vtxfile.close();
  gsl_rng_free( gsl_rand_gen );
  if ( _decayer!=0 ) delete _decayer;
}

TObjArray*
TSingleParticleGenerator::ImportParticles(Option_t*)
{
  return fParticles;
}

Double_t
TSingleParticleGenerator::GetParameter(const char *name) const
{
  if ( strcasecmp("xvtx",name) == 0 ) return _vtx[0];
  else if ( strcasecmp("yvtx",name) == 0 ) return _vtx[1];
  else if ( strcasecmp("zvtx",name) == 0 )
    {
      return _vtx[2];
    }
  else
    {
      cout << "TSingleParticleGenerator::GetParameter(), ERROR, parameter "
           << name << " is unknown, returning nan" << endl;
      return sqrt(-1);
    }
}

void
TSingleParticleGenerator::GenerateEvent()
{
  _eventNumber++;

  TParticlePDG* p = _PDGdb->GetParticle(_pid);
  _mass = p->Mass();
  _lifetime = p->Lifetime();

  //
  // Choose the parameters for the particle
  //
  GenerateVertex();
  GenerateMomentum();

  fParticles->Clear();

  TClonesArray &a = *((TClonesArray*)fParticles);

  if ( _decayer == 0 )
    {
      // single particle only, no decays
      new(a[0]) TMCParticle(1, // KS = 1 (stable particle)
			_pid , // KF flag
			0, // parent 
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
    }
  else if ( _decayer != 0 )
    {
      TLorentzVector temp_v(_p[0],_p[1],_p[2],_p[3]);
      _decayer->Decay(_pid,&temp_v);

      TClonesArray daughters("TParticle",10);
      //TClonesArray *part = &daughters;
      //part->Clear();
      Int_t nparticles = _decayer->ImportParticles(&daughters);
          
      for (int ipart=0; ipart<nparticles; ipart++)
        {
          //TParticle *pp = (TParticle*)part->At(ipart);
          TParticle *pp = (TParticle*)daughters.At(ipart);
          Int_t pid = pp->GetPdgCode();
          Float_t p[4] = {0.}; 
          p[0] = pp->Px();
          p[1] = pp->Py();
          p[2] = pp->Pz();
          p[3] = pp->Energy();
          Float_t m = pp->GetMass();
          Float_t v[3] = {0.};
          v[0] = pp->Vx() + _vtx[0]*CM2MM;
          v[1] = pp->Vy() + _vtx[1]*CM2MM;
          v[2] = pp->Vz() + _vtx[2]*CM2MM;
          Float_t t = pp->T();
          Int_t parent = pp->GetFirstMother();
          Int_t firstchild = pp->GetFirstDaughter();
          Int_t lastchild = pp->GetLastDaughter();
          Float_t ltime = pp->GetPDG()->Lifetime();	//
          Int_t ks = 1;
          if ( ipart==0 ) ks = 11;
          
          new(a[ipart]) TMCParticle(ks, // KS = 1 (stable particle)
				    pid , // KF flag
				    parent, // parent 
				    firstchild, // first child
				    lastchild, // second child
				    p[0], // px
				    p[1], // py
				    p[2], // pz
				    p[3], // e
				    m, // mass
				    v[0], // x (mm)
				    v[1], // y 
				    v[2], // z
				    t,  // time (mm/c)
				    ltime);  // lifetime
        }
      //part->Clear();
    }	// _decayer != 0

}

void
TSingleParticleGenerator::GenerateVertex()
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

void
TSingleParticleGenerator::GenerateMomentum()
{
  // Pick phi randomly from 0 to 2pi
  double phi = 2*M_PI*gsl_rng_uniform(gsl_rand_gen);

  // Eta uniform from min to max
  double eta = _etaMin + (_etaMax-_etaMin)*gsl_rng_uniform(gsl_rand_gen);

  TLorentzVector v;

  if ( _momflag == FLAT_PT ) // Pt uniform from 0 to max
    {
      double pt = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
      v.SetPtEtaPhiM(pt,eta,phi,_mass);
    }
  else if ( _momflag == FLAT_P )
    {
      double p = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
      double theta = 2.0*atan(exp(-eta));
      double pt = p*sin(theta);
      v.SetPtEtaPhiM(pt,eta,phi,_mass);
    }
  else if ( _momflag == FLAT_E )
    {
      double e = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
      while ( e < _mass ) // energy can't be below mass
        {
          e = _pMin + (_pMax-_pMin)*gsl_rng_uniform(gsl_rand_gen);
        }
      double theta = 2.0*atan(exp(-eta));
      double p = sqrt(e*e - _mass*_mass);
      double pt = p*sin(theta);
      v.SetPtEtaPhiM(pt,eta,phi,_mass);
    }
  else if ( _momflag == STEPS_ALL )
    {
      double e = log10(_pMin) + (log10(_pMax)-log10(_pMin))*gsl_rng_uniform(gsl_rand_gen);
      e=pow(10.,e);
      double deltae=(log10(_pMax)-log10(_pMin))/_nstepse;
      double eprime=log10(e)-log10(_pMin);
      double xn=eprime/deltae;
      int ixn=(int) xn;
      e=deltae*ixn+log10(_pMin);
      e=pow(10,e);

      double theta = 2.0*atan(exp(-eta));
      double deltatheta=(2.0*atan(exp(-_etaMax))-2.0*atan(exp(-_etaMin)))/_nstepstheta;
      double thetaprime=2.0*atan(exp(-eta))-2.0*atan(exp(-_etaMin));
      double xntheta=thetaprime/deltatheta;
      int ixntheta=(int) xntheta;
      theta=deltatheta*ixntheta+2.0*atan(exp(-_etaMin));
      eta=-log(tan(theta/2.));

      phi = _phitot*M_PI*gsl_rng_uniform(gsl_rand_gen); // only thow _phitot (2=2pi)
      double deltaphi=_phitot*M_PI/_nstepsphi;
      double phiprime=phi;
      double xnphi=phiprime/deltaphi;
      int ixnphi=(int) xnphi;
      phi=deltaphi*ixnphi;

      cout<<"_nstepse="<<_nstepse<<" _nstepstheta="<<_nstepstheta<<" _nstepsphi="<<_nstepsphi<<" _phitot="<<_phitot<<endl;
      cout<<"e="<<e<<" eta="<<eta<<" theta="<<theta<<" phi="<<phi<<endl;

      double p = sqrt(e*e - _mass*_mass);
      double pt = p*sin(theta);
      v.SetPtEtaPhiM(pt,eta,phi,_mass);
    }
  else if ( _momflag == EXP_PT )
    {
      TF1 ptfunc("ptfunc","x*TMath::Exp([0]*x)",_pMin, _pMax);
      TF1 etafunc("etafunc","TMath::Exp(-x*x/(2.0*[0]*[0]))",_etaMin,_etaMax);

      ptfunc.SetParameter(0,-0.6);
      etafunc.SetParameters(0,1.0);

      double pt = ptfunc.GetRandom();
      double eta = etafunc.GetRandom();

      v.SetPtEtaPhiM(pt,eta,phi,_mass);
    }

  _p[0] = v.Px();
  _p[1] = v.Py();
  _p[2] = v.Pz();
  _p[3] = v.Energy();

}

void TSingleParticleGenerator::SetParameter(const char* name, double val)
{
  std::cout << "TSingleParticleGenerator::SetParameter: set par "
	    << name << " to value " << val << std::endl;

  if ( strcasecmp("pid",name) == 0 ) _pid = (int)val;

  else if ( strcasecmp("seed",name) == 0 )
    {
      gsl_rng_set(gsl_rand_gen,(unsigned long int)val);
    }

  else if ( strcasecmp("vtxflag",name) == 0 ) _vtxflag = (int)val;

  else if ( strcasecmp("xcenter",name) == 0 )
    {
       _vtx0[0] = val;
       cout << "PHParticleGen: Note position units are in cm" << endl;
    }
  else if ( strcasecmp("xwidth",name) == 0 )
    {
      _vtxWidth[0] = val;
       cout << "PHParticleGen: Note position units are in cm" << endl;
    }
  else if ( strcasecmp("ycenter",name) == 0 )
    {
      _vtx0[1] = val;
       cout << "PHParticleGen: Note position units are in cm" << endl;
    }
  else if ( strcasecmp("ywidth",name) == 0 )
    {
      _vtxWidth[1] = val;
       cout << "PHParticleGen: Note position units are in cm" << endl;
    }
  else if ( strcasecmp("zcenter",name) == 0 )
    {
      _vtx0[2] = val;
       cout << "PHParticleGen: Note position units are in cm" << endl;
    }
  else if ( strcasecmp("zwidth",name) == 0 )
    {
      _vtxWidth[2] = val;
       cout << "PHParticleGen: Note position units are in cm" << endl;
    }
  else if ( strcasecmp("momflag",name) == 0 ) _momflag = (int)val;

  else if ( strcasecmp("etamin",name) == 0 ) _etaMin = val;
  else if ( strcasecmp("etamax",name) == 0 ) _etaMax = val;

  else if ( strcasecmp("pmin",name) == 0 ) _pMin = val;
  else if ( strcasecmp("pmax",name) == 0 ) _pMax = val;

  else if ( strcasecmp("nstepse",name) == 0 ) _nstepse = val;
  else if ( strcasecmp("nstepstheta",name) == 0 ) _nstepstheta = val;
  else if ( strcasecmp("nstepsphi",name) == 0 )
    {
      _nstepsphi = val;
      cout<<" setting "<<name<<" = "<<val<<" _nstepsphi="<<_nstepsphi<<endl;
    }
  else if ( strcasecmp("phitot",name) == 0 ) _phitot = val;

  // This option overrides the default of kAll.  Must be called BEFORE
  // SetParameter("decayfile", ... )
  else if ( strcasecmp("forcedecay",name) == 0 ) _forcedecay = (int)val;

}

void TSingleParticleGenerator::SetParameter(const char* name, const char *val)
{
  std::cout << "TSingleParticleGenerator::SetParameter: set par "
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

void TSingleParticleGenerator::setSeed(int seed)
{
  srand48(seed);
}
