#include <iostream>
#include <stdexcept>
#include <sstream>
#include <TClonesArray.h>
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif
#include <TDatabasePDG.h>

#include <THijing.h>
#include <hijingpar.h>
#include <hiparnt.h>

ClassImp(THijing);              

extern HIJINGPAR hijingpar_;
extern HIMAIN1 himain1_;
extern HIMAIN2 himain2_;
extern HIPARNT hiparnt_;

extern "C" {
  void hijing_init__();
  void hijset_(float*,char*,char*,char*,int*,int*,int*,int*,int,int,int);
  void hijing_gen__(char*,float*,float*,float*);
  void hijingseed_(int*,int*);
  void lugive_(const char*,int);
};

THijing::THijing(const char* name,const char* title) :
  PHGenerator(name,title),
  //_PDGdb(TDatabasePDG::Instance()),
  _efrm(200.0),
  _iap(197), _izp(79), _iat(197), _izt(79),
  _bmin(0.0), _bmax(20.0), _iseed(1), _iseed_skip(0),
  _jet_trigger(0), _pzero(2.0), _pthard(0.0), _pthard_max(-1.0), _jet_quench(1),
  _str_rad_flag(1), _ini_rad_flag(3),_scaleup_factor(4.0), _loss_factor(1.0),
  _pthetamin(0.0), _pthetamax(180.0), _history(0),
  _pars(hijingpar_), _hiparnt(hiparnt_), _himain1(himain1_), _himain2(himain2_)
{
  _PDGdb = new TDatabasePDG();
  fParticles = new TClonesArray("TMCParticle",10000);
}

THijing::~THijing()
{
  delete _PDGdb;
}

void
THijing::Init()
{
  // Initialize the PHENIX portion (read hijing.par, initialize random number generator)
  //
  //hijing_init__();

  // The following initialization of jet quenching, loss_factor, and jet_triggering is
  // taken verbatim from hijing_init.f.  
  //

  _hiparnt.ihpr2[3] = _jet_quench; //  ! sets jet quenching
  if ( _hiparnt.ihpr2[3] == 1 ) 
    {
      std::cout << "New HIJING 1.37: quenching is on, loss factor " << _loss_factor
		<< " times 1.0 GeV/fm" << std::endl;
      _hiparnt.hipr1[13] *= _loss_factor; 
    }

  if ( _hiparnt.ihpr2[3] == 0 ) 
    {
      std::cout << "New HIJING 1.37: quenching is off" << std::endl;
    }

  // set minimum pT transfer in hard or semihard scatterings (physics
  // pg 5-6, parameter pg 19)
  _hiparnt.hipr1[7]  = _pzero;
  _hiparnt.hipr1[8] = _pthard_max; //set the maximum hard scattering pt
  _hiparnt.ihpr2[0] = _str_rad_flag; //flag for long. string radiation
  _hiparnt.ihpr2[1] = _ini_rad_flag; //flag for initial state radiation
  _hiparnt.hipr1[50] = _scaleup_factor;

  //  std::cout << "SCALEUP FACTOR="<< _scaleup_factor<<" inpr1[50]="<< _hiparnt.hipr1[50] << std::endl;

  /*
scaleup_factor is  PARP(67) : (D = 4.) the Q2 scale of the hard scattering (see MSTP(32)) is multiplied by PARP(67) to define the maximum parton virtuality allowed in Q2-ordered spacelike
showers. (see pythia manual)
  */

  if ( _jet_trigger == 1 )
    {
      //
      // Using documentation page 31
      //
      _hiparnt.ihpr2[3] = 0; //   ! turns off jet quenching
      _hiparnt.ihpr2[2] = 1; //   ! turns on one hard scattering
      _hiparnt.hipr1[9] = _pthard; // ! hard process pt range
    }

  _hiparnt.ihpr2[20] = _history; //  ! sets history string
  if ( _hiparnt.ihpr2[20] == 1 ) 
    {
      std::cout << "HIJING history string is on: spectators are removed from the event." << std::endl; 
    }

  //
  // Initialize the random number generator
  // Using GEANT random number sequences
  //
  hijingseed_(&_iseed,&_iseed_skip);

  // Initialize the HIJING code
  //
  hijset_(&_efrm,_frame,_proj,_targ,&_iap,&_izp,&_iat,&_izt,4,4,4);
}

void
THijing::GenerateEvent()
{
  // Call HIJING
  //   bimpact is a return value
  //
  //hijing_gen__(_pars.frame,&_pars.bmin0,&_pars.bmax0,&_bimpact);
  hijing_gen__(_frame,&_bmin,&_bmax,&_bimpact);

  // get # participating nucleons information
  _nt = _himain1.nt;
  _np = _himain1.np;

  // Store the particles
  //
  fParticles->Clear();
  for(int i=0; i<_himain1.natt; i++)
    {
      int pid = _himain2.katt[0][i];	// in pythia numbering (=KF)
      TParticlePDG* p = _PDGdb->GetParticle(pid);
      float mass = p ? p->Mass() : 0.0;
      float lifetime = p ? p->Lifetime() : 0.0;
      TClonesArray &a = *((TClonesArray*)fParticles);
      
      // modified to flag directly produced and decayed 
      // particles correctly when HIJING history 
      // information is turned on
      // JGL 7/15/2010

      new(a[i]) TMCParticle(_himain2.katt[3][i], // KS 
 			    pid , // KF flag
 			    _himain2.katt[2][i], // parent 
                            0, // first child
                            0, // last child
 			    _himain2.patt[0][i], // px (GeV/c)
 			    _himain2.patt[1][i], // py
 			    _himain2.patt[2][i], // pz
 			    _himain2.patt[3][i], // e
 			    mass, // mass
 			    _himain2.vatt[0][i], // x (mm)
 			    _himain2.vatt[1][i], // y (mm)
			    _himain2.vatt[2][i], // z (mm)
			    _himain2.vatt[3][i],  // formation time (mm/c)
			    lifetime);  // lifetime (from PDG lookup)
    }

  if ( _history != 0 )
    {
      // Go through and update the first and last child
      int npart = fParticles->GetLast() + 1;

      int prev_parent = 0;
      int firstchild = -1;
      int lastchild = -1;
      for (int ipart=0; ipart<npart; ipart++)
        {
          TMCParticle *part = (TMCParticle*)fParticles->At(ipart);
          int parent = part->GetParent();
          if ( parent != prev_parent )
            {
              if ( prev_parent > 0 )	// at end of decay string
                {
                  lastchild = ipart;
                  TMCParticle *parentpart = (TMCParticle*)fParticles->At(prev_parent-1);
                  parentpart->SetFirstChild(firstchild);
                  parentpart->SetLastChild(lastchild);
                  firstchild = lastchild + 1;
                  lastchild = -1;
                }
              else
                {
                  firstchild = ipart + 1;
                }
            }
          prev_parent = parent;
        }
      // Clean up last entry
      if ( prev_parent > 0 )	// at end of decay string
        {
          lastchild = npart;
          TMCParticle *parentpart = (TMCParticle*)fParticles->At(prev_parent-1);
          parentpart->SetFirstChild(firstchild);
          parentpart->SetLastChild(lastchild);
        }
    }
}

double
THijing::GetParameter(const char* name) const
{
  if ( strcasecmp("bimpact",name) == 0 ) 
    {
      return _bimpact;
    }
  else if ( strcasecmp("iap",name) == 0 ) 
    {
      return _iap;
    }
  else if ( strcasecmp("np",name) == 0 ) 
    {
      return _np;
    }
  else if ( strcasecmp("nt",name) == 0 ) 
    {
      return _nt;
    }
  else
    {
      throw std::runtime_error("Uknown parameter in THijing::GetParameter");
    }
  return 0.0;
}

void 
THijing::SetParameter(const char* name, double val)
{
  if ( strcasecmp("iap",name) == 0 || strcasecmp("n1",name) == 0 ) 
    {
      _iap = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: iap = " << _iap << std::endl;
    }
  else if ( strcasecmp("izp",name) == 0 || strcasecmp("iz1",name) == 0 )
    {
      _izp = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: izp = " << _izp << std::endl;
    }
  else if ( strcasecmp("iat",name) == 0 || strcasecmp("n2",name) == 0 ) 
    {
      _iat = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: iat = " << _iat << std::endl;
    }
  else if ( strcasecmp("izt",name) == 0 || strcasecmp("iz2",name) == 0 ) 
    {
      _izt = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: izt = " << _izt << std::endl;
    }
  else if ( strcasecmp("elgev",name) == 0 ) 
    {
      _efrm = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: efrm = " << _efrm << std::endl;
    }
  else if ( strcasecmp("chproj",name) == 0 ) 
    {
      // Special care has to be taken for strings.  Fortran doesn't understand
      // null-terminated strings.  The rest of the string needs to be filled
      // with blanks.  NOTE: these lines assume 4 bytes length, which is what
      // is used in HIJING (of course).
      memcpy(_proj,&val,4);
      int len = strlen(_proj);
      if ( len < 4 ) memset(_proj+len,' ',4-len);
      _proj[4] = '\0';
      //std::cout << "THijing::SetParameter: proj = \"" << _proj << "\"" << std::endl;
    }
  else if ( strcasecmp("chtarg",name) == 0 ) 
    {
      memcpy(_targ,&val,4);
      int len = strlen(_targ);
      if ( len < 4 ) memset(_targ+len,' ',4-len);
      _targ[4] = '\0';
      //std::cout << "THijing::SetParameter: targ = \"" << _targ << "\"" << std::endl;
    }
  else if ( strcasecmp("ref",name) == 0 )
    {
      memcpy(_frame,&val,4);
      int len = strlen(_frame);
      if ( len < 4 ) memset(_frame+len,' ',4-len);
      _frame[4] = '\0';
      //std::cout << "THijing::SetParameter: frame = \"" << _frame << "\"" << std::endl;
    }
  else if ( strcasecmp("bmax",name) == 0 ) 
    {
      _bmax = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: bmax = " << _bmax << std::endl;
    }
  else if ( strcasecmp("bmin",name) == 0 ) 
    {
      _bmin = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: bmin = " << _bmin << std::endl;
    }
  else if ( strcasecmp("iseed",name) == 0 ) 
    {
      _iseed = static_cast<int>(val);
      std::cout << "THijing::SetParameter: iseed = " << _iseed << std::endl;
    }
  else if ( strcasecmp("iseed_skip",name) == 0 ) 
    {
      _iseed_skip = static_cast<int>(val);
      std::cout << "THijing::SetParameter: iseed_skip = " << _iseed_skip << std::endl;
    }
  else if ( strcasecmp("jet_trigger",name) == 0 ) 
    {
      _jet_trigger = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: jet_trigger = " << _jet_trigger << std::endl;
    }
  else if ( strcasecmp("pthard",name) == 0 ) 
    {
      _pthard = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: pthard = " << _pthard << std::endl;
    }
  else if ( strcasecmp("pzero",name) == 0 ) 
    {
      _pzero = static_cast<float>(val);
      std::cout << "THijing::SetParameter: pzero = " << _pzero << std::endl;
    }
  else if ( strcasecmp("pthard_max",name) == 0 ) 
    {
      _pthard_max = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: pthard_max = " << _pthard_max << std::endl;
    }
  else if ( strcasecmp("str_rad_flag",name) == 0 ) 
    {
      _str_rad_flag = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: str_rad_flag = " << _str_rad_flag << std::endl;
    }
  else if ( strcasecmp("scaleup_factor",name) == 0 ) 
    {
      _scaleup_factor = static_cast<float>(val);
      //      std::cout << "THijing::SetParameter: scaleup_factor = " << _scaleup_factor << std::endl;
    }
  else if ( strcasecmp("ini_rad_flag",name) == 0 ) 
    {
      _ini_rad_flag = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: ini_rad_flag = " << _ini_rad_flag << std::endl;
    }
  else if ( strcasecmp("jet_quench",name) == 0 ) 
    {
      _jet_quench = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: jet_quench = " << _jet_quench << std::endl;
    }
  else if ( strcasecmp("loss_factor",name) == 0 ) 
    {
      _loss_factor = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: loss_factor = " << _loss_factor << std::endl;
    }
  else if ( strcasecmp("pthetamin",name) == 0 ) 
    {
      _pthetamin = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: pthetamin = " << _pthetamin << std::endl;
    }
  else if ( strcasecmp("pthetamax",name) == 0 ) 
    {
      _pthetamax = static_cast<float>(val);
      //std::cout << "THijing::SetParameter: pthetamax = " << _pthetamax << std::endl;
    }
  else if ( strcasecmp("history",name) == 0 ) 
    {
      _history = static_cast<int>(val);
      //std::cout << "THijing::SetParameter: history = " << _history << std::endl;
    }
  else if ( strcasecmp("seed",name) == 0 ) 
    {
      // Currently ignored
      std::cout << "THijing::SetParameter: seed = IGNORED" << std::endl;
    }
  else
    {
      throw std::runtime_error("Unknown parameter in THijing::SetParameter");
    }
}

void
THijing::SetDecay(int pid, bool flag)
{
  std::ostringstream s;
  s << "MDCY(C" << pid << ",1)="<< (int)flag;
  int len = s.str().size();
  std::cout << "THijing::SetDecay: " << s.str() << std::endl;
  lugive_(s.str().c_str(),len);
}
