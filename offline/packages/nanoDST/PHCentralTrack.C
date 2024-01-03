#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include <phool.h>
#include <TClonesArray.h>
#include <DepObj.h>
#include <DepObjv1.h>
#include <DepObjv2.h>
#include <RunNumberRanges.h>
#include <recoConsts.h>
#include <cassert>
#include <iostream>

ClassImp(PHCentralTrack)

using namespace std;

static int shutup = 0;

void
PHCentralTrack::Reset()
{
  cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
  return ;
}

int
PHCentralTrack::isValid() const
{
  if (get_npart() > 0)
    {
      return 1;
    }
  return 0;
}

void
PHCentralTrack::identify(ostream &os) const
{
  os << "identify yourself: virtual PHCentralTrack object" << endl;
  return ;
}

void
PHCentralTrack::warning(const char *field) const
{
  if (!shutup)
    {
      cout << PHWHERE << "using virtual function, doing nothing" << endl;
      cout << "Offending field == " << field << endl;
    }
  return ;
}

void
PHCentralTrack::ShutUp(const int i)
{
  shutup = i;
  PHParticle::ShutUp(i);
}

void
PHCentralTrack::Compress()
{
  GetTCArray()->Compress();
}

PHCentralTrack*
PHCentralTrack::clone() const
{
  cout << "Clone method not implemented for your version of CentralTracks" << endl;
  return NULL;
}

void 
PHCentralTrack::AddPHParticle(unsigned int ipart, TObject *o)
{
  PHSnglCentralTrack *sct = AddPHParticle(ipart, *(static_cast<PHSnglCentralTrack *>(o)));
  assert(sct!=0);
}

PHSnglCentralTrack* 
PHCentralTrack::AddPHParticle(const unsigned int /*itrk*/,
				const PHSnglCentralTrack &track)
{
  return AddPHParticle(track);
}

PHSnglCentralTrack* 
PHCentralTrack::AddPHParticle(const PHSnglCentralTrack &track)
{
  // First check if TC array exists (it does not for the base class
  if (!GetCentral())
    {
      return NULL;
    }
  // this is a bit ugly but GetLast returns the  index-1, so the argument
  // for the ExpandCreate is  GetLast() + 2
  int nnew;
    nnew = GetCentral()->GetLast() + 2;
  // this is a TCArray method, it creates a new Object of
  // the type which is stored in the TCArray. It uses the default ctor
  GetCentral()->ExpandCreate(nnew);
  PHSnglCentralTrack *newtrack = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(GetCentral()->GetLast()));
  // Since the ExpandCreate calls the default ctor we still need to copy
  // the actual values from the input particle
  newtrack->Copy(track);
  set_npart(nnew); // update track counter
  return newtrack;
}

void
PHCentralTrack::RemovePHParticle(const unsigned int itrk)
{
  if (!GetCentral())
    {
      return;
    }
  GetCentral()->RemoveAt(itrk);
  return;
}

TClonesArray *
PHCentralTrack::GetCentral() const
{
  warning("GetCentral() no TCArray");
  return NULL;
}

TObject* 
PHCentralTrack::GetSingleParticle(unsigned int ipart)
{
  return get_track(ipart);
}


PHSnglCentralTrack* 
PHCentralTrack::get_track (const unsigned int itrk) const 
{
  if (!GetCentral())
    {
      cout << "No Central TClonesArray, calling identify to show you to whom you are talking:" << endl;
      identify();
      return NULL;
    }
  PHSnglCentralTrack *Particle = (PHSnglCentralTrack *) GetCentral()->UncheckedAt(itrk);
  return Particle;
}

// Here come the PHParticle methods (the PHParticle idea was not thought out till
// the end and to put them here is an ugly kludge

unsigned int
PHCentralTrack::get_npart() const
{
      warning("get_npart(const unsigned int itrk) const");
      return 0;
}

void
PHCentralTrack::set_npart(const unsigned int /*val*/)
{
      warning("set_npart(const unsigned int val) const");
      return;
}

float
PHCentralTrack::get_px(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_px(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_px() : NAN);
}

float
PHCentralTrack::get_Px(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_Px(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_Px() : NAN);
}

void
PHCentralTrack::set_px(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_px(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_px(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

void
PHCentralTrack::set_Px(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_Px(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_Px(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
           << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_py(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_py(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_py() : NAN);
}

float
PHCentralTrack::get_Py(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_Py(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_Py() : NAN);
}

void
PHCentralTrack::set_py(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_py(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_py(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

void
PHCentralTrack::set_Py(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_Py(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_Py(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
           << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pz(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pz() : NAN);
}

float
PHCentralTrack::get_Pz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_Pz(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_Pz() : NAN);
}

void
PHCentralTrack::set_pz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

void
PHCentralTrack::set_Pz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_Pz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_Pz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
           << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_E(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_E(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_E() : NAN);
}

void
PHCentralTrack::set_E(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_E(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_E(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_charge(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_charge(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_charge() : -999);
}

void
PHCentralTrack::set_charge(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_charge(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_charge(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_PID(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_PID(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_PID() : -999);
}

void
PHCentralTrack::set_PID(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_PID(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_PID(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

// Here comes the rest of the gazillion CentralTrack methods (sad sad sad...)


short
PHCentralTrack::get_quality(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_quality(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_quality() : -999);
}

void
PHCentralTrack::set_quality(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_quality(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_quality(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}


float
PHCentralTrack::get_zed(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_zed(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_zed() : -999);
}

void
PHCentralTrack::set_zed(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_zed(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_zed(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_phi() : -999);
}

void
PHCentralTrack::set_phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_phi(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_alpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_alpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_alpha() : -999);
}

void
PHCentralTrack::set_alpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_alpha(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_alpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_beta(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_beta(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_beta() : -999);
}

void
PHCentralTrack::set_beta(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_beta(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_beta(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_phi0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_phi0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_phi0() : -999);
}

void
PHCentralTrack::set_phi0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_phi0(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_phi0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_the0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_the0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_the0() : -999);
}

void
PHCentralTrack::set_the0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_the0(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_the0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mom(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mom(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mom() : -999);
}

void
PHCentralTrack::set_mom(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mom(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mom(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mompx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mompx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mompx() : -999);
}

void
PHCentralTrack::set_mompx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mompx(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mompx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mompy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mompy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mompy() : -999);
}

void
PHCentralTrack::set_mompy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mompy(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mompy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mompz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mompz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mompz() : -999);
}

void
PHCentralTrack::set_mompz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mompz(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mompz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_status(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_status(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_status() : -999);
}

void
PHCentralTrack::set_status(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_status(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_status(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_alpha1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_alpha1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_alpha1() : -999);
}

void
PHCentralTrack::set_alpha1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_alpha1(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_alpha1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_alpha2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_alpha2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_alpha2() : -999);
}

void
PHCentralTrack::set_alpha2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_alpha2(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_alpha2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_nx1hits(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_nx1hits(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_nx1hits() : -999);
}

void
PHCentralTrack::set_nx1hits(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_nx1hits(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_nx1hits(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_nx2hits(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_nx2hits(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_nx2hits() : -999);
}

void
PHCentralTrack::set_nx2hits(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_nx2hits(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_nx2hits(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mx1dist(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mx1dist(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mx1dist() : -999);
}

void
PHCentralTrack::set_mx1dist(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mx1dist(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mx1dist(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mx2dist(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mx2dist(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mx2dist() : -999);
}

void
PHCentralTrack::set_mx2dist(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mx2dist(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mx2dist(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_chi2x1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_chi2x1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_chi2x1() : -999);
}

void
PHCentralTrack::set_chi2x1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_chi2x1(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_chi2x1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_chi2x2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_chi2x2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_chi2x2() : -999);
}

void
PHCentralTrack::set_chi2x2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_chi2x2(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_chi2x2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_nx1x2fit(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_nx1x2fit(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_nx1x2fit() : -999);
}

void
PHCentralTrack::set_nx1x2fit(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_nx1x2fit(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_nx1x2fit(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mchi2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mchi2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mchi2() : -999);
}

void
PHCentralTrack::set_mchi2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mchi2(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mchi2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_error(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_error(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_error() : -999);
}

void
PHCentralTrack::set_error(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_error(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_error(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_alphaf(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_alphaf(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_alphaf() : -999);
}

void
PHCentralTrack::set_alphaf(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_alphaf(const unsigned int itrk, const float val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_alphaf(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pc1id(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc1id(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc1id() : -999);
}

void
PHCentralTrack::set_pc1id(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pc1id(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc1id(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}



short
PHCentralTrack::get_pc2id(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2id(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2id() : -999);
}

void
PHCentralTrack::set_pc2id(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pc2id(const unsigned int itrk, const short val) const");
      return;
    }

  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2id(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pc3id(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3id(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3id() : -999);
}

void
PHCentralTrack::set_pc3id(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pc3id(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3id(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_emcid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcid() : -999);
}

void
PHCentralTrack::set_emcid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_emcid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tofid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofid() : -999);
}

void
PHCentralTrack::set_tofid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tofid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tofwid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwid() : -999);
}

void
PHCentralTrack::set_tofwid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tofwid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tecid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecid() : -999);
}

void
PHCentralTrack::set_tecid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tecid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_mrpcid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mrpcid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mrpcid() : -999);
}

void
PHCentralTrack::set_mrpcid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_mrpcid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mrpcid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_spc2id(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2id(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2id() : -999);
}

void
PHCentralTrack::set_spc2id(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_spc2id(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2id(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}


short
PHCentralTrack::get_spc3id(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3id(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3id() : -999);
}

void
PHCentralTrack::set_spc3id(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_spc3id(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3id(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_semcid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcid() : -999);
}

void
PHCentralTrack::set_semcid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_semcid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_stofid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofid() : -999);
}

void
PHCentralTrack::set_stofid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_stofid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_stecid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecid() : -999);
}

void
PHCentralTrack::set_stecid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_stecid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc1x(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc1x(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc1x() : -999);
}

void
PHCentralTrack::set_ppc1x(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc1x(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc1x(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc1y(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc1y(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc1y() : -999);
}

void
PHCentralTrack::set_ppc1y(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc1y(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc1y(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc1z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc1z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc1z() : -999);
}

void
PHCentralTrack::set_ppc1z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc1z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc1z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc2x(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc2x(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc2x() : -999);
}

void
PHCentralTrack::set_ppc2x(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc2x(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc2x(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc2y(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc2y(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc2y() : -999);
}

void
PHCentralTrack::set_ppc2y(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc2y(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc2y(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc2z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc2z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc2z() : -999);
}

void
PHCentralTrack::set_ppc2z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc2z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc2z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptecx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptecx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptecx() : -999);
}

void
PHCentralTrack::set_ptecx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptecx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptecx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptecy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptecy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptecy() : -999);
}

void
PHCentralTrack::set_ptecy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptecy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptecy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptecz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptecz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptecz() : -999);
}

void
PHCentralTrack::set_ptecz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptecz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptecz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc3x(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc3x(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc3x() : -999);
}

void
PHCentralTrack::set_ppc3x(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc3x(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc3x(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc3y(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc3y(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc3y() : -999);
}

void
PHCentralTrack::set_ppc3y(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc3y(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc3y(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppc3z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppc3z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppc3z() : -999);
}

void
PHCentralTrack::set_ppc3z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppc3z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppc3z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pemcx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pemcx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pemcx() : -999);
}

void
PHCentralTrack::set_pemcx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pemcx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pemcx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pemcy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pemcy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pemcy() : -999);
}

void
PHCentralTrack::set_pemcy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pemcy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pemcy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pemcz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pemcz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pemcz() : -999);
}

void
PHCentralTrack::set_pemcz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pemcz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pemcz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptofx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptofx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptofx() : -999);
}

void
PHCentralTrack::set_ptofx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptofx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptofx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptofy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptofy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptofy() : -999);
}

void
PHCentralTrack::set_ptofy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptofy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptofy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptofz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptofz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptofz() : -999);
}

void
PHCentralTrack::set_ptofz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptofz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptofz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_phbdx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_phbdx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_phbdx() : -999);
}

void
PHCentralTrack::set_phbdx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_phbdx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_phbdx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_phbdy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_phbdy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_phbdy() : -999);
}

void
PHCentralTrack::set_phbdy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_phbdy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_phbdy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_phbdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_phbdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_phbdz() : -999);
}

void
PHCentralTrack::set_phbdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_phbdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_phbdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pmrpcx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pmrpcx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pmrpcx() : -999);
}

void
PHCentralTrack::set_pmrpcx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pmrpcx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pmrpcx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pmrpcy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pmrpcy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pmrpcy() : -999);
}

void
PHCentralTrack::set_pmrpcy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pmrpcy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pmrpcy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pmrpcz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pmrpcz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pmrpcz() : -999);
}

void
PHCentralTrack::set_pmrpcz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pmrpcz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pmrpcz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pltof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pltof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pltof() : -999);
}

void
PHCentralTrack::set_pltof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pltof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pltof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_plemc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_plemc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_plemc() : -999);
}

void
PHCentralTrack::set_plemc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_plemc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_plemc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_plmrpc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_plmrpc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_plmrpc() : -999);
}

void
PHCentralTrack::set_plmrpc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_plmrpc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_plmrpc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2dphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2dphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2dphi() : -999);
}

void
PHCentralTrack::set_pc2dphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2dphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2dphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2dz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2dz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2dz() : -999);
}

void
PHCentralTrack::set_pc2dz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2dz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2dz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecdphi() : -999);
}

void
PHCentralTrack::set_tecdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecdalpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecdalpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecdalpha() : -999);
}

void
PHCentralTrack::set_tecdalpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecdalpha(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecdalpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3dphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3dphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3dphi() : -999);
}

void
PHCentralTrack::set_pc3dphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3dphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3dphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3dz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3dz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3dz() : -999);
}

void
PHCentralTrack::set_pc3dz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3dz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3dz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcdphi() : -999);
}

void
PHCentralTrack::set_emcdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcdz() : -999);
}

void
PHCentralTrack::set_emcdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofdphi() : -999);
}

void
PHCentralTrack::set_tofdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofdz() : -999);
}

void
PHCentralTrack::set_tofdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mrpcdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mrpcdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mrpcdphi() : -999);
}

void
PHCentralTrack::set_mrpcdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mrpcdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mrpcdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_mrpcdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mrpcdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mrpcdz() : -999);
}

void
PHCentralTrack::set_mrpcdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_mrpcdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mrpcdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc2dphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2dphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2dphi() : -999);
}

void
PHCentralTrack::set_spc2dphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc2dphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2dphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc2dz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2dz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2dz() : -999);
}

void
PHCentralTrack::set_spc2dz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc2dz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2dz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc3dphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3dphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3dphi() : -999);
}

void
PHCentralTrack::set_spc3dphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc3dphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3dphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc3dz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3dz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3dz() : -999);
}

void
PHCentralTrack::set_spc3dz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc3dz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3dz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcdphi() : -999);
}

void
PHCentralTrack::set_semcdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcdz() : -999);
}

void
PHCentralTrack::set_semcdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofdphi() : -999);
}

void
PHCentralTrack::set_stofdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofdz() : -999);
}

void
PHCentralTrack::set_stofdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofwdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofwdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofwdphi() : -999);
}

void
PHCentralTrack::set_stofwdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofwdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofwdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofwdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofwdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofwdz() : -999);
}

void
PHCentralTrack::set_stofwdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofwdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofwdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stecdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecdphi() : -999);
}

void
PHCentralTrack::set_stecdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stecdalpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecdalpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecdalpha() : -999);
}

void
PHCentralTrack::set_stecdalpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecdalpha(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecdalpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_hbdid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdid() : -999);
}

void
PHCentralTrack::set_hbdid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_hbdid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_hbdsector(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdsector(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdsector() : -999);
}

void
PHCentralTrack::set_hbdsector(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_hbdsector(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdsector(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_hbdsize(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdsize(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdsize() : -999);
}

void
PHCentralTrack::set_hbdsize(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_hbdsize(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdsize(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdcharge(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdcharge(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdcharge() : -999);
}

void
PHCentralTrack::set_hbdcharge(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdcharge(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdcharge(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdx() : -999);
}

void
PHCentralTrack::set_hbdx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdy() : -999);
}

void
PHCentralTrack::set_hbdy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdz() : -999);
}

void
PHCentralTrack::set_hbdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbddphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbddphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbddphi() : -999);
}

void
PHCentralTrack::set_hbddphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbddphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbddphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbddz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbddz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbddz() : -999);
}

void
PHCentralTrack::set_hbddz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbddz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbddz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdsdphi() : -999);
}

void
PHCentralTrack::set_hbdsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdsdz() : -999);
}

void
PHCentralTrack::set_hbdsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_arm(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_arm(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_arm() : -999);
}

void
PHCentralTrack::set_arm(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_arm(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_arm(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sect() : -999);
}

void
PHCentralTrack::set_sect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_ysect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ysect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ysect() : -999);
}

void
PHCentralTrack::set_ysect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_ysect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ysect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_zsect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_zsect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_zsect() : -999);
}

void
PHCentralTrack::set_zsect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_zsect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_zsect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ecorr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ecorr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ecorr() : -999);
}

void
PHCentralTrack::set_ecorr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ecorr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ecorr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ecore(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ecore(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ecore() : -999);
}

void
PHCentralTrack::set_ecore(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ecore(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ecore(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

void
PHCentralTrack::set_dep(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ecore(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_dep(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emce(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emce(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emce() : -999);
}

void
PHCentralTrack::set_emce(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emce(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emce(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcdispy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcdispy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcdispy() : -999);
}

void
PHCentralTrack::set_emcdispy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcdispy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcdispy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcdispz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcdispz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcdispz() : -999);
}

void
PHCentralTrack::set_emcdispz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcdispz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcdispz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_temc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_temc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_temc() : -999);
}

void
PHCentralTrack::set_temc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_temc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_temc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_prob(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_prob(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_prob() : -999);
}

void
PHCentralTrack::set_prob(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_prob(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_prob(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ecent(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ecent(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ecent() : -999);
}

void
PHCentralTrack::set_ecent(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ecent(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ecent(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_twrhit(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_twrhit(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_twrhit() : -999);
}

void
PHCentralTrack::set_twrhit(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_twrhit(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_twrhit(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_e9(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_e9(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_e9() : -999);
}

void
PHCentralTrack::set_e9(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_e9(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_e9(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_re9(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_re9(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_re9() : -999);
}

void
PHCentralTrack::set_re9(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_re9(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_re9(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcchi2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcchi2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcchi2() : -999);
}

void
PHCentralTrack::set_emcchi2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcchi2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcchi2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sysect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sysect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sysect() : -999);
}

void
PHCentralTrack::set_sysect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sysect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sysect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_szsect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_szsect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_szsect() : -999);
}

void
PHCentralTrack::set_szsect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_szsect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_szsect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_secorr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_secorr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_secorr() : -999);
}

void
PHCentralTrack::set_secorr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_secorr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_secorr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_secore(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_secore(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_secore() : -999);
}

void
PHCentralTrack::set_secore(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_secore(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_secore(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semce(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semce(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semce() : -999);
}

void
PHCentralTrack::set_semce(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semce(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semce(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcdispy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcdispy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcdispy() : -999);
}

void
PHCentralTrack::set_semcdispy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcdispy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcdispy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcdispz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcdispz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcdispz() : -999);
}

void
PHCentralTrack::set_semcdispz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcdispz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcdispz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stemc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stemc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stemc() : -999);
}

void
PHCentralTrack::set_stemc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stemc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stemc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_sprob(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sprob(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sprob() : -999);
}

void
PHCentralTrack::set_sprob(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_sprob(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sprob(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_secent(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_secent(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_secent() : -999);
}

void
PHCentralTrack::set_secent(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_secent(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_secent(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_stwrhit(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stwrhit(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stwrhit() : -999);
}

void
PHCentralTrack::set_stwrhit(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_stwrhit(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stwrhit(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_se9(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_se9(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_se9() : -999);
}

void
PHCentralTrack::set_se9(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_se9(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_se9(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_sre9(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sre9(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sre9() : -999);
}

void
PHCentralTrack::set_sre9(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_sre9(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sre9(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcchi2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcchi2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcchi2() : -999);
}

void
PHCentralTrack::set_semcchi2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcchi2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcchi2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_slat(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_slat(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_slat() : -999);
}

void
PHCentralTrack::set_slat(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_slat(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_slat(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ttof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ttof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ttof() : -999);
}

void
PHCentralTrack::set_ttof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ttof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ttof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_etof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_etof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_etof() : -999);
}

void
PHCentralTrack::set_etof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_etof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_etof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_sttof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sttof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sttof() : -999);
}

void
PHCentralTrack::set_sttof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_sttof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sttof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_setof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_setof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_setof() : -999);
}

void
PHCentralTrack::set_setof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_setof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_setof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_slat_mrpc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_slat_mrpc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_slat_mrpc() : -999);
}

void
PHCentralTrack::set_slat_mrpc(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_slat_mrpc(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_slat_mrpc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ttof_mrpc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ttof_mrpc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ttof_mrpc() : -999);
}

void
PHCentralTrack::set_ttof_mrpc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ttof_mrpc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ttof_mrpc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ttofd_mrpc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ttofd_mrpc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ttofd_mrpc() : -999);
}

void
PHCentralTrack::set_ttofd_mrpc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ttofd_mrpc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ttofd_mrpc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_qtof_mrpc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_qtof_mrpc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_qtof_mrpc() : -999);
}

void
PHCentralTrack::set_qtof_mrpc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_qtof_mrpc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_qtof_mrpc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_acc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_acc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_acc() : -999);
}

void
PHCentralTrack::set_acc(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_acc(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_acc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_ring(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ring(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ring() : -999);
}

void
PHCentralTrack::set_ring(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_ring(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ring(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_n0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_n0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_n0() : -999);
}

void
PHCentralTrack::set_n0(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_n0(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_n0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_npe0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_npe0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_npe0() : -999);
}

void
PHCentralTrack::set_npe0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_npe0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_npe0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_n1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_n1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_n1() : -999);
}

void
PHCentralTrack::set_n1(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_n1(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_n1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_npe1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_npe1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_npe1() : -999);
}

void
PHCentralTrack::set_npe1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_npe1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_npe1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_chi2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_chi2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_chi2() : -999);
}

void
PHCentralTrack::set_chi2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_chi2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_chi2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_disp(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_disp(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_disp() : -999);
}

void
PHCentralTrack::set_disp(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_disp(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_disp(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tcrk(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tcrk(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tcrk() : -999);
}

void
PHCentralTrack::set_tcrk(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tcrk(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tcrk(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_cross_phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_cross_phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_cross_phi() : -999);
}

void
PHCentralTrack::set_cross_phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_cross_phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_cross_phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_cross_z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_cross_z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_cross_z() : -999);
}

void
PHCentralTrack::set_cross_z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_cross_z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_cross_z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_center_phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_center_phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_center_phi() : -999);
}

void
PHCentralTrack::set_center_phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_center_phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_center_phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_center_z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_center_z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_center_z() : -999);
}

void
PHCentralTrack::set_center_z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_center_z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_center_z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sacc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sacc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sacc() : -999);
}

void
PHCentralTrack::set_sacc(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sacc(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sacc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_sring(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sring(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sring() : -999);
}

void
PHCentralTrack::set_sring(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_sring(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sring(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sn0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sn0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sn0() : -999);
}

void
PHCentralTrack::set_sn0(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sn0(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sn0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_snpe0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_snpe0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_snpe0() : -999);
}

void
PHCentralTrack::set_snpe0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_snpe0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_snpe0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sn1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sn1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sn1() : -999);
}

void
PHCentralTrack::set_sn1(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sn1(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sn1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_snpe1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_snpe1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_snpe1() : -999);
}

void
PHCentralTrack::set_snpe1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_snpe1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_snpe1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_schi2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_schi2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_schi2() : -999);
}

void
PHCentralTrack::set_schi2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_schi2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_schi2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_sdisp(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sdisp(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sdisp() : -999);
}

void
PHCentralTrack::set_sdisp(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_sdisp(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sdisp(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stcrk(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stcrk(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stcrk() : -999);
}

void
PHCentralTrack::set_stcrk(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stcrk(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stcrk(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_scross_phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_scross_phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_scross_phi() : -999);
}

void
PHCentralTrack::set_scross_phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_scross_phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_scross_phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_scross_z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_scross_z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_scross_z() : -999);
}

void
PHCentralTrack::set_scross_z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_scross_z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_scross_z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_scenter_phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_scenter_phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_scenter_phi() : -999);
}

void
PHCentralTrack::set_scenter_phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_scenter_phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_scenter_phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_scenter_z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_scenter_z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_scenter_z() : -999);
}

void
PHCentralTrack::set_scenter_z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_scenter_z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_scenter_z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecdedx1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecdedx1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecdedx1() : -999);
}

void
PHCentralTrack::set_tecdedx1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecdedx1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecdedx1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecdedx2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecdedx2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecdedx2() : -999);
}

void
PHCentralTrack::set_tecdedx2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecdedx2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecdedx2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2sdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2sdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2sdphi() : -999);
}

void
PHCentralTrack::set_pc2sdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2sdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2sdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2sdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2sdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2sdz() : -999);
}

void
PHCentralTrack::set_pc2sdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2sdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2sdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3sdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3sdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3sdphi() : -999);
}

void
PHCentralTrack::set_pc3sdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3sdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3sdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3sdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3sdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3sdz() : -999);
}

void
PHCentralTrack::set_pc3sdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3sdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3sdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcsdphi() : -999);
}

void
PHCentralTrack::set_emcsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcsdz() : -999);
}

void
PHCentralTrack::set_emcsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofsdphi() : -999);
}

void
PHCentralTrack::set_tofsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwsdphi() : -999);
}

void
PHCentralTrack::set_tofwsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}


float
PHCentralTrack::get_tofsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofsdz() : -999);
}

void
PHCentralTrack::set_tofsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwsdz() : -999);
}

void
PHCentralTrack::set_tofwsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecsdphi() : -999);
}

void
PHCentralTrack::set_tecsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecsdalpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecsdalpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecsdalpha() : -999);
}

void
PHCentralTrack::set_tecsdalpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecsdalpha(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecsdalpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc2sdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2sdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2sdphi() : -999);
}

void
PHCentralTrack::set_spc2sdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc2sdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2sdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc2sdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2sdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2sdz() : -999);
}

void
PHCentralTrack::set_spc2sdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc2sdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2sdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc3sdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3sdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3sdphi() : -999);
}

void
PHCentralTrack::set_spc3sdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc3sdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3sdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc3sdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3sdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3sdz() : -999);
}

void
PHCentralTrack::set_spc3sdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc3sdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3sdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcsdphi() : -999);
}

void
PHCentralTrack::set_semcsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcsdz() : -999);
}

void
PHCentralTrack::set_semcsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofsdphi() : -999);
}

void
PHCentralTrack::set_stofsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofsdz() : -999);
}

void
PHCentralTrack::set_stofsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofwsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofwsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofwsdphi() : -999);
}

void
PHCentralTrack::set_stofwsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofwsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofwsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofwsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofwsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofwsdz() : -999);
}

void
PHCentralTrack::set_stofwsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofwsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofwsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stecsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecsdphi() : -999);
}

void
PHCentralTrack::set_stecsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stecsdalpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecsdalpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecsdalpha() : -999);
}

void
PHCentralTrack::set_stecsdalpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecsdalpha(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecsdalpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_m2tof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_m2tof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_m2tof() : -999);
}

void
PHCentralTrack::set_m2tof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_m2tof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_m2tof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_m2tofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_m2tofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_m2tofw() : -999);
}

void
PHCentralTrack::set_m2tofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_m2tofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_m2tofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_m2emc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_m2emc(const unsigned int itrk) const");
      return NAN;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_m2emc() : NAN);
}

void
PHCentralTrack::set_m2emc(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_m2emc(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_m2emc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPi() : -999);
}

void
PHCentralTrack::set_isPi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isK(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isK(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isK() : -999);
}

void
PHCentralTrack::set_isK(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isK(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isK(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isP(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isP(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isP() : -999);
}

void
PHCentralTrack::set_isP(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isP(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isP(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPiTofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPiTofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPiTofw() : -999);
}

void
PHCentralTrack::set_isPiTofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPiTofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPiTofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isKTofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isKTofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isKTofw() : -999);
}

void
PHCentralTrack::set_isKTofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isKTofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isKTofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPTofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPTofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPTofw() : -999);
}

void
PHCentralTrack::set_isPTofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPTofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPTofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_dcarm(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_dcarm(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_dcarm() : -999);
}

void
PHCentralTrack::set_dcarm(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_dcarm(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_dcarm(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_dcside(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_dcside(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_dcside() : -999);
}

void
PHCentralTrack::set_dcside(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_dcside(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_dcside(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pc1sect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc1sect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc1sect() : -999);
}

void
PHCentralTrack::set_pc1sect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pc1sect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc1sect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pc2sect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2sect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2sect() : -999);
}

void
PHCentralTrack::set_pc2sect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pc2sect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2sect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pc3sect(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3sect(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3sect() : -999);
}

void
PHCentralTrack::set_pc3sect(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pc3sect(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3sect(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc1phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc1phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc1phi() : -999);
}

void
PHCentralTrack::set_pc1phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc1phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc1phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc1z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc1z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc1z() : -999);
}

void
PHCentralTrack::set_pc1z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc1z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc1z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2phi() : -999);
}

void
PHCentralTrack::set_pc2phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2z() : -999);
}

void
PHCentralTrack::set_pc2z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3phi() : -999);
}

void
PHCentralTrack::set_pc3phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3z() : -999);
}

void
PHCentralTrack::set_pc3z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofphi() : -999);
}

void
PHCentralTrack::set_tofphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofz() : -999);
}

void
PHCentralTrack::set_tofz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecphi() : -999);
}

void
PHCentralTrack::set_tecphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecalpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecalpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecalpha() : -999);
}

void
PHCentralTrack::set_tecalpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecalpha(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecalpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcphi() : -999);
}

void
PHCentralTrack::set_emcphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcz() : -999);
}

void
PHCentralTrack::set_emcz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc1phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc1phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc1phi() : -999);
}

void
PHCentralTrack::set_spc1phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc1phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc1phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc1z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc1z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc1z() : -999);
}

void
PHCentralTrack::set_spc1z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc1z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc1z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc2phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2phi() : -999);
}

void
PHCentralTrack::set_spc2phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc2phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc2z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc2z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc2z() : -999);
}

void
PHCentralTrack::set_spc2z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc2z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc2z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc3phi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3phi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3phi() : -999);
}

void
PHCentralTrack::set_spc3phi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc3phi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3phi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_spc3z(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_spc3z(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spc3z() : -999);
}

void
PHCentralTrack::set_spc3z(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_spc3z(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_spc3z(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofphi() : -999);
}

void
PHCentralTrack::set_stofphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stofz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stofz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stofz() : -999);
}

void
PHCentralTrack::set_stofz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stofz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stofz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stecphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecphi() : -999);
}

void
PHCentralTrack::set_stecphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_stecalpha(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_stecalpha(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecalpha() : -999);
}

void
PHCentralTrack::set_stecalpha(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecalpha(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecalpha(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcphi() : -999);
}

void
PHCentralTrack::set_semcphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcz() : -999);
}

void
PHCentralTrack::set_semcz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcsdphi_e(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcsdphi_e(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcsdphi_e() : -999);
}

void
PHCentralTrack::set_emcsdphi_e(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcsdphi_e(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcsdphi_e(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_emcsdz_e(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcsdz_e(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcsdz_e() : -999);
}

void
PHCentralTrack::set_emcsdz_e(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_emcsdz_e(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcsdz_e(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcsdphi_e(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcsdphi_e(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcsdphi_e() : -999);
}

void
PHCentralTrack::set_semcsdphi_e(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcsdphi_e(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcsdphi_e(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_semcsdz_e(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_semcsdz_e(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_semcsdz_e() : -999);
}

void
PHCentralTrack::set_semcsdz_e(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_semcsdz_e(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_semcsdz_e(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tecnhit(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecnhit(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecnhit() : -999);
}

void
PHCentralTrack::set_tecnhit(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tecnhit(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecnhit(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_n2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_n2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_n2() : -999);
}

void
PHCentralTrack::set_n2(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_n2(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_n2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_npe2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_npe2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_npe2() : -999);
}

void
PHCentralTrack::set_npe2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_npe2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_npe2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_n3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_n3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_n3() : -999);
}

void
PHCentralTrack::set_n3(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_n3(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_n3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_npe3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_npe3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_npe3() : -999);
}

void
PHCentralTrack::set_npe3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_npe3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_npe3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sn2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sn2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sn2() : -999);
}

void
PHCentralTrack::set_sn2(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sn2(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sn2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_snpe2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_snpe2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_snpe2() : -999);
}

void
PHCentralTrack::set_snpe2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_snpe2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_snpe2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_sn3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sn3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sn3() : -999);
}

void
PHCentralTrack::set_sn3(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_sn3(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sn3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_snpe3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_snpe3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_snpe3() : -999);
}

void
PHCentralTrack::set_snpe3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_snpe3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_snpe3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_deadmap(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_deadmap(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_deadmap() : -999);
}

void
PHCentralTrack::set_deadmap(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_deadmap(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_deadmap(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_warnmap(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_warnmap(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_warnmap() : -999);
}

void
PHCentralTrack::set_warnmap(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_warnmap(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_warnmap(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_sdeadmap(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_sdeadmap(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_sdeadmap() : -999);
}

void
PHCentralTrack::set_sdeadmap(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_sdeadmap(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_sdeadmap(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_swarnmap(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_swarnmap(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_swarnmap() : -999);
}

void
PHCentralTrack::set_swarnmap(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_swarnmap(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_swarnmap(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tofecut(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofecut(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofecut() : -999);
}

void
PHCentralTrack::set_tofecut(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tofecut(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofecut(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tofsame(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofsame(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofsame() : -999);
}

void
PHCentralTrack::set_tofsame(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tofsame(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofsame(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_slatnext(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_slatnext(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_slatnext() : -999);
}

void
PHCentralTrack::set_slatnext(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_slatnext(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_slatnext(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ttofnext(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ttofnext(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ttofnext() : -999);
}

void
PHCentralTrack::set_ttofnext(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ttofnext(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ttofnext(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_etofnext(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_etofnext(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_etofnext() : -999);
}

void
PHCentralTrack::set_etofnext(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_etofnext(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_etofnext(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tzrid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrid() : -999);
}

void
PHCentralTrack::set_tzrid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tzrid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pcrid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcrid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcrid() : -999);
}

void
PHCentralTrack::set_pcrid(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pcrid(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcrid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptzrx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptzrx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptzrx() : -999);
}

void
PHCentralTrack::set_ptzrx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptzrx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptzrx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptzry(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptzry(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptzry() : -999);
}

void
PHCentralTrack::set_ptzry(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptzry(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptzry(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptzrz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptzrz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptzrz() : -999);
}

void
PHCentralTrack::set_ptzrz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptzrz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptzrz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppcrx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppcrx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppcrx() : -999);
}

void
PHCentralTrack::set_ppcrx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppcrx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppcrx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppcry(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppcry(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppcry() : -999);
}

void
PHCentralTrack::set_ppcry(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppcry(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppcry(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ppcrz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ppcrz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ppcrz() : -999);
}

void
PHCentralTrack::set_ppcrz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ppcrz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ppcrz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzrtof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrtof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrtof() : -999);
}

void
PHCentralTrack::set_tzrtof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzrtof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrtof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tzrslat(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrslat(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrslat() : -999);
}

void
PHCentralTrack::set_tzrslat(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tzrslat(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrslat(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzreloss(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzreloss(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzreloss() : -999);
}

void
PHCentralTrack::set_tzreloss(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzreloss(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzreloss(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzrx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrx() : -999);
}

void
PHCentralTrack::set_tzrx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzrx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzry(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzry(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzry() : -999);
}

void
PHCentralTrack::set_tzry(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzry(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzry(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzrz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrz() : -999);
}

void
PHCentralTrack::set_tzrz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzrz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pcrtof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcrtof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcrtof() : -999);
}

void
PHCentralTrack::set_pcrtof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pcrtof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcrtof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_pcrslat(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcrslat(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcrslat() : -999);
}

void
PHCentralTrack::set_pcrslat(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_pcrslat(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcrslat(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pcreloss(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcreloss(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcreloss() : -999);
}

void
PHCentralTrack::set_pcreloss(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pcreloss(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcreloss(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pcrx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcrx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcrx() : -999);
}

void
PHCentralTrack::set_pcrx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pcrx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcrx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pcry(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcry(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcry() : -999);
}

void
PHCentralTrack::set_pcry(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pcry(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcry(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pcrz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pcrz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pcrz() : -999);
}

void
PHCentralTrack::set_pcrz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pcrz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pcrz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzrsdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrsdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrsdphi() : -999);
}

void
PHCentralTrack::set_tzrsdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzrsdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrsdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tzrsdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrsdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrsdz() : -999);
}

void
PHCentralTrack::set_tzrsdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tzrsdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrsdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_m2tzr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_m2tzr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_m2tzr() : -999);
}

void
PHCentralTrack::set_m2tzr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_m2tzr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_m2tzr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_m2ntctof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_m2ntctof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_m2ntctof() : -999);
}

void
PHCentralTrack::set_m2ntctof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_m2ntctof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_m2ntctof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pltzr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pltzr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pltzr() : -999);
}

void
PHCentralTrack::set_pltzr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pltzr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pltzr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPitzr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPitzr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPitzr() : -999);
}

void
PHCentralTrack::set_isPitzr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPitzr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPitzr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isKtzr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isKtzr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isKtzr() : -999);
}

void
PHCentralTrack::set_isKtzr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isKtzr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isKtzr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPtzr(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPtzr(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPtzr() : -999);
}

void
PHCentralTrack::set_isPtzr(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPtzr(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPtzr(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPintctof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPintctof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPintctof() : -999);
}

void
PHCentralTrack::set_isPintctof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPintctof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPintctof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isKntctof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isKntctof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isKntctof() : -999);
}

void
PHCentralTrack::set_isKntctof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isKntctof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isKntctof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_isPntctof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_isPntctof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_isPntctof() : -999);
}

void
PHCentralTrack::set_isPntctof(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_isPntctof(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_isPntctof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tzrecut(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tzrecut(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tzrecut() : -999);
}

void
PHCentralTrack::set_tzrecut(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tzrecut(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tzrecut(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_L1Trig(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_L1Trig(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_L1Trig() : -999);
}

void
PHCentralTrack::set_L1Trig(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_L1Trig(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_L1Trig(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc1wid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc1wid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc1wid() : -999);
}

void
PHCentralTrack::set_pc1wid(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc1wid(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc1wid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc2wid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc2wid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc2wid() : -999);
}

void
PHCentralTrack::set_pc2wid(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc2wid(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc2wid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pc3wid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pc3wid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pc3wid() : -999);
}

void
PHCentralTrack::set_pc3wid(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pc3wid(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pc3wid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

long
PHCentralTrack::get_categoryl2eLowPt(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_categoryl2eLowPt(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_categoryl2eLowPt() : -999);
}

void
PHCentralTrack::set_categoryl2eLowPt(const unsigned int itrk, const long val)
{
  if (!GetCentral())
    {
      warning("set_categoryl2eLowPt(const unsigned int itrk, const long val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_categoryl2eLowPt(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

long
PHCentralTrack::get_categoryl2eHighPt(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_categoryl2eHighPt(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_categoryl2eHighPt() : -999);
}

void
PHCentralTrack::set_categoryl2eHighPt(const unsigned int itrk, const long val)
{
  if (!GetCentral())
    {
      warning("set_categoryl2eHighPt(const unsigned int itrk, const long val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_categoryl2eHighPt(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_candIDl2e(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_candIDl2e(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_candIDl2e() : -999);
}

void
PHCentralTrack::set_candIDl2e(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_candIDl2e(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_candIDl2e(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_nlvl2MatchLowOcupy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_nlvl2MatchLowOcupy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_nlvl2MatchLowOcupy() : -999);
}

void
PHCentralTrack::set_nlvl2MatchLowOcupy(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_nlvl2MatchLowOcupy(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_nlvl2MatchLowOcupy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_RawL1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_RawL1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_RawL1() : -999);
}

void
PHCentralTrack::set_RawL1(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_RawL1(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_RawL1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_LivL1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_LivL1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_LivL1() : -999);
}

void
PHCentralTrack::set_LivL1(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_LivL1(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_LivL1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_SclL1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_SclL1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_SclL1() : -999);
}

void
PHCentralTrack::set_SclL1(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_SclL1(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_SclL1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofph1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofph1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofph1() : -999);
}

void
PHCentralTrack::set_tofph1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofph1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofph1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofph2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofph2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofph2() : -999);
}

void
PHCentralTrack::set_tofph2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofph2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofph2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_toftdc1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_toftdc1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_toftdc1() : -999);
}

void
PHCentralTrack::set_toftdc1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_toftdc1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_toftdc1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_toftdc2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_toftdc2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_toftdc2() : -999);
}

void
PHCentralTrack::set_toftdc2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_toftdc2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_toftdc2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_aerindex(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerindex(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerindex() : -999);
}

void
PHCentralTrack::set_aerindex(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_aerindex(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerindex(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_aersindex(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersindex(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersindex() : -999);
}

void
PHCentralTrack::set_aersindex(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_aersindex(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersindex(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph1() : -999);
}

void
PHCentralTrack::set_aerph1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph2() : -999);
}

void
PHCentralTrack::set_aerph2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert1() : -999);
}

void
PHCentralTrack::set_aert1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert2() : -999);
}

void
PHCentralTrack::set_aert2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aernpe(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aernpe(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aernpe() : -999);
}

void
PHCentralTrack::set_aernpe(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aernpe(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aernpe(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_aerstatus(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerstatus(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerstatus() : -999);
}

void
PHCentralTrack::set_aerstatus(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_aerstatus(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerstatus(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph1_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph1_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph1_0() : -999);
}

void
PHCentralTrack::set_aerph1_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph1_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph1_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph2_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph2_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph2_0() : -999);
}

void
PHCentralTrack::set_aerph2_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph2_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph2_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert1_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert1_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert1_0() : -999);
}

void
PHCentralTrack::set_aert1_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert1_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert1_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert2_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert2_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert2_0() : -999);
}

void
PHCentralTrack::set_aert2_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert2_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert2_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph1_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph1_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph1_1() : -999);
}

void
PHCentralTrack::set_aerph1_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph1_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph1_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph2_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph2_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph2_1() : -999);
}

void
PHCentralTrack::set_aerph2_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph2_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph2_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert1_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert1_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert1_1() : -999);
}

void
PHCentralTrack::set_aert1_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert1_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert1_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert2_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert2_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert2_1() : -999);
}

void
PHCentralTrack::set_aert2_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert2_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert2_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph1_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph1_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph1_2() : -999);
}

void
PHCentralTrack::set_aerph1_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph1_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph1_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph2_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph2_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph2_2() : -999);
}

void
PHCentralTrack::set_aerph2_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph2_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph2_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert1_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert1_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert1_2() : -999);
}

void
PHCentralTrack::set_aert1_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert1_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert1_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert2_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert2_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert2_2() : -999);
}

void
PHCentralTrack::set_aert2_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert2_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert2_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph1_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph1_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph1_3() : -999);
}

void
PHCentralTrack::set_aerph1_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph1_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph1_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerph2_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerph2_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerph2_3() : -999);
}

void
PHCentralTrack::set_aerph2_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerph2_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerph2_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert1_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert1_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert1_3() : -999);
}

void
PHCentralTrack::set_aert1_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert1_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert1_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aert2_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aert2_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aert2_3() : -999);
}

void
PHCentralTrack::set_aert2_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aert2_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aert2_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_aerhitid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerhitid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerhitid() : -999);
}

void
PHCentralTrack::set_aerhitid(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_aerhitid(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerhitid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_aerhitconfig(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerhitconfig(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerhitconfig() : -999);
}

void
PHCentralTrack::set_aerhitconfig(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_aerhitconfig(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerhitconfig(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph1_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph1_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph1_0() : -999);
}

void
PHCentralTrack::set_aersph1_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph1_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph1_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph2_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph2_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph2_0() : -999);
}

void
PHCentralTrack::set_aersph2_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph2_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph2_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst1_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst1_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst1_0() : -999);
}

void
PHCentralTrack::set_aerst1_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst1_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst1_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst2_0(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst2_0(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst2_0() : -999);
}

void
PHCentralTrack::set_aerst2_0(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst2_0(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst2_0(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph1_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph1_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph1_1() : -999);
}

void
PHCentralTrack::set_aersph1_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph1_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph1_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph2_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph2_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph2_1() : -999);
}

void
PHCentralTrack::set_aersph2_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph2_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph2_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst1_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst1_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst1_1() : -999);
}

void
PHCentralTrack::set_aerst1_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst1_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst1_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst2_1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst2_1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst2_1() : -999);
}

void
PHCentralTrack::set_aerst2_1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst2_1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst2_1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph1_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph1_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph1_2() : -999);
}

void
PHCentralTrack::set_aersph1_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph1_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph1_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph2_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph2_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph2_2() : -999);
}

void
PHCentralTrack::set_aersph2_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph2_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph2_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst1_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst1_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst1_2() : -999);
}

void
PHCentralTrack::set_aerst1_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst1_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst1_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst2_2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst2_2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst2_2() : -999);
}

void
PHCentralTrack::set_aerst2_2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst2_2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst2_2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph1_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph1_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph1_3() : -999);
}

void
PHCentralTrack::set_aersph1_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph1_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph1_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aersph2_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aersph2_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aersph2_3() : -999);
}

void
PHCentralTrack::set_aersph2_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aersph2_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aersph2_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst1_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst1_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst1_3() : -999);
}

void
PHCentralTrack::set_aerst1_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst1_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst1_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_aerst2_3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aerst2_3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aerst2_3() : -999);
}

void
PHCentralTrack::set_aerst2_3(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_aerst2_3(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aerst2_3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_aershitid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aershitid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aershitid() : -999);
}

void
PHCentralTrack::set_aershitid(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_aershitid(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aershitid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_aershitconfig(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_aershitconfig(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_aershitconfig() : -999);
}

void
PHCentralTrack::set_aershitconfig(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_aershitconfig(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_aershitconfig(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecde(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecde(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecde() : -999);
}

void
PHCentralTrack::set_tecde(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecde(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecde(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecde06(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecde06(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecde06() : -999);
}

void
PHCentralTrack::set_tecde06(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecde06(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecde06(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tectrklen(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tectrklen(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tectrklen() : -999);
}

void
PHCentralTrack::set_tectrklen(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tectrklen(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tectrklen(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tecnde(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecnde(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecnde() : -999);
}

void
PHCentralTrack::set_tecnde(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tecnde(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecnde(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tecnhit100(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecnhit100(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecnhit100() : -999);
}

void
PHCentralTrack::set_tecnhit100(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tecnhit100(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecnhit100(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tecnhit200(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecnhit200(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecnhit200() : -999);
}

void
PHCentralTrack::set_tecnhit200(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tecnhit200(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecnhit200(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

short
PHCentralTrack::get_tecnhit50(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecnhit50(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecnhit50() : -999);
}

void
PHCentralTrack::set_tecnhit50(const unsigned int itrk, const short val)
{
  if (!GetCentral())
    {
      warning("set_tecnhit50(const unsigned int itrk, const short val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecnhit50(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecwtb(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecwtb(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecwtb() : -999);
}

void
PHCentralTrack::set_tecwtb(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecwtb(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecwtb(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tecwtbsq(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tecwtbsq(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecwtbsq() : -999);
}

void
PHCentralTrack::set_tecwtbsq(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecwtbsq(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecwtbsq(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_mcid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_mcid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_mcid() : -999);
}

void
PHCentralTrack::set_mcid(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_mcid(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_mcid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_dchid(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_dchid(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_dchid() : -999);
}

void
PHCentralTrack::set_dchid(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_dchid(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_dchid(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_emcrawtdc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcrawtdc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcrawtdc() : -999);
}

void
PHCentralTrack::set_emcrawtdc(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_emcrawtdc(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcrawtdc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_emcrawadc(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcrawadc(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcrawadc() : -999);
}

void
PHCentralTrack::set_emcrawadc(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_emcrawadc(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcrawadc(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_emcrawadclg(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_emcrawadclg(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_emcrawadclg() : -999);
}

void
PHCentralTrack::set_emcrawadclg(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_emcrawadclg(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_emcrawadclg(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_tof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_tof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_tof() : -999);
}

void
PHCentralTrack::set_idtrk_tof(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_tof(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_tof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_tofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_tofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_tofw() : -999);
}

void
PHCentralTrack::set_idtrk_tofw(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_tofw(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_tofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_tec(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_tec(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_tec() : -999);
}

void
PHCentralTrack::set_idtrk_tec(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_tec(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_tec(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_crk(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_crk(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_crk() : -999);
}

void
PHCentralTrack::set_idtrk_crk(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_crk(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_crk(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_pc2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_pc2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_pc2() : -999);
}

void
PHCentralTrack::set_idtrk_pc2(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_pc2(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_pc2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_pc3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_pc3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_pc3() : -999);
}

void
PHCentralTrack::set_idtrk_pc3(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_pc3(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_pc3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_hbd(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_hbd(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_hbd() : -999);
}

void
PHCentralTrack::set_idtrk_hbd(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_hbd(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_hbd(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_stof(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_stof(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_stof() : -999);
}

void
PHCentralTrack::set_idtrk_stof(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_stof(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_stof(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_stec(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_stec(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_stec() : -999);
}

void
PHCentralTrack::set_idtrk_stec(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_stec(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_stec(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_scrk(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_scrk(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_scrk() : -999);
}

void
PHCentralTrack::set_idtrk_scrk(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_scrk(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_scrk(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_spc2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_spc2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_spc2() : -999);
}

void
PHCentralTrack::set_idtrk_spc2(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_spc2(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_spc2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_idtrk_spc3(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_idtrk_spc3(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_idtrk_spc3() : -999);
}

void
PHCentralTrack::set_idtrk_spc3(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_idtrk_spc3(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_idtrk_spc3(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_dep(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_dep(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));

  recoConsts* rc = recoConsts::instance();
  int run = rc->get_IntFlag("RUNNUMBER");
  // from Run11 on this mess is finally done right !!!!!!!
  if (run >= BEGIN_OF_RUN11)
    {
      return Particle->get_dep();
    }

  float ecore = Particle->get_ecore();
  float mom = Particle->get_mom();
  float the0 = Particle->get_the0();
  float alpha = Particle->get_alpha();
  int dcarm = Particle->get_dcarm();
  int sect = Particle->get_sect();
  int charge = Particle->get_charge();
  int sector = dcarm*4+sect;
 
  if(sector<0 || sector>7){
    return -100;
  }

  DepObj* depobj;

  if (run >= BEGIN_OF_RUN8 && run < BEGIN_OF_RUN9)
    depobj = DepObjv2::instance();
  else
    depobj = DepObjv1::instance();

  if(run >=300105 && run <= 310454)
  {
   mom = mom*sin(the0);
   ecore = ecore*sin(the0);
   return depobj->get_dep(charge,mom,ecore,sector);
  }

  if ((alpha<0 && charge<0) || (alpha>0 && charge>0))
    return depobj->get_dep(charge,mom,ecore,sector);
  else
    return depobj->get_dep(-charge,mom,ecore,sector);
  
 }

float
PHCentralTrack::get_ptofwx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptofwx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptofwx() : -999);
}

void
PHCentralTrack::set_ptofwx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptofwx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptofwx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptofwy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptofwy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptofwy() : -999);
}

void
PHCentralTrack::set_ptofwy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptofwy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptofwy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_ptofwz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ptofwz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ptofwz() : -999);
}

void
PHCentralTrack::set_ptofwz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ptofwz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ptofwz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_pltofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_pltofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_pltofw() : -999);
}

void
PHCentralTrack::set_pltofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_pltofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_pltofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_qtofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_qtofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_qtofw() : -999);
}

void
PHCentralTrack::set_qtofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_qtofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_qtofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}


float
PHCentralTrack::get_tofwx(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwx(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwx() : -999);
}

void
PHCentralTrack::set_tofwx(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwx(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwx(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwy(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwy(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwy() : -999);
}

void
PHCentralTrack::set_tofwy(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwy(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwy(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwz() : -999);
}

void
PHCentralTrack::set_tofwz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}


float
PHCentralTrack::get_ttofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_ttofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_ttofw() : -999);
}

void
PHCentralTrack::set_ttofw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_ttofw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_ttofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwadcup(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwadcup(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwadcup() : -999);
}

void
PHCentralTrack::set_tofwadcup(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwadcup(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwadcup(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwadcdw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwadcdw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwadcdw() : -999);
}

void
PHCentralTrack::set_tofwadcdw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwadcdw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwadcdw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwtdcup(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwtdcup(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwtdcup() : -999);
}

void
PHCentralTrack::set_tofwtdcup(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwtdcup(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwtdcup(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwtdcdw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwtdcdw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwtdcdw() : -999);
}

void
PHCentralTrack::set_tofwtdcdw(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwtdcdw(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwtdcdw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;

}


float
PHCentralTrack::get_tofwdphi(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwdphi(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwdphi() : -999);
}

void
PHCentralTrack::set_tofwdphi(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwdphi(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwdphi(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_tofwdz(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_tofwdz(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tofwdz() : -999);
}

void
PHCentralTrack::set_tofwdz(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_tofwdz(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tofwdz(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int
PHCentralTrack::get_striptofw(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_striptofw(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_striptofw() : -999);
}

void
PHCentralTrack::set_striptofw(const unsigned int itrk, const int val)
{
  if (!GetCentral())
    {
      warning("set_striptofw(const unsigned int itrk, const int val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_striptofw(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdhubcharge(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdhubcharge(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdhubcharge() : -999);
}

void
PHCentralTrack::set_hbdhubcharge(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdhubcharge(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdhubcharge(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}



float
PHCentralTrack::get_hbdspokecharge1(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdspokecharge1(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdspokecharge1() : -999);
}

void
PHCentralTrack::set_hbdspokecharge1(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdspokecharge1(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdspokecharge1(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdspokecharge2(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdspokecharge2(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdspokecharge2() : -999);
}

void
PHCentralTrack::set_hbdspokecharge2(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdspokecharge2(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdspokecharge2(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdscharge(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdscharge(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdscharge() : -999);
}

void
PHCentralTrack::set_hbdscharge(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdscharge(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdscharge(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
      << itrk << endl;
    }
  return ;
}

float
PHCentralTrack::get_hbdlocalmax(const unsigned int itrk) const
{
  if (!GetCentral())
    {
      warning("get_hbdlocalmax(const unsigned int itrk) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_hbdlocalmax() : -999);
}

void
PHCentralTrack::set_hbdlocalmax(const unsigned int itrk, const float val)
{
  if (!GetCentral())
    {
      warning("set_hbdlocalmax(const unsigned int itrk, const float val) const");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_hbdlocalmax(val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
      << itrk << endl;
    }
  return ;
}


float PHCentralTrack::get_teccharge(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_teccharge(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_teccharge(iplane) : -999);
}

void PHCentralTrack::set_teccharge(const unsigned int itrk, const unsigned int iplane, const float val)
{
  if (!GetCentral())
    {
      warning("set_teccharge(const unsigned int itrk, const unsigned int iplane, const float val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_teccharge(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float PHCentralTrack::get_tecdphiplane(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_tecdphiplane(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecdphiplane(iplane) : -999);
}

void PHCentralTrack::set_tecdphiplane(const unsigned int itrk, const unsigned int iplane, const float val)
{
  if (!GetCentral())
    {
      warning("set_tecdphiplane(const unsigned int itrk, const unsigned int iplane, const float val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecdphiplane(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int PHCentralTrack::get_tecntimebins(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_tecntimebins(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecntimebins(iplane) : -999);
}

void PHCentralTrack::set_tecntimebins(const unsigned int itrk, const unsigned int iplane, const int val)
{
  if (!GetCentral())
    {
      warning("set_tecntimebins(const unsigned int itrk, const unsigned int iplane, const int val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecntimebins(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int PHCentralTrack::get_tecavgtimebin(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_tecavgtimebin(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_tecavgtimebin(iplane) : -999);
}

void PHCentralTrack::set_tecavgtimebin(const unsigned int itrk, const unsigned int iplane, const int val)
{
  if (!GetCentral())
    {
      warning("set_tecavgtimebin(const unsigned int itrk, const unsigned int iplane, const int val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_tecavgtimebin(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}


float PHCentralTrack::get_steccharge(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_steccharge(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_steccharge(iplane) : -999);
}

void PHCentralTrack::set_steccharge(const unsigned int itrk, const unsigned int iplane, const float val)
{
  if (!GetCentral())
    {
      warning("set_steccharge(const unsigned int itrk, const unsigned int iplane, const float val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_steccharge(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

float PHCentralTrack::get_stecdphiplane(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_stecdphiplane(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecdphiplane(iplane) : -999);
}

void PHCentralTrack::set_stecdphiplane(const unsigned int itrk, const unsigned int iplane, const float val)
{
  if (!GetCentral())
    {
      warning("set_stecdphiplane(const unsigned int itrk, const unsigned int iplane, const float val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecdphiplane(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int PHCentralTrack::get_stecntimebins(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_stecntimebins(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecntimebins(iplane) : -999);
}

void PHCentralTrack::set_stecntimebins(const unsigned int itrk, const unsigned int iplane, const int val)
{
  if (!GetCentral())
    {
      warning("set_stecntimebins(const unsigned int itrk, const unsigned int iplane, const int val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecntimebins(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

int PHCentralTrack::get_stecavgtimebin(const unsigned int itrk, const unsigned int iplane) const
{
  if (!GetCentral())
    {
      warning("get_stecavgtimebin(const unsigned int itrk, const unsigned int iplane) const");
      return -9999;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_stecavgtimebin(iplane) : -999);
}

void PHCentralTrack::set_stecavgtimebin(const unsigned int itrk, const unsigned int iplane, const int val)
{
  if (!GetCentral())
    {
      warning("set_stecavgtimebin(const unsigned int itrk, const unsigned int iplane, const int val)");
      return;
    }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle)
    {
      Particle->set_stecavgtimebin(iplane, val);
    }
  else
    {
      cout << PHWHERE << "ERROR no PHCentralTrack object found at index "
	   << itrk << endl;
    }
  return ;
}

  // Silicon Vertex (SVX) stuff

void PHCentralTrack::set_svxdphi            (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_svxdphi"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxdphi(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxdz              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_svxdz"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxdz(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxid              (const unsigned int itrk, const unsigned int ilayer, const int val)
{
  if (!GetCentral()) { warning("set_svxid"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxid(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxsdphi           (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_svxsdphi"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxsdphi(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxsdz             (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_svxsdz"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxsdz(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxsid             (const unsigned int itrk, const unsigned int ilayer, const int val)
{
  if (!GetCentral()) { warning("set_svxsid"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxsid(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_psvxx              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_psvxx"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_psvxx(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_psvxy              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_psvxy"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_psvxy(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_psvxz              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_psvxz"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_psvxz(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_spsvxx              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_spsvxx"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_spsvxx(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_spsvxy              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_spsvxy"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_spsvxy(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_spsvxz              (const unsigned int itrk, const unsigned int ilayer, const float val)
{
  if (!GetCentral()) { warning("set_spsvxz"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_spsvxz(ilayer, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxtrackid         (const unsigned int itrk, const int val)
{
  if (!GetCentral()) { warning("set_svxtrackid"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxtrackid(val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxtrackquality           (const unsigned int itrk, const float val)
{
  if (!GetCentral()) { warning("set_svxtrackquality"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxtrackquality(val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxdca2d           (const unsigned int itrk, const float val)
{
  if (!GetCentral()) { warning("set_svxdca2d"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxdca2d(val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxdca3d           (const unsigned int itrk, const float val)
{
  if (!GetCentral()) { warning("set_svxdca3d"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxdca3d(val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxdca2dkf           (const unsigned int itrk, const float val)
{
  if (!GetCentral()) { warning("set_svxdca2dkf"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxdca2dkf(val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxdca3dkf           (const unsigned int itrk, const float val)
{
  if (!GetCentral()) { warning("set_svxdca3dkf"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxdca3dkf(val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxxyz0              (const unsigned int itrk, const unsigned int icoor, const float val)
{
  if (!GetCentral()) { warning("set_svxxyz0"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxxyz0(icoor, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxpxyz0             (const unsigned int itrk, const unsigned int icoor, const float val)
{
  if (!GetCentral()) { warning("set_svxpxyz0"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxpxyz0(icoor, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxxyzkf             (const unsigned int itrk, const unsigned int icoor, const float val)
{
  if (!GetCentral()) { warning("set_svxxyzkf"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxxyzkf(icoor, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}
void PHCentralTrack::set_svxpxyzkf            (const unsigned int itrk, const unsigned int icoor, const float val)
{
  if (!GetCentral()) { warning("set_svxpxyzkf"); return; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  if (Particle) { Particle->set_svxpxyzkf(icoor, val); }
  else { cerr << PHWHERE << "ERROR no PHCentralTrack object found at index " << itrk << endl; }
  return ;
}


float PHCentralTrack::get_svxdphi            (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_svxdphi"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxdphi(ilayer) : -9999);

}
float PHCentralTrack::get_svxdz              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_svxdz"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxdz(ilayer) : -9999);
}
int PHCentralTrack::get_svxid                (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_svxid"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxid(ilayer) : -9999);
}
float PHCentralTrack::get_svxsdphi           (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_svxsdphi"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxsdphi(ilayer) : -9999);
}
float PHCentralTrack::get_svxsdz             (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_svxsdz"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxsdz(ilayer) : -9999);
}
int PHCentralTrack::get_svxsid               (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_svxsid"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxsid(ilayer) : -9999);
}
float PHCentralTrack::get_psvxx              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_psvxx"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_psvxx(ilayer) : -9999);
}
float PHCentralTrack::get_psvxy              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_psvxy"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_psvxy(ilayer) : -9999);
}
float PHCentralTrack::get_psvxz              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_psvxz"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_psvxz(ilayer) : -9999);
}
float PHCentralTrack::get_spsvxx              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_spsvxx"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spsvxx(ilayer) : -9999);
}
float PHCentralTrack::get_spsvxy              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_spsvxy"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spsvxy(ilayer) : -9999);
}
float PHCentralTrack::get_spsvxz              (const unsigned int itrk, const unsigned int ilayer) const
{
  if (!GetCentral()) { warning("get_spsvxz"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_spsvxz(ilayer) : -9999);
}
int PHCentralTrack::get_svxtrackid           (const unsigned int itrk) const
{
  if (!GetCentral()) { warning("get_svxtrackid"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxtrackid() : -9999);
}
float PHCentralTrack::get_svxtrackquality           (const unsigned int itrk) const
{
  if (!GetCentral()) { warning("get_svxtrackquality"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxtrackquality() : -9999);
}
float PHCentralTrack::get_svxdca2d           (const unsigned int itrk) const
{
  if (!GetCentral()) { warning("get_svxdca2d"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxdca2d() : -9999);
}
float PHCentralTrack::get_svxdca3d           (const unsigned int itrk) const
{
  if (!GetCentral()) { warning("get_svxdca3d"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxdca3d() : -9999);
}
float PHCentralTrack::get_svxdca2dkf           (const unsigned int itrk) const
{
  if (!GetCentral()) { warning("get_svxdca2dkf"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxdca2dkf() : -9999);
}
float PHCentralTrack::get_svxdca3dkf           (const unsigned int itrk) const
{
  if (!GetCentral()) { warning("get_svxdca3dkf"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxdca3dkf() : -9999);
}
float PHCentralTrack::get_svxxyz0              (const unsigned int itrk, const unsigned int icoor) const
{
  if (!GetCentral()) { warning("get_svxxyz0"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxxyz0(icoor) : -9999);
}
float PHCentralTrack::get_svxpxyz0             (const unsigned int itrk, const unsigned int icoor) const
{
  if (!GetCentral()) { warning("get_svxpxyz0"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxpxyz0(icoor) : -9999);
}
float PHCentralTrack::get_svxxyzkf              (const unsigned int itrk, const unsigned int icoor) const
{
  if (!GetCentral()) { warning("get_svxxyzkf"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxxyzkf(icoor) : -9999);
}
float PHCentralTrack::get_svxpxyzkf             (const unsigned int itrk, const unsigned int icoor) const
{
  if (!GetCentral()) { warning("get_svxpxyzkf"); return -9999; }
  PHSnglCentralTrack *Particle = static_cast<PHSnglCentralTrack *> (GetCentral()->UncheckedAt(itrk));
  return ((Particle) ? Particle->get_svxpxyzkf(icoor) : -9999);
}
