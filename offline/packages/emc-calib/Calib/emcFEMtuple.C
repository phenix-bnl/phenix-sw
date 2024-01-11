// $Id: emcFEMtuple.C,v 1.13 2003/02/27 18:54:27 aphecetc Exp $

#include <iostream>
#include "emcFEMtuple.h"
#include "emcCalFEMFactory.h"
#include <cassert>
#include "emcCalFEM.h"

using namespace std;

//_____________________________________________________________________________
emcFEMtuple::emcFEMtuple() : fOwner(true)
{
  Reset();
}

//_____________________________________________________________________________
emcFEMtuple::~emcFEMtuple()
{
  Reset();
}

//_____________________________________________________________________________
bool emcFEMtuple::Add(emcFEMtuple& fem)
{
  bool rv = false;
  assert(fem.GetSize() == 1);
  assert(fem.IsOwner() == true);
  if ( fem.GetSize() == 1 && fem.IsOwner() == true)
    {
      rv = true;
      fFEMs.push_back(fem.GetFEM(0));
      fem.SetOwnership(false);
      if ( fem.Get1() > fLastStartTime )
        {
          fLastStartTime = fem.Get1();
        }
      if ( fem.Get2() < fFirstEndTime )
        {
          fFirstEndTime = fem.Get2();
        }
    }
  return rv;
}

//_____________________________________________________________________________
emcCalFEM* emcFEMtuple::AppendFEM(int absPosition,
                                  const PHTimeStamp& t1,
                                  const PHTimeStamp& t2)
{
  emcCalFEM* calfem = emcCalFEMFactory::Create(GetCategory(),
					       absPosition,
					       t1, t2);
  if (calfem)
    {
      fFEMs.push_back(calfem);
      if ( t1 > fLastStartTime )
        {
          fLastStartTime = t1;
        }
      if ( t2 < fFirstEndTime )
        {
          fFirstEndTime = t2;
        }
      return fFEMs.back();
    }
  else
    {
      return 0;
    }
}

//_____________________________________________________________________________
emcCalFEM* emcFEMtuple::GetFEM(int ifem)
{
  if (CheckIndex(ifem))
    {
      return fFEMs[ifem];
    }
  else
    {
      return 0;
    }
}

//_____________________________________________________________________________
int emcFEMtuple::GetXmin(int ifem) const
{
  int rv = -1;
  if ( CheckIndex(ifem) )
    {
      rv = fFEMs[ifem]->GetXmin();
    }
  else
    {
      cerr << "<E> emcFEMtuple::GetXmin(int ifem) : ifem out of bounds." << endl;
    }
  return rv;
}

//_____________________________________________________________________________
int emcFEMtuple::GetXmax(int ifem) const
{
  int rv = -1;
  if ( CheckIndex(ifem) )
    {
      rv = fFEMs[ifem]->GetXmax();
    }
  else
    {
      cerr << "<E> emcFEMtuple::GetXmax(int ifem) : ifem out of bounds." << endl;
    }
  return rv;
}


//_____________________________________________________________________________
int emcFEMtuple::GetNumberOfChannels(void) const
{
  int n = 0;
  size_t i;
  for (i = 0;i < fFEMs.size();i++)
    {
      n += fFEMs[i]->GetNumberOfChannels();
    }
  return n;
}

//_____________________________________________________________________________
bool
emcFEMtuple::IsDraft(void) const
{
  size_t i;
  bool draft = false;
  size_t n = 0;

  for (i = 0;i < fFEMs.size();i++)
    {
      if ( fFEMs[i]->IsDraft() )
	{
	  n++;
	}
    }

  if (n)
    {
      assert(n == fFEMs.size());
      draft = true;
    }
  return draft;
}

//_____________________________________________________________________________
bool emcFEMtuple::IsValid(const PHTimeStamp& cwhen) const
{
  bool valid;

  if (fFEMs.empty())
    {
      valid = false;
    }
  else if (GetSource() == emcManageable::kFile_ASCII)
    {
      // When reading calibration data from files, once read,
      // data are valid forever.
      valid = true;
    }
  else
    {
      valid = true;
      // if we are inside our global range, fine, we do
      // not need to test each FEM separately, otherwise,
      // well, we need to...
      PHTimeStamp& when = const_cast<PHTimeStamp&>(cwhen);
      if ( !when.isInRange(fLastStartTime, fFirstEndTime) )
	{
	  valid = false;
	}
    }

  return valid;
}

//_____________________________________________________________________________
bool emcFEMtuple::IsValid(const PHTimeStamp& cwhen, int ifem) const
{
  bool rv = false;
  if ( CheckIndex(ifem) )
    {
      rv = fFEMs[ifem]->IsValid(cwhen);
    }
  return rv;
}

//_____________________________________________________________________________
void emcFEMtuple::Print(int level)
{
  Print(cout, level);
}

//_____________________________________________________________________________
ostream&
emcFEMtuple::Print(ostream& out, int level)
{
  size_t i;
  for (i = 0;i < fFEMs.size();i++)
    {
      fFEMs[i]->Print(out, level);
    }
  return out;
}

//_____________________________________________________________________________
bool emcFEMtuple::ReplaceFEM(int ifem, emcFEMtuple& fem)
{
  bool rv = false;
  assert(fOwner == true);
  assert (CheckIndex(ifem) != 0);
  assert(fem.GetNumberOfFEMs() == 1);
  assert(fem.IsOwner() == true);
  if ( fOwner && CheckIndex(ifem) &&
       fem.GetNumberOfFEMs() == 1 && fem.IsOwner() )
    {
      delete fFEMs[ifem];
      emcCalFEM* calfem = fem.GetFEM(0);
      fFEMs[ifem] = calfem;
      fem.SetOwnership(false);
      if ( calfem->GetStartValTime() > fLastStartTime )
        {
          fLastStartTime = calfem->GetStartValTime();
        }
      if ( calfem->GetEndValTime() < fFirstEndTime )
        {
          fFirstEndTime = calfem->GetEndValTime();
        }
      rv = true;
    }
  return rv;
}

//_____________________________________________________________________________
void emcFEMtuple::Reset(void)
{
  if (fOwner)
    {
      size_t i;
      for ( i = 0; i < fFEMs.size(); i++ )
        {
          delete fFEMs[i];
        }
    }

  fFEMs.clear();

  fOwner = true;

  fLastStartTime.setTics(0);
  fFirstEndTime.setToFarFuture();
}

//_____________________________________________________________________________
void
emcFEMtuple::SetDraft(bool draft)
{
  size_t i;

  for (i = 0;i < fFEMs.size();i++)
    {
      fFEMs[i]->SetDraft(draft);
    }
}

//_____________________________________________________________________________
bool emcFEMtuple::SetXmin(int xmin, int ifem)
{
  bool rv = false;
  if ( CheckIndex(ifem) )
    {
      fFEMs[ifem]->SetXmin(xmin);
      rv = true;
    }
  else
    {
      cerr << "<E> emcFEMtuple::SetXmin(int, int ifem) : ifem out of bounds" << endl;
    }
  return rv;
}

//_____________________________________________________________________________
bool emcFEMtuple::SetXmax(int xmax, int ifem)
{
  bool rv = false;
  if ( CheckIndex(ifem) )
    {
      fFEMs[ifem]->SetXmax(xmax);
      rv = true;
    }
  else
    {
      cerr << "<E> emcFEMtuple::SetXmax(int, int ifem) : ifem out of bounds" << endl;
    }
  return rv;
}
