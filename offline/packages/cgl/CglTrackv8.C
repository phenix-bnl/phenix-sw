#include <CglTrackv8.h>
#include <CglSnglTrackv8.h>

#include <phool.h>

#include <TClonesArray.h>

ClassImp(CglTrackv8)

using namespace std;

#define CGLNTRACKS 200

CglTrackv8::CglTrackv8()
{
  CglTrk = new TClonesArray("CglSnglTrackv8",CGLNTRACKS);
  return;
}

CglTrackv8::~CglTrackv8()
{
  if (CglTrk)
    {
      CglTrk->Clear();
      delete CglTrk;
    }
  return;
}

void CglTrackv8::identify(ostream& os) const
{
  os << "identify yourself: CglTrackv8 Object" << endl;
  os << "No of Tracks: " << get_CglNTrack() << endl;
  return;
}

void CglTrackv8::Reset()
{
  // the singles class need to be resetted as well,
  // In this implementation root makes the classes only once
  // in the TC->ExpandCreate() and then leaves them alive but
  // does not reset the values
  for (unsigned int i = 0; i<get_CglNTrack(); i++)
    {
      get_track(i)->Reset();
    }
 CglTrk->Clear();
 if (get_CglNTrack()>CGLNTRACKS)
   {
     CglTrk->Expand(CGLNTRACKS);
   }
 return;
}

unsigned int
CglTrackv8::get_CglNTrack() const
{
  return GetCglTrk()->GetEntriesFast();
}

int CglTrackv8::isValid() const
{
  return((get_CglNTrack()>0) ? 1 : 0);
}

void  CglTrackv8::AddCglTrack(const unsigned int itrk)
{
  TClonesArray &cgltrk = *CglTrk;
  new(cgltrk[itrk]) CglSnglTrackv8();
  return;
}

short CglTrackv8::get_arm(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_arm() : -999);
}

void CglTrackv8::set_arm(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_arm(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_id(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_id() : -999);
}

void CglTrackv8::set_id(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_id(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_dctracksid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_dctracksid() : -999);
}

void CglTrackv8::set_dctracksid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_dctracksid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_tectrackid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tectrackid() : -999);
}

void CglTrackv8::set_tectrackid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tectrackid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_pc1clusid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc1clusid() : -999);
}

void CglTrackv8::set_pc1clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc1clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_pc2clusid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc2clusid() : -999);
}

void CglTrackv8::set_pc2clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc2clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_pc3clusid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc3clusid() : -999);
}

void CglTrackv8::set_pc3clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc3clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_hbdblobid(const unsigned int itrk) const
{
   CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
   return((cgltrk) ? cgltrk->get_hbdblobid() : -999);
}

void CglTrackv8::set_hbdblobid(const unsigned int itrk, const short ival)
{
   CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
   if (cgltrk)
   {
      cgltrk->set_hbdblobid(ival);
   }
   else
   {
      cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
   }
   return;
}		   		  

short CglTrackv8::get_tofrecid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tofrecid() : -999);
}

void CglTrackv8::set_tofrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tofrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_tofwrecid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tofwrecid() : -999);
}

void CglTrackv8::set_tofwrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tofwrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_accrecid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_accrecid() : -999);
}

void CglTrackv8::set_accrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_accrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_emcclusid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_emcclusid() : -999);
}

void CglTrackv8::set_emcclusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_emcclusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_richringid(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_richringid() : -999);
}

void CglTrackv8::set_richringid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_richringid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

short CglTrackv8::get_trackModel(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_trackModel() : -999);
}

void CglTrackv8::set_trackModel(const unsigned int itrk, const short ival)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_trackModel(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

float CglTrackv8::get_quality(const unsigned int itrk) const
{

  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_quality() : -999.9);
}

void CglTrackv8::set_quality(const unsigned int itrk, const float rval)
{
  CglSnglTrackv8 *cgltrk = (CglSnglTrackv8 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_quality(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv8 object found" << endl;
  }
  return;
}

