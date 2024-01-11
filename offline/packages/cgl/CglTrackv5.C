#include "CglTrackv5.h"
#include "CglSnglTrackv5.h"
#include "dCglTrackWrapper.h"

#include "phool.h"

#include "TClonesArray.h"

ClassImp(CglTrackv5)

using namespace std;

#define CGLNTRACKS 200

CglTrackv5::CglTrackv5()
{
  CglNTrack = 0;
  CglTrk = new TClonesArray("CglSnglTrackv5",CGLNTRACKS);
  return;
}

CglTrackv5::~CglTrackv5()
{
  if (CglTrk)
    {
      CglTrk->Clear();
      delete CglTrk;
    }
  return;
}

void CglTrackv5::identify(ostream& os) const
{
  os << "identify yourself: CglTrackv5 Object" << endl;
  os << "No of Tracks: " << CglNTrack << endl;
  return;
}

void CglTrackv5::Reset()
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
 if (CglNTrack>CGLNTRACKS)
   {
     CglTrk->Expand(CGLNTRACKS);
   }
 CglNTrack = 0;
 return;
}

int CglTrackv5::isValid() const
{
  return((CglNTrack>0) ? 1 : 0);
}

void  CglTrackv5::AddCglTrack(const unsigned int itrk)
{
  TClonesArray &cgltrk = *CglTrk;
  new(cgltrk[itrk]) CglSnglTrackv5();
  return;
}

void  CglTrackv5::FillFromWrapper(dCglTrackWrapper *wrap)
{
  unsigned int itrk;
  if (wrap)
    {
      CglNTrack = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (itrk = 0; itrk < wrap->RowCount(); itrk++)
	{
          AddCglTrack(itrk);
	  set_arm(itrk,wrap->get_arm(itrk));
	  set_id(itrk,wrap->get_id(itrk));
	  set_dctracksid(itrk,wrap->get_dctracksid(itrk));
	  set_emcclusid(itrk,wrap->get_emcclusid(itrk));
	  set_pc1clusid(itrk,wrap->get_pc1clusid(itrk));
	  set_pc2clusid(itrk,wrap->get_pc2clusid(itrk));
	  set_pc3clusid(itrk,wrap->get_pc3clusid(itrk));
	  set_richringid(itrk,wrap->get_richringid(itrk));
	  set_tectrackid(itrk,wrap->get_tectrackid(itrk));
	  set_tofrecid(itrk,wrap->get_tofrecid(itrk));
	  set_trackModel(itrk,wrap->get_trackModel(itrk));
	  set_quality(itrk,wrap->get_quality(itrk));
	}
    }
  return;
}

short CglTrackv5::get_arm(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_arm() : -999);
}

void CglTrackv5::set_arm(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_arm(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_id(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_id() : -999);
}

void CglTrackv5::set_id(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_id(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_dctracksid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_dctracksid() : -999);
}

void CglTrackv5::set_dctracksid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_dctracksid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_tectrackid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tectrackid() : -999);
}

void CglTrackv5::set_tectrackid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tectrackid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_pc1clusid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc1clusid() : -999);
}

void CglTrackv5::set_pc1clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc1clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_pc2clusid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc2clusid() : -999);
}

void CglTrackv5::set_pc2clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc2clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_pc3clusid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc3clusid() : -999);
}

void CglTrackv5::set_pc3clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc3clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_tofrecid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tofrecid() : -999);
}

void CglTrackv5::set_tofrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tofrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}
short CglTrackv5::get_accrecid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_accrecid() : -999);
}

void CglTrackv5::set_accrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_accrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_mrpcrecid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_mrpcrecid() : -999);
}

void CglTrackv5::set_mrpcrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_mrpcrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_emcclusid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_emcclusid() : -999);
}

void CglTrackv5::set_emcclusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_emcclusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_richringid(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_richringid() : -999);
}

void CglTrackv5::set_richringid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_richringid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

short CglTrackv5::get_trackModel(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_trackModel() : -999);
}

void CglTrackv5::set_trackModel(const unsigned int itrk, const short ival)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_trackModel(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

float CglTrackv5::get_quality(const unsigned int itrk) const
{

  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_quality() : -999.9);
}

void CglTrackv5::set_quality(const unsigned int itrk, const float rval)
{
  CglSnglTrackv5 *cgltrk = (CglSnglTrackv5 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_quality(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv5 object found" << endl;
  }
  return;
}

