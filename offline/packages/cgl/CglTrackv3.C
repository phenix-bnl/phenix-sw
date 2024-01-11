#include "CglTrackv3.h"
#include "CglSnglTrackv3.h"
#include "dCglTrackWrapper.h"

#include "phool.h"

#include "TClonesArray.h"

ClassImp(CglTrackv3)

using namespace std;

#define CGLNTRACKS 200

CglTrackv3::CglTrackv3()
{
  CglNTrack = 0;
  CglTrk = new TClonesArray("CglSnglTrackv3",CGLNTRACKS);
  return;
}

CglTrackv3::~CglTrackv3()
{
  CglTrk->Clear();
  return;
}

void CglTrackv3::identify(ostream& os) const
{
  os << "identify yourself: CglTrackv3 Object" << endl;
  os << "No of Tracks: " << CglNTrack << endl;
  return;
}

void CglTrackv3::Reset()
{
 CglTrk->Clear();
 if (CglNTrack>CGLNTRACKS)
   {
     CglTrk->Expand(CGLNTRACKS);
   }
 CglNTrack = 0;
 return;
}

int CglTrackv3::isValid() const
{
  return((CglNTrack>0) ? 1 : 0);
}

int CglTrackv3::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > CGLNTRACKS)
    {
      CglTrk->Expand(nhits);
     }
  return nhits;
}

void  CglTrackv3::AddCglTrack(const unsigned int itrk)
{
  TClonesArray &cgltrk = *CglTrk;
  new(cgltrk[itrk]) CglSnglTrackv3();
  return;
}

void  CglTrackv3::FillFromWrapper(dCglTrackWrapper *wrap)
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

short CglTrackv3::get_arm(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_arm() : -999);
}

void CglTrackv3::set_arm(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_arm(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_id(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_id() : -999);
}

void CglTrackv3::set_id(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_id(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_dctracksid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_dctracksid() : -999);
}

void CglTrackv3::set_dctracksid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_dctracksid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_tectrackid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tectrackid() : -999);
}

void CglTrackv3::set_tectrackid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tectrackid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_pc1clusid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc1clusid() : -999);
}

void CglTrackv3::set_pc1clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc1clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_pc2clusid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc2clusid() : -999);
}

void CglTrackv3::set_pc2clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc2clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_pc3clusid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_pc3clusid() : -999);
}

void CglTrackv3::set_pc3clusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_pc3clusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_tofrecid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_tofrecid() : -999);
}

void CglTrackv3::set_tofrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_tofrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_accrecid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_accrecid() : -999);
}

void CglTrackv3::set_accrecid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_accrecid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}


short CglTrackv3::get_emcclusid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_emcclusid() : -999);
}

void CglTrackv3::set_emcclusid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_emcclusid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_richringid(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_richringid() : -999);
}

void CglTrackv3::set_richringid(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_richringid(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

short CglTrackv3::get_trackModel(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_trackModel() : -999);
}

void CglTrackv3::set_trackModel(const unsigned int itrk, const short ival)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_trackModel(ival);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}

float CglTrackv3::get_quality(const unsigned int itrk) const
{

  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  return((cgltrk) ? cgltrk->get_quality() : -999.9);
}

void CglTrackv3::set_quality(const unsigned int itrk, const float rval)
{
  CglSnglTrackv3 *cgltrk = (CglSnglTrackv3 *) GetCglTrk()->UncheckedAt(itrk);
  if (cgltrk)
    {
      cgltrk->set_quality(rval);
    }
else
  {
    cerr << PHWHERE << "ERROR no CglSnglTrackv3 object found" << endl;
  }
  return;
}





