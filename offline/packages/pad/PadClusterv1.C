#include <dPadClusterWrapper.h>
#include <PadClusterv1.h>
#include <PadSnglClusterv1.h>
#include <phool.h>
#include <TClonesArray.h>
#include <iostream>

ClassImp(PadClusterv1)

using namespace std;

static const unsigned int PADNCLUSTER = 200;

PadClusterv1::PadClusterv1()
{
  PadNCluster = 0;
  PcClus = new TClonesArray("PadSnglClusterv1",PADNCLUSTER);
  return;
}

PadClusterv1::~PadClusterv1()
{
  if (PcClus)
    {
      PcClus->Clear();
      delete PcClus;
    }
  return;
}

void PadClusterv1::identify(ostream& os) const
{
  os << "identify yourself: PadClusterv1 Object" << endl;
  os << "No of Clusters: " << PadNCluster << endl;
  return;
}

void PadClusterv1::Reset()
{
 PcClus->Clear();
 if (PadNCluster>PADNCLUSTER)
   {
     PcClus->Expand(PADNCLUSTER);
   }
 PadNCluster = 0;
 return;
}

int PadClusterv1::isValid() const
{
  return ((PadNCluster>0) ? 1 : 0);
}

void PadClusterv1::FillFromWrapper(dPadClusterWrapper *wrap)
{
  unsigned int iclus;
  if (wrap)
    {
      PadNCluster = wrap->RowCount();
      set_TClonesArraySize(wrap->RowCount());
      for (iclus = 0; iclus<wrap->RowCount();iclus++)
	{
          AddPadCluster(iclus);
	  set_arm(iclus,wrap->get_arm(iclus));
	  set_cell(iclus,wrap->get_cell(iclus));
	  set_id(iclus,wrap->get_id(iclus));
	  set_sector(iclus,wrap->get_sector(iclus));
	  set_type(iclus,wrap->get_type(iclus));
	  set_wire(iclus,wrap->get_wire(iclus));
	  for (short j = 0;j<3;j++)
	    {
	      set_dxyz(iclus,j,wrap->get_dxyz(j,iclus));
	      set_xyz(iclus,j,wrap->get_xyz(j,iclus));
	    }
	}
    }
  return;
}

int PadClusterv1::set_TClonesArraySize(const unsigned int nclus)
{
  if (nclus > PADNCLUSTER)
    {
      PcClus->Expand(nclus);
     }
  return nclus;
}

void PadClusterv1::AddPadCluster(const unsigned int iclus)
{
  TClonesArray &padclus = *PcClus;
  new(padclus[iclus]) PadSnglClusterv1();
  return;
}

short PadClusterv1::get_arm(const unsigned int iclus) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_arm() : -999);
}

void PadClusterv1::set_arm(const unsigned int iclus, const short ival)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_arm(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

short PadClusterv1::get_cell(const unsigned int iclus) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_cell() : -999);
}

void PadClusterv1::set_cell(const unsigned int iclus, const short ival)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_cell(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

short PadClusterv1::get_id(const unsigned int iclus) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_id() : -999);
}

void PadClusterv1::set_id(const unsigned int iclus, const short ival)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_id(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

short PadClusterv1::get_sector(const unsigned int iclus) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_sector() : -999);
}

void PadClusterv1::set_sector(const unsigned int iclus, const short ival)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_sector(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

short PadClusterv1::get_type(const unsigned int iclus) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_type() : -999);
}

void PadClusterv1::set_type(const unsigned int iclus, const short ival)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_type(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

short PadClusterv1::get_wire(const unsigned int iclus) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_wire() : -999);
}

void PadClusterv1::set_wire(const unsigned int iclus, const short ival)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_wire(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

float PadClusterv1::get_dxyz(const unsigned int iclus, const short i) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_dxyz(i) : -999);
}

void PadClusterv1::set_dxyz(const unsigned int iclus, const short i, const float rval)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_dxyz(rval,i);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

float PadClusterv1::get_xyz(const unsigned int iclus, const short i) const
{

  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_xyz(i) : -999);
}

void PadClusterv1::set_xyz(const unsigned int iclus, const short i, const float rval)
{
  PadSnglClusterv1 *padclus = (PadSnglClusterv1 *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_xyz(rval,i);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglClusterv1 object found" << endl;
  }
  return;
}

