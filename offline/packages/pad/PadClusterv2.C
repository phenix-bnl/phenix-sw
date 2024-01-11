#include <dPadClusterWrapper.h>
#include <PadClusterv2.h>
#include <PadSnglClusterv2.h>
#include <phool.h>
#include <TClonesArray.h>

#include <cmath>
#include <iostream>

ClassImp(PadClusterv2)

using namespace std;

static unsigned int PADNCLUSTER = 200;

PadClusterv2::PadClusterv2()
{
  PadNCluster = 0;
  PcClus = new TClonesArray("PadSnglClusterv2",PADNCLUSTER);
  return;
}

PadClusterv2::~PadClusterv2()
{
  if (PcClus)
    {
      PcClus->Clear();
      delete PcClus;
    }
  return;
}

void PadClusterv2::identify(ostream& os ) const
{
  os << "identify yourself: PadClusterv2 Object" << endl;
  os << "No of Clusters: " << PadNCluster << endl;
  return;
}

void PadClusterv2::Reset()
{
 PcClus->Clear();
 if (PadNCluster>PADNCLUSTER)
   {
     PcClus->Expand(PADNCLUSTER);
   }
 PadNCluster = 0;
 return;
}

int PadClusterv2::isValid() const
{
  return ((PadNCluster>0) ? 1 : 0);
}

void PadClusterv2::FillFromWrapper(dPadClusterWrapper *wrap)
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

int PadClusterv2::set_TClonesArraySize(const unsigned int nclus)
{
  if (nclus > PADNCLUSTER)
    {
      PcClus->Expand(nclus);
     }
  return nclus;
}

void PadClusterv2::AddPadCluster(const unsigned int iclus)
{
  TClonesArray &padclus = *PcClus;
  new(padclus[iclus]) PadSnglClusterv2();
  return;
}

short PadClusterv2::get_arm(const unsigned int iclus) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_arm() : -999);
}

void PadClusterv2::set_arm(const unsigned int iclus, const short ival)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_arm(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

short PadClusterv2::get_cell(const unsigned int iclus) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_cell() : -999);
}

void PadClusterv2::set_cell(const unsigned int iclus, const short ival)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_cell(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

short PadClusterv2::get_id(const unsigned int iclus) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_id() : -999);
}

void PadClusterv2::set_id(const unsigned int iclus, const short ival)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_id(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

short PadClusterv2::get_sector(const unsigned int iclus) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_sector() : -999);
}

void PadClusterv2::set_sector(const unsigned int iclus, const short ival)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_sector(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

short PadClusterv2::get_type(const unsigned int iclus) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_type() : -999);
}

void PadClusterv2::set_type(const unsigned int iclus, const short ival)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_type(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

short PadClusterv2::get_wire(const unsigned int iclus) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_wire() : -999);
}

void PadClusterv2::set_wire(const unsigned int iclus, const short ival)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_wire(ival);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

float PadClusterv2::get_dxyz(const unsigned int iclus, const short i) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_dxyz(i) : NAN);
}

void PadClusterv2::set_dxyz(const unsigned int iclus, const short i, const float rval)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_dxyz(rval,i);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

float PadClusterv2::get_xyz(const unsigned int iclus, const short i) const
{

  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  return((padclus) ? padclus->get_xyz(i) : NAN);
}

void PadClusterv2::set_xyz(const unsigned int iclus, const short i, const float rval)
{
  PadSnglCluster *padclus = (PadSnglCluster *) GetPcClus()->UncheckedAt(iclus);
  if (padclus)
    {
      padclus->set_xyz(rval,i);
    }
else
  {
    cout << PHWHERE << "ERROR no PadSnglCluster object found" << endl;
  }
  return;
}

