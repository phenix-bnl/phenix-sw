#include "AccClusterv1.h"
#include "AccSnglClusterv1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(AccClusterv1)

using namespace std;
static const unsigned int ACCNCLUSTER = 40; // Initial size...

  // First we implement the "standard functions"...
AccClusterv1::AccClusterv1()
{
  nCluster = 0;
  Cluster  = new TClonesArray("AccSnglClusterv1",ACCNCLUSTER);
}

AccClusterv1::AccClusterv1(const AccClusterv1& rhs)
{
  Cluster=0;
  rhs.copyto(*this);
}

AccClusterv1& 
AccClusterv1::operator=(const AccClusterv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
AccClusterv1::copyto(AccClusterv1& dest) const
{
  delete dest.Cluster;
  dest.Cluster = new TClonesArray("AccSnglClusterv1",nCluster);
  dest.nCluster = nCluster;
  for ( unsigned int i = 0; i < nCluster; ++i ) 
    {
      AccSnglClusterv1* src = static_cast<AccSnglClusterv1*>
	(get_cluster(i));
      if ( src ) 
	{
	  dest.AddCluster(i);
	  AccSnglClusterv1* d = static_cast<AccSnglClusterv1*>
	    (dest.get_cluster(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

AccClusterv1* 
AccClusterv1::clone() const
{
  return new AccClusterv1(*this);
}

AccClusterv1::~AccClusterv1()
{
  if (Cluster)
    {
      Cluster->Clear();
      delete Cluster;
    }
  return;
}

AccSnglCluster* 
AccClusterv1::get_cluster (const unsigned int itrk) const {
  AccSnglClusterv1 *Particle = (AccSnglClusterv1 *) GetCluster()->UncheckedAt(itrk);
  return Particle;
}


void AccClusterv1::identify(std::ostream& os) const
{
  os << "identify yourself: AccClusterv1 Object\n"
     << "No of Clusters: " << nCluster << std::endl;
  return;
}

void AccClusterv1::Reset()
{
 Cluster->Clear();
 if (nCluster>ACCNCLUSTER)
   {
     Cluster->Expand(ACCNCLUSTER);
   }
 nCluster = 0;
 return;
}

int AccClusterv1::isValid() const
{
  return((nCluster>0) ? 1 : 0);
}

int AccClusterv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > ACCNCLUSTER)
    {
      Cluster->Expand(nhits);
     }
  return nhits;
}

void  AccClusterv1::AddCluster(const unsigned int itrk)
{
  TClonesArray &Particle = *Cluster;
  new(Particle[itrk]) AccSnglClusterv1();
  return;
}

AccSnglCluster* 
AccClusterv1::AddCluster(const unsigned int itrk,
				const AccSnglCluster& track)
{
  const AccSnglClusterv1* test = dynamic_cast<const AccSnglClusterv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type AccSnglClusterv1"
	   << endl;
      return 0;
    }

  return new((*Cluster)[itrk]) AccSnglClusterv1(*test);
}


void  AccClusterv1::RemoveCluster(const unsigned int itrk)
{
  Cluster->RemoveAt(itrk);
  return;
}


