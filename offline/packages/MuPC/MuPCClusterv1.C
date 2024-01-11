#include "MuPCClusterv1.h"
#include "MuPCSnglClusterv1.h"
#include "phool.h"
#include "TClonesArray.h"
#include <iostream>

ClassImp(MuPCClusterv1)

using namespace std;
static const unsigned int MUPCCLUSTER = 100;

  // First we implement the "standard functions"...
MuPCClusterv1::MuPCClusterv1()
{
  nCluster = 0;
  Cluster  = new TClonesArray("MuPCSnglClusterv1",MUPCCLUSTER);
}

MuPCClusterv1::MuPCClusterv1(const MuPCClusterv1& rhs)
{
  Cluster=0;
  rhs.copyto(*this);
}

MuPCClusterv1& 
MuPCClusterv1::operator=(const MuPCClusterv1& rhs)
{
  if ( this != &rhs )
    {
      rhs.copyto(*this);
    }
  return *this;
}

void
MuPCClusterv1::copyto(MuPCClusterv1& dest) const
{
  delete dest.Cluster;
  dest.Cluster = new TClonesArray("MuPCSnglClusterv1",nCluster);
  dest.nCluster = nCluster;
  for ( unsigned int i = 0; i < nCluster; ++i ) 
    {
      MuPCSnglClusterv1* src = static_cast<MuPCSnglClusterv1*>
	(get_cluster(i));
      if ( src ) 
	{
	  dest.AddCluster(i);
	  MuPCSnglClusterv1* d = static_cast<MuPCSnglClusterv1*>
	    (dest.get_cluster(i));
	  *d = *src;
	}
      else
	{
	  cerr << PHWHERE << "src particle is null ?" << endl;
	}
    }
}

MuPCClusterv1* 
MuPCClusterv1::clone() const
{
  return new MuPCClusterv1(*this);
}

MuPCClusterv1::~MuPCClusterv1()
{
  Cluster->Clear();
  return;
}

MuPCSnglCluster* 
MuPCClusterv1::get_cluster (const unsigned int itrk) const {
  MuPCSnglClusterv1 *Particle = (MuPCSnglClusterv1 *) GetCluster()->UncheckedAt(itrk);
  return Particle;
}


void MuPCClusterv1::identify(std::ostream& os) const
{
  os << "identify yourself: MuPCClusterv1 Object\n"
     << "No of Clusters: " << nCluster << std::endl;
  return;
}

void MuPCClusterv1::Reset()
{
 Cluster->Clear();
 if (nCluster>MUPCCLUSTER)
   {
     Cluster->Expand(MUPCCLUSTER);
   }
 nCluster = 0;
 return;
}

int MuPCClusterv1::isValid() const
{
  return((nCluster>0) ? 1 : 0);
}

int MuPCClusterv1::set_TClonesArraySize(const unsigned int nhits)
{
  if (nhits > MUPCCLUSTER)
    {
      Cluster->Expand(nhits);
     }
  return nhits;
}

void  MuPCClusterv1::AddCluster(const unsigned int itrk)
{
  TClonesArray &Particle = *Cluster;
  new(Particle[itrk]) MuPCSnglClusterv1();
  return;
}

MuPCSnglCluster* 
MuPCClusterv1::AddCluster(const unsigned int itrk,
				const MuPCSnglCluster& track)
{
  const MuPCSnglClusterv1* test = dynamic_cast<const MuPCSnglClusterv1*>
    (&track);

  if (!test)
    {
      cerr << PHWHERE << " track is not of type MuPCSnglClusterv1"
	   << endl;
      return 0;
    }

  return new((*Cluster)[itrk]) MuPCSnglClusterv1(*test);
}


void  MuPCClusterv1::RemoveCluster(const unsigned int itrk)
{
  Cluster->RemoveAt(itrk);
  return;
}


