#include "MuPCCluster.h"
#include "phool.h"
#include <iostream>

ClassImp(MuPCCluster)

using namespace std;

 void 
MuPCCluster::set_ncluster(const unsigned int NTRACK) 
    {
      cout << "MuPCCluster::Error get_npart not overridden" << endl;
      return;
    }

int  
MuPCCluster::get_ncluster() const
    {
      cout << "MuPCCluster::Error get_npart not overridden" << endl;
      return 0;
    }

 MuPCSnglCluster*
MuPCCluster::get_cluster(const unsigned int itrk) const

    {
      cout << "Single Track return not implemented for your version of tracks" << endl;
      return NULL;
    }


MuPCCluster*
MuPCCluster::clone() const

    {
      cout << "Clone method not implemented for your version of CentralTracks" << endl;
      return 0;
    }

void
MuPCCluster::Reset()
{
    cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
    return;
  }


int
MuPCCluster::isValid() const
 {
    cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
    return 0;
  }

void
MuPCCluster::identify(ostream &os) const
 {
    os << "identify yourself: virtual MuPCCluster object" << endl;
    return;
  }
