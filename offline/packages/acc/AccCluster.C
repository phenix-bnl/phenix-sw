#include "AccCluster.h"
#include "phool.h"
#include <iostream>

ClassImp(AccCluster)

using namespace std;

 void 
AccCluster::set_ncluster(const unsigned int NTRACK) 
    {
      cout << "AccCluster::Error get_npart not overridden" << endl;
      return;
    }

int  
AccCluster::get_ncluster() const
    {
      cout << "AccCluster::Error get_npart not overridden" << endl;
      return 0;
    }

 AccSnglCluster*
AccCluster::get_cluster(const unsigned int itrk) const

    {
      cout << "Single Track return not implemented for your version of tracks" << endl;
      return NULL;
    }


AccCluster*
AccCluster::clone() const

    {
      cout << "Clone method not implemented for your version of CentralTracks" << endl;
      return 0;
    }

void
AccCluster::Reset()
{
    cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
    return;
  }


int
AccCluster::isValid() const
 {
    cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
    return 0;
  }

void
AccCluster::identify(ostream &os) const
 {
    os << "identify yourself: virtual AccCluster object" << endl;
    return;
  }
