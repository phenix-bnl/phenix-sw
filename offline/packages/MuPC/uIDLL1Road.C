#include "uIDLL1Road.h"
#include "phool.h"
#include <iostream>

ClassImp(uIDLL1Road)

using namespace std;

 void 
uIDLL1Road::set_nRoad_deep(const unsigned int NTRACK) 
    {
      cout << "uIDLL1Road::Error get_npart not overridden" << endl;
      return;
    }

int  
uIDLL1Road::get_nRoad_deep() const
    {
      cout << "uIDLL1Road::Error get_npart not overridden" << endl;
      return 0;
    }

 uIDLL1SnglRoad*
uIDLL1Road::get_deep_road(const unsigned int itrk) const

    {
      cout << "Single Track return not implemented for your version of tracks" << endl;
      return NULL;
    }


uIDLL1Road*
uIDLL1Road::clone() const

    {
      cout << "Clone method not implemented for your version of CentralTracks" << endl;
      return 0;
    }

void
uIDLL1Road::Reset()
{
    cout << PHWHERE << "ERROR: Reset() not implemented by daughter function" << endl;
    return;
  }


int
uIDLL1Road::isValid() const
 {
    cout << PHWHERE << "isValid() not implemented by daughter function" << endl;
    return 0;
  }

void
uIDLL1Road::identify(ostream &os) const
 {
    os << "identify yourself: virtual uIDLL1Road object" << endl;
    return;
  }
