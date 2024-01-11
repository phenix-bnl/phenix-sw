#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <iostream>

ClassImp(mpcTowerContainer)

using namespace std;

mpcTowerContainer::mpcTowerContainer()
{
}

// Get the un-grayed TDC AMU cell address
short mpcTowerContainer::get_ungray_tdc_amu() const
{
  short ungray_amu = short( get_gray(get_tdc_amu(),-1) );

  return ungray_amu;
}

// Get the un-grayed Pre-Sample AMU cell address
short mpcTowerContainer::get_ungray_pre_amu() const
{
  short ungray_amu = short( get_gray(get_pre_amu(),-1) );

  return ungray_amu;
}

// Get the un-grayed Post-Sample AMU cell address
short mpcTowerContainer::get_ungray_post_amu() const
{
  short ungray_amu = short( get_gray(get_post_amu(),-1) );

  return ungray_amu;
}

unsigned long mpcTowerContainer::get_gray(unsigned long n, int is) const
{
  //For zero or positive values of is, return the Gray code of n; if is is negative, return the inverse Gray code of n.
  // From http://lib-www.lanl.gov/numerical/bookcpdf/c20-2.pdf
  int ish;
  unsigned long ans,idiv;
  if ( is >= 0 ) return n ^ (n >> 1);
  ish=1;
  ans=n;
  for (;;) {
    ans ^= (idiv=ans >> ish);
    if (idiv <= 1 || ish == 16) return ans;
    ish <<= 1;
  }

  return 9999;
}

int mpcTowerContainer::findTower(const int ifeech)
{
  int ntow = size();
  if ( ntow==0 ) return -1;

  for (int itow=0; itow<ntow; itow++)
    {
      mpcTowerContent *tower = getTower( itow );
      if ( tower==0 )
        {
          cout << PHWHERE << " can't find tower " << itow << endl;
          continue;
        }

      int tower_feech = tower->get_ch();
      if ( tower_feech == ifeech )
        {
          return itow;
        }
    }

  return -1;
}

void mpcTowerContainer::scale(const Float_t escale, const Int_t arm)
{
  if ( arm == -1 )
    {
      *this *= escale;
    }
  else if ( arm == 0 )
    {
      int num_lhs = this->size();

      for (int ilhs=0; ilhs<num_lhs; ilhs++)
        {
          mpcTowerContent *lhs_tow = this->getTower(ilhs);
          if ( lhs_tow->get_ch()<0 ) continue;
          if ( lhs_tow->get_ch()>=288 ) continue;
            
          float e = lhs_tow->get_energy();
      
          lhs_tow->set_energy(e*escale);
          // cout << "oldenergy, new energy" << e << ", " << lhs_tow->get_energy() << endl;
        }
    }
  else if ( arm == 1 )
    {
      int num_lhs = this->size();

      for (int ilhs=0; ilhs<num_lhs; ilhs++)
        {
          mpcTowerContent *lhs_tow = this->getTower(ilhs);
          if ( lhs_tow->get_ch()<288 ) continue;
          if ( lhs_tow->get_ch()>575 ) continue;
            
          float e = lhs_tow->get_energy();
      
          lhs_tow->set_energy(e*escale);
          // cout << "oldenergy, new energy" << e << ", " << lhs_tow->get_energy() << endl;
        }
    }
  else
    {
      cout << PHWHERE << " unknown value for arm, " << arm << endl;
    }
}

