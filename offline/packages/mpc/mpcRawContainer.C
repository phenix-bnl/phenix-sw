#include <iostream>
#include "mpcRawContainer.h"
#include <mpcRawContent.h>

ClassImp(mpcRawContainer)

mpcRawContainer::mpcRawContainer()
{
}

// Get the un-grayed TDC AMU cell address
short mpcRawContainer::get_ungray_tdc_amu() const
{
  short ungray_amu = short( get_gray(get_tdc_amu(),-1) );

  return ungray_amu;
}

// Get the un-grayed Pre-Sample AMU cell address
short mpcRawContainer::get_ungray_pre_amu() const
{ 
  short ungray_amu = short( get_gray(get_pre_amu(),-1) );

  return ungray_amu;
}

// Get the un-grayed Post-Sample AMU cell address
short mpcRawContainer::get_ungray_post_amu() const
{
  short ungray_amu = short( get_gray(get_post_amu(),-1) );

  return ungray_amu;
}

unsigned long mpcRawContainer::get_gray(unsigned long n, int is) const
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

int mpcRawContainer::findTower(const int ifeech) const
{
  int ntow = size();
  if ( ntow==0 ) return -1;

  for (int itow=0; itow<ntow; itow++)
    {
      mpcRawContent *tower = getTower( itow );
      if ( tower==0 )
        {
          std::cout << PHWHERE << " can't find tower " << itow << std::endl;
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

