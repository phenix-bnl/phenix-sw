#ifndef __ZDCGEAHITSV1_H__
#define __ZDCGEAHITSV1_H__

#include "ZdcGeaHits.h"
#include "phool.h"
#include <iostream>

class ZdcGeaHitsv1 : public ZdcGeaHits
{
public:
  ZdcGeaHitsv1();
  virtual ~ZdcGeaHitsv1() {}

  // accessor methods
  unsigned int get_nhits() { return nzdcgeahits; }
  ZdcPISAHit *get_ZdcPISAHits(unsigned int ihit = 0) {
    if (ihit>=nzdcgeahits )
      {
	std::cout << PHWHERE << "ZdcPISAHit index " << ihit << " out of range" << std::endl;
        return 0;
      }
    if (!zdcgeahits)
      {
	std::cout << PHWHERE << "zdcgeahits = 0" << std::endl;
        return 0;
      }
    return (zdcgeahits + ihit);
  }

  // set methods
  void set_nhits(unsigned int n) { nzdcgeahits = n; }
  void set_ZdcPISAHits( ZdcPISAHit *hitsarray ) { zdcgeahits = hitsarray; }

  void identify(std::ostream& os = std::cout) const;
  int  isValid() const { if (nzdcgeahits>0 && zdcgeahits) return 1; return 0; }
  void Reset();

protected:
  unsigned int nzdcgeahits;	// number of pisa hits in ZDC
  ZdcPISAHit *zdcgeahits;	// pointer to array of hits

  ClassDef(ZdcGeaHitsv1,0)  // A Zdc hit instance
};

#endif

