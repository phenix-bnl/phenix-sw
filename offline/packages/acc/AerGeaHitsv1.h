#ifndef __AERGEAHITSV1_H__
#define __AERGEAHITSV1_H__

#include "AerGeaHits.h"
#include "phool.h"

class AerGeaHitsv1 : public AerGeaHits
{
public:
  AerGeaHitsv1();
  virtual ~AerGeaHitsv1() {}

  // accessor methods
  unsigned int get_nhits(){return naergeahits; }

  AerPISAHit *get_AerPISAHits(unsigned int ihit = 0) {
    if (ihit>=naergeahits )
      {
        std::cout << PHWHERE << "AerPISAHit index " << ihit << " out of range" << std::endl;
        return 0;
      }
    if (!aergeahits)
      {
        std::cout << PHWHERE << "aergeahits = 0" << std::endl;
        return 0;
      }
    return (aergeahits + ihit);
  }

  // set methods
  void set_nhits(unsigned int n) { naergeahits = n; }
  void set_AerPISAHits( AerPISAHit *hitsarray ) { aergeahits = hitsarray; }

  void identify(std::ostream& os) const;
  int  isValid() const { if (naergeahits>0 && aergeahits) return 1; return 0; }
  void Reset();

protected:
  unsigned int naergeahits;	// number of pisa hits in AER
  AerPISAHit *aergeahits;	// pointer to array of hits

  ClassDef(AerGeaHitsv1,0)  // A Aer hit instance
};

#endif
