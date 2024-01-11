#ifndef __FILLSVXRAWHITS_H__
#define __FILLSVXRAWHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>
#include <vector>
//#include <cmath>
//#include <iostream>

class FillSvxRawHits: public SubsysReco
{
 public:
   FillSvxRawHits(const std::string &name = "FILLSVXRAWHITS");
   virtual ~FillSvxRawHits() {}

   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);

/*
   float check_range(float val, float min_rng, float max_rng){
     if(std::isnan(val)){
       //std::cout<<"val is NAN : "<<val<<std::endl;
       return min_rng;
     }

     float newval=val;
     if(max_rng<val) newval=max_rng;
     if(val<min_rng) newval=min_rng;
     return newval;
   }
*/

 protected:

#ifdef useIntflag
  int FloatToInt(const float rval) const;
#else
  short int FloatToInt(const float rval) const;
#endif

#ifdef useIntflag
  void fillChipRawhit(int module, std::vector<int>& vChipHit, std::vector<int>& savethis);
#else
  void fillChipRawhit(int module, std::vector<short int>& vChipHit, std::vector<short int>& savethis);
#endif

#ifdef DUMP
  std::ofstream dumpfile;
#endif
};

#endif


