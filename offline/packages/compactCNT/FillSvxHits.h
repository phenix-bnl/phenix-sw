#ifndef __FILLSVXHITS_H__
#define __FILLSVXHITS_H__

#include "dumpflag.h"
#include "setIntflag.h"

#include <SubsysReco.h>
#include <fstream>
#include <cmath>
#include <iostream>

class FillSvxHits: public SubsysReco
{
 public:
   FillSvxHits(const std::string &name = "FILLSVXHITS");
   virtual ~FillSvxHits() {}

   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);

   void setSaveOnlyAssociatedHits(bool a) {SaveOnlyAssociatedHits=a;}
   void setSaveOnlySelectedHits(bool a) {SaveOnlySelectedHits=a;}

   void setSaveHitsToAllNode(bool flag) {m_saveHitsToAllNode=flag; }

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

 protected:

#ifdef useIntflag
  int FloatToInt(const float rval) const;
#else
  short int FloatToInt(const float rval) const;
#endif

#ifdef DUMP
  std::ofstream dumpfile;
#endif

  bool SaveOnlyAssociatedHits;
  bool SaveOnlySelectedHits;
  bool m_saveHitsToAllNode;
};

#endif


