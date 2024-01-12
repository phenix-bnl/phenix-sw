#ifndef __HBDRECALRECO_H__
#define __HBDRECALRECO_H__

//
// Wrapper class for Mini To Full Cell conversion and clusterizer
//   T. Sakaguchi, Jul 12, 2009
//

#include <Recalibrator.h>
#include <HbdWisClusterizer.h>
#include <HbdMinPadClusterizer.h>

class PHCompositeNode;
class HbdMiniToFullCell;
class hbdAdcCalib;
class HbdWisClusterizer;
class HbdMinPadClusterizer;
class HbdCellListAnalyzer;
class V24maker;

class HbdRecalReco : public Recalibrator
{

 public:
   HbdRecalReco(const std::string &name="HbdRecalReco");
   virtual ~HbdRecalReco();
   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);
   int End(PHCompositeNode *topNode);
   int isValidRun(const int runno) const;
   void Verbosity(int verbose) {verbosity = verbose;}

 protected:
   HbdMiniToFullCell *hbdconv; 
   HbdWisClusterizer wisclusters; 
   HbdMinPadClusterizer minpadclusters; 
   V24maker * v24maker;
   HbdCellListAnalyzer *cana;
   hbdAdcCalib *calib;
   int skip_flag; 
   int clst_flag;
   int verbosity;
   int mc_flag;
};

#endif /* __HBDRECALRECO_H__ */

