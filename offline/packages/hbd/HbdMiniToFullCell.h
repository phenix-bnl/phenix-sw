#ifndef __HBDMINITOFULLCELL_H__
#define __HBDMINITOFULLCELL_H__

//
// Wrapper class to produce HbdCell from HbdMiniCell
//   T. Sakaguchi, July 25, 2007
//  Now reformed to be a part of recalibrator
//   by TS, Jan 29, 2009
//

#include <SubsysReco.h>

class PHCompositeNode;
class HbdCellList;
class HbdMiniCellList;
class hbdAdcCalib;
class TrigLvl1;

class HbdMiniToFullCell : public SubsysReco
{

 public:
   HbdMiniToFullCell(const std::string &name="HbdMiniToFullCell");
   virtual ~HbdMiniToFullCell();
   int InitRun(PHCompositeNode *topNode);
   int process_event(PHCompositeNode *topNode);

 protected:
   // Node
   HbdMiniCellList *MiniCellList;
   HbdCellList *CellList;
   hbdAdcCalib *calib;

 private:
   int PadId[2304];
   int SeqSec[2304];
   int nevent;
   float ConvFac;
   int run;
};

#endif /* __HBDMINITOFULLCELL_H__ */

