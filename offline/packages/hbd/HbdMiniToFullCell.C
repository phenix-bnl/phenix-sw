//
// Wrapper class to produce HbdCellList from HbdMiniCellList
//   T. Sakaguchi, July 25, 2007
//
#include "HbdMiniToFullCell.h"

#include "HbdCellListv1.h"
#include "HbdCellv1.h"
#include "HbdMiniCellListv1.h"
#include "HbdMiniCellv1.h"

#include "hbdDetectorGeo.hh"
#include "hbdAdcCalib.hh"

#include "PHCompositeNode.h"
#include "PHIODataNode.h"

#include "phool.h"
#include "getClass.h"
#include "recoConsts.h"

#include "TrigLvl1.h"

#include <cstdlib>

#include "Rtypes.h"

using namespace std;

typedef PHIODataNode <HbdCellList> HbdCellListNode_t;
typedef PHIODataNode <HbdMiniCellList> HbdMiniCellListNode_t;

HbdCellList *d_cell_list;
HbdCellListNode_t *cell_node;

HbdMiniToFullCell::HbdMiniToFullCell(const string &name): SubsysReco(name)
{
//  baseclasses.insert("PHCentralTrack");
//  baseclasses.insert("HbdMiniCellList");
  calib = 0;
}

HbdMiniToFullCell::~HbdMiniToFullCell()
{
  delete calib;
//  delete d_cell_list;
//  delete cell_node;
  return;
}

int HbdMiniToFullCell::InitRun(PHCompositeNode *topNode)
{
   recoConsts *rc = recoConsts::instance();
   run = rc->get_IntFlag("RUNNUMBER");

   //
   // From Run10, we use three sample format, and therefore,
   // the division factor will be 2, instead of 3
   //
   if(run>=298917) ConvFac=2.0;
   else ConvFac=3.0;

   if(rc->FlagExist("compactCNTRUN")&&(rc->get_IntFlag("compactCNTRUN")>0))
      run = rc->get_IntFlag("compactCNTRUN");

   hbdDetectorGeo t;
   t.fetchPad(run);
   
   for(int i=0;i<2304;i++){
      int seqsec,padid,arm,sec;
      t.getPadInfo(i,arm,sec,seqsec,padid);
      SeqSec[i] = seqsec;
      PadId[i] = padid-1; //pad numbers go from 0-191
   }

   // fetching ADC calibrations
   calib = new hbdAdcCalib();
   
   calib->fetch(run);

   PHNodeIterator iter(topNode);
   PHCompositeNode *dstNode;

   dstNode = dynamic_cast<PHCompositeNode*>(iter.findFirst("PHCompositeNode", "DST"));

   if (!dstNode) {
      cout << PHWHERE << " Could not find DST node" << endl;
      return -1;
   }

   //
   // HbdCell Container
   //
   //HbdCellList *d_cell;

   //
   // If There is CellList, Something is WRONG!
   //
   CellList = 0;
   CellList = findNode::getClass<HbdCellList>(topNode, "HbdCellList");
   if (!CellList){
      d_cell_list = new HbdCellListv1();
      cell_node = new PHIODataNode<HbdCellList>(d_cell_list, "HbdCellList","PHObject");
      dstNode->addNode(cell_node);
   }
/*
   else{
      cout << PHWHERE << "HbdMiniToFullCell:: HbdCellList is already there. Funny!" << endl;
      return -1;
   }
*/

   nevent = 0;
   return 0;
}

int HbdMiniToFullCell::process_event(PHCompositeNode *topNode)
{

   if (verbosity > 0){
      if(nevent%1000==0) cout << "Event: " << nevent << endl;
   }

   //
   // If There is no MiniCellList, Something is WRONG!
   //
   MiniCellList = findNode::getClass<HbdMiniCellList>(topNode, "HbdMiniCellList");
   if (!MiniCellList){
      cout << PHWHERE << "HbdMiniToFullCell:: No HbdMiniCellList. Something is wrong!" << endl;
      return -1;
   }

   CellList = findNode::getClass<HbdCellList>(topNode, "HbdCellList");
   if (!CellList){
      cout << PHWHERE << "HbdMiniToFullCell:: No CellList. Something is wrong!" << endl;
      return -1;
   }

   // beam clock tick for time dependent calibration
   TrigLvl1* d_trg = findNode::getClass<TrigLvl1>(topNode,"TrigLvl1");
   calib->setClockTick(d_trg->get_lvl1_beam_clk(1),d_trg->get_lvl1_beam_clk(0));

   //
   // Now, transfer MiniCell to FullCell
   //
   int nMiniCells = MiniCellList->get_nCells();

   const char *secname[2][2][6]={ { {"East South 0","East South 1",
                               "East South 2","East South 3",
                               "East South 4","East South 5",},
                              {"East North 0","East North 1",
                               "East North 2","East North 3",
                               "East North 4","East North 5",} },
                            { {"West South 0","West South 1",
                               "West South 2","West South 3",
                               "West South 4","West South 5",},
                              {"West North 0","West North 1",
                               "West North 2","West North 3",
                               "West North 4","West North 5",} } };

   for(int i=0;i<nMiniCells;i++){
      short charge3, adcch;
      float charge;
      int side, arm;
      int padid, sector;

      charge3 = MiniCellList->get_cell(i)->get_charge();
      adcch = MiniCellList->get_cell(i)->get_adcch();

      CellList->AddCell(i);
      CellList->set_nCells(i+1);
      HbdCell *newcell = CellList->get_cell(i);

      padid  = PadId[(int)adcch];
      sector = SeqSec[(int)adcch];
      newcell->set_padnum(padid);
      newcell->set_sector(sector);

      if (sector < 6  && padid <= 95)
	{
	  side = 0;  //East South
	  arm = 0;
	}
      else if (sector >= 6 && padid <= 95)
	{
	  side = 1;  //West North
	  arm = 1;
	}
      else if (sector<6  &&  padid>95)
	{
	  side = 1;  //East North
	  arm = 0;
	}
      else if (sector >= 6 &&  padid > 95)
	{
	  side = 0;  //West South
	  arm = 1;
	}
      else
	{
	  cout << PHWHERE << " bad sector " << sector << " or padid: "
	       << padid << endl;
	  exit(1);
	}
      int locsec;
      if (sector >= 6)
	{
	  locsec = sector - 6;
	}
      else
	{
	  locsec = sector;
	}

      newcell->set_arm(arm);
      newcell->set_side(side);

      newcell->set_secchar(secname[arm][side][locsec]);

      // Apply charge calibration
      calib->ApplyCalib((int)adcch,(int)(((float)charge3)/ConvFac),charge);

      //std::cout<<"In HbdMiniCell, " << adcch<< ", : " << charge << std::endl; 
      newcell->set_charge(charge);
   }

   nevent++;

   return 0;
}
