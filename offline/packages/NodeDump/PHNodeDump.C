#include <getClass.h>

#include <EventHeader.h>
#include <RunHeader.h>

#include "PHNodeDump.h"
#include "DumpObject.h"

#include "DumpAccCluster.h"
#include "DumpAccHit.h"
#include "DumpAccRaw.h"
#include "DumpAerGeaHits.h"
#include "Dumpbbcghit.h"
#include "DumpBbcOut.h"
#include "DumpBbcRaw.h"
#include "DumpCglTrack.h"
#include "DumpCrkHit.h"
#include "Dumpcrkghit.h"
#include "DumpCrkProj.h"
#include "DumpCrkRing.h"
#include "DumpdBbcGhitRaw.h"
#include "DumpdBbcRaw.h"
#include "Dumpdcghit.h"
#include "DumpDchHitLineTable.h"
#include "DumpDchTrack.h"
#include "DumpdCrkHit.h"
#include "DumpdCrkRel2s.h"
#include "DumpdDchGhitHits.h"
#include "DumpdDchHit.h"
#include "DumpdDchNibbleGhit.h"
#include "DumpdDchTracks.h"
#include "DumpdDchTracksExt.h"
#include "DumpdEmcCalibTower.h"
#include "DumpdEmcClusterLocalExt.h"
#include "DumpdEmcGeaClusterTrack.h"
#include "DumpdEmcGeaHit.h"
#include "DumpdEmcGeaTowerTrack.h"
#include "DumpdEmcGeaTrack.h"
#include "DumpdEmcGeaTrackCluster.h"
#include "DumpdEmcGeaTrackTower.h"
#include "DumpDetectorGeometry.h"
#include "DumpdPadCluster.h"
#include "DumpdPadGhitClus.h"
#include "DumpdPadGhitRaw.h"
#include "DumpdPadNibbleGhit.h"
#include "DumpdPadRaw.h"
#include "DumpdPHDchTrack.h"
#include "DumpdTecGhitRaw.h"
#include "DumpdTofFEMhitGhit.h"
#include "DumpdTofGdigi.h"
#include "DumpdTofGhitGdigi.h"
#include "DumpdTofGdigiRec.h"
#include "DumpdTofGhitRaw.h"
#include "DumpdTofRaw.h"
#include "DumpdTofRawRec.h"
#include "DumpdTofReconstructed.h"
#include "DumpemcClusterAuxInfoContainer.h"
#include "DumpemcClusterContainer.h"
#include "DumpemcTowerContainer.h"
#include "DumpEventHeader.h"
#include "DumpErtOut.h"
#include "DumpFclOut.h"
#include "DumpFclRaw.h"
#include "Dumpfkin.h"
#include "DumpFlags.h"
#include "DumpHbdBlobList.h"
#include "DumpHbdCellList.h"
#include "DumpHbdMiniCellList.h"
#include "DumpHbdRawList.h"
#include "Dumpheader.h"
//#include "DumpKalFitOut.h"
#include "DumplpcRaw.h"
#include "DumpLvl2DecisionOut.h"
#include "DumpLvl2OutArray.h"
#include "DumpMcEvalSingleList.h"
#include "DumpmpcClusterContainer.h"
#include "DumpMpcExEventHeader.h"
#include "DumpMpcExRawHit.h"
#include "DumpmpcRawContainer.h"
#include "DumpmpcSampleContainer.h"
#include "DumpmpcTowerContainer.h"
#include "DumpMutrgHeaderArray.h"
#include "DumpMutrgHitArray.h"
#include "DumpMutrgTrkArray.h"
#include "DumpMvdRPhiZOut.h"
#include "DumpPadCluster.h"
#include "DumpPadRaw.h"
#include "Dumppcghit.h"
#include "DumpPdbCalBankSave.h"
#include "DumpPHCentralTrack.h"
#include "DumpPHCentralTrackv24.h"
#include "DumpPHDchTrackOut.h"
#include "DumpPHGlobal.h"
#include "DumpPHIOArray.h"
#include "DumpPHMuoTracksAdc.h"
#include "DumpPHMuoTracksOut.h"
#include "DumpPHTrackOut.h"
#include "Dumpprimary.h"
#include "DumpPreviousEvent.h"
#include "Dumppythia.h"
#include "DumpRpSumXYObject.h"
#include "DumpRunHeader.h"
#include "DumpSmdOut.h"
#include "DumpSpinDataEventOut.h"
#include "DumpSyncObject.h"
#include "DumpSvxCentralTrackList.h"
#include "DumpSvxClusterList.h"
#include "DumpSvxEventInfo.h"
#include "DumpSvxQAInfo.h"
#include "DumpSvxPixelRawHitList.h"
#include "DumpSvxRawhitClusterList.h"
#include "DumpSvxRawhitList.h"
#include "DumpSvxSegmentList.h"
#include "DumpT0Out.h"
#include "DumpTecClusterContainer.h"
#include "DumpTecOut.h"
#include "Dumptecghit.h"
#include "Dumptofghit.h"
#include "DumpTofOut.h"
#include "DumpTofwHit.h"
#include "DumpTofwRaw.h"
#include "DumpTrigLvl1.h"
#include "DumpTrigRunLvl1.h"
#include "DumpTrigRunLvl2.h"
#include "DumpVariableArray.h"
#include "DumpVariableArrayInt.h"
#include "DumpVtxOut.h"
#include "DumpZdcOut.h"
#include "DumpZdcRaw.h"

#include <TFvtxClus.h>
#include <TFvtxCompactTrk.h>
#include <TFvtxCoord.h>
#include <TFvtxHit.h>
#include <TFvtxResidual.h>
#include <TFvtxStraightTrkPar.h>
#include <TFvtxSvxCluster.h>
#include <TFvtxTrk.h>
#include <TMCPrimary.hh>
#include <TMuiMCHitO.h>
#include <TMuiRoadO.h>
#include <TMui1DRoadO.h>
#include <TMuiClusterO.h>
#include <TMuiHitO.h>
#include <TMuiPseudoLL1.h>
#include <TMutClus.hh>
#include <TMutCoord.hh>
#include <TMutGapCoord_v1.hh>
#include <TMutHit_v1.hh>
#include <TMutMCHit.hh>
#include <TMutMCTrk.hh>
#include <TMutStub_v1.hh>
#include <TMutTrk.hh>
#include <TMutVtx_v1.hh>

#include <TRpcClus.h>
#include <TRpcCoord.h>
#include <TRpcHit.h>
#include <TRpcTrk.h>

#include <TRxnpRawScint.h>
#include <TRxnpRawXang.h>
#include <TRxnpScint.h>

#include <string>

using namespace std;



PHNodeDump::PHNodeDump()
{
  runnumber = -9999;
  evtsequence = -9999;
  fp_precision = -1;
  outdir = "./";
}

PHNodeDump::~PHNodeDump()
{
  ignore.clear();
  exclusive.clear();
  while (dumpthis.begin() != dumpthis.end())
    {
      delete dumpthis.begin()->second;
      dumpthis.erase(dumpthis.begin());
    }
  return;
}

int
PHNodeDump::AddIgnore(const string &name)
{
  if (ignore.find(name) != ignore.end())
    {
      cout << PHWHERE << " "
           << name << "already in ignore list" << endl;
      return -1;
    }
  ignore.insert(name);
  return 0;
}

int
PHNodeDump::Select(const string  &name)
{
  if (exclusive.find(name) != exclusive.end())
    {
      cout << PHWHERE << " "
           << name << "already in exclusive list" << endl;
      return -1;
    }
  exclusive.insert(name);
  return 0;
}

int PHNodeDump::GetGlobalVars(PHCompositeNode *topNode)
{
  RunHeader *runheader = findNode::getClass<RunHeader>(topNode, "RunHeader");
  if (runheader)
    {
      runnumber = runheader->get_RunNumber();
    }
  EventHeader* eventheader = findNode::getClass<EventHeader>(topNode, "EventHeader");
  if (eventheader)
    {
      evtsequence = eventheader->get_EvtSequence();
    }
  return 0;
}

void PHNodeDump::perform(PHNode* node)
{
  map <string, DumpObject *>::iterator iter;
  if (node->getType() == "PHIODataNode")
    {
      string NodeName = node->getName().getString();
      iter = dumpthis.find(NodeName);
      if (iter == dumpthis.end())
        {
          cout << "Adding Dump Object for " << NodeName << endl;
          AddDumpObject(NodeName, node);
          iter = dumpthis.find(NodeName); // update iterator
        }

      if (iter != dumpthis.end())
        {
          iter->second->process_event(node);
        }
      else
        {
          //           for (iter = dumpthis.begin(); iter != dumpthis.end(); iter++)
          //             {
          //               cout << "registered: " << iter->second->Name() << endl;
          //             }
          cout << "Something went wrong with adding Dump Object for " << NodeName
               << ", it should exist !! Trying to create it again" << endl;
          AddDumpObject(NodeName, node);
        }

    }
  return ;
}

int PHNodeDump::CloseOutputFiles()
{
  map <string, DumpObject *>::iterator iter;
  for (iter = dumpthis.begin(); iter != dumpthis.end(); iter++)
    {
      iter->second->CloseOutputFile();
    }
  return 0;
}

int PHNodeDump::AddDumpObject(const string &NodeName, PHNode *node)
{
  DumpObject *newdump;
  string newnode = NodeName;
  if (!exclusive.empty())
    {
      if (exclusive.find(NodeName) == exclusive.end())
        {
          cout << "Exclusive find: Ignoring " << NodeName << endl;
          newdump = new DumpObject(NodeName);
	  newdump->NoOutput();
          goto initdump;
        }
    }
  if (ignore.find(NodeName) != ignore.end())
    {
      cout << "Ignoring " << NodeName << endl;
      newdump = new DumpObject(NodeName);
    }
  else if (NodeName == "AccCluster")
    {
      newdump = new DumpAccCluster(NodeName);
    }
  else if (NodeName == "AccHit")
    {
      newdump = new DumpAccHit(NodeName);
    }
  else if (NodeName == "AccRaw")
    {
      newdump = new DumpAccRaw(NodeName);
    }
  else if (NodeName == "bbcghit")
    {
      newdump = new Dumpbbcghit(NodeName);
    }
  else if (NodeName == "BbcOut")
    {
      newdump = new DumpBbcOut(NodeName);
    }
  else if (NodeName == "BbcRaw")
    {
      newdump = new DumpBbcRaw(NodeName);
    }
  else if (NodeName == "dBbcRaw")
    {
      newdump = new DumpdBbcRaw(NodeName);
    }
  else if (NodeName == "dcghit")
    {
      newdump = new Dumpdcghit(NodeName);
    }
  else if (NodeName == "DchTrack")
    {
      newdump = new DumpDchTrack(NodeName);
    }
  else if (NodeName == "dCrkHit")
    {
      newdump = new DumpdCrkHit(NodeName);
    }
  else if (NodeName == "dDchHit")
    {
      newdump = new DumpdDchHit(NodeName);
    }
  else if (NodeName == "dDchTracks" || NodeName == "dDchTracksPerf")
    {
      newdump = new DumpdDchTracks(NodeName);
    }
  else if (NodeName == "dDchTracksExt" || NodeName == "dDchTracksExtPerf")
    {
      newdump = new DumpdDchTracksExt(NodeName);
    }
  else if (NodeName == "dEmcClusterLocalExt")
    {
      newdump = new DumpdEmcClusterLocalExt(NodeName);
    }
  else if (NodeName == "dEmcCalibTower")
    {
      newdump = new DumpdEmcCalibTower(NodeName);
    }
  else if (NodeName == "dEmcGeaClusterTrack")
    {
      newdump = new DumpdEmcGeaClusterTrack(NodeName);
    }
  else if (NodeName == "dEmcGeaTrack")
    {
      newdump = new DumpdEmcGeaTrack(NodeName);
    }
  else if (NodeName == "dEmcGeaTrackCluster")
    {
      newdump = new DumpdEmcGeaTrackCluster(NodeName);
    }
  else if (NodeName == "dEmcGeaTowerTrack")
    {
      newdump = new DumpdEmcGeaTowerTrack(NodeName);
    }
  else if (NodeName == "dEmcGeaTrackTower")
    {
      newdump = new DumpdEmcGeaTrackTower(NodeName);
    }
  else if (NodeName == "DetectorGeometry")
    {
      newdump = new DumpDetectorGeometry(NodeName);
    }
  else if (NodeName == "dPc1Cluster" || NodeName == "dPc2Cluster" || NodeName == "dPc3Cluster")
    {
      newdump = new DumpdPadCluster(NodeName);
    }
  else if (NodeName == "dPc1GhitClus" || NodeName == "dPc2GhitClus" || NodeName == "dPc3GhitClus")
    {
      newdump = new DumpdPadGhitClus(NodeName);
    }
  else if (NodeName == "dPc1GhitRaw" || NodeName == "dPc2GhitRaw" || NodeName == "dPc3GhitRaw")
    {
      newdump = new DumpdPadGhitRaw(NodeName);
    }
  else if (NodeName == "dPc1NibbleGhit" || NodeName == "dPc2NibbleGhit" || NodeName == "dPc3NibbleGhit")
    {
      newdump = new DumpdPadNibbleGhit(NodeName);
    }
  else if (NodeName == "dPc1Raw")
    {
      newdump = new DumpdPadRaw(NodeName);
    }
  else if (NodeName == "dPc2Raw")
    {
      newdump = new DumpdPadRaw(NodeName);
    }
  else if (NodeName == "dPc3Raw")
    {
      newdump = new DumpdPadRaw(NodeName);
    }
  else if (NodeName == "dPHDchTrack")
    {
      newdump = new DumpdPHDchTrack(NodeName);
    }
  else if (NodeName == "dTofFEMhitGhit")
    {
      newdump = new DumpdTofFEMhitGhit(NodeName);
    }
  else if (NodeName == "dTofGdigi")
    {
      newdump = new DumpdTofGdigi(NodeName);
    }
  else if (NodeName == "dTofGhitGdigi")
    {
      newdump = new DumpdTofGhitGdigi(NodeName);
    }
  else if (NodeName == "dTofGdigiRec")
    {
      newdump = new DumpdTofGdigiRec(NodeName);
    }
  else if (NodeName == "dTofGhitRaw")
    {
      newdump = new DumpdTofGhitRaw(NodeName);
    }
  else if (NodeName == "dTofRaw")
    {
      newdump = new DumpdTofRaw(NodeName);
    }
  else if (NodeName == "dTofRawRec")
    {
      newdump = new DumpdTofRawRec(NodeName);
    }
  else if (NodeName == "dTofReconstructed")
    {
      newdump = new DumpdTofReconstructed(NodeName);
    }
  else if (NodeName == "ErtOut")
    {
      newdump = new DumpErtOut(NodeName);
    }
  else if (NodeName == "EventHeader")
    {
      newdump = new DumpEventHeader(NodeName);
    }
  else if (NodeName == "fclOutNorth")
    {
      newdump = new DumpFclOut(NodeName);
    }
  else if (NodeName == "fclOutSouth")
    {
      newdump = new DumpFclOut(NodeName);
    }
  else if (NodeName == "fclRawNorth")
    {
      newdump = new DumpFclRaw(NodeName);
    }
  else if (NodeName == "fclRawSouth")
    {
      newdump = new DumpFclRaw(NodeName);
    }
  else if (NodeName == "fkin")
    {
      newdump = new Dumpfkin(NodeName);
    }
  else if (NodeName == "Flags")
    {
      newdump = new DumpFlags(NodeName);
    }
  else if (NodeName == "header")
    {
      newdump = new Dumpheader(NodeName);
    }
  // else if (NodeName == "KalFitOut")
  //   {
  //     newdump = new DumpKalFitOut(NodeName);
  //   }
  else if (NodeName == "MvdRPhiZOut")
    {
      newdump = new DumpMvdRPhiZOut(NodeName);
    }
  else if (NodeName == "pc1ghit" || NodeName == "pc2ghit" || NodeName == "pc3ghit")
    {
      newdump = new Dumppcghit(NodeName);
    }
  else if (NodeName == "PHDchTrackOut")
    {
      newdump = new DumpPHDchTrackOut(NodeName);
    }
  else if (NodeName == "PHGlobal" || NodeName == "PhEvtHeader")
    {
      newdump = new DumpPHGlobal(NodeName);
    }
  else if (NodeName == "PHMuoTracksOO")
    {
      newdump = new DumpPHMuoTracksOut(NodeName);
    }
  else if (NodeName == "primary")
    {
      newdump = new Dumpprimary(NodeName);
    }
  else if (NodeName == "PreviousEvent")
    {
      newdump = new DumpPreviousEvent(NodeName);
    }
  else if (NodeName == "pythia")
    {
      newdump = new Dumppythia(NodeName);
    }
  else if (NodeName == "SmdOut")
    {
      newdump = new DumpSmdOut(NodeName);
    }
  else if (NodeName == "SpinDataEventOut")
    {
      newdump = new DumpSpinDataEventOut(NodeName);
    }
  else if (NodeName == "Sync")
    {
      newdump = new DumpSyncObject(NodeName);
    }
  else if (NodeName == "T0Out")
    {
      newdump = new DumpT0Out(NodeName);
    }
  else if (NodeName == "TMCPrimary")
    {
      newdump = new DumpPHIOArray<TMCPrimary>(NodeName);
    }
  else if (NodeName == "TMuiMCHitO")
    {
      newdump = new DumpPHIOArray<TMuiMCHitO>(NodeName);
    }
  else if (NodeName == "TFvtxClus")
    {
       newdump = new DumpPHIOArray<TFvtxClus>(NodeName);
    }
  else if (NodeName == "TFvtxCompactTrk")
    {
       newdump = new DumpPHIOArray<TFvtxCompactTrk>(NodeName);
    }
  else if (NodeName == "TFvtxCoord")
    {
       newdump = new DumpPHIOArray<TFvtxCoord>(NodeName);
    }
  else if (NodeName == "TFvtxHit")
    {
       newdump = new DumpPHIOArray<TFvtxHit>(NodeName);
    }
  else if (NodeName == "TFvtxResidual")
    {
       newdump = new DumpPHIOArray<TFvtxResidual>(NodeName);
    }
  else if (NodeName == "TFvtxTrk")
    {
       newdump = new DumpPHIOArray<TFvtxTrk>(NodeName);
    }
  else if (NodeName == "TFvtxStraightTrkPar")
    {
       newdump = new DumpPHIOArray<TFvtxStraightTrkPar>(NodeName);
    }
  else if (NodeName == "TFvtxSvxCluster")
    {
       newdump = new DumpPHIOArray<TFvtxSvxCluster>(NodeName);
    }
  else if (NodeName == "TMuiRoadO")
    {
      newdump = new DumpPHIOArray<TMuiRoadO>(NodeName);
    }
  else if (NodeName == "TMui1DRoadO")
    {
      newdump = new DumpPHIOArray<TMui1DRoadO>(NodeName);
    }
  else if (NodeName == "TMuiClusterO")
    {
      newdump = new DumpPHIOArray<TMuiClusterO>(NodeName);
    }
  else if (NodeName == "TMuiHitO")
    {
      newdump = new DumpPHIOArray<TMuiHitO>(NodeName);
    }
  else if (NodeName == "TMuiPseudoLL1")
    {
      newdump = new DumpPHIOArray<TMuiPseudoLL1>(NodeName);
    }
  else if (NodeName == "TMutClus")
    {
      newdump = new DumpPHIOArray<TMutClus>(NodeName);
    }
  else if (NodeName == "TMutCoord")
    {
      newdump = new DumpPHIOArray<TMutCoord>(NodeName);
    }
  else if (NodeName == "TMutGapCoord")
    {
      newdump = new DumpPHIOArray<TMutGapCoord>(NodeName);
    }
  else if (NodeName == "TMutHit")
    {
      newdump = new DumpPHIOArray<TMutHit>(NodeName);
    }
  else if (NodeName == "TMutMCHit")
    {
      newdump = new DumpPHIOArray<TMutMCHit>(NodeName);
    }
  else if (NodeName == "TMutMCTrk")
    {
      newdump = new DumpPHIOArray<TMutMCTrk>(NodeName);
    }
  //   else if (NodeName == "TMutMuiRoad")
  //     {
  //       newdump = new DumpPHIOArray<TMutMuiRoad>(NodeName);
  //     }
  else if (NodeName == "TMutStub")
    {
      newdump = new DumpPHIOArray<TMutStub>(NodeName);
    }
  else if (NodeName == "TMutTrk")
    {
      newdump = new DumpPHIOArray<TMutTrk>(NodeName);
    }
  else if (NodeName == "TMutVtx")
    {
      newdump = new DumpPHIOArray<TMutVtx>(NodeName);
    }
  else if (NodeName == "tofghit")
    {
      newdump = new Dumptofghit(NodeName);
    }
  else if (NodeName == "TofOut")
    {
      newdump = new DumpTofOut(NodeName);
    }
  else if (NodeName == "TrigLvl1")
    {
      newdump = new DumpTrigLvl1(NodeName);
    }
  else if (NodeName == "TrigRunLvl1")
    {
      newdump = new DumpTrigRunLvl1(NodeName);
    }
  else if (NodeName == "TRpcClus")
    {
      newdump = new DumpPHIOArray<TRpcClus>(NodeName);
    }
  else if (NodeName == "TRpcCoord")
    {
      newdump = new DumpPHIOArray<TRpcCoord>(NodeName);
    }
  else if (NodeName == "TRpcHit")
    {
      newdump = new DumpPHIOArray<TRpcHit>(NodeName);
    }
  else if (NodeName == "TRpcTrk")
    {
      newdump = new DumpPHIOArray<TRpcTrk>(NodeName);
    }
  else if (NodeName == "TRxnpRawScint")
    {
      newdump = new DumpPHIOArray<TRxnpRawScint>(NodeName);
    }
  else if (NodeName == "TRxnpRawXang")
    {
      newdump = new DumpPHIOArray<TRxnpRawXang>(NodeName);
    }
  else if (NodeName == "TRxnpScint")
    {
      newdump = new DumpPHIOArray<TRxnpScint>(NodeName);
    }
  else if (NodeName == "VtxOut")
    {
      newdump = new DumpVtxOut(NodeName);
    }
  else if (NodeName == "ZdcOut")
    {
      newdump = new DumpZdcOut(NodeName);
    }
  else
    {
      if (node->getType() == "PHIODataNode")
        {
          // need a static cast since only from DST these guys are of type PHIODataNode<TObject*>
          // when created they are normally  PHIODataNode<PHObject*> but can be anything else as well
          TObject *tmp = (TObject *)(static_cast <PHIODataNode<TObject> *>(node))->getData();
	  if (tmp->InheritsFrom("AerGeaHits"))
	    {
	      newdump = new DumpAerGeaHits(NodeName);
	    }
	  else if (tmp->InheritsFrom("CglTrack"))
	    {
	      newdump = new DumpCglTrack(NodeName);
	    }
          else if (tmp->InheritsFrom("crkghitWrapper"))
            {
              newdump = new Dumpcrkghit(NodeName);
            }
	  else if (tmp->InheritsFrom("CrkHit"))
	    {
	      newdump = new DumpCrkHit(NodeName);
	    }
          else if (tmp->InheritsFrom("CrkProj"))
            {
              newdump = new DumpCrkProj(NodeName);
            }
	  else if (tmp->InheritsFrom("CrkRing"))
	    {
	      newdump = new DumpCrkRing(NodeName);
	    }
          else if (tmp->InheritsFrom("dBbcGhitRawWrapper"))
            {
              newdump = new DumpdBbcGhitRaw(NodeName);
            }
          else if (tmp->InheritsFrom("dCrkRel2sWrapper"))
            {
              newdump = new DumpdCrkRel2s(NodeName);
            }
          else if (tmp->InheritsFrom("DchHitLineTable"))
            {
              newdump = new DumpDchHitLineTable(NodeName);
            }
          else if (tmp->InheritsFrom("dDchGhitHitsWrapper"))
            {
              newdump = new DumpdDchGhitHits(NodeName);
            }
          else if (tmp->InheritsFrom("dDchNibbleGhitWrapper"))
            {
              newdump = new DumpdDchNibbleGhit(NodeName);
            }
          else if (tmp->InheritsFrom("dEmcGeaHitWrapper"))
            {
              newdump = new DumpdEmcGeaHit(NodeName);
            }
          else if (tmp->InheritsFrom("dTecGhitRawWrapper"))
            {
              newdump = new DumpdTecGhitRaw(NodeName);
            }
          else if (tmp->InheritsFrom("emcClusterAuxInfoContainer"))
            {
              newdump = new DumpemcClusterAuxInfoContainer(NodeName);
            }
          else if (tmp->InheritsFrom("emcClusterContainer"))
            {
              newdump = new DumpemcClusterContainer(NodeName);
            }
          else if (tmp->InheritsFrom("emcTowerContainer"))
            {
              newdump = new DumpemcTowerContainer(NodeName);
            }
          else if (tmp->InheritsFrom("HbdBlobList"))
            {
              newdump = new DumpHbdBlobList(NodeName);
            }
          else if (tmp->InheritsFrom("HbdCellList"))
            {
              newdump = new DumpHbdCellList(NodeName);
            }
          else if (tmp->InheritsFrom("HbdMiniCellList"))
            {
              newdump = new DumpHbdMiniCellList(NodeName);
            }
          else if (tmp->InheritsFrom("HbdRawList"))
            {
              newdump = new DumpHbdRawList(NodeName);
            }
          else if (tmp->InheritsFrom("Lvl2DecisionOut"))
            {
              newdump = new DumpLvl2DecisionOut(NodeName);
            }
          else if (tmp->InheritsFrom("Lvl2OutArray"))
            {
              newdump = new DumpLvl2OutArray(NodeName);
            }
          else if (tmp->InheritsFrom("lpcRaw"))
            {
              newdump = new DumplpcRaw(NodeName);
            }
          else if (tmp->InheritsFrom("McEvalSingleList"))
            {
              newdump = new DumpMcEvalSingleList(NodeName);
            }
          else if (tmp->InheritsFrom("mpcClusterContainer"))
            {
              newdump = new DumpmpcClusterContainer(NodeName);
            }
          else if (tmp->InheritsFrom("MpcExEventHeader"))
            {
              newdump = new DumpMpcExEventHeader(NodeName);
            }
          else if (tmp->InheritsFrom("MpcExRawHit"))
            {
              newdump = new DumpMpcExRawHit(NodeName);
            }
          else if (tmp->InheritsFrom("mpcRawContainer"))
            {
              newdump = new DumpmpcRawContainer(NodeName);
            }
          else if (tmp->InheritsFrom("mpcSampleContainer"))
            {
              newdump = new DumpmpcSampleContainer(NodeName);
            }
          else if (tmp->InheritsFrom("mpcTowerContainer"))
            {
              newdump = new DumpmpcTowerContainer(NodeName);
            }
          else if (tmp->InheritsFrom("MutrgHeaderArray"))
            {
              newdump = new DumpMutrgHeaderArray(NodeName);
            }
          else if (tmp->InheritsFrom("MutrgHitArray"))
            {
              newdump = new DumpMutrgHitArray(NodeName);
            }
          else if (tmp->InheritsFrom("MutrgTrkArray"))
            {
              newdump = new DumpMutrgTrkArray(NodeName);
            }
          else if (tmp->InheritsFrom("PadCluster"))
            {
              newdump = new DumpPadCluster(NodeName);
            }
          else if (tmp->InheritsFrom("PadRaw"))
            {
              newdump = new DumpPadRaw(NodeName);
            }
          else if (tmp->InheritsFrom("PdbCalBankSave"))
            {
              newdump = new DumpPdbCalBankSave(NodeName);
            }
          else if (tmp->InheritsFrom("PHCentralTrack") && !tmp->InheritsFrom("PHCentralTrackv24"))
            {
              newdump = new DumpPHCentralTrack(NodeName);
            }
          else if (tmp->InheritsFrom("PHCentralTrackv24"))
            {
              newdump = new DumpPHCentralTrackv24(NodeName);
            }
          else if (tmp->InheritsFrom("PHMuoTracksAdc"))
            {
              newdump = new DumpPHMuoTracksAdc(NodeName);
            }
          else if (tmp->InheritsFrom("PHTrackOut"))
            {
              newdump = new DumpPHTrackOut(NodeName);
            }
          else if (tmp->InheritsFrom("RpSumXYObject"))
            {
              newdump = new DumpRpSumXYObject(NodeName);
            }
          else if (tmp->InheritsFrom("RunHeader"))
            {
              newdump = new DumpRunHeader(NodeName);
            }
          else if (tmp->InheritsFrom("SvxCentralTrackList"))
            {
              newdump = new DumpSvxCentralTrackList(NodeName);
            }
          else if (tmp->InheritsFrom("SvxClusterList"))
            {
              newdump = new DumpSvxClusterList(NodeName);
            }
          else if (tmp->InheritsFrom("SvxEventInfo"))
            {
              newdump = new DumpSvxEventInfo(NodeName);
            }
          else if (tmp->InheritsFrom("SvxQAInfo"))
            {
              newdump = new DumpSvxQAInfo(NodeName);
            }
          else if (tmp->InheritsFrom("SvxPixelRawHitList"))
            {
              newdump = new DumpSvxPixelRawHitList(NodeName);
            }
          else if (tmp->InheritsFrom("SvxRawhitClusterList"))
            {
              newdump = new DumpSvxRawhitClusterList(NodeName);
            }
          else if (tmp->InheritsFrom("SvxRawhitList"))
            {
              newdump = new DumpSvxRawhitList(NodeName);
            }
          else if (tmp->InheritsFrom("SvxSegmentList"))
            {
              newdump = new DumpSvxSegmentList(NodeName);
            }
          else if (tmp->InheritsFrom("TecClusterContainer"))
            {
              newdump = new DumpTecClusterContainer(NodeName);
            }
          else if (tmp->InheritsFrom("TecOut"))
            {
              newdump = new DumpTecOut(NodeName);
            }
          else if (tmp->InheritsFrom("tecghitWrapper"))
            {
              newdump = new Dumptecghit(NodeName);
            }
          else if (tmp->InheritsFrom("TofwHit"))
            {
              newdump = new DumpTofwHit(NodeName);
            }
          else if (tmp->InheritsFrom("TofwRaw"))
            {
              newdump = new DumpTofwRaw(NodeName);
            }
          else if (tmp->InheritsFrom("TrigRunLvl2"))
            {
              newdump = new DumpTrigRunLvl2(NodeName);
            }
          else if (tmp->InheritsFrom("VariableArray"))
            {
              newdump = new DumpVariableArray(NodeName);
            }
          else if (tmp->InheritsFrom("VariableArrayInt"))
            {
              newdump = new DumpVariableArrayInt(NodeName);
            }
          else if (tmp->InheritsFrom("ZdcRaw"))
            {
              newdump = new DumpZdcRaw(NodeName);
            }
          else
            {
              cout << "Registering Dummy for " << NodeName
                   << ", Class: " << tmp->ClassName() << endl;
              newdump = new DumpObject(NodeName);
            }
        }
      else
        {
          cout << "ignoring PHDataNode: " << NodeName << endl;
          newdump = new DumpObject(NodeName);
        }
    }

 initdump:
  newdump->SetParentNodeDump(this);
  newdump->SetOutDir(outdir);
  newdump->SetPrecision(fp_precision);
  newdump->Init();
  dumpthis[newnode] = newdump;
  return 0;
}

int
PHNodeDump::SetOutDir(const string &dirname)
{
  outdir = dirname;
  return 0;
}
