#include "mTecPIDModule.h"
#include "TecOutV1.hh"
#include "TecOutV7.hh"
#include "TecOutV6.hh"
#include "TecTrackV2.hh"
#include "TecOut.hh"
#include "EventHeader.h"
#include "RunHeader.h"
#include "TecCalibrationObject.hh"
#include "DchTrack.h"
#include "PadCluster.h"
#include "CglTrack.h"
#include "PHTrackOut.h"
#include "mTecUtilities.h"
#include "getClass.h"
#include "dCrkHitWrapper.h"
#include "CrkGeometryObject.hh"

#include "PHIODataNode.h"
#include "PHNodeIterator.h"

#include <TROOT.h>
#include <TFile.h>
#include <TH1.h>
#include <TNtuple.h>

#include "gsl/gsl_math.h"

#include <cstdio>
#include <cmath>
#include <algorithm>
#include <vector>
#include <map>
#include <cstdlib>
#include <iostream>

using namespace TecUtilities;
using namespace std;
static int EventNumber;

int 
mTecPIDModule::GetBeg12(TecGeometryObject* TGO, 
			float* beg1, float* beg2)
{

// Find coordinates of the midddle wire of the first and
// last planes in the south side of sector 1.
// This is temporary. Eventually we will calculate track
// length using intersections of the track with first and last 
// planes in corresponding sector.

    *beg1 = TGO->getGlobalX(12,TecGeometryObject::get_NumWires(0)/2);
//// use plane 3 for "out" coordinate 03/06/02
//    *beg2 = TGO->getGlobalX(18,TecGeometryObject::get_NumWires(3)/2);
// use plane 5 for "out" coordinate (03/09/2004)
    *beg2 = TGO->getGlobalX(22,TecGeometryObject::get_NumWires(3)/2);

  return 0;
}

int mTecPIDModule::GetTrackLength(int itrk,
                                  TecOutV7* tecout,
                                  CglTrack* cgl,
                                  DchTrack* dch,
                                  float& TrackLength)
{

  // Calculates track length using X and Y from Tec and Z from Dch

  float dx = tecout->getTrackXout(itrk) - tecout->getTrackXin(itrk);
  float dy = tecout->getTrackYout(itrk) - tecout->getTrackYin(itrk);
  float trl = sqrt(dx * dx + dy * dy);

  // find corresponding Dch track (if any)
  int found = 0;
  for (int i = 0; i < (int)cgl->get_CglNTrack(); i++)
    {
      if (itrk == cgl->get_tectrackid(i) && cgl->get_dctracksid(i) >= 0)
        {
          int dchtrkid = cgl->get_dctracksid(i);
          //float theta = dch->get_theta0(dchtrkid);
          float theta = dch->get_beta(dchtrkid);
          trl = trl / cos(M_PI_2  - theta);
          found = 1;
          break;
        }
    }

  if (found)
    {
      TrackLength = trl;
      return 1;
    }
  else
    {
      TrackLength = -trl;
      return 0;
    }
}

mTecPIDModule::mTecPIDModule()
{
  Verbose = 0;
  method = 1;
  Truncation = 0.6;
  mip = 5.35;
  el_gaindep[0] = 1.71;
  el_gaindep[1] = -0.0212;
  el_gaindep[2] = 0.;
  mip_gaindep[0] = -0.00484;
  mip_gaindep[1] = 0.1014;
  mip_gaindep[2] = 0.;
  el_trnkdep[0] = 1.70;
  el_trnkdep[1] = -0.348;
  el_trnkdep[2] = 0.;
  mip_trnkdep[0] = 0.914;
  mip_trnkdep[1] = -2.83;
  mip_trnkdep[2] = 4.95;
  Sigma[0] = 0.20;
  Sigma[1] = 0.15;
  Sigma[2] = 0.15;
  Sigma[3] = 0.15;
  numbinmin = 100;
  Calibration[0] = 1.41;
  Calibration[1] = 10000.0;
  UseOnlyMerged = 0;
  Write2File = 0;
  Write2Ntuple = 0;
  RejectOverlaps = 0; // overlaps are always rejected
  PerfectPID = 0;
  UseTec = 0;
  TecPc1Cut = 2.5;
  TecPc3Cut = 4.0;
  CrkRingCut = 20.;
  hitcut = 3;
  TRChargeCut = 20;
  TRTimeCut = 20;
  cgo = new CrkGeometryObject();
  cgo->UseSurvey();
  EventNumber = 0;
}

mTecPIDModule::~mTecPIDModule()
{
  delete cgo;
  return;
}

// MAIN
PHBoolean mTecPIDModule::event(PHCompositeNode *topNode)
{
  //------ Get pointers to the Tables ------------------------------------

  DchTrack * dch = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (!dch)
    {
       cerr << PHWHERE << " ERROR: Can not find DchTrack." << endl;
       //       return False;      
    }

  CglTrack * cgl = findNode::getClass<CglTrack>(topNode, "CglTrack");
  if (!cgl)
    {
       cerr << PHWHERE << " ERROR: Can not find CglTrack." << endl;
       //       return False;      
    }

  TecOutV7 * techitout = findNode::getClass<TecOutV7>(topNode, "TecHitOut");
  if (!techitout)
    {
       cerr << PHWHERE << " ERROR: Can not find TecHitOut." << endl;
       return False;      
    }

  TecOutV7 * tectrackout = findNode::getClass<TecOutV7>(topNode, "TecOut");
  if (!tectrackout)
    {
       cerr << PHWHERE << " ERROR: Can not find TecTrackOut." << endl;
       return False;      
    }

  TecOutV6 * tecoutshort = findNode::getClass<TecOutV6>(topNode, "TecOutShort");
  if (!tecoutshort)
    {
       cerr << PHWHERE << " ERROR: Can not find TecOutShort." << endl;
       return False;      
    }

  TecCalibrationObject * TecCalibration = findNode::getClass<TecCalibrationObject>(topNode, "TecCalibration");
  if (!TecCalibration)
    {
       cerr << PHWHERE << " ERROR: Can not find TecCalibration." << endl;
       return False;      
    }

  // couting operational planes
  int NOPPLANES[8];
  for (int i=0; i<8; i++)
    {
      NOPPLANES[i] = 0;
      int isect = i/2;
      int iside = i%2;
      for (int iplane=0; iplane<6; iplane++)
	  NOPPLANES[i] += TecCalibration->getAbsoluteGain(isect*12+iplane*2+iside) > 0;
    }

  //--------------------------------------------------------------------

  typedef vector<float> BUFFER;
  BUFFER buffer[6]; 
  multimap<int, int, less<int> > HITMAP;
  float WTB[6];
  float WTBNORM[6];
  int ntr[6];
  float tmpde[6], tmptr[6];  

#ifdef DEBUG
      cout << "mTecPID -> Started event # " << EventNumber << endl;
      cout << "mTecPID -> Number of tracks: " << techitout->getNTracks() << " "
      << tectrackout->getNTracks() << endl;
      cout << "mTecPID -> Number of hits: " << techitout->getNHits() << endl;
      if (cgl)
        printf("mTecPID -> # of entries in CglTrack: %d\n", cgl->get_CglNTrack());
      if (dch)
        printf("mTecPID -> # of entries in DchTrack: %d\n", dch->get_DchNTrack());
#endif

  // Fill HITMAP only if number of hits in a plane is OK

  int ntmp = 0;
  for (int j = 0; j < techitout->getNHits(); j++)
    {
      int trkid = techitout->getHitTrackID(j);
      if (trkid > -1)
        {
          if (tectrackout->getTrackNhits(trkid, techitout->getHitPlane(j)) > hitcut &&
	      tectrackout->getTrackNhits(trkid, techitout->getHitPlane(j)) < 80)
            {
              ntmp++;
              HITMAP.insert(pair<int, int>(trkid, j));
	    }
        }
    } // j - end loop over hits

#ifdef DEBUG
    cout << "mTecPID -> Number of associated hits = " << ntmp << endl;
#endif

  //======================= Loop over TEC tracks =========================
  for (int itrk = 0; itrk < tectrackout->getNTracks(); itrk++)
    {
      memset(WTB, 0, sizeof(WTB));
      memset(WTBNORM, 0, sizeof(WTBNORM));
      memset(ntr, 0, sizeof(ntr));
      memset(tmpde, 0, sizeof(tmpde));
      memset(tmptr, 0, sizeof(tmptr));
      int nhits100[6];
      memset(nhits100, 0, sizeof(nhits100));
      int nhits20[6];
      memset(nhits20, 0, sizeof(nhits20));
      float wtb[6];
      memset(wtb, 0, sizeof(wtb));
      for (int iplane=0; iplane<6; iplane++)
	buffer[iplane].clear(); // reset buffer

      // Find track length
      float TrackLength = 0.;
      if (cgl && dch)
        {
	  GetTrackLength(itrk, tectrackout, cgl, dch, TrackLength);
        }

#ifdef DEBUG
      cout << " track:        " << itrk << endl;
#endif
 
      // Find number of fired planes for this track
//       int nfpl = 0;
      
//       for (int ipl = 0; ipl < TECMAXPLANE; ipl++)
//         {
//           if (tectrackout->getTrackNhits(itrk, ipl) > hitcut &&
// 	      tectrackout->getTrackNhits(itrk, ipl) < 80)
//             {
//               nfpl++;
//             }
//         }
      // Correct TrackLength for number of operational planes
      int isect = tectrackout->getTrackSector(itrk);
      TrackLength = TrackLength / float(TECMAXPLANE) * float(NOPPLANES[isect]);

      // Fill buffer and plane variables first
      for (multimap<int, int, less<int> >::iterator it = HITMAP.begin();
           it != HITMAP.end(); ++it)
        {
          if ((*it).first != itrk) continue;
	  int ihit = (*it).second;
	  int jbin = techitout->getHitTimeBin(ihit);
	  int jindex = techitout->getHitIndex(ihit);
	  float gasgain = TecCalibration->getAbsoluteGain(jindex);
	  float gainT = TecCalibration->getTimingGain(jindex,jbin);
	  //myampl = myampl*gainT;
	  //      techitout->setHitCharge(ihit, myampl);
	  int iplane = techitout->getHitPlane(ihit);
	  int adc = techitout->getHitADC(ihit);
	  float myampl = Ampl2Charge(adc) * gasgain * gainT;
	  buffer[iplane].push_back(myampl);
	  tmpde[iplane] += myampl;
	  WTB[iplane] += techitout->getHitTimeBin(ihit) * techitout->getHitTimeBin(ihit) * myampl * gainT;
	  WTBNORM[iplane] += techitout->getHitTimeBin(ihit);
	  if (myampl > 100.)
	    nhits100[iplane]++;
	  if (myampl > 20.)
	    nhits20[iplane]++;	      
	  if (myampl>TRChargeCut && jbin>TRTimeCut)
	    {
	      ntr[iplane] ++;
	      tmptr[iplane] += myampl;
	    }
	}
      
      // finishing plane variables
      for (int iplane=0; iplane<6; iplane++)
	{
	  if (WTBNORM[iplane] != 0.)
	    {
	      wtb[iplane] = WTB[iplane] / WTBNORM[iplane];
	    }
	  
	  // Re-order buffer
	  sort(buffer[iplane].begin(), buffer[iplane].end());
	  
          // Calculate truncated mean dE/dX
	  
          int ntrunk = ((int)(buffer[iplane].size() * Truncation));
          float trnksum = 0.;
          float trnksumtot = 0.;
          for (int i = 0; i < ntrunk; i++)
            trnksum += buffer[iplane][i];
          for (int i = 0; i < (int)buffer[iplane].size(); i++)
            trnksumtot += buffer[iplane][i];
	  
          // float trnksum_perbin = 0.;
          // float trnksumtot_perbin = 0.;
          // float trnksum_perdx = 0.;
          // float trnksumtot_perdx = 0.;
          // if (ntrunk > 0)
          //   {
          //     trnksum_perbin = trnksum / ntrunk;
          //   }
	  //          trnksumtot_perbin = trnksumtot / buffer[iplane].size();
          // if (TrackLength != 0.)
          //   {
          //     trnksum_perdx = trnksum / TrackLength;
          //     trnksumtot_perdx = trnksumtot / TrackLength;
          //   }
	  tectrackout->setTrackNtr(itrk, iplane, ntr[iplane]);
	  tectrackout->setTrackDE(itrk, iplane, tmpde[iplane]);
	  tectrackout->setTrackTr(itrk, iplane, tmptr[iplane]);
          tectrackout->setTrackNdEdXbins(itrk, iplane, ntrunk);
          tectrackout->setTrackdEdX06(itrk, iplane, trnksum);
          tectrackout->setTrackNhits100(itrk, iplane, nhits100[iplane]);
          tectrackout->setTrackNhits20(itrk, iplane, nhits20[iplane]);
          tectrackout->setTrackWeightedTimeBin(itrk, iplane, wtb[iplane]);
#ifdef DEBUG
       int totnhits =  buffer[iplane].size();
       cout << "    plane:        " << iplane << endl;
       cout << "    nbins:        " << ntrunk << endl;
       cout << "    nbinstot:     " << totnhits << endl;
       cout << "    FADC_sum:     " << tectrackout->getTrackdEdX(itrk, iplane) << endl;
//       cout << "    FADC_sumtot:  " << trnksumtot << endl;
//       cout << "    Truncation:   " << Truncation << endl;
//       cout << "    ntr:          " << tectrackout->getTrackNtr(itrk, iplane) << endl;
//       cout << "    TrackLength:  " << tectrackout->getTrackLength(itrk) << endl;
#endif
	}
      tectrackout->setTrackLength(itrk, TrackLength);
//--------------------------------------------------------------------------------------------
//Fill Short track object for CNTs
      TecTrack *tectracktmp;
      tectracktmp = (TecTrack*)(tectrackout->GetTecTracks()->UncheckedAt(itrk));
      tecoutshort->AddTecTrack(*tectracktmp);

      tecoutshort->setTrackLikelihood(itrk,0);
      tecoutshort->setTrackPhi(itrk, tectrackout->getTrackPhi(itrk));
      tecoutshort->setTrackAlpha(itrk, tectrackout->getTrackAlpha(itrk));
      tecoutshort->setTrackIndex(itrk, tectrackout->getTrackIndex(itrk));
      tecoutshort->setTrackLength(itrk, tectrackout->getTrackLength(itrk));  
      for (int k=0; k<6; k++)
	{
	  tecoutshort->setTrackdEdX06(itrk, k, tectrackout->getTrackdEdX06(itrk, k));
	  tecoutshort->setTrackDE(itrk, k, tectrackout->getTrackDE(itrk, k));
	  tecoutshort->setTrackWeightedTimeBin(itrk, k, tectrackout->getTrackWeightedTimeBin(itrk, k));
	  tecoutshort->setTrackNhits20(itrk, k, tectrackout->getTrackNhits20(itrk, k));
	  tecoutshort->setTrackNhits100(itrk, k, tectrackout->getTrackNhits100(itrk, k));
	  tecoutshort->setTrackNdEdXbins(itrk, k, tectrackout->getTrackNdEdXbins(itrk, k));
	  tecoutshort->setTrackNtr(itrk, k, tectrackout->getTrackNtr(itrk, k));
	  tecoutshort->setTrackTr(itrk, k, tectrackout->getTrackTr(itrk, k));
	  tecoutshort->setTrackNwires(itrk, k, tectrackout->getTrackNwires(itrk, k));
	  tecoutshort->setTrackNhits(itrk, k, tectrackout->getTrackNhits(itrk, k));
	}
     
//------------------------------------------------------------------------------------------------
//  Find PC, EMCAl and RICH hits
      TecTrackV2* tectracktmpv2 = (TecTrackV2*)tectracktmp;
      tectracktmpv2->project2PC(topNode);
      tectracktmpv2->project2ZinvertedPC(topNode);
      tectracktmpv2->project2EMC(topNode);
      tectracktmpv2->project2ZinvertedEMC(topNode);
      tectracktmpv2->project2Crk(topNode, cgo, CrkRingCut);
      tectracktmpv2->project2ZinvertedCrk(topNode, cgo, CrkRingCut);      
    } //========================= itrk - end loop over TEC tracks

  EventNumber++;
  HITMAP.clear();

  return True;
}

