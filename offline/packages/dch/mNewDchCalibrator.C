/////////////////////////////////////
// Author:

#include <mNewDchCalibrator.hh>
#include <PHDchGeometryObject.h>
#include <PHDchCalibrationObject.h>
#include <PHDchNoiseObject.h>
#include <PHIODataNode.h>

#include <BbcCalib.hh>
#include <T0Out.h>
#include <dDchReconstructionParWrapper.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <PHTable.hh>

#include <cmath>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHObject> PHObjectNode_t;
typedef PHIODataNode<dDchGhitRawWrapper> dDchGhitRawNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<dDchGhitHitsWrapper> dDchGhitHitsNode_t;
typedef PHIODataNode<dDchReconstructionParWrapper> dDchReconstructionParNode_t;
typedef PHIODataNode<PHTable> TableNode_t;
typedef PHIODataNode<T0Out> T0OutNode_t;

mNewDchCalibrator::mNewDchCalibrator()
{
  rand = gsl_rng_alloc(gsl_rng_mt19937);
  gsl_rng_set (rand, 41074); // use fixed seed -- this is OK since this is reconstruction, not simulation

  correctTimeFlag = 1; // if 1 then correct the time using BBC calibration
  slewingCorrectionFlag = 1; // apply the slewing correction
  verbose = 0;
  timeZeroBBC = -9000;
  delt0_e = 0.;
  deldv_e = 1.;
  delt0_w = 0.;
  deldv_w = 1.;
}

mNewDchCalibrator::~mNewDchCalibrator()
{
  gsl_rng_free(rand);
}

PHBoolean mNewDchCalibrator::event(PHCompositeNode *root)
{

  topNode = root;
  PHObjectNode_t* phob;

  PHPointerList<PHNode> nodes;
  PHNodeIterator i(root);
  PHNode *n;
  PHDataNode<PHDchAddressObject>* dchDaoNode;
  PHDataNode<PHDchGeometryObject>* dchDgoNode;
  PHDataNode<PHDchCalibrationObject>* dchDcoNode;
  PHDataNode<PHDchNoiseObject>* dchDnoNode;

  nodes.clear();
  // find the Geometry Objects

  dchDaoNode = (PHDataNode<PHDchAddressObject>*)i.findFirst("PHDataNode", "DchDAO");
  dchAddressObject = dchDaoNode->getData();

  dchDgoNode = (PHDataNode<PHDchGeometryObject>*)i.findFirst("PHDataNode", "DchDGO");
  dchGeometryObject = dchDgoNode->getData();

  dchDcoNode = (PHDataNode<PHDchCalibrationObject>*)i.findFirst("PHDataNode", "DchDCO");
  dchCalibrationObject = dchDcoNode->getData();

  dchDnoNode = (PHDataNode<PHDchNoiseObject>*)i.findFirst("PHDataNode", "DchDNO");
  dchNoiseObject = dchDnoNode->getData();

  phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchRawTablev1"));
  rawTable = dynamic_cast < DchRawTable * >(phob->getData ());
  if (!rawTable)
    {
      cout << "DchRawTablev1 not found " << endl;
      return False;
    }

  phob = static_cast < PHObjectNode_t * >(i.findFirst ("PHIODataNode", "DchHitLineTablev1"));
  hitLineTable = dynamic_cast < DchHitLineTable * >(phob->getData ());
  if (!hitLineTable)
    {
      cout << "DchHitLineTablev1 not found " << endl;
      return False;
    }

  n = i.findFirst("PHIODataNode", "dDchRecoPar");
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchGhitRaw");
  nodes.append(n);

  n = i.findFirst("PHIODataNode", "dDchGhitHits");
  nodes.append(n);

  if (dchAddressObject->getFlagMC() == 1)  // Do not get BBC T0 for MC reconstruction
    {
      correctTimeFlag = 0;
    }
  if (correctTimeFlag)
    { // if corrected time is used
      if (correctTime())
        {
          callPAM(nodes);
          return True;
        }
      else
        {
          callPAM(nodes);
          return False;
        }
    }
  else
    {                // for calibration purposes
      callPAM(nodes);
      return False;
    }
}

int
mNewDchCalibrator::ResetEvent(PHCompositeNode *topNode)
{
  hitLineOutList.clearAndDestroy();
  return 0;
}

PHBoolean mNewDchCalibrator::callPAM(PHPointerList<PHNode> &nl)
{

  dDchReconstructionParNode_t* recoNode = static_cast<dDchReconstructionParNode_t*>(nl[0]);
  dDchGhitRawNode_t* gRawNode = static_cast<dDchGhitRawNode_t*>(nl[1]);
  dDchGhitHitsNode_t* gHitNode = static_cast<dDchGhitHitsNode_t*>(nl[2]);

  TABLE_HEAD_ST dDchReco_h = recoNode->getData()->TableHeader();
  DDCHRECONSTRUCTIONPAR_ST *dDchReco = recoNode->getData()->TableData();

  TABLE_HEAD_ST dDchGhitRaw_h = gRawNode->getData()->TableHeader();
  DDCHGHITRAW_ST *dDchGhitRaw = gRawNode->getData()->TableData();

  TABLE_HEAD_ST dDchGhitHits_h = gHitNode->getData()->TableHeader();
  DDCHGHITHITS_ST *dDchGhitHits = gHitNode->getData()->TableData();

  PHNodeIterator nodeIter(topNode);

  PHIODataNode<TObject> *RawNode = (PHIODataNode<TObject>*)nodeIter.findFirst("PHIODataNode", "dDchRaw");
  dDchRawWrapper* rawWrapper = (dDchRawWrapper*)(RawNode->getData());

  // locally used variables

  short mc_flag = 0;   // set to zero if NO MC information available
  long hitcounter = -1;   // preset hit counter
  // of hit was found

  DchHitLineOutv1 *hitLineOut, *hitLineOutOld;

  // zero the OUTPUT table
  dDchGhitHits_h.nok = 0;

  short mirrorHit = 1; //TKH - use the mirror mechanism for UV...
  if (dDchReco_h.nok > 0)
    {
      mirrorHit = dDchReco[0].mirrorHitAnalysis;
    }

  // check if MC information is available
  mc_flag = dchAddressObject->getFlagMC();
  PdbIndex thisIndexAccess(0, 12799, 0, "GLOBAL");

  int global, global2;
  for (unsigned long rawcounter = 0; rawcounter < rawWrapper->RowCount() ; rawcounter += 2)
    {

      global = rawWrapper->get_global(rawcounter);
      global2 = rawWrapper->get_global(rawcounter + 1);

      if (global == global2)
        {

          short ltime = rawWrapper->get_time(rawcounter);
          short ttime = rawWrapper->get_time(rawcounter + 1);

          if (!dchNoiseObject->status(global) || ltime < 0)
            {
              continue;  //  good data  or not
            }
          short arm = rawWrapper->get_arm(rawcounter);
          short plane = rawWrapper->get_plane(rawcounter);
          short cell = rawWrapper->get_cell(rawcounter);
          short side = rawWrapper->get_side(rawcounter);

          //  Set arm dependent  calibration corrections
          if ( arm == 0 )
            {
              delt0 = delt0_e;
              deldv = deldv_e;
            }
          else
            {
              delt0 = delt0_w;
              deldv = deldv_w;
            }

          if (mc_flag == 1)  //  Do not use in Monte Carlo
            {
              delt0 = 0.;
              deldv = 1.;
            }

          hitLineOut = new DchHitLineOutv1();
          hitLineOutList.append(hitLineOut);

          thisIndexAccess.setValue(global);
          float ltime_corr = (float)ltime + gsl_rng_uniform(rand);  // floatization and random smearing digitized time

          if (mc_flag != 1)
            {
              ltime_corr += 5.;    //small calibration correction
            }
          if (slewingCorrectionFlag && mc_flag != 1 )
            {
              ltime_corr = ltime_corr + slewCorrection(arm, plane, ttime - ltime);
            }

          PHLine line;
          PHVector dist;

          if (mc_flag != 1)    // Kinked drift alley only for real data otherwise linear transformation
            {
              dist = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, ltime_corr, kDchRiabovCalib, delt0, deldv);
              line = dchGeometryObject->transformDistanceToLine(arm, side, plane, cell, dist);
            }
          else
            {
              dist = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, ltime_corr, kDchLinearCalib, delt0, deldv);
              line = dchGeometryObject->transformDistanceToLine(arm, side, plane, cell, dist);
            }

          hitcounter++;
          if (hitcounter > 0x7fff)
            {
              break;
            }
          hitLineOut->setId(hitcounter);
          hitLineOut->setArm(arm);
          hitLineOut->setPlane(plane);
          hitLineOut->setCell(cell);
          hitLineOut->setSide(side);
          hitLineOut->setTime1(ltime);
          hitLineOut->setTime2(ttime);
          hitLineOut->setDistance(dist.getX());
          hitLineOut->setWidth(ttime - ltime);
          hitLineOut->setIdraw1(rawcounter);
          hitLineOut->setIdraw2(rawcounter + 1);
          hitLineOut->setXYZ(line.getBasepoint());
          hitLineOut->setVXYZ(line.getDirection());
          hitLineOut->setIdmirror(0);  // mirror==0 means front side!

          if (mc_flag == 1)
            {            // setup pointer to MC hit
              dDchGhitHits[hitcounter].ghitid = dDchGhitRaw[rawcounter].ghitid;
              dDchGhitHits[hitcounter].hitsid = hitcounter;
              dDchGhitHits_h.nok++;
            }

          //NOTE:  I have changed the mirror hit logic so that the mirror
          //hits can only be found on the UV layers...
          if (mirrorHit)
            {

              int pln = hitLineOut->getPlane();
              if ((pln > 11 && pln < 20) || (pln > 31))
                {  // Make sure hit is stereo plane

                  hitLineOutOld = hitLineOut;

                  // Add inner mirror hit to list
                  dist = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, ltime_corr, kDchInnerStereoCalib, delt0, deldv);
                  line = dchGeometryObject->transformDistanceToLine(arm, side, plane, cell, dist);

                  hitLineOut = new DchHitLineOutv1();
                  hitLineOutList.append(hitLineOut);
                  hitLineOut->setIdmirror( -1);	  // mirror==-1 means inner mirror

                  hitcounter++;
                  hitLineOut->setId(hitcounter);
                  hitLineOut->setCell(hitLineOutOld->getCell());
                  hitLineOut->setPlane(hitLineOutOld->getPlane());
                  hitLineOut->setArm(hitLineOutOld->getArm());
                  hitLineOut->setSide(hitLineOutOld->getSide());
                  hitLineOut->setDistance(dist.getX());
                  hitLineOut->setWidth(hitLineOutOld->getWidth());
                  hitLineOut->setTime2(hitLineOutOld->getTime2());
                  hitLineOut->setTime1(hitLineOutOld->getTime1());
                  hitLineOut->setIdraw1(hitLineOutOld->getIdraw1());
                  hitLineOut->setIdraw2(hitLineOutOld->getIdraw2());
                  hitLineOut->setXYZ(line.getBasepoint());
                  hitLineOut->setVXYZ(line.getDirection());
                  if (mc_flag == 1)
                    {           // setup pointer to MC hit
                      dDchGhitHits[hitcounter].ghitid = dDchGhitRaw[rawcounter].ghitid;
                      dDchGhitHits[hitcounter].hitsid = hitcounter;
                      dDchGhitHits_h.nok++;
                    }

                  // Add outer mirror hit to list
                  dist = dchCalibrationObject->transformTimeToDistance(&thisIndexAccess, ltime_corr, kDchOuterStereoCalib, delt0, deldv);
                  line = dchGeometryObject->transformDistanceToLine(arm, side, plane, cell, dist);

                  hitLineOut = new DchHitLineOutv1();
                  hitLineOutList.append(hitLineOut);
                  hitLineOut->setIdmirror(1);	  // mirror==1 means outer mirror

                  hitcounter++;
                  hitLineOut->setId(hitcounter);
                  hitLineOut->setCell(hitLineOutOld->getCell());
                  hitLineOut->setPlane(hitLineOutOld->getPlane());
                  hitLineOut->setArm(hitLineOutOld->getArm());
                  hitLineOut->setSide(hitLineOutOld->getSide());
                  hitLineOut->setDistance(dist.getX());
                  hitLineOut->setWidth(hitLineOutOld->getWidth());
                  hitLineOut->setTime2(hitLineOutOld->getTime2());
                  hitLineOut->setTime1(hitLineOutOld->getTime1());
                  hitLineOut->setIdraw1(hitLineOutOld->getIdraw1());
                  hitLineOut->setIdraw2(hitLineOutOld->getIdraw2());
                  hitLineOut->setXYZ(line.getBasepoint());
                  hitLineOut->setVXYZ(line.getDirection());
                  if (mc_flag == 1)
                    {           // setup pointer to MC hit
                      dDchGhitHits[hitcounter].ghitid = dDchGhitRaw[rawcounter].ghitid;
                      dDchGhitHits[hitcounter].hitsid = hitcounter;
                      dDchGhitHits_h.nok++;
                    }

                }
            }  // end mirror hit addition
        }
      else
        {
          cout << "******* WARNING !!! Raw Table is seriously corrupt. Trailing edge not found" << endl;
          return False;
        }
    }

  recoNode->getData()->SetRowCount(dDchReco_h.nok);
  gRawNode->getData()->SetRowCount(dDchGhitRaw_h.nok);
  gHitNode->getData()->SetRowCount(dDchGhitHits_h.nok);

  fillWrapperTables();

  fillOutputTables();

  return True;
}

void mNewDchCalibrator::fillOutputTables()
{

  DchHitLineTablev1 *hitLineTablev1 = dynamic_cast < DchHitLineTablev1 * >(hitLineTable);

  if (hitLineTablev1)
    {
      hitLineTablev1->Clear();
      hitLineTablev1->Expand( hitLineOutList.length());
      DchHitLineOutv1* hitLine;

      for (unsigned int i = 0; i < hitLineOutList.length(); i++)
        {
          hitLine = hitLineOutList[i];
          hitLineTablev1->AddHit(hitLine);
        }
    }
  else
    {
      cout << "Type cast not possible " << endl;
      hitLineTable->identify();
    }

}

void mNewDchCalibrator::fillWrapperTables()
{
  PHNodeIterator i(topNode);
  PHIODataNode<TObject> *HitNode = reinterpret_cast<PHIODataNode<TObject>*>(i.findFirst("PHIODataNode", "dDchHit"));
  if (not HitNode) {
    std::cerr << "Error: No PHIODataNode dDchHit found ... cannot continue" << std::endl;
    abort();
  }

  dDchHitWrapper *hitWrapper = reinterpret_cast<dDchHitWrapper*>(HitNode->getData());
  hitWrapper->SetRowCount(hitLineOutList.length());

  for (unsigned int i = 0; i < hitLineOutList.length(); i++)
    {
      DchHitLineOutv1* hitLine = hitLineOutList[i];
      hitWrapper->set_id(i, hitLine->getId());
      hitWrapper->set_arm(i, hitLine->getArm());
      hitWrapper->set_plane(i, hitLine->getPlane());
      hitWrapper->set_cell(i, hitLine->getCell());
      hitWrapper->set_side(i, hitLine->getSide());
      hitWrapper->set_distance(i, hitLine->getDistance());
      hitWrapper->set_width(i, hitLine->getWidth());
      hitWrapper->set_time1(i, hitLine->getTime1());
      hitWrapper->set_time2(i, hitLine->getTime2());
      hitWrapper->set_idraw1(i, hitLine->getIdraw1());
      hitWrapper->set_idraw2(i, hitLine->getIdraw2());
      hitWrapper->set_idmirror(i, hitLine->getIdmirror());
      hitWrapper->set_used(i, hitLine->getUsed());
      hitWrapper->set_xyz(0, i, hitLine->getXYZ().getX());
      hitWrapper->set_xyz(1, i, hitLine->getXYZ().getY());
      hitWrapper->set_xyz(2, i, hitLine->getXYZ().getZ());
      hitWrapper->set_vxyz(0, i, hitLine->getVXYZ().getX());
      hitWrapper->set_vxyz(1, i, hitLine->getVXYZ().getY());
      hitWrapper->set_vxyz(2, i, hitLine->getVXYZ().getZ());
    }
}

float mNewDchCalibrator::getBbcTimeZeroOffset()
{
  PHNodeIterator nodeIter(topNode);
  PHDataNode<TObject> *BBCNode = (PHDataNode<TObject>*)nodeIter.findFirst("PHDataNode", "BbcCalibPar");
  if (!BBCNode)
    {
      PHMessage("mNewDchCalibrator::getBbcTimeZeroOffset", PHWarning, "BbcCalibPar table not found T0 Offset not set");
      return 0;
    }

  BbcCalib* bbcCalib = (BbcCalib*)(BBCNode->getData());
  if (bbcCalib)
    {
      float bbcTimeZeroOffset = bbcCalib->getTZeroOff()->getCalibPar(0)->getPeakChannel();
      if (verbose)
        {
          cout << "Time Zero OFFset is (ns): " << bbcTimeZeroOffset << endl;
        }
      return bbcTimeZeroOffset;
    }
  else
    {
      return 0;
    }

}

PHBoolean mNewDchCalibrator::analyzeTimeZeroBBC()
{

  T0Out *T0out = 0;
  float time;
  PHTypedNodeIterator<T0Out> t0iter(topNode);
  T0OutNode_t *T0OutNode = t0iter.find("T0Out");

  if (T0OutNode)
    {
      T0out = T0OutNode->getData();
    }
  else
    {
      PHMessage("mNewDchCalibrator::correctTime", PHWarning,
                "T0Out table not found ");
      return False;
    }

  if (T0out->isValid())
    {    // change from isValid to BBC only
      time = T0out->get_T0();
    }
  else
    {
      return False;
    }

  float binSize = dchCalibrationObject->getBinSize();
  float tmpTimeZeroBBC;
  float bbcTimeZeroOffset = getBbcTimeZeroOffset(); // only works if bbc fired othervise is zero!!!

  if (fabs(time) > 500)
    {
      cerr << "WARNING!! T0 seems to be too big, but it is considered to be valid !???      T0 = " << time << endl << endl;
      return False;
    }

  tmpTimeZeroBBC = (time + bbcTimeZeroOffset) / binSize;
  if (verbose)
    {
      cout << "T0 (ns) " << time << endl;
      cout << "T0 (bins) " << tmpTimeZeroBBC << endl;
    }
  // lroundf returns random value for inf's and Nan's so we better check that
  // before
  if (finite(tmpTimeZeroBBC))
    {
      timeZeroBBC = lroundf(tmpTimeZeroBBC);
    }
  else
    {
      cout << PHWHERE << " Really Invalid T0: " << timeZeroBBC << endl;
      return False;
    }
  if (verbose)
    {
      cout << "Time Zero (long bins) " << timeZeroBBC << endl;
    }
  return True;
}

PHBoolean mNewDchCalibrator::correctTime()
{

  PHNodeIterator nodeIter(topNode);

  PHIODataNode<TObject> *RawNode = (PHIODataNode<TObject>*)nodeIter.findFirst("PHIODataNode", "dDchRaw");
  dDchRawWrapper* rawWrapper = (dDchRawWrapper*)(RawNode->getData());

  if (!analyzeTimeZeroBBC())
    {
      if (verbose)
        {
          PHMessage("mNewDchCalibrator::correctTime", PHWarning, "  T0 not present");
        }
      return False;
    }

  int i, time, cell;
  int totalRaws;

  totalRaws = rawWrapper->RowCount();

  for (i = 0; i < totalRaws; i++)
    {
      time = rawWrapper->get_time(i);
      cell = rawWrapper->get_cell(i);

      time = time - (timeZeroBBC);     // subtract the timeZero from the BBC
      if (cell < 0 || cell > 79)
        {
          time = -1; // set time = -1 for the cell outside
          //the Drift Chamber (Ref-chan for example)
        }
      rawWrapper->set_time(i, time);
    }

  totalRaws = rawTable->Entries();
  for (i = 0; i < totalRaws; i++)
    {
      time = rawTable->getTime(i);
      cell = rawTable->getCell(i);

      time = time - (timeZeroBBC);
      if (cell < 0 || cell > 79 )
        {
          time = -1;
        }
      rawTable->setTime(i, time);

    }

  return True;
}

int mNewDchCalibrator::getWireType(int plane)
{
  if (plane < 12)
    {
      return 1;  // X1 wire
    }
  else if (plane < 16)
    {
      return 2; // U1 wire
    }
  else if (plane < 20)
    {
      return 3; // V1 wire
    }
  else if (plane < 32)
    {
      return 4; // X2 wire
    }
  else if (plane < 36)
    {
      return 5; // U2 wire
    }
  else if (plane < 40)
    {
      return 6; // V2 wire
    }
  return 0;
}

float mNewDchCalibrator::slewCorrection(short arm, short plane, float width)
{
  int module = getWireType(plane);
  float cutoff = dchCalibrationObject->slewingArray[arm][module][0];

  float deltatime = 0.;
  if (module == 1 || module == 4)
    {  // not implemented for stereo...yet
      if (width <= cutoff)
        {
          deltatime = dchCalibrationObject->slewingArray[arm][module][1] +
	    dchCalibrationObject->slewingArray[arm][module][2] * width +
	    dchCalibrationObject->slewingArray[arm][module][3] * width * width;
        }
      else
        {
          deltatime = dchCalibrationObject->slewingArray[arm][module][4] +
	    dchCalibrationObject->slewingArray[arm][module][5] * width +
	    dchCalibrationObject->slewingArray[arm][module][6] * width * width;
        }
    }
  return deltatime;
}
