#include <mTecAnalysisModule.h>

#include <TecOutV1.hh>
#include <TecTrack.hh>
#include <dDchTracksWrapper.h>
#include <dDchHitWrapper.h>
#include <headerWrapper.h>
#include <fkinWrapper.h>
#include <tecghitWrapper.h>
#include <BbcOut.h>

#include <PHPolyLine.h>

#include <PHNode.h>
#include <PHIODataNode.h>
#include <PHTypedNodeIterator.h>

#include <TFile.h>
#include <TH1.h>
#include <TF1.h>
#include <TH2.h>
#include <TNtuple.h>
#include <TProfile.h>
#include <TROOT.h>

#include <gsl/gsl_math.h>

#include <algorithm>
#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <map>
#include <vector>

using namespace std;

typedef PHIODataNode<TecOut> TecOutNode_t;
typedef PHIODataNode<dDchTracksWrapper> dDchTracksNode_t;
typedef PHIODataNode<dDchHitWrapper> dDchHitNode_t;
typedef PHIODataNode<headerWrapper> headerNode_t;
typedef PHIODataNode<fkinWrapper> fkinNode_t;
typedef PHIODataNode<tecghitWrapper> tecghitNode_t;
typedef PHIODataNode <BbcOut> BbcOutNode_t;

//=============================================================

mTecAnalysisModule::mTecAnalysisModule()
{
  Verbose = 0;
  WriteHistograms = 0;
}

void mTecAnalysisModule::findDominantContributor(PHCompositeNode* root)
{

}

void mTecAnalysisModule::checkAlignment(PHCompositeNode* topNode)
{
  TecOutV1* tecout;
  PHTypedNodeIterator<TecOut> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOut");
  if (!TecOutNode)
    {
      cerr << "ERROR: Can not find TecOut !!!" << endl;
      return;
    }
  tecout = (TecOutV1*)TecOutNode->getData();
  cout << "TECOUT: " << tecout->getNHits() << " " << tecout->getNTracks() << endl;

  PHTypedNodeIterator<dDchTracksWrapper> iDTN(topNode);
  dDchTracksNode_t *DTN = iDTN.find("dDchTracks");
  if (!DTN)
    {
      cerr << "ERROR: dDchTracks table not found !!!" << endl;
      return;
    }
  dDchTracksWrapper* dDchTracks = DTN->getData();
  dDchTracks->Show();

  static int eventNumber = 0;
  static int first = 0;
  static TNtuple *ntp1;
  float ntpart[4];

  if (first == 0)
    {
      first = 1;
      ntp1 = new TNtuple("ntp1", "alignment", "sector:side:plane:dist");
    }

  for (unsigned int i = 0; i < dDchTracks->RowCount(); i++)
    {
      cout << i << " "
      << dDchTracks->get_arm(i) << " "
      << dDchTracks->get_quality(i) << " "
      << dDchTracks->get_side(i) << " "
      << dDchTracks->get_point(0, i) << " "
      << dDchTracks->get_point(1, i) << " "
      << dDchTracks->get_point(2, i) << " "
      << dDchTracks->get_direction(0, i) << " "
      << dDchTracks->get_direction(1, i) << " "
      << dDchTracks->get_direction(2, i) << " "
      << endl;
      float xin = dDchTracks->get_point(0, i);
      float yin = dDchTracks->get_point(1, i);
      float xout = dDchTracks->get_point(0, i) + dDchTracks->get_direction(0, i);
      float yout = dDchTracks->get_point(1, i) + dDchTracks->get_direction(1, i);
      float a = (yin - yout) / (xin - xout);
      float b = yin - a * xin;
      float aa = sqrt(a * a + 1.0);

      if (dDchTracks->get_arm(i) == 0 && dDchTracks->get_quality(i) > 20)
        {
          for (int j = 0; j < tecout->getNHits(); j++)
            {
              if (tecout->getHitSide(j) == dDchTracks->get_side(i))
                {
                  float x0 = tecout->getHitX(j);
                  float y0 = tecout->getHitY(j);
                  int trkid = tecout->getHitTrackID(j);
                  if (x0 != 0. && y0 != 0. && trkid > -1)
                    {
                      float dist = (y0 - a * x0 - b) / aa;
                      ntpart[0] = (float)tecout->getHitSector(j);
                      ntpart[1] = (float)tecout->getHitSide(j);
                      ntpart[2] = (float)tecout->getHitPlane(j);
                      ntpart[3] = dist;
                      ntp1->Fill(ntpart);
                    }
                }

            }
        } // arm==0

    }
  // this needs to be implemented without the histo factory
  //  hf->Save("tmp.root");

  eventNumber++;

  return;
}

void mTecAnalysisModule::setupTecOut(PHCompositeNode *topNode)
{
  PHNodeIterator nodeIter(topNode);

  // Find tecNode in the node tree

  PHCompositeNode* tecNode;
  tecNode = static_cast<PHCompositeNode*>(nodeIter.findFirst("PHCompositeNode", "TEC"));
  if (!tecNode)
    {
      cout << "mTecAnalysisModule::setupTecOut: TEC node does not exist." << endl;
      return;
    }
  else
    {
      cout << "mTecAnalysisModule::setupTecOut: tecNode FOUND." << endl;
    }

  // Add TecOut to tecNode

  TecOut* tecout = new TecOutV1();
  PHIODataNode<PHObject>* TecOutNode = new PHIODataNode<PHObject>(tecout, "TecOut", "PHObject");
  tecNode->addNode(TecOutNode);
  cout << "mTecAnalysisModule::setupTecOut: TecOut added to tecNode." << endl;

}

void mTecAnalysisModule::fillTecOut(PHCompositeNode *topNode)
{
  // Find TecOut in the node tree

  PHTypedNodeIterator<TecOut> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOut");
  TecOutV1* tecout = (TecOutV1*)TecOutNode->getData();

  // add some hits to TecOut
  cout << "mTecAnalysisModule::fillTecOut: started adding hits." << endl;

  for (int i = 0; i < 12; i++)
    {
      int index = 0;
      int wire = i;
      int bin = i * 2;
      int adc = i * 10;
      float charge = 99.;
      float xyz[3];
      xyz[0] = -100.;
      xyz[1] = -200.;
      xyz[2] = -300.;
      int trackid = -1;

      int tmp = tecout->AddTecHit(index, wire, bin, adc, charge, xyz, trackid);
      cout << "mTecAnalysisModule::fillTecOut: current number of hits: " << tmp
      << " out of " << tecout->getMaxNHits() << endl;

    }

  // Change values
  tecout->setHitCharge(3, 88.);
  tecout->setHitX(0, 666.);

  // Add some tracks to TecOut
  cout << "mTecAnalysisModule::fillTecOut: started adding tracks." << endl;

  for (int i = 0; i < 3; i++)
    {
      float xyzin[3], xyzout[3];
      xyzin[0] = -500. + i * 10.;
      xyzin[1] = i * 15.;
      xyzin[2] = 99.;
      xyzout[0] = -560. + i * 10.;
      xyzout[1] = i * 25.;
      xyzout[2] = 99.;

      int tmp = tecout->AddTecTrack(xyzin, xyzout);
      cout << "mTecAnalysisModule::fillTecOut: current number of tracks: "
      << tmp << endl;

    }

  cout << "mTecAnalysisModule::fillTecOut: ended: "
  << tecout->getNHits() << " " << tecout->getNTracks() << endl;

}

void mTecAnalysisModule::testTecOut(PHCompositeNode *topNode, int stophits)
{
  // Find TecOut in the node tree
  TecOutV1* tecout;
  PHTypedNodeIterator<TecOut> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOut");
  if (TecOutNode)
    {
      tecout = (TecOutV1*)TecOutNode->getData();
    }
  else
    {
      cerr << "mTecAnalysisModule::testTecOut ERROR: TecOut not found." << endl;
      return;
    }

  // Print TecOut contents
  cout << "mTecAnalysisModule::testTecOut: " << tecout->getNHits() << " " << tecout->getNTracks() << endl;

  int stop = stophits;
  if (tecout->getNHits() < stop)
    {
      stop = tecout->getNHits();
    }
  for (int i = 0; i < stop; i++)
    {
      cout << i << " " << tecout->getHitIndex(i) << " "
      << tecout->getHitWire(i) << " "
      << tecout->getHitTimeBin(i) << " "
      << tecout->getHitADC(i) << " "
      << tecout->getHitCharge(i) << " "
      << tecout->getHitX(i) << " " << tecout->getHitY(i) << " "
      << tecout->getHitTrackID(i) << endl;
    }

  for (int i = 0; i < tecout->getNTracks(); i++)
    {
      cout << i << " " << tecout->getTrackXin(i) << " "
      << tecout->getTrackYout(i) << " "
      << tecout->getTrackAlpha(i) << " "
      << tecout->getTrackSector(i) << " "
      << tecout->getTrackSide(i) << " "
      << tecout->getTrackNhits(i) << " "
      << tecout->getTrackPhi(i) << endl;
    }

}

void mTecAnalysisModule::resetTecOut(PHCompositeNode *topNode)
{
  TecOutV1* tecout;
  PHTypedNodeIterator<TecOut> teciter(topNode);
  TecOutNode_t *TecOutNode = teciter.find("TecOut");
  if (TecOutNode)
    {
      tecout = (TecOutV1*)TecOutNode->getData();
    }
  else
    {
      cerr << "mTecAnalysisModule::resetTecOut ERROR: TecOut not found." << endl;
      return;
    }

  cout << "mTecAnalysisModule::resetTecOut: clearing TecOut." << endl;
  tecout->Reset();

  cout << "mTecAnalysisModule::resetTecOut: " << tecout->getNHits()
  << " " << tecout->getNTracks() << endl;
}

PHBoolean mTecAnalysisModule::event(PHCompositeNode *root)
{
  return False;
}

