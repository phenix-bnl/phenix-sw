////////////////////////////////////////////////////////////////////////
//
//  Simple code to fill alpha v. phi histos with quality tracks
//  for beam-shift calculation
//
////////////////////////////////////////////////////////////////////////

//============Libraries==========================
#include <DchAlphaPhi.h>
#include <PHCentralTrack.h>
#include <DchTrack.h>
#include <DchSnglTrackv1.h>
#include <DchHitLineTable.hh>
#include <PHGlobal.h>
#include <PHCompositeNode.h>
#include <phool.h>
#include <Fun4AllHistoManager.h>
#include <recoConsts.h>
#include <getClass.h>
#include <PHPoint.h>
#include <PHCylPoint.h>

#include <TH1.h>
#include <TH2.h>

#include <gsl/gsl_math.h>

#include <cstdlib>

//=================Constructor===================

DchAlphaPhi::DchAlphaPhi(Fun4AllHistoManager *HM) : SubsysReco("DchAlphaPhi")
{
  std::cout << "Constructing the Beam Offset Histo-filler" << std::endl;
  hm = HM;
}

//=====================Registering Histos=========

int DchAlphaPhi::Init(PHCompositeNode *topNode)
{
  std::cout << "DchAlphaPhi::Init " << std::endl;

  //  Status histogram (VGR)
  Status = new TH1F("Status",    "", 10, -0.5, 9.5);
  hm->registerHisto(Status);
  //  PHCentralTrack alpha v phi
  PHAlphaPhiAll   =  new TH2F("PHAlphaPhiAll",    "", 1000, -M_PI / 2., 3.0*(M_PI / 2.), 1000, -0.1, 0.1);
  hm->registerHisto(PHAlphaPhiAll);
  PHAlphaPhiEast  =  new TH2F("PHAlphaPhiEast",   "", 1000, -M_PI / 2., 3.0*(M_PI / 2.), 1000, -0.1, 0.1);
  hm->registerHisto(PHAlphaPhiEast);
  PHAlphaPhiWest  =  new TH2F("PHAlphaPhiWest",   "", 1000, -M_PI / 2., 3.0*(M_PI / 2.), 1000, -0.1, 0.1);
  hm->registerHisto(PHAlphaPhiWest);
  //  Histograms to determine *our* calibration...
  Alpha1East          =  new TH2F("Alpha1East",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1East);
  Alpha2East          =  new TH2F("Alpha2East",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2East);
  Alpha1West          =  new TH2F("Alpha1West",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1West);
  Alpha2West          =  new TH2F("Alpha2West",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2West);
  // Same histograms by sides (VGR)
  Alpha1Side0East     =  new TH2F("Alpha1Side0East",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side0East);
  Alpha2Side0East     =  new TH2F("Alpha2Side0East",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side0East);
  Alpha1Side0West     =  new TH2F("Alpha1Side0West",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side0West);
  Alpha2Side0West     =  new TH2F("Alpha2Side0West",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side0West);
  Alpha1Side1East     =  new TH2F("Alpha1Side1East",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side1East);
  Alpha2Side1East     =  new TH2F("Alpha2Side1East",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side1East);
  Alpha1Side1West     =  new TH2F("Alpha1Side1West",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side1West);
  Alpha2Side1West     =  new TH2F("Alpha2Side1West",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side1West);
  // Same histograms by sides and parity (even<->odd) (VGR)
  Alpha1Side0EvenEast =  new TH2F("Alpha1Side0EvenEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side0EvenEast);
  Alpha2Side0EvenEast =  new TH2F("Alpha2Side0EvenEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side0EvenEast);
  Alpha1Side0EvenWest =  new TH2F("Alpha1Side0EvenWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side0EvenWest);
  Alpha2Side0EvenWest =  new TH2F("Alpha2Side0EvenWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side0EvenWest);
  Alpha1Side1EvenEast =  new TH2F("Alpha1Side1EvenEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side1EvenEast);
  Alpha2Side1EvenEast =  new TH2F("Alpha2Side1EvenEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side1EvenEast);
  Alpha1Side1EvenWest =  new TH2F("Alpha1Side1EvenWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side1EvenWest);
  Alpha2Side1EvenWest =  new TH2F("Alpha2Side1EvenWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side1EvenWest);
  Alpha1Side0OddEast =  new TH2F("Alpha1Side0OddEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side0OddEast);
  Alpha2Side0OddEast =  new TH2F("Alpha2Side0OddEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side0OddEast);
  Alpha1Side0OddWest =  new TH2F("Alpha1Side0OddWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side0OddWest);
  Alpha2Side0OddWest =  new TH2F("Alpha2Side0OddWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side0OddWest);
  Alpha1Side1OddEast =  new TH2F("Alpha1Side1OddEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side1OddEast);
  Alpha2Side1OddEast =  new TH2F("Alpha2Side1OddEast",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side1OddEast);
  Alpha1Side1OddWest =  new TH2F("Alpha1Side1OddWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha1Side1OddWest);
  Alpha2Side1OddWest =  new TH2F("Alpha2Side1OddWest",  "",   80, -0.5, 79.5, 80, -0.04, 0.04);
  hm->registerHisto(Alpha2Side1OddWest);
  //Alpha vs cell (VGR)
  AlphaCellEast  =  new TH2F("AlphaCellEast",   "", 80, -0.5, 79.5, 200, -0.02, 0.02);
  hm->registerHisto(AlphaCellEast);
  AlphaCellWest  =  new TH2F("AlphaCellWest",   "", 80, -0.5, 79.5, 200, -0.02, 0.02);
  hm->registerHisto(AlphaCellWest);
  // Histograms to determine whether centering is good
  DCAlphaPhiEast  =  new TH2F("DCAlphaPhiEast",   "", 160, M_PI / 2., 3.0*(M_PI / 2.), 200, -0.02, 0.02);
  hm->registerHisto(DCAlphaPhiEast);
  DCAlphaPhiWest  =  new TH2F("DCAlphaPhiWest",   "", 160, -M_PI / 2., (M_PI / 2.), 200, -0.02, 0.02);
  hm->registerHisto(DCAlphaPhiWest);
  //Same histograms only Alpha vs phi for X1&X2 (VGR)
  Alpha1PhiEast  =  new TH2F("Alpha1PhiEast",   "", 160, -M_PI / 2., 3.0*(M_PI / 2.), 500, -0.05, 0.05);
  hm->registerHisto(Alpha1PhiEast);
  Alpha2PhiEast  =  new TH2F("Alpha2PhiEast",   "", 160, -M_PI / 2., 3.0*(M_PI / 2.), 500, -0.05, 0.05);
  hm->registerHisto(Alpha2PhiEast);
  Alpha12PhiEast  =  new TH2F("Alpha12PhiEast", "", 160, -M_PI / 2., 3.0*(M_PI / 2.), 500, -0.05, 0.05);
  hm->registerHisto(Alpha12PhiEast);
  Alpha1PhiWest  =  new TH2F("Alpha1PhiWest",   "", 160, -M_PI / 2., 3.0*(M_PI / 2.), 500, -0.05, 0.05);
  hm->registerHisto(Alpha1PhiWest);
  Alpha2PhiWest  =  new TH2F("Alpha2PhiWest",   "", 160, -M_PI / 2., 3.0*(M_PI / 2.), 500, -0.05, 0.05);
  hm->registerHisto(Alpha2PhiWest);
  Alpha12PhiWest  =  new TH2F("Alpha12PhiWest", "", 160, -M_PI / 2., 3.0*(M_PI / 2.), 500, -0.05, 0.05);
  hm->registerHisto(Alpha12PhiWest);
  //Info histograms, give phi vs cell (VGR)
  CellPhiEast    =  new TH2F("CellPhiEast",     "", 160, -M_PI / 2., 3.0*(M_PI / 2.),  80, -0.5, 79.5);
  hm->registerHisto(CellPhiEast);
  CellPhiWest    =  new TH2F("CellPhiWest",     "", 160, -M_PI / 2., 3.0*(M_PI / 2.),  80, -0.5, 79.5);
  hm->registerHisto(CellPhiWest);

  HitPhi = new TH1F("HitPhi",   "", 3600, -M_PI / 2., 3.0*(M_PI / 2.));
  hm->registerHisto(HitPhi);
  Image  = new TH2F("Image",  "", 50, -300, 300, 50, -300, 300);
  hm->registerHisto(Image);

  return 0;
}// Init

int DchAlphaPhi::process_event(PHCompositeNode *topNode)
{
  findNodes(topNode);

  //Loop over all the tracks and fill histograms...
  for (unsigned int i = 0; i < d_cnt->get_npart(); i++)
    {
      float alpha  = d_cnt->get_alpha  (i);
      float phi    = d_cnt->get_phi    (i);
      int arm      = d_cnt->get_dcarm  (i);
      int quality  = d_cnt->get_quality(i);

      if ((quality == 63) || (quality == 31))
        {

          PHAlphaPhiAll->Fill(phi, alpha);
          switch (arm)
            {
            case 0:
              PHAlphaPhiEast->Fill(phi, alpha);
              break;
            case 1:
              PHAlphaPhiWest->Fill(phi, alpha);
              break;
            default:
              std::cout << "Arm " << arm << " not defined!" << std::endl;
              break;
            }
        }
    }

  //  Talk about the hits...
  int maxHits  = d_hit->Entries();
  for (int i = 0; i < maxHits; i++)
    {
      PHPoint     php = d_hit->getXYZ(i);
      PHCylPoint  pcp = php;
      HitPhi->Fill(pcp.getPhi());
      Image->Fill(php.getX(), php.getY());
    }

  //  Loop over all the tracks and fill histograms...
  for (unsigned int i = 0; i < d_dch->get_DchNTrack(); i++)
    {
      DchSnglTrackv1 *trk = d_dch->get_Track(i);
      float alpha  = trk->get_alpha  ();
      float alpha1 = trk->get_alpha1 ();
      float alpha2 = trk->get_alpha2 ();
      float phi    = trk->get_phi    ();
      int   arm    = trk->get_arm  ();
      int   quality = trk->get_quality();
      int   card1  = cardNumber(i,  0, 11);  // do X1 layer...
      int   card2  = cardNumber(i, 20, 31);  // do X2 layer...
      int   card3  = cardNumber(i, 0, 31);  // do X1&&X2 layer...
      int   side   = sideNumber(i);  // Find side ...
      int   wire1   = wireSide(i, 0, 11); //Find if odd or even side of the X1 net
      int   wire2   = wireSide(i, 20, 31); //Find if odd or even side of the X2 net

      if (arm == 0)
        {
          Alpha1East->Fill(card1, alpha1);
          Alpha2East->Fill(card2, alpha2);
          Alpha1PhiEast->Fill(phi, alpha1);
          Alpha2PhiEast->Fill(phi, alpha2);
          Alpha12PhiEast->Fill(phi, alpha1);
          Alpha12PhiEast->Fill(phi, alpha2);
          CellPhiEast->Fill(phi, card1);
          CellPhiEast->Fill(phi, card2);

          if (side == 0)
            {
              Alpha1Side0East->Fill(card1, alpha1);
              Alpha2Side0East->Fill(card2, alpha2);
              if (wire1 == 0)
                {
                  Alpha1Side0EvenEast->Fill(card1, alpha1);
                }
              else if (wire1 == 1)
                {
                  Alpha1Side0OddEast->Fill(card1, alpha1);
                }
              if (wire2 == 0)
                {
                  Alpha2Side0EvenEast->Fill(card2, alpha2);
                }
              if (wire2 == 1)
                {
                  Alpha2Side0OddEast->Fill(card2, alpha2);
                }
            }
          if (side == 1)
            {
              Alpha1Side1East->Fill(card1, alpha1);
              Alpha2Side1East->Fill(card2, alpha2);
              if (wire1 == 0)
                {
                  Alpha1Side1EvenEast->Fill(card1, alpha1);
                }
              else if (wire1 == 1)
                {
                  Alpha1Side1OddEast->Fill(card1, alpha1);
                }
              if (wire2 == 0)
                {
                  Alpha2Side1EvenEast->Fill(card2, alpha2);
                }
              else if (wire2 == 1)
                {
                  Alpha2Side1OddEast->Fill(card2, alpha2);
                }
            }
          if ((quality == 63) || (quality == 31))
            {
              DCAlphaPhiEast->Fill(phi, alpha);
              AlphaCellEast->Fill(card3, alpha);
            }
        }
      else
        {
          Alpha1West->Fill(card1, alpha1);
          Alpha2West->Fill(card2, alpha2);
          Alpha1PhiWest->Fill(phi, alpha1);
          Alpha2PhiWest->Fill(phi, alpha2);
          Alpha12PhiWest->Fill(phi, alpha1);
          Alpha12PhiWest->Fill(phi, alpha2);
          CellPhiWest->Fill(phi, card1);
          CellPhiWest->Fill(phi, card2);
          if (side == 0)
            {
              Alpha1Side0West->Fill(card1, alpha1);
              Alpha2Side0West->Fill(card2, alpha2);
              if (wire1 == 0)
                {
                  Alpha1Side0EvenWest->Fill(card1, alpha1);
                }
              else if (wire1 == 1)
                {
                  Alpha1Side0OddWest->Fill(card1, alpha1);
                }
              if (wire2 == 0)
                {
                  Alpha2Side0EvenWest->Fill(card2, alpha2);
                }
              else if (wire2 == 1)
                {
                  Alpha2Side0OddWest->Fill(card2, alpha2);
                }
            }
          else if (side == 1)
            {
              Alpha1Side1West->Fill(card1, alpha1);
              Alpha2Side1West->Fill(card2, alpha2);
              if (wire1 == 0)
                {
                  Alpha1Side1EvenWest->Fill(card1, alpha1);
                }
              else if (wire1 == 1)
                {
                  Alpha1Side1OddWest->Fill(card1, alpha1);
                }
              if (wire2 == 0)
                {
                  Alpha2Side1EvenWest->Fill(card2, alpha2);
                }
              else if (wire2 == 1)
                {
                  Alpha2Side1OddWest->Fill(card2, alpha2);
                }
            }
          if ((quality == 63) || (quality == 31))
            {
              DCAlphaPhiWest->Fill(phi, alpha);
              AlphaCellWest->Fill(card3, alpha);
            }
        }
    }

  return 0;
}//process event

int DchAlphaPhi::End(PHCompositeNode *topNode)
{

  hm->dumpHistos();

  return 0;
} //End

int DchAlphaPhi::wireSide(int iTrack, short start, short end)
{
  //  Sanity check on track index...
  int maxTrack = d_dch->get_DchNTrack();
  int maxHits  = d_hit->Entries();
  if (iTrack < 0 || iTrack >= maxTrack)
    {
      std::cout << "DchAlphaPhi::Track index out of range: " << iTrack << std::endl;
      return -1;
    }

  //  Determine "best" Net side parity by averaging parity of all hits ...
  float iFound = 0;
  float iSum   = 0;

  for (int iplane = start; iplane <= end; iplane++)
    {
      if (iplane > 11 && iplane < 20)
        {
          continue;
        }
      int hitIndex = d_dch->get_hits(iTrack, iplane);
      if (hitIndex >= 0 && hitIndex < maxHits)
        {
          if (iplane % 2 == 0)
            {
              iFound += 1.0;
            }
          else if (iplane % 2 == 1)
            {
              iFound += 1.0;
              iSum += 1.0;
            }
        }
    }

  if (iFound < 1.0)
    {
      return -1;  // No hits in the given plane range...
    }
  if (iSum == 0)
    {
      return 0;
    }
  if (iSum == iFound)
    {
      return 1;
    }
  return -1; // No tracks with hits of different parities
}

int DchAlphaPhi::sideNumber(int iTrack)
{
  //  Sanity check on track index...
  int maxTrack = d_dch->get_DchNTrack();
  if (iTrack < 0 || iTrack >= maxTrack)
    {
      std::cout << "DchAlphaPhi::Track index out of range: " << iTrack << std::endl;
      return -1;
    }

  return d_dch->get_side(iTrack);
}

int DchAlphaPhi::cardNumber(int iTrack, short start, short end)
{
  //  This routine is called from the process_event.
  //  We assume that the structure pointers like d_dch and d_hit
  //  have already been filled in that routine and we simply
  //  use the pointers here.

  //  Sanity check on track index...
  int maxTrack = d_dch->get_DchNTrack();
  int maxHits  = d_hit->Entries();
  if (iTrack < 0 || iTrack >= maxTrack)
    {
      std::cout << "DchAlphaPhi::Track index out of range: " << iTrack << std::endl;
      return -1;
    }

  //  Determine "best" cell number by averaging cell number of all hit cells...
  float iFound = 0;
  float iSum   = 0;
  int iFirstCard = -1;
  int iChangeCard = -1;
  int iFirstPlane = -1;
  int iChangePlane = -1;

  for (int iplane = start; iplane <= end; iplane++)
    {
      if (iplane > 11 && iplane < 20)
        {
          continue;
        }
      int hitIndex = d_dch->get_hits(iTrack, iplane);
      if (hitIndex >= 0 && hitIndex < maxHits)
        {
          if (iFirstCard == -1)
            {
              iFirstCard = d_hit->getCell(hitIndex);
            }
          if ((d_hit->getCell(hitIndex)) != iFirstCard)
            {
              iChangeCard = 1;
            }
          if (iFirstPlane == -1)
            {
              iFirstPlane = iplane;
            }
          if (iplane % 2 != iFirstPlane % 2)
            {
              iChangePlane = 1;
            }
          iFound += 1.0;
          iSum += d_hit->getCell(hitIndex);
        }
    }

  if (iFound < 1.0)
    {
      return -1;  // No hits in the given plane range...
    }
  if (iChangeCard == 1)
    {
      return -1; // Crossed cathods <-> do not want to deal with geometry/(t0,dv) matters (VGR)
    }
  if ((start == 0 && end == 11) || (start == 20 && end == 31))
    {
      if (iChangePlane == 1)
        {
          return -1; // Crossed board <-> do not want to deal with geometry/(t0,dv) matters (VGR)
        }
    }

  return (int)floor(iSum / iFound + 0.5); // board of planes used on track...
}


//--------------------------------------------------------
int DchAlphaPhi::findNodes(PHCompositeNode *topNode)
{

  d_cnt = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if (!d_cnt)
    {
      std::cout << "No PHCentralTrack Node!!" << std::endl;
      exit(1);
    }

  d_dch = findNode::getClass<DchTrack>(topNode, "DchTrack");
  if (!d_dch)
    {
      std::cout << "No DchTrack Node!!" << std::endl;
      exit(1);
    }

  d_hit = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTable");
  if (!d_hit)
    {
      std::cout << "No DchHitLineTable Node!!" << std::endl;
      exit(1);
    }

  global = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (!global)
    {
      std::cout << PHWHERE << "No PHGlobal Node!!" << std::endl;
      exit(1);
    }

  return 0;
}//findnodes

