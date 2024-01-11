/*!
  \file mCentralTrackEvaluator_v1.C
  \author Ralf Averbeck
  \version $Revision: 1.25 $
  \date $Date: 2017/07/13 18:53:08 $
*/

#include "mCentralTrackEvaluator_v1.hh"

#include <CrkPID.hh>
#include <DchAnaPar.h>
#include <tofghitWrapper.h>

#include <dTofGdigiWrapper.h>
#include <dTofGdigiRecWrapper.h>
#include <dcghitWrapper.h>
#include <DchHitLineTable.hh>
#include <dDchGhitHitsWrapper.h>
#include <dDchTracksWrapper.h>
#include <DchTrack.h>
#include <RecoEvalSingleTrack_v2.h>
#include <McEvalSingleTrack_v2.h>
#include <McEvalSingleList_v2.h>
#include <fkinWrapper.h>
#include <PHTrackOut.h>
#include <CglTrack.h>
#include <dPadClusterWrapper.h>
#include <pcghitWrapper.h>
#include <dPadGhitClusWrapper.h>
#include <dEmcGeaClusterTrackWrapper.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <dCrkHitWrapper.h>
#include <dTofReconstructedWrapper.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>

#include <PHLine.h>

#include <getClass.h>

#include <iostream>
#include <sstream>

using namespace std;


//________________________________________________________________
mCentralTrackEvaluator_v1::mCentralTrackEvaluator_v1():
  verbose(0),
  EventCounter(0),
  EvalList(NULL),
  d_crkpid( new CrkPID(-1) ),
  HitList(NULL),
  TrackList(NULL),
  dcghit(NULL),
  dDchGhitHits(NULL),
  dDchTracksPerf(NULL),
  fkin(NULL)
{}

//________________________________________________________________
mCentralTrackEvaluator_v1::~mCentralTrackEvaluator_v1()
{
  delete d_crkpid;
  return;
}
//________________________________________________________________
PHBoolean mCentralTrackEvaluator_v1::event(PHCompositeNode *topNode)
{


  // compares between perfect tracking and reconstruction


  dcghit = findNode::getClass<dcghitWrapper>(topNode, "dcghit");
  dDchGhitHits = findNode::getClass<dDchGhitHitsWrapper>(topNode, "dDchGhitHits");
  dDchTracksPerf = findNode::getClass<dDchTracksWrapper>(topNode, "dDchTracksPerf");
  EvalList = findNode::getClass<McEvalSingleList>(topNode, "McSingle");
  fkin    = findNode::getClass<fkinWrapper>(topNode, "fkin");
  HitList = findNode::getClass<DchHitLineTable>(topNode, "DchHitLineTable");
  TrackList = findNode::getClass<DchTrack>(topNode, "DchTrack");

  int possibleSolution;

  int numberOfPerfectTracks = dDchTracksPerf->RowCount();
  int numberOfReconstructedTracks = TrackList->get_DchNTrack();

  EventCounter++;
  if (verbose)  cout << "event counter: " << EventCounter << endl;

  if (verbose )
    {
      // show only for the first 100 events unless verbose is set to true
      cout << "perfect tracks: " << numberOfPerfectTracks << endl;
      cout << "reconstructed tracks: " << numberOfReconstructedTracks << endl;
    }

  EvalList->Reset();
  int numberOfMcTracks = EvalList->get_McEvalSingleTrackN();
  if ( numberOfMcTracks != 0 )
    {

      cout <<  PHWHERE << " error resetting the evaluation list" << endl;

    }

  // perform the main contributor analysis for a reconstructed track
  int mulmain, mulxmain, muluvmain, ambmain, ambxmain, ambuvmain;
  int main, xmain, uvmain, xhits, uvhits;
  float puremain, purexmain, pureuvmain;
  int xHitsRecoTrack[numberOfReconstructedTracks];
  int uvHitsRecoTrack[numberOfReconstructedTracks];
  int MulmainContributorRecoTrack[numberOfReconstructedTracks];
  int MulxmainContributorRecoTrack[numberOfReconstructedTracks];
  int MuluvmainContributorRecoTrack[numberOfReconstructedTracks];
  int AmbmainContributorRecoTrack[numberOfReconstructedTracks];
  int AmbxmainContributorRecoTrack[numberOfReconstructedTracks];
  int AmbuvmainContributorRecoTrack[numberOfReconstructedTracks];
  int mainContributorRecoTrack[numberOfReconstructedTracks];
  int xmainContributorRecoTrack[numberOfReconstructedTracks];
  int uvmainContributorRecoTrack[numberOfReconstructedTracks];
  float PuritymainContributorRecoTrack[numberOfReconstructedTracks];
  float PurityxmainContributorRecoTrack[numberOfReconstructedTracks];
  float PurityuvmainContributorRecoTrack[numberOfReconstructedTracks];

  // pre-save these for quicker access when calculating min distance (N^2)
  float phi[numberOfReconstructedTracks];
  float alpha[numberOfReconstructedTracks];
  for (int m = 0; m < numberOfReconstructedTracks; m++)
    {
      mainContributorCalculation(m, xhits, uvhits,
                                 mulmain, mulxmain, muluvmain,
                                 ambmain, ambxmain, ambuvmain,
                                 main, xmain, uvmain,
                                 puremain, purexmain, pureuvmain);

      xHitsRecoTrack[m] = xhits;
      uvHitsRecoTrack[m] = uvhits;
      MulmainContributorRecoTrack[m] = mulmain;
      MulxmainContributorRecoTrack[m] = mulxmain;
      MuluvmainContributorRecoTrack[m] = muluvmain;
      AmbmainContributorRecoTrack[m] = ambmain;
      AmbxmainContributorRecoTrack[m] = ambxmain;
      AmbuvmainContributorRecoTrack[m] = ambuvmain;
      mainContributorRecoTrack[m] = main;
      xmainContributorRecoTrack[m] = xmain;
      uvmainContributorRecoTrack[m] = uvmain;
      PuritymainContributorRecoTrack[m] = puremain;
      PurityxmainContributorRecoTrack[m] = purexmain;
      PurityuvmainContributorRecoTrack[m] = pureuvmain;

      phi[m]   = TrackList->get_phi(m);
      alpha[m] = TrackList->get_alpha(m);
    }

  int   itparent, itprimary, idparent, idpart;
  float ptheta, pphi, r_vertex, z_vertex, phi_vertex;

  float xparticleVTX, yparticleVTX, zparticleVTX;
  float xparentVTX, yparentVTX, zparentVTX;
  float xprimaryVTX, yprimaryVTX, zprimaryVTX;
  float momentumG, parentmomentumG, primarymomentumG, momentumR;
  int generation( 0 );
  int   particleID, parentID, primaryID;
  int   qualityG, qualityR;
  float alphaG, alphaR;
  float betaG, betaR;
  float phiG;
  float theta0G, phi0G;
  float parenttheta0G, parentphi0G;
  float primarytheta0G, primaryphi0G;
  float theta0R, phi0R;
  float phiR;
  float zedG, zedR;
  float dalphaMin, dphiMin;

  int noSolution[20];
  int perfectTracks[20];

  for (int j = 0; j < 20; j++)
    {
      noSolution[j]    = 0;
      perfectTracks[j] = 0;
    }

  for ( int i = 0; i < numberOfPerfectTracks; i++)
    {

      // store relevant information from DCH perfect tracks
      momentumG = dDchTracksPerf->get_momentum(i);
      qualityG  = dDchTracksPerf->get_quality(i);
      alphaG    = dDchTracksPerf->get_alpha(i);
      betaG     = dDchTracksPerf->get_beta(i);
      phiG      = dDchTracksPerf->get_phi(i);
      zedG      = dDchTracksPerf->get_zed(i);

      if (qualityG == 0)   (perfectTracks[0])++;
      if (qualityG&0x01) (perfectTracks[1])++;
      if (qualityG&0x02) (perfectTracks[2])++;
      if (qualityG&0x04) (perfectTracks[3])++;
      if (qualityG&0x08) (perfectTracks[4])++;

      // analyze the ancestry information related to idGeantTrack
      int idGeantTrack = -1;
      int fkinIndex = -1;

      // find the mctrack id which correspond to this track
      for (int k = 0; k < numberOfPlanes; k++)
        {
          int dcghitID = dDchTracksPerf->get_hits(k, i);
          if (dcghitID != -1)
            {

              // for main contributor Analysis
              idGeantTrack = dcghit->get_mctrack(dcghitID);
              break;
            }
        }

      for (unsigned int k = 0; k < fkin->RowCount(); k++)
        {

          if ( fkin->get_true_track(k) == idGeantTrack )
            {
              fkinIndex = k;
              break;
            }

        }

      if ( fkinIndex < 0 )
        {
          cout << PHWHERE << " unable to find matching fKin hit" << endl;
          continue;
        }

      ptheta    = fkin->get_pthet(fkinIndex);
      pphi      = fkin->get_pphi(fkinIndex);
      idparent  = fkin->get_idparent(fkinIndex);
      idpart    = fkin->get_idpart(fkinIndex);
      r_vertex  = fkin->get_r_vertex(fkinIndex);
      z_vertex  = fkin->get_z_vertex(fkinIndex);
      phi_vertex = fkin->get_ph_vertx(fkinIndex);
      itparent  = fkin->get_itparent(fkinIndex);

      theta0G = Pi * ptheta / 180.;
      phi0G   = Pi * pphi / 180.;
      if (phi0G < -Pi / 2.) phi0G += TwoPi;

      xparticleVTX = r_vertex * cos(Pi * phi_vertex / 180.);
      yparticleVTX = r_vertex * sin(Pi * phi_vertex / 180.);
      zparticleVTX = z_vertex;

      particleID = idpart;

      parentID         = 0;
      primaryID        = 0;
      xparentVTX       = 0.;
      yparentVTX       = 0.;
      zparentVTX       = 0.;
      xprimaryVTX      = 0.;
      yprimaryVTX      = 0.;
      zprimaryVTX      = 0.;
      parentmomentumG  = 0.;
      parenttheta0G    = 0.;
      parentphi0G      = 0.;
      primarymomentumG = 0.;
      primarytheta0G   = 0.;
      primaryphi0G     = 0.;

      // check if current track is a primary particle
      // or look for matching primary particle otherwise
      if (idparent == 0)
        {

          if (verbose)
            {
              cout
                << "mCentralTrackEvaluator_v1::callPAM -"
                << " mc_track: " << idGeantTrack
                << " is primary."
                << endl;
            }
          generation = 1;

        }
      else
        {

          if (idparent > 0)
            {

              if (verbose)
                {
                  cout
                    << "mCentralTrackEvaluator_v1::callPAM -"
                    << " mc_track: " << idGeantTrack
                    << " itparent: " << itparent << endl;
                }
              generation = 2;

            }
          else
            {

              cout << "mCentralTrackEvaluator_v1::callPAM - incorrect idParent value: " << idparent << endl;
              continue;

            }

          if ( generation == 2 ) parentID = idparent;
          fkinIndex = -1;
          for (unsigned int k = 0; k < fkin->RowCount(); k++)
            {

              if ( fkin->get_true_track(k) == itparent )
                {

                  fkinIndex = k;
                  break;

                }

            }

          if ( fkinIndex < 0 )
            {
              cout << "mCentralTrackEvaluator_v1::callPAM - unable to find fKinHit matching particle parent - itparent: " << itparent << endl;
              continue;
            }

          // store origin of parent particle
          int newgen = 2;//gen>=2;
          xparentVTX = fkin->get_r_vertex(fkinIndex)  * cos(Pi * fkin->get_ph_vertx(fkinIndex) / 180.);
          yparentVTX = fkin->get_r_vertex(fkinIndex) * sin(Pi * fkin->get_ph_vertx(fkinIndex) / 180.);
          zparentVTX = fkin->get_z_vertex(fkinIndex);
          parenttheta0G = Pi * fkin->get_pthet(fkinIndex) / 180.;
          parentphi0G   = Pi * fkin->get_pphi(fkinIndex) / 180.;
          if ( parentphi0G < -Pi / 2. ) parentphi0G += TwoPi;
          parentmomentumG = fkin->get_ptot(fkinIndex);

          // parce fKin to find the primary particle matching the current
          // track along the decay chain.
          bool error = false;
          while ( fkin->get_idparent(fkinIndex) != 0 && !error )
            {

              if (verbose)
                {
                  cout << PHWHERE
                       << " mc_track: " << fkin->get_true_track(fkinIndex)
                       << " itparent: " << fkin->get_itparent(fkinIndex) << endl;
                }
              itprimary = fkin->get_itparent(fkinIndex);

              newgen++;
              if ( itprimary < 0 )
                {
                  error = true;
                  cout <<  "mCentralTrackEvaluator_v1::callPAM - error in track ancestry" << endl;
                  break;
                }

              fkinIndex = -1;
              for (unsigned int k = 0; k < fkin->RowCount(); k++)
                {
                  if ( fkin->get_true_track(k) == itprimary )
                    {
                      fkinIndex = k;
                      break;
                    }
                }

              if ( fkinIndex < 0 )
                {
                  cout << "mCentralTrackEvaluator_v1::callPAM - unable to find fKinHit matching particle primary - itprimary: " << itprimary << endl;
                  error = true;
                  break;
                }

            }

          if ( error ) continue;
          else
            {

              if (verbose) cout << "mCentralTrackEvaluator_v1::callPAM - mc_track: " << fkin->get_true_track(fkinIndex) << " is primary." << endl;

            }

          // store origin of primary particle
          generation = newgen;
          primaryID   = fkin->get_idpart(fkinIndex);
          xprimaryVTX = fkin->get_r_vertex(fkinIndex) * cos(Pi * fkin->get_ph_vertx(fkinIndex) / 180.);
          yprimaryVTX = fkin->get_r_vertex(fkinIndex) * sin(Pi * fkin->get_ph_vertx(fkinIndex) / 180.);
          zprimaryVTX = fkin->get_z_vertex(fkinIndex);

          primarytheta0G = Pi * fkin->get_pthet(fkinIndex) / 180.;
          primaryphi0G   = Pi * fkin->get_pphi(fkinIndex) / 180.;
          if ( primaryphi0G < -Pi / 2. ) primaryphi0G += TwoPi;
          primarymomentumG = fkin->get_ptot(fkinIndex);

        }

      if (verbose)
        {
          cout
            << "mCentralTrackEvaluator_v1::callPAM -"
            << " mc_track: " << idGeantTrack
            << " generations: " << generation
            << endl
            << endl;
        }

      numberOfMcTracks++;

      // save in MCEvalSingleTrack object
      McEvalSingleTrack_v2 *mcparticle = new McEvalSingleTrack_v2();

      mcparticle->set_eventid(EventCounter);
      mcparticle->set_mctrackid(idGeantTrack);

      mcparticle->set_generation( generation );
      mcparticle->set_particleid(particleID);
      mcparticle->set_vertexx(xparticleVTX);
      mcparticle->set_vertexy(yparticleVTX);
      mcparticle->set_vertexz(zparticleVTX);
      mcparticle->set_momentumx(momentumG*sin(theta0G)*cos(phi0G));
      mcparticle->set_momentumy(momentumG*sin(theta0G)*sin(phi0G));
      mcparticle->set_momentumz(momentumG*cos(theta0G));
      mcparticle->set_quality(qualityG);
      mcparticle->set_momentumr(momentumG);
      mcparticle->set_theta0(theta0G);
      mcparticle->set_phi0(phi0G);
      mcparticle->set_phi(phiG);
      mcparticle->set_alpha(alphaG);
      mcparticle->set_zed(zedG);
      mcparticle->set_beta(betaG);
      if ( generation > 1 )
        {

          mcparticle->set_parentid(parentID);
          mcparticle->set_parentvertexx(xparentVTX);
          mcparticle->set_parentvertexy(yparentVTX);
          mcparticle->set_parentvertexz(zparentVTX);
          mcparticle->set_parentmomentumx(parentmomentumG*sin(parenttheta0G)*cos(parentphi0G));
          mcparticle->set_parentmomentumy(parentmomentumG*sin(parenttheta0G)*sin(parentphi0G));
          mcparticle->set_parentmomentumz(parentmomentumG*cos(parenttheta0G));
          mcparticle->set_primaryid(primaryID);
          mcparticle->set_primaryvertexx(xprimaryVTX);
          mcparticle->set_primaryvertexy(yprimaryVTX);
          mcparticle->set_primaryvertexz(zprimaryVTX);
          mcparticle->set_primarymomentumx(primarymomentumG*sin(primarytheta0G)*cos(primaryphi0G));
          mcparticle->set_primarymomentumy(primarymomentumG*sin(primarytheta0G)*sin(primaryphi0G));
          mcparticle->set_primarymomentumz(primarymomentumG*cos(primarytheta0G));

        }

      int numberOfReconstructedTracksWithCorrectMainContributor = 0;
      for ( int k = 0; k < numberOfReconstructedTracks; k++)
        {

          if (idGeantTrack == mainContributorRecoTrack[k])
            {
              numberOfReconstructedTracksWithCorrectMainContributor++;
            }

        }

      possibleSolution = 0;
      for ( int k = 0; k < numberOfReconstructedTracks; k++)
        {

          momentumR = TrackList->get_momentum(k);
          qualityR  = TrackList->get_quality(k);
          alphaR    = TrackList->get_alpha(k);
          betaR     = TrackList->get_beta(k);
          phiR      = TrackList->get_phi(k);
          zedR      = TrackList->get_zed(k);
          theta0R   = TrackList->get_theta0(k);
          phi0R     = TrackList->get_phi0(k);

          int numHits = 0;
          float averageTime = 0.;
          float thisHitsTime = 0.;
          for (int n = 0; n < 39; n++)
            {

              int hid = TrackList->get_hits(k, n);
              if ( hid != -1 )
                {

                  int planeOfHit = HitList->getPlane(hid);
                  short type = wireType[planeOfHit];
                  if (type == X1Wire || type == X2Wire)
                    {
                      numHits++;
                      thisHitsTime = HitList->getTime1(hid);
                      averageTime += thisHitsTime;
                    }
                }
            }

          if ( numHits > 0 )
            {

              averageTime = averageTime / (float)numHits;

            }
          else
            {

              averageTime = 99999.;

            }

          dalphaMin = 99999.;
          dphiMin   = 99999.;
          for ( int l = 0; l < numberOfReconstructedTracks; l++)
            {
              if ( l == k ) continue;
              if (fabs(phiR - phi[l]) < dphiMin) dphiMin = fabs(phiR - phi[l]);
              if (fabs(alphaR - alpha[l]) < dalphaMin) dalphaMin = fabs(alphaR - alpha[l]);
            }

          if (idGeantTrack == mainContributorRecoTrack[k])
            {
              RecoEvalSingleTrack_v2 *recoparticle = new RecoEvalSingleTrack_v2();
              recoparticle->set_recoid(k);
              recoparticle->set_quality(qualityR);
              recoparticle->set_momentum(momentumR);
              recoparticle->set_theta0(theta0R);
              recoparticle->set_phi0(phi0R);
              recoparticle->set_phi(phiR);
              recoparticle->set_alpha(alphaR);
              recoparticle->set_zed(zedR);
              recoparticle->set_beta(betaR);
              recoparticle->set_averagetime(averageTime);
              recoparticle->set_xhits(xHitsRecoTrack[k]);
              recoparticle->set_uvhits(uvHitsRecoTrack[k]);
              recoparticle->set_mulmain(MulmainContributorRecoTrack[k]);
              recoparticle->set_mulxmain(MulxmainContributorRecoTrack[k]);
              recoparticle->set_muluvmain(MuluvmainContributorRecoTrack[k]);
              recoparticle->set_main(mainContributorRecoTrack[k]);
              recoparticle->set_xmain(xmainContributorRecoTrack[k]);
              recoparticle->set_uvmain(uvmainContributorRecoTrack[k]);
              recoparticle->set_ambiguity(AmbmainContributorRecoTrack[k]
                                          + 2*AmbxmainContributorRecoTrack[k]
                                          + 4*AmbuvmainContributorRecoTrack[k]);
              recoparticle->set_purity(PuritymainContributorRecoTrack[k]);
              recoparticle->set_xpurity(PurityxmainContributorRecoTrack[k]);
              recoparticle->set_uvpurity(PurityuvmainContributorRecoTrack[k]);
              mcparticle->add_RecoEvalSingleTrack(recoparticle);
            }

          if (idGeantTrack == mainContributorRecoTrack[k])
            {

              possibleSolution++;

            }

        }

      if (!possibleSolution)
        {

          if (qualityG == 0)   (noSolution[0])++;
          if (qualityG&0x01) (noSolution[1])++;
          if (qualityG&0x02) (noSolution[2])++;
          if (qualityG&0x04) (noSolution[3])++;
          if (qualityG&0x08) (noSolution[4])++;

        }

      EvalList->set_McEvalSingleTrackN(numberOfMcTracks);
      EvalList->AddMcEvalSingleTrack(mcparticle, numberOfMcTracks - 1);

    }

  if (verbose)
    {
      if (perfectTracks[1] > 0)
        {
          cout << "Efficiency for tracks with > 2hits: "
               << perfectTracks[1] << " "
               << 1.0 - (float)noSolution[1] / (float)perfectTracks[1] << endl;
        }
      else
        {

          cout << "Efficiency for tracks with > 2hits: 0." << endl;

        }

      if (perfectTracks[2] > 0)
        {

          cout << "Efficiency for tracks with intersection: "
               << perfectTracks[2] << " "
               << 1.0 - (float)noSolution[2] / (float)perfectTracks[2] << endl;

        }
      else
        {

          cout << "Efficiency for tracks with intersection: 0" << endl;

        }

      if (perfectTracks[3] > 0)
        {

          cout << "Efficiency for tracks with through full DC (>=6 X1 && >=2 V2): "
               << perfectTracks[3] << " "
               << 1.0 - (float)noSolution[3] / (float)perfectTracks[3] << endl;

        }
      else
        {

          cout << "Efficiency for tracks with through full DC (>=6 X1 && >=2 V2): 0" << endl;

        }

      if (perfectTracks[4] > 0)
        {

          cout << "Efficiency for tracks with momentum >= 200 MeV/c: "
               << perfectTracks[4] << " "
               << 1.0 - (float)noSolution[4] / (float)perfectTracks[4] << endl;

        }
      else
        {

          cout << "Efficiency for tracks with momentum >= 200 MeV/c: 0" << endl;

        }

    }

  return True;

}

PHBoolean mCentralTrackEvaluator_v1::mainContributorCalculation(int& rid, int& xhitmul, int& uvhitmul, int& mul, int& xmul,
								int& uvmul, int& amb, int& ambx, int& ambuv, int& gid, int& gxid, int& guvid,
								float& pure, float& purex, float& pureuv)
{

  char  cont;

  int plane, k;
  int contribExists, xcontribExists, uvcontribExists;
  int numberOfHits, numberOfXHits, numberOfUVHits;
  int originalTrackIdPerPlane[numberOfPlanes];
  int wireTypePerPlane[numberOfPlanes];
  for (plane = 0; plane < numberOfPlanes; plane++)
    {
      originalTrackIdPerPlane[plane] = -1;
      wireTypePerPlane[plane] = -1;
    }

  int mulmain                = 0;
  int mainContributor        = -1;
  int previousCounter        = 0;
  int finalMainContributor   = -1;
  int ambiguous              = 0;
  int mulxmain               = 0;
  int xmainContributor       = -1;
  int xpreviousCounter       = 0;
  int xfinalMainContributor  = -1;
  int xambiguous             = 0;
  int muluvmain              = 0;
  int uvmainContributor      = -1;
  int uvpreviousCounter      = 0;
  int uvfinalMainContributor = -1;
  int uvambiguous            = 0;

  k     = rid;
  numberOfHits   = 0;
  numberOfXHits  = 0;
  numberOfUVHits = 0;
  if (verbose >= 9) cout << endl
			 << "Analysis of reconstructed track id: "
			 << k << endl << endl;
  for (plane = 0; plane < numberOfPlanes - 1; plane++)
    {
      int idhit = TrackList->get_hits(k, plane);
      if (verbose >= 10) cout << "plane: " << plane << " hit id: "
			      << idhit;
      if (idhit == -1)
        {
          if (verbose >= 10) cout << endl;
          continue;
        }
      int planeOfHit = HitList->getPlane(idhit);
      short type = wireType[planeOfHit];        // type of wire
      if (verbose >= 10 ) cout << " real plane: " << planeOfHit;
      numberOfHits++;
      if (type == X1Wire || type == X2Wire)
        {
          numberOfXHits++;
          if (verbose >= 10) cout << " which is X" << endl;
        }
      else
        {
          numberOfUVHits++;
          if (verbose >= 10) cout << " which is UV" << endl;
        }
      int idgeant = dDchGhitHits->get_ghitid(idhit); // geant hit id
      if (idgeant <= -1 || (unsigned int) idgeant >= dcghit->RowCount())
        cout << "id geant is = " << idgeant << endl;
      int idtrack = dcghit->get_mctrack(idgeant);    // geant track id
      originalTrackIdPerPlane[plane] = idtrack;
      wireTypePerPlane[plane] = type;
      if (verbose >= 10) cout << "ghit id: " << idgeant
			      << " geant track id: " << idtrack << endl;
      contribExists     = 0;
      xcontribExists    = 0;
      uvcontribExists   = 0;
      for (int temp = 0; temp < plane; temp++)  // found this id before?
        {
          if (idtrack == originalTrackIdPerPlane[temp])
            {
              contribExists = 1;
              if (wireTypePerPlane[temp] == X1Wire ||
                  wireTypePerPlane[temp] == X2Wire)
                xcontribExists = 1;
              else
                uvcontribExists = 1;
            }
        }
      if ( !contribExists )   mulmain++;
      if ( !xcontribExists &&
           (type == X1Wire || type == X2Wire) )  mulxmain++;
      if ( !uvcontribExists &&
           (type == UV1Wire || type == UV2Wire) ) muluvmain++;
    }
  if (verbose >= 9)
    {
      cout << "All hits : " << numberOfHits << endl;
      cout << "X   hits : " << numberOfXHits << endl;
      cout << "UV  hits : " << numberOfUVHits << endl;
      cout << "All contributors: " << mulmain << endl;
      cout << "X   contributors: " << mulxmain << endl;
      cout << "UV  contributors: " << muluvmain << endl;
    }

  // total main contributor
  for (plane = 0; plane < numberOfPlanes - 1; plane++)
    {
      int counter = 1;
      int idtrack = originalTrackIdPerPlane[plane];
      if (idtrack == -1) continue;
      for (int ii = plane + 1; ii < numberOfPlanes - 1; ii++)
        {
          int idtrack2 = originalTrackIdPerPlane[ii];
          if (idtrack2 == idtrack)
            {
              mainContributor = idtrack;
              counter++;
            }
        }
      if (counter == previousCounter) ambiguous = 1;
      if (counter > previousCounter)
        {
          ambiguous            = 0;
          previousCounter      = counter;
          finalMainContributor = mainContributor;
        }
    }
  if (verbose >= 9)
    {
      cout << "Main contributor: " << finalMainContributor << endl;
      cout << "Hits contributed: " << previousCounter << endl;
      cout << "Another equally contributing one: " << ambiguous << endl;
    }

  // x main contributor
  for (plane = 0; plane < numberOfPlanes - 1; plane++)
    {
      int xcounter = 1;
      int idtrack = originalTrackIdPerPlane[plane];
      if (idtrack == -1) continue;
      int idhit = TrackList->get_hits(k, plane);
      if (idhit == -1) continue;
      int planeOfHit = HitList->getPlane(idhit);
      short type = wireType[planeOfHit];
      if (type != X1Wire && type != X2Wire) continue;
      for (int ii = plane + 1; ii < numberOfPlanes - 1; ii++)
        {
          int idhit2 = TrackList->get_hits(k, ii);
          if (idhit2 == -1) continue;
          int planeOfHit2 = HitList->getPlane(idhit2);
          short type2 = wireType[planeOfHit2];
          if (type2 != X1Wire && type2 != X2Wire) continue;
          int idtrack2 = originalTrackIdPerPlane[ii];
          if (idtrack2 == idtrack)
            {
              xmainContributor = idtrack;
              xcounter++;
            }
        }
      if (xcounter == xpreviousCounter) xambiguous = 1;
      if (xcounter > xpreviousCounter)
        {
          xambiguous            = 0;
          xpreviousCounter      = xcounter;
          xfinalMainContributor = xmainContributor;
        }
    }
  if (verbose >= 9)
    {
      cout << "X Main contributor: " << xfinalMainContributor << endl;
      cout << "X Hits contributed: " << xpreviousCounter << endl;
      cout << "Another equally X contributing one: " << xambiguous << endl;
    }

  // uv main contributor
  for (plane = 0; plane < numberOfPlanes - 1; plane++)
    {
      int uvcounter = 1;
      int idtrack = originalTrackIdPerPlane[plane];
      if (idtrack == -1) continue;
      int idhit = TrackList->get_hits(k, plane);
      if (idhit == -1) continue;
      int planeOfHit = HitList->getPlane(idhit);
      short type = wireType[planeOfHit];
      if (type != UV1Wire && type != UV2Wire) continue;
      for (int ii = plane + 1; ii < numberOfPlanes - 1; ii++)
        {
          int idhit2 = TrackList->get_hits(k, ii);
          if (idhit2 == -1) continue;
          int planeOfHit2 = HitList->getPlane(idhit2);
          short type2 = wireType[planeOfHit2];
          if (type2 != UV1Wire && type2 != UV2Wire) continue;
          int idtrack2 = originalTrackIdPerPlane[ii];
          if (idtrack2 == idtrack)
            {
              uvmainContributor = idtrack;
              uvcounter++;
            }
        }
      if (uvcounter == uvpreviousCounter) uvambiguous = 1;
      if (uvcounter > uvpreviousCounter)
        {
          uvambiguous            = 0;
          uvpreviousCounter      = uvcounter;
          uvfinalMainContributor = uvmainContributor;
        }
    }
  if (verbose >= 9)
    {
      cout << "UV Main contributor: " << uvfinalMainContributor << endl;
      cout << "UV Hits contributed: " << uvpreviousCounter << endl;
      cout << "Another equally UV contributing one: " << uvambiguous << endl;
    }

  xhitmul  = numberOfXHits;
  uvhitmul = numberOfUVHits;
  mul      = mulmain;
  xmul     = mulxmain;
  uvmul    = muluvmain;
  amb      = ambiguous;
  ambx     = xambiguous;
  ambuv    = uvambiguous;
  gid      = finalMainContributor;
  gxid     = xfinalMainContributor;
  guvid    = uvfinalMainContributor;
  if ( numberOfHits > 0 )
    pure   = (float)previousCounter / (float)numberOfHits;
  else
    pure = 0.0;
  if ( numberOfXHits > 0 )
    purex  = (float)xpreviousCounter / (float)numberOfXHits;
  else
    purex = 0.0;
  if ( numberOfUVHits > 0 )
    pureuv = (float)uvpreviousCounter / (float)numberOfUVHits;
  else
    pureuv = 0.0;

  if (verbose >= 9 )
    {
      cout << "Final values returned:" << endl;
      cout << "mul    : " << mul << endl;
      cout << "xmul   : " << xmul << endl;
      cout << "uvmul  : " << uvmul << endl;
      cout << "amb    : " << amb << endl;
      cout << "ambx   : " << ambx << endl;
      cout << "ambuv  : " << ambuv << endl;
      cout << "gid    : " << gid << endl;
      cout << "gxid   : " << gxid << endl;
      cout << "guvid  : " << guvid << endl;
      cout << "pure   : " << pure << endl;
      cout << "purex  : " << purex << endl;
      cout << "pureuv : " << pureuv << endl;
      cout << "------------------------------" << endl;
      cout << "Hit <Enter> to continue";
      cin >> cont;
      cout << endl;
      cout << endl;
    }
  return True;
}

PHBoolean mCentralTrackEvaluator_v1::associatePC(PHCompositeNode* topNode, const int pc)
{
  if (pc < 1 || pc > 3)
    {
      cout << PHWHERE << " invalid pc: " << pc << endl;
      return False;
    }

  CglTrack* CglTrackList = findNode::getClass<CglTrack>(topNode,"CglTrack");
  ostringstream nodename;
  nodename << "pc" << pc << "ghit"; 
  pcghitWrapper* pcghit = findNode::getClass<pcghitWrapper>(topNode,nodename.str().c_str());
  nodename.str("");
  nodename << "dPc" << pc << "GhitClus";
  dPadGhitClusWrapper* pcghitClus = findNode::getClass<dPadGhitClusWrapper>(topNode,nodename.str().c_str());


  if (!CglTrackList) return False;
  if (!pcghit) return False;
  if (!pcghitClus) return False;

  int totalMcTracks = EvalList->get_McEvalSingleTrackN();
  int totalRecoTracksforMcTrack = 0;
  int idRecoTrack = -1;
  int idGeantTrack = -1;
  int pc1RecoId = -1;

  for (int i = 0; i < totalMcTracks; i++)   // Mc track loop
    {
      idGeantTrack = EvalList->get_mctrackid(i);
      totalRecoTracksforMcTrack = EvalList->get_Nreco(i);
      for (int j = 0; j < totalRecoTracksforMcTrack; j++)   // Reco track loop
        {
          idRecoTrack = EvalList->get_recoid(i, j);
          if ( pc == 1 )
            {
              pc1RecoId = CglTrackList->get_pc1clusid(idRecoTrack);
              if ( pc1RecoId >= 0 ) EvalList->set_pc1clusid(i, j, pc1RecoId);
            }
          else if ( pc == 2 )
            {
              pc1RecoId = CglTrackList->get_pc2clusid(idRecoTrack);
              if ( pc1RecoId >= 0 ) EvalList->set_pc2clusid(i, j, pc1RecoId);
            }
          else if ( pc == 3 )
            {
              pc1RecoId = CglTrackList->get_pc3clusid(idRecoTrack);
              if ( pc1RecoId >= 0 ) EvalList->set_pc3clusid(i, j, pc1RecoId);
            }
          for (int k = 0; k < int(pcghitClus->RowCount()); k++)
            {
              int tmpRecoId     = pcghitClus->get_clusid(k);
              int tmpMcId       = pcghitClus->get_ghitid(k);
              int tmpGeantTrack = pcghit->get_mctrack(tmpMcId);
              if ( tmpGeantTrack == idGeantTrack)
                {
                  if ( pc == 1 ) EvalList->set_pc1clusidtrue(i, j, tmpRecoId); //
                  if ( pc == 2 ) EvalList->set_pc2clusidtrue(i, j, tmpRecoId); //
                  if ( pc == 3 ) EvalList->set_pc3clusidtrue(i, j, tmpRecoId); //
                }
              if ( tmpRecoId == pc1RecoId )
                {
                  if ( pc == 1 ) EvalList->set_pc1clusidg(i, j, tmpGeantTrack); //
                  if ( pc == 2 ) EvalList->set_pc2clusidg(i, j, tmpGeantTrack); //
                  if ( pc == 3 ) EvalList->set_pc3clusidg(i, j, tmpGeantTrack); //
                }
            }
          for (int k = 0; k < int(pcghit->RowCount()); k++)
            {
              int tmpGeantTrack = pcghit->get_mctrack(k);
              if (tmpGeantTrack == idGeantTrack)
                {
                  if ( pc == 1 )
                    {
                      EvalList->set_pc1pointxg(i, j, pcghit->get_xyzinglo(0, k));
                      EvalList->set_pc1pointyg(i, j, pcghit->get_xyzinglo(1, k));
                      EvalList->set_pc1pointzg(i, j, pcghit->get_xyzinglo(2, k));
                    }
                  if ( pc == 2 )
                    {
                      EvalList->set_pc2pointxg(i, j, pcghit->get_xyzinglo(0, k));
                      EvalList->set_pc2pointyg(i, j, pcghit->get_xyzinglo(1, k));
                      EvalList->set_pc2pointzg(i, j, pcghit->get_xyzinglo(2, k));
                    }
                  if ( pc == 3 )
                    {
                      EvalList->set_pc3pointxg(i, j, pcghit->get_xyzinglo(0, k));
                      EvalList->set_pc3pointyg(i, j, pcghit->get_xyzinglo(1, k));
                      EvalList->set_pc3pointzg(i, j, pcghit->get_xyzinglo(2, k));
                    }
                }
            }
        }
    }
  return True;
}

PHBoolean mCentralTrackEvaluator_v1::associateTOF(PHCompositeNode* topNode)
{

  CglTrack*                  CglTrackList = findNode::getClass<CglTrack>(topNode,"CglTrack");
  dTofGdigiWrapper*          tofGdigi = findNode::getClass<dTofGdigiWrapper>(topNode,"dTofGdigi");
  dTofGdigiRecWrapper*       tofGdigiRec = findNode::getClass<dTofGdigiRecWrapper>(topNode,"dTofGdigiRec");

  if (!CglTrackList) return False;
  if (!tofGdigi) return False;
  if (!tofGdigiRec) return False;

  int totalMcTracks = EvalList->get_McEvalSingleTrackN();
  int totalRecoTracksforMcTrack = 0;
  int idRecoTrack;
  int idGeantTrack;
  int tofRecoId;

  for (int i = 0; i < totalMcTracks; i++)   // Mc track loop
    {
      idGeantTrack = EvalList->get_mctrackid(i);
      totalRecoTracksforMcTrack = EvalList->get_Nreco(i);
      for (int j = 0; j < totalRecoTracksforMcTrack; j++)   // Reco track loop
        {
          idRecoTrack = EvalList->get_recoid(i, j);
          tofRecoId   = CglTrackList->get_tofrecid(idRecoTrack);
          if ( tofRecoId >= 0 )
            {
              EvalList->set_tofid(i, j, tofRecoId);
            }
          for (int k = 0; k < int(tofGdigiRec->RowCount()); k++)
            {
              int tmpRecoId     = tofGdigiRec->get_recid(k);
              int tmpMcId       = tofGdigiRec->get_gdigiid(k);
              int tmpGeantTrack = tofGdigi->get_mctrack(tmpMcId);
              if ( tmpGeantTrack == idGeantTrack )
                {
                  EvalList->set_tofidtrue(i, j, tmpRecoId);
                }
              if ( tmpRecoId == tofRecoId )
                {
                  EvalList->set_tofidg(i, j, tmpGeantTrack);
                }
            }
          for (int k = 0; k < int(tofGdigi->RowCount()); k++)
            {
              int tmpGeantTrack = tofGdigi->get_mctrack(k);
              if ( tmpGeantTrack == idGeantTrack )
                {
                  EvalList->set_tofpointxg(i, j, tofGdigi->get_pos_m(0, k));
                  EvalList->set_tofpointyg(i, j, tofGdigi->get_pos_m(1, k));
                  EvalList->set_tofpointzg(i, j, tofGdigi->get_pos_m(2, k));
                  EvalList->set_tofg(i, j, tofGdigi->get_tof(k));
                  EvalList->set_tofelossg(i, j, tofGdigi->get_eloss(k));
                }
            }
        }
    }
  return True;
}

PHBoolean mCentralTrackEvaluator_v1::associateEMC(PHCompositeNode* topNode)
{

  CglTrack*                   CglTrackList = findNode::getClass<CglTrack>(topNode,"CglTrack");
  emcClusterContainer*        emcCluster = findNode::getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  dEmcGeaClusterTrackWrapper* emcGeaClusterTrack = findNode::getClass<dEmcGeaClusterTrackWrapper>(topNode,"dEmcGeaClusterTrack");


  if (!CglTrackList) return False;
  if (!emcCluster) return False;
  if (!emcGeaClusterTrack) return False;

  int totalMcTracks = EvalList->get_McEvalSingleTrackN();
  int totalRecoTracksforMcTrack = 0;
  int idRecoTrack;
  int idGeantTrack;
  int emcRecoId;

  for (int i = 0; i < totalMcTracks; i++)   // Mc track loop
    {
      idGeantTrack = EvalList->get_mctrackid(i);
      totalRecoTracksforMcTrack = EvalList->get_Nreco(i);
      for (int j = 0; j < totalRecoTracksforMcTrack; j++)   // Reco track loop
        {
          idRecoTrack = EvalList->get_recoid(i, j);
          emcRecoId   = CglTrackList->get_emcclusid(idRecoTrack);
          if ( emcRecoId >= 0 )
            {
              EvalList->set_emcclusid(i, j, emcRecoId);
            }
          for (int k = 0; k < int(emcGeaClusterTrack->RowCount()); k++)
            {
              if ( emcGeaClusterTrack->get_clusid(k) != emcRecoId ) continue;
              if ( emcGeaClusterTrack->get_efrac(0, k) >= 0.5 )
                EvalList->set_emcclusidg(i, j, emcGeaClusterTrack->get_trkno(0, k));
              if ( emcGeaClusterTrack->get_efrac(1, k) >= 0.5 )
                EvalList->set_emcclusidg(i, j, emcGeaClusterTrack->get_trkno(1, k));
              if ( emcGeaClusterTrack->get_efrac(2, k) >= 0.5 )
                EvalList->set_emcclusidg(i, j, emcGeaClusterTrack->get_trkno(2, k));
              EvalList->set_emcanctrk0(i, j, emcGeaClusterTrack->get_trkno(0, k));
              EvalList->set_emcanctrk1(i, j, emcGeaClusterTrack->get_trkno(1, k));
              EvalList->set_emcanctrk2(i, j, emcGeaClusterTrack->get_trkno(2, k));
              EvalList->set_emcanctwrhit0(i, j, emcGeaClusterTrack->get_tracktwrhit(0, k));
              EvalList->set_emcanctwrhit1(i, j, emcGeaClusterTrack->get_tracktwrhit(1, k));
              EvalList->set_emcanctwrhit2(i, j, emcGeaClusterTrack->get_tracktwrhit(2, k));
              EvalList->set_emcancpid0(i, j, lroundf(emcGeaClusterTrack->get_pid(0, k)));
              EvalList->set_emcancpid1(i, j, lroundf(emcGeaClusterTrack->get_pid(1, k)));
              EvalList->set_emcancpid2(i, j, lroundf(emcGeaClusterTrack->get_pid(2, k)));
              EvalList->set_emcancedep0(i, j, emcGeaClusterTrack->get_edep(0, k));
              EvalList->set_emcancedep1(i, j, emcGeaClusterTrack->get_edep(1, k));
              EvalList->set_emcancedep2(i, j, emcGeaClusterTrack->get_edep(2, k));
              EvalList->set_emcancptot0(i, j, emcGeaClusterTrack->get_ptot(0, k));
              EvalList->set_emcancptot1(i, j, emcGeaClusterTrack->get_ptot(1, k));
              EvalList->set_emcancptot2(i, j, emcGeaClusterTrack->get_ptot(2, k));
            }
          for ( int k = 0; k < int(emcGeaClusterTrack->RowCount()); k++)
            {
              int index  = -1;
              int index0 = emcGeaClusterTrack->get_trkno(0, k);
              int index1 = emcGeaClusterTrack->get_trkno(1, k);
              int index2 = emcGeaClusterTrack->get_trkno(2, k);
              if ( idGeantTrack == index0 ||
                   idGeantTrack == index1 ||
                   idGeantTrack == index2 )
                {
                  if ( idGeantTrack == index0 ) index = 0;
                  if ( idGeantTrack == index1 ) index = 1;
                  if ( idGeantTrack == index2 ) index = 2;
                  if ( emcGeaClusterTrack->get_efrac(index, k) < 0.5 ) continue;
                  EvalList->set_emcclusidtrue(i, j, emcGeaClusterTrack->get_clusid(k));

                  emcClusterContent* clustercontent = emcCluster->findCluster(emcGeaClusterTrack->get_clusid(k));
                  EvalList->set_emcpointxg(i, j, clustercontent->x());
                  EvalList->set_emcpointyg(i, j, clustercontent->y());
                  EvalList->set_emcpointzg(i, j, clustercontent->z());
                  EvalList->set_emcefracg(i, j, emcGeaClusterTrack->get_efrac(index, k));
                  EvalList->set_emcecoreg(i, j, emcGeaClusterTrack->get_ecore(k));
                  EvalList->set_emcmeaseg(i, j, emcGeaClusterTrack->get_mease(k));
                  EvalList->set_emctofg(i, j, emcGeaClusterTrack->get_tof(k));
                }
            }
        }
    }
  return True;
}

PHBoolean mCentralTrackEvaluator_v1::associateCRK(PHCompositeNode* topNode)
{

  PHTrackOut*                 phtrack = findNode::getClass<PHTrackOut>(topNode,"PHTrackOut");
  CglTrack*                   CglTrackList = findNode::getClass<CglTrack>(topNode,"CglTrack");
  dPadClusterWrapper*         pcCluster = findNode::getClass<dPadClusterWrapper>(topNode,"dPc1Cluster");
  emcClusterContainer*        emcCluster = findNode::getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  DchTrack*                   dchtrack = findNode::getClass<DchTrack>(topNode,"DchTrack");

  if (!phtrack) return False;
  if (!dchtrack) return False;
  if (!CglTrackList) return False;
  if (!pcCluster) return False;
  if (!emcCluster) return False;

  int totalMcTracks = EvalList->get_McEvalSingleTrackN();
  int totalRecoTracksforMcTrack = 0;
  int idRecoTrack;
  int pc1RecoId, pc3RecoId, emcRecoId;

  for (int i = 0; i < totalMcTracks; i++)   // Mc track loop
    {
      totalRecoTracksforMcTrack = EvalList->get_Nreco(i);
      for (int j = 0; j < totalRecoTracksforMcTrack; j++)   // Reco track loop
        {
          idRecoTrack = EvalList->get_recoid(i, j);
          if (idRecoTrack < 0) continue;
          pc1RecoId = dchtrack->get_hits(idRecoTrack, 39);
          pc3RecoId = CglTrackList->get_pc3clusid(idRecoTrack);
          emcRecoId = CglTrackList->get_emcclusid(idRecoTrack);
          if ( pc1RecoId < 0 ||
               !phtrack->get_ifIntersectPc2(idRecoTrack) ) continue;
          PHPoint pstart(pcCluster->get_xyz(0, pc1RecoId),
                         pcCluster->get_xyz(1, pc1RecoId),
                         pcCluster->get_xyz(2, pc1RecoId));
          PHPoint pend(phtrack->get_projectionPc2(idRecoTrack, 0),
                       phtrack->get_projectionPc2(idRecoTrack, 1),
                       phtrack->get_projectionPc2(idRecoTrack, 2));
          if ( pcCluster->get_xyz(0, pc1RecoId) > 0.0 )
            {
              if ( emcRecoId >= 0 )
                {
                  emcClusterContent* clustercontent = emcCluster->findCluster(emcRecoId);
                  if ( clustercontent->ecore() > 0.1 )
                    {
                      pend = PHPoint(clustercontent->x(), clustercontent->y(), clustercontent->z());
                    }
                }
            }
          else
            {
              if ( pc3RecoId >= 0 )
                {
                  pend = PHPoint(pcCluster->get_xyz(0, pc3RecoId),
                                 pcCluster->get_xyz(1, pc3RecoId),
                                 pcCluster->get_xyz(2, pc3RecoId));
                }
            }
          PHLine rich_proj(pstart, pend);
          CrkPIDout rich;
          d_crkpid->SetCrkHitFromTop(topNode);
          d_crkpid->AssociateTrack(rich_proj, &rich);
          if ( rich.accepted )
            EvalList->set_crkacc(i, j, 1);
          else
            EvalList->set_crkacc(i, j, 0);
          EvalList->set_crknpmt0(i, j, rich.npmt0);
          EvalList->set_crknpmt1(i, j, rich.npmt1);
          EvalList->set_crknpmt3(i, j, rich.npmt3);
          EvalList->set_crknpe0(i, j, rich.npe0);
          EvalList->set_crknpe1(i, j, rich.npe1);
          EvalList->set_crknpe3(i, j, rich.npe3);
          EvalList->set_crkchi2(i, j, rich.chi2);
          EvalList->set_crkdisp(i, j, rich.disp);
          EvalList->set_crkpath(i, j, rich.path);
        }
    }
  return True;
}

//to have a compact evaluation add following lines in above function

/*  SimpleHistogrammer*histo = SimpleHistogrammer::instance();
histo->initializeFile();
histo->setup(root);
static int cnt =0;
if(cnt%20==0){
  histo->flush();
}
cnt++;
*/
