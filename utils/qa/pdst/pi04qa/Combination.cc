#include <cstdlib>
#include <string>
#include <fstream>

#include "Combination.h"

//
// Calculate combination of two tracks and fill the structure
//
bool Combination::calcCombination( KCluster *c1, KCluster *c2 )
{

  goodFlag = 0;

  float prob_a, prob_b;     // nEmcClusterLocalExt's probability.
  float chi2_a, chi2_b;     //
  float me_a, me_b;       // Measured Energy of each

  float tofcorr_a, tofcorr_b;
  float lx_a, lx_b, ly_a, ly_b;

  arm_a = c1->getArm();
  arm_b = c2->getArm();

  if ( arm_a != arm_b )
    return kFALSE;

  sec_a = c1->getSec();
  sec_b = c2->getSec();

  if ( ( sec_a == 6 || sec_a == 7 ) && ( sec_b < 6 ) )
    return kFALSE;
  if ( ( sec_b == 6 || sec_b == 7 ) && ( sec_a < 6 ) )
    return kFALSE;

  iy_a = c1->getIy();
  iy_b = c2->getIy();

  iz_a = c1->getIz();
  iz_b = c2->getIz();

  if (sec_a < 6)
    {
      sm_a_iy = int(iy_a / 12);
      sm_a_iz = int(iz_a / 12);
    }
  else
    {
      sm_a_iy = int(iy_a / 16);
      sm_a_iz = int(iz_a / 16);
    }

  if (sec_b < 6)
    {
      sm_b_iy = int(iy_b / 12);
      sm_b_iz = int(iz_b / 12);
    }
  else
    {
      sm_b_iy = int(iy_b / 16);
      sm_b_iz = int(iz_b / 16);
    }

  e1 = c1->ecore();
  e2 = c2->ecore();

  // ENERGY CUT
  e = e1 + e2;
  if ( (e < 0.0) || (e > 40.0) )
    return kFALSE;

  // ASYM CUT
  asym = fabs( e1 - e2 ) / e;
  if ( asym < 0.0 || asym > 0.8)
    return kFALSE;

  norm_tr1[0] = c1->getTrackVector(0);
  norm_tr1[1] = c1->getTrackVector(1);
  norm_tr1[2] = c1->getTrackVector(2);

  norm_tr2[0] = c2->getTrackVector(0);
  norm_tr2[1] = c2->getTrackVector(1);
  norm_tr2[2] = c2->getTrackVector(2);

  // Reconstruct invariant mass.

  // MOMETUM
  float px = e1 * (norm_tr1[0]) + e2 * (norm_tr2[0]);
  float py = e1 * (norm_tr1[1]) + e2 * (norm_tr2[1]);
  float pz = e1 * (norm_tr1[2]) + e2 * (norm_tr2[2]);

  pxyz[0] = px;
  pxyz[1] = py;
  pxyz[2] = pz;

  double trkLength = sqrt( px * px + py * py + pz * pz);

  if ( trkLength == 0)
    {
      TrackVector[0] = 0;
      TrackVector[1] = 0;
      TrackVector[2] = 0;
    }
  else
    {
      TrackVector[0] = px / trkLength;
      TrackVector[1] = py / trkLength;
      TrackVector[2] = pz / trkLength;

    }

  // TRANSVERSE MOMENTUM and CUT
  pt = sqrt( (px * px) + (py * py) );
  if ( pt < 1.0 || pt > 200.0 )
    return kFALSE;

  // OPENING ANGLE AND CUT
  cosine =
    norm_tr1[0] * norm_tr2[0] +
    norm_tr1[1] * norm_tr2[1] +
    norm_tr1[2] * norm_tr2[2];

  if ( cosine < -1.0 || cosine > 1.0 )
    return kFALSE;



  // INVARIANT MASS
  //
  inv_mass = sqrt( 2 * e1 * e2 * ( 1.0 - cosine ) );
  if ( inv_mass < 0.0 ||
       inv_mass > 1.0 )
    return kFALSE;

  prob_a = c1->prob_photon();
  prob_b = c2->prob_photon();

  chi2_a = c1->chi2();
  chi2_b = c2->chi2();

  tofcorr_a = c1->tofcorr();
  tofcorr_b = c2->tofcorr();

  CRTOFCut = 0;
  if (fabs(tofcorr_a - tofcorr_b) < 1.2)
    {
      CRTOFCut = 1;
    }


  CTOFCut = 0;
  if (c1->getTOFCut() >= 1 && c2->getTOFCut() >= 1)
    {
      CTOFCut = 1;
    }
  if (c1->getTOFCut() >= 2 && c2->getTOFCut() >= 2)
    {
      CTOFCut = 2;
    }

  CChi2Cut = 0;
  if (c1->getChi2Cut() >= 1 && c2->getChi2Cut() >= 1)
    {
      CChi2Cut = 1;
    }

  me_a = c1->e();
  me_b = c2->e();
  ec_a = e1;
  ec_b = e2;

  lx_a = c1->getLx();
  lx_b = c2->getLx();
  ly_a = c1->getLy();
  ly_b = c2->getLy();

  goodFlag = 1;

  return kTRUE;
}


double Combination::getTrackVector(const int i) const
{
  if ( i < 0 || i > 2)
    return 0;

  return TrackVector[i];
}

void Combination::fillHist( THmulf *gghs, int mixed, int cent )
{

  int det = 0;
  float avenergy = (ec_a + ec_b) / 2. ;

  // already checked for pairs between
  // PbGl and PbSc in calcCombinations
  if (sec_a > 5)
    det = 1;

  if ( cosine < (1. - (0.0425*0.0425) / (2.*ec_a*ec_b)))
    {

      if ( sec_a == sec_b )
        {

          gghs->Fill(1.0, inv_mass, pt, (float)mixed, (float)cent, sec_a, 0., (float)CChi2Cut, (float)CTOFCut );

          if (asym < 0.2)
            {
              gghs->Fill(1.0, inv_mass, avenergy, (float)mixed, (float)cent, sec_a, 1., (float)CChi2Cut, (float)CTOFCut );
            }

        }
    }
}

// EOF




