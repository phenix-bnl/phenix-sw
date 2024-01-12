// ============================
// FILE: SvxCentralTrackRecal.C
// ============================

#include "SvxCentralTrackRecal.h"

#include <algorithm>

ClassImp(SvxCentralTrackRecal)

// Constructors
// """"""""""""

SvxCentralTrackRecal::SvxCentralTrackRecal()
    : track_id(-1),
      cnt_id(-1),
      hit_pattern(-1),
      chi2(-1.),
      chi2_old(-1.),
      chi2_2(-1.),
      chi2_phi(-1.),
      chi2_z(-1.),
      dca2d(-9999.),
      dcaz(-9999.),
      dca2d_old(-9999.),
      dcaz_old(-9999.),
      phi0(-9999.),
      the0(-9999.),
      momentum(-9999.),
      charge(0) {
  std::fill_n(nhit, 4, 0);
  std::fill_n(closest_approach, 3, -9999);
  std::fill_n(cluster_id[0], 4 * 2, -1);
  std::fill_n(cluster_dphi[0], 4 * 2, -9999);
  std::fill_n(cluster_dz[0], -9999, -1);
  std::fill_n(cluster_ladder[0], 4 * 2, -1);
  std::fill_n(cluster_sensor[0], 4 * 2, -1);
  std::fill_n(cluster_position[0][0], 4 * 2 * 3, -1);
  std::fill_n(projected_position[0], 4 * 3, -9999);
  std::fill_n(multi_dphi, 4, -9999);
  std::fill_n(multi_dz, 4, -9999);
  std::fill_n(expected_position[0], 4 * 3, -9999);
}

SvxCentralTrackRecal::SvxCentralTrackRecal(const SvxCentralTrackRecal& t)
    : track_id(t.track_id),
      cnt_id(t.cnt_id),
      hit_pattern(t.hit_pattern),
      unique(t.hit_pattern),
      chi2(t.chi2),
      chi2_old(t.chi2_old),
      chi2_2(t.chi2_2),
      chi2_phi(t.chi2_phi),
      chi2_z(t.chi2_z),
      dcaz(t.dcaz),
      dca2d_old(t.dca2d_old),
      dcaz_old(t.dcaz_old),
      phi0(t.phi0),
      the0(t.the0),
      momentum(t.momentum),
      charge(t.charge) {
  std::copy(&t.nhit[0]                 , &t.nhit[0]                  + 4  , &nhit[0]);
  std::copy(&t.closest_approach[0]     , &t.closest_approach[0]      + 3  , &closest_approach[0]);
  std::copy(&t.cluster_id[0][0]        , &t.cluster_id[0][0]         + 4*2, &cluster_id[0][0]);
  std::copy(&t.cluster_dphi[0][0]      , &t.cluster_dphi[0][0]       + 4*2, &cluster_dphi[0][0]);
  std::copy(&t.cluster_dz[0][0]        , &t.cluster_dz[0][0]         + 4*2, &cluster_dz[0][0]);
  std::copy(&t.cluster_ladder[0][0]    , &t.cluster_ladder[0][0]     + 4*2, &cluster_ladder[0][0]);
  std::copy(&t.cluster_sensor[0][0]    , &t.cluster_sensor[0][0]     + 4*2, &cluster_sensor[0][0]);
  std::copy(&t.cluster_position[0][0]  , &t.cluster_position[0][0]   + 4*2, &cluster_position[0][0]);
  std::copy(&t.projected_position[0][0], &t.projected_position[0][0] + 4*3, &projected_position[0][0]);
  std::copy(&t.multi_dphi[0]           , &t.multi_dphi[0]            + 4  , &multi_dphi[0]);
  std::copy(&t.multi_dz[0]             , &t.multi_dz[0]              + 4  , &multi_dz[0]);
  std::copy(&t.expected_position[0][0] , &t.expected_position[0][0]  + 4*3, &expected_position[0][0]);
}

// The "standard PHObject response" functions...
// """""""""""""""""""""""""""""""""""""""""""""
void SvxCentralTrackRecal::Reset()
{
  track_id = -1;
  cnt_id = -1;
  hit_pattern = 0;
  unique = -1;
  for ( int ilayer=0; ilayer<4; ilayer++ ) {
    nhit[ilayer] = 0;
  }
  for ( int ixyz=0; ixyz<3; ixyz++ ) {
    closest_approach[ixyz] = -9999.;
  }
  chi2 = -1.;
  chi2_old = -1.;
  chi2_2 = -1.;
  chi2_phi = -1.;
  chi2_z = -1.;
  dca2d = -9999.;
  dcaz = -9999.;
  dca2d_old = -9999.;
  dcaz_old = -9999.;
  phi0 = -9999.;
  the0 = -9999.;
  momentum = -9999.;
  charge = 0;
  for ( int ilayer=0; ilayer<4; ilayer++ ) {
    for ( int ihit=0; ihit<2; ihit++ ) {
      cluster_id[ilayer][ihit]   = -1;
      cluster_dphi[ilayer][ihit] = -9999.;
      cluster_dz[ilayer][ihit]   = -9999.;
      cluster_ladder[ilayer][ihit] = -1;
      cluster_sensor[ilayer][ihit] = -1;
      for ( int ixyz=0; ixyz<3; ixyz++ ) {
	cluster_position[ilayer][ihit][ixyz] = -9999.;
      }
    }
  }
  for ( int ilayer=0; ilayer<4; ilayer++ ) {
    for ( int ixyz=0; ixyz<3; ixyz++ ) {
      projected_position[ilayer][ixyz] = -9999.;
    }
  }
}

void SvxCentralTrackRecal::identify(std::ostream& os) const
{
  os << "Identify yourself: SvxCentralTrackRecal object: ID = "
     << track_id << std::endl;
}

// Methods
// """""""
void SvxCentralTrackRecal::print() const
{
  std::cout << "SvxCentralTrackRecal" << std::endl;
  std::cout << "  chi-square  = " << chi2  << std::endl;
  std::cout << "  2D-DCA      = " << dca2d << std::endl;
  std::cout << "  Z-DCA       = " << dcaz  << std::endl;
  std::cout << "  chi-square (old) = " << chi2_old  << std::endl;
  std::cout << "  2D-DCA (old)     = " << dca2d_old << std::endl;
  std::cout << "  Z-DCA (old)      = " << dcaz_old  << std::endl;
  std::cout << "  closest approach :";
  for ( int i=0; i<3; i++ ) {
    std::cout << " " << closest_approach[i];
  }
  std::cout << std:: endl;
  std::cout << "  cluster position :" << std::endl;
  for ( int ilyr=0; ilyr<4; ilyr++ ) {
    for ( int ihit=0; ihit<2; ihit++ ) {
      std::cout << "layer=" << ilyr << ", hit=" << ihit << ":";
      for ( int i=0; i<3; i++ ) {
	std::cout << " " << cluster_position[ilyr][ihit][i];
      }
      std::cout << std::endl;
    }
  }
  std::cout << "  projected position :" << std::endl;
  for ( int ilyr=0; ilyr<4; ilyr++ ) {
    std::cout << "layer=" << ilyr << ":";
    for ( int i=0; i<3; i++ ) {
      std::cout << " " << projected_position[ilyr][i];
    }
    std::cout << std::endl;
  }
  std::cout << "  phi0 = " << phi0 << std::endl;
  std::cout << "  the0 = " << the0 << std::endl;
  std::cout << "  momentum  = " << momentum << std::endl;
  std::cout << "  charge  = " << charge << std::endl;
}
