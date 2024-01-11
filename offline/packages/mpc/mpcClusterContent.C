#include "mpcClusterContent.h"

ClassImp(mpcClusterContent)

  using namespace std;

static int shutup = 0;

//_____________________________________________________________________________
mpcClusterContent&
mpcClusterContent::operator=(const mpcClusterContent&)
{
  return * this;
}

//_____________________________________________________________________________
void
mpcClusterContent::identify(std::ostream& os) const
{
  warning("identify(std::ostream& os)");
}

//_____________________________________________________________________________
int
mpcClusterContent::isValid() const
{
  warning("isValid()");
  return 0;
}

//_____________________________________________________________________________
void
mpcClusterContent::print(std::ostream& out) const
{
  warning("print(std::ostream& out)");
}

void
mpcClusterContent::warning(const char *field) const
{
  if (!shutup)
    {
      cout << PHWHERE << "using virtual function, doing nothing" << endl;
      cout << "Offending field == " << field << endl;
    }
  return ;
}

void
mpcClusterContent::ShutUp(const int i)
{
  shutup = i;
}

void
mpcClusterContent::Copy(const mpcClusterContent &src)
{
  ShutUp();
  set_arm(src.arm());
  set_chi2(src.chi2());
  set_corrdisp(src.corrdispy(), src.corrdispx());
  set_cutword(src.cutword());
  set_disp(src.dispy(), src.dispx());
  set_dxyz(src.dx(), src.dy(), src.dz());
  set_e(src.e());
  set_e9(src.e9());
  set_ecore(src.ecore());
  set_ecent(src.ecent());
  set_etofmin(src.etofmin());
  set_etofmax(src.etofmax());
  set_id(src.id());
  set_ipos(src.ixpos(), src.iypos());
  set_quality(src.quality());
  set_maps(src.deadmap(), src.warnmap());
  set_multiplicity(src.multiplicity());
  set_padisp(src. padispy(), src.padispx());
  set_prob_photon(src.prob_photon());
  set_phi(src.phi());
  set_pid(src.pid());
  set_rawtdc(src.rawtdc());
//  set_sector(src.sector());
  set_simfrac(src.simfrac());
  set_tof(src.tof());
  set_tofhad(src.tofhad());
  set_tofdisp(src.tofdisp());
  set_tofmin(src.tofmin());
  set_tofmax(src.tofmax());
  set_tofcorr(src.tofcorr());
  set_tofcorrmin(src.tofcorrmin());
  set_tofcorrmax(src.tofcorrmax());
  set_theta(src.theta());
//  set_type(src.type());
  set_xyz(src.x(), src.y(), src.z());
  set_yz_cg(src.ycg(), src.zcg());

  //  Extra variables for vetoes of charged particles...
/*
  set_emcpc3(src.emcpc3());
  set_emcpc3neartrk(src.emcpc3neartrk());
  set_emcpc3dz(src.emcpc3dz());
  set_emcpc3dphi(src.emcpc3dphi());
  set_emctrk(src.emctrk());
  set_emctrkdz(src.emctrkdz());
  set_emctrkdphi(src.emctrkdphi());
  set_pemctrk(src.pemctrk());
  set_emctrkquality(src.emctrkquality());
*/
  for (int i = 0; i < src.multiplicity(); i++)
    {
      set_towerid(i, src.towerid(i));
      set_partesum(i, src.partesum(i));
    }
  ShutUp(0);
}

int
mpcClusterContent::isValid(const int i) const
{
  if (i == -1)
    {
      return 0;
    }
  return 1;
}

int
mpcClusterContent::isValid(const unsigned int i) const
{
  if (i == 0)
    {
      return 0;
    }
  return 1;
}

int
mpcClusterContent::isValid(const float f) const
{
  if (isnan(f))
    {
      return 0;
    }
  return 1;
}
