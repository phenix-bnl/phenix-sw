#include <iostream>

#include "phool.h"
#include "MCHepMCParticle.h"

ClassImp(MCHepMCParticle)

using namespace std;

void MCHepMCParticle::identify(ostream &os) const
{
  os << "identify: virtual MCHepMCParticle object"<<endl;
  return;
}

void MCHepMCParticle::Reset()
{
  mc_n_part = 0;
  mc_barcode = -999;
  mc_vertex_begin = 9999;
  mc_vertex_end = 9999;
  mc_px = -999.0;
  mc_py = -999.0;
  mc_pz = -999.0;
  mc_ptot = -999.0;
  mc_x_begin = -999.0;
  mc_y_begin = -999.0;
  mc_z_begin = -999.0;
  mc_x_end = -999.0;
  mc_y_end = -999.0;
  mc_z_end = -999.0;
  mc_pid = -999;
  mc_status = -999;


}
