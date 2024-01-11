#include "PHPoint.h"

#include <vector>

class CrkHit;
class CrkPID;
class dCrkHitWrapper;
class EmcClusterLocalExt;
class PadCluster;
class PHCentralTrack;

//  Hello pad chamber tracksing fans.  This header file and its associated 
//  .C file are the codes to find pad-chamber only tracks and append them to 
//  the list of all particles found by the tracking.
//
//  The code was written by Y. Akiba and contained inside the CentNanoDSTfuncs.C
//  file.  However since that file is already significantly bloated, I have
//  removed the relevant codes into this separate file.
//
//                                       TKH 11-10-2002
//

struct pctrk {
  // stryctyre of a track constructed from
  // EMC and PC1
  pctrk(float Dphi, float Dz, float Pc1phi, float Alpha,
	PHPoint &Pc1xyz, PHPoint &Pc2xyz, PHPoint &Emcxyz,
	EmcClusterLocalExt *Emc, int Iemc)
    :dphi(Dphi),dz(Dz),pc1phi(Pc1phi),alpha(Alpha),
     pc1xyz(Pc1xyz),pc2xyz(Pc2xyz),emcxyz(Emcxyz),
     emc(Emc),iemc(Iemc),ipart(-1),n0(0),npe0(0),chi2(0),ghost(false) {}
  float dphi;   //dphi at pc2
  float dz;     // dz at pc2
  float pc1phi; // phi at pc1
  float alpha;  // alpha at pc1
  PHPoint pc1xyz;
  PHPoint pc2xyz;
  PHPoint emcxyz;
  EmcClusterLocalExt *emc;
  int   iemc;
  int   ipart;//index of DC track based particle if found.
  // the following 4 variables are meaningful
  // only when ipart >= 0
  // they are difference in (phi,z) between
  // pctrk and dctrk(projection)
  float pc1dphi;
  float pc1dz;
  float emcdphi;
  float emcdz;
  int   n0;
  float npe0;
  float chi2;
  bool ghost;
};

void find_pctrk(PadCluster *pc1, PadCluster *pc2, EmcClusterLocalExt *emc, std::vector<pctrk> &vpctrk);
void remove_duplicate(std::vector<pctrk> &vpctrk_in, PHCentralTrack *particle, std::vector<pctrk> &vpctrk_out);
void initialize_crkpid(int run);
void associate_to_RICH(std::vector<pctrk>&vpctrk, CrkHit *d_crk);
void fill_particle(PHCentralTrack *particle, int, pctrk &pc_trk);
