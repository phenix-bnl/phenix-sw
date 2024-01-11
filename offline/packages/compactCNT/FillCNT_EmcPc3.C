#include <FillCNT_EmcPc3.h>
#include "setIntflag.h"

#include <Fun4AllReturnCodes.h>
#include <PadHitMapEntry.h>
#include <PadHitMap.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <PHCentralTrack.h>
#include <PHSnglCentralTrack.h>
#include "PHGlobal.h"

#include <PHAngle.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <phool.h>
#include <getClass.h>

#include <sstream>
#include <vector>

using namespace std;

FillCNT_EmcPc3::FillCNT_EmcPc3(const std::string &name): SubsysReco(name)
{
  return;
}

int
FillCNT_EmcPc3::InitRun(PHCompositeNode *topNode)
{
  return EVENT_OK;
}

int
FillCNT_EmcPc3::process_event(PHCompositeNode *topNode)
{
  PHGlobal *glb = findNode::getClass<PHGlobal>(topNode, "PHGlobal");
  if (!glb) 
    {
      cout << PHWHERE << "FillCNT_EmcPc3:: PHGlobal not in Node Tree!" << endl;
      return -1;
    }
  float Zvtx = glb->getBbcZVertex();

  PHCentralTrack *particle = findNode::getClass<PHCentralTrack>(topNode, "PHCentralTrack");
  if (!particle) 
    {
      cout << PHWHERE << "FillCNT_EmcPc3:: PHCentralTrack not in Node Tree!" << endl;
      return -1;
    }

  PadHitMap *d_pc3  = findNode::getClass<PadHitMap>(topNode, "Pc3Hit_comp");
  if (!d_pc3) 
    {
      cout << PHWHERE << "FillCNT_EmcPc3:: Pc3Hit_comp not in Node Tree!" << endl;  
      return -1;
    }

  emcClusterContainer *d_emc  = findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  if (!d_emc) 
    {
      cout << PHWHERE << "FillCNT_EmcPc3:: emcClusterContainer not in node tree!" << endl;
      return -1;
    }
  
  const unsigned npart = particle ? particle->get_npart() : 0;

  // Cache some PHCentralTrack variables since we will check these a lot
  std::vector<std::pair<int, int> > track_ids; // [particle_index]={emcid, pc3id}
  for (unsigned i = 0; i<npart; i++) {
    const int emcid = particle->get_emcid(i);
    const int pc3id = particle->get_pc3id(i);
    track_ids.push_back(std::make_pair(emcid, pc3id));
  }

  // cache PadHitMapEntries as well, PadHitMap::GetHit is nothing for a O(n^2) loop
  std::vector<const PadHitMapEntry*> pchit_cache;
  if (d_pc3) {
    int ipc_hit = 0;
    while (const PadHitMapEntry *pchit = d_pc3->GetHit(ipc_hit++))
      pchit_cache.push_back(pchit);
  }

  // Loop over Emc clusters and check for matching Pc3 hits
  for (unsigned int iemc=0; iemc<d_emc->size(); iemc++) {
    emcClusterContent *emcclus = d_emc->getCluster(iemc);
    
    //-* if there is no pc3 object, we set crazy values.
    if ( !d_pc3 )
      {
	emcclus->set_emcpc3dz( 9999. );
	emcclus->set_emcpc3dphi( 9999. );
	emcclus->set_emcpc3( -1 );
	return -1;
      }
    
    float min_dist = 9999.;
    float min_dz = 9999.;
    float min_dphi = 9999.;
    int pc3index = -1;
    
    const float emcx = emcclus->x();
    const float emcy = emcclus->y();
    const float emcz = emcclus->z();
    const float emc_hit_radius = std::sqrt(emcx*emcx+emcy*emcy);    

    for (unsigned i = 0; i < pchit_cache.size(); i++) {
      const PadHitMapEntry* pchit = pchit_cache[i];
      const float pc3x = pchit->get_xyz(0);

      // require same arm (emc arm is opposite to rest of phenix)
      if ( (emcx < 0 && pc3x < 0) || (emcx > 0 && pc3x > 0) )
      {
        const float pc3y = pchit->get_xyz(1);
        const float pc3z = pchit->get_xyz(2);
        const float pc3_hit_radius = std::sqrt(pc3x*pc3x+pc3y*pc3y);

        float pc3z_at_emc = Zvtx + emc_hit_radius*((pc3z-Zvtx)/(pc3_hit_radius));

        float dx = emcx - pc3x;
        float dy = emcy - pc3y;
        float dz = emcz - pc3z_at_emc;

        float dist = sqrt( dx*dx + dy*dy + dz*dz );

        // Note that this takes the match with the smallest min_dist

        if ( dist < min_dist )
        {
          min_dist = dist;
          min_dz = dz;
          float emcphi = std::atan2( emcx, emcy );
          float pc3phi = std::atan2( pc3x, pc3y );
          min_dphi = pc3phi - emcphi;
          pc3index = i;
        }
      }
    }
    
    emcclus->set_emcpc3( pc3index );  // closest PC3-Zvertex track to emc hit, this is a pc3 index
    emcclus->set_emcpc3dphi( min_dphi ); // distance (signed/radians) closest PC3-Zvertex track to emc hit
                                         // assumes alpha = 0  
    emcclus->set_emcpc3dz( min_dz ); // distance (signed/cm) closest PC3-Zvertex track to emc hit

    
    //-* if there is no particle object, we set crazy values.
    if ( !particle )
      {
	emcclus->set_emcpc3neartrk( -1 );
	emcclus->set_emctrk( -1 );
	emcclus->set_emctrkdz( 9999. );
	emcclus->set_emctrkdphi( 9999. );
	emcclus->set_pemctrk( 9999. );
	emcclus->set_emctrkquality( -1 );
	return -1;
      }
    
    float minSigma = 9999;
    short foundpc3index = -9999;
    short foundqual = -9999;
    
    float minEmcDist = 9999;
    
    short foundtrkindex = -9999;
    short trkqual = -9999;
    float emcdz = -9999;
    float emcdphi = -9999;
    float pemc = -9999;

    const int emcid = emcclus->id();
    for (unsigned int itrk=0; itrk<npart; itrk++) {
      if (pc3index != -1 and
          pc3index == track_ids[itrk].second and
          fabs(particle->get_pc3dphi(itrk)) < minSigma) {
        foundqual = particle->get_quality(itrk);
        if ( foundqual==31 || foundqual==63 ) {
          minSigma = std::abs(particle->get_pc3dphi(itrk));
          foundpc3index = itrk;
        }
      }

      if (emcid != track_ids[itrk].first)
        continue;

      float trkx = particle->get_pemcx(itrk);
      float trky = particle->get_pemcy(itrk);
      float trkz = particle->get_pemcz(itrk);

      float diffx =  emcx - trkx;
      float diffy =  emcy - trky;
      float diffz =  emcz - trkz;

      float emcprojdist = std::sqrt(diffx*diffx + diffy*diffy + diffz*diffz);

      if (emcprojdist < minEmcDist) {
        foundtrkindex = itrk;
        trkqual = particle->get_quality(itrk); 
        emcdz = particle->get_emcdz(itrk); 
        emcdphi = particle->get_emcdphi(itrk); 
        pemc = particle->get_mom(itrk); 
      }
    }

    emcclus->set_emcpc3neartrk( foundpc3index );  // index of closest high quality track to pc3 hit, 
                                                  // which in turn was closest pc3z track to emc hit
                                                  // this is the dchtrack/cgltrack/phcentraltrack index

    emcclus->set_emctrk( foundtrkindex );       // closest track to emc hit
    emcclus->set_emctrkdz( emcdz );             // distance dz (cm) of closest track
    emcclus->set_emctrkdphi( emcdphi );         // distance dphi (radians) of closest track
    emcclus->set_pemctrk( pemc );               // mom of closest track (not pt)
    emcclus->set_emctrkquality( trkqual );      // quality of closest track 
  }

  return EVENT_OK;
}

