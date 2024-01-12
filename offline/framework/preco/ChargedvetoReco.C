#include "ChargedvetoReco.h"

#include <PHCompositeNode.h>

#include <phool.h>
#include <VtxOut.h>

#include <PHObject.h>
#include <PHCentralTrack.h>
#include <emcClusterContainer.h>
#include <emcClusterContent.h>
#include <PadCluster.h>

#include <PHIODataNode.h>
#include <Fun4AllReturnCodes.h>
#include <getClass.h>

#include <iostream>

using namespace std;

int
ChargedvetoReco::process_event(PHCompositeNode *topNode)
{
  // VTX
  VtxOut *d_vtx = findNode::getClass<VtxOut>(topNode,"VtxOut");
  if (!d_vtx)
    {
      cout << PHWHERE << " no VtxOut node" << endl;
      return ABORTEVENT;
    }

  // Particle
  PHCentralTrack *particle = findNode::getClass<PHCentralTrack>(topNode,"PHCentralTrack");
  if (!particle) 
    {
      cout << PHWHERE << " no PHCentralTrack node" << endl;
      return ABORTEVENT;
    }

  // PC3

  PadCluster *d_pc3 = findNode::getClass<PadCluster>(topNode,"Pc3Cluster");
  if (!d_pc3) 
    {
      cout << PHWHERE << " no Pc3Cluster node" << endl;  
      return ABORTEVENT;
    }
  
  // EMC
  emcClusterContainer *d_emc = findNode::getClass<emcClusterContainer>(topNode,"emcClusterContainer");
  if (! d_emc)
    {
      cout << PHWHERE << " No emcClusterContainer Node" << endl;
      return ABORTEVENT;
    }
  float Zvtx = d_vtx->get_ZVertex();
  
  for (unsigned int iemc=0; iemc<d_emc->size(); iemc++) {
    emcClusterContent *emcclus = d_emc->getCluster(iemc);
    
    float min_dist = 9999.;
    float min_dz = 9999.;
    float min_dphi = 9999.;
    int pc3index = -1;
    
    float emcx = emcclus->x();
    float emcy = emcclus->y();
    float emcz = emcclus->z();
    float emc_hit_radius=sqrt(emcx*emcx+emcy*emcy);    
    
    unsigned int numpc3clus = d_pc3->get_PadNCluster();

    for (unsigned int ipc3=0; ipc3<numpc3clus; ipc3++)
      {
	// require same arm (emc arm is opposite to rest of phenix)
	if ( emcclus->arm() == d_pc3->get_arm(ipc3) ) continue;
	
	float pc3x = d_pc3->get_xyz(ipc3, 0);
	float pc3y = d_pc3->get_xyz(ipc3, 1);
	float pc3z = d_pc3->get_xyz(ipc3, 2);
	
	float pc3_hit_radius=sqrt(pc3x*pc3x+pc3y*pc3y);
	
	float pc3z_at_emc = Zvtx + emc_hit_radius*((pc3z-Zvtx)/(pc3_hit_radius));
	
	float dx = emcx - pc3x;
	float dy = emcy - pc3y;
	float dz = emcz - pc3z_at_emc;
	
	float dist = sqrt( dx*dx + dy*dy + dz*dz );
	
	if ( dist < min_dist )
	  {
	    min_dist = dist;
	    min_dz = dz;
	    float emcphi = atan2( emcx, emcy );
	    float pc3phi = atan2( pc3x, pc3y );
	    min_dphi = pc3phi - emcphi;
	    pc3index = ipc3;
	  }
      }
    
    emcclus->set_emcpc3( pc3index );  // closest PC3-Zvertex track to emc hit, this is a pc3 index
    emcclus->set_emcpc3dphi( min_dphi ); // distance (signed/radians) closest PC3-Zvertex track to emc hit
                                         // assumes alpha = 0  
    emcclus->set_emcpc3dz( min_dz ); // distance (signed/cm) closest PC3-Zvertex track to emc hit

    
    float minSigma = 9999;
    short foundpc3index = -9999;
    short foundqual = -9999;
    
    float minEmcDist = 9999;
    
    short foundtrkindex = -9999;
    short trkqual = -9999;
    float emcdz = -9999;
    float emcdphi = -9999;
    float pemc = -9999;
    
    for (unsigned int itrk=0; itrk<particle->get_npart(); itrk++) {
      if (pc3index != -1) {
	if ( pc3index == particle->get_pc3id(itrk) ) {
	  if (fabs(particle->get_pc3sdphi(itrk)) < minSigma) {
	    foundqual = particle->get_quality(itrk);
	    if ( foundqual==31 || foundqual==63 ) {
	      minSigma = fabs(particle->get_pc3sdphi(itrk));
	      foundpc3index = itrk;
	    }
	  }
	}
      }

      if (emcclus->id() == particle->get_emcid(itrk)) {
	
	float trkx = particle->get_pemcx(itrk);
	float trky = particle->get_pemcy(itrk);
	float trkz = particle->get_pemcz(itrk);

	float diffx =  emcx - trkx;
	float diffy =  emcy - trky;
	float diffz =  emcz - trkz;

	float emcprojdist = sqrt(diffx*diffx + diffy*diffy + diffz*diffz);

	if (emcprojdist < minEmcDist) {
	  foundtrkindex = itrk;
	  trkqual = particle->get_quality(itrk); 
	  emcdz = particle->get_emcdz(itrk); 
	  emcdphi = particle->get_emcdphi(itrk); 
	  pemc = particle->get_mom(itrk); 
	}
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




