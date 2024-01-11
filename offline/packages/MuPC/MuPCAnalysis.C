#include "PHIODataNode.h"

#include "MuPCAnalysis.h"

#include "MuPCCluster.h"
#include "MuPCSnglCluster.h"
#include "mupcghitWrapper.h"
#include "dio_trk.hh"

#include <gsl/gsl_math.h>

#include <iostream>
#include <cmath>

using namespace std;

// Helpers for scanning Node Tree...
typedef PHIODataNode <PHObject>   PHObjectNode_t;
typedef PHIODataNode <MuPCCluster>     MuPCClusterNode_t;
typedef PHIODataNode <mupcghitWrapper> GHITNode_t;

//__________________________________________
MuPCAnalysis::MuPCAnalysis()
{
  cout << "MuPCAnalysis::Constructor " <<endl;
}

//__________________________________________
int MuPCAnalysis::Init(PHCompositeNode *topNode)
{
  cout << "MuPCAnalysis::Init Initializing the MuPC " <<endl;
  return 0;
}

//__________________________________________
int MuPCAnalysis::InitRun(PHCompositeNode *topNode)
{
  return 0;
}

//__________________________________________
int MuPCAnalysis::Reset(PHCompositeNode *topNode)
{
  mupc1cluster->Reset();
  mupc2cluster->Reset();
  mupc3cluster->Reset();
  cout << "MuPCAnalysis::Reset Resetting the MuPC " <<endl;
  return 0;
}

//__________________________________________
int MuPCAnalysis::Event(PHCompositeNode *topNode)
{
  GetNodes(topNode);

  fillAllCluster();
  cout << "MuPCAnalysis::Event Analyzing the MuPC " <<endl;
  return 0;
}
//__________________________________________
int MuPCAnalysis::GetNodes(PHCompositeNode *topNode) 
{
  mupc1cluster    = 0;     
  PHTypedNodeIterator<MuPCCluster> icluster1(topNode);
  MuPCClusterNode_t *cluster1 = icluster1.find("MuPC1Cluster");
  if(cluster1) mupc1cluster = cluster1->getData();
  if (!mupc1cluster) cout << PHWHERE << "MuPC1Cluster data not in Node Tree" << endl;
  
  mupc2cluster    = 0;     
  PHTypedNodeIterator<MuPCCluster> icluster2(topNode);
  MuPCClusterNode_t *cluster2 = icluster2.find("MuPC2Cluster");
  if(cluster2) mupc2cluster = cluster2->getData();
  if (!mupc2cluster) cout << PHWHERE << "MuPC2Cluster data not in Node Tree" << endl;
  
  mupc3cluster    = 0;     
  PHTypedNodeIterator<MuPCCluster> icluster3(topNode);
  MuPCClusterNode_t *cluster3 = icluster3.find("MuPC3Cluster");
  if(cluster3) mupc3cluster = cluster3->getData();
  if (!mupc3cluster) cout << PHWHERE << "MuPC3Cluster data not in Node Tree" << endl;
  
  mupc1ghit    = 0;     
  PHTypedNodeIterator<mupcghitWrapper> ighit1(topNode);
  GHITNode_t *ghit1 = ighit1.find("mupc1ghit");
  if(ghit1) mupc1ghit = ghit1->getData();
  if (!mupc1ghit) cout << PHWHERE << "mupc1ghit data not in Node Tree" << endl;
  
  mupc2ghit    = 0;     
  PHTypedNodeIterator<mupcghitWrapper> ighit2(topNode);
  GHITNode_t *ghit2 = ighit2.find("mupc2ghit");
  if(ghit2) mupc2ghit = ghit2->getData();
  if (!mupc2ghit) cout << PHWHERE << "mupc2ghit data not in Node Tree" << endl;
  
  mupc3ghit    = 0;     
  PHTypedNodeIterator<mupcghitWrapper> ighit3(topNode);
  GHITNode_t *ghit3 = ighit3.find("mupc3ghit");
  if(ghit3) mupc3ghit = ghit3->getData();
  if (!mupc3ghit) cout << PHWHERE << "mupc3ghit data not in Node Tree" << endl;
  
  return 0;
}

//____________________________________________________
void MuPCAnalysis::ancestor(int true_track, float& ptot, float& ptheta, 
	                   float& pphi, float& r_vertex, float& z_vertex, 
			   int& idparent, int& idpart, int& idorigin, 
			   float& ptotpri, float& pthetpri, float& pphipri, 
	      		   float& z0vertex)
{
  int nfile;
  int error;
  float theta_vertex;
  float phi_vertex;
  int itparent;
  int evttrack;
  int itorigin;

  dio_TrueTrackToEtrack(&true_track, &evttrack, &nfile);  // not working?
  if(evttrack < 1) {
	std::cerr << "\n rootAncPad <E> : evttrack invalid " << evttrack << std::endl;
  }

  dio_ptrkorigin(&true_track, &nfile, &error, 
		     &ptotpri, &pthetpri, &pphipri,
	  	     &r_vertex, &z0vertex, &theta_vertex, &phi_vertex, 
		     &itorigin, &idorigin, &idpart);

  dio_ptrkstack(&true_track, &nfile, &error, &ptot, &ptheta, &pphi,
		    &r_vertex, &z_vertex, &theta_vertex, &phi_vertex, 
		    &itparent, &idparent, &idpart);
}
//__________________________________
void MuPCAnalysis::fillMuPCCluster(mupcghitWrapper* mupcghit, MuPCCluster* mupccluster)
{
  float  ptot;
  float  ptheta;
  float  pphi;
  float  r_vertex;
  float  z_vertex;
  int  idparent;
  int  idpart;
  int  idorigin;
  float  ptotpri;
  float  pthetpri;
  float  pphipri;
  float  z0vertex;

  for(unsigned int ipc = 0; ipc<mupcghit->RowCount(); ipc++) {
     int trk = mupcghit->get_mctrack(ipc);
     ancestor(trk, ptot, ptheta, pphi, r_vertex, z_vertex, idparent, idpart, idorigin, ptotpri, pthetpri, pphipri, z0vertex);
     float pcx = mupcghit->get_xyzinglo(0, ipc);
     float pcy = mupcghit->get_xyzinglo(1, ipc);
     float pcz = mupcghit->get_xyzinglo(2, ipc);
     
     //     float rpc = sqrt(pcx*pcx+pcy*pcy);
     float phipc = 180/M_PI*atan2(pcy, pcx);
     
     mupccluster->AddCluster(ipc);
     mupccluster->set_ncluster(ipc+1);
     mupccluster->get_cluster(ipc)->set_phi(phipc);
     mupccluster->get_cluster(ipc)->set_xglo(pcx);
     mupccluster->get_cluster(ipc)->set_yglo(pcy);
     mupccluster->get_cluster(ipc)->set_zglo(pcz);
     mupccluster->get_cluster(ipc)->set_idorigin(idorigin);
     mupccluster->get_cluster(ipc)->set_idpart(idpart);
     mupccluster->get_cluster(ipc)->set_idparent(idparent);
     mupccluster->get_cluster(ipc)->set_ptot(ptot);
     mupccluster->get_cluster(ipc)->set_ptheta(ptheta);
     mupccluster->get_cluster(ipc)->set_pphi(pphi);
     mupccluster->get_cluster(ipc)->set_r_vertex(r_vertex);
     mupccluster->get_cluster(ipc)->set_z_vertex(z_vertex);
     mupccluster->get_cluster(ipc)->set_ptotpri(ptotpri);
     mupccluster->get_cluster(ipc)->set_pthetpri(pthetpri);
     mupccluster->get_cluster(ipc)->set_pphipri(pphipri);
     mupccluster->get_cluster(ipc)->set_z0vertex(z0vertex);
  } 
  return;
}

//_______________________________________
void MuPCAnalysis::fillAllCluster()
{
  fillMuPCCluster(mupc1ghit, mupc1cluster);
  fillMuPCCluster(mupc2ghit, mupc2cluster);
  fillMuPCCluster(mupc3ghit, mupc3cluster);
}
