#include "PadVertexFunction.h"
#include <math.h>
#include "gsl/gsl_math.h"

#include "dPadClusterWrapper.h"
#include "PHTable.hh"
#include "PHIODataNode.h" 
#include "PHNodeIterator.h" 

#include "emcClusterContainer.h"
#include "emcClusterContent.h"

#include "PHCompositeNode.h"
#include "TH1.h"

#include "PHLine.h"
#include "PHGeometry.h"

using namespace PHGeometry;

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

static int iCall = 0;
static int H_NBINS = 200;
static float H_LOWEND = -100.;
static float H_HIGHEND = 100.;

PadVertexFunction::PadVertexFunction()
{
  Init();
}

PadVertexFunction::~PadVertexFunction()
{
  if (h1PC1PC3) delete h1PC1PC3;   
}

int PadVertexFunction::Init()
{
  h1PC1PC3 = new TH1F("h1PC1PC3", "PC1/PC3 Z", 
		      H_NBINS, H_LOWEND, H_HIGHEND); 
  iCall = 1;
  verbose = 0;
  vpctrk.clear();

  InitPars();

  return 0;
}

void PadVertexFunction::InitPars()
{
  // Init cuts too 
  _match_limit_z = 5.0;
  _match_limit_phi = 0.012;
  _match_limit_x = 10.0;
  _match_limit_y = 10.0;

  // for evaluation at PC3 (these are actually PC2 values but the relative values should be ok)
  _sigma_dphi = 0.0045;
  _sigma_dz = 2.1;

  _min_cluster_energy = 0.15;

  // maximum pc3 multiplicity
  _max_pc3_hits = 20;

  _n_search_bins = 2; // +- 2 bins

  _min_vtx_error = 0.5;
  _max_vtx_error = 10;

  return;
}

void PadVertexFunction::PrintPars(std::ostream& os) const
{
  os << " _match_limit_x " << _match_limit_x 
     << " _match_limit_y " << _match_limit_y 
     << " _match_limit_z " << _match_limit_z << endl;

  os << " _sigma_dphi " << _sigma_dphi 
     << " _sigma_dz " << _sigma_dz << endl;

  os << " _n_search_bins " << _n_search_bins << endl;

  os << " _max_pc3_hits " << _max_pc3_hits << endl;

  os << " _min_vtx_error " << _min_vtx_error 
     << " _max_vtx_error " << _max_vtx_error 
     << endl;
  
  return;  
}

int PadVertexFunction::event(PHCompositeNode* topNode, 
			     double *z0PC, double *z0PCerr,
			     int *fault) 
{
  *z0PC = -99999.0;
  *z0PCerr = 0.;

  // init cluster tables
  dPadClusterWrapper *dPc1Cluster = NULL; 
  dPadClusterWrapper *dPc3Cluster = NULL;

  PHNodeIterator topIter(topNode);

  // Get PC3 Node
  TableNode_t * dPc3ClusterNode = static_cast<TableNode_t*>
    (topIter.findFirst("PHIODataNode","dPc3Cluster"));
  if (dPc3ClusterNode)
    {
      dPc3Cluster = static_cast<dPadClusterWrapper*>
	(dPc3ClusterNode->getData());
    }

  if (!dPc3Cluster)
    {
      cerr << __FILE__ << ":" << __LINE__
           << " cannot find dPc3Cluster object." << endl;
      return false;
    }
  int npc3 = dPc3Cluster->RowCount();
  if ( npc3>_max_pc3_hits ) return false;	// leave if the multiplicity is too high.


  // get pc1 node
  TableNode_t *dPc1ClusterNode = static_cast<TableNode_t*>
    (topIter.findFirst("PHIODataNode","dPc1Cluster"));
  // if we got the cluster nodes, get the wrapped tables
  if (dPc1ClusterNode)
    {
      dPc1Cluster = static_cast<dPadClusterWrapper*>
	(dPc1ClusterNode->getData());
    }
  if (!dPc1Cluster)
    {
      cerr << __FILE__ << ":" << __LINE__
           << " cannot find dPc1Cluster object." << endl;
      return false;
    }

  // let's get EMC clusters
  PHIODataNode<PHObject>* emcClusterContainerNode =
    (PHIODataNode<PHObject>*)topIter.findFirst("PHIODataNode",
					       "emcClusterContainer");
  if ( !emcClusterContainerNode )
    {
      cerr << __FILE__ << ":" << __LINE__
           << " cannot find emcClusterContainer node." << endl;
      return false;
    }
  
  emcClusterContainer* fClusters =
    static_cast<emcClusterContainer*>(emcClusterContainerNode->getData());
  if (!fClusters)
    {
      cerr << __FILE__ << ":" << __LINE__
           << " cannot find emcClusterContainer object." << endl;
      return false;
    }
  else
    {
/*
      if ( verbose )
        {
          fClusters->identify();
          fClusters->print();
        }
*/
    }

  find_pctrk(dPc1Cluster, dPc3Cluster, fClusters);

  remove_duplicate();

  find_vertex(z0PC, z0PCerr, fault);

  return 0;
}

void PadVertexFunction::find_pctrk(dPadClusterWrapper *dPc1Cluster,
				   dPadClusterWrapper *dPc3Cluster,
				   emcClusterContainer* fClusters)
{  // basically a copy&update of offline/packages/CNT/PCTracking code from 
  // Y. Akiba, but without worrying about momentum values

  int npc1 = dPc1Cluster->RowCount();
  int npc3 = dPc3Cluster->RowCount();
  int nemc = fClusters->size();

  /* MC
    cout << PHWHERE << " INFO:" 
       << " npc1 " << npc1
       << " npc3 " << npc3
       << " nemc " << nemc
       << endl;
  */
  vpctrk.clear();

  PHPoint pc1xyz, emcxyz, pc3xyz;
  float dphi, dz;
  int pc1arm;
  emcClusterContent *emc;

  for(int ipc1=0;ipc1<npc1;ipc1++) {
    if(dPc1Cluster->get_xyz(0,ipc1)>0) {//in West Arm
      pc1arm = 1;
    }
    else {
      pc1arm = 0;
    }
    PHPoint pc1xyz(dPc1Cluster->get_xyz(0,ipc1),
		   dPc1Cluster->get_xyz(1,ipc1),
		   dPc1Cluster->get_xyz(2,ipc1));

    for(int iemc=0;iemc<nemc;iemc++) {

      emc = fClusters->getCluster(iemc);
      if( ((emc->x()>0 && pc1arm==1) ||
	   (emc->x()<0 && pc1arm==0)) &&
	  emc->ecore()>_min_cluster_energy) {//in same arm as PC1 hit

	PHPoint emcxyz(emc->x(),
		       emc->y(),
		       emc->z());
	PHLine pc1emc(pc1xyz,emcxyz);

	for(int ipc3=0;ipc3<npc3;ipc3++) {

	  if( (dPc3Cluster->get_xyz(0,ipc3)>0 && pc1arm==1) ||
	      (dPc3Cluster->get_xyz(0,ipc3)<0 && pc1arm==0) ) {
	    //in same arm as PC1 hit

	    PHPoint pc3xyz(dPc3Cluster->get_xyz(0,ipc3),
			   dPc3Cluster->get_xyz(1,ipc3),
			   dPc3Cluster->get_xyz(2,ipc3));

	    PHPoint pc3proj = closestApproachLinePoint(pc1emc,pc3xyz);
	    dz = pc3xyz.getZ() - pc3proj.getZ();
	    float dx = pc3xyz.getX() - pc3proj.getX();
	    float dy = pc3xyz.getY() - pc3proj.getY();
	    if(fabs(dz)<_match_limit_z && 
	       fabs(dx)<_match_limit_x && 
	       fabs(dy)<_match_limit_y) {

	      float phi      = atan2(pc3xyz.getY(),pc3xyz.getX());
	      float phi_proj = atan2(pc3proj.getY(),pc3proj.getX());
	      dphi = phi - phi_proj;
	      if (fabs(dphi)<_match_limit_phi) {
		vpctrk.push_back(pctrk(dz,dphi,
				       pc1xyz,pc3xyz,emcxyz,
				       ipc1,ipc3,iemc,emc->ecore()));
	      }
	    }
	  }
	}
      }
    }
  }
  return;
}

void PadVertexFunction::remove_duplicate()
{ // also from Y. Akiba
  int npctrk =vpctrk.size();
  //DS  cout << PHWHERE << " npctrk " << npctrk << endl;
  /*
  STEP1: removed ghost tracks in PC-EMC tracking.
  This is caused by the following situation:
  A single track somehow causes two near-by clusters in PC1 or EMC, or there 
  is a background hit in PC1 or EMC that is close to real PC-EMC track.
  In this situation, we have 
  (A) 2 near-by hit in PC and single hit in EMC, and one of the PC hit is a 
  background, or 
  (B) 1 PC hit and 2 near-by EMC clusters, one of which is a backgound. 
  In these situation, we have two tracks reconstructed, but only one of them 
  is REAL and the other one is a ghost caused by the real track.
  In the following code, I remove such ghost tracks in vpctrk. (set ghost flag)
  */

  if(npctrk>=2) {

    for(int ipctrk=0;ipctrk<npctrk-1;ipctrk++) {
      for(int jpctrk=ipctrk+1;jpctrk<npctrk;jpctrk++) {
	
	if((!vpctrk[ipctrk].ghost)&&(!vpctrk[jpctrk].ghost)) {
	  
	  bool same_pc1 = false;
	  bool same_emc = false;
	  if (vpctrk[ipctrk].ipc1 == vpctrk[jpctrk].ipc1) {
	    same_pc1 = true;
	    /*DS
	    cout << PHWHERE << " i " << ipctrk
		 << " j " << jpctrk
		 << " same pc1 " << vpctrk[ipctrk].ipc1
		 << endl;
	    */
	  }
	  if (vpctrk[ipctrk].iemc == vpctrk[jpctrk].iemc) {
	    same_emc = true;
	    /*DS
	    cout << PHWHERE << " i " << ipctrk
	         << " j " << jpctrk
	         << " same emc " << vpctrk[ipctrk].iemc
		 << endl;
	    */
	  }
	  
	  if (same_pc1 || same_emc) { // one is a ghost; case A or B
	    // by design two tracks should not be the same for both pc1 and
	    // emc, but just to be general, let's include that possibility
	    // too
	    
	    if (same_emc || same_pc1) {
	      // assign the latter to be a ghost
	      vpctrk[jpctrk].ghost = true;
	    }
	    else {
	      // let's just keep the one that has the best matching to PC3
	      
	      float sphi_i = vpctrk[ipctrk].dphi/_sigma_dphi;
	      float sz_i   = vpctrk[ipctrk].dz/_sigma_dz;
	      float sphi_j = vpctrk[jpctrk].dphi/_sigma_dphi;
	      float sz_j   = vpctrk[jpctrk].dz/_sigma_dz;
	      if ( (sphi_i*sphi_i + sz_i*sz_i) > (sphi_j*sphi_j + sz_j*sz_j) ) {
		vpctrk[ipctrk].ghost = true;
		//DS cout << PHWHERE << " ghost - ipctrk "  << ipctrk << endl;
	      } 
	      else {
		vpctrk[jpctrk].ghost = true;
		//DS cout << PHWHERE << " ghost - jpctrk "  << jpctrk << endl;
	      }
	    }
	  }
	}
      }
    }
  }
  return;
}

void PadVertexFunction::find_vertex(double *z0PC, double *z0PCerr,
				    int *fault)
{
  // ok, we now have a vector of tracks, hopefully..
  int verbose = fault[0];
  int ntracks;
  *z0PC = -99999.0;
  // Reset histogram before starting to fill it
  h1PC1PC3->Reset();
  ntracks = 0;
  *fault = 0;
  iCall++;

  //
  // Direct calculation of Z vertex from PC1/PC3 (R,Z) information
  // Require at least 1 track
  //

  int npctrk =vpctrk.size();
  //DS  cout << PHWHERE << " npctrk " << npctrk << endl;
  std::vector<float> pcvertices;
  pcvertices.clear();

  for (int ipctrk=0;ipctrk<npctrk;ipctrk++) {
    if (!vpctrk[ipctrk].ghost) { // no ghosts included

      //DS      vpctrk[ipctrk].print(cout);
      
      float rPc1 = sqrt(vpctrk[ipctrk].pc1xyz.getX()*
			vpctrk[ipctrk].pc1xyz.getX() + 
			vpctrk[ipctrk].pc1xyz.getY()*
			vpctrk[ipctrk].pc1xyz.getY());
      float zPc1 = vpctrk[ipctrk].pc1xyz.getZ();
      
      float rPc3 = sqrt(vpctrk[ipctrk].pc3xyz.getX()*
			vpctrk[ipctrk].pc3xyz.getX() + 
			vpctrk[ipctrk].pc3xyz.getY()*
			vpctrk[ipctrk].pc3xyz.getY());
      float zPc3 = vpctrk[ipctrk].pc3xyz.getZ();
      
      float zIntercept = (rPc3*zPc1 - rPc1*zPc3)/(rPc3-rPc1);
      //DS cout << PHWHERE << " zIntercept " << zIntercept << endl;      
      if (zIntercept>H_LOWEND && zIntercept<H_HIGHEND) {
	
	h1PC1PC3->Fill(zIntercept);
	pcvertices.push_back(zIntercept);
	ntracks++;
      } // in histogram range
    } // no ghost
  }// trk loop
      
  //
  // Obtain PC1 PC3 Z0
  //
  Stat_t sumHist = h1PC1PC3->GetSum();
  float maxPeak = -89999.;
  float maxSum = -89999.;
  float weightedMaxPeak = -89999;
  
  std::vector<float> vertsums;
  vertsums.clear();

  if(sumHist > 0) {
    
    // go through the vertices and do an integral check around each
    maxSum = 0;
    float pcvert;
    int vertbin;
    int minbin, maxbin;
    float vert_integral;

    for (unsigned int ipc=0; ipc<pcvertices.size(); ipc++) {
      pcvert = pcvertices[ipc];
      vertbin = h1PC1PC3->GetXaxis()->FindBin(pcvert);

      minbin = vertbin - _n_search_bins;
      maxbin = vertbin + _n_search_bins;
      if (minbin<1) { minbin = 1; }
      if (maxbin>H_NBINS) { maxbin = H_NBINS; }

      vert_integral = h1PC1PC3->Integral(minbin, maxbin);
      if (vert_integral > maxSum) {
	maxSum = vert_integral;
	vertsums.clear();
	vertsums.push_back(pcvert);
      }
      else if (vert_integral == maxSum) {
	vertsums.push_back(pcvert);
      }
    } // loop over vertices 
      
    if (vertsums.size()==1) { // one main candidate 	
      *z0PC = vertsums[0];
      *fault = 0;
      *z0PCerr = 1./maxSum;
    }
    else if (vertsums.size()>1) { // more than one candidate
      // take average
      *z0PC = 0;
      for (unsigned int ipc=0; ipc<vertsums.size(); ipc++) {
	*z0PC += vertsums[ipc];
      }  
      *z0PC /= vertsums.size();
      // calc error too
      float sigma2 = 0;
      for (unsigned int ipc=0; ipc<vertsums.size(); ipc++) {
	sigma2 += (*z0PC - vertsums[ipc])*(*z0PC - vertsums[ipc]);
      } 
      *z0PCerr = sqrt(sigma2/vertsums.size());
      *fault = 0;
    }

    if (*z0PCerr > _max_vtx_error) { // too big, must be wrong
      *fault = 1;
    }
    else if (*z0PCerr < _min_vtx_error) { // too small; underestimated error(?)
      *z0PCerr = _min_vtx_error;
    }

  }  // check on sumHist > 0
  
  if(verbose) {
    cout << PHWHERE << "\n Event " << iCall
	 << ", ntracks = " << ntracks
	 << ", sumHist = " << sumHist
	 << ", maxPeak = " << maxPeak
	 << ", weighted maxPeak = " << weightedMaxPeak
	 << ", maxSum = " << maxSum
	 << endl;
  } // verbose check
  
  return;
}



