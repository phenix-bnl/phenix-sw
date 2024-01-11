// The recalreco module corrects the energy scales in pairs of
// sectors in the EMCal. The functions were derived using
// pi0 peak positions from the Run4 200 GeV data set.

#include "EmcClusterContainerResurrector.h"

#include "emcClusterContainer.h"
#include "emcClusterContainerv6.h"
#include "emcClusterContent.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"

#include "mEmcGeometryModule.h"
#include "emcTowerContainerDST.h"
#include "emcClusterAuxInfoContainerV1.h"
#include "emcClusterAuxInfoV1.h"
#include "EmcCluster.h"
#include "EmcIndexer.h"

#include "EmcScSectorRec.h"
#include "EmcGlSectorRec.h"

#include "emcNodeHelper.h"

#include "VtxOut.h"
#include "PHGlobal.h"

#include "Fun4AllReturnCodes.h"
#include "Fun4AllServer.h"
#include "getClass.h"
#include "recoConsts.h"


#include <iostream>
#include <fstream>
#include <cmath>

using namespace std;
using namespace findNode;

static const int HVECTSIZE=4608;

const float fgTowerThresholdPbSc = 0.010;  
const float fgTowerThresholdPbGl = 0.014;  

namespace {

  //_________________________________________________________________________
  void  computeCorrectedDispersion(float ycg, float zcg,
                                   float dispy, float dispz,
                                   float yModSize, float zModSize,
                                   float& corrdispy, float& corrdispz)
  {
    float zpos = zcg/zModSize;
    float ypos = ycg/yModSize;

    corrdispy = dispy/(yModSize*yModSize);
    corrdispz = dispz/(zModSize*zModSize);

    float zposmod = zpos - floor(zpos);
    float yposmod = ypos - floor(ypos);
    corrdispz -= ( zposmod - zposmod*zposmod);
    corrdispy -= ( yposmod - yposmod*yposmod);
  }

  void ToF_Process(std::vector<emcTowerContent*> phit,
			       float dist,   emcTowerContent*  hmax,
			       float* ptof, float* petof, float* ptofcorr,
			       float* pdtof,
			       float* ptofmin, float* petofmin, 
			       float* ptofmincorr,
			       float* ptofmax, float* petofmax, 
			       float* ptofmaxcorr,
			       float& tofdisp)
{
  float tof, tofcorr, etof, tflash;
  float tofmin, tofmincorr, etofmin, tofmax, tofmaxcorr, etofmax;

  tof = hmax->ToF();
  etof = hmax->Energy();

  tofmax = 0;
  tofmin = 999;
  etofmax = 0;
  etofmin = 0;

  float tofsum=0;
  float tofsum2=0;
  int nsum = 0;

  float a,b;

  std::vector<emcTowerContent*>::iterator ph = phit.begin();

  for (;  ph != phit.end(); ++ph)
    {
      if ( (a=(*ph)->Energy()) > 0)
        {
          if ( ( b=(*ph)->ToF()) > tofmax)
            {
              tofmax = b;
              etofmax = a;
            }
          if ( b < tofmin)
            {
              tofmin = b;
              etofmin = a;
            }
	  tofsum += b;
	  tofsum2 += b * b;
	  ++nsum;
        }
    }

  const float tofmean = tofsum/nsum;
  const float tofdisp_squared = (tofsum2 - nsum*tofmean*tofmean)/(nsum-1);
  if ( nsum > 1 and tofdisp_squared>=0)
    {
      tofdisp = sqrt((tofsum2 - nsum*tofmean*tofmean)/(nsum-1));
    }
  else
    {
      tofdisp = 0;
    }

  tflash = dist / 30.0;

  tof = tof - tflash;
  tofmin = tofmin - tflash;
  tofmax = tofmax - tflash;

  tofcorr = tof;
  tofmincorr = tofmin;
  tofmaxcorr = tofmax;

  *ptof = tof;
  *petof = etof;
  *ptofcorr = tofcorr;
  *pdtof = 0;
  *ptofmax = tofmax;
  *petofmax = etofmax;
  *ptofmaxcorr = tofmaxcorr;
  *ptofmin = tofmin;
  *petofmin = etofmin;
  *ptofmincorr = tofmincorr;
}

}

EmcClusterContainerResurrector::EmcClusterContainerResurrector() : geom(0)
{
  
  _emcClusterAuxInfoContainerNodeName = "emcClusterAuxInfo";
  _emcTowerContainerNodeName = "emcHitContainer";
  Name("EmcClusterContainerResurrector");
  //  already_issued_warning = 0;
}

EmcClusterContainerResurrector::~EmcClusterContainerResurrector()
{
  std::vector<SecGeom*>::iterator i = fSectorGeometries.begin();
  std::vector<SecGeom*>::iterator iend = fSectorGeometries.end();
  for(; i!=iend; i++)
    {
      delete *i;
    }

  fSectorGeometries.clear();
  
  std::vector<EmcSectorRec*>::iterator j = fSector.begin();
  std::vector<EmcSectorRec*>::iterator jend = fSector.end();
  for(; j!=jend; j++)
    {
      delete *j;
    }

  fSector.clear();

  if(geom)
    {
      delete geom;
    }

}

int EmcClusterContainerResurrector::InitRun(PHCompositeNode *topNode)
{
  
  geom = new mEmcGeometryModule(mEmcGeometryModule::kReal);
   
  int nx, ny;

  float txsz, tysz;
  EmcSectorRec* SectorRec;
  SecGeom* SecGeometry;

  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if ( is < 6 ) 
	{
	  SectorRec = new EmcScSectorRec;
	  SectorRec->SetTowerThreshold(fgTowerThresholdPbSc);
	  fSector.push_back(SectorRec);
	}
      else
	{
	  SectorRec = new EmcGlSectorRec;
	  SectorRec->SetTowerThreshold(fgTowerThresholdPbGl);
	  fSector.push_back(SectorRec);
	}


      geom->GetSectorDim(is, nx, ny);
      geom->GetTowerSize(is, txsz, tysz);
      geom->GetMatrixVector(is, emcrm, emctr);

      SecGeometry = new SecGeom;
      fSectorGeometries.push_back(SecGeometry);
      SecGeometry->nx = nx;
      SecGeometry->ny = ny;
      SecGeometry->Tower_xSize = txsz;
      SecGeometry->Tower_ySize = tysz;

      SectorRec->SetGeometry(*SecGeometry, &emcrm, &emctr);      
    }
  

  // now find the DST tree and make the emcClusterContainer

  emcNodeHelper nh;

  PHCompositeNode* dstNode =  nh.findCompositeNode(topNode,"DST");

  nh.addObject<emcClusterContainerv6>(dstNode,"emcClusterContainer");
 
  //already_issued_warning = 0;

  return 0;
  
}


int EmcClusterContainerResurrector::process_event(PHCompositeNode *topNode)
{
  float fVertex[3];

  //  if (verbosity > 1)
  //  {
  //      std::cout << __FILE__ << " " << __LINE__ << " in process_event " << std::endl;
      //  }
  
   emcClusterAuxInfoContainerV1* _emcClusterAuxInfoContainer_ptr = getClass<emcClusterAuxInfoContainerV1>(topNode, _emcClusterAuxInfoContainerNodeName.c_str());

   //   if ( ! _emcClusterAuxInfoContainer_ptr && !already_issued_warning ) 
   //  {
   //    already_issued_warning = 1;
   //    std::cout << PHWHERE << "Could not find " << _emcClusterAuxInfoContainerNodeName << " Node - using compatibility mode" << std::endl;
   // }
 
  _emcTowerContainer_ptr = getClass<emcTowerContainerDST>(topNode, _emcTowerContainerNodeName.c_str());
  if ( !_emcTowerContainer_ptr ) 
    {
      std::cout << PHWHERE << "Could not find " << _emcTowerContainerNodeName << " Node" << std::endl;
      return -1;
    }
  

  // get the vertex info from the towercontainer 
  int i;
  for ( i = 0; i < 3; i++)
    {
      fVertex[i] = _emcTowerContainer_ptr->getVertex(i); 
    }

  std::vector<EmcSectorRec*>::iterator it;

  for ( it = fSector.begin(); it != fSector.end(); it++)
    {
      (*it)->SetVertex(fVertex);
    }


  int iarm,isector,iy,iz;
  int iS,iST;

  emcClusterContainer *clustercontainer =
    findNode::getClass<emcClusterContainer>(topNode, "emcClusterContainer");
  
  if (!clustercontainer)
    {
      std::cerr << PHWHERE << " No emcClusterContainer object !" << std::endl;
      return 0;
    }

  // clear out anything that may already be in the clustercontainer
  clustercontainer->Reset();

  // no loop through the number of clusters. Since the T0 container has only
  // one tower per cluster, its size is the number of clusters.

  int n_towers = _emcTowerContainer_ptr->size(); 
     
  // pre-set current cluster
  unsigned int current_cluster_nr = 0;
  
  std::vector<emcTowerContent*> mlist;

  // --- now we go through the towers
  int current_tower = 0;
  unsigned int most_recent_clusterid;
  int n_modules = 0;
  int this_cluster_towerid;

  // now we loop through the towers in T0, and fine the matching 
  // towers in the standard container

  for ( ; current_tower < n_towers; ) 
    {  
      emcTowerContentDST *t  = _emcTowerContainer_ptr->getTower(current_tower);

      current_tower++;

      if ( t) 
	{
	  current_cluster_nr =  t->ClusterId();

	  this_cluster_towerid =  t->TowerID();  // we cache this towerid for later

	  if ( t->Energy() >0 &&  t->TowerID() >=0)
	    {
	      n_modules++;
	      mlist.push_back(t);
	      //  cout << __LINE__ << " " << __FILE__ << " cluster  "   << current_cluster_nr << "  ";
	      //t->print();
	    }

	  most_recent_clusterid = current_cluster_nr;

	  // now we loop through the towers until we find the first different cluster id, or
	  // the end of the list.
	  // note that we do not step "current_tower" in the loop construct

	  for ( ; most_recent_clusterid == current_cluster_nr && current_tower < n_towers;  )
	    { 
	      // now we are getting the next tower from the standard container. 
	      t = _emcTowerContainer_ptr->getTower(current_tower);

	      // if this tower still has the same cluster id, we are still seeing 
	      // the same cluster we are currently  working on. We just push it back on the
	      // vector

	      most_recent_clusterid =  t->ClusterId();

	      if ( most_recent_clusterid == current_cluster_nr )
		{
		  if ( t->Energy() >0 &&  t->TowerID() >=0)
		    {
		      n_modules++;
		      mlist.push_back(t);		
		      //  cout << __LINE__ << " " << __FILE__ << " cluster  "   << current_cluster_nr << "  ";
		      // t->print();
		    }
		  current_tower++;  // we only increment current_tower if we are still in this cluster
		  // else we come back to this tower in the next run of the loop
		}
	    }

	  // if we arrive here, we now have a vector of towers ready to go. 
// 	  std::cout << __FILE__ << "  " << __LINE__ << " cluster " 
// 		    << current_cluster_nr 
// 		    << " towerid " << t->TowerID() 
// 		    << " number of towers " << mlist.size() 
// 		    << std::endl;

	  
	  EmcIndexer::iPXiSiST(this_cluster_towerid,iS,iST);
	  EmcIndexer::iSTxyST(iS,iST,iz,iy);
	  EmcIndexer::sectorOnlineToOffline(iS,iarm,isector);	  
	  if ( iarm == 1 )
	    {
	      isector = 7 - isector;
	    }
	  emcClusterAuxInfo *aux = 0;

	  if ( _emcClusterAuxInfoContainer_ptr ) 
	    {
	      aux  =  _emcClusterAuxInfoContainer_ptr->getInfo(current_cluster_nr);
	      //	      std::cout << PHWHERE << " aux info: " << current_cluster_nr << "  "  << aux->getLocalEcore() << "  " << aux->getLocalChi2() << std::endl;
	    }
	  
	  digest_cluster ( clustercontainer, mlist, isector, i, n_modules, fVertex, aux);		  
	  n_modules = 0;
	  mlist.clear();
	}  // end of "next cluster"
    }
  
  return EVENT_OK;
  
}

int EmcClusterContainerResurrector::digest_cluster ( emcClusterContainer *clustercontainer, 
						     std::vector<emcTowerContent*>  &mlist, 
						     const int isector, 
						     const int current_module, 
						     const int n_modules, 
						     const float fVertex[3],
						     emcClusterAuxInfo *aux)
{

  //  EmcModule fHVect[HVECTSIZE];
  EmcPeakarea *pp;

  pp = new EmcPeakarea ( mlist, fSector[isector]);
  // mlist.erase(mlist.begin(), mlist.end());

  emcTowerContent* hmax = pp->GetMaxTowerContentPtr();
  int ndead = pp->GetNDead();
  float qual = ndead ? 1.0 : -ndead;
  
  float e,ecorr,ecore,ecorecorr;
  float xcg,ycg,xcgm,ycgm;
  float xc,yc,xgl,ygl,zgl;
  float xx,xy,yy;
  float chi2;
  float de,dx,dy,dz;
  
  // see if we have the auxiliary information, then we can make a short call.
  if ( aux) 
    {
      chi2  = aux->getLocalChi2();
      ecore = aux->getLocalEcore();
      xcgm   = aux->getLocalx();
      ycgm   = aux->getLocaly();
      pp->GetChar_compactPWG(xcgm, ycgm, ecore,
			     &e, &ecorr,  &ecorecorr,
			     &xcg, &ycg, 
			     &xc, &yc,
			     &xgl, &ygl, &zgl,
			     &xx, &xy, &yy, &de, &dx, &dy, &dz);


    }
  else // if we don't have the aux info, we have to make the full call
    {
      pp->GetChar(&e, &ecorr, &ecore, &ecorecorr,
 	      &xcg, &ycg, &xcgm, &ycgm,
 	      &xc, &yc, &xgl, &ygl, &zgl,
 	      &xx, &xy, &yy, &chi2, &de, &dx, &dy, &dz);
    }
  

//   cout << __FILE__ << "  " << __LINE__ 
//        << "  " << e << "  " << ecorr 
//        << "  " << ecore << "  " << ecorecorr
//        << "  " << xcg << "  " << ycg 
//        << "  " << xcgm << "  " << ycgm
//        << "  " << xc << "  " << yc 
//        << "  " << xgl << "  " << ygl << "  " << zgl
//        << "  " << xx << "  " << xy << "  " << yy 
//        << "  " << chi2 
//        << "  " << de 
//        << "  " << dx << "  " << dy << "  " << dz << endl;
  

  float e9 = pp->GetE9(hmax->TowerID());
  //  float re9 = (e9 > 0) ? e9 / cluster.GetE9(hmax.ich) : 0.0;
  
  int nh = pp->GetNofHits();
  assert(nh<=HVECTSIZE);
  //  pp->GetHits(fHVect, nh);
  
  float e_max_cl = pp->GetTowerEnergy(hmax->TowerID());
  
  // Principal axis dispersion (eigenvalues).
  float padisp[2];
  float pahelp = (xx + yy) * (xx + yy) - 4.0 * (xx * yy - xy * xy);
  pahelp = sqrt(abs(pahelp));
  padisp[0] = (xx + yy + pahelp) / 2.0;
  padisp[1] = (xx + yy - pahelp) / 2.0;
  
  float vx = fVertex[0]-xgl;
  float vy = fVertex[1]-ygl;
  float vz = fVertex[2]-zgl;
  
  float lactual = sqrt( vx*vx + vy*vy + vz*vz );
  float lnominal = sqrt(xgl * xgl + ygl * ygl + zgl * zgl);
  float dd = lactual - lnominal;
  
  float etof, etofmin, etofmax;
  float tof, tofcorr, dtof, tofmin, tofmax, tofmincorr, tofmaxcorr;
  float tofdisp=0;
  
  ToF_Process(pp->GetTowerVector(),
	      dd, hmax,
	      &tof, &etof, &tofcorr, &dtof,
	      &tofmin, &etofmin, &tofmincorr,
	      &tofmax, &etofmax, &tofmaxcorr,
	      tofdisp);               
  
  size_t id = clustercontainer->size();
  emcClusterContent* clus = clustercontainer->addCluster(id);
  
  clus->set_multiplicity( n_modules );
  clus->set_id(id);
  
  
  //		int is = EmcIndexer::sectorOfflineToOnline(iarm,isector);
  int is = isector;

  clus->set_xyz(xgl,ygl,zgl);
  clus->set_dxyz(dx,dy,dz);
  clus->set_e(e);
  clus->set_e9(e9);

  if (is<6)
    {
      clus->set_ecore(ecorecorr);
    }
  else
    {
      clus->set_ecore(ecorr);
    }
  clus->set_ecent(e_max_cl);
  clus->set_chi2(chi2);
		
  clus->set_tof(tof);
  clus->set_tofcorr(tofcorr);
  clus->set_tofdisp(tofdisp);
  //                  clus->set_dtof(nPeakArea, dtof);
  clus->set_quality(qual);
  clus->set_pid(0);
  clus->set_prob_photon(pp->GetCL());
  //                  clus->set_prob_neuhad(nPeakArea, 0);
  float phi = ( xgl == 0.0 && ygl == 0.0 ? 0.0 : atan2(ygl,xgl) );
  float theta = ( xgl == 0.0 && ygl == 0.0 && zgl == 0.0 ? 0.0 : 
		  atan2(sqrt(xgl*xgl+ygl*ygl),zgl) );
		
  clus->set_theta(theta);
  clus->set_phi(phi);
  //		int iy = hmax.ich / Nx[is];
  //		int iz = hmax.ich - iy * Nx[is];
		
  clus->set_tofmin(tofmin);
  clus->set_etofmin(etofmin);
  clus->set_tofcorrmin(tofmincorr);
  clus->set_tofmax(tofmax);
  clus->set_etofmax(etofmax);
  clus->set_tofcorrmax(tofmaxcorr);
  //                  clus->set_tofmean(0);
  float dispy = yy;
  float dispz = xx;
  clus->set_disp(dispy,dispz);
  clus->set_padisp(padisp[1],padisp[0]);

  if ( clus->has_yz_cg() )
    {
      clus->set_yz_cg(ycg,xcg);
		    
      float corrdispy=-9999;
      float corrdispz=-9999;
		    
      float zModSize = fSectorGeometries[is]->Tower_xSize;
      float yModSize = fSectorGeometries[is]->Tower_ySize;
		    
      computeCorrectedDispersion(ycg,xcg,
				 dispy,dispz,
				 yModSize,zModSize,
				 corrdispy,corrdispz);
		    
      clus->set_corrdisp(corrdispy,corrdispz);
    }
		
  float esum = 0;
  std::vector<emcTowerContent*>::iterator it =  mlist.begin();
  int ih = 0;

  for (; it != mlist.end(); ++it)
    {
      if ( (*it)->Energy() <=0)
	{
	  clus->set_towerid(ih,-1);
	  clus->set_partesum(ih,0);
	  ih++;
	}
      else
	{
	  clus->set_towerid(ih, (*it)->TowerID());
	  esum += (*it)->Energy();
	  clus->set_partesum(ih, esum);
	  ih++;
	}
    }
  
  clus->set_maps(hmax->ErrorNeighbours(),hmax->WarnNeighbours());

  if ( clus->has_rawtdc() )
    {
      clus->set_rawtdc(hmax->TAC());
    }
		
  if ( clus->has_adc() )
    {
      clus->set_adc(hmax->ADC());
    }

  delete pp;
  return 0;
}
