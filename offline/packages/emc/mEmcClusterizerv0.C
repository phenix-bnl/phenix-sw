#include <Fun4AllReturnCodes.h>
#include "mEmcClusterizerv0.h"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>
#include <cassert>
#include <memory>

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include "PHCompositeNode.h"

#include "EmcIndexer.h"
#include "EmcCluster.h"
#include "EmcScSectorRec.h"
#include "EmcGlSectorRec.h"

#include "mEmcGeometryModule.h"

#include "VtxOut.h"
#include "PHGlobal.h"

#include "emcTowerContainer.h"
#include "emcTowerContent.h"
#include "EmcIndexer.h"

#include "emcClusterContainer.h"
#include "emcClusterContent.h"

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

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

}

//_____________________________________________________________________________
mEmcClusterizerv0::mEmcClusterizerv0(mEmcGeometryModule* geom) :
  SubsysReco("mEmcClusterizerv0"),
  fClusters(0)
{
  int nx, ny;
  float txsz, tysz;
  PHMatrix emcrm;
  PHVector emctr;
  float towerThresh = 0.003;
  float peakThresh = 0.08;
  float eClMin = 0.02;
  EmcSectorRec* SectorRec;
  SecGeom* SecGeometry;

  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if ( is < 6 ) 
	{
	  SectorRec = new EmcScSectorRec;
	  fScSector.push_back(SectorRec);
	}
      else
	{
	  SectorRec = new EmcGlSectorRec;
	  fGlSector.push_back(SectorRec);
	}

      // Parmeters for Shower reconstruction
      SectorRec->SetTowerThreshold(towerThresh);
      SectorRec->SetPeakThreshold(peakThresh);
      SectorRec->SetChi2Limit(2);

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

  SetMinClusterEnergy(eClMin);

  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if (is < 6)
        {
          SectorRec = fScSector[is];
        }
      else
        {
          SectorRec = fGlSector[is-6];
        }
      TowerThresh[is] = SectorRec->GetTowerThreshold();
      Nx[is] = SectorRec->GetNx();
    }
  fHVect = new EmcModule[HVECTSIZE];

  displaywarning = true;
}

//_____________________________________________________________________________
mEmcClusterizerv0::~mEmcClusterizerv0()
{
  for ( size_t i = 0; i < fScSector.size(); ++i ) 
    {
      delete fScSector[i];
    }
  for ( size_t i = 0; i < fGlSector.size(); ++i ) 
    {
      delete fGlSector[i];
    }
  for ( size_t i = 0; i < fSectorGeometries.size(); ++i ) 
    {
      delete fSectorGeometries[i];
    }

  delete[] fHVect;
}

//_____________________________________________________________________________
void 
mEmcClusterizerv0::SetTowerThreshold(float Thresh)
{
  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if (is < 6)
        {
          fScSector[is]->SetTowerThreshold(Thresh);
        }
      else
        {
          fGlSector[is - 6]->SetTowerThreshold(Thresh);
        }
    }
}

//_____________________________________________________________________________
void
mEmcClusterizerv0::SetTowerThreshold(int is, float Thresh)
{
  if (is < 0 || is >= MAX_SECTORS_PROCESS)
    {
      cout << PHWHERE << "Wrong sector index:" << is << endl;
      return;
    }
  if (is < 6)
    {
      fScSector[is]->SetTowerThreshold(Thresh);
    }
  else
    {
      fGlSector[is - 6]->SetTowerThreshold(Thresh);
    }
}

//_____________________________________________________________________________
void
mEmcClusterizerv0::SetTowerThresholdPbSc(float Thresh)
{
  for (int is = 0; is < 6; is++)
    {
      fScSector[is]->SetTowerThreshold(Thresh);
    }
}

//_____________________________________________________________________________
void mEmcClusterizerv0::SetTowerThresholdPbGl(float Thresh)
{
  for (int is = 0; is < 2; is++)
    {
      fGlSector[is]->SetTowerThreshold(Thresh);
    }
}

//_____________________________________________________________________________
void 
mEmcClusterizerv0::SetPeakThreshold(float Thresh)
{
  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if (is < 6)
        {
          fScSector[is]->SetPeakThreshold(Thresh);
        }
      else
        {
          fGlSector[is - 6]->SetPeakThreshold(Thresh);
        }
    }
}

//_____________________________________________________________________________
void
mEmcClusterizerv0::SetPeakThreshold(int is, float Thresh)
{
  if (is < 0 || is >= MAX_SECTORS_PROCESS)
    {
      cout << PHWHERE << "Wrong sector index:" << is << endl;
      return;
    }
  if (is < 6)
    {
      fScSector[is]->SetPeakThreshold(Thresh);
    }
  else
    {
      fGlSector[is - 6]->SetPeakThreshold(Thresh);
    }
}

//_____________________________________________________________________________
void
mEmcClusterizerv0::fillHitList(const emcTowerContainer& towers)
{
  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      // Clean up hits list
      HitList[is].erase(HitList[is].begin(), HitList[is].end());

      EmcSectorRec* SectorRec;

      if ( is < 6 ) 
	{
	  SectorRec = fScSector[is];
	}
      else
	{
	  SectorRec = fGlSector[is-6];
	}

      // Update the towerthreshold and nx values.
      TowerThresh[is] = SectorRec->GetTowerThreshold();
      Nx[is] = SectorRec->GetNx();
    }

  for ( size_t i = 0; i < towers.size(); ++i ) 
    {
      const emcTowerContent* t = towers.getTower(i);
      int towerID = t->TowerID();
      int iarm,isector,iy,iz;
      EmcIndexer::TowerLocation(towerID,iarm,isector,iy,iz);

      if ( iarm == 1 )
	{
	  isector = 7 - isector;
	}
  
      float de = t->Energy();

      // skip too low energy towers
      if ( de<= TowerThresh[isector] ) continue;

      float adc=0.0;
      float tac=0.0;

      if ( t->hasDC() ) 
	{
	  adc = t->ADC();
	  tac = t->TDC();
	}

      EmcModule vhit(iy * Nx[isector] + iz,
		     0,
		     de,
		     t->ToF(),
		     t->ErrorNeighbours(),
		     t->WarnNeighbours(),
		     adc,
		     tac);
      
      HitList[isector].push_back(vhit);
    }
}

//_____________________________________________________________________________
void
mEmcClusterizerv0::fillPeakArea(EmcPeakarea& pp, EmcCluster& cluster, 
				const emcTowerContainer& towers,
				int arm, int sector)
{
  EmcModule hmax = pp.GetMaxTower();
  int ndead = pp.GetNDead();
  float qual = ndead ? 1.0 : -ndead;

//   float rmax = (hmax.amp > 0) ? 
//     hmax.amp / cluster.GetTowerEnergy(hmax.ich) : 0;
  
//  EmcModule himp = pp.GetImpactTower();

//   float rimp = (himp.amp > 0) ? 
//     himp.amp / cluster.GetTowerEnergy(himp.ich) : 0;

  float e,ecorr,ecore,ecorecorr;
  float xcg,ycg,xcgm,ycgm;
  float xc,yc,xgl,ygl,zgl;
  float xx,xy,yy;
  float chi2;
  float de,dx,dy,dz;

  pp.GetChar(&e, &ecorr, &ecore, &ecorecorr,
	      &xcg, &ycg, &xcgm, &ycgm,
	      &xc, &yc, &xgl, &ygl, &zgl,
	      &xx, &xy, &yy, &chi2, &de, &dx, &dy, &dz);

  float e9 = pp.GetE9(hmax.ich);
  //  float re9 = (e9 > 0) ? e9 / cluster.GetE9(hmax.ich) : 0.0;

  int nh = pp.GetNofHits();
  assert(nh<=HVECTSIZE);
  pp.GetHits(fHVect, nh);

  float e_max_cl = cluster.GetTowerEnergy(hmax.ich);
  
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
  
  //		      if ( is < 6 )
  // 		      // This is needed because simulated times for the
  // 		      // PbGl do not have their flashtime subtracted
  // 		      if (runno <re1000)
  // 			{
  // 			  dd = sqrt((Vertex[0] - xgl) * (Vertex[0] - xgl) +
  // 				    (Vertex[1] - ygl) * (Vertex[1] - ygl) +
  // 				    (Vertex[2] - zgl) * (Vertex[2] - zgl));
  // 			}

  float etof, etofmin, etofmax;
  float tof, tofcorr, dtof, tofmin, tofmax, tofmincorr, tofmaxcorr;
  float tofdisp=0;

  ToF_Process(fHVect, nh,
	      dd, hmax,
	      &tof, &etof, &tofcorr, &dtof,
	      &tofmin, &etofmin, &tofmincorr,
	      &tofmax, &etofmax, &tofmaxcorr,
	      tofdisp);               
  
  size_t id = fClusters->size();

  emcClusterContent* clus = 
    fClusters->addCluster(id);
  
  clus->set_multiplicity(nh);

  clus->set_id(id);

  int is = EmcIndexer::sectorOfflineToOnline(arm,sector);

  //                  clus->set_method(nPeakArea, 2);
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
  clus->set_prob_photon(pp.GetCL());
  //                  clus->set_prob_neuhad(nPeakArea, 0);
  float phi = ( xgl == 0.0 && ygl == 0.0 ? 0.0 : atan2(ygl,xgl) );
  float theta = ( xgl == 0.0 && ygl == 0.0 && zgl == 0.0 ? 0.0 : 
		  atan2(sqrt(xgl*xgl+ygl*ygl),zgl) );

  clus->set_theta(theta);
  clus->set_phi(phi);
  int iy = hmax.ich / Nx[is];
  int iz = hmax.ich - iy * Nx[is];

  if ( clus->version() < 6 )
    // from v6 these are restored from towerid[0]
    {  
      clus->set_arm(arm);
      clus->set_sector(sector);
      clus->set_ipos(iy,iz);
      
      if (is < 6)
	clus->set_type(1); // PbSc
      else
	clus->set_type(2); // PbGl
    }

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
  for (int ih = 0; ih < nh; ++ih)
    {
      if (fHVect[ih].amp <= 0)
	{
	  clus->set_towerid(ih,-1);
	  clus->set_partesum(ih,0);
	  
	}
      else
	{
	  int ich = fHVect[ih].ich;
	  int iy = ich / Nx[is];
	  int iz = ich - iy * Nx[is];
	  int swkey = iz + iy * 100 + 10000 * sector 
	    + 100000 * arm;
	  int towerid = EmcIndexer::TowerID(swkey);
	  clus->set_towerid(ih, towerid);
	  esum += fHVect[ih].amp;
	  clus->set_partesum(ih, esum);
	}
    }
  
  clus->set_maps(hmax.deadmap,hmax.warnmap);

  if ( clus->has_rawtdc() )
    {
      clus->set_rawtdc(hmax.tac);
    }

  if ( clus->has_adc() )
    {
      clus->set_adc(hmax.adc);
    }

  if ( clus->has_amutac() )
    {
      // We need the TowerContainer just because 
      // of amutac field that's new in ClusterContentv6.
      // Relying on hmax tower being the 0th in cluster
      emcTowerContent* t = towers.findTower( clus->towerid(0) );
      clus->set_amutac( t->AMUTAC() );
    }

}

//_____________________________________________________________________________
int
mEmcClusterizerv0::process_event(PHCompositeNode *root)
{
  PHNodeIterator it(root);

  // OK. Te be able to work, we need some objects :
  // input : VtxOut and emcTowerContainer
  // output : emcClusterContainer
  //
  PHIODataNode<PHObject>* emcTowerContainerNode = 
    (PHIODataNode<PHObject>*)it.findFirst("PHIODataNode","emcTowerContainer");
  if ( !emcTowerContainerNode ) 
    {
      cerr << __FILE__ << ":" << __LINE__ << " cannot find emcTowerContainer "
	   << "node." << endl;
      return ABORTRUN;
    }
  emcTowerContainer* towers = 
    static_cast<emcTowerContainer*>(emcTowerContainerNode->getData());
  if (!towers)
    {
      cerr << __FILE__ << ":" << __LINE__ << " cannot find emcTowerContainer "
	   << "object." << endl;
      return ABORTRUN;
    }

  if ( !towers->isValid() ) return ABORTRUN;

  PHIODataNode<PHObject>* emcClusterContainerNode =
    (PHIODataNode<PHObject>*)it.findFirst("PHIODataNode",
					  "emcClusterContainer");
  if ( !emcClusterContainerNode ) 
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " cannot find emcClusterContainer node." << endl;
      return ABORTRUN;
    }

  fClusters = 
    static_cast<emcClusterContainer*>(emcClusterContainerNode->getData());
  if (!fClusters)
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " cannot find emcClusterContainer object." << endl;
      return ABORTRUN;
    }
  else
    {
      fClusters->Reset();
    }

  PHIODataNode<PHObject>* vtxoutnode =
    (PHIODataNode<PHObject>*)it.findFirst("PHIODataNode", "VtxOut");
  
  VtxOut* vtxout = 0;

  if ( vtxoutnode )
    {
      vtxout = static_cast<VtxOut*>(vtxoutnode->getData());
    }

  fVertex.resize(3,0);

  // Try to extract vertex from vtxout
  if ( vtxout ) 
    {
      PHPoint vertex = vtxout->get_Vertex();
      fVertex[0] = vertex.getX();
      fVertex[1] = vertex.getY();
      fVertex[2] = vertex.getZ();
    }
  else  
    {
      // On failure, try to get zvtx from global node. 
    
      PHIODataNode<PHObject>* globalnode =
	(PHIODataNode<PHObject>*)it.findFirst("PHIODataNode", "PHGlobal");
      
      PHGlobal* global = 0;
      if ( globalnode ) 
	global = static_cast<PHGlobal*>(globalnode->getData());
      
      if ( global ) {
	if (displaywarning) {
	cout << PHWHERE << " WARNING: ZVertex taken from (global). "
	     << "Assuming (0,0,bbczvtx). "
	     << "- This message will show only once." << endl;
	displaywarning = false;
      }
	fVertex[0] = 0.0;
	fVertex[1] = 0.0;
	fVertex[2] = global->getBbcZVertex();
      }
      else {
	// Complain on failure of both
	cout << PHWHERE << " No information (vtxout) about vertex. Assuming "
	     << "(0,0,0)" << endl;
	fVertex[0] = 0.0;
	fVertex[1] = 0.0;
	fVertex[2] = 0.0;
      }
      
    }

  
  if (!(fabs(fVertex[0]) < 10.0 &&
        fabs(fVertex[1]) < 10.0 &&
        fabs(fVertex[2]) < 100.0))
    {
      // Reset vertex to 0 and do clustering in case of nonvalid/bad vertex
      for ( int i = 0; i < 3; ++i )
        {
          fVertex[i] = 0.0;
        }
      //      cout << PHWHERE << "No clustering done, bad Vertex=("
      //	   << fVertex[0] << ", "
      //	   << fVertex[1] << ", "
      //	   << fVertex[2] << ")"
      //	   << endl;
      //      return ABORTRUN;
    }

  // Fine. We get everything we need. Let's continue.

  // Fill the internal hit list from towers read from the node tree.
  fillHitList(*towers);

  EmcPeakarea pPList[MAX_NUMBER_OF_PEAKS];
  EmcModule peaks[MAX_NUMBER_OF_PEAKS];

  //==========> Start looping over sectors <========== 
  for ( int is = 0; is < MAX_SECTORS_PROCESS; ++is )
    {
      EmcSectorRec* SectorRec = (is < 6) ? fScSector[is] : fGlSector[is - 6];
      int arm, sector;

      if (is <= 3)
        {
          arm = 0;
          sector = is;
        }
      else
        {
          arm = 1;
          sector = 7 - is;
        }

      float MinClusterEnergy = (is < 6) ? fMinClusterEnergySc : 
	fMinClusterEnergyGl;

      SectorRec->SetVertex(&fVertex[0]);
      SectorRec->SetModules(&HitList[is]);
      int nCl = SectorRec->FindClusters();
      vector<EmcCluster>* ClusterList = SectorRec->GetClusters();

      if (nCl < 0)
        {
          cerr << " ?????? " << Name() << ": Increase parameter MaxLen !!!"
	       << endl;
          return ABORTRUN;
        }

      // Fill cluster table
      // Start looping on clusters

      vector<EmcCluster>::iterator pc;

      for ( pc = ClusterList->begin(); pc != ClusterList->end(); ++pc )
        {
          int npk = (*pc).GetPeaks(pPList, peaks);
	  EmcPeakarea* pp = pPList;
	  assert(pp!=0);

          for ( int ip = 0; ip < npk; ++ip )
            { 
              if (pp->GetTotalEnergy() > MinClusterEnergy) 
		{
		  fillPeakArea(*pp,(*pc),*towers, arm, sector);
		}
	      pp++;
	    } // end of loop over peakareas of cluster (*pc)
	} // end loop over clusters of sector is
    } // end of loop over sectors

  return EVENT_OK;
}

void
mEmcClusterizerv0::ToF_Process(EmcModule* phit, size_t nhits,
			       float dist, EmcModule& hmax,
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
  EmcModule* ph;

  tof = hmax.tof;
  etof = hmax.amp;

  tofmax = 0;
  tofmin = 999;
  etofmax = 0;
  etofmin = 0;
  ph = phit;

  float tofsum=0;
  float tofsum2=0;
  int nsum = 0;

  for ( size_t i = 0; i < nhits; i++)
    {
      if (ph->amp > 0)
        {
          if (ph->tof > tofmax)
            {
              tofmax = ph->tof;
              etofmax = ph->amp;
            }
          if (ph->tof < tofmin)
            {
              tofmin = ph->tof;
              etofmin = ph->amp;
            }
	  tofsum += ph->tof;
	  tofsum2 += ph->tof * ph->tof;
	  ++nsum;
        }
      ph++;
    }

  if ( nsum > 1 )
    {
      float tofmean = tofsum/nsum;
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
