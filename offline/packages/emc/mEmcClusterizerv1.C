#include "mEmcClusterizerv1.h"

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
#include "Fun4AllReturnCodes.h"
#include "PHCompositeNode.h"

#include "getClass.h"

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
mEmcClusterizerv1::mEmcClusterizerv1(mEmcGeometryModule* geom)
  : mEmcClusterizerv0(geom)
{

  //  std::cout << __FILE__ << "  " << __LINE__ << "  " << " in mEmcClusterizerv1::mEmcClusterizerv1" << std::endl;
  emc_cluster_id = 0;
  towers_per_event = 0;
  
  int nx, ny;
  float txsz, tysz;
  PHMatrix emcrm;
  PHVector emctr;
  float towerThresh = 0.003;
  float peakThresh = 0.08;
  float eClMin = 0.02;
  EmcSectorRec* SectorRec;
  SecGeom* SecGeometry;
  fHit = 0;
  fAuxInfoContainer = 0;
  fClusters = 0;

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
void
mEmcClusterizerv1::fillPeakArea(EmcPeakarea& pp, EmcCluster& cluster, 
				const emcTowerContainer& towers,
				int arm, int sector)
{
  //  std::cout << __FILE__ << "  " << __LINE__ << "  " << " in mEmcClusterizerv1" << std::endl;
  EmcModule hmax = pp.GetMaxTower();

  int numberofhits = pp.GetNofHits();
  assert(numberofhits<=HVECTSIZE);
  pp.GetHits(fHVect, numberofhits);

  //  float e_max_cl = cluster.GetTowerEnergy(hmax.ich);
   
   // this is the new cluster counter
  int id = emc_cluster_id++;

  // now loop over the towers of this "peakarea"

  int is = EmcIndexer::sectorOfflineToOnline(arm,sector);

  //  cout << __FILE__ << "  " << __LINE__ << " --- cluster nr " << id << "number of hits " << numberofhits << endl;

 
  for (int ih = 0; ih < numberofhits; ++ih)
    {
      

      emcTowerContentDST *ntwr; 

      if (ih == 0)
	{
 
	  // we call this monster since this is exactly what we'd call later,
	  // and we want to do the exact same thing.
	  float e,ecorr,ecore,ecorecorr;
	  float xcg,ycg,xcgm,ycgm;
	  float xc,yc,xgl,ygl,zgl;
	  float xx,xy,yy;
	  float chi2;
	  float de,dx,dy,dz;
	  
	  pp.GetChar(&e, &ecorr, 
		     &ecore, &ecorecorr,
		     &xcg, &ycg, 
		     &xcgm, &ycgm,
		     &xc, &yc, 
		     &xgl, &ygl, &zgl,
		     &xx, &xy, &yy, 
		     &chi2, 
		     &de, 
		     &dx, &dy, &dz);
	  
// 	  cout << __FILE__ << "  " << __LINE__ << "       --- tower nr " << ih 
// 	       << "  " << e << "  " << ecorr 
// 	       << "  " << ecore << "  " << ecorecorr
// 	       << "  " << xcg << "  " << ycg 
// 	       << "  " << xcgm << "  " << ycgm
// 	       << "  " << xc << "  " << yc 
// 	       << "  " << xgl << "  " << ygl << "  " << zgl
// 	       << "  " << xx << "  " << xy << "  " << yy 
// 	       << "  " << chi2 
// 	       << "  " << de 
// 	       << "  " << dx << "  " << dy << "  " << dz << endl;

	  // if we have tower number 0; we add the auxiliary info
	  if (  fAuxInfoContainer->addInfo(id, chi2, ecore, xcgm, ycgm) )
	    {
	      cout << __FILE__ << "  " << __LINE__ << " Error: Could not add AuxInfo " <<  endl;
	    }
	  
	}
 
      ntwr = new emcTowerContentDST();      

      ntwr->SetClusterid (id);

      if (fHVect[ih].amp <= 0)
	{
	  ntwr->SetTowerID(-1);
	}
      else
	{
	  int ich = fHVect[ih].ich;
	  int iy = ich / Nx[is];
	  int iz = ich - iy * Nx[is];
	  int swkey = iz + iy * 100 + 10000 * sector 
	    + 100000 * arm;
	  int towerid = EmcIndexer::TowerID(swkey);

	  ntwr->SetTowerID(towerid);
	  ntwr->SetCalibrated(fHVect[ih].amp, fHVect[ih].tof);
	  ntwr->SetNeighbours (fHVect[ih].deadmap,fHVect[ih].warnmap);
	  ntwr->SetADCTDC(fHVect[ih].adc, fHVect[ih].tac);

// 	  cout << __FILE__ << "  " << __LINE__ << "       --- tower nr " << ih 
// 	       << "  towerid = " << towerid 
// 	       << "  adc = " << fHVect[ih].adc
// 	       << "  clusterid = " << id << endl;
//	  ntwr->print();
	  
	}

      // or the standard tower goes to the standard container
      fHit->addTower(towers_per_event++,*ntwr);      
      delete ntwr;
    }


}

//_____________________________________________________________________________
int 
mEmcClusterizerv1::process_event(PHCompositeNode *root)
{

  //  cout << __FILE__ << "  " << __LINE__ << " in event method " << endl;

  emc_cluster_id = 0;
  towers_per_event = 0;


  // OK. Te be able to work, we need some objects :
  // input : VtxOut and emcTowerContainer
  // output : emcClusterContainer
  //


  emcTowerContainer* towers = findNode::getClass<emcTowerContainer>( root, "emcTowerContainer");

  if (!towers)
    {
      cerr << __FILE__ << ":" << __LINE__ << " cannot find emcTowerContainer "
	   << "object." << endl;
      return ABORTRUN;
    }

  if ( !towers->isValid() ) 
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " emcTowerContainer not valid" << endl;
      return ABORTRUN;
    }

  fHit = findNode::getClass<emcTowerContainerDST>( root, "emcHitContainer");

  if (!fHit)
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " cannot find emcHitContainer object." << endl;
      return ABORTRUN;
    }
  else
    {
      fHit->Reset();
    }

  // now we find the container for the 0th tower
 
  fAuxInfoContainer = findNode::getClass<emcClusterAuxInfoContainerV1>( root, "emcClusterAuxInfo");

  if (!fAuxInfoContainer)
    {
      cerr << __FILE__ << ":" << __LINE__ 
	   << " cannot find emcClusterAuxInfo object." << endl;
      return ABORTRUN;
    }
  else
    {
      fAuxInfoContainer ->Reset();
    }

  VtxOut* vtxout = findNode::getClass<VtxOut>(root, "VtxOut");

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
      
      PHGlobal* global = findNode::getClass<PHGlobal>(root, "PHGlobal");
      
      if ( global ) 
	{
	  if (displaywarning) 
	    {
	      cout << PHWHERE << " WARNING: ZVertex taken from (global). "
		   << "Assuming (0,0,bbczvtx). "
		   << "- This message will show only once." << endl;
	      displaywarning = false;
	    }
	  fVertex[0] = 0.0;
	  fVertex[1] = 0.0;
	  fVertex[2] = global->getBbcZVertex();
	}
      else 
	{
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
 
  for ( int i = 0; i < 3; i++ )
    {
      //      cout << PHWHERE << " Vertex " << i << "  " << fVertex[i] << endl;
      fHit->setVertex(i,fVertex[i]);
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
          cerr << " ??????  : Increase parameter MaxLen !!!"
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

