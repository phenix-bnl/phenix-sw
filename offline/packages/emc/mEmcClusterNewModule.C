// EMC basic clustering class (for PHOOL).
// Alexander Bazilevsky Sep-00

#include <Fun4AllReturnCodes.h>
#include "mEmcClusterNewModule.h"
#include "PHCompositeNode.h"
#include "mEmcGeometryModule.h"
#include "EmcCluster.h"

#include "VtxOut.h"
#include "dEmcCalibTowerWrapper.h"
#include "dEmcClusterLocalExtWrapper.h"

#include "PHNode.h"
#include "PHCompositeNode.h"
#include "PHIODataNode.h"
#include "PHNodeIterator.h"
#include "PHTable.hh"

#include <cstdio>
#include <cmath>
#include <cstdlib>
#include <iostream>

using namespace std;

typedef PHIODataNode<PHTable> TableNode_t;

#define MAX_SECTORS_PROCESS 8
#define MaxNofPeaks 10
#define HITS_TO_TABLE   16

mEmcClusterNewModule::mEmcClusterNewModule(mEmcGeometryModule* geom,
					   int runnumber):
  SubsysReco("mEmcClusterNewModule")
{
  int nx, ny;
  float txsz, tysz;
  PHMatrix emcrm;
  PHVector emctr;
  float TowerThresh = 0.003;
  float PeakThresh = 0.08;
  float eClMin = 0.02;
  EmcSectorRec *SectorRec;
  SecGeom SecGeometry;

  fRunNumber = runnumber;

  for (int is = 0; is < MAX_SECTORS_PROCESS; ++is)
    {
      SectorRec = (is < 6) ? (EmcSectorRec *)&ScSector[is] : (EmcSectorRec *)&GlSector[is - 6];

      // Parmeters for Shower reconstruction
      SectorRec->SetTowerThreshold(TowerThresh);
      SectorRec->SetPeakThreshold(PeakThresh);
      SectorRec->SetChi2Limit(2);

      geom->GetSectorDim(is, nx, ny);
      geom->GetTowerSize(is, txsz, tysz);
      geom->GetMatrixVector(is, emcrm, emctr);

      SecGeometry.nx = nx;
      SecGeometry.ny = ny;
      SecGeometry.Tower_xSize = txsz;
      SecGeometry.Tower_ySize = tysz;

      SectorRec->SetGeometry(SecGeometry, &emcrm, &emctr);
    }

  SetMinClusterEnergy(eClMin);
}

void 
mEmcClusterNewModule::SetTowerThreshold(float Thresh)
{
  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if (is < 6)
        {
          ScSector[is].SetTowerThreshold(Thresh);
        }
      else
        {
          GlSector[is - 6].SetTowerThreshold(Thresh);
        }
    }
}

void
mEmcClusterNewModule::SetTowerThreshold(int is, float Thresh)
{
  if (is < 0 || is >= MAX_SECTORS_PROCESS)
    {
      cout << PHWHERE << "Wrong sector index:" << is << endl;
      return;
    }
  if (is < 6)
    {
      ScSector[is].SetTowerThreshold(Thresh);
    }
  else
    {
      GlSector[is - 6].SetTowerThreshold(Thresh);
    }
}

void
mEmcClusterNewModule::SetTowerThresholdPbSc(float Thresh)
{
  for (int is = 0; is < 6; is++)
    {
      ScSector[is].SetTowerThreshold(Thresh);
    }
}

void mEmcClusterNewModule::SetTowerThresholdPbGl(float Thresh)
{
  for (int is = 0; is < 2; is++)
    {
      GlSector[is].SetTowerThreshold(Thresh);
    }
}

void 
mEmcClusterNewModule::SetPeakThreshold(float Thresh)
{
  for (int is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if (is < 6)
        {
          ScSector[is].SetPeakThreshold(Thresh);
        }
      else
        {
          GlSector[is - 6].SetPeakThreshold(Thresh);
        }
    }
}

void
mEmcClusterNewModule::SetPeakThreshold(int is, float Thresh)
{
  if (is < 0 || is >= MAX_SECTORS_PROCESS)
    {
      cout << PHWHERE << "Wrong sector index:" << is << endl;
      return;
    }
  if (is < 6)
    {
      ScSector[is].SetPeakThreshold(Thresh);
    }
  else
    {
      GlSector[is - 6].SetPeakThreshold(Thresh);
    }
}

int
mEmcClusterNewModule::process_event(PHCompositeNode *root)
{
  PHNodeIterator it(root);
  float lactual, lnominal;
  float Vertex[3] = {0, 0, 0};
  vector<EmcModule> HitList[MAX_SECTORS_PROCESS];
  EmcModule vhit;
  EmcModule HVect[HITS_TO_TABLE];
  vector<EmcCluster> *ClusterList;
  vector<EmcCluster>::iterator pc;
  EmcPeakarea pPList[MaxNofPeaks];
  EmcPeakarea *pp;
  EmcModule peaks[MaxNofPeaks];
  int i, iy, iz, is, ip, ih, ich, ind;
  float TowerThresh[MAX_SECTORS_PROCESS];
  int Nx[MAX_SECTORS_PROCESS];
  float MinClusterEnergy;
  EmcSectorRec *SectorRec;
  int nCluster, nPeakArea;
  int nCl, nh, npk;
  int arm, sector;
  EmcModule hmax, himp;
  float de, /*rmax, rimp,*/ e, e9, re9, chi2, pr;
  float ecore, ecorr, ecorecorr, xcg, ycg, xcgm, ycgm;
  float xx, xy, yy, xc, yc, xgl, ygl, zgl, tof, tofcorr, etof;
  float dtof, tofmin, tofmincorr, etofmin, tofmax, tofmaxcorr;
  float etofmax, dd, dx, dy, dz, e_max_cl;
  int dead, ndead, warnmap;
  float qual;
  int evno = -9999;

  float padisp[2], pahelp;


  // dEmcCalibTower
  PHIODataNode<PHTable>* dEmcCalibTowerNode =
    (PHIODataNode<PHTable>*)it.findFirst("PHIODataNode", "dEmcCalibTower");
  dEmcCalibTowerWrapper * dEmcCalibTower =
    static_cast<dEmcCalibTowerWrapper*>(dEmcCalibTowerNode->getData());
  TABLE_HEAD_ST dEmcCalibTower_h = dEmcCalibTower->TableHeader();

  PHIODataNode<PHTable>* dEmcClusterLocalExtNode =
    (PHIODataNode<PHTable>*)it.findFirst("PHIODataNode", "dEmcClusterLocalExt");
  dEmcClusterLocalExtWrapper * dEmcClusterLocalExt =
    static_cast<dEmcClusterLocalExtWrapper*>(dEmcClusterLocalExtNode->getData());
  dEmcClusterLocalExt->SetRowCount(0);

  PHIODataNode<PHObject>* vtxoutnode =
    (PHIODataNode<PHObject>*)it.findFirst("PHIODataNode", "VtxOut");

  VtxOut* vtxout = NULL;
  if (vtxoutnode)
    {
      vtxout = static_cast<VtxOut*>(vtxoutnode->getData());
    }

  if (!vtxoutnode || !vtxout)
    {
      cout << PHWHERE << " No information (vtxout) about vertex. Assuming "
	   << "(0,0,0)" << endl;

      for (i = 0; i < 3; i++)
        {
          Vertex[i] = 0.0;
        }
    }
  else
    {
      PHPoint vertex = vtxout->get_Vertex();
      Vertex[0] = vertex.getX();
      Vertex[1] = vertex.getY();
      Vertex[2] = vertex.getZ();
    }
  
  if (fRunNumber == 0)
    {
      fRunNumber = 27000;
    }

  if (!(fabs(Vertex[0]) < 10.0 &&
        fabs(Vertex[1]) < 10.0 &&
        fabs(Vertex[2]) < 100.0))
    {
      cout << PHWHERE << "No clustering done, bad Vertex=("
	   << Vertex[0] << ", "
	   << Vertex[1] << ", "
	   << Vertex[2] << ")"
	   << endl;
      return ABORTRUN;
    }

  //=========> Filling Hit List <==========
  for (is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      if (is < 6)
        {
          SectorRec = &(ScSector[is]);
        }
      else
        {
          SectorRec = &(GlSector[is - 6]);
        }
      TowerThresh[is] = SectorRec->GetTowerThreshold();
      Nx[is] = SectorRec->GetNx();
    }
  for (is = 0; is < MAX_SECTORS_PROCESS; is++)
    {
      // Clean up hits list
      HitList[is].erase(HitList[is].begin(), HitList[is].end());
    }
  if (dEmcCalibTower_h.nok <= 0)
    {
      return EVENT_OK;
    }
  for (i = 0; i < dEmcCalibTower_h.nok; i++)
    {
      // Start looping on fired modules
      iz = dEmcCalibTower->get_ind(0, i);
      iy = dEmcCalibTower->get_ind(1, i);
      is = dEmcCalibTower->get_sector(i);

      // If it's in the East Arm
      if (dEmcCalibTower->get_arm(i) == 1)
        {
          is = 7 - is;
        }
      if (is < 0 || is >= MAX_SECTORS_PROCESS)
        {
          printf(" ?????? EmcClusterLocalChi2: Sector number = %d\n", is);
          return ABORTRUN;
        }

      de = dEmcCalibTower->get_ecal(i);
      tof = dEmcCalibTower->get_tof(i);
      dead = dEmcCalibTower->get_deadmap(i);
      warnmap = dEmcCalibTower->get_warnmap(i); // MV 2001/12/06

      vhit.ich = iy * Nx[is] + iz;
      vhit.amp = de;
      vhit.tof = tof;
      vhit.deadmap = dead;
      vhit.warnmap = warnmap; // MV 2001/12/06

      // MV 2001/09/25
      vhit.adc = dEmcCalibTower->get_adc(i);
      vhit.tac = dEmcCalibTower->get_tac(i);

      if (de > TowerThresh[is])
        {
          HitList[is].push_back(vhit);
        }
    } // Finished looping on fired modules

  nCluster = 0;
  nPeakArea = 0;

  //==========> Start looping over sectors <==========
  for (is = 0; is < MAX_SECTORS_PROCESS; ++is)
    {
      SectorRec = (is < 6) ? (EmcSectorRec *)&ScSector[is] : (EmcSectorRec *)&GlSector[is - 6];
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

      MinClusterEnergy = (is < 6) ? fMinClusterEnergySc : fMinClusterEnergyGl;

      SectorRec->SetVertex(Vertex);
      SectorRec->SetModules(&HitList[is]);
      nCl = SectorRec->FindClusters();
      ClusterList = SectorRec->GetClusters();

      if (nCl < 0)
        {
          printf(" ?????? EmcClusterChi2: Increase parameter MaxLen !!!\n");
          return ABORTRUN;
        }
      // Fill cluster table 
      // Start looping on clusters
      for (pc = ClusterList->begin(); pc != ClusterList->end(); pc++)
        {
          npk = pc->GetPeaks(pPList, peaks);
          pp = pPList;

          // Fill PeakArea table 
          for (ip = 0; ip < npk; ip++)
            { 
	      // Start looping over peak areas
              if (pp->GetTotalEnergy() > MinClusterEnergy)
                {
                  nh = pp->GetNofHits();
                  hmax = pp->GetMaxTower();
                  ndead = pp->GetNDead();
		  qual = ndead ? 1.0 : -ndead;
                  /* rmax = (hmax.amp > 0) ? 
		     hmax.amp / pc->GetTowerEnergy(hmax.ich) : 0; -- flagged by scan-build as dead assignment */
                  himp = pp->GetImpactTower();
		  /* rimp = (himp.amp > 0) ? 
		     himp.amp / pc->GetTowerEnergy(himp.ich) : 0;  -- flagged by scan-build as dead assignment */
		  pp->GetChar(&e, &ecorr, &ecore, &ecorecorr,
                              &xcg, &ycg, &xcgm, &ycgm,
                              &xc, &yc, &xgl, &ygl, &zgl,
                              &xx, &xy, &yy, &chi2, &de, &dx, &dy, &dz);
                  e9 = pp->GetE9(hmax.ich);
		  re9 = (e9 > 0) ? e9 / pc->GetE9(hmax.ich) : 0.0;
                  pp->GetHits(HVect, HITS_TO_TABLE);
                  e_max_cl = pc->GetTowerEnergy(hmax.ich);

                  // Principal axis dispersion (eigenvalues)  Nov 17, 2001, GD
                  padisp[0] = 2.0;
                  padisp[1] = 1.0;
                  pahelp = (xx + yy) * (xx + yy) - 4.0 * (xx * yy - xy * xy);
                  pahelp = sqrt(abs(pahelp));
                  padisp[0] = (xx + yy + pahelp) / 2.0;
                  padisp[1] = (xx + yy - pahelp) / 2.0;
                  if (is < 6)
                    {
                      if (fRunNumber < 20000)
                        {
                          dd = sqrt((Vertex[0] - xgl) * (Vertex[0] - xgl) +
                                    (Vertex[1] - ygl) * (Vertex[1] - ygl) +
                                    (Vertex[2] - zgl) * (Vertex[2] - zgl));
                        }
                      else
                        {
                          //  PbSc now also uses physics-extracted nominal T0
                          //  w.r.t. vertex (0,0,0).  However, this is only
                          //  valid for Year-2 data analysis
			  lactual = sqrt(pow(Vertex[0] - xgl, 2) +
					 pow(Vertex[1] - ygl, 2) +
					 pow(Vertex[2] - zgl, 2));
			  lnominal = sqrt(xgl * xgl + ygl * ygl + zgl * zgl);
                          dd = lactual - lnominal;
                        }
                    }
                  else
		    {
		      // This is needed because simulated times for the
		      // PbGl do not have their flashtime subtracted
		      if (fRunNumber < 1000)
			{
			  dd = sqrt((Vertex[0] - xgl) * (Vertex[0] - xgl) +
				    (Vertex[1] - ygl) * (Vertex[1] - ygl) +
				    (Vertex[2] - zgl) * (Vertex[2] - zgl));
			}
		      else
			{
			  // MV 2001/09/18 for PbGl nominal T0 is taken
			  // into account during calibration.  We need
			  // just correct for vertex deviation from
			  // (0,0,0)
			  lactual = sqrt(pow(Vertex[0] - xgl, 2) +
					 pow(Vertex[1] - ygl, 2) +
					 // pow(Vertex[1] - ygl, 2));
					 pow(Vertex[2] - zgl, 2));
			  lnominal = sqrt(xgl * xgl + ygl * ygl + zgl * zgl);
			  dd = lactual - lnominal;
			}
		    }
	      
                  ToF_Process(HVect, dd, hmax,
                              &tof, &etof, &tofcorr, &dtof,
                              &tofmin, &etofmin, &tofmincorr,
                              &tofmax, &etofmax, &tofmaxcorr);

                
                  pr = pp->GetCL();
                 
                  dEmcClusterLocalExt->set_id(nPeakArea, nPeakArea);
                  dEmcClusterLocalExt->set_runno(nPeakArea, fRunNumber);
                  dEmcClusterLocalExt->set_evno(nPeakArea, evno);
                  dEmcClusterLocalExt->set_arm(nPeakArea, arm);
                  dEmcClusterLocalExt->set_sector(nPeakArea, sector);
                  if (is < 6)
                    {
                      dEmcClusterLocalExt->set_type(nPeakArea, 1);
                    }
                  else
                    {
                      dEmcClusterLocalExt->set_type(nPeakArea, 2);
                    }
                  dEmcClusterLocalExt->set_method(nPeakArea, 2);
                  dEmcClusterLocalExt->set_xyz(0, nPeakArea, xgl);
                  dEmcClusterLocalExt->set_xyz(1, nPeakArea, ygl);
                  dEmcClusterLocalExt->set_xyz(2, nPeakArea, zgl);
                  dEmcClusterLocalExt->set_dxyz(0, nPeakArea, dx);
                  dEmcClusterLocalExt->set_dxyz(1, nPeakArea, dy);
                  dEmcClusterLocalExt->set_dxyz(2, nPeakArea, dz);
                  dEmcClusterLocalExt->set_e(nPeakArea, e);
                  dEmcClusterLocalExt->set_ecore(nPeakArea, ecorecorr);
                  dEmcClusterLocalExt->set_ecorr(nPeakArea, ecorr);
                  dEmcClusterLocalExt->set_de(nPeakArea, de);
                  dEmcClusterLocalExt->set_ecent(nPeakArea, e_max_cl);
                  dEmcClusterLocalExt->set_chi2(nPeakArea, chi2);
                  dEmcClusterLocalExt->set_tof(nPeakArea, tof);
                  dEmcClusterLocalExt->set_tofcorr(nPeakArea, tofcorr);
                  dEmcClusterLocalExt->set_dtof(nPeakArea, dtof);
                  dEmcClusterLocalExt->set_qual(nPeakArea, qual);
                  dEmcClusterLocalExt->set_pid(nPeakArea, 0);
                  dEmcClusterLocalExt->set_prob_photon(nPeakArea, pr);
                  dEmcClusterLocalExt->set_prob_neuhad(nPeakArea, 0);
                  dEmcClusterLocalExt->set_theta(nPeakArea, 0);
                  dEmcClusterLocalExt->set_phi(nPeakArea, 0);
                  dEmcClusterLocalExt->set_unitv(0, nPeakArea, 0);
                  dEmcClusterLocalExt->set_unitv(1, nPeakArea, 0);
                  dEmcClusterLocalExt->set_unitv(2, nPeakArea, 0);
                  iy = hmax.ich / Nx[is];
                  iz = hmax.ich - iy * Nx[is];
                  dEmcClusterLocalExt->set_ind(0, nPeakArea, iz);
                  dEmcClusterLocalExt->set_ind(1, nPeakArea, iy);
                  dEmcClusterLocalExt->set_twrhit(nPeakArea, nh);
                  dEmcClusterLocalExt->set_tofmin(nPeakArea, tofmin);
                  dEmcClusterLocalExt->set_etofmin(nPeakArea, etofmin);
                  dEmcClusterLocalExt->set_tofmincorr(nPeakArea, tofmincorr);
                  dEmcClusterLocalExt->set_tofmax(nPeakArea, tofmax);
                  dEmcClusterLocalExt->set_etofmax(nPeakArea, etofmax);
                  dEmcClusterLocalExt->set_tofmaxcorr(nPeakArea, tofmaxcorr);
                  dEmcClusterLocalExt->set_tofmean(nPeakArea, 0);
                  dEmcClusterLocalExt->set_disp(0, nPeakArea, xx);
                  dEmcClusterLocalExt->set_disp(1, nPeakArea, yy);
                  dEmcClusterLocalExt->set_padisp(0, nPeakArea, padisp[0]);
                  dEmcClusterLocalExt->set_padisp(1, nPeakArea, padisp[1]);

                  dEmcClusterLocalExt->set_yz_cg(0, nPeakArea, xcg);
                  dEmcClusterLocalExt->set_yz_cg(1, nPeakArea, ycg);

                  float esum = 0;
                  for (ih = 0; ih < HITS_TO_TABLE; ih++)
                    {
                      if (HVect[ih].amp <= 0)
                        {
                          dEmcClusterLocalExt->set_twrlist(ih, nPeakArea, 0);
                          dEmcClusterLocalExt->set_partesum(ih, nPeakArea, 0);

                        }
                      else
                        {
                          ich = HVect[ih].ich;
                          iy = ich / Nx[is];
                          iz = ich - iy * Nx[is];
                          ind = iz + iy * 100 + 10000 * sector + 100000 * arm;
                          dEmcClusterLocalExt->set_twrlist(ih, nPeakArea, ind);
                          esum += HVect[ih].amp;
                          dEmcClusterLocalExt->set_partesum(ih, nPeakArea, 
							    esum);
                        }
                    }

                  dEmcClusterLocalExt->set_e9(nPeakArea, e9);
                  dEmcClusterLocalExt->set_re9(nPeakArea, re9);

                  // MV 2001/09/25
                  // as chi2_sh and prob_photon_sh fields are not used,
                  // we can use them to store ADC and TAC for the cluster
                  dEmcClusterLocalExt->set_chi2_sh(nPeakArea, hmax.adc);
                  dEmcClusterLocalExt->set_prob_photon_sh(nPeakArea, hmax.tac);

                  // MV 2001/12/06
                  dEmcClusterLocalExt->set_deadmap(nPeakArea, hmax.deadmap);
                  dEmcClusterLocalExt->set_warnmap(nPeakArea, hmax.warnmap);

                  nPeakArea++;
                } // if e>eClustMin
              pp++;
            } // Finished looping over peak areas
          nCluster++;
        } // Finished looping over clusters
    } // is

  //==========> Finished looping over sectors <==========
  dEmcClusterLocalExt->SetRowCount(nPeakArea);

  return EVENT_OK;
}

void
mEmcClusterNewModule::ToF_Process(EmcModule* phit, float dist, EmcModule& hmax,
                                  float* ptof, float* petof, float* ptofcorr,
                                  float* pdtof,
                                  float* ptofmin, float* petofmin, 
				  float* ptofmincorr,
                                  float* ptofmax, float* petofmax, 
				  float* ptofmaxcorr)
{
  int i;
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

  for (i = 0; i < HITS_TO_TABLE; i++)
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
        }
      ph++;
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
