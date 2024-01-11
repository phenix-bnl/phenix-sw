#include <Fun4AllReturnCodes.h>
#include <recoConsts.h>
#include "mEmcGeaMakeCalibTower.h"

#include <cassert>
#include <cmath> 
#include <iostream>

#include "dEmcEventWrapper.h"
#include "dEmcGeometryWrapper.h"
#include "dEmcRawDataWrapper.h"

#include "EmcIndexer.h"
#include "emcTowerContainer.h"
#include "emcTowerContent.h"

#include "mEmcGeometryModule.h"

#include "emcNodeHelper.h"

#include "VtxOut.h"
#include "PHPoint.h"
#include "gsl/gsl_const_mks.h"

/** This module makes calibrated data (GeV, ns) from the raw data,
 *  for simulation only. 
 *  This is done by trivial subtraction of pedestal and multiplication 
 *  with a fixed gain.
 *
 *  It converts the dEmcRawData STAF table into emcTowerContainer object.
 *  Inputs (must be present somwhere under top)
 *  - dEmcGeometry STAF table
 *  - dEmcRawData STAF table
 *  - dEmcEvent STAF table (to be deprecated)
 *  - mEmcGeometryModule TObject (needed to get tower global positions)
 *  - emcTowerContainer object (will be resetted prior to work)
 *  - VtxOut object (needed to get flash time)
 *  
 *  Output : filled emcTowerContainer
 */

using namespace std;

namespace {

  PHBoolean message(const char* msg)
  {
    cerr << "mEmcGeaMakeCalibTower::process_event : cannot find "
	 << msg << endl;
    return ABORTRUN;
  }
}

//_____________________________________________________________________________
mEmcGeaMakeCalibTower::mEmcGeaMakeCalibTower(): SubsysReco("mEmcGeaMakeCalibTower")
{
}

//_____________________________________________________________________________
mEmcGeaMakeCalibTower::~mEmcGeaMakeCalibTower()
{}

//_____________________________________________________________________________
int
mEmcGeaMakeCalibTower::process_event(PHCompositeNode* top)
{
  emcNodeHelper nh;

  dEmcGeometryWrapper* dEmcGeometry = 
    nh.getTable<dEmcGeometryWrapper>("dEmcGeometry",top);

  if (!dEmcGeometry)
    {
      return message("dEmcGeometry");
    }

  dEmcRawDataWrapper* dEmcRawData = 
    nh.getTable<dEmcRawDataWrapper>("dEmcRawData",top);

  if (!dEmcRawData)
    {
      return message("dEmcRawData");
    }

  dEmcEventWrapper* dEmcEvent = 
    nh.getTable<dEmcEventWrapper>("dEmcEvent",top);

  if (!dEmcEvent)
    {
      return message("dEmcEvent");
    }

  emcTowerContainer* towers = 
    nh.getObject<emcTowerContainer>("emcTowerContainer",top);

  if (!towers)
    {
      return message("emcTowerContainer");
    }

  VtxOut* vtxout = nh.getObject<VtxOut>("VtxOut",top);
  if (!vtxout)
    {
      return message("VtxOut");
    }

  PHNodeIterator it(top);
  PHIODataNode<TObject>* geometryNode = static_cast<PHIODataNode<TObject>*>(it.findFirst("PHIODataNode","mEmcGeometry"));
  if (!geometryNode)
    {
      return message("mEmcGeometry Node");      
    }
  mEmcGeometryModule* mEmcGeometry = 
    static_cast<mEmcGeometryModule*>(geometryNode->getData());
  if (!mEmcGeometry)
    {
      return message("mEmcGeometry");
    }  

  recoConsts *rc = recoConsts::instance();
  float lowgain_convfac = rc->get_FloatFlag("EMCTOWERLOWGAIN", 0.001); 
  float highgain_convfac = rc->get_FloatFlag("EMCTOWERHIGHGAIN", 0.008); 
 
  const float mult_lothr = 0.005;
  const float mult_hithr = 0.020;
  float mult_lo, mult_hi;	/* Multiplicity with low and high threshold */

  float sectore[8];		/* Energy in each sector */
  float sectoret[8];		/* Energy in each sector */
  float tote, totet;

  static int l_first = 1;
  static float torad = M_PI / 180.0;

  const float r_tdc_convfac = 0.05;

  static float emc_geom[8][96][48][8];
  /*-------------------------------------------
    
  Executable

  -----------------------------------------*/

  if ( l_first == 1 )
    {
      for ( size_t i = 0; i < dEmcGeometry->RowCount(); ++i )
        {
          short iz = dEmcGeometry->get_ind(0,i);
          short iy = dEmcGeometry->get_ind(1,i);
	  short i1;

          if (dEmcGeometry->get_arm(i) == 0)
            {
              i1 = dEmcGeometry->get_sector(i);
            }
          else
            {
              i1 = 7 - dEmcGeometry->get_sector(i);
            }

          emc_geom[0][iz][iy][i1] = dEmcGeometry->get_nomxyz(0,i);
          emc_geom[1][iz][iy][i1] = dEmcGeometry->get_nomxyz(1,i);
          emc_geom[2][iz][iy][i1] = dEmcGeometry->get_nomxyz(2,i);
          emc_geom[3][iz][iy][i1] = dEmcGeometry->get_nomtheta(i);
          emc_geom[4][iz][iy][i1] = dEmcGeometry->get_nomphi(i);
          emc_geom[5][iz][iy][i1] = dEmcGeometry->get_nomdist(i);
          emc_geom[6][iz][iy][i1] = dEmcGeometry->get_nomflash(i);
          float d_work1 = dEmcGeometry->get_nomtheta(i);
          d_work1 = torad * d_work1;
          float d_work = sin(d_work1);
          emc_geom[7][iz][iy][i1] = d_work;

        }
      l_first = 0;
    }

  /* Sanity check of header for table "dEmcRawData"... */
  if ( dEmcRawData->MaxRowCount() == 0 )
    {
      return ABORTRUN;
    }

  /* Clear sector energy array */

  for ( size_t i = 0; i < 8 ; i++)
    {
      sectore[i] = 0.0;
      sectoret[i] = 0.0;
    }

  /* Process event */
  mult_lo = 0. ;
  mult_hi = 0. ;
  tote = 0.0;
  totet = 0.0;

  towers->Reset();

  static const double c = GSL_CONST_MKS_SPEED_OF_LIGHT*1E-7; // cm.ns-1
  static const float crazy_big = GSL_CONST_MKS_DAY;

  for ( size_t i = 0; i < dEmcRawData->RowCount(); ++i )
    {
      emcTowerContent* t_i = towers->addTower(i);
      assert(t_i!=0);

      // energy, low gain
      float e_lo = dEmcRawData->get_adclopost(i)-dEmcRawData->get_adclopre(i);
      // energy, high gain
      float e_hi = dEmcRawData->get_adchipost(i)-dEmcRawData->get_adchipre(i) ;

      //e_lo = e_lo * 0.001 ;
      //e_hi = e_hi * 0.008 ;
      e_lo = e_lo * lowgain_convfac ;
      e_hi = e_hi * highgain_convfac ;

      /* Instead of hardwiring use conv. factor, to be retrieved from DB
         tof  = dEmcRawData[i].tdc * 0.05 ;
      */
      float tof = dEmcRawData->get_tdc(i) * r_tdc_convfac;

      /* Changed to "count down from high pedestal
         Nov. 30, 1999 G.D. */
      if (e_lo < 0.0)
	{
	  e_lo = - e_lo;
	}
      if (e_hi < 0.0)
	{
	  e_hi = - e_hi;
	}

      /* Max(e_lo,i_hi) is the energy */
      float e = (e_lo > e_hi) ? e_lo : e_hi;

      /* Count hit multiplicity */
      if (e > mult_lothr)
	{
	  mult_lo = mult_lo + 1.0 ;
	}
      if (e > mult_hithr)
	{
	  mult_hi = mult_hi + 1.0 ;
	}

      int fem;
      int channel;
      int towerid = EmcIndexer::TowerID(dEmcRawData->get_swkey(i));
      EmcIndexer::PXPXSM144CH(towerid,fem,channel);
      int arm,sector,iy,iz;
      EmcIndexer::TowerLocation(towerid,arm,sector,iy,iz);
      int i1;
      
      if ( arm == 0 )
	{
	  i1 = sector;
	}
      else
	{
	  i1 = 7 - sector;
	}

      t_i->SetID(fem,channel);

      /* Suppress non-physical tof */
      if (tof > 0.0 && tof < 16.0)
	{
	  tof = 0.0;
	}

      /* correct for flash time */
      float x,y,z;      
      int is = mEmcGeometry->emcOfflineToEmc(arm,sector);
      mEmcGeometry->GetTowerPosGlobal(is,iz,iy,x,y,z);
      PHPoint towerpos(x,y,z);
      PHPoint vertex(0,0,0);

      if ( vtxout->isValid() )
	{
	  vertex = vtxout->get_Vertex();
	}
      
      float distance = towerpos.distanceToPoint(vertex);
      assert(distance>0);
      float tshift = distance/c;

      if ( tof < 1E-9 ) 
	{
	  t_i->SetCalibrated(e,crazy_big);
	}
      else
	{
	  t_i->SetCalibrated(e,tof-tshift);
	}

      totet = totet + emc_geom[7][iz][iy][i1] * e;

      /* Increment sector energy array */

      sectore[i1] = sectore[i1] + e ;
      sectoret[i1] = sectoret[i1] + emc_geom[7][iz][iy][i1] * e ;
      tote = tote + e;
    }

  /* Source of "invalid writes" according to Valgrind
     Thrown out temporarily to get code running for QM04 - Dec 20, 2003, GD
     
  for ( size_t i = 0; i < 8; ++i )
    {
      dEmcEvent->set_sece(0,i,sectore[i]);
      dEmcEvent->set_secet(0,i,sectoret[i]);
    }
  */

  dEmcEvent->set_twrmultlo(0,mult_lo);
  dEmcEvent->set_twrmulthi(0,mult_hi);
  dEmcEvent->set_tote(0,tote);
  dEmcEvent->set_totet(0,totet);

  return EVENT_OK;
}
