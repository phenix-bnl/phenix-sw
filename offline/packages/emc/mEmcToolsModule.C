//-------------------------------------------------------------------------
// 
// Package: offline/packages/emc
//
// Purpose: Encapsulate functions basically used by the simul+real embedding procedure
// 
// Copyright (C) PHENIX collaboration, 2000-2001
//
// Author: David D'ENTERRIA SUBATECH 
//-------------------------------------------------------------------------


#include "mEmcToolsModule.h"
#include "dEmcCalibTowerWrapper.h"
#include "emcCalibratedDataObject.h"
#include "fkinWrapper.h"
#include "KinPISAHit.h"
#include "EmcIndexer.h"
#include "emcManageable.h"
#include "emcDataManager.h"
#include "emcRawDataAccessor.h"
#include "EmcDynamicData.h"
#include "emcDBMS.h"
#include "mEmcGeometryModule.h"


#include "PHIODataNode.h"
#include "PHNodeReset.h"

#include "TTree.h"

#include "gsl/gsl_math.h"

#include <cstdlib>
#include <iomanip>
#include <cassert>

using namespace std;

//_____________________________________________________________________________
// Default constructor

mEmcToolsModule::mEmcToolsModule()
{
  fEmc_QA = 0;
  fVerbose = 0 ;
  fZeroEnergyAndTOFInDeadTowers = true;
}

//_____________________________________________________________________________
mEmcToolsModule*
mEmcToolsModule::instance()
{
  static mEmcToolsModule* single = new mEmcToolsModule();
  return single;
}

//_____________________________________________________________________________
//
// Set ecal and tof to zero if the tower is dead (default)
// Set it to false if you want a pure simulation without real data deadmap effects
// (e.g. for pure acceptance)

void
mEmcToolsModule::setEnergyAndTOFtoZeroInDeadTowers(const bool DeadToZero) 
{ 
  fZeroEnergyAndTOFInDeadTowers = DeadToZero;
}

//_____________________________________________________________________________
Int_t
mEmcToolsModule::EmcCollectDeadMap(const PHTimeStamp when)
{

  PHTimeStamp when1;
  when1.set("Mon Jul 1 00:00:00 2000"); // Run-1 ~start

  if ( when < when1 )
    {
      cout << "<W> EmcCollectDeadMap: " << when << " is not a valid date." << endl ;
      return -1;
    }

  // Get DataMap of current configuration
  int emcmap = EmcGetDataMap();
  if ( emcmap ==-1 )
    {
      return -1;
    }

  // Collection of Q&As from Database
  emcQAs qaDriver ;

  PHTimeStamp when2;
  when2.set("Mon Jan 1 00:00:00 2001"); // Run-2 start

  string ExtraRejectListFilename ;

  emcDataManager* dm = emcDataManager::GetInstance() ;

  // Set some parameters for the qa object we want
  qaDriver.SetSource(emcDBMS::get()) ;

  // New MV reject list format 
  if ( when < when2 )  // Year-1
    {
      ExtraRejectListFilename = "/afs/rhic/phenix/phnxemc/PBGL/emc_extra_reject_yr1.list" ;  
      qaDriver.SetExtraRejectListFilename(ExtraRejectListFilename.c_str())  ;  
    }
  else if ( when > when2 ) // Year-2
    {
      ExtraRejectListFilename = "/afs/rhic/phenix/phnxemc/DATA/emc_extra_reject_Run2pp.list";
      //ExtraRejectListFilename = "/afs/rhic/phenix/phnxemc/DATA/emc_extra_reject_Run2AuAu.list";
      qaDriver.SetExtraRejectListFilename(ExtraRejectListFilename.c_str())  ;  
    }

  cout << "<I> EmcCollectDeadMap: extrareject list: " << qaDriver.GetExtraRejectListFilename() << endl ;

  // Collect it
  fEmc_QA = dynamic_cast<emcQAs*>( dm->Collect(qaDriver,when) ) ;
  if (!fEmc_QA) 
    {
      cerr << "<E> EmcCollectDeadMap: Could not collect Q&A ?!" << endl ;
      return -1;
    }

  return 0;
}

//_____________________________________________________________________________
// Gets DataMap for current EMCal configuration
Int_t
mEmcToolsModule::EmcGetDataMap()
{

  vector<Int_t> emc_map; // transitory DataMap [index<->channel] for QA
  emcRawDataAccessor* tmp_rda = emcRawDataAccessor::GetInstance() ;
  assert(tmp_rda!=0);

  Int_t nchannel = (Int_t)tmp_rda->GetDynamicData()->getEmcSize(); // get size of emc dyn. arrays
  Int_t* tmp_map = (Int_t*)tmp_rda->GetDynamicData()->getEmcMap();
  if (!tmp_map) 
    {
      cerr << "<E> EmcGetDataMap: Could not collect Q&A data map ?!" << endl ;
      return -1;
    }

  // Fill associated STL vector with data mapping. Style:  
  // 'true' = absolute FEM addresses (channel #):  address=absFEM*192+ch, where absFEM=FEM#(0-171), ch=channel#(0-191).
  // 'false'= EMCal towers mapping style (towers number): address=tower#(0-24767).
  if (!tmp_rda->GetDynamicData()->getMapStyle()) 
    {
      cout << "<W> EmcGetDataMap: Datamap for deadmap is in tower-number style "
	   << " (this is not expected in offline !!)" << endl ;
      // FIXME: We do not need to reproduce address=tower# numbering (0-24767) in this strange case ... otherwise:
      //if (emc_map.empty()) {
      //emc_map.resize(nchannel);
      //for (Int_t i=0; i<nchannel; i++) emc_map.push_back(i); 
      //}
      return -1;
    }

  if (fVerbose >0)
    {
      cout << "<I> EmcGetDataMap: Datamap for deadmap is in FEM-type style" << endl ;
    }
  if (emc_map.empty()) 
    {
      emc_map.resize(nchannel);
      copy(tmp_map,tmp_map+nchannel,emc_map.begin());
    }

  // let's swap the order of the indexes in the vector so that it is easier to find a given channel
  // Definite DataMap [channel<->index] for QA
  fEmc_map.clear();
  for (Int_t i = 0 ; i < nchannel ; i++ ) 
    { 
      fEmc_map[ (Int_t) emc_map[i] ] = i ;
    }

  return 0;
}

//_____________________________________________________________________________
// Assign DeadMap of real towers to simulated ones
Int_t 
mEmcToolsModule::AssignRealDeadMaptoSimulTowers(PHCompositeNode *topNode)
{

  if (!fEmc_QA) 
    {
      cout << "<W> AssignRealDeadMaptoSimulTowers: EMC dead-map propagation requested" 
	   << " but no EMC dead-map available" << endl ;
      return -1;
    }

  PHNodeIterator mainIter(topNode);

  dEmcCalibTowerWrapper* dEmcCalibTower = 0;
  PHIODataNode<PHTable>* dEmcCalibTowerNode = 0;
  dEmcCalibTowerNode =(PHIODataNode<PHTable>*)mainIter.findFirst("PHIODataNode","dEmcCalibTower");
  if (dEmcCalibTowerNode)
    {
      dEmcCalibTower = (dEmcCalibTowerWrapper*)dEmcCalibTowerNode->getData();
    }
  else
    {
      cout << "<W> AssignRealDeadMaptoSimulTower: dEmcCalibTower table not found ..." << endl;
      return -1 ;
    }
  
  Int_t dead = 0 ;
  Int_t arm = 0, isector = 0, yrow = 0, zrow = 0;
  Int_t towerid = 0; 
  Int_t rowcount = 0;
  Int_t dead_count = 0;

  rowcount = (Int_t)dEmcCalibTower->RowCount();

  if (rowcount) 
    {
      cout << endl << "<I> AssignRealDeadMaptoSimulTower: Retrieved EMCal DeadMaps for " 
           << (rowcount-1) << " Calibtowers " << endl;
    }

  for( Int_t j = 0 ; j < rowcount; j++ ) 
    {
      isector  = (Int_t)dEmcCalibTower->get_sector(j) ;
      yrow     = (Int_t)dEmcCalibTower->get_ind(1,j) ;
      zrow     = (Int_t)dEmcCalibTower->get_ind(0,j) ;
      arm      = (Int_t)dEmcCalibTower->get_arm(j) ; 
      // transform offline sector number (0-3 West and 0-3 East) into online one (0-7)
      // Valid transformation for Year-1 and Year-2 data. CKB 28 Nov. '01 
      if ( arm == 1 )
        {
          if( isector > 1 )
            {
              isector+=2;
            }
          else
            {
              isector+=6;
            }
        }
      towerid = EmcIndexer::getTowerId(isector,zrow,yrow);
      
      dead     = fEmc_QA->GetDead( (Int_t)fEmc_map[towerid]); 
      if ( (dead & emcQAs::IamDeadMask()) && fZeroEnergyAndTOFInDeadTowers ) 
        { 
          if (!(dead_count)) cout << "<I> AssignRealDeadMaptoSimulTower: EMCal dead channel(s): " ;
	  cout << towerid << " (" << fEmc_map[towerid] << ") = " << dead << ", " << flush ;
          // set ecal and tof to zero 
          // In the case of real data (in MDO -> RDO):
          // a) in compressed mode: the tower is simply dropped 
          // b) in non-compressed mode: ADC and TDC are set to zero
	  // We adopt b) here.  When merging with a real event,
	  // the tower will be there (i.e. observable by the evaluator) but empty.
          dEmcCalibTower->set_ecal(j,0.) ;
          dEmcCalibTower->set_tof(j,0.) ;
          dead_count++;
        }
      dEmcCalibTower->set_deadmap(j,dead) ; 
    }
  if (dead_count) cout << "<I> Total num. of EMCal dead channels: " << dead_count << endl ;
  
  return 0;
}

//_____________________________________________________________________________
PHBoolean
mEmcToolsModule::event(PHCompositeNode* topNode)
{
  int rv = AssignRealDeadMaptoSimulTowers(topNode);
  if ( rv )
    {
      return false;
    }
  else
    {
      return true;
    }
}

//_____________________________________________________________________________
// Get the Calibrated Data Object from the CalibTower Table 
// (temporary thing till we git rid of calibtower table in dst)
void 
mEmcToolsModule::GetCdoFromCalibTowerTable(const dEmcCalibTowerWrapper& dEmcCalibTower, 
					   emcCalibratedDataObject& cdo, 
					   const Float_t SimCorrection)
{
  Int_t j;
  Int_t towerid = 0;
  Int_t arm = 0 , isector = 0 , yrow = 0 , zrow = 0;
  Int_t dummy = 0 , deadNeighbours = 0;
  Float_t energy = 0. , time = 0.;  
  Float_t etotal = 0.;
  Long_t softkey = 0; 
  Int_t rowcount = 0 ;
  Int_t warn = 0;

  rowcount = (Int_t)dEmcCalibTower.RowCount();
  cdo.Reset();

  // fill the Cdo from the CalibTower table
  for (j = 0 ; j < rowcount; j++) 
    {
      // this is a tricky workaround to get part of the calibtower data
      // members just using the softkey and the cdo decodekey function
      softkey = (Long_t)dEmcCalibTower.get_swkey(j) ;
      cdo.DecodeKey( softkey, arm, isector, yrow, zrow);
      
      // alternatively this is the "long" way ...
      //arm     = dEmcCalibTower.get_arm(j) ; 
      //isector = dEmcCalibTower.get_sector(j) ;
      //yrow    = dEmcCalibTower.get_ind(1,j) ;
      //zrow    = dEmcCalibTower.get_ind(0,j) ;
      
      // let's transform offline sector number (0-3 West and 0-3 East) into online one (0-7)
      // Valid transformation for year-1 and year-2 data. CKB 28 Nov. '01 
      if ( arm == 1 )
        {
          if( isector > 1 )
            {
              isector+=2;
            }
          else
            {
              isector+=6;
            }
        }
      towerid = EmcIndexer::getTowerId(isector,zrow,yrow);

      // Correction for PbGl Simulation, to take 15% energy correction for real events into account
      if( (arm) && (isector>=6) ) energy  = dEmcCalibTower.get_ecal(j)*SimCorrection;
      else energy  = dEmcCalibTower.get_ecal(j) ;
      
      time    = dEmcCalibTower.get_tof(j) ;
      deadNeighbours = dEmcCalibTower.get_deadmap(j) ;
      warn    = dEmcCalibTower.get_warnmap(j) ;
      cdo.Set(j, towerid, softkey, dummy, energy, time, deadNeighbours, warn);
      
      etotal += energy ;
      if ( fVerbose>0 ) 
        {
          if ( j == 0 ) cout << " Cdo Filled (id,towerid,softkey,errflag,ecal,tof,deadneigh,warn) = " << endl;
          cout << j      << "," << towerid << "," << softkey        << "," << dummy << "," 
               << energy << "," << time    << "," << deadNeighbours << "," << warn ;
          cout << "  Softkey CalibTower: " << dEmcCalibTower.get_swkey(j) << " CDO Generated: " 
               << cdo.GenerateSoftwareKey(towerid) << " CDO Gotten: " << cdo.GetSoftwareKey(j) << endl ;
        }
    }
  
  cdo.SetTotalEnergy(etotal);
  cdo.SetZeroSuppressedFlag(true);
  cdo.SetEnergyCalibratedFlag(true);
  cdo.SetTimeCalibratedFlag(true) ;

}

//_____________________________________________________________________________
// Get the CalibTower Table from the Calibrated Data Object
// (temporary thing till we git rid of calibtower table in dst)
void 
mEmcToolsModule::GetCalibTowerTableFromCdo( dEmcCalibTowerWrapper& dEmcCalibTower, 
					    const emcCalibratedDataObject& cdo)
{

  Int_t j;
  Int_t arm = 0, sector = 0, yrow = 0 , zrow = 0;
  Short_t outrow = 0 ;   
  Long_t softkey = 0 ;  
  Float_t energy = 0.;
  Float_t tof = 0.;
  Int_t type = 1; // Type = 1 (PbSc) by default ...
  Int_t dead = 0 ;
  Int_t warn = 0 ;
 
  emcCalibratedDataObject cdo2(cdo); // copy to update indexmap and softkeys otherwise softkey are zero (??)
  cdo2.Update();

  // fill the CalibTower table from cdo
  for (j = 0 ; j < cdo.GetSize(); j++) 
    {
      cdo.Get( j, energy, tof) ;
      softkey = cdo2.GetSoftwareKey(j) ;
      dead = (Int_t)cdo.GetDead(j) ;
      warn = (Int_t)cdo.GetWarn(j) ;

      if (energy > 0.001) { // Hard-coded 1 MeV CalibTower threshold
        cdo.DecodeKey( softkey, arm, sector, yrow, zrow);
        dEmcCalibTower.set_id(outrow,outrow) ;
        dEmcCalibTower.set_hwkey(outrow,0) ;
        dEmcCalibTower.set_swkey(outrow,softkey) ;
        dEmcCalibTower.set_type(outrow,type) ;
        dEmcCalibTower.set_arm(outrow,arm) ;
        dEmcCalibTower.set_sector(outrow,sector) ;
        dEmcCalibTower.set_ind(1,outrow,yrow) ;
        dEmcCalibTower.set_ind(0,outrow,zrow) ;
        dEmcCalibTower.set_ecal(outrow,energy) ;
        dEmcCalibTower.set_tof(outrow,tof) ;
        dEmcCalibTower.set_deadmap(outrow,dead) ;
        dEmcCalibTower.set_warnmap(outrow, warn);
        
        if ( fVerbose>0 ) 
          {
            if ( outrow==0 )
              {
                cout << "dEmcCalibTower Filled (id,hwkey,swkey,type,arm,sector,yrow,zrow,ecal,tof,dead,warn) = " << endl ;
              }
            cout << outrow << "," << 0    << "," << softkey << "," << type   << "," << arm << "," 
                 << sector << "," << yrow << "," << zrow    << "," << energy << "," << tof << ","
                 << dead   << "," << warn << endl;
          }
        outrow++ ;
      }
    }
  
  dEmcCalibTower.SetRowCount(outrow);
  
}

//_____________________________________________________________________________
//
// If the particle of momentum p and vertex v heads into EMCal
// return the sector number (iS = 0-7). Otherwise return -1.
// Original function: CKB April '02
int
mEmcToolsModule::HitInEMCalAcceptance(const float *v, const float *p)
{
  cout << "<E> mEmcToolsModule::HitInEMCalAcceptance(..) not supported anymore." << endl
       << "    use mEmcGeometryModule::HitInEMCalAcceptance(..) instead ! " << endl
       << "    EVENT NOT ACCEPTED " << endl;
  return -1;

#if 0
  static mEmcGeometryModule EmcGeometry;
 
  // Separate the "Directions" of the track
  // p[0] < 0 is East, p[0] > 0 is West  
  // This is important because a line has no starting point
  // like a track

  // this is a real Point
  PHPoint vtx(v[0],v[1],v[2]);

  // take care that p gives only the direction of the momentum vector
  PHVector hit(p[0],p[1],p[2]);
  PHLine line(vtx,hit);

  int is;

  // isIntersection routine tells you wether a given PHLine
  // intersects with sector is (in emc convention)
  //        / 3   \  4
  // West  |  2    | 5 East
  //       |  1    | 6 
  //        \ 0   /  7

  if(p[0]>0){
    for( is = 0; is<4; is++ ){ // loop over the west arm
      if(EmcGeometry.isIntersection(line,is)) return is;
    }
  }
  else if(p[0]<0){
    for( is = 4 ; is<8 ; is++ ){ // loop over the east arm
      if(EmcGeometry.isIntersection(line,is)) return is;
    }
  }
  
  return -1;
#endif
}

//_____________________________________________________________________________
//
// The same as the former method, but now we pass the geometry as argument 
//
int 
mEmcToolsModule::HitInEMCalAcceptance(const float *v, 
				      const float *p, 
				      mEmcGeometryModule* EmcGeometry )
{

  cout << "<E> mEmcToolsModule::HitInEMCalAcceptance(..) not supported anymore." << endl
       << "    use mEmcGeometryModule::HitInEMCalAcceptance(..) instead ! " << endl
       << "    EVENT NOT ACCEPTED " << endl;
  return -1;

#if 0
  if (!EmcGeometry) 
    {
      HitInEMCalAcceptance(v, p);
    }

  // Separate the "Directions" of the track
  // p[0] < 0 is East, p[0] > 0 is West  
  // This is important because a line has no starting point
  // like a track

  // this is a real Point
  PHPoint vtx(v[0],v[1],v[2]);

  // take care that p gives only the direction of the momentum vector
  PHVector hit(p[0],p[1],p[2]);
  PHLine line(vtx,hit);

  int is;

  // isIntersection routine tells you wether a given PHLine
  // intersects with sector is (in emc convention)
  //        / 3   \  4
  // West  |  2    | 5 East
  //       |  1    | 6 
  //        \ 0   /  7

  if(p[0]>0){
    for( is = 0; is<4; is++ ){ // loop over the west arm
      if(EmcGeometry->isIntersection(line,is)) return is;
    }
  }
  else if(p[0]<0){
    for( is = 4 ; is<8 ; is++ ){ // loop over the east arm
      if(EmcGeometry->isIntersection(line,is)) return is;
    }
  }
  
  return -1;
#endif 
}

//_____________________________________________________________________________
//
// If the single direct gamma or the 2 decay-gammas from a pi0/eta decay
// head into EMCal return 1. Otherwise, return 0.
// Original function: CKB, April '02
int 
mEmcToolsModule::EventInEMCalAcceptance(const PISAEvent *pisaEvent, 
					const int kevent, TTree *T, 
					mEmcGeometryModule* geom)
{
 
  cout << "<E> mEmcToolsModule::EventInEMCalAcceptance(..) not supported anymore." << endl
       << "    use mEmcGeometryModule::EventInEMCalAcceptance(..) instead ! " << endl
       << "    EVENT NOT ACCEPTED " << endl;
  return 0;

#if 0
  T->GetEvent(kevent);
  int kpart = pisaEvent->GetKinNhit();
  if ( !kpart ) { return 0; } // no particle information: skip event
 
  float ptot;
  float pthet;
  float pphi;

  float p[3];
  float v[3];

  // loop on pisaEvent
  for( int ipart = 0; ipart<kpart; ipart++)
    {
      TClonesArray *fpisaKinHits = pisaEvent->GetKinHits();
      KinPISAHit *KinHit = (KinPISAHit*)fpisaKinHits->UncheckedAt(ipart);
      
      int idpart  = KinHit->GetIdpart();
      int idparent = KinHit->GetIdparent();
      int itparent = KinHit->GetItparent();
      
      int nGamma = 0;
      int nGamAcc = 0;

      // Found a primary photon
      if( idparent==0 && idpart==1 && itparent < 0 )
	{
	  v[0] = 0; // Pi0 decays instantly
	  v[1] = 0;
	  v[2] = KinHit->GetZvertex();
	  
	  ptot = KinHit->GetPtot() ;
	  pphi = KinHit->GetPhi() *M_PI/180.;
	  pthet = KinHit->GetPthet() *M_PI/180.;
	  
	  p[0] = ptot * sin (pthet) * cos (pphi);
	  p[1] = ptot * sin (pthet) * sin (pphi);
	  p[2] = ptot * cos (pthet);
      
	  if (HitInEMCalAcceptance(v,p,geom)>=0) nGamAcc++; // 0 is a valid sector: W0 !
	  if ( nGamAcc!=1 ) return 0; // gamma not on detector

	  return 1;

	} // photon
      
      // Found a primary pion
      // We accept only Two-Photon Decays where
      // both can hit the detector
      if( idparent==0 && idpart==7 && itparent < 0 )
	{
     	  if(kpart<3) // at least one decay photon not even stored 
	    { 
	      return 0;  // ==> no hit on active surface so skip it
	    }
 
	  for(int i = 0;i < kpart;i++)
	    {
	      // Search for Decay gammas
	      KinPISAHit *Hit = (KinPISAHit*)fpisaKinHits->UncheckedAt(i);
	      if( Hit->GetIdparent () == 7  && Hit->GetIdpart () == 1 )
		{
		  if(nGamma<2)
		    {
		      // cout << "Photon Nr " << nGamma << endl;
		      v[0] = 0; // Pi0 decays instantly
		      v[1] = 0;
		      v[2] = Hit->GetZvertex();
	     
		      ptot = Hit->GetPtot() ;
		      pphi = Hit->GetPhi() *M_PI/180.;
		      pthet = Hit->GetPthet() *M_PI/180.;
		      
		      p[0] = ptot * sin (pthet) * cos (pphi);
		      p[1] = ptot * sin (pthet) * sin (pphi);
		      p[2] = ptot * cos (pthet);
		      
		      if (HitInEMCalAcceptance(v,p,geom)>=0) nGamAcc++; // 0 is a valid sector: W0 !
		    }
		  nGamma++;	    
		}
	    }
     
	  if( nGamma!=2 ) { return 0; } // Not a 2-gamma Decay !
	  if( nGamAcc!=2 ){ return 0; } // Not both gammas on Detector 

	  return 1;
	  
	} // pi0
  
      // Found a primary eta
      // We accept only Two-Photon Decays where
      // both head to the detector
      if( idparent==0 && idpart==17 && itparent < 0)
	{
	  if(kpart<3) // at  least one decay photon not even stored 
	    {
	      return 0;  // ==> no hit on active surface so skip it
	    }
      
	  int nGamma = 0;
	  int nGamAcc = 0;
	  
	  for(int i = 0;i < kpart;i++)
	    {
	      // Search for Decay gammas
	      KinPISAHit *Hit = (KinPISAHit*)fpisaKinHits->UncheckedAt(i);
	      if(Hit->GetIdparent () == 17 && Hit->GetIdpart () == 1)
		{
		  if(nGamma<2)
		    {
		      //cout << "Photon Nr " << nGamma << endl;
		      v[0] = 0; // eta decays instantly
		      v[1] = 0;
		      v[2] = Hit->GetZvertex();
		      
		      ptot = Hit->GetPtot() ;
		      pphi = Hit->GetPhi() *M_PI/180.;
		      pthet = Hit->GetPthet() *M_PI/180.;
		      
		      p[0] = ptot * sin (pthet) * cos (pphi);
		      p[1] = ptot * sin (pthet) * sin (pphi);
		      p[2] = ptot * cos (pthet);
		      
		      if (HitInEMCalAcceptance(v,p,geom)>=0) nGamAcc++; // 0 is a valid sector: W0 !
		    }
		  nGamma++;		    
		} 
	    }

	  if( nGamma!=2 )  { return 0; } // Not a 2-gamma Decay !
	  if( nGamAcc!=2 ) { return 0; } // Not both gammas heading to the detector
	  
	  return 1;

	} // eta

    } // end loop on pisaEvent

  cout << "<W> EventInEMCalAcceptance: " 
       << "No primary pi0, eta or gamma found ... NO acceptance cut applied !" << endl;

  return 1;
#endif
}


//_____________________________________________________________________________
//
// If the particle of momentum p and vertex v heads into PbSc
// return true. Otherwise return false.
bool
mEmcToolsModule::HitInPbSc(const float *v, const float *p, int& sector, 
			   mEmcGeometryModule* EmcGeometry)
{

  cout << "<E> mEmcToolsModule::HitInPbSc() not supported anymore." << endl
       << "    use mEmcGeometryModule::HitInPbSc() instead ! " << endl
       << "    EVENT NOT ACCEPTED " << endl;
  return false;

#if 0
  sector = HitInEMCalAcceptance(v,p,EmcGeometry);

  // PbSc sectors (in EMC convention)
  if ( sector == 0 || sector == 1 || sector == 2 || sector == 3 || sector == 4 || sector == 5 ) return true;

  return false; // case: -1 (outside EMCal) or 6,7 (PbGl)
#endif
}

//_____________________________________________________________________________
//
// If the particle of momentum p and vertex v heads into PbGl
// return true. Otherwise return false.
bool
mEmcToolsModule::HitInPbGl(const float *v, const float *p, int& sector, 
			   mEmcGeometryModule* EmcGeometry)
{
  cout << "<E> mEmcToolsModule::HitInPbGl() not supported anymore." << endl
       << "    use mEmcGeometryModule::HitInPbGl() instead ! " << endl
       << "    EVENT NOT ACCEPTED " << endl;
  return false;

#if 0
  sector = HitInEMCalAcceptance(v,p,EmcGeometry);
  if ( sector == 7 || sector == 6 ) return true; // PbGl sectors (in EMC convention)

  return false; // case: -1 (outside EMCal) or 0-5 (PbSc)
#endif
}
