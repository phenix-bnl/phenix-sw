//////////////////////////////////////////////////////////////////////////////////
//
// creates emcGeaTowerContainer from raw data when simulated data is being 
// processed.
//
// This module makes calibrated data (GeV, ns) from the raw data,
//  for simulation only. 
//  This is done by trivial subtraction of pedestal and multiplication 
//  with a fixed gain.
//
// this is the reincarnation of mEmcGeaMakeCalibTower
//
//////////////////////////////////////////////////////////////////////////////////

#include <TRandom.h>
//#define norran_(x) { (*x)=0.1; }
//extern "C" void  norran_(float *x);
inline void norran_(float * x){ *x = gRandom->Gaus(); }



#include <cstdio>
#include <iostream>
#include <map>
#include <gsl/gsl_const_mks.h>

#include <PHPoint.h>
#include <PHTimeStamp.h>
#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>
#include <EmcIndexer.h>

#include <emcNodeHelper.h>

#include <mEmcGeometryModule.h>
#include <VtxOut.h>
#include <emcGeaRawDataContainer.h>
#include <emcGeaDeposit.h>
#include <emcGeaDepositContainer.h>
#include <emcGeaTowerContent.h>
#include <emcGeaTowerContainer.h>

#include <EmcGeaTowerSimMaker.h>


ClassImp(EmcGeaTowerSimMaker);



namespace {
  static const double c = GSL_CONST_MKS_SPEED_OF_LIGHT*1E-7; // cm.ns-1
  static const float crazy_big = GSL_CONST_MKS_DAY;
}



EmcGeaTowerSimMaker::EmcGeaTowerSimMaker(float lowgain, float highgain): 
  SubsysReco("EmcGeaTowerSimMaker"), lowgain_convfac(lowgain), highgain_convfac(highgain){
}



EmcGeaTowerSimMaker::~EmcGeaTowerSimMaker(){ 
}





int EmcGeaTowerSimMaker::InitRun(PHCompositeNode * root){
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );


  // check input
  VtxOut * vtxout = getGeaObject(VtxOut, VtxOut, "VtxOut", dstnode);
  EMCNODEASSERT( vtxout );

  mEmcGeometryModule * geommodule = getGeaObject(mEmcGeometryModule, mEmcGeometryModule, "mEmcGeometryModule", emcnode);
  EMCNODEASSERT( geommodule );

  emcGeaDepositContainer * deposits = getGeaObject(emcGeaDepositContainer, emcGeaDepositContainer, "emcGeaDepositContainer", dstnode);
  EMCNODEASSERT( deposits );


  // check/create output
  //emcGeaRawDataContainer * rawdatas = emcGeaRawDataContainer::createdef();
  //emcNodeHelper::insertObject< emcGeaRawDataContainer >(dstnode, rawdatas, "emcGeaRawDataContainer");

  emcGeaTowerContainer * towers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstnode);
  EMCNODEASSERT( towers );


  std::cout << __PRETTY_FUNCTION__ << ":   "
	    << "lowgain = " << lowgain_convfac << "   highgain = " << highgain_convfac
	    << std::endl;

  
  return 0;
}





int EmcGeaTowerSimMaker::Reset(PHCompositeNode * root){
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  //emcGeaRawDataContainer * rawdatas = getGeaObject(emcGeaRawDataContainer, emcGeaRawDataContainer, "emcGeaRawDataContainer", dstnode);
  //EMCNODEASSERT( rawdatas );
  //rawdatas->Reset();

  emcGeaTowerContainer * towers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstnode);
  EMCNODEASSERT( towers );
  towers->Reset();

  return 0;
}






int EmcGeaTowerSimMaker::process_event(PHCompositeNode * root){
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  VtxOut * vtxout = getGeaObject(VtxOut, VtxOut, "VtxOut", dstnode);
  EMCNODEASSERT( vtxout );

  mEmcGeometryModule * geommodule = getGeaObject(mEmcGeometryModule, mEmcGeometryModule, "mEmcGeometryModule", emcnode);
  EMCNODEASSERT( geommodule );

  emcGeaDepositContainer * deposits = getGeaObject(emcGeaDepositContainer, emcGeaDepositContainer, "emcGeaDepositContainer", dstnode);
  EMCNODEASSERT( deposits );
  
  
  //emcGeaRawDataContainer * rawdatas = getGeaObject(emcGeaRawDataContainer, emcGeaRawDataContainer, "emcGeaRawDataContainer", dstnode);
  //EMCNODEASSERT( rawdatas );
  
  emcGeaTowerContainer * towers = getGeaObject(emcGeaTowerContainer, emcTowerContainer, "emcTowerContainer", dstnode);
  EMCNODEASSERT( towers );
  




  //
  // first pass: create an entry in towertable for each tower that has deposit in it
  //  //                also record for each track that contributed to the tower in the
  //                tower's tracklist.
  //
  //                according to an agreement with gabor david we store in the
  //                calibrated tof and calibrated edep fields the tof and edep
  //                values created by the response code (that is geant edep after
  //                attenuation).
  //
  for(size_t i = 0; i < deposits->size(); i++){
    emcGeaDeposit * deposit = deposits->get(i);
    emcGeaTowerContent * tower = towers->find( deposit->get_towerid() );

    if( tower == NULL ){
      tower = emcGeaTowerContent::createdef( deposit->get_towerid() );
      int rc = towers->add( tower );
      if( rc == -1 ) return ABORTRUN; 

      tower = towers->get( rc );
      assert ( tower->get_towerid() == deposit->get_towerid() );
    }
  }





  //
  // second pass: create raw values. store them in towertable entries 
  //                entry in rawtable as well
  //
  for(size_t i = 0; i < towers->size(); i++){
    emcGeaTowerContent * tower = towers->get(i);

    emcGeaRawData * rawdata = new emcGeaRawData;
    //int rc = rawdatas->add(rawdata);
    //if( rc == -1 ){
    //  std::cerr << __PRETTY_FUNCTION__ << ": error appending raw data." << std::endl;
    //  return ABORTRUN;
    //}
    //rawdata = rawdatas->get(rc);


    // indices
    rawdata->type = EmcIndexer::isPbSc( tower->get_towerid() ) ? SECTOR_TYPE_PBSC : SECTOR_TYPE_PBGL;
    rawdata->swkey = EmcIndexer::SoftwareKey( tower->get_towerid() );
    rawdata->hwkey = 0; // todo

    // low gain
    rawdata->adclopre = low_ped;
    rawdata->adclopost = rawdata->adclopre - ( int ) (tower->get_edep(emcGeaDeposit::ORIG) / lowgain_convfac);
    if(rawdata->adclopost < minvalue) rawdata->adclopost = minvalue;
    rawdata->adclopost &= 0x00000FFF;
    
    // high gain
    rawdata->adchipre = high_ped;
    rawdata->adchipost = rawdata->adchipre - ( int ) (tower->get_edep(emcGeaDeposit::ORIG) / highgain_convfac);
    if(rawdata->adchipost < minvalue) rawdata->adchipost = minvalue;
    rawdata->adchipost &= 0x00000FFF;
    

    // tdc
    // slewing correction for pbgl
    if( rawdata->type == SECTOR_TYPE_PBSC ){
      float tof = tower->get_tof(emcGeaDeposit::ORIG);

      tof = tof - 2.2; // \\\todo what's this? part of slewing correction?
      // Add slewing correction for Year-2, because real data now
      // come out as in principle slewing corrected
      // the last term is not really slewing: it corrects for shifts
      // connected to increasing shower depths - not tested beyond 10 GeV!
      float r_slew_e = tower->get_edep(emcGeaDeposit::ORIG);
      float r_meas_t = tof;
      float r_slew_t = r_meas_t - 0.35 / pow(r_slew_e-0.015,0.75) - r_slew_e / 15.0;
      tof = r_slew_t;
      
      tower->set_tof(tof, emcGeaDeposit::ORIG);
      tower->set_tof(tof, emcGeaDeposit::CALIB);
    }


#ifdef  _COMPATIBLE_VERSION
#define NODEBUG(format...) {}
#define SIMPLEDEBUG(format...) { printf(format); fflush(stdout);}

#define TABLEDEBUG SIMPLEDEBUG

    { // tof smearing
      float noise;
      float tof = tower->get_tof(emcGeaDeposit::ORIG);

      if( rawdata->type == SECTOR_TYPE_PBSC ){

	norran_(&noise);
	/*
	// Year-2 sector dependent resolution
	if(i1==0) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.375*noise;
	if(i1==1) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.400*noise;
	if(i1==2) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.430*noise;
	if(i1==3) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.540*noise;
	if(i1==4) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.360*noise;
	if(i1==5) emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.400*noise;
	*/
	// Correction for Year-4 62 GeV effective resolution
	tof = tof + 0.280 + 0.150*noise;
	
	
	/*
	norran_(&noise);
	// Upper 2 rows in each FEM have slightly worse resolution
	if(iy==10 || iy==11 || iy==22 || iy==23 || iy==34 || iy==35) {
	emc_tof[i1][iy][iz] = emc_tof[i1][iy][iz] + 0.2*noise;
	}
	*/
      }

      if( rawdata->type == SECTOR_TYPE_PBGL ){
	tof = tof - 3.03;
	norran_(&noise);
	tof = tof - 0.6*noise;
      }


      tower->set_tof(tof, emcGeaDeposit::ORIG);
      tower->set_tof(tof, emcGeaDeposit::CALIB);
    }
#endif

    rawdata->tdc = (int)(tower->get_tof(emcGeaDeposit::ORIG) / tdc_convfac);
    rawdata->tdc &= 0x00000FFF;


    // store in tower
    if( tower->canHaveRaw() )
      tower->SetRaw(rawdata->adchipost, rawdata->adchipre, rawdata->adclopost, rawdata->adclopre, rawdata->tdc);

    if( tower->canHaveGain() )
      tower->SetADCTDC(0, tdc_convfac, highgain_convfac, lowgain_convfac);

    


#ifdef  _COMPATIBLE_VERSION
    {
      int arm, sector, iy, iz;
      EmcIndexer::TowerLocation(tower->get_towerid(), arm, sector, iy, iz);
      
      
      TABLEDEBUG("raw table:  (sector,iy,iz)=(%1d,%2d,%2d)  final    :  edep=%10f  tof=%10f  :  comp\n",
		 sector, iy, iz, tower->get_edep(emcGeaDeposit::ORIG), tower->get_tof(emcGeaDeposit::ORIG)
		 );    
      
      TABLEDEBUG("raw table:  (sector,iy,iz)=(%1d,%2d,%2d)  values   :  adc=(%4d,%4d,%4d,%4d)  tdc=%4d:  comp\n",
		 sector, iy, iz, 
		 rawdata->adclopre, rawdata->adchipre, 
		 rawdata->adclopost, rawdata->adchipost,
		 rawdata->tdc
		 );
    }
#endif


    //
    // third pass: create "orig" & "calib" values from converting raw values back to float.
    //                this Analog -> Digital -> Analog conversion simulates the 
    //                quantizing error
    //

    //    continue; // <---- third pass is disabled for now


    // energy
    float e_lo = lowgain_convfac * (rawdata->adclopost - rawdata->adclopre);
    if(e_lo < 0.0) e_lo = - e_lo;

    float e_hi = highgain_convfac * (rawdata->adchipost - rawdata->adchipre);
    if(e_hi < 0.0) e_hi = - e_hi;

    float e = (e_lo > e_hi) ? e_lo : e_hi;   // max(e_lo,i_hi) is the energy

    if( tower->ErrorNeighbours() & 0x400 ) e = 0.0;
    //    tower->set_edep(e, emcGeaDeposit::ORIG);
    tower->set_edep(e, emcGeaDeposit::CALIB);


    // tof
    float tof = tdc_convfac * rawdata->tdc;
    if (tof > 0.0 && tof < 16.0) tof = 0.0;   // Suppress non-physical tof

    int arm, sector, iy, iz;
    EmcIndexer::TowerLocation(tower->get_towerid(), arm, sector, iy, iz);

    // correct for flash time
    float x,y,z;      
    int is = geommodule->emcOfflineToEmc(arm,sector);
    geommodule->GetTowerPosGlobal(is,iz,iy,x,y,z);
    PHPoint towerpos(x,y,z);
    PHPoint vertex(0,0,0);

    if ( vtxout->isValid() ) vertex = vtxout->get_Vertex();
    
    float distance = towerpos.distanceToPoint(vertex); assert(distance > 0);
    float tshift = distance/c;
    
    if( tower->ErrorNeighbours() & 0x400 ) tof = 0.0; //tower->set_tof( 0.0 );
    else if (tof < 1E-9) tof = crazy_big; //tower->set_tof( crazy_big );
    else tof = tof-tshift;
    //    tower->set_tof( tof, emcGeaDeposit::ORIG);
    tower->set_tof( tof, emcGeaDeposit::CALIB);


    delete rawdata; rawdata = NULL;
  }



  return 0;
}



