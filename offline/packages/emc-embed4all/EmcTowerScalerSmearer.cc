
#include <cassert>

#include <TRandom.h>

#include <PHObject.h>
#include <PHNodeOperation.h>
#include <PHNodeIterator.h>
#include <Fun4AllReturnCodes.h>
#include <emcNodeHelper.h>
#include <EmcIndexer.h>

#include <emcTowerContent.h>
#include <emcGeaTowerContent.h>
#include <emcTowerContainer.h>

#include <EmcTowerScalerSmearer.h>

using namespace std;




ClassImp(EmcTowerScalerSmearer);





EmcTowerScalerSmearer::EmcTowerScalerSmearer(float scale, float smear): SubsysReco("EmcTowerScalerSmearer"){ 
  SetScale(scale);
  SetSmear(smear);
  SetSmear2(0.0);
}





int EmcTowerScalerSmearer::Init(PHCompositeNode * root){
  gRandom->SetSeed(0);

  return EVENT_OK;
}





void EmcTowerScalerSmearer::SetScale(float s0, float s1, float s2, float s3, float s4, float s5, float s6, float s7){
  scale[0] = s0, scale[1] = s1, scale[2] = s2, scale[3] = s3, scale[4] = s4, scale[5] = s5;
  scale[6] = s6, scale[7] = s7;
}





void EmcTowerScalerSmearer::SetScale(float pbsc, float pbgl){
  SetScale(pbsc, pbsc, pbsc, pbsc, pbsc, pbsc, pbgl, pbgl); 
}





void EmcTowerScalerSmearer::SetScale(float scale){
  SetScale(scale, scale); 
}





void EmcTowerScalerSmearer::SetSmear(float s0, float s1, float s2, float s3, float s4, float s5, float s6, float s7){
  smear[0] = s0, smear[1] = s1, smear[2] = s2, smear[3] = s3, smear[4] = s4, smear[5] = s5;
  smear[6] = s6, smear[7] = s7;
}





void EmcTowerScalerSmearer::SetSmear(float pbsc, float pbgl){
  SetSmear(pbsc, pbsc, pbsc, pbsc, pbsc, pbsc, pbgl, pbgl); 
}





void EmcTowerScalerSmearer::SetSmear(float smear){
  SetSmear(smear, smear); 
}




void EmcTowerScalerSmearer::SetSmear2(float s0, float s1, float s2, float s3, float s4, float s5, float s6, float s7){
  smear2[0] = s0, smear2[1] = s1, smear2[2] = s2, smear2[3] = s3, smear2[4] = s4, smear2[5] = s5;
  smear2[6] = s6, smear2[7] = s7;
}





void EmcTowerScalerSmearer::SetSmear2(float pbsc, float pbgl){
  SetSmear2(pbsc, pbsc, pbsc, pbsc, pbsc, pbsc, pbgl, pbgl); 
}





void EmcTowerScalerSmearer::SetSmear2(float smear){
  SetSmear2(smear, smear); 
}





int EmcTowerScalerSmearer::InitRun(PHCompositeNode * root){

  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(root, "DST"); 
  EMCNODEASSERT( dst );

  emcTowerContainer * towers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", dst);
  EMCNODEASSERT( towers );

  return EVENT_OK;
}





int EmcTowerScalerSmearer::process_event(PHCompositeNode * root){

  PHCompositeNode* dst = emcNodeHelper::findCompositeNode(root, "DST"); 
  EMCNODEASSERT( dst );

  emcTowerContainer * towers = emcNodeHelper::getObject< emcTowerContainer >("emcTowerContainer", dst);
  EMCNODEASSERT( towers );

  for(size_t i = 0; i < towers->size(); i++){
    emcTowerContent * tower = towers->getTower( i );
    emcGeaTowerContent * geatower = dynamic_cast< emcGeaTowerContent * >( tower );

    int iarm, isec, iy, iz;
    EmcIndexer::TowerLocation( tower->towerid(), iarm, isec, iy, iz );
    isec = iarm ? ( 7 - isec ) : isec;
    assert( 0 <= isec  &&  isec <= 7 );

		float oldE = -9999, newE = -9999;
		if ( geatower ) oldE = geatower->get_edep();
		else oldE = tower->Energy();
    newE = scale[isec]*oldE;
		newE += gRandom->Gaus(0,newE*smear[isec]) + gRandom->Gaus(0,sqrt(newE)*smear2[isec]);
		double newscale = 1;
		if (oldE>0) newscale = newE/oldE;
		// std::cout<<"sector "<<isec<<" scale "<<scale[isec]<<" smearc1 "<<smear[isec]<<" smearc2 "<<smear2[isec]<<std::endl;
		// std::cout<<"sector "<<isec<<" new scale "<<newscale<<std::endl;
    if ( geatower ) geatower->scale_edep( newscale );
    else tower->SetCalibrated( newscale * tower->Energy(), tower->ToF() );
  }


  return EVENT_OK;
}
