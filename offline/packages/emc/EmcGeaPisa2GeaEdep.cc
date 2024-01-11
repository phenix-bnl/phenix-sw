//////////////////////////////////////////////////////////////////////////////////
//
// this reco module converts the (pisa hits in a pisa 
// root file) to (EmcHit) format.
//
// this module takes it's input from the /PISA/EmcPisaHit 
// and writes data to /EMC/EmcGeaGeaEdep
//
//////////////////////////////////////////////////////////////////////////////////


#include <PHCompositeNode.h>


#include <EmcPISAHit.h>
#include <Fun4AllReturnCodes.h>

#include <emcGeaParams.h>
#include <emcGeaEdep.h>
#include <emcGeaEdepContainer.h>

#include <emcNodeHelper.h>
#include <EmcGeaPisa2GeaEdep.h>


ClassImp(EmcGeaPisa2GeaEdep);





EmcGeaPisa2GeaEdep::EmcGeaPisa2GeaEdep(): SubsysReco("EmcGeaPisa2GeaEdep"){}




EmcGeaPisa2GeaEdep::~EmcGeaPisa2GeaEdep(){}



int EmcGeaPisa2GeaEdep::InitRun(PHCompositeNode * root){

  //PHCompositeNode * pisanode = emcNodeHelper::findCompositeNode(root, "PISA"); EMCNODEASSERT( pisanode != NULL );
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );


  // check input
  emcGeaParams * params = getGeaObject(emcGeaParams, emcGeaParams, "emcGeaParams", dstnode); 
  EMCNODEASSERT( params );


  // create output
  emcGeaEdepContainer * edeps = new emcGeaEdepContainer();
  emcNodeHelper::insertObject< emcGeaEdepContainer >(emcnode, edeps, "emcGeaEdepContainer");


  return 0;
}





int EmcGeaPisa2GeaEdep::Reset(PHCompositeNode * root){
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );

  emcGeaEdepContainer * edeps = getGeaObject(emcGeaEdepContainer, emcGeaEdepContainer, "emcGeaEdepContainer", emcnode);
  EMCNODEASSERT( edeps );
  edeps->Reset();

  return 0;
}





int EmcGeaPisa2GeaEdep::process_event(PHCompositeNode * root){

  //  PHCompositeNode * pisanode = emcNodeHelper::findCompositeNode(root, "PISA"); EMCNODEASSERT( pisanode );
  PHCompositeNode * emcnode = emcNodeHelper::findCompositeNode(root, "EMC"); EMCNODEASSERT( emcnode );
  PHCompositeNode * dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );
  emcGeaParams * param = getGeaObject(emcGeaParams, emcGeaParams, "emcGeaParams", dstnode);
  EMCNODEASSERT( param );

  emcGeaEdepContainer * edeps = getGeaObject(emcGeaEdepContainer, emcGeaEdepContainer, "emcGeaEdepContainer", emcnode);
  EMCNODEASSERT( edeps );
  

  EmcPISAHit *event = EmcPISAHit::GetEmcHitEvt();
  Int_t EmcRows = EmcPISAHit::GetEmcCount();   

  for(int i = 0; i < EmcRows; i++) {
    EmcPISAHit * pisahit = &event[i];
    emcGeaEdep * edep = new emcGeaEdep();
    
    // todo: could dio_ptrkstack to get more info (part type...)
    edep->trkno = pisahit->GetMctrack();
    edep->input = pisahit->GetNfile();

    edep->x = pisahit->GetPosx();
    edep->y = pisahit->GetPosy();
    edep->z = pisahit->GetPosz();

    edep->edep = edep->geaedep = pisahit->GetDele();
    edep->tof = edep->geatof = pisahit->GetTof();

    

    switch( pisahit->GetWall() ){
    case 1: edep->arm = ARM_WEST; edep->sector = SECTOR_W0; edep->type = SECTOR_TYPE_PBSC; break;
    case 2: edep->arm = ARM_WEST; edep->sector = SECTOR_W1; edep->type = SECTOR_TYPE_PBSC; break;
    case 3: edep->arm = ARM_WEST; edep->sector = SECTOR_W2; edep->type = SECTOR_TYPE_PBSC; break;
    case 4: edep->arm = ARM_WEST; edep->sector = SECTOR_W3; edep->type = SECTOR_TYPE_PBSC; break;
    case 5: edep->arm = ARM_EAST; edep->sector = SECTOR_E2; edep->type = SECTOR_TYPE_PBSC; break;
    case 6: edep->arm = ARM_EAST; edep->sector = SECTOR_E3; edep->type = SECTOR_TYPE_PBSC; break;
      
    case 7: edep->arm = ARM_EAST; edep->sector = SECTOR_E0; edep->type = SECTOR_TYPE_PBGL; break;
    case 8: edep->arm = ARM_EAST; edep->sector = SECTOR_E1; edep->type = SECTOR_TYPE_PBGL; break;
    } 

    edep->smodind = pisahit->GetIndex1();
    edep->towerind = pisahit->GetIndex2();
    

    int nmody, nmodz, nsmody /*, nsmodz */;
    nmody = (int)(*param)[edep->sector].nmody;
    nmodz = (int)(*param)[edep->sector].nmodz;
    nsmody = (int)(*param)[edep->sector].nsmody;
    /* nsmodz = (int)(*param)[edep->sector].nsmodz; -- flagged by scan-build as dead assignment */


    {
      int z_off = ( ( edep->smodind-1 ) / nsmody )   *   nmodz;
      int y_off = ( ( edep->smodind-1 ) % nsmody )   *   nmody;
      
      /*	Get the cell array indices within the supermodule	*/
      int y_rel = 0 , z_rel = 0;
      switch( edep->type ){
      case SECTOR_TYPE_PBSC:
	z_rel = 1 + (edep->towerind - 1) /  nmody;
	y_rel = edep->towerind - (z_rel - 1) * nmody;
	break;
	
      case SECTOR_TYPE_PBGL:
	y_rel = 1 + (edep->towerind - 1) /  nmodz;
	z_rel = edep->towerind - (y_rel - 1) * nmodz;
	break;
      }
      
      edep->iz = z_rel + z_off - 1;
      edep->iy = y_rel + y_off - 1;
      
      // Numbering in iz goes the other way (from positive z to negative) in the West Arm
      if(edep->arm == ARM_WEST) edep->iz = 71 - edep->iz;
    }  
    
    
  
    switch(edep->sector){ // 100000*arm + 10000*sector + 100*y + z
    case SECTOR_W0: edep->swkey =   0000 + 100*edep->iy + edep->iz; break;
    case SECTOR_W1: edep->swkey =  10000 + 100*edep->iy + edep->iz; break;
    case SECTOR_W2: edep->swkey =  20000 + 100*edep->iy + edep->iz; break;
    case SECTOR_W3: edep->swkey =  30000 + 100*edep->iy + edep->iz; break;
    case SECTOR_E1: edep->swkey = 100000 + 100*edep->iy + edep->iz; break;
    case SECTOR_E0: edep->swkey = 110000 + 100*edep->iy + edep->iz; break;
      
    case SECTOR_E3: edep->swkey = 120000 + 100*edep->iy + edep->iz; break;
    case SECTOR_E2: edep->swkey = 130000 + 100*edep->iy + edep->iz; break;
    }
  


    { // Build hardware key
      int ismodz, ismody, irely, irelz, ismoffset, itower;
      
      ismodz = edep->iz / nmodz;
      ismody = edep->iy / nmody;
      irelz = edep->iz % nmodz;
      irely = edep->iy % nmody;
      
      ismoffset = nmodz * nmody * nsmody * ismodz  +  nmody * nmodz * ismody;
      itower = ismoffset + nmody * irely + irelz;
 
      edep->hwkey = edep->sector * 0x2000 + itower; // TODO... is the sector value ok?
    }    
    
    

    if( edeps->add(edep) == -1 ) return ABORTRUN;
  }


  return 0;
}








