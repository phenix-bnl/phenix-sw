//////////////////////////////////////////////////////////////////////////////////
//
// creates parameters table for simulation.
//
// now only calling old fortran/c/PAM/whatever module
//
////////////////////////////////////////////////////////////////////////////////////


#include <cstdio>

#include <PHCompositeNode.h>
#include <Fun4AllReturnCodes.h>

#include <emcGeaParams.h>
#include <emcGeaGeometry.h>

#include <emcNodeHelper.h>

#include <EmcGeaParamsSimMaker.h>


ClassImp(EmcGeaParamsSimMaker);





EmcGeaParamsSimMaker::EmcGeaParamsSimMaker(): SubsysReco("EmcGeaParamsSimMaker"){}



EmcGeaParamsSimMaker::~EmcGeaParamsSimMaker(){}



int EmcGeaParamsSimMaker::InitRun(PHCompositeNode * root){

  // create output
  PHCompositeNode* dstnode = emcNodeHelper::findCompositeNode(root, "DST"); EMCNODEASSERT( dstnode );

  emcGeaParams * geaparams = new emcGeaParams();
  emcNodeHelper::insertObject< emcGeaParams >(dstnode, geaparams, "emcGeaParams");
  geaparams->init(root);

  emcGeaGeometry * geometry = new emcGeaGeometry();
  emcNodeHelper::insertObject< emcGeaGeometry >(dstnode, geometry, "emcGeaGeometry");
  geometry->init(geaparams);


  return 0;
}



