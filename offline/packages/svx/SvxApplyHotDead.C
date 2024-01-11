// ===============
// FILE: SvxApplyHotDead.C
// ===============

// ******************************************************
//
// Class: SvxApplyHotDead implementation
//
// Author:  Takashi Hachiya (hachiya@rcf.rhic.bnl.gov)
// 
// Revisions: November 2011 - initial version
//
// ***************************************************************************

#include "SvxApplyHotDead.h"

#include "svxAddress.hh"
#include "SvxPixelHotDeadMap.h"
#include "SvxPixelHotDeadMapv2.h"
#include "SvxDeadMap.h"
#include "SvxDaqErrorMap.h"

#include "SvxRawhitList.h"
#include "SvxRawhit.h"

#include <Fun4AllReturnCodes.h>

#include <PHCompositeNode.h>
#include <PHIODataNode.h>
#include <getClass.h>

#include <cstdlib>
#include <iostream>
#include <TRandom3.h>

using namespace std;
TRandom3* rnd;
//----------------------------------------------------------------------------------------------------

SvxApplyHotDead::SvxApplyHotDead(const string &name) :
  SubsysReco(name),
  _timer(PHTimeServer::get()->insert_new(name))
{
  m_address      = NULL;
  m_pixelhotdead = NULL;
  m_pixelhotdead2 = NULL;
  m_striphotdead = NULL;
  m_daqerr       = NULL;
  m_applytoMerged = false;
  m_simclustercorrection = false;
  b0threshold = 0.8;
  b1threshold = 0.72;
  b2threshold = 0.98;
  b3threshold = 0.98;
  m_maskDaqErrorFlag=true; //  this should be true in the future, but temporally false until this function will be approved

  rnd = new TRandom3();
  rnd->SetSeed(0);
}

SvxApplyHotDead::~SvxApplyHotDead()
{
//  cout<<"SvxApplyHotDead::~SvxApplyHotDead"<<endl;
//  objects are deleted in SvxParManager
//     SvxPixelHotDeadMap for pixel
//     SvxDeadMap for strip
}


//------------------------------------------------------------------------------------------------------

// Run-independent initialization
int SvxApplyHotDead::Init(PHCompositeNode *topNode)
{
  if (verbosity>0) cout << "SvxApplyHotDead::Init() Execution started." << endl;

  int i = CreateNodeTree(topNode);
  if (verbosity>0) cout << "SvxApplyHotDead::Init() CreateNodeTree() returned " << i << endl;
  if (!(i==EVENT_OK)) {return EVENT_OK;}


  if (verbosity>0) cout << "SvxApplyHotDead::Init() Execution completed." << endl;

  return EVENT_OK;
}

//----------------------------------------------------------------------------------------------------

// Run-dependent initialization
int SvxApplyHotDead::InitRun(PHCompositeNode *topNode)
{

  if (verbosity>0) 
    cout << "SvxApplyHotDead::InitRun() Execution started.." << endl;


  ///////////////////////
  // added by T.Hachiya 2011.11.06
  m_address = findNode::getClass<svxAddress>(topNode, "svxAddress");
  if ( m_address == NULL) {
    if (verbosity>0) { cout << PHWHERE<< "Can't find svxAddress. " << endl; }
    return ABORTRUN;
  }
  
  m_pixelhotdead = findNode::getClass<SvxPixelHotDeadMap>(topNode, "SvxPixelHotDeadMap");
  if ( m_pixelhotdead == NULL) {
    if (verbosity>0) { cout << PHWHERE<< "Can't find SvxPixelHotDeadMap. " << endl; }
    return ABORTRUN;
  }

  //use this one for pixel hot dead map from RUN14
  m_pixelhotdead2 = findNode::getClass<SvxPixelHotDeadMapv2>(topNode, "SvxPixelHotDeadMapv2");
  if ( m_pixelhotdead2 == NULL) {
    if (verbosity>0) { cout << PHWHERE<< "Can't find SvxPixelHotDeadMapv2. " << endl; }
    return ABORTRUN;
  }

  m_striphotdead = findNode::getClass<SvxDeadMap>(topNode, "SvxStripHotDeadMap");
  if ( m_striphotdead == NULL) {
    cerr << PHWHERE<< "Can't find SvxStripHotDeadMap. " << endl;
    return ABORTRUN;
  }

  m_daqerr = findNode::getClass<SvxDaqErrorMap>(topNode, "SvxDaqErrorMap");
  if ( m_daqerr == NULL) {
    cerr << PHWHERE<< "Can't find SvxDaqErrorMap. " << endl;
    return ABORTRUN;
  }

  if (verbosity>0) 
    cout << "SvxApplyHotDead::InitRun() Node tree created." << endl;

  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------

int SvxApplyHotDead::process_event(PHCompositeNode *topNode)
{
  _timer.get()->restart();
  if (verbosity>0) cout << "SvxApplyHotDead::process_event() Execution started..." <<endl;

  SvxRawhitList *svxrawhit = findNode::getClass<SvxRawhitList>(topNode,"SvxRawhitList");

  int nrawhit = (svxrawhit!=NULL) ? svxrawhit->get_nRawhits() : 0;

  for (int ihit=0; ihit<nrawhit; ihit++) {
    SvxRawhit* rhit = svxrawhit->get_Rawhit(ihit);
    int r_section = rhit->get_svxSection();
    int r_layer   = rhit->get_layer();
    int r_ladder  = rhit->get_ladder();
    int r_channel = rhit->get_channel();

    // apply hot&dead flag to Rawhit 

    if (r_section==0) { // pixel or stripixel
      int status=0;
      bool isOkForClustering = true;
      if (r_layer<2) { // pixel
         int pixelMod = rhit->get_pixelModule();
         int pixelRoc = rhit->get_pixelROC();
         int r_ix  = m_address->getPixelRocIX0(r_channel);
         int r_iz  = m_address->getPixelRocIZ0(r_channel);

         status = m_pixelhotdead2->getStatus(pixelMod, pixelRoc, r_iz, r_ix); 
	 isOkForClustering = m_pixelhotdead2->isPixelOkForClustering(pixelMod, pixelRoc, r_iz, r_ix); //currently implemented for strips only
      }
      else { // stripixel
        int r_sensor  = rhit->get_sensor();
        int r_SS      = rhit->get_sensorSection();
        int r_readout = rhit->get_sensorReadout();

        status = m_striphotdead->channelStatus(r_layer-2,r_ladder,r_sensor,r_SS,r_readout,r_channel,0);
	//!Temporary, this needs to be fixed for strips. D McGlinchey 10/11/2013
	if (status != 0)
	  isOkForClustering = false;
      }
      rhit->set_HotDeadFlag(status);
      rhit->set_isOkForClustering(isOkForClustering);

      if (verbosity>1) {
        if (status>0) {
          cout<<"HotDead:Status="<<status<<" : "<<r_layer<<" "<<r_ladder<<" "<<r_channel<<endl;
        }
      }
    }
  }

  ///////////////////////////////
  // put the daq error flag to mask the ladder in which DAQ error usually happen
  maskDaqError(topNode);

  ///////////////////////////////

  if (m_applytoMerged) {
    if (verbosity>0) {
      cout << "SvxApplyHotDead::process_event() Applying hot/dead map to merged Rawhits..." <<endl;
    }

    SvxRawhitList *svxrawhit = findNode::getClass<SvxRawhitList>(topNode,"SvxMergedRawhitList");
    if (svxrawhit) {
      int nrawhit = svxrawhit->get_nRawhits();
      //cout << "SvxApplyHotDead: # of merged rawhits = " << nrawhit << endl;
      
      for (int ihit=0; ihit<nrawhit; ihit++) {
        SvxRawhit* rhit = svxrawhit->get_Rawhit(ihit);
        int r_section = rhit->get_svxSection();
        int r_layer   = rhit->get_layer();
	int r_ladder  = rhit->get_ladder();
	int r_channel = rhit->get_channel();

        if (r_section==0) { // pixel or stripixel
          int status=0;
	  bool isOkForClustering = true;

          if (r_layer<2) { // pixel
            int pixelMod = rhit->get_pixelModule();
            int pixelRoc = rhit->get_pixelROC();
            int r_ix  = m_address->getPixelRocIX0(r_channel);
            int r_iz  = m_address->getPixelRocIZ0(r_channel);
            status = m_pixelhotdead2->getStatus(pixelMod, pixelRoc, r_iz, r_ix);
	    isOkForClustering = m_pixelhotdead2->isPixelOkForClustering(pixelMod, pixelRoc, r_iz, r_ix); //currently implemented for strips only
          }
          else { // stripixel
            int r_sensor  = rhit->get_sensor();
            int r_SS      = rhit->get_sensorSection();
            int r_readout = rhit->get_sensorReadout();
	    int striplayer = r_layer - 2;
            status = m_striphotdead->channelStatus(striplayer,r_ladder,r_sensor,r_SS,r_readout,r_channel,0);
          }
          rhit->set_HotDeadFlag(status);
	  rhit->set_isOkForClustering(isOkForClustering);
	  //cout << "SvxApplyHotDead: rawhit status: " << status << endl;

        }
      } // end loop over merged rawhits
    } // merged rawhit node exists
  }
  // cout << " simclustercorrection is " << m_simclustercorrection << endl;


  if (m_simclustercorrection)
    {
      if (verbosity>0) {
	cout << "SvxApplyHotDead::process_event() Applying correction to nclusters to match data rate" <<endl;
      }


      SvxRawhitList *svxrawhit = findNode::getClass<SvxRawhitList>(topNode,"SvxRawhitList");
      
      int nrawhit = (svxrawhit!=NULL) ? svxrawhit->get_nRawhits() : 0;
      
      for (int ihit=0; ihit<nrawhit; ihit++) {
	SvxRawhit* rhit = svxrawhit->get_Rawhit(ihit);
	int r_layer   = rhit->get_layer();

	// apply hot&dead flag to Rawhit 
	
	if (rhit->get_isOkForClustering() == true)
	  {
	    bool isOkForClustering = true;
	    if (r_layer == 0)
	      {
		double probcheck = rnd->Uniform(1);
		
		if (probcheck > b0threshold)
		  {
		    isOkForClustering = false;
		  }
		
	      }
	    else if (r_layer == 1) 
	      {
		double probcheck = rnd->Uniform(1);
		if (probcheck > b1threshold){
		  isOkForClustering = false;
		}	    
	      }
	    else if (r_layer == 2) 
	      {
		double probcheck = rnd->Uniform(1);
		if (probcheck > b2threshold){
		  isOkForClustering = false;
		}	    
	      }
	    else if (r_layer == 3) 
	      {
		double probcheck = rnd->Uniform(1);
		if (probcheck > b3threshold){
		  isOkForClustering = false;
		}	    
	      }




	    if (isOkForClustering == false)
	      {
		
		
		rhit->set_isOkForClustering(isOkForClustering);
		
	      }
	  }
      } 
    }



  if (verbosity>0) {
    cout << "SvxApplyHotDead::process_event() Event processed." <<endl;
  }
  
  _timer.get()->stop();
  return EVENT_OK;
}

//---------------------------------------------------------------------------------------------------------

// Create the data
int SvxApplyHotDead::CreateNodeTree(PHCompositeNode *topNode) 
{
  if (verbosity>0) 
    cout << "SvxApplyHotDead::CreateNodeTree() Execution started." << endl;

  if (verbosity>0) 
    cout << "SvxApplyHotDead::CreateNodeTree() processed." <<endl;
  
  return EVENT_OK;
}

//--------------------------------------------------------------------------------------------

int SvxApplyHotDead::End(PHCompositeNode *topNode)
{
  return EVENT_OK;
}


/**
 This function is to check the DAQ error and to put the DAQ error flag to each rawhit
 */
void SvxApplyHotDead::maskDaqError(PHCompositeNode *topNode)
{
  // if false, just return
  if(!m_maskDaqErrorFlag) return;


  /////////////////////////////////
  // apply DAQerror
  SvxRawhitList *svxrawhit = findNode::getClass<SvxRawhitList>(topNode, "SvxRawhitList");

  int nrawhit = (svxrawhit!=NULL) ? svxrawhit->get_nRawhits() : 0;

  for (int ihit=0; ihit<nrawhit; ihit++) {
    SvxRawhit* rhit = svxrawhit->get_Rawhit(ihit);
    int r_section = rhit->get_svxSection();
    int r_layer   = rhit->get_layer();
    int r_ladder  = rhit->get_ladder();
    int r_sensor  = rhit->get_sensor();
    int r_channel = rhit->get_channel();

    // apply hot&dead flag to Rawhit 

    if (r_section==0) { // pixel or stripixel

      int idet   = (r_layer<2) ? 0 : 1;

      int module = (r_layer<2) ? m_address->getPixelModuleID(r_layer, r_ladder, r_sensor)
                               : m_address->getStripModuleID(r_layer, r_ladder);

      unsigned int isMaskModule = (idet==0) ? m_daqerr->getPixelStatus(module) 
                                            : m_daqerr->getStripStatus(module);

      if(isMaskModule){
        int status = rhit->get_HotDeadFlag();
        status+=DAQERROR;

        rhit->set_HotDeadFlag(status);
        rhit->set_isOkForClustering(false);

        if (verbosity>0) {
          cout<<"DaqError:Status="<<status<<" : "<<module<<" ("<<r_layer<<" "<<r_ladder<<" "<<r_sensor<<" "<<r_channel<<")"<<endl;
        }
      }

    }
  }
}

