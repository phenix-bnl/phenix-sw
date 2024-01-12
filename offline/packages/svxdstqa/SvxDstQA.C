
#include <iostream>
#include <iomanip>
#include <string>
#include <vector>

#include "PHCompositeNode.h"

#include "BbcOut.h"
#include "SvxRawhitList.h"
#include "SvxClusterList.h"
#include "SvxRawhitClusterList.h"
#include "SvxSegmentList.h"
#include "VtxOut.h"
#include "svxAddress.hh"

#include "SvxDstQA.h"
//#include "SvxFillHistoPixel.h"
#include "SvxFillHisto.h"

#include "recoConsts.h"


#include "TFile.h"
#include "getClass.h"

using namespace findNode;
using namespace std;

//ClassImp(SvxDstQA)

//==============================================================

SvxDstQA::SvxDstQA() {
  ThisName = "SvxDstQA";
  m_eventNumber=0;
  m_outputFileName="SvxDstQA.root";

  m_histoObj=NULL;
  m_fillHistoAry.clear();

  m_firstevent_in_run=true;

}

//==============================================================

SvxDstQA::SvxDstQA(string filename) {
  ThisName = "SvxDstQA";
  m_eventNumber=0;
  m_outputFileName=filename;

  m_histoObj=NULL;
  m_firstevent_in_run=true;
}

//==============================================================

SvxDstQA::~SvxDstQA() {
}

//==============================================================


bool SvxDstQA::registerFillHisto(SvxFillHisto *histObj) {
  if(verbosity>0) cout << PHWHERE << endl;

  if(histObj==NULL){
    cout<<" object is null"<<endl;
    return false;
  }

  if(verbosity>0) cout << PHWHERE <<" "<<histObj->GetName()<<endl;
  
  SvxFillHistoArray::iterator itr = m_fillHistoAry.find(histObj->GetName()); 
  if(itr!=m_fillHistoAry.end()){
    cout<<"histObj already registered: "<<histObj->GetName()<<endl;
    return false;
  }
  
  pair<string, SvxFillHisto*> p(histObj->GetName(), histObj);
  m_fillHistoAry.insert(p);
  
  return true;
}

//==============================================================

int SvxDstQA::Init(PHCompositeNode *topNode) {

  if(verbosity>0) cout << "SvxDstQA::Init started..." << endl;

  m_outputFile = new TFile(m_outputFileName.c_str(),"RECREATE");

  if(verbosity>0) cout << "SvxDstQA::Init: output file " << m_outputFileName << " opened." << endl;

  if(m_fillHistoAry.size()==0){
    cout<<"No FillHistoObject is registered"<<endl;
    return -1;
  }

  SvxFillHistoArray::iterator itr;
  for(itr=m_fillHistoAry.begin(); itr!=m_fillHistoAry.end(); ++itr){
    itr->second->initialize(m_outputFile);
  }

/*
  if(m_histoObj==NULL) {
    m_histoObj = new SvxFillHistoPixel();
    m_histoObj->initialize(m_outputFile);
  }
*/

  if(verbosity>0) cout << "SvxDstQA::Init ended." << endl;

  return 0;
}

//==============================================================
  
int SvxDstQA::InitRun(PHCompositeNode *topNode) {
  if(verbosity>0) cout << "SvxDstQA::InitRun started..." <<endl;
  m_firstevent_in_run=true;

  if(verbosity>0) cout << "SvxDstQA::InitRun ended." << endl;

  return 0;
}

//==============================================================

int SvxDstQA::process_event(PHCompositeNode *topNode) {
  static int init_ana=0;

  BbcOut               *bbc           = getClass<BbcOut>(topNode,"BbcOut");
  SvxRawhitList        *svxrawlist    = getClass<SvxRawhitList>(topNode,"SvxRawhitList");
  SvxClusterList       *svxclslist    = getClass<SvxClusterList>(topNode,"SvxClusterList");
  SvxRawhitClusterList *svxrawclslist = getClass<SvxRawhitClusterList>(topNode,"SvxRawhitClusterList");
  SvxSegmentList       *svxseglist    = getClass<SvxSegmentList>(topNode,"SvxSegmentList");
  svxAddress           *svxaddress    = getClass<svxAddress>(topNode,"svxAddress");
  VtxOut               *vtxout        = getClass<VtxOut>(topNode,"VtxOut");
//  svxAddress &address = *svxaddress;

  SvxEventContainer cnt;
  cnt.setPointer(bbc, svxrawlist, svxclslist, svxrawclslist, svxseglist,vtxout);

  if(verbosity>0) {
    if(init_ana==0) {
      cout<<"init ana"<<endl;
      //topNode->print(); 
      init_ana=1;
    
      if(svxrawlist!=NULL)    svxrawlist->identify();    else cout<<"no SvxRawhit object"<<endl;
      if(bbc!=NULL)           bbc->identify();           else cout<<"no BbcOut object"<<endl;

      cout<<endl<<endl;
      cout<<"init ana ends"<<endl;
    }
  }

  if(m_firstevent_in_run){
      recoConsts *rc = recoConsts::instance();
      int run = rc->get_IntFlag("RUNNUMBER");
      cout << "RecoConst Runnumber " << run << endl;

    //if(m_histoObj!=NULL) m_histoObj->fillInitRun(run);

    SvxFillHistoArray::iterator itr;
    for(itr=m_fillHistoAry.begin(); itr!=m_fillHistoAry.end(); ++itr){
//      itr->second->set_svxAddress(address);
      itr->second->set_svxAddress(svxaddress);
      itr->second->fillInitRun(run);
    }

    m_firstevent_in_run=false;
  }

  if(verbosity>0){
    if((m_eventNumber%1000)==0){
      cout<<"event : " <<m_eventNumber<<endl;
    }
  }

//------------------------ VTX information ----------------------------------------------------------------------

  {
    //m_histoObj->fill(svxrawlist, bbc);
    SvxFillHistoArray::iterator itr;
    for(itr=m_fillHistoAry.begin(); itr!=m_fillHistoAry.end(); ++itr){
      itr->second->fill(&cnt);
    }
  }

  m_eventNumber++;
  return 0;
}

//==============================================================

int SvxDstQA::End(PHCompositeNode *topNode) {
  if(verbosity>0) cout << "SvxDstQA::End:  Writing out..." << endl;

  m_outputFile->Write();

  //m_histoObj->write(m_outputFile);


  if(verbosity>0) cout << "SvxDstQA::End:  Closing output file..." << endl;

  m_outputFile->Close();
  delete m_outputFile;
  m_outputFile=NULL;


  return 0;
}

