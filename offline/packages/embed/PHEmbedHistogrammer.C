// ----------------------------
// Created by:  Jiangyong Jia
//-----------------------------
#include "PHEmbedHistogrammer.hh"
#include "PHEmbedMcRecoTrack.hh"

#include "TFile.h"
#include "TH1.h"
#include "TString.h"
#include "TTree.h"

#include <iostream>
using namespace std;

PHEmbedHistogrammer* PHEmbedHistogrammer::_instance =0;

PHEmbedHistogrammer::PHEmbedHistogrammer()
{
  flagInit = 0;
  filename = "embed.root";
  ntupleList = new TList();
  histoList  = new TList();
  embedMcRecoTrack = 0;
  mctrk = new PHEmbedMcRecoTrack;
  Datafile = 0;
  verbose = 0;
}

void PHEmbedHistogrammer::CloseOutputFile() {

  if(Datafile){
    cout << "PHEmbedHistogrammer: closing output file " 
	 << Datafile->GetName() << endl;
    Datafile->Close();
  }
  return;
}

PHEmbedHistogrammer::~PHEmbedHistogrammer()
{
  
  if (ntupleList) {
    ntupleList->Delete();
    delete ntupleList;
  }
  
  if (histoList) {
    histoList->Delete();
    delete histoList;
  }

  if (mctrk) {
    delete mctrk;
  }
}

PHEmbedHistogrammer* PHEmbedHistogrammer::instance()
{    
  if(!_instance) {
    cout << " making new _instance" << endl;
    _instance = new PHEmbedHistogrammer();
  }

  return _instance;
}

PHBoolean PHEmbedHistogrammer::setFileName(TString name){
  if (!flagInit) {
    if(name.Length()>0){
      filename = name;
    }
    return True;
  }
  return False;
}

int PHEmbedHistogrammer::initializeFile(char* name)
{
  if (flagInit == 0) { // initializeFile hasn't already been called
    if(name){
      filename = name;
    }
    cout << "PHEmbedHistogrammer file name is: " 
	 << filename << endl;
    Datafile = new TFile(filename,"RECREATE",filename);
    flagInit = 1;
  }

  return 0;
}
int PHEmbedHistogrammer::saveToFile(int option)
{
  initializeFile();
  flush();
  return 0;
}
//____________________________________________________________________________
int  PHEmbedHistogrammer::addNtuple(TTree* val)
{
  initializeFile();
  ntupleList->Add(val);
  return 1;
}
int PHEmbedHistogrammer::addHistogram(TH1*val)
{
  initializeFile();
  histoList->Add(val);
  return 1;
}

void PHEmbedHistogrammer::flush()
{

  TObject* obj = 0;
  if (!Datafile) { 
    cout << "PHEmbedHistogrammer::flush(): Output file not yet created. Skipping." 
	 << endl;
    return;
  }
  
  Datafile->cd();

  if (ntupleList->GetSize() > 0) {
    int nb1 = 0;
    cout << "ntupleList: " << ntupleList->GetName() << endl;
    cout << "size: " << ntupleList->GetSize() << endl;
    cout << "entries: " << ntupleList->GetEntries() << endl;
    
    TIter ntuple(ntupleList);
    while((obj = ntuple())) {
      nb1 =   obj->Write(0,TObject::kOverwrite);
      cout << "PHEmbedHistogrammer::flush(): wrote " 
	   << obj->GetName() << ", " << nb1 << " bytes." << endl;  
    }
  }

  if (histoList->GetSize() > 0) {
    int nb2 = 0;
    cout << "histoList: " << histoList->GetName() << endl;
    cout << "GetSize: " << histoList->GetSize() << endl;
    cout << "GetEntries: " << histoList->GetEntries() << endl;
    
    TIter histo(histoList);
    while((obj = histo())) {
      nb2 =   obj->Write(0,TObject::kOverwrite);
      cout << "PHEmbedHistogrammer::flush(): wrote " 
	   << obj->GetName() << ", " << nb2 << " bytes." << endl;  
    }
  }
  
  Datafile->Flush();
}

PHBoolean PHEmbedHistogrammer::fillPHEmbedMcRecoTrack(PHEmbedMcRecoTrack* track){

  if(!embedMcRecoTrack){

    initializeFile();    
    TFile *tmp=0;
    if(gFile!=Datafile){
      tmp = gFile;
      Datafile->cd();
    }

    embedMcRecoTrack = new TTree("EmbedMcRecoTrack","EmbedMcRecoTrack");
    embedMcRecoTrack->Branch("type",             &(mctrk->type),             "type/S");
    //embedMcRecoTrack->Branch("evtID",            &(mctrk->evtID),            "evtID/I");
    embedMcRecoTrack->Branch("idGeant",          &(mctrk->idGeant),          "idGeant/I");
    //embedMcRecoTrack->Branch("evtxG",            &(mctrk->evtxG),            "evtxG/F");
    //embedMcRecoTrack->Branch("evtyG",            &(mctrk->evtyG),            "evtyG/F");
    embedMcRecoTrack->Branch("evtzG",            &(mctrk->evtzG),            "evtzG/F");
    embedMcRecoTrack->Branch("pptG",             &(mctrk->pptG),             "pptG/F");
    embedMcRecoTrack->Branch("ppzG",             &(mctrk->ppzG),             "ppzG/F");
    embedMcRecoTrack->Branch("pthe0G",           &(mctrk->pthe0G),           "pthe0G/F");
    embedMcRecoTrack->Branch("pphi0G",           &(mctrk->pphi0G),           "pphi0G/F");
    embedMcRecoTrack->Branch("prapiG",           &(mctrk->prapiG),           "prapiG/F");
    
    // 7 variables
    embedMcRecoTrack->Branch("genG",             &(mctrk->genG),             "genG/S");
    embedMcRecoTrack->Branch("partidG",          &(mctrk->partidG),          "partidG/I");
    embedMcRecoTrack->Branch("pareidG",          &(mctrk->pareidG),          "pareidG/I");
    embedMcRecoTrack->Branch("primidG",          &(mctrk->primidG),          "primidG/I");
    embedMcRecoTrack->Branch("xvtxG",            &(mctrk->xvtxG),            "xvtxG/F");
    embedMcRecoTrack->Branch("yvtxG",            &(mctrk->yvtxG),            "yvtxG/F");
    embedMcRecoTrack->Branch("zvtxG",            &(mctrk->zvtxG),            "zvtxG/F");
    
    //global info
    // 9 variables
    embedMcRecoTrack->Branch("bbcqn",              &(mctrk->bbcqn),              "bbcqn/F");
    embedMcRecoTrack->Branch("zdcen",              &(mctrk->zdcen),              "zdcen/F");
    embedMcRecoTrack->Branch("bbcqs",              &(mctrk->bbcqs),              "bbcqs/F");
    embedMcRecoTrack->Branch("zdces",              &(mctrk->zdces),              "zdces/F");
    embedMcRecoTrack->Branch("bbcvtx",             &(mctrk->bbcvtx),             "bbcvtx/F");
    embedMcRecoTrack->Branch("bbct0",              &(mctrk->bbct0),              "bbct0/F");
    embedMcRecoTrack->Branch("isminbias",          &(mctrk->isminbias),          "isminbias/S");
    embedMcRecoTrack->Branch("bbccent",            &(mctrk->bbccent),            "bbccent/F");
    embedMcRecoTrack->Branch("centclock",          &(mctrk->centclock),          "centclock/F");

  // 18 variables
    embedMcRecoTrack->Branch("ntrkG",             &(mctrk->ntrkG),            "ntrkG/S");
    embedMcRecoTrack->Branch("dctrkidG",          &(mctrk->dctrkidG),         "dctrkidG/S");
    embedMcRecoTrack->Branch("dctrkQualG",        &(mctrk->dctrkQualG),       "dctrkQualG/S");
    embedMcRecoTrack->Branch("the0G",             &(mctrk->the0G),            "the0G/F");
    embedMcRecoTrack->Branch("phi0G",             &(mctrk->phi0G),            "phi0G/F");
    embedMcRecoTrack->Branch("alphaG",            &(mctrk->alphaG),           "alphaG/F");
    embedMcRecoTrack->Branch("thetaG",            &(mctrk->thetaG),           "thetaG/F");
    embedMcRecoTrack->Branch("phiG",              &(mctrk->phiG),             "phiG/F");
    embedMcRecoTrack->Branch("betaG",             &(mctrk->betaG),            "betaG/F");
    embedMcRecoTrack->Branch("zedG",              &(mctrk->zedG),             "zedG/F");
    embedMcRecoTrack->Branch("momG",              &(mctrk->momG),             "momG/F");
    embedMcRecoTrack->Branch("ptG",              &(mctrk->ptG),             "ptG/F");
    //embedMcRecoTrack->Branch("x1mG",              &(mctrk->x1mG),             "x1mG/F");
    //embedMcRecoTrack->Branch("x2mG",              &(mctrk->x2mG),             "x2mG/F");
    embedMcRecoTrack->Branch("x1hG",              &(mctrk->x1hG),             "x1hG/S");
    embedMcRecoTrack->Branch("x2hG",              &(mctrk->x2hG),             "x2hG/S");
    //embedMcRecoTrack->Branch("uv1hG",             &(mctrk->uv1hG),            "uv1hG/S");
    //embedMcRecoTrack->Branch("uv2hG",             &(mctrk->uv2hG),            "uv2hG/S");
    
    /*
     *Mc id is the responsed id( in test particle node) correaponding to the original geant hit,
     *for DC I use dctrackidG (id of perfect tracks)
     */
    // 6 variables
    //embedMcRecoTrack->Branch("pc1idMc",            &(mctrk->pc1idMc),            "pc1idMc/S");
    //embedMcRecoTrack->Branch("pc2idMc",            &(mctrk->pc2idMc),            "pc2idMc/S");
    //embedMcRecoTrack->Branch("pc3idMc",            &(mctrk->pc3idMc),            "pc3idMc/S");
    //embedMcRecoTrack->Branch("tofidMc",            &(mctrk->tofidMc),            "tofidMc/S");
    //embedMcRecoTrack->Branch("tecidMc",            &(mctrk->tecidMc),            "tecidMc/S");
    //embedMcRecoTrack->Branch("emcidMc",            &(mctrk->emcidMc),            "emcidMc/S");
    // 24 variables
    embedMcRecoTrack->Branch("pc1xMc",            &(mctrk->pc1xMc),            "pc1xMc/F");
    embedMcRecoTrack->Branch("pc1yMc",            &(mctrk->pc1yMc),            "pc1yMc/F");
    embedMcRecoTrack->Branch("pc1zMc",            &(mctrk->pc1zMc),            "pc1zMc/F");
    embedMcRecoTrack->Branch("pc2xMc",            &(mctrk->pc2xMc),            "pc2xMc/F");
    embedMcRecoTrack->Branch("pc2yMc",            &(mctrk->pc2yMc),            "pc2yMc/F");
    embedMcRecoTrack->Branch("pc2zMc",            &(mctrk->pc2zMc),            "pc2zMc/F");
    embedMcRecoTrack->Branch("pc3xMc",            &(mctrk->pc3xMc),            "pc3xMc/F");
    embedMcRecoTrack->Branch("pc3yMc",            &(mctrk->pc3yMc),            "pc3yMc/F");
    embedMcRecoTrack->Branch("pc3zMc",            &(mctrk->pc3zMc),            "pc3zMc/F");

    //TOFE
    embedMcRecoTrack->Branch("tofxMc",            &(mctrk->tofxMc),            "tofxMc/F");
    embedMcRecoTrack->Branch("tofyMc",            &(mctrk->tofyMc),            "tofyMc/F");
    embedMcRecoTrack->Branch("tofzMc",            &(mctrk->tofzMc),            "tofzMc/F");
    embedMcRecoTrack->Branch("toftMc",            &(mctrk->toftMc),            "toftMc/F");
    embedMcRecoTrack->Branch("tofeMc",            &(mctrk->tofeMc),            "tofeMc/F");
    embedMcRecoTrack->Branch("tofpidMc",          &(mctrk->tofpidMc),          "tofpidMc/I");
    embedMcRecoTrack->Branch("pltofMc",           &(mctrk->pltofMc),           "pltofMc/F");


    //TOFW
    embedMcRecoTrack->Branch("tofwxMc",            &(mctrk->tofwxMc),            "tofwxMc/F");
    embedMcRecoTrack->Branch("tofwyMc",            &(mctrk->tofwyMc),            "tofwyMc/F");
    embedMcRecoTrack->Branch("tofwzMc",            &(mctrk->tofwzMc),            "tofwzMc/F");
    embedMcRecoTrack->Branch("tofwtMc",            &(mctrk->tofwtMc),            "tofwtMc/F");
    embedMcRecoTrack->Branch("tofweMc",            &(mctrk->tofweMc),            "tofweMc/F");
    embedMcRecoTrack->Branch("tofwpidMc",          &(mctrk->tofwpidMc),          "tofwpidMc/I");
    embedMcRecoTrack->Branch("pltofwMc",           &(mctrk->pltofwMc),           "pltofwMc/F");

    embedMcRecoTrack->Branch("tecxMc",            &(mctrk->tecxMc),            "tecxMc/F");
    embedMcRecoTrack->Branch("tecyMc",            &(mctrk->tecyMc),            "tecyMc/F");
    embedMcRecoTrack->Branch("teczMc",            &(mctrk->teczMc),            "teczMc/F");
    embedMcRecoTrack->Branch("emcxMc",            &(mctrk->emcxMc),            "emcxMc/F");
    embedMcRecoTrack->Branch("emcyMc",            &(mctrk->emcyMc),            "emcyMc/F");
    embedMcRecoTrack->Branch("emczMc",            &(mctrk->emczMc),            "emczMc/F");
    //embedMcRecoTrack->Branch("pltofMc",           &(mctrk->pltofMc),           "pltofMc/F"); // MOVED TO ABOVE BY RON
    embedMcRecoTrack->Branch("plcrkMc",           &(mctrk->plcrkMc),           "plcrkMc/F");
    embedMcRecoTrack->Branch("plemcMc",           &(mctrk->plemcMc),           "plemcMc/F");

    // 15 variables
    embedMcRecoTrack->Branch("emctrkno0Mc",   &(mctrk->emctrkno0Mc),   "emctrkno0Mc/S");
    embedMcRecoTrack->Branch("emctrkno1Mc",   &(mctrk->emctrkno1Mc),   "emctrkno1Mc/S");
    embedMcRecoTrack->Branch("emctrkno2Mc",   &(mctrk->emctrkno2Mc),   "emctrkno2Mc/S");
    embedMcRecoTrack->Branch("emctwrhit0Mc",  &(mctrk->emctwrhit0Mc),  "emctwrhit0Mc/S");
    embedMcRecoTrack->Branch("emctwrhit1Mc",  &(mctrk->emctwrhit1Mc),  "emctwrhit1Mc/S");
    embedMcRecoTrack->Branch("emctwrhit2Mc",  &(mctrk->emctwrhit2Mc),  "emctwrhit2Mc/S");
    embedMcRecoTrack->Branch("emcpid0Mc",     &(mctrk->emcpid0Mc),     "emcpid0Mc/S");
    embedMcRecoTrack->Branch("emcpid1Mc",     &(mctrk->emcpid1Mc),     "emcpid1Mc/S");
    embedMcRecoTrack->Branch("emcpid2Mc",     &(mctrk->emcpid2Mc),     "emcpid2Mc/S");
    embedMcRecoTrack->Branch("emcedep0Mc",    &(mctrk->emcedep0Mc),    "emcedep0Mc/F");
    embedMcRecoTrack->Branch("emcedep1Mc",    &(mctrk->emcedep1Mc),    "emcedep1Mc/F");
    embedMcRecoTrack->Branch("emcedep2Mc",    &(mctrk->emcedep2Mc),    "emcedep2Mc/F");
    embedMcRecoTrack->Branch("emcptot0Mc",    &(mctrk->emcptot0Mc),    "emcptot0Mc/F");
    embedMcRecoTrack->Branch("emcptot1Mc",    &(mctrk->emcptot1Mc),    "emcptot1Mc/F");
    embedMcRecoTrack->Branch("emcptot2Mc",    &(mctrk->emcptot2Mc),    "emcptot2Mc/F");
  
    //response information
    // 19 variables 
    embedMcRecoTrack->Branch("ntrkS",               &(mctrk->ntrkS),               "ntrkS/S");
    //embedMcRecoTrack->Branch("dctrkidS",            &(mctrk->dctrkidS),            "dctrkidS/S"); 
    embedMcRecoTrack->Branch("dctrkQualS",          &(mctrk->dctrkQualS),          "dctrkQualS/S");
    embedMcRecoTrack->Branch("the0S",               &(mctrk->the0S),               "the0S/F"); 
    embedMcRecoTrack->Branch("phi0S",               &(mctrk->phi0S),               "phi0S/F"); 
    embedMcRecoTrack->Branch("alphaS",              &(mctrk->alphaS),              "alphaS/F");
    embedMcRecoTrack->Branch("thetaS",              &(mctrk->thetaS),              "thetaS/F");
    embedMcRecoTrack->Branch("phiS",                &(mctrk->phiS),                "phiS/F"); 
    embedMcRecoTrack->Branch("betaS",               &(mctrk->betaS),               "betaS/F");
    embedMcRecoTrack->Branch("zedS",                &(mctrk->zedS),                "zedS/F"); 
    embedMcRecoTrack->Branch("dcchi2S",             &(mctrk->dcchi2S),             "dcchi2S/F");
    embedMcRecoTrack->Branch("momS",                &(mctrk->momS),                "momS/F"); 
    embedMcRecoTrack->Branch("ptS",                &(mctrk->ptS),                "ptS/F"); 
    //embedMcRecoTrack->Branch("x1mS",                &(mctrk->x1mS),                "x1mS/F"); 
    //embedMcRecoTrack->Branch("x2mS",                &(mctrk->x2mS),                "x2mS/F"); 
    embedMcRecoTrack->Branch("x1hS",                &(mctrk->x1hS),                "x1hS/S"); 
    embedMcRecoTrack->Branch("x2hS",                &(mctrk->x2hS),                "x2hS/S"); 
    //embedMcRecoTrack->Branch("uv1hS",               &(mctrk->uv1hS),               "uv1hS/S");
    //embedMcRecoTrack->Branch("uv2hS",               &(mctrk->uv2hS),               "uv2hS/S");
  
    //30variables
    //embedMcRecoTrack->Branch("pc1idS",            &(mctrk->pc1idS),            "pc1idS/S");
    //embedMcRecoTrack->Branch("pc2idS",            &(mctrk->pc2idS),            "pc2idS/S");
    //embedMcRecoTrack->Branch("pc3idS",            &(mctrk->pc3idS),            "pc3idS/S");
    //embedMcRecoTrack->Branch("tofidS",            &(mctrk->tofidS),            "tofidS/S");
    //embedMcRecoTrack->Branch("tecidS",            &(mctrk->tecidS),            "tecidS/S");
    //embedMcRecoTrack->Branch("emcidS",            &(mctrk->emcidS),            "emcidS/S");
    embedMcRecoTrack->Branch("pc1xS",             &(mctrk->pc1xS),            "pc1xS/F");
    embedMcRecoTrack->Branch("pc1yS",             &(mctrk->pc1yS),            "pc1yS/F");
    embedMcRecoTrack->Branch("pc1zS",             &(mctrk->pc1zS),            "pc1zS/F");
    embedMcRecoTrack->Branch("pc2xS",             &(mctrk->pc2xS),            "pc2xS/F");
    embedMcRecoTrack->Branch("pc2yS",             &(mctrk->pc2yS),            "pc2yS/F");
    embedMcRecoTrack->Branch("pc2zS",             &(mctrk->pc2zS),            "pc2zS/F");
    embedMcRecoTrack->Branch("pc3xS",             &(mctrk->pc3xS),            "pc3xS/F");
    embedMcRecoTrack->Branch("pc3yS",             &(mctrk->pc3yS),            "pc3yS/F");
    embedMcRecoTrack->Branch("pc3zS",             &(mctrk->pc3zS),            "pc3zS/F");

    //TOFE
    embedMcRecoTrack->Branch("tofxS",             &(mctrk->tofxS),            "tofxS/F");
    embedMcRecoTrack->Branch("tofyS",             &(mctrk->tofyS),            "tofyS/F");
    embedMcRecoTrack->Branch("tofzS",             &(mctrk->tofzS),            "tofzS/F");
    embedMcRecoTrack->Branch("toftS",             &(mctrk->toftS),            "toftS/F");
    embedMcRecoTrack->Branch("tofeS",             &(mctrk->tofeS),            "tofeS/F");
    embedMcRecoTrack->Branch("tofpidS",           &(mctrk->tofpidS),            "tofpidS/I");

    //TOFW
    embedMcRecoTrack->Branch("tofwstripS",         &(mctrk->tofwstripS),        "tofwstripS/I");
    embedMcRecoTrack->Branch("tofwxS",             &(mctrk->tofwxS),            "tofwxS/F");
    embedMcRecoTrack->Branch("tofwyS",             &(mctrk->tofwyS),            "tofwyS/F");
    embedMcRecoTrack->Branch("tofwzS",             &(mctrk->tofwzS),            "tofwzS/F");
    embedMcRecoTrack->Branch("tofwtS",             &(mctrk->tofwtS),            "tofwtS/F");
    embedMcRecoTrack->Branch("tofweS",             &(mctrk->tofweS),            "tofweS/F");
    embedMcRecoTrack->Branch("tofwpidS",           &(mctrk->tofwpidS),            "tofwpidS/I");

    embedMcRecoTrack->Branch("tecxinS",           &(mctrk->tecxinS),            "tecxinS/F");
    embedMcRecoTrack->Branch("tecyinS",           &(mctrk->tecyinS),            "tecyinS/F");
    embedMcRecoTrack->Branch("teczinS",           &(mctrk->teczinS),            "teczinS/F");
    embedMcRecoTrack->Branch("tecxoutS",          &(mctrk->tecxoutS),            "tecxoutS/F");
    embedMcRecoTrack->Branch("tecyoutS",          &(mctrk->tecyoutS),            "tecyoutS/F");
    embedMcRecoTrack->Branch("teczoutS",          &(mctrk->teczoutS),            "teczoutS/F");
    embedMcRecoTrack->Branch("emcxS",             &(mctrk->emcxS),            "emcxS/F");
    embedMcRecoTrack->Branch("emcyS",             &(mctrk->emcyS),            "emcyS/F");
    embedMcRecoTrack->Branch("emczS",             &(mctrk->emczS),            "emczS/F");

    //21 variables
    embedMcRecoTrack->Branch("ppc1xS",            &(mctrk->ppc1xS),            "ppc1xS/F");
    embedMcRecoTrack->Branch("ppc1yS",            &(mctrk->ppc1yS),            "ppc1yS/F");
    embedMcRecoTrack->Branch("ppc1zS",            &(mctrk->ppc1zS),            "ppc1zS/F");
    embedMcRecoTrack->Branch("ppc2xS",            &(mctrk->ppc2xS),            "ppc2xS/F");
    embedMcRecoTrack->Branch("ppc2yS",            &(mctrk->ppc2yS),            "ppc2yS/F");
    embedMcRecoTrack->Branch("ppc2zS",            &(mctrk->ppc2zS),            "ppc2zS/F");
    embedMcRecoTrack->Branch("ppc3xS",            &(mctrk->ppc3xS),            "ppc3xS/F");
    embedMcRecoTrack->Branch("ppc3yS",            &(mctrk->ppc3yS),            "ppc3yS/F");
    embedMcRecoTrack->Branch("ppc3zS",            &(mctrk->ppc3zS),            "ppc3zS/F");

    //TOFE
    embedMcRecoTrack->Branch("ptofxS",            &(mctrk->ptofxS),            "ptofxS/F");
    embedMcRecoTrack->Branch("ptofyS",            &(mctrk->ptofyS),            "ptofyS/F");
    embedMcRecoTrack->Branch("ptofzS",            &(mctrk->ptofzS),            "ptofzS/F");
    embedMcRecoTrack->Branch("pltofS",            &(mctrk->pltofS),            "pltofS/F");

    //TOFW
    embedMcRecoTrack->Branch("ptofwxS",            &(mctrk->ptofwxS),            "ptofwxS/F");
    embedMcRecoTrack->Branch("ptofwyS",            &(mctrk->ptofwyS),            "ptofwyS/F");
    embedMcRecoTrack->Branch("ptofwzS",            &(mctrk->ptofwzS),            "ptofwzS/F");
    embedMcRecoTrack->Branch("pltofwS",            &(mctrk->pltofwS),            "pltofwS/F");

    embedMcRecoTrack->Branch("ptecxS",            &(mctrk->ptecxS),            "ptecxS/F");
    embedMcRecoTrack->Branch("ptecyS",            &(mctrk->ptecyS),            "ptecyS/F");
    embedMcRecoTrack->Branch("pteczS",            &(mctrk->pteczS),            "pteczS/F");
    embedMcRecoTrack->Branch("pemcxS",            &(mctrk->pemcxS),            "pemcxS/F");
    embedMcRecoTrack->Branch("pemcyS",            &(mctrk->pemcyS),            "pemcyS/F");
    embedMcRecoTrack->Branch("pemczS",            &(mctrk->pemczS),            "pemczS/F");
    embedMcRecoTrack->Branch("plcrkS",            &(mctrk->plcrkS),            "plcrkS/F");
    embedMcRecoTrack->Branch("plemcS",            &(mctrk->plemcS),            "plemcS/F");

    embedMcRecoTrack->Branch("aerboxidS",           &(mctrk->aerboxidS),         "aerboxidS/S");
    embedMcRecoTrack->Branch("aerph1S",             &(mctrk->aerph1S),           "aerph1S/F");
    embedMcRecoTrack->Branch("aerph2S",             &(mctrk->aerph2S),           "aerph2S/F");

    // 29 variables
    //embedMcRecoTrack->Branch("nx1x2fitS",            &(mctrk->nx1x2fitS),          "nx1x2fitS/S");
    //embedMcRecoTrack->Branch("mchi2S",               &(mctrk->mchi2S),             "mchi2S/F");
    //embedMcRecoTrack->Branch("errS",                 &(mctrk->errS),               "errS/S");          
    //embedMcRecoTrack->Branch("alphafS",              &(mctrk->alphafS),            "alphafS/F");          
    embedMcRecoTrack->Branch("crkaccS",              &(mctrk->crkaccS),            "crkaccS/S");          
    embedMcRecoTrack->Branch("crknpmt0S",            &(mctrk->crknpmt0S),          "crknpmt0S/S");          
    embedMcRecoTrack->Branch("crknpmt1S",            &(mctrk->crknpmt1S),          "crknpmt1S/S");          
    embedMcRecoTrack->Branch("crknpmt3S",            &(mctrk->crknpmt3S),          "crknpmt3S/S");          
    embedMcRecoTrack->Branch("crknpe0S",             &(mctrk->crknpe0S),           "crknpe0S/F");          
    embedMcRecoTrack->Branch("crknpe1S",             &(mctrk->crknpe1S),           "crknpe1S/F");          
    embedMcRecoTrack->Branch("crknpe3S",             &(mctrk->crknpe3S),           "crknpe3S/F");          
    embedMcRecoTrack->Branch("crkchi2S",             &(mctrk->crkchi2S),           "crkchi2S/F");          
    embedMcRecoTrack->Branch("crkdispS",             &(mctrk->crkdispS),           "crkdispS/F");          
    embedMcRecoTrack->Branch("crkpathS",             &(mctrk->crkpathS),           "crkpathS/F");          
    embedMcRecoTrack->Branch("emcswkeyS",            &(mctrk->emcswkeyS),          "emcswkeyS/S");          
    embedMcRecoTrack->Branch("emcmeaseS",            &(mctrk->emcmeaseS),          "emcmeaseS/F");          
    embedMcRecoTrack->Branch("emcecoreS",            &(mctrk->emcecoreS),          "emcecoreS/F");          
    embedMcRecoTrack->Branch("emcecentS",            &(mctrk->emcecentS),          "emcecentS/F");          
    //embedMcRecoTrack->Branch("emcecorrS",            &(mctrk->emcecorrS),          "emcecorrS/F");          
    embedMcRecoTrack->Branch("emctofS",              &(mctrk->emctofS),            "emctofS/F");          
    embedMcRecoTrack->Branch("emctofcorrS",          &(mctrk->emctofcorrS),        "emctofcorrS/F");          
    embedMcRecoTrack->Branch("emctofminS",           &(mctrk->emctofminS),         "emctofminS/F");          
    embedMcRecoTrack->Branch("emcprobphotS",         &(mctrk->emcprobphotS),       "emcprobphotS/F");          
    embedMcRecoTrack->Branch("emctwrhitS",           &(mctrk->emctwrhitS),         "emctwrhitS/S");          
    embedMcRecoTrack->Branch("emcchi2S",             &(mctrk->emcchi2S),           "emcchi2S/F");          
    embedMcRecoTrack->Branch("emcpartesum0S",            &(mctrk->emcpartesum0S),            "emcpartesum0S/F");          
    embedMcRecoTrack->Branch("emcpartesum1S",            &(mctrk->emcpartesum1S),            "emcpartesum1S/F");          
    embedMcRecoTrack->Branch("emcpartesum2S",            &(mctrk->emcpartesum2S),            "emcpartesum2S/F");          
    embedMcRecoTrack->Branch("emcpartesum3S",            &(mctrk->emcpartesum3S),            "emcpartesum3S/F");          
  
    //information to keep after embedding 
    // if correct association then  xidE == xidR
    //10 variables
    //embedMcRecoTrack->Branch("x1hE",            &(mctrk->x1hE),            "x1hE/S");          
    //embedMcRecoTrack->Branch("x2hE",            &(mctrk->x2hE),            "x2hE/S");          
    //embedMcRecoTrack->Branch("uv1hE",            &(mctrk->uv1hE),            "uv1hE/S");          
    //embedMcRecoTrack->Branch("uv2hE",            &(mctrk->uv2hE),            "uv2hE/S");          
    embedMcRecoTrack->Branch("pc1idE",            &(mctrk->pc1idE),            "pc1idE/S");          
    embedMcRecoTrack->Branch("pc2idE",            &(mctrk->pc2idE),            "pc2idE/S");          
    embedMcRecoTrack->Branch("pc3idE",            &(mctrk->pc3idE),            "pc3idE/S");          
    //embedMcRecoTrack->Branch("tofidE",            &(mctrk->tofidE),            "tofidE/S");          
    //embedMcRecoTrack->Branch("tecidE",            &(mctrk->tecidE),            "tecidE/S");          
    //embedMcRecoTrack->Branch("emcidE",            &(mctrk->emcidE),            "emcidE/S");          
                 
    //reconstruction information         
    //20 variables               
    embedMcRecoTrack->Branch("ntrkb",            &(mctrk->ntrkb),            "ntrkb/S");          
    embedMcRecoTrack->Branch("ntrka",            &(mctrk->ntrka),            "ntrka/S");          
    //embedMcRecoTrack->Branch("dctrkidR",            &(mctrk->dctrkidR),            "dctrkidR/S");          
    embedMcRecoTrack->Branch("dctrkQualR",            &(mctrk->dctrkQualR),            "dctrkQualR/S");          
    embedMcRecoTrack->Branch("the0R",            &(mctrk->the0R),            "the0R/F");          
    embedMcRecoTrack->Branch("phi0R",            &(mctrk->phi0R),            "phi0R/F");          
    embedMcRecoTrack->Branch("alphaR",            &(mctrk->alphaR),            "alphaR/F");          
    embedMcRecoTrack->Branch("thetaR",            &(mctrk->thetaR),            "thetaR/F");          
    embedMcRecoTrack->Branch("phiR",            &(mctrk->phiR),            "phiR/F");          
    embedMcRecoTrack->Branch("betaR",            &(mctrk->betaR),            "betaR/F");          
    embedMcRecoTrack->Branch("zedR",            &(mctrk->zedR),            "zedR/F");          
    embedMcRecoTrack->Branch("momR",            &(mctrk->momR),            "momR/F");          
    embedMcRecoTrack->Branch("ptR",            &(mctrk->ptR),            "ptR/F");          
    //embedMcRecoTrack->Branch("dcchi2R",            &(mctrk->dcchi2R),            "dcchi2R/F");          
    //embedMcRecoTrack->Branch("x1mR",            &(mctrk->x1mR),            "x1mR/F");          
    //embedMcRecoTrack->Branch("x2mR",            &(mctrk->x2mR),            "x2mR/F");          
    embedMcRecoTrack->Branch("x1hR",            &(mctrk->x1hR),            "x1hR/S");          
    embedMcRecoTrack->Branch("x2hR",            &(mctrk->x2hR),            "x2hR/S");          
    //embedMcRecoTrack->Branch("uv1hR",            &(mctrk->uv1hR),            "uv1hR/S");          
    //embedMcRecoTrack->Branch("uv2hR",            &(mctrk->uv2hR),            "uv2hR/S");          
    //30 variables               
    embedMcRecoTrack->Branch("pc1idR",            &(mctrk->pc1idR),            "pc1idR/S");          
    embedMcRecoTrack->Branch("pc2idR",            &(mctrk->pc2idR),            "pc2idR/S");          
    embedMcRecoTrack->Branch("pc3idR",            &(mctrk->pc3idR),            "pc3idR/S");          
    //embedMcRecoTrack->Branch("tofidR",            &(mctrk->tofidR),            "tofidR/S");          
    //embedMcRecoTrack->Branch("tecidR",            &(mctrk->tecidR),            "tecidR/S");          
    //embedMcRecoTrack->Branch("emcidR",            &(mctrk->emcidR),            "emcidR/S");          
    embedMcRecoTrack->Branch("pc1xR",            &(mctrk->pc1xR),            "pc1xR/F");          
    embedMcRecoTrack->Branch("pc1yR",            &(mctrk->pc1yR),            "pc1yR/F");          
    embedMcRecoTrack->Branch("pc1zR",            &(mctrk->pc1zR),            "pc1zR/F");          
    embedMcRecoTrack->Branch("pc2xR",            &(mctrk->pc2xR),            "pc2xR/F");          
    embedMcRecoTrack->Branch("pc2yR",            &(mctrk->pc2yR),            "pc2yR/F");          
    embedMcRecoTrack->Branch("pc2zR",            &(mctrk->pc2zR),            "pc2zR/F");          
    embedMcRecoTrack->Branch("pc3xR",            &(mctrk->pc3xR),            "pc3xR/F");          
    embedMcRecoTrack->Branch("pc3yR",            &(mctrk->pc3yR),            "pc3yR/F");          
    embedMcRecoTrack->Branch("pc3zR",            &(mctrk->pc3zR),            "pc3zR/F");          

    //TOFE
    embedMcRecoTrack->Branch("tofxR",            &(mctrk->tofxR),            "tofxR/F");          
    embedMcRecoTrack->Branch("tofyR",            &(mctrk->tofyR),            "tofyR/F");          
    embedMcRecoTrack->Branch("tofzR",            &(mctrk->tofzR),            "tofzR/F");          
    embedMcRecoTrack->Branch("toftR",            &(mctrk->toftR),            "toftR/F");          
    embedMcRecoTrack->Branch("tofeR",            &(mctrk->tofeR),            "tofeR/F");          
    embedMcRecoTrack->Branch("tofpidR",            &(mctrk->tofpidR),            "tofpidR/I");          

    //TOFW
    embedMcRecoTrack->Branch("tofwstripR",        &(mctrk->tofwstripR),        "tofwstripR/I");
    embedMcRecoTrack->Branch("tofwxR",            &(mctrk->tofwxR),            "tofwxR/F");          
    embedMcRecoTrack->Branch("tofwyR",            &(mctrk->tofwyR),            "tofwyR/F");          
    embedMcRecoTrack->Branch("tofwzR",            &(mctrk->tofwzR),            "tofwzR/F");          
    embedMcRecoTrack->Branch("tofwtR",            &(mctrk->tofwtR),            "tofwtR/F");          
    embedMcRecoTrack->Branch("tofweR",            &(mctrk->tofweR),            "tofweR/F");          
    embedMcRecoTrack->Branch("tofwpidR",            &(mctrk->tofwpidR),            "tofwpidR/I");          

    embedMcRecoTrack->Branch("tecxinR",            &(mctrk->tecxinR),            "tecxinR/F");          
    embedMcRecoTrack->Branch("tecyinR",            &(mctrk->tecyinR),            "tecyinR/F");          
    embedMcRecoTrack->Branch("teczinR",            &(mctrk->teczinR),            "teczinR/F");          
    embedMcRecoTrack->Branch("tecxoutR",            &(mctrk->tecxoutR),            "tecxoutR/F");          
    embedMcRecoTrack->Branch("tecyoutR",            &(mctrk->tecyoutR),            "tecyoutR/F");          
    embedMcRecoTrack->Branch("teczoutR",            &(mctrk->teczoutR),            "teczoutR/F");          
    embedMcRecoTrack->Branch("emcxR",            &(mctrk->emcxR),            "emcxR/F");          
    embedMcRecoTrack->Branch("emcyR",            &(mctrk->emcyR),            "emcyR/F");          
    embedMcRecoTrack->Branch("emczR",            &(mctrk->emczR),            "emczR/F");          
               
    //27 variables               
    embedMcRecoTrack->Branch("ppc1xR",            &(mctrk->ppc1xR),            "ppc1xR/F");          
    embedMcRecoTrack->Branch("ppc1yR",            &(mctrk->ppc1yR),            "ppc1yR/F");          
    embedMcRecoTrack->Branch("ppc1zR",            &(mctrk->ppc1zR),            "ppc1zR/F");          
    embedMcRecoTrack->Branch("ppc2xR",            &(mctrk->ppc2xR),            "ppc2xR/F");          
    embedMcRecoTrack->Branch("ppc2yR",            &(mctrk->ppc2yR),            "ppc2yR/F");          
    embedMcRecoTrack->Branch("ppc2zR",            &(mctrk->ppc2zR),            "ppc2zR/F");          
    embedMcRecoTrack->Branch("ppc3xR",            &(mctrk->ppc3xR),            "ppc3xR/F");          
    embedMcRecoTrack->Branch("ppc3yR",            &(mctrk->ppc3yR),            "ppc3yR/F");          
    embedMcRecoTrack->Branch("ppc3zR",            &(mctrk->ppc3zR),            "ppc3zR/F");          

    //TOFE
    embedMcRecoTrack->Branch("ptofxR",            &(mctrk->ptofxR),            "ptofxR/F");          
    embedMcRecoTrack->Branch("ptofyR",            &(mctrk->ptofyR),            "ptofyR/F");          
    embedMcRecoTrack->Branch("ptofzR",            &(mctrk->ptofzR),            "ptofzR/F");          
    embedMcRecoTrack->Branch("pltofR",            &(mctrk->pltofR),            "pltofR/F");          

    //TOFW
    embedMcRecoTrack->Branch("ptofwxR",            &(mctrk->ptofwxR),            "ptofwxR/F");          
    embedMcRecoTrack->Branch("ptofwyR",            &(mctrk->ptofwyR),            "ptofwyR/F");          
    embedMcRecoTrack->Branch("ptofwzR",            &(mctrk->ptofwzR),            "ptofwzR/F");          
    embedMcRecoTrack->Branch("pltofwR",            &(mctrk->pltofwR),            "pltofwR/F");          

    embedMcRecoTrack->Branch("ptecxR",            &(mctrk->ptecxR),            "ptecxR/F");          
    embedMcRecoTrack->Branch("ptecyR",            &(mctrk->ptecyR),            "ptecyR/F");          
    embedMcRecoTrack->Branch("pteczR",            &(mctrk->pteczR),            "pteczR/F");          
    embedMcRecoTrack->Branch("pemcxR",            &(mctrk->pemcxR),            "pemcxR/F");          
    embedMcRecoTrack->Branch("pemcyR",            &(mctrk->pemcyR),            "pemcyR/F");          
    embedMcRecoTrack->Branch("pemczR",            &(mctrk->pemczR),            "pemczR/F");          
    embedMcRecoTrack->Branch("tecdin",            &(mctrk->tecdin),            "tecdin/F");    
    embedMcRecoTrack->Branch("tecdout",           &(mctrk->tecdout),           "tecdout/F");    
    embedMcRecoTrack->Branch("plcrkR",            &(mctrk->plcrkR),            "plcrkR/F");          
    embedMcRecoTrack->Branch("plemcR",            &(mctrk->plemcR),            "plemcR/F");          

    embedMcRecoTrack->Branch("aerboxidR",             &(mctrk->aerboxidR),         "aerboxidR/S");
    embedMcRecoTrack->Branch("aerph1R",               &(mctrk->aerph1R),           "aerph1R/F");
    embedMcRecoTrack->Branch("aerph2R",               &(mctrk->aerph2R),           "aerph2R/F");

    //29 variables

    //embedMcRecoTrack->Branch("nx1x2fitR",            &(mctrk->nx1x2fitR),            "nx1x2fitR/S");          
    //embedMcRecoTrack->Branch("mchi2R",             &(mctrk->mchi2R),            "mchi2R/F");          
    //embedMcRecoTrack->Branch("errR",            &(mctrk->errR),            "errR/S");          
    //embedMcRecoTrack->Branch("alphafR",            &(mctrk->alphafR),            "alphafR/F");          
    embedMcRecoTrack->Branch("crkaccR",            &(mctrk->crkaccR),            "crkaccR/S");          
    embedMcRecoTrack->Branch("crknpmt0R",            &(mctrk->crknpmt0R),            "crknpmt0R/S");          
    embedMcRecoTrack->Branch("crknpmt1R",            &(mctrk->crknpmt1R),            "crknpmt1R/S");          
    embedMcRecoTrack->Branch("crknpmt3R",            &(mctrk->crknpmt3R),            "crknpmt3R/S");          
    embedMcRecoTrack->Branch("crknpe0R",            &(mctrk->crknpe0R),            "crknpe0R/F");          
    embedMcRecoTrack->Branch("crknpe1R",            &(mctrk->crknpe1R),            "crknpe1R/F");          
    embedMcRecoTrack->Branch("crknpe3R",            &(mctrk->crknpe3R),            "crknpe3R/F");          
    embedMcRecoTrack->Branch("crkchi2R",            &(mctrk->crkchi2R),            "crkchi2R/F");          
    embedMcRecoTrack->Branch("crkdispR",            &(mctrk->crkdispR),            "crkdispR/F");          
    embedMcRecoTrack->Branch("crkpathR",            &(mctrk->crkpathR),            "crkpathR/F");          
    embedMcRecoTrack->Branch("emcswkeyR",            &(mctrk->emcswkeyR),            "emcswkeyR/S");          
    embedMcRecoTrack->Branch("emcmeaseR",            &(mctrk->emcmeaseR),            "emcmeaseR/F");          
    embedMcRecoTrack->Branch("emcecoreR",            &(mctrk->emcecoreR),            "emcecoreR/F");          
    embedMcRecoTrack->Branch("emcecentR",            &(mctrk->emcecentR),            "emcecentR/F");          
    embedMcRecoTrack->Branch("emcecorrR",            &(mctrk->emcecorrR),            "emcecorrR/F");          
    embedMcRecoTrack->Branch("emctofR",            &(mctrk->emctofR),            "emctofR/F");          
    embedMcRecoTrack->Branch("emctofcorrR",            &(mctrk->emctofcorrR),            "emctofcorrR/F");          
    embedMcRecoTrack->Branch("emctofminR",            &(mctrk->emctofminR),            "emctofminR/F");          
    embedMcRecoTrack->Branch("emcprobphotR",            &(mctrk->emcprobphotR),            "emcprobphotR/F");          
    embedMcRecoTrack->Branch("emctwrhitR",            &(mctrk->emctwrhitR),            "emctwrhitR/S");          
    embedMcRecoTrack->Branch("emcchi2R",            &(mctrk->emcchi2R),            "emcchi2R");
    embedMcRecoTrack->Branch("emcpartesum0R",            &(mctrk->emcpartesum0R),            "emcpartesum0R/F");          
    embedMcRecoTrack->Branch("emcpartesum1R",            &(mctrk->emcpartesum1R),            "emcpartesum1R/F");          
    embedMcRecoTrack->Branch("emcpartesum2R",            &(mctrk->emcpartesum2R),            "emcpartesum2R/F");          
    embedMcRecoTrack->Branch("emcpartesum3R",            &(mctrk->emcpartesum3R),            "emcpartesum3R/F");          
  
    // fields for main contributor analysis or other efficiency study
    //24 variables
    embedMcRecoTrack->Branch("sumfound",            &(mctrk->sumfound),          "sumfound/S");          
    embedMcRecoTrack->Branch("solution",            &(mctrk->solution),          "solution/S");          
    embedMcRecoTrack->Branch("mulmainS",            &(mctrk->mulmainS),          "mulmainS/S");          
    embedMcRecoTrack->Branch("xmulmainS",           &(mctrk->xmulmainS),         "xmulmainS/S");          
    embedMcRecoTrack->Branch("uvmulmainS",          &(mctrk->uvmulmainS),        "uvmulmainS/S");          
    //embedMcRecoTrack->Branch("mainIDS",             &(mctrk->mainIDS),           "mainIDS/S");          
    //embedMcRecoTrack->Branch("xmainIDS",            &(mctrk->xmainIDS),          "xmainIDS/S");          
    //embedMcRecoTrack->Branch("uvmainIDS",           &(mctrk->uvmainIDS),         "uvmainIDS/S");          
    embedMcRecoTrack->Branch("purityS",             &(mctrk->purityS),           "purityS/F");          
    embedMcRecoTrack->Branch("xpurityS",            &(mctrk->xpurityS),          "xpurityS/F");          
    embedMcRecoTrack->Branch("uvpurityS",           &(mctrk->uvpurityS),         "uvpurityS/F");          
    embedMcRecoTrack->Branch("sumfoundS",           &(mctrk->sumfoundS),         "sumfoundS/S");          
    embedMcRecoTrack->Branch("solutionS  ",         &(mctrk->solutionS),         "solutionS/S");          
    embedMcRecoTrack->Branch("mulmainR",            &(mctrk->mulmainR),          "mulmainR/S");          
    embedMcRecoTrack->Branch("xmulmainR",           &(mctrk->xmulmainR),         "xmulmainR/S");          
    embedMcRecoTrack->Branch("uvmulmainR",          &(mctrk->uvmulmainR),        "uvmulmainR/S");          
    //embedMcRecoTrack->Branch("mainIDR",             &(mctrk->mainIDR),           "mainIDR/S");          
    //embedMcRecoTrack->Branch("xmainIDR",            &(mctrk->xmainIDR),          "xmainIDR/S");          
    //embedMcRecoTrack->Branch("uvmainIDR",           &(mctrk->uvmainIDR),         "uvmainIDR/S");          
    embedMcRecoTrack->Branch("purityR",             &(mctrk->purityR),           "purityR/F");          
    embedMcRecoTrack->Branch("xpurityR",            &(mctrk->xpurityR),          "xpurityR/F");          
    embedMcRecoTrack->Branch("uvpurityR",           &(mctrk->uvpurityR),         "uvpurityR/F");          
    embedMcRecoTrack->Branch("sumfoundR",           &(mctrk->sumfoundR),         "sumfoundR/S");          
    embedMcRecoTrack->Branch("solutionR  ",         &(mctrk->solutionR),         "solutionR/S");          
                         
                         
    //matching variables                       
    //40 variables                       
    //embedMcRecoTrack->Branch("pc2sS",              &(mctrk->pc2sS),              "pc2sS/F");          
    embedMcRecoTrack->Branch("pc2sdphiS",          &(mctrk->pc2sdphiS),          "pc2sdphiS/F");          
    embedMcRecoTrack->Branch("pc2sdzS",            &(mctrk->pc2sdzS),            "pc2sdzS/F");          
    embedMcRecoTrack->Branch("pc2dphiS",           &(mctrk->pc2dphiS),           "pc2dphiS/F");          
    embedMcRecoTrack->Branch("pc2dzS",             &(mctrk->pc2dzS),             "pc2dzS/F");          
    //embedMcRecoTrack->Branch("pc3sS",              &(mctrk->pc3sS),              "pc3sS/F");          
    embedMcRecoTrack->Branch("pc3sdphiS",          &(mctrk->pc3sdphiS),          "pc3sdphiS/F");          
    embedMcRecoTrack->Branch("pc3sdzS",            &(mctrk->pc3sdzS),            "pc3sdzS/F");          
    embedMcRecoTrack->Branch("pc3dphiS",           &(mctrk->pc3dphiS),           "pc3dphiS/F");          
    embedMcRecoTrack->Branch("pc3dzS",             &(mctrk->pc3dzS),             "pc3dzS/F");          
    //TOFE
    //embedMcRecoTrack->Branch("tofsS",              &(mctrk->tofsS),              "tofsS/F");          
    embedMcRecoTrack->Branch("tofsdphiS",          &(mctrk->tofsdphiS),          "tofsdphiS/F");          
    embedMcRecoTrack->Branch("tofsdzS",            &(mctrk->tofsdzS),            "tofsdzS/F");          
    embedMcRecoTrack->Branch("tofdphiS",           &(mctrk->tofdphiS),           "tofdphiS/F");          
    embedMcRecoTrack->Branch("tofdzS",             &(mctrk->tofdzS),             "tofdzS/F");          
    //TOFW
    //embedMcRecoTrack->Branch("tofwsS",              &(mctrk->tofwsS),              "tofwsS/F");          
    embedMcRecoTrack->Branch("tofwsdphiS",          &(mctrk->tofwsdphiS),          "tofwsdphiS/F");          
    embedMcRecoTrack->Branch("tofwsdzS",            &(mctrk->tofwsdzS),            "tofwsdzS/F");          
    embedMcRecoTrack->Branch("tofwdphiS",           &(mctrk->tofwdphiS),           "tofwdphiS/F");          
    embedMcRecoTrack->Branch("tofwdzS",             &(mctrk->tofwdzS),             "tofwdzS/F");          
    //embedMcRecoTrack->Branch("emcsS",              &(mctrk->emcsS),              "emcsS/F");          
    embedMcRecoTrack->Branch("emcsdphiS",          &(mctrk->emcsdphiS),          "emcsdphiS/F");          
    embedMcRecoTrack->Branch("emcsdzS",            &(mctrk->emcsdzS),            "emcsdzS/F");          
    embedMcRecoTrack->Branch("emcdphiS",           &(mctrk->emcdphiS),           "emcdphiS/F");          
    embedMcRecoTrack->Branch("emcdzS",             &(mctrk->emcdzS),             "emcdzS/F");          
                           
    //embedMcRecoTrack->Branch("pc2sR",              &(mctrk->pc2sR),              "pc2sR/F");          
    embedMcRecoTrack->Branch("pc2sdphiR",          &(mctrk->pc2sdphiR),          "pc2sdphiR/F");          
    embedMcRecoTrack->Branch("pc2sdzR",            &(mctrk->pc2sdzR),            "pc2sdzR/F");          
    embedMcRecoTrack->Branch("pc2dphiR",           &(mctrk->pc2dphiR),           "pc2dphiR/F");          
    embedMcRecoTrack->Branch("pc2dzR",             &(mctrk->pc2dzR),             "pc2dzR/F");          
    //embedMcRecoTrack->Branch("pc3sR",              &(mctrk->pc3sR),              "pc3sR/F");          
    embedMcRecoTrack->Branch("pc3sdphiR",          &(mctrk->pc3sdphiR),          "pc3sdphiR/F");          
    embedMcRecoTrack->Branch("pc3sdzR",            &(mctrk->pc3sdzR),            "pc3sdzR/F");          
    embedMcRecoTrack->Branch("pc3dphiR",           &(mctrk->pc3dphiR),           "pc3dphiR/F");          
    embedMcRecoTrack->Branch("pc3dzR",             &(mctrk->pc3dzR),             "pc3dzR/F");          
    //TOFE
    //embedMcRecoTrack->Branch("tofsR",              &(mctrk->tofsR),              "tofsR/F");          
    embedMcRecoTrack->Branch("tofsdphiR",          &(mctrk->tofsdphiR),          "tofsdphiR/F");          
    embedMcRecoTrack->Branch("tofsdzR",            &(mctrk->tofsdzR),            "tofsdzR/F");          
    embedMcRecoTrack->Branch("tofdphiR",           &(mctrk->tofdphiR),           "tofdphiR/F");          
    embedMcRecoTrack->Branch("tofdzR",             &(mctrk->tofdzR),             "tofdzR/F");          
    //TOFW
    //embedMcRecoTrack->Branch("tofwsR",              &(mctrk->tofwsR),              "tofwsR/F");          
    embedMcRecoTrack->Branch("tofwsdphiR",          &(mctrk->tofwsdphiR),          "tofwsdphiR/F");          
    embedMcRecoTrack->Branch("tofwsdzR",            &(mctrk->tofwsdzR),            "tofwsdzR/F");          
    embedMcRecoTrack->Branch("tofwdphiR",           &(mctrk->tofwdphiR),           "tofwdphiR/F");          
    embedMcRecoTrack->Branch("tofwdzR",             &(mctrk->tofwdzR),             "tofwdzR/F");          
    //embedMcRecoTrack->Branch("emcsR",              &(mctrk->emcsR),              "emcsR/F");          
    embedMcRecoTrack->Branch("emcsdphiR",          &(mctrk->emcsdphiR),          "emcsdphiR/F");          
    embedMcRecoTrack->Branch("emcsdzR",            &(mctrk->emcsdzR),            "emcsdzR/F");          
    embedMcRecoTrack->Branch("emcdphiR",           &(mctrk->emcdphiR),           "emcdphiR/F");          
    embedMcRecoTrack->Branch("emcdzR",             &(mctrk->emcdzR),             "emcdzR/F");          
    //relate number of hits
    //9 variables
    embedMcRecoTrack->Branch("x1hSG",            &(mctrk->x1hSG),          "x1hSG/S");        
    embedMcRecoTrack->Branch("x2hSG",            &(mctrk->x2hSG),          "x2hSG/S");        
    embedMcRecoTrack->Branch("uvhSG",            &(mctrk->uvhSG),          "uvhSG/S");        
    embedMcRecoTrack->Branch("x1hRG",            &(mctrk->x1hRG),          "x1hRG/S");        
    embedMcRecoTrack->Branch("x2hRG",            &(mctrk->x2hRG),          "x2hRG/S");        
    embedMcRecoTrack->Branch("uvhRG",            &(mctrk->uvhRG),          "uvhRG/S");        
    embedMcRecoTrack->Branch("x1hRS",            &(mctrk->x1hRS),          "x1hRS/S");        
    embedMcRecoTrack->Branch("x2hRS",            &(mctrk->x2hRS),          "x2hRS/S");        
    embedMcRecoTrack->Branch("uvhRS",            &(mctrk->uvhRS),          "uvhRS/S");        

    addNtuple(embedMcRecoTrack);
    if(tmp)tmp->cd();

  } // end of creating the TTree if it did not already exist

  if(track){

    *mctrk = *track;
    //track->fillEvaluation(array);
    embedMcRecoTrack->Fill();

  }
  return True;
}
