//#define MPCCAL_DO_CLUSTERING
#define MPCCAL_DO_FIT

#include <MpcPi0Cal.h>




#include <TSystem.h>
#include <TCanvas.h>
#include <TGraphErrors.h>
#include <TVector3.h>
#include <TMath.h>
#include <TH1.h>
#include <TH2.h>
#if ROOT_VERSION_CODE >= ROOT_VERSION(5,15,8) 
#include <TMCParticle.h>
#else
#include <TMCParticle6.h>
#endif

#include <PHObject.h>
#include <MpcMap.h>
#include <mpcTowerContainer.h>
#include <mpcTowerContent.h>
#include <mpcClusterContainer.h>
#include <mpcClusterContent.h>
#include <PHPythiaContainer.h>
#include <RunHeader.h>
#include <Fun4AllServer.h>

#include <cassert>
#include <cmath>
#include <iostream>
#include <fstream>
#include <string>

using namespace std;
using namespace findNode;

int statusmax = 11;
TString statusfile;
TString tempfile;
TString recaldir;
TString groupdir;
int groupnum;
int rungroup_lo[200];
int rungroup_hi[200];
int nrungroups = 0;
int status = 0;


extern TSystem* gSystem;

TFile* fadd[100];
int faddlength = 0;


int MpcPi0Cal::init_files(){
  TString cp_cmd = "cp "; cp_cmd+=statusfile; cp_cmd+= " "; cp_cmd+=tempfile;
  gSystem->Exec( cp_cmd.Data() );
  std::ifstream din(tempfile.Data());
  int nentries = 0;
  while(din){
    std::string temp = "hello";
    din >> temp;
    if(temp == "hello"){ std::cout << "done initfiles: " << nentries << endl;break;}
    if( (fadd[nentries] = TFile::Open(temp.c_str())) ){
      nentries++;
    }
  }
  faddlength = nentries;
  din.close();
  return nentries;

}


int MpcPi0Cal::add_histos(){
  for(int impc=0;impc<2;impc++)
    for(int ix=0;ix<18;ix++)
      for(int iy=0;iy<18;iy++){
	TH1* h = pi0mass_tower[impc][ix][iy]->GetHist();
	TH1* hbkgd = pi0mass_tower[impc][ix][iy]->GetHistBkgd();
	TString fgname = h->GetName();
	TString bgname = hbkgd->GetName();
	for(int ifile=0;ifile<faddlength;ifile++){
	  fadd[ifile]->cd();
	  TH1* hadd = (TH1D*) fadd[ifile]->Get(fgname.Data());
	  TH1* haddbkgd = (TH1D*) fadd[ifile]->Get(bgname.Data());
	  h->Add(hadd);
	  hbkgd->Add(haddbkgd);
	}
      }
  return 1;
}

    


void MpcPi0Cal::init_groups(){
 
  std::ifstream din;
  if(daflag) din.open("/direct/phenix+hp/data06/bmeredi2_taxi/run8dAuPi0Cal/fillinfo/rungroups.txt");
  else din.open("/direct/phenix+hp/data06/bmeredi2_taxi/run8ppPi0Cal/fillinfo/rungroups.txt");
  int itr = 0;
  while(din){
    int temp = -1;
    din >> temp;
    if(temp <0) break;
    
    rungroup_lo[itr] = temp;
    if(itr >0) rungroup_hi[itr-1] = rungroup_lo[itr];
    itr++;
  }
  nrungroups = itr;
  rungroup_hi[itr-1] = 260000;

}


int MpcPi0Cal::get_group(int myrun){
  if(nrungroups == 0){ init_groups();}
  int group = -1;
  for(int itr=0;itr<nrungroups;itr++){
    if(myrun >= rungroup_lo[itr] && myrun < rungroup_hi[itr]){
      group = itr;
      break;
    }
  }
  return group;
}


int MpcPi0Cal::get_status(){
  //cout << "begin get_status\n";
  TString cp_cmd = "cp "; cp_cmd+=statusfile; cp_cmd+= " "; cp_cmd+=tempfile;
  gSystem->Exec( cp_cmd.Data() );
  //cout << "after cp\n";
  std::ifstream din(tempfile.Data());
  //cout << "after init din\n";
  int nentries = 0;
  while(din){
    std::string temp = "hello";
    din >> temp;
    //cout << "temp is : " << temp << endl;
    if(temp == "hello"){ break;}
    nentries++;
  }
  din.close();
  return nentries;

}
      
void MpcPi0Cal::write_status(){
  std::ofstream dout(statusfile.Data(),ios_base::app);
  dout << rootfile << "\n";
  return;
}    


MpcPi0Cal::MpcPi0Cal(char *out_root_file, int iteration, int run)
{
  ThisName = "MPCDSTANA";
  rootfile = string(out_root_file);

  runnum = run;
  groupnum = get_group(runnum);
  std::cout << std::endl;
  std::cout << "MpcPi0Cal::MpcPi0Cal ... start" << std::endl;
  std::cout << "run number = " << runnum << std::endl;
  std::cout << "group number = " << groupnum << std::endl;
  std::cout << " output root file is " << rootfile << std::endl;

  set_iteration(iteration);

  extern TStyle* gStyle;
  m_style = gStyle;
  m_style->SetOptFit();
  
  // make output directories
/*
  TString mkdir_cmd = out_root_file;
  mkdir_cmd.ReplaceAll(".root","");
  TString temp_dirname = mkdir_cmd;
  mkdir_cmd.Prepend("mkdir -p ");
  mkdir_cmd.Append("; cd ");
  mkdir_cmd.Append(temp_dirname);
  mkdir_cmd.Append("; mkdir -p hist root text table");
  cout << "mkdir_cmd " << mkdir_cmd << endl;
*/
  recaldir = "/direct/phenix+hp/data06/bmeredi2_taxi/pi0cal/test/recal/pi0cal_";
  recaldir+=runnum; recaldir+="/";
  groupdir = "/direct/phenix+hp/data06/bmeredi2_taxi/pi0cal/test/groups/pi0cal_";
  groupdir+=groupnum; groupdir+="/";
  TString mkdir_cmd = "mkdir -p ";
  mkdir_cmd+=recaldir; mkdir_cmd+="hist ";
  mkdir_cmd+=recaldir; mkdir_cmd+="root ";
  mkdir_cmd+=recaldir; mkdir_cmd+="text ";
  mkdir_cmd+=recaldir; mkdir_cmd+="table";

  gSystem->Exec( mkdir_cmd.Data() );


  mkdir_cmd = "mkdir -p ";
  mkdir_cmd+=groupdir; mkdir_cmd+="hist ";
  mkdir_cmd+=groupdir; mkdir_cmd+="root ";
  mkdir_cmd+=groupdir; mkdir_cmd+="text ";
  mkdir_cmd+=groupdir; mkdir_cmd+="table";

  gSystem->Exec( mkdir_cmd.Data() );

  //gSystem->Exec("mkdir -p hist root text table");

  statusfile = groupdir; statusfile+="status_"; statusfile += iteration;
  statusfile+=".txt";
  tempfile = groupdir; tempfile+="temp_"; tempfile += iteration;
  tempfile+=".txt";
  TString cp_cmd = "cp "; cp_cmd+=statusfile; cp_cmd+= " "; cp_cmd+=tempfile;
  gSystem->Exec( cp_cmd.Data() );
  


  std::cout << "MpcPi0Cal::MpcPi0Cal ... finish" << std::endl;
  std::cout << std::endl;

  treeflag = 0;

  mpcmap = MpcMap::instance();
  se = Fun4AllServer::instance();
  daflag = 0;
  return;
}

int MpcPi0Cal::Init(PHCompositeNode *topNode)
{

  std::cout << "MpcPi0Cal::Init() ... start" << std::endl;
  hfile = TFile::Open(rootfile.c_str(), "RECREATE");
  assert(hfile);

  ph1 = new pi0base();
  ph2 = new pi0base();
  
  //MpcPi0MassContainer dummy;

  for(int ibin=0;ibin<20;ibin++)
    for(int impc=0;impc<2;impc++)
      {
	mix[ibin][impc] = new pi0mixer(20,0,0);
      }

  if ( treeflag )
    {
      tree = new TTree("tree","pi0");
      tree->Branch("runnum", &runnum, "runnum/I");
      tree->Branch("ntow", &ntow, "ntow/I");
      tree->Branch("nclus", &nclus, "nclus/I");
      tree->Branch("tarm", tarm, "tarm[nclus]/I");
      tree->Branch("te", te, "te[nclus]/F");
      tree->Branch("te9", te9, "te9[nclus]/F");
      tree->Branch("tecore", tecore, "tecore[nclus]/F");
      tree->Branch("tixpos", tixpos, "tixpos[nclus]/I");
      tree->Branch("tiypos", tiypos, "tiypos[nclus]/I");
      tree->Branch("tx", tx, "tx[nclus]/F");
      tree->Branch("ty", ty, "ty[nclus]/F");
      tree->Branch("tz", tz, "tz[nclus]/F");
      tree->Branch("vertex", &vertex, "vertex/F");
    }

  henergy1 = new TH1D("henergy1", "", 6000, 0, 60);
  henergy2 = new TH1D("henergy2", "", 6000, 0, 60);
  hptPi0 = new TH1D("hptPi0", "", 1000, 0, 10);
  henergyPi0 = new TH1D("henergyPi0", "",  6000, 0, 60);
  hmassPi0 = new TH1D("hmassPi0", "", 1000, 0, 1);
  hmass_vs_e_Pi0 = new TH2F("hmass_vs_e_Pi0","energy vs mass",60,0,60,500,0,0.5);

  ncalls = 0;

  InitPi0Mass();

  mc = new MpcPi0MassContainer();
  for(int impc=0;impc<MPC_ARMNUM;impc++)
    for(int ix=0;ix<MPC_X;ix++)
      for(int iy=0;iy<MPC_Y;iy++)
	{
	  //an awful hack..having problems w/ 3-D array passing for MpcPi0Mass object.
	  mc->m[impc][ix][iy] = pi0mass_tower[impc][ix][iy];
	}
  

  // Read in the original coefficients used in this pass
  ReadCoefficients();

  //Beau: These really should not be hardcoded in, but as long as I keep these in the same directory, there will be no problem
  TString mpcWarnMapFN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcppdeadmap2.txt";
  if(daflag) mpcWarnMapFN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcdadeadmap2.txt";

  for(int impc=0;impc<2;impc++)
    for(int ix=0;ix<18;ix++)
      for(int iy=0;iy<18;iy++)
	{
	  mpcWarnMap[impc][ix][iy] = 0;
	}
  
  ifstream inFile;
  inFile.open(mpcWarnMapFN.Data(),ios::in);
  if (inFile.is_open())
    {
      while (! inFile.eof() )
	{
	  //int count=0;
	  int fee = -2; float data[4]; int x; int y; 
 	  inFile>>fee;
	  if(fee == -2) {cout << "done reading file\n"; break;}
	  int mpc = (fee <288)?0:1;
	  for(int idata=0;idata<4;idata++) inFile >> data[idata];
	  if(fabs(data[1]) > 5){
	    x = -1;
	    y = -1;
	    cout << "hot ch: " << fee << endl;
	    x = mpcmap->getGridX(fee);
	    y = mpcmap->getGridY(fee);
	    if(x>=0 && y>=0 && x<18 && y<18)
	      mpcWarnMap[mpc][x][y]=-3;
	  }
	}
      inFile.close();

      
      
    }
  else cout << "Unable to open Mpc Warnmap file"; 
  
  ifstream inFile1;
  TString mpcWarn2FN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcextrabadtowers.txt";
  inFile1.open(mpcWarn2FN.Data(),ios::in);
  if(inFile1.is_open())
    {
      while(!inFile1.eof() )
	{
	  int inmpc = -1; int inx = -1; int iny = -1;
	  inFile1 >> inmpc >> inx >> iny;
	  //cout << "Debugging: " << inmpc << "\t" << inx << "\t" << iny << endl;
	  if(inx>=0 && iny>=0 && inx<18 && iny<18 && (inmpc == 0 || inmpc == 1)){
	    mpcWarnMap[inmpc][inx][iny]=-2;
	    //cout << "Warnmapvalue is " << mpcWarnMap[inmpc][inx][iny];
	  }
	}
    }
  else cout << "Unable to open Mpc extra Warnmap file"; 


  ifstream inFile2;
  TString mpcWarn3FN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcfreezelist.txt";
  inFile2.open(mpcWarn3FN.Data(),ios::in);
  if(inFile2.is_open())
    {
      while(!inFile2.eof() )
	{
	  int inmpc = -1; int inx = -1; int iny = -1;
	  inFile2 >> inmpc >> inx >> iny;
	  //cout << "Debugging: " << inmpc << "\t" << inx << "\t" << iny << endl;
	  if(inx>=0 && iny>=0 && inx<18 && iny<18 && (inmpc == 0 || inmpc == 1)){
	    mpcWarnMap[inmpc][inx][iny]=-3;
	    //cout << "Warnmapvalue is " << mpcWarnMap[inmpc][inx][iny];
	
	  }
	}
    }
  else cout << "Unable to open Mpc Freeze file"; 
 
   ifstream inFile3;
  TString mpcWarn4FN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcunfreezelist.txt";
  inFile3.open(mpcWarn4FN.Data(),ios::in);
  if(inFile3.is_open())
    {
      while(!inFile3.eof() )
	{
	  int inmpc = -1; int inx = -1; int iny = -1;
	  inFile3 >> inmpc >> inx >> iny;
	  //cout << "Debugging: " << inmpc << "\t" << inx << "\t" << iny << endl;
	  if(inx>=0 && iny>=0 && inx<18 && iny<18 && (inmpc == 0 || inmpc == 1)){
	    mpcWarnMap[inmpc][inx][iny]=0;
	    //cout << "Warnmapvalue is " << mpcWarnMap[inmpc][inx][iny];
	  }
	}
    }
  else cout << "Unable to open Mpc UNFreeze file"; 


  
   ifstream inFile4;
  TString mpcWarn5FN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcpermafreezelist.txt";
  if(daflag) mpcWarn5FN = "/direct/phenix+hp/data06/bmeredi2/devel/offline/packages/mpc/calibrations/pi0cal/warnmap/mpcpermafreezelist_da.txt";
  inFile4.open(mpcWarn5FN.Data(),ios::in);
  if(inFile4.is_open())
    {
      while(!inFile4.eof() )
	{
	  int inmpc = -1; int inx = -1; int iny = -1;
	  inFile4 >> inmpc >> inx >> iny;
	  //cout << "Debugging: " << inmpc << "\t" << inx << "\t" << iny << endl;
	  if(inx>=0 && iny>=0 && inx<18 && iny<18 && (inmpc == 0 || inmpc == 1)){
	    mpcWarnMap[inmpc][inx][iny]=-2;
	    //cout << "Warnmapvalue is " << mpcWarnMap[inmpc][inx][iny];
	  }
	}
    }
  else cout << "Unable to open Mpc permafreeze Warnmap file"; 

  



  for(int impc=0;impc<2;impc++)
    for(int iy=0;iy<18;iy++)
      for(int ix=0;ix<18;ix++)
	{
	  std::cout << mpcmap->getFeeCh(ix,iy,impc) << ", " << mpcWarnMap[impc][ix][iy] << ": ";
	}
  std::cout << "MpcPi0Cal::Init() ... finish" << std::endl;
  std::cout << std::endl;
  return 0;

}

int MpcPi0Cal::InitRun(PHCompositeNode *topNode)
{

  cout << "Beginning InitRun()\n";
  // trighelp = new TriggerHelper(topNode);
  //if ( trighelp==0 )
  //  {
  //    cout << "corr::InitRun, TriggerHelper not found" << endl;
  //    return ABORTRUN;
  //  }

  runheader = getClass<RunHeader>   (topNode, "RunHeader");
  if (!runheader) {
    cout << PHWHERE << "RunHeader node not in the tree." << endl;
    return 0;
  }
  int runnum = runheader -> get_RunNumber();
  if(runnum>=254945) daflag = 0;
  else daflag = 1;
  


  cout << "Ending InitRun()\n for run: " << runnum << "\n";
  return 0;
}


int MpcPi0Cal::process_event(PHCompositeNode *topNode)
{
  
  if ( ++ncalls%5000==0 || ncalls==0 ){
    std::cout << "MpcPi0Cal::process_event() ...  Ncalls = " << ncalls << std::endl;
    status = get_status();
    if(status >statusmax){
      std::cout << "Exiting b/c nfiles in the is rungroup is > 10\n";
      return -2;
    }
  }
  
  //std::cout << "ncalls \t" << ncalls << endl;
  
  if(ncalls > 700000) {
    std::cout << "Exiting b/c nevts > 5e5\n";
    // ncalls = 0;
    return -2;
  }
  
  
  
  //int itr = 0;
  
  //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  //itr++;
 
  GetNodes(topNode);
  //std::cout << "under getnodes\n";
  if(mpctowercont == NULL || mpcclustercont == NULL || eheader == NULL || phglobal == NULL )
   {
      std::cout << "ncalls = " << ncalls << std::endl;
      std::cout << "mpctowercont = " << mpctowercont << std::endl;
      std::cout << "mpcclustercont = " << mpcclustercont << std::endl;
      std::cout << "eheader = " << eheader << std::endl;
      std::cout << "phglobal = " << phglobal << std::endl;
      std::cout << "return..." << std::endl;
      return 0;
    }
  
  centrality = -1;
  if(daflag){
    //cout << "centrality is " << centrality << endl;
    centrality = phglobal->getCentrality();
    //cout << "centrality is " << centrality << endl;
    if(centrality < 50) return 0;
  }

  //std::cout << "under centrality\n";

//std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  //itr++;
  ntow = mpctowercont->size();
  nclus = mpcclustercont->size();
  Float_t phglobal_vertex = phglobal->getBbcZVertex();
  int event = eheader->get_EvtSequence();
/*
  TMCParticle *primary_pi0 = phpythia->getParticle(0);
  vertex = primary_pi0->GetVz()/10.;	// Vz is in mm, want to convert to cm.
*/

  vertex = phglobal_vertex;
  if( fabs(vertex) > 30) return 0;
  zbin =  (int)(vertex+30.0)/3;
  //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  //itr++;
   if (nclus > STACK_CLUSTER)
    {
      std::cout << "ncalls = " << ncalls << std::endl;
      std::cout << "nclus = " << nclus << ", return; " << std::endl;
      return 0;
    }
   //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
   //itr++;
 
  int buf_itr[2] = {0,0};
  for (int i=0; i<nclus; i++)
    {
      mpccluster = mpcclustercont->getCluster(i);
      if ( mpccluster==0 ) continue;

      int impc = mpccluster->arm(); //Beau: used below for mixer
      
      tarm[i] = mpccluster->arm();
      tixpos[i] = mpccluster->ixpos();
      tiypos[i] = mpccluster->iypos();
      bool freeze = 0;
      if(mpcWarnMap[impc][tixpos[i]][tiypos[i]] == -3) freeze = true;  //Beau: warnmap cut
      else if(mpcWarnMap[impc][tixpos[i]][tiypos[i]] < 0) continue;  //Beau: warnmap cut
      tx[i] = mpccluster->x();
      ty[i] = mpccluster->y();
      tz[i] = mpccluster->z();
      te[i] = mpccluster->e();
      tecore[i] = mpccluster->ecore();
      te9[i] = mpccluster->e9();
      SetMpcPhoton(ph1, mpccluster, vertex, runnum, event); //Beau: Sets "photon"
      if(freeze) ph1->frozen = 1;
      if(passedMpcPhotonCuts(ph1)) //Beau: Photon cuts
	{
	  mix[zbin][impc]->addptcl(ph1);  //Beau: The way a ptcl is added to the current event buffer in the mixer
	  //mix[zbin][impc]->printtest();
	  buf_itr[impc]++; //Beau: A counter for the number of photons added
                           //Beau: Really only needs to be 0 or 1
	}
    }
  
  //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  //itr++;
  for(int impc=0;impc<2;impc++)
    {
      if( buf_itr[impc] <= 0 ) break;  //Beau: there is a function in the mixing object to do this too
      if(!mix[zbin][impc]->isempty() )  //Beau: Check if there are any events on the pool
	{
	  int nmix = mix[zbin][impc]->mixmass(mc);  //Beau: mixes in zvertex bins of 3 cm
	  if(ncalls == 1000) cout << "MPC mass mixing: " << impc << ", " << nmix << endl;
	}
      mix[zbin][impc]->pushevent();  //Beau: push event after it has been mixed with all events in pool
    }
  //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  // itr++;


  if ( treeflag )
    {
      tree->Fill();
    }

  //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  //itr++;
  ////////////// pi0 reconstruct /////////////

  for (int iclus1=0; iclus1<nclus-1; iclus1++){

    if ( (tixpos[iclus1] > 17) || (tixpos[iclus1] < 0) || (tiypos[iclus1] > 17) || (tiypos[iclus1] < 0) )
      {
        continue;
      }
    

    // edge cut
    /*
    Double_t tr_1 = sqrt(tx[iclus1]*tx[iclus1]+ty[iclus1]*ty[iclus1]);
    if ( tr_1<12.5 || tr_1 >17.0 ) continue;
    */
    mpcClusterContent* clus1 = mpcclustercont->getCluster(iclus1);
    float dispx1 = clus1->dispz();
    float dispy1 = clus1->dispy();
    if(dispx1 > 3 || dispy1 > 3) continue;
    
    SetMpcPhoton(ph1, clus1, vertex, runnum, event);//Beau: added this to fg just to match cuts
    if(mpcWarnMap[ph1->arm][ph1->ia][ph1->ib] == -3) ph1->frozen = 1;  //Beau: warnmap cut
    else if(mpcWarnMap[ph1->arm][ph1->ia][ph1->ib] < 0) continue;  //Beau: fg
    if(!passedMpcPhotonCuts(ph1)) continue; //Beau: fg

    d3vec1.SetXYZ(tx[iclus1], ty[iclus1], tz[iclus1]-vertex);  // float, float, float-float
    //d3vec1 = te9[iclus1] * d3vec1.Unit();
    //d4vec1.SetT(te9[iclus1]);
    d3vec1 = tecore[iclus1] * d3vec1.Unit();
    d4vec1.SetT(tecore[iclus1]);
    d4vec1.SetVect(d3vec1);
    //    std::cout << "t4 ... " << d4vec1.X() << ":" << d4vec1.Y() << ":" << d4vec1.Z() << ":" << std::endl;

    e_1 = d4vec1.T();		//

    for (int iclus2=iclus1+1; iclus2<nclus; iclus2++)
      {
        if ( (tixpos[iclus2] > 17) || (tixpos[iclus2] < 0) || (tiypos[iclus2] > 17) || tiypos[iclus2] < 0)
          {
	    continue;
          }
      
	


        // edge cut
        /*
        Double_t tr_2 = sqrt(tx[iclus2]*tx[iclus2]+ty[iclus2]*ty[iclus2]);
        if ( tr_2<12.5 || tr_2 >17.0 ) continue;
        */

        //if( CutEnergyAsym(te9[iclus1], te9[iclus2]) == 1 ) continue;  // Float_t, Float_t
        //if( CutEnergyAsym(te[iclus1], te[iclus2]) == 1 ) continue;  // Float_t, Float_t

        d3vec2.SetXYZ(tx[iclus2], ty[iclus2], tz[iclus2]-vertex);
        d3vec2 = tecore[iclus2] * d3vec2.Unit();
        d4vec2.SetT(tecore[iclus2]);
        //d3vec2 = te9[iclus2] * d3vec2.Unit();
        //d4vec2.SetT(te9[iclus2]);
        d4vec2.SetVect(d3vec2);

        d4vecPi0 = d4vec1 + d4vec2;
        e_pi0 = d4vecPi0.T();  // Double_t = Double_t
        //      std::cout << "e_pi0 = " << e_pi0 << ", " << d4vec1.T()+d4vec2.T() << std::endl;

        // need to put this back in for the real data...
        //if(e_pi0<6.0) continue;

        mass_pi0 = d4vecPi0.Mag();
        if(mass_pi0 < 0.00001) continue;
        //      std::cout << "mass = " << mass_pi0 << std::endl;
	mpcClusterContent* clus2 = mpcclustercont->getCluster(iclus2);


	float dispx2 = clus2->dispz();
	float dispy2 = clus2->dispy();
	if(dispx2 > 3 || dispy2 > 3) continue;


	SetMpcPhoton(ph2, clus2, vertex, runnum, event); //Beau: Added this to fg only to match cuts
	if(mpcWarnMap[ph2->arm][ph2->ia][ph2->ib] == -3) ph2->frozen = 1;  //Beau: warnmap cut
	else if(mpcWarnMap[ph2->arm][ph2->ia][ph2->ib] < 0) continue;//Beau: fg
	if(!passedMpcPhotonCuts(ph2)) continue; //Beau: fg cuts
	if(!passedMpcPionCuts(ph1,ph2)) continue; //Beau: fg cuts

        p_pi0 = d4vecPi0.P();
        pt_pi0 = d4vecPi0.Perp();
        //      std::cout << "Pt = " << pt_pi0 << ", " << sqrt( (d3vec1.X()+d3vec2.X())*(d3vec1.X()+d3vec2.X()) + (d3vec1.Y()+d3vec2.Y())*(d3vec1.Y()+d3vec2.Y()) ) << std::endl;

        e_2 = d4vec2.T();

        //if ( e_pi0 < 9.0 || e_pi0>25.0 || pt_pi0<0.25 ) continue;
        
	if(!ph2->frozen)
	  pi0mass_tower[ tarm[iclus1] ][ tixpos[iclus1] ][ tiypos[iclus1] ]->FillMass(mass_pi0, pt_pi0, p_pi0, e_1 );
	if(!ph1->frozen)
	  pi0mass_tower[ tarm[iclus2] ][ tixpos[iclus2] ][ tiypos[iclus2] ]->FillMass(mass_pi0, pt_pi0, p_pi0, e_2 );
        
	
        //      pi0mass_tower[ tarm[i] ][ tixpos[i] ][ tiypos[i] ]->FillMass(mass_pi0, pt_pi0, p_pi0);
        // FillMass(Double_t, Double_t, Double_t)
        // In this case, m_h1_mass->Fill(mass_pi0) is called.
        // m_h1_mass is (TH1D*)
	
        henergy1->Fill(e_1);
        henergy2->Fill(e_2);
        hptPi0->Fill(pt_pi0);
        henergyPi0->Fill(e_pi0);
        hmassPi0->Fill(mass_pi0);
        hmass_vs_e_Pi0->Fill(e_pi0,mass_pi0);
      }
  }
  //std::cout << "ncalls = " << ncalls << "itr: " << itr << std::endl;
  //itr++;
  return 0;
}

// dump out results
int MpcPi0Cal::End(PHCompositeNode *topNode)
{
  std::cout << std::endl;
  std::cout << "MpcPi0Cal::End() ... start" << std::endl;
  status = get_status();
  std::cout << "status is: " << status << endl;
  if(status == statusmax){
    recaldir = groupdir;
    init_files();
    add_histos();
  }
  if(status>statusmax){
    return 0;
  }

  write_status();


#ifdef MPCCAL_DO_FIT
  for (int ias = 0; ias < MPC_ARMNUM; ias++)
    {
      for (int ixpos = 0; ixpos < MPC_X; ixpos++)
        {
          for (int iypos = 0; iypos < MPC_Y; iypos++)
            {
	      //std::cout << "arm=" << ias << ", ix=" << ixpos << ", iy=" << iypos << std::endl;
	      pi0mass_tower[ias][ixpos][iypos]->GetYield();
	      pi0mass_tower[ias][ixpos][iypos]->FitMass_new();
	      //pi0mass_tower[ias][ixpos][iypos]->FitMass();
            }
        }
    }

  DumpCalibResults();
  MakeSummaryTower();
  MakeSummaryCoefficient();
#endif //MPCCAL_DO_FIT

  hfile->cd();
  
  std::cout << "hoge hoge" << std::endl;

  hfile->Write();
  //  hfile->Close();

  std::cout << "MpcPi0Cal::End() ... finish" << std::endl;
  std::cout << std::endl;

  sleep(4);

  return 0;

}

void MpcPi0Cal::set_fname_coefficient()
{
  fname_coefficient = "localfit_data";
  fname_coefficient += "/coefficient_";
  fname_coefficient += runnum;
  fname_coefficient += "_iter_";
  fname_coefficient += fIteration;
  fname_coefficient += ".txt";
}

void MpcPi0Cal::InitPi0Mass()
{
  Char_t objname[1024];
  armname[0] = "south";
  armname[1] = "north";

  for (Int_t ias = 0; ias < MPC_ARMNUM; ias++) {
    for (Int_t ix = 0; ix < MPC_X; ix++) {
      for (Int_t iy = 0; iy < MPC_Y; iy++) {
	sprintf(objname, "pi0mass_%sx%iy%i", armname[ias].Data(), ix, iy);
	pi0mass_tower[ias][ix][iy] = new MpcPi0Mass(objname);
      }
    }
  }
}

// Cut on Energy Asymmetry
Int_t MpcPi0Cal::CutEnergyAsym(Float_t e1, Float_t e2)
{
  Int_t result = 0;
  Float_t alpha = 0.0;
  alpha = fabs( (e1-e2)/(e1+e2) );
  if(alpha>=0.6) result = 1;
  return result;
}

/*
Int_t MpcPi0Cal::CutDeadTower(int numx, int numy, int runnumber) 
{
  int deadch[59][2] = {
    { 6,  0}, 
    {10,  3},
    { 9,  3},
    {12,  3},
    {11,  3},
    {12,  4},
    {11,  4},
    {14,  7},
    {13,  7},
    {14,  8},
    {13,  8},
    {14,  5},
    {13,  5},
    {14,  6},
    {13,  6},
    {14,  3},
    {13,  3},
    {14,  4},
    {13,  4},
    {16,  7},
    {15,  7},
    {16,  8},
    {15,  8},
    {16,  5},
    {15,  5},
    {16,  6},
    {15,  6},
    {15,  3},
    {15,  4},
    { 4,  1},
    {17,  7},
    {17,  8},
    {16,  4},
    { 4, 16},
    {17,  6},
    { 7,  1},
    { 8,  2},
    { 7,  2},
    { 8, 17},
    { 7, 17},
    { 8, 15},
    { 8, 16},
    { 7, 16},
    {10,  1},
    { 9,  1},
    {10,  2},
    { 9,  2},
    {10, 17},
    {10, 15},
    {10, 16},
    {12,  1},
    {11,  1},
    {12,  2},
    {11,  2},
    {13,  1},
    {11,  0},
    {13, 16},
    {14,  2},
    {13,  2}
  };

  int result=0;
  if ((runnumber == 205296 ) || 
      (runnumber == 205365 ) ||
      (runnumber == 205382 ) ||
      (runnumber == 205401 ) ||
      (runnumber == 205405 ) ||
      (runnumber == 205406 ) ||
      (runnumber == 205414 ) ||
      (runnumber == 205418 ) ||
      (runnumber == 205443 ) ||
      (runnumber == 205455 ) ||
      (runnumber == 205457 )){
    for(int i=0; i<59; i++){
      //    std::cout << i << std::endl;
      if( (numx==deadch[i][0]) && (numy==deadch[i][1]) ) {
	//      std::cout << "numx , numy = " << numx << " , " << numy << std::endl;
	//      std::cout << "break ..." << std::endl;
	//      std::cout << std::endl;
	result=1;
	break;
      }
    }
  }else{
    for(int i=0; i<59; i++){
      //    std::cout << i << std::endl;
      if( (numy==deadch[i][0]) && (numx==deadch[i][1]) ) {
	//      std::cout << "numx , numy = " << numx << " , " << numy << std::endl;
	//      std::cout << "break ..." << std::endl;
	//      std::cout << std::endl;
	result=1;
	break;
      }
    }
  }
  return result;
}
*/

int MpcPi0Cal::EndRun(const int runnumber) {
  std::cout << "Ending run : " << runnumber << endl;
  return 0;
}


void MpcPi0Cal::DumpCalibResults()
{
  char fn_result[1024];
  sprintf(fn_result, "%stext/calib_result_%i.txt", recaldir.Data(),fIteration);
  FILE* of_result;
  if ( (of_result = fopen(fn_result, "w")) == 0 ) {
    fprintf(stderr, "File open error: %s\n", fn_result);
    return;
  }

  //     fprintf(of_result, 
  //	     "# Calibration Results (hot/dead/edge towers are omitted)\n");
  fprintf(of_result,"# towerID coeffi. mean+-err sigma+-err chi2/NDF poln npair status\n\n");

  for (int ias = 0; ias < MPC_ARMNUM; ias++)
    {
      for (int ix = 0; ix < MPC_X; ix++)
        {
          for (int iy = 0; iy < MPC_Y; iy++)
            {

	      Double_t mean, mean_err, sigma, sigma_err, chi2, npair_in_bin;
	      Int_t NDF, poln;

	      pi0mass_tower[ias][ix][iy]->GetResults(mean, mean_err, 
                      sigma, sigma_err, chi2, NDF, poln, npair_in_bin);

              Double_t orig_coef = GetCoefficient(ias,ix,iy);
	      Double_t new_coef = pi0mass_tower[ias][ix][iy]->GetCoefficient();

	      TString status;
	      if (pi0mass_tower[ias][ix][iy]->FitIsGood())
                {
	          status = "";
	        }
              else if (pi0mass_tower[ias][ix][iy]->FitIsFair())
                {
	          status = "fair";
	        }
              else if (pi0mass_tower[ias][ix][iy]->FitIsFail())
                {
	          status = "fail";
	        }

              int feech = mpcmap->getFeeCh(ix, iy, ias);
              if ( feech<0 ) continue;

	      fprintf(of_result,"%i %2i %2i: %4f  %4f+-%4f  %4f+-%4f  %3f/%i  pol%i  %2f  %s\n",
		feech, ix, iy, new_coef, mean, mean_err, sigma, sigma_err, 
		chi2, NDF, poln, npair_in_bin, status.Data());

	      if (! pi0mass_tower[ias][ix][iy]->FitIsFail())
                {
	          SetCoefficient(ias, ix, iy, new_coef*orig_coef);
	        }
            }
        }
    }
  fclose(of_result);

  DumpCoefficient();	// save to MpcCal.recal_gains_$iter
}

void MpcPi0Cal::DumpCoefficient()
{
  TString recal_fname = recaldir; recal_fname+="MpcCal.recal_gains_";
  recal_fname += fIteration;
  ofstream recal_file( recal_fname );
  for (int iarm = 0; iarm < MPC_ARMNUM; iarm++)
    {
      for (int ixpos = 0; ixpos < MPC_X; ixpos++)
        {
          for (int iypos = 0; iypos < MPC_Y; iypos++)
            {
              int ifeech = mpcmap->getFeeCh(ixpos,iypos,iarm);
              if ( ifeech<0 ) continue;
              recal_file << ifeech << "\t"
		         << coefficient[iarm][ixpos][iypos]
                         << "\t1\t0" << endl;
            }
        }
    }
  recal_file.close();
}

void MpcPi0Cal::GetNodes(PHCompositeNode *topNode) 
{
  // Set all pointers to null...
  mpctowercont = NULL;
  mpcclustercont = NULL;
  eheader      = NULL;
  phglobal     = NULL;
  phpythia     = NULL;

  // Search out the nodes from the node tree...

  // mpcTowerContainer
  mpctowercont = findNode::getClass<mpcTowerContainer>(topNode,"mpcTowerContainer");
  if(!mpctowercont) cout << PHWHERE << "MpcPi0Cal: mpcTowerContainer not in Node Tree"<<endl;

  // mpcClusterContainer
  mpcclustercont = findNode::getClass<mpcClusterContainer>(topNode,"mpcClusterContainer");
  if(!mpcclustercont) cout << PHWHERE << "MpcPi0Cal: mpcClusterContainer not in Node Tree"<<endl;

  // EventHeader
  eheader = findNode::getClass<EventHeader>(topNode,"EventHeader");
  if(!eheader) cout << PHWHERE << "MpcPi0Cal:: EventHeader not in Node Tree"<<endl;

  // PHGlobal
  phglobal = findNode::getClass<PHGlobal>(topNode,"PHGlobal");
  if(!phglobal) cout << PHWHERE << "MpcPi0Cal:: PHGlobal not in Node Tree"<<endl;
 
  // PHPythia
/*
  phpythia = findNode::getClass<PHPythiaContainer>(topNode,"PHPythia");
  if(!phpythia) cout << PHWHERE << "MpcPi0Cal:: PHPythia not in Node Tree"<<endl;
*/
 
  return;
}

Int_t MpcPi0Cal::MakeSummaryTower()
{
  //// tower-by-tower mean/sigma
  m_style->SetOptStat(kFALSE);
  m_style->SetOptLogy(kTRUE);

  TCanvas* c_tower_by_tower = new TCanvas("c_tower_by_tower", "", 500, 700);
  c_tower_by_tower->Divide(1, 2);

  TH1D* h1_mean_tower = new TH1D("h1_mean_tower", "", 100, 0.07, 0.20);
  TH1D* h1_sigma_tower = new TH1D("h1_sigma_tower", "", 100, 0.0, 0.1);
  h1_mean_tower->SetTitle(";mean;towers");
  h1_sigma_tower->SetTitle(";width;towers");

  for (int ias = 0; ias < MPC_ARMNUM; ias++) {
    for (int ixpos = 0; ixpos < MPC_X; ixpos++) {
      for (int iypos = 0; iypos < MPC_Y; iypos++) {
	if (! pi0mass_tower[ias][ixpos][iypos]->FitIsFail()) {
	  Double_t mean, mean_err, sigma, sigma_err, chi2, npair_in_bin;
	  Int_t NDF, poln;
	  pi0mass_tower[ias][ixpos][iypos]->GetResults(mean, mean_err, 
							 sigma, sigma_err, chi2, NDF, poln, npair_in_bin);
	  	  
	  h1_mean_tower->Fill(mean);
	  h1_sigma_tower->Fill(sigma);
	}
      }
    }
  }
  char epsfn_mean_tower[128];
  char pngfn_mean_tower[128];
	
  sprintf(epsfn_mean_tower, "%shist/mean_tower_%i.eps", recaldir.Data(),fIteration);
  sprintf(pngfn_mean_tower, "%shist/mean_tower_%i.png", recaldir.Data(),fIteration);
  
c_tower_by_tower->cd(1);
  h1_mean_tower->Draw();
  c_tower_by_tower->cd(2);
  h1_sigma_tower->Draw();


  c_tower_by_tower->SaveAs(epsfn_mean_tower);
  c_tower_by_tower->SaveAs(pngfn_mean_tower);
  
  hfile->cd();
  h1_mean_tower->Write();
  h1_sigma_tower->Write();
  delete h1_mean_tower;
  delete h1_sigma_tower;

  delete c_tower_by_tower;

  return 1;
}

Int_t MpcPi0Cal::MakeSummaryCoefficient()
{
  m_style->SetOptStat(kFALSE);
  m_style->SetOptLogy(kTRUE);
  TCanvas* c_coef = new TCanvas("c_coef");
  TH1D* h1_coef = new TH1D("h1_coef", "", 500, 0.0, 5.0);
  h1_coef->SetTitle(";coefficient;");

  for (int ias = 0; ias < MPC_ARMNUM; ias++) {
    for (int ix = 0; ix < MPC_X; ix++) {
      for (int iy = 0; iy < MPC_Y; iy++) {
	if ( ! pi0mass_tower[ias][ix][iy]->FitIsFail()) {
	  h1_coef->Fill(pi0mass_tower[ias][ix][iy]->GetCoefficient());
	}
      }
    }
  }
  h1_coef->Draw();

  char epsfn_coef[128];
  char pngfn_coef[128];

  sprintf(epsfn_coef, "%shist/coefficient_%i.eps",recaldir.Data(), fIteration);
  sprintf(pngfn_coef, "%shist/coefficient_%i.png",recaldir.Data(), fIteration);
  c_coef->SaveAs(epsfn_coef);
  c_coef->SaveAs(pngfn_coef);
  //     h1_coef->Write();
  delete h1_coef;
  delete c_coef;

  return 1;
}

void MpcPi0Cal::ReadCoefficients()
{
  // Reset the coefficients
  for (int iarm=0; iarm<2; iarm++)
    {
      for (int ix=0; ix<18; ix++)
        {
          for (int iy=0; iy<18; iy++)
            {
              coefficient[iarm][ix][iy] = 1.0;
            }
        }
    }

  // Read in Recal Gain Coefficients
  if ( fIteration == 1 ) return;
 
  TString name = groupdir; name+="MpcCal.recal_gains_"; name += (fIteration-1);
  ifstream infile( name.Data() );
  cout << "About ready to read in gains: " << endl;
  int gainitr=0;
  if ( infile.good() )
    {
      
      Float_t mean, deviation;
      Int_t   ifeech, status;
      while ( infile >> ifeech >> mean >> deviation >> status )
        {
          if ( ifeech<0 || ifeech>575 )
            {
              continue;
            }
	  
	  gainitr++;
	  cout << "reading in gains: " << gainitr << endl;
          Int_t arm = (ifeech < 288) ? 0 : 1;
          Int_t gridx = mpcmap->getGridX( ifeech );
          Int_t gridy = mpcmap->getGridY( ifeech );
          coefficient[arm][gridx][gridy] = mean;
        }
    
      infile.close();
    }
  else
    {
      cout << PHWHERE << " ERROR, failed to open correction_file " << name << endl;
    }
}

