/*
Beau Meredith 11-11-2010

This class takes output from the Subsysreco class mpcPi0TTree and calculates the
pi0 mass value in each tower determined from running 200 GeV
p+p pythia through GEANT.  The output file will be in the directory recal_sim.

I use a simplified procedure to find the value of the mass in each 
bin.  There is no subtraction, but rather a Gaussian is fit a small 
region (+/- 25 MeV) around the maximum of the mass peak.  The mean 
value and standard deviation are then calculated from this fit; the 
stdv is still a good measure of the width of the peak, but is not an 
exact measure. 


The cuts made on the data are

fabs(zvtx) < 30.0

Eclus > 2
chi2/dof < 3
Max(dispx, dispy) < 4.0

pt_pair > 0.45
9 < E_pair < 17
Easymm < 0.6

*/




#include <iostream>
#include <TGraphErrors.h>
#include <TH1F.h>
#include <TH2F.h>
#include <TCanvas.h>
#include <fstream>
#include <string>
#include <TTree.h>
#include <TChain.h>
#include <THmulf.h>
#include <cmath>
#include <TFile.h>
#include <TF1.h>
#include <TStyle.h>
#include <TSystem.h>
#include <MpcMap.h>
#include <algorithm>
#include <vector>
#include <deque>
#include <TPad.h>
#include <TPaveText.h>
#include <TText.h>

using namespace std;

extern TStyle *gStyle;
extern TSystem *gSystem;



TChain *ttree;
TString DeadTowerList;
int tower[2][18][18];
MpcMap* mpcmap;
bool loaded = 0;


TH1D* hpi[2][18][18];




struct PH{
  int event; 
  int run;
  float zvtx;
  float x1;  
  float y1;
  float z1;
  float px1;  
  float py1;
  float pz1;
  float energy1;
  int arm1;
 
  int fee1;
  
  float disp;

  float eta;
  float phi;
  float theta;
  
  float chi1;
  int ixpos1;
  int iypos1;
};




void chain(char* name, char* list){
  ttree = new TChain(name);
  std::ifstream din;
  din.open(list);
  while(din){
    std::string temp;
    din >> temp;
    TString t_temp = temp;
    if(t_temp.Contains(".root"))
      {
        ttree->Add(t_temp.Data());
      }
  }
  return;
}
      
void InitWM(){
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){
	tower[iarm][ix][iy] = 0;
      }
    }
  }
  ifstream inFile;
  DeadTowerList = "/direct/phenix+hp/data06/bmeredi2/../bmeredi2_taxi/warnmap/mpcpp2.txt";
  inFile.open(DeadTowerList.Data(),ios::in);
  if (inFile.is_open())
    {
      while (! inFile.eof() )
	{
          //int count=0;
          int arm, x,y;
          inFile>>arm >>x>>y;
          tower[arm][y][x] = -1;
          if(arm < 0 || arm > 1) {cout << "exiting wm\n"; break;}
        }
      inFile.close();
      for(int impc=0;impc<2;impc++)
        for(int iy=0;iy<18;iy++)
	  for(int ix=0;ix<18;ix++)
	  {std::cout << mpcmap->getFeeCh(ix,iy,impc) << ", " << tower[impc][iy][ix] << endl;}
    }
  else cout << "Unable to open file";
  return;
}
  
  
  
void ana_pi0cal_sim(char* inlist = "list_ttree.txt", char* outfile="out_ana.root", long nent = 0){
  
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){
	TString name = "hpi_1_"; name+=ix; name+="_"; name+=iy;
	if(iarm == 0) name.ReplaceAll("hpi_1","hpi_0");
	hpi[iarm][ix][iy] = new TH1D(name.Data(),name.Data(),200,0,0.5);
      }
    }
  }

  int event; 
  int run;
  float pi0_z;
  float x1;  
  float y1;
  float z1;
  float x2;
  float y2;
  float z2;
  float px1;  
  float py1;
  float pz1;
  float px2;
  float py2;
  float pz2;  
  float energy1;
  float energy2;
  float pi0_px; 
  float pi0_py; 
  float pi0_pz;
  float pi0_energy;
  float mass;
  float pt;
  int arm1;
  int arm2;
  
 
 
  int driver1;
  int driver2;
  int fee1;
  int fee2;
  
  float etot1;
  float etot2;
  float cdisp;
  float disp;
  
  
  float ldisp;
  float lcdisp;
  float corrdispx1;
  float corrdispy1;
  float corrdispx2;
  float corrdispy2;
  float logcorrdispx1;
  float logcorrdispy1;
  float logcorrdispx2;
  float logcorrdispy2;
  float dispx1;
  float dispy1;
  float dispx2;
  float dispy2;
  float logdispx1;
  float logdispy1;
  float logdispx2;
  float logdispy2;


  float eta;
  float phi;
  float theta;
  
  float chi1;
  float chi2;
  float pi0_chi;
  
  int ixpos1;
  int iypos1;
  int ixpos2;
  int iypos2;

 
  mpcmap = MpcMap::instance();
  
  TH2F* h2_m_phi = new TH2F("mvphi","mass vs phi",100,0,1,50,-3.14159,3.14159);
  


  
  InitWM();
  TString name = "pi0_ttree";
  chain( const_cast<char*> (name.Data()),inlist);
  
  TTree* outtree = new TTree("t","t");
  
  outtree->Branch("event",&event,"event/i");
  outtree->Branch("x1",&x1,"x1/F");
  outtree->Branch("y1",&y1,"y1/F");
  outtree->Branch("x2",&x2,"x2/F");
  outtree->Branch("y2",&y2,"y2/F");
  //  outtree->Branch("pi0_x",&pi0_x,"pi0_x/F"); 
  //outtree->Branch("pi0_y",&pi0_y,"pi0_y/F"); 
  outtree->Branch("pi0_z",&pi0_z,"pi0_z/F"); 


  outtree->Branch("disp",&disp,"disp/F");
  outtree->Branch("cdisp",&cdisp,"cdisp/F");
  outtree->Branch("ldisp",&ldisp,"ldisp/F");
  outtree->Branch("lcdisp",&lcdisp,"lcdisp/F");
  
  outtree->Branch("chi2",&chi2,"chi2/F");
  outtree->Branch("chi1",&chi1,"chi1/F");
  outtree->Branch("pi0_chi",&pi0_chi,"pi0_chi/F");
  


  outtree->Branch("pi0_px",&pi0_px,"pi0_px/F"); 
  outtree->Branch("pi0_py",&pi0_py,"pi0_py/F"); 
  outtree->Branch("pi0_pz",&pi0_pz,"pi0_pz/F"); 
  outtree->Branch("phi",&phi,"phi/F");
  outtree->Branch("theta",&theta,"theta/F");
  outtree->Branch("eta",&eta,"eta/F");
  outtree->Branch("px1",&px1,"px1/F"); 
  outtree->Branch("py1",&py1,"py1/F"); 
  outtree->Branch("pz1",&pz1,"pz1/F"); 
  outtree->Branch("px2",&px2,"px2/F"); 
  outtree->Branch("py2",&py2,"py2/F"); 
  outtree->Branch("pz2",&pz2,"pz2/F"); 
  outtree->Branch("energy1",&energy1,"energy1/F");
  outtree->Branch("energy2",&energy2,"energy2/F"); 
  outtree->Branch("pi0_energy",&pi0_energy,"pi0_energy/F");
  outtree->Branch("mass",&mass,"mass/F");
  outtree->Branch("pt",&pt,"pt/F");
  outtree->Branch("arm1",&arm1,"arm1/i");
  outtree->Branch("fee1",&fee1,"fee1/i");
  outtree->Branch("fee2",&fee2,"fee2/i");
  //outtree->Branch("event",&event,"event/F");
  outtree->Branch("ixpos1",&ixpos1,"ixpos1/i");
  outtree->Branch("iypos1",&iypos1,"iypos1/i");

  outtree->Branch("ixpos2",&ixpos2,"ixpos2/i");
  outtree->Branch("iypos2",&iypos2,"iypos2/i");





  
  ttree->SetBranchAddress("event",&event);
  ttree->SetBranchAddress("x1",&x1);
  ttree->SetBranchAddress("y1",&y1);
  ttree->SetBranchAddress("x2",&x2);
  ttree->SetBranchAddress("y2",&y2);
  //  ttree->SetBranchAddress("pi0_x",&pi0_x,"pi0_x/F"); 
  //ttree->SetBranchAddress("pi0_y",&pi0_y,"pi0_y/F"); 
  ttree->SetBranchAddress("pi0_z",&pi0_z);


  ttree->SetBranchAddress("disp",&disp);
  ttree->SetBranchAddress("cdisp",&cdisp);
  ttree->SetBranchAddress("ldisp",&ldisp);
  ttree->SetBranchAddress("lcdisp",&lcdisp);
  
  ttree->SetBranchAddress("chi2",&chi2);
  ttree->SetBranchAddress("chi1",&chi1);
  ttree->SetBranchAddress("pi0_chi",&pi0_chi);
  


  ttree->SetBranchAddress("pi0_px",&pi0_px);
  ttree->SetBranchAddress("pi0_py",&pi0_py);
  ttree->SetBranchAddress("pi0_pz",&pi0_pz);
  ttree->SetBranchAddress("phi",&phi);
  ttree->SetBranchAddress("theta",&theta);
  ttree->SetBranchAddress("px1",&px1);
  ttree->SetBranchAddress("py1",&py1);
  ttree->SetBranchAddress("pz1",&pz1);
  ttree->SetBranchAddress("px2",&px2);
  ttree->SetBranchAddress("py2",&py2);
  ttree->SetBranchAddress("pz2",&pz2);
  ttree->SetBranchAddress("energy1",&energy1);
  ttree->SetBranchAddress("energy2",&energy2);
  ttree->SetBranchAddress("pi0_energy",&pi0_energy);
  ttree->SetBranchAddress("mass",&mass);
  ttree->SetBranchAddress("pt",&pt);
  ttree->SetBranchAddress("arm1",&arm1);
  ttree->SetBranchAddress("fee1",&fee1);
  ttree->SetBranchAddress("fee2",&fee2);
  

  Long64_t nentries = ttree->GetEntries();
  if(nent <= 0 || nent > nentries) nent = nentries;
  for (Long64_t i=0; i<nent;i++)
  //  for (Long64_t i=0; i<200000;i++)
    {
      if( i%10000 == 0 ) cout << i << endl;
      ttree->GetEntry(i);

      if(pi0_energy < 9) continue;
      if(pt < 0.45) continue;
      //      if(arm1 != 1) continue;
      //if(arm1 == 0) continue;
      if(pi0_chi > 4) continue;
      if(disp > 4) continue;
      if(pi0_energy > 17) continue;

      
      float r1 = sqrt(x1*x1+y1*y1);

      float r2 = sqrt(x2*x2+y2*y2);
      //      if(r2 < 11 || r2 > 19) continue;
      eta = -log(tan(theta/2.));
      //      if( fabs(eta) > 3.8 || fabs(eta) < 3.1) continue;
      float zvtx = pi0_z - 220;
      if(arm1 == 0) zvtx = pi0_z + 220;
      if(fabs(zvtx) > 30) continue;
      
      
      int driver1 = mpcmap->getDriver(fee1);
      int driver2 = mpcmap->getDriver(fee2);
      int edge1 = mpcmap->isEdge(fee1);
      int edge2 = mpcmap->isEdge(fee2);
      

      int ix1 = mpcmap->getGridX(fee1);
      int iy1 = mpcmap->getGridY(fee1);
      //      if(tower[1][iy1][ix1] < 0) continue;
      
      
      int ix2 = mpcmap->getGridX(fee2);
      int iy2 = mpcmap->getGridY(fee2);


      if(iy1 == 0 && arm1 == 1) continue;
      if(iy2 == 0 && arm1 == 1) continue;
      if(iy1 == 1 && ix1 <=12 && ix1 >=5  && arm1 == 1) continue;
      if(iy2 == 1 && ix2 <=12 && ix2 >=5  && arm1 == 1) continue;
      //      if(fee1 == 95 || fee2 == 95) continue;

      ixpos1 = ix1;
      ixpos2 = ix2;
      iypos1 = iy1;
      iypos2 = iy2;
      float weight1 = energy1/pi0_energy;
      float weight2 = energy2/pi0_energy;
      
      bool fill1 = 1;
      bool fill2 = 1;

      if(edge2 == 1) fill1 = 0;
      //      if(driver2 == 18 && driver1 != 18) fill1 = 0;

      if(edge1 == 1) fill2 = 0;
      //      if(driver1 == 18 && driver2 != 18) fill2 = 0;
      

      if(fill1) hpi[arm1][ix1][iy1]->Fill(mass,weight1);
      if(fill2) hpi[arm1][ix2][iy2]->Fill(mass,weight2);
      //      if(tower[1][iy2][ix2] < 0) continue;
      

      
      
      
      //      outtree->Fill();
    }
  
  TFile *fout = new TFile(outfile,"recreate");
  
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){
	hpi[iarm][ix][iy]->Write();
	
      }
    }
  }
  
  ofstream dout("recal_sim/sim_means_new.txt");
  
  int driver_colors[20] = {30,4,5,6,7,8,9,10,11,12,30,4,5,6,7,8,9,10,11,12};

  
  TCanvas* cbig[2];
  cbig[0]= new TCanvas("cbigS","cbigS",4000,4000);
  cbig[0]->Divide(18,18,0.000001,0.000001);
  cbig[1]= new TCanvas("cbigN","cbigN",4000,4000);
  cbig[1]->Divide(18,18,0.000001,0.000001);
  
  for(int iarm=0;iarm<2;iarm++){
    for(int ix=0;ix<18;ix++){
      for(int iy=0;iy<18;iy++){

	if(iarm == 1 && iy == 1 && (ix >=5 && ix <= 12)) continue;
	if(iarm == 1 && iy == 0 && (ix >=6 && ix <= 11)) continue;
      
      int feech= mpcmap->getFeeCh(ix,iy,iarm);
      //      if(feech == 95) continue;
      if(ix == 0 && iy == 7) cout << "ix = 0, iy = 7, feech = " << feech << endl;
      int driver = mpcmap->getDriver(feech);
      
      if(ix == 0 && iy == 7) cout << "ix = 0, iy = 7, feech = " << feech << " driver is: " << driver << endl;
      
      if(feech < 0 || feech > 576) continue;
      if(mpcmap->getGridX(feech) < 0) continue;
      int ipad = (17-iy)*18+ix;
      cbig[iarm]->cd(ipad+1);
      cbig[iarm]->GetPad(ipad+1)->SetFrameFillColor(driver_colors[(driver-1)]);

      TF1* fit = new TF1("fgaus","gaus",0,1);
      fit->SetParameter(0,1);
      fit->SetParameter(1,0.135);
      fit->SetParameter(2,0.15);
      TH1* h = hpi[iarm][ix][iy];
      int maxbin = h->GetMaximumBin();
      float val = h->GetBinCenter(maxbin);
      float val_lo = val-0.025;
      float val_hi = val+0.025;
      
      
      h->Fit(fit,"Q","",val_lo,val_hi);
      float mean = fit->GetParameter(1);
      float std = fit->GetParameter(2);
      float mean_err = fit->GetParError(1);
      float std_err = fit->GetParError(2);
      float maxmassyield = val;
      
      dout << iarm << "\t" << ix << "\t" << iy << "\t" << mean << endl;
      
      
      TPaveText *ptyield = new TPaveText(0.55,0.6,0.89,0.89,"brNDC");
      ptyield->SetLineColor(1);
      char m1[1024];
      char m2[1024];
      char m3[1024];
      sprintf(m1,"mean = %.3f +/- %.3f",mean, mean_err);
      sprintf(m2,"max = %.3f",maxmassyield);
      sprintf(m3,"std = %.3f +/- %.3f",std, std_err);
      
      TText* ty1 = ptyield->AddText(m1);
      TText* ty2 = ptyield->AddText(m2);
      TText* ty3 = ptyield->AddText(m3);
      
      if(mean > 0.18 || std>0.05){
	h->SetFillColor(2);
      }
      
      h->SetFillColor(46);
      h->Draw();
      ptyield->Draw("same");
      fit->Draw("same");
      }
    }
  }



	

  cbig[1]->SaveAs("recal_sim/sim_massN.png");
  cbig[0]->SaveAs("recal_sim/sim_massS.png");


}
    
