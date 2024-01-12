#include<iostream>
#include<TFile.h>

void SvxRp_draw(){
  //gStyle->SetOptStat(0);
  gStyle->SetOptTitle(0);
  TCanvas* c[3];
  int icent,izv;
  int idet;
  int ncent=5;
  int nzv=5;
  int nhar=6;
  int ndet=76;
  float max=1.0;
  float min=-1.0;
  float max_w=2500.0;
  char name[40];
  
  TProfile* qa[10][3];
  TProfile* QA[3];
  
  TFile *f = TFile::Open("SvxRpEventQA.root");
  
  cout <<"please choose detector id" <<endl;
  cin >> idet;
  
  for(int j=0;j<3;j++){
    if(j==0) sprintf(name,"sumx_id%d",idet);
    if(j==1) sprintf(name,"sumy_id%d",idet);
    if(j==2) sprintf(name,"sumw");
    c[j] = new TCanvas(name,name);
    c[j]->Divide(5,2,0,0,0);
  }
  for(int j=0;j<3;j++){
    for(int icent=0;icent<ncent;icent++){
      if(j<2){
	if(j==0) sprintf(name,"C%dQx",icent);
	else if(j==1) sprintf(name,"C%dQy",icent);
	qa[icent][j] = new TProfile(name,name,nhar,-0.5,nhar-0.5,-10.0,10.0); 
      }
      else { 
	sprintf(name,"C%dQw",icent);
	qa[icent][j] = new TProfile(name,name,ndet,-0.5,ndet-0.5,0,10000); 
      }
      ///////////////////////
      //merge eventclass z_vertex
      ///////////////////////
      for(int izv=0;izv<nzv;izv++){
	if(j==0) sprintf(name,"C%dZ%dD%dQx",icent,izv,idet);
	else if(j==1) sprintf(name,"C%dZ%dD%dQy",icent,izv,idet);
	else  sprintf(name,"C%dZ%dQw",icent,izv);
	QA[j]=(TProfile*)f->Get(name); 
	qa[icent][j]->Add(QA[j]);
      }
      c[j]->cd(icent+1);
      if(j<2){
	qa[icent][j]->SetMinimum(min);
	qa[icent][j]->SetMaximum(max);
      }
      else{
	qa[icent][j]->SetMinimum(0.0);
	qa[icent][j]->SetMaximum(max_w);
      }
      qa[icent][j]->Draw();
    }
    for(int izv=0;izv<nzv;izv++){
      if(j<2){
	if(j==0) sprintf(name,"Z%dQx",izv);
	else if(j==1) sprintf(name,"Z%dQy",izv);
	qa[izv+5][j] = new TProfile(name,name,nhar,-0.5,nhar-0.5,1.0,-1.0); 
      }
      else{
	sprintf(name,"Z%dQw",izv);
	qa[izv+5][j] = new TProfile(name,name,ndet,-0.5,ndet-0.5,0,10000); 	
      }
      ///////////////////////
      //merge eventclass centrality
      ///////////////////////
      for(int icent=0;icent<ncent;icent++){
	if(j==0) sprintf(name,"C%dZ%dD%dQx",icent,izv,idet);    
	else if(j==1) sprintf(name,"C%dZ%dD%dQy",icent,izv,idet);
	else sprintf(name,"C%dZ%dQw",icent,izv);
	QA[j]=(TProfile*)f->Get(name);
	qa[izv+5][j]->Add(QA[j]);
      }
      c[j]->cd(izv+6);
      if(j<2){
	qa[izv+5][j]->SetMinimum(min);
	qa[izv+5][j]->SetMaximum(max);
      }
      else{
	qa[izv+5][j]->SetMinimum(0.0);
	qa[izv+5][j]->SetMaximum(max_w);
      }
      qa[izv+5][j]->Draw();
    }
  }
  TCanvas *a;
  TH1F* hist;

  a=new TCanvas;
  //hist = new TH1F("Qw","Qw",1,0,ndet);
  //hist->SetMaximum(max_w);
  //hist->Draw();
  qa[1][2]->SetMarkerStyle(20);
  qa[1][2]->Draw();
}
  
