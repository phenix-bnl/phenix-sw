#include <iostream>
#include <vector>
#include <unistd.h>
#include <stdio.h>
#include <string.h>


#include "TROOT.h"
#include "TObject.h"
#include "TDirectory.h"
#include "TFile.h"
#include "TKey.h"
#include "TString.h"

#include "TTree.h"
#include "TNtuple.h"
#include "TChain.h"
#include "TH1.h"
#include "TH2.h"
#include "TH3.h"


class objectstat{
public :
  TString name;
  int type;
  
  TChain *chain;
  TH1F *th1;
  TH2F *th2;
  TH3F *th3;
 
  objectstat(TObject *obj);
  ~objectstat(){};
  void Add(TObject *obj);
  void Save(TFile *fout);
};

objectstat::objectstat(TObject *obj){
  //  object = obj;
  name = obj->GetName();

  TString s = obj->ClassName();
  const char *sdata = s.Data();
  TString shead =  sdata[0];
  shead.Append(sdata[1]);
  shead.Append(sdata[2]);

  if(s=="TTree"){
    chain = new TChain(name);
    type = 1;
    Add(obj);
  }
  else if(s=="TNtuple"){
    chain = new TChain(name);
    type = 2;
    Add(obj);
  }
  else if(s=="TChain"){
    chain = new TChain(name);
    type = 3;
    Add(obj);
  }
  else if(shead=="TH1" ){
    th1 = (TH1F*) obj->Clone(name);
    type = 4;
  }
  else if(shead=="TH2" ){
    th2 = (TH2F*) obj->Clone(name);
    type = 5;
  } 
  else if(shead=="TH3" ){
    th3 = (TH3F*) obj->Clone(name);
    type = 6;
  } 
  else 
    type = 0;
}

void objectstat::Add(TObject *obj){
  switch(type){
  case 1:  chain->Add((((TTree*)obj)->GetDirectory())->GetName()); break;
  case 2:  chain->Add((((TTree*)obj)->GetDirectory())->GetName()); break;
  case 3:  chain->Add((TChain*)obj); break;
  case 4:  th1->Add((TH1F*) obj); break;
  case 5:  th2->Add((TH2F*) obj); break;
  case 6:  th3->Add((TH3F*) obj); break;
  default : break;
  }
}

void objectstat::Save(TFile *fout){
  switch(type){
  case 1:  chain->Write(); break;
  case 2:  chain->Write(); break;
  case 3:  chain->Write(); break;
  case 4:  th1->Write(); break;
  case 5:  th2->Write(); break;
  case 6:  th3->Write(); break;
  default : break;
  }  
}

void print_help(void){
  cout << "usage : merger [option] <merge file1> <merge file2> ..." << endl;
  cout << "   options" << endl;
  cout << "      -o <file> : output file is set to <file> (default : default.root)" << endl;
  cout << "      -h        : print this message" << endl;
    
}

void main(int argc,char **argv){

  char ofname[200] = "default.root";
  int found;

  int c;

  TKey *key;
  TObject* obj;
  vector <objectstat> objstat;
  TFile *fin;  

  while((c = getopt(argc, argv, "o:h")) != EOF){
    switch(c){
    case 'o':
      strcpy(ofname,optarg);
      break;
    case 'h':
      print_help();
      exit(0);
      break;
    }
  }

  if(argc - optind < 2){
      print_help();
      exit(0);
  }

  TDirectory *topdir = new TDirectory("TOP","Top direcotry");
  for(; optind < argc; optind++){
    printf("processing %s\n", argv[optind]);
    fin = new TFile(argv[optind]);
    TIter next(fin->GetListOfKeys());

    while ((key = (TKey *) next())) {

      obj = key->ReadObj();
      
      found = 0;  
      for(int i=0;i < objstat.size();i++)
	if(obj->GetName()==objstat[i].name){
	  objstat[i].Add(obj);
	  found = 1;
	}
      if(found == 0){
	topdir->cd();
	objstat.push_back(objectstat(obj));
	fin->cd();
      }
    }

    fin->Close();    
  }

  TFile *fout = new TFile(ofname,"RECREATE");
  for(int i=0;i < objstat.size();i++)
    objstat[i].Save(fout);
  fout->Close();
}
