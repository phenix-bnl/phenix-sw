#include <stream.h>
#include <iostream>
#include <fstream>
#include <TObject.h>
#include <TFile.h>
#include <TNtuple.h>

void txt2ntuple(char* txtlist,char* outnt,int start=0,int end=0){
			    
  //
  char cline[256];
  char fname[256];
  int iline=0;
  char ctag;
  int bit;
  float e4x4, e2x2;
  int arm , sect, sm;
  int tr4a, tr4b, tr4c, tr2;
  int glmin, glg1, glg2, glch, glel, glphi;
  int e_arm, e_sect, e_sm;
  int evt, evt_read,trig;
  int nline = 0;
  //
  
  char* arglist =
    "arm:sect:sm:e4x4:e2x2:tr4a:tr4b:tr4c:tr2:evt:glmin:glg1:glg2:glch:glel:glphi:e_arm:e_sect:e_sm:bit";
  cout<<" Open output root file : "<<outnt<<endl;
  TFile* nf = new TFile(outnt,"RECREATE");
  TNtuple* nt = new TNtuple("nt", "txt2ntuple",arglist);
  float nt_nt[100];
  TNtuple* nt_evt = new TNtuple("nt_evt","Event ntuple","bit:evt:trig");
  
  cout<<" Open file "<<txtlist<<endl;
  ifstream flist(txtlist);

  while( flist.getline(fname,256) ){
    iline++;
    //    cout<<" reading line = "<<iline<<endl;
    if( ( iline >= start && iline <= end  ) || ( start == 0 && end==0 ) ){

      cout<<" Open txt file : "<<fname<<endl; 
      ifstream fin(fname);
      //      cout<<" looping............ "<<endl;

      while(fin>>bit){
	if( nline++ % 1000 == 0 ) cout<<" analyzing line : "<<nline-1<<endl;
	//cout<<" analyzing line : "<<nline++<<endl;
	//cout<<" bit = "<<bit<<endl;
	if( bit == 0 ){
	  fin>>evt_read>>trig;
	  nt_nt[0] = bit;
	  nt_nt[1] = evt;
	  nt_nt[2] = trig;
	  nt_evt->Fill(nt_nt);
	  //fin.getline(cline,256);
	  evt++;
	} else if( bit == 1 ){
	  fin>>arm>>sect>>sm;
	  fin>>e4x4>>tr4a>>tr4b>>tr4c>>e2x2>>tr2;
	  fin>>ctag;
	  fin>>glmin>>glg1>>glg2>>glch>>glel>>glphi;
	  //
	  //cout<<bit<<" ";
	  //cout<<arm<<" "<<sect<<" "<<sm<<" ";
	  //cout<<e4x4<<" "<<tr4a<<" "<<tr4b<<" "<<tr4c<<" "<<e2x2<<" "<<tr2<<" ";
	  //cout<<" : ";
	  //cout<<glmin<<" "<<glg1<<" "<<glg2<<" "<<glch<<" "<<glel<<" "<<glphi<<" ";
	  //cout<<" : ";
	  //cout<<e_arm<<" "<<e_sect<<" "<<e_sm;
	  //cout<<endl;
	  nt_nt[0] = arm;
	  nt_nt[1] = sect;
	  nt_nt[2] = sm;
	  nt_nt[3] = e4x4;
	  nt_nt[4] = e2x2;
	  nt_nt[5] = tr4a;
	  nt_nt[6] = tr4b;
	  nt_nt[7] = tr4c;
	  nt_nt[8] = tr2;
	  nt_nt[9] = evt;
	  nt_nt[10] = glmin;
	  nt_nt[11] = glg1;
	  nt_nt[12] = glg2;
	  nt_nt[13] = glel;
	  nt_nt[14] = glch;
	  nt_nt[15] = glphi;
	  nt_nt[16] = arm;
	  nt_nt[17] = sect;
	  nt_nt[18] = sm;
	  nt_nt[19] = bit;
	  nt->Fill(nt_nt);
	}else if( bit == 2 ){
	  fin.getline(cline,256);
	  evt++;
#ifdef SKIPSKIPKIS
	  fin>>arm>>sect>>sm;
	  fin>>e4x4>>tr4a>>tr4b>>tr4c>>e2x2>>tr2;
	  fin>>ctag;
	  fin>>glmin>>glg1>>glg2>>glch>>glel>>glphi;
	  fin>>ctag;
	  fin>>e_arm>>e_sect>>e_sm;
	  nt_nt[0] = arm;
	  nt_nt[1] = sect;
	  nt_nt[2] = sm;
	  nt_nt[3] = e4x4;
	  nt_nt[4] = e2x2;
	  nt_nt[5] = tr4a;
	  nt_nt[6] = tr4b;
	  nt_nt[7] = tr4c;
	  nt_nt[8] = tr2;
	  nt_nt[9] = evt;
	  nt_nt[10] = glmin;
	  nt_nt[11] = glg1;
	  nt_nt[12] = glg2;
	  nt_nt[13] = glel;
	  nt_nt[14] = glch;
	  nt_nt[15] = glphi;
	  nt_nt[16] = arm;
	  nt_nt[17] = sect;
	  nt_nt[18] = sm;
	  nt_nt[19] = bit;
	  
	  nt->Fill(nt_nt);
#endif
	}
      } // End of looping.....
   
      fin.close();

    }
  } // End of looping
   
  flist.close();

  cout<<" Close output file : "<<nf->GetName()<<endl;
  nf->cd();
  nt->Write();
  nt_evt->Write();
  nf->Close();
  //  delete nt;
  //  delete nf;
  //
}
//
