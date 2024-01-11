#include "Coefficient.h"
#include "UncalibTowerList.h"

#include <TH1D.h>
#include <TH2D.h>
#include <TStyle.h>
#include <TCanvas.h>
#include <TFile.h>


#include <cstdlib>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;
using namespace EmcAnaCommon;

Coefficient::Coefficient()
{

  m_uncalib_list = new UncalibTowerList();


   m_coef = new Double_t[N_TOWER];
   m_coef_err = new Double_t[N_TOWER];

   for(int i_twr = 0; i_twr < N_TOWER; i_twr++){
     m_coef[i_twr] = 1;
     m_coef_err[i_twr] = 0;
   }

   
   m_coef_supermod = new Double_t *[N_ARMSECT];
   m_coef_supermod_err = new Double_t *[N_ARMSECT];

   for(int i_as = 0; i_as < N_ARMSECT; i_as++){
     m_coef_supermod[i_as] = new Double_t [N_SUPERMOD];
     m_coef_supermod_err[i_as] = new Double_t [N_SUPERMOD];
   }

   for(int i_as = 0; i_as < N_ARMSECT; i_as++){
     for(int i_sm = 0; i_sm < N_SUPERMOD; i_sm++){
       m_coef_supermod[i_as][i_sm] = 1;
       m_coef_supermod_err[i_as][i_sm] = 0;
     }
   }

						
   ResetCoef();
}

Coefficient::~Coefficient()
{
   delete [] m_coef;
   delete [] m_coef_err;

   for(int i_as = 0; i_as < N_ARMSECT; i_as++){
     delete [] m_coef_supermod[i_as];
     delete [] m_coef_supermod_err[i_as];
   }
   delete [] m_coef_supermod;
   delete [] m_coef_supermod_err;

   delete m_uncalib_list;


}

void Coefficient::WriteCoef(const Char_t* coef_table, const Char_t* coef_table_supermod)
{
   ofstream of_coef_table(coef_table);
   if (of_coef_table.is_open())
      cout << "Coefficient::WriteConf():" << endl
           << "  Open '" << coef_table << "'." << endl;
   else
      return;
   
   for (int towerid = 0; towerid < N_TOWER; towerid++) {
      int as, y, z;
      TowerID2AsYZ(towerid, as, y, z);
      of_coef_table << as << "  " << y << "  " << z << "  " 
                    << GetCoef(towerid) << "  " << GetCoefErr(towerid) << endl;
   }
   of_coef_table.close();

   ofstream of_coef_table_supermod(coef_table_supermod);
   if (of_coef_table_supermod.is_open())
      cout << "Coefficient::WriteConf():" << endl
           << "  Open '" << coef_table_supermod << "'." << endl;
   else
      return;
   
   for (int i_as = 0; i_as < N_ARMSECT; i_as++) {
     for (int i_sm = 0; i_sm < N_SUPERMOD; i_sm++) {
       if(!IsValidSM(i_as,i_sm)){continue;}
       of_coef_table_supermod << i_as << "  " << i_sm << "  " 
			      << GetCoefSm(i_as, i_sm) << "  " << GetCoefSmErr(i_as,i_sm) << endl;
     }
   }
   of_coef_table_supermod.close();
}

void Coefficient::ReadCoef(const Char_t* coef_table, const Char_t* coef_table_supermod)
{
   ifstream if_coef_table(coef_table);
   if (if_coef_table.is_open())
      cout << "Coefficient::ReadConf():" << endl
           << "  Open '" << coef_table << "'." << endl;
   else
      return;

   char line[1000];
   Int_t as, y, z;
   Double_t coef;
   Double_t coef_err;
   float _coef;
   float _coef_err;
   while (if_coef_table.getline(line,1000)){
     sscanf(line,"%d  %d  %d  %f  %f",&as,&y,&z,&_coef,&_coef_err);
     coef = (Double_t)_coef;
     coef_err = (Double_t)_coef_err;

     Int_t towerid = AsYZ2TowerID(as, y, z);
     if (0 <= towerid && towerid < N_TOWER) {
       SetCoef(towerid, coef, coef_err);
     } else {
       cerr << "Bad towerid; (armsect ypos zpos towerid) = ("
	    << as << " " << y << " " << z << " " << towerid << ")"
	    << endl << "Abort." << endl;
       exit(0);
     }
   }

   //supermod
   ifstream if_coef_table_supermod(coef_table_supermod);
   if (if_coef_table_supermod.is_open())
      cout << "Coefficient::ReadConf():" << endl
           << "  Open '" << coef_table_supermod << "'." << endl;
   else
      return;

   Int_t sm;
   while (if_coef_table_supermod.getline(line,1000)){
     sscanf(line,"%d  %d  %f  %f",&as,&sm,&_coef,&_coef_err);
     coef = (Double_t)_coef;
     coef_err = (Double_t)_coef_err;

     if ((0 <= as && as < N_ARMSECT)&&IsValidSM(as,sm)) {
       SetCoefSm(as, sm, coef, coef_err);
     } else {
       cerr << "Nonexistant SM; (armsect sm) = ("
	    << as << " " << sm << ")"
	    << endl << "Abort." << endl;
       exit(0);
     }
   }
}

Double_t Coefficient::GetCoef(const Int_t towerid)
{
 
  if (0 <= towerid && towerid < N_TOWER){
    if(m_coef[towerid]<-998){
      Int_t as;
      Int_t y, z;
      TowerID2AsYZ(towerid, as, y, z);
      return m_coef_supermod[as][GetSuperModule(as,y,z)];
    } else return m_coef[towerid];    
  }
  else return -1;

}

Double_t Coefficient::GetCoefSm(const Int_t as, const Int_t sm)
{
 
  if (IsValidSM(as,sm)){
    return m_coef_supermod[as][sm];
  } else return -1;
}

Double_t Coefficient::GetCoefSmErr(const Int_t as, const Int_t sm)
{
 
  if ((as==4||as==5)&&(0 <= sm && sm < 24)){
    return m_coef_supermod_err[as][sm];
  } else if ((0 <= as && as < 8)&&(0 <= sm && sm < 18)){
    return m_coef_supermod_err[as][sm];
  } else return -1;
}

Double_t Coefficient::GetCoef(const Int_t as, const Int_t y, const Int_t z)
{
   return GetCoef(AsYZ2TowerID(as, y, z));
}

Double_t Coefficient::GetCoefErr(const Int_t towerid)
{
   if (0 <= towerid && towerid < N_TOWER) return m_coef_err[towerid];
   else return -1;
}

Double_t Coefficient::GetCoefErr(const Int_t as, const Int_t y, const Int_t z)
{
   return GetCoefErr(AsYZ2TowerID(as, y, z));
}

void Coefficient::ResetCoef()
{
  for (Int_t itower = 0; itower < N_TOWER; itower++) {
    m_coef[itower] = -999.0;
    m_coef_err[itower] = 0.0;
  }
}

void Coefficient::SetCoef(const Int_t towerid, const Double_t coef, const Double_t coef_err)
{ 
   m_coef[towerid] = coef; 
   m_coef_err[towerid] = coef_err;
}

void Coefficient::SetCoefSm(const Int_t as, const Int_t sm, const Double_t coef, const Double_t coef_err)
{ 
   m_coef_supermod[as][sm] = coef; 
   m_coef_supermod_err[as][sm] = coef_err;
}

void Coefficient::Draw(const Char_t* fname_1d, const Char_t* fname_2d,  const Char_t* uncalib_list, const Char_t* froot_name)
{

  m_uncalib_list->ReadUncalibList(uncalib_list);

   extern TStyle* gStyle;
   gStyle->SetOptStat(0);
   gStyle->SetPalette(1);
   


   TH1D* h1_coef = new TH1D("h1_coef", ";coefficient;towers", 500, 0, 4);
   TH1D* h1_coef_err = new TH1D("h1_coef",";coef.uncerainty;towers", 500, 0 ,0.3);

   TH2D* h2_coef[N_ARMSECT];
   TH2D* h2_coef_err[N_ARMSECT];
   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      int ny, nz;
      if (IsPbGl(ias)) { ny = N_YPOS_PBGL;  nz = N_ZPOS_PBGL; }
      else             { ny = N_YPOS_PBSC;  nz = N_ZPOS_PBSC; }

      Char_t hname[256], htitle[256];
      sprintf(hname,  "h2_coef_as%i", ias);
      sprintf(htitle, "%s;zpos;ypos", SECTOR_NAME[ias]);
      h2_coef[ias] = new TH2D(hname, htitle, nz, 0, nz, ny, 0, ny);

      sprintf(hname,  "h2_coef_err_as%i", ias);
      sprintf(htitle, "%s;zpos;ypos", SECTOR_NAME[ias]);
      h2_coef_err[ias] = new TH2D(hname, htitle, nz, 0, nz, ny, 0, ny);
   }

   for (int itwr = 0; itwr < N_TOWER; itwr++) {
      int as, y, z;
      TowerID2AsYZ(itwr, as, y, z);
      if (IsEdgePos(as, y, z)) continue;

      double coef = GetCoef(itwr);
      double coef_err = GetCoefErr(itwr);

      if (!(m_uncalib_list->IsInUncalibList(itwr))){
	h1_coef->Fill(coef);
	h1_coef_err->Fill(coef_err);
      } else {
	coef = 0;
	coef_err = 0;
      }
      
      h2_coef[as]->SetBinContent(z + 1, y + 1, coef);
      h2_coef_err[as]->SetBinContent(z + 1, y + 1, coef_err);
   }
   


   TCanvas* c1_coef = new TCanvas("c1_coef", "");
   c1_coef->Divide(2,1);
   c1_coef->SetLogy(kTRUE);
   c1_coef->cd(1);
   h1_coef->Draw();
   c1_coef->cd(2);
   h1_coef_err->Draw();
   
   c1_coef->SaveAs(fname_1d);

   c1_coef->SetLogy(kFALSE);
   c1_coef->Clear();
   c1_coef->Divide(2, 4);

   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      c1_coef->cd( ias < 4  ?  2*(4-ias)-1  :  2*(8-ias) );
      h2_coef[ias]->GetZaxis()->SetRangeUser(0.5, 1.5);
      h2_coef[ias]->Draw("colz");
   }
   c1_coef->SaveAs(fname_2d);


   cout<<"about to write"<<endl;
   if(strcmp(froot_name,"")){
     TFile * fout = new TFile(froot_name,"RECREATE");
     char hname[200];
     for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
       sprintf(hname,"h2_coef%d",ias);
       fout->WriteObject(h2_coef[ias],hname);
       sprintf(hname,"h2_coef_err%d",ias);
       fout->WriteObject(h2_coef_err[ias],hname);

     }
     fout->WriteObject(h1_coef,"h1_coef");
     fout->WriteObject(h1_coef_err,"h1_coef_err");
     fout->Close();
   }
   cout<<"finished writing"<<endl;

   
   delete c1_coef;
   
   delete h1_coef;
   delete h1_coef_err;
   for (Int_t ias = 0; ias < N_ARMSECT; ias++){
     delete h2_coef[ias];
     delete h2_coef_err[ias];

   }
   
}

void Coefficient::DrawDiff(const Char_t* fname_coef_table_base, const Char_t* ofname)
{
   Coefficient* coef_base = new Coefficient();
   coef_base->ReadCoef(fname_coef_table_base,"NULL");

   TH1D* h1_coef_diff[N_ARMSECT];
   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      ostringstream oss_name;
      oss_name << "h1_coef_diff_as" << ias;
      ostringstream oss_title;
      oss_title << SECTOR_NAME[ias]
                << ";coefficient difference;towers";
      h1_coef_diff[ias] = new TH1D(oss_name.str().c_str(),
                                   oss_title.str().c_str(), 100, -0.5, 0.5);
   }
   for (Int_t itwr = 0; itwr < N_TOWER; itwr++) {
      Int_t as, y, z;
      TowerID2AsYZ(itwr, as, y, z);
      Double_t c  = this     ->GetCoef(itwr);
      Double_t cb = coef_base->GetCoef(itwr);

      if (! IsEdgePos(as, y, z) && c != 1 && cb != 1) {
         h1_coef_diff[as]->Fill(c - cb);
      }
   }

   TCanvas* c1_coef_diff = new TCanvas("c1_coef_diff", "");
   c1_coef_diff->Divide(4, 2);
   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      c1_coef_diff->cd(ias + 1);
      h1_coef_diff[ias]->Draw();
   }
   c1_coef_diff->SaveAs(ofname);

   delete c1_coef_diff;
   delete coef_base;
}
