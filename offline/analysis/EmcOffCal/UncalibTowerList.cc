#include "Warnmap.h"
#include "UncalibTowerList.h"

#include <TFile.h>
#include <TH2D.h>
#include <TTree.h>
#include <TChain.h>
#include <TCanvas.h>

#include <TSystem.h>
#include <TStyle.h>
#include <TVector3.h>

#include <cstdio>
#include <iostream>
#include <iomanip>
#include <fstream>
#include <cstring>

using namespace std;
using namespace EmcAnaCommon;

UncalibTowerList::UncalibTowerList()
{
   ;
}

UncalibTowerList::~UncalibTowerList()
{
   ;
}

void UncalibTowerList::ReadUncalibList(const Char_t* fn_uncalib)
{
   ClearUncalibList();

   ifstream if_list(fn_uncalib);
   if (if_list.is_open())
      cout << "UncalibTowerList::ReadUncalibList():" << endl
           << "  Open '" << fn_uncalib << "'." << endl;
   else
      return;

   Int_t as, y, z, reason;
   while (if_list >> as >> y >> z >> reason) {
      AddUncalibList( AsYZ2TowerID(as, y, z), reason );
   }
   if_list.close();
}

void UncalibTowerList::WriteUncalibList(const Char_t* fn_uncalib)
{
   ofstream of_list(fn_uncalib);

   for (UncalibListMap_t::iterator ient = m_towerid_uncalib.begin();
        ient != m_towerid_uncalib.end(); ient++) {
      Int_t as, y, z;
      TowerID2AsYZ(ient->first, as, y, z);
      of_list << as << "  " << y << "  " << z << "  " << ient->second << endl;
   }
   of_list.close();
}

void UncalibTowerList::AddUncalibList(Int_t towerid, Int_t reason)
{
   m_towerid_uncalib[towerid] = reason;
}

void UncalibTowerList::AddUncalibList(Int_t as, Int_t y, Int_t z, Int_t reason)
{
   m_towerid_uncalib[AsYZ2TowerID(as, y, z)] = reason;
}

Int_t UncalibTowerList::IsInUncalibList(Int_t towerid)
{
   if ( m_towerid_uncalib.find(towerid) != m_towerid_uncalib.end() )
      return m_towerid_uncalib[towerid];
   else return false;
}

Int_t UncalibTowerList::IsInUncalibList(Int_t as, Int_t y, Int_t z)
{
   return IsInUncalibList(AsYZ2TowerID(as, y, z));
}

void UncalibTowerList::Draw(const Char_t* fname)
{
   extern TStyle* gStyle;
   gStyle->SetOptStat(0);
   gStyle->SetPalette(1);

   TH2D* h2_towers[N_ARMSECT];
   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      int ny, nz;
      if (IsPbGl(ias)) { ny = N_YPOS_PBGL;  nz = N_ZPOS_PBGL; }
      else             { ny = N_YPOS_PBSC;  nz = N_ZPOS_PBSC; }

      Char_t hname[256], htitle[256];
      sprintf(hname,  "h2_towers_as%i", ias);
      sprintf(htitle, "%s;zpos;ypos", SECTOR_NAME[ias]);
      h2_towers[ias] = new TH2D(hname, htitle, nz, 0, nz, ny, 0, ny);
   }

   for (UncalibListMap_t::iterator ient = m_towerid_uncalib.begin();
        ient != m_towerid_uncalib.end(); ient++) {
      Int_t as, y, z;
      TowerID2AsYZ(ient->first, as, y, z);
//      h2_towers[as]->SetBinContent(z + 1, y + 1, ient->second);
      h2_towers[as]->SetBinContent(z + 1, y + 1, 1);
   }

   TCanvas* c1_towers = new TCanvas("c1_towers", "");
   c1_towers->Divide(2, 4);

   for (Int_t ias = 0; ias < N_ARMSECT; ias++) {
      c1_towers->cd( ias < 4  ?  2*(4-ias)-1  :  2*(8-ias) );
//      h2_towers[ias]->GetZaxis()->SetRangeUser(0, 4);
//      h2_towers[ias]->Draw("colz");
      h2_towers[ias]->Draw("col");
   }
   c1_towers->SaveAs(fname);

   delete c1_towers;
   for (Int_t ias = 0; ias < N_ARMSECT; ias++)
      delete h2_towers[ias];
}

void UncalibTowerList::Print(const Char_t* ofname, const Char_t* fn_warnmap)
{
   Warnmap* warnmap = new Warnmap();
   warnmap->ReadMap(fn_warnmap);

   Int_t n_uncalib_tot[N_ARMSECT + 1];
   Int_t n_uncalib_good[N_ARMSECT + 1];
   Int_t n_uncalib_bad[N_ARMSECT + 1];
   Int_t n_uncalib_around[N_ARMSECT + 1];
   memset(n_uncalib_tot,    0, sizeof(n_uncalib_tot));
   memset(n_uncalib_good,   0, sizeof(n_uncalib_good));
   memset(n_uncalib_bad,    0, sizeof(n_uncalib_bad));
   memset(n_uncalib_around, 0, sizeof(n_uncalib_around));

   for (UncalibListMap_t::iterator ient = m_towerid_uncalib.begin();
        ient != m_towerid_uncalib.end(); ient++) {
      Int_t as, y, z;
      TowerID2AsYZ(ient->first, as, y, z);

      n_uncalib_tot[8]++;
      n_uncalib_tot[as]++;

      if (warnmap->IsHot(as, y, z) || warnmap->IsDead(as, y, z)) {
         n_uncalib_bad[8]++;
         n_uncalib_bad[as]++;
      } else if (warnmap->IsAroundHot(as, y, z) || 
                 warnmap->IsAroundDead(as, y, z)) {
         n_uncalib_around[8]++;
         n_uncalib_around[as]++;
      } else {
         n_uncalib_good[8]++;
         n_uncalib_good[as]++;
      }
   }

   ////
   //// print
   ////
   ofstream of_uncalib(ofname);
   of_uncalib << "sector  total  good  hot/dead  around" << endl;

   for (Int_t ias = 0; ias < N_ARMSECT + 1; ias++) {
      of_uncalib << setw(6) 
                 << ( ias != N_ARMSECT  ?  SECTOR_NAME[ias]  :  "all" )
                 << "  " << setw(5) << n_uncalib_tot[ias]
                 << "  " << setw(4) << n_uncalib_good[ias] << "  "
                 << setw(8) << n_uncalib_bad[ias] << "  "
                 << setw(6) << n_uncalib_around[ias] << endl;
   }
   of_uncalib.close();
   delete warnmap;
}

void UncalibTowerList::ClearUncalibList()
{
   m_towerid_uncalib.clear();
}
