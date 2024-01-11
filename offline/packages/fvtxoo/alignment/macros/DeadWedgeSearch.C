
#include <cassert>
#include <string>
#include "SaveCanvas.C"
#include "SetOKStyle.C"

using namespace std;

void
DeadWedgeSearch(string input = "FvtxGlobalAlign_evl.root", int sign = +1)
{
  TFile *_file0 = TFile::Open("FvtxGlobalAlign.root");

  TTree * misalignment = (TTree *)_file0->GetObjectChecked("misalignment","TTree");
  const int n = misalignment->
      Draw("arm:cage:station:sector","is_fvtx_wedge && nb_tracks<10","GOFF");

  string outfile = input +".DeadWedgeSearch.txt";
  fstream out(outfile.c_str(), ios_base::out);

  Int_t arm = -1;
  Int_t cage = -1;
  Int_t station = -1;
  Int_t sector = -1;

  for (int i = 0; i<n; i++)
    {
      if (
          arm == misalignment->GetV1()[i] &&
          cage == misalignment->GetV2()[i] &&
          station == misalignment->GetV3()[i] &&
          sector == misalignment->GetV4()[i]
                                          )
        continue;
      else
        {

          arm = misalignment->GetV1()[i] ;
          cage = misalignment->GetV2()[i] ;
          station = misalignment->GetV3()[i] ;
          sector = misalignment->GetV4()[i] ;

          out
              << arm << "\t" //
              << cage << "\t" //
              << station << "\t" //
              << sector << "\t" //
              <<endl;

        }


    }

  cout << "Done (" << n << " dead sides)." << endl;

  cout << "Saved to " << outfile << endl;
}
