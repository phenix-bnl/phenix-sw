#include "TFile.h"
#include "TTree.h"
#include <map>
#include <string>
#include <fstream>
// good for pAu
//int EVENT_SHIFT = 10000;
// good for pp
//int EVENT_SHIFT = 40000;
int EVENT_SHIFT = 0;

void write_misalign_text_new_format(const char* ifile = "misalignment_data/run15pp/haddout/421989.root",
                                    const char* ofile = "421989_ascii.txt")
{
  TFile* f = new TFile(ifile);

  int run(-9999), ladder(-9999), chip(-9999), n_misalign(-9999);
  int misalignments[1000][3] = {{0}};
  bool trash(false), died(false);

  TTree* tree = (TTree*)f->Get("misalignments");
  tree->SetBranchAddress("run",&run);
  tree->SetBranchAddress("ladder",&ladder);
  tree->SetBranchAddress("chip",&chip);
  tree->SetBranchAddress("n_misalign",&n_misalign);
  tree->SetBranchAddress("misalignments",&misalignments);
  tree->SetBranchAddress("trash",&trash);
  tree->SetBranchAddress("died",&died);

  int nchips = tree->GetEntries();

  // Using a multimap will automatically sort the entries by event number 
  // and allow multiple entries with that event number
  typedef std::multimap<int, std::string> map_type;
  map_type mis_map;

  for(int i = 0; i < nchips; ++i)
  {
    tree->GetEntry(i);
    
    if(n_misalign > 0 && !trash && !died)
    {
      int chip_number = ladder*16 + chip;

      for(int j = 0; j < n_misalign; ++j)
      {
        std::string str = Form("%d %d %d %d %d",run,chip_number,misalignments[j][0] + EVENT_SHIFT,-1,(-1)*misalignments[j][2]);
        mis_map.insert( map_type::value_type(misalignments[j][0], str) );
      }
    }
  }
  ofstream ofs;
  ofs.open (ofile);

  for(map_type::iterator map_it = mis_map.begin(); map_it != mis_map.end(); ++map_it)
  {
    ofs << (*map_it).second << endl;
  }
  ofs.close();
}
