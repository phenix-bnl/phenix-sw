////////////////////////////////////////////////
//
// This code merges the TProfile's in TTree's 
// output by SvxStabilityQA
//
////////////////////////////////////////////////
//
// Darren McGlinchey
// 6-20-2013
// 
////////////////////////////////////////////////

#include <TFile.h>
#include <TTree.h>
#include <TProfile.h>
#include <TList.h>
#include <TBenchmark.h>

#include <fstream>
#include <iostream>
#include <vector>

using namespace std;

void merge_unstable_ttree(
 const char* outFileName = "merge_test.root",
 const char* inFileList = "tmp.txt"
 )
{

  bool run_pixels = false;

  //First make a list of all the files to be added
  TList* fileList = new TList();

  ifstream inFile;
  inFile.open(inFileList);

  if (!inFile)
  {
    cout << "ERROR!! Unable to open inFileList=" << inFileList << endl;
    return;
  }

  int nfiles = 0;
  string tmpfile;
  getline(inFile, tmpfile);
  while (inFile.good())
  {
    cout << " Opening ifile=" << nfiles << " named " << tmpfile << endl;
    fileList->Add( TFile::Open(tmpfile.c_str()) );

    nfiles++;

    getline(inFile, tmpfile);

  }
  cout << "--> Will merge " << nfiles << " files." << endl;

  // Make the output file and create our output tree
  cout << "--> Creating output file " << outFileName << " to which the merged tree will be written" << endl;
  TFile* fout = new TFile(outFileName, "RECREATE");

  TTree* merged_tree = new TTree("profiletree","Tree of profiles for each pixel or channel in the VTX");
  TProfile *merged_profile = 0;
  merged_tree->Branch("pixel","TProfile",&merged_profile);
  merged_tree->SetAutoFlush(1000);

  //get the tree from the first file, use this as the template for later trees
  TFile *first_input = (TFile*)fileList->First();
  
  TTree* first_tree = (TTree*) first_input->Get("profiletree");
  if (!first_tree)
  {
    cout << "ERROR!! Unable to find TTree with name 'profiletree' in first file." << endl;
    return;
  }

  TProfile* first_profile=0;
  first_tree->SetBranchAddress("pixel",&first_profile);

  //create a vector of pointers to all the tree's
  vector<TTree*> tree_vect;
  TProfile* tmp_profile = 0;
  TFile *next_input = (TFile*)fileList->After( first_input );
  while (next_input)
  {

    TTree* tmptree = (TTree*) next_input->Get("profiletree");

    if (!tmptree)
    {
     cout << "ERROR!! Unable to find TTree with name 'profiletree', moving to next file." << endl;
   }
   else
   {
     tmptree->SetBranchAddress("pixel",&tmp_profile);

     tree_vect.push_back(tmptree);
   }


   next_input = (TFile*)fileList->After( next_input );
 }


  //Loop over all entries in the tree and add up the TProfiles
 gBenchmark->Start("entrytimer");
 for (unsigned int ientry = 0; ientry < first_tree->GetEntries(); ientry++)
 {
  if (ientry%100000 == 0)
   cout << "-->Merging profiles from entry " << ientry << endl;

  if(ientry >= 3932160  || run_pixels) //first strip entry
  {
    first_tree->GetEntry(ientry);

    merged_profile = (TProfile*) first_profile->Clone();

    for ( std::vector<TTree*>::size_type  i = 0; i < tree_vect.size(); i++)
    {
      tree_vect[i]->GetEntry(ientry);
       merged_profile->Add(tmp_profile);
    }
  }
  merged_tree->Fill();

	if(ientry >= 3932160  || run_pixels) //first strip entry
  	delete merged_profile;
}


cout << "-->Writing merged tree to file" << endl;

fout->cd();
merged_tree->Write();

gBenchmark->Show("entrytimer");

}
