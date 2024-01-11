#include<iostream>
#include<map>

#include<PHKey.hh>
#include<TMutHit.hh>
#include<MUTOO.h>
#include<TRandom.h>
#include<TFile.h>
#include<TTree.h>
#include<TClonesArray.h>
#include<boost/timer.hpp>

/*! @defgroup test Test Programs
  These small routines are useful for understanding  usage semantics.
*/

/*! @ingroup test */

/*! \file testIORead.cxx
\brief test  ROOT I/O
*/

int main()
{
  MUTOO::TRACE("Reading back test_file.root");
  TClonesArray* b = new TClonesArray("TMutHit",3);
  new TFile("test_file.root");
  TTree* tree = (TTree*)gDirectory->Get("tree");  
  TBranch* branch = tree->GetBranch("clone_branch");
  branch->SetAddress(&b);  
  tree->GetEntry(0);
  for (Int_t i=0; i<b->GetEntries();i++) {
    TMutHit* hit_ptr  = static_cast<TMutHit*>(b->At(i));
    hit_ptr->print();
  } 
//    TObjArray* b = new TObjArray(3);
//    TFile* file_in_ptr = new TFile("test_file.root");
//    TTree* tree = (TTree*)gDirectory->Get("tree");  
//    TBranch* branch = tree->GetBranch("branch");
//    branch->SetAddress(&b);  
//    tree->GetEntry(0);
//    for (Int_t i=0; i<b->GetEntries();i++) {
//      TMutHit* hit_ptr  = static_cast<TMutHit*>(b->At(i));
//      hit_ptr->print();
//      hit_ptr->get_clus_key_list()->print();    
//    } 
}


