#include<iostream>
#include<map>

#include<PHKey.hh>
#include<TMutHit.hh>
#include<TRandom.h>
#include<TFile.h>
#include<TTree.h>
#include<TClonesArray.h>
#include<boost/timer.hpp>
#include<TMutMapIO.h>

/*! @defgroup test Test Programs
  These small routines are useful for understanding  usage semantics.
*/

/*! @ingroup test */

/*! \file testIOWrite.cxx
\brief test ROOT I/O
*/
void readback_clone();
void readback_obj();

typedef TMutHit STREAMED_TYPE;

int main()
{

  MUTOO::TRACE("Test TMutMapIO");
  MUTOO::TRACE("Generating test_file.root");
  TFile* file = new TFile("test_file.root", "RECREATE");
  TTree* tree_ptr = new TTree("tree", "TestTree");  
  
  // select mode and instantiate io utility
  //

  TMutMapIO<STREAMED_TYPE> io(1000);
  io.initialize();
  MUTOO::TRACE(io.get_array()->ClassName());
  tree_ptr->Branch("branch", io.get_array()->ClassName(), io.get_array_address(),8000,1);  
  
  for(int i=0;i<3;i++){
    STREAMED_TYPE* hit_ptr = new STREAMED_TYPE();
    hit_ptr->set_arm(0);
    hit_ptr->set_station(1);
    hit_ptr->set_octant(2);
    hit_ptr->set_half_octant(1);
    hit_ptr->set_gap(1);
    hit_ptr->set_cathode(0);
    hit_ptr->set_strip(i);
    hit_ptr->set_key(Key(123,456));
    hit_ptr->print();    
    io.insert(hit_ptr);
    delete hit_ptr;
  }
  tree_ptr->Fill();
  tree_ptr->Write();  
  file->Close();

  MUTOO::TRACE("Finished Generating test_file.root");

  readback_clone();
  
}

void readback_clone() {

  MUTOO::TRACE("starting readback");

  TFile* file = new TFile("test_file.root");
  TTree* tree = (TTree*)gDirectory->Get("tree");  
  TBranch* branch = tree->GetBranch("branch");

  TClonesArray* b = new TClonesArray(STREAMED_TYPE().GetName(),3);
  branch->SetAddress(&b);  
  tree->GetEntry(0);
  for (Int_t i=0; i<b->GetEntries();i++) {
    TMutHit* hit_ptr  = static_cast<TMutHit*>(b->At(i));
    hit_ptr->print();
  } 

}
void readback_obj() {

  MUTOO::TRACE("starting readback");

  TFile* file = new TFile("test_file.root");
  TTree* tree = (TTree*)gDirectory->Get("tree");  
  TBranch* branch = tree->GetBranch("branch");

  TObjArray* b = new TObjArray();; 
  branch->SetAddress(&b);  
  tree->GetEntry(0);
  for (Int_t i=0; i<b->GetEntries();i++) {
    TMutHit* hit_ptr  = static_cast<TMutHit*>(b->At(i));
    hit_ptr->print();
  } 

}








