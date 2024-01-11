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

int main()
{
  MUTOO::TRACE("Generating test_file.root");
  TFile* file = new TFile("test_file.root", "RECREATE");
  TTree* tree_ptr = new TTree("tree", "TestTree");  

  MUTOO::TRACE("Test TClonesArray");
  TObjArray* a = new TClonesArray("TMutHit",3);
  MUTOO::TRACE(a->ClassName());
  tree_ptr->Branch("branch", "TClonesArray", &a,8000,1);  
  for(int i=0;i<3;i++){
    new ((*a)[i]) TMutHit();
    TMutHit* hit_ptr  = static_cast<TMutHit*>(a->At(i));
    hit_ptr->set_arm(0);
    hit_ptr->set_station(i);
    hit_ptr->set_half_octant(i);
    hit_ptr->set_strip(i);
    hit_ptr->set_key(Key(123,456));
    hit_ptr->print();    
 }  
  
//    MUTOO::TRACE("Test TObjArray");
//    TObjArray* b = new TObjArray(1000);
//    tree_ptr->Branch("object_branch", "TObjArray", &b,1024,1);  
//    for(int i=0;i<3;i++){
//      TMutHit* foo = new TMutHit();
//      b->Add(foo);
//      //    (*b)[i] = new TMutHit();
//      TMutHit* hit_ptr  = static_cast<TMutHit*>(b->At(i));
//      hit_ptr->set_arm(0);
//      hit_ptr->set_station(i);
//      hit_ptr->set_plane(i);
//      hit_ptr->set_half_octant(i);
//      hit_ptr->set_strip(i);
//      hit_ptr->print();    
//      hit_ptr->get_clus_key_list()->print();    
//    }  
  
  tree_ptr->Fill();
  tree_ptr->Write();
  MUTOO::TRACE("Finished Generating test_file.root");

//    MUTOO::TRACE("Generating test_file.root");
//    file = new TFile("test_file2.root", "RECREATE");
//    tree_ptr = new TTree("tree", "TestTree");  
  
//    TMutMapIO<TMutHit> io;
  //    io.initialize();
//    io.insert(new TMutHit());
//    TClonesArray* array_ptr = io.get_array();
//    tree_ptr->Branch("clone_branch", "TClonesArray", &array_ptr,8000,1);  
//    tree_ptr->Fill();
//    tree_ptr->Write();

}









