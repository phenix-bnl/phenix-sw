#include "ana_dst.h"

// event loop globals
//
PHCompositeNode* mutoo_node;
PHCompositeNode* top_node;
PhMutooDisplay *display = 0;
static bool display_mode = false;

int process_event (PHCompositeNode *dst_node)
{
  
  try {  
    
    static int ievent=0;
    static bool init_done=false;
    if(!init_done) { 
      setup_all(dst_node);
      if(display_mode) setup_display();
      top_node->addNode(mutoo_node);
      top_node->addNode(dst_node);
      top_node->print();
      init_done=true;
    }  
    // Clear maps (IOCs) from last event
    //
    PHMapManager::clear();

    // This call fills the IOCs from the DST-resident objects
    //
    PHMapManager::read(dst_node);

    // Get the TMutTrkMap pointer from the node tree
    //
    TMutTrkMap* trk_map = TMutNode<TMutTrkMap>::find_node(mutoo_node,"TMutTrkMap");
    
    // The loop below is an example of how to access interface objects (IOs)
    // directly from their associated containers (IOC).  Here we use the range() 
    // common to all IOCs to get an iterator to all of the objects in the map. Note
    // the use of the trk_ptr->get(), this syntax is an attempt to amortize the cost
    // of using containers of (shared) pointers , hopefully this is better than 
    // (*trk_ptr)-> which also works. 

    // Loop over TMutTrk
    //
    TMutTrkMap::const_iterator trk_iter = trk_map->range();
    while(TMutTrkMap::const_pointer trk_ptr = trk_iter.next()){
      
      // Dump the contents of the track map to std::cout
      //
      trk_ptr->get()->print(std::cout, true);      

      // The loop below is an example of how to access interface objects via the 
      // object association technology used in MUTOO.  The call to get_associated
      // returns an iterator to any TMutCoord objects that are associated with the
      // TMutTrk.  Notice the iterator syntax is identical to that used for accessing
      // objects directly from their maps.  Also notice the actual interator object
      // is different, ie "const_key_iterator" as opposed to const_iterator.

      // Get the associated TMutCoord from the TMutTrk
      //
      TMutCoordMap::const_key_iterator coord_iter = trk_ptr->get()->get_associated<TMutCoord>();
      while(TMutCoordMap::const_pointer coord_ptr = coord_iter.next()){
	coord_ptr->get()->print();
      }

      // The loop below is an example of how to access members of an IO that are
      // stored in a Standard Library container (in MUTOO this is almost always 
      // a std::vector). The TMutTrk object has a variable length array 
      // (std::vector) of TMutTrkRes objects.  The syntax for iterating over a 
      // SL containers is explained in detail by any good C++ book and should be 
      // in the standard vocab of any C++ developer.

      // Loop over TMutTrk residuals and dump data into ntuple
      //
      const TMutTrk::residual_list* residuals = trk_ptr->get()->get_w_residual_list();
      TMutTrk::const_residual_iterator res_iter = residuals->begin();
      for(;res_iter!=residuals->end();++res_iter){      
	res_iter->print();
      }
    }
    
    // Do the display
    //
    if(display_mode) {
      gPhenix->SetRunNum(0);
      gPhenix->event(top_node);  
      gPhenix->Draw();
    }
    
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  
  return 0;  
}

int setup_all(PHCompositeNode* dst_node) {

  // create a new top node
  //
  top_node = new PHCompositeNode("NEWTOP");

  // create a node for MUTOO interface object containers
  //
  mutoo_node = new PHCompositeNode("MUTOO");

  try {
    // Create the the IOCs here and couple to input DST
    //
    TMutNode<TMutHitMap>::new_dst_input_node(mutoo_node,"TMutHitMap", dst_node,"TMutHit" );
    TMutNode<TMutClusMap>::new_dst_input_node(mutoo_node,"TMutClusMap", dst_node,"TMutClus");
    TMutNode<TMutCoordMap>::new_dst_input_node(mutoo_node,"TMutCoordMap", dst_node,"TMutCoord");
    TMutNode<TMutGapCoordMap>::new_dst_input_node(mutoo_node,"TMutGapCoordMap", dst_node,"TMutGapCoord");
    TMutNode<TMutTrkMap>::new_dst_input_node(mutoo_node, "TMutTrkMap",dst_node,"TMutTrk");
  } catch(std::exception& e) {
    MUTOO::TRACE(e.what());
  }
  std::cout<<"Haven't crashed yet\n" << std::endl;
  return 0;
};

int end_all() {
  cout<< "Still haven't crashed" << std::endl;
  return 0;
}

void setup_display() {
  
  if(!gClient){
    cout<<"you are running in batch"<<endl;
    return;
  }
  
  // This creates the gPhenix pointer
  //
  new PhenixRun("Phenix-Run","Phenix-Run",top_node);
  
  // Add MUTOO event display
  //
  display = new PhMutooDisplay();
  gPhenix->GetDisplay()->AddSubsystem(display);
  
  // Hack from dispfuncs.C
  //
  PhEventDisplay* disp = new PhEventDisplay("Main",
					    gClient->GetRoot(),
					    1000,
					    1000,
					    gPhenix->GetDisplay()->GetList());
  
  PhCanvas* can = (PhCanvas*)(disp->GetCanvas());
  can->DefaultRange(-550,-550,-550,550,550,550); //in cm
  can->ResetRange();
  disp->Popup();  
}

void draw_plane(UShort_t arm, UShort_t octant){
  display->paint_plane_view(arm,octant);
}

int dinit() {
  return 0;
}








