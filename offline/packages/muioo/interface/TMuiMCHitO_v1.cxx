#include <TMuiMCHitO_v1.h>
//#include <TMutMCTrkMap.h>
#include<PHKeyIterator.h>
#include<PHConstKeyIterator.h>


ClassImp(TMuiMCHitO_v1)

using namespace std;

/*! default constructor */
TMuiMCHitO_v1::TMuiMCHitO_v1() : _arm(0),
				 _plane(0),
				 _index(0),
				 _track_id(0),
				 _x(0),
				 _y(0),
				 _z(0),
				 _px(0),
				 _py(0),
				 _pz(0),
				 _pid(0),
				 _file_key(0)
{
  // nothing being done right now
}

TMuiMCHitO_v1::TMuiMCHitO_v1(const Key& key, 
			     UShort_t arm, 
			     UShort_t plane, 
			     UShort_t index) : 
  TMuiMCHitO(key), 
  _arm(arm), 
  _plane(plane),
  _index(index),
  _track_id(0),
  _x(0),
  _y(0),
  _z(0),
  _px(0),
  _py(0),
  _pz(0),
  _pid(0),
  _file_key(0)
{
  // nothing being done right now
}

TMuiMCHitO_v1::TMuiMCHitO_v1(const TMuiMCHitO* base_ptr) : 
  TMuiMCHitO(*base_ptr),
  _arm(base_ptr->get_arm()),
  _plane(base_ptr->get_plane()),
  _index(base_ptr->get_index()),
  _track_id(base_ptr->get_track_id()),
  _x(base_ptr->get_x()),
  _y(base_ptr->get_y()),
  _z(base_ptr->get_z()),
  _px(base_ptr->get_px()),
  _py(base_ptr->get_py()),
  _pz(base_ptr->get_pz()),
  _pid(base_ptr->get_pid()),
  _file_key(base_ptr->get_file_key()),
  _twopack_list(*(base_ptr->get_twopack_list()))  
{
}

TMuiMCHitO_v1::TMuiMCHitO_v1(const TMuiMCHitO& base_ref) : 
  TMuiMCHitO(base_ref),
  _arm(base_ref.get_arm()),
  _plane(base_ref.get_plane()),
  _index(base_ref.get_index()),
  _track_id(base_ref.get_track_id()),
  _x(base_ref.get_x()),
  _y(base_ref.get_y()),
  _z(base_ref.get_z()),
  _px(base_ref.get_px()),
  _py(base_ref.get_py()),
  _pz(base_ref.get_pz()),
  _pid(base_ref.get_pid()),
  _file_key(base_ref.get_file_key()),
  _twopack_list(*(base_ref.get_twopack_list()))  
{

}

void TMuiMCHitO_v1::print(std::ostream& os) const {		       

  MUIOO::PRINT(os,GetName());
  os << " arm: " << _arm
     << " plane: " << _plane
     << " index: " << _index << std::endl;
  os << " trk_index : " << _track_id << std::endl;
  os << " p = {" << _px << "," << _py << "," << _pz << "}" << std::endl;
  os << " x = {" << _x << "," << _y << "," << _z << "}" << std::endl;
  os << " pid : " << _pid << std::endl;
  os << " file_key : " << _file_key << std::endl;

  // Print TMuiMCTwopacks
  //
  const twopack_list* twopack = get_twopack_list();  
  twopack_iterator twopack_iter = twopack->begin();  
  for(;twopack_iter!=twopack->end();++twopack_iter){    
    twopack_iter->print(os);
  }

  //  os << " #### dump associated mctrk information #### " <<endl;
  //  TMutMCTrkMap::const_key_iterator mc_trk_iter = get_associated<TMutMCTrk>();
  //  os << " number of associated mctrk : " << mc_trk_iter.count() << endl; 
  //  while(TMutMCTrkMap::const_pointer mc_trk_ptr = mc_trk_iter.next()) {
  //      mc_trk_ptr->get()->print();
  //    }
  MUIOO::PRINT(os,"**");
}









