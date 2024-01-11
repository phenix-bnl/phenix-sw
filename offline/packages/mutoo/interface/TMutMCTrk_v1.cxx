// $Id: TMutMCTrk_v1.cxx,v 1.11 2011/12/29 20:19:30 slash Exp $

#include <MUTOO.h>
#include <TMutMCTrk_v1.hh>
#include <TMutTrkMap.h>
#include <TMutMCHitMap.h>

ClassImp(TMutMCTrk_v1);

/*! default constructor */
TMutMCTrk_v1::TMutMCTrk_v1() :	 
	_arm(0),
	_index(0),
	_track_id(0),
	_pid(0),
	_parent_id(0),
	_charge(0),
	_x_orig(0),
	_y_orig(0),
	_z_orig(0),
	_px_orig(0),
	_py_orig(0),
	_pz_orig(0),
	_x_us_gap(0),
	_y_us_gap(0),
	_z_us_gap(0),
	_px_us_gap(0),
	_py_us_gap(0),
	_pz_us_gap(0),
	_x_ds_gap(0),
	_y_ds_gap(0),
	_z_ds_gap(0),
	_px_ds_gap(0),
	_py_ds_gap(0),
	_pz_ds_gap(0){;}

TMutMCTrk_v1::TMutMCTrk_v1(const Key& key, 
				 UShort_t arm, 
				 UShort_t index) :
	TMutMCTrk(key), 
	_arm(arm), 
	_index(index),
	_track_id(0),
	_pid(0),
	_parent_id(0),
	_charge(0),
	_x_orig(0),
	_y_orig(0),
	_z_orig(0),
	_px_orig(0),
	_py_orig(0),
	_pz_orig(0),
	_x_us_gap(0),
	_y_us_gap(0),
	_z_us_gap(0),
	_px_us_gap(0),
	_py_us_gap(0),
	_pz_us_gap(0),
	_x_ds_gap(0),
	_y_ds_gap(0),
	_z_ds_gap(0),
	_px_ds_gap(0),
	_py_ds_gap(0),
	_pz_ds_gap(0){;}

TMutMCTrk_v1::TMutMCTrk_v1(const TMutMCTrk* base_ptr):
	TMutMCTrk(*base_ptr),
	_arm(base_ptr->get_arm()),
	_index(base_ptr->get_index()),
	_track_id(base_ptr->get_track_id()),
	_pid(base_ptr->get_pid()),
	_parent_id(base_ptr->get_parent_id()),
	_charge(base_ptr->get_charge()),
	_x_orig(base_ptr->get_x_orig()),
	_y_orig(base_ptr->get_y_orig()),
	_z_orig(base_ptr->get_z_orig()),
	_px_orig(base_ptr->get_px_orig()),
	_py_orig(base_ptr->get_py_orig()),
	_pz_orig(base_ptr->get_pz_orig()),
	_x_us_gap(base_ptr->get_x_us_gap()),
	_y_us_gap(base_ptr->get_y_us_gap()),
	_z_us_gap(base_ptr->get_z_us_gap()),
	_px_us_gap(base_ptr->get_px_us_gap()),
	_py_us_gap(base_ptr->get_py_us_gap()),
	_pz_us_gap(base_ptr->get_pz_us_gap()),
	_x_ds_gap(base_ptr->get_x_ds_gap()),
	_y_ds_gap(base_ptr->get_y_ds_gap()),
	_z_ds_gap(base_ptr->get_z_ds_gap()),
	_px_ds_gap(base_ptr->get_px_ds_gap()),
	_py_ds_gap(base_ptr->get_py_ds_gap()),
	_pz_ds_gap(base_ptr->get_pz_ds_gap()){;}

TMutMCTrk_v1::TMutMCTrk_v1(const TMutMCTrk& base_ref):
	TMutMCTrk(base_ref),
	_arm(base_ref.get_arm()),
	_index(base_ref.get_index()),
	_track_id(base_ref.get_track_id()),
	_pid(base_ref.get_pid()),
	_parent_id(base_ref.get_parent_id()),
	_charge(base_ref.get_charge()),
	_x_orig(base_ref.get_x_orig()),
	_y_orig(base_ref.get_y_orig()),
	_z_orig(base_ref.get_z_orig()),
	_px_orig(base_ref.get_px_orig()),
	_py_orig(base_ref.get_py_orig()),
	_pz_orig(base_ref.get_pz_orig()),
	_x_us_gap(base_ref.get_x_us_gap()),
	_y_us_gap(base_ref.get_y_us_gap()),
	_z_us_gap(base_ref.get_z_us_gap()),
	_px_us_gap(base_ref.get_px_us_gap()),
	_py_us_gap(base_ref.get_py_us_gap()),
	_pz_us_gap(base_ref.get_pz_us_gap()),
	_x_ds_gap(base_ref.get_x_ds_gap()),
	_y_ds_gap(base_ref.get_y_ds_gap()),
	_z_ds_gap(base_ref.get_z_ds_gap()),
	_px_ds_gap(base_ref.get_px_ds_gap()),
	_py_ds_gap(base_ref.get_py_ds_gap()),
	_pz_ds_gap(base_ref.get_pz_ds_gap()){;}
	

	
void TMutMCTrk_v1::print(std::ostream& os) const {					 
	MUTOO::PRINT(os,GetName());
	os << " arm: " << _arm
		 << "	index: " << _index << std::endl;
	os << " track id: " << _track_id << " particle id : " << _pid << " parent id : " << _parent_id << std::endl;
	os << " charge: " << _charge << std::endl;
	os << " p orig = {" << _px_orig << "," << _py_orig << "," << _pz_orig << "}" << std::endl;
	os << " x orig = {" << _x_orig << "," << _y_orig << "," << _z_orig << "}" << std::endl;
	os << " p us gap = {" << _px_us_gap << "," << _py_us_gap << "," << _pz_us_gap << "}" << std::endl;
	os << " x us gap = {" << _x_us_gap << "," << _y_us_gap << "," << _z_us_gap << "}" << std::endl;
	os << " p ds gap = {" << _px_ds_gap << "," << _py_ds_gap << "," << _pz_ds_gap << "}" << std::endl;
	os << " x ds gap = {" << _x_ds_gap << "," << _y_ds_gap << "," << _z_ds_gap << "}" << std::endl;
	os << " ptot orig = " << get_ptot_orig() << "	ptot us = " << get_ptot_us_gap() << std::endl;
	os << " eloss absorber = " << get_ptot_orig() - get_ptot_us_gap() << std::endl;
	os << " eloss st2 = " << get_ptot_us_gap() - get_ptot_ds_gap() << std::endl;
	os << " Number of associated TMutMCHit = " << get_associated<TMutMCHit>().count() << std::endl;
	os << " Number of associated TMutTrk = " << get_associated<TMutTrk>().count() << std::endl;
	MUTOO::PRINT(os,"**");
}





