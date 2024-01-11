#ifndef __MMUTSTRIPOCCUP_HH__
#define __MMUTSTRIPOCCUP_HH__

//PHENIX
#include<TDataType.h>
#include<TMutHitMap.h>
#include<MUTOO_HASH_MAP.h>
#include<TH1.h>

class mMutStripOccupPar;
class PHCompositeNode;

//! store relevant occupancy data for histogram filling
class MutStripOccup 
{
 public:
  // constructor
  // 
  MutStripOccup(){}
  
  MutStripOccup(float occup_ava, int occup_sum, float q_ava, float q_rms, float length, TH1F* hist) :
    _occup_ava(occup_ava),
    _occup_sum(occup_sum),
    _q_ava(q_ava),
    _q_rms(q_rms),
    _length(length),
    _charge(hist){}
  

  // deconstructor
  //
  virtual ~MutStripOccup(){}

  // getters
  //
  float get_occup_ava() { return _occup_ava;}
  int   get_occup_sum() { return _occup_sum;}
  float get_q_ava() { return _q_ava;}
  float get_q_rms() { return _q_rms;}
  float get_length() { return _length;} 

  //setters 
  //
  void set_occup_ava(float occup_ava) { _occup_ava = occup_ava;}
  void set_occup_sum() { _occup_sum = _occup_sum + 1;}
  void set_q_ava() { _q_ava = _charge->GetMean();}
  void set_q_rms() { _q_rms = _charge->GetRMS();}
  void set_length(float length) { _length=length;}
  void set_charge(float charge) { _charge->Fill(charge);}
  // definer
  //
  void define_charge(TH1F *charge_hist) { _charge=charge_hist;}
   
 private:  
  float _occup_ava;
  int   _occup_sum;
  float _q_ava;
  float _q_rms;
  float _length;
  TH1F* _charge;

};
  
/*! 
	fill histograms for occupancy studies, out put a text file
	to evalute the statues of the strips.
*/
class mMutStripOccup
{
 public:
  typedef MUTOO::hash_map<ULong_t, MutStripOccup>::type Occup_map;
  mMutStripOccup();
  virtual ~mMutStripOccup(){}
  virtual PHBoolean event(PHCompositeNode*);
  void fill_hist(TString rootfile);
  void fill_text(TString filename);
  void clean_up();
  void set_interface_ptrs(PHCompositeNode* top_node);
  void fill_map();
  
 private:
  
  // Interface pointers
  const mMutStripOccupPar* _mod_par;
  TMutHitMap* _hit_map;
  int nevent;
  static Occup_map&  _occup_map();
  static Occup_map& init_map();
};

#endif  



















