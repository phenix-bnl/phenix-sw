#ifndef __MFVTXFINDCLUSPAR_HH__
#define __MFVTXFINDCLUSPAR_HH__

#include<PHObject.h>
#include<FVTXOO.h>
#include<TFvtxParBase.h>
#include<TString.h>

class mFvtxFindClusPar : public TFvtxParBase
{
  
 public:
  
  // default constructor
  
  mFvtxFindClusPar() : 
    _auto_load_dead_map(kTRUE),_dead_map_name(""),
  _min_clus_width(1),
    _max_clus_width(10),
    _min_charge_sum(2),
    _min_gain(1),
    _merge_adjacent_clusters(true)
    {;}


  //destructor
  ~mFvtxFindClusPar() {;}

 
  /*! Minimum cluster width */
  unsigned short get_min_cluster_width() const {return _min_clus_width;} 
  
  /*! Maximum cluster width */
  unsigned short get_max_cluster_width() const {return _max_clus_width;} 
  
  /*! Minimum total charge */
  Float_t get_min_charge_sum() const {return _min_charge_sum;} 
  
  /*! Minimum gain for a channel to be considered alive */
  Float_t get_min_gain() const {return _min_gain;} 

  /*! Option to merge adjacent clusters if only one excusable missing strip */
  Bool_t get_merge_adjacent_clusters() const {return _merge_adjacent_clusters;}

  /*! Minimum cluster width */
  void set_min_cluster_width(unsigned short width) {_min_clus_width = width;}

  /*! Maximum cluster width */
  void set_max_cluster_width(unsigned short width) {_max_clus_width = width;}
  
  /*! Minimum cluster width */
  void set_min_charge_sum(unsigned short min_charge_sum) {_min_charge_sum = min_charge_sum;}

  /*! Minimum gain */
  void set_min_gain(unsigned short min_gain) {_min_gain = min_gain;}

  /*! Option to merge adjacent clusters if only one excusable missing strip */
  void set_merge_adjacent_clusters(Bool_t merge_adjacent_clusters) {_merge_adjacent_clusters = merge_adjacent_clusters;}

  // ! automatic load dead channel map according to run number- Obsolete, use TFvtxDeadMap instead
  Bool_t get_auto_load_dead_map() const {return _auto_load_dead_map;}

  // ! automatic load dead channel map according to run number- Obsolete, use TFvtxDeadMap instead
  void set_auto_load_dead_map(Bool_t a) {_auto_load_dead_map = a;}

  //! database name for dead channel map- Obsolete, use TFvtxDeadMap instead
  const TString & get_dead_map_name() const {return _dead_map_name;}

  //! database name for dead channel map- Obsolete, use TFvtxDeadMap instead
  void set_dead_map_name(TString n) {
    if (n.Length())
      _auto_load_dead_map = kFALSE;

    _dead_map_name = n;
  }

  // print method
  void print(std::ostream& out = std::cout) const {
    FVTXOO::PRINT(out, "mFvtxFindClusPar");
    out << "_min_clus_width = " << _min_clus_width << ".\n";
    out << "_max_clus_width = " << _max_clus_width << ".\n";
    out << "_min_charge_sum = " << _min_charge_sum << ".\n";
    out << "_merge_adjacent_clusters = " << _merge_adjacent_clusters
              << ".\n";
    // out << "_auto_load_dead_map = " << _auto_load_dead_map
    //     << ".- Obsolete, use TFvtxDeadMap instead\n";
    // out << "dead channel map = " << (_dead_map_name.Length() > 0
    //                                      ? _dead_map_name.Data()
    //                                      : "(Not specified)")
    //     << ".- Obsolete, use TFvtxDeadMap instead\n";
    FVTXOO::PRINT(out, "***");
  }

 private:
  //! load dead map according to run number- Obsolete, use TFvtxDeadMap instead
  Bool_t _auto_load_dead_map;

  //! database name for dead channel map- Obsolete, use TFvtxDeadMap instead
  TString _dead_map_name;
  
  //! minimum cluster width
  unsigned short _min_clus_width;
  
  //! maximum cluster width
  unsigned short _max_clus_width;
  
  //! minimum charge sum
  Float_t _min_charge_sum;
  
  //! minimum strip gain
  Float_t _min_gain;
  
  //! if set to true, merge clusters separated by a dead channel
  Bool_t _merge_adjacent_clusters;

  ClassDef(mFvtxFindClusPar,1);
};

#endif /* __MFVTXFINDCLUSPAR_HH__ */
