#ifndef __MMUTFINDCLUSPAR_HH__
#define __MMUTFINDCLUSPAR_HH__

#include<PHObject.h>
#include<MUTOO.h>
#include<TMutParBase.h>
#include<TMutParameterDB.h>

#include <boost/array.hpp>

//!  Runtime parameter object for mMutFindClus analysis module
/*!
*/
class mMutFindClusPar : public TMutParBase
{

 public:

  /*! default constructor */
  mMutFindClusPar():
    _do_cluster_cuts( true ),
    _min_clus_width(1),
    _max_clus_width(4),
    _min_charge_sum(2),
    _min_gain(1),
    _merge_adjacent_clusters(false)
  {
    TMutParameterDB::get().get<unsigned short>("mMutFindClus_verbosity", _verbosity );
    TMutParameterDB::get().get<unsigned short>("mMutFindClus_max_clus_width", _max_clus_width );

    // min mean charge per hit in clusters
    // based on Mike's study for Run9 and Run10 data
    // south arm
    set_min_mean_charge(0, 0, 1.5 );
    set_min_mean_charge(0, 1, 1.0 );
    set_min_mean_charge(0, 2, 1.2 );

    // north arm
    set_min_mean_charge(1, 0, 1.0 );
    set_min_mean_charge(1, 1, 1.0 );
    set_min_mean_charge(1, 2, 1.5 );

    // min hit charge rms in clusters
    // based on Mike's study for Run9 and Run10 data
    // right now, the same cut is applied to all stations
    _min_charge_rms.assign(0.2);


  }

  /*! destructor */
  ~mMutFindClusPar() {;}

  /*! enable cluster cuts for filtering */
  Bool_t get_do_cluster_cuts( void ) const
  { return _do_cluster_cuts; }

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

  /*! enable cluster cuts for filtering */
  void set_do_cluster_cuts( Bool_t value )
  { _do_cluster_cuts = value; }

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

  //! min mean charge per hit in clusters
  void set_min_mean_charge( unsigned short arm, unsigned short station, Float_t value )
  { _min_mean_charge[station + MUTOO::NumberOfStations*arm] = value; }

  //! min mean charge per hit in clusters
  Float_t get_min_mean_charge( unsigned short arm, unsigned short station ) const
  { return _min_mean_charge[station + MUTOO::NumberOfStations*arm]; }

  //! min hit charge rms in clusters
  void set_min_charge_rms( unsigned short arm, unsigned short station, Float_t value )
  { _min_charge_rms[station + MUTOO::NumberOfStations*arm] = value; }

  //! min hit charge rms in clusters
  Float_t get_min_charge_rms( unsigned short arm, unsigned short station ) const
  { return _min_charge_rms[station + MUTOO::NumberOfStations*arm]; }

  /*! Print method */
  void print( std::ostream& out = std::cout ) const
  {

    MUTOO::PRINT( out, "mMutFindClusPar" );
    out << "_do_cluster_cuts = " << _do_cluster_cuts << ".\n";
    out << "_min_clus_width = " << _min_clus_width << ".\n";
    out << "_max_clus_width = " << _max_clus_width << ".\n";
    out << "_min_charge_sum = " << _min_charge_sum << ".\n";
    out << "_merge_adjacent_clusters = " << _merge_adjacent_clusters << ".\n";

    for( int arm = 0; arm < MUTOO::NumberOfArms; arm++ )
      for( int station = 0; station < MUTOO::NumberOfStations; station++ )
    {
        out
          << "arm: " << arm
          << " station: " << station
          << " _min_mean_charge: " << get_min_mean_charge( arm, station )
          << " _min_charge_rms: " << get_min_charge_rms( arm, station )
          << std::endl;
    }
    MUTOO::PRINT( std::cout, "**" );

  }

 private:

  //! enable cluster cuts for filtering
  Bool_t _do_cluster_cuts;

  //! minimum cluster width
  unsigned short _min_clus_width;

  //! maximum cluster width
  unsigned short _max_clus_width;

  //! minimum charge sum
  Float_t _min_charge_sum;

  //! minimum strip gain
  Float_t _min_gain;

  //! total number of stations
  enum { number_of_stations = MUTOO::NumberOfArms*MUTOO::NumberOfStations };

  //! minimum mean charge per strip in cluster
  boost::array<Float_t, number_of_stations> _min_mean_charge;

  //! minimum charge rms in clusters
  boost::array<Float_t, number_of_stations> _min_charge_rms;

  //! if set to true, merge clusters separated by a dead channel
  Bool_t _merge_adjacent_clusters;

};

#endif /* __MMUTFINDCLUSPAR_HH__ */





