#ifndef __MMUIROADFINDER1PAR_HH__
#define __MMUIROADFINDER1PAR_HH__

#include<PHObject.h>
#include<MUIOO.h>
#include<TMuiParBase.h>

//!  Runtime parameter object for mMuiRoadfinder1 analysis module
/*! 
 */
class mMuiRoadFinder1Par : public TMuiParBase
{  
 public: 
  
  /*! default constructor */
  mMuiRoadFinder1Par() : 
    _num_seed_loops(2),
    _min_last_gap_1d(2),
    _min_fired_gaps(2),
    _max_xref_1d(180.0),
    _max_yref_1d(180.0),
    _max_clusters_per_gap_search(1),
    _min_last_gap_2d(2),
    _max_del_last_gap(1),
    _max_del_total_hits(1),
    _max_xref_2d(180.0),
    _max_yref_2d(180.0),
    _max_xchisq(1000.0),
    _max_ychisq(1000.0),
    _mut_window(20),
    _mui_window(30),
    _mut_z_north(620),
    _mut_z_south(-470),
    _max_occupancy_per_arm(300),
    _xvert(0.0),
    _yvert(0.0),
    _zvert(0.0)
  {
    std::fill(_search_length,_search_length+MAXSEEDLOOPS,6);
    std::fill(_weight_par_1d,_weight_par_1d+NUMFITPOINTS,1.0);
    std::fill(_weight_par_2d,_weight_par_2d+NUMFITPOINTS,1.0);
    int t_SearchOrder[MAXSEEDLOOPS][NUMFITPOINTS] = 
    { {-1,2,1,0,3,4}, {-1,1,2,0,3,4} };
    
    for(int iLoop = 0; iLoop < MAXSEEDLOOPS; iLoop++ )
    {
      for(int iPoint = 0; iPoint < NUMFITPOINTS; iPoint++ )
      { _search_order[iLoop][iPoint]=t_SearchOrder[iLoop][iPoint]; }	  
    }
  }
  
  /*! destructor */
  ~mMuiRoadFinder1Par(){;}
  
  enum{NUMFITPOINTS=6, MAXSEEDLOOPS = 2};
  
  /*! get methods */  
  UShort_t get_num_seed_loops() const 
  { return _num_seed_loops;}
  
  UShort_t get_search_length(int loop) const 
  { return _search_length[loop];}
  
  UShort_t get_search_order(int loop, int point) const 
  { return _search_order[loop][point];}
  
  /*! 1D */
  UShort_t get_min_last_gap_1d() const 
  { return _min_last_gap_1d;}
  
  UShort_t get_min_fired_gaps() const 
  { return _min_fired_gaps;}
  
  Float_t get_weight_par_1d(int point) const 
  { return _weight_par_1d[point];}
  
  Float_t get_max_xref_1d() const
  { return _max_xref_1d;}
  
  Float_t get_max_yref_1d() const 
  { return _max_yref_1d;}
  
  UShort_t get_max_clusters_per_gap_search() const 
  { return _max_clusters_per_gap_search;}
  
  UShort_t get_min_last_gap_2d() const 
  { return _min_last_gap_2d;}
  
  UShort_t get_max_del_last_gap() const 
  { return _max_del_last_gap;}
  
  UShort_t get_max_del_total_hits() const 
  { return _max_del_total_hits;}
  
  Float_t get_max_xref_2d() const 
  { return _max_xref_2d;}
  
  Float_t get_max_yref_2d() const 
  { return _max_yref_2d;}
  
  /*! 2D */
  Float_t get_weight_par_2d(int point) const 
  { return _weight_par_2d[point];}
  
  Float_t get_max_xchisq() const 
  { return _max_xchisq;}
  
  Float_t get_max_ychisq() const 
  { return _max_ychisq;}
  
  Float_t get_mut_window() const 
  { return _mut_window;}
  
  Float_t get_mui_window() const 
  { return _mui_window;}
  
  Float_t get_mut_z_north() const 
  { return _mut_z_north;}
  
  Float_t get_mut_z_south() const 
  { return _mut_z_south;}
  
  UShort_t get_max_occupancy_per_arm() const 
  { return _max_occupancy_per_arm;}
  
  /*! vertex */
  Float_t get_xvert() const 
  { return _xvert;}
  
  Float_t get_yvert() const 
  { return _yvert;}
  
  Float_t get_zvert() const 
  { return _zvert;}
  
  /*! set methods */
  void set_num_seed_loops(UShort_t sval) 
  { _num_seed_loops = sval;}
  
  void set_search_length(int point, UShort_t sval) 
  { _search_length[point] = sval;}
  
  void set_search_order(int loop, int point, UShort_t sval) 
  { _search_order[loop][point] = sval;}
  
  /*! 1D */
  void set_weight_par_1d(int point, Float_t fval) 
  { _weight_par_1d[point] = fval;}
  
  void set_max_xref_1d(Float_t fval) 
  { _max_xref_1d = fval;}
  
  void set_max_yref_1d(Float_t fval) 
  { _max_yref_1d = fval;}
  
  void set_min_last_gap_1d(UShort_t sval)
  { _min_last_gap_1d = sval;}
  
  void set_min_fired_gaps(UShort_t sval) 
  { _min_fired_gaps = sval;}
  
  void set_max_clusters_per_gap_search(UShort_t sval) 
  { _max_clusters_per_gap_search = sval;}
  
  /*! 2D */
  void set_max_xref_2d(Float_t fval) 
  { _max_xref_2d = fval;}
  
  void set_max_yref_2d(Float_t fval) 
  { _max_yref_2d = fval;}
  
  void set_weight_par_2d(int point, Float_t fval) 
  { _weight_par_2d[point] = fval;}
  
  void set_max_xchisq(Float_t fval) 
  { _max_xchisq = fval;}
  
  void set_max_ychisq(Float_t fval) 
  { _max_ychisq = fval;}
  
  void set_mut_window(Float_t fval) 
  { _mut_window = fval;}
  
  void set_mui_window(Float_t fval) 
  { _mui_window = fval;}
  
  void set_mut_z_north(Float_t fval) 
  { _mut_z_north = fval;}
  
  void set_mut_z_south(Float_t fval) 
  { _mut_z_south = fval;}
  
  void set_min_last_gap_2d(UShort_t sval) 
  { _min_last_gap_2d = sval;}
  
  void set_max_del_last_gap(UShort_t sval) 
  { _max_del_last_gap = sval;}
  
  void set_max_del_total_hits(UShort_t sval) 
  { _max_del_total_hits = sval;}
  
  void set_max_occupancy_per_arm(UShort_t sval) 
  { _max_occupancy_per_arm = sval;}
  
  /*! vertex */
  void set_xvert(Float_t fval) 
  { _xvert = fval;}
  
  void set_yvert(Float_t fval) 
  { _yvert = fval;}

  void set_zvert(Float_t fval) 
  { _zvert = fval;}

	//! dump all parameters  
	void print( std::ostream& out = std::cout ) 
	{
		MUIOO::PRINT( out, "mMuiRoadFinder1Par" );
    out << "_num_seed_loops: " << _num_seed_loops << std::endl;              
    out << "_min_last_gap_1d: " << _min_last_gap_1d << std::endl;             
    out << "_min_fired_gaps: " << _min_fired_gaps << std::endl;              
    out << "_max_xref_1d: " << _max_xref_1d << std::endl;                 
    out << "_max_yref_1d: " << _max_yref_1d << std::endl;                 
    out << "_max_clusters_per_gap_search: " << _max_clusters_per_gap_search << std::endl; 
    out << "_min_last_gap_2d: " << _min_last_gap_2d << std::endl;             
    out << "_max_del_last_gap: " << _max_del_last_gap << std::endl;            
    out << "_max_del_total_hits: " << _max_del_total_hits << std::endl;           
    out << "_max_xref_2d: " << _max_xref_2d << std::endl;                 
    out << "_max_yref_2d: " << _max_yref_2d << std::endl;                 
    out << "_max_xchisq: " << _max_xchisq << std::endl;                  
    out << "_max_ychisq: " << _max_ychisq << std::endl;                  
    out << "_mut_window: " << _mut_window << std::endl;                  
    out << "_mui_window: " << _mui_window << std::endl;                  
    out << "_mut_z_north: " << _mut_z_north << std::endl;                 
    out << "_mut_z_south: " << _mut_z_south << std::endl;                 
    out << "_max_occupancy_per_arm: " << _max_occupancy_per_arm << std::endl;       


		MUIOO::PRINT( out, "**" );
	}  
  private:  
  
  // Grabbed the control parameters from the dMuiRoadControl table.
  // Parameters to controal road model and collection
  UShort_t _num_seed_loops;
  UShort_t _search_length[MAXSEEDLOOPS];
  UShort_t _search_order[MAXSEEDLOOPS][NUMFITPOINTS];

  // Parameters for 1d road quality check
  UShort_t _min_last_gap_1d;
  UShort_t _min_fired_gaps;
  Float_t _weight_par_1d[NUMFITPOINTS];
  Float_t _max_xref_1d;
  Float_t _max_yref_1d;
  UShort_t _max_clusters_per_gap_search;
  
  // Parameters for 2D road quality check
  UShort_t _min_last_gap_2d;
  UShort_t _max_del_last_gap;
  UShort_t _max_del_total_hits;
  Float_t _max_xref_2d;
  Float_t _max_yref_2d;
  Float_t _weight_par_2d[NUMFITPOINTS];
  Float_t _max_xchisq;
  Float_t _max_ychisq;
  Float_t _mut_window;
  Float_t _mui_window;
  Float_t _mut_z_north;
  Float_t _mut_z_south;

  // Number of hits in the arm above which we should
  // skip the event.
  UShort_t _max_occupancy_per_arm;

  Float_t _xvert;
  Float_t _yvert;
  Float_t _zvert;

};

#endif /* __MMUIROADFINDER1PAR_HH__ */
