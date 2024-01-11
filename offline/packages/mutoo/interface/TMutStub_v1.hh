#ifndef _TMUTSTUB_V1H_
#define _TMUTSTUB_V1H_

// headers
//
#include<TDataType.h>
#include<TMutStub.hh>

class TMutStub_v1 : public TMutStub {
  
public:
  
  TMutStub_v1();  
  
  virtual ~TMutStub_v1(){;}
  
  TMutStub_v1(const Key&,
	      UShort_t arm,
	      UShort_t station,
	      UShort_t octant,
	      UShort_t half_octant,
	      UShort_t index);

  TMutStub_v1(const TMutStub* base_ptr);
  
  TMutStub_v1(const TMutStub& base_ref);
  
  const TMutFitPar* get_fit_par() const {return &_fit_par;}
  void set_fit_par(const TMutFitPar* fit_par) {_fit_par = *fit_par;}
  void set_fit_par(const TMutFitPar& fit_par) {_fit_par = fit_par;}
  void set_z_begin(double z_begin) {_fit_par.set_z_begin(z_begin);}
  void set_z_end(double z_end) {_fit_par.set_z_end(z_end);}
  PHVector get_anode_direction() const; 
  void push_w_residual(const TMutTrkRes& residual){_w_residual_list.push_back(residual);}
  const residual_list* get_w_residual_list() const { return &_w_residual_list; } 
  void clear_w_residual_list(){_w_residual_list.clear();}
  size_t get_n_w_residual() const { return _w_residual_list.size();}

  void push_r_residual(double residual){_r_residual_list.push_back(residual);}
  const std::vector<double>* get_r_residual_list() const { return &_r_residual_list; } 
  void clear_r_residual_list(){_r_residual_list.clear();}
  size_t get_n_r_residual() const { return _r_residual_list.size();}

  UShort_t get_n_coord() const;
  UShort_t get_n_gap_coord() const;

  double get_w_chi_square() const { return _w_chi_square; }
  double get_r_chi_square() const { return _r_chi_square; }
  double get_chi_pdf() const;
  
  double get_chi_square() const { return _w_chi_square + _r_chi_square; }
  void set_w_chi_square(double w_chi_square) {_w_chi_square = w_chi_square;}
  void set_r_chi_square(double r_chi_square) {_r_chi_square = r_chi_square;}

  double get_phi_min() const {
    return _phi_min; 
  }
  double get_phi_max() const {
    return _phi_max; 
  }
  void set_phi_min(double phi_min){
    _phi_min = phi_min;
  }
  void set_phi_max( double phi_max){
    _phi_max = phi_max;
  }
  double get_theta_min() const {
    return _theta_min; 
  }
  double get_theta_max() const {
    return _theta_max; 
  }
  void set_theta_min(double theta_min){
    _theta_min = theta_min;
  }
  void set_theta_max(double theta_max){
    _theta_max = theta_max;
  }

#ifndef __CINT__
  double get_theta() const {
    double r = std::sqrt(MUTOO::SQUARE(_fit_par.get_x()) +
			 MUTOO::SQUARE(_fit_par.get_y()));
    return atan2(r,std::fabs(_fit_par.get_z()));
  }
  bool check_theta_window(const std::pair<float,float>& theta_window) {
    double theta = get_theta();
    return (theta > theta_window.first && theta < theta_window.second);
  }

  double get_phi() const {
    return atan2(_fit_par.get_y(), _fit_par.get_x());
  }
  bool check_phi_window(const std::pair<float,float>& phi_window) const; 
  bool has_point() const;
  double get_dwdz() const;
#endif

  void set_arm(UShort_t arm) { _arm=arm;}
  void set_station(UShort_t station) { _station=station;}
  void set_octant(UShort_t octant) { _octant=octant;}
  void set_half_octant(UShort_t half_octant) { _half_octant=half_octant;}
  void set_index(UShort_t index) { _index=index;}
  UShort_t get_arm() const {return _arm;}
  UShort_t get_station() const {return _station;}
  UShort_t get_octant() const {return _octant;}
  UShort_t get_half_octant() const {return _half_octant;}
  UShort_t get_index() const {return _index;}
  void print(std::ostream& os = std::cout, bool max=false) const;

private:
  
  UShort_t _arm;
  UShort_t _station;
  UShort_t _octant;
  UShort_t _half_octant;
  UShort_t _index;

  // Stub fit par
  //
  TMutFitPar _fit_par;
  std::vector<TMutTrkRes> _w_residual_list;  
  std::vector<double> _r_residual_list;

  double _w_chi_square;
  double _r_chi_square;
  double _phi_min;
  double _phi_max;
  double _theta_min;
  double _theta_max;
  
  ClassDef(TMutStub_v1,1)

};
  
#endif /* _TMUTSTUB_V1H_ */
	      








