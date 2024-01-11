// $Id: TMutTrk_v2.hh,v 1.20 2011/12/29 20:19:31 slash Exp $
// Interface Object Class : TMutTrk_v1
// Author: S.Kelly
// Date: 1/2/21
// Description: Class for Muon Tracker Trk.

#ifndef __TMUTTRK_V2H__
#define __TMUTTRK_V2H__

#include<TDataType.h>
#include<PHKey.hh>
#include<TMutTrk.hh>
#include<TMutTrkPar.hh>
#include<TMutRecoPar.hh>
#include<TMutTrkRes.hh>
#include<MUTOO.h>

class TMutTrk_v2 : public TMutTrk
{

 public:

  TMutTrk_v2();

  virtual ~TMutTrk_v2(){;}

  TMutTrk_v2(const Key&,
	     UShort_t arm,
	     UShort_t octant,
	     UShort_t index);

  TMutTrk_v2(const TMutTrk* base_ptr);
  TMutTrk_v2(const TMutTrk& base_ref);

  const TMutTrkPar* get_trk_par() const
  {return &_trk_par;}

  void set_trk_par(const TMutTrkPar& trk_par)
  {_trk_par = trk_par;}

  const TMutTrkPar* get_trk_par_vtx() const
  {return &_trk_par_vtx;}

  void set_trk_par_vtx(const TMutTrkPar& trk_par)
  {_trk_par_vtx = trk_par;}

  void push_trk_par(const TMutTrkPar& trk_par)
  { _trk_par_list.push_back(trk_par);}

  void clear_trk_par_list()
  {_trk_par_list.clear();}

  const trk_par_list* get_trk_par_list() const
  {return &_trk_par_list;}

  size_t get_n_trk_par() const
  { return _trk_par_list.size(); }

  const TMutBPPar* get_bp_par() const
  { return &_bp_par;}

  TMutBPPar* get_bp_par()
  { return &_bp_par;}

  void set_bp_par(const TMutBPPar& bp_par)
  {_bp_par = bp_par;}

  const TMutFitPar* get_fit_par() const
  {return &_fit_par;}

  void set_fit_par(const TMutFitPar* fit_par)
  {_fit_par = *fit_par;}

  void set_fit_par(const TMutFitPar& fit_par)
  {_fit_par = fit_par;}

  double get_w_chi_square() const
  { return _w_chi_square; }

  double get_w_chi_square_pdf() const;

  double get_r_chi_square() const
  { return _r_chi_square; }

  double get_r_chi_square_pdf() const;

  double get_chi_square() const
  { return _w_chi_square + _r_chi_square; }

  void set_w_chi_square(double w_chi_square)
  {_w_chi_square = w_chi_square;}

  void set_r_chi_square(double r_chi_square)
  {_r_chi_square = r_chi_square;}

  double get_phi_min(UShort_t station) const
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _phi_min[station];}

  double get_phi_max(UShort_t station) const
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _phi_max[station];}

  void set_phi_min(UShort_t station, double phi_min)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _phi_min[station] = phi_min;}

  void set_phi_max(UShort_t station, double phi_max)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _phi_max[station] = phi_max;}

  double get_theta_min(UShort_t station) const
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _theta_min[station]; }

  double get_theta_max(UShort_t station) const
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _theta_max[station];}

  void set_theta_min(UShort_t station, double theta_min)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _theta_min[station] = theta_min; }

  void set_theta_max(UShort_t station, double theta_max)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _theta_max[station] = theta_max; }

  void push_w_residual(const TMutTrkRes& residual)
  { _w_residual_list.push_back(residual); }

  void clear_w_residual_list()
  { _w_residual_list.clear(); }

  size_t get_n_w_residual() const
  { return _w_residual_list.size(); }

  const residual_list* get_w_residual_list() const
  { return &_w_residual_list; }

  void push_r_residual(double residual)
  { _r_residual_list.push_back(residual); }

  void clear_r_residual_list()
  { _r_residual_list.clear(); }

  size_t get_n_r_residual() const
  { return _r_residual_list.size(); }

  const std::vector<double>* get_r_residual_list() const
  { return &_r_residual_list;}

  UShort_t  get_arm() const
  {return _arm;}

  UShort_t  get_octant() const
  {return _octant;}

  UShort_t  get_index() const
  {return _index;}

  void set_arm(UShort_t arm)
  { _arm = arm; }

  void set_octant(UShort_t octant)
  { _octant = octant; }

  void  set_index(UShort_t index)
  { _index = index; }

  Float_t get_charge() const
  {return _trk_par.get_charge();}

  void set_charge(int charge)
  {_trk_par.set_charge(charge);}

  //!@name status
  //@{

  bool get_reco_success() const
  {
    return
      (get_global_fit() || get_kalman_fit())
      && !(get_global_fail() || get_kalman_fail());
  }

  //! set status bit
  void set_status( Status bit, bool value )
  {
    if( value ) { _status |= (1<<bit); }
    else { _status &= (~(1<<bit)); }
  }

  //! check status bit
  bool get_status( Status bit ) const
  { return get_status() & (1<<bit); }

  UShort_t get_status() const
  {return _status;}

  void copy_status( const TMutTrk& trk )
  { _status = trk.get_status(); }

  void clear_status()
  { _status=0;}

  //@}

  void print(std::ostream& os = std::cout, bool max=false) const;

private:

  UShort_t _arm;
  UShort_t _octant;
  UShort_t _index;

  TMutTrkPar _trk_par;
  TMutTrkPar _trk_par_vtx;
  std::vector<TMutTrkPar> _trk_par_list;

  TMutBPPar _bp_par;

  TMutRecoPar _reco_par;
  TMutRecoPar _reco_par_vtx;
  std::vector<TMutRecoPar> _reco_par_list;

  TMutFitPar _fit_par;
  std::vector<TMutTrkRes> _w_residual_list;
  std::vector<double> _r_residual_list;

  enum { WINDOWS_SIZE=3 };
  double _phi_min[WINDOWS_SIZE];
  double _phi_max[WINDOWS_SIZE];
  double _theta_min[WINDOWS_SIZE];
  double _theta_max[WINDOWS_SIZE];

  double _w_chi_square;
  double _r_chi_square;
  double _charge;
  UShort_t _status;

  ClassDef(TMutTrk_v2,1)
};

#endif /* __TMUTTRK_V2H__*/



