// $Id: TFvtxTrk_v1.h,v 1.16 2014/11/27 02:41:56 slash Exp $

#ifndef __TFvtxTRK_v1H__
#define __TFvtxTRK_v1H__

/*!
\file TFvtxTrk_v1.hh
\brief The Forward Silicon (FVTX) Track object 
\author M. Brooks
\version $Revision: 1.16 $
\date $Date: 2014/11/27 02:41:56 $

*/

#include<TDataType.h>
#include<PHKey.hh>
#include<TFvtxTrk.h>
#include<TMutTrkPar.hh>
#include<TMutTrkRes.hh>
#include<FVTXOO.h>

class TFvtxTrk_v1 : public TFvtxTrk
{
  
  public:
  
  //! constructor
  TFvtxTrk_v1();
  
  //! destructor
  virtual ~TFvtxTrk_v1(){;}
  
  //! constructor
  TFvtxTrk_v1(const Key&, const unsigned short& arm, const unsigned short& index);
  
  //! constructor 
  TFvtxTrk_v1(const TFvtxTrk* base_ptr);

  //! constructor 
  TFvtxTrk_v1(const TFvtxTrk& base_ref);
  
  //! track parameters
  const TMutTrkPar* get_trk_par() const 
  {return &_trk_par;}
  
  //! track parameters
  void set_trk_par(const TMutTrkPar& trk_par) 
  {_trk_par = trk_par;}
  
  //! track parameters at vertex
  const TMutTrkPar* get_trk_par_vtx() const 
  {return &_trk_par_vtx;}
  
  //! track parameters at vertex 
  void set_trk_par_vtx(const TMutTrkPar& trk_par) 
  {_trk_par_vtx = trk_par;}
 
  //! track parameters at vertex with original mutr fit
  const TMutTrkPar* get_trk_par_mutr() const
  {return &_trk_par_mutr;}

  //! track parameters at vertex with original mutr kalfit
  void set_trk_par_mutr(const TMutTrkPar& trk_par)
  {_trk_par_mutr = trk_par;}

  //! track parameter list
  void push_trk_par(const TMutTrkPar& trk_par)
  { _trk_par_list.push_back(trk_par);}
  
  //! track parameter list
  void clear_trk_par_list()
  {_trk_par_list.clear();}
  
  //! track parameter list
  const trk_par_list* get_trk_par_list() const 
  {return &_trk_par_list;} 
  
  //! number of track parameter sets in list
  size_t get_n_trk_par() const 
  { return _trk_par_list.size(); }
  
  /*! \brief returns track number of degrees of freedom. */
  size_t get_ndf() const
  { return _ndf; }
    
  /*! \brief returns track hit (TFvtxCoord) pattern. */
  virtual unsigned short get_hit_pattern() const; 

  /*! sets hit (TFvtxCoord) pattern */
  virtual void set_hit_pattern( unsigned short pattern )
  { _hit_pattern = pattern; }
        
  double get_w_chi_square() const 
  { return _w_chi_square; }
  
  double get_w_chi_square_pdf() const;
  
  double get_r_chi_square() const 
  { return _r_chi_square; }
  
  double get_r_chi_square_pdf() const;
  
  double get_chi_square() const 
  { return _w_chi_square + _r_chi_square; }
  
  /*! \brief
    sets track number of degrees of freedom.
  */
  void set_ndf( size_t ndf )
  { _ndf = ndf; }
    
  void set_w_chi_square(double w_chi_square) 
  {_w_chi_square = w_chi_square;}
  
  void set_r_chi_square(double r_chi_square) 
  {_r_chi_square = r_chi_square;}
  
  double get_phi_min(unsigned short station) const 
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _phi_min[station];}
  
  double get_phi_max(unsigned short station) const 
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _phi_max[station];}
  
  void set_phi_min(unsigned short station, double phi_min)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _phi_min[station] = phi_min;}
  
  void set_phi_max(unsigned short station, double phi_max)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _phi_max[station] = phi_max;}
  
  double get_theta_min(unsigned short station) const 
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _theta_min[station]; }
  
  double get_theta_max(unsigned short station) const 
  { BOUNDS_CHECK(station,WINDOWS_SIZE); return _theta_max[station];}
  
  void set_theta_min(unsigned short station, double theta_min)
  { BOUNDS_CHECK(station,WINDOWS_SIZE); _theta_min[station] = theta_min; }
  
  void set_theta_max(unsigned short station, double theta_max)
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
  
  unsigned short  get_arm() const 
  {return _arm;}
  
  unsigned short  get_index() const 
  {return _index;}  
  
  void set_arm(unsigned short arm)
  { _arm = arm; }
  
  void  set_index(unsigned short index)
  { _index = index; }               
  
  void set_kalman_fit() 
  {_status |= 1<<KALMAN_FIT;}
  
  bool get_kalman_fit() const 
  { return (_status & 1<<KALMAN_FIT) != 0;}
  
  bool get_reco_success() const 
  { return (get_kalman_fit()) && !(get_kalman_fail()); }
  
  void set_kalman_fail() 
  {_status |= 1<<KALMAN_FAIL;}
  
  bool get_kalman_fail() const 
  { return (_status & 1<<KALMAN_FAIL) != 0;}
  
  void set_reco_min_hits() 
  {_status |= 1<<RECO_MIN_HIT;}
  
  bool get_reco_min_hits() const 
  { return (_status & 1<<RECO_MIN_HIT) != 0;}
  
  void set_ghost() 
  {_status |= 1<<GHOST;}
  
  bool get_ghost() const 
  { return (_status & 1<<GHOST) != 0;}
  
  void set_no_fit() 
  {_status |= 1<<NO_FIT;}
  
  bool get_no_fit() const 
  { return (_status & 1<<NO_FIT) != 0;}
  
  void set_mutr_associated( bool a=true) 
  {_status |= a<<ASSOCIATED;}
  
  bool get_mutr_associated() const
  { return (_status & 1<<ASSOCIATED) != 0;}

  void set_swapped_mutr_associated( bool a=true)
  {_status |= a<<SWAPPED_ASSOCIATED;}
  
  bool get_swapped_mutr_associated() const
  { return (_status & 1<<SWAPPED_ASSOCIATED) != 0;}

  Float_t get_charge() const 
  {return _trk_par.get_charge();}
  
  void set_charge(int charge) 
  {_trk_par.set_charge(charge);}
  
  unsigned short get_status() const 
  {return _status;}
  
  void copy_status( const TFvtxTrk& trk )
  { _status = trk.get_status(); }
  
  void clear_status() 
  { _status=0;}
  
  void print(std::ostream& os, bool max) const;
  void print(std::ostream& os = std::cout) const { print(os, false); }

  private:	
  
  unsigned short _arm;
  unsigned short _index;
  
  TMutTrkPar _trk_par;  
  TMutTrkPar _trk_par_vtx;
  TMutTrkPar _trk_par_mutr;
  std::vector<TMutTrkPar> _trk_par_list;  
  std::vector<TMutTrkRes> _w_residual_list;
  std::vector<double> _r_residual_list;
  
  enum { WINDOWS_SIZE=4 };
  double _phi_min[WINDOWS_SIZE];
  double _phi_max[WINDOWS_SIZE];
  double _theta_min[WINDOWS_SIZE];
  double _theta_max[WINDOWS_SIZE];
  
  //! w chisquare (i.e. perp to the strips
  double _w_chi_square;
  
  /*! \brief
    r chisquare (i.e. perp to the anodes)
    does not make sense for Kalman Fit
  */
  double _r_chi_square;

  //! track number of degrees of freedom
  size_t _ndf;
  
  //! track hit pattern
  unsigned short _hit_pattern;
  
  //! track charge
  double _charge;
  
  //! track status 
  unsigned short _status;
  
  ClassDef(TFvtxTrk_v1,1)
};

#endif /* __TFVTXTrk_v1H__*/



