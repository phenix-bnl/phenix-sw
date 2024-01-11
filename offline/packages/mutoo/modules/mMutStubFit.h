// $Id: mMutStubFit.h,v 1.13 2017/10/12 01:40:32 shlim Exp $
#ifndef __MMUTSTUBFIT_HH__
#define __MMUTSTUBFIT_HH__

#include<PHTimeServer.h>
#include<mMutStubFitPar.h>
#include<TMutStubMap.h>
#include<TMutCoordMap.h>
#include<TMutGapCoordMap.h>
#include<TMutLinearModel.h>

#include<PHGslMatrix.h>

#include<gsl/gsl_math.h>
#include<gsl/gsl_rng.h>
#include<gsl/gsl_randist.h>
#include<gsl/gsl_vector.h>
#include<gsl/gsl_blas.h>
#include<gsl/gsl_multifit_nlin.h>
#include<list>
#include<stack>
#include<boost/array.hpp>

class PHCompositeNode;

/*! \ingroup modules */
//! Constrains TMutCoord/TMutGapCoord associated with TMutStub objects to linear track model.
/*!
The mMutStubFit executes a least squares fit to the selected using
a linear track model to TMutStub objects.  The data used to the constrain
the fit are the set  of TMutCoord/TMutGapCoord associated with a TMutStub
object.  The fitter uses the measurement model defined in TMutMeasureModel and
the track model in TMutLinearModel. The minimization is done using the 
gsl <b>gsl_multifit</b> package. 
<br>

<h2>Analysis Module Interface Specification</h2>
<table>
<tr>
<td> Object </td>
<td> Description </td>
<td> Privilege </td>
</tr>
<tr>
<td> const mMutStubFitPar*</td>
<td> Parameter Table </td>
<td> immutable </td>
</tr>
<tr>
<td> TMutStubMap*</td>
<td> Track IOC</td>
<td> mutable </td>
</table>
*/

class mMutStubFit
{
  public: 

  mMutStubFit(); 
  virtual ~mMutStubFit(){}

  virtual PHBoolean event(PHCompositeNode*);

  PHBoolean event(PHCompositeNode* top_node, unsigned short arm, unsigned short station);

  static double get_w_error();
  
  static double get_r_error();

  void set_interface_ptrs(PHCompositeNode* top_node);

 private:  

  struct stub_less_ftor
  {
    bool operator()(TMutStubMap::value_type stub1, TMutStubMap::value_type stub2)
    {
      double mod1 = std::fabs(stub1.get()->get_fit_par()->get_x()) +	
	std::fabs(stub1.get()->get_fit_par()->get_y())+
	std::fabs(stub1.get()->get_fit_par()->get_z())+
	std::fabs(stub1.get()->get_fit_par()->get_drdz());      
      double mod2 = std::fabs(stub2.get()->get_fit_par()->get_x()) +	
	std::fabs(stub2.get()->get_fit_par()->get_y())+
	std::fabs(stub2.get()->get_fit_par()->get_z())+
	std::fabs(stub2.get()->get_fit_par()->get_drdz());
			//Sanghoon
			//There're are case of showing differnet comparion results between same float-point numbers in SL6 and SL7 becase of +/-1e-19 flucutation
			//This fluctuation cause different sorting results and cluster associations (Not good in high occupancy cases)
			//if(mod1 < mod2){
			if((mod1-mod2)<1e-4){
	return true;
      } else {
	return false;
      }
    }
  };   

  struct stub_equal_ftor
  {
    bool operator()(TMutStubMap::value_type stub1, TMutStubMap::value_type stub2) {
      double dx = stub1.get()->get_fit_par()->get_x() - stub2.get()->get_fit_par()->get_x();
      double dy = stub1.get()->get_fit_par()->get_y() - stub2.get()->get_fit_par()->get_y(); 	
      double ddxdz = stub1.get()->get_fit_par()->get_dxdz() - stub2.get()->get_fit_par()->get_dxdz(); 	
      double ddydz = stub1.get()->get_fit_par()->get_dydz() - stub2.get()->get_fit_par()->get_dydz(); 	
      double diff = std::sqrt(MUTOO::SQUARE(dx) + MUTOO::SQUARE(dy) + MUTOO::SQUARE(ddxdz) + MUTOO::SQUARE(ddydz));
//       stub1.get()->print();
//       stub2.get()->print();
//       std::cout << diff << std::endl;
      if(diff<0.2) return true;
      return false;
    }
  };   

  class stub_unary_equal_ftor
  {
  public:    

    stub_unary_equal_ftor(ULong_t target_key) : _target_key(target_key){}
    
    bool operator()(TMutStubMap::value_type stub) {
      if(stub.get()->get_key().get_obj_key() == _target_key) return true;
      return false;
    }
  private:
    ULong_t _target_key;
  };   
    
  void stub_loop();

  void fit_loop(TMutStubMap::pointer);
  
  bool build_hit_list(TMutStubMap::pointer);

  void fit_stub(TMutStubMap::pointer);

  /*! \brief 
    alternative stub fit method 
    using simple straight line chisquare minimization
  */
  void fast_fit_stub(TMutStubMap::pointer);

  void write_residuals(TMutStubMap::pointer);

  void reject_ghosts();

  std::pair<bool,TMutStubMap::value_type> reject_ghost_chi_square(TMutStubMap::value_type, TMutStubMap::value_type);
  
  void print_summary();

  void print_solver(const gsl_multifit_fdfsolver*, 
		    std::ostream& os = std::cout) const; 
  
  unsigned short get_number_data_points() const;

  bool is_station_active(unsigned short station);

  void initialize_mask_stack(TMutStubMap::const_pointer trk_ptr);

  bool pop_mask_stack();

  void write_stub_pars(TMutStubMap::pointer);

  // Interface pointers
  //
  const mMutStubFitPar* _mod_par; // parameter table	
  TMutStubMap*  _stub_map;        // TMutStub IOC
  
  // Track model pointer
  //
  TMutLinearModel _track_model;

  // Array of booleans for station mask
  //
  boost::array<bool,3> _station_active;
  
  // Storage for cathode masking 
  //
  std::stack<PHKey::key_type> _mask_stack;
  
  // Current cathode mask
  //
  PHKey::key_type _cathode_mask;

  // Timer
  //
  PHTimeServer::timer _timer;  
  
 public:
  typedef std::list<TMutCoord*> coord_list;
  typedef std::list<TMutGapCoord*> gap_coord_list;

 private:
  // Define local sortable storage for associated TMutCoord
  //
  coord_list _coord_list;
  
  // Define local sortable storage for associated TMutGapCoord
  //
  gap_coord_list _gap_coord_list;
  
  // Define a class scope struct for passing necessary parameters 
  // to GSL fit routines
  //
 public:
  struct FitData {
    TMutStub* stub_ptr;
    coord_list& coord_list_ref;
    gap_coord_list& gap_coord_list_ref;
    TMutLinearModel* track_model_ptr;
    bool use_anodes;
  };

 private:

  double _z_reference;
  bool _use_section;
  unsigned short _arm;
  unsigned short _station;

  //! structure to store GSL matrices for fast stub fit
  struct Node {
  
    //! constructor
    Node( TMutCoord* coord, const double& z_reference );
  
    //! default constructor
    Node( void ):
      _h( 4, 1 ),
      _m( 1, 1 ),
      _g( 1, 1 )
    {}  
    
  
    //! projection matrix from state vector to measurement
    PHGslMatrix _h;
    
    //! measurement matrix
    PHGslMatrix _m;
    
    //! gain matrix
    PHGslMatrix _g;
  
  };

  //! shortcut to define list of nodes
  typedef std::list<Node> node_list;
  
  //! shortcut to define iterator over list of nodes
  typedef node_list::iterator node_iterator;

};

#endif /* __MMUTTSTUBFIT_HH__ */














