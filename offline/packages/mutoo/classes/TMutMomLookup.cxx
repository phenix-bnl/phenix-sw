#include<TMutMomLookup.h>
#include<TFile.h>
#include<TTree.h>
#include<string>
#include<climits>
#include<gsl/gsl_math.h>
#include<TMutTrackUtil.h>
#include<TMutStubMap.h>

#include "DBFile.h"

// Definition of non const static data members
// 
const double TMutMomLookup::CELL_WIDTH_THETA = 1; 
const double TMutMomLookup::CELL_WIDTH_PHI12 = 0.05; 
const double TMutMomLookup::CELL_WIDTH_PHI23 = 0.05; 

long TMutMomLookup::MIN_THETA_SOUTH = LONG_MAX;
long TMutMomLookup::MIN_PHI12_SOUTH = LONG_MAX;
long TMutMomLookup::MIN_PHI23_SOUTH = LONG_MAX;
long TMutMomLookup::MAX_THETA_SOUTH = LONG_MIN;
long TMutMomLookup::MAX_PHI12_SOUTH = LONG_MIN;
long TMutMomLookup::MAX_PHI23_SOUTH = LONG_MIN;

long TMutMomLookup::MIN_THETA_NORTH = LONG_MAX;
long TMutMomLookup::MIN_PHI12_NORTH = LONG_MAX;
long TMutMomLookup::MIN_PHI23_NORTH = LONG_MAX;
long TMutMomLookup::MAX_THETA_NORTH = LONG_MIN;
long TMutMomLookup::MAX_PHI12_NORTH = LONG_MIN;
long TMutMomLookup::MAX_PHI23_NORTH = LONG_MIN;


void
TMutMomLookup::print(std::ostream& os) {

  size_t north_node=0;
  size_t south_node=0;
  
  {
    MUTOO::PRINT(os,"North");
    std::vector<Node>::iterator iter = _north->begin();
    for(;iter!=_north->end();++iter){
      if(iter->count) {
	++north_node;
	os << "theta: " << iter->theta << "   ";
	os << "phi12: " << iter->phi12 << "   ";
	os << "phi23: " << iter->phi23 << "   ";
	os << "ptotus: " << iter->ptotus << "   ";
	os << "ptotvx: " << iter->ptotvx << "   ";
	os << "count: " << iter->count << std::endl;
      }
    }
  }
  {
    MUTOO::PRINT(os,"South");
    std::vector<Node>::iterator iter = _south->begin();
    for(;iter!=_south->end();++iter){
      if(iter->count) {
	++south_node;
	os << "theta: " << iter->theta << "   ";
	os << "phi12: " << iter->phi12 << "   ";
	os << "phi23: " << iter->phi23 << "   ";
	os << "ptotus: " << iter->ptotus << "   ";
	os << "ptotvx: " << iter->ptotvx << "   ";
	os << "count: " << iter->count << std::endl;
      }
    }
  }
  os << "north nodes = " << north_node << "   south nodes = " << south_node << std::endl;
  MUTOO::PRINT(os,"**");
}

void 
TMutMomLookup::initialize()
{
  MUTOO::PRINT(std::cout,"Momentum Lookup Initialization");  
  _timer.restart();

  {    
    // Grab the tree pointer from given file
    //
    TFile f(_south_filename.c_str());
    if(!f.IsOpen()){
      throw std::runtime_error(DESCRIPTION("can't find input file "));
    }
    
    TTree* tree = (TTree*)gDirectory->Get("nt1");
    if(!tree){
      throw std::runtime_error(DESCRIPTION("can't find input file tree: map"));
    }
    
    // Setup locals for tree access
    //
    Float_t theta=0;
    Float_t phi12=0;
    Float_t phi23=0;
    Float_t ptotus=0;
    Float_t ptotvx=0;
    
    tree->SetBranchAddress("theta1",&theta);
    tree->SetBranchAddress("edphi12",&phi12);
    tree->SetBranchAddress("edphi23",&phi23);
    tree->SetBranchAddress("ptotus",&ptotus);
    tree->SetBranchAddress("ptotvx",&ptotvx);
    
    // Determine the bounds of the lookup
    //
    Stat_t nentries = tree->GetEntries();
    for (int jentry=0; jentry<nentries;jentry++) {
      tree->GetEntry(jentry);   
      update_bounds_south(theta,phi12,phi23);
    }
    
    // Allocate the resources
    //
    MUTOO::TRACE(" initializing lookup");
    initialize_lookup_south();
    
    // Set the root field point
    //
    for (int jentry=0; jentry<nentries;jentry++) {
      tree->GetEntry(jentry);   
      add_point_south(theta,phi12,phi23,ptotus,ptotvx);    
    }
//      for (int jentry=0; jentry<nentries;jentry++) {
//        tree->GetEntry(jentry);   
//        double ptest = get_us_momentum(0,theta,phi12,phi23);
//        std::cout << "ptotus: " << ptotus << "  ptest: " << ptest << std::endl;
//      }
    f.Close();
  }

  {    
    // Grab the tree pointer from given file
    //
    TFile f(_north_filename.c_str());
    if(!f.IsOpen()){
      throw std::runtime_error(DESCRIPTION("can't find input file "));
    }
    
    TTree* tree = (TTree*)gDirectory->Get("nt1");
    if(!tree){
      throw std::runtime_error(DESCRIPTION("can't find input file tree: map"));
    }
    
    // Setup locals for tree access
    //
    Float_t theta=0;
    Float_t phi12=0;
    Float_t phi23=0;
    Float_t ptotus=0;
    Float_t ptotvx=0;
    
    tree->SetBranchAddress("theta1",&theta);
    tree->SetBranchAddress("edphi12",&phi12);
    tree->SetBranchAddress("edphi23",&phi23);
    tree->SetBranchAddress("ptotus",&ptotus);
    tree->SetBranchAddress("ptotvx",&ptotvx);
    
    // Determine the bounds of the lookup
    //
    Stat_t nentries = tree->GetEntries();
    for (int jentry=0; jentry<nentries;jentry++) {
      tree->GetEntry(jentry);   
      update_bounds_north(theta,phi12,phi23);
    }
//      for (int jentry=0; jentry<nentries;jentry++) {
//        tree->GetEntry(jentry);   
//        double ptest = get_us_momentum(0,theta,phi12,phi23);
//        std::cout << "ptotus: " << ptotus << "  ptest: " << ptest << std::endl;
//      }
    
    // Allocate the resources
    //
    MUTOO::TRACE(" initializing lookup");
    initialize_lookup_north();
    
    // Set the root field point
    //
    for (int jentry=0; jentry<nentries;jentry++) {
      tree->GetEntry(jentry);   
      add_point_north(theta,phi12,phi23,ptotus,ptotvx);    
    }
    f.Close();
  }

  _initialized = true;  
  _timer.print();
  MUTOO::PRINT(std::cout,"Momentum Lookup Initialization Done");  
}


double
TMutMomLookup::get_us_momentum(unsigned short arm, const double& theta, const double& phi12, const double& phi23) const
{
  std::vector<Node>* lookup = (arm==MUTOO::South) ? _south : _north;
  ULong_t key=0;
  typedef size_t (TMutMomLookup::* HashFunctionPtr) (const double&, const double&, const double&,
						     int, int, int) const; 
  
  // Set the hash function pointer to either north or south hash function
  //
  HashFunctionPtr hash_func = (arm==MUTOO::South) ? &TMutMomLookup::hash_function_south :
    &TMutMomLookup::hash_function_north;
  
  // Search from hashed bin backward and forwards until find a non-emtpy hash cell
  // in the lookup table.  This is primitive -- having enough Monte-Carlo statistics
  // to ensure no empty hash cells over a given range would be a start.
  //
  ULong_t offset = 0;
  while(1) {
    
    // + offset
    //
    key = (this->*hash_func)(theta, phi12, phi23, 0, offset, offset);
    const Node& hash_pos = (*lookup)[key];
    if(hash_pos.count != 0) return hash_pos.ptotus;
    
    // - offset
    //			  
    key = (this->*hash_func)(theta, phi12, phi23, 0, -1*offset, -1*offset);
    const Node& hash_neg = (*lookup)[key];
    if(hash_neg.count != 0) return hash_neg.ptotus;
    
    // Increment offset with sanity check
    //
    offset++;
    enum {MAX_OFFSET = 100};
    if(offset>MAX_OFFSET) {
      throw std::runtime_error(DESCRIPTION("momentum lookup failed due to out of range offset"));
    }
  }
}

void 
TMutMomLookup::add_point_north(const double& theta, const double& phi12, const double& phi23,
			       const double& ptotus, const double& ptotvx)
{
  // Hash input values
  //
  ULong_t key = hash_function_north(theta,phi12,phi23);
  
  // Get the current values from this node
  //
  double m_theta = (*_north)[key].theta;
  double m_phi12 = (*_north)[key].phi12;
  double m_phi23 = (*_north)[key].phi23;
  double m_ptotus = (*_north)[key].ptotus;
  double m_ptotvx = (*_north)[key].ptotvx;
  double count = (*_north)[key].count;
  
  // Update the mean values
  //
  (*_north)[key].theta = (count*m_theta + theta)/(count+1);
  (*_north)[key].phi12 = (count*m_phi12 + phi12)/(count+1);
  (*_north)[key].phi23 = (count*m_phi23 + phi23)/(count+1);
  (*_north)[key].ptotus = (count*m_ptotus + ptotus)/(count+1);
  (*_north)[key].ptotvx = (count*m_ptotvx + ptotvx)/(count+1);
  (*_north)[key].count++;
}

void 
TMutMomLookup::add_point_south(const double& theta, const double& phi12, const double& phi23,
			       const double& ptotus, const double& ptotvx)
{
  // Hash input values
  //
  ULong_t key = hash_function_south(theta,phi12,phi23);

  // Get the current values from this node
  //
  double m_theta = (*_south)[key].theta;
  double m_phi12 = (*_south)[key].phi12;
  double m_phi23 = (*_south)[key].phi23;
  double m_ptotus = (*_south)[key].ptotus;
  double m_ptotvx = (*_south)[key].ptotvx;
  double count = (*_south)[key].count;
  
  // Update the mean values
  //
  (*_south)[key].theta = (count*m_theta + theta)/(count+1);
  (*_south)[key].phi12 = (count*m_phi12 + phi12)/(count+1);
  (*_south)[key].phi23 = (count*m_phi23 + phi23)/(count+1);
  (*_south)[key].ptotus = (count*m_ptotus + ptotus)/(count+1);
  (*_south)[key].ptotvx = (count*m_ptotvx + ptotvx)/(count+1);
  (*_south)[key].count++;
}


void 
TMutMomLookup::update_bounds_south(const double& theta, const double& phi12, const double& phi23)
{

  if(phi12 > 4 || phi23 > 3 || theta > 35 || theta < 12) return;

  long i_theta = static_cast<int>(std::floor(theta/CELL_WIDTH_THETA));
  long i_phi12 = static_cast<int>(std::floor(phi12/CELL_WIDTH_PHI12));
  long i_phi23 = static_cast<int>(std::floor(phi23/CELL_WIDTH_PHI23));

  MIN_THETA_SOUTH = std::min(i_theta,MIN_THETA_SOUTH);
  MIN_PHI12_SOUTH = std::min(i_phi12,MIN_PHI12_SOUTH);
  MIN_PHI23_SOUTH = std::min(i_phi23,MIN_PHI23_SOUTH);

  MAX_THETA_SOUTH = std::max(i_theta,MAX_THETA_SOUTH);
  MAX_PHI12_SOUTH = std::max(i_phi12,MAX_PHI12_SOUTH);
  MAX_PHI23_SOUTH = std::max(i_phi23,MAX_PHI23_SOUTH);
}

void 
TMutMomLookup::update_bounds_north(const double& theta, const double& phi12, const double& phi23)
{

  if(phi12 > 6 || phi23 > 6 || theta > 35 || theta < 10) return;

  long i_theta = static_cast<int>(std::floor(theta/CELL_WIDTH_THETA));
  long i_phi12 = static_cast<int>(std::floor(phi12/CELL_WIDTH_PHI12));
  long i_phi23 = static_cast<int>(std::floor(phi23/CELL_WIDTH_PHI23));

  MIN_THETA_NORTH = std::min(i_theta,MIN_THETA_NORTH);
  MIN_PHI12_NORTH = std::min(i_phi12,MIN_PHI12_NORTH);
  MIN_PHI23_NORTH = std::min(i_phi23,MIN_PHI23_NORTH);

  MAX_THETA_NORTH = std::max(i_theta,MAX_THETA_NORTH);
  MAX_PHI12_NORTH = std::max(i_phi12,MAX_PHI12_NORTH);
  MAX_PHI23_NORTH = std::max(i_phi23,MAX_PHI23_NORTH);
}

void 
TMutMomLookup::initialize_lookup_south()
{

  // Offset 2 is (1 for distance calc + 1 for empty border)
  //
  _size_theta_south = (MAX_THETA_SOUTH-MIN_THETA_SOUTH + 2); 
  _size_phi12_south = (MAX_PHI12_SOUTH-MIN_PHI12_SOUTH + 2); 
  _size_phi23_south = (MAX_PHI23_SOUTH-MIN_PHI23_SOUTH + 2); 
  
  // Allocate storage for lookup
  //
  _south = new std::vector<Node>(_size_theta_south*_size_phi12_south*_size_phi23_south);  
}

void 
TMutMomLookup::initialize_lookup_north()
{
  
  // Offset 2 is (1 for distance calc + 1 for empty border)
  //
  _size_theta_north = (MAX_THETA_NORTH-MIN_THETA_NORTH + 2); 
  _size_phi12_north = (MAX_PHI12_NORTH-MIN_PHI12_NORTH + 2); 
  _size_phi23_north = (MAX_PHI23_NORTH-MIN_PHI23_NORTH + 2); 
  
  // Allocate storage for lookup
  //
  _north = new std::vector<Node>(_size_theta_north*_size_phi12_north*_size_phi23_north);  
}

inline size_t TMutMomLookup::hash_function_south(const double& theta, const double& phi12, const double& phi23,
						 int theta_offset, int phi12_offset, int phi23_offset)  const
{  
  // hash cell theta 
  //
  size_t i_theta = static_cast<int>(std::floor(theta/CELL_WIDTH_THETA)) - MIN_THETA_SOUTH;
  
  // hash cell phi12 
  //
  size_t i_phi12 = static_cast<int>(std::floor(phi12/CELL_WIDTH_PHI12)) - MIN_PHI12_SOUTH;
  
  // hash cell phi23 
  //
  size_t i_phi23 = static_cast<int>(std::floor(phi23/CELL_WIDTH_PHI23)) - MIN_PHI23_SOUTH;

  // Boundary checks
  //
  i_theta = (i_theta < _size_theta_south-2) ? i_theta : _size_theta_south - 2;
  i_phi12 = (i_phi12 < _size_phi12_south-2) ? i_phi12 : _size_phi12_south - 2;
  i_phi23 = (i_phi23 < _size_phi23_south-2) ? i_phi23 : _size_phi23_south - 2;
  i_theta = (i_theta > 0) ? i_theta : 0;
  i_phi12 = (i_phi12 > 0) ? i_phi12 : 0;
  i_phi23 = (i_phi23 > 0) ? i_phi23 : 0;

  return (i_theta+theta_offset)*_size_theta_south*_size_phi12_south + 
    (i_phi12+phi12_offset)*_size_theta_south + i_phi23+phi23_offset;
}

inline size_t TMutMomLookup::hash_function_north(const double& theta, const double& phi12, const double& phi23,
						 int theta_offset, int phi12_offset, int phi23_offset) const
{  
  // hash cell theta 
  //
  size_t i_theta = static_cast<int>(std::floor(theta/CELL_WIDTH_THETA)) - MIN_THETA_NORTH;
  
  // hash cell phi12 
  //
  size_t i_phi12 = static_cast<int>(std::floor(phi12/CELL_WIDTH_PHI12)) - MIN_PHI12_NORTH;
  
  // hash cell phi23 
  //
  size_t i_phi23 = static_cast<int>(std::floor(phi23/CELL_WIDTH_PHI23)) - MIN_PHI23_NORTH;

  // Boundary checks
  //
  i_theta = (i_theta < _size_theta_north-2) ? i_theta : _size_theta_north - 2;
  i_phi12 = (i_phi12 < _size_phi12_north-2) ? i_phi12 : _size_phi12_north - 2;
  i_phi23 = (i_phi23 < _size_phi23_north-2) ? i_phi23 : _size_phi23_north - 2;
  i_theta = (i_theta > 0) ? i_theta : 0;
  i_phi12 = (i_phi12 > 0) ? i_phi12 : 0;
  i_phi23 = (i_phi23 > 0) ? i_phi23 : 0;
  
  return (i_theta+theta_offset)*_size_theta_north*_size_phi12_north + 
    (i_phi12+phi12_offset)*_size_theta_north + i_phi23+phi23_offset;
}

double
TMutMomLU::get_ephi12(TMutTrkMap::const_pointer trk_ptr)
{

  std::pair<bool,PHVector> tangent_pair = TMutTrackUtil::estimate_tangent(trk_ptr, MUTOO::Station1);
  if(!tangent_pair.first) return 0;
  PHVector& tangent = tangent_pair.second;

  //  Extrapolate tangent to station 2 and calculate delta phi
  //
  PHPoint point[3];
  TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()) {
    unsigned short station = stub_ptr->get()->get_station();
    point[station] = stub_ptr->get()->get_fit_par()->get_point();
  }
  
  double deltaz = point[1].getZ() - point[0].getZ();
  PHPoint point01(point[0].getX() + (tangent.getX()/tangent.getZ())*(deltaz),
		  point[0].getY() + (tangent.getY()/tangent.getZ())*(deltaz),
		  point[1].getZ());
  
  double mod1 = std::sqrt(MUTOO::SQUARE(point[1].getX()) + MUTOO::SQUARE(point[1].getY()));
  double mod01 = std::sqrt(MUTOO::SQUARE(point01.getX()) + MUTOO::SQUARE(point01.getY()));

  double ephi12 = MUTOO::RAD_TO_DEG*std::acos( (1.0/(mod01*mod1)) * 
					       (point01.getX()*point[1].getX() + 
						point01.getY()*point[1].getY()));
  return ephi12;
}

double
TMutMomLU::get_ephi23(TMutTrkMap::const_pointer trk_ptr)
{

  std::pair<bool,PHVector> tangent_pair = TMutTrackUtil::estimate_tangent(trk_ptr, MUTOO::Station2);
  if(!tangent_pair.first) return 0;
  PHVector& tangent = tangent_pair.second;

  //  Extrapolate tangent to station 2 and calculate delta phi
  //
  PHPoint point[3];
  TMutStubMap::const_key_iterator stub_iter = trk_ptr->get()->get_associated<TMutStub>();
  while(TMutStubMap::const_pointer stub_ptr = stub_iter.next()) {
    unsigned short station = stub_ptr->get()->get_station();
    point[station] = stub_ptr->get()->get_fit_par()->get_point();
  }
  
  double deltaz = point[2].getZ() - point[1].getZ();
  PHPoint point12(point[1].getX() + (tangent.getX()/tangent.getZ())*(deltaz),
		  point[1].getY() + (tangent.getY()/tangent.getZ())*(deltaz),
		  point[2].getZ());
  
  double mod2 = std::sqrt(MUTOO::SQUARE(point[2].getX()) + MUTOO::SQUARE(point[2].getY()));
  double mod12 = std::sqrt(MUTOO::SQUARE(point12.getX()) + MUTOO::SQUARE(point12.getY()));

  double ephi23 = MUTOO::RAD_TO_DEG*std::acos( (1.0/(mod12*mod2)) * 
					       (point12.getX()*point[2].getX() + 
						point12.getY()*point[2].getY()));
  return ephi23;
}




