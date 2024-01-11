#include "ana_dst.h"
#include "TMutMathieson.h"

int run_test() 
{
  std::cout << "hello world" << std::endl;
  boost::array<double,3> charges = {{1,1,1}};

  // Feed the below function with q1, q2, and q3 from
  // a Mathieson distribution
  //
  double x = TMutMathieson::get_x_local(charges);

  // Save in an NTUPLE x qp/qtot
  //
  std::cout << x << std::endl;
  return 0;  
}

int process_event (PHCompositeNode *dst_node)
{    
  return 0;
}

int setup_all(PHCompositeNode* dst_node) {
  return 0;
};

int end_all() {
  return 0;
}

void setup_display() {
}

int dinit() {
  return 0;
}








