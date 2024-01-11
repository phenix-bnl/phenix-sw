		
#include <mFvtxFindTrackPar.h>
		
#include<iostream>

using namespace std;

//class imp for mFvtxFindTrackPar, so it can work with CINT
ClassImp(mFvtxFindTrackPar);
		

void
mFvtxFindTrackPar::obsolete_warning(const std::string & function,
    const std::string & suggestion)
{
  std::cout << std::endl;
  std::cout << "mFvtxFindTrackPar - WARNING - mFvtxFindTrackPar::" << function << " is obsolete."
      << " Please use " << suggestion << " instead" << std::endl;
  std::cout << std::endl;
  sleep(5);
}
