#include "PdbRxNPSurvey.hh"
#include <iostream>

using namespace std;

PdbRxNPSurvey::PdbRxNPSurvey() 
{
  for(int arm = 0; arm<2; arm++) 
      for(int phi = 0; phi<12; phi++) {
        for(int id = 0; id<6; id++) {
           X[arm][phi][id] = 0;
           Y[arm][phi][id] = 0;
           Z[arm][phi][id] = 0;
	}
      }
}

void PdbRxNPSurvey::print() const
{
  for(int arm = 0; arm<2; arm++) 
      for(int phi = 0; phi<12; phi++) {
	 for(int id = 0; id<6; id++) {
	   cout<<" arm: "<<arm<<" "<<" phi: "<<phi<<" id = "<<id
  	   <<" X: "<<X[arm][phi][id]
           <<" Y: "<<Y[arm][phi][id]
	   <<" Z:  "<<Z[arm][phi][id]<<endl;
	 }
      }
}
