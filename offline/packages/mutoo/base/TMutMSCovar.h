//////////////////////////////////////////////////////////////////
//
// Utility class: TMutMSCovar.h
// Author: S.Kelly 
// Date: 4/16/02
// Description: Encapsulates description of MS covariance in Fe/Cu absorber
//              
//////////////////////////////////////////////////////////////////

#ifndef __TMUTMSCOVAR_H__
#define __TMUTMSCOVAR_H__

#include<PHException.h>
#include<MUTOO.h>
#include<boost/array.hpp>
#include<fstream>

class TMutMSCovar
{
 public: 

  static const double get_ms_covar(unsigned short i, unsigned short j, double pz) {
    
    // initialization upon first use 
    //
    static parameter_array* covar = initialize();
    
    // MS matrix elements are power-law parameterized y = par1 * pz^par2
    //
    unsigned short index = i*COVAR_ROW+j;
    double value = covar->at(index).first * std::pow(std::fabs(pz),covar->at(index).second);
    return value;
  }
  
 private:  
  
  enum {COVAR_SIZE=25, COVAR_ROW=5};
  typedef boost::array<std::pair<double,double>, COVAR_SIZE> parameter_array;
  
  
  static parameter_array* initialize() {
    parameter_array* array_ptr = new parameter_array;
    array_ptr->assign(std::make_pair(0,0));
    ifstream file("covar_par.txt");      
    int i=0, j=0;
    double par1=0, par2=0;
    if(file.is_open()){
      while(!file.eof()){
	file >> i >> j >> par1 >> par2;      
	array_ptr->at(i*COVAR_ROW+j).first = par1;
	array_ptr->at(i*COVAR_ROW+j).second = par2;
      }
    } else {
      array_ptr->at(0*COVAR_ROW+0).first = 105.324;
      array_ptr->at(0*COVAR_ROW+0).second = -2.52672;
      array_ptr->at(1*COVAR_ROW+1).first =  84.1374;
      array_ptr->at(1*COVAR_ROW+1).second = -2.33436;
      array_ptr->at(2*COVAR_ROW+2).first =  0.0940602;
      array_ptr->at(2*COVAR_ROW+2).second = -1.66766;
      array_ptr->at(3*COVAR_ROW+3).first = 0.0118626;
      array_ptr->at(3*COVAR_ROW+3).second = -3.15337;
    }
    return array_ptr;
  }
  
};

#endif



