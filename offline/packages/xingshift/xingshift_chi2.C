#include "xingshift_chi2.h"

#include<iostream>

using namespace std;

void xingshift_chi2::show(std::ostream& out) const
{
  out << "ishift:" << ishift
      << "\trelative:" << relativeshift
      << "\tchi2:" << chi2
      << endl;
   
}

bool operator> (const xingshift_chi2& obj1, const xingshift_chi2& obj2){
  return ( obj1.chi2 > obj2.chi2 );
}

bool operator< (const xingshift_chi2& obj1, const xingshift_chi2& obj2){
  return ( obj1.chi2 < obj2.chi2 );
}

bool operator<= (const xingshift_chi2& obj1, const xingshift_chi2& obj2){
  return ( obj1.chi2 <= obj2.chi2 );
}
bool operator>= (const xingshift_chi2& obj1, const xingshift_chi2& obj2){
  return ( obj1.chi2 >= obj2.chi2 );
}

