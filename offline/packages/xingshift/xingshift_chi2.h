#ifndef __xingshift_chi2_h_
#define __xingshift_chi2_h_

/////////////////////////////////////////////////////////
//
// class xingshift_chi2
//
//      Author: Kazuya Aoki
//              April 25,2005
//
/////////////////////////////////////////////////////////

#include <iostream>

class xingshift_chi2{
public:
  int ishift;
  int relativeshift;
  float chi2;

  xingshift_chi2():ishift(0),relativeshift(0),chi2(0.) {}
  void show(std::ostream& out = std::cout) const;

  enum error_code{
    multiple_match = -100, no_match = -200
  };
};

bool operator> (const xingshift_chi2& obj1, const xingshift_chi2& obj2);
bool operator< (const xingshift_chi2& obj1, const xingshift_chi2& obj2);
bool operator<= (const xingshift_chi2& obj1, const xingshift_chi2& obj2);
bool operator>= (const xingshift_chi2& obj1, const xingshift_chi2& obj2);

#endif
