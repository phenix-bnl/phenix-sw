#ifndef _TBunch_h_
#define _TBunch_h_

//////////////////////////////////////////////////////////////////////////////
//
// class TBunch
//
//   "# of events" observed as Up/Down/Unpol/Unfill is stored.
//   "# of events" is defined as float so you can do any normalization you like.
//
//          Author: Kazuya Aoki ( Kyoto Univ. )
//                  April 25,2005
//////////////////////////////////////////////////////////////////////////////

#include <iostream>

class TBunch
{
public: // difinitions of constants.
  static const int npat = 4;
  enum pattern {
    up = 0, down = 1, unpol = 2,unfill = 3
  };
public: // methods
  TBunch(int nup,int ndown,int nunpol,int nunfill);
  TBunch();

  void clear();

  float get(pattern pat) const;       // if pat is out of bound, you get zero.
  void set(pattern pat,float value);  // if pat is out of bound, it do nothing.
  const float& at(pattern pat) const; // if pat is out of bound, dummy entry will be returned.
  float& at(pattern pat);             // if pat is out of bound, dummy entry will be returned.

  void show(std::ostream& out = std::cout) const;

  pattern get_index_majority() const;
    // get pattern of the majority.
    // if get_total() == 0 , you get TBunch::unfill

  float get_total() const;
  void multiply(float val);

  float chi2(pattern pat) const; // if pat is out of bound, you get zero.
  float instability() const;

  float chi2_neglect_pattern(float point = 1.0) const; // ( up+down+unpol+unfill - point )**2

public: // some utility functions 
  // conversion function     up/down/unpol/unfill  <--> 1/-1/0/-99
  static int pat2num(pattern pat); // if pat is out of bound, you get -99
  static pattern num2pat(int int_pat);  // if int_pat is not 1/-1/0/-99, you get unfill
  static char get_char(pattern pat);   // if pat is out of bound, you get '?'

private: // methods for internal use
  static bool bound_check(pattern pat);

private: // constants for internal use
  const static int int_pattern[npat];

private: // member variables
  float nevts[npat];
  static float dummy;
};

#endif
