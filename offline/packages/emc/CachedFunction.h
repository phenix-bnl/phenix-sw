#ifndef __EMC_CACHEDFUNCTION_H__
#define __EMC_CACHEDFUNCTION_H__





#include <cmath>
#include <vector>
#include <functional>
#include <cassert>
#include <cstddef>

template <class ARG = float, class RET = double>
class my_unary_function {
public:
  typedef ARG argument_type;
  typedef RET result_type;
  
  virtual result_type operator()( argument_type x ) = 0;
};




template <class ARG, class RET>
class CachedFunction {
public:
  typedef ARG argument_type;
  typedef RET result_type;
  

public:
  CachedFunction(my_unary_function< argument_type, result_type > * f, size_t n, argument_type min, argument_type max):
    fx(0), dfx(0), function(f) {
    assert(n != 0);
    x0 = min;
    dx = (max - min) / n;
    
    resize_by_n(n);
  }



  CachedFunction(my_unary_function<argument_type, result_type> * f, argument_type min, argument_type stepsize):
    fx(0), dfx(0), function(f) {
    x0 = min;
    dx = stepsize;
  }


  ~CachedFunction(){
    delete function;
  }



  size_t resize_by_n(size_t n) const {
    size_t oldsize, newsize;


    oldsize = fx.size(); newsize = n + 2;
    fx.resize( newsize );
    for(size_t i = oldsize; i < newsize; i++) fx[i] = function->operator()(x0 + dx * (argument_type)i);

    oldsize = dfx.size(); newsize = fx.size() - 1;
    dfx.resize( newsize );
    for(size_t i = oldsize; i < newsize; i++) dfx[i] = (fx[i+1] - fx[i]) / dx;

    return n;
  }
  


  result_type operator[] (argument_type x) const {
    result_type my = operator()( x );
    result_type real = function->operator()( x );
    double ratio = my / real;
    if( XABS(ratio-1.0) > 1e-6 ) printf("%s: f(%12f) = %12g / %12g ratio=%12g\n", __PRETTY_FUNCTION__, x, real, my, ratio-1.0);
    return real;
  }



  result_type operator() (argument_type x) const {
    size_t n = (size_t)( (x - x0) / dx );
    double residual = x - (dx * n);

    if ( dfx.size() <= n ) n = resize_by_n(n);

    return fx[n] + residual * dfx[n];
  }
    

protected:
  inline result_type XABS(result_type x) const { return (x < 0) ? -x : x; }


protected:
  mutable argument_type x0;
  mutable argument_type dx;

  mutable std::vector< result_type > fx;
  mutable std::vector< result_type > dfx;

  mutable my_unary_function<argument_type, result_type> * function;
};





#endif /* ! __EMC_CACHEDFUNCTION_H__ */

