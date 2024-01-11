#ifndef HASH_H
#define HASH_H

// $Id: hash_vector.hh,v 1.4 2006/12/20 17:04:59 hpereira Exp $ 

/*!
   \file hash_vector.hh
   \brief hashed vector class. 
   \author Timothy A. Budd
   \version $Revision: 1.4 $
   \date $Date: 2006/12/20 17:04:59 $
*/

#include <cstddef>
#include <vector>

//! hash vector class
/*! hash vector class. H is the template for the index. T is the template for the contents. */
template <class H, class T> class hashVector : public std::vector<T>
{
  public:
  
  //! constructors
  /*! arguments are the vector size and the hash function */
  hashVector(size_t max, size_t (*f)(const H &));
  
  //! constructor
  /*! arguments are the vector size, the hash function and the initial value for all elements */
  hashVector(size_t max, size_t (*f)(const H &), T & initialValue);
  
  //! copy constructor
  hashVector(const hashVector<H, T> & v);

  //! accessor
  T & operator [] (const H & index);
  
  //! constant accessor
  const T & operator [] (const H & index) const;

  private:
  
  //! hash function
  size_t (*hashfun)(const H &);
  
};

//----------------------------------------------------------------------
//	class hashVector implementation
//----------------------------------------------------------------------

template <class H, class T>
inline hashVector<H, T>::hashVector(size_t max, size_t (*f)(const H &))
  : std::vector<T>(max), hashfun(f)
  {}



template <class H, class T>
inline hashVector<H, T>::hashVector(size_t max, size_t (*f)(const H&), T& initialValue): 
  std::vector<T>(max, initialValue), hashfun(f)
  {}


template <class H, class T>
inline hashVector<H, T>::hashVector(const hashVector<H,T> & v)
  : std::vector<T>(v), hashfun(v.hashfun)
  {}

template <class H, class T>
inline T & hashVector<H, T>::operator[] (const H & index)
{
  // subscript a hash vector
  // hash the index value before indexing vector
  return std::vector<T>::operator[] ((*hashfun)(index) % this->size());
}


template <class H, class T>
inline const T & hashVector<H, T>::operator[] (const H & index) const
{ return std::vector<T>::operator[] ((*hashfun)(index) % this->size());}

#endif
