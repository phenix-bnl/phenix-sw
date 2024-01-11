//////////////////////////////////////////////////////////////////
//
// Utility class: PHMapIterator
// Author: S.Kelly 
// Date: 2/29/01
// Description: Iterator for PHMap
//              
//
//////////////////////////////////////////////////////////////////

#ifndef __PHMAPITERATOR_H__
#define __PHMAPITERATOR_H__

#include "PHException.h"

// BOOST headesr
//
#include<boost/smart_ptr.hpp>

// STL headers
//
#include<map>
#include<iterator>

// forward declarations
//
template<typename Key, typename Value> class PHConstMapIterator;
//template<typename Key, typename Value> class PHMap;

//! Provides access to PHMap elements.

/*! \ingroup classes

PHMapIterator provides access to PHMap elements.  The important 
aspects of the public interface are denumerated below.

<ul>
<li> 
The object returned by de-referencing a PHMapIterator or by the
member function next() is a reference counted pointer to the object
the user wishes to access.  To access the object held by the reference
pointer one envokes the ref pointers member function boost::shared_ptr::get().  
This is an important design feature of the PHMap class and associated iterator
classes that removes the responsibility for object resource deallocation
from the user.  As long as objects are held and manipulated by their
associated ref pointers they will remain in scope mitigating the
usual memory leaks and wild pointers associated with manipulating heap
allocated objects with raw pointers.  The cost to the user is the 
additional call to boost::shared_ptr::get() to access the desired object.
Please see the member function documentation for next() and operator++()
for usage examples.<br>

<li>
This class supports ROOT style iteration with the PHMapIterator::next() 
method and iterator bounds. PHMapIterator differs from STL iterators
in that the iterator bounds are part of the iterator state.  In other
words iterators have a begin() and an end().  When you first bring an
iterator into being, its current value is equal to its lower bound and
one can safely advance the iterator until its current value is equal
to the upper bound. The next() method returns a pointer to the PHMap 
value_type and advances the iterators current value.  If the current
value is equal to the upper bound it next() returns the null pointer.  

<li>
STL forward-iterator behavior is supported by PHMapIterator::operator++().
In this case the user is responsible for iterator initialization and 
explicitly advances the iterator in a for loop while checking that 
the current value has not reached the terminal value.    Note as is the 
case in STL containers the upper bound is one past the last element 
in a range.  That is ranges are defined [lower,upper).  

<li>
PHMapIterator satisfies the requirements of an STL forward iterator so 
it can be used with algorithms in <algorithm> appropriate for associated 
(ordered) containers.
*/

template<typename Key, typename Value> class PHMapIterator
{
public:   
  
  //!* typedefs for compliance with STL forward iterator requirements */
  typedef boost::shared_ptr<Value> value_type;
  typedef std::ptrdiff_t difference_type;
  typedef std::forward_iterator_tag iterator_category;
  typedef value_type* pointer;
  typedef value_type& reference;

  PHMapIterator();
  PHMapIterator(const PHMapIterator&);
  pointer next();
  pointer current();

  /*! returns a reference to current value */
  reference operator*(){
#ifdef DEBUG
    sanity_check();
#endif    
    return _current->second;
  }

  /*! *iter. */
  pointer operator->(){
#ifdef DEBUG
    sanity_check();
#endif    
    return &_current->second;
  }

  void operator++();
  void operator++(int);

  /*! Returns the number of valid elements in this iterators range */
  size_t count() const 
  { return std::distance(_lower_bound,_upper_bound); }

  //! return true if empty
  bool empty() const
  { return _upper_bound == _lower_bound; }

  /*! Set iterator current value to lower bound */
  void reset() {_current = _lower_bound;} 

  /*! Returns true if iterators current value == upper bound */
  bool at_end() {return _current == _upper_bound;} 

#ifndef DOXYGEN_SHOULD_SKIP_THIS

  // Part of the public interface to avoid the additional template parameter
  // necessary to make PHMap a friend.  We don't document because there this
  // is not really part of the API
  //

  // private constructors used by PHMap and begin() end()
  //
  typedef typename std::map<Key,value_type>::iterator private_iterator;  
  PHMapIterator(const private_iterator& lower, 
		const private_iterator& current, 
		const private_iterator& upper);

  // construct from range specified by two PHMapIterators
  //
  PHMapIterator(const PHMapIterator<Key,Value>& lower, 
		const PHMapIterator<Key,Value>& upper);
#endif
  
 private:
  
  friend class PHConstMapIterator<Key,Value>;
  
  // private data members
  //
  private_iterator _lower_bound;
  private_iterator _current;
  private_iterator _upper_bound;

  // We provide a sanity check function to ensure users don't 
  // attempt data access from an out of bounds iterator.
  //
  void sanity_check() { 
    if(_current == _upper_bound) {
      throw std::out_of_range(DESCRIPTION("Attempt to access data from out of range iterator"));    
    }
  }
};

//
// non inlined member function definitions
//
template <typename Key, typename Value>

/*! Default constructor */
PHMapIterator<Key,Value>::PHMapIterator() :
  _lower_bound(0),
  _current(0),
  _upper_bound(0){;}

template <typename Key, typename Value>

/*! Copy constructor */
PHMapIterator<Key,Value>::PHMapIterator(const PHMapIterator<Key,Value>& in) :
  _lower_bound(in._lower_bound),
  _current(in._current),
  _upper_bound(in._upper_bound){;}

// used by PHMap to return a iterator with specific range
//
template <typename Key, typename Value>
PHMapIterator<Key,Value>::PHMapIterator(const private_iterator& lower, 
					const private_iterator& current, 
					const private_iterator& upper) :
  _lower_bound(lower),
  _current(current),
  _upper_bound(upper){;}

// used by PHMap to return an iterator with a specific range
//
template <typename Key, typename Value>
PHMapIterator<Key,Value>::PHMapIterator(const PHMapIterator<Key,Value>& lower, 
					const PHMapIterator<Key,Value>& upper) :
  _lower_bound(lower._current),
  _current(lower._current),
  _upper_bound(upper._current){;}

/*!
  ROOT style iteration over map elements. <br><br>  Returns a pointer
  to the iterators current value_type and advances the iterator.

  \b Example <br> <br>
  \code

  #include <PHMap.h>
  #include <iostream>

  int main(){  
    // Heap allocate a map. The user is *not* responsible
    // for deleting the map.
    //
    PHMap<int,int>* map = new PHMap<int,int>();
    for(int i=0; i<10; i++){
      map->insert(i,1000+i);
    }

    // get an iterator with keys in range [2,5)
    //
    PHMapIterator<int,int>::iterator iter = map->find(2,5);
    // iterate over range
    //
    while(boost::shared_ptr<int>* i = iter.next()){
      std::cout << *(i->get()) << std::endl;
    }
  }
  // output
  //
  1002
  1003
  1004

  \endcode
*/

template <typename Key, typename Value>
typename PHMapIterator<Key,Value>::pointer 
PHMapIterator<Key,Value>::next()
{
  // end of range 
  //
  if(_current == _upper_bound) {
    return 0; 
  }    
  // increment _current;
  //
  private_iterator tmp = _current++;
  return &(tmp->second);
}

/*! 
  Returns a pointer to the iterators currrent value and does not
  advance the iterator.  Contrast with next that also returns a
  pointer to the current value_type but does not advance the 
  iterator.
*/
template <typename Key, typename Value>
typename PHMapIterator<Key,Value>::pointer 
PHMapIterator<Key,Value>::current()
{
  // end of range 
  //
  if(_current == _upper_bound) {
    return 0; 
  }    
  // increment _current;
  //
  return &(_current->second);
}

/*! 

  Supports STL-like forward iteration.  The user initializes
  the iterator and checks that is has not reached the  terminal 
  value of its range in a for loop. <br> <br>
  
  \b Example <br> <br>

  \code 

  #include <PHMap.h>
  #include <iostream>
  
  int main()
  {  
    // Heap allocate a map. The user is *not* responsible
    // for deleting the map.
    //  
    PHMap<int,int>* map = new PHMap<int,int>();
    for(int i=0; i<5; i++){
      map->insert(i,1000+i);
    }
  
    // get iterator
    //
    PHMapIterator<int,int>::iterator iter = map->find(map.begin(),map.end());
  
    // STL-like iteration
    //
    for(iter.reset(); !iter.at_end(); iter++){
      std::cout << *(iter->get()) << std::endl;
    }
  }
  
  // output
  //
  1000
  1001
  1002
  1003
  1004
  
  \endcode

*/
template <typename Key, typename Value>
void PHMapIterator<Key,Value>::operator++(){_current++;}                         

/*! prefix form of increment operator */
template <typename Key, typename Value>
void PHMapIterator<Key,Value>::operator++(int){_current++;}                         

#endif










