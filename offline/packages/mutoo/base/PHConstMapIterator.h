//////////////////////////////////////////////////////////////////
//
// Utility class: PHConstMapIterator
// Author: S.Kelly 
// Date: 2/29/01
// Description: Const iterator for PHMap
//              
//
//////////////////////////////////////////////////////////////////

#ifndef __PHCONSTMAPITERATOR_H__
#define __PHCONSTMAPITERATOR_H__

// BOOST headers
//
#include<boost/smart_ptr.hpp>

// STL headers
//
#include<map>

// PHENIX headers
//
#include<PHMapIterator.h>

// forward declarations
//
//template<typename Key, typename Value> class PHMap;

//! Provides access to PHMap elements.

/*! \ingroup classes
  Const version of PHMapIterator. Please see PHMapIterator documentation for
  detailed description.
*/

template<typename Key, typename Value> class PHConstMapIterator
{
public:   
  
  typedef boost::shared_ptr<Value> value_type;
  typedef const value_type* const_pointer;
  typedef const value_type& const_reference;

  // public constructors
  //

  PHConstMapIterator();
  PHConstMapIterator(const PHConstMapIterator&);
  PHConstMapIterator(const PHMapIterator<Key,Value>&);

  // public methods
  //

  const_pointer next();

  /*! returns a const_reference to current value */
  const_reference operator*(){
#ifdef DEBUG
    sanity_check();
#endif    
    return _current->second;
  }

  /*! *iter. */
  const_pointer operator->(){
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
  typedef typename std::map<Key,value_type>::const_iterator private_iterator;
  
  // private constructors used by PHMap
  //
  PHConstMapIterator(const private_iterator& lower, 
		     const private_iterator& current, 
		     const private_iterator& upper);
  
  // construct from range specified by two PHConstMapIterators
  //
  PHConstMapIterator(const PHConstMapIterator<Key,Value>& lower, 
		     const PHConstMapIterator<Key,Value>& upper);
  
#endif
  
 private:
  
  //  friend PHMap<Key,Value>;
  
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
/*! Default constructor */
template <typename Key, typename Value>
PHConstMapIterator<Key,Value>::PHConstMapIterator() :
  _lower_bound(0),
  _current(0),
  _upper_bound(0){;}

/*! Copy constructor*/
template <typename Key, typename Value>
PHConstMapIterator<Key,Value>::PHConstMapIterator(const PHConstMapIterator& in) :
  _lower_bound(in._lower_bound),
  _current(in._current),
  _upper_bound(in._upper_bound){;}

/*! Conversion from mutable to non-mutable iterator*/
template <typename Key, typename Value>
PHConstMapIterator<Key, Value>::PHConstMapIterator(const PHMapIterator<Key,Value>& in) :
  _lower_bound(in._lower_bound),
  _current(in._current),
  _upper_bound(in._upper_bound){;}

template <typename Key, typename Value>
PHConstMapIterator<Key,Value>::PHConstMapIterator(const private_iterator& lower, 
						  const private_iterator& current, 
						  const private_iterator& upper) :
  _lower_bound(lower),
  _current(current),
  _upper_bound(upper){;}

template <typename Key, typename Value>
PHConstMapIterator<Key,Value>::PHConstMapIterator(const PHConstMapIterator<Key,Value>& lower, 
						  const PHConstMapIterator<Key,Value>& upper) :
  _lower_bound(lower._current),
  _current(lower._current),
  _upper_bound(upper._current){;}

/*!
  ROOT style iteration over map elements. <br><br>
 */

template <typename Key, typename Value>
typename PHConstMapIterator<Key,Value>::const_pointer 
PHConstMapIterator<Key,Value>::next()
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

/*! increment operator */
template <typename Key, typename Value>
void PHConstMapIterator<Key,Value>::operator++(){_current++;}                         

/*! prefix form of increment operator */
template <typename Key, typename Value>
void PHConstMapIterator<Key,Value>::operator++(int){_current++;}                         

#endif















