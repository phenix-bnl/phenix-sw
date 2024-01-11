// $Id: PHKeyIterator.h,v 1.3 2007/05/28 14:36:12 hpereira Exp $

/*!
	 \file PHKeyIterator.h
	 \brief PHMap iteration from a key list
	 \author S. Kelly
	 \version $Revision: 1.3 $
	 \date $Date: 2007/05/28 14:36:12 $
*/

#ifndef __PHKEYITERATOR_H__
#define __PHKEYITERATOR_H__
	
// PHENIX headers
//
#include<PHMap.h>
#include<PHKey.hh>
// BOOST headers
//
#include<boost/smart_ptr.hpp>
#include<boost/any.hpp>

// forward declerations
//
template<typename Value> class PHConstKeyIterator;

//! PHMap iteration from a key list

/*! \ingroup classes

PHKeyIterator models bounded iteration over list of associated objects.	
The iterator is initialized with a list of keys of type PHKey::key_list_type.	
Except for the constructor it provides the same public interface as 
PHMapIterator.	This class is used to provide iteration over objects 
that are associated with a primary object via a key list.	<br><br>

<h3>Example</h3>

Class TMutClus is associated with 3 instances of class TMutHit.	TMutClus
inherits from PHKey and uses the interface provided therin to store the
keys associated with the 3 instances of TMutHit. The TMutClus interface
provides a method that returns an instance of PHKeyIterator initialized with
the TMutClus key list.	The holder of the TMutClus object is therefore able
to iterate over the associated hits without having to explicitly access
the hits container and manually match the keys.

*/

template<typename Value> class PHKeyIterator
{
public:	 

	typedef boost::shared_ptr<Value> value_type;
	typedef std::ptrdiff_t difference_type;
	typedef std::forward_iterator_tag iterator_category;
	typedef value_type* pointer;
	typedef value_type& reference;

	PHKeyIterator(const PHKeyIterator&);
	PHKeyIterator(const std::pair<PHKey::associate_iterator, PHKey::associate_iterator>&);

	pointer next(); 
	pointer current();

	/*! returns a reference to current value */
	reference operator*()
	{		
#ifdef DEBUG
		sanity_check();
#endif		
		return *_current;
	} 
	
	/*! *iter. */
	pointer operator->()
	{
#ifdef DEBUG
		sanity_check();
#endif		
		return &(*_current);
	} 
	
	/*! returns a pointer to the current value */
	pointer get(){
#ifdef DEBUG
		sanity_check();
#endif		
		return &(*_current);
	} 
	
	/*! increment operator*/
	void operator++(); 

	/*! prefix form increment operator*/
	void operator++(int); 
	
	/*! operator= */
	void operator=(const PHKeyIterator& rhs) 
	{
		_values = rhs._values;
		_current = _values.begin();
	}

	/*! Returns the number of valid elements in this iterators range */
	size_t count() const 
	{ return std::distance(_values.begin(),_values.end()); }
	
  //! return true if empty
  bool empty() const
  { return _values.empty(); }
  
	/*! Set iterator current value to lower bound */
	void reset() 
	{_current = _values.begin();}	
	
	/*! Returns true if iterators current value == upper bound */
	bool at_end() 
	{return _current == _values.end();}	
	
 private:

	//! no default constructor (private to forbid usage)
	PHKeyIterator()
	{}
		
	friend class PHConstKeyIterator<Value>;
	
	// Cache the current shared pointer here.	Used by next()
	// and current() to provide access via raw pointer while 
	// holding associated objects via shared pointer.
	typedef typename std::vector<value_type>::iterator private_iterator;	
	std::vector<value_type> _values;
	private_iterator _current;
	
	// We provide a sanity check function to ensure users don't attempt data
	// access from a out of bounds iterator.
	//
	void sanity_check() { 
		if(_current == _values.end()) {
			throw std::out_of_range(DESCRIPTION("Attempt to access data from out of range iterator\n"));		
		}
	}	
};

/*! 
	Copy constructor 
*/

template <typename Value>
PHKeyIterator<Value>::PHKeyIterator(const PHKeyIterator& in) :
	_values(in._values) 
{ _current = _values.begin(); }

/*! 
	Construct from a PHKey::key_list and a reference to the container associated
	with the input keys. 
*/

template <typename Value>
PHKeyIterator<Value>::PHKeyIterator(const std::pair<PHKey::associate_iterator, PHKey::associate_iterator>& in_pair) 
{
	// Here we do the work to any_cast the range of input values into 
	// shared pointers to Value.	
	//
	PHKey::associate_iterator lcl_iter = in_pair.first;
	for(;lcl_iter!=in_pair.second;++lcl_iter)
	_values.push_back( boost::any_cast<value_type>(lcl_iter->second) );	
	_current = _values.begin();
}

/*! 
	ROOT style bounded iterator next see example in 
	PHMapIterator<Key,Value>::next documentation 
*/

template <typename Value>
typename PHKeyIterator<Value>::pointer 
PHKeyIterator<Value>::next()
{
	// end of range 
	//
	if(_current == _values.end())
	return 0; 


	// increment current and return address of shared pointer
	//
	pointer tmp = &(*_current);
	++_current;
	return tmp;
}

/*! 
	ROOT style bounded iterator next see example in 
	PHMapIterator<Key,Value>::next documentation 
*/

template <typename Value>
typename PHKeyIterator<Value>::pointer 
PHKeyIterator<Value>::current()
{
	// end of range 
	if(_current == _values.end()) return 0; 

	// return address of shared pointer
	return &(*_current);
}

template <typename Value>
void PHKeyIterator<Value>::operator++()
{_current++;}

template <typename Value>
void PHKeyIterator<Value>::operator++(int)
{_current++;}

#endif
