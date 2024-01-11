// $Id: PHConstKeyIterator.h,v 1.3 2007/05/28 14:36:12 hpereira Exp $

/*!
	 \file PHKeyIterator.h
	 \brief Const version of PHKeyIterator
	 \author S. Kelly
	 \version $Revision: 1.3 $
	 \date $Date: 2007/05/28 14:36:12 $
*/

#ifndef __PHCONSTKEYITERATOR_H__
#define __PHCONSTKEYITERATOR_H__

// PHENIX headers
//
#include<PHMap.h>
#include<PHKey.hh>

// BOOST headers
//
#include<boost/smart_ptr.hpp>
#include<boost/any.hpp>

//! PHMap iteration from a key list
/*! \ingroup classes
	Const version of PHKeyIterator.	
	Please see PHKeyIterator documentation for
	a detailed description.	
*/

template<typename Value> class PHConstKeyIterator
{
public:	 

	typedef boost::shared_ptr<Value> value_type;
	typedef std::ptrdiff_t difference_type;
	typedef std::forward_iterator_tag iterator_category;
	typedef const value_type* const_pointer;
	typedef const value_type& const_reference;

	/*! copy constructor */
	PHConstKeyIterator(const PHConstKeyIterator&);

	/*! Conversion from mutable to non-mutable iterator */
	PHConstKeyIterator(const PHKeyIterator<Value>&);
	
	PHConstKeyIterator(const std::pair<PHKey::const_associate_iterator,
				 PHKey::const_associate_iterator>&);
	
	const_pointer next(); 
	
	const_pointer current();

	/*! returns a reference to current value */
	const_reference operator*()
	{
#ifdef DEBUG
		sanity_check();
#endif		
		return *_current;
	} 
	
	/*! *iter. */
	const_pointer operator->()
	{
#ifdef DEBUG
		sanity_check();
#endif		
		return &(*_current);
	} 
	
	/*! returns a pointer to the current value */
	const_pointer get()
	{
#ifdef DEBUG
		sanity_check();
#endif		
		return _current;
	} 
	
	/*! increment operator*/
	void operator++(); 

	/*! prefix form increment operator*/
	void operator++(int); 

	/*! operator= */
	void operator=(const PHConstKeyIterator& rhs) 
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

	//! default constructor (private to forbid access)
	PHConstKeyIterator()
	{;}
	
	/*! 
		Cache the current shared pointer here.	Used by next()
		to provide access via pointer while holding associated 
		objects via shared pointer.
	*/
	typedef typename std::vector<value_type>::iterator private_iterator;	
	
	std::vector<value_type> _values;
	
	private_iterator _current;
	
	/*! 
		We provide a sanity check function to ensure users don't attempt data
		access from a out of bounds iterator.
	*/
	void sanity_check() 
	{ 
		if(_current == _values.end()) {
			throw std::out_of_range(DESCRIPTION("Attempt to access data from out of range iterator\n"));		
		}
	}

};

//___________________________________________________________
template <typename Value> PHConstKeyIterator<Value>::PHConstKeyIterator(const PHConstKeyIterator& in) :
	_values(in._values) 
{
	_current = _values.begin();
}



//___________________________________________________________ 
template <typename Value> PHConstKeyIterator<Value>::PHConstKeyIterator(const PHKeyIterator<Value>& in) :
	_values(in._values)
{
	_current = _values.begin();
}

//___________________________________________________________  
/*! 
	Construct from an instance of PHKey::key_list_type and a 
	reference to the container associated with the input keys. 
*/
template <typename Value> PHConstKeyIterator<Value>::PHConstKeyIterator(const std::pair<PHKey::const_associate_iterator,
								PHKey::const_associate_iterator>& in_pair) 
{

  // Here we do the work to any_cast the range of input values into 
	// shared pointers to Value.	
	PHKey::const_associate_iterator lcl_iter = in_pair.first;
	for(;lcl_iter!=in_pair.second;++lcl_iter)
  {
		_values.push_back( boost::any_cast<value_type>(lcl_iter->second) );
	}	
	_current = _values.begin();

}

//___________________________________________________________   
/*! 
	ROOT style bounded iterator next see example in 
	PHMapIterator<Key,Value>::next documentation 
*/
template <typename Value> typename PHConstKeyIterator<Value>::const_pointer 
PHConstKeyIterator<Value>::next()
{
	// end of range 
	if(_current == _values.end()) { return 0; }		

  // increment current and return address of shared pointer
	const_pointer tmp = &(*_current);
	++_current;
	return tmp;
}

//___________________________________________________________   
template <typename Value> typename PHConstKeyIterator<Value>::const_pointer 
PHConstKeyIterator<Value>::current()
{
	// end of range 
	if(_current == _values.end()) { return 0; }		

  // return address of shared pointer
	return _current;
}

//___________________________________________________________    
template <typename Value> void PHConstKeyIterator<Value>::operator++()
{_current++;}

//___________________________________________________________    
template <typename Value> void PHConstKeyIterator<Value>::operator++(int)
{_current++;}

#endif










