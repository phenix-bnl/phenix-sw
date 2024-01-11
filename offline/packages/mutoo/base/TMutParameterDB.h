// $Id: TMutParameterDB.h,v 1.1 2006/04/22 01:53:02 hpereira Exp $
#ifndef __TMutParameterDB_H__
#define __TMutParameterDB_H__

//////////////////////////////////////////////////////////////////
/*!
   \file    TMutParameterDB.h
   \brief   mutoo runtime parameter manager
   \author  Hugo Pereira
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 01:53:02 $
*/
//////////////////////////////////////////////////////////////////

#include <string>
#include <map>
#include <iostream>
#include <sstream>
#include <stdexcept>

#include <TROOT.h>
#include <TObject.h>

#include "MUTOO.h"

//////////////////////////////////////////////////////////////////
/*!
	\class   TMutParameterDB
	\brief   mutoo runtime parameter manager
*/
//////////////////////////////////////////////////////////////////

class TMutParameterDB: public TObject  {
	public:
	
	//! initialize with given filename
	static void initialize( const std::string filename ) 
  {
		if( !_pointer ) _pointer = new TMutParameterDB( filename );
		else std::cout 
      << "in TMutParameterDB::initialize - table already initialized from file \"" 
			<< _pointer->_filename << "\"\n";
	}
		
	//! return singleton
	static const TMutParameterDB& get( void ) 
  {
		if( !_pointer ) _pointer = new TMutParameterDB( "mutoo_parameters.txt" );
		return *_pointer;
	}
	
	//! returns filename
	std::string get_filename( void ) const 
  { return _filename; }

	//! verbosity
	const MUTOO::Verbosity& get_verbosity( void ) const
	{ return _verbosity; }
	
	//! verbosity
	void set_verbosity( const MUTOO::Verbosity& verbosity )
	{ _verbosity = verbosity; }
	
	//! dump all parameters
	void print( std::ostream &out = std::cout ) const;
	
	//! adds new or change existing key
	void add_raw( const char* key, const char* value ) 
  { if( key && value ) add_raw( std::string( key ), std::string( value ) ) ; }
		

	#ifndef __CINT__
	//! add/change
	template < typename T >
	void add( const std::string& key, const T& value ) 
	{
		std::string string_value;
		std::ostringstream s( string_value );
		s << value;
		add_raw( key, string_value );
	}
	
	//! returns string value associated to key
	template < typename T >
	bool get( const std::string & key, T& value ) const
	{
		// try get matching par
		const_par_iterator par = _get_const_iterator( key );
		if( par == _pars.end() ) return false;
		
		// dump string value in required type and test
		std::istringstream s( par->second );
		T tmp; s >> tmp;
		if( s.rdstate() & std::ios::failbit ) return false;
		
		// assign new value
		value = tmp;
		return true;
	}
	
	//! adds new or change existing key
	void add_raw( const std::string &, const std::string & );
	#endif

	protected:
	
	//! constructor
	TMutParameterDB( const std::string& filename ):
    _filename( filename ),
		_verbosity( MUTOO::NONE )
	{ read( filename ); }

	
	//! pointer to singleton
	static TMutParameterDB* _pointer;
		
	private:

	/*! 
		read pars from file
		parameter format is key::value. when "//" is encountered, rest of the line is skipped.
	*/
	void read( const std::string& filename );
	
		
	//! name of the file from which the singleton has been initialize
	std::string _filename;
	
	//! key, value map
	std::map< std::string, std::string > _pars;
	typedef std::map< std::string, std::string >::iterator par_iterator; 			//!< iterator over parameter map
	typedef std::map< std::string, std::string >::const_iterator const_par_iterator;	//!< constant iterator over parameter map
	
	//! returns iterator from the map, matching key, if any
	par_iterator _get_iterator( const std::string & );
	
	//! returns const iterator from the map, matching key, if any
	const_par_iterator _get_const_iterator( const std::string & ) const;

	//! verbosity
	MUTOO::Verbosity _verbosity;
	
	ClassDef(TMutParameterDB,0)
	
};

#endif
