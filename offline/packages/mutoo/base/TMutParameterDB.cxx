// $Id: TMutParameterDB.cxx,v 1.1 2006/04/22 01:53:02 hpereira Exp $
/*!
   \file    TMutParameterDB.cxx
   \brief   mutoo runtime parameter par manager
   \author  Hugo Pereira
   \version $Revision: 1.1 $
   \date    $Date: 2006/04/22 01:53:02 $
*/
#include "MUTOO.h"
#include "PHException.h"
#include "TMutParameterDB.h"
#include <fstream>
#include <stdexcept>

using namespace std;

//_______________________________________
ClassImp( TMutParameterDB );
TMutParameterDB* TMutParameterDB::_pointer = 0;
 
//_______________________________________
void TMutParameterDB::read( const string& filename )
{	
	static const string delimitor("::");
	
	// open, check input file
	ifstream in( filename.c_str() );
	if( !in ) {
		if( _verbosity >= MUTOO::SOME )
		cerr << "in TMutParameterDB::read - Using default/run_time parameters.\n";
		return;
	} 
    
	
	MUTOO::PRINT( cout, "TMutParameterDB");
	cout << "reading parameters from " << filename << endl;
	
	// read in
	char readline[1024];
	while ((in.rdstate() & ios::failbit) == 0 ) {
	
		// Read into the raw char array and then construct a string
		// to do the searching
		in.getline(readline, 1024);
		istringstream s(readline);		
		
		while ( ( s.rdstate() & ios::failbit ) == 0 ) {
			
			string key_value; 
			s >> key_value;
			
			// check stream status
			if( s.rdstate() & ios::failbit ) break;
			
			// skipp rest of line if comments found
			if( key_value.substr( 0, 2 ) == "//" ) break;
			
			// look for "::" in key_value pair
			size_t position = key_value.find( delimitor );
			if( position == string::npos ) {
        ostringstream what; what << "wrong format for key::value pair: " << key_value;
        cerr << what.str() << endl;
				throw  logic_error( DESCRIPTION( what.str() ) ); 
			}
				
			// split key_value pair
			string key( key_value.substr( 0, position ) );
			string value( key_value.substr( position+delimitor.size(), key_value.size()-delimitor.size() ) );
			
			// check value does not contain a new delimitor
			if( value.find( delimitor ) != string::npos ) {
        ostringstream what; what << "wrong format for key::value pair: " << key_value;
        cerr << what.str() << endl;
				throw  logic_error( DESCRIPTION( what.str() ) ); 
			}
						
			// add key value pair
			add_raw( key, value );
		}		
	}
	
	MUTOO::PRINT( cout, "**");

	return;
	
}

//_______________________________________
void TMutParameterDB::print( ostream& out ) const
{
	MUTOO::PRINT( out, "TMutParameterDB::print" );
	out << "filename: " << _filename << endl;
	for( const_par_iterator par = _pars.begin(); par != _pars.end(); par++ ) 
	out << par->first << "::" << par->second << "\n";
	MUTOO::PRINT( out, "**" );
}	

//__________________________________________________________
TMutParameterDB::par_iterator TMutParameterDB::_get_iterator( const string& key )
{ 
	par_iterator par = _pars.find( key );
	return par;
}

//__________________________________________________________
TMutParameterDB::const_par_iterator TMutParameterDB::_get_const_iterator( const string& key ) const
{ 
	const_par_iterator par = _pars.find( key );
	return par;
}

//__________________________________________________________
void TMutParameterDB::add_raw( const string& key, const string& value )
{
	par_iterator par = _get_iterator( key );
	if( par != _pars.end() ) _pars.erase( par );
	_pars.insert(  pair< string, string >( key, value ) );
}
