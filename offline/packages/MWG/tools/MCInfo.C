// $Id: MCInfo.C,v 1.1 2007/10/02 19:39:32 mwysocki Exp $
#include "Tools.h"
#include <headerWrapper.h>

/*!
	\file MCinfo.C
	\brief retrieve retrieve header information from headerWrapper node
	\author	Hugo Pereira
	\version $Revision: 1.1 $
	\date $Date: 2007/10/02 19:39:32 $
*/

#include <iostream>
using namespace std;

//________________________________________________
int Tools::runNumberMC( headerWrapper* header, bool& error )
{
	 
	error = false;
	
	if( !header ) 
	{
		error = true;
		return 0;
	}

	// retrieve staff table
	HEADER_ST* header_table( header->TableData() );
	if( !header_table ) 
	{
		error = true;
		return 0;
	}
	
	return header_table[0].run;
		
}

//________________________________________________
long int Tools::eventNumberMC( headerWrapper* header, bool& error )
{
	 
	error = false;
	
	if( !header ) 
	{
		error = true;
		return 0;
	}

	// retrieve staff table
	HEADER_ST* header_table( header->TableData() );
	if( !header_table ) 
	{
		error = true;
		return 0;
	}
	
	return header_table[0].event;
		
}

//________________________________________________
double Tools::zVertexMC( headerWrapper* header, bool& error )
{
	 
	error = false;
	
	if( !header ) 
	{
		error = true;
		return -9999;
	}

	// retrieve staff table
	HEADER_ST* header_table( header->TableData() );
	if( !header_table ) 
	{
		error = true;
		return -9999;
	}
	
	return header_table[0].vertex[2];
	
}
