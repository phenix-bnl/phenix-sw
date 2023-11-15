// $Id: PdbMutHVDisabled.cc,v 1.9 2009/08/19 17:07:22 pinkenbu Exp $

/*!
   \file PdbMutHVDisabled.cc
   \brief container for disabled HV channels in database
   \author J. Ying, X. He
   \version $Revision: 1.9 $
   \date $Date: 2009/08/19 17:07:22 $
*/

#include <PdbMutHVDisabled.hh>

#include <algorithm>
#include <iostream>
#include <set>

using namespace std;

//________________________________________________________
void PdbMutHVDisabled::print() const
{
  cout << "PdbMutHVDisabled::print - Run Number = " << runNumber << endl;
  cout << "PdbMutHVDisabled::print - Number of dead channels = " << getNumChnls() << endl;
  cout << "PdbMutHVDisabled::print - Dead channels: " << endl;
  for( vector<string>::const_iterator iter = CharString.begin(); iter != CharString.end(); iter++ )
  { cout << *iter << endl; }

}

//________________________________________________________
void PdbMutHVDisabled::addDeadHVCharString( const string& channel )
{
  
  // test if channel is already in vector, if not, adds it
  if( find_if( CharString.begin(), CharString.end(), SameChannelFTor( channel ) ) == CharString.end() )
  { CharString.push_back( channel ); }
  else { cout << "PdbMutHVDisabled::addDeadHVCharString - channel " << channel << " already disabled." << endl; }
  
}

//________________________________________________________
void PdbMutHVDisabled::removeDuplicates( void )
{
  
  set<string> tmp;
  for( vector<string>::const_iterator iter = CharString.begin(); iter != CharString.end(); iter++ )
  { tmp.insert( *iter );}
  
  CharString.clear();
  for( set<string>::const_iterator iter = tmp.begin(); iter != tmp.end(); iter++ )
  { CharString.push_back( *iter ); }
  
}
