// $Id: PHMapManager.cxx,v 1.12 2013/01/18 03:55:15 phnxbld Exp $

/*!
   \file PHMapManager.cxx
   \brief static storage of registered PHMap
   \author Sean Kelly, Hugo Pereira
   \version $Revision: 1.12 $
   \date $Date: 2013/01/18 03:55:15 $
*/

#include<PHMapManager.h>
#include<PHMapBase.h>

using namespace std;

bool PHMapManager::_disable_clear=false;
MUTOO::Verbosity PHMapManager::_verbosity = MUTOO::NONE;

//_______________________________________________
boost::any PHMapManager::get(key_type key)
{

  PrivateMap::iterator iter = map().find(key);
  return (iter == map().end()) ? boost::any(): iter->second;

}

//________________________________
PHMapManager::PrivateMap& PHMapManager::map()
{
  static PrivateMap* lcl = new PrivateMap();
  return *lcl;
}

//________________________________
PHMapManager::PrivateSet& PHMapManager::set()
{
  static PrivateSet* lcl = new PrivateSet();
  return *lcl;
}

//________________________________
void PHMapManager::write()
{
  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "PHMapManager::write" );

  private_set_iterator iter = set().begin();
  for(;iter!=set().end();++iter)
  if((*iter)->is_writeable())
  {

    if( _verbosity >= MUTOO::SOME )
    cout << "writing " << (*iter)->get_name() << endl;
    (*iter)->write_array();

  }

  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "**" );

}

//_______________________________________________________
void PHMapManager::read(PHCompositeNode* dst_node)
{
  if( _verbosity >= MUTOO::SOME ) MUTOO::PRINT( cout, "PHMapManager::read" );

  private_set_iterator iter = set().begin();
  for(;iter!=set().end();++iter)
  if((*iter)->is_readable()){
    if( _verbosity >= MUTOO::SOME ) cout << "reading " << (*iter)->get_name() << endl;
    (*iter)->read_array(dst_node);
  }

  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "**" );

}

//_________________________________________________________
void PHMapManager::print_array(ostream& os)
{
  private_set_iterator iter = set().begin();
  for(;iter!=set().end();++iter)
  {
    if((*iter)->is_writeable())
    { (*iter)->print_array(os); }
  }
}

//________________________________
void PHMapManager::clear_arrays()
{

  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "PHMapManager::clear_arrays" );

  private_set_iterator iter = set().begin();
  for(;iter!=set().end();++iter)
  if((*iter)->is_writeable())
  {
    if((*iter)->is_writeable())
    { (*iter)->clear_array(); }

  }

  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "**" );

}

//_________________________________________________________
void PHMapManager::clear()
{
  if(_disable_clear)
  {
    MUTOO::TRACE( "PHMapManager::clear - disable_clear is true. Request denied" );
    return;
  }

  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "PHMapManager::clear" );

  private_set_iterator iter = set().begin();
  for(;iter!=set().end();++iter)
  {

    if( _verbosity >= MUTOO::SOME )
    { cout << "clearing " << (*iter)->get_name() << endl; }

    (*iter)->update_statistics();
    (*iter)->clear();

  }

  if( _verbosity >= MUTOO::SOME )
  MUTOO::PRINT( cout, "**" );

}

//_________________________________________________________
void PHMapManager::print_stat( ostream& os )
{

  // do nothing if empty
  if( set().empty() ) return;

  MUTOO::PRINT( os, "PHMap statistics" );
  for( private_set_iterator iter = set().begin();iter!=set().end();++iter)
  {
    if( (*iter)->get_ncycle() )
    {
      os <<  "size of " << (*iter)->get_name()
	 <<  "- accumulated: " << (*iter)->get_accumulated_size()
	 << ", per cycle " << double( (*iter)->get_accumulated_size() )/(*iter)->get_ncycle()
	 << endl;
    }
  }
  MUTOO::PRINT( os, "**" );

}
