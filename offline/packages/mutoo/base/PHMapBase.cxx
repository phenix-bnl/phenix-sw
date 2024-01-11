#include<PHMapBase.h>
#include<PHMapManager.h>
#include<PHGslRng.h>
#include<time.h>

//____________________________________________________
PHMapBase::PHMapBase()
{
  _key = get_gui();
}

//____________________________________________________
PHMapBase::PHMapBase(key_type key) :
_key(key)
{}

//____________________________________________________
void PHMapBase::change_map_key( key_type key )
{ PHMapManager::change_map_key( this, key ); }

//____________________________________________________
int PHMapBase::isValid() const
{ return 1; }

//____________________________________________________
PHMapBase::key_type PHMapBase::get_gui()
{
  static PHGslRng rng( time( NULL ) );
  return gsl_rng_get( rng.get() );
}
