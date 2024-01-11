#include "emcCalFEMFactory.h"
#include "emcDefines.h"

#include <string>
#include <iostream>

//_____________________________________________________________________________
emcCalFEMFactory::CreatorMap&
emcCalFEMFactory::fCreators()
{
  static CreatorMap map_;
  return map_;
}

//_____________________________________________________________________________
emcCalFEM*
emcCalFEMFactory::Create(const char* category, int absPosition,
                         const PHTimeStamp& start, const PHTimeStamp& end)
{
  return create(category,absPosition,start,end,false);
}

//_____________________________________________________________________________
emcCalFEM*
emcCalFEMFactory::create(const char* category,
			 int absPosition,
			 const PHTimeStamp& start,
			 const PHTimeStamp& end,
			 bool isDefault)
{
  CreatorMap::iterator it = fCreators().find(category);
  if ( it != fCreators().end() )
    {
      return (it->second)(absPosition,start,end,isDefault);
    }
  else
    {
      std::cerr << "emcCalFEMFactory::create : I do not know this flavour :"
		<< category << std::endl;
      return 0;
    }
}

//_____________________________________________________________________________
emcCalFEM* emcCalFEMFactory::CreateDefault(const char* category,
					   int absPosition,
					   const PHTimeStamp& start,
					   const PHTimeStamp& end)
{
  return create(category,absPosition,start,end,true);
}

//_____________________________________________________________________________
void
emcCalFEMFactory::print(std::ostream& os)
{
  CreatorMap::iterator it;
  os << "emcCalFEMFactory: I know the following flavors:" << std::endl;
  for ( it = fCreators().begin(); it != fCreators().end(); ++it ) 
    {
      os << it->first << std::endl;
    }
}

//_____________________________________________________________________________
bool
emcCalFEMFactory::registerCreator(const char* category,
				  emcCalFEMFactory::Creator creator)
{
  return fCreators().insert(CreatorMap::value_type(category,creator)).second;
}
