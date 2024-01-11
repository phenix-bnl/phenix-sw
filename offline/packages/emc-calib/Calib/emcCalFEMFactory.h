#ifndef __EMCCALFEMFACTORY_H__
#define __EMCCALFEMFACTORY_H__

class emcCalFEM;
#include "PHTimeStamp.h"
#include <iostream>
#include <string>
#include <map>

/** (Factory) Build emcCalFEM objects. 
 *
 *  Builds emcCalFEM-type objects, either
 *  void ones using Create or default (i.e. filled) ones using CreateDefault.
 * @ingroup calibration
 */

class emcCalFEMFactory
{
public:

  typedef emcCalFEM* (*Creator)(int absolutePosition,
				const PHTimeStamp& start,
				const PHTimeStamp& end,
				bool isDefault);
  typedef std::map<std::string,Creator> CreatorMap;

  /// Creates an object containing no channels.
  static emcCalFEM*
  Create(const char* category, 
	 int absPosition,
	 const PHTimeStamp& start=PHTimeStamp(), 
	 const PHTimeStamp& end=PHTimeStamp(PHTimeStamp::PHFarFuture));

  //  /// Same w/o validity period.
  //  static emcCalFEM* Create(const char* category,
  //			   int absPosition);

  /// Creates an object containing 144 channels with default values.
  static emcCalFEM* 
  CreateDefault(const char* category, 
		int absPosition, 
		const PHTimeStamp& start=PHTimeStamp(), 
		const PHTimeStamp& end=PHTimeStamp(PHTimeStamp::PHFarFuture));

  /// Show list of supported objects.
  static void print(std::ostream& out = std::cout);

  /// Same w/o validity period.
  //  static emcCalFEM* CreateDefault(const char* category, 
  //                                  int absPosition);

  static bool registerCreator(const char* category,
			      Creator creator);
private:

  static emcCalFEM* create(const char* category,
			   int absPosition,
			   const PHTimeStamp& start,
			   const PHTimeStamp& end,
			   bool isDefault);

  static CreatorMap& fCreators();
};

#endif
