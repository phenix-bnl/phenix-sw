#ifndef __SVXCOMMON_H__
#define __SVXCOMMON_H__

#include <vector>
#include <string>

/**
 * @brief  Variables and functions common to the Svx code.
 *
 * Only for test or debug.  Do not abuse this.
 * Please replace with a better method if exists.
 */
namespace SvxCommon {
   static std::vector<double>      rusage_sec;
   static std::vector<std::string> rusage_label;
   static std::vector<int>         rusage_level;
   void ClearRUsage(); ///< Clear the history of rusage
   void AddRUsage(const char* label, const int level=0); ///< Add a rusage entry
   void PrintRUsage(const int level_min=0); ///< Print the history of rusage
};

#endif // __SVXCOMMON_H__
