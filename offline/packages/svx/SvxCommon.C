#include <iostream>
#include <iomanip>
#include <sys/time.h>
#include <sys/resource.h>

#include "SvxCommon.h"

using namespace std;

/**
 * Clear the history of rusage.
 */
void SvxCommon::ClearRUsage()
{
   rusage_sec  .clear();
   rusage_label.clear();
   rusage_level.clear();
}

/**
 * Add a rusage entry.
 * This calls the 'getrusage()' and store the CPU time into 'rusage_sec'.
 * @param[in] label  a label of this entry.
 * @param[in] level  a level of this entry.  see 'PrintRUsage()'.
 */
void SvxCommon::AddRUsage(const char* label, const int level)
{
   struct rusage t;
   getrusage(RUSAGE_SELF, &t);
   rusage_sec  .push_back(t.ru_utime.tv_sec + t.ru_utime.tv_usec * 1e-6);
   rusage_label.push_back(label);
   rusage_level.push_back(level);
};

/**
 * Print the history of rusage.
 * For each entry, the CPU time spent from a previous entry, which has to have a same or larger level, is calculated and shown.
 * @param[in] level_min  entries with a level below this value are not shown.
 */
void SvxCommon::PrintRUsage(const int level_min)
{
  cout << "##\n"
     "## SvxCommon::PrintRUsage()\n"
     "## CPU time usage (in sec unit)\n"
     "##\n";
  for (unsigned int ient = 0; ient < rusage_sec.size(); ient++) {
     int level = rusage_level[ient];
     if (level < level_min) continue;

     cout << setw(3) << ient << ") " << setw(35) 
          << rusage_label[ient] << " | " << rusage_sec[ient] << " | ";
     for (int j = ient - 1; j >= 0; j--) {
        if (rusage_level[j] >= level) {
           cout << "   " << rusage_sec[ient] - rusage_sec[j] 
                << " from " << j;
           break;
        }
     }
     cout << endl;
  }
  cout << endl;
}
