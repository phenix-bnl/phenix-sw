#ifndef __utils_h__
#define __utils_h__

class TH1;
class TDirectory;
class dEmcGeaTrackWrapper;

#include <string>
#include <vector>

bool PathIsInDir(const std::string& path, TDirectory* dir);
TH1* GetHisto(TDirectory* dir, const std::string& histoname, 
	      const std::string& where);
void splitPath(const std::string& path,
	       std::vector<std::string>& paths);

bool mkpath(TDirectory* dir, const std::string& path);

bool mkpath(TDirectory* dir, const std::string& path, const std::string& title );

void dump(dEmcGeaTrackWrapper* d);


#endif
