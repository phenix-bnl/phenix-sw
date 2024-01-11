#ifndef __DIRFILEMANIP_H__
#define __DIRFILEMANIP_H__

#include <string>

// Create a directory
bool createDirectory(const std::string& dir);

// Check if file exists.
bool checkFile(const std::string& file);

// Expand env. variable in dir name.
std::string expand(const std::string& dir);

#endif
