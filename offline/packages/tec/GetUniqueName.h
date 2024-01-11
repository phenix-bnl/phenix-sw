#ifndef __GETUNIQUENAME_H__
#define __GETUNIQUENAME_H__

/**
 * GetUniqueName returns a name that is unique with regards to
 * ROOT objects. The returned name will be of the form
 * (baseName) (positive integer).
 * <p>
 * The caller is responsible for deleting the memory of the
 * returned C string.
 * 
 * @param baseName Valid prefix for a variable name, such as "hist".
 * @param baseNumber Starting point for suffix; the more unique, the better.
 * @return A name unique in ROOT.
 */
char* GetUniqueName(const char* baseName, int baseNumber = 0);

#endif
