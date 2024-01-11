#ifndef __PHEMCCONSTANTS_H__
#define __PHEMCCONSTANTS_H__

enum EMCTYPE { PBSC = 0, PBGL, NUMEMCTYPES };
enum EMCARM { EMCWEST = 0, EMCEAST, EMCEITHERARM };
const int EmcHwChannel[] = { 0, 2592, 5184, 7776, 10368, 12960, 15552, 20160 };
const int EmcZtowers[] = { 72, 72, 72, 72, 72, 72, 96, 96 };
const int EmcYtowers[] = { 36, 36, 36, 36, 36, 36, 48, 48 };

#endif	// __PHEMCCONSTANTS_H__
