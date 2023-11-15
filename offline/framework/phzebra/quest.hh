#ifndef __QUEST_HH__
#define __QUEST_HH__

#include "cfortran.h"

typedef struct {
  int iquest[100];
} QUEST_DEF;

#define Quest COMMON_BLOCK(QUEST,quest)
COMMON_BLOCK_DEF(QUEST_DEF,Quest);

#endif /* __QUEST_HH__ */
