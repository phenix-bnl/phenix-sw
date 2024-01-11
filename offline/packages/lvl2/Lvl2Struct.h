#ifndef __LVL2STRUCT_H__
#define __LVL2STRUCT_H__
typedef unsigned long ulong;

enum Lvl2OperationMode {EvBLvl2disabled, EvBLvl2enabledNoReject, EvBLvl2enabledReject};

struct TrigAlgo {
  std::string name;
  ulong scaledown;
  Lvl2OperationMode algoInitialMode;
  std::string lvl1BitName;
  ulong bitNumber;
  ulong partition;
  ulong forcedAcceptN;
  Lvl2OperationMode bitLvl2Mode;
};

struct Lvl2Algo {
  std::string algoName;
  ulong algoIndex;
};

#endif // __LVL2STRUCT_H__









