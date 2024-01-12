#ifndef MUONQA_FLAGS_H
#define MUONQA_FLAGS_H

namespace MuonQA {

enum ObsType
{
  ENTRIES = 1 << 0,
  MEAN = 1 << 1,
  PEAK = 1 << 2,
  ACTIVEWEDGE = 1 << 3,
  INVALID_TYPE = 1 << 4
};

enum SummaryFlag
{
  INDIVIDUAL = 1 << 0,
  OVERALL = 1 << 1,
  SUM = 1 << 2,
  AVERAGE = 1 << 3,
  INVALID_FLAG = 1 << 4
};

}

#endif /* MUONQA_FLAGS_H */
