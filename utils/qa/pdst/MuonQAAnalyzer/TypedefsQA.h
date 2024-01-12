#ifndef TYPEDEFS_QA_H
#define TYPEDEFS_QA_H

#ifndef __CINT__
#include<boost/shared_ptr.hpp>
#endif /* __CINT__ */

class MuonQAAnalyzer;
class SubsystemQA;
class HistoQA;
class ObservableQABase;

#ifndef __CINT__
typedef boost::shared_ptr<SubsystemQA> SubsysPtr;
typedef boost::shared_ptr<HistoQA> HistoPtr;
typedef boost::shared_ptr<ObservableQABase> ObservablePtr;
#else
class SubsysPtr;
class HistoPtr;
class ObservablePtr;
#endif /* __CINT__ */

#endif /* TYPEDEFS_QA_H
