#ifndef __MUONQAANALYZER_UTILITY_QA_H__
#define __MUONQAANALYZER_UTILITY_QA_H__

#include<algorithm>
#include<functional>

#include<boost/bind.hpp>
#include<boost/function.hpp>

#include<HistoQABase.h>
#include<SubsystemQA.h>

using boost::bind;

namespace { // Unnamed namespace
  template<typename MapType,
           typename KeyArgType,
           typename ValueArgtype>
  typename MapType::iterator
  addOrIncrement(MapType& m,
                 const KeyArgType& k,
                 const ValueArgtype& v)
  {
    typename MapType::iterator lb =
      m.lower_bound(k);

    if(lb != m.end() &&
       !(m.key_comp()(k, lb->first))) 
      {
        lb->second += v;
        return lb;
      }
    else
      {
        typedef typename MapType::value_type MVT;
        return m.insert(lb, MVT(k, v));
      }
  }

  template<typename MapType,
           typename KeyArgType,
           typename ValueArgtype>
  typename MapType::iterator
  addOrUpdate(MapType& m,
              const KeyArgType& k,
              const ValueArgtype& v)
  {
    typename MapType::iterator lb =
      m.lower_bound(k);

    if(lb != m.end() &&
       !(m.key_comp()(k, lb->first))) 
      {
        lb->second = v;
        return lb;
      }
    else
      {
        typedef typename MapType::value_type MVT;
        return m.insert(lb, MVT(k, v));
      }
  }

  template<typename MapType>
  void normByEvents(const HistoQABase::ptr& h,
                    MapType& m)
  {
    normByFunction(h, m, &SubsystemQA::getEventsInRun);
  }

  template<typename MapType>
  void normBySegments(const HistoQABase::ptr& h,
                    MapType& m)
  {
    normByFunction(h, m, &SubsystemQA::getNumSegmentsInRun);
  }

  template<typename MapType,
           typename FuncType>
  void normByFunction(const HistoQABase::ptr& h,
                      MapType& m,
                      const FuncType& mf)
  {
    typedef typename MapType::iterator::value_type MVT;
    MapType tmpMap;
    
    std::transform(m.begin(),
                   m.end(),
                   std::inserter(tmpMap,tmpMap.begin()),         
                   bind(&std::make_pair<typename MapType::key_type,
                        typename MapType::mapped_type>,
                        bind(&MVT::first,_1),
                        bind(std::divides<double>(),                             
                             bind(&MVT::second,_1),                             
                             bind(mf,
                                  bind(&HistoQABase::getParent,h),
                                  bind(&MVT::first,_1)
                                  )
                             )
                        )
                   );
    std::swap(tmpMap, m);    
  }
}

#endif // __MUONQAANALYZER_UTILITY_QA_H__
