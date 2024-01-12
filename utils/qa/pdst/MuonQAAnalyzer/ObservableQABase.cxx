#include "ObservableQABase.h"

#include<boost/foreach.hpp>

void ObservableQABase::setBounds(double lb, double ub)
{
	_validityBounds.insert(boost::make_tuple(FIRST_RUN,LAST_RUN,lb,ub));
}
void ObservableQABase::setBounds(int firstRun, int lastRun, double lb, double ub)
{
	_validityBounds.insert(boost::make_tuple(firstRun,lastRun,lb,ub));
}
void ObservableQABase::setBoundsFrom(int firstRun, double lb, double ub)
{
	_validityBounds.insert(boost::make_tuple(firstRun,LAST_RUN,lb,ub));
}
void ObservableQABase::setBoundsTo(int lastRun,  double lb, double ub)
{
	_validityBounds.insert(boost::make_tuple(FIRST_RUN,lastRun,lb,ub));
}

std::set<int> ObservableQABase::getGoodRunList()
{
	std::set<int> GoodRunList;
	for(int ibin=1;ibin<=_summary->GetXaxis()->GetNbins();ibin++)
	{
		int irun = static_cast<int> (_summary->GetXaxis()->GetBinCenter(ibin));
		double bin_content = static_cast<double> (_summary->GetBinContent(ibin));
		typedef boost::tuple<int, int, double, double> Tuple;

		bool is_good = true;
		BOOST_FOREACH(Tuple val_bound,_validityBounds)
		{
			if(
					irun>val_bound.get<0>()
					&&irun<val_bound.get<1>()
					&&
					!(bin_content>val_bound.get<2>()&&bin_content<val_bound.get<3>())
				)
				is_good = false;
		}
		if(is_good)
			GoodRunList.insert(irun);
	}
	return GoodRunList;
}

void ObservableQABase::process(TH1* histo){;}
void ObservableQABase::createSummary(){;}
