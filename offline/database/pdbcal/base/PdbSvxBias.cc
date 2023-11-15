#include <PdbSvxBias.hh>
#include <phool.h>
#include <iomanip>

using namespace std;

PdbSvxBias::PdbSvxBias()
{
  reset();
}

int PdbSvxBias::set(const std::string& channel,const int timestamp,const int flag)
{
  badlist[channel]=pair<int,int>(timestamp,flag);
  return 0;
}

PdbSvxBias& PdbSvxBias::operator=(const PdbSvxBias& p)
{
  this->badlist=p.badlist;
  return *this;
}

void PdbSvxBias::reset()
{
  badlist.clear();
}

void PdbSvxBias::print() const
{
  cout << "List of channels marked as bad:\n";
  cout << "channel\ttimestamp\tbitflag\n";
  for(std::map<std::string,std::pair<int,int> >::const_iterator ii=badlist.begin(); ii!=badlist.end(); ++ii)
    {
      cout << (*ii).first << "\t" << (*ii).second.first << "\t" << (*ii).second.second << endl;
    }
}

