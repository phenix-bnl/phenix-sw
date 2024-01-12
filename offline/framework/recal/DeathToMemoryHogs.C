#include <DeathToMemoryHogs.h>
#include <Fun4AllReturnCodes.h>
#include <Fun4AllServer.h>
#include <PHCompositeNode.h>

#include <TH1.h>

#include <cstdlib>
#include <fstream>
#include <sstream>
#include <unistd.h>

unsigned int channel_alloc = 5000;

using namespace std;

DeathToMemoryHogs::DeathToMemoryHogs(const string &name): SubsysReco(name)
{
  pid = 0;
  lastcheck = 0;
  evtcnt = 0;
  nChannels = 0;
  currchan = 0;
  array = 0;
  maxmem = 0;
  savehisto = 0;
  memlimit();
  check_frequency();
  event_frequency();
  exitcode();
  return ;
}

DeathToMemoryHogs::~DeathToMemoryHogs()
{
  delete [] array;
  return;
}

int DeathToMemoryHogs::Init(PHCompositeNode *topNode)
{
  array = new unsigned int[channel_alloc];
  memset(array, 0, (channel_alloc)*sizeof(unsigned int));
  nChannels += channel_alloc;
  pid = getpid();
  ostringstream fs;
  fs << "/proc/" << pid << "/status";
  procfilename = fs.str();
  return EVENT_OK;
}

int
DeathToMemoryHogs::process_event(PHCompositeNode *topNode)
{
  if (evtfreq > 0)
    {
      if ((evtcnt % evtfreq) == 0)
        {
          unsigned int currmem = getmemsize();
          Addcurrmem(currmem);
          if (currmem > maxmem)
            {
              maxmem = currmem;
            }
        }
      evtcnt++;
    }
  else
    {
      time_t currtime = time(0);
      if ((currtime - lastcheck) > deltat)
        {
          lastcheck = currtime;
          unsigned int currmem = getmemsize();
          Addcurrmem(currmem);
          if (currmem > maxmem)
            {
              maxmem = currmem;
            }
          if (currmem > memlim)
            {
              cout << "DeathToMemoryHogs: Memory Limit of " << memlim << " kB exceeded"
                   << " current use: " << currmem << "kB" <<  endl;
              exit(iexit);
            }
        }
    }
  //  int iret = 0;
  return EVENT_OK;
}

int
DeathToMemoryHogs::End(PHCompositeNode *topNode)
{
  if (verbosity > 0)
    {
      cout << "DeathToMemoryHogs: maximum memory used " << maxmem << " kB" << endl;
    }
  if (savehisto)
    {
      TH1 *h1 = new TH1I("memuse", "Memory Use (kB)", currchan, 0, currchan);
      for (unsigned int i = 0; i < currchan; i++)
        {
          h1->SetBinContent(i + 1, array[i]);
        }
      Fun4AllServer *se = Fun4AllServer::instance();
      se->registerHisto(h1);
    }
  return EVENT_OK;
}

unsigned int
DeathToMemoryHogs::getmemsize()
{
  ifstream procfile(procfilename.c_str(), ifstream::in);
  string line;
  unsigned int memsize = 0;
  while (procfile >> line)
    {
      if (line.find("VmRSS") != string::npos)
        {
          procfile >> memsize;
          break;
        }
    }
  procfile.close();
  if (verbosity > 1)
    {
      cout << PHWHERE << " current memory footprint " << memsize << " kB" << endl;
    }
  return memsize;
}

void
DeathToMemoryHogs::Addcurrmem(unsigned int currmem)
{
  if (currchan < nChannels + 1)
    {
      array[currchan] = currmem;
      currchan++;
    }
  else
    {
      unsigned int *tmparray = array;
      array = new unsigned int[nChannels+channel_alloc];
      memset(array, 0, (nChannels + channel_alloc)*sizeof(unsigned int));
      for (unsigned int i = 0; i < nChannels; i++)
        {
          array[i] = tmparray[i];
        }
      currchan = nChannels;
      nChannels += channel_alloc;
      delete [] tmparray;
      Addcurrmem(currmem);
    }
  return;
}
