//    chp: use printf to write to stdout, cout is redirected by the
//         message system in 1008 and leads to lots of useless blabbering
//         for the monitoring shift crew

#include <TrigRunLvl1.h>

#include <phool.h>

#include <iostream>
#include <sstream>

ClassImp(TrigRunLvl1)

using namespace std;

static int shutup = 0;

TrigRunLvl1*
TrigRunLvl1::clone() const
{
  cout << PHWHERE << " is not implemented in daughter class. Sorry"
       << endl;
  return NULL;
}

void 
TrigRunLvl1::Copy(TrigRunLvl1 * in)
{

  ostringstream d1;
  d1 << in->get_lvl1_trigger_description();
  set_lvl1_trigger_description(d1.str().c_str());

  set_lvl1_trigger_version(in->get_lvl1_trigger_version());

  ostringstream d2;
  d2 << in->get_lvl1_bbcll1_description();
  set_lvl1_bbcll1_description(d2.str().c_str());

  set_lvl1_bbcll1_version( in->get_lvl1_bbcll1_version());

  ostringstream d3;
  d3 << in->get_lvl1_partition_name();
  set_lvl1_partition_name(d3.str().c_str());

  for (int ibit = 0; ibit < 32;ibit++)
    {
      ostringstream d4;
      d4 << in->get_lvl1_trig_name(ibit);
      set_lvl1_trig_name(d4.str().c_str(), ibit);

      ostringstream d5;
      d5 << in->get_lvl1_rbit_name(ibit) << endl;
      set_lvl1_rbit_name(d5.str().c_str(), ibit);

      set_lvl1_trigger_enable(in->get_lvl1_trigger_enable(ibit), ibit);
      set_lvl1_trig_bit(in->get_lvl1_trig_bit(ibit), ibit);
      set_lvl1_trig_scale_down(in->get_lvl1_trig_scale_down(ibit), ibit);
      set_lvl1_lvl2_reject_enable(in->get_lvl1_lvl2_reject_enable(ibit), ibit);
    }
}

int
TrigRunLvl1::isValid() const
{
  cout << PHWHERE << ": isValid() not implemented" << endl;
  return 0;
}

void
TrigRunLvl1::identify(ostream& os) const
{
  os << "identify yourself: virtual TrigRunLvl1 object" << endl;
  return ;
}

void
TrigRunLvl1::Reset()
{
  cout << PHWHERE << ": ERROR Reset() not implemented" << endl;
  return ;
}

void
TrigRunLvl1::warning(const char *funcname) const
{
  if (!shutup)
    {
      printf("TrigRunLvl1: %s is virtual\n",funcname); 
    }
  return;
}

const char *
TrigRunLvl1::get_lvl1_trigger_description() const
{
  warning("get_lvl1_trigger_description()");
  return NULL;
}

void
TrigRunLvl1::set_lvl1_trigger_description(const char* /*name*/)
{
  warning("set_lvl1_trigger_description(const char *name)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_trigger_version() const
{
  warning("get_lvl1_trigger_version()");
  return 0;
}

void
TrigRunLvl1::set_lvl1_trigger_version(const unsigned int /*ival*/)
{
  warning("set_lvl1_trigger_version(const unsigned int ival)");
  return;
}

const char *
TrigRunLvl1::get_lvl1_bbcll1_description() const
{
  warning("get_lvl1_bbcll1_description()");
  return NULL;
}

void
TrigRunLvl1::set_lvl1_bbcll1_description(const char* /*name*/)
{
  warning("set_lvl1_bbcll1_description(const char *name)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_bbcll1_version() const
{
  warning("get_lvl1_bbcll1_version() const");
  return 0;
}

void
TrigRunLvl1::set_lvl1_bbcll1_version(const unsigned int /*ival*/)
{
  warning("set_lvl1_bbcll1_version(const unsigned int ival)");
  return;
}

const char *
TrigRunLvl1::get_lvl1_partition_name() const
{
  warning("get_lvl1_partition_name() const");
  return NULL;
}

void
TrigRunLvl1::set_lvl1_partition_name(const char* /*name*/)
{
  warning("set_lvl1_partition_name(const char *name)");
  return;
}

const char *
TrigRunLvl1::get_lvl1_trig_name(const unsigned int /*i*/) const
{
  warning("get_lvl1_trig_name(const unsigned int i) const");
  return NULL;
}

const char *
TrigRunLvl1::get_lvl1_trig_name_bybit(const unsigned int /*bit*/) const
{
  warning("get_lvl1_trig_name_bybit(const unsigned int bit) const");
  return NULL;
}

void
TrigRunLvl1::set_lvl1_trig_name(const char* /*name*/,const unsigned int /*i*/)
{
  warning("set_lvl1_trig_name(const char *name,const unsigned int i)");
  return;
}

const char *
TrigRunLvl1::get_lvl1_rbit_name(const unsigned int /*i*/) const
{
  warning("get_lvl1_rbit_name(const unsigned int i) const");
  return NULL;
}

void
TrigRunLvl1::set_lvl1_rbit_name(const char* /*name*/,const unsigned int /*i*/)
{
  warning("set_lvl1_rbit_name(const char *name,const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_trig_bitmask(const unsigned int /*i*/) const
{
  warning("get_lvl1_trig_bitmask(const unsigned int i) const");
  return 0;
}

void
TrigRunLvl1::set_lvl1_trig_bitmask(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl1_trig_bitmask(const unsigned int ival, const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_trigger_enable(const unsigned int /*i*/) const
{
  warning("get_lvl1_trigger_enable(const unsigned int i) const");
  return 0;
}

unsigned int
TrigRunLvl1::get_lvl1_trigger_enable_bybit(const unsigned int /*bit*/) const
{
  warning("get_lvl1_trigger_enable_bybit(const unsigned int bit) const");
  return 0;
}

void
TrigRunLvl1::set_lvl1_trigger_enable(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl1_trigger_enable(const unsigned int ival, const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_trig_bit(const unsigned int /*i*/) const
{
  warning("get_lvl1_trig_bit(const unsigned int i) const");
  return 0;
}

unsigned int
TrigRunLvl1::get_lvl1_trig_index(const unsigned int /*bit*/) const
{
  warning("get_lvl1_trig_index(const unsigned int bit) const");
  return 0;
}

void
TrigRunLvl1::set_lvl1_trig_bit(const unsigned int /*ival*/,const unsigned int /*i*/)
{
  warning("set_lvl1_trig_bit(const unsigned int ival,const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_trig_scale_down(const unsigned int /*i*/) const
{
  warning("get_lvl1_trig_scale_down(const unsigned int i) const");
  return 0;
}

unsigned int
TrigRunLvl1::get_lvl1_trig_scale_down_bybit(const unsigned int /*bit*/) const
{
  warning("get_lvl1_trig_scale_down_bybit(const unsigned int bit) const");
  return 0;
}

void
TrigRunLvl1::set_lvl1_trig_scale_down(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl1_trig_scale_down(const unsigned int ival,const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl1::get_lvl1_lvl2_reject_enable(const unsigned int /*i*/) const
{
  warning("get_lvl1_lvl2_reject_enable(const unsigned int i) const");
  return 0;
}

unsigned int
TrigRunLvl1::get_lvl1_lvl2_reject_enable_bybit(const unsigned int /*bit*/) const
{
  warning("get_lvl1_lvl2_reject_enable_bybit(const unsigned int bit) const");
  return 0;
}

void
TrigRunLvl1::set_lvl1_lvl2_reject_enable(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i) ");
  return;
}

float
TrigRunLvl1::get_lvl1_trig_rate_begin(const unsigned int /*i*/) const
{
  warning("get_lvl1_trig_rate_begin(const unsigned int i) const");
  return 0.;
}

void
TrigRunLvl1::set_lvl1_trig_rate_begin(float /*val*/, const unsigned int /*i*/)
{
  warning("set_lvl1_trig_rate_begin(float val,const unsigned int i)");
  return;
}

void
TrigRunLvl1::dump_info(const TrigRunLvl1 *runlevel1) const
{
  printf("RUN LEVEL1 TRIGGER OBJECT INFORMATION\n");
  printf("=====================================\n\n");
  printf("  Trigger Description       = %s\n",runlevel1->get_lvl1_trigger_description());
  printf("  Trigger Version           = %u\n",runlevel1->get_lvl1_trigger_version());
  printf("  Partition Name            = %s\n",runlevel1->get_lvl1_partition_name());
  printf("  BBCLL1  Description       = %s\n",runlevel1->get_lvl1_bbcll1_description());
  printf("  BBCLL1  Version           = %u\n\n",runlevel1->get_lvl1_bbcll1_version());

  for (int i = 0; i < 32; i++)
    {
      printf("    Lvl1 Name               = %s\n",runlevel1->get_lvl1_trig_name(i));
      printf("      Index=%d Bit=%u",i,runlevel1->get_lvl1_trig_bit(i));
      printf(" Enable=%u",runlevel1->get_lvl1_trigger_enable(i));
      printf(" ScaleDown=%u\n",runlevel1->get_lvl1_trig_scale_down(i));
    }
  return;
}

int
TrigRunLvl1::get_run_number() const
{
  warning("get_run_number() const");
  return 0;
}

void
TrigRunLvl1::set_run_number(const int /*runno*/)
{
  warning("set_run_number(const int runno)");
  return;
}

time_t
TrigRunLvl1::get_start_time() const
{
  warning("get_start_time() const");
  return 0;
}

void
TrigRunLvl1::set_start_time(const time_t /*time*/)
{
  warning("set_start_time(const time_t time)");
  return;
}

void
TrigRunLvl1::ShutUp(const int i)
{
  shutup = i;
  return;
}

