//    chp: use printf to write to stdout, cout is redirected by the
//         message system in 1008 and leads to lots of useless blabbering
//         for the monitoring shift crew

#include <TrigRunLvl2.h>

#include <phool.h>

#include <sstream>


ClassImp(TrigRunLvl2)

using namespace std;

static int shutup = 0;

TrigRunLvl2*
TrigRunLvl2::clone() const
{
  cout << PHWHERE << " is not implemented in daughter class. Sorry."
       << endl;
  return 0;
}

void TrigRunLvl2::Copy(TrigRunLvl2 * in)
{

  ostringstream d1;
  d1 << in->get_lvl2_description();
  set_lvl2_description(d1.str().c_str());

  set_lvl2_version(in->get_lvl2_version());
  set_lvl2_run_enable(in->get_lvl2_run_enable());
  set_lvl2_reject_enable(in->get_lvl2_reject_enable());

  for (int i1bit = 0; i1bit < 32; i1bit++)
    {

      set_lvl1_lvl2_reject_enable(in->get_lvl1_lvl2_reject_enable(i1bit), i1bit);
      set_lvl1_lvl2_force_accept(in->get_lvl1_lvl2_force_accept(i1bit), i1bit);

      for (int i2bit = 0; i2bit < 64; i2bit++)
        {
          set_lvl2_lvl1_prescale(in->get_lvl2_lvl1_prescale(i2bit, i1bit), i2bit, i1bit);
          set_lvl2_lvl1_assoc(in->get_lvl2_lvl1_assoc(i2bit, i1bit), i2bit, i1bit);
        }
    }

  for (int i2bit = 0; i2bit < 64; i2bit++)
    {
      ostringstream d2;
      d2 << in->get_lvl2_trig_name(i2bit);
      set_lvl2_trig_name(d2.str().c_str(), i2bit);
      set_lvl2_trig_bit(in->get_lvl2_trig_bit(i2bit), i2bit);
      set_lvl2_trig_version(in->get_lvl2_trig_version(i2bit), i2bit);
    }

}

int
TrigRunLvl2::isValid() const
{
  cout << PHWHERE << ": isValid() not implemented" << endl;
  return 0;
}

void
TrigRunLvl2::identify(ostream& os) const
{
  os << "identify yourself: virtual TrigRunLvl2 object" << endl;
  return ;
}

void
TrigRunLvl2::Reset()
{
  cout << PHWHERE << ": ERROR Reset() not implemented" << endl;
  return ;
}

void
TrigRunLvl2::warning(const char *funcname) const
{
  if (!shutup)
    {
      printf("TrigRunLvl2: %s is virtual\n", funcname);
    }
    return ;
  }

const char *
TrigRunLvl2::get_lvl2_description() const
{
  warning("get_lvl2_description()");
  return NULL;
}

void
TrigRunLvl2::set_lvl2_description(const char* /*name*/)
{
  warning("set_lvl2_description(const char *name)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_version() const
{
  warning("get_lvl2_version() const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_version(const unsigned int /*ival*/)
{
  warning("set_lvl2_version(const unsigned int ival)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_run_enable() const
{
  warning("get_lvl2_run_enable() const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_run_enable(const unsigned int /*ival*/)
{
  warning("set_lvl2_run_enable(const unsigned int ival)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_reject_enable() const
{
  warning("get_lvl2_reject_enable() const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_reject_enable(const unsigned int /*ival*/)
{
  warning("set_lvl2_reject_enable(const unsigned int ival)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl1_lvl2_reject_enable(const unsigned int /*i*/) const
{
  warning("get_lvl1_lvl2_reject_enable(const unsigned int i) const");
  return 0;
}

void
TrigRunLvl2::set_lvl1_lvl2_reject_enable(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl1_lvl2_reject_enable(const unsigned int ival, const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl1_lvl2_force_accept(const unsigned int /*i*/) const
{
  warning("get_lvl1_lvl2_force_accept(const unsigned int i) const");
  return 0;
}

void
TrigRunLvl2::set_lvl1_lvl2_force_accept(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl1_lvl2_force_accept(const unsigned int ival,const unsigned int i)");
  return;
}

const char *
TrigRunLvl2::get_lvl2_trig_name(const unsigned int /*i*/) const
{
  warning("get_lvl2_trig_name(const unsigned int i) const");
  return NULL;
}

void
TrigRunLvl2::set_lvl2_trig_name(const char* /*name*/, const unsigned int /*i*/)
{
  warning("set_lvl2_trig_name(const char *name,const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_trig_bit(const unsigned int /*i*/) const
{
  warning("get_lvl2_trig_bit(const unsigned int i) const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_trig_bit(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl2_trig_bit(const unsigned int ival,const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_trig_version(const unsigned int /*i*/) const
{
  warning("get_lvl2_trig_version(const unsigned int i) const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_trig_version(const unsigned int /*ival*/, const unsigned int /*i*/)
{
  warning("set_lvl2_trig_version(const unsigned int ival, const unsigned int i)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_lvl1_assoc(const unsigned int /*i*/, const unsigned int /*j*/) const
{
  warning("get_lvl2_lvl1_assoc(const unsigned int i, const unsigned int j) const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_lvl1_assoc(const unsigned int /*ival*/, const unsigned int /*i*/, const unsigned int /*j*/)
{
  warning("set_lvl2_lvl1_assoc(const unsigned int ival, const unsigned int i, const unsigned int j)");
  return;
}

unsigned int
TrigRunLvl2::get_lvl2_lvl1_prescale(const unsigned int /*i*/, const unsigned int /*j*/) const
{
  warning("get_lvl2_lvl1_prescale(const unsigned int i, const unsigned int j) const");
  return 0;
}

void
TrigRunLvl2::set_lvl2_lvl1_prescale(const unsigned int /*ival*/, const unsigned int /*i*/, const unsigned int /*j*/)
{
  warning("set_lvl2_lvl1_prescale(const unsigned int ival, const unsigned int i, const unsigned int j)");
  return;
}

void
TrigRunLvl2::dump_info(const TrigRunLvl2 *runlevel2) const
{
  printf("RUN LEVEL2 TRIGGER OBJECT INFORMATION\n");
  printf("=====================================\n\n");
  printf("  Trigger Description       = %s\n",runlevel2->get_lvl2_description());
  printf("  Trigger Version           = %u\n",runlevel2->get_lvl2_version());
  printf("  Run    Enable = %u\n",runlevel2->get_lvl2_run_enable());
  printf("  Reject Enable = %u\n\n",runlevel2->get_lvl2_reject_enable());
  for (int i = 0; i < 28; i++)
    {
      if (runlevel2->get_lvl2_trig_name(i) != NULL)
	{
	  printf("    Lvl2 Name = %s\n",runlevel2->get_lvl2_trig_name(i));
	  printf("         Bit  = %u\n",runlevel2->get_lvl2_trig_bit(i));
	}
    }
  return;
}

void
TrigRunLvl2::ShutUp(const int i)
{
  shutup = i;
  return;
}
