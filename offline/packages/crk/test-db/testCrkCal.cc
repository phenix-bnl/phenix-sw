#include <iostream>
#include <string>
#include "CrkCalib.h"

// class for menu processor
template <class T>
class TestCommand {
public:
  TestCommand(){}
  TestCommand(string Descrip, void (*func)(T *)) {
    d_Descrip = Descrip; d_func = func;
  }
  print(void) {
    cout << d_Descrip << endl;
  }

// data members
  string d_Descrip;
  void (*d_func)(T*);
};

// functions to test member func of class CrkCalADC
static void print_adc(CrkCalib *cal) {cal->print_adc();}
static void read_file_adc(CrkCalib *cal) {cal->read_file_adc();}
static void write_file_adc(CrkCalib *cal) {cal->write_file_adc();}
static void fetch_DB_adc(CrkCalib *cal) {cal->fetch_DB_adc();}
static void store_DB_adc(CrkCalib *cal) {cal->store_DB_adc();}
static void make_default_adc(CrkCalib *cal) {cal->make_default_adc();}

static void print_t0(CrkCalib *cal) {cal->print_t0();}
static void read_file_t0(CrkCalib *cal) {cal->read_file_t0();}
static void write_file_t0(CrkCalib *cal) {cal->write_file_t0();}
static void fetch_DB_t0(CrkCalib *cal) {cal->fetch_DB_t0();}
static void store_DB_t0(CrkCalib *cal) {cal->store_DB_t0();}
static void make_default_t0(CrkCalib *cal) {cal->make_default_t0();}

static void print_tac(CrkCalib *cal) {cal->print_tac();}
static void read_file_tac(CrkCalib *cal) {cal->read_file_tac();}
static void write_file_tac(CrkCalib *cal) {cal->write_file_tac();}
static void fetch_DB_tac(CrkCalib *cal) {cal->fetch_DB_tac();}
static void store_DB_tac(CrkCalib *cal) {cal->store_DB_tac();}
static void make_default_tac(CrkCalib *cal) {cal->make_default_tac();}

static void print_slew(CrkCalib *cal) {cal->print_slew();}
static void read_file_slew(CrkCalib *cal) {cal->read_file_slew();}
static void write_file_slew(CrkCalib *cal) {cal->write_file_slew();}
static void fetch_DB_slew(CrkCalib *cal) {cal->fetch_DB_slew();}
static void store_DB_slew(CrkCalib *cal) {cal->store_DB_slew();}
static void make_default_slew(CrkCalib *cal) {cal->make_default_slew();}

static void bye(CrkCalib *) { exit(0);}

int main()
{

  vector< TestCommand<CrkCalib> > menu;
  menu.push_back(TestCommand<CrkCalib>("read_file_adc",read_file_adc));
  menu.push_back(TestCommand<CrkCalib>("write_file_adc",write_file_adc));
  menu.push_back(TestCommand<CrkCalib>("print_adc",print_adc));
  menu.push_back(TestCommand<CrkCalib>("fetch_DB_adc",fetch_DB_adc));
  menu.push_back(TestCommand<CrkCalib>("store_DB_adc",store_DB_adc));
  menu.push_back(TestCommand<CrkCalib>("make_default_adc",make_default_adc));

  menu.push_back(TestCommand<CrkCalib>("read_file_t0",read_file_t0));
  menu.push_back(TestCommand<CrkCalib>("write_file_t0",write_file_t0));
  menu.push_back(TestCommand<CrkCalib>("print_t0",print_t0));
  menu.push_back(TestCommand<CrkCalib>("fetch_DB_t0",fetch_DB_t0));
  menu.push_back(TestCommand<CrkCalib>("store_DB_t0",store_DB_t0));
  menu.push_back(TestCommand<CrkCalib>("make_default_t0",make_default_t0));

  menu.push_back(TestCommand<CrkCalib>("read_file_tac",read_file_tac));
  menu.push_back(TestCommand<CrkCalib>("write_file_tac",write_file_tac));
  menu.push_back(TestCommand<CrkCalib>("print_tac",print_tac));
  menu.push_back(TestCommand<CrkCalib>("fetch_DB_tac",fetch_DB_tac));
  menu.push_back(TestCommand<CrkCalib>("store_DB_tac",store_DB_tac));
  menu.push_back(TestCommand<CrkCalib>("make_default_tac",make_default_tac));

  menu.push_back(TestCommand<CrkCalib>("read_file_slew",read_file_slew));
  menu.push_back(TestCommand<CrkCalib>("write_file_slew",write_file_slew));
  menu.push_back(TestCommand<CrkCalib>("print_slew",print_slew));
  menu.push_back(TestCommand<CrkCalib>("fetch_DB_slew",fetch_DB_slew));
  menu.push_back(TestCommand<CrkCalib>("store_DB_slew",store_DB_slew));
  menu.push_back(TestCommand<CrkCalib>("make_default_slew",make_default_slew));

  menu.push_back(TestCommand<CrkCalib>("Exit from the program",bye));

  CrkCalib *calib = new CrkCalib();

  while(1) {
    int icmd;

    cout << endl << " ----- Menu ----- " << endl;
    while(1) {

      int i = 0;
      TestCommand<CrkCalib> *cmd = menu.begin();
      while(cmd != menu.end()) {
	cout << i << " : " << cmd->d_Descrip << endl;
	++i;
	++cmd;
      }

      cout << " select command: ";
      cin >> icmd;

      if( 0 <= icmd && icmd < menu.size()) break;
      else cout << "Invaid command" << endl;
    }
    menu[icmd].d_func(calib);
  }
}
