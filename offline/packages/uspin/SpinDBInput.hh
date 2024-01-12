//////////////////////////////////////////////////////////////
//
// SpinDBInput class
// Author      : Y. Fukao
// Description : Utility to write data into the spin database
// Created     : 2005-03-04
//
//
//
//
// 2008-2-26   : Updated by T.Kempel.  Added protection into UpdateDBContent()
//             :    now the function checks for ERROR_VALUE before setting values
//
// 2008-3-7    : Updated by T. Kempel to include qa_level index
//             : Also added 'Initialize' functions
//
//////////////////////////////////////////////////////////////

#ifndef _SPINDBINPUT_
#define _SPINDBINPUT_

#include <stdio.h>
#include <sstream>

#include <TObject.h>

#include "SpinDBContent.hh"

namespace odbc{
  class Connection;
  class Statement;
  class ResultSet;
};

class SpinDBInput : public TObject{
public:
  SpinDBInput(void){Initialize();}
  virtual ~SpinDBInput(void){Delete();}
  void Initialize(void);
  void Delete(Option_t* ="");
  int IsConnected(void);
  int CheckRunRow(int runnum, int qa_level, const char *opt="");
  int CreateRunRow(int runnum, int qa_level);
  int DeleteRunRow(int runnum, int qa_level);
  int CheckQARunRow(int runnum);
  int CreateQARunRow(int runnum);
  int DeleteQARunRow(int runnum);
  int SetQADefault(int runnum,int qa_level);
  int InitializeRunRow(SpinDBContent spin_cont);
  int UpdateDBContent(SpinDBContent spin_cont);
  int UpdateValue(int runnum, int qa_level, const char *name,int value);
  int UpdateValue(int runnum, int qa_level, const char *name,float value);
  int UpdateValue(int runnum, int qa_level, const char *name,double value);
  int UpdateArray(int runnum, int qa_level, const char *name,int *value,int nvalue);
  int UpdateArray(int runnum, int qa_level, const char *name,float *value,int nvalue);
  int UpdateArray(int runnum, int qa_level, const char *name,double *value,int nvalue);
  int UpdateArray(int runnum, int qa_level, const char *name,unsigned int *value,int nvalue);
  int UpdateArray(int runnum, int qa_level, const char *name,long long *value,int nvalue);
  int InitializeValue(int runnum, int qa_level, const char *name);
  int InitializeArray(int runnum, int qa_level, const char *name,int nvalue);

private:
  static const char *TABLE_NAME;
  static const int ERROR_VALUE;

  odbc::Connection *con;
  int run_check;
  int qa_check;

  int UpdateValue(int runnum, int qa_level, const char *cmd);
  template<class T>
  int UpdateValueTemp(int runnum, int qa_level, const char *name,T value);
  template<class T>
  int UpdateArrayTemp(int runnum, int qa_level, const char *name,T *value,int nvalue);

  ClassDef(SpinDBInput,1)
};

#endif /* _SPINDBINPUT_ */
