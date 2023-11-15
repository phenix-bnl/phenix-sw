#include <stdio.h>
#include <iostream>
#include <stdlib.h>
#include <sstream>
#include <string>
#include <odbc++/connection.h>
#include <odbc++/setup.h>
#include <odbc++/types.h>
#include <odbc++/errorhandler.h>
#include <sql.h>
#include <odbc++/drivermanager.h>
#include <odbc++/resultset.h>
#include <odbc++/resultsetmetadata.h>
#include <odbc++/preparedstatement.h>
#include <odbc++/databasemetadata.h>
#include "phool.h"

using namespace odbc;
using namespace std;

extern "C" {             // This constuction is used
  extern struct {        // to pass variables to/from
    float xmid[24];      // named common block
    float ymid[24];      // common/wedgesurvey/.
    float wphi[24];      // 
  } wedgesurvey_ ;       // (name cannot contain underscores)  
}                        //

extern "C" void fvtx_getsurvey__(int &ver_passed, char* object, int &len)
{
  int ver = abs(ver_passed);
  //cout<<"GetSurvey. version: "<<ver<<", object: "<<object<<", length: "<<len<<endl;
  Connection* con = NULL;
  Statement* stmt = NULL;
  ResultSet* rs = NULL;
  ostringstream cmd;
 
  int v_seen;
  char disk[4];                // 'NE0'+null
  strncpy(disk,object,3);      // unequal length strings
  disk[3] = NULL;              // terminate properly by hand

  //Connect to database fvtx as user phnxrc. Database server
  //name comes from /opt/phenix/etc/odbc.ini config file. The third
  //argument is the password and left blank because it's automatically
  //picked up from odbc.ini file 
  try { con = DriverManager::getConnection("fvtx", "phnxrc", ""); }
  catch (SQLException& e)  { 
    cout << PHWHERE
         << " Exception caught during DriverManager::getConnection" << endl;
    cout << e.getMessage() << endl;
    return;
  }

  char cobject[len+1];                    // 'object name'+null
  //char cobject[20];                    // 'object name'+null
  strncpy(cobject,object,len);            // unequal length strings
  cobject[len] = NULL;                    // terminate

  //------------------------------------ get delta_x,y,phi_m ---------------------------------------

  //cout<<"GetSurvey - getting from data base: "<< object << endl;
  if ( strncmp(object,"delta",5) == 0) {
    for (int xyphi = 1; xyphi<=3; xyphi++) {

      cmd.str("");
      cmd << "select version,object from fvtx_survey WHERE  object='"<<cobject<<"_";
      if (xyphi==1) cmd <<"x_m' ORDER BY version DESC";
      if (xyphi==2) cmd <<"y_m' ORDER BY version DESC";
      if (xyphi==3) cmd <<"phi_m' ORDER BY version DESC";

      //cout << "1 cmd: "<< cmd.str() <<endl;
      stmt = con->createStatement();
      rs = stmt->executeQuery(cmd.str().c_str());

      int nrows=0;            // see if there are any rows, and how many
      int v_get = 0;          // find highest version number
      while (rs->next() ) {
        nrows++;
        v_seen = rs->getInt(1);                   // read the version numbers
        if (v_seen>v_get) v_get = v_seen;         // get the highest version
      }
      if (ver != 0) {
        if (ver < v_get) v_get = ver;             // request ~99 -> use latest version
      }
      ver = v_get;                                // pass back the version that was used 

      if (nrows==0 ) {
        cout <<" NO RECORD seen for "<<cobject<<" wedge x"<<endl; 
      }
      else {                                      // there is at least one record
        cmd.str("");
        cmd << "SELECT array_dims(values) FROM fvtx_survey WHERE version="<<v_get<<" AND object='"<<cobject<<"_";
        if (xyphi==1) cmd <<"x_m' ORDER BY version DESC";
        if (xyphi==2) cmd <<"y_m' ORDER BY version DESC";
        if (xyphi==3) cmd <<"phi_m' ORDER BY version DESC";

        //cout << "2 cmd: "<< cmd.str() <<endl;
        stmt = con->createStatement();
        try { rs = stmt->executeQuery(cmd.str().c_str()); }
        catch (SQLException& e) { cout << e.getMessage() << endl; }
        rs->next();
  
        string dim;                          // Postgres array dimension has
        dim = "";                            //  the following format: [1:24]
        dim = rs->getString(1);              // 1 - is array dim, 24 is the length
        dim = dim.substr(dim.size()-3,2);    // Array size in ascii. Fish out the '24'.
        int idim = atoi(dim.c_str());
        
        for(int i = 1; i <= idim; i++){    // loop over array elements 
          cmd.str("");
          cmd << "select values[" << i << "] from fvtx_survey where version="<<v_get<<" AND object='"<<cobject<<"_";
          if (xyphi==1) cmd <<"x_m' ORDER BY version DESC";
          if (xyphi==2) cmd <<"y_m' ORDER BY version DESC";
          if (xyphi==3) cmd <<"phi_m' ORDER BY version DESC";

          stmt = con->createStatement();
          try { rs = stmt->executeQuery(cmd.str().c_str()); }
          catch (SQLException& e) { cout << e.getMessage() << endl;  }
          rs->next();
          //cout <<"getFloat: "<< rs->getFloat(1) << endl;
          if (xyphi==1) wedgesurvey_.xmid[i-1] = rs->getFloat(1);   // copy to struct
          if (xyphi==2) wedgesurvey_.ymid[i-1] = rs->getFloat(1);   // 
          if (xyphi==3) wedgesurvey_.wphi[i-1] = rs->getFloat(1);   // 
        }                        // loop over array entries
      }                        //if there is a delta_x,y,phi disk record
    }                        // end loop over x, y, phi
  }                        // if object name starts with 'delta'

  //------------------------------------ get wedge data --------------------------------------------

  else {
    cmd.str("");
    cmd << "select version,object from fvtx_survey WHERE object='"<<disk<<" wedge x' ORDER BY version DESC";
    stmt = con->createStatement();
    rs = stmt->executeQuery(cmd.str().c_str());

    int nrows=0;            // see if there are any rows, and how many
    int v_get = 0;          // find highest version number
    while (rs->next() ) {
      nrows++;
      v_seen = rs->getInt(1);                   // read the version numbers
      if (v_seen>v_get) v_get = v_seen;         // get the highest version
    }
    if (ver != 0) {
      if (ver < v_get) v_get = ver;                  // request ~99 -> use latest version
    }
    ver = v_get;                                // pass back the version that was used 

    if (nrows==0 ) {
      cout <<" NO RECORD seen for "<<disk<<" wedge x"<<endl; 
    }
    else {                                      // there is at least one record
      //cout <<"v_get= "<<v_get<<endl;
      for (int xyphi=0; xyphi<3; xyphi++) {     // loop over x,y,phi
        cmd.str("");
        cmd << "select array_dims(values) from fvtx_survey where version="<<v_get<<" AND object='"<<disk<<" wedge ";
        if (xyphi==0) cmd << "x'";
        if (xyphi==1) cmd << "y'";
        if (xyphi==2) cmd << "phi'";
        stmt = con->createStatement();
        try { rs = stmt->executeQuery(cmd.str().c_str()); }
        catch (SQLException& e) { cout << e.getMessage() << endl; }
        rs->next();
  
        string dim;                          // Postgres array dimension has
        dim = "";                            //  the following format: [1:24]
        dim = rs->getString(1);              // 1 - is array dim, 24 is the length
        dim = dim.substr(dim.size()-3,2);    // Array size in ascii. Fish out the '24'.

        for(int i = 1; i <= atoi(dim.c_str()); i++){    // loop over array elements 
          cmd.str("");
          cmd << "select values[" << i << "] from fvtx_survey where version="<<v_get<<" AND object='"<<disk<<" wedge ";
          if (xyphi==0) cmd << "x'";
          if (xyphi==1) cmd << "y'";
          if (xyphi==2) cmd << "phi'";
          stmt = con->createStatement();
          try { rs = stmt->executeQuery(cmd.str().c_str()); }
          catch (SQLException& e) { cout << e.getMessage() << endl;  }
          rs->next();
          //cout <<"getFloat: "<< rs->getFloat(1) << endl;
          if (xyphi==0) wedgesurvey_.xmid[i-2] = rs->getFloat(1);   // copy to struct
          if (xyphi==1) wedgesurvey_.ymid[i-2] = rs->getFloat(1);
          if (xyphi==2) wedgesurvey_.wphi[i-2] = rs->getFloat(1);

          //if (xyphi==2 && (i==1 || i==24) && strcmp(disk,"SE3") ) {
          //  cout<<disk<<" wedge phi, version "<<v_get<<", value= "<<rs->getFloat(1)<<endl;
	  //}

        }                        // loop over array entries
      }                          // loop over x, y, phi
    }                            //if there is a record
  }                              // get wedges
  delete rs;                     // close the data base connection
  delete con;                    // close the data base connection
}                                // end of getsurvey
//-------------------------------------------------------------
/* removes all char = x in the C string s */
/* recall C strings have a '\0' to terminate the string */
/*
extern "C" void removeAll( char s[], char x ) {
  int i, j;
  for( i=0; s[i]!=0; ++i ) {
    while(s[i]==x) {  // copy all chars, including NULL at end, over char to left 
      j=i;
      while(s[j]!=0) {
          s[j]=s[j+1];
          ++j;
      }
    }
  }     // end for
}       //   end removeAll
*/
//---------------------------------------------------------------
