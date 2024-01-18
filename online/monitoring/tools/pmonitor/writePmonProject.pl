#! /usr/bin/perl

if (@ARGV < 1) {
  print "Usage: writePmonProject.pl <projectname>\n" ;
  print "   e.g writePmonProject.pl MyAnalysis \n" ;
  exit 1;
}

$projectname=$ARGV[0];

print "creating project $projectname\n";

open (CC, "> $projectname.cc");

print CC <<EOF;

#include <iostream>
#include <pmonitor.h>
#include <TH1.h>
#include <TH2.h>

#include "$projectname.h"


int init_done = 0;

//TH1F *h1; 
//TH1F *h2; 

using namespace std;

int pinit()
{

  if (init_done) return 1;
  init_done = 1;

  //  h1 = new TH1F("H1", "Channel 0", 101,-49.5,49.5);

  return 0;
}

int process_event (Event * e)
{

  //  Packet *p = e->getPacket(1003);
  //  if (p)
      //    {
	//      h1->Fill (p->iValue(0));
	//      delete p;
	//    }

  return 0;
}

EOF

    close CC;

open (HH, "> $projectname.h");


print HH "#ifndef __";
print HH uc($projectname);
print HH "_H__\n";
print HH "#define __";
print HH uc($projectname);
print HH "_H__\n";

print HH <<EOF;

#include <Event.h>

int process_event (Event *e);

EOF
print HH "#endif /* __";
print HH uc($projectname);
print HH "_H__ */\n";


close HH;

open (MF, "> $projectname.Makefile");

print MF <<EOF;
PACKAGE = $projectname

CXXFLAGS = -I.  -I\$(ROOTSYS)/include -I\$(OFFLINE_MAIN)/include 

LDFLAGS = -L\$(OFFLINE_MAIN)/lib  -lpmonitor


HDRFILES = \$(PACKAGE).h
LINKFILE = \$(PACKAGE)LinkDef.h

ADDITIONAL_SOURCES = 


SO = lib\$(PACKAGE).so

\$(SO) : \$(PACKAGE).cc \$(PACKAGE)_dict.C \$(ADDITIONAL_SOURCES) \$(LINKFILE)
\t\$(CXX) \$(CXXFLAGS) -o \$@ -shared  \$<  \$(ADDITIONAL_SOURCES) \$(PACKAGE)_dict.C \$(LDFLAGS) 


\$(PACKAGE)_dict.C : \$(HDRFILES) \$(LINKFILE)
\trootcint -f \$@  -c \$(CXXFLAGS) \$^


.PHONY: clean

clean: 
\trm -f \$(SO)  \$(PACKAGE)_dict.C \$(PACKAGE)_dict.h

EOF

close MF;

$ldname = $projectname . "LinkDef.h";

open (LD, "> $ldname");

print LD <<EOF;
#ifdef __CINT__
#pragma link off all globals;
#pragma link off all classes;
#pragma link off all functions;
#endif /* __CINT__ */
EOF

close LD;
