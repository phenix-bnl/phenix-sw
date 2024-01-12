For More detailed instructions, check the wiki at

     https://www.phenix.bnl.gov/WWW/offline/wikioffline/index.php/PHPythia

     
--------------------------------------------------------------------------------------


Directory structure
-------------------

macros/
-------

Example macros for generating single particle events.

The phparticlegen.C macro in there is a self-explanatory (?) example macro.

To run, just type

              root.exe -b -q phparticlegen.C

custom_gen/
----------

One can make one's own custom single particle generator and add them here.  This allows
one to make any distribution that one likes.  Just follow the example of
TSingleParticleCustomGenerator.  Just override the two methods

  virtual void GenerateVertex();
  virtual void GenerateMomentum();

These two methods are defined as virtual in the base class TSingleParticleGenerator. 


--------------------------------------------------------------------------------------


Of course you have to have compiled all the code and have added the
lib location to your LD_LIBRARY_PATH.

Currently, the default pythia library used is located in 

   /opt/phenix/lib/libPythia6.so

though this is subject to change.  You can also use your own pythia
library if you want.  You will simply need to load that library in
your macro using gSystem->Load().


--------------------------------------------------------------------------------------


Note that there is also useful information, and much of the code in PHParticleGen
relies on base code in PHPythia.  For example, analyzing the phparticlegen.root file
can be done using the same code as for PHPythia:

event_gen/src/PHPythia/Ana: Fun4All example of analyzing a PHPythia file

event_gen/src/PHPythia/Ana/macros: Example macro for running the pythia analysis.

            To run, just type
              root.exe -b -q anaphpythia.C
            You need to point it to a file created with PHPythia or PHParticleGen or PHHijing.

event_gen/src/PHPythia/Triggers: PHPyTrigger derived classes that let you select only
          the events you are interested in to write out.


