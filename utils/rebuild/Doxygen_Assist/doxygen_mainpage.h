/** 
 * @file  doxygen_mainpage.h
 * @brief This is not actually a header file but contains a doxygen "\mainpage" section.
 */
/** \mainpage
<!-- ================================================================ -->

\tableofcontents

Welcome to PHENIX offline software documentation!

\section STARTwebsite Start using this website

\subsection UsefulFeatures Highlighted Features

Many feature of the website will help you navigate through the ocean of PHENIX software. Few are highlighted here:
\li Ever want to find a class or function? It is easy now using the <b>"Search" box</b> on top right of every page. Type few initial letters it will give hints to the rest.
\li Left side is a <b>navigation bar</b> that expands automatically with your browsing by default. You can also make these two independent of each other by toggling the arrows on left top.
\li Want to browse the software <b>directory tree</b>? Check out the <a href="files.html"><i>Files</i> button in the navigation bar</a>.
\li How a class is inherited? Check out the <b>Inheritance diagram</b> in the class pages. They are auto folded by default. Example: click <i>"Inheritance diagram for VtxOutv7"</i> in page \ref VtxOutv7. 
\li Find out <b>inter-dependencies between files</b>. Example is on \ref PHPythia.h page, you can try clicking to unfold <i>"Include dependency graph"</i> and <i>"files directly or indirectly include this file"</i>.
\li Highlighted <b>source code</b> with crossreferencing. Example click <i>"Go to the source code"</i> in \ref PHPythia.h.
There are many more features, which you are welcome to explore.

\subsection Crossreference Crossreferencing with PHENIX CVS

Through a special build of Doxygen, <a href = "http://www.phenix.bnl.gov/viewvc/viewvc.cgi/phenix/">
PHENIX CVS</a> were automatically linked for
Files, directories and member definitions.
And users can choose between viewing the local copy of the source code (with more features) or the CVS one (most up-to-date).
Check out links like <EM>View newest version in PHENIX CVS</EM>
in e.g. mFvtxFindClus::automatic_load_dead_chan_map .
 
\subsection SlowQA Slow to load?

On some computers, user may experience delay up to 10s while loading pages from this web site. This is usually due to that the user browser rebuilds the navigation panel on the left side,
which contains the index of thousands of software modules. In that case, to speed up the browsing, users can choose to stop auto-updating of the navigation panel by toggle off the "Sync" switch:
<img src="sync_on.png" alt="click to disable panel synchronisation" title="the Sync switch icon as on the left top" style="horizontal-align:left">
, which is on the left top.

\section Subsystem Subsystem Guide

Following subsystems have more detail documentation pages: 

\li \ref FVTX "Forward silion vertex detector (FVTX)"
\li \ref VTX "Barrel silion vertex detector (VTX)"
\li \ref MuonArm "Muon Arm Software (MUTOO)"

You are welcome to contribute to new topics by contacting <a href="mailto:jhuang@bnl.gov">Jin Huang</a>.

\section Write_your_code Write your code with Doxygen documentation

You are highly encouraged to put Doxygen-style comments in your code, which will then be automatically applied to the documentation pages on this website (refreshed from CVS every a few hours). 
These features are easy to use, and helpful for readers to understand and use your developemnt.

To start use the Doxygen-style comments, please read the <a href = "http://www.stack.nl/~dimitri/doxygen/manual/docblocks.html#cppblock">instruction page on the Doxygen website</a>. 
Or even more easier, please just take a look at <a href="http://www.stack.nl/~dimitri/doxygen/manual/docblocks.html#docexamples">this example</a>, 
which will show up in <a href="http://www.stack.nl/~dimitri/doxygen/manual/examples/qtstyle/html/class_test.html">Doxygen page like this</a>. 
You are also welcome to let <a href="mailto:jhuang@bnl.gov">Jin Huang</a> know, if there is any question or suggestions.

\section OWiki offline Wiki

<a href = "https://www.phenix.bnl.gov/WWW/offline/wikioff/index.php/Main_Page">PHENIX offline Wiki</a> provides an up-to-date documentation source written by PHENIX analyzers for PHENIX analyzers. 

\section REBUILD Rebuild Info

\li Check out the <a href="https://www.phenix.bnl.gov/WWW/offline//software/tinderbox">rebuild</a> system for PHENIX.  It shows the state of attempts to rebuild all of our code from scratch.
\li Full <a href="https://www.phenix.bnl.gov/WWW/publish/hpereira/index_rebuild.php">list of rebuilds</a>.

<!-- ================================================================ -->
 */
