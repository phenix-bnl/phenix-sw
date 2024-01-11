/*! @defgroup supermodules Fun4All modules for muon new software

Hereabove is the list of fun4all modules used to interface fun4all to the new muon software. Requirement for these modules are:

<ul>
<li> must derive from Subsyreco class
<li> must have as few as possible public non baseclass functions
<li> must be kept as simple and modular as possible. It is better to use several specific such modules in conjonction in a .C macro than create twickable large modules doing everything. The modules may then be more easily considered as black boxes whereas the .C root macro, which is what is being 
used keeps better track of what is really being done.
</ul>

*/
