/*! @defgroup container Interface Object Containers

Interface Objects are stored in keyed containers.  The use of keyed containers 
that support iteration (generalized looping over variable length collections) 
mitigates many of the well know problems with storing and accessing data
members in fixed length arrays. Specifically the containers implemented for
use in the MUTOO analysis package are desinged to meet the following 
requirements.

<ul>
<li> A standard interface for safely iterating through container contents
<li> Well defined ownership semantics
<li> Support for iteration over associated objects
<li> Efficient (space) and fast
<li> Capable of reading and writing contents to ROOT TClonesArray
for persistent I/O
</ul>

*/












