Immutable Distributed Hash Table
    The program implements an immutable distributed hash table (DHT) similar to Chord. This
	is a data structure that stores key-value pairs, distributed across a ring of nodes1. A key/value
	pair can be inserted into the DHT, or a key can be queried to retrieve the associated value. Since
	this distributed hash table is immutable, the values for each key cannot be updated after they are 
	initially set.

Team member: Minjia Huang, Minghua Zhang

Feature: - insertion for key/value pair into the DHT
         - querying to get information from the DHT
         - distributed programming, which mean the DHT is able to run across multiple computers, with actors
           randomly assigned to each computer
         - covered special case:
         	+ insert from node i that is smaller/greater than the node it should hash to
         	+ query before insert
         	+ two different keys hash to the same node
     		+ query K1 in node i, K2 insert to same node i, but query will keep waiting in case another insert
     		  meet querying requirement

Bug: - the program won't terminal by itself, need to use "^C" & "A" to crash program
	 - might have some warning for unused variable

Potential Bug: - if the input is incorrect(ie. fromNode in query is greater than the total number of nodes),
				 it will cause the program to crash

Simulation: - the program has tested with 4 VMs, and it worked out as expected when commands is inputted after 
			  all 4 VMs are completely set up
			- the program has also tested with 2^6 nodes and it worked out as expected
Compile & Run: example input1:
				   $ bash run.sh
				   4
				   insert 2 testing foo
				   insert 1 bestclass ProgLang
				   query 3 8 bestclass
				   query 12 1 testing