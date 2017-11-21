---
layout: single
permalink: homework03.html
title: P4 Switch Project      
---

# Changelog

* v1: November 10, 2017
        
# Overview    
    
In this assignment, you will continue the task we began in Phase I:
implementing the key components of a network switch in P4. We will
follow this schedule:
    
* Phase I: Released October 31; Due November 9th.
* Phase II: Released November 10th; Due November 21st.
* Phase III: Due December 1st

For the purposes of assigning course grades, Phases I will serve as
the 2nd homework assignments, Phase II as the 3rd homework assignment,
and Phase III will serve as the course project.

The project may be completed individually or with a partner. However,
If you choose to work with a partner, you must register on CMS, you
must complete all phases of the project as a team, and both members of
the partnership must make significant contributions to the solution.

# Academic Integrity    

All work you submit must be your own and sharing or receiving code is
forbidden, with the exception of your partner. Do not look for or
submit code you find on the Internet, and do not post solutions or
partial solutions on the discussion site. If you make use of _any_
outside materials, you must give proper attribution. You may ask
general questions about the development environment, `p4app`, `p4c`,
`bmv2`, Mininet, etc., and you may discuss high-level details of the
exercises with your classmates. If you have any questions about what
is allowed and what is not allowed, please ask the instructor first!

# Software Environment

We will use the same `p4app` Docker container as we used for phase I.
    
# Starter Code

You can download starter code for the programming exerises in this
assignment as a zipfile from CMS.
                
# Phase II

In the second phase of the project, you will extend your switch with
two additional pieces of functionality.

## Exercise 1: Monitoring

In this exercise, you will extend the simple router with support for
traffic monitoring. More specifically, your application will maintain
counters that tabulate the total amount of traffic (i.e., in bytes)
sent by each host, as identified by its IP source address.
        
### Implementation Steps
    
To complete this exericse, you should extend the simple router as
follows:

* Construct a `counter` object in the ingress control
* Create a new action to count the bytes sent by each host, or modify an existing action
* Invoke this action at a suitable location in the program
* Edit the `s1.config` file if needed to reflect any new actions

Note that your P4 program may impose a limit on how many hosts can be
monitored, but it should *not* "bake in" any particular IP
addresses. (Any specific IP addresses should go into the `s1.config`
file instead.)
    
### Testing
    
To test that your program is working correctly, you can use the
`send.py` and `receive.py` scripts to send an receive network
packets. To test that your switch is correctly tabulating packets, you
can fetch the values of the counters using the BMv2 command-line
interface. For example, the following will fetch the first value from
`my_counter`:
    
```
mininet> s1 simple_switch_CLI
Obtaining JSON from switch...
Done
Control utility for runtime P4 table manipulation
RuntimeCmd: counter_read my_counter 0
```                    
**To submit:** `monitoring.p4app.{tar.gz,zip}`   
    
## Exercise 2: Tunneling
    
In this exercise, you will extend the basic router with support for a
simple form of tunneling. Tunneling protocols such as MPLS are often
used by Internet Service Providers to implement end-to-end forwarding
of customer traffic through their networks without having to examine
the original packet's headers.

The behavior of our switch will be divided into two disjoint cases:
1. If the packet has Ethertype 0x800, then it will use standard IP forwarding.

2. If the packet has ethertype 0x6114, then it will instead forward
the packet using a tunnel identifier that encodes the output port. 

The format of the tunnel header is as follows:
```
+---------------------------+----------+
|11111111 | 8-bit tunnel id | 00000000 |
+---------------------------+----------+
```
    
### Implementation Steps

* Define a header for tunnel and add to struct representing parsed headers 
* Extend the parser with support for the tunneled header
* Extend the ingress control to either use standard IP forwarding or to forward using the tunnel identifier
* Extend the deparser to emit the tunnel header
    
### Testing    

To test your solution, we have provided you with the `send.py` and
`receive.py` scripts. The `send.py` script is capable of generating
both tunneled and non-tunneled packets depending on the presence or
absence of the `--tunnel_id` argument.
        
**To submit:** `tunnel.p4app.zip`

## Karma Problems (Optional)

* Generalize your solution to Exercise 1 by implementing a counting
  Bloom Filter rather than a standard counter object.

* Generalize your solution to Exercise 2 so that there are three kinds
  of switches: (i) ingress switches, (ii) internal switches, and (iii)
  egress switches. Ingress switches should attach a tunnel identifier
  to ordinary IP packets, internal switches should forward based on
  tunnel identifier, and egress switches remove the tunnel
  identifier. To test your solution you can use the `multiswitch`
  target for `p4app`. However, note that `p4app` currently only
  supports a single P4 program that is installed on all switches, so
  you will need to write a program that handles all three cases and
  instantiate the behavior using the control plane.

## Exercise 3: Proposal Status

In Phase III of the project, you will use P4 to develop a novel piece
of data plane functionality that you design. Please provide a brief
status update on your project. This status update should describe your
approach to solving the problem and initial aspects of the design of
your solution (e.g., header types, sketch of control flow, and more
details on evaluation plans). Your status update can be brief -- no
more than a single page!
        
**To submit:** `project-update.{txt,pdf}`    
        
## Exercise 4: Debriefing    

* How many hours did you spend on this assignment? 

* Would you rate it as easy, moderate, or difficult? 

* If you worked with a partner, please briefly describe both of your
contributions to the solution you submitted.
    
* How deeply do you feel you understand the material it covers (0%-100%)?

* If you have any other comments, we would like to hear them! Please
write them down or send email to `jnfoster@cs.cornell.edu`

**To submit:** debriefing.txt


