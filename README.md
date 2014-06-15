FP-Router
=========
A framework to experiment with Haskell implementation of network routing algorithms.
The intention is build a framework which is generic enough to support any ‘simple’ IP (v4!) routing protocols, with an eye to extension to more complex domains, e.g. bridging, multiple IP address spaces/VPNs, IPv6, etc.
The intention is to build a framework which will allow various levels of completeness of the network stack: a minimal stack represents protocol messages exchanged as abstract data types; increasing levels of completeness might consist of - serialised messages containing valid protocol structures - complete network level messages at L2 or L3 (all these still passed between Haskell threads in a single ‘program’) - and beyond, implementation as routing daemons within multiple virtualised OS containers, using standard networking APIs for both route table control and message exchange.
The more abstract implementation is both an early/simpler goal and also expected to provide the most scalable test environment for large configurations.
