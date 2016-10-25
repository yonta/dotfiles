#!/bin/bash

#
#  o windows                  @ dn-windows   -+
#  |                          |               |
#  o                          ...             +- rebase copy of dn-windows
#  |                          |               |
#  ...                        o windows      -+
#  |                          |
#  o @ dn-changes[1-4]   =>   ...
#  | |                        |
#  | ...                      |  - close old dn-windows branch
#  | |                        |  |   not dn-changes[1-4]
#  | o                        |  o
#  | |                        |  ...
#  |/                         |/
#  o                          o
#  |                          |
#  ...                        ...

# close old dn-windows branch
hg up dn-windows
hg ci --close-branch -m 'Close branch'

# make new dn-windows branch
hg up windows
hg branch -f dn-windows
hg graft dn-changes1
hg graft dn-changes2
hg graft dn-changes3
hg graft dn-changes4
