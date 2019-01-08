# Viruscraft

In this project we ask - what determines the ability of a virus to infect some hosts but not others? Working with evolutionary biologist Ben Longdon, we're developing a citizen science project using visual programming, craft, tangible interfaces and games to explore virus host shifts – where a virus jumps from one host species to another.

![](concept/concept-1c.jpg)

https://fo.am/viruscraft/

# todo

- add host extinct/virus extinct to score record
- add virus description to score record
- increase death rate with susceptibility? highly specialised = quicker extinction

aims

-

# Parameters 

These are ones that can be tweaked & need to be checked:

## host-susceptibility

Currently a probability curve between 0 (totally immune) and 1 (certain to get infection)

"returns the susceptibility of the host of infection to the supplied virus
susceptibility will range from 0..1 with 0 being completely immune
pow give us a slope so that 1 is always 1 but we reduce the likelyhood
with fewer receptors - not science"

The point here is to make the successful strategy one of continual
mutation.  In order for this to work, we need to construct a tradeoff
so that viruses with one or two of each type of ligands are not very
good. Specialist viruses should generally be successful to infect.

