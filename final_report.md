---
title: Project Update
author: Eric Campbell and Dietrich Geisler
date: November 21, 2017
publish: true
urlcolor: blue
---

We have re-implemented and measured [HashPipe][1]. This algorithm approximately
calculates the top heavy-hitters on a switch. To do this, it defines a pipeline of
`n` hash tables that record the byte-counts for each incoming flow. Of course, these
tables are finite, so it cannot actually record every flow, and so we must specify
a collision and eviction scheme.

Additionally, we introduce, implement, and evaluate a novel variation to the structure
original HashPipe paper.  Our modification consists of using unique hash functions
within a single table for each subsequent pipeline stage, which allows for improved
key distribution and so helps minimize the probability of evicting a heavy-hitter.

### Actions (Control Flow)

We define two different kinds of actions, `flow_in` and `heavy_hitter`: the `flow_in`
action handles flows as they come in and acts as a MRU cache; the `heavy_hitter`
actions add heavy-hitters to later stages in the pipeline calculations. There is
only one call to the `flow_in` action, but there may be arbitrarily many calls to
`heavy_hitter` actions.

The `flow_in` action always accepts the incoming packet. It, as its name implies,
computes the hash function for the key of the incoming flow and stores it in the
associated table index.  An important observation about this action is the requirement
that local action variables, such as `currIndex` and `currDiff`, are stored in the
metadata header to allow for concurrent computations. If there is a collision `flow-in`
writes-back the previous value at the given table index to the first `heavy_hitter`
action.  Collisions are identified to occur in both the `flow-in` and `heavy-hitter`
actions when the `validBit` of a given index is 1.  Write-back values are stored
in the metadata fields `fwdKey` and `fwdCount`.

In the `heavy-hitter` action, we accept writes only when `fwdKey` and `fwdCount`
are non-zero.  When a packet collides with another in a `heavy-hitter` hash table,
the flow that has the greater byte-count is left in place, and the flow that has
the lesser is written back to the next `heavy-hitter` action in the pipeline. If
the flows are the same, however, then the incoming count and current count are summed.
If the current `heavy-hitter` action is the last one, we drop the packet count from
the table. Hash tables are not implemented as switch-level `tables` but as _register
arrays_.  

### Implementation Notes

Our initial HashPipe implementation pipeline mimics the example given in the paper
and uses three stages, each of equal size.  Note that only one register array is
necessary for actual implementation, as each abstract table can be represented in
an offset block within the register.

When applying a unique hash function to each stage of the pipeline, we make the following
adjustments.  First, the table structure of the pipeline is modified so that the
`flow_in` action uses a single table, but each `heavy-hitter` action uses the same
table for checking for collisions.  Second, `flow_in` uses the same hash function
as the first call to the `heavy-hitter` action, since the `flow_in` cache does not
directly interact with the cache used by each `heavy-hitter` action.  Third, each
call to `heavy-hitter` uses a unique hash function to properly distribute key values.

### Evaluation

![Host Scaling](./hhc.png)
![Table Size 10](./ts10.PNG)
![Table Size 20](./ts20.PNG)
![Table Size 30](./ts30.PNG)

[1]: https://conferences.sigcomm.org/sosr/2017/papers/sosr17-heavy-hitter.pdf
