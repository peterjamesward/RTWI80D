
# This is a clone of STABLE version of tree-based GPX reader.

Idea is to slim this down and see if it can read the entire route
of Mark Beaumont's Round the World in 80 Days, and display on a
Map with selective rendering.

PROVED, now to refine.

DONE 1. Reduce domain model by chucking out anything not Map related.
DONE 2. Discard 3D view and un-needed view stuff. Keep slider & maybe info box.
1. Define 5 separate trees; load and display separately.
4. Ingest piecewise, so we can reclaim memory after each piece. (340MB file!)
2. At top level, Map click detect decides which tree.
3. Ditch the scroll bar and info box.
5. Get bounding box from Map and use that to render at a depth that gives a stable # points.
6. Try storing data in more compact form (CBOR) for faster loading; must remove file prompts.