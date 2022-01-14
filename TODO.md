
# This is a clone of STABLE version of tree-based GPX reader.

Idea is to slim this down and see if it can read the entire route
of Mark Beaumont's Round the World in 80 Days, and display on a
Map with selective rendering.

1. Reduce domain model by chucking out anything not Map related.
2. Discard 3D view and un-needed view stuff. Keep slider & maybe info box.
3. Ingest piecewise, so we can reclaim memory after each piece.
4. Get bounding box from Map and use that to render at a depth that gives a stable # points.
5. Try storing data in more compact form (CROB) for faster loading?