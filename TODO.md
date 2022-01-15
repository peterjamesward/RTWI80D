
# This is a clone of STABLE version of tree-based GPX reader.

Idea is to slim this down and see if it can read the entire route
of Mark Beaumont's Round the World in 80 Days, and display on a
Map with selective rendering.

PROVED, now to refine.

DONE Reduce domain model by chucking out anything not Map related.
DONE Discard 3D view and un-needed view stuff. Keep slider & maybe info box.
2. Make Map Zoom and Move events
7. Get visible box from Map and use that to render at a depth that gives a stable # points.
> Think we need to get map Move and Zoom events (and disable tilt & rotate).
3. Define 5 separate trees; load and display separately. (so as not to show air legs)?
4. Ingest piecewise, so we can reclaim memory after each piece. (340MB file!)
5. Ditch click detect, therefore related "bounding box" aggregates.
6. Try storing data in more compact form (CBOR) for faster loading; get data via HTTP.
7. Or, look at M3O.
