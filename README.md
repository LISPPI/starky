# starky

Work in progress.

This is a simplified OpenVG library, inspired by a.j.stark's openvg library, for exploring the system and figuring out how this works.

Freetype in ft:

(tin)
(ttt)
(tout)


Memory management is still a pain.  Here is the lowdown, now that I've done it every way stupid.

You have to allocate and deallocate.  In the meantime you have to carry the pointer.

With-... wraps it right there.  It is pretty efficient; the pointer is local.

My foolish malloc global stack - not so good.  It seems a tiny bit more compact, but it's still a pointer to keep the allocated space, a call to allocate, a call to deallocate.  Now the pointer is in a global space, which may be better for caching, but also fucks up all kinds of things, makes it possible to forget to deallocate, and makes it possible to deallocate something you mean to keep around.

A pool - single allocation, single deallocation, one pointer for many objects with offsets - that makes some sense.  It's still localized, and it is clear what goes with what.

Statics - good!  So this looks like an in-function allocation, but it is global, just initialized in function.  It persists, so within a thread you have allocated space guaranteed.  A little iffy from functional standpoint, not recursive.

Finally, just easier syntax...

