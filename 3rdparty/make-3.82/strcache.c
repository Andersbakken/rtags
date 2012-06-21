/* Constant string caching for GNU Make.
Copyright (C) 2006, 2007, 2008, 2009, 2010 Free Software Foundation, Inc.
This file is part of GNU Make.

GNU Make is free software; you can redistribute it and/or modify it under the
terms of the GNU General Public License as published by the Free Software
Foundation; either version 3 of the License, or (at your option) any later
version.

GNU Make is distributed in the hope that it will be useful, but WITHOUT ANY
WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

You should have received a copy of the GNU General Public License along with
this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include "make.h"

#include <assert.h>

#include "hash.h"

/* The size (in bytes) of each cache buffer.
   Try to pick something that will map well into the heap.  */
#define CACHE_BUFFER_SIZE   (8192 - 16)


/* A string cached here will never be freed, so we don't need to worry about
   reference counting.  We just store the string, and then remember it in a
   hash so it can be looked up again. */

struct strcache {
  struct strcache *next;    /* The next block of strings.  */
  char *end;                /* Pointer to the beginning of the free space.  */
  int count;                /* # of strings in this buffer (for stats).  */
  int bytesfree;            /* The amount of the buffer that is free.  */
  char buffer[1];           /* The buffer comes after this.  */
};

static int bufsize = CACHE_BUFFER_SIZE;
static struct strcache *strcache = NULL;

/* Add a new buffer to the cache.  Add it at the front to reduce search time.
   This can also increase the overhead, since it's less likely that older
   buffers will be filled in.  However, GNU make has so many smaller strings
   that this doesn't seem to be much of an issue in practice.
 */
static struct strcache *
new_cache()
{
  struct strcache *new;
  new = xmalloc (sizeof (*new) + bufsize);
  new->end = new->buffer;
  new->count = 0;
  new->bytesfree = bufsize;

  new->next = strcache;
  strcache = new;

  return new;
}

static const char *
add_string(const char *str, int len)
{
  struct strcache *best = NULL;
  struct strcache *sp;
  const char *res;

  /* If the string we want is too large to fit into a single buffer, then
     we're screwed; nothing will ever fit!  Change the maximum size of the
     cache to be big enough.  */
  if (len > bufsize)
    bufsize = len * 2;

  /* First, find a cache with enough free space.  We always look through all
     the blocks and choose the one with the best fit (the one that leaves the
     least amount of space free).  */
  for (sp = strcache; sp != NULL; sp = sp->next)
    if (sp->bytesfree > len && (!best || best->bytesfree > sp->bytesfree))
      best = sp;

  /* If nothing is big enough, make a new cache.  */
  if (!best)
    best = new_cache();

  assert (best->bytesfree > len);

  /* Add the string to the best cache.  */
  res = best->end;
  memcpy (best->end, str, len);
  best->end += len;
  *(best->end++) = '\0';
  best->bytesfree -= len + 1;
  ++best->count;

  return res;
}


/* Hash table of strings in the cache.  */

static unsigned long
str_hash_1 (const void *key)
{
  return_ISTRING_HASH_1 ((const char *) key);
}

static unsigned long
str_hash_2 (const void *key)
{
  return_ISTRING_HASH_2 ((const char *) key);
}

static int
str_hash_cmp (const void *x, const void *y)
{
  return_ISTRING_COMPARE ((const char *) x, (const char *) y);
}

static struct hash_table strings;
static unsigned long total_adds = 0;

static const char *
add_hash (const char *str, int len)
{
  /* Look up the string in the hash.  If it's there, return it.  */
  char *const *slot = (char *const *) hash_find_slot (&strings, str);
  const char *key = *slot;

  /* Count the total number of adds we performed.  */
  ++total_adds;

  if (!HASH_VACANT (key))
    return key;

  /* Not there yet so add it to a buffer, then into the hash table.  */
  key = add_string (str, len);
  hash_insert_at (&strings, key, slot);
  return key;
}

/* Returns true if the string is in the cache; false if not.  */
int
strcache_iscached (const char *str)
{
  struct strcache *sp;

  for (sp = strcache; sp != 0; sp = sp->next)
    if (str >= sp->buffer && str < sp->end)
      return 1;

  return 0;
}

/* If the string is already in the cache, return a pointer to the cached
   version.  If not, add it then return a pointer to the cached version.
   Note we do NOT take control of the string passed in.  */
const char *
strcache_add (const char *str)
{
  return add_hash (str, strlen (str));
}

const char *
strcache_add_len (const char *str, int len)
{
  /* If we're not given a nul-terminated string we have to create one, because
     the hashing functions expect it.  */
  if (str[len] != '\0')
    {
      char *key = alloca (len + 1);
      memcpy (key, str, len);
      key[len] = '\0';
      str = key;
    }

  return add_hash (str, len);
}

int
strcache_setbufsize(int size)
{
  if (size > bufsize)
    bufsize = size;
  return bufsize;
}

void
strcache_init (void)
{
  hash_init (&strings, 8000, str_hash_1, str_hash_2, str_hash_cmp);
}


/* Generate some stats output.  */

void
strcache_print_stats (const char *prefix)
{
  int numbuffs = 0, numstrs = 0;
  int totsize = 0, avgsize, maxsize = 0, minsize = bufsize;
  int totfree = 0, avgfree, maxfree = 0, minfree = bufsize;
  int lastused = 0, lastfree = 0;

  if (strcache)
    {
      const struct strcache *sp;

      /* Count the first buffer separately since it's not full.  */
      lastused = strcache->end - strcache->buffer;
      lastfree = strcache->bytesfree;

      for (sp = strcache->next; sp != NULL; sp = sp->next)
        {
          int bf = sp->bytesfree;
          int sz = sp->end - sp->buffer;

          ++numbuffs;
          numstrs += sp->count;

          totsize += sz;
          maxsize = (sz > maxsize ? sz : maxsize);
          minsize = (sz < minsize ? sz : minsize);

          totfree += bf;
          maxfree = (bf > maxfree ? bf : maxfree);
          minfree = (bf < minfree ? bf : minfree);
        }
    }

  avgsize = numbuffs ? (int)(totsize / numbuffs) : 0;
  avgfree = numbuffs ? (int)(totfree / numbuffs) : 0;

  printf (_("\n%s # of strings in strcache: %d / lookups = %lu / hits = %lu\n"),
          prefix, numstrs, total_adds, (total_adds - numstrs));
  printf (_("%s # of strcache buffers: %d (* %d B/buffer = %d B)\n"),
          prefix, (numbuffs + 1), bufsize, ((numbuffs + 1) * bufsize));
  printf (_("%s strcache used: total = %d (%d) / max = %d / min = %d / avg = %d\n"),
          prefix, totsize, lastused, maxsize, minsize, avgsize);
  printf (_("%s strcache free: total = %d (%d) / max = %d / min = %d / avg = %d\n"),
          prefix, totfree, lastfree, maxfree, minfree, avgfree);

  fputs (_("\n# strcache hash-table stats:\n# "), stdout);
  hash_print_stats (&strings, stdout);
}
