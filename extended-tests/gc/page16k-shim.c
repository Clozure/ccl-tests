/* page16k-shim.c -- make a 4 KiB-page Linux look like a 16 KiB-page one
   to the process that preloads this library.

   A 16 KiB-page kernel reports 16384 from sysconf(_SC_PAGESIZE), refuses
   with EINVAL a MAP_FIXED mmap, an mprotect, an munmap or an madvise
   whose address is not a multiple of 16384, applies each of those to
   whole 16 KiB pages, and returns mappings on a 16 KiB boundary.  This
   library does the same through the real 4 KiB calls, so code that
   assumes 4 KiB pages fails here as it fails on such a kernel.

   Build: cc -shared -fPIC -O2 -o page16k-shim.so page16k-shim.c -ldl
   Use:   LD_PRELOAD=./page16k-shim.so <program>
   It writes one line to stderr when it loads, and one line for each
   call it refuses. */

#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <stdint.h>
#include <stdio.h>
#include <sys/mman.h>
#include <sys/types.h>
#include <unistd.h>

#define PAGE ((uintptr_t)16384)
#define MISALIGNED(a) (((uintptr_t)(a)) & (PAGE - 1))
#define ROUND_UP(n) ((((uintptr_t)(n)) + PAGE - 1) & ~(PAGE - 1))

static void *(*real_mmap)(void *, size_t, int, int, int, off_t);
static int (*real_mprotect)(void *, size_t, int);
static int (*real_munmap)(void *, size_t);
static int (*real_madvise)(void *, size_t, int);
static long (*real_sysconf)(int);

static void
refuse(const char *call, void *addr, size_t len)
{
  fprintf(stderr, "page16k-shim: EINVAL %s addr=%p len=0x%zx\n",
          call, addr, len);
}

__attribute__((constructor)) static void
init(void)
{
  real_mmap = dlsym(RTLD_NEXT, "mmap");
  real_mprotect = dlsym(RTLD_NEXT, "mprotect");
  real_munmap = dlsym(RTLD_NEXT, "munmap");
  real_madvise = dlsym(RTLD_NEXT, "madvise");
  real_sysconf = dlsym(RTLD_NEXT, "sysconf");
  fprintf(stderr, "page16k-shim: loaded, page size %lu\n",
          (unsigned long)PAGE);
}

long
sysconf(int name)
{
  if (name == _SC_PAGESIZE)
    return PAGE;
  return real_sysconf(name);
}

int
getpagesize(void)
{
  return PAGE;
}

void *
mmap(void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
  if (MISALIGNED(off) || ((flags & MAP_FIXED) && MISALIGNED(addr))) {
    refuse("mmap", addr, len);
    errno = EINVAL;
    return MAP_FAILED;
  }
  len = ROUND_UP(len);
  if (flags & MAP_FIXED)
    return real_mmap(addr, len, prot, flags, fd, off);
  /* Reserve one page more than asked, so a 16 KiB boundary is inside,
     map there, and give back the slack on both sides. */
  {
    char *r = real_mmap(addr, len + PAGE, PROT_NONE,
                        MAP_PRIVATE | MAP_ANONYMOUS | MAP_NORESERVE, -1, 0);
    char *a, *m;

    if (r == MAP_FAILED)
      return r;
    a = (char *)ROUND_UP(r);
    m = real_mmap(a, len, prot, flags | MAP_FIXED, fd, off);
    if (m == MAP_FAILED) {
      int err = errno;
      real_munmap(r, len + PAGE);
      errno = err;
      return m;
    }
    if (a > r)
      real_munmap(r, a - r);
    if (a + len < r + len + PAGE)
      real_munmap(a + len, (r + len + PAGE) - (a + len));
    return m;
  }
}

void *
mmap64(void *addr, size_t len, int prot, int flags, int fd, off_t off)
{
  return mmap(addr, len, prot, flags, fd, off);
}

int
mprotect(void *addr, size_t len, int prot)
{
  if (MISALIGNED(addr)) {
    refuse("mprotect", addr, len);
    errno = EINVAL;
    return -1;
  }
  return real_mprotect(addr, ROUND_UP(len), prot);
}

int
munmap(void *addr, size_t len)
{
  if (MISALIGNED(addr)) {
    refuse("munmap", addr, len);
    errno = EINVAL;
    return -1;
  }
  return real_munmap(addr, ROUND_UP(len));
}

int
madvise(void *addr, size_t len, int advice)
{
  if (MISALIGNED(addr)) {
    refuse("madvise", addr, len);
    errno = EINVAL;
    return -1;
  }
  return real_madvise(addr, ROUND_UP(len), advice);
}
