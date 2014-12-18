#include <windows.h>
void *mmap(void *addr, size_t len, int prot, int flags, int fildes, off_t off)
{
  HANDLE fm, h;
  void *map;
  const off_t maxSize = off + (off_t)len;

  h = (HANDLE)_get_osfhandle(fildes);
  fm = CreateFileMapping(h, NULL, PAGE_EXECUTE_READWRITE, 0, maxSize, NULL);
  map = MapViewOfFile(fm, FILE_MAP_READ | FILE_MAP_WRITE | FILE_MAP_EXECUTE, 0, off, len);
  CloseHandle(fm);
  return map;
}

void *dlsym(void *handle, char *name)
{
  if (!strcmp(name, "open"  )) return &open;
  if (!strcmp(name, "read"  )) return &read;
  if (!strcmp(name, "close" )) return &close;
  if (!strcmp(name, "printf")) return &printf;
  if (!strcmp(name, "malloc")) return &malloc;
  if (!strcmp(name, "memset")) return &memset;
  if (!strcmp(name, "memcmp")) return &memcmp;
  if (!strcmp(name, "memcpy")) return &memcpy;
  if (!strcmp(name, "mmap"  )) return &mmap;
  if (!strcmp(name, "dlsym" )) return &dlsym;
  if (!strcmp(name, "qsort" )) return &qsort;
  if (!strcmp(name, "exit"  )) return &exit;
  return 0;
}
#define CHAR TYCHAR
#define INT TYINT
