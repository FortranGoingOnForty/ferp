/*
 * ferp_posix.c - POSIX struct accessors for ferp_dir.f90
 *
 * Fortran cannot portably describe struct stat or struct dirent: their field
 * offsets differ by platform and architecture. ferp_dir.f90 used to hard-code
 * them (st_mode at byte 24, d_name at byte 19), which is correct only on
 * Linux/x86_64 -- on macOS st_mode sits at byte 4 and is 16 bits wide, and
 * d_name starts at byte 21.
 *
 * These shims let the C compiler read the real headers, so the layout is
 * always right and never needs restating here.
 */

#include <sys/stat.h>
#include <dirent.h>
#include <string.h>

/* Directory test. Follows symlinks, matching stat(2), so a symlink to a
 * directory reports as a directory the way grep treats it. */
int ferp_is_dir(const char *path)
{
    struct stat st;

    if (!path) return 0;
    return (stat(path, &st) == 0 && S_ISDIR(st.st_mode)) ? 1 : 0;
}

/* Symlink test. Must use lstat: stat would resolve the link and never
 * report one. */
int ferp_is_lnk(const char *path)
{
    struct stat st;

    if (!path) return 0;
    return (lstat(path, &st) == 0 && S_ISLNK(st.st_mode)) ? 1 : 0;
}

/* Regular-file test, excluding directories, devices, fifos and sockets. */
int ferp_is_reg(const char *path)
{
    struct stat st;

    if (!path) return 0;
    return (stat(path, &st) == 0 && S_ISREG(st.st_mode)) ? 1 : 0;
}

/*
 * Copy a dirent's d_name into buf and return its length.
 *
 * The name is copied bytewise up to the NUL. The previous Fortran version
 * scanned for "printable" bytes and stopped at anything outside 32..126,
 * which truncated every non-ASCII filename -- "cafe\xcc\x81.txt" became
 * "caf". Filenames are opaque byte strings on POSIX, so no such filtering
 * belongs here.
 *
 * Returns 0 if the entry or buffer is unusable. The result is always
 * NUL-terminated when bufsize > 0.
 */
int ferp_dirent_name(const void *entry, char *buf, int bufsize)
{
    const struct dirent *d = (const struct dirent *)entry;
    size_t len;

    if (!buf || bufsize <= 0) return 0;
    buf[0] = '\0';
    if (!d) return 0;

    len = strlen(d->d_name);
    if (len > (size_t)(bufsize - 1)) len = (size_t)(bufsize - 1);

    memcpy(buf, d->d_name, len);
    buf[len] = '\0';

    return (int)len;
}
