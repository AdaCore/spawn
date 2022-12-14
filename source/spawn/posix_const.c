#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <sys/ioctl.h>
#include <sys/wait.h>

#ifndef O_CLOEXEC
/* There is no such constant on RHES 5 and earlier */
#define O_CLOEXEC 0x80000
#endif

int SPAWN_O_NONBLOCK = O_NONBLOCK;
int SPAWN_O_CLOEXEC = O_CLOEXEC;
int SPAWN_O_RDWR = O_RDWR;
int SPAWN_O_NOCTTY = O_NOCTTY;
int SPAWN_WNOHANG = WNOHANG;
int SPAWN_FD_CLOEXEC = FD_CLOEXEC;
int SPAWN_F_SETFD = F_SETFD;
int SPAWN_F_SETFL = F_SETFL;
int SPAWN_EINTR = EINTR;
int SPAWN_EAGAIN = EAGAIN;
int SPAWN_TIOCSCTTY = TIOCSCTTY;
unsigned short SPAWN_POLLIN = POLLIN;
unsigned short SPAWN_POLLOUT = POLLOUT;
unsigned short SPAWN_POLLHUP = POLLHUP;

extern int SPAWN_NONBLOCK;
extern int SPAWN_CLOEXEC;
extern int SPAWN_WNOHANG;
extern int SPAWN_F_SETFL;
extern int SPAWN_EINTR;
extern int SPAWN_EAGAIN;
extern unsigned short SPAWN_POLLIN;
extern unsigned short SPAWN_POLLOUT;
extern unsigned short SPAWN_POLLHUP;

/* Macros */

extern int __spawn_WIFEXITED(int status)
{
    return (int)WIFEXITED(status);
}

extern unsigned __spawn_WEXITSTATUS(int status)
{
    return WEXITSTATUS(status);
}

extern int __spawn_WIFSIGNALED(int status)
{
    return (int)WIFSIGNALED(status);
}

extern unsigned __spawn_WTERMSIG(int status)
{
    return WTERMSIG(status);
}

extern int __spawn_fcntli (int fd, int cmd, int arg)
{
    return fcntl(fd, cmd, arg);
}

extern int __spawn_ioctli (int fd, int cmd, int arg)
{
    return ioctl(fd, cmd, arg);
}
