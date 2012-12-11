#include <unistd.h>

int _sortsmillff_hello_init (int ac, char **av, char **env);

int
main (int argc, char **argv)
{
  char *av[] = { "hello", NULL };
  _sortsmillff_hello_init (1, av, __environ);
  return 0;
}
