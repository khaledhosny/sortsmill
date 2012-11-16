//-*- mode: c -*-

static int my_main (int argc, char **argv);

struct _my_args
{
  int argc;
  char **argv;
};

static void *
call_my_main (void *args)
{
  struct _my_args a = *(struct _my_args *) args;
  int *exit_status = xmalloc (sizeof (int));
  *exit_status = my_main (a.argc, a.argv);
  return (void *) exit_status;
}

int
main (int argc, char **argv)
{
  // This looks complicated only because of the need to pass data
  // around through void pointers.

  struct _my_args args = { argc, argv };
  int *exit_status = (int *) scm_with_guile (call_my_main, (void *) &args);
  int status = *exit_status;
  free (exit_status);
  return status;
}
